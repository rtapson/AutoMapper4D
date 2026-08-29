unit AutoMapper;

{$IFDEF FPC}
  {$MODE Delphi}
  {$H+}
{$ENDIF}

interface

uses
{$IFDEF FPC}
  SysUtils,
  TypInfo,
  Generics.Collections;
{$ELSE}
  System.SysUtils,
  System.TypInfo,
  System.Generics.Collections;
{$ENDIF}

type
  EAutoMapperError = class(Exception);

  //Non-generic engine holding everything that does not depend on the target
  //type: the RTTI context, the plan cache, the global settings and the mapping
  //itself.
  //
  //Splitting it out is not only tidier - it is required for Free Pascal, which
  //as of 3.2.2 parses `class var` inside a generic class but does not emit
  //storage for it, so a specialisation fails to link.
  //
  //Map through TAutoMapper<T>; come here for the settings.
  TMapperEngine = class
  private
    class var FFuzzyMatchThreshold : Double;
    class var FStrict : Boolean;
    class procedure SetFuzzyMatchThreshold(const Value : Double); static;
  public
    class procedure MapObject(const Entity : TObject; const Target : TObject;
      const TargetInfo : PTypeInfo; const Configuration : TDictionary<string, string>);

    //Cached plans are built against the current threshold, so changing it
    //discards them. Exposed as well for tests and for long-running processes
    //that map many short-lived types.
    class procedure ClearCache;

    //Minimum similarity ratio a source property name must reach before it is
    //considered a fuzzy match. Defaults to 0.8. Setting it clears the cache.
    class property FuzzyMatchThreshold : Double read FFuzzyMatchThreshold
      write SetFuzzyMatchThreshold;

    //When set, a target property that no source property matches raises
    //EAutoMapperError instead of being silently left at its default. Off by
    //default. The check runs before anything is written, so a strict failure
    //leaves the target untouched.
    class property Strict : Boolean read FStrict write FStrict;
  end;

  TAutoMapper<T : class, constructor> = class
  public
    //Deliberately not overloaded, and public so AutoMapper.Helper can reach
    //them: FPC cannot resolve a call to the overloaded Map from inside a
    //generic helper method, because T could itself be a TDictionary.
    class function MapNew(const Entity : TObject;
      Configuration : TDictionary<string, string> = nil) : T;
    class procedure MapOnto(const Source : TObject; const Target : T;
      Configuration : TDictionary<string, string> = nil);

    //Configuration, when supplied, maps target property name -> source property
    //name, and acts as an override layer on top of automatic matching: target
    //properties it does not mention are still matched by name and fuzzily. The
    //caller retains ownership of the dictionary; it is never freed here.
    class function Map(const Entity : TObject; Configuration : TDictionary<string, string> = nil): T; overload;
    class procedure Map(const Source : TObject; const Target : T; Configuration : TDictionary<string, string> = nil); overload;
  end;

implementation

uses
{$IFDEF FPC}
  Rtti,
  SyncObjs,
{$ELSE}
  System.Rtti,
  System.SyncObjs,
{$ENDIF}
  uFuzzyStringMatch;

const
  //Nested mapping only ever descends into objects the target already holds, but
  //two object graphs that both contain a cycle would still recurse forever.
  MaxNestingDepth = 16;

{$IFDEF FPC}
  //The type kinds FPC 3.2.2 TRttiProperty.GetValue/SetValue actually handle.
  //SetValue RAISES on anything else - notably tkSet and tkRecord - so those
  //properties are skipped rather than mapped. See IsAssignable.
  FPCSupportedKinds = [tkSString, tkAString, tkInteger, tkInt64, tkQWord,
                       tkChar, tkBool, tkWChar, tkEnumeration, tkFloat,
                       tkInterface, tkDynArray];
{$ENDIF}

type
  //One resolved source -> target property pair. The type handles are resolved
  //once, when the plan is built, so that replaying a plan never has to touch
  //TRttiProperty.PropertyType and therefore never re-enters the RTTI pool.
  TPropertyMapping = record
    Source : TRttiProperty;
    Target : TRttiProperty;
    TargetTypeInfo : PTypeInfo;
    SameType : Boolean;
    //Both sides are object properties: map into the instance the target already
    //holds rather than assigning the source's.
    Nested : Boolean;
  end;

  //The resolved mapping for one source/target type combination, plus the target
  //properties nothing matched - recorded here so strict mode costs nothing on
  //the mapping path.
  TMappingPlan = record
    Items : TArray<TPropertyMapping>;
    Unmatched : TArray<string>;
  end;

  TPlansBySource = TDictionary<PTypeInfo, TMappingPlan>;

var
  //Long lived on purpose: cached plans hold TRttiProperty references, which
  //belong to this context's pool and would dangle if it were released.
  GContext : TRttiContext;
  GLock : TCriticalSection;
  //Nested rather than keyed on a pair, so a lookup is two pointer hashes and
  //needs no key allocation on the hot path.
  GPlans : TDictionary<PTypeInfo, TPlansBySource>;

procedure MapRecursive(const Entity : TObject; const Target : TObject;
  const TargetInfo : PTypeInfo; const Configuration : TDictionary<string, string>;
  const Depth : Integer); forward;

//Static, type-level compatibility test used when a plan is built. Kept
//conservative: identical types, or a conversion within the ordinal/float family
//or within the string family.
function IsAssignable(const Source, Target : TRttiType) : Boolean;
{$IFNDEF FPC}
const
  OrdinalKinds = [tkInteger, tkInt64, tkFloat, tkEnumeration, tkChar, tkWChar];
  StringKinds  = [tkString, tkLString, tkWString, tkUString];
{$ENDIF}
begin
  if not (Assigned(Source) and Assigned(Target)) then
    Exit(False);

{$IFDEF FPC}
  //Two restrictions on FPC, both from its Rtti unit rather than from here:
  //
  //  - there is no TValue.TryCast, and hand-rolling one over TValue.Make means
  //    writing raw bytes for every ordinal width and float type, so only
  //    identically typed properties are mapped;
  //  - GetValue/SetValue only implement a subset of type kinds, and SetValue
  //    raises on the rest, so unsupported kinds are excluded here.
  //
  //Both are a strict subset of the Delphi behaviour: nothing is mapped
  //differently, only fewer things are mapped.
  Result := (Source.Handle = Target.Handle) and (Target.TypeKind in FPCSupportedKinds);
{$ELSE}
  if Source.Handle = Target.Handle then
    Exit(True);

  Result := ((Source.TypeKind in OrdinalKinds) and (Target.TypeKind in OrdinalKinds))
         or ((Source.TypeKind in StringKinds) and (Target.TypeKind in StringKinds));
{$ENDIF}
end;

//Converts Source to TargetTypeInfo when the two differ. Delphi delegates to
//TValue.TryCast; on FPC this is only reachable through explicit configuration,
//and always declines - see the note in IsAssignable.
function TryConvert(const Source : TValue; const TargetTypeInfo : PTypeInfo;
  out Value : TValue) : Boolean;
begin
{$IFDEF FPC}
  Value := Source;
  Result := False;
{$ELSE}
  Result := Source.TryCast(TargetTypeInfo, Value);
{$ENDIF}
end;

//Reads SourceProp off Entity and, when the result can legally be written to
//TargetProp, returns it in Value. Guards the two ways a naive GetValue/SetValue
//pair blows up at run time: accessors that are write-only/read-only, and source
//values that are not assignment compatible with the target property type.
function TryGetMappedValue(const SourceProp, TargetProp : TRttiProperty;
  const Entity : TObject; out Value : TValue) : Boolean;
var
  Raw : TValue;
begin
  Result := False;

  if not (Assigned(SourceProp) and Assigned(TargetProp)) then
    Exit;
  if not (SourceProp.IsReadable and TargetProp.IsWritable) then
    Exit;

  Raw := SourceProp.GetValue(Pointer(Entity));

  //Identical types always assign; anything else has to survive a conversion.
  if SourceProp.PropertyType.Handle = TargetProp.PropertyType.Handle then
  begin
    Value := Raw;
    Result := True;
  end
  else
    Result := TryConvert(Raw, TargetProp.PropertyType.Handle, Value);
end;

//Is SourceProp a usable source for TargetProp? WantInstance selects between the
//nested case (both sides objects) and the scalar case (assignable types).
function IsUsableSource(const SourceProp, TargetProp : TRttiProperty;
  const WantInstance : Boolean) : Boolean;
begin
  Result := Assigned(SourceProp) and SourceProp.IsReadable
            and Assigned(SourceProp.PropertyType)
            and (SourceProp.PropertyType.IsInstance = WantInstance);
  if Result and not WantInstance then
    Result := IsAssignable(SourceProp.PropertyType, TargetProp.PropertyType);
end;

//Highest scoring usable source property at or above the threshold, rather than
//whichever one happens to come last in declaration order.
function BestFuzzyMatch(const SourceProps : TArray<TRttiProperty>;
  const TargetProp : TRttiProperty; const WantInstance : Boolean) : TRttiProperty;
var
  BestScore, Score : Double;
  J : Integer;
begin
  Result := nil;
  BestScore := 0;

  for J := 0 to High(SourceProps) do
  begin
    if not IsUsableSource(SourceProps[J], TargetProp, WantInstance) then
      Continue;

    Score := TFuzzyStringMatch.StringSimilarityRatio(TargetProp.Name, SourceProps[J].Name, True);
    if (Score >= TMapperEngine.FuzzyMatchThreshold) and (Score > BestScore) then
    begin
      Result := SourceProps[J];
      BestScore := Score;
    end;
  end;
end;

function BuildPlan(const SourceType, TargetType : TRttiType) : TMappingPlan;
var
  TargetProps, SourceProps : TArray<TRttiProperty>;
  TargetProp, SourceProp : TRttiProperty;
  Count, Missing, I : Integer;
  WantInstance : Boolean;
begin
  TargetProps := TargetType.GetProperties;
  SourceProps := SourceType.GetProperties;

  SetLength(Result.Items, Length(TargetProps));
  SetLength(Result.Unmatched, Length(TargetProps));
  Count := 0;
  Missing := 0;

  for I := 0 to High(TargetProps) do
  begin
    TargetProp := TargetProps[I];

    if not Assigned(TargetProp.PropertyType) then
      Continue;

    WantInstance := TargetProp.PropertyType.IsInstance;

    //A nested target is read, not written: mapping descends into the instance
    //it already holds. A scalar target has to be writable. Either way a target
    //we could never write is skipped silently rather than reported as
    //unmatched - there is no source that would help.
    if WantInstance then
    begin
      if not TargetProp.IsReadable then
        Continue;
    end
    else
      if not TargetProp.IsWritable then
        Continue;

    //An exact name match still has to be usable; if it is not, fall through to
    //fuzzy matching rather than treating the property as matched.
    SourceProp := SourceType.GetProperty(TargetProp.Name);
    if not IsUsableSource(SourceProp, TargetProp, WantInstance) then
      SourceProp := BestFuzzyMatch(SourceProps, TargetProp, WantInstance);

    if not Assigned(SourceProp) then
    begin
      Result.Unmatched[Missing] := TargetProp.Name;
      Inc(Missing);
      Continue;
    end;

    Result.Items[Count].Source := SourceProp;
    Result.Items[Count].Target := TargetProp;
    Result.Items[Count].Nested := WantInstance;
    if WantInstance then
    begin
      Result.Items[Count].TargetTypeInfo := nil;
      Result.Items[Count].SameType := False;
    end
    else
    begin
      Result.Items[Count].TargetTypeInfo := TargetProp.PropertyType.Handle;
      Result.Items[Count].SameType :=
        SourceProp.PropertyType.Handle = TargetProp.PropertyType.Handle;
    end;
    Inc(Count);
  end;

  SetLength(Result.Items, Count);
  SetLength(Result.Unmatched, Missing);
end;

function GetPlan(const SourceInfo, TargetInfo : PTypeInfo) : TMappingPlan;
var
  BySource : TPlansBySource;
begin
  //Built under the lock rather than outside it: unlike replaying a plan, plan
  //construction walks the RTTI pool, and this is a cold path that runs once per
  //source/target combination.
  GLock.Acquire;
  try
    if not GPlans.TryGetValue(TargetInfo, BySource) then
    begin
      BySource := TPlansBySource.Create;
      GPlans.Add(TargetInfo, BySource);
    end;

    if not BySource.TryGetValue(SourceInfo, Result) then
    begin
      Result := BuildPlan(GContext.GetType(SourceInfo), GContext.GetType(TargetInfo));
      BySource.Add(SourceInfo, Result);
    end;
  finally
    GLock.Release;
  end;
end;

procedure CheckNothingUnmatched(const Plan : TMappingPlan; const TargetInfo : PTypeInfo;
  const Configuration : TDictionary<string, string>);
var
  I : Integer;
begin
  for I := 0 to High(Plan.Unmatched) do
  begin
    //An explicit configuration entry counts as a match.
    if Assigned(Configuration) and Configuration.ContainsKey(Plan.Unmatched[I]) then
      Continue;

    raise EAutoMapperError.CreateFmt(
      'Strict mapping: no source property matches target property "%s" on %s.',
      [Plan.Unmatched[I], GContext.GetType(TargetInfo).Name]);
  end;
end;

procedure ApplyPlan(const Plan : TMappingPlan; const Entity : TObject;
  const Target : TObject; const Depth : Integer);
var
  I : Integer;
  Raw, Value : TValue;
  SourceChild, TargetChild : TObject;
begin
  //Everything this loop needs was resolved when the plan was built, so it never
  //re-enters the RTTI pool and needs no lock.
  for I := 0 to High(Plan.Items) do
  begin
    if Plan.Items[I].Nested then
    begin
      //Map into the instance the target already holds. A nil child is skipped:
      //nothing is constructed here, so ownership never changes hands.
      SourceChild := Plan.Items[I].Source.GetValue(Pointer(Entity)).AsObject;
      TargetChild := Plan.Items[I].Target.GetValue(Pointer(Target)).AsObject;
      if Assigned(SourceChild) and Assigned(TargetChild) then
        MapRecursive(SourceChild, TargetChild, TargetChild.ClassInfo, nil, Depth + 1);
      Continue;
    end;

    Raw := Plan.Items[I].Source.GetValue(Pointer(Entity));
    if Plan.Items[I].SameType then
      Plan.Items[I].Target.SetValue(Pointer(Target), Raw)
    else if TryConvert(Raw, Plan.Items[I].TargetTypeInfo, Value) then
      Plan.Items[I].Target.SetValue(Pointer(Target), Value);
  end;
end;

//Applied on top of the automatic plan, so a configuration entry overrides the
//match for the properties it names and leaves the rest alone.
procedure ApplyConfiguration(const Entity : TObject; const Target : TObject;
  const TargetInfo : PTypeInfo; const Configuration : TDictionary<string, string>);
var
  SourceType : TRttiType;
  TargetProps : TArray<TRttiProperty>;
  TargetProp, SourceProp : TRttiProperty;
  MappedProp : string;
  Value : TValue;
  I : Integer;
begin
  //Not cached: the caller can hand a different dictionary to every call.
  GLock.Acquire;
  try
    SourceType := GContext.GetType(Entity.ClassInfo);
    TargetProps := GContext.GetType(TargetInfo).GetProperties;

    for I := 0 to High(TargetProps) do
    begin
      TargetProp := TargetProps[I];

      if not Assigned(TargetProp.PropertyType) then
        Continue;
      if TargetProp.PropertyType.IsInstance then
        Continue;
      if not Configuration.TryGetValue(TargetProp.Name, MappedProp) then
        Continue;

      SourceProp := SourceType.GetProperty(MappedProp);
      if not Assigned(SourceProp) then
        raise EAutoMapperError.CreateFmt('Invalid property mapping. Source: %s; Target: %s', [MappedProp, TargetProp.Name]);

      //An explicit mapping that cannot be honoured is a configuration error, so
      //say so rather than failing with an EInvalidCast.
      if not TryGetMappedValue(SourceProp, TargetProp, Entity, Value) then
        raise EAutoMapperError.CreateFmt('Property mapping is not assignable. Source: %s; Target: %s', [MappedProp, TargetProp.Name]);

      TargetProp.SetValue(Pointer(Target), Value);
    end;
  finally
    GLock.Release;
  end;
end;

procedure MapRecursive(const Entity : TObject; const Target : TObject;
  const TargetInfo : PTypeInfo; const Configuration : TDictionary<string, string>;
  const Depth : Integer);
var
  Plan : TMappingPlan;
begin
  if Depth > MaxNestingDepth then
    raise EAutoMapperError.CreateFmt(
      'Nested mapping exceeded %d levels; the object graph probably contains a cycle.',
      [MaxNestingDepth]);

  Plan := GetPlan(Entity.ClassInfo, TargetInfo);

  //Checked before anything is written, so a strict failure leaves the target
  //exactly as it was.
  if TMapperEngine.Strict then
    CheckNothingUnmatched(Plan, TargetInfo, Configuration);

  ApplyPlan(Plan, Entity, Target, Depth);

  if Assigned(Configuration) then
    ApplyConfiguration(Entity, Target, TargetInfo, Configuration);
end;

procedure ReleasePlans;
var
  Inner : TPlansBySource;
begin
  for Inner in GPlans.Values do
    Inner.Free;
end;

{ TMapperEngine }

class procedure TMapperEngine.SetFuzzyMatchThreshold(const Value : Double);
begin
  GLock.Acquire;
  try
    if Value = FFuzzyMatchThreshold then
      Exit;
    FFuzzyMatchThreshold := Value;
    //Cached plans were resolved against the old threshold.
    ReleasePlans;
    GPlans.Clear;
  finally
    GLock.Release;
  end;
end;

class procedure TMapperEngine.ClearCache;
begin
  GLock.Acquire;
  try
    ReleasePlans;
    GPlans.Clear;
  finally
    GLock.Release;
  end;
end;

class procedure TMapperEngine.MapObject(const Entity : TObject; const Target : TObject;
  const TargetInfo : PTypeInfo; const Configuration : TDictionary<string, string>);
begin
  if not Assigned(Entity) then
    raise EAutoMapperError.Create('AutoMapper: the source object is nil.');
  if not Assigned(Target) then
    raise EAutoMapperError.Create('AutoMapper: the target object is nil.');

  MapRecursive(Entity, Target, TargetInfo, Configuration, 0);
end;

{ TAutoMapper<T> }

class function TAutoMapper<T>.MapNew(const Entity : TObject;
  Configuration : TDictionary<string, string>) : T;
var
  Obj : T;
begin
  Obj := T.Create;
  try
    TMapperEngine.MapObject(Entity, Obj, T.ClassInfo, Configuration);
  except
    Obj.Free;
    raise;
  end;
  Result := Obj;
end;

class procedure TAutoMapper<T>.MapOnto(const Source : TObject; const Target : T;
  Configuration : TDictionary<string, string>);
begin
  TMapperEngine.MapObject(Source, Target, T.ClassInfo, Configuration);
end;

class function TAutoMapper<T>.Map(const Entity: TObject; Configuration: TDictionary<string, string>): T;
begin
  Result := MapNew(Entity, Configuration);
end;

class procedure TAutoMapper<T>.Map(const Source: TObject; const Target: T; Configuration: TDictionary<string, string>);
begin
  MapOnto(Source, Target, Configuration);
end;

initialization
  GContext := TRttiContext.Create;
  GLock := TCriticalSection.Create;
  GPlans := TDictionary<PTypeInfo, TPlansBySource>.Create;
  TMapperEngine.FFuzzyMatchThreshold := 0.8;
  TMapperEngine.FStrict := False;

finalization
  //Inner dictionaries are owned here rather than by a TObjectDictionary, which
  //keeps the ownership obvious and the code identical on both compilers.
  ReleasePlans;
  GPlans.Free;
  GLock.Free;
  GContext.Free;

end.
