unit AutoMapper;

{$IFDEF FPC}
  {$MODE Delphi}
  {$H+}
{$ENDIF}

interface

uses
{$IFDEF FPC}
  TypInfo,
  Generics.Collections;
{$ELSE}
  System.TypInfo,
  System.Generics.Collections;
{$ENDIF}

type
  //Non-generic engine holding everything that does not depend on the target
  //type: the RTTI context, the plan cache and the mapping itself.
  //
  //Splitting it out is not only tidier - it is required for Free Pascal, which
  //as of 3.2.2 parses `class var` inside a generic class but does not emit
  //storage for it, so a specialisation fails to link.
  //
  //This type is an implementation detail; use TAutoMapper<T> or Adapt<T>.
  TMapperEngine = class
  public
    class procedure MapObject(const Entity : TObject; const Target : TObject;
      const TargetInfo : PTypeInfo; const Configuration : TDictionary<string, string>);
  end;

  TAutoMapper<T : class, constructor> = class
  private
    //Deliberately not overloaded. FPC cannot resolve a call to the overloaded
    //Map from inside the Adapt<T> helper, because T could itself be a
    //TDictionary, so both Map and Adapt route through these.
    class function MapNew(const Entity : TObject;
      Configuration : TDictionary<string, string>) : T;
    class procedure MapOnto(const Source : TObject; const Target : T;
      Configuration : TDictionary<string, string>);
  public
    //Configuration, when supplied, maps target property name -> source property
    //name. The caller retains ownership of the dictionary; it is never freed
    //here.
    class function Map(const Entity : TObject; Configuration : TDictionary<string, string> = nil): T; overload;
    class procedure Map(const Source : TObject; const Target : T; Configuration : TDictionary<string, string> = nil); overload;
  end;

  AutoMapper4D = class helper for TObject
    function Adapt<T: class, constructor>: T; overload;
    procedure Adapt<T: class, constructor>(const DestObject: T); overload;
  end;

implementation

uses
{$IFDEF FPC}
  SysUtils,
  Rtti,
  SyncObjs,
{$ELSE}
  System.SysUtils,
  System.Rtti,
  System.SyncObjs,
{$ENDIF}
  uFuzzyStringMatch;

const
{$IFDEF FPC}
  //The type kinds FPC 3.2.2 TRttiProperty.GetValue/SetValue actually handle.
  //SetValue RAISES on anything else - notably tkSet and tkRecord - so those
  //properties are skipped rather than mapped. See IsAssignable.
  FPCSupportedKinds = [tkSString, tkAString, tkInteger, tkInt64, tkQWord,
                       tkChar, tkBool, tkWChar, tkEnumeration, tkFloat,
                       tkInterface, tkDynArray];
{$ENDIF}

  //Minimum similarity ratio a source property name must reach before it is
  //considered a fuzzy match for a target property.
  FuzzyMatchThreshold = 0.8;

type
  //One resolved source -> target property pair. The type handles are resolved
  //once, when the plan is built, so that replaying a plan never has to touch
  //TRttiProperty.PropertyType and therefore never re-enters the RTTI pool.
  TPropertyMapping = record
    Source : TRttiProperty;
    Target : TRttiProperty;
    TargetTypeInfo : PTypeInfo;
    SameType : Boolean;
  end;

  //The full set of pairs for one source/target type combination. Name matching
  //and fuzzy scoring depend only on the two types, so a plan is computed once
  //and replayed for every subsequent object.
  TMappingPlan = TArray<TPropertyMapping>;

  TPlansBySource = TDictionary<PTypeInfo, TMappingPlan>;

var
  //Long lived on purpose: cached plans hold TRttiProperty references, which
  //belong to this context's pool and would dangle if it were released.
  GContext : TRttiContext;
  GLock : TCriticalSection;
  //Nested rather than keyed on a pair, so a lookup is two pointer hashes and
  //needs no key allocation on the hot path.
  GPlans : TDictionary<PTypeInfo, TPlansBySource>;

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

function BuildPlan(const SourceType, TargetType : TRttiType) : TMappingPlan;
var
  TargetProps, SourceProps : TArray<TRttiProperty>;
  TargetProp, SourceProp, Prop, BestProp : TRttiProperty;
  BestScore, Score : Double;
  Count, I, J : Integer;
begin
  TargetProps := TargetType.GetProperties;
  SourceProps := SourceType.GetProperties;

  SetLength(Result, Length(TargetProps));
  Count := 0;

  for I := 0 to High(TargetProps) do
  begin
    TargetProp := TargetProps[I];

    if not Assigned(TargetProp.PropertyType) then
      Continue;
    if TargetProp.PropertyType.IsInstance then
      Continue;
    if not TargetProp.IsWritable then
      Continue;

    SourceProp := SourceType.GetProperty(TargetProp.Name);

    if not Assigned(SourceProp) then
    begin
      //Fuzzy match: keep the highest scoring candidate we could actually
      //assign, rather than whichever one comes last in declaration order.
      BestProp := nil;
      BestScore := 0;

      for J := 0 to High(SourceProps) do
      begin
        Prop := SourceProps[J];
        Score := TFuzzyStringMatch.StringSimilarityRatio(TargetProp.Name, Prop.Name, True);
        if (Score >= FuzzyMatchThreshold) and (Score > BestScore) and Prop.IsReadable
           and IsAssignable(Prop.PropertyType, TargetProp.PropertyType) then
        begin
          BestProp := Prop;
          BestScore := Score;
        end;
      end;

      SourceProp := BestProp;
    end;

    if not Assigned(SourceProp) then
      Continue;
    if not SourceProp.IsReadable then
      Continue;
    if not IsAssignable(SourceProp.PropertyType, TargetProp.PropertyType) then
      Continue;

    Result[Count].Source := SourceProp;
    Result[Count].Target := TargetProp;
    Result[Count].TargetTypeInfo := TargetProp.PropertyType.Handle;
    Result[Count].SameType := SourceProp.PropertyType.Handle = TargetProp.PropertyType.Handle;
    Inc(Count);
  end;

  SetLength(Result, Count);
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

procedure MapUsingConfiguration(const Entity : TObject; const Target : TObject;
  const TargetInfo : PTypeInfo; const Configuration : TDictionary<string, string>);
var
  SourceType : TRttiType;
  TargetProps : TArray<TRttiProperty>;
  TargetProp, SourceProp : TRttiProperty;
  MappedProp : string;
  Value : TValue;
  I : Integer;
begin
  //Not cached: the caller can hand a different dictionary to every call, and an
  //explicit mapping skips the expensive part (fuzzy scoring) anyway.
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
        raise Exception.CreateFmt('Invalid property mapping. Source: %s; Target: %s', [MappedProp, TargetProp.Name]);

      //An explicit mapping that cannot be honoured is a configuration error, so
      //say so rather than failing with an EInvalidCast.
      if not TryGetMappedValue(SourceProp, TargetProp, Entity, Value) then
        raise Exception.CreateFmt('Property mapping is not assignable. Source: %s; Target: %s', [MappedProp, TargetProp.Name]);

      TargetProp.SetValue(Pointer(Target), Value);
    end;
  finally
    GLock.Release;
  end;
end;

procedure ReleasePlans;
var
  Inner : TPlansBySource;
begin
  for Inner in GPlans.Values do
    Inner.Free;
end;

{ TMapperEngine }

class procedure TMapperEngine.MapObject(const Entity : TObject; const Target : TObject;
  const TargetInfo : PTypeInfo; const Configuration : TDictionary<string, string>);
var
  Plan : TMappingPlan;
  Raw, Value : TValue;
  I : Integer;
begin
  if Assigned(Configuration) then
  begin
    MapUsingConfiguration(Entity, Target, TargetInfo, Configuration);
    Exit;
  end;

  Plan := GetPlan(Entity.ClassInfo, TargetInfo);

  //Everything this loop needs was resolved when the plan was built, so it never
  //re-enters the RTTI pool and needs no lock.
  for I := 0 to High(Plan) do
  begin
    Raw := Plan[I].Source.GetValue(Pointer(Entity));
    if Plan[I].SameType then
      Plan[I].Target.SetValue(Pointer(Target), Raw)
    else if TryConvert(Raw, Plan[I].TargetTypeInfo, Value) then
      Plan[I].Target.SetValue(Pointer(Target), Value);
  end;
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

{ AutoMapper4D }

function AutoMapper4D.Adapt<T>: T;
begin
  //Routed through MapNew rather than Map so the call is not overloaded: the new
  //object is still freed if mapping raises.
  Result := TAutoMapper<T>.MapNew(Self, nil);
end;

procedure AutoMapper4D.Adapt<T>(const DestObject: T);
begin
  TAutoMapper<T>.MapOnto(Self, DestObject, nil);
end;

initialization
  GContext := TRttiContext.Create;
  GLock := TCriticalSection.Create;
  GPlans := TDictionary<PTypeInfo, TPlansBySource>.Create;

finalization
  //Inner dictionaries are owned here rather than by a TObjectDictionary, which
  //keeps the ownership obvious and the code identical on both compilers.
  ReleasePlans;
  GPlans.Free;
  GLock.Free;
  GContext.Free;

end.
