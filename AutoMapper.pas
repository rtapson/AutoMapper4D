unit AutoMapper;

interface

uses
  System.TypInfo,
  System.Rtti,
  System.Generics.Collections;

type
  TAutoMapper<T : class, constructor> = class
  private
    type
      //One resolved source -> target property pair. The type handles are
      //resolved once, when the plan is built, so that replaying a plan never
      //has to touch TRttiProperty.PropertyType and therefore never re-enters
      //the RTTI pool.
      TPropertyMapping = record
        Source : TRttiProperty;
        Target : TRttiProperty;
        TargetTypeInfo : PTypeInfo;
        SameType : Boolean;
      end;
      //The full set of pairs for a given source type. Name matching and fuzzy
      //scoring depend only on the two types involved, so the plan is computed
      //once and replayed for every subsequent object of that type.
      TMappingPlan = TArray<TPropertyMapping>;
    const
      //Minimum similarity ratio a source property name must reach before it is
      //considered a fuzzy match for a target property.
      FuzzyMatchThreshold = 0.8;
    class var
      //Long lived on purpose: cached plans hold TRttiProperty references, which
      //belong to this context's pool and would dangle if it were released.
      FContext : TRttiContext;
      //Keyed on the source PTypeInfo; the target type is fixed per T.
      FPlanCache : TDictionary<PTypeInfo, TMappingPlan>;
      FLock : TObject;

    class constructor Create;
    class destructor Destroy;

    //Static, type-level compatibility test used when a plan is built. Kept
    //conservative: identical types, or a conversion within the ordinal/float
    //family or within the string family.
    class function IsAssignable(const Source, Target : TRttiType) : Boolean;

    //Reads SourceProp off Entity and, when the result can legally be written to
    //TargetProp, returns it in Value. Guards the two ways a naive
    //GetValue/SetValue pair blows up at run time: accessors that are
    //write-only/read-only, and source values that are not assignment
    //compatible with the target property type.
    class function TryGetMappedValue(const SourceProp, TargetProp : TRttiProperty;
      const Entity : TObject; out Value : TValue) : Boolean;

    class function BuildPlan(const SourceType : TRttiType) : TMappingPlan;
    class function GetPlan(const Entity : TObject) : TMappingPlan;
    class procedure MapUsingConfiguration(const Entity : TObject; const Target : TObject;
      const Configuration : TDictionary<string, string>);
    class procedure DoMapping(const Entity : TObject; const Target : TObject;
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
  System.SysUtils,
  uFuzzyStringMatch;

{ TAutoMapper<T> }

class constructor TAutoMapper<T>.Create;
begin
  FContext := TRttiContext.Create;
  FLock := TObject.Create;
  FPlanCache := TDictionary<PTypeInfo, TMappingPlan>.Create;
end;

class destructor TAutoMapper<T>.Destroy;
begin
  FPlanCache.Free;
  FLock.Free;
  FContext.Free;
end;

class function TAutoMapper<T>.IsAssignable(const Source, Target : TRttiType) : Boolean;
const
  OrdinalKinds = [tkInteger, tkInt64, tkFloat, tkEnumeration, tkChar, tkWChar];
  StringKinds  = [tkString, tkLString, tkWString, tkUString];
begin
  if not (Assigned(Source) and Assigned(Target)) then
    Exit(False);
  if Source.Handle = Target.Handle then
    Exit(True);

  Result := ((Source.TypeKind in OrdinalKinds) and (Target.TypeKind in OrdinalKinds))
         or ((Source.TypeKind in StringKinds) and (Target.TypeKind in StringKinds));
end;

class function TAutoMapper<T>.TryGetMappedValue(const SourceProp, TargetProp : TRttiProperty;
  const Entity : TObject; out Value : TValue) : Boolean;
var
  Raw : TValue;
begin
  Result := False;

  if not (Assigned(SourceProp) and Assigned(TargetProp)) then
    Exit;
  if not (SourceProp.IsReadable and TargetProp.IsWritable) then
    Exit;

  Raw := SourceProp.GetValue(Entity);

  //Identical types always assign; anything else has to survive a cast.
  if SourceProp.PropertyType.Handle = TargetProp.PropertyType.Handle then
  begin
    Value := Raw;
    Result := True;
  end
  else
    Result := Raw.TryCast(TargetProp.PropertyType.Handle, Value);
end;

class function TAutoMapper<T>.BuildPlan(const SourceType : TRttiType) : TMappingPlan;
var
  TargetProps, SourceProps : TArray<TRttiProperty>;
  TargetProp, SourceProp, Prop, BestProp : TRttiProperty;
  BestScore, Score : Double;
  Count : Integer;
begin
  TargetProps := FContext.GetType(TypeInfo(T)).GetProperties;
  SourceProps := SourceType.GetProperties;

  SetLength(Result, Length(TargetProps));
  Count := 0;

  for TargetProp in TargetProps do
  begin
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

      for Prop in SourceProps do
      begin
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

class function TAutoMapper<T>.GetPlan(const Entity : TObject) : TMappingPlan;
var
  Key : PTypeInfo;
begin
  Key := Entity.ClassInfo;

  //Built under the lock rather than outside it: unlike the replay path below,
  //plan construction walks the RTTI pool, and this is a cold path that runs
  //once per source type.
  TMonitor.Enter(FLock);
  try
    if not FPlanCache.TryGetValue(Key, Result) then
    begin
      Result := BuildPlan(FContext.GetType(Key));
      FPlanCache.Add(Key, Result);
    end;
  finally
    TMonitor.Exit(FLock);
  end;
end;

class procedure TAutoMapper<T>.MapUsingConfiguration(const Entity : TObject; const Target : TObject;
  const Configuration : TDictionary<string, string>);
var
  SourceType : TRttiType;
  TargetProp, SourceProp : TRttiProperty;
  MappedProp : string;
  Value : TValue;
begin
  //Not cached: the caller can hand a different dictionary to every call, and
  //an explicit mapping skips the expensive part (fuzzy scoring) anyway.
  TMonitor.Enter(FLock);
  try
    SourceType := FContext.GetType(Entity.ClassInfo);

    for TargetProp in FContext.GetType(TypeInfo(T)).GetProperties do
    begin
      if not Assigned(TargetProp.PropertyType) then
        Continue;
      if TargetProp.PropertyType.IsInstance then
        Continue;
      if not Configuration.TryGetValue(TargetProp.Name, MappedProp) then
        Continue;

      SourceProp := SourceType.GetProperty(MappedProp);
      if not Assigned(SourceProp) then
        raise Exception.CreateFmt('Invalid property mapping. Source: %s; Target: %s', [MappedProp, TargetProp.Name]);

      //An explicit mapping that cannot be honoured is a configuration error,
      //so say so rather than failing with an EInvalidCast.
      if not TryGetMappedValue(SourceProp, TargetProp, Entity, Value) then
        raise Exception.CreateFmt('Property mapping is not assignable. Source: %s; Target: %s', [MappedProp, TargetProp.Name]);

      TargetProp.SetValue(Target, Value);
    end;
  finally
    TMonitor.Exit(FLock);
  end;
end;

class procedure TAutoMapper<T>.DoMapping(const Entity: TObject; const Target : TObject;
  Configuration : TDictionary<string, string>);
var
  Plan : TMappingPlan;
  Raw, Value : TValue;
  I : Integer;
begin
  if Assigned(Configuration) then
  begin
    MapUsingConfiguration(Entity, Target, Configuration);
    Exit;
  end;

  Plan := GetPlan(Entity);

  //Everything this loop needs was resolved when the plan was built, so it never
  //re-enters the RTTI pool and needs no lock.
  for I := 0 to High(Plan) do
  begin
    Raw := Plan[I].Source.GetValue(Entity);
    if Plan[I].SameType then
      Plan[I].Target.SetValue(Target, Raw)
    else if Raw.TryCast(Plan[I].TargetTypeInfo, Value) then
      Plan[I].Target.SetValue(Target, Value);
  end;
end;

class function TAutoMapper<T>.Map(const Entity: TObject; Configuration: TDictionary<string, string>): T;
var
  Obj : T;
begin
  Obj := T.Create;
  try
    DoMapping(Entity, Obj, Configuration);
  except
    Obj.Free;
    raise;
  end;
  Result := Obj;
end;

class procedure TAutoMapper<T>.Map(const Source: TObject; const Target: T; Configuration: TDictionary<string, string>);
begin
  DoMapping(Source, Target, Configuration);
end;

{ AutoMapper4D }

function AutoMapper4D.Adapt<T>: T;
begin
  //Delegated so the new object is not leaked if mapping raises.
  Result := TAutoMapper<T>.Map(Self);
end;

procedure AutoMapper4D.Adapt<T>(const DestObject: T);
begin
  TAutoMapper<T>.Map(Self, DestObject);
end;

end.
