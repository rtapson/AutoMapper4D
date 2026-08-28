unit AutoMapper;

interface

uses
  System.Rtti,
  Spring.Collections;

type
  TAutoMapper<T : class, constructor> = class
  private
    type
      //One resolved source -> target property pair.
      TPropertyMapping = record
        Source : TRttiProperty;
        Target : TRttiProperty;
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
      //Keyed on the source PTypeInfo; the target type is fixed per T.
      FPlanCache : IDictionary<Pointer, TMappingPlan>;
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
      const Configuration : IDictionary<string, string>);
    class procedure DoMapping(const Entity : TObject; const Target : TObject; Configuration : IDictionary<string, string>);
  public
    class function Map(const Entity : TObject; Configuration : IDictionary<string, string> = nil): T; overload;
    class procedure Map(const Source : TObject; const Target : T; Configuration : IDictionary<string, string> = nil); overload;
  end;

  AutoMapper4D = class helper for TObject
    function Adapt<T: class, constructor>: T; overload;
    procedure Adapt<T: class, constructor>(const DestObject: T); overload;
  end;

implementation

uses
  System.SysUtils,
  System.TypInfo,
  Spring.Reflection,
  uFuzzyStringMatch;

{ TAutoMapper<T> }

class constructor TAutoMapper<T>.Create;
begin
  FLock := TObject.Create;
  FPlanCache := TCollections.CreateDictionary<Pointer, TMappingPlan>;
end;

class destructor TAutoMapper<T>.Destroy;
begin
  FPlanCache := nil;
  FLock.Free;
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
  Plan : TMappingPlan;
  Count : Integer;
begin
  Plan := nil;
  Count := 0;

  TType.GetType<T>.Properties.ForEach(
    procedure(const TargetProp : TRttiProperty)
    var
      SourceProp : TRttiProperty;
      BestProp : TRttiProperty;
      BestScore : Double;
    begin
      if not Assigned(TargetProp.PropertyType) then
        Exit;
      if TargetProp.PropertyType.IsInstance then
        Exit;
      if not TargetProp.IsWritable then
        Exit;

      if SourceType.HasProperty(TargetProp.Name) then
        SourceProp := SourceType.GetProperty(TargetProp.Name)
      else  //do fuzzy match
      begin
        //Keep the highest scoring candidate we could actually assign, rather
        //than whichever one happens to come last in declaration order.
        BestProp := nil;
        BestScore := 0;

        SourceType.Properties.ForEach(
          procedure(const Prop : TRttiProperty)
          var
            Score : Double;
          begin
            Score := TFuzzyStringMatch.StringSimilarityRatio(TargetProp.Name, Prop.Name, True);
            if (Score >= FuzzyMatchThreshold) and (Score > BestScore) and Prop.IsReadable
               and IsAssignable(Prop.PropertyType, TargetProp.PropertyType) then
            begin
              BestProp := Prop;
              BestScore := Score;
            end;
          end);

        SourceProp := BestProp;
      end;

      if not Assigned(SourceProp) then
        Exit;
      if not SourceProp.IsReadable then
        Exit;
      if not IsAssignable(SourceProp.PropertyType, TargetProp.PropertyType) then
        Exit;

      if Count = Length(Plan) then
        SetLength(Plan, Count + 8);
      Plan[Count].Source := SourceProp;
      Plan[Count].Target := TargetProp;
      Inc(Count);
    end);

  SetLength(Plan, Count);
  Result := Plan;
end;

class function TAutoMapper<T>.GetPlan(const Entity : TObject) : TMappingPlan;
var
  Key : Pointer;
begin
  Key := Entity.ClassInfo;

  TMonitor.Enter(FLock);
  try
    if FPlanCache.TryGetValue(Key, Result) then
      Exit;
  finally
    TMonitor.Exit(FLock);
  end;

  //Built outside the lock. Plan construction is idempotent, so at worst two
  //threads racing on a cold cache each build the same plan and one wins.
  Result := BuildPlan(TType.GetType(Entity.ClassInfo));

  TMonitor.Enter(FLock);
  try
    FPlanCache[Key] := Result;
  finally
    TMonitor.Exit(FLock);
  end;
end;

class procedure TAutoMapper<T>.MapUsingConfiguration(const Entity : TObject; const Target : TObject;
  const Configuration : IDictionary<string, string>);
var
  SourceType : TRttiType;
begin
  //Not cached: the caller can hand a different dictionary to every call, and
  //an explicit mapping skips the expensive part (fuzzy scoring) anyway.
  SourceType := TType.GetType(Entity.ClassInfo);

  TType.GetType<T>.Properties.ForEach(
    procedure(const TargetProp : TRttiProperty)
    var
      MappedProp : string;
      Value : TValue;
    begin
      if not Assigned(TargetProp.PropertyType) then
        Exit;
      if TargetProp.PropertyType.IsInstance then
        Exit;
      if not Configuration.TryGetValue(TargetProp.Name, MappedProp) then
        Exit;

      if not SourceType.HasProperty(MappedProp) then
        raise Exception.CreateFmt('Invalid property mapping. Source: %s; Target: %s', [MappedProp, TargetProp.Name]);

      //An explicit mapping that cannot be honoured is a configuration error,
      //so say so rather than failing with an EInvalidCast.
      if not TryGetMappedValue(SourceType.GetProperty(MappedProp), TargetProp, Entity, Value) then
        raise Exception.CreateFmt('Property mapping is not assignable. Source: %s; Target: %s', [MappedProp, TargetProp.Name]);

      TargetProp.SetValue(Target, Value);
    end);
end;

class procedure TAutoMapper<T>.DoMapping(const Entity: TObject; const Target : TObject; Configuration : IDictionary<string, string>);
var
  Plan : TMappingPlan;
  Value : TValue;
  I : Integer;
begin
  if Assigned(Configuration) then
  begin
    MapUsingConfiguration(Entity, Target, Configuration);
    Exit;
  end;

  Plan := GetPlan(Entity);
  for I := 0 to High(Plan) do
    if TryGetMappedValue(Plan[I].Source, Plan[I].Target, Entity, Value) then
      Plan[I].Target.SetValue(Target, Value);
end;

class function TAutoMapper<T>.Map(const Entity: TObject; Configuration: IDictionary<string, string>): T;
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

class procedure TAutoMapper<T>.Map(const Source: TObject; const Target: T; Configuration: IDictionary<string, string>);
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
