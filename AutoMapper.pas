unit AutoMapper;

interface

uses
  Spring.Collections;

type
  TAutoMapper<T : class, constructor> = class
  private 
    class procedure DoMapping(const Entity : TObject; const Target : TObject; Configuration : IDictionary<string, string>);
  public
    class function Map(const Entity : TObject; Configuration : IDictionary<string, string> = nil): T; overload;
    class procedure Map(const Source : TObject; const Target : T; Configuration : IDictionary<string, string> = nil); overload;
  end;

implementation

uses
  System.SysUtils,
  System.Rtti,
  Spring.Reflection,
  uFuzzyStringMatch;

{ TAutoMapper<T> }

class procedure TAutoMapper<T>.DoMapping(const Entity: TObject; const Target : TObject; Configuration : IDictionary<string, string>);
var
  TempPropName : string;
begin
  TType.GetType<T>.Properties.ForEach(
    procedure(const AProp : TRttiProperty)
    var
      aType : TRttiType;
      MappedProp : string;
    begin
      if not AProp.PropertyType.IsInstance then
      begin
        aType := TType.FindType(Entity.ClassName);
        if not Assigned(Configuration) then
        begin
          if AType.HasProperty(AProp.Name) then
            AProp.SetValue(Target, aType.GetProperty(AProp.Name).GetValue(Entity))
          else  //do fuzzy match
          begin
            aType.Properties.ForEach(
            procedure(const Prop : TRttiProperty)
            begin
              if TFuzzyStringMatch.StringSimilarityRatio(AProp.Name, Prop.Name, True) >= 0.8 then
              begin
                AProp.SetValue(Target, aType.GetProperty(Prop.Name).GetValue(Entity));
              end;
            end);
          end;
        end
        else
        begin
          if Configuration.TryGetValue(AProp.Name, MappedProp) then
          begin
            if AType.HasProperty(MappedProp) then
              AProp.SetValue(Target, aType.GetProperty(MappedProp).GetValue(Entity))
            else
              raise Exception.CreateFmt('Invalid property mapping. Source: %s; Target: %s', [MappedProp, AProp.Name]);
          end;
        end;
      end;
    end);
end;

class function TAutoMapper<T>.Map(const Entity: TObject; Configuration: IDictionary<string, string>): T;
var
  Obj : T;
begin
  Obj := T.Create;
  DoMapping(Entity, Obj, Configuration);
  Result := Obj;
end;

class procedure TAutoMapper<T>.Map(const Source: TObject; const Target: T; Configuration: IDictionary<string, string>);
begin
  DoMapping(Source, Target, Configuration);
end;

end.
