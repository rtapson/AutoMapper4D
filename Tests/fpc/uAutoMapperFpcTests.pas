unit uAutoMapperFpcTests;

{
  fpcunit test cases for the Free Pascal build.

  This unit is the single source of FPC test logic. Two runners drive it:

    Tests/fpc/FpcAutoMapperTests.lpr   console, for CI - build-and-run.cmd
    fpcAutoMapperTests.lpi             LCL GUI runner, for use inside Lazarus

  Delphi mode rather than objfpc so the generic syntax matches the Delphi
  tests - no `specialize` noise.
}

{$MODE Delphi}
{$H+}

interface

uses
  Classes,
  SysUtils,
  Generics.Collections,
  fpcunit,
  testregistry,
  uTestTypes,
  uTestClassA,
  uTestClassB,
  uFuzzyStringMatch,
  AutoMapper,
  AutoMapper.Helper;

type
  //Published so Free Pascal's RTTI can see the properties at all - see the
  //README's note on the FPC limitations.
  {$M+}
  TUnrelatedTarget = class
  private
    FCompletelyDifferent : string;
  published
    property CompletelyDifferent : string read FCompletelyDifferent write FCompletelyDifferent;
  end;

  TChild = class
  private
    FCode : string;
  published
    property Code : string read FCode write FCode;
  end;

  TParentWithChild = class
  private
    FName : string;
    FChild : TChild;
  public
    constructor Create;
    destructor Destroy; override;
    procedure DropChild;
  published
    property Name : string read FName write FName;
    property Child : TChild read FChild;
  end;
  {$M-}

  TAutoMapperFpcTests = class(TTestCase)
  private
    FSource : TTestClassA;
    function MapToB : TTestClassB;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    //scalar property kinds
    procedure TestStringProperty;
    procedure TestIntegerProperty;
    procedure TestDateTimeProperty;
    procedure TestBooleanProperty;
    procedure TestEnumProperty;
    procedure TestSetPropertyIsSkippedOnFpc;

    //name matching
    procedure TestCaseMismatchedName;
    procedure TestFuzzyMatchedName;
    procedure TestSimilarityRatioIsWhatWeExpect;

    //entry points
    procedure TestMapOntoExistingObject;
    procedure TestAdaptReturnsMappedObject;
    procedure TestAdaptOntoExistingObject;

    //configuration
    procedure TestConfigOverridesNamedProperty;
    procedure TestConfigLeavesOtherPropertiesAutoMapped;
    procedure TestConfigWithUnknownSourcePropertyRaises;

    //guards and settings
    procedure TestMapFromNilRaises;
    procedure TestMapOntoNilRaises;
    procedure TestStrictModeRaisesOnUnmatchedProperty;
    procedure TestThresholdIsHonoured;

    //nested mapping
    procedure TestNestedObjectMappedIntoExistingInstance;
    procedure TestNestedObjectSkippedWhenTargetChildIsNil;

    //the cached plan path
    procedure TestRepeatedMapsUseCachedPlanCorrectly;
  end;

implementation

{ TParentWithChild }

constructor TParentWithChild.Create;
begin
  inherited Create;
  FChild := TChild.Create;
end;

destructor TParentWithChild.Destroy;
begin
  FChild.Free;
  inherited Destroy;
end;

procedure TParentWithChild.DropChild;
begin
  FreeAndNil(FChild);
end;

{ TAutoMapperFpcTests }

procedure TAutoMapperFpcTests.SetUp;
begin
  inherited SetUp;
  FSource := TTestClassA.Create;
  FSource.Name        := 'Free Pascal';
  FSource.TestDate    := 45623.5;
  FSource.CapTestProp := 'CapTestProp';
  FSource.First_Name  := 'Thomas';
  FSource.HasPropTrue := True;
  FSource.TestEnum    := teSecond;
  FSource.Tests       := [teFirst, teSecond];
  FSource.Age         := 42;
end;

procedure TAutoMapperFpcTests.TearDown;
begin
  FreeAndNil(FSource);
  //Settings are global; make sure one test cannot leak into the next.
  TMapperEngine.Strict := False;
  TMapperEngine.FuzzyMatchThreshold := 0.8;
  inherited TearDown;
end;

function TAutoMapperFpcTests.MapToB : TTestClassB;
begin
  Result := TAutoMapper<TTestClassB>.Map(FSource);
end;

procedure TAutoMapperFpcTests.TestStringProperty;
var
  B : TTestClassB;
begin
  B := MapToB;
  try
    AssertEquals('string property', FSource.Name, B.Name);
  finally
    B.Free;
  end;
end;

procedure TAutoMapperFpcTests.TestIntegerProperty;
var
  B : TTestClassB;
begin
  B := MapToB;
  try
    AssertEquals('integer property', FSource.Age, B.Age);
  finally
    B.Free;
  end;
end;

procedure TAutoMapperFpcTests.TestDateTimeProperty;
var
  B : TTestClassB;
begin
  B := MapToB;
  try
    AssertEquals('TDateTime property', FSource.TestDate, B.TestDate, 1E-9);
  finally
    B.Free;
  end;
end;

procedure TAutoMapperFpcTests.TestBooleanProperty;
var
  B : TTestClassB;
begin
  B := MapToB;
  try
    AssertTrue('boolean property', B.HasPropTrue = FSource.HasPropTrue);
  finally
    B.Free;
  end;
end;

procedure TAutoMapperFpcTests.TestEnumProperty;
var
  B : TTestClassB;
begin
  B := MapToB;
  try
    AssertTrue('enum property', B.TestEnum = FSource.TestEnum);
  finally
    B.Free;
  end;
end;

procedure TAutoMapperFpcTests.TestSetPropertyIsSkippedOnFpc;
var
  B : TTestClassB;
begin
  //FPC's TRttiProperty.SetValue raises on tkSet, so the mapper excludes set
  //properties rather than crashing. This pins that down: if a future FPC gains
  //set support, this test is the one to revisit.
  B := MapToB;
  try
    AssertTrue('set property should be skipped on FPC', B.Tests = []);
  finally
    B.Free;
  end;
end;

procedure TAutoMapperFpcTests.TestCaseMismatchedName;
var
  B : TTestClassB;
begin
  B := MapToB;
  try
    AssertEquals('CapTestProp -> captestprop', FSource.CapTestProp, B.captestprop);
  finally
    B.Free;
  end;
end;

procedure TAutoMapperFpcTests.TestFuzzyMatchedName;
var
  B : TTestClassB;
begin
  B := MapToB;
  try
    AssertEquals('First_Name -> FirstName', FSource.First_Name, B.FirstName);
  finally
    B.Free;
  end;
end;

procedure TAutoMapperFpcTests.TestSimilarityRatioIsWhatWeExpect;
begin
  AssertEquals('FirstName vs First_Name', 0.9,
    TFuzzyStringMatch.StringSimilarityRatio('FirstName', 'First_Name', True), 1E-6);
end;

procedure TAutoMapperFpcTests.TestMapOntoExistingObject;
var
  B : TTestClassB;
begin
  B := TTestClassB.Create;
  try
    TAutoMapper<TTestClassB>.Map(FSource, B);
    AssertEquals('name', FSource.Name, B.Name);
    AssertEquals('fuzzy name', FSource.First_Name, B.FirstName);
  finally
    B.Free;
  end;
end;

procedure TAutoMapperFpcTests.TestAdaptReturnsMappedObject;
var
  B : TTestClassB;
begin
  B := FSource.Adapt<TTestClassB>;
  try
    AssertEquals('name', FSource.Name, B.Name);
    AssertEquals('fuzzy name', FSource.First_Name, B.FirstName);
  finally
    B.Free;
  end;
end;

procedure TAutoMapperFpcTests.TestAdaptOntoExistingObject;
var
  B : TTestClassB;
begin
  B := TTestClassB.Create;
  try
    FSource.Adapt<TTestClassB>(B);
    AssertEquals('name', FSource.Name, B.Name);
    AssertEquals('fuzzy name', FSource.First_Name, B.FirstName);
  finally
    B.Free;
  end;
end;

procedure TAutoMapperFpcTests.TestConfigOverridesNamedProperty;
var
  B : TTestClassB;
  Config : TDictionary<string, string>;
begin
  Config := TDictionary<string, string>.Create;
  try
    Config.Add('FirstName', 'Name');   //target FirstName <- source Name
    B := TAutoMapper<TTestClassB>.Map(FSource, Config);
    try
      AssertEquals('configured property', FSource.Name, B.FirstName);
    finally
      B.Free;
    end;
  finally
    Config.Free;
  end;
end;

procedure TAutoMapperFpcTests.TestConfigLeavesOtherPropertiesAutoMapped;
var
  B : TTestClassB;
  Config : TDictionary<string, string>;
begin
  //A configuration is an override layer, not a whitelist.
  Config := TDictionary<string, string>.Create;
  try
    Config.Add('FirstName', 'Name');
    B := TAutoMapper<TTestClassB>.Map(FSource, Config);
    try
      AssertEquals('unlisted property still mapped', FSource.Age, B.Age);
      AssertEquals('unlisted property still mapped', FSource.Name, B.Name);
    finally
      B.Free;
    end;
  finally
    Config.Free;
  end;
end;

procedure TAutoMapperFpcTests.TestConfigWithUnknownSourcePropertyRaises;
var
  Config : TDictionary<string, string>;
  Raised : Boolean;
begin
  Config := TDictionary<string, string>.Create;
  try
    Config.Add('FirstName', 'NoSuchSourceProperty');
    Raised := False;
    try
      TAutoMapper<TTestClassB>.Map(FSource, Config).Free;
    except
      on EAutoMapperError do
        Raised := True;
    end;
    AssertTrue('an unknown source property should raise EAutoMapperError', Raised);
  finally
    Config.Free;
  end;
end;

procedure TAutoMapperFpcTests.TestMapFromNilRaises;
var
  Raised : Boolean;
begin
  Raised := False;
  try
    TAutoMapper<TTestClassB>.Map(nil).Free;
  except
    on EAutoMapperError do
      Raised := True;
  end;
  AssertTrue('mapping from nil should raise, not access-violate', Raised);
end;

procedure TAutoMapperFpcTests.TestMapOntoNilRaises;
var
  Raised : Boolean;
begin
  Raised := False;
  try
    TAutoMapper<TTestClassB>.Map(FSource, TTestClassB(nil));
  except
    on EAutoMapperError do
      Raised := True;
  end;
  AssertTrue('mapping onto nil should raise, not access-violate', Raised);
end;

procedure TAutoMapperFpcTests.TestStrictModeRaisesOnUnmatchedProperty;
var
  Raised : Boolean;
begin
  TMapperEngine.Strict := True;
  Raised := False;
  try
    TAutoMapper<TUnrelatedTarget>.Map(FSource).Free;
  except
    on EAutoMapperError do
      Raised := True;
  end;
  AssertTrue('strict mode should reject an unmatched target property', Raised);
end;

procedure TAutoMapperFpcTests.TestThresholdIsHonoured;
var
  B : TTestClassB;
begin
  //First_Name scores 0.9 against FirstName; demanding 0.95 rejects it. Changing
  //the threshold must also discard the plan cached at the old value.
  TMapperEngine.FuzzyMatchThreshold := 0.95;
  B := MapToB;
  try
    AssertEquals('fuzzy match below the threshold should be rejected', '', B.FirstName);
  finally
    B.Free;
  end;

  TMapperEngine.FuzzyMatchThreshold := 0.8;
  B := MapToB;
  try
    AssertEquals('lowering the threshold should invalidate the cached plan',
      FSource.First_Name, B.FirstName);
  finally
    B.Free;
  end;
end;

procedure TAutoMapperFpcTests.TestNestedObjectMappedIntoExistingInstance;
var
  Source, Target : TParentWithChild;
  ChildBefore : TObject;
begin
  Source := TParentWithChild.Create;
  Target := TParentWithChild.Create;
  try
    Source.Name := 'parent';
    Source.Child.Code := 'CHILD-CODE';
    ChildBefore := Target.Child;

    TAutoMapper<TParentWithChild>.Map(Source, Target);

    AssertEquals('scalar property', 'parent', Target.Name);
    AssertEquals('nested property', 'CHILD-CODE', Target.Child.Code);
    AssertSame('the existing child should be mapped into, not replaced',
      ChildBefore, TObject(Target.Child));
  finally
    Target.Free;
    Source.Free;
  end;
end;

procedure TAutoMapperFpcTests.TestNestedObjectSkippedWhenTargetChildIsNil;
var
  Source, Target : TParentWithChild;
begin
  Source := TParentWithChild.Create;
  Target := TParentWithChild.Create;
  try
    Source.Name := 'parent';
    Source.Child.Code := 'CHILD-CODE';
    Target.DropChild;

    //Nothing is constructed by the mapper, so a nil child is simply skipped.
    TAutoMapper<TParentWithChild>.Map(Source, Target);

    AssertEquals('scalar properties should still map', 'parent', Target.Name);
    AssertNull('a nil target child should be left nil', TObject(Target.Child));
  finally
    Target.Free;
    Source.Free;
  end;
end;

procedure TAutoMapperFpcTests.TestRepeatedMapsUseCachedPlanCorrectly;
var
  B : TTestClassB;
  I : Integer;
begin
  //The first map builds the plan; the rest replay it.
  for I := 1 to 200 do
  begin
    B := MapToB;
    try
      AssertEquals('name on iteration ' + IntToStr(I), FSource.Name, B.Name);
      AssertEquals('fuzzy name on iteration ' + IntToStr(I), FSource.First_Name, B.FirstName);
    finally
      B.Free;
    end;
  end;
end;

initialization
  RegisterTest(TAutoMapperFpcTests);

end.
