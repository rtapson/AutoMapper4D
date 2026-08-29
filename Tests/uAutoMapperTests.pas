unit uAutoMapperTests;

interface
uses
  DUnitX.TestFramework,
  uTestClassA,
  uTestClassB;

type
  //Fixtures for the edge cases. Published (with {$M+}) to match uTestClassA and
  //uTestClassB, so the same declarations would also be visible to Free Pascal.
  {$M+}

  //Nothing here matches a TTestClassA property, exactly or fuzzily.
  TUnrelatedTarget = class
  private
    FCompletelyDifferent : string;
  published
    property CompletelyDifferent : string read FCompletelyDifferent write FCompletelyDifferent;
  end;

  //Name matches TTestClassA.Name exactly, but has no setter.
  TReadOnlyTarget = class
  private
    FName : string;
  public
    constructor Create;
  published
    property Name : string read FName;
  end;

  //Counts its own live instances, so a leaked target is directly observable.
  TCountedTarget = class
  private
    FFirstName : string;
  public
    class var LiveCount : Integer;
    constructor Create;
    destructor Destroy; override;
  published
    property FirstName : string read FFirstName write FFirstName;
  end;

  //FirstNam scores 0.889 against FirstName and FirstName12 scores 0.818, with
  //the weaker candidate declared last. TestData scores 0.875 against a
  //TDateTime TestDate, so it is a name match but not an assignable one.
  TFuzzySource = class
  private
    FFirstNam : string;
    FFirstName12 : string;
    FTestData : string;
  published
    property FirstNam : string read FFirstNam write FFirstNam;
    property FirstName12 : string read FFirstName12 write FFirstName12;
    property TestData : string read FTestData write FTestData;
  end;

  TFuzzyTarget = class
  private
    FFirstName : string;
    FTestDate : TDateTime;
  published
    property FirstName : string read FFirstName write FFirstName;
    property TestDate : TDateTime read FTestDate write FTestDate;
  end;

  //Nested mapping fixtures. The child is only ever mapped INTO, never
  //created, so both sides construct their own.
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

  [TestFixture]
  TAutoMapperTests = class(TObject)
  private
    TestA : TTestClassA;
    TestB : TTestClassB;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    [TestCase('string', 'Spring4D')]
    procedure TestStringPropertiesAreEqual(const AValue : string);

    [Test]
    [TestCase('Integer', '10')]
    procedure TestIntegerPropertiesAreEqual(const AValue : integer);

    [Test]
    [TestCase('Date', '45623.01254')]
    procedure TestDateTimePropertiesAreEqual(const AValue : TDateTime);

    [Test]
    [TestCase('Boolean', 'True')]
    [TestCase('Boolean', 'False')]
    procedure TestBooleanProperty(const AValue : Boolean);

    [Test]
    procedure TestEnumProperty;

    [Test]
    procedure TestSetProperty;

    [Test]
    procedure TestPropertyNameCaseMismatch;

    [Test]
    //In this case using an _ to seperate words
    procedure TestPropertyNamesAreSlightlyDifferent;

    // --- the other two entry points ---

    [Test]
    procedure TestMapOntoExistingObject;

    [Test]
    procedure TestAdaptReturnsMappedObject;

    [Test]
    procedure TestAdaptOntoExistingObject;

    // --- explicit configuration ---

    [Test]
    procedure TestUsingConfigForMapper;

    [Test]
    procedure TestConfigWithUnknownSourcePropertyRaises;

    // --- regressions: one per bug fixed in the correctness pass ---

    [Test]
    procedure TestBestFuzzyCandidateWins;

    [Test]
    procedure TestTypeIncompatibleFuzzyMatchIsSkipped;

    [Test]
    procedure TestReadOnlyTargetPropertyIsSkipped;

    [Test]
    procedure TestFailedMappingDoesNotLeakTarget;

    [Test]
    procedure TestUnmatchedTargetPropertyKeepsDefault;

    // --- nil guard (#6) ---

    [Test]
    procedure TestMapWithNilSourceRaises;

    [Test]
    procedure TestMapOntoNilTargetRaises;

    // --- configuration extends rather than replaces (#8) ---

    [Test]
    procedure TestConfigOverridesButLeavesOtherPropertiesMapped;

    // --- nested mapping (#9) ---

    [Test]
    procedure TestNestedObjectIsMappedIntoExistingInstance;

    [Test]
    procedure TestNestedObjectIsSkippedWhenTargetChildIsNil;

    // --- strict mode (#10) ---

    [Test]
    procedure TestStrictModeRaisesOnUnmatchedProperty;

    [Test]
    procedure TestStrictModeAcceptsConfiguredProperty;

    [Test]
    procedure TestStrictModeLeavesTargetUntouchedOnFailure;

    // --- configurable fuzzy threshold (#11) ---

    [Test]
    procedure TestRaisingThresholdRejectsWeakerMatch;

    [Test]
    procedure TestLoweringThresholdAcceptsWeakerMatch;
  end;

implementation

uses
  AutoMapper,
  AutoMapper.Helper,
  uTestTypes,
  System.SysUtils,
  System.Generics.Collections;

{ TReadOnlyTarget }

constructor TReadOnlyTarget.Create;
begin
  inherited Create;
  FName := 'untouched';
end;

{ TCountedTarget }

constructor TCountedTarget.Create;
begin
  inherited Create;
  Inc(LiveCount);
end;

destructor TCountedTarget.Destroy;
begin
  Dec(LiveCount);
  inherited Destroy;
end;

{ TAutoMapperTests }

procedure TAutoMapperTests.Setup;
begin
  //Cleared up front: the fixture instance is reused across tests, so a test
  //that never assigns TestB must not let TearDown free the previous one.
  TestB := nil;
  TestA := TTestClassA.Create;
  TestA.TestDate := Now;
  TestA.CapTestProp := 'CapTestProp';
  TestA.First_Name := 'Thomas';
  TestA.HasPropTrue := True;
  TestA.TestEnum := teSecond;
  TestA.Tests := [teFirst, teSecond];

end;

procedure TAutoMapperTests.TearDown;
begin
  FreeAndNil(TestA);
  FreeAndNil(TestB);
end;

procedure TAutoMapperTests.TestBooleanProperty(const AValue: Boolean);
begin
  TestA.HasPropTrue := AValue;

  TestB := TAutoMapper<TTestClassB>.Map(TestA);
  Assert.AreEqual(AValue, TestB.HasPropTrue);
end;

procedure TAutoMapperTests.TestDateTimePropertiesAreEqual(const AValue: TDateTime);
begin
  TestA.TestDate := AValue;

  TestB := TAutoMapper<TTestClassB>.Map(TestA);
  //Compared with a tolerance rather than exactly: AValue is parsed from a
  //string in the [TestCase] attribute, so an exact float compare is fragile.
  Assert.AreEqual(Extended(AValue), Extended(TestB.TestDate), 1E-9);
end;

procedure TAutoMapperTests.TestEnumProperty;
begin
  TestB := TAutoMapper<TTestClassB>.Map(TestA);

  Assert.AreEqual(TestA.TestEnum, TestB.TestEnum);
end;

procedure TAutoMapperTests.TestSetProperty;
begin
  TestB := TAutoMapper<TTestClassB>.Map(TestA);

  Assert.IsTrue(TestB.Tests = TestA.Tests, 'set property was not mapped');
  Assert.IsTrue(teFirst in TestB.Tests);
  Assert.IsTrue(teSecond in TestB.Tests);
end;

procedure TAutoMapperTests.TestIntegerPropertiesAreEqual(const AValue: integer);
begin
  TestA.Age := AValue;

  TestB := TAutoMapper<TTestClassB>.Map(TestA);
  Assert.AreEqual(AValue, TestB.Age);
end;

procedure TAutoMapperTests.TestPropertyNameCaseMismatch;
begin
  TestB := TAutoMapper<TTestClassB>.Map(TestA);
  Assert.AreEqual(TestA.CapTestProp, TestB.captestprop);
end;

procedure TAutoMapperTests.TestPropertyNamesAreSlightlyDifferent;
begin
  TestB := TAutoMapper<TTestClassB>.Map(TestA);
  Assert.AreEqual(TestA.First_Name, TestB.FirstName);
end;

procedure TAutoMapperTests.TestStringPropertiesAreEqual(const AValue : string);
begin
  TestA.Name := AValue;

  TestB := TAutoMapper<TTestClassB>.Map(TestA);
  Assert.AreEqual(AValue, TestB.Name);
end;

procedure TAutoMapperTests.TestMapOntoExistingObject;
begin
  TestA.Name := 'onto existing';

  TestB := TTestClassB.Create;
  TAutoMapper<TTestClassB>.Map(TestA, TestB);

  Assert.AreEqual(TestA.Name, TestB.Name);
  Assert.AreEqual(TestA.First_Name, TestB.FirstName);
  Assert.AreEqual(TestA.CapTestProp, TestB.captestprop);
end;

procedure TAutoMapperTests.TestAdaptReturnsMappedObject;
begin
  TestA.Name := 'adapted';

  TestB := TestA.Adapt<TTestClassB>;

  Assert.AreEqual(TestA.Name, TestB.Name);
  Assert.AreEqual(TestA.First_Name, TestB.FirstName);
end;

procedure TAutoMapperTests.TestAdaptOntoExistingObject;
begin
  TestA.Name := 'adapted onto';

  TestB := TTestClassB.Create;
  TestA.Adapt<TTestClassB>(TestB);

  Assert.AreEqual(TestA.Name, TestB.Name);
  Assert.AreEqual(TestA.First_Name, TestB.FirstName);
end;

procedure TAutoMapperTests.TestUsingConfigForMapper;
var
  Config : TDictionary<string, string>;
begin
  TestA.Name := 'configured';

  Config := TDictionary<string, string>.Create;
  try
    //Key is the TARGET property, value is the SOURCE property.
    Config.Add('FirstName', 'Name');
    TestB := TAutoMapper<TTestClassB>.Map(TestA, Config);
  finally
    Config.Free;
  end;

  Assert.AreEqual(TestA.Name, TestB.FirstName);
end;

procedure TAutoMapperTests.TestConfigOverridesButLeavesOtherPropertiesMapped;
var
  Config : TDictionary<string, string>;
begin
  //A configuration is an override layer, not a whitelist: it redirects the
  //properties it names and leaves the rest to automatic matching.
  TestA.Age := 99;
  TestA.Name := 'source name';

  Config := TDictionary<string, string>.Create;
  try
    Config.Add('FirstName', 'Name');
    TestB := TAutoMapper<TTestClassB>.Map(TestA, Config);
  finally
    Config.Free;
  end;

  Assert.AreEqual(TestA.Name, TestB.FirstName, 'the configured property should be overridden');
  Assert.AreEqual(99, TestB.Age, 'an unlisted property should still be matched automatically');
  Assert.AreEqual(TestA.Name, TestB.Name, 'an unlisted property should still be matched automatically');
  Assert.AreEqual(TestA.CapTestProp, TestB.captestprop, 'fuzzy matching should still apply to unlisted properties');
end;

procedure TAutoMapperTests.TestConfigWithUnknownSourcePropertyRaises;
var
  Config : TDictionary<string, string>;
begin
  Config := TDictionary<string, string>.Create;
  try
    Config.Add('FirstName', 'NoSuchSourceProperty');

    Assert.WillRaise(
      procedure
      begin
        TAutoMapper<TTestClassB>.Map(TestA, Config).Free;
      end,
      EAutoMapperError,
      'a configuration naming a missing source property should raise');
  finally
    Config.Free;
  end;
end;

procedure TAutoMapperTests.TestBestFuzzyCandidateWins;
var
  Source : TFuzzySource;
  Target : TFuzzyTarget;
begin
  //Both candidates clear the 0.8 threshold and the weaker one is declared
  //last, so a "last match wins" loop would pick FirstName12.
  Source := TFuzzySource.Create;
  try
    Source.FirstNam    := 'BEST';    //0.889
    Source.FirstName12 := 'WORST';   //0.818

    Target := TAutoMapper<TFuzzyTarget>.Map(Source);
    try
      Assert.AreEqual('BEST', Target.FirstName,
        'the highest scoring fuzzy candidate should win, not the last one');
    finally
      Target.Free;
    end;
  finally
    Source.Free;
  end;
end;

procedure TAutoMapperTests.TestTypeIncompatibleFuzzyMatchIsSkipped;
var
  Source : TFuzzySource;
  Target : TFuzzyTarget;
begin
  //TestData (string) scores 0.875 against TestDate (TDateTime): a good enough
  //name match, but not assignable. It must be skipped, not raise EInvalidCast.
  Source := TFuzzySource.Create;
  try
    Source.TestData := 'not a date';

    Target := TAutoMapper<TFuzzyTarget>.Map(Source);
    try
      Assert.AreEqual(Extended(0), Extended(Target.TestDate), 1E-9,
        'a type-incompatible fuzzy match should be skipped');
    finally
      Target.Free;
    end;
  finally
    Source.Free;
  end;
end;

procedure TAutoMapperTests.TestReadOnlyTargetPropertyIsSkipped;
var
  Target : TReadOnlyTarget;
begin
  //Name matches exactly but the target property has no setter; writing it
  //would raise EPropReadOnly.
  TestA.Name := 'from source';

  Target := TAutoMapper<TReadOnlyTarget>.Map(TestA);
  try
    Assert.AreEqual('untouched', Target.Name,
      'a read-only target property should be skipped');
  finally
    Target.Free;
  end;
end;

procedure TAutoMapperTests.TestFailedMappingDoesNotLeakTarget;
var
  Config : TDictionary<string, string>;
  I : Integer;
begin
  //The target is constructed before mapping runs, so a mapping that raises
  //must still free it.
  Config := TDictionary<string, string>.Create;
  try
    Config.Add('FirstName', 'NoSuchSourceProperty');

    TCountedTarget.LiveCount := 0;
    for I := 1 to 25 do
    try
      TAutoMapper<TCountedTarget>.Map(TestA, Config).Free;
    except
      //expected
    end;

    Assert.AreEqual(0, TCountedTarget.LiveCount,
      'a failed mapping should not leak the half-built target');
  finally
    Config.Free;
  end;
end;

procedure TAutoMapperTests.TestUnmatchedTargetPropertyKeepsDefault;
var
  Target : TUnrelatedTarget;
begin
  //No property of TTestClassA matches CompletelyDifferent, exactly or fuzzily.
  Target := TAutoMapper<TUnrelatedTarget>.Map(TestA);
  try
    Assert.AreEqual('', Target.CompletelyDifferent,
      'an unmatched target property should keep its default');
  finally
    Target.Free;
  end;
end;

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

{ nil guard }

procedure TAutoMapperTests.TestMapWithNilSourceRaises;
begin
  Assert.WillRaise(
    procedure
    begin
      TAutoMapper<TTestClassB>.Map(nil).Free;
    end,
    EAutoMapperError,
    'mapping from nil should raise, not access-violate');
end;

procedure TAutoMapperTests.TestMapOntoNilTargetRaises;
begin
  Assert.WillRaise(
    procedure
    begin
      TAutoMapper<TTestClassB>.Map(TestA, TTestClassB(nil));
    end,
    EAutoMapperError,
    'mapping onto nil should raise, not access-violate');
end;

{ nested mapping }

procedure TAutoMapperTests.TestNestedObjectIsMappedIntoExistingInstance;
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

    Assert.AreEqual('parent', Target.Name);
    Assert.AreEqual('CHILD-CODE', Target.Child.Code, 'the nested object should be mapped');
    Assert.AreSame(ChildBefore, TObject(Target.Child),
      'the existing child instance should be mapped into, not replaced');
  finally
    Target.Free;
    Source.Free;
  end;
end;

procedure TAutoMapperTests.TestNestedObjectIsSkippedWhenTargetChildIsNil;
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

    Assert.AreEqual('parent', Target.Name, 'scalar properties should still map');
    Assert.IsNull(TObject(Target.Child), 'a nil target child should be left nil');
  finally
    Target.Free;
    Source.Free;
  end;
end;

{ strict mode }

procedure TAutoMapperTests.TestStrictModeRaisesOnUnmatchedProperty;
begin
  TMapperEngine.Strict := True;
  try
    Assert.WillRaise(
      procedure
      begin
        TAutoMapper<TUnrelatedTarget>.Map(TestA).Free;
      end,
      EAutoMapperError,
      'strict mode should reject a target property nothing matches');
  finally
    TMapperEngine.Strict := False;
  end;
end;

procedure TAutoMapperTests.TestStrictModeAcceptsConfiguredProperty;
var
  Config : TDictionary<string, string>;
  Target : TUnrelatedTarget;
begin
  //An explicit configuration entry satisfies strict mode.
  TMapperEngine.Strict := True;
  Config := TDictionary<string, string>.Create;
  try
    Config.Add('CompletelyDifferent', 'Name');
    TestA.Name := 'supplied';

    Target := TAutoMapper<TUnrelatedTarget>.Map(TestA, Config);
    try
      Assert.AreEqual('supplied', Target.CompletelyDifferent);
    finally
      Target.Free;
    end;
  finally
    Config.Free;
    TMapperEngine.Strict := False;
  end;
end;

procedure TAutoMapperTests.TestStrictModeLeavesTargetUntouchedOnFailure;
var
  Target : TUnrelatedTarget;
begin
  //The strict check runs before anything is written.
  Target := TUnrelatedTarget.Create;
  try
    Target.CompletelyDifferent := 'preserved';
    TMapperEngine.Strict := True;
    try
      try
        TAutoMapper<TUnrelatedTarget>.Map(TestA, Target);
      except
        on EAutoMapperError do ; //expected
      end;
    finally
      TMapperEngine.Strict := False;
    end;

    Assert.AreEqual('preserved', Target.CompletelyDifferent,
      'a strict failure should leave the target untouched');
  finally
    Target.Free;
  end;
end;

{ configurable fuzzy threshold }

procedure TAutoMapperTests.TestRaisingThresholdRejectsWeakerMatch;
var
  Source : TFuzzySource;
  Target : TFuzzyTarget;
begin
  //FirstNam scores 0.889 against FirstName. Demanding 0.95 rejects it.
  TMapperEngine.FuzzyMatchThreshold := 0.95;
  try
    Source := TFuzzySource.Create;
    try
      Source.FirstNam := 'BEST';
      Target := TAutoMapper<TFuzzyTarget>.Map(Source);
      try
        Assert.AreEqual('', Target.FirstName,
          'a candidate below the threshold should not be used');
      finally
        Target.Free;
      end;
    finally
      Source.Free;
    end;
  finally
    TMapperEngine.FuzzyMatchThreshold := 0.8;
  end;
end;

procedure TAutoMapperTests.TestLoweringThresholdAcceptsWeakerMatch;
var
  Source : TFuzzySource;
  Target : TFuzzyTarget;
begin
  //TestData scores 0.875 against TestDate but is a string, so it stays
  //unassignable regardless. FirstName12 scores 0.818 and is assignable, so it
  //becomes the only candidate once FirstNam is removed from contention by
  //raising, then lowering, the bar. Here we simply confirm the setting is
  //honoured and that the cache was invalidated by the change.
  TMapperEngine.FuzzyMatchThreshold := 0.95;
  try
    Source := TFuzzySource.Create;
    try
      Source.FirstNam := 'BEST';

      Target := TAutoMapper<TFuzzyTarget>.Map(Source);
      try
        Assert.AreEqual('', Target.FirstName, 'precondition: rejected at 0.95');
      finally
        Target.Free;
      end;

      //Lowering the bar must discard the plan cached at the higher threshold.
      TMapperEngine.FuzzyMatchThreshold := 0.8;

      Target := TAutoMapper<TFuzzyTarget>.Map(Source);
      try
        Assert.AreEqual('BEST', Target.FirstName,
          'lowering the threshold should invalidate the cached plan');
      finally
        Target.Free;
      end;
    finally
      Source.Free;
    end;
  finally
    TMapperEngine.FuzzyMatchThreshold := 0.8;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TAutoMapperTests);
end.
