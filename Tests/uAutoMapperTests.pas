unit uAutoMapperTests;

interface
uses
  DUnitX.TestFramework,
  uTestClassA,
  uTestClassB;

type

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
    procedure TestPropertyNameCaseMismatch;

    [Test]
    //In this case using an _ to seperate words
    procedure TestPropertyNamesAreSlightlyDifferent;


    [Test]
    procedure TestUsingConfigForMapper;
  end;

implementation

uses
  AutoMapper, uTestTypes, System.SysUtils;

procedure TAutoMapperTests.Setup;
begin
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
  TestA.Free;
  TestB.Free;
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
  Assert.AreEqual(AValue, TestB.TestDate);
end;

procedure TAutoMapperTests.TestEnumProperty;
begin
  TestB := TAutoMapper<TTestClassB>.Map(TestA);

  Assert.AreEqual(TestA.TestEnum, TestB.TestEnum);
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



procedure TAutoMapperTests.TestUsingConfigForMapper;
begin

end;

initialization
  TDUnitX.RegisterTestFixture(TAutoMapperTests);
end.
