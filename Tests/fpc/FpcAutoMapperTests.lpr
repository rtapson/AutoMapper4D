program FpcAutoMapperTests;

{$MODE Delphi}
{$H+}

uses
  SysUtils,
  Generics.Collections,
  uTestTypes,
  uTestClassA,
  uTestClassB,
  uFuzzyStringMatch,
  AutoMapper;

var
  Failures : Integer = 0;

procedure Check(const Name : string; Passed : Boolean; const Detail : string = '');
begin
  if Passed then
    Writeln('  PASS  ', Name)
  else
  begin
    Writeln('  FAIL  ', Name, '  ', Detail);
    Inc(Failures);
  end;
end;

function MakeSource : TTestClassA;
begin
  Result := TTestClassA.Create;
  Result.Name        := 'Spring-free';
  Result.TestDate    := 45623.5;
  Result.CapTestProp := 'CapTestProp';
  Result.First_Name  := 'Thomas';
  Result.HasPropTrue := True;
  Result.TestEnum    := teSecond;
  Result.Tests       := [teFirst, teSecond];
  Result.Age         := 42;
end;

procedure TestAutomaticMapping;
var
  A : TTestClassA;
  B : TTestClassB;
begin
  A := MakeSource;
  try
    B := TAutoMapper<TTestClassB>.Map(A);
    try
      Check('string property',        B.Name = A.Name, '(got "' + B.Name + '")');
      Check('integer property',       B.Age = A.Age, '(got ' + IntToStr(B.Age) + ')');
      Check('TDateTime property',     B.TestDate = A.TestDate);
      Check('boolean property',       B.HasPropTrue = A.HasPropTrue);
      Check('enum property',          B.TestEnum = A.TestEnum);
      //FPC's Rtti cannot set a tkSet property, so the mapper skips it.
      Check('set property skipped on FPC', B.Tests = []);
      Check('case-mismatched name',   B.captestprop = A.CapTestProp, '(got "' + B.captestprop + '")');
      Check('fuzzy name First_Name',  B.FirstName = A.First_Name, '(got "' + B.FirstName + '")');
    finally
      B.Free;
    end;
  finally
    A.Free;
  end;
end;

procedure TestMapOntoExisting;
var
  A : TTestClassA;
  B : TTestClassB;
begin
  A := MakeSource;
  B := TTestClassB.Create;
  try
    TAutoMapper<TTestClassB>.Map(A, B);
    Check('map onto existing object', (B.Name = A.Name) and (B.Age = A.Age)
                                      and (B.FirstName = A.First_Name));
  finally
    B.Free;
    A.Free;
  end;
end;

procedure TestAdaptHelper;
var
  A : TTestClassA;
  B, C : TTestClassB;
begin
  A := MakeSource;
  try
    B := A.Adapt<TTestClassB>;
    try
      Check('Adapt<T> returns mapped object', (B.Name = A.Name) and (B.FirstName = A.First_Name));
    finally
      B.Free;
    end;

    C := TTestClassB.Create;
    try
      A.Adapt<TTestClassB>(C);
      Check('Adapt<T>(existing)', (C.Name = A.Name) and (C.FirstName = A.First_Name));
    finally
      C.Free;
    end;
  finally
    A.Free;
  end;
end;

procedure TestConfiguration;
var
  A : TTestClassA;
  B : TTestClassB;
  Config : TDictionary<string, string>;
begin
  A := MakeSource;
  Config := TDictionary<string, string>.Create;
  try
    Config.Add('FirstName', 'Name');   //target FirstName <- source Name
    B := TAutoMapper<TTestClassB>.Map(A, Config);
    try
      Check('config maps listed property', B.FirstName = A.Name, '(got "' + B.FirstName + '")');
      Check('config skips unlisted property', B.Age = 0, '(got ' + IntToStr(B.Age) + ')');
    finally
      B.Free;
    end;
  finally
    Config.Free;
    A.Free;
  end;
end;

procedure TestConfigurationErrorRaises;
var
  A : TTestClassA;
  Config : TDictionary<string, string>;
  Raised : Boolean;
begin
  A := MakeSource;
  Config := TDictionary<string, string>.Create;
  try
    Config.Add('FirstName', 'NoSuchSourceProperty');
    Raised := False;
    try
      TAutoMapper<TTestClassB>.Map(A, Config).Free;
    except
      on E: Exception do Raised := True;
    end;
    Check('invalid config raises', Raised);
  finally
    Config.Free;
    A.Free;
  end;
end;

procedure TestPlanIsCached;
var
  A : TTestClassA;
  B : TTestClassB;
  I : Integer;
  AllOk : Boolean;
begin
  //Second and later maps take the cached path; make sure they still map.
  A := MakeSource;
  AllOk := True;
  try
    for I := 1 to 200 do
    begin
      B := TAutoMapper<TTestClassB>.Map(A);
      try
        if (B.Name <> A.Name) or (B.FirstName <> A.First_Name) or (B.Age <> A.Age) then
          AllOk := False;
      finally
        B.Free;
      end;
    end;
    Check('200 cached-plan maps all correct', AllOk);
  finally
    A.Free;
  end;
end;

begin
  Writeln('FPC ', {$I %FPCVERSION%}, ' - AutoMapper4D runtime checks');
  Writeln('similarity FirstName/First_Name = ',
    TFuzzyStringMatch.StringSimilarityRatio('FirstName', 'First_Name', True):0:4);
  Writeln;

  TestAutomaticMapping;
  TestMapOntoExisting;
  TestAdaptHelper;
  TestConfiguration;
  TestConfigurationErrorRaises;
  TestPlanIsCached;

  Writeln;
  if Failures = 0 then
    Writeln('ALL FPC CHECKS PASSED')
  else
  begin
    Writeln(Failures, ' FPC CHECK(S) FAILED');
    ExitCode := 1;
  end;
end.
