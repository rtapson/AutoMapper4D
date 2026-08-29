program FpcAutoMapperTests;

{
  Console runner for the Free Pascal test suite. The test cases themselves live
  in uAutoMapperFpcTests, which the Lazarus GUI project (fpcAutoMapperTests.lpi)
  also uses - one suite, two runners.

  Built and run by build-and-run.cmd. Needs no Lazarus installation, only fpc.
}

{$MODE Delphi}
{$H+}

uses
  Classes,
  consoletestrunner,
  uAutoMapperFpcTests;

var
  App : TTestRunner;

begin
  App := TTestRunner.Create(nil);
  try
    App.Initialize;
    App.Title := 'AutoMapper4D - Free Pascal test suite';
    App.Run;
  finally
    App.Free;
  end;
end.
