program fpcAutoMapperTests;

{
  LCL GUI runner for the Free Pascal test suite, for use inside Lazarus.

  The test cases live in Tests/fpc/uAutoMapperFpcTests.pas, which the console
  runner (Tests/fpc/FpcAutoMapperTests.lpr) also uses - one suite, two runners.
  For CI use the console one; it needs no LCL.
}

{$MODE Delphi}
{$H+}

uses
  Interfaces,
  Forms,
  GuiTestRunner,
  uAutoMapperFpcTests;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.
