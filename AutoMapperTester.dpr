program AutoMapperTester;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

uses
{$IFnDEF FPC}
  Vcl.Forms,
{$ELSE}
{$ENDIF}
  fmMain in 'fmMain.pas' {Form3},
  AutoMapper in 'AutoMapper.pas',
  AutoMapper.Helper in 'AutoMapper.Helper.pas',
  uTestClassA in 'uTestClassA.pas',
  uTestClassB in 'uTestClassB.pas',
  uFuzzyStringMatch in 'uFuzzyStringMatch.pas',
  uTestTypes in 'uTestTypes.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm3, Form3);
  Application.Run;
end.
