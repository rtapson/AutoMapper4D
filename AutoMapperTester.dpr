program AutoMapperTester;

uses
  Vcl.Forms,
  fmMain in 'fmMain.pas' {Form3},
  AutoMapper in 'AutoMapper.pas',
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
