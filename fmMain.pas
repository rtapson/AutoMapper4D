unit fmMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm3 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form3: TForm3;

implementation

{$R *.dfm}

uses
  Spring.Collections,
  uTestClassA,
  uTestClassB,
  AutoMapper,
  uFuzzyStringMatch,
  uTestTypes;


procedure TForm3.Button1Click(Sender: TObject);
var
  TestA : TTestClassA;
  TestB : TTestClassB;
  i: Integer;
begin
  TestA := TTestClassA.Create;
  TestA.Name := 'Testing This is a new object';
  TestA.TestDate := Now;
  TestA.CapTestProp := 'CapTestProp';
  TestA.First_Name := 'Thomas';
  TestA.HasPropTrue := True;
  TestA.TestEnum := teSecond;
  TestA.Tests := [teFirst, teSecond];

  TestB := TAutoMapper<TTestClassB>.Map(TestA);

  Memo1.Lines.Add(TestB.Name);
  Memo1.Lines.Add(FormatDateTime('MM/DD/YYYY HH:nn:ss', TestB.TestDate));
  Memo1.Lines.Add(TestB.captestprop);
  Memo1.Lines.Add(TestB.FirstName);
  if TestB.HasPropTrue then
    Memo1.Lines.Add('True')
  else
    Memo1.Lines.Add('False');

  case TestB.TestEnum of
  teFirst : Memo1.Lines.Add('teFirst');
  teSecond: Memo1.Lines.Add('teSecond');
  end;

  if teFirst in TestB.Tests then
    Memo1.Lines.Add('TestB.Tests.teFirst');
  if teSecond in TestB.Tests then
    Memo1.Lines.Add('TestB.Tests.teSecond');
end;

procedure TForm3.Button2Click(Sender: TObject);
var
  TestA : TTestClassA;
  TestB : TTestClassB;
  Config : IDictionary<string, string>;
begin
  TestA := TTestClassA.Create;
  TestA.Name := 'Testing This is a new object';
  TestA.TestDate := Now;

  Config := TCollections.CreateDictionary<string, string>;
  Config.Add('Name', 'Name');

  TestB := TAutoMapper<TTestClassB>.Map(TestA, Config);

  Memo1.Lines.Add(TestB.Name);
  Memo1.Lines.Add(FormatDateTime('MM/DD/YYYY HH:nn:ss', TestB.TestDate));
end;

procedure TForm3.Button3Click(Sender: TObject);
begin
  Memo1.Lines.Add(TFuzzyStringMatch.StringSimilarityRatio('FirstName', 'First_Name', True).ToString);
  Memo1.Lines.Add(TFuzzyStringMatch.StringSimilarityRatio('FirstName', 'Last_Name', True).ToString);
  Memo1.Lines.Add(TFuzzyStringMatch.StringSimilarityRatio('Quantity', 'Qty', True).ToString);
end;

procedure TForm3.Button4Click(Sender: TObject);
var
  TestA : TTestClassA;
  TestB : TTestClassB;
begin
  TestA := TTestClassA.Create;
  TestA.Name := 'Testing This is a new object';
  TestA.TestDate := Now;
  TestA.CapTestProp := 'CapTestProp';
  TestA.First_Name := 'Thomas';

  TestB := TTestClassB.Create;

  TAutoMapper<TTestClassB>.Map(TestA, TestB);

  Memo1.Lines.Add(TestB.Name);
  Memo1.Lines.Add(FormatDateTime('MM/DD/YYYY HH:nn:ss', TestB.TestDate));
  Memo1.Lines.Add(TestB.captestprop);
  Memo1.Lines.Add(TestB.FirstName);
end;

procedure TForm3.Button5Click(Sender: TObject);
//var
//  TestA : TTestClassA;
//  TestB : TTestClassB;
begin
  var TestA := TTestClassA.Create;
  TestA.Name := 'Testing This is a new object';
  TestA.TestDate := Now;
  TestA.CapTestProp := 'CapTestProp';
  TestA.First_Name := 'Thomas';

  var TestB := TestA.Adapt<TTestClassB>;

  Memo1.Lines.Add(TestB.Name);
  Memo1.Lines.Add(FormatDateTime('MM/DD/YYYY HH:nn:ss', TestB.TestDate));
  Memo1.Lines.Add(TestB.captestprop);
  Memo1.Lines.Add(TestB.FirstName);
end;

procedure TForm3.Button6Click(Sender: TObject);
begin
  var TestA := TTestClassA.Create;
  TestA.Name := 'Testing This is a new object';
  TestA.TestDate := Now;
  TestA.CapTestProp := 'CapTestProp';
  TestA.First_Name := 'Thomas';

  var TestB := TTestClassB.Create;

  TestA.Adapt<TTestClassB>(TestB);

  Memo1.Lines.Add(TestB.Name);
  Memo1.Lines.Add(FormatDateTime('MM/DD/YYYY HH:nn:ss', TestB.TestDate));
  Memo1.Lines.Add(TestB.captestprop);
  Memo1.Lines.Add(TestB.FirstName);
end;

end.
