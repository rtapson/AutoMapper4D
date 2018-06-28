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
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
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
  uFuzzyStringMatch;

procedure TForm3.Button1Click(Sender: TObject);
var
  TestA : TTestClassA;
  TestB : TTestClassB;
begin
  TestA := TTestClassA.Create;
  TestA.Name := 'Testing This is a new object';
  TestA.TestDate := Now;
  TestA.CapTestProp := 'CapTestProp';
  TestA.First_Name := 'Thomas';

  TestB := TAutoMapper<TTestClassB>.Map(TestA);

  Memo1.Lines.Add(TestB.Name);
  Memo1.Lines.Add(FormatDateTime('MM/DD/YYYY HH:nn:ss', TestB.TestDate));
  Memo1.Lines.Add(TestB.captestprop);
  Memo1.Lines.Add(TestB.FirstName);
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

end.
