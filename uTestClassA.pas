unit uTestClassA;

{$IFDEF FPC}
  {$MODE Delphi}
  {$H+}
{$ENDIF}

interface

uses
  uTestTypes;

type
  //{$M+} so the properties get RTTI. Free Pascal only exposes PUBLISHED
  //members through its Rtti unit, unlike Delphi, whose extended RTTI covers
  //public members too. Published works on both.
  {$M+}
  TTestClassA = class
  private
    FName: string;
    FTestDate: TDateTime;
    FCapTestProp: string;
    FFirst_Name: string;
    FHasPropTrue: Boolean;
    FTestEnum: TTestEnumeration;
    FTests: TTests;
    FAge: Integer;
  published
    property Name: string read FName write FName;
    property TestDate: TDateTime read FTestDate write FTestDate;
    property CapTestProp: string read FCapTestProp write FCapTestProp;
    property First_Name: string read FFirst_Name write FFirst_Name;
    property HasPropTrue: Boolean read FHasPropTrue write FHasPropTrue;
    property TestEnum: TTestEnumeration read FTestEnum write FTestEnum;
    property Tests: TTests read FTests write FTests;
    property Age: Integer read FAge write FAge;
  end;
  {$M-}

implementation

end.
