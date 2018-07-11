unit uTestClassA;

interface

uses
  uTestTypes;

type
  TTestClassA = class
  private
    FName: string;
    FTestDate: TDateTime;
    FCapTestProp: string;
    FFirst_Name: string;
    FHasPropTrue: Boolean;
    FTestEnum: TTestEnumeration;
    FTests: TTests;
  public
    property Name: string read FName write FName;
    property TestDate: TDateTime read FTestDate write FTestDate;
    property CapTestProp: string read FCapTestProp write FCapTestProp;
    property First_Name: string read FFirst_Name write FFirst_Name;
    property HasPropTrue: Boolean read FHasPropTrue write FHasPropTrue;
    property TestEnum: TTestEnumeration read FTestEnum write FTestEnum;
    property Tests: TTests read FTests write FTests;
  end;

implementation

end.
