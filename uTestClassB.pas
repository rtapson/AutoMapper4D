unit uTestClassB;

interface

uses
  uTestTypes;

type
  TTestClassB = class
  private
    FName: string;
    FTestDate: TDateTime;
    Fcaptestprop: string;
    FFirstName: string;
    FHasPropTrue: Boolean;
    FTestEnum: TTestEnumeration;
    FTests: TTests;
    FAge: Integer;
  public
    property Name: string read FName write FName;
    property TestDate: TDateTime read FTestDate write FTestDate;
    property captestprop: string read Fcaptestprop write Fcaptestprop;
    property FirstName: string read FFirstName write FFirstName;
    property HasPropTrue: Boolean read FHasPropTrue write FHasPropTrue;
    property TestEnum: TTestEnumeration read FTestEnum write FTestEnum;
    property Tests: TTests read FTests write FTests;
    property Age: Integer read FAge write FAge;
  end;

implementation

end.
