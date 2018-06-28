unit uTestClassA;

interface

type
  TTestClassA = class
  private
    FName: string;
    FTestDate: TDateTime;
    FCapTestProp: string;
    FFirst_Name: string;
  public
    property Name: string read FName write FName;
    property TestDate: TDateTime read FTestDate write FTestDate;
    property CapTestProp: string read FCapTestProp write FCapTestProp;
    property First_Name: string read FFirst_Name write FFirst_Name;
  end;

implementation

end.
