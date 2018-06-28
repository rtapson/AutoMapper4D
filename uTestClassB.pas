unit uTestClassB;

interface

type
  TTestClassB = class
  private
    FName: string;
    FTestDate: TDateTime;
    Fcaptestprop: string;
    FFirstName: string;
  public
    property Name: string read FName write FName;
    property TestDate: TDateTime read FTestDate write FTestDate;
    property captestprop: string read Fcaptestprop write Fcaptestprop;
    property FirstName: string read FFirstName write FFirstName;
  end;

implementation

end.
