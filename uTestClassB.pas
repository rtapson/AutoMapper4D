unit uTestClassB;

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
  published
    property Name: string read FName write FName;
    property TestDate: TDateTime read FTestDate write FTestDate;
    property captestprop: string read Fcaptestprop write Fcaptestprop;
    property FirstName: string read FFirstName write FFirstName;
    property HasPropTrue: Boolean read FHasPropTrue write FHasPropTrue;
    property TestEnum: TTestEnumeration read FTestEnum write FTestEnum;
    property Tests: TTests read FTests write FTests;
    property Age: Integer read FAge write FAge;
  end;
  {$M-}

implementation

end.
