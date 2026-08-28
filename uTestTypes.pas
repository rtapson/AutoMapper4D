unit uTestTypes;

{$IFDEF FPC}
  {$MODE Delphi}
  {$H+}
{$ENDIF}

interface

type
  TTestEnumeration = (teFirst, teSecond);

  TTests = set of TTestEnumeration;

implementation

end.
