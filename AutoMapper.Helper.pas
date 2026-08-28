unit AutoMapper.Helper;

{$IFDEF FPC}
  {$MODE Delphi}
  {$H+}
{$ENDIF}

{
  Optional syntactic sugar: `SomeObject.Adapt<TTarget>`.

  This lives in its own unit on purpose. It is a class helper for TObject, and
  Delphi allows only ONE active helper per type in a given scope - so whichever
  unit appears later in the uses clause wins and the other's methods silently
  become invisible, with no compiler warning.

  Keeping it here means using the mapper does not cost a consumer their one
  TObject helper slot. Add this unit to the uses clause only if you want the
  sugar; otherwise call TAutoMapper<T>.Map directly, which is always available.
}

interface

uses
{$IFDEF FPC}
  Generics.Collections,
{$ELSE}
  System.Generics.Collections,
{$ENDIF}
  AutoMapper;

type
  AutoMapper4D = class helper for TObject
    function Adapt<T: class, constructor>: T; overload;
    procedure Adapt<T: class, constructor>(const DestObject: T); overload;
  end;

implementation

{ AutoMapper4D }

function AutoMapper4D.Adapt<T>: T;
begin
  //Routed through MapNew rather than the overloaded Map: FPC cannot resolve the
  //overload here, because T could itself be a TDictionary. MapNew still frees
  //the new object if mapping raises.
  Result := TAutoMapper<T>.MapNew(Self, nil);
end;

procedure AutoMapper4D.Adapt<T>(const DestObject: T);
begin
  TAutoMapper<T>.MapOnto(Self, DestObject, nil);
end;

end.
