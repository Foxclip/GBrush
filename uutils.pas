unit UUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  TDoublePoint = record
    x, y: double;
  end;

function D2P(x, y: double): TDoublePoint;

implementation

function D2P(x, y: double): TDoublePoint;
var
  temp: TDoublePoint;
begin
  Result.x := x;
  Result.y := y;
end;

end.

