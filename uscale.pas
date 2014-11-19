unit UScale;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Math, UGlobalPointers, UUtils;

type

  TDoubleRect = record
    x1, y1, x2, y2: double;
  end;

  TPointArray = array of TPoint;
  TDoublePointArray = array of TDoublePoint;

const
  ScaleMultiplier = 1.1;
  MinScale = 0.000001;
  MaxScale = 1000000;

var
  Offset: TDoublePoint;
  Scale: double;
  UpdateFieldBoundingBox: PointerToMethod;
  FieldBoundingBox: TDoubleRect;
  GlobalUpdateScrollBars: PointerToMethod;

function W2SX(x: double): int64;
function W2SY(y: double): int64;
function S2WX(x: int64): double;
function S2WY(y: int64): double;
function S2W(src: TPoint): TDoublePoint;
function W2S(src: TDoublePoint): TPoint;
function W2SArray(src: TDoublePointArray): TPointArray;
procedure SetScale(newscale: double);
function GetScale(): double;
procedure RectScale(x1, y1, x2, y2: double; exact: boolean);

implementation

function W2SX(x: double): int64;
begin
  Result := trunc((x - Offset.x) * GetScale);
end;

function W2SY(y: double): int64;
begin
  Result := trunc((y - Offset.y) * GetScale);
end;

function S2WX(x: int64): double;
begin
  Result := x / GetScale + Offset.x;
end;

function S2WY(y: int64): double;
begin
  Result := y / GetScale + Offset.y;
end;

function W2SArray(src: TDoublePointArray): TPointArray;
var
  i: integer;
  dst: TPointArray;
begin
  SetLength(dst, Length(src));
  for i := Low(src) to High(src) do
  begin
    dst[i].X := W2SX(src[i].x);
    dst[i].Y := W2SY(src[i].y);
  end;
  Result := dst;
end;

function S2W(src: TPoint): TDoublePoint;
var
  dst: TDoublePoint;
begin
  dst.x := S2WX(src.X);
  dst.y := S2WY(src.Y);
  Result := dst;
end;

function W2S(src: TDoublePoint): TPoint;
var
  dst: TPoint;
begin
  dst.X := W2SX(src.x);
  dst.Y := W2SY(src.y);
  Result := dst;
end;

procedure SetScale(newscale: double);
begin
  if newscale > MaxScale then
    Scale := MaxScale
  else if newscale < MinScale then
    Scale := MinScale
  else
    Scale := newscale;
  GlobalUpdateScrollBars;
end;

function GetScale(): double;
begin
  Result := Scale;
end;

procedure RectScale(x1, y1, x2, y2: double; exact: boolean);
begin
  if exact then
  begin
    SetScale(GetScale * (Max((GlobalWidth / GetScale) / (x2 - x1),
      (GlobalHeight / GetScale) / (y2 - y1))));
  end
  else
    SetScale(GetScale * (Min((GlobalWidth / GetScale) / (x2 - x1),
      (GlobalHeight / GetScale) / (y2 - y1))) / ScaleMultiplier);
  Offset.x := x1 + (x2 - x1) / 2 - GlobalWidth / GetScale / 2;
  Offset.y := y1 + (y2 - y1) / 2 - GlobalHeight / GetScale / 2;
  GlobalUpdateScrollBars;
end;

end.
