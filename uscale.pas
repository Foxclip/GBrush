unit UScale;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Math, UGlobalPointers;

type

  TDoublePoint = record
    x, y: double;
  end;

  TDoubleRect = record
    x1, y1, x2, y2: double;
  end;

  TPointArray = array of TPoint;
  TDoublePointArray = array of TDoublePoint;

const
  ScaleMultiplier = 1.1;

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
procedure RectScale(x1, y1, x2, y2: double; exact: boolean);

implementation

function W2SX(x: double): int64;
begin
  Result := trunc((x - Offset.x) * Scale);
end;

function W2SY(y: double): int64;
begin
  Result := trunc((y - Offset.y) * Scale);
end;

function S2WX(x: int64): double;
begin
  Result := x / Scale + Offset.x;
end;

function S2WY(y: int64): double;
begin
  Result := y / Scale + Offset.y;
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

procedure RectScale(x1, y1, x2, y2: double; exact: boolean);
begin
  Scale *= Min((GlobalWidth / Scale) / (x2 - x1),
    (GlobalHeight / Scale) / (y2 - y1));
  if not exact then
    Scale /= ScaleMultiplier;
  Offset.x := x1 + (x2 - x1) / 2 - GlobalWidth / Scale / 2;
  Offset.y := y1 + (y2 - y1) / 2 - GlobalHeight / Scale / 2;
end;

end.
