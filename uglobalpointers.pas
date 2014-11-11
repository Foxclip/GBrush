unit UGlobalPointers;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics;

type

  PointerToMethod = procedure of object;

var
  GlobalPenColor: TColor;
  GlobalBrushColor: TColor;
  GlobalIsMouseDownLeft, GlobalIsMouseDownMiddle: boolean;
  GlobalMousePoint: TPoint;
  GlobalInitUi: PointerToMethod;
  GlobalWidth, GlobalHeight: integer;

implementation

end.

