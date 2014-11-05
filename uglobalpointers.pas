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
  GlobalPenStyle: TPenStyle;
  GlobalBrushStyle: TBrushStyle;
  GlobalPenSize: Integer;
  GlobalAngleNum: Integer;
  GlobalIsMouseDownLeft, GlobalIsMouseDownMiddle: Boolean;
  GlobalMousePoint: TPoint;
  GlobalWidth, GlobalHeight: Integer;
  GlobalNote: String;

implementation

end.

