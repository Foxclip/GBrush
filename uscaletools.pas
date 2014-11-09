unit UScaleTools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Controls, Dialogs, Buttons, UFigures,
  ExtCtrls, UScale, UTools, UGlobalPointers, Math;

type

  THandTool = class(TTool)
    MouseLastX, MouseLastY: integer;
    procedure MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; MousePoint: TDoublePoint); override;
    procedure MouseMove(Sender: TObject; Shift: TShiftState;
      MousePoint: TDoublePoint); override;
    procedure MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; MousePoint: TDoublePoint); override;
    function GetGlyphString: string; override;
  end;

  TZoomTool = class(TTool)
    TempZoomRectangle: TRectangle;
    IsMouseDragged: boolean;
    procedure MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; MousePoint: TDoublePoint); override;
    procedure MouseMove(Sender: TObject; Shift: TShiftState;
      MousePoint: TDoublePoint); override;
    procedure MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; MousePoint: TDoublePoint); override;
    function GetGlyphString: string; override;
  end;

const
  ScaleMultiplier = 1.5;

implementation

//Рука

procedure THandTool.MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; MousePoint: TDoublePoint);
begin
  if GlobalIsMouseDownLeft then
  begin
    MouseLastX := GlobalMousePoint.x;
    MouseLastY := GlobalMousePoint.y;
  end;
end;

procedure THandTool.MouseMove(Sender: TObject; Shift: TShiftState;
  MousePoint: TDoublePoint);
begin
  if GlobalIsMouseDownLeft then
  begin
    Offset.x -= (GlobalMousePoint.x - MouseLastX) / Scale;
    Offset.y -= (GlobalMousePoint.y - MouseLastY) / Scale;
    MouseLastX := GlobalMousePoint.x;
    MouseLastY := GlobalMousePoint.y;
  end;
  GlobalUpdateScrollBars;
end;

procedure THandTool.MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; MousePoint: TDoublePoint);
begin

end;

function THandTool.GetGlyphString: string;
begin
  Result := 'glyphs/hand.bmp';
end;

//Лупа

procedure TZoomTool.MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; MousePoint: TDoublePoint);
begin
  IsMouseDragged := False;
end;

procedure TZoomTool.MouseMove(Sender: TObject; Shift: TShiftState;
  MousePoint: TDoublePoint);
begin
  if GlobalIsMouseDownLeft then
  begin
    IsMouseDragged := True;
    if TempFigure = nil then
    begin
      TempZoomRectangle := TRectangle.Create(MousePoint);
      with TempZoomRectangle do
      begin
        PenColor := clBlack;
        PenSize := 1;
        PenStyle := psDash;
        BrushStyle := bsClear;
      end;
      TempFigure := TempZoomRectangle;
    end
    else
      TempZoomRectangle.Point2 := MousePoint;
  end;
end;

procedure TZoomTool.MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; MousePoint: TDoublePoint);
var
  temp: double;
begin
  if not IsMouseDragged then
  begin
    if Button = mbLeft then
    begin
      Scale *= ScaleMultiplier;
      Offset.x += GlobalMousePoint.x * (ScaleMultiplier - 1) / Scale;
      Offset.y += GlobalMousePoint.y * (ScaleMultiplier - 1) / Scale;
    end;
    if Button = mbRight then
    begin
      Scale /= ScaleMultiplier;
      Offset.x -= GlobalMousePoint.x * (ScaleMultiplier - 1) /
        (ScaleMultiplier * Scale);
      Offset.y -= GlobalMousePoint.y * (ScaleMultiplier - 1) /
        (ScaleMultiplier * Scale);
    end;
  end
  else
  begin
    with TempZoomRectangle do
    begin
      RectScale(
        Min(Point1.x, Point2.x),
        Min(Point1.y, Point2.y),
        Max(Point1.x, Point2.x),
        Max(Point1.y, Point2.y),
        True);
    end;
    TempFigure := nil;
  end;
  GlobalUpdateScrollBars;
end;

function TZoomTool.GetGlyphString: string;
begin
  Result := 'glyphs/zoom.bmp';
end;

initialization

  RegisterTool(THandTool.Create);
  RegisterTool(TZoomTool.Create);

end.
