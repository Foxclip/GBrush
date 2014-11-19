unit UOtherTools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Controls, Dialogs, Buttons, UFigures,
  ExtCtrls, UScale, UTools, UProperties, UGlobalPointers, Math, UUtils;

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

  TSelectTool = class(TTool)
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
      TempZoomRectangle := TRectangle.Create(Properties);
      TempZoomRectangle.Point1 := MousePoint;
      TempZoomRectangle.Point2 := MousePoint;
      with TempZoomRectangle do
      begin
        AddProperty(TPenColorProperty.Create(clBlack));
        AddProperty(TPenWidthProperty.Create(1));
        AddProperty(TPenStyleProperty.Create(psDash));
        AddProperty(TBrushStyleProperty.Create(bsClear));
      end;
      TempFigure := TempZoomRectangle;
    end
    else
      TempZoomRectangle.Point2 := MousePoint;
  end;
end;

procedure TZoomTool.MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; MousePoint: TDoublePoint);
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

//Выделение

procedure TSelectTool.MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; MousePoint: TDoublePoint);
var
  i: integer;
  Count: integer;
begin
  IsMouseDragged := False;
  if not GlobalIsKeyDownControl then
    for i := Low(FigureArray) to High(FigureArray) do
      FigureArray[i].IsSelected := False;
  for i := High(FigureArray) downto Low(FigureArray) do
  begin
    if FigureArray[i].IsPointInRegion(W2S(MousePoint)) then
    begin
      if Button = mbLeft then
        FigureArray[i].IsSelected := True
      else if Button = mbRight then
        FigureArray[i].IsSelected := False;
      Exit;
    end;
  end;
end;

procedure TSelectTool.MouseMove(Sender: TObject; Shift: TShiftState;
  MousePoint: TDoublePoint);
begin
end;

procedure TSelectTool.MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; MousePoint: TDoublePoint);
begin

end;

function TSelectTool.GetGlyphString: string;
begin
  Result := 'glyphs/select.bmp';
end;

initialization

  RegisterTool(THandTool.Create);
  RegisterTool(TZoomTool.Create);
  RegisterTool(TSelectTool.Create);

end.
