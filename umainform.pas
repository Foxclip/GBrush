unit UMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons, Grids, Spin, UFigures, UTools,
  UScale, types, Math, UGlobalPointers;

type

  { TMainForm }

  TMainForm = class(TForm)
    ColorDialog1: TColorDialog;
    CornerPanel: TPanel;
    PropertyPanel: TPanel;
    FullExtendButton: TSpeedButton;
    ClearButton: TSpeedButton;
    ToolPanel: TPanel;
    ScrollBarSide: TScrollBar;
    ScrollBarBottom: TScrollBar;
    Pallet: TDrawGrid;
    FirstColorImage: TImage;
    SecondColorImage: TImage;
    MainPaintBox: TPaintBox;
    InstrumentPanel: TPanel;
    ColorChangeButton: TSpeedButton;
    procedure ClearButtonClick(Sender: TObject);
    procedure ColorChangeButtonClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure ScrollBarScroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: integer);
    procedure OrigialSizeClick(Sender: TObject);
    procedure WheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: boolean);
    procedure WheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: boolean);
    procedure PalletDrawCell(Sender: TObject; aCol, aRow: integer;
      aRect: TRect; aState: TGridDrawState);
    procedure PalletMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure FormCreate(Sender: TObject);
    procedure FirstColorImageMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure SecondColorImageMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure MouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure Paint(Sender: TObject);
    procedure InstrumentButtonClick(Sender: TObject);
    procedure CreateToolControls;
    procedure CreateInstrumentButton(tool: TTool);
    procedure SetPenColor(col: Tcolor);
    procedure SetBrushColor(col: Tcolor);
    procedure UpdateColors;
    procedure UpdateScrollBars;
    procedure UpdateGlobalRectangle;
    procedure InitPropertiesEdit;
    //procedure PropertyChange;
  end;

const
  ButtonSize = 40;
  ButtonDist = 10;
  MinWidth = 10;
  MinHeight = 10;

var
  ButtonCols: integer;
  MainForm: TMainForm;
  PalletColors: array[0..9, 0..3] of TColor;
  InstrumentButtons, ToolButtons: array of TSpeedButton;
  SpeedButtonClick: PointerToMethod;
  MouseLastX, MouseLastY: integer;

implementation

{$R *.lfm}

procedure TMainForm.MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
var
  MousePoint: TDoublePoint;
begin
  MousePoint.x := S2WX(X);
  MousePoint.y := S2WY(Y);
  if (Button = mbLeft) or (Button = mbRight) then
  begin
    GlobalIsMouseDownLeft := True;
    ToolArray[SelectedTool].MouseDown(
      Sender, Button, Shift, MousePoint);
  end;
  if Button = mbMiddle then
  begin
    GlobalIsMouseDownMiddle := True;
    MouseLastX := X;
    MouseLastY := Y;
  end;
  MainPaintBox.Invalidate;
end;

procedure TMainForm.MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: integer);
var
  MousePoint: TDoublePoint;
begin
  GlobalMousePoint.X := X;
  GlobalMousePoint.Y := Y;
  MousePoint.x := S2WX(X);
  MousePoint.y := S2WY(Y);
  ToolArray[SelectedTool].MouseMove(
    Sender, Shift, MousePoint);
  if GlobalIsMouseDownMiddle then
  begin
    Offset.x -= (X - MouseLastX) / GetScale;
    Offset.y -= (Y - MouseLastY) / GetScale;
    MouseLastX := X;
    MouseLastY := Y;
    UpdateScrollBars;
  end;
  MainPaintBox.Invalidate;
end;

procedure TMainForm.MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
var
  MousePoint: TDoublePoint;
begin
  MousePoint.x := S2WX(X);
  MousePoint.y := S2WY(Y);
  if Button = mbLeft then
    GlobalIsMouseDownLeft := False;
  if (Button = mbLeft) or (Button = mbRight) then
  begin
    ToolArray[SelectedTool].MouseUp(Sender,
      Button, Shift, MousePoint);
  end;
  if Button = mbMiddle then
    GlobalIsMouseDownMiddle := False;
  MainPaintBox.Invalidate;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  DoubleBuffered := True;
  Constraints.MinWidth := MinWidth;
  Constraints.MinHeight := MinHeight;
  ButtonCols := InstrumentPanel.Width div ButtonSize;
  GlobalUpdateScrollBars := @UpdateScrollBars;
  UpdateFieldBoundingBox := @UpdateGlobalRectangle;
  CreateToolControls;
  SelectedTool := 0;
  GlobalPenColor := clBlack;
  GlobalBrushColor := clWhite;
  GlobalWidth := MainPaintBox.Width;
  GlobalHeight := MainPaintBox.Height;
  SetScale(1);
  FieldBoundingBox.x2 := MainPaintBox.Width;
  FieldBoundingBox.y2 := MainPaintBox.Height;
  PalletColors[0, 0] := clRed;
  PalletColors[1, 0] := TColor($00FF00);
  PalletColors[2, 0] := clBlue;
  UpdateColors;
  InitPropertiesEdit;
end;

procedure TMainForm.FirstColorImageMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  if ColorDialog1.Execute then
  begin
    GlobalPenColor := ColorDialog1.Color;
    UpdateColors;
  end;
end;

procedure TMainForm.SecondColorImageMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  if ColorDialog1.Execute then
  begin
    GlobalBrushColor := ColorDialog1.Color;
    UpdateColors;
  end;
end;

procedure TMainForm.InstrumentButtonClick(Sender: TObject);
begin
  SelectedTool := (Sender as TSpeedButton).Tag;
  InitPropertiesEdit;
end;

procedure TMainForm.PalletDrawCell(Sender: TObject; aCol, aRow: integer;
  aRect: TRect; aState: TGridDrawState);
begin
  Pallet.Canvas.Brush.Color := PalletColors[aCol, aRow];
  Pallet.Canvas.FillRect(aRect);
end;

procedure TMainForm.ColorChangeButtonClick(Sender: TObject);
var
  temp: TColor;
begin
  temp := GlobalPenColor;
  SetPenColor(GlobalBrushColor);
  SetBrushColor(temp);
  UpdateColors;
end;

procedure TMainForm.ClearButtonClick(Sender: TObject);
var
  i: integer;
begin
  for i := Low(FigureArray) to High(FigureArray) do
    FigureArray[i].Free;
  SetLength(FigureArray, 0);
  TempFigure := nil;
  TempFigure.Free;
  UpdateGlobalRectangle;
  MainPaintBox.Invalidate;
end;

procedure TMainForm.FormResize(Sender: TObject);
var
  IsVisibleElement: boolean;
begin
  GlobalWidth := MainPaintBox.Width;
  GlobalHeight := MainPaintBox.Height;
  //Палитра
  if (Width < InstrumentPanel.Width + Pallet.Width * 2) or (Height < 300) then
    Pallet.Visible := False
  else
    Pallet.Visible := True;
  //Панель
  if (Width < InstrumentPanel.Width * 2) or
    (Height < ((Length(ToolArray) div ButtonCols) + 1) *
    ButtonSize + ButtonDist) then
    InstrumentPanel.Visible := False
  else
    InstrumentPanel.Visible := True;
  //Параметры
  if Height < (Length(ToolArray) div ButtonCols) * ButtonSize +
  ButtonDist + 300 then
    IsVisibleElement := False
  else
    IsVisibleElement := True;
  FirstColorImage.Visible := IsVisibleElement;
  SecondColorImage.Visible := IsVisibleElement;
  ColorChangeButton.Visible := IsVisibleElement;
end;

procedure TMainForm.ScrollBarScroll(Sender: TObject;
  ScrollCode: TScrollCode; var ScrollPos: integer);
var
  ox, oy: double;
  bx1, by1, bx2, by2: double;
  w, h: double;
  TempBoundingBox: TDoubleRect;
begin
  ox := Offset.x;
  oy := Offset.y;
  bx1 := FieldBoundingBox.x1;
  by1 := FieldBoundingBox.y1;
  bx2 := FieldBoundingBox.x2;
  by2 := FieldBoundingBox.y2;
  w := ox + MainPaintBox.Width / GetScale;
  h := oy + MainPaintBox.Height / GetScale;
  TempBoundingBox.x1 := Math.Min(ox, bx1);
  TempBoundingBox.y1 := Math.Min(oy, by1);
  TempBoundingBox.x2 := Math.Max(w, bx2);
  TempBoundingBox.y2 := Math.Max(h, by2);
  case ScrollCode of
    scTrack:
    begin
      if Sender = ScrollBarBottom then
        Offset.x := (ScrollPos * (TempBoundingBox.x2 -
          TempBoundingBox.x1)) / ScrollBarBottom.Max + TempBoundingBox.x1;
      if Sender = ScrollBarSide then
        Offset.y := (ScrollPos * (TempBoundingBox.y2 -
          TempBoundingBox.y1)) / ScrollBarSide.Max + TempBoundingBox.y1;
    end;
    scEndScroll:
      UpdateScrollBars;
  end;
  MainPaintBox.Invalidate;
end;

procedure TMainForm.OrigialSizeClick(Sender: TObject);
begin
  with FieldBoundingBox do
    RectScale(x1, y1, x2, y2, False);
  MainPaintBox.Invalidate;
end;

procedure TMainForm.WheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: boolean);
begin
  SetScale(GetScale * ScaleMultiplier);
  Offset.x += GlobalMousePoint.x * (ScaleMultiplier - 1) / GetScale;
  Offset.y += GlobalMousePoint.y * (ScaleMultiplier - 1) / GetScale;
  MainPaintBox.Invalidate;
end;

procedure TMainForm.WheelDown(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: boolean);
begin
  SetScale(GetScale / ScaleMultiplier);
  Offset.x -= GlobalMousePoint.x * (ScaleMultiplier - 1) /
    (ScaleMultiplier * GetScale);
  Offset.y -= GlobalMousePoint.y * (ScaleMultiplier - 1) /
    (ScaleMultiplier * GetScale);
  MainPaintBox.Invalidate;
end;

procedure TMainForm.PalletMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);

var
  tp: TPoint;
begin
  tp.x := X;
  tp.y := Y;
  if Button = mbLeft then
  begin
    SetPenColor(PalletColors[Pallet.MouseToCell(tp).x]
      [Pallet.MouseToCell(tp).y]);
    UpdateColors;
  end;
  if Button = mbRight then
    if ColorDialog1.Execute then
    begin
      PalletColors[Pallet.MouseToCell(tp).x]
        [Pallet.MouseToCell(tp).y] := ColorDialog1.Color;
      Pallet.Invalidate;
    end;
end;

procedure TMainForm.UpdateColors;
begin
  FirstColorImage.Canvas.Brush.Color := GlobalPenColor;
  FirstColorImage.Canvas.Rectangle(0, 0, FirstColorImage.Width,
    FirstColorImage.Height);
  SecondColorImage.Canvas.Brush.Color := GlobalBrushColor;
  SecondColorImage.Canvas.Rectangle(0, 0, SecondColorImage.Width,
    SecondColorImage.Height);
end;

procedure TMainForm.UpdateScrollBars;
var
  ox, oy: double;
  bx1, by1, bx2, by2: double;
  w, h: double;
  TempBoundingBox: TDoubleRect;
begin
  ox := Offset.x;
  oy := Offset.y;
  bx1 := FieldBoundingBox.x1;
  by1 := FieldBoundingBox.y1;
  bx2 := FieldBoundingBox.x2;
  by2 := FieldBoundingBox.y2;
  w := ox + MainPaintBox.Width / GetScale;
  h := oy + MainPaintBox.Height / GetScale;
  TempBoundingBox.x1 := Math.Min(ox, bx1);
  TempBoundingBox.y1 := Math.Min(oy, by1);
  TempBoundingBox.x2 := Math.Max(w, bx2);
  TempBoundingBox.y2 := Math.Max(h, by2);
  ScrollBarBottom.PageSize :=
    trunc(((MainPaintBox.Width / GetScale) /
    (TempBoundingBox.x2 - TempBoundingBox.x1)) * ScrollBarBottom.Max);
  ScrollBarSide.PageSize :=
    trunc(((MainPaintBox.Height / GetScale) /
    (TempBoundingBox.y2 - TempBoundingBox.y1)) * ScrollBarSide.Max);
  ScrollBarBottom.Position :=
    trunc(((ox - TempBoundingBox.x1) / (TempBoundingBox.x2 -
    TempBoundingBox.x1)) * ScrollBarBottom.Max);
  ScrollBarSide.Position :=
    trunc(((oy - TempBoundingBox.y1) / (TempBoundingBox.y2 -
    TempBoundingBox.y1)) * ScrollBarSide.Max);
  ScrollBarBottom.Update;
  ScrollBarSide.Update;
end;

procedure TMainForm.UpdateGlobalRectangle;
var
  i: integer;
begin
  if Length(FigureArray) > 0 then
  begin
    FieldBoundingBox.x1 := FigureArray[0].BoundingBox().x1;
    FieldBoundingBox.y1 := FigureArray[0].BoundingBox().y1;
    FieldBoundingBox.x2 := FigureArray[0].BoundingBox().x2;
    FieldBoundingBox.y2 := FigureArray[0].BoundingBox().y2;
    for i := Low(FigureArray) to High(FigureArray) do
    begin
      { TODO : Заменить на min и max }
      if FigureArray[i].BoundingBox.x1 < FieldBoundingBox.x1 then
        FieldBoundingBox.x1 := FigureArray[i].BoundingBox.x1;
      if FigureArray[i].BoundingBox.x2 > FieldBoundingBox.x2 then
        FieldBoundingBox.x2 := FigureArray[i].BoundingBox.x2;
      if FigureArray[i].BoundingBox.y1 < FieldBoundingBox.y1 then
        FieldBoundingBox.y1 := FigureArray[i].BoundingBox.y1;
      if FigureArray[i].BoundingBox.y2 > FieldBoundingBox.y2 then
        FieldBoundingBox.y2 := FigureArray[i].BoundingBox.y2;
    end;
  end;
  UpdateScrollBars;
end;

procedure TMainForm.SetPenColor(col: Tcolor);
begin
  GlobalPenColor := col;
end;

procedure TMainForm.SetBrushColor(col: Tcolor);
begin
  GlobalBrushColor := col;
end;

procedure TMainForm.Paint(Sender: TObject);
var
  i: integer;
begin
  with MainPaintBox.Canvas do
  begin
    Pen.Color := clWhite;
    Brush.Color := clWhite;
    Brush.Style := bsSolid;
    Rectangle(0, 0, Width, Height);
    if Length(FigureArray) > 0 then
      for i := 0 to High(FigureArray) do
        FigureArray[i].Draw(MainPaintBox.Canvas);
    if TempFigure <> nil then
      TempFigure.Draw(MainPaintBox.Canvas);
    Pen.Color := clRed;
    Pen.Width := 1;
    Brush.Style := bsClear;
    TextOut(0, 0, 'Масштаб: ' + FormatFloat('0.000', GetScale) +
      '; Э x: ' + FormatFloat('0.000', Offset.x) + '; Э y: ' +
      FormatFloat('0.000', Offset.y));
    TextOut(0, 20, BoolToStr(TempFigure = nil));
  end;
end;

procedure TMainForm.CreateToolControls;
var
  i: integer;
begin
  for i := Low(ToolArray) to High(ToolArray) do
    CreateInstrumentButton(ToolArray[i]);
end;

procedure TMainForm.InitPropertiesEdit;
var
  i: integer;
begin
  while PropertyPanel.ControlCount > 0 do
    PropertyPanel.Controls[0].Free;
  with ToolArray[SelectedTool] do
    for i := Low(Properties) to High(Properties) do
    begin
      Properties[i].CreateEdit(PropertyPanel, PropertyPanel.ControlCount);
    end;
end;

procedure TMainForm.CreateInstrumentButton(tool: TTool);
var
  NewButton: TSpeedButton;
  i: integer;
begin
  SetLength(InstrumentButtons, Length(InstrumentButtons) + 1);
  NewButton := TSpeedButton.Create(InstrumentPanel);
  InstrumentButtons[High(InstrumentButtons)] := NewButton;
  with NewButton do
  begin
    OnClick := @InstrumentButtonClick;
    Parent := InstrumentPanel;
    i := High(InstrumentButtons);
    Tag := i;
    GroupIndex := 1;
    SetBounds((i mod ButtonCols) * ButtonSize + ButtonDist,
      (i div ButtonCols) * ButtonSize + ButtonDist, ButtonSize, ButtonSize);
    if i = 0 then
      Down := True;
    Glyph.LoadFromFile(tool.GetGlyphString);
  end;
end;

initialization

end.
