unit UMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons, Grids, Spin, UFigures, UTools,
  UScale, types, Math, UGlobalPointers, UUtils;

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
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
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
    procedure MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: integer);
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
    function EnhanceBoundingBox(subj, obj: TDoubleRect): TDoubleRect;
  end;

const
  ButtonSize = 40;
  ButtonDist = 10;
  MinWidth = 10;
  MinHeight = 10;
  SelectBoxOffset = 5;

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
  if GlobalIsMouseDownLeft then
    UpdateFieldBoundingBox;
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
  GlobalCanvasInvalidate := @MainPaintBox.Invalidate;
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
  if TempFigure <> nil then
  begin
    AddFigure(TempFigure);
    TempFigure := nil;
  end;
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

procedure TMainForm.FormKeyDown(Sender: TObject; var Key: word;
  Shift: TShiftState);
begin
  case Key of
    17: GlobalIsKeyDownControl := True;
  end;
end;

procedure TMainForm.FormKeyUp(Sender: TObject; var Key: word;
  Shift: TShiftState);
begin
  case Key of
    17: GlobalIsKeyDownControl := False;
  end;
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
  TempBoundingBox: TDoubleRect;
begin
  with TempBoundingBox do
  begin
    x1 := Math.Min(Offset.x, FieldBoundingBox.x1);
    y1 := Math.Min(Offset.y, FieldBoundingBox.y1);
    x2 := Math.Max(Offset.x + MainPaintBox.Width / GetScale,
      FieldBoundingBox.x2);
    y2 := Math.Max(Offset.y + MainPaintBox.Height / GetScale,
      FieldBoundingBox.y2);
    case ScrollCode of
      scTrack:
      begin
        if Sender = ScrollBarBottom then
          Offset.x := (ScrollPos * (x2 - x1)) / ScrollBarBottom.Max + x1;
        if Sender = ScrollBarSide then
          Offset.y := (ScrollPos * (y2 - y1)) / ScrollBarSide.Max + y1;
      end;
      scEndScroll:
        UpdateScrollBars;
    end;
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
  TempBoundingBox: TDoubleRect;
begin
  with TempBoundingBox do
  begin
    x1 := Math.Min(Offset.x, FieldBoundingBox.x1);
    y1 := Math.Min(Offset.y, FieldBoundingBox.y1);
    x2 := Math.Max(Offset.x + MainPaintBox.Width / GetScale,
      FieldBoundingBox.x2);
    y2 := Math.Max(Offset.y + MainPaintBox.Height / GetScale,
      FieldBoundingBox.y2);
    ScrollBarBottom.PageSize :=
      trunc(((MainPaintBox.Width / GetScale) / (x2 - x1)) *
      ScrollBarBottom.Max);
    ScrollBarSide.PageSize :=
      trunc(((MainPaintBox.Height / GetScale) / (y2 - y1)) *
      ScrollBarSide.Max);
    ScrollBarBottom.Position :=
      trunc(((Offset.x - x1) / (x2 - x1)) * ScrollBarBottom.Max);
    ScrollBarSide.Position :=
      trunc(((Offset.y - y1) / (y2 - y1)) * ScrollBarSide.Max);
  end;
  ScrollBarBottom.Update;
  ScrollBarSide.Update;
end;

procedure TMainForm.UpdateGlobalRectangle;
var
  i: integer;
begin
  if TempFigure <> nil then
    FieldBoundingBox := TempFigure.BoundingBox;
  if Length(FigureArray) > 0 then
  begin
    for i := Low(FigureArray) to High(FigureArray) do
      FieldBoundingBox := EnhanceBoundingBox(FigureArray[i].BoundingBox,
        FieldBoundingBox);
  end;
  if TempFigure <> nil then
    FieldBoundingBox := EnhanceBoundingBox(TempFigure.BoundingBox,
      FieldBoundingBox);
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
    for i := Low(FigureArray) to High(FigureArray) do
    begin
      FigureArray[i].Draw(MainPaintBox.Canvas);
    end;
    for i := Low(FigureArray) to High(FigureArray) do
    begin
      if FigureArray[i].IsSelected then
      begin
        Pen.Color := clBlue;
        Pen.Style := psDash;
        Pen.Width := 1;
        Brush.Style := bsClear;
        with FigureArray[i].BoundingBox do
        begin
          Rectangle(
            W2SX(x1 - SelectBoxOffset),
            W2SY(y1 - SelectBoxOffset),
            W2SX(x2 + SelectBoxOffset),
            W2SY(y2 + SelectBoxOffset)
            );
        end;
      end;
    end;
    if TempFigure <> nil then
      TempFigure.Draw(MainPaintBox.Canvas);
    Pen.Style := psSolid;
    Pen.Color := clRed;
    Pen.Width := 1;
    Brush.Style := bsClear;
    TextOut(0, 0, 'Масштаб: ' + FormatFloat('0.000', GetScale) +
      '; Э x: ' + FormatFloat('0.000', Offset.x) + '; Э y: ' +
      FormatFloat('0.000', Offset.y));
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

function TMainForm.EnhanceBoundingBox(subj, obj: TDoubleRect): TDoubleRect;
begin
  with obj do
  begin
    x1 := Min(subj.x1, obj.x1);
    y1 := Min(subj.y1, obj.y1);
    x2 := Max(subj.x2, obj.x2);
    y2 := Max(subj.y2, obj.y2);
  end;
  Result := obj;
end;

initialization

end.
