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
    BrushStyleChoice: TComboBox;
    AngleLabel: TLabel;
    CornerPanel: TPanel;
    SpeedButton1: TSpeedButton;
    ToolPanel: TPanel;
    ScrollBarSide: TScrollBar;
    ScrollBarBottom: TScrollBar;
    SizeLabel: TLabel;
    BrushStyleLabel: TLabel;
    Pallet: TDrawGrid;
    FirstColorImage: TImage;
    SecondColorImage: TImage;
    MainPaintBox: TPaintBox;
    InstrumentPanel: TPanel;
    ColorChangeButton: TSpeedButton;
    AngleNumSpinEdit: TSpinEdit;
    PenSizeSpinEdit: TSpinEdit;
    procedure AngleNumEditingDone(Sender: TObject);
    procedure BrushStyleChoiceSelect(Sender: TObject);
    procedure ColorChangeButtonClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure PenSizeSpinEditEditingDone(Sender: TObject);
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
    procedure PenSizeEditEditingDone(Sender: TObject);
    procedure SecondColorImageMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure MouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure Paint(Sender: TObject);
    procedure InstrumentButtonClick(Sender: TObject);
    procedure CreateControls;
    procedure CreateInstrumentButton(tool: TTool);
    procedure SetPenColor(col: Tcolor);
    procedure SetBrushColor(col: Tcolor);
    procedure UpdateColors;
    procedure SetPenStyle(style: TPenStyle);
    procedure SetBrushStyle(style: TBrushStyle);
    procedure UpdateScrollBars;
    procedure UpdateGlobalRectangle;
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
  CreateControls;
  SelectedTool := 0;
  GlobalPenColor := clBlack;
  GlobalBrushColor := clWhite;
  GlobalBrushStyle := bsClear;
  GlobalPenSize := 1;
  GlobalAngleNum := 6;
  GlobalWidth := MainPaintBox.Width;
  GlobalHeight := MainPaintBox.Height;
  SetScale(1);
  FieldBoundingBox.x2 := MainPaintBox.Width;
  FieldBoundingBox.y2 := MainPaintBox.Height;
  PalletColors[0, 0] := clRed;
  PalletColors[1, 0] := TColor($00FF00);
  PalletColors[2, 0] := clBlue;
  UpdateColors;
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

procedure TMainForm.PenSizeEditEditingDone(Sender: TObject);
begin
  try
    GlobalPenSize := StrToInt(PenSizeSpinEdit.Text);
  except
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
  AngleLabel.Visible := IsVisibleElement;
  AngleNumSpinEdit.Visible := IsVisibleElement;
  SizeLabel.Visible := IsVisibleElement;
  PenSizeSpinEdit.Visible := IsVisibleElement;
  BrushStyleLabel.Visible := IsVisibleElement;
  BrushStyleChoice.Visible := IsVisibleElement;
  FirstColorImage.Visible := IsVisibleElement;
  SecondColorImage.Visible := IsVisibleElement;
  ColorChangeButton.Visible := IsVisibleElement;
end;

procedure TMainForm.PenSizeSpinEditEditingDone(Sender: TObject);
begin
  GlobalPenSize := PenSizeSpinEdit.Value;
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

procedure TMainForm.BrushStyleChoiceSelect(Sender: TObject);
begin
  case BrushStyleChoice.ItemIndex of
    0: SetBrushStyle(bsSolid);
    1: SetBrushStyle(bsClear);
  end;
end;

procedure TMainForm.AngleNumEditingDone(Sender: TObject);
begin
  GlobalAngleNum := AngleNumSpinEdit.Value;
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

procedure TMainForm.SetPenStyle(style: TPenStyle);
begin
  GlobalPenStyle := style;
end;

procedure TMainForm.SetBrushStyle(style: TBrushStyle);
begin
  GlobalBrushStyle := style;
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
    TextOut(0, 0, 'Масштаб: ' + FloatToStr(GetScale) +
      '; Э x: ' + FloatToStr(Offset.x) + '; Э y: ' + FloatToStr(Offset.y));
    TextOut(0, 20, GlobalNote);
  end;
end;

procedure TMainForm.CreateControls;
var
  i: integer;
begin
  for i := Low(ToolArray) to High(ToolArray) do
    CreateInstrumentButton(ToolArray[i]);
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
