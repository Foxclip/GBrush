unit UProperties;

{$mode objfpc}{$H+}

interface

uses
  Classes, ExtCtrls, Dialogs, StdCtrls, Spin, SysUtils, Graphics,
  UGlobalPointers;

type

  PointerToMethodLongInt = procedure(x: integer) of object;

  TProperty = class
    procedure PushProperty(canv: TCanvas); virtual; abstract;
    procedure CreateEdit(panel: TPanel; index: integer); virtual; abstract;
    procedure PropertyChange(Sender: TObject); virtual; abstract;
    function CreateCopy(): TProperty; virtual; abstract;
  end;

  PropertyArray = array of TProperty;

  TPenColorProperty = class(TProperty)
    PenColor: TColor;
    Dialog: TColorDialog;
    Button: TColorButton;
    procedure PushProperty(canv: TCanvas); override;
    procedure CreateEdit(panel: TPanel; index: integer); override;
    procedure PropertyChange(Sender: TObject); override;
    function CreateCopy(): TProperty; override;
    constructor Create(col: TColor);
  end;

  TBrushColorProperty = class(TProperty)
    BrushColor: TColor;
    Dialog: TColorDialog;
    Button: TColorButton;
    procedure PushProperty(canv: TCanvas); override;
    procedure CreateEdit(panel: TPanel; index: integer); override;
    procedure PropertyChange(Sender: TObject); override;
    function CreateCopy(): TProperty; override;
    constructor Create(col: TColor);
  end;

  TPenStyleProperty = class(TProperty)
    PenStyle: TPenStyle;
    procedure PushProperty(canv: TCanvas); override;
    procedure CreateEdit(panel: TPanel; index: integer); override;
    procedure PropertyChange(Sender: TObject); override;
    function CreateCopy(): TProperty; override;
    constructor Create(ps: TPenStyle);
  end;

  TPenWidthProperty = class(TProperty)
    PenWidth: integer;
    procedure PushProperty(canv: TCanvas); override;
    procedure CreateEdit(panel: TPanel; index: integer); override;
    procedure PropertyChange(Sender: TObject); override;
    function CreateCopy(): TProperty; override;
    constructor Create(w: integer);
  end;

  TBrushStyleProperty = class(TProperty)
    BrushStyle: TBrushStyle;
    procedure PushProperty(canv: TCanvas); override;
    procedure CreateEdit(panel: TPanel; index: integer); override;
    procedure PropertyChange(Sender: TObject); override;
    function CreateCopy(): TProperty; override;
    constructor Create(bs: TBrushStyle);
  end;

  TVerticesNumProperty = class(TProperty)
    Vertices: integer;
    BuildMethod: PointerToMethodLongInt;
    procedure PushProperty(canv: TCanvas); override;
    procedure CreateEdit(panel: TPanel; index: integer); override;
    procedure PropertyChange(Sender: TObject); override;
    function CreateCopy(): TProperty; override;
    constructor Create(vert: integer; build: PointerToMethodLongInt);
  end;

  TRoundProperty = class(TProperty)
    RoundX, RoundY: integer;
    procedure PushProperty(canv: TCanvas); override;
    procedure CreateEdit(panel: TPanel; index: integer); override;
    procedure PropertyChange(Sender: TObject); override;
    function CreateCopy(): TProperty; override;
    constructor Create(X, Y: integer);
  end;

const
  ItmHeight = 30;

var
  TempRoundX, TempRoundY: integer;
  TempVertices: integer;

implementation

constructor TPenColorProperty.Create(col: TColor);
begin
  PenColor := col;
end;

constructor TBrushColorProperty.Create(col: TColor);
begin
  BrushColor := col;
end;

constructor TPenStyleProperty.Create(ps: TPenStyle);
begin
  PenStyle := ps;
end;

constructor TPenWidthProperty.Create(w: integer);
begin
  PenWidth := w;
end;

constructor TBrushStyleProperty.Create(bs: TBrushStyle);
begin
  BrushStyle := bs;
end;

constructor TVerticesNumProperty.Create(vert: integer;
  build: PointerToMethodLongInt);
begin
  Vertices := vert;
  BuildMethod := build;
end;

constructor TRoundProperty.Create(X, Y: integer);
begin
  RoundX := X;
  RoundY := Y;
end;

procedure TPenColorProperty.PushProperty(canv: TCanvas);
begin
  canv.Pen.Color := PenColor;
end;

procedure TPenColorProperty.CreateEdit(panel: TPanel; index: integer);
begin
  Dialog := TColorDialog.Create(panel);
  Button := TColorButton.Create(panel);
  Button.OnColorChanged := @PropertyChange;
  Button.Parent := panel;
  Button.ColorDialog := Dialog;
  Button.Color := PenColor;
  Button.SetBounds(5, index * ItmHeight, Button.Width, Button.Height);
end;

procedure TPenColorProperty.PropertyChange(Sender: TObject);
begin
  PenColor := Button.ButtonColor;
  GlobalCanvasInvalidate;
end;

function TPenColorProperty.CreateCopy(): TProperty;
begin
  Result := TPenColorProperty.Create(PenColor);
end;

procedure TBrushColorProperty.PushProperty(canv: TCanvas);
begin
  canv.Brush.Color := BrushColor;
end;

procedure TBrushColorProperty.CreateEdit(panel: TPanel; index: integer);
begin
  Dialog := TColorDialog.Create(panel);
  Button := TColorButton.Create(panel);
  Button.OnColorChanged := @PropertyChange;
  Button.Parent := panel;
  Button.ColorDialog := Dialog;
  Button.Color := BrushColor;
  Button.SetBounds(5, index * ItmHeight, Button.Width, Button.Height);
end;

procedure TBrushColorProperty.PropertyChange(Sender: TObject);
begin
  BrushColor := Button.ButtonColor;
  GlobalCanvasInvalidate;
end;

function TBrushColorProperty.CreateCopy(): TProperty;
begin
  Result := TBrushColorProperty.Create(BrushColor);
end;

procedure TPenStyleProperty.PushProperty(canv: TCanvas);
begin
  canv.Pen.Style := PenStyle;
end;

procedure TPenStyleProperty.CreateEdit(panel: TPanel; index: integer);
var
  Edit: TComboBox;
begin
  Edit := TComboBox.Create(panel);
  with Edit do
  begin
    OnChange := @PropertyChange;
    Parent := panel;
    Items.Add('psSolid');
    Items.Add('psClear');
    Items.Add('psDash');
    Items.Add('psDot');
    Items.Add('psDashDot');
    Items.Add('psDashDotDot');
    ItemIndex := 0;
    SetBounds(5, index * ItmHeight, Width, Height);
  end;
end;

procedure TPenStyleProperty.PropertyChange(Sender: TObject);
begin
  case (Sender as TComboBox).ItemIndex of
    0: PenStyle := psSolid;
    1: PenStyle := psClear;
    2: PenStyle := psDash;
    3: PenStyle := psDot;
    4: PenStyle := psDashDot;
    5: PenStyle := psDashDotDot;
  end;
  GlobalCanvasInvalidate;
end;

function TPenStyleProperty.CreateCopy(): TProperty;
begin
  Result := TPenStyleProperty.Create(PenStyle);
end;

procedure TPenWidthProperty.PushProperty(canv: TCanvas);
begin
  canv.Pen.Width := PenWidth;
end;

procedure TPenWidthProperty.CreateEdit(panel: TPanel; index: integer);
var
  Edit: TSpinEdit;
begin
  Edit := TSpinEdit.Create(panel);
  with Edit do
  begin
    OnChange := @PropertyChange;
    Parent := panel;
    MinValue := 1;
    MaxValue := 100;
    Value := PenWidth;
    SetBounds(5, index * ItmHeight, Width, Height);
  end;
end;

procedure TPenWidthProperty.PropertyChange(Sender: TObject);
begin
  PenWidth := (Sender as TSpinEdit).Value;
  GlobalCanvasInvalidate;
end;

function TPenWidthProperty.CreateCopy(): TProperty;
begin
  Result := TPenWidthProperty.Create(PenWidth);
end;

procedure TBrushStyleProperty.PushProperty(canv: TCanvas);
begin
  canv.Brush.Style := BrushStyle;
end;

procedure TBrushStyleProperty.CreateEdit(panel: TPanel; index: integer);
var
  Edit: TComboBox;
begin
  Edit := TComboBox.Create(panel);
  with Edit do
  begin
    OnChange := @PropertyChange;
    Parent := panel;
    Items.Add('bsSolid');
    Items.Add('bsClear');
    Items.Add('bsHorizontal');
    Items.Add('bsVertical');
    ItemIndex := 1;
    SetBounds(5, index * ItmHeight, Width, Height);
  end;
end;

procedure TBrushStyleProperty.PropertyChange(Sender: TObject);
begin
  case (Sender as TComboBox).ItemIndex of
    0: BrushStyle := bsSolid;
    1: BrushStyle := bsClear;
    2: BrushStyle := bsHorizontal;
    3: BrushStyle := bsVertical;
  end;
  GlobalCanvasInvalidate;
end;

function TBrushStyleProperty.CreateCopy(): TProperty;
begin
  Result := TBrushStyleProperty.Create(BrushStyle);
end;

procedure TVerticesNumProperty.PushProperty(canv: TCanvas);
begin
  BuildMethod(Vertices);
end;

procedure TVerticesNumProperty.CreateEdit(panel: TPanel; index: integer);
var
  Edit: TSpinEdit;
begin
  Edit := TSpinEdit.Create(panel);
  with Edit do
  begin
    OnChange := @PropertyChange;
    Parent := panel;
    MinValue := 1;
    MaxValue := 100;
    Value := Vertices;
    SetBounds(5, index * ItmHeight, Width, Height);
  end;
end;

function TVerticesNumProperty.CreateCopy(): TProperty;
begin
  Result := TVerticesNumProperty.Create(Vertices, BuildMethod);
end;

procedure TVerticesNumProperty.PropertyChange(Sender: TObject);
begin
  Vertices := (Sender as TSpinEdit).Value;
  GlobalCanvasInvalidate;
end;

procedure TRoundProperty.PushProperty(canv: TCanvas);
begin
  TempRoundX := RoundX;
  TempRoundY := RoundY;
end;

procedure TRoundProperty.CreateEdit(panel: TPanel; index: integer);
var
  EditX, EditY: TSpinEdit;
begin
  EditX := TSpinEdit.Create(panel);
  with EditX do
  begin
    OnChange := @PropertyChange;
    Parent := panel;
    MinValue := 1;
    MaxValue := 1000;
    Value := RoundX;
    Tag := 1;
    SetBounds(5, index * ItmHeight, Width, Height);
  end;
  EditY := TSpinEdit.Create(panel);
  with EditY do
  begin
    OnChange := @PropertyChange;
    Parent := panel;
    MinValue := 1;
    MaxValue := 1000;
    Value := RoundY;
    Tag := 2;
    SetBounds(5, (index + 1) * ItmHeight, Width, Height);
  end;
end;

procedure TRoundProperty.PropertyChange(Sender: TObject);
begin
  case (Sender as TSpinEdit).Tag of
    1: RoundX := (Sender as TSpinEdit).Value;
    2: RoundY := (Sender as TSpinEdit).Value;
  end;
  GlobalCanvasInvalidate;
end;

function TRoundProperty.CreateCopy(): TProperty;
begin
  Result := TRoundProperty.Create(RoundX, RoundY);
end;

end.
