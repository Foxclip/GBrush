unit UProperties;

{$mode objfpc}{$H+}

interface

uses
  Classes, ExtCtrls, StdCtrls, Spin, SysUtils, Graphics, UGlobalPointers;

type

  PointerToMethodLongInt = procedure(x: integer) of object;

  TProperty = class
    procedure PullProperty(canv: TCanvas); virtual; abstract;
    procedure CreateEdit(panel: TPanel; index: integer); virtual; abstract;
  end;

  PropertyArray = array of TProperty;

  TPenStyleProperty = class(TProperty)
    PenStyle: TPenStyle;
    procedure PullProperty(canv: TCanvas); override;
    procedure CreateEdit(panel: TPanel; index: integer); override;
    constructor Create(ps: TPenStyle);
  end;

  TPenWidthProperty = class(TProperty)
    PenWidth: integer;
    procedure PullProperty(canv: TCanvas); override;
    procedure CreateEdit(panel: TPanel; index: integer); override;
    constructor Create(w: integer);
  end;

  TBrushStyleProperty = class(TProperty)
    BrushStyle: TBrushStyle;
    procedure PullProperty(canv: TCanvas); override;
    procedure CreateEdit(panel: TPanel; index: integer); override;
    constructor Create(bs: TBrushStyle);
  end;

  TVerticesNumProperty = class(TProperty)
    Vertices: integer;
    BuildMethod: PointerToMethodLongInt;
    procedure PullProperty(canv: TCanvas); override;
    procedure CreateEdit(panel: TPanel; index: integer); override;
    constructor Create(vert: integer; build: PointerToMethodLongInt);
  end;

const
  ItmHeight = 30;

implementation

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

procedure TPenStyleProperty.PullProperty(canv: TCanvas);
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

procedure TPenWidthProperty.PullProperty(canv: TCanvas);
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
    Parent := panel;
    MinValue := 1;
    MaxValue := 100;
    Value := PenWidth;
    SetBounds(5, index * ItmHeight, Width, Height);
  end;
end;

procedure TBrushStyleProperty.PullProperty(canv: TCanvas);
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
    Parent := panel;
    Items.Add('bsSolid');
    Items.Add('bsClear');
    Items.Add('bsHorizontal');
    Items.Add('bsVertical');
    ItemIndex := 0;
    SetBounds(5, index * ItmHeight, Width, Height);
  end;
end;

procedure TVerticesNumProperty.PullProperty(canv: TCanvas);
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
    Parent := panel;
    MinValue := 1;
    MaxValue := 100;
    Value := Vertices;
    SetBounds(5, index * ItmHeight, Width, Height);
  end;
end;

end.