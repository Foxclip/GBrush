unit UProperties;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics;

type

  TProperty = class
    procedure SetProperty(canv: TCanvas); virtual; abstract;
  end;

  TPenColorProperty = class(TProperty)
    PenColor: TColor;
    procedure SetProperty(canv: TCanvas); override;
    constructor Create(pc: TColor);
  end;

  TPenStyleProperty = class(TProperty)
    PenStyle: TPenStyle;
    procedure SetProperty(canv: TCanvas); override;
    constructor Create(ps: TPenStyle);
  end;

  TPenWidthProperty = class(TProperty)
    PenWidth: integer;
    procedure SetProperty(canv: TCanvas); override;
    constructor Create(w: integer);
  end;


  TBrushColorProperty = class(TProperty)
    BrushColor: TColor;
    procedure SetProperty(canv: TCanvas); override;
    constructor Create(bc: TColor);
  end;

  TBrushStyleProperty = class(TProperty)
    BrushStyle: TBrushStyle;
    procedure SetProperty(canv: TCanvas); override;
    constructor Create(bs: TBrushStyle);
  end;

implementation

constructor TPenColorProperty.Create(pc: TColor);
begin
  PenColor := pc;
end;

constructor TPenStyleProperty.Create(ps: TPenStyle);
begin
  PenStyle := ps;
end;

constructor TPenWidthProperty.Create(w: integer);
begin
  PenWidth := w;
end;

constructor TBrushColorProperty.Create(bc: TColor);
begin
  BrushColor := bc;
end;

constructor TBrushStyleProperty.Create(bs: TBrushStyle);
begin
  BrushStyle := bs;
end;

procedure TPenColorProperty.SetProperty(canv: TCanvas);
begin
  canv.Pen.Color := PenColor;
end;

procedure TPenStyleProperty.SetProperty(canv: TCanvas);
begin
  canv.Pen.Style := PenStyle;
end;

procedure TPenWidthProperty.SetProperty(canv: TCanvas);
begin
  canv.Pen.Width := PenWidth;
end;

procedure TBrushColorProperty.SetProperty(canv: TCanvas);
begin
  canv.Brush.Color := BrushColor;
end;

procedure TBrushStyleProperty.SetProperty(canv: TCanvas);
begin
  canv.Brush.Style := BrushStyle;
end;

end.
