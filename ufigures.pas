unit UFigures;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Dialogs, Math, UScale, UGlobalPointers;

type

  TFigure = class
  public
    PenColor: TColor;
    PenStyle: TPenStyle;
    PenSize: integer;
    procedure Draw(Canv: TCanvas); virtual; abstract;
    function BoundingBox(): TDoubleRect; virtual; abstract;
    constructor Create;
  end;

  TTwoPointFigure = class(TFigure)
  public
    Point1, Point2: TDoublePoint;
    function BoundingBox(): TDoubleRect; override;
    constructor Create(MousePoint: TDoublePoint);
  end;

  TTwoPointFigureFilled = class(TTwoPointFigure)
  public
    BrushColor: TColor;
    BrushStyle: TBrushStyle;
    constructor Create(MousePoint: TDoublePoint);
  end;

  PointArray = array of TDoublePoint;

  TArrayPointFigure = class(TFigure)
  public
    Points: PointArray;
    Bounds: TDoubleRect;
    procedure AddPoint(MousePoint: TDoublePoint);
    function BoundingBox(): TDoubleRect; override;
  end;

  TArrayPointFigureFilled = class(TArrayPointFigure)
  public
    BrushColor: TColor;
    BrushStyle: TBrushStyle;
    constructor Create;
  end;

  TPenLine = class(TArrayPointFigure)
  public
    procedure Draw(Canv: TCanvas); override;
  end;

  TLine = class(TTwoPointFigure)
  public
    procedure Draw(Canv: TCanvas); override;
  end;

  TRectangle = class(TTwoPointFigureFilled)
  public
    procedure Draw(Canv: TCanvas); override;
  end;

  TEllipse = class(TTwoPointFigureFilled)
  public
    procedure Draw(Canv: TCanvas); override;
  end;

  TPolyLine = class(TArrayPointFigure)
  public
    procedure Draw(Canv: TCanvas); override;
  end;

  TPolygon = class(TArrayPointFigureFilled)
  public
    procedure Draw(Canv: TCanvas); override;
  end;

  TRegularPolygon = class(TArrayPointFigureFilled)
  public
    Vertices: integer;
    Center: TDoublePoint;
    procedure Draw(Canv: TCanvas); override;
    procedure BuildRegularPolygon(MousePoint: TDoublePoint);
    constructor Create(MousePoint: TDoublePoint);
  end;

var
  FigureArray: array of TFigure;

procedure AddFigure(Fig: TFigure);

implementation

constructor TFigure.Create;
begin
  PenColor := GlobalPenColor;
  PenStyle := GlobalPenStyle;
  PenSize := GlobalPenSize;
end;

constructor TTwoPointFigure.Create(MousePoint: TDoublePoint);
begin
  inherited Create;
  Point1 := MousePoint;
  Point2 := MousePoint;
end;

function TTwoPointFigure.BoundingBox(): TDoubleRect;
begin
  Result.x1 := Min(Point1.x, Point2.x);
  Result.y1 := Min(Point1.y, Point2.y);
  Result.x2 := Max(Point1.x, Point2.x);
  Result.y2 := Max(Point1.y, Point2.y);
end;

constructor TTwoPointFigureFilled.Create(MousePoint: TDoublePoint);
begin
  inherited Create(MousePoint);
  BrushColor := GlobalBrushColor;
  BrushStyle := GlobalBrushStyle;
end;

constructor TArrayPointFigureFilled.Create;
begin
  inherited Create;
  BrushColor := GlobalBrushColor;
  BrushStyle := GlobalBrushStyle;
end;

constructor TRegularPolygon.Create(MousePoint: TDoublePoint);
begin
  inherited Create;
  Vertices := GlobalAngleNum;
  Center := MousePoint;
  BuildRegularPolygon(MousePoint);
end;

procedure TArrayPointFigure.AddPoint(MousePoint: TDoublePoint);
var
  i: integer;
begin
  SetLength(Points, Length(Points) + 1);
  Points[High(Points)] := MousePoint;
  Bounds.x1 := Points[High(Points)].x;
  Bounds.y1 := Points[High(Points)].y;
  Bounds.x2 := Points[High(Points)].x;
  Bounds.y2 := Points[High(Points)].y;
  if Length(Points) > 0 then
    for i := Low(Points) to High(Points) do
    begin
      Bounds.x1 := Min(Points[i].x, Bounds.x1);
      Bounds.y1 := Min(Points[i].y, Bounds.y1);
      Bounds.x2 := Max(Points[i].x, Bounds.x2);
      Bounds.y2 := Max(Points[i].y, Bounds.y2);
    end;
  UpdateFieldBoundingBox;
end;

function TArrayPointFigure.BoundingBox(): TDoubleRect;
begin
  Result := Bounds;
end;

//Карандаш

procedure TPenLine.Draw(Canv: TCanvas);
var
  i: integer;
begin
  Canv.Pen.Color := PenColor;
  Canv.Pen.Style := PenStyle;
  Canv.Pen.Width := PenSize;
  Canv.Polyline(W2SArray(Points));
end;

//Линия

procedure TLine.Draw(Canv: TCanvas);
begin
  Canv.Pen.Color := PenColor;
  Canv.Pen.Style := PenStyle;
  Canv.Pen.Width := PenSize;
  Canv.Line(W2SX(Point1.X), W2SY(Point1.Y), W2SX(Point2.X),
    W2SY(Point2.Y));
end;

//Прямоугольник

procedure TRectangle.Draw(Canv: TCanvas);
begin
  Canv.Pen.Color := PenColor;
  Canv.Pen.Style := PenStyle;
  Canv.Brush.Color := BrushColor;
  Canv.Brush.Style := BrushStyle;
  Canv.Pen.Width := PenSize;
  Canv.Rectangle(W2SX(Point1.X), W2SY(Point1.Y), W2SX(Point2.X),
    W2SY(Point2.Y));
end;

//Эллипс

procedure TEllipse.Draw(Canv: TCanvas);
begin
  Canv.Pen.Color := PenColor;
  Canv.Pen.Style := PenStyle;
  Canv.Brush.Color := BrushColor;
  Canv.Brush.Style := BrushStyle;
  Canv.Pen.Width := PenSize;
  Canv.Ellipse(W2SX(Point1.X), W2SY(Point1.Y), W2SX(Point2.X),
    W2SY(Point2.Y));
end;

//Ломаная

procedure TPolyLine.Draw(Canv: TCanvas);
begin
  Canv.Pen.Color := PenColor;
  Canv.Pen.Style := PenStyle;
  Canv.Pen.Width := PenSize;
  Canv.Polyline(W2SArray(Points));
end;

//Многоугольник

procedure TPolygon.Draw(Canv: TCanvas);
begin
  Canv.Pen.Color := PenColor;
  Canv.Pen.Style := PenStyle;
  Canv.Brush.Color := BrushColor;
  Canv.Brush.Style := BrushStyle;
  Canv.Pen.Width := PenSize;
  Canv.Polygon(W2SArray(Points));
end;

//Правильный многоугольник

procedure TRegularPolygon.Draw(Canv: TCanvas);
begin
  Canv.Pen.Color := PenColor;
  Canv.Pen.Style := PenStyle;
  Canv.Brush.Color := BrushColor;
  Canv.Brush.Style := BrushStyle;
  Canv.Pen.Width := PenSize;
  Canv.Polygon(W2SArray(Points));
end;

procedure TRegularPolygon.BuildRegularPolygon(MousePoint: TDoublePoint);
var
  i: integer;
  Rad: double;
  Point: TDoublePoint;
begin
  Rad := sqrt(power(Center.X - S2WX(GlobalMousePoint.X), 2) +
    power(Center.Y - S2WY(GlobalMousePoint.Y), 2));
  setLength(Points, 0);
  for i := 0 to Vertices do
  begin
    Point.x := Center.X + Rad * cos(((2 * pi * i) / Vertices));
    Point.y := Center.Y + Rad * sin(((2 * pi * i) / Vertices));
    AddPoint(Point);
  end;
end;

procedure AddFigure(Fig: TFigure);
begin
  SetLength(FigureArray, Length(FigureArray) + 1);
  FigureArray[High(FigureArray)] := Fig;
  UpdateFieldBoundingBox;
end;

end.
