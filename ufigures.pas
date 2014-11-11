unit UFigures;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Dialogs, Math, UScale, UProperties,
  UGlobalPointers;

type

  TFigure = class
  public
    Properties: array of TProperty;
    procedure Draw(Canv: TCanvas); virtual; abstract;
    procedure AddProperty(prop: TProperty);
    procedure SetProperties(canv: TCanvas);
    function BoundingBox(): TDoubleRect; virtual; abstract;
    constructor Create(props: PropertyArray);
  end;

  TTwoPointFigure = class(TFigure)
  public
    Point1, Point2: TDoublePoint;
    function BoundingBox(): TDoubleRect; override;
  end;

  TTwoPointFigureFilled = class(TTwoPointFigure)
  public
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
  end;

var
  FigureArray: array of TFigure;

procedure AddFigure(Fig: TFigure);

implementation

constructor TFigure.Create(props: PropertyArray);
begin
  Properties := props;
end;

function TTwoPointFigure.BoundingBox(): TDoubleRect;
begin
  with Result do
  begin
    x1 := Min(Point1.x, Point2.x);
    y1 := Min(Point1.y, Point2.y);
    x2 := Max(Point1.x, Point2.x);
    y2 := Max(Point1.y, Point2.y);
  end;
end;

procedure TFigure.AddProperty(prop: TProperty);
begin
  SetLength(Properties, Length(Properties) + 1);
  Properties[High(Properties)] := prop;
end;

procedure TFigure.SetProperties(canv: TCanvas);
var
  i: integer;
begin
  for i := Low(Properties) to High(Properties) do
  begin
    Properties[i].SetProperty(canv);
  end;
end;

procedure TArrayPointFigure.AddPoint(MousePoint: TDoublePoint);
var
  i: integer;
begin
  SetLength(Points, Length(Points) + 1);
  Points[High(Points)] := MousePoint;
  with Bounds do
  begin
    x1 := Points[High(Points)].x;
    y1 := Points[High(Points)].y;
    x2 := Points[High(Points)].x;
    y2 := Points[High(Points)].y;
    if Length(Points) > 0 then
      for i := Low(Points) to High(Points) do
      begin
        x1 := Min(Points[i].x, x1);
        y1 := Min(Points[i].y, y1);
        x2 := Max(Points[i].x, x2);
        y2 := Max(Points[i].y, y2);
      end;
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
  SetProperties(canv);
  Canv.Polyline(W2SArray(Points));
end;

//Линия

procedure TLine.Draw(Canv: TCanvas);
begin
  SetProperties(canv);
  Canv.Line(W2SX(Point1.X), W2SY(Point1.Y), W2SX(Point2.X),
    W2SY(Point2.Y));
end;

//Прямоугольник

procedure TRectangle.Draw(Canv: TCanvas);
begin
  SetProperties(canv);
  Canv.Rectangle(W2SX(Point1.X), W2SY(Point1.Y), W2SX(Point2.X),
    W2SY(Point2.Y));
end;

//Эллипс

procedure TEllipse.Draw(Canv: TCanvas);
begin
  SetProperties(canv);
  Canv.Ellipse(W2SX(Point1.X), W2SY(Point1.Y), W2SX(Point2.X),
    W2SY(Point2.Y));
end;

//Ломаная

procedure TPolyLine.Draw(Canv: TCanvas);
begin
  SetProperties(canv);
  Canv.Polyline(W2SArray(Points));
end;

//Многоугольник

procedure TPolygon.Draw(Canv: TCanvas);
begin
  SetProperties(canv);
  Canv.Polygon(W2SArray(Points));
end;

//Правильный многоугольник

procedure TRegularPolygon.Draw(Canv: TCanvas);
begin
  SetProperties(canv);
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
