unit UFigures;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Dialogs, Math, UScale, UProperties,
  UGlobalPointers;

type

  TFigure = class
  public
    PenColor: TColor;
    Properties: array of TProperty;
    procedure Draw(Canv: TCanvas); virtual; abstract;
    function AddProperty(prop: TProperty): TProperty;
    procedure SetProperties(canv: TCanvas);
    procedure UnbindProperties;
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
    BrushColor: TColor;
    constructor Create(props: PropertyArray);
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
    constructor Create(props: PropertyArray);
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

  TRoundedRectangle = class(TTwoPointFigureFilled)
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
    Center: TDoublePoint;
    procedure Draw(Canv: TCanvas); override;
  end;

var
  FigureArray: array of TFigure;

procedure AddFigure(Fig: TFigure);

implementation

constructor TFigure.Create(props: PropertyArray);
var
  i: integer;
begin
  SetLength(Properties, Length(props));
  for i := Low(Properties) to High(Properties) do
  begin
    Properties[i] := props[i];
  end;
  PenColor := GlobalPenColor;
end;

constructor TTwoPointFigureFilled.Create(props: PropertyArray);
begin
  inherited Create(props);
  BrushColor := GlobalBrushColor;
end;

constructor TArrayPointFigureFilled.Create(props: PropertyArray);
begin
  inherited Create(props);
  BrushColor := GlobalBrushColor;
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

function TFigure.AddProperty(prop: TProperty): TProperty;
begin
  SetLength(Properties, Length(Properties) + 1);
  Properties[High(Properties)] := prop;
  Result := prop;
end;

procedure TFigure.SetProperties(canv: TCanvas);
var
  i: integer;
begin
  for i := Low(Properties) to High(Properties) do
  begin
    Properties[i].PushProperty(canv);
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
  SetProperties(canv);
  Canv.Polyline(W2SArray(Points));
end;

//Линия

procedure TLine.Draw(Canv: TCanvas);
begin
  Canv.Pen.Color := PenColor;
  SetProperties(canv);
  Canv.Line(W2SX(Point1.X), W2SY(Point1.Y), W2SX(Point2.X),
    W2SY(Point2.Y));
end;

//Прямоугольник

procedure TRectangle.Draw(Canv: TCanvas);
begin
  Canv.Pen.Color := PenColor;
  Canv.Brush.Color := BrushColor;
  SetProperties(canv);
  Canv.Rectangle(W2SX(Point1.X), W2SY(Point1.Y), W2SX(Point2.X),
    W2SY(Point2.Y));
end;

//Эллипс

procedure TEllipse.Draw(Canv: TCanvas);
begin
  Canv.Pen.Color := PenColor;
  Canv.Brush.Color := BrushColor;
  SetProperties(canv);
  Canv.Ellipse(W2SX(Point1.X), W2SY(Point1.Y), W2SX(Point2.X),
    W2SY(Point2.Y));
end;

//Прямоугольник со скруглёнными углами

procedure TRoundedRectangle.Draw(Canv: TCanvas);
begin
  Canv.Pen.Color := PenColor;
  Canv.Brush.Color := BrushColor;
  SetProperties(canv);
  Canv.RoundRect(W2SX(Point1.X), W2SY(Point1.Y),
    W2SX(Point2.X), W2SY(Point2.Y), TempRoundX, TempRoundY);
end;

//Ломаная

procedure TPolyLine.Draw(Canv: TCanvas);
begin
  Canv.Pen.Color := PenColor;
  SetProperties(canv);
  Canv.Polyline(W2SArray(Points));
end;

//Многоугольник

procedure TPolygon.Draw(Canv: TCanvas);
begin
  Canv.Pen.Color := PenColor;
  Canv.Brush.Color := BrushColor;
  SetProperties(canv);
  Canv.Polygon(W2SArray(Points));
end;

//Правильный многоугольник

procedure TRegularPolygon.Draw(Canv: TCanvas);
begin
  Canv.Pen.Color := PenColor;
  Canv.Brush.Color := BrushColor;
  SetProperties(canv);
  Canv.Polygon(W2SArray(Points));
end;

procedure AddFigure(Fig: TFigure);
begin
  Fig.UnbindProperties;
  SetLength(FigureArray, Length(FigureArray) + 1);
  FigureArray[High(FigureArray)] := Fig;
  UpdateFieldBoundingBox;
end;

procedure TFigure.UnbindProperties;
var
  i: integer;
begin
  for i := Low(Properties) to High(Properties) do
  begin
    Properties[i] := Properties[i].CreateCopy();
  end;
end;

end.
