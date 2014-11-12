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
    procedure AddProperty(prop: TProperty);
    procedure SetProperties(canv: TCanvas);
    procedure UnbindProperties;
    function BoundingBox(): TDoubleRect; virtual; abstract;
    function CreateCopy(): TFigure; virtual; abstract;
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
    function CreateCopy(): TFigure; override;
  end;

  TLine = class(TTwoPointFigure)
  public
    procedure Draw(Canv: TCanvas); override;
    function CreateCopy(): TFigure; override;
  end;

  TRectangle = class(TTwoPointFigureFilled)
  public
    procedure Draw(Canv: TCanvas); override;
    function CreateCopy(): TFigure; override;
  end;

  TEllipse = class(TTwoPointFigureFilled)
  public
    procedure Draw(Canv: TCanvas); override;
    function CreateCopy(): TFigure; override;
  end;

  TPolyLine = class(TArrayPointFigure)
  public
    procedure Draw(Canv: TCanvas); override;
    function CreateCopy(): TFigure; override;
  end;

  TPolygon = class(TArrayPointFigureFilled)
  public
    procedure Draw(Canv: TCanvas); override;
    function CreateCopy(): TFigure; override;
  end;

  TRegularPolygon = class(TArrayPointFigureFilled)
  public
    Center: TDoublePoint;
    procedure Draw(Canv: TCanvas); override;
    function CreateCopy(): TFigure; override;
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
  SetProperties(canv);
  Canv.Polyline(W2SArray(Points));
end;

function TPenLine.CreateCopy(): TFigure;
var
  temp: TPenLine;
begin
  temp := TPenLine.Create(Properties);
  temp.Points := Points;
  Result := temp;
end;

//Линия

procedure TLine.Draw(Canv: TCanvas);
begin
  Canv.Pen.Color := PenColor;
  SetProperties(canv);
  Canv.Line(W2SX(Point1.X), W2SY(Point1.Y), W2SX(Point2.X),
    W2SY(Point2.Y));
end;

function TLine.CreateCopy(): TFigure;
var
  temp: TLine;
begin
  temp := TLine.Create(Properties);
  temp.Point1 := Point1;
  temp.Point2 := Point2;
  Result := temp;
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

function TRectangle.CreateCopy(): TFigure;
var
  temp: TRectangle;
begin
  temp := TRectangle.Create(Properties);
  temp.Point1 := Point1;
  temp.Point2 := Point2;
  Result := temp;
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

function TEllipse.CreateCopy(): TFigure;
var
  temp: TEllipse;
begin
  temp := TEllipse.Create(Properties);
  temp.Point1 := Point1;
  temp.Point2 := Point2;
  Result := temp;
end;

//Ломаная

procedure TPolyLine.Draw(Canv: TCanvas);
begin
  Canv.Pen.Color := PenColor;
  SetProperties(canv);
  Canv.Polyline(W2SArray(Points));
end;

function TPolyLine.CreateCopy(): TFigure;
var
  temp: TPolyLine;
begin
  temp := TPolyLine.Create(Properties);
  temp.Points := Points;
  Result := temp;
end;

//Многоугольник

procedure TPolygon.Draw(Canv: TCanvas);
begin
  Canv.Pen.Color := PenColor;
  Canv.Brush.Color := BrushColor;
  SetProperties(canv);
  Canv.Polygon(W2SArray(Points));
end;

function TPolygon.CreateCopy(): TFigure;
var
  temp: TPolygon;
begin
  temp := TPolygon.Create(Properties);
  temp.Points := Points;
  Result := temp;
end;

//Правильный многоугольник

procedure TRegularPolygon.Draw(Canv: TCanvas);
begin
  Canv.Pen.Color := PenColor;
  Canv.Brush.Color := BrushColor;
  SetProperties(canv);
  Canv.Polygon(W2SArray(Points));
end;

function TRegularPolygon.CreateCopy(): TFigure;
var
  temp: TRegularPolygon;
begin
  temp := TRegularPolygon.Create(Properties);
  temp.Center := Center;
  temp.Points := Points;
  Result := temp;
end;

procedure AddFigure(Fig: TFigure);
var
  temp: TFigure;
begin
  temp := Fig.CreateCopy();
  temp.UnbindProperties;
  SetLength(FigureArray, Length(FigureArray) + 1);
  FigureArray[High(FigureArray)] := temp;
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
