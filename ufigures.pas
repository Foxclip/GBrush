unit UFigures;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Dialogs, Math, UScale, UProperties,
  UGlobalPointers, ULazregions, UUtils;

type

  TFigure = class
  public
    Properties: array of TProperty;
    IsSelected: boolean;
    procedure Draw(Canv: TCanvas); virtual; abstract;
    function AddProperty(prop: TProperty): TProperty;
    procedure SetProperties(canv: TCanvas);
    procedure UnbindProperties;
    function BoundingBox(): TDoubleRect; virtual; abstract;
    function IsPointInRegion(Point: TPoint): boolean; virtual; abstract;
    constructor Create(props: PropertyArray);
  end;

  TSelectionStyle = class(TFigure)
  end;

  TTwoPointFigure = class(TFigure)
  public
    Point1, Point2: TDoublePoint;
    function BoundingBox(): TDoubleRect; override;
  end;

  TTwoPointFigureFilled = class(TTwoPointFigure)
  public
    function IsPointInRegion(Point: TPoint): boolean; override;
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
    function IsPointInRegion(Point: TPoint): boolean; override;
  end;

  TPenLine = class(TArrayPointFigure)
  public
    procedure Draw(Canv: TCanvas); override;
    function IsPointInRegion(Point: TPoint): boolean; override;
  end;

  TLine = class(TTwoPointFigure)
  public
    procedure Draw(Canv: TCanvas); override;
    function IsPointInRegion(Point: TPoint): boolean; override;
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
    function IsPointInRegion(Point: TPoint): boolean; override;
  end;

  TPolyLine = class(TArrayPointFigure)
  public
    procedure Draw(Canv: TCanvas); override;
    function IsPointInRegion(Point: TPoint): boolean; override;
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

const
  LineOffset = 10;

var
  FigureArray: array of TFigure;
  SelectionStyle: TSelectionStyle;

function LineToPolygon(Point1, Point2: TDoublePoint): TPointArray;

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

function TTwoPointFigureFilled.IsPointInRegion(Point: TPoint): boolean;
var
  Region: TLazRegion;
  Rect: TRect;
begin
  Region := TLazRegion.Create;
  Rect.TopLeft := W2S(Point1);
  Rect.BottomRight := W2S(Point2);
  Region.AddRectangle(Rect);
  Result := Region.IsPointInRegion(Point.x, Point.y);
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

function TArrayPointFigureFilled.IsPointInRegion(Point: TPoint): boolean;
var
  Region: TLazRegion;
  RegionPoints: TPointArray;
begin
  RegionPoints := W2SArray(Points);
  Region := TLazRegion.Create;
  Region.AddPolygon(RegionPoints, rfmOddEven);
  Result := Region.IsPointInRegion(Point.x, Point.y);
end;

//Карандаш

procedure TPenLine.Draw(Canv: TCanvas);
begin
  SetProperties(canv);
  if IsSelected then
    SelectionStyle.SetProperties(Canv);
  Canv.Polyline(W2SArray(Points));
end;

function TPenLine.IsPointInRegion(Point: TPoint): boolean;
var
  i: integer;
  Region: TLazRegion;
  RegionPoints: TPointArray;
begin
  Region := TLazRegion.Create;
  for i := Low(Points) to High(Points) - 1 do
  begin
    RegionPoints := LineToPolygon(Points[i], Points[i + 1]);
    Region.AddPolygon(RegionPoints, rfmOddEven);
  end;
  Result := Region.IsPointInRegion(Point.x, Point.y);
end;

//Линия

procedure TLine.Draw(Canv: TCanvas);
begin
  SetProperties(canv);
  if IsSelected then
    SelectionStyle.SetProperties(Canv);
  Canv.Line(
    W2SX(Point1.X),
    W2SY(Point1.Y),
    W2SX(Point2.X),
    W2SY(Point2.Y)
    );
end;

function TLine.IsPointInRegion(Point: TPoint): boolean;
var
  Region: TLazRegion;
  RegionPoints: TPointArray;
begin
  Region := TLazRegion.Create;
  RegionPoints := LineToPolygon(Point1, Point2);
  Region.AddPolygon(RegionPoints, rfmOddEven);
  Result := Region.IsPointInRegion(Point.x, Point.y);
end;

//Прямоугольник

procedure TRectangle.Draw(Canv: TCanvas);
begin
  SetProperties(canv);
  if IsSelected then
    SelectionStyle.SetProperties(Canv);
  Canv.Rectangle(
    W2SX(Point1.X),
    W2SY(Point1.Y),
    W2SX(Point2.X),
    W2SY(Point2.Y)
    );
end;

//Эллипс

procedure TEllipse.Draw(Canv: TCanvas);
begin
  SetProperties(canv);
  if IsSelected then
    SelectionStyle.SetProperties(Canv);
  Canv.Ellipse(
    W2SX(Point1.X),
    W2SY(Point1.Y),
    W2SX(Point2.X),
    W2SY(Point2.Y)
    );
end;

function TEllipse.IsPointInRegion(Point: TPoint): boolean;
var
  Region: TLazRegion;
begin
  Region := TLazRegion.Create;
  Region.AddEllipse(
    W2SX(Point1.X),
    W2SY(Point1.Y),
    W2SX(Point2.X),
    W2SY(Point2.Y)
    );
  Result := Region.IsPointInRegion(Point.x, Point.y);
end;

//Прямоугольник со скруглёнными углами

procedure TRoundedRectangle.Draw(Canv: TCanvas);
begin
  SetProperties(canv);
  if IsSelected then
    SelectionStyle.SetProperties(Canv);
  Canv.RoundRect(
    W2SX(Point1.X),
    W2SY(Point1.Y),
    W2SX(Point2.X),
    W2SY(Point2.Y),
    TempRoundX,
    TempRoundY
    );
end;

//Ломаная

procedure TPolyLine.Draw(Canv: TCanvas);
begin
  SetProperties(canv);
  if IsSelected then
    SelectionStyle.SetProperties(Canv);
  Canv.Polyline(W2SArray(Points));
end;

function TPolyLine.IsPointInRegion(Point: TPoint): boolean;
var
  i: integer;
  Region: TLazRegion;
  RegionPoints: TPointArray;
begin
  Region := TLazRegion.Create;
  for i := Low(Points) to High(Points) - 1 do
  begin
    RegionPoints := LineToPolygon(Points[i], Points[i + 1]);
    Region.AddPolygon(RegionPoints, rfmOddEven);
  end;
  Result := Region.IsPointInRegion(Point.x, Point.y);
end;

//Многоугольник

procedure TPolygon.Draw(Canv: TCanvas);
begin
  SetProperties(canv);
  if IsSelected then
    SelectionStyle.SetProperties(Canv);
  Canv.Polygon(W2SArray(Points));
end;

//Правильный многоугольник

procedure TRegularPolygon.Draw(Canv: TCanvas);
begin
  SetProperties(canv);
  if IsSelected then
    SelectionStyle.SetProperties(Canv);
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

function LineToPolygon(Point1, Point2: TDoublePoint): TPointArray;
var
  d, p: double;
  x1, y1, x2, y2, x, y, sx, sy: double;
  p1x, p1y, p2x, p2y, p3x, p3y, p4x, p4y: double;
  RegionPoints: TPointArray;
begin
  x1 := Point1.x;
  y1 := Point1.y;
  x2 := Point2.x;
  y2 := Point2.y;
  d := sqrt(sqr(x2 - x1) + sqr(y2 - y1));
  p := (LineOffset / Scale) / d;
  x := x2 - x1;
  y := y2 - y1;
  sx := p * x;
  sy := p * y;
  p1x := x1 - sy;
  p1y := y1 + sx;
  p2x := x1 + sy;
  p2y := y1 - sx;
  p3x := x2 + sy;
  p3y := y2 - sx;
  p4x := x2 - sy;
  p4y := y2 + sx;
  SetLength(RegionPoints, 4);
  RegionPoints[0] := W2S(D2P(p1x, p1y));
  RegionPoints[1] := W2S(D2P(p2x, p2y));
  RegionPoints[2] := W2S(D2P(p3x, p3y));
  RegionPoints[3] := W2S(D2P(p4x, p4y));
  Result := RegionPoints;
end;

initialization

  SelectionStyle := TSelectionStyle.Create(nil);
  SelectionStyle.AddProperty(TPenStyleProperty.Create(psDot));
  SelectionStyle.AddProperty(TPenColorProperty.Create(clRed));

end.
