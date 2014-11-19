unit UTools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Controls, Dialogs, Buttons, UFigures,
  ExtCtrls, UScale, UProperties, UGlobalPointers, Math, UUtils;

type

  TTool = class
  public
    Properties: array of TProperty;
    procedure MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; MousePoint: TDoublePoint); virtual; abstract;
    procedure MouseMove(Sender: TObject; Shift: TShiftState;
      MousePoint: TDoublePoint); virtual; abstract;
    procedure MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; MousePoint: TDoublePoint); virtual; abstract;
    function AddProperty(prop: TProperty): TProperty;
    function GetGlyphString: string; virtual; abstract;
  end;

  TPenTool = class(TTool)
  public
    TempPenLine: TPenLine;
    procedure MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; MousePoint: TDoublePoint); override;
    procedure MouseMove(Sender: TObject; Shift: TShiftState;
      MousePoint: TDoublePoint); override;
    procedure MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; MousePoint: TDoublePoint); override;
    function GetGlyphString: string; override;
    constructor Create;
  end;

  TLineTool = class(TTool)
  public
    TempLine: TLine;
    procedure MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; MousePoint: TDoublePoint); override;
    procedure MouseMove(Sender: TObject; Shift: TShiftState;
      MousePoint: TDoublePoint); override;
    procedure MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; MousePoint: TDoublePoint); override;
    function GetGlyphString: string; override;
    constructor Create;
  end;

  TRectangleTool = class(TTool)
  public
    TempRectangle: TRectangle;
    procedure MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; MousePoint: TDoublePoint); override;
    procedure MouseMove(Sender: TObject; Shift: TShiftState;
      MousePoint: TDoublePoint); override;
    procedure MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; MousePoint: TDoublePoint); override;
    function GetGlyphString: string; override;
    constructor Create;
  end;

  TEllipseTool = class(TTool)
  public
    TempEllipse: TEllipse;
    procedure MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; MousePoint: TDoublePoint); override;
    procedure MouseMove(Sender: TObject; Shift: TShiftState;
      MousePoint: TDoublePoint); override;
    procedure MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; MousePoint: TDoublePoint); override;
    function GetGlyphString: string; override;
    constructor Create;
  end;

  TRoundedRectangleTool = class(TTool)
  public
    TempRoundedRectangle: TRoundedRectangle;
    procedure MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; MousePoint: TDoublePoint); override;
    procedure MouseMove(Sender: TObject; Shift: TShiftState;
      MousePoint: TDoublePoint); override;
    procedure MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; MousePoint: TDoublePoint); override;
    function GetGlyphString: string; override;
    constructor Create;
  end;

  TPolyLineTool = class(TTool)
  public
    TempPolyLine: TPolyLine;
    procedure MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; MousePoint: TDoublePoint); override;
    procedure MouseMove(Sender: TObject; Shift: TShiftState;
      MousePoint: TDoublePoint); override;
    procedure MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; MousePoint: TDoublePoint); override;
    function GetGlyphString: string; override;
    constructor Create;
  end;

  TPolyGonTool = class(TTool)
  public
    TempPolygon: TPolygon;
    procedure MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; MousePoint: TDoublePoint); override;
    procedure MouseMove(Sender: TObject; Shift: TShiftState;
      MousePoint: TDoublePoint); override;
    procedure MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; MousePoint: TDoublePoint); override;
    function GetGlyphString: string; override;
    constructor Create;
  end;

  TRegularPolygonTool = class(TTool)
  public
    TempRegularPolygon: TRegularPolygon;
    BuildMethodBack:
    procedure(canv: TCanvas) of object;
    procedure MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; MousePoint: TDoublePoint); override;
    procedure MouseMove(Sender: TObject; Shift: TShiftState;
      MousePoint: TDoublePoint); override;
    procedure MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; MousePoint: TDoublePoint); override;
    procedure BuildRegularPolygon(Vertices: integer);
    function GetGlyphString: string; override;
    constructor Create;
  end;

var
  ToolArray: array of TTool;
  SelectedTool: integer;
  TempFigure: TFigure;

procedure RegisterTool(tool: TTool);

implementation

function TTool.AddProperty(prop: TProperty): TProperty;
begin
  SetLength(Properties, Length(Properties) + 1);
  Properties[High(Properties)] := prop;
  Result := prop;
end;

//Карандаш

constructor TPenTool.Create;
begin
  AddProperty(TPenColorProperty.Create(clBlack));
  AddProperty(TBrushColorProperty.Create(clWhite));
  AddProperty(TPenWidthProperty.Create(1));
  AddProperty(TPenStyleProperty.Create(psSolid));
end;

procedure TPenTool.MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; MousePoint: TDoublePoint);
begin
  if Button = mbLeft then
  begin
    if TempFigure <> nil then
      AddFigure(TempFigure);
    if TempPenLine = nil then
    begin
      TempPenLine := TPenLine.Create(Properties);
      TempFigure := TempPenLine;
    end
    else
    begin
      TempFigure := TempPenLine;
      TempPenLine := nil;
    end;
  end;
end;

procedure TPenTool.MouseMove(Sender: TObject; Shift: TShiftState;
  MousePoint: TDoublePoint);
begin
  if GlobalIsMouseDownLeft and (TempPenLine <> nil) then
    TempPenLine.AddPoint(MousePoint);
end;

procedure TPenTool.MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; MousePoint: TDoublePoint);
begin
  if TempPenLine <> nil then
  begin
    TempFigure := TempPenLine;
    TempPenLine := nil;
  end;
end;

function TPenTool.GetGlyphString: string;
begin
  Result := 'glyphs/pen.bmp';
end;

//Линия

constructor TLineTool.Create;
begin
  AddProperty(TPenColorProperty.Create(clBlack));
  AddProperty(TBrushColorProperty.Create(clWhite));
  AddProperty(TPenWidthProperty.Create(1));
  AddProperty(TPenStyleProperty.Create(psSolid));
end;

procedure TLineTool.MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; MousePoint: TDoublePoint);
begin
  if Button = mbLeft then
  begin
    if TempFigure <> nil then
      AddFigure(TempFigure);
    if TempLine = nil then
    begin
      TempLine := TLine.Create(Properties);
      TempLine.Point1 := MousePoint;
      TempLine.Point2 := MousePoint;
      TempFigure := TempLine;
    end
    else
    begin
      TempFigure := TempLine;
      TempLine := nil;
    end;
  end;
end;

procedure TLineTool.MouseMove(Sender: TObject; Shift: TShiftState;
  MousePoint: TDoublePoint);
begin
  if GlobalIsMouseDownLeft and (TempLine <> nil) then
  begin
    TempLine.Point2 := MousePoint;
  end;
end;

procedure TLineTool.MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; MousePoint: TDoublePoint);
begin
  if TempLine <> nil then
  begin
    TempFigure := TempLine;
    TempLine := nil;
  end;
end;

function TLineTool.GetGlyphString: string;
begin
  Result := 'glyphs/line.bmp';
end;

//Прямоугольник

constructor TRectangleTool.Create;
begin
  AddProperty(TPenColorProperty.Create(clBlack));
  AddProperty(TBrushColorProperty.Create(clWhite));
  AddProperty(TPenWidthProperty.Create(1));
  AddProperty(TPenStyleProperty.Create(psSolid));
  AddProperty(TBrushStyleProperty.Create(bsClear));
end;

procedure TRectangleTool.MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; MousePoint: TDoublePoint);
begin
  if Button = mbLeft then
  begin
    if TempFigure <> nil then
      AddFigure(TempFigure);
    if TempRectangle = nil then
    begin
      TempRectangle := TRectangle.Create(Properties);
      TempRectangle.Point1 := MousePoint;
      TempRectangle.Point2 := MousePoint;
      TempFigure := TempRectangle;
    end
    else
    begin
      TempFigure := TempRectangle;
      TempRectangle := nil;
    end;
  end;
end;

procedure TRectangleTool.MouseMove(Sender: TObject; Shift: TShiftState;
  MousePoint: TDoublePoint);
begin
  if GlobalIsMouseDownLeft and (TempRectangle <> nil) then
  begin
    TempRectangle.Point2 := MousePoint;
  end;
end;

procedure TRectangleTool.MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; MousePoint: TDoublePoint);
begin
  if TempRectangle <> nil then
  begin
    TempFigure := TempRectangle;
    TempRectangle := nil;
  end;
end;

function TRectangleTool.GetGlyphString: string;
begin
  Result := 'glyphs/rectangle.bmp';
end;

//Эллипс

constructor TEllipseTool.Create;
begin
  AddProperty(TPenColorProperty.Create(clBlack));
  AddProperty(TBrushColorProperty.Create(clWhite));
  AddProperty(TPenWidthProperty.Create(1));
  AddProperty(TPenStyleProperty.Create(psSolid));
  AddProperty(TBrushStyleProperty.Create(bsClear));
end;

procedure TEllipseTool.MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; MousePoint: TDoublePoint);
begin
  if Button = mbLeft then
  begin
    if TempFigure <> nil then
      AddFigure(TempFigure);
    if TempEllipse = nil then
    begin
      TempEllipse := TEllipse.Create(Properties);
      TempEllipse.Point1 := MousePoint;
      TempEllipse.Point2 := MousePoint;
      TempFigure := TempEllipse;
    end
    else
    begin
      TempFigure := TempEllipse;
      TempEllipse := nil;
    end;
  end;
end;

procedure TEllipseTool.MouseMove(Sender: TObject; Shift: TShiftState;
  MousePoint: TDoublePoint);
begin
  if GlobalIsMouseDownLeft and (TempEllipse <> nil) then
  begin
    TempEllipse.Point2 := MousePoint;
  end;
end;

procedure TEllipseTool.MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; MousePoint: TDoublePoint);
begin
  if TempEllipse <> nil then
  begin
    TempFigure := TempEllipse;
    TempEllipse := nil;
  end;
end;

function TEllipseTool.GetGlyphString: string;
begin
  Result := 'glyphs/ellipse.bmp';
end;

//Прямоугольник со скруглёнными углами

constructor TRoundedRectangleTool.Create;
begin
  AddProperty(TPenColorProperty.Create(clBlack));
  AddProperty(TBrushColorProperty.Create(clWhite));
  AddProperty(TPenWidthProperty.Create(1));
  AddProperty(TPenStyleProperty.Create(psSolid));
  AddProperty(TBrushStyleProperty.Create(bsClear));
  AddProperty(TRoundProperty.Create(50, 50));
end;

procedure TRoundedRectangleTool.MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; MousePoint: TDoublePoint);
begin
  if Button = mbLeft then
  begin
    if TempFigure <> nil then
      AddFigure(TempFigure);
    if TempRoundedRectangle = nil then
    begin
      TempRoundedRectangle := TRoundedRectangle.Create(Properties);
      TempRoundedRectangle.Point1 := MousePoint;
      TempRoundedRectangle.Point2 := MousePoint;
      TempFigure := TempRoundedRectangle;
    end
    else
    begin
      TempFigure := TempRoundedRectangle;
      TempRoundedRectangle := nil;
    end;
  end;
end;

procedure TRoundedRectangleTool.MouseMove(Sender: TObject;
  Shift: TShiftState; MousePoint: TDoublePoint);
begin
  if GlobalIsMouseDownLeft and (TempRoundedRectangle <> nil) then
  begin
    TempRoundedRectangle.Point2 := MousePoint;
  end;
end;

procedure TRoundedRectangleTool.MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; MousePoint: TDoublePoint);
begin
  if TempRoundedRectangle <> nil then
  begin
    TempFigure := TempRoundedRectangle;
    TempRoundedRectangle := nil;
  end;
end;

function TRoundedRectangleTool.GetGlyphString: string;
begin
  Result := 'glyphs/rounded_rectangle.bmp';
end;

//Ломаная

constructor TPolyLineTool.Create;
begin
  AddProperty(TPenColorProperty.Create(clBlack));
  AddProperty(TBrushColorProperty.Create(clWhite));
  AddProperty(TPenWidthProperty.Create(1));
  AddProperty(TPenStyleProperty.Create(psSolid));
end;

procedure TPolyLineTool.MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; MousePoint: TDoublePoint);
begin
  if Button = mbLeft then
  begin
    if TempFigure <> nil then
      AddFigure(TempFigure);
    if TempPolyLine = nil then
    begin
      TempPolyLine := TPolyLine.Create(Properties);
      TempPolyLine.AddPoint(MousePoint);
      TempPolyLine.AddPoint(MousePoint);
    end
    else
      TempPolyLine.AddPoint(MousePoint);
    TempFigure := TempPolyLine;
  end;
  if (Button = mbRight) and (TempPolyLine <> nil) then
  begin
    TempPolyLine.AddPoint(MousePoint);
    TempFigure := TempPolyLine;
    TempPolyLine := nil;
  end;
end;

procedure TPolyLineTool.MouseMove(Sender: TObject; Shift: TShiftState;
  MousePoint: TDoublePoint);
begin
  if TempPolyLine <> nil then
  begin
    TempPolyLine.Points[High(TempPolyLine.Points)] := MousePoint;
  end;
end;

procedure TPolyLineTool.MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; MousePoint: TDoublePoint);
begin
end;

function TPolyLineTool.GetGlyphString: string;
begin
  Result := 'glyphs/polyline.bmp';
end;

//Многоугольник

constructor TPolyGonTool.Create;
begin
  AddProperty(TPenColorProperty.Create(clBlack));
  AddProperty(TBrushColorProperty.Create(clWhite));
  AddProperty(TPenWidthProperty.Create(1));
  AddProperty(TPenStyleProperty.Create(psSolid));
  AddProperty(TBrushStyleProperty.Create(bsClear));
end;

procedure TPolygonTool.MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; MousePoint: TDoublePoint);
begin
  if Button = mbLeft then
  begin
    if TempFigure <> nil then
      AddFigure(TempFigure);
    if TempPolygon = nil then
    begin
      TempPolygon := TPolygon.Create(Properties);
      TempFigure := TempPolygon;
      TempPolygon.AddPoint(MousePoint);
      TempPolygon.AddPoint(MousePoint);
    end
    else
      TempPolygon.AddPoint(MousePoint);
  end;
  if (Button = mbRight) and (TempPolygon <> nil) then
  begin
    TempPolygon.AddPoint(MousePoint);
    TempFigure := TempPolygon;
    TempPolygon := nil;
  end;
end;

procedure TPolyGonTool.MouseMove(Sender: TObject; Shift: TShiftState;
  MousePoint: TDoublePoint);
begin
  if TempPolygon <> nil then
    TempPolygon.Points[High(TempPolygon.Points)] := MousePoint;
end;

procedure TPolyGonTool.MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; MousePoint: TDoublePoint);
begin
end;

function TPolygonTool.GetGlyphString: string;
begin
  Result := 'glyphs/polygon.bmp';
end;

//Правильный многоугольник

constructor TRegularPolygonTool.Create;
var
  prop: TProperty;
begin
  AddProperty(TPenColorProperty.Create(clBlack));
  AddProperty(TBrushColorProperty.Create(clWhite));
  AddProperty(TPenWidthProperty.Create(1));
  AddProperty(TPenStyleProperty.Create(psSolid));
  AddProperty(TBrushStyleProperty.Create(bsClear));
  prop := AddProperty(TVerticesNumProperty.Create(6, @BuildRegularPolygon));
  BuildMethodBack := @prop.PushProperty;
end;

procedure TRegularPolygonTool.MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; MousePoint: TDoublePoint);
begin
  if Button = mbLeft then
  begin
    if TempFigure <> nil then
      AddFigure(TempFigure);
    if TempRegularPolygon = nil then
    begin
      TempRegularPolygon := TRegularPolygon.Create(Properties);
      TempRegularPolygon.Center := MousePoint;
      BuildMethodBack(nil);
      TempFigure := TempRegularPolygon;
    end
    else
    begin
      TempFigure := TempRegularPolygon;
      TempRegularPolygon := nil;
    end;
  end;
end;

procedure TRegularPolygonTool.MouseMove(Sender: TObject;
  Shift: TShiftState; MousePoint: TDoublePoint);
begin
  if GlobalIsMouseDownLeft and (TempRegularPolygon <> nil) then
    BuildMethodBack(nil);
end;

procedure TRegularPolygonTool.MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; MousePoint: TDoublePoint);
begin
  if TempRegularPolygon <> nil then
  begin
    TempFigure := TempRegularPolygon;
    TempRegularPolygon := nil;
  end;
end;

procedure TRegularPolygonTool.BuildRegularPolygon(Vertices: integer);
var
  i: integer;
  Rad: double;
  Point: TDoublePoint;
begin
  Rad := sqrt(power(TempRegularPolygon.Center.X - S2WX(GlobalMousePoint.X),
    2) + power(TempRegularPolygon.Center.Y - S2WY(GlobalMousePoint.Y), 2));
  setLength(TempRegularPolygon.Points, 0);
  for i := 0 to Vertices do
  begin
    Point.x := TempRegularPolygon.Center.X + Rad * cos(
      ((2 * pi * i) / Vertices));
    Point.y := TempRegularPolygon.Center.Y + Rad * sin(
      ((2 * pi * i) / Vertices));
    TempRegularPolygon.AddPoint(Point);
  end;
end;

function TRegularPolygonTool.GetGlyphString: string;
begin
  Result := 'glyphs/regularpolygon.bmp';
end;

procedure RegisterTool(tool: TTool);
begin
  SetLength(ToolArray, Length(ToolArray) + 1);
  ToolArray[High(ToolArray)] := tool;
end;

initialization

  RegisterTool(TPenTool.Create);
  RegisterTool(TLineTool.Create);
  RegisterTool(TRectangleTool.Create);
  RegisterTool(TEllipseTool.Create);
  RegisterTool(TPolyLineTool.Create);
  RegisterTool(TPolygonTool.Create);
  RegisterTool(TRegularPolygonTool.Create);
  RegisterTool(TRoundedRectangleTool.Create);

end.
