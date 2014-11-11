unit UTools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Controls, Dialogs, Buttons, UFigures,
  ExtCtrls, UScale, UProperties, UGlobalPointers;

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
    procedure AddProperty(prop: TProperty);
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
    procedure MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; MousePoint: TDoublePoint); override;
    procedure MouseMove(Sender: TObject; Shift: TShiftState;
      MousePoint: TDoublePoint); override;
    procedure MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; MousePoint: TDoublePoint); override;
    function GetGlyphString: string; override;
    constructor Create;
  end;

var
  ToolArray: array of TTool;
  SelectedTool: integer;
  TempFigure: TFigure;

procedure RegisterTool(tool: TTool);

implementation

procedure TTool.AddProperty(prop: TProperty);
begin
  SetLength(Properties, Length(Properties) + 1);
  Properties[High(Properties)] := prop;
end;

//Карандаш

constructor TPenTool.Create;
begin
  AddProperty(TPenStyleProperty.Create(psSolid));
  AddProperty(TPenWidthProperty.Create(1));
end;

procedure TPenTool.MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; MousePoint: TDoublePoint);
begin
  if Button = mbLeft then
  begin
    if TempFigure = nil then
    begin
      TempPenLine := TPenLine.Create(Properties);
      TempFigure := TempPenLine;
    end
    else
    begin
      AddFigure(TempPenLine);
      TempFigure := nil;
    end;
  end;
end;

procedure TPenTool.MouseMove(Sender: TObject; Shift: TShiftState;
  MousePoint: TDoublePoint);
begin
  if GlobalIsMouseDownLeft and (TempFigure <> nil) then
    TempPenLine.AddPoint(MousePoint);
end;

procedure TPenTool.MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; MousePoint: TDoublePoint);
begin
  if TempFigure <> nil then
  begin
    AddFigure(TempPenLine);
    TempFigure := nil;
  end;
end;

function TPenTool.GetGlyphString: string;
begin
  Result := 'glyphs/pen.bmp';
end;

//Линия

constructor TLineTool.Create;
begin
  AddProperty(TPenStyleProperty.Create(psSolid));
  AddProperty(TPenWidthProperty.Create(1));
end;

procedure TLineTool.MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; MousePoint: TDoublePoint);
begin
  if Button = mbLeft then
  begin
    if TempFigure = nil then
    begin
      TempLine := TLine.Create(Properties);
      TempLine.Point1 := MousePoint;
      TempLine.Point2 := MousePoint;
      TempFigure := TempLine;
    end
    else
    begin
      AddFigure(TempLine);
      TempFigure := nil;
    end;
  end;
end;

procedure TLineTool.MouseMove(Sender: TObject; Shift: TShiftState;
  MousePoint: TDoublePoint);
begin
  if GlobalIsMouseDownLeft and (TempFigure <> nil) then
  begin
    TempLine.Point2 := MousePoint;
  end;
end;

procedure TLineTool.MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; MousePoint: TDoublePoint);
begin
  if TempFigure <> nil then
  begin
    AddFigure(TempLine);
    TempFigure := nil;
  end;
end;

function TLineTool.GetGlyphString: string;
begin
  Result := 'glyphs/line.bmp';
end;

//Прямоугольник

constructor TRectangleTool.Create;
begin
  AddProperty(TPenStyleProperty.Create(psSolid));
  AddProperty(TPenWidthProperty.Create(1));
  AddProperty(TBrushStyleProperty.Create(bsClear));
end;

procedure TRectangleTool.MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; MousePoint: TDoublePoint);
begin
  if Button = mbLeft then
  begin
    if TempFigure = nil then
    begin
      TempRectangle := TRectangle.Create(Properties);
      TempRectangle.Point1 := MousePoint;
      TempRectangle.Point2 := MousePoint;
      TempFigure := TempRectangle;
    end
    else
    begin
      AddFigure(TempRectangle);
      TempFigure := nil;
    end;
  end;
end;

procedure TRectangleTool.MouseMove(Sender: TObject; Shift: TShiftState;
  MousePoint: TDoublePoint);
begin
  if GlobalIsMouseDownLeft and (TempFigure <> nil) then
  begin
    TempRectangle.Point2 := MousePoint;
  end;
end;

procedure TRectangleTool.MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; MousePoint: TDoublePoint);
begin
  if TempFigure <> nil then
  begin
    AddFigure(TempRectangle);
    TempFigure := nil;
  end;
end;

function TRectangleTool.GetGlyphString: string;
begin
  Result := 'glyphs/rectangle.bmp';
end;

//Эллипс

constructor TEllipseTool.Create;
begin
  AddProperty(TPenStyleProperty.Create(psSolid));
  AddProperty(TPenWidthProperty.Create(1));
  AddProperty(TBrushStyleProperty.Create(bsClear));
end;

procedure TEllipseTool.MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; MousePoint: TDoublePoint);
begin
  if Button = mbLeft then
  begin
    if TempFigure = nil then
    begin
      TempEllipse := TEllipse.Create(Properties);
      TempEllipse.Point1 := MousePoint;
      TempEllipse.Point2 := MousePoint;
      TempFigure := TempEllipse;
    end
    else
    begin
      AddFigure(TempEllipse);
      TempFigure := nil;
    end;
  end;
end;

procedure TEllipseTool.MouseMove(Sender: TObject; Shift: TShiftState;
  MousePoint: TDoublePoint);
begin
  if GlobalIsMouseDownLeft and (TempFigure <> nil) then
  begin
    TempEllipse.Point2 := MousePoint;
  end;
end;

procedure TEllipseTool.MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; MousePoint: TDoublePoint);
begin
  if TempFigure <> nil then
  begin
    AddFigure(TempEllipse);
    TempFigure := nil;
  end;
end;

function TEllipseTool.GetGlyphString: string;
begin
  Result := 'glyphs/ellipse.bmp';
end;

//Ломаная

constructor TPolyLineTool.Create;
begin
  AddProperty(TPenStyleProperty.Create(psSolid));
  AddProperty(TPenWidthProperty.Create(1));
end;

procedure TPolyLineTool.MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; MousePoint: TDoublePoint);
begin
  if Button = mbLeft then
  begin
    if TempFigure = nil then
    begin
      TempPolyLine := TPolyLine.Create(Properties);
      TempPolyLine.AddPoint(MousePoint);
      TempPolyLine.AddPoint(MousePoint);
    end
    else
      TempPolyLine.AddPoint(MousePoint);
    TempFigure := TempPolyLine;
  end;
  if (Button = mbRight) and (TempFigure <> nil) then
  begin
    TempPolyLine.AddPoint(MousePoint);
    AddFigure(TempPolyLine);
    TempFigure := nil;
  end;
end;

procedure TPolyLineTool.MouseMove(Sender: TObject; Shift: TShiftState;
  MousePoint: TDoublePoint);
begin
  if TempFigure <> nil then
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
  AddProperty(TPenStyleProperty.Create(psSolid));
  AddProperty(TPenWidthProperty.Create(1));
  AddProperty(TBrushStyleProperty.Create(bsClear));
end;

procedure TPolygonTool.MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; MousePoint: TDoublePoint);
begin
  if Button = mbLeft then
  begin
    if TempFigure = nil then
    begin
      TempPolygon := TPolygon.Create(Properties);
      TempFigure := TempPolygon;
      TempPolygon.AddPoint(MousePoint);
      TempPolygon.AddPoint(MousePoint);
    end
    else
      TempPolygon.AddPoint(MousePoint);
  end;
  if (Button = mbRight) and (TempFigure <> nil) then
  begin
    TempPolygon.AddPoint(MousePoint);
    AddFigure(TempPolygon);
    TempFigure := nil;
  end;
end;

procedure TPolyGonTool.MouseMove(Sender: TObject; Shift: TShiftState;
  MousePoint: TDoublePoint);
begin
  if TempFigure <> nil then
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
begin
  AddProperty(TPenStyleProperty.Create(psSolid));
  AddProperty(TPenWidthProperty.Create(1));
  AddProperty(TBrushStyleProperty.Create(bsClear));
end;

procedure TRegularPolygonTool.MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; MousePoint: TDoublePoint);
begin
  if Button = mbLeft then
  begin
    if TempFigure = nil then
    begin
      TempRegularPolygon := TRegularPolygon.Create(Properties);
      TempFigure := TempRegularPolygon;
    end
    else
    begin
      AddFigure(TempRegularPolygon);
      TempFigure := nil;
    end;
  end;
end;

procedure TRegularPolygonTool.MouseMove(Sender: TObject;
  Shift: TShiftState; MousePoint: TDoublePoint);
begin
  if GlobalIsMouseDownLeft and (TempFigure <> nil) then
    TempRegularPolygon.BuildRegularPolygon(MousePoint);
end;

procedure TRegularPolygonTool.MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; MousePoint: TDoublePoint);
begin
  if TempFigure <> nil then
  begin
    AddFigure(TempRegularPolygon);
    TempFigure := nil;
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

end.
