unit RggDiagram;

interface

{$ifdef fpc}
{$mode delphi}
{$endif}

uses
  SysUtils,
  Classes,
  Types,
  ExtCtrls,
  Graphics,
  RggTypes,
  RggChartModel,
  RggChartModel01;

type
  TRggBox = class
  public
    X: single;
    Y: single;
    Width: single;
    Height: single;
  end;

  TRggDiagram = class
  private
    FImage: TImage; // injected, not owned
    FBitmap: TBitmap;

    Width: Integer;
    Height: Integer;
    FScale: single;

    LineData: TZugPolyLine;

    CM: TChartModel; // injected, not owned

    procedure InitBitmap;
    procedure SetImage(const Value: TImage);
  private
    Box: TRggBox;
    Raster: Integer;
    Padding: Integer;
  private
    procedure DrawToCanvas(g: TCanvas);
    procedure DrawChart(g: TCanvas);
    procedure DrawLabels(g: TCanvas);
    procedure DrawLegend(g: TCanvas);
    procedure PaintBackground(g: TCanvas);
  public
    constructor Create(Model: TChartModel);
    destructor Destroy; override;
    procedure Draw(Sender: TObject);
    property Image: TImage read FImage write SetImage;
  end;

implementation

uses
  RiggVar.App.Main;

{ TChartGraph }

constructor TRggDiagram.Create(Model: TChartModel);
begin
  CM := Model;

  FScale := MainVar.Scale;

  SetLength(LineData, LNr + 1);

  Width := 650;
  Height := 400;

  Box := TRggBox.Create;
  Box.X := 120;
  Box.Y := 80;
  Box.Width := 500;
  Box.Height := 300;

  Raster := Round(24 * FScale);
  Padding := Round(2 * FScale);

  CM.WantRectangles := True;
  CM.WantTextRect := False;
  CM.WantLegend := True;
end;

destructor TRggDiagram.Destroy;
begin
  FBitmap.Free;
  Box.Free;
  inherited;
end;

procedure TRggDiagram.InitBitmap;
begin
  if FBitmap <> nil then
  begin
    FBitmap.Free;
  end;
  FBitmap := TBitmap.Create;
  FBitmap.Width := Round(Width * FScale);
  FBitmap.Height := Round(Height * FScale);
  Image.Width := FBitmap.Width;
  Image.Height := FBitmap.Height;
end;

procedure TRggDiagram.SetImage(const Value: TImage);
begin
  FImage := Value;
  InitBitmap;
end;

procedure TRggDiagram.Draw(Sender: TObject);
begin
  if (Image <> nil) and (FBitmap <> nil) then
  begin
    DrawToCanvas(FBitmap.Canvas);
    Image.Canvas.CopyMode := cmSrcCopy;
    Image.Canvas.Draw(0, 0, FBitmap);
  end;
end;

procedure TRggDiagram.DrawToCanvas(g: TCanvas);
begin
  if g = nil then
    Exit;

  PaintBackground(g);
  DrawLegend(g);
  DrawChart(g);
end;

procedure TRggDiagram.PaintBackground(g: TCanvas);
var
  R: TRect;
begin
  R := Rect(0, 0, Image.Width, Image.Height);
  g.Brush.Color := clNavy;
  g.FillRect(R);
end;

procedure TRggDiagram.DrawChart(g: TCanvas);
var
  P: TPointF;
  i, param: Integer;
  Radius: single;
  tempX, tempY: single;
  WantChartPunktX: Boolean;
  xrange: single;
  yrange: single;

  function Limit(a: single): single;
  begin
    if a < -32000 then
      a := -32000
    else if a > 32000 then
      a := 32000;
    Result := a;
  end;

  procedure DrawVerticalLine;
  begin
    P.X := Box.X + Limit(tempX);
    P.Y := Box.Y + Limit(tempY);
    LineData[0].X := Round(P.X * FScale);
    LineData[0].Y := Round(P.Y * FScale);
    P.Y := Box.Y;
    LineData[1].X := Round(P.X * FScale);
    LineData[1].Y := Round(P.Y * FScale);
    g.MoveTo(LineData[0].X, LineData[0].Y);
    g.LineTo(LineData[1].X, LineData[1].Y);
  end;

begin
  DrawLabels(g);

  g.Pen.Width := 1;

  xrange := CM.Xmax - CM.Xmin;
  yrange := CM.Ymax - CM.Ymin;

  { ChartPunktX }
  WantChartPunktX := True;
  if WantChartPunktX then
  begin
    g.Pen.Color := clRed;
    tempX := Box.Width * ((CM.ChartPunktX) - CM.Xmin) / xrange;
    tempY := Box.Height;
    DrawVerticalLine;

    g.Pen.Color := clSilver;
    tempX := Box.Width * (CM.ChartPunktX - CM.APWidth - CM.Xmin) / xrange;
    DrawVerticalLine;

    tempX := Box.Width * (CM.ChartPunktX + CM.APWidth - CM.Xmin) / xrange;
    DrawVerticalLine;
  end;

  Radius := 3 * FScale;

  for param := 0 to CM.ParamCount - 1 do
  begin
    { Kurve }
    g.Pen.Color := CM.cf[param];
    tempY := Box.Height - Box.Height * (CM.bf[param, 0] - CM.Ymin) / yrange;
    P.X := Box.X;
    P.Y := Box.Y + Round(Limit(tempY));
    LineData[0].X := Round(P.X * FScale);
    LineData[0].Y := Round(P.Y * FScale);
    for i := 1 to LNr do
    begin
      tempX := Box.Width * i / LNr;
      tempY := Box.Height - Box.Height * (CM.bf[param, i] - CM.Ymin) / yrange;
      P.X := Box.X + Limit(tempX);
      P.Y := Box.Y + Limit(tempY);
      LineData[i].X := Round(P.X * FScale);
      LineData[i].Y := Round(P.Y * FScale);
    end;
    g.Polyline(LineData);

    if CM.WantRectangles then
    begin
      { Rechtecke }
      g.Pen.Width := 1;
      g.Pen.Color := clWhite;
      g.Brush.Color := CM.cf[param];
      for i := 0 to LNr do
      begin
        tempX := Box.Width * i / LNr;
        tempY := Box.Height - Box.Height * (CM.bf[param, i] - CM.Ymin) / yrange;
        P.X := Box.X + Limit(tempX);
        P.Y := Box.Y + Limit(tempY);
        P.X := P.X * FScale;
        P.Y := P.Y * FScale;
        g.FillRect(Rect(
          Round(P.x - Radius), Round(P.y - Radius),
          Round(P.x + Radius), Round(P.y + Radius)));
      end;
    end;

  end;
end;

procedure TRggDiagram.DrawLabels(g: TCanvas);
var
  PosX: Integer;
  PosY: Integer;
  s: string;

  procedure TextRect(s: string);
  begin
    g.TextOut(PosX, PosY, s);
  end;

begin
  if not CM.WantLegend then
    Exit;

  g.Pen.Width := 1;
  g.Pen.Color := clYellow;
  g.Brush.Style := TBrushStyle.bsClear;
  g.Font.Name := 'Consolas';
  g.Font.Size := 13;

  { Column 1 }
  PosX := Padding;

  { Column 1, Row 1 }
  PosY := Padding;
  s := Format('Xmin..Xmax = %.1f .. %.1f', [CM.Xmin, CM.Xmax]);
  TextRect(s);

  { Column 1, Row 2 }
  PosY := PosY + Raster;
  s := Format('Ymin..Ymax = %.1f .. %.1f', [CM.Ymin, CM.Ymax]);
  TextRect(s);

  { Column 1, Row 3 }
  PosX := Round(20 * FScale);
  PosY := PosY + Raster;
  s := 'Parameter';
  TextRect(s);

  { Column 2 }
  PosX := Round(300 * FScale);

  { Column 2, Row 1 }
  PosY := Padding;
  s := CM.XTitle;
  TextRect(s);

  { Column 2, Row 2 }
  PosY := PosY + Raster;
  s := CM.YTitle;
  TextRect(s);

  { Column 2, Row 3 }
  if CM.ParamCount > 1 then
  begin
    PosX := PosX + Round(15 * FScale);
    PosY := PosY + Raster;
    s := CM.PTitle;
    TextRect(s);
  end;
end;

procedure TRggDiagram.DrawLegend(g: TCanvas);
var
  param: Integer;
  PosX, PosY: Integer;
  s: string;
var
  bw, bh: Integer;
begin
  if not CM.WantLegend then
    Exit;

  if CM.ParamCount < 2 then
    Exit;

  bw :=Round(16 * FScale);
  bh := Round(3 * FScale);

  { continue in Colum  1, Row 4 }
  PosX := Round(20 * FScale);
  PosY := 3 * Raster + Round(10 * FScale);
  g.Font.Name := 'Consolas';
  g.Font.Size := 13;
  for param := 0 to CM.ParamCount - 1 do
  begin
    { Bullet }
    g.Brush.Style := TBrushStyle.bsSolid;
    g.Pen.Width := 1;
    g.Pen.Color := clWhite;
    g.Brush.Color := CM.cf[param];
    g.FillRect(Rect(PosX, PosY, PosX + bw, PosY + bh));

    { Text }
    g.Brush.Style := TBrushStyle.bsClear;
    g.Font.Color := clSilver;
    PosY := PosY + Round(16 * FScale);
    if CM.Valid then
      s := CM.PText[param]
    else
      s := CM.PColorText[param];
    g.TextOut(PosX, PosY, s);
    PosY := PosY + Round(30 * FScale);
  end;
end;

end.
