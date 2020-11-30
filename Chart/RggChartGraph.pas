unit RggChartGraph;

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
  RggInter,
  RggChartModel;

type
  TRggBox = class
  public
    X: single;
    Y: single;
    Width: single;
    Height: single;
  end;

  TChartGraph = class(TChartModel)
  private
    FImage: TImage; // injected, not owned
    FBitmap: TBitmap;

    Width: Integer;
    Height: Integer;
    FScale: single;

    LineData: TZugPolyLine;

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
    constructor Create(ARigg: IRigg);
    destructor Destroy; override;
    procedure Draw; override;
    property Image: TImage read FImage write SetImage;
  end;

implementation

uses
  RiggVar.App.Main;

{ TChartGraph }

constructor TChartGraph.Create(ARigg: IRigg);
begin
  inherited;

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

  WantRectangles := True;
  WantTextRect := False;
  WantLegend := True;
end;

destructor TChartGraph.Destroy;
begin
  FBitmap.Free;
  Box.Free;
  inherited;
end;

procedure TChartGraph.InitBitmap;
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

procedure TChartGraph.SetImage(const Value: TImage);
begin
  FImage := Value;
  InitBitmap;
end;

procedure TChartGraph.Draw;
begin
  if (Image <> nil) and (FBitmap <> nil) then
  begin
    DrawToCanvas(FBitmap.Canvas);
    Image.Canvas.CopyMode := cmSrcCopy;
    Image.Canvas.Draw(0, 0, FBitmap);
  end;
end;

procedure TChartGraph.DrawToCanvas(g: TCanvas);
begin
  if g = nil then
    Exit;

  PaintBackground(g);
  DrawLegend(g);
  DrawChart(g);
end;

procedure TChartGraph.PaintBackground(g: TCanvas);
var
  R: TRect;
begin
  R := Rect(0, 0, Image.Width, Image.Height);
  g.Brush.Color := clNavy;
  g.FillRect(R);
end;

procedure TChartGraph.DrawChart(g: TCanvas);
var
  P: TPointF;
  i, param: Integer;
  Radius: single;
  tempX, tempY: single;
  WantChartPunktX: Boolean;

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

  { ChartPunktX }
  WantChartPunktX := True;
  if WantChartPunktX then
  begin
    g.Pen.Color := clRed;
    tempX := Box.Width * ((ChartPunktX) - Xmin) / (XMax - Xmin);
    tempY := Box.Height;
    DrawVerticalLine;

    g.Pen.Color := clSilver;
    tempX := Box.Width * ((ChartPunktX-APWidth) - Xmin) / (XMax - Xmin);
    DrawVerticalLine;

    tempX := Box.Width * ((ChartPunktX+APWidth) - Xmin) / (XMax - Xmin);
    DrawVerticalLine;
  end;

  Radius := 3 * FScale;

  for param := 0 to ParamCount-1 do
  begin
    { Kurve }
    g.Pen.Color := cf[param];
    tempY := Box.Height - Box.Height * (bf[param, 0] - Ymin) / (Ymax - Ymin);
    P.X := Box.X;
    P.Y := Box.Y + Round(Limit(tempY));
    LineData[0].X := Round(P.X * FScale);
    LineData[0].Y := Round(P.Y * FScale);
    for i := 1 to LNr do
    begin
      tempX := Box.Width * i / LNr;
      tempY := Box.Height - Box.Height * (bf[param, i] - Ymin) / (Ymax - Ymin);
      P.X := Box.X + Limit(tempX);
      P.Y := Box.Y + Limit(tempY);
      LineData[i].X := Round(P.X * FScale);
      LineData[i].Y := Round(P.Y * FScale);
    end;
    g.Polyline(LineData);

    if WantRectangles then
    begin
      { Rechtecke }
      g.Pen.Width := 1;
      g.Pen.Color := clWhite;
      g.Brush.Color := cf[param];
      for i := 0 to LNr do
      begin
        tempX := Box.Width * i / LNr;
        tempY := Box.Height - Box.Height * (bf[param, i] - Ymin) / (Ymax - Ymin);
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

procedure TChartGraph.DrawLabels(g: TCanvas);
var
  PosX: Integer;
  PosY: Integer;
  s: string;

  procedure TextRect(s: string);
  begin
    g.TextOut(PosX, PosY, s);
  end;

begin
  if not WantLegend then
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
  s := Format('Xmin..Xmax = %.1f .. %.1f', [Xmin, Xmax]);
  TextRect(s);

  { Column 1, Row 2 }
  PosY := PosY + Raster;
  s := Format('Ymin..Ymax = %.1f .. %.1f', [Ymin, Ymax]);
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
  s := XTitle;
  TextRect(s);

  { Column 2, Row 2 }
  PosY := PosY + Raster;
  s := YTitle;
  TextRect(s);

  { Column 2, Row 3 }
  if ParamCount > 1 then
  begin
    PosX := PosX + Round(15 * FScale);
    PosY := PosY + Raster;
    s := PTitle;
    TextRect(s);
  end;
end;

procedure TChartGraph.DrawLegend(g: TCanvas);
var
  param: Integer;
  PosX, PosY: Integer;
  s: string;
var
  bw, bh: Integer;
begin
  if not WantLegend then
    Exit;

  if ParamCount < 2 then
    Exit;

  bw :=Round(16 * FScale);
  bh := Round(3 * FScale);

  { continue in Colum  1, Row 4 }
  PosX := Round(20 * FScale);
  PosY := 3 * Raster + Round(10 * FScale);
  g.Font.Name := 'Consolas';
  g.Font.Size := 13;
  for param := 0 to ParamCount-1 do
  begin
    { Bullet }
    g.Brush.Style := TBrushStyle.bsSolid;
    g.Pen.Width := 1;
    g.Pen.Color := clWhite;
    g.Brush.Color := cf[param];
    g.FillRect(Rect(PosX, PosY, PosX + bw, PosY + bh));

    { Text }
    g.Brush.Style := TBrushStyle.bsClear;
    g.Font.Color := clSilver;
    PosY := PosY + Round(16 * FScale);
    if Valid then
      s := PText[param]
    else
      s := PColorText[param];
    g.TextOut(PosX, PosY, s);
    PosY := PosY + Round(30 * FScale);
  end;
end;

end.
