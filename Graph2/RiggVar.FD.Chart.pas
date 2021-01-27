unit RiggVar.FD.Chart;

(*
-
-     F
-    * * *
-   *   *   G
-  *     * *   *
- E - - - H - - - I
-  *     * *         *
-   *   *   *           *
-    * *     *             *
-     D-------A---------------B
-              *
-              (C) federgraph.de
-
*)

interface

uses
  System.Types,
  System.UITypes,
  Graphics,
  RiggVar.FB.Color,
  RiggVar.FD.Elements;

type
  TRggBox = record
    X: Integer;
    Y: Integer;
    Width: Integer;
    Height: Integer;
  end;

  TRggChart = class(TRggElement)
  protected
    LNr: Integer;
  public
    Poly: array of single;

    Box: TRggBox;

    Xmin: single;
    Xmax: single;
    Ymin: single;
    Ymax: single;

    ChartPunktX: single;

    WantChartPunktX: Boolean;
    WantRectangles: Boolean;
    WantCurve: Boolean;

    PointRadius: Integer;
    CurveOpacity: single;

    constructor Create(ACount: Integer = 20);
    procedure Draw(g: TCanvas); override;

    procedure InitDefault;

    procedure LookForYMinMax;
    property Count: Integer read LNr;
  end;

implementation

{ TRggChart }

procedure TRggChart.LookForYMinMax;
var
  i: Integer;
  t: single;
begin
  Ymax := Poly[0];
  Ymin := Ymax;
  for i := 0 to LNr do
  begin
    t := Poly[i];
    if t > Ymax then
      Ymax := t;
    if t < Ymin then
      Ymin := t;
  end;
end;

constructor TRggChart.Create(ACount: Integer = 20);
begin
  inherited Create;
  TypeName := 'Chart';
  IndentItem := True;

  LNr := ACount;
  if ACount > 9 then
    LNr := ACount;

  SetLength(Poly, LNr + 1);

  Box.X := 0;
  Box.Y := 0;
  Box.Width := 800;
  Box.Height := 800;

  PointRadius := 3;

  WantCurve := True;
end;

procedure TRggChart.Draw(g: TCanvas);
var
  P: TPoint;
  i: Integer;
  tempX: single;
  tempY: single;

  ox, oy: Integer;

  function Limit(a: single): single;
  begin
    if a < -32000 then
      a := -32000
    else if a > 32000 then
      a := 32000;
    Result := a;
  end;

begin
  ox := Round(Box.X + Drawing.FaxPoint3D.X);
  oy := Round(Box.Y + Drawing.FaxPoint3D.Y);

  g.Pen.Width := StrokeThickness;
  g.Pen.Color := StrokeColor;

  if WantCurve then
  begin
    g.Pen.Color := StrokeColor;
  tempY := Box.Height - Box.Height * (Poly[0] - Ymin) / (Ymax - Ymin);
    P.X := ox;
    P.Y := oy + Round(Limit(tempY));
    g.MoveTo(P.X, P.Y);
    for i := 1 to LNr do
    begin
      tempX := Box.Width * i / LNr;
      tempY := Box.Height - Box.Height * (Poly[i] - Ymin) / (Ymax - Ymin);
      P.X := ox + Round(Limit(tempX));
      P.Y := oy + Round(Limit(tempY));
      g.LineTo(P.X, P.Y);
    end;
  end;

  if WantRectangles then
  begin
    g.Pen.Width := 1;
    g.Pen.Color := TRggColors.White;
    g.Brush.Color := StrokeColor;
    for i := 0 to LNr do
    begin
      tempX := Box.Width * i / LNr;
      tempY := Box.Height - Box.Height * (Poly[i] - Ymin) / (Ymax - Ymin);
      P.X := ox + Round(Limit(tempX));
      P.Y := oy + Round(Limit(tempY));
      g.FillRect(
        Rect(P.X - PointRadius, P.Y - PointRadius,
             P.X + PointRadius, P.Y + PointRadius));
    end;
  end;

  if WantChartPunktX then
  begin
    tempX := Box.Width * ((ChartPunktX) - Xmin) / (XMax - Xmin);
    tempY := Box.Height;
    P.X := ox + Round(Limit(tempX));
    P.Y := oy + Round(Limit(tempY));
    g.FillRect(
      Rect(P.X - PointRadius, P.Y - PointRadius,
           P.X + PointRadius, P.Y + PointRadius));
  end;

end;

procedure TRggChart.InitDefault;
var
  i: Integer;
begin
  for i := 0 to LNr do
  begin
    Poly[i] := sin(i / LNr * 2 * Pi);
  end;
  LookForYMinMax;
end;

end.
