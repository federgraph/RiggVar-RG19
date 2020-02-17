unit RggGetriebeGraph;

interface

uses
  Winapi.Windows,
  System.Classes,
  System.Types,
  Vcl.Graphics,
  Vcl.ExtCtrls,
  Vcl.Dialogs,
  RggTypes,
  RggCalc,
  RggMatrix,
  RggZug,
  RggRaumGraph;

type
  TGetriebeGraph = class(TRaumGraph)
  protected
    Zug1: TZug1;
    Zug2: TZug2;
    Zug3: TZug3;
    procedure Update2D;

    procedure SetZoom(Value: single); override;
  public
    FZoom2D: double; // eventuell von Zoom unterschiedlicher Faktor für 2D
    FZoomFaktor: Integer; // Faktor für 'Auflösung' (typisch 0 oder 10)
    RelationZoom2D: double; // Verhältnis Zoom2D / Zoom. (Zoom = Zoom3D)

    cOffsetX1, cOffsetY1: Integer;
    cOffsetX2, cOffsetY2: Integer;
    cOffsetX3, cOffsetY3: Integer;
    cOffsetX4, cOffsetY4: Integer;

    OffsetX1, OffsetY1: Integer;
    OffsetX2, OffsetY2: Integer;
    OffsetX3, OffsetY3: Integer;
    OffsetX4, OffsetY4: Integer;

    procedure SetZoomFaktor(Value: Integer);
    procedure UpdateOffset;
    procedure CalcOffset(R: TRect);
  public
    constructor Create;
    destructor Destroy; override;

    procedure InitZoom;
    procedure InitRotation;

    procedure Update; override;
    procedure DrawToCanvas(g: TCanvas); override;
    procedure DrawToMeta(Canvas: TCanvas);

    property ZoomFaktor: Integer read FZoomFaktor write SetZoomFaktor;
  end;

implementation

uses
  RiggVar.RG.Def;

constructor TGetriebeGraph.Create;
begin
  inherited Create;

//  InitZoom; // needs injected Transformer

  Zug1 := TZug1.Create;
  Zug2 := TZug2.Create;
  Zug3 := TZug3.Create;

  Zug1.Data := RaumGraphData;
  Zug1.Props := RaumGraphProps;
  Zug2.Data := RaumGraphData;
  Zug2.Props := RaumGraphProps;
  Zug3.Data := RaumGraphData;
  Zug3.Props := RaumGraphProps;

  { Offset }
  cOffsetX1 := 130;
  cOffsetY1 := 410;

  cOffsetX2 := 130;
  cOffsetY2 := 410;

  cOffsetX3 := 130;
  cOffsetY3 := 150;

  cOffsetX4 := 150;
  cOffsetY4 := 250;

  UpdateOffset;

  Koppel := True;
  ViewPoint := vp3D;
  Color := clEntspannt;
  Bogen := True;
end;

destructor TGetriebeGraph.Destroy;
begin
  Zug1.Free;
  Zug2.Free;
  Zug3.Free;

  inherited Destroy;
end;

procedure TGetriebeGraph.SetZoom(Value: single);
begin
  inherited SetZoom(Value);
  FZoom2D := RelationZoom2D * Zoom;
end;

{ mit ZoomFaktor kann in die 'höhere Auflösung' umgeschaltet werden }
procedure TGetriebeGraph.SetZoomFaktor(Value: Integer);
begin
  if Value <> FZoomFaktor then
  begin
    { Zoom und! Zoom2D werden verändert in SetZoom() }
    Zoom := Zoom / ZoomFaktor * Value;
    FZoomFaktor := Value;
    { Im Gegensatz zu Zoom wird mit Zoomfaktor auch der Offset verschoben!
      Dies ist für die Ausgabe nach Preview und auf Drucker notwendig. }
    UpdateOffset;
  end;
end;

procedure TGetriebeGraph.UpdateOffset;
begin
  OffsetX1 := cOffsetX1 * FZoomFaktor;
  OffsetY1 := cOffsetY1 * FZoomFaktor;
  OffsetX2 := cOffsetX2 * FZoomFaktor;
  OffsetY2 := cOffsetY2 * FZoomFaktor;
  OffsetX3 := cOffsetX3 * FZoomFaktor;
  OffsetY3 := cOffsetY3 * FZoomFaktor;
  OffsetX4 := cOffsetX4 * FZoomFaktor;
  OffsetY4 := cOffsetY4 * FZoomFaktor;

  Zug1.ZoomFaktor := FZoomFaktor;

  Zug1.OffsetX1 := OffsetX1;
  Zug1.OffsetY1 := OffsetY1;

  Zug2.OffsetX2 := OffsetX2;
  Zug2.OffsetY2 := OffsetY2;

  Zug3.OffsetX3 := OffsetX3;
  Zug3.OffsetY3 := OffsetY3;

  Zug4.OffsetX4 := OffsetX4;
  Zug4.OffsetY4 := OffsetY4;

  Updated := False;
end;

procedure TGetriebeGraph.Update;
begin
  UpdateZugProps;
  Update2D;
  case ViewPoint of
    vpSeite: Zug1.FillZug;
    vpAchtern: Zug2.FillZug;
    vpTop: Zug3.FillZug;
  end;

  if Viewpoint = vp3D then
  begin
    inherited Update;
    Zug4.FillZug;
  end;
end;

procedure TGetriebeGraph.DrawToCanvas(g: TCanvas);
begin
  if not GrafikOK then
    Exit;
  if not Updated then
    Update;

  with g do
  begin
    case ViewPoint of
      vpSeite: Zug1.DrawToCanvas(g);
      vpAchtern: Zug2.DrawToCanvas(g);
      vpTop: Zug3.DrawToCanvas(g);
      vp3D: Zug4.DrawToCanvas(g);
    end;
  end;
end;

procedure TGetriebeGraph.DrawToMeta(Canvas: TCanvas);
begin
  DrawToCanvas(Canvas);
end;

procedure TGetriebeGraph.InitZoom;
begin
  if Transformer <> nil then
  begin
    { Zoom }
    RelationZoom2D := 5.5 / 12;
    FZoomFaktor := 1;
    { wird z.Bsp. für höhere Auflösung auf 10 umgeschaltet }
    Zoom := 1 / 5.5;
    { als allgemeiner Skalierungsfaktor benutzt }
    FZoom2D := RelationZoom2D * Zoom;

    UpdateOffset;
  end;
end;

procedure TGetriebeGraph.InitRotation;
begin
  if (Transformer <> nil) and (Transformer.Rotator <> nil) then
  begin
    Transformer.Rotator.DeltaPhi := 0;
    Transformer.Rotator.DeltaTheta := -90;
    Transformer.Rotator.XRot := -87;
  end;
end;

procedure TGetriebeGraph.Update2D;
var
  j: Integer;
  FixPunkt2D: TRealPoint;
  Temp: TRealPoint;
  xTemp: Integer;
  zTemp: Integer;
begin
  // 1. Koordinate verschieben mit Vektor '-FixPunkt'
  // 2. Skalieren
  // 3. verschieben um Offset
  // 4. Bildschirmkoordinaten entnehmen

  { Fixpunkt enthält nach Aufruf von FillZug3D den gedrehten Fixpunkt, hier wird
    aber immer der nicht gedrehte Fixpunkt benötigt! }
  FixPunkt2D := rP[FixPoint];

  { Koppelkurve }
  if KoppelKurveNeedFill then
  begin
    for j := 0 to 100 do
    begin
      Temp := vsub(KoppelKurve[j], FixPunkt2D);
      xTemp := Round(Temp[x] * FZoom2D);
      zTemp := Round(Temp[z] * FZoom2D);
      Zug1.Zug1KoppelKurve[j+1].X :=  xTemp + OffsetX1;
      Zug1.Zug1KoppelKurve[j+1].Y := -zTemp + OffsetY1;
    end;
    KoppelKurveNeedFill := False;
  end;

  { Mastkurve }
  for j := 0 to BogenMax do
  begin
    Temp := vsub(Kurve[j], FixPunkt2D);
    xTemp := Round(Temp[x] * FZoom2D);
    zTemp := Round(Temp[z] * FZoom2D);
    Zug1.Zug1MastKurve[j+1].X :=  xTemp + OffsetX1;
    Zug1.Zug1MastKurve[j+1].Y := -zTemp + OffsetY1;
  end;

  { Radius für Kreisbogen um C0 }
  Zug1.IntR := Round( Abstand(rP[ooC], rP[ooC0]) * FZoom2D );

  A0 := vsub(rP[ooA0], FixPunkt2D);
  B0 := vsub(rP[ooB0], FixPunkt2D);
  C0 := vsub(rP[ooC0], FixPunkt2D);
  D0 := vsub(rP[ooD0], FixPunkt2D);
  E0 := vsub(rP[ooE0], FixPunkt2D);
  F0 := vsub(rP[ooF0], FixPunkt2D);
  A := vsub(rP[ooA], FixPunkt2D);
  B := vsub(rP[ooB], FixPunkt2D);
  C := vsub(rP[ooC], FixPunkt2D);
  D := vsub(rP[ooD], FixPunkt2D);
  E := vsub(rP[ooE], FixPunkt2D);
  F := vsub(rP[ooF], FixPunkt2D);

  with RaumGraphData do
  begin
    xA0 := Round(A0[x] * FZoom2D);
    yA0 := Round(A0[y] * FZoom2D);
    zA0 := Round(A0[z] * FZoom2D);

    xB0 := xA0;
    yB0 := -yA0;
    zB0 := zA0;

    xC0 := Round(C0[x] * FZoom2D);
    yC0 := 0;
    zC0 := Round(C0[z] * FZoom2D);

    Zug1.MP.x := xC0 + OffsetX1;
    Zug1.MP.y := -zC0 + OffsetY1;

    xD0 := Round(D0[x] * FZoom2D);
    yD0 := 0;
    zD0 := Round(D0[z] * FZoom2D);

    xE0 := Round(E0[x] * FZoom2D);
    yE0 := 0;
    zE0 := Round(E0[z] * FZoom2D);

    xF0 := Round(F0[x] * FZoom2D);
    yF0 := 0;
    zF0 := Round(F0[z] * FZoom2D);

    xA := Round( A[x] * FZoom2D);
    yA := Round( A[y] * FZoom2D);
    zA := Round( A[z] * FZoom2D);

    xB := xA;
    yB := -yA;
    zB := zA;

    xC := Round(C[x] * FZoom2D);
    yC := 0;
    zC := Round(C[z] * FZoom2D);

    xD := Round(D[x] * FZoom2D);
    yD := 0;
    zD := Round(D[z] * FZoom2D);

    xE := Round(E[x] * FZoom2D);
    yE := 0;
    zE := Round(E[z] * FZoom2D);

    xF := Round(F[x] * FZoom2D);
    yF := 0;
    zF := Round(F[z] * FZoom2D);
  end;
end;

procedure TGetriebeGraph.CalcOffset(R: TRect);
var
  k: TRiggPoint;
  Temp: TRealPoint;
  xmin, xmax, ymin, ymax, zmin, zmax: Integer;
  tempx, tempy, tempz: Integer;
  w, h: Integer;
  tempf1, tempf2: double;
begin
  { maximale Abmessungen ermitteln }
  Temp := rP[ooD0]; // beliebiger Eckpunkt
  xmin := Round(Temp[x]);
  xmax := xmin;
  ymin := Round(Temp[y]);
  ymax := ymin;
  zmin := Round(Temp[z]);
  zmax := zmin;
  for k := ooA0 to ooF do
  begin
    if k = ooF0 then
      Continue;
    Temp := rP[k];
    tempx := Round(Temp[x]);
    tempy := Round(Temp[y]);
    tempz := Round(Temp[z]);
    if tempx < xmin then
      xmin := tempx;
    if tempy < ymin then
      ymin := tempy;
    if tempz < zmin then
      zmin := tempz;
    if tempx > xmax then
      xmax := tempx;
    if tempy > ymax then
      ymax := tempy;
    if tempz > zmax then
      zmax := tempz;
  end;
  tempx := xmax-xmin;
  tempy := ymax-ymin;
  tempz := zmax-zmin;

  try
    { Zoom festlegen }
    w := R.Right - R.Left; // 293
    h := R.Bottom - R.Top; // 422
    if tempx > tempy then
      tempx := tempy;
    tempf1 := tempx/w;
    tempf2 := tempz/h;
    if tempf2 > tempf1 then
      tempf1 := tempf2;
    tempf1 := tempf1*FZoom2D;
    Zoom := 0.95 * Zoom / tempf1;

    { Offset festlegen }
    cOffsetX1 := Round(0.44 * w);
    cOffsetY1 := Round(0.98 * h);

    cOffsetX2 := cOffsetX1;
    cOffsetY2 := cOffsetY1;

    cOffsetX3 := cOffsetX1;
    cOffsetY3 := Round(0.35 * h);

    cOffsetX4 := Round(0.52 * w);
    cOffsetY4 := Round(0.59 * h);

    UpdateOffset;
  except
    ShowMessage('Offset kann nicht berechnet werden.');
  end;
end;

end.
