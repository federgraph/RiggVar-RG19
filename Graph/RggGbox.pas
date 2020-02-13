unit RggGbox;

interface

{.$define Rotator}

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
  RggRaumGraph;

type
  TGetriebeGraph = class(TRaumGraph)
  protected
//    xA0,xB0,xC0,xD0,xE0,xF0,xA,xB,xC,xD,xE,xF: Integer;
//    yA0,yB0,yC0,yD0,yE0,yF0,yA,yB,yC,yD,yE,yF: Integer;
    zA0,zB0,zC0,zD0,zE0,zF0,zA,zB,zC,zD,zE,zF: Integer;

    FKoppelKurve: TKoordLine; // Daten für Koppelkurve
    FKoppelBtnDown: Boolean;
    FKoppelKurveNeedFill: Boolean;
  public
    procedure SetKoppelKurve(const Kurve: TKoordLine); override;
  protected
    MP: TPoint; // Mittelpunkt für Kreis um C0
    IntR: Integer; // Radius für Kreis um C0

    Zug1Rumpf: array[1..4] of TPoint;
    Zug1Mast: array[1..4] of TPoint;
    Zug1MastKurve: array[1..BogenMax+2] of TPoint;
    Zug1Saling: array[1..2] of TPoint;
    Zug1Wanten: array[1..3] of TPoint;
    Zug1Vorstag: array[1..2] of TPoint;
    Zug1KoppelKurve: array[1..101] of TPoint;
    Zug1Controller: TRect;

    Zug2Rumpf: array[1..6] of TPoint;
    Zug2Wanten: array[1..5] of TPoint;
    Zug2Saling: array[1..4] of TPoint;
    Zug2SalingDS: array[1..3] of TPoint;
    Zug2Mast: array[1..2] of TPoint;

    Zug3Wanten: array[1..5] of TPoint;
    Zug3Rumpf: array[1..10] of TPoint;
    Zug3Mast: array[1..4] of TPoint;
    Zug3SalingFS: array[1..4] of TPoint;
    Zug3SalingDS: array[1..3] of TPoint;

    { TPoint-arrays für 3D werden vererbt }

    procedure FillZug2D;
    procedure FillZug3D; override;

    procedure SetZoom(Value: single); override;
  public
    FColor: TColor;
    FZoom2D: double; // eventuell von Zoom unterschiedlicher Faktor für 2D
    FZoomFaktor: Integer; // Faktor für 'Auflösung' (typisch 0 oder 10)
    RelationZoom2D: double; // Verhältnis Zoom2D/Zoom. (Zoom = Zoom3D)

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
    constructor Create; override;
    destructor Destroy; override;

    procedure Update; override;
    procedure DrawToCanvas(Canvas: TCanvas); override;
    procedure DrawToMeta(Canvas: TMetaFileCanvas);

    property Farbe: TColor read FColor write FColor;
    property ZoomFaktor: Integer read FZoomFaktor write SetZoomFaktor;
    property KoppelKurve: TKoordLine read FKoppelKurve write SetKoppelKurve;
    property ZeichneKoppel: Boolean read FKoppelBtnDown write FKoppelBtnDown;
  end;

implementation

uses
  RiggVar.RG.Def;

constructor TGetriebeGraph.Create;
begin
  inherited Create;
  { Zoom }
  RelationZoom2D := 5.5/12;
  FZoomFaktor := 1; { wird z.Bsp. für höhere Auflösung auf 10 umgeschaltet }
  Zoom := 1/5.5; { als allgemeiner Skalierungsfaktor benutzt }
  FZoom2D := RelationZoom2D * Zoom;

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

  FViewPoint := vp3D;
  FColor := clEntspannt;
  FBogen := True;

{$ifdef Rotator}
  Rotator := TPolarKar2.Create;
  with Rotator do
  begin
    DeltaPhi := 0;
    DeltaTheta := -90;
    Xrot := -87;
  end;
{$endif}
end;

destructor TGetriebeGraph.Destroy;
begin
{$ifdef Rotator}
  Rotator.Free;
{$endif}
  inherited Destroy;
end;

procedure TGetriebeGraph.SetZoom(Value: single);
begin
  inherited SetZoom(Value);
  FZoom2D := RelationZoom2D * Zoom;
  FKoppelKurveNeedFill := True;
end;

{ mit ZoomFaktor kann in die 'höhere Auflösung' umgeschaltet werden }
procedure TGetriebeGraph.SetZoomFaktor(Value: Integer);
begin
  if Value <> FZoomFaktor then
  begin
    { Zoom und! Zoom2D werden verändert in SetZoom() }
    Zoom := Zoom/ZoomFaktor*Value;
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
  Updated := False;
end;

procedure TGetriebeGraph.SetKoppelKurve(const Kurve: TKoordLine);
begin
  FKoppelKurve := Kurve;
  FKoppelKurveNeedFill := True;
end;

procedure TGetriebeGraph.Update;
begin
  FillZug2D;
  if Assigned(Rotator) then
  begin
    inherited Update;
  end;
end;

procedure TGetriebeGraph.DrawToCanvas(Canvas: TCanvas);
begin
  if not GrafikOK then
    Exit;
  if not Updated then
    Update;
  with Canvas do
  begin
    case FViewPoint of
    vpSeite:
    begin
       if FKoppelBtnDown and Coloriert then
       begin
         { Koppelkurve }
         Pen.Color := clKoppelKurve;
         Polyline(Zug1Koppelkurve);
         { Kreisbogen mit Radius Vorstaglänge um C0 }
         Pen.Color := clBlack;
         Arc(MP.x-IntR, MP.y-IntR,
             MP.x+IntR, MP.y+IntR,
             MP.x     , MP.y-IntR,
             MP.x-IntR, MP.y-200*ZoomFaktor);
        end;
        { Rumpf }
        if Coloriert then
          Pen.Color := clRumpf
        else
          Pen.Color := FColor;
        PolyLine(Zug1Rumpf);
        { Saling }
        if Coloriert then
          Pen.Color := clSaling
        else
          Pen.Color := FColor;
        if (SalingTyp = stFest) or (SalingTyp = stDrehbar) then
          PolyLine(Zug1Saling);
        { Mast }
        if Coloriert then
        begin
          Pen.Color := clMast;
          if FBogen then
            PolyLine(Zug1MastKurve)
          else
            Polyline(Zug1Mast);
        end
        else
        begin
          Pen.Color := FColor;
          PolyLine(Zug1Mast);
        end;
        { Controller }
        if ControllerTyp <> ctOhne then
        begin
          Pen.Color := clController;
          Rectangle(Zug1Controller.Left,Zug1Controller.Top,
          Zug1Controller.Right,Zug1Controller.Bottom);
        end;
        { Wanten }
        if Coloriert then
          Pen.Color := clWanten
        else
          Pen.Color := FColor;
        PolyLine(Zug1Wanten);
        if FGestrichelt then
        begin
          Pen.Color := clBlue;
          Pen.Style := psDot;
          SetBkMode(Handle, TRANSPARENT);
          PolyLine(Zug1Wanten);
          Pen.Style := psSolid;
          SetBkMode(Handle, OPAQUE);
        end;
        { Vorstag }
        if Coloriert then
          Pen.Color := clWanten
        else
          Pen.Color := FColor;
        PolyLine(Zug1Vorstag);
      end;

      vpAchtern:
      begin
        { Rumpf }
        if Coloriert then
          Pen.Color := clRumpf
        else
          Pen.Color := FColor;
        PolyLine(Zug2Rumpf);
        { Mast }
        if Coloriert then
          Pen.Color := clMast
        else
          Pen.Color := FColor;
        PolyLine(Zug2Mast);
        { Salinge }
        if Coloriert then
          Pen.Color := clSaling
        else
          Pen.Color := FColor;
        if SalingTyp = stFest then
          PolyLine(Zug2Saling)
        else if SalingTyp = stDrehbar then
          PolyLine(Zug2SalingDS);
        { Wanten }
        if Coloriert then
          Pen.Color := clWanten
        else
          Pen.Color := FColor;
        PolyLine(Zug2Wanten);
        if FGestrichelt then
        begin
          Pen.Color := clBlue;
          Pen.Style := psDot;
          SetBkMode(Handle, TRANSPARENT);
          PolyLine(Zug2Wanten);
          Pen.Style := psSolid;
          SetBkMode(Handle, OPAQUE);
        end;
      end;

      vpTop:
      begin
        { Rumpf }
        if Coloriert then
          Pen.Color := clRumpf
        else
          Pen.Color := FColor;
        PolyLine(Zug3Rumpf);
        { Salinge }
        if Coloriert then
          Pen.Color := clSaling
        else
          Pen.Color := FColor;
        if SalingTyp = stFest then
          PolyLine(Zug3SalingFS);
        if SalingTyp = stDrehbar then
          PolyLine(Zug3SalingDS);
        { Mast }
        if Coloriert then
          Pen.Color := clMast
        else
          Pen.Color := FColor;
        PolyLine(Zug3Mast);
        { Wanten }
        if Coloriert then
          Pen.Color := clWanten
        else
          Pen.Color := FColor;
        PolyLine(Zug3Wanten);
        if FGestrichelt then
        begin
          Pen.Color := clBlue;
          Pen.Style := psDot;
          SetBkMode(Handle, TRANSPARENT);
          PolyLine(Zug3Wanten);
          Pen.Style := psSolid;
          SetBkMode(Handle, OPAQUE);
        end;
      end;

      vp3D:
      begin
        { FixPunkt }
        if Coloriert then
          Pen.Color := clYellow
        else
          Pen.Color := FColor;
        Ellipse(
          -TransKreisRadius,
          -TransKreisRadius,
          TransKreisRadius,
          TransKreisRadius);
        { Rumpf }
        if Coloriert then
          Pen.Color := clRumpf
        else
          Pen.Color := FColor;
        PolyLine(ZugRumpf);
        { Salinge }
        if Coloriert then
          Pen.Color := clSaling
        else
          Pen.Color := FColor;
        if SalingTyp = stFest then
          PolyLine(ZugSalingFS);
        if SalingTyp = stDrehbar then
          PolyLine(ZugSalingDS);
        { Mast }
        if Coloriert then
        begin
          Pen.Color := clMast;
          if FBogen then
            PolyLine(ZugMastKurve)
          else
            Polyline(ZugMast);
        end
        else
        begin
          Pen.Color := FColor;
          PolyLine(ZugMast);
        end;
        { Wanten }
        if Coloriert then
          Pen.Color := clWanten
        else
          Pen.Color := FColor;
        PolyLine(ZugWanteBb);
        PolyLine(ZugWanteStb);
        if FGestrichelt then
        begin
          Pen.Color := clBlue;
          Pen.Style := psDot;
          SetBkMode(Handle, TRANSPARENT);
          PolyLine(ZugWanteBb);
          PolyLine(ZugWanteStb);
          Pen.Style := psSolid;
          SetBkMode(Handle, OPAQUE);
        end;
        { Controller }
        if ControllerTyp <> ctOhne then
        begin
          if Coloriert then
            Pen.Color := clController
          else
            Pen.Color := FColor;
          PolyLine(ZugController);
        end;
        { Vorstag }
        if Coloriert then
          Pen.Color := clVorstag
        else
          Pen.Color := FColor;
        PolyLine(ZugVorstag);
      end;
    end;
  end;
end;

procedure TGetriebeGraph.DrawToMeta(Canvas: TMetaFileCanvas);
var
  SavedPenWidth: Integer;
begin
  if not GrafikOK then
    Exit;
  if not Updated then
    Update;
  with Canvas do
  begin
    case FViewPoint of
      vpSeite:
      begin
        { Koppelkurve und Kreisbogen mit Radius Vorstaglänge um C0 }
        if FKoppelBtnDown and Coloriert then
        begin
          SavedPenWidth := Pen.Width;
          Pen.Width := 1;
          Pen.Color := clBlack;
          Polyline(Zug1Koppelkurve);
          Arc(MP.x-IntR, MP.y-IntR,
              MP.x+IntR, MP.y+IntR,
              MP.x     , MP.y-IntR,
              MP.x-IntR, MP.y-200*ZoomFaktor);
          Pen.Width := SavedPenWidth;
        end;
        { Rumpf }
        if Coloriert then
          Pen.Color := clRumpf
        else
          Pen.Color := FColor;
        PolyLine(Zug1Rumpf);
        { Saling }
        if Coloriert then
          Pen.Color := clSaling
        else
          Pen.Color := FColor;
        if (SalingTyp = stFest) or (SalingTyp = stDrehbar) then
          PolyLine(Zug1Saling);
        { Mast }
        if Coloriert then
        begin
          Pen.Color := clMast;
          if FBogen then
            PolyLine(Zug1MastKurve)
          else
            Polyline(Zug1Mast); end
        else
        begin
          Pen.Color := FColor;
          PolyLine(Zug1Mast);
        end;
        { Controller }
        if ControllerTyp <> ctOhne then
        begin
          Pen.Color := clController;
          Rectangle(Zug1Controller.Left,Zug1Controller.Top,
          Zug1Controller.Right,Zug1Controller.Bottom);
        end;
        { Wanten }
        if Coloriert then
          Pen.Color := clWanten
        else
          Pen.Color := FColor;
        PolyLine(Zug1Wanten);
        if FGestrichelt then
        begin
          Pen.Color := clBlue;
          Pen.Style := psDashDot;
          SetBkMode(Handle, TRANSPARENT);
          PolyLine(Zug1Wanten);
          Pen.Style := psSolid;
          SetBkMode(Handle, OPAQUE);
        end;
        { Vorstag }
        if Coloriert then
          Pen.Color := clWanten
        else
          Pen.Color := FColor;
        PolyLine(Zug1Vorstag);
      end;

      vpAchtern:
      begin
        { Rumpf }
        if Coloriert then
          Pen.Color := clRumpf
        else
          Pen.Color := FColor;
        PolyLine(Zug2Rumpf);
        { Mast }
        if Coloriert then
          Pen.Color := clMast
        else
          Pen.Color := FColor;
        PolyLine(Zug2Mast);
        { Salinge }
        if Coloriert then
          Pen.Color := clSaling
        else
          Pen.Color := FColor;
        if SalingTyp = stFest then
          PolyLine(Zug2Saling)
        else if SalingTyp = stDrehbar then
          PolyLine(Zug2SalingDS);
        { Wanten }
        if Coloriert then
          Pen.Color := clWanten
        else
          Pen.Color := FColor;
        PolyLine(Zug2Wanten);
        if FGestrichelt then
        begin
          Pen.Color := clBlue;
          Pen.Style := psDashDot;
          SetBkMode(Handle, TRANSPARENT);
          PolyLine(Zug2Wanten);
          Pen.Style := psSolid;
          SetBkMode(Handle, OPAQUE);
        end;
      end;

      vpTop:
      begin
        { Rumpf }
        if Coloriert then
          Pen.Color := clRumpf
        else
          Pen.Color := FColor;
        PolyLine(Zug3Rumpf);
        { Salinge }
        if Coloriert then
          Pen.Color := clSaling
        else
          Pen.Color := FColor;
        if SalingTyp = stFest then
          PolyLine(Zug3SalingFS);
        if SalingTyp = stDrehbar then
          PolyLine(Zug3SalingDS);
        { Mast }
        if Coloriert then
          Pen.Color := clMast
        else
          Pen.Color := FColor;
        PolyLine(Zug3Mast);
        { Wanten }
        if Coloriert then
          Pen.Color := clWanten
        else
          Pen.Color := FColor;
        PolyLine(Zug3Wanten);
        if FGestrichelt then
        begin
          Pen.Color := clBlue;
          Pen.Style := psDashDot;
          SetBkMode(Handle, TRANSPARENT);
          PolyLine(Zug3Wanten);
          Pen.Style := psSolid;
          SetBkMode(Handle, OPAQUE);
        end;
      end;

      vp3D:
      begin
        { FixPunkt }
          // if Coloriert then
          //   Pen.Color := clYellow;
          // Ellipse(
          //     Offset.x - TransKreisRadius,
          //     Offset.y - TransKreisRadius,
          //     Offset.x + TransKreisRadius,
          //     Offset.y + TransKreisRadius);
        { Rumpf }
        if Coloriert then
          Pen.Color := clRumpf
        else
          Pen.Color := FColor;
        PolyLine(ZugRumpf);
        { Salinge }
        if Coloriert then
          Pen.Color := clSaling
        else
          Pen.Color := FColor;
        if SalingTyp = stFest then
          PolyLine(ZugSalingFS);
        if SalingTyp = stDrehbar then
          PolyLine(ZugSalingDS);
        { Mast }
        if Coloriert then
        begin
          Pen.Color := clMast;
          if FBogen then
            PolyLine(ZugMastKurve)
          else
            Polyline(ZugMast);
        end
        else
        begin
          Pen.Color := FColor;
          PolyLine(ZugMast);
        end;
        { Wanten }
        if Coloriert then
          Pen.Color := clWanten
        else
          Pen.Color := FColor;
        PolyLine(ZugWanteBb);
        PolyLine(ZugWanteStb);
        if FGestrichelt then
        begin
          Pen.Color := clBlue;
          Pen.Style := psDashDot;
          SetBkMode(Handle, TRANSPARENT);
          PolyLine(ZugWanteBb); PolyLine(ZugWanteStb);
          Pen.Style := psSolid;
          SetBkMode(Handle, OPAQUE);
        end;
        { Controller }
        if ControllerTyp <> ctOhne then
        begin
          if Coloriert then
            Pen.Color := clController
          else
            Pen.Color := FColor;
          PolyLine(ZugController);
        end;
        { Vorstag }
        if Coloriert then
          Pen.Color := clVorstag
        else
          Pen.Color := FColor;
        PolyLine(ZugVorstag);
      end;
    end;
  end;
end;

procedure TGetriebeGraph.FillZug2D;
var
  i, j: Integer;
  FixPunkt2D: TRealPoint;
  { temporäre Koordinaten double transformed }
  Temp: TRealPoint;
//  A0, B0, C0, D0, E0, F0: TRealPoint;
//  A,  B,  C,  D,  E,  F:  TRealPoint;
  { temporäre Koordinaten Integer transformed }
  xTemp, zTemp: Integer;
begin
  // 1. Koordinate verschieben mit Vektor '-FixPunkt'
  // 2. Skalieren
  // 3. verschieben um Offset
  // 4. Bildschirmkoordinaten entnehmen

  { Fixpunkt enthält nach Aufruf von FillZug3D den gedrehten Fixpunkt, hier wird
    aber immer der nicht gedrehte Fixpunkt benötigt! }
  FixPunkt2D := rP[FixPoint];

  { Koppelkurve }
  if FKoppelKurveNeedFill then
  begin
    for j := 0 to 100 do
    begin
      Temp := vsub(FKoppelKurve[j], FixPunkt2D);
      xTemp := Round(Temp[x] * FZoom2D);
      zTemp := Round(Temp[z] * FZoom2D);
      Zug1KoppelKurve[j+1].X :=  xTemp + OffsetX1;
      Zug1KoppelKurve[j+1].Y := -zTemp + OffsetY1;
    end;
    FKoppelKurveNeedFill := False;
  end;

  { Mastkurve }
  for j := 0 to BogenMax do
  begin
    Temp := vsub(Kurve[j], FixPunkt2D);
    xTemp := Round(Temp[x] * FZoom2D);
    zTemp := Round(Temp[z] * FZoom2D);
    Zug1MastKurve[j+1].X :=  xTemp + OffsetX1;
    Zug1MastKurve[j+1].Y := -zTemp + OffsetY1;
  end;

  { Radius für Kreisbogen um C0 }
  IntR := Round( Abstand(rP[ooC],rP[ooC0]) * FZoom2D );

  A0 := vsub(rP[ooA0],FixPunkt2D);
  B0 := vsub(rP[ooB0],FixPunkt2D);
  C0 := vsub(rP[ooC0],FixPunkt2D);
  D0 := vsub(rP[ooD0],FixPunkt2D);
  E0 := vsub(rP[ooE0],FixPunkt2D);
  F0 := vsub(rP[ooF0],FixPunkt2D);
  A := vsub(rP[ooA],FixPunkt2D);
  B := vsub(rP[ooB],FixPunkt2D);
  C := vsub(rP[ooC],FixPunkt2D);
  D := vsub(rP[ooD],FixPunkt2D);
  E := vsub(rP[ooE],FixPunkt2D);
  F := vsub(rP[ooF],FixPunkt2D);

  xA0 := Round(A0[x]*FZoom2D);
  yA0 := Round(A0[y]*FZoom2D);
  zA0 := Round(A0[z]*FZoom2D);

  xB0 := xA0;
  yB0 := -yA0;
  zB0 := zA0;

  xC0 := Round(C0[x]*FZoom2D);
  yC0 := 0;
  zC0 := Round(C0[z]*FZoom2D);

  MP.x := xC0 + OffsetX1;
  MP.y := -zC0 + OffsetY1;

  xD0 := Round(D0[x]*FZoom2D);
  yD0 := 0;
  zD0 := Round(D0[z]*FZoom2D);

  xE0 := Round(E0[x]*FZoom2D);
  yE0 := 0;
  zE0 := Round(E0[z]*FZoom2D);

  xF0 := Round(F0[x]*FZoom2D);
  yF0 := 0;
  zF0 := Round(F0[z]*FZoom2D);

  xA := Round( A[x]*FZoom2D);
  yA := Round( A[y]*FZoom2D);
  zA := Round( A[z]*FZoom2D);

  xB := xA;
  yB := -yA;
  zB := zA;

  xC := Round(C[x]*FZoom2D);
  yC := 0;
  zC := Round(C[z]*FZoom2D);

  xD := Round(D[x]*FZoom2D);
  yD := 0;
  zD := Round(D[z]*FZoom2D);

  xE := Round(E[x]*FZoom2D);
  yE := 0;
  zE := Round(E[z]*FZoom2D);

  xF := Round(F[x]*FZoom2D);
  yF := 0;
  zF := Round(F[z]*FZoom2D);

  { Rumpf von der Seite }
  Zug1Rumpf[ 1].X :=  xA0 + OffsetX1;
  Zug1Rumpf[ 1].Y := -zA0 + OffsetY1;
  Zug1Rumpf[ 2].X :=  xD0 + OffsetX1;
  Zug1Rumpf[ 2].Y := -zD0 + OffsetY1;
  Zug1Rumpf[ 3].X :=  xC0 + OffsetX1;
  Zug1Rumpf[ 3].Y := -zC0 + OffsetY1;
  Zug1Rumpf[ 4].X :=  xA0 + OffsetX1;
  Zug1Rumpf[ 4].Y := -zA0 + OffsetY1;

  { Mast von der Seite }
  Zug1Mast[ 1].X :=  xD0 + OffsetX1;
  Zug1Mast[ 1].Y := -zD0 + OffsetY1;
  Zug1Mast[ 2].X :=  xD + OffsetX1;
  Zug1Mast[ 2].Y := -zD + OffsetY1;
  Zug1Mast[ 3].X :=  xC + OffsetX1;
  Zug1Mast[ 3].Y := -zC + OffsetY1;
  Zug1Mast[ 4].X :=  xF + OffsetX1;
  Zug1Mast[ 4].Y := -zF + OffsetY1;

  Zug1MastKurve[BogenMax+2].X :=  xF + OffsetX1;
  Zug1MastKurve[BogenMax+2].Y := -zF + OffsetY1;

  { Wanten von der Seite }
  Zug1Wanten[ 1].X :=  xA0 + OffsetX1;
  Zug1Wanten[ 1].Y := -zA0 + OffsetY1;
  Zug1Wanten[ 2].X :=  xA + OffsetX1;
  Zug1Wanten[ 2].Y := -zA + OffsetY1;
  Zug1Wanten[ 3].X :=  xC + OffsetX1;
  Zug1Wanten[ 3].Y := -zC + OffsetY1;

  {Vorstag von der Seite}
  Zug1Vorstag[ 1].X :=  xC + OffsetX1;
  Zug1Vorstag[ 1].Y := -zC + OffsetY1;
  Zug1Vorstag[ 2].X :=  xC0 + OffsetX1;
  Zug1Vorstag[ 2].Y := -zC0 + OffsetY1;

  { Saling von der Seite }
  Zug1Saling[1].X :=  xA + OffsetX1;
  Zug1Saling[1].Y := -zA + OffsetY1;
  Zug1Saling[2].X :=  xD + OffsetX1;
  Zug1Saling[2].Y := -zD + OffsetY1;

  { Controller von der Seite }
  with Zug1Controller do
  begin
    Left :=  xE + OffsetX1;
    Top  := -zE - 2*ZoomFaktor + OffsetY1;
    Right :=  xE0 + OffsetX1;
    Bottom := -zE0 + 2*ZoomFaktor + OffsetY1;
  end;

  { Rumpf von hinten }
  Zug2Rumpf[1].X :=  yA0 + OffsetX2;
  Zug2Rumpf[1].Y := -zA0 + OffsetY2;
  Zug2Rumpf[2].X :=  yC0 + OffsetX2;
  Zug2Rumpf[2].Y := -zC0 + OffsetY2;
  Zug2Rumpf[3].X :=  yB0 + OffsetX2;
  Zug2Rumpf[3].Y := -zB0 + OffsetY2;
  Zug2Rumpf[4].X :=  yA0 + OffsetX2;
  Zug2Rumpf[4].Y := -zA0 + OffsetY2;
  Zug2Rumpf[5].X :=  yD0 + OffsetX2;
  Zug2Rumpf[5].Y := -zD0 + OffsetY2;
  Zug2Rumpf[6].X :=  yB0 + OffsetX2;
  Zug2Rumpf[6].Y := -zB0 + OffsetY2;

  { Wanten von hinten }
  Zug2Wanten[1].X :=  yB0 + OffsetX2;
  Zug2Wanten[1].Y := -zB0 + OffsetY2;
  Zug2Wanten[2].X :=  yB + OffsetX2;
  Zug2Wanten[2].Y := -zB + OffsetY2;
  Zug2Wanten[3].X :=  yC + OffsetX2;
  Zug2Wanten[3].Y := -zC + OffsetY2;
  Zug2Wanten[4].X :=  yA + OffsetX2;
  Zug2Wanten[4].Y := -zA + OffsetY2;
  Zug2Wanten[5].X :=  yA0 + OffsetX2;
  Zug2Wanten[5].Y := -zA0 + OffsetY2;

  { Salingdreieck von hinten }
  Zug2Saling[1].X :=  yB + OffsetX2;
  Zug2Saling[1].Y := -zB + OffsetY2;
  Zug2Saling[2].X :=  yA + OffsetX2;
  Zug2Saling[2].Y := -zA + OffsetY2;
  Zug2Saling[3].X :=  yD + OffsetX2;
  Zug2Saling[3].Y := -zD + OffsetY2;
  Zug2Saling[4].X :=  yB + OffsetX2;
  Zug2Saling[4].Y := -zB + OffsetY2;

  for i := 1 to 3 do
  begin
    Zug2SalingDS[i].X := Zug2Saling[i+1].X;
    Zug2SalingDS[i].Y := Zug2Saling[i+1].Y;
  end;

  { Mast von hinten }
  Zug2Mast[1].X :=  yD0 + OffsetX2;
  Zug2Mast[1].Y := -zD0 + OffsetY2;
  Zug2Mast[2].X :=  yF + OffsetX2;
  Zug2Mast[2].Y := -zF + OffsetY2;
  
  { Wanten in Draufsicht - rot }
  Zug3Wanten[ 1].X :=  yB0 + OffsetX3;
  Zug3Wanten[ 1].Y := -xB0 + OffsetY3;
  Zug3Wanten[ 2].X :=  yB + OffsetX3;
  Zug3Wanten[ 2].Y := -xB + OffsetY3;
  Zug3Wanten[ 3].X :=  yC + OffsetX3;
  Zug3Wanten[ 3].Y := -xC + OffsetY3;
  Zug3Wanten[ 4].X :=  yA + OffsetX3;
  Zug3Wanten[ 4].Y := -xA + OffsetY3;
  Zug3Wanten[ 5].X :=  yA0 + OffsetX3;
  Zug3Wanten[ 5].Y := -xA0 + OffsetY3;

  { Rumpftetraeder in Draufsicht - grün }
  Zug3Rumpf[1].X :=  yA0 + OffsetX3;
  Zug3Rumpf[1].Y := -xA0 + OffsetY3;
  Zug3Rumpf[2].X :=  yB0 + OffsetX3;
  Zug3Rumpf[2].Y := -xB0 + OffsetY3;
  Zug3Rumpf[3].X :=  yC0 + OffsetX3;
  Zug3Rumpf[3].Y := -xC0 + OffsetY3;
  Zug3Rumpf[4].X :=  yA0 + OffsetX3;
  Zug3Rumpf[4].Y := -xA0 + OffsetY3;
  Zug3Rumpf[5].X :=  yD0 + OffsetX3;
  Zug3Rumpf[5].Y := -xD0 + OffsetY3;
  Zug3Rumpf[6].X :=  yC0 + OffsetX3;
  Zug3Rumpf[6].Y := -xC0 + OffsetY3;
  Zug3Rumpf[7].X :=  yD0 + OffsetX3;
  Zug3Rumpf[7].Y := -xD0 + OffsetY3;
  Zug3Rumpf[8].X :=  yB0 + OffsetX3;
  Zug3Rumpf[8].Y := -xB0 + OffsetY3;
  Zug3Rumpf[9].X :=  yF0 + OffsetX3;
  Zug3Rumpf[9].Y := -xF0 + OffsetY3;
  Zug3Rumpf[10].X :=  yA0 + OffsetX3;
  Zug3Rumpf[10].Y := -xA0 + OffsetY3;

  { Salingdreieck in Draufsicht - lime, für stFest }
  Zug3SalingFS[1].X :=  yB + OffsetX3;
  Zug3SalingFS[1].Y := -xB + OffsetY3;
  Zug3SalingFS[2].X :=  yA + OffsetX3;
  Zug3SalingFS[2].Y := -xA + OffsetY3;
  Zug3SalingFS[3].X :=  yD + OffsetX3;
  Zug3SalingFS[3].Y := -xD + OffsetY3;
  Zug3SalingFS[4].X :=  yB + OffsetX3;
  Zug3SalingFS[4].Y := -xB + OffsetY3;

  { Salingdreieck in Draufsicht - lime, für stDrehbar }
  Zug3SalingDS[1].X :=  yA + OffsetX3;
  Zug3SalingDS[1].Y := -xA + OffsetY3;
  Zug3SalingDS[2].X :=  yD + OffsetX3;
  Zug3SalingDS[2].Y := -xD + OffsetY3;
  Zug3SalingDS[3].X :=  yB + OffsetX3;
  Zug3SalingDS[3].Y := -xB + OffsetY3;

  { Mast in Draufsicht - blau }
  Zug3Mast[ 1].X :=  yD0 + OffsetX3;
  Zug3Mast[ 1].Y := -xD0 + OffsetY3;
  Zug3Mast[ 2].X :=  yD + OffsetX3;
  Zug3Mast[ 2].Y := -xD + OffsetY3;
  Zug3Mast[ 3].X :=  yC + OffsetX3;
  Zug3Mast[ 3].Y := -xC + OffsetY3;
  Zug3Mast[ 4].X :=  yF + OffsetX3;
  Zug3Mast[ 4].Y := -xF + OffsetY3;
end;

procedure TGetriebeGraph.FillZug3D;
var
  j: Integer;
begin
  { Rumpf 3D }
  ZugRumpf[0].x := xA0 + OffsetX4;
  ZugRumpf[0].y := -yA0 + OffsetY4;
  ZugRumpf[1].x := xB0 + OffsetX4;
  ZugRumpf[1].y := -yB0 + OffsetY4;
  ZugRumpf[2].x := xC0 + OffsetX4;
  ZugRumpf[2].y := -yC0 + OffsetY4;
  ZugRumpf[3].x := xA0 + OffsetX4;
  ZugRumpf[3].y := -yA0 + OffsetY4;

  ZugRumpf[4].x := xD0 + OffsetX4;
  ZugRumpf[4].y := -yD0 + OffsetY4;
  ZugRumpf[5].x := xB0 + OffsetX4;
  ZugRumpf[5].y := -yB0 + OffsetY4;
  ZugRumpf[6].x := xC0 + OffsetX4;
  ZugRumpf[6].y := -yC0 + OffsetY4;
  ZugRumpf[7].x := xD0 + OffsetX4;
  ZugRumpf[7].y := -yD0 + OffsetY4;

  { Mast 3D }
  ZugMast[0].x := xD0 + OffsetX4;
  ZugMast[0].y := -yD0 + OffsetY4;
  ZugMast[1].x := xD + OffsetX4;
  ZugMast[1].y := -yD + OffsetY4;
  ZugMast[2].x := xC + OffsetX4;
  ZugMast[2].y := -yC + OffsetY4;
  ZugMast[3].x := xF + OffsetX4;
  ZugMast[3].y := -yF + OffsetY4;

  { WanteStb 3D }
  ZugWanteStb[0].x := xA0 + OffsetX4;
  ZugWanteStb[0].y := -yA0 + OffsetY4;
  ZugWanteStb[1].x := xA + OffsetX4;
  ZugWanteStb[1].y := -yA + OffsetY4;
  ZugWanteStb[2].x := xC + OffsetX4;
  ZugWanteStb[2].y := -yC + OffsetY4;

  { WanteBb 3D }
  ZugWanteBb[0].x := xB0 + OffsetX4;
  ZugWanteBb[0].y := -yB0 + OffsetY4;
  ZugWanteBb[1].x := xB + OffsetX4;
  ZugWanteBb[1].y := -yB + OffsetY4;
  ZugWanteBb[2].x := xC + OffsetX4;
  ZugWanteBb[2].y := -yC + OffsetY4;

  { SalingFS 3D }
  ZugSalingFS[0].x := xA + OffsetX4;
  ZugSalingFS[0].y := -yA + OffsetY4;
  ZugSalingFS[1].x := xD + OffsetX4;
  ZugSalingFS[1].y := -yD + OffsetY4;
  ZugSalingFS[2].x := xB + OffsetX4;
  ZugSalingFS[2].y := -yB + OffsetY4;
  ZugSalingFS[3].x := xA + OffsetX4;
  ZugSalingFS[3].y := -yA + OffsetY4;

  { SalingDS 3D }
  ZugSalingDS[0].x := xA + OffsetX4;
  ZugSalingDS[0].y := -yA + OffsetY4;
  ZugSalingDS[1].x := xD + OffsetX4;
  ZugSalingDS[1].y := -yD + OffsetY4;
  ZugSalingDS[2].x := xB + OffsetX4;
  ZugSalingDS[2].y := -yB + OffsetY4;

  { Controller 3D }
  ZugController[0].x := xE0 + OffsetX4;
  ZugController[0].y := -yE0 + OffsetY4;
  ZugController[1].x := xE + OffsetX4;
  ZugController[1].y := -yE + OffsetY4;

  { Vorstag 3D }
  ZugVorstag[0].x := xC0 + OffsetX4;
  ZugVorstag[0].y := -yC0 + OffsetY4;
  ZugVorstag[1].x := xC + OffsetX4;
  ZugVorstag[1].y := -yC + OffsetY4;

  { MastKurve 3D }
  for j := 0 to BogenMax do
  begin
    ZugMastKurve[j].x := ZugMastKurve[j].x + OffsetX4;
    ZugMastKurve[j].y := ZugMastKurve[j].y + OffsetY4;
  end;
  ZugMastKurve[BogenMax + 1].x := xF + OffsetX4;
  ZugMastKurve[BogenMax + 1].y := -yF + OffsetY4;

//  BogenIndexD := FindBogenIndexOf(rP[ooD]);
//  ZugMastKurveD0D := Copy(ZugMastKurve, 0, BogenIndexD + 1);
//  ZugMastKurveDC := Copy(ZugMastKurve, BogenIndexD, Length(ZugMastKurve)-1);
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
  xmin := Round(Temp[x]); xmax := xmin;
  ymin := Round(Temp[y]); ymax := ymin;
  zmin := Round(Temp[z]); zmax := zmin;
  for k := ooA0 to ooF do
  begin
    if k = ooF0 then
      Continue;
    Temp := rP[k];
    tempx := Round(Temp[x]);
    tempy := Round(Temp[y]);
    tempz := Round(Temp[z]);
    if tempx < xmin then xmin := tempx;
    if tempy < ymin then ymin := tempy;
    if tempz < zmin then zmin := tempz;
    if tempx > xmax then xmax := tempx;
    if tempy > ymax then ymax := tempy;
    if tempz > zmax then zmax := tempz;
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
