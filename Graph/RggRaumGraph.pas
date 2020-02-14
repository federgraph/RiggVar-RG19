unit RggRaumGraph;

interface

uses
  System.Types,
  System.SysUtils,
  System.Classes,
  System.UITypes,
  System.UIConsts,
  Vcl.Graphics,
  RggCalc,
  RggTypes,
  RggDisplay,
  RggZug,
  RggBootGraph;

type
  TRaumGraph = class(TBootGraph)
  protected
    { transformed coordinates of Rigg }
    A0, B0, C0, D0, E0, F0: TRealPoint;
    A,  B,  C,  D,  E,  F:  TRealPoint;
  protected
    Zug3D: TZug3D;
    Zug4: TZug4;
  protected
    procedure UpdateZugProps;
    procedure UpdateFixPunkt;
    procedure Update1;
  public
    DL: TRggDisplayList;

    WantFixPunkt: Boolean;
    WantRumpf: Boolean;
    WantSaling: Boolean;
    WantController: Boolean;
    WantWante: Boolean;
    WantMast: Boolean;
    WantVorstag: Boolean;

    constructor Create;
    destructor Destroy; override;

    procedure Update; override;
    procedure UpdateDisplayList;
    procedure DrawToCanvas(g: TCanvas); override;
    procedure Draw;

    procedure GetPlotList(ML: TStrings); override;
    procedure ToggleRenderOption(const fa: Integer);
    function QueryRenderOption(const fa: Integer): Boolean;
  end;

implementation

uses
  RiggVar.RG.Def;

constructor TRaumGraph.Create;
begin
  inherited;

  WantFixPunkt := True;
  WantRumpf := True;
  WantSaling := True;
  WantController := True;
  WantWante := True;
  WantMast := True;
  WantVorstag := True;

  Zug3D := TZug3D.Create;
  Zug3D.Data := RaumGraphData;
  Zug3D.Props := RaumGraphProps;

  Zug4 := TZug4.Create;
  Zug4.Data := RaumGraphData;
  Zug4.Props := RaumGraphProps;

  DL := TRggDisplayList.Create;
end;

destructor TRaumGraph.Destroy;
begin
  Zug3D.Free;
  Zug4.Free;
  DL.Free;
  inherited;
end;

procedure TRaumGraph.UpdateFixPunkt;
var
  fp: TRealPoint;
begin
  { FixPunkt is the rotated FixPoint, not scaled or translated }
  fp := rP[FixPoint];
  FixPunkt := Rotator.Rotiere(fp);
end;

procedure TRaumGraph.Update;
begin
  Update1;
  UpdateZugProps;
  Zug3D.FillZug; // needs updated Props (BogenIndex)
  Updated := True;
end;

procedure TRaumGraph.Update1;
var
  KurveRotiert: array [0 .. BogenMax] of TRealPoint;
  KoordRotiert: TRealRiggPoints;
  i: TRiggPoint;
  j: Integer;
begin
  { Graph drehen }
  if Assigned(Rotator) then
  begin
    for i := ooA0 to ooF0 do
      KoordRotiert[i] := Rotator.Rotiere(rP[i]);
    for i := ooA to ooF do
      KoordRotiert[i] := Rotator.Rotiere(rP[i]);
    for j := 0 to BogenMax do
      KurveRotiert[j] := Rotator.Rotiere(Kurve[j]);
  end;

  UpdateFixPunkt; // gedrehter FixPunkt

  A0 := vsub(KoordRotiert[ooA0], FixPunkt);
  B0 := vsub(KoordRotiert[ooB0], FixPunkt);
  C0 := vsub(KoordRotiert[ooC0], FixPunkt);
  D0 := vsub(KoordRotiert[ooD0], FixPunkt);
  E0 := vsub(KoordRotiert[ooE0], FixPunkt);
  F0 := vsub(KoordRotiert[ooF0], FixPunkt);
  A := vsub(KoordRotiert[ooA],FixPunkt);
  B := vsub(KoordRotiert[ooB], FixPunkt);
  C := vsub(KoordRotiert[ooC],FixPunkt);
  D := vsub(KoordRotiert[ooD], FixPunkt);
  E := vsub(KoordRotiert[ooE], FixPunkt);
  F := vsub(KoordRotiert[ooF],FixPunkt);

  for j := 0 to BogenMax do
    KurveRotiert[j] := vsub(KurveRotiert[j], FixPunkt);

  with RaumGraphData do
  begin
    for j := 0 to BogenMax do
    begin
      xA0 := Round(KurveRotiert[j, x] * Zoom);
      yA0 := Round(KurveRotiert[j, z] * Zoom);
      Zug3D.ZugMastKurve[j].x := xA0;
      Zug3D.ZugMastKurve[j].y := -yA0;
      Zug4.ZugMastKurve[j].x := xA0;
      Zug4.ZugMastKurve[j].y := -yA0;
    end;

    xA0 := Round(A0[x] * Zoom);
    yA0 := Round(A0[z] * Zoom);
    xB0 := Round(B0[x] * Zoom);
    yB0 := Round(B0[z] * Zoom);
    xC0 := Round(C0[x] * Zoom);
    yC0 := Round(C0[z] * Zoom);
    xD0 := Round(D0[x] * Zoom);
    yD0 := Round(D0[z] * Zoom);
    xE0 := Round(E0[x] * Zoom);
    yE0 := Round(E0[z] * Zoom);
    xF0 := Round(F0[x] * Zoom);
    yF0 := Round(F0[z] * Zoom);

    xA := Round(A[x] * Zoom);
    yA := Round(A[z] * Zoom);
    xB := Round(B[x] * Zoom);
    yB := Round(B[z] * Zoom);
    xC := Round(C[x] * Zoom);
    yC := Round(C[z] * Zoom);
    xD := Round(D[x] * Zoom);
    yD := Round(D[z] * Zoom);
    xE := Round(E[x] * Zoom);
    yE := Round(E[z] * Zoom);
    xF := Round(F[x] * Zoom);
    yF := Round(F[z] * Zoom);
  end;
end;

procedure TRaumGraph.DrawToCanvas(g: TCanvas);
begin
  if not GrafikOK then
    Exit;

  if not Updated then
      Update;

  Zug3D.DrawToCanvas(g);
end;

procedure TRaumGraph.GetPlotList(ML: TStrings);
//var
//  SavedZoom: double;
begin
//  SavedZoom := Zoom;
//  Zoom := 10;
  if not GrafikOK then
    Exit;
  if not Updated then
      Update;

  Zug3D.GetPlotList(ML);
end;

function TRaumGraph.QueryRenderOption(const fa: Integer): Boolean;
begin
  result := False;
end;

procedure TRaumGraph.UpdateZugProps;
var
  cr: TRaumGraphProps;
begin
  BogenIndexD := FindBogenIndexOf(rP[ooD]);

  cr := RaumGraphProps;

  cr.BogenIndexD := BogenIndexD;
  cr.Bogen := Bogen;

  cr.Koppel := Koppel;
  cr.Gestrichelt := WanteGestrichelt;

  cr.SalingTyp := SalingTyp;
  cr.ControllerTyp := ControllerTyp;

  cr.Coloriert := Coloriert;
  cr.Color := Color;
end;

procedure TRaumGraph.Draw;
begin

end;

procedure TRaumGraph.ToggleRenderOption(const fa: Integer);
begin

end;

procedure TRaumGraph.UpdateDisplayList;
var
  DI: TDisplayItem;
begin
  DL.Clear;
  DI := DL.DI;

  with Zug3D do
  begin

    if WantFixpunkt then
    begin
      DI.StrokeColor := clYellow;
      DI.StrokeWidth := 1;
      DL.Ellipse(FixPunkt, FixPunkt, Point(0, 0), TransKreisRadius);
    end;

    { Rumpf }
    if WantRumpf then
    begin
      DI.StrokeColor := clSilver;
      DI.StrokeWidth := 3;
      DL.Line(A0, B0, ZugRumpf[0], ZugRumpf[1], clRed);
      DL.Line(B0, C0, ZugRumpf[1], ZugRumpf[2], clGreen);
      DL.Line(A0, C0, ZugRumpf[2], ZugRumpf[3], clBlue);

      DL.Line(D0, A0, ZugRumpf[0], ZugRumpf[4], clAqua);
      DL.Line(D0, B0, ZugRumpf[1], ZugRumpf[4], clFuchsia);
      DL.Line(D0, C0, ZugRumpf[2], ZugRumpf[4], TColors.Orange);
    end;

    { Mast }
    if WantMast then
    begin
      DI.StrokeColor := TColors.Cornflowerblue;
      DI.StrokeWidth := 5;
      if Bogen then
      begin
        DL.PolyLine(D0, D, ZugMastKurveD0D, TColors.Cornflowerblue);
        DL.PolyLine(D, C, ZugMastKurveDC, TColors.Blue);
        DL.Line(C, F, ZugMast[2], ZugMast[3], TColors.Navy);
      end
      else
      begin
        DL.Line(D0, D, ZugMast[0], ZugMast[1], TColors.Cornflowerblue);
        DL.Line(D, C, ZugMast[1], ZugMast[2], TColors.Cornflowerblue);
        DL.Line(C, F, ZugMast[2], ZugMast[3], TColors.Cornflowerblue);
      end;
    end;

    { Wanten }
    if WantWante then
    begin
      { Wante Stb }
      DI.StrokeColor := clRed;
      DI.StrokeWidth := 2;
      DL.Line(A0, A, ZugWanteStb[0], ZugWanteStb[1], clRed);
      DL.Line(A, C, ZugWanteStb[1], ZugWanteStb[2], clRed);

      { Wante Bb }
      DI.StrokeColor := clGreen;
      DI.StrokeWidth := 2;
      DL.Line(B0, B, ZugWanteBb[0], ZugWanteBb[1], clGreen);
      DL.Line(B, C, ZugWanteBb[1], ZugWanteBb[2], clGreen);
    end;

    { Saling }
    if WantSaling then
    begin
      DI.StrokeColor := clLime;
      DI.StrokeWidth := 6;
      if SalingTyp = stFest then
      begin
        DL.Line(A, D, ZugSalingFS[0], ZugSalingFS[1], clLime);
        DL.Line(B, D, ZugSalingFS[2], ZugSalingFS[1], clLime);
        DL.Line(A, B, ZugSalingFS[0], ZugSalingFS[2], clLime);
      end;
      if SalingTyp = stDrehbar then
      begin
        DI.StrokeColor := clLime;
        DI.StrokeWidth := 2;
        DL.Line(A, D, ZugSalingDS[0], ZugSalingDS[1], clLime);
        DL.Line(B, D, ZugSalingDS[2], ZugSalingDS[1], clLime);
      end;
    end;

    { Controller }
    if WantController then
    begin
    if ControllerTyp <> ctOhne then
    begin
        DI.StrokeColor := clAqua;
        DI.StrokeWidth := 4;
        DL.Line(E0, E, ZugController[0], ZugController[1], clAqua);
      end;
    end;

    { Vorstag }
    if WantVorstag then
    begin
      DI.StrokeColor := clYellow;
      DI.StrokeWidth := 4;
      DL.Line(C0, C, ZugVorstag[0], ZugVorstag[1], clYellow);
    end;

  end;
end;

end.
