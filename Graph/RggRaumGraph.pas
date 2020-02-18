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
    { original definition of Achsen }
    AchseN: TRealPoint;
    AchseX: TRealPoint;
    AchseY: TRealPoint;
    AchseZ: TRealPoint;

    { transformed coordinates Achsen }
    AchseNT: TRealPoint;
    AchseXT: TRealPoint;
    AchseYT: TRealPoint;
    AchseZT: TRealPoint;

    { transformed coordinates of Rigg }
    A0, B0, C0, D0, E0, F0: TRealPoint;
    A,  B,  C,  D,  E,  F:  TRealPoint;
  protected
    Zug3D: TZug3D;
    Zug4: TZug4;
  private
    function GetFixPunkt: TRealPoint;
  protected
    procedure UpdateZugProps;
    procedure Update2;
  public
    DL: TRggDisplayList;

    WantFixPunkt: Boolean;
    WantRumpf: Boolean;
    WantSaling: Boolean;
    WantController: Boolean;
    WantWante: Boolean;
    WantMast: Boolean;
    WantVorstag: Boolean;
    WantAchsen: Boolean;

    constructor Create;
    destructor Destroy; override;

    procedure Update; override;
    procedure UpdateDisplayList;
    procedure DrawToCanvas(g: TCanvas); override;

    procedure GetPlotList(ML: TStrings); override;
    property FixPunkt: TRealPoint read GetFixPunkt;
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
  WantAchsen := False;

  Zug3D := TZug3D.Create;
  Zug3D.Data := RaumGraphData;
  Zug3D.Props := RaumGraphProps;

  Zug4 := TZug4.Create;
  Zug4.Data := RaumGraphData;
  Zug4.Props := RaumGraphProps;

  DL := TRggDisplayList.Create;

  AchseN[x] := 0;
  AchseN[y] := 0;
  AchseN[z] := 0;

  AchseX[x] := 1;
  AchseX[y] := 0;
  AchseX[z] := 0;

  AchseY[x] := 0;
  AchseY[y] := 1;
  AchseY[z] := 0;

  AchseZ[x] := 0;
  AchseZ[y] := 0;
  AchseZ[z] := 1;

  AchseX := SkalarMult(AchseX, 1000);
  AchseY := SkalarMult(AchseY, 1000);
  AchseZ := SkalarMult(AchseZ, 1000);
end;

destructor TRaumGraph.Destroy;
begin
  Zug3D.Free;
  Zug4.Free;
  DL.Free;
  inherited;
end;

procedure TRaumGraph.Update;
begin
  Update2;
  UpdateZugProps;
  Zug3D.FillZug; // needs updated Props (BogenIndex)
  Updated := True;
end;

procedure TRaumGraph.Update2;
var
  i: TRiggPoint;
  j: Integer;
  RPT: TRealRiggPoints;
  MKT: array [0 .. BogenMax] of TRealPoint;
  KKT: TKoordLine;
begin
  Transformer.UpdateTransformedFixPunkt;

  { Graph drehen }
  if Assigned(Transformer) then
  begin
    for i := ooA0 to ooF0 do
      RPT[i] := Transformer.TransformPoint(rP[i]);
    for i := ooA to ooF do
      RPT[i] := Transformer.TransformPoint(rP[i]);
    for j := 0 to BogenMax do
      MKT[j] := Transformer.TransformPoint(Kurve[j]);

    if Koppel then
    for j := 0 to 100 do
      KKT[j] := Transformer.TransformPoint(KoppelKurve[j]);
  end;

  AchseNT := Transformer.TransformPoint(AchseN);
  AchseXT := Transformer.TransformPoint(AchseX);
  AchseYT := Transformer.TransformPoint(AchseY);
  AchseZT := Transformer.TransformPoint(AchseZ);

  { Es wurde nicht nur rotiert,
    sondern bereits auch verschoben und skaliert }

  with RaumGraphData do
  begin
    xA0 := Round(RPT[ooA0, x]);
    yA0 := Round(RPT[ooA0, z]);
    xB0 := Round(RPT[ooB0, x]);
    yB0 := Round(RPT[ooB0, z]);
    xC0 := Round(RPT[ooC0, x]);
    yC0 := Round(RPT[ooC0, z]);
    xD0 := Round(RPT[ooD0, x]);
    yD0 := Round(RPT[ooD0, z]);
    xE0 := Round(RPT[ooE0, x]);
    yE0 := Round(RPT[ooE0, z]);
    xF0 := Round(RPT[ooF0, x]);
    yF0 := Round(RPT[ooF0, z]);

    xA := Round(RPT[ooA, x]);
    yA := Round(RPT[ooA, z]);
    xB := Round(RPT[ooB, x]);
    yB := Round(RPT[ooB, z]);
    xC := Round(RPT[ooC, x]);
    yC := Round(RPT[ooC, z]);
    xD := Round(RPT[ooD, x]);
    yD := Round(RPT[ooD, z]);
    xE := Round(RPT[ooE, x]);
    yE := Round(RPT[ooE, z]);
    xF := Round(RPT[ooF, x]);
    yF := Round(RPT[ooF, z]);

    xN := Round(AchseNT[x]);
    yN := Round(AchseNT[z]);
    xX := Round(AchseXT[x]);
    yX := Round(AchseXT[z]);
    xY := Round(AchseYT[x]);
    yY := Round(AchseYT[z]);
    xZ := Round(AchseZT[x]);
    yZ := Round(AchseZT[z]);
  end;

  { MastKurve }
  for j := 0 to BogenMax do
  begin
    Zug3D.ZugMastKurve[j].x := Round(MKT[j, x]);
    Zug3D.ZugMastKurve[j].y := -Round(MKT[j, z]);
    Zug4.ZugMastKurve[j].x := Zug3D.ZugMastKurve[j].x;
    Zug4.ZugMastKurve[j].y := Zug3D.ZugMastKurve[j].y;
  end;

  { Koppelkurve }
  if Koppel then
  begin
    for j := 0 to 100 do
    begin
      Zug3D.ZugKoppelKurve[j].X := Round(KKT[j, x]);
      Zug3D.ZugKoppelKurve[j].Y := -Round(KKT[j, z]);
    end;
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

function TRaumGraph.GetFixPunkt: TRealPoint;
begin
  result := Transformer.TransformedFixPunkt;
end;

procedure TRaumGraph.GetPlotList(ML: TStrings);
begin
  if not GrafikOK then
    Exit;
  if not Updated then
      Update;

  Zug3D.GetPlotList(ML);
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

  cr.RiggLED := RiggLED;
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

  { Achsen }
  if WantAchsen then
  begin
    DI.StrokeWidth := 1;
    DI.StrokeColor := clFuchsia;
    DL.Line(AchseNT, AchseXT, ZugAchsen[0], ZugAchsen[1], clRed);
    DI.StrokeColor := clLime;
    DL.Line(AchseNT, AchseYT, ZugAchsen[0], ZugAchsen[2], clGreen);
    DI.StrokeColor := clAqua;
    DL.Line(AchseNT, AchseZT, ZugAchsen[0], ZugAchsen[3], clBlue);
  end;

  end;
end;

end.
