unit RggRaumGraph;

interface

uses
  Types,
  SysUtils,
  Classes,
  Graphics,
  RiggVar.FD.Point,
  RggCalc,
  RggTypes,
  RggDisplayTypes,
  RggDisplay,
  RggDisplayOrder,
  RggZug,
  RggBootGraph;

type
  TRaumGraph = class(TBootGraph)
  protected
    { original definition of Achsen }
    AchseN: TPoint3D;
    AchseX: TPoint3D;
    AchseY: TPoint3D;
    AchseZ: TPoint3D;

    { transformed coordinates Achsen }
    AchseNT: TPoint3D;
    AchseXT: TPoint3D;
    AchseYT: TPoint3D;
    AchseZT: TPoint3D;

    { transformed coordinates of Rigg }
    A0, B0, C0, D0, E0, F0, P0: TPoint3D;
    A,  B,  C,  D,  E,  F,  P:  TPoint3D;
    M, N: TPoint3D;
  protected
    Zug3D: TZug3DBase; // injected via constructor
  private
    function GetFixPunkt: TPoint3D;
    function GetStrokeWidthS: Integer;
  protected
    procedure UpdateZugProps;
    procedure Update2;
  public
    DF: TRggFrame;
    DL: TRggDisplayList;

    WantFixPunkt: Boolean;
    WantRumpf: Boolean;
    WantSaling: Boolean;
    WantController: Boolean;
    WantWante: Boolean;
    WantMast: Boolean;
    WantVorstag: Boolean;
    WantAchsen: Boolean;

    WantRenderE: Boolean;
    WantRenderF: Boolean;
//    WantRenderH: Boolean;
    WantRenderP: Boolean;
    WantRenderS: Boolean;

    constructor Create(AZug3D: TZug3DBase);
    destructor Destroy; override;

    procedure Update; override;
    procedure UpdateDisplayList;
    procedure DrawToCanvas(g: TCanvas); override;

    procedure SetChecked(fa: Integer; Value: Boolean);
    function GetChecked(fa: Integer): Boolean;
    procedure GetPlotList(ML: TStrings); override;
    property FixPunkt: TPoint3D read GetFixPunkt;

    property WantRenderH: Boolean read WantRumpf write WantRumpf;
    property StrokeWidthS: Integer read GetStrokeWidthS;
  end;

implementation

uses
  RiggVar.FB.ActionConst,
  RiggVar.FB.Color;

constructor TRaumGraph.Create(AZug3D: TZug3DBase);
begin
  inherited Create;

  WantFixPunkt := True;
  WantRumpf := True;
  WantSaling := True;
  WantController := False;
  WantWante := True;
  WantMast := True;
  WantVorstag := True;
  WantAchsen := False;

  WantRenderF := True;
  WantRenderP := True;

  Zug3D := AZug3D;
  Zug3D.Data := RaumGraphData;
  Zug3D.Props := RaumGraphProps;

  DF := TRggFrame.Create;
  DL := TRggDisplayList.Create;
  DL.DF := DF;

  AchseN.X := 0;
  AchseN.Y := 0;
  AchseN.Z := 0;

  AchseX.X := 1;
  AchseX.Y := 0;
  AchseX.Z := 0;

  AchseY.X := 0;
  AchseY.Y := 1;
  AchseY.Z := 0;

  AchseZ.X := 0;
  AchseZ.Y := 0;
  AchseZ.Z := 1;

  AchseX := AchseX * 1000;
  AchseY := AchseY * 1000;
  AchseZ := AchseZ * 1000;
end;

destructor TRaumGraph.Destroy;
begin
  Zug3D.Free;
  DL.Free;
  DF.Free;
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
  RPT: TRiggPoints;
  MKT: array [0 .. BogenMax] of TPoint3D;
  KKT: TKoordLine;
begin
  { Graph drehen }
  if Assigned(Transformer) then
  begin
    for i := Low(TRiggPoint) to High(TRiggPoint) do
      RPT.V[i] := Transformer.TransformPoint(rP.V[i]);
    for j := 0 to BogenMax do
      MKT[j] := Transformer.TransformPoint(Kurve[j]);

    if Koppel then
    for j := 0 to 100 do
      KKT[j] := Transformer.TransformPoint(KoppelKurve[j]);
  end;

  DF.Koordinaten := RPT;

  AchseNT := Transformer.TransformPoint(AchseN);
  AchseXT := Transformer.TransformPoint(AchseX);
  AchseYT := Transformer.TransformPoint(AchseY);
  AchseZT := Transformer.TransformPoint(AchseZ);

  A0 := RPT.A0;
  B0 := RPT.B0;
  C0 := RPT.C0;
  D0 := RPT.D0;
  E0 := RPT.E0;
  F0 := RPT.F0;
  P0 := RPT.P0;

  A := RPT.A;
  B := RPT.B;
  C := RPT.C;
  D := RPT.D;
  E := RPT.E;
  F := RPT.F;
  P := RPT.P;

  M := RPT.M;

  { Es wurde nicht nur rotiert,
    sondern bereits auch verschoben und skaliert }

  with RaumGraphData do
  begin
    xA0 := Round(RPT.A0.X);
    yA0 := Round(RPT.A0.Z);
    xB0 := Round(RPT.B0.X);
    yB0 := Round(RPT.B0.Z);
    xC0 := Round(RPT.C0.X);
    yC0 := Round(RPT.C0.Z);
    xD0 := Round(RPT.D0.X);
    yD0 := Round(RPT.D0.Z);
    xE0 := Round(RPT.E0.X);
    yE0 := Round(RPT.E0.Z);
    xF0 := Round(RPT.F0.X);
    yF0 := Round(RPT.F0.Z);

    xA := Round(RPT.A.X);
    yA := Round(RPT.A.Z);
    xB := Round(RPT.B.X);
    yB := Round(RPT.B.Z);
    xC := Round(RPT.C.X);
    yC := Round(RPT.C.Z);
    xD := Round(RPT.D.X);
    yD := Round(RPT.D.Z);
    xE := Round(RPT.E.X);
    yE := Round(RPT.E.Z);
    xF := Round(RPT.F.X);
    yF := Round(RPT.F.Z);

    xP0 := Round(RPT.P0.X);
    yP0 := Round(RPT.P0.Z);
    xP := Round(RPT.P.X);
    yP := Round(RPT.P.Z);
    xM := Round(RPT.M.X);
    yM := Round(RPT.M.Z);
    xN := Round(AchseNT.X);
    yN := Round(AchseNT.Z);

    xX := Round(AchseXT.X);
    yX := Round(AchseXT.Z);
    xY := Round(AchseYT.X);
    yY := Round(AchseYT.Z);
    xZ := Round(AchseZT.X);
    yZ := Round(AchseZT.Z);
  end;

  { MastKurve }
  for j := 0 to BogenMax do
  begin
    Zug3D.ZugMastKurve[j].x := Round(MKT[j].X);
    Zug3D.ZugMastKurve[j].y := -Round(MKT[j].Z);
  end;

  { Koppelkurve }
  if Koppel then
  begin
    for j := 0 to 100 do
    begin
      Zug3D.ZugKoppelKurve[j].X := Round(KKT[j].X);
      Zug3D.ZugKoppelKurve[j].Y := -Round(KKT[j].Z);
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

function TRaumGraph.GetFixPunkt: TPoint3D;
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

function TRaumGraph.GetStrokeWidthS: Integer;
begin
  if WantRenderS then
    result := 5
  else
    result := 2;
end;

procedure TRaumGraph.UpdateZugProps;
var
  cr: TRaumGraphProps;
begin
  BogenIndexD := FindBogenIndexOf(rP.D);

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
      DL.Ellipse('Fixpunkt', deFixPunkt, FixPunkt, FixPunkt, Point(0, 0), TKR);
    end;

    { Rumpf }
    if WantRumpf then
    begin
      DI.StrokeColor := clAqua;
      DI.StrokeWidth := 3;
      DL.Line('A0-B0', deA0B0, A0, B0, ZugRumpf[0], ZugRumpf[1], TRggColors.Blue);
      DL.Line('B0-C0', deB0C0, B0, C0, ZugRumpf[1], ZugRumpf[2], TRggColors.DodgerBlue);
      DL.Line('C0-A0', deA0C0, C0, A0, ZugRumpf[2], ZugRumpf[0], TRggColors.Cornflowerblue);

      DL.Line('A0-D0', deA0D0, A0, D0, ZugRumpf[0], ZugRumpf[4], clRed);
      DL.Line('B0-D0', deB0D0, B0, D0, ZugRumpf[1], ZugRumpf[4], clGreen);
      DL.Line('C0-D0', deC0D0, C0, D0, ZugRumpf[2], ZugRumpf[4], clYellow);
    end;

    { Mast }
    if WantMast then
    begin
      DI.StrokeColor := TRggColors.Cornflowerblue;
      DI.StrokeWidth := 5;
      if Props.Bogen then
      begin
        DL.PolyLine('D0-D', deD0D, D0, D, ZugMastKurveD0D, TRggColors.Cornflowerblue);
        DL.PolyLine('D-C', deCD, D, C, ZugMastKurveDC, TRggColors.Plum);
        DL.Line('C-F', deCF, C, F, ZugMast[2], ZugMast[3], TRggColors.Navy);
      end
      else
      begin
        DL.Line('D0-D', deD0D, D0, D, ZugMast[0], ZugMast[1], TRggColors.Cornflowerblue);
        DL.Line('D-C', deCD, D, C, ZugMast[1], ZugMast[2], TRggColors.Lime);
        DL.Line('C-F', deCF, C, F, ZugMast[2], ZugMast[3], TRggColors.Navy);
      end;
    end;

    { Wanten }
    if WantWante then
    begin
      { Wante Stb }
      DI.StrokeColor := clGreen;
      DI.StrokeWidth := StrokeWidthS;
      DL.Line('A0-A', deA0A, A0, A, ZugWanteStb[0], ZugWanteStb[1], clGreen);
      DL.Line('A-C', deAC, A, C, ZugWanteStb[1], ZugWanteStb[2], clLime);

      { Wante Bb }
      DI.StrokeColor := clRed;
      DI.StrokeWidth := StrokeWidthS;
      DL.Line('B0-B', deB0B, B0, B, ZugWanteBb[0], ZugWanteBb[1], clRed);
      DL.Line('B-C', deBC, B, C, ZugWanteBb[1], ZugWanteBb[2], clLime);
    end;

    { Saling }
    if WantSaling then
    begin
      DI.StrokeColor := TRggColors.Chartreuse;
      DI.StrokeWidth := 6;
      if Props.SalingTyp = stFest then
      begin
        DL.Line('A-D', deAD, A, D, ZugSalingFS[0], ZugSalingFS[1], clLime);
        DL.Line('B-D', deBD, B, D, ZugSalingFS[2], ZugSalingFS[1], TRggColors.Chartreuse);
        DL.Line('A-B', deAB, A, B, ZugSalingFS[0], ZugSalingFS[2], clTeal);
      end;
      if Props.SalingTyp = stDrehbar then
      begin
        DI.StrokeColor := TRggColors.Chartreuse;
        DI.StrokeWidth := 2;
        DL.Line('A-D', deAD, A, D, ZugSalingDS[0], ZugSalingDS[1], TRggColors.Chartreuse);
        DL.Line('B-D', deBD, B, D, ZugSalingDS[2], ZugSalingDS[1], TRggColors.Chartreuse);
      end;
    end;

    { Controller }
    if WantController then
    begin
      if Props.ControllerTyp <> ctOhne then
    begin
        DI.StrokeColor := TRggColors.Orchid;
        DI.StrokeWidth := 4;
        DL.Line('E0-E', deE0E, E0, E, ZugController[0], ZugController[1], clTeal);
      end;
    end;

    { Vorstag }
    if WantVorstag then
    begin
      DI.StrokeColor := clYellow;
      DI.StrokeWidth := StrokeWidthS;
      DL.Line('C0-C', deC0C, C0, C, ZugVorstag[0], ZugVorstag[1], clYellow);
    end;

  { Achsen }
  if WantAchsen then
  begin
      DI.StrokeWidth := 1;
      DI.StrokeColor := clFuchsia;
      DL.Line('N-X', deNX, AchseNT, AchseXT, ZugAchsen[0], ZugAchsen[1], clRed);
      DI.StrokeColor := clLime;
      DL.Line('N-Y', deNY, AchseNT, AchseYT, ZugAchsen[0], ZugAchsen[2], clGreen);
      DI.StrokeColor := clAqua;
      DL.Line('N-Z', deNZ, AchseNT, AchseZT, ZugAchsen[0], ZugAchsen[3], clBlue);
  end;

    if WantRenderF then
    begin
      DI.StrokeColor := TRggColors.Goldenrod;
      DI.StrokeWidth := 1;
      DL.Line('F-M', deMastFall, F, M, ZugMastfall[0], ZugMastfall[1], TRggColors.Goldenrod);
      DI.StrokeWidth := 4;
      DL.Line('M-F0', deMastFall, M, F0, ZugMastfall[1], ZugMastfall[2], clYellow);
    end;

    if WantRenderP then
    begin
      DI.StrokeColor := clSilver;
      DI.StrokeWidth := 1;
      DL.Line('N-D0', deHullFrame, N, D0, ZugRP[0], ZugRP[1], clFuchsia);
      DL.Line('D0-P0', deHullFrame, D0, P0, ZugRP[1], ZugRP[2], clLime);
      DL.Line('P0-F0', deHullFrame, P0, F0, ZugRP[2], ZugRP[3], clAqua);
      DL.Line('F0-N', deHullFrame, F0, N, ZugRP[3], ZugRP[0], clSilver);
    end;

  end;

  DF.WantController := WantController;
  DF.WantAchsen := WantAchsen;
  DF.Sort;
end;

function TRaumGraph.GetChecked(fa: Integer): Boolean;
begin
  case fa of
    faToggleSegmentF: result := WantFixPunkt;
    faToggleSegmentR: result := WantRumpf;
    faToggleSegmentS: result := WantSaling;
    faToggleSegmentM: result := WantMast;
    faToggleSegmentV: result := WantVorstag;
    faToggleSegmentW: result := WantWante;
    faToggleSegmentC: result := WantController;
    faToggleSegmentA: result := WantAchsen;

    faRggBogen: result := Bogen;
    faRggKoppel: result := Koppel;
    else
      result := False;
  end;
end;

procedure TRaumGraph.SetChecked(fa: Integer; Value: Boolean);
begin
  case fa of
    faToggleSegmentF: WantFixPunkt := Value;
    faToggleSegmentR: WantRumpf := Value;
    faToggleSegmentS: WantSaling := Value;
    faToggleSegmentM: WantMast := Value;
    faToggleSegmentV: WantVorstag := Value;
    faToggleSegmentW: WantWante := Value;
    faToggleSegmentC: WantController := Value;
    faToggleSegmentA: WantAchsen := Value;
  end;
end;

end.
