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
  RggDisplayTypes,
  RggDisplay,
  RggDisplayOrder,
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
    A0, B0, C0, D0, E0, F0, P0: TRealPoint;
    A,  B,  C,  D,  E,  F,  P:  TRealPoint;
    M, N: TRealPoint;
  protected
    Zug3D: TZug3DBase; // injected via constructor
  private
    function GetFixPunkt: TRealPoint;
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

    function GetChecked(fa: Integer): Boolean;
    procedure GetPlotList(ML: TStrings); override;
    property FixPunkt: TRealPoint read GetFixPunkt;

    property WantRenderH: Boolean read WantRumpf write WantRumpf;
    property StrokeWidthS: Integer read GetStrokeWidthS;
  end;

implementation

uses
  RiggVar.FB.ActionConst;

constructor TRaumGraph.Create(AZug3D: TZug3DBase);
begin
  inherited Create;

  WantFixPunkt := True;
  WantRumpf := True;
  WantSaling := True;
  WantController := True;
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
  MKT: array [0 .. BogenMax] of TRealPoint;
  KKT: TKoordLine;
begin
  Transformer.UpdateTransformedFixPunkt;

  { Graph drehen }
  if Assigned(Transformer) then
  begin
    for i := Low(TRiggPoint) to High(TRiggPoint) do
      RPT[i] := Transformer.TransformPoint(rP[i]);
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

  A0 := RPT[ooA0];
  B0 := RPT[ooB0];
  C0 := RPT[ooC0];
  D0 := RPT[ooD0];
  E0 := RPT[ooE0];
  F0 := RPT[ooF0];
  P0 := RPT[ooP0];

  A := RPT[ooA];
  B := RPT[ooB];
  C := RPT[ooC];
  D := RPT[ooD];
  E := RPT[ooE];
  F := RPT[ooF];
  P := RPT[ooP];

  M := RPT[ooM];

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

    xP0 := Round(RPT[ooP0, x]);
    yP0 := Round(RPT[ooP0, z]);
    xP := Round(RPT[ooP, x]);
    yP := Round(RPT[ooP, z]);
    xM := Round(RPT[ooM, x]);
    yM := Round(RPT[ooM, z]);
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
      DL.Ellipse('Fixpunkt', deFixPunkt, FixPunkt, FixPunkt, Point(0, 0), TKR);
    end;

    { Rumpf }
    if WantRumpf then
    begin
      DI.StrokeColor := clAqua;
      DI.StrokeWidth := 3;
      DL.Line('A0-B0', deA0B0, A0, B0, ZugRumpf[0], ZugRumpf[1], TColors.Blue);
      DL.Line('B0-C0', deB0C0, B0, C0, ZugRumpf[1], ZugRumpf[2], TColors.DodgerBlue);
      DL.Line('C0-A0', deA0C0, C0, A0, ZugRumpf[2], ZugRumpf[0], TColors.Cornflowerblue);

      DL.Line('A0-D0', deA0D0, A0, D0, ZugRumpf[0], ZugRumpf[4], clRed);
      DL.Line('B0-D0', deB0D0, B0, D0, ZugRumpf[1], ZugRumpf[4], clGreen);
      DL.Line('C0-D0', deC0D0, C0, D0, ZugRumpf[2], ZugRumpf[4], clYellow);
    end;

    { Mast }
    if WantMast then
    begin
      DI.StrokeColor := TColors.Cornflowerblue;
      DI.StrokeWidth := 5;
      if Props.Bogen then
      begin
        DL.PolyLine('D0-D', deD0D, D0, D, ZugMastKurveD0D, TColors.Cornflowerblue);
        DL.PolyLine('D-C', deCD, D, C, ZugMastKurveDC, TColors.Plum);
        DL.Line('C-F', deCF, C, F, ZugMast[2], ZugMast[3], TColors.Navy);
      end
      else
      begin
        DL.Line('D0-D', deD0D, D0, D, ZugMast[0], ZugMast[1], TColors.Cornflowerblue);
        DL.Line('D-C', deCD, D, C, ZugMast[1], ZugMast[2], TColors.Lime);
        DL.Line('C-F', deCF, C, F, ZugMast[2], ZugMast[3], TColors.Navy);
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
      DI.StrokeColor := TColors.Chartreuse;
      DI.StrokeWidth := 6;
      if Props.SalingTyp = stFest then
      begin
        DL.Line('A-D', deAD, A, D, ZugSalingFS[0], ZugSalingFS[1], clLime);
        DL.Line('B-D', deBD, B, D, ZugSalingFS[2], ZugSalingFS[1], TColors.Chartreuse);
        DL.Line('A-B', deAB, A, B, ZugSalingFS[0], ZugSalingFS[2], clTeal);
      end;
      if Props.SalingTyp = stDrehbar then
      begin
        DI.StrokeColor := TColors.Chartreuse;
        DI.StrokeWidth := 2;
        DL.Line('A-D', deAD, A, D, ZugSalingDS[0], ZugSalingDS[1], TColors.Chartreuse);
        DL.Line('B-D', deBD, B, D, ZugSalingDS[2], ZugSalingDS[1], TColors.Chartreuse);
      end;
    end;

    { Controller }
    if WantController then
    begin
      if Props.ControllerTyp <> ctOhne then
    begin
        DI.StrokeColor := TColors.Orchid;
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
      DI.StrokeColor := TColors.Goldenrod;
      DI.StrokeWidth := 1;
      DL.Line('F-M', deMastFall, F, M, ZugMastfall[0], ZugMastfall[1], TColors.Goldenrod);
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

end.
