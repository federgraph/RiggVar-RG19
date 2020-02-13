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
  RggBootGraph;

type
  TRaumGraph = class(TBootGraph)
  protected
    xA0, xB0, xC0, xD0, xE0, xF0, xA, xB, xC, xD, xE, xF: Integer;
    yA0, yB0, yC0, yD0, yE0, yF0, yA, yB, yC, yD, yE, yF: Integer;
//    xX, yX, xY, yY, xZ, yZ, xN, yN: Integer;
    BogenIndexD: Integer;
    function FindBogenIndexOf(P: TRealPoint): Integer;
    procedure UpdateFixPunkt;
    procedure FillZug3D; virtual;
  protected
    { transformed coordinates Rigg }
    A0, B0, C0, D0, E0, F0: TRealPoint;
    A,  B,  C,  D,  E,  F:  TRealPoint;
  protected
    procedure SetViewPoint(const Value: TViewPoint);
    procedure SetWanteGestrichelt(const Value: Boolean);
    procedure SetBogen(const Value: Boolean);
  protected
    FBogen: Boolean;
    FGestrichelt: Boolean;
    FViewPoint: TViewPoint;

    ZugRumpf: TRggPolyLine;
    ZugMast: TRggPolyLine;
    ZugMastKurve: TRggPolyLine;
    ZugSalingFS: TRggPolyLine;
    ZugSalingDS: TRggPolyLine;
    ZugWanteStb: TRggPolyLine;
    ZugWanteBb: TRggPolyLine;
    ZugController: TRggPolyLine;
    ZugVorstag: TRggPolyLine;

    { no need to call SetLength for these, will be copied via Copy }
    ZugMastKurveD0D: TRggPolyLine;
    ZugMastKurveDC: TRggPolyLine;
  public
    WantFixPunkt: Boolean;
    WantRumpf: Boolean;
    WantSaling: Boolean;
    WantController: Boolean;
    WantWante: Boolean;
    WantMast: Boolean;
    WantVorstag: Boolean;

    constructor Create; override;

    procedure Update; override;
    procedure UpdateDisplayList;
    procedure DrawToCanvas(g: TCanvas); override;
    procedure Draw;

    procedure GetPlotList(ML: TStrings); override;
    procedure ToggleRenderOption(const fa: Integer);
    function QueryRenderOption(const fa: Integer): Boolean;

    property ViewPoint: TViewPoint read FViewPoint write SetViewPoint;
    property Bogen: Boolean read FBogen write SetBogen;
    property WanteGestrichelt: Boolean write SetWanteGestrichelt;
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

  SetLength(ZugRumpf, 8);
  SetLength(ZugMast, 4);
  SetLength(ZugMastKurve, BogenMax + 2);
  SetLength(ZugSalingFS, 4);
  SetLength(ZugSalingDS, 3);
  SetLength(ZugWanteStb, 3);
  SetLength(ZugWanteBb, 3);
  SetLength(ZugController, 2);
  SetLength(ZugVorstag, 2);
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
var
  KurveRotiert: array [0 .. BogenMax] of TRealPoint;
  tempRP: TRealRiggPoints;
  i: TRiggPoint;
  j: Integer;
begin
  { Graph drehen }
  if Assigned(Rotator) then
  begin
    for i := ooA0 to ooF0 do
      tempRP[i] := Rotator.Rotiere(rP[i]);
    for i := ooA to ooF do
      tempRP[i] := Rotator.Rotiere(rP[i]);
    for j := 0 to BogenMax do
      KurveRotiert[j] := Rotator.Rotiere(Kurve[j]);
  end;

  UpdateFixPunkt; // gedrehter FixPunkt

  A0 := vsub(tempRP[ooA0], FixPunkt);
  B0 := vsub(tempRP[ooB0], FixPunkt);
  C0 := vsub(tempRP[ooC0], FixPunkt);
  D0 := vsub(tempRP[ooD0], FixPunkt);
  E0 := vsub(tempRP[ooE0], FixPunkt);
  F0 := vsub(tempRP[ooF0], FixPunkt);
  A := vsub(tempRP[ooA],FixPunkt);
  B := vsub(tempRP[ooB], FixPunkt);
  C := vsub(tempRP[ooC],FixPunkt);
  D := vsub(tempRP[ooD], FixPunkt);
  E := vsub(tempRP[ooE], FixPunkt);
  F := vsub(tempRP[ooF],FixPunkt);

  for j := 0 to BogenMax do
    KurveRotiert[j] := vsub(KurveRotiert[j], FixPunkt);

  { Skalieren }
  for j := 0 to BogenMax do
  begin
    xA0 := Round(KurveRotiert[j, x] * Zoom);
    yA0 := Round(KurveRotiert[j, z] * Zoom);
    ZugMastKurve[j].x := xA0;
    ZugMastKurve[j].y := -yA0;
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
//xF0 := Round(F0[x] * Zoom);
//yF0 := Round(F0[z] * Zoom);

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

  FillZug3D;
end;

procedure TRaumGraph.UpdateDisplayList;
var
  DI: TDisplayItem;
begin
  DL.Clear;
  DI := DL.DI;

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
    if FBogen then
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

procedure TRaumGraph.DrawToCanvas(g: TCanvas);
begin
  if GrafikOK then
  begin
    if not Updated then
      Update;
    with g do
    begin
      Pen.Color := clBtnFace;
      Pen.Width := 1;

      { FixPunkt }
      if Coloriert then
        Pen.Color := clYellow;
      Ellipse(
        -TransKreisRadius,
        -TransKreisRadius,
        TransKreisRadius,
        TransKreisRadius);

      { Rumpf }
      if Coloriert then
        Pen.Color := clRumpf;
      PolyLine(ZugRumpf);

      { Saling }
      if Coloriert then
        Pen.Color := clSaling;
      if SalingTyp = stFest then
        PolyLine(ZugSalingFS)
      else if SalingTyp = stDrehbar then
        PolyLine(ZugSalingDS);

      { Mast }
      if Coloriert then
        Pen.Color := clMast;
      if FBogen then
      begin
        PolyLine(ZugMastKurve);
        Pen.Color := clNavy;
        MoveTo(ZugMast[2].X, ZugMast[2].Y);
        LineTo(ZugMast[3].X, ZugMast[3].Y);
      end
      else
        PolyLine(ZugMast);

      { Controller }
      if ControllerTyp <> ctOhne then
      begin
        if Coloriert then
          Pen.Color := clController;
        PolyLine(ZugController);
      end;

      { Wanten }
      if Coloriert then
      begin
        Pen.Color := clGreen;
        if FGestrichelt then
          Pen.Color := TColors.Antiquewhite;
      end;
      PolyLine(ZugWanteStb);

      if Coloriert then
      begin
        Pen.Color := clRed;
        if FGestrichelt then
          Pen.Color := TColors.Antiquewhite;
      end;
      PolyLine(ZugWanteBb);

      { Vorstag }
      if Coloriert then
        Pen.Color := clVorstag;
      PolyLine(ZugVorstag);
    end;
  end;
end;

function TRaumGraph.FindBogenIndexOf(P: TRealPoint): Integer;
var
  i, j: Integer;
  MinIndex: Integer;
  MinAbstand: double;
  a: double;
begin
  j := Length(Kurve);
  MinIndex := j div 2;
  MinAbstand := 1000;
  for i := 0 to j - 1 do
  begin
    a := Abstand(P, Kurve[i]);
    if a < MinAbstand then
    begin
      MinAbstand := a;
      MinIndex := i;
    end;
  end;
  result := MinIndex;
end;

procedure TRaumGraph.GetPlotList(ML: TStrings);
  procedure Plot(L: array of TPoint);
  var
    s: string;
    i: Integer;
  begin
    with ML do
    begin
      s := Format('PU %d %d;', [L[0].x, L[0].y]);
      Add(s);
      for i := 1 to High(L) do
      begin
        s := Format('PD %d %d;', [L[i].x, L[i].y]);
        Add(s);
      end;
    end;
  end;
var
  SavedZoom: double;
begin
  SavedZoom := Zoom;
  Zoom := 10;
  if GrafikOK then
  begin
    if not Updated then
      Update;
    with ML do
    begin
      { Rumpf }
      Add('SP 1;');
      Plot(ZugRumpf);
      { Saling }
      if (SalingTyp = stFest) or (SalingTyp = stDrehbar) then
      begin
        add('SP 2;');
        if SalingTyp = stFest then
          Plot(ZugSalingFS)
        else if SalingTyp = stDrehbar then
          Plot(ZugSalingDS);
      end;
      { Mast }
      Add('SP 3;');
      Plot(ZugMast);
      Add('SP 4;');
      Plot(ZugMastKurve);
      { Controller }
      Add('SP 5;');
      if ControllerTyp <> ctOhne then
        Plot(ZugController);
      { Wanten }
      Add('SP 6;');
      Plot(ZugWanteStb);
      Add('SP 7;');
      Plot(ZugWanteBb);
      { Vorstag }
      Add('SP 8;');
      Plot(ZugVorstag);
    end;
    Zoom := SavedZoom;
  end;
end;

procedure TRaumGraph.SetBogen(const Value: Boolean);
begin
  FBogen := Value;
end;

procedure TRaumGraph.SetViewPoint(const Value: TViewPoint);
begin
  FViewPoint := Value;
end;

procedure TRaumGraph.SetWanteGestrichelt(const Value: Boolean);
begin
  FGestrichelt := Value;
end;

function TRaumGraph.QueryRenderOption(const fa: Integer): Boolean;
begin
  result := False;
end;

procedure TRaumGraph.Draw;
begin

end;

procedure TRaumGraph.ToggleRenderOption(const fa: Integer);
begin

end;

procedure TRaumGraph.FillZug3D;
begin
  { Rumpf }
  ZugRumpf[0].x := xA0;
  ZugRumpf[0].y := -yA0;
  ZugRumpf[1].x := xB0;
  ZugRumpf[1].y := -yB0;
  ZugRumpf[2].x := xC0;
  ZugRumpf[2].y := -yC0;
  ZugRumpf[3].x := xA0;
  ZugRumpf[3].y := -yA0;

  ZugRumpf[4].x := xD0;
  ZugRumpf[4].y := -yD0;
  ZugRumpf[5].x := xB0;
  ZugRumpf[5].y := -yB0;
  ZugRumpf[6].x := xC0;
  ZugRumpf[6].y := -yC0;
  ZugRumpf[7].x := xD0;
  ZugRumpf[7].y := -yD0;

  { Mast }
  ZugMast[0].x := xD0;
  ZugMast[0].y := -yD0;
  ZugMast[1].x := xD;
  ZugMast[1].y := -yD;
  ZugMast[2].x := xC;
  ZugMast[2].y := -yC;
  ZugMast[3].x := xF;
  ZugMast[3].y := -yF;

  { WanteStb }
  ZugWanteStb[0].x := xA0;
  ZugWanteStb[0].y := -yA0;
  ZugWanteStb[1].x := xA;
  ZugWanteStb[1].y := -yA;
  ZugWanteStb[2].x := xC;
  ZugWanteStb[2].y := -yC;

  { WanteBb }
  ZugWanteBb[0].x := xB0;
  ZugWanteBb[0].y := -yB0;
  ZugWanteBb[1].x := xB;
  ZugWanteBb[1].y := -yB;
  ZugWanteBb[2].x := xC;
  ZugWanteBb[2].y := -yC;

  { SalingFS }
  ZugSalingFS[0].x := xA;
  ZugSalingFS[0].y := -yA;
  ZugSalingFS[1].x := xD;
  ZugSalingFS[1].y := -yD;
  ZugSalingFS[2].x := xB;
  ZugSalingFS[2].y := -yB;
  ZugSalingFS[3].x := xA;
  ZugSalingFS[3].y := -yA;

  { SalingDS }
  ZugSalingDS[0].x := xA;
  ZugSalingDS[0].y := -yA;
  ZugSalingDS[1].x := xD;
  ZugSalingDS[1].y := -yD;
  ZugSalingDS[2].x := xB;
  ZugSalingDS[2].y := -yB;

  { Controller }
  ZugController[0].x := xE0;
  ZugController[0].y := -yE0;
  ZugController[1].x := xE;
  ZugController[1].y := -yE;

  { Vorstag }
  ZugVorstag[0].x := xC0;
  ZugVorstag[0].y := -yC0;
  ZugVorstag[1].x := xC;
  ZugVorstag[1].y := -yC;

  { MastKurve }
  ZugMastKurve[BogenMax + 1].x := xF;
  ZugMastKurve[BogenMax + 1].y := -yF;

  BogenIndexD := FindBogenIndexOf(rP[ooD]);
  ZugMastKurveD0D := Copy(ZugMastKurve, 0, BogenIndexD + 1);
  ZugMastKurveDC := Copy(ZugMastKurve, BogenIndexD, Length(ZugMastKurve)-1);
end;

end.
