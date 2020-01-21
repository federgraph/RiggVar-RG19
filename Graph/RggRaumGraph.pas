unit RggRaumGraph;

interface

uses
  Winapi.Windows,
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
  private
    { temporäre Koordinaten double transformed }
    A0, B0, C0, D0, E0, F0: TRealPoint;
    A,  B,  C,  D,  E,  F:  TRealPoint;
  protected
    ZugRumpf: array [1 .. 8] of TPoint;
    ZugMast: array [1 .. 4] of TPoint;
    ZugMastKurve: array [1 .. BogenMax + 2] of TPoint;
    ZugSalingFS: array [1 .. 4] of TPoint;
    ZugSalingDS: array [1 .. 3] of TPoint;
    ZugWanteStb: array [1 .. 3] of TPoint;
    ZugWanteBb: array [1 .. 3] of TPoint;
    ZugController: array [1 .. 2] of TPoint;
    ZugVorstag: array [1 .. 2] of TPoint;
    procedure FillZug3D;
  public
    procedure Update; override;
    procedure UpdateDisplayList;
    procedure Draw(Canvas: TCanvas); override;
    procedure GetPlotList(List: TStringList); override;
  end;

implementation

uses
  RiggVar.RG.Def;

procedure TRaumGraph.Update;
begin
  FillZug3D;
end;

procedure TRaumGraph.FillZug3D;
var
  tempRP: TRealRiggPoints;
  FixPunkt3D: TRealPoint;
  i: TRiggPoint;
  j: Integer;
  { temporäre Koordinaten Mastkurve double transformed }
  KurveRotiert: array [0 .. BogenMax] of TRealPoint;
  { temporäre Koordinaten Integer transformed }
  xA0, xB0, xC0, xD0, xE0, { xF0, } xA, xB, xC, xD, xE, xF: Integer;
  yA0, yB0, yC0, yD0, yE0, { yF0, } yA, yB, yC, yD, yE, yF: Integer;
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

  { den Fixpunkt des gedrehten Graphen in den Nullpunkt verschieben }
  FixPunkt3D := tempRP[FixPoint];
  FixPunkt := FixPunkt3D;
  A0 := vsub(tempRP[ooA0], FixPunkt3D);
  B0 := vsub(tempRP[ooB0], FixPunkt3D);
  C0 := vsub(tempRP[ooC0], FixPunkt3D);
  D0 := vsub(tempRP[ooD0], FixPunkt3D);
  E0 := vsub(tempRP[ooE0], FixPunkt3D);
  F0 := vsub(tempRP[ooF0], FixPunkt3D);
  A := vsub(tempRP[ooA],FixPunkt3D);
  B := vsub(tempRP[ooB], FixPunkt3D);
  C := vsub(tempRP[ooC],FixPunkt3D);
  D := vsub(tempRP[ooD], FixPunkt3D);
  E := vsub(tempRP[ooE], FixPunkt3D);
  F := vsub(tempRP[ooF],FixPunkt3D);
  for j := 0 to BogenMax do
    KurveRotiert[j] := vsub(KurveRotiert[j], FixPunkt3D);

   { Skalieren und um den Offset verschieben }
  for j := 0 to BogenMax do
  begin
    xA0 := Round(KurveRotiert[j, x] * Zoom);
    yA0 := Round(KurveRotiert[j, z] * Zoom);
    ZugMastKurve[j + 1].x := xA0 + Offset.x;
    ZugMastKurve[j + 1].y := -yA0 + Offset.y;
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
  //xF0 := Round(F0[x]*Zoom);
  //yF0 := Round(F0[z]*Zoom);

  xA := Round(A[x]*Zoom);
  yA := Round(A[z]*Zoom);
  xB := Round(B[x] * Zoom);
  yB := Round(B[z] * Zoom);
  xC := Round(C[x]*Zoom);
  yC := Round(C[z]*Zoom);
  xD := Round(D[x] * Zoom);
  yD := Round(D[z] * Zoom);
  xE := Round(E[x] * Zoom);
  yE := Round(E[z] * Zoom);
  xF := Round(F[x]*Zoom);
  yF := Round(F[z]*Zoom);

  { Rumpf }
  ZugRumpf[1].x := xA0 + Offset.x;
  ZugRumpf[1].y := -yA0 + Offset.y;
  ZugRumpf[2].x := xB0 + Offset.x;
  ZugRumpf[2].y := -yB0 + Offset.y;
  ZugRumpf[3].x := xC0 + Offset.x;
  ZugRumpf[3].y := -yC0 + Offset.y;
  ZugRumpf[4].x := xA0 + Offset.x;
  ZugRumpf[4].y := -yA0 + Offset.y;

  ZugRumpf[5].x := xD0 + Offset.x;
  ZugRumpf[5].y := -yD0 + Offset.y;
  ZugRumpf[6].x := xB0 + Offset.x;
  ZugRumpf[6].y := -yB0 + Offset.y;
  ZugRumpf[7].x := xC0 + Offset.x;
  ZugRumpf[7].y := -yC0 + Offset.y;
  ZugRumpf[8].x := xD0 + Offset.x;
  ZugRumpf[8].y := -yD0 + Offset.y;

  { Mast }
  ZugMast[1].x := xD0 + Offset.x;
  ZugMast[1].y := -yD0 + Offset.y;
  ZugMast[2].x := xD + Offset.x;
  ZugMast[2].y := -yD + Offset.y;
  ZugMast[3].x := xC + Offset.x;
  ZugMast[3].y := -yC + Offset.y;
  ZugMast[4].x := xF + Offset.x;
  ZugMast[4].y := -yF + Offset.y;

  ZugMastKurve[BogenMax + 2].x := xF + Offset.x;
  ZugMastKurve[BogenMax + 2].y := -yF + Offset.y;

  { WanteStb }
  ZugWanteStb[1].x := xA0 + Offset.x;
  ZugWanteStb[1].y := -yA0 + Offset.y;
  ZugWanteStb[2].x := xA + Offset.x;
  ZugWanteStb[2].y := -yA + Offset.y;
  ZugWanteStb[3].x := xC + Offset.x;
  ZugWanteStb[3].y := -yC + Offset.y;

  { WanteBb }
  ZugWanteBb[3].x := xC + Offset.x;
  ZugWanteBb[3].y := -yC + Offset.y;
  ZugWanteBb[2].x := xB + Offset.x;
  ZugWanteBb[2].y := -yB + Offset.y;
  ZugWanteBb[1].x := xB0 + Offset.x;
  ZugWanteBb[1].y := -yB0 + Offset.y;

  { SalingFS }
  ZugSalingFS[1].x := xA + Offset.x;
  ZugSalingFS[1].y := -yA + Offset.y;
  ZugSalingFS[2].x := xD + Offset.x;
  ZugSalingFS[2].y := -yD + Offset.y;
  ZugSalingFS[3].x := xB + Offset.x;
  ZugSalingFS[3].y := -yB + Offset.y;
  ZugSalingFS[4].x := xA + Offset.x;
  ZugSalingFS[4].y := -yA + Offset.y;

  { SalingDS }
  ZugSalingDS[1].x := xA + Offset.x;
  ZugSalingDS[1].y := -yA + Offset.y;
  ZugSalingDS[2].x := xD + Offset.x;
  ZugSalingDS[2].y := -yD + Offset.y;
  ZugSalingDS[3].x := xB + Offset.x;
  ZugSalingDS[3].y := -yB + Offset.y;

  { Controller }
  ZugController[1].x := xE0 + Offset.x;
  ZugController[1].y := -yE0 + Offset.y;
  ZugController[2].x := xE + Offset.x;
  ZugController[2].y := -yE + Offset.y;

  { Vorstag }
  ZugVorstag[1].x := xC0 + Offset.x;
  ZugVorstag[1].y := -yC0 + Offset.y;
  ZugVorstag[2].x := xC + Offset.x;
  ZugVorstag[2].y := -yC + Offset.y;
end;

procedure TRaumGraph.UpdateDisplayList;
var
  DI: TDisplayItem;
begin
  DL.Clear;
  DI := DL.DI;

  DI.Color := clYellow;
  DI.StrokeWidth := 1;
  DL.Ellipse(FixPunkt, FixPunkt, Offset, 10);

  { Rumpf }
  DI.Color := TColors.Lightgrey;
  DI.StrokeWidth := 10;
  DL.Line(A0, B0, ZugRumpf[1], ZugRumpf[2]);
  DL.Line(B0, C0, ZugRumpf[2], ZugRumpf[3]);
  DL.Line(A0, C0, ZugRumpf[1], ZugRumpf[3]);
  DL.Line(D0, A0, ZugRumpf[5], ZugRumpf[1]);
  DL.Line(D0, B0, ZugRumpf[5], ZugRumpf[2]);
  DL.Line(D0, C0, ZugRumpf[5], ZugRumpf[3]);

  { Mast }
  DI.Color := TColors.Cornflowerblue;
  DI.StrokeWidth := 8;
  DL.PolyLine(D0, D, ZugMastKurve);

  { Wante Stb }
  DI.Color := clRed;
  DI.StrokeWidth := 2;
  DL.Line(A0, A, ZugWanteStb[1], ZugWanteStb[2]);
  DL.Line(A, C, ZugWanteStb[2], ZugWanteStb[3]);

  { Wante Bb }
  DI.Color := clGreen;
  DI.StrokeWidth := 2;
  DL.Line(B0, B, ZugWanteBb[1], ZugWanteBb[2]);
  DL.Line(B, C, ZugWanteBb[2], ZugWanteBb[3]);

  { Saling }
  DI.Color := clLime;
  DI.StrokeWidth := 6;

  if SalingTyp = stFest then
  begin
    DL.Line(A, D, ZugSalingFS[1], ZugSalingFS[2]);
    DL.Line(B, D, ZugSalingFS[3], ZugSalingFS[2]);
    DL.Line(A, B, ZugSalingFS[1], ZugSalingFS[3]);
  end;

  if SalingTyp = stDrehbar then
  begin
    DI.Color := clLime;
    DI.StrokeWidth := 2;
    DL.Line(A, D, ZugSalingDS[1], ZugSalingDS[2]);
    DL.Line(B, D, ZugSalingDS[3], ZugSalingDS[2]);
  end;

  { Controller }
  if ControllerTyp <> ctOhne then
  begin
    DI.Color := clAqua;
    DI.StrokeWidth := 4;
    DL.Line(E0, E, ZugController[1], ZugController[2]);
  end;

  { Vorstag }
  DI.Color := clYellow;
  DI.StrokeWidth := 4;
  DL.Line(C, C0, ZugVorstag[1], ZugVorstag[2]);
end;

procedure TRaumGraph.Draw(Canvas: TCanvas);
begin
  if GrafikOK then
  begin
    if not Updated then
      Update;
    with Canvas do
    begin
      Pen.Color := clBtnFace;
      Pen.Width := 1;
      { FixPunkt }
      if Coloriert then
        Pen.Color := clYellow;
      Ellipse(Offset.x - 10, Offset.y - 10, Offset.x + 10, Offset.y + 10);
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
      PolyLine(ZugMastKurve); { PolyLine(ZugMast); }
      { Controller }
      if ControllerTyp <> ctOhne then
      begin
        if Coloriert then
          Pen.Color := clController;
        PolyLine(ZugController);
      end;
      { Wanten }
      if Coloriert then
        Pen.Color := clGreen;
      PolyLine(ZugWanteStb);
      if Coloriert then
        Pen.Color := clRed;
      PolyLine(ZugWanteBb);
      { Vorstag }
      if Coloriert then
        Pen.Color := clVorstag;
      PolyLine(ZugVorstag);
    end;
  end;
end;

procedure TRaumGraph.GetPlotList(List: TStringList);
  procedure Plot(L: array of TPoint);
  var
    s: string;
    i: Integer;
  begin
    with List do
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
    with List do
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

end.
