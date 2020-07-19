unit RggZug2D;

interface

uses
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  System.Types,
  System.UITypes,
  Vcl.Graphics,
  RggTypes,
  RggZug;

type
  TZug1 = class(TZug0)
  public
    ZoomFaktor: Integer;

    Zug1Rumpf: array[1..4] of TPoint;
    Zug1Mast: array[1..4] of TPoint;
    Zug1MastKurve: array[1..BogenMax+2] of TPoint;
    Zug1Saling: array[1..2] of TPoint;
    Zug1Wanten: array[1..3] of TPoint;
    Zug1Vorstag: array[1..2] of TPoint;
    Zug1KoppelKurve: array[1..101] of TPoint;
    Zug1Controller: TRect;
  public
    MP: TPoint;
    IntR: Integer;
    procedure FillZug;
    procedure DrawToCanvas(g: TCanvas);
  end;

  TZug2 = class(TZug0)
  public
    Zug2Rumpf: array[1..6] of TPoint;
    Zug2Wanten: array[1..5] of TPoint;
    Zug2Saling: array[1..4] of TPoint;
    Zug2SalingDS: array[1..3] of TPoint;
    Zug2Mast: array[1..2] of TPoint;
    procedure FillZug;
    procedure DrawToCanvas(g: TCanvas);
  end;

  TZug3 = class(TZug0)
  public
    Zug3Wanten: array[1..5] of TPoint;
    Zug3Rumpf: array[1..10] of TPoint;
    Zug3Mast: array[1..4] of TPoint;
    Zug3SalingFS: array[1..4] of TPoint;
    Zug3SalingDS: array[1..3] of TPoint;

    procedure FillZug;
    procedure DrawToCanvas(g: TCanvas);
  end;

  TZug4 = class(TZug3DBase)
  public
    procedure FillZug; override;
    procedure DrawToCanvas(g: TCanvas); override;
  end;

implementation

uses
  RiggVar.RG.Def;

{ TZug1 }

procedure TZug1.DrawToCanvas(g: TCanvas);
begin
  with g do
  begin
   if Props.Koppel and Props.Coloriert then
   begin
     { Koppelkurve }
     Pen.Color := clKoppelKurve;
     Polyline(Zug1Koppelkurve);
     { Kreisbogen mit Radius Vorstaglänge um C0 }
     Pen.Color := TColors.Black;
     Arc(MP.x-IntR, MP.y-IntR,
         MP.x+IntR, MP.y+IntR,
         MP.x     , MP.y-IntR,
         MP.x-IntR, MP.y-200 * ZoomFaktor);
    end;
    { Rumpf }
    if Props.Coloriert then
      Pen.Color := clRumpf
    else
      Pen.Color := Props.Color;
    PolyLine(Zug1Rumpf);
    { Saling }
    if Props.Coloriert then
      Pen.Color := clSaling
    else
      Pen.Color := Props.Color;
    if (Props.SalingTyp = stFest) or (Props.SalingTyp = stDrehbar) then
      PolyLine(Zug1Saling);
    { Mast }
    if Props.Coloriert then
    begin
      Pen.Color := clMast;
      if Props.Bogen then
        PolyLine(Zug1MastKurve)
      else
        Polyline(Zug1Mast);
    end
    else
    begin
      Pen.Color := Props.Color;
      PolyLine(Zug1Mast);
    end;
    { Controller }
    if Props.ControllerTyp <> ctOhne then
    begin
      Pen.Color := clController;
      Rectangle(
        Zug1Controller.Left,
        Zug1Controller.Top,
        Zug1Controller.Right,
        Zug1Controller.Bottom);
    end;
    { Wanten }
    if Props.Coloriert then
      Pen.Color := clWanten
    else
      Pen.Color := Props.Color;
    PolyLine(Zug1Wanten);
    if Props.Gestrichelt then
    begin
      Pen.Color := clBlue;
      Pen.Style := psDot;
      SetBkMode(Handle, TRANSPARENT);
      PolyLine(Zug1Wanten);
      Pen.Style := psSolid;
      SetBkMode(Handle, OPAQUE);
    end;
    { Vorstag }
    if Props.Coloriert then
      Pen.Color := clWanten
    else
      Pen.Color := Props.Color;
    PolyLine(Zug1Vorstag);
  end;
end;

procedure TZug1.FillZug;
begin
  with Data do
  begin
    { Rumpf von der Seite }
    Zug1Rumpf[1].X :=  xA0 + OffsetX;
    Zug1Rumpf[1].Y := -zA0 + OffsetY;
    Zug1Rumpf[2].X :=  xD0 + OffsetX;
    Zug1Rumpf[2].Y := -zD0 + OffsetY;
    Zug1Rumpf[3].X :=  xC0 + OffsetX;
    Zug1Rumpf[3].Y := -zC0 + OffsetY;
    Zug1Rumpf[4].X :=  xA0 + OffsetX;
    Zug1Rumpf[4].Y := -zA0 + OffsetY;

    { Mast von der Seite }
    Zug1Mast[1].X :=  xD0 + OffsetX;
    Zug1Mast[1].Y := -zD0 + OffsetY;
    Zug1Mast[2].X :=  xD + OffsetX;
    Zug1Mast[2].Y := -zD + OffsetY;
    Zug1Mast[3].X :=  xC + OffsetX;
    Zug1Mast[3].Y := -zC + OffsetY;
    Zug1Mast[4].X :=  xF + OffsetX;
    Zug1Mast[4].Y := -zF + OffsetY;

    Zug1MastKurve[BogenMax+2].X :=  xF + OffsetX;
    Zug1MastKurve[BogenMax+2].Y := -zF + OffsetY;

    { Wanten von der Seite }
    Zug1Wanten[1].X :=  xA0 + OffsetX;
    Zug1Wanten[1].Y := -zA0 + OffsetY;
    Zug1Wanten[2].X :=  xA + OffsetX;
    Zug1Wanten[2].Y := -zA + OffsetY;
    Zug1Wanten[3].X :=  xC + OffsetX;
    Zug1Wanten[3].Y := -zC + OffsetY;

    {Vorstag von der Seite}
    Zug1Vorstag[1].X :=  xC + OffsetX;
    Zug1Vorstag[1].Y := -zC + OffsetY;
    Zug1Vorstag[2].X :=  xC0 + OffsetX;
    Zug1Vorstag[2].Y := -zC0 + OffsetY;

    { Saling von der Seite }
    Zug1Saling[1].X :=  xA + OffsetX;
    Zug1Saling[1].Y := -zA + OffsetY;
    Zug1Saling[2].X :=  xD + OffsetX;
    Zug1Saling[2].Y := -zD + OffsetY;

    { Controller von der Seite }
    Zug1Controller.Left :=  xE + OffsetX;
    Zug1Controller.Top  := -zE - 2 * ZoomFaktor + OffsetY;
    Zug1Controller.Right :=  xE0 + OffsetX;
    Zug1Controller.Bottom := -zE0 + 2 * ZoomFaktor + OffsetY;
  end;
end;

{ TZug2 }

procedure TZug2.DrawToCanvas(g: TCanvas);
begin
  with g do
  begin
    { Rumpf }
    if Props.Coloriert then
      Pen.Color := clRumpf
    else
      Pen.Color := Props.Color;
    PolyLine(Zug2Rumpf);
    { Mast }
    if Props.Coloriert then
      Pen.Color := clMast
    else
      Pen.Color := Props.Color;
    PolyLine(Zug2Mast);
    { Salinge }
    if Props.Coloriert then
      Pen.Color := clSaling
    else
      Pen.Color := Props.Color;
    if Props.SalingTyp = stFest then
      PolyLine(Zug2Saling)
    else if Props.SalingTyp = stDrehbar then
      PolyLine(Zug2SalingDS);
    { Wanten }
    if Props.Coloriert then
      Pen.Color := clWanten
    else
      Pen.Color := Props.Color;
    PolyLine(Zug2Wanten);
    if Props.Gestrichelt then
    begin
      Pen.Color := clBlue;
      Pen.Style := psDot;
      SetBkMode(Handle, TRANSPARENT);
      PolyLine(Zug2Wanten);
      Pen.Style := psSolid;
      SetBkMode(Handle, OPAQUE);
    end;
  end;
end;

procedure TZug2.FillZug;
var
  i: Integer;
begin
  with Data do
  begin
    { Rumpf von hinten }
    Zug2Rumpf[1].X :=  yA0 + OffsetX;
    Zug2Rumpf[1].Y := -zA0 + OffsetY;
    Zug2Rumpf[2].X :=  yC0 + OffsetX;
    Zug2Rumpf[2].Y := -zC0 + OffsetY;
    Zug2Rumpf[3].X :=  yB0 + OffsetX;
    Zug2Rumpf[3].Y := -zB0 + OffsetY;
    Zug2Rumpf[4].X :=  yA0 + OffsetX;
    Zug2Rumpf[4].Y := -zA0 + OffsetY;
    Zug2Rumpf[5].X :=  yD0 + OffsetX;
    Zug2Rumpf[5].Y := -zD0 + OffsetY;
    Zug2Rumpf[6].X :=  yB0 + OffsetX;
    Zug2Rumpf[6].Y := -zB0 + OffsetY;

    { Wanten von hinten }
    Zug2Wanten[1].X :=  yB0 + OffsetX;
    Zug2Wanten[1].Y := -zB0 + OffsetY;
    Zug2Wanten[2].X :=  yB + OffsetX;
    Zug2Wanten[2].Y := -zB + OffsetY;
    Zug2Wanten[3].X :=  yC + OffsetX;
    Zug2Wanten[3].Y := -zC + OffsetY;
    Zug2Wanten[4].X :=  yA + OffsetX;
    Zug2Wanten[4].Y := -zA + OffsetY;
    Zug2Wanten[5].X :=  yA0 + OffsetX;
    Zug2Wanten[5].Y := -zA0 + OffsetY;

    { Salingdreieck von hinten }
    Zug2Saling[1].X :=  yB + OffsetX;
    Zug2Saling[1].Y := -zB + OffsetY;
    Zug2Saling[2].X :=  yA + OffsetX;
    Zug2Saling[2].Y := -zA + OffsetY;
    Zug2Saling[3].X :=  yD + OffsetX;
    Zug2Saling[3].Y := -zD + OffsetY;
    Zug2Saling[4].X :=  yB + OffsetX;
    Zug2Saling[4].Y := -zB + OffsetY;

    for i := 1 to 3 do
    begin
      Zug2SalingDS[i].X := Zug2Saling[i+1].X;
      Zug2SalingDS[i].Y := Zug2Saling[i+1].Y;
    end;

    { Mast von hinten }
    Zug2Mast[1].X :=  yD0 + OffsetX;
    Zug2Mast[1].Y := -zD0 + OffsetY;
    Zug2Mast[2].X :=  yF + OffsetX;
    Zug2Mast[2].Y := -zF + OffsetY;
  end;
end;

{ TZug3 }

procedure TZug3.DrawToCanvas(g: TCanvas);
begin
  with g do
  begin
    { Rumpf }
    if Props.Coloriert then
      Pen.Color := clRumpf
    else
      Pen.Color := Props.Color;
    PolyLine(Zug3Rumpf);
    { Salinge }
    if Props.Coloriert then
      Pen.Color := clSaling
    else
      Pen.Color := Props.Color;
    if Props.SalingTyp = stFest then
      PolyLine(Zug3SalingFS);
    if Props.SalingTyp = stDrehbar then
      PolyLine(Zug3SalingDS);
    { Mast }
    if Props.Coloriert then
      Pen.Color := clMast
    else
      Pen.Color := Props.Color;
    PolyLine(Zug3Mast);
    { Wanten }
    if Props.Coloriert then
      Pen.Color := clWanten
    else
      Pen.Color := Props.Color;
    PolyLine(Zug3Wanten);
    if Props.Gestrichelt then
    begin
      Pen.Color := clBlue;
      Pen.Style := psDot;
      SetBkMode(Handle, TRANSPARENT);
      PolyLine(Zug3Wanten);
      Pen.Style := psSolid;
      SetBkMode(Handle, OPAQUE);
    end;
  end;
end;

procedure TZug3.FillZug;
begin
  with Data do
  begin
    { Wanten in Draufsicht - rot }
    Zug3Wanten[1].X :=  yB0 + OffsetX;
    Zug3Wanten[1].Y := -xB0 + OffsetY;
    Zug3Wanten[2].X :=  yB + OffsetX;
    Zug3Wanten[2].Y := -xB + OffsetY;
    Zug3Wanten[3].X :=  yC + OffsetX;
    Zug3Wanten[3].Y := -xC + OffsetY;
    Zug3Wanten[4].X :=  yA + OffsetX;
    Zug3Wanten[4].Y := -xA + OffsetY;
    Zug3Wanten[5].X :=  yA0 + OffsetX;
    Zug3Wanten[5].Y := -xA0 + OffsetY;

    { Rumpftetraeder in Draufsicht - grün }
    Zug3Rumpf[1].X :=  yA0 + OffsetX;
    Zug3Rumpf[1].Y := -xA0 + OffsetY;
    Zug3Rumpf[2].X :=  yB0 + OffsetX;
    Zug3Rumpf[2].Y := -xB0 + OffsetY;
    Zug3Rumpf[3].X :=  yC0 + OffsetX;
    Zug3Rumpf[3].Y := -xC0 + OffsetY;
    Zug3Rumpf[4].X :=  yA0 + OffsetX;
    Zug3Rumpf[4].Y := -xA0 + OffsetY;
    Zug3Rumpf[5].X :=  yD0 + OffsetX;
    Zug3Rumpf[5].Y := -xD0 + OffsetY;
    Zug3Rumpf[6].X :=  yC0 + OffsetX;
    Zug3Rumpf[6].Y := -xC0 + OffsetY;
    Zug3Rumpf[7].X :=  yD0 + OffsetX;
    Zug3Rumpf[7].Y := -xD0 + OffsetY;
    Zug3Rumpf[8].X :=  yB0 + OffsetX;
    Zug3Rumpf[8].Y := -xB0 + OffsetY;
    Zug3Rumpf[9].X :=  yF0 + OffsetX;
    Zug3Rumpf[9].Y := -xF0 + OffsetY;
    Zug3Rumpf[10].X :=  yA0 + OffsetX;
    Zug3Rumpf[10].Y := -xA0 + OffsetY;

    { Salingdreieck in Draufsicht - lime, für stFest }
    Zug3SalingFS[1].X :=  yB + OffsetX;
    Zug3SalingFS[1].Y := -xB + OffsetY;
    Zug3SalingFS[2].X :=  yA + OffsetX;
    Zug3SalingFS[2].Y := -xA + OffsetY;
    Zug3SalingFS[3].X :=  yD + OffsetX;
    Zug3SalingFS[3].Y := -xD + OffsetY;
    Zug3SalingFS[4].X :=  yB + OffsetX;
    Zug3SalingFS[4].Y := -xB + OffsetY;

    { Salingdreieck in Draufsicht - lime, für stDrehbar }
    Zug3SalingDS[1].X :=  yA + OffsetX;
    Zug3SalingDS[1].Y := -xA + OffsetY;
    Zug3SalingDS[2].X :=  yD + OffsetX;
    Zug3SalingDS[2].Y := -xD + OffsetY;
    Zug3SalingDS[3].X :=  yB + OffsetX;
    Zug3SalingDS[3].Y := -xB + OffsetY;

    { Mast in Draufsicht - blau }
    Zug3Mast[1].X :=  yD0 + OffsetX;
    Zug3Mast[1].Y := -xD0 + OffsetY;
    Zug3Mast[2].X :=  yD + OffsetX;
    Zug3Mast[2].Y := -xD + OffsetY;
    Zug3Mast[3].X :=  yC + OffsetX;
    Zug3Mast[3].Y := -xC + OffsetY;
    Zug3Mast[4].X :=  yF + OffsetX;
    Zug3Mast[4].Y := -xF + OffsetY;
  end;
end;

{ TZug4 }

procedure TZug4.DrawToCanvas(g: TCanvas);
begin
  with g do
  begin
    Pen.Color := clBtnFace;
    Pen.Width := 1;

    { FixPunkt }
    if Props.RiggLED then
      Pen.Color := clLime
    else
      Pen.Color := clYellow;
    Ellipse(-TKR, -TKR, TKR, TKR);

    { Rumpf }
    if Props.Coloriert then
      Pen.Color := clRumpf
    else
      Pen.Color := Props.Color;
    PolyLine(ZugRumpf);

    { Salinge }
    if Props.Coloriert then
      Pen.Color := clSaling
    else
      Pen.Color := Props.Color;
    if Props.SalingTyp = stFest then
      PolyLine(ZugSalingFS);
    if Props.SalingTyp = stDrehbar then
      PolyLine(ZugSalingDS);

    { Mast }
    if Props.Coloriert then
    begin
      Pen.Color := clMast;
      if Props.Bogen then
        PolyLine(ZugMastKurve)
      else
        Polyline(ZugMast);
    end
    else
    begin
      Pen.Color := Props.Color;
      PolyLine(ZugMast);
    end;

    { Wanten }
    if Props.Coloriert then
      Pen.Color := clWanten
    else
      Pen.Color := Props.Color;
    PolyLine(ZugWanteBb);
    PolyLine(ZugWanteStb);
    if Props.Gestrichelt then
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
    if Props.ControllerTyp <> ctOhne then
    begin
      if Props.Coloriert then
        Pen.Color := clController
      else
        Pen.Color := Props.Color;
      PolyLine(ZugController);
    end;

    { Vorstag }
    if Props.Coloriert then
      Pen.Color := clVorstag
    else
      Pen.Color := Props.Color;
    PolyLine(ZugVorstag);
  end;
end;

procedure TZug4.FillZug;
var
  j: Integer;
begin
  with Data do
  begin
    { Rumpf }
    ZugRumpf[0].x := xA0 + OffsetX;
    ZugRumpf[0].y := -yA0 + OffsetY;
    ZugRumpf[1].x := xB0 + OffsetX;
    ZugRumpf[1].y := -yB0 + OffsetY;
    ZugRumpf[2].x := xC0 + OffsetX;
    ZugRumpf[2].y := -yC0 + OffsetY;
    ZugRumpf[3].x := xA0 + OffsetX;
    ZugRumpf[3].y := -yA0 + OffsetY;

    ZugRumpf[4].x := xD0 + OffsetX;
    ZugRumpf[4].y := -yD0 + OffsetY;
    ZugRumpf[5].x := xB0 + OffsetX;
    ZugRumpf[5].y := -yB0 + OffsetY;
    ZugRumpf[6].x := xC0 + OffsetX;
    ZugRumpf[6].y := -yC0 + OffsetY;
    ZugRumpf[7].x := xD0 + OffsetX;
    ZugRumpf[7].y := -yD0 + OffsetY;

    { Mast }
    ZugMast[0].x := xD0 + OffsetX;
    ZugMast[0].y := -yD0 + OffsetY;
    ZugMast[1].x := xD + OffsetX;
    ZugMast[1].y := -yD + OffsetY;
    ZugMast[2].x := xC + OffsetX;
    ZugMast[2].y := -yC + OffsetY;
    ZugMast[3].x := xF + OffsetX;
    ZugMast[3].y := -yF + OffsetY;

    { WanteStb }
    ZugWanteStb[0].x := xA0 + OffsetX;
    ZugWanteStb[0].y := -yA0 + OffsetY;
    ZugWanteStb[1].x := xA + OffsetX;
    ZugWanteStb[1].y := -yA + OffsetY;
    ZugWanteStb[2].x := xC + OffsetX;
    ZugWanteStb[2].y := -yC + OffsetY;

    { WanteBb }
    ZugWanteBb[0].x := xB0 + OffsetX;
    ZugWanteBb[0].y := -yB0 + OffsetY;
    ZugWanteBb[1].x := xB + OffsetX;
    ZugWanteBb[1].y := -yB + OffsetY;
    ZugWanteBb[2].x := xC + OffsetX;
    ZugWanteBb[2].y := -yC + OffsetY;

    { SalingFS }
    ZugSalingFS[0].x := xA + OffsetX;
    ZugSalingFS[0].y := -yA + OffsetY;
    ZugSalingFS[1].x := xD + OffsetX;
    ZugSalingFS[1].y := -yD + OffsetY;
    ZugSalingFS[2].x := xB + OffsetX;
    ZugSalingFS[2].y := -yB + OffsetY;
    ZugSalingFS[3].x := xA + OffsetX;
    ZugSalingFS[3].y := -yA + OffsetY;

    { SalingDS }
    ZugSalingDS[0].x := xA + OffsetX;
    ZugSalingDS[0].y := -yA + OffsetY;
    ZugSalingDS[1].x := xD + OffsetX;
    ZugSalingDS[1].y := -yD + OffsetY;
    ZugSalingDS[2].x := xB + OffsetX;
    ZugSalingDS[2].y := -yB + OffsetY;

    { Controller }
    ZugController[0].x := xE0 + OffsetX;
    ZugController[0].y := -yE0 + OffsetY;
    ZugController[1].x := xE + OffsetX;
    ZugController[1].y := -yE + OffsetY;

    { Vorstag }
    ZugVorstag[0].x := xC0 + OffsetX;
    ZugVorstag[0].y := -yC0 + OffsetY;
    ZugVorstag[1].x := xC + OffsetX;
    ZugVorstag[1].y := -yC + OffsetY;

    { MastKurve 3D }
    for j := 0 to BogenMax do
    begin
      ZugMastKurve[j].x := ZugMastKurve[j].x + OffsetX;
      ZugMastKurve[j].y := ZugMastKurve[j].y + OffsetY;
    end;
    ZugMastKurve[BogenMax + 1].x := xF + OffsetX;
    ZugMastKurve[BogenMax + 1].y := -yF + OffsetY;
  end;
end;

end.
