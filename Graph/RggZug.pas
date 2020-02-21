unit RggZug;

interface

uses
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  System.Types,
  System.UITypes,
  Vcl.Graphics,
  RggTypes;

type
  TRaumGraphData = class
  public
    xA0, xB0, xC0, xD0, xE0, xF0, xA, xB, xC, xD, xE, xF: Integer;
    yA0, yB0, yC0, yD0, yE0, yF0, yA, yB, yC, yD, yE, yF: Integer;
    zA0, zB0, zC0, zD0, zE0, zF0, zA, zB, zC, zD, zE, zF: Integer;
    xX, yX, xY, yY, xZ, yZ, xN, yN: Integer;
  end;

  TRaumGraphProps = class
  public
    SalingTyp: TSalingTyp;
    ControllerTyp: TControllerTyp;
    BogenIndexD: Integer;
    Bogen: Boolean;
    Coloriert: Boolean;
    Color: TColor;
    Koppel: Boolean;
    Gestrichelt: Boolean;
    RiggLED: Boolean;
  end;

  TZug0 = class
  public
    Data: TRaumGraphData; // injected
    Props: TRaumGraphProps; // injected
  end;

  TZug3D = class(TZug0)
  public
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

    ZugKoppelKurve: TRggPolyLine;

    ZugAchsen: TRggPolyLine;

    constructor Create;
    procedure FillZug;
    procedure DrawToCanvas(g: TCanvas);
    procedure GetPlotList(ML: TStrings);
  end;

  TZug1 = class(TZug0)
  public
    OffsetX1: Integer;
    OffsetY1: Integer;

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
    constructor Create;
    procedure FillZug;
    procedure DrawToCanvas(g: TCanvas);
  end;

  TZug2 = class(TZug0)
  public
    OffsetX2: Integer;
    OffsetY2: Integer;

    Zug2Rumpf: array[1..6] of TPoint;
    Zug2Wanten: array[1..5] of TPoint;
    Zug2Saling: array[1..4] of TPoint;
    Zug2SalingDS: array[1..3] of TPoint;
    Zug2Mast: array[1..2] of TPoint;
    constructor Create;
    procedure FillZug;
    procedure DrawToCanvas(g: TCanvas);
  end;

  TZug3 = class(TZug0)
  public
    OffsetX3: Integer;
    OffsetY3: Integer;

    Zug3Wanten: array[1..5] of TPoint;
    Zug3Rumpf: array[1..10] of TPoint;
    Zug3Mast: array[1..4] of TPoint;
    Zug3SalingFS: array[1..4] of TPoint;
    Zug3SalingDS: array[1..3] of TPoint;

    constructor Create;
    procedure FillZug;
    procedure DrawToCanvas(g: TCanvas);
  end;

  TZug4 = class(TZug0)
  public
    OffsetX4: Integer;
    OffsetY4: Integer;

    ZugRumpf: TRggPolyLine;
    ZugMast: TRggPolyLine;
    ZugMastKurve: TRggPolyLine;
    ZugSalingFS: TRggPolyLine;
    ZugSalingDS: TRggPolyLine;
    ZugWanteStb: TRggPolyLine;
    ZugWanteBb: TRggPolyLine;
    ZugController: TRggPolyLine;
    ZugVorstag: TRggPolyLine;

    constructor Create;
    procedure FillZug;
    procedure DrawToCanvas(g: TCanvas);
  end;

implementation

uses
  RiggVar.RG.Def;

{ TZug0 }

{ TZug3D }

constructor TZug3D.Create;
begin
  inherited;
  SetLength(ZugAchsen, 4);
  SetLength(ZugRumpf, 8);
  SetLength(ZugMast, 4);
  SetLength(ZugMastKurve, BogenMax + 2);
  SetLength(ZugSalingFS, 4);
  SetLength(ZugSalingDS, 3);
  SetLength(ZugWanteStb, 3);
  SetLength(ZugWanteBb, 3);
  SetLength(ZugController, 2);
  SetLength(ZugVorstag, 2);
  SetLength(ZugKoppelKurve, 101);
end;

procedure TZug3D.FillZug;
begin
  with Data do
  begin
    { Achsen }
    ZugAchsen[0].x := xN;
    ZugAchsen[0].y := -yN;
    ZugAchsen[1].x := xX;
    ZugAchsen[1].y := -yX;
    ZugAchsen[2].x := xY;
    ZugAchsen[2].y := -yY;
    ZugAchsen[3].x := xZ;
    ZugAchsen[3].y := -yZ;

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
  end;

  ZugMastKurveD0D := Copy(ZugMastKurve, 0, Props.BogenIndexD + 1);
  ZugMastKurveDC := Copy(ZugMastKurve, Props.BogenIndexD, Length(ZugMastKurve)-1);
end;

procedure TZug3D.DrawToCanvas(g: TCanvas);
begin
  with g do
  begin
    Pen.Width := 1;

    { FixPunkt }
    if Props.RiggLED then
      Pen.Color := clLime
    else
      Pen.Color := clYellow;
    Ellipse(
      -TransKreisRadius,
      -TransKreisRadius,
      TransKreisRadius,
      TransKreisRadius);

    Pen.Color := Props.Color;

    { Koppelkurve }
    if Props.Koppel then
    begin
      Pen.Color := clKoppelKurve;
      PolyLine(ZugKoppelkurve);
    end;

    { Rumpf }
    if Props.Coloriert then
      Pen.Color := clRumpf;
    PolyLine(ZugRumpf);

    { Saling }
    if Props.Coloriert then
      Pen.Color := clSaling;
    if Props.SalingTyp = stFest then
      PolyLine(ZugSalingFS)
    else if Props.SalingTyp = stDrehbar then
      PolyLine(ZugSalingDS);

    { Mast }
    if Props.Coloriert then
    begin
      Pen.Color := clMast;
      if Props.Bogen then
      begin
        PolyLine(ZugMastKurve);
        Pen.Color := clNavy;
        MoveTo(ZugMast[2].X, ZugMast[2].Y);
        LineTo(ZugMast[3].X, ZugMast[3].Y);
      end
      else
      begin
        PolyLine(ZugMast);
      end;
    end
    else
    begin
      PolyLine(ZugMast);
    end;

    { Controller }
    if Props.ControllerTyp <> ctOhne then
    begin
      if Props.Coloriert then
        Pen.Color := clController;
      PolyLine(ZugController);
    end;

    { Wanten }
    if Props.Coloriert then
    begin
      Pen.Color := clGreen;
      if Props.Gestrichelt then
        Pen.Color := TColors.Antiquewhite;
    end;
    PolyLine(ZugWanteStb);

    if Props.Coloriert then
    begin
      Pen.Color := clRed;
      if Props.Gestrichelt then
        Pen.Color := TColors.Antiquewhite;
    end;
    PolyLine(ZugWanteBb);

    { Vorstag }
    if Props.Coloriert then
      Pen.Color := clVorstag;
    PolyLine(ZugVorstag);
  end;
end;

procedure TZug3D.GetPlotList(ML: TStrings);
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

begin
  with ML do
  begin
    { Rumpf }
    Add('SP 1;');
    Plot(ZugRumpf);
    { Saling }
    if (Props.SalingTyp = stFest) or (Props.SalingTyp = stDrehbar) then
    begin
      Add('SP 2;');
      if Props.SalingTyp = stFest then
        Plot(ZugSalingFS)
      else if Props.SalingTyp = stDrehbar then
        Plot(ZugSalingDS);
    end;
    { Mast }
    Add('SP 3;');
    Plot(ZugMast);
    Add('SP 4;');
    Plot(ZugMastKurve);
    { Controller }
    Add('SP 5;');
    if Props.ControllerTyp <> ctOhne then
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
end;

{ TZug1 }

constructor TZug1.Create;
begin

end;

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
     Pen.Color := clBlack;
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
  end;
end;

{ TZug2 }

constructor TZug2.Create;
begin

end;

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
  end;
end;

{ TZug3 }

constructor TZug3.Create;
begin
end;

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
end;

{ TZug4 }

constructor TZug4.Create;
begin
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
    Ellipse(
      -TransKreisRadius,
      -TransKreisRadius,
      TransKreisRadius,
      TransKreisRadius);

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

    { Mast }
    ZugMast[0].x := xD0 + OffsetX4;
    ZugMast[0].y := -yD0 + OffsetY4;
    ZugMast[1].x := xD + OffsetX4;
    ZugMast[1].y := -yD + OffsetY4;
    ZugMast[2].x := xC + OffsetX4;
    ZugMast[2].y := -yC + OffsetY4;
    ZugMast[3].x := xF + OffsetX4;
    ZugMast[3].y := -yF + OffsetY4;

    { WanteStb }
    ZugWanteStb[0].x := xA0 + OffsetX4;
    ZugWanteStb[0].y := -yA0 + OffsetY4;
    ZugWanteStb[1].x := xA + OffsetX4;
    ZugWanteStb[1].y := -yA + OffsetY4;
    ZugWanteStb[2].x := xC + OffsetX4;
    ZugWanteStb[2].y := -yC + OffsetY4;

    { WanteBb }
    ZugWanteBb[0].x := xB0 + OffsetX4;
    ZugWanteBb[0].y := -yB0 + OffsetY4;
    ZugWanteBb[1].x := xB + OffsetX4;
    ZugWanteBb[1].y := -yB + OffsetY4;
    ZugWanteBb[2].x := xC + OffsetX4;
    ZugWanteBb[2].y := -yC + OffsetY4;

    { SalingFS }
    ZugSalingFS[0].x := xA + OffsetX4;
    ZugSalingFS[0].y := -yA + OffsetY4;
    ZugSalingFS[1].x := xD + OffsetX4;
    ZugSalingFS[1].y := -yD + OffsetY4;
    ZugSalingFS[2].x := xB + OffsetX4;
    ZugSalingFS[2].y := -yB + OffsetY4;
    ZugSalingFS[3].x := xA + OffsetX4;
    ZugSalingFS[3].y := -yA + OffsetY4;

    { SalingDS }
    ZugSalingDS[0].x := xA + OffsetX4;
    ZugSalingDS[0].y := -yA + OffsetY4;
    ZugSalingDS[1].x := xD + OffsetX4;
    ZugSalingDS[1].y := -yD + OffsetY4;
    ZugSalingDS[2].x := xB + OffsetX4;
    ZugSalingDS[2].y := -yB + OffsetY4;

    { Controller }
    ZugController[0].x := xE0 + OffsetY4;
    ZugController[0].y := -yE0 + OffsetX4;
    ZugController[1].x := xE + OffsetY4;
    ZugController[1].y := -yE + OffsetY4;

    { Vorstag }
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
  end;
end;

end.
