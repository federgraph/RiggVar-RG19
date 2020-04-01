unit RggZug3D;

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
  TZug3D = class(TZug3DBase)
  public
    procedure FillZug; override;
    procedure DrawToCanvas(g: TCanvas); override;
    procedure GetPlotList(ML: TStrings); override;
  end;

implementation

uses
  RiggVar.RG.Def;

{ TZug3D }

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
  { not including Point F }
  ZugMastKurveDC := Copy(ZugMastKurve, Props.BogenIndexD, Length(ZugMastKurve)-2-Props.BogenIndexD);
  { including Point F }
//  ZugMastKurveDC := Copy(ZugMastKurve, Props.BogenIndexD, Length(ZugMastKurve)-1-Props.BogenIndexD);
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

end.
