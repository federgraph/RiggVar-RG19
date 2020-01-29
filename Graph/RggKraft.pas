unit RggKraft;

interface

{$SCOPEDENUMS ON}

uses
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  System.IniFiles,
  System.Types,
  System.Math,
  Vcl.Dialogs,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.ExtCtrls,
  Vcl.StdCtrls,
  Vcl.ComCtrls,
  RggTypes,
  RggCalc,
  RggSchnittKK,
  RggUnit1,
  RggUnit2;

type
  TKraftGraph = class(TKraftGraph0)
  private
    FOffsetX, FOffsetY: Integer;
    FZoomX, FZoomY: double;
    FPunktPos: TPoint;
    FKurveOhne,
    FKurveMit,
    FVerschoben,
    FVerschobenKorrigiert: TLineDataR150;
    FKurveOhneKorrigiert,
    FKurveMitKorrigiert: TLineDataR100;
    procedure Draw(Canvas: TCanvas; Rect: TRect);
    procedure DrawPunkt(Canvas: TCanvas; Punkt: TPoint);
    procedure DrawKurve(Canvas: TCanvas; Kurve: TLineDataR150);
    procedure DrawKurveQuer(Canvas: TCanvas; Kurve: TLineDataR100);
    function GetKoppelFaktor: double;
    function GetSalingAlpha: double;
    function GetKorrigiert: Boolean;
    function GetControllerTyp: TControllerTyp;
  public
    Mast: TMast;
    constructor Create(AMast: TMast); virtual;
    procedure DrawPaintBoxK(Canvas: TCanvas; Rect: TRect); override;
    procedure GetTestKurven; override;
    property KoppelFaktor: double read GetKoppelFaktor;
    property SalingAlpha: double read GetSalingAlpha;
    property Korrigiert: Boolean read GetKorrigiert;
    property ControllerTyp: TControllerTyp read GetControllerTyp;
  end;

implementation

constructor TKraftGraph.Create(AMast: TMast);
begin
  inherited Create;
  if not Assigned(AMast) then begin
    Free;
    Exit;
  end;
  Mast := AMast;
  FOffsetX := 750;
  FOffsetY := 500;
  FZoomX := 15;
  FZoomY := 0.1;
  ShowAll := True;
end;

function TKraftGraph.GetKoppelFaktor: double;
begin
  result := Mast.KoppelFaktor;
end;

function TKraftGraph.GetSalingAlpha: double;
begin
  result := Mast.SalingAlpha;
end;

function TKraftGraph.GetKorrigiert: Boolean;
begin
  result := Mast.Korrigiert;
end;

function TKraftGraph.GetControllerTyp: TControllerTyp;
begin
  result := Mast.ControllerTyp;
end;

procedure TKraftGraph.Draw(Canvas: TCanvas; Rect: TRect);
var
  Pos: TPoint;
  R: TRect;
  bmp: TBitmap;
begin
  bmp := TBitmap.Create;
  with bmp do
  begin
    Width := Rect.Right - Rect.Left;
    Height := Rect.Bottom - Rect.Top;
  end;
  try
    PaintBackGround(bmp);

    with Rect do
    begin
      Pos.x := Left + (Right-Left) div 2;
      Pos.y := Top + (Bottom-Top) div 2;
    end;

    with bmp.Canvas do
    begin
      SetMapMode(Handle, MM_ISOTROPIC);
      SetWindowExtEx(Handle, 2100, -2100, nil);
      SetWindowOrgEx(Handle, FOffsetX, FOffsetY, nil);
      SetViewPortExtEx(Handle, bmp.Width, bmp.Height, nil);
      SetViewPortOrgEx(Handle, Pos.x, Pos.y, nil);

      { Koordinaten des Rahmenrechtecks }
      with R do begin
        Left := -1500+FOffsetX;
        Right := 1500+FOffsetX;
        Top := 1000+FOffsetY;
        Bottom := -1000+FOffsetY;
      end;

      { Rahmen zeichnen und Clipping setzen }
      Pen.Color := clBlue;
      Brush.Style := bsClear;
      Rectangle( R.Left, R.Top, R.Right, R.Bottom);
      IntersectClipRect(Handle, R.Left, R.Top, R.Right, R.Bottom);

      { Achsenkreuz }
      Pen.Color := clBlack;
      MoveTo(R.Left, 0);
      LineTo(R.Right, 0);
      MoveTo(0,R.Top);
      LineTo(0,R.Bottom);

      { Punkte }
      Pen.Color := clBlue;
      Brush.Style := bsSolid;
      Brush.Color := clRed;
      { Nullpunkt }
      DrawPunkt(bmp.Canvas, Point(0,0));
      Brush.Color := clAqua;
      { aktueller Punkt }
      DrawPunkt(bmp.Canvas, FPunktPos);

      { Maßlinien beim aktuellen Punkt }
      Pen.Color := clBlack;
      MoveTo(-50, Round(FPunktPos.y*FZoomY));
      LineTo(  0, Round(FPunktPos.y*FZoomY));
      MoveTo(Round(FPunktPos.x*FZoomX),0);
      LineTo(Round(FPunktPos.x*FZoomX),-50);

      { Beschriftung }
      Font.Color := clBlue;
      Brush.Color := clSilver;
      SetTextAlign(Handle, TA_RIGHT or TA_BOTTOM);
      TextOut(R.Right-50, 50, 'hd[mm]');
      SetTextAlign(Handle, TA_LEFT or TA_TOP);
      TextOut(50, R.Top-50, 'F[N]');
      { Weg des Punkt antragen }
      if FPunktPos.x = 150 then
      begin
        SetTextAlign(Handle, TA_RIGHT or TA_TOP);
        TextOut(Round(FPunktPos.x*FZoomX)-50, -50, IntToStr(FPunktPos.x))
      end
      else
      begin
        SetTextAlign(Handle, TA_LEFT or TA_TOP);
        TextOut(Round(FPunktPos.x*FZoomX)+50, -50, IntToStr(FPunktPos.x));
      end;
      { Kraft des Punkt antragen }
      SetTextAlign(Handle, TA_RIGHT or TA_BOTTOM);
      TextOut(-50, Round(FPunktPos.y*FZoomY)+50, IntToStr(FPunktPos.y));

      { Kurven zeichnen }
      Pen.Color := clGreen;
      MoveTo(0, 0);
      LineTo(Round(KoppelFaktor*10000*SalingAlpha*FZoomX), Round(10000*FZoomY));

      { KnickKurven mit und ohne Controller, nicht korrigiert }
      Pen.Color := clBlue;
      DrawKurve(bmp.Canvas, FKurveOhne);
      DrawKurve(bmp.Canvas, FKurveMit);
      { KnickKurven mit und ohne Controller, korrigiert }
      Pen.Color := clGray;
      DrawKurveQuer(bmp.Canvas, FKurveOhneKorrigiert);
      DrawKurveQuer(bmp.Canvas, FKurveMitKorrigiert);

      { PunktKurveMitController - nur zeichnen, wenn Controller vorhanden }
      if ControllerTyp <> ctOhne then
      begin
        Pen.Color := clYellow;
        { blaue Kurve verschoben in SP mit FKurveOhne }
        if not Korrigiert then
          DrawKurve(bmp.Canvas, FVerschoben);
        { blaue Kurve verschoben in den SP mit FKurveOhneKorrigiert }
        if Korrigiert then
          DrawKurve(bmp.Canvas, FVerschobenKorrigiert);
      end;

      { PunktKurveOhneController }
      Pen.Color := clFuchsia;
      { überschreibe blaue Kurve mit Fuchsia }
      if not Korrigiert then
        DrawKurve(bmp.Canvas, FKurveOhne);
      { überschreibe graue Kurve mit Fuchsia }
      if Korrigiert then
        DrawKurveQuer(bmp.Canvas, FKurveOhneKorrigiert);

      SetMapMode(Handle, MM_TEXT);
      // SetWindowExtEx(Handle, 1, 1, nil);
      SetWindowOrgEx(Handle, 0, 0, nil);
      // SetWindowExtEx(Handle, 1, 1, nil);
      SetViewPortOrgEx(Handle, 0, 0, nil);
    end;

    with Canvas do
    begin
      CopyMode := cmSrcCopy;
      Draw(0, 0, bmp);
    end;

  finally
    bmp.Free;
  end;
end;

procedure TKraftGraph.DrawKurve(Canvas: TCanvas; Kurve: TLineDataR150);
var
  i: Integer;
  P: TPoint;
begin
  P.x := 0;
  P.y := 0;
  Canvas.MoveTo(P.x,P.y);
  for i := 0 to 150 do
  begin
    P.x := Round(i * FZoomX);
    P.y := Round(Kurve[i] * FZoomY);
    Canvas.LineTo(P.x,P.y);
  end;
end;

procedure TKraftGraph.DrawKurveQuer(Canvas: TCanvas; Kurve: TLineDataR100);
var
  i: Integer;
  P: TPoint;
begin
  Canvas.MoveTo(0,0);
  for i := 0 to 100 do
  begin
    P.x := Round(Kurve[i] * FZoomX);
    P.y := Round(i * 10);
    Canvas.LineTo(P.x,P.y);
  end;
end;

procedure TKraftGraph.DrawPunkt(Canvas: TCanvas; Punkt: TPoint);
var
  P: TPoint;
begin
  P.x := Round(Punkt.x * FZoomX);
  P.y := Round(Punkt.y * FZoomY);
  Canvas.Ellipse( P.x - 30, P.y - 30,
                  P.x + 30, P.y + 30);
end;

procedure TKraftGraph.GetTestKurven;
var
  i: Integer;
  tempHd: double;
  tempKorrigiert: Boolean;
  tempControllerTyp: TControllerTyp;
  Knicklaenge, KnickLast, Kraft, Weg: double;
begin
  with Mast do
  begin
    { mit FSalingAlpha wird in FvonW korrigiert, daher auch in WvonF gebraucht;
      mit FControllerWeg wird in SchnittKraefte getestet, ob Controller anliegt }
    GetControllerWeg; { FSalingAlpha und FControllerWeg }
    { mit FContollerAlpha wird in CalcWKnick die ControllerKraft ausgerechnet }
    GetSalingWeg; { FControllerAlpha und FSalingWeg }
    { mit SalingWegKnick wird in CalcWKnick KurvenTyp und Offset bestimmt }
    GetSalingWegKnick; { FSalingWegKnick }
    { FKurveOhne und FKurveMit }
    for i := 0 to 150 do
    begin
      FwSchnittOhne := i; { in mm }
      FSchnittPunktKraft := FvonW(FwSchnittOhne, TKurvenTyp.KurveOhneController, False);
      FKurveOhne[i] := FSchnittPunktKraft; { in N }
      FwSchnittMit := i; { in mm }
      FSchnittPunktKraft := FvonW(FwSchnittMit, TKurvenTyp.KurveMitController, False);
      FKurveMit[i] := FSchnittPunktKraft; { in N }
    end;
    { FKurveOhneKorrigiert }
    FKnicklaenge := lc;
    FXPos := ld;
    KnickLast := EI*3.14*3.14/FKnicklaenge/FKnicklaenge; { Knicklast in N }
    for i := 0 to 100 do
    begin
      Kraft := i * 100;
      if Kraft > 0.9 * KnickLast then
        FKurveOhneKorrigiert[i] := 150
      else begin
        Weg := WvonF(Kraft, TKurvenTyp.KurveOhneController, True);
        if Weg < 150 then
          FKurveOhneKorrigiert[i] := Weg
        else
          FKurveOhneKorrigiert[i] := 150;
      end;
    end;

    { FKurveMitKorrigiert }
    Knicklaenge := FKnicklaenge * FKorrekturFaktor;
    KnickLast := EI*3.14*3.14/Knicklaenge/Knicklaenge; { Knicklast in N }
    for i := 0 to 100 do
    begin
      Kraft := i*100;
      if Kraft > 0.9 * KnickLast then
        FKurveMitKorrigiert[i] := 150
      else
      begin
        Weg := WvonF(Kraft, TKurvenTyp.KurveMitController,True);
        if Weg < 150 then
          FKurveMitKorrigiert[i] := Weg
        else
          FKurveMitKorrigiert[i] := 150;
      end;
    end;

    tempHd := hd; { hd sichern }
    tempControllerTyp := ControllerTyp;
    tempKorrigiert := Korrigiert;

    for i := 0 to 150 do
    begin
      { FVerschoben }
      Korrigiert := False;
      ControllerTyp := ctZugDruck;
      hd := i;
      CalcWKnick;
      if MastOK or ShowAll then
        FVerschoben[i] := Round(-FC) { in N }
      else
        FVerschoben[i] := 0;

      { FVerschobenKorrigiert }
      Korrigiert := True;
      ControllerTyp := ctZugDruck;
      hd := i;
      CalcWKnick;
      if MastOK or ShowAll then
        FVerschobenKorrigiert[i] := Round(-FC) { in N }
      else
        FVerschobenKorrigiert[i] := 0;
    end;

    { hd und FKorrigiert restaurieren }
    hd := tempHd;
    Korrigiert := tempKorrigiert;
    ControllerTyp := tempControllerTyp;
    CalcWKnick;
  end; { with Mast do begin }
end;

procedure TKraftGraph.DrawPaintBoxK(Canvas: TCanvas; Rect: TRect);
begin
  FPunktPos.x := Round(Mast.hd); {in mm}
  FPunktPos.y := Round(-Mast.FC); {in N}
  Draw(Canvas, Rect);
end;

end.
