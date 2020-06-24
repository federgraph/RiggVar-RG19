unit RggKraftGraph;

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
    procedure DrawGraph(g: TCanvas);
    procedure DrawPunkt(Canvas: TCanvas; Punkt: TPoint);
    procedure DrawKurve(Canvas: TCanvas; Kurve: TLineDataR150);
    procedure DrawKurveQuer(Canvas: TCanvas; Kurve: TLineDataR100);
    function GetKoppelFaktor: double;
    function GetSalingAlpha: double;
    function GetKorrigiert: Boolean;
    function GetControllerTyp: TControllerTyp;
    procedure InitBitmap;
    procedure PaintBackGround(g: TCanvas);
  protected
    Bitmap: TBitmap;
    Width: Integer;
    Height: Integer;
  public
    Mast: TMast;
    constructor Create(AMast: TMast); virtual;
    destructor Destroy; override;
    procedure Draw; override;
    procedure GetTestKurven; override;
    property KoppelFaktor: double read GetKoppelFaktor;
    property SalingAlpha: double read GetSalingAlpha;
    property Korrigiert: Boolean read GetKorrigiert;
    property ControllerTyp: TControllerTyp read GetControllerTyp;
//    property Image: TImage read FImage write SetImage;
  end;

implementation

constructor TKraftGraph.Create(AMast: TMast);
begin
  inherited Create;
  if not Assigned(AMast) then
  begin
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

destructor TKraftGraph.Destroy;
begin
  Bitmap.Free;
  inherited;
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

procedure TKraftGraph.PaintBackGround(g: TCanvas);
var
  R: TRect;
begin
  R := Rect(0, 0, Width, Height);
  g.Brush.Color := clBtnFace;
  g.FillRect(R);
end;

procedure TKraftGraph.DrawGraph(g: TCanvas);
var
  Pos: TPoint;
  R: TRect;
begin
  Pos.X := Width div 2;
  Pos.Y := Height div 2;

  SetMapMode(g.Handle, MM_ISOTROPIC);
  SetWindowExtEx(g.Handle, 2100, -2100, nil);
  SetWindowOrgEx(g.Handle, FOffsetX, FOffsetY, nil);
  SetViewPortExtEx(g.Handle, Width, Height, nil);
  SetViewPortOrgEx(g.Handle, Pos.X, Pos.Y, nil);

  { Koordinaten des Rahmenrechtecks }
  with R do
  begin
    Left := -1500 + FOffsetX;
    Right := 1500 + FOffsetX;
    Top := 1000 + FOffsetY;
    Bottom := -1000 + FOffsetY;
  end;

  { Rahmen zeichnen und Clipping setzen }
  g.Pen.Color := clBlue;
  g.Brush.Style := bsClear;
  g.Rectangle( R.Left, R.Top, R.Right, R.Bottom);
  IntersectClipRect(g.Handle, R.Left, R.Top, R.Right, R.Bottom);

  { Achsenkreuz }
  g.Pen.Color := clBlack;
  g.MoveTo(R.Left, 0);
  g.LineTo(R.Right, 0);
  g.MoveTo(0,R.Top);
  g.LineTo(0,R.Bottom);

  { Punkte }
  g.Pen.Color := clBlue;
  g.Brush.Style := bsSolid;
  g.Brush.Color := clRed;
  { Nullpunkt }
  DrawPunkt(g, Point(0,0));
  g.Brush.Color := clAqua;
  { aktueller Punkt }
  DrawPunkt(g, FPunktPos);

  { Maßlinien beim aktuellen Punkt }
  g.Pen.Color := clBlack;
  g.MoveTo(-50, Round(FPunktPos.Y * FZoomY));
  g.LineTo(  0, Round(FPunktPos.Y * FZoomY));
  g.MoveTo(Round(FPunktPos.X * FZoomX), 0);
  g.LineTo(Round(FPunktPos.X * FZoomX), -50);

  { Beschriftung }
  g.Font.Height := 12 * 10;
  g.Font.Color := clBlue;
  g.Brush.Color := clSilver;
  SetTextAlign(g.Handle, TA_RIGHT or TA_BOTTOM);
  g.TextOut(R.Right - 50, 50, 'hd[mm]');
  SetTextAlign(g.Handle, TA_LEFT or TA_TOP);
  g.TextOut(50, R.Top - 50, 'F[N]');
  { Weg des Punkt antragen }
  if FPunktPos.x = 150 then
  begin
    SetTextAlign(g.Handle, TA_RIGHT or TA_TOP);
    g.TextOut(Round(FPunktPos.X * FZoomX) - 50, -50, IntToStr(FPunktPos.x))
  end
  else
  begin
    SetTextAlign(g.Handle, TA_LEFT or TA_TOP);
    g.TextOut(Round(FPunktPos.X * FZoomX) + 50, -50, IntToStr(FPunktPos.x));
  end;
  { Kraft des Punkt antragen }
  SetTextAlign(g.Handle, TA_RIGHT or TA_BOTTOM);
  g.TextOut(-50, Round(FPunktPos.Y * FZoomY) + 50, IntToStr(FPunktPos.y));

  { Kurven zeichnen }
  g.Pen.Color := clGreen;
  g.MoveTo(0, 0);
  g.LineTo(Round(KoppelFaktor * 10000 * SalingAlpha * FZoomX), Round(10000 * FZoomY));

  { KnickKurven mit und ohne Controller, nicht korrigiert }
  g.Pen.Color := clBlue;
  DrawKurve(g, FKurveOhne);
  DrawKurve(g, FKurveMit);
  { KnickKurven mit und ohne Controller, korrigiert }
  g.Pen.Color := clGray;
  DrawKurveQuer(g, FKurveOhneKorrigiert);
  DrawKurveQuer(g, FKurveMitKorrigiert);

  { PunktKurveMitController - nur zeichnen, wenn Controller vorhanden }
  if ControllerTyp <> ctOhne then
  begin
    g.Pen.Color := clYellow;
    { blaue Kurve verschoben in SP mit FKurveOhne }
    if not Korrigiert then
      DrawKurve(g, FVerschoben);
    { blaue Kurve verschoben in den SP mit FKurveOhneKorrigiert }
    if Korrigiert then
      DrawKurve(g, FVerschobenKorrigiert);
  end;

  { PunktKurveOhneController }
  g.Pen.Color := clFuchsia;
  { überschreibe blaue Kurve mit Fuchsia }
  if not Korrigiert then
    DrawKurve(g, FKurveOhne);
  { überschreibe graue Kurve mit Fuchsia }
  if Korrigiert then
    DrawKurveQuer(g, FKurveOhneKorrigiert);

  SetMapMode(g.Handle, MM_TEXT);
  SetWindowOrgEx(g.Handle, 0, 0, nil);
  SetViewPortOrgEx(g.Handle, 0, 0, nil);
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
  P.X := Round(Punkt.X * FZoomX);
  P.Y := Round(Punkt.Y * FZoomY);
  Canvas.Ellipse( P.X - 30, P.Y - 30,
                  P.X + 30, P.Y + 30);
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

procedure TKraftGraph.InitBitmap;
begin
//  FImage := Value;
  Width := Image.Width;
  Height := Image.Height;

  Bitmap := TBitmap.Create;
  Bitmap.Width := Width;
  Bitmap.Height := Height;
end;

procedure TKraftGraph.Draw;
begin
  if Image = nil then
    Exit;

  if Bitmap = nil then
    InitBitmap;

  PaintBackGround(Bitmap.Canvas);

  FPunktPos.X := Round(Mast.hd); {in mm}
  FPunktPos.Y := Round(-Mast.FC); {in N}

  DrawGraph(Bitmap.Canvas);

  Image.Canvas.CopyMode := cmSrcCopy;
  Image.Canvas.Draw(0, 0, Bitmap);
end;

end.
