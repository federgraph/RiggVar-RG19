unit RggKraftGraph;

interface

{$SCOPEDENUMS ON}

uses
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  System.Types,
  Vcl.Graphics,
  Vcl.ExtCtrls,
  RggTypes;

type
  TKraftGraph = class
  private
    FShowAll: Boolean;
    FOffsetX, FOffsetY: Integer;
    FZoomX, FZoomY: double;
    FPunktPos: TPoint;
    procedure DrawGraph(g: TCanvas);
    procedure DrawPunkt(Canvas: TCanvas; Punkt: TPoint);
    procedure DrawKurve(Canvas: TCanvas; Kurve: TLineDataR150);
    procedure DrawKurveQuer(Canvas: TCanvas; Kurve: TLineDataR100);
    procedure InitBitmap;
    procedure PaintBackGround(g: TCanvas);
    procedure SetImage(const Value: TImage);
    procedure SetShowAll(const Value: Boolean);
  protected
    FImage: TImage;
    Bitmap: TBitmap;
    Width: Integer;
    Height: Integer;
  public
    Mast: TKraftKurven;
    constructor Create(AMast: TKraftKurven); virtual;
    destructor Destroy; override;
    procedure Draw;
    property Image: TImage read FImage write SetImage;
    property ShowAll: Boolean read FShowAll write SetShowAll;
  end;

implementation

{ TKraftGraph }

constructor TKraftGraph.Create(AMast: TKraftKurven);
begin
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

procedure TKraftGraph.PaintBackGround(g: TCanvas);
var
  R: TRect;
begin
  R := Rect(0, 0, Width, Height);
  g.Brush.Color := clBtnFace;
  g.FillRect(R);
end;

procedure TKraftGraph.SetImage(const Value: TImage);
begin
  FImage := Value;
end;

procedure TKraftGraph.SetShowAll(const Value: Boolean);
begin
  FShowAll := Value;
  Mast.ShowAll := Value;
end;

procedure TKraftGraph.InitBitmap;
begin
  Width := Image.Width;
  Height := Image.Height;

  Bitmap := TBitmap.Create;
  Bitmap.Width := Width;
  Bitmap.Height := Height;
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
  R.Left := -1500 + FOffsetX;
  R.Right := 1500 + FOffsetX;
  R.Top := 1000 + FOffsetY;
  R.Bottom := -1000 + FOffsetY;

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
  if FPunktPos.X = 150 then
  begin
    SetTextAlign(g.Handle, TA_RIGHT or TA_TOP);
    g.TextOut(Round(FPunktPos.X * FZoomX) - 50, -50, IntToStr(FPunktPos.X))
  end
  else
  begin
    SetTextAlign(g.Handle, TA_LEFT or TA_TOP);
    g.TextOut(Round(FPunktPos.X * FZoomX) + 50, -50, IntToStr(FPunktPos.X));
  end;
  { Kraft des Punkt antragen }
  SetTextAlign(g.Handle, TA_RIGHT or TA_BOTTOM);
  g.TextOut(-50, Round(FPunktPos.Y * FZoomY) + 50, IntToStr(FPunktPos.Y));

  { Kurven zeichnen }
  g.Pen.Color := clGreen;
  g.MoveTo(0, 0);
  g.LineTo(Round(Mast.KoppelFaktor * 10000 * Mast.SalingAlpha * FZoomX), Round(10000 * FZoomY));

  { KnickKurven mit und ohne Controller, nicht korrigiert }
  g.Pen.Color := clBlue;
  DrawKurve(g, Mast.KurveOhne);
  DrawKurve(g, Mast.KurveMit);
  { KnickKurven mit und ohne Controller, korrigiert }
  g.Pen.Color := clGray;
  DrawKurveQuer(g, Mast.KurveOhneKorrigiert);
  DrawKurveQuer(g, Mast.KurveMitKorrigiert);

  { PunktKurveMitController - nur zeichnen, wenn Controller vorhanden }
  if Mast.ControllerTyp <> ctOhne then
  begin
    g.Pen.Color := clYellow;
    { blaue Kurve verschoben in Schnittpunkt mit FKurveOhne }
    if not Mast.Korrigiert then
      DrawKurve(g, Mast.KurveVerschoben);
    { blaue Kurve verschoben in den Schnittpunkt mit FKurveOhneKorrigiert }
    if Mast.Korrigiert then
      DrawKurve(g, Mast.KurveVerschobenKorrigiert);
  end;

  { PunktKurveOhneController }
  g.Pen.Color := clFuchsia;
  { überschreibe blaue Kurve mit Fuchsia }
  if not Mast.Korrigiert then
    DrawKurve(g, Mast.KurveOhne);
  { überschreibe graue Kurve mit Fuchsia }
  if Mast.Korrigiert then
    DrawKurveQuer(g, Mast.KurveOhneKorrigiert);

  SetMapMode(g.Handle, MM_TEXT);
  SetWindowOrgEx(g.Handle, 0, 0, nil);
  SetViewPortOrgEx(g.Handle, 0, 0, nil);
end;

procedure TKraftGraph.DrawKurve(Canvas: TCanvas; Kurve: TLineDataR150);
var
  i: Integer;
  P: TPoint;
begin
  P.X := 0;
  P.Y := 0;
  Canvas.MoveTo(P.X, P.Y);
  for i := 0 to 150 do
  begin
    P.X := Round(i * FZoomX);
    P.Y := Round(Kurve[i] * FZoomY);
    Canvas.LineTo(P.X, P.Y);
  end;
end;

procedure TKraftGraph.DrawKurveQuer(Canvas: TCanvas; Kurve: TLineDataR100);
var
  i: Integer;
  P: TPoint;
begin
  Canvas.MoveTo(0, 0);
  for i := 0 to 100 do
  begin
    P.X := Round(Kurve[i] * FZoomX);
    P.Y := Round(i * 10);
    Canvas.LineTo(P.X, P.Y);
  end;
end;

procedure TKraftGraph.DrawPunkt(Canvas: TCanvas; Punkt: TPoint);
var
  P: TPoint;
  R: Integer;
begin
  P.X := Round(Punkt.X * FZoomX);
  P.Y := Round(Punkt.Y * FZoomY);
  R := 30;
  Canvas.Ellipse( P.X - R, P.Y - R, P.X + R, P.Y + R);
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
