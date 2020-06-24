unit RggMastGraph;

interface

uses
  Winapi.Windows,
  System.Types,
  System.UITypes,
  RggTypes,
  RggUnit2,
  Vcl.Graphics,
  Vcl.ExtCtrls;

type
  TRggMastGraph = class(TMastGraph0)
  private
    procedure PaintBackGround(g: TCanvas);
  public
    Bitmap: TBitmap;
    Width: Integer;
    Height: Integer;
    constructor Create;
    destructor Destroy; override;
    procedure DrawMastLine(g: TCanvas);
    procedure Draw; override;
  end;

implementation

procedure TRggMastGraph.PaintBackGround(g: TCanvas);
var
  R: TRect;
begin
  R := Rect(0, 0, Width, Height);
  g.Brush.Color := clBtnFace;
  g.FillRect(R);
end;

procedure TRggMastGraph.DrawMastLine(g: TCanvas);
var
  Pos: TPoint;
  min, max, Mitte: double;
  i: Integer;
  PlotLine: Linie;
  StraightLine: Boolean;
begin
  { Skalieren:  PlotLine soll Integerbereich gut ausfüllen.
    Es ist garantiert, daß Anfangs- und Endpunkt der Linie
    Null sind }
  StraightLine := False;
  max := LineData[0];
  min := max;
  for i := 0 to FLineCountM do
  begin
    if LineData[i] > max then
      max := LineData[i];
    if LineData[i] < min then
      min := LineData[i];
  end;
  if max = min then
    StraightLine := True
  else
  begin
    Mitte := abs(max-min)/2 + min;
    for i := 0 to FLineCountM do
    begin
      PlotLine[i].x := Round( 1000 * (LineData[i]- Mitte)/ abs(max-min) ) + 1000;
      PlotLine[i].y := 20 * i; { 0 .. 2000 }
    end;
  end;

  Pos.X := Width div 2;
  Pos.Y := Height div 2;

  SetMapMode(g.Handle, MM_ANISOTROPIC);
  SetWindowExtEx(g.Handle, 2200, -2200, nil);
  SetWindowOrgEx(g.Handle, 1000, 1000, nil);
  SetViewPortExtEx(g.Handle, Width, Height, nil);
  SetViewPortOrgEx(g.Handle, Pos.X, Pos.Y, nil);

  { Mastbiegekurve zeichnen }
  g.Pen.Color := clBlue;
  if StraightLine or not GetriebeOK then
  begin
    g.MoveTo(1000, 0);
    g.LineTo(1000, 2000);
  end
  else
    g.PolyLine(PlotLine);

  SetMapMode(g.Handle, MM_TEXT);
  SetWindowOrgEx(g.Handle, 0, 0, nil);
  SetViewPortOrgEx(g.Handle, 0, 0, nil);
end;

constructor TRggMastGraph.Create;
begin
  Width := 60;
  Height := 163;

  Bitmap := TBitmap.Create;
  Bitmap.Width := Width;
  Bitmap.Height := Height;
end;

destructor TRggMastGraph.Destroy;
begin
  Bitmap.Free;
  inherited;
end;

procedure TRggMastGraph.Draw;
begin
  if Image = nil then
    Exit;

  PaintBackGround(Bitmap.Canvas);

  DrawMastLine(Bitmap.Canvas);

  Image.Canvas.CopyMode := cmSrcCopy;
  Image.Canvas.Draw(0, 0, Bitmap);
end;

end.
