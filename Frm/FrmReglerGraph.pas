unit FrmReglerGraph;

interface

uses
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.StdCtrls,
  Vcl.Buttons,
  Vcl.ExtCtrls,
  RggTypes,
  FrmRegler;

type
  TCtrlDlg1 = class(TCtrlDlg)
    RegelPaintBox: TPaintBox;
    Bevel1: TBevel;
    procedure FormPaint(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure sbMastfallScroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);
  private
    TopTitel, LeftTitel, BottomTitel, RightTitel: string;
    Xmin, Xmax, Ymin, Ymax, XGap, YGap: real;
    {ChartPunktX, ChartPunktY: real;}
    f, TestF: TChartLine;
    procedure GetTestData;
    procedure GetData;
    procedure GetXMinMax;
    procedure GetYMinMax;
    function GetXText: String;
    function GetYText: String;
    procedure DrawToPaintBox(Canvas: TCanvas; Rect: TRect);
    procedure PaintBackGround(Image: TBitMap);
  public
    ChartValid: Boolean;
    procedure Draw(Sender: TObject);
  end;

var
  CtrlDlg1: TCtrlDlg1;

implementation

uses RiggUnit;

{$R *.DFM}

procedure TCtrlDlg1.FormCreate(Sender: TObject);
begin
  inherited;
  GetTestData;
  RiggModul.Rigg.OnRegelGrafik := Draw;
  ChartValid := True;
end;

procedure TCtrlDlg1.FormPaint(Sender: TObject);
begin
  inherited;
  Draw(Self);
end;

procedure TCtrlDlg1.Draw(Sender: TObject);
begin
  if ChartValid then
  begin
    LeftTitel := GetYText;
    BottomTitel := GetXText;
    GetData;
    GetXMinMax;
    GetYMinMax;
  end
  else
  begin
    TopTitel := '';
    LeftTitel := 'Funktionswerte';
    BottomTitel := 'Argumente';
    RightTitel := '';
    Xmin := 0;
    Xmax := 100;
    Ymin := 0;
    Ymax := 100;
    f := TestF;
  end;

  YGap := Round((Ymax-Ymin)/10)+1;
  if YGap = 0 then
    YGap := 0.2;
  XGap := Round((Xmax-Xmin)/10)+1;
  if XGap = 0 then
    XGap := 0.2;
  if (Ymax-Ymin < 1) then
  begin
    Ymin := Ymin-1;
    Ymax := Ymax+1;
  end;
  if (Xmax-Xmin < 1) then
  begin
    Xmin := Xmin-1;
    Ymax := Ymax+1;
  end;

  DrawToPaintBox(RegelPaintBox.Canvas, RegelPaintBox.BoundsRect);
end;

procedure TCtrlDlg1.DrawToPaintBox(Canvas: TCanvas; Rect: TRect);
var
  P: TPoint;
  i: Integer;
  ZoomX, ZoomY: Integer;
  temp, temp1, temp2: real;
  Bitmap: TBitmap;
begin
  ZoomX := 10;
  ZoomY := 5;

  Bitmap := TBitmap.Create;
  with Bitmap do
  begin
    Width := Rect.Right - Rect.Left;
    Height := Rect.Bottom - Rect.Top;
  end;
  try
    PaintBackGround(Bitmap);

    with Bitmap.Canvas do
    begin
      SetMapMode(Handle, MM_ANISOTROPIC);
      SetWindowExtEx(Handle, Round((Xmax-Xmin)*ZoomX), Round((Ymin-Ymax)*ZoomY), nil);
      SetWindowOrgEx(Handle, Round(Xmin*ZoomX), Round(Ymin*ZoomY), nil);
      SetViewPortExtEx(Handle, Bitmap.Width, Bitmap.Height, nil);
      SetViewPortOrgEx(Handle, 0, Bitmap.Height, nil);

      { Kurve }
      Pen.Color := clBlue;
      temp := (Xmin + (CLMax-2)/CLMax*(Xmax-Xmin));
      P.x := Round(temp*ZoomX);
      P.y := Round(f[CLMax-2]*ZoomY);
      MoveTo(P.x,P.y);
      for i := CLMax-2 downto 6 do
      begin
        P.x := Round( ( Xmin + (Xmax-Xmin)*i/CLMax )*ZoomX );
        if f[i] > 5000 then
        begin
          f[i] := 5000; { Overflow verhindern: 5000 * ZoomY > 32000 }
          Pen.Color := clSilver;
        end;
        P.y := Round(f[i]*ZoomY);
        LineTo(P.x, P.y);
      end;

      { Maßstab Kraft }
      temp1 := (Xmin + 3/CLMax*(Xmax-Xmin));
      temp2 := (Xmin + 6/CLMax*(Xmax-Xmin));
      Pen.Color := clBlack;
      P.x := Round(temp1*ZoomX); P.y := Round(500*ZoomY);
      MoveTo(P.x,P.y); P.x := Round(temp2*ZoomX); LineTo(P.x, P.y);
      P.x := Round(temp1*ZoomX); P.y := Round(1000*ZoomY);
      MoveTo(P.x,P.y); P.x := Round(temp2*ZoomX); LineTo(P.x, P.y);
      P.x := Round(temp1*ZoomX); P.y := Round(1500*ZoomY);
      MoveTo(P.x,P.y); P.x := Round(temp2*ZoomX); LineTo(P.x, P.y);

      { Sollwert Kraft }
      Pen.Color := clRed;
      P.x := Round(Xmin*ZoomX);
      P.y := Round(sbSpannung.Position*ZoomY);
      MoveTo(P.x,P.y);
      P.x := Round(Xmax*ZoomX); { P.y bleibt unverändert }
      LineTo(P.x, P.y);

      { Istwert SalingH/SalingL - Scheibenwischer-Linie }
      temp := RiggModul.Rigg.Antrieb;
      Pen.Color := clGreen;
      P.x := Round(temp*ZoomX);
      P.y := Round(Ymin*ZoomY);
      MoveTo(P.x,P.y);
      P.x := Round(temp*ZoomX);
      P.y := Round(Ymax*ZoomY);
      LineTo(P.x, P.y);

      { Y-Achse }
      temp := (Xmin + 6/CLMax*(Xmax-Xmin));
      Pen.Color := clBlack;
      P.x := Round(temp*ZoomX);
      P.y := Round(Ymin*ZoomY);
      MoveTo(P.x,P.y);
      P.x := Round(temp*ZoomX);
      P.y := Round(Ymax*ZoomY);
      LineTo(P.x, P.y);

      { TrySalingH }
      temp := RiggModul.Rigg.TrySalingH;
      Pen.Color := clRed;
      P.x := Round(temp*ZoomX);
      P.y := Round(Ymin*ZoomY);
      MoveTo(P.x,P.y);
      P.x := Round(temp*ZoomX);
      P.y := Round(Ymax*ZoomY);
      LineTo(P.x, P.y);

      (*
      { limitA }
      temp := RiggModul.Rigg.limitA;
      Pen.Color := clRed;
      P.x := Round(temp*ZoomX);
      P.y := Round(Ymin*ZoomY);
      MoveTo(P.x,P.y);
      P.x := Round(temp*ZoomX);
      P.y := Round(Ymax*ZoomY);
      LineTo(P.x, P.y);

      { limitB }
      temp := RiggModul.Rigg.limitB;
      Pen.Color := clRed;
      P.x := Round(temp*ZoomX);
      P.y := Round(Ymin*ZoomY);
      MoveTo(P.x,P.y);
      P.x := Round(temp*ZoomX);
      P.y := Round(Ymax*ZoomY);
      LineTo(P.x, P.y);
      *)

      (*
      { aktueller Punkt }
      R.Left := 0; R.Top := 0; R.Bottom := 5; R.Right := 5;
      DPTOLP(Handle, R, 2);
      RadiusX := R.Right-R.Left; RadiusY := R.Bottom-R.Top;
      Brush.Color := PunktColor;
      Brush.Style := bsSolid;
      P.x := Round(ChartPunktX) * ZoomX;
      P.y := Round(ChartPunktY) * ZoomY;
      Ellipse( P.x - RadiusX, P.y - RadiusY,
               P.x + RadiusX, P.y + RadiusY);
      *)

      SetMapMode(Handle, MM_TEXT);
      SetWindowOrgEx(Handle, 0, 0, nil);
      SetViewPortOrgEx(Handle, 0, 0, nil);

      { Rahmen zeichnen }
      Pen.Width := 1;
      Pen.Color := clBlack;
      Brush.Style := bsClear;
      Rectangle( 0, 0, Bitmap.Width, Bitmap.Height);
    end;

    with Canvas do
    begin
      CopyMode := cmSrcCopy;
      Draw(0, 0, BitMap);
    end;

  finally
    Bitmap.Free;
  end;
  (*
  lbAchseX.Caption := BottomTitel;
  lbAchseY.Caption := Lefttitel;
  lbXLeft.Caption := IntToStr(Round(Xmin));
  lbXRight.Caption := IntToStr(Round(Xmax));
  lbYBottom.Caption := IntToStr(Round(Ymin));
  lbYTop.Caption := IntToStr(Round(Ymax));
  *)
end;

function TCtrlDlg1.GetXText: String;
var
  S: String;
begin
  case RiggModul.Rigg.SalingTyp of
    stFest: S := 'Saling-Höhe [mm]';
    stDrehbar: S := 'Saling-Länge [mm]';
  end;
  Result := S;
end;

function TCtrlDlg1.GetYText: String;
var
  S: String;
begin
  S := 'Vorstag-Spannung [N]';
  Result := S;
end;

procedure TCtrlDlg1.GetYMinMax;
var
  i: Integer;
begin
  Ymax := f[0];
  Ymin := Ymax;
  for i := 0 to CLMax do
  begin
    if f[i] > Ymax then
      Ymax := f[i];
    if f[i] < Ymin then
      Ymin := f[i];
  end;
end;

procedure TCtrlDlg1.GetXMinMax;
begin
  Xmin := RiggModul.Rigg.Anfang;
  Xmax := RiggModul.Rigg.Ende;
end;

procedure TCtrlDlg1.GetTestData;
var
  i: Integer;
begin
  for i := 0 to CLMax do f[i] := 100 * i/CLMax;
  TestF := f;
end;

procedure TCtrlDlg1.GetData;
begin
  ChartValid := True;
  f := RiggModul.Rigg.KurveF;
end;

procedure TCtrlDlg1.PaintBackGround(Image: TBitMap);
var
  R: TRect;
begin
  R := Rect(0, 0, Image.Width, Image.Height);
  with Image.Canvas do
  begin
    Brush.Color := clWhite;
    FillRect(R);
  end;
end;

procedure TCtrlDlg1.sbMastfallScroll(Sender: TObject;
  ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
  inherited;
  if (ScrollCode = scEndScroll) and (Sender = sbSpannung) then
    DrawToPaintBox(RegelPaintBox.Canvas, RegelPaintBox.BoundsRect);
end;

end.

