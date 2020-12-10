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
  RiggVar.App.Model;

type
  TFormReglerGraph = class(TForm)
    RegelPaintBox: TPaintBox;
    sbMastfall: TScrollBar;
    sbSpannung: TScrollBar;
    sbBiegung: TScrollBar;
    lbMastfall: TLabel;
    lbSpannung: TLabel;
    lbBiegung: TLabel;
    lbZaehler: TLabel;
    ZaehlerEdit: TEdit;
    LoopBtn: TButton;
    OK: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure sbMastfallScroll(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
    procedure LoopBtnClick(Sender: TObject);
  private
    TopTitel: string;
    LeftTitel: string;
    BottomTitel: string;
    RightTitel: string;
    Xmin: double;
    Xmax: double;
    Ymin: double;
    Ymax: double;
    XGap: double;
    YGap: double;
    f: TChartLine;
    TestF: TChartLine;
    procedure GetTestData;
    procedure GetData;
    procedure GetXMinMax;
    procedure GetYMinMax;
    function GetXText: String;
    function GetYText: String;
    procedure DrawToPaintBox(Canvas: TCanvas; Rect: TRect);
    procedure PaintBackGround(Image: TBitMap);
  private
    Rigg: TRigg;
    procedure SetupCtrls;
    procedure UpdateLabels;
    procedure UpdateWithCurrentValue;
  public
    Counter: Integer;
    TrimmIst: TTrimm;
    TrimmSoll: TTrimm;
  public
    ChartValid: Boolean;
    RGD: TRegelGraphData;
    procedure Draw(Sender: TObject);
  end;

var
  FormReglerGraph: TFormReglerGraph;

implementation

{$R *.DFM}

uses
  RiggVar.App.Main,
  RiggVar.RG.Def;

procedure TFormReglerGraph.FormCreate(Sender: TObject);
begin
  Rigg := Main.Rigg;
  SetupCtrls;
  GetTestData;
  Rigg.OnRegelGrafik := Draw;
  RGD := Rigg.RegelGraphData;
  ChartValid := True;
end;

procedure TFormReglerGraph.FormShow(Sender: TObject);
begin
  ZaehlerEdit.Text := '0';
  TrimmIst := Rigg.Trimm;
  UpdateWithCurrentValue;
end;

procedure TFormReglerGraph.SetupCtrls;
begin
  sbMastfall.SetParams(1100, 1000, 1300);
  sbSpannung.SetParams(800, 500, 1500);
  sbBiegung.SetParams(40, 10, 80);

  sbMastfall.SmallChange := 1;
  sbSpannung.SmallChange := 10;
  sbBiegung.SmallChange := 1;

  sbMastfall.LargeChange := 10;
  sbSpannung.LargeChange := 100;
  sbBiegung.LargeChange := 10;
end;

procedure TFormReglerGraph.UpdateLabels;
begin
  lbMastfall.Caption := Format('Mastfall = %d mm', [sbMastfall.Position]);
  lbSpannung.Caption := Format('Vorstagspannung = %d N', [sbSpannung.Position]);
  lbBiegung.Caption := Format('Mastbiegung = %d mm', [sbBiegung.Position]);
  ZaehlerEdit.Text := '0';
end;

procedure TFormReglerGraph.UpdateWithCurrentValue;
var
  v: Integer;
  mf: double;
  mfv: double;
begin
  mf := Main.ParamValue[fpMastfallF0F];
  mfv := Main.ParamValue[fpMastfallVorlauf];
  v := Round(mf - mfv);
  if (v > sbMastfall.Min) and (v < sbMastfall.Max) then
    sbMastfall.Position := v;

  v := Round(Main.ParamValue[fpBiegung]);
  if (v > sbBiegung.Min) and (v < sbBiegung.Max) then
    sbBiegung.Position := v;

  UpdateLabels;
end;

procedure TFormReglerGraph.LoopBtnClick(Sender: TObject);
begin
  TrimmSoll.Mastfall := sbMastfall.Position;
  TrimmSoll.Spannung := sbSpannung.Position;
  TrimmSoll.BiegungS := sbBiegung.Position;
  TrimmSoll.BiegungC := TrimmIst.BiegungC;
  TrimmSoll.Flexwert := TrimmIst.Flexwert;
  ZaehlerEdit.Text := '0';
  Screen.Cursor := crHourGlass;
  try
    Counter := Rigg.Regeln(TrimmSoll);
    if Rigg.GetriebeOK then
    begin
    { GCtrls werden nicht sofort aktualisiert. Deshalb sind die Einstellwerte
      für Mastfall und Biegung noch exakt. Die Wanten haben ungeradzahlige Längen.
      In UpdateRigg werden die Labels und die Graphic richtig aktualisiert.
      Die GCtrls werden erst nach Schließen des Dialogfensters aktualisiert.
      Gerundet auf geradzahlige Wantenwerte wird aber erst nach erneuter
      Berechnung des Getriebes, ausgelöst vom Benutzer }

//      RiggModul.DoGraphics;
//      RiggModul.UpdateRigg;

      Main.UpdateGetriebe;

    { Alternative: }
    { Die GCtrls werden sofort aktualisiert. Damit werden die Werte
      für die Wanten geradzahlig und die Einstellwerte für Mastfall und
      Biegung verändern sich. }

//      RiggModul.UpdateGCtrls(RiggModul.Rigg.Glieder);
//      RiggModul.UpdateGetriebe;

    end;
  finally
    if Counter = 20 then
      ZaehlerEdit.Text := 'Max'
    else
      ZaehlerEdit.Text := IntToStr(Counter);
    Screen.Cursor := crDefault;
  end;
end;

procedure TFormReglerGraph.sbMastfallScroll(Sender: TObject;
  ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
  if Sender = sbMastfall then
    lbMastfall.Caption := Format('Mastfall MF = %d mm', [ScrollPos])
  else if Sender = sbSpannung then
    lbSpannung.Caption := Format('Vorstagspannung = %d N', [ScrollPos])
  else if Sender = sbBiegung then
    lbBiegung.Caption := Format('Biegung Bie = %d mm', [ScrollPos]);

  if (Sender = sbSpannung)
//    and (ScrollCode = scEndScroll)
  then
    DrawToPaintBox(RegelPaintBox.Canvas, RegelPaintBox.BoundsRect);
end;

procedure TFormReglerGraph.FormPaint(Sender: TObject);
begin
  Draw(Self);
end;

procedure TFormReglerGraph.Draw(Sender: TObject);
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

  YGap := Round((Ymax - Ymin) / 10) + 1;
  if YGap = 0 then
    YGap := 0.2;
  XGap := Round((Xmax - Xmin) / 10) + 1;
  if XGap = 0 then
    XGap := 0.2;
  if (Ymax-Ymin < 1) then
  begin
    Ymin := Ymin - 1;
    Ymax := Ymax + 1;
  end;
  if (Xmax-Xmin < 1) then
  begin
    Xmin := Xmin - 1;
    Ymax := Ymax + 1;
  end;

  DrawToPaintBox(RegelPaintBox.Canvas, RegelPaintBox.BoundsRect);
end;

procedure TFormReglerGraph.DrawToPaintBox(Canvas: TCanvas; Rect: TRect);
var
  P: TPoint;
  i: Integer;
  ZoomX, ZoomY: Integer;
  temp, temp1, temp2: double;
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
      SetWindowExtEx(Handle, Round((Xmax-Xmin) * ZoomX), Round((Ymin-Ymax) * ZoomY), nil);
      SetWindowOrgEx(Handle, Round(Xmin * ZoomX), Round(Ymin*ZoomY), nil);
      SetViewPortExtEx(Handle, Bitmap.Width, Bitmap.Height, nil);
      SetViewPortOrgEx(Handle, 0, Bitmap.Height, nil);

      { Kurve }
      Pen.Color := clBlue;
      temp := (Xmin + (CLMax - 2) / CLMax * (Xmax - Xmin));
      P.X := Round(temp*ZoomX);
      P.Y := Round(f[CLMax - 2] * ZoomY);
      MoveTo(P.X, P.Y);
      for i := CLMax - 2 downto 6 do
      begin
        P.X := Round((Xmin + (Xmax-Xmin) * i / CLMax) * ZoomX);
        if f[i] > 5000 then
        begin
          f[i] := 5000; { Overflow verhindern: 5000 * ZoomY > 32000 }
          Pen.Color := clSilver;
        end;
        P.Y := Round(f[i] * ZoomY);
        LineTo(P.X, P.Y);
      end;

      { Maßstab Kraft }
      temp1 := (Xmin + 3 / CLMax * (Xmax - Xmin));
      temp2 := (Xmin + 6 / CLMax * (Xmax - Xmin));
      Pen.Color := clBlack;
      P.X := Round(temp1 * ZoomX);
      P.Y := Round(500 * ZoomY);
      MoveTo(P.X, P.Y);
      P.X := Round(temp2 * ZoomX);
      LineTo(P.X, P.Y);
      P.X := Round(temp1 * ZoomX);
      P.Y := Round(1000 * ZoomY);
      MoveTo(P.X,P.Y);
      P.X := Round(temp2 * ZoomX);
      LineTo(P.X, P.Y);
      P.Y := Round(temp1 * ZoomX);
      P.Y := Round(1500 * ZoomY);
      MoveTo(P.X, P.Y);
      P.X := Round(temp2*ZoomX);
      LineTo(P.X, P.Y);

      { Sollwert Kraft }
      Pen.Color := clRed;
      P.X := Round(Xmin*ZoomX);
      P.Y := Round(sbSpannung.Position*ZoomY);
      MoveTo(P.X, P.Y);
      P.X := Round(Xmax*ZoomX); { P.Y bleibt unverändert }
      LineTo(P.X, P.Y);

      { Istwert SalingH/SalingL - Scheibenwischer-Linie }
      temp := RGD.Antrieb;
      Pen.Color := clGreen;
      P.X := Round(temp * ZoomX);
      P.Y := Round(Ymin * ZoomY);
      MoveTo(P.X, P.Y);
      P.X := Round(temp * ZoomX);
      P.Y := Round(Ymax * ZoomY);
      LineTo(P.X, P.Y);

      { Y-Achse }
      temp := (Xmin + 6 / CLMax * (Xmax - Xmin));
      Pen.Color := clBlack;
      P.X := Round(temp*ZoomX);
      P.Y := Round(Ymin*ZoomY);
      MoveTo(P.X, P.Y);
      P.X := Round(temp*ZoomX);
      P.Y := Round(Ymax*ZoomY);
      LineTo(P.X, P.Y);

      { TrySalingH }
      temp := RGD.TrySalingH;
      Pen.Color := clRed;
      P.X := Round(temp*ZoomX);
      P.Y := Round(Ymin*ZoomY);
      MoveTo(P.X, P.Y);
      P.X := Round(temp*ZoomX);
      P.Y := Round(Ymax*ZoomY);
      LineTo(P.X, P.Y);

      (*
      {limitA}
      temp := Rigg.limitA;
      Pen.Color := clRed;
      P.X := Round(temp * ZoomX);
      P.Y := Round(Ymin * ZoomY);
      MoveTo(P.X, P.Y);
      P.X := Round(temp * ZoomX);
      P.Y := Round(Ymax * ZoomY);
      LineTo(P.X, P.Y);

      { limitB }
      temp := Rigg.limitB;
      Pen.Color := clRed;
      P.X := Round(temp * ZoomX);
      P.Y := Round(Ymin * ZoomY);
      MoveTo(P.X, P.Y);
      P.X := Round(temp * ZoomX);
      P.Y := Round(Ymax * ZoomY);
      LineTo(P.X, P.Y);
      *)

      (*
      { aktueller Punkt }
      R.Left := 0;
      R.Top := 0;
      R.Bottom := 5;
      R.Right := 5;
      DPTOLP(Handle, R, 2);
      RadiusX := R.Right - R.Left;
      RadiusY := R.Bottom - R.Top;
      Brush.Color := PunktColor;
      Brush.Style := bsSolid;
      P.X := Round(ChartPunktX) * ZoomX;
      P.Y := Round(ChartPunktY) * ZoomY;
      Ellipse(P.X - RadiusX, P.Y - RadiusY,
              P.Y + RadiusX, P.Y + RadiusY);
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

function TFormReglerGraph.GetXText: String;
var
  s: string;
begin
  case Rigg.SalingTyp of
    stFest: s := 'Saling-Höhe [mm]';
    stDrehbar: s := 'Saling-Länge [mm]';
  end;
  result := s;
end;

function TFormReglerGraph.GetYText: String;
var
  s: string;
begin
  s := 'Vorstag-Spannung [N]';
  result := s;
end;

procedure TFormReglerGraph.GetYMinMax;
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

procedure TFormReglerGraph.GetXMinMax;
begin
  Xmin := RGD.Anfang;
  Xmax := RGD.Ende;
end;

procedure TFormReglerGraph.GetTestData;
var
  i: Integer;
begin
  for i := 0 to CLMax do
    f[i] := 100 * i/CLMax;
  TestF := f;
end;

procedure TFormReglerGraph.GetData;
begin
  ChartValid := True;
  f := RGD.KurveF;
end;

procedure TFormReglerGraph.PaintBackGround(Image: TBitMap);
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

end.

