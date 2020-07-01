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
  RggUnit4;

type
  TFormReglerGraph = class(TForm)
    RegelPaintBox: TPaintBox;
    lbMastfall: TLabel;
    lbSpannung: TLabel;
    lbBiegungS: TLabel;
    lbZaehler: TLabel;
    sbMastfall: TScrollBar;
    sbSpannung: TScrollBar;
    sbBiegungS: TScrollBar;
    ZaehlerEdit: TEdit;
    LoopBtn: TBitBtn;
    OK: TBitBtn;
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
  public
    Counter: Integer;
    TrimmIst: TTrimm;
    TrimmSoll: TTrimm;
  public
    ChartValid: Boolean;
    procedure Draw(Sender: TObject);
  end;

var
  FormReglerGraph: TFormReglerGraph;

implementation

uses
  RiggVar.App.Main;

{$R *.DFM}

procedure TFormReglerGraph.FormCreate(Sender: TObject);
begin
  SetupCtrls;
  GetTestData;
  Rigg := Main.Rigg;
  Rigg.OnRegelGrafik := Draw;
  ChartValid := True;
end;

procedure TFormReglerGraph.FormShow(Sender: TObject);
begin
  ZaehlerEdit.Text := '0';
  TrimmIst := Rigg.Trimm;
end;

procedure TFormReglerGraph.SetupCtrls;
begin
  sbMastfall.SetParams(1100, 1000, 1300);
  sbSpannung.SetParams(800, 500, 1500);
  sbBiegungS.SetParams(40, 10, 80);

  sbMastfall.SmallChange := 1;
  sbSpannung.SmallChange := 10;
  sbBiegungS.SmallChange := 1;

  sbMastfall.LargeChange := 10;
  sbSpannung.LargeChange := 100;
  sbBiegungS.LargeChange := 10;

  lbMastfall.Caption := Format('Mastfall = %d mm', [sbMastfall.Position]);
  lbSpannung.Caption := Format('Vorstagspannung = %d N', [sbSpannung.Position]);
  lbBiegungS.Caption := Format('Mastbiegung = %d mm', [sbBiegungS.Position]);
  ZaehlerEdit.Text := '0';
end;

procedure TFormReglerGraph.LoopBtnClick(Sender: TObject);
begin
  TrimmSoll.Mastfall := sbMastfall.Position;
  TrimmSoll.Spannung := sbSpannung.Position;
  TrimmSoll.BiegungS := sbBiegungS.Position;
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
      Rigg.UpdateGetriebe;

//   Alternative:
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
    lbMastfall.Caption := Format('Mastfall = %d mm', [ScrollPos])
  else if Sender = sbSpannung then
    lbSpannung.Caption := Format('Vorstagspannung = %d N', [ScrollPos])
  else if Sender = sbBiegungS then
    lbBiegungS.Caption := Format('Mastbiegung = %d mm', [ScrollPos]);

  if (ScrollCode = scEndScroll) and (Sender = sbSpannung) then
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
      temp := Rigg.Antrieb;
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
      temp := Rigg.TrySalingH;
      Pen.Color := clRed;
      P.x := Round(temp*ZoomX);
      P.y := Round(Ymin*ZoomY);
      MoveTo(P.x,P.y);
      P.x := Round(temp*ZoomX);
      P.y := Round(Ymax*ZoomY);
      LineTo(P.x, P.y);

      (*
      { limitA }
      temp := Rigg.limitA;
      Pen.Color := clRed;
      P.x := Round(temp*ZoomX);
      P.y := Round(Ymin*ZoomY);
      MoveTo(P.x,P.y);
      P.x := Round(temp*ZoomX);
      P.y := Round(Ymax*ZoomY);
      LineTo(P.x, P.y);

      { limitB }
      temp := Rigg.limitB;
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

function TFormReglerGraph.GetXText: String;
var
  s: string;
begin
  case Rigg.SalingTyp of
    stFest: s := 'Saling-Höhe [mm]';
    stDrehbar: s := 'Saling-Länge [mm]';
  end;
  Result := s;
end;

function TFormReglerGraph.GetYText: String;
var
  S: String;
begin
  S := 'Vorstag-Spannung [N]';
  Result := S;
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
  Xmin := Rigg.Anfang;
  Xmax := Rigg.Ende;
end;

procedure TFormReglerGraph.GetTestData;
var
  i: Integer;
begin
  for i := 0 to CLMax do f[i] := 100 * i/CLMax;
  TestF := f;
end;

procedure TFormReglerGraph.GetData;
begin
  ChartValid := True;
  f := Rigg.KurveF;
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

