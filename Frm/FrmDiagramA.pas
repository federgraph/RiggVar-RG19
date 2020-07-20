unit FrmDiagramA;

interface

uses
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  System.Types,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.Buttons,
  Vcl.ComCtrls,
  RiggVar.RG.Def,
  RggTypes,
  RggScroll,
  RggCalc,
  RggUnit4;

const
  ANr = 6;

type
  ChartArray = array[0..ANr - 1] of TChartLineData;

  TFormDiagramA = class(TForm)
    OutputPages: TPageControl;
    ChartSheet: TTabSheet;
    pnChart2: TPanel;
    KurveBtn: TSpeedButton;
    PunktBtn: TSpeedButton;
    KurveValidLED: TShape;
    lbAchseX: TLabel;
    lbAchseY: TLabel;
    lbXLeft: TLabel;
    lbXRight: TLabel;
    lbYTop: TLabel;
    lbYBottom: TLabel;
    ChartPaintBox: TPaintBox;
    YCombo: TComboBox;
    cbFollowPoint: TCheckBox;
    XCombo: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure YComboChange(Sender: TObject);
    procedure KurveBtnClick(Sender: TObject);
    procedure PunktBtnClick(Sender: TObject);
    procedure cbFollowPointClick(Sender: TObject);
    procedure ChartPaintBoxPaint(Sender: TObject);
    procedure XComboChange(Sender: TObject);
  private
    FIntendedX: TsbName;
    FActualX: TsbName;
    FKurveValid: Boolean;
    FChartValid: Boolean;

    ShowTriangle: Boolean;
    InputBuffer: TTrimmControls;

    TopTitel: string;
    LeftTitel: string;
    BottomTitel: string;
    RightTitel: string;

    Xmin: single;
    Xmax: single;
    Ymin: single;
    Ymax: single;
    YGap: single;

    ChartPunktX: single;
    ChartPunktY: single;
    PunktColor: TColor;

    f: TChartLineData;
    TestF: TChartLineData;

    af: ChartArray;
    bf: array[0..ANr - 1] of double;
    procedure InitXComboItems;
    procedure SetParam(const Value: TFederParam);

    procedure StraightLine;
    procedure GetCurves;
    procedure LookForYMinMax;

    procedure DrawPoint;
    procedure DrawChartPaintBox(Canvas: TCanvas; Rect: TRect);
    procedure PaintBackGround(Image: TBitmap);
    procedure DrawChart;

    function GetXText(sbn: TsbName): string;
    function GetYText(Text: string): string;
    function GetPunktColor: TColor;
    procedure SetKurveValidLED(Value: Boolean);
    procedure SetKurveValid(Value: Boolean);
    procedure SetIntendedX(Value: TSBName);

    function GetXComboIndexOfParam(Value: TFederParam): Integer;
    function XComboIndexToParam(Value: Integer): TFederParam;
  public
    Rigg: TRigg;
    YComboSavedItemIndex: Integer;

    procedure UpdateGetriebePunkt;
    procedure UpdateRiggPunkt;

    procedure UpdateOnParamChanged;

    property KurveValid: Boolean read FKurveValid write SetKurveValid;
    property IntendedX: TSBName read FIntendedX write SetIntendedX;
    property ActualX: TSBName read FActualX write FActualX;

    property Param: TFederParam write SetParam;
  end;

var
  FormDiagramA: TFormDiagramA;

implementation

{$r *.dfm}

uses
  RiggVar.App.Main;

procedure TFormDiagramA.FormCreate(Sender: TObject);
begin
  Rigg := Main.Rigg;

  FKurveValid := False;
  FChartValid := False;

  StraightLine;

  InputBuffer := Rigg.Glieder;

  XCombo.OnChange := nil;
  InitXComboItems;
  XCombo.ItemIndex := 2;
  XCombo.OnChange := XComboChange;

  FIntendedX := fpVorstag;
  FActualX := fpVorstag;

  YCombo.ItemIndex := YCombo.Items.IndexOf('Mastfall F0F');
  YComboSavedItemIndex := YCombo.ItemIndex;

  cbFollowPoint.Checked := True;

  KurveBtnClick(nil);
end;

procedure TFormDiagramA.YComboChange(Sender: TObject);
var
  ii: Integer;
begin
  ii := YCombo.ItemIndex;
  if (ii > -1) and (ii < ANr) then
    f := af[ii]
  else
    f := TestF;
  DrawChart;
end;

procedure TFormDiagramA.KurveBtnClick(Sender: TObject);
var
  cr: TRggSB;
begin
  BottomTitel := GetXText(IntendedX);
  cr := Rigg.GSB.Find(IntendedX);
  Xmin := cr.Min;
  Xmax := cr.Max;
  ActualX := IntendedX;
  GetCurves;
end;

procedure TFormDiagramA.PunktBtnClick(Sender: TObject);
begin
  DrawPoint;
end;

procedure TFormDiagramA.cbFollowPointClick(Sender: TObject);
begin
  DrawPoint;
end;

procedure TFormDiagramA.ChartPaintBoxPaint(Sender: TObject);
begin
  DrawChartPaintBox(ChartPaintBox.Canvas,ChartPaintBox.BoundsRect);
end;

procedure TFormDiagramA.SetKurveValidLED(Value: Boolean);
begin
  if Value then
    KurveValidLED.Brush.Color := clLime
  else
    KurveValidLED.Brush.Color := clRed;
end;

procedure TFormDiagramA.SetParam(const Value: TFederParam);
begin
  if (Value >= Low(TSBName))and (Value <= High(TSBName))then
    IntendedX := Value;
end;

procedure TFormDiagramA.DrawChart;
var
  i: Integer;
begin
  if FChartValid then
  begin
    LeftTitel := GetYText(YCombo.Text);
    i := YCombo.ItemIndex;
    if i = -1 then
    begin
      i := YComboSavedItemIndex;
    end;
    f := af[i];
    case IntendedX of
      fpController: ChartPunktX := InputBuffer.Controller;
      fpWinkel: ChartPunktX := InputBuffer.Winkel;
      fpVorstag: ChartPunktX := InputBuffer.Vorstag;
      fpWante: ChartPunktX := InputBuffer.Wanten;
      fpWoben: ChartPunktX := InputBuffer.Woben;
      fpSalingH: ChartPunktX := InputBuffer.SalingH;
      fpSalingA: ChartPunktX := InputBuffer.SalingA;
      fpVorstagOS: ChartPunktX := InputBuffer.Vorstag;
    end;
    ChartPunktY := bf[i];
    LookForYMinMax;
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

  if (Ymax - Ymin < 1) then
  begin
    Ymin := Ymin - 1;
    Ymax := Ymax + 1;
  end;
  if (Xmax - Xmin < 1) then
  begin
    Xmin := Xmin - 1;
    Ymax := Ymax + 1;
  end;

  PunktColor := GetPunktColor;
  DrawChartPaintBox(ChartPaintBox.Canvas, ChartPaintBox.BoundsRect);
end;

procedure TFormDiagramA.DrawChartPaintBox(Canvas: TCanvas; Rect: TRect);

  function Limit(a: double): double;
  begin
    if a < -32000 then
      a := -32000
    else if a > 32000 then
      a := 32000;
    Result := a;
  end;

var
  P: TPoint;
  R: TRect;
  i, RadiusX, RadiusY: Integer;
  bmp: TBitmap;
  PlotWidth, PlotHeight: Integer;
  PlotExtX, PlotExtY: Integer;
  PlotOrgX, PlotOrgY: Integer;
  tempX, tempY: double;
  c: TCanvas;
  h: HDC;
begin
  PlotWidth := Rect.Right - Rect.Left;
  PlotHeight := Rect.Bottom - Rect.Top;
  PlotExtX := PlotWidth;
  PlotExtY := PlotHeight;
  PlotOrgX := 0;
  PlotOrgY := 0;

  bmp := TBitmap.Create;
  bmp.Width := PlotWidth;
  bmp.Height := PlotHeight;
  try
    PaintBackGround(bmp);
    c := bmp.Canvas;
    h := c.Handle;
    SetMapMode(h, MM_ANISOTROPIC);
    SetWindowExtEx(h, PlotExtX, -PlotExtY, nil);
    SetWindowOrgEx(h, PlotOrgX, PlotOrgY, nil);
    SetViewPortExtEx(h, PlotWidth, PlotHeight, nil);
    SetViewPortOrgEx(h, 0, PlotHeight, nil);

    {Kurve}
    c.Pen.Color := clBlue;
    c.MoveTo(0, 0);
    for i := 0 to CPMax do
    begin
      tempX := PlotExtX * (i / CPMax);
      tempY := PlotExtY * (f[i] - Ymin) / (YMax - Ymin);
      P.X := Round(Limit(tempX));
      P.Y := Round(Limit(tempY));
      c.LineTo(P.X, P.Y);
    end;

    { Aktueller Punkt bzw. X-Position }
    R.Left := 0;
    R.Top := 0;
    R.Bottom := 5;
    R.Right := 5;
    DPTOLP(h, R, 2);
    RadiusX := R.Right - R.Left;
    RadiusY := R.Bottom - R.Top;

    tempX := PlotExtX * (ChartPunktX - Xmin) / (XMax - Xmin);
    tempY := PlotExtY * (ChartPunktY - Ymin) / (YMax - Ymin);
    P.X := Round(Limit(tempX));
    P.Y := Round(Limit(tempY));
    if (P.Y <> 0) and KurveValid then
    begin
      { aktueller Punkt }
      c.Brush.Color := PunktColor;
      c.Brush.Style := bsSolid;
      c.Ellipse(P.X - RadiusX, P.Y - RadiusY, P.X + RadiusX, P.Y + RadiusY);
    end
    else if ShowTriangle then
    begin
      { Positionsdreieck X }
      c.Pen.Color := clBlack;
      P.Y := 0;
      RadiusX := RadiusX;
      RadiusY := RadiusY * 2;
      c.Polyline(
        [Point(P.X, P.Y),
         Point(P.X - RadiusX, P.Y - RadiusY),
         Point(P.X + RadiusX, P.Y - RadiusY),
         Point(P.X, P.Y)]);
    end;

    SetMapMode(h, MM_TEXT);
    SetWindowOrgEx(h, 0, 0, nil);
    SetViewPortOrgEx(h, 0, 0, nil);

    { Rahmen zeichnen }
    c.Pen.Width := 1;
    c.Pen.Color := clBlack;
    c.Brush.Style := bsClear;
    c.Rectangle(0, 0, bmp.Width, bmp.Height);

    Canvas.CopyMode := cmSrcCopy;
    Canvas.Draw(0, 0, bmp);

  finally
    bmp.Free;
  end;

  lbAchseX.Caption := BottomTitel;
  lbAchseY.Caption := Lefttitel;
  lbXLeft.Caption := IntToStr(Round(Xmin));
  lbXRight.Caption := IntToStr(Round(Xmax));
  lbYBottom.Caption := IntToStr(Round(Ymin));
  lbYTop.Caption := IntToStr(Round(Ymax));
end;

function TFormDiagramA.GetPunktColor: TColor;
var
  i: Integer;
  ML: TStrings;
begin
  result := clLime;
  i := YCombo.ItemIndex;
  ML := YCombo.Items;

  if (i = ML.IndexOf('Vorstag-Spannung')) or
     (i = ML.IndexOf('Wanten-Spannung')) or
     (i = ML.IndexOf('Elastizität Punkt C')) then
  begin
    if not (Rigg.RiggOK and Rigg.GetriebeOK and Rigg.MastOK) then
      result := clRed;
  end;

  if (i = ML.IndexOf('Mastfall F0F')) or
     (i = ML.IndexOf('Mastfall F0C')) or
     (i = ML.IndexOf('Durchbiegung hd')) then
  begin
    if not (Rigg.GetriebeOK and Rigg.MastOK) then
      result := clRed;
  end;
end;

procedure TFormDiagramA.DrawPoint;
var
  i: Integer;
begin
  if not FChartValid then
    Exit;
  { Koordinaten des Punktes }
  case IntendedX of
    fpController: ChartPunktX := InputBuffer.Controller;
    fpWinkel: ChartPunktX := InputBuffer.Winkel;
    fpVorstag: ChartPunktX := InputBuffer.Vorstag;
    fpWante: ChartPunktX := InputBuffer.Wanten;
    fpWoben: ChartPunktX := InputBuffer.Woben;
    fpSalingH: ChartPunktX := InputBuffer.SalingH;
    fpSalingA: ChartPunktX := InputBuffer.SalingA;
    fpVorstagOS: ChartPunktX := InputBuffer.Vorstag;
  end;
  i := YCombo.ItemIndex;
  if i = -1 then
  begin
    i := YComboSavedItemIndex;
{$ifdef debug}
    MessageBeep(MB_ICONQUESTION);
{$endif}
  end;
  ChartPunktY := bf[i];
  { Farbe des Punktes }
  PunktColor := GetPunktColor;
  DrawChartPaintBox(ChartPaintBox.Canvas, ChartPaintBox.BoundsRect);
end;

procedure TFormDiagramA.LookForYMinMax;
var
  i: Integer;
begin
  if Rigg.CalcTyp = ctQuerKraftBiegung then
  begin
    if (YCombo.Text = 'Vorstag-Spannung') or
      (YCombo.Text = 'Wanten-Spannung') then
    begin
      YMax := 5000; { 5000 N }
      YMin := -1000; { -1000 N }
      Exit;
    end;
    if (YCombo.Text = 'Elastizität Punkt C') then
    begin
      YMax := 1000; { 1000 mm }
      YMin := 0;
      Exit;
    end;
  end;

  Ymax := f[0];
  Ymin := Ymax;
  for i := 0 to CPMax do
  begin
    if f[i] > Ymax then
      Ymax := f[i];
    if f[i] < Ymin then
      Ymin := f[i];
  end;
end;

function TFormDiagramA.GetXText(sbn: TsbName): string;
var
  s: string;
begin
  if sbn = fpController then
    s := 'Zustellung Mast-Controller [mm]'
  else if sbn = fpWinkel then
    s := 'Winkel [Grad]'
  else if (sbn = fpVorstag) or (sbn = fpVorstagOS) then
    s := 'Vorstaglänge [mm]'
  else if sbn = fpWante then
    s := 'Wantenlänge [mm]'
  else if sbn = fpWoben then
    s := 'Länge des oberen Wantenabschnitts [mm]'
  else if sbn = fpSalingH then
    s := 'Höhe des Salingdreiecks [mm]'
  else if sbn = fpSalingA then
    s := 'Saling-Abstand [mm]';
  Result := s;
end;

function TFormDiagramA.GetYText(Text: string): string;
var
  s: string;
begin
  if Text = 'Wanten-Spannung' then
    s := 'Wantenspannung [N]'
  else if Text = 'Vorstag-Spannung' then
    s := 'Vorstagspannung [N]'
  else if Text = 'Elastizität Punkt C' then
    s := 'Auslenkung Punkt C [mm]'
  else if Text = 'Durchbiegung hd' then
    s := 'Mastbiegung hd [mm]'
  else if Text = 'Mastfall F0F' then
    s := 'Mastfall F0F [mm]'
  else if Text = 'Mastfall F0C' then
    s := 'Mastfall F0C [mm]';
  Result := s;
end;

procedure TFormDiagramA.SetIntendedX(Value: TSBName);
begin
  if FIntendedX <> Value then
  begin
    FIntendedX := Value;
    KurveValid := False;
  end;
  if ActualX = IntendedX then
    ShowTriangle := True
  else
    ShowTriangle := False;
  DrawChart;
end;

procedure TFormDiagramA.PaintBackGround(Image: TBitmap);
var
  R: TRect;
begin
  if Image = nil then
    Exit;

  R := Rect(0, 0, Image.Width, Image.Height);
  Image.Canvas.Brush.Color := clWhite;
  Image.Canvas.FillRect(R);
end;

procedure TFormDiagramA.StraightLine;
var
  i: Integer;
begin
  for i := 0 to CPMax do
    f[i] := 100 * i / CPMax;
  TestF := f;
end;

procedure TFormDiagramA.GetCurves;
var
  i, tempIndex: Integer;
  Antrieb, Anfang, Ende: double;
  InputRec: TTrimmControls;
  PunktOK: Boolean;
begin
  Screen.Cursor := crHourGlass;
  try
    FChartValid := False;

    { Getriebezustand sichern und verfügbar machen }
    InputRec := Rigg.Glieder;
    Rigg.ProofRequired := False;

    { Definitionsbereich bestimmen und Berechnungsschleife starten }
    Anfang := Rigg.GSB.Find(IntendedX).Min;
    Ende := Rigg.GSB.Find(IntendedX).Max;
    for i := 0 to CPMax do
    begin
      Antrieb := Anfang + (Ende - Anfang) * i / CPMax;
      { Antrieb ansteuern }
      case IntendedX of
        fpController: Rigg.RealGlied[fpController] := Antrieb;
        fpWinkel: Rigg.RealGlied[fpWinkel] := Antrieb * pi / 180;
        fpVorstag: Rigg.RealGlied[fpVorstag] := Antrieb;
        fpWante: Rigg.RealGlied[fpWante] := Antrieb;
        fpWoben: Rigg.RealGlied[fpWoben] := Antrieb;
        fpSalingH: Rigg.RealGlied[fpSalingH] := Antrieb;
        fpSalingA: Rigg.RealGlied[fpSalingA] := Antrieb;
        fpVorstagOS: Rigg.RealGlied[fpVorstag] := Antrieb;
      end;

      { Berechnen }
      Rigg.UpdateGetriebe;
      Rigg.UpdateRigg;
      PunktOK := Rigg.GetriebeOK and Rigg.MastOK and Rigg.RiggOK;

      { Ergebnisse einspeichern }
      tempIndex := YCombo.Items.IndexOf('Vorstag-Spannung');
      if (tempIndex <> -1) and (tempIndex < ANr) then
      begin
        if PunktOK then
          af[tempIndex, i] := Rigg.rF[14]
        else
          af[tempIndex, i] := 0;
      end;
      tempIndex := YCombo.Items.IndexOf('Wanten-Spannung');
      if (tempIndex <> -1) and (tempIndex < ANr) then
      begin
        if PunktOK then
          af[tempIndex, i] := Rigg.rF[8]
        else
          af[tempIndex, i] := 0;
      end;
      tempIndex := YCombo.Items.IndexOf('Mastfall F0F');
      if (tempIndex <> -1) and (tempIndex < ANr) then
      begin
        af[tempIndex, i] := Abstand(Rigg.rP[ooF0], Rigg.rP[ooF]);
      end;
      tempIndex := YCombo.Items.IndexOf('Mastfall F0C');
      if (tempIndex <> -1) and (tempIndex < ANr) then
      begin
        af[tempIndex, i] := Abstand(Rigg.rP[ooF0], Rigg.rP[ooC]);
      end;
      tempIndex := YCombo.Items.IndexOf('Elastizität Punkt C');
      if (tempIndex <> -1) and (tempIndex < ANr) then
      begin
        if PunktOK then
          af[tempIndex, i] := Abstand(Rigg.rP[ooC], Rigg.rPe[ooC])
        else
          af[tempIndex, i] := 0;
      end;
      tempIndex := YCombo.Items.IndexOf('Durchbiegung hd');
      if (tempIndex <> -1) and (tempIndex < ANr) then
      begin
        af[tempIndex, i] := Rigg.hd;
      end;
    end;

    FChartValid := True;
    KurveValid := True;
    DrawChart;

    ActiveControl := YCombo

  finally
    { restore Model (Getriebe) }
    Rigg.ProofRequired := True;
    Rigg.Glieder := InputRec;
    Main.UpdateGetriebe;
    DrawPoint;
    Screen.Cursor := crDefault;
  end;
end;

procedure TFormDiagramA.UpdateGetriebePunkt;
var
  tempIndex: Integer;
begin
  InputBuffer := Rigg.Glieder;

  if not cbFollowPoint.Checked then
    Exit;

  { Ergebnisse einspeichern }
  tempIndex := YCombo.Items.IndexOf('Mastfall F0F');
  if (tempIndex <> -1) and (tempIndex < ANr) then
  begin
    bf[tempIndex] := Abstand(Rigg.rP[ooF0], Rigg.rP[ooF]);
  end;
  tempIndex := YCombo.Items.IndexOf('Mastfall F0C');
  if (tempIndex <> -1) and (tempIndex < ANr) then
  begin
    bf[tempIndex] := Abstand(Rigg.rP[ooF0], Rigg.rP[ooC]);
  end;
  tempIndex := YCombo.Items.IndexOf('Durchbiegung hd');
  if (tempIndex <> -1) and (tempIndex < ANr) then
  begin
    bf[tempIndex] := Rigg.hd;
  end;

  { RiggPunkte Null setzen }
  tempIndex := YCombo.Items.IndexOf('Vorstag-Spannung');
  if (tempIndex <> -1) and (tempIndex < ANr) then
    bf[tempIndex] := 0;
  tempIndex := YCombo.Items.IndexOf('Wanten-Spannung');
  if (tempIndex <> -1) and (tempIndex < ANr) then
    bf[tempIndex] := 0;
  tempIndex := YCombo.Items.IndexOf('Elastizität Punkt C');
  if (tempIndex <> -1) and (tempIndex < ANr) then
    bf[tempIndex] := 0;

  { Punkte im Diagramm aktualisieren }
  if not (Main.SofortBerechnen and Rigg.GetriebeOK and Rigg.MastOK) then
    DrawPoint
  else
  begin
    { DrawPoint will be called from UpdateRiggPunkt }
  end;
end;

procedure TFormDiagramA.UpdateRiggPunkt;
var
  tempIndex: Integer;
begin
  if not cbFollowPoint.Checked then
    Exit;

  { RiggPunkte bereits in UpdateGetriebePunkt genullt }
  if (Rigg.GetriebeOK and Rigg.MastOK and Rigg.RiggOK) then
  begin
    tempIndex := YCombo.Items.IndexOf('Vorstag-Spannung');
    if (tempIndex <> -1) and (tempIndex < ANr) then
      bf[tempIndex] := Rigg.rF[14];
    tempIndex := YCombo.Items.IndexOf('Wanten-Spannung');
    if (tempIndex <> -1) and (tempIndex < ANr) then
      bf[tempIndex] := Rigg.rF[13];
    tempIndex := YCombo.Items.IndexOf('Elastizität Punkt C');
    if (tempIndex <> -1) and (tempIndex < ANr) then
      bf[tempIndex] := Abstand(Rigg.rP[ooC], Rigg.rPe[ooC]);
  end;

  DrawPoint;
end;

procedure TFormDiagramA.SetKurveValid(Value: Boolean);
begin
  if FKurveValid <> Value then
  begin
    FKurveValid := Value;
    SetKurveValidLED(Value);
  end;
end;

procedure TFormDiagramA.InitXComboItems;
var
  ML: TStrings;
begin
  ML := XCombo.Items;
  ML.Clear;
  ML.Add('Controller');
  ML.Add('Winkel');
  ML.Add('Vorstag');
  ML.Add('Wante');
  ML.Add('Woben');
  ML.Add('SalingH');
  ML.Add('SalingA');
end;

function TFormDiagramA.GetXComboIndexOfParam(Value: TFederParam): Integer;
begin
  case Value of
    fpController: result := 0;
    fpWinkel: result := 1;
    fpVorstag: result := 2;
    fpWante: result := 3;
    fpWoben: result := 4;
    fpSalingH: result := 5;
    fpSalingA: result := 6;
    else result := -1;
  end;
end;

function TFormDiagramA.XComboIndexToParam(Value: Integer): TFederParam;
begin
  case Value of
    0: result := fpController;
    1: result := fpWinkel;
    2: result := fpVorstag;
    3: result := fpWante;
    4: result := fpWoben;
    5: result := fpSalingH;
    6: result := fpSalingA;
    else
      result := fpVorstag;
  end;
end;

procedure TFormDiagramA.XComboChange(Sender: TObject);
var
  ii: Integer;
  sbn: TSBName;
begin
  ii := XCombo.ItemIndex;
  sbn := XComboIndexToParam(ii);
  IntendedX := sbn;
end;

procedure TFormDiagramA.UpdateOnParamChanged;
var
  sb: Boolean;
  ii: Integer;
begin
  ii := GetXComboIndexOfParam(Main.Param);
  if ii = -1 then
  begin
    KurveValid := False;
    cbFollowPoint.Checked := False;
  end
  else
  begin
    Param := Main.Param;
    XCombo.ItemIndex := ii;

    sb := Main.SofortBerechnen;
    Main.SofortBerechnenNoChange := True;

    cbFollowPoint.Checked := True;
    KurveBtnClick(nil);

    Main.SofortBerechnenNoChange := sb;
  end;
end;

end.
