unit FrmChart;

interface

{$ifdef fpc}
{$mode delphi}
{$endif}

{$define RG19}

uses
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  System.Types,
  System.UITypes,
  System.Math,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  Vcl.StdCtrls,
  Vcl.Mask,
  Vcl.Menus,
  Vcl.Buttons,
  Vcl.ComCtrls,
  RggUnit4,
  RggTypes,
  RggDoc,
  RggSaling3Eck;

const
  ANr = 6; { maximale Anzahl Kurven, d.h. berechneter Y Werte }
  PNr = 5; { maximale Anzahl der Werte des Parameters }
  VNr = 14; { Anzahl der zur Auswahl stehenden Y Werte }
  LNr = 100; { Anzahl der Punkte im Diagramm - 1 }
  ErrorIndex = 999;
  D180 = 180 / PI;
  P180 = PI / 180;

type
  TxpName = (
    xpController,
    xpWinkel,
    xpVorstag,
    xpWante,
    xpWoben,
    xpSalingH,
    xpSalingA,
    xpSalingL,
    xpVorstagOS,
    xpWPowerOS,
    xpSalingW);

  TxpNameSet = set of TXpName;

  TChartStatus = (csBerechnet, csGeladen);
  TYLineArray = array[0..ANr-1] of TLineDataR100;
  TYAchseSortedList = array[0..VNr-1] of TYAchseValue;
  TYAchseSet = set of TYAchseValue; { die berechneten Kurven }
  TYAchseStringArray = array[0..PNr-1] of string;

  TChartForm = class(TForm)
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    {Panel AlignTop}
    BedienPanel: TPanel;
    YCombo: TComboBox;
    YComboLabel: TLabel;
    YMinEdit: TEdit;
    YMinLabel: TLabel;
    YMaxEdit: TEdit;
    YMaxLabel: TLabel;
    YLED: TShape;
    BevelY: TBevel;
    {Ctrls}
    PEdit: TEdit;
    PSpinner: TUpDown;
    PCountLabel: TLabel;
    KurvenzahlEdit: TEdit;
    KurvenZahlSpinner: TUpDown;
    KurvenZahlLabel: TLabel;
    APEdit: TEdit;
    APSpinner: TUpDown;
    APLabel: TLabel;
    CalcBtn: TSpeedButton;
    BuissyBtn: TSpeedButton;
    BereichBtn: TSpeedButton;
    APBtn: TSpeedButton;
    AuswahlBtn: TSpeedButton;
    MemoBtn: TSpeedButton;
    ShowTogetherBtn: TSpeedButton;
    BevelCtrls: TBevel;
    XCombo: TComboBox;
    XComboLabel: TLabel;
    XMinEdit: TMaskEdit;
    XMinLabel: TLabel;
    XMaxEdit: TMaskEdit;
    XMaxLabel: TLabel;
    XLED: TShape;
    XBevel: TBevel;
    PCombo: TComboBox;
    PComboLabel: TLabel;
    PMinEdit: TMaskEdit;
    PMinLabel: TLabel;
    PMaxEdit: TMaskEdit;
    PMaxLabel: TLabel;
    PLED: TShape;
    PBevel: TBevel;
    lbXLeft: TLabel;
    lbAchseX: TLabel;
    lbXRight: TLabel;
    ChartPaintBox: TPaintBox;
    PaintBoxLegend: TPaintBox;
    ChartBevelInner: TBevel;
    lbParam: TLabel;
    ChartBevelOuter: TBevel;
    procedure FormCreate(Sender: TObject);
    procedure YAuswahlClick(Sender: TObject);
    procedure CalcItemClick(Sender: TObject);
    procedure BuissyItemClick(Sender: TObject);
    procedure APItemClick(Sender: TObject);
    procedure BereichItemClick(Sender: TObject);
    procedure CloseItemClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormActivate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PSpinnerChanging(Sender: TObject; var AllowChange: Boolean);
    procedure OpenItemClick(Sender: TObject);
    procedure SaveItemClick(Sender: TObject);
    procedure UpdateRiggItemClick(Sender: TObject);
    procedure RectangleItemClick(Sender: TObject);
    procedure PEditChange(Sender: TObject);
    procedure XComboChange(Sender: TObject);
    procedure YComboChange(Sender: TObject);
    procedure PComboChange(Sender: TObject);
    procedure MemoItemClick(Sender: TObject);
    procedure ShowTogetherBtnClick(Sender: TObject);
    procedure KurvenZahlSpinnerClick(Sender: TObject; Button: TUDBtnType);
    procedure KurvenzahlEditChange(Sender: TObject);
    procedure ChartMenuClick(Sender: TObject);
    procedure UpdateChartItemClick(Sender: TObject);
    procedure BereichBtnClick(Sender: TObject);
    procedure APEditChange(Sender: TObject);
    procedure KurvenZahlSpinnerChanging(Sender: TObject; var AllowChange: Boolean);
    procedure FormPaint(Sender: TObject);
    procedure ChartPaintBoxPaint(Sender: TObject);
    procedure PaintBoxLegendPaint(Sender: TObject);
  private
    FBuissy: Boolean;
    FStatus: Set of TChartStatus;
    FValid: Boolean;
    FLegend: Boolean;
    FShowGroup: Boolean;
    FSalingTyp: TSalingTyp;
    FXTextClicked, FPTextClicked: string;
    procedure GetMemoText;
    function GetYText(Text: string): string;
    function GetTsbName(Value: string): TxpName;
    procedure SetSalingTyp(Value: TSalingTyp);
  protected
    function ValidateInput(Input: TMaskEdit): Boolean;
    procedure TakeOver;
  protected
    tempSpinnerPosition: Integer;
    YAchseRecordList: TYAchseRecordList;
    YAchseSortedList: TYAchseSortedList;
    YAchseSet: TYAchseSet;
    TopTitle: string;
    YTitle: string;
    XTitle: string;
    PTitle: string;
    Xmin, Xmax, Ymin, Ymax: single;
    ParamCount: Integer;
    APWidth: Integer;
    TempF: TLineDataR100;
    TestF: TLineDataR100;
    af: array[0..PNr-1] of TYLineArray;
    bf: array[0..PNr-1] of TLineDataR100;
    cf: array[0..PNr-1] of TColor;
    PText, PColorText: TYAchseStringArray;
    RggDocument: TRggDocument;
    SalingDreieck: TSalingDreieck;
    procedure InitStraightLine;
    procedure LookForYMinMax;
    procedure UpdateYMinMax;
    procedure GetCurves;
    procedure LoadNormal;
    procedure Draw;
    procedure DrawInternal;
    procedure DrawNormal;
    procedure DrawToChart;
    procedure DoLegend;
    procedure SaveToStream(S: TStream);
    procedure LoadFromStream(S: TStream);
    procedure UpdateYAchseList;
    procedure UpdateYAchseSet;
    function ComboIndexToCurve(ComboIndex: Integer): Integer;
    procedure RebuildYCombo;
    procedure ShowTogether(ParamNo: Integer);
  public
    XSet: TxpNameSet;
    PSet: TxpNameSet;
    ProgressPosition: Integer;
    ProgressCaption: string;
    CalcCounter: Integer;
    MemoCounter: Integer;
    ChartPunktX: double;
    MemoLines: TStringList;
    XAchseMin, XAchseMax, ParamMin, ParamMax: Integer;
    XComboText, PComboText: string;
    XAchseText, ParamText: string;
    GroupKurvenZahl: Integer;
    GroupText: TYAchseStringArray;
    function GetXText(Text: string): string;
    function GetPText(Text: string): string;
    procedure UpdateXCombo(SalingTyp: TSalingTyp);
    procedure UpdatePCombo(SalingTyp: TSalingTyp);
    procedure UpdateXMinMax;
    procedure UpdatePMinMax;
    function CheckBeforeCalc: Boolean;
    procedure Calc;
    procedure DoAfterCalc;
    procedure SaveToFile(FileName: string);
    procedure LoadFromFile(FileName: string);
    procedure Reset;

    property SalingTyp: TSalingTyp read FSalingTyp write SetSalingTyp;
    property Valid: Boolean read FValid write FValid;
    property ShowGroup: Boolean read FShowGroup;
  protected
    MainMenu: TMainMenu;
    ChartMenu: TMenuItem;
    BerechnenItem: TMenuItem;
    AuswahlItem: TMenuItem;
    ResetItem: TMenuItem;
    APItem: TMenuItem;
    BereichItem: TMenuItem;
    OpenItem: TMenuItem;
    SaveItem: TMenuItem;
    MemoItem: TMenuItem;
    TogetherItem: TMenuItem;
    UpdateRiggItem: TMenuItem;
    UpdateChartItem: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    N3: TMenuItem;
    procedure InitMenu;
  protected
    Rigg: TRigg;
    SofortBerechnen: Boolean;
    FDarkColors: Boolean;
    procedure UpdateGetriebe;
    procedure SetDarkColors(const Value: Boolean);
    property DarkColors: Boolean read FDarkColors write SetDarkColors;
  private
    procedure DrawChartPaintBox(Canvas: TCanvas; Rect: TRect);
    procedure PaintBackGround(Image: TBitMap);
  protected
    procedure DrawLegend(Canvas: TCanvas; Rect: TRect);
    procedure DrawLabels;
  protected
    WantRectangles: Boolean;
    RectangleItem: TMenuItem;
    N4: TMenuItem;
  end;

var
  ChartForm: TChartForm;

implementation

{$R *.DFM}

uses
{$ifdef RG19}
  RggModul,
  FrmMemo,
{$endif}
  RiggVar.RG.Def,
  RiggVar.FB.Classes,
  RggCalc,
  RggScroll,
  FrmAuswahl;

procedure TChartForm.FormCreate(Sender: TObject);
begin
  Include(XSet, xpController);
  Include(XSet, xpWinkel);
  Include(XSet, xpVorstag);
  Include(XSet, xpWante);
  Include(XSet, xpWoben);
  Include(XSet, xpSalingH);
  Include(XSet, xpSalingA);
  Include(XSet, xpSalingL);
  Include(XSet, xpSalingW);

  PSet := XSet;
//  Include(PSet, xpController);
//  Include(PSet, xpWinkel);
//  Include(PSet, xpVorstag);
//  Include(PSet, xpWante);
//  Include(PSet, xpWoben);
//  Include(PSet, xpSalingH);
//  Include(PSet, xpSalingA);
//  Include(PSet, xpSalingL);
//  Include(PSet, xpSalingW);

  WantRectangles := True;
  FLegend := True;

  ChartForm := self; { wird schon in AchsForm.Create benötigt }
  HorzScrollBar.Position := 0;

{$ifdef RG19}
  Rigg := RiggModul.Rigg;
  SofortBerechnen := True;
{$endif}

  FSalingTyp := stFest;

  RggDocument := TRggDocument.Create;
  MemoLines := TStringList.Create;
  MemoLines.Add(AnfangsZustandString);
  SalingDreieck := TSalingDreieck.Create;

  ParamCount := 3;
  APWidth := 30;

  YLED.Brush.Color := clRed;
  XLED.Brush.Color := clRed;
  PLED.Brush.Color := clRed;

  TakeOver;

  FXTextClicked := VorstagString;
  FPTextClicked := SalingHString;

  UpdateXCombo(SalingTyp);
  UpdatePCombo(SalingTyp);

  XCombo.ItemIndex := XCombo.Items.IndexOf(FXTextClicked);
  PCombo.ItemIndex := PCombo.Items.IndexOf(FPTextClicked);

  APSpinner.Position := 30;
  KurvenZahlSpinner.Position := 3;

  InitYAchseRecordList(YAchseRecordList);
  { Hiermit werden die Felder ComboText und Text initialisiert.
    ComboIndex wird in UpdateYAchseList weiter unten bestimmt.
    ArrayIndex wird beim Berechnen oder Einlesen neu bestimmt.
    YAchseSet = [] zeigt an, daß ArrayIndex nicht gültig ist.
  }

  DarkColors := False;

  PColorText[0] := BlueString;
  PColorText[1] := RedString;
  PColorText[2] := GreenString;
  PColorText[3] := WhiteString;
  PColorText[4] := YellowString;

  PText := PColorText;

  YCombo.Items.Assign(YAuswahlDlg.DstList.Items);
  YCombo.ItemIndex := 1;
  UpdateYAchseList; { ComboIndex festlegen in YAchseRecordList }

{$ifdef RG19}
  if RiggModul.RG19A then
  begin
    FormStyle := fsMDIChild;
    InitMenu;
  end;
{$endif}

  InitStraightLine;

  Reset;

  ClientWidth := 800;
  ClientHeight := 478;

  with ChartBevelInner do
  begin
    Left := ChartPaintBox.Left-1;
    Top := ChartPaintBox.Top-1;
    Width := ChartPaintBox.Width+2;
    Height := ChartPaintBox.Height+2;
  end;
end;

procedure TChartForm.FormDestroy(Sender: TObject);
begin
  RggDocument.Free;
  MemoLines.Free;
  SalingDreieck.Free;
end;

procedure TChartForm.APItemClick(Sender: TObject);
begin
  { APItem bedeutet ArbeitspunktItem }
  APItem.Checked := not APItem.Checked;
  APBtn.Down := APItem.Checked;
  UpdateXMinMax;
  UpdatePMinMax;
end;

procedure TChartForm.BereichItemClick(Sender: TObject);
begin
  BereichItem.Checked  := not BereichItem.Checked;
  BereichBtn.Down := BereichItem.Checked;
  UpdateXMinMax;
  UpdatePMinMax;
end;

procedure TChartForm.Reset;
begin
  FBuissy := False;
  FValid := False;
  FStatus := [];
  YAchseSet := [];
  PText := PColorText;
  YLED.Brush.Color := clRed;
  XLED.Brush.Color := clRed;
  PLED.Brush.Color := clRed;
  TakeOver;
  DrawInternal;
  MemoLines.Clear;
  MemoLines.Add(ResetMessageString);
end;

procedure TChartForm.SetDarkColors(const Value: Boolean);
begin
  FDarkColors := Value;
  if FDarkColors then
  begin
    cf[0] := clBlue;
    cf[1] := clRed;
    cf[2] := clGreen;
    cf[3] := clWhite;
    cf[4] := clYellow;
  end
  else
  begin
    cf[0] := clAqua;
    cf[1] := clFuchsia;
    cf[2] := clLime;
    cf[3] := clWhite;
    cf[4] := clYellow;
  end;
end;

procedure TChartForm.InitStraightLine;
var
  i: Integer;
begin
  for i := 0 to LNr do
  begin
    TempF[i] := i;
    bf[0, i] := TempF[i];
    bf[1, i] := TempF[i] + 5;
    bf[2, i] := TempF[i] + 10;
    bf[3, i] := TempF[i] + 20;
    bf[4, i] := TempF[i] + 50;
  end;
  TestF := TempF;
end;

procedure TChartForm.UpdateYAchseList;
var
  i: Integer;
  s: string;
  YAV: TYAchseValue;
begin
  { wird von YComboClick aufgerufen und vom constructor }

  { ComboIndex zurücksetzen auf -1 }
  for YAV := Low(TYAchseValue) to High(TYAchseValue) do
    YAchseRecordList[YAV].ComboIndex := -1;
  { ComboIndex neu bestimmen. Nicht ausgewählte Einträge bleiben auf -1 }
  for i := 0 to YCombo.Items.Count-1 do
  begin
    s := YCombo.Items[i];
    for YAV := Low(TYAchseValue) to High(TYAchseValue) do
      { Position j des Eintrag finden durch  Textvergleich }
      if s = YAchseRecordList[YAV].ComboText then
      begin
        { Position in der ComboBox festhalten }
        YAchseRecordList[YAV].ComboIndex := i;
        { Reihenfolge in Liste festhalten }
        YAchseSortedList[i] := YAV;
        break;
      end;
  end;
end;

procedure TChartForm.UpdateYAchseSet;
var
  i: Integer;
  s: string;
  YAV: TYAchseValue;
begin
  { Wird nur bei Neuberechnung aufgerufen }

  YAchseSet := [];
  for i := 0 to YCombo.Items.Count-1 do
  begin
    s := YCombo.Items[i];
    for YAV := Low(TYAchseValue) to High(TYAchseValue) do
      { Position j des Eintrag finden durch  Textvergleich }
      if s = YAchseRecordList[YAV].ComboText then
      begin
        { Position in Combo festhalten }
        YAchseRecordList[YAV].ArrayIndex := i;
        { festhalten, welche Kurven existieren }
        if i <= ANr-1 then
          Include(YAchseSet, YAV);
        break;
      end;
  end;
end;

function TChartForm.ComboIndexToCurve(ComboIndex: Integer): Integer;
var
  YAV: TYAchseValue;
begin
  YAV := YAchseSortedList[ComboIndex];
  if YAV in YAchseSet then
  begin
    if (csBerechnet in FStatus) or (csGeladen in FStatus) then
      FValid := True;
    result := YAchseRecordList[YAV].ArrayIndex;
  end
  else
  begin
    FValid := False;
    result := ErrorIndex;
  end;
end;

procedure TChartForm.Calc;
begin
  if not FBuissy then
  begin
    { Berechnen }
    Inc(CalcCounter);

    if not CheckBeforeCalc then
      Exit;

    FBuissy := True;

    { Parameterzahl bearbeiten }
    if PCombo.Text = NoParamString then
    begin
      PMinEdit.Text := IntToStr(0);
      PMaxEdit.Text := IntToStr(0);
      KurvenZahlSpinner.Position := 1;
    end;

    ParamCount := KurvenZahlSpinner.Position;

    if PSpinner.Position > ParamCount then
      PSpinner.Position := ParamCount;

    PSpinner.Max := ParamCount;
    { MaxValue muß größer MinValue sein }
    if PSpinner.Max = 1 then
      PSpinner.Max := 2;

    if YCombo.ItemIndex > ANr-1 then
      YCombo.ItemIndex := 1;

    UpdateYAchseSet;
    GetCurves;

    DoAfterCalc;
    Rigg.GetDocument(RggDocument);
    GetMemoText;
    Include(FStatus, csBerechnet);
    YLED.Brush.Color := clLime;
    FBuissy := False;

    LoadNormal;
    DrawInternal;
  end;
end;

procedure TChartForm.LoadNormal;
var
  j: Integer;
  p: Integer;
begin
  { Anzeigen }
  j := ComboIndexToCurve(YCombo.ItemIndex);
  for p := 0 to ParamCount - 1 do
    bf[p] := af[p, j];
  UpdateYMinMax;
end;

procedure TChartForm.GetCurves;
var
  i, j, p: Integer;
  Antrieb, PAntrieb: double;
  Anfang, Ende, PAnfang, PEnde: double;
  InputRec: TTrimmControls;
  PunktOK: Boolean;
  temp, tempL, tempH, tempA: double;
  st: string;
  xn: TxpName;
  pn: TxpName;
begin
  xn := GetTsbName(XCombo.Text);
  pn := GetTsbName(PCombo.Text);

  { Getriebezustand sichern und verfügbar machen }
  InputRec := Rigg.Glieder;

  case xn of
    xpController: ChartPunktX := InputRec.Controller;
    xpWinkel: ChartPunktX := InputRec.Winkel; // / 10;
    xpVorstag: ChartPunktX := InputRec.Vorstag;
    xpWante: ChartPunktX := InputRec.Wanten;
    xpWoben: ChartPunktX := InputRec.Woben;
    xpSalingH: ChartPunktX := InputRec.SalingH;
    xpSalingA: ChartPunktX := InputRec.SalingA;
    xpSalingL: ChartPunktX := InputRec.SalingL;
    xpVorstagOS: ChartPunktX := InputRec.Vorstag;
  end;

  TopTitle := Format('Co%dVo%dWi%dWo%dWa%dSh%dSa%dSl%d',
    [InputRec.Controller,
     InputRec.Vorstag,
     InputRec.Winkel,
     InputRec.Woben,
     InputRec.Wanten,
     InputRec.SalingH,
     InputRec.SalingA,
     InputRec.SalingL]);

  case SalingTyp of
    stFest: st := SalingFestString;
    stDrehbar: st := SalingDrehbarString;
    stOhne: st := OhneSalingString;
    stOhne_2: st := OhneSalingStarrString;
  end;

  TopTitle := Format('(%s/%s)', [TopTitle, st]);
  TopTitle := TopTitleString + ' - ' + DateToStr(Date) + ' - ' + TopTitle;

  Rigg.ProofRequired := False;

  try
    { Parameterbereich bestimmen und Schleife starten }
    PAnfang := StrToInt(PminEdit.Text);
    PEnde := StrToInt(PmaxEdit.Text);
    PAntrieb := (PEnde + PAnfang) / 2;
    for p := 0 to ParamCount - 1 do
    begin
      if ParamCount > 1 then
        ProgressCaption := Format(ProgressCaptionFormatString, [p+1, ParamCount])
      else
        ProgressCaption := ProgressCaptionString;

      if ParamCount > 1 then
      begin
        PAntrieb := PAnfang + (PEnde - PAnfang) * p / (ParamCount - 1);
        PText[p] := Format('%6.2f', [PAntrieb]);
      end;

      { Parameter ansteuern }
      if ParamCount < 2 then
      begin
       { do nothing }
      end
      else
      case pn of
        xpController: Rigg.RealGlied[fpController] := PAntrieb;
        xpWinkel: Rigg.RealGlied[fpWinkel] := PAntrieb * P180;
        xpVorstag: Rigg.RealGlied[fpVorstag] := PAntrieb;
        xpWante: Rigg.RealGlied[fpWante] := PAntrieb;
        xpWoben: Rigg.RealGlied[fpWoben] := PAntrieb;
        xpSalingH: Rigg.RealGlied[fpSalingH] := PAntrieb;
        xpSalingA: Rigg.RealGlied[fpSalingA] := PAntrieb;
        xpSalingL:
        begin
          case SalingTyp of
            stDrehbar: Rigg.RealGlied[fpSalingL] := PAntrieb;
            stFest:
      begin
        tempL := Rigg.RealGlied[fpSalingL];
        temp := PAntrieb/tempL;
        tempH := temp * Rigg.RealGlied[fpSalingH];
        tempA := temp * Rigg.RealGlied[fpSalingA];
        Rigg.RealGlied[fpSalingH] := tempH;
        Rigg.RealGlied[fpSalingA] := tempA;
            end;
          end;
        end;
        xpVorstagOS: ;
        xpWPowerOS: ;
        xpSalingW:
      begin
        temp := PAntrieb * P180;
        tempL := Rigg.RealGlied[fpSalingL];
        tempH := tempL * sin(temp);
        tempA := 2 * tempL * cos(temp);
        Rigg.RealGlied[fpSalingH] := tempH;
        Rigg.RealGlied[fpSalingA] := tempA;
      end;
      end;

      { Definitionsbereich bestimmen und Berechnungsschleife starten }
      Anfang := StrToInt(XminEdit.Text);
      Ende := StrToInt(XmaxEdit.Text);
      for i := 0 to LNr do
      begin
        if i mod 5 = 0 then
          ProgressPosition := i;

        Antrieb := Anfang + (Ende - Anfang) * i / LNr;

        { Antrieb ansteuern }
        case xn of
          xpController: Rigg.RealGlied[fpController] := Antrieb;
          xpWinkel: Rigg.RealGlied[fpWinkel] := Antrieb * P180;
          xpVorstag: Rigg.RealGlied[fpVorstag] := Antrieb;
          xpWante: Rigg.RealGlied[fpWante] := Antrieb;
          xpWOben: Rigg.RealGlied[fpWoben] := Antrieb;
          xpSalingH: Rigg.RealGlied[fpSalingH] := Antrieb;
          xpSalingA: Rigg.RealGlied[fpSalingA] := Antrieb;
          xpSalingL:
          begin
            case SalingTyp of
              stDrehbar: Rigg.RealGlied[fpSalingL] := Antrieb;
              stFest:
        begin
          tempL := Rigg.RealGlied[fpSalingL];
          temp := Antrieb/tempL;
          tempH := temp * Rigg.RealGlied[fpSalingH];
          tempA := temp * Rigg.RealGlied[fpSalingA];
          Rigg.RealGlied[fpSalingH] := tempH;
          Rigg.RealGlied[fpSalingA] := tempA;
              end;
            end;
          end;
          xpSalingW:
        begin
          temp := Antrieb * P180;
          tempL := Rigg.RealGlied[fpSalingL];
          tempH := tempL * sin(temp);
          tempA := 2 * tempL * cos(temp);
          Rigg.RealGlied[fpSalingH] := tempH;
          Rigg.RealGlied[fpSalingA] := tempA;
        end;
        end;

        { Berechnen }
        if SalingTyp = stFest then
        begin
          if (XCombo.Text = WinkelString) or
             (PCombo.Text = WinkelString) then
            Rigg.UpdateGetriebeFS
          else
            Rigg.BerechneWinkel;
        end
        else
        begin
          Rigg.UpdateGetriebe;
        end;
        Rigg.UpdateRigg;
        PunktOK := Rigg.GetriebeOK and Rigg.MastOK and Rigg.RiggOK;

        { Ergebnisse einspeichern }
        if yavVorstagSpannung in YAchseSet then
        begin
          j := YAchseRecordList[yavVorstagSpannung].ArrayIndex;
          if PunktOK then
            af[p,j,i] := Rigg.rF[14]
          else
            af[p,j,i] := 0;
        end;
        if yavWantenSpannung in YAchseSet then
        begin
          j := YAchseRecordList[yavWantenSpannung].ArrayIndex;
          if PunktOK then
            af[p,j,i] := Rigg.rF[8]
          else
            af[p,j,i] := 0;
        end;
        if yavMastfallF0F in YAchseSet then
        begin
          j := YAchseRecordList[yavMastfallF0F].ArrayIndex;
          af[p,j,i] := Abstand(Rigg.rP[ooF0],Rigg.rP[ooF]);
        end;
        if yavMastfallF0C in YAchseSet then
        begin
          j := YAchseRecordList[yavMastfallF0C].ArrayIndex;
          af[p,j,i] := Abstand(Rigg.rP[ooF0],Rigg.rP[ooC]);
        end;
        if yavAuslenkungC in YAchseSet then
        begin
          j := YAchseRecordList[yavAuslenkungC].ArrayIndex;
          if PunktOK then
            af[p,j,i] := Abstand(Rigg.rP[ooC],Rigg.rPe[ooC])
          else
            af[p,j,i] := 0;
        end;
        if yavDurchbiegungHD in YAchseSet then
        begin
          j := YAchseRecordList[yavDurchbiegungHD].ArrayIndex;
          af[p,j,i] := Rigg.hd;
        end;
        if yavRF00 in YAchseSet then
        begin
          j := YAchseRecordList[yavRF00].ArrayIndex;
          if PunktOK then
            af[p,j,i] := Rigg.rF[0]
          else
            af[p,j,i] := 0;
        end;
        if yavRF01 in YAchseSet then
        begin
          j := YAchseRecordList[yavRF01].ArrayIndex;
          if PunktOK then
            af[p,j,i] := Rigg.rF[1]
          else
            af[p,j,i] := 0;
        end;
        if yavRF03 in YAchseSet then
        begin
          j := YAchseRecordList[yavRF03].ArrayIndex;
          if PunktOK then
            af[p,j,i] := Rigg.rF[3]
          else
            af[p,j,i] := 0;
        end;
        if yavRF05 in YAchseSet then
        begin
          j := YAchseRecordList[yavRF05].ArrayIndex;
          if PunktOK then
            af[p,j,i] := Rigg.rF[5]
          else
            af[p,j,i] := 0;
        end;
        if yavRF10 in YAchseSet then
        begin
          j := YAchseRecordList[yavRF10].ArrayIndex;
          if PunktOK then
            af[p,j,i] := Rigg.rF[10]
          else
            af[p,j,i] := 0;
        end;
        if yavRF11 in YAchseSet then
        begin
          j := YAchseRecordList[yavRF11].ArrayIndex;
          if PunktOK then
            af[p,j,i] := Rigg.rF[11]
          else
            af[p,j,i] := 0;
        end;
        if yavRF13 in YAchseSet then
        begin
          j := YAchseRecordList[yavRF13].ArrayIndex;
          if PunktOK then
            af[p,j,i] := Rigg.rF[13]
          else
            af[p,j,i] := 0;
        end;
      end;
    end;

  finally
    { Getriebe wiederherstellen }
    Rigg.ProofRequired := True;
    Rigg.Glieder := InputRec;
    UpdateGetriebe;
  end;
end;

procedure TChartForm.Draw;
begin

end;

procedure TChartForm.DrawNormal;
begin
  FShowGroup := False;
  LoadNormal;
  DrawInternal;
end;

procedure TChartForm.DrawInternal;
begin
  FShowGroup := False;
  DoLegend;
  if FValid then
  begin
    YTitle := GetYText(YCombo.Text);
    XTitle := XAchseText;
    PTitle := Format('%s', [ParamText]);
    Xmin := XAchseMin;
    Xmax := XAchseMax;
    LookForYMinMax;
  end
  else
  begin
    TopTitle := TopTitleTestString;
    XTitle := BottomTitleTestString;
    PTitle := ParamTitleTestString;
    if FStatus = [] then
      YTitle := StatusResetString // 'Diagramm wurde zurückgesetzt'
    else if csBerechnet in FStatus then
      YTitle := StatusNotComputedString // 'Kurve wurde nicht berechnet!'
    else if csGeladen in FStatus then
      YTitle := StatusNotLoadedString; // 'Kurve wurde nicht geladen!';

    Xmin := 0;
    Xmax := 100;
    Ymin := 0;
    Ymax := 100;

    InitStraightLine;
  end;

  if (Ymax - Ymin < 0.1) then
  begin
    Ymin := Ymin - 0.1;
    Ymax := Ymax + 0.1;
  end;

  if (Xmax - Xmin < 0.1) then
  begin
    Xmin := Xmin - 0.1;
    Ymax := Ymax + 0.1;
  end;

  DrawToChart;
end;

procedure TChartForm.YComboChange(Sender: TObject);
var
  j, p: Integer;
begin
  if (YCombo.ItemIndex < 0) or (YCombo.ItemIndex > VNr-1) then
  begin
    FValid := False;
    YMinEdit.Text := YMinEditString;
    YMaxEdit.Text := YMaxEditString;
    DrawInternal;
    Exit;
  end;
  j := ComboIndexToCurve(YCombo.ItemIndex);
  if not Valid then
  begin
    YMinEdit.Text := YMinString;
    YMaxEdit.Text := YMaxString;
    DrawInternal;
    Exit;
  end;
  for p := 0 to ParamCount - 1 do
    bf[p] := af[p, j];
  UpdateYMinMax;
  DrawInternal; { auch TestF zeichnen }
end;

procedure TChartForm.ShowTogetherBtnClick(Sender: TObject);
begin
  if FStatus = [] then
    Exit;
  if KurvenzahlSpinner.Position > ParamCount then
    KurvenzahlSpinner.Position := ParamCount;
  ShowTogether(KurvenZahlSpinner.Position);
  FShowGroup := True;
end;

procedure TChartForm.ShowTogether(ParamNo: Integer);
var
  i, j, p: Integer;
  YAV: TYAchseValue;
  min, max, diff, temp: double;
  tempParamCount: Integer;
  tempPText: TYAchseStringArray;
begin
  if ParamNo > ParamCount then
    ParamNo := ParamCount;

  { bf füllen }
  p := 0; { p steht hier für die Anzahl der Kurven in YAchseSet }
  for i := 0 to YCombo.Items.Count - 1 do
  begin
    YAV := YAchseSortedList[i];
    if YAV in YAchseSet then
    begin
      j := YAchseRecordList[YAV].ArrayIndex;
      if p = PNr then
        break;
      bf[p] := af[ParamNo - 1, j];
      GroupText[p] := YAchseRecordList[YAV].ComboText;
      p := p + 1;
    end;
  end;

  GroupKurvenZahl := p;
  for p := 0 to GroupKurvenZahl - 1 do
  begin
    { Maximum und Minimum ermitteln }
    max := bf[p, 0];
    min := max;
    for i := 0 to LNr do
    begin
      if bf[p, i] > max then
        max := bf[p, i];
      if bf[p, i] < min then
        min := bf[p, i];
    end;

    { Normieren }
    diff := max - min;
    temp := p * 100 / GroupKurvenZahl;
    if max-min = 0 then
      for i := 0 to LNr do
        bf[p, i] := temp
    else
    begin
      for i := 0 to LNr do
      try
        bf[p, i] := (bf[p, i] - min) * 100 / diff;
      except on EMathError do
        bf[p, i] := 0;
      end;
    end;
  end;

  FValid := True;
  YMinEdit.Text := YMinString;
  YMaxEdit.Text := YMaxString;

  TopTitle := '';
  YTitle := AllCurvesNormalizedString; // 'Alle Kurven normiert [%]';
  XTitle := XAchseText;
  PTitle := Format('%s%d', [PIdentString, ParamNo]); //Nr.1

  Xmin := XAchseMin;
  Xmax := XAchseMax;
  Ymin := 0;
  Ymax := 100;

  if (Ymax-Ymin < 0.1) then
  begin
    Ymin := Ymin - 0.1;
    Ymax := Ymax + 0.1;
  end;
  if (Xmax-Xmin < 0.1) then
  begin
    Xmin := Xmin - 0.1;
    Ymax := Ymax + 0.1;
  end;

  tempParamCount := ParamCount;
  tempPText := PText;

  ParamCount := GroupKurvenZahl;
  PText := GroupText;

  DoLegend;
  DrawToChart;

  ParamCount := tempParamCount;
  PText := tempPText;
end;

procedure TChartForm.UpdateYMinMax;
var
  min, max: double;
  i, j, p: Integer;
begin
  { Maximum und Minimum suchen für eine einzelne Kurve. }
  p := PSpinner.Position - 1;
  j := ComboIndexToCurve(YCombo.ItemIndex);
  if not Valid then
  begin
    { MessageBeep(MB_ICONEXCLAMATION); }
    YMinEdit.Text := YMinString;
    YMaxEdit.Text := YMaxString;
    Exit;
  end;
  TempF := af[p, j];
  max := TempF[0];
  min := max;
  for i := 0 to LNr do
  begin
    if TempF[i] > max then
      max := TempF[i];
    if TempF[i] < min then
      min := TempF[i];
  end;
  YMinEdit.Text := Format('%6.2f', [min]);
  YMaxEdit.Text := Format('%6.2f', [max]);
end;

procedure TChartForm.LookForYMinMax;
var
  i, j, p: Integer;
begin
  { Maximum und Minimum suchen über alle Parameter hinweg }
  if RggDocument.CalcTyp = ctQuerKraftBiegung then
  begin
    if (YCombo.Text = VorstagSpannungString) or
       (YCombo.Text = WantenSpannungString) then
    begin
      Ymax := 5000; { 5000 N }
      Ymin := -1000; { -1000 N }
      Exit;
    end;
    if (YCombo.Text = ElasticityPointCString) then
    begin
      YMax := 1000; { 1000 mm }
      YMin := 0;
      Exit;
    end;
  end;

  p := PSpinner.Position - 1; { Index für Parameter }
  j := ComboIndexToCurve(YCombo.ItemIndex); { Index für Kurve }
  if not Valid then
  begin
    { MessageBeep(MB_ICONEXCLAMATION); }
    YMinEdit.Text := YMinString;
    YMaxEdit.Text := YMaxString;
    Exit;
  end;
  Ymax := af[p, j, 0];
  Ymin := Ymax;
  for p := 0 to ParamCount-1 do
  begin
    TempF := af[p, j];
    for i := 0 to LNr do
    begin
      if TempF[i] > Ymax then
        Ymax := TempF[i];
      if TempF[i] < Ymin then
        Ymin := TempF[i];
    end;
  end;
end;

function TChartForm.GetYText(Text: string): string;
var
  YAV: TYAchseValue;
begin
  result := '';
  for YAV := Low(TYAchseValue) to High(TYAchseValue) do
    if YAchseRecordList[YAV].ComboText = Text then
    begin
      result := YAchseRecordList[YAV].Text;
      break;
    end;
end;

procedure TChartForm.GetMemoText;
var
  p: Integer;
  YAV: TYAchseValue;
  xpName: TxpName;
  T: TTrimmTabDaten;
begin
  Inc(MemoCounter);
  with MemoLines do
  begin
    Clear;
    { SalingTyp }
    case SalingTyp of
      stFest: Add('SalingTyp: Feste Salinge');
      stDrehbar: Add('SalingTyp: Drehbare Salinge');
      stOhne_2: Add('SalingTyp: Ohne Salinge (Mast biegsam)');
      stOhne: Add('SalingTyp: Ohne Salinge (Mast starr)');
    end;
    { ControllerTyp }
    case Rigg.ControllerTyp of
      ctDruck: Add('ControllerTyp: Controller überträgt Druck');
      ctOhne: Add('ControllerTyp: kein Controller');
    end;
    { CalcTyp }
    if SalingTyp = stOhne then
      Add('BerechnungsTyp: Wantenkraft vorgegeben');
    if SalingTyp <> stOhne then
    case Rigg.CalcTyp of
      ctQuerKraftBiegung: Add('BerechnungsTyp: nur Quekraftbiegung');
      ctBiegeKnicken: Add('BerechnungsTyp: Biegeknicken');
      ctKraftGemessen: begin
        Add('BerechnungsTyp: Trimmtabelle verwendet');
        Add('');
        Add('Trimmtabelle:');
        T := Rigg.TrimmTab.TrimmTabDaten;
        case T.TabellenTyp of
          itKonstante:
          begin
            Add('  KurvenTyp: Konstante');
            Add(Format('  Konstante Kraft: %g N',[T.a1]));
            Add(Format('  Maximale Kraft: %g N',[T.x2]));
            Add(Format('  Maximale Auslenkung: %g mm',[T.x1]));
          end;
          itGerade:
          begin
            Add('  KurvenTyp: Gerade');
            Add(Format('  Steigung: %g N/mm',[1/T.a1]));
            Add(Format('  Maximale Kraft: %g N',[T.x2]));
            Add(Format('  Maximale Auslenkung: %g mm',[T.a1*T.x2]));
          end;
          itParabel:
          begin
            Add('  KurvenTyp: Parabel');
            Add('  Weg/mm = a2*(Kraft/N)^2 + a1*Kraft/N');
            Add(Format('  a1: %g', [T.a1]));
            Add(Format('  a2: %g', [T.a2]));
            Add(Format('  Maximale Kraft: %g N', [T.x2]));
            Add(Format('  Maximale Auslenkung: %g mm', [T.a2*T.x2*T.x2 + T.a1*T.x2]));
          end;
          itBezier:
          begin
            Add('  KurvenTyp: Bezier');
            Add(Format('  KontrollPunkt bei (%g mm, %g N)', [T.a1, T.a2]));
            Add(Format('  Endpunkt bei (%g mm, %g N)', [T.x1, T.x2]));
          end;
        end;
      end;
    end;
    { X }
    Add('');
    Add('XAchse: ' + XAchseText);
    Add(Format('  Use AP: %s', [BoolStr[APBtn.Down]]));
    Add(Format('  AP Width: %d', [APWidth]));
    Add(Format('  %d ... %d', [XAchseMin, XAchseMax]));
    { P }
    if ParamCount > 1 then
    begin
      Add('');
      Add('Parameter: ' + ParamText);
      with MemoLines do
      begin
        for p := 0 to ParamCount-1 do
          Add(Format('  #%d: %s (%s) ', [p + 1, PText[p], PColorText[p]]));
      end;
    end;
    { Y }
    if YAchseSet <> [] then
    begin
      Add('');
      Add('YAchse: Berechnete Kurven');
      for YAV := Low(TYAchseValue) to High(TYAchseValue) do
        if YAV in YAchseSet then
          Add('  ' + YAchseRecordList[YAV].ComboText);
    end;
    { Längen }
    xpName := GetTsbName(PComboText);
    Add('');
    Add('Rigg: Einstellwerte');
    with Rigg do
    begin
      if (ControllerTyp = ctDruck) and (xpName <> xpController) then
        Add(Format('  Controller: %g mm', [RealGlied[fpController]]));
      if (SalingTyp <> stOhne) and ManipulatorMode and (xpName <> xpWinkel) then
        Add(Format(  '  Winkel: %g Grad', [RealGlied[fpWinkel] * D180]));
      if (SalingTyp <> stOhne) and not ManipulatorMode and (xpName <> xpVorstag) then
        Add(Format('  Vorstag: %g mm', [RealGlied[fpVorstag]]));
      if xpName <> xpWante then
        Add(Format(  '  Wante: %g mm', [RealGlied[fpWante]]));
      case SalingTyp of stFest, stDrehbar:
        if xpName <> xpWoben then
          Add(Format('  WanteOben: %g mm', [RealGlied[fpWoben]]));
      end;

      if (SalingTyp = stFest) and not (xpName in [xpSalingH, xpSalingL, xpSalingW]) then
        Add(Format('  SalingHöhe: %g mm', [RealGlied[fpSalingH]]));
      if (SalingTyp = stFest) and not (xpName in [xpSalingA, xpSalingL, xpSalingW]) then
        Add(Format('  SalingAbstand: %g mm', [RealGlied[fpSalingA]]));
      if (SalingTyp = stFest) and (xpName = xpSalingL) then
        { SalingWinkel ausgeben }
        if RealGlied[fpSalingA] <> 0 then
          Add(Format('  SalingWinkel: %g Grad',[
            arctan2(RealGlied[fpSalingH], RealGlied[fpSalingA]) * D180]));
      if (SalingTyp = stFest) and (xpName = xpSalingW) then
        Add(Format('  SalingLänge: %6.2f mm', [RealGlied[fpSalingL]]));

      if (SalingTyp = stDrehbar) and (xpName <> xpSalingL) then
        Add(Format('  SalingLänge: %6.2f mm', [RealGlied[fpSalingL]]));
      if (SalingTyp = stOhne) and (xpName <> xpVorstag) then { nicht VorstagOS - ok }
        Add(Format('  Vorstag: %g mm', [RealGlied[fpVorstagOS]]));
      if (SalingTyp = stOhne) and (xpName <> xpWPowerOS) then
        Add(Format('  Wantenspannung: %g N', [RealGlied[fpWPowerOS]]));
    end;
    { Koordinaten }
    Add('');
    Add('Rumpf: Koordinaten (x,y,z) [mm]');
    with Rigg do
    begin
      Add(Format('  A0(%g,%g,%g)', [rP[ooA0,x],rP[ooA0,y],rP[ooA0,z]]));
      Add(Format('  B0(%g,%g,%g)', [rP[ooB0,x],rP[ooB0,y],rP[ooB0,z]]));
      Add(Format('  C0(%g,%g,%g)', [rP[ooC0,x],rP[ooC0,y],rP[ooC0,z]]));
      Add(Format('  D0(%g,%g,%g)', [rP[ooD0,x],rP[ooD0,y],rP[ooD0,z]]));
      Add(Format('  E0(%g,%g,%g)', [rP[ooE0,x],rP[ooE0,y],rP[ooE0,z]]));
      Add(Format('  F0(%g,%g,%g)', [rP[ooF0,x],rP[ooF0,y],rP[ooF0,z]]));
    end;
    { Mast }
    Add('');
    Add('Mast:');
    with Rigg do
    begin
      Add(Format('  D0D: %d mm (Saling)', [Round(MastUnten)]));
      Add(Format('  D0C: %d mm (Vorstag)', [Round(MastUnten + MastOben)]));
      Add(Format('  D0F: %d mm (Top)', [Round(MastLaenge)]));
      Add(Format('  Biegesteifigkeit EI: %d Nm^2', [MastEI]));
    end;
    { Exit Counters }
    Add('');
    with Rigg do
    begin
      if ExitCounter1 > 0 then Add(Format('  EC 1: %d ', [ExitCounter1]));
      if ExitCounter2 > 0 then Add(Format('  EC 2: %d ', [ExitCounter2]));
      if ExitCounter3 > 0 then Add(Format('  EC 3: %d ', [ExitCounter3]));
      if ExitCounter4 > 0 then Add(Format('  EC 4: %d ', [ExitCounter4]));
      if ExitCounter5 > 0 then Add(Format('  EC 5: %d ', [ExitCounter5]));
      if ExitCounter6 > 0 then Add(Format('  EC 6: %d ', [ExitCounter6]));
    end;
    Add(Format('Memo Counter: %d', [MemoCounter]));
    Add(Format('Calc Counter: %d', [CalcCounter]));
  end;
end;

procedure TChartForm.XComboChange(Sender: TObject);
begin
  UpdateXMinMax;
  FXTextClicked := XCombo.Text;
  if (XCombo.Text = XComboText) and (csBerechnet in FStatus) then
    XLED.Brush.Color := clLime
  else
    XLED.Brush.Color := clRed;
  UpdatePCombo(SalingTyp);
end;

procedure TChartForm.PComboChange(Sender: TObject);
begin
  UpdatePMinMax;
  FPTextClicked := PCombo.Text;
  if (PCombo.Text = PComboText) and (csBerechnet in FStatus) then
    PLED.Brush.Color := clLime
  else
    PLED.Brush.Color := clRed;
end;

procedure TChartForm.UpdateXCombo(SalingTyp: TSalingTyp);
var
  i: Integer;
begin
  with XCombo.Items do
  begin
    Clear;
    if SalingTyp = stFest then
    begin
      Add(ControllerString);
      Add(VorstagString);
      Add(WinkelString);
      Add(WanteString);
      Add(WanteObenString);
      Add(SalingHString);
      Add(SalingAString);
      Add(SalingLString);
      Add(SalingWString);
    end;
    if SalingTyp = stDrehbar then
    begin
      Add(ControllerString);
      Add(VorstagString);
      Add(WanteString);
      Add(WanteObenString);
      Add(SalingLString);
    end;
    if SalingTyp = stOhne_2 then
    begin
      Add(ControllerString);
      Add(VorstagString);
      Add(WanteString);
    end;
    if SalingTyp = stOhne then
    begin
      Add(VorstagString);
    end;
  end;
  XCombo.ItemIndex := XCombo.Items.IndexOf(VorstagString);
  for i := 0 to XCombo.Items.Count-1 do
    if (XCombo.Items[i] = FXTextClicked) then
    begin
      XCombo.ItemIndex := i;
      break;
    end;
  UpdatePCombo(SalingTyp);
end;

procedure TChartForm.UpdatePCombo(SalingTyp: TSalingTyp);
var
  i: Integer;
begin
  with PCombo.Items do
  begin
    Clear;
    Add(NoParamString);
    if XCombo.Text = ControllerString then
    begin
      if SalingTyp = stFest then
      begin
        Add(VorstagString);
        Add(WinkelString);
        Add(WanteString);
        Add(WanteObenString);
        Add(SalingHString);
        Add(SalingAString);
        Add(SalingLString);
        Add(SalingWString);
      end;
      if SalingTyp = stDrehbar then
      begin
        Add(VorstagString);
        Add(WanteString);
        Add(WanteObenString);
        Add(SalingLString);
      end;
      if SalingTyp = stOhne_2 then
      begin
        Add(VorstagString);
        Add(WanteString);
      end;
    end
    else if (XCombo.Text = VorstagString) then
    begin
      if SalingTyp = stFest then
      begin
        Add(ControllerString);
        Add(WanteString);
        Add(WanteObenString);
        Add(SalingHString);
        Add(SalingAString);
        Add(SalingLString);
        Add(SalingWString);
      end;
      if SalingTyp = stDrehbar then
      begin
        Add(ControllerString);
        Add(WanteString);
        Add(WanteObenString);
        Add(SalingLString);
      end;
      if SalingTyp = stOhne_2 then
      begin
        Add(ControllerString);
        Add(WanteString);
      end;
    end
    else if (XCombo.Text = WinkelString) then
    begin
      if SalingTyp = stFest then
      begin
        Add(ControllerString);
        Add(WanteString);
        Add(WanteObenString);
        Add(SalingHString);
        Add(SalingAString);
        Add(SalingLString);
        Add(SalingWString);
      end;
    end
    else if (XCombo.Text = WanteString) then
    begin
      if SalingTyp = stFest then
      begin
        Add(ControllerString);
        Add(VorstagString);
        Add(WinkelString);
        Add(WanteObenString);
        Add(SalingHString);
        Add(SalingAString);
        Add(SalingLString);
        Add(SalingWString);
      end;
      if SalingTyp = stDrehbar then
      begin
        Add(ControllerString);
        Add(VorstagString);
        Add(WanteObenString);
        Add(SalingLString);
      end;
      if SalingTyp = stOhne_2 then
      begin
        Add(ControllerString);
        Add(VorstagString);
      end;
    end
    else if (XCombo.Text = WanteObenString) then
    begin
      if SalingTyp = stFest then
      begin
        Add(ControllerString);
        Add(VorstagString);
        Add(WinkelString);
        Add(WanteString);
        Add(SalingHString);
        Add(SalingAString);
        Add(SalingLString);
        Add(SalingWString);
      end;
      if SalingTyp = stDrehbar then
      begin
        Add(ControllerString);
        Add(VorstagString);
        Add(WanteString);
        Add(SalingLString);
      end;
    end
    else if (XCombo.Text = SalingHString) then
    begin
      if SalingTyp = stFest then
      begin
        Add(ControllerString);
        Add(VorstagString);
        Add(WinkelString);
        Add(WanteString);
        Add(WanteObenString);
        Add(SalingAString);
        Add(SalingLString);
        Add(SalingWString);
      end;
    end
    else if (XCombo.Text = SalingAString) then
    begin
      if SalingTyp = stFest then
      begin
        Add(ControllerString);
        Add(VorstagString);
        Add(WinkelString);
        Add(WanteString);
        Add(WanteObenString);
        Add(SalingHString);
        Add(SalingLString);
        Add(SalingWString);
      end;
    end
    else if (XCombo.Text = SalingLString) then
    begin
      if SalingTyp = stFest then
      begin
        Add(ControllerString);
        Add(VorstagString);
        Add(WinkelString);
        Add(WanteString);
        Add(WanteObenString);
        Add(SalingHString);
        Add(SalingAString);
        Add(SalingWString);
      end;
      if SalingTyp = stDrehbar then
      begin
        Add(ControllerString);
        Add(VorstagString);
        Add(WanteString);
        Add(WanteObenString);
      end;
    end
    else if (XCombo.Text = SalingWString) then
    begin
      if SalingTyp = stFest then
      begin
        Add(ControllerString);
        Add(VorstagString);
        Add(WinkelString);
        Add(WanteString);
        Add(WanteObenString);
      end;
    end;
  end;
  PCombo.ItemIndex := 0;
  for i := 0 to PCombo.Items.Count-1 do
    if (PCombo.Items[i] = FPTextClicked) then
    begin
      PCombo.ItemIndex := i;
      break;
    end;

  UpdatePMinMax;
end;

function TChartForm.GetXText(Text: string): string;
var
  s: string;
begin
  if Text = ControllerString then s := ControllerText
  else if Text = WinkelString then s := WinkelText
  else if Text = VorstagString then s := VorstagText
  else if Text = WanteString then s := WanteText
  else if Text = WanteObenString then s := WanteObenText
  else if Text = SalingHString then s := SalingHText
  else if Text = SalingAString then s := SalingAText
  else if Text = SalingLString then s := SalingLText
  else if Text = SalingWString then s := SalingWText;
  result := s;
end;

function TChartForm.GetPText(Text: string): string;
var
  s: string;
begin
  if Text = ControllerString then s := ControllerText
  else if Text = WinkelString then s := WinkelText
  else if Text = VorstagString then s := VorstagText
  else if Text = WanteString then s := WanteText
  else if Text = WanteObenString then s := WanteObenText
  else if Text = SalingHString then s := SalingHText
  else if Text = SalingAString then s := SalingAText
  else if Text = SalingLString then s := SalingLText
  else if Text = SalingWString then s := SalingWText;
  result := s;
end;

function TChartForm.GetTsbName(Value: string): TxpName;
var
  xp: TxpName;
begin
  xp := xpController;
  if Value = WinkelString then xp := xpWinkel
  else if Value = VorstagString then xp := xpVorstag
  else if Value = WanteString then xp := xpWante
  else if Value = WanteObenString then xp := xpWoben
  else if Value = SalingHString then xp := xpSalingH
  else if Value = SalingAString then xp := xpSalingA
  else if Value = SalingLString then xp := xpSalingL
  else if Value = SalingWString then xp := xpSalingW
  else if Value = VorstagOhneSalingString then xp := xpVorstagOS
  else if Value = WantenkraftOhneSalingString then xp := xpWPowerOS;
  result := xp;
end;

procedure TChartForm.UpdateXMinMax;
var
  s: string;
  xp: TxpName;
  tempMin, tempMax, tempIst, Minimum, Maximum: Integer;
  f: TRggSB;
begin
  s := XCombo.Text;
  xp := GetTsbName(s);
  if not (xp in XSet) then
    Exit;

  if (SalingTyp = stFest) and (xp = xpSalingL) then
  begin
    SalingDreieck.CopyFromRigg(Rigg);
    tempMin := Ceil(SalingDreieck.Saling_LMin);
    tempMax := Floor(SalingDreieck.Saling_LMax);
    tempIst := Round(SalingDreieck.Saling_L);
  end
  else if xp = xpSalingW then
  begin
    SalingDreieck.CopyFromRigg(Rigg);
    tempMin := Ceil(SalingDreieck.Saling_WMin * D180);
    tempMax := Floor(SalingDreieck.Saling_WMax * D180);
    tempIst := Round(SalingDreieck.Saling_W * D180);
  end
  else
  begin
    f := Rigg.GSB.GetSB(TsbName(xp));
    tempMin := Round(f.Min);
    tempMax := Round(f.Max);
    tempIst := Round(f.Ist);
  end;

  try
    if BereichBtn.Down then
    begin
      Minimum := tempMin;
      Maximum := tempMax;
    end
    else if APBtn.Down then
    begin
      Minimum := tempMin;
      Maximum := tempMax;
      if (tempIst - APWidth) > tempMin then
        Minimum := tempIst - APWidth;
      if (tempIst + APWidth) < tempMax then
        Maximum := tempIst + APWidth;
    end
    else
    begin
      try
        Minimum := StrToInt(XMinEdit.Text);
        Maximum := StrToInt(XMaxEdit.Text);
        if Minimum < tempMin then Minimum := tempMin;
        if Maximum > tempMax then Maximum := tempMax;
        if Minimum > Maximum then Minimum := tempMin;
        if Maximum < Minimum then Maximum := tempMax;
      except
        on EConvertError do
        begin
          Minimum := tempMin;
          Maximum := tempMax;
        end;
      end;
    end;
    XMinEdit.Text := IntToStr(Minimum);
    XMaxEdit.Text := IntToStr(Maximum);
  except
    on ERangeError do
    begin
      XMinEdit.Text := IntToStr(0);
      XMaxEdit.Text := IntToStr(100);
      Valid := False;
      XLED.Brush.Color := clRed;
    end;
  end;
end;

procedure TChartForm.UpdatePMinMax;
var
  s: string;
  xp: TxpName;
  tempMin, tempMax, tempIst, Minimum, Maximum: Integer;
  f: TRggSB;
begin
  s := PCombo.Text;
  if s = NoParamString then
  begin
    PMinEdit.Text := IntToStr(0);
    PMaxEdit.Text := IntToStr(0);
    KurvenZahlSpinner.Position := 1;
    Exit;
  end;

  xp := GetTsbName(s);
  if not (xp in XSet) then
    Exit;

  if (SalingTyp = stFest) and (xp = xpSalingL) then
  begin
    SalingDreieck.CopyFromRigg(Rigg);
    tempMin := Ceil(SalingDreieck.Saling_LMin);
    tempMax := Floor(SalingDreieck.Saling_LMax);
    tempIst := Round(SalingDreieck.Saling_L);
  end
  else if xp = xpSalingW then
  begin
    SalingDreieck.CopyFromRigg(Rigg);
    tempMin := Ceil(SalingDreieck.Saling_WMin * D180);
    tempMax := Floor(SalingDreieck.Saling_WMax * D180);
    tempIst := Round(SalingDreieck.Saling_W * D180);
  end
  else
  begin
    f := Rigg.GSB.GetSB(TsbName(xp));
    tempMin := Round(f.Min);
    tempMax := Round(f.Max);
    tempIst := Round(f.Ist);
  end;

  try
    if BereichBtn.Down then
    begin
      Minimum := tempMin;
      Maximum := tempMax;
    end
    else if APBtn.Down then
    begin
      Minimum := tempMin;
      Maximum := tempMax;
      if (tempIst - APWidth) > tempMin then
        Minimum := tempIst - APWidth;
      if (tempIst + APWidth) < tempMax then
        Maximum := tempIst + APWidth;
    end
    else
    begin
      try
        Minimum := StrToInt(PMinEdit.Text);
        Maximum := StrToInt(PMaxEdit.Text);
        if Minimum < tempMin then Minimum := tempMin;
        if Maximum > tempMax then Maximum := tempMax;
        if Minimum > Maximum then Minimum := tempMin;
        if Maximum < Minimum then Maximum := tempMax;
      except
        on EConvertError do
        begin
          Minimum := tempMin;
          Maximum := tempMax;
        end;
      end;
    end;
    PMinEdit.Text := IntToStr(Minimum);
    PMaxEdit.Text := IntToStr(Maximum);
  except
    on ERangeError do
    begin
      PMinEdit.Text := IntToStr(0);
      PMaxEdit.Text := IntToStr(100);
      Valid := False;
      PLED.Brush.Color := clRed;
    end;
  end;
end;

function  TChartForm.CheckBeforeCalc: Boolean;
begin
  result := True;
  if (XMinEdit.Text = XMinEditString) or (XMaxEdit.Text = XMaxEditString) then
    UpdateXMinMax;
  if (PMinEdit.Text = PMinEditString) or (PMaxEdit.Text = PMaxEditString) then
    UpdatePMinMax;
  if not ValidateInput(XMinEdit) then result := False;
  if not ValidateInput(XMaxEdit) then result := False;
  if not ValidateInput(PMinEdit) then result := False;
  if not ValidateInput(PMaxEdit) then result := False;
end;

procedure TChartForm.DoAfterCalc;
begin
  { X }
  XLED.Brush.Color := clLime;
  XComboText := XCombo.Text;
  XAchseText := GetXText(XCombo.Text);
  XAchseMin := StrToInt(XMinEdit.Text);
  XAchseMax := StrToInt(XMaxEdit.Text);

  { Parameter }
  PLED.Brush.Color := clLime;
  PComboText := PCombo.Text;
  ParamText := GetPText(PCombo.Text);
  ParamMin := StrToInt(PMinEdit.Text);
  ParamMax := StrToInt(PMaxEdit.Text);
end;

procedure TChartForm.ChartMenuClick(Sender: TObject);
begin
  BereichItem.Checked := BereichBtn.Down;
  APItem.Checked := APBtn.Down;
end;

procedure TChartForm.BereichBtnClick(Sender: TObject);
begin
  UpdateXMinMax;
  UpdatePMinMax;
end;

procedure TChartForm.APEditChange(Sender: TObject);
begin
  APWidth := APSpinner.Position;
end;

procedure TChartForm.UpdateRiggItemClick(Sender: TObject);
begin
{$ifdef RG19}
  if not Assigned(RggDocument) then
    Exit;
  if (csGeladen in FStatus) or (csBerechnet in FStatus)then
  begin
    RiggModul.Neu(RggDocument);
    RiggModul.ViewModelMain.Caption := 'Rigg';
  end;
{$endif}
end;

procedure TChartForm.InitMenu;
var
  p: TMenuItem;
  mi: TMenuItem;

  function AddP(AName: string): TMenuItem;
  begin
    mi := TMenuItem.Create(MainMenu);
    mi.Name := AName;
    p := mi;
    MainMenu.Items.Add(p);
    result := mi;
  end;

  function AddI(AName: string): TMenuItem;
  begin
    mi := TMenuItem.Create(MainMenu);
    mi.Name := AName;
    p.Add(mi);
    result := mi;
  end;

begin
  MainMenu := TMainMenu.Create(Self);

  ChartMenu := AddP('ChartMenu');
  mi.Caption := 'Diagram&m';
  mi.GroupIndex := 8;
  mi.Hint := '  Diagramm Optionen';
  mi.OnClick := ChartMenuClick;

  BerechnenItem := AddI('BerechnenItem');
  mi.Caption := '&Berechnen...';
  mi.Hint := '  Berechnung starten';
  mi.OnClick := CalcItemClick;

  ResetItem := AddI('ResetItem');
  mi.Caption := '&Zurücksetzen';
  mi.Hint := '  Diagramm für Neuberechnung freigeben (nach Fehler)';
  mi.OnClick := BuissyItemClick;

  UpdateChartItem := AddI('UpdateChartItem');
  mi.Caption := 'Diagramm aktualisieren';
  mi.Hint := '  Istwerte neu einlesen';
  mi.OnClick := UpdateChartItemClick;

  UpdateRiggItem := AddI('UpdateRiggItem');
  mi.Caption := 'Rigg aktualisieren';
  mi.Hint := 'Erzeugungsdaten zurückschreiben';
  mi.OnClick := UpdateRiggItemClick;

  N1 := AddI('N1');
  mi.Caption := '-';

  APItem := AddI('APItem');
  mi.Caption := 'Arbeits&punkt';
  mi.Checked := True;
  mi.Hint := '  automatische X - Werte: Arbeitspunkt +/- 30';
  mi.OnClick := APItemClick;

  BereichItem := AddI('BereichItem');
  mi.Caption := 'Be&reich';
  mi.Hint := '  automatische X - Werte: gesamter Bereich';
  mi.OnClick := BereichItemClick;

  N2 := AddI('N2');
  mi.Caption := '-';

  AuswahlItem := AddI('AuswahlItem');
  mi.Caption := '&Auswahl Y ...';
  mi.Hint := '  Auswahl der Größen für die Y-Achse';
  mi.OnClick := YAuswahlClick;

  MemoItem := AddI('MemoItem');
  mi.Caption := 'Erzeugungsdaten...';
  mi.Hint := '  Erzeugungsdaten anzeigen';
  mi.OnClick := MemoItemClick;

  TogetherItem := AddI('TogetherItem');
  mi.Caption := '&Gruppiert anzeigen';
  mi.Hint := '  Kurven in einem Diagramm anzeigen';
  mi.OnClick := ShowTogetherBtnClick;

  N3 := AddI('N3');
  mi.Caption := '-';

  OpenItem := AddI('OpenItem');
  mi.Caption := '&Öffnen...';
  mi.Hint := '  gespeichertes Diagramm laden';
  mi.OnClick := OpenItemClick;

  SaveItem := AddI('SaveItem');
  mi.Caption := '&Speichern...';
  mi.Hint := '  Diagramm speichern';
  mi.OnClick := SaveItemClick;

  p := ChartMenu;

  N4 := AddI('N4');
  mi.Caption := '-';
  mi.GroupIndex := 3;

  RectangleItem := AddI('RectangleItem');
  mi.Caption := 'Rechtecke';
  mi.Checked := WantRectangles;
  mi.GroupIndex := 3;
  mi.Hint := '  Rechtecke anzeigen';
  mi.OnClick := RectangleItemClick;

end;

procedure TChartForm.UpdateGetriebe;
begin
  Rigg.UpdateGetriebe;

  if not (SofortBerechnen and Rigg.GetriebeOK and Rigg.MastOK) then
  begin
    if Rigg.GetriebeOK and not Rigg.MastOK then
    begin
    end;
  end;

  if (SofortBerechnen and Rigg.GetriebeOK and Rigg.MastOK) then
  begin
    Rigg.UpdateRigg;
  end;
end;

procedure TChartForm.DoLegend;
var
  R: TRect;
begin
  FLegend := ParamCount > 1;
  if FLegend then
    DrawLegend(PaintBoxLegend.Canvas, PaintBoxLegend.BoundsRect)
  else
    with PaintBoxLegend do
    begin
      R := Rect(0,0,Width,Height);
      Canvas.Brush.Color := clBtnFace;
      Canvas.FillRect(R);
    end;
end;

procedure TChartForm.DrawToChart;
begin
  DrawChartPaintBox(ChartPaintBox.Canvas, ChartPaintBox.BoundsRect);
end;

procedure TChartForm.DrawLegend(Canvas: TCanvas; Rect: TRect);
var
  Bitmap: TBitmap;
  p, PosX, PosY: Integer;
begin
  Bitmap := TBitmap.Create;
  with Bitmap do
  begin
    Width := Rect.Right-Rect.Left;
    Height := Rect.Bottom-Rect.Top;
  end;
  try
    PaintBackGround(Bitmap);
    with Bitmap.Canvas do
    begin
      PosX := 0;
      PosY := 0;
      for p := 0 to ParamCount-1 do
      begin
        { Bullet }
        Pen.Color := clBlack; { clBlue }
        Brush.Color := cf[p];
        Brush.Style := bsSolid;
        PosY := PosY + 30;
        Rectangle( PosX, PosY, PosX + 10, PosY + 5);
        { Text }
        Brush.Style := bsClear;
        PosY := PosY + 10;
        if Valid then
          TextOut(PosX, PosY, PText[p])
        else
          TextOut(PosX, PosY, PColorText[p]);
      end;
    end;

    with Canvas do
    begin
      CopyMode := cmSrcCopy;
      Draw(0, 0, BitMap);
    end;
  finally
    Bitmap.Free;
  end;
end;

procedure TChartForm.DrawChartPaintBox(Canvas: TCanvas; Rect: TRect);

  function Limit(a: double): double;
  begin
    if a < -32000 then
      a := -32000
    else if a > 32000 then
      a := 32000;
    Result := a;
  end;

var
  Pt: TPoint;
  R: TRect;
  i, p, RadiusX, RadiusY: Integer;
  Bitmap: TBitmap;
  PlotWidth, PlotHeight: Integer;
  PlotExtX, PlotExtY: Integer;
  PlotOrgX, PlotOrgY: Integer;
  tempX, tempY: double;
begin
  { schnelle direkte Textausgabe für oft veränderte Labels }
  DrawLabels;

  { diese Labels sind nicht zeitkritisch }
  lbAchseX.Caption := XTitle;
  lbXLeft.Caption := IntToStr(Round(Xmin));
  lbXRight.Caption := IntToStr(Round(Xmax));
  lbParam.Caption := PTitle;

  PlotWidth := Rect.Right - Rect.Left;
  PlotHeight := Rect.Bottom - Rect.Top;
  PlotExtX := PlotWidth;
  PlotExtY := PlotHeight;
  PlotOrgX := 0;
  PlotOrgY := 0;

  Bitmap := TBitmap.Create;
  with Bitmap do
  begin
    Width := PlotWidth;
    Height := PlotHeight;
  end;
  try
    PaintBackGround(Bitmap);

    with Bitmap.Canvas do
    begin
      SetMapMode(Handle, MM_ANISOTROPIC);
      SetWindowExtEx(Handle, PlotExtX, -PlotExtY, nil);
      SetWindowOrgEx(Handle, PlotOrgX, PlotOrgY, nil);
      SetViewPortExtEx(Handle, PlotWidth, PlotHeight, nil);
      SetViewPortOrgEx(Handle, 0, PlotHeight, nil);

      { Radius }
      R.Left := 0; R.Top := 0; R.Bottom := 3; R.Right := 3;
      DPTOLP(Handle, R, 2);
      RadiusX := R.Right-R.Left; RadiusY := R.Bottom-R.Top;

      for p := 0 to ParamCount-1 do
      begin

        { Kurve }
        Pen.Color := cf[p];
        tempY := PlotExtY * (bf[p,0]-Ymin)/(Ymax-Ymin);
        Pt.y := Round(Limit(tempY));
        MoveTo(0,Pt.y);
        for i := 1 to 100 do
        begin
          tempX := PlotExtX * (i/100);
          tempY := PlotExtY * (bf[p,i]-Ymin)/(Ymax-Ymin);
          Pt.x := Round(Limit(tempX));
          Pt.y := Round(Limit(tempY));
          LineTo(Pt.x, Pt.y);
        end;

        if WantRectangles then
        begin
          { Rechtecke }
          Pen.Color := clBlack;
          Brush.Color := cf[p];
          Brush.Style := bsSolid;
          for i := 0 to 100 do
          begin
            tempX := PlotExtX * (i/100);
            tempY := PlotExtY * (bf[p,i]-Ymin)/(Ymax-Ymin);
            Pt.x := Round(Limit(tempX));
            Pt.y := Round(Limit(tempY));
            Rectangle( Pt.x - RadiusX, Pt.y - RadiusY,
                       Pt.x + RadiusX, Pt.y + RadiusY);
          end;
        end;

      end;

      SetMapMode(Handle, MM_TEXT);
      SetWindowOrgEx(Handle, 0, 0, nil);
      SetViewPortOrgEx(Handle, 0, 0, nil);
    end;

    with Canvas do
    begin
      CopyMode := cmSrcCopy;
      Draw(0, 0, BitMap);
    end;

  finally
    Bitmap.Free;
  end;
end;

procedure TChartForm.DrawLabels;
var
  PosX, PosY: Integer;
  R: TRect;
  S: String;
begin
  with Canvas do
  begin
    Brush.Style := bsSolid;
    Brush.Color := ClBtnFace;
    PosX := ChartPaintBox.Left - 55;
    PosY := ChartPaintBox.Top - 24;
    R := Rect(PosX, PosY, PosX+210, PosY+Font.Height);
    SetTextAlign(Handle, TA_LEFT or TA_TOP);
    TextRect(R, PosX, PosY, YTitle);

    PosX := ChartPaintBox.Left - 8;
    PosY := ChartPaintBox.Top;
    R := Rect(PosX-60, PosY, PosX, PosY+Font.Height);
    SetTextAlign(Handle, TA_RIGHT or TA_TOP);
    S := IntToStr(Round(Ymax));
    TextRect(R, PosX, PosY, S);

    PosX := ChartPaintBox.Left - 8;
    PosY := ChartPaintBox.Top + ChartPaintBox.Height;
    R := Rect(PosX-60, PosY-Font.Height, PosX, PosY);
    SetTextAlign(Handle, TA_RIGHT or TA_BOTTOM);
    S := IntToStr(Round(Ymin));
    TextRect(R, PosX, PosY, S);
  end;
end;

procedure TChartForm.FormPaint(Sender: TObject);
begin
  inherited;
  { direkt auf den Canvas des Formulars zeichnen }
  DrawLabels;
end;

procedure TChartForm.ChartPaintBoxPaint(Sender: TObject);
var
  tempParamCount: Integer;
begin
  if ShowGroup then
  begin
    tempParamCount := ParamCount;
    ParamCount := GroupKurvenzahl;
    DrawToChart;
    ParamCount := tempParamCount;
  end
  else
    DrawToChart;
end;

procedure TChartForm.PaintBoxLegendPaint(Sender: TObject);
var
  tempParamCount: Integer;
  tempPText: TYAchseStringArray;
begin
  if ShowGroup then
  begin
    tempParamCount := ParamCount;
    tempPText := PText;
    ParamCount := GroupKurvenzahl;
    PText := GroupText;
    DoLegend;
    ParamCount := tempParamCount;
    PText := tempPText;
  end
  else
    DoLegend;
end;

procedure TChartForm.RectangleItemClick(Sender: TObject);
begin
  WantRectangles := not WantRectangles;
  RectangleItem.Checked := WantRectangles;
  if ShowGroup then
    ShowTogetherBtnClick(Self)
  else
    DrawInternal;
end;

procedure TChartForm.PaintBackGround(Image: TBitMap);
var
  R: TRect;
begin
  R := Rect(0, 0, Image.Width, Image.Height);
  with Image.Canvas do
  begin
    Brush.Color := clBtnFace;
    FillRect(R);
  end;
end;

procedure TChartForm.RebuildYCombo;
var
  YAV: TYAchseValue;
begin
  { YComboBox }
  YCombo.Items.Clear;
  for YAV := Low(TYAchseValue) to High(TYAchseValue) do
    if YAV in YAchseSet then
      YCombo.Items.Add(YAchseRecordList[YAV].ComboText);
  if YCombo.Items.Count > 0 then YCombo.ItemIndex := 0;
  UpdateYAchseList;
  { YAuswahlDlg.DstList }
  YAuswahlDlg.DstList.Items := YCombo.Items;
  { YAuswahlDlg.SrcList }
  YAuswahlDlg.SrcList.Clear;
  for YAV := Low(TYAchseValue) to High(TYAchseValue) do
    if not (YAV in YAchseSet) then
  YAuswahlDlg.SrcList.Items.Add(YAchseRecordList[YAV].ComboText);
end;

procedure TChartForm.KurvenZahlSpinnerChanging(Sender: TObject;
  var AllowChange: Boolean);
begin
  tempSpinnerPosition := KurvenZahlSpinner.Position; { vor Veränderung }
end;

procedure TChartForm.KurvenzahlEditChange(Sender: TObject);
{ Reihenfolge des Aufrufs:
   1. KurvenzahlSpinnerChanging
   2. KurvenZahlEditChange
   3. KurvenZahlSpinnerClick }
begin
  { jetzt in KurvenzahlSpinnerClick(): }
  { if ShowGroup then ShowTogetherBtnClick(Sender); }
end;

procedure TChartForm.KurvenZahlSpinnerClick(Sender: TObject; Button: TUDBtnType);
begin
  if (tempSpinnerPosition = ParamCount) and (Button = btNext) then
    Exit;
  if (tempSpinnerPosition = 1) and (Button = btPrev) then
    Exit;
  if ShowGroup then
    ShowTogetherBtnClick(Sender);
end;

procedure TChartForm.YAuswahlClick(Sender: TObject);
var
  i: Integer;
begin
  with YAuswahlDlg do
  begin
    if not (DstList.ItemIndex = -1) then
    begin
      { clear selection if any }
      for i := 0 to DstList.Items.Count-1 do
      begin
        DstList.Selected[i] := False;
      end;
      { In DstList den gleichen Eintrag wie in YComboBox selektieren }
      DstList.ItemIndex := YCombo.ItemIndex;
      DstList.Selected[YCombo.ItemIndex] := True;
    end;

    if ShowModal = mrOK then
    begin
      if (DstList.Items.Count = 0) then
      begin
        { mindestens ein Eintrag muß sich in DestList befinden }
        DstList.Items.AddObject(SrcList.Items[0], SrcList.Items.Objects[0]);
        SrcList.Items.Delete(0);
      end;
      YCombo.Items := DstList.Items;
      YCombo.ItemIndex := DstList.ItemIndex;
      UpdateYAchseList;
      YComboChange(Sender);
    end;
  end;
end;

procedure TChartForm.PSpinnerChanging(Sender: TObject; var AllowChange: Boolean);
begin
  if ParamCount = 1 then
    AllowChange := False;
end;

procedure TChartForm.PEditChange(Sender: TObject);
begin
  UpdateYMinMax;
end;

procedure TChartForm.MemoItemClick(Sender: TObject);
begin
{$ifdef RG19}
  MemoFormC := TMemoFormC.Create(Self);
  with MemoFormC do
  begin
    try
      Memo.Lines.Clear;
      Memo.Lines := MemoLines;
      ShowModal;
    finally
      Free;
    end;
  end;
{$endif}
end;

function TChartForm.ValidateInput(Input: TMaskEdit): Boolean;
var
  s: string;
  I: Integer;
  Code: Integer;
begin
  Result := False;
  try
    Val(Input.Text, I, Code);
    if Code <> 0 then
    begin
      s := Format('''%s'' ist kein gültiger Integerwert', [Input.Text]);
      MessageDlg(s, mtWarning, [mbOK], 0);
      Input.SetFocus;
    end
    else
    begin
      if (I >= 0) and (I < MaxInt) then
      Result := True;
    end;
  except
    on EConvertError do
    begin
      s := Format('''%s'' ist kein gültiger Integerwert', [Input.Text]);
      MessageDlg(s, mtWarning, [mbOK], 0);
      Input.SetFocus;
    end;
  end;
end;

procedure TChartForm.LoadFromFile(FileName: string);
var
  S: TFileStream;
begin
  S := TFileStream.Create(OpenDialog.FileName, fmOpenRead);
  try
    LoadFromStream(S);
  finally
    S.Free;
  end;
end;

procedure TChartForm.SaveToFile(FileName: string);
var
  S: TFileStream;
begin
  S := TFileStream.Create(SaveDialog.FileName, fmCreate);
  try
    SaveToStream(S);
  finally
    S.Free;
  end;
end;

{ yArray enthält jetzt Werte vom Typ single, um Platz zu sparen.
  Es wird mit der Option 'Ausgerichtete RecordFelder' compiliert!
  Damit hat das Feld RggDocument.TrimmTabDaten.TabellenTyp 4 Byte.
  Dieses Format muß beibehalten werden.
}
procedure TChartForm.SaveToStream(S: TStream);
var
  ParamValue: double;
  p: Integer;
begin
  with S do
  begin
    WriteBuffer(FLegend, SizeOf(Boolean));
    WriteBuffer(XAchseMin, SizeOf(Integer));
    WriteBuffer(XAchseMax, SizeOf(Integer));
    WriteBuffer(ParamCount, SizeOf(Integer));
    WriteBuffer(YAchseSet, SizeOf(YAchseSet));
    WriteBuffer(YAchseRecordList, SizeOf(YAchseRecordList));
    for p := 0 to PNr-1 do
      WriteBuffer(af[p], SizeOf(TYLineArray));
    for p := 0 to ParamCount-1 do begin
      ParamValue := StrToFloat(PText[p]);
      WriteBuffer(ParamValue, SizeOf(double));
    end;
    RggDocument.SaveToStream(S);
    MemoLines.Add(XComboText);
    MemoLines.Add(PComboText);
    MemoLines.SaveToStream(S);
  end;
end;

procedure TChartForm.LoadFromStream(S: TStream);
var
  ParamValue: double;
  p: Integer;
begin
  with S do
  begin
    ReadBuffer(FLegend, SizeOf(Boolean));
    ReadBuffer(XAchseMin, SizeOf(Integer));
    ReadBuffer(XAchseMax, SizeOf(Integer));
    ReadBuffer(ParamCount, SizeOf(Integer));
    ReadBuffer(YAchseSet, SizeOf(YAchseSet));
    ReadBuffer(YAchseRecordList, SizeOf(YAchseRecordList));
    for p := 0 to PNr-1 do
      ReadBuffer(af[p], SizeOf(TYLineArray));
    for p := 0 to ParamCount-1 do
    begin
      ReadBuffer(ParamValue, SizeOf(double));
      PText[p] := Format('%6.2f', [ParamValue]);
    end;
    RggDocument.LoadFromStream(S);
    MemoLines.LoadFromStream(S);
    XComboText := MemoLines[MemoLines.Count-2];
    PComboText := MemoLines[MemoLines.Count-1];
    MemoLines.Delete(MemoLines.Count-1);
    MemoLines.Delete(MemoLines.Count-1);
  end;
end;

procedure TChartForm.OpenItemClick(Sender: TObject);
begin
  if not OpenDialog.Execute then Exit;
  LoadFromFile(OpenDialog.FileName);
  Exclude(FStatus, csBerechnet);
  Include(FStatus, csGeladen);
  XAchseText := GetXText(XComboText); { benötigt für BottomTitel }
  ParamText := GetPText(PComboText); { benötigt für RightTitel }
  XLED.Brush.Color := clRed;
  PLED.Brush.Color := clRed;
  PSpinner.Position := 1;
  PSpinner.Max := ParamCount;
  if PSpinner.Max = 1 then
    PSpinner.Max := 2;
  KurvenZahlSpinner.Position := ParamCount;
  RebuildYCombo;
  YComboChange(Self);
end;

procedure TChartForm.SaveItemClick(Sender: TObject);
begin
  if not SaveDialog.Execute then
    Exit;
  SaveToFile(SaveDialog.FileName);
end;

procedure TChartForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
{$ifdef RG19}
  RiggModul.ViewModelMain.HideDiagramm;
  RiggModul.ChartFormActive := False;
  Action := caFree;
{$endif}
end;

procedure TChartForm.CloseItemClick(Sender: TObject);
begin
  Close;
end;

procedure TChartForm.FormActivate(Sender: TObject);
begin
  TakeOver;
end;

procedure TChartForm.UpdateChartItemClick(Sender: TObject);
begin
  TakeOver;
end;

procedure TChartForm.TakeOver;
begin
  Rigg.UpdateGSB;
  SalingTyp := Rigg.SalingTyp;
  { ControllerTyp := Rigg.ControllerTyp; }
  { CalcTyp := Rigg.CalcTyp; }
end;

procedure TChartForm.SetSalingTyp(Value: TSalingTyp);
begin
  if FSalingTyp <> Value then
  begin
    FSalingTyp := Value;
    FValid := False;
    YLED.Brush.Color := clRed;
    UpdateXCombo(SalingTyp);
  end;
end;

procedure TChartForm.CalcItemClick(Sender: TObject);
begin
  Calc;
  PSpinner.Update;
  ActiveControl := YCombo;
end;

procedure TChartForm.BuissyItemClick(Sender: TObject);
begin
  Reset;
end;

end.
