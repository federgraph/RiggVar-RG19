unit FrmChart;

interface

uses
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
  FrmProgress,
  RggTypes,
  RggDoc,
  RggSaling3Eck;

const
  ANr = 6; { maximale Anzahl Kurven, d.h. berechneter Y Werte }
  PNr = 5; { maximale Anzahl der Werte des Parameters }
  VNr = 14; { Anzahl der zur Auswahl stehenden Y Werte }
  ErrorIndex = 999;

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

  TChartStatus = (csBerechnet, csGeladen);
  yArray = array[0..ANr-1] of TLineDataR100;
  TYAchseSortedList = array[0..VNr-1] of TYAchseValue;
  TYAchseSet = Set of TYAchseValue; {die berechneten Kurven}
  TYAchseStringArray = array[0..PNr-1] of string;

  TChartForm = class(TForm)
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    {Panel AlignTop}
    BedienPanel: TPanel;
    {Y}
    YComboBox: TComboBox;
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
    {X}
    XComboBox: TComboBox;
    XComboLabel: TLabel;
    XMinEdit: TMaskEdit;
    XMinLabel: TLabel;
    XMaxEdit: TMaskEdit;
    XMaxLabel: TLabel;
    XLED: TShape;
    XBevel: TBevel;
    {P}
    PComboBox: TComboBox;
    PComboLabel: TLabel;
    PMinEdit: TMaskEdit;
    PMinLabel: TLabel;
    PMaxEdit: TMaskEdit;
    PMaxLabel: TLabel;
    PLED: TShape;
    PBevel: TBevel;
    procedure FormCreate(Sender: TObject);
    procedure YAuswahlClick(Sender: TObject);
    procedure YComboBoxChange(Sender: TObject);
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
    procedure PEditChange(Sender: TObject);
    procedure XComboBoxChange(Sender: TObject);
    procedure PComboBoxChange(Sender: TObject);
    procedure MemoItemClick(Sender: TObject);
    procedure ShowTogetherBtnClick(Sender: TObject);
    procedure KurvenZahlSpinnerClick(Sender: TObject; Button: TUDBtnType);
    procedure KurvenzahlEditChange(Sender: TObject);
    procedure ChartMenuClick(Sender: TObject);
    procedure UpdateChartItemClick(Sender: TObject);
    procedure BereichBtnClick(Sender: TObject);
    procedure APEditChange(Sender: TObject);
    procedure UpdateRiggItemClick(Sender: TObject);
    procedure KurvenZahlSpinnerChanging(Sender: TObject; var AllowChange: Boolean);
  private
    FBuissy: Boolean;
    FStatus: Set of TChartStatus;
    FValid: Boolean;
    FLegend: Boolean;
    FShowTogether: Boolean;
    FSalingTyp: TSalingTyp;
    { FControllerTyp: TControllerTyp; }
    { FCalcTyp: TCalcTyp; }
    FXTextClicked, FPTextClicked: string;
    procedure GetMemoText;
    function GetYText(Text: string): string;
    function GetTsbName(Text: string): TxpName;
    procedure SetSalingTyp(Value: TSalingTyp);
    { procedure SetControllerTyp(Value: TSalingTyp); }
    { procedure SetCalcTyp(Value: TSalingTyp); }
    function ValidateInput(Input: TMaskEdit): Boolean;
    procedure TakeOver;
  protected
    tempSpinnerPosition: Integer;
    YAchseRecordList: TYAchseRecordList;
    YAchseSortedList: TYAchseSortedList;
    YAchseSet: TYAchseSet;
    TopTitel, LeftTitel, BottomTitel, RightTitel: string;
    Xmin, Xmax, Ymin, Ymax, YGap: Single;
    ParamCount: Integer;
    APWidth: Integer;
    f, TestF: TLineDataR100;
    af: array[0..PNr-1] of yArray;
    bf: array[0..PNr-1] of TLineDataR100;
    cf: array[0..PNr-1] of TColor;
    PText, PColorText: TYAchseStringArray;
    ProgressDlg: TProgressDlg;
    RggDocument: TRggDocument;
    SalingDreieck: TSalingDreieck;
    procedure StraightLine;
    procedure LookForYMinMax;
    procedure UpdateYMinMax;
    procedure GetCurves;
    procedure Draw;
    procedure DrawToChart; virtual;
    procedure DoLegend; virtual;
    procedure SaveToStream(S: TStream);
    procedure LoadFromStream(S: TStream);
    procedure UpdateYAchseList;
    procedure UpdateYAchseSet;
    function ComboIndexToCurve(ComboIndex: Integer): Integer;
    procedure RebuildYCombo;
    procedure ShowTogether(ParamNo: Integer);
  public
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
    procedure Calc(Sender: TObject);
    procedure DoAfterCalc;
    procedure SaveToFile(FileName: string);
    procedure LoadFromFile(FileName: string);
    procedure Reset;

    property SalingTyp: TSalingTyp read FSalingTyp write SetSalingTyp;
    { property ControllerTyp: TControllerTyp read FControllerTyp write SetControllerTyp; }
    { property CalcTyp: TCalcTyp read FCalcTyp write SetCalcTyp; }
    property Legend: Boolean read FLegend;
    property Valid: Boolean read FValid write FValid;
    property ShowGroup: Boolean read FShowTogether;
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
    procedure InitMenu; virtual;
  end;

var
  ChartForm: TChartForm;

implementation

{$R *.DFM}

uses
  RiggVar.RG.Def,
  RggModul,
  RggCalc,
  RggUnit4,
  RggScroll,
  FrmAuswahl,
  FrmMemo;

procedure TChartForm.FormCreate(Sender: TObject);
begin
  HorzScrollBar.Position := 0;

  ChartForm := Self; { wird schon in AchsForm.Create benötigt }
  ProgressDlg := TProgressDlg.Create(Self);
  ProgressDlg.OnStart := Calc;
  RggDocument := TRggDocument.Create;
  MemoLines := TStringList.Create;
  MemoLines.Add('Diagramm befindet sich im Anfangszustand.');
  SalingDreieck := TSalingDreieck.Create;

  FLegend := True;
  ParamCount := 3;
  APWidth := 30;
  APSpinner.Position := 30;
  YLED.Brush.Color := clRed;
  XLED.Brush.Color := clRed;
  PLED.Brush.Color := clRed;
  TakeOver;
  UpdateXCombo(SalingTyp);
  UpdatePCombo(SalingTyp);
  FXTextClicked := 'Vorstag';
  FPTextClicked := 'Kein Parameter';
  XComboBox.ItemIndex := XComboBox.Items.IndexOf(FXTextClicked);
  PComboBox.ItemIndex := PComboBox.Items.IndexOf(FPTextClicked);

  InitYAchseRecordList(YAchseRecordList);
  { Hiermit werden die Felder ComboText und Text initialisiert.
    ComboIndex wird in UpdateYAchseList weiter unten bestimmt.
    ArrayIndex wird beim Berechnen oder Einlesen neu bestimmt.
    YAchseSet = [] zeigt an, daß ArrayIndex nicht gültig ist.
  }

  cf[0] := clBlue;
  cf[1] := clRed;
  cf[2] := clLime;
  cf[3] := clWhite;
  cf[4] := clYellow;
  PColorText[0] := 'Blau';
  PColorText[1] := 'Rot';
  PColorText[2] := 'Grün';
  PColorText[3] := 'Weiß';
  PColorText[4] := 'Gelb';
  PText := PColorText;

  YComboBox.Items.Assign(YAuswahlDlg.DstList.Items);
  YComboBox.ItemIndex := 1;
  UpdateYAchseList; { ComboIndex festlegen in YAchseRecordList }

  if RiggModul.RG19A then
  begin
    FormStyle := fsMDIChild;
    InitMenu;
  end;

  StraightLine;
  Draw;
end;

procedure TChartForm.FormDestroy(Sender: TObject);
begin
  RggDocument.Free;
  ProgressDlg.Free;
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

procedure TChartForm.BuissyItemClick(Sender: TObject);
begin
  Reset;
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
  Draw;
  MemoLines.Clear;
  MemoLines.Add('Diagramm wurde in den Anfangszustand versetzt.');
end;

procedure TChartForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  RiggModul.ViewModelMain.HideDiagramm;
  RiggModul.ChartFormActive := False;
  Action := caFree;
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
  RiggModul.Rigg.UpdateGSB;
  SalingTyp := RiggModul.Rigg.SalingTyp;
  { ControllerTyp := RiggModul.Rigg.ControllerTyp; }
  { CalcTyp := RiggModul.Rigg.CalcTyp; }
end;

procedure TChartForm.SetSalingTyp(Value: TSalingTyp);
begin
  if FSalingTyp <> Value then begin
    FSalingTyp := Value;
    FValid := False;
    YLED.Brush.Color := clRed;
    UpdateXCombo(SalingTyp);
  end;
end;

{
procedure TChartForm.SetControllerTyp(Value: TControllerTyp);
begin
  if FController <> Value then begin
    FControllerTyp := Value;
    FValid := False;
    YLED.Brush.Color := clRed;
  end;
end;

procedure TChartForm.SetCalcTyp(Value: TCalcTyp);
begin
  if FCalcTyp <> Value then begin
    FCalcTyp := Value;
    FValid := False;
    YLED.Brush.Color := clRed;
  end;
end;
}

procedure TChartForm.StraightLine;
var
  i: Integer;
begin
  for i := 0 to 100 do begin
    f[i] := i;
    bf[0,i] := f[i];
    bf[1,i] := f[i]+5;
    bf[2,i] := f[i]+10;
    bf[3,i] := f[i]+20;
    bf[4,i] := f[i]+50;
  end;
  TestF := f;
end;

{wird von YComboClick aufgerufen und vom Constructor}
procedure TChartForm.UpdateYAchseList;
var
  i: Integer;
  S: string;
  YAV: TYAchseValue;
begin
  { ComboIndex zurücksetzen auf -1 }
  for YAV := Low(TYAchseValue) to High(TYAchseValue) do
    YAchseRecordList[YAV].ComboIndex := -1;
  { ComboIndex neu bestimmen. Nicht ausgewählte Einträge bleiben auf -1 }
  with YComboBox do
    for i := 0 to Items.Count-1 do {ComboBox iterieren}
    begin
      S := Items[i];
      for YAV := Low(TYAchseValue) to High(TYAchseValue) do
        { Position j des Eintrag finden durch  Textvergleich }
        if S = YAchseRecordList[YAV].ComboText then
        begin
          { Position in der ComboBox festhalten }
          YAchseRecordList[YAV].ComboIndex := i;
          { umgekehrt auch Reihenfolge festhalten: (sortierte Liste) }
          YAchseSortedList[i] := YAV;
          Break;
        end;
    end;
end;

{ Wird nur bei NeuBerechnung aufgerufen }
procedure TChartForm.UpdateYAchseSet;
var
  i: Integer;
  S: string;
  YAV: TYAchseValue;
begin
  YAchseSet := [];
  with YComboBox do
    for i := 0 to Items.Count-1 do
    begin
      S := Items[i];
      for YAV := Low(TYAchseValue) to High(TYAchseValue) do
        { Position j des Eintrag finden durch  Textvergleich }
        if S = YAchseRecordList[YAV].ComboText then
        begin
          { Position in der ComboBox festhalten }
          YAchseRecordList[YAV].ArrayIndex := i;
          { festhalten, welche Kurven existieren }
          if i <= ANr-1 then Include(YAchseSet, YAV);
          Break;
        end;
    end;
end;

function TChartForm.ComboIndexToCurve(ComboIndex: Integer): Integer;
var
  YAV: TYAchseValue;
begin
  YAV := YAchseSortedList[ComboIndex];
  if YAV in YAchseSet then begin
    if (csBerechnet in FStatus) or (csGeladen in FStatus) then FValid := True;
    result := YAchseRecordList[YAV].ArrayIndex;
  end
  else begin
    FValid := False;
    result := ErrorIndex; { ev. hier Exception auslösen }
  end;
end;

procedure TChartForm.RebuildYCombo;
var
  YAV: TYAchseValue;
begin
  { YComboBox }
  YComboBox.Items.Clear;
  for YAV := Low(TYAchseValue) to High(TYAchseValue) do
    if YAV in YAchseSet then
      YComboBox.Items.Add(YAchseRecordList[YAV].ComboText);
  if YComboBox.Items.Count > 0 then YComboBox.ItemIndex := 0;
  UpdateYAchseList;
  { YAuswahlDlg.DstList }
  YAuswahlDlg.DstList.Items := YComboBox.Items;
  { YAuswahlDlg.SrcList }
  YAuswahlDlg.SrcList.Clear;
  for YAV := Low(TYAchseValue) to High(TYAchseValue) do
    if not (YAV in YAchseSet) then
  YAuswahlDlg.SrcList.Items.Add(YAchseRecordList[YAV].ComboText);
end;

procedure TChartForm.CalcItemClick(Sender: TObject);
var
  j, p: Integer;
begin
  if not FBuissy then
  begin
    { Berechnen }
    FBuissy := True;
    if not CheckBeforeCalc then
      Exit;
    { Parameterzahl bearbeiten }
    if PComboBox.Text = ('kein Parameter') then
    begin
      PMinEdit.Text := '0';
      PMaxEdit.Text := '0';
      KurvenZahlSpinner.Position := 1;
    end;
    ParamCount := KurvenZahlSpinner.Position;
    if PSpinner.Position > ParamCount then
      PSpinner.Position := ParamCount;
    PSpinner.Max := ParamCount;
    { MaxValue muß größer MinValue sein! }
    if PSpinner.Max = 1 then
      PSpinner.Max := 2;
    PSpinner.Update;

    if YComboBox.ItemIndex > ANr-1 then
      YComboBox.ItemIndex := 1;

    ProgressDlg.ShowModal;
    { Calc(Self); } { von ProgressDlg aufgerufen }
    { ProgressDlg.Hide; } { Dialog verschwindet von selbst }
    if (ProgressDlg.ModalResult = mrOK) or ProgressDlg.Aborted then
    begin
      { Dialog wurde geschlossen, ohne zu berechnen
        oder die Berechnung wurde abgebrochen }
      FBuissy := False;
      Exit;
    end;

    DoAfterCalc;
    RiggModul.Rigg.GetDocument(RggDocument);
    GetMemoText;
    Include(FStatus, csBerechnet);
    YLED.Brush.Color := clLime;
    FBuissy := False;
    { Anzeigen }
    j := ComboIndexToCurve(YComboBox.ItemIndex);
    for p := 0 to ParamCount-1 do
      bf[p] := af[p,j];
    UpdateYMinMax;
    Draw;
    ActiveControl := YComboBox;
  end;
end;

procedure TChartForm.Calc(Sender: TObject);
begin
  { wird vom Modal angezeigten ProgressDlg aufgerufen }
  UpdateYAchseSet;
  GetCurves;
end;

procedure TChartForm.GetCurves;
var
  i, j, p: Integer;
  Antrieb, PAntrieb: double;
  Anfang, Ende, PAnfang, PEnde: double;
  InputRec: TTrimmControls;
  PunktOK: Boolean;
  Rigg: TRigg;
  S: string;
  temp, tempL, tempH, tempA: double;
begin
  Rigg := RiggModul.Rigg;

  { Getriebezustand sichern und verfügbar machen }
  InputRec := Rigg.Glieder;
  with InputRec do
  begin
    TopTitel := Format('(Co%dVo%dWi%dWo%dWa%dSh%dSa%dSl%d',
    [Controller,Vorstag,Winkel,Woben,Wanten,SalingH,SalingA,SalingL]);
    if SalingTyp = stFest then TopTitel := TopTitel + '/fest)';
    if SalingTyp = stDrehbar then TopTitel := TopTitel + '/drehbar)';
    if SalingTyp = stOhne_2 then TopTitel := TopTitel + '/ohne Saling (BK))';
    if SalingTyp = stOhne then TopTitel := TopTitel + '/ohne Saling)';
    TopTitel := 'Riggchart - ' + DateToStr(Date) + ' - ' + TopTitel;
  end;
  Rigg.ProofRequired := False;

  try

  { Parameterbereich bestimmen und Schleife starten }
  PAnfang := StrToInt(PminEdit.Text);
  PEnde := StrToInt(PmaxEdit.Text);
  PAntrieb := (PEnde+PAnfang) / 2;
  for p := 0 to ParamCount-1 do
  begin
    with ProgressDlg.ParamLabel do
    begin
      if ParamCount > 1 then
        Caption := Format('Parameter %d von %d', [p+1, ParamCount])
      else
        Caption := 'Kurve wird berechnet';
      Update;
    end;

    if ParamCount > 1 then
    begin
      PAntrieb := PAnfang+(PEnde-PAnfang)*p/(ParamCount-1);
      PText[p] := Format('%6.2f', [PAntrieb]);
    end;

    { Parameter ansteuern }
    S := PComBoBox.Text;
    if ParamCount < 2 then
    begin
     { do nothing }
    end
    else if (S = 'Controller') then
        Rigg.RealGlied[fpController] := PAntrieb
    else if (S = 'Winkel') then
      Rigg.RealGlied[fpWinkel] := PAntrieb/10*pi/180
    else if (S = 'Vorstag') then
      Rigg.RealGlied[fpVorstag] := PAntrieb
    else if (S = 'Wante') then
      Rigg.RealGlied[fpWante] := PAntrieb
    else if (S = 'Wante oben') then
      Rigg.RealGlied[fpWoben] := PAntrieb
    else if (S = 'Saling Höhe') then
      Rigg.RealGlied[fpSalingH] := PAntrieb
    else if (S = 'Saling Abstand') then
      Rigg.RealGlied[fpSalingA] := PAntrieb
    else if (S = 'Saling Länge') and (SalingTyp = stDrehbar) then
      Rigg.RealGlied[fpSalingL] := PAntrieb
    else if (S = 'Saling Länge') and (SalingTyp = stFest) then begin
      tempL := Rigg.RealGlied[fpSalingL];
      temp := PAntrieb/tempL;
      tempH := temp * Rigg.RealGlied[fpSalingH];
      tempA := temp * Rigg.RealGlied[fpSalingA];
      Rigg.RealGlied[fpSalingH] := tempH;
      Rigg.RealGlied[fpSalingA] := tempA;
    end else if (S = 'Saling Winkel') then begin
      temp := PAntrieb*pi/180;
      tempL := Rigg.RealGlied[fpSalingL];
      tempH := tempL * sin(temp);
      tempA := 2 * tempL * cos(temp);
      Rigg.RealGlied[fpSalingH] := tempH;
      Rigg.RealGlied[fpSalingA] := tempA;
    end;

    { Definitionsbereich bestimmen und Berechnungsschleife starten }
    Anfang := StrToInt(XminEdit.Text);
    Ende := StrToInt(XmaxEdit.Text);
    for i := 0 to 100 do
    begin
      if i mod 5 = 0 then
        ProgressDlg.Gauge.Position := i;

      Antrieb := Anfang + (Ende-Anfang)*i/100;

      { Antrieb ansteuern }
      S := XComboBox.Text;
      if (S = 'Controller') then
        Rigg.RealGlied[fpController] := Antrieb
      else if (S = 'Winkel') then
        Rigg.RealGlied[fpWinkel] := Antrieb/10 * pi/180
      else if (S = 'Vorstag') then
        Rigg.RealGlied[fpVorstag] := Antrieb
      else if (S = 'Wante') then
        Rigg.RealGlied[fpWante] := Antrieb
      else if (S = 'Wante oben') then
        Rigg.RealGlied[fpWoben] := Antrieb
      else if (S = 'Saling Höhe') then
        Rigg.RealGlied[fpSalingH] := Antrieb
      else if (S = 'Saling Abstand') then
        Rigg.RealGlied[fpSalingA] := Antrieb
      else if (S = 'Saling Länge') and (SalingTyp = stDrehbar) then
        Rigg.RealGlied[fpSalingL] := Antrieb
      else if (S = 'Saling Länge') and (SalingTyp = stFest) then
      begin
        tempL := Rigg.RealGlied[fpSalingL];
        temp := Antrieb/tempL;
        tempH := temp * Rigg.RealGlied[fpSalingH];
        tempA := temp * Rigg.RealGlied[fpSalingA];
        Rigg.RealGlied[fpSalingH] := tempH;
        Rigg.RealGlied[fpSalingA] := tempA;
      end else if (S = 'Saling Winkel') then
      begin
        temp := Antrieb*pi/180;
        tempL := Rigg.RealGlied[fpSalingL];
        tempH := tempL * sin(temp);
        tempA := 2 * tempL * cos(temp);
        Rigg.RealGlied[fpSalingH] := tempH;
        Rigg.RealGlied[fpSalingA] := tempA;
      end;

      { Berechnen }
      if SalingTyp = stFest then
        if (XComboBox.Text = 'Winkel') or
           (PComboBox.Text = 'Winkel') then
          Rigg.UpdateGetriebeFS
        else
          Rigg.BerechneWinkel
      else
        Rigg.UpdateGetriebe;
      Rigg.UpdateRigg;
      PunktOK := Rigg.GetriebeOK and Rigg.MastOK and Rigg.RiggOK;

      { Ergebnisse einspeichern }
      if yavVorstagSpannung in YAchseSet then
      begin
        j := YAchseRecordList[yavVorstagSpannung].ArrayIndex;
        if PunktOK then af[p,j,i] := Rigg.rF[14]
        else af[p,j,i] := 0;
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

      Application.ProcessMessages;
      if ProgressDlg.Aborted then
        Break;
    end; { i-Schleife }
    if ProgressDlg.Aborted then
    begin
      Reset;
      Break;
    end;
  end; { p-Schleife }

  finally { EOutOfMemory }
    { Getriebe wiederherstellen }
    Rigg.ProofRequired := True;
    Rigg.Glieder := InputRec;
    RiggModul.UpdateGetriebe;
  end;
end;

procedure TChartForm.Draw;
begin
  FShowTogether := False;
  DoLegend;
  if FValid then
  begin
    LeftTitel := GetYText(YComboBox.Text);
    BottomTitel := XAchseText;
    if FLegend then
      RightTitel := Format('Parameter %s', [ParamText])
    else
      RightTitel := '';
    Xmin := XAchseMin;
    Xmax := XAchseMax;
    LookForYMinMax;
  end
  else
  begin
    TopTitel := '';
    if FStatus = [] then
      LeftTitel := 'Diagramm wurde zurückgesetzt'
    else if csBerechnet in FStatus then
      LeftTitel := 'Kurve wurde nicht berechnet!'
    else if csGeladen in FStatus then
      LeftTitel := 'Kurve wurde nicht geladen!';
    BottomTitel := '';
    RightTitel := '';
    Xmin := 0;
    Xmax := 100;
    Ymin := 0;
    Ymax := 100;
    StraightLine;
  end;

  YGap := Round((Ymax-Ymin)/10)+1;
  if YGap = 0 then
    YGap := 0.2;
  {Ymax-Ymin darf nicht Null sein - sonst Abbruch nach VBX-Exception!}
  if (Ymax-Ymin < 0.1) then
  begin
    Ymin := Ymin-0.1;
    Ymax := Ymax+0.1;
  end;
  {Xmax-Xmin darf nicht Null sein - sonst Abbruch nach VBX-Exception!}
  if (Xmax-Xmin < 0.1) then
  begin
    Xmin := Xmin-0.1;
    Ymax := Ymax+0.1;
  end;

  DrawToChart;
end;

procedure TChartForm.YComboBoxChange(Sender: TObject);
var
  p, j: Integer;
begin
  if (YComboBox.ItemIndex < 0) and (YComboBox.ItemIndex > VNr-1) then
  begin
    FValid := False;
    YMinEdit.Text := 'YMin';
    YMaxEdit.Text := 'YMax';
    Draw;
    Exit;
  end;
  j := ComboIndexToCurve(YComboBox.ItemIndex);
  if not Valid then
  begin
    YMinEdit.Text := 'YMin';
    YMaxEdit.Text := 'YMax';
    Draw;
    Exit;
  end;
  for p := 0 to ParamCount-1 do
    bf[p] := af[p,j];
  UpdateYMinMax;
  Draw; { auch TestF zeichnen }
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

procedure TChartForm.ShowTogetherBtnClick(Sender: TObject);
begin
  if FStatus = [] then
    Exit;
  if KurvenzahlSpinner.Position > ParamCount then
    KurvenzahlSpinner.Position := ParamCount;
  ShowTogether(KurvenZahlSpinner.Position);
  FShowTogether := True;
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
  for i := 0 to YComboBox.Items.Count-1 do
  begin
    YAV := YAchseSortedList[i];
    if YAV in YAchseSet then
    begin
      j := YAchseRecordList[YAV].ArrayIndex;
      if p = PNr then Break;
      bf[p] := af[ParamNo-1,j];
      GroupText[p] := YAchseRecordList[YAV].ComboText;
      p := p+1;
    end;
  end;

  GroupKurvenZahl := p;
  for p := 0 to GroupKurvenZahl-1 do
  begin
    { Maximum und Minimum ermitteln }
    max := bf[p,0];
    min := max;
    for i := 0 to LineCount do
    begin
      if bf[p,i] > max then max := bf[p,i];
      if bf[p,i] < min then min := bf[p,i];
    end;

    { Normieren }
    diff := max-min;
    temp := P*100/GroupKurvenZahl;
    if max-min = 0 then
      for i := 0 to LineCount do
        bf[p,i] := temp
    else
    begin
      for i := 0 to LineCount do
      try
        bf[p,i] := (bf[p,i]-min)*100/diff;
      except on EMathError do
        bf[p,i] := 0;
      end;
    end;
  end;

  FValid := True;
  YMinEdit.Text := 'YMin';
  YMaxEdit.Text := 'YMax';
  TopTitel := '';
  LeftTitel := 'Alle Kurven normiert [%]';
  BottomTitel := XAchseText;
  RightTitel := Format('Parameter Nr.%d', [ParamNo]);
  Xmin := XAchseMin;
  Xmax := XAchseMax;
  Ymin := 0;
  Ymax := 100;

  YGap := Round((Ymax-Ymin)/10)+1;
  if YGap = 0 then
    YGap := 0.2;
  if (Ymax-Ymin < 0.1) then
  begin
    Ymin := Ymin-0.1;
    Ymax := Ymax+0.1;
  end;
  if (Xmax-Xmin < 0.1) then
  begin
    Xmin := Xmin-0.1;
    Ymax := Ymax+0.1;
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

{ Maximum und Minimum suchen für eine einzelne Kurve. }
procedure TChartForm.UpdateYMinMax;
var
  min, max: double;
  i, j, p: Integer;
begin
  p := PSpinner.Position-1;
  j := ComboIndexToCurve(YComboBox.ItemIndex);
  if not Valid then
  begin
    { MessageBeep(MB_ICONEXCLAMATION); }
    YMinEdit.Text := 'YMin';
    YMaxEdit.Text := 'YMax';
    Exit;
  end;
  f := af[p,j];
  max := f[0];
  min := max;
  for i := 0 to LineCount do
  begin
    if f[i] > max then max := f[i];
    if f[i] < min then min := f[i];
  end;
  YMinEdit.Text := Format('%6.2f',[min]);
  YMaxEdit.Text := Format('%6.2f',[max]);
end;

{ Maximum und Minimum suchen über alle Parameter hinweg }
procedure TChartForm.LookForYMinMax;
var
  i, j, p: Integer;
begin
  if RggDocument.CalcTyp = ctQuerKraftBiegung then
  begin
    if (YComboBox.Text = 'Vorstag-Spannung') or
       (YComboBox.Text = 'Wanten-Spannung') then
    begin
      YMax := 5000; {5000 N}
      YMin := -1000; {-1000 N}
      Exit;
    end;
    if (YComboBox.Text = 'Elastizität Punkt C') then
    begin
      YMax := 1000; {1000 mm}
      YMin := 0;
      Exit;
    end;
  end;

  p := PSpinner.Position-1; {Index für Parameter}
  j := ComboIndexToCurve(YComboBox.ItemIndex); {Index für Kurve}
  if not Valid then
  begin
    { MessageBeep(MB_ICONEXCLAMATION); }
    YMinEdit.Text := 'YMin';
    YMaxEdit.Text := 'YMax';
    Exit;
  end;
  Ymax := af[p,j,0];
  Ymin := Ymax;
  for p := 0 to ParamCount-1 do
  begin
    f := af[p,j];
    for i := 0 to LineCount do
    begin
      if f[i] > Ymax then
        Ymax := f[i];
      if f[i] < Ymin then
        Ymin := f[i];
    end;
  end;
end;

procedure TChartForm.YAuswahlClick(Sender: TObject);
var
  i: Integer;
begin
  with YAuswahlDlg do
  begin
    if not (DstList.ItemIndex = -1) then
    begin
      { In DstList den gleichen Eintrag wie in YComboBox selektieren }
      for i := 0 to DstList.Items.Count-1 do
        DstList.Selected[i] := False;
      DstList.ItemIndex := YComboBox.ItemIndex;
      DstList.Selected[YComboBox.ItemIndex] := True;
    end;
    if ShowModal = mrOK then
    begin
      if (DstList.Items.Count = 0) then
      begin
        { mindestens ein Eintrag muß sich in DestList befinden }
        DstList.Items.AddObject(SrcList.Items[0], SrcList.Items.Objects[0]);
        SrcList.Items.Delete(0);
      end;
      YComboBox.Items := DstList.Items;
      YComboBox.ItemIndex := DstList.ItemIndex;
      UpdateYAchseList;
      YComboBoxChange(Sender);
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

procedure TChartForm.DoLegend; { Virtuell }
begin
  if ParamCount = 1 then FLegend := False;
  if ParamCount > 1 then FLegend := True;
end;

procedure TChartForm.DrawToChart; { Virtuell }
begin
  { do nothing here }
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
end;

procedure TChartForm.GetMemoText;
var
  p: Integer;
  YAV: TYAchseValue;
  xpName: TxpName;
  T: TTrimmTabDaten;
begin
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
    case RiggModul.ControllerTyp of
      ctDruck: Add('ControllerTyp: Controller überträgt Druck');
      ctOhne: Add('ControllerTyp: kein Controller');
    end;
    { CalcTyp }
    if SalingTyp = stOhne then
      Add('BerechnungsTyp: Wantenkraft vorgegeben');
    if SalingTyp <> stOhne then
    case RiggModul.CalcTyp of
      ctQuerKraftBiegung: Add('BerechnungsTyp: nur Quekraftbiegung');
      ctBiegeKnicken: Add('BerechnungsTyp: Biegeknicken');
      ctKraftGemessen: begin
        Add('BerechnungsTyp: Trimmtabelle verwendet');
        Add('');
        Add('Trimmtabelle:');
        T := RiggModul.Rigg.TrimmTab.TrimmTabDaten;
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
            Add(Format('  a1: %g',[T.a1]));
            Add(Format('  a2: %g',[T.a2]));
            Add(Format('  Maximale Kraft: %g N',[T.x2]));
            Add(Format('  Maximale Auslenkung: %g mm',[T.a2*T.x2*T.x2 + T.a1*T.x2]));
          end;
          itBezier:
          begin
            Add('  KurvenTyp: Bezier');
            Add(Format('  KontrollPunkt bei (%g mm, %g N)',[T.a1,T.a2]));
            Add(Format('  Endpunkt bei (%g mm, %g N)',[T.x1,T.x2]));
          end;
        end;
      end;
    end;
    { X }
    Add('');
    Add('XAchse: ' + XAchseText);
    Add(Format('  %d ... %d',[XAchseMin, XAchseMax]));
    { P }
    if ParamCount > 1 then
    begin
      Add('');
      Add('Parameter: '+ ParamText);
      with MemoLines do
      begin
        for p := 0 to ParamCount-1 do
          Add(Format('  #%d: %s (%s) ',[p+1, PText[p], PColorText[p]]));
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
    with RiggModul.Rigg do
    begin
      if (ControllerTyp = ctDruck) and (xpName <> xpController) then
        Add(Format('  Controller: %g mm',[RealGlied[fpController]]));
      if (SalingTyp <> stOhne) and ManipulatorMode and (xpName <> xpWinkel) then
        Add(Format(  '  Winkel: %g Grad',[RealGlied[fpWinkel]*180/pi]));
      if (SalingTyp <> stOhne) and not ManipulatorMode and (xpName <> xpVorstag) then
        Add(Format('  Vorstag: %g mm',[RealGlied[fpVorstag]]));
      if xpName <> xpWante then
        Add(Format(  '  Wante: %g mm',[RealGlied[fpWante]]));
      case SalingTyp of stFest, stDrehbar:
        if xpName <> xpWoben then
          Add(Format('  WanteOben: %g mm',[RealGlied[fpWoben]]));
      end;

      if (SalingTyp = stFest) and not (xpName in [xpSalingH, xpSalingL, xpSalingW]) then
        Add(Format('  SalingHöhe: %g mm',[RealGlied[fpSalingH]]));
      if (SalingTyp = stFest) and not (xpName in [xpSalingA, xpSalingL, xpSalingW]) then
        Add(Format('  SalingAbstand: %g mm',[RealGlied[fpSalingA]]));
      if (SalingTyp = stFest) and (xpName = xpSalingL) then
        { SalingWinkel ausgeben }
        if RealGlied[fpSalingA] <> 0 then
          Add(Format('  SalingWinkel: %g Grad',[
            arctan2(RealGlied[fpSalingH], RealGlied[fpSalingA])*180/pi]));
      if (SalingTyp = stFest) and (xpName = xpSalingW) then
        Add(Format('  SalingLänge: %g mm',[RealGlied[fpSalingL]]));

      if (SalingTyp = stDrehbar) and (xpName <> xpSalingL) then
        Add(Format('  SalingLänge: %g mm',[RealGlied[fpSalingL]]));
      if (SalingTyp = stOhne) and (xpName <> xpVorstag) then { nicht VorstagOS - ok }
        Add(Format('  Vorstag: %g mm',[RealGlied[fpVorstagOS]]));
      if (SalingTyp = stOhne) and (xpName <> xpWPowerOS) then
        Add(Format('  Wantenspannung: %g N',[RealGlied[fpWPowerOS]]));
    end;
    { Koordinaten }
    Add('');
    Add('Rumpf: Koordinaten (x,y,z) [mm]');
    with RiggModul.Rigg do
    begin
      Add(Format('  A0(%g,%g,%g)',[rP[ooA0,x],rP[ooA0,y],rP[ooA0,z]]));
      Add(Format('  B0(%g,%g,%g)',[rP[ooB0,x],rP[ooB0,y],rP[ooB0,z]]));
      Add(Format('  C0(%g,%g,%g)',[rP[ooC0,x],rP[ooC0,y],rP[ooC0,z]]));
      Add(Format('  D0(%g,%g,%g)',[rP[ooD0,x],rP[ooD0,y],rP[ooD0,z]]));
      Add(Format('  E0(%g,%g,%g)',[rP[ooE0,x],rP[ooE0,y],rP[ooE0,z]]));
      Add(Format('  F0(%g,%g,%g)',[rP[ooF0,x],rP[ooF0,y],rP[ooF0,z]]));
    end;
    { Mast }
    Add('');
    Add('Mast:');
    with RiggModul.Rigg do
    begin
      Add(Format('  D0D: %d mm (Saling)',[Round(MastUnten)]));
      Add(Format('  D0C: %d mm (Vorstag)',[Round(MastUnten + MastOben)]));
      Add(Format('  D0F: %d mm (Top)',[Round(MastLaenge)]));
      Add(Format('  Biegesteifigkeit EI: %d Nm^2',[MastEI]));
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
      WriteBuffer(af[p], SizeOf(yArray));
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
      ReadBuffer(af[p], SizeOf(yArray));
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
  if PSpinner.Max = 1 then PSpinner.Max := 2;
  KurvenZahlSpinner.Position := ParamCount;
  RebuildYCombo;
  YComboBoxChange(Self);
end;

procedure TChartForm.SaveItemClick(Sender: TObject);
begin
  if not SaveDialog.Execute then Exit;
  SaveToFile(SaveDialog.FileName);
end;

procedure TChartForm.XComboBoxChange(Sender: TObject);
begin
  UpdateXMinMax;
  FXTextClicked := XComboBox.Text;
  if (XComboBox.Text = XComboText) and (csBerechnet in FStatus) then
    XLED.Brush.Color := clLime
  else
    XLED.Brush.Color := clRed;
  UpdatePCombo(SalingTyp);
end;

procedure TChartForm.PComboBoxChange(Sender: TObject);
begin
  UpdatePMinMax;
  FPTextClicked := PComboBox.Text;
  if (PComboBox.Text = PComboText) and (csBerechnet in FStatus) then
    PLED.Brush.Color := clLime
  else
    PLED.Brush.Color := clRed;
end;

procedure TChartForm.UpdateXCombo(SalingTyp: TSalingTyp);
var
  i: Integer;
begin
  with XComboBox.Items do
  begin
    Clear;
    if SalingTyp = stFest then
    begin
      Add('Controller');
      Add('Vorstag');
      Add('Winkel');
      Add('Wante');
      Add('Wante oben');
      Add('Saling Höhe');
      Add('Saling Abstand');
      Add('Saling Länge');
      Add('Saling Winkel');
    end;
    if SalingTyp = stDrehbar then
    begin
      Add('Controller');
      Add('Vorstag');
      Add('Wante');
      Add('Wante oben');
      Add('Saling Länge');
    end;
    if SalingTyp = stOhne_2 then
    begin
      Add('Controller');
      Add('Vorstag');
      Add('Wante');
    end;
    if SalingTyp = stOhne then
    begin
      Add('Vorstag');
    end;
  end;
  XComboBox.ItemIndex := XComboBox.Items.IndexOf('Vorstag');
  for i := 0 to XComboBox.Items.Count-1 do
    if (XComboBox.Items[i] = FXTextClicked) then
    begin
      XComboBox.ItemIndex := i;
      Break;
    end;
  UpdatePCombo(SalingTyp);
end;

procedure TChartForm.UpdatePCombo(SalingTyp: TSalingTyp);
var
  i: Integer;
begin
  with PComboBox.Items do
  begin
    Clear;
    Add('kein Parameter');
    if XComboBox.Text = 'Controller' then
    begin
      if SalingTyp = stFest then
      begin
        Add('Vorstag');
        Add('Winkel');
        Add('Wante');
        Add('Wante oben');
        Add('Saling Höhe');
        Add('Saling Abstand');
        Add('Saling Länge');
        Add('Saling Winkel');
      end;
      if SalingTyp = stDrehbar then
      begin
        Add('Vorstag');
        Add('Wante');
        Add('Wante oben');
        Add('Saling Länge');
      end;
      if SalingTyp = stOhne_2 then
      begin
        Add('Vorstag');
        Add('Wante');
      end;
    end
    else if (XComboBox.Text = 'Vorstag') then
    begin
      if SalingTyp = stFest then
      begin
        Add('Controller');
        Add('Wante');
        Add('Wante oben');
        Add('Saling Höhe');
        Add('Saling Abstand');
        Add('Saling Länge');
        Add('Saling Winkel');
      end;
      if SalingTyp = stDrehbar then
      begin
        Add('Controller');
        Add('Wante');
        Add('Wante oben');
        Add('Saling Länge');
      end;
      if SalingTyp = stOhne_2 then
      begin
        Add('Controller');
        Add('Wante');
      end;
    end
    else if (XComboBox.Text = 'Winkel') then
    begin
      if SalingTyp = stFest then
      begin
        Add('Controller');
        Add('Wante');
        Add('Wante oben');
        Add('Saling Höhe');
        Add('Saling Abstand');
        Add('Saling Länge');
        Add('Saling Winkel');
      end;
    end
    else if (XComboBox.Text = 'Wante') then
    begin
      if SalingTyp = stFest then
      begin
        Add('Controller');
        Add('Vorstag');
        Add('Winkel');
        Add('Wante oben');
        Add('Saling Höhe');
        Add('Saling Abstand');
        Add('Saling Länge');
        Add('Saling Winkel');
      end;
      if SalingTyp = stDrehbar then
      begin
        Add('Controller');
        Add('Vorstag');
        Add('Wante oben');
        Add('Saling Länge');
      end;
      if SalingTyp = stOhne_2 then
      begin
        Add('Controller');
        Add('Vorstag');
      end;
    end
    else if (XComboBox.Text = 'Wante oben') then
    begin
      if SalingTyp = stFest then
      begin
        Add('Controller');
        Add('Vorstag');
        Add('Winkel');
        Add('Wante');
        Add('Saling Höhe');
        Add('Saling Abstand');
        Add('Saling Länge');
        Add('Saling Winkel');
      end;
      if SalingTyp = stDrehbar then
      begin
        Add('Controller');
        Add('Vorstag');
        Add('Wante');
        Add('Saling Länge');
      end;
    end
    else if (XComboBox.Text = 'Saling Höhe') then
    begin
      if SalingTyp = stFest then
      begin
        Add('Controller');
        Add('Vorstag');
        Add('Winkel');
        Add('Wante');
        Add('Wante oben');
        Add('Saling Abstand');
        Add('Saling Länge');
        Add('Saling Winkel');
      end;
    end
    else if (XComboBox.Text = 'Saling Abstand') then
    begin
      if SalingTyp = stFest then
      begin
        Add('Controller');
        Add('Vorstag');
        Add('Winkel');
        Add('Wante');
        Add('Wante oben');
        Add('Saling Höhe');
        Add('Saling Länge');
        Add('Saling Winkel');
      end;
    end
    else if (XComboBox.Text = 'Saling Länge') then
    begin
      if SalingTyp = stFest then
      begin
        Add('Controller');
        Add('Vorstag');
        Add('Winkel');
        Add('Wante');
        Add('Wante oben');
        Add('Saling Höhe');
        Add('Saling Abstand');
        Add('Saling Winkel');
      end;
      if SalingTyp = stDrehbar then
      begin
        Add('Controller');
        Add('Vorstag');
        Add('Wante');
        Add('Wante oben');
      end;
    end
    else if (XComboBox.Text = 'Saling Winkel') then
    begin
      if SalingTyp = stFest then
      begin
        Add('Controller');
        Add('Vorstag');
        Add('Winkel');
        Add('Wante');
        Add('Wante oben');
      end;
    end;
  end;
  PComboBox.ItemIndex := 0;
  for i := 0 to PComboBox.Items.Count-1 do
    if (PComboBox.Items[i] = FPTextClicked) then
    begin
      PComboBox.ItemIndex := i;
      Break;
    end;

  UpdatePMinMax;
end;

function TChartForm.GetXText(Text: string): string;
var
  S: string;
begin
  if Text = 'Controller' then S := 'Zustellung Mast-Controller [mm]'
  else if Text = 'Winkel' then S := 'Winkel [1E-1 Grad]'
  else if Text = 'Vorstag' then S := 'Vorstaglänge [mm]'
  else if Text = 'Wante' then S := 'Wantenlänge [mm]'
  else if Text = 'Wante oben' then S := 'Länge des oberen Wantenabschnitts [mm]'
  else if Text = 'Saling Höhe' then S := 'Höhe des Salingdreiecks [mm]'
  else if Text = 'Saling Abstand' then S := 'Saling-Abstand [mm]'
  else if Text = 'Saling Länge' then S := 'Saling-Länge [mm]'
  else if Text = 'Saling Winkel' then S := 'Saling-Winkel [Grad]';
  Result := S;
end;

function TChartForm.GetPText(Text: string): string;
var
  S: string;
begin
  if Text = 'Controller' then S := 'Zustellung Mast-Controller [mm]'
  else if Text = 'Winkel' then S := 'Winkel [1E-1 Grad]'
  else if Text = 'Vorstag' then S := 'Vorstaglänge [mm]'
  else if Text = 'Wante' then S := 'Wantenlänge [mm]'
  else if Text = 'Wante oben' then S := 'Länge des oberen Wantenabschnitts [mm]'
  else if Text = 'Saling Höhe' then S := 'Höhe des Salingdreiecks [mm]'
  else if Text = 'Saling Abstand' then S := 'Saling-Abstand [mm]'
  else if Text = 'Saling Länge' then S := 'Saling-Länge [mm]'
  else if Text = 'Saling Winkel' then S := 'Saling-Winkel [Grad]';
  Result := S;
end;

function TChartForm.GetTsbName(Text: string): TxpName;
begin
  result := xpController;
  if Text = 'Winkel' then result := xpWinkel
  else if Text = 'Vorstag' then result := xpVorstag
  else if Text = 'Wante' then result := xpWante
  else if Text = 'Wante oben' then result := xpWoben
  else if Text = 'Saling Höhe' then result := xpSalingH
  else if Text = 'Saling Abstand' then result := xpSalingA
  else if Text = 'Saling Länge' then result := xpSalingL
  else if Text = 'Saling Winkel' then result := xpSalingW;
  {else if Text = 'Vorstag OS' then result := xpVorstagOS}
  {else if Text = 'Wantenkraft OS' then result := xpWPowerOS}
end;

procedure TChartForm.UpdateXMinMax;
var
  S: string;
  name: TxpName;
  tempMin, tempMax, tempIst, Minimum, Maximum: Integer;
  f: TRggSB;
begin
  S := XComboBox.Text;

  if S = 'Controller' then name := xpController
  else if S = 'Winkel' then name := xpWinkel
  else if S = 'Vorstag' then name := xpVorstag
  else if S = 'Wante' then name := xpWante
  else if S = 'Wante oben' then name := xpWoben
  else if S = 'Saling Höhe' then name := xpSalingH
  else if S = 'Saling Abstand' then name := xpSalingA
  else if S = 'Saling Länge' then name := xpSalingL
  else if S = 'Saling Winkel' then name := xpSalingW
  else
    exit;

  if (SalingTyp = stFest) and (name = xpSalingL) then
  begin
    SalingDreieck.CopyFromRigg(RiggModul.Rigg);
    tempMin := Ceil(SalingDreieck.Saling_LMin);
    tempMax := Floor(SalingDreieck.Saling_LMax);
    tempIst := Round(SalingDreieck.Saling_L);
  end else if name = xpSalingW then
  begin
    SalingDreieck.CopyFromRigg(RiggModul.Rigg);
    tempMin := Ceil(SalingDreieck.Saling_WMin*180/pi);
    tempMax := Floor(SalingDreieck.Saling_WMax*180/pi);
    tempIst := Round(SalingDreieck.Saling_W*180/pi);
  end
  else
  begin
    f := RiggModul.Rigg.GSB.GetSB(TsbName(name));
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
      XMinEdit.Text := '0';
      XMaxEdit.Text := '100';
      Valid := False;
      XLED.Brush.Color := clRed;
    end;
  end;
end;

procedure TChartForm.UpdatePMinMax;
var
  S: string;
  name: TxpName;
  tempMin, tempMax, tempIst, Minimum, Maximum: Integer;
  f: TRggSB;
begin
  S := PComboBox.Text;
  if S = ('kein Parameter') then
  begin
    PMinEdit.Text := '0';
    PMaxEdit.Text := '0';
    KurvenZahlSpinner.Position := 1;
    Exit;
  end;

  if S = 'Controller' then name := xpController
  else if S = 'Winkel' then name := xpWinkel
  else if S = 'Vorstag' then name := xpVorstag
  else if S = 'Wante' then name := xpWante
  else if S = 'Wante oben' then name := xpWoben
  else if S = 'Saling Höhe' then name := xpSalingH
  else if S = 'Saling Abstand' then name := xpSalingA
  else if S = 'Saling Länge' then name := xpSalingL
  else if S = 'Saling Winkel' then name := xpSalingW
  else
    Exit;

  if (SalingTyp = stFest) and (name = xpSalingL) then
  begin
    SalingDreieck.CopyFromRigg(RiggModul.Rigg);
    tempMin := Ceil(SalingDreieck.Saling_LMin);
    tempMax := Floor(SalingDreieck.Saling_LMax);
    tempIst := Round(SalingDreieck.Saling_L);
  end
  else if name = xpSalingW then
  begin
    SalingDreieck.CopyFromRigg(RiggModul.Rigg);
    tempMin := Ceil(SalingDreieck.Saling_WMin*180/pi);
    tempMax := Floor(SalingDreieck.Saling_WMax*180/pi);
    tempIst := Round(SalingDreieck.Saling_W*180/pi);
  end
  else
  begin
    f := RiggModul.Rigg.GSB.GetSB(TsbName(name));
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
      PMinEdit.Text := '0';
      PMaxEdit.Text := '100';
      Valid := False;
      PLED.Brush.Color := clRed;
    end;
  end;
end;

function TChartForm.ValidateInput(Input: TMaskEdit): Boolean;
var
  S: string;
  I: Integer;
  Code: Integer;
begin
  Result := False;
  try
    Val(Input.Text, I, Code);
    if Code <> 0 then
    begin
      S := Format('''%s'' ist kein gültiger Integerwert', [Input.Text]);
      MessageDlg(S, mtWarning, [mbOK], 0);
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
      S := Format('''%s'' ist kein gültiger Integerwert', [Input.Text]);
      MessageDlg(S, mtWarning, [mbOK], 0);
      Input.SetFocus;
    end;
  end;
end;

function  TChartForm.CheckBeforeCalc: Boolean;
begin
  result := True;
  if (XMinEdit.Text = 'XMinEdit') or (XMaxEdit.Text = 'XMaxEdit') then
    UpdateXMinMax;
  if (PMinEdit.Text = 'PMinEdit') or (PMaxEdit.Text = 'PMaxEdit') then
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
  XComboText := XComboBox.Text;
  XAchseText := GetXText(XComboBox.Text);
  XAchseMin := StrToInt(XMinEdit.Text);
  XAchseMax := StrToInt(XMaxEdit.Text);
  { Parameter }
  PLED.Brush.Color := clLime;
  PComboText := PComboBox.Text;
  ParamText := GetPText(PComboBox.Text);

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
  if not Assigned(RggDocument) then Exit;
  if (csGeladen in FStatus) or (csBerechnet in FStatus)then
  begin
    RiggModul.Neu(RggDocument);
    RiggModul.ViewModelMain.Caption := 'Rigg';
  end;
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
end;

end.

