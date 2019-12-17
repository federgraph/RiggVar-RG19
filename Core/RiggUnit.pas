unit RiggUnit;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Types,
  UITypes,
  Math,
  Graphics,
  Forms,
  Controls,
  Menus,
  Dialogs,
  StdCtrls,
  Buttons,
  ExtCtrls,
  Tabs,
  ComCtrls,
  RggTypes,
  RggGBox,
  Rggunit4,
  IOTypes,
  Vcalc116,
  RggCtrls,
  Rggdoc,
  Printers,
  uRggPrinter,
  Polarkar,
  RiggVar.RG.Def;

type
  TSBMappingArray = array[TsbName] of Integer;

const
  ANr = 6;
  SBMappingArray: TSBMappingArray = (0, 1, 1, 2, 3, 4, 5, 5, 6, 7);

type
  ChartArray = array[0..ANr - 1] of TChartLineData;

  TReportItem = (
    rL_Item,
    rLe_Item,
    rF_Item,
    rP_Item,
    rPe_Item,
    DiffL_Item,
    DiffP_Item,
    Log_Item
  );

  TRiggModul = class(TComponent)
  private
    { Felder für Properties }
    FSofortBerechnen: Boolean;
    FKorrigiertItem: Boolean;
    FPaintBtnDown: Boolean;
    FBtnBlauDown: Boolean;
    FBtnGrauDown: Boolean;
    FKoppelBtnDown: Boolean;
    FZweischlagBtnDown: Boolean;
    FControllerBtnDown: Boolean;
    FWinkelBtnDown: Boolean;
    FDiffBtnDown: Boolean;
    FSofortBtnDown: Boolean;
    FReportItem: TReportItem;
    FCalcTyp: TCalcTyp;
    FKurveValid: Boolean;
    FChartValid: Boolean;
    FViewPoint: TViewPoint;
    FLEDShape: Boolean;
    FSBName: TsbName;
    FCursorSB: TsbName;
    FSalingTyp: TSalingTyp;
    FControllerTyp: TControllerTyp;

    { private Variablen, keine Properties }
    NeedPaint: Boolean;
    Grauzeichnen: Boolean;
    TextFlipFlop: Boolean;

    (* wird nicht benötigt ?
    Changed: Boolean; { um mehrfaches Update zu verhindern }
    Changing: Boolean;
    *)

    { Diagramm }
    {   an anderer Stelle:
      SBName,
      CursorSB,
      KurveValid,
      ChartValid,
      YComboSavedItemIndex,
      YComboBox-Zeiger,
      ChartPaintBox-Zeiger,
      procedure DrawPoint;
      procedure DrawChartPaintBox(Canvas: TCanvas; Rect: TRect);
    }
    sbPuffer: TTrimmControls;
    TopTitel, LeftTitel, BottomTitel, RightTitel: string;
    Xmin, Xmax, Ymin, Ymax, YGap: single;
    ChartPunktX, ChartPunktY: single;
    PunktColor: TColor;
    f, TestF: TChartLineData;
    af: ChartArray;
    bf: array[0..ANr - 1] of real;
    ShowTriangle: Boolean;
    procedure StraightLine;
    procedure GetCurves;
    procedure UpdateGetriebePunkt;
    procedure UpdateRiggPunkt;
    procedure DrawChart;
    procedure LookForYMinMax;
    function GetXText(sbn: TsbName): string;
    function GetYText(Text: string): string;
    function GetPunktColor: TColor;
    {Ende Diagramm}

    { private Getters and Setters }
    function GetControllerEnabled: Boolean;
    procedure SetKorrigiertItem(Value: Boolean);
    procedure SetViewPoint(Value: TViewPoint);
    procedure SetPaintBtnDown(Value: Boolean);
    procedure SetBtnBlauDown(Value: Boolean);
    procedure SetBtnGrauDown(Value: Boolean);
    procedure SetKoppelBtnDown(Value: Boolean);
    procedure SetZweischlagBtnDown(Value: Boolean);
    procedure SetControllerBtnDown(Value: Boolean);
    procedure SetDiffBtnDown(Value: Boolean);
    procedure SetSofortBtnDown(Value: Boolean);
    procedure SetReportItem(Value: TReportItem);
    procedure SetCalcTyp(Value: TCalcTyp);
    procedure SetKurveValid(Value: Boolean);
    procedure SetLEDShape(Value: Boolean);
    procedure SetSBName(Value: TSBName);
    procedure SetSalingTyp(Value: TSalingTyp);
    procedure SetControllerTyp(Value: TControllerTyp);
    procedure SetWinkelBtnDown(Value: Boolean);
  public
    Rigg: TRigg;
    RiggReport: TRiggReport;
    FWReport: TFWReport;
    MemCtrl: TTrimmControls;
    RefCtrl: TTrimmControls;
    RefPoints: TRealRiggPoints;

    ConsoleActive: Boolean;
    ChartFormActive: Boolean;
    ReportFormActive: Boolean;
    RotaFormActive: Boolean;

    IniFileName: string;
    lbMastfall, lbSpannung, lbBiegung: string;
    Modified: Boolean;

    GetriebeGrafik: TGetriebeGraph;
    BitmapG: TBitmap;
    MetaFileG: TRiggMetaFile;
    DataInMeta: Boolean;
    DataInMetaCounter: Integer;
    MetaGPaintCount: Integer;
    MetaGMaxCount: Integer;
    ThickPenWidth: Integer;

    SalingCtrl: TSalingCtrl;
    BitmapS: TBitmap;
    BitmapC: TBitmap;
    ControllerPaintBox: TPaintBox;
    SalingPaintBox: TPaintBox;
    KraftPaintBox: TPaintBox;
    ChartPaintBox: TPaintBox;
    YComboBox: TComboBox;
    YComboSavedItemIndex: Integer;

    AutoSave: Boolean;
    AllreadyUpdatedGetriebeFlag: Boolean;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure DoOnWheelScroll(fp: TFederParam; ScrollPos: Integer);

    procedure SetupGCtrl(a: TScrollBar; b: TsbName);
    procedure SetupGCtrls;
    procedure UpdateGControls;
    procedure UpdateGCtrls(InputRec: TTrimmControls);
    procedure UpdateGCtrlLabels(InputRec: TTrimmControls);
    procedure UpdateGetriebe;
    procedure UpdateRigg;
    procedure AusgabeText;
    procedure AusgabeKommentar;
    procedure ResetPaintBoxG;
    procedure PrintPaintBoxG;
    procedure PreviewPaintBoxG;
    procedure DrawPaintBoxG(Canvas: TCanvas);
    procedure DrawToMetaG(Canvas: TMetaFileCanvas);
    procedure CopyMetaFileG; { --> Clipboard }
    procedure DrawPaintBoxM;
    procedure DrawPaintBoxS(Canvas: TCanvas);
    procedure DrawPaintBoxC(Canvas: TCanvas);
    procedure Draw;
    procedure PaintBackGround(Image: TBitmap);
    procedure DoGraphics;
    procedure DrawPoint;
    procedure DrawChartPaintBox(Canvas: TCanvas; Rect: TRect);
    procedure AdjustGrafik;
    procedure AdjustGBox(Sender: TObject);
    procedure GetGBoxOffset;

    {frühere EventHandler}
    procedure UpdateGUI;
    procedure Neu(Doc: TRggDocument);
    procedure Open(FileName: string);
    procedure Save;
    procedure About;
    procedure rLItemClick(Item: TReportItem);
    procedure UpdateBtnClick;
    procedure BiegeNeigeItemClick;
    procedure ReglerBtnClick;
    procedure MemoryBtnClick;
    procedure MemoryRecallBtnClick;
    procedure FestItemClick;
    procedure OhneItemClick;
    procedure OSDlgItemClick;
    procedure DrehbarItemClick;
    procedure DatenItemClick;
    procedure ZustellBtnClick;
    procedure OutputPagesChange(Seite: Integer);
    procedure YComboBoxChange(ItemIndex: Integer);
    procedure KurveBtnClick;
    procedure SalingPaintBoxClick;
    procedure TestBtnClick;
    procedure ChartItemClick;
    procedure ReportItemClick;
    procedure OptionItemClick;
    procedure RotaFormItemClick;
    procedure PrintGrafik;
    procedure WriteReportToMemo(Memo: TMemo);
    procedure SalingTypChange(Sender: TObject);
    procedure InputPagesChange(Sender: TObject);
    procedure sbControllerScroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);

    { Properties }
    property KorrigiertItem: Boolean read FKorrigiertItem write SetKorrigiertItem;
    property ViewPoint: TViewPoint read FViewPoint write SetViewPoint;
    property PaintBtnDown: Boolean read FPaintBtnDown write SetPaintBtnDown;
    property BtnBlauDown: Boolean read FBtnBlauDown write SetBtnBlauDown;
    property BtnGrauDown: Boolean read FBtnGrauDown write SetBtnGrauDown;
    property KoppelBtnDown: Boolean read FKoppelBtnDown write SetKoppelBtnDown;
    property ZweischlagBtnDown: Boolean read FZweischlagBtnDown write SetZweischlagBtnDown;
    property ControllerBtnDown: Boolean read FControllerBtnDown write SetControllerBtnDown;
    property WinkelBtnDown: Boolean read FWinkelBtnDown write SetWinkelBtnDown;
    property DiffBtnDown: Boolean read FDiffBtnDown write SetDiffBtnDown;
    property SofortBtnDown: Boolean read FSofortBtnDown write SetSofortBtnDown;
    property ReportItem: TReportItem read FReportItem write SetReportItem;
    property CalcTyp: TCalcTyp read FCalcTyp write SetCalcTyp;
    property ControllerEnabled: Boolean read GetControllerEnabled;
    property KurveValid: Boolean read FKurveValid write SetKurveValid;
    property LEDShape: Boolean read FLEDShape write SetLEDShape;
    property SBName: TSBName read FSBName write SetSBName;
    property CursorSB: TSBName read FCursorSB write FCursorSB;
    property SalingTyp: TSalingTyp read FSalingTyp write SetSalingTyp;
    property ControllerTyp: TControllerTyp read FControllerTyp write SetControllerTyp;
    property SofortBerechnen: Boolean read FSofortBerechnen write FSofortBerechnen;
  end;

var
  RiggModul: TRiggModul;

implementation

uses
  Clipbrd,
  FWUnit,
  FrmMain,
  RggScroll,
  Rggmat01,
  FrmConsole,
  FrmInput,
  FrmOutput,
  FrmGrafic,
  FrmReport,
  FrmSelect,
  FrmRegler,
  FrmReglerGraph,
  FrmOptions,
  FrmChart,
  FrmChartRgg,
  FrmAniRot,
  FrmRot,
  FrmPreview,
  FrmBiege,
  FrmKreis,
  FrmAdjust;

constructor TRiggModul.Create(AOwner: TComponent);
begin
  inherited;
  FSofortBerechnen := True;
  FKorrigiertItem := True;
  FPaintBtnDown := False;
  FBtnBlauDown := False;
  FBtnGrauDown := True;
  FKoppelBtnDown := True;
  FZweischlagBtnDown := False;
  FControllerBtnDown := True;
  FWinkelBtnDown := False;
  FDiffBtnDown := False;
  FSofortBtnDown := True;
  FReportItem := rF_Item;
  FCalcTyp := ctBiegeKnicken;
  FKurveValid := False;
  FChartValid := False;
  FViewPoint := vpSeite;
  FLEDShape := False;
  FSBName := fpVorstag;
  FCursorSB := fpVorstag;
  FSalingTyp := stFest;
  FControllerTyp := ctDruck;

  {Grauzeichnen := False;}{ braucht nicht initialisiert werden }
  {TextFlipFlop := False;}{ braucht nicht initialisiert werden }
  IniFileName := '';

  RiggModul := Self;
  ControllerPaintBox := OutputForm.ControllerPaintBox;
  SalingPaintBox := OutputForm.SalingPaintBox;
  KraftPaintBox := OutputForm.KraftPaintBox;
  ChartPaintBox := OutputForm.ChartPaintBox;
  YComboBox := OutputForm.YComboBox;

  {Rigg}
  Rigg := TRigg.Create;

  SetupGCtrls;

  MemCtrl := ZeroCtrl;
  RefCtrl := Rigg.Glieder;
  SBPuffer := Rigg.Glieder;
  RefPoints := Rigg.rP;

  { GetriebeGrafik }
  GetriebeGrafik := TGetriebeGraph.Create;
  GetriebeGrafik.Rotator := TPolarKar2.Create;
  with GetriebeGrafik.Rotator do
  begin
    DeltaPhi := 0;
    DeltaTheta := -90;
    Xrot := -87;
  end;
  GetriebeGrafik.SetMastKurve(Rigg.MastLinie, Rigg.lc, Rigg.beta);
  GetriebeGrafik.Koordinaten := Rigg.rP;
  GetriebeGrafik.Koppelkurve := Rigg.Koppelkurve;
  GetriebeGrafik.Ansicht := vpSeite;

  BitmapG := TBitmap.Create;
  with BitmapG do
  begin
    Width := 293; {PaintBoxG.Width;}
    Height := 422; {PaintBoxG.Height;}
    Canvas.Font.Name := 'Arial'; {'MS Sans Serif';}
    Canvas.Font.Height := 14;
  end;
  PaintBackGround(BitmapG);

  MetaGMaxCount := 50;
  MetaFileG := TRiggMetaFile.Create;
  with MetaFileG do
  begin
    Width := 293;
    Height := 422;
  end;

  {SalingCtrls}
  SalingCtrl := TSalingCtrl.Create;
  SalingCtrl.PBSize := Point(453, 220);
  {SalingCtrl.PBSize := SalingPaintBox.ClientRect.BottomRight;}

  BitmapS := TBitmap.Create;
  with BitmapS do
  begin
    Width := 453; {SalingPaintBox.Width;}
    Height := 220; {SalingPaintBox.Height;}
  end;
  PaintBackGround(BitmapS);

  BitmapC := TBitmap.Create;
  with BitmapC do
  begin
    Width := 453; {ControllerPaintBox.Width;}
    Height := 220; {ControllerPaintBox.Height;}
  end;
  PaintBackGround(BitmapC);

  { Berichte }
  RiggReport := TRiggReport.Create;
  FWReport := TFWReport.Create;

  {Chart}
  YComboBox.ItemIndex := 1; { sonst Exception, wenn ItemIndex := -1 }
  YComboSavedItemIndex := 1;
  StraightLine;
  DrawChart;

  NeedPaint := True;
  UpdateGetriebe;
  TestBtnClick;

//  try
//    RggInputServer := TRggInputServer.Create;
//    RggOutputServer := TRggOutputServer.Create;
//  except
//    RggInputServer.Free;
//    RggInputServer := nil;
//    RggOutputServer.Free;
//    RggInputServer := nil;
//  end;
end;

destructor TRiggModul.Destroy;
begin
//  RggInputServer.Free;
//  RggOutputServer.Free;

  GetriebeGrafik.Rotator.Free;
  GetriebeGrafik.Free;
  SalingCtrl.Free;
  RiggReport.Free;
  FWReport.Free;
  BitmapG.Free;
  MetaFileG.Free;
  BitmapS.Free;
  BitmapC.Free;

  inherited;
end;

procedure TRiggModul.SetupGCtrl(a: TScrollBar; b: TsbName);
var
  cr: TRggSB;
begin
  cr := Rigg.GSB.Find(b);
  a.SetParams(cr.Ist, cr.Min, cr.Max);
  a.LargeChange := cr.BigStep;
  a.SmallChange := cr.TinyStep;
end;

procedure TRiggModul.SetupGCtrls;
begin
  with InputForm do
  begin
    {Controller}
    SetupGCtrl(sbController, fpController);
    SetupGCtrl(sbControllerD, fpController);
    SetupGCtrl(sbControllerOhne, fpController);
    {Vorstag/Winkel}
    if WinkelBtnDown then
      SetupGCtrl(sbWinkel, fpWinkel)
    else
      SetupGCtrl(sbWinkel, fpVorstag);
    SetupGCtrl(sbVorstagD, fpVorstag);
    SetupGCtrl(sbVorstagOhne, fpVorstag);
    {Wante}
    SetupGCtrl(sbWante, fpWante);
    SetupGCtrl(sbWanteD, fpWante);
    SetupGCtrl(sbWanteOhne, fpWante);
    {Woben}
    SetupGCtrl(sbWoben, fpWoben);
    SetupGCtrl(sbWobenD, fpWoben);
    {Saling}
    SetupGCtrl(sbSalingH, fpSalingH);
    SetupGCtrl(sbSalingA, fpSalingA);
    SetupGCtrl(sbSalingLD, fpSalingL);
    {Ohne Saling starr}
    SetupGCtrl(sbVorstagOS, fpVorstagOS);
    sbVorstagOS.Position := Rigg.GSB.Find(fpVorstag).Ist;
    SetupGCtrl(sbWPowerOS, fpWPowerOS);
  end;
  UpdateGCtrlLabels(Rigg.Glieder);
end;

procedure TRiggModul.UpdateGControls;
begin
  UpdateGCtrls(Rigg.Glieder);
end;

procedure TRiggModul.UpdateGCtrlLabels(InputRec: TTrimmControls);
begin
  with InputRec, InputForm do
  begin
    lbValue1.Caption := Format('%d mm', [Controller - MemCtrl.Controller]);
    if WinkelBtnDown then
    begin
      lbWinkel.Caption := 'Winkel';
      lbValue2.Caption := Format('%5.2f Grad', [(Winkel - MemCtrl.Winkel) / 10]);
    end
    else
    begin
      lbWinkel.Caption := 'Vorstag';
      lbValue2.Caption := Format('%d mm', [Vorstag - MemCtrl.Vorstag]);
    end;
    lbValue3.Caption := Format('%d mm', [Wanten - MemCtrl.Wanten]);
    lbValue4.Caption := Format('%d mm', [Woben - MemCtrl.Woben]);
    lbValue5.Caption := Format('%d mm', [SalingH - MemCtrl.SalingH]);
    lbValue6.Caption := Format('%d mm', [SalingA - MemCtrl.SalingA]);
    lbD5.Caption := Format('%d mm', [SalingL - MemCtrl.SalingL]);
    lbValue7.Caption := Format('%d mm', [Vorstag - MemCtrl.Vorstag]);
    lbValue8.Caption := Format('%d mm', [WPowerOS - MemCtrl.WPowerOS]);

    lbD1.Caption := lbValue1.Caption;
    lbD2.Caption := lbValue2.Caption;
    lbD3.Caption := lbValue3.Caption;
    lbD4.Caption := lbValue4.Caption;
    {lbD5.Caption := lbValue5.Caption;}{ SalingL: oben schon gesetzt }

    lbOhne1.Caption := lbValue1.Caption;
    lbOhne2.Caption := lbValue2.Caption;
    lbOhne3.Caption := lbValue3.Caption;
  end;
end;

procedure TRiggModul.UpdateGCtrls(InputRec: TTrimmControls);
begin
  sbPuffer := InputRec;

  with InputRec, InputForm do
  begin
    sbController.Position := Controller;
    sbControllerD.Position := Controller;
    sbControllerOhne.Position := Controller;

    if WinkelBtnDown then
      sbWinkel.Position := Winkel
    else
      sbWinkel.Position := Vorstag;
    sbVorstagD.Position := Vorstag;
    sbVorstagOhne.Position := Vorstag;

    sbWante.Position := Wanten;
    sbWanteD.Position := Wanten;
    sbWanteOhne.Position := Wanten;

    sbWoben.Position := Woben;
    sbWobenD.Position := Woben;

    sbSalingH.Position := SalingH;
    sbSalingA.Position := SalingA;
    sbSalingLD.Position := SalingL;

    sbVorstagOS.Position := Vorstag;
    sbWPowerOS.Position := WPowerOS;
  end;

  UpdateGCtrlLabels(InputRec);
  if RotaFormActive then
    AniRotationForm.Modified := True;
end;

procedure TRiggModul.sbControllerScroll(Sender: TObject;
  ScrollCode: TScrollCode; var ScrollPos: Integer);
var
  InputRec: TTrimmControls;
begin
  Modified := True;
  InputRec := Rigg.Glieder;

  { resolution of with clause explained: }
//  if Sender = InputForm.sbController then
//  begin
//    InputRec.Controller := ScrollPos;
//    InputForm.lbValue1.Caption := Format('%d mm', [ScrollPos - MemCtrl.Controller]);
//    if not ControllerBtnDown then
//      NeedPaint := False;
//  end;

  with InputRec, InputForm do
  begin
    if Sender = sbController then
    begin
      Controller := ScrollPos;
      lbValue1.Caption := Format('%d mm', [ScrollPos - MemCtrl.Controller]);
      if not ControllerBtnDown then
        NeedPaint := False;
    end
    else if Sender = sbControllerD then
    begin
      Controller := ScrollPos;
      lbD1.Caption := Format('%d mm', [ScrollPos - MemCtrl.Controller]);
      if not ControllerBtnDown then
        NeedPaint := False;
    end
    else if Sender = sbControllerOhne then
    begin
      Controller := ScrollPos;
      lbOhne1.Caption := Format('%d mm', [ScrollPos - MemCtrl.Controller]);
      if not ControllerBtnDown then
        NeedPaint := False;
    end
    else if Sender = sbWinkel then
    begin
      if WinkelBtnDown then
      begin
        Winkel := ScrollPos;
        lbValue2.Caption := Format('%5.2f Grad', [(Winkel - MemCtrl.Winkel) /
          10]);
      end
      else
      begin
        Vorstag := ScrollPos;
        lbValue2.Caption := Format('%d mm', [Vorstag - MemCtrl.Vorstag]);
      end
    end
    else if Sender = sbVorstagD then
    begin
      Vorstag := ScrollPos;
      lbD2.Caption := Format('%d mm', [Vorstag - MemCtrl.Vorstag]);
    end
    else if Sender = sbVorstagOhne then
    begin
      Vorstag := ScrollPos;
      lbOhne2.Caption := Format('%d mm', [Vorstag - MemCtrl.Vorstag]);
    end
    else if Sender = sbWante then
    begin
      Wanten := ScrollPos;
      lbValue3.Caption := Format('%d mm', [ScrollPos - MemCtrl.Wanten]);
    end
    else if Sender = sbWanteD then
    begin
      Wanten := ScrollPos;
      lbD3.Caption := Format('%d mm', [ScrollPos - MemCtrl.Wanten]);
    end
    else if Sender = sbWanteOhne then
    begin
      Wanten := ScrollPos;
      lbOhne3.Caption := Format('%d mm', [ScrollPos - MemCtrl.Wanten]);
    end
    else if Sender = sbWoben then
    begin
      Woben := ScrollPos;
      lbValue4.Caption := Format('%d mm', [ScrollPos - MemCtrl.Woben]);
    end
    else if Sender = sbWobenD then
    begin
      Woben := ScrollPos;
      lbD4.Caption := Format('%d mm', [ScrollPos - MemCtrl.Woben]);
    end
    else if Sender = sbSalingH then
    begin
      SalingH := ScrollPos;
      lbValue5.Caption := Format('%d mm', [ScrollPos - MemCtrl.SalingH]);
    end
    else if Sender = sbSalingA then
    begin
      SalingA := ScrollPos;
      lbValue6.Caption := Format('%d mm', [ScrollPos - MemCtrl.SalingA]);
    end
    else if Sender = sbSalingLD then
    begin
      SalingL := ScrollPos;
      lbD5.Caption := Format('%d mm', [ScrollPos - MemCtrl.SalingL]);
    end
    else if Sender = sbVorstagOs then
    begin
      Vorstag := ScrollPos;
      lbValue7.Caption := Format('%d mm', [ScrollPos - MemCtrl.Vorstag])
    end
    else if Sender = sbWPowerOS then
    begin
      WPowerOS := ScrollPos;
      lbValue8.Caption := Format('%d N', [ScrollPos - MemCtrl.WPowerOS]);
      NeedPaint := False;
    end;
  end;

  if (ScrollCode = scEndScroll) or not SofortBerechnen then
  begin
    if (Sender as TScrollbar).Tag = SBMappingArray[CursorSB] then
      ShowTriangle := True
    else
    begin
      ShowTriangle := False;
      KurveValid := False;
    end;
    sbPuffer := InputRec;
    Rigg.Glieder := InputRec;
    UpdateGetriebe;
    AllreadyUpdatedGetriebeFlag := True;
  end;
end;

procedure TRiggModul.UpdateGetriebe;
begin
  Rigg.UpdateGetriebe;
  if NeedPaint then
    DoGraphics;
  NeedPaint := True;
  if (SofortBerechnen and Rigg.GetriebeOK and Rigg.MastOK) then
    UpdateRigg;
end;

procedure TRiggModul.DoGraphics;
var
  TrimmRec: TTrimmControls;
begin
  { Koppelkurve }
  if (SalingTyp = stFest) and (KoppelBtnDown = True) then
    GetriebeGrafik.Koppelkurve := Rigg.Koppelkurve;

  { 3D Grafik - AniRotationForm muß erzeugt sein! }
  if RotaFormActive then
  begin
    if AniRotationForm.Visible then
    begin
      { Trackbar und Labels flackern zu sehr, daher nicht immer aktualisieren }
      //if SofortBerechnen then
      //  Modified := True;
      AniRotationForm.UpdateAll(Rigg);
      AniRotationForm.Draw;
      AniRotationForm.Invalidate;
    end;
  end;

  Getriebegrafik.SetMastKurve(Rigg.MastLinie, Rigg.lc, Rigg.beta);

  { ControllerPaintBox }
  if OutputForm.OutputPages.ActivePage = OutputForm.ControllerSheet then
  begin
    TrimmRec := Rigg.Glieder;
    with SalingCtrl do
    begin
      { Abstand(iP[ooE0,x],iP[ooE,x]) in mm}
      ControllerPos := TrimmRec.Controller;
      { Position des Mastes in Deckshöhe von D0 aus in mm }
      ParamXE := Rigg.MastPositionE;
      { Abstand(iP[ooD0,x],iP[ooE0,x]) in mm }
      ParamXE0 := Rigg.iP[ooE0, x] - Rigg.iP[ooD0, x];
      { Abstand von E0 zur Anschlagkante Deck + Klotzdicke }
      EdgePos := Rigg.GSB.Find(fpController).Min;
      if Assigned(ControllerPaintBox) then
        DrawPaintBoxC(ControllerPaintBox.Canvas);
    end;
  end;

  { SalingPaintBox }
  if OutputForm.OutputPages.ActivePage = OutputForm.SalingSheet then
  begin
    TrimmRec := Rigg.Glieder;
    with SalingCtrl do
    begin
      { SalingAbstand }
      SalingA := TrimmRec.SalingA;
      { Abstand Verbindungslinie Salinge zu Hinterkante Mast in mm }
      SalingH := TrimmRec.SalingH;
      { Salinglänge in mm - außerhalb berechnen }
      SalingL := TrimmRec.SalingL;
      if Assigned(SalingPaintBox) then
        DrawPaintBoxS(SalingPaintBox.Canvas);
    end;
  end;

  { Diagramm aktuellen Punkt setzen }
  if OutputForm.OutputPages.ActivePage = OutputForm.ChartSheet then
    UpdateGetriebePunkt;

  TextFlipFlop := False;
  { Grafik aktualisieren, aber nicht zweimal!}
  if not (SofortBerechnen and Rigg.GetriebeOK and Rigg.MastOK) then
  begin
    Grauzeichnen := False;
    if Rigg.GetriebeOK then
      LEDShape := True
    else
      LEDShape := False;
    FormMain.Statusbar.Panels[1].Text := Rigg.GetriebeStatusText;
    DrawPaintBoxM;
    Draw;
    if Rigg.GetriebeOK and not Rigg.MastOK then
    begin
      LEDShape := False;
      FormMain.Statusbar.Panels[1].Text := Rigg.MastStatusText;
    end;
  end;
end;

procedure TRiggModul.UpdateRigg;
begin
  Rigg.UpdateRigg;
  FormMain.Statusbar.Panels[1].Text := Rigg.RiggStatusText;
  if Rigg.RiggOK then
  begin
    Grauzeichnen := True;
    LEDShape := True;
  end
  else
  begin
    Grauzeichnen := False;
    LEDShape := False;
  end;
  if OutputForm.OutputPages.ActivePage = OutputForm.ChartSheet then
    UpdateRiggPunkt; {GetriebePunkte oben schon aktualisiert}
  AusgabeText; {Texte müssen vor Draw gesetzt werden}
  AusgabeKommentar;
  DrawPaintBoxM;
  Draw;
  rLItemClick(ReportItem);
end;

(***************************************************************************)

procedure TRiggModul.Draw;
var
  MetaCanvas: TMetaFileCanvas;
begin
  with BitmapG.Canvas do
  begin
    if PaintBtnDown = False then
    begin
      PaintBackGround(BitmapG);
      Textout(180, 16, lbMastfall);
      Textout(180, 32, lbSpannung);
      Textout(180, 48, lbBiegung);
    end
    else if TextFlipFlop then
      PaintBackGround(BitmapG);
  end;

  DrawPaintBoxG(BitMapG.Canvas);
  with GrafikForm.PaintBoxG.Canvas do
  begin
    CopyMode := cmSrcCopy;
    Draw(0, 0, BitMapG);
  end;

  if PaintBtnDown = True and (MetaGPaintCount <= MetaGMaxCount) then
  begin
    MetaCanvas := TMetaFileCanvas.Create(MetaFileG, 0);
    try
      if DataInMeta and (DataInMetaCounter < 8) then
      begin
        Inc(DataInMetaCounter);
        MetaCanvas.Draw(0, 0, MetaFileG);
      end
      else
        DataInMetaCounter := 0;
      GetriebeGrafik.ZoomFaktor := 10;
      DrawToMetaG(MetaCanvas);
    finally
      MetaCanvas.Free;
      GetriebeGrafik.ZoomFaktor := 1;
      Inc(MetaGPaintCount);
    end;
  end;

end;

procedure TRiggModul.DrawPaintBoxG(Canvas: TCanvas);
begin
  GetriebeGrafik.ZeichneKoppel := KoppelBtnDown;

  { entspanntes Rigg grau zeichnen }
  if Grauzeichnen and BtnGrauDown then
  begin
    with GetriebeGrafik do
    begin
      Farbe := clEntspannt;
      Coloriert := False;
      WanteGestrichelt := not Rigg.GetriebeOK;
      Koordinaten := Rigg.rPe;
      Draw(Canvas);
    end;
  end;
  { Nullstellung hellblau zeichnen }
  if BtnBlauDown then
  begin
    with GetriebeGrafik do
    begin
      Farbe := clNull;
      Coloriert := False;
      WanteGestrichelt := False;
      Koordinaten := RefPoints;
      Draw(Canvas);
    end;
  end;
  { gespanntes Rigg farbig zeichnen}
  with Getriebegrafik do
  begin
    // SetMastKurve(Rigg.MastLinie, Rigg.lc, Rigg.beta); // jetzt oben schon gesetzt
    Coloriert := True;
    WanteGestrichelt := not Rigg.GetriebeOK;
    Koordinaten := Rigg.rP;
    Draw(Canvas);
  end;
end;

procedure TRiggModul.DrawToMetaG(Canvas: TMetaFileCanvas);
begin
  GetriebeGrafik.ZeichneKoppel := KoppelBtnDown;

  { entspanntes Rigg grau zeichnen }
  if Grauzeichnen and BtnGrauDown then
  begin
    with GetriebeGrafik do
    begin
      Farbe := clBlack;
      Coloriert := False;
      WanteGestrichelt := not Rigg.GetriebeOK;
      Koordinaten := Rigg.rPe;
      DrawToMeta(Canvas);
    end;
  end;
  { Nullstellung hellblau zeichnen }
  if BtnBlauDown then
  begin
    with GetriebeGrafik do
    begin
      Farbe := clNull;
      Coloriert := False;
      WanteGestrichelt := False;
      Koordinaten := RefPoints;
      DrawToMeta(Canvas);
    end;
  end;
  { gespanntes Rigg farbig zeichnen}
  with Getriebegrafik do
  begin
    // SetMastKurve(Rigg.MastLinie, Rigg.lc, Rigg.beta); // siehe oben
    Coloriert := True;
    WanteGestrichelt := not Rigg.GetriebeOK;
    Koordinaten := Rigg.rP;
    Canvas.Pen.Width := ThickPenWidth;
    DrawToMeta(Canvas);
  end;

  DataInMeta := True;
end;

procedure TRiggModul.PaintBackGround(Image: TBitMap);
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

procedure TRiggModul.DrawPaintBoxM;
var
  PaintBox: TPaintBox;
begin
  case InputForm.InputPages.ActivePage.Tag of
    0: PaintBox := InputForm.PaintBoxM;
    1: PaintBox := InputForm.PaintBoxMD;
    2: PaintBox := InputForm.PaintBoxMOhne;
  else
    Exit;
  end;
  Rigg.DrawMastLine(PaintBox.Canvas, PaintBox.BoundsRect);
end;

(******************************************************************)

procedure TRiggModul.AusgabeText;
var
  tempSalingDaten: TSalingDaten;
  MemoPosY: LongInt;
begin
  tempSalingDaten := Rigg.SalingDaten;

  MemoPosY := SendMessage(OutputForm.DisplayMemo.Handle,
    EM_GETFIRSTVISIBLELINE, 0, 0);
  with Rigg, tempSalingDaten, OutputForm.DisplayMemo.Lines do
  begin
    BeginUpdate;
    Clear;

    { Text setzen }
    lbMastFall := Format('Mastfall = %5.1f cm', [Trimm.Mastfall / 10]);
    lbSpannung := Format('Spannung = %5.0f N', [rF[14]]);
    lbBiegung := Format('Biegung  = %5.1f cm', [hd / 10]);

    Add('Trimm:');
    Add(Format('  Mastfall F0F     = %8.1f cm', [Trimm.Mastfall / 10]));
    Add(Format('  Vorstagspannung  = %8.1f N', [rF[14]]));
    Add(Format('  Durchbiegung hd  = %8.1f cm', [hd / 10]));

    Add('');
    Add('Saling:');
    Add(Format('  Saling Länge   = %6.2f mm', [SalingL]));
    Add(Format('  Saling Höhe    = %6.2f mm', [SalingH]));
    Add(Format('  Saling Abstand = %6.2f mm', [SalingA]));
    Add(Format('  Saling Winkel  = %6.2f Grad', [SalingW]));
    Add(Format('  Wanten Winkel  = %6.2f Grad', [WantenWinkel]));
    Add(Format('  Kraft Winkel   = %6.2f Grad', [KraftWinkel]));

    Add('');
    Add('SchnittKräfte:');
    Add(Format('  FC  = %8.2f N    (Mastdruckkraft)', [FC]));
    Add(Format('  FB  = %8.2f N    (Wanten/Vorstag)', [FB]));
    Add(Format('  F2  = %8.2f N    (Saling)', [F2]));
    Add(Format('  F1  = %8.2f N    (Controller)', [F1]));
    Add(Format('  FA  = %8.2f N    (Mastfuß)', [FA]));
    Add(Format('  hd  = %8.2f mm   (Saling Durchbiegung)', [hd]));
    Add(Format('  he  = %8.2f mm   (Controller Durchbiegung)', [he]));
    Add(Format('  sd  = %8.2f mm   (hd-FSalingWegKnick)', [hd-FSalingWegKnick]));

    Add('');
    Add('BiegeKnicken:');
    Add(Format('  KoppelFaktor       = %8.5f', [FKoppelFaktor]));
    Add(Format('  SalingAlpha        = %8.5f mm/N', [FSalingAlpha]));
    Add(Format('  ControllerAlpha    = %8.5f mm/N', [FControllerAlpha]));
    Add(Format('  SalingWeg          = %8.2f mm', [FSalingWeg]));
    Add(Format('  SalingWegKnick     = %8.2f mm', [FSalingWegKnick]));
    Add(Format('  ControllerWeg      = %8.2f mm', [FControllerWeg]));
    Add(Format('  FSchnittPunktKraft = %8.2f N', [FSchnittPunktKraft]));
    Add(Format('  FwSchnittOhne      = %8.2f mm', [FwSchnittOhne]));
    Add(Format('  FwSchnittMit       = %8.2f mm', [FwSchnittMit]));
    Add(Format('  FwSchnittOffset    = %8.2f mm', [FwSchnittOffset]));

    Add('');
    Add('SchnittWinkel:');
    Add(Format('  alpha1 = %6.2f Grad', [alpha1 * 180 / pi]));
    Add(Format('  alpha2 = %6.2f Grad', [alpha2 * 180 / pi]));
    Add(Format('  delta1 = %6.2f Grad', [delta1 * 180 / pi]));
    Add(Format('  delta2 = %6.2f Grad', [delta2 * 180 / pi]));
    Add(Format('  gamma  = %6.2f Grad', [gamma * 180 / pi]));
    Add(Format('  beta   = %6.2f Grad', [beta * 180 / pi]));

    Add('');
    Add('Winkel:');
    Add(Format('  phi       = %6.2f Grad', [phi * 180 / pi]));
    Add(Format('  psi       = %6.2f Grad', [psi * 180 / pi]));
    Add(Format('  alpha     = %6.2f Grad', [alpha * 180 / pi]));
    Add(Format('  phi-alpha = %6.2f Grad (Mast-Neigung)', [(phi-alpha)*180/pi]));
    Add(Format('  psi-alpha = %6.2f Grad (Wanten-Neigung)', [(psi-alpha)*180/pi]));

    Add('');
    Add('MastWinkel:');
    Add(Format('  epsB = %6.2f Grad', [epsB * 180 / pi]));
    Add(Format('  eps2 = %6.2f Grad', [eps2 * 180 / pi]));
    Add(Format('  eps1 = %6.2f Grad', [eps1 * 180 / pi]));
    Add(Format('  epsA = %6.2f Grad', [epsA * 180 / pi]));
    Add(Format('  Epsilon  = %6.2f Grad', [epsilon * 180 / pi]));

    SendMessage(OutputForm.DisplayMemo.Handle, EM_LINESCROLL, 0, MemoPosY);
    EndUpdate; {OutputForm.DisplayMemo.Lines.EndUpdate}
  end;
end;

procedure TRiggModul.AusgabeKommentar;
var
  tempSalingDaten: TSalingDaten;
  temp: real;
begin
  tempSalingDaten := Rigg.SalingDaten;
  with Rigg, tempSalingDaten, OutputForm.KommentarMemo.Lines do
  begin
    BeginUpdate;
    Clear;

    temp := hd / 10; { Biegung in cm }
    if temp < 0 then
      Add('Mastbiegung negativ!');
    if temp < 2 then
      Add('Mast hat fast keine Vorbiegung.');
    if temp > 10 then
      Add('Mastbiegung zu groß.');

    temp := rF[14]; { Vorstagspannung in N }
    if temp < 800 then
      Add('Vorstagspannung zu gering.');
    if temp > 2000 then
      Add('Vorstagspannung zu groß.');

    EndUpdate;
  end;
end;

procedure TRiggModul.SetReportItem(Value: TReportItem);
begin
  if FReportItem <> Value then
  begin
    FReportItem := Value;
    rLItemClick(Value);
    with OutputForm do
      OutputPages.ActivePage := MasterMemo;
  end;
end;

procedure TRiggModul.rLItemClick(Item: TReportItem);
begin
  RiggReport.ML.Clear;
  RiggReport.ML.Add('');
  with RiggReport, Rigg do
  begin
    if Item = rL_Item then
      AusgabeRL(rL)
    else if Item = rLe_Item then
      AusgabeRLE(rLe)
    else if Item = rP_Item then
      AusgabeRP(rP)
    else if Item = rPe_Item then
      AusgabeRPE(rPe)
    else if Item = rF_Item then
      AusgabeRF(rF)
    else if Item = DiffL_Item then
      AusgabeDiffL(rL, rLe)
    else if Item = DiffP_Item then
      AusgabeDiffP(rP, rPe)
    else if Item = Log_Item then
      AusgabeLog(LogList);
  end;
  with RiggReport.ML do
  begin
    Add(' Angezeigt werden die zuletzt gültigen Werte.');
    Add('');
    Add(' Die Tabellenwerte sind aktuell und gültig, wenn');
    Add(' - die LED Grün ist und');
    Add(' - die Taste "=" gedrückt wurde bzw.');
    Add(' - der Schalter "A" gedrückt ist.');
    Add('');
    Add(' Die Tabellenwerte können ungültig sein, wenn');
    Add(' - die LED Rot ist und/oder');
    Add(' - die Taste "=" nicht gedrückt wurde bzw.');
    Add(' - der Schalter "A" nicht gedrückt ist');
  end;
  with OutputForm do
  begin
    Memo.Lines.BeginUpdate;
    try
      Memo.Clear;
      Memo.Lines := RiggReport.ML;
      Memo.SelStart := 0;
    finally
      Memo.Lines.EndUpdate;
    end;
  end;
  RiggReport.ML.Clear;
end;

procedure TRiggModul.WriteReportToMemo(Memo: TMemo);
var
  i: Integer;
  SavedIndexAuswahl: set of TRiggLIndexRange;
begin
  Memo.Clear;

  { Rigg - Report }
  SavedIndexAuswahl := RiggReport.IndexAuswahlL;
  RiggReport.IndexAuswahlL := [0..19];
  RiggReport.IndexAuswahlP := [ooA0..ooP];
  RiggReport.ML.Clear;
  RiggReport.ML.Add('{Ausgaben Rigg 3d:}');
  RiggReport.ML.Add('');
  with RiggReport, Rigg do
  begin
    for i := 0 to MemoDlg.DstList.Items.Count - 1 do
    begin
      if MemoDlg.DstList.Items[i] = 'rP' then
        AusgabeRP(rP);
      if MemoDlg.DstList.Items[i] = 'rPe' then
        AusgabeRPE(rPe);
      if MemoDlg.DstList.Items[i] = 'DiffP' then
        AusgabeDiffP(rP, rPe);
      if MemoDlg.DstList.Items[i] = 'rL' then
        AusgabeRL(rL);
      if MemoDlg.DstList.Items[i] = 'rLe' then
        AusgabeRLE(rLe);
      if MemoDlg.DstList.Items[i] = 'DiffL' then
        AusgabeDiffL(rL, rLe);
      if MemoDlg.DstList.Items[i] = 'rF' then
        AusgabeRF(rF);
      if MemoDlg.DstList.Items[i] = 'Winkel' then
        AusgabeWinkel(alpha,
          alpha1, alpha2, beta, gamma, delta1, delta2, epsilon, phi, psi);
      if MemoDlg.DstList.Items[i] = 'TrimmControls' then
        AusgabeTrimmControls(Glieder);
      if MemoDlg.DstList.Items[i] = 'SalingDaten' then
        AusgabeSalingDaten(SalingDaten);
    end;
  end;
  Memo.Lines := RiggReport.ML;
  RiggReport.ML.Clear;
  RiggReport.IndexAuswahlL := SavedIndexAuswahl;
  RiggReport.IndexAuswahlP := [ooA..ooF];

  { Fachwerk - Report }
  Rigg.Fachwerk.BerechneVerschiebungen := True;
  Rigg.UpdateRigg;
  Rigg.Fachwerk.BerechneVerschiebungen := False;
  FWReport.ML.Clear;
  FWReport.ML.Add('{Ausgaben Fachwerk 2d:}');
  FWReport.ML.Add('');
  with FWReport, Rigg, Fachwerk do
  begin
    for i := 0 to MemoDlg.DstList.Items.Count - 1 do
    begin
      // FWReport.Ausgabe(Rigg.Fachwerk);
      if MemoDlg.DstList.Items[i] = 'FW_Geometrie' then
        AusgabeGeometrie(G, S);
      if MemoDlg.DstList.Items[i] = 'FW_StabQuerschnitte' then
        AusgabeStabQuerschnitte(vektorEA, S);
      if MemoDlg.DstList.Items[i] = 'FW_Elastizitaeten' then
        AusgabeElastizitaeten(Q, S);
      if MemoDlg.DstList.Items[i] = 'FW_Koordinaten' then
        AusgabeKoordinaten(KX, KY, K);
      if MemoDlg.DstList.Items[i] = 'FW_Belastung' then
        AusgabeBelastung(FXsaved, FYsaved, K);
      if MemoDlg.DstList.Items[i] = 'FW_Auflagerkraefte' then
        AusgabeAuflagerkraefte(Lager);
      if MemoDlg.DstList.Items[i] = 'FW_Stabkraefte' then
        AusgabeStabkraefte(FS, S);
      if MemoDlg.DstList.Items[i] = 'FW_Verschiebungen' then
        AusgabeVerschiebungen(FO1, FO2, FO, PO1, PO2, K);
    end;
  end;
  Memo.Lines.AddStrings(FWReport.ML);
  FWReport.ML.Clear;

  Memo.SelStart := 0;
end;

procedure TRiggModul.UpdateGUI;
begin
  Modified := False;
  SalingTyp := Rigg.SalingTyp;
  ControllerBtnDown := Rigg.ControllerTyp <> ctOhne;
  // ControllerTyp := Rigg.ControllerTyp; //see SetControllerBtnDown
  CalcTyp := Rigg.CalcTyp;
  FormMain.TakeOver;
  (* automatisch erneut aufgerufen:
  UpdateGetriebe; { redundant }
  Rigg.UpdateGSB; { redundant }
  SetupGCtrls; { notwendig }
  *)
end;

procedure TRiggModul.Neu(Doc: TRggDocument);
begin
  IniFileName := '';
  if Doc = nil then
    Rigg.SetDefaultDocument { --> Rigg.SetDocument }
  else
    Rigg.SetDocument(Doc);
  UpdateGUI;
end;

procedure TRiggModul.Open(FileName: string);
begin
  try
    Rigg.LoadFromDocFile(FileName); { --> Rigg.SetDocument }
    IniFileName := FileName;
    UpdateGUI;
  except
    on EFileFormatError do { eat ecxeption }
      if IniFileName = '' then
        FormMain.Caption := 'Rigg';
  end;
end;

procedure TRiggModul.Save;
begin
  Rigg.WriteToDocFile(IniFileName);
  Modified := False;
end;

procedure TRiggModul.SetViewPoint(Value: TViewPoint);
begin
  if Value <> FViewPoint then
  begin
    FViewPoint := Value;
    GetriebeGrafik.Ansicht := Value;
    TextFlipFlop := True;
    ResetPaintBoxG;
    GrafikForm.ViewTab.TabIndex := Ord(FViewPoint);
  end;
end;

procedure TRiggModul.ResetPaintBoxG;
begin
  MetaFileG.Clear;
  DataInMeta := False;
  ThickPenWidth := 1;
  MetaGPaintCount := 0;
  TextFlipFlop := True;
  Draw;
end;

procedure TRiggModul.SetPaintBtnDown(Value: Boolean);
begin
  if FPaintBtnDown <> Value then
  begin
    FPaintBtnDown := Value;
    ResetPaintBoxG;
  end;
end;

procedure TRiggModul.SetBtnBlauDown(Value: Boolean);
begin
  if FBtnBlauDown <> Value then
  begin
    FBtnBlauDown := Value;
    Draw;
  end;
end;

procedure TRiggModul.SetBtnGrauDown(Value: Boolean);
begin
  if FBtnGrauDown <> Value then
  begin
    FBtnGrauDown := Value;
    if Value then
      Draw
    else
      UpdateGetriebe;
  end;
end;

procedure TRiggModul.SetKoppelBtnDown(Value: Boolean);
begin
  if FKoppelBtnDown <> Value then
  begin
    FKoppelBtnDown := Value;
    Getriebegrafik.ZeichneKoppel := Value;
    if Value then
      GetriebeGrafik.Koppelkurve := Rigg.Koppelkurve;
    Draw;
  end;
end;

procedure TRiggModul.SetZweischlagBtnDown(Value: Boolean);
begin
  if FZweischlagBtnDown <> Value then
  begin
    FZweischlagBtnDown := Value;
    GetriebeGrafik.Bogen := not Value;
    Draw;
  end;
end;

function TRiggModul.GetControllerEnabled;
begin
  Result := True;
  if CalcTyp = ctKraftGemessen then
    Result := False;
  if SalingTyp = stOhne then
    Result := False;
end;

procedure TRiggModul.SetControllerTyp(Value: TControllerTyp);
begin
  if FControllerTyp <> Value then
  begin
    FControllerTyp := Value;
    Rigg.ControllerTyp := Value;
    GetriebeGrafik.ControllerTyp := Value;
    SalingCtrl.ControllerTyp := Value;
  end;
end;

procedure TRiggModul.SetControllerBtnDown(Value: Boolean);
var
  CT: TControllerTyp;
begin
  if FControllerBtnDown <> Value then
  begin
    FControllerBtnDown := Value;
    if Value then
      CT := ctDruck
    else
      CT := ctOhne;
    ControllerTyp := CT;
    UpdateGetriebe;
  end;
end;

procedure TRiggModul.SetCalcTyp(Value: TCalcTyp);
begin
  if Value = ctKraftGemessen then
    ControllerBtnDown := False;
  if FCalcTyp <> Value then
  begin
    FCalcTyp := Value;
    Rigg.CalcTyp := Value;
    if Value <> ctBiegeKnicken then
    begin
      with OutputForm do
        if OutputPages.ActivePage = KraftSheet then
          OutputPages.ActivePage := OutputPages.FindNextPage(KraftSheet, False,
            False);
      OutputForm.Kraftsheet.TabVisible := False;
    end
    else
      OutputForm.Kraftsheet.TabVisible := True;
    KurveValid := False;
    UpdateGetriebe;
  end;
end;

procedure TRiggModul.SetWinkelBtnDown(Value: Boolean);
begin
  if FWinkelBtnDown <> Value then
  begin
    FWinkelBtnDown := Value;
    Rigg.ManipulatorMode := Value;
    Rigg.UpdateGSB;
    SetupGCtrls;
    if Value and InputForm.rbWinkel.Checked then
      SBName := fpWinkel
    else if not Value and InputForm.rbWinkel.Checked then
      SBName := fpVorstag;
    KurveValid := False;
    UpdateGetriebe;
  end;
end;

procedure TRiggModul.SetDiffBtnDown(Value: Boolean);
begin
  if FDiffBtnDown <> Value then
  begin
    FDiffBtnDown := Value;
    if Value then
      MemCtrl := RefCtrl
    else
      MemCtrl := ZeroCtrl;
    UpdateGCtrlLabels(Rigg.Glieder);
  end;
end;

procedure TRiggModul.SetSofortBtnDown(Value: Boolean);
begin
  if FSofortBtnDown <> Value then
  begin
    FSofortBtnDown := Value;
    SofortBerechnen := Value;
    if Value then
      UpdateGetriebe
    else
      PaintBtnDown := False;
  end;
end;

procedure TRiggModul.SetSalingTyp(Value: TSalingTyp);
begin
  if FSalingTyp <> Value then
  begin
    FSalingTyp := Value;
    Rigg.SalingTyp := Value;
    Getriebegrafik.SalingTyp := Value;
    if ChartFormActive then
      ChartForm.SalingTyp := Value;
  end;
end;

procedure TRiggModul.SetLEDShape(Value: Boolean);
begin
  if FLEDShape <> Value then
  begin
    FLEDShape := Value;
    if Value then
      FormMain.LEDShape.Brush.Color := clLime
    else
      FormMain.LEDShape.Brush.Color := clRed;
  end;
end;

procedure TRiggModul.UpdateBtnClick;
begin
  Rigg.Schnittkraefte;
  UpdateRigg;
end;

procedure TRiggModul.BiegeNeigeItemClick;
var
  ControllerAnschlag: Integer;
begin
  { Controller zurückfahren auf Rigg.ControllerAnschlag
    --> entspricht 50 in GUI }
  ControllerAnschlag := 50;
  with InputForm do
  begin
    if SalingTyp = stFest then
    begin
      sbController.Position := ControllerAnschlag;
      sbControllerScroll(sbController, scEndScroll, ControllerAnschlag);
    end
    else if SalingTyp = stDrehbar then
    begin
      sbControllerD.Position := ControllerAnschlag;
      sbControllerScroll(sbControllerD, scEndScroll, ControllerAnschlag);
    end;
  end;
  BiegeUndNeigeForm.ShowModal;
  UpdateGCtrls(Rigg.Glieder);
  KurveValid := False;
  DrawPoint;
end;

procedure TRiggModul.ReglerBtnClick;
var
  CtrlForm: TForm;
  ControllerAnschlag: Integer;
begin
  { Controller zurückfahren auf Rigg.FiControllerAnschlag }
  ControllerAnschlag := 50;
  with InputForm do
  begin
    if SalingTyp = stFest then
    begin
      sbController.Position := ControllerAnschlag;
      sbControllerScroll(sbController, scEndScroll, ControllerAnschlag);
    end
    else if SalingTyp = stDrehbar then
    begin
      sbControllerD.Position := ControllerAnschlag;
      sbControllerScroll(sbControllerD, scEndScroll, ControllerAnschlag);
    end;
  end;
  CtrlForm := CtrlDlg1; {CtrlForm := CtrlDlg oder CtrlDlg1;}
  if Rigg.CalcTyp = ctKraftGemessen then
    CtrlForm := BiegeUndNeigeForm;

  CtrlForm.ShowModal;
  UpdateGCtrls(Rigg.Glieder);
  KurveValid := False;
  DrawPoint;
end;

procedure TRiggModul.MemoryBtnClick;
begin
  RefCtrl := Rigg.Glieder;
  RefPoints := Rigg.rP;
  if DiffBtnDown then
    MemCtrl := RefCtrl;
  UpdateGCtrlLabels(Rigg.Glieder);
  Draw;
end;

procedure TRiggModul.MemoryRecallBtnClick;
begin
  Rigg.Glieder := RefCtrl;
  if DiffBtnDown then
    MemCtrl := RefCtrl;
  UpdateGCtrls(Rigg.Glieder);
  KurveValid := False;
  UpdateGetriebe;
end;

procedure TRiggModul.OhneItemClick;
begin
  InputForm.pnOhneBK.Update;
  InputForm.pnMastOhne.Update;

  WinkelBtnDown := False;

  SalingTyp := stOhne_2;

  KurveValid := False;
  with InputForm do
  begin
    if rbControllerOhne.Checked then
      SBName := fpController
    else if rbVorstagOhne.Checked then
      SBName := fpVorstag
    else if rbWanteOhne.Checked then
      SBName := fpWante;
  end;

  UpdateGetriebe;
  Rigg.UpdateGSB;
  SetupGCtrls;
end;

procedure TRiggModul.DrehbarItemClick;
begin
  InputForm.pnDrehbar.Update;
  InputForm.pnMastD.Update;

  WinkelBtnDown := False;

  SalingTyp := stDrehbar;

  KurveValid := False;
  with InputForm do
  begin
    if rbControllerD.Checked then
      SBName := fpController
    else if rbVorstagD.Checked then
      SBName := fpVorstag
    else if rbWanteD.Checked then
      SBName := fpWante
    else if rbWobenD.Checked then
      SBName := fpWoben
    else if rbSalingLD.Checked then
      SBName := fpSalingL;
  end;

  UpdateGetriebe;
  Rigg.UpdateGSB;
  SetupGCtrls;
end;

procedure TRiggModul.FestItemClick;
begin
  InputForm.pnFest.Update;
  InputForm.pnMast.Update;

  SalingTyp := stFest;

  KurveValid := False;
  with InputForm do
  begin
    if rbController.Checked then
      SBName := fpController
    else if rbWinkel.Checked and WinkelBtnDown then
      SBName := fpWinkel
    else if rbWinkel.Checked and not WinkelBtnDown then
      SBName := fpVorstag
    else if rbWante.Checked then
      SBName := fpWante
    else if rbWoben.Checked then
      SBName := fpWoben
    else if rbSalingH.Checked then
      SBName := fpSalingH
    else if rbSalingA.Checked then
      SBName := fpSalingA;
  end;

  UpdateGetriebe;
  Rigg.UpdateGSB;
  SetupGCtrls;
end;

procedure TRiggModul.OSDlgItemClick;
begin
  InputForm.pnOhne.Update;

  WinkelBtnDown := False;
  SalingTyp := stOhne;
  ControllerBtnDown := False;
  KurveValid := False;
  SBName := fpVorstag;

  UpdateGetriebe;
  Rigg.UpdateGSB;
  SetupGCtrls;
end;

procedure TRiggModul.DatenItemClick;
begin
  InputForm.DoDatenItemClick;
end;

procedure TRiggModul.SalingTypChange(Sender: TObject);
begin
  with InputForm, FormMain do
  begin
    if Sender = FestItem then
      InputPages.ActivePage := tsFest;
    if Sender = DrehbarItem then
      InputPages.ActivePage := tsDrehbar;
    if Sender = OhneItem then
      InputPages.ActivePage := tsOhne;
    if Sender = OSDlgItem then
      InputPages.ActivePage := tsOhneStarr;
    if Sender = DatenItem then
      InputPages.ActivePage := tsDatenbank;
  end;
  InputPagesChange(Sender);
end;

procedure TRiggModul.InputPagesChange(Sender: TObject);
begin
  with FormMain do
  begin
    case InputForm.InputPages.ActivePage.Tag of
      0: FestItemClick(Sender);
      1: DrehbarItemClick(Sender);
      2: OhneItemClick(Sender);
      3: OSDlgItemClick(Sender);
      4: DatenItemClick(Sender);
    end;
  end;
end;

procedure TRiggModul.ChartItemClick;
begin
  ChartForm := TChartFormGS.Create(Application);
end;

procedure TRiggModul.ReportItemClick;
begin
  ReportForm := TReportForm.Create(FormMain);
end;

procedure TRiggModul.OptionItemClick;
begin
  Rigg.UpdateGSB;
  { Istwerte in GSB aktualisieren für aktuelle Werte in Optionform! }
  OptionForm.ShowModal;
  if OptionForm.ModalResult = mrOK then
  begin
    Rigg.UpdateGlieder; { neue GSB Werte --> neue Integerwerte }
    Rigg.Reset; { neue Integerwerte --> neue Gleitkommawerte }
    KurveValid := False;
    UpdateGetriebe;
    // Rigg.UpdateGSB; { enfällt hier, da GSB schon aktuell }
    SetupGCtrls;
    SBPuffer := Rigg.Glieder; { weil Istwerte nicht über Scrollbar verändert }
  end;
end;

procedure TRiggModul.RotaFormItemClick;
begin
  AniRotationForm := TAniRotationForm.Create(FormMain);
  AniRotationForm.UpdateAll(Rigg);
  (*
  { dies wird durch AniRotationForm.UpdateAll(Rigg) ersetzt
    beachte, daß Draw nicht aufgerufen wird }
  RotationForm.RaumGrafik.Salingtyp := Salingtyp;
  RotationForm.RaumGrafik.ControllerTyp := ControllerTyp;
  RotationForm.RaumGrafik.Koordinaten := Rigg.rP;
  RotationForm.RaumGrafik.SetMastKurve(Rigg.MastLinie, Rigg.lc, Rigg.beta);
  *)
end;

procedure TRiggModul.About;
begin
  KreisForm := TKreisForm.Create(Application);
  try
    KreisForm.ShowModal;
  finally
    KreisForm.Free;
  end;
end;

procedure TRiggModul.PrintGrafik;
begin
  if not PaintBtnDown then
    PreviewGForm.cbThickLines.Enabled := True
  else
  begin
    PreviewGForm.cbThickLines.Enabled := False;
    PreviewGForm.cbThickLines.Checked := False;
  end;
  PreviewGForm.ShowModal;
end;

procedure TRiggModul.SetKorrigiertItem(Value: Boolean);
begin
  if FKorrigiertItem <> Value then
  begin
    FKorrigiertItem := Value;
    Rigg.Korrigiert := Value;
    Rigg.Schnittkraefte;
    UpdateRigg;
  end;
end;

{******************************************************************************}

function GetEnvelopeSize: TPoint;
var
  EnvW, EnvH: Integer;
  PixPerInX: Integer;
  PixPerInY: Integer;
begin
  if RggPrinter.OKToPrint then
  begin
    PixPerInX := GetDeviceCaps(Printer.Handle, LOGPIXELSX);
    PixPerInY := GetDeviceCaps(Printer.Handle, LOGPIXELSY);
    EnvW := trunc((210 - 50) / 25.4 * PixPerInX);
    EnvH := trunc((297 - 50) / 25.4 * PixPerInY);
    Result := Point(EnvW, EnvH);
  end
  else
  begin
    PixPerInX := RggPrinter.PixPerInX;
    PixPerInY := RggPrinter.PixPerInY;
    EnvW := trunc((210 - 50) / 25.4 * PixPerInX);
    EnvH := trunc((297 - 50) / 25.4 * PixPerInY);
    Result := Point(EnvW, EnvH);
  end;
end;

function GetEnvelopePos(EnvSize: TPoint): TRect;
begin
  if RggPrinter.OKToPrint then
  begin
    Result := Rect(
      (Printer.PageWidth - EnvSize.x) div 2,
      (Printer.PageHeight - EnvSize.y) div 2 + 58,
      (Printer.PageWidth - EnvSize.x) div 2 + EnvSize.x,
      (Printer.PageHeight - EnvSize.y) div 2 + EnvSize.y + 58);
  end
  else
  begin
    Result := Rect(
      (RggPrinter.PageWidth - EnvSize.x) div 2,
      (RggPrinter.PageHeight - EnvSize.y) div 2 + 58,
      (RggPrinter.PageWidth - EnvSize.x) div 2 + EnvSize.x,
      (RggPrinter.PageHeight - EnvSize.y) div 2 + EnvSize.y + 58);
  end;
end;

procedure TRiggModul.PrintPaintBoxG;
var
  Rgn: THandle;
  EnvSize: TPoint;
  EnvPos: TRect;
  SavedZoomFaktor, Zoom: Integer;
  MetaCanvas: TMetaFileCanvas;
begin
  if not RggPrinter.OKToPrint then
  begin
    MessageDlg('Kein Drucker konfiguriert.', mtInformation, [mbOK], 0);
    exit;
  end;

  Zoom := 10;
  if Printer.Orientation <> poPortrait then
    Printer.Orientation := poPortrait;
  Printer.Title := 'Rigg/Getriebegrafik';
  EnvSize := GetEnvelopeSize;
  EnvPos := GetEnvelopePos(EnvSize);

  Printer.BeginDoc;

  with GrafikForm.PaintBoxG do
  begin
    SetMapMode(Printer.Canvas.Handle, MM_ISOTROPIC);
    SetWindowExtEx(Printer.Canvas.Handle, Width * Zoom, Height * Zoom, nil);
    SetWindowOrgEx(Printer.Canvas.Handle, (Width * Zoom) div 2, (Height * Zoom)
      div 2, nil);
    SetViewPortExtEx(Printer.Canvas.Handle, EnvSize.x, EnvSize.y, nil);
    SetViewPortOrgEx(Printer.Canvas.Handle,
      EnvPos.Left + EnvSize.x div 2,
      EnvPos.Bottom - EnvSize.y div 2, nil);
  end;

  Rgn := CreateRectRgnIndirect(EnvPos);
  SelectClipRgn(Printer.Canvas.Handle, Rgn);
  { SelectClipRgn() arbeitet mit Kopie von Rgn! }
  DeleteObject(Rgn);

  { Metafile schreiben; mit Pen.Width = Zoom, wenn Box gecheckt }
  if not PaintBtnDown then
  begin
    SavedZoomFaktor := GetriebeGrafik.ZoomFaktor;
    GetriebeGrafik.ZoomFaktor := Zoom;
    MetaCanvas := TMetaFileCanvas.Create(MetaFileG, 0);
    try
      if PreviewGForm.cbThickLines.Checked then
        ThickPenWidth := Zoom;
      DrawToMetaG(MetaCanvas);
    finally
      MetaCanvas.Free;
      GetriebeGrafik.ZoomFaktor := SavedZoomFaktor;
    end;
  end;

  Printer.Canvas.Draw(0, 0, MetaFileG);

  Printer.EndDoc;
end;

procedure TRiggModul.PreviewPaintBoxG;
var
  Rgn: THandle;
  R: TRect;
  Rand, OffsetY: Integer;
  SavedZoomFaktor, Zoom: Integer;
  MetaCanvas: TMetaFileCanvas;

  WindowExtX, WindowExtY: Integer;
  WindowOrgX, WindowOrgY: Integer;
  ViewPortExtX, ViewPortExtY: Integer;
  ViewPortOrgX, ViewPortOrgY: Integer;
begin
  ThickPenWidth := 1;
  Zoom := 10;

  { wenn notwendig MetafileG schreiben,
      falls Box gecheckt mit Pen.Width = 4 * Zoom }
  if not PaintBtnDown then
  begin
    SavedZoomFaktor := GetriebeGrafik.ZoomFaktor;
    GetriebeGrafik.ZoomFaktor := Zoom;
    MetaCanvas := TMetaFileCanvas.Create(MetaFileG, 0);
    try
      if PreviewGForm.cbThickLines.Checked then
        ThickPenWidth := 4 * Zoom;
      DrawToMetaG(MetaCanvas);
    finally
      MetaCanvas.Free;
      GetriebeGrafik.ZoomFaktor := SavedZoomFaktor;
    end;
  end;

  WindowExtX := GrafikForm.PaintBoxG.Width * Zoom;
  WindowExtY := GrafikForm.PaintBoxG.Height * Zoom;
  WindowOrgX := WindowExtX div 2;
  WindowOrgY := WindowExtY div 2;

  with PreviewGForm.PreviewGBox do
  begin
    Rand := 10;
    OffsetY := 0;
    ViewPortExtX := Width - Rand;
    ViewPortExtY := Height - Rand;
    ViewPortOrgX := Left + Rand div 2 + ViewPortExtX div 2;
    ViewPortOrgY := Top + Rand div 2 + OffsetY + ViewPortExtY div 2;

    SetMapMode(Canvas.Handle, MM_ISOTROPIC);
    SetWindowExtEx(Canvas.Handle, WindowExtX, WindowExtY, nil);
    SetWindowOrgEx(Canvas.Handle, WindowOrgX, WindowOrgY, nil);
    SetViewPortExtEx(Canvas.Handle, ViewPortExtX, ViewPortExtY, nil);
    SetViewPortOrgEx(Canvas.Handle, ViewPortOrgX, ViewPortOrgY, nil);

    Canvas.Brush.Color := clSilver;
    Canvas.Pen.Color := clBlue;
    Canvas.Pen.Width := 1;
    Canvas.Rectangle(0, 0, WindowExtX, WindowExtY);

    R := Rect(0, 0, WindowExtX, WindowExtY);
    LPTODP(Canvas.Handle, R, 2);
    Rgn := CreateRectRgnIndirect(R);
    SelectClipRgn(Canvas.Handle, Rgn);
    { SelectClipRgn() arbeitet mit Kopie von Rgn! }
    DeleteObject(Rgn);

    Canvas.Draw(0, 0, MetaFileG)
  end;
end;

procedure TRiggModul.CopyMetaFileG;
var
  MetaFile: TMetaFile;
begin
  MetaFile := TMetaFile.Create;
  MetaFile.Width := 293;
  MetaFile.Height := 422;
  with TMetaFileCanvas.CreateWithComment(MetaFile, 0,
    'Gustav Schubert', 'Rigg, Getriebegrafik') do
  begin
    try
      SetMapMode(Handle, MM_ISOTROPIC);
      SetWindowExtEx(Handle, 100, 100, nil);
      SetWindowOrgEx(Handle, 0, 0, nil);
      SetViewPortExtEx(Handle, 10, 10, nil);
      SetViewPortOrgEx(Handle, 0, 0, nil);

      Brush.Color := clSilver;
      Pen.Color := clBlue;
      Rectangle(0, 0, 2930, 4220);

      Draw(0, 0, MetaFileG);
    finally
      Free;
    end;
  end;
  Clipboard.Assign(MetaFile);
  MetaFile.Free;
end;

{************************************************************************}

procedure TRiggModul.StraightLine;
var
  i: Integer;
begin
  for i := 0 to CPMax do
    f[i] := 100 * i / CPMax;
  TestF := f;
end;

procedure TRiggModul.GetCurves;
var
  i, tempIndex: Integer;
  Antrieb, Anfang, Ende: real;
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
    Anfang := Rigg.GSB.Find(SBname).Min;
    Ende := Rigg.GSB.Find(SBName).Max;
    for i := 0 to CPMax do
    begin
      Antrieb := Anfang + (Ende - Anfang) * i / CPMax;
      { Antrieb ansteuern }
      case SBname of
        fpController: Rigg.RealGlied[fpController] := Antrieb;
        fpWinkel: Rigg.RealGlied[fpWinkel] := Antrieb / 10 * pi / 180;
        fpVorstag: Rigg.RealGlied[fpVorstag] := Antrieb;
        fpWante: Rigg.RealGlied[fpWante] := Antrieb;
        fpWoben: Rigg.RealGlied[fpWoben] := Antrieb;
        fpSalingH: Rigg.RealGlied[fpSalingH] := Antrieb;
        fpSalingA: Rigg.RealGlied[fpSalingA] := Antrieb;
        fpSalingL: Rigg.RealGlied[fpSalingL] := Antrieb;
        fpVorstagOS: Rigg.RealGlied[fpVorstag] := Antrieb;
      end;

      { Berechnen }
      Rigg.UpdateGetriebe;
      Rigg.UpdateRigg;
      PunktOK := Rigg.GetriebeOK and Rigg.MastOK and Rigg.RiggOK;

      { Ergebnisse einspeichern }
      tempIndex := YComboBox.Items.IndexOf('Vorstag-Spannung');
      if (tempIndex <> -1) and (tempIndex < ANr) then
      begin
        if PunktOK then
          af[tempIndex, i] := Rigg.rF[14]
        else
          af[tempIndex, i] := 0;
      end;
      tempIndex := YComboBox.Items.IndexOf('Wanten-Spannung');
      if (tempIndex <> -1) and (tempIndex < ANr) then
      begin
        if PunktOK then
          af[tempIndex, i] := Rigg.rF[8]
        else
          af[tempIndex, i] := 0;
      end;
      tempIndex := YComboBox.Items.IndexOf('Mastfall F0F');
      if (tempIndex <> -1) and (tempIndex < ANr) then
      begin
        af[tempIndex, i] := Abstand(Rigg.rP[ooF0], Rigg.rP[ooF]);
      end;
      tempIndex := YComboBox.Items.IndexOf('Mastfall F0C');
      if (tempIndex <> -1) and (tempIndex < ANr) then
      begin
        af[tempIndex, i] := Abstand(Rigg.rP[ooF0], Rigg.rP[ooC]);
      end;
      tempIndex := YComboBox.Items.IndexOf('Elastizität Punkt C');
      if (tempIndex <> -1) and (tempIndex < ANr) then
      begin
        if PunktOK then
          af[tempIndex, i] := Abstand(Rigg.rP[ooC], Rigg.rPe[ooC])
        else
          af[tempIndex, i] := 0;
      end;
      tempIndex := YComboBox.Items.IndexOf('Durchbiegung hd');
      if (tempIndex <> -1) and (tempIndex < ANr) then
      begin
        af[tempIndex, i] := Rigg.hd;
      end;
    end; { i-Schleife }

    FChartValid := True;
    KurveValid := True;
    DrawChart;

    if OutputForm.OutputPages.ActivePage = OutputForm.ChartSheet then
      if Screen.ActiveForm = ConsoleForm then
        ConsoleForm.ActiveControl := OutputForm.YComboBox
      else if Screen.ActiveForm = OutputForm then
        OutputForm.ActiveControl := OutputForm.YComboBox
      else
        MessageBeep(MB_ICONASTERISK);

  finally
    { Getriebe wiederherstellen }
    Rigg.ProofRequired := True;
    Rigg.Glieder := InputRec;
    UpdateGetriebe;
    DrawPoint;
    Screen.Cursor := crDefault;
  end;
end; { GetCurves }

procedure TRiggModul.UpdateGetriebePunkt;
var
  tempIndex: Integer;
begin
  { Ergebnisse einspeichern }
  tempIndex := YComboBox.Items.IndexOf('Mastfall F0F');
  if (tempIndex <> -1) and (tempIndex < ANr) then
  begin
    bf[tempIndex] := Abstand(Rigg.rP[ooF0], Rigg.rP[ooF]);
  end;
  tempIndex := YComboBox.Items.IndexOf('Mastfall F0C');
  if (tempIndex <> -1) and (tempIndex < ANr) then
  begin
    bf[tempIndex] := Abstand(Rigg.rP[ooF0], Rigg.rP[ooC]);
  end;
  tempIndex := YComboBox.Items.IndexOf('Durchbiegung hd');
  if (tempIndex <> -1) and (tempIndex < ANr) then
  begin
    bf[tempIndex] := Rigg.hd;
  end;

  { RiggPunkte Null setzen }
  tempIndex := YComboBox.Items.IndexOf('Vorstag-Spannung');
  if (tempIndex <> -1) and (tempIndex < ANr) then
    bf[tempIndex] := 0;
  tempIndex := YComboBox.Items.IndexOf('Wanten-Spannung');
  if (tempIndex <> -1) and (tempIndex < ANr) then
    bf[tempIndex] := 0;
  tempIndex := YComboBox.Items.IndexOf('Elastizität Punkt C');
  if (tempIndex <> -1) and (tempIndex < ANr) then
    bf[tempIndex] := 0;

  { Punkte im Diagramm aktualisieren }
  if OutputForm.cbFollowPoint.Checked and not
    (SofortBerechnen and Rigg.GetriebeOK and Rigg.MastOK) then
    DrawPoint;
end;

procedure TRiggModul.UpdateRiggPunkt;
var
  tempIndex: Integer;
begin
  with OutputForm do
  begin
    { RiggPunkte bereits in UpdateGetriebePunkt genullt! }
    if (Rigg.GetriebeOK and Rigg.MastOK and Rigg.RiggOK) then
    begin
      tempIndex := YComboBox.Items.IndexOf('Vorstag-Spannung');
      if (tempIndex <> -1) and (tempIndex < ANr) then
        bf[tempIndex] := Rigg.rF[14];
      tempIndex := YComboBox.Items.IndexOf('Wanten-Spannung');
      if (tempIndex <> -1) and (tempIndex < ANr) then
        bf[tempIndex] := Rigg.rF[13];
      tempIndex := YComboBox.Items.IndexOf('Elastizität Punkt C');
      if (tempIndex <> -1) and (tempIndex < ANr) then
        bf[tempIndex] := Abstand(Rigg.rP[ooC], Rigg.rPe[ooC]);
    end;
    if cbFollowPoint.Checked then
      DrawPoint;
  end;
end;

procedure TRiggModul.DrawChart;
var
  i: Integer;
begin
  if FChartValid then
  begin
    LeftTitel := GetYText(YComboBox.Text);
    i := YComboBox.ItemIndex;
    if i = -1 then
    begin
      i := YComboSavedItemIndex;
      // MessageBeep(MB_ICONASTERISK); { debugging }
    end;
    f := af[i];
    case SBname of
      fpController: ChartPunktX := sbPuffer.Controller;
      fpWinkel: ChartPunktX := sbPuffer.Winkel / 10;
      fpVorstag: ChartPunktX := sbPuffer.Vorstag;
      fpWante: ChartPunktX := sbPuffer.Wanten;
      fpWoben: ChartPunktX := sbPuffer.Woben;
      fpSalingH: ChartPunktX := sbPuffer.SalingH;
      fpSalingA: ChartPunktX := sbPuffer.SalingA;
      fpSalingL: ChartPunktX := sbPuffer.SalingL;
      fpVorstagOS: ChartPunktX := sbPuffer.Vorstag;
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
  if OutputForm.OutputPages.ActivePage = OutputForm.ChartSheet then
    DrawChartPaintBox(ChartPaintBox.Canvas, ChartPaintBox.BoundsRect);
end;

procedure TRiggModul.DrawChartPaintBox(Canvas: TCanvas; Rect: TRect);

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
begin
  PlotWidth := Rect.Right - Rect.Left;
  PlotHeight := Rect.Bottom - Rect.Top;
  PlotExtX := PlotWidth;
  PlotExtY := PlotHeight;
  PlotOrgX := 0;
  PlotOrgY := 0;

  bmp := TBitmap.Create;
  with bmp do
  begin
    Width := PlotWidth;
    Height := PlotHeight;
  end;
  try
    PaintBackGround(bmp);

    with bmp.Canvas do
    begin
      SetMapMode(Handle, MM_ANISOTROPIC);
      SetWindowExtEx(Handle, PlotExtX, -PlotExtY, nil);
      SetWindowOrgEx(Handle, PlotOrgX, PlotOrgY, nil);
      SetViewPortExtEx(Handle, PlotWidth, PlotHeight, nil);
      SetViewPortOrgEx(Handle, 0, PlotHeight, nil);

      {Kurve}
      Pen.Color := clBlue;
      MoveTo(0, 0);
      for i := 0 to CPMax do
      begin
        tempX := PlotExtX * (i / CPMax);
        tempY := PlotExtY * (f[i] - Ymin) / (YMax - Ymin);
        P.x := Round(Limit(tempX));
        P.y := Round(Limit(tempY));
        LineTo(P.x, P.y);
      end;

      { Aktueller Punkt bzw. X-Position }
      R.Left := 0;
      R.Top := 0;
      R.Bottom := 5;
      R.Right := 5;
      DPTOLP(Handle, R, 2);
      RadiusX := R.Right - R.Left;
      RadiusY := R.Bottom - R.Top;

      tempX := PlotExtX * (ChartPunktX - Xmin) / (XMax - Xmin);
      tempY := PlotExtY * (ChartPunktY - Ymin) / (YMax - Ymin);
      P.x := Round(Limit(tempX));
      P.y := Round(Limit(tempY));
      if (P.y <> 0) and KurveValid then
      begin
        { aktueller Punkt }
        Brush.Color := PunktColor;
        Brush.Style := bsSolid;
        Ellipse(P.x - RadiusX, P.y - RadiusY,
          P.x + RadiusX, P.y + RadiusY);
      end
      else if ShowTriangle then
      begin
        { Positionsdreieck X }
        Pen.Color := clBlack;
        P.y := 0;
        RadiusX := RadiusX;
        RadiusY := RadiusY * 2;
        Polyline([Point(P.x, P.y),
          Point(P.x - RadiusX, P.y - RadiusY),
            Point(P.x + RadiusX, P.y - RadiusY),
            Point(P.x, P.y)]);
      end;

      SetMapMode(Handle, MM_TEXT);
      SetWindowOrgEx(Handle, 0, 0, nil);
      SetViewPortOrgEx(Handle, 0, 0, nil);

      { Rahmen zeichnen }
      Pen.Width := 1;
      Pen.Color := clBlack;
      Brush.Style := bsClear;
      Rectangle(0, 0, bmp.Width, bmp.Height);
    end;

    with Canvas do
    begin
      CopyMode := cmSrcCopy;
      Draw(0, 0, bmp);
    end;

  finally
    bmp.Free;
  end;

  with OutputForm do
  begin
    lbAchseX.Caption := BottomTitel;
    lbAchseY.Caption := Lefttitel;
    lbXLeft.Caption := IntToStr(Round(Xmin));
    lbXRight.Caption := IntToStr(Round(Xmax));
    lbYBottom.Caption := IntToStr(Round(Ymin));
    lbYTop.Caption := IntToStr(Round(Ymax));
  end;
end;

function TRiggModul.GetPunktColor: TColor;
var
  i: Integer;
begin
  result := clLime;
  i := YComboBox.ItemIndex;
  with YComboBox.Items do
  begin
    if (i = IndexOf('Vorstag-Spannung')) or
      (i = IndexOf('Wanten-Spannung')) or
      (i = IndexOf('Elastizität Punkt C')) then
      if not (Rigg.RiggOK and Rigg.GetriebeOK and Rigg.MastOK) then
        result := clRed;
    if (i = IndexOf('Mastfall F0F')) or
      (i = IndexOf('Mastfall F0C')) or
      (i = IndexOf('Durchbiegung hd')) then
      if not (Rigg.GetriebeOK and Rigg.MastOK) then
        result := clRed;
  end;
end;

procedure TRiggModul.DrawPoint;
var
  i: Integer;
begin
  if not FChartValid then
    Exit;
  { Koordinaten des Punktes }
  case SBname of
    fpController: ChartPunktX := sbPuffer.Controller;
    fpWinkel: ChartPunktX := sbPuffer.Winkel / 10;
    fpVorstag: ChartPunktX := sbPuffer.Vorstag;
    fpWante: ChartPunktX := sbPuffer.Wanten;
    fpWoben: ChartPunktX := sbPuffer.Woben;
    fpSalingH: ChartPunktX := sbPuffer.SalingH;
    fpSalingA: ChartPunktX := sbPuffer.SalingA;
    fpSalingL: ChartPunktX := sbPuffer.SalingL;
    fpVorstagOS: ChartPunktX := sbPuffer.Vorstag;
  end;
  i := YComboBox.ItemIndex;
  if i = -1 then
  begin
    i := YComboSavedItemIndex;
    // MessageBeep(MB_ICONQUESTION); { debugging }
  end;
  ChartPunktY := bf[i];
  { Farbe des Punktes }
  PunktColor := GetPunktColor;
  if OutputForm.OutputPages.ActivePage = OutputForm.ChartSheet then
    DrawChartPaintBox(ChartPaintBox.Canvas, ChartPaintBox.BoundsRect);
end;

procedure TRiggModul.LookForYMinMax;
var
  i: Integer;
begin
  if Rigg.CalcTyp = ctQuerKraftBiegung then
  begin
    if (YComboBox.Text = 'Vorstag-Spannung') or
      (YComboBox.Text = 'Wanten-Spannung') then
    begin
      YMax := 5000; { 5000 N }
      YMin := -1000; { -1000 N }
      Exit;
    end;
    if (YComboBox.Text = 'Elastizität Punkt C') then
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

function TRiggModul.GetXText(sbn: TsbName): string;
var
  S: string;
begin
  if sbn = fpController then
    S := 'Zustellung Mast-Controller [mm]'
  else if sbn = fpWinkel then
    S := 'Winkel [Grad]'
  else if (sbn = fpVorstag) or (sbn = fpVorstagOS) then
    S := 'Vorstaglänge [mm]'
  else if sbn = fpWante then
    S := 'Wantenlänge [mm]'
  else if sbn = fpWoben then
    S := 'Länge des oberen Wantenabschnitts [mm]'
  else if sbn = fpSalingH then
    S := 'Höhe des Salingdreiecks [mm]'
  else if sbn = fpSalingA then
    S := 'Saling-Abstand [mm]'
  else if sbn = fpSalingL then
    S := 'Saling-Länge [mm]';
  Result := S;
end;

function TRiggModul.GetYText(Text: string): string;
var
  S: string;
begin
  if Text = 'Wanten-Spannung' then
    S := 'Wantenspannung [N]'
  else if Text = 'Vorstag-Spannung' then
    S := 'Vorstagspannung [N]'
  else if Text = 'Elastizität Punkt C' then
    S := 'Auslenkung Punkt C [mm]'
  else if Text = 'Durchbiegung hd' then
    S := 'Mastbiegung hd [mm]'
  else if Text = 'Mastfall F0F' then
    S := 'Mastfall F0F [mm]'
  else if Text = 'Mastfall F0C' then
    S := 'Mastfall F0C [mm]';
  Result := S;
end;

procedure TRiggModul.SetSBName(Value: TSBName);
begin
  if FSBName <> Value then
  begin
    FSBName := Value;
    KurveValid := False;
  end;
  if CursorSB = SBName then
    ShowTriangle := True
  else
    ShowTriangle := False;
  if OutputForm.OutputPages.ActivePage = OutputForm.ChartSheet then
    DrawChart;
end;

procedure TRiggModul.YComboBoxChange(ItemIndex: Integer);
begin
  (*
  if YComboBox.ItemIndex <> YComboBox2.ItemIndex then begin
    if Sender = YComboBox then YComboBox2.ItemIndex := YComboBox.ItemIndex;
    if Sender = YComboBox2 then YComboBox.ItemIndex := YComboBox2.ItemIndex;
    if (YComboBox.ItemIndex > -1) and (YComboBox.ItemIndex < ANr) then
      f := af[YComboBox.ItemIndex]
    else
      f := TestF;
    DrawChart;
  end;
  *)
  if (ItemIndex > -1) and (ItemIndex < ANr) then
    f := af[ItemIndex]
  else
    f := TestF;
  DrawChart;
end;

procedure TRiggModul.SetKurveValid(Value: Boolean);
begin
  if FKurveValid <> Value then
  begin
    FKurveValid := Value;
    OutputForm.SetKurveValidLED(Value);
  end;
end;

procedure TRiggModul.KurveBtnClick;
var
  cr: TRggSB;
begin
  BottomTitel := GetXText(SBname);
  cr := Rigg.GSB.Find(SBname);
  Xmin := cr.Min;
  Xmax := cr.Max;
  if SBname = fpWinkel then
  begin
    Xmin := Xmin / 10;
    Xmax := Xmax / 10;
  end;
  CursorSB := SBName;
  GetCurves;
end;

{
procedure TRiggModul.ChartFXItemClick;
var
  CF: TChartFXForm;
begin
  CF := TChartFXForm.Create(Self);
  try
    CF.TopTitel := TopTitel;
    CF.LeftTitel := LeftTitel;
    CF.BottomTitel := BottomTitel;
    CF.RightTitel := RightTitel;
    CF.Xmin := Xmin;
    CF.Xmax := Xmax;
    CF.Ymin := Ymin;
    CF.Ymax := Ymax;
    CF.YGap := YGap;
    CF.ChartPunktX := ChartPunktX;
    CF.ChartPunktY := ChartPunktY;
    CF.PunktColor := PunktColor;
    CF.f := f;
    if KurveValid then CF.LEDColor := clLime else CF.LEDColor := clRed;
    CF.ShowModal;
  finally
    CF.Free;
  end;
end;
}

{******************************************************************************}

procedure TRiggModul.SalingPaintBoxClick;
begin
  SalingCtrl.SalingDetail := not SalingCtrl.SalingDetail;
end;

procedure TRiggModul.DrawPaintBoxS(Canvas: TCanvas);
begin
  PaintBackGround(BitmapS);
  with SalingCtrl do
  begin
    if SalingDetail then
      DrawSalingDetail(BitmapS.Canvas)
    else
      DrawSalingAll(BitmapS.Canvas);
  end;
  with Canvas do
  begin
    CopyMode := cmSrcCopy;
    Draw(0, 0, BitMapS);
  end;
end;

procedure TRiggModul.DrawPaintBoxC(Canvas: TCanvas);
begin
  PaintBackGround(BitmapC);
  SalingCtrl.DrawController(BitmapC.Canvas);
  with Canvas do
  begin
    CopyMode := cmSrcCopy;
    Draw(0, 0, BitMapC);
  end;
end;

procedure TRiggModul.OutputPagesChange(Seite: Integer);
var
  TrimmRec: TTrimmControls;
begin
  case Seite of
    1: { Controller }
      begin
        TrimmRec := Rigg.Glieder;
        with SalingCtrl do
        begin
          { ControllerParameter }
          ControllerPos := TrimmRec.Controller;
          ParamXE := Rigg.MastPositionE;
          ParamXE0 := Rigg.iP[ooE0, x] - Rigg.iP[ooD0, x];
          EdgePos := Rigg.GSB.Find(fpController).Min;
        end;
      end;
    3: { Saling }
      begin
        TrimmRec := Rigg.Glieder;
        with SalingCtrl do
        begin
          { SalingParameter }
          SalingA := TrimmRec.SalingA;
          SalingH := TrimmRec.SalingH;
          SalingL := TrimmRec.SalingL;
        end;
      end;
  end;
end;

procedure TRiggModul.ZustellBtnClick;
var
  TrimmRec: TTrimmControls;
begin
  TrimmRec := Rigg.Glieder;
  TrimmRec.Controller := 50;
  Rigg.Glieder := TrimmRec;
  Rigg.UpdateGetriebe;
  GetriebeGrafik.Koordinaten := Rigg.rP;
  Getriebegrafik.SetMastKurve(Rigg.MastLinie, Rigg.lc, Rigg.beta);
  with SalingCtrl do
  begin
    ControllerPos := ParamXE0 - Rigg.MastPositionE;
    TrimmRec.Controller := ControllerPos;
  end;
  UpdateGCtrls(TrimmRec);
  Rigg.Glieder := TrimmRec;
  UpdateGetriebe;
end;

procedure TRiggModul.TestBtnClick;
begin
  Screen.Cursor := crHourGlass;
  Rigg.GetTestKurven;
  if Assigned(KraftPaintBox) then
    Rigg.DrawPaintBoxK(KraftPaintBox.Canvas, KraftPaintBox.BoundsRect);
  Screen.Cursor := crDefault;
end;

procedure TRiggModul.AdjustGrafik;
begin
  ShowAdjustForm(GetriebeGrafik, AdjustGBox);
  if ViewPoint <> GetriebeGrafik.Ansicht then
    { Alles notwendige wird automatisch in SetViewPoint() erledigt. }
    ViewPoint := GetriebeGrafik.Ansicht
  else
  begin
    TextFlipFlop := True;
    ResetPaintBoxG;
    GrafikForm.ViewTab.TabIndex := Ord(FViewPoint);
  end;
end;

procedure TRiggModul.AdjustGBox(Sender: TObject);
begin
  { Koppelkurve }
  if (SalingTyp = stFest) and
    (KoppelBtnDown = True) and
    (GetriebeGrafik.Ansicht = vpSeite) then
    GetriebeGrafik.Koppelkurve := Rigg.Koppelkurve;
  Draw;
end;

procedure TRiggModul.GetGBoxOffset;
begin
  GetriebeGrafik.CalcOffset(GrafikForm.PaintBoxG.ClientRect);
  AdjustGbox(Self);
end;

procedure TRiggModul.DoOnWheelScroll(fp: TFederParam; ScrollPos: Integer);
var
  InputRec: TTrimmControls;
  ls: string;
  t: Integer;
begin
  t := -1;
  Modified := True;
  InputRec := Rigg.Glieder;

  case fp of
    fpController:
    begin
      InputRec.Controller := ScrollPos;
      ls := Format('%d mm', [ScrollPos - MemCtrl.Controller]);
      case SalingTyp of
        stFest:
        begin
          InputForm.sbController.Position := ScrollPos;
          t := InputForm.sbController.Tag;
          InputForm.lbValue1.Caption := ls;
        end;
        stDrehbar:
        begin
          InputForm.sbControllerD.Position := ScrollPos;
          t := InputForm.sbControllerD.Tag;
          InputForm.lbD1.Caption := ls;
        end;
        stOhne:
        begin
          InputForm.lbOhne1.Caption := ls;
        end;
      end;
      if not ControllerBtnDown then
        NeedPaint := False;
    end;

    fpVorstag, fpWinkel:
    begin
      ls := Format('%d mm', [ScrollPos - MemCtrl.Vorstag]);
      case SalingTyp of
        stFest:
        begin
          if WinkelBtnDown then
          begin
            InputRec.Winkel := ScrollPos;
            InputForm.sbWinkel.Position := ScrollPos;
            t := InputForm.sbWinkel.Tag;
            InputForm.lbValue2.Caption := Format('%5.2f Grad', [(ScrollPos - MemCtrl.Winkel) / 10]);
          end
          else
          begin
            InputRec.Vorstag := ScrollPos;
            InputForm.sbWinkel.Position := ScrollPos;
            t := InputForm.sbWinkel.Tag;
            InputForm.lbValue2.Caption := ls;
          end;
        end;
        stDrehbar:
        begin
          InputRec.Vorstag := ScrollPos;
          InputForm.sbVorstagD.Position := ScrollPos;
          t := InputForm.sbVorstagD.Tag;
          InputForm.lbD2.Caption := ls;
        end;
        stOhne:
        begin
          InputRec.Vorstag := ScrollPos;
          InputForm.sbVorstagOhne.Position := ScrollPos;
          t := InputForm.sbVorstagOhne.Tag;
          InputForm.lbOhne1.Caption := ls;
        end;
        stOhne_2:
        begin
          InputForm.sbVorstagOS.Position := ScrollPos;
          t := InputForm.sbVorstagOS.Tag;
          InputForm.lbOhne2.Caption := ls;
        end;
      end;
    end;

    fpWante:
    begin
      InputRec.Wanten := ScrollPos;
      ls := Format('%d mm', [ScrollPos - MemCtrl.Wanten]);
      case SalingTyp of
        stFest:
        begin
          InputForm.sbWante.Position := ScrollPos;
          t := InputForm.sbWante.Tag;
          InputForm.lbValue3.Caption := ls;
        end;
        stDrehbar:
        begin
          InputForm.sbWanteD.Position := ScrollPos;
          t := InputForm.sbWanteD.Tag;
          InputForm.lbD3.Caption := ls;
        end;
        stOhne:
        begin
          InputForm.sbWanteOhne.Position := ScrollPos;
          t := InputForm.sbWanteOhne.Tag;
          InputForm.lbOhne3.Caption := ls;
        end;
      end;
    end;

    fpWoben:
    begin
      ls := Format('%d mm', [ScrollPos - MemCtrl.Woben]);
      case SalingTyp of
        stFest:
        begin
          InputRec.Woben := ScrollPos;
          InputForm.sbWoben.Position := ScrollPos;
          t := InputForm.sbWoben.Tag;
          InputForm.lbValue4.Caption := ls;
        end;
        stDrehbar:
        begin
          InputRec.Woben := ScrollPos;
          InputForm.sbWobenD.Position := ScrollPos;
          t := InputForm.sbWobenD.Tag;
          InputForm.lbD4.Caption := ls;
        end;
      end;
    end;

    fpSalingH:
    begin
      ls := Format('%d mm', [ScrollPos - MemCtrl.SalingH]);
      case SalingTyp of
        stFest:
        begin
          InputRec.SalingH := ScrollPos;
          InputForm.sbSalingH.Position := ScrollPos;
          t := InputForm.sbSalingH.Tag;
          InputForm.lbValue5.Caption := ls;
        end;
      end;
    end;

    fpSalingA:
    begin
      ls := Format('%d mm', [ScrollPos - MemCtrl.SalingA]);
      InputRec.SalingA := ScrollPos;
      InputForm.sbSalingA.Position := ScrollPos;
      t := InputForm.sbSalingA.Tag;
      InputForm.lbValue6.Caption := ls;
    end;

    fpSalingL:
    begin
      ls := Format('%d mm', [ScrollPos - MemCtrl.SalingL]);
      InputRec.SalingL := ScrollPos;
      InputForm.sbSalingLD.Position := ScrollPos;
      t := InputForm.sbSalingLD.Tag;
      InputForm.lbD5.Caption := ls;
    end;

  end;

//  with InputRec, InputForm do
//  begin
//    else if Sender = sbVorstagOs then
//    begin
//      Vorstag := ScrollPos;
//      lbValue7.Caption := Format('%d mm', [ScrollPos - MemCtrl.Vorstag])
//    end
//    else if Sender = sbWPowerOS then
//    begin
//      WPowerOS := ScrollPos;
//      lbValue8.Caption := Format('%d N', [ScrollPos - MemCtrl.WPowerOS]);
//      NeedPaint := False;
//    end;
//  end;

  //if not SofortBerechnen then
  begin
    if t = SBMappingArray[CursorSB] then
      ShowTriangle := True
    else
    begin
      ShowTriangle := False;
      KurveValid := False;
    end;
    sbPuffer := InputRec;
    Rigg.Glieder := InputRec;
    UpdateGetriebe;
    AllreadyUpdatedGetriebeFlag := True;
  end;
end;

end.

