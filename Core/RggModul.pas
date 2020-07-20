unit RggModul;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Classes,
  System.Types,
  System.UITypes,
  System.Math,
  Vcl.Graphics,
  Vcl.Forms,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.Tabs,
  RggTypes,
  RggCalc,
  RggUnit4,
  RggRota,
  RggKraftGraph,
  RggMastGraph,
  RggReport,
  RggCtrls,
  RggDoc,
  RiggVar.FB.ActionConst,
  RiggVar.RG.Def,
  RiggVar.VM.FormMain;

type
  TSBMappingArray = array[TsbName] of Integer;

const
  ANr = 6;

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

  TRiggModulBase = class
  protected
    FControllerBtnDown: Boolean;
    FWinkelBtnDown: Boolean;
    FDiffBtnDown: Boolean;
    FCalcTyp: TCalcTyp;
    FViewPoint: TViewPoint;
    FLEDShape: Boolean;
    FSalingTyp: TSalingTyp;
    FControllerTyp: TControllerTyp;
    FBackgroundColor: TColor;
    FSofortBtnDown: Boolean;
    FGrauZeichnen: Boolean;
    FNeedPaint: Boolean;
    procedure PaintBackGround(Image: TBitmap);
    procedure UpdateGetriebe;
  public
    Modified: Boolean;
    AutoSave: Boolean;
    ViewModelM: TViewModelMain00;
    Rigg: TRigg;
    InputBuffer: TTrimmControls;
    MemCtrl: TTrimmControls;
    RefCtrl: TTrimmControls;
    RefPoints: TRealRiggPoints;
    constructor Create;
    property SofortBerechnen: Boolean read FSofortBtnDown;
  end;

  TRiggModulInput = class(TRiggModulBase)
  private
    function MatchesCurrentlyShown(SB: TScrollBar): Boolean;
    function TagForSBName(Value: TSBName): Integer;
  protected
    FShowTriangle: Boolean;
    FActualX: TsbName;
    FKurveValid: Boolean;

    MastGraph: TMastGraph;

    procedure OriginalHandleScroll(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);

    procedure SetupGCtrl(a: TScrollBar; b: TsbName);
    procedure SetupGCtrls;
    procedure UpdateGCtrls(InputRec: TTrimmControls);
    procedure UpdateGCtrlLabels(InputRec: TTrimmControls);
  public
    constructor Create;
    destructor Destroy; override;
    procedure HandleScroll(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
    procedure UpdateGControls;
  end;

  TRiggModulKraft = class(TRiggModulInput)
  protected
    KraftPaintBox: TImage;
    KraftGraph: TKraftGraph;
  public
    constructor Create;
    destructor Destroy; override;
    procedure UpdateKraftGraphBtnClick;
  end;

  TRiggModulGetriebe = class(TRiggModulKraft)
  public
    SalingGraph: TSalingGraph;
    BitmapS: TBitmap;
    BitmapC: TBitmap;
    ControllerPaintBox: TPaintBox;
    SalingPaintBox: TPaintBox;

    lbMastfall: string;
    lbSpannung: string;
    lbBiegung: string;

    constructor Create;
    destructor Destroy; override;

    procedure ControllerZustellenBtnClick;
    procedure OutputPagesChange(Seite: Integer);
    procedure SalingPaintBoxClick;

    procedure DoOnUpdateSalingTyp(Value: TSalingTyp);
    procedure DrawPaintBoxM;
    procedure DrawPaintBoxS(Canvas: TCanvas);
    procedure DrawPaintBoxC(Canvas: TCanvas);
  end;

  TRiggModulReport = class(TRiggModulGetriebe)
  protected
    FReportItem: TReportItem;
    procedure SetReportItem(Value: TReportItem);
  public
    RiggReport: TRiggReport;
    FWReport: TFWReport;

    constructor Create;
    destructor Destroy; override;

    procedure AusgabeText;
    procedure AusgabeKommentar;
    procedure rLItemClick(Item: TReportItem);
    procedure WriteReportToMemo(Memo: TMemo);
    property ReportItem: TReportItem read FReportItem write SetReportItem;
  end;

  TRiggModulDiagram = class(TRiggModulReport)
  private
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
  protected
    FIntendedX: TsbName;
    FChartValid: Boolean;
    procedure SetKurveValid(Value: Boolean);
    procedure SetIntendedX(Value: TSBName);
  public
    YComboBox: TComboBox;
    YComboSavedItemIndex: Integer;

    ChartPaintBox: TPaintBox;

    constructor Create;
    destructor Destroy; override;

    procedure DoUpdateChartBuffer;
    procedure DrawPoint;
    procedure DrawChartPaintBox(Canvas: TCanvas; Rect: TRect);
    procedure YComboBoxChange(ItemIndex: Integer);
    procedure KurveBtnClick;

    procedure StraightLine;
    procedure GetCurves;
    procedure UpdateGetriebePunkt;
    procedure UpdateRiggPunkt;
    procedure DrawChart;
    procedure LookForYMinMax;
    function GetXText(sbn: TsbName): string;
    function GetYText(Text: string): string;
    function GetPunktColor: TColor;

    property KurveValid: Boolean read FKurveValid write SetKurveValid;
    property IntendedX: TSBName read FIntendedX write SetIntendedX;
    property ActualX: TSBName read FActualX write FActualX;
  end;

  TRiggModul = class(TRiggModulDiagram)
  protected
    function GetControllerEnabled: Boolean;
    procedure SetControllerBtnDown(Value: Boolean);
    procedure SetDiffBtnDown(Value: Boolean);
    procedure SetSofortBtnDown(Value: Boolean);
    procedure SetCalcTyp(Value: TCalcTyp);
    procedure SetLEDShape(Value: Boolean);
    procedure SetSalingTyp(Value: TSalingTyp);
    procedure SetControllerTyp(Value: TControllerTyp);
    procedure SetWinkelBtnDown(Value: Boolean);
  public
    constructor Create;
    destructor Destroy; override;

    procedure DoGraphics;

    procedure DoOnWheelScroll(fp: TFederParam; ScrollPos: Integer);
    procedure DoResetForTrimmData;

    procedure UpdateUI;
    procedure UpdateBtnClick;
    procedure BiegeNeigeItemClick;
    procedure ReglerBtnClick;
    procedure FestItemClick;
    procedure OSSItemClick;
    procedure OSBItemClick;
    procedure DrehbarItemClick;

    property ControllerBtnDown: Boolean read FControllerBtnDown write SetControllerBtnDown;
    property WinkelBtnDown: Boolean read FWinkelBtnDown write SetWinkelBtnDown;
    property DiffBtnDown: Boolean read FDiffBtnDown write SetDiffBtnDown;
    property SofortBtnDown: Boolean read FSofortBtnDown write SetSofortBtnDown;
    property CalcTyp: TCalcTyp read FCalcTyp write SetCalcTyp;
    property ControllerEnabled: Boolean read GetControllerEnabled;
    property LEDShape: Boolean read FLEDShape write SetLEDShape;
    property SalingTyp: TSalingTyp read FSalingTyp write SetSalingTyp;
    property ControllerTyp: TControllerTyp read FControllerTyp write SetControllerTyp;
    property GrauZeichnen: Boolean read FGrauZeichnen;
  end;

  TRiggModulA = class(TRiggModul)
  public
    constructor Create(ARigg: TRigg);
    destructor Destroy; override;
    procedure Init;
    procedure DoOnUpdateRigg;
  end;

var
  RiggModul: TRiggModulA;

implementation

uses
  RiggVar.App.Main,
  RggFachwerk,
  RggScroll,
  RggMatrix,
  FrmInput,
  FrmOutput,
  FrmSelect,
  FrmRegler,
  FrmReglerGraph,
  FrmBiege,
  FrmKreis;

procedure TRiggModulInput.SetupGCtrl(a: TScrollBar; b: TsbName);
var
  cr: TRggSB;
begin
  cr := Rigg.GSB.Find(b);
  a.SetParams(Round(cr.Ist), Round(cr.Min), Round(cr.Max));
  a.LargeChange := Round(cr.BigStep);
  a.SmallChange := Round(cr.SmallStep);
end;

procedure TRiggModulInput.SetupGCtrls;
begin
  { Controller }
  SetupGCtrl(InputForm.sbController, fpController);
  SetupGCtrl(InputForm.sbControllerD, fpController);
  SetupGCtrl(InputForm.sbControllerOhne, fpController);

  { Vorstag/Winkel }
  if FWinkelBtnDown then
    SetupGCtrl(InputForm.sbWinkel, fpWinkel)
  else
    SetupGCtrl(InputForm.sbWinkel, fpVorstag);
  SetupGCtrl(InputForm.sbVorstagD, fpVorstag);
  SetupGCtrl(InputForm.sbVorstagOhne, fpVorstag);

  { Wante }
  SetupGCtrl(InputForm.sbWante, fpWante);
  SetupGCtrl(InputForm.sbWanteD, fpWante);
  SetupGCtrl(InputForm.sbWanteOhne, fpWante);

  { Woben }
  SetupGCtrl(InputForm.sbWoben, fpWoben);
  SetupGCtrl(InputForm.sbWobenD, fpWoben);

  { Saling }
  SetupGCtrl(InputForm.sbSalingH, fpSalingH);
  SetupGCtrl(InputForm.sbSalingA, fpSalingA);
  SetupGCtrl(InputForm.sbSalingLD, fpSalingL);

  { Ohne Saling starr }
  SetupGCtrl(InputForm.sbVorstagOS, fpVorstagOS);
  InputForm.sbVorstagOS.Position := Round(Rigg.GSB.Find(fpVorstag).Ist);
  SetupGCtrl(InputForm.sbWPowerOS, fpWPowerOS);
  UpdateGCtrlLabels(Rigg.Glieder);
end;

procedure TRiggModulInput.UpdateGControls;
begin
  UpdateGCtrls(Rigg.Glieder);
end;

procedure TRiggModulInput.UpdateGCtrlLabels(InputRec: TTrimmControls);
begin
  InputForm.lbValue1.Caption := Format('%d mm', [InputRec.Controller - MemCtrl.Controller]);
  if FWinkelBtnDown then
  begin
    InputForm.lbWinkel.Caption := 'Winkel';
    InputForm.lbValue2.Caption := Format('%5.2f Grad', [(InputRec.Winkel - MemCtrl.Winkel) / 10]);
  end
  else
  begin
    InputForm.lbWinkel.Caption := 'Vorstag';
    InputForm.lbValue2.Caption := Format('%d mm', [InputRec.Vorstag - MemCtrl.Vorstag]);
  end;
  InputForm.lbValue3.Caption := Format('%d mm', [InputRec.Wanten - MemCtrl.Wanten]);
  InputForm.lbValue4.Caption := Format('%d mm', [InputRec.Woben - MemCtrl.Woben]);
  InputForm.lbValue5.Caption := Format('%d mm', [InputRec.SalingH - MemCtrl.SalingH]);
  InputForm.lbValue6.Caption := Format('%d mm', [InputRec.SalingA - MemCtrl.SalingA]);
  InputForm.lbD5.Caption := Format('%d mm', [InputRec.SalingL - MemCtrl.SalingL]);
  InputForm.lbValue7.Caption := Format('%d mm', [InputRec.Vorstag - MemCtrl.Vorstag]);
  InputForm.lbValue8.Caption := Format('%d mm', [InputRec.WPowerOS - MemCtrl.WPowerOS]);

  InputForm.lbD1.Caption := InputForm.lbValue1.Caption;
  InputForm.lbD2.Caption := InputForm.lbValue2.Caption;
  InputForm.lbD3.Caption := InputForm.lbValue3.Caption;
  InputForm.lbD4.Caption := InputForm.lbValue4.Caption;
//InputForm.lbD5.Caption := InputForm.lbValue5.Caption; { SalingL: see above }

  InputForm.lbOhne1.Caption := InputForm.lbValue1.Caption;
  InputForm.lbOhne2.Caption := InputForm.lbValue2.Caption;
  InputForm.lbOhne3.Caption := InputForm.lbValue3.Caption;
end;

procedure TRiggModulInput.UpdateGCtrls(InputRec: TTrimmControls);
begin
  InputBuffer := InputRec;

  InputForm.sbController.Position := InputRec.Controller;
  InputForm.sbControllerD.Position := InputRec.Controller;
  InputForm.sbControllerOhne.Position := InputRec.Controller;

  if FWinkelBtnDown then
    InputForm.sbWinkel.Position := InputRec.Winkel
  else
    InputForm.sbWinkel.Position := InputRec.Vorstag;
  InputForm.sbVorstagD.Position := InputRec.Vorstag;
  InputForm.sbVorstagOhne.Position := InputRec.Vorstag;

  InputForm.sbWante.Position := InputRec.Wanten;
  InputForm.sbWanteD.Position := InputRec.Wanten;
  InputForm.sbWanteOhne.Position := InputRec.Wanten;

  InputForm.sbWoben.Position := InputRec.Woben;
  InputForm.sbWobenD.Position := InputRec.Woben;

  InputForm.sbSalingH.Position := InputRec.SalingH;
  InputForm.sbSalingA.Position := InputRec.SalingA;
  InputForm.sbSalingLD.Position := InputRec.SalingL;

  InputForm.sbVorstagOS.Position := InputRec.Vorstag;
  InputForm.sbWPowerOS.Position := InputRec.WPowerOS;

  UpdateGCtrlLabels(InputRec);
end;

procedure TRiggModulInput.HandleScroll(Sender: TObject;
  ScrollCode: TScrollCode; var ScrollPos: Integer);
var
  fp: TFederParam;
begin
  fp := fpVorstag;

  if Sender = InputForm.sbController then
  begin
    fp := fpController;
  end
  else if Sender = InputForm.sbControllerD then
  begin
    fp := fpController;
  end
  else if Sender = InputForm.sbControllerOhne then
  begin
    fp := fpController;
  end
  else if Sender = InputForm.sbWinkel then
  begin
    if FWinkelBtnDown then
    begin
      fp := fpWinkel;
    end
    else
    begin
      fp := fpVorstag;
    end
  end
  else if Sender = InputForm.sbVorstagD then
  begin
    fp := fpVorstag;
  end
  else if Sender = InputForm.sbVorstagOhne then
  begin
    fp := fpVorstag;
  end
  else if Sender = InputForm.sbWante then
  begin
    fp := fpWante;
  end
  else if Sender = InputForm.sbWanteD then
  begin
    fp := fpWante;
  end
  else if Sender = InputForm.sbWanteOhne then
  begin
    fp := fpWante;
  end
  else if Sender = InputForm.sbWoben then
  begin
    fp := fpWoben;
  end
  else if Sender = InputForm.sbWobenD then
  begin
    fp := fpWoben;
  end
  else if Sender = InputForm.sbSalingH then
  begin
    fp := fpSalingH;
  end
  else if Sender = InputForm.sbSalingA then
  begin
    fp := fpSalingA;
  end
  else if Sender = InputForm.sbSalingLD then
  begin
    fp := fpSalingL;
  end
  else if Sender = InputForm.sbVorstagOS then
  begin
    fp := fpVorstagOS;
  end
  else if Sender = InputForm.sbWPowerOS then
  begin
    fp := fpWPowerOS;
  end;

  RiggModul.DoOnWheelScroll(fp, ScrollPos);
end;

procedure TRiggModulInput.OriginalHandleScroll(Sender: TObject;
  ScrollCode: TScrollCode; var ScrollPos: Integer);
var
  InputRec: TTrimmControls;
//  fp: TFederParam;
begin
  Modified := True;
  InputRec := Rigg.Glieder;

  if Sender = InputForm.sbController then
  begin
//    fp := fpController;
    InputRec.Controller := ScrollPos;
    InputForm.lbValue1.Caption := Format('%d mm', [ScrollPos - MemCtrl.Controller]);
    if not FControllerBtnDown then
      FNeedPaint := False;
  end
  else if Sender = InputForm.sbControllerD then
  begin
//    fp := fpController;
    InputRec.Controller := ScrollPos;
    InputForm.lbD1.Caption := Format('%d mm', [ScrollPos - MemCtrl.Controller]);
    if not FControllerBtnDown then
      FNeedPaint := False;
  end
  else if Sender = InputForm.sbControllerOhne then
  begin
//    fp := fpController;
    InputRec.Controller := ScrollPos;
    InputForm.lbOhne1.Caption := Format('%d mm', [ScrollPos - MemCtrl.Controller]);
    if not FControllerBtnDown then
      FNeedPaint := False;
  end
  else if Sender = InputForm.sbWinkel then
  begin
    if FWinkelBtnDown then
    begin
//      fp := fpWinkel;
      InputRec.Winkel := ScrollPos;
      InputForm.lbValue2.Caption := Format('%5.2f Grad', [(InputRec.Winkel - MemCtrl.Winkel) / 10]);
    end
    else
    begin
//      fp := fpVorstag;
      InputRec.Vorstag := ScrollPos;
      InputForm.lbValue2.Caption := Format('%d mm', [InputRec.Vorstag - MemCtrl.Vorstag]);
    end
  end
  else if Sender = InputForm.sbVorstagD then
  begin
//    fp := fpVorstag;
    InputRec.Vorstag := ScrollPos;
    InputForm.lbD2.Caption := Format('%d mm', [InputRec.Vorstag - MemCtrl.Vorstag]);
  end
  else if Sender = InputForm.sbVorstagOhne then
  begin
//    fp := fpVorstag;
    InputRec.Vorstag := ScrollPos;
    InputForm.lbOhne2.Caption := Format('%d mm', [InputRec.Vorstag - MemCtrl.Vorstag]);
  end
  else if Sender = InputForm.sbWante then
  begin
//    fp := fpWante;
    InputRec.Wanten := ScrollPos;
    InputForm.lbValue3.Caption := Format('%d mm', [ScrollPos - MemCtrl.Wanten]);
  end
  else if Sender = InputForm.sbWanteD then
  begin
//    fp := fpWante;
    InputRec.Wanten := ScrollPos;
    InputForm.lbD3.Caption := Format('%d mm', [ScrollPos - MemCtrl.Wanten]);
  end
  else if Sender = InputForm.sbWanteOhne then
  begin
//    fp := fpWante;
    InputRec.Wanten := ScrollPos;
    InputForm.lbOhne3.Caption := Format('%d mm', [ScrollPos - MemCtrl.Wanten]);
  end
  else if Sender = InputForm.sbWoben then
  begin
//    fp := fpWoben;
    InputRec.Woben := ScrollPos;
    InputForm.lbValue4.Caption := Format('%d mm', [ScrollPos - MemCtrl.Woben]);
  end
  else if Sender = InputForm.sbWobenD then
  begin
//    fp := fpWoben;
    InputRec.Woben := ScrollPos;
    InputForm.lbD4.Caption := Format('%d mm', [ScrollPos - MemCtrl.Woben]);
  end
  else if Sender = InputForm.sbSalingH then
  begin
//    fp := fpSalingH;
    InputRec.SalingH := ScrollPos;
    InputForm.lbValue5.Caption := Format('%d mm', [ScrollPos - MemCtrl.SalingH]);
  end
  else if Sender = InputForm.sbSalingA then
  begin
//    fp := fpSalingA;
    InputRec.SalingA := ScrollPos;
    InputForm.lbValue6.Caption := Format('%d mm', [ScrollPos - MemCtrl.SalingA]);
  end
  else if Sender = InputForm.sbSalingLD then
  begin
//    fp := fpSalingL;
    InputRec.SalingL := ScrollPos;
    InputForm.lbD5.Caption := Format('%d mm', [ScrollPos - MemCtrl.SalingL]);
  end
  else if Sender = InputForm.sbVorstagOS then
  begin
//    fp := fpVorstagOS;
    InputRec.Vorstag := ScrollPos;
    InputForm.lbValue7.Caption := Format('%d mm', [ScrollPos - MemCtrl.Vorstag])
  end
  else if Sender = InputForm.sbWPowerOS then
  begin
//    fp := fpWPowerOS;
    InputRec.WPowerOS := ScrollPos;
    InputForm.lbValue8.Caption := Format('%d N', [ScrollPos - MemCtrl.WPowerOS]);
    FNeedPaint := False;
  end;

  if (ScrollCode = TScrollCode.scEndScroll) or not SofortBerechnen then
  begin
    if MatchesCurrentlyShown(Sender as TScrollbar) then
      FShowTriangle := True
    else
    begin
      FShowTriangle := False;
      FKurveValid := False; // <-- should be assignment to property with setter called
    end;
    InputBuffer := InputRec;
    Rigg.Glieder := InputRec;
    UpdateGetriebe;
  end;
end;

function TRiggModulInput.MatchesCurrentlyShown(SB: TScrollBar): Boolean;
begin
  result := SB.Tag = TagForSBName(FActualX);
end;

function TRiggModulInput.TagForSBName(Value: TSBName): Integer;
begin
  { SBMappingArray: TSBMappingArray = (0, 1, 1, 2, 3, 4, 5, 5, 6, 7); }
  case Value of
    fpController: result := 0;
    fpWinkel: result := 1;
    fpVorstag: result := 1;
    fpWante: result := 2;
    fpWoben: result := 3;
    fpSalingH: result := 4;
    fpSalingA: result := 5;
    fpSalingL: result := 5;
    fpVorstagOS: result := 6;
    fpWPowerOS: result := 7;
    else result := 1;
  end;
end;

procedure TRiggModul.DoGraphics;
var
  TrimmRec: TTrimmControls;
begin
  { ControllerPaintBox }
  if OutputForm.OutputPages.ActivePage = OutputForm.ControllerSheet then
  begin
    SalingGraph.ControllerTyp := Rigg.ControllerTyp;
    TrimmRec := Rigg.Glieder;
    { Abstand(iP[ooE0,x], iP[ooE,x]) in mm}
    SalingGraph.ControllerPos := TrimmRec.Controller;
    { Position des Mastes in Deckshöhe von D0 aus in mm }
    SalingGraph.ParamXE := Round(Rigg.MastPositionE);
    { Abstand(iP[ooD0,x], iP[ooE0,x]) in mm }
    SalingGraph.ParamXE0 := Round(Rigg.rP[ooE0, x] - Rigg.rP[ooD0, x]);
    { Abstand von E0 zur Anschlagkante Deck + Klotzdicke }
    SalingGraph.EdgePos := Round(Rigg.GSB.Find(fpController).Min);
    if Assigned(ControllerPaintBox) then
      DrawPaintBoxC(ControllerPaintBox.Canvas);
  end;

  { SalingPaintBox }
  if OutputForm.OutputPages.ActivePage = OutputForm.SalingSheet then
  begin
    TrimmRec := Rigg.Glieder;
    { SalingAbstand }
    SalingGraph.SalingA := TrimmRec.SalingA;
    { Abstand Verbindungslinie Salinge zu Hinterkante Mast in mm }
    SalingGraph.SalingH := TrimmRec.SalingH;
    { Salinglänge in mm - außerhalb berechnen }
    SalingGraph.SalingL := TrimmRec.SalingL;
    if Assigned(SalingPaintBox) then
      DrawPaintBoxS(SalingPaintBox.Canvas);
  end;

  { Diagramm aktuellen Punkt setzen }
  if OutputForm.OutputPages.ActivePage = OutputForm.ChartSheet then
  begin
    UpdateGetriebePunkt;
  end;

  { Grafik aktualisieren }
  if not (SofortBerechnen and Rigg.GetriebeOK and Rigg.MastOK) then
  begin
    FGrauZeichnen := False;
    if Rigg.GetriebeOK then
      LEDShape := True
    else
      LEDShape := False;
    ViewModelM.StatusPanelText1 := Rigg.GetriebeStatusText;
    DrawPaintBoxM;

    if Rigg.GetriebeOK and not Rigg.MastOK then
    begin
      LEDShape := False;
      ViewModelM.StatusPanelText1 := Rigg.MastStatusText;
    end;
  end;
  ViewModelM.UpdateView;
end;

procedure TRiggModulBase.PaintBackGround(Image: TBitmap);
var
  R: TRect;
begin
  if Image = nil then
    Exit;

  R := Rect(0, 0, Image.Width, Image.Height);
  Image.Canvas.Brush.Color := FBackgroundColor;
  Image.Canvas.FillRect(R);
end;

procedure TRiggModulGetriebe.DrawPaintBoxM;
var
  img: TImage;
begin
  case InputForm.InputPages.ActivePage.Tag of
    0: img := InputForm.PaintBoxM;
    1: img := InputForm.PaintBoxMD;
    2: img := InputForm.PaintBoxMOhne;
  else
    Exit;
  end;
  Rigg.UpdateMastGraph(MastGraph);
  MastGraph.Image := img;
  MastGraph.Draw;
end;

procedure TRiggModulReport.AusgabeText;
var
  MemoPosY: LongInt;
  ML: TStrings;
begin
  MemoPosY := SendMessage(OutputForm.DisplayMemo.Handle, EM_GETFIRSTVISIBLELINE, 0, 0);
  ML := OutputForm.DisplayMemo.Lines;
  ML.BeginUpdate;
  ML.Clear;

  { Text for GetriebeGraph }
  lbMastFall := Format('Mastfall = %5.1f cm', [Rigg.Trimm.Mastfall / 10]);
  lbSpannung := Format('Spannung = %5.0f N', [Rigg.rF[14]]);
  lbBiegung := Format('Biegung  = %5.1f cm', [Rigg.hd / 10]);

  Rigg.AusgabeText(ML);

  SendMessage(OutputForm.DisplayMemo.Handle, EM_LINESCROLL, 0, MemoPosY);
  ML.EndUpdate;
end;

procedure TRiggModulReport.AusgabeKommentar;
var
  ML: TStrings;
begin
  ML := OutputForm.KommentarMemo.Lines;
  ML.BeginUpdate;
  ML.Clear;

  Rigg.AusgabeKommentar(ML);

  ML.EndUpdate;
end;

procedure TRiggModulReport.SetReportItem(Value: TReportItem);
begin
  if FReportItem <> Value then
  begin
    FReportItem := Value;
    rLItemClick(Value);
    OutputForm.OutputPages.ActivePage := OutputForm.MasterMemo;
  end;

  if not OutputForm.Visible then
    OutputForm.Visible := True;
end;

procedure TRiggModulReport.rLItemClick(Item: TReportItem);
var
  ML: TStrings;
begin
  RiggReport.ML.Clear;
  RiggReport.ML.Add('');

  if Item = rL_Item then
    RiggReport.AusgabeRL(Rigg.rL)
  else if Item = rLe_Item then
    RiggReport.AusgabeRLE(Rigg.rLe)
  else if Item = rP_Item then
    RiggReport.AusgabeRP(Rigg.rP)
  else if Item = rPe_Item then
    RiggReport.AusgabeRPE(Rigg.rPe)
  else if Item = rF_Item then
    RiggReport.AusgabeRF(Rigg.rF)
  else if Item = DiffL_Item then
    RiggReport.AusgabeDiffL(Rigg.rL, Rigg.rLe)
  else if Item = DiffP_Item then
    RiggReport.AusgabeDiffP(Rigg.rP, Rigg.rPe)
  else if Item = Log_Item then
    RiggReport.AusgabeLog(Rigg.LogList);

  ML := RiggReport.ML;
  ML.Add(' Angezeigt werden die zuletzt gültigen Werte.');
  ML.Add('');
  ML.Add(' Die Tabellenwerte sind aktuell und gültig, wenn');
  ML.Add(' - die LED Grün ist und');
  ML.Add(' - die Taste "=" gedrückt wurde bzw.');
  ML.Add(' - der Schalter "A" gedrückt ist.');
  ML.Add('');
  ML.Add(' Die Tabellenwerte können ungültig sein, wenn');
  ML.Add(' - die LED Rot ist und/oder');
  ML.Add(' - die Taste "=" nicht gedrückt wurde bzw.');
  ML.Add(' - der Schalter "A" nicht gedrückt ist');

  OutputForm.Memo.Lines.BeginUpdate;
  try
    OutputForm.Memo.Clear;
    OutputForm.Memo.Lines := RiggReport.ML;
    OutputForm.Memo.SelStart := 0;
  finally
    OutputForm.Memo.Lines.EndUpdate;
  end;
  RiggReport.ML.Clear;
end;

procedure TRiggModulReport.WriteReportToMemo(Memo: TMemo);
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
  for i := 0 to MemoDlg.DstList.Items.Count - 1 do
  begin
    if MemoDlg.DstList.Items[i] = 'rP' then
      RiggReport.AusgabeRP(Rigg.rP);
    if MemoDlg.DstList.Items[i] = 'rPe' then
      RiggReport.AusgabeRPE(Rigg.rPe);
    if MemoDlg.DstList.Items[i] = 'DiffP' then
      RiggReport.AusgabeDiffP(Rigg.rP, Rigg.rPe);
    if MemoDlg.DstList.Items[i] = 'rL' then
      RiggReport.AusgabeRL(Rigg.rL);
    if MemoDlg.DstList.Items[i] = 'rLe' then
      RiggReport.AusgabeRLE(Rigg.rLe);
    if MemoDlg.DstList.Items[i] = 'DiffL' then
      RiggReport.AusgabeDiffL(Rigg.rL, Rigg.rLe);
    if MemoDlg.DstList.Items[i] = 'rF' then
      RiggReport.AusgabeRF(Rigg.rF);
    if MemoDlg.DstList.Items[i] = 'Winkel' then
      RiggReport.AusgabeWinkel(
        Rigg.alpha,
        Rigg.alpha1,
        Rigg.alpha2,
        Rigg.beta,
        Rigg.gamma,
        Rigg.delta1,
        Rigg.delta2,
        Rigg.epsilon,
        Rigg.phi,
        Rigg.psi);
    if MemoDlg.DstList.Items[i] = 'TrimmControls' then
      RiggReport.AusgabeTrimmControls(Rigg.Glieder);
    if MemoDlg.DstList.Items[i] = 'SalingDaten' then
      RiggReport.AusgabeSalingDaten(Rigg.SalingDaten);
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
  for i := 0 to MemoDlg.DstList.Items.Count - 1 do
  begin
    { output selected reports }
    if MemoDlg.DstList.Items[i] = 'FW_Geometrie' then
      FWReport.AusgabeGeometrie(Rigg.Fachwerk.G, Rigg.Fachwerk.S);
    if MemoDlg.DstList.Items[i] = 'FW_StabQuerschnitte' then
      FWReport.AusgabeStabQuerschnitte(Rigg.Fachwerk.vektorEA, Rigg.Fachwerk.S);
    if MemoDlg.DstList.Items[i] = 'FW_Elastizitaeten' then
      FWReport.AusgabeElastizitaeten(Rigg.Fachwerk.Q, Rigg.Fachwerk.S);
    if MemoDlg.DstList.Items[i] = 'FW_Koordinaten' then
      FWReport.AusgabeKoordinaten(Rigg.Fachwerk.KX, Rigg.Fachwerk.KY, Rigg.Fachwerk.K);
    if MemoDlg.DstList.Items[i] = 'FW_Belastung' then
      FWReport.AusgabeBelastung(Rigg.Fachwerk.FXsaved, Rigg.Fachwerk.FYsaved, Rigg.Fachwerk.K);
    if MemoDlg.DstList.Items[i] = 'FW_Auflagerkraefte' then
      FWReport.AusgabeAuflagerkraefte(Rigg.Fachwerk.Lager);
    if MemoDlg.DstList.Items[i] = 'FW_Stabkraefte' then
      FWReport.AusgabeStabkraefte(Rigg.Fachwerk.FS, Rigg.Fachwerk.S);
    if MemoDlg.DstList.Items[i] = 'FW_Verschiebungen' then
      FWReport.AusgabeVerschiebungen(
        Rigg.Fachwerk.FO1,
        Rigg.Fachwerk.FO2,
        Rigg.Fachwerk.FO,
        PO1,
        PO2,
        Rigg.Fachwerk.K);
  end;
  Memo.Lines.AddStrings(FWReport.ML);
  FWReport.ML.Clear;

  Memo.SelStart := 0;
end;

procedure TRiggModul.UpdateUI;
begin
  Modified := False;

  SalingTyp := Rigg.SalingTyp;
  ControllerTyp := Rigg.ControllerTyp;
  CalcTyp := Rigg.CalcTyp;
  FControllerBtnDown := FControllerTyp <> ctOhne;

  ViewModelM.ControllerEnabled := ControllerEnabled;
  ViewModelM.ControllerDown := ControllerBtnDown;

  ViewModelM.UpdateView;
end;

function TRiggModul.GetControllerEnabled: Boolean;
begin
  Result := True;
  if CalcTyp = ctKraftGemessen then
    Result := False;
  if SalingTyp = stOhneStarr then
    Result := False;
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
  if FCalcTyp <> Value then
  begin
    FCalcTyp := Value;
    Rigg.CalcTyp := Value;

    if Value = ctKraftGemessen then
      ControllerBtnDown := False;

    if Value <> ctBiegeKnicken then
    begin
      if OutputForm.OutputPages.ActivePage = OutputForm.KraftSheet then
        OutputForm.OutputPages.ActivePage := OutputForm.OutputPages.FindNextPage(OutputForm.KraftSheet, False, False);
      OutputForm.Kraftsheet.TabVisible := False;
    end
    else
      OutputForm.Kraftsheet.TabVisible := True;
    KurveValid := False;
    UpdateGetriebe;

    ViewModelM.KnickenItemClick(Value);
    ViewModelM.ControllerEnabled := ControllerEnabled;
    ViewModelM.ControllerDown := ControllerBtnDown;
    ViewModelM.WinkelDown := WinkelBtnDown;
    ViewModelM.UpdateView;
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
      IntendedX := fpWinkel
    else if not Value and InputForm.rbWinkel.Checked then
      IntendedX := fpVorstag;
    KurveValid := False;
    UpdateGetriebe;
    ViewModelM.WinkelDown := FWinkelBtnDown;
    ViewModelM.UpdateView;
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
    UpdateGetriebe;
  end;
end;

procedure TRiggModul.SetSalingTyp(Value: TSalingTyp);
var
  fa: Integer;
begin
  if SalingTyp <> Value then
  begin
    case Value of
      stFest: fa := faSalingTypFest;
      stDrehbar: fa := faSalingTypDrehbar;
      stOhneBiegt: fa := faSalingTypOhne;
      stOhneStarr: fa := faSalingTypOhneStarr;
      else
        fa := faSalingTypFest;
    end;
    Main.HandleAction(fa);
  end;
end;

procedure TRiggModulGetriebe.DoOnUpdateSalingTyp(Value: TSalingTyp);
begin
  if FSalingTyp <> Value then
  begin
    FSalingTyp := Value;

    case Value of
      stFest: InputForm.InputPages.ActivePage := InputForm.tsFest;
      stDrehbar: InputForm.InputPages.ActivePage := InputForm.tsDrehbar;
      stOhneBiegt: InputForm.InputPages.ActivePage := InputForm.tsOhne;
      stOhneStarr: InputForm.InputPages.ActivePage := InputForm.tsOhneStarr;
    end;

    case Value of
      stFest: ViewModelM.FestItemClick;
      stDrehbar: ViewModelM.DrehbarItemClick;
      stOhneBiegt: ViewModelM.OSBItemClick;
      stOhneStarr: ViewModelM.OSSItemClick;
    end;
  end;
end;

procedure TRiggModul.SetLEDShape(Value: Boolean);
begin
  if FLEDShape <> Value then
  begin
    FLEDShape := Value;
    if Value then
      ViewModelM.LEDColor := clLime
    else
      ViewModelM.LEDColor := clRed;
    ViewModelM.UpdateView;
  end;
end;

procedure TRiggModul.BiegeNeigeItemClick;
var
  ControllerAnschlag: Integer;
begin
  ControllerAnschlag := 50;
  if SalingTyp = stFest then
  begin
    InputForm.sbController.Position := ControllerAnschlag;
    HandleScroll(InputForm.sbController, TScrollCode.scEndScroll, ControllerAnschlag);
  end
  else if SalingTyp = stDrehbar then
  begin
    InputForm.sbControllerD.Position := ControllerAnschlag;
    HandleScroll(InputForm.sbControllerD, TScrollCode.scEndScroll, ControllerAnschlag);
  end;

  if BiegeUndNeigeForm = nil then
    BiegeUndNeigeForm := TBiegeUndNeigeForm.Create(Application);

  BiegeUndNeigeForm.ShowModal;
  UpdateGCtrls(Rigg.Glieder);
  KurveValid := False;
  DrawPoint;

  { When Modal dialog is closed, update the current param value. }
  Main.Param := Main.Param;
end;

procedure TRiggModul.ReglerBtnClick;
var
  RF: TForm;
  ControllerAnschlag: Integer;
begin
  { reset Rigg.FiControllerAnschlag }
  ControllerAnschlag := 50;
  if SalingTyp = stFest then
  begin
    InputForm.sbController.Position := ControllerAnschlag;
    HandleScroll(InputForm.sbController, TScrollCode.scEndScroll, ControllerAnschlag);
  end
  else if SalingTyp = stDrehbar then
  begin
    InputForm.sbControllerD.Position := ControllerAnschlag;
    HandleScroll(InputForm.sbControllerD, TScrollCode.scEndScroll, ControllerAnschlag);
  end;

  if Rigg.CalcTyp = ctKraftGemessen then
  begin
    if BiegeUndNeigeForm = nil then
      BiegeUndNeigeForm := TBiegeUndNeigeForm.Create(Application);
    RF := BiegeUndNeigeForm;
  end
  else
  begin
    if FormReglerGraph = nil then
      FormReglerGraph := TFormReglerGraph.Create(Application);
    RF := FormReglerGraph;
  end;

  RF.ShowModal;
  UpdateGCtrls(Rigg.Glieder);
  KurveValid := False;
  DrawPoint;

  { Update the current param value }
  Main.Param := Main.Param;
end;

procedure TRiggModul.OSBItemClick;
begin
  InputForm.pnOhneBK.Update;
  InputForm.pnMastOhne.Update;

  WinkelBtnDown := False;

  SalingTyp := stOhneBiegt;

  KurveValid := False;
  if InputForm.rbControllerOhne.Checked then
    IntendedX := fpController
  else if InputForm.rbVorstagOhne.Checked then
    IntendedX := fpVorstag
  else if InputForm.rbWanteOhne.Checked then
    IntendedX := fpWante;

  UpdateGetriebe;
  Rigg.UpdateGSB;
  SetupGCtrls;

  ViewModelM.OSBItemClick;
  ViewModelM.ControllerEnabled := ControllerEnabled;
  ViewModelM.WinkelDown := WinkelBtnDown;
  ViewModelM.UpdateView;
end;

procedure TRiggModul.DrehbarItemClick;
begin
  InputForm.pnDrehbar.Update;
  InputForm.pnMastD.Update;

  WinkelBtnDown := False;

  SalingTyp := stDrehbar;

  KurveValid := False;
  if InputForm.rbControllerD.Checked then
    IntendedX := fpController
  else if InputForm.rbVorstagD.Checked then
    IntendedX := fpVorstag
  else if InputForm.rbWanteD.Checked then
    IntendedX := fpWante
  else if InputForm.rbWobenD.Checked then
    IntendedX := fpWoben
  else if InputForm.rbSalingLD.Checked then
    IntendedX := fpSalingL;

  UpdateGetriebe;
  Rigg.UpdateGSB;
  SetupGCtrls;

  ViewModelM.DrehbarItemClick;
  ViewModelM.ControllerEnabled := ControllerEnabled;
  ViewModelM.WinkelDown := WinkelBtnDown;
  ViewModelM.UpdateView;
end;

procedure TRiggModul.FestItemClick;
begin
  InputForm.pnFest.Update;
  InputForm.pnMast.Update;

  SalingTyp := stFest;

  KurveValid := False;
  if InputForm.rbController.Checked then
    IntendedX := fpController
  else if InputForm.rbWinkel.Checked and WinkelBtnDown then
    IntendedX := fpWinkel
  else if InputForm.rbWinkel.Checked and not WinkelBtnDown then
    IntendedX := fpVorstag
  else if InputForm.rbWante.Checked then
    IntendedX := fpWante
  else if InputForm.rbWoben.Checked then
    IntendedX := fpWoben
  else if InputForm.rbSalingH.Checked then
    IntendedX := fpSalingH
  else if InputForm.rbSalingA.Checked then
    IntendedX := fpSalingA;

  UpdateGetriebe;
  Rigg.UpdateGSB;
  SetupGCtrls;

  ViewModelM.FestItemClick;
  ViewModelM.ControllerEnabled := ControllerEnabled;
  ViewModelM.WinkelDown := WinkelBtnDown;
  ViewModelM.UpdateView;
end;

procedure TRiggModul.OSSItemClick;
begin
  InputForm.pnOhne.Update;

  WinkelBtnDown := False;
  SalingTyp := stOhneStarr;
  ControllerBtnDown := False;
  KurveValid := False;
  IntendedX := fpVorstag;

  UpdateGetriebe;
  Rigg.UpdateGSB;
  SetupGCtrls;

  ViewModelM.OSSItemClick;
  ViewModelM.ControllerEnabled := ControllerEnabled;
  ViewModelM.UpdateView;
end;

procedure TRiggModulDiagram.StraightLine;
var
  i: Integer;
begin
  for i := 0 to CPMax do
    f[i] := 100 * i / CPMax;
  TestF := f;
end;

procedure TRiggModulDiagram.GetCurves;
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
    end;

    FChartValid := True;
    KurveValid := True;
    DrawChart;

    if OutputForm.OutputPages.ActivePage = OutputForm.ChartSheet then
      if Screen.ActiveForm = OutputForm then
        OutputForm.ActiveControl := OutputForm.YComboBox;

  finally
    { restore Model (Getriebe) }
    Rigg.ProofRequired := True;
    Rigg.Glieder := InputRec;
    UpdateGetriebe;
    DrawPoint;
    Screen.Cursor := crDefault;
  end;
end;

procedure TRiggModulDiagram.UpdateGetriebePunkt;
var
  tempIndex: Integer;
begin
  InputBuffer := Rigg.Glieder;

  if not OutputForm.cbFollowPoint.Checked then
    Exit;

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
  if not (SofortBerechnen and Rigg.GetriebeOK and Rigg.MastOK) then
  begin
    DrawPoint;
  end
  else
  begin
    { DrawPoint will be called from UpdateRiggPunkt }
  end;
end;

procedure TRiggModulDiagram.UpdateRiggPunkt;
var
  tempIndex: Integer;
begin
  if not OutputForm.cbFollowPoint.Checked then
    Exit;

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

  DrawPoint;
end;

procedure TRiggModulDiagram.DrawChart;
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
{$ifdef debug}
      MessageBeep(MB_ICONASTERISK);
{$endif}
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
      fpSalingL: ChartPunktX := InputBuffer.SalingL;
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
  if OutputForm.OutputPages.ActivePage = OutputForm.ChartSheet then
    DrawChartPaintBox(ChartPaintBox.Canvas, ChartPaintBox.BoundsRect);
end;

procedure TRiggModulDiagram.DrawChartPaintBox(Canvas: TCanvas; Rect: TRect);

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
    else if FShowTriangle then
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

  OutputForm.lbAchseX.Caption := BottomTitel;
  OutputForm.lbAchseY.Caption := Lefttitel;
  OutputForm.lbXLeft.Caption := IntToStr(Round(Xmin));
  OutputForm.lbXRight.Caption := IntToStr(Round(Xmax));
  OutputForm.lbYBottom.Caption := IntToStr(Round(Ymin));
  OutputForm.lbYTop.Caption := IntToStr(Round(Ymax));
end;

function TRiggModulDiagram.GetPunktColor: TColor;
var
  i: Integer;
  ML: TStrings;
begin
  result := clLime;
  i := YComboBox.ItemIndex;
  ML := YComboBox.Items;

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

procedure TRiggModulDiagram.DrawPoint;
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
    fpSalingL: ChartPunktX := InputBuffer.SalingL;
    fpVorstagOS: ChartPunktX := InputBuffer.Vorstag;
  end;
  i := YComboBox.ItemIndex;
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
  if OutputForm.OutputPages.ActivePage = OutputForm.ChartSheet then
    DrawChartPaintBox(ChartPaintBox.Canvas, ChartPaintBox.BoundsRect);
end;

procedure TRiggModulDiagram.LookForYMinMax;
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

function TRiggModulDiagram.GetXText(sbn: TsbName): string;
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
    s := 'Saling-Abstand [mm]'
  else if sbn = fpSalingL then
    s := 'Saling-Länge [mm]';
  Result := s;
end;

function TRiggModulDiagram.GetYText(Text: string): string;
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

procedure TRiggModulDiagram.SetIntendedX(Value: TSBName);
begin
  if FIntendedX <> Value then
  begin
    FIntendedX := Value;
    KurveValid := False;
  end;
  if ActualX = IntendedX then
    FShowTriangle := True
  else
    FShowTriangle := False;
  if OutputForm.OutputPages.ActivePage = OutputForm.ChartSheet then
    DrawChart;
end;

procedure TRiggModulDiagram.YComboBoxChange(ItemIndex: Integer);
begin
  if (ItemIndex > -1) and (ItemIndex < ANr) then
    f := af[ItemIndex]
  else
    f := TestF;
  DrawChart;
end;

procedure TRiggModulDiagram.SetKurveValid(Value: Boolean);
begin
  if FKurveValid <> Value then
  begin
    FKurveValid := Value;
    OutputForm.SetKurveValidLED(Value);
  end;
end;

procedure TRiggModulDiagram.KurveBtnClick;
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

procedure TRiggModulGetriebe.SalingPaintBoxClick;
begin
  SalingGraph.SalingDetail := not SalingGraph.SalingDetail;
end;

procedure TRiggModulGetriebe.DrawPaintBoxS(Canvas: TCanvas);
begin
  PaintBackGround(BitmapS);
  SalingGraph.DrawSaling(BitmapS.Canvas);
  Canvas.CopyMode := cmSrcCopy;
  Canvas.Draw(0, 0, BitMapS);
end;

procedure TRiggModulGetriebe.DrawPaintBoxC(Canvas: TCanvas);
begin
  PaintBackGround(BitmapC);
  SalingGraph.DrawController(BitmapC.Canvas);
  Canvas.CopyMode := cmSrcCopy;
  Canvas.Draw(0, 0, BitMapC);
end;

procedure TRiggModulGetriebe.OutputPagesChange(Seite: Integer);
var
  TrimmRec: TTrimmControls;
begin
  case Seite of
    1: { Controller }
      begin
        TrimmRec := Rigg.Glieder;
        { Controller Parameter }
        SalingGraph.ControllerTyp := Rigg.ControllerTyp;
        SalingGraph.ControllerPos := TrimmRec.Controller;
        SalingGraph.ParamXE := Round(Rigg.MastPositionE);
        SalingGraph.ParamXE0 := Round(Rigg.rP[ooE0, x] - Rigg.rP[ooD0, x]);
        SalingGraph.EdgePos := Round(Rigg.GSB.Find(fpController).Min);
      end;
    3: { Saling }
      begin
        TrimmRec := Rigg.Glieder;
        { Saling Parameter }
        SalingGraph.SalingA := TrimmRec.SalingA;
        SalingGraph.SalingH := TrimmRec.SalingH;
        SalingGraph.SalingL := TrimmRec.SalingL;
      end;
  end;
end;

procedure TRiggModulGetriebe.ControllerZustellenBtnClick;
var
  TrimmRec: TTrimmControls;
begin
  TrimmRec := Rigg.Glieder;
  TrimmRec.Controller := 50;
  Rigg.Glieder := TrimmRec;
  Rigg.UpdateGetriebe;
  SalingGraph.ControllerPos := Round(SalingGraph.ParamXE0 - Rigg.MastPositionE);
  TrimmRec.Controller := SalingGraph.ControllerPos;
  UpdateGCtrls(TrimmRec);
  Rigg.Glieder := TrimmRec;
  UpdateGetriebe;
end;

procedure TRiggModulKraft.UpdateKraftGraphBtnClick;
begin
  Screen.Cursor := crHourGlass;
  KraftGraph.GetTestKurven;
  if Assigned(KraftPaintBox) then
  begin
    KraftGraph.Image := KraftPaintBox;
    KraftGraph.Draw;
  end;
  Screen.Cursor := crDefault;
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
        stOhneStarr:
        begin
          InputForm.lbOhne1.Caption := ls;
        end;
      end;
      if not ControllerBtnDown then
        FNeedPaint := False;
    end;

    fpVorstag, fpWinkel, fpVorstagOS:
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
        stOhneBiegt:
        begin
          InputRec.Vorstag := ScrollPos;
          InputForm.sbVorstagOhne.Position := ScrollPos;
          t := InputForm.sbVorstagOhne.Tag;
          InputForm.lbOhne1.Caption := ls;
        end;
        stOhneStarr:
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
        stOhneBiegt:
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

  if t = TagForSBName(FActualX) then
    FShowTriangle := True
  else
  begin
    FShowTriangle := False;
    KurveValid := False;
  end;

  InputBuffer := InputRec;
  Rigg.Glieder := InputRec;
  UpdateGetriebe;
end;

procedure TRiggModulDiagram.DoUpdateChartBuffer;
begin
  InputBuffer := Rigg.Glieder;
end;

procedure TRiggModul.DoResetForTrimmData;
begin
  SalingTyp := stFest;
  ControllerTyp := ctOhne;
  CalcTyp := ctQuerKraftBiegung;
end;

procedure TRiggModul.UpdateBtnClick;
begin
  UpdateGetriebe;
end;

procedure TRiggModulBase.UpdateGetriebe;
begin
  Main.UpdateGetriebe;
end;

procedure TRiggModulA.DoOnUpdateRigg;
begin
  if Rigg.GetriebeOK then
  begin
    ViewModelM.StatusPanelText1 := Rigg.RiggStatusText;
    ViewModelM.UpdateView;
  end;

  if OutputForm.Visible then
  begin
    if OutputForm.OutputPages.ActivePage = OutputForm.ChartSheet then
      UpdateRiggPunkt; { GetriebePunkte oben schon aktualisiert }
    AusgabeText; { update text before drawing }
    AusgabeKommentar;
    DrawPaintBoxM;
    rLItemClick(ReportItem);
  end;
end;

procedure TRiggModul.SetControllerTyp(Value: TControllerTyp);
begin
  if FControllerTyp <> Value then
  begin
    FControllerTyp := Value;
    Rigg.ControllerTyp := Value;
  end;
  SalingGraph.ControllerTyp := Value;
end;

procedure TRiggModulA.Init;
begin
  SetupGCtrls;

  MemCtrl := ZeroCtrl;
  InputBuffer := Rigg.Glieder;
  RefCtrl := Rigg.Glieder;
  RefPoints := Rigg.rP;

  FNeedPaint := True;

  YComboBox.ItemIndex := 3;
  YComboSavedItemIndex := YComboBox.ItemIndex;
  StraightLine;
  DrawChart;
end;

constructor TRiggModulInput.Create;
begin
  inherited;
  MastGraph := TMastGraph.Create;
end;

destructor TRiggModulInput.Destroy;
begin
  MastGraph.Free;
  inherited;
end;

constructor TRiggModulKraft.Create;
begin
  inherited;
  KraftPaintBox := OutputForm.KraftPaintBox;
  KraftGraph := TKraftGraph.Create(Rigg);
end;

destructor TRiggModulKraft.Destroy;
begin
  KraftGraph.Free;
  inherited;
end;

constructor TRiggModulGetriebe.Create;
begin
  inherited;
  ControllerPaintBox := OutputForm.ControllerPaintBox;
  SalingPaintBox := OutputForm.SalingPaintBox;

  SalingGraph := TSalingGraph.Create;
  SalingGraph.BackgroundColor := FBackgroundColor;

  BitmapS := TBitmap.Create;
  BitmapS.Width := Round(453 * MainVar.Scale);
  BitmapS.Height := Round(220 * MainVar.Scale);
  PaintBackGround(BitmapS);

  BitmapC := TBitmap.Create;
  BitmapC.Width := Round(453 * MainVar.Scale);
  BitmapC.Height := Round(220 * MainVar.Scale);
  PaintBackGround(BitmapC);
end;

destructor TRiggModulGetriebe.Destroy;
begin
  SalingGraph.Free;

  BitmapS.Free;
  BitmapC.Free;
  inherited;
end;

constructor TRiggModulReport.Create;
begin
  inherited;
  FReportItem := rF_Item;
  RiggReport := TRiggReport.Create;
  FWReport := TFWReport.Create;
end;

destructor TRiggModulReport.Destroy;
begin
  RiggReport.Free;
  FWReport.Free;
  inherited;
end;

constructor TRiggModulDiagram.Create;
begin
  inherited;
  FKurveValid := False;
  FChartValid := False;
  FIntendedX := fpVorstag;
  FActualX := fpVorstag;

  ChartPaintBox := OutputForm.ChartPaintBox;
  YComboBox := OutputForm.YComboBox;
end;

destructor TRiggModulDiagram.Destroy;
begin
  inherited;
end;

constructor TRiggModulA.Create(ARigg: TRigg);
begin
  Rigg := ARigg;
  RiggModul := Self;
  inherited Create;
end;

destructor TRiggModulA.Destroy;
begin
  inherited;
end;

constructor TRiggModulBase.Create;
begin
//  FBackgroundColor := TColor($333333);
  FBackgroundColor := clBtnFace;

  FSalingTyp := stFest;
  FControllerTyp := ctOhne;
  FCalcTyp := ctBiegeKnicken;

  FControllerBtnDown := True;
  FWinkelBtnDown := False;
  FDiffBtnDown := False;
  FSofortBtnDown := False;

  FViewPoint := vpSeite;

  FLEDShape := False;
end;

constructor TRiggModul.Create;
begin
  inherited;
end;

destructor TRiggModul.Destroy;
begin
  ViewModelM.Free;

  RiggModul := nil;
  inherited;
end;

end.

