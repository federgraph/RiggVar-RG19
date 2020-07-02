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

  TRiggModul = class
  protected
    FBackgroundColor: TColor;
    FKorrigiertItem: Boolean;
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

    { private variables, not properties }
    NeedPaint: Boolean;
    FGrauZeichnen: Boolean;
    TextFlipFlop: Boolean;
  public
    ViewModelM: TViewModelMain00;

    Rigg: TRigg;
    RiggReport: TRiggReport;
    FWReport: TFWReport;
    MemCtrl: TTrimmControls;
    RefCtrl: TTrimmControls;
    RefPoints: TRealRiggPoints;

    IniFileName: string;
    lbMastfall, lbSpannung, lbBiegung: string;
    Modified: Boolean;

    AutoSave: Boolean;
  protected
    ShowTriangle: Boolean;
    sbPuffer: TTrimmControls;

    function GetControllerEnabled: Boolean;
    procedure SetKorrigiertItem(Value: Boolean);
    procedure SetControllerBtnDown(Value: Boolean);
    procedure SetDiffBtnDown(Value: Boolean);
    procedure SetSofortBtnDown(Value: Boolean);
    procedure SetReportItem(Value: TReportItem);
    procedure SetCalcTyp(Value: TCalcTyp);
    procedure SetKurveValid(Value: Boolean);
    procedure SetLEDShape(Value: Boolean);
    procedure SetSBName(Value: TSBName);
    procedure SetSalingTyp(Value: TSalingTyp);
    procedure SetControllerTyp(Value: TControllerTyp); virtual; abstract;
    procedure SetWinkelBtnDown(Value: Boolean);
  protected
    procedure UpdateGetriebe;
    procedure DrawPoint; virtual; abstract;
    procedure DrawChart; virtual; abstract;
    procedure PaintBackGround(Image: TBitmap);
  public
    constructor Create;
    destructor Destroy; override;

    procedure DoOnWheelScroll(fp: TFederParam; ScrollPos: Integer);
    procedure DoResetForTrimmData;

    procedure SetupGCtrl(a: TScrollBar; b: TsbName);
    procedure SetupGCtrls;
    procedure UpdateGControls;
    procedure UpdateGCtrls(InputRec: TTrimmControls);
    procedure UpdateGCtrlLabels(InputRec: TTrimmControls);

    { former event handlers }
    procedure UpdateUI;
    procedure Neu(Doc: TRggDocument);
    procedure Open(FileName: string);
    procedure Save;
    procedure rLItemClick(Item: TReportItem);
    procedure UpdateBtnClick;
    procedure BiegeNeigeItemClick;
    procedure ReglerBtnClick;
    procedure FestItemClick;
    procedure OhneItemClick;
    procedure OSDlgItemClick;
    procedure DrehbarItemClick;
    procedure ChartItemClick;
    procedure WriteReportToMemo(Memo: TMemo);
    procedure sbControllerScroll(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);

    { Properties }
    property KorrigiertItem: Boolean read FKorrigiertItem write SetKorrigiertItem;
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
    property SofortBerechnen: Boolean read FSofortBtnDown;
    property GrauZeichnen: Boolean read FGrauZeichnen;
  end;

  TRiggModulD = class(TRiggModul)
  private
    TopTitel, LeftTitel, BottomTitel, RightTitel: string;
    Xmin, Xmax, Ymin, Ymax, YGap: single;
    ChartPunktX, ChartPunktY: single;
    PunktColor: TColor;
    f, TestF: TChartLineData;
    af: ChartArray;
    bf: array[0..ANr - 1] of double;
  public
    YComboBox: TComboBox;
    YComboSavedItemIndex: Integer;

    ChartPaintBox: TPaintBox;

    procedure DoUpdateChartBuffer;
    procedure DrawPoint; override;
    procedure DrawChartPaintBox(Canvas: TCanvas; Rect: TRect);
    procedure YComboBoxChange(ItemIndex: Integer);
    procedure KurveBtnClick;

    procedure StraightLine;
    procedure GetCurves;
    procedure UpdateGetriebePunkt;
    procedure UpdateRiggPunkt;
    procedure DrawChart; override;
    procedure LookForYMinMax;
    function GetXText(sbn: TsbName): string;
    function GetYText(Text: string): string;
    function GetPunktColor: TColor;
  end;

  TRiggModulG = class(TRiggModulD)
  public
    SalingGraph: TSalingGraph;
    BitmapS: TBitmap;
    BitmapC: TBitmap;
    ControllerPaintBox: TPaintBox;
    SalingPaintBox: TPaintBox;
    KraftPaintBox: TImage;

    MastGraph: TMastGraph;
    KraftGraph: TKraftGraph;

    constructor Create;
    destructor Destroy; override;

    procedure TestBtnClick;

    procedure ZustellBtnClick;
    procedure OutputPagesChange(Seite: Integer);
    procedure SalingPaintBoxClick;

    procedure DoOnUpdateSalingTyp(Value: TSalingTyp);
    procedure AusgabeText;
    procedure AusgabeKommentar;
    procedure DrawPaintBoxM;
    procedure DrawPaintBoxS(Canvas: TCanvas);
    procedure DrawPaintBoxC(Canvas: TCanvas);
    procedure DoGraphics;
  end;

  TRiggModulA = class(TRiggModulG)
  protected
    procedure SetControllerTyp(Value: TControllerTyp); override;
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
  FrmChart,
  FrmBiege,
  FrmKreis;

constructor TRiggModul.Create;
begin
  inherited;
  FBackgroundColor := clBtnFace;
  FKorrigiertItem := True;
  FControllerBtnDown := True;
  FWinkelBtnDown := False;
  FDiffBtnDown := False;
  FSofortBtnDown := False;
  FReportItem := rF_Item;
  FCalcTyp := ctBiegeKnicken;
  FKurveValid := False;
  FChartValid := False;
  FViewPoint := vpSeite;
  FLEDShape := False;
  FSBName := fpVorstag;
  FCursorSB := fpVorstag;
  FSalingTyp := stFest;
  FControllerTyp := ctOhne;
end;

destructor TRiggModul.Destroy;
begin
  RiggReport.Free;
  FWReport.Free;

  ViewModelM.Free;

  RiggModul := nil;
  inherited;
end;

procedure TRiggModul.SetupGCtrl(a: TScrollBar; b: TsbName);
var
  cr: TRggSB;
begin
  cr := Rigg.GSB.Find(b);
  a.SetParams(Round(cr.Ist), Round(cr.Min), Round(cr.Max));
  a.LargeChange := Round(cr.BigStep);
  a.SmallChange := Round(cr.SmallStep);
end;

procedure TRiggModul.SetupGCtrls;
begin
  { Controller }
  SetupGCtrl(InputForm.sbController, fpController);
  SetupGCtrl(InputForm.sbControllerD, fpController);
  SetupGCtrl(InputForm.sbControllerOhne, fpController);
  { Vorstag/Winkel }
  if WinkelBtnDown then
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

procedure TRiggModul.UpdateGControls;
begin
  UpdateGCtrls(Rigg.Glieder);
end;

procedure TRiggModul.UpdateGCtrlLabels(InputRec: TTrimmControls);
begin
  InputForm.lbValue1.Caption := Format('%d mm', [InputRec.Controller - MemCtrl.Controller]);
  if WinkelBtnDown then
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

procedure TRiggModul.UpdateGCtrls(InputRec: TTrimmControls);
begin
  sbPuffer := InputRec;

  InputForm.sbController.Position := InputRec.Controller;
  InputForm.sbControllerD.Position := InputRec.Controller;
  InputForm.sbControllerOhne.Position := InputRec.Controller;

  if WinkelBtnDown then
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

procedure TRiggModul.sbControllerScroll(Sender: TObject;
  ScrollCode: TScrollCode; var ScrollPos: Integer);
var
  InputRec: TTrimmControls;
begin
  Modified := True;
  InputRec := Rigg.Glieder;

  if Sender = InputForm.sbController then
  begin
    InputRec.Controller := ScrollPos;
    InputForm.lbValue1.Caption := Format('%d mm', [ScrollPos - MemCtrl.Controller]);
    if not ControllerBtnDown then
      NeedPaint := False;
  end
  else if Sender = InputForm.sbControllerD then
  begin
    InputRec.Controller := ScrollPos;
    InputForm.lbD1.Caption := Format('%d mm', [ScrollPos - MemCtrl.Controller]);
    if not ControllerBtnDown then
      NeedPaint := False;
  end
  else if Sender = InputForm.sbControllerOhne then
  begin
    InputRec.Controller := ScrollPos;
    InputForm.lbOhne1.Caption := Format('%d mm', [ScrollPos - MemCtrl.Controller]);
    if not ControllerBtnDown then
      NeedPaint := False;
  end
  else if Sender = InputForm.sbWinkel then
  begin
    if WinkelBtnDown then
    begin
      InputRec.Winkel := ScrollPos;
      InputForm.lbValue2.Caption := Format('%5.2f Grad', [(InputRec.Winkel - MemCtrl.Winkel) /
        10]);
    end
    else
    begin
      InputRec.Vorstag := ScrollPos;
      InputForm.lbValue2.Caption := Format('%d mm', [InputRec.Vorstag - MemCtrl.Vorstag]);
    end
  end
  else if Sender = InputForm.sbVorstagD then
  begin
    InputRec.Vorstag := ScrollPos;
    InputForm.lbD2.Caption := Format('%d mm', [InputRec.Vorstag - MemCtrl.Vorstag]);
  end
  else if Sender = InputForm.sbVorstagOhne then
  begin
    InputRec.Vorstag := ScrollPos;
    InputForm.lbOhne2.Caption := Format('%d mm', [InputRec.Vorstag - MemCtrl.Vorstag]);
  end
  else if Sender = InputForm.sbWante then
  begin
    InputRec.Wanten := ScrollPos;
    InputForm.lbValue3.Caption := Format('%d mm', [ScrollPos - MemCtrl.Wanten]);
  end
  else if Sender = InputForm.sbWanteD then
  begin
    InputRec.Wanten := ScrollPos;
    InputForm.lbD3.Caption := Format('%d mm', [ScrollPos - MemCtrl.Wanten]);
  end
  else if Sender = InputForm.sbWanteOhne then
  begin
    InputRec.Wanten := ScrollPos;
    InputForm.lbOhne3.Caption := Format('%d mm', [ScrollPos - MemCtrl.Wanten]);
  end
  else if Sender = InputForm.sbWoben then
  begin
    InputRec.Woben := ScrollPos;
    InputForm.lbValue4.Caption := Format('%d mm', [ScrollPos - MemCtrl.Woben]);
  end
  else if Sender = InputForm.sbWobenD then
  begin
    InputRec.Woben := ScrollPos;
    InputForm.lbD4.Caption := Format('%d mm', [ScrollPos - MemCtrl.Woben]);
  end
  else if Sender = InputForm.sbSalingH then
  begin
    InputRec.SalingH := ScrollPos;
    InputForm.lbValue5.Caption := Format('%d mm', [ScrollPos - MemCtrl.SalingH]);
  end
  else if Sender = InputForm.sbSalingA then
  begin
    InputRec.SalingA := ScrollPos;
    InputForm.lbValue6.Caption := Format('%d mm', [ScrollPos - MemCtrl.SalingA]);
  end
  else if Sender = InputForm.sbSalingLD then
  begin
    InputRec.SalingL := ScrollPos;
    InputForm.lbD5.Caption := Format('%d mm', [ScrollPos - MemCtrl.SalingL]);
  end
  else if Sender = InputForm.sbVorstagOs then
  begin
    InputRec.Vorstag := ScrollPos;
    InputForm.lbValue7.Caption := Format('%d mm', [ScrollPos - MemCtrl.Vorstag])
  end
  else if Sender = InputForm.sbWPowerOS then
  begin
    InputRec.WPowerOS := ScrollPos;
    InputForm.lbValue8.Caption := Format('%d N', [ScrollPos - MemCtrl.WPowerOS]);
    NeedPaint := False;
  end;

  if (ScrollCode = TScrollCode.scEndScroll) or not SofortBerechnen then
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
  end;
end;

procedure TRiggModulG.DoGraphics;
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
    SalingGraph.ParamXE0 := Round(Rigg.iP[ooE0, x] - Rigg.iP[ooD0, x]);
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
    UpdateGetriebePunkt;

  TextFlipFlop := False;
  { Grafik aktualisieren, aber nicht zweimal!}
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

procedure TRiggModul.PaintBackGround(Image: TBitmap);
var
  R: TRect;
begin
  if Image = nil then
    Exit;

  R := Rect(0, 0, Image.Width, Image.Height);
  Image.Canvas.Brush.Color := FBackgroundColor;
  Image.Canvas.FillRect(R);
end;

procedure TRiggModulG.DrawPaintBoxM;
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

procedure TRiggModulG.AusgabeText;
var
  tempSalingDaten: TSalingDaten;
  MemoPosY: LongInt;
  ML: TStrings;
begin
  tempSalingDaten := Rigg.SalingDaten;

  MemoPosY := SendMessage(OutputForm.DisplayMemo.Handle, EM_GETFIRSTVISIBLELINE, 0, 0);
  ML := OutputForm.DisplayMemo.Lines;
  ML.BeginUpdate;
  ML.Clear;

  { Text setzen }
  lbMastFall := Format('Mastfall = %5.1f cm', [Rigg.Trimm.Mastfall / 10]);
  lbSpannung := Format('Spannung = %5.0f N', [Rigg.rF[14]]);
  lbBiegung := Format('Biegung  = %5.1f cm', [Rigg.hd / 10]);

  ML.Add('Trimm:');
  ML.Add(Format('  Mastfall F0F     = %8.1f cm', [Rigg.Trimm.Mastfall / 10]));
  ML.Add(Format('  Vorstagspannung  = %8.1f N', [Rigg.rF[14]]));
  ML.Add(Format('  Durchbiegung hd  = %8.1f cm', [Rigg.hd / 10]));

  ML.Add('');
  ML.Add('Saling:');
  ML.Add(Format('  Saling Länge   = %6.2f mm', [tempSalingDaten.SalingL]));
  ML.Add(Format('  Saling Höhe    = %6.2f mm', [tempSalingDaten.SalingH]));
  ML.Add(Format('  Saling Abstand = %6.2f mm', [tempSalingDaten.SalingA]));
  ML.Add(Format('  Saling Winkel  = %6.2f Grad', [tempSalingDaten.SalingW]));
  ML.Add(Format('  Wanten Winkel  = %6.2f Grad', [tempSalingDaten.WantenWinkel]));
  ML.Add(Format('  Kraft Winkel   = %6.2f Grad', [tempSalingDaten.KraftWinkel]));

  ML.Add('');
  ML.Add('SchnittKräfte:');
  ML.Add(Format('  FC  = %8.2f N    (Mastdruckkraft)', [Rigg.FC]));
  ML.Add(Format('  FB  = %8.2f N    (Wanten/Vorstag)', [Rigg.FB]));
  ML.Add(Format('  F2  = %8.2f N    (Saling)', [Rigg.F2]));
  ML.Add(Format('  F1  = %8.2f N    (Controller)', [Rigg.F1]));
  ML.Add(Format('  FA  = %8.2f N    (Mastfuß)', [Rigg.FA]));
  ML.Add(Format('  hd  = %8.2f mm   (Saling Durchbiegung)', [Rigg.hd]));
  ML.Add(Format('  he  = %8.2f mm   (Controller Durchbiegung)', [Rigg.he]));
  ML.Add(Format('  sd  = %8.2f mm   (hd-FSalingWegKnick)', [Rigg.hd-Rigg.FSalingWegKnick]));

  ML.Add('');
  ML.Add('BiegeKnicken:');
  ML.Add(Format('  KoppelFaktor       = %8.5f', [Rigg.FKoppelFaktor]));
  ML.Add(Format('  SalingAlpha        = %8.5f mm/N', [Rigg.FSalingAlpha]));
  ML.Add(Format('  ControllerAlpha    = %8.5f mm/N', [Rigg.FControllerAlpha]));
  ML.Add(Format('  SalingWeg          = %8.2f mm', [Rigg.FSalingWeg]));
  ML.Add(Format('  SalingWegKnick     = %8.2f mm', [Rigg.FSalingWegKnick]));
  ML.Add(Format('  ControllerWeg      = %8.2f mm', [Rigg.FControllerWeg]));
  ML.Add(Format('  FSchnittPunktKraft = %8.2f N', [Rigg.FSchnittPunktKraft]));
  ML.Add(Format('  FwSchnittOhne      = %8.2f mm', [Rigg.FwSchnittOhne]));
  ML.Add(Format('  FwSchnittMit       = %8.2f mm', [Rigg.FwSchnittMit]));
  ML.Add(Format('  FwSchnittOffset    = %8.2f mm', [Rigg.FwSchnittOffset]));

  ML.Add('');
  ML.Add('SchnittWinkel:');
  ML.Add(Format('  alpha1 = %6.2f Grad', [Rigg.alpha1 * 180 / pi]));
  ML.Add(Format('  alpha2 = %6.2f Grad', [Rigg.alpha2 * 180 / pi]));
  ML.Add(Format('  delta1 = %6.2f Grad', [Rigg.delta1 * 180 / pi]));
  ML.Add(Format('  delta2 = %6.2f Grad', [Rigg.delta2 * 180 / pi]));
  ML.Add(Format('  gamma  = %6.2f Grad', [Rigg.gamma * 180 / pi]));
  ML.Add(Format('  beta   = %6.2f Grad', [Rigg.beta * 180 / pi]));

  ML.Add('');
  ML.Add('Winkel:');
  ML.Add(Format('  phi       = %6.2f Grad', [Rigg.phi * 180 / pi]));
  ML.Add(Format('  psi       = %6.2f Grad', [Rigg.psi * 180 / pi]));
  ML.Add(Format('  alpha     = %6.2f Grad', [Rigg.alpha * 180 / pi]));
  ML.Add(Format('  phi-alpha = %6.2f Grad (Mast-Neigung)', [(Rigg.phi-Rigg.alpha)*180/pi]));
  ML.Add(Format('  psi-alpha = %6.2f Grad (Wanten-Neigung)', [(Rigg.psi-Rigg.alpha)*180/pi]));

  ML.Add('');
  ML.Add('MastWinkel:');
  ML.Add(Format('  epsB = %6.2f Grad', [Rigg.epsB * 180 / pi]));
  ML.Add(Format('  eps2 = %6.2f Grad', [Rigg.eps2 * 180 / pi]));
  ML.Add(Format('  eps1 = %6.2f Grad', [Rigg.eps1 * 180 / pi]));
  ML.Add(Format('  epsA = %6.2f Grad', [Rigg.epsA * 180 / pi]));
  ML.Add(Format('  Epsilon  = %6.2f Grad', [epsilon * 180 / pi]));

  SendMessage(OutputForm.DisplayMemo.Handle, EM_LINESCROLL, 0, MemoPosY);
  ML.EndUpdate;
end;

procedure TRiggModulG.AusgabeKommentar;
var
  temp: double;
  ML: TStrings;
begin
  ML := OutputForm.KommentarMemo.Lines;
  ML.BeginUpdate;
  ML.Clear;

  temp := Rigg.hd / 10; { Biegung in cm }
  if temp < 0 then
    ML.Add('Mastbiegung negativ!');
  if temp < 2 then
    ML.Add('Mast hat fast keine Vorbiegung.');
  if temp > 10 then
    ML.Add('Mastbiegung zu groß.');

  temp := Rigg.rF[14]; { Vorstagspannung in N }
  if temp < 800 then
    ML.Add('Vorstagspannung zu gering.');
  if temp > 2000 then
    ML.Add('Vorstagspannung zu groß.');

  ML.EndUpdate;
end;

procedure TRiggModul.SetReportItem(Value: TReportItem);
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

procedure TRiggModul.rLItemClick(Item: TReportItem);
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
    { output all}
//     FWReport.Ausgabe(Rigg.Fachwerk);

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

procedure TRiggModul.Neu(Doc: TRggDocument);
begin
  IniFileName := '';
  if Doc = nil then
    Rigg.SetDefaultDocument { --> Rigg.SetDocument }
  else
    Rigg.SetDocument(Doc);
  UpdateUI;
end;

procedure TRiggModul.Open(FileName: string);
begin
  try
    Rigg.LoadFromDocFile(FileName); { --> Rigg.SetDocument }
    IniFileName := FileName;
    UpdateUI;
  except
    on EFileFormatError do { eat ecxeption }
      if IniFileName = '' then
      begin
        ViewModelM.Caption := 'Rigg';
        ViewModelM.UpdateView;
      end;
  end;
end;

procedure TRiggModul.Save;
begin
  Rigg.WriteToDocFile(IniFileName);
  Modified := False;
end;

function TRiggModul.GetControllerEnabled: Boolean;
begin
  Result := True;
  if CalcTyp = ctKraftGemessen then
    Result := False;
  if SalingTyp = stOhne then
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
      SBName := fpWinkel
    else if not Value and InputForm.rbWinkel.Checked then
      SBName := fpVorstag;
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
      stOhne: fa := faSalingTypOhne;
      stOhne_2: fa := faSalingTypOhneStarr;
      else
        fa := faSalingTypFest;
    end;
    Main.HandleAction(fa);
  end;
end;

procedure TRiggModulG.DoOnUpdateSalingTyp(Value: TSalingTyp);
begin
  if FSalingTyp <> Value then
  begin
    FSalingTyp := Value;

    case Value of
      stFest: InputForm.InputPages.ActivePage := InputForm.tsFest;
      stDrehbar: InputForm.InputPages.ActivePage := InputForm.tsDrehbar;
      stOhne: InputForm.InputPages.ActivePage := InputForm.tsOhne;
      stOhne_2: InputForm.InputPages.ActivePage := InputForm.tsOhneStarr;
    end;

    case Value of
      stFest: ViewModelM.FestItemClick;
      stDrehbar: ViewModelM.DrehbarItemClick;
      stOhne: ViewModelM.OhneItemClick;
      stOhne_2: ViewModelM.OSDlgItemClick;
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
    sbControllerScroll(InputForm.sbController, TScrollCode.scEndScroll, ControllerAnschlag);
  end
  else if SalingTyp = stDrehbar then
  begin
    InputForm.sbControllerD.Position := ControllerAnschlag;
    sbControllerScroll(InputForm.sbControllerD, TScrollCode.scEndScroll, ControllerAnschlag);
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
    sbControllerScroll(InputForm.sbController, TScrollCode.scEndScroll, ControllerAnschlag);
  end
  else if SalingTyp = stDrehbar then
  begin
    InputForm.sbControllerD.Position := ControllerAnschlag;
    sbControllerScroll(InputForm.sbControllerD, TScrollCode.scEndScroll, ControllerAnschlag);
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

procedure TRiggModul.OhneItemClick;
begin
  InputForm.pnOhneBK.Update;
  InputForm.pnMastOhne.Update;

  WinkelBtnDown := False;

  SalingTyp := stOhne_2;

  KurveValid := False;
  if InputForm.rbControllerOhne.Checked then
    SBName := fpController
  else if InputForm.rbVorstagOhne.Checked then
    SBName := fpVorstag
  else if InputForm.rbWanteOhne.Checked then
    SBName := fpWante;

  UpdateGetriebe;
  Rigg.UpdateGSB;
  SetupGCtrls;

  ViewModelM.OhneItemClick;
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
    SBName := fpController
  else if InputForm.rbVorstagD.Checked then
    SBName := fpVorstag
  else if InputForm.rbWanteD.Checked then
    SBName := fpWante
  else if InputForm.rbWobenD.Checked then
    SBName := fpWoben
  else if InputForm.rbSalingLD.Checked then
    SBName := fpSalingL;

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
    SBName := fpController
  else if InputForm.rbWinkel.Checked and WinkelBtnDown then
    SBName := fpWinkel
  else if InputForm.rbWinkel.Checked and not WinkelBtnDown then
    SBName := fpVorstag
  else if InputForm.rbWante.Checked then
    SBName := fpWante
  else if InputForm.rbWoben.Checked then
    SBName := fpWoben
  else if InputForm.rbSalingH.Checked then
    SBName := fpSalingH
  else if InputForm.rbSalingA.Checked then
    SBName := fpSalingA;

  UpdateGetriebe;
  Rigg.UpdateGSB;
  SetupGCtrls;

  ViewModelM.FestItemClick;
  ViewModelM.ControllerEnabled := ControllerEnabled;
  ViewModelM.WinkelDown := WinkelBtnDown;
  ViewModelM.UpdateView;
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

  ViewModelM.OSDlgItemClick;
  ViewModelM.ControllerEnabled := ControllerEnabled;
  ViewModelM.UpdateView;
end;

procedure TRiggModul.ChartItemClick;
begin
  ChartForm := TChartForm.Create(Application);
end;

procedure TRiggModul.SetKorrigiertItem(Value: Boolean);
begin
  if FKorrigiertItem <> Value then
  begin
    FKorrigiertItem := Value;
    Rigg.Korrigiert := Value;
    UpdateGetriebe;
  end;
end;

procedure TRiggModulD.StraightLine;
var
  i: Integer;
begin
  for i := 0 to CPMax do
    f[i] := 100 * i / CPMax;
  TestF := f;
end;

procedure TRiggModulD.GetCurves;
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
    Anfang := Rigg.GSB.Find(SBName).Min;
    Ende := Rigg.GSB.Find(SBName).Max;
    for i := 0 to CPMax do
    begin
      Antrieb := Anfang + (Ende - Anfang) * i / CPMax;
      { Antrieb ansteuern }
      case SBName of
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
    end;

    FChartValid := True;
    KurveValid := True;
    DrawChart;

    if OutputForm.OutputPages.ActivePage = OutputForm.ChartSheet then
      if Screen.ActiveForm = OutputForm then
        OutputForm.ActiveControl := OutputForm.YComboBox
      else
      begin
{$ifdef debug}
        MessageBeep(MB_ICONASTERISK);
{$endif}
      end;

  finally
    { restore Model (Getriebe) }
    Rigg.ProofRequired := True;
    Rigg.Glieder := InputRec;
    UpdateGetriebe;
    DrawPoint;
    Screen.Cursor := crDefault;
  end;
end;

procedure TRiggModulD.UpdateGetriebePunkt;
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

procedure TRiggModulD.UpdateRiggPunkt;
var
  tempIndex: Integer;
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
  if OutputForm.cbFollowPoint.Checked then
    DrawPoint;
end;

procedure TRiggModulD.DrawChart;
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
    case SBName of
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

procedure TRiggModulD.DrawChartPaintBox(Canvas: TCanvas; Rect: TRect);

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

  OutputForm.lbAchseX.Caption := BottomTitel;
  OutputForm.lbAchseY.Caption := Lefttitel;
  OutputForm.lbXLeft.Caption := IntToStr(Round(Xmin));
  OutputForm.lbXRight.Caption := IntToStr(Round(Xmax));
  OutputForm.lbYBottom.Caption := IntToStr(Round(Ymin));
  OutputForm.lbYTop.Caption := IntToStr(Round(Ymax));
end;

function TRiggModulD.GetPunktColor: TColor;
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

procedure TRiggModulD.DrawPoint;
var
  i: Integer;
begin
  if not FChartValid then
    Exit;
  { Koordinaten des Punktes }
  case SBName of
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

procedure TRiggModulD.LookForYMinMax;
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

function TRiggModulD.GetXText(sbn: TsbName): string;
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

function TRiggModulD.GetYText(Text: string): string;
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

procedure TRiggModulD.YComboBoxChange(ItemIndex: Integer);
begin
  (*
  if YComboBox.ItemIndex <> YComboBox2.ItemIndex then
  begin
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

procedure TRiggModulD.KurveBtnClick;
var
  cr: TRggSB;
begin
  BottomTitel := GetXText(SBName);
  cr := Rigg.GSB.Find(SBName);
  Xmin := cr.Min;
  Xmax := cr.Max;
  if SBName = fpWinkel then
  begin
    Xmin := Xmin / 10;
    Xmax := Xmax / 10;
  end;
  CursorSB := SBName;
  GetCurves;
end;

procedure TRiggModulG.SalingPaintBoxClick;
begin
  SalingGraph.SalingDetail := not SalingGraph.SalingDetail;
end;

procedure TRiggModulG.DrawPaintBoxS(Canvas: TCanvas);
begin
  PaintBackGround(BitmapS);
  SalingGraph.DrawSaling(BitmapS.Canvas);
  Canvas.CopyMode := cmSrcCopy;
  Canvas.Draw(0, 0, BitMapS);
end;

procedure TRiggModulG.DrawPaintBoxC(Canvas: TCanvas);
begin
  PaintBackGround(BitmapC);
  SalingGraph.DrawController(BitmapC.Canvas);
  Canvas.CopyMode := cmSrcCopy;
  Canvas.Draw(0, 0, BitMapC);
end;

procedure TRiggModulG.OutputPagesChange(Seite: Integer);
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
        SalingGraph.ParamXE0 := Round(Rigg.iP[ooE0, x] - Rigg.iP[ooD0, x]);
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

procedure TRiggModulG.ZustellBtnClick;
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

procedure TRiggModulG.TestBtnClick;
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

  // if not SofortBerechnen then
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
  end;
end;

procedure TRiggModulD.DoUpdateChartBuffer;
begin
  sbPuffer := Rigg.Glieder;
end;

procedure TRiggModul.DoResetForTrimmData;
begin
  SalingTyp := stFest;
  ControllerTyp := ctOhne;
  CalcTyp := ctQuerKraftBiegung;
end;

{ RiggModulG }

constructor TRiggModulG.Create;
begin
  inherited;

  ControllerPaintBox := OutputForm.ControllerPaintBox;
  SalingPaintBox := OutputForm.SalingPaintBox;
  KraftPaintBox := OutputForm.KraftPaintBox;
  ChartPaintBox := OutputForm.ChartPaintBox;
  YComboBox := OutputForm.YComboBox;

  { SalingCtrls }
  SalingGraph := TSalingGraph.Create;
  SalingGraph.BackgroundColor := FBackgroundColor;

  BitmapS := TBitmap.Create;
  BitmapS.Width := 453;
  BitmapS.Height := 220;
  PaintBackGround(BitmapS);

  BitmapC := TBitmap.Create;
  BitmapC.Width := 453;
  BitmapC.Height := 220;
  PaintBackGround(BitmapC);

  MastGraph := TMastGraph.Create;
  KraftGraph := TKraftGraph.Create(Rigg);
end;

destructor TRiggModulG.Destroy;
begin
  MastGraph.Free;
  KraftGraph.Free;
  SalingGraph.Free;

  BitmapS.Free;
  BitmapC.Free;
  inherited;
end;

{ RiggModulA }

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

procedure TRiggModul.UpdateBtnClick;
begin
  UpdateGetriebe;
end;

procedure TRiggModul.UpdateGetriebe;
begin
  Main.UpdateGetriebe;

//  Rigg.UpdateGetriebe;
//  if NeedPaint then
//    DoGraphics;
//  NeedPaint := True;
//  if (SofortBerechnen and Rigg.GetriebeOK and Rigg.MastOK) then
//    UpdateRigg;
end;

//procedure TRiggModulA.UpdateRigg;
//begin
//  Rigg.UpdateRigg;
//
//  if Rigg.RiggOK then
//  begin
//    FGrauZeichnen := True;
//    LEDShape := True;
//  end
//  else
//  begin
//    FGrauZeichnen := False;
//    LEDShape := False;
//  end;
//
//  DoOnUpdateRigg;
//end;

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

procedure TRiggModulA.SetControllerTyp(Value: TControllerTyp);
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
  IniFileName := '';

  SetupGCtrls;

  MemCtrl := ZeroCtrl;
  sbPuffer := Rigg.Glieder;
  RefCtrl := Rigg.Glieder;
  RefPoints := Rigg.rP;

  { Berichte }
  RiggReport := TRiggReport.Create;
  FWReport := TFWReport.Create;

  NeedPaint := True;

  { avoid Exception, if ItemIndex = -1 }
  YComboBox.ItemIndex := 1;
  YComboSavedItemIndex := 1;
  StraightLine;
  DrawChart;
end;

end.

