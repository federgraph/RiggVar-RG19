unit FrmMain;

(*
-
-     F
-    * * *
-   *   *   G
-  *     * *   *
- E - - - H - - - I
-  *     * *         *
-   *   *   *           *
-    * *     *             *
-     D-------A---------------B
-              *
-              (C) federgraph.de
-
*)

interface

{$ifdef fpc}
{$mode delphi}
{$endif}

uses
  RiggVar.FB.Color,
  RiggVar.FB.SpeedColor,
  RiggVar.FB.SpeedBar,
  RiggVar.RG.Def,
  RiggVar.RG.Model,
  RiggVar.RG.Report,
  RiggVar.RG.Rota,
  RggCtrls,
  RggChartGraph,
  RggTypes,
  SysUtils,
  Classes,
  Types,
  UITypes,
  Vcl.Menus,
  Vcl.ComCtrls,
  Controls,
  Forms,
  StdCtrls,
  ExtCtrls,
  Dialogs,
  Graphics;

{$define Vcl}

type
  TFormMain = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    procedure UpdateFormat(w, h: Integer);
    procedure GotoLandscape;
    procedure GotoNormal;
    procedure GotoPortrait;
    procedure GotoSquare;
  protected
    procedure InitScreenPos;
  private
    FScale: single;
    DefaultCaption: string;
    FormShown: Boolean;
    procedure ApplicationEventsException(Sender: TObject; E: Exception);
    procedure FormCreate2(Sender: TObject);
    procedure FormDestroy2(Sender: TObject);
    procedure HandleShowHint(Sender: TObject);
    procedure Flash(s: string);
    procedure Reset;
    procedure PlaceImage(PosLeft, PosTop: Integer);
    procedure InitDebugInfo;
    procedure InitZOrderInfo;
    procedure ShowHelpText(fa: Integer);
    function GetCanShowMemo: Boolean;
  protected
    HelpTopic: Integer;
    ShowingHelp: Boolean;
    HL: TStringList;
    RL: TStrings;
    TL: TStrings;
    procedure InitParamListbox;
  public
    procedure ShowTrimm;
    procedure ShowTrimmData;
  public
    FWantButtonReport: Boolean;
    procedure UpdateReport;
    property WantButtonReport: Boolean read FWantButtonReport;
  public
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    procedure InitOpenDialog;
    procedure InitSaveDialog;
    function GetOpenFileName(dn, fn: string): string;
    function GetSaveFileName(dn, fn: string): string;
  public
    FocusContainer: TButton;
    HintContainer: TWinControl;
    HintText: TLabel;
    ReportText: TMemo;
    TrimmText: TMemo;
    ParamListbox: TListBox;
    ReportListbox: TListBox;
    ReportLabel: TLabel;
    function FindItemIndexOfParam(ML: TStrings): Integer;
    procedure UpdateItemIndexParams;
    procedure UpdateItemIndexParamsLB;
    procedure UpdateItemIndexReports;
    procedure UpdateItemIndexTrimms;
    procedure ParamListboxChange(Sender: TObject);
    procedure ReportListboxChange(Sender: TObject);
  public
    procedure ShowReport(const Value: TRggReport);
    function GetShowDataText: Boolean;
    function GetShowDiffText: Boolean;
    function GetShowTrimmText: Boolean;
    procedure SetShowDataText(const Value: Boolean);
    procedure SetShowDiffText(const Value: Boolean);
    procedure SetShowTrimmText(const Value: Boolean);
    property ShowTrimmText: Boolean read GetShowTrimmText write SetShowTrimmText;
    property ShowDiffText: Boolean read GetShowDiffText write SetShowDiffText;
    property ShowDataText: Boolean read GetShowDataText write SetShowDataText;
  public
    ComponentsCreated: Boolean;
    procedure UpdateParent;
    procedure CreateComponents;
    procedure CheckSpaceForImages;
    procedure CheckSpaceForMemo;
    procedure CheckSpaceForListbox;
    procedure SetupMemo(MM: TMemo);
    procedure SetupListbox(LB: TListBox);
  public
    Raster: Integer;
    Margin: Integer;
    ListboxWidth: Integer;
    ReportMemoWidth: Integer;
    SpeedPanelHeight: Integer;
    SpeedPanel: TActionSpeedBar;
    SpeedPanel01: TActionSpeedBar;
    SpeedPanel02: TActionSpeedBar;
    SpeedPanel03: TActionSpeedBar;
    SpeedPanel04: TActionSpeedBar;
    SpeedColorScheme: TSpeedColorScheme;
    procedure InitSpeedButtons;
    procedure LayoutSpeedPanel(SP: TActionSpeedBar);
    procedure UpdateSpeedButtonDown;
    procedure UpdateSpeedButtonEnabled;
    procedure ToggleSpeedPanel;
    procedure ToggleSpeedPanelFontSize;
    procedure SwapSpeedPanel(Value: Integer);
    procedure SwapRota(Value: Integer);
  public
    procedure ChartImageBtnClick(Sender: TObject);
    procedure SalingImageBtnClick(Sender: TObject);
    procedure ControllerImageBtnClick(Sender: TObject);

    procedure LineColorBtnClick(Sender: TObject);
    procedure SeiteBtnClick(Sender: TObject);
    procedure AchternBtnClick(Sender: TObject);
    procedure TopBtnClick(Sender: TObject);
    procedure NullBtnClick(Sender: TObject);

    procedure MemoryBtnClick(Sender: TObject);
    procedure MemoryRecallBtnClick(Sender: TObject);

    procedure SofortBtnClick(Sender: TObject);
    procedure GrauBtnClick(Sender: TObject);
    procedure BlauBtnClick(Sender: TObject);
    procedure MultiBtnClick(Sender: TObject);

    procedure BogenBtnClick(Sender: TObject);
    procedure KoppelBtnClick(Sender: TObject);
    procedure HullBtnClick(Sender: TObject);

    procedure SuperSimpleBtnClick(Sender: TObject);
    procedure SuperNormalBtnClick(Sender: TObject);
    procedure SuperGrauBtnClick(Sender: TObject);
    procedure SuperBlauBtnClick(Sender: TObject);
    procedure SuperMultiBtnClick(Sender: TObject);
    procedure SuperDisplayBtnClick(Sender: TObject);
    procedure SuperQuickBtnClick(Sender: TObject);
  public
    procedure UpdateColorScheme;
    procedure LayoutComponents;
    function GetActionFromKey(Shift: TShiftState; Key: Word): Integer;
    function GetActionFromKeyChar(KeyChar: char): Integer;
    function GetChecked(fa: Integer): Boolean;
    procedure HandleAction(fa: Integer);
  public
    RotaForm: TRotaForm;
    procedure HandleSegment(fa: Integer);
  public
    Rigg: TRigg;
    ReportManager: TRggReportManager;
    FViewPoint: TViewPoint;
    procedure UpdateOnParamChanged;
    procedure UpdateOnParamValueChanged;
    procedure SetIsUp(const Value: Boolean);
    function GetIsUp: Boolean;
    procedure SetViewPoint(const Value: TViewPoint);
    property ViewPoint: TViewPoint read FViewPoint write SetViewPoint;
    property IsUp: Boolean read GetIsUp write SetIsUp;
    property CanShowMemo: Boolean read GetCanShowMemo;
  public
    Image: TImage;
    ImagePositionX: Integer;
    ImagePositionY: Integer;
    TextPositionX: Integer;
    TextPositionY: Integer;
    procedure UpdateFederText;
    procedure CenterRotaForm;
    procedure ToggleAllText;
  public
    SalingImage: TImage;
    SalingGraph: TSalingGraph;
    ControllerImage: TImage;
    ControllerGraph: TSalingGraph;
    ChartImage: TImage;
    ChartGraph: TChartGraph;
    ChartControl: TWinControl;
    procedure DoOnUpdateChart(Sender: TObject);
    procedure InitSalingGraph;
    procedure InitControllerGraph;
    procedure InitChartGraph;
    procedure UpdateSalingGraph;
    procedure UpdateControllerGraph;
    procedure UpdateChartGraph;
    procedure LayoutImages;
  protected
    procedure DestroyForms;
    procedure ShowDiagramA;
    procedure ShowDiagramC;
    procedure ShowDiagramE;
    procedure ShowDiagramQ;
    procedure ShowFormKreis;
    procedure ShowFormDetail;
    procedure ShowFormSplash;
    procedure ShowFormKraft;
    procedure ShowFormTabelle;
    procedure ShowFormSaling;
    procedure ShowFormController;
    procedure MemoBtnClick(Sender: TObject);
    procedure ActionsBtnClick(Sender: TObject);
    procedure ConfigBtnClick(Sender: TObject);
    procedure TrimmTabBtnClick(Sender: TObject);
    procedure CheckFormBounds(AForm: TForm);
  public
    StatusBar: TStatusBar;
    MainMenu: TMainMenu;

    FileMenu: TMenuItem;
    NewItem: TMenuItem;
    OpenItem: TMenuItem;
    SaveItem: TMenuItem;
    SaveAsItem: TMenuItem;
    N9: TMenuItem;
    ExitItem: TMenuItem;

    BearbeitenMenu: TMenuItem;
    RecalcItem: TMenuItem;
    BiegeNeigeItem: TMenuItem;
    ReglerItem: TMenuItem;
    MemoryItem: TMenuItem;
    MemoryRecallItem: TMenuItem;

    AnsichtMenu: TMenuItem;
    InputFormItem: TMenuItem;
    OutputFormItem: TMenuItem;
    ConfigItem: TMenuItem;
    TrimmTabItem: TMenuItem;

    N1: TMenuItem;
    SpeedBarItem: TMenuItem;
    StatusBarItem: TMenuItem;

    MemoMenu: TMenuItem;
    rLItem: TMenuItem;
    rLeItem: TMenuItem;
    rFItem: TMenuItem;
    rPItem: TMenuItem;
    rPeItem: TMenuItem;
    DiffLItem: TMenuItem;
    DiffPItem: TMenuItem;
    LogItem: TMenuItem;

    GrafikMenu: TMenuItem;
    VonDerSeiteItem: TMenuItem;
    VonHintenItem: TMenuItem;
    VonObenItem: TMenuItem;
    Von3DItem: TMenuItem;
    N3: TMenuItem;
    ReferenzItem: TMenuItem;
    EntlastetItem: TMenuItem;
    KoppelItem: TMenuItem;
    BogenItem: TMenuItem;

    OptionenMenu: TMenuItem;
    FestItem: TMenuItem;
    DrehbarItem: TMenuItem;
    OSBItem: TMenuItem;
    OSSItem: TMenuItem;
    N11: TMenuItem;
    ControllerItem: TMenuItem;
    DifferenzItem: TMenuItem;
    WinkelItem: TMenuItem;
    SofortItem: TMenuItem;
    N8: TMenuItem;
    QuerKraftItem: TMenuItem;
    KnickenItem: TMenuItem;
    KraftGemessenItem: TMenuItem;
    N2: TMenuItem;
    KorrigiertItem: TMenuItem;

    HelpMenu: TMenuItem;
    InfoItem: TMenuItem;
    LogoItem: TMenuItem;
    AboutItem: TMenuItem;
  private
    { File Menu }
    procedure NewItemClick(Sender: TObject);
    procedure OpenItemClick(Sender: TObject);
    procedure SaveItemClick(Sender: TObject);
    procedure SaveAsItemClick(Sender: TObject);
    procedure ExitItemClick(Sender: TObject);

    { Bearbeiten Menu }
    procedure BiegeNeigeItemClick(Sender: TObject);

    { Ansicht Menu }
    procedure InputFormItemClick(Sender: TObject);
    procedure OutputFormItemClick(Sender: TObject);
    procedure ConfigItemClick(Sender: TObject);
    procedure TrimmTabItemClick(Sender: TObject);

    procedure SpeedBarItemClick(Sender: TObject);
    procedure StatusBarItemClick(Sender: TObject);

    { Memo Menu }
    procedure rLItemClick(Sender: TObject);

    { Grafik Menu }
    procedure VonDerSeiteItemClick(Sender: TObject);

    { Optionen Menu }
    procedure SalingTypChanged(Sender: TObject);

    procedure WinkelItemClick(Sender: TObject);
    procedure SofortItemClick(Sender: TObject);
    procedure DifferenzItemClick(Sender: TObject);
    procedure KnickenItemClick(Sender: TObject);
    procedure KorrigiertItemClick(Sender: TObject);

    { Help Menu }
    procedure InfoItemClick(Sender: TObject);
    procedure LogoItemClick(Sender: TObject);
    procedure AboutItemClick(Sender: TObject);
  public
    procedure SetControllerChecked(Value: Boolean);
    procedure ControllerBtnClick(Sender: TObject);
    procedure UpdateBtnClick(Sender: TObject);
    procedure ReglerBtnClick(Sender: TObject);
  private
    FReportLabelCaption: string;
    procedure InitMenu;
    procedure InitStatusBar;
    procedure SetReportLabelCaption(const Value: string);
    property ReportLabelCaption: string read FReportLabelCaption write SetReportLabelCaption;
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

uses
  FrmMemo,
  FrmAction,
  FrmConfig,
  FrmTrimmTab,
  FrmDiagramA,
  FrmDiagramC,
  FrmDiagramE,
  FrmDiagramQ,
  FrmInfo,
  FrmKreis,
  FrmInput,
  FrmOutput,
  FrmSplash,
  FrmKraft,
  FrmDetail,
  FrmTabelle,
  FrmSaling,
  FrmController,
  RggModul,
  RggStrings,
  RiggVar.Util.AppUtils,
  RiggVar.VM.FormMain,
  RiggVar.RG.Main,
  RiggVar.RG.Speed01,
  RiggVar.RG.Speed02,
  RiggVar.RG.Speed03,
  RiggVar.RG.Speed04,
  RiggVar.App.Main,
  RiggVar.FB.ActionConst,
  RiggVar.FB.Classes;

const
  HelpCaptionText = 'RG68 press ? for help';
  ApplicationTitleText = 'RG68';

{ TFormMain }

procedure TFormMain.ApplicationEventsException(Sender: TObject; E: Exception);
begin
  if (Main <> nil) and (Main.Logger <> nil) then
    Main.Logger.Info(E.Message);
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  FormatSettings.DecimalSeparator := '.';

  SpeedColorScheme := TSpeedColorScheme.Create;
  SpeedColorScheme.InitDark;
  TActionSpeedBar.SpeedColorScheme := SpeedColorScheme;

  FormCreate2(Sender);
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  MainVar.AppIsClosing := True;

  FormDestroy2(Sender);

  SpeedColorScheme.Free;
end;

procedure TFormMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
  Screen.OnActiveFormChange := nil;
  // Application.HelpCommand(HELP_QUIT,0);
end;

procedure TFormMain.FormCreate2(Sender: TObject);
begin
{$ifdef Debug}
  ReportMemoryLeaksOnShutdown := True;
{$endif}

  FScale := 1.0;
{$ifdef MSWindows}
  FScale := ScaleFactor;
{$endif}

  Application.OnException := ApplicationEventsException;

  FormMain := self;
  InitScreenPos;

  Margin := Round(2 * FScale);
  Raster := Round(MainVar.Raster * FScale);
  MainVar.Scale := FScale;
  MainVar.ScaledRaster := Raster;
  TKR := Round(TKR * FScale);

  SpeedPanelHeight := Raster - Round(FScale * Margin);
  ListboxWidth := Round(230 * FScale);

  CreateComponents;

  SetupMemo(ReportText);
  SetupMemo(TrimmText);

  SetupListbox(ParamListbox);
  SetupListbox(ReportListbox);

  Rigg := TRigg.Create;
  Rigg.TrimmTabelle.FScale := FScale;
  Rigg.ControllerTyp := ctOhne;

  Main := TMain.Create(Rigg);
  Main.Logger.Verbose := True;
  Main.IsUp := True;

  RotaForm := TRotaForm.Create;
  RotaForm.Image := Image;
  RotaForm.Init;
  RotaForm.SwapRota(1);

  { Params }
  if ParamListbox <> nil then
  begin
    InitParamListbox;
    ParamListbox.OnClick := ParamListboxChange;
    ParamListbox.ItemIndex := ParamListbox.Items.IndexOf('Vorstag');
  end;

  { Reports }
  HL := TStringList.Create;
  RL := TStringList.Create;
  ReportManager := TRggReportManager.Create(RL);
  ReportManager.CurrentReport := rgDiffText;
  if ReportListbox <> nil then
  begin
    ReportManager.InitLB(ReportListbox.Items);
    ReportListbox.OnClick := ReportListboxChange;
    ReportListbox.ItemIndex := ReportListbox.Items.IndexOf(
    ReportManager.GetReportCaption(ReportManager.CurrentReport));
  end;

  TL := TStringList.Create;
  Main.UpdateTrimm0;
  ShowTrimm;

  Reset;

  InitSalingGraph;
  InitControllerGraph;
  InitChartGraph;

  Main.Draw;
  Main.MemoryBtnClick;

  Application.OnHint := HandleShowHint;
  InitSpeedButtons;
  UpdateSpeedButtonDown;
  UpdateSpeedButtonEnabled;
  UpdateColorScheme;

  SwapSpeedPanel(RotaForm.Current);

  Main.InitDefaultData;
  CenterRotaForm;
  Main.FixPoint := ooD0;
  Main.HullVisible := False;
  Main.OnUpdateChart := DoOnUpdateChart;

  InitMenu;
  InitStatusBar;

  InputForm := TInputForm.Create(Self);
  OutputForm := TOutputForm.Create(Self);

  RiggModul := TRiggModulA.Create(Rigg);
  RiggModul.ViewModelM := TViewModelMain00.Create;
  RiggModul.Init;

  Main.RiggModul := RiggModul;
  RiggModul.ViewModelM.IsUp := True;
  RiggModul.UpdateUI;

  OnCloseQuery := FormCloseQuery;
  Main.FederText.CheckState;
end;

procedure TFormMain.FormDestroy2(Sender: TObject);
begin
  DestroyForms;

  RiggModul.Free;

  TL.Free;
  RL.Free;
  HL.Free;
  ReportManager.Free;

  Main.Free;
  Main := nil;

  SalingGraph.Free;
  ControllerGraph.Free;
  ChartGraph.Free;

  RotaForm.Free;
end;

procedure TFormMain.FormKeyPress(Sender: TObject; var Key: Char);
var
  fa: Integer;
begin
  fa := GetActionFromKeyChar(Key);

  if fa <> faNoop then
  begin
    Main.ActionHandler.Execute(fa);
  end;
//  ShowTrimm;
end;

procedure TFormMain.UpdateOnParamChanged;
begin
  if (FormController <> nil) and (FormController.Visible = True) then
    FormController.UpdateGraph;

  if (FormDiagramA <> nil) and (FormDiagramA.Visible) then
  begin
    FormDiagramA.UpdateOnParamChanged;
  end;
end;

procedure TFormMain.UpdateOnParamValueChanged;
begin
  ShowTrimm;
  UpdateSalingGraph;
  UpdateControllerGraph;
  UpdateChartGraph;

  if (FormDetail <> nil) and (FormDetail.Visible = True) then
    FormDetail.AusgabeText;

  if (FormSaling <> nil) and (FormSaling.Visible = True) then
    FormSaling.UpdateGraph;

  if (FormController <> nil) and (FormController.Visible = True) then
    FormController.UpdateGraph;
end;

procedure TFormMain.UpdateReport;
begin
  if not FormShown then
    Exit;

  if ShowingHelp then
    Exit;
  if ReportText = nil then
    Exit;
  if not ReportText.Visible then
    Exit;
  if ReportManager = nil then
    Exit;
  if RL = nil then
    Exit;
  if not IsUp then
    Exit;

  RL.Clear;

  if WantButtonReport then
  begin
    Main.FederText.Report(RL);
    ReportText.Text := RL.Text;
  end
  else
  begin
    ReportManager.ShowCurrentReport;
    ReportText.Text := RL.Text;
  end;
end;

procedure TFormMain.UpdateFormat(w, h: Integer);
begin
  ClientWidth := w;
  ClientHeight := h;
  Flash(Format('%d x %d', [ClientWidth, ClientHeight]));
end;

procedure TFormMain.UpdateItemIndexParams;
begin
  UpdateItemIndexParamsLB;
  ShowTrimm;
end;

procedure TFormMain.UpdateItemIndexParamsLB;
var
  ii: Integer;
  ik: Integer;
begin
  if ParamListbox = nil then
    Exit;
  ii := ParamListbox.ItemIndex;
  ik := FindItemIndexOfParam(ParamListbox.Items);
  if ii <> ik then
  begin
    ParamListbox.OnClick := nil;
    ParamListbox.ItemIndex := ik;
    ParamListbox.OnClick := ParamListboxChange;
  end;
end;

function TFormMain.FindItemIndexOfParam(ML: TStrings): Integer;
var
  fp: TFederParam;
  i: Integer;
begin
  fp := Main.Param;
  result := -1;
  for i := 0 to ML.Count-1 do
  begin
    if TFederParam(ML.Objects[i]) = fp then
    begin
      result := i;
      break;
    end;
  end;
end;

procedure TFormMain.UpdateItemIndexReports;
var
  ii: Integer;
  ij: Integer;
begin
  if ReportListbox = nil then
    Exit;
  ii := ReportListbox.ItemIndex;
  ij := ReportManager.GetItemIndexOfReport(ReportManager.CurrentReport);
  if ii <> ij then
  begin
    ReportListbox.OnClick := nil;
    ReportListbox.ItemIndex := ij;
    ReportListbox.OnClick := ReportListboxChange;
  end;
end;

procedure TFormMain.UpdateItemIndexTrimms;
begin
end;

procedure TFormMain.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  Main.DoMouseWheel(Shift, WheelDelta);
  Handled := True;
end;

procedure TFormMain.FormShow(Sender: TObject);
begin
  if not FormShown then
  begin
    FormShown := True;
    UpdateParent;

    { ClientHeigt is now available }
    LayoutComponents;
    LayoutImages;

    UpdateSpeedButtonDown;
    UpdateReport;

    RotaForm.IsUp := True;
    RotaForm.Draw;

    FocusContainer.SetFocus;
  end;
end;

procedure TFormMain.FormResize(Sender: TObject);
begin
  if (Main <> nil) and Main.IsUp then
  begin
    MainVar.Scale := ScaleFactor;
    Inc(Main.ResizeCounter);
    Main.UpdateTouch;
    UpdateFederText;
  end;

  if FormShown then
  begin
    CheckSpaceForListbox;
    CheckSpaceForMemo;
    CheckSpaceForImages;

    UpdateReport;
    SpeedPanel.UpdateLayout;
  end;
end;

procedure TFormMain.CheckSpaceForListbox;
begin
  if not FormShown then
    Exit;
  ReportListbox.Visible := ClientHeight > 910 * FScale;
end;

procedure TFormMain.PlaceImage(PosLeft, PosTop: Integer);
begin
  Image.Left := PosLeft;
  Image.Top := PosTop;
  Image.Width := ClientWidth - Image.Left - Raster - Margin;
  Image.Height := ClientHeight - Image.Top - Raster - Margin - StatusBar.Height;
  if Image.Width > RotaForm.RotaForm1.BitmapWidth * FScale then
     Image.Width := Round(RotaForm.RotaForm1.BitmapWidth * FScale);
  if Image.Height > RotaForm.RotaForm1.BitmapHeight * FScale then
     Image.Height := Round(RotaForm.RotaForm1.BitmapHeight * FScale);
end;

procedure TFormMain.CheckSpaceForMemo;
var
  PosLeft, PosTop: Integer;
begin
  if not FormShown then
    Exit;
  if not ComponentsCreated then
    Exit;

  UpdateParent;

  if not CanShowMemo then
  begin
    if RotaForm.LegendItemChecked then
    begin
      RotaForm.LegendBtnClick(nil);
    end;

    SpeedPanel.Visible := False;

    TrimmText.Visible := False;
    ParamListbox.Visible := False;
    if ReportListbox <> nil then
      ReportListbox.Visible := False;

    ReportText.Visible := False;
    ReportText.Anchors := [];

    FocusContainer.Left := Raster + Margin;
    FocusContainer.Visible := Width - 2 * Raster > FocusContainer.Width;

    HintContainer.Left := 2 * Raster + Margin;
    HintContainer.Visible := Width - 2 * Raster > HintContainer.Width;

    PosLeft := Raster + Margin;
    PosTop := 2 * Raster + Margin;
    if HintContainer.Visible then
      PosTop := HintContainer.Top + HintContainer.Height + Margin;
    PlaceImage(PosLeft, PosTop);
  end
  else
  begin
    SpeedPanel.Visible := True;
    SpeedPanel.Width := ClientWidth - 3 * Raster - Margin;

    TrimmText.Visible := True;
    ParamListbox.Visible := True;
    ReportListbox.Visible := True;

    ReportListbox.Anchors := [];
    ReportListbox.Left := ParamListbox.Left;
    ReportListbox.Top := ParamListbox.Top + ParamListbox.Height + Margin;
    ReportListbox.Width := ParamListbox.Width;
    ReportListbox.Height := ClientHeight - ReportListbox.Top - Raster - Margin - StatusBar.Height;
    ReportListbox.Anchors := [TAnchorKind.akLeft, TAnchorKind.akTop, TAnchorKind.akBottom];

    FocusContainer.Visible := True;
    FocusContainer.Left := TrimmText.Left + TrimmText.Width + Margin;

    HintContainer.Visible := True;
    HintContainer.Left := FocusContainer.Left + FocusContainer.Width + Margin;

    ReportText.Visible := True;
    ReportText.Anchors := [];
    ReportText.Left := TrimmText.Left + TrimmText.Width + Margin;
    ReportText.Top := HintContainer.Top + HintContainer.Height + Margin;
    ReportText.Height := ClientHeight - ReportText.Top - Raster - Margin - StatusBar.Height;
    ReportText.Width := ReportMemoWidth;
    ReportText.Anchors := [TAnchorKind.akLeft, TAnchorKind.akTop, TAnchorKind.akBottom];

    PlaceImage(ImagePositionX, ImagePositionY);
  end;
end;

procedure TFormMain.CheckSpaceForImages;
begin
  if not ComponentsCreated then
    Exit;

  { At aplication start up FormResize is called serveral times,
    but always before FormShow always called. }

  { ClientWidth and ClientHeight are not yet available when starting up.
    ClientHeigt is available when FormShow is called. }

  if FormShown then
  begin
    { when FormResize is called after FormShow }
    if (ChartControl.Left + ChartControl.Width > ClientWidth - Raster) or
       (ChartControl.Top + ChartControl.Height > ClientHeight - Raster) then
      ChartControl.Visible := False;

    if (ControllerImage.BoundsRect.Left < ReportText.BoundsRect.Right) or
       (ControllerImage.BoundsRect.Bottom > ClientHeight - Raster) then
      ControllerImage.Visible := False;

    if (SalingImage.BoundsRect.Left < ReportText.BoundsRect.Right) or
       (SalingImage.BoundsRect.Bottom > ClientHeight - Raster) then
      SalingImage.Visible := False;
  end
  else
  begin
    { when FormResize is called before FormShow }
    if (Width < 1200) or (Height < 600) then
      ChartControl.Visible := False;
    if (Width < 1500) or (Height < 655) then
      ControllerImage.Visible := False;
    if (Width < 1500) or (Height < 875) then
      SalingImage.Visible := False;
  end;

  Main.FederText.CheckState;
  UpdateSpeedButtonDown;
end;

procedure TFormMain.Reset;
begin
  HelpTopic := faShowHelpText;
  DefaultCaption := ApplicationTitleText;
  Flash(DefaultCaption);
end;

procedure TFormMain.GotoNormal;
begin
  if WindowState = TWindowState.wsMaximized then
    WindowState := TWindowState.wsNormal;
end;

procedure TFormMain.GotoLandscape;
begin
  GotoNormal;
  if Screen.Width > Screen.Height then
  begin
    Height := Round(Screen.WorkAreaHeight);
    ClientWidth := Round(ClientHeight * 4 / 3);
    Top := 0;
  end
  else
  begin
    Width := Round(Screen.WorkAreaWidth);
    ClientHeight := Round(ClientWidth * 3 / 4);
    Left := 0;
  end;
  Flash('Landscape');
end;

procedure TFormMain.GotoPortrait;
begin
  GotoNormal;
  if Screen.Width > Screen.Height then
  begin
    Height := Round(Screen.WorkAreaHeight);
    ClientWidth := Round(ClientHeight * 3 / 4);
    Top := 0;
  end
  else
  begin
    Width := Round(Screen.WorkAreaWidth);
    ClientHeight := Round(ClientWidth * 4 / 3);
    Left := 0;
    Top := 0;
  end;
  Flash('Portrait');
end;

procedure TFormMain.GotoSquare;
begin
  GotoNormal;
  if Screen.Width > Screen.Height then
  begin
    Height := Round(Screen.WorkAreaHeight);
    ClientWidth := Round(ClientHeight);
    Top := 0;
  end
  else
  begin
    Width := Round(Screen.WorkAreaWidth);
    ClientHeight := Round(ClientWidth);
    Left := 0
  end;
  Flash('Square');
end;

procedure TFormMain.Flash(s: string);
begin
  Caption := s;
end;

procedure TFormMain.HandleShowHint(Sender: TObject);
begin
  HintText.Caption := Application.Hint;
  StatusBar.Panels[0].Text := Application.Hint;
end;

procedure TFormMain.HandleAction(fa: Integer);
begin
  case fa of
    faToggleAllText: ToggleAllText;
    faToggleSpeedPanel: ToggleSpeedPanel;

    faToggleHelp:
    begin
      ShowingHelp := not ShowingHelp;
      if ShowingHelp then
         ShowHelpText(HelpTopic)
      else
      UpdateReport;
    end;

    faMemeGotoLandscape: GotoLandscape;
    faMemeGotoPortrait: GotoPortrait;
    faMemeGotoSquare: GotoSquare;

    faToggleReport:
    begin
      Flash(HelpCaptionText);
      ShowingHelp := False;
      UpdateReport;
    end;

    faMemeFormat1: UpdateFormat(1000, 750);
    faMemeFormat2: UpdateFormat(800, 600);
    faMemeFormat3: UpdateFormat(640, 480);
    faMemeFormat4: UpdateFormat(480, 480);
    faMemeFormat5: UpdateFormat(512, 512);
    faMemeFormat6: UpdateFormat(600, 600);
    faMemeFormat7: UpdateFormat(700, 700);
    faMemeFormat8: UpdateFormat(800, 800);
    faMemeFormat9: UpdateFormat(900, 900);
    faMemeFormat0:
    begin
      Top := 0;
      UpdateFormat(750, 1000)
    end;

    faToggleButtonReport:
    begin
      FWantButtonReport := not WantButtonReport;
      UpdateReport;
    end;

    faReportNone..faReportReadme:
    begin
      ReportManager.HandleAction(fa);
      UpdateReport;
    end;

    faChartRect..faChartReset: ChartGraph.HandleAction(fa);

    faToggleLineColor: LineColorBtnClick(nil);

    faToggleSegmentF..faToggleSegmentA: HandleSegment(fa);

    faRggZoomIn: RotaForm.ZoomInBtnClick(nil);
    faRggZoomOut: RotaForm.ZoomOutBtnClick(nil);

    faToggleUseDisplayList:
    begin
      RotaForm.UseDisplayListBtnClick(nil);
      UpdateSpeedButtonEnabled;
    end;

    faToggleShowLegend: RotaForm.LegendBtnClick(nil);
    faToggleUseQuickSort: RotaForm.UseQuickSortBtnClick(nil);
    faToggleSalingGraph: SalingImageBtnClick(nil);
    faToggleControllerGraph: ControllerImageBtnClick(nil);
    faToggleChartGraph: ChartImageBtnClick(nil);
    faToggleMatrixText: RotaForm.MatrixItemClick(nil);

    faMemoryBtn: MemoryBtnClick(nil);
    faMemoryRecallBtn: MemoryRecallBtnClick(nil);

    faRggBogen: BogenBtnClick(nil);
    faRggKoppel: KoppelBtnClick(nil);
    faRggHull: HullBtnClick(nil);

    faSofortBtn: SofortBtnClick(nil);
    faGrauBtn: GrauBtnClick(nil);
    faBlauBtn: BlauBtnClick(nil);
    faMultiBtn: MultiBtnClick(nil);

    faSuperSimple: SuperSimpleBtnClick(nil);
    faSuperNormal: SuperNormalBtnClick(nil);
    faSuperGrau: SuperGrauBtnClick(nil);
    faSuperBlau: SuperBlauBtnClick(nil);
    faSuperMulti: SuperMultiBtnClick(nil);
    faSuperDisplay: SuperDisplayBtnClick(nil);
    faSuperQuick: SuperQuickBtnClick(nil);

    faShowMemo: MemoBtnClick(nil);
    faShowActions: ActionsBtnClick(nil);
    faShowConfig: ConfigBtnClick(nil);
    faShowTrimmTab: TrimmTabBtnClick(nil);

    faShowDiagA: ShowDiagramA;
    faShowDiagC: ShowDiagramC;
    faShowDiagE: ShowDiagramE;
    faShowDiagQ: ShowDiagramQ;

    faShowKreis: ShowFormKreis;
    faShowInfo: ShowInfo;
    faShowSplash: ShowFormSplash;
    faShowForce: ShowFormKraft;
    faShowDetail: ShowFormDetail;
    faShowTabelle: ShowFormTabelle;
    faShowSaling: ShowFormSaling;
    faShowController: ShowFormController;

    faToggleSandboxed: MainVar.IsSandboxed := MainConst.MustBeSandboxed or (not MainVar.IsSandboxed);
    faToggleAllProps: MainVar.AllProps := not MainVar.AllProps;
    faToggleAllTags: MainVar.AllTags := not MainVar.AllTags;

    faRotaForm1: SwapRota(1);
    faRotaForm2: SwapSpeedPanel(2); // SwapRota(2);
    faRotaForm3: SwapSpeedPanel(3); // SwapRota(3);

    faReset,
    faResetPosition,
    faResetRotation,
    faResetZoom: RotaForm.HandleAction(fa);

    faPan:
    begin
      Main.SetParameter(faPan);
      ShowTrimm;
    end;

    faShowZOrderInfo,
    faShowNormalKeyInfo,
    faShowSpecialKeyInfo,
    faShowDebugInfo,
    faShowInfoText,
    faShowHelpText: ShowHelpText(fa);

    else
    begin
      { do nothing }
    end;

  end;
  UpdateSpeedButtonDown;
end;

function TFormMain.GetActionFromKey(Shift: TShiftState; Key: Word): Integer;
begin
  result := faNoop;
  case Key of
    vkF12: result := faMemeGotoSquare;
//    vkC: result := faCopyTrimmItem;
//    vkV: result := faPasteTrimmItem;
    vkEscape:
    begin
      if Shift = [ssShift] then
        result := faResetPosition
      else
        result := faReset;
    end;
  end;
end;

function TFormMain.GetActionFromKeyChar(KeyChar: char): Integer;
var
  fa: Integer;
begin
  fa := faNoop;
  case KeyChar of
    'a': fa := faSalingA;
    'A': fa := faFixpointA0;

    'b': fa := faBiegung;
    'B': fa := faFixpointB0;

    'c': fa := faMastfallF0C;
    'C': fa := faFixpointC0;

    'd': fa := faFixpointD;
    'D': fa := faFixpointD0;

    'e': fa := faFixpointE;
    'E': fa := faFixpointE0;

    'f': fa := faMastfallF0F;
    'F': fa := faFixpointF0;

    'g': fa := faMastfallVorlauf;

    'h': fa := faSalingH;
    'H': fa := faToggleHelp;

    'i': fa := faWheelRight;
    'I': fa := faWheelLeft;

    'j': fa := faWheelUp;
    'J': fa := faWheelDown;

    'k': ;
    'K': fa := faRggKoppel;

    'l': fa := faToggleShowLegend;
    'L': fa := faMemeGotoLandscape;

    'm': fa := faMemoryBtn;
    'M': fa := faCopyAndPaste;

    'n': fa := faShowNormalKeyInfo;

    'o': fa := faWoben;

    'p': fa := faPan;
    'P': fa := faMemeGotoPortrait;

    'q': fa := faToggleAllText;

    'r': fa := faToggleReport;
    'R': fa := faReadTrimmFile;

    's': fa := faShowSpecialKeyInfo;
    'S': fa := faMemeGotoSquare;

    't': fa := faToggleFontColor;
    'T': fa := faToggleSpeedPanel;

    'u': fa := faToggleDataText;
    'U': fa := faToggleDiffText;

    'v': fa := faVorstag;

    'w': fa := faWante;

    'z': fa := faShowInfoText;
    'Z': fa := faUpdateTrimm0;

    '0': fa := faTrimm0;
    '1': fa := faTrimm1;
    '2': fa := faTrimm2;
    '3': fa := faTrimm3;
    '4': fa := faTrimm4;
    '5': fa := faTrimm5;
    '6': fa := faTrimm6;
    '7': fa := fa420;
    '8': fa := faLogo;
    '9': ;

    '!': fa := faShowNormalKeyInfo;
    '"': fa := faShowSpecialKeyInfo;
    '§': fa := faShowInfoText;
    '$': fa := faShowDebugInfo;
    '=': fa := faShowZOrderInfo;
    '?': fa := faShowHelpText;

    '+': fa := faActionPageP;
    '*': fa := faActionPageM;

    '#': fa := faActionPage4;

    ',': fa := faRotaForm1;
    '.': fa := faRotaForm2;
    '-': fa := faRotaForm3;

    ';': fa := faRotaForm1;
    ':': fa := faRotaForm2;
    '_': fa := faRotaForm3;

    else fa := faNoop;

  end;
  result := fa;
end;

function TFormMain.GetIsUp: Boolean;
begin
  if not MainVar.AppIsClosing and Assigned(Main) then
    result := Main.IsUp
  else
    result := False;
end;

procedure TFormMain.SetIsUp(const Value: Boolean);
begin
  Main.IsUp := Value;
end;

function TFormMain.GetOpenFileName(dn, fn: string): string;
begin
  if not Assigned(OpenDialog) then
    OpenDialog := TOpenDialog.Create(self);

  OpenDialog.Options := [
    TOpenOption.ofPathMustExist,
    TOpenOption.ofFileMustExist,
    TOpenOption.ofNoNetworkButton,
    TOpenOption.ofEnableSizing];
  OpenDialog.Filter := 'Trimm-File|*.txt|Trimm-Datei|*.trm';
  OpenDialog.InitialDir := ExcludeTrailingPathDelimiter(dn);
  OpenDialog.FileName := fn;

  if OpenDialog.Execute then
    result := OpenDialog.FileName
  else
    result := '';
end;

function TFormMain.GetSaveFileName(dn, fn: string): string;
begin
  if not Assigned(SaveDialog) then
    SaveDialog := TSaveDialog.Create(self);

  SaveDialog.Options := [
    TOpenOption.ofHideReadOnly,
    TOpenOption.ofPathMustExist,
    TOpenOption.ofNoReadOnlyReturn,
    TOpenOption.ofNoNetworkButton,
    TOpenOption.ofEnableSizing];
  SaveDialog.Filter := 'Trimm-File|*.txt|Trimm-Datei|*.trm';
  SaveDialog.InitialDir := ExcludeTrailingPathDelimiter(dn);
  SaveDialog.FileName := fn;

  if SaveDialog.Execute then
    result := SaveDialog.FileName
  else
    result := '';
end;

function TFormMain.GetShowDataText: Boolean;
begin
  result := ReportText.Visible and (ReportManager.CurrentReport = TRggReport.rgDataText);
end;

function TFormMain.GetShowDiffText: Boolean;
begin
  result := ReportText.Visible and (ReportManager.CurrentReport = TRggReport.rgDiffText);
end;

function TFormMain.GetShowTrimmText: Boolean;
begin
  result := ReportText.Visible and (ReportManager.CurrentReport = TRggReport.rgTrimmText);
end;

procedure TFormMain.ShowReport(const Value: TRggReport);
begin
  ReportText.Visible := True;
  ReportManager.CurrentReport := Value;
  UpdateReport;
  UpdateItemIndexReports;
end;

procedure TFormMain.SetShowDataText(const Value: Boolean);
begin
  if Value then
  begin
    ShowingHelp := False;
    ShowReport(TRggReport.rgDataText);
  end
  else
  begin
    ReportText.Visible := False;
  end;
end;

procedure TFormMain.SetShowDiffText(const Value: Boolean);
begin
  if Value then
  begin
    ShowingHelp := False;
    ShowReport(TRggReport.rgDiffText);
  end
  else
  begin
    ReportText.Visible := False;
  end;
end;

procedure TFormMain.SetShowTrimmText(const Value: Boolean);
begin
  if Value then
  begin
    ShowingHelp := False;
    ShowReport(TRggReport.rgTrimmText);
  end
  else
  begin
    ReportText.Visible := False;
  end;
end;

procedure TFormMain.SetupListbox(LB: TListBox);
begin
  if LB = nil then
    Exit;

  LB.Font.Name := 'Consolas';
  LB.Font.Size := 11;
  LB.Font.Color := TRggColors.Blue;
end;

procedure TFormMain.SetupMemo(MM: TMemo);
begin
  if MM = nil then
    Exit;

  MM.Parent := Self;
  MM.Font.Name := 'Consolas';
  MM.Font.Size := 11;
  MM.Font.Color := TRggColors.Teal;
  MM.ScrollBars := ssBoth;
end;

procedure TFormMain.CreateComponents;
begin
  StatusBar := TStatusBar.Create(Self);
  StatusBar.Parent := Self;

  FocusContainer := TButton.Create(Self);
  FocusContainer.Name := 'FocusContainer';
  FocusContainer.Parent := Self;
  FocusContainer.TabStop := True;
  FocusContainer.Caption := '';

  HintContainer := TWinControl.Create(Self);
  HintContainer.Name := 'HintContainer';
  HintContainer.Parent := Self;

  HintText := TLabel.Create(Self);
  HintText.Name := 'HintText';
  HintText.Parent := HintContainer;
  HintText.Font.Name := 'Consolas';
  HintText.Font.Size := 14;
  HintText.Font.Color := TRggColors.OrangeRed;
  HintText.AutoSize := True;
  HintText.WordWrap := False;

  ReportText := TMemo.Create(Self);
  ReportText.Name := 'ReportText';
  SetupMemo(ReportText);

  TrimmText := TMemo.Create(Self);
  SetupMemo(TrimmText);
  TrimmText.ReadOnly := True;
  TrimmText.TabStop := False;

  Image := TImage.Create(Self);
  Image.Name := 'Image';
  Image.Parent := Self;

  SpeedPanel01 := TActionSpeedBarRG01.Create(Self);
  SpeedPanel01.Name := 'SpeedPanel01';
  SpeedPanel01.Parent := Self;
  SpeedPanel01.ShowHint := True;
  SpeedPanel01.Visible := False;
  SpeedPanel01.Caption := '';

  SpeedPanel02 := TActionSpeedBarRG02.Create(Self);
  SpeedPanel02.Name := 'SpeedPanel02';
  SpeedPanel02.Parent := Self;
  SpeedPanel02.ShowHint := True;
  SpeedPanel02.Visible := False;
  SpeedPanel02.Caption := '';

  SpeedPanel03 := TActionSpeedBarRG03.Create(Self);
  SpeedPanel03.Name := 'SpeedPanel03';
  SpeedPanel03.Parent := Self;
  SpeedPanel03.ShowHint := True;
  SpeedPanel03.Visible := False;
  SpeedPanel03.Caption := '';

  SpeedPanel04 := TActionSpeedBarRG04.Create(Self);
  SpeedPanel04.Name := 'SpeedPanel04';
  SpeedPanel04.Parent := Self;
  SpeedPanel04.ShowHint := True;
  SpeedPanel04.Visible := False;
  SpeedPanel04.Caption := '';

  SpeedPanel := SpeedPanel03;
  SpeedPanel.Visible := True;

  ParamListbox := TListbox.Create(Self);
  ParamListbox.Name := 'ParamListbox';
  ParamListbox.Parent := Self;

  ReportListbox := TListbox.Create(Self);
  ReportListbox.Name := 'ReportListbox';
  ReportListbox.Parent := Self;

  ComponentsCreated := True;
end;

procedure TFormMain.ToggleSpeedPanel;
begin
  if SpeedPanel = SpeedPanel01 then
    SwapSpeedPanel(RotaForm.Current)
  else
    SwapSpeedPanel(0);
end;

procedure TFormMain.SwapSpeedPanel(Value: Integer);
begin
  SpeedPanel.Visible := False;

  case Value of
    1: SpeedPanel := SpeedPanel03;
    2: SpeedPanel := SpeedPanel04;
    3: SpeedPanel := SpeedPanel01;
  else
    SpeedPanel := SpeedPanel01;
  end;

  SpeedPanel.Width := ClientWidth - 3 * Raster - Margin;
  SpeedPanel.Visible := True;
  SpeedPanel.UpdateLayout;;
  SpeedPanel.UpdateSpeedButtonEnabled;
  SpeedPanel.UpdateSpeedButtonDown;
  SpeedPanel.DarkMode := MainVar.ColorScheme.IsDark;
  SpeedPanel.UpdateColor;

  if Main.RiggModul <> nil then
    Main.RiggModul.ViewModelM.UpdateView;
end;

procedure TFormMain.LayoutSpeedPanel(SP: TActionSpeedBar);
begin
  SP.Anchors := [];
  SP.Left := 2 * Raster + Margin;
  SP.Top := Raster + Margin;
  SP.Width := ClientWidth - 3 * Raster - 2 * Margin;
  SP.Height := SpeedPanelHeight;
  SP.Anchors := [TAnchorKind.akLeft, TAnchorKind.akTop, TAnchorKind.akRight];
  SP.UpdateLayout;
end;

procedure TFormMain.LayoutComponents;
begin
  if not ComponentsCreated then
    Exit;

  { ClientWidth and ClientHeight may still be at DesignTime Values, }
  { when called earlier than FormShow. }
  { Then it only 'works' if these values are big enough, }
  { so that computed values for Height and Width are > 0 }

  LayoutSpeedPanel(SpeedPanel01);
  LayoutSpeedPanel(SpeedPanel02);
  LayoutSpeedPanel(SpeedPanel03);
  LayoutSpeedPanel(SpeedPanel04);

  TrimmText.Left := Raster + Margin;
  TrimmText.Top := 2 * Raster + Margin;
  TrimmText.Width := ListboxWidth;
  TrimmText.Height := Round(190 * FScale);

  ParamListbox.Left := TrimmText.Left;
  ParamListbox.Top := TrimmText.Top + TrimmText.Height + Margin;
  ParamListbox.Width := ListboxWidth;
  ParamListbox.Height := Round(260 * FScale);

  ReportListbox.Left := ParamListbox.Left;
  ReportListbox.Top := ParamListbox.Top + ParamListbox.Height + Margin;
  ReportListbox.Width := ParamListbox.Width;
  ReportListbox.Height := ClientHeight - ReportListbox.Top - Raster - Margin - StatusBar.Height;
  ReportListbox.Anchors := ReportListbox.Anchors + [TAnchorKind.akBottom];

  FocusContainer.Left := TrimmText.Left + TrimmText.Width + Margin;
  FocusContainer.Top := TrimmText.Top;
  FocusContainer.Width := Round(40 * FScale);
  FocusContainer.Height := Round(40 * FScale);

  HintContainer.Left := FocusContainer.Left + FocusContainer.Width + Margin;
  HintContainer.Top := TrimmText.Top;
  HintContainer.Width := ReportMemoWidth - FocusContainer.Width - Margin;
  HintContainer.Height := Round(40 * FScale);

  HintText.Left := Round(10 * FScale);
  HintText.Top := Round(10 * FScale);

  ReportText.Left := TrimmText.Left + TrimmText.Width + Margin;
  ReportText.Top := HintContainer.Top + HintContainer.Height + Margin;
  ReportText.Height := ClientHeight - ReportText.Top - Raster - Margin - StatusBar.Height;
  ReportText.Width := ReportMemoWidth;
  ReportText.Anchors := ReportText.Anchors + [TAnchorKind.akBottom];
  ReportText.WordWrap := False;

  TextPositionX := ReportText.Left;
  TextPositionY := ReportText.Top;

  Image.Left := ReportText.Left + ReportText.Width + Margin;
  Image.Top := 2 * Raster + Margin;
  Image.Width := ClientWidth - Image.Left - Raster - Margin;
  Image.Height := ClientHeight - Image.Top - Raster - Margin - Statusbar.Height;
  Image.Anchors := Image.Anchors + [TAnchorKind.akRight, TAnchorKind.akBottom];
  ImagePositionX := Image.Left;
  ImagePositionY := Image.Top;

  SpeedPanel.Width := ClientWidth - 3 * Raster - Margin;
end;

procedure TFormMain.LineColorBtnClick(Sender: TObject);
begin
  RotaForm.WantLineColors := not RotaForm.WantLineColors;
  UpdateSpeedButtonDown;
  RotaForm.Draw;
end;

procedure TFormMain.HandleSegment(fa: Integer);
var
  b: Boolean;
begin
  b := RotaForm.GetChecked(fa);
  b := not b;
  RotaForm.SetChecked(fa, b);

  RotaForm.Draw;
end;

procedure TFormMain.SeiteBtnClick(Sender: TObject);
begin
  RotaForm.ViewPoint := vpSeite;
end;

procedure TFormMain.AchternBtnClick(Sender: TObject);
begin
  RotaForm.ViewPoint := vpAchtern;
end;

procedure TFormMain.TopBtnClick(Sender: TObject);
begin
  RotaForm.ViewPoint := vpTop;
end;

procedure TFormMain.NullBtnClick(Sender: TObject);
begin
  RotaForm.ViewPoint := vp3D;
end;

procedure TFormMain.ChartImageBtnClick(Sender: TObject);
begin
  ChartControl.Visible := not ChartControl.Visible;
  if ChartControl.Visible then
    ChartControl.BringToFront;

  ChartImage.Visible := ChartControl.Visible;

  if ChartControl.Visible and ChartImage.Visible then
    UpdateChartGraph;

  Main.FederText.PaintBackgroundNeeded := True;
  Main.FederText.Repaint;
end;

procedure TFormMain.SalingImageBtnClick(Sender: TObject);
begin
  SalingImage.Visible := not SalingImage.Visible;
  if SalingImage.Visible then
    SalingImage.BringToFront;
  if SalingImage.Visible then
    UpdateSalingGraph;
  Main.FederText.PaintBackgroundNeeded := True;
  Main.FederText.Repaint;
end;

procedure TFormMain.ControllerImageBtnClick(Sender: TObject);
begin
  ControllerImage.Visible := not ControllerImage.Visible;
  if ControllerImage.Visible then
    ControllerImage.BringToFront;
  if ControllerImage.Visible then
    UpdateControllerGraph;
  Main.FederText.PaintBackgroundNeeded := True;
  Main.FederText.Repaint;
end;

procedure TFormMain.InitSalingGraph;
begin
  SalingImage := TImage.Create(Self);
  SalingImage.Name := 'SalingImage';
  SalingImage.Parent := Self;
  SalingImage.Visible := False;

  SalingGraph := TSalingGraph.Create;
  SalingGraph.BackgroundColor := TRggColors.Antiquewhite;
  SalingGraph.ImageOpacity := 0.2;
  SalingGraph.SalingA := 850;
  SalingGraph.SalingH := 120;
  SalingGraph.SalingL := 479;
  SalingGraph.SalingHOffset := 37;
  SalingGraph.Image := SalingImage;
  UpdateSalingGraph;
end;

procedure TFormMain.UpdateSalingGraph;
begin
  if IsUp and SalingImage.Visible then
  begin
    SalingGraph.SalingA := Round(Main.ParamValue[fpSalingA]);
    SalingGraph.SalingH := Round(Main.ParamValue[fpSalingH]);
    SalingGraph.SalingL := Round(Main.ParamValue[fpSalingL]);
    SalingGraph.Draw(TFigure.dtSalingDetail);
  end;
end;

procedure TFormMain.InitControllerGraph;
begin
  ControllerImage := TImage.Create(Self);
  ControllerImage.Name := 'ControllerImage';
  ControllerImage.Parent := Self;
  ControllerImage.Visible := False;

  ControllerGraph := TSalingGraph.Create;
  ControllerGraph.BackgroundColor := MainVar.ColorScheme.claBackground;
  ControllerGraph.ImageOpacity := 0.2;

  ControllerGraph.ControllerTyp := TControllerTyp.ctDruck;
  ControllerGraph.EdgePos := 25;
  ControllerGraph.ControllerPos := 80;
  ControllerGraph.ParamXE := -20;
  ControllerGraph.ParamXE0 := 110;

  ControllerGraph.Image := ControllerImage;
  UpdateControllerGraph;
end;

procedure TFormMain.UpdateControllerGraph;
begin
  if IsUp and ControllerImage.Visible then
  begin
    ControllerGraph.ControllerTyp := Rigg.ControllerTyp;
    ControllerGraph.ControllerPos := Round(Main.ParamValue[fpController]);
    ControllerGraph.ParamXE := Rigg.MastPositionE;
    ControllerGraph.ParamXE0 := Round(Rigg.GetPoint3D(ooE0).X - Rigg.GetPoint3D(ooD0).X);
    ControllerGraph.EdgePos := Round(Rigg.RggFA.Find(fpController).Min);

    ControllerGraph.Draw(TFigure.dtController);
  end;
end;

procedure TFormMain.InitChartGraph;
begin
  ChartControl := TWinControl.Create(Self);
  ChartControl.Name := 'ChartControl';
  ChartControl.Parent := Self;
  ChartControl.Visible := False;

  ChartImage := TImage.Create(Self);
  ChartImage.Name := 'ChartImage';
  ChartImage.Parent := ChartControl;
  ChartImage.Visible := False;

  ChartGraph := TChartGraph.Create;
  ChartGraph.Image := ChartImage;

  UpdateChartGraph;
end;

procedure TFormMain.UpdateChartGraph;
begin
  if IsUp and ChartControl.Visible and ChartImage.Visible then
  begin
    ChartGraph.SuperCalc;
  end;
end;

procedure TFormMain.LayoutImages;
var
  PosX: Integer;
  PosY: Integer;
begin
  if not ComponentsCreated then
    Exit;

  PosX := ClientWidth - (Raster + Margin + ControllerImage.Width);
  PosY := SpeedPanel.Top + SpeedPanel.Height + Margin;

  ControllerImage.Left := PosX;
  ControllerImage.Top := PosY;
  ControllerImage.Anchors := [TAnchorKind.akTop, TAnchorKind.akRight];

  if Screen.Height > FScale * 1000 then
    PosY := PosY + ControllerImage.Height;

  SalingImage.Left := PosX;
  SalingImage.Top := PosY + ControllerImage.Height + Margin;
  SalingImage.Anchors := [TAnchorKind.akTop, TAnchorKind.akRight];

  ChartControl.Left := Round(ReportText.Left + 200 * FScale);
  ChartControl.Top := Round(ReportText.Top + 20 * FScale);
  ChartControl.Width := Round(ChartImage.Width);
  ChartControl.Height := Round(ChartImage.Height);
end;

procedure TFormMain.SetViewPoint(const Value: TViewPoint);
begin
  FViewPoint := Value;
  RotaForm.ViewPoint := Value;
end;

procedure TFormMain.ReportListboxChange(Sender: TObject);
var
  ii: Integer;
begin
  RL.Clear;
  ii := ReportListbox.ItemIndex;
  if (ii >= 0) and (ii <= Integer(High(TRggReport)))then
  begin
    ShowingHelp := False;
    ReportText.Visible := True;
    ReportManager.CurrentIndex := ii;
    UpdateReport;
    Main.FederText.CheckState;
  end;
  ReportLabelCaption := ReportManager.GetCurrentCaption;
end;

procedure TFormMain.ParamListboxChange(Sender: TObject);
begin
  if ParamListbox.ItemIndex <> -1 then
    Main.Param := Main.Text2Param(ParamListbox.Items[ParamListbox.ItemIndex]);
  ShowTrimm;
  UpdateControllerGraph;
  Main.FederText.CheckState;
end;

procedure TFormMain.InitParamListbox;
var
  rm: TMain;
  LI: TStrings;
  fp: TFederParam;
  s: string;

  procedure Add(fp: TFederParam);
  begin
    LI.AddObject(rm.Param2Text(fp), TObject(fp));
  end;
begin
  rm := Main;
  LI := ParamListbox.Items;
  LI.Clear;

  { Add a subset of available Params }
  Add(fpController);
  Add(fpWinkel);
  Add(fpVorstag);
  Add(fpWante);
  Add(fpWoben);
  Add(fpSalingH);
  Add(fpSalingA);
  Add(fpSalingL);
  Add(fpSalingW);
  Add(fpMastfallF0C);
  Add(fpMastfallF0F);
  Add(fpBiegung);
  Add(fpD0X);

  { Init ItemIndex }
  fp := rm.Param;
  s := rm.Param2Text(fp);
  ParamListbox.ItemIndex := LI.IndexOf(s);
end;

procedure TFormMain.ShowTrimmData;
begin
  RL.BeginUpdate;
  try
    RL.Clear;
    Main.CurrentTrimm.WantAll := MainVar.AllProps;
    Main.CurrentTrimm.SaveTrimmItem(RL);
    Main.CurrentTrimm.WantAll := False;
    if ReportLabel <> nil then
    begin
      ReportLabel.Caption := 'Trimm' + IntToStr(Main.Trimm);
    end;
  finally
    RL.EndUpdate;
  end;
end;

procedure TFormMain.ShowTrimm;
begin
  if TL <> nil then
  begin
    Main.UpdateTrimmText(TL);
    TrimmText.Text := TL.Text;
    UpdateFederText;
  end;
  UpdateReport;
end;

procedure TFormMain.SofortBtnClick(Sender: TObject);
begin
  Main.SofortBerechnen := not Main.SofortBerechnen;
  if Sender <> nil then
    Main.FederText.CheckState;
  UpdateReport;
end;

procedure TFormMain.GrauBtnClick(Sender: TObject);
begin
  Main.BtnGrauDown := not Main.BtnGrauDown;
  Main.RiggModul.UpdateUI;
end;

procedure TFormMain.BlauBtnClick(Sender: TObject);
begin
  Main.BtnBlauDown := not Main.BtnBlauDown;
  Main.RiggModul.UpdateUI;
end;

procedure TFormMain.MemoryBtnClick(Sender: TObject);
begin
  Main.MemoryBtnClick;
  UpdateReport;
end;

procedure TFormMain.MemoryRecallBtnClick(Sender: TObject);
begin
  Main.MemoryRecallBtnClick;
  ShowTrimm;
end;

procedure TFormMain.MultiBtnClick(Sender: TObject);
begin
  RotaForm.WantOverlayedRiggs := not RotaForm.WantOverlayedRiggs;
  Main.Draw;
  Main.RiggModul.UpdateUI;
end;

procedure TFormMain.BogenBtnClick(Sender: TObject);
begin
  Main.Bogen := not Main.Bogen;
  if Sender <> nil then
    Main.FederText.CheckState;
end;

procedure TFormMain.KoppelBtnClick(Sender: TObject);
begin
  Main.Koppel := not Main.Koppel;
  if Sender <> nil then
    Main.FederText.CheckState;
end;

procedure TFormMain.HullBtnClick(Sender: TObject);
begin
  Main.HullVisible := not Main.HullVisible;
  if Sender <> nil then
    Main.FederText.CheckState;
end;

function TFormMain.GetChecked(fa: Integer): Boolean;
begin
  result := false;
  if not IsUp then
    Exit;

  case fa of
    faToggleSandboxed: result := MainVar.IsSandboxed;
    faToggleAllProps: result := MainVar.AllProps;
    faToggleAllTags: result := MainVar.AllTags;

    faToggleHelp: result := ShowingHelp;
    faToggleReport: result := ReportText.Visible;
    faToggleButtonReport: result := WantButtonReport;
    faReportNone..faReportReadme: result := ReportManager.GetChecked(fa);
    faToggleSegmentF..faToggleSegmentA: result := RotaForm.GetChecked(fa);

    faToggleLineColor: result := RotaForm.WantLineColors;
    faToggleShowLegend: result := RotaForm.LegendItemChecked;

    faToggleUseDisplayList: result := RotaForm.UseDisplayList;
    faToggleUseQuickSort: result := RotaForm.UseQuickSort;

    faRggBogen: result := Main.Bogen;
    faRggKoppel: result := Main.Koppel;

    faSofortBtn: result := Main.SofortBerechnen;
    faGrauBtn: result := Main.BtnGrauDown;
    faBlauBtn: result := Main.BtnBlauDown;
    faMemoryBtn: result := False;
    faMultiBtn: result := RotaForm.WantOverlayedRiggs;

    faChartRect..faChartReset: result := ChartGraph.GetChecked(fa);
    faToggleChartGraph: result := ChartControl.Visible;
    faToggleSalingGraph: result := SalingImage.Visible;
    faToggleControllerGraph: result := ControllerImage.Visible;
    faToggleMatrixText: result := RotaForm.MatrixItemChecked;

    faRotaForm1: result := RotaForm.Current = 1;
    faRotaForm2: result := RotaForm.Current = 2;
    faRotaForm3: result := RotaForm.Current = 3;
  end;
end;

procedure TFormMain.CheckFormBounds(AForm: TForm);
begin
  if Screen.Height <= 768 then
    AForm.Top := 0;
  if Screen.Width <= 768 then
    AForm.Left := 0;
  if AForm.Left + AForm.Width > Screen.Width then
    AForm.Width := Screen.Width - AForm.Left - 20;
  if AForm.Top + AForm.Height > Screen.Height then
    AForm.Height := Screen.Width - AForm.Top - 20;
end;

procedure TFormMain.InitScreenPos;
begin
  if (Screen.Width >= FScale * 1920) and (Screen.Height >= FScale * 1024) then
  begin
    { Tested on normal HD screen }
    Left := Round(100 * FScale);
    Top := Round(30 * FScale);
    Width := Round(1700 * FScale);
    Height := Round(960 * FScale);
    ReportMemoWidth := Round(480 * FScale);
  end
  else
  begin
    { Tested on Microsoft Surface Tablet }
    Left := Round(20 * FScale);
    Top := Round(30 * FScale);
    Width := Round(1336 * FScale);
    Height := Round(800 * FScale);
    ReportMemoWidth := Round(320 * FScale);
  end;
end;

procedure TFormMain.MemoBtnClick(Sender: TObject);
begin
  if not Assigned(FormMemo) then
  begin
    FormMemo := TFormMemo.Create(nil);
    FormMemo.Parent := nil;
    FormMemo.Memo.Lines.Clear;
    CheckFormBounds(FormMemo);
  end;
  FormMemo.Visible := True;
end;

procedure TFormMain.ActionsBtnClick(Sender: TObject);
begin
  if not Assigned(FormAction) then
  begin
    FormAction := TFormAction.Create(nil);
    FormAction.Parent := nil;
    CheckFormBounds(FormAction);
  end;
  FormAction.Visible := True;
end;

procedure TFormMain.ConfigBtnClick(Sender: TObject);
begin
  if FormConfig = nil then
  begin
    FormConfig := TFormConfig.Create(Application);
    FormConfig.Parent := nil;
    FormConfig.Init(Rigg);
  end;

  { Istwerte in GSB aktualisieren für aktuelle Werte in Optionform }
  Rigg.UpdateGSB;
  FormConfig.ShowModal;
  if FormConfig.ModalResult = mrOK then
  begin
    Rigg.UpdateGlieder; { neue GSB Werte --> neue Integerwerte }
    Rigg.Reset; { neue Integerwerte --> neue Gleitkommawerte }
    Main.UpdateGetriebe;
    UpdateReport;
  end;
end;

procedure TFormMain.TrimmTabBtnClick(Sender: TObject);
begin
  if not Assigned(FormTrimmTab) then
  begin
    FormTrimmTab := TFormTrimmTab.Create(Application);
    FormTrimmTab.Parent := nil;
    FormTrimmTab.Init(Rigg);
  end;

  FormTrimmTab.ShowModal;
  if FormTrimmTab.ModalResult = mrOK then
  begin
    UpdateReport;
  end;
end;

procedure TFormMain.DestroyForms;
begin
  if FormAction <> nil then
  begin
    FormAction.Free;
    FormAction := nil;
  end;
  if FormMemo <> nil then
  begin
    FormMemo.Free;
    FormMemo := nil;
  end;
  if FormDiagramC <> nil then
  begin
    FormDiagramC.Free;
    FormDiagramC := nil;
  end;

  { Forms owned by Application not freed here. }
  { FormSplash is disposing of itself. }
end;

procedure TFormMain.InitSpeedButtons;
begin
  if SpeedPanel01 <> nil then
    SpeedPanel01.InitSpeedButtons;

  if SpeedPanel02 <> nil then
    SpeedPanel02.InitSpeedButtons;

  if SpeedPanel03 <> nil then
    SpeedPanel03.InitSpeedButtons;

  if SpeedPanel04 <> nil then
    SpeedPanel04.InitSpeedButtons;
end;

procedure TFormMain.UpdateSpeedButtonDown;
begin
  if SpeedPanel <> nil then
    SpeedPanel.UpdateSpeedButtonDown;
end;

procedure TFormMain.UpdateSpeedButtonEnabled;
begin
  if SpeedPanel <> nil then
    SpeedPanel.UpdateSpeedButtonEnabled;
end;

procedure TFormMain.UpdateColorScheme;
begin
  if not ComponentsCreated then
    Exit;

  RotaForm.BackgroundColor := MainVar.ColorScheme.claBackground;

  ControllerGraph.BackgroundColor := MainVar.ColorScheme.claBackground;
  UpdateControllerGraph;

  SalingGraph.BackgroundColor := MainVar.ColorScheme.claBackground;
  UpdateSalingGraph;

  RotaForm.DarkMode := MainVar.ColorScheme.IsDark;
end;

procedure TFormMain.SuperSimpleBtnClick(Sender: TObject);
begin
  RotaForm.UseDisplayList := False;
  RotaForm.WantOverlayedRiggs := False;
  Main.GraphRadio := gSimple;
end;

procedure TFormMain.SuperNormalBtnClick(Sender: TObject);
begin
  RotaForm.UseDisplayList := False;
  RotaForm.WantOverlayedRiggs := False;
  Main.GraphRadio := gNormal;
end;

procedure TFormMain.SuperGrauBtnClick(Sender: TObject);
begin
  RotaForm.UseDisplayList := False;
  RotaForm.WantOverlayedRiggs := True;
  Main.GraphRadio := gGrau;
end;

procedure TFormMain.SuperBlauBtnClick(Sender: TObject);
begin
  RotaForm.UseDisplayList := False;
  RotaForm.WantOverlayedRiggs := True;
  Main.GraphRadio := gBlau;
end;

procedure TFormMain.SuperMultiBtnClick(Sender: TObject);
begin
  RotaForm.UseDisplayList := False;
  RotaForm.WantOverlayedRiggs := True;
  Main.GraphRadio := gMulti;
end;

procedure TFormMain.SuperDisplayBtnClick(Sender: TObject);
begin
  RotaForm.UseDisplayList := True;
  RotaForm.WantOverlayedRiggs := False;
  RotaForm.UseQuickSort := False;
  Main.GraphRadio := gDisplay;
end;

procedure TFormMain.SuperQuickBtnClick(Sender: TObject);
begin
  RotaForm.UseDisplayList := True;
  RotaForm.WantOverlayedRiggs := False;
  RotaForm.UseQuickSort := True;
  Main.GraphRadio := gQuick;
end;

procedure TFormMain.UpdateParent;
var
  ft: TWinControl;
begin
  ft := Main.FederText;
  Image.Parent := ft;
  SalingImage.Parent := ft;
  ControllerImage.Parent := ft;
  ReportText.Parent := ft;
  ParamListbox.Parent := ft;
  ReportListbox.Parent := ft;
  TrimmText.Parent := ft;
  FocusContainer.Parent := ft;
  HintContainer.Parent := ft;
  SpeedPanel01.Parent := ft;
  SpeedPanel02.Parent := ft;
  SpeedPanel03.Parent := ft;
  SpeedPanel04.Parent := ft;
end;

procedure TFormMain.InitZOrderInfo;
var
  i: Integer;
  c: TControl;
begin
  for i := 0 to Self.ControlCount-1 do
  begin
    c := Controls[i];
    HL.Add(Format('%2d - %s: %s', [i, c.Name, c.ClassName]));
  end;
end;

procedure TFormMain.InitDebugInfo;
begin
  HL.Add('Window-Info:');
  HL.Add(Format('  Initial-Client-W-H = (%d, %d)', [ClientWidth, ClientHeight]));
  HL.Add(Format('  Handle.Scale = %.1f', [MainVar.Scale]));
end;

procedure TFormMain.ShowHelpText(fa: Integer);
begin
  HL.Clear;

  case fa of
    faShowHelpText: Main.FederBinding.InitSplashText(HL);
    faShowInfoText: Main.FederBinding.InitInfoText(HL);
    faShowNormalKeyInfo: Main.FederBinding.InitNormalKeyInfo(HL);
    faShowSpecialKeyInfo: Main.FederBinding.InitSpecialKeyInfo(HL);
    faShowDebugInfo: InitDebugInfo;
    faShowZOrderInfo: InitZOrderInfo;
  end;

  ShowingHelp := True;
  ReportText.Text := HL.Text;
  ReportText.Visible := True;
end;

procedure TFormMain.SwapRota(Value: Integer);
begin
  RotaForm.SwapRota(Value);
  RotaForm.BackgroundColor := MainVar.ColorScheme.claBackground;
  RotaForm.DarkMode := MainVar.ColorScheme.IsDark;
  SwapSpeedPanel(RotaForm.Current);
end;

procedure TFormMain.DoOnUpdateChart(Sender: TObject);
begin
  if (ChartGraph <> nil) and (Main.Param = fpAPW)  then
  begin
    ChartGraph.APWidth := Round(Main.CurrentValue);
    ChartGraph.UpdateXMinMax;
  end;
end;

procedure TFormMain.UpdateFederText;
begin
  if Main.Action = faPan then
  begin
    Main.FederText1.ST00.Text.Caption := 'Pan';
    Main.FederText.SB00.Text.Caption := '';
  end
  else
  begin
    Main.FederText.ST00.Text.Caption := Main.ParamCaption;
    Main.FederText.SB00.Text.Caption := Main.ParamValueString[Main.Param];
  end;
end;

procedure TFormMain.CenterRotaForm;
begin
  RotaForm.InitPosition(Width, Height, 0, 0);
  if FormShown then
    RotaForm.Draw;
end;

procedure TFormMain.ToggleSpeedPanelFontSize;
begin
  SpeedPanel.ToggleBigMode;
  LayoutComponents;
  CheckSpaceForMemo;
  CheckSpaceForImages;
end;

procedure TFormMain.ToggleAllText;
var
  b: Boolean;
begin
  if not Main.FederText.Visible then
    Exit;

  if Main.FederText = Main.FederText2 then
    Exit;

  if not CanShowMemo then
    Exit;

  b := not ParamListbox.Visible;

  SpeedPanel.Visible := b;
  FocusContainer.Visible := b;
  TrimmText.Visible := b;
  ParamListbox.Visible := b;
  ReportListbox.Visible := b;
  ReportText.Visible := b;
end;

function TFormMain.GetCanShowMemo: Boolean;
begin
  result := True;

  if (ClientWidth < 900 * FScale) then
    result := False;

  if (ClientHeight < 700 * FScale) then
    result := False;

  if Main.IsPhone then
    result := False;
end;

procedure TFormMain.ShowDiagramE;
begin
  if not Assigned(FormDiagramE) then
  begin
    FormDiagramE := TFormDiagramE.Create(Application);
  end;
  FormDiagramE.Show;
end;

procedure TFormMain.ShowDiagramQ;
begin
  if not Assigned(FormDiagramQ) then
  begin
    FormDiagramQ := TFormDiagramQ.Create(Application);
  end;
  FormDiagramQ.Show;
end;

procedure TFormMain.ShowDiagramC;
begin
  if not Assigned(FormDiagramC) then
  begin
    FormDiagramC := TFormDiagramC.Create(nil);
    FormDiagramC.Parent := nil;
    FormDiagramC.ChartModel := ChartGraph;
    ChartGraph.OnActionHandled := FormDiagramC.UpdateUI;

    if not ChartControl.Visible then
    begin
      Main.FederText.ActionPage := 9;
      ChartImageBtnClick(nil);
      UpdateSpeedButtonDown;
    end;
  end;

  FormDiagramC.Visible := True;
end;

procedure TFormMain.ShowFormKreis;
begin
  if not Assigned(KreisForm) then
  begin
    KreisForm := TKreisForm.Create(Application);
  end;
  KreisForm.Show;
end;

procedure TFormMain.ShowDiagramA;
begin
  if not Assigned(FormDiagramA) then
  begin
    FormDiagramA := TFormDiagramA.Create(Application);
  end;
  FormDiagramA.Show;
end;

procedure TFormMain.ShowFormTabelle;
begin
  if not Assigned(FormTabelle) then
  begin
    FormTabelle := TFormTabelle.Create(Application);
  end;
  FormTabelle.Show;
end;

procedure TFormMain.ShowFormDetail;
begin
  if not Assigned(FormDetail) then
  begin
    FormDetail := TFormDetail.Create(Application);
  end;
  FormDetail.Show;
end;

procedure TFormMain.ShowFormController;
begin
  if not Assigned(FormController) then
  begin
    FormController := TFormController.Create(Application);
  end;
  FormController.Show;
  FormController.UpdateGraph;
end;

procedure TFormMain.ShowFormSaling;
begin
  if not Assigned(FormSaling) then
  begin
    FormSaling := TFormSaling.Create(Application);
  end;
  FormSaling.Show;
  FormSaling.UpdateGraph;
end;

procedure TFormMain.ShowFormKraft;
begin
  if not Assigned(FormKraft) then
  begin
    FormKraft := TFormKraft.Create(Application);
  end;
  FormKraft.Show;
end;

procedure TFormMain.ShowFormSplash;
begin
  if not Assigned(FormSplash) then
  begin
    FormSplash := TFormSplash.Create(Application);
    FormSplash.Show;
  end;
end;

procedure TFormMain.InitMenu;
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
  if MainMenu = nil then
    MainMenu := TMainMenu.Create(Self)
  else
    MainMenu.Items.Clear;

  { File }

  FileMenu := AddP('FileMenu');
  mi.Caption := '&Datei';
  mi.Hint := '  Dateibefehle';

  NewItem := AddI('NewItem');
  mi.Caption := '&Neu';
  mi.Hint := '  Standardwerte laden';
  mi.OnClick := NewItemClick;

  OpenItem := AddI('OpenItem');
  mi.Caption := '&Öffnen ...';
  mi.Hint := '  Konfiguration aus Datei laden';
  mi.OnClick := OpenItemClick;

  SaveItem := AddI('SaveItem');
  mi.Caption := '&Speichern';
  mi.Hint := '  Konfiguration in Datei speichern';
  mi.OnClick := SaveItemClick;

  SaveAsItem := AddI('SaveAsItem');
  mi.Caption := 'Speichern &unter ...';
  mi.Hint := '  Konfiguration in neue Datei schreiben';
  mi.OnClick := SaveAsItemClick;

  N9 := AddI('N9');
  mi.Caption := '-';

  ExitItem := AddI('ExitItem');
  mi.Caption := '&Beenden';
  mi.Hint := '  Anwendung verlassen';
  mi.ShortCut := 32856;
  mi.OnClick := ExitItemClick;

  { Bearbeiten }

  BearbeitenMenu := AddP('BearbeitenMenu');
  mi.Caption := '&Bearbeiten';
  mi.GroupIndex := 2;
  mi.Hint := '  Bearbeitungsbefehle';

  RecalcItem := AddI('RecalcItem');
  mi.Caption := 'Neu &berechnen ( = )';
  mi.Hint := '  Rigg neu berechnen';
  mi.OnClick := UpdateBtnClick;

  BiegeNeigeItem := AddI('BiegeNeigeItem');
  mi.Caption := 'Biegen und &Neigen ...';
  mi.Hint := '  Mastbiegung und Mastfall einstellen';
  mi.OnClick := BiegeNeigeItemClick;

  ReglerItem := AddI('ReglerItem');
  mi.Caption := 'Trimm &regeln ... ( R )';
  mi.Hint := '  Trimm automatisch einstellen';
  mi.OnClick := ReglerBtnClick;

  MemoryItem := AddI('MemoryItem');
  mi.Caption := 'Trimm &speichern ( M )';
  mi.Hint := '  Trimm in den Zwischenspeicher kopieren';
  mi.OnClick := MemoryBtnClick;

  MemoryRecallItem := AddI('MemoryRecallItem');
  mi.Caption := 'Trimm &zurücksetzen ( MR )';
  mi.Hint := '  Trimm aus dem Zwischenspeicher zurückholen';
  mi.OnClick := MemoryRecallBtnClick;

  { Ansicht }

  AnsichtMenu := AddP('AnsichtMenu');
  mi.Caption := '&Ansicht';
  mi.GroupIndex := 2;
  mi.Hint := '  Fenster anzeigen und verbergen';

  InputFormItem := AddI('InputItem');
  mi.Caption := '&Eingabe ...';
  mi.Hint := '  Eingabeseiten anzeigen';
  mi.ShortCut := 16453;
  mi.OnClick := InputFormItemClick;

  OutputFormItem := AddI('OutputFormItem');
  mi.Caption := '&Ausgabe ...';
  mi.Hint := '  Ausgabeseiten anzeigen';
  mi.ShortCut := 16449;
  mi.OnClick := OutputFormItemClick;

  ConfigItem := AddI('ConfigItem');
  mi.Caption := '&Konfiguration ...';
  mi.Hint := '  Konstanten und Parameter verändern';
  mi.ShortCut := 16459;
  mi.OnClick := ConfigItemClick;

  TrimmTabItem := AddI('TrimmTabItem');
  mi.Caption := '&Trimm Tabelle ...';
  mi.Hint := '  Edit TrimmTab Properties';
  mi.OnClick := TrimmTabItemClick;

  N1 := AddI('N1');
  mi.Caption := '-';

  SpeedBarItem := AddI('SpeedBarItem');
  mi.Caption := 'Symbolleiste';
  mi.Checked := True;
  mi.Hint := '  Symbolleiste einblenden';
  mi.OnClick := SpeedBarItemClick;

  StatusBarItem := AddI('StatusBarItem');
  mi.Caption := 'Statusleiste';
  mi.Checked := True;
  mi.Hint := '  Statusleiste einblenden';
  mi.OnClick := StatusBarItemClick;

  { Tabellen }

  MemoMenu := AddP('MemoMenu');
  mi.Caption := '&Tabellen';
  mi.GroupIndex := 3;
  mi.Hint := '  Tabelle für Anzeige im Memo auswählen';

  rLItem := AddI('rLItem');
  mi.Caption := 'rL';
  mi.Hint := '  Längen (Rigg verformt) anzeigen';
  mi.RadioItem := True;
  mi.ShortCut := 16460;
  mi.OnClick := rLItemClick;

  rLeItem := AddI('rLeItem');
  mi.Caption := 'rLe';
  mi.Hint := '  Längen (Rigg entspannt) anzeigen';
  mi.RadioItem := True;
  mi.OnClick := rLItemClick;

  rFItem := AddI('rFItem');
  mi.Caption := 'rF';
  mi.Checked := True;
  mi.Hint := '  Kräfte anzeigen';
  mi.RadioItem := True;
  mi.ShortCut := 16454;
  mi.OnClick := rLItemClick;

  rPItem := AddI('rPItem');
  mi.Caption := 'rP';
  mi.Hint := '  Koordinaten (Rigg verformt) anzeigen';
  mi.RadioItem := True;
  mi.ShortCut := 16464;
  mi.OnClick := rLItemClick;

  rPeItem := AddI('rPeItem');
  mi.Caption := 'rPe';
  mi.Hint := '  Koordinaten (Rigg entlastet) anzeigen';
  mi.RadioItem := True;
  mi.OnClick := rLItemClick;

  DiffLItem := AddI('DiffLItem');
  mi.Caption := 'Diff_L';
  mi.Hint := '  Längendifferenzen (entlastet - belastet) anzeigen';
  mi.RadioItem := True;
  mi.OnClick := rLItemClick;

  DiffPItem := AddI('DiffPItem');
  mi.Caption := 'Diff_P';
  mi.Hint := '  Punktverschiebungen (entlastet - belastet) anzeigen';
  mi.RadioItem := True;
  mi.OnClick := rLItemClick;

  LogItem := AddI('LogItem');
  mi.Caption := 'Log';
  mi.Hint := '  Log anzeigen';
  mi.RadioItem := True;
  mi.OnClick := rLItemClick;

  { Grafik }

  GrafikMenu := AddP('GrafikMenu');
  mi.Caption := '&Grafik';
  mi.GroupIndex := 3;
  mi.Hint := '  2D Grafikoptionen ';

  VonDerSeiteItem := AddI('VonDerSeiteItem');
  mi.Caption := 'Seitenansicht';
  mi.Checked := True;
  mi.Hint := '  Rigg von der Seite gesehen darstellen';
  mi.RadioItem := True;
  mi.OnClick := VonDerSeiteItemClick;

  VonHintenItem := AddI('VonHintenItem');
  mi.Caption := 'Blick von Achtern';
  mi.Hint := '  Rigg von hinten gesehen darstellen';
  mi.RadioItem := True;
  mi.OnClick := VonDerSeiteItemClick;

  VonObenItem := AddI('VonObenItem');
  mi.Caption := 'Draufsicht';
  mi.Hint := '  Rigg von oben gesehen darstellen';
  mi.RadioItem := True;
  mi.OnClick := VonDerSeiteItemClick;

  Von3DItem := AddI('Von3DItem');
  mi.Caption := 'Perspektive';
  mi.Hint := '  Rigg schräg von oben gesehen darstellen';
  mi.RadioItem := True;
  mi.OnClick := VonDerSeiteItemClick;

  N3 := AddI('N3');
  mi.Caption := '-';

  ReferenzItem := AddI('ReferenzItem');
  mi.Caption := 'Referenzstellung';
  mi.GroupIndex := 1;
  mi.Hint := '  Nullstellung einblenden';
  mi.OnClick := BlauBtnClick;

  EntlastetItem := AddI('EntlastetItem');
  mi.Caption := 'Entspanntes Rigg';
  mi.Checked := True;
  mi.GroupIndex := 1;
  mi.Hint := '  Entspanntes Rigg einblenden';
  mi.OnClick := GrauBtnClick;

  KoppelItem := AddI('KoppelItem');
  mi.Caption := 'Koppelkurve';
  mi.Checked := True;
  mi.GroupIndex := 1;
  mi.Hint := '  Koppelkurve einblenden';
  mi.OnClick := KoppelBtnClick;

  BogenItem := AddI('BogenItem');
  mi.Caption := 'Bogen';
  mi.GroupIndex := 1;
  mi.Hint := '  Mast als Bogen oder Zweischlag zeichnen';
  mi.OnClick := BogenBtnClick;

  { Modell }

  OptionenMenu := AddP('OptionenMenu');
  mi.Caption := '&Modell';
  mi.GroupIndex := 3;
  mi.Hint := '  Modell - und Berechnungsoptionen';

  FestItem := AddI('FestItem');
  mi.Caption := 'feste Salinge';
  mi.Checked := True;
  mi.Hint := '  Modell: Salinge starr befestigt';
  mi.RadioItem := True;
  mi.OnClick := SalingTypChanged;

  DrehbarItem := AddI('DrehbarItem');
  mi.Caption := 'drehbare Salinge';
  mi.Hint := '  Modell: Salinge drehbar angelenkt';
  mi.RadioItem := True;
  mi.OnClick := SalingTypChanged;

  OSBItem := AddI('OSBItem');
  mi.Caption := 'ohne Salinge / Mast biegt aus';
  mi.Hint := '  Modell: Biegeknicken des Mastes ohne Salinge';
  mi.RadioItem := True;
  mi.OnClick := SalingTypChanged;

  OSSItem := AddI('OSSItem');
  mi.Caption := 'ohne Saling / Mast starr';
  mi.Hint := '  Modell: Mast steif ohne Salinge';
  mi.RadioItem := True;
  mi.OnClick := SalingTypChanged;

  N11 := AddI('N11');
  mi.Caption := '-';
  mi.GroupIndex := 1;

  ControllerItem := AddI('ControllerItem');
  mi.Caption := 'Controller ( C )';
  mi.Checked := True;
  mi.GroupIndex := 1;
  mi.Hint := '  Mastcontroller berücksichtigen';
  mi.ShortCut := 16451;
  mi.OnClick := ControllerBtnClick;

  DifferenzItem := AddI('DifferenzItem');
  mi.Caption := 'Differenzen ( D )';
  mi.GroupIndex := 1;
  mi.Hint := '  Länge als Differenz oder Absolutwert anzeigen';
  mi.ShortCut := 16452;
  mi.OnClick := DifferenzItemClick;

  WinkelItem := AddI('WinkelItem');
  mi.Caption := 'Winkel einstellbar ( W )';
  mi.GroupIndex := 1;
  mi.Hint := ' Wanten-Winkel oder Vorstaglänge einstellen';
  mi.OnClick := WinkelItemClick;

  SofortItem := AddI('SofortItem');
  mi.Caption := 'Rigg automatisch berechnen ( A )';
  mi.Checked := False;
  mi.GroupIndex := 1;
  mi.Hint := '  Rigg (Kräfte) automatisch berechnen';
  mi.OnClick := SofortItemClick;

  N8 := AddI('N8');
  mi.Caption := '-';
  mi.GroupIndex := 2;

  QuerKraftItem := AddI('QuerKraftItem');
  mi.Caption := 'QuerKraftBiegung';
  mi.GroupIndex := 2;
  mi.Hint := '  Kraftberechnung nur mit Querkraftbiegung - kein Knicken';
  mi.RadioItem := True;
  mi.OnClick := KnickenItemClick;

  KnickenItem := AddI('KnickenItem');
  mi.Caption := 'Biegeknicken';
  mi.Checked := True;
  mi.GroupIndex := 2;
  mi.Hint := '  Biegeknicken bei der Kraftberechnung berücksichtigen';
  mi.RadioItem := True;
  mi.OnClick := KnickenItemClick;

  KraftGemessenItem := AddI('KraftGemessenItem');
  mi.Caption := 'gemessene Kraftwerte verwenden';
  mi.GroupIndex := 2;
  mi.Hint := '  Kräfte aus der Trimmtabelle entnehmen';
  mi.RadioItem := True;
  mi.OnClick := KnickenItemClick;

  N2 := AddI('N2');
  mi.Caption := '-';
  mi.GroupIndex := 3;

  KorrigiertItem := AddI('KorrigiertItem');
  mi.Caption := 'BiegeKnicken korrigiert';
  mi.Checked := True;
  mi.GroupIndex := 3;
  mi.Hint := '  Anteil der Salingkraft an der Mastbiegung beachten';
  mi.OnClick := KorrigiertItemClick;

  { Help }

  HelpMenu := AddP('HelpMenu');;
  mi.Caption := '&Hilfe';
  mi.GroupIndex := 10;
  mi.Hint := '  Hilfethemen';
  mi.Enabled := True;

//  HilfeItem := AddI('HilfeItem');
//  mi.Caption := '&Hilfe ...';
//  mi.Hint := '  Hilfesystem starten';
//  mi.Enabled := False;

  InfoItem := AddI('InfoItem');
  mi.Caption := '&Info ...';
  mi.Hint := '  Infofenster anzeigen';
  mi.OnClick := InfoItemClick;

  AboutItem := AddI('AboutItem');
  mi.Caption := 'About ...';
  mi.Hint := '  KreisForm.ShowModal';
  mi.OnClick := AboutItemClick;

  LogoItem := AddI('LogoItem');
  mi.Caption := 'Logo';
  mi.Hint := '  Toggle between Logo and 420 (Reset)';
  mi.OnClick := LogoItemClick;
end;

procedure TFormMain.NewItemClick(Sender: TObject);
var
  DialogValue: Integer;
  FName: string;
begin
  if (Caption <> 'Rigg') and Main.Modified then
  begin
    FName := Caption;
    DialogValue := MessageDlg(Format(SWarningText, [FName]), mtConfirmation, [mbYes, mbNo, mbCancel], 0);
    case DialogValue of
      mrYes: SaveItemClick(Sender);
      mrCancel: Exit;
    end;
  end;
  Main.Neu(nil);
end;

procedure TFormMain.OpenItemClick(Sender: TObject);
var
  DialogValue: Integer;
  FName: string;
begin
  if Main.Modified then
  begin
    FName := Caption;
    DialogValue := MessageDlg(Format(SWarningText, [FName]), mtConfirmation, mbYesNoCancel, 0);
    case DialogValue of
      mrYes: SaveItemClick(Sender);
      mrCancel: Exit;
    end;
  end;

  if not Assigned(OpenDialog) then
    OpenDialog := TOpenDialog.Create(self);

  InitOpenDialog;

  if OpenDialog.Execute then
    Main.Open(OpenDialog.FileName);
end;

procedure TFormMain.SaveItemClick(Sender: TObject);
begin
  if Main.IniFileName = '' then
    SaveAsItemClick(Sender)
  else
    Main.Save;
end;

procedure TFormMain.SaveAsItemClick(Sender: TObject);
begin
  if not Assigned(SaveDialog) then
    SaveDialog := TSaveDialog.Create(self);

  InitSaveDialog;

  SaveDialog.FileName := Main.IniFileName;
  if SaveDialog.Execute then
  begin
    Main.IniFileName := SaveDialog.FileName;
    SaveItemClick(Sender);
  end;
end;

procedure TFormMain.ExitItemClick(Sender: TObject);
begin
  Close;
end;

procedure TFormMain.InitOpenDialog;
begin
  OpenDialog.DefaultExt := 'rgi';
  OpenDialog.Filter := 'Rigg Ini File (*.rgi)|*.rgi|Alle Dateien (*.*)|*.*';
  OpenDialog.FilterIndex := 1;
  OpenDialog.Options := [ofOverwritePrompt, ofPathMustExist, ofFileMustExist];
end;

procedure TFormMain.InitSaveDialog;
begin
  SaveDialog.DefaultExt := 'rgi';
  SaveDialog.Filter := 'Rigg Ini File (*.rgi)|*.rgi|Alle Dateien (*.*)|*.*';
  SaveDialog.FilterIndex := 1;
  SaveDialog.Options := [ofOverwritePrompt, ofPathMustExist];
end;

procedure TFormMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
  DialogValue: Integer;
  FName: string;
begin
  if not Main.AutoSave then
  begin
    Main.IsUp := False;
    CanClose := True;
    FormShown := False;
    Exit;
  end;

  if Main.Modified then
  begin
    FName := Caption;
    DialogValue := MessageDlg(Format(SWarningText, [FName]), mtConfirmation,
      mbYesNoCancel, 0);
    case DialogValue of
      mrYes:
        begin
          SaveItemClick(Sender);
          CanClose := not Main.Modified;
        end;
      mrNo:
        CanClose := True;
      mrCancel:
        CanClose := False;
    end;
  end;
end;

procedure TFormMain.VonDerSeiteItemClick(Sender: TObject);
var
  ViewPoint: TViewPoint;
begin
  VonDerSeiteItem.Checked := False;
  VonHintenItem.Checked := False;
  VonObenItem.Checked := False;
  Von3DItem.Checked := False;
  if Sender = VonDerSeiteItem then
  begin
    ViewPoint := vpSeite;
    VonDerSeiteItem.Checked := True;
  end
  else if Sender = VonHintenItem then
  begin
    ViewPoint := vpAchtern;
    VonHintenItem.Checked := True;
  end
  else if Sender = VonObenItem then
  begin
    ViewPoint := vpTop;
    VonObenItem.Checked := True;
  end
  else // if Sender = Von3DItem then
  begin
    ViewPoint := vp3D;
    Von3DItem.Checked := True;
  end;

  Main.ViewPoint := ViewPoint;
end;

procedure TFormMain.WinkelItemClick(Sender: TObject);
begin
  WinkelItem.Checked := not WinkelItem.Checked;
//  WinkelBtn.Down := WinkelItem.Checked;
//  RiggModul.WinkelBtnDown := WinkelBtn.Down;
end;

procedure TFormMain.DifferenzItemClick(Sender: TObject);
begin
  DifferenzItem.Checked := not DifferenzItem.Checked;
//  DiffBtn.Down := DifferenzItem.Checked;
//  RiggModul.DiffBtnDown := DiffBtn.Down;
end;

procedure TFormMain.SofortItemClick(Sender: TObject);
begin
//  SofortItem.Checked := not SofortItem.Checked;
//  SofortBtn.Down := SofortItem.Checked;
//  RiggModul.SofortBtnDown := SofortBtn.Down;
//  if SofortItem.Checked then
//  begin
//    PaintItem.Enabled := True;
//    PaintBtn.Enabled := True;
//  end
//  else
//  begin
//    StatusBar.Panels[1].Text := Main.Rigg.GetriebeStatusText;
//    PaintItem.Checked := False;
//    PaintItem.Enabled := False;
//    PaintBtn.Down := False;
//    PaintBtn.Enabled := False;
//  end;
//  Main.Draw;
end;

procedure TFormMain.SetControllerChecked(Value: Boolean);
begin
  ControllerItem.Checked := Value;
//  ControllerBtn.Down := Value;
  RiggModul.ControllerBtnDown := Value;
  RotaForm.RotaForm1.ControllerTyp := Main.Rigg.ControllerTyp;
//  RotaForm.RaumGraph.ControllerTyp := RiggModul.ControllerTyp;
  RotaForm.Draw;
end;

procedure TFormMain.ControllerBtnClick(Sender: TObject);
begin
  SetControllerChecked(not ControllerItem.Checked);
end;

procedure TFormMain.KnickenItemClick(Sender: TObject);
begin
  if Sender = QuerKraftItem then
    Main.Rigg.CalcTyp := ctQuerKraftBiegung
  else if Sender = KnickenItem then
    Main.Rigg.CalcTyp := ctBiegeKnicken
  else if Sender = KraftGemessenItem then
    Main.Rigg.CalcTyp := ctKraftGemessen;

  Main.RiggModul.CalcTyp := Main.Rigg.CalcTyp;
end;

procedure TFormMain.KorrigiertItemClick(Sender: TObject);
begin
  KorrigiertItem.Checked := not KorrigiertItem.Checked;
  Main.Korrigiert := KorrigiertItem.Checked;
end;

procedure TFormMain.LogoItemClick(Sender: TObject);
begin
  if Main.Trimm <> 8 then
    Main.ActionHandler.Execute(faLogo)
  else
    Main.ActionHandler.Execute(fa420);

  WantLogoData := Main.Trimm = 8;
  LogoItem.Checked := WantLogoData;
//  RiggModul.Neu(nil);
//  RiggModul.UpdateGetriebe;
end;

procedure TFormMain.UpdateBtnClick(Sender: TObject);
begin
  RiggModul.UpdateBtnClick;
end;

procedure TFormMain.BiegeNeigeItemClick(Sender: TObject);
begin
  RiggModul.BiegeNeigeItemClick;
end;

procedure TFormMain.ReglerBtnClick(Sender: TObject);
begin
  RiggModul.ReglerBtnClick;
  Main.StrokeRigg.Bogen := False;
end;

procedure TFormMain.ConfigItemClick(Sender: TObject);
begin
  ConfigBtnClick(nil);
end;

procedure TFormMain.TrimmTabItemClick(Sender: TObject);
begin
  TrimmTabBtnClick(nil);
end;

procedure TFormMain.AboutItemClick(Sender: TObject);
begin
  if KreisForm = nil then
    KreisForm := TKreisForm.Create(Application);

  KreisForm.ShowModal;
end;

procedure TFormMain.InfoItemClick(Sender: TObject);
begin
  FrmInfo.ShowInfo;
end;

procedure TFormMain.InputFormItemClick(Sender: TObject);
begin
  InputFormItem.Checked := not InputFormItem.Checked;
  if InputFormItem.Checked then
  begin
    if InputForm = nil then
    begin
      InputForm := TInputForm.Create(Self);
    end;

    InputForm.Parent := nil;
    InputForm.BorderStyle := bsSizeable;
    InputForm.ClientHeight := Round(195 * FScale);
    InputForm.ClientWidth := Round(465 * FScale);
    InputForm.Show;
  end
  else
    InputForm.Hide;
end;

procedure TFormMain.OutputFormItemClick(Sender: TObject);
begin
  OutputFormItem.Checked := not OutputFormItem.Checked;
  if OutputFormItem.Checked then
  begin
    if OutputForm = nil then
    begin
      OutputForm := TOutputForm.Create(Self);
    end;

    OutputForm.Parent := nil;
    OutputForm.BorderStyle := bsSizeable;
    OutputForm.ClientHeight := Round(255 * FScale);
    OutputForm.ClientWidth := Round(465 * FScale);
    OutputForm.Show;
    if OutputForm.YComboBox.ItemIndex = -1 then
      OutputForm.YComboBox.ItemIndex := RiggModul.YComboSavedItemIndex;
  end
  else
    OutputForm.Hide;
end;

procedure TFormMain.SpeedBarItemClick(Sender: TObject);
begin
  SpeedBarItem.Checked := not SpeedBarItem.Checked;
  Main.FederText.PaintBackgroundNeeded := True;
  SpeedPanel.Visible := SpeedBarItem.Checked;
//  SpeedPanel.Visible := not SpeedPanel.Visible;
end;

procedure TFormMain.StatusBarItemClick(Sender: TObject);
begin
  StatusBarItem.Checked := not StatusBarItem.Checked;
  StatusBar.Visible := StatusBarItem.Checked;
end;

procedure TFormMain.rLItemClick(Sender: TObject);
var
  i: Integer;
  Item: TReportItem;
begin
  for i := 0 to MemoMenu.Count - 1 do
    MemoMenu.Items[i].Checked := False;

  if Sender = rLItem then
  begin
    Item := rL_Item;
    rLItem.Checked := True;
  end
  else if Sender = rLeItem then
  begin
    Item := rLe_Item;
    rLeItem.Checked := True;
  end
  else if Sender = rPItem then
  begin
    Item := rP_Item;
    rPItem.Checked := True;
  end
  else if Sender = rPeItem then
  begin
    Item := rPe_Item;
    rPeItem.Checked := True;
  end
  else if Sender = rFItem then
  begin
    Item := rF_Item;
    rFItem.Checked := True;
  end
  else if Sender = DiffLItem then
  begin
    Item := DiffL_Item;
    DiffLItem.Checked := True;
  end
  else if Sender = DiffPItem then
  begin
    Item := DiffP_Item;
    DiffPItem.Checked := True;
  end
  else //if Sender = LogItem then
  begin
    Item := Log_Item;
    LogItem.Checked := True;
  end;

  RiggModul.ReportItem := Item;
end;

procedure TFormMain.SalingTypChanged(Sender: TObject);
begin
//  if Sender = FestItem then
//    Main.Rigg.SalingTyp := stFest
//  else if Sender = DrehbarItem then
//    Main.Rigg.SalingTyp := stDrehbar
//  else if Sender = OhneBiegtItem then
//    Main.Rigg.SalingTyp := stOhneBiegt
//  else if Sender = OhneStarrItem then
//    Main.Rigg.SalingTyp := stOhneStarr;

  if Sender = FestItem then
    Main.RiggModul.FestItemClick
  else if Sender = DrehbarItem then
    Main.RiggModul.DrehbarItemClick
  else if Sender = OSBItem then
    Main.RiggModul.OSBItemClick
  else if Sender = OSSItem then
    Main.RiggModul.OSSItemClick
end;

procedure TFormMain.InitStatusBar;
var
  sp: TStatusPanel;
begin
  StatusBar.Panels.Clear;

  sp := StatusBar.Panels.Add;
  sp.Text := 'MenuText';
  sp.Width := 353;

  sp := StatusBar.Panels.Add;
  sp.Text := 'RiggText';
  sp.Width := 300;

  sp := StatusBar.Panels.Add;
  sp.Text := 'ReportLabel';
  sp.Width := 50;
end;

procedure TFormMain.SetReportLabelCaption(const Value: string);
begin
  FReportLabelCaption := Value;
  StatusBar.Panels[2].Text := Value;
end;

end.
