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
  RiggVar.RG.Def,
  RiggVar.RG.Report,
  RiggVar.FB.SpeedBar,
  RiggVar.FB.Color,
  RiggVar.RG.Graph,
  RggTypes,
  RggUnit4,
  System.SysUtils,
  System.Classes,
  System.Types,
  System.UITypes,
  RggDisplayTypes,
  RggDisplay,
  RggStrings,
  RggRaumGraph,
  RggRota,
  RggCtrls,
  RggChartGraph,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.Dialogs,
  Vcl.Graphics;

type

  { TFormMain }

  TFormMain = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
  private
    procedure UpdateFormat(w, h: Integer);
    procedure GotoLandscape;
    procedure GotoNormal;
    procedure GotoPortrait;
    procedure GotoSquare;
  private
    FScale: single;
    DefaultCaption: string;
    FormShown: Boolean;
    procedure ApplicationEventsException(Sender: TObject; E: Exception);
    procedure HandleShowHint(Sender: TObject);
    procedure Flash(s: string);
    procedure Reset;
    procedure PlaceImage(PosLeft, PosTop: Integer);
  protected
    HL: TStringList;
    RL: TStrings;
    TL: TStrings;
    procedure InitHelpText;
    procedure InitParamListbox;
    procedure InitTrimmCombo;
    procedure InitParamCombo;
    procedure InitReportCombo;
  public
    AllProps: Boolean;
    procedure ShowTrimm;
    procedure ShowTrimmData;
  public
    FWantButtonFrameReport: Boolean;
    procedure UpdateReport;
    procedure UpdateBackgroundColor(AColor: TColor);
    property WantButtonFrameReport: Boolean read FWantButtonFrameReport;
  public
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    function GetOpenFileName(dn, fn: string): string;
    function GetSaveFileName(dn, fn: string): string;
  public
    HintContainer: TWinControl;
    HintText: TLabel;
    ReportText: TMemo;
    TrimmText: TMemo;
    ParamListbox: TListBox;
    ReportListbox: TListBox;
    ReportLabel: TLabel;
    TrimmCombo: TComboBox;
    ParamCombo: TComboBox;
    ReportCombo: TComboBox;
    function FindItemIndexOfParam(ML: TStrings): Integer;
    procedure UpdateItemIndexParams;
    procedure UpdateItemIndexParamsLB;
    procedure UpdateItemIndexParamsCB;
    procedure UpdateItemIndexReports;
    procedure UpdateItemIndexTrimms;
    procedure ParamListboxChange(Sender: TObject);
    procedure ReportListboxChange(Sender: TObject);
    procedure TrimmComboChange(Sender: TObject);
    procedure ParamComboChange(Sender: TObject);
    procedure ReportComboChange(Sender: TObject);
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
    procedure CreateComponents;
    procedure CheckSpaceForImages;
    procedure CheckSpaceForMemo;
    procedure CheckSpaceForListbox;
    procedure SetupMemo(MM: TMemo);
    procedure SetupCombobox(CB: TComboBox);
    procedure SetupListbox(LB: TListBox);
    procedure UpdateParent;
  public
    Raster: Integer;
    Margin: Integer;
    ListboxWidth: Integer;
    ReportMemoWidth: Integer;
    SpeedPanelHeight: Integer;
    SpeedPanel: TActionSpeedBar;
    SpeedPanel1: TActionSpeedBar;
    SpeedPanel2: TActionSpeedBar;
    procedure InitSpeedButtons;
    procedure LayoutSpeedPanel(SP: TActionSpeedBar);
    procedure UpdateSpeedButtonDown;
    procedure UpdateSpeedButtonEnabled;
    procedure ToggleSpeedPanel;
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
    function GetActionFromKey(Key: Word): Integer;
    function GetActionFromKeyChar(KeyChar: char): Integer;
    function GetChecked(fa: Integer): Boolean;
    procedure HandleAction(fa: Integer);
  public
    DL: TRggDisplayList;
    RotaForm: TRotaForm;
    StrokeRigg: IStrokeRigg;
    procedure HandleSegment(fa: Integer);
  public
    Rigg: TRigg;
    ReportManager: TRggReportManager;
    FViewPoint: TViewPoint;
    procedure UpdateOnParamValueChanged;
    procedure SetIsUp(const Value: Boolean);
    function GetIsUp: Boolean;
    procedure SetViewPoint(const Value: TViewPoint);
    property ViewPoint: TViewPoint read FViewPoint write SetViewPoint;
    property IsUp: Boolean read GetIsUp write SetIsUp;
  public
    Image: TImage;
    ImagePositionX: Integer;
    ImagePositionY: Integer;
    TextPositionX: Integer;
    TextPositionY: Integer;
  public
    SalingImage: TImage;
    SalingGraph: TSalingGraph;
    ControllerImage: TImage;
    ControllerGraph: TSalingGraph;
    ChartControl: TWinControl;
    ChartImage: TImage;
    ChartGraph: TChartGraph;
    procedure InitSalingGraph;
    procedure InitControllerGraph;
    procedure InitChartGraph;
    procedure UpdateSalingGraph;
    procedure UpdateControllerGraph;
    procedure UpdateChartGraph;
    procedure LayoutImages;
  protected
    procedure ShowDiagramC;
    procedure ShowDiagramE;
    procedure ShowDiagramQ;
    procedure DestroyForms;
    procedure MemoBtnClick(Sender: TObject);
    procedure ActiBtnClick(Sender: TObject);
    procedure ConfigBtnClick(Sender: TObject);
    procedure TrimmTabBtnClick(Sender: TObject);
    procedure CheckFormBounds(AForm: TForm);
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
  FrmDiagramC,
  FrmDiagramE,
  FrmDiagramQ,
  RiggVar.RG.Main,
  RiggVar.RG.Speed01,
  RiggVar.RG.Speed02,
  RiggVar.RG.Speed03,
  RiggVar.App.Main,
  RiggVar.FB.ActionConst,
  RiggVar.FB.Classes;

const
  HelpCaptionText = 'press h for help';
  ApplicationTitleText = 'RG67';

{ TFormMain }

procedure TFormMain.ApplicationEventsException(Sender: TObject; E: Exception);
begin
  if (Main <> nil) and (Main.Logger <> nil) then
    Main.Logger.Info(E.Message);
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
{$ifdef Debug}
  ReportMemoryLeaksOnShutdown := True;
{$endif}
  FormatSettings.DecimalSeparator := '.';

  FScale := 1.0;
{$ifdef MSWindows}
  FScale := ScaleFactor;
{$endif}

  Application.OnException := ApplicationEventsException;

  FormMain := self;
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

  Margin := Round(2 * FScale);
  Raster := Round(MainVar.Raster * FScale);
  MainVar.Scale := FScale;
  MainVar.ScaledRaster := Raster;

  SpeedPanelHeight := Raster - Round(FScale * Margin);
  ListboxWidth := Round(200 * FScale);

  CreateComponents;

  SetupMemo(ReportText);
  SetupMemo(TrimmText);

  SetupListbox(ParamListbox);
  SetupListbox(ReportListbox);

  Rigg := TRigg.Create;
  Rigg.TrimmTab.FScale := FScale;
  Rigg.ControllerTyp := ctOhne;

  Main := TMain.Create(Rigg);
  Main.Logger.Verbose := True;
  Main.InitLogo; // sets WantLogoData to true
  Main.Init420; // resets WantLogoData to false

  Main.Trimm := 1;

  Main.InitText;
  Main.IsUp := True;

  RotaForm := TRotaForm.Create;
  StrokeRigg := RotaForm;
  Main.StrokeRigg := RotaForm;
  RotaForm.Image := Image;
  RotaForm.Init;
  DL := RotaForm.RaumGraph.DL;
  RotaForm.ViewPoint := vp3D;
  RotaForm.ZoomIndex := 8;
  RotaForm.FixPoint := ooD0;
  { Set initial translation in RotatForm, FXPos and FYPos, default. }

  { Params }
  Main.Param := fpVorstag;
  if ParamListbox <> nil then
  begin
    InitParamListbox;
    ParamListbox.OnClick := ParamListboxChange;
    ParamListbox.ItemIndex := ParamListbox.Items.IndexOf('Vorstag');
  end;
  if ParamCombo <> nil then
  begin
    InitParamCombo;
    ParamCombo.ItemIndex := ParamCombo.Items.IndexOf('Vorstag');
    ParamCombo.OnChange := ParamComboChange;
  end;

  { Reports }
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
  if ReportCombo <> nil then
  begin
    InitReportCombo;
    ReportCombo.OnChange := ReportComboChange;
    ReportManager.InitLB(ReportListbox.Items);
  end;

  if TrimmCombo <> nil then
  begin
    InitTrimmCombo;
    TrimmCombo.ItemIndex := 0;
    TrimmCombo.OnChange := TrimmComboChange;
  end;

  HL := TStringList.Create;
  InitHelpText;

  TL := TStringList.Create;
  Main.UpdateTrimm0;
  ShowTrimm;

  Reset;

  InitSalingGraph;
  InitControllerGraph;
  InitChartGraph;

  Main.ChartGraph := ChartGraph;
  Main.Draw;
  Main.MemoryBtnClick;
  Main.FederText.CheckState;

  Application.OnHint := HandleShowHint;
  InitSpeedButtons;
  UpdateSpeedButtonDown;
  UpdateSpeedButtonEnabled;
  UpdateColorScheme;
  Main.FederText1.SendToBack;
  Main.FederText2.SendToBack;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  DestroyForms;

  HL.Free;
  TL.Free;
  RL.Free;
  ReportManager.Free;

  Main.Free;
  Main := nil;

  SalingGraph.Free;
  ControllerGraph.Free;
  ChartGraph.Free;
end;

procedure TFormMain.FormKeyPress(Sender: TObject; var Key: char);
var
  fa: Integer;
begin
  fa := GetActionFromKeyChar(Key);
  if fa <> faNoop then
  begin
    Main.ActionHandler.Execute(fa);
  end;
  ShowTrimm;
end;

procedure TFormMain.UpdateOnParamValueChanged;
begin
  ShowTrimm;
  UpdateSalingGraph;
  UpdateControllerGraph;
  UpdateChartGraph;
end;

procedure TFormMain.UpdateReport;
begin
  if not FormShown then
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

  if WantButtonFrameReport then
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
  UpdateItemIndexParamsCB;
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

procedure TFormMain.UpdateItemIndexParamsCB;
var
  ii: Integer;
  ik: Integer;
begin
  if ParamCombo = nil then
    Exit;
  ii := ParamCombo.ItemIndex;
  ik := FindItemIndexOfParam(ParamCombo.Items);
  if ii <> ik then
  begin
    ParamCombo.OnClick := nil;
    ParamCombo.ItemIndex := ik;
    ParamCombo.OnClick := ParamComboChange;
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
var
  ii: Integer;
  ij: Integer;
begin
  if TrimmCombo = nil then
    Exit;
  ii := TrimmCombo.ItemIndex;
  ij := Main.Trimm-1;
  if ii <> ij then
  begin
    TrimmCombo.OnChange := nil;
    TrimmCombo.ItemIndex := ij;
    TrimmCombo.OnChange := TrimmComboChange;
  end;
end;

procedure TFormMain.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  if (ssShift in Shift) or (ssCtrl in Shift) then
  begin
    Main.DoMouseWheel(Shift, WheelDelta);
    Handled := True;
  end;
end;

procedure TFormMain.FormShow(Sender: TObject);
begin
  if not FormShown then
  begin
    FormShown := True;

    { ClientHeigt is now available }
    LayoutComponents;
    LayoutImages;

    RotaForm.IsUp := True;
    RotaForm.Draw;
  end;
end;

procedure TFormMain.FormResize(Sender: TObject);
begin
  //if not FormShown then
  //   Exit;

  { will be done via Resize and UpdateTouch }
  //MainVar.ClientWidth := ClientWidth;
  //MainVar.ClientHeight := ClientHeight;

  if (Main <> nil) and Main.IsUp then
  begin
    Inc(Main.ResizeCounter);
    Main.UpdateTouch;
    Main.UpdateText;
  end;

  if FormShown then
  begin
    SpeedPanel.UpdateLayout;
    UpdateReport;
    CheckSpaceForListbox;
    CheckSpaceForMemo;
    CheckSpaceForImages;
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
  Image.Height := ClientHeight - Image.Top - Raster - Margin;
  if Image.Width > RotaForm.BitmapWidth * FScale then
     Image.Width := Round(RotaForm.BitmapWidth * FScale);
  if Image.Height > RotaForm.BitmapHeight * FScale then
     Image.Height := Round(RotaForm.BitmapHeight * FScale);
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

  if (ClientWidth < 900 * FScale) or (ClientHeight < 700 * FScale) then
  begin
    if RotaForm.LegendItemChecked then
    begin
      RotaForm.LegendBtnClick(nil);
    end;
    TrimmText.Visible := False;
    ParamListbox.Visible := False;
    if ReportListbox <> nil then
      ReportListbox.Visible := False;

    ReportText.Visible := False;
    ReportText.Anchors := [];

    HintContainer.Left := Raster + Margin;
    HintContainer.Visible := Width - 2 * Raster > HintContainer.Width;

    PosLeft := Raster + Margin;
    PosTop := 2 * Raster + Margin;
    if HintContainer.Visible then
      PosTop := HintContainer.Top + HintContainer.Height + Margin;
    PlaceImage(PosLeft, PosTop);
  end
  else
  begin
    TrimmText.Visible := True;
    ParamListbox.Visible := True;
    ReportListbox.Visible := True;

    HintContainer.Visible := True;
    HintContainer.Left := TrimmText.Left + TrimmText.Width + Margin;

    ReportText.Visible := True;
    ReportText.Anchors := [];
    ReportText.Left := TrimmText.Left + TrimmText.Width + Margin;
    ReportText.Top := HintContainer.Top + HintContainer.Height + Margin;
    ReportText.Height := ClientHeight - ReportText.Top - Raster - Margin;
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

procedure TFormMain.UpdateBackgroundColor(AColor: TColor);
begin
//  Self.Fill.Color := AColor;
end;

procedure TFormMain.Reset;
begin
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
end;

procedure TFormMain.HandleAction(fa: Integer);
begin
  case fa of
    faToggleSpeedPanel: ToggleSpeedPanel;

    faMemeToggleHelp:
    begin
      ReportText.Visible := False;
    end;

    faMemeGotoLandscape: GotoLandscape;
    faMemeGotoPortrait: GotoPortrait;
    faMemeGotoSquare: GotoSquare;

    faMemeToggleReport:
    begin
      Flash(HelpCaptionText);
      ReportText.Visible := not ReportText.Visible;
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

    faButtonFrameReport:
    begin
      FWantButtonFrameReport := not WantButtonFrameReport;
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
    faToggleChartGraph: ChartImageBtnClick(nil);
    faToggleSalingGraph: SalingImageBtnClick(nil);
    faToggleControllerGraph: ControllerImageBtnClick(nil);
    faToggleMatrixText: RotaForm.MatrixItemClick(nil);

    faMemoryBtn: MemoryBtnClick(nil);
    faMemoryRecallBtn: MemoryRecallBtnClick(nil);

    faRggBogen: BogenBtnClick(nil);
    faRggKoppel: KoppelBtnClick(nil);

    faSofortBtn: SofortBtnClick(nil);
    faGrauBtn: GrauBtnClick(nil);
    faBlauBtn: BlauBtnClick(nil);
    faMultiBtn: MultiBtnClick(nil);

    faShowActi: ActiBtnClick(nil);
    faShowMemo: MemoBtnClick(nil);
    faShowConf: ConfigBtnClick(nil);
    faShowTrimmTab: TrimmTabBtnClick(nil);

    faShowDiagC: ShowDiagramC;
    faShowDiagE: ShowDiagramE;
    faShowDiagQ: ShowDiagramQ;

    faToggleSandboxed: IsSandboxed := not IsSandboxed;
    faToggleAllProps: AllProps := not AllProps;
    faToggleAllTags: ReportManager.XmlAllTags := not ReportManager.XmlAllTags;

    else
    begin
      { do nothing }
    end;

  end;
  UpdateSpeedButtonDown;
end;

function TFormMain.GetActionFromKey(Key: Word): Integer;
begin
  result := faNoop;
  case Key of
    vkF12: ; // result := faMemeSaveBitmap;
    vkC: ; // result := faMemeCopyBitmap;
    vkV: ; // result := faMemePasteBitmap;
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

    'b': fa := faFixpointB;
    'B': fa := faFixpointB0;

    'c': fa := faCycleColorSchemeP;
    'C': fa := faCycleColorSchemeM;

    'd': fa := faFixpointD;
    'D': fa := faFixpointD0;

    'e': fa := faFixpointE;
    'E': fa := faFixpointE0;

    'f': fa := faFixpointF;
    'F': fa := faFixpointF0;

    'g': ;
    'G': ;

    'h': fa := faSalingH;
    'H': fa := faRggHull;

    'i': fa := faWheelRight;
    'I': fa := faWheelLeft;

    'j': fa := faWheelUp;
    'J': fa := faWheelDown;

    'k': ;
    'K': fa := faRggKoppel;

    'l': fa := faMemeGotoLandscape;
    'L': fa := faToggleShowLegend;

    'm': fa := faMemoryBtn;
    'M': fa := faCopyAndPaste;

    'n': ;
    'N': ;

    'r': fa := faMemeToggleReport;
    'R': fa := faReadTrimmFile;

    'o': fa := faWoben;

    'p': fa := faMemeGotoPortrait;

    'Q': fa := faToggleUseQuickSort;

    's': fa := faMemeGotoSquare;

    't': fa := faToggleFontColor;
    'T': fa := FaToggleSpeedPanel;

    'u': fa := faToggleDataText;
    'U': fa := faToggleDiffText;

    'v': fa := faVorstag;
    'w': fa := faWante;

    'z': ; // fa := faResetZoom;
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

    '!': ;
    '"': ;

    '=': ; //fa := faActionPageE;
    '?': ; //fa := faActionPageX;

    '+': fa := faActionPageP;
    '*': fa := faActionPageM;

    '#': ;

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

procedure TFormMain.InitHelpText;
begin
  HL.Add('Toggle Text with Keys:');
  HL.Add('  h    - toggle help');
  HL.Add('  r    - toggle Report');
  HL.Add('');
  HL.Add('Select current param with Button:');
  HL.Add('');
  HL.Add('Change param value with Wheel!');
  HL.Add('  Shift-Wheel = small step');
  HL.Add('  Ctrl-Wheel  = bigger step');
  HL.Add('');
  HL.Add('Another Test: change Format of Window.');
  HL.Add('  1..8, 0 - Trimm selection');
  HL.Add('  l, p, s - Landscape, Portrait, Square');
  HL.Add('');
  HL.Add('Window Status:');
  HL.Add(Format('  Client-W-H = (%d, %d)', [ClientWidth, ClientHeight]));
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
    ShowReport(TRggReport.rgTrimmText);
  end
  else
  begin
    ReportText.Visible := False;
  end;
end;

procedure TFormMain.SetupCombobox(CB: TComboBox);
begin
  if CB = nil then
    Exit;

  CB.Style := csDropDownList;
  CB.DropDownCount := Integer(High(TFederParam));
  CB.Font.Name := 'Consolas';
  CB.Font.Size := 11;
  CB.Font.Color := TRggColors.Red;
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
  HintContainer := TWinControl.Create(Self);
  HintContainer.Parent := Self;

  HintText := TLabel.Create(Self);
  HintText.Parent := HintContainer;
  HintText.Font.Name := 'Consolas';
  HintText.Font.Size := 14;
  HintText.Font.Color := TRggColors.OrangeRed;
  HintText.AutoSize := True;
  HintText.WordWrap := False;

  ReportText := TMemo.Create(Self);
  SetupMemo(ReportText);

  TrimmText := TMemo.Create(Self);
  SetupMemo(TrimmText);

  SpeedPanel1 := TActionSpeedBarRG01.Create(Self);
  SpeedPanel1.Parent := Self;
  SpeedPanel1.ShowHint := True;
  SpeedPanel1.Visible := False;

  SpeedPanel2 := TActionSpeedBarRG03.Create(Self);
  SpeedPanel2.Parent := Self;
  SpeedPanel2.ShowHint := True;
  SpeedPanel2.Visible := False;

  SpeedPanel := SpeedPanel2;
  SpeedPanel.Visible := True;

  if ParamListbox = nil then
  begin
    ParamListbox := TListbox.Create(Self);
    ParamListbox.Parent := Self;
  end;

  if ReportListbox = nil then
  begin
    ReportListbox := TListbox.Create(Self);
    ReportListbox.Parent := Self;
  end;

  if Image = nil then
  begin
    Image := TImage.Create(Self);
    Image.Parent := Self;
  end;

  ComponentsCreated := True;
end;

procedure TFormMain.ToggleSpeedPanel;
begin
  SpeedPanel.Visible := False;

  if SpeedPanel = SpeedPanel1 then
    SpeedPanel := SpeedPanel2
  else
    SpeedPanel := SpeedPanel1;

  SpeedPanel.Visible := True;
  SpeedPanel.UpdateSpeedButtonEnabled;
  SpeedPanel.UpdateSpeedButtonDown;
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

  LayoutSpeedPanel(SpeedPanel1);
  LayoutSpeedPanel(SpeedPanel2);

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
  ReportListbox.Height := ClientHeight - ReportListbox.Top - Raster - Margin;
  ReportListbox.Anchors := ReportListbox.Anchors + [TAnchorKind.akBottom];

  HintContainer.Left := TrimmText.Left + TrimmText.Width + Margin;
  HintContainer.Top := TrimmText.Top;
  HintContainer.Width := ReportMemoWidth;
  HintContainer.Height := Round(40 * FScale);

  HintText.Left := Round(10 * FScale);
  HintText.Top := Round(10 * FScale);

  ReportText.Left := TrimmText.Left + TrimmText.Width + Margin;
  ReportText.Top := HintContainer.Top + HintContainer.Height + Margin;
  ReportText.Height := ClientHeight - ReportText.Top - Raster - Margin;
  ReportText.Width := ReportMemoWidth;
  ReportText.Anchors := ReportText.Anchors + [TAnchorKind.akBottom];
  ReportText.WordWrap := False;

  TextPositionX := ReportText.Left;
  TextPositionY := ReportText.Top;

  Image.Left := ReportText.Left + ReportText.Width + Margin;
  Image.Top := 2 * Raster + Margin;
  Image.Width := ClientWidth - Image.Left - Raster - Margin;
  Image.Height := ClientHeight - Image.Top - Raster - Margin;
  Image.Anchors := [];
  ImagePositionX := Image.Left;
  ImagePositionY := Image.Top;
end;

procedure TFormMain.LineColorBtnClick(Sender: TObject);
var
  b: Boolean;
begin
  b := not DL.WantLineColors;
  DL.WantLineColors := b;
  UpdateSpeedButtonDown;
  RotaForm.Draw;
end;

procedure TFormMain.HandleSegment(fa: Integer);
var
  rg: TRaumGraph;
begin
  rg := RotaForm.RaumGraph;

  case fa of
    faToggleSegmentF: rg.WantFixpunkt := not rg.WantFixPunkt;
    faToggleSegmentM: rg.WantMast:= not rg.WantMast;
    faToggleSegmentW: rg.WantWante:= not rg.WantWante;
    faToggleSegmentV: rg.WantVorstag := not rg.WantVorstag;
    faToggleSegmentS: rg.WantSaling := not rg.WantSaling;
    faToggleSegmentR: rg.WantRumpf := not rg.WantRumpf;
    faToggleSegmentC: rg.WantController := not rg.WantController;
    faToggleSegmentA: rg.WantAchsen := not rg.WantAchsen;
  end;

  RotaForm.Draw;
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

procedure TFormMain.InitSalingGraph;
begin
  SalingImage := TImage.Create(Self);
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
    ControllerGraph.ParamXE0 := Round(Rigg.iP[ooE0, x] - Rigg.iP[ooD0, x]);
    ControllerGraph.EdgePos := Round(Rigg.GSB.Find(fpController).Min);

    ControllerGraph.Draw(TFigure.dtController);
  end;
end;

procedure TFormMain.InitChartGraph;
begin
  ChartControl := TWinControl.Create(Self);
  ChartControl.Parent := Self;
  ChartControl.Visible := False;

  ChartImage := TImage.Create(Self);
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
    ReportText.Visible := True;
    ReportManager.CurrentIndex := ii;
    UpdateReport;
    Main.FederText.CheckState;
  end;
end;

procedure TFormMain.InitReportCombo;
begin
  ReportManager.InitLB(ReportCombo.Items);
end;

procedure TFormMain.ReportComboChange(Sender: TObject);
var
  ii: Integer;
begin
  if ReportCombo <> nil then
  begin
    ii := ReportCombo.ItemIndex;
    ReportManager.CurrentIndex := ii;
    UpdateReport;
    Main.FederText.CheckState;
  end;
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

procedure TFormMain.InitParamCombo;
  procedure Add(fp: TFederParam);
  begin
    ParamCombo.Items.AddObject(Main.Param2Text(fp), TObject(fp));
  end;
begin
  if ParamCombo <> nil then
    Exit;
  Add(fpVorstag);
  Add(fpWinkel);
  Add(fpController);
  Add(fpWante);
  Add(fpWoben);
  Add(fpSalingH);
  Add(fpSalingA);
  Add(fpSalingL);
  Add(fpSalingW);
  Add(fpMastfallF0C);
  Add(fpMastfallF0F);
  Add(fpMastfallVorlauf);
  Add(fpBiegung);
  Add(fpD0X);
end;

procedure TFormMain.ParamComboChange(Sender: TObject);
var
  ii: Integer;
  fp: TFederParam;
begin
  if ParamCombo <> nil then
  begin
    ii := ParamCombo.ItemIndex;
    fp := TFederParam(ParamCombo.Items.Objects[ii]);
    Main.Param := fp;
    ShowTrimm;
  end;
end;

procedure TFormMain.InitTrimmCombo;
var
  cl: TStrings;
begin
  if TrimmCombo <> nil then
  begin
    cl := TrimmCombo.Items;
    cl.AddObject('Trimm1', TObject(1));
    cl.AddObject('Trimm2', TObject(2));
    cl.AddObject('Trimm3', TObject(3));
    cl.AddObject('Trimm4', TObject(4));
    cl.AddObject('Trimm5', TObject(5));
    cl.AddObject('Trimm6', TObject(6));
    cl.AddObject('Trimm7 (420)', TObject(7));
    cl.AddObject('Trimm8 (Logo)', TObject(8));
  end;
end;

procedure TFormMain.TrimmComboChange(Sender: TObject);
var
  t: Integer;
  ii: Integer;
begin
  if TrimmCombo <> nil then
  begin
    ii := TrimmCombo.ItemIndex;
    t := Integer(TrimmCombo.Items.Objects[ii]);
    Main.Trimm := t;
    Main.FederText.CheckState;
    if ReportText.Visible then
    begin
    ShowTrimmData;
      ReportText.Text := RL.Text;
    end;
  end;
end;

procedure TFormMain.ShowTrimmData;
begin
  RL.BeginUpdate;
  try
    RL.Clear;
    Main.CurrentTrimm.WantAll := AllProps;
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
  end;
  UpdateReport;
end;

procedure TFormMain.SofortBtnClick(Sender: TObject);
begin
  Main.SofortBerechnen := not Main.SofortBerechnen;
end;

procedure TFormMain.GrauBtnClick(Sender: TObject);
begin
  Main.BtnGrauDown := not Main.BtnGrauDown;
end;

procedure TFormMain.BlauBtnClick(Sender: TObject);
begin
  Main.BtnBlauDown := not Main.BtnBlauDown;
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
end;

function TFormMain.GetChecked(fa: Integer): Boolean;
begin
  result := false;
  if not IsUp then
    Exit;

  case fa of
    faToggleSandboxed: result := IsSandboxed;
    faToggleAllProps: result := AllProps;
    faToggleAllTags: result := ReportManager.XmlAllTags;

    faMemeToggleReport: result := ReportText.Visible;
    faButtonFrameReport: result := WantButtonFrameReport;
    faChartRect..faChartReset: result := ChartGraph.GetChecked(fa);
    faReportNone..faReportReadme: result := ReportManager.GetChecked(fa);
    faToggleSegmentF..faToggleSegmentA: result := RotaForm.RaumGraph.GetChecked(fa);

    faToggleLineColor: result := DL.WantLineColors;
    faToggleShowLegend: result := RotaForm.LegendItemChecked;

    faToggleUseDisplayList: result := RotaForm.UseDisplayList;
    faToggleUseQuickSort: result := RotaForm.RaumGraph.DL.UseQuickSort;

    faRggBogen: result := RotaForm.Bogen;
    faRggKoppel: result := RotaForm.RaumGraph.Koppel;

    faSofortBtn: result := Main.SofortBerechnen;
    faGrauBtn: result := Main.BtnGrauDown;
    faBlauBtn: result := Main.BtnBlauDown;
    faMemoryBtn: result := False;
    faMultiBtn: result := RotaForm.WantOverlayedRiggs;

    faToggleChartGraph: result := ChartControl.Visible;
    faToggleSalingGraph: result := SalingImage.Visible;
    faToggleControllerGraph: result := ControllerImage.Visible;
    faToggleMatrixText: result := RotaForm.MatrixItemChecked;
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

procedure TFormMain.ActiBtnClick(Sender: TObject);
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
//    Main.RggMain.UpdateGetriebe;
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
end;

procedure TFormMain.InitSpeedButtons;
begin
  if SpeedPanel1 <> nil then
    SpeedPanel1.InitSpeedButtons;
  if SpeedPanel2 <> nil then
    SpeedPanel2.InitSpeedButtons;
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

  ControllerGraph.BackgroundColor := MainVar.ColorScheme.claBackground;
  UpdateControllerGraph;

  SalingGraph.BackgroundColor := MainVar.ColorScheme.claBackground;
  UpdateSalingGraph;
end;

procedure TFormMain.BogenBtnClick(Sender: TObject);
begin
  RotaForm.BogenBtnClick(Sender);
  if Sender <> nil then
    Main.FederText.CheckState;
end;

procedure TFormMain.KoppelBtnClick(Sender: TObject);
begin
  RotaForm.KoppelBtnClick(Sender);
  if Sender <> nil then
    Main.FederText.CheckState;
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
  RotaForm.RaumGraph.DL.UseQuickSort := False;
  Main.GraphRadio := gDisplay;
end;

procedure TFormMain.SuperQuickBtnClick(Sender: TObject);
begin
  RotaForm.UseDisplayList := True;
  RotaForm.WantOverlayedRiggs := False;
  RotaForm.RaumGraph.DL.UseQuickSort := True;
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

end.
