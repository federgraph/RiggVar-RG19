﻿unit FrmMain;

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

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Classes,
  RggTypes,
  RggReport,
  RggRota,
  RiggVar.FB.ActionConst,
  RiggVar.RG.Def,
  RiggVar.RG.Graph,
  RiggVar.RG.Report,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ComCtrls,
  Vcl.Menus,
  Vcl.ExtCtrls,
  Vcl.Buttons,
  Vcl.StdCtrls,
  System.UITypes;

type
  TFormMain = class(TForm)
    SpeedPanel: TPanel;
    StatusBar: TStatusBar;
    TrimmCombo: TComboBox;
    ParamCombo: TComboBox;
    FixpointCombo: TComboBox;
    TrimmMemo: TMemo;
    ReportListBox: TListBox;
    ReportMemo: TMemo;
    PaintBoxR: TPaintBox;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormResize(Sender: TObject);

    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure FormShow(Sender: TObject);

  private
    procedure wmGetMinMaxInfo(var Msg: TMessage); message wm_GetMinMaxInfo;
    procedure ShowHint(Sender: TObject);

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
    procedure GrafikFormItemClick(Sender: TObject);
    procedure OutputFormItemClick(Sender: TObject);

    procedure RotaFormItemClick(Sender: TObject);
    procedure ChartFormItemClick(Sender: TObject);
    procedure ReportFormItemClick(Sender: TObject);
    procedure OptionItemClick(Sender: TObject);

    procedure ConsoleItemClick(Sender: TObject);
    procedure SpeedBarItemClick(Sender: TObject);
    procedure StatusBarItemClick(Sender: TObject);

    { Memo Menu }
    procedure rLItemClick(Sender: TObject);

    { Grafik Menu }
    procedure VonDerSeiteItemClick(Sender: TObject);

    procedure CalcOffsetItemClick(Sender: TObject);
    procedure AdjustFormItemClick(Sender: TObject);
    procedure PrintItemClick(Sender: TObject);

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
  private
    procedure UpdateBtnClick(Sender: TObject);
    procedure ReglerBtnClick(Sender: TObject);
    procedure MemoryBtnClick(Sender: TObject);
    procedure MemoryRecallBtnClick(Sender: TObject);

    procedure BtnGrauClick(Sender: TObject);
    procedure KoppelBtnClick(Sender: TObject);
    procedure PaintBtnClick(Sender: TObject);
    procedure BtnBlauClick(Sender: TObject);
    procedure ControllerBtnClick(Sender: TObject);
    procedure ZweischlagBtnClick(Sender: TObject);

    procedure M10BtnClick(Sender: TObject);
    procedure M1BtnClick(Sender: TObject);
    procedure P1BtnClick(Sender: TObject);
    procedure P10BtnClick(Sender: TObject);

    procedure SandboxedBtnClick(Sender: TObject);
    procedure AllPropsBtnClick(Sender: TObject);
    procedure AllTagsBtnClick(Sender: TObject);

    procedure MT0BtnClick(Sender: TObject);
    procedure ReadTrimmFileBtnClick(Sender: TObject);
    procedure SaveTrimmFileBtnClick(Sender: TObject);
    procedure CopyTrimmFileBtnClick(Sender: TObject);
    procedure CopyTrimmItemBtnClick(Sender: TObject);
    procedure PasteTrimmItemBtnClick(Sender: TObject);
    procedure CopyAndPasteBtnClick(Sender: TObject);

    procedure ListBoxClick(Sender: TObject);

    procedure TrimmComboChange(Sender: TObject);
    procedure ParamComboChange(Sender: TObject);
//    procedure ViewpointComboChange(Sender: TObject);
    procedure FixpointComboChange(Sender: TObject);
  public
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
    GrafikFormItem: TMenuItem;
    OptionItem: TMenuItem;
    N4: TMenuItem;
    ConsoleItem: TMenuItem;
    RotaFormItem: TMenuItem;
    ChartFormItem: TMenuItem;
    ReportFormItem: TMenuItem;
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
    CalcOffsetItem: TMenuItem;
    AdjustFormItem: TMenuItem;
    PrintItem: TMenuItem;
    N6: TMenuItem;
    PaintItem: TMenuItem;
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
    HilfeItem: TMenuItem;
    InfoItem: TMenuItem;
    LogoItem: TMenuItem;
    AboutItem: TMenuItem;
  public
    LedShape: TShape;

    UpdateBtn: TSpeedButton;
    ReglerBtn: TSpeedButton;
    MemoryBtn: TSpeedButton;
    MemoryRecallBtn: TSpeedButton;

    SofortBtn: TSpeedButton;
    DiffBtn: TSpeedButton;
    WinkelBtn: TSpeedButton;
    ControllerBtn: TSpeedButton;

    M10Btn: TSpeedButton;
    M1Btn: TSpeedButton;
    P1Btn: TSpeedButton;
    P10Btn: TSpeedButton;

    MT0Btn: TSpeedButton;
    ReadTrimmFileBtn: TSpeedButton;
    SaveTrimmFileBtn: TSpeedButton;
    CopyTrimmFileBtn: TSpeedButton;
    CopyTrimmItemBtn: TSpeedButton;
    PasteTrimmItemBtn: TSpeedButton;
    CopyAndPasteBtn: TSpeedButton;

    SandboxedBtn: TSpeedButton;
    AllPropsBtn: TSpeedButton;
    AllTagsBtn: TSpeedButton;

    PaintBtn: TSpeedButton;
    BtnGrau: TSpeedButton;
    BtnBlau: TSpeedButton;
    KoppelBtn: TSpeedButton;
    ZweischlagBtn: TSpeedButton;

    BuntBtn: TSpeedButton;
    HullBtn: TSpeedButton;
    MatrixBtn: TSpeedButton;

    SeiteBtn: TSpeedButton;
    TopBtn: TSpeedButton;
    AchternBtn: TSpeedButton;
    NullBtn: TSpeedButton;

    ZoomInBtn: TSpeedButton;
    ZoomOutBtn: TSpeedButton;

    UseDisplayListBtn: TSpeedButton;
    UseQuickSortBtn: TSpeedButton;
    BogenBtn: TSpeedButton;

    procedure SeiteBtnClick(Sender: TObject);
    procedure AchternBtnClick(Sender: TObject);
    procedure TopBtnClick(Sender: TObject);
    procedure NullBtnClick(Sender: TObject);
  private
    BtnTop: Integer;
    BtnLeft: Integer;
    BtnWidth: Integer;
    BtnHeight: Integer;
    BtnCounter: Integer;
    BtnSpace: Integer;
    BtnGroupSpace: Integer;
    BtnColor: TColor;

    procedure InitSpeedPanel;
    procedure InitSpeedButtons;

    function AddSpeedBtn(N: string; AGroupSpace: Integer = 0): TSpeedButton;
    function RefSpeedBtn(B: TSpeedButton; AGroupSpace: Integer = 0): TSpeedButton;
    function AddShapeBtn(N: string; AGroupSpace: Integer): TShape;
    function RefShapeBtn(S: TShape; AGroupSpace: Integer): TShape;
  private
    procedure InitTrimmCombo;
    procedure InitParamCombo;
    procedure InitViewpointCombo;
    procedure InitFixpointCombo;
    function GetComboFixPoint: TRiggPoint;
  private
    TL: TStrings;
    ML: TStrings;
    ReportManager: TRggReportManager;
    FReportLabelCaption: string;
    WantConsole: Boolean;

    procedure FormCreate1;
    procedure FormCreate2;

    procedure LayoutComponents;
    procedure InitOutputForm;
    procedure InitReportListBox;
    procedure InitToolbar;
    procedure InitOpenDialog;
    procedure InitSaveDialog;
    procedure InitStatusBar;
    procedure InitMenu;
    procedure InitEventHandlers;

    procedure SetupMemo(Memo: TMemo);
    procedure SetupComboBox(CB: TComboBox);
    procedure SetupListBox(LB: TListBox);

    procedure SetControllerChecked(Value: Boolean);
    procedure SetKoppelChecked(Value: Boolean);
    procedure SetReportLabelCaption(const Value: string);

    procedure ShowTrimm;
    procedure ShowCurrentReport;
  public
    FScale: single;
    Raster: Integer;
    Margin: Integer;
    function GetOpenFileName(dn, fn: string): string;
    function GetSaveFileName(dn, fn: string): string;
    procedure UpdateOnParamValueChanged;

    property ReportLabelCaption: string read FReportLabelCaption write SetReportLabelCaption;
  private
    FormShown: Boolean;
    FormClosing: Boolean;
    RotaForm: TRotaForm1;
    StrokeRigg: IStrokeRigg;
    procedure CenterRotaForm;
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

uses
  RiggVar.App.Main,
  RiggVar.VM.FormMainD,
  RiggVar.RG.Main,
  RggModul,
  FrmInfo,
  FrmConsole,
  FrmInput,
  FrmKreis,
  FrmOutput,
  FrmGrafik,
  FrmReport,
  FrmChart,
  FrmRot,
  FrmAniRot;

const
  SWarningText = 'Änderungen in %s sichern?';

procedure TFormMain.wmGetMinMaxInfo(var Msg: TMessage);
begin
  inherited;
  PMinMaxInfo(Msg.lParam)^.ptMinTrackSize.X := Round(900 * FScale);
  PMinMaxInfo(Msg.lParam)^.ptMinTrackSize.Y := Round(700 * FScale);
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
{$ifdef Debug}
   ReportMemoryLeaksOnShutdown := True;
{$endif}
  FormatSettings.DecimalSeparator := '.';

  DoubleBuffered := True;

  FScale := 1.0;
{$ifdef MSWindows}
//  FScale := ScaleFactor;
{$endif}

  FormMain := self;
  if (Screen.Width > 1700 * FScale) then
  begin
    Left := Round(60 * FScale);
    Top := Round(105 * FScale);
    Width := Round(1600 * FScale);
    Height := Round(768 * FScale);
  end
  else
  begin
    Left := Round(60 * FScale);
    Top := Round(105 * FScale);
    Width := Round(1024 * FScale);
    Height := Round(768 * FScale);
  end;

  Margin := Round(10 * FScale);
  Raster := Round(MainVar.Raster * FScale);
  MainVar.Scale := FScale;
  MainVar.ScaledRaster := Raster;
  TKR := Round(TKR * FScale);

  FormCreate1;
  FormCreate2;
  Main.IsUp := True;
  RiggModul.ViewModelM.IsUp := True;
  RiggModul.UpdateUI;
end;

procedure TFormMain.FormCreate1;
begin
  InputForm := TInputForm.Create(Application);
  OutputForm := TOutputForm.Create(Application);
  GrafikForm := TGrafikForm.Create(Application);

  RiggModul := TRiggModul.Create;
  RiggModul.RG19A := False;
  RiggModul.ViewModelM := TViewModelMainC.Create;
  RiggModul.Init;
  RiggModul.PBG := GrafikForm.PaintBoxG;
  RiggModul.SofortBtnDown := False;
  RiggModul.BtnGrauDown := True;

  Main := TMain.Create(RiggModul.Rigg);
  Main.Logger.Verbose := True;
  Main.RiggModul := RiggModul;

  Main.InitLogo; // sets WantLogoData to true
  Main.Init420; // sets WantLogo to false
  WantLogoData := False;

  StatusBar.Panels[0].Text := '';
  Application.OnHint := ShowHint;

  RiggModul.ViewModelM.Caption := 'Rigg19';
  OnClose := FormClose;
  OnCloseQuery := FormCloseQuery;

  RotaForm := TRotaForm1.Create;
  StrokeRigg := RotaForm;
  Main.StrokeRigg := RotaForm;
  RotaForm.PaintBox3D := PaintboxR;
  RotaForm.UseDisplayList := False;
  RotaForm.Init;
  PaintboxR := RotaForm.PaintBox3D;
  RotaForm.IsUp := True;
  RotaForm.ZoomIndex := 8;
end;

procedure TFormMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Screen.OnActiveFormChange := nil;
  RiggModul.Free;
  // Application.HelpCommand(HELP_QUIT,0);
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  FormClosing := True;
  ReportManager.Free;

  Main.Free;
  Main := nil;
end;

procedure TFormMain.SalingTypChanged(Sender: TObject);
begin
  if Sender = FestItem then
    RiggModul.SalingTyp := stFest
  else if Sender = DrehbarItem then
    RiggModul.SalingTyp := stDrehbar
  else if Sender = OSBItem then
    RiggModul.SalingTyp := stOhneBiegt
  else if Sender = OSSItem then
    RiggModul.SalingTyp := stOhneStarr;
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

procedure TFormMain.ShowHint(Sender: TObject);
begin
  StatusBar.Panels[0].Text := Application.Hint;
end;

procedure TFormMain.NewItemClick(Sender: TObject);
var
  DialogValue: Integer;
  FName: string;
begin
  if (Caption <> 'Rigg') and RiggModul.Modified then
  begin
    FName := Caption;
    DialogValue := MessageDlg(Format(SWarningText, [FName]), mtConfirmation,
      [mbYes, mbNo, mbCancel], 0);
    case DialogValue of
      mrYes:
        SaveItemClick(Sender);
      { mrNo: weiter ohne speichern }
      mrCancel:
        Exit;
    end;
  end;
  RiggModul.Neu(nil);
end;

procedure TFormMain.OpenItemClick(Sender: TObject);
var
  DialogValue: Integer;
  FName: string;
begin
  if RiggModul.Modified then
  begin
    FName := Caption;
    DialogValue := MessageDlg(Format(SWarningText, [FName]), mtConfirmation, mbYesNoCancel, 0);
    case DialogValue of
      mrYes: SaveItemClick(Sender);
      mrCancel: Exit;
    end;
  end;
  if OpenDialog.Execute then
  begin
    Main.Open(OpenDialog.FileName);
    UpdateOnParamValueChanged;
  end;
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

procedure TFormMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
  DialogValue: Integer;
  FName: string;
begin
  if not RiggModul.AutoSave then
  begin
    CanClose := True;
    Exit;
  end;

  if RiggModul.Modified then
  begin
    FName := Caption;
    DialogValue := MessageDlg(Format(SWarningText, [FName]), mtConfirmation,
      mbYesNoCancel, 0);
    case DialogValue of
      mrYes:
        begin
          SaveItemClick(Sender);
          CanClose := not RiggModul.Modified;
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

  RiggModul.ViewPoint := ViewPoint;
end;

procedure TFormMain.PaintBtnClick(Sender: TObject);
begin
  PaintItem.Checked := not PaintItem.Checked;
  PaintBtn.Down := PaintItem.Checked;
  RiggModul.PaintBtnDown := PaintBtn.Down;
end;

procedure TFormMain.BtnBlauClick(Sender: TObject);
begin
  ReferenzItem.Checked := not ReferenzItem.Checked;
  BtnBlau.Down := ReferenzItem.Checked;
  RiggModul.BtnBlauDown := BtnBlau.Down;
end;

procedure TFormMain.BtnGrauClick(Sender: TObject);
begin
  EntlastetItem.Checked := not EntlastetItem.Checked;
  BtnGrau.Down := EntlastetItem.Checked;
  RiggModul.BtnGrauDown := BtnGrau.Down;
end;

procedure TFormMain.SetKoppelChecked(Value: Boolean);
begin
  KoppelItem.Checked := Value;
  KoppelBtn.Down := Value;
  RiggModul.KoppelBtnDown := Value;
end;

procedure TFormMain.KoppelBtnClick(Sender: TObject);
begin
  SetKoppelChecked(not KoppelItem.Checked);
end;

procedure TFormMain.ZweischlagBtnClick(Sender: TObject);
begin
  BogenItem.Checked := not BogenItem.Checked;
  ZweischlagBtn.Down := not BogenItem.Checked;
  RiggModul.ZweischlagBtnDown := ZweischlagBtn.Down;
end;

procedure TFormMain.WinkelItemClick(Sender: TObject);
begin
  WinkelItem.Checked := not WinkelItem.Checked;
  WinkelBtn.Down := WinkelItem.Checked;
  RiggModul.WinkelBtnDown := WinkelBtn.Down;
end;

procedure TFormMain.DifferenzItemClick(Sender: TObject);
begin
  DifferenzItem.Checked := not DifferenzItem.Checked;
  DiffBtn.Down := DifferenzItem.Checked;
  RiggModul.DiffBtnDown := DiffBtn.Down;
end;

procedure TFormMain.SofortItemClick(Sender: TObject);
begin
  SofortItem.Checked := not SofortItem.Checked;
  SofortBtn.Down := SofortItem.Checked;
  RiggModul.SofortBtnDown := SofortBtn.Down;
  if SofortItem.Checked then
  begin
    PaintItem.Enabled := True;
    PaintBtn.Enabled := True;
  end
  else
  begin
    StatusBar.Panels[1].Text := RiggModul.Rigg.GetriebeStatusText;
    PaintItem.Checked := False;
    PaintItem.Enabled := False;
    PaintBtn.Down := False;
    PaintBtn.Enabled := False;
  end;
  Main.Draw;
end;

procedure TFormMain.SetControllerChecked(Value: Boolean);
begin
  ControllerItem.Checked := Value;
  ControllerBtn.Down := Value;
  RiggModul.ControllerBtnDown := Value;
  RotaForm.RaumGraph.ControllerTyp := Main.Rigg.ControllerTyp;
  RotaForm.Draw;
end;

procedure TFormMain.ControllerBtnClick(Sender: TObject);
begin
  SetControllerChecked(not ControllerItem.Checked);
end;

procedure TFormMain.KnickenItemClick(Sender: TObject);
begin
  if Sender = QuerKraftItem then
    RiggModul.CalcTyp := ctQuerKraftBiegung
  else if Sender = KnickenItem then
    RiggModul.CalcTyp := ctBiegeKnicken
  else if Sender = KraftGemessenItem then
    RiggModul.CalcTyp := ctKraftGemessen;
end;

procedure TFormMain.KorrigiertItemClick(Sender: TObject);
begin
  KorrigiertItem.Checked := not KorrigiertItem.Checked;
  Main.Korrigiert := KorrigiertItem.Checked;
end;

procedure TFormMain.LogoItemClick(Sender: TObject);
begin
  WantLogoData := not WantLogoData;
  LogoItem.Checked := WantLogoData;
  RiggModul.Neu(nil);
  RiggModul.UpdateGetriebe;
end;

procedure TFormMain.UpdateBtnClick(Sender: TObject);
begin
  Main.Rigg.UpdateGetriebe;
end;

procedure TFormMain.BiegeNeigeItemClick(Sender: TObject);
begin
  RiggModul.BiegeNeigeItemClick;
end;

procedure TFormMain.ReglerBtnClick(Sender: TObject);
begin
  RiggModul.ReglerBtnClick;
  SetKoppelChecked(False);
end;

procedure TFormMain.MemoryBtnClick(Sender: TObject);
begin
  RiggModul.MemoryBtnClick;
end;

procedure TFormMain.MemoryRecallBtnClick(Sender: TObject);
begin
  RiggModul.MemoryRecallBtnClick;
end;

procedure TFormMain.OptionItemClick(Sender: TObject);
begin
  RiggModul.OptionItemClick;
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

procedure TFormMain.PrintItemClick(Sender: TObject);
begin
  RiggModul.PrintGrafik;
end;

procedure TFormMain.AdjustFormItemClick(Sender: TObject);
begin
  RiggModul.AdjustGrafik;
end;

procedure TFormMain.CalcOffsetItemClick(Sender: TObject);
begin
  RiggModul.GetGBoxOffset;
end;

procedure TFormMain.InputFormItemClick(Sender: TObject);
begin
  InputFormItem.Checked := not InputFormItem.Checked;
  if InputFormItem.Checked then
  begin
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

procedure TFormMain.GrafikFormItemClick(Sender: TObject);
begin
  GrafikFormItem.Checked := not GrafikFormItem.Checked;
  if GrafikFormItem.Checked then
  begin
    GrafikForm.Parent := nil;
    GrafikForm.BorderStyle := bsSizeable;
    GrafikForm.ClientWidth := Round(305 * FScale);
    GrafikForm.ClientHeight := Round(457 * FScale);
    GrafikForm.Show;
  end
  else
    GrafikForm.Hide;
end;

procedure TFormMain.ChartFormItemClick(Sender: TObject);
begin
  if RiggModul.ChartFormActive then
  begin
    ChartForm.Close;
    Exit;
  end;
  RiggModul.ChartItemClick;
  RiggModul.ChartFormActive := True;
  ChartFormItem.Caption := 'Diagramm schließen';
  ChartFormItem.Hint := '  Diagramm schließen';
end;

procedure TFormMain.ReportFormItemClick(Sender: TObject);
begin
  if RiggModul.ReportFormActive then
  begin
    ReportForm.Close;
    Exit;
  end;
  RiggModul.ReportItemClick;
  RiggModul.ReportFormActive := True;
  ReportFormItem.Caption := 'Report schließen';
  ReportFormItem.Hint := '  Report schließen';
end;

procedure TFormMain.RotaFormItemClick(Sender: TObject);
begin
  if RiggModul.RotaFormActive then
  begin
    AniRotationForm.Close;
    Exit;
  end;
  RiggModul.RotaFormItemClick;
  RiggModul.RotaFormActive := True;
  RotaFormItem.Caption := '3D Grafik schließen';
  RotaFormItem.Hint := '  3D Grafik schließen';
end;

procedure TFormMain.SpeedBarItemClick(Sender: TObject);
begin
  SpeedBarItem.Checked := not SpeedBarItem.Checked;
  SpeedPanel.Visible := SpeedBarItem.Checked;
end;

procedure TFormMain.StatusBarItemClick(Sender: TObject);
begin
  StatusBarItem.Checked := not StatusBarItem.Checked;
  StatusBar.Visible := StatusBarItem.Checked;
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

procedure TFormMain.InitToolbar;
begin
  UpdateBtn.OnClick := UpdateBtnClick;
  ReglerBtn.OnClick := ReglerBtnClick;
  MemoryBtn.OnClick := MemoryBtnClick;
  MemoryRecallBtn.OnClick := MemoryRecallBtnClick;

  PaintBtn.OnClick := PaintBtnClick;
  BtnBlau.OnClick := BtnBlauClick;
  BtnGrau.OnClick := BtnGrauClick;
  KoppelBtn.OnClick := KoppelBtnClick;
  ZweischlagBtn.OnClick := ZweischlagBtnClick;

  ControllerBtn.OnClick := ControllerBtnClick;
  WinkelBtn.OnClick := WinkelItemClick;
  DiffBtn.OnClick := DifferenzItemClick;
  SofortBtn.OnClick := SofortItemClick;
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

procedure TFormMain.InitSpeedPanel;
begin
  SpeedPanel.Align := alTop;
  SpeedPanel.BevelOuter := bvNone;
  SpeedPanel.ParentShowHint := False;
  SpeedPanel.ShowHint := True;
  SpeedPanel.TabOrder := 0;
end;

function TFormMain.AddSpeedBtn(N: string; AGroupSpace: Integer): TSpeedButton;
begin
  result := TSpeedButton.Create(SpeedPanel);
  result.Parent := SpeedPanel;
  result.Name := N;
  RefSpeedBtn(result, AGroupSpace);
end;

function TFormMain.RefSpeedBtn(B: TSpeedButton; AGroupSpace: Integer): TSpeedButton;
begin
  result := B;
  BtnLeft := BtnLeft + AGroupSpace;
  B.Left := BtnLeft + BtnCounter * BtnWidth + BtnSpace;
  B.Top := BtnTop;
  B.Width := BtnWidth;
  B.Height := BtnHeight;
  B.Font.Name := 'Consolas';
  B.Font.Size := 12;
  B.Font.Color := BtnColor;
  Inc(BtnCounter);
end;

function TFormMain.AddShapeBtn(N: string; AGroupSpace: Integer): TShape;
begin
  result := TShape.Create(SpeedPanel);
  result.Parent := SpeedPanel;
  result.Name := N;
  RefShapeBtn(result, AGroupSpace);
end;

function TFormMain.RefShapeBtn(S: TShape; AGroupSpace: Integer): TShape;
var
  temp: Integer;
begin
  temp := (BtnWidth - Margin) div 2;
  result := S;
  S.Left := BtnLeft + BtnCounter * BtnWidth + BtnSpace + temp;
  S.Top := BtnTop + 1;
  S.Width := BtnWidth - 2 * temp;
  S.Height := BtnHeight - Round(2 * FScale);
  S.Brush.Color := clGreen;
  Inc(BtnCounter);
end;

procedure TFormMain.InitSpeedButtons;
var
  sb: TSpeedButton;
  WantPaintOptionBtn: Boolean;
begin
  WantPaintOptionBtn := False;

  BtnCounter := 0;
  BtnLeft := 0;
  BtnTop := Round(3 * FScale);
  BtnSpace := Round(3 * FScale);
  BtnGroupSpace := Round(12 * FScale);
  BtnWidth := Round(30 * FScale);
  BtnHeight := Round(30 * FScale);

  SpeedPanel.Height := BtnHeight + 2 * BtnTop;

  { visual group of four buttons, still group index 0 }

  sb := AddSpeedBtn('UpdateBtn', BtnGroupSpace);
  UpdateBtn := sb;
  sb.Caption := '=';
  sb.Hint := 'Compute|Rigg neu Berechnen';
  sb.GroupIndex := 0;
  sb.OnClick := UpdateBtnClick;

  sb := AddSpeedBtn('ReglerBtn', 0);
  ReglerBtn := sb;
  sb.Caption := 'R';
  sb.Hint := 'Regeln|Trimm Regeln';
  sb.GroupIndex := 0;
  sb.OnClick := ReglerBtnClick;

  sb := AddSpeedBtn('MemoryBtn', 0);
  MemoryBtn := sb;
  sb.Caption := 'M';
  sb.Hint := 'Memory|Memory (Trimm als Referenz speichern)';
  sb.GroupIndex := 0;
  sb.OnClick := MemoryBtnClick;

  sb := AddSpeedBtn('MemoryRecallBtn', 0);
  MemoryRecallBtn := sb;
  sb.Caption := 'MR';
  sb.Hint := 'Memory Recall';
  sb.GroupIndex := 0;
  sb.OnClick := MemoryRecallBtnClick;

  { Paint option buttons }

  sb := AddSpeedBtn('PaintBtn', BtnGroupSpace);
  PaintBtn := sb;
  sb.Caption := 'pb';
  sb.Hint := 'Paint Btn|2D Grafik - Alte Grafik stehenlassen';
  sb.AllowAllUp := True;
  sb.Down := False;
  sb.GroupIndex := 1;
  sb.OnClick := PaintBtnClick;
  sb.Visible := WantPaintOptionBtn;

  sb := AddSpeedBtn('BtnBlau', 0);
  BtnBlau := sb;
  sb.Caption := 'bb';
  sb.Hint := 'Blue Btn|2D Grafik - Nullstellung anzeigen';
  sb.AllowAllUp := True;
  sb.Down := False;
  sb.GroupIndex := 2;
  sb.OnClick := BtnBlauClick;
  sb.Visible := WantPaintOptionBtn;

  sb := AddSpeedBtn('BtnGrau', 0);
  BtnGrau := sb;
  sb.Caption := 'gb';
  sb.Hint := 'Gray Btn|2D Grafik - Entspanntes Rigg einblenden';
  sb.AllowAllUp := True;
  sb.Down := True;
  sb.GroupIndex := 3;
  sb.OnClick := BtnGrauClick;
  sb.Visible := WantPaintOptionBtn;

  sb := AddSpeedBtn('KoppelBtn', 0);
  KoppelBtn := sb;
  sb.Caption := 'kk';
  sb.Hint := 'Koppelkurve|2D Grafik - Koppelkurve anzeigen';
  sb.AllowAllUp := True;
  sb.Down := True;
  sb.GroupIndex := 4;
  sb.OnClick := KoppelBtnClick;
  sb.Visible := WantPaintOptionBtn;

  sb := AddSpeedBtn('ZweischlagBtn', 0);
  ZweischlagBtn := sb;
  sb.Caption := 'zs';
  sb.Hint := 'Zweischlag|2D Grafik - Mast als Zweischlag einzeichnen';
  sb.AllowAllUp := True;
  sb.Down := False;
  sb.GroupIndex := 5;
  sb.OnClick := ControllerBtnClick;
  sb.Visible := WantPaintOptionBtn;

  if not WantPaintOptionBtn then
  begin
    BtnCounter := 0;
    BtnLeft := MemoryRecallBtn.Left + BtnWidth;
  end;

  { Model option buttons }

  sb := AddSpeedBtn('ControllerBtn', BtnGroupSpace);
  ControllerBtn := sb;
  sb.Caption := 'C';
  sb.Hint := 'Controller|Umschalter für Controller-Modus';
  sb.AllowAllUp := True;
  sb.Down := False;
  sb.GroupIndex := 6;
  sb.OnClick := ControllerBtnClick;

  sb := AddSpeedBtn('DiffBtn', 0);
  DiffBtn := sb;
  sb.Caption := 'D';
  sb.Hint := 'Diff|Umschalter Differenzen/Absolutwerte';
  sb.AllowAllUp := True;
  sb.Down := False;
  sb.GroupIndex := 7;
  sb.OnClick := DifferenzItemClick;

  sb := AddSpeedBtn('WinkelBtn', 0);
  WinkelBtn := sb;
  sb.Caption := 'W';
  sb.Hint := 'Winkel|Umschalter Winkel/Vorstag';
  sb.AllowAllUp := True;
  sb.Down := False;
  sb.GroupIndex := 8;
  sb.OnClick := WinkelItemClick;

  sb := AddSpeedBtn('SofortBtn', 0);
  SofortBtn := sb;
  sb.Caption := 'A';
  sb.Hint := 'Automatik|Umschalter Rigg sofort berechnen (Automatik)';
  sb.AllowAllUp := True;
  sb.Down := False;
  sb.GroupIndex := 9;
  sb.OnClick := SofortItemClick;

  { LED }

  LedShape := AddShapeBtn('LedShape', BtnGroupSpace);

  { New Button group Trimm Data }

  BtnCounter := 0;
  BtnLeft := LedShape.Left + LedShape.Width; // skip over LED shape
  BtnWidth := Round(50 * FScale); // new button width for new buttons
  BtnColor := clGreen;

  sb := AddSpeedBtn('MT0Btn', BtnGroupSpace);
  MT0Btn := sb;
  sb.Caption := 'MT0';
  sb.Hint := 'Memory Trimm 0';
  sb.GroupIndex := 10;
  sb.OnClick := MT0BtnClick;

  BtnColor := clFuchsia;

  sb := AddSpeedBtn('ReadTrimmFileBtn', 0);
  ReadTrimmFileBtn := sb;
  sb.Caption := 'rtf';
  sb.Hint := 'Read File|Read Trimm File';
  sb.GroupIndex := 10;
  sb.OnClick := ReadTrimmFileBtnClick;

  sb := AddSpeedBtn('SaveTrimmFileBtn', 0);
  SaveTrimmFileBtn := sb;
  sb.Caption := 'stf';
  sb.Hint := 'Save File|Save Trimm File';
  sb.GroupIndex := 10;
  sb.OnClick := SaveTrimmFileBtnClick;

  sb := AddSpeedBtn('CopyTrimmFileBtn', 0);
  CopyTrimmFileBtn := sb;
  sb.Caption := 'ctf';
  sb.Hint := 'Copy File|Copy Trimm File';
  sb.GroupIndex := 10;
  sb.OnClick := CopyTrimmFileBtnClick;

  BtnColor := clBlue;

  sb := AddSpeedBtn('CopyTrimmItemBtn', 0);
  CopyTrimmItemBtn := sb;
  sb.Caption := 'cti';
  sb.Hint := 'Copy Item|Copy Trimm Item';
  sb.GroupIndex := 10;
  sb.OnClick := CopyTrimmItemBtnClick;

  sb := AddSpeedBtn('PasteTrimmItemBtn', 0);
  PasteTrimmItemBtn := sb;
  sb.Caption := 'pti';
  sb.Hint := 'Paste Item|Paste Trimm Item';
  sb.GroupIndex := 10;
  sb.OnClick := PasteTrimmItemBtnClick;

  BtnColor := clBlack;

  sb := AddSpeedBtn('CopyAndPasteBtn', 0);
  CopyAndPasteBtn := sb;
  sb.Caption := 'M';
  sb.Hint := 'Update Item|Copy and Paste Btn';
  sb.GroupIndex := 10;
  sb.OnClick := CopyAndPasteBtnClick;

  { Button Group Param Value Change }

  BtnColor := TColors.Teal;

  sb := AddSpeedBtn('M10Btn', BtnGroupSpace);
  M10Btn := sb;
  sb.Caption := 'M10';
  sb.Hint := '-10|Param Value Minus 10';
  sb.GroupIndex := 10;
  sb.OnClick := M10BtnClick;

  sb := AddSpeedBtn('M1Btn', 0);
  M1Btn := sb;
  sb.Caption := 'M1';
  sb.Hint := '-1|Param Value Minus 1';
  sb.GroupIndex := 10;
  sb.OnClick := M1BtnClick;

  sb := AddSpeedBtn('P1Btn', 0);
  P1Btn := sb;
  sb.Caption := 'P1';
  sb.Hint := '+1|Param Value Plus 1';
  sb.GroupIndex := 10;
  sb.OnClick := P1BtnClick;

  sb := AddSpeedBtn('P10Btn', 0);
  P10Btn := sb;
  sb.Caption := 'P10';
  sb.Hint := '+10|Param Value Plus 10';
  sb.GroupIndex := 10;
  sb.OnClick := P10BtnClick;

  { new 'checkbox' group }

  sb := AddSpeedBtn('SandboxedBtn', BtnGroupSpace);
  SandboxedBtn := sb;
  sb.Caption := 'SB';
  sb.Hint := 'Sandboxed';
  sb.AllowAllUp := True;
  sb.Down := MainVar.IsSandboxed;
  sb.GroupIndex := 11;
  sb.OnClick := SandboxedBtnClick;

  sb := AddSpeedBtn('AllPropsBtn', 0);
  AllPropsBtn := sb;
  sb.Caption := 'AP';
  sb.Hint := 'All Props';
  sb.AllowAllUp := True;
  sb.Down := False;
  sb.GroupIndex := 11;
  sb.OnClick := AllPropsBtnClick;

  sb := AddSpeedBtn('AllTagsBtn', 0);
  AllTagsBtn := sb;
  sb.Caption := 'AT';
  sb.Hint := 'All Tags';
  sb.AllowAllUp := True;
  sb.Down := False;
  sb.GroupIndex := 11;
  sb.OnClick := AllTagsBtnClick;

  { TRotaForm options }

  sb := AddSpeedBtn('HullBtn', BtnGroupSpace);
  HullBtn := sb;
  sb.Caption := 'Hull';
  sb.Hint := 'Hull';
  sb.AllowAllUp := True;
  sb.Down := RotaForm.RumpfItemChecked;
  sb.GroupIndex := 12;
  sb.OnClick := RotaForm.RumpfBtnClick;

  sb := AddSpeedBtn('BuntBtn', 0);
  BuntBtn := sb;
  sb.Caption := 'Bunt';
  sb.Hint := 'Paint Btn 2|Paint Button for RotaForm';
  sb.AllowAllUp := True;
  sb.Down := RotaForm.PaintItemChecked;
  sb.GroupIndex := 12;
  sb.OnClick := RotaForm.PaintBtnClick;

  sb := AddSpeedBtn('MatrixBtn', 0);
  MatrixBtn := sb;
  sb.Caption := 'Mat';
  sb.Hint := 'Matrix Btn|Toggle display of Matrix';
  sb.AllowAllUp := True;
  sb.Down := RotaForm.MatrixItemChecked;
  sb.GroupIndex := 12;
  sb.OnClick := RotaForm.MatrixItemClick;

  BtnCounter := 0;
  BtnLeft := sb.Left + BtnWidth;
  BtnWidth := Round(30 * FScale);

  sb := AddSpeedBtn('SeiteBtn', BtnGroupSpace);
  SeiteBtn := sb;
  sb.Caption := 'S';
  sb.Hint := 'Side View|Viewpoint Seite';
  sb.AllowAllUp := True;
  sb.Down := RotaForm.ViewPoint = vpSeite;
  sb.GroupIndex := 13;
  sb.OnClick := SeiteBtnClick;

  sb := AddSpeedBtn('AchternBtn', 0);
  AchternBtn := sb;
  sb.Caption := 'A';
  sb.Hint := 'Stern View|Viewpoint Achtern';
  sb.AllowAllUp := False;
  sb.Down := RotaForm.ViewPoint = vpAchtern;
  sb.GroupIndex := 13;
  sb.OnClick := AchternBtnClick;

  sb := AddSpeedBtn('TopBtn', 0);
  TopBtn := sb;
  sb.Caption := 'T';
  sb.Hint := 'Top View|Viewpoint Top';
  sb.AllowAllUp := False;
  sb.Down := RotaForm.ViewPoint = vpTop;
  sb.GroupIndex := 13;
  sb.OnClick := TopBtnClick;

  sb := AddSpeedBtn('NullBtn', 0);
  NullBtn := sb;
  sb.Caption := '3D';
  sb.Hint := 'Viewpoint 3D';
  sb.AllowAllUp := False;
  sb.Down := RotaForm.ViewPoint = vp3D;
  sb.GroupIndex := 13;
  sb.OnClick := NullBtnClick;

  { Zoom Buttons }

  sb := AddSpeedBtn('ZoomOutBtn', BtnGroupSpace);
  ZoomOutBtn := sb;
  sb.Caption := 'Z-';
  sb.Hint := 'Zoom Out';
  sb.GroupIndex := 0;
  sb.OnClick := RotaForm.ZoomOutBtnClick;

  sb := AddSpeedBtn('ZoomInBtn', 0);
  ZoomInBtn := sb;
  sb.Caption := 'Z+';
  sb.Hint := 'Zoom In';
  sb.GroupIndex := 0;
  sb.OnClick := RotaForm.ZoomInBtnClick;

  { Graph Option Buttons}

  sb := AddSpeedBtn('UseDisplayListBtn', BtnGroupSpace);
  UseDisplayListBtn := sb;
  sb.Caption := 'DL';
  sb.Hint := 'Toggle Use Display List';
  sb.AllowAllUp := True;
  sb.GroupIndex := 14;
  sb.OnClick := RotaForm.UseDisplayListBtnClick;

  sb := AddSpeedBtn('UseQuickSortBtn');
  UseQuickSortBtn := sb;
  sb.Caption := 'QS';
  sb.Hint := 'Toggle Use Quick Sort';
  sb.AllowAllUp := True;
  sb.GroupIndex := 15;
  sb.OnClick := RotaForm.UseQuickSortBtnClick;

  sb := AddSpeedBtn('Bogentn', 0);
  BogenBtn := sb;
  sb.Caption := 'B';
  sb.Hint := 'Bogen';
  sb.GroupIndex := 16;
  sb.AllowAllUp := True;
  sb.Down := RotaForm.Bogen;
  sb.OnClick := RotaForm.BogenBtnClick;
end;

procedure TFormMain.InitStatusBar;
var
  sp: TStatusPanel;
begin
  StatusBar.Panels.Clear;

  sp := StatusBar.Panels.Add;
  sp.Text := 'MenuText';
  sp.Width := Round(353 * FScale);

  sp := StatusBar.Panels.Add;
  sp.Text := 'RiggText';
  sp.Width := Round(300 * FScale);

  sp := StatusBar.Panels.Add;
  sp.Text := 'RepotLabel';
  sp.Width := Round(50 * FScale);
end;

procedure TFormMain.FormCreate2;
begin
  TL := TrimmMemo.Lines;
  ML := ReportMemo.Lines;

  InitSpeedPanel;
  InitSpeedButtons;
  InitToolbar;
  InitStatusBar;

  WantConsole := False;
  LayoutComponents;

  SetupComboBox(TrimmCombo);
  SetupComboBox(ParamCombo);
//  SetupComboBox(ViewpointCombo);
  SetupComboBox(FixpointCombo);

  SetupListBox(ReportListBox);
  SetupMemo(TrimmMemo);
  SetupMemo(ReportMemo);

  TrimmMemo.ScrollBars := TScrollStyle.ssNone;
  TrimmMemo.Width := ReportListBox.Width;

  ReportManager := TRggReportManager.Create(ReportMemo);

  InitReportListBox;
  InitTrimmCombo;
  InitParamCombo;
  InitViewpointCombo;
  InitFixpointCombo;

  TrimmCombo.ItemIndex := 0;
  ParamCombo.ItemIndex := 0;

  ReportListBox.ItemIndex := 0;

  Main.Trimm := 1;
  MT0BtnClick(nil);
  ShowTrimm;

  InitOpenDialog;
  InitSaveDialog;
  InitEventHandlers;
  InitMenu;
  if WantConsole then
    InitOutputForm;
end;

procedure TFormMain.LayoutComponents;
var
  ConsoleWidth: Integer;
  ConsoleHeight: Integer;
  ComboHeight: Integer;
begin
  TrimmMemo.Left := Margin;
  TrimmMemo.Top := SpeedPanel.Height + Margin;
  TrimmMemo.Height := Round(185 * FScale);
  TrimmMemo.Width := Round(170 * FScale);

  TrimmCombo.Left := TrimmMemo.Left;
  ParamCombo.Left := TrimmCombo.Left;
//  ViewpointCombo.Left := TrimmCombo.Left;
  FixpointCombo.Left := TrimmCombo.Left;

  TrimmCombo.Width := TrimmMemo.Width;
  ParamCombo.Width := TrimmCombo.Width;
//  ViewpointCombo.Width := TrimmCombo.Width;
  FixpointCombo.Width := TrimmCombo.Width;

  ComboHeight := TrimmCombo.Height + 2 * Margin;
  TrimmCombo.Top := TrimmMemo.Top + TrimmMemo.Height + Margin;
  ParamCombo.Top := TrimmCombo.Top + ComboHeight;
//  ViewpointCombo.Top := TrimmCombo.Top + 2 * ComboHeight;
  FixpointCombo.Top := TrimmCombo.Top + 2 * ComboHeight;

  ReportListBox.Left := TrimmMemo.Left;
  ReportListBox.Top := FixpointCombo.Top + ComboHeight;
  ReportListBox.Width := TrimmMemo.Width;
  ReportListBox.Height := StatusBar.Top - ReportListBox.Top - Margin;
  ReportListBox.Anchors := ReportListBox.Anchors + [akBottom];

  if WantConsole then
  begin
    ConsoleWidth := Round(770 * FScale) + 1 * Margin;
    ConsoleHeight := Round(457 * FScale) + 2 * Margin;
  end
  else
  begin
    ConsoleWidth := Round(500 * FScale);
    ConsoleHeight := 0;
  end;

  ReportMemo.Left := ReportListBox.Left + ReportListBox.Width + Margin;
  ReportMemo.Top := SpeedPanel.Top + SpeedPanel.Height + ConsoleHeight + Margin;
  ReportMemo.Height := StatusBar.Top - ReportMemo.Top - Margin;
  ReportMemo.Width := ConsoleWidth;
  ReportMemo.Anchors := ReportMemo.Anchors + [akBottom];

  PaintboxR.Left := ReportMemo.Left + ReportMemo.Width + Margin;
  PaintboxR.Top := SpeedPanel.Top + SpeedPanel.Height + Margin;
  PaintboxR.Width := ClientWidth - PaintboxR.Left - Margin;
  PaintboxR.Height := StatusBar.Top - PaintboxR.Top - Margin;
  PaintboxR.Anchors := PaintboxR.Anchors + [akRight, akBottom];
end;

procedure TFormMain.InitEventHandlers;
begin
  ReportListBox.OnClick := ListBoxClick;
  Self.OnMouseWheel := FormMouseWheel;

  M1Btn.OnClick := M1BtnClick;
  M10Btn.OnClick := M10BtnClick;
  P1Btn.OnClick := P1BtnClick;
  P10Btn.OnClick := P10BtnClick;
  MT0Btn.OnClick := MT0BtnClick;
  CopyAndPasteBtn.OnClick := CopyAndPasteBtnClick;
  CopyTrimmItemBtn.OnClick := CopyTrimmItemBtnClick;
  PasteTrimmItemBtn.OnClick := PasteTrimmItemBtnClick;
  ReadTrimmFileBtn.OnClick := ReadTrimmFileBtnClick;
  SaveTrimmFileBtn.OnClick := SaveTrimmFileBtnClick;

  ParamCombo.OnChange := ParamComboChange;
  TrimmCombo.OnChange := TrimmComboChange;
//  ViewpointCombo.OnChange := ViewpointComboChange;
  FixpointCombo.OnChange := FixpointComboChange;

  SandboxedBtn.OnClick := SandboxedBtnClick;
  AllPropsBtn.OnClick := nil;
  AllTagsBtn.OnClick := AllTagsBtnClick;
end;

procedure TFormMain.FormResize(Sender: TObject);
begin
  if FormClosing then
    Exit;

  if FormShown then
  begin
    Inc(Main.ResizeCounter);
    CenterRotaForm;
  end;
end;

procedure TFormMain.FormShow(Sender: TObject);
begin
  FormShown := True;
end;

procedure TFormMain.SetupComboBox(CB: TComboBox);
begin
  CB.Style := csDropDownList;
  CB.Font.Name := 'Consolas';
  CB.Font.Size := 11;
  CB.Font.Color := clRed;
end;

procedure TFormMain.SetupListBox(LB: TListBox);
begin
  LB.Font.Name := 'Consolas';
  LB.Font.Size := 11;
  LB.Font.Color := clBlue;
end;

procedure TFormMain.SetupMemo(Memo: TMemo);
begin
{$ifdef FMX}
  //Memo.Align := TAlignLayout.Client;
  Memo.ControlType := TControlType.Styled;
  Memo.StyledSettings := [];
  Memo.ShowScrollBars := True;
  Memo.TextSettings.Font.Family := 'Consolas';
  Memo.TextSettings.Font.Size := 14;
  Memo.TextSettings.FontColor := claBlack;
{$endif}

  //Memo.Align := alClient;
  //Memo.Font.Name := 'Courier New';
  Memo.Font.Name := 'Consolas';
  Memo.Font.Size := 11;
  Memo.Font.Color := clTeal;
  Memo.ScrollBars := TScrollStyle.ssBoth;
end;

procedure TFormMain.CopyTrimmItemBtnClick(Sender: TObject);
begin
  Main.CopyTrimmItem;
  ShowTrimm;
end;

procedure TFormMain.PasteTrimmItemBtnClick(Sender: TObject);
begin
  Main.PasteTrimmItem;
  ShowTrimm;
end;

procedure TFormMain.CopyAndPasteBtnClick(Sender: TObject);
begin
  Main.CopyAndPaste;
  ShowTrimm;
end;

procedure TFormMain.CopyTrimmFileBtnClick(Sender: TObject);
begin
  Main.CopyTrimmFile;
  ShowTrimm;
end;

procedure TFormMain.ReadTrimmFileBtnClick(Sender: TObject);
begin
  Main.ReadTrimmFile;
  ShowTrimm;
end;

procedure TFormMain.SaveTrimmFileBtnClick(Sender: TObject);
begin
  Main.SaveTrimmFile;
  ShowTrimm;
end;

procedure TFormMain.MT0BtnClick(Sender: TObject);
begin
  Main.UpdateTrimm0;
  //Main.FederText.UpdateText;
  ShowTrimm;
end;

procedure TFormMain.AllPropsBtnClick(Sender: TObject);
begin
  { do nothing }
end;

procedure TFormMain.AllTagsBtnClick(Sender: TObject);
begin
  MainVar.AllTags := AllTagsBtn.Down;
end;

procedure TFormMain.SandboxedBtnClick(Sender: TObject);
begin
  MainVar.IsSandboxed := SandboxedBtn.Down;
end;

procedure TFormMain.InitReportListBox;
begin
  ReportListBox.Clear;
  ReportManager.InitLB(ReportListBox.Items);
end;

procedure TFormMain.ListBoxClick(Sender: TObject);
var
  ii: Integer;
begin
  ii := ReportListBox.ItemIndex;
  if ii > -1 then
  begin
    ReportManager.CurrentIndex := ii;
    ShowCurrentReport;
  end;
end;

procedure TFormMain.ShowCurrentReport;
begin
  ReportManager.ShowCurrentReport;
  ReportLabelCaption := ReportManager.GetCurrentCaption;
end;

procedure TFormMain.InitViewpointCombo;
//var
//  cl: TStrings;
begin
//  cl := ViewpointCombo.Items;
//  cl.Add('Seite');
//  cl.Add('Achtern');
//  cl.Add('Top');
//  cl.Add('3D');
//  ViewpointCombo.DropDownCount := cl.Count;
end;

procedure TFormMain.InitFixpointCombo;
var
  cl: TStrings;
begin
  cl := FixpointCombo.Items;
  cl.Add('A0');
  cl.Add('A');
  cl.Add('B0');
  cl.Add('B');
  cl.Add('C0');
  cl.Add('C');
  cl.Add('D0');
  cl.Add('D');
  cl.Add('E0');
  cl.Add('E');
  cl.Add('F0');
  cl.Add('F');
  FixpointCombo.ItemIndex := cl.IndexOf('D');
  FixpointCombo.DropDownCount := cl.Count;
end;

function TFormMain.GetComboFixPoint: TRiggPoint;
var
  fp: TRiggPoint;
  s: string;
begin
  fp := ooD0;
  s := FixpointCombo.Text;
  if s = 'A0' then fp := ooA0
  else if s = 'B0' then fp := ooB0
  else if s = 'C0' then fp := ooC0
  else if s = 'D0' then fp := ooD0
  else if s = 'E0' then fp := ooE0
  else if s = 'F0' then fp := ooF0
  else if s = 'A' then fp := ooA
  else if s = 'B' then fp := ooB
  else if s = 'C' then fp := ooC
  else if s = 'D' then fp := ooD
  else if s = 'E' then fp := ooE
  else if s = 'F' then fp := ooF;
  result := fp;
end;

procedure TFormMain.InitParamCombo;
  procedure Add(fp: TFederParam);
  begin
    ParamCombo.Items.AddObject(Main.Param2Text(fp), TObject(fp));
  end;
begin
  if ParamCombo = nil then
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
  ParamCombo.DropDownCount := ParamCombo.Items.Count;
end;

procedure TFormMain.InitTrimmCombo;
var
  cl: TStrings;
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
  TrimmCombo.DropDownCount := cl.Count;
end;

procedure TFormMain.TrimmComboChange(Sender: TObject);
var
  t: Integer;
  ii: Integer;
begin
  ii := TrimmCombo.ItemIndex;
  t := Integer(TrimmCombo.Items.Objects[ii]);
  Main.Trimm := t;

  ML.BeginUpdate;
  try
    ML.Clear;

    //Main.CurrentTrimm.SaveTrimmFile(ML);

    Main.CurrentTrimm.WantAll := AllPropsBtn.Down;
    Main.CurrentTrimm.SaveTrimmItem(ML);
    Main.CurrentTrimm.WantAll := False;

    //Main.CurrentTrimm.WriteReport(ML);

    ReportLabelCaption := 'Trimm' + IntToStr(t);
  finally
    ML.EndUpdate;
  end;
end;

procedure TFormMain.ParamComboChange(Sender: TObject);
var
  ii: Integer;
  fp: TFederParam;
begin
  ii := ParamCombo.ItemIndex;
  fp := TFederParam(ParamCombo.Items.Objects[ii]);
  Main.Param := fp;
  ShowTrimm;
end;

procedure TFormMain.M10BtnClick(Sender: TObject);
begin
  Main.HandleAction(faParamValueMinus10);
  ShowTrimm;
end;

procedure TFormMain.M1BtnClick(Sender: TObject);
begin
  Main.HandleAction(faParamValueMinus1);
  ShowTrimm;
end;

procedure TFormMain.P10BtnClick(Sender: TObject);
begin
  Main.HandleAction(faParamValuePlus10);
  ShowTrimm;
end;

procedure TFormMain.P1BtnClick(Sender: TObject);
begin
  Main.HandleAction(faParamValuePlus1);
  ShowTrimm;
end;

procedure TFormMain.ShowTrimm;
begin
  Main.UpdateTrimmText(TL);
  ShowCurrentReport;
end;

procedure TFormMain.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  if (ssShift in Shift) or (ssCtrl in Shift) then
  begin
    Main.DoMouseWheel(Shift, WheelDelta);
    ShowTrimm;
    Handled := True;
  end;
end;

//procedure TFormRG19.ViewpointComboChange(Sender: TObject);
//var
//  ii: Integer;
//begin
//  ii := ViewpointCombo.ItemIndex;
//  case ii of
//    0: Main.HandleAction(faViewpointS);
//    1: Main.HandleAction(faViewpointA);
//    2: Main.HandleAction(faViewpointT);
//    3: Main.HandleAction(faViewpoint3);
//  end;
//end;

procedure TFormMain.FixpointComboChange(Sender: TObject);
begin
  RotaForm.FixPoint := GetComboFixPoint;
end;
(*
procedure TFormMain.BogenBtnClick(Sender: TObject);
begin
  RotaForm.BogenBtnClick(Sender);
  Main.RiggModul.UpdateUI;
  SpeedPanel.UpdateSpeedButtonDown;
  if Sender <> nil then
    Main.FederText.CheckState;
end;

procedure TFormMain.KoppelBtnClick(Sender: TObject);
begin
  RotaForm.KoppelBtnClick(Sender);
  Main.RiggModul.UpdateUI;
  SpeedPanel.UpdateSpeedButtonDown;
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
*)
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

  GrafikFormItem := AddI('GrafikFormItem');
  mi.Caption := '&Grafik ...';
  mi.Hint := '  Grafik-Ausgabeseiten separat anzeigen';
  mi.ShortCut := 16455;
  mi.OnClick := GrafikFormItemClick;

  OptionItem := AddI('OptionItem');
  mi.Caption := '&Konfiguration ...';
  mi.Hint := '  Konstanten und Parameter verändern';
  mi.ShortCut := 16459;
  mi.OnClick := OptionItemClick;

  N4 := AddI('');
  mi.Caption := '-';

  RotaFormItem := AddI('RotaFormItem');
  mi.Caption := '3D Grafik ...';
  mi.Hint := '  Rigg räumlich darstellen';
  mi.OnClick := RotaFormItemClick;

  ChartFormItem := AddI('ChartFormItem');
  mi.Caption := 'Diagramm ...';
  mi.Hint := '  Diagramm aktivieren';
  mi.OnClick := ChartFormItemClick;

  ReportFormItem := AddI('ReportFormItem');
  mi.Caption := 'Report ...';
  mi.Hint := '  Report erstellen';
  mi.OnClick := ReportFormItemClick;

  N1 := AddI('N1');
  mi.Caption := '-';

  ConsoleItem := AddI('ConsoleItem');
  mi.Caption := 'Konsole';
  mi.Enabled := True;
  mi.Checked := WantConsole;
  mi.OnClick := ConsoleItemClick;
  mi.Visible := True;

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

  CalcOffsetItem := AddI('CalcOffsetItem');
  mi.Caption := 'Grafik ausrichten';
  mi.Hint := '  2D Grafik automatisch ausrichten';
  mi.OnClick := CalcOffsetItemClick;

  AdjustFormItem := AddI('AdjustFormItem');
  mi.Caption := 'Grafik einrichten...';
  mi.Hint := '  2D Grafik verschieben und skalieren';
  mi.OnClick := AdjustFormItemClick;

  PrintItem := AddI('PrintItem');
  mi.Caption := 'Grafik exportieren...';
  mi.Hint := '  2D Grafik ausgeben';
  mi.OnClick := PrintItemClick;

  N6 := AddI('N6');
  mi.Caption := '-';

  PaintItem := AddI('PaintItem');
  mi.Caption := 'Alte Grafik stehenlassen';
  mi.GroupIndex := 1;
  mi.Hint := '  Alte Grafik löschen oder stehenlassen';
  mi.OnClick := PaintBtnClick;

  ReferenzItem := AddI('ReferenzItem');
  mi.Caption := 'Referenzstellung';
  mi.GroupIndex := 1;
  mi.Hint := '  Nullstellung einblenden';
  mi.OnClick := BtnBlauClick;

  EntlastetItem := AddI('EntlastetItem');
  mi.Caption := 'Entspanntes Rigg';
  mi.Checked := True;
  mi.GroupIndex := 1;
  mi.Hint := '  Entspanntes Rigg einblenden';
  mi.OnClick := BtnGrauClick;

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
  mi.OnClick := ZweischlagBtnClick;

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

  HilfeItem := AddI('HilfeItem');
  mi.Caption := '&Hilfe ...';
  mi.Hint := '  Hilfesystem starten';
  mi.Enabled := False;

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

procedure TFormMain.InitOutputForm;
var
  temp: Integer;
begin
  { GrafikForm }

  GrafikForm.Hide;
  GrafikForm.BorderStyle := bsNone;
  GrafikForm.Parent := Self;
  GrafikForm.Position := poDesigned;
  GrafikForm.Left := TrimmMemo.Left + TrimmMemo.Width + Margin;
  GrafikForm.Top := SpeedPanel.Top + SpeedPanel.Height + Margin;
  GrafikForm.ClientWidth := Round(305 * FScale);
  GrafikForm.ClientHeight := Round(457 * FScale);
  GrafikForm.Visible := WantConsole;
  GrafikFormItem.Checked := WantConsole;

  { InputForm }

  InputForm.Hide;
  InputForm.BorderStyle := bsNone;
  InputForm.Parent := Self;
  InputForm.Position := poDesigned;
  InputForm.Left := GrafikForm.Left + GrafikForm.Width + Margin;
  InputForm.Top := SpeedPanel.Top + SpeedPanel.Height + Margin;
    InputForm.ClientHeight := Round(195 * FScale);
    InputForm.ClientWidth := Round(465 * FScale);
  InputForm.Visible := WantConsole;
  InputFormItem.Checked := WantConsole;

  { OutputForm }

  temp := OutputForm.YComboBox.ItemIndex;
  if temp = -1 then
    temp := RiggModul.YComboSavedItemIndex;
  OutputForm.Hide;
  OutputForm.BorderStyle := bsNone;
  OutputForm.Parent := Self;
  OutputForm.Position := poDesigned;
  OutputForm.Left := InputForm.Left;
  OutputForm.Top := InputForm.Top + InputForm.Height + Margin;
    OutputForm.ClientHeight := Round(255 * FScale);
    OutputForm.ClientWidth := Round(465 * FScale);
  OutputForm.YComboBox.ItemIndex := temp;
  OutputForm.Visible := WantConsole;
  OutputFormItem.Checked := WantConsole;
end;

procedure TFormMain.ConsoleItemClick(Sender: TObject);
begin
  WantConsole := not WantConsole;
  ConsoleItem.Checked := WantConsole;
  LayoutComponents;
  InitOutputForm;
end;

procedure TFormMain.SetReportLabelCaption(const Value: string);
begin
  FReportLabelCaption := Value;
  StatusBar.Panels[2].Text := Value;
end;

procedure TFormMain.UpdateOnParamValueChanged;
begin
  ShowTrimm;
end;

procedure TFormMain.CenterRotaForm;
var
  w, h: Integer;
begin
  w := PaintboxR.Width;
  h := PaintboxR.Height;
  RotaForm.InitPosition(w, h, 0, 0);
  if FormShown then
    RotaForm.Draw;
end;

end.
