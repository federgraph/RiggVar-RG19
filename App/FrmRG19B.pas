unit FrmRG19B;

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
  RiggVar.RG.Def,
  RiggVar.RG.Report,
  IoTypes,
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
  TFormRG19B = class(TForm)
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
    KoppelkurveItem: TMenuItem;
    ZweischlagItem: TMenuItem;
    OptionenMenu: TMenuItem;
    FestItem: TMenuItem;
    DrehbarItem: TMenuItem;
    OhneItem: TMenuItem;
    OSDlgItem: TMenuItem;
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
    AutoLoadItem: TMenuItem;
    HelpMenu: TMenuItem;
    HilfeItem: TMenuItem;
    AboutItem: TMenuItem;
    LogoItem: TMenuItem;
    StatusBar: TStatusBar;
    SpeedPanel: TPanel;
    OpenBtn: TSpeedButton;
    SaveBtn: TSpeedButton;
    ExitBtn: TSpeedButton;
    UpdateBtn: TSpeedButton;
    ReglerBtn: TSpeedButton;
    MemoryBtn: TSpeedButton;
    MemoryRecallBtn: TSpeedButton;
    BtnGrau: TSpeedButton;
    KoppelBtn: TSpeedButton;
    PaintBtn: TSpeedButton;
    BtnBlau: TSpeedButton;
    ZweischlagBtn: TSpeedButton;
    LedShape: TShape;
    SofortBtn: TSpeedButton;
    DiffBtn: TSpeedButton;
    WinkelBtn: TSpeedButton;
    ControllerBtn: TSpeedButton;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    M10Btn: TSpeedButton;
    M1Btn: TSpeedButton;
    P1Btn: TSpeedButton;
    P10Btn: TSpeedButton;
    MT0Btn: TSpeedButton;
    PasteTrimmItemBtn: TSpeedButton;
    ReadTrimmFileBtn: TSpeedButton;
    SaveTrimmFileBtn: TSpeedButton;
    CopyTrimmItemBtn: TSpeedButton;
    CopyAndPasteBtn: TSpeedButton;
    TrimmCombo: TComboBox;
    ParamCombo: TComboBox;
    ViewpointCombo: TComboBox;
    FixpointCombo: TComboBox;
    Listbox: TListBox;
    ReportMemo: TMemo;
    TrimmMemo: TMemo;
    PaintBoxR: TPaintBox;

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormResize(Sender: TObject);
    procedure WindowCascadeItemClick(Sender: TObject);
    procedure WindowTileItemClick(Sender: TObject);
    procedure WindowArrangeItemClick(Sender: TObject);
    procedure WindowMinimizeItemClick(Sender: TObject);

    procedure NewItemClick(Sender: TObject);
    procedure OpenItemClick(Sender: TObject);
    procedure SaveItemClick(Sender: TObject);
    procedure SaveAsItemClick(Sender: TObject);
    procedure ExitItemClick(Sender: TObject);
    procedure PrintItemClick(Sender: TObject);

    procedure ConsoleItemClick(Sender: TObject);
    procedure ChartFormItemClick(Sender: TObject);
    procedure InputFormItemClick(Sender: TObject);
    procedure GrafikFormItemClick(Sender: TObject);
    procedure OutputFormItemClick(Sender: TObject);
    procedure RotaFormItemClick(Sender: TObject);
    procedure ReportFormItemClick(Sender: TObject);
    procedure OptionItemClick(Sender: TObject);
    procedure SpeedBarItemClick(Sender: TObject);
    procedure StatusBarItemClick(Sender: TObject);

    procedure BiegeNeigeItemClick(Sender: TObject);
    procedure rLItemClick(Sender: TObject);

    procedure VonDerSeiteItemClick(Sender: TObject);

    procedure FestItemClick(Sender: TObject);
    procedure DrehbarItemClick(Sender: TObject);
    procedure OhneItemClick(Sender: TObject);
    procedure OSDlgItemClick(Sender: TObject);
    procedure WinkelItemClick(Sender: TObject);
    procedure SofortItemClick(Sender: TObject);
    procedure DifferenzItemClick(Sender: TObject);
    procedure KnickenItemClick(Sender: TObject);
    procedure KorrigiertItemClick(Sender: TObject);
    procedure AutoLoadItemClick(Sender: TObject);
    procedure AboutItemClick(Sender: TObject);

    procedure UpdateBtnClick(Sender: TObject);
    procedure BtnGrauClick(Sender: TObject);
    procedure KoppelBtnClick(Sender: TObject);
    procedure ReglerBtnClick(Sender: TObject);
    procedure MemoryBtnClick(Sender: TObject);
    procedure MemoryRecallBtnClick(Sender: TObject);
    procedure PaintBtnClick(Sender: TObject);
    procedure BtnBlauClick(Sender: TObject);
    procedure ControllerBtnClick(Sender: TObject);
    procedure ZweischlagBtnClick(Sender: TObject);

    procedure SalingTypChanged(Sender: TObject);
    procedure ShowHint(Sender: TObject);
    procedure AdjustFormItemClick(Sender: TObject);
    procedure CalcOffsetItemClick(Sender: TObject);
    procedure LogoItemClick(Sender: TObject);

    procedure TestBtnClick(Sender: TObject);
    procedure ListBoxClick(Sender: TObject);
    procedure TrimmComboChange(Sender: TObject);
    procedure ParamComboChange(Sender: TObject);
    procedure ViewpointComboChange(Sender: TObject);
    procedure FixpointComboChange(Sender: TObject);
    procedure M10BtnClick(Sender: TObject);
    procedure M1BtnClick(Sender: TObject);
    procedure P1BtnClick(Sender: TObject);
    procedure P10BtnClick(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure cbSandboxedClick(Sender: TObject);
    procedure CopyAndPasteBtnClick(Sender: TObject);
    procedure CopyTrimmFileBtnClick(Sender: TObject);
    procedure CopyTrimmItemBtnClick(Sender: TObject);
    procedure MT0BtnClick(Sender: TObject);
    procedure PasteTrimmItemBtnClick(Sender: TObject);
    procedure ReadTrimmFileBtnClick(Sender: TObject);
    procedure SaveTrimmFileBtnClick(Sender: TObject);
    procedure PaintBtn2Click(Sender: TObject);
    procedure cbAllTagsClick(Sender: TObject);
  private
    TL: TStrings;
    ML: TStrings;
    ReportManager: TRggReportManager;

    BtnTop: Integer;
    BtnLeft: Integer;
    BtnWidth: Integer;
    BtnHeight: Integer;
    BtnCounter: Integer;
    BtnSpace: Integer;
    BtnGroupSpace: Integer;
    BtnColor: TColor;

    ComboHeight: Integer;
    ConsoleWidth: Integer;
    ConsoleHeight: Integer;

    procedure InitListBox;

    procedure InitTrimmCombo;
    procedure InitParamCombo;
    procedure InitViewpointCombo;
    procedure InitFixpointCombo;

    procedure SetupMemo(Memo: TMemo);
    procedure SetupComboBox(CB: TComboBox);
    procedure SetupListBox(LB: TListBox);
    procedure ShowTrimm;
    procedure ShowCurrentReport;
  protected
    procedure SetupLabel(L: TLabel);
  public
    ResizeCounter: Integer;
    function GetOpenFileName(dn, fn: string): string;
    function GetSaveFileName(dn, fn: string): string;

  private
    Margin: Integer;
    FReportLabelCaption: string;
    procedure wmGetMinMaxInfo(var Msg: TMessage); message wm_GetMinMaxInfo;
    procedure FormCreate1;
    procedure FormCreate2;
    procedure InitEventHandlers;
    procedure InitOutputForm;
    procedure SetReportLabelCaption(const Value: string);
    procedure LayoutComponents;
    function GetComboFixPoint: TRiggPoints;
  protected
    function AddSpeedBtn(N: string; AGroupSpace: Integer = 0): TSpeedButton;
    function RefSpeedBtn(B: TSpeedButton; AGroupSpace: Integer = 0): TSpeedButton;
    function RefShapeBtn(B: TShape; AGroupSpace: Integer): TShape;
    procedure InitToolbar;
    procedure InitOpenDialog;
    procedure InitSaveDialog;
    procedure InitStatusBar;
    procedure InitSpeedButtons;
    procedure InitSpeedPanel;
    procedure InitMenuClick;
    procedure InitMenu;
  public
    WindowCascadeItem: TMenuItem;
    WindowTileItem: TMenuItem;
    WindowArrangeItem: TMenuItem;
    WindowMinimizeItem: TMenuItem;
    procedure SetControllerEnabled;
    procedure SetControllerChecked(Value: Boolean);
    procedure SetKoppelChecked(Value: Boolean);
  public
    SandboxedBtn: TSpeedButton;
    AllPropsBtn: TSpeedButton;
    AllTagsBtn: TSpeedButton;
    property ReportLabelCaption: string read FReportLabelCaption write SetReportLabelCaption;
  public
//    PaintBtn: TSpeedButton;
//    BtnGrau: TSpeedButton;
//    BtnBlau: TSpeedButton;
//    KoppelBtn: TSpeedButton;
//    ZweischlagBtn: TSpeedButton;
  public
//    BuntBtn: TSpeedButton;
//    HullBtn: TSpeedButton;

    SeiteBtn: TSpeedButton;
    TopBtn: TSpeedButton;
    AchternBtn: TSpeedButton;
    NullBtn: TSpeedButton;

    ZoomInBtn: TSpeedButton;
    ZoomOutBtn: TSpeedButton;

    procedure SeiteBtnClick(Sender: TObject);
    procedure AchternBtnClick(Sender: TObject);
    procedure TopBtnClick(Sender: TObject);
    procedure NullBtnClick(Sender: TObject);
  end;

var
  FormRG19B: TFormRG19B;

implementation

{$R *.DFM}

uses
  RiggVar.App.Main,
  RiggVar.VM.FormMainB,
  RiggVar.RG.Main,
  RiggUnit,
  RggRota,
  FrmInfo,
  FrmConsole,
  FrmInput,
  FrmOutput,
  FrmGrafic,
  FrmText,
  FrmReport,
  FrmChart,
  FrmRot,
  FrmAniRot;

const
  SWarningText = 'Änderungen in %s sichern?';

procedure TFormRG19B.wmGetMinMaxInfo(var Msg: TMessage);
begin
  inherited;
  PMinMaxInfo(Msg.lParam)^.ptMinTrackSize.X := 900;
  PMinMaxInfo(Msg.lParam)^.ptMinTrackSize.Y := 700;
end;

procedure TFormRG19B.FormCreate(Sender: TObject);
begin
{$ifdef Debug}
   ReportMemoryLeaksOnShutdown := True;
{$endif}

  FormRG19B := Self;

  Margin := 5;
  FormCreate1;
  FormCreate2;
end;

procedure TFormRG19B.FormCreate1;
var
  rggm: TRggMain;
begin
  InputForm := TInputForm.Create(Application);
  OutputForm := TOutputForm.Create(Application);
  GrafikForm := TGrafikForm.Create(Application);

  RiggModul := TRiggModul.Create(Self);
  RiggModul.RG19A := False;
  RiggModul.ViewModelMain := TViewModelMainB.Create;
  RiggModul.Init;
  RiggModul.BackgroundColor := TColors.Wheat;

  rggm := TRggMain.Create(RiggModul.Rigg);
  RiggModul.PBG := GrafikForm.PaintBoxG;

  Main := TMain.Create(rggm);
  Main.Logger.Verbose := True;

  Left := 60;
  Top := 105;
  Height := 768;
  if Screen.Width > 1800 then
    Width := 1500
  else
    Width := 1024;

  Caption := 'Rigg';
  StatusBar.Panels[0].Text := '';
  Application.OnHint := ShowHint;

  Main.IsUp := True;

  {
    ControllerItem.Checked := True;
    ControllerBtn.Down := ControllerItem.Checked;
    WinkelItem.Checked := False;
    WinkelBtn.Down := WinkelItem.Checked;
    SofortItem.Checked := True;
    SofortBtn.Down := SofortItem.Checked;
    DifferenzItem.Checked := True;
    DiffBtn.Down := DiffItem.Checked;
    KoppelkurveItem.Checked := True;
    KoppelBtn.Down := KoppelKurveItem.Checked;
    rFItem.Checked := True;
    FestItem.Checked := True;
    AutoLoadItem := False;
    }
  AutoLoadItem.Visible := False;
  LogoItem.Checked := WantLogoData;

  Caption := 'Rigg - Form';
  OnClose := FormClose;
  OnCloseQuery := FormCloseQuery;

  RiggModul.RotaForm := TRotaForm.Create;
  RiggModul.RotaForm.PaintBox3D := PaintboxR;
  RiggModul.RotaForm.Init;
  PaintboxR := RiggModul.RotaForm.PaintBox3D;
  RiggModul.DoGraphics;
end;

procedure TFormRG19B.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Screen.OnActiveFormChange := nil;
  RiggModul.Free;
  // Application.HelpCommand(HELP_QUIT,0);
end;

procedure TFormRG19B.FormDestroy(Sender: TObject);
begin
  ReportManager.Free;

  Main.Free;
  Main := nil;
end;

procedure TFormRG19B.SalingTypChanged(Sender: TObject);
begin
  if Sender = FestItem then
    RiggModul.SalingTyp := stFest
  else if Sender = DrehbarItem then
    RiggModul.SalingTyp := stDrehbar
  else if Sender = OhneItem then
    RiggModul.SalingTyp := stOhne
  else if Sender = OSDlgItem then
    RiggModul.SalingTyp := stOhne_2;
end;

procedure TFormRG19B.rLItemClick(Sender: TObject);
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

procedure TFormRG19B.ShowHint(Sender: TObject);
begin
  StatusBar.Panels[0].Text := Application.Hint;
end;

procedure TFormRG19B.NewItemClick(Sender: TObject);
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
  Caption := 'Rigg';
end;

procedure TFormRG19B.OpenItemClick(Sender: TObject);
var
  DialogValue: Integer;
  FName: string;
begin
  if RiggModul.Modified then
  begin
    FName := Caption;
    DialogValue := MessageDlg(Format(SWarningText, [FName]), mtConfirmation,
      mbYesNoCancel, 0);
    case DialogValue of
      mrYes:
        SaveItemClick(Sender);
      { mrNo: weiter ohne speichern }
      mrCancel:
        Exit;
    end;
  end;
  if OpenDialog.Execute then
  begin
    RiggModul.Open(OpenDialog.FileName);
    Caption := 'Rigg - ' + ExtractFileName(RiggModul.IniFileName);
  end;
end;

procedure TFormRG19B.SaveItemClick(Sender: TObject);
begin
  if RiggModul.IniFileName = '' then
    SaveAsItemClick(Sender)
  else
    RiggModul.Save;
end;

procedure TFormRG19B.SaveAsItemClick(Sender: TObject);
begin
  SaveDialog.FileName := RiggModul.IniFileName;
  if SaveDialog.Execute then
  begin
    RiggModul.IniFileName := SaveDialog.FileName;
    Caption := 'Rigg - ' + ExtractFileName(RiggModul.IniFileName);
    SaveItemClick(Sender);
  end;
end;

procedure TFormRG19B.ExitItemClick(Sender: TObject);
begin
  Close;
end;

procedure TFormRG19B.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
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

procedure TFormRG19B.VonDerSeiteItemClick(Sender: TObject);
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

procedure TFormRG19B.PaintBtnClick(Sender: TObject);
begin
  PaintItem.Checked := not PaintItem.Checked;
  PaintBtn.Down := PaintItem.Checked;
  RiggModul.PaintBtnDown := PaintBtn.Down;
end;

procedure TFormRG19B.BtnBlauClick(Sender: TObject);
begin
  ReferenzItem.Checked := not ReferenzItem.Checked;
  BtnBlau.Down := ReferenzItem.Checked;
  RiggModul.BtnBlauDown := BtnBlau.Down;
end;

procedure TFormRG19B.BtnGrauClick(Sender: TObject);
begin
  EntlastetItem.Checked := not EntlastetItem.Checked;
  BtnGrau.Down := EntlastetItem.Checked;
  RiggModul.BtnGrauDown := BtnGrau.Down;
end;

procedure TFormRG19B.SetKoppelChecked(Value: Boolean);
begin
  KoppelkurveItem.Checked := Value;
  KoppelBtn.Down := Value;
  RiggModul.KoppelBtnDown := Value;
end;

procedure TFormRG19B.SetReportLabelCaption(const Value: string);
begin
  FReportLabelCaption := Value;
  StatusBar.Panels[2].Text := Value;
end;

procedure TFormRG19B.KoppelBtnClick(Sender: TObject);
begin
  SetKoppelChecked(not KoppelkurveItem.Checked);
end;

procedure TFormRG19B.ZweischlagBtnClick(Sender: TObject);
begin
  ZweischlagItem.Checked := not ZweischlagItem.Checked;
  ZweischlagBtn.Down := ZweischlagItem.Checked;
  RiggModul.ZweischlagBtnDown := ZweischlagBtn.Down;
end;

procedure TFormRG19B.WinkelItemClick(Sender: TObject);
begin
  WinkelItem.Checked := not WinkelItem.Checked;
  WinkelBtn.Down := WinkelItem.Checked;
  RiggModul.WinkelBtnDown := WinkelBtn.Down;
end;

procedure TFormRG19B.DifferenzItemClick(Sender: TObject);
begin
  DifferenzItem.Checked := not DifferenzItem.Checked;
  DiffBtn.Down := DifferenzItem.Checked;
  RiggModul.DiffBtnDown := DiffBtn.Down;
end;

procedure TFormRG19B.SofortItemClick(Sender: TObject);
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
end;

procedure TFormRG19B.SetControllerEnabled;
var
  tempBool: Boolean;
begin
  tempBool := RiggModul.ControllerEnabled;
  ControllerItem.Enabled := tempBool;
  ControllerBtn.Enabled := tempBool;
end;

procedure TFormRG19B.SetControllerChecked(Value: Boolean);
begin
  ControllerItem.Checked := Value;
  ControllerBtn.Down := Value;
  RiggModul.ControllerBtnDown := Value;
end;

procedure TFormRG19B.ControllerBtnClick(Sender: TObject);
begin
  SetControllerChecked(not ControllerItem.Checked);
end;

procedure TFormRG19B.KnickenItemClick(Sender: TObject);
begin
  if Sender = QuerKraftItem then
    RiggModul.CalcTyp := ctQuerKraftBiegung
  else if Sender = KnickenItem then
    RiggModul.CalcTyp := ctBiegeKnicken
  else if Sender = KraftGemessenItem then
    RiggModul.CalcTyp := ctKraftGemessen;
end;

procedure TFormRG19B.KorrigiertItemClick(Sender: TObject);
begin
  KorrigiertItem.Checked := not KorrigiertItem.Checked;
  RiggModul.KorrigiertItem := KorrigiertItem.Checked;
end;

procedure TFormRG19B.LogoItemClick(Sender: TObject);
begin
  WantLogoData := not WantLogoData;
  LogoItem.Checked := WantLogoData;
  RiggModul.Neu(nil);
  RiggModul.UpdateGetriebe;
end;

procedure TFormRG19B.UpdateBtnClick(Sender: TObject);
begin
  RiggModul.UpdateBtnClick;
end;

procedure TFormRG19B.BiegeNeigeItemClick(Sender: TObject);
begin
  RiggModul.BiegeNeigeItemClick;
end;

procedure TFormRG19B.ReglerBtnClick(Sender: TObject);
begin
  RiggModul.ReglerBtnClick;
  SetKoppelChecked(False);
end;

procedure TFormRG19B.MemoryBtnClick(Sender: TObject);
begin
  RiggModul.MemoryBtnClick;
end;

procedure TFormRG19B.MemoryRecallBtnClick(Sender: TObject);
begin
  RiggModul.MemoryRecallBtnClick;
end;

procedure TFormRG19B.OhneItemClick(Sender: TObject);
begin
  RiggModul.OhneItemClick;
end;

procedure TFormRG19B.DrehbarItemClick(Sender: TObject);
begin
  RiggModul.DrehbarItemClick;
end;

procedure TFormRG19B.FestItemClick(Sender: TObject);
begin
  RiggModul.FestItemClick;
end;

procedure TFormRG19B.OSDlgItemClick(Sender: TObject);
begin
  RiggModul.OSDlgItemClick;
end;

procedure TFormRG19B.OptionItemClick(Sender: TObject);
begin
  RiggModul.OptionItemClick;
end;

procedure TFormRG19B.AboutItemClick(Sender: TObject);
begin
  FrmInfo.ShowInfo;
end;

procedure TFormRG19B.PrintItemClick(Sender: TObject);
begin
  RiggModul.PrintGrafik;
end;

procedure TFormRG19B.AdjustFormItemClick(Sender: TObject);
begin
  RiggModul.AdjustGrafik;
end;

procedure TFormRG19B.CalcOffsetItemClick(Sender: TObject);
begin
  RiggModul.GetGBoxOffset;
end;

procedure TFormRG19B.WindowCascadeItemClick(Sender: TObject);
begin
  Cascade;
end;

procedure TFormRG19B.WindowTileItemClick(Sender: TObject);
begin
  TileMode := TTileMode.tbVertical;
  Tile;
end;

procedure TFormRG19B.WindowArrangeItemClick(Sender: TObject);
begin
  ArrangeIcons;
end;

procedure TFormRG19B.WindowMinimizeItemClick(Sender: TObject);
var
  i: Integer;
begin
  { Must be done backwards through the MDIChildren array }
  for i := MDIChildCount - 1 downto 0 do
    MDIChildren[i].WindowState := wsMinimized;
end;

procedure TFormRG19B.InputFormItemClick(Sender: TObject);
begin
  InputFormItem.Checked := not InputFormItem.Checked;
  if InputFormItem.Checked then
    InputForm.Show
  else
    InputForm.Hide;
end;

procedure TFormRG19B.OutputFormItemClick(Sender: TObject);
begin
  OutputFormItem.Checked := not OutputFormItem.Checked;
  if OutputFormItem.Checked then
  begin
    OutputForm.Show;
    if OutputForm.YComboBox.ItemIndex = -1 then
      OutputForm.YComboBox.ItemIndex := RiggModul.YComboSavedItemIndex;
  end
  else
    OutputForm.Hide;
end;

procedure TFormRG19B.GrafikFormItemClick(Sender: TObject);
begin
//  GrafikFormItem.Checked := not GrafikFormItem.Checked;
//  if GrafikFormItem.Checked then
//    GrafikForm.Show
//  else
//    GrafikForm.Hide;
end;

procedure TFormRG19B.ChartFormItemClick(Sender: TObject);
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

procedure TFormRG19B.ReportFormItemClick(Sender: TObject);
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

procedure TFormRG19B.RotaFormItemClick(Sender: TObject);
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

procedure TFormRG19B.ConsoleItemClick(Sender: TObject);
begin
  if RiggModul.ConsoleActive then
  begin
    ConsoleForm.Close;
    Exit;
  end;
  ConsoleForm := TConsoleForm.Create(nil);
end;

procedure TFormRG19B.SpeedBarItemClick(Sender: TObject);
begin
  SpeedBarItem.Checked := not SpeedBarItem.Checked;
  SpeedPanel.Visible := SpeedBarItem.Checked;
end;

procedure TFormRG19B.StatusBarItemClick(Sender: TObject);
begin
  StatusBarItem.Checked := not StatusBarItem.Checked;
  StatusBar.Visible := StatusBarItem.Checked;
end;

procedure TFormRG19B.AutoLoadItemClick(Sender: TObject);
begin
  AutoLoadItem.Checked := not AutoLoadItem.Checked;
end;

function TFormRG19B.GetOpenFileName(dn, fn: string): string;
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

function TFormRG19B.GetSaveFileName(dn, fn: string): string;
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

procedure TFormRG19B.InitToolbar;
begin
  OpenBtn.OnClick := OpenItemClick;
  SaveBtn.OnClick := SaveItemClick;
  ExitBtn.OnClick := ExitItemClick;

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

procedure TFormRG19B.InitOpenDialog;
begin
  OpenDialog.DefaultExt := 'ini';
  OpenDialog.Filter := 'Alle Dateien (*.*)|*.*|Rigg Einstellungen (*.rgg)|*.rgg';
  OpenDialog.FilterIndex := 2;
  OpenDialog.Options := [ofOverwritePrompt, ofPathMustExist, ofFileMustExist];
end;

procedure TFormRG19B.InitSaveDialog;
begin
  SaveDialog.DefaultExt := 'rgg';
  SaveDialog.Filter := 'Rigg Einstellungen (*.rgg)|*.rgg|Rigg IniFile (*.rgi)|*.rgi|Alle Dateien (*.*)|*.*';
  SaveDialog.Options := [ofOverwritePrompt, ofPathMustExist];
end;

procedure TFormRG19B.InitSpeedPanel;
begin
  SpeedPanel.Align := alTop;
  SpeedPanel.BevelOuter := bvNone;
  SpeedPanel.ParentShowHint := False;
  SpeedPanel.ShowHint := True;
  SpeedPanel.TabOrder := 0;
end;

function TFormRG19B.AddSpeedBtn(N: string; AGroupSpace: Integer): TSpeedButton;
begin
  result := TSpeedButton.Create(SpeedPanel);
  result.Parent := SpeedPanel;
  result.Name := N;
  RefSpeedBtn(result, AGroupSpace);
end;

function TFormRG19B.RefSpeedBtn(B: TSpeedButton; AGroupSpace: Integer): TSpeedButton;
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

function TFormRG19B.RefShapeBtn(B: TShape; AGroupSpace: Integer): TShape;
var
  temp: Integer;
begin
  temp := (BtnWidth - 10) div 2;
  result := B;
  LedShape.Left := BtnLeft + BtnCounter * BtnWidth + BtnSpace + temp;
  LedShape.Top := BtnTop + 1;
  LedShape.Width := BtnWidth - 2 * temp;
  LedShape.Height := BtnHeight - 2;
  LedShape.Brush.Color := clGreen;
  Inc(BtnCounter);
end;

procedure TFormRG19B.InitSpeedButtons;
var
  sb: TSpeedButton;
begin
  BtnCounter := 0;
  BtnLeft := 0;
  BtnTop := 3;
  BtnSpace := 3;
  BtnGroupSpace := 12;
  BtnWidth := 30;
  BtnHeight := 30;

  SpeedPanel.Height := BtnHeight + 2 * BtnTop;

  { File Menu buttons }

  sb := RefSpeedBtn(OpenBtn, BtnGroupSpace);
  sb.Hint := 'Öffnen|';
  sb.GroupIndex := 0;
  sb.OnClick := OpenItemClick;

  sb := RefSpeedBtn(SaveBtn, 0);
  sb.Hint := 'Speichern|';
  sb.GroupIndex := 0;
  sb.OnClick := SaveItemClick;

  sb := RefSpeedBtn(ExitBtn, 0);
  sb.Hint := 'Beenden|';
  sb.GroupIndex := 0;
  sb.OnClick := ExitItemClick;

  { visual group of four buttons, still group index 0 }

  sb := RefSpeedBtn(UpdateBtn, BtnGroupSpace);
  sb.Hint := 'Rigg neu Berechnen|';
  sb.GroupIndex := 0;
  sb.OnClick := UpdateBtnClick;

  sb := RefSpeedBtn(ReglerBtn, 0);
  sb.Caption := 'R';
  sb.Hint := 'Trimm Regeln|';
  sb.GroupIndex := 0;
  sb.OnClick := ReglerBtnClick;

  sb := RefSpeedBtn(MemoryBtn, 0);
  sb.Caption := 'M';
  sb.Hint := 'Memory (Trimm als Referenz speichern|)';
  sb.GroupIndex := 0;
  sb.OnClick := MemoryBtnClick;

  sb := RefSpeedBtn(MemoryRecallBtn, 0);
  sb.Caption := 'MR';
  sb.Hint := 'Memory Recall|';
  sb.GroupIndex := 0;
  sb.OnClick := MemoryRecallBtnClick;

  { Paint option buttons }

  sb := RefSpeedBtn(PaintBtn, BtnGroupSpace);
  sb.Hint := '2D Grafik - Alte Grafik stehenlassen|';
  sb.AllowAllUp := True;
  sb.Down := False;
  sb.GroupIndex := 1;
  sb.OnClick := PaintBtnClick;

  sb := RefSpeedBtn(BtnBlau, 0);
  sb.Hint := '2D Grafik - Nullstellung anzeigen|';
  sb.AllowAllUp := True;
  sb.Down := False;
  sb.GroupIndex := 2;
  sb.OnClick := BtnBlauClick;

  sb := RefSpeedBtn(BtnGrau, 0);
  sb.Hint := '2D Grafik - Entspanntes Rigg einblenden|';
  sb.AllowAllUp := True;
  sb.Down := True;
  sb.GroupIndex := 3;
  sb.OnClick := BtnGrauClick;

  sb := RefSpeedBtn(KoppelBtn, 0);
  sb.Hint := '2D Grafik - Koppelkurve anzeigen|';
  sb.AllowAllUp := True;
  sb.Down := True;
  sb.GroupIndex := 4;
  sb.OnClick := KoppelBtnClick;

  sb := RefSpeedBtn(ZweischlagBtn, 0);
  sb.Hint := '2D Grafik - Mast als Zweischlag einzeichnen|';
  sb.AllowAllUp := True;
  sb.Down := False;
  sb.GroupIndex := 5;
  sb.OnClick := ControllerBtnClick;

  { Model option buttons }

  sb := RefSpeedBtn(ControllerBtn, BtnGroupSpace);
  sb.Caption := 'C';
  sb.Hint := 'Umschalter für Controller-Modus|';
  sb.AllowAllUp := True;
  sb.Down := True;
  sb.GroupIndex := 6;
  sb.OnClick := ControllerBtnClick;

  sb := RefSpeedBtn(DiffBtn, 0);
  sb.Caption := 'D';
  sb.Hint := 'Umschalter Differenzen/Absolutwerte|';
  sb.AllowAllUp := True;
  sb.Down := False;
  sb.GroupIndex := 7;
  sb.OnClick := DifferenzItemClick;

  sb := RefSpeedBtn(WinkelBtn, 0);
  sb.Caption := 'W';
  sb.Hint := 'Umschalter Winkel/Vorstag|';
  sb.AllowAllUp := True;
  sb.Down := False;
  sb.GroupIndex := 8;
  sb.OnClick := WinkelItemClick;

  sb := RefSpeedBtn(SofortBtn, 0);
  sb.Caption := 'A';
  sb.Hint := 'Umschalter Rigg sofort berechnen (Automatik)|';
  sb.AllowAllUp := True;
  sb.Down := True;
  sb.GroupIndex := 9;
  sb.OnClick := SofortItemClick;

  { LED }

  RefShapeBtn(LedShape, BtnGroupSpace);

  { New Button group Trimm Data }

  BtnCounter := 0;
  BtnLeft := LedShape.Left + LedShape.Width; // skip over LED shape
  BtnWidth := 50; // new button width for new buttons
  BtnColor := clGreen;

  sb := RefSpeedBtn(MT0Btn, BtnGroupSpace);
  sb.Caption := 'MT0';
  sb.Hint := 'Memory Trimm 0|';
  sb.GroupIndex := 10;
  sb.OnClick := MT0BtnClick;

  BtnColor := clFuchsia;

  sb := RefSpeedBtn(ReadTrimmFileBtn, 0);
  sb.Caption := 'rtf';
  sb.Hint := 'Read Trimm File|';
  sb.GroupIndex := 10;
  sb.OnClick := ReadTrimmFileBtnClick;

  sb := RefSpeedBtn(SaveTrimmFileBtn, 0);
  sb.Caption := 'stf';
  sb.Hint := 'MT0|';
  sb.GroupIndex := 10;
  sb.OnClick := SaveTrimmFileBtnClick;

  BtnColor := clBlue;

  sb := RefSpeedBtn(CopyTrimmItemBtn, 0);
  sb.Caption := 'cti';
  sb.Hint := 'Copy Trimm Item|';
  sb.GroupIndex := 10;
  sb.OnClick := CopyTrimmItemBtnClick;

  BtnColor := clBlack;

  sb := RefSpeedBtn(PasteTrimmItemBtn, 0);
  sb.Caption := 'pti';
  sb.Hint := 'Paste Trimm Item|';
  sb.GroupIndex := 10;
  sb.OnClick := PasteTrimmItemBtnClick;

  sb := RefSpeedBtn(CopyAndPasteBtn, 0);
  sb.Caption := 'M';
  sb.Hint := 'Copy and Paste Btn|';
  sb.GroupIndex := 10;
  sb.OnClick := CopyAndPasteBtnClick;

  { Button Group Param Value Change }

  BtnColor := TColors.Teal;

  sb := RefSpeedBtn(M10Btn, BtnGroupSpace);
  sb.Caption := 'M10';
  sb.Hint := 'Param Value Minus 10|';
  sb.GroupIndex := 10;
  sb.OnClick := M10BtnClick;

  sb := RefSpeedBtn(M1Btn, 0);
  sb.Caption := 'M1';
  sb.Hint := 'Param Value Minus 1|';
  sb.GroupIndex := 10;
  sb.OnClick := M1BtnClick;

  sb := RefSpeedBtn(P1Btn, 0);
  sb.Caption := 'P1';
  sb.Hint := 'Param Value Plus 1|';
  sb.GroupIndex := 10;
  sb.OnClick := P1BtnClick;

  sb := RefSpeedBtn(P10Btn, 0);
  sb.Caption := 'P10';
  sb.Hint := 'Param Value Plus 10|';
  sb.GroupIndex := 10;
  sb.OnClick := P10BtnClick;

  { new 'checkbox' group }

  sb := AddSpeedBtn('SandboxedBtn', BtnGroupSpace);
  SandboxedBtn := sb;
  sb.Caption := 'SB';
  sb.Hint := 'Sandboxed|';
  sb.AllowAllUp := True;
  sb.Down := IsSandboxed;
  sb.GroupIndex := 11;
  sb.OnClick := cbSandboxedClick;

  sb := AddSpeedBtn('AllPropsBtn', 0);
  AllPropsBtn := sb;
  sb.Caption := 'AP';
  sb.Hint := 'All Props|';
  sb.AllowAllUp := True;
  sb.Down := False;
  sb.GroupIndex := 11;
  sb.OnClick := cbSandboxedClick;

  sb := AddSpeedBtn('AllTagsBtn', 0);
  AllTagsBtn := sb;
  sb.Caption := 'AT';
  sb.Hint := 'All Tags|';
  sb.AllowAllUp := True;
  sb.Down := False;
  sb.GroupIndex := 11;
  sb.OnClick := cbSandboxedClick;

  { TRotaForm options }

//  sb := AddSpeedBtn('HullBtn', BtnGroupSpace);
//  HullBtn := sb;
//  sb.Caption := 'Hull';
//  sb.Hint := 'Hull|';
//  sb.AllowAllUp := True;
//  sb.Down := RiggModul.RotaForm.RumpfItemChecked;
//  sb.GroupIndex := 12;
//  sb.OnClick := RiggModul.RotaForm.RumpfBtnClick;

//  sb := AddSpeedBtn('BuntBtn', 0);
//  BuntBtn := sb;
//  sb.Caption := 'Bunt';
//  sb.Hint := 'Paint Button for RotaForm|';
//  sb.AllowAllUp := True;
//  sb.Down := RiggModul.RotaForm.PaintItemChecked;
//  sb.GroupIndex := 12;
//  sb.OnClick := RiggModul.RotaForm.PaintBtnClick;

  BtnCounter := 0;
  BtnLeft := sb.Left + BtnWidth;
  BtnWidth := 30;

  sb := AddSpeedBtn('SeiteBtn', BtnGroupSpace);
  SeiteBtn := sb;
  sb.Caption := 'S';
  sb.Hint := 'Viewpoint Seite|';
  sb.AllowAllUp := True;
  sb.Down := RiggModul.RotaForm.ViewPoint = vpSeite;
  sb.GroupIndex := 13;
  sb.OnClick := SeiteBtnClick;

  sb := AddSpeedBtn('AchternBtn', 0);
  AchternBtn := sb;
  sb.Caption := 'A';
  sb.Hint := 'Viewpoint Achtern|';
  sb.AllowAllUp := False;
  sb.Down := RiggModul.RotaForm.ViewPoint = vpAchtern;
  sb.GroupIndex := 13;
  sb.OnClick := AchternBtnClick;

  sb := AddSpeedBtn('TopBtn', 0);
  TopBtn := sb;
  sb.Caption := 'T';
  sb.Hint := 'Viewpoint Top|';
  sb.AllowAllUp := False;
  sb.Down := RiggModul.RotaForm.ViewPoint = vpTop;
  sb.GroupIndex := 13;
  sb.OnClick := TopBtnClick;

  sb := AddSpeedBtn('NullBtn', 0);
  NullBtn := sb;
  sb.Caption := '3D';
  sb.Hint := 'Viewpoint 3D|';
  sb.AllowAllUp := False;
  sb.Down := RiggModul.RotaForm.ViewPoint = vp3D;
  sb.GroupIndex := 13;
  sb.OnClick := NullBtnClick;

  sb := AddSpeedBtn('ZoomOutBtn', BtnGroupSpace);
  ZoomOutBtn := sb;
  sb.Caption := 'Z-';
  sb.Hint := 'Zoom Out|';
  sb.GroupIndex := 0;
  sb.OnClick := RiggModul.RotaForm.ZoomOutBtnClick;

  sb := AddSpeedBtn('ZoomInBtn', 0);
  ZoomInBtn := sb;
  sb.Caption := 'Z+';
  sb.Hint := 'Zoom In|';
  sb.GroupIndex := 0;
  sb.OnClick := RiggModul.RotaForm.ZoomInBtnClick;
end;

procedure TFormRG19B.InitStatusBar;
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
  sp.Text := 'RepotLabel';
  sp.Width := 50;
end;

procedure TFormRG19B.FormCreate2;
begin
  TL := TrimmMemo.Lines;
  ML := ReportMemo.Lines;

  InitSpeedPanel;
  InitSpeedButtons;
  InitToolbar;
  InitStatusBar;

  LayoutComponents;

  SetupComboBox(TrimmCombo);
  SetupComboBox(ParamCombo);
  SetupComboBox(ViewpointCombo);
  SetupComboBox(FixpointCombo);

  SetupListBox(ListBox);
  SetupMemo(TrimmMemo);
  SetupMemo(ReportMemo);

  TrimmMemo.ScrollBars := TScrollStyle.ssNone;
  TrimmMemo.Width := ListBox.Width;

  ReportManager := TRggReportManager.Create(ReportMemo);

  InitListBox;
  InitTrimmCombo;
  InitParamCombo;
  InitViewpointCombo;
  InitFixpointCombo;

  TrimmCombo.ItemIndex := 0;
  ParamCombo.ItemIndex := 0;
  ViewpointCombo.ItemIndex := 0;
  FixpointCombo.ItemIndex := FixpointCombo.Items.IndexOf('D0');

  ListBox.ItemIndex := 0;

  Main.Trimm := 1;
  MT0BtnClick(nil);
  ShowTrimm;

  InitEventHandlers;
  InitMenu;
  InitOutputForm;
end;

procedure TFormRG19B.LayoutComponents;
begin
  TrimmMemo.Left := Margin;
  TrimmMemo.Top := SpeedPanel.Height + Margin;
  TrimmMemo.Height := 185;
  TrimmMemo.Width := 170;

  TrimmCombo.Left := TrimmMemo.Left;
  ParamCombo.Left := TrimmCombo.Left;
  ViewpointCombo.Left := TrimmCombo.Left;
  FixpointCombo.Left := TrimmCombo.Left;

  TrimmCombo.Width := TrimmMemo.Width;
  ParamCombo.Width := TrimmCombo.Width;
  ViewpointCombo.Width := TrimmCombo.Width;
  FixpointCombo.Width := TrimmCombo.Width;

  ComboHeight := TrimmCombo.Height + 2 * Margin;
  TrimmCombo.Top := TrimmMemo.Top + TrimmMemo.Height + Margin;
  ParamCombo.Top := TrimmCombo.Top + ComboHeight;
  ViewpointCombo.Top := TrimmCombo.Top + 2 * ComboHeight;
  FixpointCombo.Top := TrimmCombo.Top + 3 * ComboHeight;

  Listbox.Left := TrimmMemo.Left;
  Listbox.Top := FixpointCombo.Top + ComboHeight + Margin;
  Listbox.Width := TrimmMemo.Width;
  Listbox.Height := StatusBar.Top - Listbox.top - Margin;
  Listbox.Anchors := Listbox.Anchors + [akBottom];

  ConsoleWidth := 770 + 1 * Margin;
  ConsoleHeight := 457;

  ReportMemo.Left := Listbox.Left + Listbox.Width + Margin;
  ReportMemo.Top := SpeedPanel.Top + SpeedPanel.Height + ConsoleHeight + 2 * Margin;
  ReportMemo.Height := StatusBar.Top - ReportMemo.Top - Margin;
  ReportMemo.Width := ConsoleWidth;
  ReportMemo.Anchors := ReportMemo.Anchors + [akBottom];

  PaintboxR.Left := ReportMemo.Left + ReportMemo.Width + Margin;
  PaintboxR.Top := SpeedPanel.Top + SpeedPanel.Height + Margin;
  PaintboxR.Width := ClientWidth - PaintboxR.Left - Margin;
  PaintboxR.Height := StatusBar.Top - PaintboxR.Top - Margin;
  PaintboxR.Anchors := PaintboxR.Anchors + [akRight, akBottom];
end;

procedure TFormRG19B.InitEventHandlers;
begin
  ListBox.OnClick := ListBoxClick;
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
  ViewpointCombo.OnChange := ViewpointComboChange;
  FixpointCombo.OnChange := FixpointComboChange;

  SandboxedBtn.OnClick := cbSandboxedClick;
  AllPropsBtn.OnClick := nil;
  AllTagsBtn.OnClick := cbAllTagsClick;
end;

procedure TFormRG19B.FormResize(Sender: TObject);
begin
  Inc(ResizeCounter);
end;

procedure TFormRG19B.SetupLabel(L: TLabel);
begin
  L.Font.Name := 'Consolas';
  L.Font.Size := 11;
  L.Font.Color := clPurple;
end;

procedure TFormRG19B.SetupComboBox(CB: TComboBox);
begin
  CB.Style := csDropDownList;
  CB.DropDownCount := Integer(High(TFederParam));
  CB.Font.Name := 'Consolas';
  CB.Font.Size := 11;
  CB.Font.Color := clRed;
end;

procedure TFormRG19B.SetupListBox(LB: TListBox);
begin
  LB.Font.Name := 'Consolas';
  LB.Font.Size := 11;
  LB.Font.Color := clBlue;
end;

procedure TFormRG19B.SetupMemo(Memo: TMemo);
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

procedure TFormRG19B.CopyTrimmItemBtnClick(Sender: TObject);
begin
  Main.CopyTrimmItem;
  ShowTrimm;
end;

procedure TFormRG19B.PasteTrimmItemBtnClick(Sender: TObject);
begin
  Main.PasteTrimmItem;
  ShowTrimm;
end;

procedure TFormRG19B.CopyAndPasteBtnClick(Sender: TObject);
begin
  Main.CopyAndPaste;
  ShowTrimm;
end;

procedure TFormRG19B.CopyTrimmFileBtnClick(Sender: TObject);
begin
  Main.CopyTrimmFile;
  ShowTrimm;
end;

procedure TFormRG19B.ReadTrimmFileBtnClick(Sender: TObject);
begin
  Main.ReadTrimmFile;
  ShowTrimm;
end;

procedure TFormRG19B.SaveTrimmFileBtnClick(Sender: TObject);
begin
  Main.SaveTrimmFile;
  ShowTrimm;
end;

procedure TFormRG19B.MT0BtnClick(Sender: TObject);
begin
  Main.UpdateTrimm0;
  //Main.FederText.UpdateText;
  ShowTrimm;
end;

procedure TFormRG19B.cbAllTagsClick(Sender: TObject);
begin
  ReportManager.XmlAllTags := AllTagsBtn.Down;
end;

procedure TFormRG19B.cbSandboxedClick(Sender: TObject);
begin
  IsSandboxed := SandboxedBtn.Down;
end;

procedure TFormRG19B.InitListBox;
begin
  Listbox.Clear;
  ReportManager.InitLB(ListBox.Items);
end;

procedure TFormRG19B.ListBoxClick(Sender: TObject);
var
  ii: Integer;
begin
  ii := Listbox.ItemIndex;
  if ii > -1 then
  begin
    ReportManager.CurrentIndex := ii;
    ShowCurrentReport;
  end;
end;

procedure TFormRG19B.ShowCurrentReport;
begin
  ReportManager.ShowCurrentReport;
  ReportLabelCaption := ReportManager.GetCurrentCaption;
end;

procedure TFormRG19B.TestBtnClick(Sender: TObject);
begin
  ReportMemo.Lines.Clear;
  Main.RggData.WriteReport(ML);
end;

procedure TFormRG19B.InitViewpointCombo;
var
  cl: TStrings;
begin
  cl := ViewpointCombo.Items;
  cl.Add('Seite');
  cl.Add('Achtern');
  cl.Add('Top');
  cl.Add('3D');
end;

procedure TFormRG19B.InitFixpointCombo;
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
end;

function TFormRG19B.GetComboFixPoint: TRiggPoints;
var
  NewFixName: TRiggPoints;
  s: string;
begin
  NewFixName := ooD0;
  s := FixpointCombo.Text;
  if s = 'A0' then NewFixName := ooA0
  else if s = 'B0' then NewFixName := ooB0
  else if s = 'C0' then NewFixName := ooC0
  else if s = 'D0' then NewFixName := ooD0
  else if s = 'E0' then NewFixName := ooE0
  else if s = 'F0' then NewFixName := ooF0
  else if s = 'A' then NewFixName := ooA
  else if s = 'B' then NewFixName := ooB
  else if s = 'C' then NewFixName := ooC
  else if s = 'D' then NewFixName := ooD
  else if s = 'E' then NewFixName := ooE
  else if s = 'F' then NewFixName := ooF;
  result := NewFixName;
end;

procedure TFormRG19B.InitParamCombo;
  procedure ACI(fp: TFederParam);
  var
    s: string;
  begin
    s := Main.RggMain.Param2Text(fp);
    ParamCombo.Items.AddObject(s, TObject(fp));
  end;
begin
  ACI(fpVorstag);
  ACI(fpWinkel);
  ACI(fpController);
  ACI(fpWante);
  ACI(fpWoben);
  ACI(fpSalingH);
  ACI(fpSalingA);
  ACI(fpSalingL);
  ACI(fpSalingW);
  ACI(fpMastfallF0C);
  ACI(fpMastfallF0F);
  ACI(fpMastfallVorlauf);
  ACI(fpBiegung);
  ACI(fpD0X);
end;

procedure TFormRG19B.InitTrimmCombo;
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
end;

procedure TFormRG19B.TrimmComboChange(Sender: TObject);
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

procedure TFormRG19B.ParamComboChange(Sender: TObject);
var
  ii: Integer;
  fp: TFederParam;
begin
  ii := ParamCombo.ItemIndex;
  fp := TFederParam(ParamCombo.Items.Objects[ii]);
  Main.RggMain.Param := fp;
  ShowTrimm;
end;

procedure TFormRG19B.M10BtnClick(Sender: TObject);
begin
  Main.HandleAction(faParamValueMinus10);
  ShowTrimm;
end;

procedure TFormRG19B.M1BtnClick(Sender: TObject);
begin
  Main.HandleAction(faParamValueMinus1);
  ShowTrimm;
end;

procedure TFormRG19B.P10BtnClick(Sender: TObject);
begin
  Main.HandleAction(faParamValuePlus10);
  ShowTrimm;
end;

procedure TFormRG19B.P1BtnClick(Sender: TObject);
begin
  Main.HandleAction(faParamValuePlus1);
  ShowTrimm;
end;

procedure TFormRG19B.ShowTrimm;
begin
  Main.RggMain.UpdateTrimmText(TL);
  ShowCurrentReport;
end;

procedure TFormRG19B.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  if (ssShift in Shift) or (ssCtrl in Shift) then
  begin
    Main.DoMouseWheel(Shift, WheelDelta);
    ShowTrimm;
    Handled := True;
  end;
end;

procedure TFormRG19B.PaintBtn2Click(Sender: TObject);
begin
  Main.RggMain.UpdateGraph;
end;

procedure TFormRG19B.ViewpointComboChange(Sender: TObject);
var
  ii: Integer;
begin
  ii := ViewpointCombo.ItemIndex;
  case ii of
    0: Main.HandleAction(faViewpointS);
    1: Main.HandleAction(faViewpointA);
    2: Main.HandleAction(faViewpointT);
    3: Main.HandleAction(faViewpoint3);
  end;
end;

procedure TFormRG19B.FixpointComboChange(Sender: TObject);
begin
  RiggModul.RotaForm.FixPoint := GetComboFixPoint;
end;

procedure TFormRG19B.InitMenuClick;
begin
  { File }

  NewItem.OnClick := NewItemClick;
  OpenItem.OnClick := OpenItemClick;
  SaveItem.OnClick := SaveItemClick;
  SaveAsItem.OnClick := SaveAsItemClick;

  ExitItem.OnClick := ExitItemClick;

  { Bearbeiten }

  RecalcItem.OnClick := UpdateBtnClick;
  BiegeNeigeItem.OnClick := BiegeNeigeItemClick;
  ReglerItem.OnClick := ReglerBtnClick;
  MemoryItem.OnClick := MemoryBtnClick;
  MemoryRecallItem.OnClick := MemoryRecallBtnClick;

  { Ansicht }

//  InputFormItem.OnClick := InputFormItemClick;
//  OutputFormItem.OnClick := OutputFormItemClick;
//  GrafikFormItem.OnClick := GrafikFormItemClick;

  OptionItem.OnClick := OptionItemClick;

  ConsoleItem.OnClick := ConsoleItemClick;
  RotaFormItem.OnClick := RotaFormItemClick;
  ChartFormItem.OnClick := ChartFormItemClick;
  ReportFormItem.OnClick := ReportFormItemClick;

  SpeedBarItem.OnClick := SpeedBarItemClick;
  StatusBarItem.OnClick := StatusBarItemClick;

  { Memo }

  rLItem.OnClick := rLItemClick;
  rLeItem.OnClick := rLItemClick;
  rFItem.OnClick := rLItemClick;
  rPItem.OnClick := rLItemClick;
  rPeItem.OnClick := rLItemClick;
  DiffLItem.OnClick := rLItemClick;
  DiffPItem.OnClick := rLItemClick;
  LogItem.OnClick := rLItemClick;

  { Grafik }

  VonDerSeiteItem.OnClick := VonDerSeiteItemClick;
  VonHintenItem.OnClick := VonDerSeiteItemClick;
  VonObenItem.OnClick := VonDerSeiteItemClick;
  Von3DItem.OnClick := VonDerSeiteItemClick;

  CalcOffsetItem.OnClick := CalcOffsetItemClick;
  AdjustFormItem.OnClick := AdjustFormItemClick;
  PrintItem.OnClick := PrintItemClick;

  PaintItem.OnClick := PaintBtnClick;
  ReferenzItem.OnClick := BtnBlauClick;
  EntlastetItem.OnClick := BtnGrauClick;
  KoppelkurveItem.OnClick := KoppelBtnClick;
  ZweischlagItem.OnClick := ZweischlagBtnClick;

  { Optionen }

  FestItem.OnClick := SalingTypChanged;
  DrehbarItem.OnClick := SalingTypChanged;
  OhneItem.OnClick := SalingTypChanged;
  OSDlgItem.OnClick := SalingTypChanged;

  ControllerItem.OnClick := ControllerBtnClick;
  DifferenzItem.OnClick := DifferenzItemClick;
  WinkelItem.OnClick := WinkelItemClick;
  SofortItem.OnClick := SofortItemClick;

  QuerKraftItem.OnClick := KnickenItemClick;
  KnickenItem.OnClick := KnickenItemClick;
  KraftGemessenItem.OnClick := KnickenItemClick;

  KorrigiertItem.OnClick := KorrigiertItemClick;

  AutoLoadItem.OnClick := AutoLoadItemClick; //not visible

  { Window }

//  WindowCascadeItem.OnClick := WindowCascadeItemClick;
//  WindowTileItem.OnClick := WindowTileItemClick;
//  WindowArrangeItem.OnClick := WindowArrangeItemClick;
//  WindowMinimizeItem.OnClick := WindowMinimizeItemClick;

  { Help }

//  HilfeItem.OnClick := HilfeItemClick;
  AboutItem.OnClick := AboutItemClick;
  LogoItem.OnClick := LogoItemClick;
end;

procedure TFormRG19B.InitMenu;
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
  mi.Caption := 'Trimm &zur'#252'cksetzen ( MR )';
  mi.Hint := '  Trimm aus dem Zwischenspeicher zurückholen';
  mi.OnClick := MemoryRecallBtnClick;

  { Ansicht }

  AnsichtMenu := AddP('AnsichtMenu');
  mi.Caption := '&Ansicht';
  mi.GroupIndex := 2;
  mi.Hint := '  Fenster anzeigen und verbergen';

  InputFormItem := AddI('InputItem');
  mi.Caption := '&Eingabe ...';
  mi.Hint := '  Eingabeseiten im eigenen Fenster anzeigen';
  mi.ShortCut := 16453;
  mi.OnClick := InputFormItemClick;
  mi.Visible := False;

  OutputFormItem := AddI('OutputFormItem');
  mi.Caption := '&Ausgabe ...';
  mi.Hint := '  Ausgabeseiten im eigenen Fenster anzeigen';
  mi.ShortCut := 16449;
  mi.OnClick := OutputFormItemClick;
  mi.Visible := False;

  GrafikFormItem := AddI('GrafikFormItem');
  mi.Caption := '&Grafik ...';
  mi.Hint := '  Grafik-Ausgabeseiten separat anzeigen';
  mi.ShortCut := 16455;
  mi.OnClick := GrafikFormItemClick;
  mi.Visible := False;

  OptionItem := AddI('OptionItem');
  mi.Caption := '&Konfiguration ...';
  mi.Hint := '  Konstanten und Parameter verändern';
  mi.ShortCut := 16459;
  mi.OnClick := OptionItemClick;

  N4 := AddI('');
  mi.Caption := '-';

  ConsoleItem := AddI('ConsoleItem');
  mi.Caption := 'Konsole';
  mi.Enabled := False;
  mi.OnClick := ConsoleItemClick;
  mi.Visible := False;

  RotaFormItem := AddI('RotaFormItem');
  mi.Caption := '3D Grafik ...';
  mi.Hint := '  Rigg r'#228'umlich darstellen';
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

  { Memo }

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
  mi.Hint := '  Koordinaten (Rigg verformt ) anzeigen';
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
  mi.Hint := '  Rigg schr'#228'g von oben gesehen darstellen';
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

  KoppelkurveItem := AddI('KoppelkurveItem');
  mi.Caption := 'Koppelkurve';
  mi.Checked := True;
  mi.GroupIndex := 1;
  mi.Hint := '  Koppelkurve einblenden';
  mi.OnClick := KoppelBtnClick;

  ZweischlagItem := AddI('ZweischlagItem');
  mi.Caption := 'Mast als Zweischlag zeichnen';
  mi.GroupIndex := 1;
  mi.Hint := '  Mast als Bogen oder Zweischlag zeichnen';
  mi.OnClick := ZweischlagBtnClick;

  { Optionen }

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

  OhneItem := AddI('OhneItem');
  mi.Caption := 'ohne Salinge / Mast biegt aus';
  mi.Hint := '  Modell: Biegeknicken des Mastes ohne Salinge';
  mi.RadioItem := True;
  mi.OnClick := SalingTypChanged;

  OSDlgItem := AddI('OSDlgItem');
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
  mi.Hint := ' Wanten-Winkel oder Vorstagl'#228'nge einstellen';
  mi.OnClick := WinkelItemClick;

  SofortItem := AddI('SofortItem');
  mi.Caption := 'Rigg automatisch berechnen ( A )';
  mi.Checked := True;
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

  AutoLoadItem := AddI('AutoLoadItem');
  mi.Caption := 'Datensatz automatisch laden';
  mi.GroupIndex := 3;
  mi.Hint := '  Datensätze aus Datenbank einlesen, wenn selektiert';
  mi.OnClick := AutoLoadItemClick;

  { Window }

  if RiggModul.RG19A then
  begin
    WindowMenu := AddP('WindowMenu');
    mi.Caption := '&Fenster';
    mi.GroupIndex := 9;
    mi.Hint := '  MDI Fenster verwalten';

    WindowCascadeItem := AddI('WindowCascadeItem');
    mi.Caption := '&'#220'berlappend';
    mi.Hint := '  Fenster überlappend anordnen';
    mi.OnClick := WindowCascadeItemClick;

    WindowTileItem := AddI('WindowTileItem');
    mi.Caption := '&Nebeneinander';
    mi.Hint := '  Fenster nebeneinander anordnen';
    mi.OnClick := WindowTileItemClick;

    WindowArrangeItem := AddI('WindowArrangeItem');
    mi.Caption := '&Symbole anordnen';
    mi.Hint := '  Fenstersymbole anordnen';
    mi.OnClick := WindowArrangeItemClick;

    WindowMinimizeItem := AddI('WindowMinimizeItem');
    mi.Caption := '&Alle verkleinern';
    mi.Hint := '  Alle Fenster zum Symbol verkleinern';
    mi.OnClick := WindowMinimizeItemClick;
  end;

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

  AboutItem := AddI('AboutItem');
  mi.Caption := '&Info...';
  mi.Hint := '  Infofenster anzeigen';
  mi.OnClick := AboutItemClick;

  LogoItem := AddI('LogoItem');
  mi.Caption := 'Logo';
  mi.OnClick := LogoItemClick;
end;

procedure TFormRG19B.SeiteBtnClick(Sender: TObject);
begin
  RiggModul.RotaForm.ViewPoint := vpSeite;
end;

procedure TFormRG19B.AchternBtnClick(Sender: TObject);
begin
  RiggModul.RotaForm.ViewPoint := vpAchtern;
end;

procedure TFormRG19B.TopBtnClick(Sender: TObject);
begin
  RiggModul.RotaForm.ViewPoint := vpTop;
end;

procedure TFormRG19B.NullBtnClick(Sender: TObject);
begin
  RiggModul.RotaForm.ViewPoint := vp3D;
end;

procedure TFormRG19B.InitOutputForm;
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
  GrafikForm.ClientWidth := 305;
  GrafikForm.ClientHeight := 457;
  GrafikForm.Visible := True;

  { InputForm }

  InputForm.Hide;
  InputForm.BorderStyle := bsNone;
  InputForm.Parent := Self;
  InputForm.Position := poDesigned;
  InputForm.Left := GrafikForm.Left + GrafikForm.Width + Margin;
  InputForm.Top := SpeedPanel.Top + SpeedPanel.Height + Margin;
  InputForm.ClientHeight := 195;
  InputForm.ClientWidth := 465;
  InputForm.Visible := True;

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
  OutputForm.ClientHeight := 255;
  OutputForm.ClientWidth := 465;
  OutputForm.YComboBox.ItemIndex := temp;
  OutputForm.Visible := True;
end;

end.
