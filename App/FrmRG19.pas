unit FrmRG19;

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
  System.UITypes,
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
  Vcl.StdCtrls;

type
  TFormRG19 = class(TForm)
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
    MemoryRecallkItem: TMenuItem;
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
    WindowMenu: TMenuItem;
    WindowCascadeItem: TMenuItem;
    WindowTileItem: TMenuItem;
    WindowArrangeItem: TMenuItem;
    WindowMinimizeItem: TMenuItem;
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
    BtnGrau: TSpeedButton;
    KoppelBtn: TSpeedButton;
    ReglerBtn: TSpeedButton;
    MemoryBtn: TSpeedButton;
    MemoryRecallBtn: TSpeedButton;
    PaintBtn: TSpeedButton;
    LedShape: TShape;
    BtnBlau: TSpeedButton;
    SofortBtn: TSpeedButton;
    DiffBtn: TSpeedButton;
    WinkelBtn: TSpeedButton;
    ControllerBtn: TSpeedButton;
    ZweischlagBtn: TSpeedButton;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    Panel: TPanel;
    ReportLabel: TLabel;
    M10Btn: TSpeedButton;
    M1Btn: TSpeedButton;
    P1Btn: TSpeedButton;
    P10Btn: TSpeedButton;
    CopyAndPasteBtn: TSpeedButton;
    CopyTrimmItemBtn: TSpeedButton;
    MT0Btn: TSpeedButton;
    PasteTrimmItemBtn: TSpeedButton;
    ReadTrimmFileBtn: TSpeedButton;
    SaveTrimmFileBtn: TSpeedButton;
    ParamCombo: TComboBox;
    TrimmMemo: TMemo;
    TrimmCombo: TComboBox;
    cbSandboxed: TCheckBox;
    cbAllProps: TCheckBox;
    ViewpointCombo: TComboBox;
    cbAllTags: TCheckBox;
    ListBox: TListBox;
    ReportMemo: TMemo;

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormResize(Sender: TObject);

    procedure UpdateMenuItems(Sender: TObject);
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

    procedure SalingTypChange(Sender: TObject);
    procedure ShowHint(Sender: TObject);
    procedure AdjustFormItemClick(Sender: TObject);
    procedure CalcOffsetItemClick(Sender: TObject);
    procedure LogoItemClick(Sender: TObject);

    procedure TestBtnClick(Sender: TObject);
    procedure ListBoxClick(Sender: TObject);
    procedure TrimmComboChange(Sender: TObject);
    procedure ParamComboChange(Sender: TObject);
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
    procedure ViewpointComboChange(Sender: TObject);
    procedure PaintBtn2Click(Sender: TObject);
    procedure cbAllTagsClick(Sender: TObject);
  private
    TL: TStrings;
    ML: TStrings;
    ReportManager: TRggReportManager;

    BtnTop: Integer;
    BtnLeft: Integer;
    BtnWidth: Integer;
    BtnCounter: Integer;
    BtnColor: TColor;

    procedure SetupMemo(Memo: TMemo);
    procedure InitListBox;
    procedure InitTrimmCombo;
    procedure InitParamCombo;
    procedure SetupComboBox(CB: TComboBox);
    procedure SetupListBox(LB: TListBox);
    procedure SetupLabel(L: TLabel);
    procedure AB(B: TSpeedButton);
    procedure ShowTrimm;
    procedure ShowCurrentReport;
    procedure InitViewpointCombo;
  public
    ResizeCounter: Integer;
    function GetOpenFileName(dn, fn: string): string;
    function GetSaveFileName(dn, fn: string): string;

  private
    procedure wmGetMinMaxInfo(var Msg: TMessage); message wm_GetMinMaxInfo;
    procedure FormCreate1;
    procedure FormCreate2;
    procedure InitEventHandlers;
  protected
    function AddSpeedBtn(N: string; L: Integer): TSpeedButton;
    procedure InitToolbar;
    procedure InitOpenDialog;
    procedure InitSaveDialog;
    procedure InitStatusBar;
    procedure InitSpeedButtons;
    procedure InitSpeedPanel;
    procedure InitLED;
    procedure InitMenu;
  public
    procedure SetControllerEnabled;
    procedure SetControllerChecked(Value: Boolean);
    procedure SetKoppelChecked(Value: Boolean);
  end;

var
  FormRG19: TFormRG19;

implementation

{$R *.DFM}

uses
  RiggVar.App.Main,
  RiggVar.RG.Main,
  RiggUnit,
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

procedure TFormRG19.wmGetMinMaxInfo(var Msg: TMessage);
begin
  inherited;
  PMinMaxInfo(Msg.lParam)^.ptMinTrackSize.X := 600;
  PMinMaxInfo(Msg.lParam)^.ptMinTrackSize.Y := 220;
end;

procedure TFormRG19.FormCreate(Sender: TObject);
begin
  FormRG19 := Self;

  FormCreate1;
  FormCreate2;
end;

procedure TFormRG19.FormCreate1;
var
  rggm: TRggMain;
begin
  InputForm := TInputForm.Create(Application);
  OutputForm := TOutputForm.Create(Application);
  GrafikForm := TGrafikForm.Create(Application);

  RiggModul := TRiggModul.Create(Self);
  rggm := TRggMain.Create(RiggModul.Rigg);

  Main := TMain.Create(rggm);
  Main.Logger.Verbose := True;

  Left := 60;
  Top := 105;
  Height := 768;
  if Screen.Width > 1800 then
    Width := 1500
  else
    Width := 1024;

  Screen.OnActiveFormChange := UpdateMenuItems;

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
  InitToolbar;

  Caption := 'Rigg - Form';
  OnClose := FormClose;
  OnCloseQuery := FormCloseQuery;
end;

procedure TFormRG19.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Screen.OnActiveFormChange := nil;
  RiggModul.Free;
  // Application.HelpCommand(HELP_QUIT,0);
end;

procedure TFormRG19.FormDestroy(Sender: TObject);
begin
  ReportManager.Free;

  Main.Free;
  Main := nil;
end;

procedure TFormRG19.SalingTypChange(Sender: TObject);
begin
  if Sender = FestItem then
    RiggModul.SalingTypChanged(stFest)
  else if Sender = DrehbarItem then
    RiggModul.SalingTypChanged(stDrehbar)
  else if Sender = OhneItem then
    RiggModul.SalingTypChanged(stOhne)
  else if Sender = OSDlgItem then
    RiggModul.SalingTypChanged(stOhne_2);
end;

procedure TFormRG19.rLItemClick(Sender: TObject);
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

procedure TFormRG19.ShowHint(Sender: TObject);
begin
  StatusBar.Panels[0].Text := Application.Hint;
end;

procedure TFormRG19.NewItemClick(Sender: TObject);
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

procedure TFormRG19.OpenItemClick(Sender: TObject);
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

procedure TFormRG19.SaveItemClick(Sender: TObject);
begin
  if RiggModul.IniFileName = '' then
    SaveAsItemClick(Sender)
  else
    RiggModul.Save;
end;

procedure TFormRG19.SaveAsItemClick(Sender: TObject);
begin
  SaveDialog.FileName := RiggModul.IniFileName;
  if SaveDialog.Execute then
  begin
    RiggModul.IniFileName := SaveDialog.FileName;
    Caption := 'Rigg - ' + ExtractFileName(RiggModul.IniFileName);
    SaveItemClick(Sender);
  end;
end;

procedure TFormRG19.ExitItemClick(Sender: TObject);
begin
  Close;
end;

procedure TFormRG19.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
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

procedure TFormRG19.VonDerSeiteItemClick(Sender: TObject);
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

procedure TFormRG19.PaintBtnClick(Sender: TObject);
begin
  PaintItem.Checked := not PaintItem.Checked;
  PaintBtn.Down := PaintItem.Checked;
  RiggModul.PaintBtnDown := PaintBtn.Down;
end;

procedure TFormRG19.BtnBlauClick(Sender: TObject);
begin
  ReferenzItem.Checked := not ReferenzItem.Checked;
  BtnBlau.Down := ReferenzItem.Checked;
  RiggModul.BtnBlauDown := BtnBlau.Down;
end;

procedure TFormRG19.BtnGrauClick(Sender: TObject);
begin
  EntlastetItem.Checked := not EntlastetItem.Checked;
  BtnGrau.Down := EntlastetItem.Checked;
  RiggModul.BtnGrauDown := BtnGrau.Down;
end;

procedure TFormRG19.SetKoppelChecked(Value: Boolean);
begin
  KoppelkurveItem.Checked := Value;
  KoppelBtn.Down := Value;
  RiggModul.KoppelBtnDown := Value;
end;

procedure TFormRG19.KoppelBtnClick(Sender: TObject);
begin
  SetKoppelChecked(not KoppelkurveItem.Checked);
end;

procedure TFormRG19.ZweischlagBtnClick(Sender: TObject);
begin
  ZweischlagItem.Checked := not ZweischlagItem.Checked;
  ZweischlagBtn.Down := ZweischlagItem.Checked;
  RiggModul.ZweischlagBtnDown := ZweischlagBtn.Down;
end;

procedure TFormRG19.WinkelItemClick(Sender: TObject);
begin
  WinkelItem.Checked := not WinkelItem.Checked;
  WinkelBtn.Down := WinkelItem.Checked;
  RiggModul.WinkelBtnDown := WinkelBtn.Down;
end;

procedure TFormRG19.DifferenzItemClick(Sender: TObject);
begin
  DifferenzItem.Checked := not DifferenzItem.Checked;
  DiffBtn.Down := DifferenzItem.Checked;
  RiggModul.DiffBtnDown := DiffBtn.Down;
end;

procedure TFormRG19.SofortItemClick(Sender: TObject);
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

procedure TFormRG19.SetControllerEnabled;
var
  tempBool: Boolean;
begin
  tempBool := RiggModul.ControllerEnabled;
  ControllerItem.Enabled := tempBool;
  ControllerBtn.Enabled := tempBool;
end;

procedure TFormRG19.SetControllerChecked(Value: Boolean);
begin
  ControllerItem.Checked := Value;
  ControllerBtn.Down := Value;
  RiggModul.ControllerBtnDown := Value;
end;

procedure TFormRG19.ControllerBtnClick(Sender: TObject);
begin
  SetControllerChecked(not ControllerItem.Checked);
end;

procedure TFormRG19.KnickenItemClick(Sender: TObject);
begin
  if Sender = QuerKraftItem then
    RiggModul.CalcTypChanged(ctQuerKraftBiegung)
  else if Sender = KnickenItem then
    RiggModul.CalcTypChanged(ctBiegeKnicken)
  else if Sender = KraftGemessenItem then
    RiggModul.CalcTypChanged(ctKraftGemessen);
end;

procedure TFormRG19.KorrigiertItemClick(Sender: TObject);
begin
  KorrigiertItem.Checked := not KorrigiertItem.Checked;
  RiggModul.KorrigiertItem := KorrigiertItem.Checked;
end;

procedure TFormRG19.LogoItemClick(Sender: TObject);
begin
  WantLogoData := not WantLogoData;
  LogoItem.Checked := WantLogoData;
  RiggModul.Neu(nil);
  RiggModul.UpdateGetriebe;
end;

procedure TFormRG19.UpdateBtnClick(Sender: TObject);
begin
  RiggModul.UpdateBtnClick;
end;

procedure TFormRG19.BiegeNeigeItemClick(Sender: TObject);
begin
  RiggModul.BiegeNeigeItemClick;
end;

procedure TFormRG19.ReglerBtnClick(Sender: TObject);
begin
  RiggModul.ReglerBtnClick;
  SetKoppelChecked(False);
end;

procedure TFormRG19.MemoryBtnClick(Sender: TObject);
begin
  RiggModul.MemoryBtnClick;
end;

procedure TFormRG19.MemoryRecallBtnClick(Sender: TObject);
begin
  RiggModul.MemoryRecallBtnClick;
end;

procedure TFormRG19.OhneItemClick(Sender: TObject);
begin
  RiggModul.OhneItemClick;
end;

procedure TFormRG19.DrehbarItemClick(Sender: TObject);
begin
  RiggModul.DrehbarItemClick;
end;

procedure TFormRG19.FestItemClick(Sender: TObject);
begin
  RiggModul.FestItemClick;
end;

procedure TFormRG19.OSDlgItemClick(Sender: TObject);
begin
  RiggModul.OSDlgItemClick;
end;

procedure TFormRG19.OptionItemClick(Sender: TObject);
begin
  RiggModul.OptionItemClick;
end;

procedure TFormRG19.AboutItemClick(Sender: TObject);
begin
  FrmInfo.ShowInfo;
end;

procedure TFormRG19.PrintItemClick(Sender: TObject);
begin
  RiggModul.PrintGrafik;
end;

procedure TFormRG19.AdjustFormItemClick(Sender: TObject);
begin
  RiggModul.AdjustGrafik;
end;

procedure TFormRG19.CalcOffsetItemClick(Sender: TObject);
begin
  RiggModul.GetGBoxOffset;
end;

procedure TFormRG19.WindowCascadeItemClick(Sender: TObject);
begin
  Cascade;
end;

procedure TFormRG19.WindowTileItemClick(Sender: TObject);
begin
  TileMode := TTileMode.tbVertical;
  Tile;
end;

procedure TFormRG19.WindowArrangeItemClick(Sender: TObject);
begin
  ArrangeIcons;
end;

procedure TFormRG19.WindowMinimizeItemClick(Sender: TObject);
var
  i: Integer;
begin
  { Must be done backwards through the MDIChildren array }
  for i := MDIChildCount - 1 downto 0 do
    MDIChildren[i].WindowState := wsMinimized;
end;

procedure TFormRG19.UpdateMenuItems(Sender: TObject);
begin
  WindowCascadeItem.Enabled := MDIChildCount > 0;
  WindowTileItem.Enabled := MDIChildCount > 0;
  WindowArrangeItem.Enabled := MDIChildCount > 0;
  WindowMinimizeItem.Enabled := MDIChildCount > 0;
end;

procedure TFormRG19.InputFormItemClick(Sender: TObject);
begin
  InputFormItem.Checked := not InputFormItem.Checked;
  if InputFormItem.Checked then
    InputForm.Show
  else
    InputForm.Hide;
end;

procedure TFormRG19.GrafikFormItemClick(Sender: TObject);
begin
  GrafikFormItem.Checked := not GrafikFormItem.Checked;
  if GrafikFormItem.Checked then
    GrafikForm.Show
  else
    GrafikForm.Hide;
end;

procedure TFormRG19.OutputFormItemClick(Sender: TObject);
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

procedure TFormRG19.ChartFormItemClick(Sender: TObject);
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

procedure TFormRG19.ReportFormItemClick(Sender: TObject);
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

procedure TFormRG19.RotaFormItemClick(Sender: TObject);
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

procedure TFormRG19.ConsoleItemClick(Sender: TObject);
begin
  if RiggModul.ConsoleActive then
  begin
    ConsoleForm.Close;
    Exit;
  end;
  ConsoleForm := TConsoleForm.Create(nil);
end;

procedure TFormRG19.SpeedBarItemClick(Sender: TObject);
begin
  SpeedBarItem.Checked := not SpeedBarItem.Checked;
  SpeedPanel.Visible := SpeedBarItem.Checked;
end;

procedure TFormRG19.StatusBarItemClick(Sender: TObject);
begin
  StatusBarItem.Checked := not StatusBarItem.Checked;
  StatusBar.Visible := StatusBarItem.Checked;
end;

procedure TFormRG19.AutoLoadItemClick(Sender: TObject);
begin
  AutoLoadItem.Checked := not AutoLoadItem.Checked;
end;

function TFormRG19.GetOpenFileName(dn, fn: string): string;
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

function TFormRG19.GetSaveFileName(dn, fn: string): string;
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

procedure TFormRG19.InitToolbar;
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

procedure TFormRG19.InitOpenDialog;
begin
  OpenDialog.DefaultExt := 'ini';
  OpenDialog.Filter := 'Alle Dateien (*.*)|*.*|Rigg Einstellungen (*.rgg)|*.rgg';
  OpenDialog.FilterIndex := 2;
  OpenDialog.Options := [ofOverwritePrompt, ofPathMustExist, ofFileMustExist];
end;

procedure TFormRG19.InitSaveDialog;
begin
  SaveDialog.DefaultExt := 'rgg';
  SaveDialog.Filter := 'Rigg Einstellungen (*.rgg)|*.rgg|Rigg IniFile (*.rgi)|*.rgi|Alle Dateien (*.*)|*.*';
  SaveDialog.Options := [ofOverwritePrompt, ofPathMustExist];
end;

procedure TFormRG19.InitSpeedPanel;
begin
  SpeedPanel.Align := alTop;
  SpeedPanel.BevelOuter := bvNone;
  SpeedPanel.ParentShowHint := False;
  SpeedPanel.ShowHint := True;
  SpeedPanel.TabOrder := 0;
end;

function TFormRG19.AddSpeedBtn(N: string; L: Integer): TSpeedButton;
begin
  result := TSpeedButton.Create(SpeedPanel);
  result.Name := N;
  result.Left := L;
  result.Top := 3;
  result.Width := 25;
  result.Height := 25;
end;

procedure TFormRG19.InitSpeedButtons;
var
  sb: TSpeedButton;
begin
  sb := AddSpeedBtn('OpenBtn', 8);
  sb.Hint := 'Öffnen|';
  sb.OnClick := OpenItemClick;

  sb := AddSpeedBtn('SaveBtn', 33);
  sb.Hint := 'Speichern|';
  sb.OnClick := SaveItemClick;

  sb := AddSpeedBtn('ExitBtn', 66);
  sb.Hint := 'Beenden|';
  sb.OnClick := ExitItemClick;

  sb := AddSpeedBtn('UpdateBtn', 113);
  sb.Hint := 'Rigg neu Berechnen|';
  sb.OnClick := UpdateBtnClick;

  sb := AddSpeedBtn('BtnGrau', 294);
  sb.Hint := '2D Grafik - Entspanntes Rigg einblenden|';
  sb.AllowAllUp := True;
  sb.Down := True;
  sb.GroupIndex := 2;
  sb.OnClick := BtnGrauClick;

  sb := AddSpeedBtn('KoppelBtn', 319);
  sb.Hint := '2D Grafik - Koppelkurve anzeigen|';
  sb.AllowAllUp := True;
  sb.Down := True;
  sb.GroupIndex := 2;
  sb.OnClick := KoppelBtnClick;

  sb := AddSpeedBtn('ReglerBtn', 138);
  sb.Caption := 'R';
  sb.Hint := 'Trimm Regeln|';
  sb.OnClick := ReglerBtnClick;

  sb := AddSpeedBtn('MemoryBtn', 163);
  sb.Caption := 'M';
  sb.Hint := 'Memory (Trimm als Referenz speichern|)';
  sb.OnClick := MemoryBtnClick;

  sb := AddSpeedBtn('MemoryRecallBtn', 188);
  sb.Caption := 'MR';
  sb.Hint := 'Memory Recall|';
  sb.OnClick := MemoryRecallBtnClick;

  sb := AddSpeedBtn('PaintBtn', 244);
  sb.Hint := '2D Grafik - Alte Grafik stehenlassen|';
  sb.AllowAllUp := True;
  sb.Down := False;
  sb.GroupIndex := 1;
  sb.OnClick := PaintBtnClick;

  sb := AddSpeedBtn('BtnBlau', 269);
  sb.Hint := '2D Grafik - Nullstellung anzeigen|';
  sb.AllowAllUp := True;
  sb.Down := False;
  sb.GroupIndex := 4;
  sb.OnClick := BtnBlauClick;

  sb := AddSpeedBtn('SofortBtn', 485);
  sb.Caption := 'A';
  sb.Hint := 'Umschalter Rigg sofort berechnen (Automatik)|';
  sb.AllowAllUp := True;
  sb.Down := True;
  sb.GroupIndex := 8;
  sb.OnClick := SofortItemClick;

  sb := AddSpeedBtn('DiffBtn', 423);
  sb.Caption := 'D';
  sb.Hint := 'Umschalter Differenzen/Absolutwerte|';
  sb.AllowAllUp := True;
  sb.Down := False;
  sb.GroupIndex := 7;
  sb.OnClick := DifferenzItemClick;

  sb := AddSpeedBtn('WinkelBtn', 454);
  sb.Caption := 'W';
  sb.Hint := 'Umschalter Winkel/Vorstag|';
  sb.AllowAllUp := True;
  sb.Down := False;
  sb.GroupIndex := 9;
  sb.OnClick := WinkelItemClick;

  sb := AddSpeedBtn('ControllerBtn', 404);
  sb.Caption := 'C';
  sb.Hint := 'Umschalter für Controller-Modus|';
  sb.AllowAllUp := True;
  sb.Down := True;
  sb.GroupIndex := 6;
  sb.OnClick := ControllerBtnClick;

  sb := AddSpeedBtn('ZweischlagBtn', 344);
//  sb.Caption := 'Z';
  sb.Hint := '2D Grafik - Mast als Zweischlag einzeichnen|';
  sb.AllowAllUp := True;
  sb.Down := False;
  sb.GroupIndex := 5;
  sb.OnClick := ControllerBtnClick;

end;

procedure TFormRG19.InitLED;
begin
//  LedShape := TShape.Create(Self);
  LedShape.Left := 520;
  LedShape.Top := 7;
  LedShape.Width := 9;
  LedShape.Height := 16;
  LedShape.Brush.Color := clGreen;
end;

procedure TFormRG19.InitMenu;
var
  mi: TMenuItem;
begin

  mi := FileMenu;
  mi.Caption := '&Datei';
  mi.Hint := '  Dateibefehle';

  mi := NewItem;
  mi.Caption := '&Neu';
  mi.Hint := '  Standardwerte laden';
  mi.OnClick := NewItemClick;

  mi := OpenItem;
  mi.Caption := '&Öffnen ...';
  mi.Hint := '  Konfiguration aus Datei laden';
  mi.OnClick := OpenItemClick;

  mi := SaveItem;
  mi.Caption := '&Speichern';
  mi.Hint := '  Konfiguration in Datei speichern';
  mi.OnClick := SaveItemClick;

  mi := N9;
  mi.Caption := '-';

  mi := ExitItem;
  mi.Caption := '&Beenden';
  mi.Hint := '  Anwendung verlassen';
  //mi.ShortCut := 32856;
  mi.OnClick := ExitItemClick;

  { Bearbeiten Menu }

  mi := BearbeitenMenu;
  mi.Caption := '&Bearbeiten';
  mi.GroupIndex := 2;
  mi.Hint := '  Bearbeitungsbefehle';

  mi := RecalcItem;
  mi.Caption := 'Neu &berechnen ( = )';
  mi.Hint := '  Rigg neu berechnen';
  mi.OnClick := UpdateBtnClick;

  mi := BiegeNeigeItem;
  mi.Caption := 'Biegen und &Neigen ...';
  mi.Hint := '  Mastbiegung und Mastfall einstellen';
  mi.OnClick := BiegeNeigeItemClick;

  mi := ReglerItem;
  mi.Caption := 'Trimm &regeln ... ( R )';
  mi.Hint := '  Trimm automatisch einstellen';
  mi.OnClick := ReglerBtnClick;

  mi := MemoryItem;
  mi.Caption := 'Trimm &speichern ( M )';
  mi.Hint := '  Trimm in den Zwischenspeicher kopieren';
  mi.OnClick := MemoryBtnClick;

  mi := MemoryRecallkItem;
  mi.Caption := 'Trimm &zur'#252'cksetzen ( MR )';
  mi.Hint := '  Trimm aus dem Zwischenspeicher zurückholen';
  mi.OnClick := MemoryRecallBtnClick;

  { Ansicht Menu }

  mi := AnsichtMenu;
  mi.Caption := '&Ansicht';
  mi.GroupIndex := 2;
  mi.Hint := '  Fenster anzeigen und verbergen';

  mi := InputFormItem;
  mi.Caption := '&Eingabe ...';
  mi.Hint := '  Eingabeseiten im eigenen Fenster anzeigen';
  mi.ShortCut := 16453;
  mi.OnClick := InputFormItemClick;

  mi := OutputFormItem;
  mi.Caption := '&Ausgabe ...';
  mi.Hint := '  Ausgabeseiten im eigenen Fenster anzeigen';
  mi.ShortCut := 16449;
  mi.OnClick := OutputFormItemClick;

  mi := GrafikFormItem;
  mi.Caption := '&Grafik ...';
  mi.Hint := '  Grafik-Ausgabeseiten separat anzeigen';
  mi.ShortCut := 16455;
  mi.OnClick := GrafikFormItemClick;

  mi := OptionItem;
  mi.Caption := '&Konfiguration ...';
  mi.Hint := '  Konstanten und Parameter verändern';
  mi.ShortCut := 16459;
  mi.OnClick := OptionItemClick;

  mi := N4;
  mi.Caption := '-';

  mi := ConsoleItem;
  mi.Caption := 'Konsole';
  mi.OnClick := ConsoleItemClick;

  mi := RotaFormItem;
  mi.Caption := '3D Grafik ...';
  mi.Hint := '  Rigg r'#228'umlich darstellen';
  mi.OnClick := RotaFormItemClick;

  mi := ChartFormItem;
  mi.Caption := 'Diagramm ...';
  mi.Hint := '  Diagramm aktivieren';
  mi.OnClick := ChartFormItemClick;

  mi := ReportFormItem;
  mi.Caption := 'Report ...';
  mi.Hint := '  Report erstellen';
  mi.OnClick := ReportFormItemClick;

  mi := N1;
  mi.Caption := '-';

  mi := SpeedBarItem;
  mi.Caption := 'Symbolleiste';
  mi.Checked := True;
  mi.Hint := '  Symbolleiste einblenden';
  mi.OnClick := SpeedBarItemClick;

  mi := StatusBarItem;
  mi.Caption := 'Statusleiste';
  mi.Checked := True;
  mi.Hint := '  Statusleiste einblenden';
  mi.OnClick := StatusBarItemClick;

  { Memo Menu }

  mi := MemoMenu;
  mi.Caption := '&Tabellen';
  mi.GroupIndex := 3;
  mi.Hint := '  Tabelle für Anzeige im Memo auswählen';

  mi := rLItem;
  mi.Caption := 'rL';
  mi.Hint := '  Längen (Rigg verformt) anzeigen';
  mi.RadioItem := True;
  mi.ShortCut := 16460;
  mi.OnClick := rLItemClick;

  mi := rLeItem;
  mi.Caption := 'rLe';
  mi.Hint := '  Längen (Rigg entspannt) anzeigen';
  mi.RadioItem := True;
  mi.OnClick := rLItemClick;

  mi := rFItem;
  mi.Caption := 'rF';
  mi.Checked := True;
  mi.Hint := '  Kräfte anzeigen';
  mi.RadioItem := True;
  mi.ShortCut := 16454;
  mi.OnClick := rLItemClick;

  mi := rPItem;
  mi.Caption := 'rP';
  mi.Hint := '  Koordinaten (Rigg verformt ) anzeigen';
  mi.RadioItem := True;
  mi.ShortCut := 16464;
  mi.OnClick := rLItemClick;

  mi := rPeItem;
  mi.Caption := 'rPe';
  mi.Hint := '  Koordinaten (Rigg entlastet) anzeigen';
  mi.RadioItem := True;
  mi.OnClick := rLItemClick;

  mi := DiffLItem;
  mi.Caption := 'Diff_L';
  mi.Hint := '  Längendifferenzen (entlastet - belastet) anzeigen';
  mi.RadioItem := True;
  mi.OnClick := rLItemClick;

  mi := DiffPItem;
  mi.Caption := 'Diff_P';
  mi.Hint := '  Punktverschiebungen (entlastet - belastet) anzeigen';
  mi.RadioItem := True;
  mi.OnClick := rLItemClick;

  mi := LogItem;
  mi.Caption := 'Log';
  mi.Hint := '  Log anzeigen';
  mi.RadioItem := True;
  mi.OnClick := rLItemClick;

  { Grafik Menu }

  mi := GrafikMenu;
  mi.Caption := '&Grafik';
  mi.GroupIndex := 3;
  mi.Hint := '  2D Grafikoptionen ';

  mi := VonDerSeiteItem;
  mi.Caption := 'Seitenansicht';
  mi.Checked := True;
  mi.Hint := '  Rigg von der Seite gesehen darstellen';
  mi.RadioItem := True;
  mi.OnClick := VonDerSeiteItemClick;

  mi := VonHintenItem;
  mi.Caption := 'Blick von Achtern';
  mi.Hint := '  Rigg von hinten gesehen darstellen';
  mi.RadioItem := True;
  mi.OnClick := VonDerSeiteItemClick;

  mi := VonObenItem;
  mi.Caption := 'Draufsicht';
  mi.Hint := '  Rigg von oben gesehen darstellen';
  mi.RadioItem := True;
  mi.OnClick := VonDerSeiteItemClick;

  mi := Von3DItem;
  mi.Caption := 'Perspektive';
  mi.Hint := '  Rigg schr'#228'g von oben gesehen darstellen';
  mi.RadioItem := True;
  mi.OnClick := VonDerSeiteItemClick;

  mi := N3;
  mi.Caption := '-';

  mi := CalcOffsetItem;
  mi.Caption := 'Grafik ausrichten';
  mi.Hint := '  2D Grafik automatisch ausrichten';
  mi.OnClick := CalcOffsetItemClick;

  mi := AdjustFormItem;
  mi.Caption := 'Grafik einrichten...';
  mi.Hint := '  2D Grafik verschieben und skalieren';
  mi.OnClick := AdjustFormItemClick;

  mi := PrintItem;
  mi.Caption := 'Grafik exportieren...';
  mi.Hint := '  2D Grafik ausgeben';
  mi.OnClick := PrintItemClick;

  mi := N6;
  mi.Caption := '-';

  mi := PaintItem;
  mi.Caption := 'Alte Grafik stehenlassen';
  mi.GroupIndex := 1;
  mi.Hint := '  Alte Grafik löschen oder stehenlassen';
  mi.OnClick := PaintBtnClick;

  mi := ReferenzItem;
  mi.Caption := 'Referenzstellung';
  mi.GroupIndex := 1;
  mi.Hint := '  Nullstellung einblenden';
  mi.OnClick := BtnBlauClick;

  mi := EntlastetItem;
  mi.Caption := 'Entspanntes Rigg';
  mi.Checked := True;
  mi.GroupIndex := 1;
  mi.Hint := '  Entspanntes Rigg einblenden';
  mi.OnClick := BtnGrauClick;

  mi := KoppelkurveItem;
  mi.Caption := 'Koppelkurve';
  mi.Checked := True;
  mi.GroupIndex := 1;
  mi.Hint := '  Koppelkurve einblenden';
  mi.OnClick := KoppelBtnClick;

  mi := ZweischlagItem;
  mi.Caption := 'Mast als Zweischlag zeichnen';
  mi.GroupIndex := 1;
  mi.Hint := '  Mast als Bogen oder Zweischlag zeichnen';
  mi.OnClick := ZweischlagBtnClick;

  { Optionen Menu }

  mi := OptionenMenu;
  mi.Caption := '&Modell';
  mi.GroupIndex := 3;
  mi.Hint := '  Modell - und Berechnungsoptionen';

  mi := FestItem;
  mi.Caption := 'feste Salinge';
  mi.Checked := True;
  mi.Hint := '  Modell: Salinge starr befestigt';
  mi.RadioItem := True;
  mi.OnClick := SalingTypChange;

  mi := DrehbarItem;
  mi.Caption := 'drehbare Salinge';
  mi.Hint := '  Modell: Salinge drehbar angelenkt';
  mi.RadioItem := True;
  mi.OnClick := SalingTypChange;

  mi := OhneItem;
  mi.Caption := 'ohne Salinge / Mast biegt aus';
  mi.Hint := '  Modell: Biegeknicken des Mastes ohne Salinge';
  mi.RadioItem := True;
  mi.OnClick := SalingTypChange;

  mi := OSDlgItem;
  mi.Caption := 'ohne Saling / Mast starr';
  mi.Hint := '  Modell: Mast steif ohne Salinge';
  mi.RadioItem := True;
  mi.OnClick := SalingTypChange;

  mi := N11;
  mi.Caption := '-';
  mi.GroupIndex := 1;

  mi := ControllerItem;
  mi.Caption := 'Controller ( C )';
  mi.Checked := True;
  mi.GroupIndex := 1;
  mi.Hint := '  Mastcontroller berücksichtigen';
  mi.ShortCut := 16451;
  mi.OnClick := ControllerBtnClick;

  mi := DifferenzItem;
  mi.Caption := 'Differenzen ( D )';
  mi.GroupIndex := 1;
  mi.Hint := '  Länge als Differenz oder Absolutwert anzeigen';
  mi.ShortCut := 16452;
  mi.OnClick := DifferenzItemClick;

  mi := WinkelItem;
  mi.Caption := 'Winkel einstellbar ( W )';
  mi.GroupIndex := 1;
  mi.Hint := ' Wanten-Winkel oder Vorstagl'#228'nge einstellen';
  mi.OnClick := WinkelItemClick;

  mi := SofortItem;
  mi.Caption := 'Rigg automatisch berechnen ( A )';
  mi.Checked := True;
  mi.GroupIndex := 1;
  mi.Hint := '  Rigg (Kräfte) automatisch berechnen';
  mi.OnClick := SofortItemClick;

  mi := N8;
  mi.Caption := '-';
  mi.GroupIndex := 2;

  mi := QuerKraftItem;
  mi.Caption := 'QuerKraftBiegung';
  mi.GroupIndex := 2;
  mi.Hint := '  Kraftberechnung nur mit Querkraftbiegung - kein Knicken';
  mi.RadioItem := True;
  mi.OnClick := KnickenItemClick;

  mi := KnickenItem;
  mi.Caption := 'Biegeknicken';
  mi.Checked := True;
  mi.GroupIndex := 2;
  mi.Hint := '  Biegeknicken bei der Kraftberechnung berücksichtigen';
  mi.RadioItem := True;
  mi.OnClick := KnickenItemClick;

  mi := KraftGemessenItem;
  mi.Caption := 'gemessene Kraftwerte verwenden';
  mi.GroupIndex := 2;
  mi.Hint := '  Kräfte aus der Trimmtabelle entnehmen';
  mi.RadioItem := True;
  mi.OnClick := KnickenItemClick;

  mi := N2;
  mi.Caption := '-';
  mi.GroupIndex := 3;

  mi := KorrigiertItem;
  mi.Caption := 'BiegeKnicken korrigiert';
  mi.Checked := True;
  mi.GroupIndex := 3;
  mi.Hint := '  Anteil der Salingkraft an der Mastbiegung beachten';
  mi.OnClick := KorrigiertItemClick;

  mi := AutoLoadItem;
  mi.Caption := 'Datensatz automatisch laden';
  mi.GroupIndex := 3;
  mi.Hint := '  Datensätze aus Datenbank einlesen, wenn selektiert';
  mi.OnClick := AutoLoadItemClick;

  { Window Menu }

  mi := WindowMenu;
  mi.Caption := '&Fenster';
  mi.GroupIndex := 9;
  mi.Hint := '  MDI Fenster verwalten';

  mi := WindowCascadeItem;
  mi.Caption := '&'#220'berlappend';
  mi.Hint := '  Fenster überlappend anordnen';
  mi.OnClick := WindowCascadeItemClick;

  mi := WindowTileItem;
  mi.Caption := '&Nebeneinander';
  mi.Hint := '  Fenster nebeneinander anordnen';
  mi.OnClick := WindowTileItemClick;

  mi := WindowArrangeItem;
  mi.Caption := '&Symbole anordnen';
  mi.Hint := '  Fenstersymbole anordnen';
  mi.OnClick := WindowArrangeItemClick;

  mi := WindowMinimizeItem;
  mi.Caption := '&Alle verkleinern';
  mi.Hint := '  Alle Fenster zum Symbol verkleinern';
  mi.OnClick := WindowMinimizeItemClick;

  { Help Menu }

  mi := HelpMenu;
  mi.Caption := '&Hilfe';
  mi.GroupIndex := 10;
  mi.Hint := '  Hilfethemen';

  mi := HilfeItem;
  mi.Caption := '&Hilfe ...';
  mi.Hint := '  Hilfesystem starten';

  mi := AboutItem;
  mi.Caption := '&Info...';
  mi.Hint := '  Infofenster anzeigen';
  mi.OnClick := AboutItemClick;

  mi := LogoItem;
  mi.Caption := 'Logo';
  mi.OnClick := LogoItemClick;
end;

procedure TFormRG19.InitStatusBar;
var
  sp: TStatusPanel;
begin
  sp := StatusBar.Panels.Add;
  sp.Text := 'MenuText';
  sp.Width := 353;

  sp := StatusBar.Panels.Add;
  sp.Text := 'RiggText';
  sp.Width := 50;
end;

procedure TFormRG19.FormCreate2;
begin
//  Left := 810;
//  Height := 640;

  TL := TrimmMemo.Lines;
  ML := ReportMemo.Lines;

  Panel.ShowCaption := False;
  Panel.Align := alTop;
  Listbox.Align := alLeft;
  TrimmMemo.Align := alLeft;
  ReportMemo.Align := alLeft;

  BtnCounter := 0;
  BtnWidth := 50;
  BtnTop := MT0Btn.Top;
  BtnLeft := MT0Btn.Left;

  BtnColor := clGreen;
  AB(MT0Btn);

  BtnColor := clFuchsia;
  AB(ReadTrimmFileBtn);
  AB(SaveTrimmFileBtn);

  BtnColor := clBlue;
  AB(CopyTrimmItemBtn);
  AB(PasteTrimmItemBtn);

  BtnColor := clBlack;
  AB(CopyAndPasteBtn);

  BtnCounter := 0;
  BtnTop := M10Btn.Top;
  BtnLeft := M10Btn.Left;

  BtnColor := clTeal;
  AB(M10Btn);
  AB(M1Btn);
  AB(P1Btn);
  AB(P10Btn);

  SetupComboBox(TrimmCombo);
  SetupComboBox(ParamCombo);
  SetupComboBox(ViewpointCombo);
  SetupLabel(ReportLabel);
  SetupListBox(ListBox);
  SetupMemo(TrimmMemo);
  SetupMemo(ReportMemo);

  TrimmMemo.ScrollBars := ssNone;
  TrimmMemo.Width := ListBox.Width;

  ReportManager := TRggReportManager.Create(ReportMemo);

  InitListBox;
  InitTrimmCombo;
  InitParamCombo;
  InitViewpointCombo;

  TrimmCombo.ItemIndex := 0;
  ParamCombo.ItemIndex := 0;
  ViewpointCombo.ItemIndex := 0;
  ListBox.ItemIndex := 0;

  Main.Trimm := 1;
  MT0BtnClick(nil);
  ShowTrimm;

  InitEventHandlers;
end;

procedure TFormRG19.InitEventHandlers;
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

  cbSandboxed.OnClick := cbSandboxedClick;
  cbAllProps.OnClick := nil;
  cbAllTags.OnClick := cbAllTagsClick;
end;

procedure TFormRG19.FormResize(Sender: TObject);
begin
  Inc(ResizeCounter);
end;

procedure TFormRG19.AB(B: TSpeedButton);
begin
  B.Left := BtnLeft + BtnCounter * BtnWidth + 12;
  B.Width := 45;
  B.Height := 30;
  B.Font.Name := 'Consolas';
  B.Font.Size := 12;
  B.Font.Color := BtnColor;
  Inc(BtnCounter);
end;

procedure TFormRG19.SetupLabel(L: TLabel);
begin
  L.Font.Name := 'Consolas';
  L.Font.Size := 11;
  L.Font.Color := clPurple;
end;

procedure TFormRG19.SetupComboBox(CB: TComboBox);
begin
  CB.Style := csDropDownList;
  CB.DropDownCount := Integer(High(TFederParam));
  CB.Font.Name := 'Consolas';
  CB.Font.Size := 11;
  CB.Font.Color := clRed;
end;

procedure TFormRG19.SetupListBox(LB: TListBox);
begin
  LB.Font.Name := 'Consolas';
  LB.Font.Size := 11;
  LB.Font.Color := clBlue;
end;

procedure TFormRG19.SetupMemo(Memo: TMemo);
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
  Memo.ScrollBars := ssBoth;
end;

procedure TFormRG19.CopyTrimmItemBtnClick(Sender: TObject);
begin
  Main.CopyTrimmItem;
  ShowTrimm;
end;

procedure TFormRG19.PasteTrimmItemBtnClick(Sender: TObject);
begin
  Main.PasteTrimmItem;
  ShowTrimm;
end;

procedure TFormRG19.CopyAndPasteBtnClick(Sender: TObject);
begin
  Main.CopyAndPaste;
  ShowTrimm;
end;

procedure TFormRG19.CopyTrimmFileBtnClick(Sender: TObject);
begin
  Main.CopyTrimmFile;
  ShowTrimm;
end;

procedure TFormRG19.ReadTrimmFileBtnClick(Sender: TObject);
begin
  Main.ReadTrimmFile;
  ShowTrimm;
end;

procedure TFormRG19.SaveTrimmFileBtnClick(Sender: TObject);
begin
  Main.SaveTrimmFile;
  ShowTrimm;
end;

procedure TFormRG19.MT0BtnClick(Sender: TObject);
begin
  Main.UpdateTrimm0;
  //Main.FederText.UpdateText;
  ShowTrimm;
end;

procedure TFormRG19.cbAllTagsClick(Sender: TObject);
begin
  ReportManager.XmlAllTags := cbAllTags.Checked;
end;

procedure TFormRG19.cbSandboxedClick(Sender: TObject);
begin
  IsSandboxed := cbSandboxed.Checked;
end;

procedure TFormRG19.InitListBox;
begin
  ReportManager.InitLB(ListBox.Items);
end;

procedure TFormRG19.ListBoxClick(Sender: TObject);
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

procedure TFormRG19.ShowCurrentReport;
begin
  ReportManager.ShowCurrentReport;
  ReportLabel.Caption := ReportManager.GetCurrentCaption;
end;

procedure TFormRG19.TestBtnClick(Sender: TObject);
begin
  ReportMemo.Lines.Clear;
  Main.RggData.WriteReport(ML);
end;

procedure TFormRG19.InitViewpointCombo;
var
  cl: TStrings;
begin
  cl := ViewpointCombo.Items;
  cl.Add('Seite');
  cl.Add('Achtern');
  cl.Add('Top');
  cl.Add('3D');
end;

procedure TFormRG19.InitParamCombo;
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

procedure TFormRG19.InitTrimmCombo;
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

procedure TFormRG19.TrimmComboChange(Sender: TObject);
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

    Main.CurrentTrimm.WantAll := cbAllProps.Checked;
    Main.CurrentTrimm.SaveTrimmItem(ML);
    Main.CurrentTrimm.WantAll := False;

    //Main.CurrentTrimm.WriteReport(ML);

    ReportLabel.Caption := 'Trimm' + IntToStr(t);
  finally
    ML.EndUpdate;
  end;
end;

procedure TFormRG19.ParamComboChange(Sender: TObject);
var
  ii: Integer;
  fp: TFederParam;
begin
  ii := ParamCombo.ItemIndex;
  fp := TFederParam(ParamCombo.Items.Objects[ii]);
  Main.RggMain.Param := fp;
  ShowTrimm;
end;

procedure TFormRG19.M10BtnClick(Sender: TObject);
begin
  Main.HandleAction(faParamValueMinus10);
  ShowTrimm;
end;

procedure TFormRG19.M1BtnClick(Sender: TObject);
begin
  Main.HandleAction(faParamValueMinus1);
  ShowTrimm;
end;

procedure TFormRG19.P10BtnClick(Sender: TObject);
begin
  Main.HandleAction(faParamValuePlus10);
  ShowTrimm;
end;

procedure TFormRG19.P1BtnClick(Sender: TObject);
begin
  Main.HandleAction(faParamValuePlus1);
  ShowTrimm;
end;

procedure TFormRG19.ShowTrimm;
begin
  Main.RggMain.UpdateTrimmText(TL);
  ShowCurrentReport;
end;

procedure TFormRG19.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  if (ssShift in Shift) or (ssCtrl in Shift) then
  begin
    Main.DoMouseWheel(Shift, WheelDelta);
    ShowTrimm;
    Handled := True;
  end;
end;

procedure TFormRG19.PaintBtn2Click(Sender: TObject);
begin
  Main.RggMain.UpdateGraph;
end;

procedure TFormRG19.ViewpointComboChange(Sender: TObject);
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

end.
