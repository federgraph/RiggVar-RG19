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

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Classes,
  System.UITypes,
  Vcl.Graphics,
  Vcl.Forms,
  Vcl.Controls,
  Vcl.Menus,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.Buttons,
  Vcl.ExtCtrls,
  Vcl.ComCtrls,
  RggTypes;

type
  TFormMain = class(TForm)
    { Dialoge }
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    { Menü }
    MainMenu: TMainMenu;

    FileMenu: TMenuItem;
    NewItem: TMenuItem;
    OpenItem: TMenuItem;
    SaveAsItem: TMenuItem;
    SaveItem: TMenuItem;
    ExitItem: TMenuItem;
    PrintItem: TMenuItem;

    BearbeitenMenu: TMenuItem;
    MemoryItem: TMenuItem;
    MemoryRecallkItem: TMenuItem;
    RecalcItem: TMenuItem;
    ReglerItem: TMenuItem;
    BiegeNeigeItem: TMenuItem;
    GrafikMenu: TMenuItem;
    VonDerSeiteItem: TMenuItem;
    VonHintenItem: TMenuItem;
    VonObenItem: TMenuItem;
    Von3DItem: TMenuItem;

    AnsichtMenu: TMenuItem;
    InputFormItem: TMenuItem;
    OutputFormItem: TMenuItem;
    GrafikFormItem: TMenuItem;
    OptionItem: TMenuItem;
    N3: TMenuItem;
    ConsoleItem: TMenuItem;
    ChartFormItem: TMenuItem;
    ReportFormItem: TMenuItem;
    RotaFormItem: TMenuItem;
    N4: TMenuItem;
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
    N6: TMenuItem;
    LogItem: TMenuItem;

    OptionenMenu: TMenuItem;
    FestItem: TMenuItem;
    DrehbarItem: TMenuItem;
    OhneItem: TMenuItem;
    OSDlgItem: TMenuItem;
    DatenItem: TMenuItem;
    N11: TMenuItem;
    ControllerItem: TMenuItem;
    DifferenzItem: TMenuItem;
    WinkelItem: TMenuItem;
    SofortItem: TMenuItem;
    EntlastetItem: TMenuItem;
    KoppelkurveItem: TMenuItem;
    PaintItem: TMenuItem;
    N9: TMenuItem;
    ReferenzItem: TMenuItem;
    N8: TMenuItem;
    QuerKraftItem: TMenuItem;
    KnickenItem: TMenuItem;
    KraftGemessenItem: TMenuItem;
    KorrigiertItem: TMenuItem;
    ZweischlagItem: TMenuItem;
    N2: TMenuItem;
    AutoLoadItem: TMenuItem;
    N1: TMenuItem;

    WindowMenu: TMenuItem;
    WindowCascadeItem: TMenuItem;
    WindowTileItem: TMenuItem;
    WindowArrangeItem: TMenuItem;
    WindowMinimizeItem: TMenuItem;

    HelpMenu: TMenuItem;
    HilfeItem: TMenuItem;
    AboutItem: TMenuItem;

    AdjustFormItem: TMenuItem;
    CalcOffsetItem: TMenuItem;
    LogoItem: TMenuItem;

    { Speedbar }
    SpeedPanel: TPanel;

    OpenBtn: TSpeedButton;
    SaveBtn: TSpeedButton;
    ExitBtn: TSpeedButton;

    UpdateBtn: TSpeedButton;
    ReglerBtn: TSpeedButton;
    MemoryBtn: TSpeedButton;
    MemoryRecallBtn: TSpeedButton;

    PaintBtn: TSpeedButton;
    BtnBlau: TSpeedButton;
    BtnGrau: TSpeedButton;
    KoppelBtn: TSpeedButton;
    ZweischlagBtn: TSpeedButton;

    ControllerBtn: TSpeedButton;
    WinkelBtn: TSpeedButton;
    DiffBtn: TSpeedButton;
    SofortBtn: TSpeedButton;

    LedShape: TShape;

    { StatusBar }
    StatusBar: TStatusBar;

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
    procedure DatenItemClick(Sender: TObject);
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

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);

    procedure SalingTypChange(Sender: TObject);
    procedure ShowHint(Sender: TObject);
    procedure AdjustFormItemClick(Sender: TObject);
    procedure CalcOffsetItemClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure LogoItemClick(Sender: TObject);
  private
    procedure wmGetMinMaxInfo(var Msg: TMessage); message wm_GetMinMaxInfo;
  public
    ResizeCounter: Integer;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    procedure InitRetina;
    function GetOpenFileName(dn, fn: string): string;
    function GetSaveFileName(dn, fn: string): string;
    procedure SetControllerEnabled;
    procedure SetControllerChecked(Value: Boolean);
    procedure SetKoppelChecked(Value: Boolean);
    procedure TakeOver;
  end;

var
  FormMain: TFormMain;

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

procedure TFormMain.wmGetMinMaxInfo(var Msg: TMessage);
begin
  inherited;
  PMinMaxInfo(Msg.lParam)^.ptMinTrackSize.X := 600;
  PMinMaxInfo(Msg.lParam)^.ptMinTrackSize.Y := 220;
end;

procedure TFormMain.FormCreate(Sender: TObject);
var
  rggm: TRggMain;
begin
  FormMain := Self;

  { Auomatische Freigabe funktioniert hier nicht,
    wenn Self als Owner angegeben wird! }
  InputForm := TInputForm.Create(Application);
  OutputForm := TOutputForm.Create(Application);
  GrafikForm := TGrafikForm.Create(Application);

  RiggModul := TRiggModul.Create(Self);
  rggm := TRggMain.Create(RiggModul.Rigg);

  Main := TMain.Create(rggm);
  Main.Logger.Verbose := True;

  InitRetina;

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
end;

procedure TFormMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Screen.OnActiveFormChange := nil;
  RiggModul.Free;
  // Application.HelpCommand(HELP_QUIT,0);
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  { Freigabe der Formulare erforderlich, wenn Self als Owner angegeben wurde. }
  (*
    InputForm.Free;
    OutputForm.Free;
    GrafikForm.Free;
    *)
  Main.Free;
  Main := nil;
end;

procedure TFormMain.SalingTypChange(Sender: TObject);
begin
  RiggModul.SalingTypChange(Sender);
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
  Caption := 'Rigg';
end;

procedure TFormMain.OpenItemClick(Sender: TObject);
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

procedure TFormMain.SaveItemClick(Sender: TObject);
begin
  if RiggModul.IniFileName = '' then
    SaveAsItemClick(Sender)
  else
    RiggModul.Save;
end;

procedure TFormMain.SaveAsItemClick(Sender: TObject);
begin
  SaveDialog.FileName := RiggModul.IniFileName;
  if SaveDialog.Execute then
  begin
    RiggModul.IniFileName := SaveDialog.FileName;
    Caption := 'Rigg - ' + ExtractFileName(RiggModul.IniFileName);
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
  KoppelkurveItem.Checked := Value;
  KoppelBtn.Down := Value;
  RiggModul.KoppelBtnDown := Value;
end;

procedure TFormMain.KoppelBtnClick(Sender: TObject);
begin
  SetKoppelChecked(not KoppelkurveItem.Checked);
end;

procedure TFormMain.ZweischlagBtnClick(Sender: TObject);
begin
  ZweischlagItem.Checked := not ZweischlagItem.Checked;
  ZweischlagBtn.Down := ZweischlagItem.Checked;
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
end;

procedure TFormMain.SetControllerEnabled;
var
  tempBool: Boolean;
begin
  tempBool := RiggModul.ControllerEnabled;
  ControllerItem.Enabled := tempBool;
  ControllerBtn.Enabled := tempBool;
end;

procedure TFormMain.SetControllerChecked(Value: Boolean);
begin
  ControllerItem.Checked := Value;
  ControllerBtn.Down := Value;
  RiggModul.ControllerBtnDown := Value;
end;

procedure TFormMain.ControllerBtnClick(Sender: TObject);
begin
  SetControllerChecked(not ControllerItem.Checked);
end;

procedure TFormMain.KnickenItemClick(Sender: TObject);
begin
  KorrigiertItem.Enabled := False;
  KnickenItem.Checked := False;
  KraftGemessenItem.Checked := False;
  QuerKraftItem.Checked := False;
  if Sender = QuerKraftItem then
  begin
    RiggModul.CalcTyp := ctQuerKraftBiegung;
    QuerKraftItem.Checked := True;
  end
  else if Sender = KnickenItem then
  begin
    RiggModul.CalcTyp := ctBiegeKnicken;
    KnickenItem.Checked := True;
    KorrigiertItem.Enabled := True;
  end
  else if Sender = KraftGemessenItem then
  begin
    RiggModul.CalcTyp := ctKraftGemessen;
    KraftGemessenItem.Checked := True;
    if ControllerBtn.Down then
    begin
      ControllerBtn.Down := False;
      ControllerItem.Checked := False;
      { RiggModul.ControllerBtnDown := False;
        wird in RiggModul.SetCalcTyp veranlaßt! }
    end;
  end;
  SetControllerEnabled;
end;

procedure TFormMain.KorrigiertItemClick(Sender: TObject);
begin
  KorrigiertItem.Checked := not KorrigiertItem.Checked;
  RiggModul.KorrigiertItem := KorrigiertItem.Checked;
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
  RiggModul.UpdateBtnClick;
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

procedure TFormMain.OhneItemClick(Sender: TObject);
begin
  RiggModul.OhneItemClick;

  FestItem.Checked := False;
  DrehbarItem.Checked := False;
  OhneItem.Checked := True;
  OSDlgItem.Checked := False;
  DatenItem.Checked := False;

  WinkelItem.Checked := False;
  WinkelBtn.Down := False;
  WinkelItem.Enabled := False;
  WinkelBtn.Enabled := False;
  RiggModul.WinkelBtnDown := False;

  BiegeNeigeItem.Enabled := False;
  ReglerItem.Enabled := False;
  ReglerBtn.Enabled := False;

  QuerKraftItem.Enabled := True;
  KnickenItem.Enabled := True;
  KraftGemessenItem.Enabled := True;
  KorrigiertItem.Enabled := True;

  SetKoppelChecked(False);
  KoppelBtn.Enabled := False;
  KoppelkurveItem.Enabled := False;

  SetControllerEnabled;
end;

procedure TFormMain.DrehbarItemClick(Sender: TObject);
begin
  RiggModul.DrehbarItemClick;

  FestItem.Checked := False;
  DrehbarItem.Checked := True;
  OhneItem.Checked := False;
  OSDlgItem.Checked := False;
  DatenItem.Checked := False;

  WinkelItem.Checked := False;
  WinkelBtn.Down := False;
  WinkelItem.Enabled := False;
  WinkelBtn.Enabled := False;
  RiggModul.WinkelBtnDown := False;

  BiegeNeigeItem.Enabled := True;
  ReglerItem.Enabled := True;
  ReglerBtn.Enabled := True;

  QuerKraftItem.Enabled := True;
  KnickenItem.Enabled := True;
  KraftGemessenItem.Enabled := True;
  KorrigiertItem.Enabled := True;

  SetKoppelChecked(False);
  KoppelBtn.Enabled := False;
  KoppelkurveItem.Enabled := False;

  SetControllerEnabled;
end;

procedure TFormMain.FestItemClick(Sender: TObject);
begin
  RiggModul.FestItemClick;

  FestItem.Checked := True;
  DrehbarItem.Checked := False;
  OhneItem.Checked := False;
  OSDlgItem.Checked := False;
  DatenItem.Checked := False;

  WinkelItem.Enabled := True;
  WinkelBtn.Enabled := True;

  BiegeNeigeItem.Enabled := True;
  ReglerItem.Enabled := True;
  ReglerBtn.Enabled := True;

  QuerKraftItem.Enabled := True;
  KnickenItem.Enabled := True;
  KraftGemessenItem.Enabled := True;
  KorrigiertItem.Enabled := True;

  SetKoppelChecked(False);
  KoppelBtn.Enabled := True;
  KoppelkurveItem.Enabled := True;

  SetControllerEnabled;
end;

procedure TFormMain.OSDlgItemClick(Sender: TObject);
begin
  RiggModul.OSDlgItemClick;

  FestItem.Checked := False;
  DrehbarItem.Checked := False;
  OhneItem.Checked := False;
  OSDlgItem.Checked := True;
  DatenItem.Checked := False;

  WinkelItem.Checked := False;
  WinkelBtn.Down := False;
  WinkelItem.Enabled := False;
  WinkelBtn.Enabled := False;
  RiggModul.WinkelBtnDown := False;

  BiegeNeigeItem.Enabled := False;
  ReglerItem.Enabled := False;
  ReglerBtn.Enabled := False;

  SetKoppelChecked(False);
  KoppelBtn.Enabled := False;
  KoppelkurveItem.Enabled := False;

  SetControllerChecked(False);
  SetControllerEnabled;

  QuerKraftItem.Enabled := False;
  KnickenItem.Enabled := False;
  KraftGemessenItem.Enabled := False;
  KorrigiertItem.Enabled := False;
end;

procedure TFormMain.DatenItemClick(Sender: TObject);
begin
  RiggModul.DatenItemClick;
end;

procedure TFormMain.OptionItemClick(Sender: TObject);
begin
  RiggModul.OptionItemClick;
end;

procedure TFormMain.AboutItemClick(Sender: TObject);
begin
  // RiggModul.About;
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

{ **************************************************************************** }

procedure TFormMain.WindowCascadeItemClick(Sender: TObject);
begin
  Cascade;
end;

procedure TFormMain.WindowTileItemClick(Sender: TObject);
begin
  TileMode := TTileMode.tbVertical;
  Tile;
end;

procedure TFormMain.WindowArrangeItemClick(Sender: TObject);
begin
  ArrangeIcons;
end;

procedure TFormMain.WindowMinimizeItemClick(Sender: TObject);
var
  i: Integer;
begin
  { Must be done backwards through the MDIChildren array }
  for i := MDIChildCount - 1 downto 0 do
    MDIChildren[i].WindowState := wsMinimized;
end;

procedure TFormMain.UpdateMenuItems(Sender: TObject);
begin
  WindowCascadeItem.Enabled := MDIChildCount > 0;
  WindowTileItem.Enabled := MDIChildCount > 0;
  WindowArrangeItem.Enabled := MDIChildCount > 0;
  WindowMinimizeItem.Enabled := MDIChildCount > 0;
end;

{ **************************************************************************** }

procedure TFormMain.InputFormItemClick(Sender: TObject);
begin
  InputFormItem.Checked := not InputFormItem.Checked;
  if InputFormItem.Checked then
    InputForm.Show
  else
    InputForm.Hide;
end;

procedure TFormMain.GrafikFormItemClick(Sender: TObject);
begin
  GrafikFormItem.Checked := not GrafikFormItem.Checked;
  if GrafikFormItem.Checked then
    GrafikForm.Show
  else
    GrafikForm.Hide;
end;

procedure TFormMain.OutputFormItemClick(Sender: TObject);
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

procedure TFormMain.ConsoleItemClick(Sender: TObject);
begin
  if RiggModul.ConsoleActive then
  begin
    ConsoleForm.Close;
    Exit;
  end;
  ConsoleForm := TConsoleForm.Create(nil);
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

procedure TFormMain.AutoLoadItemClick(Sender: TObject);
begin
  AutoLoadItem.Checked := not AutoLoadItem.Checked;
end;

procedure TFormMain.TakeOver;
var
  Sender: TObject;
begin
  Sender := nil;
  ControllerItem.Checked := RiggModul.ControllerBtnDown;
  ControllerBtn.Down := ControllerItem.Checked;

  case RiggModul.SalingTyp of
    stFest:
      Sender := FestItem;
    stDrehbar:
      Sender := DrehbarItem;
    stOhne_2:
      Sender := OhneItem;
    stOhne:
      Sender := OSDlgItem;
  end;
  SalingTypChange(Sender);

  { automatisch erneut aufgerufen: }
  {
    UpdateGetriebe; // redundant
    Rigg.UpdateGSB; // redundant
    SetupGCtrls; // notwendig
  }

  case RiggModul.CalcTyp of
    ctQuerKraftBiegung:
      Sender := QuerKraftItem;
    ctBiegeKnicken:
      Sender := KnickenItem;
    ctKraftGemessen:
      Sender := KraftGemessenItem;
  end;
  KnickenItemClick(Sender);
end;

procedure TFormMain.InitRetina;
//var
//  t: single;
begin
//  t := self.Handle.Scale; //Viewport1.Scene.GetSceneScale;
//  if t > 1 then
//  begin
//    if Main <> nil then
//    begin
//      Main.IsRetina := True;
//    end;
//  end;
//  if Main <> nil then
//  begin
//     Main.Logger.InfoVerbose('in TFormMain.InitRetina');
//     Main.Logger.InfoVerbose('  Scale = ' + FloatToStr(t));
//     Main.Logger.InfoVerbose('  Retina = ' + BoolStr[Main.IsRetina]);
//  end;
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

end.
