unit FrmText;

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
  Vcl.Forms,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.StdCtrls,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  Vcl.ComCtrls,
  RiggVar.RG.Def,
  RiggVar.RG.Report,
  IoTypes,
  Vcl.Buttons;

type
  TTextForm = class(TForm)
    ReportMemo: TMemo;
    ListBox: TListBox;
    Panel: TPanel;
    ReportLabel: TLabel;
    ParamCombo: TComboBox;
    M10Btn: TSpeedButton;
    M1Btn: TSpeedButton;
    P1Btn: TSpeedButton;
    P10Btn: TSpeedButton;
    TrimmMemo: TMemo;
    TrimmCombo: TComboBox;
    cbSandboxed: TCheckBox;
    CopyAndPasteBtn: TSpeedButton;
    CopyTrimmItemBtn: TSpeedButton;
    MT0Btn: TSpeedButton;
    PasteTrimmItemBtn: TSpeedButton;
    ReadTrimmFileBtn: TSpeedButton;
    SaveTrimmFileBtn: TSpeedButton;
    cbAllProps: TCheckBox;
    ViewpointCombo: TComboBox;
    cbAllTags: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
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
    procedure PaintBtnClick(Sender: TObject);
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

    procedure InitRetina;
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
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    function GetOpenFileName(dn, fn: string): string;
    function GetSaveFileName(dn, fn: string): string;
  end;

var
  TextForm: TTextForm;

implementation

{$R *.dfm}

uses
  RiggVar.App.Main,
  RiggVar.FB.Classes,
  RiggUnit;

procedure TTextForm.FormCreate(Sender: TObject);
begin
  Left := 810;
  Height := 640;
  TL := TrimmMemo.Lines;
  ML := ReportMemo.Lines;

  Panel.ShowCaption := False;

  Panel.Align := alTop;
  Listbox.Align := alLeft;
  TrimmMemo.Align := alLeft;
  ReportMemo.Align := alClient;

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

  InitRetina;
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
end;

procedure TTextForm.FormDestroy(Sender: TObject);
begin
  ReportManager.Free;
end;

procedure TTextForm.FormResize(Sender: TObject);
begin
  Inc(ResizeCounter);
end;

procedure TTextForm.AB(B: TSpeedButton);
begin
  B.Left := BtnLeft + BtnCounter * BtnWidth + 12;
  B.Width := 45;
  B.Height := 30;
  B.Font.Name := 'Consolas';
  B.Font.Size := 12;
  B.Font.Color := BtnColor;
  Inc(BtnCounter);
end;

procedure TTextForm.SetupLabel(L: TLabel);
begin
  L.Font.Name := 'Consolas';
  L.Font.Size := 11;
  L.Font.Color := clPurple;
end;

procedure TTextForm.SetupComboBox(CB: TComboBox);
begin
  CB.Style := csDropDownList;
  CB.DropDownCount := Integer(High(TFederParam));
  CB.Font.Name := 'Consolas';
  CB.Font.Size := 11;
  CB.Font.Color := clRed;
end;

procedure TTextForm.SetupListBox(LB: TListBox);
begin
  LB.Font.Name := 'Consolas';
  LB.Font.Size := 11;
  LB.Font.Color := clBlue;
end;

procedure TTextForm.SetupMemo(Memo: TMemo);
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

procedure TTextForm.InitRetina;
//var
//  t: single;
begin
{$ifdef FMX}
  t := self.Handle.Scale; //Viewport1.Scene.GetSceneScale;
  if t > 1 then
  begin
    if Main <> nil then
    begin
      Main.IsRetina := True;
    end;
  end;
  if Main <> nil then
  begin
     Main.Logger.InfoVerbose('in TFormMain.InitRetina');
     Main.Logger.InfoVerbose('  Scale = ' + FloatToStr(t));
     Main.Logger.InfoVerbose('  Retina = ' + BoolStr[Main.IsRetina]);
  end;
{$endif}

end;

function TTextForm.GetOpenFileName(dn, fn: string): string;
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

function TTextForm.GetSaveFileName(dn, fn: string): string;
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

procedure TTextForm.CopyTrimmItemBtnClick(Sender: TObject);
begin
  Main.CopyTrimmItem;
  ShowTrimm;
end;

procedure TTextForm.PasteTrimmItemBtnClick(Sender: TObject);
begin
  Main.PasteTrimmItem;
  ShowTrimm;
end;

procedure TTextForm.CopyAndPasteBtnClick(Sender: TObject);
begin
  Main.CopyAndPaste;
  ShowTrimm;
end;

procedure TTextForm.CopyTrimmFileBtnClick(Sender: TObject);
begin
  Main.CopyTrimmFile;
  ShowTrimm;
end;

procedure TTextForm.ReadTrimmFileBtnClick(Sender: TObject);
begin
  Main.ReadTrimmFile;
  ShowTrimm;
end;

procedure TTextForm.SaveTrimmFileBtnClick(Sender: TObject);
begin
  Main.SaveTrimmFile;
  ShowTrimm;
end;

procedure TTextForm.MT0BtnClick(Sender: TObject);
begin
  Main.UpdateTrimm0;
  //Main.FederText.UpdateText;
  ShowTrimm;
end;

procedure TTextForm.cbAllTagsClick(Sender: TObject);
begin
  ReportManager.XmlAllTags := cbAllTags.Checked;
end;

procedure TTextForm.cbSandboxedClick(Sender: TObject);
begin
  IsSandboxed := cbSandboxed.Checked;
end;

procedure TTextForm.InitListBox;
begin
  ReportManager.InitLB(ListBox.Items);
end;

procedure TTextForm.ListBoxClick(Sender: TObject);
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

procedure TTextForm.ShowCurrentReport;
begin
  ReportManager.ShowCurrentReport;
  ReportLabel.Caption := ReportManager.GetCurrentCaption;
end;

procedure TTextForm.TestBtnClick(Sender: TObject);
begin
  ReportMemo.Lines.Clear;
  Main.RggData.WriteReport(ML);
end;

procedure TTextForm.InitViewpointCombo;
var
  cl: TStrings;
begin
  cl := ViewpointCombo.Items;
  cl.Add('Seite');
  cl.Add('Achtern');
  cl.Add('Top');
  cl.Add('3D');
end;

procedure TTextForm.InitParamCombo;
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

procedure TTextForm.InitTrimmCombo;
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

procedure TTextForm.TrimmComboChange(Sender: TObject);
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

procedure TTextForm.ParamComboChange(Sender: TObject);
var
  ii: Integer;
  fp: TFederParam;
begin
  ii := ParamCombo.ItemIndex;
  fp := TFederParam(ParamCombo.Items.Objects[ii]);
  Main.RggMain.Param := fp;
  ShowTrimm;
end;

procedure TTextForm.M10BtnClick(Sender: TObject);
begin
  Main.HandleAction(faParamValueMinus10);
  ShowTrimm;
end;

procedure TTextForm.M1BtnClick(Sender: TObject);
begin
  Main.HandleAction(faParamValueMinus1);
  ShowTrimm;
end;

procedure TTextForm.P10BtnClick(Sender: TObject);
begin
  Main.HandleAction(faParamValuePlus10);
  ShowTrimm;
end;

procedure TTextForm.P1BtnClick(Sender: TObject);
begin
  Main.HandleAction(faParamValuePlus1);
  ShowTrimm;
end;

procedure TTextForm.ShowTrimm;
begin
  Main.RggMain.UpdateTrimmText(TL);
  ShowCurrentReport;
end;

procedure TTextForm.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  if (ssShift in Shift) or (ssCtrl in Shift) then
  begin
    Main.DoMouseWheel(Shift, WheelDelta);
    ShowTrimm;
    Handled := True;
  end;
end;

procedure TTextForm.PaintBtnClick(Sender: TObject);
begin
  Main.RggMain.UpdateGraph;
end;

procedure TTextForm.ViewpointComboChange(Sender: TObject);
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
