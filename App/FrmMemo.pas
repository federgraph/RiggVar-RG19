unit FrmMemo;

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
  SysUtils,
  Classes,
  Generics.Collections,
  Controls,
  ExtCtrls,
  Forms,
  Dialogs,
  StdCtrls;

type
  TMemoAction = record
  public
    Tag: Integer;
    Caption: string;
    Handler: TNotifyEvent;
  end;

  TMemoActionList = class (TList<TMemoAction>)
  public
    procedure AddMemoAction(ACaption: string; AHandler: TNotifyEvent);
    function FindByTag(ATag: Integer): TMemoAction;
  end;

  TFormMemo = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  public
    ToolbarPanel: TPanel;
    ListView: TListbox;
    Memo: TMemo;
    ReadBtn: TButton;
    procedure ReadBtnClick(Sender: TObject);
    procedure ListViewItemClick(Sender: TObject);
    procedure CreateComponents;
    procedure LayoutComponents;
  private
    FScale: single;
    SL: TStringList;
    MemoActionList: TMemoActionList;
    procedure InitItems;
    procedure InitList;
    procedure MemoBeginUpdate;
    procedure MemoEndUpdate;
  protected
    procedure WriteTrimmItem(Sender: TObject);
    procedure WriteTrimmFile(Sender: TObject);
    procedure WriteHullPoints(Sender: TObject);

    procedure InfoText(Sender: TObject);
    procedure HelpText(Sender: TObject);
    procedure HelpTextForIO(Sender: TObject);
    procedure WriteShortcuts(Sender: TObject);

    procedure DeviceReportBtnClick(Sender: TObject);

    procedure ActionTestBtnClick(Sender: TObject);
    procedure WriteNewActionConstBtnClick(Sender: TObject);
    procedure WriteActionConstBtnClick(Sender: TObject);
    procedure WriteActionNamesBtnClick(Sender: TObject);
  end;

var
  FormMemo: TFormMemo;

implementation

{$R *.dfm}

uses
  RiggVar.FB.Color,
  RiggVar.App.Main;

procedure TFormMemo.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := TCloseAction.caHide;
end;

procedure TFormMemo.FormCreate(Sender: TObject);
begin
  FScale := MainVar.Scale;

  Caption := 'Form Memo';
  Left := Round(10 * FScale);
  Top := Round(120 * FScale);
  Width := Round(800 * FScale);
  Height := Round(750 * FScale);
  SL := TStringList.Create;

  CreateComponents;
  LayoutComponents;

  ListView.Align := alLeft;

  Memo.Align := alClient;

  MemoActionList := TMemoActionList.Create;
  InitList;
  InitItems;
end;

procedure TFormMemo.FormDestroy(Sender: TObject);
begin
  SL.Free;
  MemoActionList.Free;
end;

procedure TFormMemo.ReadBtnClick(Sender: TObject);
begin
  MemoBeginUpdate;
  SL.Text := Memo.Lines.Text;
  Main.ReadText(SL);
  MemoEndUpdate;
end;

procedure TFormMemo.WriteTrimmItem(Sender: TObject);
begin
  MemoBeginUpdate;
  Main.WriteTrimmItem;
  Memo.Lines.Text := Main.FLText;
  MemoEndUpdate;
end;

procedure TFormMemo.WriteTrimmFile(Sender: TObject);
begin
  MemoBeginUpdate;
  Main.WriteTrimmFile;
  Memo.Lines.Text := Main.FLText;
  MemoEndUpdate;
end;

procedure TFormMemo.WriteHullPoints(Sender: TObject);
begin
  MemoBeginUpdate;
  Memo.Lines.Clear;
  Main.FederBinding.InitHullPoints(Memo.Lines);
  MemoEndUpdate;
end;

procedure TFormMemo.InfoText(Sender: TObject);
begin
  MemoBeginUpdate;
  Memo.Lines.Clear;
  Main.FederBinding.InitInfoText(Memo.Lines);
  MemoEndUpdate;
end;

procedure TFormMemo.HelpText(Sender: TObject);
begin
  MemoBeginUpdate;
  Memo.Lines.Clear;
  Main.FederBinding.InitHelpText(Memo.Lines);
  MemoEndUpdate;
end;

procedure TFormMemo.HelpTextForIO(Sender: TObject);
begin
  MemoBeginUpdate;
  Memo.Lines.Clear;
  Main.FederBinding.InitHelpTextForIO(Memo.Lines);
  MemoEndUpdate;
end;

procedure TFormMemo.DeviceReportBtnClick(Sender: TObject);
begin
  MemoBeginUpdate;
  Memo.Lines.Clear;
  Memo.Lines.Add('DeviceReport not implemented');
//  if not Assigned(DeviceCheck) then
//    DeviceCheck := TDeviceCheck.Create;
//  DeviceCheck.GetDeviceReport(Memo.Lines);
  MemoEndUpdate;
end;

procedure TFormMemo.ActionTestBtnClick(Sender: TObject);
begin
  MemoBeginUpdate;
  Memo.Lines.Clear;
  Main.ActionTest.TestAll;
  Memo.Lines.Text := Main.ActionTest.SL.Text;
  MemoEndUpdate;
end;

procedure TFormMemo.WriteNewActionConstBtnClick(Sender: TObject);
begin
  MemoBeginUpdate;
  Memo.Lines.Clear;
  Main.ActionTest.WriteNewActionConst(Memo.Lines);
  MemoEndUpdate;
end;

procedure TFormMemo.WriteActionConstBtnClick(Sender: TObject);
begin
  MemoBeginUpdate;
  Memo.Lines.Clear;
  Main.ActionTest.WriteActionConst(Memo.Lines);
  MemoEndUpdate;
end;

procedure TFormMemo.WriteActionNamesBtnClick(Sender: TObject);
begin
  MemoBeginUpdate;
  Memo.Lines.Clear;
  Main.ActionTest.WriteActionNames(Memo.Lines);
  MemoEndUpdate;
end;

procedure TFormMemo.WriteShortcuts(Sender: TObject);
begin
  MemoBeginUpdate;
  Memo.Lines.Clear;
  Main.ActionHelper.GetShortcutReport(Memo.Lines);
  MemoEndUpdate;
end;

procedure TFormMemo.InitList;
begin
  MemoActionList.AddMemoAction('Show Trimm Item', WriteTrimmItem);
  MemoActionList.AddMemoAction('Show Trimm File', WriteTrimmFile);
  MemoActionList.AddMemoAction('Show Hull Points', WriteHullPoints);
  MemoActionList.AddMemoAction('Info Text', InfoText);
  MemoActionList.AddMemoAction('Help Text', HelpText);

{$ifdef MSWINDOWS}
  MemoActionList.AddMemoAction('Help Text for IO', HelpTextForIO);
  MemoActionList.AddMemoAction('Write Shortcuts', WriteShortcuts);
//  MemoActionList.AddMemoAction('Device Report', DeviceReportBtnClick);

  MemoActionList.AddMemoAction('Action Test', ActionTestBtnClick);
  MemoActionList.AddMemoAction('Write Action Const', WriteActionConstBtnClick);
  MemoActionList.AddMemoAction('Write New Action Const', WriteNewActionConstBtnClick);
  MemoActionList.AddMemoAction('Write Action Names', WriteActionNamesBtnClick);
{$endif}
end;

procedure TFormMemo.InitItems;
var
  cr: TMemoAction;
begin
  ListView.Items.Clear;
  for cr in MemoActionList do
  begin
    ListView.Items.Add(cr.Caption);
  end;
end;

procedure TFormMemo.ListViewItemClick(Sender: TObject);
var
  cr: TMemoAction;
begin
  cr := MemoActionList.FindByTag(ListView.ItemIndex);
  if cr.Tag > -1 then
    cr.Handler(nil);
end;

{ TMemoActionList }

procedure TMemoActionList.AddMemoAction(ACaption: string; AHandler: TNotifyEvent);
var
  ma: TMemoAction;
begin
  ma.Caption := ACaption;
  ma.Handler := AHandler;
  ma.Tag := Self.Count;
  Self.Add(ma);
end;

function TMemoActionList.FindByTag(ATag: Integer): TMemoAction;
var
  cr: TMemoAction;
begin
  result.Caption := 'not found';
  result.Tag := -1;
  for cr in Self do
    if cr.Tag = ATag then
    begin
      result := cr;
      break;
    end;
end;

procedure TFormMemo.MemoBeginUpdate;
begin
  Memo.Lines.BeginUpdate;
end;

procedure TFormMemo.MemoEndUpdate;
begin
  Memo.Lines.EndUpdate;
end;

procedure TFormMemo.CreateComponents;
begin
  ToolbarPanel := TPanel.Create(Self);
  ListView := TListbox.Create(Self);
  Memo := TMemo.Create(Self);
  ReadBtn := TButton.Create(Self);

  ToolbarPanel.Parent := Self;
  ListView.Parent := Self;
  Memo.Parent := Self;
  ReadBtn.Parent := ToolBarPanel;
end;

procedure TFormMemo.LayoutComponents;
   function Scaled(Value: Integer): Integer;
   begin
     //result := Value;
     result := Round(Value * FScale);
   end;
begin
  ToolbarPanel.Height := Scaled(41);
  ToolbarPanel.Align := alTop;

  ReadBtn.Left := Scaled(16);
  ReadBtn.Top := Scaled(8);
  ReadBtn.Width := Scaled(49);
  ReadBtn.Height := Scaled(22);
  ReadBtn.Caption := 'Read';
//  ReadBtn.Enabled := False;
  ReadBtn.OnClick := ReadBtnClick;

  ListView.Left := 24;
  ListView.Top := 112;
  ListView.Width := Scaled(220);
  ListView.Font.Name := 'Consolas';
  ListView.Font.Size := 12;
  ListView.Font.Color := TRggColors.Dodgerblue;
  ListView.OnClick := ListViewItemClick;

  Memo.Left := 328;
  Memo.Top := 72;
  Memo.Font.Name := 'Consolas';
  Memo.Font.Size := 12;
  Memo.Font.Name := 'Courier New';
  Memo.Font.Size := 11;
  Memo.Font.Color := TRggColors.Teal;
  Memo.ScrollBars := ssBoth;
end;

end.
