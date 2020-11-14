unit FrmDiagramE;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.StdCtrls,
  Vcl.WinXCtrls,
  Vcl.ComCtrls,
  Vcl.ExtCtrls,
  System.SysUtils,
  System.Classes,
  System.UITypes,
  RggDiagram,
  RggChart,
  RggChartModel01;

type
  TFormDiagramE = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    UpdateBtn: TButton;
    XBtn: TButton;
    CalcBtn: TButton;
    AuswahlBtn: TButton;
    LayoutBtn: TButton;

    XBox: TListBox;
    PBox: TListBox;
    YBox: TListBox;

    AutoToggle: TToggleSwitch;
    AToggle: TToggleSwitch;
    GToggle: TToggleSwitch;
    UpDown: TUpDown;

    Memo: TMemo;
    Image: TImage;

    procedure UpdateBtnClick(Sender: TObject);
    procedure XBtnClick(Sender: TObject);
    procedure CalcBtnClick(Sender: TObject);
    procedure GToggleClick(Sender: TObject);
    procedure YBoxClick(Sender: TObject);
    procedure AToggleClick(Sender: TObject);
    procedure PBoxClick(Sender: TObject);
    procedure UpDownClick(Sender: TObject; Button: TUDBtnType);
    procedure XBoxClick(Sender: TObject);
    procedure AutoToggleClick(Sender: TObject);
    procedure AuswahlBtnClick(Sender: TObject);
    procedure LayoutBtnClick(Sender: TObject);
  private
    FScale: single;
    BoxWidth: Integer;
    BoxHeight: Integer;
    MemoWidth: Integer;
    Layout: Integer;
    cr: TControl;
    Margin: Integer;
    FormShown: Boolean;
    function ToggleState(Value: Boolean): TToggleSwitchState;
    procedure UpdateMemo;
    procedure AnchorVertical(c: TControl);
    procedure StackH(c: TControl);
    procedure StackV(c: TControl);
    procedure LayoutComponents1;
    procedure LayoutComponents2;
    procedure InitComponentLinks;
    procedure InitComponentSize;
  public
    WantAutoUpdate: Boolean;
    ChartModel: TRggChartModel01;
    ChartGraph: TRggDiagram;
    procedure CreateComponents;
    procedure LayoutComponents;
    procedure UpdateUI(Sender: TObject);
  end;

var
  FormDiagramE: TFormDiagramE;

implementation

{$R *.dfm}

uses
  RiggVar.App.Main;

procedure TFormDiagramE.FormCreate(Sender: TObject);
begin
  FScale := MainVar.Scale;

  Margin := Round(10 * FScale);
  Width := Round(1500 * FScale);
  Height := Round(800 * FScale);

  BoxWidth := Round(200 * FScale);
  BoxHeight := Round(160 * FScale);
  MemoWidth := Round(350 * FScale);

  WantAutoUpdate := True;

  CreateComponents;
  Layout := 2;

  ChartModel := TRggChartModel01.Create;
  ChartModel.Rigg := Main.Rigg;

  ChartGraph := TRggDiagram.Create(ChartModel);
  ChartGraph.Image := Image;

  ChartModel.OnUpdateAvailable := ChartGraph.Draw;
end;

procedure TFormDiagramE.FormDestroy(Sender: TObject);
begin
  ChartGraph.Free;
  ChartModel.Free;
end;

procedure TFormDiagramE.FormShow(Sender: TObject);
begin
  if not FormShown then
  begin
    LayoutComponents;
    FormShown := True;
    UpdateUI(nil); // --> update Listboxes
    ChartModel.SuperCalc; // --> Draw
  end;
end;

procedure TFormDiagramE.CreateComponents;
begin
  UpdateBtn := TButton.Create(Self);
  UpdateBtn.Parent := Self;
  UpdateBtn.Caption := 'Update UI';

  XBtn := TButton.Create(Self);
  XBtn.Parent := Self;
  XBtn.Caption := 'Select X';

  CalcBtn := TButton.Create(Self);
  CalcBtn.Parent := Self;
  CalcBtn.Caption := 'Calc';

  AuswahlBtn := TButton.Create(Self);
  AuswahlBtn.Parent := Self;
  AuswahlBtn.Caption := 'YAV';

  LayoutBtn := TButton.Create(Self);
  LayoutBtn.Parent := Self;
  LayoutBtn.Caption := 'Layout';

  AutoToggle := TToggleSwitch.Create(Self);
  AutoToggle.Parent := Self;
  AutoToggle.OnClick := nil;
  AutoToggle.State := ToggleState(WantAutoUpdate);
  AutoToggle.ThumbColor := TColors.Gray;
  AutoToggle.StateCaptions.CaptionOn := 'Auto-Update On';
  AutoToggle.StateCaptions.CaptionOff := 'Auto-Update off';

  AToggle := TToggleSwitch.Create(Self);
  AToggle.Parent := Self;
  AToggle.OnClick := nil;
  AToggle.State := TToggleSwitchState.tssOff;
  AToggle.ThumbColor := TColors.Dodgerblue;
  AToggle.StateCaptions.CaptionOff := 'Arbeitspunkt';
  AToggle.StateCaptions.CaptionOn := 'Bereich';

  GToggle := TToggleSwitch.Create(Self);
  GToggle.Parent := Self;
  GToggle.OnClick := nil;
  GToggle.State := TToggleSwitchState.tssOff;
  GToggle.ThumbColor := TColors.Red;
  GToggle.StateCaptions.CaptionOn := 'Grouping On';
  GToggle.StateCaptions.CaptionOff := 'Grouping off';

  UpDown := TUpDown.Create(Self);
  UpDown.Parent := Self;
  UpDown.OnChanging := nil;
  UpDown.Orientation := TUDOrientation.udHorizontal;
  UpDown.Width := Round(100 * FScale);
  UpDown.Height := Round(40 * FScale);
  UpDown.Min := 1;
  UpDown.Max := 31;
  UpDown.Position := 30;

  XBox := TListBox.Create(Self);
  XBox.Parent := Self;

  PBox := TListBox.Create(Self);
  PBox.Parent := Self;

  YBox := TListBox.Create(Self);
  YBox.Parent := Self;

  Memo := TMemo.Create(Self);
  Memo.Parent := Self;
  Memo.ScrollBars := TScrollStyle.ssBoth;

  Image := TImage.Create(Self);
  Image.Parent := Self;

  InitComponentSize;
  InitComponentLinks;
end;

procedure TFormDiagramE.InitComponentSize;
begin
  XBox.Width := BoxWidth;
  XBox.Height := BoxHeight;

  PBox.Width := BoxWidth;
  PBox.Height := BoxHeight;

  YBox.Width := BoxWidth;
  YBox.Height := BoxHeight;

  Memo.Width := MemoWidth;
end;

procedure TFormDiagramE.InitComponentLinks;
begin
  XBox.OnClick := XBoxClick;
  PBox.OnClick := PBoxClick;
  YBox.OnClick := YBoxClick;

  UpdateBtn.OnClick := UpdateBtnClick;
  XBtn.OnClick := XBtnClick;
  CalcBtn.OnClick := CalcBtnClick;
  AuswahlBtn.OnClick := AuswahlBtnClick;
  LayoutBtn.OnClick := LayoutBtnClick;

  AutoToggle.OnClick := AutoToggleClick;
  GToggle.OnClick := GToggleClick;
  AToggle.OnClick := AToggleClick;
  UpDown.OnClick := UpDownClick;
end;

function TFormDiagramE.ToggleState(Value: Boolean): TToggleSwitchState;
begin
  if Value then
    result := TToggleSwitchState.tssOn
  else
    result := TToggleSwitchState.tssOff;
end;

procedure TFormDiagramE.UpdateMemo;
begin
  ChartModel.GetMemoText;
  Memo.Text := ChartModel.MemoLines.Text;
end;

procedure TFormDiagramE.UpdateUI(Sender: TObject);
begin
  if ChartGraph = nil then
    Exit;

  if not Visible then
    Exit;

  AToggle.State := ToggleState(not ChartModel.AP);
  GToggle.State := ToggleState(ChartModel.ShowGroup);

  XBox.Items := ChartModel.XComboItems;
  PBox.Items := ChartModel.PComboItems;
  YBox.Items := ChartModel.YComboItems;

  XBox.ItemIndex := ChartModel.XComboItemIndex;
  PBox.ItemIndex := ChartModel.PComboItemIndex;
  YBox.ItemIndex := ChartModel.YComboItemIndex;
  YBox.Enabled := not ChartModel.ShowGroup;

  UpdateMemo;

  UpDown.Position := ChartModel.APWidth;
end;

procedure TFormDiagramE.XBoxClick(Sender: TObject);
begin
  if WantAutoUpdate then
  begin
    XBtnClick(nil);
    CalcBtnClick(nil);
  end;
end;

procedure TFormDiagramE.XBtnClick(Sender: TObject);
begin
  { Step 1 - select X }
  ChartModel.XComboItemIndex := XBox.ItemIndex;

  if ChartModel.XComboItemIndex >= ChartModel.XComboItems.Count then
    ChartModel.XComboItemIndex := 0;

  ChartModel.UpdatePCombo(ChartModel.FSalingTyp);

  PBox.Items := ChartModel.PComboItems;
  PBox.ItemIndex := ChartModel.PComboItemIndex;
end;

procedure TFormDiagramE.UpdateBtnClick(Sender: TObject);
begin
  UpdateUI(nil);
end;

procedure TFormDiagramE.AutoToggleClick(Sender: TObject);
begin
  WantAutoUpdate := AutoToggle.IsOn;
end;

procedure TFormDiagramE.AuswahlBtnClick(Sender: TObject);
begin
  ChartModel.YAuswahlClick;
  YBox.Items := ChartModel.YComboItems;
end;

procedure TFormDiagramE.CalcBtnClick(Sender: TObject);
begin
  ChartModel.PComboItemIndex := PBox.ItemIndex;
  ChartModel.YComboItemIndex := YBox.ItemIndex;

  ChartModel.AP := not AToggle.IsOn;
  ChartModel.ShowGroup := GToggle.IsOn;

  ChartModel.SuperCalc;

  Memo.Text := ChartModel.MemoLines.Text;
end;

procedure TFormDiagramE.PBoxClick(Sender: TObject);
begin
  ChartModel.PComboItemIndex := PBox.ItemIndex;

  if WantAutoUpdate then
  begin
    CalcBtnClick(nil);
  end;
end;

procedure TFormDiagramE.YBoxClick(Sender: TObject);
begin
  if not GToggle.IsOn then
  begin
    ChartModel.YComboItemIndex := YBox.ItemIndex;
    ChartModel.Calc;
  end;
end;

procedure TFormDiagramE.AToggleClick(Sender: TObject);
begin
  ChartModel.AP := not AToggle.IsOn;
  ChartModel.Calc;
  Main.FederText.CheckState;
  Memo.Text := ChartModel.MemoLines.Text;
end;

procedure TFormDiagramE.GToggleClick(Sender: TObject);
begin
  ChartModel.ShowGroup := GToggle.IsOn;
  ChartModel.DrawGroup;
  YBox.Enabled := not ChartModel.ShowGroup;
  Main.FederText.CheckState;
  UpdateMemo;
end;

procedure TFormDiagramE.UpDownClick(Sender: TObject; Button: TUDBtnType);
begin
  ChartModel.APWidth := UpDown.Position;
  ChartModel.SuperCalc;
  Memo.Text := ChartModel.MemoLines.Text;
end;

procedure TFormDiagramE.StackH(c: TControl);
begin
  c.Left := cr.Left + cr.Width + Margin;
  c.Top := cr.Top;
  cr := c;
end;

procedure TFormDiagramE.StackV(c: TControl);
begin
  c.Left := cr.Left;
  c.Top := cr.Top + cr.Height + Margin;
  cr := c;
end;

procedure TFormDiagramE.AnchorVertical(c: TControl);
begin
  c.Height := ClientHeight - c.Top - Margin;
  c.Anchors := c.Anchors + [TAnchorKind.akBottom];
end;

procedure TFormDiagramE.LayoutBtnClick(Sender: TObject);
begin
  Inc(Layout);
  if Layout = 3 then
    Layout := 1;
  LayoutComponents;
end;

procedure TFormDiagramE.LayoutComponents;
begin
  UpdateBtn.Left := Margin;
  UpdateBtn.Top := Margin;

  cr := UpdateBtn;
  StackH(XBtn);
  StackH(CalcBtn);
  StackH(AuswahlBtn);
  StackH(LayoutBtn);

  case Layout of
    1: LayoutComponents1;
    2: LayoutComponents2;
  end;

  AnchorVertical(Memo);
end;

procedure TFormDiagramE.LayoutComponents1;
begin
  cr := UpdateBtn;
  StackV(XBox);
  StackV(PBox);
  StackV(AutoToggle);
  StackV(AToggle);
  StackV(UpDown);

  cr := XBox;
  StackH(Memo);
  StackH(YBox);
  StackV(GToggle);
  StackV(Image);

  ClientWidth := Image.Left + Image.Width + Margin;
end;

procedure TFormDiagramE.LayoutComponents2;
begin
  cr := UpdateBtn;
  StackV(XBox);
  StackH(PBox);
  StackH(YBox);
  StackH(AutoToggle);
  StackV(GToggle);
  StackV(AToggle);
  StackV(UpDown);

  cr := XBox;
  StackV(Memo);
  StackH(Image);

  ClientWidth := Image.Left + Image.Width + Margin;
end;

end.
