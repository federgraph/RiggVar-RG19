unit FrmDiagramQ;

{$ifdef fpc}
{$mode delphi}
{$endif}

interface

uses
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
  RggChartGraph,
  RggChartModel;

type
  TFormDiagramQ = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    LayoutBtn: TButton;
    XBox: TListBox;
    PBox: TListBox;
    YBox: TListBox;
    AToggle: TToggleSwitch;
    GToggle: TToggleSwitch;
    UpDown: TUpDown;
    Image: TImage;

    procedure XBtnClick(Sender: TObject);
    procedure CalcBtnClick(Sender: TObject);
    procedure LayoutBtnClick(Sender: TObject);

    procedure XBoxClick(Sender: TObject);
    procedure YBoxClick(Sender: TObject);
    procedure PBoxClick(Sender: TObject);
    procedure UpDownClick(Sender: TObject; Button: TUDBtnType);

    procedure AToggleClick(Sender: TObject);
    procedure GToggleClick(Sender: TObject);
  private
    FScale: single;
    BoxWidth: Integer;
    BoxHeight: Integer;
    Layout: Integer;
    cr: TControl;
    Margin: Integer;
    FormShown: Boolean;
    function ToggleState(Value: Boolean): TToggleSwitchState;
    procedure UpdateMemo;
  protected
    TempR: Integer;
    TempB: Integer;
    FMaxRight: Integer;
    FMaxBottom: Integer;
    procedure RecordMax;
    procedure AnchorVertical(c: TControl);
    procedure StackH(c: TControl);
    procedure StackV(c: TControl);
    procedure LayoutComponents1;
    procedure LayoutComponents2;
    procedure InitComponentSize;
    procedure InitComponentLinks;
  public
    WantAutoUpdate: Boolean;
    ChartModel: TChartGraph;
    procedure CreateComponents;
    procedure LayoutComponents;
    procedure UpdateUI(Sender: TObject);
  end;

var
  FormDiagramQ: TFormDiagramQ;

implementation

{$R *.dfm}

uses
  RiggVar.App.Main;

procedure TFormDiagramQ.FormCreate(Sender: TObject);
begin
  FScale := MainVar.Scale;

  Margin := Round(10 * FScale);
  Width := Round(1500 * FScale);
  Height := Round(800 * FScale);

  BoxWidth := Round(200 * FScale);
  BoxHeight := Round(160 * FScale);

  WantAutoUpdate := True;

  CreateComponents;
  Layout := 2;

  ChartModel := TChartGraph.Create(Main.Rigg);
  ChartModel.Image := Image;
end;

procedure TFormDiagramQ.FormDestroy(Sender: TObject);
begin
  ChartModel.Free;
end;

procedure TFormDiagramQ.FormShow(Sender: TObject);
begin
  if not FormShown then
  begin
    LayoutComponents;
    FormShown := True;
    UpdateUI(nil); // --> update Listboxes
    ChartModel.SuperCalc; // --> Draw
  end;
end;

procedure TFormDiagramQ.CreateComponents;
begin
  LayoutBtn := TButton.Create(Self);
  LayoutBtn.Parent := Self;
  LayoutBtn.Caption := 'Layout';

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
  UpDown.Max := 100;
  UpDown.Position := 30;

  XBox := TListBox.Create(Self);
  XBox.Parent := Self;

  PBox := TListBox.Create(Self);
  PBox.Parent := Self;

  YBox := TListBox.Create(Self);
  YBox.Parent := Self;

  Image := TImage.Create(Self);
  Image.Parent := Self;

  InitComponentSize;
  InitComponentLinks;
end;

procedure TFormDiagramQ.InitComponentSize;
begin
  XBox.Width := BoxWidth;
  XBox.Height := BoxHeight;

  PBox.Width := BoxWidth;
  PBox.Height := BoxHeight;

  YBox.Width := BoxWidth;
  YBox.Height := BoxHeight;
end;

procedure TFormDiagramQ.InitComponentLinks;
begin
  LayoutBtn.OnClick := LayoutBtnClick;

  XBox.OnClick := XBoxClick;
  PBox.OnClick := PBoxClick;
  YBox.OnClick := YBoxClick;

  GToggle.OnClick := GToggleClick;
  AToggle.OnClick := AToggleClick;
  UpDown.OnClick := UpDownClick;
end;

function TFormDiagramQ.ToggleState(Value: Boolean): TToggleSwitchState;
begin
  if Value then
    result := TToggleSwitchState.tssOn
  else
    result := TToggleSwitchState.tssOff;
end;

procedure TFormDiagramQ.UpdateMemo;
begin
  ChartModel.GetMemoText;
end;

procedure TFormDiagramQ.UpdateUI(Sender: TObject);
begin
  if ChartModel = nil then
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

procedure TFormDiagramQ.XBoxClick(Sender: TObject);
begin
  if WantAutoUpdate then
  begin
    XBtnClick(nil);
    CalcBtnClick(nil);
  end;
end;

procedure TFormDiagramQ.XBtnClick(Sender: TObject);
begin
  { Step 1 - select X }
  ChartModel.XComboItemIndex := XBox.ItemIndex;

  if ChartModel.XComboItemIndex >= ChartModel.XComboItems.Count then
    ChartModel.XComboItemIndex := 0;

  ChartModel.UpdatePCombo(ChartModel.FSalingTyp);

  PBox.Items := ChartModel.PComboItems;
  PBox.ItemIndex := ChartModel.PComboItemIndex;
end;

procedure TFormDiagramQ.CalcBtnClick(Sender: TObject);
begin
  ChartModel.PComboItemIndex := PBox.ItemIndex;
  ChartModel.YComboItemIndex := YBox.ItemIndex;

  ChartModel.AP := not AToggle.IsOn;
  ChartModel.ShowGroup := GToggle.IsOn;

  ChartModel.SuperCalc;
end;

procedure TFormDiagramQ.PBoxClick(Sender: TObject);
begin
  ChartModel.PComboItemIndex := PBox.ItemIndex;

  if WantAutoUpdate then
  begin
    CalcBtnClick(nil);
  end;
end;

procedure TFormDiagramQ.YBoxClick(Sender: TObject);
begin
  if not GToggle.IsOn then
  begin
    ChartModel.YComboItemIndex := YBox.ItemIndex;
    ChartModel.Calc;
  end;
end;

procedure TFormDiagramQ.AToggleClick(Sender: TObject);
begin
  ChartModel.AP := not AToggle.IsOn;
  ChartModel.Calc;
  Main.FederTextCheckState;
end;

procedure TFormDiagramQ.GToggleClick(Sender: TObject);
begin
  ChartModel.ShowGroup := GToggle.IsOn;
  ChartModel.DrawGroup;
  YBox.Enabled := not ChartModel.ShowGroup;
  Main.FederTextCheckState;
  UpdateMemo;
end;

procedure TFormDiagramQ.UpDownClick(Sender: TObject; Button: TUDBtnType);
begin
  ChartModel.APWidth := UpDown.Position;
  ChartModel.SuperCalc;
end;

procedure TFormDiagramQ.RecordMax;
begin
  TempR := cr.Left + cr.Width;
  if TempR > FMaxRight then
    FMaxRight := TempR;

  TempB := cr.Top + cr.Height;
  if TempB > FMaxBottom then
    FMaxBottom := TempB;
end;

procedure TFormDiagramQ.StackH(c: TControl);
begin
  c.Left := cr.Left + cr.Width + Margin;
  c.Top := cr.Top;
  cr := c;
  RecordMax;
end;

procedure TFormDiagramQ.StackV(c: TControl);
begin
  c.Left := cr.Left;
  c.Top := cr.Top + cr.Height + Margin;
  cr := c;
  RecordMax;
end;

procedure TFormDiagramQ.AnchorVertical(c: TControl);
begin
  c.Height := ClientHeight - c.Top - Margin;
  c.Anchors := c.Anchors + [TAnchorKind.akBottom];
end;

procedure TFormDiagramQ.LayoutBtnClick(Sender: TObject);
begin
  Inc(Layout);
  if Layout = 3 then
    Layout := 1;
  LayoutComponents;
end;

procedure TFormDiagramQ.LayoutComponents;
begin
  FMaxRight := 0;
  FMaxBottom := 0;

  XBox.Left := Margin;
  XBox.Top := Margin;

  cr := XBox;

  case Layout of
    1: LayoutComponents1;
    2: LayoutComponents2;
  end;

  ClientWidth := FMaxRight + Margin;
  ClientHeight := FMaxBottom + Margin;
end;

procedure TFormDiagramQ.LayoutComponents1;
begin
  { Vertical ListBoxes }
  cr := XBox;
  StackV(PBox);
  StackH(UpDown);

  cr := PBox;
  StackV(LayoutBtn);

  cr := XBox;
  StackH(YBox);
  StackH(Image);
  StackV(AToggle);
  StackH(GToggle);
end;

procedure TFormDiagramQ.LayoutComponents2;
begin
  { Horizontal ListBoxes }
  StackH(PBox);
  StackH(YBox);
  StackH(GToggle);
  StackV(AToggle);
  StackV(UpDown);
  StackV(LayoutBtn);

  cr := XBox;
  StackV(Image);
end;

end.
