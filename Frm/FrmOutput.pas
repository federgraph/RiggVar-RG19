unit FrmOutput;

interface

uses
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.Buttons,
  Vcl.ComCtrls;

type
  TOutputForm = class(TForm)
    OutputPages: TPageControl;
    MasterMemo: TTabSheet;
    Memo: TMemo;
    DetailsSheet: TTabSheet;
    DisplayMemo: TMemo;
    Salingsheet: TTabSheet;
    pnSaling: TPanel;
    SalingPaintBox: TPaintBox;
    ControllerSheet: TTabSheet;
    pnController: TPanel;
    ControllerPaintBox: TPaintBox;
    ZustellenBtn: TButton;
    KraftSheet: TTabSheet;
    pnKraft: TPanel;
    KraftPaintBox: TImage;
    UpdateKraftGraphBtn: TButton;
    ChartSheet: TTabSheet;
    pnChart2: TPanel;
    KurveBtn: TSpeedButton;
    PunktBtn: TSpeedButton;
    KurveValidLED: TShape;
    ChartPaintBox: TPaintBox;
    lbAchseX: TLabel;
    lbAchseY: TLabel;
    lbXLeft: TLabel;
    lbXRight: TLabel;
    lbYTop: TLabel;
    lbYBottom: TLabel;
    YComboBox: TComboBox;
    cbFollowPoint: TCheckBox;
    CommentSheet: TTabSheet;
    KommentarMemo: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure OutputPagesChange(Sender: TObject);
    procedure YComboBoxChange(Sender: TObject);
    procedure KurveBtnClick(Sender: TObject);
    procedure PunktBtnClick(Sender: TObject);
    procedure cbFollowPointClick(Sender: TObject);
    procedure UpdateKraftGraphBtnClick(Sender: TObject);
    procedure ZustellenBtnClick(Sender: TObject);
    procedure SalingPaintBoxClick(Sender: TObject);
    procedure SalingPaintBoxPaint(Sender: TObject);
    procedure ChartPaintBoxPaint(Sender: TObject);
    procedure ControllerPaintBoxPaint(Sender: TObject);
    procedure FormHide(Sender: TObject);
  public
    procedure SetKurveValidLED(Value: Boolean);
  end;

var
  OutputForm: TOutputForm;

implementation

{$R *.DFM}

uses
  RggModul;

procedure TOutputForm.FormCreate(Sender: TObject);
begin
  cbFollowPoint.Checked := False;
  YComboBox.ItemIndex := 3; { Exception when ItemIndex = -1 }
  OutputPages.ActivePage := OutputPages.Pages[0];
end;

procedure TOutputForm.FormDestroy(Sender: TObject);
begin
  if RiggModul <> nil then
  begin
    RiggModul.ControllerPaintBox := nil;
    RiggModul.SalingPaintBox := nil;
  end;
end;

procedure TOutputForm.YComboBoxChange(Sender: TObject);
begin
  RiggModul.YComboBoxChange(YComboBox.ItemIndex);
end;

procedure TOutputForm.KurveBtnClick(Sender: TObject);
begin
  RiggModul.KurveBtnClick;
end;

procedure TOutputForm.PunktBtnClick(Sender: TObject);
begin
  RiggModul.DrawPoint;
end;

procedure TOutputForm.cbFollowPointClick(Sender: TObject);
begin
  (*
  if cbFollowPoint.Checked <> cbFollowPoint2.Checked then begin
    if Sender = cbFollowPoint then cbFollowPoint2.Checked := cbFollowPoint.Checked;
    if Sender = cbFollowPoint2 then cbFollowPoint.Checked := cbFollowPoint2.Checked;
    DrawPoint;
  end;
  *)
  RiggModul.DrawPoint;
end;

procedure TOutputForm.ChartPaintBoxPaint(Sender: TObject);
begin
  RiggModul.DrawChartPaintBox(ChartPaintBox.Canvas,ChartPaintBox.BoundsRect);
end;

procedure TOutputForm.SalingPaintBoxClick(Sender: TObject);
begin
  RiggModul.SalingPaintBoxClick;
  SalingPaintBox.Invalidate;
end;

procedure TOutputForm.SalingPaintBoxPaint(Sender: TObject);
begin
  RiggModul.DrawPaintBoxS(SalingPaintBox.Canvas);
end;

procedure TOutputForm.ControllerPaintBoxPaint(Sender: TObject);
begin
  RiggModul.DrawPaintBoxC(ControllerPaintBox.Canvas);
end;

procedure TOutputForm.ZustellenBtnClick(Sender: TObject);
begin
  RiggModul.ControllerZustellenBtnClick;
end;

procedure TOutputForm.UpdateKraftGraphBtnClick(Sender: TObject);
begin
  RiggModul.UpdateKraftGraphBtnClick;
end;

procedure TOutputForm.OutputPagesChange(Sender: TObject);
begin
  RiggModul.OutputPagesChange(OutputPages.ActivePage.Tag);
end;

procedure TOutputForm.SetKurveValidLED(Value: Boolean);
begin
  if Value then
    KurveValidLED.Brush.Color := clLime
  else
    KurveValidLED.Brush.Color := clRed;
end;

procedure TOutputForm.FormHide(Sender: TObject);
begin
  if RiggModul <> nil then
  begin
    RiggModul.ViewModelM.OutputFormItemChecked := False;
    RiggModul.ViewModelM.UpdateView;
  end;
end;

end.
