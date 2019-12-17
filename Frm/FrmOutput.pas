unit FrmOutput;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  ExtCtrls,
  Buttons,
  ComCtrls;

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
    ZustellBtn: TButton;
    KraftSheet: TTabSheet;
    pnKraft: TPanel;
    KraftPaintBox: TPaintBox;
    TestBtn: TButton;
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
    procedure TestBtnClick(Sender: TObject);
    procedure ZustellBtnClick(Sender: TObject);
    procedure SalingPaintBoxClick(Sender: TObject);
    procedure SalingPaintBoxPaint(Sender: TObject);
    procedure ChartPaintBoxPaint(Sender: TObject);
    procedure ControllerPaintBoxPaint(Sender: TObject);
    procedure KraftPaintBoxPaint(Sender: TObject);
    procedure FormHide(Sender: TObject);
  public
    procedure SetKurveValidLED(Value: Boolean);
  end;

var
  OutputForm: TOutputForm;

implementation

{$R *.DFM}

uses
  FrmMain,
  RiggUnit;

procedure TOutputForm.FormCreate(Sender: TObject);
begin
  cbFollowPoint.Checked := False;
  YComboBox.ItemIndex := 1; {sonst Exception, wenn ItemIndex := -1}
end;

procedure TOutputForm.FormDestroy(Sender: TObject);
begin
  RiggModul.ControllerPaintBox := nil;
  RiggModul.SalingPaintBox := nil;
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

{****************************************************************************}

procedure TOutputForm.SalingPaintBoxClick(Sender: TObject);
begin
  RiggModul.SalingPaintBoxClick;
  SalingPaintBox.Invalidate;
end;

procedure TOutputForm.SalingPaintBoxPaint(Sender: TObject);
begin
  RiggModul.DrawPaintBoxS(SalingPaintBox.Canvas);
end;

{****************************************************************************}

procedure TOutputForm.ControllerPaintBoxPaint(Sender: TObject);
begin
  RiggModul.DrawPaintBoxC(ControllerPaintBox.Canvas);
end;

procedure TOutputForm.ZustellBtnClick(Sender: TObject);
begin
  RiggModul.ZustellBtnClick;
end;

{****************************************************************************}

procedure TOutputForm.KraftPaintBoxPaint(Sender: TObject);
begin
  RiggModul.Rigg.DrawPaintBoxK(KraftPaintBox.Canvas, KraftPaintBox.BoundsRect);
end;

procedure TOutputForm.TestBtnClick(Sender: TObject);
begin
  RiggModul.TestBtnClick;
end;

{****************************************************************************}

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
  FormMain.OutputFormItem.Checked := False;
end;

end.
