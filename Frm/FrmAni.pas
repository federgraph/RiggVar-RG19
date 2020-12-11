unit FrmAni;

interface

uses
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  Vcl.Forms,
  Vcl.Controls,
  Vcl.Buttons,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.ComCtrls;

type
  TAnimationForm = class(TForm)
    pnAnimation: TPanel;
    AnimateBtn: TSpeedButton;
    tbWinkel: TTrackBar;
    UpDownSelStart: TUpDown;
    UpDownSelEnd: TUpDown;
    UpDownStepCount: TUpDown;
    UpDownInterval: TUpDown;
    SelStartEdit: TEdit;
    SelEndEdit: TEdit;
    IntervalEdit: TEdit;
    StepCountEdit: TEdit;
    lbSelStart: TLabel;
    lbStepCount: TLabel;
    lbSelEnd: TLabel;
    lbInterval: TLabel;
    cbSinus: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure tbWinkelChange(Sender: TObject);
    procedure UpDownSelStartClick(Sender: TObject; Button: TUDBtnType);
    procedure UpDownSelEndClick(Sender: TObject; Button: TUDBtnType);
    procedure AnimateBtnClick(Sender: TObject);
    procedure IntervalEditChange(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
  private
  end;

var
  AnimationForm: TAnimationForm;

implementation

{$R *.DFM}

uses
  RiggVar.RG.Def,
  FrmAniRot,
  FrmCmd,
  RggTypes;

procedure TAnimationForm.FormCreate(Sender: TObject);
begin
  tbWinkel.Max := AniRotationForm.ParamMax[fpWinkel];
  tbWinkel.Position := AniRotationForm.ParamPos[fpWinkel];
  tbWinkel.Min := AniRotationForm.ParamMin[fpWinkel];
  tbWinkel.SelEnd := AniRotationForm.WinkelSelEnd;
  tbWinkel.SelStart := AniRotationForm.WinkelSelStart;
  tbWinkel.LineSize := 1;
  tbWinkel.PageSize := 2;
  tbWinkel.Frequency := 2;

  UpdownSelStart.Max := tbWinkel.Position - 5;
  SelStartEdit.Text := IntToStr(tbWinkel.SelStart);
  UpdownSelStart.Min := tbWinkel.Min;
  UpdownSelStart.Increment := 1;

  UpdownSelEnd.Max := tbWinkel.Max;
  SelEndEdit.Text := IntToStr(tbWinkel.SelEnd);
  UpdownSelEnd.Min := tbWinkel.Position + 5;
  UpdownSelEnd.Increment := 1;

  UpdownInterval.Max := 2000;
  UpdownInterval.Position := 200;
  UpdownInterval.Min := 10;
  UpdownInterval.Increment := 100;
  IntervalEdit.Text := IntToStr(200);

  UpdownStepCount.Max := AniStepCountMax;
  UpdownStepCount.Position := 10;
  UpdownStepCount.Min := 1;
  UpdownStepCount.Increment := 1;
  StepCountEdit.Text := IntToStr(10);
end;

procedure TAnimationForm.tbWinkelChange(Sender: TObject);
begin
  if AnimateBtn.Down then
    Exit;
  if Sender = tbWinkel then
    AniRotationForm.ParamProp[fpWinkel] := tbWinkel.Position;
end;

procedure TAnimationForm.UpDownSelStartClick(Sender: TObject;
  Button: TUDBtnType);
begin
  tbWinkel.SelStart := UpDownSelStart.Position;
end;

procedure TAnimationForm.UpDownSelEndClick(Sender: TObject;
  Button: TUDBtnType);
begin
  tbWinkel.SelEnd := UpDownSelEnd.Position;
end;

procedure TAnimationForm.IntervalEditChange(Sender: TObject);
begin
  AniRotationForm.Timer.Interval := UpDownInterval.Position;
  UpDownInterval.Increment := UpDownInterval.Position div 10;
end;

procedure TAnimationForm.AnimateBtnClick(Sender: TObject);
begin
  AniRotationForm.AnimationItemClick(Self);
end;

procedure TAnimationForm.FormHide(Sender: TObject);
begin
  if AniRotationForm.AniDlgItem <> nil then
    AniRotationForm.AniDlgItem.Checked := False;
end;

procedure TAnimationForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then Hide;
  if (ssCtrl in Shift) and (Key = VK_TAB) then
   if CommandForm.Visible then
    CommandForm.SetFocus
   else if AniRotationForm.Visible then
    AniRotationForm.SetFocus;
end;

procedure TAnimationForm.FormShow(Sender: TObject);
begin
  ActiveControl := tbWinkel;
end;

end.
