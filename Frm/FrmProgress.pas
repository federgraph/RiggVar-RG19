unit FrmProgress;

interface

uses
  SysUtils,
  Messages,
  Classes,
  UITypes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  Buttons,
  ComCtrls;

type
  TProgressDlg = class(TForm)
    ParamLabel: TLabel;
    OKBtn: TBitBtn;
    StartBtn: TBitBtn;
    StopBtn: TBitBtn;
    Gauge: TProgressBar;
    procedure FormShow(Sender: TObject);
    procedure StartBtnClick(Sender: TObject);
    procedure StopBtnClick(Sender: TObject);
  private
    FOnStart: TNotifyEvent;
    procedure HandleError(E: Exception);
  public
    Aborted: Boolean;
    property OnStart: TNotifyEvent read FOnStart write FOnStart;
  end;

implementation

{$R *.DFM}

procedure TProgressDlg.FormShow(Sender: TObject);
begin
  Aborted := False;
  Gauge.Position := 0;
  ParamLabel.Caption := 'Fortschritt';
  StartBtn.Enabled := True;
  StopBtn.Enabled := False;
  OKBtn.Enabled := True;
end;

procedure TProgressDlg.StartBtnClick(Sender: TObject);
begin
  StartBtn.Enabled := False;
  StopBtn.Enabled := True;
  OKBtn.Enabled := False;
  try
    try
      if Assigned(FOnStart) then
        FOnStart(Self); // ChartForm.Calc;
    except
      on E: EIntError do HandleError(E);
      on E: EMathError do HandleError(E);
    end;
  finally
    { StopBtn.Enabled := False; }
    { OKBtn.Enabled := True; }
    Close;
  end;
end;

procedure TProgressDlg.HandleError(E: Exception);
var
  S: String;
begin
  S := Format('Exception vom Typ ''%s''.', [E.ClassName]);
  S := S + #13 + 'Bitte überprüfen Sie die Einstellungen.';
  MessageDlg(S, mtWarning, [mbOK], 0);
end;

procedure TProgressDlg.StopBtnClick(Sender: TObject);
begin
  Aborted := True;
  Hide;
end;

end.
