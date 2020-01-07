unit FrmInfo;

interface

uses
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  Vcl.Forms,
  Vcl.Controls,
  Vcl.StdCtrls,
  Vcl.Buttons,
  Vcl.ExtCtrls;

type
  TFormInfo = class(TForm)
    OKButton: TButton;
    Memo: TMemo;
    procedure MemoKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  public
    procedure InitMemo;
  end;

procedure ShowInfo;

implementation

{$R *.DFM}

uses
  RiggVar.InfoMemo;

procedure ShowInfo;
begin
  with TFormInfo.Create(nil) do
  begin
    InitMemo;
    ShowModal;
    Free;
  end;
end;

procedure TFormInfo.MemoKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_Return) or (Key = VK_Escape) then
    Close;
end;

procedure TFormInfo.InitMemo;
var
  InfoMemo: TInfoMemo;
begin
  InfoMemo := TInfoMemo.Create;
  InfoMemo.Fill(Memo.Lines);
  InfoMemo.Free;
end;

end.

