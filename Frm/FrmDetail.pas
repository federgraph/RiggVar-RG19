unit FrmDetail;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.Buttons,
  Vcl.ComCtrls;

type
  TFormDetail = class(TForm)
    OutputPages: TPageControl;
    DetailsSheet: TTabSheet;
    DisplayMemo: TMemo;
    procedure FormCreate(Sender: TObject);
  public
    procedure AusgabeText;
  end;

var
  FormDetail: TFormDetail;

implementation

{$r *.dfm}

uses
  RiggVar.App.Main;

procedure TFormDetail.FormCreate(Sender: TObject);
begin
  Caption := 'Form Detail';
  AusgabeText;
end;

procedure TFormDetail.AusgabeText;
var
  MemoPosY: LongInt;
  ML: TStrings;
begin
  MemoPosY := SendMessage(DisplayMemo.Handle, EM_GETFIRSTVISIBLELINE, 0, 0);
  ML := DisplayMemo.Lines;
  ML.BeginUpdate;
  ML.Clear;

  Main.Rigg.AusgabeText(ML);

  SendMessage(DisplayMemo.Handle, EM_LINESCROLL, 0, MemoPosY);
  ML.EndUpdate;
end;

end.
