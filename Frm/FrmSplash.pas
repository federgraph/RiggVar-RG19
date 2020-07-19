unit FrmSplash;

interface

uses
  SysUtils,
  Classes,
  Forms,
  Dialogs,
  StdCtrls,
  Buttons,
  ExtCtrls,
  Graphics,
  Controls;

type
  TFormSplash = class(TForm)
    Panel: TPanel;
    Shape1: TShape;
    Shape3: TShape;
    Label1: TLabel;
    Label2: TLabel;
    Shape2: TShape;
    MastImage: TImage;
    Label4: TLabel;
    Label3: TLabel;
    Shape4: TShape;
    DelayTimer: TTimer;
    procedure DelayTimerTimer(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
  end;

var
  FormSplash: TFormSplash;

implementation

{$r *.DFM}

procedure TFormSplash.DelayTimerTimer(Sender: TObject);
begin
  DelayTimer.Enabled := False;
  Close;
end;

procedure TFormSplash.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TFormSplash.FormDestroy(Sender: TObject);
begin
  FormSplash := nil;
end;

end.
