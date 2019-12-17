unit FrmSplash;
{ Projekt: Rigg19
  Autor: Gustav Schubert
  LastUpdate: 10.10.97
}

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
  TRiggSplash = class(TForm)
    Panel1: TPanel;
    Shape1: TShape;
    Shape3: TShape;
    Label1: TLabel;
    Label2: TLabel;
    Shape2: TShape;
    ProctorImage: TImage;
    Label4: TLabel;
    Label3: TLabel;
    Shape4: TShape;
    DelayTimer: TTimer;
    procedure DelayTimerTimer(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  end;

var
  RiggSplash: TRiggSplash;

implementation

{$r *.DFM}

procedure TRiggSplash.DelayTimerTimer(Sender: TObject);
begin
  DelayTimer.Enabled := False;
  Close;
end;

procedure TRiggSplash.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

end.
