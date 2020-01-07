unit FrmMemo;

interface

uses
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.StdCtrls,
  Vcl.ExtCtrls;

type
  TMemoFormC = class(TForm)
    Panel: TPanel;
    Memo: TMemo;
  private
  end;

var
  MemoFormC: TMemoFormC;

implementation

{$R *.DFM}

end.

