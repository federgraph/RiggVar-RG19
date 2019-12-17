unit FrmGrafic;

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
  ExtCtrls,
  ComCtrls;

type
  TGrafikForm = class(TForm)
    ViewTab: TTabControl;
    pnGetriebe: TPanel;
    PaintBoxG: TPaintBox;
    procedure PaintBoxGClick(Sender: TObject);
    procedure ViewTabChange(Sender: TObject);
    procedure PaintBoxGPaint(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
  public
  end;

var
  GrafikForm: TGrafikForm;

implementation

{$R *.DFM}

uses
  FrmMain,
  RiggUnit;

procedure TGrafikForm.ViewTabChange(Sender: TObject);
begin
  with FormMain do
  begin
    if ViewTab.TabIndex = 0 then
      VonDerSeiteItemClick(VonDerSeiteItem)
    else if ViewTab.TabIndex = 1 then
      VonDerSeiteItemClick(VonHintenItem)
    else if ViewTab.TabIndex = 2 then
      VonDerSeiteItemClick(VonObenItem)
    else if ViewTab.TabIndex = 3 then
      VonDerSeiteItemClick(Von3DItem);
  end;
end;

procedure TGrafikForm.PaintBoxGClick(Sender: TObject);
begin
  RiggModul.ResetPaintBoxG;
end;

procedure TGrafikForm.PaintBoxGPaint(Sender: TObject);
begin
  RiggModul.Draw;
end;

procedure TGrafikForm.FormHide(Sender: TObject);
begin
  FormMain.GrafikFormItem.Checked := False;
end;

procedure TGrafikForm.FormShow(Sender: TObject);
begin
  ClientHeight := 457;
end;

end.
