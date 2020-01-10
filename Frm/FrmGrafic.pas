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
  RggTypes,
  RiggUnit;

procedure TGrafikForm.ViewTabChange(Sender: TObject);
begin
  if ViewTab.TabIndex = 0 then
    RiggModul.Viewpoint := vpSeite
  else if ViewTab.TabIndex = 1 then
    RiggModul.Viewpoint := vpAchtern
  else if ViewTab.TabIndex = 2 then
    RiggModul.Viewpoint := vpTop
  else if ViewTab.TabIndex = 3 then
    RiggModul.Viewpoint := vp3D
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
  if RiggModul <> nil then
  begin
    RiggModul.ViewModelMain.GrafikFormItemChecked := False;
    RiggModul.ViewModelMain.UpdateView;
  end;
end;

procedure TGrafikForm.FormShow(Sender: TObject);
begin
  ClientHeight := 457;
end;

end.
