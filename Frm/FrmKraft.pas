unit FrmKraft;

interface

uses
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.Buttons,
  Vcl.ComCtrls,
  RggKraftGraph;

type
  TFormKraft = class(TForm)
    OutputPages: TPageControl;
    KraftSheet: TTabSheet;
    pnKraft: TPanel;
    KraftPaintBox: TImage;
    UpateBtn: TButton;
    procedure UpateBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    KraftGraph: TKraftGraph;
  public
    procedure UpdateGraph;
  end;

var
  FormKraft: TFormKraft;

implementation

{$R *.DFM}

uses
  RiggVar.App.Main;

procedure TFormKraft.FormCreate(Sender: TObject);
begin
  KraftGraph := TKraftGraph.Create(Main.Rigg);
  UpdateGraph;
end;

procedure TFormKraft.FormDestroy(Sender: TObject);
begin
  KraftGraph.Free;
end;

procedure TFormKraft.UpateBtnClick(Sender: TObject);
begin
  UpdateGraph;
end;

procedure TFormKraft.UpdateGraph;
begin
  Screen.Cursor := crHourGlass;
  KraftGraph.GetTestKurven;
  if Assigned(KraftPaintBox) then
  begin
    KraftGraph.Image := KraftPaintBox;
    KraftGraph.Draw;
  end;
  Screen.Cursor := crDefault;
end;

end.
