unit FrmPreview;

interface

uses
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  Vcl.StdCtrls,
  Vcl.Buttons;

type
  TPreviewGForm = class(TForm)
    PaintBoxPanel: TPanel;
    PreviewGBox: TPaintBox;
    BitBtn1: TBitBtn;
    PrintBtn: TBitBtn;
    cbThickLines: TCheckBox;
    rgMediaChoice: TRadioGroup;
    SaveDlg: TSaveDialog;
    procedure PreviewGBoxPaint(Sender: TObject);
    procedure PrintBtnClick(Sender: TObject);
    procedure cbThickLinesClick(Sender: TObject);
  public
  end;

var
  PreviewGForm: TPreviewGForm;

implementation

{$R *.DFM}

uses
  RggModul;

procedure TPreviewGForm.PreviewGBoxPaint(Sender: TObject);
begin
  RiggModul.PreviewPaintBoxG;
end;

procedure TPreviewGForm.PrintBtnClick(Sender: TObject);
begin
  case rgMediaChoice.ItemIndex of
    { Drucker }
    0: RiggModul.PrintPaintBoxG;

    { Datei }
    1: if SaveDlg.Execute then
      RiggModul.MetaFileG.SaveToFile(SaveDlg.FileName);

      (* if RiggModul.PaintBtnDown then
           RiggModul.MetaFileG.SaveToFile(SaveDlg.FileName)
         else
         begin
           RiggForm.PaintBtnClick(nil);
           RiggModul.MetaFileG.SaveToFile(SaveDlg.FileName);
           RiggForm.PaintBtnClick(nil);
         end; *)

    { Zwischenablage }
    2: RiggModul.CopyMetaFileG;

  end;
end;

procedure TPreviewGForm.cbThickLinesClick(Sender: TObject);
begin
  RiggModul.PreviewPaintBoxG;
end;

end.

