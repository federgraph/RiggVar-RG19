unit FrmReport;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  UITypes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  Menus,
  StdCtrls,
  ExtCtrls,
  Printers,
  uRggPrinter;

type
  TReportForm = class(TForm)
    FramePanel: TPanel;
    ReportMemo: TMemo;
    PrintDialog: TPrintDialog;
    SaveDialog: TSaveDialog;
    MainMenu: TMainMenu;
    ReportMenu: TMenuItem;
    KopierenItem: TMenuItem;
    DruckenItem: TMenuItem;
    SpeichernItem: TMenuItem;
    AktualisierenItem: TMenuItem;
    ElementeItem: TMenuItem;
    N4: TMenuItem;
    procedure FormShow(Sender: TObject);
    procedure CopyItemClick(Sender: TObject);
    procedure UpdateItemClick(Sender: TObject);
    procedure AuswahlItemClick(Sender: TObject);
    procedure PrintItemClick(Sender: TObject);
    procedure PrintToFileItemClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  public
    procedure GetReport;
  end;

var
  ReportForm: TReportForm;

implementation

{$R *.DFM}

uses
  RiggUnit,
  FrmSelect;

procedure TReportForm.GetReport;
begin
  RiggModul.WriteReportToMemo(ReportMemo);
end;

procedure TReportForm.FormShow(Sender: TObject);
begin
  GetReport;
end;

procedure TReportForm.CopyItemClick(Sender: TObject);
begin
  if ReportMemo.SelLength = 0 then ReportMemo.SelectAll;
  ReportMemo.CopyToClipBoard;
end;

procedure TReportForm.UpdateItemClick(Sender: TObject);
begin
  GetReport;
end;

procedure TReportForm.AuswahlItemClick(Sender: TObject);
begin
  if MemoDlg.ShowModal = mrOK then GetReport;
end;

procedure TReportForm.PrintItemClick(Sender: TObject);
var
  i: Integer;
  MemoText: TextFile;
begin
  if not RggPrinter.OKToPrint then
  begin
    MessageDlg('Kein Drucker konfiguriert.', mtInformation, [mbOK], 0);
    exit;
  end;

  if PrintDialog.Execute then
  begin
    AssignPrn(MemoText);
    try
      Rewrite(MemoText);
      Printer.Canvas.Font.Assign(ReportMemo.Font);
      for i := 1 to ReportMemo.Lines.Count - 1 do
        WriteLn(MemoText, '          ' + ReportMemo.Lines[i]);
    finally
      CloseFile(MemoText);
    end;
  end;
end;

procedure TReportForm.PrintToFileItemClick(Sender: TObject);
var
  S: String;
begin
  S := RiggModul.InifileName;
  if S <> '' then begin
    S := ExtractFileName(S);
    S := ChangeFileExt(S,'.txt'); end
  else S := 'RggReprt.txt';
  SaveDialog.FileName := S;
  if SaveDialog.Execute then
    ReportMemo.Lines.SaveToFile(SaveDialog.FileName);
end;

procedure TReportForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  RiggModul.ReportFormActive := False;
  Action := caFree;
end;

end.
