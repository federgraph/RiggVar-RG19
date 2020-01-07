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
    procedure FormShow(Sender: TObject);
    procedure CopyItemClick(Sender: TObject);
    procedure UpdateItemClick(Sender: TObject);
    procedure AuswahlItemClick(Sender: TObject);
    procedure PrintItemClick(Sender: TObject);
    procedure PrintToFileItemClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    MainMenu: TMainMenu;
    ReportMenu: TMenuItem;
    KopierenItem: TMenuItem;
    DruckenItem: TMenuItem;
    SpeichernItem: TMenuItem;
    AktualisierenItem: TMenuItem;
    ElementeItem: TMenuItem;
    N4: TMenuItem;
    procedure InitMenu;
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

procedure TReportForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  RiggModul.ReportFormActive := False;
  Action := caFree;
end;

procedure TReportForm.FormCreate(Sender: TObject);
begin
  if Application.Title = 'RG19A' then
  begin
    FormStyle := fsMDIChild;
    InitMenu;
  end;
end;

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
  if MemoDlg.ShowModal = mrOK then
    GetReport;
end;

procedure TReportForm.PrintItemClick(Sender: TObject);
var
  i: Integer;
  MemoText: TextFile;
begin
  if not RggPrinter.OKToPrint then
  begin
    MessageDlg('Kein Drucker konfiguriert.', mtInformation, [mbOK], 0);
    Exit;
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
  s: string;
begin
  S := RiggModul.InifileName;
  if s <> '' then
  begin
    s := ExtractFileName(s);
    s := ChangeFileExt(S,'.txt');
  end
  else
    s := 'RggReprt.txt';
  SaveDialog.FileName := s;
  if SaveDialog.Execute then
    ReportMemo.Lines.SaveToFile(SaveDialog.FileName);
end;

procedure TReportForm.InitMenu;
var
  p: TMenuItem;
  mi: TMenuItem;

  function AddP(AName: string): TMenuItem;
  begin
    mi := TMenuItem.Create(MainMenu);
    mi.Name := AName;
    p := mi;
    MainMenu.Items.Add(p);
    result := mi;
  end;

  function AddI(AName: string): TMenuItem;
  begin
    mi := TMenuItem.Create(MainMenu);
    mi.Name := AName;
    p.Add(mi);
    result := mi;
  end;

begin
  MainMenu := TMainMenu.Create(Self);

  ReportMenu := AddP('ReportMenu');
  mi.Caption := '&Report';
  mi.GroupIndex := 8;
  mi.Hint := '  Befehle für den Report';

  KopierenItem := AddI('KopierenItem');
  mi.Caption := '&Kopieren';
  mi.Hint := '  Selektierten Text in die Zwischenablage kopieren';
  mi.OnClick := CopyItemClick;

  SpeichernItem := AddI('SpeichernItem');
  mi.Caption := '&Speichern unter ...';
  mi.Hint := '  Report in Textdatei speichern';
  mi.OnClick := PrintToFileItemClick;

  DruckenItem := AddI('DruckenItem');
  mi.Caption := '&Drucken ...';
  mi.Hint := '  Report ausdrucken';
  mi.OnClick := PrintItemClick;

  N4 := AddI('N4');
  mi.Caption := '-';

  AktualisierenItem := AddI('AktualisierenItem');
  mi.Caption := '&Aktualisieren';
  mi.Hint := '  Report aktualisieren';
  mi.OnClick := UpdateItemClick;

  ElementeItem := AddI('ElementeItem');
  mi.Caption := '&Elemente ausw'#228'hlen ...';
  mi.Hint := '  den Inhalt des Reports festlegen';
  mi.OnClick := AuswahlItemClick;
end;

end.
