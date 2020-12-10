unit FrmTabelle;

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
  RiggVar.App.Model,
  RggReport;

type
  TTableItem = (
    rL,
    rLe,
    rP,
    rPe,
    rF,
    DiffL,
    DiffP,
    Log,
    All
  );

  TFormTabelle = class(TForm)
    Memo: TMemo;
    ReportCombo: TComboBox;
    AllBtn: TButton;
    procedure ReportComboChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure AllBtnClick(Sender: TObject);
  private
    Rigg: TRigg;
    RiggReport: TRiggReport;
    FWReport: TFWReport;
    procedure ShowReport(Value: TTableItem);
    procedure InitReportComboItems;
    procedure WriteReportToMemo(Memo: TMemo);
  end;

var
  FormTabelle: TFormTabelle;

implementation

{$r *.dfm}

uses
  RggTypes,
  RggFachwerk,
  RiggVar.App.Main;

procedure TFormTabelle.FormCreate(Sender: TObject);
begin
  Rigg := Main.Rigg;

  RiggReport := TRiggReport.Create;
  FWReport := TFWReport.Create;

  ReportCombo.OnChange := nil;
  InitReportComboItems;
  ReportCombo.ItemIndex := 0;
  ReportCombo.OnChange := ReportComboChange;

  Memo.Left := 0;
  Memo.Width := ClientWidth;
  Memo.Height := ClientHeight - Memo.Top;
  Memo.Anchors := Memo.Anchors + [TAnchorKind.akRight, TAnchorKind.akBottom];
end;

procedure TFormTabelle.FormDestroy(Sender: TObject);
begin
  RiggReport.Free;
  FWReport.Free;
end;

procedure TFormTabelle.InitReportComboItems;
var
  ML: TStrings;
begin
  ML := ReportCombo.Items;
  ML.Clear;
  ML.Add('rL');
  ML.Add('rLe');
  ML.Add('rP');
  ML.Add('rPe');
  ML.Add('rF');
  ML.Add('DiffL');
  ML.Add('DiffP');
  ML.Add('Log');
end;

procedure TFormTabelle.ReportComboChange(Sender: TObject);
var
  ii: Integer;
begin
  ii := ReportCombo.ItemIndex;
  ShowReport(TTableItem(ii));
end;

procedure TFormTabelle.ShowReport(Value: TTableItem);
var
  ML: TStrings;
begin
  RiggReport.ML.Clear;
  RiggReport.ML.Add('');
  case Value of
    rL: RiggReport.AusgabeRL(Rigg.RiggLengths);
    rLe: RiggReport.AusgabeRLE(Rigg.RelaxedRiggLengths);
    rP: RiggReport.AusgabeRP(Rigg.RiggPoints);
    rPe: RiggReport.AusgabeRPE(Rigg.RelaxedRiggPoints);
    rF: RiggReport.AusgabeRF(Rigg.StabKraefte);
    DiffL: RiggReport.AusgabeDiffL(Rigg.RiggLengths, Rigg.RelaxedRiggLengths);
    DiffP: RiggReport.AusgabeDiffP(Rigg.RiggPoints, Rigg.RelaxedRiggPoints);
    Log: RiggReport.AusgabeLog(Rigg.LogList);
  end;

  ML := RiggReport.ML;
  ML.Add(' Angezeigt werden die zuletzt gültigen Werte.');
  ML.Add('');
  ML.Add(' Die Tabellenwerte sind aktuell und gültig, wenn');
  ML.Add(' - die LED Grün ist und');
  ML.Add(' - die Taste "=" gedrückt wurde bzw.');
  ML.Add(' - der Schalter "A" gedrückt ist.');
  ML.Add('');
  ML.Add(' Die Tabellenwerte können ungültig sein, wenn');
  ML.Add(' - die LED Rot ist und/oder');
  ML.Add(' - die Taste "=" nicht gedrückt wurde bzw.');
  ML.Add(' - der Schalter "A" nicht gedrückt ist');

  Memo.Lines.BeginUpdate;
  try
    Memo.Clear;
    Memo.Lines := RiggReport.ML;
    Memo.SelStart := 0;
  finally
    Memo.Lines.EndUpdate;
  end;
  RiggReport.ML.Clear;
end;

procedure TFormTabelle.WriteReportToMemo(Memo: TMemo);
var
  SavedIndexAuswahl: set of TRiggRodIndexRange;
begin
  Memo.Clear;

  { Rigg - Report }
  SavedIndexAuswahl := RiggReport.IndexAuswahlL;
  RiggReport.IndexAuswahlL := [0..19];
  RiggReport.IndexAuswahlP := [ooA0..ooP];
  RiggReport.ML.Clear;
  RiggReport.ML.Add('{Ausgaben Rigg 3d:}');
  RiggReport.ML.Add('');

//  for i := 0 to MemoDlg.DstList.Items.Count - 1 do
//  begin
//    if MemoDlg.DstList.Items[i] = 'rP' then
      RiggReport.AusgabeRP(Rigg.RiggPoints);
//    if MemoDlg.DstList.Items[i] = 'rPe' then
      RiggReport.AusgabeRPE(Rigg.RelaxedRiggPoints);
//    if MemoDlg.DstList.Items[i] = 'DiffP' then
      RiggReport.AusgabeDiffP(Rigg.RiggPoints, Rigg.RelaxedRiggPoints);
//    if MemoDlg.DstList.Items[i] = 'rL' then
      RiggReport.AusgabeRL(Rigg.RiggLengths);
//    if MemoDlg.DstList.Items[i] = 'rLe' then
      RiggReport.AusgabeRLE(Rigg.RelaxedRiggLengths);
//    if MemoDlg.DstList.Items[i] = 'DiffL' then
      RiggReport.AusgabeDiffL(Rigg.RiggLengths, Rigg.RelaxedRiggLengths);
//    if MemoDlg.DstList.Items[i] = 'rF' then
      RiggReport.AusgabeRF(Rigg.StabKraefte);
//    if MemoDlg.DstList.Items[i] = 'Winkel' then
      RiggReport.AusgabeAngle(Rigg.Angles);
//    if MemoDlg.DstList.Items[i] = 'TrimmControls' then
      RiggReport.AusgabeTrimmControls(Rigg.Glieder);
//    if MemoDlg.DstList.Items[i] = 'SalingDaten' then
      RiggReport.AusgabeSalingDaten(Rigg.SalingDaten);
//  end;

  Memo.Lines := RiggReport.ML;
  RiggReport.ML.Clear;
  RiggReport.IndexAuswahlL := SavedIndexAuswahl;
  RiggReport.IndexAuswahlP := [ooA..ooF];

  { Fachwerk - Report }
  Rigg.Fachwerk.BerechneVerschiebungen := True;
  Rigg.UpdateRigg;
  Rigg.Fachwerk.BerechneVerschiebungen := False;
  FWReport.ML.Clear;
  FWReport.ML.Add('{Ausgaben Fachwerk 2d:}');
  FWReport.ML.Add('');
//  for i := 0 to MemoDlg.DstList.Items.Count - 1 do
//  begin
    { output all}
//     FWReport.Ausgabe(Rigg.Fachwerk);

    { output selected reports }
//    if MemoDlg.DstList.Items[i] = 'FW_Geometrie' then
      FWReport.AusgabeGeometrie(Rigg.Fachwerk.G, Rigg.Fachwerk.S);
//    if MemoDlg.DstList.Items[i] = 'FW_StabQuerschnitte' then
      FWReport.AusgabeStabQuerschnitte(Rigg.Fachwerk.vektorEA, Rigg.Fachwerk.S);
//    if MemoDlg.DstList.Items[i] = 'FW_Elastizitaeten' then
      FWReport.AusgabeElastizitaeten(Rigg.Fachwerk.Q, Rigg.Fachwerk.S);
//    if MemoDlg.DstList.Items[i] = 'FW_Koordinaten' then
      FWReport.AusgabeKoordinaten(Rigg.Fachwerk.KX, Rigg.Fachwerk.KY, Rigg.Fachwerk.K);
//    if MemoDlg.DstList.Items[i] = 'FW_Belastung' then
      FWReport.AusgabeBelastung(Rigg.Fachwerk.FXsaved, Rigg.Fachwerk.FYsaved, Rigg.Fachwerk.K);
//    if MemoDlg.DstList.Items[i] = 'FW_Auflagerkraefte' then
      FWReport.AusgabeAuflagerkraefte(Rigg.Fachwerk.Lager);
//    if MemoDlg.DstList.Items[i] = 'FW_Stabkraefte' then
      FWReport.AusgabeStabkraefte(Rigg.Fachwerk.FS, Rigg.Fachwerk.S);
//    if MemoDlg.DstList.Items[i] = 'FW_Verschiebungen' then
      FWReport.AusgabeVerschiebungen(
        Rigg.Fachwerk.FO1,
        Rigg.Fachwerk.FO2,
        Rigg.Fachwerk.FO,
        PO1,
        PO2,
        Rigg.Fachwerk.K);
//  end;
  Memo.Lines.AddStrings(FWReport.ML);
  FWReport.ML.Clear;

  Memo.SelStart := 0;
end;

procedure TFormTabelle.AllBtnClick(Sender: TObject);
begin
  WriteReportToMemo(Memo);
end;

end.
