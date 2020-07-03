unit RiggVar.VM.FormMainA;

interface

uses
  RiggVar.VM.FormMain;

type
  TViewModelMainA = class(TViewModelMain00)
  public
    function GetOpenFileName(dn, fn: string): string; override;
    function GetSaveFileName(dn, fn: string): string; override;
    procedure HideDiagramm; override;
    procedure HideReport; override;
    procedure HideGrafik; override;
    procedure UpdateView; override;
  end;

implementation

uses
  FrmRG19A;

{ TViewModelMainA }

function TViewModelMainA.GetOpenFileName(dn, fn: string): string;
begin
  result := FormRG19A.GetOpenFileName(dn, fn);
end;

function TViewModelMainA.GetSaveFileName(dn, fn: string): string;
begin
  result := FormRG19A.GetSaveFileName(dn, fn);
end;

procedure TViewModelMainA.HideDiagramm;
begin
  inherited;
  if FormRG19A <> nil then
  begin
    FormRG19A.ChartFormItem.Caption := ChartFormItemCaption;
    FormRG19A.ChartFormItem.Hint := ChartFormItemHint;
  end
end;

procedure TViewModelMainA.HideGrafik;
begin
  inherited;
  if FormRG19A <> nil then
  begin
    FormRG19A.RotaFormItem.Caption := RotaFormItemCaption;
    FormRG19A.RotaFormItem.Hint := RotaFormItemHint;
  end;
end;

procedure TViewModelMainA.HideReport;
begin
  inherited;
  if FormRG19A <> nil then
  begin
    FormRG19A.ReportFormItem.Caption := 'Report ...';
    FormRG19A.ReportFormItem.Hint := '  Report anzeigen';
  end
end;

procedure TViewModelMainA.UpdateView;
begin
  inherited;

  if not IsUp then
    Exit;

  FormRG19A.LEDShape.Brush.Color := LEDColor;
  FormRG19A.Statusbar.Panels[1].Text := StatusPanelText1;
  FormRG19A.Caption := Caption;

  FormRG19A.FestItem.Checked := FestItemChecked;
  FormRG19A.DrehbarItem.Checked := DrehbarItemChecked;
  FormRG19A.OSBItem.Checked := OSBItemChecked;
  FormRG19A.OSSItem.Checked := OSSItemChecked;

  FormRG19A.WinkelItem.Checked := WinkelDown;
  FormRG19A.WinkelBtn.Down := WinkelDown;

  FormRG19A.WinkelItem.Enabled := WinkelEnabled;
  FormRG19A.WinkelBtn.Enabled := WinkelEnabled;

  FormRG19A.BiegeNeigeItem.Enabled := BiegeNeigeItemEnabled;
  FormRG19A.ReglerItem.Enabled := ReglerItemEnabled;
  FormRG19A.ReglerBtn.Enabled := ReglerBtnEnabled;

  FormRG19A.QuerKraftItem.Enabled := QuerKraftItemEnabled;
  FormRG19A.KnickenItem.Enabled := KnickenItemEnabled;
  FormRG19A.KraftGemessenItem.Enabled := KraftGemessenItemEnabled;
  FormRG19A.KorrigiertItem.Enabled := KorrigiertItemEnabled;

  FormRG19A.ControllerItem.Enabled := ControllerEnabled;
  FormRG19A.ControllerBtn.Enabled := ControllerEnabled;
  FormRG19A.ControllerItem.Checked := ControllerDown;
  FormRG19A.ControllerBtn.Down := ControllerDown;

  FormRG19A.KoppelkurveItem.Checked := KoppelKurveEnabled;
  FormRG19A.KoppelBtn.Down := KoppelKurveEnabled;

  FormRG19A.QuerKraftItem.Checked := QuerKraftItemChecked;
  FormRG19A.KnickenItem.Checked := KnickenItemChecked;
  FormRG19A.KorrigiertItem.Enabled := KorrigiertItemEnabled;
  FormRG19A.KraftGemessenItem.Checked := KraftGemessenItemChecked;

  FormRG19A.VonDerSeiteItem.Checked := VonDerSeiteItemChecked;
  FormRG19A.VonHintenItem.Checked := VonHintenItemChecked;
  FormRG19A.VonObenItem.Checked := VonObenItemChecked;
  FormRG19A.Von3DItem.Checked := Von3DItemChecked;

  FormRG19A.InputFormItem.Checked := InputFormItemChecked;
  FormRG19A.OutputFormItem.Checked := OutputFormItemChecked;
  FormRG19A.GrafikFormItem.Checked := GrafikFormItemChecked;

  FormRG19A.InputFormItem.Enabled := InputFormItemEnabled;
  FormRG19A.OutputFormItem.Enabled := OutputFormItemEnabled;
  FormRG19A.GrafikFormItem.Enabled := GrafikFormItemEnabled;

  FormRG19A.ConsoleItem.Caption := ConsoleItemCaption;
  FormRG19A.ConsoleItem.Hint := ConsoleItemHint;
end;

end.
