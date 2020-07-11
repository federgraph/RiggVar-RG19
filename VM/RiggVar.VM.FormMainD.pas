unit RiggVar.VM.FormMainD;

interface

uses
  RiggVar.VM.FormMain;

type
  TViewModelMainC = class(TViewModelMain00)
  public
    constructor Create;
    function GetOpenFileName(dn, fn: string): string; override;
    function GetSaveFileName(dn, fn: string): string; override;
    procedure HideDiagramm; override;
    procedure HideReport; override;
    procedure HideGrafik; override;
    procedure UpdateView; override;
  end;

implementation

uses
  FrmMain;

{ TViewModelMainC }

constructor TViewModelMainC.Create;
begin
  inherited;

  InputFormItemEnabled := True;
  OutputFormItemEnabled := True;
  GrafikFormItemEnabled := True;

  InputFormItemChecked := False;
  OutputFormItemChecked := False;
  GrafikFormItemChecked := False;
end;

function TViewModelMainC.GetOpenFileName(dn, fn: string): string;
begin
  result := FormMain.GetOpenFileName(dn, fn);
end;

function TViewModelMainC.GetSaveFileName(dn, fn: string): string;
begin
  result := FormMain.GetSaveFileName(dn, fn);
end;

procedure TViewModelMainC.HideDiagramm;
begin
  inherited;
  if FormMain <> nil then
  begin
    FormMain.ChartFormItem.Caption := ChartFormItemCaption;
    FormMain.ChartFormItem.Hint := ChartFormItemHint;
  end;
end;

procedure TViewModelMainC.HideGrafik;
begin
  inherited;
  if FormMain <> nil then
  begin
    FormMain.RotaFormItem.Caption := RotaFormItemCaption;
    FormMain.RotaFormItem.Hint := RotaFormItemHint;
  end;
end;

procedure TViewModelMainC.HideReport;
begin
  inherited;
  if FormMain <> nil then
  begin
    FormMain.ReportFormItem.Caption := ReportFormItemCaption;
    FormMain.ReportFormItem.Hint := ReportFormItemHint;
  end;
end;

procedure TViewModelMainC.UpdateView;
begin
  inherited;

  if not IsUp then
    Exit;

  FormMain.LEDShape.Brush.Color := LEDColor;
  FormMain.Statusbar.Panels[1].Text := StatusPanelText1;
  FormMain.Caption := Caption;

  FormMain.FestItem.Checked := FestItemChecked;
  FormMain.DrehbarItem.Checked := DrehbarItemChecked;
  FormMain.OSBItem.Checked := OSBItemChecked;
  FormMain.OSSItem.Checked := OSSItemChecked;

  FormMain.WinkelItem.Checked := WinkelDown;
  FormMain.WinkelBtn.Down := WinkelDown;

  FormMain.WinkelItem.Enabled := WinkelEnabled;
  FormMain.WinkelBtn.Enabled := WinkelEnabled;

//  FormMain.BogenItem.Checked := BogenItemChecked;
//
//  FormMain.KoppelItem.Checked := KoppelItemChecked;
//  FormMain.KoppelItem.Enabled := KoppelKurveEnabled;

  FormMain.KoppelBtn.Down := KoppelItemChecked;
  FormMain.KoppelBtn.Enabled := KoppelKurveEnabled;

  FormMain.BiegeNeigeItem.Enabled := BiegeNeigeItemEnabled;
  FormMain.ReglerItem.Enabled := ReglerItemEnabled;
  FormMain.ReglerBtn.Enabled := ReglerBtnEnabled;

  FormMain.QuerKraftItem.Enabled := QuerKraftItemEnabled;
  FormMain.KnickenItem.Enabled := KnickenItemEnabled;
  FormMain.KraftGemessenItem.Enabled := KraftGemessenItemEnabled;
  FormMain.KorrigiertItem.Enabled := KorrigiertItemEnabled;

  FormMain.ControllerItem.Enabled := ControllerEnabled;
  FormMain.ControllerBtn.Enabled := ControllerEnabled;
  FormMain.ControllerItem.Checked := ControllerDown;
  FormMain.ControllerBtn.Down := ControllerDown;

  FormMain.KoppelItem.Checked := KoppelKurveEnabled;
  FormMain.KoppelBtn.Down := KoppelKurveEnabled;

  FormMain.QuerKraftItem.Checked := QuerKraftItemChecked;
  FormMain.KnickenItem.Checked := KnickenItemChecked;
  FormMain.KorrigiertItem.Enabled := KorrigiertItemEnabled;
  FormMain.KraftGemessenItem.Checked := KraftGemessenItemChecked;

  FormMain.VonDerSeiteItem.Checked := VonDerSeiteItemChecked;
  FormMain.VonHintenItem.Checked := VonHintenItemChecked;
  FormMain.VonObenItem.Checked := VonObenItemChecked;
  FormMain.Von3DItem.Checked := Von3DItemChecked;

  FormMain.InputFormItem.Checked := InputFormItemChecked;
  FormMain.OutputFormItem.Checked := OutputFormItemChecked;
  FormMain.GrafikFormItem.Checked := GrafikFormItemChecked;

  FormMain.InputFormItem.Enabled := InputFormItemEnabled;
  FormMain.OutputFormItem.Enabled := OutputFormItemEnabled;
  FormMain.GrafikFormItem.Enabled := GrafikFormItemEnabled;

//  FormMain.ConsoleItem.Caption := ConsoleItemCaption;
  FormMain.ConsoleItem.Hint := ConsoleItemHint;
end;

end.
