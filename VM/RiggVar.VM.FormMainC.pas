unit RiggVar.VM.FormMainC;

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
  FrmRG19;

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
  result := FormRG19.GetOpenFileName(dn, fn);
end;

function TViewModelMainC.GetSaveFileName(dn, fn: string): string;
begin
  result := FormRG19.GetSaveFileName(dn, fn);
end;

procedure TViewModelMainC.HideDiagramm;
begin
  inherited;
  if FormRG19 <> nil then
  begin
    FormRG19.ChartFormItem.Caption := ChartFormItemCaption;
    FormRG19.ChartFormItem.Hint := ChartFormItemHint;
  end;
end;

procedure TViewModelMainC.HideGrafik;
begin
  inherited;
  if FormRG19 <> nil then
  begin
    FormRG19.RotaFormItem.Caption := RotaFormItemCaption;
    FormRG19.RotaFormItem.Hint := RotaFormItemHint;
  end;
end;

procedure TViewModelMainC.HideReport;
begin
  inherited;
  if FormRG19 <> nil then
  begin
    FormRG19.ReportFormItem.Caption := ReportFormItemCaption;
    FormRG19.ReportFormItem.Hint := ReportFormItemHint;
  end;
end;

procedure TViewModelMainC.UpdateView;
begin
  inherited;

  if not IsUp then
    Exit;

  FormRG19.LEDShape.Brush.Color := LEDColor;
  FormRG19.Statusbar.Panels[1].Text := StatusPanelText1;
  FormRG19.Caption := Caption;

  FormRG19.FestItem.Checked := FestItemChecked;
  FormRG19.DrehbarItem.Checked := DrehbarItemChecked;
  FormRG19.OhneItem.Checked := OhneItemChecked;
  FormRG19.OSDlgItem.Checked := OSDlgItemChecked;

  FormRG19.WinkelItem.Checked := WinkelDown;
  FormRG19.WinkelBtn.Down := WinkelDown;

  FormRG19.WinkelItem.Enabled := WinkelEnabled;
  FormRG19.WinkelBtn.Enabled := WinkelEnabled;

  FormRG19.BiegeNeigeItem.Enabled := BiegeNeigeItemEnabled;
  FormRG19.ReglerItem.Enabled := ReglerItemEnabled;
  FormRG19.ReglerBtn.Enabled := ReglerBtnEnabled;

  FormRG19.QuerKraftItem.Enabled := QuerKraftItemEnabled;
  FormRG19.KnickenItem.Enabled := KnickenItemEnabled;
  FormRG19.KraftGemessenItem.Enabled := KraftGemessenItemEnabled;
  FormRG19.KorrigiertItem.Enabled := KorrigiertItemEnabled;

  FormRG19.ControllerItem.Enabled := ControllerEnabled;
  FormRG19.ControllerBtn.Enabled := ControllerEnabled;
  FormRG19.ControllerItem.Checked := ControllerDown;
  FormRG19.ControllerBtn.Down := ControllerDown;

  FormRG19.KoppelkurveItem.Checked := KoppelKurveEnabled;

  if FormRG19.KoppelBtn <> nil then
  begin
    { Condition strictly no longer necessary because of IsUp guard. }
    { KoppelBtn is created at runtime,
      and may not be ready when UpdateView is called.}
    FormRG19.KoppelBtn.Down := KoppelKurveEnabled;
  end;

  FormRG19.QuerKraftItem.Checked := QuerKraftItemChecked;
  FormRG19.KnickenItem.Checked := KnickenItemChecked;
  FormRG19.KorrigiertItem.Enabled := KorrigiertItemEnabled;
  FormRG19.KraftGemessenItem.Checked := KraftGemessenItemChecked;

  FormRG19.VonDerSeiteItem.Checked := VonDerSeiteItemChecked;
  FormRG19.VonHintenItem.Checked := VonHintenItemChecked;
  FormRG19.VonObenItem.Checked := VonObenItemChecked;
  FormRG19.Von3DItem.Checked := Von3DItemChecked;

  FormRG19.InputFormItem.Checked := InputFormItemChecked;
  FormRG19.OutputFormItem.Checked := OutputFormItemChecked;
  FormRG19.GrafikFormItem.Checked := GrafikFormItemChecked;

  FormRG19.InputFormItem.Enabled := InputFormItemEnabled;
  FormRG19.OutputFormItem.Enabled := OutputFormItemEnabled;
  FormRG19.GrafikFormItem.Enabled := GrafikFormItemEnabled;

//  FormRG19.ConsoleItem.Caption := ConsoleItemCaption;
  FormRG19.ConsoleItem.Hint := ConsoleItemHint;
end;

end.
