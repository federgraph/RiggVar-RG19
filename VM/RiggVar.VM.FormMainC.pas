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
  FrmRG19C;

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
  result := FormRG19C.GetOpenFileName(dn, fn);
end;

function TViewModelMainC.GetSaveFileName(dn, fn: string): string;
begin
  result := FormRG19C.GetSaveFileName(dn, fn);
end;

procedure TViewModelMainC.HideDiagramm;
begin
  inherited;
  if FormRG19C <> nil then
  begin
    FormRG19C.ChartFormItem.Caption := ChartFormItemCaption;
    FormRG19C.ChartFormItem.Hint := ChartFormItemHint;
  end;
end;

procedure TViewModelMainC.HideGrafik;
begin
  inherited;
  if FormRG19C <> nil then
  begin
    FormRG19C.RotaFormItem.Caption := RotaFormItemCaption;
    FormRG19C.RotaFormItem.Hint := RotaFormItemHint;
  end;
end;

procedure TViewModelMainC.HideReport;
begin
  inherited;
  if FormRG19C <> nil then
  begin
    FormRG19C.ReportFormItem.Caption := ReportFormItemCaption;
    FormRG19C.ReportFormItem.Hint := ReportFormItemHint;
  end;
end;

procedure TViewModelMainC.UpdateView;
begin
  inherited;

  if not IsUp then
    Exit;

  FormRG19C.LEDShape.Brush.Color := LEDColor;
  FormRG19C.Statusbar.Panels[1].Text := StatusPanelText1;
  FormRG19C.Caption := Caption;

  FormRG19C.FestItem.Checked := FestItemChecked;
  FormRG19C.DrehbarItem.Checked := DrehbarItemChecked;
  FormRG19C.OhneItem.Checked := OhneItemChecked;
  FormRG19C.OSDlgItem.Checked := OSDlgItemChecked;

  FormRG19C.WinkelItem.Checked := WinkelDown;
  FormRG19C.WinkelBtn.Down := WinkelDown;

  FormRG19C.WinkelItem.Enabled := WinkelEnabled;
  FormRG19C.WinkelBtn.Enabled := WinkelEnabled;

//  FormRG19C.BogenBtn.Down := BogenBtnDown;

  FormRG19C.BiegeNeigeItem.Enabled := BiegeNeigeItemEnabled;
  FormRG19C.ReglerItem.Enabled := ReglerItemEnabled;
  FormRG19C.ReglerBtn.Enabled := ReglerBtnEnabled;

  FormRG19C.QuerKraftItem.Enabled := QuerKraftItemEnabled;
  FormRG19C.KnickenItem.Enabled := KnickenItemEnabled;
  FormRG19C.KraftGemessenItem.Enabled := KraftGemessenItemEnabled;
  FormRG19C.KorrigiertItem.Enabled := KorrigiertItemEnabled;

  FormRG19C.ControllerItem.Enabled := ControllerEnabled;
  FormRG19C.ControllerBtn.Enabled := ControllerEnabled;
  FormRG19C.ControllerItem.Checked := ControllerDown;
  FormRG19C.ControllerBtn.Down := ControllerDown;

  FormRG19C.KoppelkurveItem.Checked := KoppelKurveEnabled;
  FormRG19C.KoppelBtn.Down := KoppelKurveEnabled;

  FormRG19C.QuerKraftItem.Checked := QuerKraftItemChecked;
  FormRG19C.KnickenItem.Checked := KnickenItemChecked;
  FormRG19C.KorrigiertItem.Enabled := KorrigiertItemEnabled;
  FormRG19C.KraftGemessenItem.Checked := KraftGemessenItemChecked;

  FormRG19C.VonDerSeiteItem.Checked := VonDerSeiteItemChecked;
  FormRG19C.VonHintenItem.Checked := VonHintenItemChecked;
  FormRG19C.VonObenItem.Checked := VonObenItemChecked;
  FormRG19C.Von3DItem.Checked := Von3DItemChecked;

  FormRG19C.InputFormItem.Checked := InputFormItemChecked;
  FormRG19C.OutputFormItem.Checked := OutputFormItemChecked;
  FormRG19C.GrafikFormItem.Checked := GrafikFormItemChecked;

  FormRG19C.InputFormItem.Enabled := InputFormItemEnabled;
  FormRG19C.OutputFormItem.Enabled := OutputFormItemEnabled;
  FormRG19C.GrafikFormItem.Enabled := GrafikFormItemEnabled;

//  FormRG19C.ConsoleItem.Caption := ConsoleItemCaption;
  FormRG19C.ConsoleItem.Hint := ConsoleItemHint;
end;

end.
