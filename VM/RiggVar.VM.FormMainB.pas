unit RiggVar.VM.FormMainB;

interface

uses
  RiggVar.VM.FormMain;

type
  TViewModelMainB = class(TViewModelMain00)
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
  FrmRG19B;

{ TViewModelMainB }

constructor TViewModelMainB.Create;
begin
  inherited;

  InputFormItemEnabled := True;
  OutputFormItemEnabled := True;
  GrafikFormItemEnabled := True;

  InputFormItemChecked := False;
  OutputFormItemChecked := False;
  GrafikFormItemChecked := False;
end;

function TViewModelMainB.GetOpenFileName(dn, fn: string): string;
begin
  result := FormRG19B.GetOpenFileName(dn, fn);
end;

function TViewModelMainB.GetSaveFileName(dn, fn: string): string;
begin
  result := FormRG19B.GetSaveFileName(dn, fn);
end;

procedure TViewModelMainB.HideDiagramm;
begin
  inherited;
  if FormRG19B <> nil then
  begin
    FormRG19B.ChartFormItem.Caption := ChartFormItemCaption;
    FormRG19B.ChartFormItem.Hint := ChartFormItemHint;
  end;
end;

procedure TViewModelMainB.HideGrafik;
begin
  inherited;
  if FormRG19B <> nil then
  begin
    FormRG19B.RotaFormItem.Caption := RotaFormItemCaption;
    FormRG19B.RotaFormItem.Hint := RotaFormItemHint;
  end;
end;

procedure TViewModelMainB.HideReport;
begin
  inherited;
  if FormRG19B <> nil then
  begin
    FormRG19B.ReportFormItem.Caption := ReportFormItemCaption;
    FormRG19B.ReportFormItem.Hint := ReportFormItemHint;
  end;
end;

procedure TViewModelMainB.UpdateView;
begin
  inherited;

  FormRG19B.LEDShape.Brush.Color := LEDColor;
  FormRG19B.Statusbar.Panels[1].Text := StatusPanelText1;
  FormRG19B.Caption := Caption;

  FormRG19B.FestItem.Checked := FestItemChecked;
  FormRG19B.DrehbarItem.Checked := DrehbarItemChecked;
  FormRG19B.OhneItem.Checked := OhneItemChecked;
  FormRG19B.OSDlgItem.Checked := OSDlgItemChecked;

  FormRG19B.WinkelItem.Checked := WinkelDown;
  FormRG19B.WinkelBtn.Down := WinkelDown;

  FormRG19B.WinkelItem.Enabled := WinkelEnabled;
  FormRG19B.WinkelBtn.Enabled := WinkelEnabled;

  FormRG19B.BiegeNeigeItem.Enabled := BiegeNeigeItemEnabled;
  FormRG19B.ReglerItem.Enabled := ReglerItemEnabled;
  FormRG19B.ReglerBtn.Enabled := ReglerBtnEnabled;

  FormRG19B.QuerKraftItem.Enabled := QuerKraftItemEnabled;
  FormRG19B.KnickenItem.Enabled := KnickenItemEnabled;
  FormRG19B.KraftGemessenItem.Enabled := KraftGemessenItemEnabled;
  FormRG19B.KorrigiertItem.Enabled := KorrigiertItemEnabled;

  FormRG19B.ControllerItem.Enabled := ControllerEnabled;
  FormRG19B.ControllerBtn.Enabled := ControllerEnabled;

  FormRG19B.KoppelkurveItem.Checked := KoppelKurveEnabled;
  FormRG19B.KoppelBtn.Down := KoppelKurveEnabled;

  FormRG19B.QuerKraftItem.Checked := QuerKraftItemChecked;
  FormRG19B.KnickenItem.Checked := KnickenItemChecked;
  FormRG19B.KorrigiertItem.Enabled := KorrigiertItemEnabled;
  FormRG19B.KraftGemessenItem.Checked := KraftGemessenItemChecked;

  FormRG19B.VonDerSeiteItem.Checked := VonDerSeiteItemChecked;
  FormRG19B.VonHintenItem.Checked := VonHintenItemChecked;
  FormRG19B.VonObenItem.Checked := VonObenItemChecked;
  FormRG19B.Von3DItem.Checked := Von3DItemChecked;

  FormRG19B.InputFormItem.Checked := InputFormItemChecked;
  FormRG19B.OutputFormItem.Checked := OutputFormItemChecked;
  FormRG19B.GrafikFormItem.Checked := GrafikFormItemChecked;

  FormRG19B.InputFormItem.Enabled := InputFormItemEnabled;
  FormRG19B.OutputFormItem.Enabled := OutputFormItemEnabled;
  FormRG19B.GrafikFormItem.Enabled := GrafikFormItemEnabled;

//  FormRG19B.ConsoleItem.Caption := ConsoleItemCaption;
  FormRG19B.ConsoleItem.Hint := ConsoleItemHint;
end;

end.
