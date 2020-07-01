unit RiggVar.VM.FormMain;

interface

uses
  Vcl.Graphics,
  RggTypes;

type
  TViewModelMain00 = class
  private
    FIsUp: Boolean;
    procedure SetIsUp(const Value: Boolean);
  public
    Caption: string;
    LEDColor: TColor;
    StatusPanelText1: string;

    FestItemChecked: Boolean;
    DrehbarItemChecked: Boolean;
    OhneItemChecked: Boolean;
    OSDlgItemChecked: Boolean;

    WinkelEnabled: Boolean;
    WinkelDown: Boolean;

//    BogenBtnDown: Boolean;

    BiegeNeigeItemEnabled: Boolean;
    ReglerItemEnabled: Boolean;
    ReglerBtnEnabled: Boolean;

    QuerKraftItemEnabled: Boolean;
    KnickenItemEnabled: Boolean;
    KraftGemessenItemEnabled: Boolean;
    KorrigiertItemEnabled: Boolean;

    ControllerEnabled: Boolean;
    ControllerDown: Boolean;

    KoppelKurveEnabled: Boolean;

    KnickenItemChecked: Boolean;
    KraftGemessenItemChecked: Boolean;
    QuerKraftItemChecked: Boolean;

    VonDerSeiteItemChecked: Boolean;
    VonHintenItemChecked: Boolean;
    VonObenItemChecked: Boolean;
    Von3DItemChecked: Boolean;

    GrafikFormItemChecked: Boolean;
    OutputFormItemChecked: Boolean;
    InputFormItemChecked: Boolean;

    InputFormItemEnabled: Boolean;
    OutputFormItemEnabled: Boolean;
    GrafikFormItemEnabled: Boolean;

    ConsoleItemCaption: string;
    ConsoleItemHint: string;
    ChartFormItemCaption: string;
    ChartFormItemHint: string;
    ReportFormItemCaption: string;
    ReportFormItemHint: string;
    RotaFormItemCaption: string;
    RotaFormItemHint: string;

    constructor Create;

    procedure FestItemClick;
    procedure DrehbarItemClick;
    procedure OSDlgItemClick;
    procedure OhneItemClick;

    procedure KnickenItemClick(ct: TCalcTyp);

    procedure VonDerSeiteItemClick(vp: TViewPoint);

    procedure ShowConsole;
    procedure HideConsole;
    procedure HideDiagramm; virtual;
    procedure HideReport; virtual;
    procedure HideGrafik; virtual;

    procedure UpdateView; virtual;

    function GetOpenFileName(dn, fn: string): string; virtual;
    function GetSaveFileName(dn, fn: string): string; virtual;
    property IsUp: Boolean read FIsUp write SetIsUp;
  end;

implementation

uses
  FrmMain,
  RggModul;

constructor TViewModelMain00.Create;
begin
  FestItemClick;

  InputFormItemEnabled := True;
  OutputFormItemEnabled := True;
  GrafikFormItemEnabled := True;

  InputFormItemChecked := False;
  OutputFormItemChecked := False;
  GrafikFormItemChecked := False;
end;

function TViewModelMain00.GetOpenFileName(dn, fn: string): string;
begin
  result := FormMain.GetOpenFileName(dn, fn);
end;

function TViewModelMain00.GetSaveFileName(dn, fn: string): string;
begin
  result := FormMain.GetSaveFileName(dn, fn);
end;

procedure TViewModelMain00.FestItemClick;
begin
  FestItemChecked := True;
  DrehbarItemChecked := False;
  OhneItemChecked := False;
  OSDlgItemChecked := False;

  WinkelEnabled := True;
  KoppelKurveEnabled := True;
  BiegeNeigeItemEnabled := True;
  ReglerItemEnabled := True;
  ReglerBtnEnabled := True;

  QuerKraftItemEnabled := True;
  KnickenItemEnabled := True;
  KraftGemessenItemEnabled := True;
  KorrigiertItemEnabled := True;

  UpdateView;
end;

procedure TViewModelMain00.DrehbarItemClick;
begin
  FestItemChecked := False;
  DrehbarItemChecked := True;
  OhneItemChecked := False;
  OSDlgItemChecked := False;

  WinkelEnabled := False;
  KoppelKurveEnabled := False;
  BiegeNeigeItemEnabled := True;
  ReglerItemEnabled := True;
  ReglerBtnEnabled := True;

  QuerKraftItemEnabled := True;
  KnickenItemEnabled := True;
  KraftGemessenItemEnabled := True;
  KorrigiertItemEnabled := True;

  UpdateView;
end;

procedure TViewModelMain00.OhneItemClick;
begin
  WinkelDown := False;

  FestItemChecked := False;
  DrehbarItemChecked := False;
  OhneItemChecked := True;
  OSDlgItemChecked := False;

  WinkelEnabled := False;
  KoppelKurveEnabled := False;
  BiegeNeigeItemEnabled := False;
  ReglerItemEnabled := False;
  ReglerBtnEnabled := False;

  QuerKraftItemEnabled := True;
  KnickenItemEnabled := True;
  KraftGemessenItemEnabled := True;
  KorrigiertItemEnabled := True;

  UpdateView;
end;

procedure TViewModelMain00.OSDlgItemClick;
begin
  FestItemChecked := False;
  DrehbarItemChecked := False;
  OhneItemChecked := False;
  OSDlgItemChecked := True;

  WinkelEnabled := False;
  KoppelKurveEnabled := False;
  BiegeNeigeItemEnabled := False;
  ReglerItemEnabled := False;
  ReglerBtnEnabled := False;

  QuerKraftItemEnabled := False;
  KnickenItemEnabled := False;
  KraftGemessenItemEnabled := False;
  KorrigiertItemEnabled := False;

  UpdateView;
end;

procedure TViewModelMain00.KnickenItemClick(ct: TCalcTyp);
begin
  KorrigiertItemEnabled := False;
  KnickenItemChecked := False;
  KraftGemessenItemChecked := False;
  QuerKraftItemChecked := False;

  case ct of
    ctQuerKraftBiegung: QuerKraftItemChecked := True;

    ctBiegeKnicken:
    begin
      KnickenItemChecked := True;
      KorrigiertItemEnabled := True;
    end;

    ctKraftGemessen:
    begin
      KraftGemessenItemChecked := True;
      ControllerDown := False;
    end;
  end;
end;

procedure TViewModelMain00.VonDerSeiteItemClick(vp: TViewPoint);
begin
  VonDerSeiteItemChecked := False;
  VonHintenItemChecked := False;
  VonObenItemChecked := False;
  Von3DItemChecked := False;
  case vp of
    vpSeite: VonDerSeiteItemChecked := True;
    vpAchtern: VonHintenItemChecked := True;
    vpTop: VonObenItemChecked := True;
    vp3D: Von3DItemChecked := True;
  end;
end;

procedure TViewModelMain00.SetIsUp(const Value: Boolean);
begin
  FIsUp := Value;
  if Value then
    UpdateView;
end;

procedure TViewModelMain00.ShowConsole;
begin
  ConsoleItemCaption := 'Console schlieﬂen';
  ConsoleItemHint := '  Anordnung der Dialoge aufheben';

  InputFormItemEnabled := False;
  OutputFormItemEnabled := False;
  GrafikFormItemEnabled := False;

  InputFormItemChecked := True;
  OutputFormItemChecked := True;
  GrafikFormItemChecked := True;

  UpdateView;
end;

//procedure TViewModelMain00.UpdateView;
//begin
//  if RiggModul <> nil then
//  begin
//    ControllerEnabled := RiggModul.ControllerEnabled;
//    ControllerDown := RiggModul.ControllerBtnDown;
//    WinkelDown := RiggModul.WinkelBtnDown;
//  end;
//end;

procedure TViewModelMain00.HideConsole;
begin
  ConsoleItemCaption := 'Console ...';
  ConsoleItemHint := '  Dialoge im Formular anordnen';

  InputFormItemEnabled := True;
  OutputFormItemEnabled := True;
  GrafikFormItemEnabled := True;

  InputFormItemChecked := False;
  OutputFormItemChecked := False;
  GrafikFormItemChecked := False;

  UpdateView;
end;

procedure TViewModelMain00.HideDiagramm;
begin
  ChartFormItemCaption := 'Diagramm ...';
  ChartFormItemHint := '  Diagramm aktivieren';

//  if FormMain <> nil then
//  begin
//    FormMain.ChartFormItem.Caption := ChartFormItemCaption;
//    FormMain.ChartFormItem.Hint := ChartFormItemHint;
//  end;
end;

procedure TViewModelMain00.HideReport;
begin
  ReportFormItemCaption := 'Report ...';
  ReportFormItemHint := '  Report anzeigen';

//  if FormMain <> nil then
//  begin
//    FormMain.ReportFormItem.Caption := ReportFormItemCaption;
//    FormMain.ReportFormItem.Hint := ReportFormItemHint;
//  end;
end;

procedure TViewModelMain00.HideGrafik;
begin
  RotaFormItemCaption := '3D Grafik ...';
  RotaFormItemHint := '  3D Grafik anzeigen';

//  if FormMain <> nil then
//  begin
//    FormMain.RotaFormItem.Caption := RotaFormItemCaption;
//    FormMain.RotaFormItem.Hint := RotaFormItemHint;
//  end;
end;

procedure TViewModelMain00.UpdateView;
begin
  inherited;

  if not IsUp then
    Exit;

//  FormMain.LEDShape.Brush.Color := LEDColor;
  FormMain.Statusbar.Panels[1].Text := StatusPanelText1;
  FormMain.Caption := Caption;

  FormMain.FestItem.Checked := FestItemChecked;
  FormMain.DrehbarItem.Checked := DrehbarItemChecked;
  FormMain.OhneItem.Checked := OhneItemChecked;
  FormMain.OSDlgItem.Checked := OSDlgItemChecked;

  FormMain.WinkelItem.Checked := WinkelDown;
//  FormMain.WinkelBtn.Down := WinkelDown;

  FormMain.WinkelItem.Enabled := WinkelEnabled;
//  FormMain.WinkelBtn.Enabled := WinkelEnabled;

//  FormMain.BogenBtn.Down := BogenBtnDown;

  FormMain.BiegeNeigeItem.Enabled := BiegeNeigeItemEnabled;
  FormMain.ReglerItem.Enabled := ReglerItemEnabled;
//  FormMain.ReglerBtn.Enabled := ReglerBtnEnabled;

  FormMain.QuerKraftItem.Enabled := QuerKraftItemEnabled;
  FormMain.KnickenItem.Enabled := KnickenItemEnabled;
  FormMain.KraftGemessenItem.Enabled := KraftGemessenItemEnabled;
  FormMain.KorrigiertItem.Enabled := KorrigiertItemEnabled;

  FormMain.ControllerItem.Enabled := ControllerEnabled;
//  FormMain.ControllerBtn.Enabled := ControllerEnabled;
  FormMain.ControllerItem.Checked := ControllerDown;
//  FormMain.ControllerBtn.Down := ControllerDown;

  FormMain.KoppelkurveItem.Checked := KoppelKurveEnabled;
//  FormMain.KoppelBtn.Down := KoppelKurveEnabled;

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
//  FormMain.GrafikFormItem.Checked := GrafikFormItemChecked;

  FormMain.InputFormItem.Enabled := InputFormItemEnabled;
  FormMain.OutputFormItem.Enabled := OutputFormItemEnabled;
//  FormMain.GrafikFormItem.Enabled := GrafikFormItemEnabled;
end;

end.
