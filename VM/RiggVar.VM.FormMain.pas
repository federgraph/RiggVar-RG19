unit RiggVar.VM.FormMain;

interface

uses
  Vcl.Graphics,
  RggTypes;

type
  TViewModelMain00 = class
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

    procedure FestItemClick;
    procedure DrehbarItemClick;
    procedure OSDlgItemClick;
    procedure OhneItemClick;

    procedure KnickenItemClick(ct: TCalcTyp);

    procedure VonDerSeiteItemClick(vp: TViewPoint);

    procedure ShowConsole;
    procedure HideConsole;
    procedure HideDiagramm;
    procedure HideReport;
    procedure HideGrafik;

    procedure UpdateView; virtual; abstract;

    function GetOpenFileName(dn, fn: string): string;
    function GetSaveFileName(dn, fn: string): string;
  end;

  TViewModelMainA = class(TViewModelMain00)
  public
    procedure UpdateView; override;
  end;

  TViewModelMainB = class(TViewModelMain00)
  public
    procedure UpdateView; override;
  end;

implementation

uses
  FrmRG19,
  FrmMain;

procedure TViewModelMain00.FestItemClick;
begin
  FestItemChecked := True;
  DrehbarItemChecked := False;
  OhneItemChecked := False;
  OSDlgItemChecked := False;

  WinkelEnabled := True;
  BiegeNeigeItemEnabled := True;
  ReglerItemEnabled := True;
  ReglerBtnEnabled := True;

  QuerKraftItemEnabled := True;
  KnickenItemEnabled := True;
  KraftGemessenItemEnabled := True;
  KorrigiertItemEnabled := True;

  KoppelKurveEnabled := True;
end;

function TViewModelMain00.GetOpenFileName(dn, fn: string): string;
begin
  result := FormMain.GetOpenFileName(dn, fn);
end;

function TViewModelMain00.GetSaveFileName(dn, fn: string): string;
begin
  result := FormMain.GetSaveFileName(dn, fn);
end;

procedure TViewModelMain00.DrehbarItemClick;
begin
  WinkelDown := False;

  FestItemChecked := False;
  DrehbarItemChecked := True;
  OhneItemChecked := False;
  OSDlgItemChecked := False;

  WinkelEnabled := False;
  WinkelDown := False;

  BiegeNeigeItemEnabled := True;
  ReglerItemEnabled := True;
  ReglerBtnEnabled := True;

  QuerKraftItemEnabled := True;
  KnickenItemEnabled := True;
  KraftGemessenItemEnabled := True;
  KorrigiertItemEnabled := True;

  KoppelKurveEnabled := False;
end;

procedure TViewModelMain00.OSDlgItemClick;
begin
  WinkelDown := False;

  FestItemChecked := False;
  DrehbarItemChecked := False;
  OhneItemChecked := False;
  OSDlgItemChecked := True;

  WinkelDown := False;
  WinkelEnabled := False;

  BiegeNeigeItemEnabled := False;
  ReglerItemEnabled := False;
  ReglerBtnEnabled := False;

  KoppelKurveEnabled := False;

  QuerKraftItemEnabled := False;
  KnickenItemEnabled := False;
  KraftGemessenItemEnabled := False;
  KorrigiertItemEnabled := False;
end;

procedure TViewModelMain00.OhneItemClick;
begin
  WinkelDown := False;

  FestItemChecked := False;
  DrehbarItemChecked := False;
  OhneItemChecked := True;
  OSDlgItemChecked := False;

  WinkelDown := False;
  WinkelEnabled := False;

  BiegeNeigeItemEnabled := False;
  ReglerItemEnabled := False;
  ReglerBtnEnabled := False;

  QuerKraftItemEnabled := True;
  KnickenItemEnabled := True;
  KraftGemessenItemEnabled := True;
  KorrigiertItemEnabled := True;

  KoppelKurveEnabled := False;
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

procedure TViewModelMain00.ShowConsole;
begin
  ConsoleItemCaption := 'Console schlieﬂen';
  ConsoleItemHint := '  Anordnung der Dialoge aufheben';

  InputFormItemEnabled := True;
  OutputFormItemEnabled := True;
  GrafikFormItemEnabled := True;

  UpdateView;
end;

procedure TViewModelMain00.HideConsole;
begin
  ConsoleItemCaption := 'Console ...';
  ConsoleItemHint := '  Dialoge im Formular anordnen';

  InputFormItemChecked := False;
  OutputFormItemChecked := False;
  GrafikFormItemChecked := False;

  InputFormItemEnabled := False;
  OutputFormItemEnabled := False;
  GrafikFormItemEnabled := False;

  UpdateView;
end;

procedure TViewModelMain00.HideDiagramm;
begin
  ChartFormItemCaption := 'Diagramm ...';
  ChartFormItemHint := '  Diagramm aktivieren';
  if FormMain <> nil then
  begin
    FormMain.ChartFormItem.Caption := 'Diagramm ...';
    FormMain.ChartFormItem.Hint := '  Diagramm aktivieren';
  end;
end;

procedure TViewModelMain00.HideReport;
begin
  ReportFormItemCaption := 'Report ...';
  ReportFormItemHint := '  Report anzeigen';
  if FormMain <> nil then
  begin
    FormMain.ReportFormItem.Caption := 'Report ...';
    FormMain.ReportFormItem.Hint := '  Report anzeigen';
  end;
end;

procedure TViewModelMain00.HideGrafik;
begin
  RotaFormItemCaption := '3D Grafik ...';
  RotaFormItemHint := '  3D Grafik anzeigen';
  if FormMain <> nil then
  begin
    FormMain.RotaFormItem.Caption := '3D Grafik ...';
    FormMain.RotaFormItem.Hint := '  3D Grafik anzeigen';
  end;
end;

{ TViewModelMainA }

procedure TViewModelMainA.UpdateView;
begin
  FormMain.LEDShape.Brush.Color := LEDColor;
  FormMain.Statusbar.Panels[1].Text := StatusPanelText1;
  FormMain.Caption := Caption;

  FormMain.FestItem.Checked := FestItemChecked;
  FormMain.DrehbarItem.Checked := DrehbarItemChecked;
  FormMain.OhneItem.Checked := OhneItemChecked;
  FormMain.OSDlgItem.Checked := OSDlgItemChecked;

  FormMain.WinkelItem.Checked := WinkelDown;
  FormMain.WinkelBtn.Down := WinkelDown;

  FormMain.WinkelItem.Enabled := WinkelEnabled;
  FormMain.WinkelBtn.Enabled := WinkelEnabled;

  FormMain.BiegeNeigeItem.Enabled := BiegeNeigeItemEnabled;
  FormMain.ReglerItem.Enabled := ReglerItemEnabled;
  FormMain.ReglerBtn.Enabled := ReglerBtnEnabled;

  FormMain.QuerKraftItem.Enabled := QuerKraftItemEnabled;
  FormMain.KnickenItem.Enabled := KnickenItemEnabled;
  FormMain.KraftGemessenItem.Enabled := KraftGemessenItemEnabled;
  FormMain.KorrigiertItem.Enabled := KorrigiertItemEnabled;

  FormMain.ControllerItem.Enabled := ControllerEnabled;
  FormMain.ControllerBtn.Enabled := ControllerEnabled;

  FormMain.KoppelkurveItem.Checked := KoppelKurveEnabled;
  FormMain.KoppelBtn.Down := KoppelKurveEnabled;

  FormMain.QuerKraftItem.Checked := QuerKraftItemChecked;
  FormMain.KnickenItem.Checked := KnickenItemChecked;
  FormMain.KorrigiertItem.Enabled := KorrigiertItemEnabled;
  FormMain.KraftGemessenItem.Checked := KraftGemessenItemChecked;

  FormMain.VonDerSeiteItem.Checked := VonDerSeiteItemChecked;
  FormMain.VonHintenItem.Checked := VonHintenItemChecked;
  FormMain.VonObenItem.Checked := VonObenItemChecked;
  FormMain.Von3DItem.Checked := Von3DItemChecked;

  FormMain.GrafikFormItem.Checked := GrafikFormItemChecked;
  FormMain.OutputFormItem.Checked := OutputFormItemChecked;
  FormMain.InputFormItem.Checked := InputFormItemChecked;

  FormMain.InputFormItem.Checked := InputFormItemChecked;
  FormMain.OutputFormItem.Checked := OutputFormItemChecked;
  FormMain.GrafikFormItem.Checked := GrafikFormItemChecked;

  FormMain.InputFormItem.Enabled := InputFormItemEnabled;
  FormMain.OutputFormItem.Enabled := OutputFormItemEnabled;
  FormMain.GrafikFormItem.Enabled := GrafikFormItemEnabled;

  FormMain.ConsoleItem.Caption := ConsoleItemCaption;
  FormMain.ConsoleItem.Hint := ConsoleItemHint;
end;

{ TViewModelMainB }

procedure TViewModelMainB.UpdateView;
begin
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

  FormRG19.KoppelkurveItem.Checked := KoppelKurveEnabled;
  FormRG19.KoppelBtn.Down := KoppelKurveEnabled;

  FormRG19.QuerKraftItem.Checked := QuerKraftItemChecked;
  FormRG19.KnickenItem.Checked := KnickenItemChecked;
  FormRG19.KorrigiertItem.Enabled := KorrigiertItemEnabled;
  FormRG19.KraftGemessenItem.Checked := KraftGemessenItemChecked;

  FormRG19.VonDerSeiteItem.Checked := VonDerSeiteItemChecked;
  FormRG19.VonHintenItem.Checked := VonHintenItemChecked;
  FormRG19.VonObenItem.Checked := VonObenItemChecked;
  FormRG19.Von3DItem.Checked := Von3DItemChecked;

  FormRG19.GrafikFormItem.Checked := GrafikFormItemChecked;
  FormRG19.OutputFormItem.Checked := OutputFormItemChecked;
  FormRG19.InputFormItem.Checked := InputFormItemChecked;

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
