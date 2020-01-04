unit RiggVar.VM.FormMain;

interface

uses
  Vcl.Graphics,
  RggTypes;

type
  TViewModelMain = class
  private
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

    procedure UpdateView;

    function GetOpenFileName(dn, fn: string): string;
    function GetSaveFileName(dn, fn: string): string;
  end;

implementation

uses
  FrmMain;

{ TViewModelMain }

procedure TViewModelMain.UpdateView;
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

procedure TViewModelMain.FestItemClick;
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

function TViewModelMain.GetOpenFileName(dn, fn: string): string;
begin
  result := FormMain.GetOpenFileName(dn, fn);
end;

function TViewModelMain.GetSaveFileName(dn, fn: string): string;
begin
  result := FormMain.GetSaveFileName(dn, fn);
end;

procedure TViewModelMain.DrehbarItemClick;
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

procedure TViewModelMain.OSDlgItemClick;
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

procedure TViewModelMain.OhneItemClick;
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

procedure TViewModelMain.KnickenItemClick(ct: TCalcTyp);
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

procedure TViewModelMain.VonDerSeiteItemClick(vp: TViewPoint);
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

procedure TViewModelMain.ShowConsole;
begin
  ConsoleItemCaption := 'Console schlieﬂen';
  ConsoleItemHint := '  Anordnung der Dialoge aufheben';

  InputFormItemEnabled := True;
  OutputFormItemEnabled := True;
  GrafikFormItemEnabled := True;

  UpdateView;
end;

procedure TViewModelMain.HideConsole;
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

procedure TViewModelMain.HideDiagramm;
begin
  ChartFormItemCaption := 'Diagramm ...';
  ChartFormItemHint := '  Diagramm aktivieren';
  FormMain.ChartFormItem.Caption := 'Diagramm ...';
  FormMain.ChartFormItem.Hint := '  Diagramm aktivieren';
end;

procedure TViewModelMain.HideReport;
begin
  ReportFormItemCaption := 'Report ...';
  ReportFormItemHint := '  Report anzeigen';
  FormMain.ReportFormItem.Caption := 'Report ...';
  FormMain.ReportFormItem.Hint := '  Report anzeigen';
end;

procedure TViewModelMain.HideGrafik;
begin
  RotaFormItemCaption := '3D Grafik ...';
  RotaFormItemHint := '  3D Grafik anzeigen';
  FormMain.RotaFormItem.Caption := '3D Grafik ...';
  FormMain.RotaFormItem.Hint := '  3D Grafik anzeigen';
end;

end.
