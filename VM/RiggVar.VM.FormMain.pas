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
    OSBItemChecked: Boolean;
    OSSItemChecked: Boolean;

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

    BogenItemChecked: Boolean;

    KoppelItemChecked: Boolean;
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
    procedure OSBItemClick;
    procedure OSSItemClick;

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
  RggModul;

constructor TViewModelMain00.Create;
begin
  FestItemClick;
end;

function TViewModelMain00.GetOpenFileName(dn, fn: string): string;
begin
  result := '';
end;

function TViewModelMain00.GetSaveFileName(dn, fn: string): string;
begin
  result := '';
end;

procedure TViewModelMain00.FestItemClick;
begin
  FestItemChecked := True;
  DrehbarItemChecked := False;
  OSBItemChecked := False;
  OSSItemChecked := False;

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
  OSBItemChecked := False;
  OSSItemChecked := False;

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

procedure TViewModelMain00.OSBItemClick;
begin
  WinkelDown := False;

  FestItemChecked := False;
  DrehbarItemChecked := False;
  OSBItemChecked := True;
  OSSItemChecked := False;

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

procedure TViewModelMain00.OSSItemClick;
begin
  FestItemChecked := False;
  DrehbarItemChecked := False;
  OSBItemChecked := False;
  OSSItemChecked := True;

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
  ConsoleItemCaption := 'Console schließen';
  ConsoleItemHint := '  Anordnung der Dialoge aufheben';

  InputFormItemEnabled := False;
  OutputFormItemEnabled := False;
  GrafikFormItemEnabled := False;

  InputFormItemChecked := True;
  OutputFormItemChecked := True;
  GrafikFormItemChecked := True;

  UpdateView;
end;

procedure TViewModelMain00.UpdateView;
begin
  if RiggModul <> nil then
  begin
    ControllerEnabled := RiggModul.ControllerEnabled;
    ControllerDown := RiggModul.ControllerBtnDown;
    WinkelDown := RiggModul.WinkelBtnDown;
  end;
end;

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
end;

procedure TViewModelMain00.HideReport;
begin
  ReportFormItemCaption := 'Report ...';
  ReportFormItemHint := '  Report anzeigen';
end;

procedure TViewModelMain00.HideGrafik;
begin
  RotaFormItemCaption := '3D Grafik ...';
  RotaFormItemHint := '  3D Grafik anzeigen';
end;

end.
