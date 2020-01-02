unit RiggVar.VM.FormMain;

interface

uses
  Vcl.Graphics,
  RggTypes;

type
  TViewModelMain = class
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

    procedure FestItemClick;
    procedure DrehbarItemClick;
    procedure OSDlgItemClick;
    procedure OhneItemClick;

    procedure KnickenItemClick(ct: TCalcTyp);

    procedure UpdateView;
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

end.
