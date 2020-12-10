unit FrmBiege;

interface

uses
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  Vcl.Forms,
  Vcl.Controls,
  Vcl.Buttons,
  Vcl.StdCtrls,
  RggTypes,
  RiggVar.App.Model;

type
  TBiegeUndNeigeForm = class(TForm)
    sbMastfall: TScrollBar;
    sbBiegung: TScrollBar;
    lbMastfall: TLabel;
    lbBiegung: TLabel;
    BiegeBtn: TBitBtn;
    OK: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure BiegeBtnClick(Sender: TObject);
    procedure sbMastfallScroll(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
    procedure InitBtnClick(Sender: TObject);
  private
    Rigg: TRigg;
    procedure SetupCtrls;
    procedure UpdateLabels;
    procedure UpdateWithCurrentValue;
  public

  end;

var
  BiegeUndNeigeForm: TBiegeUndNeigeForm;

implementation

{$R *.DFM}

uses
  RiggVar.App.Main,
  RiggVar.RG.Def;

procedure TBiegeUndNeigeForm.FormCreate(Sender: TObject);
begin
  Rigg := Main.Rigg;
  SetupCtrls;
end;

procedure TBiegeUndNeigeForm.FormShow(Sender: TObject);
begin
  UpdateWithCurrentValue;
end;

procedure TBiegeUndNeigeForm.InitBtnClick(Sender: TObject);
begin
  UpdateWithCurrentValue;
end;

procedure TBiegeUndNeigeForm.UpdateWithCurrentValue;
var
  v: Integer;
  mf: double;
  mfv: double;
begin
  mf := Main.ParamValue[fpMastfallF0F];
  mfv := Main.ParamValue[fpMastfallVorlauf];
  v := Round(mf - mfv);
  if (v > sbMastfall.Min) and (v < sbMastfall.Max) then
    sbMastfall.Position := v;

  v := Round(Main.ParamValue[fpBiegung]);
  if (v > sbBiegung.Min) and (v < sbBiegung.Max) then
    sbBiegung.Position := v;

  UpdateLabels;
end;

procedure TBiegeUndNeigeForm.SetupCtrls;
begin
  sbMastfall.SetParams(1100, 1000, 1300);
  sbBiegung.SetParams(40, 10, 80);
  sbMastfall.SmallChange := 1;
  sbBiegung.SmallChange := 1;
  sbMastfall.LargeChange := 10;
  sbBiegung.LargeChange := 10;

//  UpdateLabels;
end;

procedure TBiegeUndNeigeForm.UpdateLabels;
begin
  lbMastfall.Caption := Format('Mastfall MF = %d mm', [sbMastfall.Position]);
  lbBiegung.Caption := Format('Biegung Bie = %d mm', [sbBiegung.Position]);
end;

procedure TBiegeUndNeigeForm.BiegeBtnClick(Sender: TObject);
var
  Mastfall, Biegung: double;
begin
  Screen.Cursor := crHourGlass;
  try
    Mastfall := sbMastfall.Position;
    Biegung := sbBiegung.Position;
    Rigg.BiegeUndNeigeF1(Mastfall, Biegung);
    Rigg.SchnittKraefte;
    { Getriebe nicht neu berechnen,
      damit die Einstellwerte nicht sofort gerundet werden. }
    if Rigg.GetriebeOK then
    begin
//      RiggModul.DoGraphics;
//      RiggModul.UpdateRigg;
      Main.UpdateGetriebe;
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TBiegeUndNeigeForm.sbMastfallScroll(Sender: TObject;
  ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
  if Sender = sbMastfall then
    lbMastfall.Caption := Format('Mastfall = %d mm', [ScrollPos])
  else if Sender = sbBiegung then
    lbBiegung.Caption := Format('Mastbiegung = %d mm', [ScrollPos]);
end;

end.
