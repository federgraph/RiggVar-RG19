﻿unit FrmBiege;

interface

uses 
  Windows, SysUtils, Classes, Graphics, Forms, Controls, Buttons, StdCtrls,
  RggTypes;

type
  TBiegeUndNeigeForm = class(TForm)
    sbMastfall: TScrollBar;
    sbBiegungS: TScrollBar;
    lbMastfall: TLabel;
    lbBiegungS: TLabel;
    BiegeBtn: TBitBtn;
    OK: TBitBtn;
    procedure BiegeBtnClick(Sender: TObject);
    procedure sbMastfallScroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);
    procedure FormCreate(Sender: TObject);
  private
    procedure SetupCtrls;
  public

  end;

var
  BiegeUndNeigeForm: TBiegeUndNeigeForm;

implementation

{$R *.DFM}

uses
  RiggUnit;

procedure TBiegeUndNeigeForm.SetupCtrls;
begin
  sbMastfall.SetParams(1100, 1000, 1300);
  sbBiegungS.SetParams(40, 10, 80);
  sbMastfall.SmallChange := 1;
  sbBiegungS.SmallChange := 1;
  sbMastfall.LargeChange := 10;
  sbBiegungS.LargeChange := 10;
  lbMastfall.Caption := Format('Mastfall = %d mm', [sbMastfall.Position]);
  lbBiegungS.Caption := Format('Mastbiegung = %d mm', [sbBiegungS.Position]);
end;

procedure TBiegeUndNeigeForm.BiegeBtnClick(Sender: TObject);
var
  Mastfall, Biegung: double;
begin
  Screen.Cursor := crHourGlass;
  try
    Mastfall := sbMastfall.Position;
    Biegung := sbBiegungS.Position;
    with RiggModul.Rigg do
    begin
      BiegeUndNeigeF(Mastfall, Biegung);
      SchnittKraefte;
    end;
    {Getriebe nicht neu berechnen, damit die Einstellwerte nicht sofort
     gerundet werden.}
    if RiggModul.Rigg.GetriebeOK then begin
      RiggModul.DoGraphics;
      RiggModul.UpdateRigg;
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
  else if Sender = sbBiegungS then
    lbBiegungS.Caption := Format('Mastbiegung = %d mm', [ScrollPos]);
end;

procedure TBiegeUndNeigeForm.FormCreate(Sender: TObject);
begin
  SetupCtrls;
end;

end.