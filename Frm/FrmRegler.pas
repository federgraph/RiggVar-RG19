unit FrmRegler;

interface

uses
  Sysutils,
  Classes,
  Graphics,
  Forms,
  Controls,
  Buttons,
  StdCtrls,
  RggTypes;

type
  TCtrlDlg = class(TForm)
    sbMastfall: TScrollBar;
    sbSpannung: TScrollBar;
    sbBiegungS: TScrollBar;
    lbMastfall: TLabel;
    lbSpannung: TLabel;
    lbBiegungS: TLabel;
    lbZaehler: TLabel;
    ZaehlerEdit: TEdit;
    LoopBtn: TBitBtn; {startet die Regelung}
    OK: TBitBtn;
    procedure LoopBtnClick(Sender: TObject);
    procedure sbMastfallScroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    procedure SetupCtrls;
  public
    Zaehler: Integer;
    TrimmIst, TrimmSoll: TTrimm;
  end;

var
  CtrlDlg: TCtrlDlg;

implementation

{$R *.DFM}

uses
  Riggunit;

procedure TCtrlDlg.SetupCtrls;
begin
  sbMastfall.SetParams(1100, 1000, 1300);
  sbSpannung.SetParams(800, 500, 1500);
  sbBiegungS.SetParams(40, 10, 80);

  sbMastfall.SmallChange := 1;
  sbSpannung.SmallChange := 10;
  sbBiegungS.SmallChange := 1;

  sbMastfall.LargeChange := 10;
  sbSpannung.LargeChange := 100;
  sbBiegungS.LargeChange := 10;

  lbMastfall.Caption := Format('Mastfall = %d mm', [sbMastfall.Position]);
  lbSpannung.Caption := Format('Vorstagspannung = %d N', [sbSpannung.Position]);
  lbBiegungS.Caption := Format('Mastbiegung = %d mm', [sbBiegungS.Position]);
  ZaehlerEdit.Text := '0';
end;

procedure TCtrlDlg.LoopBtnClick(Sender: TObject);
begin
  TrimmSoll.Mastfall := sbMastfall.Position;
  TrimmSoll.Spannung := sbSpannung.Position;
  TrimmSoll.BiegungS := sbBiegungS.Position;
  TrimmSoll.BiegungC := TrimmIst.BiegungC;
  TrimmSoll.Flexwert := TrimmIst.Flexwert;
  ZaehlerEdit.Text := '0';
  Screen.Cursor := crHourGlass;
  try
    Zaehler := RiggModul.Rigg.Regeln(TrimmSoll);
    if RiggModul.Rigg.GetriebeOK then begin
    { GCtrls werden nicht sofort aktualisiert. Deshalb sind die Einstellwerte
      für Mastfall und Biegung noch exakt. Die Wanten haben ungeradzahlige Längen.
      In UpdateRigg werden die Labels und die Graphic richtig aktualisiert.
      Die GCtrls werden erst nach Schließen des Dialogfensters aktualisiert.
      Gerundet auf geradzahlige Wantenwerte wird aber erst nach erneuter
      Berechnung des Getriebes, ausgelöst vom Benutzer }
      RiggModul.DoGraphics;
      RiggModul.UpdateRigg;
   (* Alternative:
    { Die GCtrls werden sofort aktualisiert. Damit werden die Werte
      für die Wanten geradzahlig und die Einstellwerte für Mastfall und
      Biegung verändern sich. }
      RiggModul.UpdateGCtrls(RiggModul.Rigg.Glieder);
      RiggModul.UpdateGetriebe; *)
    end;
  finally
    if Zaehler = 20 then ZaehlerEdit.Text := 'Max'
    else ZaehlerEdit.Text := IntToStr(Zaehler);
    Screen.Cursor := crDefault;
  end;
end;

procedure TCtrlDlg.sbMastfallScroll(Sender: TObject;
  ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
  if Sender = sbMastfall then
    lbMastfall.Caption := Format('Mastfall = %d mm', [ScrollPos])
  else if Sender = sbSpannung then
    lbSpannung.Caption := Format('Vorstagspannung = %d N', [ScrollPos])
  else if Sender = sbBiegungS then
    lbBiegungS.Caption := Format('Mastbiegung = %d mm', [ScrollPos]);
end;

procedure TCtrlDlg.FormCreate(Sender: TObject);
begin
  SetupCtrls;
end;

procedure TCtrlDlg.FormShow(Sender: TObject);
begin
  ZaehlerEdit.Text := '0';
  TrimmIst := RiggModul.Rigg.Trimm;
end;

end.
