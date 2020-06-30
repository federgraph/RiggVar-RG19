unit FrmRegler;

interface

uses
  System.Sysutils,
  System.Classes,
  Vcl.Graphics,
  Vcl.Forms,
  Vcl.Controls,
  Vcl.Buttons,
  Vcl.StdCtrls,
  RggTypes;

type
  TFormRegler = class(TForm)
    sbMastfall: TScrollBar;
    sbSpannung: TScrollBar;
    sbBiegungS: TScrollBar;
    lbMastfall: TLabel;
    lbSpannung: TLabel;
    lbBiegungS: TLabel;
    lbZaehler: TLabel;
    ZaehlerEdit: TEdit;
    LoopBtn: TBitBtn;
    OK: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LoopBtnClick(Sender: TObject);
    procedure sbMastfallScroll(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
  private
    procedure SetupCtrls;
  public
    Counter: Integer;
    TrimmIst: TTrimm;
    TrimmSoll: TTrimm;
  end;

var
  FormRegler: TFormRegler;

implementation

{$R *.DFM}

uses
  RggModul;

procedure TFormRegler.FormCreate(Sender: TObject);
begin
  SetupCtrls;
end;

procedure TFormRegler.FormShow(Sender: TObject);
begin
  ZaehlerEdit.Text := '0';
  TrimmIst := RiggModul.Rigg.Trimm;
end;

procedure TFormRegler.SetupCtrls;
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

procedure TFormRegler.LoopBtnClick(Sender: TObject);
begin
  TrimmSoll.Mastfall := sbMastfall.Position;
  TrimmSoll.Spannung := sbSpannung.Position;
  TrimmSoll.BiegungS := sbBiegungS.Position;
  TrimmSoll.BiegungC := TrimmIst.BiegungC;
  TrimmSoll.Flexwert := TrimmIst.Flexwert;
  ZaehlerEdit.Text := '0';
  Screen.Cursor := crHourGlass;
  try
    Counter := RiggModul.Rigg.Regeln(TrimmSoll);
    if RiggModul.Rigg.GetriebeOK then
    begin
    { GCtrls werden nicht sofort aktualisiert. Deshalb sind die Einstellwerte
      für Mastfall und Biegung noch exakt. Die Wanten haben ungeradzahlige Längen.
      In UpdateRigg werden die Labels und die Graphic richtig aktualisiert.
      Die GCtrls werden erst nach Schließen des Dialogfensters aktualisiert.
      Gerundet auf geradzahlige Wantenwerte wird aber erst nach erneuter
      Berechnung des Getriebes, ausgelöst vom Benutzer }
      RiggModul.DoGraphics;
      RiggModul.UpdateRigg;

//   Alternative:
    { Die GCtrls werden sofort aktualisiert. Damit werden die Werte
      für die Wanten geradzahlig und die Einstellwerte für Mastfall und
      Biegung verändern sich. }
//      RiggModul.UpdateGCtrls(RiggModul.Rigg.Glieder);
//      RiggModul.UpdateGetriebe;

    end;
  finally
    if Counter = 20 then
      ZaehlerEdit.Text := 'Max'
    else
      ZaehlerEdit.Text := IntToStr(Counter);
    Screen.Cursor := crDefault;
  end;
end;

procedure TFormRegler.sbMastfallScroll(Sender: TObject;
  ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
  if Sender = sbMastfall then
    lbMastfall.Caption := Format('Mastfall = %d mm', [ScrollPos])
  else if Sender = sbSpannung then
    lbSpannung.Caption := Format('Vorstagspannung = %d N', [ScrollPos])
  else if Sender = sbBiegungS then
    lbBiegungS.Caption := Format('Mastbiegung = %d mm', [ScrollPos]);
end;

end.
