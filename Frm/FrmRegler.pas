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
  RggTypes,
  RggUnit4;

type
  TFormRegler = class(TForm)
    sbMastfall: TScrollBar;
    sbSpannung: TScrollBar;
    sbBiegung: TScrollBar;
    lbMastfall: TLabel;
    lbSpannung: TLabel;
    lbBiegung: TLabel;
    lbZaehler: TLabel;
    ZaehlerEdit: TEdit;
    LoopBtn: TBitBtn;
    OK: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure sbMastfallScroll(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
    procedure LoopBtnClick(Sender: TObject);
  private
    Rigg: TRigg;
    procedure SetupCtrls;
    procedure UpdateLabels;
    procedure UpdateWithCurrentValue;
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
  RiggVar.App.Main,
  RiggVar.RG.Def;

procedure TFormRegler.FormCreate(Sender: TObject);
begin
  Rigg := Main.Rigg;
  SetupCtrls;
end;

procedure TFormRegler.FormShow(Sender: TObject);
begin
  ZaehlerEdit.Text := '0';
  TrimmIst := Rigg.Trimm;
  UpdateWithCurrentValue;
end;

procedure TFormRegler.SetupCtrls;
begin
  sbMastfall.SetParams(1100, 1000, 1300);
  sbSpannung.SetParams(800, 500, 1500);
  sbBiegung.SetParams(40, 10, 80);

  sbMastfall.SmallChange := 1;
  sbSpannung.SmallChange := 10;
  sbBiegung.SmallChange := 1;

  sbMastfall.LargeChange := 10;
  sbSpannung.LargeChange := 100;
  sbBiegung.LargeChange := 10;
end;

procedure TFormRegler.UpdateLabels;
begin
  lbMastfall.Caption := Format('Mastfall = %d mm', [sbMastfall.Position]);
  lbSpannung.Caption := Format('Vorstagspannung = %d N', [sbSpannung.Position]);
  lbBiegung.Caption := Format('Mastbiegung = %d mm', [sbBiegung.Position]);
  ZaehlerEdit.Text := '0';
end;

procedure TFormRegler.UpdateWithCurrentValue;
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

procedure TFormRegler.LoopBtnClick(Sender: TObject);
begin
  TrimmSoll.Mastfall := sbMastfall.Position;
  TrimmSoll.Spannung := sbSpannung.Position;
  TrimmSoll.BiegungS := sbBiegung.Position;
  TrimmSoll.BiegungC := TrimmIst.BiegungC;
  TrimmSoll.Flexwert := TrimmIst.Flexwert;
  ZaehlerEdit.Text := '0';
  Screen.Cursor := crHourGlass;
  try
    Counter := Rigg.Regeln(TrimmSoll);
    if Rigg.GetriebeOK then
    begin
    { GCtrls werden nicht sofort aktualisiert. Deshalb sind die Einstellwerte
      für Mastfall und Biegung noch exakt. Die Wanten haben ungeradzahlige Längen.
      In UpdateRigg werden die Labels und die Graphic richtig aktualisiert.
      Die GCtrls werden erst nach Schließen des Dialogfensters aktualisiert.
      Gerundet auf geradzahlige Wantenwerte wird aber erst nach erneuter
      Berechnung des Getriebes, ausgelöst vom Benutzer }

//      RiggModul.DoGraphics;
//      RiggModul.UpdateRigg;

      Main.UpdateGetriebe;

    { Alternative: }
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
    lbMastfall.Caption := Format('Mastfall MF = %d mm', [ScrollPos])
  else if Sender = sbSpannung then
    lbSpannung.Caption := Format('Vorstagspannung = %d N', [ScrollPos])
  else if Sender = sbBiegung then
    lbBiegung.Caption := Format('Biegung Bie = %d mm', [ScrollPos]);
end;

end.
