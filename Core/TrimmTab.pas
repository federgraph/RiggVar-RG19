unit TrimmTab;

interface

uses
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  System.Types,
  System.Inifiles,
  System.Math,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl. Buttons,
  RggTypes;

const
  PunkteMax = 20; { maximale Anzahl Punkte im MemoScript }
  { Konstanten für Bezierkurve }
  BezierKurveVomGrad = 2; { quadratische Bezierkurve, 3 Control Points }
  AnzahlKurvenPunkte = 100; { AnzahlKurvenPunkte + 1 KurvenPunkte }

type
  { TTabellenTyp = (itKonstante, itGerade, itParabel, itBezier); }
  { siehe RggTypes }
  TBezier = class;

  TTrimmTab = class(TObject)
  private
    FTabellenTyp: TTabellenTyp;
    FValid: Boolean;
    Fry1: double; { immer y2/2 im Fall itParabel }
  protected
    a1, a2: double;
    procedure SetTabellenTyp(Value: TTabellenTyp);
    procedure SetMitte(Value: TPoint); virtual;
    function GetMitte: TPoint; virtual;
    procedure SetEndPunkt(Value: TPoint);
    function GetEndPunkt: TPoint;
    procedure SetEndwertKraft(Value: Integer); virtual;
    procedure SetEndwertWeg(Value: Integer); virtual;
    procedure SetTrimmTabDaten(Value: TTrimmTabDaten); virtual;
    function GetTrimmTabDaten: TTrimmTabDaten; virtual;
  public
    EvalDirection: Boolean;
    Kurve: array [1 .. PunkteMax] of TPoint; { Punkt 0 ist der NullPunkt }
    PunkteAnzahl: Integer; { tatsächliche Anzahl Punkte entsprechend Memo }
    EndKraftMin, EndWegMin, KraftMax, WegMax: Integer;
    Bezier: TBezier;
    x1, y1, x2, y2: Integer;

    constructor Create;
    destructor Destroy; override;

    procedure GetPolynom; virtual;

    procedure LoadFromIniFile(IniFile: TIniFile);
    procedure WriteToIniFile(IniFile: TIniFile);
    procedure LoadFromStream(S: TStream);
    procedure SaveToStream(S: TStream);
    procedure GetMemoLines(MemoLines: TStrings);
    procedure Draw(Canvas: TCanvas; Rect: TRect); virtual;
    procedure ProcessTrimmTab(Tabelle: TStrings);
    function EvalY(x: double): double; virtual;
    function EvalX(y: double): double; virtual;

    property TabellenTyp: TTabellenTyp read FTabellenTyp write SetTabellenTyp;
    property Valid: Boolean read FValid write FValid;
    property MittelPunkt: TPoint read GetMitte write SetMitte;
    property EndPunkt: TPoint read GetEndPunkt write SetEndPunkt;
    property EndwertKraft: Integer read x2 write SetEndwertKraft;
    property EndwertWeg: Integer read y2 write SetEndwertWeg;
    property TrimmtabDaten: TTrimmTabDaten read GetTrimmTabDaten write SetTrimmTabDaten;
  end;

  vec3 = record
    x, y, z: double;
  end;

  TControlPunkte = array [1 .. BezierKurveVomGrad + 1] of vec3;
  TBezierKurve = array [1 .. AnzahlKurvenPunkte + 1] of vec3;
  TKoeffizientenArray = array [1 .. BezierKurveVomGrad + 1] of Integer;

  TBezier = class(TObject)
  private
    c: TKoeffizientenArray; { n+1 }
    n: Integer; { there are n+1 Control Points }
    m: Integer; { there are m+1 points along the interval of 0<= u <= 1 }
    function BlendingValue(u: double; k: Integer): double;
    procedure ComputePoint(u: double; var pt: vec3);
  public
    curve: TBezierKurve; { m+1 }
    Controls: TControlPunkte; { n+1 }
    constructor Create;
    procedure ComputeCoefficients;
    procedure GenerateCurve;
  end;

implementation

{
  y-Achse: Weg in mm
  x-Achse: Kraft in N
  Die Kurve muß für 0 < y < y2 vollständig im 1.Quadranten liegen.
  TabellenTyp bzw. KurveTyp:
  itGerade: Gerade durch NullPunkt(0,0) und EndPunkt(x2,y2).
  itParabel: quadratisches Interpolationspolymom durch drei gegebene Punkte.
  1. Nullpunkt als Anfangspunkt der Kurve (0,0)
  2. Endpunkt der Kurve (x2,y2)
  3. Punkt in der Mitte, der die Krümmung bestimmt. (x1,y1)
  Für diesen Punkt wird nur der Kraftwert x1 eingegeben.
  Der Weg Fry1 ist immer y2/2.
  itBezier: Kurve durch Nullpunkt und Endpunkt. Die Tangente an die Kurve in
  diesen Punkten geht durch den Kontrollpunkt (x1,y1).
}

procedure PaintBackGround(Image: TBitMap);
var
  R: TRect;
begin
  R := Rect(0, 0, Image.Width, Image.Height);
  with Image.Canvas do
  begin
    Brush.Color := clBtnFace;
    FillRect(R);
  end;
end;

{ TTrimmTab }

constructor TTrimmTab.Create;
begin
  Bezier := TBezier.Create;
  EndKraftMin := 100;
  KraftMax := 3000; { in N }
  EndWegMin := 10;
  WegMax := 300; { in mm }
  x2 := 1000; { EndwertKraft in N }
  y2 := 100; { EndwertWeg in mm }
  x1 := x2 div 2;
  y1 := y2 div 2;
  TabellenTyp := itGerade; { --> SetTabellenTyp --> GetPolynom }
  TrimmtabDaten := DefaultTrimmTabDaten; // war vorher auskommentiert
end;

destructor TTrimmTab.Destroy;
begin
  Bezier.Free;
  inherited Destroy;
end;

procedure TTrimmTab.SetTabellenTyp(Value: TTabellenTyp);
begin
  // if Value = FTabellenTyp then Exit;
  FTabellenTyp := Value;
  case TabellenTyp of
    itKonstante:
      begin
      end;
    itGerade:
      begin
        x1 := x2 div 2;
        y1 := y2 div 2;
      end;
    itParabel:
      begin
        Fry1 := y2 / 2;
        y1 := y2 div 2;
      end;
    itBezier:
      begin
      end;
  end;
  MittelPunkt := MittelPunkt;
  GetPolynom;
end;

function TTrimmTab.GetTrimmTabDaten: TTrimmTabDaten;
begin
  if not Valid then
    GetPolynom;
  result.TabellenTyp := TabellenTyp;
  result.a0 := 0; // ergänzt
  result.x0 := 0; // ergänzt
  case TabellenTyp of
    itKonstante:
      begin
        result.a1 := x1;
        result.a2 := 0;
        result.x1 := y2;
        result.x2 := x2;
      end;
    itGerade:
      begin
        result.a1 := y2 / x2;
        result.a2 := 0;
        result.x1 := 0;
        result.x2 := x2;
      end;
    itParabel:
      begin
        result.a1 := a1;
        result.a2 := a2;
        result.x1 := x1;
        result.x2 := x2;
      end;
    itBezier:
      begin
        result.a1 := y1; { ControllPoint }
        result.a2 := x1; { ControllPoint }
        result.x1 := y2; { EndPunkt }
        result.x2 := x2; { EndPunkt }
      end;
  end;
end;

procedure TTrimmTab.SetTrimmTabDaten(Value: TTrimmTabDaten);
begin
  FTabellenTyp := Value.TabellenTyp; // SetTabellenTyp() hier nicht aufrufen
  Valid := True; // Aufruf von GetPolynom in EvalY() unterbinden
  a1 := Value.a1;
  a2 := Value.a2;
  case Value.TabellenTyp of
    itKonstante:
      begin
        x1 := Round(Value.a1);
        y2 := Round(Value.x1);
        x2 := Round(Value.x2);
      end;
    itGerade:
      begin
        x2 := Round(Value.x2);
        y2 := Round(EvalY(x2));
      end;
    itParabel:
      begin
        x1 := Round(Value.x1);
        x2 := Round(Value.x2);
        y2 := Round(EvalY(x2));
        y1 := y2 div 2;
        Fry1 := y2 / 2;
      end;
    itBezier:
      begin
        y1 := Round(Value.a1); { ControllPoint }
        x1 := Round(Value.a2); { ControllPoint }
        y2 := Round(Value.x1); { EndPoint }
        x2 := Round(Value.x2); { EndPoint }
        with Bezier do
        begin
          Controls[2].x := x1;
          Controls[2].y := y1;
          Controls[3].x := x2;
          Controls[3].y := y2;
        end;
      end; { itBezier }
  end; { case }
  TabellenTyp := Value.TabellenTyp; // SetTabellenTyp() aufrufen!
end;

procedure TTrimmTab.WriteToIniFile(IniFile: TIniFile);
var
  S, S1: string;
begin
  with IniFile do
  begin
    S := 'Trimmtabelle';
    WriteInteger(S, 'TabellenTyp', Ord(FTabellenTyp));
    with TrimmtabDaten do
    begin
      S1 := Format('%f', [a1]);
      WriteString(S, 'a1', S1);
      S1 := Format('%f', [a2]);
      WriteString(S, 'a2', S1);
      S1 := Format('%f', [x1]);
      WriteString(S, 'x1', S1);
      S1 := Format('%f', [x2]);
      WriteString(S, 'x2', S1);
    end;
  end;
end;

procedure TTrimmTab.SaveToStream(S: TStream);
var
  T: TTrimmTabDaten;
begin
  T := TrimmtabDaten;
  S.WriteBuffer(T, SizeOf(TTrimmTabDaten));
end;

procedure TTrimmTab.LoadFromIniFile(IniFile: TIniFile);
var
  S, S1: string;
  T: TTrimmTabDaten;
begin
  T := TrimmtabDaten; { T mit den aktuellen Werten als Default initialisieren }
  with IniFile do
  begin
    S := 'Trimmtabelle';
    try
      T.TabellenTyp := TTabellenTyp(ReadInteger(S, 'TabellenTyp', Ord(FTabellenTyp)));
      S1 := ReadString(S, 'a1', FloatToStrF(T.a1, ffGeneral, 8, 2));
      T.a1 := StrToFloat(S1);
      S1 := ReadString(S, 'a2', FloatToStrF(T.a2, ffGeneral, 8, 2));
      T.a2 := StrToFloat(S1);
      S1 := ReadString(S, 'x1', FloatToStrF(T.x1, ffGeneral, 8, 2));
      T.x1 := StrToFloat(S1);
      S1 := ReadString(S, 'x2', FloatToStrF(T.x2, ffGeneral, 8, 2));
      T.x2 := StrToFloat(S1);
    except
      on EConvertError do
        T := DefaultTrimmTabDaten;
    end;
  end;
  TrimmtabDaten := T;
end;

procedure TTrimmTab.LoadFromStream(S: TStream);
var
  T: TTrimmTabDaten;
begin
  try
    S.ReadBuffer(T, SizeOf(TTrimmTabDaten));
  except
    on EStreamError do
      T := DefaultTrimmTabDaten;
  end;
  TrimmtabDaten := T;
end;

procedure TTrimmTab.GetMemoLines(MemoLines: TStrings);
begin
  with MemoLines do
  begin
    Clear;
    Add('[X/mm = Y/N]');
    case TabellenTyp of
      itKonstante:
        begin
          Add(Format('%d=%d', [Round(y2 * 0.2), x1]));
          Add(Format('%d=%d', [Round(y2 * 0.4), x1]));
          Add(Format('%d=%d', [Round(y2 * 0.6), x1]));
          Add(Format('%d=%d', [Round(y2 * 0.8), x1]));
          Add(Format('%d=%d', [y2, x2]));
        end;
      itGerade, itParabel, itBezier:
        begin
          if EvalDirection then
          begin
            Add(Format('%d=%d', [Round(EvalY(x2 * 0.2)), Round(x2 * 0.2)]));
            Add(Format('%d=%d', [Round(EvalY(x2 * 0.4)), Round(x2 * 0.4)]));
            Add(Format('%d=%d', [Round(EvalY(x2 * 0.6)), Round(x2 * 0.6)]));
            Add(Format('%d=%d', [Round(EvalY(x2 * 0.8)), Round(x2 * 0.8)]));
            Add(Format('%d=%d', [y2, x2]));
          end
          else
          begin
            Add(Format('%d=%d', [Round(y2 * 0.2), Round(EvalX(y2 * 0.2))]));
            Add(Format('%d=%d', [Round(y2 * 0.4), Round(EvalX(y2 * 0.4))]));
            Add(Format('%d=%d', [Round(y2 * 0.6), Round(EvalX(y2 * 0.6))]));
            Add(Format('%d=%d', [Round(y2 * 0.8), Round(EvalX(y2 * 0.8))]));
            Add(Format('%d=%d', [y2, x2]));
          end;
        end;
    end; { case }
  end; { with Memo.Lines do }
end;

function TTrimmTab.GetEndPunkt;
begin
  result := Point(x2, y2);
end;

procedure TTrimmTab.SetEndPunkt(Value: TPoint);
begin
  Valid := False;
  EndwertKraft := Value.x;
  EndwertWeg := Value.y;
end;

procedure TTrimmTab.SetEndwertKraft(Value: Integer);
begin
  Valid := False;
  x2 := Value;
  if x2 < EndKraftMin then
    x2 := EndKraftMin;
  if x2 > KraftMax then
    x2 := KraftMax;
  case TabellenTyp of
    itKonstante:
      begin
      end;
    itGerade:
      begin
      end;
    itParabel:
      begin
      end;
    itBezier:
      begin
        Bezier.Controls[3].x := x2;
      end;
  end;
end;

procedure TTrimmTab.SetEndwertWeg(Value: Integer);
begin
  Valid := False;
  y2 := Value;
  if y2 < EndWegMin then
    y2 := EndWegMin;
  if y2 > WegMax then
    y2 := WegMax;
  case TabellenTyp of
    itGerade:
      begin
      end;
    itParabel:
      begin
        Fry1 := y2 / 2;
        y1 := y2 div 2;
      end;
    itBezier:
      begin
        Bezier.Controls[3].y := y2;
      end;
  end;
end;

procedure TTrimmTab.SetMitte(Value: TPoint);
var
  rTemp, min, max: double;
  iTemp: Integer;
  Wurzel2Halbe: double;
begin
  Valid := False;
  x1 := Value.x;
  y1 := Value.y;
  if x1 < 0 then
    x1 := 0; { ist garantiert }
  if y1 < 0 then
    y1 := 0; { ist garantiert }
  if x1 > x2 then
    x1 := x2;
  if y1 > y2 then
    y1 := y2;
  case TabellenTyp of
    itKonstante:
      begin
        y1 := y2 div 2; { Value.y wird ignoriert indem auf Standard gesetzt }
      end;
    itGerade:
      begin
        x1 := x2 div 2; { Value wird ignoriert }
        y1 := y2 div 2;
      end;
    itParabel:
      begin
        iTemp := Value.x;
        rTemp := iTemp;
        Wurzel2Halbe := sqrt(2) / 2;
        max := x2 * Wurzel2Halbe; { max := x2 * 4 div 5 }
        min := x2 - max; { min := x2 div 5 }
        if rTemp < min then
          iTemp := Ceil(min)
        else if rTemp > max then
          iTemp := Floor(max);
        x1 := iTemp;
        y1 := y2 div 2; { Value.y wird ignoriert }
      end;
    itBezier:
      begin
        Bezier.Controls[2].x := x1;
        Bezier.Controls[2].y := y1;
      end;
  end;
end;

function TTrimmTab.GetMitte: TPoint;
begin
  result := Point(x1, y1);
  (*
    case TabellenTyp of
    itKonstante: begin
    result := Point(x1,0);
    end;
    itGerade: begin
    result := Point(x1,y1);
    end;
    itParabel: begin
    result := Point(x1,Round(Fry1));
    end;
    itBezier: begin
    result := Point(Round(Bezier.Controls[2].x),Round(Bezier.Controls[2].y));
    end;
    end;
  *)
end;

procedure TTrimmTab.GetPolynom;
begin
  case TabellenTyp of
    itKonstante:
      begin
        a1 := x1;
      end;
    itGerade:
      begin
        a1 := y2 / x2;
      end;
    itParabel:
      begin
        a1 := (Fry1) / (x1);
        a2 := ((y2 - Fry1) / (x2 - x1) - a1) / (x2);
      end;
    itBezier:
      begin
        with Bezier do
        begin
          Controls[1].x := 0;
          Controls[1].y := 0;
          Controls[1].z := 0;
          Controls[2].x := x1;
          Controls[2].y := y1;
          Controls[2].z := 0;
          Controls[3].x := x2;
          Controls[3].y := y2;
          Controls[3].z := 0;
          ComputeCoefficients;
        end;
      end;
  end; { case }
  Valid := True;
end;

{ liefert den Weg y in mm als Funktion der Kraft x in N zurück }
function TTrimmTab.EvalY(x: double): double;
var
  KraftSoll, KraftIst, Diff: double; { Kräfte in N }
  uA, uB, uIst: double; { Parameter u: 0 <= u <= 1 }
  Zaehler: Integer;
  Temp: vec3;
begin
  result := 0;

  if not Valid then
    GetPolynom;
  { Maximalwert des Weges begrenzen auf das WegEnde y2 }
  if x > x2 then
  begin
    result := y2;
    Exit;
  end;
  if x < 0 then
  begin
    result := 0;
    Exit;
  end;

  case TabellenTyp of
    itKonstante:
      begin
        result := 0; { result ist undefiniert - ev. Exception auslösen }
      end;
    itGerade:
      begin
        result := a1 * x;
      end;
    itParabel:
      begin
        { result := a1*(x-x0) + a2*(x-x0)*(x-x1); }
        result := a1 * x + a2 * x * (x - x1);
      end;
    itBezier:
      begin
        KraftSoll := x; { nur der Lesbarkeit halber }
        uA := 0;
        uB := 1;
        Zaehler := 0;
        repeat
          Zaehler := Zaehler + 1;
          uIst := (uA + uB) / 2;
          Bezier.ComputePoint(uIst, Temp);
          KraftIst := Temp.x;
          Diff := KraftIst - KraftSoll;
          if Diff < 0 then
            uA := uIst
          else
            uB := uIst;
        until (abs(Diff) < 0.1) or (Zaehler = 100);
        if Zaehler < 100 then
          result := Temp.y
        else
          result := y2;
      end;
  end;
end;

{ liefert die Kraft x in N als Funktion des Weges y in mm zurück }
{ bzw. liefert Wantenspannung 3D in Abhängigkeit von der Auslenkung des Vorstags }
function TTrimmTab.EvalX(y: double): double;
var
  WegSoll, WegIst, Diff: double; { Wege in mm }
  KraftA, KraftB, KraftIst: double; { Kräfte in N }
  uA, uB, uIst: double;
  Zaehler: Integer;
  Temp: vec3;
begin
  result := 0;

  if not Valid then
    GetPolynom;
  { Maximalwert der Kraft begrenzen auf das KraftEnde x2 }
  if y > y2 then
  begin
    result := x2;
    Exit;
  end;
  if y < 0 then
  begin
    result := 0;
    Exit;
  end;

  case TabellenTyp of
    itKonstante:
      begin
        result := x1;
      end;
    itGerade:
      begin
        result := 1 / a1 * y;
      end;

    itParabel:
      begin { Umkehrfunktion zu y = a1*(x-x0) + a2*(x-x0)*(x-x1); }
        { Normalfall: Kraft zwischen Null und KraftEnde }
        WegSoll := y; { nur der Lesbarkeit halber }
        KraftA := 0; { KraftA := KraftAnfang; }
        KraftB := x2; { KraftB := KraftEnde; }
        Zaehler := 0;
        repeat
          Zaehler := Zaehler + 1;
          KraftIst := (KraftA + KraftB) / 2;
          WegIst := a1 * KraftIst + a2 * KraftIst * (KraftIst - x1);
          Diff := WegIst - WegSoll;
          if Diff < 0 then
            KraftA := KraftIst
          else
            KraftB := KraftIst;
        until (abs(Diff) < 0.01) or (Zaehler = 100);
        result := KraftIst;
      end;

    itBezier:
      begin
        WegSoll := y; { nur der Lesbarkeit halber }
        uA := 0;
        uB := 1;
        Zaehler := 0;
        repeat
          Zaehler := Zaehler + 1;
          uIst := (uA + uB) / 2;
          Bezier.ComputePoint(uIst, Temp);
          WegIst := Temp.y;
          Diff := WegIst - WegSoll;
          if Diff < 0 then
            uA := uIst
          else
            uB := uIst;
        until (abs(Diff) < 0.01) or (Zaehler = 100);
        if Zaehler < 100 then
          result := Temp.x
        else
          result := x2;
      end;
  end;
end;

procedure TTrimmTab.ProcessTrimmTab(Tabelle: TStrings);
var
  i, Code1, Code2: Integer;
  Punkt: TPoint;
  S, S1, S2: string;
begin
  Valid := False;
  PunkteAnzahl := 0;
  { Achtung: Endweg wird nicht richtig erfaßt, wenn EndKraftMin zu klein ist! }
  EndPunkt := Point(EndKraftMin, EndWegMin);
  { später Nebenwirkung über Eigenschaft }
  for i := 0 to Tabelle.Count - 1 do
  begin
    S := Tabelle[i];

    if S = '' then
      Continue; { Daher niemals Zugriff auf S[1] }
    if Pos('*', S) <> 0 then
      Continue; { vermeidet Zugriff auf S[1] }
    { Zugriff auf S[1] verursacht Exception, wenn String leer! }
    { if S[1] = '*' then Continue; } { '*' steht für ungültigen Eintrag }

    { String ohne '=' überspringen }
    if Pos('=', S) = 0 then
    begin
      Tabelle[i] := Concat('***', S);
      Continue;
    end;

    { Negative Zahlen nicht erlaubt }
    if Pos('-', S) <> 0 then
    begin
      Tabelle[i] := Concat('***', S);
      Continue;
    end;

    S1 := Tabelle.Names[i]; { String vor dem '=' }
    S2 := Tabelle.Values[S1]; { String nach dem '=' }
    Val(Trim(S1), Punkt.y, Code1); { Weg y }
    Val(Trim(S2), Punkt.x, Code2); { Kraft x }
    { Fehler bei der Umwandlung in Integerwert? }
    if (Code1 <> 0) or (Code2 <> 0) then
    begin
      Tabelle[i] := Concat('***', S);
      Continue;
    end;
    if PunkteAnzahl < PunkteMax then
    begin
      Inc(PunkteAnzahl);
      Kurve[PunkteAnzahl] := Punkt;
      if Punkt.x >= x2 then
        EndPunkt := Punkt;
    end;
  end;
  MittelPunkt := MittelPunkt; { Mittelpunkt auf Gültigkeit kontrollieren }
end;

procedure TTrimmTab.Draw(Canvas: TCanvas; Rect: TRect);

  function Limit(a: double): double;
  begin
    if a < -32000 then
      a := -32000
    else if a > 32000 then
      a := 32000;
    result := a;
  end;

var
  pt: TPoint;
  R: TRect;
  i, RadiusX, RadiusY: Integer;
  Bitmap: TBitMap;
  PlotWidth, PlotHeight: Integer;
  PlotExtX, PlotExtY: Integer;
  PlotOrgX, PlotOrgY: Integer;
  tempX, tempY: double;
  PosX, PosY: Integer;
  S: string;
begin
  if not Valid then
    GetPolynom;
  if TabellenTyp = itBezier then
    Bezier.GenerateCurve;

  PlotWidth := Rect.Right - Rect.Left;
  PlotHeight := Rect.Bottom - Rect.Top;
  PlotExtX := PlotWidth;
  PlotExtY := PlotHeight;
  PlotOrgX := 0;
  PlotOrgY := 0;

  Bitmap := TBitMap.Create;
  with Bitmap do
  begin
    Width := PlotWidth;
    Height := PlotHeight;
  end;
  try
    PaintBackGround(Bitmap);

    with Bitmap.Canvas do
    begin
      SetMapMode(Handle, MM_ANISOTROPIC);
      SetWindowExtEx(Handle, PlotExtX, -PlotExtY, nil);
      SetWindowOrgEx(Handle, PlotOrgX, PlotOrgY, nil);
      SetViewPortExtEx(Handle, PlotWidth, PlotHeight, nil);
      SetViewPortOrgEx(Handle, 0, PlotHeight, nil);

      { Radius }
      R.Left := 0;
      R.Top := 0;
      R.Bottom := 3;
      R.Right := 3;
      DPTOLP(Handle, R, 2);
      RadiusX := R.Right - R.Left;
      RadiusY := R.Bottom - R.Top;

      { Kurve }
      Pen.Color := clBlue;
      case TabellenTyp of
        itKonstante:
          begin
            tempY := PlotExtY * (x1 / EndwertKraft);
            pt.y := Round(Limit(tempY));
            MoveTo(0, pt.y);
            LineTo(PlotExtX, pt.y);
          end;
        itGerade:
          begin
            MoveTo(0, 0);
            LineTo(PlotExtX, PlotExtY);
          end;
        itParabel, itBezier:
          begin
            MoveTo(0, 0);
            tempX := 0;
            tempY := 0;
            for i := 0 to 100 do
            begin
              case TabellenTyp of
                itParabel:
                  begin
                    { EndwertWeg = EvalY(i/100*EndwertKraft) }
                    tempX := PlotExtX * EvalY(i / 100 * EndwertKraft) /
                      (EndwertWeg); { Weg }
                    tempY := PlotExtY * (i / 100);
                    { Kraft als Argument auf y-Achse }
                  end;
                itBezier:
                  begin
                    tempX := PlotExtX * Bezier.curve[i + 1].y / EndwertWeg;
                    { Weg }
                    tempY := PlotExtY * Bezier.curve[i + 1].x / EndwertKraft;
                    { Kraft }
                  end;
              end;
              pt.x := Round(Limit(tempX));
              pt.y := Round(Limit(tempY));
              LineTo(pt.x, pt.y);
            end;
          end;
      end;

      { Rechtecke }
      Pen.Color := clBlack;
      Brush.Color := clYellow;
      Brush.Style := bsSolid;
      for i := 1 to PunkteAnzahl do
      begin
        tempX := PlotExtX * Kurve[i].y / EndwertWeg;
        tempY := PlotExtY * Kurve[i].x / EndwertKraft;
        pt.x := Round(Limit(tempX));
        pt.y := Round(Limit(tempY));
        Rectangle(pt.x - RadiusX, pt.y - RadiusY, pt.x + RadiusX,
          pt.y + RadiusY);
      end;

      Pen.Color := clBlack;
      Brush.Color := clRed;
      Brush.Style := bsSolid;

      if TabellenTyp > itGerade then
      begin
        tempX := PlotExtX * y1 / EndwertWeg;
        tempY := PlotExtY * x1 / EndwertKraft;
        pt.x := Round(Limit(tempX));
        pt.y := Round(Limit(tempY));
        Rectangle(pt.x - RadiusX, pt.y - RadiusY, pt.x + RadiusX,
          pt.y + RadiusY);
      end;

      pt := Point(0, 0);
      Rectangle(pt.x - RadiusX, pt.y - RadiusY, pt.x + RadiusX, pt.y + RadiusY);

      tempX := PlotExtX * y2 / EndwertWeg;
      tempY := PlotExtY * x2 / EndwertKraft;
      pt.x := Round(Limit(tempX));
      pt.y := Round(Limit(tempY));
      Rectangle(pt.x - RadiusX, pt.y - RadiusY, pt.x + RadiusX, pt.y + RadiusY);

      SetMapMode(Handle, MM_TEXT);
      SetWindowOrgEx(Handle, 0, 0, nil);
      SetViewPortOrgEx(Handle, 0, 0, nil);

      { Texte }
      Brush.Style := bsClear;
      { Font := YFont; }
      SetTextAlign(Handle, TA_LEFT or TA_TOP);
      PosX := 5;
      PosY := 5;
      TextOut(PosX, PosY, 'Kraft [N]');

      Font.Color := clBlack;
      PosY := PosY - Font.Height + 5;
      S := Format('(%d ... %d)', [0, EndwertKraft]);
      TextOut(PosX, PosY, S);

      { Font := XFont; }
      SetTextAlign(Handle, TA_RIGHT or TA_BOTTOM);
      PosX := PlotWidth - 5;
      PosY := PlotHeight - 5;
      TextOut(PosX, PosY, 'Weg [mm]');

      Font.Color := clBlack;
      PosY := PosY + Font.Height - 5;
      S := Format('(%d ... %d)', [0, EndwertWeg]);
      TextOut(PosX, PosY, S);

      Brush.Style := bsSolid;
      Brush.Color := clBtnFace;

      { Rahmen zeichnen }
      {
        Pen.Width := 1;
        Pen.Color := clRed;
        Brush.Style := bsClear;
        Rectangle( 0, 0, Bitmap.Width, Bitmap.Height);
        }
    end; { with Bitmap.Canvas do begin }

    with Canvas do
    begin
      CopyMode := cmSrcCopy;
      Draw(0, 0, Bitmap);
    end;

  finally
    Bitmap.Free;
  end;
end;

{ TBezier }

constructor TBezier.Create;
begin
  n := BezierKurveVomGrad; { there are n+1 Control Points }
  m := AnzahlKurvenPunkte;
  { there are m+1 points along the interval of 0<= u <= 1 }
end;

procedure TBezier.ComputeCoefficients;
var
  j, k: Integer;
begin
  for k := 0 to n do
  begin
    { compute n!/(k!(n-k)!) }
    c[k + 1] := 1;
    for j := n downto k + 1 do
      c[k + 1] := c[k + 1] * j;
    for j := n - k downto 2 do
      c[k + 1] := c[k + 1] div j;
  end;
end;

function TBezier.BlendingValue(u: double; k: Integer): double;
var
  j: Integer;
  bv: double;
begin
  { compute c[k] * (u to kth power) * ((1-u) to (n-k) power) }
  bv := c[k];
  for j := 1 to k - 1 do
    bv := bv * u;
  for j := 1 to n - k + 1 do
    bv := bv * (1 - u);
  result := bv;
end;

procedure TBezier.ComputePoint(u: double; var pt: vec3);
var
  k: Integer;
  b: double;
begin
  pt.x := 0.0;
  pt.y := 0.0;
  pt.z := 0.0; { pt := Null; }
  for k := 1 to n + 1 do
  begin
    { add in influence of each control point }
    b := BlendingValue(u, k);
    pt.x := pt.x + Controls[k].x * b;
    pt.y := pt.y + Controls[k].y * b;
    pt.z := pt.z + Controls[k].z * b;
  end;
end;

procedure TBezier.GenerateCurve;
{ Uses n+1 control points. Generates curve by  finding m+1 points along
  the interval of 0 <= u <= 1 }
var
  k: Integer;
begin
  ComputeCoefficients;
  for k := 0 to m do
    ComputePoint(k / m, curve[k + 1]);
end;

end.
