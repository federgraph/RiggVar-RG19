unit RiggVar.RG.Graph;

interface

uses
  RggTypes,
  RggUnit4;

type
  IStrokeRigg = interface
  ['{6BEF1811-8B39-42C7-B04A-694343D7B27C}']
    procedure SetSalingTyp(const Value: TSalingTyp);
    procedure SetControllerTyp(const Value: TControllerTyp);

    procedure SetKoordinaten(const Value: TRealRiggPoints);
    procedure SetMastKurve(const Value: TMastKurve);
    procedure SetMastLineData(const Value: TLineDataR100; L: double; Beta: double);
    procedure SetKoppelKurve(const Value: TKoordLine);

    procedure SetWanteGestrichelt(const Value: Boolean);
    procedure SetBogen(const Value: Boolean);

    procedure SetFixPoint(const Value: TRiggPoint);
    procedure SetViewPoint(const Value: TViewPoint);

    function GetMastKurvePoint(const Index: Integer): TRealPoint;

    procedure ToggleRenderOption(const fa: Integer);
    function QueryRenderOption(const fa: Integer): Boolean;

    procedure Draw;
  end;

  TStrokeRigg = class
  private
    FRigg: TRigg;
  public
    Koordinaten: TRealRiggPoints;
    Kurve: TMastKurve;

    SalingTyp: TSalingTyp;
    ControllerTyp: TControllerTyp;
    WanteGestrichelt: Boolean;
    Bogen: Boolean;

    FixPoint: TRiggPoint;
    ViewPoint: TViewPoint;

    WantRenderH: Boolean;
    WantRenderP: Boolean;
    WantRenderF: Boolean;
    WantRenderE: Boolean;
    WantRenderS: Boolean;

    constructor Create(rgg: TRigg);

    procedure Draw;

    procedure SetMastLineData(const Value: TLineDataR100; L: double; Beta: double);

    procedure ToggleRenderOption(const fa: Integer);
    function QueryRenderOption(const fa: Integer): Boolean;
  end;

implementation

uses
  RiggVar.App.Main;

{ TStrokeRigg }

constructor TStrokeRigg.Create(rgg: TRigg);
begin
  FRigg := rgg;
end;

procedure TStrokeRigg.Draw;
begin

end;

function TStrokeRigg.QueryRenderOption(const fa: Integer): Boolean;
begin
  result := False;
end;

procedure TStrokeRigg.SetMastLineData(const Value: TLineDataR100; L: double; Beta: double);
var
  temp1, temp2, temp3, temp4, tempL: double;
  j, k: Integer;
begin
  temp1 := cos(pi / 2 - Beta);
  temp2 := cos(beta);
  temp3 := sin(pi / 2 - Beta);
  temp4 := sin(beta);
  for j := 0 to BogenMax do
  begin
    k := Round(100 / BogenMax * j);
    tempL := j * L / BogenMax;
    Kurve[j, x] := FRigg.rP[ooD0, x] - tempL * temp1 + Value[k] * temp2;
    Kurve[j, y] := 0;
    Kurve[j, z] := FRigg.rP[ooD0, z] + tempL * temp3 + Value[k] * temp4;
  end;
end;

procedure TStrokeRigg.ToggleRenderOption(const fa: Integer);
begin

end;

end.
