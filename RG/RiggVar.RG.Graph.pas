unit RiggVar.RG.Graph;

interface

uses
  RggTypes,
  RggUnit4;

type
  TStrokeRigg = class
  private
    FRigg: TRigg;
  public
    SalingTyp: TSalingTyp;
    ControllerTyp: TControllerTyp;
    Koordinaten: TRealRiggPoints;
    WanteGestrichelt: Boolean;
    Bogen: Boolean;

    WantRenderH: Boolean;
    WantRenderP: Boolean;
    WantRenderF: Boolean;
    WantRenderE: Boolean;
    WantRenderS: Boolean;

    Kurve: array [0 .. BogenMax] of TRealPoint;

    constructor Create(rgg: TRigg);
    procedure SetMastKurve(f: TLineDataR100; L: double; beta: double);
  end;

implementation

uses
  RiggVar.App.Main;

{ TStrokeRigg }

constructor TStrokeRigg.Create(rgg: TRigg);
begin
  FRigg := rgg;
end;

procedure TStrokeRigg.SetMastKurve(f: TLineDataR100; L, beta: double);
var
  temp1, temp2, temp3, temp4, tempL: double;
  j, k: Integer;
begin
  temp1 := cos(pi / 2 - beta);
  temp2 := cos(beta);
  temp3 := sin(pi / 2 - beta);
  temp4 := sin(beta);
  for j := 0 to BogenMax do
  begin
    k := Round(100 / BogenMax * j);
    tempL := j * L / BogenMax;
    Kurve[j, x] := FRigg.rP[ooD0, x] - tempL * temp1 + f[k] * temp2;
    Kurve[j, y] := 0;
    Kurve[j, z] := FRigg.rP[ooD0, z] + tempL * temp3 + f[k] * temp4;
  end;
end;

end.
