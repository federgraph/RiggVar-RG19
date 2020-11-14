unit RggZug;

interface

uses
  System.Classes,
  Vcl.Graphics,
  RggTypes;

type
  TRaumGraphData = class
  public
    xA0, xB0, xC0, xD0, xE0, xF0, xA, xB, xC, xD, xE, xF: Integer;
    yA0, yB0, yC0, yD0, yE0, yF0, yA, yB, yC, yD, yE, yF: Integer;
    zA0, zB0, zC0, zD0, zE0, zF0, zA, zB, zC, zD, zE, zF: Integer;

    xP0, yP0: Integer;
    xX, yX: Integer;
    xY, yY: Integer;
    xZ, yZ: Integer;
    xM, yM: Integer;
    xN, yN: Integer;
    xP, yP: Integer;
  end;

  TRaumGraphProps = class
  public
    SalingTyp: TSalingTyp;
    ControllerTyp: TControllerTyp;
    BogenIndexD: Integer;
    Bogen: Boolean;
    Coloriert: Boolean;
    Color: TColor;
    Koppel: Boolean;
    Gestrichelt: Boolean;
    RiggLED: Boolean;
  end;

  TZug0 = class
  public
    OffsetX: Integer;
    OffsetY: Integer;
    Data: TRaumGraphData; // injected
    Props: TRaumGraphProps; // injected
  end;

  TZug3DBase = class(TZug0)
  public
    ZugRumpf: TZugPolyLine;
    ZugMast: TZugPolyLine;
    ZugMastKurve: TZugPolyLine;
    ZugSalingFS: TZugPolyLine;
    ZugSalingDS: TZugPolyLine;
    ZugWanteStb: TZugPolyLine;
    ZugWanteBb: TZugPolyLine;
    ZugController: TZugPolyLine;
    ZugVorstag: TZugPolyLine;
    ZugKoppelKurve: TZugPolyLine;
    ZugAchsen: TZugPolyLine;
    ZugMastfall: TZugPolyLine;
    ZugRP: TZugPolyLine;

    { no need to call SetLength for these, will be copied via Copy }
    ZugMastKurveD0D: TZugPolyLine;
    ZugMastKurveDC: TZugPolyLine;

    constructor Create;
    procedure FillZug; virtual; abstract;
    procedure DrawToCanvas(g: TCanvas); virtual; abstract;
    procedure GetPlotList(ML: TStrings); virtual;
  end;

implementation

{ TZug3DBase }

constructor TZug3DBase.Create;
begin
  inherited;
  SetLength(ZugRumpf, 8);
  SetLength(ZugMast, 4);
  SetLength(ZugMastKurve, BogenMax + 2);
  SetLength(ZugSalingFS, 4);
  SetLength(ZugSalingDS, 3);
  SetLength(ZugWanteStb, 3);
  SetLength(ZugWanteBb, 3);
  SetLength(ZugController, 2);
  SetLength(ZugVorstag, 2);
  SetLength(ZugAchsen, 4);
  SetLength(ZugMastfall, 3);
  SetLength(ZugRP, 4);
  SetLength(ZugKoppelKurve, 101);
end;

procedure TZug3DBase.GetPlotList(ML: TStrings);
begin

end;

end.
