unit RiggVar.Graph1.Rigg;

interface

{$define WantDisplayList}

uses
  Types,
  SysUtils,
  Classes,
  IniFiles,
  Graphics,
  RiggVar.RG.Types,
  RiggVar.FD.Point,
  RiggVar.RG.Calc,
  RiggVar.RG.Def,
  RiggVar.FB.ActionConst,
  RiggVar.FB.Color,
{$ifdef WantDisplayList}
  RiggVar.Graph1.DisplayTypes,
  RiggVar.Graph1.DisplayList,
  RiggVar.Graph1.DisplayOrder,
{$endif}
  RiggVar.Graph1.Transform;

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

  TZug3D = class(TZug3DBase)
  public
    procedure FillZug; override;
    procedure DrawToCanvas(g: TCanvas); override;
    procedure GetPlotList(ML: TStrings); override;
  end;

  TRggGraph = class
  protected
    FColor: TColor;
    FColored: Boolean;
    procedure SetColor(const Value: TColor);
    procedure SetColored(const Value: Boolean);
    procedure SetFixPoint(const Value: TRiggPoint);
    procedure SetZoom(Value: single); virtual;
    function GetFixPoint: TRiggPoint;
    function GetZoom: single;
  protected
    GrafikOK: Boolean; // loaded with data
    Updated: Boolean; // transformed
    KoppelKurveNeedFill: Boolean;
  public
    RaumGraphData: TRaumGraphData;
    RaumGraphProps: TRaumGraphProps;

    Transformer: TRggTransformer; // injected, not owned

    constructor Create;
    destructor Destroy; override;

    procedure Update; virtual;
    procedure DrawToCanvas(Canvas: TCanvas); virtual;
    procedure GetPlotList(ML: TStrings); virtual;

    property FixPoint: TRiggPoint read GetFixPoint write SetFixPoint;
    property Zoom: single read GetZoom write SetZoom;
    property Coloriert: Boolean read FColored write SetColored;
    property Color: TColor read FColor write SetColor;
  end;

  TBootGraph = class(TRggGraph)
  private
    FSalingTyp: TSalingTyp;
    FControllerTyp: TControllerTyp;
    FKoppelKurve: TKoordLine;
    FKoppel: Boolean;
    FBogen: Boolean;
    FGestrichelt: Boolean;
    FViewPoint: TViewPoint;
    FRiggLED: Boolean;
    procedure SetKoppel(const Value: Boolean);
    procedure SetKoordinaten(const Value: TRiggPoints);
    procedure SetSalingTyp(const Value: TSalingTyp);
    procedure SetControllerTyp(const Value: TControllerTyp);
    procedure SetViewPoint(const Value: TViewPoint);
    procedure SetWanteGestrichelt(const Value: Boolean);
    procedure SetBogen(const Value: Boolean);
    procedure SetRiggLED(const Value: Boolean);
  protected
    BogenIndexD: Integer;
    function FindBogenIndexOf(P: TPoint3D): Integer;
    function GetFreshRiggPoints: TRiggPoints;
  public
    rP: TRiggPoints;
    Kurve: TMastKurve;

    constructor Create;

    function OnGetFixPunkt: TPoint3D;

    procedure LoadFromIniFile(FileName: string);

    procedure SetMastLineData(const Value: TLineDataR100; L: single; Beta: single);
    procedure SetMastKurve(const Value: TMastKurve);
    procedure SetKoppelKurve(const Value: TKoordLine);
    function GetMastKurvePoint(const Index: Integer): TPoint3D;

    property Koordinaten: TRiggPoints read rP write SetKoordinaten;
    property KoppelKurve: TKoordLine read FKoppelKurve write SetKoppelKurve;
    property Koppel: Boolean read FKoppel write SetKoppel;
    property ControllerTyp: TControllerTyp read FControllerTyp write SetControllerTyp;
    property SalingTyp: TSalingTyp read FSalingTyp write SetSalingTyp;
    property ViewPoint: TViewPoint read FViewPoint write SetViewPoint;
    property Bogen: Boolean read FBogen write SetBogen;
    property WanteGestrichelt: Boolean read FGestrichelt write SetWanteGestrichelt;
    property RiggLED: Boolean read FRiggLED write SetRiggLED;
  end;

  TRaumGraph = class(TBootGraph)
  protected
    { original definition of Achsen }
    AchseN: TPoint3D;
    AchseX: TPoint3D;
    AchseY: TPoint3D;
    AchseZ: TPoint3D;

    { transformed coordinates Achsen }
    AchseNT: TPoint3D;
    AchseXT: TPoint3D;
    AchseYT: TPoint3D;
    AchseZT: TPoint3D;

    { transformed coordinates of Rigg }
    A0, B0, C0, D0, E0, F0, P0: TPoint3D;
    A,  B,  C,  D,  E,  F,  P:  TPoint3D;
    M, N: TPoint3D;
  protected
    Zug3D: TZug3DBase; // injected via constructor
  private
    function GetFixPunkt: TPoint3D;
    function GetStrokeWidthS: Integer;
  protected
    procedure UpdateZugProps;
    procedure Update2;
  public
{$ifdef WantDisplayList}
    DF: TRggFrame;
    DL: TRggDisplayList;
{$endif}
    WantFixPunkt: Boolean;
    WantRumpf: Boolean;
    WantSaling: Boolean;
    WantController: Boolean;
    WantWante: Boolean;
    WantMast: Boolean;
    WantVorstag: Boolean;
    WantAchsen: Boolean;

    WantRenderE: Boolean;
    WantRenderF: Boolean;
//    WantRenderH: Boolean;
    WantRenderP: Boolean;
    WantRenderS: Boolean;

    constructor Create(AZug3D: TZug3DBase);
    destructor Destroy; override;

    procedure Update; override;
{$ifdef WantDisplayList}
    procedure UpdateDisplayList;
{$endif}
    procedure DrawToCanvas(g: TCanvas); override;

    procedure SetChecked(fa: Integer; Value: Boolean);
    function GetChecked(fa: Integer): Boolean;
    procedure GetPlotList(ML: TStrings); override;
    property FixPunkt: TPoint3D read GetFixPunkt;

    property WantRenderH: Boolean read WantRumpf write WantRumpf;
    property StrokeWidthS: Integer read GetStrokeWidthS;
  end;

implementation

{ TRggGraph }

constructor TRggGraph.Create;
begin
  RaumGraphData := TRaumGraphData.Create;
  RaumGraphProps := TRaumGraphProps.Create;
  FColor := clGray;
  FColored := True;
end;

destructor TRggGraph.Destroy;
begin
  RaumGraphData.Free;
  RaumGraphProps.Free;
  inherited;
end;

procedure TRggGraph.SetColor(const Value: TColor);
begin
  FColor := Value;
  RaumGraphProps.Color := Value;
end;

procedure TRggGraph.SetColored(const Value: Boolean);
begin
  FColored := Value;
  RaumGraphProps.Coloriert := FColored;
end;

procedure TRggGraph.SetFixPoint(const Value: TRiggPoint);
begin
  Transformer.FixPoint := Value;
  Updated := False;
end;

procedure TRggGraph.SetZoom(Value: single);
begin
  Transformer.Zoom := Value;
  Updated := False;
  KoppelKurveNeedFill := True;
end;

procedure TRggGraph.Update;
begin
  //if GrafikOK then ...
  //virtual
end;

procedure TRggGraph.DrawToCanvas(Canvas: TCanvas);
begin
  //if GrafikOK then ...
  //virtual
end;

function TRggGraph.GetFixPoint: TRiggPoint;
begin
  result := Transformer.FixPoint;
end;

procedure TRggGraph.GetPlotList(ML: TStrings);
begin
  //if GrafikOK then ...
  //virtual
end;

function TRggGraph.GetZoom: single;
begin
  result := Transformer.Zoom;
end;

{ TBootGraph }

constructor TBootGraph.Create;
begin
  inherited Create;
  FSalingTyp := stFest;
  FControllerTyp := ctOhne;
end;

function TBootGraph.OnGetFixPunkt: TPoint3D;
begin
  result := rP.V[FixPoint];
end;

procedure TBootGraph.SetKoordinaten(const Value: TRiggPoints);
begin
  rP := Value;
  GrafikOK := True;
  Updated := False;
end;

procedure TBootGraph.SetKoppel(const Value: Boolean);
begin
  FKoppel := Value;
  RaumGraphProps.Koppel := True;
end;

procedure TBootGraph.SetKoppelKurve(const Value: TKoordLine);
begin
  FKoppelKurve := Value;
  KoppelKurveNeedFill := True;
end;

procedure TBootGraph.SetControllerTyp(const Value: TControllerTyp);
begin
  FControllerTyp := Value;
  RaumGraphProps.ControllerTyp := Value;
end;

procedure TBootGraph.SetMastLineData(const Value: TLineDataR100; L: single; Beta: single);
var
  temp1, temp2, temp3, temp4, tempL: single;
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
    Kurve[j].X := rP.D0.X - tempL * temp1 + Value[k] * temp2;
    Kurve[j].Y := 0;
    Kurve[j].Z := rP.D0.Z + tempL * temp3 + Value[k] * temp4;
  end;
end;

procedure TBootGraph.SetRiggLED(const Value: Boolean);
begin
  FRiggLED := Value;
end;

procedure TBootGraph.SetMastKurve(const Value: TMastKurve);
begin
  Kurve := Value;
  Updated := False;
end;

procedure TBootGraph.SetSalingTyp(const Value: TSalingTyp);
begin
  FSalingTyp := Value;
  RaumGraphProps.SalingTyp := Value;
end;

procedure TBootGraph.SetBogen(const Value: Boolean);
begin
  FBogen := Value;
  RaumGraphProps.Bogen := Value;
  Updated := False;
end;

procedure TBootGraph.SetViewPoint(const Value: TViewPoint);
begin
  FViewPoint := Value;
  Updated := False;
end;

procedure TBootGraph.SetWanteGestrichelt(const Value: Boolean);
begin
  FGestrichelt := Value;
  RaumGraphProps.Gestrichelt := Value;
end;

function TBootGraph.GetMastKurvePoint(const Index: Integer): TPoint3D;
begin
  if (Index >= 0) and (Index < Length(Kurve)) then
    result := Kurve[Index]
  else
  begin
    result := TPoint3D.Zero;
  end;
end;

function TBootGraph.FindBogenIndexOf(P: TPoint3D): Integer;
var
  i, j: Integer;
  MinIndex: Integer;
  MinAbstand: single;
  a: single;
begin
  j := Length(Kurve);
  MinIndex := j div 2;
  MinAbstand := 1000;
  for i := 0 to j - 1 do
  begin
    a := P.Distance(Kurve[i]);
    if a < MinAbstand then
    begin
      MinAbstand := a;
      MinIndex := i;
    end;
  end;
  result := MinIndex;
end;

function TBootGraph.GetFreshRiggPoints: TRiggPoints;
var
  i: TRiggPoint;
begin
  for i := Low(TRiggPoint) to High(TRiggPoint) do
  begin
    result.V[i] := TPoint3D.Zero;
  end;
end;

procedure TBootGraph.LoadFromIniFile(FileName: string);
var
  IniFile: TIniFile;
  S: string;
  i: TRiggPoint;
  iP: TRiggPoints;
begin
  iP := GetFreshRiggPoints;
  IniFile := TIniFile.Create(FileName);
  S := 'Koordinaten Rumpf';
  try
    with IniFile do
    begin
      iP.A0.X := ReadInteger(S, 'A0x', Round(iP.A0.X));
      iP.A0.Y := ReadInteger(S, 'A0y', Round(iP.A0.Y));
      iP.A0.Z := ReadInteger(S, 'A0z', Round(iP.A0.Z));
      iP.B0.X := ReadInteger(S, 'B0x', Round(iP.B0.X));
      iP.B0.Y := ReadInteger(S, 'B0y', Round(iP.B0.Y));
      iP.B0.Z := ReadInteger(S, 'B0z', Round(iP.B0.Z));
      iP.C0.X := ReadInteger(S, 'C0x', Round(iP.C0.X));
      iP.C0.Y := ReadInteger(S, 'C0y', Round(iP.C0.Y));
      iP.C0.Z := ReadInteger(S, 'C0z', Round(iP.C0.Z));
      iP.D0.X := ReadInteger(S, 'D0x', Round(iP.D0.X));
      iP.D0.Y := ReadInteger(S, 'D0y', Round(iP.D0.Y));
      iP.D0.Z := ReadInteger(S, 'D0z', Round(iP.D0.Z));
      iP.E0.X := ReadInteger(S, 'E0x', Round(iP.E0.X));
      iP.E0.Y := ReadInteger(S, 'E0y', Round(iP.E0.Y));
      iP.E0.Z := ReadInteger(S, 'E0z', Round(iP.E0.Z));
      iP.F0.X := ReadInteger(S, 'F0x', Round(iP.F0.X));
      iP.F0.Y := ReadInteger(S, 'F0y', Round(iP.F0.Y));
      iP.F0.Z := ReadInteger(S, 'F0z', Round(iP.F0.Z));

      S := 'Koordinaten Rigg';
      iP.A.X := ReadInteger(S, 'Ax', Round(iP.A.X));
      iP.A.Y := ReadInteger(S, 'Ay', Round(iP.A.Y));
      iP.A.Z := ReadInteger(S, 'Az', Round(iP.A.Z));
      iP.B.X := ReadInteger(S, 'Bx', Round(iP.B.X));
      iP.B.Y := ReadInteger(S, 'By', Round(iP.B.Y));
      iP.B.Z := ReadInteger(S, 'Bz', Round(iP.B.Z));
      iP.C.X := ReadInteger(S, 'Cx', Round(iP.C.X));
      iP.C.Y := ReadInteger(S, 'Cy', Round(iP.C.Y));
      iP.C.Z := ReadInteger(S, 'Cz', Round(iP.C.Z));
      iP.D.X := ReadInteger(S, 'Dx', Round(iP.D.X));
      iP.D.Y := ReadInteger(S, 'Dy', Round(iP.D.Y));
      iP.D.Z := ReadInteger(S, 'Dz', Round(iP.D.Z));
      iP.E.X := ReadInteger(S, 'Ex', Round(iP.E.X));
      iP.E.Y := ReadInteger(S, 'Ey', Round(iP.E.Y));
      iP.E.Z := ReadInteger(S, 'Ez', Round(iP.E.Z));
      iP.F.X := ReadInteger(S, 'Fx', Round(iP.F.X));
      iP.F.Y := ReadInteger(S, 'Fy', Round(iP.F.Y));
      iP.F.Z := ReadInteger(S, 'Fz', Round(iP.F.Z));
    end;
    for i := ooA0 to ooF0 do
    begin
      rP.V[i] := iP.V[i];
    end;
    for i := ooA to ooF do
    begin
      rP.V[i] := iP.V[i];
    end;
    GrafikOK := True;
    Updated := False;
  finally
    IniFile.Free;
  end;
end;

{ TRaumGraph }

constructor TRaumGraph.Create(AZug3D: TZug3DBase);
begin
  inherited Create;

  WantFixPunkt := True;
  WantRumpf := True;
  WantSaling := True;
  WantController := False;
  WantWante := True;
  WantMast := True;
  WantVorstag := True;
  WantAchsen := False;

  WantRenderF := True;
  WantRenderP := True;

  Zug3D := AZug3D;
  Zug3D.Data := RaumGraphData;
  Zug3D.Props := RaumGraphProps;

{$ifdef WantDisplayList}
  DF := TRggFrame.Create;
  DL := TRggDisplayList.Create;
  DL.DF := DF;
{$endif}

  AchseN.X := 0;
  AchseN.Y := 0;
  AchseN.Z := 0;

  AchseX.X := 1;
  AchseX.Y := 0;
  AchseX.Z := 0;

  AchseY.X := 0;
  AchseY.Y := 1;
  AchseY.Z := 0;

  AchseZ.X := 0;
  AchseZ.Y := 0;
  AchseZ.Z := 1;

  AchseX := AchseX * 1000;
  AchseY := AchseY * 1000;
  AchseZ := AchseZ * 1000;
end;

destructor TRaumGraph.Destroy;
begin
  Zug3D.Free;
{$ifdef WantDisplayList}
  DL.Free;
  DF.Free;
{$endif}
  inherited;
end;

procedure TRaumGraph.Update;
begin
  Update2;
  UpdateZugProps;
  Zug3D.FillZug; // needs updated Props (BogenIndex)
  Updated := True;
end;

procedure TRaumGraph.Update2;
var
  i: TRiggPoint;
  j: Integer;
  RPT: TRiggPoints;
  MKT: array [0 .. BogenMax] of TPoint3D;
  KKT: TKoordLine;
begin
  { Graph drehen }
  if Assigned(Transformer) then
  begin
    for i := Low(TRiggPoint) to High(TRiggPoint) do
      RPT.V[i] := Transformer.TransformPoint(rP.V[i]);
    for j := 0 to BogenMax do
      MKT[j] := Transformer.TransformPoint(Kurve[j]);

    if Koppel then
    for j := 0 to 100 do
      KKT[j] := Transformer.TransformPoint(KoppelKurve[j]);
  end;

{$ifdef WantDisplayList}
  DF.Koordinaten := RPT;
{$endif}

  AchseNT := Transformer.TransformPoint(AchseN);
  AchseXT := Transformer.TransformPoint(AchseX);
  AchseYT := Transformer.TransformPoint(AchseY);
  AchseZT := Transformer.TransformPoint(AchseZ);

  A0 := RPT.A0;
  B0 := RPT.B0;
  C0 := RPT.C0;
  D0 := RPT.D0;
  E0 := RPT.E0;
  F0 := RPT.F0;
  P0 := RPT.P0;

  A := RPT.A;
  B := RPT.B;
  C := RPT.C;
  D := RPT.D;
  E := RPT.E;
  F := RPT.F;
  P := RPT.P;

  M := RPT.M;

  { Es wurde nicht nur rotiert,
    sondern bereits auch verschoben und skaliert }

  with RaumGraphData do
  begin
    xA0 := Round(RPT.A0.X);
    yA0 := Round(RPT.A0.Z);
    xB0 := Round(RPT.B0.X);
    yB0 := Round(RPT.B0.Z);
    xC0 := Round(RPT.C0.X);
    yC0 := Round(RPT.C0.Z);
    xD0 := Round(RPT.D0.X);
    yD0 := Round(RPT.D0.Z);
    xE0 := Round(RPT.E0.X);
    yE0 := Round(RPT.E0.Z);
    xF0 := Round(RPT.F0.X);
    yF0 := Round(RPT.F0.Z);

    xA := Round(RPT.A.X);
    yA := Round(RPT.A.Z);
    xB := Round(RPT.B.X);
    yB := Round(RPT.B.Z);
    xC := Round(RPT.C.X);
    yC := Round(RPT.C.Z);
    xD := Round(RPT.D.X);
    yD := Round(RPT.D.Z);
    xE := Round(RPT.E.X);
    yE := Round(RPT.E.Z);
    xF := Round(RPT.F.X);
    yF := Round(RPT.F.Z);

    xP0 := Round(RPT.P0.X);
    yP0 := Round(RPT.P0.Z);
    xP := Round(RPT.P.X);
    yP := Round(RPT.P.Z);
    xM := Round(RPT.M.X);
    yM := Round(RPT.M.Z);
    xN := Round(AchseNT.X);
    yN := Round(AchseNT.Z);

    xX := Round(AchseXT.X);
    yX := Round(AchseXT.Z);
    xY := Round(AchseYT.X);
    yY := Round(AchseYT.Z);
    xZ := Round(AchseZT.X);
    yZ := Round(AchseZT.Z);
  end;

  { MastKurve }
  for j := 0 to BogenMax do
  begin
    Zug3D.ZugMastKurve[j].x := Round(MKT[j].X);
    Zug3D.ZugMastKurve[j].y := -Round(MKT[j].Z);
  end;

  { Koppelkurve }
  if Koppel then
  begin
    for j := 0 to 100 do
    begin
      Zug3D.ZugKoppelKurve[j].X := Round(KKT[j].X);
      Zug3D.ZugKoppelKurve[j].Y := -Round(KKT[j].Z);
    end;
  end;
end;

procedure TRaumGraph.DrawToCanvas(g: TCanvas);
begin
  if not GrafikOK then
    Exit;

  if not Updated then
    Update;

  Zug3D.DrawToCanvas(g);
end;

function TRaumGraph.GetFixPunkt: TPoint3D;
begin
  result := Transformer.TransformedFixPunkt;
end;

procedure TRaumGraph.GetPlotList(ML: TStrings);
begin
  if not GrafikOK then
    Exit;
  if not Updated then
      Update;

  Zug3D.GetPlotList(ML);
end;

function TRaumGraph.GetStrokeWidthS: Integer;
begin
  if WantRenderS then
    result := 5
  else
    result := 2;
end;

procedure TRaumGraph.UpdateZugProps;
var
  cr: TRaumGraphProps;
begin
  BogenIndexD := FindBogenIndexOf(rP.D);

  cr := RaumGraphProps;

  cr.BogenIndexD := BogenIndexD;
  cr.Bogen := Bogen;

  cr.Koppel := Koppel;
  cr.Gestrichelt := WanteGestrichelt;

  cr.SalingTyp := SalingTyp;
  cr.ControllerTyp := ControllerTyp;

  cr.Coloriert := Coloriert;
  cr.Color := Color;

  cr.RiggLED := RiggLED;
end;

{$ifdef WantDisplayList}
procedure TRaumGraph.UpdateDisplayList;
var
  DI: TDisplayItem;
begin
  DL.Clear;
  DI := DL.DI;

  with Zug3D do
  begin

    if WantFixpunkt then
    begin
      DI.StrokeColor := clYellow;
      DI.StrokeWidth := 1;
      DL.Ellipse('Fixpunkt', deFixPunkt, FixPunkt, FixPunkt, Point(0, 0), TKR);
    end;

    { Rumpf }
    if WantRumpf then
    begin
      DI.StrokeColor := clAqua;
      DI.StrokeWidth := 3;
      DL.Line('A0-B0', deA0B0, A0, B0, ZugRumpf[0], ZugRumpf[1], TRggColors.Blue);
      DL.Line('B0-C0', deB0C0, B0, C0, ZugRumpf[1], ZugRumpf[2], TRggColors.DodgerBlue);
      DL.Line('C0-A0', deA0C0, C0, A0, ZugRumpf[2], ZugRumpf[0], TRggColors.Cornflowerblue);

      DL.Line('A0-D0', deA0D0, A0, D0, ZugRumpf[0], ZugRumpf[4], clGreen);
      DL.Line('B0-D0', deB0D0, B0, D0, ZugRumpf[1], ZugRumpf[4], clRed);
      DL.Line('C0-D0', deC0D0, C0, D0, ZugRumpf[2], ZugRumpf[4], clYellow);
    end;

    { Mast }
    if WantMast then
    begin
      DI.StrokeColor := TRggColors.Cornflowerblue;
      DI.StrokeWidth := 5;
      if Props.Bogen then
      begin
        DL.PolyLine('D0-D', deD0D, D0, D, ZugMastKurveD0D, TRggColors.Cornflowerblue);
        DL.PolyLine('D-C', deCD, D, C, ZugMastKurveDC, TRggColors.Plum);
        DL.Line('C-F', deCF, C, F, ZugMast[2], ZugMast[3], TRggColors.Navy);
      end
      else
      begin
        DL.Line('D0-D', deD0D, D0, D, ZugMast[0], ZugMast[1], TRggColors.Cornflowerblue);
        DL.Line('D-C', deCD, D, C, ZugMast[1], ZugMast[2], TRggColors.Lime);
        DL.Line('C-F', deCF, C, F, ZugMast[2], ZugMast[3], TRggColors.Navy);
      end;
    end;

    { Wanten }
    if WantWante then
    begin
      { Wante Stb }
      DI.StrokeColor := clGreen;
      DI.StrokeWidth := StrokeWidthS;
      DL.Line('A0-A', deA0A, A0, A, ZugWanteStb[0], ZugWanteStb[1], clGreen);
      DL.Line('A-C', deAC, A, C, ZugWanteStb[1], ZugWanteStb[2], clLime);

      { Wante Bb }
      DI.StrokeColor := clRed;
      DI.StrokeWidth := StrokeWidthS;
      DL.Line('B0-B', deB0B, B0, B, ZugWanteBb[0], ZugWanteBb[1], clRed);
      DL.Line('B-C', deBC, B, C, ZugWanteBb[1], ZugWanteBb[2], clLime);
    end;

    { Saling }
    if WantSaling then
    begin
      DI.StrokeColor := TRggColors.Chartreuse;
      DI.StrokeWidth := 6;
      if Props.SalingTyp = stFest then
      begin
        DL.Line('A-D', deAD, A, D, ZugSalingFS[0], ZugSalingFS[1], clLime);
        DL.Line('B-D', deBD, B, D, ZugSalingFS[2], ZugSalingFS[1], TRggColors.Chartreuse);
        DL.Line('A-B', deAB, A, B, ZugSalingFS[0], ZugSalingFS[2], clTeal);
      end;
      if Props.SalingTyp = stDrehbar then
      begin
        DI.StrokeColor := TRggColors.Chartreuse;
        DI.StrokeWidth := 2;
        DL.Line('A-D', deAD, A, D, ZugSalingDS[0], ZugSalingDS[1], TRggColors.Chartreuse);
        DL.Line('B-D', deBD, B, D, ZugSalingDS[2], ZugSalingDS[1], TRggColors.Chartreuse);
      end;
    end;

    { Controller }
    if WantController then
    begin
      if Props.ControllerTyp <> ctOhne then
      begin
        DI.StrokeColor := TRggColors.Orchid;
        DI.StrokeWidth := 4;
        DL.Line('E0-E', deE0E, E0, E, ZugController[0], ZugController[1], clTeal);
      end;
    end;

    { Vorstag }
    if WantVorstag then
    begin
      DI.StrokeColor := clYellow;
      DI.StrokeWidth := StrokeWidthS;
      DL.Line('C0-C', deC0C, C0, C, ZugVorstag[0], ZugVorstag[1], clYellow);
    end;

    { Achsen }
    if WantAchsen then
    begin
      DI.StrokeWidth := 1;
      DI.StrokeColor := clFuchsia;
      DL.Line('N-X', deNX, AchseNT, AchseXT, ZugAchsen[0], ZugAchsen[1], clRed);
      DI.StrokeColor := clLime;
      DL.Line('N-Y', deNY, AchseNT, AchseYT, ZugAchsen[0], ZugAchsen[2], clGreen);
      DI.StrokeColor := clAqua;
      DL.Line('N-Z', deNZ, AchseNT, AchseZT, ZugAchsen[0], ZugAchsen[3], clBlue);
    end;

    if WantRenderF then
    begin
      DI.StrokeColor := TRggColors.Goldenrod;
      DI.StrokeWidth := 1;
      DL.Line('F-M', deMastFall, F, M, ZugMastfall[0], ZugMastfall[1], TRggColors.Goldenrod);
      DI.StrokeWidth := 4;
      DL.Line('M-F0', deMastFall, M, F0, ZugMastfall[1], ZugMastfall[2], clYellow);
    end;

    if WantRenderP then
    begin
      DI.StrokeColor := clSilver;
      DI.StrokeWidth := 1;
      DL.Line('N-D0', deHullFrame, N, D0, ZugRP[0], ZugRP[1], clFuchsia);
      DL.Line('D0-P0', deHullFrame, D0, P0, ZugRP[1], ZugRP[2], clLime);
      DL.Line('P0-F0', deHullFrame, P0, F0, ZugRP[2], ZugRP[3], clAqua);
      DL.Line('F0-N', deHullFrame, F0, N, ZugRP[3], ZugRP[0], clSilver);
    end;

  end;

  DF.WantController := WantController;
  DF.WantAchsen := WantAchsen;
  DF.Sort;
end;
{$endif}

function TRaumGraph.GetChecked(fa: Integer): Boolean;
begin
  case fa of
    faToggleSegmentF: result := WantFixPunkt;
    faToggleSegmentR: result := WantRumpf;
    faToggleSegmentS: result := WantSaling;
    faToggleSegmentM: result := WantMast;
    faToggleSegmentV: result := WantVorstag;
    faToggleSegmentW: result := WantWante;
    faToggleSegmentC: result := WantController;
    faToggleSegmentA: result := WantAchsen;

    faWantRenderE: result := WantRenderE;
    faWantRenderF: result := WantRenderF;
    faWantRenderH: result := WantRenderH;
    faWantRenderP: result := WantRenderP;
    faWantRenderS: result := WantRenderS;

    faRggBogen: result := Bogen;
    faRggKoppel: result := Koppel;
    else
      result := False;
  end;
end;

procedure TRaumGraph.SetChecked(fa: Integer; Value: Boolean);
begin
  case fa of
    faToggleSegmentF: WantFixPunkt := Value;
    faToggleSegmentR: WantRumpf := Value;
    faToggleSegmentS: WantSaling := Value;
    faToggleSegmentM: WantMast := Value;
    faToggleSegmentV: WantVorstag := Value;
    faToggleSegmentW: WantWante := Value;
    faToggleSegmentC: WantController := Value;
    faToggleSegmentA: WantAchsen := Value;

    faWantRenderE:WantRenderE := Value;
    faWantRenderF: WantRenderF := Value;
    faWantRenderH: WantRenderH := Value;
    faWantRenderP: WantRenderP := Value;
    faWantRenderS: WantRenderS := Value;
  end;
end;

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

{ TZug3D }

procedure TZug3D.FillZug;
begin
  with Data do
  begin
    { ZugMastfall }
    ZugMastfall[0].x := xF;
    ZugMastfall[0].y := -yF;
    ZugMastfall[1].x := xM;
    ZugMastfall[1].y := -yM;
    ZugMastfall[2].x := xF0;
    ZugMastfall[2].y := -yF0;

    { ZugRP }
    ZugRP[0].x := xN;
    ZugRP[0].y := -yN;
    ZugRP[1].x := xD0;
    ZugRP[1].y := -yD0;
    ZugRP[2].x := xP0;
    ZugRP[2].y := -yP0;
    ZugRP[3].x := xF0;
    ZugRP[3].y := -yF0;

    { Achsen }
    ZugAchsen[0].x := xN;
    ZugAchsen[0].y := -yN;
    ZugAchsen[1].x := xX;
    ZugAchsen[1].y := -yX;
    ZugAchsen[2].x := xY;
    ZugAchsen[2].y := -yY;
    ZugAchsen[3].x := xZ;
    ZugAchsen[3].y := -yZ;

    { Rumpf }
    ZugRumpf[0].x := xA0;
    ZugRumpf[0].y := -yA0;
    ZugRumpf[1].x := xB0;
    ZugRumpf[1].y := -yB0;
    ZugRumpf[2].x := xC0;
    ZugRumpf[2].y := -yC0;
    ZugRumpf[3].x := xA0;
    ZugRumpf[3].y := -yA0;

    ZugRumpf[4].x := xD0;
    ZugRumpf[4].y := -yD0;
    ZugRumpf[5].x := xB0;
    ZugRumpf[5].y := -yB0;
    ZugRumpf[6].x := xC0;
    ZugRumpf[6].y := -yC0;
    ZugRumpf[7].x := xD0;
    ZugRumpf[7].y := -yD0;

    { Mast }
    ZugMast[0].x := xD0;
    ZugMast[0].y := -yD0;
    ZugMast[1].x := xD;
    ZugMast[1].y := -yD;
    ZugMast[2].x := xC;
    ZugMast[2].y := -yC;
    ZugMast[3].x := xF;
    ZugMast[3].y := -yF;

    { WanteBb }
    ZugWanteBb[0].x := xB0;
    ZugWanteBb[0].y := -yB0;
    ZugWanteBb[1].x := xB;
    ZugWanteBb[1].y := -yB;
    ZugWanteBb[2].x := xC;
    ZugWanteBb[2].y := -yC;

    { WanteStb }
    ZugWanteStb[0].x := xA0;
    ZugWanteStb[0].y := -yA0;
    ZugWanteStb[1].x := xA;
    ZugWanteStb[1].y := -yA;
    ZugWanteStb[2].x := xC;
    ZugWanteStb[2].y := -yC;

    { SalingFS }
    ZugSalingFS[0].x := xA;
    ZugSalingFS[0].y := -yA;
    ZugSalingFS[1].x := xD;
    ZugSalingFS[1].y := -yD;
    ZugSalingFS[2].x := xB;
    ZugSalingFS[2].y := -yB;
    ZugSalingFS[3].x := xA;
    ZugSalingFS[3].y := -yA;

    { SalingDS }
    ZugSalingDS[0].x := xA;
    ZugSalingDS[0].y := -yA;
    ZugSalingDS[1].x := xD;
    ZugSalingDS[1].y := -yD;
    ZugSalingDS[2].x := xB;
    ZugSalingDS[2].y := -yB;

    { Controller }
    ZugController[0].x := xE0;
    ZugController[0].y := -yE0;
    ZugController[1].x := xE;
    ZugController[1].y := -yE;

    { Vorstag }
    ZugVorstag[0].x := xC0;
    ZugVorstag[0].y := -yC0;
    ZugVorstag[1].x := xC;
    ZugVorstag[1].y := -yC;

    { MastKurve }
    ZugMastKurve[BogenMax + 1].x := xF;
    ZugMastKurve[BogenMax + 1].y := -yF;
  end;

  ZugMastKurveD0D := Copy(ZugMastKurve, 0, Props.BogenIndexD + 1);

  ZugMastKurveDC := Copy(
    ZugMastKurve, // string or dynamic array
    Props.BogenIndexD, // start index
    Length(ZugMastKurve) - (Props.BogenIndexD + 1) // count of elements
  );
end;

procedure TZug3D.DrawToCanvas(g: TCanvas);
begin
  with g do
  begin
    Pen.Width := 1;

    { FixPunkt }
    if Props.RiggLED then
      Pen.Color := clLime
    else
      Pen.Color := clYellow;
    Ellipse(-TKR, -TKR, TKR, TKR);

    Pen.Color := Props.Color;

    { Koppelkurve }
    if Props.Koppel then
    begin
      Pen.Color := RggColors.clKoppelKurve;
      PolyLine(ZugKoppelkurve);
    end;

    { Rumpf }
    if Props.Coloriert then
      Pen.Color := RggColors.clRumpf;
    PolyLine(ZugRumpf);

    { Saling }
    if Props.Coloriert then
      Pen.Color := RggColors.clSaling;
    if Props.SalingTyp = stFest then
      PolyLine(ZugSalingFS)
    else if Props.SalingTyp = stDrehbar then
      PolyLine(ZugSalingDS);

    { Mast }
    if Props.Coloriert and Props.Bogen then
    begin
      Pen.Color := RggColors.clMast;
      PolyLine(ZugMastKurve);
      Pen.Color := clNavy;
      MoveTo(ZugMast[2].X, ZugMast[2].Y);
      LineTo(ZugMast[3].X, ZugMast[3].Y);
    end
    else if Props.Coloriert then
    begin
      Pen.Color := RggColors.clMast;
      PolyLine(ZugMast);
    end
    else
    begin
      Pen.Color := Props.Color;
      PolyLine(ZugMast);
    end;

    { Controller }
    if Props.ControllerTyp <> ctOhne then
    begin
      if Props.Coloriert then
        Pen.Color := RggColors.clController;
      PolyLine(ZugController);
    end;

    { Wante Bb }
    if Props.Coloriert then
    begin
      if Props.Gestrichelt then
        Pen.Color := TRggColors.Antiquewhite
      else
        Pen.Color := clRed;
    end
    else
      Pen.Color := Props.Color;
    PolyLine(ZugWanteBb);

    { Wante Stb }
    if Props.Coloriert then
    begin
      if Props.Gestrichelt then
        Pen.Color := TRggColors.Antiquewhite
      else
        Pen.Color := clGreen;
    end
    else
      Pen.Color := Props.Color;
    PolyLine(ZugWanteStb);

    { Vorstag }
    if Props.Coloriert then
      Pen.Color := RggColors.clVorstag;
    PolyLine(ZugVorstag);
  end;
end;

procedure TZug3D.GetPlotList(ML: TStrings);
  procedure Plot(L: array of TPoint);
  var
    s: string;
    i: Integer;
  begin
    with ML do
    begin
      s := Format('PU %d %d;', [L[0].x, L[0].y]);
      Add(s);
      for i := 1 to High(L) do
      begin
        s := Format('PD %d %d;', [L[i].x, L[i].y]);
        Add(s);
      end;
    end;
  end;

begin
  with ML do
  begin
    { Rumpf }
    Add('SP 1;');
    Plot(ZugRumpf);
    { Saling }
    if (Props.SalingTyp = stFest) or (Props.SalingTyp = stDrehbar) then
    begin
      Add('SP 2;');
      if Props.SalingTyp = stFest then
        Plot(ZugSalingFS)
      else if Props.SalingTyp = stDrehbar then
        Plot(ZugSalingDS);
    end;
    { Mast }
    Add('SP 3;');
    Plot(ZugMast);
    Add('SP 4;');
    Plot(ZugMastKurve);
    { Controller }
    Add('SP 5;');
    if Props.ControllerTyp <> ctOhne then
      Plot(ZugController);
    { Wanten }
    Add('SP 6;');
    Plot(ZugWanteStb);
    Add('SP 7;');
    Plot(ZugWanteBb);
    { Vorstag }
    Add('SP 8;');
    Plot(ZugVorstag);
  end;
end;

end.
