unit BootGraph;

interface

uses
  IniFiles,
  RggTypes,
  RggGraph;

type
  TBootGraph = class(TRggGraph)
  private
    FFixName: TRiggPoints;
    FSalingTyp: TSalingTyp;
    FControllerTyp: TControllerTyp;
    procedure SetKoord(Value: TRealRiggPoints);
    procedure SetFixName(Value: TRiggPoints);
  protected
    rP: TRealRiggPoints;
    Kurve: array [0 .. BogenMax] of TRealPoint;
  public
    constructor Create; override;
    procedure LoadFromIniFile(FileName: string);
    procedure SetMastKurve(f: TLineDataR100; L, beta: real);

    property FixName: TRiggPoints read FFixName write SetFixName;
    property SalingTyp: TSalingTyp read FSalingTyp write FSalingTyp;
    property ControllerTyp
      : TControllerTyp read FControllerTyp write FControllerTyp;
    property Koordinaten: TRealRiggPoints read rP write SetKoord;
  end;

implementation

constructor TBootGraph.Create;
begin
  inherited Create;
  FFixName := ooD0;
  FSalingTyp := stFest;
  FControllerTyp := ctDruck;
end;

procedure TBootGraph.SetKoord(Value: TRealRiggPoints);
begin
  rP := Value;
  //Fixpunkt wird in TBootGraph nicht benötigt, aber er wird eventuell
  //herauskopiert - z.Bsp. nach HullGraph - und muß deshalb aktuell sein.
  FixPunkt := rP[FixName];
  GrafikOK := True;
  Updated := False;
end;

procedure TBootGraph.SetFixName(Value: TRiggPoints);
begin
  FFixName := Value;
  FixPunkt := rP[Value]; //--> Updated := False;
end;

procedure TBootGraph.SetMastKurve(f: TLineDataR100; L, beta: real);
var
  temp1, temp2, temp3, temp4, tempL: real;
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
    Kurve[j, x] := rP[ooD0, x] - tempL * temp1 + f[k] * temp2;
    Kurve[j, y] := 0;
    Kurve[j, z] := rP[ooD0, z] + tempL * temp3 + f[k] * temp4;
  end;
end;

procedure TBootGraph.LoadFromIniFile(FileName: string);
var
  IniFile: TIniFile;
  S: string;
  i: TRiggPoints;
  iP: TIntRiggPoints;
begin
  IniFile := TIniFile.Create(FileName);
  S := 'Koordinaten Rumpf';
  try
    with IniFile do
    begin
      iP[ooA0, x] := ReadInteger(S, 'A0x', iP[ooA0, x]);
      iP[ooA0, y] := ReadInteger(S, 'A0y', iP[ooA0, y]);
      iP[ooA0, z] := ReadInteger(S, 'A0z', iP[ooA0, z]);
      iP[ooB0, x] := ReadInteger(S, 'B0x', iP[ooB0, x]);
      iP[ooB0, y] := ReadInteger(S, 'B0y', iP[ooB0, y]);
      iP[ooB0, z] := ReadInteger(S, 'B0z', iP[ooB0, z]);
      iP[ooC0, x] := ReadInteger(S, 'C0x', iP[ooC0, x]);
      iP[ooC0, y] := ReadInteger(S, 'C0y', iP[ooC0, y]);
      iP[ooC0, z] := ReadInteger(S, 'C0z', iP[ooC0, z]);
      iP[ooD0, x] := ReadInteger(S, 'D0x', iP[ooD0, x]);
      iP[ooD0, y] := ReadInteger(S, 'D0y', iP[ooD0, y]);
      iP[ooD0, z] := ReadInteger(S, 'D0z', iP[ooD0, z]);
      iP[ooE0, x] := ReadInteger(S, 'E0x', iP[ooE0, x]);
      iP[ooE0, y] := ReadInteger(S, 'E0y', iP[ooE0, y]);
      iP[ooE0, z] := ReadInteger(S, 'E0z', iP[ooE0, z]);
      iP[ooF0, x] := ReadInteger(S, 'F0x', iP[ooF0, x]);
      iP[ooF0, y] := ReadInteger(S, 'F0y', iP[ooF0, y]);
      iP[ooF0, z] := ReadInteger(S, 'F0z', iP[ooF0, z]);

      S := 'Koordinaten Rigg';
      iP[ooA, x] := ReadInteger(S, 'Ax', iP[ooA, x]);
      iP[ooA, y] := ReadInteger(S, 'Ay', iP[ooA, y]);
      iP[ooA, z] := ReadInteger(S, 'Az', iP[ooA, z]);
      iP[ooB, x] := ReadInteger(S, 'Bx', iP[ooB, x]);
      iP[ooB, y] := ReadInteger(S, 'By', iP[ooB, y]);
      iP[ooB, z] := ReadInteger(S, 'Bz', iP[ooB, z]);
      iP[ooC, x] := ReadInteger(S, 'Cx', iP[ooC, x]);
      iP[ooC, y] := ReadInteger(S, 'Cy', iP[ooC, y]);
      iP[ooC, z] := ReadInteger(S, 'Cz', iP[ooC, z]);
      iP[ooD, x] := ReadInteger(S, 'Dx', iP[ooD, x]);
      iP[ooD, y] := ReadInteger(S, 'Dy', iP[ooD, y]);
      iP[ooD, z] := ReadInteger(S, 'Dz', iP[ooD, z]);
      iP[ooE, x] := ReadInteger(S, 'Ex', iP[ooE, x]);
      iP[ooE, y] := ReadInteger(S, 'Ey', iP[ooE, y]);
      iP[ooE, z] := ReadInteger(S, 'Ez', iP[ooE, z]);
      iP[ooF, x] := ReadInteger(S, 'Fx', iP[ooF, x]);
      iP[ooF, y] := ReadInteger(S, 'Fy', iP[ooF, y]);
      iP[ooF, z] := ReadInteger(S, 'Fz', iP[ooF, z]);
    end;
    for i := ooA0 to ooF0 do
    begin
      rP[i, x] := iP[i, x];
      rP[i, y] := iP[i, y];
      rP[i, z] := iP[i, z];
    end;
    for i := ooA to ooF do
    begin
      rP[i, x] := iP[i, x];
      rP[i, y] := iP[i, y];
      rP[i, z] := iP[i, z];
    end;
    GrafikOK := True;
    Updated := False;
  finally
    IniFile.Free;
  end;
end;

end.
