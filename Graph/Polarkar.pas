unit Polarkar;

interface

uses
  SysUtils,
  Math,
  Vector3D,
  RggTypes,
  Rggmat01;

type
  TCalcAngleEvent = procedure(Sender: TObject; var wx, wy, wz: real) of object;

  TPolarKar2 = class(TObject)
  private
    FPhi, FTheta, FGamma, Fxrot, Fyrot, Fzrot: real;
    FValid: Boolean;
    FMode: Boolean;
    FOnCalcAngle: TCalcAngleEvent;
    procedure SetPhi(Value: real);
    procedure SetTheta(Value: real);
    procedure SetGamma(Value: real);
    procedure SetXrot(Value: real);
    procedure SetYrot(Value: real);
    procedure SetZrot(Value: real);
    procedure SetRotMode(Value: Boolean);
    procedure SetRotAngle(index: TRotationAngle; Value: real);
    function GetPhi: real;
    function GetTheta: real;
    function GetGamma: real;
    function GetXrot: real;
    function GetYrot: real;
    function GetZrot: real;
    function GetRotAngle(index: TRotationAngle): real;
  protected
    p1, p2: vec3;
    angle: real;
    tmat: TMatrix4x4;
    procedure GetMat;
    procedure FillMatrix;
    procedure FillMatrixInc;
    function GetMatrix: Matrix4x4;
    procedure SetMatrix(Value: Matrix4x4);
  public
    mat: TMatrix4x4;
    constructor Create;
    destructor Destroy; override;
    function Rotiere(Punkt: TRealPoint): TRealPoint;
    procedure Reset;
    procedure GetAngle(var wx, wy, wz: real);
    procedure GetAngle1(Sender: TObject; var wx, wy, wz: real);
    procedure GetAngle2(Sender: TObject; var wp, wt, wg: real);
    property DeltaTheta: real read GetTheta write SetTheta;
    property DeltaPhi: real read GetPhi write SetPhi;
    property DeltaGamma: real read GetGamma write SetGamma;
    property xrot: real read GetXrot write SetXrot;
    property yrot: real read GetYrot write SetYrot;
    property zrot: real read GetZrot write SetZrot;
    property RotAngle[index: TRotationAngle]
      : real read GetRotAngle write SetRotAngle;
    property Matrix: Matrix4x4 read GetMatrix write SetMatrix;
    property Mode: Boolean read FMode write SetRotMode;
    property OnCalcAngle: TCalcAngleEvent read FOnCalcAngle write FOnCalcAngle;
  end;

implementation

constructor TPolarKar2.Create;
begin
  mat := TMatrix4x4.Create;
  tmat := TMatrix4x4.Create;
  Reset;
end;

destructor TPolarKar2.Destroy;
begin
  mat.Free;
  tmat.Free;
end;

procedure TPolarKar2.SetPhi(Value: real);
begin
  FPhi := Value * pi / 180;
  FValid := False;
end;

procedure TPolarKar2.SetTheta(Value: real);
begin
  FTheta := Value * pi / 180;
  FValid := False;
end;

procedure TPolarKar2.SetGamma(Value: real);
begin
  FGamma := Value * pi / 180;
  FValid := False;
end;

procedure TPolarKar2.SetXrot(Value: real);
begin
  Fxrot := Value * pi / 180;
  FValid := False;
end;

procedure TPolarKar2.SetYrot(Value: real);
begin
  Fyrot := Value * pi / 180;
  FValid := False;
end;

procedure TPolarKar2.SetZrot(Value: real);
begin
  Fzrot := Value * pi / 180;
  FValid := False;
end;

function TPolarKar2.GetPhi: real;
begin
  Result := Int(FPhi * 180 / pi);
end;

function TPolarKar2.GetTheta: real;
begin
  Result := Int(FTheta * 180 / pi);
end;

function TPolarKar2.GetGamma: real;
begin
  Result := Int(FGamma * 180 / pi);
end;

function TPolarKar2.GetXrot: real;
begin
  Result := Int(Fxrot * 180 / pi);
end;

function TPolarKar2.GetYrot: real;
begin
  Result := Int(Fyrot * 180 / pi);
end;

function TPolarKar2.GetZrot: real;
begin
  Result := Int(Fzrot * 180 / pi);
end;

procedure TPolarKar2.SetRotAngle(index: TRotationAngle; Value: real);
var
  temp: real;
begin
  temp := Value * pi / 180;
  case index of
    raPhi:
      FPhi := temp;
    raTheta:
      FTheta := temp;
    raGamma:
      FGamma := temp;
    raXrot:
      Fxrot := temp;
    raYrot:
      Fyrot := temp;
    raZrot:
      Fzrot := temp;
  end;
end;

function TPolarKar2.GetRotAngle(index: TRotationAngle): real;
var
  temp: real;
begin
  temp := 0;
  case index of
    raPhi:
      temp := FPhi;
    raTheta:
      temp := FTheta;
    raGamma:
      temp := FGamma;
    raXrot:
      temp := Fxrot;
    raYrot:
      temp := Fyrot;
    raZrot:
      temp := Fzrot;
  end;
  Result := Int(temp * 180 / pi);
end;

function TPolarKar2.GetMatrix: Matrix4x4;
begin
  if FValid = False then
    GetMat;
  Result := mat.mat;
end;

procedure TPolarKar2.SetMatrix(Value: Matrix4x4);
begin
  Reset;
  mat.mat := Value;
end;

procedure TPolarKar2.GetMat;
begin
  if Mode = False then
    FillMatrixInc
  else
    FillMatrix;
end;

procedure TPolarKar2.FillMatrixInc;
begin
  tmat.Identity;
  p1 := NullVec;
  if FTheta <> 0 then
  begin
    p2.x := mat.mat[1, 2];
    p2.y := mat.mat[2, 2];
    p2.z := mat.mat[3, 2];
    angle := FTheta;
    tmat.rotate(p1, p2, angle);
    FTheta := 0;
  end;
  if FPhi <> 0 then
  begin
    p2.x := mat.mat[1, 1];
    p2.y := mat.mat[2, 1];
    p2.z := mat.mat[3, 1];
    angle := -FPhi;
    tmat.rotate(p1, p2, angle);
    FPhi := 0;
  end;
  if FGamma <> 0 then
  begin
    p2.x := mat.mat[1, 3];
    p2.y := mat.mat[2, 3];
    p2.z := mat.mat[3, 3];
    angle := FGamma;
    tmat.rotate(p1, p2, angle);
    FGamma := 0;
  end;
  if Fzrot <> 0 then
  begin
    p2 := yVec;
    angle := Fzrot;
    tmat.rotate(p1, p2, angle);
    Fzrot := 0;
  end;
  if Fyrot <> 0 then
  begin
    p2 := xVec;
    angle := Fyrot;
    tmat.rotate(p1, p2, angle);
    Fyrot := 0;
  end;
  if Fxrot <> 0 then
  begin
    p2 := zVec;
    angle := Fxrot;
    tmat.rotate(p1, p2, angle);
    Fxrot := 0;
  end;
  FValid := True;
  mat.PreMultiply(tmat.mat);
end;

procedure TPolarKar2.FillMatrix; //für Absolutmodus
begin
  mat.Identity;
  { 1. Rotation um globale y-Achse, gleichzeitig lokale y-Achse }
  p1.x := 0.0;
  p1.y := 0.0;
  p1.z := 0.0;
  p2.x := 0.0;
  p2.y := 1.0;
  p2.z := 0.0;
  angle := FTheta;
  mat.rotate(p1, p2, angle);
  { 2. Rotation um globale z-Achse }
  p1.x := 0.0;
  p1.y := 0.0;
  p1.z := 0.0;
  p2.x := 0.0;
  p2.y := 0.0;
  p2.z := 1.0;
  angle := FPhi;
  mat.rotate(p1, p2, angle);
  { 3. Rotation um locale x-Achse }
  p1.x := 0.0;
  p1.y := 0.0;
  p1.z := 0.0;
  p2.x := mat.mat[1, 1];
  p2.y := mat.mat[2, 1];
  p2.z := mat.mat[3, 1];
  angle := FGamma;
  mat.rotate(p1, p2, angle);
  FValid := True;
end;

function TPolarKar2.Rotiere(Punkt: TRealPoint): TRealPoint;
var
  temp: vec3;
begin
  if FValid = False then
    GetMat;
  temp.x := Punkt[x];
  temp.y := Punkt[y];
  temp.z := Punkt[z];
  mat.transformPoint(temp);
  Result[x] := temp.x;
  Result[y] := temp.y;
  Result[z] := temp.z;
end;

procedure TPolarKar2.Reset;
begin
  mat.Identity;
  FPhi := 0;
  FTheta := 0;
  FGamma := 0;
  Fxrot := 0;
  Fyrot := 0;
  Fzrot := 0;
  FValid := True;
end;

procedure TPolarKar2.GetAngle(var wx, wy, wz: real);
begin
  wx := 0;
  wy := 0;
  wz := 0;
  if Assigned(OnCalcAngle) then
    OnCalcAngle(Self, wx, wy, wz);
end;

procedure TPolarKar2.GetAngle1(Sender: TObject; var wx, wy, wz: real);

  function angle(a, b: vec3): real;
  var
    temp: real;
  begin
    temp := dot(a, b);
    if temp > 1 then
      temp := 1;
    if temp < -1 then
      temp := -1;
    Result := ArcCos(temp) * 180 / pi;
  end;

var
  FLocalX, FlocalY, FLocalZ: vec3;
begin
  mat.GetLocals(FLocalX, FlocalY, FLocalZ);
  wx := angle(FLocalX, xVec);
  wy := angle(FlocalY, yVec);
  wz := angle(FLocalZ, zVec);
end;

procedure TPolarKar2.SetRotMode(Value: Boolean);
begin
  if FMode <> Value then
  begin
    FMode := Value;
    FValid := False;
    if FMode then
    begin //AbsolutMode
      GetAngle(FPhi, FTheta, FGamma);
      Fxrot := 0;
      Fyrot := 0;
      Fzrot := 0;
    end;
    if FMode = False then
    begin //IncrementMode
      FPhi := 0;
      FTheta := 0;
      FGamma := 0;
      Fxrot := 0;
      Fyrot := 0;
      Fzrot := 0;
    end;
  end;
end;

procedure TPolarKar2.GetAngle2(Sender: TObject; var wp, wt, wg: real);

  function CheckSinCos(c: Extended): Extended;
  begin
    Assert(c <= 1, Format('sincos > 1 (%6.5f)', [c]));
    Assert(c >= -1, Format('sincos < -1 (%6.5f)', [c]));
    if c > 1 then
      c := 1;
    if c < -1 then
      c := -1;
    Result := c;
  end;

var
  tempcos, tempsin: real;
  ux, uy, uz, tempVec, tempY, tempZ: vec3;
  tempmat: TMatrix4x4;
  Theta90: Boolean;
begin
  wp := 0;
  wt := 0;
  wg := 0;

  tempmat := TMatrix4x4.Create;
  try
    tempmat.copyFrom((Sender as TPolarKar2).mat);
    tempmat.GetLocals(ux, uy, uz);

    { Winkel Theta ermitteln im Bereich -90..90 Grad }
    tempsin := -ux.z;
    //tempcos := Dot(ux,zVec); //nicht verwendet
    wt := arcsin(CheckSinCos(tempsin));
    Theta90 := abs(tempsin * 180 / pi) > 89.9; //Theta90 := abs(tempsin) > 0.99;

    { Winkel Gamma ermitteln im Bereich -180..180 Grad }
    if Theta90 then
    begin
      //Winkel Gamma immer Null setzen, wenn lokale x-Achse senkrecht!
      //tempcos := 1;
      //tempsin := 0;
      wg := 0;
    end
    else
    begin
      tempY := Cross(zVec, ux);
      Normalize(tempY);
      tempZ := Cross(ux, tempY);
      Normalize(tempZ);
      tempcos := dot(uz, tempZ);
      tempsin := -dot(uz, tempY);
      wg := ArcCos(CheckSinCos(tempcos));
      if tempsin < 0 then
        wg := -wg;
    end;

    { Winkel Phi ermitteln im Bereich -180..180 Grad }
    if Theta90 then
    begin
      tempVec := Cross(uy, zVec);
      Normalize(tempVec);
      tempcos := tempVec.x; //tempcos := -uz.x;
      tempsin := tempVec.y; //tempsin := -uz.y;
    end
    else
    begin
      tempVec := ux;
      tempVec.z := 0;
      Normalize(tempVec); //d := Hypot(ux.x,ux.y);
      tempcos := dot(xVec, tempVec); //tempcos := ux.x/d;
      tempsin := dot(yVec, tempVec); //tempsin := ux.y/d;
    end;
    wp := ArcCos(CheckSinCos(tempcos));
    if tempsin < 0 then
      wp := -wp;

    wg := wg * 180 / pi;
    wt := wt * 180 / pi;
    wp := wp * 180 / pi;

    wg := Round(wg * 10) / 10;
    wt := Round(wt * 10) / 10;
    wp := Round(wp * 10) / 10;

  finally
    tempmat.Free;
  end;
end;

end.
