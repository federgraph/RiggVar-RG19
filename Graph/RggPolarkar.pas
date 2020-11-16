unit RggPolarKar;

interface

{$ifdef fpc}
{$mode delphi}
{$endif}

uses
  System.SysUtils,
  System.Math,
  RiggVar.FD.Point,
  RggTypes,
  RggMatrix;

type
  TCalcAngleEvent = procedure(Sender: TObject; var wx, wy, wz: single) of object;

  TPolarKar = class
  private
    FPhi, FTheta, FGamma, FXRot, FYRot, FZRot: single;
    FValid: Boolean;
    FMode: Boolean;
    FOnCalcAngle: TCalcAngleEvent;
    procedure SetPhi(Value: single);
    procedure SetTheta(Value: single);
    procedure SetGamma(Value: single);
    procedure SetXrot(Value: single);
    procedure SetYrot(Value: single);
    procedure SetZrot(Value: single);
    procedure SetRotMode(Value: Boolean);
    procedure SetRotAngle(index: TRotationAngle; Value: single);
    function GetPhi: single;
    function GetTheta: single;
    function GetGamma: single;
    function GetXrot: single;
    function GetYrot: single;
    function GetZrot: single;
    function GetRotAngle(index: TRotationAngle): single;
  protected
    p1, p2: TPoint3D;
    Angle: single;
    tmat: TMatrix4x4;
    procedure GetMat;
    procedure FillMatrix;
    procedure FillMatrixInc;
    function GetMatrix: Matrix4x4;
    procedure SetMatrix(Value: Matrix4x4);
  public
    Mat: TMatrix4x4;
    constructor Create;
    destructor Destroy; override;
    function Rotiere(Punkt: TPoint3D): TPoint3D;
    procedure Reset;
    procedure GetAngle(var wx, wy, wz: single);
    procedure GetAngle1(Sender: TObject; var wx, wy, wz: single);
    procedure GetAngle2(Sender: TObject; var wp, wt, wg: single);
    property DeltaTheta: single read GetTheta write SetTheta;
    property DeltaPhi: single read GetPhi write SetPhi;
    property DeltaGamma: single read GetGamma write SetGamma;
    property XRot: single read GetXrot write SetXrot;
    property YRot: single read GetYrot write SetYrot;
    property ZRot: single read GetZrot write SetZrot;
    property RotAngle[index: TRotationAngle]: single read GetRotAngle write SetRotAngle;
    property Matrix: Matrix4x4 read GetMatrix write SetMatrix;
    property Mode: Boolean read FMode write SetRotMode;
    property OnCalcAngle: TCalcAngleEvent read FOnCalcAngle write FOnCalcAngle;
  end;

implementation

constructor TPolarKar.Create;
begin
  Mat := TMatrix4x4.Create;
  tmat := TMatrix4x4.Create;
  Reset;
end;

destructor TPolarKar.Destroy;
begin
  Mat.Free;
  tmat.Free;
end;

procedure TPolarKar.SetPhi(Value: single);
begin
  FPhi := DegToRad(Value);
  FValid := False;
end;

procedure TPolarKar.SetTheta(Value: single);
begin
  FTheta := Value * pi / 180;
  FValid := False;
end;

procedure TPolarKar.SetGamma(Value: single);
begin
  FGamma := DegToRad(Value);
  FValid := False;
end;

procedure TPolarKar.SetXrot(Value: single);
begin
  FXRot := DegToRad(Value);
  FValid := False;
end;

procedure TPolarKar.SetYrot(Value: single);
begin
  FYRot := DegToRad(Value);
  FValid := False;
end;

procedure TPolarKar.SetZrot(Value: single);
begin
  FZRot := DegToRad(Value);
  FValid := False;
end;

function TPolarKar.GetPhi: single;
begin
  Result := Int(RadToDeg(FPhi));
end;

function TPolarKar.GetTheta: single;
begin
  Result := Int(RadToDeg(FTheta));
end;

function TPolarKar.GetGamma: single;
begin
  Result := Int(RadToDeg(FGamma));
end;

function TPolarKar.GetXrot: single;
begin
  Result := Int(RadToDeg(FXRot));
end;

function TPolarKar.GetYrot: single;
begin
  Result := Int(RadToDeg(FYRot));
end;

function TPolarKar.GetZrot: single;
begin
  Result := Int(RadToDeg(FZRot));
end;

procedure TPolarKar.SetRotAngle(index: TRotationAngle; Value: single);
var
  temp: single;
begin
  temp := DegToRad(Value);
  case index of
    raPhi:
      FPhi := temp;
    raTheta:
      FTheta := temp;
    raGamma:
      FGamma := temp;
    raXrot:
      FXRot := temp;
    raYrot:
      FYRot := temp;
    raZrot:
      FZRot := temp;
  end;
end;

function TPolarKar.GetRotAngle(index: TRotationAngle): single;
var
  temp: single;
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
      temp := FXRot;
    raYrot:
      temp := FYRot;
    raZrot:
      temp := FZRot;
  end;
  Result := Int(RadToDeg(temp));
end;

function TPolarKar.GetMatrix: Matrix4x4;
begin
  if FValid = False then
    GetMat;
  Result := Mat.Mat;
end;

procedure TPolarKar.SetMatrix(Value: Matrix4x4);
begin
  Reset;
  Mat.Mat := Value;
end;

procedure TPolarKar.GetMat;
begin
  if Mode = False then
    FillMatrixInc
  else
    FillMatrix;
end;

procedure TPolarKar.FillMatrixInc;
begin
  tmat.Identity;
  p1 := TPoint3D.Zero;
  if FTheta <> 0 then
  begin
    p2.x := Mat.Mat[1, 2];
    p2.y := Mat.Mat[2, 2];
    p2.z := Mat.Mat[3, 2];
    Angle := FTheta;
    tmat.Rotate(p1, p2, Angle);
    FTheta := 0;
  end;
  if FPhi <> 0 then
  begin
    p2.x := Mat.Mat[1, 1];
    p2.y := Mat.Mat[2, 1];
    p2.z := Mat.Mat[3, 1];
    Angle := -FPhi;
    tmat.Rotate(p1, p2, Angle);
    FPhi := 0;
  end;
  if FGamma <> 0 then
  begin
    p2.x := Mat.Mat[1, 3];
    p2.y := Mat.Mat[2, 3];
    p2.z := Mat.Mat[3, 3];
    Angle := FGamma;
    tmat.Rotate(p1, p2, Angle);
    FGamma := 0;
  end;
  if FZRot <> 0 then
  begin
    p2 := yVec;
    Angle := FZRot;
    tmat.Rotate(p1, p2, Angle);
    FZRot := 0;
  end;
  if FYRot <> 0 then
  begin
    p2 := xVec;
    Angle := FYRot;
    tmat.Rotate(p1, p2, Angle);
    FYRot := 0;
  end;
  if FXRot <> 0 then
  begin
    p2 := zVec;
    Angle := FXRot;
    tmat.Rotate(p1, p2, Angle);
    FXRot := 0;
  end;
  FValid := True;
  Mat.PreMultiply(tmat.Mat);
end;

procedure TPolarKar.FillMatrix;
begin
  { für Absolutmodus }
  Mat.Identity;
  { 1. Rotation um globale y-Achse, gleichzeitig lokale y-Achse }
  p1.x := 0.0;
  p1.y := 0.0;
  p1.z := 0.0;
  p2.x := 0.0;
  p2.y := 1.0;
  p2.z := 0.0;
  Angle := FTheta;
  Mat.Rotate(p1, p2, Angle);
  { 2. Rotation um globale z-Achse }
  p1.x := 0.0;
  p1.y := 0.0;
  p1.z := 0.0;
  p2.x := 0.0;
  p2.y := 0.0;
  p2.z := 1.0;
  Angle := FPhi;
  Mat.Rotate(p1, p2, Angle);
  { 3. Rotation um locale x-Achse }
  p1.x := 0.0;
  p1.y := 0.0;
  p1.z := 0.0;
  p2.x := Mat.Mat[1, 1];
  p2.y := Mat.Mat[2, 1];
  p2.z := Mat.Mat[3, 1];
  Angle := FGamma;
  Mat.Rotate(p1, p2, Angle);
  FValid := True;
end;

function TPolarKar.Rotiere(Punkt: TPoint3D): TPoint3D;
var
  temp: TPoint3D;
begin
  if FValid = False then
    GetMat;
  temp.x := Punkt.X;
  temp.y := Punkt.Y;
  temp.z := Punkt.Z;
  Mat.TransformPoint(temp);
  Result.X := temp.x;
  Result.Y := temp.y;
  Result.Z := temp.z;
end;

procedure TPolarKar.Reset;
begin
  Mat.Identity;
  FPhi := 0;
  FTheta := 0;
  FGamma := 0;
  FXRot := 0;
  FYRot := 0;
  FZRot := 0;
  FValid := True;
end;

procedure TPolarKar.GetAngle(var wx, wy, wz: single);
begin
  wx := 0;
  wy := 0;
  wz := 0;
  if Assigned(OnCalcAngle) then
    OnCalcAngle(Self, wx, wy, wz);
end;

procedure TPolarKar.GetAngle1(Sender: TObject; var wx, wy, wz: single);

  function angle(a, b: TPoint3D): single;
  var
    temp: single;
  begin
    temp := a.DotProduct(b);
    if temp > 1 then
      temp := 1;
    if temp < -1 then
      temp := -1;
    Result := RadToDeg(ArcCos(temp));
  end;

var
  FLocalX, FlocalY, FLocalZ: TPoint3D;
begin
  Mat.GetLocals(FLocalX, FlocalY, FLocalZ);
  wx := angle(FLocalX, xVec);
  wy := angle(FlocalY, yVec);
  wz := angle(FLocalZ, zVec);
end;

procedure TPolarKar.SetRotMode(Value: Boolean);
begin
  if FMode <> Value then
  begin
    FMode := Value;
    FValid := False;
    if FMode then
    begin
      { Absolute }
      GetAngle(FPhi, FTheta, FGamma);
      FXRot := 0;
      FYRot := 0;
      FZRot := 0;
    end;
    if FMode = False then
    begin
      { Incremental }
      FPhi := 0;
      FTheta := 0;
      FGamma := 0;
      FXRot := 0;
      FYRot := 0;
      FZRot := 0;
    end;
  end;
end;

procedure TPolarKar.GetAngle2(Sender: TObject; var wp, wt, wg: single);

  function CheckSinCos(c: Extended): Extended;
  begin
//    Assert(c <= 1, Format('sincos > 1 (%6.5f)', [c]));
//    Assert(c >= -1, Format('sincos < -1 (%6.5f)', [c]));
    if c > 1 then
      c := 1;
    if c < -1 then
      c := -1;
    Result := c;
  end;

var
  tempcos, tempsin: single;
  ux, uy, uz, tempVec, tempY, tempZ: TPoint3D;
  tempmat: TMatrix4x4;
  Theta90: Boolean;
begin
  wp := 0;
  wt := 0;
  wg := 0;

  tempmat := TMatrix4x4.Create;
  try
    tempmat.CopyFrom((Sender as TPolarKar).Mat);
    tempmat.GetLocals(ux, uy, uz);

    { Winkel Theta ermitteln im Bereich -90..90 Grad }
    tempsin := -ux.z;
    //tempcos := Dot(ux,zVec); //nicht verwendet
    wt := arcsin(CheckSinCos(tempsin));
  Theta90 := abs(RadToDeg(tempsin)) > 89.9; //Theta90 := abs(tempsin) > 0.99;

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
      tempY := zVec.CrossProduct(ux).Normalize;
      tempZ := ux.CrossProduct(tempY).Normalize;
      tempcos := uz.DotProduct(tempZ);
      tempsin := -uz.DotProduct(tempY);
      wg := ArcCos(CheckSinCos(tempcos));
      if tempsin < 0 then
        wg := -wg;
    end;

    { Winkel Phi ermitteln im Bereich -180..180 Grad }
    if Theta90 then
    begin
      tempVec := uy.CrossProduct(zVec).Normalize;
      tempcos := tempVec.x;
      tempsin := tempVec.y;
    end
    else
    begin
      tempVec := ux;
      tempVec.z := 0;
      tempVec := tempVec.Normalize;
      tempcos := xVec.DotProduct(tempVec);
      tempsin := yVec.DotProduct(tempVec);
    end;
    wp := ArcCos(CheckSinCos(tempcos));
    if tempsin < 0 then
      wp := -wp;

  wg := RadToDeg(wg);
  wt := RadToDeg(wt);
  wp := RadToDeg(wp);

    wg := Round(wg * 10) / 10;
    wt := Round(wt * 10) / 10;
    wp := Round(wp * 10) / 10;

  finally
    tempmat.Free;
  end;
end;

end.
