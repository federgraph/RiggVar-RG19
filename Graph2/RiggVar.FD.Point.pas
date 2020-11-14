unit RiggVar.FD.Point;

interface

type
  TEpsilon = record
  const
    Matrix = 1E-5;
    Vector = 1E-4;
    Scale = 1E-4;
    FontSize = 1E-2;
    Position = 1E-3;
    Angle = 1E-4;
  end;

  TVector3DType = array [0..3] of single;
  TPoint3DArray = array [0..2] of single;

  TPoint3D = record
    class function Create(const AX, AY, AZ: single): TPoint3D; overload; static; inline;
    class function Create(const P: TVector3DType): TPoint3D; overload; static; inline;

    class operator Add(const APoint1, APoint2: TPoint3D): TPoint3D;
    class operator Subtract(const APoint1, APoint2: TPoint3D): TPoint3D;
    class operator Equal(const APoint1, APoint2: TPoint3D): Boolean; inline;
    class operator NotEqual(const APoint1, APoint2: TPoint3D): Boolean; inline;
    class operator Negative(const APoint: TPoint3D): TPoint3D;
    class operator Multiply(const APoint1, APoint2: TPoint3D): TPoint3D;
    class operator Multiply(const APoint: TPoint3D; const AFactor: single): TPoint3D; inline;
    class operator Multiply(const AFactor: single; const APoint: TPoint3D): TPoint3D; inline;
    class operator Divide(const APoint: TPoint3D; const AFactor: single): TPoint3D;

    class function Zero: TPoint3D; inline; static;

    class function RotD(const Value: TPoint3D): TPoint3D; static;
    class function RotR(const Value: TPoint3D): TPoint3D; static;

    procedure Offset(const ADelta: TPoint3D); overload; inline;
    procedure Offset(const ADeltaX, ADeltaY, ADeltaZ: single); overload; inline;

    function CrossProduct(const APoint: TPoint3D): TPoint3D;
    function DotProduct(const APoint: TPoint3D): single; inline;
    function EqualsTo(const APoint: TPoint3D; const Epsilon: single = 0): Boolean; inline;

    function Length: single; inline;
    function Normalize: TPoint3D;
    function Distance(const APoint: TPoint3D): single;
    function Rotate(const AAxis: TPoint3D; const AAngle: single): TPoint3D; inline;
    function Reflect(const APoint: TPoint3D): TPoint3D; inline;
    function MidPoint(const APoint: TPoint3D): TPoint3D; inline;
    function AngleCosine(const APoint: TPoint3D): single;

    case Integer of
      0: (V: TPoint3DArray;);
      1: (X: single;
          Y: single;
          Z: single;);
  end;
  PPoint3D = ^TPoint3D;

  tagVECTOR3D = record
    case Integer of
      0: (V: TVector3DType;);
      1: (X: single;
          Y: single;
          Z: single;
          W: single;);
  end;

  TVector3D = record
    class function Create(const AX, AY, AZ: single; const AW: single = 1.0): TVector3D; overload; static; inline;
    class function Create(const APoint: TPoint3D; const AW: single = 1.0): TVector3D; overload; static; inline;

    class operator Add(const AVector1, AVector2: TVector3D): TVector3D;
    class operator Subtract(const AVector1, AVector2: TVector3D): TVector3D;
    class operator Equal(const AVector1, AVector2: TVector3D): Boolean;
    class operator NotEqual(const AVector1, AVector2: TVector3D): Boolean;
    class operator Negative(const AVector: TVector3D): TVector3D;
    class operator Implicit(const APoint: TPoint3D): TVector3D;
    class operator Explicit(const AVector: TVector3D): TPoint3D;
    class operator Multiply(const AVector1, AVector2: TVector3D): TVector3D;
    class operator Multiply(const AVector: TVector3D; const AFactor: single): TVector3D; inline;
    class operator Multiply(const AFactor: single; const AVector: TVector3D): TVector3D; inline;
    class operator Divide(const AVector: TVector3D; const AFactor: single): TVector3D;

    class function Zero: TVector3D; inline; static;

    function EqualsTo(const AVector: TVector3D; const Epsilon: single = 0): Boolean; inline;

    function Length: single;
    function Normalize: TVector3D;
    function Distance(const AVector: TVector3D): single;
    function MidVector(const AVector: TVector3D): TVector3D;

    case Integer of
      0: (V: TVector3DType;);
      1: (X: single;
          Y: single;
          Z: single;
          W: single;);
  end;

  TVector3DArray = array [0..2] of TVector3D;
  TVector3DArrayBase = array[0..2] of tagVECTOR3D;

  TMatrix3DType = array [0..3] of TVector3D;
  TMatrix3DTypeBase = array[0..3] of tagVECTOR3D;

  TMatrix3D = record
  private
    function DetInternal(const a1, a2, a3, b1, b2, b3, c1, c2, c3: single): single; inline;
    function Scale(const AFactor: single): TMatrix3D;
  public
    constructor Create(const AM11, AM12, AM13, AM14, AM21, AM22, AM23, AM24, AM31, AM32, AM33,
      AM34, AM41, AM42, AM43, AM44: single); overload;

    class function CreateScaling(const AScale: TPoint3D): TMatrix3D; static;
    class function CreateTranslation(const ATranslation: TPoint3D): TMatrix3D; static;

    class function CreateRotationX(const AAngle: single): TMatrix3D; static;
    class function CreateRotationY(const AAngle: single): TMatrix3D; static;
    class function CreateRotationZ(const AAngle: single): TMatrix3D; static;

    class function CreateRotation(const AAxis: TPoint3D; const AAngle: single): TMatrix3D; static;
    class function CreateRotationYawPitchRoll(const AYaw, APitch, ARoll: single): TMatrix3D; static;
    class function CreateRotationHeadingPitchBank(const AHeading, APitch, ABank: single): TMatrix3D; static;

    class function CreateFromEulerAngles(heading, attitude, bank: single): TMatrix3D; static;

    class function CreateLookAtRH(const ASource, ATarget, ACeiling: TPoint3D): TMatrix3D; static;
    class function CreateLookAtLH(const ASource, ATarget, ACeiling: TPoint3D): TMatrix3D; static;
    class function CreateLookAtDirRH(const ASource, ADirection, ACeiling: TPoint3D): TMatrix3D; static;
    class function CreateLookAtDirLH(const ASource, ADirection, ACeiling: TPoint3D): TMatrix3D; static;
    class function CreateOrthoLH(const AWidth, AHeight, AZNear, AZFar: single): TMatrix3D; static;
    class function CreateOrthoRH(const AWidth, AHeight, AZNear, AZFar: single): TMatrix3D; static;
    class function CreateOrthoOffCenterLH(const ALeft, ATop, ARight, ABottom, AZNear, AZFar: single): TMatrix3D; static;
    class function CreateOrthoOffCenterRH(const ALeft, ATop, ARight, ABottom, AZNear, AZFar: single): TMatrix3D; static;
    class function CreatePerspectiveFovLH(const AFOV, AAspect, AZNear, AZFar: single; const AHorizontalFOV: Boolean = False): TMatrix3D; static;
    class function CreatePerspectiveFovRH(const AFOV, AAspect, AZNear, AZFar: single; const AHorizontalFOV: Boolean = False): TMatrix3D; static;

    class operator Multiply(const APoint1, APoint2: TMatrix3D): TMatrix3D;
    class operator Multiply(const APoint: TPoint3D; const AMatrix: TMatrix3D): TPoint3D;
    class operator Multiply(const AVector: TVector3D; const AMatrix: TMatrix3D): TVector3D;

    function Transpose: TMatrix3D;
    function Determinant: single;
    function Adjoint: TMatrix3D;
    function Inverse: TMatrix3D;

    function EyePosition: TPoint3D;
    function ToEulerAngles: TPoint3D;

    case Integer of
      0: (M: TMatrix3DType;);
      1: (m11, m12, m13, m14: single;
          m21, m22, m23, m24: single;
          m31, m32, m33, m34: single;
          m41, m42, m43, m44: single);
  end;

  TQuaternion3D = record
    constructor Create(const AAxis: TPoint3D; const AAngle: single); overload;
    constructor Create(const AYaw, APitch, ARoll: single); overload;
    constructor Create(const AMatrix: TMatrix3D); overload;

    class operator Implicit(const AQuaternion: TQuaternion3D): TMatrix3D;
    class operator Multiply(const AQuaternion1, AQuaternion2: TQuaternion3D): TQuaternion3D;

    class function CreateQuaternionFromEulerAngles(yaw, pitch, roll: single): TQuaternion3D; static;

    function Length: single;
    function Normalize: TQuaternion3D;

    function ToEulerAngles: TPoint3D;

    case Integer of
      0: (V: TVector3DType;);
      1: (ImagPart: TPoint3D;
          RealPart: single;);
  end;

const
  Matrix3DIdentity: TMatrix3D = (
      m11: 1; m12: 0; m13: 0; m14: 0;
      m21: 0; m22: 1; m23: 0; m24: 0;
      m31: 0; m32: 0; m33: 1; m34: 0;
      m41: 0; m42: 0; m43: 0; m44: 1;
    );

  Quaternion3DIdentity: TQuaternion3D = (
    ImagPart: (X: 0; Y: 0; Z: 0);
    RealPart: 1;
  );

  NullVector3D: TVector3D = (X: 0; Y: 0; Z: 0; W: 1);
  NullPoint3D: TPoint3D = (X: 0; Y: 0; Z: 0);

function Vector3D(const X, Y, Z: single; const W: single = 1.0): TVector3D; overload;
function Vector3D(const P: TPoint3D; const W: single = 1.0): TVector3D; overload;
function Point3D(const X, Y, Z: single): TPoint3D; overload;

function SameValue(const A, B: single; Epsilon: single): Boolean;

implementation

uses
  Math;

const
  Epsilon: single = 1E-40;
  Epsilon2: single = 1E-30;
  FuzzFactor = 1000;
  singleResolution   = 1E-7 * FuzzFactor;

function SameValue(const A, B: single; Epsilon: single): Boolean;
begin
  if Epsilon = 0 then
    Epsilon := Max(Min(Abs(A), Abs(B)) * singleResolution, singleResolution);
  if A > B then
    Result := (A - B) <= Epsilon
  else
    Result := (B - A) <= Epsilon;
end;

procedure SinCossingle(const Theta: single; out S, C: single);
begin
  S := sin(Theta);
  C := cos(Theta);
end;

function Vector3D(const X, Y, Z: single; const W: single = 1.0): TVector3D; overload;
begin
  result := TVector3D.Create(X, Y , Z, W);
end;

function Vector3D(const P: TPoint3D; const W: single = 1.0): TVector3D; overload;
begin
  result := TVector3D.Create(P, W);
end;

function Point3D(const X, Y, Z: single): TPoint3D; overload;
begin
  result := TPoint3D.Create(X, Y, Z);
end;

{ TPoint3D }

class function TPoint3D.Create(const AX, AY, AZ: single): TPoint3D;
begin
  Result.X := AX;
  Result.Y := AY;
  Result.Z := AZ;
end;

class function TPoint3D.Create(const P: TVector3DType): TPoint3D;
begin
  Result.X := P[0];
  Result.Y := P[1];
  Result.Z := P[2];
end;

class operator TPoint3D.Add(const APoint1, APoint2: TPoint3D): TPoint3D;
begin
  Result.X := APoint1.X + APoint2.X;
  Result.Y := APoint1.Y + APoint2.Y;
  Result.Z := APoint1.Z + APoint2.Z;
end;

class operator TPoint3D.Subtract(const APoint1, APoint2: TPoint3D): TPoint3D;
begin
  Result.X := APoint1.X - APoint2.X;
  Result.Y := APoint1.Y - APoint2.Y;
  Result.Z := APoint1.Z - APoint2.Z;
end;

class operator TPoint3D.Equal(const APoint1, APoint2: TPoint3D): Boolean;
begin
  Result :=
    SameValue(APoint1.X, APoint2.X, TEpsilon.Vector) and
    SameValue(APoint1.Y, APoint2.Y, TEpsilon.Vector) and
    SameValue(APoint1.Z, APoint2.Z, TEpsilon.Vector);
end;

function TPoint3D.EqualsTo(const APoint: TPoint3D; const Epsilon: single): Boolean;
begin
  Result :=
    SameValue(X, APoint.X, Epsilon) and
    SameValue(Y, APoint.Y, Epsilon) and
    SameValue(Z, APoint.Z, Epsilon);
end;

class operator TPoint3D.NotEqual(const APoint1, APoint2: TPoint3D): Boolean;
begin
  Result := not (APoint1 = APoint2);
end;

class operator TPoint3D.Negative(const APoint: TPoint3D): TPoint3D;
begin
  Result.X := - APoint.X;
  Result.Y := - APoint.Y;
  Result.Z := - APoint.Z;
end;

class operator TPoint3D.Multiply(const APoint1, APoint2: TPoint3D): TPoint3D;
begin
  Result.X := APoint1.X * APoint2.X;
  Result.Y := APoint1.Y * APoint2.Y;
  Result.Z := APoint1.Z * APoint2.Z;
end;

class operator TPoint3D.Multiply(const APoint: TPoint3D; const AFactor: single): TPoint3D;
begin
  Result := APoint * TPoint3D.Create(AFactor, AFactor, AFactor);
end;

class operator TPoint3D.Multiply(const AFactor: single; const APoint: TPoint3D): TPoint3D;
begin
  Result := APoint * AFactor;
end;

class operator TPoint3D.Divide(const APoint: TPoint3D; const AFactor: single): TPoint3D;
begin
  if AFactor <> 0 then
    Result := APoint * (1 / AFactor)
  else
    Result := APoint;
end;

procedure TPoint3D.Offset(const ADelta: TPoint3D);
begin
  Self := Self + ADelta;
end;

procedure TPoint3D.Offset(const ADeltaX, ADeltaY, ADeltaZ: single);
begin
  Self.Offset(TPoint3D.Create(ADeltaX, ADeltaY, ADeltaZ));
end;

function TPoint3D.CrossProduct(const APoint: TPoint3D): TPoint3D;
begin
  Result.X := (Self.Y * APoint.Z) - (Self.Z * APoint.Y);
  Result.Y := (Self.Z * APoint.X) - (Self.X * APoint.Z);
  Result.Z := (Self.X * APoint.Y) - (Self.Y * APoint.X);
end;

function TPoint3D.DotProduct(const APoint: TPoint3D): single;
begin
  Result := (Self.X * APoint.X) + (Self.Y * APoint.Y) + (Self.Z * APoint.Z);
end;

function TPoint3D.Length: single;
begin
  Result := Sqrt(Self.DotProduct(Self));
end;

function TPoint3D.Normalize: TPoint3D;
var
  VecLen: single;
begin
  VecLen := Self.Length;

  if VecLen > 0.0 then
    Result := Self / VecLen
  else
    Result := Self;
end;

function TPoint3D.Distance(const APoint: TPoint3D): single;
begin
  Result := (Self - APoint).Length;
end;

function TPoint3D.Rotate(const AAxis: TPoint3D; const AAngle: single): TPoint3D;
begin
  Result := Self * TMatrix3D.CreateRotation(AAxis, AAngle);
end;

function TPoint3D.Reflect(const APoint: TPoint3D): TPoint3D;
begin
  Result := Self + APoint * (-2 * Self.DotProduct(APoint));
end;

function TPoint3D.MidPoint(const APoint: TPoint3D): TPoint3D;
begin
  Result := (Self + APoint) * 0.5;
end;

function TPoint3D.AngleCosine(const APoint: TPoint3D): single;
begin
  Result := Self.Length * APoint.Length;

  if Abs(Result) > Epsilon then
    Result := Self.DotProduct(APoint) / Result
  else
    Result := Self.DotProduct(APoint) / Epsilon;

  Result := Max(Min(Result, 1), -1);
end;

class function TPoint3D.Zero: TPoint3D;
begin
  Result.X := 0;
  Result.Y := 0;
  Result.Z := 0;
end;

{ TVector3D }

class function TVector3D.Create(const AX, AY, AZ, AW: single): TVector3D;
begin
  Result.X := AX;
  Result.Y := AY;
  Result.Z := AZ;
  Result.W := AW;
end;

class function TVector3D.Create(const APoint: TPoint3D; const AW: single): TVector3D;
begin
  Result.X := APoint.X;
  Result.Y := APoint.Y;
  Result.Z := APoint.Z;
  Result.W := AW;
end;

class operator TVector3D.Add(const AVector1, AVector2: TVector3D): TVector3D;
begin
  Result.X := AVector1.X + AVector2.X;
  Result.Y := AVector1.Y + AVector2.Y;
  Result.Z := AVector1.Z + AVector2.Z;
  Result.W := AVector1.W + AVector2.W;
end;

class operator TVector3D.Subtract(const AVector1, AVector2: TVector3D): TVector3D;
begin
  Result.X := AVector1.X - AVector2.X;
  Result.Y := AVector1.Y - AVector2.Y;
  Result.Z := AVector1.Z - AVector2.Z;
  Result.W := AVector1.W - AVector2.W;
end;

class operator TVector3D.Equal(const AVector1, AVector2: TVector3D): Boolean;
begin
  Result :=
    SameValue(AVector1.X, AVector2.X, TEpsilon.Vector) and
    SameValue(AVector1.Y, AVector2.Y, TEpsilon.Vector) and
    SameValue(AVector1.Z, AVector2.Z, TEpsilon.Vector) and
    SameValue(AVector1.W, AVector2.W, TEpsilon.Vector);
end;

function TVector3D.EqualsTo(const AVector: TVector3D; const Epsilon: single): Boolean;
begin
  Result :=
    SameValue(X, AVector.X, Epsilon) and
    SameValue(Y, AVector.Y, Epsilon) and
    SameValue(Z, AVector.Z, Epsilon) and
    SameValue(W, AVector.W, Epsilon);
end;

class operator TVector3D.NotEqual(const AVector1, AVector2: TVector3D): Boolean;
begin
  Result := not (AVector1 = AVector2);
end;

class operator TVector3D.Negative(const AVector: TVector3D): TVector3D;
begin
  Result.X := - AVector.X;
  Result.Y := - AVector.Y;
  Result.Z := - AVector.Z;
  Result.W := - AVector.W;
end;

class operator TVector3D.Implicit(const APoint: TPoint3D): TVector3D;
begin
  Result := TVector3D.Create(APoint);
end;

class operator TVector3D.Explicit(const AVector: TVector3D): TPoint3D;
var
  ReciprocalW: single;
begin
  if not SameValue(AVector.W, 0, TEpsilon.Vector) then
  begin
    ReciprocalW := 1 / AVector.W;
    Result.X := AVector.X * ReciprocalW;
    Result.Y := AVector.Y * ReciprocalW;
    Result.Z := AVector.Z * ReciprocalW;
  end
  else
  begin
    Result.X := AVector.X;
    Result.Y := AVector.Y;
    Result.Z := AVector.Z;
  end;
end;

class operator TVector3D.Multiply(const AVector1, AVector2: TVector3D): TVector3D;
begin
  Result.X := AVector1.X * AVector2.X;
  Result.Y := AVector1.Y * AVector2.Y;
  Result.Z := AVector1.Z * AVector2.Z;
  Result.W := AVector1.W * AVector2.W;
end;

class operator TVector3D.Multiply(const AVector: TVector3D; const AFactor: single): TVector3D;
begin
  Result := AVector * TVector3D.Create(AFactor, AFactor, AFactor, AFactor);
end;

class operator TVector3D.Multiply(const AFactor: single; const AVector: TVector3D): TVector3D;
begin
  Result := AVector * AFactor;
end;

class operator TVector3D.Divide(const AVector: TVector3D; const AFactor: single): TVector3D;
begin
  if not SameValue(AFactor, 0, TEpsilon.Vector) then
    Result := AVector * (1 / AFactor)
  else
    Result := AVector;
end;

function TVector3D.Length: single;
begin
  Result := Sqrt(Self.X * Self.X + Self.Y * Self.Y + Self.Z * Self.Z + Self.W * Self.W);
end;

function TVector3D.Normalize: TVector3D;
var
  VecLen: single;
begin
  VecLen := Self.Length;
  if VecLen > 0 then
    Result := Self / VecLen
  else
    Result := Self;
end;

function TVector3D.Distance(const AVector: TVector3D): single;
begin
  Result := (AVector - Self).Length;
end;

function TVector3D.MidVector(const AVector: TVector3D): TVector3D;
begin
  Result := (Self + AVector) * 0.5;
end;

class function TVector3D.Zero: TVector3D;
begin
  Result.X := 0;
  Result.Y := 0;
  Result.Z := 0;
  Result.W := 0;
end;

{ TMatrix3D }

constructor TMatrix3D.Create(const AM11, AM12, AM13, AM14, AM21, AM22, AM23, AM24, AM31, AM32, AM33, AM34, AM41, AM42,
  AM43, AM44: single);
begin
  Self.m11 := AM11;
  Self.m12 := AM12;
  Self.m13 := AM13;
  Self.m14 := AM14;
  Self.m21 := AM21;
  Self.m22 := AM22;
  Self.m23 := AM23;
  Self.m24 := AM24;
  Self.m31 := AM31;
  Self.m32 := AM32;
  Self.m33 := AM33;
  Self.m34 := AM34;
  Self.m41 := AM41;
  Self.m42 := AM42;
  Self.m43 := AM43;
  Self.m44 := AM44;
end;

//constructor TMatrix3D.Create(const AArray: TsingleDynArray);
//begin
//  Self := TMatrix3D.Create(
//    AArray[0], AArray[4], AArray[8], AArray[12],
//    AArray[1], AArray[5], AArray[9], AArray[13],
//    AArray[2], AArray[6], AArray[10], AArray[14],
//    AArray[3], AArray[7], AArray[11], AArray[15]);
//end;

class function TMatrix3D.CreateScaling(const AScale: TPoint3D): TMatrix3D;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.m11 := AScale.X;
  Result.m22 := AScale.Y;
  Result.m33 := AScale.Z;
  Result.m44 := 1;
end;

class function TMatrix3D.CreateTranslation(const ATranslation: TPoint3D): TMatrix3D;
begin
  Result := Matrix3DIdentity;
  Result.m41 := ATranslation.X;
  Result.m42 := ATranslation.Y;
  Result.m43 := ATranslation.Z;
end;

class function TMatrix3D.CreateRotationX(const AAngle: single): TMatrix3D;
var
  Sine, Cosine: single;
begin
  SinCossingle(AAngle, Sine, Cosine);

  Result := Matrix3DIdentity;
  Result.m22 := Cosine;
  Result.m23 := Sine;
  Result.m32 := - Result.m23;
  Result.m33 := Result.m22;
end;

class function TMatrix3D.CreateRotationY(const AAngle: single): TMatrix3D;
var
  Sine, Cosine: single;
begin
  SinCossingle(AAngle, Sine, Cosine);

  Result := Matrix3DIdentity;
  Result.m11 := Cosine;
  Result.m13 := - Sine;
  Result.m31 := - Result.m13;
  Result.m33 := Result.m11;
end;

class function TMatrix3D.CreateRotationZ(const AAngle: single): TMatrix3D;
var
  Sine, Cosine: single;
begin
  SinCossingle(AAngle, Sine, Cosine);

  Result := Matrix3DIdentity;
  Result.m11 := Cosine;
  Result.m12 := Sine;
  Result.m21 := - Result.m12;
  Result.m22 := Result.m11;
end;

class function TMatrix3D.CreateRotation(const AAxis: TPoint3D; const AAngle: single): TMatrix3D;
var
  NormAxis: TPoint3D;
  Cosine, Sine, OneMinusCos: single;
begin
  SinCossingle(AAngle, Sine, Cosine);
  OneMinusCos := 1 - Cosine;
  NormAxis := AAxis.Normalize;

  Result := Matrix3DIdentity;
  Result.m11 := (OneMinusCos * NormAxis.X * NormAxis.X) + Cosine;
  Result.m12 := (OneMinusCos * NormAxis.X * NormAxis.Y) + (NormAxis.Z * Sine);
  Result.m13 := (OneMinusCos * NormAxis.Z * NormAxis.X) - (NormAxis.Y * Sine);
  Result.m21 := (OneMinusCos * NormAxis.X * NormAxis.Y) - (NormAxis.Z * Sine);
  Result.m22 := (OneMinusCos * NormAxis.Y * NormAxis.Y) + Cosine;
  Result.m23 := (OneMinusCos * NormAxis.Y * NormAxis.Z) + (NormAxis.X * Sine);
  Result.m31 := (OneMinusCos * NormAxis.Z * NormAxis.X) + (NormAxis.Y * Sine);
  Result.m32 := (OneMinusCos * NormAxis.Y * NormAxis.Z) - (NormAxis.X * Sine);
  Result.m33 := (OneMinusCos * NormAxis.Z * NormAxis.Z) + Cosine;
end;

class function TMatrix3D.CreateRotationYawPitchRoll(const AYaw, APitch, ARoll: single): TMatrix3D;
var
  SineYaw, SinePitch, SineRoll: single;
  CosineYaw, CosinePitch, CosineRoll: single;
begin
  SinCossingle(AYaw, SineYaw, CosineYaw);
  SinCossingle(APitch, SinePitch, CosinePitch);
  SinCossingle(ARoll, SineRoll, CosineRoll);

  Result := Matrix3DIdentity;
  Result.m11 := CosineRoll * CosineYaw + SinePitch * SineRoll * SineYaw;
  Result.m12 := CosineYaw * SinePitch * SineRoll - CosineRoll * SineYaw;
  Result.m13 := - CosinePitch * SineRoll;
  Result.m21 := CosinePitch * SineYaw;
  Result.m22 := CosinePitch * CosineYaw;
  Result.m23 := SinePitch;
  Result.m31 := CosineYaw * SineRoll - CosineRoll * SinePitch * SineYaw;
  Result.m32 := - CosineRoll * CosineYaw * SinePitch - SineRoll * SineYaw;
  Result.m33 := CosinePitch * CosineRoll;
end;

class function TMatrix3D.CreateRotationHeadingPitchBank(const AHeading, APitch, ABank: single): TMatrix3D;
var
  SineHeading, SinePitch, SineBank: single;
  CosineHeading, CosinePitch, CosineBank: single;
begin
  SinCossingle(AHeading, SineHeading, CosineHeading);
  SinCossingle(APitch, SinePitch, CosinePitch);
  SinCossingle(ABank, SineBank, CosineBank);

  Result := Matrix3DIdentity;
  Result.m11 := (CosineHeading * CosineBank) + (SineHeading * SinePitch * SineBank);
  Result.m12 := (- CosineHeading * SineBank) + (SineHeading * SinePitch * CosineBank);
  Result.m13 := SineHeading * CosinePitch;
  Result.m21 := SineBank * CosinePitch;
  Result.m22 := CosineBank * CosinePitch;
  Result.m23 := - SinePitch;
  Result.m31 := (- SineHeading * CosineBank) + (CosineHeading * SinePitch * SineBank);
  Result.m32 := (SineBank * SineHeading) + (CosineHeading * SinePitch * CosineBank);
  Result.m33 := CosineHeading * CosinePitch;
end;

class function TMatrix3D.CreateLookAtRH(const ASource, ATarget, ACeiling: TPoint3D): TMatrix3D;
var
  ZAxis, XAxis, YAxis: TPoint3D;
begin
  ZAxis := (ASource - ATarget).Normalize;
  XAxis := ACeiling.CrossProduct(ZAxis).Normalize;
  YAxis := ZAxis.CrossProduct(XAxis);

  Result := Matrix3DIdentity;
  Result.m11 := XAxis.X;
  Result.m12 := YAxis.X;
  Result.m13 := ZAxis.X;
  Result.m21 := XAxis.Y;
  Result.m22 := YAxis.Y;
  Result.m23 := ZAxis.Y;
  Result.m31 := XAxis.Z;
  Result.m32 := YAxis.Z;
  Result.m33 := ZAxis.Z;
  Result.m41 := - XAxis.DotProduct(ASource);
  Result.m42 := - YAxis.DotProduct(ASource);
  Result.m43 := - ZAxis.DotProduct(ASource);
end;

class function TMatrix3D.CreateLookAtLH(const ASource, ATarget, ACeiling: TPoint3D): TMatrix3D;
var
  ZAxis, XAxis, YAxis: TPoint3D;
begin
  ZAxis := (ATarget - ASource).Normalize;
  XAxis := ACeiling.CrossProduct(ZAxis).Normalize;
  YAxis := ZAxis.CrossProduct(XAxis);

  Result := Matrix3DIdentity;
  Result.m11 := XAxis.X;
  Result.m12 := YAxis.X;
  Result.m13 := ZAxis.X;
  Result.m21 := XAxis.Y;
  Result.m22 := YAxis.Y;
  Result.m23 := ZAxis.Y;
  Result.m31 := XAxis.Z;
  Result.m32 := YAxis.Z;
  Result.m33 := ZAxis.Z;
  Result.m41 := - XAxis.DotProduct(ASource);
  Result.m42 := - YAxis.DotProduct(ASource);
  Result.m43 := - ZAxis.DotProduct(ASource);
end;

class function TMatrix3D.CreateLookAtDirRH(const ASource, ADirection, ACeiling: TPoint3D): TMatrix3D;
var
  ZAxis, XAxis, YAxis: TPoint3D;
begin
  ZAxis := ADirection.Normalize;
  XAxis := ACeiling.CrossProduct(ZAxis).Normalize;
  YAxis := ZAxis.CrossProduct(XAxis);

  Result := Matrix3DIdentity;
  Result.m11 := XAxis.X;
  Result.m12 := YAxis.X;
  Result.m13 := ZAxis.X;
  Result.m21 := XAxis.Y;
  Result.m22 := YAxis.Y;
  Result.m23 := ZAxis.Y;
  Result.m31 := XAxis.Z;
  Result.m32 := YAxis.Z;
  Result.m33 := ZAxis.Z;
  Result.m41 := - XAxis.DotProduct(ASource);
  Result.m42 := - YAxis.DotProduct(ASource);
  Result.m43 := - ZAxis.DotProduct(ASource);
end;

class function TMatrix3D.CreateLookAtDirLH(const ASource, ADirection, ACeiling: TPoint3D): TMatrix3D;
var
  ZAxis, XAxis, YAxis: TPoint3D;
begin
  ZAxis := - ADirection.Normalize;
  XAxis := ACeiling.CrossProduct(ZAxis).Normalize;
  YAxis := ZAxis.CrossProduct(XAxis);

  Result := Matrix3DIdentity;
  Result.m11 := XAxis.X;
  Result.m12 := YAxis.X;
  Result.m13 := ZAxis.X;
  Result.m21 := XAxis.Y;
  Result.m22 := YAxis.Y;
  Result.m23 := ZAxis.Y;
  Result.m31 := XAxis.Z;
  Result.m32 := YAxis.Z;
  Result.m33 := ZAxis.Z;
  Result.m41 := - XAxis.DotProduct(ASource);
  Result.m42 := - YAxis.DotProduct(ASource);
  Result.m43 := - ZAxis.DotProduct(ASource);
end;

class function TMatrix3D.CreateOrthoLH(const AWidth, AHeight, AZNear, AZFar: single): TMatrix3D;
begin
  Result := Matrix3DIdentity;
  Result.m11 := 2 / AWidth;
  Result.m22 := 2 / AHeight;
  Result.m33 := 1 / (AZFar - AZNear);
  Result.m42 := AZNear / (AZNear - AZFar);
end;

class function TMatrix3D.CreateOrthoRH(const AWidth, AHeight, AZNear, AZFar: single): TMatrix3D;
begin
  Result := Matrix3DIdentity;
  Result.m11 := 2 / AWidth;
  Result.m22 := 2 / AHeight;
  Result.m33 := 1 / (AZNear - AZFar);
  Result.m42 := AZNear / (AZNear - AZFar);
end;

class function TMatrix3D.CreateOrthoOffCenterLH(const ALeft, ATop, ARight, ABottom, AZNear, AZFar: single): TMatrix3D;
begin
  Result := Matrix3DIdentity;
  Result.m11 := 2 / (ARight - ALeft);
  Result.m22 := 2 / (ATop - ABottom);
  Result.m33 := 1 / (AZFar - AZNear);
  Result.m41 := (ALeft + ARight) / (ALeft - ARight);
  Result.m42 := (ATop + ABottom) / (ABottom - ATop);
  Result.m43 := AZNear / (AZNear - AZFar);
end;

class function TMatrix3D.CreateOrthoOffCenterRH(const ALeft, ATop, ARight, ABottom, AZNear, AZFar: single): TMatrix3D;
begin
  Result := Matrix3DIdentity;
  Result.m11 := 2 / (ARight - ALeft);
  Result.m22 := 2 / (ATop - ABottom);
  Result.m33 := 1 / (AZNear - AZFar);
  Result.m41 := (ALeft + ARight) / (ALeft - ARight);
  Result.m42 := (ATop + ABottom) / (ABottom - ATop);
  Result.m43 := AZNear / (AZNear - AZFar);
end;

class function TMatrix3D.CreatePerspectiveFovLH(
  const AFOV, AAspect, AZNear, AZFar: single;
  const AHorizontalFOV: Boolean = False): TMatrix3D;
var
  XScale, YScale: single;
begin
  if AHorizontalFOV then
  begin
    XScale := 1 / Tangent(AFOV / 2);
    YScale := XScale / AAspect;
  end else
  begin
    YScale := 1 / Tangent(AFOV / 2);
    XScale := YScale / AAspect;
  end;

  Result := Matrix3DIdentity;
  Result.m11 := XScale;
  Result.m22 := YScale;
  Result.m33 := AZFar / (AZFar - AZNear);
  Result.m34 := 1;
  Result.m43 := -AZNear * AZFar / (AZFar - AZNear);
  Result.m44 := 0;
end;

class function TMatrix3D.CreatePerspectiveFovRH(
  const AFOV, AAspect, AZNear, AZFar: single;
  const AHorizontalFOV: Boolean = False): TMatrix3D;
var
  XScale, YScale: single;
begin
  if AHorizontalFOV then
  begin
    XScale := 1 / Tangent(AFOV / 2);
    YScale := XScale / AAspect;
  end else
  begin
    YScale := 1 / Tangent(AFOV / 2);
    XScale := YScale / AAspect;
  end;

  Result := Matrix3DIdentity;
  Result.m11 := XScale;
  Result.m22 := YScale;
  Result.m33 := AZFar / (AZNear - AZFar);
  Result.m34 := -1;
  Result.m43 := AZNear * AZFar / (AZNear - AZFar);
  Result.m44 := 0;
end;

class operator TMatrix3D.Multiply(const APoint1, APoint2: TMatrix3D): TMatrix3D;
begin
  Result.M[0].V[0] :=
      APoint1.M[0].V[0] * APoint2.M[0].V[0]
    + APoint1.M[0].V[1] * APoint2.M[1].V[0]
    + APoint1.M[0].V[2] * APoint2.M[2].V[0]
    + APoint1.M[0].V[3] * APoint2.M[3].V[0];
  Result.M[0].V[1] :=
      APoint1.M[0].V[0] * APoint2.M[0].V[1]
    + APoint1.M[0].V[1] * APoint2.M[1].V[1]
    + APoint1.M[0].V[2] * APoint2.M[2].V[1]
    + APoint1.M[0].V[3] * APoint2.M[3].V[1];
  Result.M[0].V[2] :=
      APoint1.M[0].V[0] * APoint2.M[0].V[2]
    + APoint1.M[0].V[1] * APoint2.M[1].V[2]
    + APoint1.M[0].V[2] * APoint2.M[2].V[2]
    + APoint1.M[0].V[3] * APoint2.M[3].V[2];
  Result.M[0].V[3] :=
      APoint1.M[0].V[0] * APoint2.M[0].V[3]
    + APoint1.M[0].V[1] * APoint2.M[1].V[3]
    + APoint1.M[0].V[2] * APoint2.M[2].V[3]
    + APoint1.M[0].V[3] * APoint2.M[3].V[3];
  Result.M[1].V[0] :=
      APoint1.M[1].V[0] * APoint2.M[0].V[0]
    + APoint1.M[1].V[1] * APoint2.M[1].V[0]
    + APoint1.M[1].V[2] * APoint2.M[2].V[0]
    + APoint1.M[1].V[3] * APoint2.M[3].V[0];
  Result.M[1].V[1] :=
      APoint1.M[1].V[0] * APoint2.M[0].V[1]
    + APoint1.M[1].V[1] * APoint2.M[1].V[1]
    + APoint1.M[1].V[2] * APoint2.M[2].V[1]
    + APoint1.M[1].V[3] * APoint2.M[3].V[1];
  Result.M[1].V[2] :=
      APoint1.M[1].V[0] * APoint2.M[0].V[2]
    + APoint1.M[1].V[1] * APoint2.M[1].V[2]
    + APoint1.M[1].V[2] * APoint2.M[2].V[2]
    + APoint1.M[1].V[3] * APoint2.M[3].V[2];
  Result.M[1].V[3] :=
      APoint1.M[1].V[0] * APoint2.M[0].V[3]
    + APoint1.M[1].V[1] * APoint2.M[1].V[3]
    + APoint1.M[1].V[2] * APoint2.M[2].V[3]
    + APoint1.M[1].V[3] * APoint2.M[3].V[3];
  Result.M[2].V[0] :=
      APoint1.M[2].V[0] * APoint2.M[0].V[0]
    + APoint1.M[2].V[1] * APoint2.M[1].V[0]
    + APoint1.M[2].V[2] * APoint2.M[2].V[0]
    + APoint1.M[2].V[3] * APoint2.M[3].V[0];
  Result.M[2].V[1] :=
      APoint1.M[2].V[0] * APoint2.M[0].V[1]
    + APoint1.M[2].V[1] * APoint2.M[1].V[1]
    + APoint1.M[2].V[2] * APoint2.M[2].V[1]
    + APoint1.M[2].V[3] * APoint2.M[3].V[1];
  Result.M[2].V[2] :=
      APoint1.M[2].V[0] * APoint2.M[0].V[2]
    + APoint1.M[2].V[1] * APoint2.M[1].V[2]
    + APoint1.M[2].V[2] * APoint2.M[2].V[2]
    + APoint1.M[2].V[3] * APoint2.M[3].V[2];
  Result.M[2].V[3] :=
      APoint1.M[2].V[0] * APoint2.M[0].V[3]
    + APoint1.M[2].V[1] * APoint2.M[1].V[3]
    + APoint1.M[2].V[2] * APoint2.M[2].V[3]
    + APoint1.M[2].V[3] * APoint2.M[3].V[3];
  Result.M[3].V[0] :=
      APoint1.M[3].V[0] * APoint2.M[0].V[0]
    + APoint1.M[3].V[1] * APoint2.M[1].V[0]
    + APoint1.M[3].V[2] * APoint2.M[2].V[0]
    + APoint1.M[3].V[3] * APoint2.M[3].V[0];
  Result.M[3].V[1] :=
      APoint1.M[3].V[0] * APoint2.M[0].V[1]
    + APoint1.M[3].V[1] * APoint2.M[1].V[1]
    + APoint1.M[3].V[2] * APoint2.M[2].V[1]
    + APoint1.M[3].V[3] * APoint2.M[3].V[1];
  Result.M[3].V[2] :=
      APoint1.M[3].V[0] * APoint2.M[0].V[2]
    + APoint1.M[3].V[1] * APoint2.M[1].V[2]
    + APoint1.M[3].V[2] * APoint2.M[2].V[2]
    + APoint1.M[3].V[3] * APoint2.M[3].V[2];
  Result.M[3].V[3] :=
      APoint1.M[3].V[0] * APoint2.M[0].V[3]
    + APoint1.M[3].V[1] * APoint2.M[1].V[3]
    + APoint1.M[3].V[2] * APoint2.M[2].V[3]
    + APoint1.M[3].V[3] * APoint2.M[3].V[3];
end;

class operator TMatrix3D.Multiply(const APoint: TPoint3D; const AMatrix: TMatrix3D): TPoint3D;
begin
  Result.X := (APoint.X * AMatrix.m11) + (APoint.Y * AMatrix.m21) + (APoint.Z * AMatrix.m31) + AMatrix.m41;
  Result.Y := (APoint.X * AMatrix.m12) + (APoint.Y * AMatrix.m22) + (APoint.Z * AMatrix.m32) + AMatrix.m42;
  Result.Z := (APoint.X * AMatrix.m13) + (APoint.Y * AMatrix.m23) + (APoint.Z * AMatrix.m33) + AMatrix.m43;
end;

class operator TMatrix3D.Multiply(const AVector: TVector3D;
 const AMatrix: TMatrix3D): TVector3D;
begin
  Result.X := (AVector.X * AMatrix.m11) + (AVector.Y * AMatrix.m21) + (AVector.Z * AMatrix.m31) + (AVector.W * AMatrix.m41);
  Result.Y := (AVector.X * AMatrix.m12) + (AVector.Y * AMatrix.m22) + (AVector.Z * AMatrix.m32) + (AVector.W * AMatrix.m42);
  Result.Z := (AVector.X * AMatrix.m13) + (AVector.Y * AMatrix.m23) + (AVector.Z * AMatrix.m33) + (AVector.W * AMatrix.m43);
  Result.W := (AVector.X * AMatrix.m14) + (AVector.Y * AMatrix.m24) + (AVector.Z * AMatrix.m34) + (AVector.W * AMatrix.m44);
end;

function TMatrix3D.Scale(const AFactor: single): TMatrix3D;
var
  i: Integer;
begin
  for i := 0 to 3 do
  begin
    Result.M[i].V[0] := Self.M[i].V[0] * AFactor;
    Result.M[i].V[1] := Self.M[i].V[1] * AFactor;
    Result.M[i].V[2] := Self.M[i].V[2] * AFactor;
    Result.M[i].V[3] := Self.M[i].V[3] * AFactor;
  end;
end;

function TMatrix3D.Transpose: TMatrix3D;
begin
  Result.M[0].V[0] := Self.M[0].V[0];
  Result.M[0].V[1] := Self.M[1].V[0];
  Result.M[0].V[2] := Self.M[2].V[0];
  Result.M[0].V[3] := Self.M[3].V[0];
  Result.M[1].V[0] := Self.M[0].V[1];
  Result.M[1].V[1] := Self.M[1].V[1];
  Result.M[1].V[2] := Self.M[2].V[1];
  Result.M[1].V[3] := Self.M[3].V[1];
  Result.M[2].V[0] := Self.M[0].V[2];
  Result.M[2].V[1] := Self.M[1].V[2];
  Result.M[2].V[2] := Self.M[2].V[2];
  Result.M[2].V[3] := Self.M[3].V[2];
  Result.M[3].V[0] := Self.M[0].V[3];
  Result.M[3].V[1] := Self.M[1].V[3];
  Result.M[3].V[2] := Self.M[2].V[3];
  Result.M[3].V[3] := Self.M[3].V[3];
end;

function TMatrix3D.EyePosition: TPoint3D;
type
  TMatrix3DArray = array [0 .. 15] of single;
begin
  Result.X :=
    - TMatrix3DArray(Self)[0] * TMatrix3DArray(Self)[12]
    - TMatrix3DArray(Self)[1] * TMatrix3DArray(Self)[13]
    - TMatrix3DArray(Self)[2] * TMatrix3DArray(Self)[14];

  Result.Y :=
    - TMatrix3DArray(Self)[4] * TMatrix3DArray(Self)[12]
    - TMatrix3DArray(Self)[5] * TMatrix3DArray(Self)[13]
    - TMatrix3DArray(Self)[6] * TMatrix3DArray(Self)[14];

  Result.Z :=
    - TMatrix3DArray(Self)[8] * TMatrix3DArray(Self)[12]
    - TMatrix3DArray(Self)[9] * TMatrix3DArray(Self)[13]
    - TMatrix3DArray(Self)[10] * TMatrix3DArray(Self)[14];
end;

function TMatrix3D.DetInternal(const a1, a2, a3, b1, b2, b3, c1, c2, c3: single): single;
begin
  Result := a1 * (b2 * c3 - b3 * c2) - b1 * (a2 * c3 - a3 * c2) + c1 * (a2 * b3 - a3 * b2);
end;

function TMatrix3D.Determinant: single;
begin
  Result :=
    Self.M[0].V[0] * DetInternal(
      Self.M[1].V[1],
      Self.M[2].V[1],
      Self.M[3].V[1],
      Self.M[1].V[2],
      Self.M[2].V[2],
      Self.M[3].V[2],
      Self.M[1].V[3],
      Self.M[2].V[3],
      Self.M[3].V[3])
    - Self.M[0].V[1] * DetInternal(
      Self.M[1].V[0],
      Self.M[2].V[0],
      Self.M[3].V[0],
      Self.M[1].V[2],
      Self.M[2].V[2],
      Self.M[3].V[2],
      Self.M[1].V[3],
      Self.M[2].V[3],
      Self.M[3].V[3])
    + Self.M[0].V[2] * DetInternal(
      Self.M[1].V[0],
      Self.M[2].V[0],
      Self.M[3].V[0],
      Self.M[1].V[1],
      Self.M[2].V[1],
      Self.M[3].V[1],
      Self.M[1].V[3],
      Self.M[2].V[3],
      Self.M[3].V[3])
    - Self.M[0].V[3] * DetInternal(
      Self.M[1].V[0],
      Self.M[2].V[0],
      Self.M[3].V[0],
      Self.M[1].V[1],
      Self.M[2].V[1],
      Self.M[3].V[1],
      Self.M[1].V[2],
      Self.M[2].V[2],
      Self.M[3].V[2]);
end;

function TMatrix3D.Adjoint: TMatrix3D;
var
  a1, a2, a3, a4, b1, b2, b3, b4, c1, c2, c3, c4, d1, d2, d3, d4: single;
begin
  a1 := Self.M[0].V[0];
  b1 := Self.M[0].V[1];
  c1 := Self.M[0].V[2];
  d1 := Self.M[0].V[3];
  a2 := Self.M[1].V[0];
  b2 := Self.M[1].V[1];
  c2 := Self.M[1].V[2];
  d2 := Self.M[1].V[3];
  a3 := Self.M[2].V[0];
  b3 := Self.M[2].V[1];
  c3 := Self.M[2].V[2];
  d3 := Self.M[2].V[3];
  a4 := Self.M[3].V[0];
  b4 := Self.M[3].V[1];
  c4 := Self.M[3].V[2];
  d4 := Self.M[3].V[3];

  Result.M[0].V[0] := DetInternal(b2, b3, b4, c2, c3, c4, d2, d3, d4);
  Result.M[1].V[0] := -DetInternal(a2, a3, a4, c2, c3, c4, d2, d3, d4);
  Result.M[2].V[0] := DetInternal(a2, a3, a4, b2, b3, b4, d2, d3, d4);
  Result.M[3].V[0] := -DetInternal(a2, a3, a4, b2, b3, b4, c2, c3, c4);

  Result.M[0].V[1] := -DetInternal(b1, b3, b4, c1, c3, c4, d1, d3, d4);
  Result.M[1].V[1] := DetInternal(a1, a3, a4, c1, c3, c4, d1, d3, d4);
  Result.M[2].V[1] := -DetInternal(a1, a3, a4, b1, b3, b4, d1, d3, d4);
  Result.M[3].V[1] := DetInternal(a1, a3, a4, b1, b3, b4, c1, c3, c4);

  Result.M[0].V[2] := DetInternal(b1, b2, b4, c1, c2, c4, d1, d2, d4);
  Result.M[1].V[2] := -DetInternal(a1, a2, a4, c1, c2, c4, d1, d2, d4);
  Result.M[2].V[2] := DetInternal(a1, a2, a4, b1, b2, b4, d1, d2, d4);
  Result.M[3].V[2] := -DetInternal(a1, a2, a4, b1, b2, b4, c1, c2, c4);

  Result.M[0].V[3] := -DetInternal(b1, b2, b3, c1, c2, c3, d1, d2, d3);
  Result.M[1].V[3] := DetInternal(a1, a2, a3, c1, c2, c3, d1, d2, d3);
  Result.M[2].V[3] := -DetInternal(a1, a2, a3, b1, b2, b3, d1, d2, d3);
  Result.M[3].V[3] := DetInternal(a1, a2, a3, b1, b2, b3, c1, c2, c3);
end;

function TMatrix3D.Inverse: TMatrix3D;
const
  DefaultValue: TMatrix3D = (
    m11: 1.0; m12: 0.0; m13: 0.0; m14: 0.0;
    m21: 0.0; m22: 1.0; m23: 0.0; m24: 0.0;
    m31: 0.0; m32: 0.0; m33: 1.0; m34: 0.0;
    m41: 0.0; m42: 0.0; m43: 0.0; m44: 1.0;);
var
  Det: single;
begin
  Det := Self.Determinant;
  if Abs(Det) < Epsilon then
    Result := DefaultValue
  else
    Result := Self.Adjoint.Scale(1 / Det);
end;

{ TQuaternion3D }

constructor TQuaternion3D.Create(const AAxis: TPoint3D; const AAngle: single);
var
  AxisLen, Sine, Cosine: single;
begin
  AxisLen := AAxis.Length;

  if AxisLen > 0 then
  begin
    SinCossingle(AAngle / 2, Sine, Cosine);

    Self.RealPart := Cosine;
    Self.ImagPart := AAxis * (Sine / AxisLen);
  end else Self := Quaternion3DIdentity;
end;

constructor TQuaternion3D.Create(const AYaw, APitch, ARoll: single);
begin
  Self :=
    TQuaternion3D.Create(Point3D(0, 1, 0), AYaw) *
    TQuaternion3D.Create(Point3D(1, 0, 0), APitch) *
    TQuaternion3D.Create(Point3D(0, 0, 1), ARoll);
end;

constructor TQuaternion3D.Create(const AMatrix: TMatrix3D);
var
  Trace, S: double;
  NewQuat: TQuaternion3D;
begin
  Trace := AMatrix.m11 + AMatrix.m22 + AMatrix.m33;
  if Trace > EPSILON then
  begin
    S := 0.5 / Sqrt(Trace + 1.0);
    NewQuat.ImagPart.X := (AMatrix.M23 - AMatrix.M32) * S;
    NewQuat.ImagPart.Y := (AMatrix.M31 - AMatrix.M13) * S;
    NewQuat.ImagPart.Z := (AMatrix.M12 - AMatrix.M21) * S;
    NewQuat.RealPart := 0.5 * Sqrt(Trace + 1.0);
  end
  else if (AMatrix.M11 > AMatrix.M22) and (AMatrix.M11 > AMatrix.M33) then
  begin
    S := Sqrt(Max(EPSILON, 1 + AMatrix.M11 - AMatrix.M22 - AMatrix.M33)) * 2.0;
    NewQuat.ImagPart.X := 0.25 * S;
    NewQuat.ImagPart.Y := (AMatrix.M12 + AMatrix.M21) / S;
    NewQuat.ImagPart.Z := (AMatrix.M31 + AMatrix.M13) / S;
    NewQuat.RealPart := (AMatrix.M23 - AMatrix.M32) / S;
  end
  else if (AMatrix.M22 > AMatrix.M33) then
  begin
    S := Sqrt(Max(EPSILON, 1 + AMatrix.M22 - AMatrix.M11 - AMatrix.M33)) * 2.0;
    NewQuat.ImagPart.X := (AMatrix.M12 + AMatrix.M21) / S;
    NewQuat.ImagPart.Y := 0.25 * S;
    NewQuat.ImagPart.Z := (AMatrix.M23 + AMatrix.M32) / S;
    NewQuat.RealPart := (AMatrix.M31 - AMatrix.M13) / S;
  end else
  begin
    S := Sqrt(Max(EPSILON, 1 + AMatrix.M33 - AMatrix.M11 - AMatrix.M22)) * 2.0;
    NewQuat.ImagPart.X := (AMatrix.M31 + AMatrix.M13) / S;
    NewQuat.ImagPart.Y := (AMatrix.M23 + AMatrix.M32) / S;
    NewQuat.ImagPart.Z := 0.25 * S;
    NewQuat.RealPart := (AMatrix.M12 - AMatrix.M21) / S;
  end;
  Self := NewQuat.Normalize;
end;

class operator TQuaternion3D.Implicit(const AQuaternion: TQuaternion3D): TMatrix3D;
var
  NormQuat: TQuaternion3D;
  xx, xy, xz, xw, yy, yz, yw, zz, zw: single;
begin
  NormQuat := AQuaternion.Normalize;

  xx := NormQuat.ImagPart.X * NormQuat.ImagPart.X;
  xy := NormQuat.ImagPart.X * NormQuat.ImagPart.Y;
  xz := NormQuat.ImagPart.X * NormQuat.ImagPart.Z;
  xw := NormQuat.ImagPart.X * NormQuat.RealPart;
  yy := NormQuat.ImagPart.Y * NormQuat.ImagPart.Y;
  yz := NormQuat.ImagPart.Y * NormQuat.ImagPart.Z;
  yw := NormQuat.ImagPart.Y * NormQuat.RealPart;
  zz := NormQuat.ImagPart.Z * NormQuat.ImagPart.Z;
  zw := NormQuat.ImagPart.Z * NormQuat.RealPart;

  FillChar(Result, Sizeof(Result), 0);
  Result.M11 := 1 - 2 * (yy + zz);
  Result.M21 := 2 * (xy - zw);
  Result.M31 := 2 * (xz + yw);
  Result.M12 := 2 * (xy + zw);
  Result.M22 := 1 - 2 * (xx + zz);
  Result.M32 := 2 * (yz - xw);
  Result.M13 := 2 * (xz - yw);
  Result.M23 := 2 * (yz + xw);
  Result.M33 := 1 - 2 * (xx + yy);
  Result.M44 := 1;
end;

class operator TQuaternion3D.Multiply(const AQuaternion1, AQuaternion2: TQuaternion3D): TQuaternion3D;
begin
  Result.RealPart :=
      AQuaternion1.RealPart * AQuaternion2.RealPart
    - AQuaternion1.ImagPart.X * AQuaternion2.ImagPart.X
    - AQuaternion1.ImagPart.Y * AQuaternion2.ImagPart.Y
    - AQuaternion1.ImagPart.Z * AQuaternion2.ImagPart.Z;

  Result.ImagPart.X :=
      AQuaternion1.RealPart * AQuaternion2.ImagPart.X
    + AQuaternion2.RealPart * AQuaternion1.ImagPart.X
    + AQuaternion1.ImagPart.Y * AQuaternion2.ImagPart.Z
    - AQuaternion1.ImagPart.Z * AQuaternion2.ImagPart.Y;

  Result.ImagPart.Y :=
      AQuaternion1.RealPart * AQuaternion2.ImagPart.Y
    + AQuaternion2.RealPart * AQuaternion1.ImagPart.Y
    + AQuaternion1.ImagPart.Z * AQuaternion2.ImagPart.X
    - AQuaternion1.ImagPart.X * AQuaternion2.ImagPart.Z;

  Result.ImagPart.Z :=
      AQuaternion1.RealPart * AQuaternion2.ImagPart.Z
    + AQuaternion2.RealPart * AQuaternion1.ImagPart.Z
    + AQuaternion1.ImagPart.X * AQuaternion2.ImagPart.Y
    - AQuaternion1.ImagPart.Y * AQuaternion2.ImagPart.X;
end;

function TQuaternion3D.Length: single;
begin
  Result := Sqrt(Self.ImagPart.DotProduct(Self.ImagPart) + Self.RealPart * Self.RealPart);
end;

function TQuaternion3D.Normalize: TQuaternion3D;
var
  QuatLen, InvLen: single;
begin
  QuatLen := Self.Length;
  if QuatLen > EPSILON2 then
  begin
    InvLen := 1 / QuatLen;
    Result.ImagPart := Self.ImagPart * InvLen;
    Result.RealPart := Self.RealPart * InvLen;
  end
  else
    Result := Quaternion3DIdentity;
end;

function TMatrix3D.ToEulerAngles: TPoint3D;
var
  heading: single;
  attitude: single;
  bank: single;
begin

{  this conversion uses conventions as described on page:
   https://www.euclideanspace.com/maths/geometry/rotations/euler/index.htm

   Coordinate System: right hand
   Positive angle: right hand
   Order of euler angles: heading first, then attitude, then bank

   matrix row column ordering in code on website:
   [m00 m01 m02]
   [m10 m11 m12]
   [m20 m21 m22]

   matrix row column ordering in TMatrix3D:
   [m11 m12 m13]
   [m21 m22 m23]
   [m31 m32 m33]
}

  { Tait-Bryan angles Y1 Z2 X3 }

  { Assuming the angles are in radians. }
  if (m21 > 0.998) then
  begin
    { singularity at north pole }
    heading := arctan2(m13, m33);
    attitude := PI/2;
    bank := 0;
  end
  else if (m21 < -0.998) then
  begin
    { singularity at south pole }
    heading := arctan2(m13, m33);
    attitude := -PI/2;
    bank := 0;
  end
  else
  begin
    heading := arctan2(-m31, m11);
    attitude := arcsin(m21);
    bank := arctan2(-m23, m22);
  end;

  result := TPoint3D.Create(heading, attitude, bank);
end;

class function TMatrix3D.CreateFromEulerAngles(heading, attitude, bank: single): TMatrix3D;
var
  c1, s1: single;
  c2, s2: single;
  c3, s3: single;
begin
  result := Matrix3DIdentity;

  { angles are in radians }
  c1 := cos(heading);
  s1 := sin(heading);

  c2 := cos(attitude);
  s2 := sin(attitude);

  c3 := cos(bank);
  s3 := sin(bank);

  { http://www.euclideanspace.com/maths/geometry/rotations/conversions/eulerToMatrix/index.htm }
  { https://en.wikipedia.org/wiki/Euler_angles }

  { Tait-Bryan angles Y1 Z2 X3 }

  { first row}
  result.m11 := c1 * c2; { first column }
  result.m12 := s1 * s3 - c1 * s2 * c3;
  result.m13 := c1 * s2 * s3 + s1 * c3;

  { second row}
  result.m21 := s2;
  result.m22 := c2 * c3;
  result.m23 := -c2 * s3;

  { third row }
  result.m31 := -s1 * c2;
  result.m32 := s1 * s2 * c3 + c1 * s3;
  result.m33 := -s1 * s2 * s3 + c1 * c3;
end;

function TQuaternion3D.ToEulerAngles: TPoint3D;
var
  test: single;
  sqx: single;
  sqy: single;
  sqz: single;
  heading: single;
  attitude: single;
  bank: single;
begin
  test := ImagPart.X * ImagPart.Y + ImagPart.Z * RealPart;
  if (test > 0.499) then
  begin
    { singularity at north pole }
    heading := 2 * arctan2(ImagPart.X, RealPart);
    attitude := PI/2;
    bank := 0;
  end
  else if (test < -0.499) then
  begin
    { singularity at south pole }
    heading := -2 * arctan2(ImagPart.X, RealPart);
    attitude := - PI/2;
    bank := 0;
  end
  else
  begin
    sqx := ImagPart.X * ImagPart.X;
    sqy := ImagPart.Y * ImagPart.Y;
    sqz := ImagPart.Z * ImagPart.Z;

    heading := arctan2(
      2 * ImagPart.Y * RealPart - 2 * ImagPart.X * ImagPart.Z,
      1 - 2 * sqy - 2 * sqz);

    attitude := arcsin(2 * test);

    bank := arctan2(
      2 * ImagPart.X * RealPart - 2 * ImagPart.Y * ImagPart.Z,
      1 - 2 * sqx - 2 * sqz);
  end;

  result := TPoint3D.Create(heading, attitude, bank);
end;

class function TQuaternion3D.CreateQuaternionFromEulerAngles(yaw, pitch, roll: single): TQuaternion3D;
var
  cy, sy, cp, sp, cr, sr: single;
  q: TQuaternion3D;
begin
  { Z = yaw,
    Y = pitch,
    X = roll }

  cy := cos(yaw * 0.5);
  sy := sin(yaw * 0.5);

  cp := cos(pitch * 0.5);
  sp := sin(pitch * 0.5);

  cr := cos(roll * 0.5);
  sr := sin(roll * 0.5);

  q.ImagPart.X := sr * cp * cy - cr * sp * sy;
  q.ImagPart.Y := cr * sp * cy + sr * cp * sy;
  q.ImagPart.Z := cr * cp * sy - sr * sp * cy;

  q.RealPart := cr * cp * cy + sr * sp * sy;

  result := q;
end;

class function TPoint3D.RotD(const Value: TPoint3D): TPoint3D;
begin
  result := TPoint3D.Create(
    RadToDeg(Value.X),
    RadToDeg(Value.Y),
    RadToDeg(Value.Z));
end;

class function TPoint3D.RotR(const Value: TPoint3D): TPoint3D;
begin
  result := TPoint3D.Create(
    DegToRad(Value.X),
    DegToRad(Value.Y),
    DegToRad(Value.Z));
end;

end.
