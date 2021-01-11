﻿unit RiggVar.FD.Elements;

(*
-
-     F
-    * * *
-   *   *   G
-  *     * *   *
- E - - - H - - - I
-  *     * *         *
-   *   *   *           *
-    * *     *             *
-     D-------A---------------B
-              *
-              (C) federgraph.de
-
*)

{$ifdef fpc}
  {$mode delphi}
{$endif}

interface

uses
  System.SysUtils,
  System.Classes,
  System.Types,
  System.UITypes,
  System.UIConsts,
  System.Math,
  RiggVar.RG.Calc,
  RiggVar.FB.Color,
  RiggVar.FD.Point,
  Vcl.Graphics;

type
  TRggColorScheme = record
    TextColor: TRggColor;
    BackgroundColor: TRggColor;
    LabelColor: TRggColor;
    procedure GoDark;
    procedure GoLight;
  end;

  TLineSegmentCompareCase = (
    ccNone,
    ccNil,
    ccHardcodedAbove,
    ccHardcodedBelow,
    ccParallel,
    ccTotallyAbove,
    ccTotallyBelow,
    ccTotallySame,
    ccCommonNone,
    ccCommonAbove,
    ccCommonBelow,
    ccCommonSame,
    ccAbove,
    ccBelow,
    ccSame,
    ccUnknown
  );

  TDisplayItemType = (
    diLine,
    diPolyLine,
    diEllipse
  );

  TBemerkungGG = (
    g1Vertical,
    g2Vertical,
    ggParallel,
    ggOK
  );

  TRggPoint3D = record
    function Rotate(const AAngle: Single): TRggPoint3D;
    function Angle(const APoint: TRggPoint3D): single;
    function Length: single;
    function Normalize: TRggPoint3D;
    function Distance(const APoint: TRggPoint3D): single;
    class function Zero: TRggPoint3D; static;

    class operator Add(const APoint1, APoint2: TRggPoint3D): TRggPoint3D;
    class operator Subtract(const APoint1, APoint2: TRggPoint3D): TRggPoint3D;
    class operator Implicit(const APoint: TPoint): TRggPoint3D; inline;
    class operator Implicit(const APoint: TRggPoint3D): TPoint; inline;

    case Integer of
      0: (X: single;
          Y: single;
          Z: single;);
      1: (C: TPoint3D);
      2: (P: TPointF;
          T: single;);
  end;

  TRggPoly = array of TRggPoint3D;

  TRggDrawingBase = class
  private
    FIsDark: Boolean;
    procedure SetIsDark(const Value: Boolean);
  public
    WantRotation: Boolean;
    WheelFlag: Boolean;
    InplaceFlag: Boolean;
    ViewpointFlag: Boolean;
    FixPoint3D: TPoint3D;
    Colors: TRggColorScheme;
    FaxPoint3D: TRggPoint3D;
    class var
      WantOffset: Boolean;
    procedure Reset; virtual; abstract;
    procedure Transform(M: TMatrix3D); virtual; abstract;
    procedure GoDark; virtual;
    procedure GoLight; virtual;
    property IsDark: Boolean read FIsDark write SetIsDark;
  end;

  TRggElement = class
  private
    FStrokeColor: TColor;
    FStrokeThickness: Integer;
    procedure SetStrokeColor(const Value: TColor);
    procedure SetStrokeThickness(const Value: Integer);
  protected
    TypeName: string;
    TextCenter: TPointF;
    TextAngle: single;
    TextRadius: single;
    WantTextRect: Boolean;
    class var
    Temp1: TRggPoint3D;
    Temp2: TRggPoint3D;
    Temp3: TRggPoint3D;
    procedure TextOut(g: TCanvas; s: string);
    procedure TextOutLeading(g: TCanvas; s: string);
  public
    Caption: string;
    ShowCaption: Boolean;
    SpecialDraw: Boolean;
    Painted: Boolean;
    IsComputed: Boolean;
    Visible: Boolean;
    Drawing: TRggDrawingBase;

    constructor Create;

    function GetListCaption: string; virtual;
    procedure GetInfo(ML: TStrings); virtual;
    function GetValid: Boolean; virtual;

    procedure Transform; virtual;
    procedure Draw(g: TCanvas); virtual;

    procedure Param1(Delta: single); virtual;
    procedure Param2(Delta: single); virtual;
    procedure Param3(Delta: single); virtual;

    procedure Param7(Delta: single);
    procedure Param8(Delta: single);

    property StrokeThickness: Integer read FStrokeThickness write SetStrokeThickness;
    property StrokeColor: TColor read FStrokeColor write SetStrokeColor;
  end;

  TRggLabel = class(TRggElement)
  public
    Position: TPointF;
    Text: string;
    IsMemoLabel: Boolean;
    constructor Create;
    function GetListCaption: string; override;
    procedure Draw(g: TCanvas); override;
  end;

  TRggParam = class(TRggElement)
  private
    FOriginalValue: single;
    FValue: single;
    FScale: single;
    FBaseValue: single;
    procedure SetValue(const Value: single);
    function GetRelativeValue: single;
    procedure SetScale(const Value: single);
    procedure SetBaseValue(const Value: single);
  public
    StartPoint: TPointF;
    Text: string;
    constructor Create;
    procedure Save;
    procedure Reset;
    procedure Param1(Delta: single); override;
    procedure Draw(g: TCanvas); override;
    property Value: single read FValue write SetValue;
    property BaseValue: single read FBaseValue write SetBaseValue;
    property OriginalValue: single read FOriginalValue;
    property RelativeValue: single read GetRelativeValue;
    property Scale: single read FScale write SetScale;
  end;

  TRggCircle = class(TRggElement)
  private
    FRadius: single;
    procedure SetRadius(const Value: single);
  protected
    property Radius: single read FRadius write SetRadius;
  public
    OriginalCenter: TRggPoint3D;
    Center: TRggPoint3D;
    class var
      Matrix: TMatrix3D;

    constructor Create; overload;
    constructor Create(ACaption: string); overload;

    procedure Save;
    procedure Reset;
    procedure Transform; override;
    procedure TransformI;
    procedure WriteCode(ML: TStrings);

    procedure Draw(g: TCanvas); override;

    procedure Param1(Delta: single); override;
    procedure Param2(Delta: single); override;
    procedure Param3(Delta: single); override;

    procedure Param1I(Delta: single);
    procedure Param2I(Delta: single);

    function IsEqual(B: TRggCircle): Boolean;
    function CompareZ(Q: TRggCircle): Integer;

    class function Compare(const Left, Right: TRggCircle): Integer;
  end;

  TRggBigCircle = class(TRggCircle)
  public
    constructor Create(ACaption: string = '');
    procedure Draw(g: TCanvas); override;
    procedure Param3(Delta: single); override;
    property Radius;
  end;

  TRggFixpointCircle = class(TRggCircle)
  public
    constructor Create(ACaption: string = '');
    procedure Draw(g: TCanvas); override;
  end;

  TRggBigArc = class(TRggElement)
  private
    FSweepAngle: single;
    procedure SetSweepAngle(const Value: single);
  public
    Point1: TRggCircle;
    Point2: TRggCircle;

    constructor Create(ACaption: string = '');

    procedure GetInfo(ML: TStrings); override;
    function GetValid: Boolean; override;

    procedure Draw(g: TCanvas); override;

    procedure Param1(Delta: single); override;

    property SweepAngle: single read FSweepAngle write SetSweepAngle;
  end;

  TRggLine = class(TRggElement)
  private
    function GetLength: single;
  public
    Point1: TRggCircle;
    Point2: TRggCircle;

    Bemerkung: TLineSegmentCompareCase;

    constructor Create(ACaption: string = '');

    procedure GetInfo(ML: TStrings); override;
    function GetValid: Boolean; override;

    procedure Draw(g: TCanvas); override;
    procedure Param1(Delta: single); override;
    procedure Param2(Delta: single); override;
    function V2: TPointF;
    function V3: TPoint3D;

    function IsSame(Other: TRggLine): Boolean;
    function IsTotallyAbove(Other: TRggLine): Boolean;
    function IsTotallyBelow(Other: TRggLine): Boolean;
    function ComputeSPZ(SP: TPoint3D): single;
    procedure ReportData(ML: TStrings);

    class var
    CounterLeftNil: Integer;
    CounterRightNil: Integer;
    CounterHardCodedAbove: Integer;
    CounterHardCodedBelow: Integer;
    CounterSame: Integer;
    CounterTotallyAbove: Integer;
    CounterTotallyBelow: Integer;
    CounterCommon: Integer;
    CounterParallel: Integer;
    CounterSPZ: Integer;
    CounterZero: Integer;

    class procedure ResetCounter;
    class function CounterSum: Integer;
    class function Compare(const Left, Right: TRggLine): Integer;

    property LineLength: single read GetLength;
  end;

  TRggRotaLine = class(TRggLine)
  public
    constructor Create(ACaption: string = '');
    procedure Param1(Delta: single); override;
    procedure Param2(Delta: single); override;
  end;

  TRggLagerLine = class(TRggLine)
  private
    procedure DrawLager(g: TCanvas; P: TPointF; FestLager: Boolean);
  public
    procedure Draw(g: TCanvas); override;
  end;

  TRggPolyLine = class(TRggLine)
  private
    FCount: Integer;
  protected
    TransformedPoly: array of TPoint;
    procedure DrawText(g: TCanvas);
  public
    Poly: array of TPoint;
    ShowPoly: Boolean;
    constructor Create(ACaption: string = ''); overload;
    constructor Create(ACaption: string; ACount: Integer); overload;
    procedure Draw(g: TCanvas); override;
    property Count: Integer read FCount;
  end;

  TRggPolyCurve = class(TRggElement)
  private
    FCount: Integer;
  protected
    TransformedPoly: array of TPoint;
    procedure DrawText(g: TCanvas);
  public
    Poly: array of TPoint;
    constructor Create(ACaption: string; ACount: Integer); overload;
    procedure Draw(g: TCanvas); override;
    procedure AssignPoly(const APoly: array of TPointF);
    property Count: Integer read FCount;
  end;

  TRggPolyLine3D = class(TRggPolyLine)
  private
    procedure UpdateCount;
  public
    RggPoly: TRggPoly;
    WantRotation: Boolean;
    constructor Create(ACaption: string; ACount: Integer);
    procedure Transform; override;
    procedure Draw(g: TCanvas); override;
    procedure Reset;
  end;

  TRggFederLine = class(TRggPolyLine)
  private
    function RotateDegrees(ov: TPoint3D; wi: single): TPoint3D;
  public
    constructor Create(ACaption: string = '');
    procedure Draw(g: TCanvas); override;
  end;

  TRggTriangle = class(TRggElement)
  private
    Poly: array of TPoint;
  public
    Point1: TRggCircle;
    Point2: TRggCircle;
    Point3: TRggCircle;
    constructor Create;
    procedure GetInfo(ML: TStrings); override;
    function GetValid: Boolean; override;
    procedure Draw(g: TCanvas); override;
  end;

  TRggArc = class(TRggElement)
  private
    FTextRadiusFactor: single;
    FRadius: single;
    RadiusF: TPointF;
    procedure SetRadius(const Value: single);
    function GetSweepAngle: single;
  public
    Point1: TRggCircle; // injected
    Point2: TRggCircle;
    Point3: TRggCircle;
    constructor Create(ACaption: string);
    procedure GetInfo(ML: TStrings); override;
    function GetValid: Boolean; override;
    procedure Param1(Delta: single); override;
    procedure Param2(Delta: single); override;
    procedure Draw(g: TCanvas); override;
    property Radius: single read FRadius write SetRadius;
    property SweepAngle: single read GetSweepAngle;
  end;

  TRggLinePair = record
    L1: TRggLine;
    L2: TRggLine;
    SP: TPoint3D;
    function SchnittGG: Boolean;
    function HasCommonPoint: Boolean;
    function CompareCommon: Integer;
    function IsParallel: Boolean;
    function CompareSPZ: Integer;
    procedure ReportData(ML: TStrings);
    function CompareVV(v1, v2: TPoint3D): Integer;
  end;

  TSchnittKKCircleLL = class(TRggCircle)
  public
    Radius1: single;
    Radius2: single;
    L1: TRggLine;
    L2: TRggLine;
    SchnittKK: TSchnittKK;
    Counter: Integer;
    constructor Create(ACaption: string = '');
    destructor Destroy; override;
    procedure GetInfo(ML: TStrings); override;
    function GetValid: Boolean; override;
    procedure Param1(Delta: single); override;
    procedure Param2(Delta: single); override;
    procedure Compute;
    procedure InitRadius;
  end;

  TSchnittKKCircle = class(TRggCircle)
  private
    R1: single;
    R2: single;
    S1: TPoint3D;
    S2: TPoint3D;
    sv: Boolean;
    NeedCalc: Boolean;
    Bem: TBemerkungKK;
    procedure ComputeInternal;
    function GetBem: TBemerkungKK;
    function GetBemerkung: string;
    function Vorhanden: Boolean;
    function GetL1: single;
    function GetL2: single;
  public
    Radius1: single;
    Radius2: single;
    MP1: TRggCircle;
    MP2: TRggCircle;
    Counter: Integer;
    WantS2: Boolean;
    constructor Create(ACaption: string = '');
    procedure GetInfo(ML: TStrings); override;
    function GetValid: Boolean; override;
    procedure Param1(Delta: single); override;
    procedure Param2(Delta: single); override;
    procedure Compute;
    procedure InitRadius;
    procedure Draw(g: TCanvas); override;
    property Status: TBemerkungKK read GetBem;
    property Bemerkung: string read GetBemerkung;
    property SPVorhanden: Boolean read Vorhanden;
    property L1: single read GetL1;
    property L2: single read GetL2;
  end;

var
  GlobalShowCaption: Boolean = False;
  DefaultShowCaption: Boolean = False;

const
  RggPoint3DZero: TRggPoint3D = (X: 0; Y: 0; Z: 0);

implementation

const
  Eps = 0.0001;
  DefaultTextAngle: single = 45 * PI / 180;
  DefaultTextRadius: single = 30.0;

{ TRggPoint3D }

class operator TRggPoint3D.Add(const APoint1, APoint2: TRggPoint3D): TRggPoint3D;
begin
  Result.X := APoint1.X + APoint2.X;
  Result.Y := APoint1.Y + APoint2.Y;
  Result.Z := APoint1.Z + APoint2.Z;
end;

class operator TRggPoint3D.Subtract(const APoint1, APoint2: TRggPoint3D): TRggPoint3D;
begin
  Result.X := APoint1.X - APoint2.X;
  Result.Y := APoint1.Y - APoint2.Y;
  Result.Z := APoint1.Z - APoint2.Z;
end;

function TRggPoint3D.Length: Single;
begin
  result := C.Length;
end;

function TRggPoint3D.Normalize: TRggPoint3D;
begin
  C := C.Normalize;
  result := self;
end;

function TRggPoint3D.Rotate(const AAngle: Single): TRggPoint3D;
var
  Sine, Cosine: Single;
begin
  Sine := sin(AAngle);
  Cosine := cos(AAngle);
  Result.X := X * Cosine - Y * Sine;
  Result.Y := X * Sine + Y * Cosine;
end;

function TRggPoint3D.Angle(const APoint: TRggPoint3D): single;
begin
  Result := Arctan2(Self.Y - APoint.Y, Self.X - APoint.X);
end;

function TRggPoint3D.Distance(const APoint: TRggPoint3D): single;
begin
  Result := (Self - APoint).Length;
end;

class function TRggPoint3D.Zero: TRggPoint3D;
begin
  result.C := TPoint3D.Zero;
end;

class operator TRggPoint3D.Implicit(const APoint: TPoint): TRggPoint3D;
begin
  result.X := APoint.X;
  result.Y := APoint.Y;
  result.Z := 0;
end;

class operator TRggPoint3D.Implicit(const APoint: TRggPoint3D): TPoint;
begin
  result.X := Round(APoint.X);
  result.Y := Round(APoint.Y);
end;

{ TRggElement }

constructor TRggElement.Create;
begin
  Visible := True;
  FStrokeThickness := 3;
  FStrokeColor := TRggColors.Red;
  TypeName := 'Element';
  TextRadius := DefaultTextRadius;
  TextAngle := DefaultTextAngle;
end;

procedure TRggElement.GetInfo(ML: TStrings);
begin
  if Caption = '' then
    ML.Add('Element has no Caption');
end;

function TRggElement.GetValid: Boolean;
begin
  result := Caption <> '';
end;

function TRggElement.GetListCaption: string;
begin
  result := TypeName + ' ' + Caption;
  if IsComputed then
    result := '-- ' + result;
end;

procedure TRggElement.Param1(Delta: single);
begin

end;

procedure TRggElement.Param2(Delta: single);
begin

end;

procedure TRggElement.Param3(Delta: single);
begin

end;

//procedure TRggElement.SetOpacity(const Value: single);
//begin
//  FOpacity := Value;
//end;

procedure TRggElement.SetStrokeColor(const Value: TColor);
begin
  FStrokeColor := Value;
end;

//procedure TRggElement.SetStrokeDash(const Value: TStrokeDash);
//begin
//  FStrokeDash := Value;
//end;

procedure TRggElement.SetStrokeThickness(const Value: Integer);
begin
  FStrokeThickness := Value;
end;

procedure TRggElement.TextOut(g: TCanvas; s: string);
var
  R: TRect;
  x, y: Integer;
  w, h: Integer;
  tf: TTextFormat;
begin
  w := 100;
  h := 12;
  x := Round(TextCenter.X + TextRadius * cos(TextAngle));
  y := Round(TextCenter.Y + TextRadius * sin(TextAngle));
  R := Rect(x - w, y - h, x + w, y + h);

  g.Font.Name := 'Consolas';
  g.Font.Size := 12;
  tf := [tfCenter, tfVerticalCenter];
  g.Brush.Style := bsClear;
  g.Font.Color := Drawing.Colors.TextColor;
  g.TextRect(R, s, tf);
end;

procedure TRggElement.TextOutLeading(g: TCanvas; s: string);
var
  x, y: Integer;
begin
  x := Round(TextCenter.X + TextRadius * cos(TextAngle));
  y := Round(TextCenter.Y + TextRadius * sin(TextAngle));
  g.Font.Name := 'Consolas';
  g.Font.Size := 12;
  g.Brush.Style := bsClear;
  g.Font.Color := Drawing.Colors.TextColor;
  g.TextOut(x, y, s);
end;

procedure TRggElement.Transform;
begin

end;

procedure TRggElement.Param7(Delta: single);
begin
  TextAngle := TextAngle + DegToRad(Delta);
end;

procedure TRggElement.Param8(Delta: single);
begin
  TextRadius := TextRadius + Delta;
end;

procedure TRggElement.Draw(g: TCanvas);
begin
  TextOut(g, Caption);
end;

{ TRggCircle }

constructor TRggCircle.Create;
begin
  inherited;
  Matrix := Matrix3DIdentity;
  TypeName := 'Circle';
  StrokeThickness := 2;
  FRadius := 10;
  Center.X := 100;
  Center.Y := 100;
  ShowCaption := DefaultShowCaption;
end;

constructor TRggCircle.Create(ACaption: string);
begin
  Create;
  Caption := ACaption;
end;

procedure TRggCircle.Draw(g: TCanvas);
var
  R: TRect;
begin
  if not Visible then
    Exit;

  Temp1 := Center + Drawing.FaxPoint3D;

  if Radius > 5 then
  begin
    R := Rect(
      Round(Temp1.X - Radius),
      Round(Temp1.Y - Radius),
      Round(Temp1.X + Radius),
      Round(Temp1.Y + Radius));

    g.Brush.Color := Drawing.Colors.BackgroundColor;
    g.Ellipse(R);

    g.Pen.Color := StrokeColor;
    g.Pen.Width := StrokeThickness;
    g.Ellipse(R);
  end;

  if ShowCaption or GlobalShowCaption then
  begin
    TextCenter := Temp1.P;
    TextOut(g, Caption);
  end;
end;

procedure TRggCircle.Param1(Delta: single);
begin
  OriginalCenter.X := OriginalCenter.X + Delta;
  Center := OriginalCenter;
end;

procedure TRggCircle.Param2(Delta: single);
begin
  OriginalCenter.Y := OriginalCenter.Y + Delta;
  Center := OriginalCenter;
end;

procedure TRggCircle.Param1I(Delta: single);
begin
  Center.X := Center.X + Delta;
  OriginalCenter := Center;
end;

procedure TRggCircle.Param2I(Delta: single);
begin
  Center.Y := Center.Y + Delta;
  OriginalCenter := Center;
end;

procedure TRggCircle.Param3(Delta: single);
begin
  OriginalCenter.Z := OriginalCenter.Z + Delta;
  Center := OriginalCenter;
end;

procedure TRggCircle.Reset;
begin
  Center := OriginalCenter;
  TextAngle := DefaultTextAngle;
  TextRadius := DefaultTextRadius;
end;

procedure TRggCircle.Save;
begin
  OriginalCenter := Center;
end;

procedure TRggCircle.SetRadius(const Value: single);
begin
  FRadius := Value;
end;

procedure TRggCircle.Transform;
begin
  Center.C := Center.C * Matrix;
end;

procedure TRggCircle.TransformI;
begin
  OriginalCenter.C := OriginalCenter.C * Matrix;
end;

procedure TRggCircle.WriteCode(ML: TStrings);
begin
  ML.Add(Format('cr := Find(''%s'');', [Caption]));
  ML.Add(Format('cr.Center.X := %.2f;', [Center.X]));
  ML.Add(Format('cr.Center.Y := %.2f;', [Center.Y]));
  ML.Add(Format('cr.Center.Z := %.2f;', [Center.Z]));

  if TextAngle <> DefaultTextAngle then
    ML.Add(Format('cr.TextAngle := %.2f;', [RadToDeg(TextAngle)]));
  if TextRadius <> DefaultTextRadius then
    ML.Add(Format('cr.TextRadius := %.2f;', [TextRadius]));

  ML.Add('');
end;

function TRggCircle.IsEqual(B: TRggCircle): Boolean;
begin
  result := Center.C = B.Center.C;
end;

function TRggCircle.CompareZ(Q: TRggCircle): Integer;
begin
  if Center.Z > Q.Center.Z then
    result := 1
  else if Center.Z < Q.Center.Z then
    result := -1
  else
    result := 0;
end;

class function TRggCircle.Compare(const Left, Right: TRggCircle): Integer;
begin
  result := Left.CompareZ(Right);
end;

{ TRggLine }

constructor TRggLine.Create(ACaption: string);
begin
  inherited Create;
  TypeName := 'Line';
  Caption := ACaption;
  ShowCaption := DefaultShowCaption;
end;

procedure TRggLine.GetInfo(ML: TStrings);
begin
  inherited;
  if Point1 = nil then
    ML.Add(Caption + '.Point1 = nil');
  if Point2 = nil then
    ML.Add(Caption + '.Point2 = nil');
end;

function TRggLine.GetValid: Boolean;
begin
  result := inherited;
  result := result and (Point1 <> nil);
  result := result and (Point2 <> nil);
end;

procedure TRggLine.Draw(g: TCanvas);
begin
  if not Visible then
    Exit;

  Temp1 := Point1.Center + Drawing.FaxPoint3D;
  Temp2 := Point2.Center + Drawing.FaxPoint3D;

  g.Pen.Width := StrokeThickness;
  g.Pen.Color := StrokeColor;
  g.MoveTo(Round(Temp1.X), Round(Temp1.Y));
  g.LineTo(Round(Temp2.X), Round(Temp2.Y));

  if ShowCaption or GlobalShowCaption then
  begin
    TextCenter := Temp1.P + (Temp2.P - Temp1.P) * 0.5;
    TextOut(g, Caption);
  end;
end;

function TRggLine.GetLength: single;
begin
  result := (Point2.Center.C - Point1.Center.C).Length;
end;

procedure TRggLine.Param1(Delta: single);
var
  u, v: TPointF;
begin
  { change length of line element, change Point2 }
  u := V2 * (1 + Delta / 100);
  v := Point1.Center.P + u;

  Point2.OriginalCenter.P := v;
  Point2.Center := Point2.OriginalCenter;
end;

procedure TRggLine.Param2(Delta: single);
var
  alpha: single;
begin
  { rotate line around Point2 }
  alpha := DegToRad(-Delta / 5);

  Point2.OriginalCenter.P := Point1.Center.P + V2.Rotate(alpha);
  Point2.Center := Point2.OriginalCenter;
end;

function TRggLine.V2: TPointF;
begin
  result := Point2.Center.P - Point1.Center.P;
end;

function TRggLine.V3: TPoint3D;
begin
  result := Point2.Center.C - Point1.Center.C;
end;

function TRggLine.IsTotallyAbove(Other: TRggLine): Boolean;
begin
  result :=
    (Point1.Center.Z > Other.Point1.Center.Z) and
    (Point1.Center.Z > Other.Point2.Center.Z) and
    (Point2.Center.Z > Other.Point1.Center.Z) and
    (Point2.Center.Z > Other.Point2.Center.Z);
end;

function TRggLine.IsTotallyBelow(Other: TRggLine): Boolean;
begin
  result :=
    (Point1.Center.Z < Other.Point1.Center.Z) and
    (Point1.Center.Z < Other.Point2.Center.Z) and
    (Point2.Center.Z < Other.Point1.Center.Z) and
    (Point2.Center.Z < Other.Point2.Center.Z);
end;

function TRggLine.IsSame(Other: TRggLine): Boolean;
begin
  result := False;
  if Point1.IsEqual(Other.Point1) and Point2.IsEqual(Other.Point2) then
    result := True
  else if Point1.IsEqual(Other.Point2) and Point2.IsEqual(Other.Point1) then
    result := True;
end;

function TRggLine.ComputeSPZ(SP: TPoint3D): single;
var
  vSP: TPoint3D;
  vAB: TPoint3D;

  vABxy: TPoint3D;
  vSPxy: TPoint3D;
  lengthABxy, lengthSPxy: double;
  RatioSPtoAB, g: double;
begin
  result := (Point1.Center.Z + Point2.Center.Z) / 2;

  vSP := SP - Point1.Center.C;
  vAB := Point2.Center.C - Point1.Center.C;

  vABxy := Point3D(vAB.X, vAB.Y, 0);
  lengthABxy := vABxy.Length;

  vSPxy := Point3D(vSP.X, vSP.Y, 0);
  lengthSPxy := vSPxy.Length;

  if lengthABxy < Eps then
  begin
    Exit;
  end;

  RatioSPtoAB := lengthSPxy / lengthABxy;

  g := RatioSPtoAB;

  if Sign(vAB.X) <> Sign(vSP.X) then
    g := -RatioSPtoAB;

  if Abs(g) > 10000 then
  begin
    { does not come in here }
    result := Point1.Center.Z;
    Exit;
  end;

  result := Point1.Center.Z + g * vAB.Z;
end;

procedure TRggLine.ReportData(ML: TStrings);
  procedure AddPoint(LN, PN: string; P: TPoint3D);
  begin
    ML.Add(Format('%s [%s] = (%.2f, %.2f, %.2f)', [PN, LN, P.X, P.Y, P.Z]));
  end;
begin
  AddPoint(Caption, 'A', Point1.Center.C);
  AddPoint(Caption, 'B', Point2.Center.C);
end;

class procedure TRggLine.ResetCounter;
begin
  CounterLeftNil := 0;
  CounterRightNil := 0;
  CounterHardcodedAbove := 0;
  CounterHardcodedBelow := 0;
  CounterSame := 0;
  CounterTotallyAbove := 0;
  CounterTotallyBelow := 0;
  CounterCommon := 0;
  CounterParallel := 0;
  CounterSPZ := 0;
  CounterZero := 0;
end;

class function TRggLine.CounterSum: Integer;
begin
  result :=
    CounterLeftNil +
    CounterRightNil +
    CounterHardCodedAbove +
    CounterHardCodedBelow +
    CounterSame +
    CounterTotallyAbove +
    CounterTotallyBelow +
    CounterCommon +
    CounterParallel +
    CounterSPZ;
end;

class function TRggLine.Compare(const Left, Right: TRggLine): Integer;
var
  LP: TRggLinePair;
  r: Integer;
begin
  if Left = nil then
  begin
    Left.Bemerkung := ccNil;
    Inc(CounterLeftNil);
    result := 0;
    Exit;
  end;

  if Right = nil then
  begin
    Left.Bemerkung := ccNil;
    Inc(CounterRightNil);
    result := 0;
    Exit;
  end;

  LP.SP := TPoint3D.Zero;
  LP.L1 := Left;
  LP.L2 := Right;

  if False then

  else if LP.L1.IsSame(LP.L2) then
  begin
    Inc(CounterSame);
    Left.Bemerkung := ccTotallySame;
    r := 0;
    Dec(CounterZero); // compensate for Inc below
  end

  else if LP.L1.IsTotallyAbove(LP.L2) then
  begin
    Inc(CounterTotallyAbove);
    Left.Bemerkung := ccTotallyAbove;
    r := 1;
  end

  else if LP.L1.IsTotallyBelow(LP.L2) then
  begin
    Inc(CounterTotallyBelow);
    Left.Bemerkung := ccTotallyBelow;
    r := -1;
  end

  else if LP.HasCommonPoint then
  begin
    Inc(CounterCommon);
    r := LP.CompareCommon;
    case r of
      0: Left.Bemerkung := ccCommonSame;
      1: Left.Bemerkung := ccCommonAbove;
      -1: Left.Bemerkung := ccCommonBelow;
      else
        Left.Bemerkung := ccCommonNone;
    end;
  end

  { As a side effect, this call to IsParallel will set SP }
  else if LP.IsParallel then
  begin
    Inc(CounterParallel);
    Left.Bemerkung := ccParallel;
    r := 0;
  end

  else
  begin
    Inc(CounterSPZ);
    r := LP.CompareSPZ;
    case r of
      0: Left.Bemerkung := ccSame;
      1: Left.Bemerkung := ccAbove;
      -1: Left.Bemerkung := ccBelow;
      else
        Left.Bemerkung := ccNone;
    end;
  end;

  if r = 0 then
  begin
    Inc(CounterZero);
  end;

  result := r;
end;

{ TRggTriangle }

procedure TRggTriangle.GetInfo(ML: TStrings);
begin
  inherited;
  if Point1 = nil then
    ML.Add(Caption + '.Point1 = nil');
  if Point2 = nil then
    ML.Add(Caption + '.Point2 = nil');
  if Point3 = nil then
    ML.Add(Caption + '.Point3 = nil');
end;

function TRggTriangle.GetValid: Boolean;
begin
  result := inherited;
  result := result and (Point1 <> nil);
  result := result and (Point2 <> nil);
  result := result and (Point3 <> nil);
end;

constructor TRggTriangle.Create;
begin
  inherited;
  TypeName := 'Triangle';
  SetLength(Poly, 3);
end;

procedure TRggTriangle.Draw(g: TCanvas);
begin
  Poly[0] := Point1.Center + Drawing.FaxPoint3D;
  Poly[1] := Point2.Center + Drawing.FaxPoint3D;
  Poly[2] := Point3.Center + Drawing.FaxPoint3D;
  g.Brush.Color := StrokeColor;
  g.Polygon(Poly);
end;

{ TRggArc }

function TRggArc.GetValid: Boolean;
begin
  result := inherited;
  result := result and (Point1 <> nil);
  result := result and (Point2 <> nil);
  result := result and (Point3 <> nil);
end;

constructor TRggArc.Create(ACaption: string);
begin
  inherited Create;
  FTextRadiusFactor := 1.2;
  Caption := ACaption;
  TypeName := 'Arc';
  Radius := 50;
  StrokeThickness := 2;
  ShowCaption := True;
end;

procedure TRggArc.Draw(g: TCanvas);
var
  Angle2, Angle3: single;
  startAngle: single;
  sweepAngle: single;
  s: string;
  P0, P1, P2, P3, P4: TPoint;
  r: Integer;
  startAngleR: single;
  sweepAngleR: single;
begin
  Temp1 := Point1.Center + Drawing.FaxPoint3D;
  Temp2 := Point2.Center + Drawing.FaxPoint3D;
  Temp3 := Point3.Center + Drawing.FaxPoint3D;

  Angle2 := RadToDeg(Temp2.P.Angle(Temp1.P));
  Angle3 := RadToDeg(Temp3.P.Angle(Temp1.P));

  startAngle := Angle3;
  sweepAngle := (Angle2 - Angle3);

  if sweepAngle < -180 then
  begin
    sweepAngle := sweepAngle + 360;
  end;

  if sweepAngle > 180 then
  begin
    sweepAngle := sweepAngle - 360;
  end;

  s := Caption;

  g.Pen.Color := StrokeColor;
  g.Pen.Width := StrokeThickness;
  { FMX: g.DrawArc(Point1.Center.P, RadiusF, startAngle, sweepAngle, 1.0); }

  { VCL:
    Mit der Methode Arc können Sie mit dem aktuellen Pen-Objekt einen elliptischen Bogen zeichnen.
    Der Bogen läuft durch den Umfang einer von den Koordinaten (X1,Y1) und (X2,Y2) begrenzten Ellipse hindurch.

    Der Bogen wird gegen den Uhrzeigersinn gezeichnet und folgt vom Anfangs- bis zum Endpunkt dem Umfang der Ellipse.

    Der Anfangspunkt ist der Schnittpunkt zwischen der Ellipse und einer Linie,
    die durch den Mittelpunkt der Ellipse und dem Punkt (X3,Y3) definiert ist.

    Der Endpunkt ist der Schnittpunkt zwischen der Ellipse und einer Linie,
    die durch den Mittelpunkt der Ellipse und den Punkt (X4,Y4) definiert ist.
  }

  P0.X := Round(Temp1.X);
  P0.Y := Round(Temp1.Y);

  r := Round(Radius);

  P1.X := P0.X - r;
  P1.Y := P0.Y - r;
  P2.X := P0.X + r;
  P2.Y := P0.Y + r;

  startAngleR := DegToRad(startAngle);
  sweepAngleR := DegToRad(sweepAngle);

  if sweepAngle < 0 then
  begin
    P3.X := Round(P0.X + cos(startAngleR) * 100);
    P3.Y := Round(P0.Y + sin(startAngleR) * 100);

    P4.X := Round(P0.X + cos(startAngleR + sweepAngleR) * 100);
    P4.Y := Round(P0.Y + sin(startAngleR + sweepAngleR) * 100);
  end
  else
  begin
    P4.X := Round(P0.X + cos(startAngleR) * 100);
    P4.Y := Round(P0.Y + sin(startAngleR) * 100);

    P3.X := Round(P0.X + cos(startAngleR + sweepAngleR) * 100);
    P3.Y := Round(P0.Y + sin(startAngleR + sweepAngleR) * 100);
  end;

  g.Arc(P1.X, P1.Y, P2.X, P2.Y, P3.X, P3.Y, P4.X, P4.Y);

  if ShowCaption or GlobalShowCaption then
  begin
    TextAngle := DegToRad(startAngle + sweepAngle / 2);
    TextRadius := Radius * FTextRadiusFactor;
    TextCenter := Temp1.P;
    TextOut(g, s);
  end;
end;

procedure TRggArc.GetInfo(ML: TStrings);
begin
  inherited;
  if Point1 = nil then
    ML.Add(Caption + '.Point1 = nil');
  if Point2 = nil then
    ML.Add(Caption + '.Point2 = nil');
  if Point3 = nil then
    ML.Add(Caption + '.Point3 = nil');
end;

function TRggArc.GetSweepAngle: single;
var
  Angle2, Angle3: single;
  sweepAngle: single;
begin
  Angle2 := RadToDeg(Point2.Center.P.Angle(Point1.Center.P));
  Angle3 := RadToDeg(Point3.Center.P.Angle(Point1.Center.P));

  sweepAngle := (Angle2 - Angle3);
  result := sweepAngle;
end;

procedure TRggArc.SetRadius(const Value: single);
begin
  FRadius := Value;
  RadiusF.X := FRadius;
  RadiusF.Y := FRadius;
end;

procedure TRggArc.Param1(Delta: single);
begin
  Radius := Radius + Delta;
end;

procedure TRggArc.Param2(Delta: single);
begin
  FTextRadiusFactor := FTextRadiusFactor + Delta / 50;
end;

{ TRggLagerLine }

procedure TRggLagerLine.Draw(g: TCanvas);
begin
  inherited;
  DrawLager(g, (Drawing.FaxPoint3D + Point1.Center).P, True);
  DrawLager(g, (Drawing.FaxPoint3D + Point2.Center).P, False);
end;

procedure TRggLagerLine.DrawLager(g: TCanvas; P: TPointF; FestLager: Boolean);
var
  Angle: single;
  l: single;
  d: single;

  TempA: TPointF;
  TempB: TPointF;
  TempC: TPointF;
  TempD: TPointF;

  TempE: TPointF;
  TempF: TPointF;

  o: TPointF;
  TempP: array of TPoint;
  i: Integer;
begin
  Angle := DegToRad(30);
  l := 30;

  TempA.X := cos(Angle) * Point1.FRadius;
  TempA.Y := -sin(Angle) * Point1.FRadius;
  TempB.X := TempA.X + sin(Angle) * l;
  TempB.Y := TempA.Y + cos(Angle) * l;
  TempC.X := -TempB.X;
  TempC.Y := TempB.Y;
  TempD.X := -TempA.X;
  TempD.Y := TempA.Y;
  o.X := P.X;
  o.Y := P.Y;

  TempA.Offset(o);
  TempB.Offset(o);
  TempC.Offset(o);
  TempD.Offset(o);

  SetLength(TempP, 4);
  TempP[0] := Point(Round(TempA.X), Round(TempA.Y));
  TempP[1] := Point(Round(TempB.X), Round(TempB.Y));
  TempP[2] := Point(Round(TempC.X), Round(TempC.Y));
  TempP[3] := Point(Round(TempD.X), Round(TempD.Y));

  g.Brush.Color := Drawing.Colors.BackgroundColor;
  g.Pen.Color := TRggColors.Gray;
  g.Pen.Width := 3;
  g.Polygon(TempP);

  if not FestLager then
  begin
    o.X := 0;
    o.Y := 5;
    TempB.Offset(o);
    TempC.Offset(o);
    { g.DrawLine(TempB, TempC, 1.0); }
    g.MoveTo(Round(TempB.X), Round(TempB.Y));
    g.LineTo(Round(TempC.X), Round(TempC.Y));
  end;

  TempE := TempC;
  TempF.X := TempE.X - sin(Angle) * l * 0.5;
  TempF.Y := TempE.Y + cos(Angle) * l * 0.5;

  d := (TempB - TempC).Length / 3;

  o.X := -0.4 * d;
  o.Y := 0;
  TempE.Offset(o);
  TempF.Offset(o);

  o.X := d;
  o.Y := 0;
  for i := 1 to 3 do
  begin
    TempE.Offset(o);
    TempF.Offset(o);
    { g.DrawLine(TempE, TempF, Opacity); }
    g.MoveTo(Round(TempE.X), Round(TempE.Y));
    g.LineTo(Round(TempF.X), Round(TempF.Y));
  end;
end;

{ TRggLinePair }

function TRggLinePair.CompareVV(v1, v2: TPoint3D): Integer;
var
  m1, m2: TPoint3D;
  r: single;
begin
  m1 := v1.Normalize;
  m2 := v2.Normalize;
  r := m2.Z - m1.Z;
  if r > 0 then
    result := -1
  else if r < 0 then
    result := 1
  else
    result := 0;
end;

procedure TRggLinePair.ReportData(ML: TStrings);
  procedure AddPoint(LN, PN: string; P: TPoint3D);
  begin
    ML.Add(Format('%s [%s] = (%.2f, %.2f, %.2f)', [PN, LN, P.X, P.Y, P.Z]));
  end;
begin
  AddPoint(L1.Caption, 'A', L1.Point1.Center.C);
  AddPoint(L1.Caption, 'B', L1.Point2.Center.C);
  AddPoint(L2.Caption, 'C', L2.Point1.Center.C);
  AddPoint(L2.Caption, 'D', L2.Point2.Center.C);
end;

function TRggLinePair.CompareSPZ: Integer;
var
  za, zb, dz: single;
begin
  za := L1.ComputeSPZ(SP);
  zb := L2.ComputeSPZ(SP);

  dz := zb - za;

  if dz > 0 then
    result := -1
  else if dz < 0 then
    result := 1
  else
    result := 0;
end;

function TRggLinePair.HasCommonPoint: Boolean;
begin
  result :=
    (L1.Point1.Center.C = L2.Point1.Center.C) or
    (L1.Point1.Center.C = L2.Point2.Center.C) or
    (L1.Point2.Center.C = L2.Point1.Center.C) or
    (L1.Point2.Center.C = L2.Point2.Center.C);
end;

function TRggLinePair.IsParallel: Boolean;
begin
  result := not SchnittGG;
end;

function TRggLinePair.CompareCommon: Integer;
var
  v1, v2: TPoint3D;
begin
  result := 0;
  if L1.Point1.IsEqual(L2.Point1) then
  begin
    v1 := L1.Point2.Center.C - L1.Point1.Center.C;
    v2 := L2.Point2.Center.C - L2.Point1.Center.C;
    result := CompareVV(v1, v2);
  end
  else if L1.Point1.IsEqual(L2.Point2) then
  begin
    v1 := L1.Point2.Center.C - L1.Point1.Center.C;
    v2 := L2.Point1.Center.C - L2.Point2.Center.C;
    result := CompareVV(v1, v2);
  end
  else if L1.Point2.IsEqual(L2.Point1) then
  begin
    v1 := L1.Point1.Center.C - L1.Point2.Center.C;
    v2 := L2.Point2.Center.C - L2.Point1.Center.C;
    result := CompareVV(v1, v2);
  end
  else if L1.Point2.IsEqual(L2.Point2) then
  begin
    v1 := L1.Point1.Center.C - L1.Point2.Center.C;
    v2 := L2.Point1.Center.C - L2.Point2.Center.C;
    result := CompareVV(v1, v2);
  end;
end;

function TRggLinePair.SchnittGG: Boolean;
var
  a1, a2: single;
  sx, sz, x1, z1, x3, z3: single;
  Quotient: single;
  Fall: TBemerkungGG;
begin
  result := True;
  Fall := ggOK;

  a1 := 0;
  a2 := 0;
  sx := 0;
  sz := 0;

  x1 := L1.Point1.Center.X;
  z1 := L1.Point1.Center.Z;
  x3 := L2.Point1.Center.X;
  z3 := L2.Point1.Center.X;

  Quotient := L1.Point2.Center.X - L1.Point1.Center.X;
  if abs(Quotient) > 0.001 then
    a1 := (L1.Point2.Center.Z - L1.Point1.Center.Z) / Quotient
  else
    Fall := g1Vertical;

  Quotient := L2.Point2.Center.X - L2.Point1.Center.X;
  if abs(Quotient) > 0.001 then
    a2 := (L2.Point2.Center.Z - L1.Point1.Center.Z) / Quotient
  else
    Fall := g2Vertical;

  if (Fall = ggOK) and (abs(a2-a1) < 0.001) then
    Fall := ggParallel;

  case Fall of
    ggParallel:
    begin
      sx := 0;
      sz := 0;
      result := False;
    end;

    ggOK:
      begin
        sx := (-a1 * x1 + a2 * x3 - z3 + z1) / (-a1 + a2);
        sz := (-a2 * a1 * x1 + a2 * z1 + a2 * x3 * a1 - z3 * a1) / (-a1 + a2);
      end;

    g1Vertical:
      begin
        sz := a2 * x1 - a2 * x3 + z3;
        sx := x1;
      end;

    g2Vertical:
      begin
        sz := a1 * x3 - a1 * x1 + z1;
        sx := x3;
      end;
  end;

  SP.X := sx;
  SP.Y := 0;
  SP.Z := sz;
end;

{ TRggLabel }

constructor TRggLabel.Create;
begin
  inherited;
  TypeName := 'Label';
  Position.X := 20;
  Position.Y := 20;
end;

procedure TRggLabel.Draw(g: TCanvas);
var
  R: TRect;
  x, y: Integer;
  w, h: Integer;
  tf: TTextFormat;
begin
  TextCenter := Position;

  x := Round(TextCenter.X);
  y := Round(TextCenter.Y);

  g.Pen.Color := Drawing.Colors.LabelColor;
  g.Brush.Style := TBrushStyle.bsClear;
  g.Font.Name := 'Consolas';
  g.Font.Size := 12;

  if IsMemoLabel then
  begin
    g.Font.Color := TRggColors.Plum;
    w := 520;
    h := 500;
    R := Rect(x, y, x + w, y + h);
    tf := [tfLeft, tfTop];
    g.TextRect(R, Text, tf);
  end
  else
  begin
    g.TextOut(x, y, Text);
  end;
end;

function TRggLabel.GetListCaption: string;
begin
  result := inherited;
  result := '-- ' + result;
end;

{ TRggPolyLine }

constructor TRggPolyLine.Create(ACaption: string = '');
begin
  inherited;
  TypeName := 'PolyLine';
end;

constructor TRggPolyLine.Create(ACaption: string; ACount: Integer);
begin
  Create(ACaption);
  if (ACount > 2) and (ACount < 202) then
  begin
    FCount := ACount;
    SetLength(Poly, Count);
    SetLength(TransformedPoly, Count);
  end;
end;

procedure TRggPolyLine.Draw(g: TCanvas);
var
  i: Integer;
begin
  Temp1 := Point1.Center + Drawing.FaxPoint3D;
  Temp2 := Point2.Center + Drawing.FaxPoint3D;

  if not ShowPoly then
    inherited
  else
  begin
    g.Pen.Width := Round(StrokeThickness);
    g.Pen.Color := StrokeColor;

    if Drawing.WantOffset then
    begin
      for i := 0 to Length(Poly) - 1 do
      begin
        TransformedPoly[i].X := Round(Poly[i].X + Drawing.FaxPoint3D.X);
        TransformedPoly[i].Y := Round(Poly[i].Y + Drawing.FaxPoint3D.Y);
      end;
      g.PolyLine(TransformedPoly);
    end
    else
      g.Polyline(Poly);

    DrawText(g);
  end;
end;

procedure TRggPolyLine.DrawText(g: TCanvas);
begin
  if ShowCaption or GlobalShowCaption then
  begin
    TextCenter := Temp1.P + (Temp2.P - Temp1.P) * 0.5;
    TextOut(g, Caption);
  end;
end;

{ TRggPolyLine }

constructor TRggPolyLine3D.Create(ACaption: string; ACount: Integer);
begin
  inherited;
  TypeName := 'PolyLine3D';
  UpdateCount;
end;

procedure TRggPolyLine3D.UpdateCount;
var
  l: Integer;
begin
  l := Length(Poly);
  if Length(RggPoly) <> l then
    SetLength(RggPoly, l);
  if Length(TransformedPoly) <> l then
    SetLength(TransformedPoly, l);
end;

procedure TRggPolyLine3D.Draw(g: TCanvas);
var
  i: Integer;
begin
  if not Visible then
    Exit;

  if not WantRotation then
  begin
    inherited;
    Exit;
  end;

  if not ShowPoly then
    inherited
  else
  begin
    g.Pen.Width := StrokeThickness;
    g.Pen.Color := StrokeColor;
    for i := 0 to Length(RggPoly) - 1 do
    begin
      TransformedPoly[i].X := Round(RggPoly[i].X + Drawing.FaxPoint3D.X);
      TransformedPoly[i].Y := Round(RggPoly[i].Y + Drawing.FaxPoint3D.Y);
    end;
    g.PolyLine(TransformedPoly);
    DrawText(g);
  end;
end;

procedure TRggPolyLine3D.Transform;
var
  i: Integer;
begin
  if not WantRotation then
    Exit;

  Assert(FCount = Length(RggPoly));

  for i := 0 to FCount - 1 do
  begin
    RggPoly[i].C := RggPoly[i].C * TRggCircle.Matrix;
  end;
end;

procedure TRggPolyLine3D.Reset;
var
  i: Integer;
  l: Integer;
begin
  l := Length(RggPoly);
  for i := 0 to l - 1 do
  begin
    RggPoly[i].P := Poly[i];
    RggPoly[i].Z := 0;
  end;
end;

{ TSchnittKKCircleLL }

constructor TSchnittKKCircleLL.Create(ACaption: string);
begin
  inherited;
  TypeName := 'SKK Circle LL';
  IsComputed := True;
  Radius1 := 100;
  Radius2 := 100;
  SchnittKK := TSchnittKK.Create;
end;

destructor TSchnittKKCircleLL.Destroy;
begin
  SchnittKK.Free;
  inherited;
end;

procedure TSchnittKKCircleLL.InitRadius;
begin
  Radius1 := L1.LineLength;
  Radius2 := L2.LineLength;
end;

procedure TSchnittKKCircleLL.Param1(Delta: single);
begin
  Radius1 := Radius1 + Delta;
end;

procedure TSchnittKKCircleLL.Param2(Delta: single);
begin
  Radius2 := Radius2 + Delta;
end;

procedure TSchnittKKCircleLL.GetInfo(ML: TStrings);
begin
  inherited;
  if L1 = nil then
    ML.Add(Caption + '.L1 = nil');
  if L2 = nil then
    ML.Add(Caption + '.L2 = nil');
end;

function TSchnittKKCircleLL.GetValid: Boolean;
begin
  result := inherited;
  result := result and (L1 <> nil);
  result := result and (L2 <> nil);
end;

procedure TSchnittKKCircleLL.Compute;
begin
  Inc(Counter);

  SchnittKK.SchnittEbene := seXY;
  SchnittKK.Radius1 := Radius1;
  SchnittKK.Radius2 := Radius2;
  SchnittKK.MittelPunkt1 := L1.Point1.Center.C;
  SchnittKK.MittelPunkt2 := L2.Point1.Center.C;
  Center.C := SchnittKK.SchnittPunkt2;

  L1.Point2.OriginalCenter.C := Center.C;
  L2.Point2.OriginalCenter.C := Center.C;

  L1.Point2.Center.C := Center.C;
  L2.Point2.Center.C := Center.C;
end;

{ TSchnittKKCircle }

constructor TSchnittKKCircle.Create(ACaption: string);
begin
  inherited;
  TypeName := 'SKK Circle';
  IsComputed := True;
  Radius1 := 100;
  Radius2 := 100;
  NeedCalc := True;
  WantS2 := True;
end;

procedure TSchnittKKCircle.InitRadius;
begin
  Radius1 := (Center.C - MP1.Center.C).Length;
  Radius2 := (Center.C - MP2.Center.C).Length;
end;

function TSchnittKKCircle.GetBem: TBemerkungKK;
begin
  if NeedCalc = True then
    ComputeInternal;
  result := Bem;
end;

function TSchnittKKCircle.GetBemerkung: string;
begin
  if NeedCalc = True then
    ComputeInternal;
  case Bem of
    bmKonzentrisch:
      result := 'concentric circles';
    bmZwei:
      result := 'two intersections';
    bmEntfernt:
      result := 'two distant circles';
    bmEinerAussen:
      result := 'touching outside';
    bmEinerK1inK2:
      result := 'touching inside, C1 in C2';
    bmEinerK2inK1:
      result := 'touching inside, C2 in C1';
    bmK1inK2:
      result := 'C1 inside C2';
    bmK2inK1:
      result := 'C2 inside C1';
    bmRadiusFalsch:
      result := 'invalid radius';
  end;
end;

procedure TSchnittKKCircle.GetInfo(ML: TStrings);
begin
  inherited;
  if MP1 = nil then
    ML.Add(Caption + '.MP1 = nil');
  if MP2 = nil then
    ML.Add(Caption + '.MP2 = nil');
end;

function TSchnittKKCircle.GetL1: single;
begin
  if NeedCalc then
    ComputeInternal;
  result := (Center.C - MP1.Center.C).Length;
end;

function TSchnittKKCircle.GetL2: single;
begin
  if NeedCalc then
    ComputeInternal;
  result := (Center.C - MP2.Center.C).Length;
end;

function TSchnittKKCircle.GetValid: Boolean;
begin
  result := inherited;
  result := result and (MP1 <> nil);
  result := result and (MP2 <> nil);
end;

procedure TSchnittKKCircle.Param1(Delta: single);
begin
  Radius1 := Radius1 + Delta;
  NeedCalc := True;
end;

procedure TSchnittKKCircle.Param2(Delta: single);
begin
  Radius2 := Radius2 + Delta;
  NeedCalc := True;
end;

function TSchnittKKCircle.Vorhanden: Boolean;
begin
  if NeedCalc = True then
    ComputeInternal;
  result := sv;
end;

procedure TSchnittKKCircle.ComputeInternal;
var
  a, b, h1, h2, p, q, Entfernung: single;
  DeltaX, DeltaY: single;
  AbsDeltaX, AbsDeltaY: single;
  DeltaNullx, DeltaNully: Boolean;
  M1M2, M1S1, KreuzProd: TPoint3D;
  M1, M2, SP: TPoint3D;
begin
  R1 := Radius1;
  R2 := Radius2;
  M1 := MP1.Center.C;
  M2 := MP2.Center.C;

  NeedCalc := False;
  sv := False;

  S1 := TPoint3D.Zero;
  S2 := TPoint3D.Zero;

  { Radien sollen größer Null sein }
  if (R1 <= 0) or (R2 <= 0) then
  begin
    Bem := bmRadiusFalsch;
    Exit;
  end;

  DeltaX := M2.X - M1.X;
  DeltaY := M2.Y - M1.Y;
  DeltaNullx := DeltaX = 0;
  DeltaNully := DeltaY = 0;
  AbsDeltaX := abs(DeltaX);
  AbsDeltaY := abs(DeltaY);

  { Spezialfall konzentrische Kreise }
  if DeltaNullx and DeltaNully then
  begin
    Bem := bmKonzentrisch;
    Exit;
  end;

  h1 := (R1 * R1 - R2 * R2) + (M2.X * M2.X - M1.X * M1.X) + (M2.Y * M2.Y - M1.Y * M1.Y);

  { Rechnung im Normalfall }

  if AbsDeltaY > AbsDeltaX then
  begin
    a := - DeltaX / DeltaY;
    b := h1 / (2 * DeltaY);
    p := 2 * (a * b - M1.X - a * M1.Y) / (1 + a * a);
    q := (M1.X * M1.X + b * b - 2 * b * M1.Y + M1.Y * M1.Y - R1 * R1) / (1 + a * a);
    h2 := p * p / 4 - q;
    if h2 >= 0 then
    begin
      h2 := sqrt(h2);
      S1.X := -p / 2 + h2;
      S2.X := -p / 2 - h2;
      S1.Y := a * S1.X + b;
      S2.Y := a * S2.X + b;
      sv := True;
    end;
  end
  else
  begin
    a := - DeltaY / DeltaX;
    b := h1 / (2 * DeltaX);
    p := 2 * (a * b - M1.Y - a * M1.X) / (1 + a * a);
    q := (M1.Y * M1.Y + b * b - 2 * b * M1.X + M1.X * M1.X - R1 * R1) / (1 + a * a);
    h2 := p * p / 4 - q;
    if h2 >= 0 then
    begin
      h2 := sqrt(h2);
      S1.Y := -p / 2 + h2;
      S2.Y := -p / 2 - h2;
      S1.X := a * S1.Y + b;
      S2.X := a * S2.Y + b;
      sv := True;
    end;
  end;

  Entfernung := (M2 - M1).Length;

  if sv = False then
  begin
    if Entfernung > R1 + R2 then
      Bem := bmEntfernt
    else if Entfernung + R1 < R2 then
      Bem := bmK1inK2
    else if Entfernung + R2 < R1 then
      Bem := bmK2inK1;
    Exit;
  end;

  if sv = True then
  begin
    Bem := bmZwei;
    if Entfernung + R1 = R2 then
      Bem := bmEinerK1inK2
    else if Entfernung + R2 = R1 then
      Bem := bmEinerK2inK1
    else if Entfernung = R1 + R2 then
      Bem := bmEinerAussen;
  end;

  { den "richtigen" SchnittPunkt ermitteln }
  if Bem = bmZwei then
  begin
    M1M2 := M2 - M1;
    M1S1 := S1 - M1;
    KreuzProd := M1M2.CrossProduct(M1S1);
    if KreuzProd.Z < 0 then
    begin
      SP := S2;
      S2 := S1;
      S1 := SP;
    end;
  end;
end;

procedure TSchnittKKCircle.Compute;
begin
//  if NeedCalc then
    ComputeInternal;
  if WantS2 then
    Center.C := S2
  else
    Center.C := S1;
end;

procedure TSchnittKKCircle.Draw(g: TCanvas);
begin
  Temp1 := MP1.Center + Drawing.FaxPoint3D;
  Temp2 := MP2.Center + Drawing.FaxPoint3D;
  Temp3 := Center + Drawing.FaxPoint3D;

  g.Pen.Width := StrokeThickness;
  g.Pen.Color := StrokeColor;

  g.MoveTo(Round(Temp1.X), Round(Temp1.Y));
  g.LineTo(Round(Temp3.X), Round(Temp3.Y));

  g.MoveTo(Round(Temp2.X), Round(Temp2.Y));
  g.LineTo(Round(Temp3.X), Round(Temp3.Y));

  inherited;
end;

{ TRggParam }

constructor TRggParam.Create;
begin
  inherited;
  TypeName := 'Param';
  FScale := 1.0;
  FOriginalValue := 400;
  FValue := FOriginalValue;
  StartPoint := TPointF.Create(10, 10);
  StrokeThickness := 2;
  StrokeColor := TRggColors.Gray;
  ShowCaption := True;
end;

procedure TRggParam.Reset;
begin
  FValue := FOriginalValue;
end;

procedure TRggParam.Save;
begin
  FOriginalValue := FValue;
end;

procedure TRggParam.SetBaseValue(const Value: single);
begin
  FBaseValue := Value;
end;

procedure TRggParam.SetScale(const Value: single);
begin
  FScale := Value;
end;

procedure TRggParam.SetValue(const Value: single);
begin
  FValue := Value;
end;

procedure TRggParam.Param1(Delta: single);
begin
  FValue := FValue + Delta;
end;

procedure TRggParam.Draw(g: TCanvas);
var
  EndPoint: TPointF;
begin
  EndPoint.Y := StartPoint.Y;
  EndPoint.X := StartPoint.X + FOriginalValue;

  g.Pen.Width := 5;
  g.Pen.Color := clYellow;
  g.MoveTo(Round(StartPoint.X), Round(StartPoint.Y));
  g.LineTo(Round(EndPoint.X), Round(EndPoint.Y));

  EndPoint.X := StartPoint.X + FValue;
  g.Pen.Width := 1;
  g.Pen.Color := clNavy;
  g.MoveTo(Round(StartPoint.X), Round(StartPoint.Y));
  g.LineTo(Round(EndPoint.X), Round(EndPoint.Y));

  if ShowCaption or GlobalShowCaption then
  begin
    TextCenter := StartPoint;
    TextCenter.Offset(20, -15);
    TextOutLeading(g, Text);
  end;
end;

function TRggParam.GetRelativeValue: single;
begin
  result := FBaseValue + (Value - 400) * FScale;
end;

{ TRggRotaLine }

constructor TRggRotaLine.Create(ACaption: string);
begin
  inherited Create(ACaption);
  TypeName := 'RotaLine';
end;

procedure TRggRotaLine.Param1(Delta: single);
begin
  { swap Params, do inherited Param 2}
  inherited Param2(Delta);
end;

procedure TRggRotaLine.Param2(Delta: single);
begin
  { swap Params, do inherited Param 1}
  inherited Param1(Delta);
end;

{ TFederLine }

constructor TRggFederLine.Create(ACaption: string);
begin
  inherited Create(ACaption, 8);
end;

procedure TRggFederLine.Draw(g: TCanvas);
var
  i: Integer;
  l: single;
  a: single;
  b: single;
  vp, vq: TPointF;
  vn, wn: TPointF;
  v, w: TPointF;

  p0, p1: TPointF;
  vx, vy: TPoint3D;
begin
  Temp1 := Point1.Center + Drawing.FaxPoint3D;
  Temp2 := Point2.Center + Drawing.FaxPoint3D;

  vp := Temp1.P;
  vq := Temp2.P;

  v := vq - vp;

  vn := v.Normalize;
  vx := TPoint3D.Create(vn.X, vn.Y, 0);
  vy := RotateDegrees(vx, 90);
  wn := TPointF.Create(vy.X, vy.Y);

  l := v.Length;
  a := l / 3 / 8;
  b := 20.0;

  Poly[0] := Point(Round(vp.X), Round(vp.Y));

  v := vn * 8 * a;
  p0.X := vp.X + v.X;
  p0.Y := vp.Y + v.Y;
  Poly[1] := Point(Round(p0.X), Round(p0.Y));

  v := vn * a;
  w := wn *  b;
  for i := 2 to FCount-3 do
  begin
    p0 := p0 + v;
    if i mod 2 = 0 then
      p1 := p0 + w
    else
      p1 := p0 - w;
    Poly[i] := Point(Round(p1.X), Round(p1.Y));
  end;

  p0 := p0 + v;
  Poly[FCount-2] := Point(Round(p0.X), Round(p0.Y));

  Poly[FCount-1] := Point(Round(vq.X), Round(vq.Y));

  g.Pen.Width := StrokeThickness;
  g.Pen.Color := StrokeColor;
  g.Polyline(Poly);
end;

function TRggFederLine.RotateDegrees(ov: TPoint3D; wi: single): TPoint3D;
var
  a: single;
  m: TMatrix3D;
begin
  a := DegToRad(DegNormalize(Abs(wi)));
  if wi >= 0 then
    m := TMatrix3D.CreateRotation(TPoint3D.Create(0,0,1), a)
  else
    m := TMatrix3D.CreateRotation(TPoint3D.Create(0,0,-1), a);
  result := ov * m;
end;

{ TRggBigCircle }

constructor TRggBigCircle.Create(ACaption: string);
begin
  inherited Create;
  TypeName := 'BigCircle';
  Caption := ACaption;
  ShowCaption := DefaultShowCaption;
end;

procedure TRggBigCircle.Draw(g: TCanvas);
var
  R: TRect;
begin
  Temp1 := Center + Drawing.FaxPoint3D;

  R := Rect(
    Round(Temp1.X - Radius),
    Round(Temp1.Y - Radius),
    Round(Temp1.X + Radius),
    Round(Temp1.Y + Radius));

  g.Pen.Color := StrokeColor;
  g.Pen.Width := StrokeThickness;
  g.Brush.Style := bsClear;
  g.Ellipse(R);

  if ShowCaption or GlobalShowCaption then
  begin
    TextCenter := Temp1.P;
    TextOut(g, Caption);
  end;
end;

procedure TRggBigCircle.Param3(Delta: single);
begin
  FRadius := FRadius + Delta;
end;

{ TRggBigArc }

constructor TRggBigArc.Create(ACaption: string);
begin
  inherited Create;
  TypeName := 'BigArc';
  Caption := ACaption;
  ShowCaption := False;
  FSweepAngle := 30;
end;

procedure TRggBigArc.Draw(g: TCanvas);
var
  Arrow: TRggPoint3D;
  Angle: single;
  StartAngle: single;
  RadiusF: TPointF;
  P0, P1, P2, P3, P4: TPoint;
  r: Integer;
  startAngleR: single;
  sweepAngleR: single;
begin
  Temp1 := Point1.Center + Drawing.FaxPoint3D;
  Temp2 := Point2.Center + Drawing.FaxPoint3D;

  Arrow.C := Temp2.C - Temp1.C;
  Angle := RadToDeg(Arrow.P.Angle(TPointF.Zero));
  RadiusF.X := Arrow.Length;
  RadiusF.Y := RadiusF.X;

  StartAngle := Angle - SweepAngle / 2;
  SweepAngle := SweepAngle;

  g.Pen.Color := StrokeColor;
  g.Pen.Width := StrokeThickness;

  P0.X := Round(Temp1.X);
  P0.Y := Round(Temp1.Y);

  r := Round(RadiusF.X);

  P1.X := P0.X - r;
  P1.Y := P0.Y - r;
  P2.X := P0.X + r;
  P2.Y := P0.Y + r;

  startAngleR := DegToRad(startAngle);
  sweepAngleR := DegToRad(sweepAngle);

  if sweepAngle < 0 then
  begin
    P3.X := Round(P0.X + cos(startAngleR) * 100);
    P3.Y := Round(P0.Y + sin(startAngleR) * 100);

    P4.X := Round(P0.X + cos(startAngleR + sweepAngleR) * 100);
    P4.Y := Round(P0.Y + sin(startAngleR + sweepAngleR) * 100);
  end
  else
  begin
    P4.X := Round(P0.X + cos(startAngleR) * 100);
    P4.Y := Round(P0.Y + sin(startAngleR) * 100);

    P3.X := Round(P0.X + cos(startAngleR + sweepAngleR) * 100);
    P3.Y := Round(P0.Y + sin(startAngleR + sweepAngleR) * 100);
  end;

  g.Arc(P1.X, P1.Y, P2.X, P2.Y, P3.X, P3.Y, P4.X, P4.Y);

  {FMX: g.DrawArc(Point1.Center.P, RadiusF, startAngle, sweepAngle, Opacity); }

  if ShowCaption or GlobalShowCaption then
  begin
    TextCenter := Temp1.P + (Temp2.P - Temp1.P) * 0.5;
    TextOut(g, Caption);
  end;
end;

procedure TRggBigArc.GetInfo(ML: TStrings);
begin
  inherited;
  if Point1 = nil then
    ML.Add(Caption + '.Point1 = nil');
  if Point2 = nil then
    ML.Add(Caption + '.Point2 = nil');
end;

function TRggBigArc.GetValid: Boolean;
begin
  result := inherited;
  result := result and (Point1 <> nil);
  result := result and (Point2 <> nil);
end;

procedure TRggBigArc.Param1(Delta: single);
begin
  SweepAngle := FSweepAngle + Delta;
end;

procedure TRggBigArc.SetSweepAngle(const Value: single);
begin
  FSweepAngle := Value;
  if FSweepAngle < 10 then
    FSweepAngle := 10;
end;

{ TRggPolyCurve }

procedure TRggPolyCurve.AssignPoly(const APoly: array of TPointF);
var
  i: Integer;
  l: Integer;
begin
  l := Length(APoly);
  if l <> Count then
  begin
    FCount := l;
    SetLength(Poly, l);
    SetLength(TransformedPoly, l);
  end;

  { Poly := APoly; }
  for i := 0 to l-1 do
  begin
    Poly[i].X := Round(APoly[i].X);
    Poly[i].Y := Round(APoly[i].Y);
   end;
end;

constructor TRggPolyCurve.Create(ACaption: string; ACount: Integer);
begin
  inherited Create;
  TypeName := 'PolyCurve';
  Caption := ACaption;
  if (ACount > 2) and (ACount < 361) then
  begin
    FCount := ACount;
    SetLength(Poly, Count);
    SetLength(TransformedPoly, Count);
  end;
end;

procedure TRggPolyCurve.Draw(g: TCanvas);
var
  i: Integer;
begin
  g.Pen.Width := StrokeThickness;
  g.Pen.Color := StrokeColor;

  if Drawing.WantOffset then
  begin
    for i := 0 to Length(Poly) - 1 do
    begin
      TransformedPoly[i].X := Round(Poly[i].X + Drawing.FaxPoint3D.X);
      TransformedPoly[i].Y := Round(Poly[i].Y + Drawing.FaxPoint3D.Y);
    end;
    g.PolyLine(TransformedPoly);
  end
  else
    g.Polyline(Poly);

  DrawText(g);
end;

procedure TRggPolyCurve.DrawText(g: TCanvas);
var
  pf: TPointF;
begin
  if ShowCaption or GlobalShowCaption then
  begin
    if Drawing.WantOffset then
    begin
      pf.X := TransformedPoly[0].X;
      pf.Y := TransformedPoly[0].Y;
    end
    else
    begin
      pf.X := Poly[0].X;
      pf.Y := Poly[0].Y;
    end;
    TextCenter := pf;
    TextOut(g, Caption);
  end;
end;

{ TRggColorScheme }

procedure TRggColorScheme.GoDark;
begin
  TextColor := TRggColors.White;
  BackgroundColor := TRggColors.Color333333;
  LabelColor := TRggColors.Antiquewhite;
end;

procedure TRggColorScheme.GoLight;
begin
  TextColor := TRggColors.Black;
  BackgroundColor := TRggColors.White;
  LabelColor := TRggColors.Plum;
end;

{ TRggFixpointCircle }

constructor TRggFixpointCircle.Create(ACaption: string);
begin
  inherited;
  TypeName := 'Circle';
  ShowCaption := False;
  IsComputed := True;
end;

procedure TRggFixpointCircle.Draw(g: TCanvas);
var
  R: TRect;
begin
  Temp1 := Center + Drawing.FaxPoint3D;

  R := Rect(
    Round(Temp1.X - FRadius),
    Round(Temp1.Y - FRadius),
    Round(Temp1.X + FRadius),
    Round(Temp1.Y + FRadius));

  g.Brush.Color := Drawing.Colors.BackgroundColor;
  g.Ellipse(R);

  g.Pen.Color := TRggColors.Plum;
  g.Pen.Width := StrokeThickness;
  g.Ellipse(R);
end;

{ TRggDrawingBase }

procedure TRggDrawingBase.GoDark;
begin

end;

procedure TRggDrawingBase.GoLight;
begin

end;

procedure TRggDrawingBase.SetIsDark(const Value: Boolean);
begin
  if FIsDark <> Value then
  begin
    FIsDark := Value;
    if Value then
    begin
      Colors.GoDark;
      GoDark;
    end
    else
    begin
      Colors.GoLight;
      GoLight;
    end;
  end;
end;

end.
