unit RggTransformer;

interface

uses
  RggCalc,
  RggTypes,
  RggMatrix,
  RggPolarKar;

type
  TRggGetFixPunkt = function: TRealPoint of object;

  { Base version with OnGetFixPunkt }
  TRggTransformer00 = class
  protected
    Updated: Boolean;
    FFixPoint: TRiggPoint;
    FZoom: double;
    FFixPunkt: TRealPoint;
    FTransformedFixPunkt: TRealPoint;
    FOnGetFixPunkt: TRggGetFixPunkt;
    procedure SetFixPoint(const Value: TRiggPoint);
    procedure SetZoom(const Value: double);
    procedure SetOnGetFixPunkt(const Value: TRggGetFixPunkt);
  public
    Rotator: TPolarKar; // injected, not owned
    constructor Create;
    property Zoom: double read FZoom write SetZoom;
    property FixPoint: TRiggPoint read FFixPoint write SetFixPoint;
    property TransformedFixPunkt: TRealPoint read FTransformedFixPunkt;
    property OnGetFixPunkt: TRggGetFixPunkt read FOnGetFixPunkt write SetOnGetFixPunkt;
  end;

  { not used, FixPunkt not acessible }
  TRggTransformer01 = class(TRggTransformer00)
  protected
    procedure SetFixPunkt(const Value: TRealPoint);
  public
    procedure UpdateTransformedFixPunkt;
    property FixPunkt: TRealPoint read FFixPunkt write SetFixPunkt;
  end;

  { using Matrix4x4, see Hull }
  TRggTransformer4x4 = class(TRggTransformer01)
  private
    procedure BuildMatrix;
  public
    Mat: TMatrix4x4;
    constructor Create;
    destructor Destroy; override;
    function TransformPoint(p: TRealPoint): TRealPoint;
  end;

  TRggTransformer = TRggTransformer4x4;

implementation

uses
  RggVector;

{ TRggTransformer00 }

constructor TRggTransformer00.Create;
begin
  FFixPoint := ooD0;
  FZoom := 0.05;
end;

procedure TRggTransformer00.SetFixPoint(const Value: TRiggPoint);
begin
  FFixPoint := Value;
  Updated := False;
end;

procedure TRggTransformer00.SetOnGetFixPunkt(const Value: TRggGetFixPunkt);
begin
  FOnGetFixPunkt := Value;
end;

procedure TRggTransformer00.SetZoom(const Value: double);
begin
  FZoom := Value;
  Updated := False;
end;

{ TRggTransformer01 }

procedure TRggTransformer01.SetFixPunkt(const Value: TRealPoint);
begin
  FFixPunkt := Value;
  Updated := False;
end;

procedure TRggTransformer01.UpdateTransformedFixPunkt;
begin
  FTransformedFixPunkt := Rotator.Rotiere(FFixPunkt);
end;

{ TRggTransformer4x4 }

constructor TRggTransformer4x4.Create;
begin
  inherited;
  Mat := TMatrix4x4.Create;
end;

destructor TRggTransformer4x4.Destroy;
begin
  Mat.Free;
  inherited;
end;

procedure TRggTransformer4x4.BuildMatrix;
begin
  if Assigned(OnGetFixPunkt) then
    FFixPunkt := OnGetFixPunkt;

  FTransformedFixPunkt := Rotator.Rotiere(FFixPunkt);

  Mat.Identity;
  Mat.Translate(
    -FTransformedFixPunkt[x],
    -FTransformedFixPunkt[y],
    -FTransformedFixPunkt[z]
  );
  Mat.Multiply(Rotator.Matrix);
  Mat.ScaleXYZ(Zoom, Zoom, Zoom);
end;

function TRggTransformer4x4.TransformPoint(p: TRealPoint): TRealPoint;
var
  pt: vec3;
begin
  if not Updated then
    BuildMatrix;

  pt.x := p[x];
  pt.y := p[y];
  pt.z := p[z];
  Mat.TransformPoint(pt);
  result[x] := pt.x;
  result[y] := pt.y;
  result[z] := pt.z;
end;

end.
