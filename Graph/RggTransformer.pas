unit RggTransformer;

interface

uses
 System.Math.Vectors,
  RggCalc,
  RggTypes,
  RggMatrix,
  RggPolarKar;

type
  TRggGetFixPunkt = function: TRealPoint of object;

  TRggTransformer = class
  private
    Updated: Boolean;
    FFixPoint: TRiggPoint;
    FZoom: double;
    FFixPunkt: TRealPoint;
    FTransformedFixPunkt: TRealPoint;
    FOnGetFixPunkt: TRggGetFixPunkt;
    procedure BuildMatrix;
    procedure SetFixPoint(const Value: TRiggPoint);
    procedure SetZoom(const Value: double);
    procedure SetFixPunkt(const Value: TRealPoint);
    procedure SetOnGetFixPunkt(const Value: TRggGetFixPunkt);
  public
    Mat: TMatrix4x4;
    Rotator: TPolarKar; // injected, not owned
    constructor Create;
    destructor Destroy; override;
    procedure UpdateTransformedFixPunkt;
    function TransformPoint(p: TRealPoint): TRealPoint;
    property Zoom: double read FZoom write SetZoom;
    property FixPoint: TRiggPoint read FFixPoint write SetFixPoint;
    property FixPunkt: TRealPoint read FFixPunkt write SetFixPunkt;
    property TransformedFixPunkt: TRealPoint read FTransformedFixPunkt;
    property OnGetFixPunkt: TRggGetFixPunkt read FOnGetFixPunkt write SetOnGetFixPunkt;
  end;

implementation

uses
  Vector3D;

procedure TRggTransformer.BuildMatrix;
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

constructor TRggTransformer.Create;
begin
  Mat := TMatrix4x4.Create;
  FFixPoint := ooD0;
  FZoom := 0.05;
end;

destructor TRggTransformer.Destroy;
begin
  Mat.Free;
  inherited;
end;

procedure TRggTransformer.SetFixPoint(const Value: TRiggPoint);
begin
  FFixPoint := Value;
  Updated := False;
end;

procedure TRggTransformer.SetFixPunkt(const Value: TRealPoint);
begin
  FFixPunkt := Value;
  Updated := False;
end;

procedure TRggTransformer.SetOnGetFixPunkt(const Value: TRggGetFixPunkt);
begin
  FOnGetFixPunkt := Value;
end;

procedure TRggTransformer.SetZoom(const Value: double);
begin
  FZoom := Value;
  Updated := False;
end;

function TRggTransformer.TransformPoint(p: TRealPoint): TRealPoint;
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

procedure TRggTransformer.UpdateTransformedFixPunkt;
begin
  FTransformedFixPunkt := Rotator.Rotiere(FFixPunkt);
end;

end.
