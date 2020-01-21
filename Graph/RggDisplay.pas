unit RggDisplay;

interface

uses
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  System.Generics.Defaults,
  Vcl.Graphics,
  RggTypes,
  RggCalc;

type
  TDisplayItemType = (
    diLine,
    diPolyLine,
    diEllipse
  );

  TSchnittGG = class
  public
    A, B: TRealPoint;
    C, D: TRealPoint;
    SP: TRealPoint;
    Fall: TBermerkungGG;
    Eps: double;
    procedure Schnitt;
  end;

  TDisplayItem = class
  public
    ItemType: TDisplayItemType;
    P1: TRealPoint;
    P2: TRealPoint;

    StrokeWidth: Integer;
    Color: TColor;

    LineStart, LineEnd: TPoint;
    CenterPoint: TPoint;
    Radius: Integer;

    PolyArray: array of TPoint;

    procedure Draw(Canvas: TCanvas);
    procedure Assign(Value: TDisplayItem);
    class function CompareDepth(const Left, Right: TDisplayItem): Integer;
  end;

  TDisplayItemComparer = class(TInterfacedObject, IComparer<TDisplayItem>)
  public
    function Compare(const Left, Right: TDisplayItem): Integer;
  end;

  TDisplayList = TList<TDisplayItem>;

  TRggDisplayList = class
  private
    FList: TDisplayList;
    FIndex: Integer;
    FCount: Integer;
    FCapacity: Integer;
    FNeedSort: Boolean;
    function Add: TDisplayItem;
  protected
    DisplayItemComparer: IComparer<TDisplayItem>;
  public
    DI: TDisplayItem;
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Ellipse(P1, P2: TRealPoint; CenterPoint: TPoint; Radius: Integer = 10);
    procedure Line(P1, P2: TRealPoint; A, B: TPoint);
    procedure PolyLine(P1, P2: TRealPoint; A: array of TPoint);
    procedure Draw(Canvas: TCanvas);
  end;

implementation

uses
  System.Math.Vectors;

{ TDisplayItem }

procedure TDisplayItem.Assign(Value: TDisplayItem);
begin
  StrokeWidth := Value.StrokeWidth;
  Color := Value.Color;
end;

procedure TDisplayItem.Draw(Canvas: TCanvas);
begin
  Canvas.Pen.Width := StrokeWidth;
  Canvas.Pen.Color := Color;

  case ItemType of
    diLine:
    begin
      Canvas.MoveTo(LineStart.X, LineStart.Y);
      Canvas.LineTo(LineEnd.X, LineEnd.Y);
    end;

    diPolyLine:
    begin
      Canvas.Polyline(PolyArray);
    end;

    diEllipse:
    begin
      Canvas.Ellipse(
        CenterPoint.x - Radius, CenterPoint.y - Radius,
        CenterPoint.x + Radius, CenterPoint.y + Radius);
    end;

  end;
end;

class function TDisplayItem.CompareDepth(const Left, Right: TDisplayItem): Integer;
var
  A, B, C, D: TRealPoint;
  sp: TRealPoint;

  vv: TRealPoint; // Rgg Vector 3D
  vx: double;
  vy: double;
  vz: double;

  ya, yb, dy: double;

  v, w: TVector; // Delphi 2D Vectors
  lv, lw: double;
  f: double;

  IsParallel: Boolean;
begin
  result := 0;

  A := Left.P1;
  B := Left.P2;
  C := Right.P1;
  D := Right.P2;

  if (A[y] = B[y]) and (C[y] > D[y]) then
  begin
    dy := Left.P1[y] - Right.P1[y];
    if dy > 0 then
      result := 1
    else if dy < 0 then
      result := -1
    else
      result := 0;
    Exit;
  end;

  IsParallel := SchnittGG(A, B, C, D, sp);
  if IsParallel then
  begin
    Exit;
  end;

  { compute ya }
  vv := vsub(B, A);
  vx := vv[x];
  vy := vv[y];
  vz := vv[z];

  v := TVector.Create(vx, vz);
  lv := v.Length;
  w := TVector.Create(sp[x]-A[x], sp[z]-B[z]);
  lw := w.Length;

  if lv < 0.001 then
    Exit;
  f := lw / lv;
  ya := Left.P1[y] + f * vy;

  { compute yb }
  vv := vsub(D, C);
  vx := vv[x];
  vy := vv[y];
  vz := vv[z];

  v := TVector.Create(vx, vz);
  lv := v.Length;
  w := TVector.Create(sp[x]-C[x], sp[z]-D[z]);
  lw := w.Length;

  if lv < 0.001 then
    Exit;
  f := lw / lv;
  yb := Right.P1[y] + f * vy;

  { diff }
  dy := yb - ya;

  { return result }
  if dy > 0 then
    result := 1
  else if dy < 0 then
    result := -1
  else
    result := 0;
end;

{ TRggDisplayList }

constructor TRggDisplayList.Create;
begin
  FCapacity := 200;
  FList := TDisplayList.Create;
  DI := TDisplayItem.Create;
  Clear;

  DisplayItemComparer := TDisplayItemComparer.Create;
end;

destructor TRggDisplayList.Destroy;
var
  i: Integer;
begin
  for i := 0 to FList.Count-1 do
    FList[i].Free;
  FList.Clear;
  FList.Free;
  DI.Free;
  inherited;
end;

procedure TRggDisplayList.Clear;
begin
  FIndex := -1;
  FNeedSort := True;
end;

function TRggDisplayList.Add: TDisplayItem;
begin
  Inc(FIndex);
  if FIndex < FCount then
  begin
    result := FList[FIndex];
  end
  else if (FIndex >= FCount) and (FIndex < FCapacity) then
  begin
    result := TDisplayItem.Create;
    FList.Add(result); // returns Index in List
    Inc(FCount);
    Assert(FCount <= FList.Count);
  end
  else
  begin
    FIndex := FList.Count - 1;
    result := FList.Last;
  end;
  result.Assign(DI);
end;

procedure TRggDisplayList.Ellipse(P1, P2: TRealPoint; CenterPoint: TPoint; Radius: Integer = 10);
var
  cr: TDisplayItem;
begin
  cr := Add;
  cr.ItemType := diEllipse;
  cr.P1 := P1;
  cr.P2 := P2;
  cr.P1[x] := cr.P1[x] - Radius;
  cr.P2[x] := cr.P2[x] + Radius;
  cr.CenterPoint := CenterPoint;
  cr.Radius := Radius;
end;

procedure TRggDisplayList.Line(P1, P2: TRealPoint; A, B: TPoint);
var
  cr: TDisplayItem;
begin
  cr := Add;
  cr.ItemType := diLine;
  cr.P1 := P1;
  cr.P2 := P2;
  cr.LineStart := A;
  cr.LineEnd := B;
end;

procedure TRggDisplayList.PolyLine(P1, P2: TRealPoint; A: array of TPoint);
var
  cr: TDisplayItem;
  c: Integer;
  i: Integer;
begin
  cr := Add;
  cr.ItemType := diPolyLine;
  cr.P1 := P1;
  cr.P2 := P2;
  c := Length(A);
  SetLength(cr.PolyArray, c);
  for i := 0 to c - 1 do
    cr.PolyArray[i] := A[i];
end;

procedure TRggDisplayList.Draw(Canvas: TCanvas);
var
  cr: TDisplayItem;
begin
  if FNeedSort then
  begin
    FList.Sort(DisplayItemComparer);
  end;
  for cr in FList do
  begin
    cr.Draw(Canvas);
  end;
  FNeedSort := False;
end;

{ TDisplayItemComparer }

function TDisplayItemComparer.Compare(const Left, Right: TDisplayItem): Integer;
begin
  result := TDisplayItem.CompareDepth(Left, Right);
end;

{ TSchnittGG }

procedure TSchnittGG.Schnitt;
var
  a1, a2: double;
  sx, sz, x1, z1, x3, z3: double;
  q: double;
begin
  Eps := 0.001;
  Fall := ggOK;

  a1 := 0;
  a2 := 0;
  sx := 0;
  sz := 0;
  x1 := 0;
  z1 := 0;
  x3 := 0;
  z3 := 0;

  q := B[x] - A[x];
  if abs(q) > Eps then
    a1 := (B[z] - A[z]) / q
  else
    Fall := g1Vertical;

  q := D[x] - C[x];
  if abs(q) > Eps then
    a2 := (D[z] - C[z]) / q
  else
    Fall := g2Vertical;

  if (Fall = ggOK) and (a2-a1 < Eps) then
    Fall := ggParallel;

  case Fall of
    ggParallel:
    begin
      sx := 0;
      sz := 0;
    end;

    ggOK:
      begin
        x1 := A[x];
        z1 := A[z];
        x3 := C[x];
        z3 := C[z];
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

  SP[x] := sx;
  SP[y] := 0;
  SP[z] := sz;
end;

end.
