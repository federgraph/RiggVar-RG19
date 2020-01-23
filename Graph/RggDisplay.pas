unit RggDisplay;

interface

uses
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  System.Math,
  System.Math.Vectors,
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

  TRggPoint = record
    P: TRealPoint;
    function IsEqual(B: TRggPoint): Boolean;
    function Compare(Q: TRggPoint): Integer;
  end;

  TRggLine = record
    A: TRggPoint;
    B: TRggPoint;
    function IsTotallyAbove(Other: TRggLine): Boolean;
    function IsTotallyBelow(Other: TRggLine): Boolean;
    function IsSame(Other: TRggLine): Boolean;
  private
    function ComputeSPY(SP: TRealPoint): double;
  end;

  TRggLinePair = record
    L1: TRggLine;
    L2: TRggLine;
    SP: TRealPoint;
    function HasCommonPoint: Boolean;
    function CompareCommon: Integer;
    function IsParallel: Boolean;
    function CompareSPY: Integer;
  end;

//  TSchnittGG = class
//  public
//    A, B: TRealPoint;
//    C, D: TRealPoint;
//    SP: TRealPoint;
//    Fall: TBemerkungGG;
//    procedure Schnitt;
//  end;

  TDisplayItem = class
  public
    ItemType: TDisplayItemType;
    P1: TRealPoint;
    P2: TRealPoint;

    StrokeWidth: Integer;
    StrokeColor: TColor;

    LineStart, LineEnd: TPoint;
    CenterPoint: TPoint;
    Radius: Integer;

    PolyArray: array of TPoint;

    procedure Draw(Canvas: TCanvas);
    procedure Assign(Value: TDisplayItem);
    class function Compare(const Left, Right: TDisplayItem): Integer;
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
    FCapacity: Integer;
    FNeedSort: Boolean;
    function Add: TDisplayItem;
    procedure CheckCount;
  protected
    DisplayItemComparer: IComparer<TDisplayItem>;
  public
    DI: TDisplayItem;
    WantLineColors: Boolean;
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Ellipse(P1, P2: TRealPoint; CenterPoint: TPoint; Radius: Integer = 10);
    procedure Line(P1, P2: TRealPoint; A, B: TPoint; Color: TColor);
    procedure PolyLine(P1, P2: TRealPoint; A: array of TPoint);
    procedure Draw(Canvas: TCanvas);
  end;

implementation

const
  Eps = 0.0001;

{ TDisplayItem }

procedure TDisplayItem.Assign(Value: TDisplayItem);
begin
  StrokeWidth := Value.StrokeWidth;
  StrokeColor := Value.StrokeColor;
end;

procedure TDisplayItem.Draw(Canvas: TCanvas);
begin
  Canvas.Pen.Width := StrokeWidth;
  Canvas.Pen.Color := StrokeColor;

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

class function TDisplayItem.Compare(const Left, Right: TDisplayItem): Integer;
var
  LP: TRggLinePair;
  r: Integer;
begin
  if Left = nil then
  begin
    result := 0;
    Exit;
  end;

  if Right = nil then
  begin
    result := 0;
    Exit;
  end;

  LP.L1.A.P := Left.P1;
  LP.L1.B.P := Left.P2;
  LP.L2.A.P := Right.P1;
  LP.L2.B.P := Right.P2;

  if False then

  else if Left.ItemType = diEllipse then
  begin
    r := 1;
  end

  else if Right.ItemType = diEllipse then
  begin
    r := -1;
  end

  else if LP.L1.IsSame(LP.L2) then
  begin
    r := 0;
  end

  else if LP.L1.IsTotallyAbove(LP.L2) then
  begin
    r := 1;
  end

  else if LP.L1.IsTotallyBelow(LP.L2) then
  begin
    r := -1;
  end

  else if LP.HasCommonPoint then
  begin
    r := LP.CompareCommon;
  end

  else if LP.IsParallel then
  begin
    r := 0;
  end

  else
  begin
    r := LP.CompareSPY;
  end;

  result := -r;
end;

{ TRggDisplayList }

constructor TRggDisplayList.Create;
begin
  FCapacity := 100;
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

procedure TRggDisplayList.CheckCount;
var
  i: Integer;
begin
  if FList.Count > FIndex + 1 then
  begin
    for i := FIndex + 1 to FList.Count - 1 do
      FList[i].Free;
    FList.Count := FIndex + 1;
  end;
end;

function TRggDisplayList.Add: TDisplayItem;
begin
  Inc(FIndex);
  if FIndex < FList.Count then
  begin
    result := FList[FIndex];
  end
  else if (FIndex >= FList.Count) and (FIndex < FCapacity) then
  begin
    result := TDisplayItem.Create;
    FList.Add(result); // returns Index in List
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

procedure TRggDisplayList.Line(P1, P2: TRealPoint; A, B: TPoint; Color: TColor);
var
  cr: TDisplayItem;
begin
  if WantLineColors then
    DI.StrokeColor := Color;
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
  CheckCount;

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
  result := TDisplayItem.Compare(Left, Right);
end;

{ TSchnittGG }

//procedure TSchnittGG.Schnitt;
//var
//  a1, a2: double;
//  sx, sz, x1, z1, x3, z3: double;
//  q: double;
//begin
//  Fall := ggOK;
//
//  a1 := 0;
//  a2 := 0;
//  sx := 0;
//  sz := 0;
//  x1 := 0;
//  z1 := 0;
//  x3 := 0;
//  z3 := 0;
//
//  q := B[x] - A[x];
//  if abs(q) > Eps then
//    a1 := (B[z] - A[z]) / q
//  else
//    Fall := g1Vertical;
//
//  q := D[x] - C[x];
//  if abs(q) > Eps then
//    a2 := (D[z] - C[z]) / q
//  else
//    Fall := g2Vertical;
//
//  if (Fall = ggOK) and (a2-a1 < Eps) then
//    Fall := ggParallel;
//
//  case Fall of
//    ggParallel:
//    begin
//      sx := 0;
//      sz := 0;
//    end;
//
//    ggOK:
//      begin
//        x1 := A[x];
//        z1 := A[z];
//        x3 := C[x];
//        z3 := C[z];
//        sx := (-a1 * x1 + a2 * x3 - z3 + z1) / (-a1 + a2);
//        sz := (-a2 * a1 * x1 + a2 * z1 + a2 * x3 * a1 - z3 * a1) / (-a1 + a2);
//      end;
//
//    g1Vertical:
//      begin
//        sz := a2 * x1 - a2 * x3 + z3;
//        sx := x1;
//      end;
//
//    g2Vertical:
//      begin
//        sz := a1 * x3 - a1 * x1 + z1;
//        sx := x3;
//      end;
//  end;
//
//  SP[x] := sx;
//  SP[y] := 0;
//  SP[z] := sz;
//end;

{ TRggPoint }

function TRggPoint.Compare(Q: TRggPoint): Integer;
begin
  if P[y] > Q.P[y] then
    result := -1
  else if P[y] < Q.P[y] then
    result := 1
  else
    result := 0;
end;

function TRggPoint.IsEqual(B: TRggPoint): Boolean;
begin
  result :=
    (P[x] = B.P[x]) and
    (P[y] = B.P[y]) and
    (P[z] = B.P[z]);
end;

{ TRggLine }

function TRggLine.IsSame(Other: TRggLine): Boolean;
begin
  result := False;
  if A.IsEqual(Other.A) and B.IsEqual(Other.B) then
    result := True
  else if A.IsEqual(Other.B) and B.IsEqual(Other.A) then
    result := True;
end;

function TRggLine.IsTotallyAbove(Other: TRggLine): Boolean;
begin
  result :=
    (A.P[y] > Other.A.P[y]) and
    (A.P[y] > Other.B.P[y]) and
    (B.P[y] > Other.A.P[y]) and
    (B.P[y] > Other.B.P[y]);
end;

function TRggLine.IsTotallyBelow(Other: TRggLine): Boolean;
begin
  result :=
    (A.P[y] < Other.A.P[y]) and
    (A.P[y] < Other.B.P[y]) and
    (B.P[y] < Other.A.P[y]) and
    (B.P[y] < Other.B.P[y]);
end;

function TRggLine.ComputeSPY(SP: TRealPoint): double;
var
  vs: TRealPoint;
  vv: TRealPoint; // Rgg Vector 3D
  vx: double;
  vy: double;
  vz: double;

  v, w: TVector; // Delphi 2D Vectors
  lv, lw: double;
  f, g: double;
begin
  result := (A.P[y] + B.P[y]) / 2;

  vs := vsub(SP, A.P);
  vv := vsub(B.P, A.P);
  vx := vv[x];
  vy := vv[y];
  vz := vv[z];

  v := TVector.Create(vx, vz);
  lv := v.Length;
  w := TVector.Create(vs[x], vs[z]);
  lw := w.Length;

//  if lv < Eps then
//  begin
//    result := (A.P[y] + B.P[y]) / 2;
//    Exit;
//  end;

  f := lw / lv;

  if Sign(vv[x]) <> Sign(vs[x]) then
    g := -f
  else
    g := f;

  if f > 10000 then
  begin
    result := A.P[y];
    Exit;
  end;

  if f < 1 then
  begin
    result := A.P[y] + g * vy;
    Exit;
  end;

  if f > 1 then
  begin
    result := A.P[y] + g * vy;
    Exit;
  end;

end;

{ TRggLinePair }

function TRggLinePair.CompareCommon: Integer;
begin
  result := 0;
  if L1.A.IsEqual(L2.A) then
    result := L1.B.Compare(L2.B)
  else if L1.A.IsEqual(L2.B) then
    result := L1.B.Compare(L2.A)

  else if L1.B.IsEqual(L2.A) then
    result := L1.A.Compare(L2.B)
  else if L1.B.IsEqual(L2.B) then
    result := L1.A.Compare(L2.A);
end;

function TRggLinePair.HasCommonPoint: Boolean;
begin
  result :=
    L1.A.IsEqual(L2.A) or
    L1.A.IsEqual(L2.B) or
    L1.B.IsEqual(L2.A) or
    L1.B.IsEqual(L2.B);
end;

function TRggLinePair.IsParallel: Boolean;
begin
  result := SchnittGG(L1.A.P, L1.B.P, L2.A.P, L2.B.P, SP);
end;

function TRggLinePair.CompareSPY: Integer;
var
  ya, yb, dy: double;
begin
  ya := L1.ComputeSPY(SP);
  yb := L2.ComputeSPY(SP);

  dy := ya - yb;

  if dy > 0 then
    result := 1
  else if dy < 0 then
    result := -1
  else
    result := 0;
end;

end.
