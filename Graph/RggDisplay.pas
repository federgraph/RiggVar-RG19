unit RggDisplay;

interface

uses
  System.Types,
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
  TLineSegmentCompareCase = (
    ccNone,
    ccNil,
    ccEllipse,
    ccParallel,
    ccNoVisibleCrossing,
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
    function HasVisibleCrossing(SP: TRealPoint): Boolean;
    function ComputeSPY(SP: TRealPoint): double;
  end;

  TRggLinePair = record
    L1: TRggLine;
    L2: TRggLine;
    SP: TRealPoint;
    function HasCommonPoint: Boolean;
    function CompareCommon: Integer;
    function IsParallel: Boolean;
    function DoesNotHaveVisibleCrossing: Boolean;
    function CompareSPY: Integer;
  end;

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

    PolyArray: TRggPolyLine;

    Bemerkung: TLineSegmentCompareCase;

    procedure Draw(g: TCanvas);
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
    procedure PolyLine(P1, P2: TRealPoint; A: TRggPolyLine; Color: TColor);
    procedure Draw(Canvas: TCanvas);
    function CompareItems(i1, i2: Integer): Integer;
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

procedure TDisplayItem.Draw(g: TCanvas);
begin
  g.Pen.Width := StrokeWidth;
  g.Pen.Color := StrokeColor;

  case ItemType of
    diLine:
    begin
      g.MoveTo(LineStart.X, LineStart.Y);
      g.LineTo(LineEnd.X, LineEnd.Y);
    end;

    diPolyLine:
    begin
      g.Polyline(PolyArray);
    end;

    diEllipse:
    begin
      g.Ellipse(
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
    Left.Bemerkung := ccNil;
    result := 0;
    Exit;
  end;

  if Right = nil then
  begin
    Right.Bemerkung := ccNil;
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
    Left.Bemerkung := ccEllipse;
    r := -1;
  end

  else if Right.ItemType = diEllipse then
  begin
    Right.Bemerkung := ccEllipse;
    r := 1;
  end

  else if LP.L1.IsSame(LP.L2) then
  begin
    Left.Bemerkung := ccTotallySame;
    r := 0;
  end

  else if LP.L1.IsTotallyAbove(LP.L2) then
  begin
    Left.Bemerkung := ccTotallyAbove;
    r := -1;
  end

  else if LP.L1.IsTotallyBelow(LP.L2) then
  begin
    Left.Bemerkung := ccTotallyBelow;
    r := 1;
  end

  else if LP.HasCommonPoint then
  begin
    r := LP.CompareCommon;
    case r of
      0: Left.Bemerkung := ccCommonSame;
      1: Left.Bemerkung := ccCommonAbove;
      -1: Left.Bemerkung := ccCommonBelow;
      else
        Left.Bemerkung := ccCommonNone;
    end;
  end

  else if LP.IsParallel then
  begin
    Left.Bemerkung := ccParallel;
    r := 0;
  end

  else if LP.DoesNotHaveVisibleCrossing then
  begin
    Left.Bemerkung := ccNoVisibleCrossing;
    r := 0;
  end

  else
  begin
    r := LP.CompareSPY;
    case r of
      0: Left.Bemerkung := ccSame;
      1: Left.Bemerkung := ccAbove;
      -1: Left.Bemerkung := ccBelow;
      else
        Left.Bemerkung := ccNone;
    end;
  end;

  result := r;
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

function TRggDisplayList.CompareItems(i1, i2: Integer): Integer;
var
  cr1, cr2: TDisplayItem;
begin
  cr1 := FList.Items[i1];
  cr2 := FList.Items[i2];
  result := TDisplayItem.Compare(cr1, cr2);
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

procedure TRggDisplayList.PolyLine(P1, P2: TRealPoint; A: TRggPolyLine; Color: TColor);
var
  cr: TDisplayItem;
begin
  if WantLineColors then
    DI.StrokeColor := Color;
  cr := Add;
  cr.ItemType := diPolyLine;
  cr.P1 := P1;
  cr.P2 := P2;
  cr.PolyArray := A;
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

function TRggLine.HasVisibleCrossing(SP: TRealPoint): Boolean;
var
  vSP: TRealPoint;
  vAB: TRealPoint; // Rgg Vector 3D

  vABxz: TVector;
  vSPxz: TVector; // Delphi 2D Vectors
  lengthABxz, lengthSPxz: double;
  RatioSPtoAB, g: double;
begin
  result := False;

  vSP := vsub(SP, A.P);
  vAB := vsub(B.P, A.P);

  vABxz := TVector.Create(vAB[x], vAB[z]);
  lengthABxz := vABxz.Length;

  vSPxz := TVector.Create(vSP[x], vSP[z]);
  lengthSPxz := vSPxz.Length;

  if lengthABxz < Eps then
  begin
    Exit;
  end;

  RatioSPtoAB := lengthSPxz / lengthABxz;

  g := Abs(RatioSPtoAB);

  result := (g > 0.1) and (g < 0.9);
end;

function TRggLine.ComputeSPY(SP: TRealPoint): double;
var
  vSP: TRealPoint;
  vAB: TRealPoint; // Rgg Vector 3D

  vABxz: TVector;
  vSPxz: TVector; // Delphi 2D Vectors
  lengthABxz, lengthSPxz: double;
  RatioSPtoAB, g: double;
begin
  result := (A.P[y] + B.P[y]) / 2;

  vSP := vsub(SP, A.P);
  vAB := vsub(B.P, A.P);

  vABxz := TVector.Create(vAB[x], vAB[z]);
  lengthABxz := vABxz.Length;

  vSPxz := TVector.Create(vSP[x], vSP[z]);
  lengthSPxz := vSPxz.Length;

  if lengthABxz < Eps then
  begin
    Exit;
  end;

  RatioSPtoAB := lengthSPxz / lengthABxz;

  g := RatioSPtoAB;

  if Sign(vAB[x]) <> Sign(vSP[x]) then
    g := -RatioSPtoAB;

  if Abs(g) > 10000 then
  begin
    { does not come in here }
    result := A.P[y];
    Exit;
  end;

  if A.P[y] > B.P[y] then
  begin
    result := A.P[y] - g * vAB[y];
  end
  else
  begin
    result := A.P[y] + g * vAB[y];
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
    result := -L1.A.Compare(L2.B)
  else if L1.B.IsEqual(L2.B) then
    result := -L1.A.Compare(L2.A);
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
  result := not SchnittGG(L1.A.P, L1.B.P, L2.A.P, L2.B.P, SP);
end;

function TRggLinePair.DoesNotHaveVisibleCrossing: Boolean;
begin
  result := not L1.HasVisibleCrossing(SP);
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
