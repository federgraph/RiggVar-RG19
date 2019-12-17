unit ThreeDSolid;

interface

uses
  Windows,
  Dialogs,
  SysUtils,
  UITypes,
  Graphics,
  Vector3D,
  ThreeD;

type
  Point = Vector;

  TTriangle = class //Node für permanente Dreieckliste des Geometriemodells
  public
    V1, V2, V3: Point;
    ColorIndex: Integer;
    Centroid: Point;
    Normal: Vector;
    d: double;
    Next: TTriangle;
    procedure ComputeCentroid;
    procedure ComputeNormal;
    procedure ComputeD;
  end;

  PList = ^TList;

  TList = class //Node für temporäre Dreiecklisten
  public
    Tri: TTriangle;
    Next: TList;
  end;

  TBSPNode = class //Node für den permanenten BSP Tree
  public
    Tri: TTriangle;
    Inside: TBSPNode;
    Outside: TBSPNode;
    constructor Create(Triangle: TTriangle);
  end;

  TThreeDSolid = class(TThreeD)
  public
    TriList: TTriangle; //permanente Dreiecksliste
    BSPTree: TBSPNode; //BSP Tree
    Ambient: double;
    Diffuse: double;
    Light: Vector;
    WireFrame: Boolean;
    Shading: Boolean;
    constructor Create;
    destructor Destroy; override;
    function Read3DObject(filename: string): Integer; override;
    function Read3DModel: Integer; override;
    function BuildModel: Integer;
    procedure CalcPlaneEqs;
    procedure DisposeBSP(tree: TBSPNode);
    procedure DisposeTriList;
    function MakeBSPNode(Triangle: TTriangle): TBSPNode;
    procedure AddList(ListAddress: PList; Tri: TTriangle);
    function CalcSign(p: Point; Tri: TTriangle): double;
    procedure Intersect(Tri: TTriangle; var V1, V2, loc: Point);
    procedure InsertTriangle(ListAddress: PList; var V1, V2, V3: Point;
      copyFrom: TTriangle);
    procedure Split(frontList, backList: PList; signOfV1, signOfV2,
      signOfV3: double; sepPlane, Tri: TTriangle);
    function MakeBSPTree(l: TList): TBSPNode;
    procedure WorldToDisplay(x, y, z: double; var pc: TPoint);
    procedure CalcTriNormals;
    procedure PrecomputeCentroids;
    function ComputeColor(var p: Point; var Normal: Vector; colorNdx: Integer)
      : TColor;
    procedure DisplayTriangle(Canvas: TCanvas; Tri: TTriangle);
    procedure View(Canvas: TCanvas); override;
    procedure TraverseTree(Canvas: TCanvas; tree: TBSPNode);
  end;

implementation

procedure TTriangle.ComputeCentroid;
begin
  Centroid.x := (V1.x + V2.x + V3.x) / 3;
  Centroid.y := (V1.y + V2.y + V3.y) / 3;
  Centroid.z := (V1.z + V2.z + V3.z) / 3;
end;

procedure TTriangle.ComputeNormal;
var
  d1, d2: Vector;
begin
  d1 := Subtract(V1, V2);
  d2 := Subtract(V1, V3);
  Normal := Cross(d1, d2);
  Normalize(Normal);
end;

procedure TTriangle.ComputeD;
begin
  d := -(Normal.x * V1.x + Normal.y * V1.y + Normal.z * V1.z);
end;

{ **************************************************************************** }

constructor TBSPNode.Create(Triangle: TTriangle);
begin
  inherited Create;
  Tri := Triangle;
  Outside := nil;
  Inside := nil;
end;

{ **************************************************************************** }

constructor TThreeDSolid.Create;
begin
  inherited Create;
  Diffuse := 0.30;
  Ambient := 0.30;
  Light.x := -10;
  Light.y := 100;
  Light.z := 100;
end;

destructor TThreeDSolid.Destroy;
begin
  DisposeBSP(BSPTree);
  DisposeTriList;
end;

function TThreeDSolid.Read3DObject(filename: string): Integer;
begin
  inherited Read3DObject(filename);
  //nicht kompatibel mit altem Fileformat aber umschaltbar Wireframe/Solid
  { if WireFrame then Exit; }
  result := BuildModel; //BSP Tree aufbauen
end;

function TThreeDSolid.Read3DModel;
begin
  inherited Read3DModel;
  //nicht kompatibel mit altem Fileformat aber umschaltbar Wireframe/Solid
  { if WireFrame then Exit; }
  result := BuildModel; //BSP Tree aufbauen
end;

function TThreeDSolid.BuildModel: Integer;
var
  i: Integer;
  Tri, t: TTriangle;
  nuL, tempList: TList;
begin
  DisposeBSP(BSPTree);
  DisposeTriList;

  //Create TriList - a TTriangle list of triangle data for the BSP tree
  TriList := nil;
  i := 1;
  while i < Length do
  begin
    t := TTriangle.Create;
    if t = nil then
    begin
      result := 0; //out of Memory
      Exit;
    end;
    t.V1 := Points[Connect[i]];
    t.V2 := Points[Connect[i + 1]];
    t.V3 := Points[Connect[i + 2]];
    t.ColorIndex := -Connect[i + 3];
    i := i + 4;
    t.Next := TriList;
    TriList := t;
  end;

  //Create tempList - a TList list of all the triangles in TriList
  Tri := TriList;
  tempList := nil;
  while Tri <> nil do
  begin
    nuL := TList.Create;
    if nuL = nil then
    begin
      result := 0; //out of Memory
      Exit;
    end;
    nuL.Tri := Tri;
    nuL.Next := tempList;
    tempList := nuL; //insert at head of list
    Tri := Tri.Next;
  end;

  //BSPTree erzeugen und Vorausberechnungen vornehmen
  CalcTriNormals; //muß vor MakeBSPTree stehen
  CalcPlaneEqs; //muß vor MakeBSPTree stehen
  BSPTree := MakeBSPTree(tempList);
  PrecomputeCentroids; //muß nach MakeBSPTree stehen

  //tempList freigeben, Variable nuL wird hier nochmal verwendet
  nuL := tempList;
  while nuL <> nil do
  begin
    tempList := nuL.Next;
    nuL.Free;
    nuL := tempList;
  end;

  result := 1;
end;

procedure TThreeDSolid.DisposeTriList;
var
  Tri: TTriangle;
begin
  //TriList;
  while TriList <> nil do
  begin
    Tri := TriList.Next;
    TriList.Free;
    TriList := Tri;
  end;
end;

{ Dispose of the BSP tree }
procedure TThreeDSolid.DisposeBSP(tree: TBSPNode);
begin
  if Assigned(tree) then
  begin
    DisposeBSP(tree.Outside);
    DisposeBSP(tree.Inside);
    tree.Free;
  end;
end;

{ Create a new node for the BSP tree }
function TThreeDSolid.MakeBSPNode(Triangle: TTriangle): TBSPNode;
var
  node: TBSPNode;
begin
  node := TBSPNode.Create(Triangle);
  result := node;
end;

{ Add the triangle to the triangle list. }
procedure TThreeDSolid.AddList(ListAddress: PList; Tri: TTriangle);
{ tri wird am Ende von tliste eingefügt!
  ListAddress ist die Adresse der Liste bzw. des Zeigers auf das Listenelementes.
  Dies ist notwendig, da die Liste auch nil sein kann! }
var
  l, nuL, back: TList;
begin
  l := ListAddress^;
  back := l;
  nuL := TList.Create;
  if nuL = nil then
  begin
    ShowMessage('AddList: Out of Memory');
    Exit;
  end;
  nuL.Tri := Tri;
  nuL.Next := nil;
  if Assigned(l) then
  begin
    while Assigned(l) do
    begin
      back := l;
      l := l.Next;
    end;
    back.Next := nuL;
  end
  else
    ListAddress^ := nuL; //List is emty. This is the first node.
end;

{ Calculate the sign that indicates which side of the separation plane
  vertex p is on. Note that a tolerance value is used to account for arithmetic
  round off errors by the computer. This function uses the fact that a plane
  equation is: ax + bx + cz = 0. If p is on the plane and plugged into the
  equation, the result will be 0. If p is not on the plane, the sign of the result
  indicates which side of the plane the point is on. }
function TThreeDSolid.CalcSign(p: Point; Tri: TTriangle): double;
var
  value: double;
begin
  value := p.x * Tri.Normal.x + p.y * Tri.Normal.y + p.z * Tri.Normal.z + Tri.d;
  if abs(value) < TOL then
    result := 0.0 //The sign is on the plane
  else
    result := value; //The sign of the value indicates which side p is on
end;

{ Uses a parametric equation to determin where a line intersects the plane.
  The two vertices v1 and v2 are the endpoints of the line. }
procedure TThreeDSolid.Intersect(Tri: TTriangle; var V1, V2, loc: Point);
var
  t: double;
begin
  t := (Tri.Normal.x * (V2.x - V1.x) + Tri.Normal.y * (V2.y - V1.y)
      + Tri.Normal.z * (V2.z - V1.z));

  if t <> 0 then
    t := -(Tri.Normal.x * V1.x + Tri.Normal.y * V1.y + Tri.Normal.z * V1.z +
        Tri.d) / t
  else
    ShowMessage('t = 0');

  if (t >= -TOL) and (t <= 1 + TOL) then
  begin
    loc.x := V1.x + t * (V2.x - V1.x);
    loc.y := V1.y + t * (V2.y - V1.y);
    loc.z := V1.z + t * (V2.z - V1.z);
  end;
end;

{ Insert the triangle formed by the vertices v1, v2, v3 to the beginning
  of the triangle list, TriList. Add an appropriate pointer to this TRIANGLE
  structure to then end of TList. }
procedure TThreeDSolid.InsertTriangle(ListAddress: PList;
  var V1, V2, V3: Point; copyFrom: TTriangle);
var
  nuT: TTriangle;
begin
  //Add a new triangle structure to the beginning of the list of
  //triangles in then figure
  nuT := TTriangle.Create;
  if nuT = nil then
  begin
    ShowMessage('InsertTriangle: Out of Memory');
    Exit;
  end;
  nuT.Next := TriList;
  TriList := nuT;
  nuT.V1 := V1;
  nuT.V2 := V2;
  nuT.V3 := V3;
  nuT.Normal := copyFrom.Normal;
  nuT.d := copyFrom.d;
  nuT.ColorIndex := copyFrom.ColorIndex;

  //Append a pointer to this triangle data to tlist.
  //It must be appended, because the head of the list is used.
  AddList(ListAddress, nuT);
end;

{ Split the triangle by the plane specified }
procedure TThreeDSolid.Split(frontList, backList: PList; signOfV1, signOfV2,
  signOfV3: double; sepPlane, Tri: TTriangle);
var
  p, p2: Point;
begin
  if signOfV1 = 0 then
  begin //The plane goes through vertex V1
    Intersect(sepPlane, Tri.V2, Tri.V3, p);
    if signOfV2 > 0 then
    begin //Make right half of front side
      InsertTriangle(frontList, Tri.V1, Tri.V2, p, Tri);
      InsertTriangle(backList, Tri.V1, p, Tri.V3, Tri);
    end
    else
    begin
      InsertTriangle(backList, Tri.V1, Tri.V2, p, Tri);
      InsertTriangle(frontList, Tri.V1, p, Tri.V3, Tri);
    end;
  end
  else if signOfV2 = 0 then
  begin //The plane goes through vertex V2
    Intersect(sepPlane, Tri.V1, Tri.V3, p);
    if signOfV1 > 0 then
    begin //Make right half of front side
      InsertTriangle(frontList, Tri.V1, Tri.V2, p, Tri);
      InsertTriangle(backList, p, Tri.V2, Tri.V3, Tri);
    end
    else
    begin
      InsertTriangle(backList, Tri.V1, Tri.V2, p, Tri);
      InsertTriangle(frontList, p, Tri.V2, Tri.V3, Tri);
    end;
  end
  else if signOfV3 = 0 then
  begin //The plane goes through vertex V3
    Intersect(sepPlane, Tri.V1, Tri.V2, p);
    if signOfV1 > 0 then
    begin //Make right half of front side
      InsertTriangle(frontList, Tri.V1, p, Tri.V3, Tri);
      InsertTriangle(backList, p, Tri.V2, Tri.V3, Tri);
    end
    else
    begin
      InsertTriangle(backList, Tri.V1, p, Tri.V3, Tri);
      InsertTriangle(frontList, p, Tri.V2, Tri.V3, Tri);
    end;
  end
  else if (signOfV1 > 0) and (signOfV3 > 0) then
  begin //Vertex 2 d
    Intersect(sepPlane, Tri.V1, Tri.V2, p);
    Intersect(sepPlane, Tri.V2, Tri.V3, p2);
    InsertTriangle(frontList, Tri.V1, p, Tri.V3, Tri);
    InsertTriangle(frontList, p, p2, Tri.V3, Tri);
    InsertTriangle(backList, p, Tri.V2, p2, Tri);
  end
  else if (signOfV1 < 0) and (signOfV3 < 0) then
  begin //Vertex 2 d
    Intersect(sepPlane, Tri.V1, Tri.V2, p);
    Intersect(sepPlane, Tri.V2, Tri.V3, p2);
    InsertTriangle(backList, Tri.V1, p, Tri.V3, Tri);
    InsertTriangle(backList, p, p2, Tri.V3, Tri);
    InsertTriangle(frontList, p, Tri.V2, p2, Tri);
  end
  else if (signOfV2 > 0) and (signOfV3 > 0) then
  begin //Vertex 1 d
    Intersect(sepPlane, Tri.V1, Tri.V2, p);
    Intersect(sepPlane, Tri.V1, Tri.V3, p2);
    InsertTriangle(frontList, p, Tri.V3, p2, Tri);
    InsertTriangle(frontList, p, Tri.V2, Tri.V3, Tri);
    InsertTriangle(backList, Tri.V1, p, p2, Tri);
  end
  else if (signOfV2 < 0) and (signOfV3 < 0) then
  begin //Vertex 1 d
    Intersect(sepPlane, Tri.V1, Tri.V2, p);
    Intersect(sepPlane, Tri.V1, Tri.V3, p2);
    InsertTriangle(backList, p, Tri.V3, p2, Tri);
    InsertTriangle(backList, p, Tri.V2, Tri.V3, Tri);
    InsertTriangle(frontList, Tri.V1, p, p2, Tri);
  end
  else if (signOfV1 > 0) and (signOfV2 > 0) then
  begin //Vertex 3 d
    Intersect(sepPlane, Tri.V2, Tri.V3, p);
    Intersect(sepPlane, Tri.V1, Tri.V3, p2);
    InsertTriangle(frontList, Tri.V1, Tri.V2, p, Tri);
    InsertTriangle(frontList, Tri.V1, p, p2, Tri);
    InsertTriangle(backList, p2, p, Tri.V3, Tri);
  end
  else if (signOfV1 < 0) and (signOfV2 < 0) then
  begin //Vertex 3 d
    Intersect(sepPlane, Tri.V2, Tri.V3, p);
    Intersect(sepPlane, Tri.V1, Tri.V3, p2);
    InsertTriangle(backList, Tri.V1, Tri.V2, p, Tri);
    InsertTriangle(backList, Tri.V1, p, p2, Tri);
    InsertTriangle(frontList, p, p2, Tri.V3, Tri);
  end;
end;

{ Make BSP tree structure }
function TThreeDSolid.MakeBSPTree(l: TList): TBSPNode;
var
  backList, frontList, root, Tri, p: TList;
  signOfV1, signOfV2, signOfV3: double;
  node: TBSPNode;
begin
  if not Assigned(l) then
  begin
    result := nil;
    Exit;
  end;
  backList := nil;
  frontList := nil;
  root := l; //Set the root as the first triangle in the list l
  Tri := root.Next;
  while Assigned(Tri) do
  begin
    signOfV1 := CalcSign(Tri.Tri.V1, root.Tri);
    signOfV2 := CalcSign(Tri.Tri.V2, root.Tri);
    signOfV3 := CalcSign(Tri.Tri.V3, root.Tri);
    if (signOfV1 >= 0) and (signOfV2 >= 0) and (signOfV3 >= 0) then
      AddList(@frontList, Tri.Tri) //Triangle is in front of root
    else if (signOfV1 <= 0) and (signOfV2 <= 0) and (signOfV3 <= 0) then
      AddList(@backList, Tri.Tri) //Triangle is in back of root
    else
      Split(@frontList, @backList, signOfV1, signOfV2, signOfV3, root.Tri,
        Tri.Tri);
    Tri := Tri.Next;
  end;

  node := MakeBSPNode(root.Tri); //Add node for the root triangle
  if node = nil then
  begin
    ShowMessage('MakeBSPTree: Out of Memory');
    result := nil;
    Exit;
  end;

  node.Outside := MakeBSPTree(frontList);
  node.Inside := MakeBSPTree(backList);

  //backList und frontList freigeben
  p := frontList;
  while p <> nil do
  begin
    frontList := p.Next;
    p.Free;
    p := frontList;
  end;
  p := backList;
  while p <> nil do
  begin
    backList := p.Next;
    p.Free;
    p := backList;
  end;
  result := node;
end;

{ Convert world coordinates to display coordinates }
procedure TThreeDSolid.WorldToDisplay(x, y, z: double; var pc: TPoint);
var
  xc, yc, zc, xw, yw: double;
begin
  xc := (x * A1.x + y * A1.y + z * A1.z + Offsx) * DVal;
  yc := (x * A2.x + y * A2.y + z * A2.z + Offsy) * DVal;
  zc := x * A3.x + y * A3.y + z * A3.z + Offsz;
  xw := xc / zc;
  yw := yc / zc; //geändert: Vorzeichen in C enthalten! vorher: ym := yc/-zc;
  pc.x := Round(A * xw + B); //siehe TThreeD.WorldToPC
  pc.y := Round(C * yw + d);
end;

{ Precompute the d term in the plane equation for each polygon.
  The normal of the plane and a point on the plane -- one of its vertices --
  are used to solve for d }
procedure TThreeDSolid.CalcPlaneEqs;
var
  t: TTriangle;
begin
  t := TriList;
  while t <> nil do
  begin
    t.ComputeD;
    t := t.Next;
  end;
end;

{ Calculate the normals of all the triangles }
procedure TThreeDSolid.CalcTriNormals;
var
  t: TTriangle;
begin
  t := TriList;
  while t <> nil do
  begin
    t.ComputeNormal;
    t := t.Next;
  end;
end;

{ Precompute the centroids of all the triangles }
procedure TThreeDSolid.PrecomputeCentroids;
var
  t: TTriangle;
begin
  t := TriList;
  while t <> nil do
  begin
    t.ComputeCentroid;
    t := t.Next;
  end;
end;

{ Return the color to paint the polygon. Only ambient and diffuse lighting
  is used. You are looking at point p and the variable 'normal' is the normal
  ot the polygon. ColorIndex specifies the desired color of the polygon. }
function TThreeDSolid.ComputeColor(var p: Point; var Normal: Vector;
  colorNdx: Integer): TColor;
var
  l: Vector;
  lDotN, temp: double;
  iRed, iGreen, iBlue: Integer;
  Red, Green, Blue: Byte;
begin
  if Shading then
  begin
    //zunächst den diffusen Anteil in lDotN ermitteln
    l := Subtract(Light, p);
    Normalize(l);
    lDotN := Dot(l, Normal);
    temp := lDotN * Diffuse * NUM_SHADES;
    if lDotN <= 0 then
      lDotN := -temp
    else
      lDotN := temp;
    //dann den ambienten Anteil in temp addieren
    temp := lDotN + Ambient * NUM_SHADES;
  end
  else
    temp := 0;

  //mit temp den Grundwert beinflussen
  iRed := Round(ColorPal[colorNdx].r + temp);
  iGreen := Round(ColorPal[colorNdx].g + temp);
  iBlue := Round(ColorPal[colorNdx].B + temp);
  //Begrenzen auf 255
  if iRed > 255 then
    iRed := 255;
  if iGreen > 255 then
    iGreen := 255;
  if iBlue > 255 then
    iBlue := 255;
  {
    if iRed < 0 then iRed := 0;
    if iGreen < 0 then iGreen := 0;
    if iBlue < 0 then iBlue := 0;
    }
  //nächstliegende Farbe aus Palette auswählen
  Red := iRed;
  Green := iGreen;
  Blue := iBlue;
  result := RGB(Red, Green, Blue);
  //result := PaletteRGB(Red,Green,Blue);
  //result := PaletteRGB(Red,0,0);
end;

{ Display the triangle }
procedure TThreeDSolid.DisplayTriangle(Canvas: TCanvas; Tri: TTriangle);
var
  t: array [0 .. 3] of TPoint; //Screen coordinates of triangle to display
begin
  WorldToDisplay(Tri.V1.x, Tri.V1.y, Tri.V1.z, t[0]);
  WorldToDisplay(Tri.V2.x, Tri.V2.y, Tri.V2.z, t[1]);
  WorldToDisplay(Tri.V3.x, Tri.V3.y, Tri.V3.z, t[2]);
  t[3] := t[0]; //Close the triangle
  Canvas.Brush.Color := ComputeColor(Tri.Centroid, Tri.Normal, Tri.ColorIndex);
  Canvas.Pen.Color := Canvas.Brush.Color;
  Canvas.Polygon(t);
end;

{ Display the figure stored in the BSP tree }
procedure TThreeDSolid.View(Canvas: TCanvas);
begin
  if WireFrame then
    inherited View(Canvas)
  else
    TraverseTree(Canvas, BSPTree);
end;

{ Traverse a BSP tree, rendering a three-dimensional scene }
procedure TThreeDSolid.TraverseTree(Canvas: TCanvas; tree: TBSPNode);
var
  s: Vector;
begin
  if tree = nil then
    Exit;
  s.x := From.x - tree.Tri.V1.x;
  s.y := From.y - tree.Tri.V1.y;
  s.z := From.z - tree.Tri.V1.z;
  Normalize(s);
  if Dot(s, tree.Tri.Normal) > 0 then
  begin //The eye is in front of the polygon
    TraverseTree(Canvas, tree.Inside);
    DisplayTriangle(Canvas, tree.Tri);
    TraverseTree(Canvas, tree.Outside);
  end
  else
  begin //The eye is in back of the polygon
    TraverseTree(Canvas, tree.Outside);
    DisplayTriangle(Canvas, tree.Tri);
    TraverseTree(Canvas, tree.Inside);
  end;
end;

end.
