unit ThreeD;

interface

uses
  System.Classes,
  System.SysUtils,
  System.Types,
  Vcl.Graphics,
  Vector3D;

const
  EM_FILEOPENERROR = -100;
  EM_FILETOOBIG = -101;
  EM_TOOMANYCOLORS = -102;
  NUMCONNECTIONS = 905; { An object can have this many vertices }
  NUMVERTICES = 125; { An object can have this many connections }
  NUMCOLORS = 16; { in Unit Windows definiert zu 24 }
  NUMSHADES = 32;
  NUM_SHADES = 255;
  NOEDGE: Integer = $00;
  LEFTEDGE: Integer = $01;
  RIGHTEDGE: Integer = $02;
  BOTTOMEDGE: Integer = $04;
  TOPEDGE: Integer = $08;
  VLL = 0; { Borders of viewing region on the screen }
  VRR = 1000; { vorher 1000 }
  VTT = 0;
  VBB = 1000; { vorher 1000 }
  TOL = 0.001;
  PiD180 = 0.017453293;

type
  VECTOR = vec3;
  TVerticeArray = array [0 .. NUMVERTICES] of VECTOR;

  RGBColor = record
    r, g, b: Byte;
  end;

  { A three dimensional viewing class }
  TThreeD = class
  public
    A, B, C, D, DVal: double;
    From, At, Up: VECTOR; { Viewing parameters }
    Angle: double; { The viewing angle }
    A1, A2, A3: VECTOR; { Used in three-dimensional transform }
    Connect: array [0 .. NUMCONNECTIONS] of Integer; { Vertex connections }
    Points: TVerticeArray; { Vertices }
    Vl, Vt, Vr, Vb: Integer; { Screen boundaries of viewing area }
    Length: Integer; { Number of vertex connections }
    Vertices: Integer; { Number of vertices }
    ObjMinx, ObjMaxx: double; { Extent of three-dimensional objects }
    ObjMiny, ObjMaxy: double;
    ObjMinz, ObjMaxz: double;
    Offsx, Offsy, Offsz: double; { Transform variables }
    Dist: VECTOR; { Distance between the from and at points }
    { neue Felder: }
    ColorPal: array [0 .. NUMCOLORS] of RGBColor;
    NumColor: Integer;
    constructor Create;
    procedure Display(Canvas: TCanvas); { High-level display routine }
    function Read3DObject(filename: string): Integer; virtual;
    function Read3DModel: Integer; virtual;
    procedure MinMax;
    procedure WORLDtoPC(xw, yw: double; var pc: TPoint);
    function Code(x, y, z: double): Integer;
    procedure SetAt;
    procedure SetFrom; virtual;
    procedure SetEye;
    procedure View(Canvas: TCanvas); virtual;
    procedure TransformSeg(Canvas: TCanvas; v1, v2: VECTOR; var pc1, pc2: TPoint);
    procedure Clip3D(Canvas: TCanvas; x1, y1, z1, x2, y2, z2: double; var pc1, pc2: TPoint); virtual;
  end;

procedure PaintBackGround(Image: TBitmap);

implementation

procedure PaintBackGround(Image: TBitmap);
var
  r: TRect;
begin
  r := Rect(0, 0, Image.Width, Image.Height);
  with Image.Canvas do
  begin
    Brush.Color := clWhite;
    FillRect(r);
  end;
end;

{ Return the larger of two values }
function MaxOf(val1, val2: double): double;
begin
  if val1 > val2 then
    Result := val1
  else
    Result := val2;
end;

{ The constructor sets up various default values for the camera model }
constructor TThreeD.Create;
begin
  Vb := VBB;
  Vt := VTT; { Set up the dimensions of }
  Vr := VRR;
  Vl := VLL; { the viewing region }
  A := (Vr - Vl) / 2; { Set viewport and window }
  B := Vl - A * (-1); { mapping variables }
  C := (Vt - Vb) / 2;
  D := Vb - C * (-1);
  { Set default values for from, at and up vectors }
  At.x := 0.0;
  At.y := 0.0;
  At.z := 0.0;
  From.x := 0;
  From.y := 0;
  From.z := 10;
  Up.x := 0.0;
  Up.y := 10;
  Up.z := 0.0;
  Angle := 60.0 * 0.017453293; { Convert to radians }
end;

procedure TThreeD.Display(Canvas: TCanvas);
begin
  SetEye;
  View(Canvas);
end;

{ Convert clipped world coordinates to the window's coordinates }
procedure TThreeD.WORLDtoPC(xw, yw: double; var pc: TPoint);
begin
  pc.x := Round(A * xw + B);
  pc.y := Round(C * yw + D);
end;

{ Return the minimum and maximum values in the Points array for the X, Y, and Z axes. }
procedure TThreeD.MinMax;
var
  i: Integer;
begin
  ObjMinx := 3200;
  ObjMiny := 3200;
  ObjMinz := 3200;
  ObjMaxx := -3200;
  ObjMaxy := -3200;
  ObjMaxz := -3200;
  for i := 1 to Vertices do
  begin
    if (Points[i].x > ObjMaxx) then
      ObjMaxx := Points[i].x
    else if (Points[i].x < ObjMinx) then
      ObjMinx := Points[i].x;
    if (Points[i].y > ObjMaxy) then
      ObjMaxy := Points[i].y
    else if (Points[i].y < ObjMiny) then
      ObjMiny := Points[i].y;
    if (Points[i].z > ObjMaxz) then
      ObjMaxz := Points[i].z
    else if (Points[i].z < ObjMinz) then
      ObjMinz := Points[i].z;
  end;
end;

{ Routine to provide a default value for the at point.
  It is set to the midpoint of the extents of the object. }
procedure TThreeD.SetAt;
begin
  MinMax;
  At.x := (ObjMinx + ObjMaxx) / 2.0;
  At.y := (ObjMiny + ObjMaxy) / 2.0;
  At.z := (ObjMinz + ObjMaxz) / 2.0;
end;

{ Routine to provide a default value for the from point. It
  is dependent on the at point and the view angle. }
procedure TThreeD.SetFrom;
const
  { Ratio used to determine from point. It is based on size of object. }
  Width: double = 1.7;
begin
  { hier geändert am 20.04.97 }
  From.x := At.x;
  From.y := At.y;
  From.z := At.z + (ObjMaxz - ObjMinz) / 2.0 + Width * MaxOf
    ((ObjMaxx - ObjMinx) / 2.0, (ObjMaxy - ObjMiny) / 2.0);
end;

{ There must be a valid object in the Points array before calling this
  function. It sets up the various variables used in transforming an
  object from world to eye coordinates. }
procedure TThreeD.SetEye;
var
  temp: VECTOR;
  tempmag: double;
begin
  DVal := cos(Angle / 2.0) / sin(Angle / 2.0);
  Dist := Subtract(At, From);

  temp := Dist;
  tempmag := Mag(Dist);
  A3 := Divide(temp, tempmag); // Einheitsvektor in Blickrichtung (z)

  temp := Cross(Dist, Up);
  tempmag := Mag(temp);
  A1 := Divide(temp, tempmag); // Einheitsvektor auf x-Achse des Bildes

  temp := Cross(A1, A3);
  tempmag := Mag(temp);
  A2 := Divide(temp, tempmag); // Einheitsvektor auf y-Achse des Bildes (UP)

  Offsx := -(A1.x * From.x + A1.y * From.y + A1.z * From.z);
  Offsy := -(A2.x * From.x + A2.y * From.y + A2.z * From.z);
  Offsz := -(A3.x * From.x + A3.y * From.y + A3.z * From.z);
end;

{ Return a code specifying which edge in the viewing pyramid was
  crossed. There may be more than one. }
function TThreeD.Code(x, y, z: double): Integer;
var
  C: Integer;
begin
  C := NOEDGE;
  if (x < -z) then
    C := C or LEFTEDGE;
  if (x > z) then
    C := C or RIGHTEDGE;
  if (y < -z) then
    C := C or BOTTOMEDGE;
  if (y > z) then
    C := C or TOPEDGE;
  Result := C;
end;

{ Clip the line segment in 3D coordinates to the viewing pyramid.
  The clipped coordinates are returned as screen coordinates
  in the variables (pc1.x,pc1.y) and (pc2.x,pc2.y). }
procedure TThreeD.Clip3D(Canvas: TCanvas; x1, y1, z1, x2, y2, z2: double;
  var pc1, pc2: TPoint);
var
  C, c1, c2: Integer;
  x, y, z, t: double;
begin
  c1 := Code(x1, y1, z1);
  c2 := Code(x2, y2, z2);
  while ((c1 <> NOEDGE) or (c2 <> NOEDGE)) do
  begin
    { Don't draw anything if the line is not in the viewing pyramid: }
    if ((c1 and c2) <> NOEDGE) then
      Exit;

    C := c1;
    if (C = NOEDGE) then
      C := c2;

    x := 0.0;
    y := 0.0;
    z := 0.0;
    if ((C and LEFTEDGE) = LEFTEDGE) then
    begin
      { Crosses left edge }
      t := (z1 + x1) / ((x1 - x2) - (z2 - z1));
      z := t * (z2 - z1) + z1;
      x := -z;
      y := t * (y2 - y1) + y1;
    end
    else if ((C and RIGHTEDGE) = RIGHTEDGE) then
    begin
      { Crosses right edge }
      t := (z1 - x1) / ((x2 - x1) - (z2 - z1));
      z := t * (z2 - z1) + z1;
      x := z;
      y := t * (y2 - y1) + y1;
    end
    else if ((C and BOTTOMEDGE) = BOTTOMEDGE) then
    begin
      { Crosses bottom edge }
      t := (z1 + y1) / ((y1 - y2) - (z2 - z1));
      z := t * (z2 - z1) + z1;
      x := t * (x2 - x1) + x1;
      y := -z;
    end
    else if ((C and TOPEDGE) = TOPEDGE) then
    begin
      { Crosses top edge }
      t := (z1 - y1) / ((y2 - y1) - (z2 - z1));
      z := t * (z2 - z1) + z1;
      x := t * (x2 - x1) + x1;
      y := z;
    end;

    if (C = c1) then
    begin
      x1 := x;
      y1 := y;
      z1 := z;
      c1 := Code(x, y, z);
    end
    else
    begin
      x2 := x;
      y2 := y;
      z2 := z;
      c2 := Code(x, y, z);
    end;
  end;
  if (z1 <> 0) then
  begin
    WORLDtoPC(x1 / z1, y1 / z1, pc1);
    WORLDtoPC(x2 / z2, y2 / z2, pc2);
  end
  else
  begin
    WORLDtoPC(x1, y1, pc1);
    WORLDtoPC(x2, y2, pc2);
  end;
  Canvas.MoveTo(pc1.x, pc1.y);
  Canvas.LineTo(pc2.x, pc2.y);
end;

{ Transform the segment connecting the two vectors into
  the viewing plane. Clip3D clips and draws the line if visible. }
procedure TThreeD.TransformSeg(Canvas: TCanvas; v1, v2: VECTOR; var pc1, pc2: TPoint);
var
  x1, y1, z1, x2, y2, z2: double;
begin
  x1 := (v1.x * A1.x + v1.y * A1.y + v1.z * A1.z + Offsx) * DVal;
  y1 := (v1.x * A2.x + v1.y * A2.y + v1.z * A2.z + Offsy) * DVal;
  z1 := (v1.x * A3.x + v1.y * A3.y + v1.z * A3.z + Offsz);
  x2 := (v2.x * A1.x + v2.y * A1.y + v2.z * A1.z + Offsx) * DVal;
  y2 := (v2.x * A2.x + v2.y * A2.y + v2.z * A2.z + Offsy) * DVal;
  z2 := (v2.x * A3.x + v2.y * A3.y + v2.z * A3.z + Offsz);
  Clip3D(Canvas, x1, y1, z1, x2, y2, z2, pc1, pc2);
end;

{ Increment through the Points array, which contains the vertices of the
  object and display them as you go. This will draw out the object. }
procedure TThreeD.View(Canvas: TCanvas);
var
  i, startOfSide: Integer;
  pc1, pc2: TPoint;
begin
  i := 1;
  while (i < Length) do
  begin
    startOfSide := i;
    i := i + 1;
    while (Connect[i] > 0) do
    begin
      TransformSeg(Canvas, Points[Connect[i - 1]], Points[Connect[i]], pc1,
        pc2);
      i := i + 1;
    end;
    { Close off the polygon }
    TransformSeg(Canvas, Points[Connect[i - 1]], Points[Connect[startOfSide]],
      pc1, pc2);
    { Skip the negative value in the Connect array; it'll be
      used in Chapter 16 to specify the polygon's color. }
    i := i + 1;
  end;
end;

function TThreeD.Read3DObject(filename: string): Integer;
var
  i: Integer;
  infile: TextFile;
begin
  AssignFile(infile, filename);
  Reset(infile);
  try
    { Anzahl der Farben einlesen }
    read(infile, NumColor);
    if NumColor >= NUMCOLORS then
      raise EInOutError.Create('EM_TOOMANYCOLORS in Read3DSolid');
    { Farben einlesen }
    if NumColor > 0 then // no color palette if 0 or negative
      for i := 1 to NumColor do
        read(infile, ColorPal[i].r, ColorPal[i].g, ColorPal[i].b);
    { Anzahl der Koordinaten und Verbindungen einlesen }
    read(infile, Vertices, Length);
    if ((Vertices >= NUMVERTICES) or (Length >= NUMCONNECTIONS)) then
      raise EInOutError.Create('EM_FILETOOBIG in Read3DSolid');
    {  Koordinaten einlesen }
    for i := 1 to Vertices do
      read(infile, Points[i].x, Points[i].y, Points[i].z);
    { Verbindungen einlesen }
    for i := 1 to Length do
      read(infile, Connect[i]);
    Result := 1;
  finally
    CloseFile(infile);
  end;
end;

function TThreeD.Read3DModel: Integer;
  procedure ReadFace(i, A, b, C, D: Integer);
  begin
    Connect[i] := A;
    Connect[i + 1] := b;
    Connect[i + 2] := C;
    Connect[i + 3] := D;
  end;
  procedure ReadColor(i, r, g, b: Integer);
  begin
    ColorPal[i].r := r;
    ColorPal[i].g := g;
    ColorPal[i].b := b;
  end;
  procedure ReadPoint(i: Integer; x, y, z: double);
  begin
    Points[i].x := x;
    Points[i].y := y;
    Points[i].z := z;
  end;
begin
  { Anzahl der Farben einlesen }
  NumColor := 8;
  if NumColor >= NUMCOLORS then
    raise EInOutError.Create('EM_TOOMANYCOLORS in Read3DSolid');

  { Farben einlesen }
  ReadColor(1, 0, 0, 0);
  ReadColor(2, 0, 0, 128);
  ReadColor(3, 0, 128, 0);
  ReadColor(4, 128, 0, 0);
  ReadColor(5, 128, 128, 128);
  ReadColor(6, 0, 0, 255);
  ReadColor(7, 0, 255, 0);
  ReadColor(8, 255, 0, 0);

  { Anzahl der Koordinaten und Verbindungen einlesen }
  Vertices := 10;
  Length := 48;
  if ((Vertices >= NUMVERTICES) or (Length >= NUMCONNECTIONS)) then
    raise EInOutError.Create('EM_FILETOOBIG in Read3DSolid');

  { Koordinaten einlesen }
  ReadPoint(1, -1.5, -1.0, -1.0);
  ReadPoint(2, -1.5, 1.0, -1.0);
  ReadPoint(3, 1.5, 1.0, -1.0);
  ReadPoint(4, 1.5, -1.0, -1.0);

  ReadPoint(5, -1.5, -1.0, 1.0);
  ReadPoint(6, -1.5, 1.0, 1.0);
  ReadPoint(7, 1.5, 1.0, 1.0);
  ReadPoint(8, 1.5, -1.0, 1.0);

  ReadPoint(9, -2.5, 0.0, 0.0);
  ReadPoint(10, 2.5, 0.0, 0.0);

  { Verbindungen einlesen }
  ReadFace(1, 1, 2, 3, -1);
  ReadFace(5, 3, 4, 1, -1);
  ReadFace(9, 5, 8, 7, -2);
  ReadFace(13, 7, 6, 5, -2);
  ReadFace(17, 1, 4, 8, -3);
  ReadFace(21, 8, 5, 1, -3);
  ReadFace(25, 2, 6, 7, -4);
  ReadFace(29, 7, 3, 2, -4);
  ReadFace(33, 4, 3, 10, -5);
  ReadFace(37, 3, 7, 10, -8);
  ReadFace(41, 7, 8, 10, -6);
  ReadFace(45, 8, 4, 10, -7);

  Result := 1;
end;

end.
