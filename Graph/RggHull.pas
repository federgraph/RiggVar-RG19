unit RggHull;

interface

uses
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  RggTypes,
  RggCalc,
  RggMatrix,
  Vector3D,
  RggGraph,
  RggBootGraph,
  RggRaumGraph;

type
  THullGraph = class(TRggGraph)
  protected
    { Koordinaten }
    vert: TVertArrayF; { Gleitkomma-Koordinaten }
    tvert: TVertArrayI; { Integer-Koordinaten - transformed }
    nvert: Integer;
    { Connections }
    con: TConArray;
    ncon: Integer;
    gr: TConColors;
    { Matrix }
    mat: TMatrix4x4;
    xmin, xmax, ymin, ymax, zmin, zmax: double;
    zfac: double;
    procedure ReadVertex;
    procedure ReadVertex1;
    procedure ReadVertex2(Memo: TStrings);
    procedure ReadCons;
    procedure ReadCons1;
    procedure ReadCons2(k, l: Integer);
    function AddVert(x, y, z: single): Integer;
    procedure AddLine(p1, p2: Integer);
    procedure Paint(g: TCanvas);
    procedure FindBB;
    function GetColor(i: Integer): TColor;
  public
    Factor: vec3; // FaktorX, FaktorY, FaktorZ: double;
    ModelFactor: vec3;
    VertexFileName: string;
    VertexMemo: TStrings;
    constructor Create; override;
    destructor Destroy; override;
    procedure Load;
    procedure Update; override;
    procedure DrawToCanvas(Canvas: TCanvas); override;
    procedure GetPlotList(List: TStringList); override;
  end;

var
  HullGraph: THullGraph;

implementation

uses
  RiggVar.FB.Classes;

constructor THullGraph.Create;
var
  xw, yw, zw: double;
begin
  inherited Create;
  Factor.x := 1;
  Factor.y := 1;
  Factor.z := 1;
  ModelFactor := Factor;
  Load;
  mat := TMatrix4x4.Create;
  mat.XRot(20);
  mat.YRot(30);
  FindBB();
  //compress();
  xw := xmax - xmin;
  yw := ymax - ymin;
  zw := zmax - zmin;
  zfac := xw;
  if (yw > xw) then
    zfac := yw;
  if (zw > xw) then
    zfac := zw;
end;

destructor THullGraph.Destroy;
begin
  mat.Free;
  inherited Destroy;
end;

procedure THullGraph.Load;
begin
  nvert := 0;
  ncon := 0;
  ReadVertex;
  ReadCons;
  GrafikOK := True;
end;

{ Add a vertex to the Model }
function THullGraph.AddVert(x, y, z: single): Integer;
var
  i: Integer;
begin
  i := nvert;
  if i >= maxvert then
  begin
    result := nvert;
    Exit;
  end;
  i := i * 3;
  vert[i] := Factor.x * ModelFactor.x * x;
  vert[i + 1] := Factor.y * ModelFactor.y * y;
  vert[i + 2] := Factor.z * ModelFactor.z * z;
  Inc(nvert);
  result := nvert;
end;

{ Add a line from vertex p1 to vertex p2 }
procedure THullGraph.AddLine(p1, p2: Integer);
var
  i, t: Integer;
begin
  i := ncon;
  if (p1 >= nvert) or (p2 >= nvert) then
    Exit;
  if (i >= maxcon) then
    Exit;
  if (p1 > p2) then
  begin
    { vertauschen }
    t := p1;
    p1 := p2;
    p2 := t;
  end;
  con[i] := (p1 shl 16) or p2;
  ncon := i + 1;
end;

procedure THullGraph.Update;
begin
  if not GrafikOK then
    Exit;
  if nvert <= 0 then
    Exit;
  mat.Identity;
  mat.Translate(-FixPunkt[x], -FixPunkt[y], -FixPunkt[z]);
  mat.Multiply(Rotator.Matrix);
  { x und z werden abgebildet (siehe GBox3D) }
  mat.ScaleXYZ(Zoom, 20 / zfac, Zoom);
  mat.Translate(NOffset.x, 4, -NOffset.y);
  mat.Transform(vert, tvert, nvert);
  Updated := True;
end;

procedure THullGraph.DrawToCanvas(Canvas: TCanvas);
begin
  if not GrafikOK then
    Exit;
  if not Updated then
    Update;
  Paint(Canvas);
end;

procedure THullGraph.Paint(g: TCanvas);
var
  i, lim, t, p1, p2, grey: Integer;
  c: TConArray;
  v: TVertArrayI;
begin
  if nvert <= 0 then
    Exit;

  lim := ncon;
  c := con;
  v := tvert;
  if (lim <= 0) or (nvert <= 0) then
    Exit;

  for i := 0 to lim - 1 do
  begin
    { Indizes in das Vertice-array bestimmen }
    t := c[i]; // T wie Temp
    p1 := ((t shr 16) and $FFFF) * 3; // Index Punkt1
    p2 := (t and $FFFF) * 3; // Index Punkt2

    { Farbe bestimmen Variante 1 }
    if Coloriert then
    begin
      grey := v[p1 + 1] + v[p2 + 1]; // Summe der z-Werte
      if (grey < 0) then
        grey := 0; // grey zwischen 0 und 15
      if (grey > 15) then
        grey := 15;
      g.Pen.Color := GetColor(grey);
    end
    else
      g.Pen.Color := clBtnFace;

    { Farbe bestimmen, Varinate 2 }
    // if Coloriert then
    //   g.Pen.Color := GetColor(i)
    // else
    //   g.Pen.Color := clBtnFace;

    g.Pen.Width := 1;
    { Linie zeichnen }
    g.MoveTo(v[p1], -v[p1 + 2]);
    g.LineTo(v[p2], -v[p2 + 2]);
  end;
end;

function THullGraph.GetColor(i: Integer): TColor;
var
  idx: Word;
  R, G, B: Byte;
begin
  R := 0;
  G := 0;
  B := 1;
  idx := Round(R * 32 + G * 64 + B * 96 + i * 2);
  result := PaletteIndex(idx);
end;

{ Find the bounding box of this model }
procedure THullGraph.FindBB;
var
  v: TVertArrayF;
  lxmin, lymin, lzmin, lxmax, lymax, lzmax: single;
  x, y, z: single;
  i, j: Integer;
begin
  if (nvert <= 0) then
    Exit;

  v := vert;
  lxmin := v[0];
  lxmax := lxmin;
  lymin := v[1];
  lymax := lymin;
  lzmin := v[2];
  lzmax := lzmin;
  for j := nvert downto 0 do
  begin
    i := j * 3;
    x := v[i];
    if (x < lxmin) then
      lxmin := x;
    if (x > lxmax) then
      lxmax := x;

    y := v[i + 1];
    if (y < lymin) then
      lymin := y;
    if (y > lymax) then
      lymax := y;

    z := v[i + 2];
    if (z < lzmin) then
      lzmin := z;
    if (z > lzmax) then
      lzmax := z;
  end;
  xmax := lxmax;
  xmin := lxmin;
  ymax := lymax;
  ymin := lymin;
  zmax := lzmax;
  zmin := lzmin;
end;

procedure THullGraph.GetPlotList(List: TStringList);
var
  i, t, p1, p2: Integer;
  c: TConArray;
  v: TVertArrayI;
  s: string;
  SavedZoom: double;
begin
  if (ncon <= 0) or (nvert <= 0) then
    Exit;
  if not GrafikOK then
    Exit;
  SavedZoom := Zoom;
  Zoom := 10;
  if not Updated then
    Update;
  List.add('SP 1;');
  c := con;
  v := tvert;
  for i := 0 to ncon - 1 do
  begin
    { Indizes in das Vertice-array bestimmen }
    t := c[i]; //T  wie Temp
    p1 := ((t shr 16) and $FFFF) * 3; // Index Punkt1
    p2 := (t and $FFFF) * 3; // Index Punkt2
    // g.MoveTo(v[p1], -v[p1 + 2]);
    S := Format('PU %d %d;', [v[p1], -v[p1 + 2]]);
    List.Add(S);
    // g.LineTo(v[p2], -v[p2 + 2]);
    s := Format('PD %d %d;', [ v[p2], -v[p2 + 2] ]);
    List.Add(s);
  end;
  Zoom := SavedZoom;
end;

procedure THullGraph.ReadVertex2(Memo: TStrings);
var
  i, Code: Integer;
  Zeile, Wort: string;
  a, b, c: Integer;

  { local procedure }
  procedure GetReal(var RealValue: double);
  begin
    Zeile := Trim(Zeile);
    Wort := TUtils.StripFirstWord(Zeile);
    if Wort = '' then
      Wort := Zeile;
    Val(Wort, RealValue, Code);
    if Code <> 0 then
      MessageBeep(0);
  end;

  { local procedure }
  procedure GetInteger(var IntValue: Integer);
  begin
    Zeile := Trim(Zeile);
    Wort := TUtils.StripFirstWord(Zeile);
    if Wort = '' then
      Wort := Zeile;
    Val(Wort, IntValue, Code);
    if Code <> 0 then
      MessageBeep(0);
  end;

begin
  { in Zeile 0 stehen die Faktoren * 100 }
  for i := 0 to Memo.Count - 1 do
  begin
    Zeile := Memo[i];
    if Zeile = '' then
      Continue;
    GetInteger(a);
    GetInteger(b);
    GetInteger(c);
    ModelFactor.x := a / 100;
    ModelFactor.y := b / 100;
    ModelFactor.z := c / 100;
    Break;
  end;

  for i := 1 to Memo.Count - 1 do
  begin
    Zeile := Memo[i];
    if Zeile = '' then
      Continue;
    GetInteger(a);
    GetInteger(b);
    GetInteger(c);
    AddVert(a, b, c);
  end;
end;

procedure THullGraph.ReadVertex;
var
  Memo: TStringList;
begin
  if VertexMemo <> nil then
    ReadVertex2(VertexMemo)
  else if VertexFileName <> '' then
  begin
    Memo := TStringList.Create;
    try
      Memo.LoadFromFile(VertexFileName);
      ReadVertex2(Memo);
    finally
      Memo.Free;
    end;
  end
  else
    ReadVertex1;
end;

procedure THullGraph.ReadVertex1;
begin
  ModelFactor.x := 1;
  ModelFactor.y := 1;
  ModelFactor.z := 1;

  AddVert(4200, 0, 328); // Steven Spant 1, Koord 1..7
  AddVert(4194, 0, 260);
  AddVert(4188, 0, 195);
  AddVert(4178, 0, 128);
  AddVert(4168, 0, 78);
  AddVert(4151, 0, 26);
  AddVert(4130, 0, 0);

  AddVert(4100, -157, 325); // Spant 2, Koord 8..20
  AddVert(4100, -149, 268);
  AddVert(4100, -126, 189);
  AddVert(4100, -100, 131);
  AddVert(4100, -69, 74);
  AddVert(4100, -30, 8);
  AddVert(4100, 0, -48);
  AddVert(4100, 30, 8);
  AddVert(4100, 69, 74);
  AddVert(4100, 100, 131);
  AddVert(4100, 126, 189);
  AddVert(4100, 149, 268);
  AddVert(4100, 157, 325);

  AddVert(4000, -244, 322); // Spant 3, Koord 21..33
  AddVert(4000, -237, 263);
  AddVert(4000, -219, 186);
  AddVert(4000, -193, 115);
  AddVert(4000, -159, 51);
  AddVert(4000, -88, -41);
  AddVert(4000, 0, -117);
  AddVert(4000, 88, -41);
  AddVert(4000, 159, 51);
  AddVert(4000, 193, 115);
  AddVert(4000, 219, 186);
  AddVert(4000, 237, 263);
  AddVert(4000, 244, 322);

  AddVert(3750, -402, 315); // pant 4, Koord 34..46
  AddVert(3750, -387, 263);
  AddVert(3750, -374, 176);
  AddVert(3750, -345, 96);
  AddVert(3750, -281, 7);
  AddVert(3750, -155, -93);
  AddVert(3750, 0, -178);
  AddVert(3750, 155, -93);
  AddVert(3750, 281, 7);
  AddVert(3750, 345, 96);
  AddVert(3750, 374, 176);
  AddVert(3750, 387, 263);
  AddVert(3750, 402, 315);

  AddVert(3400, -570, 308); // Spant 5, Koord 47..59
  AddVert(3400, -541, 253);
  AddVert(3400, -506, 166);
  AddVert(3400, -445, 42);
  AddVert(3400, -380, -30);
  AddVert(3400, -212, -126);
  AddVert(3400, 0, -202);
  AddVert(3400, 212, -126);
  AddVert(3400, 380, -30);
  AddVert(3400, 445, 42);
  AddVert(3400, 506, 166);
  AddVert(3400, 541, 253);
  AddVert(3400, 570, 308);

  AddVert(3000, -699, 302); // Spant 6, Koord 60..72
  AddVert(3000, -661, 248);
  AddVert(3000, -619, 163);
  AddVert(3000, -539, 26);
  AddVert(3000, -446, -54);
  AddVert(3000, -251, -138);
  AddVert(3000, 0, -205);
  AddVert(3000, 251, -138);
  AddVert(3000, 446, -54);
  AddVert(3000, 539, 26);
  AddVert(3000, 619, 163);
  AddVert(3000, 661, 248);
  AddVert(3000, 699, 302);

  AddVert(2400, -793, 297); // Spant 7, Koord 73..85
  AddVert(2400, -749, 245);
  AddVert(2400, -716, 167);
  AddVert(2400, -634, 31);
  AddVert(2400, -500, -65);
  AddVert(2400, -296, -135);
  AddVert(2400, 0, -191);
  AddVert(2400, 296, -135);
  AddVert(2400, 500, -65);
  AddVert(2400, 634, 31);
  AddVert(2400, 716, 167);
  AddVert(2400, 749, 245);
  AddVert(2400, 793, 297);

  AddVert(1800, -800, 290); // Spant 8, Koord 86..98
  AddVert(1800, -755, 241);
  AddVert(1800, -725, 173);
  AddVert(1800, -634, 34);
  AddVert(1800, -480, -58);
  AddVert(1800, -269, -115);
  AddVert(1800, 0, -161);
  AddVert(1800, 269, -115);
  AddVert(1800, 480, -58);
  AddVert(1800, 634, 34);
  AddVert(1800, 725, 173);
  AddVert(1800, 755, 241);
  AddVert(1800, 800, 290);

  AddVert(1000, -730, 275); // Spant 9, Koord 99..111
  AddVert(1000, -696, 237);
  AddVert(1000, -674, 185);
  AddVert(1000, -585, 56);
  AddVert(1000, -414, -23);
  AddVert(1000, -214, -64);
  AddVert(1000, 0, -97);
  AddVert(1000, 214, -64);
  AddVert(1000, 414, -23);
  AddVert(1000, 585, 56);
  AddVert(1000, 674, 185);
  AddVert(1000, 696, 237);
  AddVert(1000, 730, 275);

  AddVert(0, -580, 250); // Spant 10, Koord 112..124
  AddVert(0, -568, 226);
  AddVert(0, -560, 187);
  AddVert(0, -485, 89);
  AddVert(0, -300, 30);
  AddVert(0, -167, 13);
  AddVert(0, 0, 0);
  AddVert(0, 167, 13);
  AddVert(0, 300, 30);
  AddVert(0, 485, 89);
  AddVert(0, 560, 187);
  AddVert(0, 568, 226);
  AddVert(0, 580, 250);
end;

procedure THullGraph.ReadCons;
begin
  // ReadCons1;
  ReadCons2(10, 7);
end;

procedure THullGraph.ReadCons1;
  procedure addcon7(a1, a2, a3, a4, a5, a6, a7: Integer);
  begin
    AddLine(a1 - 1, a2 - 1);
    AddLine(a2 - 1, a3 - 1);
    AddLine(a3 - 1, a4 - 1);
    AddLine(a4 - 1, a5 - 1);
    AddLine(a5 - 1, a6 - 1);
    AddLine(a6 - 1, a7 - 1);
  end;
  procedure addcon10(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10: Integer);
  begin
    AddLine(a1 - 1, a2 - 1);
    AddLine(a2 - 1, a3 - 1);
    AddLine(a3 - 1, a4 - 1);
    AddLine(a4 - 1, a5 - 1);
    AddLine(a5 - 1, a6 - 1);
    AddLine(a6 - 1, a7 - 1);
    AddLine(a7 - 1, a8 - 1);
    AddLine(a8 - 1, a9 - 1);
    AddLine(a9 - 1, a10 - 1);
  end;

begin
  addcon7(1, 2, 3, 4, 5, 6, 7);

  addcon7(8, 9, 10, 11, 12, 13, 14);
  addcon7(14, 15, 16, 17, 18, 19, 20);

  addcon7(21, 22, 23, 24, 25, 26, 27);
  addcon7(27, 28, 29, 30, 31, 32, 33);

  addcon7(34, 35, 36, 37, 38, 39, 40);
  addcon7(40, 41, 42, 43, 44, 45, 46);

  addcon7(47, 48, 49, 50, 51, 52, 53);
  addcon7(53, 54, 55, 56, 57, 58, 59);

  addcon7(60, 61, 62, 63, 64, 65, 66);
  addcon7(66, 67, 68, 69, 70, 71, 72);

  addcon7(73, 74, 75, 76, 77, 78, 79);
  addcon7(79, 80, 81, 82, 83, 84, 85);

  addcon7(86, 87, 88, 89, 90, 91, 92);
  addcon7(92, 93, 94, 95, 96, 97, 98);

  addcon7(99, 100, 101, 102, 103, 104, 105);
  addcon7(105, 106, 107, 108, 109, 110, 111);

  addcon7(112, 113, 114, 115, 116, 117, 118);
  addcon7(118, 119, 120, 121, 122, 123, 124);

  addcon10(1, 8, 21, 34, 47, 60, 73, 86, 99, 112);
  addcon10(2, 9, 22, 35, 48, 61, 74, 87, 100, 113);
  addcon10(3, 10, 23, 36, 49, 62, 75, 88, 101, 114);
  addcon10(4, 11, 24, 37, 50, 63, 76, 89, 102, 115);
  addcon10(5, 12, 25, 38, 51, 64, 77, 90, 103, 116);
  addcon10(6, 13, 26, 39, 52, 65, 78, 91, 104, 117);
  addcon10(7, 14, 27, 40, 53, 66, 79, 92, 105, 118);
  addcon10(6, 15, 28, 41, 54, 67, 80, 93, 106, 119);
  addcon10(5, 16, 29, 42, 55, 68, 81, 94, 107, 120);
  addcon10(4, 17, 30, 43, 56, 69, 82, 95, 108, 121);
  addcon10(3, 18, 31, 44, 57, 70, 83, 96, 109, 122);
  addcon10(2, 19, 32, 45, 58, 71, 84, 97, 110, 123);
  addcon10(1, 20, 33, 46, 59, 72, 85, 98, 111, 124);
end;

procedure THullGraph.ReadCons2(k, l: Integer);

  procedure AddSection(a, b, c, n: Integer);
  { a = 1.Punkt
    b = 2.Punkt
    c = Increment zwischen Punkten ab dem 2. Punkt
    n = Anzahl der Verbindungen
  }
  var
    i: Integer;
  begin
    AddLine(a-1, b-1);
    if n = 1 then Exit;
    for i := 2 to n do begin
      a := b;
      b := b + c;
      AddLine(a-1, b-1);
    end;
  end;

var
  SpantenZahl: Integer;
  LinienZahl: Integer;
  i, a, b, vs, vl: Integer;
begin
  { Beispiel-Eingaben }
  // k := 10; // Anzahl der Spanten einschließlich Steven
  // l := 7; // Anzahl der Linien

  SpantenZahl := k - 1; // Anzahl Spanten = 9
  LinienZahl := 2*l-1; // Anzahl Linien = 13
  vs := l - 1; // Anzahl Verbindungen eines Spantes = 6
  vl := k - 1; // Anzahl Verbindungen einer Linie = 9

  a := 1;
  AddSection(a, a + 1, 1, vs); { der Steven }
  a := l;
  for i := 1 to SpantenZahl do
  begin
    AddSection(a + 1, a + 2, 1, 2 * vs); { die Spanten }
    a := a + LinienZahl;
  end;

  a := 1;
  b := l + 1;
  for i := 1 to l - 1 do
  begin
    AddSection(a, b, LinienZahl, vl); { Linien links und Kiel }
    a := a + 1;
    b := b + 1;
  end;
  for i := l to LinienZahl do
  begin
    AddSection(a, b, LinienZahl, vl); { Linien rechts }
    a := a - 1;
    b := b + 1;
  end;
end;

end.
