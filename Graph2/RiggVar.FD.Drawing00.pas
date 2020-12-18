unit RiggVar.FD.Drawing00;

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
  RiggVar.RG.Types,
  System.UITypes,
  System.UIConsts,
  RiggVar.FB.Color,
  RiggVar.FD.Point,
  RiggVar.FD.Elements,
  RiggVar.FD.Drawings;

{$define WantPoly}

type
  { This will be the Live drawing - connected to the model of the real App. }
  TRggDrawingD00 = class(TRggDrawing)
  private
    A0B0: TRggLine;
    A0C0: TRggLine;
    B0C0: TRggLine;
    A0D0: TRggLine;

    B0D0: TRggLine;
    C0D0: TRggLine;

    A0A: TRggLine;
    B0B: TRggLine;
    C0C: TRggLine;
    E0E: TRggLine;
    P0P: TRggLine;

    CF: TRggLine;

    AC: TRggLine;
    BC: TRggLine;

    AB: TRggLine;
    AD: TRggLine;
    BD: TRggLine;

    N0F0: TRggLine;
    F0M: TRggLine;
    MF: TRggLine;

  public
    N0: TRggCircle;

    A0, A: TRggCircle;
    B0, B: TRggCircle;
    C0, C: TRggCircle;
    D0, D: TRggCircle;
    E0, E: TRggCircle;
    P0, P: TRggCircle;
    F0, F: TRggCircle;

    M: TRggCircle;

    D0D: TRggLine;
    DC: TRggLine;

{$ifdef WantPoly}
    MK: TRggPolyLine3D;
    KK: TRggPolyLine3D;
{$endif}

    Koordinaten: TRiggPoints;
    rP_D0: TPoint3D;
    rP_FX: TPoint3D;
    InitialZoom: single;

    InitialZoomDefault: single;

    FixPoint: TRiggPoint;
    FX: TRggFixpointCircle;

    constructor Create;
    procedure Transform(AM: TMatrix3D); override;
    procedure GoDark; override;
    procedure GoLight; override;

    function GetFixRggCircle: TRggCircle;
    procedure UpdateFromRigg;
    procedure UpdateFX;
  end;

implementation

{ TRggDrawingD00 }

constructor TRggDrawingD00.Create;
var
  L: TRggLine;
begin
  inherited;
  Name := 'D00-Live-Rigg';

  InitialZoomDefault := 0.09;

  InitialZoom := InitialZoomDefault;

  { Points }

  DefaultShowCaption := True;

  FX := TRggFixpointCircle.Create;
  FX.Caption := 'Fixpoint';
  Add(FX);

  N0 := TRggCircle.Create('N0');
  N0.StrokeColor := TRggColors.Green;

  A0 := TRggCircle.Create('A0');
  A0.StrokeColor := TRggColors.Green;

  B0 := TRggCircle.Create('B0');
  B0.StrokeColor := TRggColors.Red;

  C0 := TRggCircle.Create('C0');
  C0.StrokeColor := TRggColors.Yellow;

  D0 := TRggCircle.Create('D0');
  D0.StrokeColor := TRggColors.Blue;

  E0 := TRggCircle.Create('E0');
  E0.StrokeColor := TRggColors.Cyan;

  F0 := TRggCircle.Create('F0');
  F0.StrokeColor := TRggColors.Orange;

  P0 := TRggCircle.Create('P0');
  P0.StrokeColor := TRggColors.Brown;

  A := TRggCircle.Create('A');
  A.StrokeColor := TRggColors.Green;

  B := TRggCircle.Create('B');
  B.StrokeColor := TRggColors.Red;

  C := TRggCircle.Create('C');
  C.StrokeColor := TRggColors.Yellow;

  D := TRggCircle.Create('D');
  D.StrokeColor := TRggColors.Blue;

  E := TRggCircle.Create('E');
  E.StrokeColor := TRggColors.Cyan;

  F := TRggCircle.Create('F');
  F.StrokeColor := TRggColors.Gray;

  P := TRggCircle.Create('P');
  P.StrokeColor := TRggColors.Brown;

  M := TRggCircle.Create('M');
  M.StrokeColor := TRggColors.Gray;

  { Lines }

  DefaultShowCaption := False;

  L := TRggLine.Create('A0B0');
  L.StrokeColor := TRggColors.Gray;
  L.Point1 := A0;
  L.Point2 := B0;
  Add(L);
  A0B0 := L;

  L := TRggLine.Create('A0C0');
  L.StrokeColor := TRggColors.Gray;
  L.Point1 := A0;
  L.Point2 := C0;
  Add(L);
  A0C0 := L;

  L := TRggLine.Create('B0C0');
  L.StrokeColor := TRggColors.Gray;
  L.Point1 := B0;
  L.Point2 := C0;
  Add(L);
  B0C0 := L;

  { --- }

  L := TRggLine.Create('A0D0');
  L.StrokeColor := TRggColors.Black;
  L.Point1 := A0;
  L.Point2 := D0;
  Add(L);
  A0D0 := L;

  L := TRggLine.Create('B0D0');
  L.StrokeColor := TRggColors.Black;
  L.Point1 := B0;
  L.Point2 := D0;
  Add(L);
  B0D0 := L;

  L := TRggLine.Create('C0D0');
  L.StrokeColor := TRggColors.Black;
  L.Point1 := C0;
  L.Point2 := D0;
  Add(L);
  C0D0 := L;

  { --- }

  L := TRggLine.Create('A0A');
  L.StrokeColor := TRggColors.Red;
  L.Point1 := A0;
  L.Point2 := A;
  Add(L);
  A0A := L;

  L := TRggLine.Create('B0B');
  L.StrokeColor := TRggColors.Green;
  L.Point1 := B0;
  L.Point2 := B;
  Add(L);
  B0B := L;

  L := TRggLine.Create('C0C');
  L.StrokeColor := TRggColors.Yellow;
  L.Point1 := C0;
  L.Point2 := C;
  Add(L);
  C0C := L;

  L := TRggLine.Create('D0D');
  L.StrokeColor := TRggColors.Blue;
  L.Point1 := D0;
  L.Point2 := D;
  Add(L);
  D0D := L;

  L := TRggLine.Create('E0E');
  L.StrokeThickness := 1;
  L.StrokeColor := TRggColors.Cyan;
  L.Point1 := E0;
  L.Point2 := E;
  Add(L);
  E0E := L;

  L := TRggLine.Create('P0P');
  L.StrokeThickness := 1;
  L.StrokeColor := TRggColors.Brown;
  L.Point1 := P0;
  L.Point2 := P;
  Add(L);
  P0P := L;

  { --- }

  L := TRggLine.Create('AC');
  L.StrokeColor := TRggColors.Red;
  L.Point1 := A;
  L.Point2 := C;
  Add(L);
  AC := L;

  L := TRggLine.Create('BC');
  L.StrokeColor := TRggColors.Green;
  L.Point1 := B;
  L.Point2 := C;
  Add(L);
  BC := L;

  L := TRggLine.Create('DC');
  L.StrokeColor := TRggColors.Blue;
  L.Point1 := D;
  L.Point2 := C;
  Add(L);
  DC := L;

  { --- }

  L := TRggLine.Create('AB');
  L.StrokeColor := TRggColors.Lime;
  L.Point1 := A;
  L.Point2 := B;
  Add(L);
  AB := L;

  L := TRggLine.Create('AD');
  L.StrokeColor := TRggColors.Lime;
  L.Point1 := A;
  L.Point2 := D;
  Add(L);
  AD := L;

  L := TRggLine.Create('BD');
  L.StrokeColor := TRggColors.Lime;
  L.Point1 := B;
  L.Point2 := D;
  Add(L);
  BD := L;

  L := TRggLine.Create('CF');
  L.StrokeColor := TRggColors.Gray;
  L.Point1 := C;
  L.Point2 := F;
  Add(L);
  CF := L;

  { --- }

  L := TRggLine.Create('N0F0');
  L.StrokeColor := TRggColors.Antiquewhite;
  L.Point1 := N0;
  L.Point2 := F0;
  Add(L);
  N0F0 := L;

  L := TRggLine.Create('F0M');
  L.StrokeThickness := 2;
  L.StrokeColor := TRggColors.Antiquewhite;
  L.Point1 := F0;
  L.Point2 := M;
  Add(L);
  F0M := L;

  L := TRggLine.Create('MF');
  L.StrokeThickness := 2;
  L.StrokeColor := TRggColors.Orange;
  L.Point1 := M;
  L.Point2 := F;
  Add(L);
  MF := L;

{$ifdef WantPoly}
  KK := TRggPolyLine3D.Create('KK', High(TKoordLine) + 1);
  KK.StrokeThickness := 2;
  KK.StrokeColor := TRggColors.Yellow;
  KK.Point1 := D;
  KK.Point2 := C;
  KK.ShowPoly := True;
  KK.WantRotation := True;
  Add(KK);

  MK := TRggPolyLine3D.Create('MK', BogenMax + 1);
  MK.StrokeThickness := 10;
  MK.StrokeColor := TRggColors.Dodgerblue;
  MK.Point1 := D0;
  MK.Point2 := C;
  MK.ShowPoly := True;
  MK.WantRotation := True;
  Add(MK);
{$endif}

  Add(N0);

  Add(A0);
  Add(B0);
  Add(C0);
  Add(D0);
  Add(E0);
  Add(F0);
  Add(P0);

  Add(A);
  Add(B);
  Add(C);
  Add(D);
  Add(E);
  Add(F);
  Add(P);

  Add(M);

  FixPoint3D := D.Center.C;
  FixPoint := ooD;

  WantRotation := True;
  WantSort := True;

  E0.Visible := False;
  E.Visible := False;
  E0E.Visible := True;

  P0.Visible := False;
  P.Visible := False;
  P0P.Visible := False;
end;

procedure TRggDrawingD00.Transform(AM: TMatrix3D);
begin
  inherited;
{$ifdef WantPoly}
  MK.Transform;
  KK.Transform;
{$endif}
end;

procedure TRggDrawingD00.GoLight;
begin
  inherited;

  N0.StrokeColor := TRggColors.Plum;
  N0F0.StrokeColor := TRggColors.Plum;

  A0.StrokeColor := TRggColors.Red;
  A.StrokeColor := TRggColors.Red;
  A0A.StrokeColor := TRggColors.Red;

  B0.StrokeColor := TRggColors.Green;
  B.StrokeColor := TRggColors.Green;
  B0B.StrokeColor := TRggColors.Green;

  C0.StrokeColor := TRggColors.Yellow;
  C.StrokeColor := TRggColors.Yellow;
  C0C.StrokeColor := TRggColors.Yellow;
  MF.StrokeColor := TRggColors.Orange;

  D0.StrokeColor := TRggColors.Dodgerblue;
  D.StrokeColor := TRggColors.Dodgerblue;
  D0D.StrokeColor := TRggColors.Dodgerblue;

  E0.StrokeColor := TRggColors.Cyan;
  E.StrokeColor := TRggColors.Cyan;
  E0E.StrokeColor := TRggColors.Cyan;

  F0.StrokeColor := TRggColors.Plum;
  F.StrokeColor := TRggColors.Gray;
  F0M.StrokeColor := TRggColors.Gray;
  MF.StrokeColor := TRggColors.Orange;

  P0.StrokeColor := TRggColors.Beige;
  P.StrokeColor := TRggColors.Beige;
  P0P.StrokeColor := TRggColors.Beige;

  A0B0.StrokeColor := TRggColors.Gray;
  A0C0.StrokeColor := TRggColors.Gray;
  B0C0.StrokeColor := TRggColors.Gray;

  B0D0.StrokeColor := TRggColors.Plum;
  C0D0.StrokeColor := TRggColors.Plum;
  A0D0.StrokeColor := TRggColors.Plum;

  AC.StrokeColor := TRggColors.Red;
  BC.StrokeColor := TRggColors.Green;
  DC.StrokeColor := TRggColors.Blue;

  AB.StrokeColor := TRggColors.Lime;
  AD.StrokeColor := TRggColors.Lime;
  BD.StrokeColor := TRggColors.Lime;

  CF.StrokeColor := TRggColors.Dodgerblue;

{$ifdef WantPoly}
  KK.StrokeColor := TRggColors.Yellow;
  MK.StrokeColor := TRggColors.Dodgerblue;
{$endif}
end;

procedure TRggDrawingD00.GoDark;
begin
  inherited;

  N0.StrokeColor := TRggColors.White;
  N0F0.StrokeColor := TRggColors.White;

  A0.StrokeColor := TRggColors.Red;
  A.StrokeColor := TRggColors.Red;
  A0A.StrokeColor := TRggColors.Red;

  B0.StrokeColor := TRggColors.Green;
  B.StrokeColor := TRggColors.Green;
  B0B.StrokeColor := TRggColors.Green;

  C0.StrokeColor := TRggColors.Yellow;
  C.StrokeColor := TRggColors.Yellow;
  C0C.StrokeColor := TRggColors.Yellow;

  E0.StrokeColor := TRggColors.Cyan;
  E.StrokeColor := TRggColors.Cyan;
  E0E.StrokeColor := TRggColors.Cyan;

  F0.StrokeColor := TRggColors.White;
  F.StrokeColor := TRggColors.Gray;
  F0M.StrokeColor := TRggColors.Gray;
  MF.StrokeColor := TRggColors.Orange;

  P0.StrokeColor := TRggColors.Brown;
  P.StrokeColor := TRggColors.Brown;
  P0P.StrokeColor := TRggColors.Brown;

  D0.StrokeColor := TRggColors.Dodgerblue;
  D.StrokeColor := TRggColors.Dodgerblue;
  D0D.StrokeColor := TRggColors.Dodgerblue;

  F0.StrokeColor := TRggColors.Gray;
  F.StrokeColor := TRggColors.Gray;

  A0B0.StrokeColor := TRggColors.Gray;
  A0C0.StrokeColor := TRggColors.Gray;
  B0C0.StrokeColor := TRggColors.Gray;

  A0D0.StrokeColor := TRggColors.Cyan;
  B0D0.StrokeColor := TRggColors.Cyan;
  C0D0.StrokeColor := TRggColors.Cyan;

  AC.StrokeColor := TRggColors.Red;
  BC.StrokeColor := TRggColors.Green;
  DC.StrokeColor := TRggColors.Dodgerblue;

  AB.StrokeColor := TRggColors.Lime;
  AD.StrokeColor := TRggColors.Lime;
  BD.StrokeColor := TRggColors.Lime;

  CF.StrokeColor := TRggColors.Cyan;

{$ifdef WantPoly}
  KK.StrokeColor := TRggColors.Yellow;
  MK.StrokeColor := TRggColors.Dodgerblue;
{$endif}
end;

function TRggDrawingD00.GetFixRggCircle: TRggCircle;
var
  cr: TRggCircle;
begin
  cr := D;
  case FixPoint of
    ooN0: ;
    ooA0: cr := A0;
    ooB0: cr := B0;
    ooC0: cr := C0;
    ooD0: cr := D0;
    ooE0: cr := E0;
    ooF0: cr := F0;
    ooP0: cr := P0;
    ooA: cr := A;
    ooB: cr := B;
    ooC: cr := C;
    ooD: cr := D;
    ooE: cr := E;
    ooF: cr := F;
    ooP: cr := P;
    ooM: cr := M;
  end;
  result := cr;
end;

procedure TRggDrawingD00.UpdateFromRigg;
var
  t: TPoint3D;

  procedure Temp(cr: TRggCircle; oo: TRiggPoint);
  begin
    t := Koordinaten.V[oo] - rP_FX;
    cr.Center.X := t.X * InitialZoom;
    cr.Center.Y := - t.Z * InitialZoom;
    cr.Center.Z := -t.Y * InitialZoom;
    cr.Save;
  end;
begin
  rP_D0 := Koordinaten.V[ooD0];
  rP_FX := Koordinaten.V[FixPoint];

  Temp(N0, ooN0);

  Temp(A0, ooA0);
  Temp(B0, ooB0);
  Temp(C0, ooC0);
  Temp(D0, ooD0);
  Temp(E0, ooE0);
  Temp(F0, ooF0);
  Temp(P0, ooP0);

  Temp(A, ooA);
  Temp(B, ooB);
  Temp(C, ooC);
  Temp(D, ooD);
  Temp(E, ooE);
  Temp(F, ooF);
  Temp(P, ooP);

  Temp(M, ooM);

  UpdateFX;
end;

procedure TRggDrawingD00.UpdateFX;
var
  cr: TRggCircle;
begin
  cr := GetFixRggCircle;
  FixPoint3D := cr.Center.C;
  FX.OriginalCenter := cr.OriginalCenter;
  FX.Center := cr.Center;
end;

end.
