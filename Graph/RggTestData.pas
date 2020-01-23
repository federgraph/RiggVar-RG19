unit RggTestData;

interface

uses
  RggTypes;

type
  TRggTestData = class
  public
    class function GetKoordinaten420: TRealRiggPoints; static;
  end;

implementation

class function TRggTestData.GetKoordinaten420: TRealRiggPoints;
var
  rp: TRealRiggPoints;
begin
  rp[ooA, x] := 2398;
  rp[ooA, y] := 425;
  rp[ooA, z] := 2496;

  rp[ooB, x] := 2398;
  rp[ooB, y] := -425;
  rp[ooB, z] := 2496;

  rp[ooC, x] := 2354;
  rp[ooC, y] := 0;
  rp[ooC, z] := 4470;

  rp[ooD, x] := 2618;
  rp[ooD, y] := 0;
  rp[ooD, z] := 2488;

  rp[ooE, x] := 2870;
  rp[ooE, y] := 0;
  rp[ooE, z] := 450;

  rp[ooF, x] := 2142;
  rp[ooF, y] := 0;
  rp[ooF, z] := 5970;

  rp[ooP, x] := 2398;
  rp[ooP, y] := 0;
  rp[ooP, z] := 2496;

  rp[ooA0, x] := 2560;
  rp[ooA0, y] := 765;
  rp[ooA0, z] := 430;

  rp[ooB0, x] := 2560;
  rp[ooB0, y] := -765;
  rp[ooB0, z] := 430;

  rp[ooC0, x] := 4140;
  rp[ooC0, y] := 0;
  rp[ooC0, z] := 340;

  rp[ooD0, x] := 2870;
  rp[ooD0, y] := 0;
  rp[ooD0, z] := -100;

  rp[ooE0, x] := 2970;
  rp[ooE0, y] := 0;
  rp[ooE0, z] := 450;

  rp[ooF0, x] := -30;
  rp[ooF0, y] := 0;
  rp[ooF0, z] := 300;

  rp[ooP0, x] := 2560;
  rp[ooP0, y] := 0;
  rp[ooP0, z] := 430;

  result := rp;
end;

end.
