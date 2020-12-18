unit RiggVar.RG.Def;

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

interface

uses
  Graphics;

type
  TSelectedCircle = (
    scC1,
    scC2
  );

  TCircleParam = (
    fpR1,
    fpR2,
    fpM1X,
    fpM1Y,
    fpM1Z,
    fpM2X,
    fpM2Y,
    fpM2Z,
    fpA1,
    fpA2,
    fpE1,
    fpE2
  );

  RggColors = record
  const
    clRumpf: TColor = clSilver;
    clMast: TColor = clBlue;
    clWanten: TColor = clRed;
    clVorstag: TColor = clYellow;
    clSaling: TColor = clLime;
    clController: TColor = clAqua;
    clEntspannt: TColor = clSilver;
    clNullStellung: TColor = clAqua;
    clKoppelKurve: TColor = clYellow;
    clGestrichelt: TColor = clWhite;
    clFixPont: TColor = clYellow;
  end;

implementation

end.
