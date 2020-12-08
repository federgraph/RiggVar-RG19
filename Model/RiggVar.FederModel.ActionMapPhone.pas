unit RiggVar.FederModel.ActionMapPhone;

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
  RiggVar.FB.Color,
  RiggVar.FB.ActionConst,
  RiggVar.FB.ActionMap;

type
  TActionMapPhone = class(TActionMap)
  private
    cla: TRggColor;
    claParam: TRggColor;
    claSample: TRggColor;
    claOption: TRggColor;
  protected
    procedure InitActionsRG(Layout: Integer);
  public
    constructor Create;
    procedure InitActions(Layout: Integer); override;
  end;

{
[9]----[10]
[1]--------
[2]--------
[3]--------
-----------
--------[4]
--------[5]
--------[6]
[7]--- -[8]

[9][1][2][3]----[10]
--------------------
--------------------
--------------------
--------------------
[7]------[4][5][6][8]

}

implementation

const
  PageCountRG = 10;

constructor TActionMapPhone.Create;
begin
  inherited;
  claParam := TRggColors.Plum;
  claSample := TRggColors.Cyan;
  claOption := TRggColors.Beige;
  FPageCount := PageCountRG;
  TestName := 'Phone Page';
end;

procedure TActionMapPhone.InitActions(Layout: Integer);
begin
  InitActionsRG(Layout);
end;

procedure TActionMapPhone.InitActionsRG(Layout: Integer);
begin
  case Layout of
    1:
    begin
      cla := claParam;
      IAC(1, faVorstag, cla);
      IAC(2, faWante, cla);
      IAC(3, faMastfussD0X, cla);

      IAC(4, faController, cla);
      IAC(5, faWOben, cla);
      IAC(6, faMastfallVorlauf, cla);

      cla := TRggColors.CornflowerBlue;
      IAC(7, faBiegung, cla);
      IAC(8, faMastfallF0C, cla);
    end;

    2:
    begin
      cla := TRggColors.Lime;
      IAC(1, faSalingH, cla);
      IAC(2, faSalingA, cla);
      IAC(3, faNoop, TRggColors.Beige);

      IAC(4, faNoop, TRggColors.Beige);
      IAC(5, faSalingL, cla);
      IAC(6, faSalingW, cla);

      cla := TRggColors.CornflowerBlue;
      IAC(7, fa420, cla);
      IAC(8, faLogo, cla);
    end;

    3:
    begin
      cla := TRggColors.Beige;
      IAC(1, faFixpointA, cla);
      IAC(2, faFixpointB, cla);
      IAC(3, faFixpointC, cla);

      IAC(4, faFixpointD, cla);
      IAC(5, faFixpointE, cla);
      IAC(6, faFixpointF, cla);

      cla := TRggColors.CornflowerBlue;
      IAC(7, faVorstag, cla);
      IAC(8, faWante, cla);
    end;

    4:
    begin
      cla := claOption;
      IAC(1, faFixpointA0, cla);
      IAC(2, faFixpointB0, cla);
      IAC(3, faFixpointC0, cla);

      IAC(4, faFixpointD0, cla);
      IAC(5, faFixpointE0, cla);
      IAC(6, faFixpointF0, cla);

      cla := TRggColors.CornflowerBlue;
      IAC(7, faCycleColorSchemeM, cla);
      IAC(8, faCycleColorSchemeP, cla);
    end;

    5:
    begin
      cla := claOption;
      IAC(1, faRggHull, TRggColors.Cyan);
      IAC(2, faNoop, cla);
      IAC(3, faNoop, cla);

      IAC(4, faNoop, cla);
      IAC(5, faNoop, cla);
      IAC(6, faNoop, cla);

      cla := TRggColors.CornflowerBlue;
      IAC(7, faNoop, cla);
      IAC(8, faNoop, cla);
    end;

    6:
    begin
      cla := claSample;
      IAC(1, faTrimm1, cla);
      IAC(2, faTrimm2, cla);
      IAC(3, faTrimm3, cla);

      IAC(4, faTrimm4, cla);
      IAC(5, faTrimm5, cla);
      IAC(6, faTrimm6, cla);

      cla := TRggColors.CornflowerBlue;
      IAC(7, fa420, cla);
      IAC(8, faLogo, cla);
    end;

    7:
    begin
      cla := claOption;
      IAC(1, faViewpointA, cla);
      IAC(2, faViewpointT, cla);
      IAC(3, faViewpoint3, cla);

      cla := TRggColors.Lime;
      IAC(4, faSalingTypOhne, cla);
      IAC(5, faSalingTypDrehbar, cla);
      IAC(6, faSalingTypFest, cla);

      cla := TRggColors.CornflowerBlue;
      IAC(7, faViewpointS, cla);
      IAC(8, faNoop, TRggColors.Yellow);
    end;

    8:
    begin
      cla := claOption;
      IAC(1, faWantRenderE, cla);
      IAC(2, faWantRenderH, cla);
      IAC(3, faWantRenderP, cla);

      IAC(4, faNoop, cla);
      IAC(5, faWantRenderS, cla);
      IAC(6, faWantRenderF, cla);

      cla := TRggColors.CornflowerBlue;
      IAC(7, faNoop, TRggColors.White);
      IAC(8, faNoop, TRggColors.Yellow);
    end;

    9:
    begin
      cla := TRggColors.White;
      IAC(1, faReadTrimmFile, cla);
      IAC(2, faSaveTrimmFile, cla);
      IAC(3, faNoop, TRggColors.White);

      IAC(4, faCopyTrimmFile, cla);
      IAC(5, faCopyTrimmItem, cla);
      IAC(6, faPasteTrimmItem, cla);

      cla := TRggColors.CornflowerBlue;
      IAC(7, faCycleColorSchemeM, cla);
      IAC(8, faCycleColorSchemeP, cla);
    end;

    10:
    begin
      cla := TRggColors.White;
      IAC(1, faMB01, cla);
      IAC(2, faMB02, cla);
      IAC(3, faMB03, cla);

      IAC(4, faMB04, cla);
      IAC(5, faMB05, cla);
      IAC(6, faMB06, cla);

      cla := TRggColors.CornflowerBlue;
      IAC(7, faMB07, cla);
      IAC(8, faMB08, cla);
    end;

  end;
end;

end.

