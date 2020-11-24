unit RiggVar.FB.Scheme;

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

{$ifdef fpc}
{$mode delphi}
{$endif}

uses
  RiggVar.FB.Color;

type
  TColorScheme = record
  public
    WantBlackText: Boolean;

    Scheme: Integer;
    SchemeDefault: Integer;

    claBackground: TRggColor;
    claToolBtnFill: TRggColor;
    claTouchBtnFill: TRggColor;
    claCornerScrollbar: TRggColor;
    claCornerBtnText: TRggColor;
    claTouchbarText: TRggColor;
    claNull: TRggColor;

    IsDark: Boolean;

    Dark: Integer;
    Light: Integer;

    constructor Create(cs: Integer);

    procedure BlackText;
    procedure GrayText;
    procedure WhiteText;

    procedure Init(cs: Integer);
  end;

implementation

{ TColorScheme }

procedure TColorScheme.BlackText;
begin
  claToolBtnFill := TRggColors.Gray;
  claTouchBtnFill := TRggColors.Gray;
  claCornerScrollbar := TRggColors.Gray;
  claCornerBtnText:= TRggColors.Blue;
end;

procedure TColorScheme.GrayText;
begin
  claToolBtnFill := TRggColors.Gray;
  claTouchBtnFill := TRggColors.Gray;
  claCornerScrollbar := TRggColors.Gray;
  claCornerBtnText:= TRggColors.Blue;
end;

procedure TColorScheme.WhiteText;
begin
  claToolBtnFill := TRggColors.White;
  claTouchBtnFill := TRggColors.White;
  claCornerScrollbar := TRggColors.Gray;
  claCornerBtnText:= TRggColors.White;
end;

constructor TColorScheme.Create(cs: Integer);
begin
  Dark := 5;
  Light := 2;
  WantBlackText := True;

  claTouchbarText := TRggColors.Black;
  claNull := TRggColors.Null;

  SchemeDefault := cs;
  Scheme := SchemeDefault;
  Init(Scheme);
end;

procedure TColorScheme.Init(cs: Integer);
begin
  Scheme := cs;
  IsDark := True;

  case cs of
    1:
    begin
      if WantBlackText then
      begin
        claBackground := TRggColors.Slateblue;
        claToolBtnFill := TRggColors.Gray;
        claTouchBtnFill := TRggColors.Gray;
        claCornerScrollbar := TRggColors.Lightsalmon;
        claCornerBtnText:= TRggColors.Blue;
      end
      else
      begin
        claBackground := TRggColors.Lavender;
        claToolBtnFill := TRggColors.Gray;
        claTouchBtnFill := TRggColors.Gray;
        claCornerScrollbar := TRggColors.Gray;
        claCornerBtnText:= TRggColors.Blue;
      end;
    end;
    2:
    begin
      IsDark := False;
      claBackground := TRggColors.ColorF9F9F9;
      claToolBtnFill := TRggColors.Gray;
      claTouchBtnFill := TRggColors.Gray;
      claCornerScrollbar := TRggColors.Lavender;
      claCornerBtnText:= TRggColors.Blue;
    end;
    3:
    begin
      claBackground := TRggColors.Cornflowerblue;
      claToolBtnFill := TRggColors.White;
      claTouchBtnFill := TRggColors.White;
      claCornerScrollbar := TRggColors.White;
      claCornerBtnText:= TRggColors.Black;
    end;
    4:
    begin
      claBackground := TRggColors.Color372E69;
      claToolBtnFill := TRggColors.White;
      claTouchBtnFill := TRggColors.White;
      claCornerScrollbar := TRggColors.Antiquewhite;
      claCornerBtnText:= TRggColors.Blue;
    end;
    5:
    begin
      claBackground := TRggColors.Color333333;
      claToolBtnFill := TRggColors.White;
      claTouchBtnFill := TRggColors.White;
      claCornerScrollbar := TRggColors.WindowWhite;
      claCornerBtnText:= TRggColors.Blue;
    end;
    6:
    begin
      claBackground := TRggColors.Black;
      claToolBtnFill := TRggColors.White;
      claTouchBtnFill := TRggColors.White;
      claCornerScrollbar := TRggColors.Lightgray;
      claCornerBtnText:= TRggColors.Blue;
    end;
    7:
    begin
      claBackground := TRggColors.Purple;
      claToolBtnFill := TRggColors.Gray;
      claTouchBtnFill := TRggColors.Gray;
      claCornerScrollbar := TRggColors.Lightgoldenrodyellow;
      claCornerBtnText:= TRggColors.Blue;
    end;
  end;
end;

end.

