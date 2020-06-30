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
  TColorScheme = class
  public
    WantBlackText: Boolean;

    Scheme: Integer;
    SchemeDefault: Integer;
    claBackground: TRggColor;

    claLabelText: TRggColor;
    claSampleText: TRggColor;
    claOptionText: TRggColor;
    claToolBtnFill: TRggColor;
    claTouchBtnFill: TRggColor;
    claCornerScrollbar: TRggColor;
    claCornerBtnText: TRggColor;
    claEquationFill: TRggColor;
    claEquationText: TRggColor;
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
  claLabelText := TRggColors.Black;
  claSampleText := TRggColors.Black;
  claToolBtnFill := TRggColors.Gray;
  claTouchBtnFill := TRggColors.Gray;
  claCornerScrollbar := TRggColors.Gray;
  claCornerBtnText:= TRggColors.Blue;
  claEquationFill := TRggColors.Null;
  claEquationText := TRggColors.Black;

  claOptionText := claSampleText;
end;

procedure TColorScheme.GrayText;
begin
  claLabelText := TRggColors.Gray;
  claSampleText := TRggColors.Gray;
  claToolBtnFill := TRggColors.Gray;
  claTouchBtnFill := TRggColors.Gray;
  claCornerScrollbar := TRggColors.Gray;
  claCornerBtnText:= TRggColors.Blue;
  claEquationFill := TRggColors.Null;
  claEquationText := TRggColors.Black;

  claOptionText := claSampleText;
end;

procedure TColorScheme.WhiteText;
begin
  claLabelText := TRggColors.White;
  claSampleText := TRggColors.White;
  claToolBtnFill := TRggColors.White;
  claTouchBtnFill := TRggColors.White;
  claCornerScrollbar := TRggColors.Gray;
  claCornerBtnText:= TRggColors.White;
  claEquationFill := TRggColors.Null;
  claEquationText := TRggColors.White;

  claOptionText := claSampleText;
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
        claLabelText := TRggColors.Black;
        claSampleText := TRggColors.Black;
        claToolBtnFill := TRggColors.Gray;
        claTouchBtnFill := TRggColors.Gray;
        claCornerScrollbar := TRggColors.Lightsalmon;
        claCornerBtnText:= TRggColors.Blue;
        claEquationFill := TRggColors.Null;
        claEquationText := TRggColors.Black;
      end
      else
      begin
        claBackground := TRggColors.Lavender;
        claLabelText := TRggColors.Gray;
        claSampleText := TRggColors.Gray;
        claToolBtnFill := TRggColors.Gray;
        claTouchBtnFill := TRggColors.Gray;
        claCornerScrollbar := TRggColors.Gray;
        claCornerBtnText:= TRggColors.Blue;
        claEquationFill := TRggColors.Null;
        claEquationText := TRggColors.Black;
      end;
    end;
    2:
    begin
      IsDark := False;
      claBackground := TRggColors.ColorF9F9F9;
      claLabelText := TRggColors.White;
      claSampleText := TRggColors.White;
      claToolBtnFill := TRggColors.Gray;
      claTouchBtnFill := TRggColors.Gray;
      claCornerScrollbar := TRggColors.Lavender;
      claCornerBtnText:= TRggColors.Blue;
      claEquationFill := TRggColors.Null;
      claEquationText := TRggColors.Black;
    end;
    3:
    begin
      claBackground := TRggColors.Cornflowerblue;
      claLabelText := TRggColors.White;
      claSampleText := TRggColors.White;
      claToolBtnFill := TRggColors.White;
      claTouchBtnFill := TRggColors.White;
      claCornerScrollbar := TRggColors.White;
      claCornerBtnText:= TRggColors.Black;
      claEquationFill := TRggColors.Null;
      claEquationText := TRggColors.Black;
    end;
    4:
    begin
      claBackground := TRggColors.Color372E69;
      claLabelText := TRggColors.White;
      claSampleText := TRggColors.White;
      claToolBtnFill := TRggColors.White;
      claTouchBtnFill := TRggColors.White;
      claCornerScrollbar := TRggColors.Antiquewhite;
      claCornerBtnText:= TRggColors.Blue;
      claEquationFill := TRggColors.Null;
      claEquationText := TRggColors.White;
    end;
    5:
    begin
      claBackground := TRggColors.Color333333;
      claLabelText := TRggColors.White;
      claSampleText := TRggColors.White;
      claToolBtnFill := TRggColors.White;
      claTouchBtnFill := TRggColors.White;
      claCornerScrollbar := TRggColors.WindowWhite;
      claCornerBtnText:= TRggColors.Blue;
      claEquationFill := TRggColors.Null;
      claEquationText := TRggColors.White;
    end;
    6:
    begin
      claBackground := TRggColors.Black;
      claLabelText := TRggColors.White;
      claSampleText := TRggColors.White;
      claToolBtnFill := TRggColors.White;
      claTouchBtnFill := TRggColors.White;
      claCornerScrollbar := TRggColors.Lightgray;
      claCornerBtnText:= TRggColors.Blue;
      claEquationFill := TRggColors.Null;
      claEquationText := TRggColors.White;
    end;
    7:
    begin
      claBackground := TRggColors.Purple; //TRggColors.Null;
      claLabelText := TRggColors.Black;
      claSampleText := TRggColors.Black;
      claToolBtnFill := TRggColors.Gray;
      claTouchBtnFill := TRggColors.Gray;
      claCornerScrollbar := TRggColors.Lightgoldenrodyellow;
      claCornerBtnText:= TRggColors.Blue;
      claEquationFill := TRggColors.Null;
      claEquationText := TRggColors.Black;
    end;
  end;
  claOptionText := claSampleText;
end;

end.

