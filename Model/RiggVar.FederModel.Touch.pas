unit RiggVar.FederModel.Touch;

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
  Classes,
  Controls,
  ExtCtrls,
  RiggVar.FB.Color,
  RiggVar.FB.ActionConst,
  RiggVar.FederModel.TouchBase;

type
  TFederTouch = class(TFederTouchBase)
  private
    FCornerBtnOpacity: single;
    procedure InitCornerMenu;
    procedure ResetCornerMenu;
  protected
    procedure InitShapes;
    procedure SetActionMap(const Value: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Init; override;
    procedure InitActions(Layout: Integer); override;
    procedure UpdateColorScheme; override;
  end;

implementation

uses
  RiggVar.App.Main;

{ TFederTouch }

constructor TFederTouch.Create(AOwner: TComponent);
begin
  inherited;
  ActionMap := 1;
  OpacityValue := 0.5;
  Main.ActionMapTablet.ActionProc := InitAction;
  Main.ActionMapTablet.ActionColorProc := InitActionWithColor;
end;

procedure TFederTouch.Init;
begin
  if not InitOK then
  begin
    InitShapes;
    UpdateColorScheme;
    InitOK := True;
    UpdateShape;
  end;
end;

procedure TFederTouch.InitShapes;
begin
  if not InitOK then
  begin
    TCornerBtn.BtnBorderColor := TRggColors.Gray;
    TCornerBtn.BtnBorderWidth := 2;

    ToolBtn := TTouchBtn.Create(OwnerComponent);
    ToolBtn.Circle := True;
    ToolBtn.X := 1;
    ToolBtn.Y := 1;
    ToolBtn.Init;
    ToolBtn.Left := MainVar.ScaledRaster;
    ToolBtn.Top := MainVar.ScaledRaster;
    ToolBtn.Width := MainVar.ScaledRaster;
    ToolBtn.Height := MainVar.ScaledRaster;
    ToolBtn.Opacity := 0.1;
    ToolBtn.Shape.Brush.Color := MainVar.ColorScheme.claToolBtnFill;
    ToolBtn.UpdatePosition;

    InitCornerMenu;

    InitClick(ToolBtn);
    InitHandlers(SL00);
    InitHandlers(SR00);
    InitHandlers(ST00);
    InitHandlers(SB00);
  end;
end;

procedure TFederTouch.UpdateColorScheme;
var
  b: TCornerBtn;
  tc1, tc2: TRggColor;
begin
  for b in CornerBtnList do
    b.Text.Font.Color := MainVar.ColorScheme.claCornerBtnText;

  tc1 := MainVar.ColorScheme.claCornerScrollbar;
  tc2 := MainVar.ColorScheme.claCornerBtnText;

  ST00.Shape.Brush.Color := tc1;
  ST00.Text.Font.Color := tc2;
  SB00.Shape.Brush.Color := tc1;
  SB00.Text.Font.Color := tc2;
  SL00.Shape.Brush.Color := tc1;
  SL00.Text.Font.Color := tc2;
  SR00.Shape.Brush.Color := tc1;
  SR00.Text.Font.Color := tc2;
end;

procedure TFederTouch.InitCornerMenu;
var
  cp: TCornerPos;
  cl: TRggColor;
  fa: Integer;
begin
  TCornerBtn.OffsetX := 0;
  TCornerBtn.OffsetY := 0;
  TCornerBtn.BtnWidth := MainVar.Raster;
  TCornerBtn.BtnHeight := MainVar.Raster;
  TCornerBtn.Circle := False;
  TCornerBtn.BtnBorderColor := TRggColors.Gray;
  TCornerBtn.BtnBorderWidth := 2;

  cl := TRggColors.Gray;
  fa := faNoop;

  cp := cpTL;
  PageBtnM := CornerMenu.NewBtn(cp, 0, 0, cl, fa, 1);
  CornerBtnList.Add(PageBtnM);
  CornerBtnList.Add(CornerMenu.NewBtn(cp, 0, 1, cl, fa, 2));
  CornerBtnList.Add(CornerMenu.NewBtn(cp, 1, 0, cl, fa, 3));
  CornerBtnList.Add(CornerMenu.NewBtn(cp, 2, 0, cl, fa, 4));
  CornerBtnList.Add(CornerMenu.NewBtn(cp, 3, 0, cl, fa, 5));
  CornerBtnList.Add(CornerMenu.NewBtn(cp, 4, 0, cl, fa, 6));

  cp := cpTR;
  PageBtnP := CornerMenu.NewBtn(cp, 0, 0, cl, fa, 7);
  CornerBtnList.Add(PageBtnP);
  CornerBtnList.Add(CornerMenu.NewBtn(cp, 0, 1, cl, fa, 8));
  CornerBtnList.Add(CornerMenu.NewBtn(cp, 0, 2, cl, fa, 9));
  CornerBtnList.Add(CornerMenu.NewBtn(cp, 0, 3, cl, fa, 10));

  CornerBtnList.Add(CornerMenu.NewBtn(cp, 1, 0, cl, fa, 11));
  CornerBtnList.Add(CornerMenu.NewBtn(cp, 2, 0, cl, fa, 12));
  CornerBtnList.Add(CornerMenu.NewBtn(cp, 3, 0, cl, fa, 13));
  CornerBtnList.Add(CornerMenu.NewBtn(cp, 4, 0, cl, fa, 14));

  cp := cpBL;
  CornerBtnList.Add(CornerMenu.NewBtn(cp, 0, 0, cl, fa, 15));
  CornerBtnList.Add(CornerMenu.NewBtn(cp, 1, 0, cl, fa, 16));
  CornerBtnList.Add(CornerMenu.NewBtn(cp, 2, 0, cl, fa, 17));
  CornerBtnList.Add(CornerMenu.NewBtn(cp, 3, 0, cl, fa, 18));
  CornerBtnList.Add(CornerMenu.NewBtn(cp, 4, 0, cl, fa, 19));

  CornerBtnList.Add(CornerMenu.NewBtn(cp, 0, 1, cl, fa, 20));
  CornerBtnList.Add(CornerMenu.NewBtn(cp, 0, 2, cl, fa, 21));
  CornerBtnList.Add(CornerMenu.NewBtn(cp, 0, 3, cl, fa, 22));

  cp := cpBR;
  CornerBtnList.Add(CornerMenu.NewBtn(cp, 0, 0, cl, fa, 23));
  CornerBtnList.Add(CornerMenu.NewBtn(cp, 0, 1, cl, fa, 24));

  CornerBtnList.Add(CornerMenu.NewBtn(cp, 1, 0, cl, fa, 25));
  CornerBtnList.Add(CornerMenu.NewBtn(cp, 2, 0, cl, fa, 26));
  CornerBtnList.Add(CornerMenu.NewBtn(cp, 3, 0, cl, fa, 27));
  CornerBtnList.Add(CornerMenu.NewBtn(cp, 4, 0, cl, fa, 28));

  cl := MainVar.ColorScheme.claCornerScrollbar;
  ST00 := CornerMenu.NewBtn(cpT, 6, 0, cl, fa);
  SR00 := CornerMenu.NewBtn(cpR, 0, 5, cl, fa);
  SB00 := CornerMenu.NewBtn(cpB, 6, 0, cl, fa);
  SL00 := CornerMenu.NewBtn(cpL, 0, 5, cl, fa);

  FCornerBtnOpacity := PageBtnM.Shape.Opacity;

  ResetCornerMenu;
end;

procedure TFederTouch.InitActions(Layout: Integer);
begin
  Main.ActionMapTablet.InitActions(Layout);
end;

procedure TFederTouch.ResetCornerMenu;
var
  cb: TCornerBtn;
begin
  { Assign actions to buttons, and Color, Caption, Opacity ... }

  ActionPage := ActionPage; // call virtual setter --> InitActions

  for cb in CornerBtnList do
    cb.Opacity := FCornerBtnOpacity;
end;

procedure TFederTouch.SetActionMap(const Value: Integer);
begin
  inherited;

  MaxPageIndex := Main.ActionMapTablet.PageCount;

  if InitOK then
    ResetCornerMenu;
end;

end.
