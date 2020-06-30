unit RiggVar.FederModel.TouchPhone;

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
  SysUtils,
  Classes,
  Controls,
  ExtCtrls,
  Graphics,
  RiggVar.FB.Color,
  RiggVar.FB.ActionConst,
  RiggVar.FederModel.TouchBase;

type
  TFederTouchPhone = class(TFederTouchBase)
  private
    procedure InitCornerMenu;
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

{ TFederTouchPhone }

constructor TFederTouchPhone.Create(AOwner: TComponent);
begin
  inherited;
  ActionMap := 1;
  OpacityValue := 0.5;
  Main.ActionMapPhone.ActionProc := InitAction;
  Main.ActionMapPhone.ActionColorProc := InitActionWithColor;
end;

procedure TFederTouchPhone.Init;
begin
  Width := MainVar.ClientWidth;
  Height := MainVar.ClientHeight;
  if not InitOK then
  begin
    InitShapes;
    UpdateColorScheme;
    InitOK := True;
    UpdateShape;
  end;
end;

procedure TFederTouchPhone.InitShapes;
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

procedure TFederTouchPhone.UpdateColorScheme;
var
  b: TCornerBtn;
  tc1, tc2: TRggColor;
begin
  ToolBtn.Shape.Brush.Color := MainVar.ColorScheme.claToolBtnFill;

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

procedure TFederTouchPhone.InitCornerMenu;
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

  fa := faNoop;
  cl := TRggColors.Yellow;
  PageBtnP := CornerMenu.NewBtn(cpTR, 0, 0, cl, faActionPageP, 9);
  PageBtnM := CornerMenu.NewBtn(cpTL, 0, 0, cl, faActionPageM, 10);
  CornerBtnList.Add(PageBtnP);
  CornerBtnList.Add(PageBtnM);
  cl := TRggColors.CornflowerBlue;
  CornerBtnList.Add(CornerMenu.NewBtn(cpBL, 0, 0, cl, faNoop, 7));
  CornerBtnList.Add(CornerMenu.NewBtn(cpBR, 0, 0, cl, faNoop, 8));

  cp := cpTL;
  cl := TRggColors.White;
  CornerBtnList.Add(CornerMenu.NewBtn(cp, 1, 0, cl, fa, 1));
  CornerBtnList.Add(CornerMenu.NewBtn(cp, 2, 0, cl, fa, 2));
  CornerBtnList.Add(CornerMenu.NewBtn(cp, 3, 0, cl, fa, 3));

  cp := cpBR;
  cl := TRggColors.White;
  CornerBtnList.Add(CornerMenu.NewBtn(cp, 1, 0, cl, fa, 6));
  CornerBtnList.Add(CornerMenu.NewBtn(cp, 2, 0, cl, fa, 5));
  CornerBtnList.Add(CornerMenu.NewBtn(cp, 3, 0, cl, fa, 4));

  cl := MainVar.ColorScheme.claCornerScrollbar;
  ST00 := CornerMenu.NewBtn(cpT, 3, 0, cl, fa);
  SR00 := CornerMenu.NewBtn(cpR, 0, 5, cl, fa);
  SB00 := CornerMenu.NewBtn(cpB, 3, 0, cl, fa);
  SL00 := CornerMenu.NewBtn(cpL, 0, 5, cl, fa);

  ST00.Text.Font.Size := MainConst.DefaultBtnFontSize;
  SB00.Text.Font.Size := MainConst.DefaultBtnFontSize;
  SL00.Text.Font.Size := MainConst.DefaultBtnFontSize;
  SR00.Text.Font.Size := MainConst.DefaultBtnFontSize;

  InitActions(1);
end;

procedure TFederTouchPhone.InitActions(Layout: Integer);
begin
  Main.ActionMapPhone.InitActions(Layout);
end;

procedure TFederTouchPhone.SetActionMap(const Value: Integer);
begin
  inherited;
  MaxPageIndex := Main.ActionMapPhone.PageCount;
end;

end.
