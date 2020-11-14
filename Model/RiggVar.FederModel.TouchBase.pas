unit RiggVar.FederModel.TouchBase;

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
  Math,
  UITypes,
  Generics.Collections,
  StdCtrls,
  ExtCtrls,
  Controls,
  RiggVar.FB.Color,
  RiggVar.FB.ActionConst,
  RiggVar.FB.Action,
  RiggVar.FB.TextBase;

type
  TBtnShape = class(TShape)
  public
    Opacity: single;
  published
    property OnClick;
  end;

  TTouchBtn = class(TControl)
  private
    FID: Integer;
    FCaption: string;
    FText: TLabel;
    FShape: TBtnShape;
    FAction: TFederAction;
    procedure SetCaption(const Value: string);
    procedure SetAction2(const Value: TFederAction);
  protected
    procedure SetColor(const Value: TColor);
    procedure SetHint(const Value: string);
  protected
    procedure HandleClick(Sender: TObject);
  public
    Opacity: single;
    TextOpacity: single;

    X0, Y0: Integer;
    X, Y: Integer;
    class var
      OffsetX: Integer;
      OffsetY: Integer;
      BtnWidth: Integer;
      BtnHeight: Integer;
      Circle: Boolean;
      WantHint: Boolean;
      BtnBorderWidth: Integer;
      BtnBorderColor: TRggColor;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure InitPosition; virtual;

    procedure Init;
    procedure CheckState;
    procedure CheckCircleState;
    procedure UpdateHint;
    procedure UpdatePosition;
    procedure UpdateVisibility;

    property ID: Integer read FID write FID;
    property Caption: string read FCaption write SetCaption;
    property Hint: string write SetHint;
    property Action2: TFederAction read FAction write SetAction2;
    property Shape: TBtnShape read FShape;
    property Text: TLabel read FText;
    property Color: TColor write SetColor;
  end;

  TCornerPos = (
    cpTL, //TopLeft corner
    cpTR,
    cpBL,
    cpBR,
    cpT, //top side
    cpR,
    cpB,
    cpL
  );

  TCornerBtn = class(TTouchBtn)
  private
    procedure UpdateUV;
  public
    U, V: Integer;
    CornerPos: TCornerPos;
    procedure InitPosition; override;
  end;

  TCornerMenu = class
  public
    function NewBtn(
      CornerPos: TCornerPos;
      X, Y: Integer;
      BtnColor: TRggColor;
      Action: TFederAction;
      BtnID: Integer = 0
      ): TCornerBtn;

    class var
    TStart: Integer;
    RStart: Integer;
    BStart: Integer;
    LStart: Integer;
    TCount: Integer;
    RCount: Integer;
    BCount: Integer;
    LCount: Integer;
  end;

  TFederTouchBase = class(TFederTouch0)
  private
    procedure SetOwnsMouse(const Value: Boolean);
    function GetMaxCount: Integer;
    function GetMinCount: Integer;
  protected
    MaxPageIndex: Integer;

    OldX: single;
    OldY: single;
    Down: Boolean;
    FOwnsMouse: Boolean;

    ToolBtn: TTouchBtn;

    procedure OnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure OnMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure OnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure OnMouseLeave(Sender: TObject);
    procedure BorderTrack(Sender: TObject; X, Y: single);

    procedure SetActionPage(const Value: Integer); override;
    procedure SetFrameVisible(const Value: Boolean); override;

    procedure InitAction(BtnID: Integer; fa: TFederAction);
    procedure InitActionWithColor(BtnID: Integer; fa: TFederAction; ac: TRggColor);
  public
    CornerBtnList: TObjectList<TCornerBtn>;
    CornerMenu: TCornerMenu;

    PageBtnP: TCornerBtn;
    PageBtnM: TCornerBtn;

    SL00: TCornerBtn;
    ST00: TCornerBtn;
    SB00: TCornerBtn;
    SR00: TCornerBtn;

    class var
      OpacityValue: single;
      OwnerComponent: TComponent;
      ParentObject: TWinControl;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure InitHandlers(T: TTouchBtn);
    procedure InitClick(T: TTouchBtn);
    procedure ToolBtnClick(Sender: TObject);
    procedure ToggleTouchFrame; override;
    procedure UpdateText; override;

    procedure Init; override;
    procedure CheckState;
    procedure UpdateWH;

    procedure Report(ML: TStrings);
    function FindCornerBtn(id: Integer): TCornerBtn;

    procedure InitActions(Layout: Integer); virtual;
    procedure UpdateToolSet(Delta: Integer);
    procedure UpdateShape;
    procedure UpdateColorScheme; virtual;
    procedure UpdatePageBtnText;

    property FrameVisible: Boolean read FFrameVisible write SetFrameVisible;
    property OwnsMouse: Boolean read FOwnsMouse write SetOwnsMouse;

    property MinCount: Integer read GetMinCount;
    property MaxCount: Integer read GetMaxCount;
  end;

implementation

uses
  RiggVar.App.Main;

{ TTouchBtn }

constructor TTouchBtn.Create(AOwner: TComponent);
begin
  inherited;
  Width := BtnWidth;
  Height := BtnHeight;
end;

destructor TTouchBtn.Destroy;
begin
  inherited;
end;

procedure TTouchBtn.HandleClick(Sender: TObject);
begin
  Main.ActionHandler.Execute(Action2);
  Main.FederText.CheckState; // if not done in Execute
end;

procedure TTouchBtn.CheckState;
var
  b: Boolean;
begin
  if FShape.Shape <> stCircle then
  begin
    b := Main.ActionHandler.GetChecked(self.Action2);
    if not b then
      FShape.Shape := stRectangle
    else
      FShape.Shape := stRoundRect
  end;
end;

procedure TTouchBtn.CheckCircleState;
var
  b: Boolean;
begin
  if FShape.Shape = TShapeType.stCircle then
  begin
    b := Main.ActionHandler.GetChecked(self.Action2);
    if b then
      FShape.Brush.Color := TRggColors.Aqua
    else
      FShape.Brush.Color := MainVar.ColorScheme.claTouchBtnFill;
  end;
end;

procedure TTouchBtn.SetAction2(const Value: TFederAction);
begin
  FAction := Value;
end;

procedure TTouchBtn.SetCaption(const Value: string);
begin
  FCaption := Value;
  if Assigned(FText) then
    FText.Caption := Value;
end;

procedure TTouchBtn.SetColor(const Value: TColor);
begin
  if Assigned(FShape) then
  begin
    FShape.Brush.Color := Value;
    FText.Color := Value;
  end;
end;

procedure TTouchBtn.SetHint(const Value: string);
begin
  if Assigned(FText) then
  begin
    FText.Hint := Value;
    FShape.Hint := Value;
  end;
end;

procedure TTouchBtn.UpdateHint;
begin
  if WantHint then
  begin
    Hint := Main.ActionHandler.GetCaption(FAction);
  end;
end;

procedure TTouchBtn.UpdateVisibility;
begin
  Shape.Visible := Visible;
  Text.Visible := Visible;
end;

procedure TTouchBtn.Init;
begin
  FShape := TBtnShape.Create(TFederTouchBase.OwnerComponent);
  if Circle then
  begin
    FShape.Shape := stCircle;
  end
  else
  begin
    FShape.Shape := stRectangle;
  end;

  FText := TLabel.Create(Self);
  FText.Caption := FCaption;
  FText.Font.Size := MainConst.DefaultBtnFontSize;

  FShape.Pen.Color := BtnBorderColor;
  FShape.Pen.Width := BtnBorderWidth;
  FShape.Opacity := 1.0;
  FShape.ShowHint := True;

  Self.Parent := TFederTouchBase.ParentObject;
  FShape.Parent := TFederTouchBase.ParentObject;
  FText.Parent := TFederTouchBase.ParentObject;

  FShape.OnClick := HandleClick;
  FText.OnClick := HandleClick;

  if WantHint then
  begin
    FText.ShowHint := True;
    UpdateHint;
  end;

  InitPosition;
end;

procedure TTouchBtn.InitPosition;
begin
  Left := X * BtnWidth + OffsetX;
  Top := Y * BtnHeight + OffsetY;

  FShape.Left := Left;
  FShape.Top := Top;

  FText.Left := Left + Round(10 * MainVar.Scale);
  FText.Top := Top + Round(10 * MainVar.Scale);
end;

{ TCornerMenu }

function TCornerMenu.NewBtn(
  CornerPos: TCornerPos;
  X, Y: Integer;
  BtnColor: TColor;
  Action: TFederAction;
  BtnID: Integer = 0
  ): TCornerBtn;
var
  b: TCornerBtn;
begin
  b := TCornerBtn.Create(TFederTouchBase.OwnerComponent);
  b.Parent := TFederTouchBase.ParentObject;
  b.CornerPos := CornerPos;
  b.FID := BtnID;
  b.X := X;
  b.Y := Y;
  b.Action2 := Action;
  b.Opacity := TFederTouchBase.OpacityValue;
  b.Init;
  b.Color := BtnColor;

  b.Caption := Main.ActionHandler.GetShortCaption(Action);
  b.FText.Font.Color := MainVar.ColorScheme.claCornerBtnText;
  b.FText.Font.Size := MainConst.DefaultBtnFontSize;
  b.FText.AutoSize := True;
  b.TextOpacity := 1.0;

  result := b;
end;

{ TCornerBtn }

procedure TCornerBtn.UpdateUV;
begin
  U := X;
  V := Y;

  case CornerPos of
    cpTL, cpTR,  cpBL, cpBR:
    begin
      if not Main.IsLandscape then
      begin
        U := Y;
        V := X;
      end;

      if U > 0 then
      begin
        case CornerPos of
          cpTL:
          begin
            Inc(TCornerMenu.TStart);
            Inc(TCornerMenu.TCount);
          end;
          cpTR:
          begin
            Inc(TCornerMenu.TCount);
          end;
          cpBL:
          begin
            Inc(TCornerMenu.BStart);
            Inc(TCornerMenu.BCount);
          end;
          cpBR:
          begin
            Inc(TCornerMenu.BCount);
          end;
        end;
      end;
      if V > 0 then
      begin
        case CornerPos of
          cpTL:
          begin
            Inc(TCornerMenu.LStart);
            Inc(TCornerMenu.LCount);
          end;
          cpTR:
          begin
            Inc(TCornerMenu.RStart);
            Inc(TCornerMenu.RCount);
          end;
          cpBL:
          begin
            Inc(TCornerMenu.LCount);
          end;
          cpBR:
          begin
            Inc(TCornerMenu.RCount);
          end;
        end;
      end;

    end;
  end;
end;

procedure TCornerBtn.InitPosition;
var
  mx, my: Boolean;
begin
  UpdateUV;
  case CornerPos of
    cpTL:
    begin
      mx := False;
      my := False;
    end;
    cpTR:
    begin
      mx := True;
      my := False;
    end;
    cpBR:
    begin
      mx := True;
      my := True;
    end;
    cpBL:
    begin
      mx := False;
      my := True;
    end;

    cpT:
    begin
      mx := False;
      my := False;
    end;
    cpR:
    begin
      mx := True;
      my := False;
    end;
    cpB:
    begin
      mx := False;
      my := True;
    end;
    cpL:
    begin
      mx := False;
      my := False;
    end;

    else
    begin
      mx := False;
      my := False;
    end;
  end;

  if mx then
    Left := MainVar.ClientWidth - Round((U + 1) * MainVar.ScaledRaster)
  else
    Left := Round(U * MainVar.ScaledRaster);

  if my then
    Top := MainVar.ClientHeight - Round((V + 1) * MainVar.ScaledRaster)
  else
    Top := Round(V * MainVar.ScaledRaster);

  { Corner buttons }
  FShape.Left := Left + Round(MainVar.Scale * 2);
  FShape.Top := Top + Round(MainVar.Scale * 2);

  FText.Left := Left + Round(MainVar.Scale * 10);
  FText.Top := Top + Round(MainVar.Scale * 20);
end;

procedure TTouchBtn.UpdatePosition;
begin
  { Touchbars between Corner buttons }
  Shape.Left := Left + Round(MainVar.Scale * 2);
  Shape.Top := Top + Round(MainVar.Scale * 2);
  Shape.Width := Width - Round(MainVar.Scale * 5); // 5 = (2 + 2 - 1)
  Shape.Height := Height - Round(MainVar.Scale * 5);
end;

{ TFederTouchBase }

procedure TFederTouchBase.UpdateWH;
begin
  Width := MainVar.ClientWidth;
  Height := MainVar.ClientHeight;
end;

procedure TFederTouchBase.CheckState;
var
  b: TCornerBtn;
begin
  PaintBackgroundNeeded := True;
  for b in CornerBtnList do
    b.CheckState;
end;

constructor TFederTouchBase.Create(AOwner: TComponent);
begin
  inherited;
  FFrameVisible := True;

  FActionPage := 1;
  CornerBtnList := TObjectList<TCornerBtn>.Create;
  CornerBtnList.OwnsObjects := False;
  CornerMenu := TCornerMenu.Create;
end;

destructor TFederTouchBase.Destroy;
begin
  CornerMenu.Free;
  CornerBtnList.Free;
  inherited;
end;

function TFederTouchBase.GetMinCount: Integer;
begin
  result := Min(MainVar.ClientWidth, MainVar.ClientHeight) div MainVar.Raster;
end;

function TFederTouchBase.GetMaxCount: Integer;
begin
  result := Max(MainVar.ClientWidth, MainVar.ClientHeight) div MainVar.Raster;
end;

procedure TFederTouchBase.Init;
begin

end;

procedure TFederTouchBase.SetActionPage(const Value: Integer);
begin
  FActionPage := Value;

  if FActionPage > MaxPageIndex then
    FActionPage := 1;
  if FActionPage < 1 then
      FActionPage := MaxPageIndex;

  InitActions(FActionPage);

  UpdateText;
  CheckState;
end;

procedure TFederTouchBase.UpdateColorScheme;
begin

end;

procedure TFederTouchBase.SetFrameVisible(const Value: Boolean);
var
  b: TCornerBtn;
begin
  FFrameVisible := Value;
  PaintBackgroundNeeded := True;

  if Value then
    ToolBtn.Opacity := 0.1
  else
    ToolBtn.Opacity := 0.05;

  for b in CornerBtnList do
  begin
    b.Visible := Value;
    b.Shape.Visible := Value;
    b.Text.Visible := Value;
  end;

  ST00.Visible := Value;
  SR00.Visible := Value;
  SB00.Visible := Value;
  SL00.Visible := Value;

  ST00.UpdateVisibility;
  SR00.UpdateVisibility;
  SB00.UpdateVisibility;
  SL00.UpdateVisibility;
end;

procedure TFederTouchBase.BorderTrack(Sender: TObject; X, Y: single);
begin
  if Sender = SB00.Shape then
  begin
    if Abs(X - OldX) > 0 then
    begin
      Main.DoTouchbarBottom(X - OldX);
      OldX := X;
      OldY := Y;
    end;
  end
  else if Sender = ST00.Shape then
  begin
    if Abs(X - OldX) > 0 then
    begin
      Main.DoTouchBarTop(X - OldX);
      OldX := X;
      OldY := Y;
    end;
  end
  else if Sender = SL00.Shape then
  begin
    if Abs(Y - OldY) > 0 then
    begin
      Main.DoTouchbarLeft(OldY - Y);
      OldX := X;
      OldY := Y;
    end;
  end
  else if Sender = SR00.Shape then
  begin
    if Abs(Y - OldY) > 0 then
    begin
      Main.DoTouchbarRight(OldY - Y);
      OldX := X;
      OldY := Y;
    end;
  end
  else
  begin
    OldX := X;
    OldY := Y;
  end;
end;

procedure TFederTouchBase.OnMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  Down := True;
  OldX := X;
  OldY := Y;
  FOwnsMouse := True;
end;

procedure TFederTouchBase.OnMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if Main.IsUp then
  begin
    if Down then
      BorderTrack(Sender, X, Y);
  end;
end;

procedure TFederTouchBase.OnMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  Down := False;
  FOwnsMouse := False;
end;

procedure TFederTouchBase.SetOwnsMouse(const Value: Boolean);
begin
  FOwnsMouse := Value;
end;

procedure TFederTouchBase.OnMouseLeave(Sender: TObject);
begin
  Down := False;
end;

procedure TFederTouchBase.UpdateShape;
var
  b: TCornerBtn;
begin
  UpdateWH;

  if InitOK then
  begin
    TCornerMenu.TStart := 1;
    TCornerMenu.RStart := 1;
    TCornerMenu.BStart := 1;
    TCornerMenu.LStart := 1;

    TCornerMenu.TCount := 2;
    TCornerMenu.RCount := 2;
    TCornerMenu.BCount := 2;
    TCornerMenu.LCount := 2;

    for b in CornerBtnList do
      b.InitPosition;

    ST00.Left := TCornerMenu.TStart * MainVar.ScaledRaster;
    ST00.Top := 0;

    SR00.Left := MainVar.ClientWidth - MainVar.ScaledRaster;
    SR00.Top := TCornerMenu.RStart * MainVar.ScaledRaster;

    SB00.Left := TCornerMenu.BStart * MainVar.ScaledRaster;
    SB00.Top := MainVar.ClientHeight - MainVar.ScaledRaster;

    SL00.Left := 0;
    SL00.Top := TCornerMenu.LStart * MainVar.ScaledRaster;

    ST00.Width := MainVar.ClientWidth - (TCornerMenu.TCount) * MainVar.ScaledRaster;
    SB00.Width := MainVar.ClientWidth - (TCornerMenu.BCount) * MainVar.ScaledRaster;
    SL00.Height := MainVar.ClientHeight - (TCornerMenu.LCount) * MainVar.ScaledRaster;
    SR00.Height := MainVar.ClientHeight - (TCornerMenu.RCount) * MainVar.ScaledRaster;

    ToolBtn.Left := MainVar.ScaledRaster;
    ToolBtn.Top := MainVar.ScaledRaster;
    ToolBtn.Width := MainVar.ScaledRaster;
    ToolBtn.Height := MainVar.ScaledRaster;

    ST00.UpdatePosition;
    SB00.UpdatePosition;
    SL00.UpdatePosition;
    SR00.UpdatePosition;
  end;
end;

procedure TFederTouchBase.UpdateToolSet(Delta: Integer);
begin
  ActionPage := FActionPage + Delta;
end;

procedure TFederTouchBase.InitAction(BtnID: Integer; fa: TFederAction);
var
  tb: TCornerBtn;
begin
  tb := FindCornerBtn(BtnID);
  if Assigned(tb) then
  begin
    tb.Action2 := fa;
    tb.Caption := Main.ActionHandler.GetShortCaption(fa);
    tb.UpdateHint;
  end;
end;

procedure TFederTouchBase.InitActionWithColor(BtnID: Integer; fa: TFederAction; ac: TRggColor);
var
  tb: TCornerBtn;
begin
  tb := FindCornerBtn(BtnID);
  if Assigned(tb) then
  begin
    tb.Action2 := fa;
    tb.Caption := Main.ActionHandler.GetShortCaption(fa);
    tb.UpdateHint;
    tb.Color := ac;
  end;
end;

procedure TFederTouchBase.InitActions(Layout: Integer);
begin

end;

function TFederTouchBase.FindCornerBtn(id: Integer): TCornerBtn;
var
  cb: TCornerBtn;
begin
  result := nil;
  for cb in CornerBtnList do
    if cb.ID = id then
    begin
      result := cb;
      break;
    end;
end;

procedure TFederTouchBase.UpdateText;
begin
  if InitOK then
  begin
    UpdatePageBtnText;
  end;
end;

procedure TFederTouchBase.UpdatePageBtnText;
begin
  PageBtnP.Text.Caption := IntToStr(ActionPage);
  PageBtnM.Text.Caption := IntToStr(ActionPage);
end;

procedure TFederTouchBase.Report(ML: TStrings);
var
  cb: TCornerBtn;
  s: string;

  function GetLocationString(cp: TCornerPos): string;
  begin
    case cp of
      cpTL: result := 'TL';
      cpTR: result := 'TR';
      cpBL: result := 'BL';
      cpBR: result := 'BR';
      cpT: result := 'T';
      cpR: result := 'R';
      cpB: result := 'B';
      cpL: result := 'L';
    end;
  end;

  procedure AddLine(cb: TCornerBtn);
  begin
    s := Format('%.2d: %s, %s = %s', [
      cb.ID,
      GetLocationString(cb.CornerPos),
      Main.ActionHandler.GetCaption(cb.Action2),
      cb.Caption
      ]);
    ML.Add(s);
  end;

begin
  for cb in CornerBtnList do
    AddLine(cb);
end;

procedure TFederTouchBase.InitHandlers(T: TTouchBtn);
begin
  T.Shape.OnClick := nil;

  T.Shape.OnMouseDown := OnMouseDown;
  T.Shape.OnMouseMove := OnMouseMove;
  T.Shape.OnMouseUp := OnMouseUp;
  T.Shape.OnMouseLeave := OnMouseLeave;
end;

procedure TFederTouchBase.InitClick(T: TTouchBtn);
begin
  T.Shape.OnClick := ToolBtnClick;
end;

procedure TFederTouchBase.ToolBtnClick(Sender: TObject);
begin
  ToggleTouchFrame;
end;

procedure TFederTouchBase.ToggleTouchFrame;
begin
  FrameVisible := not FrameVisible;
end;

end.
