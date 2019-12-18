unit RggPBox;

interface

uses
  Windows,
  Messages,
  Types,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  ExtCtrls;

type
  TRggPaintBox = class(TPaintBox)
  private
    procedure WMRButtonDown(var message: TWMRButtonDown); message WM_RBUTTONDOWN;
    procedure WMRButtonDblClk(var message: TWMRButtonDblClk); message WM_RBUTTONDBLCLK;
    procedure WMRButtonUp(var message: TWMRButtonUp); message WM_RBUTTONUP;
  protected
    procedure WndProc(var message: TMessage); override;
  public
    procedure IncludeCSClicked;
    procedure ExcludeCSClicked;
    procedure IncludeCSLButtonDown;
    procedure ExcludeCSLButtonDown;
  published
  end;

implementation

procedure TRggPaintBox.IncludeCSClicked;
var
  temp: TControlState;
begin
  temp := ControlState;
  Include(temp, csClicked);
  ControlState := temp;
end;

procedure TRggPaintBox.ExcludeCSClicked;
var
  temp: TControlState;
begin
  temp := ControlState;
  Exclude(temp, csClicked);
  ControlState := temp;
end;

procedure TRggPaintBox.IncludeCSLButtonDown;
var
  temp: TControlState;
begin
  temp := ControlState;
  Include(temp, csLButtonDown);
  ControlState := temp;
end;

procedure TRggPaintBox.ExcludeCSLButtonDown;
var
  temp: TControlState;
begin
  temp := ControlState;
  Exclude(temp, csLButtonDown);
  ControlState := temp;
end;

procedure TRggPaintBox.WMRButtonDblClk(var message: TWMRButtonDblClk);
begin
  SendCancelMode(Self);
  if csCaptureMouse in ControlStyle then
    MouseCapture := True;
  if csClickEvents in ControlStyle then
    DblClick;
  inherited;
end;

procedure TRggPaintBox.WMRButtonDown(var message: TWMRButtonDown);
begin
  SendCancelMode(Self);
  if csCaptureMouse in ControlStyle then
    MouseCapture := True;
  if csClickEvents in ControlStyle then
    IncludeCSClicked;
  inherited;
end;

procedure TRggPaintBox.WMRButtonUp(var message: TWMRButtonUp);
begin
  if csCaptureMouse in ControlStyle then
    MouseCapture := False;
  if csClicked in ControlState then
  begin
    ExcludeCSClicked;
    if PtInRect(ClientRect, SmallPointToPoint(message.Pos)) then
      Click;
  end;
  message.Result := 1; // unterdrückt Popups
  inherited;
end;

procedure TRggPaintBox.WndProc(var message: TMessage);
begin
  if (message.Msg >= WM_MOUSEFIRST) and (message.Msg <= WM_MOUSELAST) then
  begin
    if not(csDoubleClicks in ControlStyle) then
      case message.Msg of
        WM_RBUTTONDBLCLK:
          Dec(message.Msg, WM_LBUTTONDBLCLK - WM_LBUTTONDOWN);
      end;
    case message.Msg of
      WM_RBUTTONDOWN, WM_RBUTTONDBLCLK:
        IncludeCSLButtonDown;
      WM_RBUTTONUP:
        ExcludeCSLButtonDown;
    end;
  end;
  inherited WndProc(message);
end;

end.
