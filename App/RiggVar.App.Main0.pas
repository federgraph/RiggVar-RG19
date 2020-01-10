unit RiggVar.App.Main0;

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

{$ifdef FPC}
{$mode delphi}
{$endif}

uses
  System.SysUtils,
  System.Classes,
  RiggVar.Util.Logger;

type
  TMain0 = class
  protected
    FL: TStringList;
    procedure CopyText;
    procedure Viewpoint3;
    procedure ViewpointA;
    procedure ViewpointS;
    procedure ViewpointT;
  public
    Logger: TLogger;
    IsUp: Boolean;
    IsOrthoProjection: Boolean;
    constructor Create;
    destructor Destroy; override;

    procedure DoMouseWheel(Shift: TShiftState; WheelDelta: Integer);

    procedure DropTargetDropped(fn: string); virtual;
  end;

implementation

uses
  Clipbrd,
  RggTypes,
  RiggUnit,
  RiggVar.App.Main;

{ TMain0 }

constructor TMain0.Create;
begin
  inherited;
  Logger := TLogger.Create;
  FL := TStringList.Create;
end;

destructor TMain0.Destroy;
begin
  Logger.Free;
  FL.Free;
  inherited;
end;

procedure TMain0.DropTargetDropped(fn: string);
begin

end;

procedure TMain0.CopyText;
begin
  Clipboard.AsText := FL.Text;
  Logger.Info('in CopyText ( check clipboard )');
end;

procedure TMain0.ViewpointS;
begin
  RiggModul.ViewPoint := TViewpoint.vpSeite;
end;

procedure TMain0.ViewpointA;
begin
  RiggModul.ViewPoint := TViewpoint.vpAchtern;
end;

procedure TMain0.ViewpointT;
begin
  RiggModul.ViewPoint := TViewpoint.vpTop;
end;

procedure TMain0.Viewpoint3;
begin
  RiggModul.ViewPoint := TViewpoint.vp3D;
end;

procedure TMain0.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer);
begin
  if ssCtrl in Shift then
  begin
    Main.DoBigWheel(WheelDelta);
  end
  else if ssShift in Shift then
  begin
    Main.DoSmallWheel(WheelDelta);
  end
end;

end.

