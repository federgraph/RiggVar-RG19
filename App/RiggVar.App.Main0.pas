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

uses
  System.SysUtils,
  System.Classes,
  RiggVar.RG.Main;

type
  TMain0 = class(TRggMain)
  protected
    procedure Viewpoint3;
    procedure ViewpointA;
    procedure ViewpointS;
    procedure ViewpointT;
  public
    IsOrthoProjection: Boolean;

    Scale: single;
    IsRetina: Boolean;

    constructor Create;
    destructor Destroy; override;

    procedure HandleAction(fa: Integer); virtual;
    function GetChecked(fa: Integer): Boolean; virtual;

    procedure DoMouseWheel(Shift: TShiftState; WheelDelta: Integer);
  end;

implementation

uses
  RggTypes,
  RggModul,
  RiggVar.App.Main;

{ TMain0 }

constructor TMain0.Create;
begin
  inherited;
  Scale := MainVar.Scale;
  IsRetina := Scale > 1;
end;

destructor TMain0.Destroy;
begin
  inherited;
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

function TMain0.GetChecked(fa: Integer): Boolean;
begin
  result := False;
end;

procedure TMain0.HandleAction(fa: Integer);
begin

end;

end.
