unit RiggVar.RG.Track;

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
  SysUtils,
  System.Classes,
  System.UITypes,
  RiggVar.RG.Def;

type
  TFederTrackbar = class
  private
    FValueOld: Integer;
    FValue: Integer;
    procedure SetValue(const AValue: Integer);
  public
    Min: Integer;
    Max: Integer;
    Frequency: Integer;
    LineSize: Integer;
    PageSize: Integer;
    OnChange: TNotifyEvent;
    Tracking: Boolean;
    property Value: Integer read FValue write SetValue;
    property ValueNoChange: Integer read FValue write FValue;
  end;

implementation

{ TFederTrackbar }

procedure TFederTrackbar.SetValue(const AValue: Integer);
begin
  if (AValue <= Min) then
    FValue := Min
  else if AValue >= Max then
    FValue := Max
  else
    FValue := AValue;

  if (FValueOld <> FValue) and Assigned(OnChange) then
  begin
    FValueOld := FValue;
    OnChange(self);
  end;
end;

end.
