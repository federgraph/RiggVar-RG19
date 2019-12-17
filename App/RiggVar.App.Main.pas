unit RiggVar.App.Main;

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
  RiggVar.Util.Logger;

type
  TMain = class
  public
    Logger: TLogger;
    IsUp: Boolean;
    constructor Create;
    destructor Destroy; override;
  end;

var
  Main: TMain;

implementation

{ TMain }

constructor TMain.Create;
begin
  Main := Self;
  Logger := TLogger.Create;
end;

destructor TMain.Destroy;
begin
  Logger.Free;
  inherited;
end;

end.
