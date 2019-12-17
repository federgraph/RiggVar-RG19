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
  public
    Logger: TLogger;
    IsRetina: Boolean;
    RetinaScale: single;
    constructor Create;
    destructor Destroy; override;
    procedure Init;
    procedure DropTargetDropped(fn: string); virtual;
  end;

implementation

uses
  Clipbrd,
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

procedure TMain0.Init;
begin
end;

procedure TMain0.CopyText;
begin
  Clipboard.AsText := FL.Text;
  Logger.Info('in CopyText ( check clipboard )');
end;

end.

