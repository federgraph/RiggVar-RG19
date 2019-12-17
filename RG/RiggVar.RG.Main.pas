unit RiggVar.RG.Main;

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
  Rggunit4,
  RiggVar.RG.Data;

type
  TRggMain = class
  private
    function GetRigg: TRigg;
  public

    constructor Create;
    destructor Destroy; override;

    procedure DoBigWheel(Delta: single);
    procedure DoSmallWheel(Delta: single);

    procedure LoadTrimm(fd: TRggData);
    procedure SaveTrimm(fd: TRggData);

    property Rigg: TRigg read GetRigg;
  end;

implementation

uses
  RiggUnit;

{ TRggMain }

constructor TRggMain.Create;
begin
  inherited Create;
end;

destructor TRggMain.Destroy;
begin
  inherited;
end;

procedure TRggMain.DoBigWheel(Delta: single);
begin

end;

procedure TRggMain.DoSmallWheel(Delta: single);
begin

end;

function TRggMain.GetRigg: TRigg;
begin
  result := RiggModul.Rigg;
end;

procedure TRggMain.LoadTrimm(fd: TRggData);
begin

end;

procedure TRggMain.SaveTrimm(fd: TRggData);
begin

end;

end.
