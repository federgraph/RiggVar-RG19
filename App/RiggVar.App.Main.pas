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
  System.Classes,
//  RiggVar.FB.Scheme,
//  RiggVar.FB.Transform,
  RiggVar.App.Main1;

type
  TMain = TMain1;

var
  Main: TMain;
  CmdLineInt: Integer = 0;
  CmdLineVal: string = '';

type
  MainConst = class
  const
    MaxModelCount = 1;
    PerspectiveZoomDefault = 10;
    OrthoZoomDefault = 0.5;
    TrackbarFrequency = 0.1;
    PlotCount = 11;
    SceneCount = 5;
    GraphCount = 5;
    FigureCount = 4;
    DrawFigureCount = 7;
    DefaultMeshSize = 256;
    DefaultBtnFontSize = 24;
  end;

  MainVar = class
  private
    class function GetRG: Boolean; static;
    class function GetFC: Boolean; static;
    class function GetFR: Boolean; static;
    class procedure SetRG(const Value: Boolean); static;
    class procedure SetFC(const Value: Boolean); static;
    class procedure SetFR(const Value: Boolean); static;
  public
  class var
    AppIsClosing: Boolean;
    BatchStopRequested: Boolean;
    WantTextureRepeat: Boolean;
    WantOnResize: Boolean;
    TextureRepeat: Boolean;
    ShouldRecycleSocket: Boolean;
    AppTitle: string;
    ShowDebugData: Boolean;

//    ColorScheme: TColorScheme;
    //Transform2D: TTransform2D;
//    Transform3D: TTransform3D;

    Raster: Integer;
    WheelFrequency: single;
    ShiftState: TShiftState;

    ClientWidth: Integer;
    ClientHeight: Integer;
    IconSize: Integer;
    NoCopyFlag: Boolean;
    PngCopyFlag: Boolean;
    HardCopyFlag: Boolean;

    CurrentApp: Integer;

    class constructor Create;
    class destructor Destroy;
    class property FR: Boolean read GetFR write SetFR;
    class property FC: Boolean read GetFC write SetFC;
    class property RG: Boolean read GetRG write SetRG;
  end;

var
  IsWebApp: Boolean = false;
  IsService: Boolean = false;
  IsWinGUI: Boolean = false;
  IsTest: Boolean = false;
  IsSandboxed: Boolean = false;
  WantErrorWindow: Boolean = true;
  CacheRequestToken: string;

const
  ColorSchemeCount = 6;
  AppFC = 0;
  AppRG = 1;
  AppFR = 2;

  TrimmFileName = 'Trimm-File.txt';
  TrimmFileNameAuto = 'Trimm-File-Auto.txt';

implementation

{ MainVars }

class constructor MainVar.Create;
begin
  { init class vars }
  AppIsClosing := False;
  BatchStopRequested := False;
  WantTextureRepeat := False;
  TextureRepeat := False;
  ShouldRecycleSocket := False;
  AppTitle := '';

//  ColorScheme := TColorScheme.Create(1, 4, 5);

//  Transform2D := TTransform2D.Create;
//  Transform2D.Init;

//  Transform3D := TTransform3D.Create;
//  Transform3D.Init;

  Raster := 70;
  WheelFrequency := 1;
  ShiftState := [];

  IconSize := 640;
  NoCopyFlag := False;
  PngCopyFlag := False;
  HardCopyFlag := False;
end;

class destructor MainVar.Destroy;
begin
//  Transform2D.Free;
//  Transform3D.Free;
  inherited;
end;

class function MainVar.GetFC: Boolean;
begin
  result := CurrentApp = 0;
end;

class function MainVar.GetFR: Boolean;
begin
  result := CurrentApp = 2;
end;

class function MainVar.GetRG: Boolean;
begin
  result := CurrentApp = 1;
end;

class procedure MainVar.SetFC(const Value: Boolean);
begin
  CurrentApp := 0;
end;

class procedure MainVar.SetFR(const Value: Boolean);
begin
  CurrentApp := 2;
end;

class procedure MainVar.SetRG(const Value: Boolean);
begin
  CurrentApp := 1;
end;

end.

