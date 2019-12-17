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
  System.SysUtils,
  System.Classes,
  System.Math,
  RiggVar.RG.Data,
  RiggVar.RG.Def,
  RiggVar.RG.Track,
  RiggVar.RG.Graph,
  RggScroll,
  RggTypes,
  Rggunit4,
  VCalc116;

type
  TFederAction = Integer;

  TRggMain0 = class
  private
    procedure SetHullVisible(const Value: Boolean);
  protected
    FAction: TFederAction;
    FParam: TFederParam;
    FHullVisible: Boolean;

    function GetBigStep: single;
    function GetSmallStep: single;
    procedure TrackBarChange(Sender: TObject);
    procedure RggSpecialDoOnTrackBarChange; virtual;
  public
    RggTrackbar: TFederTrackbar;

    constructor Create;
    destructor Destroy; override;

    procedure TestStream;

    procedure ToggleHullVisible; virtual;

    procedure UpdateText(ClearFlash: Boolean = False);
    procedure HandleKey(KeyChar: Char);

    procedure DoWheel(Delta: single);
    procedure DoZoom(Delta: single);
    procedure DoRotation(Delta: single);

    procedure DoBigWheel(Delta: single);
    procedure DoSmallWheel(Delta: single);
    procedure DoRasterWheel(Delta: single);

    procedure ViewportChanged(Sender: TObject);
    property HullVisible: Boolean read FHullVisible write SetHullVisible;
  end;

  TRggMain = class(TRggMain0)
  private
    FFixName: TRiggPoints;
    FixPunkt: TRealPoint;
    FVisible: Boolean;

    BiegungGF: single;
    BiegungGFD: single;

    TML: TStrings;

    function FormatValue(Value: single): string;
    procedure DoBiegungGF;

    procedure ChangeRigg(Value: single);

    procedure SetParam(Value: TFederParam);
    procedure SetParamValue(idx: TFederParam; Value: single);
    function GetParamValue(idx: TFederParam): single;
    procedure SetFixName(const Value: TRiggPoints);
    function GetCurrentValue: single;
    procedure SetCurrentValue(const Value: single);
    function GetParamValueString(fp: TFederParam): string;
    function GetParamValueStringDiff(fp: TFederParam): string;
    function GetMastfall: string;
    procedure SetupTrackBarForRgg;
    procedure SetVisible(const Value: Boolean);
    procedure AL(A: string; fp: TFederParam);
    procedure BL(A: string; C: string);
  protected
    procedure InitFactArray;
    procedure RggSpecialDoOnTrackBarChange; override;
  public
    MinValCaption: string;
    MaxValCaption: string;
    IstValCaption: string;
    ParamCaption: string;

    FactArray: TRggFactArray;

    Rigg: TRigg;

    Demo: Boolean;

    StrokeRigg: TStrokeRigg;

    SofortBerechnen: Boolean;
    Grauzeichnen: Boolean;
    LEDShape: Boolean;
    StatusText: string;

    InitialFixName: TRiggPoints;

    UpdateTextCounter: Integer;

    constructor Create(ARigg: TRigg);
    destructor Destroy; override;

    procedure Reset;
    procedure UpdateGetriebe;
    procedure UpdateGraph;

    procedure LoadTrimm(fd: TRggData);
    procedure SaveTrimm(fd: TRggData);

    procedure Init;
    procedure InitStrokeRigg;

    procedure Init420;
    procedure InitLogo;
    procedure InitSalingTyp(i: Integer);

    procedure Draw;

    function GetPlotValue(PlotID: Integer; x, y: single): single;

    procedure DebugBiegungGF(ML: TStrings);
    procedure UpdateColumnC(ML: TStrings);
    procedure UpdateColumnD(ML: TStrings);
    procedure UpdateTrimmText(ML: TStrings);
    procedure UpdateDataText(ML: TStrings);
    procedure UpdateDiffText(ML: TStrings);
    procedure UpdateFactArrayFromRigg;

    function Param2Text(P: TFederParam): string;
    function Text2Param(T: string): TFederParam;

    procedure ToggleRenderOption(fa: TFederAction);
    procedure SetParameter(fa: TFederAction);
    procedure SetOption(fa: TFederAction);

    property Action: TFederAction read FAction;
    property FixName: TRiggPoints read FFixName write SetFixName;
    property Param: TFederParam read FParam write SetParam;
    property ParamValue[index: TFederParam]: single read GetParamValue write SetParamValue;
    property ParamValueString[index: TFederParam]: string read GetParamValueString;
    property ParamValueStringDiff[index: TFederParam]: string read GetParamValueStringDiff;
    property CurrentValue: single read GetCurrentValue write SetCurrentValue;
    property Mastfall: string read GetMastfall;

    property Visible: Boolean read FVisible write SetVisible;
  end;

implementation

uses
  FrmMain,
  RggDoc,
  RiggUnit,
  RiggVar.App.Main;

const
  tfs = '%-3s %s %8s %6s';

{ TRggMain }

constructor TRggMain.Create(ARigg: TRigg);
begin
  inherited Create;

  InitialFixName := ooD;

  Demo := False;

  Rigg := ARigg;
  FactArray := Rigg.GSB;
  Rigg.ControllerTyp := ctOhne;
  Init;
end;

destructor TRggMain.Destroy;
begin
  StrokeRigg.Free;
  Rigg.Free;
  inherited;
end;

procedure TRggMain.Init;
begin
  if not Assigned(RggTrackbar) then
    Exit;

  RggTrackbar.OnChange := TrackBarChange;
  StrokeRigg := TStrokeRigg.Create(Rigg);

  InitFactArray;

  Param := fpVorstag;
  FixName := InitialFixName;

  InitStrokeRigg;
  Draw;
end;

procedure TRggMain.InitStrokeRigg;
var
  cr: TStrokeRigg;
begin
  cr := StrokeRigg;
  cr.SalingTyp := Rigg.SalingTyp;
  cr.ControllerTyp := Rigg.ControllerTyp;
  cr.Koordinaten := Rigg.rP;
  cr.SetMastKurve(Rigg.MastLinie, Rigg.lc, Rigg.beta);
  cr.WanteGestrichelt := not Rigg.GetriebeOK;
end;

procedure TRggMain.SetParameter(fa: TFederAction);
begin
  FAction := fa;
  case fa of
    faController: Param := fpController;
    faWinkel: Param := fpWinkel;
    faVorstag: Param := fpVorstag;
    faWante: Param := fpWante;
    faWoben: Param := fpWoben;
    faSalingH: Param := fpSalingH;
    faSalingA: Param := fpSalingA;
    faSalingL: Param := fpSalingL;
    faSalingW: Param := fpSalingW;
    faMastfallF0C: Param := fpMastfallF0C;
    faMastfallF0F: Param := fpMastfallF0F;
    faMastfallVorlauf: Param := fpMastfallVorlauf;
    faBiegung: Param := fpBiegung;
    faMastfussD0X: Param := fpD0X;
    faParamT1: Param := fpT1;
    faParamT2: Param := fpT2;
  end;
end;

procedure TRggMain.SetCurrentValue(const Value: single);
begin
  FactArray.Find(FParam).Ist := Value;
end;

procedure TRggMain.SetFixName(const Value: TRiggPoints);
begin
  FFixName := Value;
  FixPunkt := Rigg.rP[Value];
  Draw;
end;

procedure TRggMain.SetOption(fa: TFederAction);
begin
  case fa of
    faHull: ToggleHullVisible;
    faDemo:
    begin
      Demo := not Demo;
      if Demo then
      begin
        Rigg.SetDefaultDocument;
        InitFactArray;
        SetParam(FParam);
      end;
    end;
  end;
  UpdateText;
end;

procedure TRggMain.UpdateGraph;
var
  sr: TStrokeRigg;
begin
  ChangeRigg(CurrentValue);
  case FParam of
    fpController,
    fpWinkel,
    fpVorstag,
    fpWante,
    fpWoben,
    fpSalingH,
    fpSalingA,
    fpSalingL,
    fpSalingW,
    fpVorstagOS,
    fpWPowerOS,
    fpMastfallVorlauf:
    begin
      if not RiggModul.AllreadyUpdatedGetriebeFlag then
        UpdateGetriebe;
    end;

    fpMastfallF0C,
    fpMastfallF0F,
    fpBiegung:
    begin
      UpdateGetriebe; //otherwise Input-Hysterese when scrolling over Text-Form
      Rigg.Schnittkraefte;
    end;

    fpD0X:
    begin
      Rigg.Reset;
      UpdateGetriebe;
    end;
  end;

  if StrokeRigg <> nil then
  begin
    sr := StrokeRigg;
    sr.Koordinaten := Rigg.rP;
    sr.SetMastKurve(Rigg.MastLinie, Rigg.lc, Rigg.beta);
    sr.WanteGestrichelt := not Rigg.GetriebeOK;
    sr.Bogen := (FParam <> fpWinkel);
    Draw;
  end;
end;

procedure TRggMain.ChangeRigg(Value: single);
var
  tempH, tempA, tempL, tempW: double;
begin
  case FParam of
    fpController: Rigg.RealGlied[fpController] := Value;

    fpWinkel: Rigg.RealGlied[fpWinkel] := Value * pi / 180;

    fpVorstag: Rigg.RealGlied[fpVorstag] := Value;
    fpWante: Rigg.RealGlied[fpWante] := Value;
    fpWoben: Rigg.RealGlied[fpWoben] := Value;

    fpSalingH:
    begin
      tempH := FactArray.SalingH.Ist;
      tempA := FactArray.SalingA.Ist;
      tempL := sqrt(sqr(tempA / 2) + sqr(tempH));
      tempW := arctan2(tempH * 2, tempA) * 180 / pi;

      Rigg.RealGlied[fpSalingH] := tempH;
      Rigg.RealGlied[fpSalingA] := tempA;
      Rigg.RealGlied[fpSalingL] := tempL;

      //SalingH no change (just changed)
      //SalingA no change (kept unchanged)
      FactArray.SalingL.Ist := tempL;
      FactArray.SalingW.Ist := tempW;
    end;

    fpSalingA:
    begin
      tempH := FactArray.SalingH.Ist;
      tempA := FactArray.SalingA.Ist;
      tempL := sqrt(sqr(tempA / 2) + sqr(tempH));
      tempW := arctan2(tempH * 2, tempA) * 180 / pi;

      Rigg.RealGlied[fpSalingH] := tempH;
      Rigg.RealGlied[fpSalingA] := tempA;
      Rigg.RealGlied[fpSalingL] := tempL;

      //SalingH no change (kept unchanged)
      //SalingA no change (just changed)
      FactArray.SalingL.Ist := tempL;
      FactArray.SalingW.Ist := tempW;
    end;

    fpSalingL:
    begin
      tempW := FactArray.SalingW.Ist;
      tempL := FactArray.SalingL.Ist;
      tempH := tempL * sin(tempW * pi / 180);
      tempA := 2 * tempL * cos(tempW * pi / 180);

      Rigg.RealGlied[fpSalingH] := tempH;
      Rigg.RealGlied[fpSalingA] := tempA;
      Rigg.RealGlied[fpSalingL] := tempL;

      FactArray.SalingH.Ist := tempH;
      FactArray.SalingA.Ist := tempA;
      //SalingL no change (just changed)
      //SalingW no change (kept unchanged)
    end;

    fpSalingW:
    begin
      tempW := FactArray.SalingW.Ist;
      tempL := FactArray.SalingL.Ist;
      tempH := tempL * sin(tempW * pi / 180);
      tempA := 2 * tempL * cos(tempW * pi / 180);

      Rigg.RealGlied[fpSalingH] := tempH;
      Rigg.RealGlied[fpSalingA] := tempA;
      Rigg.RealGlied[fpSalingL] := tempL;

      FactArray.SalingH.Ist := tempH;
      FactArray.SalingA.Ist := tempA;
      //SalingL no change
      //SalingW no change
    end;

    fpMastfallF0F:
      Rigg.BiegeUndNeigeF(Value - FactArray.MastfallVorlauf.Ist, FactArray.Biegung.Ist);

    fpMastfallF0C:
      Rigg.BiegeUndNeigeC(Value, FactArray.Biegung.Ist);

    fpMastfallVorlauf:
      Rigg.MastfallVorlauf := Value;

    fpBiegung:
      Rigg.BiegeUndNeigeC(FactArray.MastfallF0C.Ist, Value);

    fpD0X:
      Rigg.iP[ooD0, X] := Round(Value);
  end;
end;

procedure TRggMain.SetParamValue(idx: TFederParam; Value: single);
var
  sb: TRggSB;
begin
  sb := FactArray.Find(idx);
  if Assigned(sb) then
  begin
    if Value = CurrentValue then
      //do nothing
    else if Value >= sb.Max then
      sb.Ist := sb.Max
    else if Value <= sb.Min then
      sb.Ist := sb.Min
    else
      sb.Ist := Value;

    RiggModul.AllreadyUpdatedGetriebeFlag := False;
    RiggModul.DoOnWheelScroll(idx, Round(Value));
    UpdateGraph;
  end;
end;

function TRggMain.GetCurrentValue: single;
begin
  result := FactArray.Find(FParam).Ist;
end;

function TRggMain.GetMastfall: string;
begin
  result := Format('%.0f', [FactArray.MastfallF0F.Ist - FactArray.MastfallVorlauf.Ist]);
end;

function TRggMain.GetParamValue(idx: TFederParam): single;
begin
  result := FactArray.Find(idx).Ist;
end;

function TRggMain.GetParamValueString(fp: TFederParam): string;
begin
  result := Format('%.0f', [FactArray.Find(fp).Ist]);
end;

function TRggMain.GetParamValueStringDiff(fp: TFederParam): string;
var
  tv: single;
  fd: TRggData;
begin
  fd := Main.Trimm0;
  tv := FactArray.Find(fp).Ist;
  case fp of
    fpD0X: tv := tv - fd.D0X;
    fpController: tv := tv - fd.CPPos;
    fpWinkel: tv := tv - fd.WIPos;
    fpVorstag: tv := tv - fd.VOPos;
    fpWante: tv := tv - fd.WLPos;
    fpWoben: tv := tv - fd.WOPos;
    fpSalingH: tv := tv - fd.SHPos;
    fpSalingA: tv := tv - fd.SAPos;
    fpSalingL: tv := tv - fd.SLPos;
    fpSalingW: tv := tv - fd.SWPos;
    fpMastfallVorlauf: tv := tv - fd.MV;
    fpMastfallF0C: tv := tv - fd.F0C;
    fpMastfallF0F: tv := tv - fd.F0F;
    fpBiegung: tv := tv - fd.Bie;
  end;
  result := Format('%.0f', [tv]);
end;

procedure TRggMain.SetParam(Value: TFederParam);
begin
//  Main.Param := Integer(Value);

  if Demo then
  begin
    InitFactArray;
    ChangeRigg(FactArray.Find(FParam).Ist); // Istwert zurücksetzen
    Rigg.RealGlied[fpVorstag] := FactArray.Vorstag.Ist;
  end;

  if Value = fpWinkel then
  begin
    { Wanten straff ziehen }
    case Rigg.SalingTyp of
      stFest:
        Rigg.MakeSalingHBiggerFS(FactArray.SalingH.Ist);
      stDrehbar:
        Rigg.MakeSalingLBiggerDS(FactArray.SalingL.Ist);
    end;
  end;

  if Value = fpController then
    Rigg.ControllerTyp := ctDruck
  else
    Rigg.ControllerTyp := ctOhne;

  if Assigned(StrokeRigg) then
  begin
    StrokeRigg.ControllerTyp := Rigg.ControllerTyp;
  end;

  Rigg.ManipulatorMode := (Value = fpWinkel);
  FParam := Value;
  CurrentValue := FactArray.Find(FParam).Ist;
  SetupTrackBarForRgg;
  UpdateGraph;
end;

function TRggMain.Text2Param(T: string): TFederParam;
begin
  result := fpT1;
  if T = 'Controller' then
    result := fpController
  else if T = 'Winkel' then
    result := fpWinkel
  else if T = 'Vorstag' then
    result := fpVorstag
  else if T = 'Wante' then
    result := fpWante
  else if (T = 'Wante oben') or (T = 'Woben') then
    result := fpWoben
  else if (T = 'Saling Höhe') or (T = 'SalingH') then
    result := fpSalingH
  else if (T = 'Saling Abstand') or (T = 'SalingA') then
    result := fpSalingA
  else if (T = 'Saling Länge') or (T = 'SalingL') then
    result := fpSalingL
  else if (T = 'Saling Winkel') or (T = 'SalingW') then
    result := fpSalingW
  else if T = 'Mastfall F0C' then
    result := fpMastfallF0C
  else if T = 'Mastfall F0F' then
    result := fpMastfallF0F
  else if T = 'Mastfall Vorlauf' then
    result := fpMastfallVorlauf
  else if T = 'Biegung' then
    result := fpBiegung
  else if T = 'Mastfuß D0x' then
    result := fpD0X
  else if T = 't1' then
    result := fpT1
  else if T = 't2' then
    result := fpT2
    ;
end;

function TRggMain.Param2Text(P: TFederParam): string;
begin
  result := '';
  if P = fpController then
    result := 'Controller'
  else if P = fpWinkel then
    result := 'Winkel'
  else if P = fpVorstag then
    result := 'Vorstag'
  else if P = fpWante then
    result := 'Wante'
  else if P = fpWoben then
    result := 'Wante oben'
  else if P = fpSalingH then
    result := 'Saling Höhe'
  else if P = fpSalingA then
    result := 'Saling Abstand'
  else if P = fpSalingL then
    result := 'Saling Länge'
  else if P = fpSalingW then
    result := 'Saling Winkel'
  else if P = fpMastfallF0C then
    result := 'Mastfall F0C'
  else if P = fpMastfallF0F then
    result := 'Mastfall F0F'
  else if P = fpMastfallVorlauf then
    result := 'Mastfall Vorlauf'
  else if P = fpBiegung then
    result := 'Biegung'
  else if P = fpD0X then
    result := 'Mastfuß D0x'
  else if P = fpT1 then
    result := 't1'
  else if P = fpT2 then
    result := 't2'
    ;
end;

procedure TRggMain.Reset;
begin
  Rigg.SetDefaultDocument;
  Rigg.ControllerTyp := ctOhne;
  InitFactArray;
  UpdateGraph;
end;

procedure TRggMain.InitFactArray;
var
  temp, tempH, tempA: single;
  i: TFederParam;
  sb: TRggSB;
begin
//  FactArray.Controller.Ist := Rigg.GSB.Controller.Ist;
//  FactArray.Winkel.Ist := Rigg.GSB.Winkel.Ist;
//  FactArray.Vorstag.Ist := Rigg.GSB.Vorstag.Ist;
//  FactArray.Wante.Ist := Rigg.GSB.Wante.Ist;
//  FactArray.Woben.Ist := Rigg.GSB.Woben.Ist;
    tempH := Rigg.GSB.SalingH.Ist;
//  FactArray.SalingH.Ist := tempH;
    tempA := Rigg.GSB.SalingA.Ist;
//  FactArray.SalingA.Ist := tempA;
//  FactArray.SalingL.Ist := Rigg.GSB.SalingL.Ist;
  FactArray.SalingW.Ist := Round(180 / pi * arctan2(tempH * 2, tempA));

  FactArray.MastfallF0C.Ist := Rigg.RealTrimm[tiMastfallF0C];
  FactArray.MastfallF0F.Ist := Rigg.RealTrimm[tiMastfallF0F];
  FactArray.Biegung.Ist := Rigg.RealTrimm[tiBiegungS];
  FactArray.D0X.Ist := Rigg.iP[ooD0, X];

  FactArray.T1.Ist := 650;
  FactArray.T2.Ist := 150;

  // allgemein setzen
  for i := fpController to fpD0X do
  begin
    sb := FactArray.Find(i);
    sb.TinyStep := 1;
    sb.BigStep := 10;
    temp := sb.Ist;
    sb.Min := temp - 100;
    sb.Max := temp + 100;
  end;

  // speziell überschreiben
  if WantLogoData then
  begin
    FactArray.Winkel.Min := 70;
    FactArray.Winkel.Max := 120;
    // FactArray.Woben.Min := 2000;
    // FactArray.Woben.Max := 2100;
    FactArray.SalingW.Min := 40;
    FactArray.SalingW.Max := 60;
    // FactArray.MastfallF0F.Max := 6400;
    FactArray.Biegung.Min := 0;
    FactArray.Biegung.Max := 120;
  end
  else
  begin

    FactArray.Wante.Min := 4020;
    FactArray.Wante.Max := 4220;

    FactArray.Vorstag.Min := 4200;
    FactArray.Vorstag.Max := 5000;

    FactArray.Winkel.Min := 80;
    FactArray.Winkel.Max := 115;

    FactArray.Woben.Min := 2000;
    FactArray.Woben.Max := 2100;

    FactArray.SalingH.Min := 170;
    FactArray.SalingH.Max := 1020;

    FactArray.SalingA.Min := 250;
    FactArray.SalingA.Max := 1550;

    FactArray.SalingL.Ist := 480;
    FactArray.SalingL.Min := 240;
    FactArray.SalingL.Max := 1200;

    FactArray.SalingW.Min := 15;
    FactArray.SalingW.Max := 87;

    FactArray.D0X.Min := 2600;
    FactArray.D0X.Ist := 2870;
    FactArray.D0X.Max := 3300;

    FactArray.MastfallF0C.Min := 4000;
    FactArray.MastfallF0C.Ist := 4800;
    FactArray.MastfallF0C.Max := 5100;

    FactArray.MastfallF0F.Min := 5370;
    FactArray.MastfallF0F.Ist := 6070;
    FactArray.MastfallF0F.Max := 6570;

    FactArray.MastfallVorlauf.Min := 4950;
    FactArray.MastfallVorlauf.Ist := 5000;
    FactArray.MastfallVorlauf.Max := 5150;

    FactArray.Biegung.Min := 0;
    FactArray.Biegung.Max := 500;

    FactArray.T1.Min := 0;
    FactArray.T1.Max := 500;

    FactArray.T2.Min := 1;
    FactArray.T2.Max := 800;
  end;
end;

procedure TRggMain.UpdateFactArrayFromRigg;
var
  i: TFederParam;
  sb: TRggSB;
begin
  for i := fpController to fpD0X do
  begin
    sb := FactArray.Find(i);
    case i of
      fpController:
        sb.Ist := Rigg.RealGlied[fpController];
      fpWinkel:
        sb.Ist := Rigg.RealGlied[fpWinkel] * 180 / pi;
      fpVorstag:
        sb.Ist := Rigg.RealGlied[fpVorstag];
      fpWante:
        sb.Ist := Rigg.RealGlied[fpWante];
      fpWoben:
        sb.Ist := Rigg.RealGlied[fpWoben];
      fpSalingH:
        sb.Ist := Rigg.RealGlied[fpSalingH];
      fpSalingA:
        sb.Ist := Rigg.RealGlied[fpSalingA];
      fpSalingL:
        sb.Ist := Rigg.RealGlied[fpSalingL];
      fpSalingW:
        sb.Ist := arctan2(Rigg.RealGlied[fpSalingH] * 2, Rigg.RealGlied[fpSalingA]) * 180 / pi;
      fpMastfallF0C:
        sb.Ist := Abstand(Rigg.rP[ooF0], Rigg.rP[ooC]);
      fpMastfallF0F:
        sb.Ist := Abstand(Rigg.rP[ooF0], Rigg.rP[ooF]);
      fpBiegung:
        sb.Ist := Rigg.hd;
      fpD0X:
        sb.Ist := Rigg.rP[ooD0][x];
    end;
  end;

end;

function TRggMain.GetPlotValue(PlotID: Integer; x, y: single): single;
var
  tx, ty: single;
begin
  case PlotID of
    1..12:
    begin
      tx := FactArray.Vorstag.Ist;
      ty := FactArray.SalingL.Ist;
      Rigg.RealGlied[fpVorstag] := tx + x;
      Rigg.RealGlied[fpSalingA] := ty + y / 10;
      Rigg.UpdateGetriebe;
      if Rigg.GetriebeOK then
      begin
        result := Abstand(Rigg.rP[ooF0], Rigg.rP[ooF]); // - 5000;
        UpdateFactArrayFromRigg;
      end
      else
        result := 0;
    end;
    else
      result := 0;
  end;
end;

procedure TRggMain.LoadTrimm(fd: TRggData);
var
  temp, tempH, tempA: single;
  i: TFederParam;
  sb: TRggSB;
begin
  Rigg.SetDefaultDocument;
  Rigg.LoadFromFederData(fd);

//  FactArray.Controller.Ist := Rigg.GSB.Controller.Ist;
//  FactArray.Winkel.Ist := Rigg.GSB.Winkel.Ist;
//  FactArray.Vorstag.Ist := Rigg.GSB.Vorstag.Ist;
//  FactArray.Wante.Ist := Rigg.GSB.Wante.Ist;
//  FactArray.Woben.Ist := Rigg.GSB.Woben.Ist;
    tempH := Rigg.GSB.SalingH.Ist;
//  FactArray.SalingH.Ist := tempH;
    tempA := Rigg.GSB.SalingA.Ist;
//  FactArray.SalingA.Ist := tempA;
//  FactArray.SalingL.Ist := Rigg.GSB.SalingL.Ist;
  FactArray.SalingW.Ist := Round(180 / pi * arctan2(tempH * 2, tempA));

  FactArray.MastfallF0C.Ist := Rigg.RealTrimm[tiMastfallF0C];
  FactArray.MastfallF0F.Ist := Rigg.RealTrimm[tiMastfallF0F];
  FactArray.Biegung.Ist := Rigg.RealTrimm[tiBiegungS];
  FactArray.D0X.Ist := Rigg.iP[ooD0, X];

  fd.F0C := Round(FactArray.MastfallF0C.Ist);
  fd.F0F := Round(FactArray.MastfallF0F.Ist);
  fd.Bie := Round(FactArray.Biegung.Ist);

  // allgemein setzen
  for i := fpController to fpD0X do
  begin
    sb := FactArray.Find(i);
    sb.TinyStep := 1;
    sb.BigStep := 10;
    temp := sb.Ist;
    sb.Min := temp - 100;
    sb.Max := temp + 100;
  end;

  FactArray.Controller.Min := fd.CPMin;
  FactArray.Controller.Max := fd.CPMax;

  FactArray.Wante.Min := fd.WLMin;
  FactArray.Wante.Max := fd.WLMax;

  FactArray.Vorstag.Min := fd.VOMin;
  FactArray.Vorstag.Max := fd.VOMax;

  FactArray.Winkel.Min := fd.WIMin;
  FactArray.Winkel.Max := fd.WIMax;

  FactArray.Woben.Min := fd.WOMin;
  FactArray.Woben.Max := fd.WOMax;

  FactArray.SalingH.Min := fd.SHMin;
  FactArray.SalingH.Max := fd.SHMax;

  FactArray.SalingA.Min := fd.SAMin;
  FactArray.SalingA.Max := fd.SAMax;

  FactArray.SalingL.Min := fd.SLMin;
  FactArray.SalingL.Max := fd.SLMax;

  FactArray.SalingW.Min := fd.SWMin;
  FactArray.SalingW.Max := fd.SWMax;

  FactArray.D0X.Min := fd.D0X - 100;
  FactArray.D0X.Max := fd.D0X + 100;

  FactArray.MastfallVorlauf.Ist := fd.MV;
  FactArray.MastfallVorlauf.Min := fd.MV - 100;
  FactArray.MastfallVorlauf.Max := fd.MV + 100;

//  tempA := Abstand(Rigg.rP[ooF0], Rigg.rP[ooC]);
//  tempH := FactArray.MastfallF0C.Ist;
//  temp := tempA - tempH; // = 0
  temp := FactArray.MastfallF0C.Ist;
  FactArray.MastfallF0C.Min := temp - 700;
  FactArray.MastfallF0C.Max := temp + 500;

//  tempA := Abstand(Rigg.rP[ooF0], Rigg.rP[ooF]);
//  tempH := FactArray.MastfallF0F.Ist;
//  temp := tempA - tempH; // = 0
  temp := FactArray.MastfallF0F.Ist;
  FactArray.MastfallF0F.Min := temp - 700;
  FactArray.MastfallF0F.Max := temp + 500;

  FactArray.Biegung.Min := 0;
  FactArray.Biegung.Max := 500;

  FactArray.T1.Min := 0;
  FactArray.T1.Max := 800;

  FactArray.T2.Min := 1;
  FactArray.T2.Max := 800;

  SetParam(FParam);

end;

procedure TRggMain.SaveTrimm(fd: TRggData);
begin
  Rigg.SaveToFederData(fd);

  fd.CPMin := Round(FactArray.Controller.Min);
  fd.CPPos := Round(FactArray.Controller.Ist);
  fd.CPMax := Round(FactArray.Controller.Max);

  fd.SHMin := Round(FactArray.SalingH.Min);
  fd.SHPos := Round(FactArray.SalingH.Ist);
  fd.SHMax := Round(FactArray.SalingH.Max);

  fd.SAMin := Round(FactArray.SalingA.Min);
  fd.SAPos := Round(FactArray.SalingA.Ist);
  fd.SaMax := Round(FactArray.SalingA.Max);

  fd.SLMin := Round(FactArray.SalingL.Min);
  fd.SLPos := Round(FactArray.SalingL.Ist);
  fd.SLMax := Round(FactArray.SalingL.Max);

  fd.SWMin := Round(FactArray.SalingW.Min);
  fd.SWPos := Round(FactArray.SalingW.Ist);
  fd.SWMax := Round(FactArray.SalingW.Max);

  fd.VOMin := Round(FactArray.Vorstag.Min);
  fd.VOPos := Round(FactArray.Vorstag.Ist);
  fd.VOMax := Round(FactArray.Vorstag.Max);

  fd.WIMin := Round(FactArray.Winkel.Min);
  fd.WIPos := Round(FactArray.Winkel.Ist);
  fd.WIMax := Round(FactArray.Winkel.Max);

  fd.WLMin := Round(FactArray.Wante.Min);
  fd.WLPos := Round(FactArray.Wante.Ist);
  fd.WLMax := Round(FactArray.Wante.Max);

  fd.WOMin := Round(FactArray.Woben.Min);
  fd.WOPos := Round(FactArray.Woben.Ist);
  fd.WOMax := Round(FactArray.Woben.Max);
end;

procedure TRggMain.Init420;
var
  doc: TRggDocument;
begin
  doc := TRggDocument.Create;
  WantLogoData := False;
  doc.GetDefaultDocument;
  Rigg.SetDocument(doc);
  doc.Free;

  Rigg.ControllerTyp := TControllerTyp.ctOhne;
  InitFactArray();
  StrokeRigg.SalingTyp := Rigg.SalingTyp;
  SetParam(FParam);
  FixName := ooD;

  SaveTrimm(Main.Trimm7);
  Main.TrimmNoChange := 7;
end;

procedure TRggMain.InitLogo;
var
  doc: TRggDocument;
begin
  doc := TRggDocument.Create;
  WantLogoData := True;
  doc.GetDefaultDocument;
  Rigg.SetDocument(doc);
  doc.Free;

  Rigg.ControllerTyp := TControllerTyp.ctOhne;
  InitFactArray();
  StrokeRigg.SalingTyp := Rigg.SalingTyp;
  SetParam(FParam);
  FixName := ooD;

  SaveTrimm(Main.Trimm8);
  Main.TrimmNoChange := 8;
end;

procedure TRggMain.InitSalingTyp(i: Integer);
begin
  case i of
    0: Rigg.SalingTyp := stOhne;
    1: Rigg.SalingTyp := stDrehbar;
    2: Rigg.SalingTyp := stFest;
  end;
  StrokeRigg.SalingTyp := Rigg.SalingTyp;
  SetParam(FParam);
end;

procedure TRggMain.UpdateColumnC(ML: TStrings);
begin
  DoBiegungGF;
  ML.Add(ParamValueString[fpVorstag]);
  ML.Add(ParamValueString[fpWante]);
  ML.Add(ParamValueString[fpWoben]);
  ML.Add(ParamValueString[fpSalingH]);
  ML.Add(ParamValueString[fpSalingA]);
  ML.Add(ParamValueString[fpSalingL]);
  ML.Add(ParamValueString[fpSalingW]);
  ML.Add(ParamValueString[fpMastfallVorlauf]);
  ML.Add(Mastfall);
  ML.Add(ParamValueString[fpMastfallF0F]);
  ML.Add(ParamValueString[fpMastfallF0C]);
  ML.Add(ParamValueString[fpBiegung]);
  ML.Add(FormatValue(BiegungGF));
end;

procedure TRggMain.UpdateColumnD(ML: TStrings);
begin
  ML.Add(ParamValueStringDiff[fpVorstag]);
  ML.Add(ParamValueStringDiff[fpWante]);
  ML.Add(ParamValueStringDiff[fpWoben]);
  ML.Add(ParamValueStringDiff[fpSalingH]);
  ML.Add(ParamValueStringDiff[fpSalingA]);
  ML.Add(ParamValueStringDiff[fpSalingL]);
  ML.Add(ParamValueStringDiff[fpSalingW]);
  ML.Add(ParamValueStringDiff[fpMastfallVorlauf]);
  ML.Add('');
  ML.Add(ParamValueStringDiff[fpMastfallF0F]);
  ML.Add(ParamValueStringDiff[fpMastfallF0C]);
  ML.Add(ParamValueStringDiff[fpBiegung]);
  ML.Add(''); //FormatValue(BiegungGFD));
end;

procedure TRggMain.AL(A: string; fp: TFederParam);
var
  B, C, D: string;
begin
  B := ':';
  C := ParamValueString[fp];
  D := ParamValueStringDiff[fp];
  TML.Add(Format(tfs, [A, B, C, D]));
end;

procedure TRggMain.BL(A: string; C: string);
var
  B, D: string;
begin
  B := ':';
  D := '';
  TML.Add(Format(tfs, [A, B, C, D]));
end;

procedure TRggMain.UpdateDiffText(ML: TStrings);
begin
  DoBiegungGF;

  ML.Clear;
  TML := ML;

  AL('VA', fpVorstag);
  AL('WA', fpWante);
  AL('WO', fpWoben);
  AL('SH', fpSalingH);
  AL('SA', fpSalingA);
  AL('SL', fpSalingL);
  AL('SW', fpSalingW);
  AL('MV', fpMastfallVorlauf);

  BL('MF', Mastfall);
  AL('F0F', fpMastfallF0F);
  AL('F0C', fpMastfallF0C);
  AL('Bie', fpBiegung);
  BL('BGF', FormatValue(BiegungGF));
end;

procedure TRggMain.SetupTrackbarForRgg;
var
  temp: single;
  S: string;
  sb: TRggSB;
  tb: TFederTrackbar;
begin
  tb := RggTrackbar;
  temp := ParamValue[Param];
  sb := FactArray.Find(Param);
  tb.Min := -200; // kleiner als der kleinste Wert
  tb.Max := sb.Max;
  tb.ValueNoChange := temp;
  tb.Min := sb.Min;
  tb.Frequency := sb.TinyStep;
//  tb.LineSize := sb.TinyStep;
//  tb.PageSize := sb.BigStep;

  if Param = fpWinkel then
    S := 'Grad'
  else if Param = fpSalingW then
    S := 'Grad'
  else
    S := 'mm';
  MinValCaption := Format('%.0f %s', [sb.Min, S]);
  MaxValCaption := Format('%.0f %s', [sb.Max, S]);
  IstValCaption := Format('%.0f %s', [temp, S]);
  ParamCaption := Param2Text(Param);
end;

procedure TRggMain.RggSpecialDoOnTrackBarChange;
begin
  if FParam = fpWinkel then
    IstValCaption := Format('%.0f Grad', [RggTrackbar.Value])
  else if FParam = fpSalingW then
    IstValCaption := Format('%.0f Grad', [RggTrackbar.Value])
  else
    IstValCaption := Format('%.0f mm', [RggTrackbar.Value]);

  ParamValue[FParam] := Round(RggTrackbar.Value);
end;

procedure TRggMain.Draw;
begin
  // ...
  UpdateFactArrayFromRigg;
  UpdateText;
end;

procedure TRggMain.ToggleRenderOption(fa: TFederAction);
begin
  case fa of
    faWantRenderH: StrokeRigg.WantRenderH := not StrokeRigg.WantRenderH;
    faWantRenderP: StrokeRigg.WantRenderP := not StrokeRigg.WantRenderP;
    faWantRenderF: StrokeRigg.WantRenderF := not StrokeRigg.WantRenderF;
    faWantRenderE: StrokeRigg.WantRenderE := not StrokeRigg.WantRenderE;
    faWantRenderS: StrokeRigg.WantRenderS := not StrokeRigg.WantRenderS;
  end;
  Draw;
end;

procedure TRggMain.SetVisible(const Value: Boolean);
begin
  FVisible := Value;
end;

{ TRggMain0 }

constructor TRggMain0.Create;
begin
  FHullVisible := True;
  RggTrackbar := TFederTrackbar.Create;
end;

destructor TRggMain0.Destroy;
begin
  RggTrackbar.Free;
  inherited;
end;

procedure TRggMain0.SetHullVisible(const Value: Boolean);
begin
  FHullVisible := Value;
end;

procedure TRggMain0.ToggleHullVisible;
begin
  HullVisible := not HullVisible;
end;

procedure TRggMain0.TrackBarChange(Sender: TObject);
begin
  // ...
  RggSpecialDoOnTrackBarChange;
end;

procedure TRggMain0.RggSpecialDoOnTrackBarChange;
begin
end;

procedure TRggMain0.DoRasterWheel(Delta: single);
begin
end;

procedure TRggMain0.DoSmallWheel(Delta: single);
var
  f: single;
begin
  f := GetSmallStep;
  if Delta > 0 then
    DoWheel(f)
  else
    DoWheel(-f);
end;

procedure TRggMain0.DoBigWheel(Delta: single);
var
  f: single;
begin
  f := GetBigStep;
  if Delta > 0 then
    DoWheel(f)
  else
    DoWheel(-f);
end;

procedure TRggMain0.DoWheel(Delta: single);
begin
  RggTrackbar.Value := RggTrackbar.Value + Delta; // --> UpdateGraph;
end;

procedure TRggMain0.DoRotation(Delta: single);
begin
  // ...
  UpdateText;
end;

procedure TRggMain0.DoZoom(Delta: single);
begin
  // ...
  UpdateText;
end;

procedure TRggMain0.ViewportChanged(Sender: TObject);
begin
  if Main.IsUp then
    UpdateText;
end;

function TRggMain0.GetBigStep: single;
begin
  case FParam of
    fpWante: result := 5;
    fpWinkel: result := 1;
    fpSalingW: result := 1;
    else
      result := 10;
  end;
end;

function TRggMain0.GetSmallStep: single;
begin
  result := 1;
end;

procedure TRggMain0.UpdateText(ClearFlash: Boolean);
begin
//  if Main.IsUp and Assigned(Main.FederText) then
//  begin
//    if ClearFlash then
//      Main.FederText.ClearFlash;
//    Main.FederText.UpdateText;
//  end;
end;

procedure TRggMain0.HandleKey(KeyChar: Char);
//var
//  ah: TFederActionHandler;
//  fa: TFederAction;
//  Key: Word;
begin
//  fa := Main.Keyboard.KeyUpAction(Key, KeyChar, []);
//  Key := 0;
//  ah := Main.ActionHandler;

//  if fa <> faNoop then
//    ah.Execute(fa);

//  if KeyChar = 'v' then
//    ah.Execute(faVorstag)
//
//  else if KeyChar = 'w' then
//    ah.Execute(faWante)
//
//  else if KeyChar = 'o' then
//    ah.Execute(faWoben)
//
//  else if KeyChar = 'h' then
//    ah.Execute(faSalingH)
//
//  else if KeyChar = 'a' then
//    ah.Execute(faSalingA)
//
//  else if KeyChar = 'b' then
//    ah.Execute(faCycleBitmapP)
//  else if KeyChar = 'B' then
//    ah.Execute(faCycleBitmapM)
//
//  else if KeyChar = 'c' then
//    ah.Execute(faCycleColorSchemeP)
//  else if KeyChar = 'C' then
//    ah.Execute(faCycleColorSchemeM)
//
//  else if KeyChar = 'd' then
//    ah.Execute(faFixpointD)
//  else if KeyChar = 'D' then
//    ah.Execute(faFixpointD0)
//
//  else if KeyChar = 'e' then
//    ah.Execute(faFixpointE)
//  else if KeyChar = 'E' then
//    ah.Execute(faFixpointE0)
//
//  else if KeyChar = 'f' then
//    ah.Execute(faFixpointF)
//  else if KeyChar = 'F' then
//    ah.Execute(faFixpointF0)
//
//  else if KeyChar = 'H' then
//    ah.Execute(faHull)
//
//  else if KeyChar = 'p' then
//    ah.Execute(faResetPosition)
//
//  else if KeyChar = 'R' then
//    ah.Execute(faReadTrimmFile)
//
//  else if KeyChar = 'M' then
//    ah.Execute(faUpdateTrimm0)
//
//  else if KeyChar = 'n' then
//    ah.Execute(faRandomBlack)
//  else if KeyChar = 'N' then
//    ah.Execute(faRandomWhite)
//
//  else if KeyChar = 't' then
//    ah.Execute(faParamT1)
//  else if KeyChar = 'T' then
//    ah.Execute(faParamT2)
//
//  else if KeyChar = 'u' then
//    ah.Execute(faToggleDataText)
//  else if KeyChar = 'U' then
//    ah.Execute(faToggleDiffText)
//
//  else if KeyChar = 'z' then
//    ah.Execute(faResetZoom)
//  else if KeyChar = 'Z' then
//    ah.Execute(faUpdateTrimm0)
//
//  else if KeyChar = '0' then
//    ah.Execute(faTrimm0)
//  else if KeyChar = '1' then
//    ah.Execute(faTrimm1)
//  else if KeyChar = '2' then
//    ah.Execute(faTrimm2)
//  else if KeyChar = '3' then
//    ah.Execute(faTrimm3)
//  else if KeyChar = '4' then
//    ah.Execute(faTrimm4)
//  else if KeyChar = '5' then
//    ah.Execute(faTrimm5)
//  else if KeyChar = '6' then
//    ah.Execute(faTrimm6)
//  else if KeyChar = '7' then
//    ah.Execute(fa420)
//  else if KeyChar = '8' then
//    ah.Execute(faLogo)
//
//  else if KeyChar = '!' then
//    ah.Execute(faParamT1)
//  else if KeyChar = '"' then
//    ah.Execute(faParamT2)
//
//  else if KeyChar = '=' then
//    ah.Execute(faActionPageE)
//  else if KeyChar = '?' then
//    //ah.Execute(faActionPageX)
//  else if KeyChar = '*' then
//    ah.Execute(faActionPageM)
//  else if KeyChar = '+' then
//    ah.Execute(faActionPageP)
//  else if KeyChar = '#' then
//    ah.Execute(faBitmapEscape)
//
//  ;
end;

procedure TRggMain0.TestStream;
var
  d: TRggDocument;
begin
  d := TRggDocument.Create;
  d.GetDefaultDocument;
  d.TestStream;
  d.Free;
end;

function TRggMain.FormatValue(Value: single): string;
begin
  result := Format('%.0f', [Value]);
end;

procedure TRggMain.DoBiegungGF;
var
  a, b, c, k, h: double;
  pc, pf: TRealPoint;
  kg, kh, kc: TRealPoint;
  IndexG, IndexH, IndexC: Integer;
begin
  if StrokeRigg <> nil then
  begin
    pc := Rigg.rP[ooC];
    pf := Rigg.rP[ooF];

    IndexG := 13;
    IndexH := 40;
    IndexC := 50;

    kg := StrokeRigg.Kurve[IndexG];
    kh := StrokeRigg.Kurve[IndexH];
    kc := StrokeRigg.Kurve[IndexC];

    a := Abstand(kg, pf);
    b := Abstand(pf, kh);
    c := Abstand(kh, kg);

    h := Hoehe(a-0.00001, b, c, k);

    BiegungGFD := BiegungGF-h;
    BiegungGF := h;
  end
  else
  begin
    BiegungGFD := 0;
    BiegungGF := 0;
  end;
end;

procedure TRggMain.DebugBiegungGF(ML: TStrings);
var
  a, b, c, k, h: double;
  pd, pc, pf: TRealPoint;
  kd, kg, kh, kc: TRealPoint;
  bm, l: double;
  t: double;
  IndexG, IndexD, IndexH, IndexC: double;
begin
  ML.Clear;
  if StrokeRigg <> nil then
  begin
    pd := Rigg.rP[ooD];
    pc := Rigg.rP[ooC];
    pf := Rigg.rP[ooF];

    bm := BogenMax;
    l := Rigg.rL[15] + Rigg.rL[16];

    ML.Add('');
    t := 1160;
    ML.Add('G = 1160');
    IndexG := bm * t / l;
    ML.Add(Format('iG = %.2f', [IndexG]));
    ML.Add('IndexG := 13;');

    ML.Add('');
    t := Rigg.rL[16];
    ML.Add(Format('D = Rigg.rL[16] = %.2f', [t]));
    IndexD := bm * t / l;
    ML.Add(Format('iD = %.2f', [IndexD]));
    ML.Add('IndexD := 28;');

    ML.Add('');
    ML.Add('H = 1160 + (4900 / 2)');
    t := 1160 + (4900 / 2);
    ML.Add(Format('H = %.2f', [t]));
    IndexH := bm * t / l;
    ML.Add(Format('iH = %.2f', [IndexH]));
    ML.Add('IndexH := 40;');

    ML.Add('');
    t := l;
    ML.Add(Format('C = D0D + DC = %.2f', [t]));
    IndexC := bm * t / l;
    ML.Add(Format('iC = %.2f', [IndexC]));
    ML.Add('IndexC := 50;');

    kg := StrokeRigg.Kurve[Round(IndexG)];
    kd := StrokeRigg.Kurve[Round(IndexD)];
    kh := StrokeRigg.Kurve[Round(IndexH)];
    kc := StrokeRigg.Kurve[Round(IndexC)];

//    ML.Add('');
//    t := Abstand(pd, kd);
//    ML.Add(Format('ooD D = %.2f', [t]));
//    t := Abstand(kg, pf);
//    ML.Add(Format('G ooF = %.2f', [t]));
//    t := Abstand(Rigg.rP[ooD0], kg);
//    ML.Add(Format('ooD0 D = %.2f', [t]));
//    t := Abstand(pd, kh);
//    ML.Add(Format('ooD H = %.2f', [t]));

    a := Abstand(kg, pf);
    b := Abstand(pf, kh);
    c := Abstand(kh, kg);

    ML.Add('');
    h := Hoehe(a-0.00001, b, c, k);
    ML.Add('Triangle abc = (GF, FH, HG)');
    ML.Add(Format('(%.2f, %.2f, %.2f)', [a, b, c]));
    ML.Add(Format('Hoehe BGF at H = %.2f', [h]));
  end;
end;

procedure TRggMain.UpdateTrimmText(ML: TStrings);
begin
  ML.Clear;
  ML.Add('Trimm = ' + IntToStr(Main.Trimm));
  ML.Add('Name = ' + Main.CurrentTrimm.Name);
  if Main.RggMain.Action = faPan then
  begin
    ML.Add('Param = Pan');
    ML.Add('Min = ');
    ML.Add('Pos = ');
    ML.Add('Max = ');
  end
  else
  begin
    ML.Add('Param = ' + Main.RggMain.ParamCaption);
    ML.Add('Min = ' + Main.RggMain.MinValCaption);
    ML.Add('Pos = ' + Main.RggMain.IstValCaption);
    ML.Add('Max = ' + Main.RggMain.MaxValCaption);
  end;

  if Main.RggMain.Demo then
    ML.Add('Modus = Demo')
  else
    ML.Add('Modus = Pro');

  ML.Add('CounterG = ' + IntToStr(Rigg.UpdateGetriebeCounter));
  ML.Add('CounterT = ' + IntToStr(UpdateTextCounter));
end;

procedure TRggMain.UpdateDataText(ML: TStrings);
begin
  ML.Text := Main.TrimmData;
end;

procedure TRggMain.UpdateGetriebe;
begin
  RiggModul.UpdateGetriebe;
end;

end.
