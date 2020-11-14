unit RiggVar.FB.ActionGroup;

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
  RiggVar.FB.ActionConst;

type
  TActionGroup = array of Integer;

var
  ActionGroupEmptyAction: TActionGroup;
  ActionGroupTouchLayout: TActionGroup;
  ActionGroupPages: TActionGroup;
  ActionGroupColorScheme: TActionGroup;
  ActionGroupWheel: TActionGroup;
  ActionGroupForms: TActionGroup;
  ActionGroupViewParams: TActionGroup;
  ActionGroupFederText: TActionGroup;
  ActionGroupRggControls: TActionGroup;
  ActionGroupRggFixPoints: TActionGroup;
  ActionGroupRggTrimms: TActionGroup;
  ActionGroupRggSalingType: TActionGroup;
  ActionGroupRggCalcType: TActionGroup;
  ActionGroupRggAppMode: TActionGroup;
  ActionGroupRggSuper: TActionGroup;
  ActionGroupRggTrimmFile: TActionGroup;
  ActionGroupRggTrimmText: TActionGroup;
  ActionGroupRggViewPoint: TActionGroup;
  ActionGroupRggRenderOptions: TActionGroup;
  ActionGroupRggChart: TActionGroup;
  ActionGroupRggReport: TActionGroup;
  ActionGroupRggSegment: TActionGroup;
  ActionGroupRggGraph: TActionGroup;
  ActionGroupMemeFormat: TActionGroup;
  ActionGroupRggSonstiges: TActionGroup;
  ActionGroupBtnLegendTablet: TActionGroup;
  ActionGroupBtnLegendPhone: TActionGroup;
  ActionGroupCircles: TActionGroup;
  ActionGroupParamT: TActionGroup;

  ActionGroupViewOptions: TActionGroup;
  ActionGroupViewType: TActionGroup;
  ActionGroupRggHullMesh: TActionGroup;
  ActionGroupEmptyLastLine: TActionGroup;
  ActionGroupHelp: TActionGroup;
  ActionGroupCopyPaste: TActionGroup;
  ActionGroupCopyImage: TActionGroup;
  ActionGroupTextureImport: TActionGroup;
  ActionGroupInput: TActionGroup;
  ActionGroupBitmapCycle: TActionGroup;
  ActionGroupViewFlags: TActionGroup;
  ActionGroupFormat: TActionGroup;
  ActionGroupIconSize: TActionGroup;
  ActionGroupReset: TActionGroup;
  ActionGroupCopyOptions: TActionGroup;

  ActionGroupRggInfo: TActionGroup;
  ActionGroupDropTarget: TActionGroup;
  ActionGroupLanguage: TActionGroup;
  ActionGroupHullMesh: TActionGroup;

implementation

uses
 SysUtils,
 Classes;

function IntegerArray(const Values: array of Integer): TActionGroup;
var
  l: Integer;
  i: Integer;
begin
  l := Length(Values);
  SetLength(Result, l);
  for i := 0 to high(Result) do
    Result[i] := Values[i];
end;

initialization

ActionGroupEmptyAction := IntegerArray([
faNoop]);

ActionGroupTouchLayout := IntegerArray([
faTouchTablet,
faTouchPhone,
faTouchDesk]);

ActionGroupPages := IntegerArray([
faActionPageM,
faActionPageP,
faActionPageE,
faActionPageS,
faActionPageX,
faActionPage1,
faActionPage2,
faActionPage3,
faActionPage4,
faActionPage5,
faActionPage6]);

ActionGroupColorScheme := IntegerArray([
faCycleColorSchemeM,
faCycleColorSchemeP]);

ActionGroupWheel := IntegerArray([
faPlusOne,
faPlusTen,
faWheelLeft,
faWheelRight,
faWheelDown,
faWheelUp,
faParamValuePlus1,
faParamValueMinus1,
faParamValuePlus10,
faParamValueMinus10]);

ActionGroupForms := IntegerArray([
  faRotaForm1,
  faRotaForm2,
  faRotaForm3,
  faShowMemo,
faShowMemo,
faShowActions,
faShowOptions,
faShowDrawings,
faShowConfig,
faShowKreis,
faShowInfo,
faShowSplash,
faShowForce,
faShowTabelle,
faShowDetail,
faShowSaling,
faShowController,
faShowText,
faShowTrimmTab,
faShowChart,
faShowDiagA,
faShowDiagC,
faShowDiagE,
faShowDiagQ]);

ActionGroupViewParams := IntegerArray([
faPan,
faParamORX,
faParamORY,
faParamORZ,
faParamRX,
faParamRY,
faParamRZ,
faParamCZ]);

ActionGroupFederText := IntegerArray([
faToggleAllText,
faToggleTouchFrame]);

ActionGroupRggControls := IntegerArray([
faController,
faWinkel,
faVorstag,
faWante,
faWoben,
faSalingH,
faSalingA,
faSalingL,
faSalingW,
faMastfallF0C,
faMastfallF0F,
faMastfallVorlauf,
faBiegung,
faMastfussD0X,
faVorstagOS,
faWPowerOS,
faParamAPW,
faParamEAH,
faParamEAR,
faParamEI]);

ActionGroupRggFixPoints := IntegerArray([
faFixpointA0,
faFixpointA,
faFixpointB0,
faFixpointB,
faFixpointC0,
faFixpointC,
faFixpointD0,
faFixpointD,
faFixpointE0,
faFixpointE,
faFixpointF0,
faFixpointF]);

ActionGroupRggTrimms := IntegerArray([
faTrimm0,
faTrimm1,
faTrimm2,
faTrimm3,
faTrimm4,
faTrimm5,
faTrimm6,
fa420,
faLogo]);

ActionGroupRggSalingType := IntegerArray([
faSalingTypOhne,
faSalingTypDrehbar,
faSalingTypFest,
faSalingTypOhneStarr]);

ActionGroupRggCalcType := IntegerArray([
faCalcTypQuer,
faCalcTypKnick,
faCalcTypGemessen]);

ActionGroupRggAppMode := IntegerArray([
faDemo,
faMemoryBtn,
faMemoryRecallBtn,
faKorrigiertItem,
faSofortBtn,
faGrauBtn,
faBlauBtn,
faMultiBtn]);

ActionGroupRggSuper := IntegerArray([
faSuperSimple,
faSuperNormal,
faSuperGrau,
faSuperBlau,
faSuperMulti,
faSuperDisplay,
faSuperQuick]);

ActionGroupRggTrimmFile := IntegerArray([
faCopyTrimmItem,
faPasteTrimmItem,
faCopyAndPaste,
faUpdateTrimm0,
faReadTrimmFile,
faSaveTrimmFile,
faCopyTrimmFile]);

ActionGroupRggTrimmText := IntegerArray([
faToggleTrimmText,
faToggleDiffText,
faToggleDataText,
faToggleDebugText,
faUpdateReportText]);

ActionGroupRggViewPoint := IntegerArray([
faViewpointS,
faViewpointA,
faViewpointT,
faViewpoint3]);

ActionGroupRggRenderOptions := IntegerArray([
faWantRenderH,
faWantRenderP,
faWantRenderF,
faWantRenderE,
faWantRenderS]);

ActionGroupRggChart := IntegerArray([
  faChartRect,
  faChartTextRect,
  faChartLegend,
  faChartAP,
  faChartBP,
  faChartGroup,

  faParamCountPlus,
  faParamCountMinus,

  faPComboPlus,
  faPComboMinus,

  faXComboPlus,
  faXComboMinus,

  faYComboPlus,
  faYComboMinus,

  faChartReset]);

ActionGroupRggReport := IntegerArray([
  faReportNone,
  faReportLog,
  faReportJson,
  faReportData,
  faReportShort,
  faReportLong,
  faReportTrimmText,
  faReportJsonText,
  faReportDataText,
  faReportDiffText,
  faReportAusgabeDetail,
  faReportAusgabeRL,
  faReportAusgabeRP,
  faReportAusgabeRLE,
  faReportAusgabeRPE,
  faReportAusgabeDiffL,
  faReportAusgabeDiffP,
  faReportXML,
  faReportDebugReport,
  faReportReadme]);

ActionGroupRggSegment := IntegerArray([
  faToggleSegmentF,
  faToggleSegmentR,
  faToggleSegmentS,
  faToggleSegmentM,
  faToggleSegmentV,
  faToggleSegmentW,
  faToggleSegmentC,
  faToggleSegmentA]);

ActionGroupRggGraph := IntegerArray([
  faToggleLineColor,
  faToggleUseDisplayList,
  faToggleUseQuickSort,
  faToggleShowLegend,

  faRggBogen,
  faRggKoppel,
  faRggHull,

  faRggZoomIn,
  faRggZoomOut,

  faToggleSalingGraph,
  faToggleControllerGraph,
  faToggleChartGraph,
  faToggleKraftGraph,

  faToggleMatrixText]);

ActionGroupMemeFormat := IntegerArray([
  faMemeGotoLandscape,
  faMemeGotoSquare,
  faMemeGotoPortrait,
  faMemeFormat0,
  faMemeFormat1,
  faMemeFormat2,
  faMemeFormat3,
  faMemeFormat4,
  faMemeFormat5,
  faMemeFormat6,
  faMemeFormat7,
  faMemeFormat8,
  faMemeFormat9]);

ActionGroupRggInfo := IntegerArray([
  faShowHelpText,
  faShowInfoText,
  faShowNormalKeyInfo,
  faShowSpecialKeyInfo,
  faShowDebugInfo,
  faShowZOrderInfo]);

ActionGroupRggSonstiges := IntegerArray([
  faToggleHelp,
  faToggleReport,
  faToggleButtonReport,
  faToggleFontColor,
  faToggleSandboxed,
  faToggleSpeedPanel,
  faToggleAllProps,
  faToggleAllTags]);

ActionGroupBtnLegendTablet := IntegerArray([
  faTL01,
  faTL02,
  faTL03,
  faTL04,
  faTL05,
  faTL06,

  faTR01,
  faTR02,
  faTR03,
  faTR04,
  faTR05,
  faTR06,
  faTR07,
  faTR08,

  faBL01,
  faBL02,
  faBL03,
  faBL04,
  faBL05,
  faBL06,
  faBL07,
  faBL08,

  faBR01,
  faBR02,
  faBR03,
  faBR04,
  faBR05,
  faBR06]);

ActionGroupBtnLegendPhone := IntegerArray([
  faMB01,
  faMB02,
  faMB03,
  faMB04,
  faMB05,
  faMB06,
  faMB07,
  faMB08]);

ActionGroupCircles := IntegerArray([
  faCirclesSelectC0,
  faCirclesSelectC1,
  faCirclesSelectC2,
  faCircleParamR1,
  faCircleParamR2,
  faCircleParamM1X,
  faCircleParamM1Y,
  faCircleParamM2X,
  faCircleParamM2Y,
  faLineParamA1,
  faLineParamA2,
  faLineParamE1,
  faLineParamE2,
  faCircleParamM1Z,
  faCircleParamM2Z,
  faCirclesReset]);

ActionGroupReset := IntegerArray([
  faReset,
  faResetPosition,
  faResetRotation,
  faResetZoom]);

ActionGroupDropTarget := IntegerArray([
  faToggleDropTarget]);

ActionGroupLanguage := IntegerArray([
  faToggleLanguage]);

ActionGroupCopyPaste := IntegerArray([
  faSave,
  faLoad,
  faOpen,
  faCopy,
  faPaste,
  faShare]);

ActionGroupViewOptions := IntegerArray([
  faToggleMoveMode,
  faLinearMove,
  faExpoMove]);

ActionGroupViewType := IntegerArray([
  faToggleViewType,
  faViewTypeOrtho,
  faViewTypePerspective]);

ActionGroupHullMesh := IntegerArray([
  faHullMesh,
  faHullMeshOn,
  faHullMeshOff]);

end.

