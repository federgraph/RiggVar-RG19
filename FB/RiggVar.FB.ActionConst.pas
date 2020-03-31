unit RiggVar.FB.ActionConst;

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

type
  TFederAction = Integer;

const

  faController = 1;
  faWinkel = 2;
  faVorstag = 3;
  faWante = 4;
  faWoben = 5;
  faSalingH = 6;
  faSalingA = 7;
  faSalingL = 8;
  faVorstagOS = 9;
  faWPowerOS = 10;
  faSalingW = 11;
  faMastfallF0C = 12;
  faMastfallF0F = 13;
  faMastfallVorlauf = 14;
  faBiegung= 15;
  faMastfussD0X = 16;
  faParamT1 = 17;
  faParamT2 = 18;

  faSalingTypOhne = 19;
  faSalingTypDrehbar = 20;
  faSalingTypFest = 21;

  faFixpointA0 = 22;
  faFixpointA = 23;
  faFixpointB0 = 24;
  faFixpointB = 25;
  faFixpointC0 = 26;
  faFixpointC = 27;
  faFixpointD0 = 28;
  faFixpointD = 29;
  faFixpointE0 = 30;
  faFixpointE = 31;
  faFixpointF0 = 32;
  faFixpointF = 33;

  faViewpointS = 34;
  faViewpointA = 35;
  faViewpointT = 36;
  faViewpoint3 = 37;

  faWantRenderH = 38;
  faWantRenderP = 39;
  faWantRenderF = 40;
  faWantRenderE = 41;
  faWantRenderS = 42;

  faHull = 43;
  faDemo = 44;

  faParamValueMinus1 = 45;
  faParamValuePlus1 = 46;
  faParamValuePlus10 = 47;
  faParamValueMinus10 = 48;

  faWheelLeft = 49;
  faWheelRight = 50;
  faWheelUp = 51;
  faWheelDown = 52;

  faTrimm0 = 53;
  faTrimm1 = 54;
  faTrimm2 = 55;
  faTrimm3 = 56;
  faTrimm4 = 57;
  faTrimm5 = 58;
  faTrimm6 = 59;
  fa420 = 60;
  faLogo = 61;

  faUpdateTrimm0 = 62;
  faCopyAndPaste = 63;
  faCopyTrimmItem = 64;
  faPasteTrimmItem = 65;

  faReadTrimmFile = 66;
  faCopyTrimmFile = 67;
  faSaveTrimmFile = 68;

  faToggleTrimmText = 69;
  faToggleDiffText = 70;
  faToggleDataText = 71;

  faToggleViewType = 72;
  faPan = 73;

  faUpdateReportText = 74;
  faToggleDebugText = 75;

  faSalingTypOhneStarr = 76;

  //Pages
  faActionPageM = 103;
  faActionPageP = 104;

  //ViewFlags
  faToggleAllText = 323;
  faToggleTouchFrame = 324;

  //ColorScheme
  faCycleColorSchemeM = 653;
  faCycleColorSchemeP = 654;

  //TouchLayout
  faTouchTablet = 662;
  faTouchPhone = 663;
  faTouchDesk = 664;

  //EmptyAction
  faNoop = 871;

  faButtonFrameReport = 1070;

  //MemeBuilder
  faMemeNoop = 1071;
  faMemeToggleHelp = 1086;
  faMemeToggleReport = 1087;
  faMemeGotoLandscape = 1100;
  faMemeGotoSquare = 1101;
  faMemeGotoPortrait = 1102;
  faMemeFormat0 = 1103;
  faMemeFormat1 = 1104;
  faMemeFormat2 = 1105;
  faMemeFormat3 = 1106;
  faMemeFormat4 = 1107;
  faMemeFormat5 = 1108;
  faMemeFormat6 = 1109;
  faMemeFormat7 = 1110;
  faMemeFormat8 = 1111;
  faMemeFormat9 = 1112;

  // new Actions for Rgg Chart
  faChartRect = 2200;
  faChartTextRect = 2201;
  faChartLegend = 2202;
  faChartAP = 2203;
  faChartBP = 2204;
  faChartGroup = 2205;

  faParamCountPlus = 2206;
  faParamCountMinus = 2207;

  faPComboPlus = 2208;
  faPComboMinus = 2209;

  faXComboPlus = 2210;
  faXComboMinus = 2211;

  faYComboPlus = 2212;
  faYComboMinus = 2213;

  faChartReset = 2214;

  faToggleFontColor = 3000;

  faReportNone = 3001;
  faReportLog = 3002;
  faReportJson = 3003;
  faReportData = 3004;
  faReportTrimmText = 3005;
  faReportDataText = 3006;
  faReportDiffText = 3007;
  faReportAusgabeRL = 3008;
  faReportAusgabeRP = 3009;
  faReportAusgabeRLE = 3010;
  faReportAusgabeRPE = 3011;
  faReportAusgabeDiffL = 3012;
  faReportAusgabeDiffP = 3013;
  faReportXML = 3014;
  faReportDebugReport = 3015;
  faReportReadme = 3016;

implementation

end.
