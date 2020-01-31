unit RiggVar.RG.Def;

interface

uses
  Vcl.Graphics;

type
  TFederParam = (
    fpController,
    fpWinkel,
    fpVorstag,
    fpWante,
    fpWoben,
    fpSalingH,
    fpSalingA,
    fpSalingL,
    fpVorstagOS,
    fpWPowerOS,
    fpSalingW,
    fpMastfallF0C,
    fpMastfallF0F,
    fpMastfallVorlauf,
    fpBiegung,
    fpD0X,
    fpT1,
    fpT2
    );

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

  cFaktor = 'Faktor';
  cName = 'Name';

  cA0X = 'A0X';
  cA0Y = 'A0Y';
  cA0Z = 'A0Z';

  cC0X = 'C0X';
  cC0Y = 'C0Y';
  cC0Z = 'C0Z';

  cD0X = 'D0X';
  cD0Y = 'D0Y';
  cD0Z = 'D0Z';

  cE0X = 'E0X';
  cE0Y = 'E0Y';
  cE0Z = 'E0Z';

  cF0X = 'F0X';
  cF0Y = 'F0Y';
  cF0Z = 'F0Z';

  cMU = 'MU';
  cMO = 'MO';
  cML = 'ML';
  cMV = 'MV';
  cCA = 'CA';

  cCPMin = 'CPMin';
  cCPPos = 'CPPos';
  cCPMax = 'CPMax';

  cSHMin = 'SHMin';
  cSHPos = 'SHPos';
  cSHMax = 'SHMax';

  cSAMin = 'SAMin';
  cSAPos = 'SAPos';
  cSAMax = 'SAMax';

  cSLMin = 'SLMin';
  cSLPos = 'SLPos';
  cSLMax = 'SLMax';

  cSWMin = 'SWMin';
  cSWPos = 'SWPos';
  cSWMax = 'SWMax';

  cVOMin = 'VOMin';
  cVOPos = 'VOPos';
  cVOMax = 'VOMax';

  cWIMin = 'WIMin';
  cWIPos = 'WIPos';
  cWIMax = 'WIMax';

  cWLMin = 'WLMin';
  cWLPos = 'WLPos';
  cWLMax = 'WLMax';

  cWOMin = 'WOMin';
  cWOPos = 'WOPos';
  cWOMax = 'WOMax';

  cCP = 'cp';
  cSH = 'sh';
  cSA = 'sa';
//  cSL = 'sl';
//  cSW = 'sw';
  cVO = 'vo';
  cWI = 'wi';
  cWL = 'wl';
  cWO = 'wo';

// gespeicherte Basiswerte
  ch0 = 'h0';
  cl2 = 'l2'; //schon in FC
  ch2 = 'h2';

  //nicht gespeichert
  ch1 = 'h1';
  ch3 = 'h3';
  cl3 = 'l3'; //schon in FC
  cw3 = 'w3';

  //Delphi code format strings
  dsg = '%s := %g;';
  dsd = '%s := %d;';
  dss = '%s := %s;';
  dsf = '%s := %2.2f;';

  dbg = '%d := %g;';
  dbd = '%d := %d;';
  dbs = '%d := %s;';
  dbf = '%d := %2.2f;';

  //Java code format strings
  jsg = '%s = %g;';
  jsd = '%s = %d;';
  jss = '%s = %s;';
  jsf = '%s = %2.2f;';

  jbg = '%d = %g;';
  jbd = '%d = %d;';
  jbs = '%d = %s;';
  jbf = '%d = %2.2f;';

  //normal properties file format strings
  fsg = '%s=%g';
  fsd = '%s=%d';
  fss = '%s=%s';
  fsf = '%s=%2.2f';

  fbg = '%d=%g';
  fbd = '%d=%d';
  fbs = '%d=%s';
  fbf = '%d=%2.2f';

  cVersion = 'version';
  nVersion=0;

  cOffsetX = 'OffsetX';
  cOffsetY = 'OffsetY';
  cOffsetZ = 'OffsetZ';
  nOffsetX = 27;
  nOffsetY = 28;
  nOffsetZ = 29;

  clRumpf: TColor = clBlack;
  clMast: TColor = clBlue;
  clWanten: TColor = clRed;
  clVorstag: TColor = clYellow;
  clSaling: TColor = clLime;
  clController: TColor = clAqua;
  clEntspannt: TColor = clGray;
  clNullStellung: TColor = clAqua;
  clKoppelKurve: TColor = clYellow;
  clGestrichelt: TColor = clWhite;
  clFixPont: TColor = clYellow;

implementation

end.
