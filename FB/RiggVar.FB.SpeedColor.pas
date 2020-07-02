unit RiggVar.FB.SpeedColor;

interface

{$ifdef fpc}
{$mode delphi}
{$endif}

uses
  RiggVar.FB.Color;

type
  TSpeedColorValue = (
    clvScheme,

    clvBack,
    clvHot,
    clvLog,
    clvReport,
    clvOption,
    clvProp,
    clvData,
    clvWheel,
    clvTrimm,

    clvHintText,
    clvTrimmText,
    clvReportText,
    clvHelpText,
    clvParamList,
    clvReportList,

    clvGraph,
    clvSegment,
    clvBogen,
    clvImage,
    clvMemory,
    clvRigg,
    clvView,
    clvZoom
  );

  TSpeedColorScheme = record
    claScheme: TRggColor;

    claBack: TRggColor;
    claHot: TRggColor;
    claLog: TRggColor;
    claReport: TRggColor;
    claOption: TRggColor;
    claProp: TRggColor;
    claData: TRggColor;
    claWheel: TRggColor;
    claTrimm: TRggColor;

    claGraph: TRggColor;
    claSegment: TRggColor;
    claBogen: TRggColor;
    claImage: TRggColor;
    claMemory: TRggColor;
    claRigg: TRggColor;
    claView: TRggColor;
    claZoom: TRggColor;

    claHintText: TRggColor;
    claTrimmText: TRggColor;
    claReportText: TRggColor;
    claHelpText: TRggColor;
    claParamList: TRggColor;
    claReportList: TRggColor;

    claList1: TRggColor;
    claList2: TRggColor;
    claList3: TRggColor;

    claMemo: TRggColor;

    IsDark: Boolean;
    procedure Init(WantDark: Boolean);
    procedure InitDark;
    procedure InitLight;
    function GetColor(Value: TSpeedColorValue): TRggColor;
  end;

implementation

{ TSpeedColorScheme }

function TSpeedColorScheme.GetColor(Value: TSpeedColorValue): TRggColor;
begin
  case Value of
    clvBack: result := claBack;
    clvHot: result := claHot;

    clvScheme: result := claScheme;

    clvLog: result := claLog;
    clvReport: result := claReport;
    clvOption: result := claOption;
    clvProp: result := claProp;
    clvData: result := claData;
    clvWheel: result := claWheel;
    clvTrimm: result := claTrimm;

    clvGraph: result := claGraph;
    clvSegment: result := claSegment;
    clvBogen: result := claBogen;
    clvImage: result := claImage;
    clvMemory: result := claMemory;
    clvRigg: result := claRigg;
    clvView: result := claView;
    clvZoom: result := claZoom;

    clvHintText: result := claHintText;
    clvTrimmText: result := claTrimmText;
    clvReportText: result := claReportText;
    clvHelpText: result := claHelpText;
    clvParamList: result := claParamList;
    clvReportList: result := claReportList;

    else
      result := TRggColors.Red;
  end;
end;

procedure TSpeedColorScheme.Init(WantDark: Boolean);
begin
  if WantDark then
    InitDark
  else
    InitLight;
end;

procedure TSpeedColorScheme.InitDark;
begin
  IsDark := True;

  claBack := TRggColors.SlateGray;
  claHot := TRggColors.Beige;

  claScheme := TRggColors.Orange;

  claLog := TRggColors.Orange;
  claReport := TRggColors.Orange;
  claOption := TRggColors.OrangeRed;
  claProp := TRggColors.Goldenrod;
  claData := TRggColors.White;
  claWheel := TRggColors.DodgerBlue;
  claTrimm := TRggColors.Yellow;

  claGraph := TRggColors.Coral;
  claSegment := TRggColors.Crimson;
  claBogen := TRggColors.Dodgerblue;
  claImage := TRggColors.Goldenrod;
  claMemory := TRggColors.White;
  claRigg := TRggColors.Slateblue;
  claView := TRggColors.Beige;
  claZoom := TRggColors.Teal;

  claHintText := TRggColors.Yellow;
  claTrimmText := TRggColors.Plum;
  claReportText := TRggColors.AntiqueWhite;
  claHelpText := TRggColors.White;

  claParamList := TRggColors.Aqua;
  claReportList := TRggColors.Aquamarine;

  claList1 := TRggColors.Gold;
  claList2 := TRggColors.Coral;
  claList3 := TRggColors.Antiquewhite;

  claMemo := TRggColors.Dodgerblue;
end;

procedure TSpeedColorScheme.InitLight;
begin
  IsDark := False;

  claScheme := TRggColors.Orange;

  claBack := TRggColors.WindowWhite;

  claHot := TRggColors.Black;
  claLog := TRggColors.Orange;
  claReport := TRggColors.Burlywood;
  claOption := TRggColors.OrangeRed;
  claProp := TRggColors.Goldenrod;
  claData := TRggColors.Green;
  claWheel := TRggColors.Coral;
  claTrimm := TRggColors.Dodgerblue;

  claGraph := TRggColors.Coral;
  claSegment := TRggColors.Crimson;
  claBogen := TRggColors.Dodgerblue;
  claImage := TRggColors.Goldenrod;
  claMemory := TRggColors.Slategray;
  claRigg := TRggColors.Slateblue;
  claView := TRggColors.Burlywood;
  claZoom := TRggColors.Purple;

  claHintText := TRggColors.Darkorange;
  claTrimmText := TRggColors.Dodgerblue;
  claReportText := TRggColors.Black;
  claHelpText := TRggColors.Blue;

  claParamList := TRggColors.Orange;
  claReportList := TRggColors.Slateblue;

  claList1 := TRggColors.Slateblue;
  claList2 := TRggColors.Slategray;
  claList3 := TRggColors.Orangered;

  claMemo := TRggColors.Dodgerblue;
end;

end.
