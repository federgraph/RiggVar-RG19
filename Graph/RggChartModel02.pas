unit RggChartModel02;

interface

uses
  SysUtils,
  Classes,
  Graphics,
  RggChart,
  RggChartModel01,
  RggTypes;

{.$define RG19}

type
  TRggChartModel02 = class(TRggChartModel01)
//  private
//    FLegend: Boolean;
  public
    function CheckBeforeCalc: Boolean; override;

    procedure TakeOver;
    procedure UpdateRiggItemClick;

    procedure OpenItemClick(AFileName: string);
    procedure SaveItemClick(AFileName: string);

//    procedure LoadFromFile(FileName: string);
//    procedure SaveToFile(FileName: string);

//    procedure LoadFromStream(S: TStream);
//    procedure SaveToStream(S: TStream);
  end;

implementation

uses
  FrmChart;

{ TRggChartModel02}

procedure TRggChartModel02.OpenItemClick(AFileName: string);
begin
//  LoadFromFile(AFileName);
//  Exclude(FStatus, csBerechnet);
//  Include(FStatus, csGeladen);
//  XAchseText := GetXText(XComboText); { ben�tigt f�r BottomTitel }
//  ParamText := GetPText(PComboText); { ben�tigt f�r RightTitel }
//  XLEDFillColor := clRed;
//  PLEDFillColor := clRed;
//  PSpinnerValue := 1;
//  PSpinnerMax := ParamCount;
//  if PSpinnerMax = 1 then
//    PSpinnerMax := 2;
//  KurvenZahlSpinnerValue := ParamCount;
//  RebuildYCombo;
//  YComboChange(nil);
end;

procedure TRggChartModel02.SaveItemClick(AFileName: string);
begin
//  SaveToFile(AFileName);
end;

//procedure TRggChartModel02.LoadFromFile(FileName: string);
//var
//  S: TFileStream;
//begin
//  S := TFileStream.Create(FileName, fmOpenRead);
//  try
//    LoadFromStream(S);
//  finally
//    S.Free;
//  end;
//end;

//procedure TRggChartModel02.SaveToFile(FileName: string);
//var
//  S: TFileStream;
//begin
//  S := TFileStream.Create(FileName, fmCreate);
//  try
//    SaveToStream(S);
//  finally
//    S.Free;
//  end;
//end;

//procedure TRggChartModel02.SaveToStream(S: TStream);
//var
//  ParamValue: double;
//  param: Integer;
//begin
//  with S do
//  begin
//    WriteBuffer(FLegend, SizeOf(Boolean));
//    WriteBuffer(XAchseMin, SizeOf(Integer));
//    WriteBuffer(XAchseMax, SizeOf(Integer));
//    WriteBuffer(ParamCount, SizeOf(Integer));
//    WriteBuffer(YAchseSet, SizeOf(YAchseSet));
//    WriteBuffer(YAchseRecordList, SizeOf(YAchseRecordList));
//    for param := 0 to PNr - 1 do
//      WriteBuffer(af[param], SizeOf(TYLineArray));
//    for param := 0 to ParamCount - 1 do
//    begin
//      ParamValue := StrToFloat(PText[param]);
//      WriteBuffer(ParamValue, SizeOf(double));
//    end;
//    RggDocument.SaveToStream(S);
//    MemoLines.Add(XComboText);
//    MemoLines.Add(PComboText);
//    MemoLines.SaveToStream(S);
//  end;
//end;

//procedure TRggChartModel02.LoadFromStream(S: TStream);
//var
//  ParamValue: double;
//  param: Integer;
//begin
//  with S do
//  begin
//    ReadBuffer(FLegend, SizeOf(Boolean));
//    ReadBuffer(XAchseMin, SizeOf(Integer));
//    ReadBuffer(XAchseMax, SizeOf(Integer));
//    ReadBuffer(ParamCount, SizeOf(Integer));
//    ReadBuffer(YAchseSet, SizeOf(YAchseSet));
//    ReadBuffer(YAchseRecordList, SizeOf(YAchseRecordList));
//    for param := 0 to PNr - 1 do
//      ReadBuffer(af[param], SizeOf(TYLineArray));
//    for param := 0 to ParamCount - 1 do
//    begin
//      ReadBuffer(ParamValue, SizeOf(double));
//      PText[param] := Format('%6.2f', [ParamValue]);
//    end;
//    RggDocument.LoadFromStream(S);
//    MemoLines.LoadFromStream(S);
//    XComboText := MemoLines[MemoLines.Count-2];
//    PComboText := MemoLines[MemoLines.Count-1];
//    MemoLines.Delete(MemoLines.Count-1);
//    MemoLines.Delete(MemoLines.Count-1);
//  end;
//end;

function  TRggChartModel02.CheckBeforeCalc: Boolean;
begin
  result := ChartForm.CheckBeforeCalc;
end;

procedure TRggChartModel02.TakeOver;
begin
  Rigg.UpdateGSB;
  SalingTyp := Rigg.SalingTyp;
  { ControllerTyp := Rigg.ControllerTyp; }
  { CalcTyp := Rigg.CalcTyp; }
end;

procedure TRggChartModel02.UpdateRiggItemClick;
begin
{$ifdef RG19}
  if not Assigned(RggDocument) then
    Exit;
  if (csGeladen in FStatus) or (csBerechnet in FStatus)then
  begin
    RiggModul.Neu(RggDocument);
    RiggModul.ViewModelMain.Caption := 'Rigg';
  end;
{$endif}
end;

end.
