unit Rggunit4;

interface

uses
  System.SysUtils,
  System.Classes,
  RiggVar.RG.Def,
  RggTypes,
  Rggunit3,
  Rggdoc,
  Vcalc116;

type
  TRigg = class(TRiggFS)
  private
    function GetRealTrimm(Index: TTrimmIndex): double;
  public
    procedure WriteToDocFile(FileName: String);
    procedure LoadFromDocFile(FileName: String);
    procedure Assign(Source: TPersistent); override;
    procedure GetDocument(Doc: TRggDocument);
    procedure SetDocument(Doc: TRggDocument);
    procedure SetDefaultDocument;
    procedure GetRealTrimmRecord(var RealTrimm: TRealTrimm);
    property RealTrimm[Index: TTrimmIndex]: double read GetRealTrimm;
    property Trimm: TTrimm read FTrimm;
  end;

implementation

{ TRigg }

{
  procedure TRigg.WriteToIniFile(FileName: String);
  var
  IniFile: TIniFile;
  begin
  IniFile := TIniFile.Create(FileName);
  try
    inherited WriteToIniFile(IniFile);
  finally
    IniFile.Free;
  end;
  end;

  procedure TRigg.LoadFromIniFile(FileName: String);
  var
  IniFile: TIniFile;
  begin
  IniFile := TIniFile.Create(FileName);
  try
    inherited LoadFromIniFile(IniFile);
  finally
    IniFile.Free;
  end;
  end;
}

procedure TRigg.WriteToDocFile(FileName: String);
var
  Document: TRggDocument;
  S: String;
begin
  Document := TRggDocument.Create;
  try
    GetDocument(Document);
    S := ExtractFileExt(FileName);
    if S = '.rgi' then
    begin
      Document.WriteToIniFile(FileName);
    end
    else if S = '.rgg' then
    begin
      S := ChangeFileExt(FileName, '.rgi');
      Document.WriteToIniFile(FileName);
      // Document.SaveToFile(FileName);
    end;
  finally
    Document.Free;
  end;
end;

procedure TRigg.LoadFromDocFile(FileName: String);
var
  Document: TRggDocument;
  S: String;
begin
  Document := TRggDocument.Create;
  try
    S := ExtractFileExt(FileName);
    if S = '.rgi' then
    begin
      Document.LoadFromIniFile(FileName);
      SetDocument(Document);
    end;
    // if S = '.rgg' then
    // begin
    // Document.LoadFromFile(FileName);
    // SetDocument(Document);
    // end;
  finally
    Document.Free;
  end;
end;

procedure TRigg.Assign(Source: TPersistent);
var
  Document: TRggDocument;
begin
  if Source is TRigg then
  begin
    Document := TRggDocument.Create;
    try
      (Source as TRigg).GetDocument(Document);
      SetDocument(Document);
    finally
      Document.Free;
    end;
    Exit;
  end;
  inherited Assign(Source);
end;

procedure TRigg.GetDocument(Doc: TRggDocument);
begin
  UpdateGSB;
  { Rigg: Typ }
  Doc.SalingTyp := SalingTyp;
  Doc.ControllerTyp := ControllerTyp;
  Doc.CalcTyp := CalcTyp;
  { Mast: Abmessungen }
  Doc.FiMastL := FiMastL;
  Doc.FiMastunten := FiMastunten;
  Doc.FiMastoben := FiMastoben;
  Doc.FiMastfallVorlauf := FiMastfallVorlauf;
  Doc.FiControllerAnschlag := FiControllerAnschlag;
  { Rumpf: Koordinaten }
  Doc.iP := iP;
  { Festigkeitswerte }
  Doc.rEA := rEA;
  Doc.EI := EI;
  { Grenzwerte und Istwerte }
  Doc.GSB := GSB;
  { Trimmtabelle }
  Doc.TrimmTabDaten := TrimmTab.TrimmTabDaten;
end;

procedure TRigg.SetDocument(Doc: TRggDocument);
var
  InputRec: TTrimmControls;
  tempManipulatorMode: Boolean;
begin
  { Mast: Abmessungen }
  FiMastL := Doc.FiMastL;
  FiMastunten := Doc.FiMastunten;
  FiMastoben := Doc.FiMastoben;
  FiMastfallVorlauf := Doc.FiMastfallVorlauf;
  FiControllerAnschlag := Doc.FiControllerAnschlag;
  { Rumpf: Koordinaten }
  iP := Doc.iP;
  { Festigkeitswerte }
  rEA := Doc.rEA;
  EI := Doc.EI;
  { Grenzwerte }
  GSB := Doc.GSB;
  { Trimmtabelle }
  TrimmTab.TrimmTabDaten := Doc.TrimmTabDaten;
  { Istwerte }
  InputRec.Controller := Doc.GSB[fpController,Ist];
  InputRec.Winkel := Doc.GSB[fpWinkel,Ist];
  InputRec.Vorstag := Doc.GSB[fpVorstag,Ist];
  InputRec.Wanten := Doc.GSB[fpWante,Ist];
  InputRec.Woben := Doc.GSB[fpWoben,Ist];
  InputRec.Wunten := InputRec.Wanten - InputRec.Woben;
  InputRec.SalingH := Doc.GSB[fpSalingH,Ist];
  InputRec.SalingA := Doc.GSB[fpSalingA,Ist];
  InputRec.SalingL := Doc.GSB[fpSalingL,Ist];
  InputRec.Vorstag := Doc.GSB[fpVorstagOS,Ist];
  InputRec.WPowerOS := Doc.GSB[fpWPowerOS,Ist];
  Glieder := InputRec; { --> IntGliederToReal }
  Reset; { restliche Gleitkommawerte für Rumpf und Mast aktualisieren }

  { Rigg: Typ }
  SalingTyp := Doc.SalingTyp;
  ControllerTyp := Doc.ControllerTyp;
  CalcTyp := Doc.CalcTyp;

  tempManipulatorMode := ManipulatorMode;
  ManipulatorMode := false;
  UpdateGetriebe;
  UpdateRigg;
  ManipulatorMode := tempManipulatorMode;

  UpdateGSB;
end;

procedure TRigg.SetDefaultDocument;
var
  Document: TRggDocument;
begin
  Document := TRggDocument.Create;
  try
   Document.GetDefaultDocument;
   SetDocument(Document);
  finally
    Document.Free;
  end;
end;

procedure TRigg.GetRealTrimmRecord(var RealTrimm: TRealTrimm);
{ Die Funktion überprüft nicht, ob die Werte in Rigg aktualisiert sind.
  Einige Werte stehen schon nach Aufruf von UpdateGetriebe() zur Verfügung.
  Andere erst nach Aufruf von UpdateRigg(). }
begin
  { Auslenkung und Wantenspannung }
  RealTrimm.VorstagDiff := VorstagDiff;
  RealTrimm.SpannungW := SpannungW;
  with RealTrimm do
   begin
    { Mastfall }
    MastfallF0F := Abstand(rP[ooF0], rP[ooF]); { in mm }
    { Vorstagspannung }
    if abs(rF[14]) < 32000 then
      SpannungV := rF[14] { in N }
    else
    begin
      if rF[14] > 32000 then
        SpannungV := 32000;
      if rF[14] < -32000 then
        SpannungV := -32000;
    end;
    { Biegung an den Salingen }
    BiegungS := hd; { in mm }
    { Biegung am Controller }
    BiegungC := he; { in mm }
    { "Elastizität" }
    FlexWert := Abstand(rP[ooC], rPe[ooC]); { in mm }
  end;
end;

function TRigg.GetRealTrimm(Index: TTrimmIndex): double;
var
  temp: double;
begin
  temp := 0;
  case Index of
    tiMastfallF0F: temp := Abstand(rP[ooF0], rP[ooF]);
    tiMastfallF0C: temp := Abstand(rP[ooF0], rP[ooC]);
    tiVorstagDiff: temp := VorstagDiff;
    tiVorstagDiffE: temp := Abstand(rPe[ooC0], rPe[ooC]) - rL[14];
    tiSpannungW: temp := SpannungW;
    tiSpannungV:
    begin
        if abs(rF[14]) < 32000 then
          temp := rF[14] { in N }
        else
        begin
          if rF[14] > 32000 then
            temp := 32000;
          if rF[14] < -32000 then
            temp := -32000;
      end;
    end;
    tiBiegungS: temp := hd;
    tiBiegungC: temp := he;
    tiFlexWert: temp := Abstand(rP[ooC], rPe[ooC]); { in mm }
  end;
  result := temp;
end;

end.
