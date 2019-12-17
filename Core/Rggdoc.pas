unit Rggdoc;

{
  Als primäre Daten werden die Rumpfkoordinaten und die RiggLängen betrachtet.
  Der Vollständigkeit halber werden aber auch die Riggkoordinaten gespeichert.
  Dies stellt zwar eine Redundanz dar, hat aber den Vorteil, daß eine vollständige
  Koordinatenschnittstelle nach außen zur Verfügung steht. Die RumpfLängen werden
  jedoch nicht im Dokument gespeichert.

  Alle Koordinaten und Längen sind Integerwerte! Sie sind in mm angegeben. Es ist
  nicht praxisbezogen, für die Ein- und Ausgabe dieser Werte Kommastellen zuzulassen.

  Die Rigglängen sind in den Istwerten des GSB Arrays gespeichert.
  (GSB = GetriebeScrollBar)
  Die Kraft FiWPowerOS ist ebenfalls im GSB Array untegebracht.
  Die Arrayfelder TinyStep und BigStep können eventuell eingespart werden.

  Das Salingdreieck wird grundsätzlich mit den beiden Werten FiSalingH und
  FiSalingA beschrieben. Der zugehörige Wert FiSalingL wird zwar gespeichert,
  sollte aber immer aus FiSalingH und FiSalingA berechnet werden. Da FiSalingH
  und FiSalingA Integerwerte sind, kann für FiSalingL nämlich kein exakt passender
  Integerwert gespeichert werden.

  Der allgemeine und zu bevorzugende SalingTyp ist stFest. Ein Rigg vom SalingTyp
  stDrehbar, stOhne bzw. stOhne_2 kann auch mit SalingTyp stFest gespeichert
  werden.

  Die Festigkeitswerte EA und EI sowie die meisten Felder der Trimmtabelle sind
  Gleitkommazahlen. Wenn als Calctyp Biegeknicken angegeben ist, dann werden in
  der 'Trimmtabelle' die Biegeknickparameter gespeichert.

  Binärformat:
  Pos Size Description, Typ
  0   8    Signature, Array[1..8] of Char
  8   4    SalingTyp, Integer
  12  4    ControllerTyp, Integer
  16  4    CalcTyp, Integer
  20  4    FiMastL, Integer
  24  4    FiMastunten, Integer
  28  4    FiMastoben, Integer
  32  4    FiMastfallVorlauf, Integer
  36  4    FiControllerAnschlag, Integer
  40  4    FiReserved, Integer
  iP, TIntRiggPoints
  rEA, TRiggLvektor
  EI, double
  GSB, TsbArray
  TrimmTabDaten, TTrimmTabDaten

}

interface

uses
  System.SysUtils,
  System.Classes,
  System.Inifiles,
  System.Math,
  RiggVar.RG.Def,
  Dialogs,
  RggTypes;

const
  RggDocSignature: string = 'RGGDOC01';

type
  TRggDocument = class
  private
    FFileName: String; // nur zum Zwischenspeichern des Dateinamens
    procedure GetLogoDoc;
    procedure GetDefaultDoc;
    procedure LoadSignatureFromStream(strm: TStream);
    procedure SaveSignatureToStream(strm: TStream);
  public
    Signature: string;
    { Rigg: Typ }
    SalingTyp: TSalingTyp;
    ControllerTyp: TControllerTyp;
    CalcTyp: TCalcTyp;
    { Mast: Abmessungen in mm }
    FiMastL: Integer;
    FiMastunten: Integer;
    FiMastoben: Integer;
    FiMastfallVorlauf: Integer;
    FiControllerAnschlag: Integer;
    FiReserved: Integer;
    { Rumpf: Koordinaten in mm }
    iP: TIntRiggPoints; { Array enthält auch die Riggkoordinaten }
    { Festigkeitswerte }
    rEA: TRiggLvektor; { N }
    EI: double; { Nmm^2 }
    { Grenzwerte und Istwerte }
    GSB: TsbArray;
    { Trimmtabelle }
    TrimmTabDaten: TTrimmTabDaten;

    procedure TestStream;

    function SignatureOK: Boolean; virtual;
    procedure GetDefaultDocument; virtual;
    procedure LoadFromFile(FileName: String); virtual;
    procedure SaveToFile(FileName: String); virtual;
    procedure LoadFromStream(strm: TStream); virtual;
    procedure SaveToStream(strm: TStream); virtual;
    procedure LoadFromIniFile(FileName: String); virtual;
    procedure WriteToIniFile(FileName: String); virtual;
    procedure DumpToMemo(Memo: TStrings); virtual;
    procedure ReadFromDFM(Memo: TStrings);
    procedure WriteToDFM(Memo: TStrings);
    function SaveToString: string;
    procedure LoadFromString(s: string);
    function SaveToXML: string;
    procedure LoadFromXML(s: string);
    function SaveToXMLBase64: string;
    function LoadFromXMLBase64(s: string): string;
  end;

implementation

uses
  RiggVar.App.Main,
  StrBox,
  IdCoderMime;

procedure TRggDocument.LoadFromFile(FileName: String);
var
  S: TFileStream;
begin
  S := TFileStream.Create(FileName, fmOpenRead);
  try
    FFileName := FileName;
    LoadFromStream(S);
  finally
    S.Free;
  end;
end;

procedure TRggDocument.SaveToFile(FileName: String);
var
  S: TFileStream;
begin
  S := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(S);
  finally
    S.Free;
  end;
end;

procedure TRggDocument.TestStream;
var
  ms: TMemoryStream;
begin
  ms := TMemoryStream.Create;
  SaveToStream(ms);
  ms.Position := 0;
  LoadFromStream(ms);
  ms.Free;
end;

procedure TRggDocument.SaveSignatureToStream(strm: TStream);
var
  ss: TStringStream;
  c: Integer;
begin
  //strm.WriteBuffer(RggDocSignature[1], 8);

  ss := TStringStream.Create;
  ss.WriteString(RggDocSignature);
  c := ss.Position;
  ss.Position := 0;
  strm.CopyFrom(ss, c);
  ss.Free;
end;

procedure TRggDocument.LoadSignatureFromStream(strm: TStream);
var
  ss: TStringStream;
  c: Integer;
begin
  { Signature }
  ss := TStringStream.Create;
  ss.CopyFrom(strm, RggDocSignature.Length);
  c := ss.Position;
  ss.Position := 0;
  Signature := 'temp';
  Signature := ss.ReadString(c);
  ss.Free;
end;

procedure TRggDocument.LoadFromStream(strm: TStream);
var
  temp: Integer;
begin
  { Signature }
  LoadSignatureFromStream(strm);
  if not SignatureOK then
    raise EFileFormatError.Create('FormatError');

  { Rigg: Typ }
  strm.ReadBuffer(temp, SizeOf(Integer));
  SalingTyp := TSalingTyp(temp);
  strm.ReadBuffer(temp, SizeOf(Integer));
  ControllerTyp := TControllerTyp(temp);
  strm.ReadBuffer(temp, SizeOf(Integer));
  CalcTyp := TCalcTyp(temp);
  { Mast: Abmessungen }
  strm.ReadBuffer(FiMastL, SizeOf(Integer));
  strm.ReadBuffer(FiMastunten, SizeOf(Integer));
  strm.ReadBuffer(FiMastoben, SizeOf(Integer));
  strm.ReadBuffer(FiMastfallVorlauf, SizeOf(Integer));
  strm.ReadBuffer(FiControllerAnschlag, SizeOf(Integer));
  strm.ReadBuffer(FiReserved, SizeOf(Integer));
  { Rumpf: Koordinaten }
  strm.ReadBuffer(iP, SizeOf(TIntRiggPoints));
  { Festigkeitswerte }
  strm.ReadBuffer(rEA, SizeOf(TRiggLvektor));
  strm.ReadBuffer(EI, SizeOf(double));
  { Grenzwerte }
  strm.ReadBuffer(GSB, SizeOf(TsbArray));
  { Trimmtabelle }
  strm.ReadBuffer(TrimmTabDaten, SizeOf(TTrimmTabDaten));
end;

procedure TRggDocument.SaveToStream(strm: TStream);
begin
  { Signature }
  SaveSignatureToStream(strm);

  { Rigg: Typ }
  strm.WriteBuffer(SalingTyp, SizeOf(Integer));
  strm.WriteBuffer(ControllerTyp, SizeOf(Integer));
  strm.WriteBuffer(CalcTyp, SizeOf(Integer));
  { Mast: Abmessungen }
  strm.WriteBuffer(FiMastL, SizeOf(Integer));
  strm.WriteBuffer(FiMastunten, SizeOf(Integer));
  strm.WriteBuffer(FiMastoben, SizeOf(Integer));
  strm.WriteBuffer(FiMastfallVorlauf, SizeOf(Integer));
  strm.WriteBuffer(FiControllerAnschlag, SizeOf(Integer));
  strm.WriteBuffer(FiReserved, SizeOf(Integer));
  { Rumpf: Koordinaten }
  strm.WriteBuffer(iP, SizeOf(TIntRiggPoints));
  { Festigkeitswerte }
  strm.WriteBuffer(rEA, SizeOf(TRiggLvektor));
  strm.WriteBuffer(EI, SizeOf(double));
  { Grenzwerte }
  strm.WriteBuffer(GSB, SizeOf(TsbArray));
  { Trimmtabelle }
  strm.WriteBuffer(TrimmTabDaten, SizeOf(TTrimmTabDaten));
end;

procedure TRggDocument.LoadFromIniFile(FileName: String);
var
  S, S1, S2: String;
  i: Integer;
  T: TTrimmTabDaten;
  IniFile: TIniFile;
begin
  GetDefaultDocument;
  IniFile := TIniFile.Create(FileName);
  try
    with IniFile do
    begin
    S := 'Rigg';
      SalingTyp := TSalingTyp(ReadInteger(S, 'SalingTyp', Ord(stFest)));
      ControllerTyp := TControllerTyp(ReadInteger(S, 'ControllerTyp', Ord(ctDruck)));
      CalcTyp := TCalcTyp(ReadInteger(S, 'CalcTyp', Ord(ctBiegeKnicken)));

    S := 'Trimmtabelle';
    T := DefaultTrimmTabDaten;
    with TrimmTabDaten do
    begin
      try
          T.TabellenTyp := TTabellenTyp(ReadInteger(S, 'TabellenTyp', Ord(itParabel)));
          S1 := ReadString(S, 'a0', FloatToStrF(T.a0, ffGeneral, 8, 2));
          T.a0 := StrToFloat(S1);
          S1 := ReadString(S, 'a1', FloatToStrF(T.a1, ffGeneral, 8, 2));
          T.a1 := StrToFloat(S1);
          S1 := ReadString(S, 'a2', FloatToStrF(T.a2, ffGeneral, 8, 2));
          T.a2 := StrToFloat(S1);
          S1 := ReadString(S, 'x0', FloatToStrF(T.x0, ffGeneral, 8, 2));
          T.x0 := StrToFloat(S1);
          S1 := ReadString(S, 'x1', FloatToStrF(T.x1, ffGeneral, 8, 2));
          T.x1 := StrToFloat(S1);
          S1 := ReadString(S, 'x2', FloatToStrF(T.x2, ffGeneral, 8, 2));
          T.x2 := StrToFloat(S1);
        except
          on EConvertError do
            T := DefaultTrimmTabDaten;
      end;
    end;
    TrimmTabDaten := T;

    S := 'Mast';
      FiMastL := ReadInteger(S, 'MastL', FiMastL);
      FiMastunten := ReadInteger(S, 'Mastunten', FiMastunten);
      FiMastoben := ReadInteger(S, 'Mastoben', FiMastoben);
      FiMastfallVorlauf := ReadInteger(S, 'MastfallVorlauf', FiMastfallVorlauf);
      FiControllerAnschlag := ReadInteger(S, 'ControllerAnschlag', FiControllerAnschlag);
    EI := ReadInteger(S, 'EI', 14700) * 1E6;

    S := 'Ist';
    GSB[fpController,Ist] := ReadInteger(S, 'Controller', GSB[fpController,Ist]);
    GSB[fpWinkel,Ist] := ReadInteger(S, 'Winkel', GSB[fpWinkel,Ist]);
    GSB[fpVorstag,Ist] := ReadInteger(S, 'Vorstag', GSB[fpVorstag,Ist]);
    GSB[fpWante,Ist] := ReadInteger(S, 'Wante', GSB[fpWante,Ist]);
    GSB[fpWoben,Ist] := ReadInteger(S, 'Woben', GSB[fpWoben,Ist]);
    GSB[fpSalingH,Ist] := ReadInteger(S, 'SalingH', GSB[fpSalingH,Ist]);
    GSB[fpSalingA,Ist] := ReadInteger(S, 'SalingA', GSB[fpSalingA,Ist]);
    GSB[fpSalingL,Ist] := ReadInteger(S, 'SalingL', GSB[fpSalingL,Ist]);
    GSB[fpVorstagOS,Ist] := ReadInteger(S, 'VorstagOS', GSB[fpVorstagOS,Ist]);
    GSB[fpWPowerOS,Ist] := ReadInteger(S, 'WPowerOS', GSB[fpWPowerOS,Ist]);

    S := 'Min';
    GSB[fpController,Min] := ReadInteger(S,'Controller',GSB[fpController,Min]);
    GSB[fpWinkel,Min] := ReadInteger(S,'Winkel',GSB[fpWinkel,Min]);
    GSB[fpVorstag,Min] := ReadInteger(S,'Vorstag',GSB[fpVorstag,Min]);
    GSB[fpWante,Min] := ReadInteger(S,'Wante',GSB[fpWante,Min]);
    GSB[fpWoben,Min] := ReadInteger(S,'Woben',GSB[fpWoben,Min]);
    GSB[fpSalingH,Min] := ReadInteger(S,'SalingH',GSB[fpSalingH,Min]);
    GSB[fpSalingA,Min] := ReadInteger(S,'SalingA',GSB[fpSalingA,Min]);
    GSB[fpSalingL,Min] := ReadInteger(S,'SalingL',GSB[fpSalingL,Min]);
    GSB[fpVorstagOS,Min] := ReadInteger(S,'VorstagOS',GSB[fpVorstagOS,Min]);
    GSB[fpWPowerOS,Min] := ReadInteger(S,'WPowerOS',GSB[fpWPowerOS,Min]);

    S := 'Max';
    GSB[fpController,Max] := ReadInteger(S,'Controller',GSB[fpController,Max]);
    GSB[fpWinkel,Max] := ReadInteger(S,'Winkel',GSB[fpWinkel,Max]);
    GSB[fpVorstag,Max] := ReadInteger(S,'Vorstag',GSB[fpVorstag,Max]);
    GSB[fpWante,Max] := ReadInteger(S,'Wante',GSB[fpWante,Max]);
    GSB[fpWoben,Max] := ReadInteger(S,'Woben',GSB[fpWoben,Max]);
    GSB[fpSalingH,Max] := ReadInteger(S,'SalingH',GSB[fpSalingH,Max]);
    GSB[fpSalingA,Max] := ReadInteger(S,'SalingA',GSB[fpSalingA,Max]);
    GSB[fpSalingL,Max] := ReadInteger(S,'SalingL',GSB[fpSalingL,Max]);
    GSB[fpVorstagOS,Max] := ReadInteger(S,'VorstagOS',GSB[fpVorstagOS,Max]);
    GSB[fpWPowerOS,Max] := ReadInteger(S,'WPowerOS',GSB[fpWPowerOS,Max]);

    S := 'Koordinaten Rumpf';
    iP[ooA0,x] := ReadInteger(S,'A0x',iP[ooA0,x]);
    iP[ooA0,y] := ReadInteger(S,'A0y',iP[ooA0,y]);
    iP[ooA0,z] := ReadInteger(S,'A0z',iP[ooA0,z]);
    iP[ooB0,x] := ReadInteger(S,'B0x',iP[ooB0,x]);
    iP[ooB0,y] := ReadInteger(S,'B0y',iP[ooB0,y]);
    iP[ooB0,z] := ReadInteger(S,'B0z',iP[ooB0,z]);
    iP[ooC0,x] := ReadInteger(S,'C0x',iP[ooC0,x]);
    iP[ooC0,y] := ReadInteger(S,'C0y',iP[ooC0,y]);
    iP[ooC0,z] := ReadInteger(S,'C0z',iP[ooC0,z]);
    iP[ooD0,x] := ReadInteger(S,'D0x',iP[ooD0,x]);
    iP[ooD0,y] := ReadInteger(S,'D0y',iP[ooD0,y]);
    iP[ooD0,z] := ReadInteger(S,'D0z',iP[ooD0,z]);
    iP[ooE0,x] := ReadInteger(S,'E0x',iP[ooE0,x]);
    iP[ooE0,y] := ReadInteger(S,'E0y',iP[ooE0,y]);
    iP[ooE0,z] := ReadInteger(S,'E0z',iP[ooE0,z]);
    iP[ooF0,x] := ReadInteger(S,'F0x',iP[ooF0,x]);
    iP[ooF0,y] := ReadInteger(S,'F0y',iP[ooF0,y]);
    iP[ooF0,z] := ReadInteger(S,'F0z',iP[ooF0,z]);

    S := 'Koordinaten Rigg';
    iP[ooA,x] := ReadInteger(S,'Ax',iP[ooA,x]);
    iP[ooA,y] := ReadInteger(S,'Ay',iP[ooA,y]);
    iP[ooA,z] := ReadInteger(S,'Az',iP[ooA,z]);
    iP[ooB,x] := ReadInteger(S,'Bx',iP[ooB,x]);
    iP[ooB,y] := ReadInteger(S,'By',iP[ooB,y]);
    iP[ooB,z] := ReadInteger(S,'Bz',iP[ooB,z]);
    iP[ooC,x] := ReadInteger(S,'Cx',iP[ooC,x]);
    iP[ooC,y] := ReadInteger(S,'Cy',iP[ooC,y]);
    iP[ooC,z] := ReadInteger(S,'Cz',iP[ooC,z]);
    iP[ooD,x] := ReadInteger(S,'Dx',iP[ooD,x]);
    iP[ooD,y] := ReadInteger(S,'Dy',iP[ooD,y]);
    iP[ooD,z] := ReadInteger(S,'Dz',iP[ooD,z]);
    iP[ooE,x] := ReadInteger(S,'Ex',iP[ooE,x]);
    iP[ooE,y] := ReadInteger(S,'Ey',iP[ooE,y]);
    iP[ooE,z] := ReadInteger(S,'Ez',iP[ooE,z]);
    iP[ooF,x] := ReadInteger(S,'Fx',iP[ooF,x]);
    iP[ooF,y] := ReadInteger(S,'Fy',iP[ooF,y]);
    iP[ooF,z] := ReadInteger(S,'Fz',iP[ooF,z]);

    S := 'EA';
    for i := 0 to 19 do
    begin
      S1 := IntToStr(i);
      S2 := ReadString(S, S1, '100000');
      rEA[i] := StrToFloat(S2);
    end;
  end;
  finally
    IniFile.Free;
  end;
end;

procedure TRggDocument.WriteToIniFile(FileName: String);
var
  S, S1, S2: String;
  i, tempEI: Integer;
  IniFile: TIniFile;
begin
  IniFile := TIniFile.Create(FileName);
  try
  with IniFile do
  begin
    S := 'Rigg';
      WriteInteger(S, 'SalingTyp', Ord(SalingTyp));
      WriteInteger(S, 'ControllerTyp', Ord(ControllerTyp));
      WriteInteger(S, 'CalcTyp', Ord(CalcTyp));

    S := 'Trimmtabelle';
      with TrimmTabDaten do
      begin
        WriteInteger(S, 'TabellenTyp', Ord(TabellenTyp));
        S1 := Format('%10.5f', [a0]);
        WriteString(S, 'a0', S1);
        S1 := Format('%10.5f', [a1]);
        WriteString(S, 'a1', S1);
        S1 := Format('%10.5f', [a2]);
        WriteString(S, 'a2', S1);
        S1 := Format('%10.5f', [x0]);
        WriteString(S, 'x0', S1);
        S1 := Format('%10.5f', [x1]);
        WriteString(S, 'x1', S1);
        S1 := Format('%10.5f', [x2]);
        WriteString(S, 'x2', S1);
    end;

    S := 'Mast';
      WriteInteger(S, 'MastL', FiMastL);
      WriteInteger(S, 'Mastunten', FiMastunten);
      WriteInteger(S, 'Mastoben', FiMastoben);
      WriteInteger(S, 'MastfallVorlauf', FiMastfallVorlauf);
      tempEI := Round(EI / 1E6);
    WriteInteger(S, 'EI', tempEI);

    S := 'Ist';
    WriteInteger(S,'Controller',GSB[fpController,Ist]);
    WriteInteger(S,'Winkel',GSB[fpWinkel,Ist]);
    WriteInteger(S,'Vorstag',GSB[fpVorstag,Ist]);
    WriteInteger(S,'Wante',GSB[fpWante,Ist]);
    WriteInteger(S,'Woben',GSB[fpWoben,Ist]);
    WriteInteger(S,'SalingH',GSB[fpSalingH,Ist]);
    WriteInteger(S,'SalingA',GSB[fpSalingA,Ist]);
    WriteInteger(S,'SalingL',GSB[fpSalingL,Ist]);
    WriteInteger(S,'VorstagOS',GSB[fpVorstagOS,Ist]);
    WriteInteger(S,'WPowerOS',GSB[fpWPowerOS,Ist]);

    S := 'Min';
    WriteInteger(S,'Controller',GSB[fpController,Min]);
    WriteInteger(S,'Winkel',GSB[fpWinkel,Min]);
    WriteInteger(S,'Vorstag',GSB[fpVorstag,Min]);
    WriteInteger(S,'Wante',GSB[fpWante,Min]);
    WriteInteger(S,'Woben',GSB[fpWoben,Min]);
    WriteInteger(S,'SalingH',GSB[fpSalingH,Min]);
    WriteInteger(S,'SalingA',GSB[fpSalingA,Min]);
    WriteInteger(S,'SalingL',GSB[fpSalingL,Min]);
    WriteInteger(S,'VorstagOS',GSB[fpVorstagOS,Min]);
    WriteInteger(S,'WPowerOS',GSB[fpWPowerOS,Min]);

    S := 'Max';
    WriteInteger(S,'Controller',GSB[fpController,Max]);
    WriteInteger(S,'Winkel',GSB[fpWinkel,Max]);
    WriteInteger(S,'Vorstag',GSB[fpVorstag,Max]);
    WriteInteger(S,'Wante',GSB[fpWante,Max]);
    WriteInteger(S,'Woben',GSB[fpWoben,Max]);
    WriteInteger(S,'SalingH',GSB[fpSalingH,Max]);
    WriteInteger(S,'SalingA',GSB[fpSalingA,Max]);
    WriteInteger(S,'SalingL',GSB[fpSalingL,Max]);
    WriteInteger(S,'VorstagOS',GSB[fpVorstagOS,Max]);
    WriteInteger(S,'WPowerOS',GSB[fpWPowerOS,Max]);

    S := 'Koordinaten Rumpf';
    WriteInteger(S,'A0x',iP[ooA0,x]);
    WriteInteger(S,'A0y',iP[ooA0,y]);
    WriteInteger(S,'A0z',iP[ooA0,z]);
    WriteInteger(S,'B0x',iP[ooB0,x]);
    WriteInteger(S,'B0y',iP[ooB0,y]);
    WriteInteger(S,'B0z',iP[ooB0,z]);
    WriteInteger(S,'C0x',iP[ooC0,x]);
    WriteInteger(S,'C0y',iP[ooC0,y]);
    WriteInteger(S,'C0z',iP[ooC0,z]);
    WriteInteger(S,'D0x',iP[ooD0,x]);
    WriteInteger(S,'D0y',iP[ooD0,y]);
    WriteInteger(S,'D0z',iP[ooD0,z]);
    WriteInteger(S,'E0x',iP[ooE0,x]);
    WriteInteger(S,'E0y',iP[ooE0,y]);
    WriteInteger(S,'E0z',iP[ooE0,z]);
    WriteInteger(S,'F0x',iP[ooF0,x]);
    WriteInteger(S,'F0y',iP[ooF0,y]);
    WriteInteger(S,'F0z',iP[ooF0,z]);

    S := 'Koordinaten Rigg';
    WriteInteger(S,'Ax',iP[ooA,x]);
    WriteInteger(S,'Ay',iP[ooA,y]);
    WriteInteger(S,'Az',iP[ooA,z]);
    WriteInteger(S,'Bx',iP[ooB,x]);
    WriteInteger(S,'By',iP[ooB,y]);
    WriteInteger(S,'Bz',iP[ooB,z]);
    WriteInteger(S,'Cx',iP[ooC,x]);
    WriteInteger(S,'Cy',iP[ooC,y]);
    WriteInteger(S,'Cz',iP[ooC,z]);
    WriteInteger(S,'Dx',iP[ooD,x]);
    WriteInteger(S,'Dy',iP[ooD,y]);
    WriteInteger(S,'Dz',iP[ooD,z]);
    WriteInteger(S,'Ex',iP[ooE,x]);
    WriteInteger(S,'Ey',iP[ooE,y]);
    WriteInteger(S,'Ez',iP[ooE,z]);
    WriteInteger(S,'Fx',iP[ooF,x]);
    WriteInteger(S,'Fy',iP[ooF,y]);
    WriteInteger(S,'Fz',iP[ooF,z]);

    S := 'EA';
    for i := 0 to 19 do
    begin
      S1 := IntToStr(i);
      S2 := Format('%.6g', [rEA[i]]);
      WriteString(S, S1, S2);
    end;
  end;
  finally
    IniFile.Free;
  end;
end;

procedure TRggDocument.GetDefaultDocument;
begin
  if WantLogoData then
    GetLogoDoc
  else
    GetDefaultDoc;
end;

procedure TRggDocument.GetDefaultDoc;
const
  EModulStahl = 210E3; { N/mm^2 }
  EModulAlu = 70E3; { N/mm^2 }
  EAgross = 100E6; { N }
  EARumpf = 10E6; { N }
  EASaling = 1E6; { N }
var
  i: TsbName;
begin
  // see (update) similar code (duplication) in TGetriebe.GetDefaultData

  { Signature }
  Signature := RggDocSignature;

  { Rigg: Typ }
  SalingTyp := stFest;
  ControllerTyp := ctDruck;
  CalcTyp := ctBiegeKnicken;

  { Mast: Abmessungen }
  FiMastL := 6115; { Gesamtlänge Mast }
  FiMastunten := 2600; { unterer Teil Mast }
  FiMastoben := 2000; { oberer Teil Mast }
  FiMastfallVorlauf := 5000; { Abstand der Meßmarken }
  FiControllerAnschlag := 50;
  FiReserved := 0;

  { Rumpf: Koordinaten }
  iP[ooA0, x] := 2560; { Pütting Stbd }
  iP[ooA0, y] := 765;
  iP[ooA0, z] := 430;

  iP[ooB0, x] := 2560; { Püttinge Bb }
  iP[ooB0, y] := -765;
  iP[ooB0, z] := 430;

  iP[ooC0, x] := 4140; { Vorstag }
  iP[ooC0, y] := 0;
  iP[ooC0, z] := 340;

  iP[ooD0, x] := 2870; { Mastfuß }
  iP[ooD0, y] := 0;
  iP[ooD0, z] := -100;

  iP[ooE0, x] := 2970; { Controller }
  iP[ooE0, y] := 0;
  iP[ooE0, z] := 450;

  iP[ooF0, x] := -30; { Spiegel }
  iP[ooF0, y] := 0;
  iP[ooF0, z] := 300;

  iP[ooP0] := iP[ooA0];
  iP[ooP0, y] := 0;

  { iP[ooA]..iP[ooF] werden hier nicht gefüllt! }

  { Festigkeitswerte }
  rEA[0] :=  EAgross;
  rEA[1] :=  EARumpf;
  rEA[2] :=  EARumpf;
  rEA[3] :=  EARumpf;
  rEA[4] :=  EARumpf;
  rEA[5] :=  EARumpf;
  rEA[6] :=  EARumpf;
  rEA[7] :=  13 * EModulStahl;
  rEA[8] :=  13 * EModulStahl;
  rEA[9] :=  EAgross;
  rEA[10] := EAgross;
  rEA[11] := EASaling;
  rEA[12] := 13 * EModulStahl;
  rEA[13] := 13 * EModulStahl;
  rEA[14] := 13 * EModulStahl;
  rEA[15] := EAgross;
  rEA[16] := EAgross;
  rEA[17] := EAgross;
  rEA[18] := EAgross;
  rEA[19] := EAgross;

  EI := 14.7E9; { Nmm^2 }

  {Grenzwerte und Istwerte}
  GSB[fpController,Ist] := 100; { Controllerposition bzw. Abstand E0-E }
  GSB[fpWinkel,Ist] := 950; { Winkel der unteren Wantabschnitte Winkel in 10E-1 Grad }
  GSB[fpVorstag,Ist] := 4500;
  GSB[fpWante,Ist] := 4120;
  GSB[fpWoben,Ist] := 2020;
  GSB[fpSalingH,Ist] := 220;
  GSB[fpSalingA,Ist] := 850;
  GSB[fpSalingL,Ist] := Round(sqrt(sqr(GSB[fpSalingH,Ist])+sqr(GSB[fpSalingA,Ist]/2)));
  GSB[fpVorstagOS,Ist] := GSB[fpVorstag,Ist];
  GSB[fpWPowerOS,Ist] := 1000; { angenommene Wantenspannung 3d }

  for i := fpController to fpWPowerOS do
  begin
    GSB[i, TinyStep] := 1;
    GSB[i, BigStep] := 10;
  end;

  { Bereichsgrenzen einstellen:
    Woben2d.Min + SalingH.Min > Mastoben
    Mastunten + SalingH.Min > Abstand D0-P, daraus Winkel.Max }
  GSB[fpController,Min] :=  50;
  GSB[fpController,Max] := 200;
  GSB[fpWinkel,Min] :=  850;
  GSB[fpWinkel,Max] := 1050;
  GSB[fpVorstag,Min] := 4400;
  GSB[fpVorstag,Max] := 4600;
  GSB[fpWante,Min] := 4050;
  GSB[fpWante,Max] := 4200;
  GSB[fpWoben,Min] := 2000;
  GSB[fpWoben,Max] := 2070;
  GSB[fpSalingH,Min] := 140;
  GSB[fpSalingH,Max] := 300;
  GSB[fpSalingA,Min] :=  780;
  GSB[fpSalingA,Max] := 1000;
  GSB[fpSalingL,Min] := 450;
  GSB[fpSalingL,Max] := 600;
  GSB[fpVorstagOS,Min] := 4200;
  GSB[fpVorstagOS,Max] := 4700;
  GSB[fpWPowerOS,Min] := 100;
  GSB[fpWPowerOS,Max] := 3000;

  { TrimmTab.TrimmTabDaten := DefaultTrimmTabDaten; } { siehe RggTypes }
  with TrimmTabDaten do
  begin
    TabellenTyp := itGerade;
    a0 := 0; { zur Zeit nicht verwendet }
    a1 := 0.1;
    a2 := 0;
    x0 := 0; { zur Zeit nicht verwendet }
    x1 := 500;
    x2 := 1000;
  end;
end;

procedure TRggDocument.GetLogoDoc;
const
  EModulStahl = 210E3; { N/mm^2 }
  EModulAlu = 70E3; { N/mm^2 }
  EAgross = 100E6; { N }
  EARumpf = 10E6; { N }
  EASaling = 1E6; { N }

var
  i: TsbName;
  f, ox, oz: Integer;
begin
  // see similar code (duplication) in TGetriebe.GetLogoData

  { Signature }
  Signature := string(RggDocSignature);

  ox := 1400;
  oz := -350;

  f := 18;

  { Rigg: Typ }
  SalingTyp := stFest;
  ControllerTyp := ctDruck;
  CalcTyp := ctBiegeKnicken;

  { Mast: Abmessungen }
  FiMastL := Round((40 + sqrt(250) * 10) * f); { Gesamtlänge Mast }
  FiMastunten := Round((sqrt(40) + sqrt(10)) * 10 * f); { unterer Teil Mast }
  FiMastoben := Round(sqrt(40) * 10 * f); { oberer Teil Mast }
  FiMastfallVorlauf := Round(FiMastL * 0.75); { Abstand der Meßmarken }
  FiControllerAnschlag := 50;
  FiReserved := 0;

  { RumpfKoordinaten in mm }
  iP[ooA0, x] := 30 * f + ox; { Pütting Stbd }
  iP[ooA0, y] := 40 * f;
  iP[ooA0, z] := 40 * f + oz;

  iP[ooB0, x] := 30 * f + ox;
  iP[ooB0, y] := -40 * f;
  iP[ooB0, z] := 40 * f + oz;

  iP[ooC0, x] := 150 * f + ox;
  iP[ooC0, y] := 0;
  iP[ooC0, z] := 40 * f + oz;

  iP[ooD0, x] := 80 * f + ox;
  iP[ooD0, y] := 0;
  iP[ooD0, z] := 10 * f + oz;

  iP[ooE0, x] := 85 * f + ox;
  iP[ooE0, y] := 0;
  iP[ooE0, z] := 50 * f + oz;

  iP[ooF0, x] := -85 * f + ox;
  iP[ooF0, y] := 0;
  iP[ooF0, z] := 40 * f + oz;

  iP[ooP0] := iP[ooA0];
  iP[ooP0, y] := 0;

  { iP[ooA]..iP[ooF] werden hier nicht gefüllt! }

  { Festigkeitswerte }
  rEA[0] :=  EAgross;
  rEA[1] :=  EARumpf;
  rEA[2] :=  EARumpf;
  rEA[3] :=  EARumpf;
  rEA[4] :=  EARumpf;
  rEA[5] :=  EARumpf;
  rEA[6] :=  EARumpf;
  rEA[7] :=  13 * EModulStahl;
  rEA[8] :=  13 * EModulStahl;
  rEA[9] :=  EAgross;
  rEA[10] := EAgross;
  rEA[11] := EASaling;
  rEA[12] := 13 * EModulStahl;
  rEA[13] := 13 * EModulStahl;
  rEA[14] := 13 * EModulStahl;
  rEA[15] := EAgross;
  rEA[16] := EAgross;
  rEA[17] := EAgross;
  rEA[18] := EAgross;
  rEA[19] := EAgross;

  EI := 14.7E9; { Nmm^2 }

  { Grenzwerte und Istwerte }

  GSB[fpController,Ist] := 100; {Controllerposition bzw. Abstand E0-E}
  GSB[fpWinkel,Ist] := Round((90 + arctan2(1,3) * 180 / pi) * 10);
  { Winkel der unteren Wantabschnitte Winkel in 10E-1 Grad }
  GSB[fpVorstag,Ist] := Round(sqrt(288) * 10 * f);
  GSB[fpWante,Ist] := Round((sqrt(40) + sqrt(56)) * 10 * f);
  GSB[fpWoben,Ist] := Round(sqrt(56) * 10 * f);
  GSB[fpSalingH,Ist] := 40 * f;
  GSB[fpSalingA,Ist] := 80 * f;
  GSB[fpSalingL,Ist] := Round(sqrt(sqr(GSB[fpSalingH,Ist])+sqr(GSB[fpSalingA,Ist]/2)));
  GSB[fpVorstagOS,Ist] := GSB[fpVorstag,Ist];
  GSB[fpWPowerOS,Ist] := 1000; {angenommene Wantenspannung 3d}

  for i := fpController to fpWPowerOS do
  begin
    GSB[i,TinyStep] := 1;
    GSB[i,BigStep] := 10;
  end;

  { Bereichsgrenzen einstellen:
    Woben2d.Min + SalingH.Min > Mastoben
    Mastunten + SalingH.Min > Abstand D0-P, daraus Winkel.Max }
  GSB[fpController,Min] :=  50;
  GSB[fpController,Max] := 200;
  GSB[fpWinkel,Min] :=  700;
  GSB[fpWinkel,Max] := 1200;
  GSB[fpVorstag,Min] := GSB[fpVorstag,Ist] - 10 * f;
  GSB[fpVorstag,Max] := GSB[fpVorstag,Ist] + 10 * f;
  GSB[fpWante,Min] := GSB[fpWante,Ist] - 10 * f;
  GSB[fpWante,Max] := GSB[fpWante,Ist] + 10 * f;
  GSB[fpWoben,Min] := GSB[fpWoben,Ist] - 10 * f;
  GSB[fpWoben,Max] := GSB[fpWoben,Ist] + 10 * f;
  GSB[fpSalingH,Min] := GSB[fpSalingH,Ist] - 10 * f;
  GSB[fpSalingH,Max] := GSB[fpSalingH,Ist] + 10 * f;
  GSB[fpSalingA,Min] :=  GSB[fpSalingA,Ist] - 10 * f;
  GSB[fpSalingA,Max] := GSB[fpSalingA,Ist] + 10 * f;
  GSB[fpSalingL,Min] := GSB[fpSalingL,Ist] - 10 * f;
  GSB[fpSalingL,Max] := GSB[fpSalingL,Ist] + 10 * f;
  GSB[fpVorstagOS,Min] := GSB[fpVorstagOS,Ist] - 10 * f;
  GSB[fpVorstagOS,Max] := GSB[fpVorstagOS,Ist] + 10 * f;
  GSB[fpWPowerOS,Min] := 100;
  GSB[fpWPowerOS,Max] := 3000;

  { TrimmTab.TrimmTabDaten := DefaultTrimmTabDaten; } { siehe RggTypes }
  with TrimmTabDaten do
  begin
    TabellenTyp := itGerade;
    a0 := 0; { zur Zeit nicht verwendet }
    a1 := 0.1;
    a2 := 0;
    x0 := 0; { zur Zeit nicht verwendet }
    x1 := 500;
    x2 := 1000;
  end;
end;

function TRggDocument.SignatureOK: Boolean;
var
  S, SCaption, S1: String;
begin
  result := False;
  if Signature = String(RggDocSignature) then
    result := True
  else
  begin
    SCaption := Format('Lesen von %s', [ExtractFileName(FFileName)]);
    S := 'FormatFehler';
    S1 := Copy(Signature, 1, 6);
    if S1 = 'RGGDOC' then
    begin
      S := S + #13 + 'vorliegend: Version ' + Signature;
      S := S + #13 + 'erforderlich: Version ' + String(RggDocSignature);
    end
    else
    begin
      S := S + #13 + 'Die Datei enthält kein gültiges';
      S := S + #13 + 'Rigg - Dokument.';
    end;
    Main.Logger.Info(Format(S, [SCaption]));
  end;
end;

procedure TRggDocument.DumpToMemo(Memo: TStrings);
var
  S1, S2: String;
  i, tempEI: Integer;
begin
  with Memo do
  begin
    Add('[Rigg]');
    Add(Format('SalingTyp=%d', [Ord(SalingTyp)]));
    Add(Format('ControllerTyp=%d', [Ord(ControllerTyp)]));
    Add(Format('CalcTyp=%d', [Ord(CalcTyp)]));
    Add('');

    Add('[Trimmtabelle]');
    with TrimmTabDaten do
    begin
      Add(Format('TabellenTyp=%d', [Ord(TabellenTyp)]));
      Add(Format('a0=%10.5f', [a0]));
      Add(Format('a1=%10.5f', [a1]));
      Add(Format('a2=%10.5f', [a2]));
      Add(Format('x0=%10.5f', [x0]));
      Add(Format('x1=%10.5f', [x1]));
      Add(Format('x2=%10.5f', [x2]));
    end;
    Add('');

    Add('[Mast]');
    Add(Format('MastL=%d', [FiMastL]));
    Add(Format('Mastunten=%d', [FiMastunten]));
    Add(Format('Mastoben=%d', [FiMastoben]));
    Add(Format('MastfallVorlauf=%d', [FiMastfallVorlauf]));
    tempEI := Round(EI / 1E6);
    Add(Format('EI=%d', [tempEI]));
    Add('');

    Add('[Ist]');
    Add(Format('Controller=%d',[GSB[fpController,Ist]]));
    Add(Format('Winkel=%d',[GSB[fpWinkel,Ist]]));
    Add(Format('Vorstag=%d',[GSB[fpVorstag,Ist]]));
    Add(Format('Wante=%d',[GSB[fpWante,Ist]]));
    Add(Format('Woben=%d',[GSB[fpWoben,Ist]]));
    Add(Format('SalingH=%d',[GSB[fpSalingH,Ist]]));
    Add(Format('SalingA=%d',[GSB[fpSalingA,Ist]]));
    Add(Format('SalingL=%d',[GSB[fpSalingL,Ist]]));
    Add(Format('VorstagOS=%d',[GSB[fpVorstagOS,Ist]]));
    Add(Format('WPowerOS=%d',[GSB[fpWPowerOS,Ist]]));
    Add('');

    Add('[Min]');
    Add(Format('Controller=%d',[GSB[fpController,Min]]));
    Add(Format('Winkel=%d',[GSB[fpWinkel,Min]]));
    Add(Format('Vorstag=%d',[GSB[fpVorstag,Min]]));
    Add(Format('Wante=%d',[GSB[fpWante,Min]]));
    Add(Format('Woben=%d',[GSB[fpWoben,Min]]));
    Add(Format('SalingH=%d',[GSB[fpSalingH,Min]]));
    Add(Format('SalingA=%d',[GSB[fpSalingA,Min]]));
    Add(Format('SalingL=%d',[GSB[fpSalingL,Min]]));
    Add(Format('VorstagOS=%d',[GSB[fpVorstagOS,Min]]));
    Add(Format('WPowerOS=%d',[GSB[fpWPowerOS,Min]]));
    Add('');

    Add('[Max]');
    Add(Format('Controller=%d',[GSB[fpController,Max]]));
    Add(Format('Winkel=%d',[GSB[fpWinkel,Max]]));
    Add(Format('Vorstag=%d',[GSB[fpVorstag,Max]]));
    Add(Format('Wante=%d',[GSB[fpWante,Max]]));
    Add(Format('Woben=%d',[GSB[fpWoben,Max]]));
    Add(Format('SalingH=%d',[GSB[fpSalingH,Max]]));
    Add(Format('SalingA=%d',[GSB[fpSalingA,Max]]));
    Add(Format('SalingL=%d',[GSB[fpSalingL,Max]]));
    Add(Format('VorstagOS=%d',[GSB[fpVorstagOS,Max]]));
    Add(Format('WPowerOS=%d',[GSB[fpWPowerOS,Max]]));
    Add('');

    Add('[Koordinaten Rumpf]');
    Add(Format('A0x=%d',[iP[ooA0,x]]));
    Add(Format('A0y=%d',[iP[ooA0,y]]));
    Add(Format('A0z=%d',[iP[ooA0,z]]));
    Add(Format('B0x=%d',[iP[ooB0,x]]));
    Add(Format('B0y=%d',[iP[ooB0,y]]));
    Add(Format('B0z=%d',[iP[ooB0,z]]));
    Add(Format('C0x=%d',[iP[ooC0,x]]));
    Add(Format('C0y=%d',[iP[ooC0,y]]));
    Add(Format('C0z=%d',[iP[ooC0,z]]));
    Add(Format('D0x=%d',[iP[ooD0,x]]));
    Add(Format('D0y=%d',[iP[ooD0,y]]));
    Add(Format('D0z=%d',[iP[ooD0,z]]));
    Add(Format('E0x=%d',[iP[ooE0,x]]));
    Add(Format('E0y=%d',[iP[ooE0,y]]));
    Add(Format('E0z=%d',[iP[ooE0,z]]));
    Add(Format('F0x=%d',[iP[ooF0,x]]));
    Add(Format('F0y=%d',[iP[ooF0,y]]));
    Add(Format('F0z=%d',[iP[ooF0,z]]));
    Add('');

    Add('[Koordinaten Rigg]');
    Add(Format('Ax=%d',[iP[ooA,x]]));
    Add(Format('Ay=%d',[iP[ooA,y]]));
    Add(Format('Az=%d',[iP[ooA,z]]));
    Add(Format('Bx=%d',[iP[ooB,x]]));
    Add(Format('By=%d',[iP[ooB,y]]));
    Add(Format('Bz=%d',[iP[ooB,z]]));
    Add(Format('Cx=%d',[iP[ooC,x]]));
    Add(Format('Cy=%d',[iP[ooC,y]]));
    Add(Format('Cz=%d',[iP[ooC,z]]));
    Add(Format('Dx=%d',[iP[ooD,x]]));
    Add(Format('Dy=%d',[iP[ooD,y]]));
    Add(Format('Dz=%d',[iP[ooD,z]]));
    Add(Format('Ex=%d',[iP[ooE,x]]));
    Add(Format('Ey=%d',[iP[ooE,y]]));
    Add(Format('Ez=%d',[iP[ooE,z]]));
    Add(Format('Fx=%d',[iP[ooF,x]]));
    Add(Format('Fy=%d',[iP[ooF,y]]));
    Add(Format('Fz=%d',[iP[ooF,z]]));
    Add('');

    Add('[EA]');
    for i := 0 to 19 do
    begin
      S1 := IntToStr(i);
      S2 := Format('%.6g', [rEA[i]]);
      Add(Format('%s=%s', [S1, S2]));
    end;
    Add('');
  end;
end;

procedure TRggDocument.WriteToDFM(Memo: TStrings);
var
  i, tempEI: Integer;
begin
  with Memo do
   begin
    // Rigg - diese Properties werden im Objektinspektor gesetzt
    // Add(Format('SalingTyp=%d',[Ord(SalingTyp)]));
    // Add(Format('ControllerTyp=%d',[Ord(ControllerTyp)]));
    // Add(Format('CalcTyp=%d',[Ord(CalcTyp)]));

    // Trimmtabelle
    with TrimmTabDaten do
    begin
      Add(Format('TabellenTyp= %d', [Ord(TabellenTyp)]));
      Add(Format('Ta0= %10.5f', [a0]));
      Add(Format('Ta1= %10.5f', [a1]));
      Add(Format('Ta2= %10.5f', [a2]));
      Add(Format('Tx0= %10.5f', [x0]));
      Add(Format('Tx1= %10.5f', [x1]));
      Add(Format('Tx2= %10.5f', [x2]));
    end;

    // Mast
    Add(Format('MastL= %d', [FiMastL]));
    Add(Format('Mastunten= %d', [FiMastunten]));
    Add(Format('Mastoben= %d', [FiMastoben]));
    Add(Format('MastfallVorlauf= %d', [FiMastfallVorlauf]));
    tempEI := Round(EI / 1E6);
    Add(Format('EI= %d', [tempEI]));

    //GSB
    Add(Format('%s= %6d %6d %6d', ['Controller',GSB[fpController,Min],GSB[fpController,Ist],GSB[fpController,Max]]));
    Add(Format('%s= %6d %6d %6d', ['Winkel',GSB[fpWinkel,Min],GSB[fpWinkel,Ist],GSB[fpWinkel,Max]]));
    Add(Format('%s= %6d %6d %6d', ['Vorstag',GSB[fpVorstag,Min],GSB[fpVorstag,Ist],GSB[fpVorstag,Max]]));
    Add(Format('%s= %6d %6d %6d', ['Wante',GSB[fpWante,Min],GSB[fpWante,Ist],GSB[fpWante,Max]]));
    Add(Format('%s= %6d %6d %6d', ['Woben',GSB[fpWoben,Min],GSB[fpWoben,Ist],GSB[fpWoben,Max]]));
    Add(Format('%s= %6d %6d %6d', ['SalingH',GSB[fpSalingH,Min],GSB[fpSalingH,Ist],GSB[fpSalingH,Max]]));
    Add(Format('%s= %6d %6d %6d', ['SalingA',GSB[fpSalingA,Min],GSB[fpSalingA,Ist],GSB[fpSalingA,Max]]));
    Add(Format('%s= %6d %6d %6d', ['SalingL',GSB[fpSalingL,Min],GSB[fpSalingL,Ist],GSB[fpSalingL,Max]]));
    Add(Format('%s= %6d %6d %6d', ['VorstagOS',GSB[fpVorstagOS,Min],GSB[fpVorstagOS,Ist],GSB[fpVorstagOS,Max]]));
    Add(Format('%s= %6d %6d %6d', ['WPowerOS',GSB[fpWPowerOS,Min],GSB[fpWPowerOS,Ist],GSB[fpWPowerOS,Max]]));

    //Koordinaten
    Add(Format('%s= %6d %6d %6d',['A0',iP[ooA0,x],iP[ooA0,y],iP[ooA0,z]]));
    Add(Format('%s= %6d %6d %6d',['B0',iP[ooB0,x],iP[ooB0,y],iP[ooB0,z]]));
    Add(Format('%s= %6d %6d %6d',['C0',iP[ooC0,x],iP[ooC0,y],iP[ooC0,z]]));
    Add(Format('%s= %6d %6d %6d',['D0',iP[ooD0,x],iP[ooD0,y],iP[ooD0,z]]));
    Add(Format('%s= %6d %6d %6d',['E0',iP[ooE0,x],iP[ooE0,y],iP[ooE0,z]]));
    Add(Format('%s= %6d %6d %6d',['F0',iP[ooF0,x],iP[ooF0,y],iP[ooF0,z]]));
    Add(Format('%s= %6d %6d %6d',['A', iP[ooA,x], iP[ooA,y], iP[ooA,z]]));
    Add(Format('%s= %6d %6d %6d',['B', iP[ooB,x], iP[ooB,y], iP[ooB,z]]));
    Add(Format('%s= %6d %6d %6d',['C', iP[ooC,x], iP[ooC,y], iP[ooC,z]]));
    Add(Format('%s= %6d %6d %6d',['D', iP[ooD,x], iP[ooD,y], iP[ooD,z]]));
    Add(Format('%s= %6d %6d %6d',['E', iP[ooE,x], iP[ooE,y], iP[ooE,z]]));
    Add(Format('%s= %6d %6d %6d',['F', iP[ooF,x], iP[ooF,y], iP[ooF,z]]));

    //EA
    for i := 0 to 19 do
      Add(Format('EA%d= %.6g',[i,rEA[i]]));
  end;
end;

procedure TRggDocument.ReadFromDFM(Memo: TStrings);
  procedure ReadGSB(c: TsbName; S: String);
  var
    S1: String;
  begin
    if S = '' then
      Exit;
    S := Trim(S);
    S1 := StripFirstWord(S);
    GSB[c, Min] := StrToInt(S1);
    S := Trim(S);
    S1 := StripFirstWord(S);
    GSB[c, Ist] := StrToInt(S1);
    GSB[c, Max] := StrToInt(S);
  end;
  procedure ReadKoord(k: TRiggPoints; S: String);
  var
    S1: String;
  begin
    if S = '' then
      Exit;
    S := Trim(S);
    S1 := StripFirstWord(S);
    iP[k,x] := StrToInt(S1);
    S := Trim(S);
    S1 := StripFirstWord(S);
    iP[k,y] := StrToInt(S1);
    iP[k,z] := StrToInt(S);
  end;
  procedure ReadInteger(S: String; var a: Integer);
  begin
    if S = '' then
      Exit;
    a :=  StrToInt(S);
  end;
  procedure ReadFloat(S: String; var a: double);
  begin
    if S = '' then
      Exit;
    a :=  StrToFloat(S);
  end;

var
  S: String;
  i, tempEI: Integer;
  T: TTrimmTabDaten;
begin
  with Memo do
  begin
    // Rigg - diese Properties werden im Objektinspektor gesetzt
    // SalingTyp := TSalingTyp(StrToInt(Values['SalingTyp']));
    // ControllerTyp := TControllerTyp(StrToInt(Values['ControllerTyp']));
    // CalcTyp := TCalcTyp(StrToInt(Values['CalcTyp']));

    // Trimmtabelle
    T := DefaultTrimmTabDaten;
    try
      S := Values['TabellenTyp'];
      if S <> '' then
        T.TabellenTyp := TTabellenTyp(StrToInt(S));
      S := Values['Ta0'];
      ReadFloat(S, T.a0);
      S := Values['Ta1'];
      ReadFloat(S, T.a1);
      S := Values['Ta2'];
      ReadFloat(S, T.a2);
      S := Values['Tx0'];
      ReadFloat(S, T.x0);
      S := Values['Tx1'];
      ReadFloat(S, T.x1);
      S := Values['Tx2'];
      ReadFloat(S, T.x2);
    except
      on EConvertError do
      begin
        Main.Logger.Info('DefaultTrimmTabDaten geladen');
        T := DefaultTrimmTabDaten;
      end;
    end;
    TrimmTabDaten := T;

    // Mast
    S := Values['MastL'];
    ReadInteger(S, FiMastL);
    S := Values['Mastunten'];
    ReadInteger(S, FiMastunten);
    S := Values['Mastoben'];
    ReadInteger(S, FiMastoben);
    S := Values['MastfallVorlauf'];
    ReadInteger(S, FiMastfallVorlauf);
    S := Values['EI'];
    ReadInteger(S, tempEI);
    if S <> '' then
      EI := tempEI * 1E6;

    // GSB (Min,Ist,Max)
    S := Values['Controller'];
    ReadGSB(fpController, S);
    S := Values['Winkel'];
    ReadGSB(fpWinkel, S);
    S := Values['Vorstag'];
    ReadGSB(fpVorstag, S);
    S := Values['Wante'];
    ReadGSB(fpWante, S);
    S := Values['Woben'];
    ReadGSB(fpWoben, S);
    S := Values['SalingH'];
    ReadGSB(fpSalingH, S);
    S := Values['SalingA'];
    ReadGSB(fpSalingA, S);
    S := Values['SalingL'];
    ReadGSB(fpSalingL, S);
    S := Values['VorstagOS'];
    ReadGSB(fpVorstagOS, S);
    S := Values['WPowerOS'];
    ReadGSB(fpWPowerOS, S);

    // Koordinaten (x,y,z)
    S := Values['A0'];
    ReadKoord(ooA0, S);
    S := Values['B0'];
    ReadKoord(ooB0, S);
    S := Values['C0'];
    ReadKoord(ooC0, S);
    S := Values['D0'];
    ReadKoord(ooD0, S);
    S := Values['E0'];
    ReadKoord(ooE0, S);
    S := Values['F0'];
    ReadKoord(ooF0, S);
    S := Values['A'];
    ReadKoord(ooA, S);
    S := Values['B'];
    ReadKoord(ooB, S);
    S := Values['C'];
    ReadKoord(ooC, S);
    S := Values['D'];
    ReadKoord(ooD, S);
    S := Values['E'];
    ReadKoord(ooE, S);
    S := Values['F'];
    ReadKoord(ooF, S);

    // EA
    for i := 0 to 19 do
    begin
      S := Values[Format('EA%d', [i])];
      if S <> '' then
        rEA[i] := StrToFloat(S);
    end;
  end;
end;

procedure TRggDocument.LoadFromString(s: string);
var
  Decoder: TIdDecoderMime;
  sDecoded: string;
  Stream: TStringStream;
  //Stream: TStream;
begin
  Decoder := TIdDecoderMime.Create(nil);
  sDecoded := Decoder.DecodeString(s);;
  Stream := TStringStream.Create(sDecoded);
  Stream.Seek(0, soFromBeginning);
  LoadFromStream(Stream);
  Stream.Free;
  Decoder.Free;
  {
  Stream := TMemoryStream.Create;
  Decoder := TIdDecoderMime.Create(nil);
  Decoder.DecodeToStream(s, Stream);
  Stream.Seek(0, soFromBeginning);
  LoadFromStream(Stream);
  Stream.Free;
  Decoder.Free;
  }
end;

function TRggDocument.SaveToString: string;
var
  Encoder: TIdEncoderMime;
  Stream: TStream;
begin
  Stream := TMemoryStream.Create;
  Encoder := TIdEncoderMime.Create(nil);
  SaveToStream(Stream);
  Stream.Seek(0, soFromBeginning);
  result := Encoder.Encode(Stream, Stream.Size);
  Stream.Free;
  Encoder.Free;
end;

function TRggDocument.SaveToXMLBase64: string;
var
  Encoder: TIdEncoderMime;
begin
  Encoder := TIdEncoderMime.Create(nil);
  result := Encoder.Encode(SaveToXML);
  Encoder.Free;
end;

function TRggDocument.LoadFromXMLBase64(s: string): string;
var
  Decoder: TIdDecoderMime;
begin
  Decoder := TIdDecoderMime.Create(nil);
  result := Decoder.DecodeString(s);
  Decoder.Free;
end;

procedure TRggDocument.LoadFromXML(s: string);
begin
end;

function TRggDocument.SaveToXML: string;
//var
//  i: Integer;
//  rp: TRiggPoints;
//  tempEI: double;
//  SBParam: TsbParam; // = (Ist, Min, Max, TinyStep, BigStep);
//  SBName: TsbName;
//  g: TXMLGenerator;
//  OldDecimalSeparator: Char;
begin
//  OldDecimalSeparator := DecimalSeparator;
//  g := TXMLGenerator.CreateWithEncoding(8 * 1024, encUTF_8);
//  try
//    g.StartTag('RggDoc');
//
//      { RiggType }
//      g.SetAttribute('SalingTyp', IntToStr(Ord(SalingTyp)));
//      g.SetAttribute('ControllerTyp', IntToStr(Ord(ControllerTyp)));
//      g.SetAttribute('CalcTyp', IntToStr(Ord(CalcTyp)));
//
//      { Koord }
//      g.StartTag('Koordinaten');
//        g.StartTag('Rumpf');
//          for rp := ooA0 to ooP0 do
//          begin
//          g.StartTag('Koord');
//          g.SetAttribute('ID', KoordTexteXML[rp]);
//          g.SetAttribute('Label', XMLKoordLabels[rp]);
//          g.SetAttribute('x', IntToStr(iP[rp][x]));
//          g.SetAttribute('y', IntToStr(iP[rp][y]));
//          g.SetAttribute('z', IntToStr(iP[rp][z]));
//          g.StopTag;
//          end;
//        g.StopTag;
//        g.StartTag('Rigg');
//          for rp := ooA to ooP do
//          begin
//          g.StartTag('Koord');
//          g.SetAttribute('ID', KoordTexteXML[rp]);
//          g.SetAttribute('Label', XMLKoordLabels[rp]);
//          g.SetAttribute('x', IntToStr(iP[rp][x]));
//          g.SetAttribute('y', IntToStr(iP[rp][y]));
//          g.SetAttribute('z', IntToStr(iP[rp][z]));
//          g.StopTag;
//          end;
//        g.StopTag;
//      g.StopTag;
//
//      { Mast: Abmessungen in mm }
//      g.StartTag('Mast');
//        g.StartTag('L');
//        g.SetAttribute('ID', 'D0F');
//        g.SetAttribute('Label', 'Mastfuss-Top');
//        g.AddData(IntToStr(FiMastL));
//        g.StopTag;
//
//        g.StartTag('L');
//        g.SetAttribute('ID', 'D0D');
//        g.SetAttribute('Label', 'Mastfuss-Saling');
//        g.AddData(IntToStr(FiMastunten));
//        g.StopTag;
//
//        g.StartTag('L');
//        g.SetAttribute('ID', 'DC');
//        g.SetAttribute('Label', 'Saling-Vorstag');
//        g.AddData(IntToStr(FiMastoben));
//        g.StopTag;
//
//        g.StartTag('Zusaetzlich');
//          g.StartTag('L');
//          g.SetAttribute('ID', 'FX');
//          g.SetAttribute('Label', 'MastfallVorlauf');
//          g.AddData(IntToStr(FiMastfallVorlauf));
//          g.StopTag;
//
//          g.StartTag('L');
//          g.SetAttribute('ID', 'C0X');
//          g.SetAttribute('Label', 'ControllerAnschlag');
//          g.AddData(IntToStr(FiControllerAnschlag));
//          g.StopTag;
//
//          //g.StartTag('L');
//          //g.SetAttribute('ID', 'Reserved');
//          //g.SetAttribute('Label', 'Reserved');
//          //g.AddData(IntToStr(FiReserved));
//          //g.StopTag;
//        g.StopTag;
//      g.StopTag;
//
//      { GSB - Grenzwerte und Istwerte }
//      g.StartTag('Scrollbar');
//        for SBName := Low(TsbName) to SalingL do
//        begin
//        g.StartTag('Param');
//        g.SetAttribute('ID', XMLSBName[SBName]);
//        g.SetAttribute('Label', XMLSBNameLabels[SBName]);
//        for SBParam := Low(TSBParam) to High(TSBParam) do
//        begin
//        g.SetAttribute(XMLSBParamLabels[SBParam], IntToStr(GSB[SBName, SBParam]));
//        end;
//        g.StopTag;
//        end;
//      g.StopTag;
//
//      DecimalSeparator := '.';
//
//      { Festigkeitswerte }
//      g.StartTag('Festigkeit');
//        g.StartTag('ZugDruck');
//          for i := 0 to 19 do
//          begin
//          g.StartTag('EA');
//          g.SetAttribute('Stab', IntToStr(i));
//          g.SetAttribute('Value', Format('%.6g', [rEA[i]]));
//          g.StopTag;
//          end;
//        g.StopTag;
//        g.StartTag('Biegung');
//          g.StartTag('EI');
//          tempEI := Round(EI/1E6);
//          g.SetAttribute('Label', 'Mast');
//          g.SetAttribute('Value', Format('%.6g', [tempEI]));
//          g.StopTag;
//        g.StopTag;
//      g.StopTag;
//
//      { Trimmtabelle }
//      g.StartTag('Trimmtabelle');
//      with TrimmTabDaten do
//        begin
//        g.SetAttribute('KurvenTyp', IntToStr(Ord(TabellenTyp)));
//        g.StartTag('T');
//        g.SetAttribute('ID', 'a0');
//        g.AddData(Format('%.6g',[a0]));
//        g.StopTag;
//        g.StartTag('T');
//        g.SetAttribute('ID', 'a1');
//        g.AddData(Format('%.6g',[a1]));
//        g.StopTag;
//        g.StartTag('T');
//        g.SetAttribute('ID', 'a2');
//        g.AddData(Format('%.6g',[a2]));
//        g.StopTag;
//        g.StartTag('T');
//        g.SetAttribute('ID', 'x0');
//        g.AddData(Format('%.6g',[x0]));
//        g.StopTag;
//        g.StartTag('T');
//        g.SetAttribute('ID', 'x1');
//        g.AddData(Format('%.6g',[x1]));
//        g.StopTag;
//        g.StartTag('T');
//        g.SetAttribute('ID', 'x2');
//        g.AddData(Format('%.6g',[x2]));
//        g.StopTag;
//      end;
//      g.StopTag;
//
//    g.StopTag;
//    result := g.AsUTF8;
//  finally
//    g.Free;
//    DecimalSeparator := OldDecimalSeparator;
//  end;
  result := '';
end;

end.
