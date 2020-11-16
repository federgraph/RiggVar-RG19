unit FrmOptions;

interface

uses
  Winapi.Windows, // because of VK_Return
  System.SysUtils,
  System.Classes,
  System.Types,
  System.UITypes,
  System.IniFiles,
  Vcl.Forms,
  Vcl.Controls,
  Vcl.Dialogs,
  Vcl.Graphics,
  Vcl.Grids,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.Buttons,
  Vcl.Mask,
  Vcl.Menus,
  Vcl.ComCtrls,
  RggScroll,
  RggUnit4,
  RggTypes,
  RggTrimmTab,
  RggTrimmTabGraph;

type
  TOptionForm = class(TForm)
    PageControl: TPageControl;
    tsTrimm: TTabSheet;
    LabelMin: TLabel;
    LabelPos: TLabel;
    LabelMax: TLabel;
    MinEdit: TEdit;
    PosEdit: TEdit;
    MaxEdit: TEdit;
    LengthEditLabel: TLabel;
    TrimmComboLabel: TLabel;
    TrimmCombo: TComboBox;
    tsFachwerk: TTabSheet;
    ElementLabel: TLabel;
    ElementCombo: TComboBox;
    EAEdit: TEdit;
    EAEditLabel: TLabel;
    TakeOverBtn: TButton;
    MaterialCombo: TComboBox;
    MaterialComboLabel: TLabel;
    QuerschnittComboLabel: TLabel;
    QuerschnittCombo: TComboBox;
    ALabel: TLabel;
    AEdit: TEdit;
    EEdit: TEdit;
    ELabel: TLabel;
    EEditLabel: TLabel;
    AEditLabel: TLabel;
    tsMast: TTabSheet;
    MastTypeComboLabel: TLabel;
    MastTypeCombo: TComboBox;
    MastMassComboLabel: TLabel;
    MastMassCombo: TComboBox;
    EIEdit: TEdit;
    MastMassEdit: TEdit;
    EILabel: TLabel;
    MassMassEditLabel: TLabel;
    tsRumpf: TTabSheet;
    GroupBoxRumpf: TGroupBox;
    RumpfLabel: TLabel;
    RumpfEdit: TEdit;
    RumpfBtn: TButton;
    Grid: TStringGrid;
    OKBtn: TButton;
    CancelBtn: TButton;
    tsTabelle: TTabSheet;
    RumpfSpinEdit: TUpDown;
    tsIniMemo: TTabSheet;
    IniMemo: TMemo;
    SaveIniBtn: TButton;
    LoadIniBtn: TButton;
    MemoLabel: TLabel;
    X1Label: TLabel;
    Y1Label: TLabel;
    X2Label: TLabel;
    Y2Label: TLabel;
    TrimmMemo: TMemo;
    UpDownKraft1: TUpDown;
    Kraft1Edit: TEdit;
    Weg2Edit: TEdit;
    Kraft2Edit: TEdit;
    Weg1Edit: TEdit;
    UpDownWeg1: TUpDown;
    rbKonstante: TRadioButton;
    rbGerade: TRadioButton;
    rbParabel: TRadioButton;
    rbBezier: TRadioButton;
    EvalOptionBtn: TSpeedButton;
    GroupBoxMast: TGroupBox;
    GroupBoxTrimm: TGroupBox;
    GroupBoxMaterial: TGroupBox;
    WriteMemoBtn: TSpeedButton;
    ReadMemoBtn: TSpeedButton;
    Image: TImage;
    procedure MastMassEditExit(Sender: TObject);
    procedure MastMassEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure MastTypeComboChange(Sender: TObject);
    procedure MastMassComboChange(Sender: TObject);
    procedure TrimmComboChange(Sender: TObject);
    procedure QuerschnittComboChange(Sender: TObject);
    procedure MaterialComboChange(Sender: TObject);
    procedure ElementComboChange(Sender: TObject);
    procedure StoreItemClick(Sender: TObject);
    procedure LoadItemClick(Sender: TObject);
    procedure TakeOverBtnClick(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure MinEditExit(Sender: TObject);
    procedure MinEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure GridSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
    procedure RumpfBtnClick(Sender: TObject);
    procedure RumpfSpinEditEnter(Sender: TObject);
    procedure RumpfSpinEditExit(Sender: TObject);
    procedure RumpfSpinEditChanging(Sender: TObject; var AllowChange: Boolean);
    procedure ReadMemoBtnClick(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure WriteMemoBtnClick(Sender: TObject);
    procedure EvalOptionBtnClick(Sender: TObject);
    procedure Kraft1EditChange(Sender: TObject);
    procedure rbKonstanteClick(Sender: TObject);
    procedure PaintBoxTabelleMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  private
    FiMastSaling: Integer;
    FiMastWante: Integer;
    FiMastTop: Integer;
    FiEI: Integer;
    FEAarray: TRiggLvektor;

    FMastTypList: TStringList;
    FMastMassList: TStringList;
    FElementList: TStringList;
    FMaterialList: TStringList;
    FQuerschnittList: TStringList;
    FTrimmList: TStringList;
    FTempList: TStringList;

    FGSB: TRggFA;
    FiP: TRiggPoints;
    FRumpfCell: TPoint;
    FTrimmTabDaten: TTrimmTabDaten;
    FTrimmTabelle: TTrimmTab;
    FTabellenTyp: TTabellenTyp;
    FTabChanging: Boolean;
    procedure GetKeyList(Source, Dest: TStringList);
    procedure FillInifileLists;
    procedure FillRiggLists;
    procedure LoadInifileCombos;
    procedure LoadRiggCombos;
    procedure CheckTabelle;
  public
    FirstColumnIndex: Integer;
    FirstRowIndex: Integer;
    SecondRowIndex: Integer;

    Rigg: TRigg;
    IniFileName: string;
    TrimmTabGraph: TTrimmTabGraph;
    procedure DrawTrimmTab;
    procedure Init;
    procedure LoadFromIniFile;
    procedure WriteToIniFile;
  end;

var
  OptionForm: TOptionForm;

implementation

{$R *.DFM}

uses
  RggStrings,
  RiggVar.App.Main,
  RiggVar.RG.Def;

procedure TOptionForm.GetKeyList(Source, Dest: TStringList);
var
  i: Integer;
  s: string;
begin
  Dest.Clear;
  for i := 0 to Source.Count - 1 do
  begin
    s := Copy(Source[i], 1, Pos('=', Source[i]) - 1);
    Dest.Add(s);
  end;
end;

procedure TOptionForm.FormCreate(Sender: TObject);
begin
  Rigg := Main.Rigg;
  FirstColumnIndex := 1;
  FirstRowIndex := 1; // 0 in FMX and 1 in VCL/LCL
  SecondRowIndex := FirstRowIndex + 1;

  FMastTypList := TStringList.Create;
  FMastMassList := TStringList.Create;
  FElementList := TStringList.Create;
  FMaterialList := TStringList.Create;
  FQuerschnittList := TStringList.Create;
  FTrimmList := TStringList.Create;
  FTempList := TStringList.Create;

  TrimmTabGraph := TTrimmTabGraph.Create;
  TrimmTabGraph.Image := Image;
  Init;
end;

procedure TOptionForm.Init;
begin
  IniFileName := ChangeFileExt(Application.ExeName, '.ini');

  FRumpfCell := Point(1, 1);
  Grid.Cells[0,0] := '';
  Grid.Cells[1,0] := '    x';
  Grid.Cells[2,0] := '    y';
  Grid.Cells[3,0] := '    z';
  Grid.Cells[0,1] := '   A0';
  Grid.Cells[0,2] := '   B0';
  Grid.Cells[0,3] := '   C0';
  Grid.Cells[0,4] := '   D0';
  Grid.Cells[0,5] := '   E0';
  Grid.Cells[0,6] := '   F0';
  Rumpflabel.Caption := 'Feld A0x';

  FTabellenTyp := itGerade;
  rbGerade.Checked := True;
  UpDownKraft1.Enabled := False;
  UpDownWeg1.Enabled := False;
  FillRiggLists;
  LoadRiggCombos;
  Assert(FTrimmTabelle <> nil);
  FTabellenTyp := FTrimmTabelle.TabellenTyp;
  FillIniFileLists;
  LoadIniFileCombos;
end;

procedure TOptionForm.FormDestroy(Sender: TObject);
begin
  FMastTypList.Free;
  FMastMassList.Free;
  FElementList.Free;
  FMaterialList.Free;
  FQuerschnittList.Free;
  FTrimmList.Free;
  FTempList.Free;

  TrimmTabGraph.Free;
end;

procedure TOptionForm.FillRiggLists;
begin
  FGSB := Rigg.GSB;
  FEAarray := Rigg.EA; { EA in KN }
  FiEI := Rigg.MastEI;
  FiMastSaling := Round(Rigg.MastUnten);
  FiMastWante := FiMastSaling + Round(Rigg.MastOben);
  FiMastTop := Round(Rigg.MastLaenge);
  FiP := Rigg.rP;
  FTrimmTabelle := Rigg.TrimmTab;
  FTrimmTabDaten := FTrimmTabelle.TrimmTabDaten;

  FMastMassList.Clear;
  FElementList.Clear;
  FTrimmList.Clear;

  FMastMassList.Add(Format('Saling=%d',[FiMastSaling]));
  FMastMassList.Add(Format('Wante=%d',[FiMastWante]));
  FMastMassList.Add(Format('Top=%d',[FiMastTop]));

  FElementList.Add(Format('Wanten=%.6g',[FEAarray.V[7]]));
  FElementList.Add(Format('Vorstag=%.6g',[FEAarray.V[14]]));
  FElementList.Add(Format('Mast=%.6g',[FEAarray.V[0]]));
  FElementList.Add(Format('Saling=%.6g',[FEAarray.V[9]]));
  FElementList.Add(Format('Saling-Verbindung=%.6g',[FEAarray.V[11]]));
  FElementList.Add(Format('Rumpfstäbe=%.6g',[FEAarray.V[1]]));

  FTrimmList.Add('Controller');
  FTrimmList.Add('Winkel');
  FTrimmList.Add('Vorstag');
  FTrimmList.Add('Wante');
  FTrimmList.Add('Wante oben');
  FTrimmList.Add('Saling Höhe');
  FTrimmList.Add('Saling Abstand');
  FTrimmList.Add('Saling Länge');

  CheckTabelle;
end;

procedure TOptionForm.CheckTabelle;
begin
  { wenn Tabelle außerhalb des Dialoges verändert wurde - d.h. neu eingelesen }
  if FTabellenTyp <> FTrimmTabelle.TabellenTyp then
    case FTrimmTabelle.TabellenTyp of
      { Checked ändern --> Click() wird aufgerufen }
      itKonstante: rbKonstante.Checked := True;
      itGerade: rbGerade.Checked := True;
      itParabel: rbParabel.Checked := True;
      itBezier: rbBezier.Checked := True;
    end;
  FTrimmTabelle.GetMemoLines(TrimmMemo.Lines);
  ReadMemoBtnClick(Self);
end;

procedure TOptionForm.FillInifileLists;
var
  ML: TStrings;
begin
  FMastTypList.Clear;
  FQuerschnittList.Clear;
  FMaterialList.Clear;

  if FileExists(InifileName) then
  begin
    LoadFromIniFile;
    if FMastTypList.Count = 0 then
      FMastTypList.Add('TestProfil=15000');
    if FQuerschnittList.Count = 0 then
      FQuerschnittList.Add('Rund D 4 mm=12.56');
    if FMaterialList.Count = 0 then
      FMaterialList.Add('Stahl=210');

    IniMemo.Lines.Clear;
    IniMemo.Lines.LoadFromFile(InifileName);

    Exit;
  end;

  { wenn Inifile nicht existiert dann Standardwerte laden }

  { EI in Nm^2 }
  ML := FMastTypList;
  ML.Add('PD=14700');
  ML.Add('PE=15000');
  ML.Add('PK=18000');

  { A in mm^2 }
  ML := FQuerschnittList;
  ML.Add('Rund D 4 mm=12.56');
  ML.Add('Rund D 10 mm=78.5');
  ML.Add('Profil=315');
  FQuerschnittList.Add('Faktor 100=100');

  { E in KN/mm^2 }
  ML := FMaterialList;
  ML.Add('Stahl=210');
  ML.Add('Niro=250');
  ML.Add('Alu=70');
  ML.Add('Kevlar=200');
  ML.Add('EAklein=10');
  ML.Add('EAmittel=100');
  ML.Add('EAgross=1000');

  ML := IniMemo.Lines;
  ML.Clear;
  ML.Add('[Profile]');
  ML.Add('Profil D=14700');
  ML.Add('Profil E=15000');
  ML.Add('Profil K=18000');
  ML.Add('');
  ML.Add('[Querschnitte]');
  ML.Add('Rund D 4 mm=12.56');
  ML.Add('Rund D 10 mm=78.5');
  ML.Add('Profil=315');
  ML.Add('Faktor 100=100');
  ML.Add('');
  ML.Add('[Material]');
  ML.Add('Stahl=210');
  ML.Add('Niro=250');
  ML.Add('Alu=70');
  ML.Add('Kevlar=200');
  ML.Add('EAklein=10');
  ML.Add('EAmittel=100');
  ML.Add('EAgross=1000');
end;

procedure TOptionForm.LoadRiggCombos;
var
  m: TRiggPoint;
  n: Integer;
  c, r: Integer;
begin
  { Trimm }
  TrimmCombo.Items := FTrimmList;
  TrimmCombo.ItemIndex := Ord(fpWante);
  MinEdit.Text := IntToStr(Round(FGSB.Wante.Min));
  PosEdit.Text := IntToStr(Round(FGSB.Wante.Ist));
  MaxEdit.Text := IntToStr(Round(FGSB.Wante.Max));

  { elements }
  GetKeyList(FElementList, FTempList);
  ElementCombo.Items := FTempList;
  ElementCombo.ItemIndex := 0;
  EAEdit.Text := FElementList.Values[ElementCombo.Text];

  { mast length values }
  GetKeyList(FMastMassList, FTempList);
  MastMassCombo.Items := FTempList;
  MastMassCombo.ItemIndex := 0;
  MastMassEdit.Text := FMastMassList.Values[MastMassCombo.Text];

  { hull coordinates }
  r := FirstRowIndex - 1;
  for m := ooA0 to ooF0 do
  begin
    Inc(r);
    for n := 0 to 2 do
    begin
      c := Ord(n) + 1;
      Grid.Cells[c, r] := Format('%4.0f', [FiP.V[m].V[n]]);
    end;
  end;
end;

procedure TOptionForm.LoadIniFileCombos;
var
  i, j: Integer;
begin
  { Material }
  GetKeyList(FMaterialList, FTempList);
  MaterialCombo.Items := FTempList;
  MaterialCombo.ItemIndex := 0;
  EEdit.Text := FMaterialList.Values[MaterialCombo.Text];

  { Querschnitt }
  GetKeyList(FQuerschnittList, FTempList);
  QuerschnittCombo.Items := FTempList;
  QuerschnittCombo.ItemIndex := 0;
  AEdit.Text := FQuerschnittList.Values[QuerschnittCombo.Text];

  { MastTyp }
  GetKeyList(FMastTypList, FTempList);
  MastTypeCombo.Items := FTempList;
  j := 0;
  for i := 0 to FMastTypList.Count - 1 do
    if IntToStr(FiEI) = FMastTypList.Values[MastTypeCombo.Items[i]] then
      j := i;
  MastTypeCombo.ItemIndex := j;
  EIEdit.Text := FMastTypList.Values[MastTypeCombo.Text];
end;

procedure TOptionForm.LoadFromIniFile;
var
  IniFile: TMemIniFile;
begin
  IniFile := TMemIniFile.Create(IniFileName);
  try
    FMaterialList.Clear;
    IniFile.ReadSectionValues('Material', FMaterialList);
    FQuerschnittList.Clear;
    IniFile.ReadSectionValues('Querschnitte', FQuerschnittList);
    FMastTypList.Clear;
    IniFile.ReadSectionValues('Profile', FMastTypList);
  finally
    IniFile.Free;
  end;
end;

procedure TOptionForm.WriteToIniFile;
begin
  IniMemo.Lines.SaveToFile(InifileName);
end;

procedure TOptionForm.LoadItemClick(Sender: TObject);
var
  s: string;
begin
  if FileExists(InifileName) then
  begin
    LoadFromIniFile;
    LoadInifileCombos;
    IniMemo.Lines.Clear;
    IniMemo.Lines.LoadFromFile(InifileName);
  end
  else
  begin
    s := ExtractFileName(InifileName);
    s := s + 'nicht gefunden';
    MessageDlg(s, mtInformation, [mbOK], 0);
  end;
end;

procedure TOptionForm.StoreItemClick(Sender: TObject);
begin
  WriteToIniFile;
end;

procedure TOptionForm.TakeOverBtnClick(Sender: TObject);
var
  s: string;
  a, b, c: double;
begin
  { Values loaded from ini-file did contain wrong decimal separator}
  s := StringReplace(EEdit.Text, ',', '.', []);
  a := StrToFloat(s);
  s := StringReplace(AEdit.Text, ',', '.', []);
  b := StrToFloat(s);
  c := a * b;
  EAEdit.Text := Format('%.6g', [c]);

  s := ElementCombo.Text;
  if s = '' then
    Exit;

  if s = 'Rumpflängen' then
  begin
    FEAarray.V[1] := c;
    FEAarray.V[2] := c;
    FEAarray.V[3] := c;
    FEAarray.V[4] := c;
    FEAarray.V[5] := c;
    FEAarray.V[6] := c;
    end
  else if s = 'Wanten' then
  begin
    FEAarray.V[7] := c;
    FEAarray.V[8] := c;
    FEAarray.V[12] := c;
    FEAarray.V[13] := c;
  end
  else if s = 'Vorstag' then
    FEAarray.V[14] := c
  else if s = 'Saling' then
  begin
    FEAarray.V[9] := c;
    FEAarray.V[10] := c;
  end
  else if s = 'Saling-Verbindung' then
    FEAarray.V[11] := c;

  FElementList.Values[s] := EAEdit.Text;
end;

procedure TOptionForm.TrimmComboChange(Sender: TObject);
var
  i: TFederParam;
  f: TRggSB;
begin
  i := TFederParam(TrimmCombo.ItemIndex);
  if i = TFederParam.fpWinkel then
    LengthEditLabel.Caption := 'Winkel in Grad'
  else
    LengthEditLabel.Caption := 'Abmessungen in mm';

  f := FGSB.Find(i);
  MinEdit.Text := IntToStr(Round(f.Min));
  PosEdit.Text := IntToStr(Round(f.Ist));
  MaxEdit.Text := IntToStr(Round(f.Max));
end;

procedure TOptionForm.MinEditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_Return then
    MinEditExit(Sender);
end;

procedure TOptionForm.MinEditExit(Sender: TObject);
var
  i: Integer;
  f: TRggSB;
  iMin, iIst, iMax: Integer;
  iVar: Integer;
begin
  i := TrimmCombo.ItemIndex;
  f := FGSB.Find(TFederParam(i));
  iMin := Round(f.Min);
  iIst := Round(f.Ist);
  iMax := Round(f.Max);

  if Sender = MinEdit then
  begin
    iVar := StrToIntDef(TEdit(Sender).Text, iMin);
    if iVar > iIst then
      iVar := iIst;
    f.Min := iVar;
    MinEdit.Text := IntToStr(iVar);
  end;

  if Sender = PosEdit then
  begin
    iVar := StrToIntDef(TEdit(Sender).Text, iIst);
    if iVar < iMin then
      iVar := iMin;
    if iVar > iMax then
      iVar := iMax;
    f.Ist := iVar;
    PosEdit.Text := IntToStr(iVar);
  end;

  if Sender = MaxEdit then
  begin
    iVar := StrToIntDef(TEdit(Sender).Text, iMax);
    if iVar < iIst then
      iVar := iIst;
    f.Max := iVar;
    MaxEdit.Text := IntToStr(iVar);
  end;
end;

procedure TOptionForm.MastMassEditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_Return then
    MastMassEditExit(Sender);
end;

procedure TOptionForm.MastMassEditExit(Sender: TObject);
var
  i: Integer;
  temp: Integer;
  s: string;
begin
  s := MastMassCombo.Text;
  if s = '' then
    Exit;

  temp := 0;
  if s = 'Saling' then
    temp := FiMastSaling
  else if s = 'Wante' then
    temp := FiMastWante
  else if s = 'Top' then
    temp := FiMastTop;

  i := StrToIntDef(MastMassEdit.Text, temp);
  MastMassEdit.Text := IntToStr(i);

  FMastMassList.Values[s] := IntToStr(i);
  if s = 'Saling' then
    FiMastSaling := i
  else if s = 'Wante' then
    FiMastWante := i
  else if s = 'Top' then
    FiMastTop := i;
end;

procedure TOptionForm.OKBtnClick(Sender: TObject);
begin
  Rigg.rP := FiP; { Rumpfkoordinaten}
  Rigg.MastUnten := FiMastSaling;
  Rigg.MastOben := FiMastWante - FiMastSaling;
  Rigg.MastLaenge := FiMastTop;
  Rigg.GSB := FGSB; { neue Grenzen und Istwerte }
  Rigg.EA := FEAarray;
  Rigg.MastEI := FiEI;
  FTabellenTyp := FTrimmTabelle.TabellenTyp;
end;

procedure TOptionForm.CancelBtnClick(Sender: TObject);
begin
  { wenn TabellenTyp verändert wurde }
  if FTabellenTyp <> FTrimmTabelle.TabellenTyp then
    case FTabellenTyp of
      { Checked ändern --> Click() wird aufgerufen }
      itKonstante: rbKonstante.Checked := True;
      itGerade: rbGerade.Checked := True;
      itParabel: rbParabel.Checked := True;
      itBezier: rbBezier.Checked := True;
    end;
  FTrimmTabelle.TrimmTabDaten := FTrimmTabDaten; { wiederherstellen }
end;

procedure TOptionForm.MastTypeComboChange(Sender: TObject);
begin
  EIEdit.Text := FMastTypList.Values[MastTypeCombo.Text];
  FiEI := StrToInt(EIEdit.Text);
end;

procedure TOptionForm.MastMassComboChange(Sender: TObject);
begin
  MastMassEdit.Text := FMastMassList.Values[MastMassCombo.Text];
end;

procedure TOptionForm.QuerschnittComboChange(Sender: TObject);
begin
  AEdit.Text := FQuerschnittList.Values[QuerschnittCombo.Text];
end;

procedure TOptionForm.MaterialComboChange(Sender: TObject);
begin
  EEdit.Text := FMaterialList.Values[MaterialCombo.Text];
end;

procedure TOptionForm.ElementComboChange(Sender: TObject);
begin
  EAEdit.Text := FElementList.Values[ElementCombo.Text];
end;

procedure TOptionForm.FormShow(Sender: TObject);
begin
  FillRiggLists;
  LoadRiggCombos;
  RumpfSpinEdit.Position := StrToIntDef(Grid.Cells[FRumpfCell.X,FRumpfCell.Y], 0);
  RumpfEdit.Text := Format('%4d mm', [RumpfSpinEdit.Position]);
end;

procedure TOptionForm.GridSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
var
  sRowHeaderText, sColHeaderText, sCellText: string;
begin
  CanSelect := True;

  if (ACol = 2) and (ARow > SecondRowIndex) then
    CanSelect := False;

  if CanSelect then
  begin
    FRumpfCell := Point(ACol, ARow);

    sRowHeaderText := TrimLeft(Grid.Cells[0, ARow]);
    sColHeaderText := TrimLeft(Grid.Cells[ACol, 0]);
    sCellText := Grid.Cells[ACol, ARow];

    RumpfLabel.Caption := Format('%s %s%s:', [FieldString, sRowHeaderText, sColHeaderText]);
    if RumpfSpinEdit <> nil then
    begin
      RumpfSpinEdit.Position := StrToIntDef(sCellText, 0);
      RumpfEdit.Text := Format('%4d mm', [RumpfSpinEdit.Position]);
    end;
  end;
end;

procedure TOptionForm.RumpfBtnClick(Sender: TObject);
var
  oo: TRiggPoint;
  kk: Integer;
begin
  oo := TRiggPoint(FRumpfCell.Y - FirstRowIndex + Ord(ooA0));
  kk := FRumpfCell.X - FirstColumnIndex;

  FiP.V[oo].V[kk] :=  RumpfSpinEdit.Position;
  Grid.Cells[FRumpfCell.X, FRumpfCell.Y] := Format('%4d', [RumpfSpinEdit.Position]);
  if FRumpfCell.Y = SecondRowIndex then
  begin
    FiP.V[ooA0] := FiP.V[ooB0];
    FiP.V[ooA0].V[1] := -FiP.V[ooB0].V[1];
    Grid.Cells[1, FirstRowIndex] := Format('%4.0f', [FiP.A0.X]);
    Grid.Cells[2, FirstRowIndex] := Format('%4.0f', [FiP.A0.Y]);
    Grid.Cells[3, FirstRowIndex] := Format('%4.0f', [FiP.A0.Z]);
  end;
  if FRumpfCell.Y = FirstRowIndex then
  begin
    FiP.B0 := FiP.A0;
    FiP.B0.Y := -FiP.A0.Y;
    Grid.Cells[1, SecondRowIndex] := Format('%4.0f', [FiP.B0.X]);
    Grid.Cells[2, SecondRowIndex] := Format('%4.0f', [FiP.B0.Y]);
    Grid.Cells[3, SecondRowIndex] := Format('%4.0f', [FiP.B0.Z]);
  end;
end;

procedure TOptionForm.RumpfSpinEditChanging(Sender: TObject;
  var AllowChange: Boolean);
begin
  RumpfEdit.Text := Format('%4d mm',[RumpfSpinEdit.Position]);
end;

procedure TOptionForm.RumpfSpinEditEnter(Sender: TObject);
begin
  RumpfEdit.Color := clWindow;
end;

procedure TOptionForm.RumpfSpinEditExit(Sender: TObject);
begin
  RumpfEdit.Color := clBtnFace;
end;

procedure TOptionForm.ReadMemoBtnClick(Sender: TObject);
var
  Temp: TPoint;
begin
  Assert(FTrimmTabelle <> nil);
  FTrimmTabelle.ProcessTrimmTab(TrimmMemo.Lines);
  UpDownKraft1.Increment := FTrimmTabelle.EndwertKraft div 30 + 1;
  Weg2Edit.Text := IntToStr(FTrimmTabelle.EndwertWeg);
  Kraft2Edit.Text := IntToStr(FTrimmTabelle.EndwertKraft);
  Temp := FTrimmTabelle.MittelPunkt;
  { Temp ist notwendig, siehe KraftEditChange,
    Weg1Edit ist noch nicht gesetzt und verfälscht sonst den Mittelunkt,
    auch umgekehrt. }
  FTabChanging := True;
  Kraft1Edit.Text := IntToStr(Temp.x);
  Weg1Edit.Text := IntToStr(Temp.y);
  FTabChanging := False;
  DrawTrimmTab;
end;

procedure TOptionForm.Kraft1EditChange(Sender: TObject);
var
  Temp: TPoint;
begin
  Assert(FTrimmTabelle <> nil);
  Temp.x := StrToInt(Kraft1Edit.Text);
  Temp.y := StrToInt(Weg1Edit.Text);
  FTrimmTabelle.MittelPunkt := Temp; { Temp ist ein Vorschlag für neuen MittelPunkt }
  Temp := FTrimmTabelle.MittelPunkt; { Temp ist jetzt überprüft und ev. korrigiert }
  Kraft1Edit.Text := IntToStr(Temp.x);
  Weg1Edit.Text := IntToStr(Temp.y);
  if not FTabChanging then
    DrawTrimmTab;
end;

procedure TOptionForm.PaintBoxTabelleMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  tempX, tempY: double;
begin
  tempY := Image.Height;
  tempY := (tempY -Y) * FTrimmTabelle.EndwertKraft / tempY;
  tempX := X * FTrimmTabelle.EndwertWeg / Image.Width;
  FTabChanging := True;
  Kraft1Edit.Text := IntToStr(Round(tempY));
  Weg1Edit.Text := IntToStr(Round(tempX));
  FTabChanging := False;
  DrawTrimmTab;
end;

procedure TOptionForm.rbKonstanteClick(Sender: TObject);
begin
  if Sender = rbKonstante then
  begin
    { rbKonstante.Checked := True; }
    UpDownKraft1.Enabled := True;
    UpDownWeg1.Enabled := False;
  end
  else if Sender = rbGerade then
  begin
    { rbGerade.Checked := True; }
    UpDownKraft1.Enabled := False;
    UpDownWeg1.Enabled := False;
  end
  else if Sender = rbParabel then
  begin
    { rbParabel.Checked := True; }
    UpDownKraft1.Enabled := True;
    UpDownWeg1.Enabled := False;
  end
  else if Sender = rbBezier then
  begin
    { rbBezier.Checked := True; }
    UpDownKraft1.Enabled := True;
    UpDownWeg1.Enabled := True;
  end;
  Assert(FTrimmTabelle <> nil);
  FTrimmTabelle.TabellenTyp := TTabellenTyp((Sender as TRadioButton).Tag);
  DrawTrimmTab;
end;

procedure TOptionForm.WriteMemoBtnClick(Sender: TObject);
begin
  FTrimmTabelle.GetMemoLines(TrimmMemo.Lines);
end;

procedure TOptionForm.EvalOptionBtnClick(Sender: TObject);
begin
  FTrimmTabelle.EvalDirection := EvalOptionBtn.Down;
end;

procedure TOptionForm.DrawTrimmTab;
begin
  FTrimmTabelle.UpdateGraphModel(TrimmTabGraph.Model);
  TrimmTabGraph.Draw;
end;

end.
