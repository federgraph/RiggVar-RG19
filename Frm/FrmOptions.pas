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
    RumpfGrid: TStringGrid;
    OKBtn: TButton;
    CancelBtn: TButton;
    tsTabelle: TTabSheet;
    RumpfSpinEdit: TUpDown;
    tsIniMemo: TTabSheet;
    InifileMemo: TMemo;
    Speichern: TButton;
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
    procedure RumpfGridSelectCell(Sender: TObject; Col, Row: Integer; var CanSelect: Boolean);
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

    FMastTypListe: TStringList;
    FMastMassListe: TStringList;
    FElementListe: TStringList;
    FMaterialListe: TStringList;
    FQuerschnittListe: TStringList;
    FTrimmListe: TStringList;
    FTempListe: TStringList;

    FGSB: TRggFA;
    FiP: TIntRiggPoints;
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
    Rigg: TRigg;
    IniFileName: string;
    TrimmTabGraph: TTrimmTabGraph;
    procedure DrawTrimmTab;
    procedure Init(ARigg: TRigg);
    procedure LoadFromIniFile;
    procedure WriteToIniFile;
  end;

var
  OptionForm: TOptionForm;

implementation

{$R *.DFM}

uses
  RiggVar.RG.Def,
  RggModul;

procedure TOptionForm.GetKeyList(Source, Dest: TStringList);
var
  i: Integer;
  S: string;
begin
  Dest.Clear;
  for i := 0 to Source.Count - 1 do
  begin
    S := Copy(Source[i], 1, Pos('=', Source[i]) - 1);
    Dest.Add(S);
  end;
end;

procedure TOptionForm.FormCreate(Sender: TObject);
begin
  FMastTypListe := TStringList.Create;
  FMastMassListe := TStringList.Create;
  FElementListe := TStringList.Create;
  FMaterialListe := TStringList.Create;
  FQuerschnittListe := TStringList.Create;
  FTrimmListe := TStringList.Create;
  FTempListe := TStringList.Create;

  TrimmTabGraph := TTrimmTabGraph.Create;
  TrimmTabGraph.Image := Image;

  Init(RiggModul.Rigg);
end;

procedure TOptionForm.Init(ARigg: TRigg);
begin
  Rigg := ARigg;
  InifileName := ChangeFileExt(Application.ExeName,'.ini');
  { IniFileName := ChangeFileExt(ParamStr(0), '.ini'); } { Alternative }

  FRumpfCell := Point(1, 1);
  RumpfGrid.Cells[0,0] := '';
  RumpfGrid.Cells[1,0] := '    x';
  RumpfGrid.Cells[2,0] := '    y';
  RumpfGrid.Cells[3,0] := '    z';
  RumpfGrid.Cells[0,1] := '   A0';
  RumpfGrid.Cells[0,2] := '   B0';
  RumpfGrid.Cells[0,3] := '   C0';
  RumpfGrid.Cells[0,4] := '   D0';
  RumpfGrid.Cells[0,5] := '   E0';
  RumpfGrid.Cells[0,6] := '   F0';
  Rumpflabel.Caption := 'Feld A0x';

  FTabellenTyp := itGerade;
  rbGerade.Checked := True;
  UpDownKraft1.Enabled := False;
  UpDownWeg1.Enabled := False;
  FillRiggLists;
  LoadRiggCombos;
  Assert(FTrimmTabelle <> nil);
  FTabellenTyp := FTrimmTabelle.TabellenTyp;
  FillInifileLists;
  LoadInifileCombos;
end;

procedure TOptionForm.FormDestroy(Sender: TObject);
begin
  FMastTypListe.Free;
  FMastMassListe.Free;
  FElementListe.Free;
  FMaterialListe.Free;
  FQuerschnittListe.Free;
  FTrimmListe.Free;
  FTempListe.Free;

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
  FiP := Rigg.iP;
  FTrimmTabelle := Rigg.TrimmTab;
  FTrimmTabDaten := FTrimmTabelle.TrimmTabDaten;

  FMastMassListe.Clear;
  FElementListe.Clear;
  FTrimmListe.Clear;

  FMastMassListe.Add(Format('Saling=%d',[FiMastSaling]));
  FMastMassListe.Add(Format('Wante=%d',[FiMastWante]));
  FMastMassListe.Add(Format('Top=%d',[FiMastTop]));

  FElementListe.Add(Format('Wanten=%.6g',[FEAarray[7]]));
  FElementListe.Add(Format('Vorstag=%.6g',[FEAarray[14]]));
  FElementListe.Add(Format('Mast=%.6g',[FEAarray[0]]));
  FElementListe.Add(Format('Saling=%.6g',[FEAarray[9]]));
  FElementListe.Add(Format('Saling-Verbindung=%.6g',[FEAarray[11]]));
  FElementListe.Add(Format('Rumpfstäbe=%.6g',[FEAarray[1]]));

  FTrimmListe.Add('Controller');
  FTrimmListe.Add('Winkel');
  FTrimmListe.Add('Vorstag');
  FTrimmListe.Add('Wante');
  FTrimmListe.Add('Wante oben');
  FTrimmListe.Add('Saling Höhe');
  FTrimmListe.Add('Saling Abstand');
  FTrimmListe.Add('Saling Länge');

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
  FMastTypListe.Clear;
  FQuerschnittListe.Clear;
  FMaterialListe.Clear;

  if FileExists(InifileName) then
  begin
    LoadFromIniFile;
    if FMastTypListe.Count = 0 then
      FMastTypListe.Add('TestProfil=15000');
    if FQuerschnittListe.Count = 0 then
      FQuerschnittListe.Add('Rund D 4 mm=12,56');
    if FMaterialListe.Count = 0 then
      FMaterialListe.Add('Stahl=210');

    InifileMemo.Lines.Clear;
    InifileMemo.Lines.LoadFromFile(InifileName);

    Exit;
  end;

  { wenn Inifile nicht existiert dann Standardwerte laden }

  { EI in Nm^2 }
  ML := FMastTypListe;
  ML.Add('PD=14700');
  ML.Add('PE=15000');
  ML.Add('PK=18000');

  { A in mm^2 }
  ML := FQuerschnittListe;
  ML.Add('Rund D 4 mm=12,56');
  ML.Add('Rund D 10 mm=78,5');
  ML.Add('Profil=315');
  FQuerschnittListe.Add('Faktor 100=100');

  { E in KN/mm^2 }
  ML := FMaterialListe;
  ML.Add('Stahl=210');
  ML.Add('Niro=250');
  ML.Add('Alu=70');
  ML.Add('Kevlar=200');
  ML.Add('EAklein=10');
  ML.Add('EAmittel=100');
  ML.Add('EAgross=1000');

  ML := InifileMemo.Lines;
  ML.Clear;
  ML.Add('[Profile]');
  ML.Add('Profil D=14700');
  ML.Add('Profil E=15000');
  ML.Add('Profil K=18000');
  ML.Add('');
  ML.Add('[Querschnitte]');
  ML.Add('Rund D 4 mm=12,56');
  ML.Add('Rund D 10 mm=78,5');
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
  n: TKoord;
begin
  { Trimm }
  TrimmCombo.Items := FTrimmListe;
  TrimmCombo.ItemIndex := Ord(fpWante);
  MinEdit.Text := IntToStr(Round(FGSB.Wante.Min));
  PosEdit.Text := IntToStr(Round(FGSB.Wante.Ist));
  MaxEdit.Text := IntToStr(Round(FGSB.Wante.Max));

  { Elemente }
  GetKeyList(FElementListe, FTempListe);
  ElementCombo.Items := FTempListe;
  ElementCombo.ItemIndex := 0;
  EAEdit.Text := FElementListe.Values[ElementCombo.Text];

  { MastMaße }
  GetKeyList(FMastMassListe, FTempListe);
  MastMassCombo.Items := FTempListe;
  MastMassCombo.ItemIndex := 0;
  MastMassEdit.Text := FMastMassListe.Values[MastMassCombo.Text];

  { Werte in FiP im StringGrid anzeigen }
  for m := ooA0 to ooF0 do
    for n := x to z do
      RumpfGrid.Cells[Ord(n) + 1, Ord(m)] := Format('%4.0f', [FiP[m,n]]);
end;

procedure TOptionForm.LoadIniFileCombos;
var
  i, j: Integer;
begin
  { Material }
  GetKeyList(FMaterialListe, FTempListe);
  MaterialCombo.Items := FTempListe;
  MaterialCombo.ItemIndex := 0;
  EEdit.Text := FMaterialListe.Values[MaterialCombo.Text];

  { Querschnitt }
  GetKeyList(FQuerschnittListe, FTempListe);
  QuerschnittCombo.Items := FTempListe;
  QuerschnittCombo.ItemIndex := 0;
  AEdit.Text := FQuerschnittListe.Values[QuerschnittCombo.Text];

  { MastTyp }
  GetKeyList(FMastTypListe, FTempListe);
  MastTypeCombo.Items := FTempListe;
  j := 0;
  for i := 0 to FMastTypListe.Count - 1 do
    if IntToStr(FiEI) = FMastTypListe.Values[MastTypeCombo.Items[i]] then
      j := i;
  MastTypeCombo.ItemIndex := j;
  EIEdit.Text := FMastTypListe.Values[MastTypeCombo.Text];
end;

procedure TOptionForm.LoadFromIniFile;
var
  IniFile: TMemIniFile;
begin
  IniFile := TMemIniFile.Create(IniFileName);
  try
    FMaterialListe.Clear;
    IniFile.ReadSectionValues('Material', FMaterialListe);
    FQuerschnittListe.Clear;
    IniFile.ReadSectionValues('Querschnitte', FQuerschnittListe);
    FMastTypListe.Clear;
    IniFile.ReadSectionValues('Profile', FMastTypListe);
  finally
    IniFile.Free;
  end;
end;

procedure TOptionForm.WriteToIniFile;
begin
  InifileMemo.Lines.SaveToFile(InifileName);
end;

procedure TOptionForm.LoadItemClick(Sender: TObject);
var
  S: string;
begin
  if FileExists(InifileName) then
  begin
    LoadFromIniFile;
    LoadInifileCombos;
    InifileMemo.Lines.Clear;
    InifileMemo.Lines.LoadFromFile(InifileName);
  end
  else
  begin
    S := ExtractFileName(InifileName);
    S := S + 'nicht gefunden';
    MessageDlg(S, mtInformation, [mbOK], 0);
  end;
end;

procedure TOptionForm.StoreItemClick(Sender: TObject);
begin
  WriteToIniFile;
end;

procedure TOptionForm.TakeOverBtnClick(Sender: TObject);
var
  a, b, c: double;
begin
  a := StrToFloat(EEdit.Text);
  b := StrToFloat(AEdit.Text);
  c := a * b;
  if ElementCombo.Text = 'Rumpflängen' then
  begin
    FEAarray[1] := c;
    FEAarray[2] := c;
    FEAarray[3] := c;
    FEAarray[4] := c;
    FEAarray[5] := c;
    FEAarray[6] := c;
    end
  else if ElementCombo.Text = 'Wanten' then
  begin
    FEAarray[7] := c;
    FEAarray[8] := c;
    FEAarray[12] := c;
    FEAarray[13] := c;
  end
  else if ElementCombo.Text = 'Vorstag' then
    FEAarray[14] := c
  else if ElementCombo.Text = 'Saling' then
  begin
    FEAarray[9] := c;
    FEAarray[10] := c;
  end
  else if ElementCombo.Text = 'Saling-Verbindung' then
    FEAarray[11] := c;
  EAEdit.Text := Format('%.6g',[c]);
  FElementListe.Values[ElementCombo.Text] := EAEdit.Text;
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
  temp := 0;

  s := MastMassCombo.Text;
  if s = 'Saling' then
    temp := FiMastSaling
  else if s = 'Wante' then
    temp := FiMastWante
  else if s = 'Top' then
    temp := FiMastTop;

  i := StrToIntDef(MastMassEdit.Text, temp);
  MastMassEdit.Text := IntToStr(i);

  FMastMassListe.Values[s] := IntToStr(i);
  if s = 'Saling' then
    FiMastSaling := i
  else if s = 'Wante' then
    FiMastWante := i
  else if s = 'Top' then
    FiMastTop := i;
end;

procedure TOptionForm.OKBtnClick(Sender: TObject);
begin
  Rigg.iP := FiP; { Rumpfkoordinaten}
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
  EIEdit.Text := FMastTypListe.Values[MastTypeCombo.Text];
  FiEI := StrToInt(EIEdit.Text);
end;

procedure TOptionForm.MastMassComboChange(Sender: TObject);
begin
  MastMassEdit.Text := FMastMassListe.Values[MastMassCombo.Text];
end;

procedure TOptionForm.QuerschnittComboChange(Sender: TObject);
begin
  AEdit.Text := FQuerschnittListe.Values[QuerschnittCombo.Text];
end;

procedure TOptionForm.MaterialComboChange(Sender: TObject);
begin
  EEdit.Text := FMaterialListe.Values[MaterialCombo.Text];
end;

procedure TOptionForm.ElementComboChange(Sender: TObject);
begin
  EAEdit.Text := FElementListe.Values[ElementCombo.Text];
end;

procedure TOptionForm.FormShow(Sender: TObject);
begin
  FillRiggLists;
  LoadRiggCombos;
  RumpfSpinEdit.Position := StrToIntDef(RumpfGrid.Cells[FRumpfCell.x,FRumpfCell.y], 0);
  RumpfEdit.Text := Format('%4d mm',[RumpfSpinEdit.Position]);
end;

procedure TOptionForm.RumpfGridSelectCell(Sender: TObject; Col, Row: Integer; var CanSelect: Boolean);
begin
  CanSelect := True;
  if (Col = 2) and (Row > 2) then CanSelect := False;
  if CanSelect then
  begin
    FRumpfCell := Point(Col,Row);
    RumpfLabel.Caption := Format('Feld %s%s:',
      [TrimLeft(RumpfGrid.Cells[0,Row]),
       TrimLeft(RumpfGrid.Cells[Col,0])]);
    RumpfSpinEdit.Position := StrToInt(RumpfGrid.Cells[Col,Row]);
    RumpfEdit.Text := Format('%4d mm', [RumpfSpinEdit.Position]);
  end;
end;

procedure TOptionForm.RumpfBtnClick(Sender: TObject);
var
  oo: TRiggPoint;
  kk: TKoord;
begin
  { changed from old days:
    - introduction of ooN0
    - elements of a TIntPoint are of type double now, no longer an Integer
  }

  oo := TRiggPoint(FRumpfCell.y); { A0 is now second element in enum }
  kk := TKoord(FRumpfCell.x - 1); {this has not changed }

  FiP[oo, kk] :=  RumpfSpinEdit.Position;
  RumpfGrid.Cells[FRumpfCell.x, FRumpfCell.y] := Format('%4d', [RumpfSpinEdit.Position]);
  if FRumpfCell.y = 2 then
  begin
    FiP[ooA0] := FiP[ooB0];
    FiP[ooA0,y] := -FiP[ooB0,y];
    RumpfGrid.Cells[1,1] := Format('%4.0f',[FiP[ooA0,x]]);
    RumpfGrid.Cells[2,1] := Format('%4.0f',[FiP[ooA0,y]]);
    RumpfGrid.Cells[3,1] := Format('%4.0f',[FiP[ooA0,z]]);
  end;
  if FRumpfCell.y = 1 then
  begin
    FiP[ooB0] := FiP[ooA0];
    FiP[ooB0,y] := -FiP[ooA0,y];
    RumpfGrid.Cells[1,2] := Format('%4.0f',[FiP[ooB0,x]]);
    RumpfGrid.Cells[2,2] := Format('%4.0f',[FiP[ooB0,y]]);
    RumpfGrid.Cells[3,2] := Format('%4.0f',[FiP[ooB0,z]]);
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
