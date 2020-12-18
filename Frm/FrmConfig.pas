unit FrmConfig;

{$ifdef fpc}
{$mode delphi}
{$endif}

interface

uses
{$ifdef fpc}
  LCLType,
{$else}
  Winapi.Windows,
{$endif}
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
  Vcl.ComCtrls,
  Vcl.Buttons,
  Vcl.Menus,
  RiggVar.App.Strings,
  RiggVar.RG.Scroll,
  RiggVar.App.Model,
  RiggVar.RG.Types;

type

  { TFormConfig }

  TFormConfig = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    OKBtn: TButton;
    CancelBtn: TButton;

    PageControl: TPageControl;

    { Trimm }
    tsTrimm: TTabSheet;
    GroupBoxTrimm: TGroupBox;

    LabelMin: TLabel;
    LabelPos: TLabel;
    LabelMax: TLabel;

    MinEdit: TEdit;
    PosEdit: TEdit;
    MaxEdit: TEdit;

    LengthEditLabel: TLabel;

    TrimmComboLabel: TLabel;
    TrimmCombo: TComboBox;

    { Fachwerk / Material }
    tsFachwerk: TTabSheet;
    GroupBoxMaterial: TGroupBox;

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

    { Mast }
    tsMast: TTabSheet;
    GroupBoxMast: TGroupBox;

    MastTypeComboLabel: TLabel;
    MastTypeCombo: TComboBox;
    EIEdit: TEdit;
    EILabel: TLabel;

    MastMassComboLabel: TLabel;
    MastMassCombo: TComboBox;
    MastMassEdit: TEdit;
    MassMassEditLabel: TLabel;

    { Rumpf }
    tsRumpf: TTabSheet;
    GroupBoxRumpf: TGroupBox;

    Grid: TStringGrid;

    RumpfLabel: TLabel;
    RumpfEdit: TEdit;
    RumpfBtn: TButton;
    RumpfSpinEdit: TUpDown;

    { Ini Memo }
    tsIniMemo: TTabSheet;
    IniMemo: TMemo;
    SaveIniBtn: TButton;
    LoadIniBtn: TButton;

    procedure OKBtnClick(Sender: TObject);

    procedure MastMassEditExit(Sender: TObject);
    procedure MastMassEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure MastTypeComboChange(Sender: TObject);
    procedure MastMassComboChange(Sender: TObject);

    procedure TrimmComboChange(Sender: TObject);
    procedure MinEditExit(Sender: TObject);
    procedure MinEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

    procedure QuerschnittComboChange(Sender: TObject);
    procedure MaterialComboChange(Sender: TObject);
    procedure ElementComboChange(Sender: TObject);
    procedure StoreItemClick(Sender: TObject);
    procedure LoadItemClick(Sender: TObject);
    procedure TakeOverBtnClick(Sender: TObject);
    procedure GridSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
    procedure RumpfBtnClick(Sender: TObject);
    procedure RumpfSpinEditChanging(Sender: TObject; var AllowChange: Boolean);
    procedure RumpfSpinEditEnter(Sender: TObject);
    procedure RumpfSpinEditExit(Sender: TObject);
  protected
    cr: TControl;
    TempR: Integer;
    TempB: Integer;
    FMaxRight: Integer;
    FMaxBottom: Integer;

    Margin: Integer;
    Raster: Integer;

    FScale: single;
    function Scale(Value: Integer): Integer;

    procedure RecordMax;
    procedure AnchorVertical(c: TControl);
    procedure StackH(c: TControl);
    procedure StackV(c: TControl);
  private
    FiMastSaling: Integer;
    FiMastWante: Integer;
    FiMastTop: Integer;
    FiEI: Integer;
    FEAarray: TRiggRods;

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

    procedure GetKeyList(Source, Dest: TStringList);
    procedure FillIniLists;
    procedure FillRiggLists;
    procedure LoadInifileCombos;
    procedure LoadRiggCombos;
    procedure CreateComponents;
    procedure InitComponentLinks;
    procedure InitComponentSize;
    procedure InitComponentProps;
    procedure InitTabOrder;
    procedure LayoutComponents;
    procedure InitGrid;
    procedure SelectInitialCell;
  public
    FirstColumnIndex: Integer;
    FirstRowIndex: Integer;
    SecondRowIndex: Integer;

    Rigg: IRigg;
    IniFileName: string;
    FormShown: Boolean;
    procedure Init(ARigg: IRigg);
    procedure LoadFromIniFile;
    procedure WriteToIniFile;
  end;

var
  FormConfig: TFormConfig;

implementation

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

uses
  RiggVar.App.Main,
  RiggVar.RG.Def;

procedure TFormConfig.FormCreate(Sender: TObject);
begin
  Caption := 'Form Config';

  FScale := MainVar.Scale;

  FGSB := TRggFA.Create;

  FMastTypList := TStringList.Create;
  FMastMassList := TStringList.Create;
  FElementList := TStringList.Create;
  FMaterialList := TStringList.Create;
  FQuerschnittList := TStringList.Create;
  FTrimmList := TStringList.Create;
  FTempList := TStringList.Create;

  FirstColumnIndex := 1;
  FirstRowIndex := 1; // 0 in FMX and 1 in VCL/LCL
  SecondRowIndex := FirstRowIndex + 1;

  FScale := MainVar.Scale;

  Margin := Scale(10);
  Raster := Scale(70);

  CreateComponents;
  InitComponentSize;

  PageControl.ActivePage := tsRumpf;
end;

procedure TFormConfig.FormDestroy(Sender: TObject);
begin
  FMastTypList.Free;
  FMastMassList.Free;
  FElementList.Free;
  FMaterialList.Free;
  FQuerschnittList.Free;
  FTrimmList.Free;
  FTempList.Free;
  FGSB.Free;
end;

procedure TFormConfig.GetKeyList(Source, Dest: TStringList);
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

procedure TFormConfig.InitGrid;
begin
  Grid.Left := Margin;
  Grid.Top := Margin;
  Grid.Width := Scale(263);
  Grid.Height := Scale(178);

  Grid.ColCount := 4;
  Grid.RowCount := 7;
  Grid.Options := [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goDrawFocusSelected];
  Grid.ParentFont := True;
  Grid.ScrollBars := ssNone;
  Grid.TabOrder := 0;
  Grid.OnSelectCell := GridSelectCell;

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
end;

procedure TFormConfig.SelectInitialCell;
var
  b: Boolean;
begin
  b := True;
  FRumpfCell := Point(1, FirstRowIndex);
  GridSelectCell(nil, FRumpfCell.X, FRumpfCell.Y, b);
end;

procedure TFormConfig.Init(ARigg: IRigg);
begin
  Rigg := ARigg;
  IniFileName := ChangeFileExt(ParamStr(0), '.ini');

  SelectInitialCell;

  FillRiggLists;
  LoadRiggCombos;
  FillIniLists;
  LoadInifileCombos;
end;

procedure TFormConfig.FillRiggLists;
var
  fs: string;
begin
  FGSB.Assign(Rigg.RggFA);
  FEAarray := Rigg.EA; { EA in KN }
  FiEI := Rigg.MastEI;
  FiMastSaling := Round(Rigg.MastUnten);
  FiMastWante := FiMastSaling + Round(Rigg.MastOben);
  FiMastTop := Round(Rigg.MastLength);
  FiP := Rigg.RiggPoints;

  FMastMassList.Clear;
  FElementList.Clear;
  FTrimmList.Clear;

  fs := '%s=%d';
  FMastMassList.Add(Format(fs, [RggStrings.MastComboTextSpreader, FiMastSaling]));
  FMastMassList.Add(Format(fs, [RggStrings.MastComboTextShroud, FiMastWante]));
  FMastMassList.Add(Format(fs, [RggStrings.MastComboTextTop, FiMastTop]));

  fs := '%s=%.6g';
  FElementList.Add(Format(fs, [RggStrings.ComboTextSpreader, FEAarray.B0B]));
  FElementList.Add(Format(fs, [RggStrings.ComboTextVorstag, FEAarray.C0C]));
  FElementList.Add(Format(fs, [RggStrings.ComboTextMast, FEAarray.D0C]));
  FElementList.Add(Format(fs, [RggStrings.ComboTextSpreader, FEAarray.BD]));
  FElementList.Add(Format(fs, [RggStrings.ComboTextSpreaderConnection, FEAarray.AB]));
  FElementList.Add(Format(fs, [RggStrings.ComboTextHullRods, FEAarray.C0D0]));

  FTrimmList.Add(RggStrings.ControllerString);
  FTrimmList.Add(RggStrings.WinkelString);
  FTrimmList.Add(RggStrings.VorstagString);
  FTrimmList.Add(RggStrings.WanteString);
  FTrimmList.Add(RggStrings.WanteObenString);
  FTrimmList.Add(RggStrings.SalingHString);
  FTrimmList.Add(RggStrings.SalingAString);
  FTrimmList.Add(RggStrings.SalingLString);
end;

procedure TFormConfig.FillIniLists;
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
  ML.Add(RggStrings.EA_S_Key + '=10');
  ML.Add(RggStrings.EA_M_Key + '=100');
  ML.Add(RggStrings.EA_L_Key + '=1000');

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
  ML.Add(RggStrings.EA_S_Key + '=10');
  ML.Add(RggStrings.EA_M_Key + '=100');
  ML.Add(RggStrings.EA_L_Key + '=1000');
end;

procedure TFormConfig.LoadRiggCombos;
var
  m: TRiggPoint;
  r: Integer;
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
    Grid.Cells[1, r] := Format('%4.0f', [FiP.V[m].X]);
    Grid.Cells[2, r] := Format('%4.0f', [FiP.V[m].Y]);
    Grid.Cells[3, r] := Format('%4.0f', [FiP.V[m].Z]);
  end;
end;

procedure TFormConfig.LoadIniFileCombos;
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

procedure TFormConfig.LoadFromIniFile;
var
  IniFile: TMemIniFile;
begin
  IniFile := TMemIniFile.Create(IniFileName);
  try
    FMaterialList.Clear;
    IniFile.ReadSectionValues(RggStrings.Material_IniSectionString, FMaterialList);
    FQuerschnittList.Clear;
    IniFile.ReadSectionValues(RggStrings.Querschnitte_IniSectionString, FQuerschnittList);
    FMastTypList.Clear;
    IniFile.ReadSectionValues(RggStrings.Profile_IniSectionString, FMastTypList);
  finally
    IniFile.Free;
  end;
end;

procedure TFormConfig.WriteToIniFile;
begin
  IniMemo.Lines.SaveToFile(InifileName);
end;

procedure TFormConfig.LoadItemClick(Sender: TObject);
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
    s := s + RggStrings.MsgStr_NotFound;
    MessageDlg(s, mtInformation, [mbOK], 0);
  end;
end;

procedure TFormConfig.StoreItemClick(Sender: TObject);
begin
  WriteToIniFile;
end;

procedure TFormConfig.TakeOverBtnClick(Sender: TObject);
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

  if s = RggStrings.ComboTextHullRods then
  begin
    FEAarray.C0D0 := c;
    FEAarray.B0C0 := c;
    FEAarray.A0C0 := c;
    FEAarray.B0D0 := c;
    FEAarray.A0D0 := c;
    FEAarray.A0B0 := c;
  end
  else if s = RggStrings.ComboTextWanten then
  begin
    FEAarray.B0B := c;
    FEAarray.A0A := c;
    FEAarray.BC := c;
    FEAarray.AC := c;
  end
  else if s = RggStrings.ComboTextVorstag then
    FEAarray.C0C := c
  else if s = RggStrings.ComboTextSpreader then
  begin
    FEAarray.BD := c;
    FEAarray.AD := c;
  end
  else if s = RggStrings.ComboTextSpreaderConnection then
    FEAarray.AB := c;

  FElementList.Values[s] := EAEdit.Text;
end;

procedure TFormConfig.TrimmComboChange(Sender: TObject);
var
  i: TFederParam;
  f: TRggSB;
begin
  i := TFederParam(TrimmCombo.ItemIndex);
  if i = TFederParam.fpWinkel then
    LengthEditLabel.Caption := RggStrings.LabelText_WinkelInGrad
  else
    LengthEditLabel.Caption := RggStrings.LabelText_DistanceInMM;

  f := FGSB.Find(i);
  MinEdit.Text := IntToStr(Round(f.Min));
  PosEdit.Text := IntToStr(Round(f.Ist));
  MaxEdit.Text := IntToStr(Round(f.Max));
end;

procedure TFormConfig.MinEditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_Return then
    MinEditExit(Sender);
end;

procedure TFormConfig.MinEditExit(Sender: TObject);
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

procedure TFormConfig.MastMassEditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_Return then
    MastMassEditExit(Sender);
end;

procedure TFormConfig.MastMassEditExit(Sender: TObject);
var
  i: Integer;
  temp: Integer;
  s: string;
begin
  s := MastMassCombo.Text;
  if s = '' then
    Exit;

  temp := 0;
  if s = RggStrings.MastComboTextSpreader then
    temp := FiMastSaling
  else if s = RggStrings.MastComboTextShroud then
    temp := FiMastWante
  else if s = RggStrings.MastComboTextTop then
    temp := FiMastTop;

  i := StrToIntDef(MastMassEdit.Text, temp);
  MastMassEdit.Text := IntToStr(i);

  FMastMassList.Values[s] := IntToStr(i);
  if s = RggStrings.MastComboTextSpreader then
    FiMastSaling := i
  else if s = RggStrings.MastComboTextShroud then
    FiMastWante := i
  else if s = RggStrings.MastComboTextTop then
    FiMastTop := i;
end;

procedure TFormConfig.OKBtnClick(Sender: TObject);
begin
  Rigg.RiggPoints := FiP; { Rumpfkoordinaten }
  Rigg.MastUnten := FiMastSaling;
  Rigg.MastOben := FiMastWante - FiMastSaling;
  Rigg.MastLength := FiMastTop;
  Rigg.RggFA.Assign(FGSB); { neue Grenzen und Istwerte }
  Rigg.EA := FEAarray;
  Rigg.MastEI := FiEI;
end;

procedure TFormConfig.MastTypeComboChange(Sender: TObject);
begin
  EIEdit.Text := FMastTypList.Values[MastTypeCombo.Text];
  FiEI := StrToInt(EIEdit.Text);
end;

procedure TFormConfig.MastMassComboChange(Sender: TObject);
begin
  MastMassEdit.Text := FMastMassList.Values[MastMassCombo.Text];
end;

procedure TFormConfig.QuerschnittComboChange(Sender: TObject);
begin
  AEdit.Text := FQuerschnittList.Values[QuerschnittCombo.Text];
end;

procedure TFormConfig.MaterialComboChange(Sender: TObject);
begin
  EEdit.Text := FMaterialList.Values[MaterialCombo.Text];
end;

procedure TFormConfig.ElementComboChange(Sender: TObject);
begin
  EAEdit.Text := FElementList.Values[ElementCombo.Text];
end;

procedure TFormConfig.FormShow(Sender: TObject);
begin
  if not FormShown then
  begin
    LayoutComponents;
    InitTabOrder;
    InitComponentProps;
    InitComponentLinks;
    FormShown := True;
  end;

  FillRiggLists;
  LoadRiggCombos;
  RumpfSpinEdit.Position := StrToIntDef(Grid.Cells[FRumpfCell.X,FRumpfCell.Y], 0);
  RumpfEdit.Text := Format('%4d mm',[RumpfSpinEdit.Position]);
end;

procedure TFormConfig.GridSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
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

    RumpfLabel.Caption := Format('%s %s%s:', [RggStrings.FieldString, sRowHeaderText, sColHeaderText]);
    if RumpfSpinEdit <> nil then
    begin
      RumpfSpinEdit.Position := StrToIntDef(sCellText, 0);
      RumpfEdit.Text := Format('%4d mm', [RumpfSpinEdit.Position]);
    end;
  end;
end;

procedure TFormConfig.RumpfBtnClick(Sender: TObject);
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
    FiP.A0 := FiP.B0;
    FiP.A0.Y := -FiP.B0.Y;
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

procedure TFormConfig.RumpfSpinEditChanging(Sender: TObject;
  var AllowChange: Boolean);
begin
  RumpfEdit.Text := Format('%4d mm',[RumpfSpinEdit.Position]);
end;

procedure TFormConfig.RumpfSpinEditEnter(Sender: TObject);
begin
  RumpfEdit.Color := clWindow;
end;

procedure TFormConfig.RumpfSpinEditExit(Sender: TObject);
begin
  RumpfEdit.Color := clBtnFace;
end;

procedure TFormConfig.CreateComponents;
var
  pc: TPageControl;
  ts: TTabSheet;
  gb: TGroupBox;
  ML: TStrings;
begin
  BorderIcons := [biSystemMenu, biMinimize];
  BorderStyle := bsDialog;
  Color := clBtnFace;

  FormStyle := fsStayOnTop;
  Position := poScreenCenter;
  PixelsPerInch := 96;

  OKBtn :=  TButton.Create(Self);
  OKBtn.Parent := Self;
  OKBtn.Caption := RggStrings.OKBtnCaption;
  OKBtn.ModalResult := 1;

  CancelBtn := TButton.Create(Self);
  CancelBtn.Parent := Self;
  CancelBtn.Cancel := True;
  CancelBtn.Caption := RggStrings.CancelBtnCaption;
  CancelBtn.Default := True;
  CancelBtn.ModalResult := 2;

  PageControl := TPageControl.Create(Self);
  pc := PageControl;
  pc.Parent := Self;

  { Page Trimm }

  tsTrimm := TTabSheet.Create(pc);
  tsTrimm.Caption := RggStrings.TrimmPageCaption;
  ts := tsTrimm;
  ts.PageControl := pc;

  GroupBoxTrimm := TGroupBox.Create(Self);
  GroupBoxTrimm.Caption := RggStrings.TrimmGroupBoxCaption;
  gb := GroupBoxTrimm;
  gb.Parent := ts;

  LabelMin := TLabel.Create(Self);
  LabelMin.Parent := gb;
  LabelMin.Caption := RggStrings.MinLabelCaption; // 'Min';

  LabelPos := TLabel.Create(Self);
  LabelPos.Parent := gb;
  LabelPos.Caption := RggStrings.PosLabelCaption; // 'Pos';

  LabelMax := TLabel.Create(Self);
  LabelMax.Parent := gb;
  LabelMax.Caption := RggStrings.MaxLabelCaption; // 'Max';

  MinEdit := TEdit.Create(Self);
  MinEdit.Parent := gb;
  MinEdit.Text := '    ';

  PosEdit := TEdit.Create(Self);
  PosEdit.Parent := gb;
  PosEdit.Text := '    ';

  MaxEdit := TEdit.Create(Self);
  MaxEdit.Parent := gb;
  MaxEdit.Text := '    ';

  LengthEditLabel := TLabel.Create(Self);
  LengthEditLabel.Parent := gb;
  LengthEditLabel.Caption := RggStrings.LengthEditLabelCaption; // 'Abmessungen in mm';

  TrimmComboLabel := TLabel.Create(Self);
  TrimmComboLabel.Parent := gb;
  TrimmComboLabel.Caption := RggStrings.TrimmComboLabelCaption; // 'Trimmvariable';

  TrimmCombo := TComboBox.Create(Self);
  TrimmCombo.Parent := gb;
  TrimmCombo.Style := csDropDownList;
  ML := TrimmCombo.Items;
  ML.Add(RggStrings.ControllerString);
  ML.Add(RggStrings.WinkelString);
  ML.Add(RggStrings.VorstagString);
  ML.Add(RggStrings.WanteString);
  ML.Add(RggStrings.WanteObenString);
  ML.Add(RggStrings.SalingHString);
  ML.Add(RggStrings.SalingAString);
  ML.Add(RggStrings.SalingLString);

  { Page Fachwerk / Material }

  tsFachwerk := TTabSheet.Create(pc);
  tsFachwerk.Caption := RggStrings.FachwerkPageCaption; // 'Fachwerk';
  ts := tsFachwerk;
  ts.PageControl := pc;

  GroupBoxMaterial := TGroupBox.Create(Self);
  GroupBoxMaterial.Caption := RggStrings.GroupBoxMaterialCaption; // 'Material';
  gb := GroupBoxMaterial;
  gb.Parent := ts;

  ElementLabel := TLabel.Create(Self);
  ElementLabel.Parent := ts;
  ElementLabel.Caption := RggStrings.ElementLabelCaption; //'Fachwerkstäbe';

  ElementCombo := TComboBox.Create(Self);
  ElementCombo.Parent := ts;
  ElementCombo.Style := csDropDownList;
  ML := ElementCombo.Items;
  ML.Add(RggStrings.ComboTextHullRods);
  ML.Add(RggStrings.ComboTextWanten);
  ML.Add(RggStrings.ComboTextVorstag);
  ML.Add(RggStrings.ComboTextSpreader);
  ML.Add(RggStrings.ComboTextSpreaderConnection);

  EAEdit := TEdit.Create(Self);
  EAEdit.Parent := ts;
  EAEdit.Text := RggStrings.EAEditText; // 'EAEdit';
  EAEdit.Color := clBtnFace;
  EAEdit.ReadOnly := True;

  EAEditLabel := TLabel.Create(Self);
  EAEditLabel.Parent := ts;
  EAEditLabel.Caption := RggStrings.EAEditLabelCaption; // 'EA in KN';

  TakeOverBtn := TButton.Create(Self);
  TakeOverBtn.Parent := ts;
  TakeOverBtn.Caption := RggStrings.TakeOverBtnCaption; //'Auswahl übernehmen';

  MaterialCombo := TComboBox.Create(Self);
  MaterialCombo.Parent := gb;
  MaterialCombo.Style := csDropDownList;

  MaterialComboLabel := TLabel.Create(Self);
  MaterialComboLabel.Parent := gb;
  MaterialComboLabel.Caption := RggStrings.MaterialComboLabelCaption; // 'Material';

  QuerschnittComboLabel := TLabel.Create(Self);
  QuerschnittComboLabel.Parent := gb;
  QuerschnittComboLabel.Caption := RggStrings.QuerschnittComboLabelCaption; // 'Querschnitt';

  QuerschnittCombo := TComboBox.Create(Self);
  QuerschnittCombo.Parent := gb;
  QuerschnittCombo.Style := csDropDownList;

  ALabel := TLabel.Create(Self);
  ALabel.Parent := gb;
  ALabel.Caption := RggStrings.ALabelCaption; // 'A';

  AEdit := TEdit.Create(Self);
  AEdit.Parent := gb;
  AEdit.Text := RggStrings.AEditText; // 'AEdit';
  EAEdit.Color := clBtnFace;
  AEdit.ReadOnly := True;

  EEdit := TEdit.Create(Self);
  EEdit.Parent := gb;
  EEdit.Text := RggStrings.EEditText; //'EEdit';
  EAEdit.Color := clBtnFace;
  EEdit.ReadOnly := True;

  ELabel := TLabel.Create(Self);
  ELabel.Parent := gb;
  ELabel.Caption := RggStrings.ELabelCaption; // 'E';

  EEditLabel := TLabel.Create(Self);
  EEditLabel.Parent := gb;
  EEditLabel.Caption := RggStrings.EEditLabelCaption; // 'E-Modul in KN/mm^2';

  AEditLabel := TLabel.Create(Self);
  AEditLabel.Parent := gb;
  AEditLabel.Caption := RggStrings.AEditLabelCaption; // 'Querschnitt in mm^2';

  { Page Mast }

  tsMast := TTabSheet.Create(pc);
  tsMast.Caption := RggStrings.MastPageCaption; // 'Mast';
  ts := tsMast;
  ts.PageControl := pc;

  GroupBoxMast := TGroupBox.Create(Self);
  GroupBoxMast.Caption := RggStrings.GroupBoxMastCaption; // 'Mast';
  gb := GroupBoxMast;
  gb.Parent := ts;

  MastTypeComboLabel := TLabel.Create(Self);
  MastTypeComboLabel.Parent := gb;
  MastTypeComboLabel.Caption := RggStrings.MastTypeComboLabelCaption; // 'Profil';

  MastTypeCombo := TComboBox.Create(Self);
  MastTypeCombo.Parent := gb;
  MastTypeCombo.Style := csDropDownList;

  EIEdit := TEdit.Create(Self);
  EIEdit.Parent := gb;
  EIEdit.Text := RggStrings.EIEditText; // 'EIEdit';
  EIEdit.Color := clBtnFace;
  EIEdit.ReadOnly := True;

  EILabel := TLabel.Create(Self);
  EILabel.Parent := gb;
  EILabel.Caption := RggStrings.EILabelCaption; // 'Biegesteifigkeit EI in Nm^2';

  MastMassComboLabel := TLabel.Create(Self);
  MastMassComboLabel.Parent := gb;
  MastMassComboLabel.Caption := RggStrings.MastMassComboLabelCaption; //'Abmessungen';

  MastMassCombo := TComboBox.Create(Self);
  MastMassCombo.Parent := gb;
  MastMassCombo.Style := csDropDownList;
  ML := MastMassCombo.Items;
  ML.Add(RggStrings.MastComboTextController);
  ML.Add(RggStrings.MastComboTextSpreader);
  ML.Add(RggStrings.MastComboTextShroud);
  ML.Add(RggStrings.MastComboTextTop);

  MastMassEdit := TEdit.Create(Self);
  MastMassEdit.Parent := gb;

  MassMassEditLabel := TLabel.Create(Self);
  MassMassEditLabel.Parent := gb;
  MassMassEditLabel.Caption := RggStrings.MassMassEditLabelCaption; // 'Abstand vom Mastfuß in mm';

  { Page Rumpf }

  tsRumpf := TTabSheet.Create(pc);
  tsRumpf.Caption := RggStrings.HullPageCaption; // 'Rumpf';
  ts := tsRumpf;
  ts.PageControl := pc;

  GroupBoxRumpf := TGroupBox.Create(Self);
  GroupBoxRumpf.Caption := RggStrings.GroupBoxHullCaption; // 'Feld Editieren';
  gb := GroupBoxRumpf;
  gb.Parent := ts;

  RumpfLabel := TLabel.Create(Self);
  RumpfLabel.Parent := gb;
  RumpfLabel.WordWrap := False;
  RumpfLabel.AutoSize := True;

  RumpfEdit := TEdit.Create(Self);
  RumpfEdit.Parent := gb;
  RumpfEdit.Text := '10';
  RumpfEdit.ReadOnly := True;
  RumpfEdit.AutoSelect := False;

  RumpfBtn := TButton.Create(Self);
  RumpfBtn.Parent := gb;
  RumpfBtn.Caption := RggStrings.RumpfBtnCaption; // 'Übernehmen';

  RumpfSpinEdit := TUpDown.Create(Self);
  RumpfSpinEdit.Parent := gb;
  RumpfSpinEdit.Min := -32000;
  RumpfSpinEdit.Max := 32000;
  RumpfSpinEdit.Position := 10;
  RumpfSpinEdit.OnEnter := RumpfSpinEditEnter;
  RumpfSpinEdit.OnExit := RumpfSpinEditExit;

  Grid := TStringGrid.Create(Self);
  Grid.Parent := ts;
  InitGrid;

  { Page Ini }

  tsIniMemo := TTabSheet.Create(pc);
  tsIniMemo.Caption := RggStrings.IniMemoPageCaption; // 'Rigg.ini';
  ts := tsIniMemo;
  ts.PageControl := pc;

  IniMemo := TMemo.Create(Self);
  IniMemo.Parent := ts;

  SaveIniBtn := TButton.Create(Self);
  SaveIniBtn.Parent := ts;
  SaveIniBtn.Caption := RggStrings.SaveIniBtnCaption; // 'Speichern';

  LoadIniBtn := TButton.Create(Self);
  LoadIniBtn.Parent := ts;
  LoadIniBtn.Caption := RggStrings.LoadIniBtnCaption; // 'Laden';
end;

procedure TFormConfig.InitComponentLinks;
begin
  { Trimm }
  MinEdit.OnExit := MinEditExit;
  MinEdit.OnKeyDown := MinEditKeyDown;
  PosEdit.OnExit := MinEditExit;
  PosEdit.OnKeyDown := MinEditKeyDown;
  MaxEdit.OnExit := MinEditExit;
  MaxEdit.OnKeyDown := MinEditKeyDown;

  TrimmCombo.OnChange := TrimmComboChange;

  { Fachwerk }
  ElementCombo.OnChange := ElementComboChange;
  TakeOverBtn.OnClick := TakeOverBtnClick;
  MaterialCombo.OnChange := MaterialComboChange;
  QuerschnittCombo.OnChange := QuerschnittComboChange;

  { Mast }
  MastMassCombo.OnChange := MastMassComboChange;
  MastTypeCombo.OnChange := MastTypeComboChange;

  MastMassEdit.OnExit := MastMassEditExit;
  MastMassEdit.OnKeyDown := MastMassEditKeyDown;

  { Hull }
  RumpfSpinEdit.OnChanging := RumpfSpinEditChanging;
  RumpfBtn.OnClick := RumpfBtnClick;

  { IniMemo }
  SaveIniBtn.OnClick := StoreItemClick;
  LoadIniBtn.OnClick := LoadItemClick;

  { Buttons }
  OKBtn.OnClick := OKBtnClick;
end;

procedure TFormConfig.InitComponentSize;
var
  w, h: Integer;
  gbw: Integer;
begin
  PageControl.Width := Scale(520);
  PageControl.Height := Scale(270);

  gbw := PageControl.Width - 3 * Margin;

  { Trimm }
  GroupBoxTrimm.Width := gbw;
  GroupBoxTrimm.Height := Scale(170);

  LabelMin.Width := Scale(21);
  LabelPos.Width := Scale(24);
  LabelMax.Width := Scale(25);

  w := Scale(40);
  MinEdit.Width := w;
  PosEdit.Width := w;
  MaxEdit.Width := w;

  LengthEditLabel.Width := Scale(125);
  TrimmComboLabel.Width := Scale(87);
  TrimmCombo.Width := Scale(150);

  { Fachwerk }
  ElementLabel.Width := Scale(92);
  ElementCombo.Width := Scale(161);
  EAEdit.Width := Scale(105);
  EAEditLabel.Width := Scale(52);
  TakeOverBtn.Width := Scale(170);
  TakeOverBtn.Height := Scale(25);

  GroupBoxMaterial.Width := gbw;
  GroupBoxMaterial.Height := Scale(150);

  MaterialComboLabel.Width := Scale(48);
  MaterialCombo.Width := Scale(153);
  ELabel.Width := Scale(9);
  EEdit.Width := Scale(73);
  EEditLabel.Width := Scale(124);
  QuerschnittComboLabel.Width := Scale(66);
  QuerschnittCombo.Width := Scale(153);
  ALabel.Width := Scale(9);
  AEdit.Width := Scale(73);
  AEditLabel.Width := Scale(118);

  { Mast }
  GroupBoxMast.Width := gbw;
  GroupBoxMast.Height := Scale(200);

  MastTypeComboLabel.Width := Scale(30);
  MastTypeCombo.Width := Scale(145);
  EIEdit.Width := Scale(73);
  EILabel.Width := Scale(158);
  MastMassComboLabel.Width := Scale(87);
  MastMassCombo.Width := Scale(145);
  MastMassEdit.Width := Scale(73);
  MassMassEditLabel.Width := Scale(167);

  { Hull }
  GroupBoxRumpf.Width := Scale(170);
  GroupBoxRumpf.Height := Scale(145);

  RumpfLabel.Width := Scale(73);
  RumpfLabel.Height := Scale(16);
  RumpfEdit.Width := Scale(81);
  RumpfEdit.Height := Scale(24);
  RumpfBtn.Width := Scale(129);
  RumpfBtn.Height := Scale(25);

  { IniMemo }
  IniMemo.Width := Scale(400);
  IniMemo.Height := 50;
  SaveIniBtn.Width := Scale(81);
  SaveIniBtn.Height := Scale(25);
  LoadIniBtn.Width := Scale(81);
  LoadIniBtn.Height := Scale(25);

  { Buttons }
  h := Scale(27);
  OKBtn.Width := Scale(81);
  OKBtn.Height := h;
  CancelBtn.Width := Scale(101);
  CancelBtn.Height := h;
end;

procedure TFormConfig.InitComponentProps;
begin
  MinEdit.MaxLength := 4;
  PosEdit.MaxLength := 4;
  MaxEdit.MaxLength := 4;
  MastMassEdit.MaxLength := 4;

  IniMemo.ScrollBars := ssVertical;
end;

procedure TFormConfig.InitTabOrder;
begin
  PageControl.TabOrder := 0;

  { Trimm }
  GroupBoxTrimm.TabOrder := 0;
  MinEdit.TabOrder := 1;
  PosEdit.TabOrder := 2;
  MaxEdit.TabOrder := 3;
  TrimmCombo.TabOrder := 4;

  { Fachwerk }
  ElementCombo.TabOrder := 0;
  EAEdit.TabOrder := 1;
  TakeOverBtn.TabOrder := 2;
  GroupBoxMaterial.TabOrder := 3;
  MaterialCombo.TabOrder := 0;
  EEdit.TabOrder := 3;
  QuerschnittCombo.TabOrder := 1;
  AEdit.TabOrder := 2;

  AEdit.TabStop := False;
  EEdit.TabStop := False;
  EAEdit.TabStop := False;

  { Mast }
  GroupBoxMast.TabOrder := 0;
  MastTypeCombo.TabOrder := 2;
  EIEdit.TabOrder := 3;
  MastMassCombo.TabOrder := 0;
  MastMassEdit.TabOrder := 1;
  EIEdit.TabStop := False;

  { Hull }
  RumpfEdit.TabOrder := 1;
  RumpfSpinEdit.TabOrder := 2;
  RumpfSpinEdit.TabStop := True;
  RumpfBtn.TabOrder := 0;

  { IniMemo }
  IniMemo.TabOrder := 0;
  SaveIniBtn.TabOrder := 1;
  LoadIniBtn.TabOrder := 2;

  { Buttons }
  OKBtn.TabOrder := 1;
  CancelBtn.TabOrder := 2;
end;

procedure TFormConfig.LayoutComponents;
begin
  Left := Scale(230);
  Top := Scale(100);

  PageControl.Left := Margin;
  PageControl.Top := Margin;

  { Trimm }

  GroupBoxTrimm.Left := Margin;
  GroupBoxTrimm.Top := 2 * Margin;

  LabelMin.Left := 4 * Margin;
  LabelMin.Top := 3 * Margin;

  cr := LabelMin;
  StackH(LabelPos);
  StackH(LabelMax);

  cr := LabelMin;
  StackV(MinEdit);
  StackH(PosEdit);
  StackH(MaxEdit);

  LabelPos.Left := PosEdit.Left;
  LabelMax.Left := MaxEdit.Left;

  cr := MaxEdit;
  StackH(LengthEditLabel);

  cr := MinEdit;
  StackV(TrimmCombo);
  StackH(TrimmComboLabel);

  { Fachwerk }

  ElementLabel.Left := Margin;
  ElementLabel.Top := 2 * Margin;
  cr := ElementLabel;
  StackV(ElementCombo);
  StackH(EAEdit);
  StackH(TakeOverBtn);
  cr := EAEdit;
  StackV(EAEditLabel);
  EAEditLabel.Top := ElementLabel.Top;

  cr := ElementCombo;
  StackV(GroupBoxMaterial);

  MaterialComboLabel.Left := 3 * Margin;
  MaterialComboLabel.Top := 2 * Margin;

  cr := MaterialComboLabel;
  StackV(MaterialCombo);
  StackH(EEdit);
  StackH(EEditLabel);
  cr := EEdit;
  StackV(ELabel);
  ELabel.Top := MaterialComboLabel.Top;

  cr := MaterialCombo;
  StackV(QuerschnittComboLabel);
  StackV(QuerschnittCombo);
  StackH(AEdit);
  StackH(AEditLabel);
  cr := AEdit;
  StackV(ALabel);
  ALabel.Top := QuerschnittComboLabel.Top;

  { Mast }

  GroupBoxMast.Left := Margin;
  GroupBoxMast.Top := 2 * Margin;

  MastTypeComboLabel.Left := 3 * Margin;
  MastTypeComboLabel.Top := 3 * Margin;

  cr := MastTypeComboLabel;
  StackV(MastTypeCombo);
  StackH(EIEdit);
  StackH(EILabel);

  cr := MastTypeCombo;
  StackV(MastMassComboLabel);
  StackV(MastMassCombo);
  StackH(MastMassEdit);
  StackH(MassMassEditLabel);

  { Hull }

  cr := Grid;
  StackH(GroupBoxRumpf);

  RumpfLabel.Left := 2 * Margin;
  RumpfLabel.Top := 2 * Margin;

  cr := RumpfLabel;
  StackV(RumpfEdit);
  StackV(RumpfBtn);
  RumpfSpinEdit.Associate := RumpfEdit;

  { Ini Memo }

  IniMemo.Left := 0;
  IniMemo.Top := 0;
  IniMemo.Align := alLeft;

  cr := IniMemo;
  StackH(SaveIniBtn);
  SaveIniBtn.Top := SaveIniBtn.Top + 30;
  StackV(LoadIniBtn);

  { Buttons }

  cr := PageControl;
  StackV(OKBtn);
  OKBtn.Left := Scale(169);

  cr := OKBtn;
  StackH(CancelBtn);

  { Form }

  ClientWidth := PageControl.Width + 2 * Margin;
  ClientHeight := OKBtn.Top + OKBtn.Height + Margin;
end;

procedure TFormConfig.RecordMax;
begin
  TempR := cr.Left + cr.Width;
  if TempR > FMaxRight then
    FMaxRight := TempR;

  TempB := cr.Top + cr.Height;
  if TempB > FMaxBottom then
    FMaxBottom := TempB;
end;

procedure TFormConfig.StackH(c: TControl);
begin
  c.Left := cr.Left + cr.Width + Margin;
  c.Top := cr.Top;
  cr := c;
  RecordMax;
end;

procedure TFormConfig.StackV(c: TControl);
begin
  c.Left := cr.Left;
  c.Top := cr.Top + cr.Height + Margin;
  cr := c;
  RecordMax;
end;

procedure TFormConfig.AnchorVertical(c: TControl);
begin
  c.Height := ClientHeight - c.Top - Margin;
  c.Anchors := c.Anchors + [TAnchorKind.akBottom];
end;

function TFormConfig.Scale(Value: Integer): Integer;
begin
  result := Round(Value * FScale);
end;

end.
