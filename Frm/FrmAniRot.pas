unit FrmAniRot;

interface

{.$define Rigg19}

uses
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  RggTypes,
  RiggVar.App.Model,
  RiggVar.RG.Model,
  FrmRot,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Menus,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.ComCtrls,
  Vcl.Buttons,
  Vcl.ExtDlgs,
  Vcl.Dialogs;

const
  AniStepCountMax = 50;

type
  TAniRotationForm = class(TRotationForm)
    Timer: TTimer;
    RightPanel: TPanel;
    ScrollBarPanel: TPanel;
    TrackBar: TTrackBar;
    lbMin: TLabel;
    lbMax: TLabel;
    lbIst: TLabel;
    lbMinVal: TLabel;
    lbMaxVal: TLabel;
    lbIstVal: TLabel;
    lbParam: TLabel;
    ListPanel: TPanel;
    ListBox: TListBox;
    procedure FormCreate(Sender: TObject);
    procedure AniDlgItemClick(Sender: TObject);
    procedure ShowItemClick(Sender: TObject);
    procedure AnimationItemClick(Sender: TObject);
    procedure CommandLineItemClick(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure TrackBarChange(Sender: TObject);
    procedure ListBoxClick(Sender: TObject);
    procedure RiggTypItemClick(Sender: TObject);
    procedure OptionenMenuClick(Sender: TObject);
    procedure GlobalUpdateItemClick(Sender: TObject);
    procedure LocalUpdateItemClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
  private
    FParam: TFederParam;
    FAniStepCount: Integer;
    AniSteps: array[0..AniStepCountMax] of double;
    AniStep: Integer; { Index in das Array AniSteps }
    AniIncrement: Integer;

    procedure SetParameter(Value: TFederParam);
    procedure SetParamProp(Index: TFederParam; Value: double);
    function GetParamProp(Index: TFederParam): double;
    function GetParamMin(Index: TFederParam): Integer;
    function GetParamMax(Index: TFederParam): Integer;
    function GetParamPos(Index: TFederParam): Integer;
    procedure SetAniStepCount(Value: Integer);

    procedure SetupTrackBar;
    procedure TypeChanged;
    procedure PrepareAnimation;

    procedure UpdateLocalRigg;
    procedure UpdateGlobalRigg;
    procedure ShowDialog;
    procedure DoBigWheel(Delta: single);
    procedure DoSmallWheel(Delta: single);
    procedure DoWheel(Delta: single);
    function GetBigStep: single;
    function GetSmallStep: single;
    procedure HandleMouseWheel(Shift: TShiftState; WheelDelta: Integer);
    procedure Zoom(Delta: single);
  public
    Modified: Boolean;
    WinkelSelStart: Integer;
    WinkelSelEnd: Integer;

    procedure InitListboxItems;
    function Param2Text(P: TFederParam): string;
    function Text2Param(T: String): TFederParam;
    procedure UpdateAll(Rgg: TRigg);

    property Parameter: TFederParam read FParam write SetParameter;
    property ParamProp[Index: TFederParam]: double read GetParamProp write SetParamProp;
    property ParamMin[Index: TFederParam]: Integer read GetParamMin;
    property ParamMax[Index: TFederParam]: Integer read GetParamMax;
    property ParamPos[Index: TFederParam]: Integer read GetParamPos;
    property AniStepCount: Integer read FAniStepCount write SetAniStepCount;
  public
    AnimationItemChecked: Boolean; { needed when Menu will not be created ? }
  public
    OptionenMenu: TMenuItem;
    AniDlgItem: TMenuItem;
    ShowItem: TMenuItem;
    N2: TMenuItem;
    AnimationItem: TMenuItem;
    CommandLineItem: TMenuItem;
    RiggTypItem: TMenuItem;
    N1: TMenuItem;
    GlobalUpdateItem: TMenuItem;
    LocalUpdateItem: TMenuItem;
    procedure InitMenu; override;
  end;

var
  AniRotationForm: TAniRotationForm;

implementation

{$R *.DFM}

uses
  RiggVar.RG.Def,
  FrmAni,
  FrmCmd,
  Math,
  FrmModel,
  RggScroll,
  RggDoc,
  RggModul;

procedure TAniRotationForm.FormCreate(Sender: TObject);
begin
  inherited;

{$ifdef Rigg19}
  MinTrackX := 380;
  MinTrackY := 230;
{$endif}

  if RiggModul.RG19A then
    Formstyle := fsMDIChild;

  AniIncrement := 1;
  WinkelSelStart := ParamPos[fpWinkel] - 6;
  WinkelSelEnd := ParamPos[fpWinkel] + 2;

  RightPanel.Visible := True;
  ListBox.ItemIndex := ListBox.Items.IndexOf('Vorstag');
  InitListboxItems; { Parameter := Vorstag; --> SetupTrackBar }

  AniRotationForm := Self;
  AnimationForm := TAnimationForm.Create(Self);
  CommandForm := TCommandForm.Create(Self);

  if RiggModul.RG19A then
    InitMenu;

  RightPanelWidth := RightPanel.Width;
end;

procedure TAniRotationForm.ShowItemClick(Sender: TObject);
begin
  RightPanel.Visible := not RightPanel.Visible;
  ShowItem.Checked := RightPanel.Visible;
end;

procedure TAniRotationForm.AniDlgItemClick(Sender: TObject);
begin
  AniDlgItem.Checked := not AniDlgItem.Checked;
  if AniDlgItem.Checked then
    AnimationForm.Show
  else
    AnimationForm.Hide;
end;

procedure TAniRotationForm.AnimationItemClick(Sender: TObject);
begin
  AnimationItemChecked := not AnimationItemChecked;

  if AnimationItem <> nil then
    AnimationItem.Checked := AnimationItemChecked;

  AnimationForm.AnimateBtn.Down := AnimationItemChecked;

  if AnimationItemChecked then
  begin
    PrepareAnimation;
    Timer.Enabled := True;
  end
  else
  begin
    Timer.Enabled := False;
    SetupTrackbar;
  end;
end;

procedure TAniRotationForm.PrepareAnimation;
var
  i, Start, Ende, Pos: Integer;
  AniSinus: Boolean;
begin
  Start := AnimationForm.tbWinkel.SelStart;
  Ende := AnimationForm.tbWinkel.SelEnd;
  Pos := AnimationForm.tbWinkel.Position;
  AniStepCount := AnimationForm.UpDownStepCount.Position;
  AniSinus := AnimationForm.cbSinus.Checked;

  if not AniSinus then
    for i := 0 to AniStepCount do
      AniSteps[i] := Start + i/AniStepCount*(Ende-Start)
  else
  begin
    for i := 1 to AniStepCount do
      AniSteps[i] := sin(i/(AniStepCount+1)*pi);
    for i := 2 to AniStepCount do
      AniSteps[i] := AniSteps[i-1] + AniSteps[i];
    for i := 1 to AniStepCount do
      AniSteps[i] := AniSteps[i]/AniSteps[AniStepCount]*(Ende-Start);
    AniSteps[0] := Start;
    for i := 1 to AniStepCount do
      AniSteps[i] := Start + AniSteps[i];
  end;

  AniStep := 0;
  if (Start < Pos) and (Pos < Ende) then
    AniStep := Floor((Pos-Start)/(Ende-Start)*AniStepCount);

  Rigg.RealGlied[fpWinkel] := DegToRad(AniSteps[AniStep]);
end;

procedure TAniRotationForm.TimerTimer(Sender: TObject);
begin
  AniStep := AniStep + AniIncrement;
  if AniStep < 0 then
    AniStep := 0;
  if AniStep > AniStepCount then
    AniStep := AniStepCount;
  if AniStep = AniStepCount then
    AniIncrement := -1;
  if AniStep = 0 then
    AniIncrement := 1;
  Rigg.RealGlied[fpWinkel] := DegToRad(AniSteps[AniStep]);
  UpdateGraph;
  AnimationForm.tbWinkel.Position := Round(ParamProp[fpWinkel]);
end;

procedure TAniRotationForm.CommandLineItemClick(Sender: TObject);
begin
  CommandLineItem.Checked := not CommandLineItem.Checked;
  if CommandLineItem.Checked then CommandForm.Show
  else CommandForm.Hide;
end;

procedure TAniRotationForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (ssCtrl in Shift) and (Key = VK_TAB) then
    if AnimationForm.Visible then
      AnimationForm.SetFocus
    else if CommandForm.Visible then
      CommandForm.SetFocus;
end;

procedure TAniRotationForm.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  { Main.DoMouseWheel(Shift, WheelDelta); }
  HandleMouseWheel(Shift, WheelDelta);
//  ShowTrimm;
  Handled := True;
end;

procedure TAniRotationForm.SetAniStepCount(Value: Integer);
begin
  FAniStepCount := Value;
  if FAniStepCount > AniStepCountMax then
    FAniStepCount := AniStepCountMax
  else if FAniStepCount < 1 then
    FAniStepCount := 1;
end;

function TAniRotationForm.GetParamMin(Index: TFederParam): Integer;
begin
  result := Round(Rigg.RggFA.Find(Index).Min);
end;

function TAniRotationForm.GetParamMax(Index: TFederParam): Integer;
begin
  result := Round(Rigg.RggFA.Find(Index).Max);
end;

function TAniRotationForm.GetParamPos(Index: TFederParam): Integer;
begin
  result := Round(Rigg.RggFa.Find(Index).Ist);
end;

procedure TAniRotationForm.SetParamProp(Index: TFederParam; Value: double);
var
  cr: TRggSB;
begin
  if Value = ParamProp[Index] then
   Exit;
  cr := Rigg.RggFA.Find(Index);
  if Value > cr.Max then
    Exit;
  if Value < cr.Min then
    Exit;
  if Index = fpWinkel then
    Rigg.RealGlied[Index] := DegToRad(Value)
  else
    Rigg.RealGlied[Index] := Value;
  UpdateGraph;
end;

function TAniRotationForm.GetParamProp(Index: TFederParam): double;
begin
  if Index = fpWinkel then
    result := RadToDeg(Rigg.RealGlied[Index])
  else
    result := Rigg.RealGlied[Index];
end;

procedure TAniRotationForm.SetParameter(Value: TFederParam);
begin
  FParam := Value;
  SetupTrackBar;
end;

procedure TAniRotationForm.TrackBarChange(Sender: TObject);
begin
  ParamProp[Parameter] := TrackBar.Position;
  if Parameter = fpWinkel then
    lbIstVal.Caption := Format('%4d Grad', [TrackBar.Position])
  else
    lbIstVal.Caption := Format('%4d mm', [TrackBar.Position]);
end;

procedure TAniRotationForm.SetupTrackBar;
var
  temp: double;
  cr: TRggSB;
begin
  { Round(ParamProp[Parameter]) kann sich außerhalb der Grenzen befinden.
  TrackBar.Position wird aber automatisch auf TrackBar.Min bzw. TrackBar.Max gezogen.
  lbIstVal.Caption muß deshalb mit Round(ParamProp[Parameter]) bestimmt werden. }
  temp := ParamProp[Parameter];
  cr := Rigg.RggFA.Find(Parameter);
  TrackBar.Min := 0;
  TrackBar.Max := Round(cr.Max);
  TrackBar.Position := Round(temp);
  TrackBar.Min := Round(cr.Min);
  TrackBar.LineSize := Round(cr.SmallStep);
  TrackBar.PageSize := Round(cr.BigStep);
  if Parameter = fpWinkel then
  begin
    lbMinVal.Caption := Format('%4.0f Grad', [cr.Min]);
    lbMaxVal.Caption := Format('%4.0f Grad', [cr.Max]);
    lbIstVal.Caption := Format('%4.0f Grad', [temp]);
  end
  else
  begin
    lbMinVal.Caption := Format('%4.0f mm', [cr.Min]);
    lbMaxVal.Caption := Format('%4.0f mm', [cr.Max]);
    lbIstVal.Caption := Format('%4.0f mm', [temp]);
  end;
  lbParam.Caption := Param2Text(Parameter);
end;

procedure TAniRotationForm.ListBoxClick(Sender: TObject);
begin
  if ListBox.ItemIndex <> -1 then
    Parameter := Text2Param(ListBox.Items[ListBox.ItemIndex]);
end;

procedure TAniRotationForm.InitListboxItems;
var
  i: Integer;
  s: string;
begin
  s := 'Vorstag';
  if ListBox.ItemIndex <> -1 then
    s := ListBox.Items[ListBox.ItemIndex];
  with ListBox.Items do
  begin
    Clear;
    if (Rigg.ControllerTyp = ctDruck) and (Rigg.SalingTyp <> stOhneStarr) then
      Add('Controller');
    if Rigg.SalingTyp = stFest then
    begin
      if Rigg.ManipulatorMode then
        Add('Winkel')
      else
        Add('Vorstag');
      Add('Wante');
      Add('Wante oben');
      Add('Saling Höhe');
      Add('Saling Abstand');
    end;
    if Rigg.SalingTyp = stDrehbar then
    begin
      Add('Vorstag');
      Add('Wante');
      Add('Wante oben');
      Add('Saling Länge');
    end;
    if Rigg.SalingTyp = stOhneBiegt then
    begin
      Add('Vorstag');
      Add('Wante');
    end;
    if Rigg.SalingTyp = stOhneStarr then
    begin
      Add('Vorstag');
    end;
  end;
  i := ListBox.Items.IndexOf(s);
  if i <> -1 then
  begin
    ListBox.ItemIndex := i;
    Parameter := Text2Param(s);
  end
  else
  begin
    ListBox.ItemIndex := 0;
    Parameter := Text2Param(ListBox.Items[0]);
  end;
end;

function TAniRotationForm.Text2Param(T: string): TFederParam;
begin
  result := fpWPowerOS;
  if T = 'Controller' then
    result := fpController
  else if T = 'Winkel' then
    result := fpWinkel
  else if T = 'Vorstag' then
    result := fpVorstag
  else if T = 'Wante' then
    result := fpWante
  else if (T = 'Wante oben') or (T = 'Woben') then
    result := fpWoben
  else if (T = 'Saling Höhe') or (T = 'SalingH') then
    result := fpSalingH
  else if (T = 'Saling Abstand') or (T = 'SalingA') then
    result := fpSalingA
  else if (T = 'Saling Länge') or (T = 'SalingL') then
    result := fpSalingL;
end;

function TAniRotationForm.Param2Text(P: TFederParam): string;
begin
  result := '';
  if P = fpController then
    result := 'Controller'
  else if P = fpWinkel then
    result := 'Winkel'
  else if P = fpVorstag then
    result := 'Vorstag'
  else if P = fpWante then
    result := 'Wante'
  else if P = fpWoben then
    result := 'Wante oben'
  else if P = fpSalingH then
    result := 'Saling Höhe'
  else if P = fpSalingA then
    result := 'Saling Abstand'
  else if P = fpSalingL then
    result := 'Saling Länge';
end;

procedure TAniRotationForm.RiggTypItemClick(Sender: TObject);
begin
  { Animation gegebenenfalls ausschalten }
  if AnimationItemChecked then
    AnimationItemClick(Self);
  ShowDialog;
  TypeChanged;
end;

procedure TAniRotationForm.OptionenMenuClick(Sender: TObject);
var
  temp: Boolean;
begin
  temp := Rigg.ManipulatorMode;
  AniDlgItem.Enabled := temp;
  AnimationItem.Enabled := temp;
end;

procedure TAniRotationForm.GlobalUpdateItemClick(Sender: TObject);
begin
  UpdateGlobalRigg;
end;

procedure TAniRotationForm.LocalUpdateItemClick(Sender: TObject);
begin
  UpdateLocalRigg;
  TypeChanged;
end;

procedure TAniRotationForm.TypeChanged;
begin
  InitListboxItems;
  RaumGraph.Salingtyp := Rigg.Salingtyp;
  RaumGraph.ControllerTyp := Rigg.ControllerTyp;
  UpdateGraph;
end;

procedure TAniRotationForm.FormActivate(Sender: TObject);
begin
  if not Timer.enabled then
    LocalUpdateItemClick(Self);
end;

procedure TAniRotationForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
{$ifdef Rigg19}
  RiggModul.RotaFormActive := False;
  Action := caFree;
{$endif}
  inherited;
end;

procedure TAniRotationForm.UpdateLocalRigg;
{$ifdef Rigg19}
var
  RggDocument: TRggDocument;
{$endif}
begin
{$ifdef Rigg19}
  RggDocument := TRggDocument.Create;
  try
   RiggModul.Rigg.UpdateGSB;
   RiggModul.Rigg.GetDocument(RggDocument);
   Rigg.SetDocument(RggDocument);
   { ManipulatorMode nicht in RggDocument! }
   Rigg.ManipulatorMode := RiggModul.Rigg.ManipulatorMode;
   Modified := False;
  finally
    RggDocument.Free;
  end;
{$endif}
end;

procedure TAniRotationForm.UpdateGlobalRigg;
{$ifdef Rigg19}
var
  RggDocument: TRggDocument;
{$endif}
begin
{$ifdef Rigg19}
  RggDocument := TRggDocument.Create;
  try
   Rigg.GetDocument(RggDocument);
   RiggModul.Neu(RggDocument);
   if Rigg.ManipulatorMode <> RiggModul.Rigg.ManipulatorMode then
     RiggModul.WinkelBtnDown := not RiggModul.WinkelBtnDown;
  finally
    RggDocument.Free;
  end;
{$endif}
end;

procedure TAniRotationForm.UpdateAll(Rgg: TRigg);
var
  hasChanged: Boolean;
begin
  { Local Rigg nur dann automatisch nachführen,
    wenn RightPanel sichtbar ist und der Typ verändert wurde.
    Nicht nachführen, wenn nur die Werte verändert wurden!
    Der TrackBar und die Labels werden daher ungültig.
    Die Grafik kann wegspringen, wenn der Trackbar verändert wird. }

  hasChanged := Modified or
    (Rigg.SalingTyp <> Rgg.SalingTyp) or
    (Rigg.ControllerTyp <> Rgg.ControllerTyp) or
    (Rigg.ManipulatorMode <> Rgg.ManipulatorMode);

  if RightPanel.Visible and hasChanged then
  begin
    UpdateLocalRigg;
    InitListboxItems;
  end;

  RaumGraph.Salingtyp := Rgg.Salingtyp;
  RaumGraph.ControllerTyp := Rgg.ControllerTyp;
  RaumGraph.Koordinaten := Rgg.RiggPoints;
  RaumGraph.SetMastLineData(Rgg.MastLinie, Rgg.MastLC, Rgg.MastBeta);
  RaumGraph.WanteGestrichelt := not Rgg.GetriebeOK;
end;

procedure TAniRotationForm.ShowDialog;
begin
  RiggDialog := TRiggDialog.Create(Application);
  try
    RiggDialog.Rigg := Rigg;
    RiggDialog.ShowModal;
    if RiggDialog.ModalResult = mrOK then
      Modified := True;
  finally
    RiggDialog.Free;
  end;
end;

procedure TAniRotationForm.InitMenu;
var
  p, q: TMenuItem;
  mi: TMenuItem;

  function AddP(AName: string): TMenuItem;
  begin
    mi := TMenuItem.Create(MainMenu);
    mi.Name := AName;
    p := mi;
    MainMenu.Items.Add(p);
    result := mi;
  end;

  function AddI(AName: string): TMenuItem;
  begin
    mi := TMenuItem.Create(MainMenu);
    mi.Name := AName;
    p.Add(mi);
    result := mi;
    q := mi;
  end;

  function AddJ(AName: string): TMenuItem;
  begin
    mi := TMenuItem.Create(MainMenu);
    mi.Name := AName;
    q.Add(mi);
    result := mi;
  end;
begin
  inherited;

  OptionenMenu := AddP('');
  mi.Caption := '3D &Modell';
  mi.GroupIndex := 8;
  mi.Hint := '  3D Grafik manipulieren';
  mi.OnClick := OptionenMenuClick;

  RiggTypItem := AddI('');
  mi.Caption := 'Rigg&Typ einstellen...';
  mi.Hint := '  Einstellungen für lokales Riggobjekt';
  mi.ShortCut := 16468;
  mi.OnClick := RiggTypItemClick;

  ShowItem := AddI('');
  mi.Caption := '&Liste einblenden';
  mi.Hint := '  Grafik manipulieren';
  mi.OnClick := ShowItemClick;

  CommandLineItem := AddI('');
  mi.Caption := '&Kommandozeile...';
  mi.Hint := '  Tastaturinterface einblenden';
  mi.ShortCut := 16474;
  mi.OnClick := CommandLineItemClick;

  N2 := AddI('');
  mi.Caption := '-';

  AniDlgItem := AddI('');
  mi.Caption := 'Animation &vorbereiten...';
  mi.Hint := '  erfordert Winkelmodus';
  mi.OnClick := AniDlgItemClick;

  AnimationItem := AddI('');
  mi.Caption := '&Animation Ein/Aus';
  mi.Hint := '  Animation ein- bzw ausschalten';
  mi.ShortCut := 16471;
  mi.OnClick := AnimationItemClick;

  N1 := AddI('');
  mi.Caption := '-';

  GlobalUpdateItem := AddI('');
  mi.Caption := 'Änderungen übernehmen';
  mi.Hint := '  Globales Riggobjekt aktualisieren';
  mi.OnClick := GlobalUpdateItemClick;

  LocalUpdateItem := AddI('');
  mi.Caption := 'Grafik zurücksetzen';
  mi.Hint := '  Lokales Riggobjekt aktualisieren';
  mi.OnClick := LocalUpdateItemClick;
end;

procedure TAniRotationForm.HandleMouseWheel(Shift: TShiftState; WheelDelta: Integer);
var
  wd: Integer;
begin
  wd := WheelDelta; // small values may come from touchpad

  { normal mouse }
  if Abs(WheelDelta) > 100 then
    wd := WheelDelta div 120;;

  if ssCtrl in Shift then
  begin
    Zoom(wd);
  end
  else if ssShift in Shift then
  begin
    DoBigWheel(wd);
  end
  else if Shift = [] then
  begin
    DoSmallWheel(wd);
  end;
end;

procedure TAniRotationForm.Zoom(Delta: single);
begin
  FZoom := FZoom + FZoom * FZoomBase * Sign(Delta);
  RaumGraph.Zoom := FZoom;
  Draw;
end;

procedure TAniRotationForm.DoSmallWheel(Delta: single);
var
  f: single;
begin
  f := GetSmallStep;
  if Delta > 0 then
    DoWheel(f)
  else
    DoWheel(-f);
end;

procedure TAniRotationForm.DoBigWheel(Delta: single);
var
  f: single;
begin
  f := GetBigStep;
  if Delta > 0 then
    DoWheel(f)
  else
    DoWheel(-f);
end;

procedure TAniRotationForm.DoWheel(Delta: single);
begin
  Trackbar.Position := Trackbar.Position + Round(Delta);
end;

function TAniRotationForm.GetBigStep: single;
begin
  case Parameter of
    fpWante: result := 2;
    fpWinkel: result := 1;
    fpSalingW: result := 1;
    else
      result := 10;
  end;
end;

function TAniRotationForm.GetSmallStep: single;
begin
  result := 1;
end;

end.

