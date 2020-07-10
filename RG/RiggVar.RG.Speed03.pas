unit RiggVar.RG.Speed03;

interface

{$ifdef fpc}
{$mode delphi}
{$endif}

uses
  RiggVar.FB.SpeedBar,
  RiggVar.FB.SpeedColor,
  Classes,
  Buttons;

type
  TActionSpeedBarRG03 = class(TActionSpeedBar)
  private
    ColorModeBtn: TSpeedButton;
    FontSizeBtn: TSpeedButton;

    MemoryBtn: TSpeedButton;
    MemoryRecallBtn: TSpeedButton;

    BogenBtn: TSpeedButton;
    KoppelBtn: TSpeedButton;

    SimpleBtn: TSpeedButton;
    NormalBtn: TSpeedButton;
    GrauBtn: TSpeedButton;
    BlauBtn: TSpeedButton;
    MultiBtn: TSpeedButton;
    DisplayBtn: TSpeedButton;
    QuickBtn: TSpeedButton;

    LegendBtn: TSpeedButton;
    LineColorBtn: TSpeedButton;

  private
    procedure ToggleColorModeBtnClick(Sender: TObject);
    procedure ToggleFontSizeBtnClick(Sender: TObject);
  protected
    procedure SpeedButtonClick(Sender: TObject); override;
  public
    procedure InitSpeedButtons; override;
    procedure UpdateSpeedButtonDown; override;
  end;

implementation

uses
  FrmMain,
  RiggVar.App.Main,
  RggTypes,
  RiggVar.FB.ActionConst;

{ TActionSpeedBarRG03 }

procedure TActionSpeedBarRG03.SpeedButtonClick(Sender: TObject);
var
  fa: Integer;
begin
  fa := (Sender as TComponent).Tag;
  case fa of
    faMemoryBtn: FormMain.MemoryBtnClick(Sender);
    faMemoryRecallBtn: FormMain.MemoryRecallBtnClick(Sender);

    faRggBogen: FormMain.BogenBtnClick(Sender);
    faRggKoppel: FormMain.KoppelBtnClick(Sender);

    faSuperSimple: FormMain.SuperSimpleBtnClick(Sender);
    faSuperNormal: FormMain.SuperNormalBtnClick(Sender);
    faSuperGrau: FormMain.SuperGrauBtnClick(Sender);
    faSuperBlau: FormMain.SuperBlauBtnClick(Sender);
    faSuperMulti: FormMain.SuperMultiBtnClick(Sender);
    faSuperDisplay: FormMain.SuperDisplayBtnClick(Sender);
    faSuperQuick: FormMain.SuperQuickBtnClick(Sender);

    faToggleLineColor: FormMain.LineColorBtnClick(Sender);
    faToggleShowLegend: FormMain.RotaForm.LegendBtnClick(Sender);
  end;

  UpdateSpeedButtonDown;
end;

procedure TActionSpeedBarRG03.UpdateSpeedButtonDown;
begin
  MemoryBtn.Down := False;
  MemoryRecallBtn.Down := False;

  BogenBtn.Down := Main.GetChecked(faRggBogen);
  KoppelBtn.Down := Main.GetChecked(faRggKoppel);

  SimpleBtn.Down := Main.GetChecked(faSuperSimple);
  NormalBtn.Down := Main.GetChecked(faSuperNormal);
  GrauBtn.Down := Main.GetChecked(faSuperGrau);
  BlauBtn.Down := Main.GetChecked(faSuperBlau);
  MultiBtn.Down := Main.GetChecked(faSuperMulti);
  DisplayBtn.Down := Main.GetChecked(faSuperDisplay);
  QuickBtn.Down := Main.GetChecked(faSuperQuick);

  LegendBtn.Down := Main.GetChecked(faToggleShowLegend);
  LineColorBtn.Down := Main.GetChecked(faToggleLineColor);
end;

procedure TActionSpeedBarRG03.InitSpeedButtons;
var
  sb: TSpeedBtn;
begin
  { Special Buttons }

  BtnColorValue := clvScheme;

  sb := AddSpeedBtn('FontSizeBtn', BtnGroupSpace);
  FontSizeBtn := sb;
  sb.Caption := 'FS';
  sb.Hint := 'Toggle FontSize';
  sb.OnClick := ToggleFontSizeBtnClick;
  sb.Tag := faNoop;
  InitSpeedButton(sb);

  sb := AddSpeedBtn('ColorModeBtn');
  ColorModeBtn := sb;
  sb.Caption := 'CM';
  sb.Hint := 'Toggle ColorMode';
  sb.OnClick := ToggleColorModeBtnClick;
  sb.Tag := faNoop;
  InitSpeedButton(sb);

  { Memory Buttons }

  BtnColorValue := clvMemory;

  sb := AddSpeedBtn('MemoryBtn', BtnGroupSpace);
  MemoryBtn := sb;
  sb.Tag := faMemoryBtn;
  InitSpeedButton(sb);

  sb := AddSpeedBtn('MemoryRecallBtn', 0);
  MemoryRecallBtn := sb;
  sb.Tag := faMemoryRecallBtn;
  InitSpeedButton(sb);

  { Bogen and Koppel }

  BtnColorValue := clvBogen;

  sb := AddSpeedBtn('BogenBtn', BtnGroupSpace);
  BogenBtn := sb;
  sb.AllowAllUp := True;
  sb.GroupIndex := NextGroupIndex;
  sb.Tag := faRggBogen;
  InitSpeedButton(sb);

  sb := AddSpeedBtn('KoppelBtn', 0);
  KoppelBtn := sb;
  sb.AllowAllUp := True;
  sb.GroupIndex := NextGroupIndex;
  sb.Tag := faRggKoppel;
  InitSpeedButton(sb);

  { Rigg Buttons }

  BtnColorValue := clvRigg;

  sb := AddSpeedBtn('SimpleBtn', BtnGroupSpace);
  SimpleBtn := sb;
  sb.AllowAllUp := True;
  sb.GroupIndex := NextGroupIndex;
  sb.Tag := faSuperSimple;
  InitSpeedButton(sb);

  sb := AddSpeedBtn('NormalBtn', 0);
  NormalBtn := sb;
  sb.AllowAllUp := True;
  sb.GroupIndex := NextGroupIndex;
  sb.Tag := faSuperNormal;
  InitSpeedButton(sb);

  sb := AddSpeedBtn('GrauBtn', 0);
  GrauBtn := sb;
  sb.AllowAllUp := True;
  sb.GroupIndex := NextGroupIndex;
  sb.Tag := faSuperGrau;
  InitSpeedButton(sb);

  sb := AddSpeedBtn('BlauBtn', 0);
  BlauBtn := sb;
  sb.AllowAllUp := True;
  sb.GroupIndex := NextGroupIndex;
  sb.Tag := faSuperBlau;
  InitSpeedButton(sb);

  sb := AddSpeedBtn('MultiBtn', 0);
  MultiBtn := sb;
  sb.AllowAllUp := True;
  sb.GroupIndex := NextGroupIndex;
  sb.Tag := faSuperMulti;
  InitSpeedButton(sb);

  sb := AddSpeedBtn('DisplayBtn', 0);
  DisplayBtn := sb;
  sb.AllowAllUp := True;
  sb.GroupIndex := NextGroupIndex;
  sb.Tag := faSuperDisplay;
  InitSpeedButton(sb);

  sb := AddSpeedBtn('QuickBtn', 0);
  QuickBtn := sb;
  sb.AllowAllUp := True;
  sb.GroupIndex := NextGroupIndex;
  sb.Tag := faSuperQuick;
  InitSpeedButton(sb);

  BtnColorValue := clvGraph;

  sb := AddSpeedBtn('LegendBtn', BtnGroupSpace);
  LegendBtn := sb;
  sb.AllowAllUp := True;
  sb.GroupIndex := NextGroupIndex;
  sb.Tag := faToggleShowLegend;
  InitSpeedButton(sb);

  sb := AddSpeedBtn('LineColorBtn', 0);
  LineColorBtn := sb;
  sb.AllowAllUp := True;
  sb.GroupIndex := NextGroupIndex;
  sb.Tag := faToggleLineColor;
  InitSpeedButton(sb);
end;

procedure TActionSpeedBarRG03.ToggleColorModeBtnClick(Sender: TObject);
begin
  Main.ToggleDarkMode;
end;

procedure TActionSpeedBarRG03.ToggleFontSizeBtnClick(Sender: TObject);
begin
  ToggleBigMode;
  FormMain.LayoutComponents;
  FormMain.CheckSpaceForMemo;
  FormMain.CheckSpaceForImages;
end;

end.
