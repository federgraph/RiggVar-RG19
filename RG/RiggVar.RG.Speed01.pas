unit RiggVar.RG.Speed01;

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
  TActionSpeedBarRG01 = class(TActionSpeedBar)
  private
    MT0Btn: TSpeedButton;
    ReadTrimmFileBtn: TSpeedButton;
    SaveTrimmFileBtn: TSpeedButton;
    CopyTrimmFileBtn: TSpeedButton;
    CopyTrimmItemBtn: TSpeedButton;
    PasteTrimmItemBtn: TSpeedButton;
    CopyAndPasteBtn: TSpeedButton;

    M1Btn: TSpeedButton;
    M10Btn: TSpeedButton;
    P1Btn: TSpeedButton;
    P10Btn: TSpeedButton;

    SandboxedBtn: TSpeedButton;
    AllPropsBtn: TSpeedButton;
    AllTagsBtn: TSpeedButton;

    ColorModeBtn: TSpeedButton;
    FontSizeBtn: TSpeedButton;
  public
    procedure InitSpeedButtons; override;
  end;

implementation

uses
  RiggVar.App.Main,
  RiggVar.FB.ActionConst;

{ TActionSpeedBarRG01 }

procedure TActionSpeedBarRG01.InitSpeedButtons;
var
  sb: TSpeedBtn;
begin
  { Special Buttons }

  BtnColorValue := clvData;

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

  { Check Box Buttons }

  BtnColorValue := clvOption;

  sb := AddSpeedBtn('SandboxedBtn', BtnGroupSpace);
  SandboxedBtn := sb;
  sb.Caption := 'SX';
  if MainConst.MustBeSandboxed then
  begin
    sb.Hint := 'Sandboxed (Store)';
    sb.AllowAllUp := True;
    sb.GroupIndex := NextGroupIndex;
    sb.Down := False;
    sb.Tag := faNoop;
    InitSpeedButton(sb);
    sb.Enabled := False;
  end
  else
  begin
    sb.Hint := 'Sandboxed';
    sb.AllowAllUp := True;
    sb.GroupIndex := NextGroupIndex;
    sb.Down := False;
    sb.Tag := faToggleSandboxed;
    InitSpeedButton(sb);
  end;

  BtnColorValue := clvProp;

  sb := AddSpeedBtn('AllPropsBtn');
  AllPropsBtn := sb;
  sb.Caption := 'AP';
  sb.Hint := 'All Trimm Props';
  sb.AllowAllUp := True;
  sb.GroupIndex := NextGroupIndex;
  sb.Down := False;
  InitSpeedButton(sb);

  sb := AddSpeedBtn('AllTagsBtn');
  AllTagsBtn := sb;
  sb.Caption := 'AT';
  sb.Hint := 'All Xml Tags';
  sb.AllowAllUp := True;
  sb.GroupIndex := NextGroupIndex;
  sb.Down := False;
  sb.Tag := faToggleAllTags;
  InitSpeedButton(sb);

  { Data Buttons }

  BtnColorValue := clvData;

  sb := AddSpeedBtn('MT0Btn', BtnGroupSpace);
  MT0Btn := sb;
  sb.Caption := 'MT0';
  sb.Hint := 'Update Trimm 0';
  sb.Tag := faUpdateTrimm0;
  InitSpeedButton(sb);

  sb := AddSpeedBtn('ReadTrimmFileBtn');
  ReadTrimmFileBtn := sb;
  sb.Caption := 'rtf';
  sb.Hint := 'Read Trimm File';
  sb.Tag := faReadTrimmFile;
  InitSpeedButton(sb);

  sb := AddSpeedBtn('SaveTrimmFileBtn');
  SaveTrimmFileBtn := sb;
  sb.Caption := 'stf';
  sb.Hint := 'Save Trimm File';
  sb.Tag := faSaveTrimmFile;
  InitSpeedButton(sb);

  sb := AddSpeedBtn('CopyTrimmFileBtn');
  CopyTrimmFileBtn := sb;
  sb.Caption := 'ctf';
  sb.Hint := 'Copy Trimm File';
  sb.Tag := faCopyTrimmFile;
  InitSpeedButton(sb);

  sb := AddSpeedBtn('CopyTrimmItemBtn');
  CopyTrimmItemBtn := sb;
  sb.Caption := 'cti';
  sb.Hint := 'Copy Trimm Item';
  sb.Tag := faCopyTrimmItem;
  InitSpeedButton(sb);

  sb := AddSpeedBtn('PasteTrimmItemBtn');
  PasteTrimmItemBtn := sb;
  sb.Caption := 'pti';
  sb.Hint := 'Paste Trimm Item';
  sb.Tag := faPasteTrimmItem;
  InitSpeedButton(sb);

  sb := AddSpeedBtn('CopyAndPasteBtn');
  CopyAndPasteBtn := sb;
  sb.Caption := 'M';
  sb.Hint := 'Copy And Paste';
  sb.Tag := faCopyAndPaste;
  InitSpeedButton(sb);

  { Param Value Buttons }

  BtnColorValue := clvWheel;

  sb := AddSpeedBtn('M10Btn', BtnGroupSpace);
  M10Btn := sb;
  sb.Caption := '-10';
  sb.Hint := 'Param Value Minus 10';
  sb.Tag := faParamValueMinus10;
  InitSpeedButton(sb);

  sb := AddSpeedBtn('M1Btn');
  M1Btn := sb;
  sb.Caption := '-1';
  sb.Hint := 'Param Value Minus 1';
  sb.Tag := faParamValueMinus1;
  InitSpeedButton(sb);

  sb := AddSpeedBtn('P1Btn');
  P1Btn := sb;
  sb.Caption := '+1';
  sb.Hint := 'Param Value Plus 1';
  sb.Tag := faParamValuePlus1;
  InitSpeedButton(sb);

  sb := AddSpeedBtn('P10Btn');
  P10Btn := sb;
  sb.Caption := '+10';
  sb.Hint := 'Param Value Plus 10';
  sb.Tag := faParamValuePlus10;
  InitSpeedButton(sb);
end;

end.
