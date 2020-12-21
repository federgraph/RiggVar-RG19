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
    HullBtn: TSpeedButton;
  public
    procedure InitSpeedButtons; override;
  end;

implementation

uses
  RiggVar.FB.ActionConst;

{ TActionSpeedBarRG03 }

procedure TActionSpeedBarRG03.InitSpeedButtons;
var
  sb: TSpeedBtn;
begin
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

  sb := AddSpeedBtn('HullBtn', 0);
  HullBtn := sb;
  sb.AllowAllUp := True;
  sb.GroupIndex := NextGroupIndex;
  sb.Tag := faRggHull;
  InitSpeedButton(sb);
end;

end.
