﻿unit RiggVar.RG.Speed04;

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
  TActionSpeedBarRG04 = class(TActionSpeedBar)
  private
    SeiteBtn: TSpeedButton;
    TopBtn: TSpeedButton;
    AchternBtn: TSpeedButton;
    NullBtn: TSpeedButton;

    ZoomInBtn: TSpeedButton;
    ZoomOutBtn: TSpeedButton;

    BogenBtn: TSpeedButton;
    KoppelBtn: TSpeedButton;
    SortedBtn: TSpeedButton;

    MemoryBtn: TSpeedButton;
    MemoryRecallBtn: TSpeedButton;
  public
    procedure InitSpeedButtons; override;
  end;

implementation

uses
  RiggVar.FB.ActionConst;

{ TActionSpeedBarRG04 }

procedure TActionSpeedBarRG04.InitSpeedButtons;
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
  sb.Tag := faRggBogen;
  sb.GroupIndex := NextGroupIndex;
  InitSpeedButton(sb);

  sb := AddSpeedBtn('KoppelBtn', 0);
  KoppelBtn := sb;
  sb.AllowAllUp := True;
  sb.GroupIndex := NextGroupIndex;
  sb.Tag := faRggKoppel;
  InitSpeedButton(sb);

  sb := AddSpeedBtn('SortedBtn', 0);
  SortedBtn := sb;
  sb.AllowAllUp := True;
  sb.GroupIndex := NextGroupIndex;
  sb.Tag := faToggleSortedRota;
  InitSpeedButton(sb);

  { Zoom Buttons }

  BtnColorValue := clvZoom;

  sb := AddSpeedBtn('ZoomOutBtn', BtnGroupSpace);
  ZoomOutBtn := sb;
  sb.Tag := faRggZoomOut;
  InitSpeedButton(sb);

  sb := AddSpeedBtn('ZoomInBtn', 0);
  ZoomInBtn := sb;
  sb.Tag := faRggZoomIn;
  InitSpeedButton(sb);

  { ViewPoint Buttons }

  BtnColorValue := clvView;

  sb := AddSpeedBtn('SeiteBtn', BtnGroupSpace);
  SeiteBtn := sb;
  sb.Tag := faViewpointS;
  InitSpeedButton(sb);

  sb := AddSpeedBtn('AchternBtn', 0);
  AchternBtn := sb;
  sb.Tag := faViewpointA;
  InitSpeedButton(sb);

  sb := AddSpeedBtn('TopBtn', 0);
  TopBtn := sb;
  sb.Tag := faViewpointT;
  InitSpeedButton(sb);

  sb := AddSpeedBtn('NullBtn', 0);
  NullBtn := sb;
  sb.Tag := faViewpoint3;
  InitSpeedButton(sb);
end;

end.
