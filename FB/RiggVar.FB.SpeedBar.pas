unit RiggVar.FB.SpeedBar;

interface

{$ifdef fpc}
{$mode delphi}
{$endif}

uses
  Classes,
  StdCtrls,
  ExtCtrls,
  Buttons,
  RiggVar.FB.Color,
  RiggVar.FB.SpeedColor;

{$define Vcl}

type
  TSpeedBtn = class(TSpeedButton)
  public
    ColorValue: TSpeedColorValue;
    IsFirstInGroup: Boolean;
    SpecialWidth: Integer;
  end;

  TActionSpeedBar = class(TPanel)
  private
    FDarkMode: Boolean;
    FBigMode: Boolean;
    FScale: single;
    procedure InitLayoutProps;
    procedure SetDarkMode(const Value: Boolean);
    procedure SetBigMode(const Value: Boolean);
  protected
    BtnColorHot: TRggColor;
    BtnColorValue: TSpeedColorValue;
    BtnColor: TRggColor;
    BtnTop: Integer;
    BtnLeft: Integer;
    BtnWidth: Integer;
    BtnHeight: Integer;
    BtnCounter: Integer;
    BtnSpace: Integer;
    BtnGroupSpace: Integer;
    SpeedPanelHeight: Integer;
    SpeedPanelFontSize: Integer;
    TempGroupSpace: Integer;
    TempGroupIndex: Integer;
    function AddSpeedBtn(N: string; AGroupSpace: Integer = 0): TSpeedBtn;
    function RefSpeedBtn(B: TSpeedButton; AGroupSpace: Integer = 0): TSpeedButton;
    procedure UpdateLayoutForBtn(B: TSpeedButton; AGroupSpace: Integer);
    procedure InitSpeedButton(SB: TSpeedBtn);
    procedure SpeedButtonClick(Sender: TObject); virtual;
    procedure UpdateCaptions;
    procedure UpdateHints;
    function NextGroupIndex: Integer;
  public
    class var SpeedColorScheme: TSpeedColorScheme; // injected

    constructor Create(AOwner: TComponent); override;

    procedure UpdateLayout;
    procedure UpdateColor;
    procedure ToggleBigMode;

    procedure InitSpeedButtons; virtual;
    procedure UpdateSpeedButtonDown; virtual;
    procedure UpdateSpeedButtonEnabled; virtual;

    property DarkMode: Boolean read FDarkMode write SetDarkMode;
    property BigMode: Boolean read FBigMode write SetBigMode;
    property PanelHeight: Integer read SpeedPanelHeight;
  end;

  TActionSpeedBarExample = class(TActionSpeedBar)
  private
    procedure TestBtnClick(Sender: TObject);
  public
    TestBtn: TSpeedButton;
    procedure InitSpeedButtons; override;
  end;

implementation

uses
  RiggVar.App.Main,
  RiggVar.FB.ActionConst,
  RiggVar.FB.ActionShort,
  RiggVar.FB.ActionLong;

constructor TActionSpeedBar.Create(AOwner: TComponent);
begin
  inherited;
  FScale := MainVar.Scale;
  InitLayoutProps;
//  InitSpeedButtons; // Main is still nil (by design)
  DarkMode := True;
  BigMode := True;
  ShowCaption := False;
end;

procedure TActionSpeedBar.InitLayoutProps;
begin
  BtnColorHot := TRggColors.Beige;
  BtnColor := TRggColors.Blue;
  BtnCounter := 0;
  BtnLeft := 0;
  BtnTop := 3;
  BtnSpace := 2;
  BtnGroupSpace := 16;
  BtnWidth := 50;
  BtnHeight := 50;
  SpeedPanelFontSize := 16;
  SpeedPanelHeight := BtnHeight + 2 * BtnTop;
end;

function TActionSpeedBar.AddSpeedBtn(N: string; AGroupSpace: Integer): TSpeedBtn;
begin
  result := TSpeedBtn.Create(Self);
  result.Parent := Self;
  result.Name := N;
  RefSpeedBtn(result, AGroupSpace);
end;

function TActionSpeedBar.RefSpeedBtn(B: TSpeedButton; AGroupSpace: Integer): TSpeedButton;
begin
  result := B;
  BtnLeft := BtnLeft + AGroupSpace;
{$ifdef Vcl}
  B.Left := BtnLeft + BtnCounter * BtnWidth + BtnSpace;
  B.Top := BtnTop;
{$endif}
{$ifdef FMX}
  B.Position.X := BtnLeft + BtnCounter * (BtnWidth + BtnSpace);
  B.Position.Y := BtnTop;
{$endif}
  B.Width := BtnWidth;
  B.Height := BtnHeight;
{$ifdef FMX}
  { Does not work. }
  { Because B not assigned yet to actual SpeedButton instance ? }
//  InitSpeedButton(B);
  { also maybe not wanted to do this here }
{$endif}
{$ifdef Vcl}
  B.Font.Name := 'Consolas';
  B.Font.Size := 12;
  B.Font.Color := BtnColor;
{$endif}
  Inc(BtnCounter);
  TempGroupSpace := AGroupSpace;
end;

procedure TActionSpeedBar.UpdateLayoutForBtn(B: TSpeedButton; AGroupSpace: Integer);
begin
  BtnLeft := BtnLeft + AGroupSpace;
{$ifdef Vcl}
  B.Left := BtnLeft + BtnCounter * (BtnWidth + BtnSpace);
  B.Top := BtnTop;
{$endif}
{$ifdef FMX}
  B.Position.X := BtnLeft + BtnCounter * (BtnWidth + BtnSpace);
  B.Position.Y := BtnTop;
{$endif}
  B.Width := BtnWidth;
  B.Height := BtnHeight;
  Inc(BtnCounter);
end;

procedure TActionSpeedBar.UpdateLayout;
var
  i: Integer;
  cr: TComponent;
  sb: TSpeedBtn;
  gs: Integer;
begin
  InitLayoutProps;
  BigMode := BigMode;
  for i := 0 to ComponentCount-1 do
  begin
    cr := Components[i];
    if cr is TSpeedBtn then
    begin
      sb := cr as TSpeedBtn;
      if sb.IsFirstInGroup then
        gs := BtnGroupSpace
      else
        gs := 0;
      UpdateLayoutForBtn(sb, gs);
      if sb.SpecialWidth > 0 then
      begin
        BtnLeft := BtnLeft + Round(sb.SpecialWidth - sb.Width);
        sb.Width := sb.SpecialWidth;
      end;
      sb.Visible := sb.Left < self.Width - sb.Width;
    end;
  end;
end;

procedure TActionSpeedBar.UpdateCaptions;
var
  i: Integer;
  cr: TComponent;
  sb: TSpeedButton;
begin
  for i := 0 to ComponentCount-1 do
  begin
    cr := Components[i];
    if cr is TSpeedButton then
    begin
      sb := cr as TSpeedButton;
      sb.Caption := GetFederActionShort(sb.Tag);
    end;
  end;
end;

procedure TActionSpeedBar.UpdateHints;
var
  i: Integer;
  cr: TComponent;
  sb: TSpeedButton;
begin
  for i := 0 to ComponentCount-1 do
  begin
    cr := Components[i];
    if cr is TSpeedButton then
    begin
      sb := cr as TSpeedButton;
      sb.Caption := GetFederActionLong(sb.Tag);
    end;
  end;
end;

procedure TActionSpeedBar.UpdateColor;
var
  i: Integer;
  cr: TComponent;
  sb: TSpeedBtn;
begin
  for i := 0 to ComponentCount-1 do
  begin
    cr := Components[i];
    if cr is TSpeedBtn then
    begin
      sb := cr as TSpeedBtn;
      BtnColorValue := sb.ColorValue;
      sb.Font.Color := SpeedColorScheme.GetColor(sb.ColorValue);
    end;
  end;
end;

procedure TActionSpeedBar.ToggleBigMode;
var
  i: Integer;
  cr: TComponent;
  sb: TSpeedButton;
begin
  BigMode := not BigMode;

  UpdateLayout;

  for i := 0 to ComponentCount-1 do
  begin
    cr := Components[i];
    if cr is TSpeedButton then
    begin
      sb := cr as TSpeedButton;
      sb.Font.Size := SpeedPanelFontSize;
    end;
  end;
end;

procedure TActionSpeedBar.InitSpeedButton(SB: TSpeedBtn);
var
  cla: TRggColor;
begin
  cla := SpeedColorScheme.GetColor(BtnColorValue);

  if SB.Tag <> faNoop then
  begin
    sb.Text := GetFederActionShort(SB.Tag);
    sb.Hint := GetFederActionLong(SB.Tag);
    sb.OnClick := SpeedButtonClick;
  end;

  SB.Font.Size := SpeedPanelFontSize;
  SB.Font.Color := cla;

  SB.ColorValue := BtnColorValue;
  if TempGroupSpace = BtnGroupSpace then
    SB.IsFirstInGroup := True
  else
    SB.IsFirstInGroup := False;

  if sb.Width < sb.SpecialWidth then
  begin
    BtnLeft := BtnLeft + Round(sb.SpecialWidth - sb.Width);
    sb.Width := sb.SpecialWidth;
  end;
end;

procedure TActionSpeedBar.SetBigMode(const Value: Boolean);
begin
  FBigMode := Value;
  if Value then
  begin
    BtnWidth := Round(50 * FScale);
    BtnHeight := Round(50 * FScale);
    SpeedPanelFontSize := 18;
    SpeedPanelHeight := BtnHeight + 2 * BtnTop;
  end
  else
  begin
    BtnWidth := Round(35 * FScale);
    BtnHeight := Round(30 * FScale);;
    SpeedPanelFontSize := 12;
    SpeedPanelHeight := BtnHeight + 2 * BtnTop;
  end;
end;

procedure TActionSpeedBar.SetDarkMode(const Value: Boolean);
begin
  FDarkMode := Value;
  if Value then
    SpeedColorScheme.InitDark
  else
    SpeedColorScheme.InitLight;
end;

function TActionSpeedBar.NextGroupIndex: Integer;
begin
  Inc(TempGroupIndex);
  result := TempGroupIndex;
end;

procedure TActionSpeedBar.InitSpeedButtons;
begin
  { virtual }
end;

procedure TActionSpeedBar.SpeedButtonClick(Sender: TObject);
begin
  { virtual }
end;

procedure TActionSpeedBar.UpdateSpeedButtonDown;
begin
  { virtual }
end;

procedure TActionSpeedBar.UpdateSpeedButtonEnabled;
begin
  { virtual }
end;

{ TActionSpeedBarExample }

procedure TActionSpeedBarExample.InitSpeedButtons;
var
  sb: TSpeedBtn;
begin
  { Test Buttons }

  BtnColor := TRggColors.Teal;
  BtnColorValue := clvProp;

  sb := AddSpeedBtn('TestBtn');
  TestBtn := sb;
  sb.Text := 'LC';
  sb.Hint := 'Toggle WantLineColors';
  sb.AllowAllUp := True;
  sb.Down := False;
  sb.OnClick := TestBtnClick;
  sb.Tag := faNoop;
  InitSpeedButton(sb);
end;

procedure TActionSpeedBarExample.TestBtnClick(Sender: TObject);
begin

end;

end.
