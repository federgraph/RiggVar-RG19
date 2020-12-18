unit RiggVar.DT.Ctrls;

interface

{$ifdef fpc}
{$mode delphi}
{$endif}

uses
{$ifdef fpc}
  LCLIntf,
  LCLType,
{$endif}
{$ifdef MSWindows}
  Windows,
{$endif}
  SysUtils,
  Classes,
  Graphics,
  Controls,
  ExtCtrls,
  RiggVar.RG.Types,
  RiggVar.DT.Profile;

type
  TFigure = (
    dtTest,
    dtSalingAll,
    dtSalingDetail,
    dtController,
    dtProfileDrawHoch,
    dtProfileDrawQuer,
    dtProfileOuter,
    dtProfileInner,
    dtProfileLeft,
    dtProfileRight
  );

  TSalingGraph = class(TMastProfile)
  private
    FImage: TImage;
    FBitmap: TBitmap;
    procedure SetImage(const Value: TImage);
    procedure InitBitmap;
    procedure PaintBackground(g: TCanvas);
  public
    BackgroundColor: TColor;
    ControllerTyp: TControllerTyp;
    Width: Integer;
    Height: Integer;

    EdgePos: Integer; { Abstand von E0 zur Anschlagkante Deck + Klotzdicke }
    ControllerPos: Integer; { Abstand(iP[ooE0,x], iP[ooE ,x]) in mm }
    ParamXE: double; { Abstand(iP[ooD0,x], iP[ooE,x]) in mm }
    ParamXE0: Integer; { Abstand(iP[ooD0,x], iP[ooE0,x]) in mm }

    SalingA: Integer; { Abstand(iP[ooA,x], iP[ooB,x]) in mm }
    SalingH: Integer; { Abstand Verbindungslinie Salinge zu Hinterkante Mast in mm }
    SalingL: Integer; { Salinglänge in mm - außerhalb berechnen }
    SalingHOffset: Integer; { Abstand Hinterkante Mast zur neutrale Faser in mm }
    SalingDetail: Boolean; { Umschalten zwischen den beiden SalingViews }

    ImageOpacity: single;

    constructor Create;
    destructor Destroy; override;

    procedure DrawSaling(g: TCanvas);
    procedure DrawController(g: TCanvas);

    procedure Draw(df: TFigure);

    property Image: TImage read FImage write SetImage;
  end;

implementation

uses
  RiggVar.App.Main,
  RiggVar.FB.Color;

constructor TSalingGraph.Create;
begin
  inherited;

  BackgroundColor := clBtnFace;

  Width := 453; // unscaled value
  Height := 220; // unscaled value

  ControllerZoom := 1;
  SalingZoom := 5;
  ControllerTyp := ctDruck;

  { Properties für ControllerGraph in mm }
  EdgePos := 25;
  ControllerPos := 80;
  ParamXE := -20;
  ParamXE0 := 110;

  { Properties für SalingGraph in mm }
  SalingHOffset := 37;
  SalingH := 80;
  SalingA := 800;
  SalingL := 1000;
end;

procedure TSalingGraph.DrawSaling(g: TCanvas);
var
  SalingX, SalingY: Integer;
  PosX, PosY: Integer;
  s: string;
  f: single;
  radius: Integer;
  ox, oy, lh: Integer;
begin
  SetViewPortOrgEx(g.Handle,
    Round(Width * MainVar.Scale/2),
    Round((Height-40) * MainVar.Scale), nil);

  radius := Round(5 * MainVar.Scale);
  f := 0.4 * MainVar.Scale;

  SalingX := Round(SalingA / 2 * f);
  SalingY := -Round(SalingH * f);

  { SalingL }
  g.Pen.Width := Round(15 * MainVar.Scale);
  g.Pen.Color := TRggColors.Cornflowerblue;
  g.MoveTo(-SalingX, 0);
  g.LineTo( 0, SalingY);
  g.LineTo(SalingX, 0);

  { Wanten als Kreise }
  SalingY := -Round(SalingH * f);
  g.Pen.Width := Round(1 * MainVar.Scale);
  g.Pen.Color := clBlack;
  g.Brush.Color := clRed;
  g.Ellipse(-SalingX - radius, -radius, -SalingX + radius, radius);
  g.Ellipse( SalingX - radius, -radius,  SalingX + radius, radius);
  g.Ellipse( -radius, SalingY - radius,  radius, SalingY + radius);

  { SalingA }
  g.Pen.Width := Round(2 * MainVar.Scale);
  g.Pen.Color := clAqua;
  g.MoveTo(-SalingX, 0);
  g.LineTo( SalingX, 0);

  { SalingH }
  SalingY := -Round(SalingH * f);
  g.Pen.Color := clSilver;
  g.MoveTo( 0, 0);
  g.LineTo( 0, SalingY);

  { SalingH - SalingHOffset }
  SalingY := -Round((SalingH-SalingHOffset) * f);
  g.Pen.Color := clFuchsia;
  g.MoveTo( -4, 0);
  g.LineTo( -4, SalingY);

  { Profile }
  g.Pen.Color := TRggColors.Dodgerblue;
  Lage := hoch;
  OffsetX := 0;
  OffsetY := -Round((SalingH-SalingHOffset) * f) ;
  WantSegmentColor := False;
  WantRight := False;
  WantLeft := False;
  WantOuter := True;
  WantInner := False;
  ProfileZoom := f;
  InternalDrawProfile6(g);

  { Text }
  SetViewPortOrgEx(g.Handle, 0, 0, nil);
  g.Brush.Style := bsClear;
  g.Font.Height := Round(20 * MainVar.Scale);

  ox := Round(20 * MainVar.Scale);
  oy := Round(20 * MainVar.Scale);
  lh := Round(40 * MainVar.Scale);

  g.Font.Color := clFuchsia;
  PosX := ox;
  PosY := oy + 0 * lh;
  s := Format('Salinghöhe - Offset = %d mm', [SalingH - SalingHOffset]);
  g.TextOut(PosX, PosY, s);

  g.Font.Color := TRggColors.Dodgerblue;
  PosX := ox;
  PosY := oy + 1 * lh;
  s := Format('Salinglänge = %d mm',[SalingL]);
  g.TextOut(PosX, PosY, s);

  g.Font.Color := clSilver;
  PosX := ox;
  PosY := oy + 2 * lh;
  s := Format('Salinghöhe = %d mm',[SalingH]);
  g.TextOut(PosX, PosY, s);

  g.Font.Color := clYellow;
  PosX := 20;
  PosY := oy + 3 * lh;
  s := Format('SalingHOffset = %d mm', [SalingHOffset]);
  g.TextOut(PosX, PosY, s);

  g.Font.Color := clAqua;
  PosX := 100;
  PosY := oy + 4 * lh;
  s := Format('Salingabstand = %d mm',[SalingA]);
  g.TextOut(PosX, PosY, s);
end;

procedure TSalingGraph.SetImage(const Value: TImage);
begin
  FImage := Value;
  InitBitmap;
end;

procedure TSalingGraph.InitBitmap;
begin
  if FBitmap <> nil then
  begin
    FBitmap.Free;
  end;
  FBitmap := TBitmap.Create;
  FBitmap.Width := Round(Width * MainVar.Scale);
  FBitmap.Height := Round(Height * MainVar.Scale);
  Image.Width := FBitmap.Width;
  Image.Height := FBitmap.Height;
end;

procedure TSalingGraph.DrawController(g: TCanvas);
var
  i: Integer;
  KlotzX1: Integer;
  KlotzX2: Integer;
  PosXE0: Integer;
  StrichX: Integer;

  PositionXE0: Integer;
  PositionXE: Integer;
  ProfilePosMastfuss: Integer;
  ProfilePosXE: double;
  PositionEdge: Integer;

  vPositionXE0: Integer;
  vPositionXE: Integer;
  vProfilePosMastfuss: Integer;
  vProfilePosXE: double;
  vPositionEdge: Integer;

  s: string;
  clDeck: TColor;
  clMarke: TColor;
  clMassband: TColor;
  txtHeight: Integer;
  u, v: Integer;
  f: single;

  v1: Integer;
  v3: Integer;
  v5: Integer;
  v6: Integer;
  v7: Integer;
  v15: Integer;
  v80: Integer;
  v32: Integer;
  v20: Integer;
  v30: Integer;
  v40: Integer;
  v50: Integer;
  v60: Integer;
  v10: Integer;
  v85: Integer;
  v100: Integer;
  v105: Integer;
begin
  clDeck := TRggColors.Cornflowerblue;
  clMarke := clYellow;
  clMassband := clGray;

  PositionXE0 := 95; { Position der Ablesemarke, Konstante in der Grafik }
  PositionXE := PositionXE0-ControllerPos; { Position linke Kante Mastklotz }
  PositionEdge := PositionXE0-EdgePos + 15; { Abstand Deckanschlag - E0 }
  ProfilePosMastfuss := PositionXE0-ParamXE0-72; { Position Hinterkante Mastfuss }
  ProfilePosXE := ProfilePosMastfuss+ParamXE; { Position Hinterkante Mast in Höhe Punkt E }

  f := 2.0 * MainVar.Scale;

  vPositionXE0 := Round(PositionXE0 * f);
  vPositionXE := Round(PositionXE * f);
  vPositionEdge := Round(PositionEdge * f);
  vProfilePosMastfuss := Round(ProfilePosMastfuss * f);
  vProfilePosXE := Round(ProfilePosXE * f);

  v1 := Round(1 * f);
  v3 := Round(3 * f);
  v5 := Round(5 * f);
  v6 := Round(6 * f);
  v7 := Round(7 * f);
  v10 := Round(10 * f);
  v15 := Round(15 * f);
  v20 := Round(20 * f);
  v30 := Round(30 * f);
  v32 := Round(32 * f);
  v40 := Round(40 * f);
  v50 := Round(50 * f);
  v60 := Round(60 * f);
  v80 := Round(80 * f);
  v85 := Round(85 * f);
  v100 := Round(100 * f);
  v105 := Round(105 * f);

  SetViewPortOrgEx(g.Handle,
    Round(Width * MainVar.Scale/2),
    Round(Height * MainVar.Scale/2), nil);

  { Mastfuß angedeutet mit Mastquerschnitt }
  Lage := quer;
  OffsetX := 0;
  OffsetY := vProfilePosMastfuss; { OffsetY entspricht OffsetX, da gedreht }
  g.Pen.Color := clSilver;
  ControllerZoom3 := f;
  InternalDrawProfile3(g);

  { Deck }
  g.Pen.Color := clDeck;
  g.Brush.Color := clDeck;
  g.RoundRect( -v100, -v80,  v85, -v32, v20, v20);
  g.RoundRect( -v100,  v80,  v85,  v32, v20, v20);

  { Deck vorn }
  KlotzX1 := vPositionEdge - v10;
  KlotzX2 := v105 + 100; // or bigger
  g.Pen.Color := clDeck;
  g.Brush.Color := clDeck;
  g.Rectangle( KlotzX1, -v80, KlotzX2, v80);

  { rechter Klotz, um die Rundung im Deckausschnitt zeichnen }
  KlotzX2 := vPositionEdge;
  KlotzX1 := KlotzX2 - v20;
  g.Pen.Color := BackgroundColor;
  g.Brush.Color := BackgroundColor;
  g.RoundRect( KlotzX1, -v32, KlotzX2,  v32, v10, v10);

  { when active }
  if ControllerTyp <> ctOhne then
  begin
    { linker Klotz }
    KlotzX1 := vPositionXE;
    KlotzX2 := KlotzX1 + v15;
    g.Pen.Color := clBlack;
    g.Brush.Color := clAqua;
    g.Rectangle( KlotzX1, -v40, KlotzX2, v40);

    { Maßband Hintergrund }
    PosXE0 := vPositionXE0;
    g.Pen.Color := clRed;
    g.Brush.Color := clMassband;
    g.Rectangle( KlotzX1, -v7, PosXE0 + v100, v7);

    { Maßband Beschriftung }
    g.Font.Height := v6;
    g.Pen.Color := clWhite;
    g.Font.Color := clWhite;
    StrichX := KlotzX1;
    for i := 1 to 20 do
    begin
      StrichX := StrichX + v10;
      g.MoveTo(StrichX, -v5);
      g.LineTo(StrichX, v5);
      s := IntToStr(i);
      txtHeight := g.Font.Height;
      if i < 10 then
        u := StrichX - v1
      else
        u := StrichX - v3;
      v := txtHeight div 2;
      g.TextOut(u, -v, s)
    end;

    { Ablesemarke an Stelle EO }
    g.Pen.Color := clMarke;
    g.Brush.Style := bsClear;
    g.Rectangle( PosXE0 - v3, -v10, PosXE0 + v3, v10);
    g.Font.Color := clMarke;
    g.Font.Height := Round(20 * MainVar.Scale);
    g.TextOut(PosXE0 - v10, v15, 'E0');
    g.TextOut(-v60, v40, 'Ablesemarke an Position E0 + Offset');
  end;

  g.Brush.Style := bsClear;
  g.Font.Color := clMarke;
  g.Font.Height := Round(16 * MainVar.Scale);
  g.Font.Color := clAqua;
  g.TextOut(v30, -v50, Format('ControllerPos = %d', [ControllerPos]));
//  g.TextOut(0, -v50, Format('Beta = %6.2f', [Main.RggMain.Rigg.Beta]));
//  g.TextOut(0, -v40, Format('alpha1 = %6.2f', [Main.RggMain.Rigg.alpha1]));
//  g.TextOut(0, -v50, Format('lc = %6.2f', [Main.RggMain.Rigg.lc]));
  { drift because of how le is determined, see RggUnit2.FanIn() }
  g.TextOut(v30, -v40, Format('le = %6.2f', [Main.Rigg.MastLE]));
  g.TextOut(v30, -v30, Format('BiegungE = %6.2f', [Main.Rigg.DurchBiegungHE]));
  g.TextOut(v30, -v20, Format('ParamXE = %6.2f', [ParamXE]));

//  g.TextOut(0, -v50, Format('ControllerPos = %d', [ControllerPos]));
//  g.Font.Color := TRggColors.Yellow;
//  g.TextOut(0, -v40, Format('ProfilPosXE = %6.2f', [ProfilePosXE]));
//  g.Font.Color := TRggColors.Orange;
//  g.TextOut(0, -v30, Format('ParamXE = %6.2f', [ParamXE]));

  { Mastquerschnitt in Höhe E }
  g.Pen.Color := TRggColors.Cornflowerblue;
  g.Pen.Width := 1;
  ControllerZoom3 := f;
  OffsetY := Round(vProfilePosXE);
  InternalDrawProfile3(g);

  SetViewPortOrgEx(g.Handle, 0, 0, nil);
end;

destructor TSalingGraph.Destroy;
begin
  FBitmap.Free;
  inherited;
end;

procedure TSalingGraph.Draw(df: TFigure);
begin
  if Image = nil then
    Exit;

  PaintBackground(FBitmap.Canvas);

  case df of
    dtTest: ;
    dtSalingAll: ;
    dtSalingDetail: DrawSaling(FBitmap.Canvas);
    dtController: DrawController(FBitmap.Canvas);
    dtProfileDrawHoch: ;
    dtProfileDrawQuer: ;
    dtProfileOuter: ;
    dtProfileInner: ;
    dtProfileLeft: ;
    dtProfileRight: ;
  end;

  Image.Canvas.CopyMode := cmSrcCopy;
  Image.Canvas.Draw(0, 0, FBitmap);
end;

procedure TSalingGraph.PaintBackground(g: TCanvas);
var
  R: TRect;
begin
  R := Rect(0, 0, Image.Width, Image.Height);
  g.Brush.Color := BackgroundColor;
  g.FillRect(R);
end;

end.

