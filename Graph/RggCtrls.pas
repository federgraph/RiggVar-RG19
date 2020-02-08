unit RggCtrls;

interface

uses
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
//  System.Types,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.ExtCtrls,
  RggTypes;

type
  TLage = (hoch, quer);

  TSalingCtrl = class
  private
    OffsetX: Integer;
    OffsetY: Integer;
    Lage: TLage;
    SalingZoom: Integer;
    ControllerZoom: Integer;
    procedure DrawProfile(Canvas: TCanvas);

  public
    BackgroundColor: TColor;
    ControllerTyp: TControllerTyp;
    PBSize: TPoint; { PaintBox-Size }

    EdgePos: Integer; { Abstand von E0 zur Anschlagkante Deck + Klotzdicke }
    ControllerPos: Integer; { Abstand(iP[ooE0,x], iP[ooE ,x]) in mm }
    ParamXE: Integer; { Abstand(iP[ooD0,x], iP[ooE,x]) in mm }
    ParamXE0: Integer; { Abstand(iP[ooD0,x], iP[ooE0,x]) in mm }

    SalingA: Integer; { Abstand(iP[ooA,x], iP[ooB,x]) in mm }
    SalingH: Integer; { Abstand Verbindungslinie Salinge zu Hinterkante Mast in mm }
    SalingL: Integer; { Salinglänge in mm - außerhalb berechnen }
    SalingHOffset: Integer; { Abstand Hinterkante Mast zur neutrale Faser in mm }
    SalingDetail: Boolean; { Umschalten zwischen den beiden SalingViews }

    constructor Create;

    procedure DrawSalingAll(Canvas: TCanvas);
    procedure DrawSalingDetail(Canvas: TCanvas);
    procedure DrawController(Canvas: TCanvas);
  end;

implementation

constructor TSalingCtrl.Create;
begin
  BackgroundColor := clBtnFace;
  PBSize.x := 453;
  PBSize.y := 220;
  ControllerZoom := 1;
  SalingZoom := 5;
  ControllerTyp := ctDruck;

  { Properties für ControllerGrafik in mm }
  EdgePos := 25;
  ControllerPos := 80;
  ParamXE := -20;
  ParamXE0 := 110;

  { Properties für SalingGrafik in mm }
  SalingHOffset := 37;
  SalingH := 80;
  SalingA := 800;
  SalingL := 1000;
end;

procedure TSalingCtrl.DrawProfile(Canvas: TCanvas);

  procedure MetaLINE(x1, y1, x2, y2: Integer);
  begin
    if Lage = quer then
    begin
      x1 := x1 + OffsetX;
      y1 := y1 + OffsetY;
      x2 := x2 + OffsetX;
      y2 := y2 + OffsetY;
      x1 := x1 div ControllerZoom;
      y1 := y1 div ControllerZoom;
      x2 := x2 div ControllerZoom;
      y2 := y2 div ControllerZoom;
      Canvas.MoveTo(y1, x1);
      Canvas.LineTo(y2, x2);
    end
    else if Lage = hoch then
    begin
      x1 := x1 + OffsetX;
      y1 := y1 + OffsetY;
      x2 := x2 + OffsetX;
      y2 := y2 + OffsetY;
      x1 := x1 div SalingZoom;
      y1 := y1 div SalingZoom;
      x2 := x2 div SalingZoom;
      y2 := y2 div SalingZoom;
      Canvas.MoveTo(x1, y1);
      Canvas.LineTo(x2, y2);
    end;
  end;

  procedure MetaARC(xm, ym, Radius: Integer; phi1, phi2: double);
  var
    temp: Integer;
  begin
    if Lage = quer then
    begin
      xm := xm + OffsetX;
      ym := ym + OffsetY;
      xm := xm div ControllerZoom;
      ym := ym div ControllerZoom;
      Radius := Radius div ControllerZoom;
      temp := xm; xm := ym; ym := temp;
      Canvas.Arc(
        xm - Radius,
        ym - Radius,
        xm + Radius,
        ym + Radius,
        xm + Round(sin(phi2*pi/180)*Radius),
        ym + Round(cos(phi2*pi/180)*Radius),
        xm + Round(sin(phi1*pi/180)*Radius),
        ym + Round(cos(phi1*pi/180)*Radius)
      );
    end
    else if Lage = hoch then
    begin
      xm := xm + OffsetX;
      ym := ym + OffsetY;
      xm := xm div SalingZoom;
      ym := ym div SalingZoom;
      Radius := Radius div SalingZoom;
      Canvas.Arc(
        xm - Radius,
        ym - Radius,
        xm + Radius,
        ym + Radius,
        xm + Round(cos(phi1*pi/180)*Radius),
        ym + Round(sin(phi1*pi/180)*Radius),
        xm + Round(cos(phi2*pi/180)*Radius),
        ym + Round(sin(phi2*pi/180)*Radius)
      );
    end;
  end;

begin
{ MetaLINE(    x1,   y1,    x2,     y2); }
  MetaLINE(   490,  997,  1044,    520);
  MetaLINE(  2850, 4070,  2850,   4350);
  MetaLINE(   350, 1300,     0,   1300);
  MetaLINE(  1148,  609,   350,   1300);
  MetaLINE(   500,    0,   200,      0);
  MetaLINE(  2600, 3185,  2600,   4900);

  MetaLINE(  -485, 1002, -1044,    520);
  MetaLINE(     0, 1300,  -350,   1300);
  MetaLINE(  -350, 1300, -1155,    604);
  MetaLINE( -2850, 4350, -2850,   4070);
  MetaLINE(  -500,    0,  -200,      0);
  MetaLINE( -2600, 4900, -2600,   3185);
{ MetaLINE(     0, 7200,     0,      0); }

{ MetaARC(     xm,   ym,     r,    phi1,    phi2); }
  MetaARC(  -1850, 4070,  4580,  -59.83,  -50.83);
  MetaARC(  -1850, 4070,  4580,  -49.09,  -16.70);
  MetaARC(  -1850, 4070,  4700,  -60.00,    0.00);
  MetaARC(      0,  430,   750,   49.15,   90.00);
  MetaARC(      0, 4350,  2850,    0.00,   90.00);
  MetaARC(      0, 4350,  2730,   26.58,   90.00);
  MetaARC(    350,  000,   150,   47.46, -180.00);
  MetaARC(   1100, 3185,  1500,  -16.71,    0.00);
  MetaARC(   1100, 4900,  1500,    0.00,   26.58);

{ MetaARC(     xm,   ym,     r,    phi1,    phi2); }
  MetaARC(  -1100, 4900,  1500,  153.42, -180.00);
  MetaARC(  -1100, 3185,  1500, -180.00, -163.29);
  MetaARC(   -350,    0,   150,    0.00,  132.53);
  MetaARC(      0,  430,   750,   90.00,  130.85);
  MetaARC(      0, 4350,  2850,   90.00, -180.00);
  MetaARC(      0, 4350,  2730,   90.00,  153.42);
  MetaARC(   1850, 4070,  4580, -129.17, -120.16);
  MetaARC(   1850, 4070,  4580, -163.29, -130.91);
  MetaARC(   1850, 4070,  4700, -180.00, -120.00);
end;

procedure TSalingCtrl.DrawSalingAll(Canvas: TCanvas);
var
  SalingX: Integer;
  SalingY: Integer;
begin
  Lage := hoch;
  OffsetX := 0; { 0 * 100; }
  OffsetY := (SalingH-SalingHOffset) * 100;

  SetMapMode(Canvas.Handle, MM_ISOTROPIC);
  SetWindowExtEx(Canvas.Handle, 9000, 9000, nil);
  SetWindowOrgEx(Canvas.Handle, 0, -2000, nil);
  SetViewPortExtEx(Canvas.Handle, PBSize.x, -PBSize.y, nil);
  SetViewPortOrgEx(Canvas.Handle, PBSize.x div 2, PBSize.y, nil);

  { SalingH }
  SalingY := SalingH * 100 div SalingZoom;
  Canvas.Pen.Width := 2*100 div SalingZoom;
  Canvas.Pen.Color := clBlack;
  Canvas.MoveTo( 0, SalingY);
  Canvas.LineTo( 0, 0);
  { SalingA }
  SalingX := (SalingA *  50) div SalingZoom; { SalingA/2 gezoomt }
  SalingY := (SalingH*100) div SalingZoom;
  Canvas.Pen.Width := 2*100 div SalingZoom;
  Canvas.Pen.Color := clBlue;
  Canvas.MoveTo(-SalingX, 0);
  Canvas.LineTo( SalingX, 0);
  { SalingL }
  Canvas.Pen.Width := 20*100 div SalingZoom;
  Canvas.Pen.Color := $00C0DCC0; { hellgrün }
  Canvas.LineTo( 0, SalingY);
  Canvas.LineTo(-SalingX, 0);
  { Wanten als Kreise }
  Canvas.Pen.Width := 1;
  Canvas.Pen.Color := clBlack;
  Canvas.Brush.Color := clRed;
  Canvas.Ellipse(-SalingX - 100, 100, -SalingX + 100, -100);
  Canvas.Ellipse( SalingX - 100, 100,  SalingX + 100, -100);
  Canvas.Pen.Color := clBlack;
  Canvas.Brush.Color := clRed;
  Canvas.Ellipse( -100, SalingY + 100,  100, SalingY - 100);

  { Profilschnitt zeichnen }
  Canvas.Pen.Width := 2*100 div SalingZoom;
  Canvas.Pen.Color := clBlue;
  DrawProfile(Canvas);

  SetWindowOrgEx(Canvas.Handle, 0, 0, nil);
  SetViewPortOrgEx(Canvas.Handle, 0, 0, nil);
  SetMapMode(Canvas.Handle, MM_TEXT);
end;

procedure TSalingCtrl.DrawSalingDetail(Canvas: TCanvas);
var
  SalingX, SalingY: Integer;
  PosX, PosY: Integer;
  s: string;
//  t: Integer;
begin
//  t := Canvas.Font.Size; // = 8 in debugger

  Lage := hoch;
  OffsetX := 0; { 0 * 100; }
  OffsetY := (SalingH-SalingHOffset) * 100;

  SetMapMode(Canvas.Handle, MM_ISOTROPIC);
  SetWindowExtEx(Canvas.Handle, 5000, 5000, nil);
  SetWindowOrgEx(Canvas.Handle, 0, -800, nil);
  SetViewPortExtEx(Canvas.Handle, PBSize.x, -PBSize.y, nil);
  SetViewPortOrgEx(Canvas.Handle, PBSize.x div 2, PBSize.y, nil);

  { SalingH }
  SalingY := ((SalingH)*100) div SalingZoom;
  Canvas.Pen.Width := 2*100 div SalingZoom;
  Canvas.Pen.Color := clBlack;
  Canvas.MoveTo( 0, SalingY);
  Canvas.LineTo( 0, 0);
  { SalingH - SalingHOffset }
  SalingY := ((SalingH-SalingHOffset)*100) div SalingZoom;
  Canvas.Pen.Width := 2*100 div SalingZoom;
  Canvas.Pen.Color := clFuchsia;
  Canvas.MoveTo( -100, SalingY);
  Canvas.LineTo( -100, 0);
  { SalingA }
  SalingX := (SalingA * 50) div SalingZoom; { SalingA/2 gezoomt }
  SalingY := ((SalingH)*100) div SalingZoom;
  Canvas.Pen.Width := 2*100 div SalingZoom;
  Canvas.Pen.Color := clBlue;
  Canvas.MoveTo(-SalingX, 0);
  Canvas.LineTo( SalingX, 0);
  { SalingL }
  Canvas.Pen.Width := 15*100 div SalingZoom;
  Canvas.Pen.Color := $00C0DCC0; { hellgrün }
  Canvas.LineTo( 0, SalingY);
  Canvas.LineTo(-SalingX, 0);
  { Wanten als Kreise }
  Canvas.Pen.Width := 1;
  Canvas.Pen.Color := clBlack;
  Canvas.Brush.Color := clRed;
  Canvas.Ellipse(-SalingX - 100, 100, -SalingX + 100, -100);
  Canvas.Ellipse( SalingX - 100, 100,  SalingX + 100, -100);
  Canvas.Pen.Color := clBlack;
  Canvas.Brush.Color := clRed;
  Canvas.Ellipse( -70, SalingY + 70,  70, SalingY - 70);
  { Profilschnitt zeichnen }
  Canvas.Pen.Width := 2*100 div SalingZoom;
  Canvas.Pen.Color := clBlue;
  DrawProfile(Canvas);

  { Texte }
  Canvas.Font.Height := 25 * 100 div SalingZoom;

  Canvas.Font.Color := clNavy;
  { Canvas.Brush.Color := clSilver; }
  Canvas.Brush.Style := bsClear;
  SetTextAlign(Canvas.Handle, TA_CENTER or TA_TOP);
  PosX := 0;
  PosY := -700 div SalingZoom;
  s := Format('Salingabstand = %d mm',[SalingA]);
  Canvas.TextOut(PosX, PosY, s);

  Canvas.Font.Color := clGreen;
  // Canvas.Brush.Color := clWhite;
  // Canvas.Brush.Style := bsClear;
  SetTextAlign(Canvas.Handle, TA_LEFT or TA_BOTTOM);
  PosX := -220*100 div SalingZoom;
  PosY :=   70*100 div SalingZoom;
  s := Format('Salinglänge = %d mm',[SalingL]);
  Canvas.TextOut(PosX, PosY, s);

  Canvas.Font.Color := clBlack;
  // Canvas.Brush.Color := clSilver;
  // Canvas.Brush.Style := bsClear;
  // SetTextAlign(Canvas.Handle, TA_LEFT or TA_BOTTOM);
  PosX := 1000 div SalingZoom;
  PosY := 3000 div SalingZoom;
  s := Format('Salinghöhe = %d mm',[SalingH]);
  Canvas.TextOut(PosX, PosY, s);

  Canvas.Font.Color := clFuchsia;
  // Canvas.Brush.Color := clWhite;
  // Canvas.Brush.Style := bsClear;
  SetTextAlign(Canvas.Handle, TA_RIGHT or TA_BOTTOM);
  PosX := -1500 div SalingZoom;
  PosY := 1000 div SalingZoom;
  s := Format('Salinghöhe - Offset = %d mm',[SalingH - SalingHOffset]);
  Canvas.TextOut(PosX, PosY, s);

  Canvas.Font.Color := clBlack;
  // Canvas.Brush.Color := clWhite;
  // Canvas.Brush.Style := bsClear;
  SetTextAlign(Canvas.Handle, TA_LEFT or TA_BOTTOM);
  PosX := 35*100 div SalingZoom;
  PosY := 180*100 div SalingZoom;
  s := Format('SalingHOffset = %d mm',[SalingHOffset]);
  Canvas.TextOut(PosX, PosY, s);

  SetWindowOrgEx(Canvas.Handle, 0, 0, nil);
  SetViewPortOrgEx(Canvas.Handle, 0, 0, nil);
  SetMapMode(Canvas.Handle, MM_TEXT);
end;

procedure TSalingCtrl.DrawController(Canvas: TCanvas);
var
  i: Integer;
  KlotzX1: Integer;
  KlotzX2: Integer;
  PosXE0: Integer;
  StrichX: Integer;
  PositionXE0: Integer;
  PositionXE: Integer;
  ProfilPosMastfuss: Integer;
  ProfilPosXE: Integer;
  EdgePosition: Integer;
  s: string;
  clDeck: TColor;
  clMarke: TColor;
  clMassband: TColor;
  tmpFontSize: Integer;
  txtHeight: Integer;
  t: Integer;
begin
  tmpFontSize := Canvas.Font.Size;
  Canvas.Font.Size := tmpFontSize * 50;

  clDeck := clTeal;
  clMarke := clYellow;
  clMassband := clGray;

  PositionXE0 := 95; { Position der Ablesemarke, Konstante in der Grafik }
  PositionXE := PositionXE0-ControllerPos; { Position linke Kante Mastklotz }
  ProfilPosMastfuss := PositionXE0-ParamXE0-72; { Position Hinterkante Mastfuss }
  ProfilPosXE := ProfilPosMastfuss+ParamXE; { Position Hinterkante Mast in Höhe Punkt E }
  EdgePosition := PositionXE0-EdgePos + 15; { Abstand Deckanschlag - E0 }

  OffsetX := 0;
  Lage := quer;

  SetMapMode(Canvas.Handle, MM_ISOTROPIC);
  SetWindowExtEx(Canvas.Handle, 10000, 10000, nil);
  SetWindowOrgEx(Canvas.Handle, 0, 0, nil);
  SetViewPortExtEx(Canvas.Handle, PBSize.x, -PBSize.y, nil);
  SetViewPortOrgEx(Canvas.Handle, PBSize.x div 2, PBSize.y div 2, nil);

  { Mastfuß angedeutet mit Mastquerschnitt }
  OffsetY := ProfilPosMastfuss * 100; { OffsetY entspricht OffsetX, da gedreht }
  Canvas.Pen.Color := clBlack;
  DrawProfile(Canvas);

  { Deck }
  Canvas.Pen.Color := clDeck;
  Canvas.Brush.Color := clDeck;
  Canvas.RoundRect( -10000, -8000,  8500, -3200, 2000, 2000);
  Canvas.RoundRect( -10000,  8000,  8500,  3200, 2000, 2000);
  Canvas.Rectangle(   8000, -8000, 10500,  8000);

  { rechter Klotz, um die Rundung im Deckausschnitt zeichnen }
  KlotzX1 := EdgePosition * 100 - 1000;
  KlotzX2 := 8000;
  Canvas.Pen.Color := clDeck;
  Canvas.Brush.Color := clDeck;
  Canvas.Rectangle( KlotzX1, -8000, KlotzX2,  8000);

  KlotzX2 := EdgePosition * 100;
  KlotzX1 := KlotzX2 - 2000;
  Canvas.Pen.Color := BackgroundColor;
  Canvas.Brush.Color := BackgroundColor;
  Canvas.RoundRect( KlotzX1, -3200, KlotzX2,  3200, 1000, 1000);

  { Controller ausblenden, wenn OhneSaling/Mast starr }
  if ControllerTyp <> ctOhne then
  begin
    { linker Klotz }
    KlotzX1 := PositionXE * 100;
    KlotzX2 := KlotzX1 + 1500;
    Canvas.Pen.Color := clBlack;
    Canvas.Brush.Color := clAqua;
    Canvas.Rectangle( KlotzX1, -4000, KlotzX2,  4000);

    { Maßband Hintergrund }
    PosXE0 := PositionXE0 * 100;
    Canvas.Pen.Color := clRed;
    Canvas.Brush.Color := clMassband;
    Canvas.Rectangle( KlotzX1, -700, PosXE0 + 1000, 700);
    { Maßband Beschriftung }
    Canvas.Pen.Color := clWhite;
    Canvas.Font.Color := clWhite;
    SetTextAlign(Canvas.Handle, TA_CENTER or TA_TOP);
    StrichX := KlotzX1;
    for i := 1 to 20 do
    begin
      StrichX := StrichX + 1000;
      Canvas.MoveTo(StrichX, -500);
      Canvas.LineTo(StrichX, 500);
      s := IntToStr(i);
      txtHeight := Canvas.Font.Height;
      t := -500 + (1000 - txtHeight) div 2;
      Canvas.TextOut(StrichX, t, S);
    end;

    { Ablesemarke an Stelle EO }
    Canvas.Pen.Color := clMarke;
    Canvas.Brush.Style := bsClear;
    Canvas.Rectangle( PosXE0-250, -1000, PosXE0+250, 1000);
    Canvas.Font.Color := clMarke;
    Canvas.TextOut(PosXE0, 2000, 'E0');
    Canvas.TextOut(5000, -4200, 'Ablesemarke an Position E0 + Offset');

    { ButtonRechteck }
    { wirkt in Verbindung mit Shape oder Region }
    (*
    Canvas.Pen.Color := clMarke;
    Canvas.Brush.Style := bsClear;
    Canvas.RoundRect(PosXE0-2300, 4600, PosXE0+250, 3400, 250, 250);
    Canvas.Font.Color := clMarke;
    SetTextAlign(Canvas.Handle, TA_LEFT or TA_TOP);
    Canvas.TextOut(PosXE0-2200, 4400, 'Zustellen');
    *)
  end;

  { Mastquerschnitt in Höhe E }
  if ProfilPosXE > 250 then
    ProfilPosXE := 250; { Integerüberlauf vermeiden! }
  if ProfilPosXE < -250 then
    ProfilPosXE := -250; { Integerüberlauf vermeiden! }
  OffsetY := ProfilPosXE * 100;
  Canvas.Pen.Color := clBlue;
  Canvas.Pen.Width := 90;
  DrawProfile(Canvas);
  Canvas.Pen.Width := 1;

  // SetWindowOrgEx(Canvas.Handle, 0, 0, nil); { war schon auf 0,0 }
  SetViewPortOrgEx(Canvas.Handle, 0, 0, nil);
  SetMapMode(Canvas.Handle, MM_TEXT);
  { Es Die origins werden von SetMapMode MM_TEXT nicht automatisch zurückgesetzt!
   Alternative: }
  (*
  SetWindowExtEx(Canvas.Handle, 1, 1, nil);
  SetWindowOrgEx(Canvas.Handle, 0, 0, nil);
  SetViewPortExtEx(Canvas.Handle, 1, 1, nil);
  SetViewPortOrgEx(Canvas.Handle, 0, 0, nil);
  *)
  Canvas.Font.Size := tmpFontSize;
end;

end.

