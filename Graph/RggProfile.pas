unit RggProfile;

interface

uses
  UITypes,
  Graphics,
  RggTypes;

type
  TLage = (Hoch, Quer);

  TMastProfile = class
  protected
    Lage: TLage;
    OffsetX: Integer;
    OffsetY: Integer;
    SalingZoom: Integer;
    ControllerZoom: Integer;
    procedure DrawProfileOld(g: TCanvas; Zoom: single);
  protected
    SalingZoom1: single;
    SalingZoom2: single;
    SalingZoom3: single;
  protected
    ControllerZoom1: single;
    ControllerZoom2: single;
    ControllerZoom3: single;
  protected
    ProfileOpacity: single;
    ProfileZoom: single;
    WantInner: Boolean;
    WantOuter: Boolean;
    WantLeft: Boolean;
    WantRight: Boolean;
    WantSegmentColor: Boolean;
    procedure InternalDrawProfile3(g: TCanvas);
    procedure InternalDrawProfile6(g: TCanvas);
  public
    constructor Create;
    procedure DrawProfile(g: TCanvas; Zoom: single);
    procedure LineTest(g: TCanvas);
    procedure ProfileDrawTest(g: TCanvas; lg: TLage);
    procedure ProfileDraw(g: TCanvas; lg: TLage; Opacity: single);
  end;

implementation

uses
  RiggVar.FB.Color;

constructor TMastProfile.Create;
begin
  ControllerZoom1 := 1.0;
  ControllerZoom2 := 0.1;
  ControllerZoom3 := 1.0;

  SalingZoom1 := 5.0;
  SalingZoom2 := 10.0;
  SalingZoom3 := 1.0;

  ProfileZoom := 1.0;
  ProfileOpacity := 1.0;
end;

procedure TMastProfile.ProfileDrawTest(g: TCanvas; lg: TLage);
var
  ox: single;
  oy: single;
  zf: single;

  procedure MetaLINE(x1, y1, x2, y2: single; c: TColor);
  begin
    g.Pen.Color := c;
    x1 := ox + x1 * zf;
    y1 := oy + y1 * zf;
    x2 := ox + x2 * zf;
    y2 := oy + y2 * zf;
    case lg of
      Quer:
      begin
        g.MoveTo(Round(y1), Round(x1));
        g.LineTo(Round(y2), Round(x2));
      end;
      Hoch:
      begin
        g.MoveTo(Round(x1), Round(y1));
        g.LineTo(Round(x2), Round(y2));
      end;
    end;
  end;

  procedure MetaARC(xm, ym, Radius: single; phi1, phi2: single; c: TColor);
  var
    a1, a2: single;
  begin
    g.Pen.Color := c;
    if lg = Quer then
    begin
      xm := ox + xm * zf;
      ym := oy + ym * Zf;
      Radius := Radius * zf;
      a1 := (90-phi1) * pi/180;
      a2 := (90-phi2) * pi/180;
      g.Arc(
        Round(ym - Radius),
        Round(xm - Radius),
        Round(ym + Radius),
        Round(xm + Radius),
        Round(ym + cos(a1) * Radius),
        Round(xm + sin(a1) * Radius),
        Round(ym + cos(a2) * Radius),
        Round(xm + sin(a2) * Radius));
    end
    else if lg = Hoch then
    begin
      xm := ox + xm * zf;
      ym := oy + ym * Zf;
      Radius := Radius * zf;
      a1 := phi1 * pi/180;
      a2 := phi2 * pi/180;
      g.Arc(
        Round(xm - Radius),
        Round(ym - Radius),
        Round(xm + Radius),
        Round(ym + Radius),
        Round(xm + cos(a1) * Radius),
        Round(ym + sin(a1) * Radius),
        Round(xm + cos(a2) * Radius),
        Round(ym + sin(a2) * Radius));
    end;
  end;

begin
  g.Pen.Width := 1;

  ox := 200;
  oy := 10;
  zf := 5.0;

  { outer path clockwise }
  MetaLINE(   5.00,  0.00,  2.00,  0.00, clRed);
  MetaARC(  -18.50, 40.70,  47.00,  -60.00, 0.00, clBlue);
  MetaLINE(  28.50, 40.70, 28.50, 43.50, clRed);
  MetaARC(    0.00, 43.50,  28.50,    0.00,   90.00, clBlue);

  MetaARC(    0.00, 43.50, 28.50,   90.00,  180.00, clLime);
  MetaLINE( -28.50, 43.50, -28.50, 40.70, clLime);
  MetaARC(   18.50, 40.70, 47.00, 180.00, 240.00, clAqua);
  MetaLINE(  -5.00,  0.00,  -2.00,  0.00, clBlue);

  MetaARC(   -3.50,  0.00,  1.50,    0.00,  132.53, clLime);
  MetaARC(   18.50, 40.70, 45.80, -129.17, -120.16, clLime);
  MetaLINE(  -4.85, 10.02, -10.44,  5.20, clLime);
  MetaARC(    0.00,  4.30,  7.50,   90.00,  130.85, clLime);

  MetaARC(    0.00,  4.30,   7.50,   49.15,   90.00, clAqua);
  MetaLINE(   4.90,  9.97, 10.44,  5.20, clAqua);
  MetaARC(  -18.50, 40.70,  45.80,  -59.83,  -50.83, clAqua);
  MetaARC(    3.50,  0.00,   1.50,   47.46,  180.00, clAqua);

  { inner path right }
  MetaLINE(    0.0, 13.00,  3.50, 13.00, clBlue);
  MetaLINE(   3.50, 13.00, 11.48,  6.09, clBlue);
  MetaARC(  -18.50, 40.70,  45.80,  -49.09,  -16.70, clBlue);
  MetaARC(   11.00, 31.85,  15.00,  -16.71,    0.00, clBlue);
  MetaLINE(  26.00, 31.85, 26.00, 49.00, clBlue);
  MetaARC(   11.00, 49.00,  15.00,    0.00,   26.58, clBlue);
  MetaARC(    0.00, 43.50,  27.30,   26.58,   90.00, clBlue);

  { inner path left }
  MetaARC(    0.00, 43.50, 27.30,   90.00,  153.42, clRed);
  MetaARC(  -11.00, 49.00, 15.00,  153.42,  180.00, clRed);
  MetaLINE( -26.00, 49.00, -26.00, 31.85, clRed);
  MetaARC(  -11.00, 31.85, 15.00, -180.00, -163.29, clRed);
  MetaARC(   18.50, 40.70, 45.80, -163.29, -130.91, clRed);
  MetaLINE( -11.55,  6.04, -3.50, 13.00, clYellow);
  MetaLINE(  -3.50, 13.00,  0.00, 13.00, clYellow);

  { symmetry line }
//  MetaLINE(   0.00, 72.00,   0.00,  0.00, clYellow);

{ Legend: }
{ MetaLINE(    x1,   y1,    x2,     y2); }
{ MetaARC(     xm,   ym,     r,    phi1,    phi2); }
end;

procedure TMastProfile.InternalDrawProfile3(g: TCanvas);
  procedure MetaLINE(x1, y1, x2, y2: single);
  begin
    if Lage = Quer then
    begin
      x1 := x1 * ControllerZoom3 + OffsetX;
      y1 := y1 * ControllerZoom3 + OffsetY;
      x2 := x2 * ControllerZoom3 + OffsetX;
      y2 := y2 * ControllerZoom3 + OffsetY;
      g.MoveTo(Round(y1), Round(x1));
      g.LineTo(Round(y2), Round(x2));
    end
    else if Lage = Hoch then
    begin
      x1 := x1 * SalingZoom3 + OffsetX;
      y1 := y1 * SalingZoom3 + OffsetY;
      x2 := x2 * SalingZoom3 + OffsetX;
      y2 := y2 * SalingZoom3 + OffsetY;
      g.MoveTo(Round(x1), Round(y1));
      g.LineTo(Round(x2), Round(y2));
    end;
  end;

  procedure MetaARC(xm, ym, Radius: single; phi1, phi2: single);
  var
    a1, a2: single;
  begin
    if Lage = Quer then
    begin
      xm := xm * ControllerZoom3 + OffsetX;
      ym := ym * ControllerZoom3 + OffsetY;
      Radius := Radius * ControllerZoom3;
      a1 := (90-phi1) * pi/180;
      a2 := (90-phi2) * pi/180;
      g.Arc(
        Round(ym - Radius),
        Round(xm - Radius),
        Round(ym + Radius),
        Round(xm + Radius),
        Round(ym + cos(a1)*Radius),
        Round(xm + sin(a1)*Radius),
        Round(ym + cos(a2)*Radius),
        Round(xm + sin(a2)*Radius));
    end
    else if Lage = Hoch then
    begin
      xm := xm * SalingZoom3 + OffsetX;
      ym := ym * SalingZoom3 + OffsetY;
      Radius := Radius * SalingZoom3;
      a1 := phi1 * pi/180;
      a2 := phi2 * pi/180;
      g.Arc(
        Round(xm - Radius),
        Round(ym - Radius),
        Round(xm + Radius),
        Round(ym + Radius),
        Round(xm + cos(a1) * Radius),
        Round(ym + sin(a1) * Radius),
        Round(xm + cos(a2) * Radius),
        Round(ym + sin(a2) * Radius));
    end;
  end;

begin
{ MetaLINE(    x1,   y1,    x2,     y2); }
  MetaLINE(   4.90,  9.97,  10.44,    5.20);
  MetaLINE(  28.50, 40.70,  28.50,   43.50);
  MetaLINE(   3.50, 13.00,   0.00,   13.00);
  MetaLINE(  11.48,  6.09,   3.50,   13.00);
  MetaLINE(   2.00,  0.00,   5.00,    0.00);
  MetaLINE(  26.00, 31.85,  26.00,   49.00);

  MetaLINE(  -4.85, 10.02, -10.44,    5.20);
  MetaLINE(   0.00, 13.00,  -3.50,   13.00);
  MetaLINE(  -3.50, 13.00, -11.55,    6.04);
  MetaLINE( -28.50, 43.50, -28.50,   40.70);
  MetaLINE(  -5.00,  0.00,  -2.00,    0.00);
  MetaLINE( -26.00, 49.00, -26.00,   31.85);
{ MetaLINE(   0.00, 72.00,   0.00,    0.00); }

{ MetaARC(     xm,   ym,     r,    phi1,    phi2); }
  MetaARC(  -18.50, 40.70,  45.80,  -59.83,  -50.83);
  MetaARC(  -18.50, 40.70,  45.80,  -49.09,  -16.70);
  MetaARC(  -18.50, 40.70,  47.00,  -60.00,    0.00);
  MetaARC(    0.00,  4.30,   7.50,   49.15,   90.00);
  MetaARC(    0.00, 43.50,  28.50,    0.00,   90.00);
  MetaARC(    0.00, 43.50,  27.30,   26.58,   90.00);
  MetaARC(    3.50,  0.00,   1.50,   47.46,  180.00);
  MetaARC(   11.00, 31.85,  15.00,  -16.71,    0.00);
  MetaARC(   11.00, 49.00,  15.00,    0.00,   26.58);

{ MetaARC(     xm,   ym,     r,    phi1,    phi2); }
  MetaARC(  -11.00, 49.00,  15.00,  153.42, 180.00);
  MetaARC(  -11.00, 31.85,  15.00, -180.00, -163.29);
  MetaARC(   -3.50,  0.00,   1.50,    0.00,  132.53);
  MetaARC(    0.00,  4.30,   7.50,   90.00,  130.85);
  MetaARC(    0.00, 43.50,  28.50,   90.00, 180.00);
  MetaARC(    0.00, 43.50,  27.30,   90.00,  153.42);
  MetaARC(   18.50, 40.70,  45.80, -129.17, -120.16);
  MetaARC(   18.50, 40.70,  45.80, -163.29, -130.91);
  MetaARC(   18.50, 40.70,  47.00, -180.00, -120.00);
end;

procedure TMastProfile.InternalDrawProfile6(g: TCanvas);
  procedure MetaLINE(x1, y1, x2, y2: single; cla: TColor);
  begin
    if WantSegmentColor then
    begin
      g.Pen.Color := cla;
    end;
    x1 := x1 * ProfileZoom + OffsetX;
    y1 := -y1 * ProfileZoom + OffsetY;
    x2 := x2 * ProfileZoom + OffsetX;
    y2 := -y2 * ProfileZoom + OffsetY;
    g.MoveTo(Round(x1), Round(y1));
    g.LineTo(Round(x2), Round(y2));
  end;

  procedure MetaARC(xm, ym, Radius: single; phi1, phi2: single; cla: TColor);
  var
    a1, a2: single;
  begin
    if WantSegmentColor then
    begin
      g.Pen.Color := cla;
    end;
    xm := xm * ProfileZoom + OffsetX;
    ym := -ym * ProfileZoom + OffsetY;
    Radius := Radius * ProfileZoom;
    a1 := -phi1 * pi/180;
    a2 := -phi2 * pi/180;
    g.Arc(
      Round(xm - Radius),
      Round(ym - Radius),
      Round(xm + Radius),
      Round(ym + Radius),
      Round(xm + cos(a1) * Radius),
      Round(ym + sin(a1) * Radius),
      Round(xm + cos(a2) * Radius),
      Round(ym + sin(a2) * Radius));
  end;

begin
//  g.Pen.Color := clRed;
//  g.Pen.Width := 2;

  if WantOuter then
  begin
  MetaLINE(   5.00,  0.00, 2.00, 0.00, clFuchsia);
  MetaARC(  -18.50, 40.70, 47.00, -60.00, 0.00, clFuchsia);
  MetaLINE(  28.50, 40.70, 28.50, 43.50, clFuchsia);
  MetaARC(    0.00, 43.50, 28.50, 0.00, 90.00, clFuchsia);

  MetaARC(    0.00, 43.50, 28.50, 90.00, 180.00, clAqua);
  MetaLINE( -28.50, 43.50, -28.50, 40.70, clAqua);
  MetaARC(   18.50, 40.70, 47.00, -180.00, -120.00, clAqua);
  MetaLINE(  -5.00,  0.00, -2.00, 0.00, clAqua);

  MetaARC(   -3.50,  0.00, 1.50, 0.00, 132.53, clLime);
  MetaARC(   18.50, 40.70, 45.80, -129.17, -120.16, clLime);
  MetaLINE(  -4.85, 10.02, -10.44, 5.20, clLime);
  MetaARC(    0.00,  4.30, 7.50, 90.00, 130.85, clLime);

  MetaARC(    0.00,  4.30, 7.50, 49.15, 90.00, clAqua);
  MetaLINE(   4.90,  9.97, 10.44, 5.20, clAqua);
  MetaARC(  -18.50, 40.70, 45.80, -59.83, -50.83, clAqua);
  MetaARC(    3.50,  0.00, 1.50, 47.46, 180.00, clAqua);
  end;

  if WantInner then
  begin
  { right }
  MetaLINE(    0.0, 13.00, 3.50, 13.00, clBlue);
  MetaLINE(   3.50, 13.00, 11.48, 6.09, clBlue);
  MetaARC(  -18.50, 40.70, 45.80, -49.09,  -16.70, clBlue);
  MetaARC(   11.00, 31.85, 15.00, -16.71,    0.00, clBlue);
  MetaLINE(  26.00, 31.85, 26.00, 49.00, clBlue);
  MetaARC(   11.00, 49.00, 15.00, 0.00,   26.58, clBlue);
  MetaARC(    0.00, 43.50, 27.30, 26.58,   90.00, clBlue);

  { left }
  MetaARC(    0.00, 43.50, 27.30, 90.00,  153.42, clRed);
  MetaARC(  -11.00, 49.00, 15.00, 153.42,  180.00, clRed);
  MetaLINE( -26.00, 49.00, -26.00, 31.85, clRed);
  MetaARC(  -11.00, 31.85, 15.00, -180.00, -163.29, clRed);
  MetaARC(   18.50, 40.70, 45.80, -163.29, -130.91, clRed);
  MetaLINE( -11.55,  6.04, -3.50, 13.00, clYellow);
  MetaLINE(  -3.50, 13.00, 0.00, 13.00, clYellow);
  end;

  if WantRight then
  begin
{ MetaLINE(     x1,    y1,     x2,      y2, cla); }
  MetaLINE(   4.90,  9.97,  10.44,    5.20, clRed);
  MetaLINE(  28.50, 40.70,  28.50,   43.50, clGreen);
  MetaLINE(   3.50, 13.00,   0.00,   13.00, clGreen);
  MetaLINE(  11.48,  6.09,   3.50,   13.00, clYellow);
  MetaLINE(   2.00,  0.00,   5.00,    0.00, clFuchsia);
  MetaLINE(  26.00, 31.85,  26.00,   49.00, clAqua);
  end;

  if WantLeft then
  begin
  MetaLINE(  -4.85, 10.02, -10.44,    5.20, clRed);
  MetaLINE(   0.00, 13.00,  -3.50,   13.00, clGreen);
  MetaLINE(  -3.50, 13.00, -11.55,    6.04, clBlue);
  MetaLINE( -28.50, 43.50, -28.50,   40.70, clYellow);
  MetaLINE(  -5.00,  0.00,  -2.00,    0.00, clFuchsia);
  MetaLINE( -26.00, 49.00, -26.00,   31.85, clAqua);
{ MetaLINE(     0,  72.00,   0.00,    0.00, cla); }
  end;

  if WantRight then
  begin
{ MetaARC(      xm,    ym,      r,    phi1,    phi2); }
  MetaARC(  -18.50, 40.70,  45.80,  -59.83,  -50.83, clRed);
  MetaARC(  -18.50, 40.70,  45.80,  -49.09,  -16.70, clGreen);
  MetaARC(  -18.50, 40.70,  47.00,  -60.00,    0.00, clBlue);
  MetaARC(    0.00,  4.30,   7.50,   49.15,   90.00, clYellow);
  MetaARC(    0.00, 43.50,  28.50,    0.00,   90.00, clFuchsia);
  MetaARC(    0.00, 43.50,  27.30,   26.58,   90.00, clAqua);
  MetaARC(    3.50,  0.00,   1.50,   47.46,  180.00, clLime);
  MetaARC(   11.00, 31.85,  15.00,  -16.71,    0.00, TRggColors.Orange);
  MetaARC(   11.00, 49.00,  15.00,    0.00,   26.58, clTeal);
  end;

  if WantLeft then
  begin
{ MetaARC(     xm,   ym,     r,    phi1,    phi2); }
  MetaARC(  -11.00, 49.00,  15.00,  153.42, 180.00, clRed);
  MetaARC(  -11.00, 31.85,  15.00, -180.00, -163.29, clGreen);
  MetaARC(   -3.50,  0.00,   1.50,    0.00,  132.53, clBlue);
  MetaARC(    0.00,  4.30,   7.50,   90.00,  130.85, clYellow);
  MetaARC(    0.00, 43.50,  28.50,   90.00, 180.00, clFuchsia);
  MetaARC(    0.00, 43.50,  27.30,   90.00,  153.42, clAqua);
  MetaARC(   18.50, 40.70,  45.80, -129.17, -120.16, clLime);
  MetaARC(   18.50, 40.70,  45.80, -163.29, -130.91, TRggColors.Orange);
  MetaARC(   18.50, 40.70,  47.00, -180.00, -120.00, clTeal);
  end;
end;

procedure TMastProfile.ProfileDraw(g: TCanvas; lg: TLage; Opacity: single);
var
  ox: single;
  oy: single;
  zf: single;

  procedure MetaLINE(x1, y1, x2, y2: single; c: TColor);
  begin
    x1 := ox + x1 * zf;
    y1 := oy + y1 * zf;
    x2 := ox + x2 * zf;
    y2 := oy + y2 * zf;
    case lg of
      Quer:
      begin
//        PolylineFS(g.Bitmap, Line(y1, x1, y2, x2),
//        c, False, StrokeThickness, jsRound, esRound, 4, g.Transformation);
        g.MoveTo(Round(y1), Round(x1));
        g.LineTo(Round(y2), Round(x2));
      end;
      Hoch:
      begin
//        PolylineFS(g.Bitmap, Line(x1, y1, x2, y2),
//        c, False, StrokeThickness, jsRound, esRound, 4, g.Transformation);
        g.MoveTo(Round(x1), Round(y1));
        g.LineTo(Round(x2), Round(y2));
      end;
    end;
  end;

  procedure MetaARC(xm, ym, Radius: single; phi1, phi2: single; c: TColor);
  var
    a1, a2: single;
  begin
    if lg = Quer then
    begin
      xm := ox + xm * zf;
      ym := oy + ym * Zf;
      Radius := Radius * zf;
      a1 := (90-phi1) * pi/180;
      a2 := (90-phi2) * pi/180;
      g.Arc(
        Round(ym - Radius),
        Round(xm - Radius),
        Round(ym + Radius),
        Round(xm + Radius),
        Round(xm + cos(a1) * Radius),
        Round(ym + sin(a1) * Radius),
        Round(xm + cos(a2) * Radius),
        Round(ym + sin(a2) * Radius));
    end
    else if lg = Hoch then
    begin
      xm := ox + xm * zf;
      ym := oy + ym * Zf;
      Radius := Radius * zf;
      a1 := phi1 * pi/180;
      a2 := phi2 * pi/180;
      g.Arc(
        Round(xm - Radius),
        Round(ym - Radius),
        Round(xm + Radius),
        Round(ym + Radius),
        Round(xm + cos(a1) * Radius),
        Round(ym + sin(a1) * Radius),
        Round(xm + cos(a2) * Radius),
        Round(ym + sin(a2) * Radius));
    end;
  end;

begin
  g.Pen.Width := 1;
  ox := 0;
  oy := 0;
  zf := 1.0;

  { outer path clockwise }
  MetaLINE(   5.00,  0.00,  2.00,  0.00, clFuchsia);
  MetaARC(  -18.50, 40.70,  47.00,  -60.00,    0.00, clFuchsia);
  MetaLINE(  28.50, 40.70, 28.50, 43.50, clFuchsia);
  MetaARC(    0.00, 43.50,  28.50,    0.00,   90.00, clFuchsia);

  MetaARC(    0.00, 43.50, 28.50,   90.00,  180.00, clAqua);
  MetaLINE( -28.50, 43.50, -28.50, 40.70, clAqua);
  MetaARC(   18.50, 40.70, 47.00, -180.00, -120.00, clAqua);
  MetaLINE(  -5.00,  0.00,  -2.00,  0.00, clAqua);

  MetaARC(   -3.50,  0.00,  1.50,    0.00,  132.53, clLime);
  MetaARC(   18.50, 40.70, 45.80, -129.17, -120.16, clLime);
  MetaLINE(  -4.85, 10.02, -10.44,  5.20, clLime);
  MetaARC(    0.00,  4.30,  7.50,   90.00,  130.85, clLime);

  MetaARC(    0.00,  4.30,   7.50,   49.15,   90.00, clAqua);
  MetaLINE(   4.90,  9.97, 10.44,  5.20, clAqua);
  MetaARC(  -18.50, 40.70,  45.80,  -59.83,  -50.83, clAqua);
  MetaARC(    3.50,  0.00,   1.50,   47.46,  180.00, clAqua);

  { inner path right }
  MetaLINE(    0.0, 13.00,  3.50, 13.00, clBlue);
  MetaLINE(   3.50, 13.00, 11.48,  6.09, clBlue);
  MetaARC(  -18.50, 40.70,  45.80,  -49.09,  -16.70, clBlue);
  MetaARC(   11.00, 31.85,  15.00,  -16.71,    0.00, clBlue);
  MetaLINE(  26.00, 31.85, 26.00, 49.00, clBlue);
  MetaARC(   11.00, 49.00,  15.00,    0.00,   26.58, clBlue);
  MetaARC(    0.00, 43.50,  27.30,   26.58,   90.00, clBlue);

  { inner path left }
  MetaARC(    0.00, 43.50, 27.30,   90.00,  153.42, clRed);
  MetaARC(  -11.00, 49.00, 15.00,  153.42,  180.00, clRed);
  MetaLINE( -26.00, 49.00, -26.00, 31.85, clRed);
  MetaARC(  -11.00, 31.85, 15.00, -180.00, -163.29, clRed);
  MetaARC(   18.50, 40.70, 45.80, -163.29, -130.91, clRed);
  MetaLINE( -11.55,  6.04, -3.50, 13.00, clYellow);
  MetaLINE(  -3.50, 13.00,  0.00, 13.00, clYellow);

  { symmetry line }
//  MetaLINE(   0.00, 72.00,   0.00,  0.00, claRed);

{ MetaLINE(    x1,   y1,    x2,     y2          ); }
{ MetaARC(     xm,   ym,     r,    phi1,    phi2); }
end;

procedure TMastProfile.DrawProfile(g: TCanvas; Zoom: single);

  procedure MetaLINE(x1, y1, x2, y2: Integer);
  begin
    if Lage = quer then
    begin
      x1 := x1 + OffsetX;
      y1 := y1 + OffsetY;
      x2 := x2 + OffsetX;
      y2 := y2 + OffsetY;
      x1 := Round(x1 / Zoom);
      y1 := Round(y1 / Zoom);
      x2 := Round(x2 / Zoom);
      y2 := Round(y2 / Zoom);
      g.MoveTo(y1, x1);
      g.LineTo(y2, x2);
    end
    else if Lage = hoch then
    begin
      x1 := x1 + OffsetX;
      y1 := y1 + OffsetY;
      x2 := x2 + OffsetX;
      y2 := y2 + OffsetY;
      x1 := Round(x1 / Zoom);
      y1 := Round(y1 / Zoom);
      x2 := Round(x2 / Zoom);
      y2 := Round(y2 / Zoom);
      g.MoveTo(x1, y1);
      g.LineTo(x2, y2);
    end;
  end;

  procedure MetaARC(xm, ym, Radius: single; phi1, phi2: double);
  var
    temp: single;
  begin
    if Lage = quer then
    begin
      xm := xm + OffsetX;
      ym := ym + OffsetY;
      xm := xm / Zoom;
      ym := ym / Zoom;
      Radius := Radius / Zoom;
      temp := xm;
      xm := ym;
      ym := temp;
      g.Arc(
        Round(xm - Radius),
        Round(ym - Radius),
        Round(xm + Radius),
        Round(ym + Radius),
        Round(xm + cos(phi1 * pi/180) * Radius),
        Round(ym + sin(phi1 * pi/180) * Radius),
        Round(xm + cos(phi2 * pi/180) * Radius),
        Round(ym + sin(phi2 * pi/180) * Radius));
    end
    else if Lage = hoch then
    begin
      xm := xm + OffsetX;
      ym := ym + OffsetY;
      xm := xm / Zoom;
      ym := ym / Zoom;
      Radius := Radius / Zoom;
      g.Arc(
        Round(xm - Radius),
        Round(ym - Radius),
        Round(xm + Radius),
        Round(ym + Radius),
        Round(xm + cos(phi1 * pi/180) * Radius),
        Round(ym + sin(phi1 * pi/180) * Radius),
        Round(xm + cos(phi2 * pi/180) * Radius),
        Round(ym + sin(phi2 * pi/180) * Radius));
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

procedure TMastProfile.LineTest(g: TCanvas);
var
  xm, ym: Integer;
  Radius: Integer;
  a1, a2: single;
begin
    g.Pen.Color := clRed;
    g.Pen.Width := 4;
    g.MoveTo(200, 100);
    g.LineTo(100, 200);

    g.Pen.Color := clBlue;
    g.Pen.Width := 4;
    xm := 150;
    ym := 150;
    a1 := 0;
    a2 := 120 * pi / 180;
    Radius := 120;
    g.Arc(
      xm - Radius,
      ym - Radius,
      xm + Radius,
      ym + Radius,
      xm + Round(cos(a1) * Radius),
      ym + Round(sin(a1) * Radius),
      xm + Round(cos(a2) * Radius),
      ym + Round(sin(a2) * Radius));
end;

procedure TMastProfile.DrawProfileOld(g: TCanvas; Zoom: single);

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
      g.MoveTo(y1, x1);
      g.LineTo(y2, x2);
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
      g.MoveTo(x1, y1);
      g.LineTo(x2, y2);
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
      temp := xm;
      xm := ym;
      ym := temp;
      g.Arc(
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
      g.Arc(
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

end.
