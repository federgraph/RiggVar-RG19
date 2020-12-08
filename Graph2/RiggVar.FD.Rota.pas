unit RiggVar.FD.Rota;

(*
-
-     F
-    * * *
-   *   *   G
-  *     * *   *
- E - - - H - - - I
-  *     * *         *
-   *   *   *           *
-    * *     *             *
-     D-------A---------------B
-              *
-              (C) federgraph.de
-
*)

interface

{$ifdef fpc}
{$mode delphi}
{$endif}

{.$define FMX}
{$define VCL}
{.$define LCL}

uses
  Windows,
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.UIConsts,
  System.Math,
  RiggVar.RG.Graph,
  RggTypes,
  RiggVar.FB.ActionConst,
  RiggVar.FB.Color,
  RiggVar.FD.Drawing00,
  RiggVar.FD.Drawings,
  RiggVar.FD.Elements,
  RiggVar.FD.Point,
  RiggVar.FD.TransformHelper,
  Graphics,
  Controls,
  ExtCtrls;

type
  TRotaForm2 = class(TInterfacedObject, IStrokeRigg)
  private
    FBogen: Boolean;
    FKoppel: Boolean;
    FViewpoint: TViewpoint;
    FFixpoint: TRiggPoint;
    FDarkMode: Boolean;
    FBackgroundColor: TColor;

    FKoordinaten: TRiggPoints;

    FKoppelKurve: TKoordLine;

    FMastKurve: TMastKurve;

    FMastLinie: TLineDataR100;
    FMastLinieL: single;
    FMastLinieB: single;

    UseMastKurve: Boolean;

    FBitmapWidth: Integer;
    FBitmapHeight: Integer;

    procedure UpdateRiggKoords;
    procedure UpdateKoppelKurve;
    procedure UpdateMastKurve;
    procedure UpdateMastLinie;
    procedure MoveToFX;
    procedure Rota3D;
    procedure RotaSeite;
    procedure RotaAchtern;
    procedure RotaTop;
    procedure RotaHelper(aRotX, aRotY, aRotZ, aRelativeZoom: single);

    procedure SetFixpoint(const Value: TRiggPoint);
    procedure SetViewpoint(const Value: TViewpoint);
    procedure SetBogen(const Value: Boolean);
    procedure SetBtnBlauDown(const Value: Boolean);
    procedure SetBtnGrauDown(const Value: Boolean);
    procedure SetControllerTyp(const Value: TControllerTyp);
    procedure SetGrauZeichnen(const Value: Boolean);
    procedure SetHullVisible(const Value: Boolean);
    procedure SetKoordinaten(const Value: TRiggPoints);
    procedure SetKoordinatenE(const Value: TRiggPoints);
    procedure SetKoordinatenR(const Value: TRiggPoints);
    procedure SetKoppel(const Value: Boolean);
    procedure SetKoppelKurve(const Value: TKoordLine);
    procedure SetMastKurve(const Value: TMastKurve);
    procedure SetRiggLED(const Value: Boolean);
    procedure SetSalingTyp(const Value: TSalingTyp);
    procedure SetSofortBerechnen(const Value: Boolean);
    procedure SetWanteGestrichelt(const Value: Boolean);
    procedure SetDarkMode(const Value: Boolean);
    procedure SetBackgroundColor(const Value: TColor);
  protected
    MouseDown: Boolean;
    MousePos: TPoint;
    TH: TTransformHelper;
    DL: TRggDrawings;
    RD: TRggDrawingD00;
    CurrentElement: TRggElement;

    ImageMidPoint: TPoint3D;

    procedure ClearImage;
    procedure DrawToCanvas(g: TCanvas);
    procedure DoDrawToCanvas(Sender: TObject);

    procedure DoReset;
    procedure ResetBtnClick(Sender: TObject);

    procedure ImageMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ImageMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure ImageMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  public
    Bitmap: TBitmap;
    Image: TImage;
    IsUp: Boolean;
    DrawCounter: Integer;
    UseRotaCenterFullScreen: Boolean;

    constructor Create;
    destructor Destroy; override;

    procedure HandleAction(fa: Integer);

    procedure InitPosition(w, h, x, y: single);
    procedure Init;
    procedure Swap;
    procedure RotateZ(delta: single);
    procedure Zoom(delta: single);
    procedure ZoomInBtnClick(Sender: TObject);
    procedure ZoomOutBtnClick(Sender: TObject);

    function GetChecked(fa: Integer): Boolean;
    procedure SetChecked(fa: Integer; Value: Boolean);

    procedure Draw;
    procedure ImageScreenScaleChanged(Sender: TObject);

    procedure SetMastLineData(const Value: TLineDataR100; L: single; Beta: single);
    function GetMastKurvePoint(const Index: Integer): TPoint3D;

    procedure ToggleRenderOption(const fa: Integer);
    function QueryRenderOption(const fa: Integer): Boolean;
    procedure UpdateHullTexture;
    procedure UpdateCameraX(Delta: single);
    procedure UpdateCameraY(Delta: single);
    procedure DoOnUpdateStrokeRigg;

    property BackgroundColor: TColor read FBackgroundColor write SetBackgroundColor;
    property DarkMode: Boolean read FDarkMode write SetDarkMode;

    property Koordinaten: TRiggPoints write SetKoordinaten;
    property KoordinatenE: TRiggPoints write SetKoordinatenE;
    property KoordinatenR: TRiggPoints write SetKoordinatenR;
    property KoppelKurve: TKoordLine write SetKoppelKurve;
    property MastKurve: TMastKurve write SetMastKurve;
    property WanteGestrichelt: Boolean write SetWanteGestrichelt;
    property Bogen: Boolean write SetBogen;
    property Koppel: Boolean write SetKoppel;
    property HullVisible: Boolean write SetHullVisible;
    property FixPoint: TRiggPoint write SetFixPoint;
    property ViewPoint: TViewPoint write SetViewPoint;
    property SalingTyp: TSalingTyp write SetSalingTyp;
    property ControllerTyp: TControllerTyp write SetControllerTyp;
    property RiggLED: Boolean write SetRiggLED;
    property SofortBerechnen: Boolean write SetSofortBerechnen;
    property GrauZeichnen: Boolean write SetGrauZeichnen;
    property BtnGrauDown: Boolean write SetBtnGrauDown;
    property BtnBlauDown: Boolean write SetBtnBlauDown;
  end;

implementation

{ TRotaForm2 }

procedure TRotaForm2.Draw;
begin
  if (Image = nil) or (Bitmap = nil) then
    Exit;
  TH.Draw;
end;

function TRotaForm2.GetMastKurvePoint(const Index: Integer): TPoint3D;
begin
  result := TPoint3D.Zero;
end;

procedure TRotaForm2.Init;
begin
  Bitmap := TBitmap.Create;
  Bitmap.Width := Round(FBitmapWidth);
  Bitmap.Height := Round(FBitmapHeight);

  { injected Image component is probably shared with RotaForm1 }
  if Image.Picture.Graphic = nil then
    Image.Picture.Graphic := Bitmap;
end;

procedure TRotaForm2.InitPosition(w, h, x, y: single);
begin
  ImageMidPoint.X := w / 2;
  ImageMidPoint.Y := h / 2;
  TH.Offset.X := 0;
  TH.Offset.Y := 0;
end;

procedure TRotaForm2.Swap;
begin
  Image.OnMouseDown := ImageMouseDown;
  Image.OnMouseMove := ImageMouseMove;
  Image.OnMouseUp := ImageMouseUp;
//  Image.OnMouseWheel := ImageMouseWheel; { FormMain.FormMouseWheel used }
//  Image.OnScreenScaleChanged := ImageScreenScaleChanged;
end;

function TRotaForm2.QueryRenderOption(const fa: Integer): Boolean;
begin
  case fa of
    faRggBogen: result := FBogen;
    faRggKoppel: result := FKoppel;
    faToggleSortedRota: result := RD.WantSort;
    else
      result := False;
  end;
end;

procedure TRotaForm2.RotateZ(delta: single);
begin
  TH.IsRightMouseBtn := True;
  TH.DoOnMouse([], Round(delta), -Round(delta));
end;

procedure TRotaForm2.SetBackgroundColor(const Value: TColor);
begin
  FBackgroundColor := Value;
end;

procedure TRotaForm2.SetBogen(const Value: Boolean);
begin
  FBogen := Value;
  RD.MK.Visible := FBogen;
  RD.D0D.Visible := not FBogen;
  RD.DC.Visible := not FBogen;
end;

procedure TRotaForm2.SetBtnBlauDown(const Value: Boolean);
begin

end;

procedure TRotaForm2.SetBtnGrauDown(const Value: Boolean);
begin

end;

procedure TRotaForm2.SetControllerTyp(const Value: TControllerTyp);
begin

end;

procedure TRotaForm2.SetDarkMode(const Value: Boolean);
begin
  FDarkMode := Value;
  RD.IsDark := Value;
  RD.Colors.BackgroundColor := FBackgroundColor;
  Draw;
end;

procedure TRotaForm2.SetGrauZeichnen(const Value: Boolean);
begin

end;

procedure TRotaForm2.SetHullVisible(const Value: Boolean);
begin
end;

procedure TRotaForm2.SetKoordinaten(const Value: TRiggPoints);
begin
  FKoordinaten := Value;
end;

procedure TRotaForm2.SetKoordinatenE(const Value: TRiggPoints);
begin

end;

procedure TRotaForm2.SetKoordinatenR(const Value: TRiggPoints);
begin

end;

procedure TRotaForm2.SetKoppel(const Value: Boolean);
begin
  FKoppel := Value;
  RD.KK.Visible := Value;
end;

procedure TRotaForm2.SetKoppelKurve(const Value: TKoordLine);
begin
  FKoppelKurve := Value;
end;

procedure TRotaForm2.SetMastKurve(const Value: TMastKurve);
begin
  FMastKurve := Value;
end;

procedure TRotaForm2.SetMastLineData(const Value: TLineDataR100; L, Beta: single);
begin
  FMastLinie := Value;
  FMastLinieL := L;
  FMastLinieB := Beta;
end;

procedure TRotaForm2.SetRiggLED(const Value: Boolean);
begin

end;

procedure TRotaForm2.SetSalingTyp(const Value: TSalingTyp);
begin

end;

procedure TRotaForm2.SetSofortBerechnen(const Value: Boolean);
begin

end;

procedure TRotaForm2.SetViewpoint(const Value: TViewpoint);
begin
  FViewpoint := Value;
  case FViewpoint of
    vpSeite: RotaSeite;
    vpAchtern: RotaAchtern;
    vpTop: RotaTop;
    vp3D: Rota3D;
  end;
end;

procedure TRotaForm2.SetWanteGestrichelt(const Value: Boolean);
begin

end;

procedure TRotaForm2.ToggleRenderOption(const fa: Integer);
begin
  case fa of
    faToggleSortedRota: RD.WantSort := not RD.WantSort;
  end;
end;

procedure TRotaForm2.Zoom(delta: single);
begin
  { only the sign of param dy will be used }
  TH.DoOnMouse([ssShift], 0, -Round(delta));
end;

procedure TRotaForm2.ZoomInBtnClick(Sender: TObject);
begin
  { TH.DoOnMouse([ssShift], 0, -1); }
  TH.ZoomDelta := 1 + 0.1;
  TH.Draw;
  TH.ZoomDelta := 1;
end;

procedure TRotaForm2.ZoomOutBtnClick(Sender: TObject);
begin
  { TH.DoOnMouse([ssShift], 0, 1); }
  TH.ZoomDelta := 1 - 0.1;
  TH.Draw;
  TH.ZoomDelta := 1;
end;

constructor TRotaForm2.Create;
begin
  UseRotaCenterFullScreen := False;
  UseMastKurve := True;

  if UseRotaCenterFullScreen then
  begin
    FBitmapWidth := 1600;
    FBitmapHeight := 1200;
  end
  else
  begin
    { should be the same value as in RotaForm1 }
    FBitmapWidth := 1024;
    FBitmapHeight := 768;
  end;

  RD := TRggDrawingD00.Create;
  DL := TRggDrawings.Create;
  DL.UseDarkColorScheme := True;
  DL.Add(RD);

  TH := TTransformHelper.Create;
  TH.OnDrawToCanvas := DoDrawToCanvas;

  TH.CurrentDrawing := RD;
end;

procedure TRotaForm2.ImageMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  MouseDown := True;
  MousePos.X := X;
  MousePos.Y := Y;
  TH.IsRightMouseBtn := Button = TMouseButton.mbRight;
  TH.Rotation := TPoint3D.Zero;
end;

procedure TRotaForm2.ImageMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  dx, dy: Integer;
begin
  if not MouseDown then
    Exit;

  dx := X - MousePos.X;
  dy := Y - MousePos.Y;

  TH.DoOnMouse(Shift, dx, dy);

  MousePos.X := X;
  MousePos.Y := Y;
end;

procedure TRotaForm2.ImageMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  MouseDown := False;
end;

procedure TRotaForm2.UpdateCameraX(Delta: single);
begin
  TH.DoOnMouse([ssCtrl], Round(Delta), 0);
end;

procedure TRotaForm2.UpdateCameraY(Delta: single);
begin
  TH.DoOnMouse([ssCtrl], 0, -Round(Delta));
end;

procedure TRotaForm2.UpdateHullTexture;
begin

end;

procedure TRotaForm2.ResetBtnClick(Sender: TObject);
begin
  DoReset;
end;

destructor TRotaForm2.Destroy;
begin
  TH.Free;
  DL.Free;
  Bitmap.Free;
  inherited;
end;

procedure TRotaForm2.DoReset;
begin
  TH.Reset;
  FixPoint := FFixPoint;
end;

procedure TRotaForm2.ClearImage;
var
  g: TCanvas;
  R: TRect;
begin
  if (Image = nil) or (Bitmap = nil) then
    Exit;
  DrawCounter := 0;
  g := Bitmap.Canvas;

  R := Rect(0, 0, Image.Width, Image.Height);
  g.Brush.Color := TRggColors.AliceBlue;
  g.FillRect(R);

  Image.Repaint;
end;

procedure TRotaForm2.DoDrawToCanvas(Sender: TObject);
begin
  if Bitmap <> nil then
    DrawToCanvas(Bitmap.Canvas);
end;

procedure TRotaForm2.DrawToCanvas(g: TCanvas);
var
{$ifdef FMX}
  ss: single;
{$endif}
{$ifdef VCL}
  R: TRect;
{$endif}
begin
  Inc(DrawCounter);

{$ifdef FMX}

  g.Offset := TPointF.Zero;

  ss := Image.Scene.GetSceneScale;
  if g.BeginScene then
  try
    g.SetMatrix(TMatrix.CreateScaling(ss, ss));
    g.Clear(claNull);
    g.Fill.Color := claYellow;
    g.Stroke.Color := claAqua;
    g.Stroke.Thickness := 1.0;
    g.Font.Size := 16;
    g.Font.Family := 'Consolas';
    RD.FaxPoint3D.C := ImageMidPoint + TH.Offset;
    RD.Draw(g);
  finally
    g.EndScene;
  end;
  Image.Repaint;
{$endif}

{$ifdef VCL}
  R := Rect(0, 0, Image.Width, Image.Height);
  g.Brush.Color := FBackgroundColor;
  g.FillRect(R);

  g.Brush.Color := clYellow;
  g.Pen.Color := clAqua;
  g.Pen.Width := 1;
  g.Font.Size := 24;
  g.Font.Name := 'Consolas';

  RD.FaxPoint3D.C := ImageMidPoint + TH.Offset;
  RD.Draw(g);

  Image.Canvas.CopyMode := cmSrcCopy;
  Image.Canvas.Draw(0, 0, Bitmap);
{$endif}

{$ifdef LCL}
  g.FillRect(0, 0, g.Width, g.Height, FBackgroundColor, dmSet);

  g.LineCap := TPenEndCap.pecRound;
  g.JoinStyle := TPenJoinStyle.pjsRound;
  g.FontHeight:= 24;
  g.FontName := 'Consolas';

  RD.Draw(g);

  Image.Canvas.Clear;
  RD.FaxPoint3D.C := ImageMidPoint + TH.Offset;
  Bitmap.Draw(Image.Canvas, 0, 0, True);
  Image.Invalidate;
{$endif}

end;

procedure TRotaForm2.RotaSeite;
begin
  RotaHelper(0, 0, 0, 1.0);
end;

procedure TRotaForm2.RotaAchtern;
begin
  RotaHelper(0, 90, 0, 1.0)
end;

procedure TRotaForm2.RotaTop;
begin
  RotaHelper(-90, 0, 0, 2.2)
end;

procedure TRotaForm2.Rota3D;
begin
  RotaHelper(-80, 0, 0, 2.2)
end;

procedure TRotaForm2.RotaHelper(aRotX, aRotY, aRotZ, aRelativeZoom: single);
var
  x, y, z: single;
  mx, my, mz: TMatrix3D;
  mr: TMatrix3D;
begin
  x := DegToRad(aRotX);
  y := DegToRad(aRotY);
  z := DegToRad(aRotZ);

  mx := TMatrix3D.CreateRotationX(x);
  my := TMatrix3D.CreateRotationY(y);
  mz := TMatrix3D.CreateRotationZ(z);

  mr := mx * my * mz;

  TH.ResetTransform;

//  ImageMidPoint.X := Image.Width / 2;
//  ImageMidPoint.Y := Image.Height / 2;
  RD.InitialZoom := RD.InitialZoomDefault * aRelativeZoom;

  RD.ViewpointFlag := True;
  DoOnUpdateStrokeRigg;

  TH.InitTransform(mr);
end;

function TRotaForm2.GetChecked(fa: Integer): Boolean;
begin
  result := False;
  case fa of
    faToggleSortedRota: result := RD.WantSort;
  end;
end;

procedure TRotaForm2.SetChecked(fa: Integer; Value: Boolean);
begin

end;

procedure TRotaForm2.ImageScreenScaleChanged(Sender: TObject);
begin
  Draw;
end;

procedure TRotaForm2.HandleAction(fa: Integer);
var
  aRotX, aRotY, aRotZ: single;
  aRelativeZoom: single;
begin
  aRotX := 0;
  aRotY := 0;
  aRotZ := 0;
  aRelativeZoom := 1;

  case fa of
    faReset:
    begin
      case FViewpoint of
        vpAchtern: aRotY := 90;
        vpTop:
        begin
          aRotX := -90;
          aRelativeZoom := 3.0;
        end;
        vp3D:
        begin
          aRotX := -80;
          aRelativeZoom := 2.5;
        end;
      end;
      RotaHelper(aRotX, aRotY, aRotZ, aRelativeZoom);
    end;
    faResetPosition: ;
    faResetRotation: ;
    faResetZoom: ;

    faToggleSortedRota:
    begin
      RD.WantSort := not RD.WantSort;
      Draw;
    end;
  end;
end;

procedure TRotaForm2.UpdateKoppelKurve;
var
  i: Integer;
  p: TPoint3D;
begin
  p := RD.rP_FX;
  for i := 0 to 100 do
  begin
    RD.KK.RggPoly[i].X := (FKoppelKurve[i].X - p.X) * RD.InitialZoom;
    RD.KK.RggPoly[i].Y := -(FKoppelKurve[i].Z - p.Z) * RD.InitialZoom;
    RD.KK.RggPoly[i].Z := p.Y * RD.InitialZoom;
  end;
  if not RD.ViewpointFlag then
    RD.KK.Transform;
end;

procedure TRotaForm2.UpdateMastKurve;
var
  i: Integer;
  p: TPoint3D;
begin
  p := RD.rP_FX;
  for i := 0 to BogenMax do
  begin
    RD.MK.RggPoly[i].X := (FMastKurve[i].X - p.X) * RD.InitialZoom;
    RD.MK.RggPoly[i].Y := -(FMastKurve[i].Z - p.Z) * RD.InitialZoom;
    RD.MK.RggPoly[i].Z := p.Y * RD.InitialZoom;
  end;
  if not RD.ViewpointFlag then
    RD.MK.Transform;
end;

procedure TRotaForm2.UpdateMastLinie;
var
  temp1, temp2, temp3, temp4, tempL: single;
  j, k: Integer;
  p: TPoint3D;
begin
  p :=  RD.rP_FX - RD.rP_D0;
  temp1 := cos(pi / 2 + FMastLinieB);
  temp2 := cos(FMastLinieB);
  temp3 := sin(pi / 2 + FMastLinieB);
  temp4 := sin(FMastLinieB);
  for j := 0 to BogenMax do
  begin
    k := Round(100 / BogenMax * j);
    tempL := j * FMastLinieL / BogenMax;
    RD.MK.RggPoly[j].X := (tempL * temp1 + FMastLinie[k] * temp2 - p.X) * RD.InitialZoom;
    RD.MK.RggPoly[j].Y := -(tempL * temp3 + FMastLinie[k] * temp4 - p.Z) * RD.InitialZoom;
    RD.MK.RggPoly[j].Z := p.Y * RD.InitialZoom;
  end;
  if not RD.ViewpointFlag then
    RD.MK.Transform;
end;

procedure TRotaForm2.UpdateRiggKoords;
begin
  RD.UpdateFromRigg;
  if not RD.ViewpointFlag then
    RD.Transform(TH.AccuMatrix);
end;

procedure TRotaForm2.DoOnUpdateStrokeRigg;
begin
  RD.Koordinaten := FKoordinaten;
  RD.FixPoint := FFixPoint;

  UpdateRiggKoords;
  UpdateKoppelKurve;

  if UseMastKurve then
    UpdateMastKurve
  else
    UpdateMastLinie;

  RD.ViewpointFlag := False;
end;

procedure TRotaForm2.SetFixpoint(const Value: TRiggPoint);
begin
  FFixPoint := Value;
  RD.FixPoint := FFixPoint;
  RD.UpdateFX;
  TH.DrawToCanvas;
  MoveToFX;
end;

procedure TRotaForm2.MoveToFX;
var
  mr: TMatrix3D;
  ra: TPoint3D;
begin
  RD.ViewpointFlag := True;
  DoOnUpdateStrokeRigg;

  ra := TH.RotationHelper.EulerAnglesFromMatrix(TH.AccuMatrix);
  mr := TH.RotationHelper.EulerAnglesToMatrix(ra.X, ra.Y, ra.Z);
  TH.ResetTransform;
  TH.InitTransform(mr);
end;

end.
