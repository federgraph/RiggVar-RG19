unit FrmIndicator;

interface

uses
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  Vcl.Menus,
  ThreeD,
  ThreeDSolid,
  RggMatrix,
  RggPolarKar;

type
  TIndicatorForm = class(TForm)
    Panel: TPanel;
    PaintBox: TPaintBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PaintBoxPaint(Sender: TObject);
    procedure PaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    PlotWidth, PlotHeight: Integer;
    PlotExtX, PlotExtY: Integer;
    PlotOrgX, PlotOrgY: Integer;

    { Deklarationen für das Drehen mit der Maus }
    MousDown: Boolean;
    MouseButton: TMouseButton;
    Painted: Boolean;
    prevx, prevy: Integer;

    ThreeD: TThreeDSolid;
    FWireFrame: Boolean;
    FOnChanged: TNotifyEvent;
    amat, bmat, tempmat, tmat: TMatrix4x4;
    function OpenModel: Integer;
    procedure SetWireFrame(Value: Boolean);
  public
    Filename: string;
    GlobalRot: Boolean;
    Rotator: TPolarKar;
    procedure RotateFromPoint;
    procedure Draw;
    procedure Update; override;
    procedure UpdateIndicator;
    property WireFrame: Boolean read FWireFrame write SetWireFrame;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  end;


implementation

uses
  Vector3D,
  RggPBox;

{$r *.DFM}

procedure PaintBackGround(Image: TBitmap);
var
  R: TRect;
begin
  R := Rect(0, 0, Image.Width, Image.Height);
  with Image.Canvas do
  begin
    Brush.Color := clBtnFace;
    FillRect(R);
  end;
end;

procedure TIndicatorForm.FormCreate(Sender: TObject);
var
  NewPaintBox: TPaintBox;
begin
  { PaintBox austauschen }
  NewPaintBox := TRggPaintBox.Create(PaintBox.Owner);
  try
    with NewPaintBox do
    begin
      Parent := PaintBox.Parent;
      Align := PaintBox.Align;
      OnMouseDown := PaintBox.OnMouseDown;
      OnMouseMove := PaintBox.OnMouseMove;
      OnMouseUp := PaintBox.OnMouseUp;
      OnPaint := PaintBox.OnPaint;
    end;
    PaintBox.Free;
    PaintBox := NewPaintBox;
  except
    NewPaintBox.Free;
  end;

  PlotExtX := VRR - VLL;
  PlotExtY := VBB - VTT;

  { Initialisierungen für das Drehen mit der Maus }
  GlobalRot := True;
  Painted := True;
  amat := TMatrix4x4.Create;
  bmat := TMatrix4x4.Create;
  tempmat := TMatrix4x4.Create;
  tmat := TMatrix4x4.Create;
  amat.YRot(0);
  amat.XRot(0);

  ThreeD := TThreeDSolid.Create;
  ThreeD.WireFrame := False;
  FWireFrame := False;
  FormatSettings.DecimalSeparator := '.';
  Filename := '';
  OpenModel;
end;

procedure TIndicatorForm.FormDestroy(Sender: TObject);
begin
  ThreeD.Free;
  amat.Free;
  bmat.Free;
  tempmat.Free;
  tmat.Free;
end;

function TIndicatorForm.OpenModel: Integer;
begin
  Result := ThreeD.Read3DModel;
  if Result = 1 then
  begin
    ThreeD.SetAt;
    ThreeD.SetFrom;
    RotateFromPoint;
    Draw;
  end
  else
    ShowMessage(Format('Read3DObject returned %d', [Result]));
end;

procedure TIndicatorForm.PaintBoxMouseDown
  (Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  MousDown := True;
  MouseButton := Button;
  prevx := X;
  prevy := Y;
end;

{ Drehen hier immer durch Bewegen der Kamera. }
procedure TIndicatorForm.PaintBoxMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  xtheta, ytheta: single;
begin
  if not MousDown then
    Exit;
  if not Assigned(amat) then
    Exit;
  tmat.identity;
  xtheta := (X - prevx) * 360 / PaintBox.Width;
  ytheta := (Y - prevy) * 360 / PaintBox.Height;
  if GlobalRot then
  begin
    { Drehung um globales Achsensystem }
    if MouseButton = mbLeft then
      tmat.ZRot(-xtheta);
    if MouseButton = mbLeft then
      tmat.XRot(ytheta);
    if MouseButton = mbRight then
      tmat.YRot(-xtheta);
    tmat.PreMultiply(amat.Mat);
    amat.CopyFrom(tmat);
  end
  else
  begin
    { Drehung um lokales Achsensystem }
    if MouseButton = mbLeft then
      tmat.ZRot(-xtheta);
    if MouseButton = mbLeft then
      tmat.YRot(ytheta);
    if MouseButton = mbRight then
      tmat.XRot(-xtheta);
    amat.PreMultiply(tmat.Mat);
  end;

  { beim Indikator bleibt das Modell fest und die Blickrichtung ändert sich
    während in Rigg sich das Modell dreht und die Blickrichtung konstant bleibt }
  tempmat.CopyFrom(amat);
  tempmat.Transpose; // die inverse von amat!
  bmat.identity;
  bmat.PreMultiply(tempmat.Mat);

  if Painted then
  begin
    Painted := False;
    RotateFromPoint;
  end;
  Draw;
  prevx := X;
  prevy := Y;
end;

procedure TIndicatorForm.PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  MousDown := False;
  Draw;
  Rotator.Matrix := bmat.Mat;
  if Assigned(OnChanged) then
    OnChanged(Self);
end;

procedure TIndicatorForm.RotateFromPoint;
var
  tempDist: double;
  vert1, vert2: vec3;
begin
  if not Assigned(amat) then
    Exit;
  with ThreeD do
  begin
    { Kamera und Lichtquelle um den At-Point drehen, dadurch entsteht der
      Eindruck, daß das Model um die lokalen Achsen gedreht wird. }
    tempDist := Mag(Subtract(From, At));
    { From - At }
    vert1.X := 0;
    vert1.Y := -tempDist;
    vert1.z := 0;
    vert2.X := 0;
    vert2.Y := 0;
    vert2.z := 1;
    amat.TransformPoint(vert1);
    amat.TransformPoint(vert2);
    From.X := vert1.X + At.X;
    From.Y := vert1.Y + At.Y;
    From.z := vert1.z + At.z;
    Up.X := vert2.X;
    Up.Y := vert2.Y;
    Up.z := vert2.z;
  end;
end;

procedure TIndicatorForm.PaintBoxPaint(Sender: TObject);
begin
  Draw;
end;

procedure TIndicatorForm.Draw;
var
  Bitmap: TBitmap;
  Rect: TRect;
begin
  Rect := PaintBox.ClientRect;
  if (Rect.right <= Rect.bottom) then
  begin
    PlotWidth := Rect.right;
    PlotHeight := Rect.right;
    PlotOrgX := 0;
    PlotOrgY := (Rect.bottom - Rect.right) div 2;
  end
  else
  begin
    PlotWidth := Rect.bottom;
    PlotHeight := Rect.bottom;
    PlotOrgX := (Rect.right - Rect.bottom) div 2;
    PlotOrgY := 0;
  end;

  Bitmap := TBitmap.Create;
  try
    with Bitmap do
    begin
      Bitmap.Width := PlotWidth;
      Bitmap.Height := PlotHeight;
      PaintBackGround(Bitmap);

      SetMapMode(Canvas.Handle, MM_ISOTROPIC);
      SetWindowExtEx(Canvas.Handle, PlotExtX, PlotExtY, nil);
      SetWindowOrgEx(Canvas.Handle, 0, 0, nil);
      SetViewportExtEx(Canvas.Handle, PlotWidth, PlotHeight, nil);
      SetViewportOrgEx(Canvas.Handle, 0, 0, nil);

      ThreeD.Display(Canvas);

      SetMapMode(Canvas.Handle, MM_TEXT);
      SetWindowOrgEx(Canvas.Handle, 0, 0, nil);
      SetViewportOrgEx(Canvas.Handle, 0, 0, nil);
    end;

    Painted := False;
    with PaintBox.Canvas do
    begin
      CopyMode := cmSrcCopy;
      Draw(PlotOrgX, PlotOrgY, Bitmap);
    end;
    Painted := True;

  finally
    Bitmap.Free;
  end;
end;

procedure TIndicatorForm.SetWireFrame(Value: Boolean);
begin
  if FWireFrame <> Value then
  begin
    WireFrame := Value;
    ThreeD.WireFrame := Value;
    Draw;
  end;
end;

{ der Austausch der Daten erfolgt über Rotator, nicht über amat oder bmat }
procedure TIndicatorForm.UpdateIndicator;
begin
  if not Visible then
    Exit;
  if not Assigned(Rotator) then
    Exit;
  bmat.Mat := Rotator.Matrix;
  tempmat.CopyFrom(bmat);
  tempmat.Transpose;
  amat.identity;
  amat.PreMultiply(tempmat.Mat);
  RotateFromPoint;
  Draw;
end;

procedure TIndicatorForm.Update;
begin
  inherited;
  UpdateIndicator;
end;

end.
