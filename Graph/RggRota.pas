unit RggRota;

interface

{$define Rigg19}

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Classes,
  System.Types,
  System.UITypes,
  Vcl.Graphics,
  Vcl.Forms,
  Vcl.Controls,
  Vcl.Menus,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.ComCtrls,
  Vcl.ExtDlgs,
  Vector3D,
  RggTypes,
  RggGBox,
  RggMatrix,
  RggUnit4,
  RggGraph,
  RggRaumGraph,
  RggHull,
  RggPolarKar;

type
  TRotaForm = class
    procedure PaintBox3DPaint(Sender: TObject);
    procedure ZoomInBtnClick(Sender: TObject);
    procedure ZoomOutBtnClick(Sender: TObject);
    procedure RumpfBtnClick(Sender: TObject);
    procedure PaintBtnClick(Sender: TObject);
    procedure KeepInsideItemClick(Sender: TObject);
    procedure PaintBox3DMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure PaintBox3DMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure PaintBox3DMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure PositionSaveItemClick(Sender: TObject);
    procedure PositionResetItemClick(Sender: TObject);
    procedure ModusItemClick(Sender: TObject);
    procedure DrawAlwaysItemClick(Sender: TObject);
    procedure MatrixItemClick(Sender: TObject);
  private
    CreatedScreenWidth: Integer;

    FViewPoint: TViewPoint;
    FZoomBase: double;
    FZoom: double;

    FPhi: double;
    FTheta: double;
    FGamma: double;

    xmin, ymin, xmax, ymax: Integer;

    FXpos: Integer;
    FYpos: Integer;
    FIncrementW: double;
    FIncrementT: Integer;
    FZoomIndex: Integer;
    RotaData: TRotaData;
    RotaData1: TRotaData;
    RotaData2: TRotaData;
    RotaData3: TRotaData;
    RotaData4: TRotaData;

    NullpunktOffset: TPoint;
    FPaintRumpf: Boolean;
    FDrawAlways: Boolean;
    FTranslation: Boolean;
    MouseDown: Boolean;
    MouseButton: TMouseButton;
    Painted: Boolean;
    prevx, prevy: Integer;
    MouseDownX, MouseDownY: Integer;
    SavedXPos, SavedYPos: Integer;
    AlwaysShowAngle: Boolean;
    SHeadingPhi: String;
    SPitchTheta: String;
    SBankGamma: String;
    FFixPoint: TRiggPoint;
    procedure ChangeResolution;
    procedure UpdateMinMax;
    procedure DoTrans;
    procedure SetViewPoint(const Value: TViewPoint);
    procedure SetFixPoint(const Value: TRiggPoint);
  protected
    Bitmap: TBitmap;
    EraseBK: Boolean;
    MinTrackX, MinTrackY: Integer;
    MaxTrackX, MaxTrackY: Integer;
    procedure Rotate(Phi, Theta, Gamma, xrot, yrot, zrot: double);
    procedure Translate(x, y: Integer);
    procedure SetAngleText;
    procedure SetZoomText;
    procedure DrawMatrix(Canvas: TCanvas);
    procedure DrawAngleText(Canvas: TCanvas);
    procedure DrawToBitmap;
    procedure InitRotaData;
    procedure PaintBackGround(Image: TBitmap);
  public
    Rigg: TRigg;
    Rotator: TPolarKar;
    HullGraph: TRggGraph;
    RaumGrafik: TRaumGraph;
    Mode: Boolean;
    procedure Draw;
    procedure InitGraph;
    procedure InitRaumGrafik;
    procedure InitHullGraph;
    procedure InitRigg;
    procedure UpdateGraph;
  public
    PaintItemChecked: Boolean;
    MatrixItemChecked: Boolean;
    KeepInsideItemChecked: Boolean;
    RumpfItemChecked: Boolean;
  public
    PaintBox3D: TPaintBox;
    UseDisplayList: Boolean;
    constructor Create;
    destructor Destroy; override;
    procedure Init;
    property ViewPoint: TViewPoint read FViewPoint write SetViewPoint;
    property FixPoint: TRiggPoint read FFixPoint write SetFixPoint;
  end;

implementation

uses
  RggModul,
  RggDoc,
  RggCalc,
  FrmScale,
  RggPBox;

{ TRotaForm }

constructor TRotaForm.Create;
begin
  { do nothing,
    Paintbox3D reference needs to be injected,
    then Init is called from outside.
  }
  KeepInsideItemChecked := True;
end;

destructor TRotaForm.Destroy;
begin
  RiggModul.RotaForm := nil;
  Paintbox3D.Free;
  Paintbox3D := nil;
  Bitmap.Free;
  RaumGrafik.Free;
  HullGraph.Free;
  Rotator.Free;
  inherited;
end;

procedure TRotaForm.Init;
var
  wx, wy: Integer;
  NewPaintBox: TPaintBox;
begin
  FDrawAlways := True;
  AlwaysShowAngle := False;

  MinTrackX := 410;
  MinTrackY := 280;
  MaxTrackX := 1024;
  MaxTrackY := 768;

  CreatedScreenWidth := Screen.Width;
  wx := GetSystemMetrics(SM_CXSCREEN); { Width := Screen.Width }
  wy := GetSystemMetrics(SM_CYSCREEN); { Height := Screen.Height }
  if wx > MaxTrackX then
    wx := MaxTrackX;
  if wy > MaxTrackY then
    wy := MaxTrackY;

  Bitmap := TBitmap.Create;
  with Bitmap do
  begin
    Width := wx;
    Height := wy;
  end;
  PaintBackGround(Bitmap);

  FZoomBase := 0.05;
  FViewPoint := vp3D;
  FFixPoint := ooD0;

  { PaintBox austauschen }
  NewPaintBox := TRggPaintBox.Create(PaintBox3D.Owner);
  try
    NewPaintBox.Parent := PaintBox3D.Parent;
    NewPaintBox.Align := PaintBox3D.Align;
    NewPaintBox.OnMouseDown := PaintBox3DMouseDown;
    NewPaintBox.OnMouseMove := PaintBox3DMouseMove;
    NewPaintBox.OnMouseUp := PaintBox3DMouseUp;
    NewPaintBox.OnPaint := PaintBox3DPaint;

    PaintBox3D.Free;
    PaintBox3D := NewPaintBox;
  except
    NewPaintBox.Free;
  end;

  InitGraph;
  InitRaumGrafik;
  InitHullGraph;
  InitRigg;

  SetViewPoint(FViewPoint);
end;

procedure TRotaForm.InitGraph;
begin
  Rotator := TPolarKar.Create;
  Rotator.OnCalcAngle := Rotator.GetAngle2;
  InitRotaData;
end;

procedure TRotaForm.InitRaumGrafik;
begin
  if UseDisplayList then
    RaumGrafik := TRaumGraph.Create
  else
    RaumGrafik := TGetriebeGraph.Create;
  RaumGrafik.Rotator := Rotator;
  RaumGrafik.Offset := Point(1000,1000);
  RaumGrafik.Zoom := FZoom;
  Raumgrafik.FixPoint := FixPoint;
  if RaumGrafik is TGetriebeGraph then
    TGetriebeGraph(RaumGrafik).Ansicht := vp3D;
end;

procedure TRotaForm.InitHullGraph;
begin
  HullGraph := THullGraph.Create;
  HullGraph.Rotator := Rotator;
  HullGraph.Zoom := FZoom;
  HullGraph.FixPunkt := Raumgrafik.FixPunkt;
end;

procedure TRotaForm.InitRigg;
begin
  Rigg := RiggModul.Rigg;

  RaumGrafik.Salingtyp := Rigg.Salingtyp;
  RaumGrafik.ControllerTyp := Rigg.ControllerTyp;
  RaumGrafik.Koordinaten := Rigg.rP;
  RaumGrafik.SetMastKurve(Rigg.MastLinie, Rigg.lc, Rigg.beta);
  if RaumGrafik is TGetriebeGraph then
    TGetriebeGraph(RaumGrafik).WanteGestrichelt := not Rigg.GetriebeOK;
end;

procedure TRotaForm.UpdateGraph;
begin
  RaumGrafik.Salingtyp := Rigg.Salingtyp;
  RaumGrafik.ControllerTyp := Rigg.ControllerTyp;
  RaumGrafik.Koordinaten := Rigg.rP;
  RaumGrafik.SetMastKurve(Rigg.MastLinie, Rigg.lc, Rigg.beta);
  if RaumGrafik is TGetriebeGraph then
    TGetriebeGraph(RaumGrafik).WanteGestrichelt := not Rigg.GetriebeOK;
 Draw;
end;

procedure TRotaForm.InitRotaData;

  function GetMatrix(Theta, Xrot: double): Matrix4x4;
  begin
    Rotator.Reset;
    Rotator.DeltaTheta := Theta;
    Rotator.XRot := Xrot;
    result := Rotator.Matrix;
  end;

begin
  with RotaData1 do
  begin
    Xpos := -150;
    Ypos := -40;
    Matrix := GetMatrix(0,0);
    ZoomIndex := 3;
    FixPunktIndex := 7;
    IncrementIndex := 3;
    IncrementT := 10;
    IncrementW := 5;
  end;
  with RotaData2 do
  begin
    Xpos := -150;
    Ypos := -50;
    Matrix := GetMatrix(0,90);
    ZoomIndex := 2;
    FixPunktIndex := 8;
    IncrementIndex := 3;
    IncrementT := 10;
    IncrementW := 5;
  end;
  with RotaData3 do
  begin
    Xpos := -170;
    Ypos := -120;
    Matrix := GetMatrix(-90,90);
    ZoomIndex := 5;
    FixPunktIndex := 7;
    IncrementIndex := 3;
    IncrementT := 10;
    IncrementW := 5;
  end;
  with RotaData4 do
  begin
    Xpos := -130;
    Ypos := -80;
    Matrix := GetMatrix(90,-87);
    ZoomIndex := 8;
    FixPunktIndex := 7;
    IncrementIndex := 3;
    IncrementT := 10;
    IncrementW := 5;
  end;
end;

procedure TRotaForm.PaintBox3DPaint(Sender: TObject);
begin
  Draw;
end;

procedure TRotaForm.DrawMatrix(Canvas: TCanvas);
var
  S1, S2, S3: string;
  m4x4: Matrix4x4;
begin
  m4x4 := Rotator.Mat.Mat;
  S1 := Format('%8.4f %8.4f %8.4f',[m4x4[1,1],m4x4[1,2], m4x4[1,3]]);
  S2 := Format('%8.4f %8.4f %8.4f',[m4x4[2,1],m4x4[2,2], m4x4[2,3]]);
  S3 := Format('%8.4f %8.4f %8.4f',[m4x4[3,1],m4x4[3,2], m4x4[3,3]]);
  with Canvas do
  begin
    Font.Name := 'Courier New';
    Font.Size := 10;
    TextOut(20,40, S1);
    TextOut(20,60, S2);
    TextOut(20,80, S3);
  end;
end;

procedure TRotaForm.DrawAngleText(Canvas: TCanvas);
begin
  with Canvas do
  begin
    Font.Name := 'Courier New';
    Font.Size := 10;
    TextOut(20,120, SHeadingPhi);
    TextOut(20,140, SPitchtheta);
    TextOut(20,160, SBankGamma);
  end;
end;

procedure TRotaForm.Draw;
begin
  Painted := False;

  { Nach Änderung der Auflösung Probleme mit dem Grau-Überschreiben. Deshalb: }
  if Screen.Width <> CreatedScreenWidth then
    ChangeResolution;

  if not PaintItemChecked or EraseBK then
  begin
    PaintBackGround(Bitmap);
    EraseBK := False;
  end;

  NullpunktOffset.x := -RaumGrafik.Offset.x + Bitmap.Width div 2 + FXpos;
  NullpunktOffset.y := -RaumGrafik.Offset.y + Bitmap.Height div 2 + FYpos;
  DrawToBitmap;

  if MatrixItemChecked then
    DrawMatrix(Bitmap.Canvas);

  { Bitmap auf den Bildschirm kopieren }
  with PaintBox3D.Canvas do
  begin
    CopyMode := cmSrcCopy;
    Draw(0, 0, Bitmap);
  end;

  Painted := True;
end;

procedure TRotaForm.DrawToBitmap;
begin
  { Variante 1 }
  with Bitmap.Canvas do
  begin
    SetMapMode(Handle, MM_ANISOTROPIC);
    SetWindowExtEx(Handle, 1000, 1000, nil);
    SetWindowOrgEx(Handle, 0, 0, nil);
    SetViewPortExtEx(Handle, 1000, 1000, nil);
    SetViewPortOrgEx(Handle, NullpunktOffset.x, NullpunktOffset.y, nil);
  end;
  RaumGrafik.Coloriert := True;
  if UseDisplayList then
  begin
    RaumGrafik.Update;
    RaumGrafik.UpdateDisplayList;
    RaumGrafik.DL.Draw(Bitmap.Canvas);
  end
  else
    RaumGrafik.Draw(Bitmap.Canvas);

  if FPaintRumpf and (not MouseDown or (MouseDown and FDrawAlways)) then
  begin
    HullGraph.Coloriert := True;
    HullGraph.FixPunkt := RaumGrafik.FixPunkt;
    HullGraph.Draw(Bitmap.Canvas);
  end;
  with Bitmap.Canvas do
  begin
    SetWindowOrgEx(Handle, 0, 0, nil);
    SetViewPortOrgEx(Handle, 0, 0, nil);
    SetMapMode(Handle, MM_TEXT);
  end;
end;

procedure TRotaForm.DoTrans;
begin
  UpdateMinMax;

  if FXpos < xmin then
    FXpos := xmin;
  if FXpos > xmax then
    FXpos := xmax;
  if FYpos < ymin then
    FYpos := ymin;
  if FYpos > ymax then
    FYpos := ymax;

  if not PaintItemChecked then
    EraseBK := True;
  Draw;
end;

procedure TRotaForm.UpdateMinMax;
begin
  if KeepInsideItemChecked then
  begin
    xmin := -Bitmap.Width div 2;
    ymin := -Bitmap.Height div 2;
    xmax := Abs(xmin);
    ymax := Abs(ymin);
    if xmax > xmin + PaintBox3D.Width then
      xmax := xmin + PaintBox3D.Width;
    if ymax > ymin + PaintBox3D.Height then
      ymax := ymin + PaintBox3D.Height;
  end
  else
  begin
    xmin := -3000;
    ymin := -3000;
    xmax := 3000;
    ymax := 3000;
  end;
end;

procedure TRotaForm.ZoomInBtnClick(Sender: TObject);
begin
  if FZoomIndex < 11 then
  begin
    Inc(FZoomIndex);
    FZoom := FZoomBase * LookUpRa10(FZoomIndex);
    HullGraph.Zoom := FZoom;
    RaumGrafik.Zoom := FZoom;
    Draw;
    SetZoomText;
  end;
end;

procedure TRotaForm.ZoomOutBtnClick(Sender: TObject);
begin
  if FZoomIndex > 1 then
  begin
    Dec(FZoomIndex);
    FZoom := FZoomBase * LookUpRa10(FZoomIndex);
    HullGraph.Zoom := FZoom;
    RaumGrafik.Zoom := FZoom;
    Draw;
    SetZoomText;
  end;
end;

procedure TRotaForm.SetFixPoint(const Value: TRiggPoint);
begin
  FFixPoint := Value;
  Raumgrafik.FixPoint := FixPoint;
  HullGraph.FixPunkt := RaumGrafik.FixPunkt;
  Draw;
end;

procedure TRotaForm.RumpfBtnClick(Sender: TObject);
begin
  RumpfItemChecked := not RumpfItemChecked;
  FPaintRumpf := RumpfItemChecked;
  Draw;
end;

procedure TRotaForm.PaintBtnClick(Sender: TObject);
begin
  PaintItemChecked := not PaintItemChecked;
  if not PaintItemChecked then
    EraseBK := True;
  Draw;
end;

procedure TRotaForm.SetViewPoint(const Value: TViewPoint);
begin
  FViewPoint := Value;
  case FViewPoint of
    vpSeite: RotaData := RotaData1;
    vpAchtern: RotaData := RotaData2;
    vpTop: RotaData := RotaData3;
    vp3D: RotaData := RotaData4;
  end;

  FXpos := RotaData.Xpos;
  FYpos := RotaData.Ypos;
  FIncrementT := RotaData.IncrementT;
  FIncrementW := RotaData.IncrementW;
  { Rotationmatrix }
  Rotator.Matrix := RotaData.Matrix;
  Rotator.GetAngle(FPhi, FTheta, FGamma);
  SetAngleText; // Rotate() hier nicht aufrufen, um Matrix nicht zu verändern!
  { Zoom }
  FZoomIndex := RotaData.ZoomIndex;
  FZoom := FZoomBase * LookUpRa10(FZoomIndex);
  SetZoomText;
  RaumGrafik.Zoom := FZoom;
  HullGraph.Zoom := FZoom;
  { Fixpunkt }
  RaumGrafik.FixPoint := FixPoint;
  RaumGrafik.Update; // Rotate;
  HullGraph.FixPunkt := Raumgrafik.FixPunkt;
  { Neuzeichnen }
  EraseBK := True;
  Draw;
end;

procedure TRotaForm.PositionSaveItemClick(Sender: TObject);
begin
  case FViewPoint of
    vpSeite: RotaData := RotaData1;
    vpAchtern: RotaData := RotaData2;
    vpTop: RotaData := RotaData3;
    vp3D: RotaData := RotaData4;
  end;
  with RotaData do
  begin
    Xpos := FXpos;
    Ypos := FYpos;
    Matrix := Rotator.Matrix;
    ZoomIndex := FZoomIndex;
//    FixPunktIndex := FixPunktCombo.ItemIndex;
    IncrementT := FIncrementT;
    IncrementW := FIncrementW;
  end;
  case FViewPoint of
    vpSeite: RotaData1 := RotaData;
    vpAchtern: RotaData2 := RotaData;
    vpTop: RotaData3 := RotaData;
    vp3D: RotaData4 := RotaData;
  end;
end;

procedure TRotaForm.PositionResetItemClick(Sender: TObject);
begin
  InitRotaData;
  SetViewPoint(FViewPoint);
end;

procedure TRotaForm.ChangeResolution;
var
  wx, wy: Integer;
begin
  CreatedScreenWidth := Screen.Width;
  wx := GetSystemMetrics(SM_CXSCREEN); { Width := Screen.Width }
  wy := GetSystemMetrics(SM_CYSCREEN); { Height := Screen.Height }
  if wx > 1024 then
    wx := 1024;
  if wy > 768 then
    wy := 768;

  Bitmap.Palette := 0;
  Bitmap.Free;
  Bitmap := TBitmap.Create;
  with Bitmap do
  begin
    Width := wx;
    Height := wy;
  end;
  PaintBackGround(Bitmap);

  SetViewPoint(FViewPoint);
end;

procedure TRotaForm.KeepInsideItemClick(Sender: TObject);
begin
  KeepInsideItemChecked := not KeepInsideItemChecked;
  if KeepInsideItemChecked then
    Draw;
end;

{ Ergänzung für das Drehen mit der Maus. }

procedure TRotaForm.PaintBox3DMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  MouseDown := True;
  MouseButton := Button;
  prevx := x; MouseDownX := x; SavedXPos := FXPos;
  prevy := y; MouseDownY := y; SavedYPos := FYPos;

  FTranslation :=
    (Abs(RaumGrafik.Offset.x + NullPunktOffset.x - X) < 10) and
    (Abs(RaumGrafik.Offset.y + NullPunktOffset.y - Y) < 10);
end;

procedure TRotaForm.PaintBox3DMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  wx, wy, wz: single;
begin
  if not MouseDown then
    Exit;
  if Mode then
    Exit;
  if MouseButton = mbLeft then
  begin
    wx := (x - prevx) * 0.15;
    wy := (y - prevy) * 0.15;
    wz := 0;
  end
  else
  begin
    wx := 0;
    wy := 0;
    wz := (x - prevx) * 0.3;
  end;
  if Painted then
  begin
    Painted := False;
    if FTranslation or (Shift = [ssLeft, ssRight]) then
      Translate(x,y)
    else
      Rotate(0,0,0,wx,wy,wz);
    Draw;
    prevx := x;
    prevy := y;
  end;
end;

procedure TRotaForm.PaintBox3DMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  MouseDown := False;
  if (prevx = MouseDownX) and (prevy = MouseDownY) then
    EraseBK := True;
  Draw;
end;

procedure TRotaForm.Rotate(Phi, Theta, Gamma, xrot, yrot, zrot: double);
begin
  Rotator.DeltaPhi := Phi;
  Rotator.DeltaTheta := Theta;
  Rotator.DeltaGamma := Gamma;
  Rotator.XRot := Xrot;
  Rotator.YRot := Yrot;
  Rotator.ZRot := Zrot;
  RaumGrafik.Update;
  SetAngleText;
end;

procedure TRotaForm.Translate(x, y: Integer);
begin
  FXpos := SavedXpos - (MouseDownX - x);
  FYpos := SavedYpos - (MouseDownY - y);
  DoTrans;
end;

procedure TRotaForm.SetZoomText;
begin
end;

procedure TRotaForm.SetAngleText;
begin
end;

procedure TRotaForm.ModusItemClick(Sender: TObject);
begin
  Mode := not Mode;
  Rotator.Mode := Mode;

  if AlwaysShowAngle then
    Exit;

  if Mode = False then
  begin
    { Increment - Modus }
    SHeadingPhi := '';
    SPitchTheta := '';
    SBankGamma  := '';
  end;
  if Mode = True then
  begin
    { Absolut - Modus }
    { wenn Ereignis Rotator.OnCalcAngle nicht zugewiesen wurde, dann
      werden FPhi, FTheta und FGamma auf Null gesetzt }
    Rotator.GetAngle(FPhi, FTheta, FGamma);
    Rotate(FPhi, FTheta, FGamma, 0, 0, 0);
    EraseBK := True;
    Draw;
  end;
end;

procedure TRotaForm.DrawAlwaysItemClick(Sender: TObject);
begin
  FDrawAlways := not FDrawAlways;
end;

procedure TRotaForm.MatrixItemClick(Sender: TObject);
begin
  MatrixItemChecked := not MatrixItemChecked;
  Draw;
end;

procedure TRotaForm.PaintBackGround(Image: TBitmap);
var
  R: TRect;
begin
  R := Rect(0, 0, Image.Width, Image.Height);
  Image.Canvas.Brush.Color := clGray;
  Image.Canvas.FillRect(R);
end;

end.
