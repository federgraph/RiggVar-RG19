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
  RggTypes,
  RggGBox,
  Rggmat01,
  Rggunit4,
  RggGraph,
  RaumGraph,
  RggHull,
  Vector3D,
  Polarkar;

type
  TRotaForm = class
    procedure PaintBox3DPaint(Sender: TObject);
    procedure ZoomInBtnClick(Sender: TObject);
    procedure ZoomOutBtnClick(Sender: TObject);
    procedure FixPunktComboChange(Sender: TObject);
    procedure RumpfBtnClick(Sender: TObject);
    procedure PaintBtnClick(Sender: TObject);
    procedure NullBtnClick(Sender: TObject);
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
    MetaFile: TRiggMetaFile;
    MetaCanvas: TMetaFileCanvas;

    ViewPoint: TViewPoint;
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
    function ComboFixName: TRiggPoints;
    procedure ChangeResolution;
    procedure UpdateMinMax;
    procedure DoTrans;
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
    procedure DrawToBitmap1;
    procedure DrawToBitmap2;
    procedure InitRotaData;
    procedure PaintBackGround(Image: TBitmap);
    procedure ChangePosition(aViewPoint: TViewPoint);
  public
    Rigg: TRigg;
    Rotator: TPolarKar2;
    HullGraph: TRggGraph;
    RaumGrafik: TRaumGrafik;
    BackBmp: TBitmap;
    Mode: Boolean;
    Sample420Memo: TStrings;
    SampleDinghyMemo: TStrings;
    SampleYachtMemo: TStrings;
    SamplePlaningMemo: TStrings;
    procedure Draw; virtual;
    procedure InitGraph; virtual;
    procedure InitRaumGrafik; virtual;
    procedure InitHullGraph; virtual;
    procedure InitRigg; virtual;
    procedure UpdateGraph; virtual;
  public
    PaintItemChecked: Boolean;
    MatrixItemChecked: Boolean;
    KeepInsideItemChecked: Boolean;
    RumpfItemChecked: Boolean;
    FixPunktComboText: string;
    PreviewItemChecked: Boolean;
  public
    PaintBox3D: TPaintBox;
    constructor Create;
    destructor Destroy; override;
    procedure Init;
   public
    Modified: Boolean;
    procedure UpdateAll(Rgg: TRigg);
//    procedure UpdateGlobalRigg;
    procedure UpdateLocalRigg;
  end;

var
  RotaForm: TRotaForm;

implementation

uses
  RiggUnit,
  RggDoc,
  Vcalc116,
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
  BackBmp.Free;
  Bitmap.Free;
  MetaFile.Free;
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

  Metafile := TRiggMetaFile.Create;
  with Metafile do
  begin
    Width := Bitmap.Width;
    Height := Bitmap.Height;
  end;

  FZoomBase := 0.05;
  ViewPoint := vpTop;

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

  ChangePosition(ViewPoint);
end;

procedure TRotaForm.InitGraph;
begin
  { virtual }
  Rotator := TPolarKar2.Create;
  Rotator.OnCalcAngle := Rotator.GetAngle2;
  InitRotaData;
end;

procedure TRotaForm.InitRaumGrafik;
begin
  { virtual }
  RaumGrafik := TGetriebeGraph.Create;
  RaumGrafik.Rotator := Rotator;
  RaumGrafik.Offset := Point(1000,1000);
  RaumGrafik.Zoom := FZoom;
  Raumgrafik.FixName := ComboFixName;
  if RaumGrafik is TGetriebeGraph then
    TGetriebeGraph(RaumGrafik).Ansicht := vp3D;
end;

procedure TRotaForm.InitHullGraph;
begin
  { virtual }
  HullGraph := THullGraph.Create;
  HullGraph.Rotator := Rotator;
  HullGraph.Zoom := FZoom;
  HullGraph.FixPunkt := Raumgrafik.FixPunkt;
end;

procedure TRotaForm.InitRigg;
begin
  { virtual }
  Rigg := TRigg.Create;
  Rigg.ControllerTyp := ctOhne;

  RaumGrafik.Salingtyp := Rigg.Salingtyp;
  RaumGrafik.ControllerTyp := Rigg.ControllerTyp;
  RaumGrafik.Koordinaten := Rigg.rP;
  RaumGrafik.SetMastKurve(Rigg.MastLinie, Rigg.lc, Rigg.beta);
  if RaumGrafik is TGetriebeGraph then
    TGetriebeGraph(RaumGrafik).WanteGestrichelt := not Rigg.GetriebeOK;
end;

procedure TRotaForm.UpdateGraph;
begin
 { virtual }
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
    Rotator.Xrot := Xrot;
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
    Xpos := -130;
    Ypos := -80;
    Matrix := GetMatrix(90,-87);
    ZoomIndex := 8;
    FixPunktIndex := 7;
    IncrementIndex := 3;
    IncrementT := 10;
    IncrementW := 5;
  end;
  with RotaData4 do
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
  m4x4 := Rotator.mat.mat;
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
  DrawToBitmap1;

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

procedure TRotaForm.DrawToBitmap1;
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

procedure TRotaForm.DrawToBitmap2;
begin
  { Metafile anlegen, alten Eintrag grau überschreiben }
  MetaCanvas := TMetaFileCanvas.Create(MetaFile, 0);
  try
    if not PaintItemChecked then
      MetaCanvas.Draw(0,0,MetaFile);
    RaumGrafik.Coloriert := True;
    RaumGrafik.Draw(MetaCanvas);
    if FPaintRumpf = True then
    begin
      HullGraph.Coloriert := True;
      HullGraph.FixPunkt := RaumGrafik.FixPunkt;
      HullGraph.Draw(MetaCanvas);
    end;
  finally
    MetaCanvas.Free;
  end;
  { Metafile auf das Bitmap schreiben }
  Bitmap.Canvas.Draw(NullpunktOffset.x, NullpunktOffset.y, MetaFile);
  { aktuelles Bild mit grauem Stift in das Metafile schreiben }
  MetaCanvas := TMetaFileCanvas.Create(MetaFile, 0);
  try
    RaumGrafik.Coloriert := False;
    RaumGrafik.Draw(MetaCanvas);
    if FPaintRumpf = True then
    begin
      HullGraph.Coloriert := False;
      HullGraph.FixPunkt := RaumGrafik.FixPunkt;
      HullGraph.Draw(MetaCanvas);
    end;
  finally
    MetaCanvas.Free;
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

procedure TRotaForm.FixPunktComboChange(Sender: TObject);
begin
  Raumgrafik.FixName := ComboFixName;
  HullGraph.FixPunkt := RaumGrafik.FixPunkt;
  Draw;
end;

function TRotaForm.ComboFixName: TRiggPoints;
var
  NewFixName: TRiggPoints;
  S: string;
begin
  NewFixName := ooD0;
  S := FixPunktComboText;
  if S = 'A0' then NewFixName := ooA0
  else if S = 'B0' then NewFixName := ooB0
  else if S = 'C0' then NewFixName := ooC0
  else if S = 'D0' then NewFixName := ooD0
  else if S = 'E0' then NewFixName := ooE0
  else if S = 'F0' then NewFixName := ooF0
  else if S = 'A' then NewFixName := ooA
  else if S = 'B' then NewFixName := ooB
  else if S = 'C' then NewFixName := ooC
  else if S = 'D' then NewFixName := ooD
  else if S = 'E' then NewFixName := ooE
  else if S = 'F' then NewFixName := ooF;
  Result := NewFixName;
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

procedure TRotaForm.ChangePosition(aViewPoint: TViewPoint);
begin
  ViewPoint := aViewPoint;
  case ViewPoint of
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
//  FixPunktCombo.ItemIndex := RotaData.FixpunktIndex;
  RaumGrafik.FixName := ComboFixName;
  RaumGrafik.Update; // Rotate;
  HullGraph.FixPunkt := Raumgrafik.FixPunkt;
  { Neuzeichnen }
  EraseBK := True; Draw;
end;

procedure TRotaForm.PositionSaveItemClick(Sender: TObject);
begin
  case ViewPoint of
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
  case ViewPoint of
    vpSeite: RotaData1 := RotaData;
    vpAchtern: RotaData2 := RotaData;
    vpTop: RotaData3 := RotaData;
    vp3D: RotaData4 := RotaData;
  end;
end;

procedure TRotaForm.PositionResetItemClick(Sender: TObject);
begin
  InitRotaData;
  ChangePosition(ViewPoint);
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

  MetaFile.Free;
  Metafile := TRiggMetaFile.Create;
  with Metafile do
  begin
    Width := Bitmap.Width;
    Height := Bitmap.Height;
  end;

  ChangePosition(ViewPoint);
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
  wx, wy, wz: Integer;
begin
  if not MouseDown then
    Exit;
  if Mode then
    Exit;
  if MouseButton = mbLeft then
  begin
    wx := Round((x - prevx) * 360 / PaintBox3D.Width);
    wy := Round((y - prevy) * 360 / PaintBox3D.Height);
    wz := 0;
  end
  else
  begin
    wx := 0;
    wy := 0;
    wz := Round((x - prevx) * 360 / PaintBox3D.Width);
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
  Rotator.Xrot := Xrot;
  Rotator.Yrot := Yrot;
  Rotator.Zrot := Zrot;
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
    EraseBK := True; Draw;
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
  P: TPoint;
begin
  R := Rect(0, 0, Image.Width, Image.Height);
  with Image.Canvas do
  begin
    Brush.Color := clGray;
    FillRect(R);
    if BackBmp <> nil then
      if (BackBmp.Width < 100) and (BackBmp.Height < 100) then
        P := Point(30, 5)
      else
        P := Point(0,0);
      Draw(P.x,P.y,BackBmp);
  end;
end;

procedure TRotaForm.UpdateLocalRigg;
{$ifdef Rigg19}
var
  RggDocument: TRggDocument;
{$endif}
begin
{$ifdef Rigg19}
  RggDocument := TRggDocument.Create;
  try
   RiggModul.Rigg.UpdateGSB;
   RiggModul.Rigg.GetDocument(RggDocument);
   Rigg.SetDocument(RggDocument);
   { ManipulatorMode nicht in RggDocument! }
   Rigg.ManipulatorMode := RiggModul.Rigg.ManipulatorMode;
   Modified := False;
  finally
    RggDocument.Free;
  end;
{$endif}
end;

//procedure TRotaForm.UpdateGlobalRigg;
//{$ifdef Rigg19}
//var
//  RggDocument: TRggDocument;
//{$endif}
//begin
//{$ifdef Rigg19}
//  RggDocument := TRggDocument.Create;
//  try
//   Rigg.GetDocument(RggDocument);
//   RiggModul.Neu(RggDocument);
//   if Rigg.ManipulatorMode <> RiggModul.Rigg.ManipulatorMode then
//     RiggModul.WinkelBtnDown := not RiggModul.WinkelBtnDown;
//  finally
//    RggDocument.Free;
//  end;
//{$endif}
//end;

procedure TRotaForm.UpdateAll(Rgg: TRigg);
begin
  { Local Rigg nur dann automatisch nachführen, wenn der Typ verändert wurde. }
  if ((Rigg.SalingTyp <> Rgg.SalingTyp) or
      (Rigg.ControllerTyp <> Rgg.ControllerTyp) or
      (Rigg.ManipulatorMode <> Rgg.ManipulatorMode) or
       Modified) then
  begin
    UpdateLocalRigg;
  end;

  RaumGrafik.Salingtyp := Rgg.Salingtyp;
  RaumGrafik.ControllerTyp := Rgg.ControllerTyp;
  RaumGrafik.Koordinaten := Rgg.rP;
  RaumGrafik.SetMastKurve(Rgg.MastLinie, Rgg.lc, Rgg.beta);
  with RaumGrafik as TGetriebeGraph do
    WanteGestrichelt := not Rgg.GetriebeOK;
end;

end.
