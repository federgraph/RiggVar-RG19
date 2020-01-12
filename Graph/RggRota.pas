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
  FrmIndicator,
  RggGraph,
  RaumGraph,
  RggHull,
  Vector3D,
  Polarkar;

type
  TRotaForm = class
    procedure PaintBox3DPaint(Sender: TObject);
    procedure LeftBtnClick(Sender: TObject);
    procedure ZoomInBtnClick(Sender: TObject);
    procedure ZoomOutBtnClick(Sender: TObject);
    procedure FixPunktComboChange(Sender: TObject);
    procedure Btn1GradClick(Sender: TObject);
    procedure Step1ItemClick(Sender: TObject);
    procedure Step5ItemClick(Sender: TObject);
    procedure Step10ItemClick(Sender: TObject);
    procedure Step30ItemClick(Sender: TObject);
    procedure PhiDownItemClick(Sender: TObject);
    procedure Step01ItemClick(Sender: TObject);
    procedure PrintItemClick(Sender: TObject);
    procedure RumpfBtnClick(Sender: TObject);
    procedure CloseItemClick(Sender: TObject);
    procedure PlotItemClick(Sender: TObject);
    procedure StatusBarItemClick(Sender: TObject);
    procedure SpeedBarItemClick(Sender: TObject);
    procedure PaintBtnClick(Sender: TObject);
    procedure NullBtnClick(Sender: TObject);
    procedure TransLeftBtnClick(Sender: TObject);
    procedure TransLeftItemClick(Sender: TObject);
    procedure KeepInsideItemClick(Sender: TObject);
    procedure RightButtonClick(Sender: TObject);
    procedure PaintBox3DMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure PaintBox3DMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure PaintBox3DMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FocusEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure PositionSaveItemClick(Sender: TObject);
    procedure PositionResetItemClick(Sender: TObject);
    procedure ModusItemClick(Sender: TObject);
    procedure DrawAlwaysItemClick(Sender: TObject);
    procedure HullItemClick(Sender: TObject);
    procedure FaktorDlgItemClick(Sender: TObject);
    procedure IndicatorItemClick(Sender: TObject);
    procedure IndicatorLocalRotItemClick(Sender: TObject);
    procedure Options3DMenuClick(Sender: TObject);
    procedure Pos1BtnClick(Sender: TObject);
    procedure MatrixItemClick(Sender: TObject);
    procedure Sample420ItemClick(Sender: TObject);
    procedure AngleTextItemClick(Sender: TObject);
  private
    CreatedScreenWidth: Integer;
    MetaFile: TRiggMetaFile;
    MetaCanvas: TMetaFileCanvas;

    ViewPoint: TViewPoint;
    FZoomBase: real;
    FZoom: real;

    FPhi, FTheta, FGamma: real;

    FXpos: Integer;
    FYpos: Integer;
    FIncrementW: real;
    FIncrementT: Integer;
    FZoomIndex: Integer;
    RotaData: TRotaData;
    RotaData1, RotaData2, RotaData3, RotaData4: TRotaData;

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
    procedure PrintIt;
    procedure ChangeResolution;
  protected
    Bitmap: TBitmap;
    EraseBK: Boolean;
    MinTrackX, MinTrackY: Integer;
    MaxTrackX, MaxTrackY: Integer;
    procedure Rotate(Phi, Theta, Gamma, xrot, yrot, zrot: real);
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
    HullGraph: TRggGraph; // THullGraph;
    RaumGrafik: TRaumGrafik; // TGetriebeGraph;
    IndicatorForm: TIndicatorForm;
    BackBmp: TBitmap;
    Mode: Boolean;
    Sample420Memo: TStrings;
    SampleDinghyMemo: TStrings;
    SampleYachtMemo: TStrings;
    SamplePlaningMemo: TStrings;
    procedure IndicatorChanged(Sender: TObject);
    procedure Draw; virtual;
    procedure InitGraph; virtual;
    procedure InitRaumGrafik; virtual;
    procedure InitHullGraph; virtual;
    procedure InitRigg; virtual;
    procedure UpdateGraph; virtual;
  public
    PaintBtnDown: Boolean;
    MatrixItemChecked: Boolean;
    PreviewItemChecked: Boolean;
    KeepInsideItemChecked: Boolean;
    RumpfItemChecked: Boolean;
    IndicatorItemChecked: Boolean;
    IndicatorLocalRotItemChecked: Boolean;
    AngleTextItemChecked: Boolean;
    FixPunktComboText: string;
  public
    PaintBox3D: TPaintBox;
    Modified: Boolean;
    constructor Create;
    destructor Destroy; override;
    procedure Init;
    procedure UpdateAll(Rgg: TRigg);
    procedure UpdateGlobalRigg;
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

{ TRotationForm }

constructor TRotaForm.Create;
begin
  { do nothing,
    Paintbox3D reference needs to be injected,
    then Init is called from outside.
  }
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

  { auskommentierte Anweisungen werden in ChangePosition() ausgeführt }
  // Btn5Grad.Down := true;
  // FIncrementT := 10;
  // FIncrementW := 5;
  FZoomBase := 0.05;
  // FZoomIndex := 7;
  // FZoom := FZoomBase * LookUpRa10(FZoomIndex);
  // FixPunktCombo.ItemIndex := 6;
  ViewPoint := vpTop;
  // SetZoomText;


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

  IndicatorForm := TIndicatorForm.Create(Application);
  IndicatorForm.Rotator := Rotator;
  IndicatorForm.Onchanged := IndicatorChanged;

  ChangePosition(ViewPoint);
end;

procedure TRotaForm.InitGraph; {virtual}
begin
  Rotator := TPolarKar2.Create;
  Rotator.OnCalcAngle := Rotator.GetAngle2;
  InitRotaData;
end;

procedure TRotaForm.InitRaumGrafik; {virtual}
begin
  RaumGrafik := TGetriebeGraph.Create; //TRaumGrafik.Create;
  RaumGrafik.Rotator := Rotator;
  RaumGrafik.Offset := Point(1000,1000);
  RaumGrafik.Zoom := FZoom;
  Raumgrafik.FixName := ComboFixName;
  if RaumGrafik is TGetriebeGraph then
    TGetriebeGraph(RaumGrafik).Ansicht := vp3D;
end;

procedure TRotaForm.InitHullGraph; {virtual}
begin
  HullGraph := THullGraph.Create; //TRggGraph.Create;
  HullGraph.Rotator := Rotator;
  HullGraph.Zoom := FZoom;
  HullGraph.FixPunkt := Raumgrafik.FixPunkt;
end;

procedure TRotaForm.InitRigg; {virtual}
begin
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

  function GetMatrix(Theta, Xrot: real): Matrix4x4;
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
S1, S2, S3: String;
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
    TextOut(20,40,S1);
    TextOut(20,60,S2);
    TextOut(20,80,S3);
  end;
end;

procedure TRotaForm.DrawAngleText(Canvas: TCanvas);
begin
  with Canvas do
  begin
    Font.Name := 'Courier New';
    Font.Size := 10;
    TextOut(20,120,SHeadingPhi);
    TextOut(20,140,SPitchtheta);
    TextOut(20,160,SBankGamma);
  end;
end;

procedure TRotaForm.Draw;
begin
  Painted := False;

  { Nach Änderung der Auflösung Probleme mit dem Grau-Überschreiben. Deshalb: }
  if Screen.Width <> CreatedScreenWidth then
    ChangeResolution;

  if not PaintBtnDown or EraseBK then
  begin
  //if EraseBK then begin {für DrawToBitmap2}
    PaintBackGround(Bitmap);
    EraseBK := False;
  end;

  NullpunktOffset.x := -RaumGrafik.Offset.x + Bitmap.Width div 2 + FXpos;
  NullpunktOffset.y := -RaumGrafik.Offset.y + Bitmap.Height div 2 + FYpos;
  DrawToBitmap1; // bzw. DrawToBitmap2;

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

procedure TRotaForm.DrawToBitmap2; //Variante 2
begin
  { Metafile anlegen, alten Eintrag grau überschreiben }
  MetaCanvas := TMetaFileCanvas.Create(MetaFile, 0);
  try
    if not PaintBtnDown then
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

procedure TRotaForm.PrintIt;
begin
end;

procedure TRotaForm.PhiDownItemClick(Sender: TObject);
begin
//  if Sender = PhiDownItem then
//    LeftBtnClick(LeftBtn)
//  else if Sender = PhiUpItem then
//    LeftBtnClick(RightBtn)
//  else if Sender = ThetaDownItem then
//    LeftBtnClick(DownBtn)
//  else if Sender = ThetaUpItem then
//    LeftBtnClick(UpBtn)
//  else if Sender = GammaDownItem then
//    LeftBtnClick(GammaDownBtn)
//  else if Sender = GammaUpItem then
//    LeftBtnClick(GammaUpBtn);
end;

procedure TRotaForm.LeftBtnClick(Sender: TObject);
//var
//  wp, wt, wg: real;
begin
//  if Mode = False then
//  begin
//    { Incremente }
//    wp := 0; wt := 0; wg := 0;
//    if Sender = LeftBtn then wp := FIncrementW
//    else if Sender = RightBtn then wp := -FIncrementW
//    else if Sender = UpBtn then wt := FIncrementW
//    else if Sender = DownBtn then wt := - FIncrementW
//    else if Sender = GammaUpBtn then wg := FIncrementW
//    else if Sender = GammaDownBtn then wg := - FIncrementW;
//    Rotate(wp, -wt, -wg, 0, 0, 0);
//  end
//  else
//  begin
//    { Absolutwinkel }
//    if Sender = LeftBtn then FPhi := FPhi + FIncrementW
//    else if Sender = RightBtn then FPhi := FPhi - FIncrementW
//    else if Sender = UpBtn then FTheta := FTheta - FIncrementW
//    else if Sender = DownBtn then FTheta := FTheta + FIncrementW
//    else if Sender = GammaUpBtn then FGamma := FGamma + FIncrementW
//    else if Sender = GammaDownBtn then FGamma := FGamma - FIncrementW;
//    if FPhi > 180 then FPhi := FPhi - 360
//    else if FPhi < -180 then FPhi := FPhi + 360
//    else if FTheta > 90 then FTheta := 90
//    else if FTheta < -90 then FTheta := -90
//    else if FGamma > 180 then FGamma := FGamma - 360
//    else if FGamma < -180 then FGamma := FGamma + 360;
//    Rotate(FPhi, FTheta, FGamma, 0, 0, 0);
//  end;
//  Draw;
//  IndicatorForm.UpdateIndicator;
end;

procedure TRotaForm.TransLeftItemClick(Sender: TObject);
begin
//  if Sender = TransLeftItem then
//    TransLeftBtnClick(TransLeftBtn)
//  else if Sender = TransRightItem then
//    TransLeftBtnClick(TransRightBtn)
//  else if Sender = TransUpItem then
//    TransLeftBtnClick(TransUpBtn)
//  else if Sender = TransDownItem then
//    TransLeftBtnClick(TransDownBtn);
end;

procedure TRotaForm.TransLeftBtnClick(Sender: TObject);
//var
//  xmin, ymin, xmax, ymax: Integer;
begin
//  if KeepInsideItemChecked then
//  begin
//    xmin := -Bitmap.Width div 2;
//    ymin := -Bitmap.Height div 2;
//    xmax := xmin + PaintBox3D.Width;
//    ymax := ymin + PaintBox3D.Height;
//  end
//  else
//  begin
//    xmin := -3000;
//    ymin := -3000;
//    xmax := 3000;
//    ymax := 3000;
//  end;
//  if Sender = TransLeftBtn then FXpos := FXpos - FIncrementT
//  else if Sender = TransRightBtn then FXpos := FXpos + FIncrementT
//  else if Sender = TransUpBtn then FYpos := FYpos - FIncrementT
//  else if Sender = TransDownBtn then FYpos := FYpos + FIncrementT;
//  if FXpos < xmin then FXpos := xmin
//  else if FXpos > xmax then FXpos := xmax
//  else if FYpos < ymin then FYpos := ymin
//  else if FYpos > ymax then FYpos := ymax;
//  if not PaintBtnDown then EraseBK := True;
//  Draw;
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
  S: String;
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

procedure TRotaForm.Btn1GradClick(Sender: TObject);
begin
//  if Sender = Btn01Grad then
//  begin
//    FIncrementW := 0.1;
//    FIncrementT := 1;
//  end
//  else if Sender = Btn1Grad then
//  begin
//    FIncrementW := 1;
//    FIncrementT := 5;
//  end
//  else if Sender = Btn5Grad then
//  begin
//    FIncrementW := 5;
//    FIncrementT := 10;
//  end
//  else if Sender = Btn10Grad then
//  begin
//   FIncrementW := 10;
//   FIncrementT := 30;
//  end
//  else if Sender = Btn30Grad then
//  begin
//    FIncrementW := 30;
//    FIncrementT := 100;
//  end;
end;

procedure TRotaForm.Step01ItemClick(Sender: TObject);
begin
  FIncrementW := 0.1;
  FIncrementT := 1;
//  Btn01Grad.Down := True;
end;

procedure TRotaForm.Step1ItemClick(Sender: TObject);
begin
  FIncrementW := 1;
  FIncrementT := 5;
//  Btn1Grad.Down := True;
end;

procedure TRotaForm.Step5ItemClick(Sender: TObject);
begin
  FIncrementW := 5;
  FIncrementT := 10;
//  Btn5Grad.Down := True;
end;

procedure TRotaForm.Step10ItemClick(Sender: TObject);
begin
  FIncrementW := 10;
  FIncrementT := 30;
//  Btn10Grad.Down := True;
end;

procedure TRotaForm.Step30ItemClick(Sender: TObject);
begin
  FIncrementW := 30;
  FIncrementT := 100;
//  Btn30Grad.Down := True;
end;

procedure TRotaForm.PrintItemClick(Sender: TObject);
begin
  PrintIt;
end;

procedure TRotaForm.RumpfBtnClick(Sender: TObject);
begin
  RumpfItemChecked := not RumpfItemChecked;
//  RumpfBtn.Down := RumpfItemChecked;
  FPaintRumpf := RumpfItemChecked;
  Draw;
end;

procedure TRotaForm.CloseItemClick(Sender: TObject);
begin
end;

procedure TRotaForm.PlotItemClick(Sender: TObject);
//var
//  List: TStringList;
begin
//  if SaveDialog.Execute then
//  begin
//    List := TStringList.Create;
//    try
//      RaumGrafik.GetPlotList(List);
//      if FPaintRumpf then HullGraph.GetPlotList(List);
//      List.SaveToFile(SaveDialog.FileName);
//    finally
//      List.Free;
//    end;
//  end;
end;

procedure TRotaForm.Options3DMenuClick(Sender: TObject);
begin
  IndicatorItemChecked := IndicatorForm.Visible;
  IndicatorLocalRotItemChecked := not IndicatorForm.GlobalRot;
end;

procedure TRotaForm.StatusBarItemClick(Sender: TObject);
begin
//  StatusBarItem.Checked := not StatusBarItem.Checked;
//  if StatusBarItem.Checked then
//    StatusBar.Visible := True
//  else
//    StatusBar.Visible := False;
end;

procedure TRotaForm.SpeedBarItemClick(Sender: TObject);
begin
//  SpeedBarItem.Checked := not SpeedBarItem.Checked;
//  if SpeedBarItem.Checked then
//    ToolbarPanel.Visible := True
//  else
//    ToolbarPanel.Visible := False;
end;

procedure TRotaForm.PaintBtnClick(Sender: TObject);
begin
//  PaintItemChecked := not PaintItem.Checked;
//  PaintBtnDown := PaintItem.Checked;
//  if not PaintBtnDown then
//    EraseBK := True;
//  Draw;
end;

procedure TRotaForm.NullBtnClick(Sender: TObject);
begin
//  if ViewPoint = vp3D then
//    ViewPoint := vpSeite
//  else
//    inc(ViewPoint);
//  case ViewPoint of
//    vpSeite: Pos1Btn.Down := True;
//    vpAchtern: Pos2Btn.Down := True;
//    vpTop: Pos3Btn.Down := True;
//    vp3D: Pos4Btn.Down := True;
//  end;
//  ChangePosition(ViewPoint);
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
//  FocusEdit.Text := IntToStr(Ord(ViewPoint)+1);

  FXpos := RotaData.Xpos;
  FYpos := RotaData.Ypos;
  { Increment }
//  case RotaData.IncrementIndex of
//    1: Btn01Grad.Down := true;
//    2: Btn1Grad.Down := true;
//    3: Btn5Grad.Down := true;
//    4: Btn10Grad.Down := true;
//    5: Btn30Grad.Down := true;
//  end;
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
  IndicatorForm.UpdateIndicator;
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
//    if Btn01Grad.Down = true then IncrementIndex := 1
//    else if Btn1Grad.Down = true then IncrementIndex := 2
//    else if Btn5Grad.Down = true then IncrementIndex := 3
//    else if Btn10Grad.Down = true then IncrementIndex := 4
//    else if Btn30Grad.Down = true then IncrementIndex := 5;
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

procedure TRotaForm.Pos1BtnClick(Sender: TObject);
//var
//  temp: TViewPoint;
begin
//  if Sender = Pos1Btn then temp := vpSeite
//  else if Sender = Pos2Btn then temp := vpAchtern
//  else if Sender = Pos3Btn then temp := vpTop
//  else if Sender = Pos4Btn then temp := vp3D
//  else temp := vp3D;
//
//  ChangePosition(temp);
end;

procedure TRotaForm.ChangeResolution;
var
  wx, wy: Integer;
begin
  CreatedScreenWidth := Screen.Width;
  wx := GetSystemMetrics(SM_CXSCREEN); { Width := Screen.Width }
  wy := GetSystemMetrics(SM_CYSCREEN); { Height := Screen.Height }
  if wx > 1024 then wx := 1024;
  if wy > 768 then wy := 768;

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

procedure TRotaForm.RightButtonClick(Sender: TObject);
begin
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
  if not MouseDown then Exit;
  if Mode then Exit;
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
  IndicatorForm.UpdateIndicator;
end;

procedure TRotaForm.Rotate(Phi, Theta, Gamma, xrot, yrot, zrot: real);
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
var
  xmin, ymin, xmax, ymax: Integer;
begin
  if KeepInsideItemChecked then
  begin
    xmin := -Bitmap.Width div 2;
    ymin := -Bitmap.Height div 2;
    xmax := xmin + PaintBox3D.Width;
    ymax := ymin + PaintBox3D.Height;
  end
  else
  begin
    xmin := -3000;
    ymin := -3000;
    xmax := 3000;
    ymax := 3000;
  end;
  FXpos := SavedXpos - (MouseDownX - x);
  FYpos := SavedYpos - (MouseDownY - y);
  if FXpos < xmin then FXpos := xmin
  else if FXpos > xmax then
    FXpos := xmax
  else if FYpos < ymin then
    FYpos := ymin
  else if FYpos > ymax then
    FYpos := ymax;
  if not PaintBtnDown then
    EraseBK := True;
  Draw;
end;

procedure TRotaForm.FocusEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
//  if (ssCtrl in Shift) then begin
//    if Key = VK_Left then begin TransLeftBtnClick(TransLeftBtn); Key := 0; end
//    else if Key = VK_Right then begin TransLeftBtnClick(TransRightBtn); Key := 0; end
//    else if Key = VK_Up then begin TransLeftBtnClick(TransUpBtn); Key := 0; end
//    else if Key = VK_Down then begin TransLeftBtnClick(TransDownBtn); Key := 0; end;
//  end
//
//  else if (ssShift in Shift) then begin
//    if Key = VK_Left then begin LeftBtnClick(GammaDownBtn); Key := 0; end
//    else if Key = VK_Right then begin LeftBtnClick(GammaUpBtn); Key := 0; end
//    else if (Key = VK_Up) then begin {LeftBtnClick(LeftBtn);} Key := 0; end
//    else if (Key = VK_Down) then begin {LeftBtnClick(RightBtn);} Key := 0; end;
//  end
//
//  else begin
//    if Key = VK_Left then begin LeftBtnClick(LeftBtn); Key := 0; end
//    else if Key = VK_Right then begin LeftBtnClick(RightBtn); Key := 0; end
//    else if Key = VK_Up then begin LeftBtnClick(UpBtn); Key := 0; end
//    else if Key = VK_Down then begin LeftBtnClick(DownBtn); Key := 0; end
//    else if Key = VK_Add then begin ZoomInBtnClick(Sender); Key := 0; end
//    else if Key = VK_Subtract then begin ZoomOutBtnClick(Sender); Key := 0; end
//    else if Key = $20 then begin EraseBK := True; Draw; end;
//  end;
end;

procedure TRotaForm.SetZoomText;
begin
//  StatusBar.Panels.Items[7].Text := FormatFloat('0.0#',LookUpRa10(FZoomIndex));
end;

procedure TRotaForm.SetAngleText;
begin
  { falls Increment Mode }
  if (Mode = False) and (AlwaysShowAngle = False) then
    Exit;
  if (Mode = False) and (AlwaysShowAngle = True) then
    Rotator.GetAngle(FPhi, FTheta, FGamma);

  SHeadingPhi := Format('Heading: %5.1f',[FPhi]);
  SPitchTheta := Format('Pitch:   %5.1f',[FTheta]);
  SBankGamma  := Format('Bank:    %5.1f',[FGamma]);
//  with StatusBar.Panels do
//  begin
//    BeginUpdate;
//    try
//      Items[1].Text := Format('%5.1f',[FPhi]);
//      Items[3].Text := Format('%5.1f',[FTheta]);
//      Items[5].Text := Format('%5.1f',[FGamma]);
//    finally
//      EndUpdate;
//    end;
//  end;
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

procedure TRotaForm.HullItemClick(Sender: TObject);
begin
//  if OpenDialog.Execute then
//  begin
//    with HullGraph as THullGraph do
//    begin
//      VertexFileName := OpenDialog.FileName;
//      VertexMemo := nil;
//      Load;
//    end;
//    Draw;
//  end;
end;

procedure TRotaForm.Sample420ItemClick(Sender: TObject);
//var
//  Memo: TStrings;
begin
//  Memo := nil;
//  if Sender = Sample420Item then
//    Memo := Sample420Memo;
//  if Sender = SampleDinghyItem then
//    Memo := SampleDinghyMemo;
//  if Sender = SampleYachtItem then
//    Memo := SampleYachtMemo;
//  if Sender = SamplePlaningItem then
//    Memo := SamplePlaningMemo;
//
//  if Assigned(Memo) then
//  begin
//    with HullGraph as THullGraph do
//    begin
//      VertexFileName := '';
//      VertexMemo := Memo;
//      Load;
//    end;
//    Draw;
//  end;
end;

procedure TRotaForm.FaktorDlgItemClick(Sender: TObject);
begin
//  RumpfFaktorDlg := TRumpfFaktorDlg.Create(Application);
//  try
//    with HullGraph as THullGraph do
//    begin
//      RumpfFaktorDlg.Caption := 'Rumpf skalieren';
//      RumpfFaktorDlg.GroupBox.Caption := 'Faktor in %';
//      RumpfFaktorDlg.L := Round(Factor.x * 100);
//      RumpfFaktorDlg.B := Round(Factor.y * 100);
//      RumpfFaktorDlg.H := Round(Factor.z * 100);
//      RumpfFaktorDlg.ShowModal;
//      if RumpfFaktorDlg.OKBtn = mrOK then
//      begin
//        Factor.x := RumpfFaktorDlg.L / 100;
//        Factor.y := RumpfFaktorDlg.B / 100;
//        Factor.z := RumpfFaktorDlg.H / 100;
//        Load;
//      end;
//    end;
//    Draw;
//  finally
//    RumpfFaktorDlg.Free;
//  end;
end;

procedure TRotaForm.IndicatorItemClick(Sender: TObject);
begin
  IndicatorItemChecked := not IndicatorItemChecked;
  if IndicatorItemChecked then
  begin
    IndicatorForm.Show;
    IndicatorForm.UpdateIndicator;
  end
  else
    IndicatorForm.Hide;
end;

procedure TRotaForm.IndicatorLocalRotItemClick(Sender: TObject);
begin
  with IndicatorForm do
    GlobalRot := not GlobalRot;
end;

procedure TRotaForm.IndicatorChanged(Sender: TObject);
begin
  RaumGrafik.Update;
  Rotator.GetAngle(FPhi, FTheta, FGamma);
  SetAngleText;
  Draw;
end;

procedure TRotaForm.MatrixItemClick(Sender: TObject);
begin
  MatrixItemChecked := not MatrixItemChecked;
  Draw;
end;

procedure TRotaForm.AngleTextItemClick(Sender: TObject);
begin
  AngleTextItemChecked := not AngleTextItemChecked;
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
    Brush.Color := clGray; //BtnFace;
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

procedure TRotaForm.UpdateGlobalRigg;
{$ifdef Rigg19}
var
  RggDocument: TRggDocument;
{$endif}
begin
{$ifdef Rigg19}
  RggDocument := TRggDocument.Create;
  try
   Rigg.GetDocument(RggDocument);
   RiggModul.Neu(RggDocument);
   if Rigg.ManipulatorMode <> RiggModul.Rigg.ManipulatorMode then
     RiggModul.WinkelBtnDown := not RiggModul.WinkelBtnDown;
  finally
    RggDocument.Free;
  end;
{$endif}
end;

procedure TRotaForm.UpdateAll(Rgg: TRigg);
begin
  { Local Rigg nur dann automatisch nachführen, wenn RightPanel sichtbar ist
    und der Typ verändert wurde. Nicht nachführen, wenn nur die Werte verändert
    wurden! Der TrackBar und die Labels werden daher ungültig. Die Grafik kann
    wegspringen, wenn der Trackbar verändert wird. }
//  if RightPanel.Visible and
  if ((Rigg.SalingTyp <> Rgg.SalingTyp) or
      (Rigg.ControllerTyp <> Rgg.ControllerTyp) or
      (Rigg.ManipulatorMode <> Rgg.ManipulatorMode) or
       Modified) then
  begin
    UpdateLocalRigg;
//    GetListBoxItems;
  end;

  with RaumGrafik do
  begin
    Salingtyp := Rgg.Salingtyp;
    ControllerTyp := Rgg.ControllerTyp;
    Koordinaten := Rgg.rP;
    SetMastKurve(Rgg.MastLinie, Rgg.lc, Rgg.beta);
    with RaumGrafik as TGetriebeGraph do
      WanteGestrichelt := not Rgg.GetriebeOK;
  end;
end;

end.
