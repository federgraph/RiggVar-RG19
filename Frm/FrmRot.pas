unit FrmRot;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.Classes,
  System.Types,
  System.UITypes,
  Vcl.Graphics,
  Vcl.Forms,
  Vcl.Controls,
  Vcl.Menus,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.Buttons,
  Vcl.ExtCtrls,
  Vcl.Printers,
  Vcl.ComCtrls,
  Vcl.ExtDlgs,
  uRggPrinter,
  RggTypes,
  RggGBox,
  Rggmat01,
  Rggunit4,
  Print004,
  FrmIndicator,
  RggGraph,
  RaumGraph,
  RggHull,
  Vector3D,
  Polarkar;

type
  TRotationForm = class(TForm)
    SaveDialog: TSaveDialog;
    ToolbarPanel: TPanel;
    RahmenPanel: TPanel;
    PaintBox3D: TPaintBox;
    FixPunktCombo: TComboBox;
    Btn01Grad: TSpeedButton;
    Btn1Grad: TSpeedButton;
    Btn5Grad: TSpeedButton;
    Btn10Grad: TSpeedButton;
    Btn30Grad: TSpeedButton;
    LeftBtn: TSpeedButton;
    RightBtn: TSpeedButton;
    UpBtn: TSpeedButton;
    DownBtn: TSpeedButton;
    GammaDownBtn: TSpeedButton;
    GammaUpBtn: TSpeedButton;
    ZoomOutBtn: TSpeedButton;
    ZoomInBtn: TSpeedButton;
    TransLeftBtn: TSpeedButton;
    TransRightBtn: TSpeedButton;
    TransUpBtn: TSpeedButton;
    TransDownBtn: TSpeedButton;
    Panel: TPanel;
    LeftButton: TSpeedButton;
    RightButton: TSpeedButton;
    StatusBar: TStatusBar;
    FocusEdit: TEdit;
    OpenDialog: TOpenDialog;
    OpenPictureDialog: TOpenPictureDialog;
    pnPositionTools: TPanel;
    Pos1Btn: TSpeedButton;
    Pos2Btn: TSpeedButton;
    Pos3Btn: TSpeedButton;
    Pos4Btn: TSpeedButton;
    MemoryBtn: TSpeedButton;
    NullBtn: TSpeedButton;
    PaintBtn: TSpeedButton;
    RumpfBtn: TSpeedButton;
    PreviewBtn: TSpeedButton;
    QuerBtn: TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
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
    procedure A0_ItemClick(Sender: TObject);
    procedure PreviewItemClick(Sender: TObject);
    procedure PrintItemClick(Sender: TObject);
    procedure RumpfBtnClick(Sender: TObject);
    procedure CloseItemClick(Sender: TObject);
    procedure PlotItemClick(Sender: TObject);
    procedure GrafikMenuClick(Sender: TObject);
    procedure StatusBarItemClick(Sender: TObject);
    procedure SpeedBarItemClick(Sender: TObject);
    procedure PaintBtnClick(Sender: TObject);
    procedure NullBtnClick(Sender: TObject);
    procedure TransLeftBtnClick(Sender: TObject);
    procedure TransLeftItemClick(Sender: TObject);
    procedure KeepInsideItemClick(Sender: TObject);
    procedure LeftButtonClick(Sender: TObject);
    procedure RightButtonClick(Sender: TObject);
    procedure ToolbarPanelResize(Sender: TObject);
    procedure PaintBox3DMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBox3DMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure PaintBox3DMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FocusEditKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure PositionSaveItemClick(Sender: TObject);
    procedure PositionResetItemClick(Sender: TObject);
    procedure ModusItemClick(Sender: TObject);
    procedure DrawAlwaysItemClick(Sender: TObject);
    procedure HullItemClick(Sender: TObject);
    procedure FaktorDlgItemClick(Sender: TObject);
    procedure OpenBackBmpItemClick(Sender: TObject);
    procedure CloseBackBmpItemClick(Sender: TObject);
    procedure IndicatorItemClick(Sender: TObject);
    procedure IndicatorLocalRotItemClick(Sender: TObject);
    procedure Options3DMenuClick(Sender: TObject);
    procedure Pos1BtnClick(Sender: TObject);
    procedure PosiToolItemClick(Sender: TObject);
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
    function GetButton(Tag: Integer): TControl;
    function GetButtonCount: Integer;
    procedure EnableScrollButtons;
    procedure RedoButtons;
  protected
    hOldPal, hPal: HPalette;
    Bitmap: TBitmap;
    EraseBK: Boolean;
    MinTrackX, MinTrackY: Integer;
    MaxTrackX, MaxTrackY: Integer;
    function GetFirstVisibleButton: TControl;
    function GetLastVisibleButton: TControl;
    function IsButtonVisible(Button: TControl): Boolean;
    property Buttons[Index: Integer]: TControl read GetButton;
    property ButtonCount: Integer read GetButtonCount;
    procedure Rotate(Phi, Theta, Gamma, xrot, yrot, zrot: real);
    procedure Translate(x, y: Integer);
    procedure SetAngleText;
    procedure SetZoomText;
    procedure DrawPreviewBox;
    procedure DrawMatrix(Canvas: TCanvas);
    procedure DrawAngleText(Canvas: TCanvas);
    procedure DrawToBitmap1;
    procedure DrawToBitmap2;
    procedure wmGetMinMaxInfo(var Msg: TWMGetMinMaxInfo); message wm_GetMinMaxInfo;
    procedure InitRotaData;
    procedure PaintBackGround(Image: TBitmap);
    procedure ChangePosition(aViewPoint: TViewPoint);
    procedure ChangeRotationHints;
  public
    Rigg: TRigg;
    Rotator: TPolarKar2;
    HullGraph: TRggGraph; // THullGraph;
    RaumGrafik: TRaumGrafik; // TGetriebeGraph;
    IndicatorForm: TIndicatorForm;
    BackBmp: TBitmap;
    Preview: TPreview;
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
    procedure InitPreview; virtual;
    procedure UpdateGraph; virtual;
  public
    KeepInsideItemChecked: Boolean;
  public
    MainMenu: TMainMenu;
    GrafikMenu: TMenuItem;
    ZoomItem: TMenuItem;
    ZoomInItem: TMenuItem;
    ZoomOutItem: TMenuItem;
    DrehenItem: TMenuItem;
    PhiDownItem: TMenuItem;
    PhiUpItem: TMenuItem;
    ThetaDownItem: TMenuItem;
    ThetaUpItem: TMenuItem;
    GammaDownItem: TMenuItem;
    GammaUpItem: TMenuItem;
    VerschiebenItem: TMenuItem;
    TransLeftItem: TMenuItem;
    TransRightItem: TMenuItem;
    TransUpItem: TMenuItem;
    TransDownItem: TMenuItem;
    MinusItem1: TMenuItem;
    DrehpunktItem: TMenuItem;
    A0_Item: TMenuItem;
    B0_Item: TMenuItem;
    C0_Item: TMenuItem;
    D0_Item: TMenuItem;
    E0_Item: TMenuItem;
    F0_Item: TMenuItem;
    MinusItem8: TMenuItem;
    A_Item: TMenuItem;
    B_Item: TMenuItem;
    C_Item: TMenuItem;
    D_Item: TMenuItem;
    E_Item: TMenuItem;
    F_Item: TMenuItem;
    StepItem: TMenuItem;
    Step01Item: TMenuItem;
    Step1Item: TMenuItem;
    Step5Item: TMenuItem;
    Step10Item: TMenuItem;
    Step30Item: TMenuItem;
    MinusItem2: TMenuItem;
    PositionItem: TMenuItem;
    PaintItem: TMenuItem;
    RumpfItem: TMenuItem;
    PreviewItem: TMenuItem;
    PrintItem: TMenuItem;
    PlotItem: TMenuItem;
    SpeedBarItem: TMenuItem;
    StatusBarItem: TMenuItem;
    KeepInsideItem: TMenuItem;
    PositionSaveItem: TMenuItem;
    PositionResetItem: TMenuItem;
    MinusItem4: TMenuItem;
    ModusItem: TMenuItem;
    MinusItem3: TMenuItem;
    DrawAlwaysItem: TMenuItem;
    Positionen1: TMenuItem;
    MinusItem5: TMenuItem;
    MinusItem6: TMenuItem;
    IndicatorItem: TMenuItem;
    IndicatorLocalRotItem: TMenuItem;
    OpenBackBmpItem: TMenuItem;
    CloseBackBmpItem: TMenuItem;
    FaktorDlgItem: TMenuItem;
    HullItem: TMenuItem;
    Options3DMenu: TMenuItem;
    PosiToolItem: TMenuItem;
    MatrixItem: TMenuItem;
    SelectHullItem: TMenuItem;
    Sample420Item: TMenuItem;
    SampleDinghyItem: TMenuItem;
    SampleYachtItem: TMenuItem;
    SamplePlaningItem: TMenuItem;
    procedure InitMenu; virtual;
  end;

var
  RotationForm: TRotationForm;

implementation

uses
  SysUtils,
  Vcalc116,
  RggPal,
  FrmScale,
  RggPBox;

{$R *.DFM}

{ TRotationForm }

procedure TRotationForm.PaintBackGround(Image: TBitmap);
var
  R: TRect;
  P: TPoint;
begin
  R := Rect(0, 0, Image.Width, Image.Height);
  with Image.Canvas do
  begin
    Brush.Color := clBtnFace;
    FillRect(R);
    if BackBmp <> nil then
      if (BackBmp.Width < 100) and (BackBmp.Height < 100) then
        P := Point(30,FocusEdit.Top-PaintBox3D.Top)
        {P := Point(30, 5)}
      else
        P := Point(0,0);
      Draw(P.x,P.y,BackBmp);
  end;
end;

procedure TRotationForm.wmGetMinMaxInfo(var Msg: TWMGetMinMaxInfo);
begin
  inherited;
  if csLoading in ComponentState then
    Exit;
  with Msg.MinMaxInfo^ do
  begin
    ptMinTrackSize := Point(MinTrackX, MinTrackY);
    //ptMaxTrackSize := Point(MaxTrackX, MaxTrackY);
  end;
end;

procedure TRotationForm.FormCreate(Sender: TObject);
var
  wx, wy: Integer;
  NewPaintBox: TPaintBox;
begin
  ClientWidth := 800;
  ClientHeight := 480;

  FDrawAlways := True;
  AlwaysShowAngle := False;
  ChangeRotationHints;

  with Panel do
  begin
    BevelOuter := bvNone;
    Caption := '';
  end;

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

  (*
  BackBmp := TBitmap.Create;
  BackBmp.LoadFromFile(
    'E:\Programme\Borland\Delphi 3\Images\Splash\16color\Athena.BMP');
  hPal := BackBmp.Palette;
  *)
  if hPal = 0 then
    hPal := CreateRggPal32;

  Bitmap := TBitmap.Create;
  with Bitmap do
  begin
    Width := wx;
    Height := wy;
    if hPal <> 0 then
      Palette := hPal;
  end;
  PaintBackGround(Bitmap);

  Metafile := TRiggMetaFile.Create;
  with Metafile do
  begin
    Width := Bitmap.Width;
    Height := Bitmap.Height;
  end;

  ActiveControl := FixPunktCombo;

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

  if MainMenu <> nil then
  begin
    { ssAlt ist notwendig, da sonst in der Kommandozeile keine Ziffern
      eingegeben werden können! }
    Step01Item.ShortCut := ShortCut(Word('1'), [ssAlt, ssShift]);
    Step1Item.ShortCut := ShortCut(Word('1'), [ssAlt]);
    Step5Item.ShortCut := ShortCut(Word('5'), [ssAlt]);
    Step10Item.ShortCut := ShortCut(Word('1'), [ssAlt, ssCtrl]);
    Step30Item.ShortCut := ShortCut(Word('3'), [ssAlt]);
    { ZoomInItem.ShortCut := VK_ADD; } { '+' auf Zehnertastatur }
    { ZoomOutItem.ShortCut := VK_SUBTRACT; } { '-' auf Zehnertastatur }
  end;


  { PaintBox austauschen }
  NewPaintBox := TRggPaintBox.Create(PaintBox3D.Owner);
  try
    NewPaintBox.Parent := PaintBox3D.Parent;
    NewPaintBox.Align := PaintBox3D.Align;
    NewPaintBox.OnMouseDown := PaintBox3D.OnMouseDown;
    NewPaintBox.OnMouseMove := PaintBox3D.OnMouseMove;
    NewPaintBox.OnMouseUp := PaintBox3D.OnMouseUp;
    NewPaintBox.OnPaint := PaintBox3D.OnPaint;

    PaintBox3D.Free;
    PaintBox3D := NewPaintBox;
  except
    NewPaintBox.Free;
  end;

  InitGraph;
  InitRaumGrafik;
  InitHullGraph;
  InitRigg;
  InitPreview;

  IndicatorForm := TIndicatorForm.Create(Self);
  IndicatorForm.Rotator := Rotator;
  IndicatorForm.Onchanged := IndicatorChanged;

  ChangePosition(ViewPoint);
end;

procedure TRotationForm.InitGraph; {virtual}
begin
  Rotator := TPolarKar2.Create;
  Rotator.OnCalcAngle := Rotator.GetAngle2;
  InitRotaData;
end;

procedure TRotationForm.InitRaumGrafik; {virtual}
begin
  RaumGrafik := TGetriebeGraph.Create; //TRaumGrafik.Create;
  RaumGrafik.Rotator := Rotator;
  RaumGrafik.Offset := Point(1000,1000);
  RaumGrafik.Zoom := FZoom;
  Raumgrafik.FixName := ComboFixName;
  if RaumGrafik is TGetriebeGraph then
    TGetriebeGraph(RaumGrafik).Ansicht := vp3D;
end;

procedure TRotationForm.InitHullGraph; {virtual}
begin
  HullGraph := THullGraph.Create; //TRggGraph.Create;
  HullGraph.Rotator := Rotator;
  HullGraph.Zoom := FZoom;
  HullGraph.FixPunkt := Raumgrafik.FixPunkt;
end;

procedure TRotationForm.InitRigg; {virtual}
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

procedure TRotationForm.InitPreview;
begin
 { virtual }

  { Preview ruft GetDeviceCaps(Printer.Handle, LOGPIXELSX) auf
    dauert extrem lange, wenn der konfigurierte Standard-Drucker
    nicht erreichbar ist. }

  // Preview := TPreview.Create;
end;

procedure TRotationForm.UpdateGraph;
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

procedure TRotationForm.FormDestroy(Sender: TObject);
begin
  BackBmp.Free;
  Bitmap.Free;
  MetaFile.Free;
  RaumGrafik.Free;
  HullGraph.Free;
  Rotator.Free;
  Preview.Free;
  Preview := nil;
  if hPal <> 0 then
    DeleteObject(hPal);
end;

procedure TRotationForm.InitRotaData;

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

procedure TRotationForm.PaintBox3DPaint(Sender: TObject);
begin
  Draw;
end;

procedure TRotationForm.DrawPreviewBox;
begin
  if Assigned(Preview) then
  begin
    Preview.Faktor := 1/8.5;
    Preview.RPLOffsetX := 250;
    Preview.RPLOffsetY := 15;
    Preview.Draw(Bitmap.Canvas);
  end;
end;

procedure TRotationForm.DrawMatrix(Canvas: TCanvas);
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

procedure TRotationForm.DrawAngleText(Canvas: TCanvas);
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

procedure TRotationForm.Draw;
begin
  if hPal <> 0 then
  begin
    hOldPal := SelectPalette(PaintBox3D.Canvas.Handle, hPal, False);
    RealizePalette(PaintBox3D.Canvas.Handle);
  end;

  Painted := False;

  { Nach Änderung der Auflösung Probleme mit dem Grau-Überschreiben. Deshalb: }
  if Screen.Width <> CreatedScreenWidth then
    ChangeResolution;

  if not PaintBtn.Down or EraseBK then
  begin
    PaintBackGround(Bitmap);
    EraseBK := False;
  end;

  NullpunktOffset.x := -RaumGrafik.Offset.x + Bitmap.Width div 2 + FXpos;
  NullpunktOffset.y := -RaumGrafik.Offset.y + Bitmap.Height div 2 + FYpos;
  DrawToBitmap1;

  if (MainMenu <> nil) and MatrixItem.Checked then
    DrawMatrix(Bitmap.Canvas);

  if (MainMenu <> nil) and PreviewItem.Checked then
    DrawPreviewBox;

  { Bitmap auf den Bildschirm kopieren }
  with PaintBox3D.Canvas do
  begin
    CopyMode := cmSrcCopy;
    Draw(0, 0, Bitmap);
  end;

  Painted := True;
end;

procedure TRotationForm.DrawToBitmap1;
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
    SetMapMode(Canvas.Handle, MM_TEXT);
  end;
end;

procedure TRotationForm.DrawToBitmap2; //Variante 2
begin
  { Metafile anlegen, alten Eintrag grau überschreiben }
  MetaCanvas := TMetaFileCanvas.Create(MetaFile, 0);
  try
    if not PaintBtn.Down then MetaCanvas.Draw(0,0,MetaFile);
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

procedure TRotationForm.PrintIt;
var
  PrintOffset, SavedOffset: TPoint;
  PrintZoom: real;
  RandX, RandY: Integer;
  Rgn: THandle;
begin
  if not Assigned(Preview) then
  begin
    MessageDlg('Preview Object nicht verfügbar.', mtInformation, [mbOK], 0);
    Exit;
  end;

  if not RggPrinter.OKToPrint then
  begin
    MessageDlg('Kein Drucker konfiguriert.', mtInformation, [mbOK], 0);
    Exit;
  end;

  if MessageDlg('Jetzt Drucken?', mtInformation, [mbYes, mbNo], 0) = mrNO then
    Exit;

  PrintZoom := 8.5;
  RandX := 250;
  RandY := 15;

  PrintOffset.x := Round( (Bitmap.Width div 2 + FXpos - RandX) * PrintZoom
                            - Preview.PagePos.Left);
  PrintOffset.y := Round((Bitmap.Height div 2 + FYpos - RandY) * PrintZoom
                            - Preview.PagePos.Top);

  SavedOffset := RaumGrafik.Offset;

  HullGraph.Zoom := FZoom * PrintZoom;
  RaumGrafik.Zoom := FZoom * PrintZoom;
  HullGraph.Offset := PrintOffset;
  RaumGrafik.Offset := PrintOffset;

  Printer.Orientation := Preview.Orientierung;
  Printer.BeginDoc;
  if PreviewItem.Checked then
    Preview.Print;
  Rgn := CreateRectRgnIndirect(Preview.EnvPos);
  { SelectClipRgn() arbeitet mit Kopie von Rgn }
  SelectClipRgn(Printer.Canvas.Handle, Rgn);
  DeleteObject(Rgn);
  RaumGrafik.Coloriert := True;
  RaumGrafik.Draw(Printer.Canvas);
  if FPaintRumpf = True then
  begin
    HullGraph.Coloriert := True;
    HullGraph.FixPunkt := RaumGrafik.FixPunkt;
    HullGraph.Draw(Printer.Canvas);
  end;
  Printer.EndDoc;

  HullGraph.Zoom := FZoom;
  RaumGrafik.Zoom := FZoom;
  HullGraph.Offset := SavedOffset;
  RaumGrafik.Offset := SavedOffset;
end;

procedure TRotationForm.PhiDownItemClick(Sender: TObject);
begin
  if Sender = PhiDownItem then
    LeftBtnClick(LeftBtn)
  else if Sender = PhiUpItem then
    LeftBtnClick(RightBtn)
  else if Sender = ThetaDownItem then
    LeftBtnClick(DownBtn)
  else if Sender = ThetaUpItem then
    LeftBtnClick(UpBtn)
  else if Sender = GammaDownItem then
    LeftBtnClick(GammaDownBtn)
  else if Sender = GammaUpItem then
    LeftBtnClick(GammaUpBtn);
end;

procedure TRotationForm.LeftBtnClick(Sender: TObject);
var
  wp, wt, wg: real;
begin
  if Mode = False then
  begin
    { Incremente }
    wp := 0; wt := 0; wg := 0;
    if Sender = LeftBtn then wp := FIncrementW
    else if Sender = RightBtn then wp := -FIncrementW
    else if Sender = UpBtn then wt := FIncrementW
    else if Sender = DownBtn then wt := - FIncrementW
    else if Sender = GammaUpBtn then wg := FIncrementW
    else if Sender = GammaDownBtn then wg := - FIncrementW;
    Rotate(wp, -wt, -wg, 0, 0, 0);
  end
  else
  begin
    { Absolutwinkel }
    if Sender = LeftBtn then FPhi := FPhi + FIncrementW
    else if Sender = RightBtn then FPhi := FPhi - FIncrementW
    else if Sender = UpBtn then FTheta := FTheta - FIncrementW
    else if Sender = DownBtn then FTheta := FTheta + FIncrementW
    else if Sender = GammaUpBtn then FGamma := FGamma + FIncrementW
    else if Sender = GammaDownBtn then FGamma := FGamma - FIncrementW;
    if FPhi > 180 then FPhi := FPhi - 360
    else if FPhi < -180 then FPhi := FPhi + 360
    else if FTheta > 90 then FTheta := 90
    else if FTheta < -90 then FTheta := -90
    else if FGamma > 180 then FGamma := FGamma - 360
    else if FGamma < -180 then FGamma := FGamma + 360;
    Rotate(FPhi, FTheta, FGamma, 0, 0, 0);
  end;
  Draw;
  IndicatorForm.UpdateIndicator;
end;

procedure TRotationForm.TransLeftItemClick(Sender: TObject);
begin
  if Sender = TransLeftItem then
    TransLeftBtnClick(TransLeftBtn)
  else if Sender = TransRightItem then
    TransLeftBtnClick(TransRightBtn)
  else if Sender = TransUpItem then
    TransLeftBtnClick(TransUpBtn)
  else if Sender = TransDownItem then
    TransLeftBtnClick(TransDownBtn);
end;

procedure TRotationForm.TransLeftBtnClick(Sender: TObject);
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
  if Sender = TransLeftBtn then FXpos := FXpos - FIncrementT
  else if Sender = TransRightBtn then FXpos := FXpos + FIncrementT
  else if Sender = TransUpBtn then FYpos := FYpos - FIncrementT
  else if Sender = TransDownBtn then FYpos := FYpos + FIncrementT;
  if FXpos < xmin then FXpos := xmin
  else if FXpos > xmax then FXpos := xmax
  else if FYpos < ymin then FYpos := ymin
  else if FYpos > ymax then FYpos := ymax;
  if not PaintBtn.Down then EraseBK := True;
  Draw;
end;

procedure TRotationForm.ZoomInBtnClick(Sender: TObject);
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

procedure TRotationForm.ZoomOutBtnClick(Sender: TObject);
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

procedure TRotationForm.FixPunktComboChange(Sender: TObject);
begin
  Raumgrafik.FixName := ComboFixName;
  HullGraph.FixPunkt := RaumGrafik.FixPunkt;
  Draw;
end;

function TRotationForm.ComboFixName: TRiggPoints;
var
  NewFixName: TRiggPoints;
  S: String;
begin
  NewFixName := ooD0;
  S := FixPunktCombo.Text;
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

procedure TRotationForm.Btn1GradClick(Sender: TObject);
begin
  if Sender = Btn01Grad then
  begin
    FIncrementW := 0.1;
    FIncrementT := 1;
  end
  else if Sender = Btn1Grad then
  begin
    FIncrementW := 1;
    FIncrementT := 5;
  end
  else if Sender = Btn5Grad then
  begin
    FIncrementW := 5;
    FIncrementT := 10;
  end
  else if Sender = Btn10Grad then
  begin
   FIncrementW := 10;
   FIncrementT := 30;
  end
  else if Sender = Btn30Grad then
  begin
    FIncrementW := 30;
    FIncrementT := 100;
  end;
end;

procedure TRotationForm.Step01ItemClick(Sender: TObject);
begin
  FIncrementW := 0.1;
  FIncrementT := 1;
  Btn01Grad.Down := True;
end;

procedure TRotationForm.Step1ItemClick(Sender: TObject);
begin
  FIncrementW := 1;
  FIncrementT := 5;
  Btn1Grad.Down := True;
end;

procedure TRotationForm.Step5ItemClick(Sender: TObject);
begin
  FIncrementW := 5;
  FIncrementT := 10;
  Btn5Grad.Down := True;
end;

procedure TRotationForm.Step10ItemClick(Sender: TObject);
begin
  FIncrementW := 10;
  FIncrementT := 30;
  Btn10Grad.Down := True;
end;

procedure TRotationForm.Step30ItemClick(Sender: TObject);
begin
  FIncrementW := 30;
  FIncrementT := 100;
  Btn30Grad.Down := True;
end;

procedure TRotationForm.PrintItemClick(Sender: TObject);
begin
  PrintIt;
end;

procedure TRotationForm.A0_ItemClick(Sender: TObject);
begin
  if Sender = A_Item then FixPunktCombo.ItemIndex := 0
  else if Sender = B_Item then FixPunktCombo.ItemIndex := 2
  else if Sender = C_Item then FixPunktCombo.ItemIndex := 4
  else if Sender = D_Item then FixPunktCombo.ItemIndex := 6
  else if Sender = E_Item then FixPunktCombo.ItemIndex := 8
  else if Sender = F_Item then FixPunktCombo.ItemIndex := 10
  else if Sender = A0_Item then FixPunktCombo.ItemIndex := 1
  else if Sender = B0_Item then FixPunktCombo.ItemIndex := 3
  else if Sender = C0_Item then FixPunktCombo.ItemIndex := 5
  else if Sender = D0_Item then FixPunktCombo.ItemIndex := 7
  else if Sender = E0_Item then FixPunktCombo.ItemIndex := 9
  else if Sender = F0_Item then FixPunktCombo.ItemIndex := 11;
  FixPunktComboChange(Sender);
end;

procedure TRotationForm.RumpfBtnClick(Sender: TObject);
begin
  RumpfItem.Checked := not RumpfItem.Checked;
  RumpfBtn.Down := RumpfItem.Checked;
  FPaintRumpf := RumpfItem.Checked;
  Draw;
end;

procedure TRotationForm.PreviewItemClick(Sender: TObject);
var
  SL: TStrings;
begin
  if not Assigned(Preview) then
  begin
    SL := TStringList.Create;
    SL.Add('Druckfunktion + Vorschau wurden in dieser Version entfernt,');
    SL.Add('weil bei der Abfrage der Druckereigenschaften');
    SL.Add('das System in wenigen Fällen extrem lange braucht,');
    SL.Add('wenn zum Beispiel der konfigurierte Standard-Drucker,');
    SL.Add('angeschlossen an einem anderen Computer im Netzwerk,');
    SL.Add('zur Zeit nicht erreichbar ist.');
    SL.Add('Die Behandlung derartiger neuer Randprobleme sind für diese Anwendung');
    SL.Add('im Rahmen der normalen Wartung nicht vorgesehen.');
    SL.Add('Der blitzschnelle Programmstart unter allen Umständen hat Priorität.');

    MessageDlg(SL.Text, mtInformation, [mbOK], 0);
    SL.Free;
    Exit;
  end;

  PreviewItem.Checked := not PreviewItem.Checked;
  PreviewBtn.Down := PreviewItem.Checked;
  EraseBK := True;
  Draw;
end;

procedure TRotationForm.CloseItemClick(Sender: TObject);
begin
  Close;
end;

procedure TRotationForm.PlotItemClick(Sender: TObject);
var
  List: TStringList;
begin
  if SaveDialog.Execute then
  begin
    List := TStringList.Create;
    try
      RaumGrafik.GetPlotList(List);
      if FPaintRumpf then HullGraph.GetPlotList(List);
      List.SaveToFile(SaveDialog.FileName);
    finally
      List.Free;
    end;
  end;
end;

procedure TRotationForm.GrafikMenuClick(Sender: TObject);
var
  S: string;
begin
  A0_Item.Checked := False;
  B0_Item.Checked := False;
  C0_Item.Checked := False;
  D0_Item.Checked := False;
  E0_Item.Checked := False;
  F0_Item.Checked := False;
  A_Item.Checked := False;
  B_Item.Checked := False;
  C_Item.Checked := False;
  D_Item.Checked := False;
  E_Item.Checked := False;
  F_Item.Checked := False;
  S := FixPunktCombo.Text;
  if S = 'A0' then A0_Item.Checked := True
  else if S = 'B0' then B0_Item.Checked := True
  else if S = 'C0' then C0_Item.Checked := True
  else if S = 'D0' then D0_Item.Checked := True
  else if S = 'E0' then E0_Item.Checked := True
  else if S = 'F0' then F0_Item.Checked := True
  else if S = 'A' then A_Item.Checked := True
  else if S = 'B' then B_Item.Checked := True
  else if S = 'C' then C_Item.Checked := True
  else if S = 'D' then D_Item.Checked := True
  else if S = 'E' then E_Item.Checked := True
  else if S = 'F' then F_Item.Checked := True;

  Step01Item.Checked := False;
  Step1Item.Checked := False;
  Step5Item.Checked := False;
  Step10Item.Checked := False;
  Step30Item.Checked := False;
  if Btn01Grad.Down then Step01Item.Checked := True
  else if Btn1Grad.Down then Step1Item.Checked := True
  else if Btn5Grad.Down then Step5Item.Checked := True
  else if Btn10Grad.Down then Step10Item.Checked := True
  else if Btn30Grad.Down then Step30Item.Checked := True;

  ModusItem.Checked := Mode;
  DrawAlwaysItem.Checked := FDrawAlways;
end;

procedure TRotationForm.Options3DMenuClick(Sender: TObject);
begin
  IndicatorItem.Checked := IndicatorForm.Visible;
  IndicatorLocalRotItem.Checked := not IndicatorForm.GlobalRot;
end;

procedure TRotationForm.StatusBarItemClick(Sender: TObject);
begin
  StatusBarItem.Checked := not StatusBarItem.Checked;
  if StatusBarItem.Checked then
    StatusBar.Visible := True
  else
    StatusBar.Visible := False;
end;

procedure TRotationForm.SpeedBarItemClick(Sender: TObject);
begin
  SpeedBarItem.Checked := not SpeedBarItem.Checked;
  if SpeedBarItem.Checked then
    ToolbarPanel.Visible := True
  else
    ToolbarPanel.Visible := False;
end;

procedure TRotationForm.PaintBtnClick(Sender: TObject);
begin
  PaintItem.Checked := not PaintItem.Checked;
  PaintBtn.Down := PaintItem.Checked;
  if not PaintBtn.Down then
    EraseBK := True;
  Draw;
end;

procedure TRotationForm.NullBtnClick(Sender: TObject);
begin
  if ViewPoint = vp3D then
    ViewPoint := vpSeite
  else
    inc(ViewPoint);
  case ViewPoint of
    vpSeite: Pos1Btn.Down := True;
    vpAchtern: Pos2Btn.Down := True;
    vpTop: Pos3Btn.Down := True;
    vp3D: Pos4Btn.Down := True;
  end;
  ChangePosition(ViewPoint);
end;

procedure TRotationForm.ChangePosition(aViewPoint: TViewPoint);
begin
  ViewPoint := aViewPoint;
  case ViewPoint of
    vpSeite: RotaData := RotaData1;
    vpAchtern: RotaData := RotaData2;
    vpTop: RotaData := RotaData3;
    vp3D: RotaData := RotaData4;
  end;
  FocusEdit.Text := IntToStr(Ord(ViewPoint)+1);

  FXpos := RotaData.Xpos;
  FYpos := RotaData.Ypos;
  { Increment }
  case RotaData.IncrementIndex of
    1: Btn01Grad.Down := true;
    2: Btn1Grad.Down := true;
    3: Btn5Grad.Down := true;
    4: Btn10Grad.Down := true;
    5: Btn30Grad.Down := true;
  end;
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
  FixPunktCombo.ItemIndex := RotaData.FixpunktIndex;
  RaumGrafik.FixName := ComboFixName;
  RaumGrafik.Update; // Rotate;
  HullGraph.FixPunkt := Raumgrafik.FixPunkt;
  { Neuzeichnen }
  EraseBK := True; Draw;
  IndicatorForm.UpdateIndicator;
end;

procedure TRotationForm.PositionSaveItemClick(Sender: TObject);
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
    FixPunktIndex := FixPunktCombo.ItemIndex;
    if Btn01Grad.Down = true then IncrementIndex := 1
    else if Btn1Grad.Down = true then IncrementIndex := 2
    else if Btn5Grad.Down = true then IncrementIndex := 3
    else if Btn10Grad.Down = true then IncrementIndex := 4
    else if Btn30Grad.Down = true then IncrementIndex := 5;
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

procedure TRotationForm.PositionResetItemClick(Sender: TObject);
begin
  InitRotaData;
  ChangePosition(ViewPoint);
end;

procedure TRotationForm.Pos1BtnClick(Sender: TObject);
var
  temp: TViewPoint;
begin
  if Sender = Pos1Btn then temp := vpSeite
  else if Sender = Pos2Btn then temp := vpAchtern
  else if Sender = Pos3Btn then temp := vpTop
  else if Sender = Pos4Btn then temp := vp3D
  else temp := vp3D;

  ChangePosition(temp);
end;

procedure TRotationForm.ChangeResolution;
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
    if hPal <> 0 then
      Palette := hPal;
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

procedure TRotationForm.KeepInsideItemClick(Sender: TObject);
begin
  KeepInsideItemChecked := not KeepInsideItemChecked;
  KeepInsideItem.Checked := KeepInsideItemChecked;
  if KeepInsideItem.Checked then
    Draw;
end;

{ Scroll left by one button. If no buttons are visible anymore, do nothing. }
procedure TRotationForm.LeftButtonClick(Sender: TObject);
var
  Button: TControl;
begin
  { Scroll left }
  Button := GetFirstVisibleButton;
  if Button <> nil then
  begin
    if Buttons[Button.Tag+1] <> nil then
      Panel.Left := -Buttons[Button.Tag+1].Left
    else
      Panel.Left := -(Button.Left + Button.Width);
    Panel.Width := LeftButton.Left - Panel.Left;
    RedoButtons;
  end;
end;

{ Scroll right by one button. Do not scroll past the first button.
  If no buttons are visible, then do nothing. }
procedure TRotationForm.RightButtonClick(Sender: TObject);
var
  Button: TControl;
begin
  { Scroll right }
  Button := GetFirstVisibleButton;
  if (Button <> nil) and (Button.Tag > 0) then
  begin
    with Buttons[Button.Tag-1] do
      Panel.Left := -Left;
    Panel.Width := LeftButton.Left - Panel.Left;
    RedoButtons;
  end;
end;

{ After a resize, or after a scroll, determine which buttons are visible. }
procedure TRotationForm.RedoButtons;
var
  I: Integer;
begin
  { make a partially obscured button completely invisible }
  with Panel do
    for I := 0 to ControlCount-1 do
      if Controls[I].Tag >= 0 then
        Controls[I].Visible := IsButtonVisible(Controls[I]);
  EnableScrollButtons;
end;

{ Return the button whose tag property is Tag, or nil if no button matches. }
function TRotationForm.GetButton(Tag: Integer): TControl;
var
  I: Integer;
begin
  for I := 0 to Panel.ControlCount-1 do
    if Panel.Controls[I].Tag = Tag then
    begin
      Result := Panel.Controls[I] as TControl;
      Exit;
    end;
  Result := nil;
end;

{ Return the number of toolbar buttons on the panel. }
function TRotationForm.GetButtonCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Panel.ControlCount-1 do
    if Panel.Controls[I].Tag >= 0 then
      Inc(Result);
end;

{ Return whether a button is entirely visible. Scale the button's
  bounds to the toolbar, and make sure the button lies entirely
  within the bounds of the toolbar. }
function TRotationForm.IsButtonVisible(Button: TControl): Boolean;
var
  TopLeft, BottomRight: TPoint;
  Right: Integer;
begin
  TopLeft := ScreenToClient(Button.ClientToScreen(Button.ClientRect.TopLeft));
  BottomRight := ScreenToClient(Button.ClientToScreen(Button.ClientRect.BottomRight));
  if LeftButton.Visible then
    Right := LeftButton.Left
  else
    Right := ClientWidth;
  if (TopLeft.X < 0) or (TopLeft.Y < 0) then
    Result := False
  else if (BottomRight.Y > ClientHeight) or (BottomRight.X > Right) then
    Result := False
  else
    Result := True;
end;

{ Return the first visible button, or nil if they are all invisible. }
function TRotationForm.GetFirstVisibleButton: TControl;
var
  I: Integer;
begin
  with Panel do
    for I := 0 to ControlCount-1 do
      if Controls[I].Tag >= 0 then
      begin
        Result := Controls[I] as TControl;
        if IsButtonVisible(Result) then
          Exit;
      end;
  Result := nil;
end;

{ Return the last (rightmost) visible button, or nil if all the
  buttons are invisible. }
function TRotationForm.GetLastVisibleButton: TControl;
var
  I: Integer;
begin
  with Panel do
    for I := ControlCount-1 downto 0 do
      if Controls[I].Tag >= 0 then
      begin
        Result := Controls[I] as TControl;
        if IsButtonVisible(Result) then
          Exit;
      end;
  Result := nil;
end;

{ Enable or disable the toolbar scroll buttons. If the leftmost
  button is visible then disable the right button. If the rightmost
  button is visible then disable the left button. }
procedure TRotationForm.EnableScrollButtons;
begin
  RightButton.Enabled := not IsButtonVisible(Buttons[0]);
  LeftButton.Enabled  := not IsButtonVisible(Buttons[GetButtonCount-1]);
end;

{ When the toolbar changes size, move the scroll buttons so they stay
  at the right edge of the toolbar. Reset the panel to show whichever
  buttons are visible. }
procedure TRotationForm.ToolbarPanelResize(Sender: TObject);
var
  I, Right: Integer;
begin
  LeftButton.Left := ClientWidth - LeftButton.Width - 4;
  RightButton.Left := ClientWidth - RightButton.Width - 4;
  { Is there enough room for every button? }
  with Buttons[GetButtonCount-1] do
    Right := Left + Width;
  if ClientWidth >= Right then
  begin
    Panel.Left := 0;    { unscroll }
    Panel.Width := ClientWidth;
    { Hide the scroll buttons. }
    LeftButton.Visible := False;
    RightButton.Visible := False;
    { Make all the toolbar buttons visible. }
    with Panel do
      for I := 0 to ControlCount-1 do
        if Controls[I].Tag >= 0 then
          Controls[I].Visible := True
  end
  else
  begin
    { Make the scroll buttons visible. }
    LeftButton.Visible := True;
    RightButton.Visible := True;
    { Set the panel width to leave room for the scroll buttons. }
    Panel.Width := LeftButton.Left - Panel.Left;
  end;
  Panel.Height := ToolbarPanel.Height;
  RedoButtons;
end;

{ Ergänzung für das Drehen mit der Maus. }

procedure TRotationForm.PaintBox3DMouseDown(Sender: TObject;
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

procedure TRotationForm.PaintBox3DMouseMove(Sender: TObject;
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

procedure TRotationForm.PaintBox3DMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  MouseDown := False;
  if (prevx = MouseDownX) and (prevy = MouseDownY) then
    EraseBK := True;
  Draw;
  IndicatorForm.UpdateIndicator;
end;

procedure TRotationForm.Rotate(Phi, Theta, Gamma, xrot, yrot, zrot: real);
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

procedure TRotationForm.Translate(x, y: Integer);
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
  if not PaintBtn.Down then
    EraseBK := True;
  Draw;
end;

procedure TRotationForm.FocusEditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (ssCtrl in Shift) then
  begin
    if Key = VK_Left then
    begin
      TransLeftBtnClick(TransLeftBtn);
      Key := 0;
      end
    else if Key = VK_Right then
    begin
      TransLeftBtnClick(TransRightBtn);
      Key := 0;
    end
    else if Key = VK_Up then
    begin
      TransLeftBtnClick(TransUpBtn);
      Key := 0;
    end
    else if Key = VK_Down then
    begin
      TransLeftBtnClick(TransDownBtn);
      Key := 0;
    end;
  end

  else if (ssShift in Shift) then
  begin
    if Key = VK_Left then
    begin
      LeftBtnClick(GammaDownBtn);
      Key := 0;
    end
    else if Key = VK_Right then
    begin
      LeftBtnClick(GammaUpBtn);
      Key := 0;
    end
    else if (Key = VK_Up) then
    begin
      {^LeftBtnClick(LeftBtn); }
      Key := 0;
    end
    else if (Key = VK_Down) then
    begin
      { LeftBtnClick(RightBtn); }
      Key := 0;
    end;
  end

  else begin
    if Key = VK_Left then
    begin
      LeftBtnClick(LeftBtn);
      Key := 0;
    end
    else if Key = VK_Right then
    begin
      LeftBtnClick(RightBtn);
      Key := 0;
    end
    else if Key = VK_Up then
    begin
      LeftBtnClick(UpBtn);
      Key := 0;
    end
    else if Key = VK_Down then
    begin
      LeftBtnClick(DownBtn);
      Key := 0;
    end
    else if Key = VK_Add then
    begin
      ZoomInBtnClick(Sender);
      Key := 0;
    end
    else if Key = VK_Subtract then
    begin
      ZoomOutBtnClick(Sender);
      Key := 0;
    end
    else if Key = $20 then
    begin
      EraseBK := True;
      Draw;
    end;
  end;
end;

procedure TRotationForm.SetZoomText;
begin
  StatusBar.Panels.Items[7].Text :=
    FormatFloat('0.0#',LookUpRa10(FZoomIndex));
end;

procedure TRotationForm.SetAngleText;
begin
  { falls Increment Mode }
  if (Mode = False) and (AlwaysShowAngle = False) then
    Exit;
  if (Mode = False) and (AlwaysShowAngle = True) then
    Rotator.GetAngle(FPhi, FTheta, FGamma);

  SHeadingPhi := Format('Heading: %5.1f',[FPhi]);
  SPitchTheta := Format('Pitch:   %5.1f',[FTheta]);
  SBankGamma  := Format('Bank:    %5.1f',[FGamma]);
  with StatusBar.Panels do
  begin
    BeginUpdate;
    try
      Items[1].Text := Format('%5.1f',[FPhi]);
      Items[3].Text := Format('%5.1f',[FTheta]);
      Items[5].Text := Format('%5.1f',[FGamma]);
    finally
      EndUpdate;
    end;
  end;
end;

procedure TRotationForm.ModusItemClick(Sender: TObject);
begin
  Mode := not Mode;
  ChangeRotationHints;
  Rotator.Mode := Mode;

  if AlwaysShowAngle then
    Exit;

  if Mode = False then
  begin
    { Increment - Modus }
    SHeadingPhi := '';
    SPitchTheta := '';
    SBankGamma  := '';
    with StatusBar.Panels do
    begin
      BeginUpdate;
      try
        Items[1].Text := '';
        Items[3].Text := '';
        Items[5].Text := '';
      finally
        EndUpdate;
      end;
    end;
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

procedure TRotationForm.DrawAlwaysItemClick(Sender: TObject);
begin
  FDrawAlways := not FDrawAlways;
end;

procedure TRotationForm.HullItemClick(Sender: TObject);
begin
  if OpenDialog.Execute then
  begin
    with HullGraph as THullGraph do
    begin
      VertexFileName := OpenDialog.FileName;
      VertexMemo := nil;
      Load;
    end;
    Draw;
  end;
end;

procedure TRotationForm.Sample420ItemClick(Sender: TObject);
var
 Memo: TStrings;
begin
  Memo := nil;
  if Sender = Sample420Item then
    Memo := Sample420Memo;
  if Sender = SampleDinghyItem then
    Memo := SampleDinghyMemo;
  if Sender = SampleYachtItem then
    Memo := SampleYachtMemo;
  if Sender = SamplePlaningItem then
    Memo := SamplePlaningMemo;

  if Assigned(Memo) then
  begin
    with HullGraph as THullGraph do
    begin
      VertexFileName := '';
      VertexMemo := Memo;
      Load;
    end;
    Draw;
  end;
end;

procedure TRotationForm.FaktorDlgItemClick(Sender: TObject);
begin
  RumpfFaktorDlg := TRumpfFaktorDlg.Create(Self);
  try
    with HullGraph as THullGraph do
    begin
      RumpfFaktorDlg.Caption := 'Rumpf skalieren';
      RumpfFaktorDlg.GroupBox.Caption := 'Faktor in %';
      RumpfFaktorDlg.L := Round(Factor.x * 100);
      RumpfFaktorDlg.B := Round(Factor.y * 100);
      RumpfFaktorDlg.H := Round(Factor.z * 100);
      ShowModal;
      if ModalResult = mrOK then
      begin
        Factor.x := RumpfFaktorDlg.L / 100;
        Factor.y := RumpfFaktorDlg.B / 100;
        Factor.z := RumpfFaktorDlg.H / 100;
        Load;
      end;
    end;
    Draw;
  finally
    RumpfFaktorDlg.Free;
  end;
end;

procedure TRotationForm.OpenBackBmpItemClick(Sender: TObject);
var
  wx, wy: Integer;
begin
  if not OpenPictureDialog.Execute then
    Exit;
  if OpenPictureDialog.Filename = '' then
    Exit;

  SelectPalette(PaintBox3D.Canvas.Handle, GetStockObject(Default_Palette), False);
  RealizePalette(PaintBox3D.Canvas.Handle);

  if hPal <> 0 then
  begin
    DeleteObject(hPal);
    hPal := 0;
  end;

  wx := Bitmap.Width;
  wy := Bitmap.Height;
  Bitmap.Free;
  Bitmap := nil;
  BackBmp.Free;
  BackBmp := nil;

  BackBmp := TBitmap.Create;
  try
    BackBmp.LoadFromFile(OpenPictureDialog.Filename);
  except
    begin
      BackBmp.Free;
      BackBmp := nil;
    end;
  end;

  if BackBmp <> nil then
    hPal := BackBmp.Palette
  else
    hPal := CreateRggPal32;

  Bitmap := TBitmap.Create;
  with Bitmap do
  begin
    Width := wx;
    Height := wy;
    Palette := hPal;
  end;

  EraseBK := True; Draw;
end;

procedure TRotationForm.CloseBackBmpItemClick(Sender: TObject);
var
  wx, wy: Integer;
begin
  SelectPalette(PaintBox3D.Canvas.Handle, GetStockObject(Default_Palette), False);
  RealizePalette(PaintBox3D.Canvas.Handle);

  if hPal <> 0 then
  begin
    DeleteObject(hPal);
    hPal := 0;
  end;

  wx := Bitmap.Width;
  wy := Bitmap.Height;
  Bitmap.Free;
  Bitmap := nil;
  BackBmp.Free;
  BackBmp := nil;

  hPal := CreateRggPal32;

  Bitmap := TBitmap.Create;
  with Bitmap do
  begin
    Width := wx;
    Height := wy;
    Palette := hPal;
  end;

  EraseBK := True; Draw;
end;

procedure TRotationForm.IndicatorItemClick(Sender: TObject);
begin
  IndicatorItem.Checked := not IndicatorItem.Checked;
  if IndicatorItem.Checked then
  begin
    IndicatorForm.Show;
    IndicatorForm.UpdateIndicator;
  end
  else
    IndicatorForm.Hide;
end;

procedure TRotationForm.IndicatorLocalRotItemClick(Sender: TObject);
begin
  with IndicatorForm do
    GlobalRot := not GlobalRot;
end;

procedure TRotationForm.IndicatorChanged(Sender: TObject);
begin
  RaumGrafik.Update;
  Rotator.GetAngle(FPhi, FTheta, FGamma);
  SetAngleText;
  Draw;
end;

procedure TRotationForm.PosiToolItemClick(Sender: TObject);
begin
  PosiToolItem.Checked := not PosiToolItem.Checked;
  if PosiToolItem.Checked then pnPositionTools.Visible := True
  else pnPositionTools.Visible := False;
end;

procedure TRotationForm.ChangeRotationHints;
begin
  if Mode then
  begin
    { Absolute Winkel }
    LeftBtn.Hint := 'Heading - Boot links drehen (phi+)|';
    RightBtn.Hint :='Heading - Boot rechts drehen (phi-)|';
    UpBtn.Hint := 'Pitch - Bug nach oben kippen (theta-)|';
    DownBtn.Hint := 'Pitch - Bug nach unten kippen (theta+)|';
    GammaDownBtn.Hint := 'Bank - Boot nach links (gamma-)|';
    GammaUpBtn.Hint := 'Bank - Boot nach rechts (gamma+)|';

    if MainMenu <> nil then
    begin
      PhiDownItem.Hint := '  Grafik um die globale Z-Achse drehen';
      PhiUpItem.Hint := '  Grafik um die globale Z-Achse drehen';
      ThetaDownItem.Hint := '  Grafik um die lokale Y-Achse drehen';
      ThetaUpItem.Hint := '  Grafik um die lokale Y-Achse drehen';
      GammaDownItem.Hint := '  Grafik um die lokale x-Achse drehen';
      GammaUpItem.Hint := '  Grafik um die lokale x-Achse drehen';

      PhiDownItem.Caption := 'H  ( Pfeiltaste ''nach links'' )';
      PhiUpItem.Caption := 'H  ( Pfeiltaste ''nach rechts'' )';
      ThetaDownItem.Caption := 'P  ( Pfeiltaste ''nach unten'' )';
      ThetaUpItem.Caption := 'P  ( Pfeiltaste ''nach oben'' )';
      GammaDownItem.Caption := 'B  ( Umsch + Pfeiltaste ''nach links'' )';
      GammaUpItem.Caption := 'B  ( Umsch + Pfeiltaste ''nach rechts'' )';
    end;
  end;
  if not Mode then
  begin
    { um Winkelinkremente drehen }
    LeftBtn.Hint := 'Boot nach links kippen|';
    RightBtn.Hint :='Boot nach rechts kippen|';
    UpBtn.Hint := 'Bug nach oben kippen|';
    DownBtn.Hint := 'Bug nach unten kippen|';
    GammaDownBtn.Hint := 'Boot nach links drehen|';
    GammaUpBtn.Hint := 'Boot nach rechts drehen|';

    if MainMenu <> nil then
    begin
      PhiDownItem.Hint := '  Grafik um die lokale X-Achse drehen';
      PhiUpItem.Hint := '  Grafik um die lokale X-Achse drehen';
      ThetaDownItem.Hint := '  Grafik um die lokale Y-Achse drehen';
      ThetaUpItem.Hint := '  Grafik um die lokale Y-Achse drehen';
      GammaDownItem.Hint := '  Grafik um die lokale Z-Achse drehen';
      GammaUpItem.Hint := '  Grafik um die lokale Z-Achse drehen';

      PhiDownItem.Caption := 'X  ( Pfeiltaste ''nach links'' )';
      PhiUpItem.Caption := 'X  ( Pfeiltaste ''nach rechts'' )';
      ThetaDownItem.Caption := 'Y  ( Pfeiltaste ''nach unten'' )';
      ThetaUpItem.Caption := 'Y  ( Pfeiltaste ''nach oben'' )';
      GammaDownItem.Caption := 'Z  ( Umsch + Pfeiltaste ''nach links'' )';
      GammaUpItem.Caption := 'Z  ( Umsch + Pfeiltaste ''nach rechts'' )';
    end;
  end;
end;

procedure TRotationForm.MatrixItemClick(Sender: TObject);
begin
  MatrixItem.Checked := not MatrixItem.Checked;
  Draw;
end;

procedure TRotationForm.AngleTextItemClick(Sender: TObject);
begin
  // AngleTextItem.Checked := not AngleTextItem.Checked;
  Draw;
end;

procedure TRotationForm.InitMenu;
var
  p, q: TMenuItem;
  mi: TMenuItem;

  function AddP(AName: string): TMenuItem;
  begin
    mi := TMenuItem.Create(MainMenu);
    mi.Name := AName;
    p := mi;
    MainMenu.Items.Add(p);
    result := mi;
  end;

  function AddI(AName: string): TMenuItem;
  begin
    mi := TMenuItem.Create(MainMenu);
    mi.Name := AName;
    p.Add(mi);
    result := mi;
    q := mi;
  end;

  function AddJ(AName: string): TMenuItem;
  begin
    mi := TMenuItem.Create(MainMenu);
    mi.Name := AName;
    q.Add(mi);
    result := mi;
  end;

begin
  MainMenu := TMainMenu.Create(Self);

  GrafikMenu := AddP('GrafikMenu');
  mi.Caption := '&3D Grafik';
  mi.GroupIndex := 8;
  mi.Hint := '  Einstellungen für 3D Grafik';
  mi.OnClick := GrafikMenuClick;

  ZoomItem := AddI('ZoomItem');
  mi.Caption := 'Skalieren';
  mi.Hint := '  Grafik skalieren';

  ZoomInItem := AddJ('ZoomInItem');
  mi.Caption := 'Zoom In';
  mi.Hint := '  Grafik vergrößern';
  mi.ShortCut := 16457;
  mi.OnClick := ZoomInBtnClick;

  ZoomOutItem := AddJ('ZoomOutItem');
  mi.Caption := 'Zoom Out';
  mi.Hint := '  Grafik verkleinern';
  mi.ShortCut := 16463;
  mi.OnClick := ZoomOutBtnClick;

  DrehenItem := AddI('DrehenItem');
  mi.Caption := 'Rotieren';
  mi.Hint := '  Grafik drehen';

  PhiDownItem := AddJ('PhiDownItem');
  mi.Caption := 'Phi ( Pfeiltaste '#39'nach links'#39' )';
  mi.OnClick := PhiDownItemClick;

  PhiUpItem := AddJ('PhiUpItem');
  mi.Caption := 'Phi ( Pfeiltaste '#39'nach rechts'#39' )';
  mi.OnClick := PhiDownItemClick;

  ThetaDownItem := AddJ('');
  mi.Caption := 'Theta ( Pfeiltaste '#39'nach unten'#39' )';
  mi.OnClick := PhiDownItemClick;

  ThetaUpItem := AddJ('');
  mi.Caption := 'Theta ( Pfeiltaste '#39'nach oben'#39' )';
  mi.OnClick := PhiDownItemClick;

  GammaDownItem := AddJ('');
  mi.Caption := 'Gamma ( Umsch + Pfeiltaste '#39'nach links'#39' )';
  mi.OnClick := PhiDownItemClick;

  GammaUpItem := AddJ('');
  mi.Caption := 'Gamma ( Umsch + Pfeiltaste '#39'nach rechts'#39' )';
  mi.OnClick := PhiDownItemClick;

  VerschiebenItem := AddI('');
  mi.Caption := 'Verschieben';
  mi.Hint := '  Grafik verschieben';

  TransLeftItem := AddJ('');
  mi.Caption := 'nach links ( Ctrl + Pfeiltaste '#39'nach links'#39' )';
  mi.Hint := '  Grafik nach links verschieben';
  mi.OnClick := TransLeftItemClick;

  TransRightItem := AddJ('');
  mi.Caption := 'nach rechts ( Ctrl + Pfeiltaste '#39'nach rechts'#39' )';
  mi.Hint := '  Grafik nach rechts verschieben';
  mi.OnClick := TransLeftItemClick;

  TransUpItem := AddJ('');
  mi.Caption := 'nach oben ( Ctrl + Pfeiltaste '#39'nach oben'#39' )';
  mi.Hint := '  Grafik nach oben verschieben';
  mi.OnClick := TransLeftItemClick;

  TransDownItem := AddJ('');
  mi.Caption := 'nach unten ( Ctrl + Pfeiltaste '#39'nach unten'#39' )';
  mi.Hint := '  Grafik nach unten verschieben';
  mi.OnClick := TransLeftItemClick;

  MinusItem1 := AddI('MinusItem1');
  mi.Caption := '-';

  DrehpunktItem := AddI('');
  mi.Caption := 'Drehpunkt';
  mi.Hint := '  Den festgehaltenen Punkt der Grafik bestimmen';

  A0_Item := AddJ('');
  mi.Caption := 'A0';
  mi.Hint := '  P'#252'tting Stb als Fixpunkt festlegen';
  mi.OnClick := A0_ItemClick;

  B0_Item := AddJ('');
  mi.Caption := 'B0';
  mi.Hint := '  Pütting Bb als Fixpunkt festlegen';
  mi.OnClick := A0_ItemClick;

  C0_Item := AddJ('');
  mi.Caption := 'C0';
  mi.Hint := '  Vorstagbasis als Fixpunkt festlegen';
  mi.OnClick := A0_ItemClick;

  D0_Item := AddJ('');
  mi.Caption := 'D0';
  mi.Hint := '  Mastfuß als Fixpunkt festlegen';
  mi.OnClick := A0_ItemClick;

  E0_Item := AddJ('');
  mi.Caption := 'E0';
  mi.Hint := '  Controllerbasis als Fixpunkt festlegen';
  mi.OnClick := A0_ItemClick;

  F0_Item := AddJ('');
  mi.Caption := 'F0';
  mi.Hint := '  Me'#223'punkt Spiegel als Fixpunkt festlegen';
  mi.OnClick := A0_ItemClick;

  MinusItem8 := AddJ('');
  mi.Caption := '-';

  A_Item := AddJ('');
  mi.Caption := 'A';
  mi.Hint := '  Salingnocke Stb als Fixpunkt festlegen';
  mi.OnClick := A0_ItemClick;

  B_Item := AddJ('');
  mi.Caption := 'B';
  mi.Hint := '  Salingnocke Bb als Fixpunkt festlegen';
  mi.OnClick := A0_ItemClick;

  C_Item := AddJ('');
  mi.Caption := 'C';
  mi.Hint := '  Vorstagpunkt (Mast) als Fixpunkt festlegen';
  mi.OnClick := A0_ItemClick;

  D_Item := AddJ('');
  mi.Caption := 'D';
  mi.Hint := '  Salingpunkt (Mast) als Fixpunkt festlegen';
  mi.OnClick := A0_ItemClick;

  E_Item := AddJ('');
  mi.Caption := 'E';
  mi.Hint := '  Controllerpunkt (Mast) als Fixpunkt festlegen';
  mi.OnClick := A0_ItemClick;

  F_Item := AddJ('');
  mi.Caption := 'F';
  mi.Hint := '  Masttop als Fixpunkt festlegen';
  mi.OnClick := A0_ItemClick;

  StepItem := AddI('');
  mi.Caption := 'Schrittweite';
  mi.Hint := ' Schrittweite für die Drehung (und Verschiebung) festlegen';

  Step01Item := AddJ('');
  mi.Caption := '0.1 Grad';
  mi.Hint := '  Winkel-Schrittweite von 0,1 Grad einstellen';
  mi.OnClick := Step01ItemClick;

  Step1Item := AddJ('');
  mi.Caption := '   1 Grad';
  mi.Hint := '  Winkel-Schrittweite von 1 Grad einstellen';
  mi.OnClick := Step1ItemClick;

  Step5Item := AddJ('');
  mi.Caption := '   5 Grad';
  mi.Hint := '  Winkel-Schrittweite von 5 Grad einstellen';
  mi.OnClick := Step5ItemClick;

  Step10Item := AddJ('');
  mi.Caption := ' 10 Grad';
  mi.Hint := '  Winkel-Schrittweite von 10 Grad einstellen';
  mi.OnClick := Step10ItemClick;

  Step30Item := AddJ('');
  mi.Caption := ' 30 Grad';
  mi.Hint := '  Winkel-Schrittweite von 30 Grad einstellen';
  mi.OnClick := Step30ItemClick;

  Positionen1 := AddI('');
  mi.Caption := 'Positionen';
  mi.Hint := '  Ansichtspositionen';

  PositionItem := AddJ('');
  mi.Caption := 'Position wechseln';
  mi.Hint := '  Position des Betrachters umschalten';
  mi.OnClick := NullBtnClick;

  PositionSaveItem := AddJ('');
  mi.Caption := 'Position speichern';
  mi.Hint := '  aktuelle Position übernehmen';
  mi.OnClick := PositionSaveItemClick;

  PositionResetItem := AddJ('');
  mi.Caption := 'Positionen zurücksetzen';
  mi.Hint := '  alle Positionen auf Standardwerte setzen';
  mi.OnClick := PositionResetItemClick;

  MinusItem2 := AddI('');
  mi.Caption := '-';

  ModusItem := AddI('');
  mi.Caption := 'Absolutwinkel';
  mi.Hint := '  Absolutwerte für Drehwinkel oder Inkremente';
  mi.OnClick := ModusItemClick;

  KeepInsideItem := AddI('');
  mi.Caption := 'Drehpunkt sichtbar';
  mi.Checked := True;
  mi.Hint := '  Drehpunkt immer innerhalb der Bildgrenzen halten';
  mi.OnClick := KeepInsideItemClick;

  PaintItem := AddI('');
  mi.Caption := 'Alte Grafik stehenlassen';
  mi.Hint := '  Alte Grafik stehenlassen oder löschen';
  mi.ShortCut := 45;
  mi.OnClick := PaintBtnClick;

  RumpfItem := AddI('');
  mi.Caption := 'Boot einblenden';
  mi.Hint := '  Bootsrumpf einblenden';
  mi.ShortCut := 16450;
  mi.OnClick := RumpfBtnClick;

  DrawAlwaysItem := AddI('');
  mi.Caption := 'Boot immer zeichnen';
  mi.Hint := '  Boot auch w'#228'hrend der Bewegung zeichnen';
  mi.OnClick := DrawAlwaysItemClick;

  MinusItem3 := AddI('');
  mi.Caption := '-';

  PreviewItem := AddI('');
  mi.Caption := 'Seite einblenden';
  mi.Hint := '  Seitenr'#228'nder einblenden';
  mi.OnClick := PreviewItemClick;

  PrintItem := AddI('');
  mi.Caption := 'Drucken ...';
  mi.Hint := '  Gafik drucken';
  mi.OnClick := PrintItemClick;

  PlotItem := AddI('');
  mi.Caption := 'Plotfile ...';
  mi.Hint := '  Grafik im HPGL Format ausgeben';
  mi.OnClick := PlotItemClick;

  MinusItem4 := AddI('');
  mi.Caption := '-';

  SpeedBarItem := AddI('');
  mi.Caption := 'Symbolleiste';
  mi.Checked := True;
  mi.Hint := '  Symbolleiste einblenden (3D Grafik)';
  mi.OnClick := SpeedBarItemClick;

  PosiToolItem := AddI('');
  mi.Caption := 'Positionsschalter';
  mi.Checked := True;
  mi.Hint := '  Auswahlschalter für Positionen einblenden';
  mi.OnClick := PosiToolItemClick;

  StatusBarItem := AddI('');
  mi.Caption := 'Statusleiste';
  mi.Checked := True;
  mi.Hint := '  Statusleiste einblenden (3D Grafik)';
  mi.OnClick := StatusBarItemClick;

  Options3DMenu := AddP('');
  mi.Caption := '3D &Optionen';
  mi.GroupIndex := 8;
  mi.Hint := '  Optionen für 3D Grafik';
  mi.OnClick := Options3DMenuClick;

  SelectHullItem := AddI('');
  mi.Caption := 'Rumpf auswählen';

  Sample420Item := AddJ('');
  mi.Caption := 'Beispiel 420er Jolle';
  mi.OnClick := Sample420ItemClick;

  SampleDinghyItem := AddJ('');
  mi.Caption := 'Beispiel Dinglhy';
  mi.OnClick := Sample420ItemClick;

  SampleYachtItem := AddJ('');
  mi.Caption := 'Beispiel Yacht';
  mi.OnClick := Sample420ItemClick;

  SamplePlaningItem := AddJ('');
  mi.Caption := 'Beispiel Planing';
  mi.OnClick := Sample420ItemClick;

  HullItem := AddJ('');
  mi.Caption := 'Rumpf laden...';
  mi.Hint := '  Rumpfdaten aus Datei laden';
  mi.OnClick := HullItemClick;

  FaktorDlgItem := AddI('');
  mi.Caption := 'Rumpf skalieren...';
  mi.Hint := '  Rumpfgr'#246#223'e anpassen';
  mi.OnClick := FaktorDlgItemClick;

  MinusItem5 := AddI('');
  mi.Caption := '-';

  OpenBackBmpItem := AddI('');
  mi.Caption := 'Hintergrund laden...';
  mi.Hint := '  Bitmapdatei als Hintergrundbild laden';
  mi.OnClick := OpenBackBmpItemClick;

  CloseBackBmpItem := AddI('');
  mi.Caption := 'Hintergrund löschen';
  mi.Hint := '  Hintergrundbitmap löschen';
  mi.OnClick := CloseBackBmpItemClick;

  MinusItem6 := AddI('');
  mi.Caption := '-';

  IndicatorItem := AddI('');
  mi.Caption := 'Indikator';
  mi.Hint := '  Indikator für Drehwinkel anzeigen';
  mi.OnClick := IndicatorItemClick;

  IndicatorLocalRotItem := AddI('');
  mi.Caption := 'Lokale Achsen (Indikator)';
  mi.Hint := '  Indikator um lokale Achsen drehen';
  mi.OnClick := IndicatorLocalRotItemClick;

  MatrixItem := AddI('');
  mi.Caption := 'Rotationmatrix';
  mi.Hint := '  Rotationsmatrix einblenden';
  mi.OnClick := MatrixItemClick;
end;

end.
