unit FrmKreis;

interface

uses
  Winapi.Windows,
  System.Classes,
  System.Types,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  RggTypes,
  RggSchnittKK;

type
  TSprite = class
  public
    Left, Top, Width, Height: Integer;
    Step: Integer;
    GoLeft,GoRight,GoUp,GoDown: Boolean;
    constructor Create;
    procedure DrawSprite;
  end;

  TDrawingTool = (dtLine, dtEllipse);

  TKreisForm = class(TForm)
    ImagePanel: TPanel;
    ControlPanel: TPanel;
    PaintBox: TPaintBox;
    ShapeOK: TShape;
    ShapeKreise: TShape;
    lbKreise: TLabel;
    lbOK: TLabel;
    lbVersionText: TLabel;
    lbProductName: TLabel;
    Timer: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure PaintBoxPaint(Sender: TObject);
    procedure ShapeOKMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ShapeKreiseMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    SchnittKK: TSchnittKK;
    Sprite1, Sprite2: TSprite;
    FrM1, FrM2: TRealPoint; { double - Mittelpunkte der großen Kreise }
    FiM1, FiM2: TPoint; { Integer - Mittelpunkte der großen Kreise }
    FrPos1, FrPos2: TRealPoint; { double - Mittelpunkte der kleinen Kreise }
    FiPos1, FiPos2: TPoint; { Integer - Mittelpunkte der kleinen Kreise }
    FSchnittOK: Boolean;
    FKreise: Boolean;
    DrawingTool: TDrawingTool;
    TL1, TL2, TL3, TL4: TPoint;
    RB1, RB2, RB3, RB4: TPoint;
    TheImage: TBitMap;
    procedure MoveSprite;
    procedure DrawShape(TopLeft, BottomRight: TPoint; AColor: TColor);
    procedure ZeichneKreise;
    procedure ZeichneSchnittPunkte;
    procedure PaintBackground(AnImage: TBitmap);
    procedure ZeichneText;
    procedure Action;
  end;

var
  KreisForm: TKreisForm;
  KR: Integer; { Radius der großen Kreise }
  SR: Integer; { Radius der kleinen Kreise }
  Rand: Integer; { Randabstand der Reflektionskante }

implementation

{$R *.DFM}

{ TSprite }

constructor TSprite.Create;
begin
  Left := 10; { beliebiger Anfangswert }
  Top := 10; { beliebiger Anfangswert }
  Height := 2 * (Rand + KR);
  Width := 2 * (Rand + KR);
  Step := 1;
end;

procedure TSprite.DrawSprite;
begin
  if GoLeft then
    if Left > 0 then
      Left := Left - Step
    else
    begin
      GoLeft := false;
      GoRight := true;
    end;

  if GoDown then
    if (Top + Height) < KreisForm.PaintBox.ClientHeight then
      Top := Top + Step
    else
    begin
      GoDown := false;
      GoUp := true;
    end;

  if GoUp then
    if Top > 0 then
      Top := Top - Step
    else
    begin
      GoUp := false;
      GoDown := true;
    end;

  if GoRight then
    if (Left + Width) < KreisForm.PaintBox.ClientWidth then
      Left := Left + Step
    else
    begin
      GoRight := false;
      GoLeft := true;
    end;
end;

{ TKreisform }

procedure TKreisForm.FormCreate(Sender: TObject);
begin
  Caption := 'RiggVar About Form';
  ControlPanel.Font.Name := 'Courier New';
  ControlPanel.Caption := ' (c) federgraph.de';
  lbVersionText.Caption := 'RG19';

  ImagePanel.FullRepaint := False;
  ControlPanel.FullRepaint := False;

  TheImage := TBitmap.Create;
  TheImage.Height := Height;
  TheImage.Width := Width;
  PaintBackground(TheImage);

  { SchnittKK }
  SchnittKK := TSchnittKK.Create;
  with SchnittKK do
  begin
    SchnittEbene := seXY;
    Radius1 := KR;
    Radius2 := KR;
  end;

  { Sprite }
  Sprite1 := TSprite.Create;
  Sprite2 := TSprite.Create;
  with Sprite1 do
  begin
    GoRight := true;
    GoDown := true;
    GoLeft := false;
    GoUp := false;
    Step := 1;
  end;
  with Sprite2 do
  begin
    GoRight := false;
    GoDown := false;
    GoLeft := true;
    GoUp := true;
    Step := 2;
  end;

  DrawingTool := dtEllipse;
  TheImage.Canvas.Brush.Style := bsClear;
  Timer.Interval := 1; { 1 ms }
  Timer.enabled := True;
  FKreise := False;
end;

procedure TKreisForm.FormDestroy(Sender: TObject);
begin
  SchnittKK.Free;
  Sprite1.Free;
  Sprite2.Free;
  TheImage.Free;
end;

procedure TKreisForm.FormHide(Sender: TObject);
begin
  Timer.Enabled := False;
end;

procedure TKreisForm.MoveSprite;
begin
  with Sprite1 do
  begin
    DrawSprite;
    FiM1.x := Left + Rand + KR;
    FiM1.y := Top  + Rand + KR;
  end;
  with Sprite2 do
  begin
    DrawSprite;
    FiM2.x := Left + Rand + KR;
    FiM2.y := Top  + Rand + KR;
  end;
end;

procedure TKreisForm.Action;
begin
  MoveSprite;
  { neue MittelPunkte übernehmen }
  FrM1[x] := FiM1.x;
  FrM1[y] := FiM1.y;
  FrM2[x] := FiM2.x;
  FrM2[y] := FiM2.y;
  { Schnittpunkte ausrechnen }
  with SchnittKK do
  begin
    MittelPunkt1 := FrM1;
    MittelPunkt2 := FrM2;
    FrPos1 := SchnittPunkt1;
    FrPos2 := SchnittPunkt2;
  end;
  { Integer - Mittelpunkte der kleinen Kreise }
  FiPos1.x := Round(FrPos1[x]);
  FiPos1.y := Round(FrPos1[y]);
  FiPos2.x := Round(FrPos2[x]);
  FiPos2.y := Round(FrPos2[y]);
  if not(SchnittKK.Status = bmZwei) then
  begin
    FiPos1.x := -3 * SR; { kleine Kreise aus dem Bild schieben }
    FiPos2.x := -3 * SR;
  end;
  { kleine Kreise zeichnen }
  if SchnittKK.Status = bmZwei then
    FSchnittOK := True
  else
    FSchnittOK := False;
  ZeichneSchnittPunkte;
  { große Kreise zeichnen }
  ZeichneKreise;
  { Text zeichnen}
  ZeichneText;
  { TheImage auf den Bildschirm kopieren }
  with PaintBox.Canvas do
  begin
    CopyMode := cmSrcCopy;
    Draw(0, 0, TheImage);
  end;
end;

procedure TKreisform.ZeichneKreise;
begin
  DrawShape(TL3, RB3, clBlack);
  DrawShape(TL4, RB4, clBlack);
  TL3 := Point(Sprite1.Left+Rand, Sprite1.Top+Rand);
  TL4 := Point(Sprite2.Left+Rand, Sprite2.Top+Rand);
  RB3 := Point(TL3.x+2*KR, TL3.y+2*KR);
  RB4 := Point(TL4.x+2*KR, TL4.y+2*KR);
  if FKreise then
  begin
    DrawShape(TL3, RB3, clTeal);
    DrawShape(TL4, RB4, clTeal);
  end
  else
  begin
    DrawShape(TL3, RB3, clBlack);
    DrawShape(TL4, RB4, clBlack);
  end;
end;

procedure TKreisForm.ZeichneSchnittPunkte;
begin
  TL1 := Point(FiPos1.x-SR, FiPos1.y-SR);
  TL2 := Point(FiPos2.x-SR, FiPos2.y-SR);
  RB1 := Point(FiPos1.x+SR, FiPos1.y+SR);
  RB2 := Point(FiPos2.x+SR, FiPos2.y+SR);

  if FSchnittOK then
  begin
    TheImage.Canvas.Brush.Style := bsSolid;
    TheImage.Canvas.Brush.Color := clYellow;
    DrawShape(TL1, RB1, clYellow);
    TheImage.Canvas.Brush.Color := clFuchsia;
    DrawShape(TL2, RB2, clFuchsia);
    TheImage.Canvas.Brush.Style := bsClear;
  end
end;

procedure TKreisForm.DrawShape(TopLeft, BottomRight: TPoint; AColor: TColor);
begin
  with TheImage.Canvas do
  begin
    Pen.Color := AColor;
    case DrawingTool of
      dtLine:
        begin
          MoveTo(TopLeft.X, TopLeft.Y);
          LineTo(BottomRight.X, BottomRight.Y);
        end;
      dtEllipse:
        Ellipse(TopLeft.X, TopLeft.Y, BottomRight.X, BottomRight.Y);
    end;
  end;
end;

procedure TKreisform.ZeichneText;
begin
  with TheImage.Canvas do
  begin
    SetBkMode(Handle, TRANSPARENT);
    Font := lbProductName.Font; { kann im ObjectInspector gesetzt werden! }
    TextOut(lbProductName.Left, lbProductName.Top, lbProductName.Caption);
    Font := lbVersionText.Font; { kann im ObjectInspector gesetzt werden! }
    TextOut(lbVersionText.Left, lbVersionText.Top, lbVersionText.Caption);
    SetBkMode(Handle, OPAQUE);
  end;
end;

procedure TKreisform.PaintBackground(AnImage: TBitmap);
{ alles Schwarzmachen }
var
  ARect: TRect;
begin
  with AnImage.Canvas do
  begin
    CopyMode := cmBlackness;
    ARect := Rect(0, 0, Width, Height);
    CopyRect(ARect, AnImage.Canvas, ARect);
    CopyMode := cmSrcCopy;
  end;
end;

procedure TKreisForm.PaintBoxPaint(Sender: TObject);
begin
  with PaintBox.Canvas do begin
    CopyMode := cmSrcCopy;
    Draw(0, 0, TheImage);
  end;
end;

procedure TKreisForm.TimerTimer(Sender: TObject);
begin
  Action;
end;

procedure TKreisForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
  Timer.enabled := not Timer.enabled;
end;

procedure TKreisForm.FormShow(Sender: TObject);
begin
  Timer.Enabled := True;
end;

procedure TKreisForm.ShapeOKMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  Close;
end;

procedure TKreisForm.ShapeKreiseMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FKreise := not FKreise;
end;

initialization
  KR := 100; { Radius große Kreise }
  SR := 5; { Radius kleine Kreise }
  Rand := 5 + SR; { Randabstand der Reflektionskante }
  
end.
