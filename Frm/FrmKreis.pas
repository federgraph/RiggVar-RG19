unit FrmKreis;

interface

uses
  Winapi.Windows,
  System.Classes,
  System.Types,
  RiggVar.FD.Point,
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

    class var
    KR: Integer; { Radius der großen Kreise }
    SR: Integer; { Radius der kleinen Kreise }
    Rand: Integer; { Randabstand der Reflektionskante }
    Margin: Integer;

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
    procedure FormShow(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure PaintBoxPaint(Sender: TObject);
    procedure ShapeOKMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ShapeKreiseMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  private
    SchnittKK: TSchnittKK;
    Sprite1, Sprite2: TSprite;
    FrM1, FrM2: TPoint3D; { double - Mittelpunkte der großen Kreise }
    FiM1, FiM2: TPoint; { Integer - Mittelpunkte der großen Kreise }
    FrPos1, FrPos2: TPoint3D; { double - Mittelpunkte der kleinen Kreise }
    FiPos1, FiPos2: TPoint; { Integer - Mittelpunkte der kleinen Kreise }
    FSchnittOK: Boolean;
    FKreise: Boolean;
    DrawingTool: TDrawingTool;
    TL1, TL2, TL3, TL4: TPoint;
    RB1, RB2, RB3, RB4: TPoint;
    TheImage: TBitMap;

    FScale: single;
    function Scale(Value: Integer): Integer;

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

implementation

{$R *.DFM}

uses
  RiggVar.App.Main;

{ TSprite }

constructor TSprite.Create;
begin
  Left := Margin; { beliebiger Anfangswert }
  Top := Margin; { beliebiger Anfangswert }
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
  FScale := MainVar.Scale;

  TSprite.KR := Scale(100); { Radius große Kreise }
  TSprite.SR := Scale(5); { Radius kleine Kreise }
  TSprite.Rand := Scale(5) + TSprite.SR; { Randabstand der Reflektionskante }
  TSprite.Margin := Scale(10);

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
    Radius1 := TSprite.KR;
    Radius2 := TSprite.KR;
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
    FiM1.X := Left + Rand + KR;
    FiM1.Y := Top  + Rand + KR;
  end;
  with Sprite2 do
  begin
    DrawSprite;
    FiM2.X := Left + Rand + KR;
    FiM2.Y := Top  + Rand + KR;
  end;
end;

procedure TKreisForm.Action;
begin
  MoveSprite;
  { neue MittelPunkte übernehmen }
  FrM1.X := FiM1.X;
  FrM1.Y := FiM1.Y;
  FrM2.X := FiM2.X;
  FrM2.Y := FiM2.Y;
  { Schnittpunkte ausrechnen }
  with SchnittKK do
  begin
    MittelPunkt1 := FrM1;
    MittelPunkt2 := FrM2;
    FrPos1 := SchnittPunkt1;
    FrPos2 := SchnittPunkt2;
  end;
  { Integer - Mittelpunkte der kleinen Kreise }
  FiPos1.X := Round(FrPos1.X);
  FiPos1.Y := Round(FrPos1.Y);
  FiPos2.X := Round(FrPos2.X);
  FiPos2.Y := Round(FrPos2.Y);
  if not(SchnittKK.Status = bmZwei) then
  begin
    FiPos1.X := Scale(-3) * TSprite.SR; { kleine Kreise aus dem Bild schieben }
    FiPos2.X := Scale(-3) * TSprite.SR;
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
  TL3 := Point(Sprite1.Left + TSprite.Rand, Sprite1.Top + TSprite.Rand);
  TL4 := Point(Sprite2.Left + TSprite.Rand, Sprite2.Top + TSprite.Rand);
  RB3 := Point(TL3.X + 2 * TSprite.KR, TL3.Y + 2 * TSprite.KR);
  RB4 := Point(TL4.X + 2 * TSprite.KR, TL4.Y + 2 * TSprite.KR);
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
  TL1 := Point(FiPos1.X - TSprite.SR, FiPos1.Y - TSprite.SR);
  TL2 := Point(FiPos2.X - TSprite.SR, FiPos2.Y - TSprite.SR);
  RB1 := Point(FiPos1.X + TSprite.SR, FiPos1.Y + TSprite.SR);
  RB2 := Point(FiPos2.X + TSprite.SR, FiPos2.Y + TSprite.SR);

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
  with PaintBox.Canvas do
  begin
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

function TKreisForm.Scale(Value: Integer): Integer;
begin
  result := Round(Value * FScale);
end;

end.
