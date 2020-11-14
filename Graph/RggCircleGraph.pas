unit RggCircleGraph;

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
  TCircleSprite = class
  private
    FWidth: Integer;
    FHeight: Integer;
  public
    Left, Top, SW, SH: Integer;
    Step: Integer;
    GoLeft,GoRight,GoUp,GoDown: Boolean;

    constructor Create(AWidth, AHeight: Integer);
    procedure DoStep;
  end;

  TDrawingTool = (dtLine, dtEllipse);

  TCircleGraph = class
  private
    SchnittKK: TSchnittKK;
    C1, C2: TCircleSprite;
    FrM1, FrM2: TPoint3D; { double - Mittelpunkte der großen Kreise }
    FiM1, FiM2: TPoint; { Integer - Mittelpunkte der großen Kreise }
    FrPos1, FrPos2: TPoint3D; { double - Mittelpunkte der kleinen Kreise }
    FiPos1, FiPos2: TPoint; { Integer - Mittelpunkte der kleinen Kreise }
    FSchnittOK: Boolean;
    FKreise: Boolean;
    DrawingTool: TDrawingTool;
    TL1, TL2, TL3, TL4: TPoint;
    RB1, RB2, RB3, RB4: TPoint;
    procedure MoveSprite;
    procedure DrawShape(TopLeft, BottomRight: TPoint; AColor: TColor);
    procedure ZeichneKreise;
    procedure ZeichneSchnittPunkte;
    procedure PaintBackground(g: TCanvas);
    procedure ZeichneText;
  public
    Width: Integer;
    Height: Integer;

    Info: string;
    Version: string;
    ProductName: string;

    Bitmap: TBitmap;

    class var
    KR: Integer; { Radius der großen Kreise }
    SR: Integer; { Radius der kleinen Kreise }
    Margin: Integer; { Randabstand der Reflektionskante }

    procedure Action;
    procedure Draw(g: TCanvas; X, Y: Integer);

    constructor Create;
    destructor Destroy; override;

    property Kreise: Boolean read FKreise write FKreise;
  end;

implementation

{ TCircleSprite }

constructor TCircleSprite.Create(AWidth, AHeight: Integer);
begin
  FWidth := AWidth;
  FHeight := AHeight;

  Left := 10; { beliebiger Anfangswert }
  Top := 10; { beliebiger Anfangswert }

  SH := 2 * (TCircleGraph.Margin + TCircleGraph.KR);
  SW := 2 * (TCircleGraph.Margin + TCircleGraph.KR);
  Step := 1;
end;

procedure TCircleSprite.DoStep;
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
    if (Top + SH) < FHeight then
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
    if (Left + SW) < FWidth then
      Left := Left + Step
    else
    begin
      GoRight := false;
      GoLeft := true;
    end;
end;

{ TCircleGraph }

constructor TCircleGraph.Create;
begin
  Info := '(c) federgraph.de';
  Version := 'RG68';

  Width := 439;
  Height := 372;

  KR := 100; { Radius große Kreise }
  SR := 5; { Radius kleine Kreise }
  Margin := 5 + SR; { Randabstand der Reflektionskante }

  Bitmap := TBitmap.Create;
  Bitmap.Height := Height;
  Bitmap.Width := Width;
  Bitmap.Transparent := True;
  Bitmap.TransparentColor := clBlack;
  Bitmap.TransparentMode := tmFixed;
  PaintBackground(Bitmap.Canvas);

  { SchnittKK }
  SchnittKK := TSchnittKK.Create;
  with SchnittKK do
  begin
    SchnittEbene := seXY;
    Radius1 := KR;
    Radius2 := KR;
  end;

  { Sprite }
  C1 := TCircleSprite.Create(Width, Height);
  C2 := TCircleSprite.Create(Width, Height);
  with C1 do
  begin
    GoRight := true;
    GoDown := true;
    GoLeft := false;
    GoUp := false;
    Step := 1;
  end;
  with C2 do
  begin
    GoRight := false;
    GoDown := false;
    GoLeft := true;
    GoUp := true;
    Step := 2;
  end;

  DrawingTool := dtEllipse;
  Bitmap.Canvas.Brush.Style := bsClear;
  FKreise := False;
end;

destructor TCircleGraph.Destroy;
begin
  SchnittKK.Free;
  C1.Free;
  C2.Free;
  Bitmap.Free;
  inherited;
end;

procedure TCircleGraph.MoveSprite;
begin
  C1.DoStep;
  FiM1.X := C1.Left + Margin + KR;
  FiM1.Y := C1.Top  + Margin + KR;

  C2.DoStep;
  FiM2.X := C2.Left + Margin + KR;
  FiM2.Y := C2.Top  + Margin + KR;
end;

procedure TCircleGraph.Action;
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
end;

procedure TCircleGraph.ZeichneKreise;
begin
  DrawShape(TL3, RB3, clBlack);
  DrawShape(TL4, RB4, clBlack);
  TL3 := Point(C1.Left + Margin, C1.Top + Margin);
  TL4 := Point(C2.Left + Margin, C2.Top + Margin);
  RB3 := Point(TL3.X + 2 * KR, TL3.Y + 2 * KR);
  RB4 := Point(TL4.X + 2 * KR, TL4.Y + 2 * KR);
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

procedure TCircleGraph.ZeichneSchnittPunkte;
begin
  TL1 := Point(FiPos1.X - SR, FiPos1.Y - SR);
  TL2 := Point(FiPos2.X - SR, FiPos2.Y - SR);
  RB1 := Point(FiPos1.X + SR, FiPos1.Y + SR);
  RB2 := Point(FiPos2.X + SR, FiPos2.Y + SR);

  if FSchnittOK then
  begin
    Bitmap.Canvas.Brush.Style := bsSolid;
    Bitmap.Canvas.Brush.Color := clYellow;
    DrawShape(TL1, RB1, clYellow);
    Bitmap.Canvas.Brush.Color := clFuchsia;
    DrawShape(TL2, RB2, clFuchsia);
    Bitmap.Canvas.Brush.Style := bsClear;
  end
end;

procedure TCircleGraph.DrawShape(TopLeft, BottomRight: TPoint; AColor: TColor);
begin
  with Bitmap.Canvas do
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

procedure TCircleGraph.ZeichneText;
begin
  with Bitmap.Canvas do
  begin
    SetBkMode(Handle, TRANSPARENT);
    Font.Name := 'Consolas';
    Font.Size := 16;
    TextOut(10, 20, ProductName);
    TextOut(30, 50, Version);
    SetBkMode(Handle, OPAQUE);
  end;
end;

procedure TCircleGraph.PaintBackground(g: TCanvas);
var
  ARect: TRect;
begin
  g.CopyMode := cmBlackness;
  ARect := Rect(0, 0, Width, Height);
  g.CopyRect(ARect, g, ARect);
  g.CopyMode := cmSrcCopy;
end;

procedure TCircleGraph.Draw(g: TCanvas; X, Y: Integer);
begin
  Action;
  g.CopyMode := cmSrcCopy;
  g.Draw(X, Y, Bitmap);
end;

end.
