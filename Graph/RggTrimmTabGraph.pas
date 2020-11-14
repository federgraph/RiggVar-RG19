unit RggTrimmTabGraph;

interface

uses
  Windows,
  SysUtils,
  Types,
  Graphics,
  ExtCtrls,
  RggTypes,
  RggTrimmTab;

type
  TTrimmTabGraph = class
  private
    FScale: single;
    Margin: Integer;
    BackgroundColor: TColor;
    Width: Integer;
    Height: Integer;
    FImage: TImage;
    FBitmap: TBitmap;
    function Scale(Value: Integer): Integer;
    procedure SetImage(const Value: TImage);
    procedure InitBitmap;
    procedure PaintBackGround(g: TCanvas);
    procedure DrawGraph(g: TCanvas; ARect: TRect);
    procedure DrawText(g: TCanvas);
  public
    ImageOpacity: single;
    Model: TTrimmTabGraphModel;

    constructor Create;
    destructor Destroy; override;

    procedure Draw;
    property Image: TImage read FImage write SetImage;
  end;

implementation

uses
  RiggVar.App.Main;

constructor TTrimmTabGraph.Create;
begin
  FScale := MainVar.Scale;
  Margin := 5;
  BackgroundColor := clWhite;

  Width := 319; // unscaled value
  Height := 158; // unscaled value

  Model := TTrimmTabGraphModel.Create;
end;

destructor TTrimmTabGraph.Destroy;
begin
  FBitmap.Free;
  Model.Free;
  inherited;
end;

procedure TTrimmTabGraph.PaintBackGround(g: TCanvas);
var
  R: TRect;
begin
  R := Rect(0, 0, Image.Width, Image.Height);
  g.Brush.Color := BackgroundColor;
  g.FillRect(R);
end;

procedure TTrimmTabGraph.DrawGraph(g: TCanvas; ARect: TRect);
var
  P: TPoint;
  R: TRect;
  i, RadiusX, RadiusY: Integer;
  PlotWidth, PlotHeight: Integer;
  PlotExtX, PlotExtY: Integer;
  PlotOrgX, PlotOrgY: Integer;
  tempX, tempY: double;

  function Limit(a: double): double;
  begin
    if a < -32000 then
      a := -32000
    else if a > 32000 then
      a := 32000;
    result := a;
  end;

  procedure DrawR;
  begin
    g.Rectangle(
      P.X - RadiusX,
      P.Y - RadiusY,
      P.X + RadiusX,
      P.Y + RadiusY);
  end;

begin
  PlotWidth := ARect.Right - ARect.Left;
  PlotHeight := ARect.Bottom - ARect.Top;
  PlotExtX := PlotWidth;
  PlotExtY := PlotHeight;
  PlotOrgX := 0;
  PlotOrgY := 0;

  with g do
  begin
    SetMapMode(Handle, MM_ANISOTROPIC);
    SetWindowExtEx(Handle, PlotExtX, -PlotExtY, nil);
    SetWindowOrgEx(Handle, PlotOrgX, PlotOrgY, nil);
    SetViewPortExtEx(Handle, PlotWidth, PlotHeight, nil);
    SetViewPortOrgEx(Handle, 0, PlotHeight, nil);

    { Radius }
    R.Left := 0;
    R.Top := 0;
    R.Bottom := Round(3 * FScale);
    R.Right := R.Bottom;
    DPTOLP(Handle, R, 2);
    RadiusX := R.Right - R.Left;
    RadiusY := R.Bottom - R.Top;

    { Kurve }
    Pen.Color := clBlue;
    case Model.TabellenTyp of
      itKonstante:
        begin
          tempY := PlotExtY * (Model.x1 / Model.EndwertKraft);
          P.Y := Round(Limit(tempY));
          MoveTo(0, P.y);
          LineTo(PlotExtX, P.Y);
        end;
      itGerade:
        begin
          MoveTo(0, 0);
          LineTo(PlotExtX, PlotExtY);
        end;
      itParabel, itBezier:
        begin
          MoveTo(0, 0);
          for i := 0 to 100 do
          begin
            P.X := Round(Limit(PlotExtX * Model.LineDataX[i]));
            P.Y := Round(Limit(PlotExtY * Model.LineDataY[i]));
            LineTo(P.x, P.y);
          end;
        end;
    end;

    { Rechtecke }
    Pen.Color := clBlack;
    Brush.Color := clYellow;
    Brush.Style := bsSolid;
    for i := 1 to Model.PunkteAnzahl do
    begin
      tempX := PlotExtX * Model.Kurve[i].Y / Model.EndwertWeg;
      tempY := PlotExtY * Model.Kurve[i].X / Model.EndwertKraft;
      P.X := Round(Limit(tempX));
      P.Y := Round(Limit(tempY));
      DrawR;
    end;

    Pen.Color := clBlack;
    Brush.Color := clRed;
    Brush.Style := bsSolid;

    if Model.TabellenTyp > itGerade then
    begin
      tempX := PlotExtX * Model.y1 / Model.EndwertWeg;
      tempY := PlotExtY * Model.x1 / Model.EndwertKraft;
      P.X := Round(Limit(tempX));
      P.Y := Round(Limit(tempY));
    DrawR;
    end;

    P := Point(0, 0);
  DrawR;

    tempX := PlotExtX * Model.y2 / Model.EndwertWeg;
    tempY := PlotExtY * Model.x2 / Model.EndwertKraft;
    P.X := Round(Limit(tempX));
    P.Y := Round(Limit(tempY));
    DrawR;

    SetMapMode(Handle, MM_TEXT);
    SetWindowOrgEx(Handle, 0, 0, nil);
    SetViewPortOrgEx(Handle, 0, 0, nil);
  end;
end;


procedure TTrimmTabGraph.DrawText(g: TCanvas);
var
  PosX, PosY: Integer;
  s: string;
begin
  g.Brush.Style := bsClear;
  SetTextAlign(g.Handle, TA_LEFT or TA_TOP);
  PosX := Margin;
  PosY := Margin;
  g.TextOut(PosX, PosY, 'Kraft [N]');

  g.Font.Color := clBlack;
  PosY := PosY - g.Font.Height + Margin;
  s := Format('(%d ... %d)', [0, Model.EndwertKraft]);
  g.TextOut(PosX, PosY, s);

  SetTextAlign(g.Handle, TA_RIGHT or TA_BOTTOM);
  PosX := Width - Margin;
  PosY := Height - Margin;
  g.TextOut(PosX, PosY, 'Weg [mm]');

  g.Font.Color := clBlack;
  PosY := PosY + g.Font.Height - Margin;
  s := Format('(%d ... %d)', [0, Model.EndwertWeg]);
  g.TextOut(PosX, PosY, s);

  g.Brush.Style := bsSolid;
  g.Brush.Color := clBtnFace;
end;

function TTrimmTabGraph.Scale(Value: Integer): Integer;
begin
  result := Round(Value * FScale);
end;

procedure TTrimmTabGraph.SetImage(const Value: TImage);
begin
  FImage := Value;
  InitBitmap;
end;

procedure TTrimmTabGraph.InitBitmap;
begin
  if FBitmap <> nil then
  begin
    FBitmap.Free;
  end;
  FBitmap := TBitmap.Create;
  FBitmap.Width := Scale(Width);
  FBitmap.Height := Scale(Height);
  Image.Width := FBitmap.Width;
  Image.Height := FBitmap.Height;
end;

procedure TTrimmTabGraph.Draw;
var
  R: TRect;
begin
  if Image = nil then
    Exit;

  R := Rect(0, 0, Image.Width, Image.Height);

  PaintBackground(FBitmap.Canvas);

  DrawGraph(FBitmap.Canvas, R);
  DrawText(FBitmap.Canvas);

  Image.Canvas.CopyMode := cmSrcCopy;
  Image.Canvas.Draw(0, 0, FBitmap);
end;

end.
