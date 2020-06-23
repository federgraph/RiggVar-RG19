unit RggTrimmTabGraph;

interface

uses
  Winapi.Windows,
  System.SysUtils,
  System.Types,
  Vcl.Graphics,
  Vcl.ExtCtrls,
  RggTypes,
  RggTrimmTab;

type
  TTrimmTabGraph = class
  private
    FImage: TImage;
    FBitmap: TBitmap;
    procedure SetImage(const Value: TImage);
    procedure InitBitmap;
    procedure PaintBackGround(g: TCanvas);
    procedure DrawGraph(g: TCanvas; ARect: TRect);
    procedure DrawText(g: TCanvas);
  public
    FScale: single;
    BackgroundColor: TColor;
    Width: Integer;
    Height: Integer;
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
  FScale := Main.Scale;
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

  function Limit(a: double): double;
  begin
    if a < -32000 then
      a := -32000
    else if a > 32000 then
      a := 32000;
    result := a;
  end;

var
  P: TPoint;
  R: TRect;
  i, RadiusX, RadiusY: Integer;
  PlotWidth, PlotHeight: Integer;
  PlotExtX, PlotExtY: Integer;
  PlotOrgX, PlotOrgY: Integer;
  tempX, tempY: double;
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
            P.y := Round(Limit(tempY));
            MoveTo(0, P.y);
            LineTo(PlotExtX, P.y);
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
              P.x := Round(Limit(PlotExtX * Model.LineDataX[i]));
              P.y := Round(Limit(PlotExtY * Model.LineDataY[i]));
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
        tempX := PlotExtX * Model.Kurve[i].y / Model.EndwertWeg;
        tempY := PlotExtY * Model.Kurve[i].x / Model.EndwertKraft;
        P.x := Round(Limit(tempX));
        P.y := Round(Limit(tempY));
        Rectangle(
          P.x - RadiusX,
          P.y - RadiusY,
          P.x + RadiusX,
          P.y + RadiusY);
      end;

      Pen.Color := clBlack;
      Brush.Color := clRed;
      Brush.Style := bsSolid;

      if Model.TabellenTyp > itGerade then
      begin
        tempX := PlotExtX * Model.y1 / Model.EndwertWeg;
        tempY := PlotExtY * Model.x1 / Model.EndwertKraft;
        P.x := Round(Limit(tempX));
        P.y := Round(Limit(tempY));
        Rectangle(
          P.x - RadiusX,
          P.y - RadiusY,
          P.x + RadiusX,
          P.y + RadiusY);
      end;

      P := Point(0, 0);
      Rectangle(P.x - RadiusX, P.y - RadiusY, P.x + RadiusX, P.y + RadiusY);

      tempX := PlotExtX * Model.y2 / Model.EndwertWeg;
      tempY := PlotExtY * Model.x2 / Model.EndwertKraft;
      P.x := Round(Limit(tempX));
      P.y := Round(Limit(tempY));
      Rectangle(P.x - RadiusX, P.y - RadiusY, P.x + RadiusX, P.y + RadiusY);

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
  with g do
  begin
      { Texte }
      Brush.Style := bsClear;
      { Font := YFont; }
      SetTextAlign(Handle, TA_LEFT or TA_TOP);
      PosX := 5;
      PosY := 5;
      TextOut(PosX, PosY, 'Kraft [N]');

      Font.Color := clBlack;
      PosY := PosY - Font.Height + 5;
      s := Format('(%d ... %d)', [0, Model.EndwertKraft]);
      TextOut(PosX, PosY, s);

      { Font := XFont; }
      SetTextAlign(Handle, TA_RIGHT or TA_BOTTOM);
    PosX := Width - 5;
    PosY := Height - 5;
      TextOut(PosX, PosY, 'Weg [mm]');

      Font.Color := clBlack;
      PosY := PosY + Font.Height - 5;
      s := Format('(%d ... %d)', [0, Model.EndwertWeg]);
      TextOut(PosX, PosY, s);

      Brush.Style := bsSolid;
      Brush.Color := clBtnFace;
    end;
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
  FBitmap.Width := Round(Width * MainVar.Scale);
  FBitmap.Height := Round(Height * MainVar.Scale);
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
