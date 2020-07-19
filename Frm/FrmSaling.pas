unit FrmSaling;

interface

uses
  System.Classes,
  System.Types,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.Buttons,
  Vcl.ComCtrls,
  RggTypes,
  RggUnit4,
  RggCtrls;

type
  TFormSaling = class(TForm)
    OutputPages: TPageControl;
    Salingsheet: TTabSheet;
    pnSaling: TPanel;
    SalingPaintBox: TPaintBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SalingPaintBoxPaint(Sender: TObject);
  private
    FBackgroundColor: TColor;
    procedure DrawPaintBoxS(Canvas: TCanvas);
    procedure PaintBackGround(Image: TBitmap);
    procedure UpdateProps;
  public
    Rigg: TRigg;
    SalingGraph: TSalingGraph;
    BitmapS: TBitmap;
    procedure UpdateGraph;
  end;

var
  FormSaling: TFormSaling;

implementation

{$R *.DFM}

uses
  RiggVar.RG.Def,
  RiggVar.App.Main;

procedure TFormSaling.FormCreate(Sender: TObject);
begin
  FBackgroundColor := MainVar.ColorScheme.claBackground;

  Rigg := Main.Rigg;
  SalingGraph := TSalingGraph.Create;

  SalingGraph.BackgroundColor := FBackgroundColor;

  BitmapS := TBitmap.Create;
  BitmapS.Width := Round(453 * MainVar.Scale);
  BitmapS.Height := Round(220 * MainVar.Scale);
end;

procedure TFormSaling.FormDestroy(Sender: TObject);
begin
  SalingPaintBox := nil;
  SalingGraph.Free;
  BitmapS.Free;
end;

procedure TFormSaling.SalingPaintBoxPaint(Sender: TObject);
begin
  DrawPaintBoxS(SalingPaintBox.Canvas);
end;

procedure TFormSaling.UpdateProps;
var
  TrimmRec: TTrimmControls;
begin
  TrimmRec := Rigg.Glieder;
  SalingGraph.SalingA := TrimmRec.SalingA;
  SalingGraph.SalingH := TrimmRec.SalingH;
  SalingGraph.SalingL := TrimmRec.SalingL;
end;

procedure TFormSaling.UpdateGraph;
begin
  UpdateProps;
  if Assigned(SalingPaintBox) then
    DrawPaintBoxS(SalingPaintBox.Canvas);
end;

procedure TFormSaling.DrawPaintBoxS(Canvas: TCanvas);
begin
  PaintBackGround(BitmapS);
  SalingGraph.DrawSaling(BitmapS.Canvas);
  Canvas.CopyMode := cmSrcCopy;
  Canvas.Draw(0, 0, BitMapS);
end;

procedure TFormSaling.PaintBackGround(Image: TBitmap);
var
  R: TRect;
begin
  if Image = nil then
    Exit;
  R := Rect(0, 0, Image.Width, Image.Height);
  Image.Canvas.Brush.Color := FBackgroundColor;
  Image.Canvas.FillRect(R);
end;

end.
