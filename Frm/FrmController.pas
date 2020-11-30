unit FrmController;

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
  RiggVar.RG.Model,
  RggCtrls;

type
  TFormController = class(TForm)
    OutputPages: TPageControl;
    ControllerSheet: TTabSheet;
    pnController: TPanel;
    ZustellenBtn: TButton;
    ControllerPaintBox: TPaintBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ControllerPaintBoxPaint(Sender: TObject);
    procedure ZustellenBtnClick(Sender: TObject);
  private
    FBackgroundColor: TColor;
    procedure DrawPaintBoxC(Canvas: TCanvas);
    procedure PaintBackGround(Image: TBitmap);
    procedure UpdateProps;
  public
    Rigg: TRigg2;
    SalingGraph: TSalingGraph;
    BitmapC: TBitmap;
    procedure UpdateGraph;
  end;

var
  FormController: TFormController;

implementation

{$R *.DFM}

uses
  RiggVar.RG.Def,
  RiggVar.App.Main;

procedure TFormController.FormCreate(Sender: TObject);
begin
  FBackgroundColor := MainVar.ColorScheme.claBackground;

  Rigg := Main.Rigg;

  SalingGraph := TSalingGraph.Create;
  SalingGraph.BackgroundColor := FBackgroundColor;

  BitmapC := TBitmap.Create;
  BitmapC.Width := Round(453 * MainVar.Scale);
  BitmapC.Height := Round(220 * MainVar.Scale);
  PaintBackGround(BitmapC);
end;

procedure TFormController.FormDestroy(Sender: TObject);
begin
  ControllerPaintBox := nil;
  SalingGraph.Free;
  BitmapC.Free;
end;

procedure TFormController.ControllerPaintBoxPaint(Sender: TObject);
begin
  DrawPaintBoxC(ControllerPaintBox.Canvas);
end;

procedure TFormController.ZustellenBtnClick(Sender: TObject);
var
  TrimmRec: TTrimmControls;
begin
  TrimmRec := Rigg.Glieder;
  TrimmRec.Controller := 50;
  Rigg.Glieder := TrimmRec;
  Rigg.UpdateGetriebe;
  SalingGraph.ControllerPos := Round(SalingGraph.ParamXE0 - Rigg.MastPositionE);
  TrimmRec.Controller := SalingGraph.ControllerPos;
  Rigg.Glieder := TrimmRec;
  Main.UpdateGetriebe;
  UpdateGraph;
end;

procedure TFormController.UpdateProps;
var
  TrimmRec: TTrimmControls;
begin
  { ControllerPaintBox }
  SalingGraph.ControllerTyp := Rigg.ControllerTyp;
  TrimmRec := Rigg.Glieder;
  { Abstand(iP[ooE0,x], iP[ooE,x]) in mm}
  SalingGraph.ControllerPos := TrimmRec.Controller;
  { Position des Mastes in Deckshöhe von D0 aus in mm }
  SalingGraph.ParamXE := Round(Rigg.MastPositionE);
  { Abstand(iP[ooD0,x], iP[ooE0,x]) in mm }
  SalingGraph.ParamXE0 := Round(Rigg.rP.E0.X - Rigg.rP.D0.X);
  { Abstand von E0 zur Anschlagkante Deck + Klotzdicke }
  SalingGraph.EdgePos := Round(Rigg.GSB.Find(fpController).Min);
end;

procedure TFormController.UpdateGraph;
begin
  UpdateProps;
  if Assigned(ControllerPaintBox) then
    DrawPaintBoxC(ControllerPaintBox.Canvas);
end;

procedure TFormController.DrawPaintBoxC(Canvas: TCanvas);
begin
  PaintBackGround(BitmapC);
  SalingGraph.DrawController(BitmapC.Canvas);
  Canvas.CopyMode := cmSrcCopy;
  Canvas.Draw(0, 0, BitMapC);
end;

procedure TFormController.PaintBackGround(Image: TBitmap);
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
