unit FrmChartRgg;

interface

uses
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.Menus,
  Vcl.StdCtrls,
  Vcl.Mask,
  Vcl.Buttons,
  Vcl.ExtCtrls,
  Vcl.ComCtrls,
  FrmChart;

type
  TChartFormGS = class(TChartForm)
    lbXLeft: TLabel;
    lbAchseX: TLabel;
    lbXRight: TLabel;
    ChartPaintBox: TPaintBox;
    PaintBoxLegend: TPaintBox;
    ChartBevelOuter: TBevel;
    ChartBevelInner: TBevel;
    lbParam: TLabel;
    procedure FormPaint(Sender: TObject);
    procedure RectangleItemClick(Sender: TObject);
    procedure ChartPaintBoxPaint(Sender: TObject);
    procedure PaintBoxLegendPaint(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure DrawChartPaintBox(Canvas: TCanvas; Rect: TRect);
  protected
    procedure DrawToChart; override;
    procedure DoLegend; override;
    procedure DrawLegend(Canvas: TCanvas; Rect: TRect);
    procedure DrawLabels;
  protected
    WantRectangles: Boolean;
    RectangleItem: TMenuItem;
    N4: TMenuItem;
    procedure InitMenu; override;
  end;

var
  ChartFormGS: TChartFormGS;

implementation

{$R *.DFM}

uses
  RggTypes;

procedure TChartFormGS.FormCreate(Sender: TObject);
begin
  WantRectangles := True;

  inherited;

  ClientWidth := 800;
  ClientHeight := 478;

  with ChartBevelInner do
  begin
    Left := ChartPaintBox.Left-1;
    Top := ChartPaintBox.Top-1;
    Width := ChartPaintBox.Width+2;
    Height := ChartPaintBox.Height+2;
  end;
end;

procedure TChartFormGS.DoLegend;
var
  R: TRect;
begin
  { überschriebene virtuelle Methode }
  inherited;
  if Legend then DrawLegend(PaintBoxLegend.Canvas, PaintBoxLegend.BoundsRect)
  else
    with PaintBoxLegend do  begin
      R := Rect(0,0,Width,Height);
      Canvas.Brush.Color := clBtnFace;
      Canvas.FillRect(R);
    end;
end;

procedure TChartFormGS.DrawToChart;
begin
 { überschriebene virtuelle Methode }
  DrawChartPaintBox(ChartPaintBox.Canvas, ChartPaintBox.BoundsRect);
end;

procedure TChartFormGS.DrawLegend(Canvas: TCanvas; Rect: TRect);
var
  Bitmap: TBitmap;
  p, PosX, PosY: Integer;
begin
  Bitmap := TBitmap.Create;
  with Bitmap do begin
    Width := Rect.Right-Rect.Left;
    Height := Rect.Bottom-Rect.Top;
  end;
  try
    PaintBackGround(Bitmap);
    with Bitmap.Canvas do
    begin
      PosX := 0;
      PosY := 0;
      for p := 0 to ParamCount-1 do
      begin
        { Bullet }
        Pen.Color := clBlack; { clBlue }
        Brush.Color := cf[p];
        Brush.Style := bsSolid;
        PosY := PosY + 30;
        Rectangle( PosX, PosY, PosX + 10, PosY + 5);
        { Text }
        Brush.Style := bsClear;
        PosY := PosY + 10;
        if Valid then
          TextOut(PosX, PosY, PText[p])
        else
          TextOut(PosX, PosY, PColorText[p]);
      end;
      (*
      { Rahmen zeichnen }
      Pen.Width := 1;
      Pen.Color := clBlack;
      Brush.Style := bsClear;
      Rectangle( 0, 0, Bitmap.Width, Bitmap.Height);
      *)
    end;

    with Canvas do
    begin
      CopyMode := cmSrcCopy;
      Draw(0, 0, BitMap);
    end;
  finally
    Bitmap.Free;
  end;
end;

procedure TChartFormGS.DrawChartPaintBox(Canvas: TCanvas; Rect: TRect);

  function Limit(a: double): double;
  begin
    if a < -32000 then
      a := -32000
    else if a > 32000 then
      a := 32000;
    Result := a;
  end;

var
  Pt: TPoint;
  R: TRect;
  i, p, RadiusX, RadiusY: Integer;
  Bitmap: TBitmap;
  PlotWidth, PlotHeight: Integer;
  PlotExtX, PlotExtY: Integer;
  PlotOrgX, PlotOrgY: Integer;
  tempX, tempY: double;
begin
  { schnelle direkte Textausgabe für oft veränderte Labels }
  DrawLabels;

  { diese Labels sind nicht zeitkritisch }
  lbAchseX.Caption := XTitle;
  lbXLeft.Caption := IntToStr(Round(Xmin));
  lbXRight.Caption := IntToStr(Round(Xmax));
  lbParam.Caption := PTitle;

  PlotWidth := Rect.Right - Rect.Left;
  PlotHeight := Rect.Bottom - Rect.Top;
  PlotExtX := PlotWidth;
  PlotExtY := PlotHeight;
  PlotOrgX := 0;
  PlotOrgY := 0;

  Bitmap := TBitmap.Create;
  with Bitmap do
  begin
    Width := PlotWidth;
    Height := PlotHeight;
  end;
  try
    PaintBackGround(Bitmap);

    with Bitmap.Canvas do
    begin
      SetMapMode(Handle, MM_ANISOTROPIC);
      SetWindowExtEx(Handle, PlotExtX, -PlotExtY, nil);
      SetWindowOrgEx(Handle, PlotOrgX, PlotOrgY, nil);
      SetViewPortExtEx(Handle, PlotWidth, PlotHeight, nil);
      SetViewPortOrgEx(Handle, 0, PlotHeight, nil);

      { Radius }
      R.Left := 0; R.Top := 0; R.Bottom := 3; R.Right := 3;
      DPTOLP(Handle, R, 2);
      RadiusX := R.Right-R.Left; RadiusY := R.Bottom-R.Top;

      for p := 0 to ParamCount-1 do
      begin

        { Kurve }
        Pen.Color := cf[p];
        tempY := PlotExtY * (bf[p,0]-Ymin)/(Ymax-Ymin);
        Pt.y := Round(Limit(tempY));
        MoveTo(0,Pt.y);
        for i := 1 to 100 do
        begin
          tempX := PlotExtX * (i/100);
          tempY := PlotExtY * (bf[p,i]-Ymin)/(Ymax-Ymin);
          Pt.x := Round(Limit(tempX));
          Pt.y := Round(Limit(tempY));
          LineTo(Pt.x, Pt.y);
        end;

        if WantRectangles then
        begin
          { Rechtecke }
          Pen.Color := clBlack;
          Brush.Color := cf[p];
          Brush.Style := bsSolid;
          for i := 0 to 100 do
          begin
            tempX := PlotExtX * (i/100);
            tempY := PlotExtY * (bf[p,i]-Ymin)/(Ymax-Ymin);
            Pt.x := Round(Limit(tempX));
            Pt.y := Round(Limit(tempY));
            Rectangle( Pt.x - RadiusX, Pt.y - RadiusY,
                       Pt.x + RadiusX, Pt.y + RadiusY);
          end;
        end;

      end;

      SetMapMode(Handle, MM_TEXT);
      SetWindowOrgEx(Handle, 0, 0, nil);
      SetViewPortOrgEx(Handle, 0, 0, nil);

      (*
      {Rahmen zeichnen}
      Pen.Width := 1;
      Pen.Color := clBlack;
      Brush.Style := bsClear;
      Rectangle( 0, 0, Bitmap.Width, Bitmap.Height);
      *)
    end;

    with Canvas do
    begin
      CopyMode := cmSrcCopy;
      Draw(0, 0, BitMap);
    end;

  finally
    Bitmap.Free;
  end;
end;

procedure TChartFormGS.DrawLabels;
var
  PosX, PosY: Integer;
  R: TRect;
  S: String;
begin
  with Canvas do
  begin
    Brush.Style := bsSolid;
    Brush.Color := ClBtnFace;
    PosX := ChartPaintBox.Left - 55;
    PosY := ChartPaintBox.Top - 24;
    R := Rect(PosX, PosY, PosX+210, PosY+Font.Height);
    SetTextAlign(Handle, TA_LEFT or TA_TOP);
    TextRect(R, PosX, PosY, YTitle);

    PosX := ChartPaintBox.Left - 8;
    PosY := ChartPaintBox.Top;
    R := Rect(PosX-60, PosY, PosX, PosY+Font.Height);
    SetTextAlign(Handle, TA_RIGHT or TA_TOP);
    S := IntToStr(Round(Ymax));
    TextRect(R, PosX, PosY, S);

    PosX := ChartPaintBox.Left - 8;
    PosY := ChartPaintBox.Top + ChartPaintBox.Height;
    R := Rect(PosX-60, PosY-Font.Height, PosX, PosY);
    SetTextAlign(Handle, TA_RIGHT or TA_BOTTOM);
    S := IntToStr(Round(Ymin));
    TextRect(R, PosX, PosY, S);
  end;
end;

procedure TChartFormGS.FormPaint(Sender: TObject);
begin
  inherited;
  { direkt auf den Canvas des Formulars zeichnen }
  DrawLabels;
end;

procedure TChartFormGS.ChartPaintBoxPaint(Sender: TObject);
var
  tempParamCount: Integer;
begin
  if ShowGroup then
  begin
    tempParamCount := ParamCount;
    ParamCount := GroupKurvenzahl;
    DrawToChart;
    ParamCount := tempParamCount;
  end
  else
    DrawToChart;
end;

procedure TChartFormGS.PaintBoxLegendPaint(Sender: TObject);
var
  tempParamCount: Integer;
  tempPText: TYAchseStringArray;
begin
  if ShowGroup then
  begin
    tempParamCount := ParamCount;
    tempPText := PText;
    ParamCount := GroupKurvenzahl;
    PText := GroupText;
    DoLegend;
    ParamCount := tempParamCount;
    PText := tempPText;
  end
  else
    DoLegend;
end;

procedure TChartFormGS.RectangleItemClick(Sender: TObject);
begin
  WantRectangles := not WantRectangles;
  RectangleItem.Checked := WantRectangles;
  if ShowGroup then
    ShowTogetherBtnClick(Self)
  else
    DrawInternal;
end;

procedure TChartFormGS.InitMenu;
var
  p: TMenuItem;
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
  end;

begin
  inherited;

  p := ChartMenu;

  N4 := AddI('N4');
  mi.Caption := '-';
  mi.GroupIndex := 3;

  RectangleItem := AddI('RectangleItem');
  mi.Caption := 'Rechtecke';
  mi.Checked := WantRectangles;
  mi.GroupIndex := 3;
  mi.Hint := '  Rechtecke anzeigen';
  mi.OnClick := RectangleItemClick;

{
  inherited MainMenu: TMainMenu
    Left = 168
    Top = 200
    inherited ChartMenu: TMenuItem
      inherited UpdateChartItem: TMenuItem
        GroupIndex = 3
        Hint = '  Aktuelle Werte von Rigg einlesen'
      end
      inherited UpdateRiggItem: TMenuItem
        GroupIndex = 3
        Hint = '  Erzeugungsdaten zur'#252'ckschreiben'
      end
      inherited N1: TMenuItem
        GroupIndex = 3
      end
      inherited APItem: TMenuItem
        GroupIndex = 3
      end
      inherited BereichItem: TMenuItem
        GroupIndex = 3
      end
      inherited N2: TMenuItem
        GroupIndex = 3
      end
      inherited AuswahlItem: TMenuItem
        GroupIndex = 3
      end
      inherited MemoItem: TMenuItem
        GroupIndex = 3
        Hint = '  Einstellungen f'#252'r das gezeigte Diagramm'
      end
      inherited TogetherItem: TMenuItem
        GroupIndex = 3
      end
      inherited N3: TMenuItem
        GroupIndex = 3
      end
      inherited OpenItem: TMenuItem
        GroupIndex = 3
        Hint = '  Diagramm '#246'ffnen'
      end
      inherited SaveItem: TMenuItem
        GroupIndex = 3
      end
      object N4: TMenuItem
        Caption = '-'
        GroupIndex = 3
      end
      object RectangleItem: TMenuItem
        Caption = 'Rechtecke'
        Checked = True
        GroupIndex = 3
        Hint = '  Rechtecke anzeigen'
        OnClick = RectangleItemClick
      end
    end
  end
}
end;

end.

