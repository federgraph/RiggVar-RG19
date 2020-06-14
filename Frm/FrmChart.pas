unit FrmChart;

interface

{$ifdef fpc}
{$mode delphi}
{$endif}

{$define RG19}

uses
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  System.Types,
  System.UITypes,
  System.Math,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  Vcl.StdCtrls,
  Vcl.Menus,
  Vcl.Buttons,
  Vcl.ComCtrls,
  RggUnit4,
  RggTypes,
  RggDoc,
  RggChart,
  RggSaling3Eck;

type
  TRggChartModel = class;

  TChartForm = class(TForm)
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    BedienPanel: TPanel;
    YCombo: TComboBox;
    YComboLabel: TLabel;
    YMinEdit: TEdit;
    YMinLabel: TLabel;
    YMaxEdit: TEdit;
    YMaxLabel: TLabel;
    YLED: TShape;
    BevelY: TBevel;
    PEdit: TEdit;
    PSpinner: TUpDown;
    PCountLabel: TLabel;
    KurvenzahlEdit: TEdit;
    KurvenZahlSpinner: TUpDown;
    KurvenZahlLabel: TLabel;
    APEdit: TEdit;
    APSpinner: TUpDown;
    APLabel: TLabel;
    CalcBtn: TSpeedButton;
    ResetBtn: TSpeedButton;
    BereichBtn: TSpeedButton;
    APBtn: TSpeedButton;
    AuswahlBtn: TSpeedButton;
    MemoBtn: TSpeedButton;
    ShowTogetherBtn: TSpeedButton;
    BevelCtrls: TBevel;
    XCombo: TComboBox;
    XComboLabel: TLabel;
    XMinEdit: TEdit;
    XMinLabel: TLabel;
    XMaxEdit: TEdit;
    XMaxLabel: TLabel;
    XLED: TShape;
    XBevel: TBevel;
    PCombo: TComboBox;
    PComboLabel: TLabel;
    PMinEdit: TEdit;
    PMinLabel: TLabel;
    PMaxEdit: TEdit;
    PMaxLabel: TLabel;
    PLED: TShape;
    PBevel: TBevel;
    lbXLeft: TLabel;
    lbAchseX: TLabel;
    lbXRight: TLabel;
    ChartPaintBox: TPaintBox;
    PaintBoxLegend: TPaintBox;
    ChartBevelInner: TBevel;
    lbParam: TLabel;
    ChartBevelOuter: TBevel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormPaint(Sender: TObject);
    procedure CalcItemClick(Sender: TObject);
    procedure ResetItemClick(Sender: TObject);
    procedure CloseItemClick(Sender: TObject);
    procedure ChartMenuClick(Sender: TObject);
    procedure APItemClick(Sender: TObject);
    procedure BereichItemClick(Sender: TObject);
    procedure OpenItemClick(Sender: TObject);
    procedure SaveItemClick(Sender: TObject);
    procedure UpdateRiggItemClick(Sender: TObject);
    procedure RectangleItemClick(Sender: TObject);
    procedure MemoItemClick(Sender: TObject);
    procedure ShowTogetherBtnClick(Sender: TObject);
    procedure APSpinnerClick(Sender: TObject; Button: TUDBtnType);
    procedure PSpinnerClick(Sender: TObject; Button: TUDBtnType);
    procedure KurvenZahlSpinnerClick(Sender: TObject; Button: TUDBtnType);
    procedure UpdateChartItemClick(Sender: TObject);
    procedure BereichBtnClick(Sender: TObject);
    procedure ChartPaintBoxPaint(Sender: TObject);
    procedure PaintBoxLegendPaint(Sender: TObject);
    procedure XComboChange(Sender: TObject);
    procedure PComboChange(Sender: TObject);
    procedure YComboChange(Sender: TObject);
    procedure YAuswahlClick(Sender: TObject);
    procedure XMinEditChange(Sender: TObject);
    procedure XMaxEditChange(Sender: TObject);
    procedure PMinEditChange(Sender: TObject);
    procedure PMaxEditChange(Sender: TObject);
  private
    MemoForm: TForm;
    MemoFormMemo: TMemo;
    MainMenu: TMainMenu;
    ChartMenu: TMenuItem;
    BerechnenItem: TMenuItem;
    AuswahlItem: TMenuItem;
    ResetItem: TMenuItem;
    APItem: TMenuItem;
    BereichItem: TMenuItem;
    OpenItem: TMenuItem;
    SaveItem: TMenuItem;
    MemoItem: TMenuItem;
    TogetherItem: TMenuItem;
    UpdateRiggItem: TMenuItem;
    UpdateChartItem: TMenuItem;
    RectangleItem: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    N3: TMenuItem;
    N4: TMenuItem;
  private
    FLegend: Boolean;
    WantChartPunktX: Boolean;
    FScale: single;
    procedure PaintBackGround(Image: TBitMap);
    procedure DrawChartPaintBox(Canvas: TCanvas; Rect: TRect);
    procedure DrawLegend(Canvas: TCanvas; Rect: TRect);
    procedure DrawLabels;
    procedure DoLegend;
    procedure DrawToChart;
    procedure Draw;
    procedure UpdateUI;
    procedure UpdateCombos;
    procedure UpdateLEDs;
    procedure UpdateSpinners;
    procedure UpdateEdits;
    procedure UpdateXPEdits;
    procedure UpdateYEdits;
    procedure SetAP(const Value: Boolean);
    property AP: Boolean write SetAP;
    function ValidateInput(Input: TEdit): Boolean;
  protected
    procedure InitMenu;
  public
    ChartModel: TRggChartModel;
    WantAutoUpdate: Boolean;
    function CheckBeforeCalc: Boolean;
  end;

  TRggChartModel = class(TChartModel)
  private
    FSalingTyp: TSalingTyp;
    procedure SetSalingTyp(Value: TSalingTyp);
  protected
    procedure UpdateXMinMax; override;
    procedure UpdatePMinMax; override;
  public
    FLegend: Boolean;
    BereichBtnDown: Boolean;
    APBtnDown: Boolean;
    IsUp: Boolean;

    constructor Create;
    destructor Destroy; override;

    function CheckBeforeCalc: Boolean; override;
    procedure Draw; override;
    procedure TakeOver; override;

    procedure YAuswahlClick;
    procedure RebuildYCombo;

    procedure UpdateRiggItemClick;
    procedure OpenItemClick(AFileName: string);
    procedure SaveItemClick(AFileName: string);
    procedure LoadFromFile(FileName: string);
    procedure LoadFromStream(S: TStream);
    procedure SaveToFile(FileName: string);
    procedure SaveToStream(S: TStream);

    property SalingTyp: TSalingTyp read FSalingTyp write SetSalingTyp;
  end;

var
  ChartForm: TChartForm;

implementation

{$R *.DFM}

uses
{$ifdef RG19}
  RggModul,
{$endif}
  RiggVar.App.Main,
  RiggVar.RG.Def,
  RiggVar.FB.ActionConst,
  RiggVar.FB.Classes,
  RggCalc,
  RggScroll,
  FrmAuswahl;

procedure TChartForm.FormCreate(Sender: TObject);
begin
  ChartForm := self; { wird schon in AchsForm.Create benötigt }
  HorzScrollBar.Position := 0;

  ChartModel := TRggChartModel.Create;
  Caption := IntToStr(ChartModel.KurvenZahlSpinnerValue);

  APSpinner.Position := ChartModel.APWidth;
  APBtn.Down := True;
  BereichBtn.Down := False;
  KurvenZahlSpinner.Position := ChartModel.ParamCount;

  ClientWidth := 800;
  ClientHeight := 478;

  ChartBevelInner.Left := ChartPaintBox.Left - 1;
  ChartBevelInner.Top := ChartPaintBox.Top - 1;
  ChartBevelInner.Width := ChartPaintBox.Width + 2;
  ChartBevelInner.Height := ChartPaintBox.Height + 2;

{$ifdef RG19}
  if RiggModul.RG19A then
  begin
    FormStyle := fsMDIChild;
    InitMenu;
  end;
{$endif}

  FScale := 1.0;
  WantChartPunktX := True;

  UpdateUI;
  WantAutoUpdate := True;
end;

procedure TChartForm.FormDestroy(Sender: TObject);
begin
  ChartModel.Free;
end;

procedure TChartForm.Draw;
begin
  DoLegend;
  DrawToChart;
end;

procedure TChartForm.UpdateUI;
begin
  UpdateCombos;
  UpdateSpinners;
  UpdateLEDs;
  UpdateEdits;
end;

procedure TChartForm.UpdateCombos;
begin
  XCombo.Items := ChartModel.XComboItems;
  PCombo.Items := ChartModel.PComboItems;
  YCombo.Items := ChartModel.YComboItems;

  XCombo.ItemIndex := ChartModel.XComboItemIndex;
  PCombo.ItemIndex := ChartModel.PComboItemIndex;
  YCombo.ItemIndex := ChartModel.YComboItemIndex;
end;

procedure TChartForm.UpdateSpinners;
begin
  APSpinner.Position := ChartModel.APSpinnerValue;
  PSpinner.Position := ChartModel.PSpinnerValue;
  KurvenZahlSpinner.Position := ChartModel.KurvenZahlSpinnerValue;
end;

procedure TChartForm.UpdateLEDs;
begin
  XLED.Brush.Color := ChartModel.XLEDFillColor;
  PLED.Brush.Color := ChartModel.PLEDFillColor;
  YLED.Brush.Color := ChartModel.YLEDFillColor;
end;

procedure TChartForm.UpdateEdits;
begin
  UpdateXPEdits;
  UpdateYEdits;
end;

procedure TChartForm.UpdateXPEdits;
begin
  XMinEdit.Text := ChartModel.XminEditText;
  XMaxEdit.Text := ChartModel.XmaxEditText;

  PMinEdit.Text := ChartModel.PminEditText;
  PMaxEdit.Text := ChartModel.PmaxEditText;
end;

procedure TChartForm.UpdateYEdits;
begin
  YMinEdit.Text := ChartModel.YminEditText;
  YMaxEdit.Text := ChartModel.YmaxEditText;
end;

procedure TChartForm.KurvenZahlSpinnerClick(Sender: TObject; Button: TUDBtnType);
begin
  if ChartModel.FShowGroup and (KurvenZahlSpinner.Position > ChartModel.ParamCount) then
    Exit;

  ChartModel.KurvenZahlSpinnerValue := KurvenZahlSpinner.Position;

  if ChartModel.ShowGroup then
  begin
    ChartModel.DrawTogether;
    Exit;
  end;

  ChartModel.UserSelectedKurvenZahl := ChartModel.KurvenZahlSpinnerValue;
  if WantAutoUpdate then
  begin
    CalcItemClick(nil);
  end;
end;

procedure TChartForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
{$ifdef RG19}
  RiggModul.ViewModelMain.HideDiagramm;
  RiggModul.ChartFormActive := False;
  Action := caFree;
{$endif}
end;

procedure TChartForm.CloseItemClick(Sender: TObject);
begin
  Close;
end;

procedure TChartForm.FormActivate(Sender: TObject);
begin
  ChartModel.TakeOver;
end;

procedure TChartForm.UpdateChartItemClick(Sender: TObject);
begin
  ChartModel.TakeOver;
end;

procedure TChartForm.CalcItemClick(Sender: TObject);
begin
  ChartModel.ShowGroup := False;
  ChartModel.KurvenZahlSpinnerValue := KurvenZahlSpinner.Position;

  ChartModel.Calc;

  PSpinner.Position := ChartModel.PSpinnerValue;
  PSpinner.Update;

  UpdateLEDs;
  UpdateEdits;

  if not WantAutoUpdate then
  begin
    ActiveControl := YCombo;
  end;
end;

procedure TChartForm.ResetItemClick(Sender: TObject);
begin
  ChartModel.Reset;
  UpdateUI;
end;

procedure TChartForm.ShowTogetherBtnClick(Sender: TObject);
begin
  if not ChartModel.ShowGroup then
  begin
    ChartModel.KurvenZahlSpinnerValue := KurvenZahlSpinner.Position;
    ChartModel.ShowGroup := True;
    ChartModel.DrawTogether;
  end
  else
  begin
     { this will reset ChartModel.ShowGroup and restore KurvenZahlSpinner.Position }
    YComboChange(nil);
  end;
end;

procedure TChartForm.YAuswahlClick(Sender: TObject);
begin
  ChartModel.YAuswahlClick;
end;

procedure TChartForm.SetAP(const Value: Boolean);
begin
  ChartModel.BereichBtnDown := BereichBtn.Down;
  ChartModel.APBtnDown := APBtn.Down;
  ChartModel.AP := Value;
  UpdateXPEdits;
  UpdateLEDS;
end;

procedure TChartForm.BereichBtnClick(Sender: TObject);
begin
  if Sender = BereichBtn then
  begin
    AP := False;
  end
  else if Sender = APBtn then
  begin
    AP := True;
  end;
end;

procedure TChartForm.YComboChange(Sender: TObject);
begin
  ChartModel.YComboItemIndex := YCombo.ItemIndex;
  ChartModel.YComboChange(Sender);
  UpdateYEdits;
  ChartModel.KurvenZahlSpinnerValue := ChartModel.UserSelectedKurvenZahl;
  KurvenZahlSpinner.Position := ChartModel.KurvenZahlSpinnerValue;
end;

procedure TChartForm.APSpinnerClick(Sender: TObject; Button: TUDBtnType);
begin
  ChartModel.APWidth := APSpinner.Position;
  ChartModel.AP := ChartModel.AP; // trigger update
  UpdateXPEdits;
  UpdateLEDs;

  if ChartModel.BereichBtnDown then
  begin
    { Calc not needed, just redraw }
    DrawToChart;
  end
  else if WantAutoUpdate then
  begin
    CalcItemClick(nil);
  end;
end;

procedure TChartForm.DoLegend;
var
  R: TRect;
begin
  FLegend := ChartModel.ParamCount > 1;
  if FLegend then
    DrawLegend(PaintBoxLegend.Canvas, PaintBoxLegend.BoundsRect)
  else
    with PaintBoxLegend do
    begin
      R := Rect(0,0,Width,Height);
      Canvas.Brush.Color := clBtnFace;
      Canvas.FillRect(R);
    end;
end;

procedure TChartForm.DrawToChart;
begin
  DrawChartPaintBox(ChartPaintBox.Canvas, ChartPaintBox.BoundsRect);
end;

procedure TChartForm.DrawLegend(Canvas: TCanvas; Rect: TRect);
var
  Bitmap: TBitmap;
  p, PosX, PosY: Integer;
begin
  Bitmap := TBitmap.Create;
  with Bitmap do
  begin
    Width := Rect.Right-Rect.Left;
    Height := Rect.Bottom-Rect.Top;
  end;
  try
    PaintBackGround(Bitmap);
    with Bitmap.Canvas do
    begin
      PosX := 0;
      PosY := 0;
      for p := 0 to ChartModel.ParamCount - 1 do
      begin
        { Bullet }
        Pen.Color := clBlack;
        Brush.Color := ChartModel.cf[p];
        Brush.Style := bsSolid;
        PosY := PosY + 30;
        Rectangle( PosX, PosY, PosX + 10, PosY + 5);
        { Text }
        Brush.Style := bsClear;
        PosY := PosY + 10;
        if ChartModel.Valid then
          TextOut(PosX, PosY, ChartModel.PText[p])
        else
          TextOut(PosX, PosY, ChartModel.PColorText[p]);
      end;
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

procedure TChartForm.DrawChartPaintBox(Canvas: TCanvas; Rect: TRect);
var
  P: TPoint;
  R: TRect;
  i, param: Integer;
  RadiusX, RadiusY: Integer;
  Bitmap: TBitmap;
  PlotWidth, PlotHeight: Integer;
  PlotExtX, PlotExtY: Integer;
  PlotOrgX, PlotOrgY: Integer;
  tempX, tempY: double;

  xrange: single;
  yrange: single;

  P0: TPointF;
  P1: TPoint;
  P2: TPoint;

  function Limit(a: double): double;
  begin
    if a < -32000 then
      a := -32000
    else if a > 32000 then
      a := 32000;
    Result := a;
  end;

  procedure DrawVerticalLine(g: TCanvas);
  begin
    P0.X := Limit(tempX);
    P0.Y := Limit(tempY);
    P2.X := Round(P0.X * FScale);
    P2.Y := Round(P0.Y * FScale);
    P1 := P2;
    P1.Y := 0;
    g.MoveTo(P1.X, P1.Y);
    g.LineTo(P2.X, P2.Y);
  end;

begin
  DrawLabels;

  lbAchseX.Caption := ChartModel.XTitle;
  lbXLeft.Caption := IntToStr(Round(ChartModel.Xmin));
  lbXRight.Caption := IntToStr(Round(ChartModel.Xmax));
  lbParam.Caption := 'Parameter ' + ChartModel.PTitle;

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

      { ChartPunktX }
      WantChartPunktX := not ChartModel.AP;
      if WantChartPunktX then
      begin
        xrange := ChartModel.Xmax - ChartModel.Xmin;

        Pen.Color := clRed;
        tempX := PlotExtX * ((ChartModel.ChartPunktX) - ChartModel.Xmin) / xrange;
        tempY := PlotExtY;
        DrawVerticalLine(Bitmap.Canvas);

        Pen.Color := clSilver;
        tempX := PlotExtX * ((ChartModel.ChartPunktX - ChartModel.APWidth) - ChartModel.Xmin) / xrange;
        tempY := PlotExtY;
        DrawVerticalLine(Bitmap.Canvas);

        tempX := PlotExtX * ((ChartModel.ChartPunktX + ChartModel.APWidth) - ChartModel.Xmin) / xrange;
        tempY := PlotExtY;
        DrawVerticalLine(Bitmap.Canvas);
      end;

      { Radius }
      R.Left := 0;
      R.Top := 0;
      R.Bottom := 3;
      R.Right := 3;
      DPTOLP(Handle, R, 2);
      RadiusX := R.Right-R.Left;
      RadiusY := R.Bottom-R.Top;

      yrange := (ChartModel.Ymax-ChartModel.Ymin);
      for param := 0 to ChartModel.ParamCount - 1 do
      begin
        { Kurve }
        Pen.Color := ChartModel.cf[param];
        tempY := PlotExtY * (ChartModel.bf[param,0] - ChartModel.Ymin) / yrange;
        P.Y := Round(Limit(tempY));
        MoveTo(0, P.Y);
        for i := 1 to LNr do
        begin
          tempX := PlotExtX * (i/LNr);
          tempY := PlotExtY * (ChartModel.bf[param,i] - ChartModel.Ymin) / yrange;
          P.X := Round(Limit(tempX));
          P.Y := Round(Limit(tempY));
          LineTo(P.X, P.Y);
        end;

        if ChartModel.WantRectangles then
        begin
          { Rechtecke }
          Pen.Color := clBlack;
          Brush.Color := ChartModel.cf[param];
          Brush.Style := bsSolid;
          for i := 0 to LNr do
          begin
            tempX := PlotExtX * (i/LNr);
            tempY := PlotExtY * (ChartModel.bf[param,i]-ChartModel.Ymin) / yrange;
            P.X := Round(Limit(tempX));
            P.Y := Round(Limit(tempY));
            Rectangle( P.X - RadiusX, P.Y - RadiusY,
                       P.X + RadiusX, P.Y + RadiusY);
          end;
        end;

      end;

      SetMapMode(Handle, MM_TEXT);
      SetWindowOrgEx(Handle, 0, 0, nil);
      SetViewPortOrgEx(Handle, 0, 0, nil);
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

procedure TChartForm.DrawLabels;
var
  PosX, PosY: Integer;
  R: TRect;
  s: string;
begin
  with Canvas do
  begin
    Font.Height := 13;
    Pen.Color := clBlack;
    Brush.Style := bsSolid;
    Brush.Color := clBtnFace;
    PosX := ChartPaintBox.Left - 55;
    PosY := ChartPaintBox.Top - 24;
    R := Rect(PosX, PosY, PosX + 210, PosY + Font.Height);
    SetTextAlign(Handle, TA_LEFT or TA_TOP);
    TextRect(R, PosX, PosY, ChartModel.YTitle);

    PosX := ChartPaintBox.Left - 8;
    PosY := ChartPaintBox.Top;
    R := Rect(PosX - 60, PosY, PosX, PosY + Font.Height);
    SetTextAlign(Handle, TA_RIGHT or TA_TOP);
    s := IntToStr(Round(ChartModel.Ymax));
    TextRect(R, PosX, PosY, s);

    PosX := ChartPaintBox.Left - 8;
    PosY := ChartPaintBox.Top + ChartPaintBox.Height;
    R := Rect(PosX - 60, PosY-Font.Height, PosX, PosY);
    SetTextAlign(Handle, TA_RIGHT or TA_BOTTOM);
    s := IntToStr(Round(ChartModel.Ymin));
    TextRect(R, PosX, PosY, s);
  end;
end;

procedure TChartForm.FormPaint(Sender: TObject);
begin
  inherited;
  DrawLabels;
end;

procedure TChartForm.ChartPaintBoxPaint(Sender: TObject);
var
  tempParamCount: Integer;
begin
  if ChartModel.ShowGroup then
  begin
    tempParamCount := ChartModel.ParamCount;
    ChartModel.ParamCount := ChartModel.GroupKurvenzahl;
    DrawToChart;
    ChartModel.ParamCount := tempParamCount;
  end
  else
    DrawToChart;
end;

procedure TChartForm.PaintBoxLegendPaint(Sender: TObject);
var
  tempParamCount: Integer;
  tempPText: TYAchseStringArray;
begin
  if ChartModel.ShowGroup then
  begin
    tempParamCount := ChartModel.ParamCount;
    tempPText := ChartModel.PText;
    ChartModel.ParamCount := ChartModel.GroupKurvenzahl;
    ChartModel.PText := ChartModel.GroupText;
    DoLegend;
    ChartModel.ParamCount := tempParamCount;
    ChartModel.PText := tempPText;
  end
  else
    DoLegend;
end;

procedure TChartForm.PComboChange(Sender: TObject);
begin
  ChartModel.PComboItemIndex := PCombo.ItemIndex;
  ChartModel.PComboChange(Sender);
  UpdateXPEdits;

  if WantAutoUpdate then
  begin
    CalcItemClick(nil);
  end;
end;

procedure TChartForm.PaintBackGround(Image: TBitMap);
var
  R: TRect;
begin
  R := Rect(0, 0, Image.Width, Image.Height);
  with Image.Canvas do
  begin
    Brush.Color := clBtnFace;
    FillRect(R);
  end;
end;

function  TChartForm.CheckBeforeCalc: Boolean;
begin
  result := True;
  if (XMinEdit.Text = XMinEditString) or (XMaxEdit.Text = XMaxEditString) then
    ChartModel.UpdateXMinMax;
  if (PMinEdit.Text = PMinEditString) or (PMaxEdit.Text = PMaxEditString) then
    ChartModel.UpdatePMinMax;
  if not ValidateInput(XMinEdit) then result := False;
  if not ValidateInput(XMaxEdit) then result := False;
  if not ValidateInput(PMinEdit) then result := False;
  if not ValidateInput(PMaxEdit) then result := False;
end;

procedure TChartForm.PSpinnerClick(Sender: TObject; Button: TUDBtnType);
begin
  ChartModel.PSpinnerValue := PSpinner.Position;
  ChartModel.UpdateYMinMax;
  YMinEdit.Text := ChartModel.YminEditText;
  YMaxEdit.Text := ChartModel.YmaxEditText;
end;

procedure TChartForm.XMinEditChange(Sender: TObject);
begin
  ChartModel.XminEditText := XMinEdit.Text;
end;

procedure TChartForm.XMaxEditChange(Sender: TObject);
begin
  ChartModel.XmaxEditText := XMaxEdit.Text;
end;

procedure TChartForm.PMinEditChange(Sender: TObject);
begin
  ChartModel.PminEditText := PMinEdit.Text;
end;

procedure TChartForm.PMaxEditChange(Sender: TObject);
begin
  ChartModel.PmaxEditText := PMaxEdit.Text;
end;

procedure TChartForm.MemoItemClick(Sender: TObject);
begin
  if MemoForm = nil then
  begin
    MemoForm := TForm.Create(Application);
    MemoForm.Width := 400;
    MemoForm.Height := 600;
    MemoForm.Caption := 'ChartMemoForm01';

    MemoFormMemo := TMemo.Create(MemoForm);
    MemoFormMemo.Parent := MemoForm;
    MemoFormMemo.Align := alClient;
    MemoFormMemo.ScrollBars := ssBoth;
  end;
  MemoFormMemo.Lines.Clear;
  MemoFormMemo.Lines := ChartModel.MemoLines;
  MemoForm.ShowModal;
end;

procedure TChartForm.XComboChange(Sender: TObject);
begin
  ChartModel.XComboItemIndex := XCombo.ItemIndex;
  ChartModel.XComboChange(Sender);

  PCombo.Items := ChartModel.PComboItems;
  PCombo.ItemIndex := ChartModel.PComboItemIndex;

  UpdateXPEdits;

  if WantAutoUpdate then
  begin
    CalcItemClick(nil);
  end;
end;

procedure TChartForm.OpenItemClick(Sender: TObject);
begin
  if not OpenDialog.Execute then
    Exit;
end;

procedure TChartForm.SaveItemClick(Sender: TObject);
begin
  if not SaveDialog.Execute then
    Exit;
  ChartModel.SaveToFile(SaveDialog.FileName);
end;

procedure TChartForm.RectangleItemClick(Sender: TObject);
begin
  ChartModel.WantRectangles := not ChartModel.WantRectangles;
  RectangleItem.Checked := ChartModel.WantRectangles;
  if ChartModel.ShowGroup then
    ShowTogetherBtnClick(Self)
  else
    ChartModel.DrawInternal;
end;

procedure TChartForm.BereichItemClick(Sender: TObject);
begin
  BereichItem.Checked  := not BereichItem.Checked;
  BereichBtn.Down := BereichItem.Checked;
  AP := not BereichBtn.Down;
end;

procedure TChartForm.APItemClick(Sender: TObject);
begin
  APItem.Checked := not APItem.Checked;
  APBtn.Down := APItem.Checked;
  AP := APBtn.Down;
end;

procedure TChartForm.ChartMenuClick(Sender: TObject);
begin
  APItem.Checked := ChartModel.AP;
  BereichItem.Checked := not ChartModel.AP;
end;

procedure TChartForm.UpdateRiggItemClick(Sender: TObject);
begin
  ChartModel.UpdateRiggItemClick;
end;

procedure TChartForm.InitMenu;
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
  MainMenu := TMainMenu.Create(Self);

  ChartMenu := AddP('ChartMenu');
  mi.Caption := 'Diagram&m';
  mi.GroupIndex := 8;
  mi.Hint := '  Diagramm Optionen';
  mi.OnClick := ChartMenuClick;

  BerechnenItem := AddI('BerechnenItem');
  mi.Caption := '&Berechnen...';
  mi.Hint := '  Berechnung starten';
  mi.OnClick := CalcItemClick;

  ResetItem := AddI('ResetItem');
  mi.Caption := '&Zurücksetzen';
  mi.Hint := '  Diagramm für Neuberechnung freigeben (nach Fehler)';
  mi.OnClick := ResetItemClick;

  UpdateChartItem := AddI('UpdateChartItem');
  mi.Caption := 'Diagramm aktualisieren';
  mi.Hint := '  Istwerte neu einlesen';
  mi.OnClick := UpdateChartItemClick;

  UpdateRiggItem := AddI('UpdateRiggItem');
  mi.Caption := 'Rigg aktualisieren';
  mi.Hint := 'Erzeugungsdaten zurückschreiben';
  mi.OnClick := UpdateRiggItemClick;

  N1 := AddI('N1');
  mi.Caption := '-';

  APItem := AddI('APItem');
  mi.Caption := 'Arbeits&punkt';
  mi.Checked := True;
  mi.Hint := '  automatische X - Werte: Arbeitspunkt +/- 30';
  mi.OnClick := APItemClick;

  BereichItem := AddI('BereichItem');
  mi.Caption := 'Be&reich';
  mi.Hint := '  automatische X - Werte: gesamter Bereich';
  mi.OnClick := BereichItemClick;

  N2 := AddI('N2');
  mi.Caption := '-';

  AuswahlItem := AddI('AuswahlItem');
  mi.Caption := '&Auswahl Y ...';
  mi.Hint := '  Auswahl der Größen für die Y-Achse';
  mi.OnClick := YAuswahlClick;

  MemoItem := AddI('MemoItem');
  mi.Caption := 'Erzeugungsdaten...';
  mi.Hint := '  Erzeugungsdaten anzeigen';
  mi.OnClick := MemoItemClick;

  TogetherItem := AddI('TogetherItem');
  mi.Caption := '&Gruppiert anzeigen';
  mi.Hint := '  Kurven in einem Diagramm anzeigen';
  mi.OnClick := ShowTogetherBtnClick;

  N3 := AddI('N3');
  mi.Caption := '-';

  OpenItem := AddI('OpenItem');
  mi.Caption := '&Öffnen...';
  mi.Hint := '  gespeichertes Diagramm laden';
  mi.OnClick := OpenItemClick;

  SaveItem := AddI('SaveItem');
  mi.Caption := '&Speichern...';
  mi.Hint := '  Diagramm speichern';
  mi.OnClick := SaveItemClick;

  p := ChartMenu;

  N4 := AddI('N4');
  mi.Caption := '-';
  mi.GroupIndex := 3;

  RectangleItem := AddI('RectangleItem');
  mi.Caption := 'Rechtecke';
  mi.Checked := ChartModel.WantRectangles;
  mi.GroupIndex := 3;
  mi.Hint := '  Rechtecke anzeigen';
  mi.OnClick := RectangleItemClick;

end;

{ TRggChartModel }

constructor TRggChartModel.Create;
begin
  WantRectangles := True;
  FLegend := True;

  BereichBtnDown := False;
  APBtnDown := True;

  inherited;

  IsUp := True;
end;

destructor TRggChartModel.Destroy;
begin
  RggDocument.Free;
  MemoLines.Free;
  SalingDreieck.Free;
  XComboItems.Free;
  YComboItems.Free;
  PComboItems.Free;
end;

procedure TRggChartModel.Draw;
begin
  if IsUp then
    ChartForm.Draw;
end;

procedure TRggChartModel.UpdateXMinMax;
var
  s: string;
  xp: TxpName;
  tempMin, tempMax, tempIst, Minimum, Maximum: Integer;
  f: TRggSB;
begin
  s := XComboSelectedText;
  xp := GetTsbName(s);
  if not (xp in XSet) then
    Exit;

  if (SalingTyp = stFest) and (xp = xpSalingL) then
  begin
    SalingDreieck.CopyFromRigg(Rigg);
    tempMin := Ceil(SalingDreieck.Saling_LMin);
    tempMax := Floor(SalingDreieck.Saling_LMax);
    tempIst := Round(SalingDreieck.Saling_L);
  end
  else if xp = xpSalingW then
  begin
    SalingDreieck.CopyFromRigg(Rigg);
    tempMin := Ceil(SalingDreieck.Saling_WMin * D180);
    tempMax := Floor(SalingDreieck.Saling_WMax * D180);
    tempIst := Round(SalingDreieck.Saling_W * D180);
  end
  else
  begin
    f := Rigg.GSB.GetSB(TsbName(xp));
    tempMin := Round(f.Min);
    tempMax := Round(f.Max);
    tempIst := Round(f.Ist);
  end;

  try
    if BereichBtnDown then
    begin
      Minimum := tempMin;
      Maximum := tempMax;
    end
    else if APBtnDown then
    begin
      Minimum := tempMin;
      Maximum := tempMax;
      if (tempIst - APWidth) > tempMin then
        Minimum := tempIst - APWidth;
      if (tempIst + APWidth) < tempMax then
        Maximum := tempIst + APWidth;
    end
    else
    begin
      try
        Minimum := StrToInt(XMinEditText);
        Maximum := StrToInt(XMaxEditText);
        if Minimum < tempMin then Minimum := tempMin;
        if Maximum > tempMax then Maximum := tempMax;
        if Minimum > Maximum then Minimum := tempMin;
        if Maximum < Minimum then Maximum := tempMax;
      except
        on EConvertError do
        begin
          Minimum := tempMin;
          Maximum := tempMax;
        end;
      end;
    end;
    XMinEditText := IntToStr(Minimum);
    XMaxEditText := IntToStr(Maximum);
  except
    on ERangeError do
    begin
      XMinEditText := IntToStr(0);
      XMaxEditText := IntToStr(100);
      Valid := False;
      XLEDFillColor := clRed;
    end;
  end;
end;

procedure TRggChartModel.UpdatePMinMax;
var
  s: string;
  xp: TxpName;
  tempMin, tempMax, tempIst, Minimum, Maximum: Integer;
  f: TRggSB;
begin
  s := PComboSelectedText;
  if s = NoParamString then
  begin
    PMinEditText := IntToStr(0);
    PMaxEditText := IntToStr(0);
    KurvenZahlSpinnerValue := 1;
  end
  else if KurvenZahlSpinnerValue = 1 then
  begin
    KurvenzahlSpinnerValue := UserSelectedKurvenZahl;
  end;

  xp := GetTsbName(s);
  if not (xp in XSet) then
    Exit;

  if (SalingTyp = stFest) and (xp = xpSalingL) then
  begin
    SalingDreieck.CopyFromRigg(Rigg);
    tempMin := Ceil(SalingDreieck.Saling_LMin);
    tempMax := Floor(SalingDreieck.Saling_LMax);
    tempIst := Round(SalingDreieck.Saling_L);
  end
  else if xp = xpSalingW then
  begin
    SalingDreieck.CopyFromRigg(Rigg);
    tempMin := Ceil(SalingDreieck.Saling_WMin * D180);
    tempMax := Floor(SalingDreieck.Saling_WMax * D180);
    tempIst := Round(SalingDreieck.Saling_W * D180);
  end
  else
  begin
    f := Rigg.GSB.GetSB(TsbName(xp));
    tempMin := Round(f.Min);
    tempMax := Round(f.Max);
    tempIst := Round(f.Ist);
  end;

  try
    if BereichBtnDown then
    begin
      Minimum := tempMin;
      Maximum := tempMax;
    end
    else if APBtnDown then
    begin
      Minimum := tempMin;
      Maximum := tempMax;
      if (tempIst - APWidth) > tempMin then
        Minimum := tempIst - APWidth;
      if (tempIst + APWidth) < tempMax then
        Maximum := tempIst + APWidth;
    end
    else
    begin
      try
        Minimum := StrToInt(PMinEditText);
        Maximum := StrToInt(PMaxEditText);
        if Minimum < tempMin then Minimum := tempMin;
        if Maximum > tempMax then Maximum := tempMax;
        if Minimum > Maximum then Minimum := tempMin;
        if Maximum < Minimum then Maximum := tempMax;
      except
        on EConvertError do
        begin
          Minimum := tempMin;
          Maximum := tempMax;
        end;
      end;
    end;
    PMinEditText := IntToStr(Minimum);
    PMaxEditText := IntToStr(Maximum);
  except
    on ERangeError do
    begin
      PMinEditText := IntToStr(0);
      PMaxEditText := IntToStr(100);
      Valid := False;
      PLEDFillColor := clRed;
    end;
  end;
end;

function  TRggChartModel.CheckBeforeCalc: Boolean;
begin
  result := ChartForm.CheckBeforeCalc;
end;

procedure TRggChartModel.RebuildYCombo;
var
  YAV: TYAchseValue;
begin
  YComboItems.Clear;
  for YAV := Low(TYAchseValue) to High(TYAchseValue) do
    if YAV in YAchseSet then
      YComboItems.Add(YAchseRecordList[YAV].ComboText);
  if YComboItems.Count > 0 then YComboItemIndex := 0;
  UpdateYAchseList;
  YAuswahlDlg.DstList.Items := YComboItems;
  YAuswahlDlg.SrcList.Clear;
  for YAV := Low(TYAchseValue) to High(TYAchseValue) do
    if not (YAV in YAchseSet) then
  YAuswahlDlg.SrcList.Items.Add(YAchseRecordList[YAV].ComboText);
end;

procedure TRggChartModel.SetSalingTyp(Value: TSalingTyp);
begin
  if FSalingTyp <> Value then
  begin
    FSalingTyp := Value;
    FValid := False;
    YLEDFillColor := clRed;
    UpdateXCombo(SalingTyp);
  end;
end;

procedure TRggChartModel.YAuswahlClick;
var
  i: Integer;
begin
  if YAuswahlDlg = nil then
  begin
    YAuswahlDlg := TYAuswahlDlg.Create(Application);
  end;
  with YAuswahlDlg do
  begin
    if not (DstList.ItemIndex = -1) then
    begin
      { clear selection if any }
      for i := 0 to DstList.Items.Count-1 do
      begin
        DstList.Selected[i] := False;
      end;
      { In DstList den gleichen Eintrag wie in YComboBox selektieren }
      DstList.ItemIndex := YComboItemIndex;
      DstList.Selected[YComboItemIndex] := True;
    end;

    if ShowModal = mrOK then
    begin
      if (DstList.Items.Count = 0) then
      begin
        { mindestens ein Eintrag muß sich in DestList befinden }
        DstList.Items.AddObject(SrcList.Items[0], SrcList.Items.Objects[0]);
        SrcList.Items.Delete(0);
      end;
      YComboItems := DstList.Items;
      YComboItemIndex := DstList.ItemIndex;
      UpdateYAchseList;
      YComboChange(nil);
    end;
  end;
end;

function TChartForm.ValidateInput(Input: TEdit): Boolean;
var
  s: string;
  I: Integer;
  Code: Integer;
begin
  Result := False;
  try
    Val(Input.Text, I, Code);
    if Code <> 0 then
    begin
      s := Format('''%s'' ist kein gültiger Integerwert', [Input.Text]);
      MessageDlg(s, mtWarning, [mbOK], 0);
      Input.SetFocus;
    end
    else
    begin
      if (I >= 0) and (I < MaxInt) then
      Result := True;
    end;
  except
    on EConvertError do
    begin
      s := Format('''%s'' ist kein gültiger Integerwert', [Input.Text]);
      MessageDlg(s, mtWarning, [mbOK], 0);
      Input.SetFocus;
    end;
  end;
end;

procedure TRggChartModel.LoadFromFile(FileName: string);
var
  S: TFileStream;
begin
  S := TFileStream.Create(FileName, fmOpenRead);
  try
    LoadFromStream(S);
  finally
    S.Free;
  end;
end;

procedure TRggChartModel.SaveToFile(FileName: string);
var
  S: TFileStream;
begin
  S := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(S);
  finally
    S.Free;
  end;
end;

procedure TRggChartModel.SaveToStream(S: TStream);
var
  ParamValue: double;
  param: Integer;
begin
  with S do
  begin
    WriteBuffer(FLegend, SizeOf(Boolean));
    WriteBuffer(XAchseMin, SizeOf(Integer));
    WriteBuffer(XAchseMax, SizeOf(Integer));
    WriteBuffer(ParamCount, SizeOf(Integer));
    WriteBuffer(YAchseSet, SizeOf(YAchseSet));
    WriteBuffer(YAchseRecordList, SizeOf(YAchseRecordList));
    for param := 0 to PNr - 1 do
      WriteBuffer(af[param], SizeOf(TYLineArray));
    for param := 0 to ParamCount - 1 do
    begin
      ParamValue := StrToFloat(PText[param]);
      WriteBuffer(ParamValue, SizeOf(double));
    end;
    RggDocument.SaveToStream(S);
    MemoLines.Add(XComboText);
    MemoLines.Add(PComboText);
    MemoLines.SaveToStream(S);
  end;
end;

procedure TRggChartModel.LoadFromStream(S: TStream);
var
  ParamValue: double;
  param: Integer;
begin
  with S do
  begin
    ReadBuffer(FLegend, SizeOf(Boolean));
    ReadBuffer(XAchseMin, SizeOf(Integer));
    ReadBuffer(XAchseMax, SizeOf(Integer));
    ReadBuffer(ParamCount, SizeOf(Integer));
    ReadBuffer(YAchseSet, SizeOf(YAchseSet));
    ReadBuffer(YAchseRecordList, SizeOf(YAchseRecordList));
    for param := 0 to PNr - 1 do
      ReadBuffer(af[param], SizeOf(TYLineArray));
    for param := 0 to ParamCount - 1 do
    begin
      ReadBuffer(ParamValue, SizeOf(double));
      PText[param] := Format('%6.2f', [ParamValue]);
    end;
    RggDocument.LoadFromStream(S);
    MemoLines.LoadFromStream(S);
    XComboText := MemoLines[MemoLines.Count-2];
    PComboText := MemoLines[MemoLines.Count-1];
    MemoLines.Delete(MemoLines.Count-1);
    MemoLines.Delete(MemoLines.Count-1);
  end;
end;

procedure TRggChartModel.OpenItemClick(AFileName: string);
begin
  LoadFromFile(AFileName);
  Exclude(FStatus, csBerechnet);
  Include(FStatus, csGeladen);
  XAchseText := GetXText(XComboText); { benötigt für BottomTitel }
  ParamText := GetPText(PComboText); { benötigt für RightTitel }
  XLEDFillColor := clRed;
  PLEDFillColor := clRed;
  PSpinnerValue := 1;
  PSpinnerMax := ParamCount;
  if PSpinnerMax = 1 then
    PSpinnerMax := 2;
  KurvenZahlSpinnerValue := ParamCount;
  RebuildYCombo;
  YComboChange(nil);
end;

procedure TRggChartModel.SaveItemClick(AFileName: string);
begin
  SaveToFile(AFileName);
end;

procedure TRggChartModel.TakeOver;
begin
  Rigg.UpdateGSB;
  SalingTyp := Rigg.SalingTyp;
  { ControllerTyp := Rigg.ControllerTyp; }
  { CalcTyp := Rigg.CalcTyp; }
end;

procedure TRggChartModel.UpdateRiggItemClick;
begin
{$ifdef RG19}
  if not Assigned(RggDocument) then
    Exit;
  if (csGeladen in FStatus) or (csBerechnet in FStatus)then
  begin
    RiggModul.Neu(RggDocument);
    RiggModul.ViewModelMain.Caption := 'Rigg';
  end;
{$endif}
end;

end.
