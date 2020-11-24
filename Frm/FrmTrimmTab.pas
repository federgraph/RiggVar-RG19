unit FrmTrimmTab;

{$ifdef fpc}
{$mode delphi}
{$endif}

interface

uses
  System.SysUtils,
  System.Classes,
  System.Types,
  System.UITypes,
  Vcl.Forms,
  Vcl.Controls,
  Vcl.Dialogs,
  Vcl.Graphics,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.ComCtrls,
  Vcl.Buttons,
  Vcl.Menus,
  RiggVar.RG.Model,
  RggTypes,
  RggTrimmTab,
  RggTrimmTabGraph;

type
  TFormTrimmTab = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    OKBtn: TButton;
    CancelBtn: TButton;

    rbKonstante: TRadioButton;
    rbGerade: TRadioButton;
    rbParabel: TRadioButton;
    rbBezier: TRadioButton;

    MemoLabel: TLabel;
    Memo: TMemo;

    Image: TImage;

    X1Label: TLabel;
    Y1Label: TLabel;
    X2Label: TLabel;
    Y2Label: TLabel;

    W1Edit: TEdit;
    K1Edit: TEdit;
    W2Edit: TEdit;
    K2Edit: TEdit;

    W1UpDown: TUpDown;
    K1UpDown: TUpDown;

    ReadMemoBtn: TSpeedButton;
    WriteMemoBtn: TSpeedButton;
    EvalOptionBtn: TSpeedButton;

    procedure OKBtnClick(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure ApplyBtnClick(Sender: TObject);
    procedure CalcBtnClick(Sender: TObject);
    procedure EvalOptionBtnClick(Sender: TObject);
    procedure Kraft1EditChange(Sender: TObject);
    procedure rbKonstanteClick(Sender: TObject);
    procedure ImageMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  protected
    cr: TControl;
    TempR: Integer;
    TempB: Integer;
    FMaxRight: Integer;
    FMaxBottom: Integer;

    Margin: Integer;

    FScale: single;
    function Scale(Value: Integer): Integer;

    procedure RecordMax;
    procedure AnchorVertical(c: TControl);
    procedure StackH(c: TControl);
    procedure StackV(c: TControl);
  private
    FTrimmTabGraph: TTrimmTabGraph;
    FTrimmTabDaten: TTrimmTabDaten;
    FTrimmTabelle: TTrimmTab;
    FTabellenTyp: TTabellenTyp;
    FTabChanging: Boolean;
    procedure DrawTrimmTab;
    procedure CheckTabelle;
    procedure CreateComponents;
    procedure InitComponentLinks;
    procedure InitComponentSize;
    procedure InitComponentProps;
    procedure InitTabOrder;
    procedure LayoutComponents;
  public
    Rigg: TRigg;
    procedure Init(ARigg: TRigg);
  end;

var
  FormTrimmTab: TFormTrimmTab;

implementation

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

uses
  RiggVar.App.Main;

{ TFormTrimmTab }

procedure TFormTrimmTab.FormCreate(Sender: TObject);
begin
  FScale := MainVar.Scale;
  Margin := Scale(10);

  CreateComponents;
  InitComponentSize;
  InitComponentProps;
  InitComponentLinks;
  InitTabOrder;

  LayoutComponents;
  FTrimmTabGraph := TTrimmTabGraph.Create;
  FTrimmTabGraph.Image := Image;
end;

procedure TFormTrimmTab.FormDestroy(Sender: TObject);
begin
  FTrimmTabGraph.Free;
end;

procedure TFormTrimmTab.Init(ARigg: TRigg);
begin
  Rigg := ARigg;

  FTabellenTyp := itGerade;
  rbGerade.Checked := True;
  K1UpDown.Enabled := False;
  W1UpDown.Enabled := False;

  FTrimmTabelle := Rigg.TrimmTabelle;
  FTrimmTabDaten := FTrimmTabelle.TrimmTabDaten;
  CheckTabelle;

  Assert(FTrimmTabelle <> nil);
  FTabellenTyp := FTrimmTabelle.TabellenTyp;
end;

procedure TFormTrimmTab.CheckTabelle;
begin
  { wenn Tabelle außerhalb des Dialoges verändert wurde - d.h. neu eingelesen }
  if FTabellenTyp <> FTrimmTabelle.TabellenTyp then
    case FTrimmTabelle.TabellenTyp of
      { Checked ändern --> Click() wird aufgerufen }
      itKonstante: rbKonstante.Checked := True;
      itGerade: rbGerade.Checked := True;
      itParabel: rbParabel.Checked := True;
      itBezier: rbBezier.Checked := True;
    end;
  FTrimmTabelle.GetMemoLines(Memo.Lines);
  ApplyBtnClick(Self);
end;

procedure TFormTrimmTab.OKBtnClick(Sender: TObject);
begin
  FTabellenTyp := FTrimmTabelle.TabellenTyp;
end;

procedure TFormTrimmTab.CancelBtnClick(Sender: TObject);
begin
  { wenn TabellenTyp verändert wurde }
  if FTabellenTyp <> FTrimmTabelle.TabellenTyp then
    case FTabellenTyp of
      { Checked ändern --> Click() wird aufgerufen }
      itKonstante: rbKonstante.Checked := True;
      itGerade: rbGerade.Checked := True;
      itParabel: rbParabel.Checked := True;
      itBezier: rbBezier.Checked := True;
    end;
  { restore }
  FTrimmTabelle.TrimmTabDaten := FTrimmTabDaten;
end;

procedure TFormTrimmTab.ApplyBtnClick(Sender: TObject);
var
  Temp: TPoint;
begin
  Assert(FTrimmTabelle <> nil);
  FTrimmTabelle.ProcessTrimmTab(Memo.Lines);
  K1UpDown.Increment := FTrimmTabelle.EndwertKraft div 30 + 1;
  W2Edit.Text := IntToStr(FTrimmTabelle.EndwertWeg);
  K2Edit.Text := IntToStr(FTrimmTabelle.EndwertKraft);
  Temp := FTrimmTabelle.MittelPunkt;
  { Temp ist notwendig, siehe KraftEditChange,
    W1Edit ist noch nicht gesetzt und verfälscht sonst den Mittelunkt,
    auch umgekehrt. }
  FTabChanging := True;
  K1Edit.Text := IntToStr(Temp.x);
  W1Edit.Text := IntToStr(Temp.y);
  FTabChanging := False;
  DrawTrimmTab;
end;

procedure TFormTrimmTab.Kraft1EditChange(Sender: TObject);
var
  Temp: TPoint;
begin
  Assert(FTrimmTabelle <> nil);
  Temp.x := StrToInt(K1Edit.Text);
  Temp.y := StrToInt(W1Edit.Text);
  FTrimmTabelle.MittelPunkt := Temp; { Temp ist ein Vorschlag für neuen MittelPunkt }
  Temp := FTrimmTabelle.MittelPunkt; { Temp ist jetzt überprüft und ev. korrigiert }
  K1Edit.Text := IntToStr(Temp.x);
  W1Edit.Text := IntToStr(Temp.y);
  if not FTabChanging then
    DrawTrimmTab;
end;

procedure TFormTrimmTab.ImageMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  tempX, tempY: double;
begin
  tempY := Image.Height;
  tempY := (tempY -Y) * FTrimmTabelle.EndwertKraft / tempY;
  tempX := X * FTrimmTabelle.EndwertWeg / Image.Width;
  FTabChanging := True;
  K1Edit.Text := IntToStr(Round(tempY));
  W1Edit.Text := IntToStr(Round(tempX));
  FTabChanging := False;
  DrawTrimmTab;
end;

procedure TFormTrimmTab.rbKonstanteClick(Sender: TObject);
begin
  if Sender = rbKonstante then
  begin
    { rbKonstante.Checked := True; }
    K1UpDown.Enabled := True;
    W1UpDown.Enabled := False;
  end
  else if Sender = rbGerade then
  begin
    { rbGerade.Checked := True; }
    K1UpDown.Enabled := False;
    W1UpDown.Enabled := False;
  end
  else if Sender = rbParabel then
  begin
    { rbParabel.Checked := True; }
    K1UpDown.Enabled := True;
    W1UpDown.Enabled := False;
  end
  else if Sender = rbBezier then
  begin
    { rbBezier.Checked := True; }
    K1UpDown.Enabled := True;
    W1UpDown.Enabled := True;
  end;
  Assert(FTrimmTabelle <> nil);
  FTrimmTabelle.TabellenTyp := TTabellenTyp((Sender as TRadioButton).Tag);
  DrawTrimmTab;
end;

procedure TFormTrimmTab.CalcBtnClick(Sender: TObject);
begin
  FTrimmTabelle.GetMemoLines(Memo.Lines);
end;

procedure TFormTrimmTab.EvalOptionBtnClick(Sender: TObject);
begin
  FTrimmTabelle.EvalDirection := EvalOptionBtn.Down;
end;

procedure TFormTrimmTab.InitComponentSize;
var
  w, h: Integer;
begin
  ClientHeight := Scale(400);
  ClientWidth := Scale(600);

  w := Scale(75);
  h := Scale(17);
  rbKonstante.Width := w;
  rbKonstante.Height := h;
  rbGerade.Width := w;
  rbGerade.Height := h;
  rbParabel.Width := w;
  rbParabel.Height := h;
  rbBezier.Width := w;
  rbBezier.Height := h;

  Memo.Width := Scale(130);
  Memo.Height := Scale(200);

  Image.Width := Scale(319);
  Image.Height := Scale(158);

  MemoLabel.Width := Scale(108);
  MemoLabel.Height := Scale(13);

  w := Scale(25);
  h := Scale(25);
  WriteMemoBtn.Width := w;
  WriteMemoBtn.Height := h;
  ReadMemoBtn.Width := w;
  ReadMemoBtn.Height := h;
  EvalOptionBtn.Width := w;
  EvalOptionBtn.Height := h;

  w := Scale(10);
  h := Scale(13);
  X1Label.Width := w;
  X1Label.Height := h;
  Y1Label.Width := w;
  Y1Label.Height := h;
  X2Label.Width := w;
  X2Label.Height := h;
  Y2Label.Width := w;
  Y2Label.Height := h;

  w := Scale(40);
  h := Scale(21);
  W1Edit.Width := w;
  W1Edit.Height := h;
  K1Edit.Width := w;
  K1Edit.Height := h;
  W2Edit.Width := w;
  W2Edit.Height := h;
  K2Edit.Width := w;
  K2Edit.Height := h;

  w := Scale(15);
  h := Scale(21);
  W1UpDown.Width := w;
  W1UpDown.Height := h;
  K1UpDown.Width := w;
  K1UpDown.Height := h;

  w := Scale(100);
  h := Scale(27);
  OKBtn.Width := w;
  OKBtn.Height := h;

  CancelBtn.Width := w;
  CancelBtn.Height := h;
end;

procedure TFormTrimmTab.LayoutComponents;
begin
  { Memo }

  MemoLabel.Left := Margin;
  MemoLabel.Top := Margin;

  cr := MemoLabel;
  StackV(Memo);

  { Converter Buttons, between Memo and Image }

  cr := Memo;
  StackH(ReadMemoBtn);
  ReadMemoBtn.Top := ReadMemoBtn.Top + 3 * Margin;
  StackV(WriteMemoBtn);
  StackV(EvalOptionBtn);

  { Radio Buttons, stacked horizontal at top right of form }

  cr := MemoLabel;
  StackH(rbKonstante);
  rbKonstante.Left := WriteMemoBtn.Left + WriteMemoBtn.Width + Margin;
  StackH(rbGerade);
  StackH(rbParabel);
  StackH(rbBezier);

  { Image }
  cr := rbKonstante;
  StackV(Image);

  { XY Controls, below Image }

  StackV(X1Label);
  StackH(W1Edit);
  StackH(W1UpDown);
  W1UpDown.Associate := W1Edit;

  StackH(Y1Label);
  Y1Label.Left := Y1Label.Left - Margin;
  StackH(K1Edit);
  StackH(K1UpDown);
  K1UpDown.Associate := K1Edit;

  StackH(X2Label);
  StackH(W2Edit);
  StackH(Y2Label);
  StackH(K2Edit);

  { Bottom Buttons }

  cr := W1Edit;
  StackV(OKBtn);
  OKBtn.Left := Scale(80);
  OKBtn.Top := OKBtn.Top + Margin;
  StackH(CancelBtn);

  { Form Size }

  ClientWidth := FMaxRight + Margin;
  ClientHeight := FMaxBottom + Margin;

  { final adjustments }

  Memo.Top := Image.Top;
end;

procedure TFormTrimmTab.InitTabOrder;
begin
  W2Edit.TabStop := False;
  K2Edit.TabStop := False;

  Memo.TabOrder := 0;

//  ReadMemoBtn.TabOrder := 1;
//  WriteMemoBtn.TabOrder := 2;
//  EvalOptionBtn.TabOrder := 3;

  rbKonstante.TabOrder := 4;
  rbGerade.TabOrder := 5;
  rbParabel.TabOrder := 6;
  rbBezier.TabOrder := 7;

  W1Edit.TabOrder := 8;
  K1Edit.TabOrder := 9;
  W2Edit.TabOrder := 10;
  K2Edit.TabOrder := 11;

  W1UpDown.TabOrder := 12;
  K1UpDown.TabOrder := 13;

  OKBtn.TabOrder := 14;
  CancelBtn.TabOrder := 15;
end;

procedure TFormTrimmTab.InitComponentLinks;
begin
  rbKonstante.OnClick := rbKonstanteClick;
  rbGerade.OnClick := rbKonstanteClick;
  rbParabel.OnClick := rbKonstanteClick;
  rbBezier.OnClick := rbKonstanteClick;

  WriteMemoBtn.OnClick := CalcBtnClick;
  ReadMemoBtn.OnClick := ApplyBtnClick;
  EvalOptionBtn.OnClick := EvalOptionBtnClick;

  Image.OnMouseDown := ImageMouseDown;

  W1Edit.OnChange := Kraft1EditChange;
  K1Edit.OnChange := Kraft1EditChange;

  OKBtn.OnClick := OKBtnClick;
  CancelBtn.OnClick := CancelBtnClick;
end;

procedure TFormTrimmTab.CreateComponents;
begin
  OKBtn := TButton.Create(Self);
  CancelBtn := TButton.Create(Self);
  MemoLabel := TLabel.Create(Self);
  X1Label := TLabel.Create(Self);
  Y1Label := TLabel.Create(Self);
  X2Label := TLabel.Create(Self);
  Y2Label := TLabel.Create(Self);
  Memo := TMemo.Create(Self);
  Image := TImage.Create(Self);
  K1Edit := TEdit.Create(Self);
  W2Edit := TEdit.Create(Self);
  K2Edit := TEdit.Create(Self);
  W1Edit := TEdit.Create(Self);
  W1UpDown := TUpDown.Create(Self);
  K1UpDown := TUpDown.Create(Self);
  rbKonstante := TRadioButton.Create(Self);
  rbGerade := TRadioButton.Create(Self);
  rbParabel := TRadioButton.Create(Self);
  rbBezier := TRadioButton.Create(Self);
  EvalOptionBtn := TSpeedButton.Create(Self);
  WriteMemoBtn := TSpeedButton.Create(Self);
  ReadMemoBtn := TSpeedButton.Create(Self);

  rbKonstante.Parent := Self;
  rbGerade.Parent := Self;
  rbParabel.Parent := Self;
  rbBezier.Parent := Self;

  EvalOptionBtn.Parent := Self;
  WriteMemoBtn.Parent := Self;
  ReadMemoBtn.Parent := Self;

  X1Label.Parent := Self;
  Y1Label.Parent := Self;
  X2Label.Parent := Self;
  Y2Label.Parent := Self;
  Memo.Parent := Self;

  Image.Parent := Self;

  W1Edit.Parent := Self;
  K1Edit.Parent := Self;
  W2Edit.Parent :=  Self;
  K2Edit.Parent := Self;

  W1UpDown.Parent := Self;
  K1UpDown.Parent := Self;

  OKBtn.Parent := Self;
  CancelBtn.Parent := Self;

  MemoLabel.Parent := Self;
end;

procedure TFormTrimmTab.InitComponentProps;
var
  ML: TStrings;
begin
  Caption := 'Form Trimm Tab';

  MemoLabel.WordWrap := False;
  MemoLabel.AutoSize := True;
  MemoLabel.Caption := 'Tabelle (Weg = Kraft)';

  ML := Memo.Lines;
  ML.Add('[X/mm=Y/N]');
  ML.Add('10=60');
  ML.Add('20=90');
  ML.Add('30=100');
  ML.Add('40=160');
  ML.Add('50=183');
  ML.Add('60=200');
  ML.Add('70=205');
  ML.Add('80=208');
  ML.Add('90=220');
  ML.Add('100=225');

  rbKonstante.Caption := 'Konstante';
  rbGerade.Caption := 'Gerade';
  rbParabel.Caption := 'Parabel';
  rbBezier.Caption := 'Bezier';

  rbGerade.Tag := 1;
  rbParabel.Tag := 2;
  rbBezier.Tag := 3;

  rbGerade.Checked := True;

  X1Label.Caption := 'X1';
  Y1Label.Caption := 'Y1';
  X2Label.Caption := 'X2';
  Y2Label.Caption := 'Y2';

  W1Edit.Text := '0';
  K1Edit.Text := '100';
  W2Edit.Text := 'Weg2Edit';
  K2Edit.Text := 'Kraft2Edit';

  W1Edit.Hint := 'Wegwert für Punkt 1';
  K1Edit.Hint := 'Kraftwert für Punkt 1';
  W2Edit.Hint := 'Endwert Weg';
  K2Edit.Hint := 'Endwert Kraft';

  W1Edit.ParentShowHint := False;
  W1Edit.ShowHint := True;
  W1Edit.ReadOnly := True;

  K1Edit.ParentShowHint := False;
  K1Edit.ShowHint := True;
  K1Edit.ReadOnly := True;

  W2Edit.ParentShowHint := False;
  W2Edit.ReadOnly := True;
  W2Edit.ShowHint := True;

  K2Edit.ParentShowHint := False;
  K2Edit.ReadOnly := True;
  K2Edit.ShowHint := True;

  ReadMemoBtn.Caption := '>';
  WriteMemoBtn.Caption := '<';
  EvalOptionBtn.Caption := 'K';
  EvalOptionBtn.Hint := 'Weg oder Kraft als Argument verwenden|';

  EvalOptionBtn.AllowAllUp := True;
  EvalOptionBtn.GroupIndex := 1;
  EvalOptionBtn.ParentShowHint := False;
  EvalOptionBtn.ShowHint := True;

  W1UpDown.Enabled := False;
  K1UpDown.Enabled := False;
  K1UpDown.Max := 5000;
  K1UpDown.Position := 100;

  OKBtn.Caption := 'OK';
  OKBtn.ModalResult := 1;

  CancelBtn.Cancel := True;
  CancelBtn.Caption := 'Abbrechen';
  CancelBtn.Default := True;
  CancelBtn.ModalResult := 2;
end;

procedure TFormTrimmTab.DrawTrimmTab;
begin
  FTrimmTabelle.UpdateGraphModel(FTrimmTabGraph.Model);
  FTrimmTabGraph.Draw;
end;

procedure TFormTrimmTab.RecordMax;
begin
  TempR := cr.Left + cr.Width;
  if TempR > FMaxRight then
    FMaxRight := TempR;

  TempB := cr.Top + cr.Height;
  if TempB > FMaxBottom then
    FMaxBottom := TempB;
end;

procedure TFormTrimmTab.StackH(c: TControl);
begin
  c.Left := cr.Left + cr.Width + Margin;
  c.Top := cr.Top;
  cr := c;
  RecordMax;
end;

procedure TFormTrimmTab.StackV(c: TControl);
begin
  c.Left := cr.Left;
  c.Top := cr.Top + cr.Height + Margin;
  cr := c;
  RecordMax;
end;

procedure TFormTrimmTab.AnchorVertical(c: TControl);
begin
  c.Height := ClientHeight - c.Top - Margin;
  c.Anchors := c.Anchors + [TAnchorKind.akBottom];
end;

function TFormTrimmTab.Scale(Value: Integer): Integer;
begin
  result := Round(Value * FScale);
end;

end.
