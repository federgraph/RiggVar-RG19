unit FrmInput;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  ExtCtrls,
  StdCtrls,
  ComCtrls;

type
  TInputForm = class(TForm)
    InputPages: TPageControl;
    tsFest: TTabSheet;
    pnFest: TPanel;
    lbController: TLabel;
    lbWinkel: TLabel;
    lbWante: TLabel;
    lbWoben: TLabel;
    lbSalingH: TLabel;
    lbSalingA: TLabel;
    lbValue1: TLabel;
    lbValue2: TLabel;
    lbValue3: TLabel;
    lbValue4: TLabel;
    lbValue5: TLabel;
    lbValue6: TLabel;
    sbWinkel: TScrollBar;
    sbWante: TScrollBar;
    sbWoben: TScrollBar;
    sbSalingH: TScrollBar;
    sbSalingA: TScrollBar;
    rbController: TRadioButton;
    rbWinkel: TRadioButton;
    sbController: TScrollBar;
    rbWante: TRadioButton;
    rbWoben: TRadioButton;
    rbSalingH: TRadioButton;
    rbSalingA: TRadioButton;
    pnMast: TPanel;
    PaintBoxM: TPaintBox;
    tsDrehbar: TTabSheet;
    pnDrehbar: TPanel;
    lbControllerD: TLabel;
    lbWinkelD: TLabel;
    lbWanteD: TLabel;
    lbWobenD: TLabel;
    lbD1: TLabel;
    lbD2: TLabel;
    lbD3: TLabel;
    lbD4: TLabel;
    lbD5: TLabel;
    lbSalingLD: TLabel;
    sbVorstagD: TScrollBar;
    sbWanteD: TScrollBar;
    sbWobenD: TScrollBar;
    sbSalingLD: TScrollBar;
    rbControllerD: TRadioButton;
    rbVorstagD: TRadioButton;
    sbControllerD: TScrollBar;
    rbWanteD: TRadioButton;
    rbWobenD: TRadioButton;
    rbSalingLD: TRadioButton;
    pnMastD: TPanel;
    PaintBoxMD: TPaintBox;
    tsOhne: TTabSheet;
    pnOhneBK: TPanel;
    lbControllerOhne: TLabel;
    lbVorstagOhne: TLabel;
    lbWanteOhne: TLabel;
    lbOhne1: TLabel;
    lbOhne2: TLabel;
    lbOhne3: TLabel;
    sbVorstagOhne: TScrollBar;
    sbWanteOhne: TScrollBar;
    rbControllerOhne: TRadioButton;
    rbVorstagOhne: TRadioButton;
    sbControllerOhne: TScrollBar;
    rbWanteOhne: TRadioButton;
    pnMastOhne: TPanel;
    PaintBoxMOhne: TPaintBox;
    tsOhneStarr: TTabSheet;
    pnOhne: TPanel;
    lbVorstagOS: TLabel;
    lbWPowerOS: TLabel;
    lbValue7: TLabel;
    lbValue8: TLabel;
    rbVorstagOS: TRadioButton;
    sbVorstagOS: TScrollBar;
    sbWPowerOS: TScrollBar;
    tsDatenbank: TTabSheet;
    procedure FormCreate(Sender: TObject);
    procedure rbWinkelClick(Sender: TObject);
    procedure rbWinkelMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure sbControllerScroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);
    procedure InputPagesChange(Sender: TObject);
    procedure PaintBoxMPaint(Sender: TObject);
    procedure FormHide(Sender: TObject);
  public
    procedure DoDatenItemClick; virtual;
  end;

var
  InputForm: TInputForm;

implementation

{$R *.DFM}

uses
  FrmMain,
  RiggVar.RG.Def,
  RiggUnit,
  RggTypes;

procedure TInputForm.FormCreate(Sender: TObject);
begin
  InputPages.ActivePage := tsFest;
  lbWinkel.Caption := 'Winkel';
end;

procedure TInputForm.rbWinkelClick(Sender: TObject);
var
  SBName: TSBName;
begin
  if Sender = rbController then SBname := fpController
  else if Sender = rbControllerD then SBname := fpController
  else if Sender = rbControllerOhne then SBname := fpController

  else if (Sender = rbWinkel) and RiggModul.WinkelBtnDown then SBname := fpWinkel
  else if (Sender = rbWinkel) and not RiggModul.WinkelBtnDown then SBname := fpVorstag
  else if Sender = rbVorstagD then SBname := fpVorstag
  else if Sender = rbVorstagOhne then SBname := fpVorstag
  else if Sender = rbVorstagOS then SBname := fpVorstagOS

  else if Sender = rbWante then SBname := fpWante
  else if Sender = rbWanteD then SBname := fpWante
  else if Sender = rbWanteOhne then SBname := fpWante

  else if Sender = rbWoben then SBname := fpWoben
  else if Sender = rbWobenD then SBname := fpWoben

  else if Sender = rbSalingH then SBname := fpSalingH
  else if Sender = rbSalingA then SBname := fpSalingA
  else if Sender = rbSalingLD then SBname := fpSalingL

  else SBName := fpVorstag;

  RiggModul.SBName := SBName;
end;

procedure TInputForm.rbWinkelMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  { dadurch wird TRadioButton.Click auch aufgerufen, wenn der
    RadioButton gechecked ist! }
  with Sender as TRadioButton do
    if Checked then begin
      Checked := False;
      Checked := True;
    end;
end;

procedure TInputForm.InputPagesChange(Sender: TObject);
begin
  RiggModul.InputPagesChange(Sender);
end;

procedure TInputForm.sbControllerScroll(Sender: TObject; ScrollCode: TScrollCode;
  var ScrollPos: Integer);
begin
  RiggModul.sbControllerScroll(Sender, ScrollCode, ScrollPos);
end;

procedure TInputForm.PaintBoxMPaint(Sender: TObject);
begin
  RiggModul.DrawPaintBoxM;
end;

procedure TInputForm.FormHide(Sender: TObject);
begin
  FormMain.InputFormItem.Checked := False;
end;

procedure TInputForm.DoDatenItemClick;
begin
  { Methode ist im Gegensatz zu RiggForm.DatenItemClick virtuell und wird in
    InputFormNote überschrieben, um die ListBox zu focusieren, wenn das
    Datenbanksheet die aktuelle Seite wird. }
end;

end.

