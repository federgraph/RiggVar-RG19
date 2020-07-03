unit FrmInput;

interface

uses
  System.SysUtils,
  System.Classes,
  System.UITypes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.ExtCtrls,
  Vcl.StdCtrls,
  Vcl.ComCtrls;

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
    PaintBoxM: TImage;
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
    PaintBoxMD: TImage;
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
    PaintBoxMOhne: TImage;
    tsOhneStarr: TTabSheet;
    pnOhne: TPanel;
    lbVorstagOS: TLabel;
    lbWPowerOS: TLabel;
    lbValue7: TLabel;
    lbValue8: TLabel;
    rbVorstagOS: TRadioButton;
    sbVorstagOS: TScrollBar;
    sbWPowerOS: TScrollBar;
    procedure FormCreate(Sender: TObject);
    procedure rbWinkelClick(Sender: TObject);
    procedure rbWinkelMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure sbControllerScroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);
    procedure InputPagesChange(Sender: TObject);
    procedure FormHide(Sender: TObject);
  end;

var
  InputForm: TInputForm;

implementation

{$R *.DFM}

uses
  RiggVar.RG.Def,
  RggModul,
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
  if Sender = rbController then
    SBname := fpController
  else if Sender = rbControllerD then
    SBname := fpController
  else if Sender = rbControllerOhne then
    SBname := fpController

  else if (Sender = rbWinkel) and RiggModul.WinkelBtnDown then
    SBname := fpWinkel
  else if (Sender = rbWinkel) and not RiggModul.WinkelBtnDown then
    SBname := fpVorstag
  else if Sender = rbVorstagD then
    SBname := fpVorstag
  else if Sender = rbVorstagOhne then
    SBname := fpVorstag
  else if Sender = rbVorstagOS then
    SBname := fpVorstagOS

  else if Sender = rbWante then
    SBname := fpWante
  else if Sender = rbWanteD then
    SBname := fpWante
  else if Sender = rbWanteOhne then
    SBname := fpWante

  else if Sender = rbWoben then
    SBname := fpWoben
  else if Sender = rbWobenD then
    SBname := fpWoben

  else if Sender = rbSalingH then
    SBname := fpSalingH
  else if Sender = rbSalingA then
    SBname := fpSalingA
  else if Sender = rbSalingLD then
    SBname := fpSalingL

  else
    SBName := fpVorstag;

  RiggModul.SBName := SBName;
end;

procedure TInputForm.rbWinkelMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  { dadurch wird TRadioButton.Click auch aufgerufen, wenn der
    RadioButton gechecked ist! }
  with Sender as TRadioButton do
    if Checked then
    begin
      Checked := False;
      Checked := True;
    end;
end;

procedure TInputForm.InputPagesChange(Sender: TObject);
begin
  case InputForm.InputPages.ActivePage.Tag of
    0: RiggModul.SalingTyp := stFest;
    1: RiggModul.SalingTyp := stDrehbar;
    2: RiggModul.SalingTyp := stOhneBiegt;
    3: RiggModul.SalingTyp := stOhneStarr;
  end;
end;

procedure TInputForm.sbControllerScroll(Sender: TObject; ScrollCode: TScrollCode;
  var ScrollPos: Integer);
begin
  RiggModul.sbControllerScroll(Sender, ScrollCode, ScrollPos);
end;

procedure TInputForm.FormHide(Sender: TObject);
begin
  if RiggModul <> nil then
  begin
    if RiggModul.ViewModelM <> nil then
    begin
      RiggModul.ViewModelM.InputFormItemChecked := False;
      RiggModul.ViewModelM.UpdateView;
  end;
  end;
end;

end.

