unit FrmModel;

interface

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls, Rggunit4;

type
  TRiggDialog = class(TForm)
    OKBtn: TButton;
    CancelBtn: TButton;
    rgSalingTyp: TRadioGroup;
    cbWinkel: TCheckBox;
    cbController: TCheckBox;
    HelpBtn: TButton;
    Bevel: TBevel;
    procedure FormShow(Sender: TObject);
    procedure rgSalingTypClick(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
  public
    Rigg: TRigg;
  end;

var
  RiggDialog: TRiggDialog;

implementation

uses
  RggTypes;

{$R *.DFM}

procedure TRiggDialog.FormShow(Sender: TObject);
begin
  case Rigg.SalingTyp of
    stFest: rgSalingTyp.ItemIndex := 0;
    stDrehbar: rgSalingTyp.ItemIndex := 1;
    stOhne: rgSalingTyp.ItemIndex := 2;
    stOhne_2: rgSalingTyp.ItemIndex := 3;
  end;

  case Rigg.ControllerTyp of
    ctDruck, ctZugDruck: cbController.Checked := True;
    ctOhne: cbController.Checked := False;
  end;

  if Rigg.SalingTyp = stFest then begin
    cbWinkel.enabled := True;
    cbWinkel.Checked := Rigg.ManipulatorMode
  end else begin
    cbWinkel.Checked := Rigg.ManipulatorMode;
    cbWinkel.enabled := False;
  end;
end;

procedure TRiggDialog.rgSalingTypClick(Sender: TObject);
begin
  //Winkel
  if rgSalingTyp.ItemIndex <> 0 then begin
    cbWinkel.enabled := False;
    cbWinkel.Checked := False;
  end else
    cbWinkel.enabled := True;

  //Controller
  if rgSalingTyp.ItemIndex = 3 then begin
    cbController.enabled := False;
    cbController.Checked := False;
  end else
    cbController.enabled := True;
end;

procedure TRiggDialog.HelpBtnClick(Sender: TObject);
begin
  {Application.HelpContext(HelpContext);}
end;

procedure TRiggDialog.OKBtnClick(Sender: TObject);
begin
  case rgSalingTyp.ItemIndex of
    0: Rigg.SalingTyp := stFest;
    1: Rigg.SalingTyp := stDrehbar;
    2: Rigg.SalingTyp := stOhne;
    3: Rigg.SalingTyp := stOhne_2;
  end;

  if cbController.Checked then
    Rigg.ControllerTyp := ctDruck
  else
    Rigg.ControllerTyp := ctOhne;
  Rigg.ManipulatorMode := cbWinkel.Checked;
end;

end.
