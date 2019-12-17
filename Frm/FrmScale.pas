unit FrmScale;

interface

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls, 
  Buttons, ExtCtrls, Mask;

type
  TRumpfFaktorDlg = class(TForm)
    OKBtn: TButton;
    CancelBtn: TButton;
    GroupBox: TGroupBox;
    EditL: TMaskEdit;
    EditB: TMaskEdit;
    EditH: TMaskEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    procedure FormShow(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure OKBtnClick(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
  private
    Valid: Boolean;
  public
    L, B, H: Integer;
  end;

var
  RumpfFaktorDlg: TRumpfFaktorDlg;

implementation

{$R *.DFM}

procedure TRumpfFaktorDlg.FormShow(Sender: TObject);
begin
  EditL.Text := IntToStr(L);
  EditB.Text := IntToStr(B);
  EditH.Text := IntToStr(H);
  Valid := True; { dann kann auch der Btn in der Titelleiste zum Schließen
  benutzt werden, solange Valid nicht auf False gesetzt wird. }
end;

procedure TRumpfFaktorDlg.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  CanClose := Valid;
end;

procedure TRumpfFaktorDlg.OKBtnClick(Sender: TObject);
var
  tempL, tempB, tempH: Integer;
  Edit: TMaskEdit;
begin
  Edit := nil;
  try
    Valid := True;
    Edit := EditL; tempL := StrToInt(EditL.Text);
    Edit := EditB; tempB := StrToInt(EditB.Text);
    Edit := EditH; tempH := StrToInt(EditH.Text);
    L := tempL;
    B := tempB;
    H := tempH;
  except
    begin
      Valid := False;
      if Assigned(Edit) then
        ActiveControl := Edit;
    end;
  end;
end;

procedure TRumpfFaktorDlg.CancelBtnClick(Sender: TObject);
begin
  Valid := True;
end;

end.
