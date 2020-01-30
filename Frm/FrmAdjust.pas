unit FrmAdjust;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Classes,
  System.Types,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.Buttons,
  RggGBox;

type
  TAdjustForm = class(TForm)
    UpBtn: TSpeedButton;
    LeftBtn: TSpeedButton;
    DownBtn: TSpeedButton;
    MinusBtn: TSpeedButton;
    PlusBtn: TSpeedButton;
    ViewBtn: TSpeedButton;
    RightBtn: TSpeedButton;
    OKBtn: TBitBtn;
    procedure LeftBtnClick(Sender: TObject);
    procedure MinusBtnClick(Sender: TObject);
    procedure ViewBtnClick(Sender: TObject);
  private
    FOnChange: TNotifyEvent;
    procedure Change;
  public
    Grafik: TGetriebeGraph;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

procedure ShowAdjustForm(TheGrafik: TGetriebeGraph; TheEvent: TNotifyEvent);

implementation

{$R *.DFM}

uses RggTypes;

procedure TAdjustForm.LeftBtnClick(Sender: TObject);
var
  temp: TPoint;
begin
  if not Assigned(Grafik) then
    Exit;
  case Grafik.ViewPoint of
    vpSeite: temp := Point(Grafik.cOffsetX1,Grafik.cOffsetY1);
    vpAchtern: temp := Point(Grafik.cOffsetX2,Grafik.cOffsetY2);
    vpTop: temp := Point(Grafik.cOffsetX3,Grafik.cOffsetY3);
    vp3D: temp := Point(Grafik.cOffsetX4,Grafik.cOffsetY4);
  end;
  if Sender = LeftBtn then
    temp.x := temp.x - 10;
  if Sender = RightBtn then
    temp.x := temp.x + 10;
  if Sender = UpBtn then
    temp.y := temp.y - 10;
  if Sender = DownBtn then
    temp.y := temp.y + 10;
  case Grafik.ViewPoint of
    vpSeite:
    begin
      Grafik.cOffsetX1 := temp.x;
      Grafik.cOffsetY1 := temp.y;
    end;
    vpAchtern:
    begin
      Grafik.cOffsetX2 := temp.x;
      Grafik.cOffsetY2 := temp.y;
    end;
    vpTop:
    begin
      Grafik.cOffsetX3 := temp.x;
      Grafik.cOffsetY3 := temp.y;
    end;
    vp3D:
    begin
      Grafik.cOffsetX4 := temp.x;
      Grafik.cOffsetY4 := temp.y;
    end;
  end;
  Grafik.UpdateOffset;
  Change;
end;

procedure TAdjustForm.MinusBtnClick(Sender: TObject);
var
  temp: double;
begin
  if not Assigned(Grafik) then
    Exit;
  temp := Grafik.Zoom;
  if Sender = MinusBtn then
    temp := temp/1.1;
  if Sender = PlusBtn then
    temp := temp*1.1;
  Grafik.Zoom := temp;
  Change;
end;

procedure TAdjustForm.ViewBtnClick(Sender: TObject);
begin
  if not Assigned(Grafik) then
    Exit;
  case Grafik.ViewPoint of
    vpSeite: Grafik.ViewPoint := vpAchtern;
    vpAchtern: Grafik.ViewPoint := vpTop;
    vpTop: Grafik.ViewPoint := vp3D;
    vp3D: Grafik.ViewPoint := vpSeite;
  end;
  Change;
end;

procedure TAdjustForm.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure ShowAdjustForm(TheGrafik: TGetriebeGraph; TheEvent: TNotifyEvent);
var
  AdjustForm: TAdjustForm;
begin
  AdjustForm := TAdjustForm.Create(nil);
  try
    AdjustForm.Grafik := TheGrafik;
    AdjustForm.OnChange := TheEvent;
    AdjustForm.ShowModal;
  finally
    AdjustForm.Free;
  end;
end;

end.
