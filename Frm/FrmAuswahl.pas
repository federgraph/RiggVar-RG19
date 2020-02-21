unit FrmAuswahl;

interface

uses
  Winapi.Windows,
  System.Classes,
  Vcl.Graphics,
  Vcl.Forms,
  Vcl.Controls,
  Vcl.Buttons,
  Vcl.StdCtrls,
  RggTypes;

type
  TYAuswahlDlg = class(TForm)
    OKBtn: TBitBtn;
    CancelBtn: TBitBtn;
    HelpBtn: TBitBtn;
    SrcList: TListBox;
    DstList: TListBox;
    SrcLabel: TLabel;
    DstLabel: TLabel;
    IncludeBtn: TSpeedButton;
    IncAllBtn: TSpeedButton;
    ExcludeBtn: TSpeedButton;
    ExAllBtn: TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure IncludeBtnClick(Sender: TObject);
    procedure ExcludeBtnClick(Sender: TObject);
    procedure IncAllBtnClick(Sender: TObject);
    procedure ExcAllBtnClick(Sender: TObject);
    procedure MoveSelected(List: TCustomListBox; Items: TStrings);
    procedure SetItem(List: TListBox; const Idx: Integer);
    function GetFirstSelection(List: TCustomListBox): Integer;
    procedure SetButtons;
  public
    procedure GetEntries(const RecordList: TYAchseRecordList);
  end;

var
  YAuswahlDlg: TYAuswahlDlg;

implementation

{$R *.DFM}

procedure TYAuswahlDlg.FormCreate(Sender: TObject);
var
  tempRecordList: TYAchseRecordList;
begin
  InitYAchseRecordList(tempRecordList);
  GetEntries(tempRecordList);
end;

procedure TYAuswahlDlg.IncludeBtnClick(Sender: TObject);
var
  Index: Integer;
begin
  Index := GetFirstSelection(SrcList);
  MoveSelected(SrcList, DstList.Items);
  SetItem(SrcList, Index);
end;

procedure TYAuswahlDlg.ExcludeBtnClick(Sender: TObject);
var
  Index: Integer;
begin
  Index := GetFirstSelection(DstList);
  MoveSelected(DstList, SrcList.Items);
  SetItem(DstList, Index);
end;

procedure TYAuswahlDlg.IncAllBtnClick(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to SrcList.Items.Count - 1 do
    DstList.Items.AddObject(SrcList.Items[I],
      SrcList.Items.Objects[I]);
  SrcList.Items.Clear;
  SetItem(SrcList, 0);
end;

procedure TYAuswahlDlg.ExcAllBtnClick(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to DstList.Items.Count - 1 do
    SrcList.Items.AddObject(DstList.Items[I], DstList.Items.Objects[I]);
  DstList.Items.Clear;
  SetItem(DstList, 0);
end;

procedure TYAuswahlDlg.MoveSelected(List: TCustomListBox; Items: TStrings);
var
  I: Integer;
begin
  for I := List.Items.Count - 1 downto 0 do
    if List.Selected[I] then
    begin
      Items.AddObject(List.Items[I], List.Items.Objects[I]);
      List.Items.Delete(I);
    end;
end;

procedure TYAuswahlDlg.SetButtons;
var
  SrcEmpty, DstEmpty: Boolean;
begin
  SrcEmpty := SrcList.Items.Count = 0;
  DstEmpty := DstList.Items.Count = 0;
  IncludeBtn.Enabled := not SrcEmpty;
  IncAllBtn.Enabled := not SrcEmpty;
  ExcludeBtn.Enabled := not DstEmpty;
  ExAllBtn.Enabled := not DstEmpty;
end;

function TYAuswahlDlg.GetFirstSelection(List: TCustomListBox): Integer;
begin
  for Result := 0 to List.Items.Count - 1 do
  begin
    if List.Selected[Result] then
      Exit;
  end;
  Result := LB_ERR;
end;

procedure TYAuswahlDlg.SetItem(List: TListBox; const Idx: Integer);
var
  MaxIndex: Integer;
  i: Integer;
begin
  List.SetFocus;
  MaxIndex := List.Items.Count - 1;
  i := Idx;
  if i = LB_ERR then
    i := 0
  else if i > MaxIndex then
    i := MaxIndex;
  List.Selected[i] := True;
  SetButtons;
end;

procedure TYAuswahlDlg.GetEntries(const RecordList: TYAchseRecordList);
var
  YAV: TYAchseValue;
begin
  SrcList.Clear;
  DstList.Clear;
  for YAV := Low(TYAchseValue) to High(TYAchseValue) do begin
    if RecordList[YAV].ComboIndex = -1 then
      SrcList.Items.Add(RecordList[YAV].ComboText)
    else
      DstList.Items.Add(RecordList[YAV].ComboText);
  end;
end;

end.
