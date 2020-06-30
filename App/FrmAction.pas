unit FrmAction;

(*
-
-     F
-    * * *
-   *   *   G
-  *     * *   *
- E - - - H - - - I
-  *     * *         *
-   *   *   *           *
-    * *     *             *
-     D-------A---------------B
-              *
-              (C) federgraph.de
-
*)

interface

{$ifdef fpc}
{$mode delphi}
{$endif}

uses
  SysUtils,
  Classes,
  Controls,
  Forms,
  Graphics,
  ExtCtrls,
  StdCtrls,
  RiggVar.FB.Color,
  RiggVar.FB.ActionGroups;

type
  TFormAction = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FScale: single;
    Edit: TEdit;
    ListViewS: TListbox;
    ListViewG: TListbox;
    ListViewDetails: TListbox;
    ListViewGroups: TListbox;
    TextEdit: TLabel;
    TextDetail: TLabel;
    TextGroup: TLabel;
    TextGroups: TLabel;
    TextCaption: TLabel;
    TextSearchResult: TLabel;
    SortBtn: TButton;
    HideBtn: TButton;
    CaseBtn: TButton;
  protected
    function GetActionGroupList: TActionGroupList;
    property ActionGroupList: TActionGroupList read GetActionGroupList;
    procedure ListViewActionsItemClick(Sender: TObject);
    procedure SetupListbox(LB: TListbox; cla: TColor);
    procedure SetupText(L: TLabel; cla: TColor);
  private
    Margin: Integer;
    procedure EditKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ListViewSItemClick(Sender: TObject);
    procedure ListViewGItemClick(Sender: TObject);
    procedure ListViewGroupsItemClick(Sender: TObject);
    procedure SortBtnClick(Sender: TObject);
    procedure HideBtnClick(Sender: TObject);
    procedure CaseBtnClick(Sender: TObject);
    procedure CreateComponents;
    procedure LayoutComponents;
    function GetItemTag(Sender: TObject): Integer;
  private
    ofa, ofg: Integer;
    osn, oln, oan, ogn: string;
    ML: TStrings;
    GroupsSorted: Boolean;
    InsensitiveSearch: Boolean;
    procedure ListGroups;
    procedure ListGroupsSorted;
    procedure ListGroup(g: Integer);
    procedure ListGroupForAction(fa: Integer);
    procedure DoSearch;
    procedure ListMappings(fa: Integer);
    procedure ListActionsLong(const t: string);
    procedure ListActionsShort(const t: string);
    procedure Select(fa: Integer);
    procedure UpdateDetails(fa: Integer);
    procedure UpdateLabel(fa: Integer);
    procedure ClearCaption;
    procedure ClearMappings;
    procedure UpdateVars(fa: Integer);
    procedure UpdateCaseBtnCaption;
  end;

var
  FormAction: TFormAction;

implementation

{$R *.dfm}

uses
  RiggVar.App.Main,
  RiggVar.FB.ActionName,
  RiggVar.FB.ActionShort,
  RiggVar.FB.ActionLong,
  RiggVar.FB.ActionConst;

procedure TFormAction.EditKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  DoSearch;
end;

procedure TFormAction.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := TCloseAction.caHide;
end;

procedure TFormAction.FormCreate(Sender: TObject);
begin
  FScale := Main.Scale;

  Caption := 'Form Actions';
  ML := TStringList.Create;

  Left := Round(10 * FScale);
  Top := Round(120 * FScale);
  Width := Round(1000 * FScale);
  Height := Round(650 * FScale);
  Margin := Round(10 * FScale);

  CreateComponents;
  LayoutComponents;

  TextDetail.AutoSize := True;
  TextCaption.AutoSize := True;
  TextEdit.AutoSize := True;
  TextSearchResult.AutoSize := True;
  TextGroup.AutoSize := True;
  TextGroups.AutoSize := True;

  TextCaption.WordWrap := False;
  TextEdit.WordWrap := False;
  TextGroup.WordWrap := False;
  TextGroups.WordWrap := False;
  TextDetail.WordWrap := False;

  ListViewS.Anchors := [TAnchorKind.akLeft, TAnchorKind.akTop, TAnchorKind.akBottom];
  ListViewG.Anchors := [TAnchorKind.akLeft, TAnchorKind.akTop, TAnchorKind.akBottom];
  ListViewDetails.Anchors := [TAnchorKind.akLeft, TAnchorKind.akTop, TAnchorKind.akBottom];
  ListViewGroups.Anchors := [TAnchorKind.akLeft, TAnchorKind.akTop, TAnchorKind.akBottom];

  ListGroups;

  UpdateCaseBtnCaption;

  SetupListbox(ListViewS, TRggColors.Dodgerblue);
  SetupListbox(LIstViewDetails, TRggColors.Orchid);
  SetupListbox(LIstViewG, TRggColors.Dodgerblue);
  SetupListbox(LIstViewGroups, TRggColors.Blue);

  Edit.Font.Name := 'Consolas';
  Edit.Font.Size := 12;

  SetupText(TextCaption, TRggColors.Dodgerblue);
end;

procedure TFormAction.FormDestroy(Sender: TObject);
begin
  ML.Free;
end;

procedure TFormAction.DoSearch;
var
  t: string;
begin
  ListViewS.Items.Clear;
  if InsensitiveSearch then
    t := Lowercase(Edit.Text)
  else
    t := Edit.Text;

  if t.StartsWith('fa') then
  begin
    t := t.Substring(2);
    if t.Length > 2 then
      ListActionsLong(t);
  end

  else if t.StartsWith('-') then
  begin
    t := t.Substring(1);
    if t.Length > 2 then
      ListActionsLong(t);
  end

  else if (t.Length >= 1) and (t.Length <= 3) then
    ListActionsShort(t)

  else if (t.Length > 3) and (t.Length <= 8) then
    ListActionsLong(t);
end;

procedure TFormAction.ListActionsShort(const t: string);
var
  s, u: string;
  fa: Integer;
begin
  for fa := 0 to faMax-1 do
  begin
    s := GetFederActionShort(fa);
      if InsensitiveSearch then
        u := LowerCase(s)
      else
        u := s;
    if u.StartsWith(t) then
    begin
      ListViewS.Items.AddObject(s, TObject(fa));
    end;
  end;
end;

procedure TFormAction.ListActionsLong(const t: string);
var
  a, s: string;
  fa: Integer;
begin
  for fa := 0 to faMax-1 do
  begin
    if InsensitiveSearch then
      a := LowerCase(GetFederActionName(fa))
    else
      a := GetFederActionName(fa);
    s := GetFederActionShort(fa);
    if a.Contains(t) then
    begin
      ListViewS.Items.AddObject(s, TObject(fa));
    end;
  end;
end;

procedure TFormAction.ListGroups;
var
  g: Integer;
  gn: string;
  agl: TActionGroupList;
begin
  agl := ActionGroupList;
  ListViewGroups.Items.Clear;
  for g := 0 to agl.Count-1 do
  begin
    gn := agl.GetGroupName(g);
    ListViewGroups.Items.AddObject(gn, TObject(g));
  end;
end;

procedure TFormAction.ListViewGroupsItemClick(Sender: TObject);
var
  g: Integer;
begin
  g := GetItemTag(Sender);
  ClearCaption;
  ListGroup(g);
  ClearMappings;
end;

procedure TFormAction.ListGroupForAction(fa: Integer);
var
  g: Integer;
begin
  g := ActionGroupList.GetGroup(fa);
  ListGroup(g);
end;

procedure TFormAction.ListGroup(g: Integer);
var
  h, i: Integer;
  gn: string;
begin
  gn := ActionGroupList.GetGroupName(g);
  TextGroup.Caption := Format('Group %d = %s', [g, gn]);

  ListViewG.Items.Clear;
  for i := 0 to faMax-1 do
  begin
    h := ActionGroupList.GetGroup(i);
    if g = h then
    begin
      ListViewG.Items.AddObject(GetFederActionName(i), TObject(i));
    end;
  end;
end;

procedure TFormAction.ListViewGItemClick(Sender: TObject);
var
  fa: Integer;
begin
  fa := GetItemTag(Sender);
  if fa > 0 then
  begin
    UpdateLabel(fa);
    ListMappings(fa);
    Select(fa);
  end;
end;

procedure TFormAction.ListViewActionsItemClick(Sender: TObject);
var
  fa: Integer;
begin
  fa := GetItemTag(Sender);
  UpdateLabel(fa);
  ListGroupForAction(fa);
  ListMappings(fa);
end;

procedure TFormAction.ListViewSItemClick(Sender: TObject);
var
  fa: Integer;
begin
  fa := GetItemTag(Sender);
  UpdateLabel(fa);
  ListGroupForAction(fa);
  ListMappings(fa);
  Select(fa);
end;

procedure TFormAction.Select(fa: Integer);
begin
//  ListViewActions.ScrollTo(fa);
end;

procedure TFormAction.ClearCaption;
begin
  TextCaption.Caption := '';
end;

procedure TFormAction.UpdateLabel(fa: Integer);
var
  fg: Integer;
  sn, ln, gn: string;
begin
  sn := GetFederActionShort(fa);
  ln := GetFederActionLong(fa);
  fg := ActionGroupList.GetGroup(fa);
  gn := ActionGroupList.GetGroupName(fg);
  TextCaption.Caption := Format('"%s" = "%s" in group "%s"', [sn, ln, gn]);
end;

procedure TFormAction.ClearMappings;
begin
  ListViewDetails.Items.Clear;
  TextDetail.Caption := 'Action Details';
end;

procedure TFormAction.ListMappings(fa: Integer);
begin
  ML.Clear;
  UpdateVars(fa);

  ML.Add(Format('action id = %d', [ofa]));
  ML.Add(Format('action name = %s', [oan]));
  ML.Add(Format('short name = %s', [osn]));
  ML.Add(Format('long name = %s', [oln]));
  ML.Add(Format('group id = %d', [ofg]));
  ML.Add(Format('group name = %s', [ogn]));

  Main.Keyboard.GetShortcuts(fa, ML);
//  Main.ActionMap0.CollectOne(fa, ML);
  Main.ActionMapTablet.CollectOne(fa, ML);
  Main.ActionMapPhone.CollectOne(fa, ML);
//  Main.FederMenu.CollectOne(fa, ML);

  UpdateDetails(fa);
end;

procedure TFormAction.UpdateDetails(fa: Integer);
var
  i: Integer;
  s: string;
begin
  TextDetail.Caption := Format('Action %s = %s', [oan, osn]);

  ListViewDetails.Items.Clear;
  for i := 0 to ML.Count-1 do
  begin
    s := ML[i];
    if s <> '' then
    begin
      ListViewDetails.Items.Add(s);
    end;
  end;
end;

procedure TFormAction.UpdateVars(fa: Integer);
begin
  ofa := fa;
  osn := GetFederActionShort(ofa);
  oln := GetFederActionLong(ofa);
  oan := GetFederActionName(ofa);
  ofg := ActionGroupList.GetGroup(ofa);
  ogn := ActionGroupList.GetGroupName(ofg);
end;

procedure TFormAction.SortBtnClick(Sender: TObject);
begin
  GroupsSorted := not GroupsSorted;
  if GroupsSorted then
    ListGroupsSorted
  else
    ListGroups;
end;

procedure TFormAction.ListGroupsSorted;
var
  g: Integer;
  gn: string;
  agl: TActionGroupList;
  ML: TStringList;
  i: Integer;
begin
//  SortBtn.Enabled := False;

  agl := ActionGroupList;

  ML := TStringList.Create;
  ML.Sorted := True;

  for i := 0 to agl.Count - 1 do
  begin
    ML.AddObject(agl.GroupNames[i], TObject(i));
  end;

  ListViewGroups.Items.Clear;

  for i := 0 to ML.Count-1 do
  begin
    g := Integer(ML.Objects[i]);
    gn := agl.GetGroupName(g);
    gn := ML[i];
    Assert(ML[i] = agl.GetGroupName(g));
    ListViewGroups.Items.AddObject(gn, TObject(g));
  end;

  ML.Free;
end;

procedure TFormAction.HideBtnClick(Sender: TObject);
begin
  ClientWidth := Round(ListViewGroups.Left);
end;

procedure TFormAction.CaseBtnClick(Sender: TObject);
begin
  InsensitiveSearch  := not InsensitiveSearch;
  UpdateCaseBtnCaption;
  DoSearch;
end;

procedure TFormAction.UpdateCaseBtnCaption;
begin
  if InsensitiveSearch then
    CaseBtn.Caption := 'aA'
  else
    CaseBtn.Caption := 'aa';
end;

procedure TFormAction.CreateComponents;
begin
  Edit := TEdit.Create(Self);

  ListViewS := TListbox.Create(Self);
  ListViewDetails := TListbox.Create(Self);
  ListViewG := TListbox.Create(Self);
  ListViewGroups := TListbox.Create(Self);

  TextEdit := TLabel.Create(Self);
  TextDetail := TLabel.Create(Self);
  TextGroup := TLabel.Create(Self);
  TextGroups := TLabel.Create(Self);
  TextCaption := TLabel.Create(Self);
  TextSearchResult := TLabel.Create(Self);

  SortBtn := TButton.Create(Self);
  HideBtn := TButton.Create(Self);
  CaseBtn := TButton.Create(Self);

  Edit.Parent := Self;

  ListViewS.Parent := Self;
  ListViewDetails.Parent := Self;
  ListViewG.Parent := Self;
  ListViewGroups.Parent := Self;

  TextEdit.Parent := Self;
  TextDetail.Parent := Self;
  TextGroup.Parent := Self;
  TextGroups.Parent := Self;
  TextCaption.Parent := Self;
  TextSearchResult.Parent := Self;

  SortBtn.Parent := Self;
  HideBtn.Parent := Self;
  CaseBtn.Parent := Self;
end;

procedure TFormAction.LayoutComponents;
  function Scale(Value: Integer): Integer;
  begin
    result := Round(Value * FScale);
  end;
begin
  Edit.Left := Scale(16);
  Edit.Top := Scale(88);
  Edit.Width := Scale(100);
  Edit.OnKeyUp := EditKeyUp;

  ListViewS.Left := Scale(16);
  ListViewS.Top := Scale(144);
  ListViewS.Width := Scale(145);
  ListViewS.Height := ClientHeight - ListViewS.Top - Margin;
  ListViewS.OnClick := ListViewSItemClick;

  ListViewDetails.Left := Scale(176);
  ListViewDetails.Top := Scale(88);
  ListViewDetails.Width := Scale(305);
  ListViewDetails.Height := ClientHeight - ListViewDetails.Top - Margin;

  ListViewG.Left := Scale(496);
  ListViewG.Top := Scale(88);
  ListViewG.Width := Scale(241);
  ListViewG.Height := ClientHeight - ListViewG.Top - Margin;
  ListViewG.OnClick := ListViewGItemClick;

  ListViewGroups.Left := Scale(752);
  ListViewGroups.Top := Scale(88);
  ListViewGroups.Width := Scale(225);
  ListViewGroups.Height := ClientHeight - ListViewGroups.Top - Margin;
  ListViewGroups.OnClick := ListViewGroupsItemClick;

  TextCaption.Left := Scale(60);
  TextCaption.Top := Scale(8);
  TextCaption.Caption := 'SN = "LongName" in group ActionGroup';
  TextCaption.Font.Size := 20;

  TextEdit.Left := Scale(16);
  TextEdit.Top := Scale(64);
  TextEdit.Caption := 'Search';

  TextSearchResult.Left := Scale(16);
  TextSearchResult.Top := Scale(120);
  TextSearchResult.Caption := 'Search Results';

  TextGroup.Left := Scale(496);
  TextGroup.Top := Scale(64);
  TextGroup.Caption := 'Group Actions';

  TextGroups.Left := Scale(752);
  TextGroups.Top := Scale(64);
  TextGroups.Caption := 'All Groups';

  TextDetail.Left := Scale(176);
  TextDetail.Top := Scale(64);
  TextDetail.Caption := 'Action Details';

  SortBtn.Left := Scale(784);
  SortBtn.Top := Scale(24);
  SortBtn.Caption := 'Sort Groups';
  SortBtn.OnClick := SortBtnClick;

  HideBtn.Left := Scale(872);
  HideBtn.Top := Scale(24);
  HideBtn.Caption := 'Hide Groups';
  HideBtn.OnClick := HideBtnClick;

  CaseBtn.Left := Scale(128);
  CaseBtn.Top := Scale(88);
  CaseBtn.Width := Scale(33);
  CaseBtn.Height := Scale(22);
  CaseBtn.Caption := 'SO';
  CaseBtn.OnClick := CaseBtnClick;
end;

function TFormAction.GetActionGroupList: TActionGroupList;
begin
  result := Main.ActionGroupList;
end;

function TFormAction.GetItemTag(Sender: TObject): Integer;
var
  ii: Integer;
  lb: TListbox;
begin
  result := -1;
  if not (Sender is TListbox) then
    Exit;
  lb := Sender as TListbox;
  ii := lb.ItemIndex;
  if ii = -1 then
    Exit;
  result := Integer(lb.Items.Objects[ii]);
end;

procedure TFormAction.SetupListbox(LB: TListbox; cla: TColor);
begin
  LB.Font.Name := 'Consolas';
  LB.Font.Size := 12;
  LB.Font.Color := cla;
end;

procedure TFormAction.SetupText(L: TLabel; cla: TColor);
begin
  L.Font.Name := 'Consolas';
//  L.Font.Size := 12;
  L.Font.Color := cla;
end;

end.
