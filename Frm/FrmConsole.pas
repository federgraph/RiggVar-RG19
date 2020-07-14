unit FrmConsole;
{
  29. Mai 2002
  Exception tritt auf, wenn WindowState auf wsMaximized gesetzt wird,
  Console das einzige MDI Fenster ist,
  und Console geschlossen bzw. wieder geöffnet wird.
  Deshalb WindowState auf wsNormal gesetzt im OI,
  AutoScroll auf True gesetzt.
}

interface

uses
  System.SysUtils,
  System.Classes,
  Vcl.Forms;

type
  TConsoleForm = class(TForm)
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    FScale: single;
    function Scale(Value: Integer): Integer;
  end;

var
  ConsoleForm: TConsoleForm;

implementation

uses
  RiggVar.App.Main,
  RggModul,
  FrmInput,
  FrmOutput,
  FrmGrafik;

{$R *.DFM}

procedure TConsoleForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  RiggModul.ConsoleActive := False;

  { InputForm }

  InputForm.Hide;
  InputForm.Parent := nil;
  InputForm.BorderStyle := bsSizeable;
  InputForm.ClientWidth := InputForm.InputPages.Width;
  InputForm.ClientHeight := InputForm.InputPages.Height;
  InputForm.Position := poScreenCenter;

  { OutputForm }

  { Das WindowHandle von YComboBox wird zerstört, wenn ConsoleForm freigegeben
    wird. Wenn OutputForm dann neu angezeigt wird ist YComboBox.ItemIndex = -1,
    da ItemIndex der ComboBox von VCL nicht gesichert wird im Gegensatz zum
    ItemIndex einer ListBox! YComboBox.ItemIndex wird außerdem als Index in
    ein Array verwendet und dieser darf nicht Null sein. }
  RiggModul.YComboSavedItemIndex := OutputForm.YComboBox.ItemIndex;
  OutputForm.Hide;
  OutputForm.Parent := nil;
  OutputForm.BorderStyle := bsSizeable;
  OutputForm.ClientWidth := OutputForm.OutputPages.Width;
  OutputForm.ClientHeight := OutputForm.OutputPages.Height;
  OutputForm.Position := poScreenCenter;

  { GrafikForm }
  GrafikForm.Hide;
  GrafikForm.Parent := nil;
  GrafikForm.BorderStyle := bsSizeable;
  GrafikForm.ClientWidth := GrafikForm.ViewTab.Width;
  GrafikForm.ClientHeight := GrafikForm.ViewTab.Height;
  GrafikForm.Position := poScreenCenter;

  Action := caFree;
end;

procedure TConsoleForm.FormCreate(Sender: TObject);
var
  temp: Integer;
begin
  ConsoleForm := self;

  FScale := MainVar.Scale;

  ClientWidth := Scale(788);
  ClientHeight :=  Scale(470);

  { GrafikForm }

  GrafikForm.Hide;
  GrafikForm.BorderStyle := bsNone;
  GrafikForm.Parent := ConsoleForm;
  GrafikForm.Position := poDesigned;
  GrafikForm.Left := Scale(6);
  GrafikForm.Top := Scale(8);
  GrafikForm.ClientWidth := Scale(305);
  GrafikForm.ClientHeight := Scale(457);
  GrafikForm.Visible := True;

  { InputForm }

  InputForm.Hide;
  InputForm.BorderStyle := bsNone;
  InputForm.Parent := ConsoleForm;
  InputForm.Position := poDesigned;
  InputForm.Left := Scale(318);
  InputForm.Top := Scale(8);
  InputForm.ClientHeight := Scale(195);
  InputForm.ClientWidth := Scale(465);
  InputForm.Visible := True;

  { OutputForm}

  temp := OutputForm.YComboBox.ItemIndex;
  if temp = -1 then
    temp := RiggModul.YComboSavedItemIndex;
  OutputForm.Hide;
  OutputForm.BorderStyle := bsNone;
  OutputForm.Parent := ConsoleForm;
  OutputForm.Position := poDesigned;
  OutputForm.Left := Scale(318);
  OutputForm.Top := Scale(210);
  OutputForm.ClientHeight := Scale(255);
  OutputForm.ClientWidth := Scale(465);
  OutputForm.YComboBox.ItemIndex := temp;
  OutputForm.Visible := True;

  RiggModul.ConsoleActive := True;
end;

function TConsoleForm.Scale(Value: Integer): Integer;
begin
  result := Round(Value * FScale);
end;

end.
