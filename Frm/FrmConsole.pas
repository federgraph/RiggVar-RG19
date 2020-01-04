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
  end;

var
  ConsoleForm: TConsoleForm;

implementation

uses
  Riggunit,
  FrmInput,
  FrmOutput,
  FrmGrafic;

{$R *.DFM}

procedure TConsoleForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  RiggModul.ConsoleActive := False;

  with InputForm do
  begin
    Hide;
    Parent := nil;
    BorderStyle := bsSizeable; //bsDialog;
    ClientWidth := InputForm.InputPages.Width;
    ClientHeight := InputForm.InputPages.Height;
    Position := poScreenCenter;
  end;

  with OutputForm do
  begin
    { Das WindowHandle von YComboBox wird zerstört, wenn ConsoleForm freigegeben
      wird. Wenn OutputForm dann neu angezeigt wird ist YComboBox.ItemIndex = -1,
      da ItemIndex der ComboBox von VCL nicht gesichert wird im Gegensatz zum
      ItemIndex einer ListBox! YComboBox.ItemIndex wird außerdem als Index in
      ein Array verwendet und dieser darf nicht Null sein. }
    RiggModul.YComboSavedItemIndex := YComboBox.ItemIndex;
    Hide;
    Parent := nil;
    BorderStyle := bsSizeable; //bsDialog;
    ClientWidth := OutputForm.OutputPages.Width;
    ClientHeight := OutputForm.OutputPages.Height;
    Position := poScreenCenter;
  end;

  with GrafikForm do
  begin
    Hide;
    Parent := nil;
    BorderStyle := bsSizeable; //bsDialog;
    ClientWidth := GrafikForm.ViewTab.Width;
    ClientHeight := GrafikForm.ViewTab.Height;
    Position := poScreenCenter;
  end;
  Action := caFree;
end;

procedure TConsoleForm.FormCreate(Sender: TObject);
var
  temp: Integer;
begin
  ConsoleForm := Self;

  ClientWidth := 788;
  ClientHeight :=  470;

  with GrafikForm do
  begin
    Hide;
    BorderStyle := bsNone;
    Parent := ConsoleForm;
    Position := poDesigned;
    { funktioniert nicht:
      diffX := 293 - PaintBoxG.ClientWidth;
      diffY := 422 - PaintBoxG.ClientHeight;
      SetBounds(8, 8, ViewTab.Width + diffX, ViewTab.Height + diffY);
      }
    Left := 6;
    Top := 8;
    ClientWidth := 305;
    ClientHeight := 457;

    Visible := True;
  end;

  with InputForm do
  begin
    Hide;
    BorderStyle := bsNone;
    Parent := ConsoleForm;
    Position := poDesigned;
    { funktioniert nicht:
      diffX := 457 - pnOhne.ClientWidth;
      diffY := 164 - pnOhne.ClientHeight;
      SetBounds(GrafikForm.Left + GrafikForm.Width + 12, 8,
      InputPages.Width + diffX, InputPages.Height + diffY);
      }
    Left := 318;
    Top := 8;
    ClientHeight := 195;
    ClientWidth := 465;

    Visible := True;
  end;

  with OutputForm do
  begin
    temp := YComboBox.ItemIndex;
    if temp = -1 then
      temp := RiggModul.YComboSavedItemIndex;
    Hide;
    BorderStyle := bsNone;
    Parent := ConsoleForm;
    Position := poDesigned;
    { funktioniert nicht:
      diffX := 453 - SalingPaintBox.ClientWidth;
      diffY := 220 - SalingPaintBox.ClientHeight;
      SetBounds(InputForm.Left,
      InputForm.Top + InputForm.Height + 12,
      OutputPages.Width + diffX, OutputPages.Height + diffY);
      }
    Left := 318;
    Top := 210;
    ClientHeight := 255;
    ClientWidth := 465;

    YComboBox.ItemIndex := temp;
    Visible := True;
  end;

  RiggModul.ConsoleActive := True;
end;

end.
