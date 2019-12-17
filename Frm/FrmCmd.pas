unit FrmCmd;

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
  StdCtrls,
  ExtCtrls;

const
  SFormatError = 'Syntaxfehler'; { Format Error }
  SRangeError = 'Bereichsfehler'; { Range Error }
  SValueError = 'Wert ungültig';
  SParamError = 'Zuweisung verboten';

type
  TCommandForm = class(TForm)
    PanelEdit: TPanel;
    PromptEdit: TEdit;
    InputEdit: TEdit;
    OutputMemo: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure InputEditKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    EditLine: TStringList;
    Buffer: String;
    BufferName: String;
    BufferValue: String;
    function ProcessInput: String;
    procedure wmGetMinMaxInfo(var Msg: TMessage);
    message wm_GetMinMaxInfo;
  end;

var
  CommandForm: TCommandForm;

implementation

uses
  RiggVar.RG.Def,
  FrmAniRot,
  FrmAni,
  RggTypes;

{$R *.DFM}

procedure TCommandForm.wmGetMinMaxInfo(var Msg: TMessage);
begin
  PMinMaxInfo(Msg.lParam)^.ptMinTrackSize.X := 220;
  PMinMaxInfo(Msg.lParam)^.ptMinTrackSize.Y := 150;
end;

procedure TCommandForm.FormCreate(Sender: TObject);
begin
  InputEdit.Text := '';
  OutputMemo.Lines.Clear;
  EditLine := TStringList.Create;
end;

procedure TCommandForm.FormDestroy(Sender: TObject);
begin
  EditLine.Free;
end;

procedure TCommandForm.FormResize(Sender: TObject);
begin
  InputEdit.Width := PanelEdit.Width - InputEdit.Left;
end;

procedure TCommandForm.FormHide(Sender: TObject);
begin
  AniRotationForm.CommandLineItem.Checked := False;
end;

procedure TCommandForm.InputEditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  i: Integer;
begin
  if Key = VK_RETURN then
  begin
    EditLine.Clear;
    EditLine.Add(InputEdit.Text);
    Buffer := EditLine[0]; // UpperCase(EditLine[0]);
    BufferName := EditLine.Names[0]; // UpperCase(EditLine.Names[0]);
    BufferValue := EditLine.Values[BufferName];
    // UpperCase(EditLine.Values[BufferName]);
    Buffer := Trim(Buffer);
    BufferName := Trim(BufferName);
    BufferValue := Trim(BufferValue);

    with OutputMemo.Lines do
    begin
      BeginUpdate;
      try
        if Count > 50 then
          for i := 1 to 10 do
            Delete(0);
        Add('Eingabe : ' + EditLine[0]); { nicht nur Großbuchstaben }
        Add('Befehl: ' + BufferName);
        Add('Wert: ' + BufferValue);
        Add('Ergebnis: ' + ProcessInput) finally EndUpdate;
      end;
      Add('');
    end;
  end;
end;

function TCommandForm.ProcessInput: String;
var
  temp: real;
  Code: Integer;
  Param: TsbName;
begin
  result := SFormatError;

  // *** Test auf Eingabe ohne '=' (Anfrage)
  Param := AniRotationForm.Text2Param(Buffer); // Default: WPowerOS
  if Param <> fpWPowerOS then
  begin
    if Param = fpWinkel then
      result := Format('%s = %6.2f E-1Grad', [Buffer,
        AniRotationForm.ParamProp[Param]])
    else
      result := Format
        ('%s = %6.2f mm', [Buffer, AniRotationForm.ParamProp[Param]]);
    Exit;
  end;

  // *** Test auf korrekte Eingabe mit '='
  Param := AniRotationForm.Text2Param(BufferName); // Default: WPowerOS
  if Param <> fpWPowerOS then
  begin
    // *** Anfrage mit '='
    if BufferValue = '?' then
    begin
      if Param = fpWinkel then
        result := Format('%s = (%d ... %6.2f ... %d) E-1Grad', [BufferName,
          AniRotationForm.ParamMin[Param], AniRotationForm.ParamProp[Param],
          AniRotationForm.ParamMax[Param]])
      else
        result := Format('%s = (%d ... %6.2f ... %d) mm', [BufferName,
          AniRotationForm.ParamMin[Param], AniRotationForm.ParamProp[Param],
          AniRotationForm.ParamMax[Param]]);
      Exit;
    end;

    // *** Werteingabe
    Val(BufferValue, temp, Code);
    // Falscher Wert
    if Code <> 0 then
    begin
      result := SValueError;
      Exit;
    end;
    // Falscher Parameter. Zuweisen an den Parameter nicht erlaubt
    if AniRotationForm.ListBox.Items.IndexOf(AniRotationForm.Param2Text(Param))
      = -1 then
    begin
      result := SParamError;
      Exit;
    end;
    // Versuch der Zuweisung
    AniRotationForm.ParamProp[Param] := temp;
    if temp <> AniRotationForm.ParamProp[Param] then
    begin
      result := SRangeError;
      Exit;
    end;
    // nach erfogreicher Wertveränderung GUI aktualisieren
    if Param = fpWinkel then
    begin
      result := Format('%s = %6.2f E-1Grad', [BufferName,
        AniRotationForm.ParamProp[Param]]);
      AnimationForm.tbWinkel.Position := Round(temp);
    end
    else
      result := Format('%s = %6.2f mm', [BufferName,
        AniRotationForm.ParamProp[Param]]);

  end;
end;

procedure TCommandForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    Hide;
  if (ssCtrl in Shift) and (Key = VK_TAB) then
    if AniRotationForm.Visible then
      AniRotationForm.SetFocus
    else if AnimationForm.Visible then
      AnimationForm.SetFocus;
end;

(*
  function TCommandForm.Text2Param(T: String): TsbName;
  begin
  result := WPowerOS;
  if T = 'Controller' then result := Controller
  else if T = 'Winkel' then result := Winkel
  else if T = 'Vorstag' then result := Vorstag
  else if T = 'Wante' then result := Wante
  else if (T = 'Wante oben') or (T = 'Woben') then result := Woben
  else if (T = 'Saling Höhe') or (T = 'SalingH') then result := SalingH
  else if (T = 'Saling Abstand') or (T = 'SalingA') then result := SalingA
  else if (T = 'Saling Länge') or (T = 'SalingL') then result := SalingL;
  end;

  function TCommandForm.Param2Text(P: TsbName): String;
  begin
  result := '';
  if P = Controller then result := 'Controller'
  else if P = Winkel then result := 'Winkel'
  else if P = Vorstag then result := 'Vorstag'
  else if P = Wante then result := 'Wante'
  else if P = Woben then result := 'Want oben'
  else if P = SalingH then result := 'SalingH'
  else if P = SalingA then result := 'SalingA'
  else if P = SalingL then result := 'SalingL';
  end;
*)

end.
