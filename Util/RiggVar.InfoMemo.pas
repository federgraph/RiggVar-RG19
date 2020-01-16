unit RiggVar.InfoMemo;

interface

uses
  Windows, SysUtils, Classes, Forms;

type
  TInfoMemo = class
  private
    FInfo: pointer;
    FInfoSize: longint;
    FLang: PInteger;
    function GetFileVersion: string;
  public
    procedure Fill(ML: TStrings);
  end;

implementation

const
  vqvFmt = '\StringFileInfo\%4.4x%4.4x\%s';
  FileVersionToken = 'FileVersion';

{ TInfoMemo }

procedure TInfoMemo.Fill(ML: TStrings);
var
  FName: string;
  ApplicationName: string;
  ApplicationDir: string;
  FileVersion: string;
  ProductName: string;
  Description: string;
begin
  ML.Clear;
  ProductName := 'RiggVar RG';
  Description := 'Rigg Application';
  FName := Application.ExeName;
  ApplicationName := ExtractFileName(FName);
  ApplicationDir := ExtractFilePath(ParamStr(0));
  FileVersion := GetFileVersion;

  ML.Add('product name: ' + ProductName);
  ML.Add('description: ' +  Description);
  ML.Add('');
  ML.Add('dir: ' + ApplicationDir);
  ML.Add('application name: ' + ApplicationName);
  ML.Add('file version: ' + FileVersion);
  ML.Add('');
  ML.Add('(c) Gustav Schubert, 1996 - 2020');
  ML.Add('federgraph.de');
end;

function TInfoMemo.GetFileVersion: string;
var
  FName: string;
  vlen: DWord;
  vptr: pchar;

  function GetEntry(KName: string): string;
  begin
    if VerQueryValue(FInfo,
      pchar(Format(vqvFmt, [LoWord(FLang^), HiWord(FLang^), KName])),
      pointer(vptr), vlen)
    then
      result := vptr
    else
      result := '';
  end;

begin
  result := '';
  FName := Application.ExeName;
  FInfoSize := GetFileVersionInfoSize(pchar(FName), vlen);
  if FInfoSize > 0 then
  begin
    GetMem(FInfo, FInfoSize);
    if GetFileVersionInfo(pchar(fname), vlen, FInfoSize, FInfo) then
    begin
      VerQueryValue(FInfo, '\VarFileInfo\Translation', pointer(FLang), vlen);
      result := GetEntry(FileVersionToken);
    end;
  end;
  if FInfoSize > 0 then
    FreeMem(FInfo, FInfoSize);
end;

end.
