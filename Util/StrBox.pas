unit StrBox;

interface

function CleanString(S: String): String;
function GetFirstWord(S: String): String;
function StripBlanks(var S: String): String;
function StripFirstWord(var S : String) : String;
function StripFrontChars(var S: String; Ch: Char): String;

implementation

uses SysUtils, Classes;

{----------------------------------------------------
Description: Erase blanks from end and beginning
-----------------------------------------------------}
function CleanString(S: String): String;
var
  Temp: String;
begin
  Temp := StripFrontChars(S, #32);
  Temp := StripBlanks(Temp);
  CleanString := Temp;
end;

{----------------------------------------------------
Description: Get the first word from a string
-----------------------------------------------------}
function GetFirstWord(S : String) : String;
var
  i : Integer;
  S1: String;
begin
  SetLength(S1, Length(S)); {ausreichend Speicher zuweisen für AnsiString!}
  i := 1;
  while (S[i] <> ' ') and (i < Length(S)) do begin
     S1[i] := S[i];
     Inc(i);
  end;
  Dec(i);
  SetLength(S1, i);
  Result := S1;
end;

{----------------------------------------------------
Description: Strip any stray spaces from the end
-----------------------------------------------------}
function StripBlanks(var S: String): String;
var
  i: Integer;
begin
  i := Length(S);
  while S[i] = ' ' do begin
    Delete(S,i,1);
    Dec(i);
  end;
  Result := S;
end;

{----------------------------------------------------
Strip the first word from a sentence S,
return word S1 and a shortened sentence S.
Return an empty string S1 if there is no first word.
-----------------------------------------------------}
function StripFirstWord(var S : String) : String;
var
  i, Size: Integer;
  S1: String;
begin
  i := Pos(#32, S);
  if i = 0 then begin
    Result := '';
    Exit; {Kein erstes Wort, Satz bleibt gleich}
  end;

  {Erstes Wort:}
  SetLength(S1, i-1); {Speicher reservieren!}
  Move(S[1], S1[1], i-1);

  {Verkürzter Satz:}
  Size := (Length(S) - i);
  Move(S[i + 1], S[1], Size);
  SetLength(S, Size);

  Result := S1;
end;

{----------------------------------------------------
Strips any occurances of charact Ch that might precede a string.
-----------------------------------------------------}
function StripFrontChars(var S: String; Ch: Char): String;
begin
  while (S[1] = Ch) and (Length(S) > 0) do
    S := Copy(S, 2, Length(S) - 1);
  Result := S;
end;

end.
