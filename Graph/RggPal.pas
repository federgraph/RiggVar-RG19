unit RggPal;

interface

uses
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  Vcl.Controls,
  Vcl.Graphics;

type
  aRGBQuad = array [0 .. 255] of TRGBQuad;

procedure PaintBackGround(Image: TBitmap);
function CreateRggPal32: HPalette;
function CreateRggIdentPal: HPalette;

implementation

procedure PaintBackGround(Image: TBitmap);
var
  R: TRect;
begin
  R := Rect(0, 0, Image.Width, Image.Height);
  with Image.Canvas do
  begin
    Brush.Color := clBtnFace;
    FillRect(R);
  end;
end;
{$r-}

function CreateRggPal32: HPalette;
const
  NumColors = 32; // Anzahl der Stufen für Rot, Grün Blau und Grau
  NumEntries = 4 * NumColors; // Anzahl der Paletteneinträge
  n = 4; // Differenz für die RGB-Anteile
  base = 100; // Startwert {dunkelster Farbwert}
var
  i: Integer;
  pPal: PLogPalette;
  lSize: Integer;
begin
  lSize := SizeOf(TLogPalette) + (NumEntries - 1) * SizeOf(TPaletteEntry);
  GetMem(pPal, lSize);
  try
    pPal^.palVersion := $300;
    pPal^.palNumEntries := NumEntries;

    for i := 0 to NumColors - 1 do
    begin
      { Set up the shades of Gray }
      pPal^.palPalEntry[i].peRed := i * n + base;
      pPal^.palPalEntry[i].peGreen := i * n + base;
      pPal^.palPalEntry[i].peBlue := i * n + base;
      pPal^.palPalEntry[i].peFlags := PC_RESERVED;

      { Set up the shades of Red }
      pPal^.palPalEntry[i + NumColors].peRed := i * n + base;
      pPal^.palPalEntry[i + NumColors].peGreen := 20;
      pPal^.palPalEntry[i + NumColors].peBlue := 20;
      pPal^.palPalEntry[i + NumColors].peFlags := PC_RESERVED;

      { Set up the shades of Green }
      pPal^.palPalEntry[i + NumColors * 2].peRed := 20;
      pPal^.palPalEntry[i + NumColors * 2].peGreen := i * n + base;
      pPal^.palPalEntry[i + NumColors * 2].peBlue := 20;
      pPal^.palPalEntry[i + NumColors * 2].peFlags := PC_RESERVED;

      { Set up the shades of Blue }
      pPal^.palPalEntry[i + NumColors * 3].peRed := 20;
      pPal^.palPalEntry[i + NumColors * 3].peGreen := 20;
      pPal^.palPalEntry[i + NumColors * 3].peBlue := i * n + base;
      pPal^.palPalEntry[i + NumColors * 3].peFlags := PC_RESERVED;
    end;

    result := CreatePalette(pPal^);
    if result = 0 then
      raise EOutOfResources.Create('Cannot create Palette!');
  finally
    FreeMem(pPal, lSize);
  end;
end;

function CreateRggIdentPal: HPalette;
var
  i: Word;
  Log_Pal: PLogPalette;
  tempDC: HDC;
  Farbe, Start, Anzahl: Word;
  Palette: aRGBQuad;
begin
  result := 0;
  { Feste Farben }
  Palette[11].rgbRed := $0; { Fadenkreuz blau }
  Palette[11].rgbGreen := $0;
  Palette[11].rgbBlue := $FF;

  Palette[12].rgbRed := $FF; { Sterne gelb }
  Palette[12].rgbGreen := $FF;
  Palette[12].rgbBlue := 0;

  Palette[13].rgbRed := 0; { Sterne grün }
  Palette[13].rgbGreen := $FF;
  Palette[13].rgbBlue := 0;

  Palette[245].rgbRed := 0; { Hintergrund }
  Palette[245].rgbGreen := 0;
  Palette[245].rgbBlue := 0;

  { Farben für Schattierung }
  for i := 0 to 31 do
  begin
    { 32 Rotstufen (32 bis 63) }
    Palette[i + 32].rgbRed := i shl 2 + 131; { i*4 + 131 }
    Palette[i + 32].rgbGreen := 0;
    Palette[i + 32].rgbBlue := 0;
    { 32 Graustufen (64 bis 95) }
    Palette[i + 64].rgbRed := i shl 2 + 131;
    Palette[i + 64].rgbGreen := i shl 2 + 131;
    Palette[i + 64].rgbBlue := i shl 2 + 131;
  end;

  { Identity Palette erzeugen: }
  GetMem(Log_Pal, SizeOf(TLogPalette) + SizeOf(TPaletteEntry) * 255);
  tempDC := GetDC(0); { DC auf Bildschirm holen }
  try
    if (Log_Pal <> nil) and (tempDC <> 0) then
    begin
      with Log_Pal^ do
      begin
        palVersion := $300;
        palNumEntries := 256;
        { Systemfarben kopieren }
        Start := 0;
        Anzahl := 10;
        GetSystemPaletteEntries(tempDC, Start, Anzahl, palPalEntry[Start]);
        Start := 246;
        Anzahl := 10;
        GetSystemPaletteEntries(tempDC, Start, Anzahl, palPalEntry[Start]);
      end;

      { eigene Farben kopieren }
      for Farbe := 10 to 245 do
      begin
        Log_Pal^.palPalEntry[Farbe].peRed := Palette[Farbe].rgbRed;
        Log_Pal^.palPalEntry[Farbe].peGreen := Palette[Farbe].rgbGreen;
        Log_Pal^.palPalEntry[Farbe].peBlue := Palette[Farbe].rgbBlue;
        Log_Pal^.palPalEntry[Farbe].peFlags := PC_NOCOLLAPSE;
      end;

      { Systemfarben auch noch in der WinG-Palette vermerken: }
      with Log_Pal^ do
      begin
        for Farbe := 0 to 9 do
        begin
          Palette[Farbe].rgbRed := palPalEntry[Farbe].peRed;
          Palette[Farbe].rgbGreen := palPalEntry[Farbe].peGreen;
          Palette[Farbe].rgbBlue := palPalEntry[Farbe].peBlue;
        end;
        for Farbe := 246 to 255 do
        begin
          Palette[Farbe].rgbRed := palPalEntry[Farbe].peRed;
          Palette[Farbe].rgbGreen := palPalEntry[Farbe].peGreen;
          Palette[Farbe].rgbBlue := palPalEntry[Farbe].peBlue;
        end;
      end;

      result := CreatePalette(Log_Pal^);
      if result = 0 then
        raise EOutOfResources.Create('Cannot create Palette!');
    end;
  finally
    ReleaseDC(0, tempDC);
    FreeMem(Log_Pal, SizeOf(TLogPalette) + SizeOf(TPaletteEntry) * 255);
  end;
end;

{ vom WinG SDK:
  Clear the System Palette so that we can ensure an identity palette mapping for fast performance. }
procedure ClearSystemPalette;
type
  TDummyPalette = record
    Version: Word;
    NumberOfEntries: Word;
    aEntries: array [0 .. 255] of TPaletteEntry;
  end;
var
  Palette: TDummyPalette;
  ScreenPalette: HPalette;
  ScreenDC: HDC;
  Counter: Integer;
  pPal: PLogPalette;
begin
  Palette.Version := $300;
  Palette.NumberOfEntries := 256;

  { Reset everything in the system palette to black }
  for Counter := 0 to 255 do
  begin
    Palette.aEntries[Counter].peRed := 0;
    Palette.aEntries[Counter].peGreen := 0;
    Palette.aEntries[Counter].peBlue := 0;
    Palette.aEntries[Counter].peFlags := PC_NOCOLLAPSE;
  end;

  { Create, select, realize, deselect, and delete the palette }
  ScreenDC := GetDC(0);
  pPal := @Palette;
  ScreenPalette := CreatePalette(pPal^);

  if ScreenPalette <> 0 then
  begin
    ScreenPalette := SelectPalette(ScreenDC, ScreenPalette, FALSE);
    RealizePalette(ScreenDC);
    ScreenPalette := SelectPalette(ScreenDC, ScreenPalette, FALSE);
    DeleteObject(ScreenPalette);
  end;

  ReleaseDC(0, ScreenDC);
end;

end.
