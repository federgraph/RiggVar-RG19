unit RggPreview;

interface

uses
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  System.Types,
  Vcl.Graphics,
  Vcl.Printers,
  RggPrinter;

type
  TSwapType = (stLinksRum, stRechtsRum);

  TPreview = class(TObject)
  private
    FOrientation: TPrinterOrientation;

    function GetSheetSize: TPoint;
    function GetPageSize: TPoint;
    function GetEnvelopeSize: TPoint;

    function GetSheetPos: TRect;
    function GetPagePos: TRect;
    function GetEnvelopePos: TRect;

    procedure GetRPL;
    procedure SetOrientation(Value: TPrinterOrientation);
    procedure SetToLandscape;
    procedure SetToPortrait;

    function SwapTPoint(P: TPoint): TPoint;
    function SwapTRect
     (R: TRect; Bezug: TPoint; SwapType: TSwapType): TRect;

  public
    SheetSize: TPoint; { A4 - Abmessungen in pixel }
    PageSize: TPoint; { Printer.PageWidth x Printer.PageHeight in pixel }
    EnvSize: TPoint; { envelope - Abmessungen in pixel }
    PrintSize: TPoint; { envelope - Abmessungen in mm }

    SheetPos: TRect; { then sheet's position in pixel }
    PagePos: TRect; { the pages position in pixel }
    EnvPos: TRect; { the envelope's position in pixel }

    RPL01, RPL02, RPL03: array[1..5] of TPoint;
    RPLOffsetX, RPLOffsetY: Integer;
    Faktor: double;
    constructor Create;
    procedure Draw(Canvas: TCanvas);
    procedure Print;
    property Orientierung: TPrinterOrientation
      read FOrientation write SetOrientation;
  end;

implementation

constructor TPreview.Create;
begin
  try
  {Sheet}
  SheetSize := GetSheetSize;
  SheetPos := GetSheetPos;
  {Page}
  PageSize := GetPageSize;
  PagePos := GetPagePos;
  {Envelope}
  PrintSize.x := 210-50;
  PrintSize.y := 297-50;
  EnvSize := GetEnvelopeSize;
  EnvPos := GetEnvelopePos;

  FOrientation := poPortrait;
  GetRPL;

  RPLOffsetX := 0;
  RPLOffsetY := 0;
  Faktor := 1;
  except
  end;
end;

procedure TPreview.GetRPL;
begin
  RPL01[1].x := SheetPos.Left;
  RPL01[1].y := SheetSize.y - SheetPos.Top;
  RPL01[2].x := SheetPos.Left;
  RPL01[2].y := SheetSize.y - SheetPos.Bottom;
  RPL01[3].x := SheetPos.Right;
  RPL01[3].y := SheetSize.y - SheetPos.Bottom;
  RPL01[4].x := SheetPos.Right;
  RPL01[4].y := SheetSize.y - SheetPos.Top;
  RPL01[5].x := SheetPos.Left;
  RPL01[5].y := SheetSize.y - SheetPos.Top;

  RPL02[1].x := PagePos.Left;
  RPL02[1].y := SheetSize.y - PagePos.Top;
  RPL02[2].x := PagePos.Left;
  RPL02[2].y := SheetSize.y - PagePos.Bottom;
  RPL02[3].x := PagePos.Right;
  RPL02[3].y := SheetSize.y - PagePos.Bottom;
  RPL02[4].x := PagePos.Right;
  RPL02[4].y := SheetSize.y - PagePos.Top;
  RPL02[5].x := PagePos.Left;
  RPL02[5].y := SheetSize.y - PagePos.Top;

  RPL03[1].x := PagePos.Left + EnvPos.Left;
  RPL03[1].y := SheetSize.y - (PagePos.Top + EnvPos.Top);
  RPL03[2].x := PagePos.Left + EnvPos.Left;
  RPL03[2].y := SheetSize.y - (PagePos.Top + EnvPos.Bottom);
  RPL03[3].x := PagePos.Left + EnvPos.Right;
  RPL03[3].y := SheetSize.y - (PagePos.Top + EnvPos.Bottom);
  RPL03[4].x := PagePos.Left + EnvPos.Right;
  RPL03[4].y := SheetSize.y - (PagePos.Top + EnvPos.Top);
  RPL03[5].x := PagePos.Left + EnvPos.Left;
  RPL03[5].y := SheetSize.y - (PagePos.Top + EnvPos.Top);
end;

function TPreview.SwapTPoint(P: TPoint): TPoint;
begin
  Result.x := P.y;
  Result.y := P.x;
end;

function TPreview.SwapTRect(R: TRect; Bezug: TPoint; SwapType: TSwapType): TRect;
begin
  if SwapType = stRechtsRum then
  Result := Rect(
    Bezug.y - R.Bottom,
    R.Left,
    Bezug.y - R.Top,
    R.Right)
  else if SwapType = stLinksRum then
  Result := Rect(
    R.Top,
    Bezug.x - R.Right,
    R.Bottom,
    Bezug.x - R.Left);
end;

procedure TPreview.SetToLandscape;
begin
  EnvPos := SwapTRect(EnvPos, PageSize, stLinksRum);
  PagePos := SwapTRect(PagePos, SheetSize, stLinksRum);
  SheetSize := SwapTPoint(SheetSize);
  PageSize := SwapTPoint(PageSize);
  EnvSize := SwapTPoint(EnvSize);
  PrintSize := SwapTPoint(PrintSize);
  SheetPos := GetSheetPos;
  GetRPL;
end;

procedure TPreview.SetToPortrait;
begin
  EnvPos := SwapTRect(EnvPos, PageSize, stRechtsRum);
  PagePos := SwapTRect(PagePos, SheetSize, stRechtsRum);
  SheetSize := SwapTPoint(SheetSize);
  PageSize := SwapTPoint(PageSize);
  EnvSize := SwapTPoint(EnvSize);
  PrintSize := SwapTPoint(PrintSize);
  SheetPos := GetSheetPos;
  GetRPL;
end;

procedure TPreview.SetOrientation(Value: TPrinterOrientation);
begin
  if Value <> FOrientation then begin
    FOrientation := Value;
    if Value = poPortrait then SetToPortrait
    else SetToLandscape;
  end;
end;

function TPreview.GetSheetSize: TPoint;
{ Gets the sheet's size in pixels represented by a TPoint }
var
  SheetW, SheetH: Integer;
  PixPerInX: Integer;
  PixPerInY: Integer;
begin
  if RiggPrinter.OKToPrint then
  begin
    PixPerInX := GetDeviceCaps(Printer.Handle, LOGPIXELSX);
    PixPerInY := GetDeviceCaps(Printer.Handle, LOGPIXELSY);
    SheetW := trunc(210/25.4 * PixPerInX); {A4 Papier 210 mm breit}
    SheetH := trunc(297/25.4 * PixPerInY); {A4 Papier 297 mm hoch}
    Result := Point(SheetW, SheetH);
  end
  else
  begin
    PixPerInX := RiggPrinter.PixPerInX;
    PixPerInY := RiggPrinter.PixPerInY;
    SheetW := trunc(210/25.4 * PixPerInX); {A4 Papier 210 mm breit}
    SheetH := trunc(297/25.4 * PixPerInY); {A4 Papier 297 mm hoch}
    Result := Point(SheetW, SheetH);
  end;
end;

function TPreview.GetPageSize: TPoint;
begin
  if RiggPrinter.OKToPrint then
  begin
    Result.x := Printer.PageWidth;
    Result.y := Printer.PageHeight;
  end
  else
  begin
    Result.x := RiggPrinter.PageWidth;
    Result.y := RiggPrinter.PageHeight;
  end;
end;

function TPreview.GetEnvelopeSize: TPoint;
{ Gets the envelope's size in pixels represented by a TPoint }
{ PrintSize ist Envelope's Size in mm}
var
  EnvW, EnvH: Integer;
  PixPerInX: Integer;
  PixPerInY: Integer;
begin
  if RiggPrinter.OKToPrint then
  begin
    PixPerInX := GetDeviceCaps(Printer.Handle, LOGPIXELSX);
    PixPerInY := GetDeviceCaps(Printer.Handle, LOGPIXELSY);
    EnvW := trunc(PrintSize.x/25.4 * PixPerInX);
    EnvH := trunc(PrintSize.y/25.4 * PixPerInY);
    Result := Point(EnvW, EnvH);
  end
  else
  begin
    PixPerInX := RiggPrinter.PixPerInX;
    PixPerInY := RiggPrinter.PixPerInY;
    EnvW := trunc(PrintSize.x/25.4 * PixPerInX);
    EnvH := trunc(PrintSize.y/25.4 * PixPerInY);
    Result := Point(EnvW, EnvH);
  end;
end;

function TPreview.GetSheetPos: TRect;
begin
  Result := Rect(0, 0, SheetSize.x, SheetSize.y);
end;

function TPreview.GetPagePos: TRect;
{ Page Position in Bezug auf Sheet - vertikal 58 Pixel außermittig }
{ unten mehr Platz für Drucker-Rollen }
begin
  Result := Rect(
    (SheetSize.x - PageSize.x) div 2,
    (SheetSize.y - PageSize.y) div 2 - 58,
    (SheetSize.x - PageSize.x) div 2 + PageSize.x,
    (SheetSize.y - PageSize.y) div 2 + PageSize.y - 58);
end;

function TPreview.GetEnvelopePos: TRect;
{ Envelope Position in Bezug auf Page - vertikal 58 Pixel außermittig }
{ Asymetrie von Page kompensiert }
begin
  if RiggPrinter.OKToPrint then
  begin
    Result := Rect(
    (Printer.PageWidth  - EnvSize.x) div 2,
    (Printer.PageHeight - EnvSize.y) div 2 + 58,
    (Printer.PageWidth  - EnvSize.x) div 2 + EnvSize.X,
    (Printer.PageHeight - EnvSize.y) div 2 + EnvSize.Y + 58);
  end
  else
  begin
    Result := Rect(
    (RiggPrinter.PageWidth  - EnvSize.x) div 2,
    (RiggPrinter.PageHeight - EnvSize.y) div 2 + 58,
    (RiggPrinter.PageWidth  - EnvSize.x) div 2 + EnvSize.X,
    (RiggPrinter.PageHeight - EnvSize.y) div 2 + EnvSize.Y + 58);
  end;
end;

procedure TPreview.Draw(Canvas: TCanvas);
var
  i: Integer;
  RPL04, RPL05, RPL06: array[1..5] of TPoint;
begin
  for i := 1 to 5 do begin
    RPL04[i].x := Round(RPL01[i].x * Faktor) + RPLOffsetX;
    RPL04[i].y := Round(RPL01[i].y * Faktor) + RPLOffsetY;
    RPL05[i].x := Round(RPL02[i].x * Faktor) + RPLOffsetX;
    RPL05[i].y := Round(RPL02[i].y * Faktor) + RPLOffsetY;
    RPL06[i].x := Round(RPL03[i].x * Faktor) + RPLOffsetX;
    RPL06[i].y := Round(RPL03[i].y * Faktor) + RPLOffsetY;
  end;

  with Canvas do begin
    { Draw a rectangle that represents the Sheet }
    Pen.Color := clRed;
    PolyLine(RPL04);

    { Draw a rectangle that represents the Page }
    Pen.Color := clGray;
    PolyLine(RPL05);

    { Draw a rectangle that represents the envelope }
    Pen.Color := clBlue;
    PolyLine(RPL06);
  end;
end;

procedure TPreview.Print;
var
  RPL07: array[1..5] of TPoint;
begin
  RPL07[1].x := EnvPos.Left;
  RPL07[1].y := EnvPos.Top;
  RPL07[2].x := EnvPos.Left;
  RPL07[2].y := EnvPos.Bottom;
  RPL07[3].x := EnvPos.Right;
  RPL07[3].y := EnvPos.Bottom;
  RPL07[4].x := EnvPos.Right;
  RPL07[4].y := EnvPos.Top;
  RPL07[5].x := EnvPos.Left;
  RPL07[5].y := EnvPos.Top;

  if RiggPrinter.OkToPrint then
    with Printer.Canvas do
    begin
      { Draw a rectangle that represents the envelope }
      Pen.Color := clBlue;
      PolyLine(RPL07);
    end;
end;

end.
