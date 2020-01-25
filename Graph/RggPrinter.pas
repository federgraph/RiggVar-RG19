unit RggPrinter;

interface

uses
  Printers;

type
  TRggPrinterStatus = (psUnknown, psPrinterOK, psNoPrinter);

  TRiggPrinter = class
  private
    RggPrinterStatus: TRggPrinterStatus;
  public
    PageWidth: Integer;
    PageHeight: Integer;
    PixPerInX: Integer;
    PixPerInY: Integer;
    PrintingDisabled: Boolean;
    function OKToPrint: Boolean;
  end;

var
  RiggPrinter: TRiggPrinter;

implementation

function TRiggPrinter.OKToPrint: Boolean;
var
  P: TPrinter;
begin
  result := False;
  if Printingdisabled then
    Exit;

  if RggPrinterStatus = psUnknown then
  try
    P := Printer;
    if (P <> nil)
      and (P.Printers.Count > 0)
      and (P.Canvas <> nil)
    then
      RggPrinterStatus := psPrinterOK
    else
      RggPrinterStatus := psNoPrinter;
  except
    RggPrinterStatus := psNoPrinter;
  end;

  result := RggPrinterStatus = psPrinterOK;
end;

initialization
  RiggPrinter := TRiggPrinter.Create;
  RiggPrinter.PageWidth := 2400;
  RiggPrinter.PageHeight := 3285;
  RiggPrinter.PixPerInX := 300;
  RiggPrinter.PixPerInY := 300;

finalization
  RiggPrinter.Free;

end.
