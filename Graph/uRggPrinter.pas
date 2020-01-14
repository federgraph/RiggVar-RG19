unit uRggPrinter;

interface

uses
  Printers;

type
  TRggPrinterStatus = (psUnknown, psPrinterOK, psNoPrinter);

  TRggPrinter = class
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
  RggPrinter: TRggPrinter;

implementation

function TRggPrinter.OKToPrint: Boolean;
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
  RggPrinter := TRggPrinter.Create;
  RggPrinter.PageWidth := 2400;
  RggPrinter.PageHeight := 3285;
  RggPrinter.PixPerInX := 300;
  RggPrinter.PixPerInY := 300;
finalization
  RggPrinter.Free;

end.
