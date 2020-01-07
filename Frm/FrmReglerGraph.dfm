inherited CtrlDlg1: TCtrlDlg1
  Caption = 'Trimm Einstellen'
  ClientHeight = 347
  ClientWidth = 333
  OnPaint = FormPaint
  ExplicitWidth = 339
  ExplicitHeight = 376
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel [0]
    Left = 15
    Top = 151
    Width = 307
    Height = 187
  end
  object RegelPaintBox: TPaintBox [1]
    Left = 16
    Top = 152
    Width = 305
    Height = 185
    Color = clWhite
    ParentColor = False
  end
  inherited LoopBtn: TBitBtn
    Left = 144
    ExplicitLeft = 144
  end
  inherited OK: TBitBtn
    Left = 232
    ExplicitLeft = 232
  end
end
