object FormReglerGraph: TFormReglerGraph
  Left = 0
  Top = 0
  Caption = 'Trimm Einstellen'
  ClientHeight = 369
  ClientWidth = 347
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  OnCreate = FormCreate
  OnPaint = FormPaint
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object RegelPaintBox: TPaintBox
    Left = 8
    Top = 153
    Width = 305
    Height = 185
    Color = clWhite
    ParentColor = False
  end
  object lbMastfall: TLabel
    Left = 148
    Top = 8
    Width = 45
    Height = 13
    Caption = 'lbMastfall'
  end
  object lbSpannung: TLabel
    Left = 144
    Top = 35
    Width = 56
    Height = 13
    Caption = 'lbSpannung'
  end
  object lbBiegungS: TLabel
    Left = 148
    Top = 64
    Width = 52
    Height = 13
    Caption = 'lbBiegungS'
  end
  object lbZaehler: TLabel
    Left = 242
    Top = 8
    Width = 57
    Height = 13
    Caption = 'Z'#228'hlerstand'
  end
  object sbMastfall: TScrollBar
    Left = 8
    Top = 8
    Width = 121
    Height = 17
    PageSize = 0
    TabOrder = 0
  end
  object sbSpannung: TScrollBar
    Left = 8
    Top = 35
    Width = 121
    Height = 17
    PageSize = 0
    TabOrder = 1
  end
  object sbBiegungS: TScrollBar
    Left = 8
    Top = 64
    Width = 121
    Height = 17
    PageSize = 0
    TabOrder = 2
  end
  object ZaehlerEdit: TEdit
    Left = 240
    Top = 32
    Width = 73
    Height = 21
    TabOrder = 3
    Text = 'ZaehlerEdit'
  end
  object LoopBtn: TBitBtn
    Left = 136
    Top = 112
    Width = 75
    Height = 25
    Caption = 'Regeln'
    TabOrder = 4
    OnClick = LoopBtnClick
  end
  object OK: TBitBtn
    Left = 224
    Top = 112
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    NumGlyphs = 2
    TabOrder = 5
  end
end
