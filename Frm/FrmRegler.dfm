object FormRegler: TFormRegler
  Left = 331
  Top = 172
  BorderStyle = bsDialog
  Caption = 'Trimm - Regelschleife'
  ClientHeight = 152
  ClientWidth = 330
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object lbMastfall: TLabel
    Left = 152
    Top = 16
    Width = 45
    Height = 13
    Caption = 'lbMastfall'
  end
  object lbSpannung: TLabel
    Left = 152
    Top = 40
    Width = 56
    Height = 13
    Caption = 'lbSpannung'
  end
  object lbBiegungS: TLabel
    Left = 152
    Top = 64
    Width = 52
    Height = 13
    Caption = 'lbBiegungS'
  end
  object lbZaehler: TLabel
    Left = 16
    Top = 96
    Width = 57
    Height = 13
    Caption = 'Z'#228'hlerstand'
  end
  object sbMastfall: TScrollBar
    Left = 16
    Top = 16
    Width = 121
    Height = 17
    PageSize = 0
    TabOrder = 0
    OnScroll = sbMastfallScroll
  end
  object sbSpannung: TScrollBar
    Left = 16
    Top = 40
    Width = 121
    Height = 17
    PageSize = 0
    TabOrder = 1
    OnScroll = sbMastfallScroll
  end
  object sbBiegungS: TScrollBar
    Left = 16
    Top = 64
    Width = 121
    Height = 17
    PageSize = 0
    TabOrder = 2
    OnScroll = sbMastfallScroll
  end
  object ZaehlerEdit: TEdit
    Left = 16
    Top = 112
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
