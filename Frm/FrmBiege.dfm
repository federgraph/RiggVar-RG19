object BiegeUndNeigeForm: TBiegeUndNeigeForm
  Left = 348
  Top = 211
  BorderStyle = bsDialog
  Caption = 'Mast Biegen und Neigen'
  ClientHeight = 107
  ClientWidth = 324
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  Scaled = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lbMastfall: TLabel
    Left = 152
    Top = 16
    Width = 45
    Height = 13
    Caption = 'lbMastfall'
  end
  object lbBiegungS: TLabel
    Left = 152
    Top = 40
    Width = 52
    Height = 13
    Caption = 'lbBiegungS'
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
  object sbBiegungS: TScrollBar
    Left = 16
    Top = 40
    Width = 121
    Height = 17
    PageSize = 0
    TabOrder = 1
    OnScroll = sbMastfallScroll
  end
  object BiegeBtn: TBitBtn
    Left = 56
    Top = 72
    Width = 153
    Height = 25
    Caption = 'Biegen und Neigen'
    TabOrder = 2
    OnClick = BiegeBtnClick
  end
  object OK: TBitBtn
    Left = 224
    Top = 72
    Width = 81
    Height = 25
    Kind = bkOK
    NumGlyphs = 2
    TabOrder = 3
  end
end
