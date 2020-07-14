object BiegeUndNeigeForm: TBiegeUndNeigeForm
  Left = 348
  Top = 211
  BorderStyle = bsDialog
  Caption = 'Mast Biegen und Neigen'
  ClientHeight = 107
  ClientWidth = 410
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
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
  object lbBiegung: TLabel
    Left = 152
    Top = 40
    Width = 46
    Height = 13
    Caption = 'lbBiegung'
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
  object sbBiegung: TScrollBar
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
    Top = 74
    Width = 153
    Height = 25
    Caption = 'Biegen und Neigen'
    TabOrder = 2
    OnClick = BiegeBtnClick
  end
  object OK: TBitBtn
    Left = 215
    Top = 74
    Width = 81
    Height = 25
    Kind = bkOK
    NumGlyphs = 2
    TabOrder = 3
  end
end
