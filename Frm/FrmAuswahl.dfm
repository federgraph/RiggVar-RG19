object YAuswahlDlg: TYAuswahlDlg
  Left = 264
  Top = 162
  ActiveControl = OKBtn
  BorderStyle = bsDialog
  Caption = 'Kurven f'#252'r Y-Achse'
  ClientHeight = 253
  ClientWidth = 347
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object SrcLabel: TLabel
    Left = 8
    Top = 8
    Width = 145
    Height = 16
    AutoSize = False
    Caption = 'verf'#252'gbar'
    IsControl = True
  end
  object DstLabel: TLabel
    Left = 192
    Top = 8
    Width = 145
    Height = 16
    AutoSize = False
    Caption = 'ausgew'#228'hlt'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    IsControl = True
  end
  object IncludeBtn: TSpeedButton
    Left = 160
    Top = 32
    Width = 25
    Height = 25
    Caption = '>'
    Enabled = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    OnClick = IncludeBtnClick
    IsControl = True
  end
  object IncAllBtn: TSpeedButton
    Left = 160
    Top = 64
    Width = 25
    Height = 25
    Caption = '>>'
    Enabled = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    OnClick = IncAllBtnClick
    IsControl = True
  end
  object ExcludeBtn: TSpeedButton
    Left = 160
    Top = 96
    Width = 25
    Height = 25
    Caption = '<'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    OnClick = ExcludeBtnClick
    IsControl = True
  end
  object ExAllBtn: TSpeedButton
    Left = 160
    Top = 128
    Width = 25
    Height = 25
    Caption = '<<'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    OnClick = ExcAllBtnClick
    IsControl = True
  end
  object OKBtn: TBitBtn
    Left = 64
    Top = 220
    Width = 69
    Height = 27
    Kind = bkOK
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 0
    IsControl = True
  end
  object CancelBtn: TBitBtn
    Left = 144
    Top = 220
    Width = 97
    Height = 27
    Kind = bkCancel
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 1
    IsControl = True
  end
  object HelpBtn: TBitBtn
    Left = 252
    Top = 220
    Width = 77
    Height = 27
    Caption = 'Hilfe'
    Kind = bkHelp
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 2
    IsControl = True
  end
  object SrcList: TListBox
    Left = 8
    Top = 24
    Width = 144
    Height = 185
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = []
    MultiSelect = True
    ParentFont = False
    Sorted = True
    TabOrder = 3
    IsControl = True
  end
  object DstList: TListBox
    Left = 192
    Top = 24
    Width = 144
    Height = 185
    ItemHeight = 13
    Items.Strings = (
      'Durchbiegung hd'
      'Elastizit'#228't Punkt C'
      'Mastfall F0C'
      'Mastfall F0F'
      'Vorstag-Spannung'
      'Wanten-Spannung')
    MultiSelect = True
    TabOrder = 4
    IsControl = True
  end
end
