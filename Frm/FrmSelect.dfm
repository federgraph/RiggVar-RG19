object MemoDlg: TMemoDlg
  Left = 334
  Top = 148
  ActiveControl = OKBtn
  BorderStyle = bsDialog
  Caption = 'Report - Elemente'
  ClientHeight = 255
  ClientWidth = 347
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object SrcLabel: TLabel
    Left = 8
    Top = 8
    Width = 145
    Height = 16
    AutoSize = False
    Caption = 'verf'#252'gbar:'
    IsControl = True
  end
  object DstLabel: TLabel
    Left = 192
    Top = 8
    Width = 145
    Height = 16
    AutoSize = False
    Caption = 'aktiviert:'
    IsControl = True
  end
  object IncludeBtn: TSpeedButton
    Left = 160
    Top = 32
    Width = 25
    Height = 25
    Caption = '>'
    OnClick = IncludeBtnClick
    IsControl = True
  end
  object IncAllBtn: TSpeedButton
    Left = 160
    Top = 64
    Width = 25
    Height = 25
    Caption = '>>'
    OnClick = IncAllBtnClick
    IsControl = True
  end
  object ExcludeBtn: TSpeedButton
    Left = 160
    Top = 96
    Width = 25
    Height = 25
    Caption = '<'
    OnClick = ExcludeBtnClick
    IsControl = True
  end
  object ExAllBtn: TSpeedButton
    Left = 160
    Top = 128
    Width = 25
    Height = 25
    Caption = '<<'
    OnClick = ExcAllBtnClick
    IsControl = True
  end
  object OKBtn: TBitBtn
    Left = 60
    Top = 220
    Width = 77
    Height = 27
    Kind = bkOK
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 0
    IsControl = True
  end
  object CancelBtn: TBitBtn
    Left = 152
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
    Left = 260
    Top = 220
    Width = 77
    Height = 27
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
    ItemHeight = 13
    Items.Strings = (
      'FW_Auflagerkraefte'
      'FW_Belastung'
      'FW_Elastizitaeten'
      'FW_Geometrie'
      'FW_Koordinaten'
      'FW_Stabkraefte'
      'FW_StabQuerschnitte'
      'FW_Verschiebungen'
      'SalingDaten'
      'TrimmControlls'
      'Winkel')
    MultiSelect = True
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
      'rF'
      'rL'
      'rLe'
      'DiffL'
      'rP'
      'rPe'
      'DiffP')
    MultiSelect = True
    TabOrder = 4
    IsControl = True
  end
end
