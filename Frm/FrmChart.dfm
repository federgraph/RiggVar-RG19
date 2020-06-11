object ChartForm: TChartForm
  Left = 202
  Top = 165
  HorzScrollBar.Margin = 1
  VertScrollBar.Margin = 1
  Caption = 'Diagramm'
  ClientHeight = 480
  ClientWidth = 800
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Icon.Data = {
    0000010001002020100000000000E80200001600000028000000200000004000
    0000010004000000000080020000000000000000000000000000000000000000
    0000000080000080000000808000800000008000800080800000C0C0C0008080
    80000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF000000
    00000000000000000000000000000EEEEEEEEEEEEEEEEEEEEEEEEEEEEEE00EEE
    E0EEEEEEEEEEEEEEEEEEEEEEEEE00EEEE0EEEEEEEEEEEEEEEEEEEEEEEEE00EEE
    E0EEEEEEEEEEEEEEEEEEEEEEEEE00E0000000000000000000000000000E00EEE
    E0EEEEEEEEEEEEEEEEEEEEEEEEE00EEEE0EEEEEEEEEEEEEEEEEEEEEEEEE00EEE
    E0EEEEEEEEEEEE9EEEEEEEEEEEE00EEEE0EEEEEEEEEEE999EEEEEEEEEEE00EEE
    E0EEEEEEEEEE99999EEEEEEEEEE00EEEE0EEEEEEEEE999E999EEEEEEEEE00ECC
    E0EEEEEEEE999EEE999EEEEEEEE00ECCC0EEEEEEE999EEEEE999EEEEEEE00EEC
    CCEEEEEE999EEEEEEE999EEEEEE00EEECCCEEEE999EEEEEEEEE999EEEEE00EEE
    ECCCEE999EEEEEEEEEEE999EEEE00EEEE0CCC999EEEEEEEEEEEEE999EEE00EEE
    E0EC9C9EEEEEEEEEEEEEEE999EE00EEEE0E9C9CEEEEEEEEEEECEEEE99EE00EEE
    E0999CCCEEEEEEEEECCCEEEEEEE00EEEE999EECCCEEEEEEECCCCCEEEEEE00EEE
    999EEEECCCEEEEECCCECCCEEEEE00EE999EEEEEECCCEEECCCEEECCCEEEE00E99
    90EEEEEEECCCECCCEEEEECCCEEE00E99E0EEEEEEEECCCCCEEEEEEECCCEE00EEE
    E0EEEEEEEEECCCEEEEEEEEECCEE00EEEE0EEEEEEEEEECEEEEEEEEEEEEEE00EEE
    E0EEEEEEEEEEEEEEEEEEEEEEEEE00EEEE0EEEEEEEEEEEEEEEEEEEEEEEEE00EEE
    EEEEEEEEEEEEEEEEEEEEEEEEEEE0000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    000000000000000000000000000000000000000000000000000000000000}
  OldCreateOrder = False
  Position = poDefault
  Scaled = False
  Visible = True
  OnActivate = FormActivate
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnPaint = FormPaint
  PixelsPerInch = 96
  TextHeight = 13
  object lbXLeft: TLabel
    Left = 80
    Top = 446
    Width = 22
    Height = 13
    Caption = 'Xmin'
  end
  object lbAchseX: TLabel
    Left = 280
    Top = 446
    Width = 43
    Height = 13
    Caption = 'lbAchseX'
  end
  object lbXRight: TLabel
    Left = 671
    Top = 446
    Width = 26
    Height = 13
    Alignment = taRightJustify
    Caption = 'Xmax'
  end
  object ChartPaintBox: TPaintBox
    Left = 80
    Top = 182
    Width = 601
    Height = 257
    OnPaint = ChartPaintBoxPaint
  end
  object PaintBoxLegend: TPaintBox
    Left = 688
    Top = 182
    Width = 98
    Height = 257
    OnPaint = PaintBoxLegendPaint
  end
  object ChartBevelInner: TBevel
    Left = 176
    Top = 246
    Width = 297
    Height = 145
  end
  object lbParam: TLabel
    Left = 738
    Top = 158
    Width = 38
    Height = 13
    Alignment = taRightJustify
    Caption = 'lbParam'
  end
  object ChartBevelOuter: TBevel
    Left = 8
    Top = 152
    Width = 780
    Height = 314
    Style = bsRaised
  end
  object BedienPanel: TPanel
    Left = 0
    Top = 0
    Width = 800
    Height = 146
    BevelOuter = bvNone
    TabOrder = 0
    object BevelCtrls: TBevel
      Left = 400
      Top = 80
      Width = 390
      Height = 65
      Style = bsRaised
    end
    object PBevel: TBevel
      Left = 8
      Top = 80
      Width = 385
      Height = 65
      Style = bsRaised
    end
    object XBevel: TBevel
      Left = 8
      Top = 8
      Width = 385
      Height = 65
      Style = bsRaised
    end
    object BevelY: TBevel
      Left = 400
      Top = 8
      Width = 390
      Height = 65
      Style = bsRaised
    end
    object YComboLabel: TLabel
      Left = 416
      Top = 14
      Width = 45
      Height = 13
      Caption = 'y - Achse'
    end
    object YMinLabel: TLabel
      Left = 592
      Top = 14
      Width = 16
      Height = 13
      Caption = 'Min'
    end
    object YMaxLabel: TLabel
      Left = 688
      Top = 14
      Width = 20
      Height = 13
      Caption = 'Max'
    end
    object YLED: TShape
      Left = 566
      Top = 31
      Width = 8
      Height = 25
    end
    object PCountLabel: TLabel
      Left = 528
      Top = 86
      Width = 28
      Height = 13
      Caption = 'Kurve'
    end
    object CalcBtn: TSpeedButton
      Left = 679
      Top = 104
      Width = 25
      Height = 24
      Hint = 'Berechnen|'
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        04000000000000010000120B0000120B00001000000000000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00337000000000
        73333337777777773F333308888888880333337F3F3F3FFF7F33330808089998
        0333337F737377737F333308888888880333337F3F3F3F3F7F33330808080808
        0333337F737373737F333308888888880333337F3F3F3F3F7F33330808080808
        0333337F737373737F333308888888880333337F3F3F3F3F7F33330808080808
        0333337F737373737F333308888888880333337F3FFFFFFF7F33330800000008
        0333337F7777777F7F333308000E0E080333337F7FFFFF7F7F33330800000008
        0333337F777777737F333308888888880333337F333333337F33330888888888
        03333373FFFFFFFF733333700000000073333337777777773333}
      NumGlyphs = 2
      ParentShowHint = False
      ShowHint = True
      OnClick = CalcItemClick
    end
    object BuissyBtn: TSpeedButton
      Left = 729
      Top = 104
      Width = 25
      Height = 24
      Hint = 'Zur'#252'cksetzen|'
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        04000000000000010000130B0000130B00001000000000000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
        3333333333FFFFF3333333333999993333333333F77777FFF333333999999999
        3333333777333777FF33339993707399933333773337F3777FF3399933000339
        9933377333777F3377F3399333707333993337733337333337FF993333333333
        399377F33333F333377F993333303333399377F33337FF333373993333707333
        333377F333777F333333993333101333333377F333777F3FFFFF993333000399
        999377FF33777F77777F3993330003399993373FF3777F37777F399933000333
        99933773FF777F3F777F339993707399999333773F373F77777F333999999999
        3393333777333777337333333999993333333333377777333333}
      NumGlyphs = 2
      ParentShowHint = False
      ShowHint = True
      OnClick = BuissyItemClick
    end
    object AuswahlBtn: TSpeedButton
      Left = 754
      Top = 104
      Width = 25
      Height = 24
      Hint = 'Y- Auswahl|'
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        04000000000000010000120B0000120B00001000000000000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00555555555555
        55555555FFFFFFFF5555555000000005555555577777777FF555550999999900
        55555575555555775F55509999999901055557F55555557F75F5001111111101
        105577FFFFFFFF7FF75F00000000000011057777777777775F755070FFFFFF0F
        01105777F555557F7FF75500FFFFFF0F00105577F555FF7F77575550FF70000F
        0F0055575FF777757F775555000FFFFF0F005555777555FF7F77555550FF7000
        0F055555575FF777757F555555000FFFFF05555555777555FF7F55555550FF70
        0005555555575FF7777555555555000555555555555577755555555555555555
        5555555555555555555555555555555555555555555555555555}
      NumGlyphs = 2
      ParentShowHint = False
      ShowHint = True
      OnClick = YAuswahlClick
    end
    object KurvenZahlLabel: TLabel
      Left = 582
      Top = 86
      Width = 50
      Height = 13
      Caption = 'Parameter'
    end
    object XComboLabel: TLabel
      Left = 24
      Top = 14
      Width = 45
      Height = 13
      Caption = 'x - Achse'
    end
    object XLED: TShape
      Left = 174
      Top = 31
      Width = 8
      Height = 25
    end
    object XMinLabel: TLabel
      Left = 200
      Top = 14
      Width = 16
      Height = 13
      Caption = 'Min'
    end
    object XMaxLabel: TLabel
      Left = 296
      Top = 14
      Width = 20
      Height = 13
      Caption = 'Max'
    end
    object PMaxLabel: TLabel
      Left = 296
      Top = 86
      Width = 20
      Height = 13
      Caption = 'Max'
    end
    object PMinLabel: TLabel
      Left = 200
      Top = 86
      Width = 16
      Height = 13
      Caption = 'Min'
    end
    object PLED: TShape
      Left = 174
      Top = 103
      Width = 8
      Height = 25
    end
    object PComboLabel: TLabel
      Left = 24
      Top = 86
      Width = 50
      Height = 13
      Caption = 'Parameter'
    end
    object MemoBtn: TSpeedButton
      Left = 704
      Top = 104
      Width = 25
      Height = 24
      Hint = 'Erzeugungsdaten anzeigen|'
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        04000000000000010000120B0000120B00001000000000000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00555500000000
        0555555F7777777775F55500FFFFFFFFF0555577F5FFFFFFF7F550F0FEEEEEEE
        F05557F7F777777757F550F0FFFFFFFFF05557F7F5FFFFFFF7F550F0FEEEEEEE
        F05557F7F777777757F550F0FF777FFFF05557F7F5FFFFFFF7F550F0FEEEEEEE
        F05557F7F777777757F550F0FF7F777FF05557F7F5FFFFFFF7F550F0FEEEEEEE
        F05557F7F777777757F550F0FF77F7FFF05557F7F5FFFFFFF7F550F0FEEEEEEE
        F05557F7F777777757F550F0FFFFFFFFF05557F7FF5F5F5F57F550F00F0F0F0F
        005557F77F7F7F7F77555055070707070555575F7F7F7F7F7F55550507070707
        0555557575757575755555505050505055555557575757575555}
      NumGlyphs = 2
      ParentShowHint = False
      ShowHint = True
      OnClick = MemoItemClick
    end
    object ShowTogetherBtn: TSpeedButton
      Left = 634
      Top = 104
      Width = 25
      Height = 24
      Hint = 'Kurven nach Parameter gruppiert anzeigen|'
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        04000000000000010000130B0000130B00001000000000000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
        3333333333FFFFF3333333333000003333333333F777773FF333333003333300
        33333337733333773F33330333333333033333733FFFFFFF73F3303300000003
        303337F37777777337F3303330CCC0333033373337777733373F0333330C0333
        33037F33337773FFF37F03333330300033037F3FFFF73777FF7F0300000307B7
        03037F77777F77777F7F030999030BBB03037F77777F77777F7F0309990307B7
        03037377777F7777737330099903300030333777777F377737F3300000033333
        3033377777733333373333033333333303333373FF33333F7333333003333300
        3333333773FFFF77333333333000003333333333377777333333}
      NumGlyphs = 2
      ParentShowHint = False
      ShowHint = True
      OnClick = ShowTogetherBtnClick
    end
    object BereichBtn: TSpeedButton
      Left = 410
      Top = 104
      Width = 25
      Height = 24
      Hint = 'gesamter Einstellbereich|'
      AllowAllUp = True
      GroupIndex = 1
      Glyph.Data = {
        F6000000424DF600000000000000760000002800000010000000100000000100
        04000000000080000000CE0E0000D80E00001000000000000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
        3333333333333333333333333333333333333333333333333333333303333330
        3333C33003333330033CC30903333330903CC09900000000990CC99999999999
        999CC09900000000990CC30903333330903CC33003333330033C333303333330
        3333333333333333333333333333333333333333333333333333}
      ParentShowHint = False
      ShowHint = True
      OnClick = BereichBtnClick
    end
    object APBtn: TSpeedButton
      Left = 435
      Top = 104
      Width = 25
      Height = 24
      Hint = 'Umgebung um Arbeitspunkt|'
      AllowAllUp = True
      GroupIndex = 1
      Down = True
      Glyph.Data = {
        F6000000424DF600000000000000760000002800000010000000100000000100
        04000000000080000000CE0E0000D80E00001000000000000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF0033333CCCCC33
        3333333333303333333333333309033333333333330903333333333330999033
        3333333330999033333333330999990333333333099999033333333099999990
        3333333000090000333333333309033333333333330903333333333333090333
        3333333333090333333333333309033333333333330003333333}
      ParentShowHint = False
      ShowHint = True
      OnClick = BereichBtnClick
    end
    object APLabel: TLabel
      Left = 464
      Top = 86
      Width = 35
      Height = 13
      Caption = 'Bereich'
    end
    object YCombo: TComboBox
      Left = 416
      Top = 32
      Width = 145
      Height = 21
      Hint = 'Kurven nach Y Wert gruppiert anzeigen|'
      Style = csDropDownList
      ParentShowHint = False
      ShowHint = True
      TabOrder = 6
      OnChange = YComboChange
      Items.Strings = (
        'Durchbiegung hd'
        'Elastizit'#228't Punkt C'
        'Mastfall F0C'
        'Mastfall F0F'
        'Vorstag-Spannung'
        'Wanten-Spannung')
    end
    object YMinEdit: TEdit
      Left = 592
      Top = 32
      Width = 73
      Height = 21
      TabStop = False
      ReadOnly = True
      TabOrder = 7
      Text = 'YMinEdit'
    end
    object YMaxEdit: TEdit
      Left = 688
      Top = 32
      Width = 73
      Height = 21
      TabStop = False
      ReadOnly = True
      TabOrder = 10
      Text = 'YMaxEdit'
    end
    object PEdit: TEdit
      Left = 528
      Top = 104
      Width = 25
      Height = 21
      Hint = 'Y-Min/Max f'#252'r diese Kurve anzeigen|'
      ParentShowHint = False
      ReadOnly = True
      ShowHint = True
      TabOrder = 8
      Text = '1'
      OnChange = PEditChange
    end
    object KurvenzahlEdit: TEdit
      Left = 583
      Top = 104
      Width = 33
      Height = 21
      Hint = 'Parameteranzahl festlegen/Parameter ausw'#228'hlen|'
      ParentShowHint = False
      ReadOnly = True
      ShowHint = True
      TabOrder = 9
      Text = '3'
    end
    object KurvenZahlSpinner: TUpDown
      Left = 616
      Top = 104
      Width = 15
      Height = 21
      Associate = KurvenzahlEdit
      Min = 1
      Max = 5
      ParentShowHint = False
      Position = 3
      ShowHint = False
      TabOrder = 12
      OnClick = KurvenZahlSpinnerClick
    end
    object PSpinner: TUpDown
      Left = 553
      Top = 104
      Width = 15
      Height = 21
      Associate = PEdit
      Min = 1
      Max = 5
      ParentShowHint = False
      Position = 1
      ShowHint = False
      TabOrder = 11
      OnChanging = PSpinnerChanging
    end
    object XCombo: TComboBox
      Left = 24
      Top = 32
      Width = 145
      Height = 21
      Style = csDropDownList
      TabOrder = 0
      OnChange = XComboChange
      Items.Strings = (
        'Controller'
        'Winkel'
        'Vorstag'
        'Wante'
        'Wante oben'
        'Saling H'#246'he'
        'Saling Abstand')
    end
    object XMinEdit: TMaskEdit
      Left = 200
      Top = 32
      Width = 73
      Height = 21
      TabOrder = 1
      Text = 'XMinEdit'
    end
    object XMaxEdit: TMaskEdit
      Left = 296
      Top = 32
      Width = 73
      Height = 21
      TabOrder = 2
      Text = 'XMaxEdit'
    end
    object PMaxEdit: TMaskEdit
      Left = 296
      Top = 104
      Width = 73
      Height = 21
      TabOrder = 5
      Text = 'PMaxEdit'
    end
    object PMinEdit: TMaskEdit
      Left = 200
      Top = 105
      Width = 73
      Height = 21
      TabOrder = 4
      Text = 'PMinEdit'
    end
    object PCombo: TComboBox
      Left = 23
      Top = 105
      Width = 145
      Height = 21
      Style = csDropDownList
      TabOrder = 3
      OnChange = PComboChange
      Items.Strings = (
        'Saling Abstand'
        'Saling H'#246'he')
    end
    object APEdit: TEdit
      Left = 464
      Top = 104
      Width = 33
      Height = 21
      TabOrder = 13
      Text = '30'
      OnChange = APEditChange
    end
    object APSpinner: TUpDown
      Left = 497
      Top = 104
      Width = 15
      Height = 21
      Associate = APEdit
      Min = 1
      Max = 30
      Position = 30
      TabOrder = 14
    end
  end
  object OpenDialog: TOpenDialog
    Left = 136
    Top = 200
  end
  object SaveDialog: TSaveDialog
    Left = 248
    Top = 200
  end
end
