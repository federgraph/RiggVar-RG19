object OptionForm: TOptionForm
  Left = 230
  Top = 113
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsDialog
  Caption = 'Einstellungen'
  ClientHeight = 318
  ClientWidth = 529
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = True
  Position = poScreenCenter
  Scaled = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl: TPageControl
    Left = 4
    Top = 3
    Width = 521
    Height = 265
    ActivePage = tsIniMemo
    TabOrder = 0
    object tsTrimm: TTabSheet
      Caption = 'Trimm'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object LabelMin: TLabel
        Left = 56
        Top = 70
        Width = 21
        Height = 16
        Caption = 'Min'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -13
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object LabelPos: TLabel
        Left = 120
        Top = 70
        Width = 24
        Height = 16
        Caption = 'Pos'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -13
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object LabelMax: TLabel
        Left = 184
        Top = 70
        Width = 25
        Height = 16
        Caption = 'Max'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -13
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object Label15: TLabel
        Left = 236
        Top = 94
        Width = 125
        Height = 16
        Caption = 'Abmessungen in mm'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -13
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object TrimmVarLabel: TLabel
        Left = 236
        Top = 152
        Width = 87
        Height = 16
        Caption = 'Trimmvariable'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -13
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object Bevel1: TBevel
        Left = 24
        Top = 42
        Width = 465
        Height = 153
        Shape = bsFrame
      end
      object Label16: TLabel
        Left = 42
        Top = 34
        Width = 52
        Height = 16
        Caption = 'L'#228'ngen'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -13
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsItalic]
        ParentFont = False
      end
      object MinEdit: TMaskEdit
        Left = 56
        Top = 88
        Width = 41
        Height = 21
        EditMask = '!9999;1; '
        MaxLength = 4
        TabOrder = 0
        Text = '    '
        OnExit = MinEditExit
        OnKeyDown = MinEditKeyDown
      end
      object PosEdit: TMaskEdit
        Left = 120
        Top = 88
        Width = 41
        Height = 21
        EditMask = '!9999;1; '
        MaxLength = 4
        TabOrder = 1
        Text = '    '
        OnExit = MinEditExit
        OnKeyDown = MinEditKeyDown
      end
      object MaxEdit: TMaskEdit
        Left = 184
        Top = 88
        Width = 41
        Height = 21
        EditMask = '!9999;1; '
        MaxLength = 4
        TabOrder = 2
        Text = '    '
        OnExit = MinEditExit
        OnKeyDown = MinEditKeyDown
      end
      object TrimmCombo: TComboBox
        Left = 56
        Top = 144
        Width = 169
        Height = 21
        Style = csDropDownList
        TabOrder = 3
        OnChange = TrimmComboChange
        Items.Strings = (
          'Controller'
          'Winkel'
          'Vorstag'
          'Wante'
          'Wante oben'
          'Saling H'#246'he'
          'Saling Abstand'
          'Saling L'#228'nge')
      end
    end
    object tsFachwerk: TTabSheet
      Caption = 'Fachwerk'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object StabLabel: TLabel
        Left = 24
        Top = 14
        Width = 92
        Height = 16
        Caption = 'Fachwerkst'#228'be'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -13
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object Label9: TLabel
        Left = 200
        Top = 14
        Width = 52
        Height = 16
        Caption = 'EA in KN'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -13
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object Bevel4: TBevel
        Left = 24
        Top = 87
        Width = 479
        Height = 123
        Shape = bsFrame
      end
      object Label6: TLabel
        Left = 360
        Top = 80
        Width = 57
        Height = 16
        Caption = 'Auswahl'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -13
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsItalic]
        ParentFont = False
      end
      object Label2: TLabel
        Left = 48
        Top = 102
        Width = 48
        Height = 16
        Caption = 'Material'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -13
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object Label3: TLabel
        Left = 46
        Top = 154
        Width = 66
        Height = 16
        Caption = 'Querschnitt'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -13
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object A: TLabel
        Left = 222
        Top = 154
        Width = 9
        Height = 16
        Caption = 'A'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -13
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object Label4: TLabel
        Left = 224
        Top = 102
        Width = 9
        Height = 16
        Caption = 'E'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -13
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object Label8: TLabel
        Left = 302
        Top = 128
        Width = 124
        Height = 16
        Caption = 'E-Modul in KN/mm^2'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -13
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object Label10: TLabel
        Left = 300
        Top = 180
        Width = 118
        Height = 16
        Caption = 'Querschnitt in mm^2'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -13
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object ElementCombo: TComboBox
        Left = 24
        Top = 32
        Width = 161
        Height = 21
        Style = csDropDownList
        TabOrder = 0
        OnChange = ElementComboChange
        Items.Strings = (
          'Rumpfst'#228'be'
          'Wanten'
          'Vorstag'
          'Saling'
          'Saling-Verbindung')
      end
      object EAEdit: TEdit
        Left = 200
        Top = 32
        Width = 105
        Height = 21
        TabStop = False
        Color = clBtnFace
        ReadOnly = True
        TabOrder = 1
        Text = 'EAEdit'
      end
      object TakeOverBtn: TButton
        Left = 320
        Top = 31
        Width = 182
        Height = 25
        Caption = 'Auswahl '#220'bernehmen'
        TabOrder = 2
        OnClick = TakeOverBtnClick
      end
      object MaterialCombo: TComboBox
        Left = 48
        Top = 120
        Width = 153
        Height = 21
        Style = csDropDownList
        TabOrder = 3
        OnChange = MaterialComboChange
      end
      object QuerschnittCombo: TComboBox
        Left = 46
        Top = 172
        Width = 153
        Height = 21
        Style = csDropDownList
        TabOrder = 4
        OnChange = QuerschnittComboChange
      end
      object AEdit: TEdit
        Left = 222
        Top = 172
        Width = 73
        Height = 21
        TabStop = False
        Color = clBtnFace
        ReadOnly = True
        TabOrder = 5
        Text = 'AEdit'
      end
      object EEdit: TEdit
        Left = 224
        Top = 120
        Width = 73
        Height = 21
        TabStop = False
        Color = clBtnFace
        ReadOnly = True
        TabOrder = 6
        Text = 'EEdit'
      end
    end
    object tsMast: TTabSheet
      Caption = 'Mast'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Bevel3: TBevel
        Left = 24
        Top = 32
        Width = 473
        Height = 169
        Shape = bsFrame
      end
      object Label11: TLabel
        Left = 56
        Top = 62
        Width = 30
        Height = 16
        Caption = 'Profil'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -13
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object Label14: TLabel
        Left = 56
        Top = 134
        Width = 87
        Height = 16
        Caption = 'Abmessungen'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -13
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object Label5: TLabel
        Left = 296
        Top = 86
        Width = 158
        Height = 16
        Caption = 'Biegesteifigkeit EI in Nm^2'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -13
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object Label17: TLabel
        Left = 296
        Top = 160
        Width = 167
        Height = 16
        Caption = 'Abstand vom Mastfu'#223' in mm'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -13
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object Label1: TLabel
        Left = 40
        Top = 24
        Width = 36
        Height = 16
        Caption = 'Mast'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -13
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsItalic]
        ParentFont = False
      end
      object MastTypCombo: TComboBox
        Left = 56
        Top = 80
        Width = 145
        Height = 21
        Style = csDropDownList
        TabOrder = 0
        OnChange = MastTypComboChange
      end
      object MastMassCombo: TComboBox
        Left = 56
        Top = 152
        Width = 145
        Height = 21
        Style = csDropDownList
        TabOrder = 1
        OnChange = MastMassComboChange
        Items.Strings = (
          'Controller'
          'Saling'
          'Wante'
          'Top')
      end
      object EIEdit: TEdit
        Left = 216
        Top = 80
        Width = 73
        Height = 21
        TabStop = False
        Color = clBtnFace
        ReadOnly = True
        TabOrder = 2
        Text = 'EIEdit'
      end
      object MastMassEdit: TMaskEdit
        Left = 216
        Top = 152
        Width = 73
        Height = 21
        EditMask = '!9999;1; '
        MaxLength = 4
        TabOrder = 3
        Text = '    '
        OnExit = MastMassEditExit
        OnKeyDown = MastMassEditKeyDown
      end
    end
    object tsRumpf: TTabSheet
      Caption = 'Rumpf'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object RumpfGroupBox: TGroupBox
        Left = 330
        Top = 65
        Width = 161
        Height = 145
        Caption = 'Feld Editieren:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -13
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsItalic]
        ParentFont = False
        TabOrder = 1
        object RumpfLabel: TLabel
          Left = 24
          Top = 32
          Width = 73
          Height = 16
          Caption = 'RumpfLabel'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -13
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object RumpfEdit: TEdit
          Left = 24
          Top = 51
          Width = 81
          Height = 24
          AutoSelect = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = 16
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
          ReadOnly = True
          TabOrder = 1
          Text = '10'
        end
        object RumpfBtn: TButton
          Left = 16
          Top = 96
          Width = 129
          Height = 25
          Caption = #220'bernehmen'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -13
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
          TabOrder = 0
          OnClick = RumpfBtnClick
        end
        object RumpfSpinEdit: TUpDown
          Left = 105
          Top = 51
          Width = 15
          Height = 24
          Associate = RumpfEdit
          Min = -32000
          Max = 32000
          Position = 10
          TabOrder = 2
          TabStop = True
          OnChanging = RumpfSpinEditChanging
        end
      end
      object RumpfGrid: TStringGrid
        Left = 40
        Top = 32
        Width = 263
        Height = 178
        ColCount = 4
        RowCount = 7
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -13
        Font.Name = 'Arial'
        Font.Pitch = fpFixed
        Font.Style = []
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goDrawFocusSelected]
        ParentFont = False
        ScrollBars = ssNone
        TabOrder = 0
        OnSelectCell = RumpfGridSelectCell
      end
    end
    object tsTabelle: TTabSheet
      Caption = 'Tabelle'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object MemoLabel: TLabel
        Left = 8
        Top = 9
        Width = 108
        Height = 13
        Caption = 'Tabelle  (Weg = Kraft)'
      end
      object Label7: TLabel
        Left = 161
        Top = 211
        Width = 12
        Height = 13
        Caption = 'X1'
      end
      object Label12: TLabel
        Left = 260
        Top = 211
        Width = 12
        Height = 13
        Caption = 'Y1'
      end
      object Label13: TLabel
        Left = 362
        Top = 211
        Width = 12
        Height = 13
        Caption = 'X2'
      end
      object Label18: TLabel
        Left = 442
        Top = 211
        Width = 12
        Height = 13
        Caption = 'Y2'
      end
      object EvalOptionBtn: TSpeedButton
        Left = 147
        Top = 132
        Width = 25
        Height = 25
        Hint = 'Weg oder Kraft als Argument verwenden|'
        AllowAllUp = True
        GroupIndex = 1
        Caption = 'K'
        NumGlyphs = 2
        ParentShowHint = False
        ShowHint = True
        OnClick = EvalOptionBtnClick
      end
      object TrimmMemo: TMemo
        Left = 11
        Top = 32
        Width = 126
        Height = 193
        Lines.Strings = (
          '[X/mm=Y/N]'
          '10=60'
          '20=90'
          '30=100'
          '40=160'
          '50=183'
          '60=200'
          '70=205'
          '80=208'
          '90=220'
          '100=225')
        TabOrder = 0
      end
      object pnTrimmTabChart: TPanel
        Left = 182
        Top = 32
        Width = 323
        Height = 162
        BevelInner = bvLowered
        BevelOuter = bvLowered
        TabOrder = 1
        object PaintBoxTabelle: TPaintBox
          Left = 2
          Top = 2
          Width = 319
          Height = 158
          Align = alClient
          OnMouseDown = PaintBoxTabelleMouseDown
          OnPaint = PaintBoxTabellePaint
        end
      end
      object UpDownKraft1: TUpDown
        Left = 323
        Top = 203
        Width = 15
        Height = 21
        Associate = Kraft1Edit
        Enabled = False
        Max = 5000
        Position = 100
        TabOrder = 2
        Thousands = False
      end
      object Kraft1Edit: TEdit
        Left = 283
        Top = 203
        Width = 40
        Height = 21
        Hint = 'Kraftwert f'#252'r Punkt 1|'
        AutoSelect = False
        Color = clBtnFace
        ParentShowHint = False
        ReadOnly = True
        ShowHint = True
        TabOrder = 3
        Text = '100'
        OnChange = Kraft1EditChange
      end
      object Weg2Edit: TEdit
        Left = 383
        Top = 203
        Width = 40
        Height = 21
        Hint = 'Endwert Weg'
        TabStop = False
        Color = clBtnFace
        ParentShowHint = False
        ReadOnly = True
        ShowHint = True
        TabOrder = 4
        Text = 'Weg2Edit'
      end
      object Kraft2Edit: TEdit
        Left = 465
        Top = 203
        Width = 40
        Height = 21
        Hint = 'Endwert Kraft'
        TabStop = False
        Color = clBtnFace
        ParentShowHint = False
        ReadOnly = True
        ShowHint = True
        TabOrder = 5
        Text = 'Kraft2Edit'
      end
      object Weg1Edit: TEdit
        Left = 182
        Top = 203
        Width = 40
        Height = 21
        Hint = 'Wegwert f'#252'r Punkt 1|'
        Color = clBtnFace
        ParentShowHint = False
        ReadOnly = True
        ShowHint = True
        TabOrder = 6
        Text = '0'
        OnChange = Kraft1EditChange
      end
      object UpDownWeg1: TUpDown
        Left = 222
        Top = 203
        Width = 15
        Height = 21
        Associate = Weg1Edit
        Enabled = False
        TabOrder = 7
      end
      object rbKonstante: TRadioButton
        Left = 184
        Top = 9
        Width = 84
        Height = 17
        Caption = 'Konstante'
        TabOrder = 8
        OnClick = rbKonstanteClick
      end
      object rbGerade: TRadioButton
        Tag = 1
        Left = 282
        Top = 9
        Width = 74
        Height = 17
        Caption = 'Gerade'
        Checked = True
        TabOrder = 9
        TabStop = True
        OnClick = rbKonstanteClick
      end
      object rbParabel: TRadioButton
        Tag = 2
        Left = 362
        Top = 9
        Width = 74
        Height = 17
        Caption = 'Parabel'
        TabOrder = 10
        OnClick = rbKonstanteClick
      end
      object rbBezier: TRadioButton
        Tag = 3
        Left = 444
        Top = 9
        Width = 69
        Height = 17
        Caption = 'Bezier'
        TabOrder = 11
        OnClick = rbKonstanteClick
      end
      object CalcBtn: TBitBtn
        Left = 147
        Top = 107
        Width = 25
        Height = 25
        Hint = 'Trimmtabelle Berechnen|'
        Glyph.Data = {
          76010000424D7601000000000000760000002800000020000000100000000100
          04000000000000010000120B0000120B00001000000000000000000000000000
          800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
          3333333333333333333333333333333333333333333333333333333333333333
          3333333333333FF3333333333333003333333333333F77F33333333333009033
          333333333F7737F333333333009990333333333F773337FFFFFF330099999000
          00003F773333377777770099999999999990773FF33333FFFFF7330099999000
          000033773FF33777777733330099903333333333773FF7F33333333333009033
          33333333337737F3333333333333003333333333333377333333333333333333
          3333333333333333333333333333333333333333333333333333333333333333
          3333333333333333333333333333333333333333333333333333}
        NumGlyphs = 2
        ParentShowHint = False
        ShowHint = True
        TabOrder = 12
        OnClick = CalcBtnClick
      end
      object ApplyBtn: TBitBtn
        Left = 147
        Top = 59
        Width = 25
        Height = 25
        Hint = 'Trimmtabelle Zeichnen|'
        Glyph.Data = {
          76010000424D7601000000000000760000002800000020000000100000000100
          04000000000000010000120B0000120B00001000000000000000000000000000
          800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
          3333333333333333333333333333333333333333333333333333333333333333
          3333333333333333333333333333333333333333333FF3333333333333003333
          3333333333773FF3333333333309003333333333337F773FF333333333099900
          33333FFFFF7F33773FF30000000999990033777777733333773F099999999999
          99007FFFFFFF33333F7700000009999900337777777F333F7733333333099900
          33333333337F3F77333333333309003333333333337F77333333333333003333
          3333333333773333333333333333333333333333333333333333333333333333
          3333333333333333333333333333333333333333333333333333}
        NumGlyphs = 2
        ParentShowHint = False
        ShowHint = True
        TabOrder = 13
        OnClick = ApplyBtnClick
      end
    end
    object tsIniMemo: TTabSheet
      Caption = 'Rigg.ini'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object InifileMemo: TMemo
        Left = 0
        Top = 0
        Width = 393
        Height = 237
        Align = alLeft
        ScrollBars = ssVertical
        TabOrder = 0
      end
      object Speichern: TButton
        Left = 408
        Top = 16
        Width = 81
        Height = 25
        Caption = 'Speichern'
        TabOrder = 1
        OnClick = StoreItemClick
      end
      object LoadIniBtn: TButton
        Left = 408
        Top = 48
        Width = 81
        Height = 25
        Caption = 'Laden'
        TabOrder = 2
        OnClick = LoadItemClick
      end
    end
  end
  object OKBtn: TBitBtn
    Left = 169
    Top = 283
    Width = 81
    Height = 27
    Caption = 'OK'
    Glyph.Data = {
      BE060000424DBE06000000000000360400002800000024000000120000000100
      0800000000008802000000000000000000000001000000000000000000000000
      80000080000000808000800000008000800080800000C0C0C000C0DCC000F0CA
      A600000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000F0FBFF00A4A0A000808080000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00030303030303
      0303030303030303030303030303030303030303030303030303030303030303
      03030303030303030303030303030303030303030303FF030303030303030303
      03030303030303040403030303030303030303030303030303F8F8FF03030303
      03030303030303030303040202040303030303030303030303030303F80303F8
      FF030303030303030303030303040202020204030303030303030303030303F8
      03030303F8FF0303030303030303030304020202020202040303030303030303
      0303F8030303030303F8FF030303030303030304020202FA0202020204030303
      0303030303F8FF0303F8FF030303F8FF03030303030303020202FA03FA020202
      040303030303030303F8FF03F803F8FF0303F8FF03030303030303FA02FA0303
      03FA0202020403030303030303F8FFF8030303F8FF0303F8FF03030303030303
      FA0303030303FA0202020403030303030303F80303030303F8FF0303F8FF0303
      0303030303030303030303FA0202020403030303030303030303030303F8FF03
      03F8FF03030303030303030303030303FA020202040303030303030303030303
      0303F8FF0303F8FF03030303030303030303030303FA02020204030303030303
      03030303030303F8FF0303F8FF03030303030303030303030303FA0202020403
      030303030303030303030303F8FF0303F8FF03030303030303030303030303FA
      0202040303030303030303030303030303F8FF03F8FF03030303030303030303
      03030303FA0202030303030303030303030303030303F8FFF803030303030303
      030303030303030303FA0303030303030303030303030303030303F803030303
      0303030303030303030303030303030303030303030303030303030303030303
      0303}
    ModalResult = 1
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 1
    OnClick = OKBtnClick
  end
  object CancelBtn: TBitBtn
    Left = 268
    Top = 283
    Width = 101
    Height = 27
    Cancel = True
    Caption = 'Abbrechen'
    Default = True
    Glyph.Data = {
      BE060000424DBE06000000000000360400002800000024000000120000000100
      0800000000008802000000000000000000000001000000000000000000000000
      80000080000000808000800000008000800080800000C0C0C000C0DCC000F0CA
      A600000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000F0FBFF00A4A0A000808080000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00030303030303
      0303030303030303030303030303030303030303030303030303030303030303
      0303F8F80303030303030303030303030303030303FF03030303030303030303
      0303030303F90101F80303030303F9F80303030303030303F8F8FF0303030303
      03FF03030303030303F9010101F8030303F90101F8030303030303F8FF03F8FF
      030303FFF8F8FF030303030303F901010101F803F901010101F80303030303F8
      FF0303F8FF03FFF80303F8FF030303030303F901010101F80101010101F80303
      030303F8FF030303F8FFF803030303F8FF030303030303F90101010101010101
      F803030303030303F8FF030303F803030303FFF80303030303030303F9010101
      010101F8030303030303030303F8FF030303030303FFF8030303030303030303
      030101010101F80303030303030303030303F8FF0303030303F8030303030303
      0303030303F901010101F8030303030303030303030303F8FF030303F8030303
      0303030303030303F90101010101F8030303030303030303030303F803030303
      F8FF030303030303030303F9010101F8010101F803030303030303030303F803
      03030303F8FF0303030303030303F9010101F803F9010101F803030303030303
      03F8030303F8FF0303F8FF03030303030303F90101F8030303F9010101F80303
      03030303F8FF0303F803F8FF0303F8FF03030303030303F9010303030303F901
      0101030303030303F8FFFFF8030303F8FF0303F8FF0303030303030303030303
      030303F901F903030303030303F8F80303030303F8FFFFFFF803030303030303
      03030303030303030303030303030303030303030303030303F8F8F803030303
      0303030303030303030303030303030303030303030303030303030303030303
      0303}
    ModalResult = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 2
    OnClick = CancelBtnClick
  end
  object HelpBtn: TBitBtn
    Left = 391
    Top = 283
    Width = 81
    Height = 27
    Caption = 'Hilfe'
    Kind = bkHelp
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 3
  end
end
