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
  OldCreateOrder = False
  Position = poScreenCenter
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
    ActivePage = tsTrimm
    TabOrder = 0
    object tsTrimm: TTabSheet
      Caption = 'Trimm'
      object GroupBoxTrimm: TGroupBox
        Left = 16
        Top = 16
        Width = 473
        Height = 169
        Caption = 'L'#228'ngen'
        TabOrder = 0
        object LabelPos: TLabel
          Left = 112
          Top = 46
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
        object LengthEditLabel: TLabel
          Left = 228
          Top = 65
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
        object LabelMax: TLabel
          Left = 176
          Top = 46
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
        object LabelMin: TLabel
          Left = 48
          Top = 46
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
        object TrimmComboLabel: TLabel
          Left = 223
          Top = 112
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
        object MaxEdit: TEdit
          Left = 176
          Top = 64
          Width = 41
          Height = 21
          MaxLength = 4
          TabOrder = 0
          Text = '    '
          OnExit = MinEditExit
          OnKeyDown = MinEditKeyDown
        end
        object MinEdit: TEdit
          Left = 48
          Top = 68
          Width = 41
          Height = 21
          MaxLength = 4
          TabOrder = 1
          Text = '    '
          OnExit = MinEditExit
          OnKeyDown = MinEditKeyDown
        end
        object PosEdit: TEdit
          Left = 112
          Top = 64
          Width = 41
          Height = 21
          MaxLength = 4
          TabOrder = 2
          Text = '    '
          OnExit = MinEditExit
          OnKeyDown = MinEditKeyDown
        end
        object TrimmCombo: TComboBox
          Left = 48
          Top = 111
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
    end
    object tsFachwerk: TTabSheet
      Caption = 'Fachwerk'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object ElementLabel: TLabel
        Left = 24
        Top = 10
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
      object EAEditLabel: TLabel
        Left = 200
        Top = 10
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
      object GroupBoxMaterial: TGroupBox
        Left = 24
        Top = 74
        Width = 477
        Height = 151
        Caption = 'Material'
        TabOrder = 3
        object AEditLabel: TLabel
          Left = 287
          Top = 115
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
        object MaterialComboLabel: TLabel
          Left = 31
          Top = 30
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
        object QuerschnittComboLabel: TLabel
          Left = 31
          Top = 92
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
        object ELabel: TLabel
          Left = 198
          Top = 31
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
        object EEditLabel: TLabel
          Left = 287
          Top = 53
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
        object ALabel: TLabel
          Left = 198
          Top = 92
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
        object MaterialCombo: TComboBox
          Left = 31
          Top = 52
          Width = 153
          Height = 21
          Style = csDropDownList
          TabOrder = 0
          OnChange = MaterialComboChange
        end
        object QuerschnittCombo: TComboBox
          Left = 31
          Top = 114
          Width = 153
          Height = 21
          Style = csDropDownList
          TabOrder = 1
          OnChange = QuerschnittComboChange
        end
        object AEdit: TEdit
          Left = 198
          Top = 114
          Width = 73
          Height = 21
          TabStop = False
          Color = clBtnFace
          ReadOnly = True
          TabOrder = 2
          Text = 'AEdit'
        end
        object EEdit: TEdit
          Left = 198
          Top = 53
          Width = 73
          Height = 21
          TabStop = False
          Color = clBtnFace
          ReadOnly = True
          TabOrder = 3
          Text = 'EEdit'
        end
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
        Left = 319
        Top = 30
        Width = 182
        Height = 25
        Caption = 'Auswahl '#220'bernehmen'
        TabOrder = 2
        OnClick = TakeOverBtnClick
      end
    end
    object tsMast: TTabSheet
      Caption = 'Mast'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object GroupBoxMast: TGroupBox
        Left = 18
        Top = 23
        Width = 471
        Height = 194
        Caption = 'Mast'
        TabOrder = 0
        object MastTypeComboLabel: TLabel
          Left = 24
          Top = 38
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
        object MastMassComboLabel: TLabel
          Left = 24
          Top = 110
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
        object MassMassEditLabel: TLabel
          Left = 264
          Top = 133
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
        object EILabel: TLabel
          Left = 264
          Top = 57
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
        object MastMassCombo: TComboBox
          Left = 24
          Top = 132
          Width = 145
          Height = 21
          Style = csDropDownList
          TabOrder = 0
          OnChange = MastMassComboChange
          Items.Strings = (
            'Controller'
            'Saling'
            'Wante'
            'Top')
        end
        object MastMassEdit: TEdit
          Left = 185
          Top = 132
          Width = 73
          Height = 21
          MaxLength = 4
          TabOrder = 1
          OnExit = MastMassEditExit
          OnKeyDown = MastMassEditKeyDown
        end
        object MastTypeCombo: TComboBox
          Left = 24
          Top = 56
          Width = 145
          Height = 21
          Style = csDropDownList
          TabOrder = 2
          OnChange = MastTypeComboChange
        end
        object EIEdit: TEdit
          Left = 185
          Top = 56
          Width = 73
          Height = 21
          TabStop = False
          Color = clBtnFace
          ReadOnly = True
          TabOrder = 3
          Text = 'EIEdit'
        end
      end
    end
    object tsRumpf: TTabSheet
      Caption = 'Rumpf'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object GroupBoxRumpf: TGroupBox
        Left = 330
        Top = 65
        Width = 161
        Height = 145
        Caption = 'Feld Editieren'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
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
      object Grid: TStringGrid
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
        OnSelectCell = GridSelectCell
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
      object X1Label: TLabel
        Left = 164
        Top = 203
        Width = 12
        Height = 13
        Caption = 'X1'
      end
      object Y1Label: TLabel
        Left = 265
        Top = 203
        Width = 12
        Height = 13
        Caption = 'Y1'
      end
      object X2Label: TLabel
        Left = 365
        Top = 203
        Width = 12
        Height = 13
        Caption = 'X2'
      end
      object Y2Label: TLabel
        Left = 447
        Top = 203
        Width = 12
        Height = 13
        Caption = 'Y2'
      end
      object EvalOptionBtn: TSpeedButton
        Left = 143
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
      object WriteMemoBtn: TSpeedButton
        Left = 143
        Top = 101
        Width = 25
        Height = 25
        Caption = '<'
        OnClick = WriteMemoBtnClick
      end
      object ReadMemoBtn: TSpeedButton
        Left = 143
        Top = 55
        Width = 25
        Height = 25
        Caption = '>'
        OnClick = ReadMemoBtnClick
      end
      object Image: TImage
        Left = 184
        Top = 32
        Width = 105
        Height = 105
        OnMouseDown = PaintBoxTabelleMouseDown
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
      object UpDownKraft1: TUpDown
        Left = 323
        Top = 203
        Width = 15
        Height = 21
        Associate = Kraft1Edit
        Enabled = False
        Max = 5000
        Position = 100
        TabOrder = 1
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
        TabOrder = 2
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
        TabOrder = 3
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
        TabOrder = 4
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
        TabOrder = 5
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
        TabOrder = 6
      end
      object rbKonstante: TRadioButton
        Left = 184
        Top = 9
        Width = 84
        Height = 17
        Caption = 'Konstante'
        TabOrder = 7
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
        TabOrder = 8
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
        TabOrder = 9
        OnClick = rbKonstanteClick
      end
      object rbBezier: TRadioButton
        Tag = 3
        Left = 444
        Top = 9
        Width = 69
        Height = 17
        Caption = 'Bezier'
        TabOrder = 10
        OnClick = rbKonstanteClick
      end
    end
    object tsIniMemo: TTabSheet
      Caption = 'Rigg.ini'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object IniMemo: TMemo
        Left = 0
        Top = 0
        Width = 393
        Height = 237
        Align = alLeft
        ScrollBars = ssVertical
        TabOrder = 0
      end
      object SaveIniBtn: TButton
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
        Top = 47
        Width = 81
        Height = 25
        Caption = 'Laden'
        TabOrder = 2
        OnClick = LoadItemClick
      end
    end
  end
  object OKBtn: TButton
    Left = 169
    Top = 283
    Width = 81
    Height = 27
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 1
    OnClick = OKBtnClick
  end
  object CancelBtn: TButton
    Left = 268
    Top = 283
    Width = 101
    Height = 27
    Cancel = True
    Caption = 'Abbrechen'
    Default = True
    ModalResult = 2
    TabOrder = 2
    OnClick = CancelBtnClick
  end
end
