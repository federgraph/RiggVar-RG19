object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'RG19'
  ClientHeight = 675
  ClientWidth = 774
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object StatusBar: TStatusBar
    Left = 0
    Top = 653
    Width = 774
    Height = 22
    Panels = <
      item
        Text = 'MenuText'
        Width = 353
      end
      item
        Text = 'RiggText'
        Width = 50
      end>
  end
  object SpeedPanel: TPanel
    Left = 0
    Top = 0
    Width = 774
    Height = 32
    Align = alTop
    BevelOuter = bvNone
    ParentShowHint = False
    ShowHint = True
    TabOrder = 1
    object OpenBtn: TSpeedButton
      Left = 8
      Top = 3
      Width = 25
      Height = 25
      Hint = #214'ffnen|'
      Glyph.Data = {
        06020000424D0602000000000000760000002800000028000000140000000100
        0400000000009001000000000000000000001000000000000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
        3333333333333333333333333333333333333333333333333333333333333333
        3333333333333333333333333333333333333333333333333333333333333333
        333FFFFFFFFFFFFFF3333380000000000000333333888888888888883F333300
        7B7B7B7B7B7B033333883F33333333338F33330F07B7B7B7B7B70333338F8F33
        3333333383F3330B0B7B7B7B7B7B7033338F83F33333333338F3330FB0B7B7B7
        B7B7B033338F38F333333333383F330BF07B7B7B7B7B7B03338F383FFFFF3333
        338F330FBF000007B7B7B703338F33888883FFFFFF83330BFBFBFBF000000033
        338F3333333888888833330FBFBFBFBFBFB03333338F333333333338F333330B
        FBFBFBFBFBF03333338F33333FFFFFF83333330FBFBF0000000333333387FFFF
        8888888333333330000033333333333333388888333333333333333333333333
        3333333333333333333333333333333333333333333333333333333333333333
        3333333333333333333333333333333333333333333333333333333333333333
        33333333333333333333}
      NumGlyphs = 2
    end
    object SaveBtn: TSpeedButton
      Left = 33
      Top = 3
      Width = 25
      Height = 25
      Hint = 'Speichern|'
      Glyph.Data = {
        06020000424D0602000000000000760000002800000028000000140000000100
        0400000000009001000000000000000000001000000000000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
        3333333333333333333333333333333333333333333333333333333333333333
        3333333333333333333333333333FFFFFFFFFFFFFF3333380000000000008333
        333888F8FF888F888F333330CC08CCF770CC0333333888F8FF888F888F333330
        CC08CCF770CC0333333888F888888F888F333330CC07887770CC03333338888F
        FFFFF8888F333330CC60000006CC033333388888888888888F333330CCCCCCCC
        CCCC033333388888888888888F333330C6000000006C03333338888888888888
        8F333330C0FFFFFFFF0C0333333888FFFFFFFF888F333330C0FFFFFFFF0C0333
        333888FFFFFFFF888F333330C0FFFFFFFF0C0333333888FFFFFFFF888F333330
        C0FFFFFFFF0C0333333888FFFFFFFF888F33333000FFFFFFFF000333333888FF
        FFFFFF888F333330C0FFFFFFFF0C0333333888FFFFFFFF888F33333800000000
        0000833333388888888888888333333333333333333333333333333333333333
        3333333333333333333333333333333333333333333333333333333333333333
        33333333333333333333}
      NumGlyphs = 2
    end
    object ExitBtn: TSpeedButton
      Left = 66
      Top = 3
      Width = 25
      Height = 25
      Hint = 'Beenden|'
      Glyph.Data = {
        06020000424D0602000000000000760000002800000028000000140000000100
        0400000000009001000000000000000000001000000000000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00377777777777
        777777773FFFFFFFFFFFF333333F888888888888F7F7F7888888888888883333
        33888888888888877F7F788888888888888F333FF88844444400888FFF444444
        88888888888333888883333334D5007FFF433333333338F888F3338F33333333
        345D50FFFF4333333333388788F3338F3333333334D5D0FFFF433333333338F8
        78F3338F33333333345D50FEFE4333333333388788F3338F3333333334D5D0FF
        FF433333333338F878F3338F33333333345D50FEFE4333333333388788F3338F
        3333333334D5D0FFFF433333333338F878F3338F33333333345D50FEFE433333
        3333388788F3338F3333333334D5D0EFEF433333333338F878F3338F33333333
        345D50FEFE4333333333388788F3338F3333333334D5D0EFEF433333333338F8
        F8FFFF8F33333333344444444443333333333888888888833333333333333333
        3333333333333333FFFFFF333333333333300000033333333333333888888F33
        333333333330AAAA0333333333333338FFFF8F33333333333330000003333333
        33333338888883333333}
      NumGlyphs = 2
    end
    object UpdateBtn: TSpeedButton
      Left = 113
      Top = 3
      Width = 25
      Height = 25
      Hint = 'Rigg neu Berechnen|'
      Caption = '='
      NumGlyphs = 2
    end
    object BtnGrau: TSpeedButton
      Left = 294
      Top = 3
      Width = 25
      Height = 25
      Hint = '2D Grafik - Entspanntes Rigg einblenden|'
      AllowAllUp = True
      GroupIndex = 2
      Down = True
      Glyph.Data = {
        F6000000424DF600000000000000760000002800000010000000100000000100
        0400000000008000000000000000000000001000000010000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00777777777777
        77777777977788CCC7777777977788CCC7777877797888CCC7777877797888CC
        C7777877779888CCC7797877779888CCC7797887778987CCC7977887788987CC
        C7977887888897CCC9777888888897CCC9777788888779CC977777888887797C
        977777888877779CC7777788887777CCC7777777777777777777}
    end
    object KoppelBtn: TSpeedButton
      Left = 319
      Top = 3
      Width = 25
      Height = 25
      Hint = '2D Grafik - Koppelkurve anzeigen|'
      AllowAllUp = True
      GroupIndex = 3
      Down = True
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        04000000000000010000120B0000120B00001000000000000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
        3333333333333333333333333333333333333333333333333333333333333333
        3333333333333333333333BBBB3333333333331313133333333333333BBBB333
        33333331111313333333333333BBBBB333333333331133313333333333333BBB
        333333333333111133333333333333BBB333333333333131333333333333333B
        BB333333333333331133333333333333BB333333333333313133333333333333
        BB3331133333333313133B3333333333BB33313113333333131333BB3333333B
        B3333311311111111133333BBBBBBBBBB333333333333131133333333BBBBBB3
        3333333333111333333333333333333333333333333333333333}
      NumGlyphs = 2
    end
    object ReglerBtn: TSpeedButton
      Left = 138
      Top = 3
      Width = 25
      Height = 25
      Hint = 'Trimm Regeln|'
      Caption = 'R'
    end
    object MemoryBtn: TSpeedButton
      Left = 163
      Top = 3
      Width = 25
      Height = 25
      Hint = 'Memory (Trimm als Referenz speichern|)'
      Caption = 'M'
    end
    object MemoryRecallBtn: TSpeedButton
      Left = 188
      Top = 3
      Width = 30
      Height = 25
      Hint = 'Memory Recall|'
      Caption = 'MR'
    end
    object PaintBtn: TSpeedButton
      Left = 244
      Top = 3
      Width = 25
      Height = 25
      Hint = '2D Grafik - Alte Grafik stehenlassen|'
      AllowAllUp = True
      GroupIndex = 1
      Glyph.Data = {
        CA010000424DCA01000000000000760000002800000022000000110000000100
        04000000000054010000CE0E0000D80E00001000000000000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00777777777777
        77777777777777777777770000007C9C7C7C9C7C779977111717111717711700
        00007C9C7C7C9C9C79977711171711111711770000007C7C9C7C7C9C79777717
        111717111717770000007C7C9C7C7C9C99797717111717111117170000007C7C
        7C9C7C7C97797717171117171177170000007C9C7C9C7C7C9799771117111717
        1171170000007C9C7C7C9C7C77977711171711171771770000007C9C9C7C9C7C
        79977711111711171711770000007C7C9C7C7C9C797777171117171117177700
        00007C7C9C9C7C9C79777717111117111717770000007C7C7C9C7C7C97777717
        171117171177770000007C7C7C9C9C7C97777717171111171177770000007C7C
        7C7C9C7C77777717171711171777770000007C7C7C7C9C9C7777771717171111
        1777770000007C7C7C7C7C9C7777771717171711177777000000777777777777
        7777777777777777777777000000}
      NumGlyphs = 2
    end
    object LedShape: TShape
      Left = 520
      Top = 7
      Width = 9
      Height = 16
      Brush.Color = clGreen
    end
    object BtnBlau: TSpeedButton
      Left = 269
      Top = 3
      Width = 25
      Height = 25
      Hint = '2D Grafik - Nullstellung anzeigen|'
      AllowAllUp = True
      GroupIndex = 4
      Glyph.Data = {
        42010000424D4201000000000000760000002800000011000000110000000100
        040000000000CC00000000000000000000001000000010000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00777777777877
        7777700000007EE8EEEE88E7EE77700000007EEEEEEEEE7EEEEE7000000078EE
        E0000EEE00EE700000007EEE00EE00EE00E7700000007E8E00EE00EEEEEE7000
        00007EEE00EE0077E77E700000007EEE00EE00EEEEE77000000078EE00EE00EE
        7EEE700000007EEE00EE00EE7EE870000000788E00EE00EEEE8E700000007EEE
        00EE00E77EEE700000008EEEE0000EEEEE7E7000000078EE7EEEEE7E8EEE7000
        000078EE7EEE8E7EEE88700000007EE8EEEE8EEE7EEE70000000777777777777
        777770000000}
    end
    object SofortBtn: TSpeedButton
      Left = 479
      Top = 3
      Width = 25
      Height = 25
      Hint = 'Umschalter Rigg sofort berechnen (Automatik)|'
      AllowAllUp = True
      GroupIndex = 8
      Down = True
      Caption = 'A'
    end
    object DiffBtn: TSpeedButton
      Left = 429
      Top = 3
      Width = 25
      Height = 25
      Hint = 'Umschalter Differenzen/Absolutwerte|'
      AllowAllUp = True
      GroupIndex = 7
      Caption = 'D'
    end
    object WinkelBtn: TSpeedButton
      Left = 454
      Top = 3
      Width = 25
      Height = 25
      Hint = 'Umschalter Winkel/Vorstag|'
      AllowAllUp = True
      GroupIndex = 9
      Caption = 'W'
    end
    object ControllerBtn: TSpeedButton
      Left = 404
      Top = 3
      Width = 25
      Height = 25
      Hint = 'Umschalter f'#252'r Controller-Modus|'
      AllowAllUp = True
      GroupIndex = 6
      Down = True
      Caption = 'C'
    end
    object ZweischlagBtn: TSpeedButton
      Left = 344
      Top = 3
      Width = 25
      Height = 25
      Hint = '2D Grafik - Mast als Zweischlag einzeichnen|'
      AllowAllUp = True
      GroupIndex = 5
      Glyph.Data = {
        42010000424D4201000000000000760000002800000011000000110000000100
        040000000000CC00000000000000000000001000000010000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF0077CCC7777777
        7777700000007CCCCC0000077777700000007CCCCCCAAAA007777000000077CC
        CCCCAAAAA07770000000770CCCCCCAAAAA0770000000770ACCCCCCAAAA077000
        000070AAACCCCCCAAAA07000000070AAAACCCCCCAAA07000000070AAAAACCCCC
        CAA07000000070AAAAAACCCCCAA07000000070AAAAACCCCCAAA070000000770A
        AACCCCCAAA0770000000770AACCCCCCAAA07700000007770ACCCCCAAA0777000
        00007777CCCCCAA00777700000007777CCCCC00777777000000077777CCC7777
        777770000000}
    end
  end
  object Panel: TPanel
    Left = 8
    Top = 38
    Width = 625
    Height = 185
    TabOrder = 2
    object ReportLabel: TLabel
      Left = 337
      Top = 151
      Width = 58
      Height = 13
      Caption = 'ReportLabel'
    end
    object M10Btn: TSpeedButton
      Left = 176
      Top = 43
      Width = 41
      Height = 30
      Caption = '-10'
    end
    object M1Btn: TSpeedButton
      Left = 254
      Top = 43
      Width = 23
      Height = 22
      Caption = '-1'
    end
    object P1Btn: TSpeedButton
      Left = 283
      Top = 43
      Width = 23
      Height = 22
      Caption = '+1'
    end
    object P10Btn: TSpeedButton
      Left = 312
      Top = 43
      Width = 23
      Height = 22
      Caption = '+10'
    end
    object CopyAndPasteBtn: TSpeedButton
      Left = 416
      Top = 7
      Width = 23
      Height = 22
      Caption = 'M'
    end
    object CopyTrimmItemBtn: TSpeedButton
      Left = 343
      Top = 7
      Width = 23
      Height = 22
      Caption = 'cti'
    end
    object MT0Btn: TSpeedButton
      Left = 176
      Top = 7
      Width = 45
      Height = 30
      Caption = 'MT0'
    end
    object PasteTrimmItemBtn: TSpeedButton
      Left = 372
      Top = 7
      Width = 23
      Height = 22
      Caption = 'pti'
    end
    object ReadTrimmFileBtn: TSpeedButton
      Left = 254
      Top = 7
      Width = 23
      Height = 22
      Caption = 'rtf'
    end
    object SaveTrimmFileBtn: TSpeedButton
      Left = 283
      Top = 7
      Width = 23
      Height = 22
      Caption = 'stf'
    end
    object ParamCombo: TComboBox
      Left = 176
      Top = 121
      Width = 217
      Height = 21
      TabOrder = 0
      Text = 'ParamCombo'
    end
    object TrimmMemo: TMemo
      Left = 1
      Top = 1
      Width = 169
      Height = 183
      Align = alLeft
      Lines.Strings = (
        'TrimmMemo')
      TabOrder = 1
    end
    object TrimmCombo: TComboBox
      Left = 176
      Top = 94
      Width = 177
      Height = 21
      TabOrder = 2
      Text = 'TrimmCombo'
    end
    object cbSandboxed: TCheckBox
      Left = 511
      Top = 10
      Width = 97
      Height = 17
      Caption = 'Sandboxed'
      TabOrder = 3
    end
    object cbAllProps: TCheckBox
      Left = 511
      Top = 33
      Width = 97
      Height = 17
      Caption = 'All Props ( TI )'
      TabOrder = 4
    end
    object ViewpointCombo: TComboBox
      Left = 176
      Top = 148
      Width = 129
      Height = 21
      TabOrder = 5
      Text = 'ViewpointCombo'
    end
    object cbAllTags: TCheckBox
      Left = 511
      Top = 56
      Width = 97
      Height = 17
      Caption = 'All Tags ( XML )'
      TabOrder = 6
    end
  end
  object ListBox: TListBox
    Left = 8
    Top = 229
    Width = 161
    Height = 257
    ItemHeight = 13
    TabOrder = 3
  end
  object ReportMemo: TMemo
    Left = 175
    Top = 229
    Width = 430
    Height = 257
    Lines.Strings = (
      'ReportMemo')
    TabOrder = 4
  end
  object MainMenu: TMainMenu
    Left = 327
    Top = 606
    object FileMenu: TMenuItem
      Caption = '&Datei'
      Hint = '  Dateibefehle'
      object NewItem: TMenuItem
        Caption = '&Neu'
        Hint = '  Standardwerte laden'
      end
      object OpenItem: TMenuItem
        Caption = '&'#214'ffnen ...'
        Hint = '  Konfiguration aus Datei laden'
      end
      object SaveItem: TMenuItem
        Caption = '&Speichern'
        Hint = '  Konfiguration in Datei speichern'
      end
      object SaveAsItem: TMenuItem
        Caption = 'Speichern &unter ...'
        Hint = '  Konfiguration in neue Datei schreiben'
      end
      object N9: TMenuItem
        Caption = '-'
      end
      object ExitItem: TMenuItem
        Caption = '&Beenden'
        Hint = '  Anwendung verlassen'
        ShortCut = 32856
      end
    end
    object BearbeitenMenu: TMenuItem
      Caption = '&Bearbeiten'
      GroupIndex = 2
      Hint = '  Bearbeitungsbefehle'
      object RecalcItem: TMenuItem
        Caption = 'Neu &berechnen ( = )'
        Hint = '  Rigg neu berechnen'
      end
      object BiegeNeigeItem: TMenuItem
        Caption = 'Biegen und &Neigen ...'
        Hint = '  Mastbiegung und Mastfall einstellen'
      end
      object ReglerItem: TMenuItem
        Caption = 'Trimm &regeln ... ( R )'
        Hint = '  Trimm automatisch einstellen'
      end
      object MemoryItem: TMenuItem
        Caption = 'Trimm &speichern ( M )'
        Hint = '  Trimm in den Zwischenspeicher kopieren'
      end
      object MemoryRecallkItem: TMenuItem
        Caption = 'Trimm &zur'#252'cksetzen ( MR )'
        Hint = '  Trimm aus dem Zwischenspeicher zur'#252'ckholen'
      end
    end
    object AnsichtMenu: TMenuItem
      Caption = '&Ansicht'
      GroupIndex = 2
      Hint = '  Fenster anzeigen und verbergen'
      object InputFormItem: TMenuItem
        Caption = '&Eingabe ...'
        Hint = '  Eingabeseiten im eigenen Fenster anzeigen'
        ShortCut = 16453
      end
      object OutputFormItem: TMenuItem
        Caption = '&Ausgabe ...'
        Hint = '  Ausgabeseiten im eigenen Fenster anzeigen'
        ShortCut = 16449
      end
      object GrafikFormItem: TMenuItem
        Caption = '&Grafik ...'
        Hint = '  Grafik-Ausgabeseiten separat anzeigen'
        ShortCut = 16455
      end
      object OptionItem: TMenuItem
        Caption = '&Konfiguration ...'
        Hint = '  Konstanten und Parameter ver'#228'ndern'
        ShortCut = 16459
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object ConsoleItem: TMenuItem
        Caption = 'Konsole'
      end
      object RotaFormItem: TMenuItem
        Caption = '3D Grafik ...'
        Hint = '  Rigg r'#228'umlich darstellen'
      end
      object ChartFormItem: TMenuItem
        Caption = 'Diagramm ...'
        Hint = '  Diagramm aktivieren'
      end
      object ReportFormItem: TMenuItem
        Caption = 'Report ...'
        Hint = '  Report erstellen'
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object SpeedBarItem: TMenuItem
        Caption = 'Symbolleiste'
        Checked = True
        Hint = '  Symbolleiste einblenden'
      end
      object StatusBarItem: TMenuItem
        Caption = 'Statusleiste'
        Checked = True
        Hint = '  Statusleiste einblenden'
      end
    end
    object MemoMenu: TMenuItem
      Caption = '&Tabellen'
      GroupIndex = 3
      Hint = '  Tabelle f'#252'r Anzeige im Memo ausw'#228'hlen'
      object rLItem: TMenuItem
        Caption = 'rL'
        Hint = '  L'#228'ngen (Rigg verformt) anzeigen'
        RadioItem = True
        ShortCut = 16460
      end
      object rLeItem: TMenuItem
        Caption = 'rLe'
        Hint = '  L'#228'ngen (Rigg entspannt) anzeigen'
        RadioItem = True
      end
      object rFItem: TMenuItem
        Caption = 'rF'
        Checked = True
        Hint = '  Kr'#228'fte anzeigen'
        RadioItem = True
        ShortCut = 16454
      end
      object rPItem: TMenuItem
        Caption = 'rP'
        Hint = '  Koordinaten (Rigg verformt ) anzeigen'
        RadioItem = True
        ShortCut = 16464
      end
      object rPeItem: TMenuItem
        Caption = 'rPe'
        Hint = '  Koordinaten (Rigg entlastet) anzeigen'
        RadioItem = True
      end
      object DiffLItem: TMenuItem
        Caption = 'Diff_L'
        Hint = '  L'#228'ngendifferenzen (entlastet - belastet) anzeigen'
        RadioItem = True
      end
      object DiffPItem: TMenuItem
        Caption = 'Diff_P'
        Hint = '  Punktverschiebungen (entlastet - belastet) anzeigen'
        RadioItem = True
      end
      object LogItem: TMenuItem
        Caption = 'Log'
        Hint = '  Log anzeigen'
        RadioItem = True
      end
    end
    object GrafikMenu: TMenuItem
      Caption = '&Grafik'
      GroupIndex = 3
      Hint = '  2D Grafikoptionen '
      object VonDerSeiteItem: TMenuItem
        Caption = 'Seitenansicht'
        Checked = True
        Hint = '  Rigg von der Seite gesehen darstellen'
        RadioItem = True
      end
      object VonHintenItem: TMenuItem
        Caption = 'Blick von Achtern'
        Hint = '  Rigg von hinten gesehen darstellen'
        RadioItem = True
      end
      object VonObenItem: TMenuItem
        Caption = 'Draufsicht'
        Hint = '  Rigg von oben gesehen darstellen'
        RadioItem = True
      end
      object Von3DItem: TMenuItem
        Caption = 'Perspektive'
        Hint = '  Rigg schr'#228'g von oben gesehen darstellen'
        RadioItem = True
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object CalcOffsetItem: TMenuItem
        Caption = 'Grafik ausrichten'
        Hint = '  2D Grafik automatisch ausrichten'
      end
      object AdjustFormItem: TMenuItem
        Caption = 'Grafik einrichten...'
        Hint = '  2D Grafik verschieben und skalieren'
      end
      object PrintItem: TMenuItem
        Caption = 'Grafik exportieren...'
        Hint = '  2D Grafik ausgeben'
      end
      object N6: TMenuItem
        Caption = '-'
      end
      object PaintItem: TMenuItem
        Caption = 'Alte Grafik stehenlassen'
        GroupIndex = 1
        Hint = '  Alte Grafik l'#246'schen oder stehenlassen'
      end
      object ReferenzItem: TMenuItem
        Caption = 'Referenzstellung'
        GroupIndex = 1
        Hint = '  Nullstellung einblenden'
      end
      object EntlastetItem: TMenuItem
        Caption = 'Entspanntes Rigg'
        Checked = True
        GroupIndex = 1
        Hint = '  Entspanntes Rigg einblenden'
      end
      object KoppelkurveItem: TMenuItem
        Caption = 'Koppelkurve'
        Checked = True
        GroupIndex = 1
        Hint = '  Koppelkurve einblenden'
      end
      object ZweischlagItem: TMenuItem
        Caption = 'Mast als Zweischlag zeichnen'
        GroupIndex = 1
        Hint = '  Mast als Bogen oder Zweischlag zeichnen'
      end
    end
    object OptionenMenu: TMenuItem
      Caption = '&Modell'
      GroupIndex = 3
      Hint = '  Modell - und Berechnungsoptionen'
      object FestItem: TMenuItem
        Caption = 'feste Salinge'
        Checked = True
        Hint = '  Modell: Salinge starr befestigt'
        RadioItem = True
      end
      object DrehbarItem: TMenuItem
        Caption = 'drehbare Salinge'
        Hint = '  Modell: Salinge drehbar angelenkt'
        RadioItem = True
      end
      object OhneItem: TMenuItem
        Caption = 'ohne Salinge / Mast biegt aus'
        Hint = '  Modell: Biegeknicken des Mastes ohne Salinge'
        RadioItem = True
      end
      object OSDlgItem: TMenuItem
        Caption = 'ohne Saling / Mast starr'
        Hint = '  Modell: Mast steif ohne Salinge'
        RadioItem = True
      end
      object N11: TMenuItem
        Caption = '-'
        GroupIndex = 1
      end
      object ControllerItem: TMenuItem
        Caption = 'Controller ( C )'
        Checked = True
        GroupIndex = 1
        Hint = '  Mastcontroller ber'#252'cksichtigen'
        ShortCut = 16451
      end
      object DifferenzItem: TMenuItem
        Caption = 'Differenzen ( D )'
        GroupIndex = 1
        Hint = '  L'#228'nge als Differenz oder Absolutwert anzeigen'
        ShortCut = 16452
      end
      object WinkelItem: TMenuItem
        Caption = 'Winkel einstellbar ( W )'
        GroupIndex = 1
        Hint = ' Wanten-Winkel oder Vorstagl'#228'nge einstellen'
      end
      object SofortItem: TMenuItem
        Caption = 'Rigg automatisch berechnen ( A )'
        Checked = True
        GroupIndex = 1
        Hint = '  Rigg (Kr'#228'fte) automatisch berechnen'
      end
      object N8: TMenuItem
        Caption = '-'
        GroupIndex = 2
      end
      object QuerKraftItem: TMenuItem
        Caption = 'QuerKraftBiegung'
        GroupIndex = 2
        Hint = '  Kraftberechnung nur mit Querkraftbiegung - kein Knicken'
        RadioItem = True
      end
      object KnickenItem: TMenuItem
        Caption = 'Biegeknicken'
        Checked = True
        GroupIndex = 2
        Hint = '  Biegeknicken bei der Kraftberechnung ber'#252'cksichtigen'
        RadioItem = True
      end
      object KraftGemessenItem: TMenuItem
        Caption = 'gemessene Kraftwerte verwenden'
        GroupIndex = 2
        Hint = '  Kr'#228'fte aus der Trimmtabelle entnehmen'
        RadioItem = True
      end
      object N2: TMenuItem
        Caption = '-'
        GroupIndex = 3
      end
      object KorrigiertItem: TMenuItem
        Caption = 'BiegeKnicken korrigiert'
        Checked = True
        GroupIndex = 3
        Hint = '  Anteil der Salingkraft an der Mastbiegung beachten'
      end
      object AutoLoadItem: TMenuItem
        Caption = 'Datensatz automatisch laden'
        GroupIndex = 3
        Hint = '  Datens'#228'tze aus Datenbank einlesen, wenn selektiert'
      end
    end
    object WindowMenu: TMenuItem
      Caption = '&Fenster'
      GroupIndex = 9
      Hint = '  MDI Fenster verwalten'
      object WindowCascadeItem: TMenuItem
        Caption = '&'#220'berlappend'
        Hint = '  Fenster '#252'berlappend anordnen'
      end
      object WindowTileItem: TMenuItem
        Caption = '&Nebeneinander'
        Hint = '  Fenster nebeneinander anordnen'
      end
      object WindowArrangeItem: TMenuItem
        Caption = '&Symbole anordnen'
        Hint = '  Fenstersymbole anordnen'
      end
      object WindowMinimizeItem: TMenuItem
        Caption = '&Alle verkleinern'
        Hint = '  Alle Fenster zum Symbol verkleinern'
      end
    end
    object HelpMenu: TMenuItem
      Caption = '&Hilfe'
      GroupIndex = 10
      Hint = '  Hilfethemen'
      object HilfeItem: TMenuItem
        Caption = '&Hilfe ...'
        Hint = '  Hilfesystem starten'
      end
      object AboutItem: TMenuItem
        Caption = '&Info...'
        Hint = '  Infofenster anzeigen'
      end
      object LogoItem: TMenuItem
        Caption = 'Logo'
      end
    end
  end
  object OpenDialog: TOpenDialog
    DefaultExt = 'ini'
    Filter = 'Alle Dateien (*.*)|*.*|Rigg Einstellungen (*.rgg)|*.rgg'
    FilterIndex = 2
    Options = [ofOverwritePrompt, ofPathMustExist, ofFileMustExist]
    Left = 455
    Top = 606
  end
  object SaveDialog: TSaveDialog
    DefaultExt = 'rgg'
    Filter = 
      'Rigg Einstellungen (*.rgg)|*.rgg|Rigg IniFile (*.rgi)|*.rgi|Alle' +
      ' Dateien (*.*)|*.*'
    Options = [ofOverwritePrompt, ofPathMustExist]
    Left = 592
    Top = 606
  end
end
