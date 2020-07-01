object FormRG19A: TFormRG19A
  Left = 217
  Top = 165
  Caption = 'Rigg - Form'
  ClientHeight = 212
  ClientWidth = 653
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clBlack
  Font.Height = 16
  Font.Name = 'Arial'
  Font.Style = []
  FormStyle = fsMDIForm
  Icon.Data = {
    0000010001001010100000000000280100001600000028000000100000002000
    00000100040000000000C0000000000000000000000000000000000000000000
    0000000080000080000000808000800000008000800080800000C0C0C0008080
    80000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF000000
    0000000000000FFF00000000FFF00FFF0BBBBBB0FFF00FFF0BBBBBB0FFF00FFF
    0BB00BB0FFF00FFF0BB00BB0FFF00FFF0BB00BB0FFF00FFF0BB00BB0FFF00FFF
    0BB00BB0FFF00FFF0BBBBBB0FFF00FFF0BBBBBB0FFF00FFF00000000FFF00FFF
    00000000FFF00FFFFFFFFFFFFFF00FFFFFFFFFFFFFF00000000000000000FFFF
    00008FF100008811000088110000899100008991000089910000899100008991
    000088110000881100008FF100008FF100008001000080010000FFFF0000}
  Menu = MainMenu
  OldCreateOrder = True
  Position = poDefault
  Scaled = False
  Visible = True
  WindowMenu = WindowMenu
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 16
  object SpeedPanel: TPanel
    Left = 0
    Top = 0
    Width = 653
    Height = 32
    Align = alTop
    BevelOuter = bvNone
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
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
      OnClick = OpenItemClick
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
      OnClick = SaveItemClick
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
      OnClick = ExitItemClick
    end
    object UpdateBtn: TSpeedButton
      Left = 113
      Top = 3
      Width = 25
      Height = 25
      Hint = 'Rigg neu Berechnen|'
      Caption = '='
      NumGlyphs = 2
      OnClick = UpdateBtnClick
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
      OnClick = BtnGrauClick
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
      OnClick = KoppelBtnClick
    end
    object ReglerBtn: TSpeedButton
      Left = 138
      Top = 3
      Width = 25
      Height = 25
      Hint = 'Trimm Regeln|'
      Caption = 'R'
      OnClick = ReglerBtnClick
    end
    object MemoryBtn: TSpeedButton
      Left = 163
      Top = 3
      Width = 25
      Height = 25
      Hint = 'Memory (Trimm als Referenz speichern|)'
      Caption = 'M'
      OnClick = MemoryBtnClick
    end
    object MemoryRecallBtn: TSpeedButton
      Left = 188
      Top = 3
      Width = 30
      Height = 25
      Hint = 'Memory Recall|'
      Caption = 'MR'
      OnClick = MemoryRecallBtnClick
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
      OnClick = PaintBtnClick
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
      OnClick = BtnBlauClick
    end
    object SofortBtn: TSpeedButton
      Left = 479
      Top = 3
      Width = 25
      Height = 25
      Hint = 'Umschalter Rigg sofort berechnen (Automatik)|'
      AllowAllUp = True
      GroupIndex = 8
      Caption = 'A'
      OnClick = SofortItemClick
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
      OnClick = DifferenzItemClick
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
      OnClick = WinkelItemClick
    end
    object ControllerBtn: TSpeedButton
      Left = 404
      Top = 3
      Width = 25
      Height = 25
      Hint = 'Umschalter f'#252'r Controller-Modus|'
      AllowAllUp = True
      GroupIndex = 6
      Caption = 'C'
      OnClick = ControllerBtnClick
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
      OnClick = ZweischlagBtnClick
    end
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 190
    Width = 653
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
  object MainMenu: TMainMenu
    Left = 71
    Top = 70
    object FileMenu: TMenuItem
      Caption = '&Datei'
      Hint = '  Dateibefehle'
      object NewItem: TMenuItem
        Caption = '&Neu'
        Hint = '  Standardwerte laden'
        OnClick = NewItemClick
      end
      object OpenItem: TMenuItem
        Caption = '&'#214'ffnen ...'
        Hint = '  Konfiguration aus Datei laden'
        OnClick = OpenItemClick
      end
      object SaveItem: TMenuItem
        Caption = '&Speichern'
        Hint = '  Konfiguration in Datei speichern'
        OnClick = SaveItemClick
      end
      object SaveAsItem: TMenuItem
        Caption = 'Speichern &unter ...'
        Hint = '  Konfiguration in neue Datei schreiben'
        OnClick = SaveAsItemClick
      end
      object N9: TMenuItem
        Caption = '-'
      end
      object ExitItem: TMenuItem
        Caption = '&Beenden'
        Hint = '  Anwendung verlassen'
        ShortCut = 32856
        OnClick = ExitItemClick
      end
    end
    object BearbeitenMenu: TMenuItem
      Caption = '&Bearbeiten'
      GroupIndex = 2
      Hint = '  Bearbeitungsbefehle'
      object RecalcItem: TMenuItem
        Caption = 'Neu &berechnen ( = )'
        Hint = '  Rigg neu berechnen'
        OnClick = UpdateBtnClick
      end
      object BiegeNeigeItem: TMenuItem
        Caption = 'Biegen und &Neigen ...'
        Hint = '  Mastbiegung und Mastfall einstellen'
        OnClick = BiegeNeigeItemClick
      end
      object ReglerItem: TMenuItem
        Caption = 'Trimm &regeln ... ( R )'
        Hint = '  Trimm automatisch einstellen'
        OnClick = ReglerBtnClick
      end
      object MemoryItem: TMenuItem
        Caption = 'Trimm &speichern ( M )'
        Hint = '  Trimm in den Zwischenspeicher kopieren'
        OnClick = MemoryBtnClick
      end
      object MemoryRecallItem: TMenuItem
        Caption = 'Trimm &zur'#252'cksetzen ( MR )'
        Hint = '  Trimm aus dem Zwischenspeicher zur'#252'ckholen'
        OnClick = MemoryRecallBtnClick
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
        OnClick = InputFormItemClick
      end
      object OutputFormItem: TMenuItem
        Caption = '&Ausgabe ...'
        Hint = '  Ausgabeseiten im eigenen Fenster anzeigen'
        ShortCut = 16449
        OnClick = OutputFormItemClick
      end
      object GrafikFormItem: TMenuItem
        Caption = '&Grafik ...'
        Hint = '  Grafik-Ausgabeseiten separat anzeigen'
        ShortCut = 16455
        OnClick = GrafikFormItemClick
      end
      object OptionItem: TMenuItem
        Caption = '&Konfiguration ...'
        Hint = '  Konstanten und Parameter ver'#228'ndern'
        ShortCut = 16459
        OnClick = OptionItemClick
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object ConsoleItem: TMenuItem
        Caption = 'Konsole'
        OnClick = ConsoleItemClick
      end
      object RotaFormItem: TMenuItem
        Caption = '3D Grafik ...'
        Hint = '  Rigg r'#228'umlich darstellen'
        OnClick = RotaFormItemClick
      end
      object ChartFormItem: TMenuItem
        Caption = 'Diagramm ...'
        Hint = '  Diagramm aktivieren'
        OnClick = ChartFormItemClick
      end
      object ReportFormItem: TMenuItem
        Caption = 'Report ...'
        Hint = '  Report erstellen'
        OnClick = ReportFormItemClick
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object SpeedBarItem: TMenuItem
        Caption = 'Symbolleiste'
        Checked = True
        Hint = '  Symbolleiste einblenden'
        OnClick = SpeedBarItemClick
      end
      object StatusBarItem: TMenuItem
        Caption = 'Statusleiste'
        Checked = True
        Hint = '  Statusleiste einblenden'
        OnClick = StatusBarItemClick
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
        OnClick = rLItemClick
      end
      object rLeItem: TMenuItem
        Caption = 'rLe'
        Hint = '  L'#228'ngen (Rigg entspannt) anzeigen'
        RadioItem = True
        OnClick = rLItemClick
      end
      object rFItem: TMenuItem
        Caption = 'rF'
        Checked = True
        Hint = '  Kr'#228'fte anzeigen'
        RadioItem = True
        ShortCut = 16454
        OnClick = rLItemClick
      end
      object rPItem: TMenuItem
        Caption = 'rP'
        Hint = '  Koordinaten (Rigg verformt ) anzeigen'
        RadioItem = True
        ShortCut = 16464
        OnClick = rLItemClick
      end
      object rPeItem: TMenuItem
        Caption = 'rPe'
        Hint = '  Koordinaten (Rigg entlastet) anzeigen'
        RadioItem = True
        OnClick = rLItemClick
      end
      object DiffLItem: TMenuItem
        Caption = 'Diff_L'
        Hint = '  L'#228'ngendifferenzen (entlastet - belastet) anzeigen'
        RadioItem = True
        OnClick = rLItemClick
      end
      object DiffPItem: TMenuItem
        Caption = 'Diff_P'
        Hint = '  Punktverschiebungen (entlastet - belastet) anzeigen'
        RadioItem = True
        OnClick = rLItemClick
      end
      object LogItem: TMenuItem
        Caption = 'Log'
        Hint = '  Log anzeigen'
        RadioItem = True
        OnClick = rLItemClick
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
        OnClick = VonDerSeiteItemClick
      end
      object VonHintenItem: TMenuItem
        Caption = 'Blick von Achtern'
        Hint = '  Rigg von hinten gesehen darstellen'
        RadioItem = True
        OnClick = VonDerSeiteItemClick
      end
      object VonObenItem: TMenuItem
        Caption = 'Draufsicht'
        Hint = '  Rigg von oben gesehen darstellen'
        RadioItem = True
        OnClick = VonDerSeiteItemClick
      end
      object Von3DItem: TMenuItem
        Caption = 'Perspektive'
        Hint = '  Rigg schr'#228'g von oben gesehen darstellen'
        RadioItem = True
        OnClick = VonDerSeiteItemClick
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object CalcOffsetItem: TMenuItem
        Caption = 'Grafik ausrichten'
        Hint = '  2D Grafik automatisch ausrichten'
        OnClick = CalcOffsetItemClick
      end
      object AdjustFormItem: TMenuItem
        Caption = 'Grafik einrichten...'
        Hint = '  2D Grafik verschieben und skalieren'
        OnClick = AdjustFormItemClick
      end
      object PrintItem: TMenuItem
        Caption = 'Grafik exportieren...'
        Hint = '  2D Grafik ausgeben'
        OnClick = PrintItemClick
      end
      object N6: TMenuItem
        Caption = '-'
      end
      object PaintItem: TMenuItem
        Caption = 'Alte Grafik stehenlassen'
        GroupIndex = 1
        Hint = '  Alte Grafik l'#246'schen oder stehenlassen'
        OnClick = PaintBtnClick
      end
      object ReferenzItem: TMenuItem
        Caption = 'Referenzstellung'
        GroupIndex = 1
        Hint = '  Nullstellung einblenden'
        OnClick = BtnBlauClick
      end
      object EntlastetItem: TMenuItem
        Caption = 'Entspanntes Rigg'
        Checked = True
        GroupIndex = 1
        Hint = '  Entspanntes Rigg einblenden'
        OnClick = BtnGrauClick
      end
      object KoppelkurveItem: TMenuItem
        Caption = 'Koppelkurve'
        Checked = True
        GroupIndex = 1
        Hint = '  Koppelkurve einblenden'
        OnClick = KoppelBtnClick
      end
      object ZweischlagItem: TMenuItem
        Caption = 'Mast als Zweischlag zeichnen'
        GroupIndex = 1
        Hint = '  Mast als Bogen oder Zweischlag zeichnen'
        OnClick = ZweischlagBtnClick
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
        OnClick = SalingTypChanged
      end
      object DrehbarItem: TMenuItem
        Caption = 'drehbare Salinge'
        Hint = '  Modell: Salinge drehbar angelenkt'
        RadioItem = True
        OnClick = SalingTypChanged
      end
      object OhneItem: TMenuItem
        Caption = 'ohne Salinge / Mast biegt aus'
        Hint = '  Modell: Biegeknicken des Mastes ohne Salinge'
        RadioItem = True
        OnClick = SalingTypChanged
      end
      object OSDlgItem: TMenuItem
        Caption = 'ohne Saling / Mast starr'
        Hint = '  Modell: Mast steif ohne Salinge'
        RadioItem = True
        OnClick = SalingTypChanged
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
        OnClick = ControllerBtnClick
      end
      object DifferenzItem: TMenuItem
        Caption = 'Differenzen ( D )'
        GroupIndex = 1
        Hint = '  L'#228'nge als Differenz oder Absolutwert anzeigen'
        ShortCut = 16452
        OnClick = DifferenzItemClick
      end
      object WinkelItem: TMenuItem
        Caption = 'Winkel einstellbar ( W )'
        GroupIndex = 1
        Hint = ' Wanten-Winkel oder Vorstagl'#228'nge einstellen'
        OnClick = WinkelItemClick
      end
      object SofortItem: TMenuItem
        Caption = 'Rigg automatisch berechnen ( A )'
        GroupIndex = 1
        Hint = '  Rigg (Kr'#228'fte) automatisch berechnen'
        OnClick = SofortItemClick
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
        OnClick = KnickenItemClick
      end
      object KnickenItem: TMenuItem
        Caption = 'Biegeknicken'
        Checked = True
        GroupIndex = 2
        Hint = '  Biegeknicken bei der Kraftberechnung ber'#252'cksichtigen'
        RadioItem = True
        OnClick = KnickenItemClick
      end
      object KraftGemessenItem: TMenuItem
        Caption = 'gemessene Kraftwerte verwenden'
        GroupIndex = 2
        Hint = '  Kr'#228'fte aus der Trimmtabelle entnehmen'
        RadioItem = True
        OnClick = KnickenItemClick
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
        OnClick = KorrigiertItemClick
      end
      object AutoLoadItem: TMenuItem
        Caption = 'Datensatz automatisch laden'
        GroupIndex = 3
        Hint = '  Datens'#228'tze aus Datenbank einlesen, wenn selektiert'
        OnClick = AutoLoadItemClick
      end
    end
    object WindowMenu: TMenuItem
      Caption = '&Fenster'
      GroupIndex = 9
      Hint = '  MDI Fenster verwalten'
      object WindowCascadeItem: TMenuItem
        Caption = '&'#220'berlappend'
        Hint = '  Fenster '#252'berlappend anordnen'
        OnClick = WindowCascadeItemClick
      end
      object WindowTileItem: TMenuItem
        Caption = '&Nebeneinander'
        Hint = '  Fenster nebeneinander anordnen'
        OnClick = WindowTileItemClick
      end
      object WindowArrangeItem: TMenuItem
        Caption = '&Symbole anordnen'
        Hint = '  Fenstersymbole anordnen'
        OnClick = WindowArrangeItemClick
      end
      object WindowMinimizeItem: TMenuItem
        Caption = '&Alle verkleinern'
        Hint = '  Alle Fenster zum Symbol verkleinern'
        OnClick = WindowMinimizeItemClick
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
        Caption = '&About ...'
        Hint = '  Aboutfenster anzeigen'
        OnClick = AboutItemClick
      end
      object InfoItem: TMenuItem
        Caption = '&Info'
        Hint = '  Infofenster anzeigen'
        OnClick = InfoItemClick
      end
      object LogoItem: TMenuItem
        Caption = 'Logo'
        OnClick = LogoItemClick
      end
    end
  end
  object OpenDialog: TOpenDialog
    DefaultExt = 'ini'
    Filter = 'Alle Dateien (*.*)|*.*|Rigg Einstellungen (*.rgg)|*.rgg'
    FilterIndex = 2
    Options = [ofOverwritePrompt, ofPathMustExist, ofFileMustExist]
    Left = 199
    Top = 70
  end
  object SaveDialog: TSaveDialog
    DefaultExt = 'rgg'
    Filter = 
      'Rigg Einstellungen (*.rgg)|*.rgg|Rigg IniFile (*.rgi)|*.rgi|Alle' +
      ' Dateien (*.*)|*.*'
    Options = [ofOverwritePrompt, ofPathMustExist]
    Left = 336
    Top = 70
  end
end
