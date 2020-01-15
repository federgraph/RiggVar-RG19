object FormRG19: TFormRG19
  Left = 0
  Top = 0
  Caption = 'RG19'
  ClientHeight = 675
  ClientWidth = 1094
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
  object PaintBoxR: TPaintBox
    Left = 688
    Top = 287
    Width = 105
    Height = 105
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 653
    Width = 1094
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
    Width = 1094
    Height = 55
    Align = alTop
    BevelOuter = bvNone
    ParentShowHint = False
    ShowHint = True
    TabOrder = 1
    object UpdateBtn: TSpeedButton
      Left = 113
      Top = 3
      Width = 25
      Height = 25
      Hint = 'Rigg neu Berechnen|'
      Caption = '='
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
    object LedShape: TShape
      Left = 520
      Top = 7
      Width = 9
      Height = 16
      Brush.Color = clGreen
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
    object M10Btn: TSpeedButton
      Left = 762
      Top = 3
      Width = 41
      Height = 30
      Caption = '-10'
    end
    object M1Btn: TSpeedButton
      Left = 809
      Top = 3
      Width = 23
      Height = 22
      Caption = '-1'
    end
    object P1Btn: TSpeedButton
      Left = 838
      Top = 3
      Width = 23
      Height = 22
      Caption = '+1'
    end
    object P10Btn: TSpeedButton
      Left = 867
      Top = 3
      Width = 23
      Height = 22
      Caption = '+10'
    end
    object CopyAndPasteBtn: TSpeedButton
      Left = 712
      Top = 6
      Width = 23
      Height = 22
      Caption = 'M'
    end
    object CopyTrimmItemBtn: TSpeedButton
      Left = 654
      Top = 7
      Width = 23
      Height = 22
      Caption = 'cti'
    end
    object MT0Btn: TSpeedButton
      Left = 545
      Top = 7
      Width = 45
      Height = 30
      Caption = 'MT0'
    end
    object PasteTrimmItemBtn: TSpeedButton
      Left = 683
      Top = 7
      Width = 23
      Height = 22
      Caption = 'pti'
    end
    object ReadTrimmFileBtn: TSpeedButton
      Left = 596
      Top = 7
      Width = 23
      Height = 22
      Caption = 'rtf'
    end
    object SaveTrimmFileBtn: TSpeedButton
      Left = 625
      Top = 6
      Width = 23
      Height = 22
      Caption = 'stf'
    end
  end
  object Listbox: TListBox
    Left = 8
    Top = 397
    Width = 169
    Height = 180
    ItemHeight = 13
    Items.Strings = (
      'Listbox')
    TabOrder = 2
  end
  object ReportMemo: TMemo
    Left = 211
    Top = 289
    Width = 291
    Height = 140
    Lines.Strings = (
      'ReportMemo')
    TabOrder = 3
  end
  object TrimmMemo: TMemo
    Left = 8
    Top = 62
    Width = 169
    Height = 185
    Lines.Strings = (
      'TrimmMemo')
    TabOrder = 4
  end
  object TrimmCombo: TComboBox
    Left = 8
    Top = 262
    Width = 155
    Height = 21
    TabOrder = 5
    Text = 'TrimmCombo'
  end
  object ParamCombo: TComboBox
    Left = 8
    Top = 289
    Width = 155
    Height = 21
    TabOrder = 6
    Text = 'ParamCombo'
  end
  object FixpointCombo: TComboBox
    Left = 8
    Top = 316
    Width = 155
    Height = 21
    TabOrder = 7
    Text = 'FixpointCombo'
  end
  object MainMenu: TMainMenu
    Left = 207
    Top = 550
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
      object MemoryRecallItem: TMenuItem
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
    Left = 303
    Top = 550
  end
  object SaveDialog: TSaveDialog
    DefaultExt = 'rgg'
    Filter = 
      'Rigg Einstellungen (*.rgg)|*.rgg|Rigg IniFile (*.rgi)|*.rgi|Alle' +
      ' Dateien (*.*)|*.*'
    Options = [ofOverwritePrompt, ofPathMustExist]
    Left = 408
    Top = 550
  end
end
