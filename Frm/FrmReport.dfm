object ReportForm: TReportForm
  Left = 108
  Top = 152
  Caption = 'Report'
  ClientHeight = 312
  ClientWidth = 632
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'System'
  Font.Style = []
  FormStyle = fsMDIChild
  Icon.Data = {
    0000010001002020100000000000E80200001600000028000000200000004000
    0000010004000000000080020000000000000000000000000000000000000000
    0000000080000080000000808000800000008000800080800000C0C0C0008080
    80000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF000000
    0000000000000000000000000000022222222222222222222222222222200222
    2222222222222222222222222220022222000000000000000000002222200222
    220AAAAAAAAAAA007777702222200222220AAAAAAAAAAA020777702222200222
    220AA777777777022077702222200222220AAAAAAAAAAA022207702222200222
    220AAAAAAAAAAA022220702222200222220AA777777777022222002222200222
    220AAAAAAAAAAA000000002222200222220AAAAAAAAAAAAAAAAAA02222200222
    220AA77777777777777AA02222200222220AAAAAAAAAAAAAAAAAA02222200222
    220AAAAAAAAAAAAAAAAAA02222200222220AA77777777777777AA02222200222
    220AAAAAAAAAAAAAAAAAA02222200222220AAAAAAAAAAAAAAAAAA02222200222
    220AA77777777777777AA02222200222220AAAAAAAAAAAAAAAAAA02222200222
    220AAAAAAAAAAAAAAAAAA02222200222220AA77777777777777AA02222200222
    220AAAAAAAAAAAAAAAAAA02222200222220AAAAAAAAAAAAAAAAAA02222200222
    220AACCCCCCCCCCCCCCAA02222200222220AA77777777777777AA02222200222
    220AAAAAAAAAAAAAAAAAA02222200222220AAAAAAAAAAAAAAAAAA02222200222
    2200000000000000000000222220022222222222222222222222222222200222
    2222222222222222222222222220000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    000000000000000000000000000000000000000000000000000000000000}
  Menu = MainMenu
  OldCreateOrder = True
  Position = poScreenCenter
  Visible = True
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 16
  object FramePanel: TPanel
    Left = 0
    Top = 0
    Width = 632
    Height = 312
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object ReportMemo: TMemo
      Left = 0
      Top = 0
      Width = 632
      Height = 312
      Align = alClient
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -15
      Font.Name = 'Courier New'
      Font.Style = []
      Lines.Strings = (
        '')
      ParentFont = False
      ScrollBars = ssBoth
      TabOrder = 0
      WordWrap = False
    end
  end
  object PrintDialog: TPrintDialog
    Left = 448
    Top = 24
  end
  object SaveDialog: TSaveDialog
    DefaultExt = 'txt'
    Filter = 'Text Datei (*.txt)|*.text|Alle Dateien (*.*)|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofCreatePrompt, ofNoReadOnlyReturn]
    Title = 'Report Speichern'
    Left = 416
    Top = 24
  end
  object MainMenu: TMainMenu
    Left = 384
    Top = 24
    object ReportMenu: TMenuItem
      Caption = '&Report'
      GroupIndex = 8
      Hint = '  Befehle f'#252'r den Report'
      object KopierenItem: TMenuItem
        Caption = '&Kopieren'
        Hint = '  Selektierten Text in die Zwischenablage kopieren'
        OnClick = CopyItemClick
      end
      object SpeichernItem: TMenuItem
        Caption = '&Speichern unter ...'
        Hint = '  Report in Textdatei speichern'
        OnClick = PrintToFileItemClick
      end
      object DruckenItem: TMenuItem
        Caption = '&Drucken ...'
        Hint = '  Report ausdrucken'
        OnClick = PrintItemClick
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object AktualisierenItem: TMenuItem
        Caption = '&Aktualisieren'
        Hint = '  Report aktualisieren'
        OnClick = UpdateItemClick
      end
      object ElementeItem: TMenuItem
        Caption = '&Elemente ausw'#228'hlen ...'
        Hint = '  den Inhalt des Reports festlegen'
        OnClick = AuswahlItemClick
      end
    end
  end
end
