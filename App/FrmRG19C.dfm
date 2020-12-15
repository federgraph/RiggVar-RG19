object FormRG19C: TFormRG19C
  Left = 0
  Top = 0
  Caption = 'RG19C'
  ClientHeight = 695
  ClientWidth = 1094
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  OnShow = FormShow
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
    Top = 673
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
    Left = 24
    Top = 15
    Width = 966
    Height = 56
    BevelOuter = bvNone
    ParentShowHint = False
    ShowHint = True
    TabOrder = 1
  end
  object ReportListBox: TListBox
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
    Top = 77
    Width = 169
    Height = 185
    Lines.Strings = (
      'TrimmMemo')
    TabOrder = 4
  end
  object TrimmCombo: TComboBox
    Left = 8
    Top = 278
    Width = 155
    Height = 21
    TabOrder = 5
    Text = 'TrimmCombo'
  end
  object ParamCombo: TComboBox
    Left = 8
    Top = 313
    Width = 155
    Height = 21
    TabOrder = 6
    Text = 'ParamCombo'
  end
  object FixpointCombo: TComboBox
    Left = 8
    Top = 348
    Width = 155
    Height = 21
    TabOrder = 7
    Text = 'FixpointCombo'
  end
  object OpenDialog: TOpenDialog
    DefaultExt = 'rgi'
    Filter = 'Rigg Ini File (*.rgi)|*.rgi|Alle Dateien (*.*)|*.*'
    Options = [ofOverwritePrompt, ofPathMustExist, ofFileMustExist]
    Left = 303
    Top = 550
  end
  object SaveDialog: TSaveDialog
    DefaultExt = 'rgi'
    Filter = 'Rigg Ini File (*.rgi)|*.rgi|Alle Dateien (*.*)|*.*'
    Options = [ofOverwritePrompt, ofPathMustExist]
    Left = 408
    Top = 550
  end
end
