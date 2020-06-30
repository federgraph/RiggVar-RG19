object FormDiagramC: TFormDiagramC
  Left = 0
  Top = 0
  Caption = 'FormDiagramC'
  ClientHeight = 642
  ClientWidth = 880
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Memo: TMemo
    Left = 152
    Top = 88
    Width = 185
    Height = 233
    Lines.Strings = (
      'Memo')
    TabOrder = 0
  end
  object UpdateBtn: TButton
    Left = 8
    Top = 16
    Width = 75
    Height = 25
    Caption = 'Test'
    TabOrder = 1
    OnClick = UpdateBtnClick
  end
  object XBox: TListBox
    Left = 8
    Top = 88
    Width = 121
    Height = 113
    ItemHeight = 13
    TabOrder = 2
    OnClick = XBoxClick
  end
  object PBox: TListBox
    Left = 8
    Top = 216
    Width = 121
    Height = 105
    ItemHeight = 13
    TabOrder = 3
    OnClick = PBoxClick
  end
  object YBox: TListBox
    Left = 360
    Top = 88
    Width = 121
    Height = 97
    ItemHeight = 13
    TabOrder = 4
    OnClick = YBoxClick
  end
  object XBtn: TButton
    Left = 89
    Top = 16
    Width = 75
    Height = 25
    Caption = 'Select X'
    TabOrder = 5
    OnClick = XBtnClick
  end
  object CalcBtn: TButton
    Left = 170
    Top = 16
    Width = 75
    Height = 25
    Caption = 'Calc'
    TabOrder = 6
    OnClick = CalcBtnClick
  end
  object AToggle: TToggleSwitch
    Left = 26
    Top = 341
    Width = 74
    Height = 20
    TabOrder = 7
    OnClick = AToggleClick
  end
  object GToggle: TToggleSwitch
    Left = 376
    Top = 205
    Width = 74
    Height = 20
    TabOrder = 8
    OnClick = GToggleClick
  end
  object UpDown: TUpDown
    Left = 440
    Top = 328
    Width = 97
    Height = 41
    Min = 5
    Max = 30
    Orientation = udHorizontal
    TabOrder = 9
    OnClick = UpDownClick
  end
  object AutoToggle: TToggleSwitch
    Left = 376
    Top = 231
    Width = 74
    Height = 20
    TabOrder = 10
    OnClick = AutoToggleClick
  end
  object LayoutBtn: TButton
    Left = 251
    Top = 16
    Width = 75
    Height = 25
    Caption = 'Layout'
    TabOrder = 11
    OnClick = LayoutBtnClick
  end
end
