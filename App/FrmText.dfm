object TextForm: TTextForm
  Left = 0
  Top = 0
  Caption = 'Text'
  ClientHeight = 494
  ClientWidth = 646
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsMDIChild
  OldCreateOrder = False
  Position = poDesigned
  Visible = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnMouseWheel = FormMouseWheel
  PixelsPerInch = 96
  TextHeight = 13
  object ReportMemo: TMemo
    Left = 175
    Top = 224
    Width = 430
    Height = 257
    Lines.Strings = (
      'ReportMemo')
    TabOrder = 0
  end
  object ListBox: TListBox
    Left = 8
    Top = 224
    Width = 161
    Height = 257
    ItemHeight = 13
    TabOrder = 1
    OnClick = ListBoxClick
  end
  object Panel: TPanel
    Left = 8
    Top = 8
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
      OnClick = M10BtnClick
    end
    object M1Btn: TSpeedButton
      Left = 238
      Top = 43
      Width = 23
      Height = 22
      Caption = '-1'
      OnClick = M1BtnClick
    end
    object P1Btn: TSpeedButton
      Left = 267
      Top = 43
      Width = 23
      Height = 22
      Caption = '+1'
      OnClick = P1BtnClick
    end
    object P10Btn: TSpeedButton
      Left = 296
      Top = 43
      Width = 23
      Height = 22
      Caption = '+10'
      OnClick = P10BtnClick
    end
    object CopyAndPasteBtn: TSpeedButton
      Left = 416
      Top = 7
      Width = 23
      Height = 22
      Caption = 'M'
      OnClick = CopyAndPasteBtnClick
    end
    object CopyTrimmItemBtn: TSpeedButton
      Left = 343
      Top = 7
      Width = 23
      Height = 22
      Caption = 'cti'
      OnClick = CopyTrimmItemBtnClick
    end
    object MT0Btn: TSpeedButton
      Left = 176
      Top = 7
      Width = 45
      Height = 30
      Caption = 'MT0'
      OnClick = MT0BtnClick
    end
    object PasteTrimmItemBtn: TSpeedButton
      Left = 372
      Top = 7
      Width = 23
      Height = 22
      Caption = 'pti'
      OnClick = PasteTrimmItemBtnClick
    end
    object ReadTrimmFileBtn: TSpeedButton
      Left = 238
      Top = 7
      Width = 23
      Height = 22
      Caption = 'rtf'
      OnClick = ReadTrimmFileBtnClick
    end
    object SaveTrimmFileBtn: TSpeedButton
      Left = 267
      Top = 7
      Width = 23
      Height = 22
      Caption = 'stf'
      OnClick = SaveTrimmFileBtnClick
    end
    object CopyTrimmFileBtn: TSpeedButton
      Left = 296
      Top = 7
      Width = 23
      Height = 22
      Caption = 'ctf'
      OnClick = CopyTrimmFileBtnClick
    end
    object ParamCombo: TComboBox
      Left = 176
      Top = 121
      Width = 217
      Height = 21
      TabOrder = 0
      Text = 'ParamCombo'
      OnChange = ParamComboChange
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
      OnChange = TrimmComboChange
    end
    object cbSandboxed: TCheckBox
      Left = 511
      Top = 10
      Width = 97
      Height = 17
      Caption = 'Sandboxed'
      TabOrder = 3
      OnClick = cbSandboxedClick
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
      OnChange = ViewpointComboChange
    end
    object cbAllTags: TCheckBox
      Left = 511
      Top = 56
      Width = 97
      Height = 17
      Caption = 'All Tags ( XML )'
      TabOrder = 6
      OnClick = cbAllTagsClick
    end
  end
end
