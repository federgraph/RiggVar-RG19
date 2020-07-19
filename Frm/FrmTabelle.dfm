object FormTabelle: TFormTabelle
  Left = 282
  Top = 186
  Caption = 'FormTabelle'
  ClientHeight = 255
  ClientWidth = 465
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Memo: TMemo
    Left = 8
    Top = 35
    Width = 265
    Height = 145
    TabStop = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    Lines.Strings = (
      'Memo')
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object ReportCombo: TComboBox
    Left = 16
    Top = 8
    Width = 145
    Height = 21
    Style = csDropDownList
    TabOrder = 1
    OnChange = ReportComboChange
  end
  object AllBtn: TButton
    Left = 176
    Top = 4
    Width = 75
    Height = 25
    Caption = 'All'
    TabOrder = 2
    OnClick = AllBtnClick
  end
end
