object RiggDialog: TRiggDialog
  Left = 91
  Top = 233
  BorderStyle = bsDialog
  Caption = 'Modell - Einstellungen'
  ClientHeight = 179
  ClientWidth = 342
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  Scaled = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel: TBevel
    Left = 8
    Top = 8
    Width = 321
    Height = 121
    Shape = bsFrame
  end
  object OKBtn: TButton
    Left = 68
    Top = 144
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
    OnClick = OKBtnClick
  end
  object CancelBtn: TButton
    Left = 156
    Top = 144
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Abbrechen'
    ModalResult = 2
    TabOrder = 1
  end
  object rgSalingTyp: TRadioGroup
    Left = 24
    Top = 16
    Width = 289
    Height = 49
    Caption = 'SalingTyp'
    Columns = 4
    ItemIndex = 0
    Items.Strings = (
      'fest'
      'drehbar'
      'ohne'
      'ohne/BK')
    TabOrder = 3
    OnClick = rgSalingTypClick
  end
  object cbWinkel: TCheckBox
    Left = 24
    Top = 80
    Width = 281
    Height = 17
    Caption = 'Winkel statt Vorstag einstellen'
    TabOrder = 4
  end
  object cbController: TCheckBox
    Left = 24
    Top = 104
    Width = 273
    Height = 17
    Caption = 'Controller ber'#252'cksichtigen'
    TabOrder = 5
  end
  object HelpBtn: TButton
    Left = 245
    Top = 144
    Width = 75
    Height = 25
    Caption = '&Hilfe'
    TabOrder = 2
    OnClick = HelpBtnClick
  end
end
