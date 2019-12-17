object RumpfFaktorDlg: TRumpfFaktorDlg
  Left = 319
  Top = 198
  BorderStyle = bsDialog
  Caption = 'Rumpf'
  ClientHeight = 139
  ClientWidth = 252
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox: TGroupBox
    Left = 8
    Top = 8
    Width = 145
    Height = 121
    Caption = 'Abmessungen in cm'
    TabOrder = 2
    object Label1: TLabel
      Left = 24
      Top = 32
      Width = 30
      Height = 13
      Caption = 'L'#228'nge'
    end
    object Label2: TLabel
      Left = 24
      Top = 64
      Width = 27
      Height = 13
      Caption = 'Breite'
    end
    object Label3: TLabel
      Left = 24
      Top = 92
      Width = 26
      Height = 13
      Caption = 'H'#246'he'
    end
    object EditL: TMaskEdit
      Left = 64
      Top = 24
      Width = 60
      Height = 21
      EditMask = '9999;0; '
      MaxLength = 4
      TabOrder = 0
      Text = '420'
    end
    object EditB: TMaskEdit
      Left = 64
      Top = 56
      Width = 60
      Height = 21
      EditMask = '9999;0; '
      MaxLength = 4
      TabOrder = 1
      Text = '163'
    end
    object EditH: TMaskEdit
      Left = 64
      Top = 84
      Width = 60
      Height = 21
      EditMask = '9999;0; '
      MaxLength = 4
      TabOrder = 2
      Text = '75'
    end
  end
  object OKBtn: TButton
    Left = 167
    Top = 12
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
    OnClick = OKBtnClick
  end
  object CancelBtn: TButton
    Left = 167
    Top = 44
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Abbrechen'
    ModalResult = 2
    TabOrder = 1
    OnClick = CancelBtnClick
  end
end
