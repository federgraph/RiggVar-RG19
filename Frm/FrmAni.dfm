object AnimationForm: TAnimationForm
  Left = 29
  Top = 108
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Winkel einstellen'
  ClientHeight = 154
  ClientWidth = 360
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  KeyPreview = True
  OldCreateOrder = True
  Scaled = False
  OnCreate = FormCreate
  OnHide = FormHide
  OnKeyDown = FormKeyDown
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pnAnimation: TPanel
    Left = 8
    Top = 8
    Width = 345
    Height = 137
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 0
    object AnimateBtn: TSpeedButton
      Left = 184
      Top = 73
      Width = 145
      Height = 29
      Hint = 'Animation Ein/Aus|'
      AllowAllUp = True
      GroupIndex = 1
      Caption = '&Film starten'
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        04000000000000010000120B0000120B00001000000000000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00300000000000
        0000377777777777777703030303030303037F7F7F7F7F7F7F7F000000000000
        00007777777777777777933393303933337073F37F37F73F3377393393303393
        379037FF7F37F37FF777379793303379793037777337F3777737339933303339
        93303377F3F7F3F77F3733993930393993303377F737F7377FF7399993303399
        999037777337F377777793993330333393307377FF37F3337FF7333993303333
        993033377F37F33377F7333993303333993033377337F3337737333333303333
        33303FFFFFF7FFFFFFF700000000000000007777777777777777030303030303
        03037F7F7F7F7F7F7F7F00000000000000007777777777777777}
      Layout = blGlyphRight
      NumGlyphs = 2
      ParentShowHint = False
      ShowHint = True
      Spacing = 20
      OnClick = AnimateBtnClick
    end
    object lbSelStart: TLabel
      Left = 16
      Top = 16
      Width = 24
      Height = 13
      Caption = 'Start'
    end
    object lbStepCount: TLabel
      Left = 264
      Top = 16
      Width = 32
      Height = 13
      Caption = 'Anzahl'
    end
    object lbSelEnd: TLabel
      Left = 104
      Top = 16
      Width = 24
      Height = 13
      Caption = 'Ende'
    end
    object lbInterval: TLabel
      Left = 184
      Top = 16
      Width = 38
      Height = 13
      Caption = 'Interval'
    end
    object tbWinkel: TTrackBar
      Left = 8
      Top = 68
      Width = 169
      Height = 38
      Hint = 'Winkel - Istwert und Bereich f'#252'r Animation|'
      ParentShowHint = False
      PageSize = 10
      Frequency = 5
      Position = 5
      ShowHint = True
      TabOrder = 0
      OnChange = tbWinkelChange
    end
    object UpDownSelStart: TUpDown
      Left = 66
      Top = 32
      Width = 15
      Height = 21
      Associate = SelStartEdit
      TabOrder = 5
      OnClick = UpDownSelStartClick
    end
    object UpDownSelEnd: TUpDown
      Left = 154
      Top = 32
      Width = 15
      Height = 21
      Associate = SelEndEdit
      TabOrder = 6
      OnClick = UpDownSelEndClick
    end
    object UpDownStepCount: TUpDown
      Left = 314
      Top = 32
      Width = 15
      Height = 21
      Associate = StepCountEdit
      Min = 1
      Max = 20
      Position = 1
      TabOrder = 8
    end
    object UpDownInterval: TUpDown
      Left = 234
      Top = 32
      Width = 15
      Height = 21
      Associate = IntervalEdit
      Min = 100
      Max = 1000
      Increment = 100
      Position = 100
      TabOrder = 7
    end
    object IntervalEdit: TEdit
      Left = 184
      Top = 32
      Width = 50
      Height = 21
      Hint = 'Zeitinterval [ms]|'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 3
      Text = '100'
      OnChange = IntervalEditChange
    end
    object StepCountEdit: TEdit
      Left = 264
      Top = 32
      Width = 50
      Height = 21
      Hint = 'Schrittanzahl|'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 4
      Text = '1'
    end
    object SelStartEdit: TEdit
      Left = 16
      Top = 32
      Width = 50
      Height = 21
      Hint = 'Startwinkel f'#252'r Animation|'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      Text = '0'
    end
    object SelEndEdit: TEdit
      Left = 104
      Top = 32
      Width = 50
      Height = 21
      Hint = 'Endwinkel f'#252'r Animation|'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
      Text = '0'
    end
    object cbSinus: TCheckBox
      Left = 16
      Top = 112
      Width = 177
      Height = 17
      Caption = 'Sinusf'#246'rmige Animation'
      Checked = True
      State = cbChecked
      TabOrder = 9
    end
  end
end
