object InputForm: TInputForm
  Left = 266
  Top = 163
  BorderStyle = bsDialog
  Caption = 'InputForm'
  ClientHeight = 195
  ClientWidth = 465
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = True
  Scaled = False
  OnCreate = FormCreate
  OnHide = FormHide
  PixelsPerInch = 96
  TextHeight = 13
  object InputPages: TPageControl
    Left = 0
    Top = 0
    Width = 465
    Height = 195
    ActivePage = tsFest
    TabOrder = 0
    OnChange = InputPagesChange
    object tsFest: TTabSheet
      Caption = 'Salinge fest'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object pnFest: TPanel
        Left = 0
        Top = 0
        Width = 393
        Height = 167
        Align = alLeft
        BevelOuter = bvNone
        BorderWidth = 2
        BorderStyle = bsSingle
        TabOrder = 0
        object lbController: TLabel
          Left = 168
          Top = 14
          Width = 57
          Height = 16
          Caption = 'Controller'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -13
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object lbWinkel: TLabel
          Left = 168
          Top = 36
          Width = 41
          Height = 16
          Caption = 'Winkel'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -13
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object lbWante: TLabel
          Left = 168
          Top = 60
          Width = 39
          Height = 16
          Caption = 'Wante'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -13
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object lbWoben: TLabel
          Left = 168
          Top = 84
          Width = 73
          Height = 16
          Caption = 'Wante oben'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -13
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object lbSalingH: TLabel
          Left = 168
          Top = 108
          Width = 74
          Height = 16
          Caption = 'Saling H'#246'he'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -13
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object lbSalingA: TLabel
          Left = 168
          Top = 132
          Width = 91
          Height = 16
          Caption = 'Saling Abstand'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -13
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object lbValue1: TLabel
          Left = 287
          Top = 14
          Width = 53
          Height = 16
          Alignment = taRightJustify
          Caption = 'lbValue1'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -13
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object lbValue2: TLabel
          Left = 287
          Top = 36
          Width = 53
          Height = 16
          Alignment = taRightJustify
          Caption = 'lbValue2'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -13
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object lbValue3: TLabel
          Left = 287
          Top = 60
          Width = 53
          Height = 16
          Alignment = taRightJustify
          Caption = 'lbValue3'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -13
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object lbValue4: TLabel
          Left = 287
          Top = 84
          Width = 53
          Height = 16
          Alignment = taRightJustify
          Caption = 'lbValue4'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -13
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object lbValue5: TLabel
          Left = 287
          Top = 108
          Width = 53
          Height = 16
          Alignment = taRightJustify
          Caption = 'lbValue5'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -13
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object lbValue6: TLabel
          Left = 287
          Top = 132
          Width = 53
          Height = 16
          Alignment = taRightJustify
          Caption = 'lbValue6'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -13
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object sbWinkel: TScrollBar
          Tag = 1
          Left = 36
          Top = 36
          Width = 121
          Height = 17
          LargeChange = 10
          PageSize = 0
          TabOrder = 1
          OnScroll = sbControllerScroll
        end
        object sbWante: TScrollBar
          Tag = 2
          Left = 36
          Top = 60
          Width = 121
          Height = 17
          LargeChange = 5
          PageSize = 0
          TabOrder = 2
          OnScroll = sbControllerScroll
        end
        object sbWoben: TScrollBar
          Tag = 3
          Left = 36
          Top = 84
          Width = 121
          Height = 17
          LargeChange = 5
          PageSize = 0
          TabOrder = 3
          OnScroll = sbControllerScroll
        end
        object sbSalingH: TScrollBar
          Tag = 4
          Left = 36
          Top = 108
          Width = 121
          Height = 17
          LargeChange = 5
          PageSize = 0
          TabOrder = 4
          OnScroll = sbControllerScroll
        end
        object sbSalingA: TScrollBar
          Tag = 5
          Left = 36
          Top = 132
          Width = 121
          Height = 17
          LargeChange = 5
          PageSize = 0
          TabOrder = 5
          OnScroll = sbControllerScroll
        end
        object rbController: TRadioButton
          Left = 8
          Top = 12
          Width = 20
          Height = 19
          TabOrder = 6
          OnClick = rbWinkelClick
          OnMouseDown = rbWinkelMouseDown
        end
        object rbWinkel: TRadioButton
          Left = 8
          Top = 36
          Width = 20
          Height = 17
          Checked = True
          TabOrder = 7
          TabStop = True
          OnClick = rbWinkelClick
          OnMouseDown = rbWinkelMouseDown
        end
        object sbController: TScrollBar
          Left = 36
          Top = 12
          Width = 121
          Height = 17
          LargeChange = 10
          PageSize = 0
          TabOrder = 0
          OnScroll = sbControllerScroll
        end
        object rbWante: TRadioButton
          Left = 8
          Top = 60
          Width = 20
          Height = 17
          TabOrder = 8
          OnClick = rbWinkelClick
          OnMouseDown = rbWinkelMouseDown
        end
        object rbWoben: TRadioButton
          Left = 8
          Top = 84
          Width = 20
          Height = 17
          TabOrder = 9
          OnClick = rbWinkelClick
          OnMouseDown = rbWinkelMouseDown
        end
        object rbSalingH: TRadioButton
          Left = 8
          Top = 108
          Width = 20
          Height = 17
          TabOrder = 10
          OnClick = rbWinkelClick
          OnMouseDown = rbWinkelMouseDown
        end
        object rbSalingA: TRadioButton
          Left = 8
          Top = 132
          Width = 20
          Height = 17
          TabOrder = 11
          OnClick = rbWinkelClick
          OnMouseDown = rbWinkelMouseDown
        end
      end
      object pnMast: TPanel
        Left = 393
        Top = 0
        Width = 64
        Height = 167
        Align = alClient
        BevelOuter = bvNone
        BorderStyle = bsSingle
        TabOrder = 1
        object PaintBoxM: TPaintBox
          Left = 0
          Top = 0
          Width = 60
          Height = 163
          Align = alClient
          OnPaint = PaintBoxMPaint
          ExplicitHeight = 160
        end
      end
    end
    object tsDrehbar: TTabSheet
      Tag = 1
      Caption = 'drehbar'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object pnDrehbar: TPanel
        Left = 0
        Top = 0
        Width = 393
        Height = 167
        Align = alLeft
        BevelOuter = bvNone
        BorderWidth = 2
        BorderStyle = bsSingle
        TabOrder = 0
        object lbControllerD: TLabel
          Left = 168
          Top = 14
          Width = 57
          Height = 16
          Caption = 'Controller'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -13
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object lbWinkelD: TLabel
          Left = 168
          Top = 36
          Width = 47
          Height = 16
          Caption = 'Vorstag'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -13
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object lbWanteD: TLabel
          Left = 168
          Top = 60
          Width = 39
          Height = 16
          Caption = 'Wante'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -13
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object lbWobenD: TLabel
          Left = 168
          Top = 84
          Width = 73
          Height = 16
          Caption = 'Wante oben'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -13
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object lbD1: TLabel
          Left = 287
          Top = 14
          Width = 53
          Height = 16
          Alignment = taRightJustify
          Caption = 'lbValue1'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -13
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object lbD2: TLabel
          Left = 287
          Top = 36
          Width = 53
          Height = 16
          Alignment = taRightJustify
          Caption = 'lbValue2'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -13
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object lbD3: TLabel
          Left = 287
          Top = 60
          Width = 53
          Height = 16
          Alignment = taRightJustify
          Caption = 'lbValue3'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -13
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object lbD4: TLabel
          Left = 287
          Top = 84
          Width = 53
          Height = 16
          Alignment = taRightJustify
          Caption = 'lbValue4'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -13
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object lbD5: TLabel
          Left = 287
          Top = 108
          Width = 53
          Height = 16
          Alignment = taRightJustify
          Caption = 'lbValue5'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -13
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object lbSalingLD: TLabel
          Left = 168
          Top = 108
          Width = 79
          Height = 16
          Caption = 'Saling L'#228'nge'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -13
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object sbVorstagD: TScrollBar
          Tag = 1
          Left = 36
          Top = 36
          Width = 121
          Height = 17
          LargeChange = 10
          PageSize = 0
          TabOrder = 1
          OnScroll = sbControllerScroll
        end
        object sbWanteD: TScrollBar
          Tag = 2
          Left = 36
          Top = 60
          Width = 121
          Height = 17
          LargeChange = 5
          PageSize = 0
          TabOrder = 2
          OnScroll = sbControllerScroll
        end
        object sbWobenD: TScrollBar
          Tag = 3
          Left = 36
          Top = 84
          Width = 121
          Height = 17
          LargeChange = 5
          PageSize = 0
          TabOrder = 3
          OnScroll = sbControllerScroll
        end
        object sbSalingLD: TScrollBar
          Tag = 4
          Left = 36
          Top = 108
          Width = 121
          Height = 17
          LargeChange = 5
          PageSize = 0
          TabOrder = 4
          OnScroll = sbControllerScroll
        end
        object rbControllerD: TRadioButton
          Left = 8
          Top = 12
          Width = 20
          Height = 19
          TabOrder = 5
          OnClick = rbWinkelClick
          OnMouseDown = rbWinkelMouseDown
        end
        object rbVorstagD: TRadioButton
          Left = 8
          Top = 36
          Width = 20
          Height = 17
          Checked = True
          TabOrder = 6
          TabStop = True
          OnClick = rbWinkelClick
          OnMouseDown = rbWinkelMouseDown
        end
        object sbControllerD: TScrollBar
          Left = 36
          Top = 12
          Width = 121
          Height = 17
          LargeChange = 10
          PageSize = 0
          TabOrder = 0
          OnScroll = sbControllerScroll
        end
        object rbWanteD: TRadioButton
          Left = 8
          Top = 60
          Width = 20
          Height = 17
          TabOrder = 7
          OnClick = rbWinkelClick
          OnMouseDown = rbWinkelMouseDown
        end
        object rbWobenD: TRadioButton
          Left = 8
          Top = 84
          Width = 20
          Height = 17
          TabOrder = 8
          OnClick = rbWinkelClick
          OnMouseDown = rbWinkelMouseDown
        end
        object rbSalingLD: TRadioButton
          Left = 8
          Top = 108
          Width = 20
          Height = 17
          TabOrder = 9
          OnClick = rbWinkelClick
          OnMouseDown = rbWinkelMouseDown
        end
      end
      object pnMastD: TPanel
        Left = 393
        Top = 0
        Width = 64
        Height = 167
        Align = alClient
        BevelOuter = bvNone
        BorderStyle = bsSingle
        TabOrder = 1
        object PaintBoxMD: TPaintBox
          Left = 0
          Top = 0
          Width = 60
          Height = 163
          Align = alClient
          OnPaint = PaintBoxMPaint
          ExplicitHeight = 160
        end
      end
    end
    object tsOhne: TTabSheet
      Tag = 2
      Caption = 'ohne'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object pnOhneBK: TPanel
        Left = 0
        Top = 0
        Width = 393
        Height = 164
        Align = alLeft
        BevelOuter = bvNone
        BorderWidth = 2
        BorderStyle = bsSingle
        TabOrder = 0
        object lbControllerOhne: TLabel
          Left = 168
          Top = 14
          Width = 57
          Height = 16
          Caption = 'Controller'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -13
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object lbVorstagOhne: TLabel
          Left = 168
          Top = 36
          Width = 47
          Height = 16
          Caption = 'Vorstag'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -13
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object lbWanteOhne: TLabel
          Left = 168
          Top = 60
          Width = 39
          Height = 16
          Caption = 'Wante'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -13
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object lbOhne1: TLabel
          Left = 287
          Top = 14
          Width = 53
          Height = 16
          Alignment = taRightJustify
          Caption = 'lbValue1'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -13
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object lbOhne2: TLabel
          Left = 287
          Top = 36
          Width = 53
          Height = 16
          Alignment = taRightJustify
          Caption = 'lbValue2'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -13
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object lbOhne3: TLabel
          Left = 287
          Top = 60
          Width = 53
          Height = 16
          Alignment = taRightJustify
          Caption = 'lbValue3'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -13
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object sbVorstagOhne: TScrollBar
          Tag = 1
          Left = 36
          Top = 36
          Width = 121
          Height = 17
          LargeChange = 10
          PageSize = 0
          TabOrder = 1
          OnScroll = sbControllerScroll
        end
        object sbWanteOhne: TScrollBar
          Tag = 2
          Left = 36
          Top = 60
          Width = 121
          Height = 17
          LargeChange = 5
          PageSize = 0
          TabOrder = 2
          OnScroll = sbControllerScroll
        end
        object rbControllerOhne: TRadioButton
          Left = 8
          Top = 12
          Width = 20
          Height = 19
          TabOrder = 3
          OnClick = rbWinkelClick
          OnMouseDown = rbWinkelMouseDown
        end
        object rbVorstagOhne: TRadioButton
          Left = 8
          Top = 36
          Width = 20
          Height = 17
          Checked = True
          TabOrder = 4
          TabStop = True
          OnClick = rbWinkelClick
          OnMouseDown = rbWinkelMouseDown
        end
        object sbControllerOhne: TScrollBar
          Left = 36
          Top = 12
          Width = 121
          Height = 17
          LargeChange = 10
          PageSize = 0
          TabOrder = 0
          OnScroll = sbControllerScroll
        end
        object rbWanteOhne: TRadioButton
          Left = 8
          Top = 60
          Width = 20
          Height = 17
          TabOrder = 5
          OnClick = rbWinkelClick
          OnMouseDown = rbWinkelMouseDown
        end
      end
      object pnMastOhne: TPanel
        Left = 393
        Top = 0
        Width = 64
        Height = 164
        Align = alClient
        BevelOuter = bvNone
        BorderStyle = bsSingle
        TabOrder = 1
        object PaintBoxMOhne: TPaintBox
          Left = 0
          Top = 0
          Width = 60
          Height = 160
          Align = alClient
          OnPaint = PaintBoxMPaint
        end
      end
    end
    object tsOhneStarr: TTabSheet
      Tag = 3
      Caption = 'ohne ( Mast steif)'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object pnOhne: TPanel
        Left = 0
        Top = 0
        Width = 457
        Height = 164
        Align = alClient
        BevelOuter = bvNone
        BorderWidth = 2
        BorderStyle = bsSingle
        TabOrder = 0
        object lbVorstagOS: TLabel
          Left = 168
          Top = 36
          Width = 37
          Height = 13
          Caption = 'Vorstag'
        end
        object lbWPowerOS: TLabel
          Left = 168
          Top = 60
          Width = 85
          Height = 13
          Caption = 'Wantenspannung'
        end
        object lbValue7: TLabel
          Left = 288
          Top = 36
          Width = 40
          Height = 13
          Caption = 'lbValue7'
        end
        object lbValue8: TLabel
          Left = 288
          Top = 60
          Width = 40
          Height = 13
          Caption = 'lbValue8'
        end
        object rbVorstagOS: TRadioButton
          Left = 8
          Top = 36
          Width = 20
          Height = 17
          Checked = True
          TabOrder = 0
          TabStop = True
          OnClick = rbWinkelClick
          OnMouseDown = rbWinkelMouseDown
        end
        object sbVorstagOS: TScrollBar
          Tag = 6
          Left = 36
          Top = 36
          Width = 121
          Height = 17
          LargeChange = 10
          Max = 4700
          Min = 4200
          PageSize = 0
          Position = 4200
          TabOrder = 1
          OnScroll = sbControllerScroll
        end
        object sbWPowerOS: TScrollBar
          Tag = 7
          Left = 36
          Top = 60
          Width = 121
          Height = 17
          LargeChange = 10
          Max = 3000
          Min = 100
          PageSize = 0
          Position = 100
          TabOrder = 2
          OnScroll = sbControllerScroll
        end
      end
    end
    object tsDatenbank: TTabSheet
      Tag = 4
      Caption = 'Datenbank'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
    end
  end
end
