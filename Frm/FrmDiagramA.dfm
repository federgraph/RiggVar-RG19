object FormDiagramA: TFormDiagramA
  Left = 282
  Top = 186
  Caption = 'FormDiagramA'
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
  PixelsPerInch = 96
  TextHeight = 13
  object OutputPages: TPageControl
    Left = 0
    Top = 0
    Width = 465
    Height = 255
    ActivePage = ChartSheet
    TabOrder = 0
    object ChartSheet: TTabSheet
      Tag = 4
      Caption = 'Diagramm'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object pnChart2: TPanel
        Left = 0
        Top = 0
        Width = 457
        Height = 227
        Align = alClient
        BevelOuter = bvNone
        BorderStyle = bsSingle
        TabOrder = 0
        object KurveBtn: TSpeedButton
          Left = 360
          Top = 8
          Width = 73
          Height = 25
          Caption = 'Kurve'
          Glyph.Data = {
            76010000424D7601000000000000760000002800000020000000100000000100
            04000000000000010000120B0000120B00001000000000000000000000000000
            800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00337000000000
            73333337777777773F333308888888880333337F3F3F3FFF7F33330808089998
            0333337F737377737F333308888888880333337F3F3F3F3F7F33330808080808
            0333337F737373737F333308888888880333337F3F3F3F3F7F33330808080808
            0333337F737373737F333308888888880333337F3F3F3F3F7F33330808080808
            0333337F737373737F333308888888880333337F3FFFFFFF7F33330800000008
            0333337F7777777F7F333308000E0E080333337F7FFFFF7F7F33330800000008
            0333337F777777737F333308888888880333337F333333337F33330888888888
            03333373FFFFFFFF733333700000000073333337777777773333}
          NumGlyphs = 2
          OnClick = KurveBtnClick
        end
        object PunktBtn: TSpeedButton
          Left = 280
          Top = 8
          Width = 73
          Height = 25
          Caption = 'Punkt'
          Glyph.Data = {
            76010000424D7601000000000000760000002800000020000000100000000100
            04000000000000010000120B0000120B00001000000000000000000000000000
            800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00337000000000
            73333337777777773F333308888888880333337F3F3F3FFF7F33330808089998
            0333337F737377737F333308888888880333337F3F3F3F3F7F33330808080808
            0333337F737373737F333308888888880333337F3F3F3F3F7F33330808080808
            0333337F737373737F333308888888880333337F3F3F3F3F7F33330808080808
            0333337F737373737F333308888888880333337F3FFFFFFF7F33330800000008
            0333337F7777777F7F333308000E0E080333337F7FFFFF7F7F33330800000008
            0333337F777777737F333308888888880333337F333333337F33330888888888
            03333373FFFFFFFF733333700000000073333337777777773333}
          NumGlyphs = 2
          OnClick = PunktBtnClick
        end
        object KurveValidLED: TShape
          Left = 438
          Top = 8
          Width = 8
          Height = 24
          Hint = 'Rot: Kurve nicht aktuell|'
          Brush.Color = clGreen
        end
        object lbAchseX: TLabel
          Left = 128
          Top = 200
          Width = 43
          Height = 13
          Caption = 'lbAchseX'
        end
        object lbAchseY: TLabel
          Left = 48
          Top = 48
          Width = 43
          Height = 13
          Caption = 'lbAchseY'
        end
        object lbXLeft: TLabel
          Left = 64
          Top = 200
          Width = 22
          Height = 13
          Caption = 'Xmin'
        end
        object lbXRight: TLabel
          Left = 418
          Top = 200
          Width = 26
          Height = 13
          Alignment = taRightJustify
          Caption = 'Xmax'
        end
        object lbYTop: TLabel
          Left = 32
          Top = 72
          Width = 26
          Height = 13
          Alignment = taRightJustify
          Caption = 'Ymax'
        end
        object lbYBottom: TLabel
          Left = 36
          Top = 176
          Width = 22
          Height = 13
          Alignment = taRightJustify
          Caption = 'Ymin'
        end
        object ChartPaintBox: TPaintBox
          Left = 64
          Top = 70
          Width = 377
          Height = 124
          OnPaint = ChartPaintBoxPaint
        end
        object YCombo: TComboBox
          Left = 126
          Top = 8
          Width = 145
          Height = 21
          Style = csDropDownList
          TabOrder = 0
          OnChange = YComboChange
          Items.Strings = (
            'Durchbiegung hd'
            'Elastizit'#228't Punkt C'
            'Mastfall F0C'
            'Mastfall F0F'
            'Vorstag-Spannung'
            'Wanten-Spannung')
        end
        object cbFollowPoint: TCheckBox
          Left = 280
          Top = 39
          Width = 113
          Height = 17
          Caption = 'Punkt verfolgen'
          TabOrder = 1
          OnClick = cbFollowPointClick
        end
        object XCombo: TComboBox
          Left = 8
          Top = 8
          Width = 112
          Height = 21
          Style = csDropDownList
          TabOrder = 2
          OnChange = XComboChange
        end
      end
    end
  end
end
