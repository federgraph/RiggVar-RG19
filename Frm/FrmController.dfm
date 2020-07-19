object FormController: TFormController
  Left = 282
  Top = 186
  Caption = 'FormController'
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
  object OutputPages: TPageControl
    Left = 0
    Top = 0
    Width = 465
    Height = 255
    ActivePage = ControllerSheet
    TabOrder = 0
    object ControllerSheet: TTabSheet
      Tag = 1
      Caption = 'Controller'
      object pnController: TPanel
        Left = 0
        Top = 0
        Width = 457
        Height = 227
        Align = alClient
        BevelOuter = bvNone
        BorderStyle = bsSingle
        TabOrder = 0
        object ControllerPaintBox: TPaintBox
          Left = 0
          Top = 0
          Width = 453
          Height = 223
          Align = alClient
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -13
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          ExplicitHeight = 220
        end
        object ZustellenBtn: TButton
          Left = 72
          Top = 8
          Width = 75
          Height = 25
          Caption = 'Zustellen'
          TabOrder = 0
          OnClick = ZustellenBtnClick
        end
      end
    end
  end
end
