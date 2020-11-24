object FormSaling: TFormSaling
  Left = 282
  Top = 186
  Caption = 'FormSaling'
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
    ActivePage = Salingsheet
    TabOrder = 0
    object Salingsheet: TTabSheet
      Tag = 3
      Caption = 'Saling'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object pnSaling: TPanel
        Left = 0
        Top = 0
        Width = 457
        Height = 227
        Align = alClient
        BevelOuter = bvNone
        BorderStyle = bsSingle
        Color = clSilver
        TabOrder = 0
        object SalingPaintBox: TPaintBox
          Left = 0
          Top = 0
          Width = 453
          Height = 223
          Align = alClient
          Color = clWhite
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -16
          Font.Name = 'Arial Narrow'
          Font.Style = []
          ParentColor = False
          ParentFont = False
          ExplicitHeight = 220
        end
      end
    end
  end
end
