object FormKraft: TFormKraft
  Left = 282
  Top = 186
  Caption = 'FormKraft'
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
    ActivePage = KraftSheet
    TabOrder = 0
    object KraftSheet: TTabSheet
      Caption = 'Kr'#228'fte'
      object pnKraft: TPanel
        Left = 0
        Top = 0
        Width = 457
        Height = 227
        Align = alClient
        BevelOuter = bvNone
        BorderStyle = bsSingle
        TabOrder = 0
        object KraftPaintBox: TImage
          Left = 0
          Top = 0
          Width = 345
          Height = 223
          Align = alLeft
          ExplicitHeight = 220
        end
        object UpateBtn: TButton
          Left = 368
          Top = 8
          Width = 75
          Height = 25
          Caption = 'Update'
          TabOrder = 0
          OnClick = UpateBtnClick
        end
      end
    end
  end
end
