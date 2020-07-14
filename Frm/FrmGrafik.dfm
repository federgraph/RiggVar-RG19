object GrafikForm: TGrafikForm
  Left = 417
  Top = 33
  Caption = 'GrafikForm'
  ClientHeight = 457
  ClientWidth = 305
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = True
  OnHide = FormHide
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object ViewTab: TTabControl
    Left = 0
    Top = 0
    Width = 305
    Height = 457
    TabOrder = 0
    Tabs.Strings = (
      'Seite'
      'Hinten'
      'Oben'
      '3D')
    TabIndex = 0
    OnChange = ViewTabChange
    object pnGetriebe: TPanel
      Left = 4
      Top = 24
      Width = 297
      Height = 429
      Align = alClient
      BevelOuter = bvNone
      BorderStyle = bsSingle
      TabOrder = 0
      object PaintBoxG: TPaintBox
        Left = 0
        Top = 0
        Width = 293
        Height = 425
        Align = alClient
        OnClick = PaintBoxGClick
        OnPaint = PaintBoxGPaint
        ExplicitHeight = 422
      end
    end
  end
end
