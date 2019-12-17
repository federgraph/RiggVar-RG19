object IndicatorForm: TIndicatorForm
  Left = 55
  Top = 322
  BorderStyle = bsDialog
  Caption = '3D Indikator'
  ClientHeight = 150
  ClientWidth = 150
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = True
  Scaled = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Panel: TPanel
    Left = 0
    Top = 0
    Width = 150
    Height = 150
    Align = alClient
    BevelInner = bvLowered
    BevelOuter = bvNone
    BorderWidth = 2
    TabOrder = 0
    object PaintBox: TPaintBox
      Left = 3
      Top = 3
      Width = 144
      Height = 144
      Align = alClient
      Color = clWhite
      ParentColor = False
      OnMouseDown = PaintBoxMouseDown
      OnMouseMove = PaintBoxMouseMove
      OnMouseUp = PaintBoxMouseUp
      OnPaint = PaintBoxPaint
    end
  end
end
