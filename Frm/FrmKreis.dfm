object KreisForm: TKreisForm
  Left = 298
  Top = 114
  Caption = 'KreisForm'
  ClientHeight = 408
  ClientWidth = 443
  Color = clBlack
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'System'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyPress = FormKeyPress
  PixelsPerInch = 96
  TextHeight = 16
  object ImagePanel: TPanel
    Left = 0
    Top = 0
    Width = 443
    Height = 408
    Align = alClient
    BevelInner = bvLowered
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clTeal
    Font.Height = -21
    Font.Name = 'Times New Roman'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    object PaintBox: TPaintBox
      Left = 2
      Top = 2
      Width = 439
      Height = 372
      Align = alClient
      Color = clBtnFace
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clTeal
      Font.Height = -96
      Font.Name = 'Times New Roman'
      Font.Style = [fsBold, fsItalic]
      ParentColor = False
      ParentFont = False
      OnPaint = PaintBoxPaint
      ExplicitLeft = 8
      ExplicitTop = 8
      ExplicitWidth = 427
      ExplicitHeight = 360
    end
    object lbProductName: TLabel
      Left = 96
      Top = 128
      Width = 187
      Height = 108
      Caption = 'Rigg'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clTeal
      Font.Height = -96
      Font.Name = 'Times New Roman'
      Font.Style = [fsBold, fsItalic]
      ParentFont = False
    end
    object lbVersionText: TLabel
      Left = 248
      Top = 136
      Width = 96
      Height = 23
      Caption = 'Version 1.0'
    end
    object ControlPanel: TPanel
      Left = 2
      Top = 374
      Width = 439
      Height = 32
      Align = alBottom
      Alignment = taLeftJustify
      BevelOuter = bvNone
      Caption = '   (c) RiggVar  Software UG 1996 - 2011'
      Color = clBlack
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clTeal
      Font.Height = -13
      Font.Name = 'Times New Roman'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      object lbOK: TLabel
        Left = 284
        Top = 6
        Width = 37
        Height = 15
        Caption = 'Kreise'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clSilver
        Font.Height = -13
        Font.Name = 'Times New Roman'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object lbKreise: TLabel
        Left = 378
        Top = 6
        Width = 19
        Height = 15
        Caption = 'OK'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clSilver
        Font.Height = -13
        Font.Name = 'Times New Roman'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = True
      end
      object ShapeOK: TShape
        Left = 366
        Top = 2
        Width = 43
        Height = 23
        Brush.Style = bsClear
        Pen.Color = clTeal
        OnMouseDown = ShapeOKMouseDown
      end
      object ShapeKreise: TShape
        Left = 264
        Top = 2
        Width = 81
        Height = 23
        Brush.Style = bsClear
        Pen.Color = clTeal
        OnMouseDown = ShapeKreiseMouseDown
      end
    end
  end
  object Timer: TTimer
    Interval = 10
    OnTimer = TimerTimer
    Left = 336
    Top = 24
  end
end
