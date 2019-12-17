object MemoFormC: TMemoFormC
  Left = 309
  Top = 172
  Caption = 'Diagramm Erzeugungsdaten'
  ClientHeight = 211
  ClientWidth = 380
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = 16
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 16
  object Panel: TPanel
    Left = 0
    Top = 0
    Width = 380
    Height = 211
    Align = alClient
    BevelInner = bvLowered
    BevelOuter = bvNone
    BorderWidth = 4
    TabOrder = 0
    object Memo: TMemo
      Left = 5
      Top = 5
      Width = 370
      Height = 201
      Align = alClient
      BorderStyle = bsNone
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = 16
      Font.Name = 'Courier New'
      Font.Pitch = fpFixed
      Font.Style = []
      Lines.Strings = (
        'Memo'
        'Test iiiij')
      ParentFont = False
      ReadOnly = True
      ScrollBars = ssVertical
      TabOrder = 0
      WordWrap = False
    end
  end
end
