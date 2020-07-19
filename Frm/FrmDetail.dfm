object FormDetail: TFormDetail
  Left = 282
  Top = 186
  Caption = 'FormDetail'
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
    ActivePage = DetailsSheet
    TabOrder = 0
    object DetailsSheet: TTabSheet
      Caption = 'Details'
      object DisplayMemo: TMemo
        Left = 0
        Top = 0
        Width = 457
        Height = 227
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -13
        Font.Name = 'Courier New'
        Font.Style = []
        Lines.Strings = (
          'DisplayMemo')
        ParentFont = False
        ScrollBars = ssVertical
        TabOrder = 0
      end
    end
  end
end
