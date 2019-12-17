object PreviewGForm: TPreviewGForm
  Left = 302
  Top = 158
  BorderStyle = bsDialog
  Caption = 'Grafik im Metafileformat ausgeben'
  ClientHeight = 245
  ClientWidth = 348
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object PaintBoxPanel: TPanel
    Left = 16
    Top = 16
    Width = 177
    Height = 209
    BevelInner = bvLowered
    BevelOuter = bvNone
    BorderWidth = 1
    TabOrder = 0
    object PreviewGBox: TPaintBox
      Left = 2
      Top = 2
      Width = 173
      Height = 205
      Align = alClient
      OnPaint = PreviewGBoxPaint
    end
  end
  object BitBtn1: TBitBtn
    Left = 216
    Top = 16
    Width = 113
    Height = 33
    Kind = bkClose
    NumGlyphs = 2
    TabOrder = 1
  end
  object PrintBtn: TBitBtn
    Left = 216
    Top = 56
    Width = 113
    Height = 33
    Caption = 'Ausgeben'
    Glyph.Data = {
      76010000424D7601000000000000760000002800000020000000100000000100
      04000000000000010000120B0000120B00001000000000000000000000000000
      800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00300000000000
      0003377777777777777308888888888888807F33333333333337088888888888
      88807FFFFFFFFFFFFFF7000000000000000077777777777777770F8F8F8F8F8F
      8F807F333333333333F708F8F8F8F8F8F9F07F333333333337370F8F8F8F8F8F
      8F807FFFFFFFFFFFFFF7000000000000000077777777777777773330FFFFFFFF
      03333337F3FFFF3F7F333330F0000F0F03333337F77773737F333330FFFFFFFF
      03333337F3FF3FFF7F333330F00F000003333337F773777773333330FFFF0FF0
      33333337F3F37F3733333330F08F0F0333333337F7337F7333333330FFFF0033
      33333337FFFF7733333333300000033333333337777773333333}
    NumGlyphs = 2
    TabOrder = 2
    OnClick = PrintBtnClick
  end
  object cbThickLines: TCheckBox
    Left = 216
    Top = 208
    Width = 97
    Height = 17
    Caption = 'dicke Linien'
    TabOrder = 3
    OnClick = cbThickLinesClick
  end
  object rgMediaChoice: TRadioGroup
    Left = 216
    Top = 104
    Width = 113
    Height = 81
    Caption = 'Medium'
    ItemIndex = 2
    Items.Strings = (
      'Drucker'
      'Datei'
      'Zwischenablage')
    TabOrder = 4
  end
  object SaveDlg: TSaveDialog
    DefaultExt = '*.emf'
    Filter = 'Enhanced Metafile (*.emf)|*.emf|Windows Metafile (*.wmf)|*.wmf'
    Left = 104
    Top = 88
  end
end
