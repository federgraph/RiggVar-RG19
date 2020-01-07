inherited ChartFormGS: TChartFormGS
  Left = 51
  Width = 838
  HorzScrollBar.Range = 795
  VertScrollBar.Range = 469
  Position = poScreenCenter
  OnPaint = FormPaint
  ExplicitWidth = 838
  PixelsPerInch = 96
  TextHeight = 13
  object ChartBevelOuter: TBevel [0]
    Left = 9
    Top = 152
    Width = 780
    Height = 314
    Style = bsRaised
  end
  object lbXLeft: TLabel [1]
    Left = 80
    Top = 446
    Width = 22
    Height = 13
    Caption = 'Xmin'
  end
  object lbAchseX: TLabel [2]
    Left = 280
    Top = 446
    Width = 43
    Height = 13
    Caption = 'lbAchseX'
  end
  object lbXRight: TLabel [3]
    Left = 671
    Top = 446
    Width = 26
    Height = 13
    Alignment = taRightJustify
    Caption = 'Xmax'
  end
  object ChartPaintBox: TPaintBox [4]
    Left = 80
    Top = 182
    Width = 601
    Height = 257
    OnPaint = ChartPaintBoxPaint
  end
  object PaintBoxLegend: TPaintBox [5]
    Left = 688
    Top = 182
    Width = 98
    Height = 257
    OnPaint = PaintBoxLegendPaint
  end
  object ChartBevelInner: TBevel [6]
    Left = 176
    Top = 246
    Width = 297
    Height = 145
  end
  object lbParam: TLabel [7]
    Left = 738
    Top = 158
    Width = 38
    Height = 13
    Alignment = taRightJustify
    Caption = 'lbParam'
  end
  inherited BedienPanel: TPanel
    inherited PEdit: TEdit
      Hint = 'Min/Max (Y-Achse) f'#252'r diese Kurve anzeigen|'
    end
  end
  inherited OpenDialog: TOpenDialog
    Left = 104
    Top = 200
  end
  inherited SaveDialog: TSaveDialog
    Left = 136
    Top = 200
  end
end
