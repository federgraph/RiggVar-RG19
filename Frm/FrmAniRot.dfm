inherited AniRotationForm: TAniRotationForm
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  OnActivate = FormActivate
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  inherited ToolbarPanel: TPanel
    TabOrder = 1
    TabStop = False
    inherited Panel: TPanel
      inherited QuerBtn: TSpeedButton
        Hint = 'Lokales Riggobjekt einstellen|'
        OnClick = RiggTypItemClick
      end
      inherited FixPunktCombo: TComboBox
        Height = 21
        ExplicitHeight = 21
      end
    end
  end
  inherited RahmenPanel: TPanel
    Width = 544
    BorderWidth = 2
    TabOrder = 0
    ExplicitWidth = 544
    inherited PaintBox3D: TPaintBox
      Left = 3
      Top = 3
      Width = 538
      Height = 412
      ExplicitLeft = 3
      ExplicitTop = 3
      ExplicitWidth = 538
      ExplicitHeight = 392
    end
    inherited FocusEdit: TEdit
      Height = 21
      ExplicitHeight = 21
    end
  end
  inherited StatusBar: TStatusBar
    Panels = <
      item
        Bevel = pbNone
        Width = 31
      end
      item
        Alignment = taRightJustify
        Width = 50
      end
      item
        Bevel = pbNone
        Text = 'phi'
        Width = 30
      end
      item
        Alignment = taRightJustify
        Width = 50
      end
      item
        Bevel = pbNone
        Text = 'theta'
        Width = 40
      end
      item
        Alignment = taRightJustify
        Width = 50
      end
      item
        Bevel = pbNone
        Text = 'gamma'
        Width = 60
      end
      item
        Text = 'zoom'
        Width = 50
      end
      item
        Bevel = pbNone
        Text = 'zoom'
        Width = 50
      end>
  end
  object RightPanel: TPanel [3]
    Left = 575
    Top = 37
    Width = 151
    Height = 418
    Align = alRight
    BevelOuter = bvNone
    BorderWidth = 1
    TabOrder = 2
    object ScrollBarPanel: TPanel
      Left = 1
      Top = 1
      Width = 149
      Height = 112
      Align = alTop
      BevelInner = bvLowered
      BevelOuter = bvNone
      BorderWidth = 1
      TabOrder = 0
      object lbMin: TLabel
        Left = 12
        Top = 28
        Width = 16
        Height = 13
        Caption = 'Min'
      end
      object lbMax: TLabel
        Left = 12
        Top = 60
        Width = 20
        Height = 13
        Caption = 'Max'
      end
      object lbIst: TLabel
        Left = 12
        Top = 44
        Width = 13
        Height = 13
        Caption = 'Ist'
      end
      object lbMinVal: TLabel
        Left = 104
        Top = 28
        Width = 30
        Height = 13
        Alignment = taRightJustify
        Caption = 'MinVal'
      end
      object lbMaxVal: TLabel
        Left = 100
        Top = 60
        Width = 34
        Height = 13
        Alignment = taRightJustify
        Caption = 'MaxVal'
      end
      object lbIstVal: TLabel
        Left = 107
        Top = 44
        Width = 27
        Height = 13
        Alignment = taRightJustify
        Caption = 'IstVal'
      end
      object lbParam: TLabel
        Left = 12
        Top = 8
        Width = 50
        Height = 13
        Caption = 'Parameter'
        Color = clBtnFace
        ParentColor = False
      end
      object TrackBar: TTrackBar
        Left = 4
        Top = 80
        Width = 138
        Height = 25
        Position = 5
        TabOrder = 0
        TickStyle = tsNone
        OnChange = TrackBarChange
      end
    end
    object ListPanel: TPanel
      Left = 1
      Top = 113
      Width = 149
      Height = 304
      Align = alClient
      BevelOuter = bvNone
      BorderWidth = 1
      Caption = 'ListPanel'
      TabOrder = 1
      object ListBox: TListBox
        Left = 1
        Top = 1
        Width = 147
        Height = 302
        Align = alClient
        ItemHeight = 13
        Items.Strings = (
          'Controller'
          'Winkel'
          'Vorstag'
          'Wante'
          'Wante oben'
          'Saling H'#246'he'
          'Saling Abstand'
          'Saling L'#228'nge')
        TabOrder = 0
        OnClick = ListBoxClick
      end
    end
  end
  object Timer: TTimer
    Enabled = False
    Interval = 10
    OnTimer = TimerTimer
    Left = 144
    Top = 58
  end
end
