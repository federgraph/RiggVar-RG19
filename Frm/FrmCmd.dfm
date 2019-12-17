object CommandForm: TCommandForm
  Left = 434
  Top = 341
  Caption = 'Kommandozeile'
  ClientHeight = 125
  ClientWidth = 316
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = 16
  Font.Name = 'Arial'
  Font.Style = []
  FormStyle = fsStayOnTop
  KeyPreview = True
  OldCreateOrder = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnHide = FormHide
  OnKeyDown = FormKeyDown
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 16
  object PanelEdit: TPanel
    Left = 0
    Top = 0
    Width = 316
    Height = 34
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object PromptEdit: TEdit
      Left = 0
      Top = 5
      Width = 57
      Height = 24
      TabStop = False
      Color = clBtnFace
      Ctl3D = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = 16
      Font.Name = 'Courier New'
      Font.Pitch = fpFixed
      Font.Style = []
      ParentCtl3D = False
      ParentFont = False
      ReadOnly = True
      TabOrder = 1
      Text = 'Input>'
    end
    object InputEdit: TEdit
      Left = 64
      Top = 5
      Width = 226
      Height = 24
      Ctl3D = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = 16
      Font.Name = 'Courier New'
      Font.Pitch = fpFixed
      Font.Style = []
      MaxLength = 128
      ParentCtl3D = False
      ParentFont = False
      TabOrder = 0
      Text = 'InputEdit'
      OnKeyDown = InputEditKeyDown
    end
  end
  object OutputMemo: TMemo
    Left = 0
    Top = 34
    Width = 316
    Height = 91
    Align = alClient
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = 16
    Font.Name = 'Courier New'
    Font.Pitch = fpFixed
    Font.Style = []
    Lines.Strings = (
      'OutputMemo')
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 1
    WordWrap = False
  end
end
