object FormSource: TFormSource
  Left = 276
  Top = 110
  BorderStyle = bsDialog
  Caption = 'Custom Drop Source'
  ClientHeight = 229
  ClientWidth = 400
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = False
  Visible = True
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 400
    Height = 96
    Align = alClient
    BevelInner = bvLowered
    BevelOuter = bvNone
    BorderWidth = 4
    Caption = ' '
    TabOrder = 0
    object PanelSource: TPanel
      Left = 5
      Top = 24
      Width = 390
      Height = 67
      Cursor = crHandPoint
      Align = alClient
      Caption = '00:00:00.000'
      TabOrder = 0
      OnMouseDown = PanelSourceMouseDown
    end
    object Panel4: TPanel
      Left = 5
      Top = 5
      Width = 390
      Height = 19
      Align = alTop
      Caption = 'Drop source'
      TabOrder = 1
    end
  end
  object Panel3: TPanel
    Left = 0
    Top = 96
    Width = 400
    Height = 133
    Align = alBottom
    BevelOuter = bvNone
    BorderWidth = 4
    Caption = ' '
    TabOrder = 1
    object Memo1: TMemo
      Left = 4
      Top = 4
      Width = 392
      Height = 125
      Align = alClient
      BorderStyle = bsNone
      Lines.Strings = (
        
          'This application demonstrates how to define and drag custom clip' +
          'board formats.'
        ''
        
          'The custom format stores the time-of-day and a color value in a ' +
          'structure. The '
        
          'TGenericDataFormat class is used to add support for this format ' +
          'to the '
        'TDropTextSource and TDropTextTarget components.'
        ''
        
          'To see the custom clipboard format in action, drag from the sour' +
          'ce window and '
        
          'drop on the target window. You can also do this between multiple' +
          ' instances of '
        'this '
        'application.')
      ParentColor = True
      ReadOnly = True
      TabOrder = 0
      WantReturns = False
    end
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 52
    Top = 32
  end
  object DropTextSource1: TDropTextSource
    DragTypes = [dtCopy]
    Left = 16
    Top = 32
  end
end
