object FormAutoScroll: TFormAutoScroll
  Left = 251
  Top = 176
  Caption = 'Custom auto scroll demo'
  ClientHeight = 409
  ClientWidth = 428
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 428
    Height = 264
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 4
    Caption = ' '
    TabOrder = 0
    object StringGrid1: TStringGrid
      Left = 4
      Top = 45
      Width = 420
      Height = 215
      Align = alClient
      ColCount = 50
      Ctl3D = False
      RowCount = 26
      Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goThumbTracking]
      ParentCtl3D = False
      TabOrder = 0
      RowHeights = (
        24
        24
        24
        24
        24
        24
        24
        24
        24
        24
        24
        24
        24
        24
        24
        24
        24
        24
        24
        24
        24
        24
        24
        24
        24
        24)
    end
    object PanelSource: TPanel
      Left = 4
      Top = 4
      Width = 420
      Height = 41
      Cursor = crHandPoint
      Align = alTop
      BevelOuter = bvNone
      Caption = 'Drag from here and into the grid below'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'MS Shell Dlg 2'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 1
      OnMouseDown = PanelSourceMouseDown
    end
  end
  object Panel3: TPanel
    Left = 0
    Top = 264
    Width = 428
    Height = 145
    Align = alBottom
    BevelOuter = bvNone
    BorderWidth = 4
    Caption = ' '
    TabOrder = 1
    object Label1: TLabel
      Left = 4
      Top = 4
      Width = 420
      Height = 137
      Margins.Bottom = 0
      Align = alClient
      Caption = 
        'This application demonstrates custom auto scroll margins.'#13#10#13#10'Sta' +
        'ndard auto scroll uses fixed size scroll margins (or "scroll zon' +
        'e") where the size of the margins are calculated based on the ta' +
        'rget control scroll bar width.'#13#10#13#10'This works well for most uses,' +
        ' but in some situations it is desirable to be able to define dif' +
        'ferent scroll margins for each of the target control'#39's edges. An' +
        ' example is the above grid where we would like the scroll zone t' +
        'o be calculated relative to the data area of the grid.'
      ShowAccelChar = False
      WordWrap = True
      ExplicitHeight = 117
    end
  end
  object DropTextTarget1: TDropTextTarget
    DragTypes = [dtCopy]
    OnEnter = DropTextTarget1Enter
    OnDragOver = DropTextTarget1DragOver
    OnDrop = DropTextTarget1Drop
    Target = StringGrid1
    Left = 24
    Top = 56
  end
  object DropTextSource1: TDropTextSource
    DragTypes = [dtCopy]
    OnFeedback = DropTextSource1Feedback
    Left = 24
    Top = 8
  end
end
