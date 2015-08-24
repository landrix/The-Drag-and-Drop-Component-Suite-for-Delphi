object Form1: TForm1
  Left = 286
  Top = 171
  Width = 402
  Height = 288
  Caption = 'Simple Target Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  Scaled = False
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 394
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object Label1: TLabel
      Left = 8
      Top = 16
      Width = 257
      Height = 13
      Caption = 'Drop files from Windows Explorer onto this Listview...'
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 209
    Width = 394
    Height = 45
    Align = alBottom
    BevelOuter = bvNone
    Caption = ' '
    TabOrder = 1
    DesignSize = (
      394
      45)
    object ButtonClose: TButton
      Left = 159
      Top = 12
      Width = 75
      Height = 25
      Anchors = [akTop]
      Cancel = True
      Caption = '&Close'
      Default = True
      TabOrder = 0
      OnClick = ButtonCloseClick
    end
  end
  object ListView1: TListView
    Left = 0
    Top = 41
    Width = 394
    Height = 168
    Align = alClient
    Columns = <
      item
        Caption = 'Filename'
        Width = 380
      end
      item
        Caption = 'Original filename'
        Width = 0
      end>
    ReadOnly = True
    TabOrder = 2
    ViewStyle = vsReport
  end
  object DropFileTarget1: TDropFileTarget
    DragTypes = [dtCopy, dtMove, dtLink]
    OnDrop = DropFileTarget1Drop
    Target = ListView1
    OptimizedMove = True
    Left = 329
    Top = 220
  end
end
