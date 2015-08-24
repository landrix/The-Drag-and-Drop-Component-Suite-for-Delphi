object Form1: TForm1
  Left = 236
  Top = 162
  Caption = 'Simple Source Demo'
  ClientHeight = 273
  ClientWidth = 387
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  Scaled = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 387
    Height = 38
    Align = alTop
    BevelOuter = bvNone
    Caption = 'Drag files from the ListView onto Windows Explorer ...'
    TabOrder = 0
    OnMouseDown = ListView1MouseDown
  end
  object Panel2: TPanel
    Left = 0
    Top = 232
    Width = 387
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    Caption = ' '
    TabOrder = 1
    DesignSize = (
      387
      41)
    object ButtonClose: TButton
      Left = 157
      Top = 8
      Width = 75
      Height = 25
      Anchors = [akTop]
      Cancel = True
      Caption = '&Close'
      TabOrder = 0
      OnClick = ButtonCloseClick
    end
  end
  object ListView1: TListView
    Left = 0
    Top = 38
    Width = 387
    Height = 194
    Align = alClient
    Columns = <
      item
        Caption = 'Filenames'
        Width = 380
      end>
    ColumnClick = False
    MultiSelect = True
    ReadOnly = True
    TabOrder = 2
    ViewStyle = vsReport
    OnMouseDown = ListView1MouseDown
  end
  object DropFileSource1: TDropFileSource
    DragTypes = [dtCopy, dtMove, dtLink]
    OnGetDragImage = DropFileSource1GetDragImage
    ShowImage = True
    Left = 83
    Top = 72
  end
  object DropDummy1: TDropDummy
    DragTypes = []
    Target = Owner
    Left = 20
    Top = 72
  end
end
