object Form1: TForm1
  Left = 216
  Top = 237
  Caption = 'TDropPidlSource & TDropPidlTarget demo'
  ClientHeight = 232
  ClientWidth = 465
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  Scaled = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 14
  object ListView1: TListView
    Left = 0
    Top = 60
    Width = 465
    Height = 153
    Align = alClient
    Columns = <
      item
        Caption = 'Name'
        Width = 400
      end>
    MultiSelect = True
    TabOrder = 0
    ViewStyle = vsReport
    OnDblClick = ListView1DblClick
    OnDeletion = ListView1Deletion
    OnKeyPress = ListView1KeyPress
    OnMouseDown = ListView1MouseDown
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 465
    Height = 60
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object Label1: TLabel
      Left = 43
      Top = 10
      Width = 150
      Height = 13
      Caption = #169'1997-99 Johnson && Melander'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Shell Dlg 2'
      Font.Style = []
      ParentFont = False
    end
    object sbUpLevel: TSpeedButton
      Left = 4
      Top = 4
      Width = 25
      Height = 25
      Enabled = False
      Glyph.Data = {
        5A010000424D5A01000000000000760000002800000014000000130000000100
        040000000000E400000000000000000000001000000010000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00777777777777
        7777777700007777777777777777777700007777777777777777777700007700
        00000000000007770000770FBFBFBFBFBFBF07770000770BFBFBFBFBFBFB0777
        0000770FBFB000000FBF07770000770BFBF0FBFBFBFB07770000770FBFB0BFBF
        BFBF07770000770BF00000FBFBFB07770000770FBF000FBFBFBF07770000770B
        FBF0FBFBFBFB07770000770FBFBFBFBFBFBF0777000077000000000000007777
        00007770BFBFB077777777770000777700000777777777770000777777777777
        777777770000777777777777777777770000777777777777777777770000}
      OnClick = sbUpLevelClick
    end
    object Button1: TButton
      Left = 373
      Top = 5
      Width = 62
      Height = 25
      Cancel = True
      Caption = 'E&xit'
      TabOrder = 0
      OnClick = Button1Click
    end
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 213
    Width = 465
    Height = 19
    Panels = <>
    SimplePanel = True
    SimpleText = ' Copy files and/or folders by dragging them to and from Explorer'
  end
  object DropPIDLSource1: TDropPIDLSource
    DragTypes = [dtCopy, dtMove, dtLink]
    ImageHotSpotX = 40
    ImageHotSpotY = 8
    Left = 403
    Top = 98
  end
  object DropPIDLTarget1: TDropPIDLTarget
    DragTypes = [dtCopy, dtMove]
    GetDataOnEnter = True
    OnDragOver = DropPIDLTarget1DragOver
    OnDrop = DropPIDLTarget1Drop
    Left = 403
    Top = 129
  end
end
