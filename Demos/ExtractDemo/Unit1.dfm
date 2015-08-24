object FormMain: TFormMain
  Left = 193
  Top = 169
  Caption = 'Extract/Download Demo'
  ClientHeight = 261
  ClientWidth = 437
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
    Top = 41
    Width = 437
    Height = 201
    Align = alClient
    Columns = <
      item
        Caption = 'A list of  '#39'archived'#39'  files...'
        Width = 400
      end>
    ColumnClick = False
    Items.ItemData = {
      01340100000500000000000000FFFFFFFFFFFFFFFF00000000000000000D5200
      6F006F007400460069006C00650031002E0074007800740000000000FFFFFFFF
      FFFFFFFF00000000000000000D52006F006F007400460069006C00650032002E
      0077007200690000000000FFFFFFFFFFFFFFFF00000000000000001353007500
      620046006F006C006400650072005C00460069006C00650033002E0070006100
      730000000000FFFFFFFFFFFFFFFF00000000000000001353007500620046006F
      006C006400650072005C00460069006C00650034002E00640066006D00000000
      00FFFFFFFFFFFFFFFF00000000000000002353007500620046006F006C006400
      650072005C004E006500730074006500640053007500620046006F006C006400
      650072005C00460069006C00650035002E00630070007000}
    MultiSelect = True
    ReadOnly = True
    RowSelect = True
    TabOrder = 0
    ViewStyle = vsReport
    OnMouseDown = ListView1MouseDown
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 437
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object Label2: TLabel
      Left = 6
      Top = 6
      Width = 286
      Height = 14
      Margins.Bottom = 0
      Caption = 'A demo of how to drag files from a zipped archive...'
    end
    object ButtonClose: TButton
      Left = 369
      Top = 5
      Width = 62
      Height = 25
      Cancel = True
      Caption = 'E&xit'
      TabOrder = 0
      OnClick = ButtonCloseClick
    end
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 242
    Width = 437
    Height = 19
    Panels = <>
    SimplePanel = True
    SimpleText = #39'Extract'#39'  files by dragging them to Explorer ...'
  end
  object DropFileSource1: TDropFileSource
    DragTypes = [dtCopy, dtMove]
    OnDrop = DropFileSource1Drop
    OnAfterDrop = DropFileSource1AfterDrop
    OnGetData = DropFileSource1GetData
    Left = 398
    Top = 92
  end
end
