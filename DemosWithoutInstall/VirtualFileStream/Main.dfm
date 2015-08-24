object FormMain: TFormMain
  Left = 269
  Top = 103
  BorderIcons = [biSystemMenu, biMinimize]
  Caption = 'Virtual File Stream (File Contents) Demo'
  ClientHeight = 382
  ClientWidth = 505
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 505
    Height = 323
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 4
    Caption = ' '
    TabOrder = 0
    object ListView1: TListView
      Left = 4
      Top = 4
      Width = 497
      Height = 315
      Align = alClient
      Columns = <
        item
          Caption = 'File name'
          Width = 100
        end
        item
          AutoSize = True
          Caption = 'Contents'
        end>
      MultiSelect = True
      ReadOnly = True
      PopupMenu = PopupMenu1
      TabOrder = 0
      ViewStyle = vsReport
      OnMouseDown = OnMouseDown
      OnMouseMove = ListView1MouseMove
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 323
    Width = 505
    Height = 59
    Align = alBottom
    BevelOuter = bvNone
    BorderWidth = 4
    Caption = ' '
    TabOrder = 1
    object Label1: TLabel
      Left = 4
      Top = 4
      Width = 497
      Height = 51
      Align = alClient
      Caption = 
        'Drag between the above list and any application which supports t' +
        'he FileContents format (e.g. the VirtualFile demo application, F' +
        'irefox, Explorer or Outlook).'
      ShowAccelChar = False
      WordWrap = True
    end
  end
  object DropDummy1: TDropDummy
    DragTypes = []
    Target = Owner
    Left = 120
    Top = 160
  end
  object DropEmptySource1: TDropEmptySource
    DragTypes = [dtCopy, dtMove]
    OnAfterDrop = DropEmptySource1AfterDrop
    Left = 24
    Top = 68
  end
  object DropEmptyTarget1: TDropEmptyTarget
    DragTypes = [dtCopy, dtLink]
    OnEnter = DropEmptyTarget1Enter
    OnDrop = DropEmptyTarget1Drop
    Target = ListView1
    Left = 120
    Top = 68
  end
  object DataFormatAdapterSource: TDataFormatAdapter
    DragDropComponent = DropEmptySource1
    DataFormatName = 'TVirtualFileStreamDataFormat'
    Left = 24
    Top = 116
  end
  object DataFormatAdapterTarget: TDataFormatAdapter
    DragDropComponent = DropEmptyTarget1
    DataFormatName = 'TVirtualFileStreamDataFormat'
    Left = 120
    Top = 116
  end
  object PopupMenu1: TPopupMenu
    Left = 224
    Top = 68
    object MenuCopy: TMenuItem
      Action = ActionCopy
    end
    object MenuPaste: TMenuItem
      Action = ActionPaste
    end
  end
  object ActionList1: TActionList
    Left = 224
    Top = 116
    object ActionCopy: TAction
      Caption = 'Copy'
      ShortCut = 16451
      OnExecute = ActionCopyExecute
      OnUpdate = ActionCopyUpdate
    end
    object ActionPaste: TAction
      Caption = 'Paste'
      ShortCut = 16470
      OnExecute = ActionPasteExecute
      OnUpdate = ActionPasteUpdate
    end
  end
end
