object Form1: TForm1
  Left = 453
  Top = 158
  Width = 375
  Height = 472
  Caption = 'Drag and Drop Unicode Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 367
    Height = 438
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'Files'
      object GroupBox1: TGroupBox
        Left = 0
        Top = 114
        Width = 359
        Height = 296
        Align = alClient
        Caption = ' Filenames '
        TabOrder = 0
        object PaintBoxFiles: TPaintBox
          Left = 2
          Top = 15
          Width = 355
          Height = 279
          Align = alClient
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          PopupMenu = PopupMenuFileTarget
          OnPaint = PaintBoxFilesPaint
        end
      end
      object GroupBox2: TGroupBox
        Left = 0
        Top = 0
        Width = 359
        Height = 57
        Align = alTop
        Caption = ' Drop source '
        TabOrder = 1
        object PanelFileSource: TPanel
          Left = 2
          Top = 15
          Width = 355
          Height = 40
          Cursor = crHandPoint
          Align = alClient
          BevelOuter = bvNone
          Caption = 'Drag files with Unicode names from here'
          PopupMenu = PopupMenuFileSource
          TabOrder = 0
          OnMouseDown = PanelFileSourceMouseDown
        end
      end
      object GroupBox3: TGroupBox
        Left = 0
        Top = 57
        Width = 359
        Height = 57
        Align = alTop
        Caption = ' Drop target '
        TabOrder = 2
        object PanelFileTarget: TPanel
          Left = 2
          Top = 15
          Width = 355
          Height = 40
          Align = alClient
          BevelOuter = bvNone
          Caption = 'Drop files with Unicode names here'
          PopupMenu = PopupMenuFileTarget
          TabOrder = 0
        end
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Text'
      ImageIndex = 1
      object GroupBox4: TGroupBox
        Left = 0
        Top = 0
        Width = 359
        Height = 57
        Align = alTop
        Caption = ' Drop source '
        TabOrder = 0
        object PanelTextSource: TPanel
          Left = 2
          Top = 15
          Width = 355
          Height = 40
          Cursor = crHandPoint
          Align = alClient
          BevelOuter = bvNone
          Caption = 'Drag Unicode text from here'
          PopupMenu = PopupMenuTextSource
          TabOrder = 0
          OnMouseDown = PanelTextSourceMouseDown
        end
      end
      object GroupBox5: TGroupBox
        Left = 0
        Top = 57
        Width = 359
        Height = 57
        Align = alTop
        Caption = ' Drop target '
        TabOrder = 1
        object PanelTextTarget: TPanel
          Left = 2
          Top = 15
          Width = 355
          Height = 40
          Align = alClient
          BevelOuter = bvNone
          Caption = 'Drop Unicode text here'
          PopupMenu = PopupMenuTextTarget
          TabOrder = 0
        end
      end
      object GroupBox6: TGroupBox
        Left = 0
        Top = 114
        Width = 359
        Height = 296
        Align = alClient
        Caption = ' Text '
        TabOrder = 2
        object PaintBoxText: TPaintBox
          Left = 2
          Top = 15
          Width = 355
          Height = 279
          Align = alClient
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          PopupMenu = PopupMenuTextTarget
          OnPaint = PaintBoxTextPaint
        end
      end
    end
  end
  object DropFileTarget1: TDropFileTarget
    DragTypes = [dtCopy, dtLink]
    OnDrop = DropFileTarget1Drop
    Target = PanelFileTarget
    OptimizedMove = True
    Left = 60
    Top = 264
  end
  object DropFileSource1: TDropFileSource
    DragTypes = [dtCopy]
    Left = 60
    Top = 212
  end
  object DropTextSource1: TDropTextSource
    DragTypes = [dtCopy]
    Left = 172
    Top = 212
  end
  object DropTextTarget1: TDropTextTarget
    DragTypes = [dtCopy, dtLink]
    OnDrop = DropTextTarget1Drop
    Target = PanelTextTarget
    Left = 172
    Top = 264
  end
  object PopupMenuTextTarget: TPopupMenu
    Left = 172
    Top = 344
    object Pastefromclipboard1: TMenuItem
      Action = ActionTextPaste
    end
  end
  object ActionList1: TActionList
    Left = 120
    Top = 178
    object ActionTextPaste: TAction
      Caption = 'Paste from clipboard'
      OnExecute = ActionTextPasteExecute
      OnUpdate = ActionTextPasteUpdate
    end
    object ActionFilePaste: TAction
      Caption = 'Paste from clipboard'
      OnExecute = ActionFilePasteExecute
      OnUpdate = ActionFilePasteUpdate
    end
    object ActionTextCopy: TAction
      Caption = 'Copy to clipboard'
      OnExecute = ActionTextCopyExecute
    end
    object ActionFileCopy: TAction
      Caption = 'Copy to clipboard'
      OnExecute = ActionFileCopyExecute
    end
  end
  object PopupMenuFileTarget: TPopupMenu
    Left = 64
    Top = 342
    object Pastefromclipboard2: TMenuItem
      Action = ActionFilePaste
    end
  end
  object PopupMenuFileSource: TPopupMenu
    Left = 64
    Top = 310
    object Copytoclipboard1: TMenuItem
      Action = ActionFileCopy
    end
  end
  object PopupMenuTextSource: TPopupMenu
    Left = 172
    Top = 312
    object Copytoclipboard2: TMenuItem
      Action = ActionTextCopy
    end
  end
end
