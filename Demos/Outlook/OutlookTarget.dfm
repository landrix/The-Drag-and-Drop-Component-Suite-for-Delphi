object FormOutlookTarget: TFormOutlookTarget
  Left = 348
  Top = 343
  Width = 680
  Height = 427
  ActiveControl = MemoBody
  Caption = 'Outlook drop target demo'
  Color = clBtnFace
  Constraints.MinWidth = 200
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = False
  PopupMenu = PopupMenuMain
  ShowHint = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object SplitterBrowser: TSplitter
    Left = 250
    Top = 0
    Height = 374
    AutoSnap = False
    MinSize = 100
    ResizeStyle = rsUpdate
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 374
    Width = 672
    Height = 19
    Panels = <>
    ParentFont = True
    SimplePanel = True
    UseSystemFont = False
  end
  object ListViewBrowser: TListView
    Left = 0
    Top = 0
    Width = 250
    Height = 374
    Align = alLeft
    Columns = <
      item
        Caption = 'From'
        Width = 100
      end
      item
        AutoSize = True
        Caption = 'Subject'
      end>
    ColumnClick = False
    Constraints.MinWidth = 100
    HideSelection = False
    ReadOnly = True
    RowSelect = True
    TabOrder = 2
    ViewStyle = vsReport
    OnDeletion = ListViewBrowserDeletion
    OnSelectItem = ListViewBrowserSelectItem
  end
  object PanelMain: TPanel
    Left = 253
    Top = 0
    Width = 419
    Height = 374
    Align = alClient
    BevelOuter = bvNone
    Caption = ' '
    TabOrder = 1
    object SplitterAttachments: TSplitter
      Left = 0
      Top = 320
      Width = 419
      Height = 3
      Cursor = crVSplit
      Align = alBottom
      AutoSnap = False
      MinSize = 50
      ResizeStyle = rsUpdate
      Visible = False
    end
    object PanelFrom: TPanel
      Left = 0
      Top = 0
      Width = 419
      Height = 16
      Align = alTop
      AutoSize = True
      BevelOuter = bvNone
      Caption = ' '
      TabOrder = 0
      DesignSize = (
        419
        16)
      object Label1: TLabel
        Left = 4
        Top = 0
        Width = 28
        Height = 13
        Caption = 'From:'
      end
      object EditFrom: TEdit
        Left = 64
        Top = 0
        Width = 350
        Height = 16
        Cursor = crHandPoint
        Anchors = [akLeft, akTop, akRight]
        AutoSize = False
        BorderStyle = bsNone
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -11
        Font.Name = 'MS Shell Dlg 2'
        Font.Style = [fsUnderline]
        ParentColor = True
        ParentFont = False
        ReadOnly = True
        TabOrder = 0
      end
    end
    object PanelTo: TPanel
      Left = 0
      Top = 16
      Width = 419
      Height = 16
      Align = alTop
      AutoSize = True
      BevelOuter = bvNone
      Caption = ' '
      TabOrder = 1
      DesignSize = (
        419
        16)
      object Label2: TLabel
        Left = 4
        Top = 0
        Width = 16
        Height = 13
        Caption = 'To:'
      end
      object ScrollBox1: TScrollBox
        Left = 64
        Top = 0
        Width = 350
        Height = 16
        Anchors = [akLeft, akTop, akRight]
        AutoSize = True
        BorderStyle = bsNone
        Constraints.MaxHeight = 60
        TabOrder = 0
        DesignSize = (
          350
          16)
        object ListViewTo: TListView
          Left = 0
          Top = 0
          Width = 350
          Height = 16
          Anchors = [akLeft, akTop, akRight]
          BorderStyle = bsNone
          Columns = <
            item
              AutoSize = True
              Caption = 'Name'
            end>
          ColumnClick = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlue
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsUnderline]
          HotTrackStyles = [htHandPoint]
          ReadOnly = True
          ParentColor = True
          ParentFont = False
          ShowColumnHeaders = False
          TabOrder = 0
          ViewStyle = vsReport
          OnInfoTip = ListViewToInfoTip
        end
      end
    end
    object PanelSubject: TPanel
      Left = 0
      Top = 32
      Width = 419
      Height = 16
      Align = alTop
      AutoSize = True
      BevelOuter = bvNone
      Caption = ' '
      TabOrder = 2
      DesignSize = (
        419
        16)
      object Label3: TLabel
        Left = 4
        Top = 0
        Width = 40
        Height = 13
        Caption = 'Subject:'
      end
      object EditSubject: TEdit
        Left = 64
        Top = 0
        Width = 350
        Height = 16
        Anchors = [akLeft, akTop, akRight]
        AutoSize = False
        BorderStyle = bsNone
        ParentColor = True
        ReadOnly = True
        TabOrder = 0
      end
    end
    object MemoBody: TRichEdit
      Left = 0
      Top = 48
      Width = 419
      Height = 272
      Align = alClient
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Verdana'
      Font.Style = []
      Constraints.MinHeight = 50
      Lines.Strings = (
        'Drag one or more items from Outlook and drop them anywhere '
        'on this form.'
        ''
        'Note: Outlook Express is not supported.')
      ParentFont = False
      ReadOnly = True
      ScrollBars = ssBoth
      TabOrder = 3
      WantReturns = False
    end
    object ListViewAttachments: TListView
      Left = 0
      Top = 323
      Width = 419
      Height = 51
      Align = alBottom
      Columns = <
        item
          AutoSize = True
          Caption = 'Name'
        end
        item
          Caption = 'Type'
          Width = 100
        end
        item
          Caption = 'Size'
          Width = 75
        end>
      ColumnClick = False
      Constraints.MinHeight = 40
      IconOptions.AutoArrange = True
      LargeImages = ImageListBig
      ReadOnly = True
      RowSelect = True
      PopupMenu = PopupMenuList
      SmallImages = ImageListSmall
      TabOrder = 4
      Visible = False
      OnDblClick = ListViewAttachmentsDblClick
      OnDeletion = ListViewAttachmentsDeletion
      OnResize = ListViewAttachmentsResize
    end
  end
  object DataFormatAdapterOutlook: TDataFormatAdapter
    DragDropComponent = DropEmptyTarget1
    DataFormatName = 'TOutlookDataFormat'
    Left = 60
    Top = 124
  end
  object DropEmptyTarget1: TDropEmptyTarget
    DragTypes = [dtCopy, dtLink]
    OnDrop = DropTextTarget1Drop
    Target = Owner
    Left = 28
    Top = 124
  end
  object ImageListSmall: TImageList
    ShareImages = True
    Left = 60
    Top = 176
  end
  object ImageListBig: TImageList
    ShareImages = True
    Left = 28
    Top = 176
  end
  object PopupMenuList: TPopupMenu
    Left = 28
    Top = 220
    object MenuAttachmentOpen: TMenuItem
      Action = ActionAttachmentOpen
      Default = True
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object MenuAttachmentView: TMenuItem
      Caption = 'View'
      object MenuAttachmentViewLargeIcons: TMenuItem
        Action = ActionAttachmentViewLargeIcons
        RadioItem = True
      end
      object MenuAttachmentViewSmallIcons: TMenuItem
        Action = ActionAttachmentViewSmallIcons
        RadioItem = True
      end
      object MenuAttachmentViewList: TMenuItem
        Action = ActionAttachmentViewList
        RadioItem = True
      end
      object MenuAttachmentViewDetails: TMenuItem
        Action = ActionAttachmentViewDetails
        RadioItem = True
      end
    end
  end
  object PopupMenuMain: TPopupMenu
    Left = 60
    Top = 220
    object Savetofile1: TMenuItem
      Action = ActionMessageSave
    end
    object N3: TMenuItem
      Caption = '-'
    end
    object Clear1: TMenuItem
      Action = ActionMessageClear
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object Paste1: TMenuItem
      Action = ActionPaste
    end
  end
  object ActionList1: TActionList
    Left = 28
    Top = 260
    object ActionPaste: TAction
      Category = 'Clipboard'
      Caption = 'Paste'
      ShortCut = 16470
      OnExecute = ActionPasteExecute
      OnUpdate = ActionPasteUpdate
    end
    object ActionAttachmentOpen: TAction
      Category = 'Attachment'
      Caption = 'Open'
      OnExecute = ActionAttachmentOpenExecute
      OnUpdate = ActionAttachmentOpenUpdate
    end
    object ActionAttachmentViewLargeIcons: TAction
      Category = 'Attachment'
      Caption = 'Large icons'
      OnExecute = ActionAttachmentViewLargeIconsExecute
    end
    object ActionAttachmentViewSmallIcons: TAction
      Category = 'Attachment'
      Caption = 'Small icons'
      OnExecute = ActionAttachmentViewSmallIconsExecute
    end
    object ActionAttachmentViewList: TAction
      Category = 'Attachment'
      Caption = 'List'
      OnExecute = ActionAttachmentViewListExecute
    end
    object ActionAttachmentViewDetails: TAction
      Category = 'Attachment'
      Caption = 'Details'
      OnExecute = ActionAttachmentViewDetailsExecute
    end
    object ActionMessageClear: TAction
      Category = 'Message'
      Caption = 'Clear'
      OnExecute = ActionMessageClearExecute
    end
    object ActionMessageSave: TAction
      Category = 'Message'
      Caption = 'Save to file...'
      OnExecute = ActionMessageSaveExecute
      OnUpdate = ActionMessageSaveUpdate
    end
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = '.msg'
    Filter = 'Outlook messages (*.msg)|*.msg|All files (*.*)|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Title = 'Save message'
    Left = 108
    Top = 176
  end
end
