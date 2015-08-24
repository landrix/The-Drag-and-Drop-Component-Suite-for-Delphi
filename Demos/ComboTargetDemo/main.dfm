object FormMain: TFormMain
  Left = 250
  Top = 194
  Caption = 'ComboTarget Demo'
  ClientHeight = 371
  ClientWidth = 489
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 129
    Width = 489
    Height = 242
    ActivePage = TabSheetData
    Align = alClient
    MultiLine = True
    TabOrder = 0
    object TabSheetText: TTabSheet
      Caption = 'Text'
      Enabled = False
      TabVisible = False
      object MemoText: TMemo
        Left = 0
        Top = 0
        Width = 481
        Height = 232
        Align = alClient
        TabOrder = 0
      end
    end
    object TabSheetFiles: TTabSheet
      Caption = 'Files'
      ImageIndex = 1
      TabVisible = False
      object Splitter1: TSplitter
        Left = 0
        Top = 166
        Width = 481
        Height = 3
        Cursor = crVSplit
        Align = alBottom
        ResizeStyle = rsUpdate
      end
      object ListBoxFiles: TListBox
        Left = 0
        Top = 0
        Width = 481
        Height = 166
        Align = alClient
        ItemHeight = 13
        TabOrder = 0
      end
      object ListBoxMaps: TListBox
        Left = 0
        Top = 169
        Width = 481
        Height = 63
        Align = alBottom
        ItemHeight = 13
        TabOrder = 1
      end
    end
    object TabSheetBitmap: TTabSheet
      Caption = 'Bitmap'
      ImageIndex = 2
      TabVisible = False
      object ScrollBox2: TScrollBox
        Left = 0
        Top = 0
        Width = 481
        Height = 232
        Align = alClient
        BorderStyle = bsNone
        TabOrder = 0
        object ImageBitmap: TImage
          Left = 0
          Top = 0
          Width = 639
          Height = 333
          AutoSize = True
        end
      end
    end
    object TabSheetURL: TTabSheet
      Caption = 'URL'
      ImageIndex = 3
      TabVisible = False
      object Label1: TLabel
        Left = 12
        Top = 20
        Width = 19
        Height = 13
        Margins.Bottom = 0
        Caption = 'URL'
        FocusControl = EditURLURL
      end
      object Label2: TLabel
        Left = 12
        Top = 44
        Width = 20
        Height = 13
        Margins.Bottom = 0
        Caption = 'Title'
        FocusControl = EditURLTitle
      end
      object EditURLURL: TEdit
        Left = 48
        Top = 16
        Width = 573
        Height = 21
        TabOrder = 0
      end
      object EditURLTitle: TEdit
        Left = 48
        Top = 40
        Width = 573
        Height = 21
        TabOrder = 1
      end
    end
    object TabSheetData: TTabSheet
      Caption = 'Data'
      ImageIndex = 4
      TabVisible = False
      object ListViewData: TListView
        Left = 0
        Top = 0
        Width = 481
        Height = 232
        Align = alClient
        Columns = <
          item
            AutoSize = True
            Caption = 'Filename'
            MinWidth = 200
          end
          item
            Alignment = taRightJustify
            Caption = 'Size'
            MinWidth = 50
          end>
        TabOrder = 0
        ViewStyle = vsReport
        OnDblClick = ListViewDataDblClick
      end
    end
    object TabSheetMetaFile: TTabSheet
      Caption = 'MetaFile'
      ImageIndex = 5
      TabVisible = False
      object ScrollBox1: TScrollBox
        Left = 0
        Top = 0
        Width = 481
        Height = 232
        Align = alClient
        BorderStyle = bsNone
        TabOrder = 0
        object ImageMetaFile: TImage
          Left = 0
          Top = 0
          Width = 635
          Height = 325
          AutoSize = True
        end
      end
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 489
    Height = 129
    Align = alTop
    BevelOuter = bvNone
    BorderWidth = 4
    Caption = ' '
    TabOrder = 1
    object PanelDropZone: TPanel
      Left = 181
      Top = 4
      Width = 304
      Height = 121
      Align = alClient
      BevelOuter = bvNone
      BorderWidth = 4
      Caption = ' '
      TabOrder = 0
      object Label3: TLabel
        Left = 4
        Top = 4
        Width = 296
        Height = 113
        Margins.Bottom = 0
        Align = alClient
        Caption = 
          'This application demonstrates the TComboTarget component.  TComb' +
          'oTarget is an example of a drop target component which can accep' +
          't multiple unrelated data formats.  Drop data anywhere in this w' +
          'indow to have it displayed in the pane below.'
        WordWrap = True
        ExplicitHeight = 65
      end
    end
    object Panel1: TPanel
      Left = 4
      Top = 4
      Width = 177
      Height = 121
      Align = alLeft
      BevelOuter = bvNone
      Caption = ' '
      TabOrder = 1
      object GroupBox1: TGroupBox
        Left = 0
        Top = 0
        Width = 177
        Height = 121
        Align = alClient
        Caption = ' Accept the following formats '
        TabOrder = 0
        ExplicitLeft = -2
        ExplicitTop = -2
        object CheckBoxText: TCheckBox
          Left = 12
          Top = 20
          Width = 97
          Height = 17
          Caption = 'Text'
          Checked = True
          State = cbChecked
          TabOrder = 0
          OnClick = CheckBoxTextClick
        end
        object CheckBoxFiles: TCheckBox
          Left = 12
          Top = 36
          Width = 97
          Height = 17
          Caption = 'Files'
          Checked = True
          State = cbChecked
          TabOrder = 1
          OnClick = CheckBoxFilesClick
        end
        object CheckBoxURLs: TCheckBox
          Left = 12
          Top = 52
          Width = 97
          Height = 17
          Caption = 'URLs'
          Checked = True
          State = cbChecked
          TabOrder = 2
          OnClick = CheckBoxURLsClick
        end
        object CheckBoxBitmaps: TCheckBox
          Left = 12
          Top = 68
          Width = 97
          Height = 17
          Caption = 'Bitmaps'
          Checked = True
          State = cbChecked
          TabOrder = 3
          OnClick = CheckBoxBitmapsClick
        end
        object CheckBoxMetaFiles: TCheckBox
          Left = 12
          Top = 84
          Width = 97
          Height = 17
          Caption = 'Meta files'
          Checked = True
          State = cbChecked
          TabOrder = 4
          OnClick = CheckBoxMetaFilesClick
        end
        object CheckBoxData: TCheckBox
          Left = 12
          Top = 100
          Width = 97
          Height = 17
          Caption = 'Data'
          Checked = True
          State = cbChecked
          TabOrder = 5
          OnClick = CheckBoxDataClick
        end
      end
    end
  end
  object DropComboTarget1: TDropComboTarget
    DragTypes = [dtCopy, dtLink]
    OnDrop = DropComboTarget1Drop
    Target = Owner
    OptimizedMove = True
    Left = 196
    Top = 80
  end
end
