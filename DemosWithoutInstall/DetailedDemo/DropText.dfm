object FormText: TFormText
  Left = 217
  Top = 143
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Drag '#39'n'#39' Drop  Demo - Text'
  ClientHeight = 342
  ClientWidth = 461
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  Icon.Data = {
    0000010001001010100000000000280100001600000028000000100000002000
    00000100040000000000C0000000000000000000000000000000000000000000
    0000000080000080000000808000800000008000800080800000C0C0C0008080
    80000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF000000
    0000000000000000000000F8000000000000F8F800000800080F0FF0000008FF
    FF0FFF08FF0008FFFF0FFFFF0F0008FFFF0FFFF0FF0008FF870FFF078F0008FF
    FF0FF0FFFF0008FF870F07888F0008FFFF00FFFFFF0008FF870788FFFF0008FF
    FFFFFFFFFF0008FFFFFFFFFFFF0008888888888888800000000000000000FFCF
    0000FD8F0000FC0F000080010000800100008001000080010000800100008001
    0000800100008001000080010000800100008001000080010000FFFF0000}
  OldCreateOrder = True
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Memo1: TMemo
    Left = 11
    Top = 181
    Width = 439
    Height = 68
    TabStop = False
    Alignment = taCenter
    Color = clBtnFace
    Lines.Strings = (
      'This (bottom) example demonstrates dragging a text SELECTION'
      'to another application that can accept text.'
      
        'The drag code is almost identical to above but requires the edit' +
        ' control'
      'to be hooked to override normal WM_LBUTTONDOWN processing.')
    ReadOnly = True
    TabOrder = 3
    WantReturns = False
  end
  object ButtonClose: TButton
    Left = 11
    Top = 286
    Width = 439
    Height = 30
    Cancel = True
    Caption = '&Close'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'MS Shell Dlg 2'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 5
    OnClick = ButtonCloseClick
  end
  object Edit2: TEdit
    Left = 11
    Top = 256
    Width = 439
    Height = 21
    AutoSelect = False
    TabOrder = 4
    Text = 
      'Select some or all of this text and drag it to another applicati' +
      'on which will accept text.'
    OnMouseMove = Edit2MouseMove
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 323
    Width = 461
    Height = 19
    Panels = <>
    ParentFont = True
    SimplePanel = True
    SizeGrip = False
    UseSystemFont = False
  end
  object Memo2: TMemo
    Left = 11
    Top = 46
    Width = 439
    Height = 67
    TabStop = False
    Alignment = taCenter
    Color = clBtnFace
    Lines.Strings = (
      
        'This (top) example demonstrates a very simple drag operation whi' +
        'ch allows '
      
        'dragging ALL of the edit control text TO and FROM other applicat' +
        'ions which '
      
        'accept Drag'#39'n'#39'Drop text (eg WordPad). Drag to Desktop to create ' +
        'scrap file.'
      'Implementing this takes only a few lines of code.')
    ReadOnly = True
    TabOrder = 0
    WantReturns = False
  end
  object Edit1: TEdit
    Left = 11
    Top = 118
    Width = 439
    Height = 21
    Cursor = crHandPoint
    AutoSelect = False
    TabOrder = 1
    Text = 
      'Click on this edit control and drag it to another application wh' +
      'ich will accept text.'
    OnMouseDown = Edit1MouseDown
  end
  object ButtonClipboard: TButton
    Left = 11
    Top = 145
    Width = 439
    Height = 24
    Caption = 'Click this button to copy the above text to the Clipboard'
    TabOrder = 2
    OnClick = ButtonClipboardClick
  end
  object Panel1: TPanel
    Left = 10
    Top = 8
    Width = 440
    Height = 30
    BevelOuter = bvNone
    BorderWidth = 1
    BorderStyle = bsSingle
    Caption = 'TDropTextSource, TDropTextTarget'
    Color = clGray
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -16
    Font.Name = 'MS Shell Dlg 2'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 7
  end
  object DropSource1: TDropTextSource
    DragTypes = [dtCopy]
    OnFeedback = DropSourceFeedback
    Left = 16
    Top = 51
  end
  object DropTextTarget1: TDropTextTarget
    DragTypes = [dtCopy, dtLink]
    OnDrop = DropTextTarget1Drop
    Target = Edit1
    Left = 416
    Top = 80
  end
  object DropTextTarget2: TDropTextTarget
    DragTypes = [dtCopy]
    OnDrop = DropTextTarget2Drop
    Target = Edit2
    Left = 418
    Top = 216
  end
  object DropDummy1: TDropDummy
    DragTypes = []
    Target = Owner
    Left = 416
    Top = 48
  end
end
