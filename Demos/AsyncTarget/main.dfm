object FormMain: TFormMain
  Left = 288
  Top = 227
  Caption = 'Async Data Transfer Demo - Drop target'
  ClientHeight = 361
  ClientWidth = 573
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
  object StatusBar1: TStatusBar
    Left = 0
    Top = 342
    Width = 573
    Height = 19
    Panels = <>
    SimplePanel = True
    SimpleText = 
      'Note: Asyncronous drop targets are only supported on Windows 200' +
      '0, Windows ME and later.'
  end
  object ProgressBar1: TProgressBar
    Left = 0
    Top = 327
    Width = 573
    Height = 15
    Align = alBottom
    Max = 1000000
    Step = 1024
    TabOrder = 1
  end
  object Panel5: TPanel
    Left = 0
    Top = 0
    Width = 573
    Height = 105
    Align = alTop
    BevelOuter = bvLowered
    Caption = ' '
    TabOrder = 2
    object PanelTarget: TPanel
      Left = 1
      Top = 1
      Width = 160
      Height = 103
      Align = alLeft
      BevelOuter = bvNone
      BorderWidth = 4
      TabOrder = 0
      object Label4: TLabel
        Left = 4
        Top = 4
        Width = 152
        Height = 95
        Margins.Bottom = 0
        Align = alClient
        Alignment = taCenter
        Caption = 'Drop here'
        Color = 16758920
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -13
        Font.Name = 'MS Shell Dlg 2'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        ShowAccelChar = False
        Transparent = False
        Layout = tlCenter
        WordWrap = True
        ExplicitWidth = 57
        ExplicitHeight = 16
      end
    end
    object Panel6: TPanel
      Left = 161
      Top = 1
      Width = 411
      Height = 103
      Align = alClient
      BevelOuter = bvNone
      BorderWidth = 4
      TabOrder = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 185
      ExplicitHeight = 117
      object Label1: TLabel
        Left = 4
        Top = 4
        Width = 403
        Height = 95
        Margins.Bottom = 0
        Align = alClient
        Caption = 
          'This example demonstrates how to receive and process a drop in a' +
          ' thread.'#13#10'The advantage of using a thread is that the target app' +
          'lication isn'#39't blocked while the data is being transferred from ' +
          'the source.'#13#10#13#10'Note that this approach is normally only used whe' +
          'n transferring large amounts of data or when the drop source or ' +
          'drop target is very slow.'
        ShowAccelChar = False
        WordWrap = True
        ExplicitWidth = 171
        ExplicitHeight = 156
      end
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 105
    Width = 573
    Height = 222
    Cursor = crHandPoint
    Align = alClient
    BevelOuter = bvNone
    Caption = ' '
    ParentColor = True
    TabOrder = 3
    object Panel3: TPanel
      Left = 0
      Top = 0
      Width = 109
      Height = 222
      Align = alLeft
      BevelOuter = bvLowered
      Caption = ' '
      ParentColor = True
      TabOrder = 0
      object PaintBoxPie: TPaintBox
        Left = 1
        Top = 1
        Width = 107
        Height = 220
        Align = alClient
      end
    end
    object Panel4: TPanel
      Left = 109
      Top = 0
      Width = 464
      Height = 222
      Align = alClient
      BevelOuter = bvLowered
      BorderWidth = 8
      Caption = ' '
      TabOrder = 1
      DesignSize = (
        464
        222)
      object Label2: TLabel
        Left = 26
        Top = 24
        Width = 423
        Height = 93
        Margins.Bottom = 0
        Anchors = [akLeft, akTop, akRight]
        AutoSize = False
        Caption = 
          'During a normal data transfer the source application will be blo' +
          'cked while the transfer takes place because the applicaion is un' +
          'able to process events.'#13'Notice that the timer stops and the form' +
          ' can'#39't be moved or repainted during a normal data transfer.'
        ShowAccelChar = False
        WordWrap = True
      end
      object Label3: TLabel
        Left = 26
        Top = 136
        Width = 427
        Height = 85
        Margins.Bottom = 0
        Anchors = [akLeft, akTop, akRight]
        AutoSize = False
        Caption = 
          'During an asynchronous data transfer the data transfer is perfor' +
          'med in a separate thread so the target application appears unaff' +
          'ected by the transfer.'#13#10'Notice that the timer continues and the ' +
          'form can be moved and repainted during an asynchronous data tran' +
          'sfer.'
        ShowAccelChar = False
        WordWrap = True
      end
      object RadioButtonNormal: TRadioButton
        Left = 8
        Top = 8
        Width = 353
        Height = 17
        Caption = 'Normal synchronous transfer'
        Checked = True
        TabOrder = 0
        TabStop = True
        OnClick = RadioButtonNormalClick
      end
      object RadioButtonAsync: TRadioButton
        Left = 8
        Top = 120
        Width = 361
        Height = 17
        Caption = 'Asynchronous transfer'
        TabOrder = 1
        OnClick = RadioButtonAsyncClick
      end
    end
  end
  object Timer1: TTimer
    Interval = 100
    OnTimer = Timer1Timer
    Left = 8
    Top = 160
  end
  object DataFormatAdapterTarget: TDataFormatAdapter
    DragDropComponent = DropEmptyTarget1
    DataFormatName = 'TVirtualFileStreamDataFormat'
    Left = 40
    Top = 192
  end
  object DropEmptyTarget1: TDropEmptyTarget
    DragTypes = [dtCopy, dtLink]
    OnDrop = DropEmptyTarget1Drop
    OnStartAsyncTransfer = DropEmptyTarget1StartAsyncTransfer
    OnEndAsyncTransfer = DropEmptyTarget1EndAsyncTransfer
    Target = PanelTarget
    Left = 40
    Top = 160
  end
  object DropDummy1: TDropDummy
    DragTypes = []
    Target = Owner
    Left = 40
    Top = 236
  end
end
