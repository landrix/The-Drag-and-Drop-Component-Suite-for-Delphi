object FormTarget: TFormTarget
  Left = 281
  Top = 400
  Width = 446
  Height = 261
  ActiveControl = RichEditSample
  Caption = 'RTF Drop Target'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = False
  Position = poDefaultPosOnly
  ShowHint = True
  Visible = True
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 280
    Height = 227
    Align = alClient
    BevelInner = bvLowered
    BevelOuter = bvNone
    BorderWidth = 4
    Caption = ' '
    TabOrder = 0
    object PanelDest: TPanel
      Left = 5
      Top = 24
      Width = 270
      Height = 198
      Align = alClient
      TabOrder = 0
      object RichEdit1: TRichEdit
        Left = 1
        Top = 1
        Width = 268
        Height = 196
        TabStop = False
        Align = alClient
        BevelInner = bvNone
        BevelOuter = bvRaised
        BorderStyle = bsNone
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Shell Dlg 2'
        Font.Style = []
        Lines.Strings = (
          'Drop here...')
        ParentFont = False
        ReadOnly = True
        TabOrder = 0
        WantReturns = False
      end
    end
    object Panel5: TPanel
      Left = 5
      Top = 5
      Width = 270
      Height = 19
      Align = alTop
      Caption = 'Drop RTF and plain text in the control below'
      TabOrder = 1
    end
  end
  object Panel1: TPanel
    Left = 280
    Top = 0
    Width = 158
    Height = 227
    Align = alRight
    BevelInner = bvLowered
    BevelOuter = bvNone
    BorderWidth = 4
    Caption = ' '
    TabOrder = 1
    object Panel3: TPanel
      Left = 5
      Top = 24
      Width = 148
      Height = 198
      Align = alClient
      TabOrder = 0
      object RichEditSample: TRichEdit
        Left = 1
        Top = 1
        Width = 146
        Height = 196
        Cursor = crHandPoint
        Align = alClient
        BevelInner = bvNone
        BevelOuter = bvRaised
        BorderStyle = bsNone
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Shell Dlg 2'
        Font.Style = []
        ParentFont = False
        ReadOnly = True
        TabOrder = 0
        WantReturns = False
      end
    end
    object Panel4: TPanel
      Left = 5
      Top = 5
      Width = 148
      Height = 19
      Align = alTop
      Caption = 'Sample RTF text'
      TabOrder = 1
    end
  end
  object DropTextTarget1: TDropTextTarget
    DragTypes = [dtCopy]
    OnDrop = DropTextTarget1Drop
    Target = RichEdit1
    AutoRegister = False
    Left = 44
    Top = 40
  end
end
