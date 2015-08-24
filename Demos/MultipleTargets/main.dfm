object Form1: TForm1
  Left = 639
  Top = 369
  Caption = 'Multi Target demo'
  ClientHeight = 305
  ClientWidth = 492
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnDestroy = FormDestroy
  OnMouseMove = FormMouseMove
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 0
    Top = 0
    Width = 492
    Height = 50
    Margins.Bottom = 0
    Align = alTop
    Alignment = taCenter
    Caption = 
      'Example of multiple controls registrered to a single drop target' +
      ' component'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -21
    Font.Name = 'MS Shell Dlg 2'
    Font.Style = []
    ParentFont = False
    WordWrap = True
    ExplicitWidth = 486
  end
  object MemoLeft: TMemo
    Left = 40
    Top = 80
    Width = 185
    Height = 105
    TabOrder = 0
    OnMouseMove = FormMouseMove
  end
  object MemoRight: TMemo
    Left = 248
    Top = 80
    Width = 193
    Height = 105
    TabOrder = 1
    OnMouseMove = FormMouseMove
  end
  object CheckBoxLeft: TCheckBox
    Left = 88
    Top = 56
    Width = 97
    Height = 17
    Caption = 'Drop target'
    TabOrder = 2
    OnClick = CheckBoxLeftClick
  end
  object CheckBoxRight: TCheckBox
    Left = 296
    Top = 57
    Width = 97
    Height = 17
    Caption = 'Drop target'
    TabOrder = 3
    OnClick = CheckBoxRightClick
  end
  object MemoSource: TMemo
    Left = 40
    Top = 192
    Width = 401
    Height = 97
    Cursor = crHandPoint
    Alignment = taCenter
    Lines.Strings = (
      'Drag this text to one of the controls above...')
    TabOrder = 4
    WantReturns = False
    WordWrap = False
    OnMouseDown = MemoSourceMouseDown
  end
  object DropTextTarget1: TDropTextTarget
    DragTypes = [dtCopy]
    OnEnter = DropTextTarget1Enter
    OnLeave = DropTextTarget1Leave
    OnDrop = DropTextTarget1Drop
    MultiTarget = True
    Left = 224
    Top = 104
  end
  object DropTextSource1: TDropTextSource
    DragTypes = [dtCopy]
    Left = 224
    Top = 232
  end
  object DropDummy1: TDropDummy
    DragTypes = [dtCopy, dtMove, dtLink]
    Target = Owner
    Left = 224
    Top = 48
  end
end
