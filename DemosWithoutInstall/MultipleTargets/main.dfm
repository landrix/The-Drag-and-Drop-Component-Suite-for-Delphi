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
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnMouseMove = FormMouseMove
  TextHeight = 13
  object Label1: TLabel
    Left = 0
    Top = 0
    Width = 492
    Height = 50
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
end
