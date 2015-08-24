object FormTarget: TFormTarget
  Left = 281
  Top = 400
  BorderIcons = [biMinimize, biMaximize]
  BorderStyle = bsDialog
  Caption = 'Custom Drop Target'
  ClientHeight = 229
  ClientWidth = 400
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = False
  Visible = True
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 400
    Height = 229
    Align = alClient
    BevelInner = bvLowered
    BevelOuter = bvNone
    BorderWidth = 4
    Caption = ' '
    TabOrder = 0
    object PanelDest: TPanel
      Left = 5
      Top = 24
      Width = 390
      Height = 200
      Align = alClient
      Caption = 'Drop here'
      TabOrder = 0
    end
    object Panel5: TPanel
      Left = 5
      Top = 5
      Width = 390
      Height = 19
      Align = alTop
      Caption = 'Drop target'
      TabOrder = 1
    end
  end
  object DropTextTarget1: TDropTextTarget
    DragTypes = [dtCopy, dtLink]
    OnDrop = DropTextTarget1Drop
    Target = PanelDest
    Left = 16
    Top = 32
  end
end
