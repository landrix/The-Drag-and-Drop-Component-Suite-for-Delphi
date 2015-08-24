object FormMain: TFormMain
  Left = 269
  Top = 106
  Caption = 'TDataFormatAdapter demo'
  ClientHeight = 306
  ClientWidth = 518
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 518
    Height = 57
    Align = alTop
    BevelOuter = bvNone
    BorderWidth = 2
    Caption = ' '
    TabOrder = 0
    object Label1: TLabel
      Left = 2
      Top = 2
      Width = 514
      Height = 53
      Align = alClient
      AutoSize = False
      Caption = 
        'This application demonstrates how to use the TDataFormatAdapter ' +
        'component to extend existing drop target (or source) components ' +
        'with support for additional data formats.'
      WordWrap = True
    end
  end
  object GroupBox1: TGroupBox
    Left = 0
    Top = 57
    Width = 518
    Height = 72
    Align = alTop
    Caption = ' Text '
    TabOrder = 1
    object MemoText: TMemo
      Left = 2
      Top = 15
      Width = 514
      Height = 55
      Align = alClient
      BorderStyle = bsNone
      ParentColor = True
      TabOrder = 0
    end
  end
  object GroupBox2: TGroupBox
    Left = 0
    Top = 201
    Width = 518
    Height = 72
    Align = alTop
    Caption = ' URLs '
    TabOrder = 2
    object MemoURL: TMemo
      Left = 2
      Top = 15
      Width = 514
      Height = 55
      Align = alClient
      BorderStyle = bsNone
      ParentColor = True
      TabOrder = 0
    end
  end
  object GroupBox3: TGroupBox
    Left = 0
    Top = 129
    Width = 518
    Height = 72
    Align = alTop
    Caption = ' Files '
    TabOrder = 3
    object MemoFile: TMemo
      Left = 2
      Top = 15
      Width = 514
      Height = 55
      Align = alClient
      BorderStyle = bsNone
      ParentColor = True
      TabOrder = 0
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 273
    Width = 518
    Height = 33
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 4
    Caption = 'Drop text, files or URLs anywhere within this window.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Shell Dlg 2'
    Font.Style = []
    ParentFont = False
    TabOrder = 4
  end
end
