object FormDemo: TFormDemo
  Left = 298
  Top = 109
  ActiveControl = ButtonText
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Drag and Drop Demo'
  ClientHeight = 380
  ClientWidth = 292
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -16
  Font.Name = 'Arial'
  Font.Style = [fsBold]
  OldCreateOrder = True
  Position = poScreenCenter
  Scaled = False
  PixelsPerInch = 96
  TextHeight = 19
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 292
    Height = 380
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 8
    Caption = ' '
    TabOrder = 0
    object ButtonText: TBitBtn
      Left = 63
      Top = 73
      Width = 161
      Height = 57
      Caption = 'Drag && Drop &Text'
      Glyph.Data = {
        96010000424D9601000000000000760000002800000018000000180000000100
        0400000000002001000000000000000000001000000000000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00777777777777
        7777777777777777777777777777007777777777777777777770F70777777777
        788888807770F80777777777777777700F0F70777777777788888870F00F8077
        7777777777777770FFF707777777777888888880FFFF00007787780000000080
        FFF77807778778FFFFFFFFF0FF778087878778FFFFFFFFF0F7780887878778FF
        88888870F780F887878778FFFFFFFFF0780FF087878778FF8888887080FFF087
        878778FFFFFFFFF00FFFF087878778FF88888870788FF087877778FFFFFFFFFF
        FFFFF087877778FF8888888888FFF087777778FFFFFFFFFFFFFFF087777778FF
        FFFFFFFFFFFFF087777778FFFFFFFFFFFFFFF077777778888888888888888877
        7777777777777777777777777777777777777777777777777777}
      Layout = blGlyphTop
      TabOrder = 0
      OnClick = ButtonTextClick
    end
    object ButtonExit: TBitBtn
      Left = 63
      Top = 258
      Width = 161
      Height = 26
      Cancel = True
      Caption = 'E&xit'
      TabOrder = 3
      OnClick = ButtonExitClick
    end
    object ButtonFile: TBitBtn
      Left = 63
      Top = 133
      Width = 161
      Height = 57
      Caption = 'Drag && Drop &Files'
      Glyph.Data = {
        96010000424D9601000000000000760000002800000018000000180000000100
        0400000000002001000000000000000000001000000000000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00777777777777
        7777777777777777777777777777007777777777777777777770F70777777777
        788888807770F80777777777777777700F0F70777777777788888870F00F8077
        7777777777777770FFF707777777777000000080FFFF00007787788888888880
        FFF77807778778FB7B7B7B70FF778087878778F7B7B7B7B0F7780887878778FB
        7B7B7B70F7807807878778F7B7B7B7B07807B807878778FB7B7B7B70807B7807
        878778F7B7B7B7B007B7B807878778FB7B7B7B707B7B7807877778F7B7B7B7B7
        B7B7B807877778FB7B7B7B7B7B7B7807777778FFFFFFFFFFFFFFF877777778B7
        B7B7B888888888777777778B7B7B877777777777777777788888777777777777
        7777777777777777777777777777777777777777777777777777}
      Layout = blGlyphTop
      TabOrder = 1
      OnClick = ButtonFileClick
    end
    object Panel2: TPanel
      Left = 8
      Top = 307
      Width = 276
      Height = 65
      Align = alBottom
      BevelOuter = bvNone
      BorderStyle = bsSingle
      Color = clGray
      TabOrder = 4
      object Label2: TLabel
        Left = 79
        Top = 5
        Width = 121
        Height = 14
        Alignment = taCenter
        Caption = 'Copyright (c) 1997-2008.'
        Color = clGray
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object Label4: TLabel
        Left = 96
        Top = 20
        Width = 76
        Height = 14
        Caption = 'Angus Johnson'
        Color = clGray
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object Label5: TLabel
        Left = 28
        Top = 35
        Width = 86
        Height = 14
        Caption = 'Anders Melander:'
        Color = clGray
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object Label7: TLabel
        Left = 127
        Top = 35
        Width = 84
        Height = 14
        Cursor = crHandPoint
        Caption = 'http://melander.dk'
        Color = clGray
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsUnderline]
        ParentColor = False
        ParentFont = False
        OnClick = Label1Click
      end
    end
    object Panel3: TPanel
      Left = 8
      Top = 8
      Width = 276
      Height = 29
      Align = alTop
      BevelOuter = bvNone
      BorderWidth = 1
      BorderStyle = bsSingle
      Caption = 'Drag and Drop Demo'
      Color = clGray
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -16
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 5
    end
    object ButtonURL: TBitBtn
      Left = 63
      Top = 193
      Width = 161
      Height = 57
      Caption = '&URLs && Bitmaps'
      Glyph.Data = {
        96010000424D9601000000000000760000002800000018000000180000000100
        0400000000002001000000000000000000001000000010000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00777777777777
        777777777777777777777777777007777777777788888877770F707777777777
        77777707770F8077777777788888870070F70777777777777777770F00F80777
        777778000000080FFF7088880777784F4B4E4E0FFFF0000B077778C4E4E4CE0F
        FF7780A80787784C4F4B4E0FF7780A22078778C4B4E4CE0F7780A222078778EF
        EFEFEE0F780E2A22078778EEEEEEEE0780EEA222078778EEEEEEEE080EEEEEA2
        078778EEEEEEEE00EEEEEEEA078778EEEBBEEE0EEEEEEEEE078778EEBFFBEEEE
        EEEEEEEE078778EEBFFBEEEEEEEEEEEE078778EEEBBEEEEEEEEEEEEE078778EE
        EEEEEEEEEEEEEEEE077778EEEEEEEEEEEEEEEEEE077778888888888888888888
        8777777777777777777777777777777777777777777777777777}
      Layout = blGlyphTop
      TabOrder = 2
      OnClick = ButtonURLClick
    end
  end
end
