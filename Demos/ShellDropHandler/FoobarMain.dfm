object FormFileList: TFormFileList
  Left = 279
  Top = 136
  Caption = 'Shell Drop Handler Demo'
  ClientHeight = 453
  ClientWidth = 470
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 412
    Width = 470
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    Caption = ' '
    TabOrder = 0
    object Memo1: TMemo
      Left = 0
      Top = 0
      Width = 470
      Height = 41
      Align = alClient
      BorderStyle = bsNone
      Color = clBtnFace
      Enabled = False
      ReadOnly = True
      TabOrder = 0
      WantReturns = False
    end
  end
  object MemoFileList: TMemo
    Left = 0
    Top = 0
    Width = 470
    Height = 412
    Align = alClient
    TabOrder = 1
    WordWrap = False
    OnChange = MemoFileListChange
  end
  object MainMenu1: TMainMenu
    Left = 24
    Top = 16
    object MenuFile: TMenuItem
      Caption = '&File'
      object MenuFileNew: TMenuItem
        Caption = '&New list'
        OnClick = MenuFileNewClick
      end
      object MenuFileOpen: TMenuItem
        Caption = '&Open'
        ShortCut = 114
        OnClick = MenuFileOpenClick
      end
      object MenuFileSave: TMenuItem
        Caption = '&Save'
        ShortCut = 113
        OnClick = MenuFileSaveClick
      end
      object MenuFileSaveAs: TMenuItem
        Caption = 'S&ave as...'
        ShortCut = 8305
        OnClick = MenuFileSaveAsClick
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object MenuFileExit: TMenuItem
        Caption = 'E&xit'
        ShortCut = 32883
        OnClick = MenuFileExitClick
      end
    end
    object MenuSetup: TMenuItem
      Caption = '&Setup'
      object MenuSetupRegister: TMenuItem
        Caption = '&Register'
        OnClick = MenuSetupRegisterClick
      end
      object MenuSetupUnregister: TMenuItem
        Caption = 'Unregister'
        OnClick = MenuSetupUnregisterClick
      end
    end
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = 'foobar'
    Filter = 'Foobar files (*.foobar)|*.foobar|All files (*.*)|*.*'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 60
    Top = 16
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'foobar'
    Filter = 'Foobar files (*.foobar)|*.foobar|All files (*.*)|*.*'
    Options = [ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Left = 96
    Top = 16
  end
end
