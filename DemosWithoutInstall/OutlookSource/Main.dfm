object FormOutlookSource: TFormOutlookSource
  Left = 200
  Top = 189
  Caption = 'Outlook Drop Source demo'
  ClientHeight = 267
  ClientWidth = 516
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnMouseDown = FormMouseDown
  PixelsPerInch = 96
  TextHeight = 13
  object Memo1: TMemo
    Left = 0
    Top = 41
    Width = 516
    Height = 226
    Align = alClient
    BorderStyle = bsNone
    Lines.Strings = (
      
        'This application demonstrates how to drag Outlook messages (or o' +
        'ther '
      'Outlook items) FROM your application and drop them on Outlook.'
      ''
      
        '1) In Outlook save an item to disk (e.g. drag it from Outlook to' +
        ' the desktop).'
      ''
      '2) Drag the Outlook item to this application.'
      ''
      '3) Drag the Outlook item from this application back to Outlook.'
      ''
      
        'Note: Steps 1 & 2 just makes sure this demo has an item to work ' +
        'with. You don'#39't need to do that in your '
      'own code.')
    ReadOnly = True
    TabOrder = 0
    WantReturns = False
    OnMouseDown = FormMouseDown
  end
  object PanelReady: TPanel
    Left = 0
    Top = 0
    Width = 516
    Height = 41
    Cursor = crHandPoint
    Align = alTop
    Caption = 
      'Outlook item loaded. Now drag it back to Outlook (or the Desktop' +
      ', Explorer, etc.).'
    FullRepaint = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 1
    Visible = False
    OnMouseDown = FormMouseDown
  end
end
