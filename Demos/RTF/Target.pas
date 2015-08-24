unit Target;

interface

uses
  DragDrop,
  DropTarget,
  DragDropFormats,
  DragDropText,
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, ComCtrls;

type
  TFormTarget = class(TForm)
    Panel2: TPanel;
    PanelDest: TPanel;
    DropTextTarget1: TDropTextTarget;
    Panel5: TPanel;
    RichEdit1: TRichEdit;
    Panel1: TPanel;
    Panel3: TPanel;
    RichEditSample: TRichEdit;
    Panel4: TPanel;
    procedure DropTextTarget1Drop(Sender: TObject; ShiftState: TShiftState;
      Point: TPoint; var Effect: Integer);
    procedure FormCreate(Sender: TObject);
  private
    RtfDataFormat: TGenericDataFormat;
  public
  end;

var
  FormTarget: TFormTarget;

implementation

{$R *.DFM}

const
  sRtfSample: AnsiString = '{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fswiss\fcharset0 Arial;}{\f1\fnil\fcharset2 Symbol;}}'+#13+
    '{\colortbl ;\red255\green0\blue0;\red0\green255\blue0;\red0\green0\blue255;}'+#13+
    '\viewkind4\uc1\pard\lang1030\f0\fs20 Normal, \b Bold\b0 , \i Italic \i0 and \ul Underlined\ulnone .\par'+#13+
    '\par'+#13+
    '\pard{\pntext\f1\''B7\tab}{\*\pn\pnlvlblt\pnf1\pnindent0{\pntxtb\''B7}}\fi-720\li720 bullet\lang1033\par'+#13+
    '{\pntext\f1\''B7\tab}another bullet\par'+#13+
    '\pard\par'+#13+
    '\pard\qc Centered text\par'+#13+
    '\pard\cf1\b Red\cf0 , \cf2 Green \cf0 and \cf3 Blue\cf0 .\par'+#13+
    '\b0\par }';

type
  TRtfDataFormat = class(TGenericDataFormat)
  public
    function Assign(Source: TClipboardFormat): boolean; override;
    function AssignTo(Dest: TClipboardFormat): boolean; override;
  end;

function TRtfDataFormat.Assign(Source: TClipboardFormat): boolean;
begin
  if (Source is TRichTextClipboardFormat) then
  begin
    Data := TRichTextClipboardFormat(Source).Text;
    Result := True;
  end else
    Result := inherited Assign(Source);
end;

function TRtfDataFormat.AssignTo(Dest: TClipboardFormat): boolean;
begin
  if (Dest is TRichTextClipboardFormat) then
  begin
    TRichTextClipboardFormat(Dest).Text := PAnsiChar(Data);
    Result := True;
  end else
    Result := inherited AssignTo(Dest);
end;

procedure TFormTarget.FormCreate(Sender: TObject);
var
  Stream: TStream;
begin
  // Define and register our custom data format.
  RtfDataFormat := TRtfDataFormat.Create(DropTextTarget1);
  RtfDataFormat.CompatibleFormats.Add(TRichTextClipboardFormat.Create);

  // Load the RTF sample
  Stream := TStringStream.Create(sRtfSample);
  try
    RichEditSample.Lines.LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
  RichEditSample.SelectAll;
end;

procedure TFormTarget.DropTextTarget1Drop(Sender: TObject;
  ShiftState: TShiftState; Point: TPoint; var Effect: Integer);
var
  Stream: TStream;
begin
  // Determine if we got a RTF string
  if (RtfDataFormat.HasData) then
  begin
    RichEdit1.PlainText := False;
    // Wrap the RTF string in a stream
    Stream := TStringStream.Create(RtfDataFormat.Data);
    try
      // Load the stream into the RichEdit control
      RichEdit1.Lines.LoadFromStream(Stream);
    finally
      Stream.Free;
    end;
  end else
  begin
    RichEdit1.PlainText := True;
    RichEdit1.Lines.Text := DropTextTarget1.Text;
  end;
end;


end.
