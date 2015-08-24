unit main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, DropSource, DragDropFile, DragDrop, DropTarget,
  ExtCtrls, DragDropText, ActnList, Menus;

type
  TForm1 = class(TForm)
    DropFileTarget1: TDropFileTarget;
    DropFileSource1: TDropFileSource;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    PanelFileSource: TPanel;
    GroupBox3: TGroupBox;
    PanelFileTarget: TPanel;
    GroupBox4: TGroupBox;
    PanelTextSource: TPanel;
    GroupBox5: TGroupBox;
    PanelTextTarget: TPanel;
    GroupBox6: TGroupBox;
    DropTextSource1: TDropTextSource;
    DropTextTarget1: TDropTextTarget;
    PaintBoxText: TPaintBox;
    PopupMenuTextTarget: TPopupMenu;
    Pastefromclipboard1: TMenuItem;
    ActionList1: TActionList;
    ActionTextPaste: TAction;
    ActionFilePaste: TAction;
    PaintBoxFiles: TPaintBox;
    PopupMenuFileTarget: TPopupMenu;
    Pastefromclipboard2: TMenuItem;
    PopupMenuFileSource: TPopupMenu;
    PopupMenuTextSource: TPopupMenu;
    ActionTextCopy: TAction;
    ActionFileCopy: TAction;
    Copytoclipboard1: TMenuItem;
    Copytoclipboard2: TMenuItem;
    procedure DropFileTarget1Drop(Sender: TObject; ShiftState: TShiftState;
      APoint: TPoint; var Effect: Integer);
    procedure PanelFileSourceMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PanelTextSourceMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DropTextTarget1Drop(Sender: TObject; ShiftState: TShiftState;
      APoint: TPoint; var Effect: Integer);
    procedure PaintBoxTextPaint(Sender: TObject);
    procedure ActionTextPasteExecute(Sender: TObject);
    procedure ActionTextPasteUpdate(Sender: TObject);
    procedure ActionFilePasteExecute(Sender: TObject);
    procedure ActionFilePasteUpdate(Sender: TObject);
    procedure PaintBoxFilesPaint(Sender: TObject);
    procedure ActionTextCopyExecute(Sender: TObject);
    procedure ActionFileCopyExecute(Sender: TObject);
  private
    FFiles: UnicodeString;
    FText: UnicodeString;
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

// This resource just contain a string resource with Unicode characters
{$R DragDropUnicodeTexts.res}

function LoadResString(Ident: integer): UnicodeString;
var
  Handle: HRSRC;
  p: PWideChar;
  Len: integer;
begin
  Result := '';
  Handle := FindResource(HInstance, PChar(Ident div 16 + 1), RT_STRING);
  p := Pointer(LoadResource(HInstance, Handle));

  if p = nil then
    exit;

  Ident := Ident mod 16;
  while (Ident >= 0) do
  begin
    Len := PWord(p)^;
    inc(p);
    if (Len = 0) then
      break;

    if (Ident = 0) then
    begin
      SetLength(Result, Len);
      Move(p^, PWideChar(Result)^, Len*SizeOf(WideChar));
      break;
    end;

    inc(p, Len);
    dec(Ident);
  end;
end;

procedure TForm1.DropFileTarget1Drop(Sender: TObject; ShiftState: TShiftState;
  APoint: TPoint; var Effect: Integer);
begin
  FFiles := DropFileTarget1.Files.Text;
  PaintBoxFiles.Invalidate;
end;

procedure TForm1.DropTextTarget1Drop(Sender: TObject; ShiftState: TShiftState;
  APoint: TPoint; var Effect: Integer);
begin
  FText := DropTextTarget1.UnicodeText;
  PaintBoxText.Invalidate;
end;

procedure TForm1.PanelFileSourceMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (DragDetectPlus(Sender as TWinControl)) then
  begin
    if (FFiles = '') then
      DropFileSource1.Files.Text := LoadResString(0)
    else
      DropFileSource1.Files.Text := FFiles;

    DropFileSource1.Execute;
  end;
end;

procedure TForm1.PanelTextSourceMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (DragDetectPlus(Sender as TWinControl)) then
  begin
    if (FText = '') then
      DropTextSource1.UnicodeText := LoadResString(0)
    else
      DropTextSource1.UnicodeText := FText;

    DropTextSource1.Execute;
  end;
end;

procedure TForm1.PaintBoxFilesPaint(Sender: TObject);
var
  r: TRect;
begin
  r := TPaintBox(Sender).ClientRect;
  TPaintBox(Sender).Canvas.FillRect(r);

  DrawTextExW(TPaintBox(Sender).Canvas.Handle,
    PWideChar(FFiles), Length(FFiles),
    r,
    DT_CENTER or DT_NOPREFIX or DT_VCENTER or DT_WORDBREAK, nil);
end;

procedure TForm1.PaintBoxTextPaint(Sender: TObject);
var
  r: TRect;
begin
  r := TPaintBox(Sender).ClientRect;
  TPaintBox(Sender).Canvas.FillRect(r);

  DrawTextExW(TPaintBox(Sender).Canvas.Handle,
    PWideChar(FText), Length(FText),
    r,
    DT_CENTER or DT_NOPREFIX or DT_VCENTER or DT_WORDBREAK, nil);
end;

procedure TForm1.ActionTextPasteExecute(Sender: TObject);
begin
  DropTextTarget1.PasteFromClipboard;
end;

procedure TForm1.ActionTextPasteUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := DropTextTarget1.CanPasteFromClipboard;
end;

procedure TForm1.ActionFilePasteExecute(Sender: TObject);
begin
  DropFileTarget1.PasteFromClipboard;
end;

procedure TForm1.ActionFilePasteUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := DropFileTarget1.CanPasteFromClipboard;
end;

procedure TForm1.ActionTextCopyExecute(Sender: TObject);
begin
  if (FText = '') then
    DropTextSource1.UnicodeText := LoadResString(0)
  else
    DropTextSource1.UnicodeText := FText;

  DropTextSource1.CopyToClipboard;
end;

procedure TForm1.ActionFileCopyExecute(Sender: TObject);
begin
  if (FFiles = '') then
    DropFileSource1.Files.Text := LoadResString(0)
  else
    DropFileSource1.Files.Text := FFiles;

  DropFileSource1.CopyToClipboard;
end;

end.

