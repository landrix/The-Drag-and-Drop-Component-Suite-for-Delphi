unit DropText;

interface

{$include dragdrop.inc} // Disables .NET warnings

uses
  DragDrop,
  DropSource,
  DropTarget,
  DragDropText,
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls, ComCtrls, ActiveX;

type
  TFormText = class(TForm)
    Memo1: TMemo;
    DropSource1: TDropTextSource;
    ButtonClose: TButton;
    Edit2: TEdit;
    StatusBar1: TStatusBar;
    Memo2: TMemo;
    Edit1: TEdit;
    ButtonClipboard: TButton;
    Panel1: TPanel;
    DropTextTarget1: TDropTextTarget;
    DropTextTarget2: TDropTextTarget;
    DropDummy1: TDropDummy;
    procedure ButtonCloseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Edit1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ButtonClipboardClick(Sender: TObject);
    procedure Edit2MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure DropTextTarget1Drop(Sender: TObject; ShiftState: TShiftState;
      Point: TPoint; var Effect: Integer);
    procedure DropTextTarget2Drop(Sender: TObject; ShiftState: TShiftState;
      Point: TPoint; var Effect: Integer);
    procedure DropSourceFeedback(Sender: TObject; Effect: Integer;
      var UseDefaultCursors: Boolean);
  private
    //used by bottom example
    OldEdit2WindowProc: TWndMethod;
    procedure NewEdit2WindowProc(var Msg : TMessage);

    function MouseIsOverEdit2Selection(XPos: integer): boolean;
    procedure StartEdit2Drag;
  public
    { Public declarations }
  end;

var
  FormText: TFormText;


implementation

{$R *.DFM}

// Custom Cursors defined in Cursors.Res (included in DropFile.pas):
const
  crTextCopy = 107;
  crTextMove = 108;
  crTextNoAccept = 109;

//----------------------------------------------------------------------------
// TFormText methods
//----------------------------------------------------------------------------
procedure TFormText.FormCreate(Sender: TObject);
begin
  // Used for Bottom Text Drag example...
  // Hook edit window so we can intercept WM_LBUTTONDOWN messages!
  OldEdit2WindowProc := Edit2.WindowProc;
  Edit2.WindowProc := NewEdit2WindowProc;

  // Load custom cursors.
  Screen.cursors[crTextCopy] := LoadCursor(hinstance, 'CUR_DRAG_COPY_TEXT');
  Screen.cursors[crTextMove] := LoadCursor(hinstance, 'CUR_DRAG_MOVE_TEXT');
  Screen.cursors[crTextNoAccept] := LoadCursor(hinstance, 'CUR_DRAG_NOACCEPT_TEXT');
end;

procedure TFormText.FormDestroy(Sender: TObject);
begin
  // Used by Bottom Text Drag example...
  // Unhook edit window...
  Edit2.WindowProc := OldEdit2WindowProc;
end;

procedure TFormText.ButtonCloseClick(Sender: TObject);
begin
  Close;
end;

//----------------------------------------------------------------------------
// The following 4 methods are all that's needed
// for the TOP Text Drop source and target examples.
//----------------------------------------------------------------------------
procedure TFormText.Edit1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Res: TDragResult;
begin
  if (Edit1.Text = '') then
    exit;

  // Wait for user to move cursor before we start the drag/drop.
  if (DragDetectPlus(TWinControl(Sender))) then
  begin
    Statusbar1.SimpleText := '';
    Edit1.SelLength := 0;

    // Copy the data into the drop source.
    DropSource1.Text := Edit1.Text;

    // Temporarily disable Edit1 as a target so we can't drop on the same
    // control as we are dragging from.
    DropTextTarget1.DragTypes := [];
    try

      // OK, now we are all set to go. Let's start the drag from Edit1...
      Res := DropSource1.Execute;

    finally
      // Enable Edit1 as a drop target again.
      DropTextTarget1.DragTypes := [dtCopy];
    end;

    // Display the result of the drag operation.
    case Res of
      drDropCopy: Statusbar1.SimpleText := 'Copied successfully';
      drDropLink: Statusbar1.SimpleText := 'Scrap file created successfully';
      drCancel: Statusbar1.SimpleText := 'Drop cancelled';
      drOutMemory: Statusbar1.SimpleText := 'Drop failed - out of memory';
    else
      Statusbar1.SimpleText := 'Drop failed - reason unknown';
    end;

  end;
end;


procedure TFormText.DropSourceFeedback(Sender: TObject; Effect: Integer;
  var UseDefaultCursors: Boolean);
begin
  // Provide custom drop source feedback.
  // Note: To use the standard drag/drop cursors, just disable this event
  // handler or set UseDefaultCursors to True.
  UseDefaultCursors := False; // We want to use our own cursors.

  // Ignore the drag scroll flag.
  case DWORD(Effect) and not DWORD(DROPEFFECT_SCROLL) of
    DROPEFFECT_COPY: Windows.SetCursor(Screen.Cursors[crTextCopy]);
    DROPEFFECT_MOVE: Windows.SetCursor(Screen.Cursors[crTextMove]);
  else
    Windows.SetCursor(Screen.Cursors[crTextNoAccept]);
  end;

end;

procedure TFormText.DropTextTarget1Drop(Sender: TObject;
  ShiftState: TShiftState; Point: TPoint; var Effect: Integer);
begin
  // Text has been dropped onto our drop target. Copy the dropped text into the
  // edit control.
  Edit1.text := DropTextTarget1.Text;
end;

procedure TFormText.ButtonClipboardClick(Sender: TObject);
begin
  if Edit1.Text <> '' then
  begin
    // Copy data into drop source component and then...
    DropSource1.Text := Edit1.Text;

    // ...Copy the data to the clipboard.
    DropSource1.CopyToClipboard;

    StatusBar1.SimpleText := 'Text copied to clipboard.';
  end;
end;

//----------------------------------------------------------------------------
// The following methods are used for the BOTTOM Text Drop SOURCE and TARGET examples.
// The DropSource code is almost identical. However, the Edit2 control
// has been hooked to override the default WM_LBUTTONDOWN message handling.
// Using the normal OnMouseDown event method does not work for this example.
//----------------------------------------------------------------------------

// The new WindowProc for Edit2 which intercepts WM_LBUTTONDOWN messages
// before ANY OTHER processing...
procedure TFormText.NewEdit2WindowProc(var Msg : TMessage);
begin
  if (Msg.Msg = WM_LBUTTONDOWN) and
    MouseIsOverEdit2Selection(TWMMouse(Msg).XPos) then
  begin
    StartEdit2Drag; // Just a locally declared procedure.
    TWMMouse(Msg).Result := 0;
  end else
    //Otherwise do everything as before...
    OldEdit2WindowProc(Msg);
end;

// Determine if the specified X coordinate is within the edit control's
// text selection.
function TFormText.MouseIsOverEdit2Selection(XPos: integer): boolean;
var
  dc: HDC;
  SavedFont: HFont;
  size1, size2: TSize;
  s1, s2: string;
begin
  if (Edit2.SelLength > 0) and (Edit2.Focused) then
  begin
    // Create a Device Context which can be used to retrieve font metrics.
    dc := GetDC(0);
    try
      // Select the edit control's font into our DC.
      SavedFont := SelectObject(dc, Edit2.Font.Handle);
      try
        // Get text before selection.
        s1 := Copy(Edit2.Text, 1, Edit2.SelStart);
        // Get text up to and including selection.
        s2 := s1 + Edit2.SelText;
        // Get dimensions of text before selection and up to and including
        // selection.
        GetTextExtentPoint32(dc, PChar(s1), length(s1), size1);
        GetTextExtentPoint32(dc, PChar(s2), length(s2), size2);
      finally
        SelectObject(dc, SavedFont);
      end;
    finally
      ReleaseDC(0,dc);
    end;
    Result := (XPos >= size1.cx) and (XPos <= size2.cx);
  end else
    Result := False;
end;

procedure TFormText.StartEdit2Drag;
var
  Res: TDragResult;
begin
  Statusbar1.Simpletext := '';

  // Copy the data into the drop source and...
  DropSource1.Text := Edit2.SelText;

  // ...Start the drag/drop.
  Res := DropSource1.Execute;

  // Display the result of the drag operation.
  case Res of
    drDropCopy: Statusbar1.SimpleText := 'Copied successfully';
    drDropLink: Statusbar1.SimpleText := 'Scrap file created successfully';
    drCancel: Statusbar1.SimpleText := 'Drop cancelled';
    drOutMemory: Statusbar1.SimpleText := 'Drop failed - out of memory';
  else
    Statusbar1.SimpleText := 'Drop failed - reason unknown';
  end;
end;

procedure TFormText.Edit2MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  // This method just changes mouse cursor to crHandPoint if over selected text.
  if MouseIsOverEdit2Selection(X) then
    Edit2.Cursor := crHandPoint
  else
    Edit2.Cursor := crDefault;
end;

procedure TFormText.DropTextTarget2Drop(Sender: TObject;
  ShiftState: TShiftState; Point: TPoint; var Effect: Integer);
begin
  // Text has been dropped onto our drop target. Copy the dropped text into the
  // edit control.
  Edit2.Text := DropTextTarget2.Text;
end;

end.
