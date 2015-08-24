unit Main;

interface

{$include dragdrop.inc} // Disables .NET warnings

uses
  DragDrop, DropSource, DropTarget, DragDropFile, ActiveX,
  Windows, Classes, Controls, Forms, ExtCtrls, StdCtrls, ComCtrls, Menus,
  ActnList;

type
////////////////////////////////////////////////////////////////////////////////
//
//		TFormMain
//
////////////////////////////////////////////////////////////////////////////////
  TFormMain = class(TForm)
    DropDummy1: TDropDummy;
    Panel1: TPanel;
    ListView1: TListView;
    Panel2: TPanel;
    DropEmptySource1: TDropEmptySource;
    DropEmptyTarget1: TDropEmptyTarget;
    DataFormatAdapterSource: TDataFormatAdapter;
    DataFormatAdapterTarget: TDataFormatAdapter;
    PopupMenu1: TPopupMenu;
    MenuCopy: TMenuItem;
    MenuPaste: TMenuItem;
    Label1: TLabel;
    ActionList1: TActionList;
    ActionCopy: TAction;
    ActionPaste: TAction;
    procedure OnMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure DropEmptyTarget1Drop(Sender: TObject; ShiftState: TShiftState;
      Point: TPoint; var Effect: Integer);
    procedure DropEmptyTarget1Enter(Sender: TObject;
      ShiftState: TShiftState; Point: TPoint; var Effect: Integer);
    procedure DropEmptySource1AfterDrop(Sender: TObject;
      DragResult: TDragResult; Optimized: Boolean);
    procedure FormDestroy(Sender: TObject);
    procedure ListView1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure ActionCopyExecute(Sender: TObject);
    procedure ActionPasteExecute(Sender: TObject);
    procedure ActionPasteUpdate(Sender: TObject);
    procedure ActionCopyUpdate(Sender: TObject);
  private
    procedure OnGetStream(Sender: TFileContentsStreamOnDemandClipboardFormat;
      Index: integer; out AStream: IStream);
  public
  end;

var
  FormMain: TFormMain;

implementation

{$R *.DFM}

uses
  DragDropFormats, 
  ComObj;

////////////////////////////////////////////////////////////////////////////////
//
//		TFormMain
//
////////////////////////////////////////////////////////////////////////////////
procedure TFormMain.FormCreate(Sender: TObject);
begin
  // Setup event handler to let a drop target request data from our drop source.
  (DataFormatAdapterSource.DataFormat as TVirtualFileStreamDataFormat).OnGetStream := OnGetStream;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  // Need to freeze clipboard content before form is destroyed because we use
  // the content of the listview when supplying the data.
  DropEmptySource1.FlushClipboard;
end;

procedure TFormMain.ListView1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if (ListView1.GetHitTestInfoAt(X, Y) * [htOnItem, htOnIcon, htOnLabel, htOnStateIcon] <> []) then
    Screen.Cursor := crHandPoint
  else
    Screen.Cursor := crDefault;
end;

procedure TFormMain.OnMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  i: integer;
begin
  if (ListView1.SelCount > 0) and
    (ListView1.GetHitTestInfoAt(X, Y) * [htOnItem, htOnIcon, htOnLabel, htOnStateIcon] <> []) and
    (DragDetectPlus(ListView1.Handle, Point(X,Y))) then
  begin
    // Transfer the file names to the data format. The content will be extracted
    // by the target on-demand.
    TVirtualFileStreamDataFormat(DataFormatAdapterSource.DataFormat).FileNames.Clear;
    for i := 0 to ListView1.Items.Count-1 do
      if (ListView1.Items[i].Selected) then
        TVirtualFileStreamDataFormat(DataFormatAdapterSource.DataFormat).
          FileNames.Add(ListView1.Items[i].Caption);

    // ...and let it rip!
    DropEmptySource1.Execute;
  end;
end;

procedure TFormMain.DropEmptyTarget1Drop(Sender: TObject;
  ShiftState: TShiftState; Point: TPoint; var Effect: Integer);
type
  PLargeint = ^Largeint;
var
  OldCount: integer;
  Item: TListItem;
  Buffer: AnsiString;
  p: PAnsiChar;
  i: integer;
  Stream: IStream;
  StatStg: TStatStg;
  Total, BufferSize, Chunk, Size: longInt;
  FirstChunk: boolean;
const
  MaxBufferSize = 32*1024; // 32Kb
begin
  // Transfer the file names and contents from the data format.
  if (TVirtualFileStreamDataFormat(DataFormatAdapterTarget.DataFormat).FileNames.Count > 0) then
  begin
    ListView1.Items.BeginUpdate;
    try
      // Note: Since we can actually drag and drop from and onto ourself, we
      // can't clear the ListView until the data has been read from the listview
      // (by the source) and inserted into it again (by the target). To
      // accomplish this, we add the dropped items to the list first and then
      // delete the old items afterwards.
      // Another, and more common, approach would be to reject or disable drops
      // onto ourself while we are performing drag/drop operations.
      OldCount := ListView1.Items.Count;
      for i := 0 to TVirtualFileStreamDataFormat(DataFormatAdapterTarget.DataFormat).FileNames.Count-1 do
      begin
        Item := ListView1.Items.Add;
        Item.Caption := TVirtualFileStreamDataFormat(DataFormatAdapterTarget.DataFormat).FileNames[i];

        // Get data stream from source.
        Stream := TVirtualFileStreamDataFormat(DataFormatAdapterTarget.DataFormat).FileContentsClipboardFormat.GetStream(i);
        if (Stream <> nil) then
        begin
          // Read data from stream.
          Stream.Stat(StatStg, STATFLAG_NONAME);
          Total := StatStg.cbSize;

          // Assume that stream is at EOF, so set it to BOF.
          // See comment in TCustomSimpleClipboardFormat.DoSetData (in
          // DragDropFormats.pas) for an explanation of this.
          Stream.Seek(0, STREAM_SEEK_SET, PLargeint(nil)^);

          // If a really big hunk of data has been dropped on us we display a
          // small part of it since there isn't much point in trying to display
          // it all in the limited space we have available.
          // Additionally, it would be *really* bad for performce if we tried to
          // allocated a too big buffer and read sequentially into it. Tests has
          // shown that allocating a 10Mb buffer and trying to read data into it
          // in 1Kb chunks takes several minutes, while the same data can be
          // read into a 32Kb buffer in 1Kb chunks in seconds. The Windows
          // explorer uses a 1 Mb buffer, but that's too big for this demo.
          // The above tests were performed using the AsyncSource demo.
          BufferSize := Total;
          if (BufferSize > MaxBufferSize) then
            BufferSize := MaxBufferSize;

          SetLength(Buffer, BufferSize);
          p := PAnsiChar(Buffer);
          Chunk := BufferSize;
          FirstChunk := True;
          while (Total > 0) do
          begin
            Stream.Read(p, Chunk, @Size);
            if (Size = 0) then
              break;

            inc(p, Size);
            dec(Total, Size);
            dec(Chunk, Size);

            if (Chunk = 0) or (Total = 0) then
            begin
              // Display a small fraction of the first chunk.
              if (FirstChunk) then
                Item.SubItems.Add(Copy(Buffer, 1, 1024));

              p := PAnsiChar(Buffer);
              // In a real-world application we would write the buffer to disk
              // now. E.g.:
              //   FileStream.WriteBuffer(p^, BufferSize-Chunk);
              Chunk := BufferSize;
              FirstChunk := False;
            end;
          end;
          // Display a small fraction of the first chunk.
          if (FirstChunk) then
            Item.SubItems.Add(Copy(Buffer, 1, 1024));

        end else
          Item.SubItems.Add('***failed to read content***');
      end;
      // Delete the old items.
      for i := OldCount-1 downto 0 do
        ListView1.Items.Delete(i);
    finally
      ListView1.Items.EndUpdate;
    end;
  end;
end;

procedure TFormMain.DropEmptyTarget1Enter(Sender: TObject;
  ShiftState: TShiftState; Point: TPoint; var Effect: Integer);
begin
  // Reject the drop unless the source supports *both* the FileContents and
  // FileGroupDescriptor formats in the storage medium we require (IStream).
  // Normally a drop is accepted if just one of our formats is supported.
  with TVirtualFileStreamDataFormat(DataFormatAdapterTarget.DataFormat) do
    if not(FileContentsClipboardFormat.HasValidFormats(DropEmptyTarget1.DataObject) and
      (AnsiFileGroupDescriptorClipboardFormat.HasValidFormats(DropEmptyTarget1.DataObject) or
       UnicodeFileGroupDescriptorClipboardFormat.HasValidFormats(DropEmptyTarget1.DataObject))) then
      Effect := DROPEFFECT_NONE;
end;

procedure TFormMain.ActionCopyExecute(Sender: TObject);
var
  i: integer;
begin
  // Transfer the file names to the data format. The content will be extracted
  // by the target on-demand.
  TVirtualFileStreamDataFormat(DataFormatAdapterSource.DataFormat).FileNames.Clear;
  for i := 0 to ListView1.Items.Count-1 do
    if (ListView1.Items[i].Selected) then
      TVirtualFileStreamDataFormat(DataFormatAdapterSource.DataFormat).
        FileNames.Add(ListView1.Items[i].Caption);

  // ...and copy data to clipboard.
  DropEmptySource1.CopyToClipboard;

  (*
  ** Note:
  ** If DropEmptySource1.FlushClipboard (OleFlushClipboard) is called now or
  ** the application is terminated, the clipboard will be unable to supply the
  ** file contents data and only empty files will be pasted.
  ** This is believed to be a bug in the Windows clipboard.
  *)
end;

procedure TFormMain.ActionCopyUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (ListView1.SelCount > 0);
end;

procedure TFormMain.ActionPasteExecute(Sender: TObject);
begin
  DropEmptyTarget1.PasteFromClipboard;
end;

procedure TFormMain.ActionPasteUpdate(Sender: TObject);
(*
var
  DataObject: IDataObject;
*)
begin
  (*
  ** The following shows two diffent methods of determining if the clipboard
  ** contains any data that we can paste. The two methods are in fact identical.
  ** Only the second method is used.
  *)

  (*
  ** Method 1: Get the clipboard data object and test against it.
  *)
  (*
  // Open the clipboard as an IDataObject
  OleCheck(OleGetClipboard(DataObject));
  try
    // Enable paste menu if the clipboard contains data in any of
    // the supported formats.
    MenuPaste.Enabled := DropEmptyTarget1.HasValidFormats(DataObject);
  finally
    DataObject := nil;
  end;
  *)

  (*
  ** Method 2: Let the component do it for us.
  *)
  MenuPaste.Enabled := DropEmptyTarget1.CanPasteFromClipboard;
end;

procedure TFormMain.DropEmptySource1AfterDrop(Sender: TObject;
  DragResult: TDragResult; Optimized: Boolean);
begin
  // Clear the listview if items were moved.
  // Note: If we drag-move from and drop onto ourself, this would cause the
  // listview to clear after we have successfully transfered the data. To avoid
  // this (and to avoid files being accidentally deleted), our drop target
  // doesn't accept move operations. If you want it to be able to accept move
  // operations, you'll have to avoid the above situation somehow. I'll leave it
  // up to you to figure out how to do that.
  if (DragResult = drDropMove) then
    ListView1.Items.Clear;
end;

procedure TFormMain.OnGetStream(Sender: TFileContentsStreamOnDemandClipboardFormat;
  Index: integer; out AStream: IStream);
var
  Stream: TMemoryStream;
  Data: AnsiString;
  i: integer;
  SelIndex: integer;
  Found: boolean;
begin
  // This event handler is called by TFileContentsStreamOnDemandClipboardFormat
  // when the drop target requests data from the drop source (that's us).
  Stream := TMemoryStream.Create;
  try
    AStream := nil;
    // Find the listview item which corresponds to the requested data item.
    SelIndex := 0;
    Found := False;
    for i := 0 to ListView1.Items.Count-1 do
      if (ListView1.Items[i].Selected) then
      begin
        if (SelIndex = Index) then
        begin
          // Get the data stored in the listview item and...
          Data := ListView1.Items[i].SubItems[0];
          Found := True;
          break;
        end;
        inc(SelIndex);
      end;
    if (not Found) then
      exit;

    // ...Write the file contents to a regular stream...
    Stream.Write(PAnsiChar(Data)^, Length(Data));

    (*
    ** Stream.Position must be equal to Stream.Size or the Windows clipboard
    ** will fail to read from the stream. This requirement is completely
    ** undocumented.
    *)
    // Stream.Position := 0;

    // ...and return the stream back to the target as an IStream. Note that the
    // target is responsible for deleting the stream (via reference counting).
    AStream := TFixedStreamAdapter.Create(Stream, soOwned);
  except
    Stream.Free;
    raise;
  end;
end;

end.

