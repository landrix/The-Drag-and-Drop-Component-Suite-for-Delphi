unit DropFile;

interface

{$include DragDrop.inc}

uses
  DragDrop,
  DropTarget,
  DropSource,
  DragDropFile,
  ImgList,
  ComCtrls, ActiveX, ShlObj, ComObj, Controls, ShellCtrls, Menus, ExtCtrls,
  StdCtrls, Classes, Forms, Graphics, Windows;

type
  // Hack to avoid having to install the design time shell controls
  TTreeView = class(TShellTreeView);

  TListView = class(TShellListView)
  published
    property Columns stored False;
  end;

  TFormFile = class(TForm)
    Memo1: TMemo;
    btnClose: TButton;
    StatusBar1: TStatusBar;
    DropFileTarget1: TDropFileTarget;
    Panel1: TPanel;
    DropSource1: TDropFileSource;
    ImageListMultiFile: TImageList;
    DropDummy1: TDropDummy;
    PopupMenu1: TPopupMenu;
    MenuCopy: TMenuItem;
    MenuCut: TMenuItem;
    N1: TMenuItem;
    MenuPaste: TMenuItem;
    ImageListSingleFile: TImageList;
    ShellTreeView1: TTreeView;
    ListView1: TListView;
    procedure MenuCutOrCopyClick(Sender: TObject);
    procedure ListView1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure DropSource1Feedback(Sender: TObject; Effect: Integer;
      var UseDefaultCursors: Boolean);
    procedure DropFileTarget1Enter(Sender: TObject;
      ShiftState: TShiftState; Point: TPoint; var Effect: Integer);
    procedure DropFileTarget1Drop(Sender: TObject; ShiftState: TShiftState;
      Point: TPoint; var Effect: Integer);
    procedure DropFileTarget1GetDropEffect(Sender: TObject;
      ShiftState: TShiftState; Point: TPoint; var Effect: Integer);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure MenuPasteClick(Sender: TObject);
    procedure DropSource1Paste(Sender: TObject; Action: TDragResult;
      DeleteOnPaste: Boolean);
    procedure DropSource1AfterDrop(Sender: TObject;
      DragResult: TDragResult; Optimized: Boolean);
    procedure ListView1CustomDrawItem(Sender: TCustomListView;
      Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FTargetPath: string;
    FIsEXEfile: boolean;
    function GetSourcePath: string;
  protected
    procedure Loaded; override;
  public
    property TargetPath: string read FTargetPath;
    property SourcePath: string read GetSourcePath;
    property IsEXEfile: boolean read FIsEXEfile;
  end;

var
  FormFile: TFormFile;

implementation

{$R *.DFM}

uses
{$IF CompilerVersion >= 10.0}
  Types, // Required for inlining of ListView_CreateDragImage
{$endif}
  SysUtils,
  CommCtrl;

// CUSTOM CURSORS:
// The cursors in DropCursors.res are exactly the same as the default cursors.
// Use DropCursors.res as a template if you wish to customise your own cursors.
// For this demo we've created Cursors.res - some coloured cursors.
{$R Cursors.res}
const
  crCopy = 101;
  crMove = 102;
  crLink = 103;
  crCopyScroll = 104;
  crMoveScroll = 105;
  crLinkScroll = 106;

//----------------------------------------------------------------------------
// Miscellaneous utility functions
//----------------------------------------------------------------------------

procedure CreateLink(SourceFile, ShortCutName: String);
var
  IUnk: IUnknown;
  ShellLink: IShellLink;
  IPFile: IPersistFile;
  tmpShortCutName: string;
  WideStr: WideString;
  i: integer;
begin
  IUnk := CreateComObject(CLSID_ShellLink);
  ShellLink := IUnk as IShellLink;
  IPFile  := IUnk as IPersistFile;
  with ShellLink do
  begin
    SetPath(PChar(SourceFile));
    SetWorkingDirectory(PChar(ExtractFilePath(SourceFile)));
  end;
  ShortCutName := ChangeFileExt(ShortCutName,'.lnk');
  if FileExists(ShortCutName) then
  begin
    ShortCutName := copy(ShortCutName, 1, length(ShortCutName)-4);
    i := 1;
    repeat
      tmpShortCutName := ShortCutName +'(' + inttostr(i)+ ').lnk';
      inc(i);
    until not FileExists(tmpShortCutName);
    WideStr := tmpShortCutName;
  end
  else WideStr := ShortCutName;
  IPFile.Save(PWChar(WideStr), False);
end;

//----------------------------------------------------------------------------
// TFormFile methods
//----------------------------------------------------------------------------

procedure TFormFile.Loaded;
begin
  inherited Loaded;

  with ShellTreeView1 do
  begin
    AutoContextMenus := False;
    ObjectTypes := [otFolders];
    Root := 'rfDesktop';
    ShellListView := ListView1;
    UseShellImages := True;
    AutoRefresh := False;
  end;

  with ListView1 do
  begin
    AutoContextMenus := False;
    ObjectTypes := [otNonFolders];
    Root := 'rfDesktop';
    ShellTreeView := ShellTreeView1;
    Sorted := True;
  end;

  ShellTreeView1.Path := ExtractFilePath(Application.ExeName);
end;

procedure TFormFile.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  // Work around "Control has no parent window" on destroy
  ListView1.Free;
  ShellTreeView1.Free;
end;

procedure TFormFile.FormCreate(Sender: TObject);
begin
  // Load custom cursors...
  Screen.cursors[crCopy] := LoadCursor(hinstance, 'CUR_DRAG_COPY');
  Screen.cursors[crMove] := LoadCursor(hinstance, 'CUR_DRAG_MOVE');
  Screen.cursors[crLink] := LoadCursor(hinstance, 'CUR_DRAG_LINK');
  Screen.cursors[crCopyScroll] := LoadCursor(hinstance, 'CUR_DRAG_COPY_SCROLL');
  Screen.cursors[crMoveScroll] := LoadCursor(hinstance, 'CUR_DRAG_MOVE_SCROLL');
  Screen.cursors[crLinkScroll] := LoadCursor(hinstance, 'CUR_DRAG_LINK_SCROLL');
end;

function TFormFile.GetSourcePath: string;
begin
  Result := IncludeTrailingBackslash(ShellTreeView1.Path);
end;

procedure TFormFile.ListView1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  i: integer;
  Filename: string;
  Res: TDragResult;
  p: TPoint;
  First: boolean;
//  Count: integer;
begin
  // If no files selected then exit...
  if ListView1.SelCount = 0 then
    Exit;

  // Wait for user to move cursor before we start the drag/drop.
  if (DragDetectPlus(TWinControl(Sender))) then
  begin
    Statusbar1.SimpleText := '';
    DropSource1.Files.Clear;
    // DropSource1.MappedNames.Clear;

    // Fill DropSource1.Files with selected files in ListView1 and...
    // ...create drag image
    First := True;
    for i := 0 to Listview1.Items.Count-1 do
      if (Listview1.Items[i].Selected) then
      begin
        Filename := Listview1.Folders[i].PathName;
        DropSource1.Files.Add(Filename);
        // The TDropFileSource.MappedNames list can be used to indicate to the
        // drop target, that the files should be renamed once they have been
        // copied. This is the technique used when dragging files from the
        // recycle bin.
        // DropSource1.MappedNames.Add(ExtractFilePath(Filename)+'Copy of '+ExtractFileName(Filename));

        // Create a drag image.
        if (First) then
        begin
          ImageListSingleFile.Handle := ListView_CreateDragImage(ListView1.Handle,
            Listview1.Items[i].Index, p);
          // Note: ListView_CreateDragImage fails to include the list item text on
          // some versions of Windows. Known problem with no solution according to MS.
          First := False;
        end else
        begin
          ImageListMultiFile.Handle := ListView_CreateDragImage(ListView1.Handle,
            Listview1.Items[i].Index, p);
          try
            ImageListSingleFile.Handle := ImageList_Merge(ImageListSingleFile.Handle, 0, ImageListMultiFile.Handle, 0, 0, ImageListSingleFile.Height);
          finally
            ImageListMultiFile.Handle := 0;
          end;
        end;
      end;

    DropSource1.Images := ImageListSingleFile;
    DropSource1.ImageHotSpotX := X-ListView1.Selected.Left;
    DropSource1.ImageHotSpotY := Y-ListView1.Selected.Top;
    DropSource1.ImageIndex := 0;


    // Temporarily disable the list view as a drop target so we don't drop on
    // ourself.
    DropFileTarget1.Dragtypes := [];
    try

      // OK, now we are all set to go. Let's start the drag...
      Res := DropSource1.Execute;

    finally
      // Enable the list view as a drop target again.
      DropFileTarget1.Dragtypes := [dtCopy,dtMove,dtLink];
    end;

    // Note:
    // The target is responsible, from this point on, for the
    // copying/moving/linking of the file but the target feeds
    // back to the source what (should have) happened via the
    // returned value of Execute.

    // Feedback in Statusbar1 what happened...
    case Res of
      drDropCopy: StatusBar1.SimpleText := 'Copied successfully';
      drDropMove: StatusBar1.SimpleText := 'Moved successfully';
      drDropLink: StatusBar1.SimpleText := 'Linked successfully';
      drCancel: StatusBar1.SimpleText := 'Drop was cancelled';
      drOutMemory: StatusBar1.SimpleText := 'Drop cancelled - out of memory';
    else
      StatusBar1.SimpleText := 'Drop cancelled - unknown reason';
    end;

  end;
end;

procedure TFormFile.ListView1CustomDrawItem(Sender: TCustomListView;
  Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
begin
  // Items which have been "cut to clipboard" are drawn differently.
  if boolean(Item.Data) then
    Sender.Canvas.Font.Style := [fsStrikeOut];
end;

procedure TFormFile.PopupMenu1Popup(Sender: TObject);
begin
  MenuCopy.Enabled := (Listview1.SelCount > 0);
  MenuCut.Enabled := MenuCopy.Enabled;

  // Enable paste menu if the clipboard contains data in any of
  // the supported formats
  MenuPaste.Enabled := DropFileTarget1.CanPasteFromClipboard;
end;

// Demonstrates CopyToClipboard and CutToClipboard methods.
procedure TFormFile.MenuCutOrCopyClick(Sender: TObject);
var
  i: integer;
  Filename: string;
  Status: boolean;
  Operation: string;
begin
  if Listview1.SelCount = 0 then
  begin
    StatusBar1.SimpleText := 'No files have been selected!';
    exit;
  end;

  DropSource1.Files.clear;
  for i := 0 to Listview1.Items.Count-1 do
    if (Listview1.Items[i].Selected) then
    begin
      Filename := Listview1.Folders[i].PathName;
      DropSource1.Files.Add(Filename);

      // Flag item as "cut" so it can be drawn differently.
      if (Sender = MenuCut) then
        Listview1.items.Item[i].Data := pointer(True)
      else
        Listview1.items.Item[i].Data := pointer(False);
    end else
      Listview1.items.Item[i].Data := pointer(False);

  Listview1.Invalidate;

  // Transfer data to clipboard.
  if (Sender = MenuCopy) then
  begin
    Status := DropSource1.CopyToClipboard;
    Operation := 'copied';
  end else
  if (Sender = MenuCut) then
  begin
    Status := DropSource1.CutToClipboard;
    Operation := 'cut';
  end else
    Status := False;

  if (Status) then
    StatusBar1.SimpleText :=
      Format('%d file(s) %s to clipboard.',[DropSource1.Files.Count, Operation]);
end;

procedure TFormFile.MenuPasteClick(Sender: TObject);
begin
  // PasteFromClipboard fires an OnDrop event, so we don't need to do
  // anything special here.
  DropFileTarget1.PasteFromClipboard;
end;

//--------------------------
// SOURCE events...
//--------------------------
procedure TFormFile.DropSource1Feedback(Sender: TObject; Effect: Integer;
  var UseDefaultCursors: Boolean);
begin
  UseDefaultCursors := False; // We want to use our own.
  case DWORD(Effect) of
    DROPEFFECT_COPY:
      Windows.SetCursor(Screen.Cursors[crCopy]);
    DROPEFFECT_MOVE:
      Windows.SetCursor(Screen.Cursors[crMove]);
    DROPEFFECT_LINK:
      Windows.SetCursor(Screen.Cursors[crLink]);
    DROPEFFECT_SCROLL OR DROPEFFECT_COPY:
      Windows.SetCursor(Screen.Cursors[crCopyScroll]);
    DROPEFFECT_SCROLL OR DROPEFFECT_MOVE:
      Windows.SetCursor(Screen.Cursors[crMoveScroll]);
    DROPEFFECT_SCROLL OR DROPEFFECT_LINK:
      Windows.SetCursor(Screen.Cursors[crLinkScroll]);
  else
    UseDefaultCursors := True; // Use default NoDrop
  end;
end;

procedure TFormFile.DropSource1AfterDrop(Sender: TObject;
  DragResult: TDragResult; Optimized: Boolean);
var
  i: integer;
begin
  // Delete source files if target performed an unoptimized drag/move
  // operation (target copies files, source deletes them).
  if (DragResult = drDropMove) and (not Optimized) then
    for i := 0 to DropSource1.Files.Count-1 do
      DeleteFile(DropSource1.Files[i]);
end;

procedure TFormFile.DropSource1Paste(Sender: TObject; Action: TDragResult;
  DeleteOnPaste: Boolean);
var
  i: integer;
begin
  StatusBar1.SimpleText := 'Target pasted file(s)';

  // Delete source files if target performed a paste/move operation and
  // requested the source to "Delete on paste".
  if (DeleteOnPaste) then
    for i := 0 to DropSource1.Files.Count-1 do
      DeleteFile(DropSource1.Files[i]);
end;


//--------------------------
// TARGET events...
//--------------------------
procedure TFormFile.DropFileTarget1Enter(Sender: TObject;
  ShiftState: TShiftState; Point: TPoint; var Effect: Integer);
begin
  // Note: GetDataOnEnter has been set to True.
  // A side effect of this is that TDropFileTarget can't be used to accept drops
  // from WinZip. Although the file names are received correctly, the files
  // aren't extracted yet and thus can't be copied/moved.
  // This is caused by a quirk in WinZip; Apparently WinZip doesn't like
  // IDataObject.GetData to be called before IDropTarget.Drop is called.

  // Save the location (path) of the files being dragged.
  // Also flag if an EXE file is being dragged.
  // This info will be used to set the default (ie. no Shift or Ctrl Keys
  // pressed) drag behaviour (COPY, MOVE or LINK).
  if (DropFileTarget1.Files.count > 0) then
  begin
    FTargetPath := ExtractFilePath(DropFileTarget1.Files[0]);
    FIsEXEfile := (DropFileTarget1.Files.count = 1) and
      (AnsiCompareText(ExtractFileExt(DropFileTarget1.Files[0]), '.exe') = 0);
  end;
end;

procedure TFormFile.DropFileTarget1Drop(Sender: TObject;
  ShiftState: TShiftState; Point: TPoint; var Effect: Integer);
var
  i, SuccessCnt: integer;
  NewFilename: string;
begin
  SuccessCnt := 0;

  // Filter out the DROPEFFECT_SCROLL flag if set...
  // (ie: when dropping a file while the target window is scrolling)
  Effect := Effect and not DROPEFFECT_SCROLL;
  // Now, 'Effect' should equal one of the following:
  // DROPEFFECT_COPY, DROPEFFECT_MOVE or DROPEFFECT_LINK.
  // Note however, that if we call TDropTarget.PasteFromClipboard, Effect
  // can be a combination of the above drop effects.

  for i := 0 to DropFileTarget1.Files.count-1 do
  begin
    // Name mapping occurs when dragging files from Recycle Bin...
    // In most situations Name Mapping can be ignored entirely.
    if (i < DropFileTarget1.MappedNames.Count) then
      NewFilename := SourcePath+DropFileTarget1.MappedNames[i]
    else
      NewFilename := SourcePath+ExtractFilename(DropFileTarget1.Files[i]);

    if not FileExists(NewFilename) then
    begin
      if NewFilename = DropFileTarget1.Files[i] then
      begin
        Windows.MessageBox(Handle,
          'The destination folder is the same as the source!',
          'Drag/Drop Demo', mb_iconStop or mb_OK);
        Break;
      end;

      try
        if (Effect and DROPEFFECT_COPY <> 0) then
        begin
          Effect := DROPEFFECT_COPY;
          // Copy the file.
          if CopyFile(PChar(DropFileTarget1.Files[i]), PChar(NewFilename), True) then
            inc(SuccessCnt);
        end else
        if (Effect and DROPEFFECT_MOVE <> 0) then
        begin
          Effect := DROPEFFECT_MOVE;
          // Move the file.
          if RenameFile(DropFileTarget1.Files[i], NewFilename) then
            inc(SuccessCnt)
        end;
      except
        // Ignore errors.
      end;
    end;

    if (Effect and DROPEFFECT_LINK <> 0) then
    begin
      Effect := DROPEFFECT_LINK;
      // Create a shell link to the file.
      CreateLink(DropFileTarget1.Files[i], NewFilename);
      inc(SuccessCnt);
    end;

  end;

  if (Effect = DROPEFFECT_MOVE) then
    StatusBar1.SimpleText :=
      Format('%d file(s) were moved.   Files dropped at point (%d,%d).',
        [SuccessCnt, Point.x, Point.y])
  else if (Effect = DROPEFFECT_COPY) then
    StatusBar1.SimpleText :=
      Format('%d file(s) were copied.   Files dropped at point (%d,%d).',
        [SuccessCnt, Point.x, Point.y])
  else
    StatusBar1.SimpleText :=
      Format('%d file(s) were linked.   Files dropped at point (%d,%d).',
        [SuccessCnt, Point.x, Point.y]);
end;

procedure TFormFile.DropFileTarget1GetDropEffect(Sender: TObject;
  ShiftState: TShiftState; Point: TPoint; var Effect: Integer);
var
  Scroll: DWORD;
begin
  // Note: The 'Effect' parameter (on event entry) is the
  // set of effects allowed by both the source and target.
  // Use this event when you wish to override the Default behaviour...

  // Save the value of the auto scroll flag.
  // As an alternative we could implement our own auto scroll logic here.
  Scroll := DWORD(Effect) and DROPEFFECT_SCROLL;

  // We're only interested in ssShift & ssCtrl here so
  // mouse buttons states are screened out ...
  ShiftState := ([ssShift, ssCtrl] * ShiftState);

  // Reject the drop if source and target paths are the same (DROPEFFECT_NONE).
  if (SourcePath = TargetPath) then
    Effect := DROPEFFECT_NONE
  // else if Ctrl+Shift are pressed then create a link (DROPEFFECT_LINK).
  else if (ShiftState = [ssShift, ssCtrl]) and
    (Effect and DROPEFFECT_LINK <> 0) then Effect := DROPEFFECT_LINK
  // else if Shift is pressed then move (DROPEFFECT_MOVE).
  else if (ShiftState = [ssShift]) and
    (Effect and DROPEFFECT_MOVE<>0) then Effect := DROPEFFECT_MOVE
  // else if Ctrl is pressed then copy (DROPEFFECT_COPY).
  else if (ShiftState = [ssCtrl]) and
    (Effect and DROPEFFECT_COPY<>0) then Effect := DROPEFFECT_COPY
  // else if dragging a single EXE file then default to link (DROPEFFECT_LINK).
  else if IsEXEfile and (Effect and DROPEFFECT_LINK<>0) then
    Effect := DROPEFFECT_LINK
  // else if source and target drives are the same then default to MOVE (DROPEFFECT_MOVE).
  else if (TargetPath <> '') and (ExtractFileDrive(SourcePath) = ExtractFileDrive(TargetPath)) and
    (Effect and DROPEFFECT_MOVE<>0) then Effect := DROPEFFECT_MOVE
  // otherwise just use whatever we can get away with.
  else if (Effect and DROPEFFECT_COPY<>0) then Effect := DROPEFFECT_COPY
  else if (Effect and DROPEFFECT_MOVE<>0) then Effect := DROPEFFECT_MOVE
  else if (Effect and DROPEFFECT_LINK<>0) then Effect := DROPEFFECT_LINK
  else Effect := DROPEFFECT_NONE;

  // Restore auto scroll flag.
  Effect := Effect or integer(Scroll);
end;

end.
