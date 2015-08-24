unit Unit1;

interface

{$include dragdrop.inc} // Disables .NET warnings

uses
{$IF CompilerVersion >= 10.0}
  Types, // Required for inlining of ListView_CreateDragImage
{$endif}
  DragDrop,
  DropSource,
  DropTarget,
  DragDropPIDL,
  PathComboBox,
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, ActiveX, ShellApi, ShlObj, Buttons, ExtCtrls,StdCtrls, CommCtrl;

type

  TForm1 = class(TForm)
    ListView1: TListView;
    Panel1: TPanel;
    DropPIDLSource1: TDropPIDLSource;
    Button1: TButton;
    StatusBar1: TStatusBar;
    Label1: TLabel;
    DropPIDLTarget1: TDropPIDLTarget;
    sbUpLevel: TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ListView1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Button1Click(Sender: TObject);
    procedure ListView1DblClick(Sender: TObject);
    procedure ListView1KeyPress(Sender: TObject; var Key: Char);
    procedure DropPIDLTarget1Drop(Sender: TObject; ShiftState: TShiftState;
      Point: TPoint; var Effect: Integer);
    procedure sbUpLevelClick(Sender: TObject);
    procedure DropPIDLTarget1DragOver(Sender: TObject;
      ShiftState: TShiftState; Point: TPoint; var Effect: Integer);
    procedure ListView1Deletion(Sender: TObject; Item: TListItem);
  private
    CurrentShellFolder: IShellFolder;
    CurrentFolderImageIndex: integer;
    FImageList: TImageList;
    FRecyclePIDL: pItemIdList;
    FIsDragging: boolean;

    //custom component NOT installed in the IDE...
    PathComboBox: TPathComboBox;
    procedure PathComboBoxChange(Sender: TObject);

    procedure SetCurrentFolder;
    procedure RefreshListNames;
    procedure PopulateListview;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;


implementation

{$R *.DFM}

//---------------------------------------------------------------------
// TLVItemData class
// (objects used to store extra data with each Listview item)
//---------------------------------------------------------------------

type
  TLVItemData = class
    SortStr: string; {just used to sort the listview}
    RelativePIDL: pItemIDList; {each item stores its own PIDLs}
    AbsolutePIDL: pItemIDList;
  public
    destructor Destroy; override;
  end;


destructor TLVItemData.Destroy;
begin
  //nb: ShellMalloc interface declared and assigned in DropSource.pas
  ShellMalloc.Free(RelativePIDL);
  ShellMalloc.Free(AbsolutePIDL);
  inherited;
end;

//---------------------------------------------------------------------
// Local functions ...
//---------------------------------------------------------------------

//Used to sort the listview...
function ListviewSort(Item1, Item2: TListItem;
  lParam: Integer): Integer; stdcall;
Begin
  if (Item1<>nil) and (Item2<>nil) and (Item1<>Item2) then
    Result := CompareText(TLVItemData(Item1.Data).SortStr, TLVItemData(Item2.Data).SortStr)
  else
    Result :=0;
End;


//---------------------------------------------------------------------
// TForm1 class ...
//---------------------------------------------------------------------

procedure TForm1.FormCreate(Sender: TObject);
var
  sfi: TShFileInfo;
begin
  //get access to the shell imagelist...
  FImageList := TImageList.create(self);
  FImageList.Handle :=
    shGetFileInfo('', 0, sfi, SizeOf(TshFileInfo), shgfi_sysiconindex or shgfi_smallicon);
  FImageList.shareimages := true;
  FImageList.BlendColor := clHighlight;
  Listview1.SmallImages := FImageList;

  //Create our custom component...
  PathComboBox := TPathComboBox.create(self);
  PathComboBox.parent := self;
  PathComboBox.top := 35;
  PathComboBox.left := 2;
  PathComboBox.width := 434;
  PathComboBox.ShowVirtualFolders := true;
  PathComboBox.OnChange := PathComboBoxChange;
  PathComboBox.path := extractfilepath(paramstr(0));

  //SetCurrentFolder;
  DropPIDLTarget1.Register(Listview1);

  fRecyclePIDL := nil;
  shGetSpecialFolderLocation(0, CSIDL_BITBUCKET ,fRecyclePIDL);
end;
//---------------------------------------------------------------------

procedure TForm1.FormDestroy(Sender: TObject);
begin
  DropPIDLTarget1.Unregister;

  Listview1.Items.Clear;

  FImageList.free;
  ShellMalloc.Free(fRecyclePIDL);
end;
//---------------------------------------------------------------------

//---------------------------------------------------------------------
// Start a Drag and Drop (DropPIDLSource1.execute) ...
//---------------------------------------------------------------------
procedure TForm1.ListView1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
const
  AllowedAttribMask: Longint = (SFGAO_CANCOPY or SFGAO_CANMOVE);
var
  i: integer;
  attr: UINT;
  res: TDragResult;
  tmpImageList: TImageList;
  dummyPt: TPoint;
  DraggingFromRecycle: boolean;
  attributes: integer;
begin
  //If no files selected then exit...
  if Listview1.SelCount = 0 then
    exit;

  statusbar1.simpletext := '';
  if (DragDetectPlus(TWinControl(Sender), Point(X,Y))) then
  begin
    // OK, HOW TO KNOW IF WE'RE DRAGGING FROM THE 'RECYCLE BIN'...
    DraggingFromRecycle := False;
    // ILIsEqual() doesn't seem to work in Win95/Win98 ...
    if ILIsEqual(fRecyclePIDL,PathCombobox.pidl) then
      DraggingFromRecycle := true
    else
    begin
      // OK, not great but this works in Win95/Win98 ...
      attributes := integer(GetFileAttributes(pchar(PathCombobox.path)));
      if (attributes <> -1) and (attributes and FILE_ATTRIBUTE_HIDDEN <> 0) and
         (attributes and FILE_ATTRIBUTE_SYSTEM <> 0) then
        DraggingFromRecycle := true;
    end;

    // CopyFolderPidlToList automatically deletes anything from a previous dragdrop...
    DropPIDLSource1.CopyFolderPidlToList(PathComboBox.Pidl);
    // Fill DropSource1.Files with selected files in ListView1...
    for i := 0 to Listview1.items.Count-1 do
      if (Listview1.items.item[i].Selected) then
        with TLVItemData(Listview1.items.item[i].data) do
        begin
          // Make sure the shell allows us to drag each selected file/folder ...
          attr := AllowedAttribMask;
          CurrentShellFolder.GetAttributesOf(1,RelativePIDL,attr);
          // If not allowed to copy and move the quit drag...
          if (attr and AllowedAttribMask) = 0  then
            exit;
          DropPIDLSource1.CopyFilePidlToList(RelativePIDL);
          if DraggingFromRecycle then
            DropPIDLSource1.MappedNames.add(Listview1.items.item[i].Caption);
        end;

    // Let the Listview create the drag image for us ...
    tmpImageList := TImageList.Create(Self);
    try
      tmpImageList.handle :=
        ListView_CreateDragImage(Listview1.Handle, Listview1.Selected.Index, dummyPt);
      // Note: ListView_CreateDragImage fails to include the list item text on
      // some versions of Windows. Known problem with no solution according to MS.
      DropPIDLSource1.Images := tmpImageList;
      DropPIDLSource1.ImageIndex := 0;
      DropPIDLSource1.ImageHotSpotX := X-ListView1.Selected.Left;
      DropPIDLSource1.ImageHotSpotY := Y-ListView1.Selected.Top;
      DropPIDLSource1.ShowImage := True;

      statusbar1.SimpleText := 'Dragging ...';

      // DropPIDLTarget1.dragtypes := [];
      // the above line has been commented out to
      // allow dropping onto self if a subfolder is the droptarget...
      // see DropPIDLTarget1DragOver()

      //Do the dragdrop...
      FIsDragging := True;
      try
        res := DropPIDLSource1.Execute;
      finally
        FIsDragging := False;
      end;

    finally
      tmpImageList.Free;
    end;

    //DropPIDLTarget1.dragtypes := [dtCopy,dtMove];

    if res in [drDropCopy, drDropMove] then
      statusbar1.simpletext := 'Drag and Drop succeeded.'
    else
      statusbar1.simpletext := 'Drag and Drop cancelled.';

    if (res <> drDropMove) then
      exit;

    // This is a real kludge, which also may not be long enough...
    sleep(1000);
    RefreshListNames;
  end;
end;


//---------------------------------------------------------------------
// DropPIDLTarget1 Methods ...
//---------------------------------------------------------------------

// If the Listview's droptarget is a system folder then
// make sure the target highlighting is done 'cleanly'...
// otherwise don't allow the drop.
procedure TForm1.DropPIDLTarget1DragOver(Sender: TObject;
  ShiftState: TShiftState; Point: TPoint; var Effect: Integer);
var
  NewTargetListItem: TListItem;
begin
  NewTargetListItem := Listview1.GetItemAt(Point.X,Point.Y);

  if (NewTargetListItem = nil) then
  begin
    // If a folder was previously a droptarget cancel its droptarget status...
    if (Listview1.DropTarget <> nil) then
    begin
      // Hide the drag image.
      DropPIDLTarget1.ShowImage := False;
      try
        // cancel current droptarget folder as droptarget...
        Listview1.DropTarget := nil;
        Listview1.Update;
      finally
        // Windows must have time to repaint the invalidated listview
        // items before we show the drag image again.
        DropPIDLTarget1.ShowImage := True;
      end;
    end;

    // Only allow a drop into current folder if we ourself are not the source,
    // and the destination folder isn't virtual...
    if (FIsDragging) or (PathCombobox.IsVirtualPath) then
      Effect := DROPEFFECT_NONE;
  end else
    if (Listview1.DropTarget = NewTargetListItem) then
      //Effect := Effect  //ie: don't fiddle with Effect
    else
      if (TLVItemData(NewTargetListItem.data).sortstr[1] = '1') then
      begin
        // only allow file system folders to be targets...

        // Hide the drag image...
        DropPIDLTarget1.ShowImage := false;
        try
          // Cancel current droptarget folder as droptarget...
          Listview1.DropTarget := nil;
          // set the new droptarget folder...
          Listview1.DropTarget := NewTargetListItem;
          Listview1.Update;
        finally
          // windows must have time to repaint the invalidated listview
          // items before we show the drag image again.
          DropPIDLTarget1.ShowImage := True;
        end;
      end else
        Effect := DROPEFFECT_NONE;
end;
//---------------------------------------------------------------------

procedure TForm1.DropPIDLTarget1Drop(Sender: TObject;
  ShiftState: TShiftState; Point: TPoint; var Effect: Integer);
var
  i: integer;
  fos: TShFileOpStruct;
  strFrom, strTo, DestPath: string;
  Operation: integer;
begin

  //first, where are we dropping TO...
  strTo := '';
  if (Listview1.DropTarget <> nil) then
    //dropping into a subfolder...
    with TLVItemData(Listview1.DropTarget.data) do
    begin
      if sortstr[1] = '1' then
        strTo := Copy(sortstr,2,MAX_PATH)+#0#0
      else
        Effect := DROPEFFECT_NONE; //subfolder must be a system folder!
    end
  else if PathComboBox.path <> '' then
    //OK, dropping into current folder...
    strTo := PathComboBox.path +#0#0
  else
    Effect := DROPEFFECT_NONE; //current folder must be a system folder!

  Operation := 0;
  case Effect of
    DROPEFFECT_COPY: Operation := FO_COPY;
    DROPEFFECT_MOVE: Operation := FO_MOVE;
  else
    Effect := DROPEFFECT_NONE;
  end;

  // Only allow a Copy or Move operation...
  // otherwise stop and signal to source that no drop occured.
  if Effect = DROPEFFECT_NONE then
    exit;

  // now, where are we dropping FROM...
  strFrom := '';
  with DropPIDLTarget1 do
  begin
    for i := 0 to Filenames.count-1 do
      if Filenames[i] = '' then
        exit
      else // quit if 'virtual'
        strFrom := strFrom + Filenames[i]+#0;
  end;

  if strFrom = '' then
  begin
    //signal to source something wrong...
    Effect := DROPEFFECT_NONE;
    exit;
  end;

  with fos do
  begin
    wnd := self.handle;
    wFunc := Operation;
    pFrom := PChar(strFrom);
    pTo := PChar(strTo);
    fFlags := FOF_ALLOWUNDO;
    hNameMappings:= nil;
  end;

  try

    // Copy or move the files
    SHFileOperation(fos);

  except
    // Avoid that an exception interrupts the drag/drop prematurely.
    on E: Exception do
    begin
      Application.ShowException(E);
      Effect := DROPEFFECT_NONE;
      exit;
    end;
  end;

  //if dropped files need to be renamed -
  //(eg if they have been dragged from the recycle bin) ...
  with DropPIDLTarget1 do
    if MappedNames.count > 0 then
    begin
      if PathComboBox.path[length(PathComboBox.path)] <> '\' then
        DestPath := PathComboBox.path + '\' else
        DestPath := PathComboBox.path;
      for i := 0 to MappedNames.count-1 do
      begin
        if fileexists(DestPath+ extractfilename(filenames[i])) then
          renamefile(DestPath+ extractfilename(filenames[i]),
            DestPath+MappedNames[i]);
      end;
    end;

  RefreshListNames;
end;
//---------------------------------------------------------------------

procedure TForm1.SetCurrentFolder;
var
  sfi: tshfileinfo;
begin
  if PathComboBox.Pidl <> nil then
  begin
    //Get CurrentShellFolder...
    //nb: DesktopShellFolder is a Global Variable declared in PathComboBox.
    if PathComboBox.itemindex = 0 then //Desktop folder
      CurrentShellFolder := DesktopShellFolder else
      DesktopShellFolder.BindToObject(PathComboBox.Pidl,
          nil, IID_IShellFolder, pointer(CurrentShellFolder));
    //Get CurrentFolder's ImageIndex...
    shgetfileinfo(pChar(PathComboBox.Pidl),
      0,sfi,sizeof(tshfileinfo), SHGFI_PIDL or SHGFI_ICON);
    CurrentFolderImageIndex := sfi.iIcon;
    RefreshListNames;
  end;

  // Don't allow a drop onto a virtual folder...
  if PathComboBox.path <> '' then
    DropPIDLTarget1.DragTypes := [dtCopy,dtMove]
  else
    DropPIDLTarget1.DragTypes := [];

  sbUpLevel.Enabled := (PathComboBox.ItemIndex <> 0);
end;
//---------------------------------------------------------------------

procedure TForm1.RefreshListNames;
begin
  Listview1.Items.BeginUpdate;
  try
    Listview1.Items.Clear;
    Screen.Cursor := crHourglass;
    try
      PopulateListview;
    finally
      Screen.Cursor := crDefault;
    end;
  finally
    Listview1.Items.EndUpdate;
  end;
end;
//---------------------------------------------------------------------

procedure TForm1.PopulateListview;
var
  EnumIdList: IEnumIdList;
  tmpPIDL: pItemIDList;
  NewItem: TListItem;
  ItemData: TLVItemData;
  sfi: TShFileInfo;
  Flags, dummy: DWORD;
begin
  if CurrentShellFolder = nil then
    exit;

  //get files and folders...
  Flags := SHCONTF_FOLDERS or SHCONTF_NONFOLDERS or SHCONTF_INCLUDEHIDDEN;
  if FAILED(CurrentShellFolder.EnumObjects(0, Flags, EnumIdList)) then
    exit;
  while (EnumIdList.Next(1, tmpPIDL, dummy) = NOERROR) do
  begin
    NewItem := Listview1.Items.Add;
    NewItem.Caption := GetPIDLDisplayName(CurrentShellFolder, tmpPIDL);
    ItemData := TLVItemData.create;
    NewItem.Data := ItemData;
    ItemData.RelativePIDL := tmpPIDL;
    ItemData.AbsolutePIDL := ILCombine(PathComboBox.Pidl, tmpPIDL);
    shGetFileInfo(PChar(ItemData.AbsolutePIDL), 0, sfi,
      SizeOf(tshfileinfo), SHGFI_PIDL or SHGFI_ICON or SHGFI_ATTRIBUTES);
    NewItem.ImageIndex := sfi.iIcon;
    //get sort order...
    if (sfi.dwAttributes and SFGAO_FOLDER) <> 0 then
    begin
      if (sfi.dwAttributes and SFGAO_FILESYSTEM) <> 0 then
        //file system folder
        ItemData.SortStr := '1'+ GetPIDLDisplayName(CurrentShellFolder, tmpPIDL, SHGDN_FORPARSING)
      else
        //virtual folder
        ItemData.SortStr := '2'+ GetPIDLDisplayName(CurrentShellFolder, tmpPIDL, SHGDN_FORPARSING);
    end
    else
      //files
      ItemData.SortStr := '9'+ GetPIDLDisplayName(CurrentShellFolder, tmpPIDL, SHGDN_FORPARSING);
  end;
  ListView1.CustomSort(TLVCompare(@ListviewSort), 0);
  if Listview1.Items.Count > 0 then
    Listview1.Items[0].Focused := true;
end;
//---------------------------------------------------------------------

procedure TForm1.PathComboBoxChange(Sender: TObject);
begin
  SetCurrentFolder;
  caption := PathComboBox.path;
end;
//---------------------------------------------------------------------

//If a folder double-clicked - open that folder...
procedure TForm1.ListView1DblClick(Sender: TObject);
var
  SelItem: TListItem;
begin
  SelItem := Listview1.Selected;
  if SelItem = nil then exit;
  with TLVItemData(SelItem.data) do
    if (sortstr[1] < '9') then //if a folder...
      PathComboBox.Pidl := AbsolutePIDL;
end;
procedure TForm1.ListView1Deletion(Sender: TObject; Item: TListItem);
begin
  if (Item.Data <> nil) then
    TObject(Item.Data).Free;
end;

//---------------------------------------------------------------------

//If a folder selected - open that folder...
procedure TForm1.ListView1KeyPress(Sender: TObject; var Key: Char);
var
  SelItem: TListItem;
begin
  SelItem := Listview1.Selected;
  if (SelItem = nil) or (ord(Key) <> VK_RETURN) then
    exit;
  with TLVItemData(SelItem.data) do
    if (sortstr[1] < '9') then //if a folder...
      PathComboBox.Pidl := AbsolutePIDL;
end;
//---------------------------------------------------------------------

procedure TForm1.sbUpLevelClick(Sender: TObject);
var
  tmpPidl: pItemIdList;
begin
  if PathComboBox.ItemIndex > 0 then
  begin
    tmpPidl := ILClone(PathComboBox.Pidl);
    ILRemoveLastID(tmpPidl);
    PathComboBox.Pidl := tmpPidl;
    ShellMalloc.Free(tmpPidl);
  end;
end;
//---------------------------------------------------------------------

procedure TForm1.Button1Click(Sender: TObject);
begin
  Close;
end;
//---------------------------------------------------------------------
//---------------------------------------------------------------------

end.

