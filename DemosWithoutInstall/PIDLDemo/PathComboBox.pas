unit PathComboBox;

// -----------------------------------------------------------------------------
// Project:         Shell PathComboBox Component
// Component Names: TPathComboBox
// Module:          PathComboBox
// Version:         3.2a
// Date:            03-MAY-1999
// Target:          Win32; Delphi3-6, C++Builder 3-5
// Author:          Angus Johnson, ajohnson@rpi.net.au
// Copyright        ©1997-99 Angus Johnson
// -----------------------------------------------------------------------------
// 25-apr-2003  anme    Added work around for Delphi 7 TComboBox bug.
// -----------------------------------------------------------------------------

{$include dragdrop.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ShellApi, ShlObj, ActiveX, ImgList;

type

  TPathComboBox = class(TCustomComboBox)
  private
    FPath: string;
    FDisplayName: string;
    FPidl: pItemIdList;
    DesktopPIDL,DrivesPIDL: pItemIdList;
    FAllowVirtual: boolean; //? allow 'Control Panel', 'Printers' etc.
    FIsVirtualFolder: boolean;
    FImageList: TImageList;
    FDrawingEdit: boolean;

    //Can't use items.objects to store ItemData (in Delphi 3) because no
    //handle to 'Items' exists in Destroy method (where they are cleaned up)...
    //(In Delphi 4, the BeforeDestruction method could be used instead.)
    FItemDataList: TList;

    procedure BuildCore;
    procedure ClearNonCore;
    procedure SetPath(NewPath: string);
    procedure SetPidl(pidl: pItemIdList);
    procedure SortItems(StartItem, EndItem: integer);
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure WMKEYDOWN(var Message: TWMKey); message WM_KEYDOWN;
    procedure CNCommand(var Message: TWMCommand); message CN_COMMAND;
    procedure CNDrawItem(var Message: TWMDrawItem); message CN_DRAWITEM;
    function GetPath: string;
  protected
    procedure CreateWnd; override;
    procedure BuildNewList(pidl: pItemIdList);
    procedure DrawItem(Index: Integer;
      Rect: TRect; State: TOwnerDrawState); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Change; override;

    //The Path properties...
    //The path can be set with either a string or a PIDL (pItemIdList) ...
    property Path: string read GetPath write SetPath;
    property Pidl: pItemIdList read FPidl write SetPidl;
    //The Folder display name (which is different to the Path).
    property DisplayName: string read FDisplayName;
    //True if 'Virtual' folder selected (Control Panel, Printers etc)
    //nb: if IsVirtualPath = true then Path = ''.
    property IsVirtualPath: boolean read FIsVirtualFolder;

  published
    property Color;
    property Ctl3D;
    property DragMode;
    property DragCursor;
    property Enabled;
    property Font;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property ShowVirtualFolders: boolean read FAllowVirtual write FAllowVirtual;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
    property OnChange;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDropDown;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnStartDrag;
  end;

procedure Register;

//---------------------------------------------------------------------
// Some 'unnamed' Windows functions (which are very useful) ...
// (Thanks to - http://www.geocities.com/SiliconValley/4942/index.html)
//---------------------------------------------------------------------
function ILCombine(pidl1,pidl2:PItemIDList): PItemIDList; stdcall;
function ILFindLastID(pidl: PItemIDList): PItemIDList; stdcall;
function ILClone(pidl: PItemIDList): PItemIDList; stdcall;
function ILRemoveLastID(pidl: PItemIDList): LongBool; stdcall;
function ILIsEqual(pidl1,pidl2: PItemIDList): LongBool; stdcall;

var
  //The following 2 interface pointers are declared in the 'interface' section
  //as they may be very useful. They are assigned in 'initialization' section.
  DesktopShellFolder: IShellFolder;
  //ShellMalloc is used in this unit just to free 'Pidls'.
  //Probably _slightly_ quicker than using CoTaskMemFree() each time.
  ShellMalloc: IMalloc;

implementation

//---------------------------------------------------------------------
// Miscellaneous Functions ...
//---------------------------------------------------------------------

procedure Register;
begin
  RegisterComponents('Samples', [TPathComboBox]);
end;
//---------------------------------------------------------------------

function ILCombine(pidl1,pidl2:PItemIDList): PItemIDList; stdcall;
  external shell32 index 25;
function ILFindLastID(pidl: PItemIDList): PItemIDList; stdcall;
  external shell32 index 16;
function ILClone(pidl: PItemIDList): PItemIDList; stdcall;
  external shell32 index 18;
function ILRemoveLastID(pidl: PItemIDList): LongBool; stdcall;
  external shell32 index 17;
function ILIsEqual(pidl1,pidl2: PItemIDList): LongBool; stdcall;
  external shell32 index 21;
//---------------------------------------------------------------------

function GetFullPathFromPidl(pidl: pItemIDList): string;
var
  buff: pChar;
begin
  GetMem(buff,MAX_PATH);
  if SHGetPathFromIDList(pidl,buff) then
    result := buff else
    result := '';
  FreeMem(buff);
end;
//---------------------------------------------------------------------

function GetPidlFromPath(Folder: string): pItemIdList;
var
  dummy,dummy2: DWORD;
  WideStr: WideString;
begin
  WideStr := Folder;
  //nb: DesktopShellFolder is defined as a Global Variable.
  if FAILED(DesktopShellFolder.ParseDisplayName(0,
    nil,PWideChar(WideStr),dummy,result,dummy2)) then result := nil;
end;

//---------------------------------------------------------------------
// TItemData class
// (Used to store extra data with each Combobox dropdown item)
//---------------------------------------------------------------------

type

  TItemData = class
    Foldername: string;
    SortString: string; //used just to sort the "core" folder items
    Level: integer;
    Core: boolean; // flag for "core" folder items
    ImageIndex: integer;
    ImageIndexOpen: integer;
    RelativePIDL: pItemIDList; {each item stores its own PIDLs}
    AbsolutePIDL: pItemIDList;
  public
    destructor Destroy; override;
  end;

destructor TItemData.Destroy;
begin
  ShellMalloc.Free(AbsolutePIDL);
  inherited;
end;

//---------------------------------------------------------------------
// TPathComboBox Component ...
//---------------------------------------------------------------------

constructor TPathComboBox.Create(AOwner: TComponent);
var
  sfi: tshfileinfo;
begin
  inherited Create(AOwner);
  width := 230;
  Style := csOwnerDrawFixed;
  FAllowVirtual := false;
  FImageList := TImageList.create(self);

  //get the shell imagelist...
  if not (csDesigning in ComponentState) then
  begin
    FImageList.handle := shgetfileinfo('',0,
      sfi,sizeof(tshfileinfo), shgfi_sysiconindex or shgfi_smallicon);
    FImageList.shareimages := true;
    FImageList.BlendColor := clHighlight;
    FImageList.DrawingStyle := dsTransparent;
  end;

  FItemDataList := TList.create;
end;
//---------------------------------------------------------------------

procedure TPathComboBox.CreateWnd;
begin
  inherited;
  if not (csDesigning in ComponentState) then
    BuildCore;
end;
//---------------------------------------------------------------------

destructor TPathComboBox.Destroy;
var
  i: integer;
begin
  FImageList.handle := 0;
  FImageList.free;
  for i := 0 to FItemDataList.count-1 do
    TItemData(FItemDataList[i]).free;
  FItemDataList.free;
  inherited Destroy;
end;
//---------------------------------------------------------------------

procedure TPathComboBox.SortItems(StartItem, EndItem: integer);
var
  i: integer;

  procedure Swap(a,b: integer);
  var
    str: string;
  begin
    str := items[a];
    items[a] := items[b];
    items[b] := str;
    FItemDataList.exchange(a,b);
  end;

begin
  while EndItem > StartItem do
  begin
    for i := StartItem to EndItem-1 do
      if TItemData(FItemDataList[i]).SortString >
        TItemData(FItemDataList[i+1]).SortString then swap(i,i+1);
    dec(EndItem);
  end;
end;
//---------------------------------------------------------------------

type
  TAllowed = (aALL, aSYSTEM);

procedure TPathComboBox.BuildCore;
var
  sfi: tshfileinfo;
  DrivesShellFolder: IShellFolder;
  pidl,absPidl: pItemIdList;
  EnumIdList: IEnumIdList;
  SuccessCnt: DWORD;
  i, SortStartItem: integer;

  procedure AddToList(pidl: pItemIdList; ItemLevel: integer; allowed: TAllowed);
  var
    ItemData: TItemData;
    tmpPath: string;
  begin
    //nb: the 'pidl' passed as a parameter is freed when ItemData is freed.
    ShGetFileInfo(PChar(pidl), 0,sfi,sizeof(sfi),
      {SHGFI_ATTRIBUTES or} SHGFI_ICON or SHGFI_DISPLAYNAME or SHGFI_PIDL);
    if (allowed = aSYSTEM) and (sfi.dwAttributes and SFGAO_FILESYSTEM = 0) then
    begin
      //don't add this (virtual) item!
      ShellMalloc.Free(pidl);
      exit;
    end;
    tmpPath := GetFullPathFromPidl(pidl);
    ItemData := TItemData.create;
    with ItemData do
    begin
      AbsolutePIDL := pidl;
      if Level = 0 then //desktop
        RelativePIDL := AbsolutePIDL else
        RelativePIDL := ILFindLastID(AbsolutePIDL);
      Foldername := sfi.szDisplayName;
      //SortString is used to sort the "Drives" folder and Desktop folders...
      //Virtual folders follow system folders in the "Drives" folder,
      //Virtual folders go before system folders in the "Desktop" folder.
      if tmpPath <> '' then //file system folders
      begin
        if ItemLevel = 1 then
          SortString := '9'+tmpPath else
          SortString := '1'+tmpPath;
      end else
      begin //virtual folders
        if ItemLevel = 1 then
          SortString := '1'+ Foldername else
          SortString := '9'+ Foldername;
      end;
      ImageIndex := sfi.iIcon;
      ImageIndexOpen := sfi.iIcon;
      Level := ItemLevel;
      Core := true;
    end;
    // Workaround for Delphi 7 TComboBox bug
    if (tmpPath = '') then
      tmpPath := #1;
    items.Add(tmpPath);
    FItemDataList.add(ItemData);
  end;

begin

  //if just handle being reallocated then rebuild everything...
  if FItemDataList.count > 0 then
  begin
    items.clear;
    for i := 0 to FItemDataList.count-1 do
      with TItemData(FItemDataList[i]) do
      begin
        items.add(GetFullPathFromPidl(AbsolutePIDL));
        if AbsolutePIDL = FPidl then itemindex := i;
      end;
    if itemindex < 0 then itemindex := 0;
    exit;
  end;

  //BUILD THE CORE ITEMS...

  if FAILED(SHGetDesktopFolder(DesktopShellFolder)) then
    raise Exception.Create('Unable to create "DesktopShellFolder" in PathComboBox');
  if FAILED(SHGetSpecialFolderLocation(0,CSIDL_DESKTOP,DesktopPIDL)) then
    raise Exception.Create('Unable to create "DesktopPIDL" in PathComboBox');
  if FAILED(SHGetSpecialFolderLocation(0,CSIDL_DRIVES,DrivesPIDL)) then
    raise Exception.Create('Unable to create "DrivesPIDL" in PathComboBox');

  //Add desktop ...
  AddToList(DesktopPIDL,0,aALL);
  //default to desktop...
  itemindex := 0;
  FIsVirtualFolder := false;
  FPidl := DesktopPIDL;
  fPath := items[0];
  FDisplayName := TItemData(FItemDataList[0]).Foldername;

  //Add drives folder...
  AddToList(DrivesPIDL,1,aALL);

  //Add Drives sub-folders ...
  if FAILED(DesktopShellFolder.BindToObject(DrivesPIDL,
        nil, IID_IShellFolder, pointer(DrivesShellFolder))) then
    raise Exception.Create('Unable to create "DrivesShellFolder" in PathComboBox');
  if FAILED(DrivesShellFolder.EnumObjects(0,SHCONTF_FOLDERS,EnumIdList)) then
    raise Exception.Create('Unable to enumerate "Drives" Folder in PathComboBox');
  //Enumerating more than 1 at a time doesn't seem to work!?
  while SUCCEEDED(EnumIdList.Next(1,pidl,SuccessCnt)) and (SuccessCnt = 1) do
  begin
    absPidl := ILCombine(DrivesPIDL,Pidl);
    ShellMalloc.Free(pidl);
    if FAllowVirtual then
      AddToList(absPidl,2,aALL) else
      AddToList(absPidl,2,aSYSTEM);
  end;
  SortItems(2,items.count-1); //sort the "drives" folder...

  SortStartItem := items.Count; //Position for start of next sort
  //Now enumerate remaining Desktop sub-folders skipping the 'Drives' folder
  //which has already been added (file system folders added last).
  DesktopShellFolder.EnumObjects(0,SHCONTF_FOLDERS,EnumIdList);
  while SUCCEEDED(EnumIdList.Next(1,pidl,SuccessCnt)) and (SuccessCnt = 1) do
  begin
    absPidl := ILCombine(DesktopPIDL,pidl);
    ShellMalloc.Free(pidl);
    if ILIsEqual(absPidl,DrivesPIDL) then
      ShellMalloc.Free(absPidl) else
      AddToList(absPidl,1,aALL);
  end;
  SortItems(SortStartItem,items.count-1); //sort the remaining "desktop" folders...
end;
//---------------------------------------------------------------------

procedure TPathComboBox.ClearNonCore;
var
  i: integer;
begin
  for i := FItemDataList.count-1 downto 2 do
    with TItemData(FItemDataList[i]) do
      if not Core then
      begin
        if i < items.count then items.delete(i);
        free;
        FItemDataList.delete(i);
      end;
end;
//---------------------------------------------------------------------

procedure TPathComboBox.CNCommand(var Message: TWMCommand);
begin
  inherited;
  //when the dropdown window closes notify of (potential) change...
  if Message.NotifyCode = CBN_CLOSEUP then Change;
end;
//---------------------------------------------------------------------

procedure TPathComboBox.Change;
begin
  //don't process any changes while dropdown window visible...
  if (sendmessage(handle,CB_GETDROPPEDSTATE,0,0) <> 0) then exit;
  //OK, only do something if the path has changed...
  if (itemindex >= 0) and ((FPidl = nil) or
      not ILIsEqual(TItemData(FItemDataList[itemindex]).AbsolutePIDL,FPidl)) then
  begin
    SetPidl(TItemData(FItemDataList[itemindex]).AbsolutePIDL);
    inherited Change;
  end;
end;
//---------------------------------------------------------------------

procedure TPathComboBox.SetPath(NewPath: string);
var
  tmpPidl: pItemIdList;
begin
  tmpPidl := GetPidlFromPath(NewPath);
  if tmpPidl = nil then exit;
  SetPidl(tmpPidl);
  ShellMalloc.Free(tmpPidl);
end;
//---------------------------------------------------------------------

function TPathComboBox.GetPath: string;
begin
  Result := fPath;
  // Workaround for Delphi 7 TComboBox bug
  if (Result = #1) then
    Result := '';
end;
//---------------------------------------------------------------------

//NB: A 'virtual' path can only be set by its PIDL...
procedure TPathComboBox.SetPidl(pidl: pItemIdList);
var
  tmpPidl: pItemIdList;
begin
  //clone 'pidl' incase it is destroyed during BuildNewList...
  HandleNeeded;
  tmpPidl := ILClone(pidl);
  BuildNewList(tmpPidl);
  ShellMalloc.Free(tmpPidl);
  if assigned(OnChange) then OnChange(self);
end;
//---------------------------------------------------------------------

procedure TPathComboBox.BuildNewList(pidl: pItemIdList);
var
  tmpList: TList;
  i,j: integer;
  tmpPidl,tmpPidl2: pItemIdList;
  found: boolean;

  procedure CleanUp;
  var
    i: integer;
  begin
    for i := 0 to tmpList.count-1 do
      ShellMalloc.Free(pItemIdList(tmpList[i]));
    tmpList.free;
  end;

  procedure AddSubFolders(StartLevel,InsertPos: integer);
  var
    i: integer;
    sfi: tshfileinfo;
    ItemData: TItemData;
  begin
    for i := StartLevel to tmpList.count-1 do
    begin
      tmpPidl := tmpList[tmpList.count-i-1];
      ShGetFileInfo(PChar(tmpPidl),
        0,sfi,sizeof(sfi),SHGFI_ICON or SHGFI_DISPLAYNAME or SHGFI_PIDL);
      ItemData := TItemData.create;
      with ItemData do
      begin
        AbsolutePIDL := ILClone(tmpPidl);
        RelativePIDL := ILFindLastID(AbsolutePIDL);
        Foldername := sfi.szDisplayName;
        Core := false;
        ImageIndex := sfi.iIcon;
        Level := i;
        ShGetFileInfo(PChar(AbsolutePIDL),0,sfi,sizeof(sfi),
           SHGFI_SYSICONINDEX or SHGFI_OPENICON or SHGFI_PIDL);
        ImageIndexOpen := sfi.iIcon;
      end;
      FItemDataList.insert(InsertPos,ItemData);
      items.insert(InsertPos,GetFullPathFromPidl(ItemData.AbsolutePIDL));
      inc(InsertPos);
    end;
    with TItemData(FItemDataList[InsertPos-1]) do
    begin
      fPidl := AbsolutePIDL;
      FDisplayName := Foldername;
      itemindex := InsertPos-1;
      fPath := items[itemindex];
    end;
  end;

begin
  ClearNonCore;
  tmpPidl := ILClone(pidl);
  tmpPidl2 := ILClone(pidl);

  //create a list of absolute pidls from the path...
  tmpList := TList.create;
  tmpList.add(tmpPidl2);
  while ILRemoveLastID(tmpPidl) do
  begin
    tmpPidl2 := ILClone(tmpPidl);
    tmpList.add(tmpPidl2);
  end;
  ShellMalloc.Free(tmpPidl);

  FPidl := nil;
  fPath := '';
  //If the pidl is part of the core, then not much to do ...
  for i := 0 to FItemDataList.count-1 do
    if ILIsEqual( TItemData(FItemDataList[i]).AbsolutePIDL, pidl ) then
    begin
      FPidl := TItemData(FItemDataList[i]).AbsolutePIDL;
      FDisplayName := TItemData(FItemDataList[i]).Foldername;
      itemindex := i;
      fPath := items[itemindex];
      CleanUp;
      exit;
    end;

  found := false;
  //find the matching Level1 folder...
  for i := 1 to FItemDataList.count -1 do
    if ILIsEqual( tmpList[tmpList.count-2],
      TItemData(FItemDataList[i]).AbsolutePIDL) then
    begin
      //if the Level1 folder is the 'Drives' folder...
      //find the matching Level2 folder...
      if ILIsEqual( tmpList[tmpList.count-2],DrivesPIDL) then
      begin
        for j := 2 to FItemDataList.count -1 do
          if ILIsEqual( tmpList[tmpList.count-3],
            TItemData(FItemDataList[j]).AbsolutePIDL) then
          begin
            AddSubFolders(3,j+1);
            found := true;
            break;
          end;
      end else
      begin
        AddSubFolders(2,i+1);
        found := true;
        break;
      end;
    end;

  cleanup;

  if not found then
    raise Exception.create('Invalid PIDL passed to PathComboBox');

end;
//---------------------------------------------------------------------

procedure TPathComboBox.CNDrawItem(var Message: TWMDrawItem);
begin
  with Message.DrawItemStruct^ do
  begin
    //Delphi doesn't indicate in TOwnerDrawState whether it's
    //the Edit or Dropdown window that's about to painted so...
    FDrawingEdit := (itemState and ODS_COMBOBOXEDIT) <> 0;

    //A little workaround so the component name will be drawn if designing.
    if (Integer(itemID) < 0) then itemID := $FFFF;

    inherited;
  end;
end;
//---------------------------------------------------------------------

procedure TPathComboBox.DrawItem(Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
begin
  with canvas do
  begin

    FillRect(Rect);

    //if there are no items, ie designing ...
    if Index = $FFFF then
    begin
      textout(Rect.left+2, Rect.top+1, Name);
      exit;
    end;

    with TItemData(FItemDataList[Index]) do
    begin

      if FDrawingEdit then
        Rect.left := Rect.left + 2 else
        Rect.left := Rect.left + 2 + (Level*12);
      Rect.top := Rect.top + 1;

      if (AbsolutePIDL = FPidl) then
        FImageList.draw(canvas,Rect.left,Rect.top,ImageIndexOpen) else
        FImageList.draw(canvas,Rect.left,Rect.top,ImageIndex);

      Rect.left := Rect.left+FImageList.width+4;
      textout(Rect.left, Rect.top, Foldername);
    end;
  end;
end;
//---------------------------------------------------------------------

procedure TPathComboBox.WMKEYDOWN(var Message: TWMKey);
begin
  //Only process keys when the dropdown window is visible.
  //note: alt-downarrow etc still work as expected...
  if (sendmessage(handle,CB_GETDROPPEDSTATE,0,0) <> 0) then inherited;
end;
//---------------------------------------------------------------------

//Make sure there is still room for the image if small fonts are used...
procedure TPathComboBox.CMFontChanged(var Message: TMessage);

  //borrowed from Delphi :-)
  function GetItemHeight(Font: TFont): Integer;
  var
    DC: HDC;
    SaveFont: HFont;
    Metrics: TTextMetric;
  begin
    DC := GetDC(0);
    SaveFont := SelectObject(DC, Font.Handle);
    GetTextMetrics(DC, Metrics);
    SelectObject(DC, SaveFont);
    ReleaseDC(0, DC);
    Result := Metrics.tmHeight;
  end;

var
  IHeight: integer;
begin
  inherited;
  IHeight := GetItemHeight(Font);
  if IHeight < FImageList.height then IHeight := FImageList.height;
  ItemHeight := IHeight+2;
  RecreateWnd;
end;
//---------------------------------------------------------------------
//---------------------------------------------------------------------

initialization
  SHGetDesktopFolder(DesktopShellFolder);
  ShGetMalloc(ShellMalloc);

end.

