unit ContextMenuHandlerMain;
(*
** Portions copyright © 1995-2001 Borland Software Corporation.
**
** This demo is based, in part, on Delphi's TRegSvr utility.
*)
interface

uses
  DragDrop, DropTarget, DragDropContext,
  Forms, ShlObj, SysUtils, Classes, Menus, Windows, Graphics, ImgList,
  Controls;

{$include 'DragDrop.inc'}

//{$ifndef VER13_PLUS}
//type
//  TDataModule = TForm;
//{$endif}

type
  TRegAction = (raReg, raUnreg);

  (*
  ** The data module implements our shell extension and must support all the
  ** nescessary interfaces.
  **
  ** IUnknown is implemented by TComponent. The rest are implemented by the
  ** TDropContextMenu component through delegation.
  **
  ** Note that it is very important to include IUnknown in the interface list
  ** in order to get reference counting working properly.
  *)
  TDataModuleContextMenuHandler = class(TDataModule, IUnknown, IShellExtInit,
    IContextMenu, IContextMenu2, IContextMenu3)
    DropContextMenu1: TDropContextMenu;
    PopupMenu1: TPopupMenu;
    MenuCOMServer: TMenuItem;
    MenuRegister: TMenuItem;
    MenuUnregister: TMenuItem;
    MenuAbout: TMenuItem;
    MenuAboutInfo: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    procedure DropContextMenu1Popup(Sender: TObject);
    procedure MenuRegisterClick(Sender: TObject);
    procedure MenuUnregisterClick(Sender: TObject);
    procedure MenuAboutInfoMeasureItem(Sender: TObject; ACanvas: TCanvas;
      var Width, Height: Integer);
    procedure MenuAboutInfoClick(Sender: TObject);
    procedure MenuAboutInfoDrawItem(Sender: TObject; ACanvas: TCanvas;
      ARect: TRect; Selected: Boolean);
    procedure DataModuleCreate(Sender: TObject);
  private
    Status: string;
  protected
    procedure DoRegister(Action: TRegAction);
    procedure RegisterTypeLib(const Filename: string; Action: TRegAction);
    procedure RegisterExe(const Filename: string; Action: TRegAction);
    procedure RegisterActiveX(const Filename: string; Action: TRegAction);
    procedure OutputStr(const s: string);
  public
    // Aggregate IShellExtInit and IContextMenu to the TDropContextMenu component.
    property ContextMenuHandler: TDropContextMenu read DropContextMenu1
      implements IShellExtInit, IContextMenu, IContextMenu2, IContextMenu3;
  end;

implementation

{$R *.DFM}

uses
  ComServ,
  ActiveX,
  ComObj,
  ShellApi;

const
  // CLSID for this shell extension.
  // Modify this for your own shell extensions (press [Ctrl]+[Shift]+G in
  // the IDE editor to gererate a new CLSID).
  CLSID_ContextMenuHandler: TGUID = '{516EC4D3-4AD9-11D5-AA6A-00E0189008B3}';

  // Name of the file class we wish to operate on.
  sFileClass1 = 'dllfile'; // .DLL
  sFileClass2 = 'exefile'; // .EXE
  sFileClass3 = 'tlbfile'; // .TLB

  // The extension would normally have been '.dll' (or '.exe' or '.tlb'), but
  // since we don't want to delete the original file registration when we are
  // uninstalled, we specify an empty string as the extension and thus disable
  // the registration and unregistration of the file type.
  sFileExtension1 = '';
  sFileExtension2 = '';
  sFileExtension3 = '.tlb';

  // Class name of our shell extension.
  sClassName = 'TRegSvrShellExt';

resourcestring
  // Description of our shell extension.
  sDescription = 'Drag and Drop Component Suite Context Menu demo';

  // File name replacement in case multiple files has been selected.
  sManyFiles = 'multiple files';

procedure TDataModuleContextMenuHandler.DataModuleCreate(Sender: TObject);
begin
  // We keep the bitmap in a resource in order to keep the size of the DFM file
  // down. The size of the RES file with the bitmap is 83Kb. The size of the DFM
  // file if it contained the bitmap would be ~3Mb.
  MenuAboutInfo.Bitmap.LoadFromResourceName(HInstance, 'BM_ABOUT');
end;

procedure TDataModuleContextMenuHandler.DropContextMenu1Popup(Sender: TObject);

  procedure ClearItem(Item: TMenuItem);
  begin
  {$IF CompilerVersion >= 5.0}
    Item.Clear;
  {$else}
    while (Item.Count > 0) do
      Item[0].Free;
  {$endif}
  end;

begin
  (*
  ** The TDropContextMenu.OnPopup even is executed when the user has selected
  ** one or more files in the explorer and right clicks on them.
  ** The TDropContextMenu.Files string list contains the selected files.
  *)

  // Insert selected filename into menu.
  if (DropContextMenu1.Files.Count = 1) then
  begin
    MenuRegister.Caption := Format(MenuRegister.Caption,
      [ExtractFileName(DropContextMenu1.Files[0])]);
    MenuUnregister.Caption := Format(MenuUnregister.Caption,
      [ExtractFileName(DropContextMenu1.Files[0])]);
  end else
  if (DropContextMenu1.Files.Count > 1) then
  begin
    MenuRegister.Caption := Format(MenuRegister.Caption, [sManyFiles]);
    MenuUnregister.Caption := Format(MenuUnregister.Caption, [sManyFiles]);
  end else
    ClearItem(PopupMenu1.Items);
end;

procedure TDataModuleContextMenuHandler.MenuRegisterClick(Sender: TObject);
begin
  (*
  ** Register menu item was clicked.
  *)
  DoRegister(raReg);
end;

procedure TDataModuleContextMenuHandler.MenuUnregisterClick(Sender: TObject);
begin
  (*
  ** Unregister menu item was clicked.
  *)
  DoRegister(raUnreg);
end;

procedure TDataModuleContextMenuHandler.MenuAboutInfoClick(
  Sender: TObject);
begin
  (*
  ** User has clicked on the About item.
  **
  ** Launch browser to direct user to our web site.
  *)
  Screen.Cursor := crAppStart;
  try
    Application.ProcessMessages; {otherwise cursor change will be missed}
    ShellExecute(0, nil, PChar('http://melander.dk/'), nil, nil, SW_NORMAL);
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TDataModuleContextMenuHandler.DoRegister(Action: TRegAction);
var
  i: integer;
  FileExt: string;
resourcestring
  sRegStr = 'Register COM server';
  sUnregStr = 'Unregister COM server';
  sRegSuccessful = '  Successfull';
  sRegFailed = '  Failed: %s';
const
  MsgTitle: array[TRegAction] of string = (sRegStr, sUnregStr);
begin
  Status := '';
  (*
  ** DropContextMenu1.Files contains the selected files. Process each file in
  ** turn.
  *)
  for i := 0 to DropContextMenu1.Files.Count-1 do
    try
      FileExt := ExtractFileExt(DropContextMenu1.Files[i]);
      if (FileExt = '') then
        continue;

      if (CompareText(FileExt, '.TLB') = 0) then
        // Register type library.
        RegisterTypeLib(DropContextMenu1.Files[i], Action)
      else if (CompareText(FileExt, '.EXE') = 0) then
        // Register COM server.
        RegisterExe(DropContextMenu1.Files[i], Action)
      else
        // Register DLL (e.g. ActiveX).
        RegisterActiveX(DropContextMenu1.Files[i], Action);

      OutputStr(sRegSuccessful);

    except
      (*
      ** We can't allow exceptions to escape from our shell extension since that
      ** could cause the Explorer to crash.
      *)
      on E: Exception do
        OutputStr(Format(sRegFailed, [E.Message]));
    end;

  // Display final result.
  if (Status <> '') then
    MessageBox(GetForegroundWindow, PChar(Status), PChar(MsgTitle[Action]),
      MB_ICONINFORMATION or MB_OK or MB_APPLMODAL);
end;

procedure TDataModuleContextMenuHandler.RegisterActiveX(const Filename: string;
  Action: TRegAction);
const
  ProcName: array[TRegAction] of PAnsiChar =
    ('DllRegisterServer', 'DllUnregisterServer');
resourcestring
  sActiveXName = 'ActiveX file: %s';
  sLoadFail = '  Failed to load library';
  sCantFindProc = '  %s procedure not found in library';
  sRegFail = '  Call to %s failed';
  sErrorUnknown = '  Unknown error';
type
  TRegProc = function : HResult; stdcall;
var
  RegProc: TRegProc;
  LibHandle: THandle;
begin
  (*
  ** Register COM server contained in a DLL file.
  *)

  OutputStr(Format(sActiveXName, [ExtractFileName(FileName)]));
  // Load DLL.
  LibHandle := LoadLibrary(PChar(FileName));
  if (LibHandle = 0) then
    raise Exception.Create(sLoadFail);
  try
    // Get pointer to entry point.
    @RegProc := GetProcAddress(LibHandle, ProcName[Action]);
    if (@RegProc = nil) then
      raise Exception.CreateFmt(sCantFindProc, [ProcName[Action]]);
    // Execute entry point.
    if (RegProc <> 0) then
      raise Exception.CreateFmt(sRegFail, [ProcName[Action]]);
  finally
    FreeLibrary(LibHandle);
  end;
end;

procedure TDataModuleContextMenuHandler.RegisterExe(const Filename: string;
  Action: TRegAction);
var
  SI: TStartupInfo;
  PI: TProcessInformation;
  RegisterExitCode: BOOL;
const
  ExeFlags: array[TRegAction] of string =
    (' /regserver', ' /unregserver');
resourcestring
  sExeName = 'EXE file: %s';
  sErrorUnknown = '  Unknown error';
begin
  (*
  ** Register COM server contained in an EXE file.
  *)

  OutputStr(Format(sExeName, [ExtractFileName(FileName)]));
  // Execute EXE with command line.
  FillChar(SI, SizeOf(SI), 0);
  SI.cb := SizeOf(SI);
  RegisterExitCode := Win32Check(CreateProcess(PChar(FileName),
    PChar(FileName+ExeFlags[Action]), nil, nil, True, 0, nil, nil, SI, PI));
  CloseHandle(PI.hThread);
  CloseHandle(PI.hProcess);
  if (not RegisterExitCode) then
    raise Exception.Create(sErrorUnknown);
end;

procedure TDataModuleContextMenuHandler.RegisterTypeLib(const Filename: string;
  Action: TRegAction);
type
  TUnRegTlbProc = function (const libID: TGUID; wVerMajor, wVerMinor: Word;
    lcid: TLCID; syskind: TSysKind): HResult; stdcall;
var
  WFileName, DocName: WideString;
  TypeLib: ITypeLib;
  LibAttr: PTLibAttr;
  OleAutLib: THandle;
  UnRegTlbProc: TUnRegTlbProc;
resourcestring
  sTlbName = 'Type library: %s';
  sTlbGuid = '  GUID: %s';
  sCantUnregTlb = '  The version of OLEAUT32.DLL on this machine does not '+
    'support type library unregistration.';
begin
  (*
  ** Register type library.
  *)

  OutputStr(Format(sTlbName, [ExtractFileName(FileName)]));
  WFileName := FileName;
  // Load type library and get an interface to it.
  OleCheck(LoadTypeLib(PWideChar(WFileName), TypeLib));
  // Get type library attributes.
  OleCheck(TypeLib.GetLibAttr(LibAttr));
  try
    OutputStr(Format(sTlbGuid, [GuidToString(LibAttr^.Guid)]));
    if (Action = raReg) then
    begin
      // Retrieves the path of the type library's help file.
      OleCheck(TypeLib.GetDocumentation(-1, nil, nil, nil, @DocName));
      DocName := ExtractFilePath(DocName);
      // Register type library.
      OleCheck(ActiveX.RegisterTypeLib(TypeLib, PWideChar(WFileName),
        PWideChar(DocName)));
    end
    else begin
      // Determine if the system supports type library unregistration (NT 4.0
      // SP4 or later).
      OleAutLib := GetModuleHandle('OLEAUT32.DLL');
      if (OleAutLib <> 0) then
        @UnRegTlbProc := GetProcAddress(OleAutLib, 'UnRegisterTypeLib')
      else
        @UnRegTlbProc := nil;
      if (@UnRegTlbProc = nil) then
        raise Exception.Create(sCantUnregTlb);
      // Unregister type library.
      with LibAttr^ do
        OleCheck(UnRegTlbProc(Guid, wMajorVerNum, wMinorVerNum, LCID, SysKind));
    end;
  finally
    TypeLib.ReleaseTLibAttr(LibAttr);
  end;
end;

procedure TDataModuleContextMenuHandler.OutputStr(const s: string);
begin
  // Add a string to the status.
  if (Status <> '') then
    Status := Status+#13+#10;
  Status := Status+s;
end;

procedure TDataModuleContextMenuHandler.MenuAboutInfoMeasureItem(
  Sender: TObject; ACanvas: TCanvas; var Width, Height: Integer);
begin
  (*
  ** Return size of menu item.
  **
  ** This menu item only displays a bitmap, so we just return the size of the
  ** bitmap. However, since the shell automatically adds 16 pixels to make room
  ** for menu bitmaps, we have to subtract those 16 pixels in order to make the
  ** menu item look nice. We also add room for a sunken bevel (2*2 pixels) and a
  ** small margin (2*3 pixels) between the edge of the menu item and the bevel.
  *)
  Width := MenuAboutInfo.Bitmap.Width-16+10;
  Height := MenuAboutInfo.Bitmap.Height+10;
  if (Win32Platform = VER_PLATFORM_WIN32_NT) and (Win32MajorVersion > 4) then
    Inc(Width, 4);
end;

procedure TDataModuleContextMenuHandler.MenuAboutInfoDrawItem(
  Sender: TObject; ACanvas: TCanvas; ARect: TRect; Selected: Boolean);
var
  Bitmap: TBitmap;
begin
  (*
  ** Draw the menu item.
  **
  ** In this simple case we just draw a bitmap and a few decorations (bevel
  ** etc.) which completely fills the menu item.
  *)

  // Clear the background to whatever color it should be (selected/unselected).
  // We rely on the brush already being set up to the correct color.
  ACanvas.FillRect(ARect);

  // Adjust rect for margin.
  InflateRect(ARect, -3, -3);

  // Draw the sunken bevel.
  DrawEdge(ACanvas.Handle, ARect, EDGE_SUNKEN, BF_RECT or BF_ADJUST);

  // We can't modify the menu items bitmap since it is owned and managed by
  // the menu item. Instead we create a temporary copy of it and mess with that
  // instead. If we didn't need to disable the bitmaps transparency we could just
  // have displayed the menu bitmap directly.
  Bitmap := TBitmap.Create;
  try
    Bitmap.Assign(MenuAboutInfo.Bitmap);

    // This particular bitmap does not need transparency.
    Bitmap.Transparent := False;

    // Finally draw the bitmap.
    if (ARect.Right-ARect.Left >= Bitmap.Width) and (ARect.Bottom-ARect.Top >= Bitmap.Height) then
      ACanvas.Draw(ARect.Left, ARect.Top, Bitmap)
    else
      ACanvas.StretchDraw(ARect, Bitmap)
  finally
    Bitmap.Free;
  end;
end;

initialization
  (*
  ** The object factory is responsible for creating the shell extension instance
  ** (when called by the shell) and for registering and unregistering the
  ** extension (when installing and uninstalling).
  **
  ** In order to associate the shell extension with 3 different file classes,
  ** we create three object factories. We could also have created a single
  ** object factory and then created the two remaining file class associations
  ** by updating the relevant registry entries manually.
  *)
  TDropContextMenuFactory.Create(ComServer, TDataModuleContextMenuHandler,
    CLSID_ContextMenuHandler, sClassName, sDescription, sFileClass1,
    sFileExtension1, ciMultiInstance);
  TDropContextMenuFactory.Create(ComServer, TDataModuleContextMenuHandler,
    CLSID_ContextMenuHandler, sClassName, sDescription, sFileClass2,
    sFileExtension2, ciMultiInstance);
  TDropContextMenuFactory.Create(ComServer, TDataModuleContextMenuHandler,
    CLSID_ContextMenuHandler, sClassName, sDescription, sFileClass3,
    sFileExtension3, ciMultiInstance);
end.

