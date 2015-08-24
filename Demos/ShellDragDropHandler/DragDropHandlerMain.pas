unit DragDropHandlerMain;

interface

uses
  DragDrop, DropTarget, DragDropHandler, DragDropContext,
  Forms, ShlObj, SysUtils, Classes, Menus;

{$include 'DragDrop.inc'}

//{$ifndef VER13_PLUS}
//type
//  TDataModule = TForm;
//{$endif}

type
  (*
  ** The data module implements our shell extension and must support all the
  ** nescessary interfaces.
  **
  ** IUnknown is implemented by TComponent. The rest is implemented by the
  ** TDragDropHandler component through delegation.
  **
  ** Note that it is very important to include IUnknown in the interface list
  ** in order to get reference counting working properly.
  *)
  TDataModuleDragDropHandler = class(TDataModule, IUnknown, IShellExtInit,
    IContextMenu, IContextMenu2, IContextMenu3)
    PopupMenu1: TPopupMenu;
    MenuEncrypt: TMenuItem;
    MenuLine1: TMenuItem;
    DragDropHandler1: TDragDropHandler;
    procedure MenuEncryptClick(Sender: TObject);
    procedure DragDropHandler1Popup(Sender: TObject);
  private
    procedure EncryptFile(const Filename: string);
  public
    // Aggregate IShellExtInit and IContextMenu to the TDragDropHandler component.
    property DragDropHandler: TDragDropHandler read DragDropHandler1
      implements IShellExtInit, IContextMenu, IContextMenu2, IContextMenu3;
  end;

implementation

{$R *.DFM}

uses
  Windows,
  ComServ,
  ComObj,
  Registry;

const
  // CLSID for this shell extension.
  // Modify this for your own shell extensions (press [Ctrl]+[Shift]+G in
  // the IDE editor to gererate a new CLSID).
  CLSID_DragDropHandler: TGUID = '{6DC77C71-B699-11D5-ABC4-00E0189008B3}';

resourcestring
  // Name of the file class we wish to operate on.
  // Drag Drop handler shell extensions normally always operate on directories.
  // In order to extend the support to the desktop and root directories we
  // should also register under the "Folder" and "Drive" file classes. For
  // simplicity we don't do that in this example.
  sFileClass = 'Directory';
  // We do not need to register a file extension, so we specify an empty string
  // as the extension and thus disable the registration and unregistration of
  // the file type.
  sFileExtension = '';

  // Class name of our shell extension.
  sClassName = 'SimpleEncrypter';
  // Description of our shell extension.
  sDescription = 'Drag and Drop Component Suite Drag Drop Handler demo';

  // File name replacement in case multiple files has been selected.
  sManyFiles = 'multiple files';

procedure TDataModuleDragDropHandler.EncryptFile(const Filename: string);
var
  Source, Target: TFileStream;
  Buffer: array[0..255] of byte;
  Size: integer;
  i: integer;
begin
  // This is just a very simple example of a reversible encryption/decryption.
  // For simplicity we just XOR all the bytes of the source file with a constant
  // value. Another alternative could have been ROT13.
  Source := TFileStream.Create(Filename, fmOpenRead);
  try
    // Create a new target file with the same file type, but with '.xxx'
    // inserted before the extension.
    Target := TFileStream.Create(
      ChangeFileExt(Filename, '.xxx'+ExtractFileExt(Filename)), fmCreate);
    try
      repeat
        // Read chunk from source.
        Size := Source.Read(Buffer, SizeOf(Buffer));

        // XOR all bytes in chunk.
        for i := 0 to Size-1 do
          Buffer[i] := Buffer[i] XOR $AA;

        // Write the encryted data to the target.
        Target.Write(Buffer, Size);
      until (Size = 0);
    finally
      Target.Free;
    end;
  finally
    Source.Free;
  end;
end;

procedure TDataModuleDragDropHandler.MenuEncryptClick(Sender: TObject);
var
  i: integer;
begin
  // Encrypt each file in turn.
  for i := 0 to DragDropHandler1.Files.Count-1 do
    EncryptFile(DragDropHandler1.Files[i]);
end;

procedure TDataModuleDragDropHandler.DragDropHandler1Popup(Sender: TObject);

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
  // TDragDropHandler component now contains the files being dragged.

  // Insert source filename(s) into menu.
  if (DragDropHandler1.Files.Count = 1) then
    MenuEncrypt.Caption := Format(MenuEncrypt.Caption, [ExtractFileName(DragDropHandler1.Files[0])])
  else
  if (DragDropHandler1.Files.Count > 1) then
    MenuEncrypt.Caption := Format(MenuEncrypt.Caption, [sManyFiles])
  else
    ClearItem(PopupMenu1.Items);
end;

initialization
  (*
  ** The object factory is responsible for creating the shell extension instance
  ** (when called by the shell) and for registering and unregistering the
  ** extension (when installing and uninstalling).  
  *)
  TDragDropHandlerFactory.Create(ComServer, TDataModuleDragDropHandler,
    CLSID_DragDropHandler, sClassName, sDescription, sFileClass,
    sFileExtension, ciMultiInstance);
end.

