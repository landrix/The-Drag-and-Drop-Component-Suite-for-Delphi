unit ContextMenuHandlerMain;

interface

uses
  DragDrop, DropTarget, DragDropContext,
  Forms, ShlObj, SysUtils, Classes, Menus, Graphics, Windows, ImgList,
  Controls;

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
    N1: TMenuItem;
    test1: TMenuItem;
    submenu1: TMenuItem;
    Item1: TMenuItem;
    Item21: TMenuItem;
    N3: TMenuItem;
    Itemwithglyphfrombitmap1: TMenuItem;
    Itemwithglyphfromimagelist1: TMenuItem;
    Disableditem1: TMenuItem;
    Disableditem2: TMenuItem;
    N4: TMenuItem;
    Checkeditem1: TMenuItem;
    Radioitem1: TMenuItem;
    Breakitem1: TMenuItem;
    Barbreakitem1: TMenuItem;
    Glyphfromimagelist1: TMenuItem;
    N2: TMenuItem;
    ImageList1: TImageList;
    Itemwithaccelerator1: TMenuItem;
    Anotheritemwithanaccelerator1: TMenuItem;
    Yetanotheritemwithyouknowwhat1: TMenuItem;
    Followedbyaregularitem1: TMenuItem;
    Anthelastitem1: TMenuItem;
    procedure MenuTest1Click(Sender: TObject);
  private
  protected
  public
    // Aggregate IShellExtInit and IContextMenu* to the TDropContextMenu component.
    property ContextMenuHandler: TDropContextMenu read DropContextMenu1
      implements IShellExtInit, IContextMenu, IContextMenu2, IContextMenu3;
  end;

implementation

{$R *.DFM}

uses
  ComServ,
  ComObj,
  Dialogs;

const
  // CLSID for this shell extension.
  // Modify this for your own shell extensions (press [Ctrl]+[Shift]+G in
  // the IDE editor to gererate a new CLSID).
  CLSID_ContextMenuHandler: TGUID = '{F59A4B51-F67F-11D5-AC8C-00E0189008B3}';

  // Name of the file class we wish to operate on.
  sFileClass = '*'; // All files

  (*
  ** See MSDN for other "Predefined Shell Objects". Here's a a few usefull ones:
  **
  **   Class                      Description
  **   -------------------------------------------------------------------------
  **   *                          All files
  **   Folder                     All folders
  **   AllFileSystemObjects       All files and file folders
  **   Directory                  File folder
  **   Directory\Background       Desktop and Explorer background
  **   Printers                   All printers
  *)

  // Normally we would have specified the file extension of the file class our
  // shell extension should apply to, but since this demo applies to a
  // Predefined Shell Object and we don't want to delete the original file
  // registration when we are uninstalled, we specify an empty string as the
  // extension and thus disable the registration and unregistration of the file
  // type.
  sFileExtension = '';

  // Class name of our shell extension.
  sClassName = 'SimpleContextMenuDemoShellExt';

resourcestring
  // Description of our shell extension.
  sDescription = 'Drag and Drop Component Suite Simple Context Menu demo';

procedure TDataModuleContextMenuHandler.MenuTest1Click(Sender: TObject);
begin
  ShowMessage('You clicked on "'+TMenuItem(Sender).Caption+
    '" with the following files selected:'+#13+#13+
    DropContextMenu1.Files.Text);
end;

initialization
  (*
  ** The object factory is responsible for creating the shell extension instance
  ** (when called by the shell) and for registering and unregistering the
  ** extension (when installing and uninstalling).
  *)
  TDropContextMenuFactory.Create(ComServer, TDataModuleContextMenuHandler,
    CLSID_ContextMenuHandler, sClassName, sDescription, sFileClass,
    sFileExtension, ciMultiInstance);
end.

