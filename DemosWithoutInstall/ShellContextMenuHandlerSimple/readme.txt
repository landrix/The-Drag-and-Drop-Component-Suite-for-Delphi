SimpleContextMenuHandlerShellExt

This application demonstrates how to create a simple Context Menu Handler shell
extension using the TDropContextMenu component.

Description of Context Menu Handlers from MSDN:
  When a user right-clicks a shell object, the shell displays its context menu.
  For file system objects there are a number of standard items, such as Cut and
  Copy, that are on the menu by default. If the object is a file that is a
  member of a class, additional items can be specified in the registry. Finally,
  the shell checks the registry to see if the file class is associated with any
  context menu handlers. If it is, the shell queries the handler for additional
  context menu items.

Even though shell context menu handlers aren't directly associated with drag and
drop, they both use many of the same interfaces, data formats and mechanisms.
Since TDropContextMenu is actually a subset of the TDragDropHandler component,
it requires almost no additional code to implement a shell context menu
handler.

To use this demo, compile the project and register the DLL.
You can use one of the following methods to register the DLL:

  - From within the Delphi IDE, select "Register ActiveX Server" from the "Run"
    menu.

  - Use the regsvr32.exe tool provided by Microsoft.
    Execute the following command line:

      regsvr32 ContextMenuHandlerShellExt.dll

  - Use the tregsvr.exe tool provided by Borland.
    Execute the following command line:

      tregsvr ContextMenuHandlerShellExt.dll

Once the DLL has been registered, the menu items, defined in the TPopupMenu
component associated with the TDropContextMenu component, should appear when you
right click on any file in Explorer or on the desktop.

You can use one of the following methods to unregister the DLL once you are done
with it:

  - From within the Delphi IDE, select "Unregister ActiveX Server" from the
    "Run" menu.

  - Use the regsvr32.exe tool provided by Microsoft.
    Execute the following command line:

      regsvr32 /u ContextMenuHandlerShellExt.dll

  - Use the tregsvr.exe tool provided by Borland.
    Execute the following command line:

      tregsvr -u ContextMenuHandlerShellExt.dll


