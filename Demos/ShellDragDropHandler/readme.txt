DragDropHandlerShellExt

This application demonstrates how to create a Drag Drop Handler shell extension
using the TDragDropHandler component.

Description of Drag Drop Handlers from MSDN:
  When a user right-clicks a shell object to drag an object, a context menu is
  displayed when the user attempts to drop the object. A drag-and-drop handler
  is a context menu handler that can add items to this context menu.

To use this demo, compile the project and register the DLL.
You can use one of the following methods to register the DLL:

  - From within the Delphi IDE, select "Register ActiveX Server" from the "Run"
    menu.

  - Use the regsvr32.exe tool provided by Microsoft.
    Execute the following command line:

      regsvr32 DragDropHandlerShellExt.dll

  - Use the tregsvr.exe tool provided by Borland.
    Execute the following command line:

      tregsvr DragDropHandlerShellExt.dll

Once the DLL has been registered, drag a .txt file with the right mouse
button and drop it somewhere. The normal drag/drop popup menu should appear with
a custom "Encrypt" menu item added to it. If you select the "Encrypt" menu item,
the dragged file will be copied to the destination and encrypted. The original
file will not be modified. To unencrypt the dropped file again, just repeat the
process.

You can use one of the following methods to unregister the DLL once you are done
with it:

  - From within the Delphi IDE, select "Unregister ActiveX Server" from the
    "Run" menu.

  - Use the regsvr32.exe tool provided by Microsoft.
    Execute the following command line:

      regsvr32 /u DragDropHandlerShellExt.dll

  - Use the tregsvr.exe tool provided by Borland.
    Execute the following command line:

      tregsvr -u DragDropHandlerShellExt.dll


Note that some version of the windows shell does not support owner draw and
cascaded menus (IContextMenu and IContextMenu2 interfaces) for Drag Drop
Handler shell extensions.

The Windows XP shell appears to support cascaded menus, but not owner draw.

These components presently required owner-draw is to display bitmaps in cascaded
menus.
