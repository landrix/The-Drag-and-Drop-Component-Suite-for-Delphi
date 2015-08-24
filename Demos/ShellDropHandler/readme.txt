DropHandlerShellExt and FoobarEdit

This application demonstrates how to create a Drop Handler shell extension
using the TDropHandler component.

Description of Drop Handlers from MSDN:
  By default, files are not drop targets. You can make the members of a file
  class into drop targets by implementing and registering a drop handler.
  If a drop handler is registered for a file class, it is called whenever an
  object is dragged over or dropped on a member of the class. The shell manages
  the operation by calling the appropriate methods on the handler's IDropTarget
  interface.

To use this demo, compile the DropHandlerShellExt demo project and register the
DLL. You can use one of the following methods to register the DLL:

  - From within the Delphi IDE, select "Register ActiveX Server" from the "Run"
    menu.

  - Use the regsvr32.exe tool provided by Microsoft.
    Execute the following command line:

      regsvr32 DropHandlerShellExt.dll

  - Use the tregsvr.exe tool provided by Borland.
    Execute the following command line:

      tregsvr DropHandlerShellExt.dll

Once the DLL has been registered, compile and run the FoobarEdit application.

From the file menu select "Save as..." and specify a filename (file extension
must be ".foobar"). Once the file has been saved, exit the FoobarEdit
application again.

Now select a couple of files in the explorer and drop them on the .foobar file
you just saved. This should cause the Drop Handler shell extension to launch the
FoobarEdit application, load the .foobar file into it and add the dropped files
to the .foobar file.

You can use one of the following methods to unregister the DLL once you are done
with it:

  - From within the Delphi IDE, select "Unregister ActiveX Server" from the
    "Run" menu.

  - Use the regsvr32.exe tool provided by Microsoft.
    Execute the following command line:

      regsvr32 /u DropHandlerShellExt.dll

  - Use the tregsvr.exe tool provided by Borland.
    Execute the following command line:

      tregsvr -u DropHandlerShellExt.dll

