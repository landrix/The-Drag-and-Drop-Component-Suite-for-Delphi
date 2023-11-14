The Drag and Drop Component Suite for Delphi
============================================

This component suite based on The Drag and Drop Component Suite for Delphi from Anders Melander    
http://melander.dk/delphi/dragdrop  

The Drag and Drop Component Suite Version 5.84 Released 11-nov-2023

© 1997-2010 Anders Melander  
© 2011-2023 Sven Harazim

https://github.com/landrix/The-Drag-and-Drop-Component-Suite-for-Delphi  

## Description

The Drag and Drop Component Suite is a freeware VCL component library that enables your Delphi and C++Builder applications to support COM based drag and drop and integrate with the Windows clipboard.

The drag and drop system that is built into the VCL, is limited in that it only supports drag and drop within the same application. If you need to drag data from your application to other applications (e.g. Word, Explorer or Outlook), or if you need to be able to accept data dropped from other application (e.g. the Explorer), you have to use COM based drag and drop. COM based drag and drop is an integral and very important part of the Windows user interface and the Drag and Drop Component Suite makes it very easy to leverage all the features of COM based drag and drop in your own Delphi and C++Builder applications.

Every drag and drop operation involves two objects: A drop source and a drop target. The drop source provides the data to be dragged, and the drop target accepts the dragged data.
Likewise there are basically two sets of components in the Drag and Drop Component Suite; Drop source components and drop target components. Most of the source and target components are specialized to handle just one type of data, but a few of the components supports a wider range of data types or are completely generic.

In addition to the drag and drop components, the Drag and Drop Component Suite also includes components that can be used to build Windows Shell Extensions. While these components aren’t all related to Drag and Drop, they benefit from the Drag and Drop Component Suite framework and allow you to write Windows Shell Extensions with very little code. But most important; I had a lot of fun writing them :-).

## Table of Contents
1. Supported platforms
2. Installation
3. Uninstallation
4. Known problems

## 1. Supported platforms
This version of the library has been tested with Delphi 6, Delphi 7, Delphi 2007,
Delphi 2009, Delphi 2010, Delphi XE, Delphi XE2, Delphi XE3, Delphi XE4, Delphi XE5,
Delphi XE6, Delphi XE7, Delphi XE8 (Win32,Win64), Delphi 10 Seattle (Win32,Win64),
Delphi 10.1 Berlin (Win32,Win64),Delphi 10.2 Tokyo (Win32,Win64), Delphi 10.3 Rio (Win32,Win64),
Delphi 10.4 Sydney (Win32,Win64), Delphi 11, Delphi 12

## 2. Installation
1. If you are using a previous version of the Drag and Drop Component Suite, uninstall that
   version first.

2. Unzip the package to a folder of your choice.

3. In the Packages folder, find the design time package that matches your version of Delphi.
   Open it in Delphi, Compile and Install.

4. Locate the Library sub-folder that matches your version of Delphi. Add it to the Delphi
   library search path.

5. Optional: Add the Source folder to the Delphi browsing path.

## 3. Uninstallation
1. Open Delphi and uninstall the Drag and Drop Component Suite design time package.

2. Remove the Library folder from the Delphi library search path.

3. Remove the Source folder from the Delphi browsing path.

4. Locate the folder where you installed The Drag and Drop Component Suite and delete that
   folder.

## 4. Known problems
* When the demo applications are compiled with Delphi 7, some of them
  will probably emit a lot of "Unsafe type", "Unsafe code" etc. warnings.
  This doesn't mean that there's anything wrong with the demos. It just
  means that Borland, at that time, wanted you to know that they were
  moving to .NET and would like you to do the same (so you can buy the
  next version of Delphi).
  You can turn the warnings of in the project options.

* Delphi's and C++Builder's HWND and THandle types are not compatible.
  For this reason it might be nescessary to cast C++Builder's HWND values to
  Delphi's THandle type when a HWND is passed to a function. E.g.:

````delphi
if (DragDetectPlus(reinterpret_cast<THandle>(Handle), Point(X, Y))) {
  ...
}
````

* Virtual File Stream formats can only be pasted from the clipboard with live
  data (i.e. FlushClipboard/OleFlushClipboard hasn't been called on the data
  source). This problem affects TFileContentsStreamOnDemandClipboardFormat and
  the VirtualFileStream demo.  
  This is believed to be a bug in the Windows clipboard and a work around hasn't
  been found yet.

* When TDropFileTarget.GetDataOnEnter is set to True, the component doesn't work
  with WinZip.  
  Although the file names are received correctly by TDropFileTarget, WinZip
  doesn't extract the files and the files thus can't be copied/moved.
  This is caused by a quirk in WinZip; Apparently WinZip doesn't like
  IDataObject.GetData to be called before IDropTarget.Drop is called.
  
  
About
=====
  
The Drag and Drop Component Suite is a freeware VCL component library that 
enables your Delphi and C++Builder applications to support COM based drag and 
drop and integrate with the Windows clipboard.

The drag and drop system that is built into the VCL is limited in that it 
only supports drag and drop within the same application. If you need to drag 
data from your application to other applications (e.g. Word, Explorer or Outlook), 
or if you need to be able to accept data dropped from other application 
(e.g. the Explorer), you have to use COM based drag and drop. COM based drag 
and drop is an integral and very important part of the Windows user interface 
and the Drag and Drop Component Suite makes it very easy to leverage all the 
features of COM based drag and drop in your own Delphi and C++Builder applications.

Every drag and drop operation involves two objects: A drop source and a drop 
target. The drop source provides the data to be dragged, and the drop target 
accepts the dragged data.

Likewise there are basically two sets of components in the Drag and Drop 
Component Suite; Drop source components and drop target components. Most 
of the source and target components are specialized to handle just one type 
of data, but a few of the components supports a wider range of data types 
or are completely generic.

In addition to the drag and drop components, the Drag and Drop Component 
Suite also includes components that can be used to build Windows Shell 
Extensions. While these components arenâ€™t all related to Drag and Drop, 
they benefit from the Drag and Drop Component Suite framework and allow you 
to write Windows Shell Extensions with very little code. But most important; 
I had a lot of fun writing them :-).  

Features
--------
* Freeware with full source code included.
* Enables COM drag-and-drop of any kind of data between applications.
* Copy, Move and Link operations.
* Clipboard Copy, Cut and Paste support.
* Support and implement advanced features such as Delete-on-paste and Optimized Move.
* Drag image support.
* Supports both Ansi and Unicode data formats, regardless of the version of Delphi used.
* Supports asynchronous transfer. Both on the source and target side.
* Automatic scrolling of the target window during the drag operation.
* Relatively simple to derive custom drag-and-drop components to support other data formats.
* Extensive, context sensitive help file and detailed demo applications.
* Implements the IDropSource, IDropTarget, IDataObject and IAsyncOperation interfaces.
* Compatible with Delphi 5 through Delphi 2010.


Components
----------
The Drag and Drop Component Suite contains the following components:

Drop Sources
* TDropFileSource Enables you to drag files and shortcuts from your application to other applications.
* TDropTextSource Enables you to drag text from your application to other applications.
* TDropBMPSource Enables you to drag bitmaps from your application to other applications.
* TDropURLSource Enables you to drag URLs from your application to other applications.
* TDropPIDLSource Enables you to drag PIDLs (files, folders, shell objects) from your application to other  applications.
* TDropEmptySource A basic drop source component that does not support any data formats on its own. Can be extended with support for any data format through the use of a TDataFormatAdapter component.

Drop Targets
* TDropFileTarget Enables your application to accept files and shortcuts dropped on it from other applications.
* TDropTextTarget Enables your application to accept text dropped on it from other applications.
* TDropBMPTarget Enables your application to accept bitmaps dropped on it from other applications.
* TDropMetaFileTarget Enables your application to accept meta files dropped on it from other applications.
* TDropURLTarget Enables your application to accept URLs dropped on it from other applications.
* TDropComboTarget Enables your application to accept files, shortcuts, text, URLs, meta files, and bitmaps dropped on it from other applications. The component is basically all the above components rolled into one.
* TDropImageTarget Enables your application to accept meta files, and bitmaps dropped on it from other applications. The component is basically the TDropBMPTarget and the TDropMetaFileTarget components rolled into one.
* TDropPIDLTarget Enables your application to accept PIDLs (files, folder, shell objects) dropped on it from other applications.
* TDropEmptyTarget A basic drop target component that does not support any data formats on its own. Can be extended with support for any data format through the use of a TDataFormatAdapter component.

Shell Extensions	 	
* TDropHandler Implements the necessary interfaces required to build Drop Handler shell extensions. A Drop Handler shell extension is used to turn file classes (e.g. all files with a specific extension) into drop targets.
* TDragDropHandler Implements the necessary interfaces required to build DragDrop Handler shell extensions. A Drop Handler shell extension is used to extend the popup menu which is displayed by the shell when a file is dragged with the right mouse button.
* TDropContextMenu Implements the necessary interfaces required to build Context Menu Handler shell extensions. A Context Menu Handler is used to extend the shell popup menu which is displayed when a file is right-clicked.

Additional
* TDataFormatAdapter Used to extends source or target components with support for arbitrary clipboard formats.
* TDropDummy Helper component used to display drag images.

Delphinus-Support
