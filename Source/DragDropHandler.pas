unit DragDropHandler;
// -----------------------------------------------------------------------------
// Project:         New Drag and Drop Component Suite
// Module:          DragDrop
// Description:     Implements base classes and utility functions.
// Version:         5.7
// Date:            28-FEB-2015
// Target:          Win32, Win64, Delphi 6-XE7
// Authors:         Anders Melander, anders@melander.dk, http://melander.dk
// Latest Version   https://github.com/landrix/The-new-Drag-and-Drop-Component-Suite-for-Delphi
// Copyright        © 1997-1999 Angus Johnson & Anders Melander
//                  © 2000-2010 Anders Melander
//                  © 2011-2015 Sven Harazim
// -----------------------------------------------------------------------------

interface

uses
  {$IF CompilerVersion >= 23.0}
  System.SysUtils,System.Classes,System.Win.ComObj,System.Win.Registry,
  WinApi.Windows,WinApi.ShlObj,WinApi.ActiveX,
  Vcl.Menus,
  {$ELSE}
  SysUtils,Classes,ComObj,Registry,
  Windows,ShlObj,ActiveX,
  Menus,
  {$ifend}
  DragDrop,
  DragDropComObj,
  DragDropContext;


//  DragDropFile,
//  DragDropPIDL;

{$include DragDrop.inc}

type
////////////////////////////////////////////////////////////////////////////////
//
//              TDragDropHandler
//
////////////////////////////////////////////////////////////////////////////////
//
// A typical drag-and-drop handler session goes like this:
//
// 1. User right-drags (drags with the right mouse button) and drops one or more
//    source files which has a registered drag-and-drop handler.
//
// 2. The shell loads the drag-and-drop handler module.
//
// 3. The shell instantiates the registered drag drop handler object as an
//    in-process COM server.
//
// 4. The IShellExtInit.Initialize method is called with the name of the target
//    folder and a data object which contains the dragged data.
//    The target folder name is stored in the TDragDropHandler.Folder
//    property as a string and in the FolderPIDL property as a PIDL.
//
// 5. The IContextMenu.QueryContextMenu method is called to populate the popup
//    menu. This fires the TDragDropHandler.OnPrepareMenu event.
//    TDragDropHandler uses the PopupMenu property to populate the drag-and-drop
//    context menu.
//
// 6. If the user chooses one of the context menu items we have supplied,
//    the IContextMenu.InvokeCommand method is called.
//
// 7. TDragDropHandler locates the corresponding TMenuItem and fires the menu
//    items OnClick event.
//
// 8. The shell unloads the context menu handler module (usually after a few
//    seconds if you're lucky).
//
////////////////////////////////////////////////////////////////////////////////
//
// Note that some version of the windows shell does not support owner draw and
// cascaded menus (IContextMenu and IContextMenu2 interfaces) for Drag Drop
// Handler shell extensions.
//
// The XP shell appears to support cascaded menus, but not owner draw.
//
////////////////////////////////////////////////////////////////////////////////
  TDragDropHandler = class(TDropContextMenu, IShellExtInit, IContextMenu,
    IContextMenu2, IContextMenu3)
  private
  protected
    function GetSupportsOwnerDraw: boolean; override;
    { IShellExtInit }
    function Initialize(pidlFolder: PItemIDList; lpdobj: IDataObject;
      hKeyProgID: HKEY): HResult; stdcall;
  public
  end;

////////////////////////////////////////////////////////////////////////////////
//
//              TDragDropHandlerFactory
//
////////////////////////////////////////////////////////////////////////////////
// COM Class factory for TDragDropHandler.
////////////////////////////////////////////////////////////////////////////////
  TDragDropHandlerFactory = class(TDropContextMenuFactory)
  protected
    function HandlerRegSubKey: string; override;
  end;

////////////////////////////////////////////////////////////////////////////////
//
//              Misc.
//
////////////////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
//
//              	IMPLEMENTATION
//
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
implementation

//uses
//  DragDropFile,
//  DragDropPIDL;

////////////////////////////////////////////////////////////////////////////////
//
//              Utilities
//
////////////////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////////////
//
//              TDragDropHandler
//
////////////////////////////////////////////////////////////////////////////////
function TDragDropHandler.GetSupportsOwnerDraw: boolean;
begin
  Result := False;
end;

function TDragDropHandler.Initialize(pidlFolder: PItemIDList;
  lpdobj: IDataObject; hKeyProgID: HKEY): HResult;
begin
  if (pidlFolder <> nil) then
    Result := inherited Initialize(pidlFolder, lpdobj, hKeyProgID)
  else
    Result := E_INVALIDARG;
end;

////////////////////////////////////////////////////////////////////////////////
//
//              TDragDropHandlerFactory
//
////////////////////////////////////////////////////////////////////////////////
function TDragDropHandlerFactory.HandlerRegSubKey: string;
begin
  Result := 'DragDropHandlers';
end;

end.
