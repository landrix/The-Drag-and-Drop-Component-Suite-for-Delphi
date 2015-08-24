unit DropHandler;
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
  System.Classes,System.Win.ComObj,
  WinApi.Windows,WinApi.ActiveX,
  {$ELSE}
  Classes,ComObj,
  Windows,Messages,ActiveX,CommCtrl,
  {$ifend}
  DragDrop,
  DropTarget,
  DragDropFile,
  DragDropComObj;

{$include DragDrop.inc}

type
////////////////////////////////////////////////////////////////////////////////
//
//              TDropHandler
//
////////////////////////////////////////////////////////////////////////////////
// Based on Angus Johnson's DropHandler demo.
////////////////////////////////////////////////////////////////////////////////
// A typical drop handler session goes like this:
//
// 1. User drags one or more source files over a target file which has a
//    registered drop handler.
//
// 2. The shell loads the drop handler module.
//
// 3. The shell instantiates the registered drop handler object as an in-process
//    COM server.
//
// 4. The IPersistFile.Load method is called with the name of the target file.
//    The target file name is stored in the TDropHandler.TargetFile property.
//
// 5. The IDropTarget.Enter method is called. This causes a TDropHandler.OnEnter
//    event to be fired.
//
// 6. One of two things can happen next:
//    a) The user drops the source files on the target file.
//       The IDropTarget.Drop method is called. This causes a
//       TDropHandler.OnDrop event to be fired.
//       The names of the dropped files are stored in the TDropHandler.Files
//       string list property.
//    b) The user drags the source files away from the target file.
//       The IDropTarget.Leave method is called. This causes a
//       TDropHandler.OnLeave event to be fired.
//
// 7. The shell unloads the drop handler module (usually after a few seconds).
//
////////////////////////////////////////////////////////////////////////////////
  TDropHandler = class(TDropFileTarget, IPersistFile)
  private
    FTargetFile: string;
  protected
    // IPersistFile implementation
    function GetClassID(out classID: TCLSID): HResult; stdcall;
    function IsDirty: HResult; stdcall;
    function Load(pszFileName: POleStr; dwMode: Longint): HResult; stdcall;
    function Save(pszFileName: POleStr; fRemember: BOOL): HResult; stdcall;
    function SaveCompleted(pszFileName: POleStr): HResult; stdcall;
    function GetCurFile(out pszFileName: POleStr): HResult; stdcall;
  public
    property TargetFile: string read FTargetFile;
  end;

////////////////////////////////////////////////////////////////////////////////
//
//              TDropHandlerFactory
//
////////////////////////////////////////////////////////////////////////////////
// COM Class factory for TDropHandler.
////////////////////////////////////////////////////////////////////////////////
  TDropHandlerFactory = class(TShellExtFactory)
  protected
  public
    procedure UpdateRegistry(Register: Boolean); override;
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

////////////////////////////////////////////////////////////////////////////////
//
//              Utilities
//
////////////////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////////////
//
//              TDropHandler
//
////////////////////////////////////////////////////////////////////////////////
function TDropHandler.GetClassID(out classID: TCLSID): HResult;
begin
  result := E_NOTIMPL;
end;

function TDropHandler.GetCurFile(out pszFileName: POleStr): HResult;
begin
  result := E_NOTIMPL;
end;

function TDropHandler.IsDirty: HResult;
begin
  result := S_FALSE;
end;

function TDropHandler.Load(pszFileName: POleStr; dwMode: Integer): HResult;
begin
  FTargetFile := WideCharToString(pszFileName);
  result := S_OK;
end;

function TDropHandler.Save(pszFileName: POleStr; fRemember: BOOL): HResult;
begin
  result := E_NOTIMPL;
end;

function TDropHandler.SaveCompleted(pszFileName: POleStr): HResult;
begin
  result := E_NOTIMPL;
end;


////////////////////////////////////////////////////////////////////////////////
//
//              TDropHandlerFactory
//
////////////////////////////////////////////////////////////////////////////////
procedure TDropHandlerFactory.UpdateRegistry(Register: Boolean);
begin
  if Register then
  begin
    inherited UpdateRegistry(Register);
    CreateRegKey(FileClass+'\shellex\DropHandler', '', GUIDToString(ClassID));
  end else
  begin
    DeleteDefaultRegValue(FileClass+'\shellex\DropHandler');
    DeleteEmptyRegKey(FileClass+'\shellex\DropHandler', True);
    inherited UpdateRegistry(Register);
  end;
end;

end.
