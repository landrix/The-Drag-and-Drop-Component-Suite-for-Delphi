unit DragDropDesign;
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
// TODO : Default event for target components should be OnDrop.
// TODO : Add parent form to Target property editor list.

interface

{$include DragDrop.inc}

procedure Register;

implementation

uses
  {$IF CompilerVersion >= 23.0}
  System.Classes,
  {$else}
  Classes,
  {$ifend}
  {$IF CompilerVersion < 14.0}
  DsgnIntf,
  {$else}
  DesignIntf,
  DesignEditors,
  {$ifend}
  DragDrop,
  DropSource,
  DropTarget,
  DragDropFile,
  DragDropGraphics,
  DragDropContext,
  DragDropHandler,
  DropHandler,
  DragDropInternet,
  DragDropPIDL,
  DragDropText,
  DropComboTarget;

type
  TDataFormatNameEditor = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

////////////////////////////////////////////////////////////////////////////////
//
//              Component and Design-time editor registration
//
////////////////////////////////////////////////////////////////////////////////
procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(string), TDataFormatAdapter, 'DataFormatName',
    TDataFormatNameEditor);
  RegisterComponents(DragDropComponentPalettePage,
    [TDropEmptySource, TDropEmptyTarget, TDropDummy, TDataFormatAdapter,
    TDropFileTarget, TDropFileSource, TDropBMPTarget, TDropBMPSource,
    TDropMetaFileTarget, TDropImageTarget, TDropURLTarget, TDropURLSource,
    TDropPIDLTarget, TDropPIDLSource, TDropTextTarget, TDropTextSource,
    TDropComboTarget]);
  RegisterComponents(DragDropComponentPalettePage,
    [TDropHandler, TDragDropHandler, TDropContextMenu]);
end;

{ TDataFormatNameEditor }

function TDataFormatNameEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList, paMultiSelect];
end;

procedure TDataFormatNameEditor.GetValues(Proc: TGetStrProc);
var
  i: Integer;
begin
  for i := 0 to TDataFormatClasses.Count-1 do
    Proc(TDataFormatClasses.Formats[i].ClassName);
end;

end.
