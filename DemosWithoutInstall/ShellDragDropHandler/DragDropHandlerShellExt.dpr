// Note: If the Delphi IDE inserts a "{$R *.TLB}" here, just delete it again.
// This project does not use a type library.
library DragDropHandlerShellExt;

{%File 'readme.txt'}

uses
  ComServ,
  DragDropHandlerMain in 'DragDropHandlerMain.pas' {DataModuleDragDropHandler: TDataModule};

exports
  DllGetClassObject,
  DllCanUnloadNow,
  DllRegisterServer,
  DllUnregisterServer;

begin
end.
