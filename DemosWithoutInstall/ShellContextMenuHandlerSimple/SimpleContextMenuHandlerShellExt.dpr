// Note: If the Delphi IDE inserts a "{$R *.TLB}" here, just delete it again.
// This project does not use a type library.
library SimpleContextMenuHandlerShellExt;

{%File 'readme.txt'}

uses
  ComServ,
  ContextMenuHandlerMain in 'ContextMenuHandlerMain.pas' {DataModuleContextMenuHandler: TDataModule};

exports
  DllGetClassObject,
  DllCanUnloadNow,
  DllRegisterServer,
  DllUnregisterServer;

begin
end.
