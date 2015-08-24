// Note: If the Delphi IDE inserts a "{$R *.TLB}" here, just delete it again.
// This project does not use a type library.
library DropHandlerShellExt;

{%File 'readme.txt'}

uses
  ComServ,
  DropHandlerMain in 'DropHandlerMain.pas' {DataModuleDropHandler: TDataModule};

exports
  DllGetClassObject,
  DllCanUnloadNow,
  DllRegisterServer,
  DllUnregisterServer;

begin
end.
