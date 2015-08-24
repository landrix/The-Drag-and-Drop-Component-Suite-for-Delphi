program VirtualFile;

{%File 'readme.txt'}

uses
  Forms,
  Main in 'Main.pas' {FormMain};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
