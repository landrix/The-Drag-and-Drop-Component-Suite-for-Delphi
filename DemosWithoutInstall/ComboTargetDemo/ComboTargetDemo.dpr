program ComboTargetDemo;

{%File 'readme.txt'}

uses
  Forms,
  main in 'main.pas' {FormMain};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
