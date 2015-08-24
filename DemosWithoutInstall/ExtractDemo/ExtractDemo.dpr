program ExtractDemo;

{%File 'readme.txt'}

uses
  Forms,
  Unit1 in 'Unit1.pas' {FormMain};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Extract Demo';
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.

