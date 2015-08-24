program SimpleTargetDemo;

{%File 'readme.txt'}

uses
  Forms,
  Main in 'Main.pas' {Form1};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Simple Target Demo';
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
