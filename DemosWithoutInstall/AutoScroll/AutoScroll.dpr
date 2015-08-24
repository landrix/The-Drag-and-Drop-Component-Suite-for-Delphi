program AutoScroll;

{%File 'readme.txt'}

uses
  Forms,
  main in 'main.pas' {FormAutoScroll};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormAutoScroll, FormAutoScroll);
  Application.Run;
end.
