program PIDLDemo;

{%File 'readme.txt'}

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  PathComboBox in 'PathComboBox.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'TDropPIDL Source demo';
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

