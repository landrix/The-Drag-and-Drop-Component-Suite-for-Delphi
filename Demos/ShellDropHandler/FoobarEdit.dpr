program FoobarEdit;

{%File 'readme.txt'}

uses
  Forms,
  FoobarMain in 'FoobarMain.pas' {FormFileList};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'FoobarEdit - Drop Handler Demo application';
  Application.CreateForm(TFormFileList, FormFileList);
  Application.Run;
end.
