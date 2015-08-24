program TreeViewTarget;

uses
  Forms,
  main in 'main.pas' {FormTreeViewDemo};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormTreeViewDemo, FormTreeViewDemo);
  Application.Run;
end.
