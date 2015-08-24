program DragDropDemo;
                   
{%File 'readme.txt'}

uses
  Forms,
  DropText in 'DropText.pas' {FormText},
  demo in 'demo.pas' {FormDemo},
  DropFile in 'DropFile.pas' {FormFile},
  DropURL in 'DropURL.pas' {FormURL};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Drag and Drop Demo';
  Application.CreateForm(TFormDemo, FormDemo);
  Application.Run;
end.
