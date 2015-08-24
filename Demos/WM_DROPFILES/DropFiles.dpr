program DropFiles;

uses
  Forms,
  Main in 'Main.pas' {FormMain};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'WM_DROPFILES demo';
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
