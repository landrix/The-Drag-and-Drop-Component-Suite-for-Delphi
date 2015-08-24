program AsyncFTPClient;

uses
  Forms,
  main in 'main.pas' {FormMain},
  RingbufferStream in 'RingbufferStream.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Drag and Drop Component Suite FTP client demo';
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
