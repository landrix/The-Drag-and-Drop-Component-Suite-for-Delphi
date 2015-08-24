program SourceAnalyzer;

uses
  Forms,
  Main in 'Main.pas' {FormMain};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Drop Source analyzer';
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
