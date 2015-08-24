program TargetAnalyzer;

uses
  Forms,
  main in 'main.pas' {FormTarget};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Drop Target Analyzer';
  Application.CreateForm(TFormTarget, FormTarget);
  Application.Run;
end.
