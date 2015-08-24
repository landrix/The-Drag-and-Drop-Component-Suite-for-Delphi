program OutlookSource;

uses
  Forms,
  Main in 'Main.pas' {FormOutlookSource};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Outlook Drop Source demo';
  Application.CreateForm(TFormOutlookSource, FormOutlookSource);
  Application.Run;
end.
