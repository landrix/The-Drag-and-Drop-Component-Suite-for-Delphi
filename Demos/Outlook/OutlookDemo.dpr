program OutlookDemo;

{%File 'readme.txt'}

uses
  Forms,
  OutlookTarget in 'OutlookTarget.pas' {FormOutlookTarget};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Outlook Drop Target demo';
  Application.CreateForm(TFormOutlookTarget, FormOutlookTarget);
  Application.Run;
end.
