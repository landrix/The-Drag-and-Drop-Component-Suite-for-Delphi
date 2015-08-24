program RtfTarget;



uses
  Forms,
  Target in 'Target.pas' {FormTarget};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormTarget, FormTarget);
  Application.Run;
end.
