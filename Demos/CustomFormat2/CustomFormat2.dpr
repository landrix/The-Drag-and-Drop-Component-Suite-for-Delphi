program CustomFormat2;

{%File 'readme.txt'}

uses
  Forms,
  Target in 'Target.pas' {FormTarget},
  Source in 'Source.pas' {FormSource},
  DragDropTimeOfDay in 'DragDropTimeOfDay.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormSource, FormSource);
  Application.CreateForm(TFormTarget, FormTarget);
  FormTarget.Top := FormSource.Top;
  FormTarget.Left := FormSource.Left+FormSource.Width;
  Application.Run;
end.
