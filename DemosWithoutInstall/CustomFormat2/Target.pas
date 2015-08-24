unit Target;

interface

uses
  DragDrop,
  DropTarget,
  DragDropFormats,
  DragDropText,
  DragDropTimeOfDay,
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls;

type
  TFormTarget = class(TForm)
    Panel2: TPanel;
    PanelDest: TPanel;
    DropTextTarget1: TDropTextTarget;
    Panel5: TPanel;
    procedure DropTextTarget1Drop(Sender: TObject; ShiftState: TShiftState;
      Point: TPoint; var Effect: Integer);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    TimeOfDayDataFormat: TTimeOfDayDataFormat;
  public
    { Public declarations }
  end;

var
  FormTarget: TFormTarget;

implementation

{$R *.DFM}

procedure TFormTarget.FormCreate(Sender: TObject);
begin
  // Add our custom data and clipboard format to the drag/drop component.
  // This needs to be done for both the drop source and target.
  TimeOfDayDataFormat := TTimeOfDayDataFormat.Create(DropTextTarget1);
end;

procedure TFormTarget.DropTextTarget1Drop(Sender: TObject;
  ShiftState: TShiftState; Point: TPoint; var Effect: Integer);
var
  Time: TDateTime;
begin
  // Determine if we got our custom format.
  if (TimeOfDayDataFormat.HasData) then
  begin
    // Convert the time-of-day info to a TDateTime so we can display it.
    Time := EncodeTime(TimeOfDayDataFormat.TOD.hours,
      TimeOfDayDataFormat.TOD.minutes, TimeOfDayDataFormat.TOD.seconds,
      TimeOfDayDataFormat.TOD.milliseconds);

    // Display the data.
    PanelDest.Caption := FormatDateTime('hh:nn:ss.zzz', Time);
    PanelDest.Color := TimeOfDayDataFormat.TOD.color;
    PanelDest.Font.Color := not(PanelDest.Color) and $FFFFFF;
  end else
    PanelDest.Caption := TDropTextTarget(Sender).Text;
end;

end.
