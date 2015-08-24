unit Target;

interface

uses
  DragDrop,
  DropTarget,
  DragDropFormats,
  DragDropText,
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
    TimeDataFormatTarget: TGenericDataFormat;
  public
    { Public declarations }
  end;

var
  FormTarget: TFormTarget;

implementation

{$R *.DFM}

uses
  DragDropTimeOfDay; // defines the TTimeOfDay structure.

procedure TFormTarget.FormCreate(Sender: TObject);
begin
  // Define and register our custom clipboard format.
  // This needs to be done for both the drop source and target.

  TimeDataFormatTarget := TGenericDataFormat.Create(DropTextTarget1);
  TimeDataFormatTarget.AddFormat(sTimeOfDayName);
end;

procedure TFormTarget.DropTextTarget1Drop(Sender: TObject;
  ShiftState: TShiftState; Point: TPoint; var Effect: Integer);
var
  TOD: TTimeOfDay;
  Time: TDateTime;
begin
  // Determine if we got our custom format.
  if (TimeDataFormatTarget.HasData) then
  begin
    // Extract the dropped data into our custom struct.
    TimeDataFormatTarget.GetDataHere(TOD, sizeof(TOD));

    // Convert the time-of-day info to a TDateTime so we can display it.
    Time := EncodeTime(TOD.hours, TOD.minutes, TOD.seconds, TOD.milliseconds);

    // Display the data.
    PanelDest.Caption := FormatDateTime('hh:nn:ss.zzz', Time);
    PanelDest.Color := TOD.color;
    PanelDest.Font.Color := not(PanelDest.Color) and $FFFFFF;
  end else
    PanelDest.Caption := TDropTextTarget(Sender).Text;
end;

end.
