unit Source;

interface

uses
  DragDrop,
  DropSource,
  DragDropFormats,
  DragDropText,
  DragDropTimeOfDay,
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls;

type
  TFormSource = class(TForm)
    Panel1: TPanel;
    PanelSource: TPanel;
    Timer1: TTimer;
    DropTextSource1: TDropTextSource;
    Panel3: TPanel;
    Memo1: TMemo;
    Panel4: TPanel;
    procedure Timer1Timer(Sender: TObject);
    procedure PanelSourceMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    TimeOfDayDataFormat: TTimeOfDayDataFormat;
  public
    { Public declarations }
  end;

var
  FormSource: TFormSource;

implementation

{$R *.DFM}

procedure TFormSource.FormCreate(Sender: TObject);
begin
  // Add our custom data and clipboard format to the drag/drop component.
  // This needs to be done for both the drop source and target.
  TimeOfDayDataFormat := TTimeOfDayDataFormat.Create(DropTextSource1);
end;

procedure TFormSource.Timer1Timer(Sender: TObject);
begin
  PanelSource.Caption := FormatDateTime('hh:nn:ss.zzz', Now);
  PanelSource.Color := random($FFFFFF);
  PanelSource.Font.Color := not(PanelSource.Color) and $FFFFFF;
end;

procedure TFormSource.PanelSourceMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  TOD: TTimeOfDay;
begin
  Timer1.Enabled := False;
  try
    if (DragDetectPlus(TWinControl(Sender).Handle, Point(X,Y))) then
    begin
      // Transfer time as text. This is not nescessary and is only done to offer
      // maximum flexibility in case the user wishes to drag our data to some
      // other application (e.g. a word processor).
      DropTextSource1.Text := PanelSource.Caption;

      // Store the current time in a structure. This structure is our custom
      // data format.
      DecodeTime(Now, TOD.hours, TOD.minutes, TOD.seconds, TOD.milliseconds);
      TOD.color := PanelSource.Color;

      // Transfer the structure to the drop source data object and execute the drag.
      TimeOfDayDataFormat.TOD := TOD;

      DropTextSource1.Execute;
    end;
  finally
    Timer1.Enabled := True;
  end;
end;

end.
