unit main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, StdCtrls, Grids, ExtCtrls,
  DropSource, DragDropText, DragDrop, DropTarget;

type
  TFormAutoScroll = class(TForm)
    DropTextTarget1: TDropTextTarget;
    Panel1: TPanel;
    StringGrid1: TStringGrid;
    PanelSource: TPanel;
    Panel3: TPanel;
    DropTextSource1: TDropTextSource;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure PanelSourceMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DropTextTarget1Drop(Sender: TObject; ShiftState: TShiftState;
      Point: TPoint; var Effect: Integer);
    procedure DropTextTarget1Enter(Sender: TObject;
      ShiftState: TShiftState; Point: TPoint; var Effect: Integer);
    procedure DropTextTarget1DragOver(Sender: TObject;
      ShiftState: TShiftState; Point: TPoint; var Effect: Integer);
    procedure DropTextSource1Feedback(Sender: TObject; Effect: Integer;
      var UseDefaultCursors: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormAutoScroll: TFormAutoScroll;

implementation

{$R *.DFM}

uses
  ActiveX;

procedure TFormAutoScroll.FormCreate(Sender: TObject);
var
  i,j: integer;
begin
  // Populate the grid with data
  for i := ord('A') to ord('Z') do
    StringGrid1.Cells[0, 1+i-ord('A')] := chr(i);
  for i := 0 to StringGrid1.ColCount-1 do
    StringGrid1.Cells[1+i, 0] := IntToStr(i);
  for i := ord('A') to ord('Z') do
    for j := 0 to StringGrid1.ColCount-1 do
      StringGrid1.Cells[1+j, 1+i-ord('A')] := chr(i)+IntToStr(j);

  // Slow auto scroll down to 1 scroll every 100mS.
  // The default value is 50.
  DragDropScrollInterval := 100;
end;

procedure TFormAutoScroll.PanelSourceMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if DragDetectPlus(TWinControl(Sender).Handle, Point(X,Y)) then
  begin
    // Drag the current time of day.
    DropTextSource1.Text := DateTimeToStr(Now);
    DropTextSource1.Execute;
  end;
end;

procedure TFormAutoScroll.DropTextSource1Feedback(Sender: TObject;
  Effect: Integer; var UseDefaultCursors: Boolean);
begin
  if (Effect and DROPEFFECT_SCROLL <> 0) then
  begin
    // Use a custom cursor when target is auto-scrolling.
    UseDefaultCursors := False;
    Windows.SetCursor(Screen.Cursors[crSizeAll]);
  end;
end;

procedure TFormAutoScroll.DropTextTarget1Enter(Sender: TObject;
  ShiftState: TShiftState; Point: TPoint; var Effect: Integer);
var
  FirstCell: TRect;
  CustomNoScrollZone: TRect;
begin
  (*
  ** Set up a custom no-scroll zone:
  ** 1. Get the grids client rect.
  ** 2. Move the top left corner to the first data cell.
  ** 3. Shrink the rect 1/2 of the grid cell width and 2/3 of the height.
  *)
  CustomNoScrollZone := StringGrid1.ClientRect;
  FirstCell := StringGrid1.CellRect(StringGrid1.LeftCol,StringGrid1.TopRow);
  CustomNoScrollZone.TopLeft := FirstCell.TopLeft;
  InflateRect(CustomNoScrollZone, -StringGrid1.DefaultColWidth div 2,
    -MulDiv(StringGrid1.DefaultRowHeight, 2, 3));

  TCustomDropTarget(Sender).NoScrollZone := CustomNoScrollZone;
end;

procedure TFormAutoScroll.DropTextTarget1DragOver(Sender: TObject;
  ShiftState: TShiftState; Point: TPoint; var Effect: Integer);
var
  CellX, CellY: integer;
begin
  // Determine if the cursor if over a data cell. If it isn't, we do not accept
  // a drop.
  StringGrid1.MouseToCell(Point.x, Point.y, CellX, CellY);
  if (CellX < 1) or (CellY < 1) then
    Effect := DROPEFFECT_NONE;
end;

procedure TFormAutoScroll.DropTextTarget1Drop(Sender: TObject;
  ShiftState: TShiftState; Point: TPoint; var Effect: Integer);
var
  CellX, CellY: integer;
begin
  // Determine which cell we dropped on and fill it with the dragged text.
  StringGrid1.MouseToCell(Point.x, Point.y, CellX, CellY);
  StringGrid1.Cells[CellX, Celly] := TDropTextTarget(Sender).Text;
end;

end.
