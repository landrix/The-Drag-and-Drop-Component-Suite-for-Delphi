unit main;

interface

uses
  DragDrop,
  DropTarget,
  DropSource,
  DragDropText,
  Windows, Classes, Graphics, Forms, StdCtrls, Controls,Types;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    MemoLeft: TMemo;
    MemoRight: TMemo;
    CheckBoxLeft: TCheckBox;
    CheckBoxRight: TCheckBox;
    MemoSource: TMemo;
    procedure FormDestroy(Sender: TObject);
    procedure CheckBoxLeftClick(Sender: TObject);
    procedure CheckBoxRightClick(Sender: TObject);
    procedure MemoSourceMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DropTextTarget1Enter(Sender: TObject;
      ShiftState: TShiftState; Point: TPoint; var Effect: Integer);
    procedure DropTextTarget1Leave(Sender: TObject);
    procedure DropTextTarget1Drop(Sender: TObject; ShiftState: TShiftState;
      Point: TPoint; var Effect: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FormCreate(Sender: TObject);
  private
    DropTextTarget1: TDropTextTarget;
    DropTextSource1: TDropTextSource;
    DropDummy1: TDropDummy;
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.FormCreate(Sender: TObject);
begin
  DropTextTarget1:= TDropTextTarget.Create(self);
  DropTextTarget1.DragTypes := [dtCopy];
  DropTextTarget1.OnEnter := DropTextTarget1Enter;
  DropTextTarget1.OnLeave := DropTextTarget1Leave;
  DropTextTarget1.OnDrop := DropTextTarget1Drop;
  DropTextTarget1.MultiTarget := True;

  DropTextSource1 := TDropTextSource.Create(self);
  DropTextSource1.DragTypes := [dtCopy];

  DropDummy1:= TDropDummy.Create(self);
  DropDummy1.DragTypes := [dtCopy, dtMove, dtLink];
  DropDummy1.Target := self;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  // Unregister all targets.
  // This is not strictly nescessary since the target component will perform
  // the unregistration automatically when it is destroyed. Feel free to skip
  // this step if you like.
  if Assigned(DropTextTarget1) then
  begin
    DropTextTarget1.Unregister;
    DropTextTarget1.Free;
    DropTextTarget1 := nil;
  end;

  if Assigned(DropDummy1) then begin DropDummy1.Free; DropDummy1 := nil; end;
  if Assigned(DropTextSource1) then begin DropTextSource1.Free; DropTextSource1 := nil; end;
end;

procedure TForm1.MemoSourceMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  // Wait for user to move cursor before we start the drag/drop.
  if (DragDetectPlus(TWinControl(Sender))) then
  begin
    DropTextSource1.Text := MemoSource.Lines.Text;
    DropTextSource1.Execute;
  end;
end;

procedure TForm1.CheckBoxLeftClick(Sender: TObject);
begin
  // Register or unregister control as drop target according to users selection.
  if TCheckBox(Sender).Checked then
    DropTextTarget1.Register(MemoLeft)
  else
    DropTextTarget1.Unregister(MemoLeft);
end;

procedure TForm1.CheckBoxRightClick(Sender: TObject);
begin
  // Register or unregister control as drop target according to users selection.
  if TCheckBox(Sender).Checked then
    DropTextTarget1.Register(MemoRight)
  else
    DropTextTarget1.Unregister(MemoRight);
end;

procedure TForm1.DropTextTarget1Enter(Sender: TObject;
  ShiftState: TShiftState; Point: TPoint; var Effect: Integer);
begin
  // Highlight the current drop target.
  // Use the TCustomDropTarget.Target property to determine which control is
  // the current drop target:
  (TCustomDropTarget(Sender).Target as TMemo).Color := clRed;
end;

procedure TForm1.DropTextTarget1Leave(Sender: TObject);
begin
  // Remove highlight.
  (TCustomDropTarget(Sender).Target as TMemo).Color := clWindow;
end;

procedure TForm1.DropTextTarget1Drop(Sender: TObject;
  ShiftState: TShiftState; Point: TPoint; var Effect: Integer);
begin
  // Copy dragged text from target component into target control.
  (TCustomDropTarget(Sender).Target as TMemo).Lines.Text := TDropTextTarget(Sender).Text;

  // Remove highlight.
  (TCustomDropTarget(Sender).Target as TMemo).Color := clWindow;
end;

procedure TForm1.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  i: integer;
  Control: TWinControl;
begin
  // Remove highligt from all controls.
  for i := 0 to ComponentCount-1 do
    if (Components[i] is TMemo) then
      TMemo(Components[i]).Color := clWindow;

  // Demo of TCustomDropTarget.FindTarget:
  // Highlight the control under the cursor if it is a drop target.
  Control := DropTextTarget1.FindTarget((Sender as TWinControl).ClientToScreen(Point(X,Y)));
  if (Control <> nil) and (Control is TMemo) then
    TMemo(Control).Color := clLime;
end;

end.

