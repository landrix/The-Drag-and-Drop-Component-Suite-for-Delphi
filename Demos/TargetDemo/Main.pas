unit Main;

interface

uses
  DragDrop,
  DropTarget,
  DragDropFile,
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    Label1: TLabel;
    Panel2: TPanel;
    ButtonClose: TButton;
    DropFileTarget1: TDropFileTarget;
    ListView1: TListView;
    procedure ButtonCloseClick(Sender: TObject);
    procedure DropFileTarget1Drop(Sender: TObject; ShiftState: TShiftState;
      Point: TPoint; var Effect: Integer);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.ButtonCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TForm1.DropFileTarget1Drop(Sender: TObject;
  ShiftState: TShiftState; Point: TPoint; var Effect: Integer);
var
  i: integer;
  Item: TListItem;
begin
  (*
  ** The OnDrop event handler is called when the user drag and drop files
  ** onto your application.
  *)

  Listview1.Items.Clear;

  // Display mapped names if present.
  // Mapped names are usually only present when dragging from the recycle bin
  // (try it).
  if (DropFileTarget1.MappedNames.Count > 0) then
    Listview1.Columns[1].Width := 100
  else
    Listview1.Columns[1].Width := 0;

  // Copy the file names from the DropTarget component into the list view.
  for i := 0 to DropFileTarget1.Files.Count-1 do
  begin
    Item := Listview1.Items.Add;
    Item.Caption := DropFileTarget1.Files[i];

    // Display mapped names if present.
    if (DropFileTarget1.MappedNames.Count > i) then
      Item.SubItems.Add(DropFileTarget1.MappedNames[i]);
  end;

  // For this demo we reject the drop if a move operation was performed. This is
  // done so the drop source doesn't think we actually did something usefull
  // with the data.
  // This is important when moving data or when dropping from the recycle bin;
  // If we do not reject a move, the source will assume that it is safe to
  // delete the source data. See also "Optimized move".
  if (Effect = DROPEFFECT_MOVE) then
    Effect := DROPEFFECT_NONE;
end;

end.
