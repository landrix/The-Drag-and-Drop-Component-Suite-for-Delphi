unit Main;

interface

uses
  DragDrop,
  DropSource,
  DragDropFile,
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, DropTarget, Menus;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    ButtonClose: TButton;
    DropFileSource1: TDropFileSource;
    ListView1: TListView;
    DropDummy1: TDropDummy;
    procedure ButtonCloseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ListView1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DropFileSource1GetDragImage(Sender: TObject;
      const DragSourceHelper: IDragSourceHelper; var Handled: Boolean);
  private
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

uses
  ActiveX, CommCtrl;

procedure TForm1.ButtonCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TForm1.DropFileSource1GetDragImage(Sender: TObject;
  const DragSourceHelper: IDragSourceHelper; var Handled: Boolean);
var
  Pt: TPoint;
begin
  GetCursorPos(Pt);
  Handled := Succeeded(DragSourceHelper.InitializeFromWindow(Listview1.Handle, Pt, TCustomDropSource(Sender) as IDataObject));
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  Path: string;
  SearchRec: TSearchRec;
  Res: integer;
  NewItem: TListItem;
begin
  (*
  ** Fill listview with list of files from current directory...
  *)
  Path := ExtractFilePath(Application.ExeName);
  Res := FindFirst(path+'*.*', 0, SearchRec);
  try
    while (Res = 0) do
    begin
      if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
      begin
        NewItem := Listview1.Items.Add;
        NewItem.Caption := Path+SearchRec.Name;
      end;
      Res := FindNext(SearchRec);
    end;
  finally
    FindClose(SearchRec);
  end;
end;

procedure TForm1.ListView1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  i: integer;
begin
  (*
  ** Wait for user to move mouse before we start the drag/drop.
  *)

  // Note:
  // Due to some internal mouse message juggling inside TListView we will not
  // get the MouseDown event until the mouse is either moved or the mouse button
  // is released.
  // Remember this when it appears that DragDetectPlus isn't working...
  if (Listview1.SelCount > 0) and (DragDetectPlus(TWinControl(Sender))) then
  begin
    // Delete anything from a previous drag.
    DropFileSource1.Files.Clear;

    // Fill DropSource1.Files with selected files from ListView1.
    for i := 0 to Listview1.Items.Count-1 do
      if (Listview1.items.Item[i].Selected) then
        DropFileSource1.Files.Add(Listview1.items.Item[i].Caption);

    // Start the drag operation.
    DropFileSource1.Execute;
  end;
end;

end.
