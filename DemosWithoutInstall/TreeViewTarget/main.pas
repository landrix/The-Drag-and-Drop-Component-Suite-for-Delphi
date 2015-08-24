unit main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ImgList, StdCtrls, ExtCtrls, ComCtrls, DragDrop, DropTarget,
  DragDropFile;

type
  TFormTreeViewDemo = class(TForm)
    TreeView1: TTreeView;
    Panel1: TPanel;
    Label1: TLabel;
    ImageList1: TImageList;
    DropFileTarget1: TDropFileTarget;
    procedure DropFileTarget1DragOver(Sender: TObject; ShiftState: TShiftState;
      APoint: TPoint; var Effect: Integer);
    procedure FormCreate(Sender: TObject);
    procedure DropFileTarget1Enter(Sender: TObject; ShiftState: TShiftState;
      APoint: TPoint; var Effect: Integer);
    procedure DropFileTarget1Drop(Sender: TObject; ShiftState: TShiftState;
      APoint: TPoint; var Effect: Integer);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormTreeViewDemo: TFormTreeViewDemo;

implementation

{$R *.dfm}

procedure TFormTreeViewDemo.DropFileTarget1DragOver(Sender: TObject;
  ShiftState: TShiftState; APoint: TPoint; var Effect: Integer);
var
  TreeNode: TTreeNode;
begin
  TreeNode := TreeView1.GetNodeAt(APoint.X, APoint.Y);
  // Only accept drop on specific treeview items
  if (TreeNode <> nil) and (TreeNode.ImageIndex = 0) then
  begin
    // Select the item to provide visual feedback
    TreeNode.Selected := True;
    // Override the default drop effect if you need to:
    // Effect := DROPEFFECT_COPY;
  end else
    // Reject the drop
    Effect := DROPEFFECT_NONE;
end;

procedure TFormTreeViewDemo.DropFileTarget1Enter(Sender: TObject; ShiftState: TShiftState;
  APoint: TPoint; var Effect: Integer);
begin
  // Reject the drop if more than one file is being dragged
  if (DropFileTarget1.Files.Count <> 1) then
    Effect := DROPEFFECT_NONE;
end;

procedure TFormTreeViewDemo.FormCreate(Sender: TObject);
begin
  TreeView1.FullExpand;
end;

procedure TFormTreeViewDemo.DropFileTarget1Drop(Sender: TObject;
  ShiftState: TShiftState; APoint: TPoint; var Effect: Integer);
var
  TreeNode: TTreeNode;
begin
  TreeNode := TreeView1.GetNodeAt(APoint.X, APoint.Y);
  // Only accept drop on specific treeview items
  if (TreeNode <> nil) and (TreeNode.ImageIndex = 0) then
  begin
    // Select the item to provide visual feedback
    TreeNode.Selected := True;
    // Change the node caption to the name of the dropped file
    TreeNode.Text := ExtractFileName(DropFileTarget1.Files[0]);
    // Override the default drop effect if you need to:
    // Effect := DROPEFFECT_COPY;
  end else
    // Reject the drop
    Effect := DROPEFFECT_NONE;
end;

end.
