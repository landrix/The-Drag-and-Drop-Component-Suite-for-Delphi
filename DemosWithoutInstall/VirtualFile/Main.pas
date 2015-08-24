unit Main;

interface

uses
  Windows, Classes, Controls, Forms, ExtCtrls, StdCtrls,
  DragDrop, DropSource, DragDropFile, DropTarget, Graphics, ImgList, Menus,
  ActnList;

type
  (*
  ** This is a custom data format.
  ** The data format supports TFileContentsClipboardFormat and
  ** T*FileGroupDescritorClipboardFormat.
  *)
  TVirtualFileDataFormat = class(TCustomDataFormat)
  private
    FContents: AnsiString;
    FFileName: string;
  public
    constructor Create(AOwner: TDragDropComponent); override;
    function Assign(Source: TClipboardFormat): boolean; override;
    function AssignTo(Dest: TClipboardFormat): boolean; override;
    procedure Clear; override;
    function HasData: boolean; override;
    function NeedsData: boolean; override;
    property FileName: string read FFileName write FFileName;
    property Contents: AnsiString read FContents write FContents;
  end;

  TFormMain = class(TForm)
    EditFilename: TEdit;
    Label1: TLabel;
    MemoContents: TMemo;
    Label2: TLabel;
    DropDummy1: TDropDummy;
    Panel2: TPanel;
    PanelDragDrop: TPanel;
    ImageList1: TImageList;
    DropEmptySource1: TDropEmptySource;
    DropEmptyTarget1: TDropEmptyTarget;
    Label3: TLabel;
    PopupMenu1: TPopupMenu;
    MenuCopy: TMenuItem;
    MenuPaste: TMenuItem;
    ActionList1: TActionList;
    ActionCopy: TAction;
    ActionPaste: TAction;
    procedure OnMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure DropFileTarget1Drop(Sender: TObject; ShiftState: TShiftState;
      Point: TPoint; var Effect: Integer);
    procedure ActionCopyExecute(Sender: TObject);
    procedure ActionCopyUpdate(Sender: TObject);
    procedure ActionPasteExecute(Sender: TObject);
    procedure ActionPasteUpdate(Sender: TObject);
  private
    { Private declarations }
    FSourceDataFormat: TVirtualFileDataFormat;
    FTargetDataFormat: TVirtualFileDataFormat;
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.DFM}

uses
  DragDropFormats,
  ShlObj,
  ComObj,
  ActiveX,
  SysUtils;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  // Add our own custom data format to the drag/drop components.
  FSourceDataFormat := TVirtualFileDataFormat.Create(DropEmptySource1);
  FTargetDataFormat := TVirtualFileDataFormat.Create(DropEmptyTarget1);
end;

procedure TFormMain.OnMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  // Ignore right mouse button so popup menu can be invoked
  if (Button = mbLeft) and DragDetectPlus(Handle, Point(X,Y)) then
  begin
    // Transfer the file name and contents to the data format...
    FSourceDataFormat.FileName := EditFileName.Text;
    FSourceDataFormat.Contents := MemoContents.Lines.Text;

    // ...and let it rip!
    DropEmptySource1.Execute;
  end;
end;

procedure TFormMain.ActionCopyExecute(Sender: TObject);
begin
  // Transfer the file name and contents to the data format...
  FSourceDataFormat.FileName := EditFileName.Text;
  FSourceDataFormat.Contents := MemoContents.Lines.Text;

  // ...and copy to clipboard.
  DropEmptySource1.CopyToClipboard;
end;

procedure TFormMain.ActionCopyUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (EditFilename.Text <> '');
end;

procedure TFormMain.ActionPasteExecute(Sender: TObject);
begin
  DropEmptyTarget1.PasteFromClipboard;
end;

procedure TFormMain.ActionPasteUpdate(Sender: TObject);
(*
var
  DataObject: IDataObject;
*)
begin
  (*
  ** The following shows two diffent methods of determining if the clipboard
  ** contains any data that we can paste. The two methods are in fact identical.
  ** Only the second method is used.
  *)

  (*
  ** Method 1: Get the clipboard data object and test against it.
  *)
  (*
  // Open the clipboard as an IDataObject
  OleCheck(OleGetClipboard(DataObject));
  try
    // Enable paste menu if the clipboard contains data in any of
    // the supported formats.
    MenuPaste.Enabled := DropEmptyTarget1.HasValidFormats(DataObject);
  finally
    DataObject := nil;
  end;
  *)

  (*
  ** Method 2: Let the component do it for us.
  *)
  MenuPaste.Enabled := DropEmptyTarget1.CanPasteFromClipboard;
end;

procedure TFormMain.DropFileTarget1Drop(Sender: TObject;
  ShiftState: TShiftState; Point: TPoint; var Effect: Integer);
begin
  // Transfer the file name and contents from the data format.
  EditFileName.Text := FTargetDataFormat.FileName;

  // Limit the amount of data to 32Kb. If someone drops a huge amount on data on
  // us (e.g. the AsyncTransferSource demo which transfers 10Mb of data) we need
  // to limit how much data we try to stuff into the poor memo field. Otherwise
  // we could wait for hours before transfer was finished.
  MemoContents.Lines.Text := Copy(FTargetDataFormat.Contents, 1, 1024*32);
end;


{ TVirtualFileDataFormat }

constructor TVirtualFileDataFormat.Create(AOwner: TDragDropComponent);
begin
  inherited Create(AOwner);

  // Add the "file group descriptor" and "file contents" clipboard formats to
  // the data format's list of compatible formats.
  // Note: This is normally done via TCustomDataFormat.RegisterCompatibleFormat,
  // but since this data format is only used here, it is just as easy for us to
  // add the formats manually.
  CompatibleFormats.Add(TFileContentsClipboardFormat.Create);
  CompatibleFormats.Add(TAnsiFileGroupDescriptorClipboardFormat.Create);
  CompatibleFormats.Add(TUnicodeFileGroupDescriptorClipboardFormat.Create);
end;

function TVirtualFileDataFormat.Assign(Source: TClipboardFormat): boolean;
begin
  Result := True;

  (*
  ** TFileContentsClipboardFormat
  *)
  if (Source is TFileContentsClipboardFormat) then
  begin
    FContents := TFileContentsClipboardFormat(Source).Data
  end else
  (*
  ** TAnsiFileGroupDescriptorClipboardFormat
  *)
  if (Source is TAnsiFileGroupDescriptorClipboardFormat) then
  begin
    if (TAnsiFileGroupDescriptorClipboardFormat(Source).Count > 0) then
      FFileName := TAnsiFileGroupDescriptorClipboardFormat(Source).Filenames[0];
  end else
  (*
  ** TUnicodeFileGroupDescriptorClipboardFormat
  *)
  if (Source is TUnicodeFileGroupDescriptorClipboardFormat) then
  begin
    if (TUnicodeFileGroupDescriptorClipboardFormat(Source).Count > 0) then
      FFileName := TUnicodeFileGroupDescriptorClipboardFormat(Source).Filenames[0];
  end else
  (*
  ** None of the above...
  *)
    Result := inherited Assign(Source);
end;

function TVirtualFileDataFormat.AssignTo(Dest: TClipboardFormat): boolean;
begin
  Result := True;

  (*
  ** TFileContentsClipboardFormat
  *)
  if (Dest is TFileContentsClipboardFormat) then
  begin
    TFileContentsClipboardFormat(Dest).Data := FContents;
  end else
  (*
  ** TAnsiFileGroupDescriptorClipboardFormat
  *)
  if (Dest is TAnsiFileGroupDescriptorClipboardFormat) then
  begin
    TAnsiFileGroupDescriptorClipboardFormat(Dest).Count := 1;
    TAnsiFileGroupDescriptorClipboardFormat(Dest).Filenames[0] := FFileName;
  end else
  (*
  ** TUnicodeFileGroupDescriptorClipboardFormat
  *)
  if (Dest is TUnicodeFileGroupDescriptorClipboardFormat) then
  begin
    TUnicodeFileGroupDescriptorClipboardFormat(Dest).Count := 1;
    TUnicodeFileGroupDescriptorClipboardFormat(Dest).Filenames[0] := FFileName;
  end else
  (*
  ** None of the above...
  *)
    Result := inherited AssignTo(Dest);
end;

procedure TVirtualFileDataFormat.Clear;
begin
  FFileName := '';
  FContents := ''
end;

function TVirtualFileDataFormat.HasData: boolean;
begin
  Result := (FFileName <> '');
end;

function TVirtualFileDataFormat.NeedsData: boolean;
begin
  Result := (FFileName = '') or (FContents = '');
end;

end.

