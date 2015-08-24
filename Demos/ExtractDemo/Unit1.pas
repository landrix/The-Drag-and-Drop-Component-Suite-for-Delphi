unit Unit1;

interface

{$include dragdrop.inc} // Disables .NET warnings

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, ActiveX, ShellApi, ShlObj, Buttons, ExtCtrls, DropSource,
  StdCtrls, DropTarget, CommCtrl, FileCtrl, DragDrop, DragDropFile;

type

  TFormMain = class(TForm)
    ListView1: TListView;
    Panel1: TPanel;
    ButtonClose: TButton;
    StatusBar1: TStatusBar;
    DropFileSource1: TDropFileSource;
    Label2: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure ListView1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ButtonCloseClick(Sender: TObject);
    procedure DropFileSource1Drop(Sender: TObject; DragType: TDragType;
      var ContinueDrop: Boolean);
    procedure FormDestroy(Sender: TObject);
    procedure DropFileSource1AfterDrop(Sender: TObject;
      DragResult: TDragResult; Optimized: Boolean);
    procedure DropFileSource1GetData(Sender: TObject;
      const FormatEtc: tagFORMATETC; out Medium: tagSTGMEDIUM;
      var Handled: Boolean);
  private
    FTempPath: string; // path to temp folder
    FExtractedFiles: TStringList;
    FHasExtracted: boolean;
    procedure ExtractFile(FileIndex: integer; Filename: string);
    procedure ExtractFiles;
    procedure RemoveFile(FileIndex: integer);
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;


implementation

{$R *.DFM}

////////////////////////////////////////////////////////////////////////////////
//
//		Utility methods
//
////////////////////////////////////////////////////////////////////////////////
procedure MakeBlankFile(const Name: string);
var
  f: TextFile;
  path: string;
begin
  path := ExtractFilePath(name);
  if (path <> '') then
    ForceDirectories(path);
  AssignFile(f, Name);
  try
    Rewrite(f);
  finally
    CloseFile(f);
  end;
end;

function AddSlash(const str: string): string;
begin
  Result := str;
  if (Result <> '') and (Result[length(Result)] <> '\') then
    Result := Result+'\';
end;

function GetTempPath: string;
var
  Res: DWORD;
begin
  SetLength(Result, MAX_PATH);
  Res := windows.GetTempPath(MAX_PATH, PChar(Result));
  SetLength (Result, Res);
  AddSlash(Result); //append a slash if needed
end;

////////////////////////////////////////////////////////////////////////////////
//
//		Startup/Shutdown
//
////////////////////////////////////////////////////////////////////////////////
procedure TFormMain.FormCreate(Sender: TObject);
begin
  // Get path to temporary directory
  FTempPath := GetTempPath;
  // List of all extracted files
  FExtractedFiles := TStringList.Create;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
var
  i: integer;
begin
  // Before we exit, we make sure that we aren't leaving any of the files we
  // created behind. Since it is the drop target's responsibility to
  // clean up after an optimized drag/move operation, we might get away with
  // just deleting all drag/copied files, but since many ill behaved drop
  // targets doesn't clean up after them selves, we will do it for them
  // here. If you trust your drop target to clean up after itself, you can skip
  // this step.
  // Note that this means that you shouldn't exit this application before
  // the drop target has had a chance of actually copy/move the files.
  for i := 0 to FExtractedFiles.Count-1 do
    if (FileExists(FExtractedFiles[i])) then
      try
        DeleteFile(FExtractedFiles[i]);
      except
        // Ignore any errors we might get
      end;
  // Note: We should also remove any folders we created, but this example
  // doesn't do that.

  FExtractedFiles.Free;
end;

procedure TFormMain.ButtonCloseClick(Sender: TObject);
begin
  Close;
end;

////////////////////////////////////////////////////////////////////////////////
//
//		MouseDown handler.
//
////////////////////////////////////////////////////////////////////////////////
// Does drag detection, sets up the filename list and starts the drag operation.
////////////////////////////////////////////////////////////////////////////////
procedure TFormMain.ListView1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  i, j: integer;
  s: string;
begin
  // If no files selected then exit...
  if (Listview1.SelCount = 0) then
    exit;

  if (DragDetectPlus(TWinControl(Sender))) then
  begin

    // Clear any filenames left from a previous drag operation...
    DropFileSource1.Files.Clear;

    // 'Extracting' files here would be much simpler but is often
    // very inefficient as many drag ops are cancelled before the
    // files are actually dropped. Instead we delay the extracting,
    // until we know the user really wants the files, but load the
    // filenames into DropFileSource1.Files as if they already exist...

    // Add root files and top level subfolders...
    for i := 0 to Listview1.Items.Count-1 do
      if (Listview1.Items[i].Selected) then
      begin
        // Note that it isn't nescessary to list files and folders in
        // sub folders. It is sufficient to list the top level sub folders,
        // since the drag target will copy/move the sub folders and
        // everything they contain.
        // Some target applications might not be able to handle this
        // optimization or it might not suit your purposes. In that case,
        // simply remove all the code between [A] and [B] below .

        // [A]
        j := pos('\', Listview1.Items[i].Caption);
        if (j > 0) then
        begin
          // Item is a subfolder...

          // Get the top level subfolder.
          s := Copy(Listview1.Items[i].Caption, 1, j-1);

          // Add folder if it hasn't already been done.
          if DropFileSource1.Files.IndexOf(FTempPath + s) = -1 then
            DropFileSource1.Files.Add(FTempPath + s);
        end else
          // [B]
          // Item is a file in the root folder...
          DropFileSource1.Files.Add(FTempPath + Listview1.Items[i].Caption);
      end;

    FHasExtracted := False;

    // Start the drag operation...
    DropFileSource1.Execute;

  end;
end;

////////////////////////////////////////////////////////////////////////////////
//
//		OnDrop handler.
//
////////////////////////////////////////////////////////////////////////////////
// Executes when the user drops the files on a drop target.
////////////////////////////////////////////////////////////////////////////////
procedure TFormMain.DropFileSource1Drop(Sender: TObject; DragType: TDragType;
  var ContinueDrop: Boolean);
begin
  // If the user actually dropped the filenames somewhere, we would now
  // have to extract the files from the archive. The files should be
  // extracted to the same path and filename as the ones we specified
  // in the drag operation. Otherwise the drop source will not be able
  // to find the files.

  ExtractFiles;

  // As soon as this method returns, the destination's (e.g. Explorer's)
  // DropTarget.OnDrop event will trigger and the destination will
  // start copying/moving the files.
end;

////////////////////////////////////////////////////////////////////////////////
//
//		OnGetData handler.
//
////////////////////////////////////////////////////////////////////////////////
// Executes when the drop target requests data from the drop source.
//
// This is a good place to create the physical files so they are actually
// present when the filenames are returned to the drop target.
// Even though it would be more efficient to only create the files in the OnDrop
// handler, some drop targets expect the files to be present and valid at the
// time the cursor enters the drop target (i.e. at the time IDataObject.GetData
// is first called).
////////////////////////////////////////////////////////////////////////////////
procedure TFormMain.DropFileSource1GetData(Sender: TObject;
  const FormatEtc: tagFORMATETC; out Medium: tagSTGMEDIUM;
  var Handled: Boolean);
begin
  if (FormatEtc.cfFormat = CF_HDROP) then
    ExtractFiles;
end;

////////////////////////////////////////////////////////////////////////////////
//
//		OnAfterDrop handler.
//
////////////////////////////////////////////////////////////////////////////////
// Executes after the target has returned from its OnDrop event handler.
////////////////////////////////////////////////////////////////////////////////
procedure TFormMain.DropFileSource1AfterDrop(Sender: TObject;
  DragResult: TDragResult; Optimized: Boolean);
var
  i, j: integer;
begin
  // If the user performed a move operation, we now delete the selected files
  // from the archive.
  if (DragResult = drDropMove) then
    for i := Listview1.Items.Count-1 downto 0 do
      if (Listview1.Items[i].Selected) then
        RemoveFile(i);

  // If the user performed an unoptimized move operation, we must delete the
  // files that were extracted.
  if (DragResult = drDropMove) and (not Optimized) then
    for i := 0 to DropFileSource1.Files.Count-1 do
    begin
      if (FileExists(DropFileSource1.Files[i])) then
        try
          DeleteFile(DropFileSource1.Files[i]);
          // Remove the files we just deleted from the "to do" list.
          j := FExtractedFiles.IndexOf(DropFileSource1.Files[i]);
          if (j <> -1) then
            FExtractedFiles.Delete(j);
        except
          // Ignore any errors we might get.
        end;
      // Note: We should also remove any folders we created, but this example
      // doesn't do that.
    end;
end;

////////////////////////////////////////////////////////////////////////////////
//
//		Extract file from archive.
//
////////////////////////////////////////////////////////////////////////////////
// This method extracts a single file from the archive and saves it to disk.
// In a "real world" application, you would create (e.g. unzip, download etc.)
// your physical files here.
////////////////////////////////////////////////////////////////////////////////
procedure TFormMain.ExtractFile(FileIndex: integer; Filename: string);
begin
  // Of course, this is a demo so we'll just make phoney files here...
  MakeBlankFile(Filename);
  // Remember that we have extracted this file
  if (FExtractedFiles.IndexOf(Filename) = -1) then
    FExtractedFiles.Add(Filename);
end;

////////////////////////////////////////////////////////////////////////////////
//
//		Extract files
//
////////////////////////////////////////////////////////////////////////////////
procedure TFormMain.ExtractFiles;
var
  i: integer;
begin
  if (FHasExtracted) then
    exit;

  // 'Extract' all the selected files into the temporary folder tree...
  for i := Listview1.Items.Count-1 downto 0 do
    if (Listview1.Items[i].Selected) then
      ExtractFile(i, FTempPath + Listview1.Items[i].Caption);

  FHasExtracted := True;
end;

////////////////////////////////////////////////////////////////////////////////
//
//		Delete file from archive.
//
////////////////////////////////////////////////////////////////////////////////
// This method removes a single file from the archive.
////////////////////////////////////////////////////////////////////////////////
procedure TFormMain.RemoveFile(FileIndex: integer);
begin
  // This is just a demo, so we'll just remove the filename from the listview...
  Listview1.Items.Delete(FileIndex);
end;

end.

