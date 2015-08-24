unit DropHandlerMain;

interface

uses
  DragDrop, DropTarget, DragDropFile, DropHandler,
  Forms, ActiveX, Classes, Windows;

{$include 'DragDrop.inc'}

//{$ifndef VER13_PLUS}
//type
//  TDataModule = TForm;
//{$endif}

type
  TDataModuleDropHandler = class(TDataModule, IUnknown, IDropTarget, IPersistFile)
    DropHandler1: TDropHandler;
    procedure DropHandler1Drop(Sender: TObject; ShiftState: TShiftState;
      Point: TPoint; var Effect: Integer);
  public
    // Aggregate IDropTarget and IPersistFile to the TDrophandler component.
    property DropHandler: TDrophandler read DropHandler1
      implements IDropTarget, IPersistFile;
  end;

implementation

{$R *.DFM}

uses
  ShellAPI,
  ComServ,
  ComObj,
  SysUtils;

const
  // CLSID for this shell extension.
  // Modify this for your own shell extensions (press [Ctrl]+[Shift]+G in
  // the IDE editor to gererate a new CLSID).
  CLSID_DropHandler: TGUID = '{D10DA001-5859-11D4-A83A-00E0189008B3}';

resourcestring
  // Name of the file class we wish to operate on.
  sFileClass = 'FoobarFile';
  // File extension files we wish to operate on.
  sFileExtension = '.foobar';

  // Class name of our shell extension.
  sClassName = 'FoobarFileEditor';
  // Description of our shell extension.
  sDescription = 'Drag and Drop Component Suite demo drop handler';

//{$ifndef VER13_PLUS}
//function GetRegStringValue(const Key, ValueName: string): string;
//var
//  Size: DWord;
//  RegKey: HKEY;
//begin
//  Result := '';
//  if RegOpenKey(HKEY_CLASSES_ROOT, PChar(Key), RegKey) = ERROR_SUCCESS then
//  try
//    Size := 256;
//    SetLength(Result, Size);
//    if RegQueryValueEx(RegKey, PChar(ValueName), nil, nil, PByte(PChar(Result)), @Size) = ERROR_SUCCESS then
//      SetLength(Result, Size - 1) else
//      Result := '';
//  finally
//    RegCloseKey(RegKey);
//  end;
//end;
//{$endif}

(*
** TDropHandler.OnDrop is called when the user drops one or more files on the
** target file.
*)
procedure TDataModuleDropHandler.DropHandler1Drop(Sender: TObject;
  ShiftState: TShiftState; Point: TPoint; var Effect: Integer);

  procedure WinExecAndWait(const FileName, Parameters: string; Wait: boolean);
  var
    StartupInfo: TStartupInfo;
    ProcessInfo: TProcessInformation;
  begin
    FillChar(StartupInfo, Sizeof(StartupInfo),#0);
    StartupInfo.cb := Sizeof(StartupInfo);
    StartupInfo.dwFlags := STARTF_USESHOWWINDOW;
    StartupInfo.wShowWindow := SW_SHOWDEFAULT;
    // Warning: Even though we could (and I would prefer to) call CreateProcess
    // like this:
    //   CreateProcess(PChar(FileName), PChar(Parameters), ...
    // a bug in Delphi's ParamStr function would cause the target application
    // to fail if we did so. The bug causes ParamStr(1) to "disappear".
    if (CreateProcess(nil, PChar(FileName+' '+Parameters), nil, nil, False,
      CREATE_NEW_CONSOLE or NORMAL_PRIORITY_CLASS, nil, nil, StartupInfo,
      ProcessInfo)) then
    begin
      if (Wait) then
        // Wait 5 seconds then assume file has been read and can be deleted.
        WaitforSingleObject(ProcessInfo.hProcess, 5000);
    end;
  end;

var
  TargetExt: string;
  AppName: string;
  AppPath: string;
  AppParams: string;
  Files: string;
  i: integer;
  Tmp: string;
  DeleteTmp: boolean;
begin
  // Get the application associated with the registered file.
  TargetExt := ExtractFileExt(TDrophandler(Sender).TargetFile);
  AppName := GetRegStringValue(TargetExt, '');
  AppPath := GetRegStringValue(AppName+'\shell\open\command', '');
  if (AppName = '') or (AppPath = '') then
  begin
    Effect := DROPEFFECT_NONE;
    exit;
  end;

  // Convert the target and source file list to a string of space delimited and
  // quoted file names.
  // The file names must be quoted in order to handle long file names correctly.
  Files := '"'+TDrophandler(Sender).TargetFile+'"';
  for i := 0 to TDrophandler(Sender).Files.Count-1 do
    Files := Files+' "'+TDrophandler(Sender).Files[i]+'"';

  // Insert the file names into the target application's parameter template (if
  // it has one). The parameter template usually looks like this: "%1"
  i := Pos('"', AppPath);
  if (i > 0) then
  begin
    // Split application string into application and parameter template.
    AppParams := Copy(AppPath, i+1, MaxInt);
    AppPath := Copy(AppPath, 1, i-1);
    // Discard everything after the last ".
    i := Pos('"', AppParams);
    if (i > 0) then
      Delete(AppParams, i, MaxInt);
    // Insert files names into parameter template.
    AppParams := StringReplace(AppParams, '%1', Files, []);
  end else
    AppParams := Files;

  // Make sure that command line length stays within limits.
  // If the command line get too long we write all the file names to a text file
  // and use the text file name as a file list parameter (@filename).
  DeleteTmp := False;
  if (Length(AppPath)+Length(AppParams) >= MAX_PATH) then
  begin
    DeleteTmp := True;
    // Construct unique temporary file name.
    SetLength(Tmp, MAX_PATH);
    GetTempPath(MAX_PATH, PChar(Tmp));
    GetTempFileName(PChar(Tmp), 'foo', 0, PChar(Tmp));
    // Trim extra trailing zeroes.
    SetLength(Tmp, Length(PChar(Tmp)));
    // Write file name list to file.
    TDrophandler(Sender).Files.SaveToFile(Tmp);
    // Construct new command line parameters.
    AppParams := Format('"%s" "@%s"', [TDrophandler(Sender).TargetFile, Tmp]);
  end;

  WinExecAndWait(AppPath, AppParams, DeleteTmp);

  if (DeleteTmp) then
    DeleteFile(Tmp);
end;

initialization
  TDropHandlerFactory.Create(ComServer, TDataModuleDropHandler, CLSID_DropHandler,
    sClassName, sDescription, sFileClass, sFileExtension, ciMultiInstance);
end.
