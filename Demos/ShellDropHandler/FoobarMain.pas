unit FoobarMain;

interface

{$include dragdrop.inc} // Disables .NET warnings

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, StdCtrls, ExtCtrls;

type
  TFormFileList = class(TForm)
    Panel1: TPanel;
    MemoFileList: TMemo;
    MainMenu1: TMainMenu;
    MenuFile: TMenuItem;
    MenuFileOpen: TMenuItem;
    MenuFileSave: TMenuItem;
    N1: TMenuItem;
    MenuFileExit: TMenuItem;
    MenuSetup: TMenuItem;
    MenuSetupRegister: TMenuItem;
    MenuSetupUnregister: TMenuItem;
    Memo1: TMemo;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    MenuFileSaveAs: TMenuItem;
    MenuFileNew: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure MenuSetupRegisterClick(Sender: TObject);
    procedure MenuFileExitClick(Sender: TObject);
    procedure MenuSetupUnregisterClick(Sender: TObject);
    procedure MenuFileOpenClick(Sender: TObject);
    procedure MenuFileSaveClick(Sender: TObject);
    procedure MenuFileSaveAsClick(Sender: TObject);
    procedure MemoFileListChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure MenuFileNewClick(Sender: TObject);
  private
    FDirty: boolean;
    FFileName: string;
    procedure SetDirty(const Value: boolean);
    procedure SetFileName(const Value: string);
  public
    property FileName: string read FFileName write SetFileName;
    property Dirty: boolean read FDirty write SetDirty;
    procedure LoadFile(const AFilename: string);
    function SaveFile(const AFilename: string): boolean;
    function Clear: boolean;
  end;

var
  FormFileList: TFormFileList;

implementation

{$R *.DFM}

uses
  ComObj;

resourcestring
  sFileClass = 'FoobarFile';
  sFileType = 'Foobar File List';
  sFileExtension = '.foobar';

  sTitle = 'Foobar List Editor - %s';
  sNewFile = 'new list';
  sSaveMods = 'Your modifications has not been saved.'+#13+'Save now?';
  sUnregisterNotice = 'Remember to also unregister the drop handler DLL';
  sRegisterNotice = 'Remember to also register the drop handler DLL';

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

procedure TFormFileList.FormCreate(Sender: TObject);
var
  i: integer;

  procedure LoadFileList(const List: string);
  var
    Files: TStringList;
  begin
    Files := TStringList.Create;
    try
      Files.LoadFromFile(List);
      MemoFileList.Lines.AddStrings(Files);
    finally
      Files.Free;
    end;
  end;

begin
  FileName := '';

  // Display command line (for debug purposes).
  Memo1.Lines.Text := CmdLine;

  if (ParamCount > 0) then
  begin
    // First parameter is file list.
    LoadFile(ParamStr(1));

    // Additional parameters are file names which should be added to the list.
    // If a filename starts with @ it indicates that the file contains a list of
    // file names which should be added to the list.
    for i := 2 to ParamCount do
      if (Copy(ParamStr(i), 1, 1) = '@') then
        LoadFileList(Copy(ParamStr(i), 2, MaxInt))
      else
        MemoFileList.Lines.Add(ParamStr(i));
  end;

  // Determine if the file association has already been registered and modify
  // the register menu items accordingly.
  MenuSetupRegister.Enabled := (GetRegStringValue(sFileClass+'\DefaultIcon', '') = '');
  MenuSetupUnregister.Enabled := not MenuSetupRegister.Enabled;
end;

procedure TFormFileList.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  CanClose := Clear;
end;

procedure TFormFileList.MenuSetupRegisterClick(Sender: TObject);
begin
  // Register file association.
  CreateRegKey(sFileExtension, '', sFileClass);
  CreateRegKey(sFileExtension+'\ShellNew', 'NullFile', '');
  CreateRegKey(sFileClass, '', sFileType);
  CreateRegKey(sFileClass+'\shell\open\command', '', Application.ExeName+' "%1"');
  CreateRegKey(sFileClass+'\DefaultIcon', '', Application.ExeName+',0');
  MenuSetupRegister.Enabled := False;
  MenuSetupUnregister.Enabled := True;
  if (GetRegStringValue(sFileClass+'\shellex\DropHandler', '') = '') then
    ShowMessage(sRegisterNotice);
end;

procedure TFormFileList.MenuSetupUnregisterClick(Sender: TObject);
begin
  // Unregister file association.
  DeleteRegKey(sFileClass+'\DefaultIcon');
  DeleteRegKey(sFileClass+'\shell\open\command');
  DeleteRegKey(sFileClass+'\shell\open');
  DeleteRegKey(sFileClass+'\shell');
  DeleteRegKey(sFileClass);
  DeleteRegKey(sFileExtension+'\ShellNew');
  DeleteRegKey(sFileExtension);
  MenuSetupRegister.Enabled := True;
  MenuSetupUnregister.Enabled := False;
  if (GetRegStringValue(sFileClass+'\shellex\DropHandler', '') <> '') then
    ShowMessage(sUnregisterNotice);
end;

procedure TFormFileList.MenuFileExitClick(Sender: TObject);
begin
  Close;
end;

procedure TFormFileList.MenuFileNewClick(Sender: TObject);
begin
  Clear;
end;

procedure TFormFileList.MenuFileOpenClick(Sender: TObject);
begin
  if (Clear) then
  begin
    OpenDialog1.Filename := FileName;
    if (OpenDialog1.Execute) then
      LoadFile(OpenDialog1.Filename);
  end;
end;

procedure TFormFileList.MenuFileSaveAsClick(Sender: TObject);
begin
  SaveFile('');
end;

procedure TFormFileList.MenuFileSaveClick(Sender: TObject);
begin
  SaveFile(FileName);
end;

procedure TFormFileList.LoadFile(const AFilename: string);
begin
  MemoFileList.Lines.LoadFromFile(AFilename);
  FileName := AFilename;
  Dirty := False;
end;

function TFormFileList.SaveFile(const AFilename: string): boolean;
begin
  Result := True;
  if (AFilename = '') then
  begin
    SaveDialog1.Filename := FileName;
    if (SaveDialog1.Execute) then
      FileName := SaveDialog1.Filename
    else
      Result := False;
  end else
    FileName := AFilename;

  if (Result) then
  begin
    MemoFileList.Lines.SaveToFile(Filename);
    Dirty := False;
  end;
end;

function TFormFileList.Clear: boolean;
var
  Answer: word;
begin
  Result := True;
  // Check for unsaved changes and prompt.
  if (Dirty) then
  begin
    Answer := MessageDlg(sSaveMods, mtConfirmation, [mbYes, mbNo, mbCancel], 0);
    case Answer of
      mrYes:
        Result := SaveFile(FileName);
      mrCancel:
        Result := False;
    end;
  end;

  if (Result) then
  begin
    MemoFileList.Lines.Clear;
    FileName := '';
    Dirty := False;
  end;
end;

procedure TFormFileList.MemoFileListChange(Sender: TObject);
begin
  Dirty := True;
end;

procedure TFormFileList.SetDirty(const Value: boolean);
begin
  // Enable the "Save" menu item if the file has been modified and we have a
  // file name for it.
  FDirty := Value;
  MenuFileSave.Enabled := FDirty and (FileName <> '');
end;

procedure TFormFileList.SetFileName(const Value: string);
begin
  FFileName := Value;
  if (FFileName <> '') then
  begin
    Caption := Format(sTitle, [FFileName]);
    MenuFileSave.Enabled := Dirty;
  end else
  begin
    Caption := Format(sTitle, [sNewFile]);
    MenuFileSave.Enabled := False;
  end;
end;

end.

