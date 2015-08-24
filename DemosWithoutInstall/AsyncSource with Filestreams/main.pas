unit main;

interface

uses
  RingbufferStream,
  DragDrop, DropSource, DragDropFile,
  // Indy 10 required
  IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient,
  IdExplicitTLSClientServerBase, IdFTP,
  Messages, Dialogs,
  ActiveX, Windows, Classes, Controls, Forms, StdCtrls, ComCtrls, ExtCtrls,
  Buttons, ImgList, ToolWin, ActnList, System.Actions;

{$include DragDrop.inc}

{$IF CompilerVersion >= 12.0}
// Work around for interface breaking changes between different Indy 10 releases... Pffft!
type
  TIndyWorkCountInt = int64;
{$else}
type
  TIndyWorkCountInt = integer;

  // Hack to make it possible to load new indy FTP component with old indy version
  TIPVersion = (Id_IPv4, Id_IPv6);
  TIdFTP = class(IdFTP.TIdFTP)
  private
    FDummyVersion: TIPVersion;
  published
    property IPVersion: TIPVersion write FDummyVersion;
  end;
{$endif}

const
  MSG_PROGRESS = WM_USER;
  MSG_STATUS = WM_USER+1;
  MSG_TRANSFER = WM_USER+2;
  MSG_BROWSE = WM_USER+3;

type
  TDragDropStage = (dsNone, dsIdle, dsDrag, dsDragAsync, dsDragAsyncFailed, dsDrop, dsGetData, dsGetStream, dsDropComplete);

  TBrowseOption = (boBrowse, boUpdateCombo);
  TBrowseOptions = set of TBrowseOption;

  TFormMain = class(TForm)
    DropEmptySource1: TDropEmptySource;
    DataFormatAdapterSource: TDataFormatAdapter;
    ProgressBar1: TProgressBar;
    StatusBar1: TStatusBar;
    ListViewFiles: TListView;
    CoolBar1: TCoolBar;
    ToolBarMain: TToolBar;
    AnimateThrobber: TAnimate;
    ComboAddress: TComboBox;
    ButtonBack: TToolButton;
    ButtonForward: TToolButton;
    ButtonReload: TToolButton;
    ButtonStop: TToolButton;
    ImageListNormal: TImageList;
    ButtonUp: TToolButton;
    ToolButton1: TToolButton;
    ImageListDisabled: TImageList;
    ImageListHot: TImageList;
    ButtonHome: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ActionList1: TActionList;
    ActionBack: TAction;
    ActionForward: TAction;
    ActionRefresh: TAction;
    ActionStop: TAction;
    ActionUp: TAction;
    ActionHome: TAction;
    IdFTP1: TIdFTP;
    ImageListExplorer: TImageList;
    Timer1: TTimer;
    procedure DropEmptySource1Drop(Sender: TObject; DragType: TDragType;
      var ContinueDrop: Boolean);
    procedure DropEmptySource1AfterDrop(Sender: TObject;
      DragResult: TDragResult; Optimized: Boolean);
    procedure DropEmptySource1GetData(Sender: TObject;
      const FormatEtc: tagFORMATETC; out Medium: tagSTGMEDIUM;
      var Handled: Boolean);
    procedure OnMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure ComboAddressKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormDestroy(Sender: TObject);
    procedure ActionBackExecute(Sender: TObject);
    procedure ActionForwardExecute(Sender: TObject);
    procedure ActionRefreshExecute(Sender: TObject);
    procedure ActionStopExecute(Sender: TObject);
    procedure ActionUpExecute(Sender: TObject);
    procedure ActionHomeExecute(Sender: TObject);
    procedure ActionBackUpdate(Sender: TObject);
    procedure ActionForwardUpdate(Sender: TObject);
    procedure ActionRefreshUpdate(Sender: TObject);
    procedure ActionStopUpdate(Sender: TObject);
    procedure ActionUpUpdate(Sender: TObject);
    procedure ComboAddressCloseUp(Sender: TObject);
    procedure ListViewFilesDblClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure IdFTP1Work(Sender: TObject; AWorkMode: TWorkMode;
      AWorkCount: TIndyWorkCountInt);
    procedure IdFTP1WorkBegin(Sender: TObject; AWorkMode: TWorkMode;
      AWorkCountMax: TIndyWorkCountInt);
    procedure IdFTP1WorkEnd(Sender: TObject; AWorkMode: TWorkMode);
    procedure ActionHomeUpdate(Sender: TObject);
  private
    FHistoryList: TStringList;
    FHistoryIndex: Integer;
    FTempPath: string;
    FAddress: string;

    FStatus: TDragDropStage;
    FAbort: boolean;
    FAborted: boolean;
    FWriteStream: TFifoStream;
    FTransferCount: integer;
    FBusyCount: integer;
    FCurrentFileSize: int64;
    FIndyFtpAbortWarningShown: boolean;
    function GetTransferInProgress: boolean;
    function GetBusy: boolean;
    procedure SetStatus(const Value: TDragDropStage);
    procedure SetProgress(Count, MaxCount: integer);
    procedure OnGetStream(Sender: TFileContentsStreamOnDemandClipboardFormat;
      Index: integer; out AStream: IStream);

  protected
    procedure MsgProgress(var Message: TMessage); message MSG_PROGRESS;
    procedure MsgStatus(var Message: TMessage); message MSG_STATUS;
    procedure MsgTransfer(var Message: TMessage); message MSG_TRANSFER;
    procedure MsgBrowse(var Message: TMessage); message MSG_BROWSE;

  public
    procedure Browse(const Address: string; Options: TBrowseOptions = [boBrowse]);
    procedure AddSourceFile(const Filename: string);
    procedure BeginBusy;
    procedure EndBusy;
    procedure BeginTransfer;
    procedure EndTransfer;
    property Status: TDragDropStage read FStatus write SetStatus;
    property Busy: boolean read GetBusy;
    property TransferInProgress: boolean read GetTransferInProgress;
  end;

var
  FormMain: TFormMain;

implementation

{$R *.DFM}
{$R Throbber.res}

uses
  DragDropFormats,
  IdURI,
  IdFTPList,
  IdAllFTPListParsers,
  ShlObj,
  ShellApi,
  Graphics,
  SysUtils, StrUtils;

const
  sAddressHome = 'ftp://ftp.microsoft.com/';
  // Another good test site is:
  sAddressHomeAlt = 'ftp://gatekeeper.dec.com/';

resourcestring
  sIndyFtpAbortWarning =
    'Note: Due to a bug in Indy (the TCP/IP library used in this demo), aborting the'+#13+
    'FTP transfer will most likely mess up the FTP session, causing various error if'+#13+
    'another transfer is initiated.'+#13+#13+
    'A work around for this problem is unfortunately beyond the scope of this demo.';

type
  TBrowseKind = (bkAddress, bkUp, bkRefresh);

function AddTrailingSlash(const s: string): string;
begin
  Result := s;
  if (RightStr(Result, 1) <> '/') then
    Result := Result+'/';
end;

function SizeToStr(Value: Int64): string;
var
  Postfix: string;
begin
  if (Value > 1024) then
  begin
    Value := Value div 1024;
    Postfix := ' Kb';
  end else
    Postfix := '';
  Result := Format('%.0n%s', [Int(Integer(Value)), Postfix]);
end;

function DateTimeToFileTime(ADate: TDateTime): TFileTime;
var
  tmp: integer;
  LocalFileTime: TFileTime;
begin
  tmp := DateTimeToFileDate(ADate);
  DosDateTimeToFileTime(LongRec(tmp).Hi, LongRec(tmp).Lo, LocalFileTime);
  LocalFileTimeToFileTime(LocalFileTime, Result);
end;

procedure TFormMain.FormCreate(Sender: TObject);
var
  SHFileInfo: TSHFileInfo;
begin
  FHistoryList := TStringList.Create;
  FHistoryIndex := -1;
  ComboAddress.Items.Add(sAddressHomeAlt);

  // Setup event handler to let a drop target request data from our drop source.
  (DataFormatAdapterSource.DataFormat as TVirtualFileStreamDataFormat).OnGetStream := OnGetStream;

  StatusBar1.ControlStyle := StatusBar1.ControlStyle +[csAcceptsControls];
  Status := dsIdle;

  ImageListExplorer.Handle := SHGetFileInfo('', 0, SHFileInfo, SizeOf(TSHFileInfo),
    SHGFI_SYSICONINDEX or SHGFI_SMALLICON);
  ImageListExplorer.ShareImages := True;
  ImageListExplorer.BlendColor := clHighlight;
  ImageListExplorer.DrawingStyle := dsTransparent;

  SetLength(FTempPath, MAX_PATH);
  SetLength(FTempPath, GetTempPath(Length(FTempPath), PChar(FTempPath)));

  AnimateThrobber.ResName := 'AVI_THROBBER';

  // I assign the Indy FTP events manually in order to get them validated at
  // compile time. The reason is that Indy has a habit of changing the
  // method/event signatures between minor releases.
  IdFTP1.OnWorkBegin := IdFTP1WorkBegin;
  IdFTP1.OnWork := IdFTP1Work;
  IdFTP1.OnWorkEnd := IdFTP1WorkEnd;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  FHistoryList.Free;
end;

procedure TFormMain.AddSourceFile(const Filename: string);
var
  ModifiedDate: TDateTime;
  FileSize: int64;
  i: integer;
begin
  FileSize := IdFTP1.Size(Filename);
  ModifiedDate := 0;

  for i := 0 to IdFTP1.DirectoryListing.Count-1 do
    if (IdFTP1.DirectoryListing[i].FileName = Filename) then
    begin
      ModifiedDate := IdFTP1.DirectoryListing[i].ModifiedDate;
      if (FileSize = -1) then
        FileSize := IdFTP1.DirectoryListing[i].Size;
      break;
    end;

  if (FileSize = -1) then
    exit;

  // Transfer the file name to the data format.
  // The content will be extracted by the target on-demand.
  i := TVirtualFileStreamDataFormat(DataFormatAdapterSource.DataFormat).FileNames.Add(Filename);
  // Set the size and timestamp attributes of the filename we just added.
  with TVirtualFileStreamDataFormat(DataFormatAdapterSource.DataFormat).FileDescriptors[i]^ do
  begin
    if (ModifiedDate <> 0) then
      ftLastWriteTime := DateTimeToFileTime(ModifiedDate)
    else
      GetSystemTimeAsFileTime(ftLastWriteTime);
    nFileSizeLow := FileSize and $00000000FFFFFFFF;
    nFileSizeHigh := (FileSize and $FFFFFFFF00000000) shr 32;
    dwFlags := FD_WRITESTIME or FD_FILESIZE or FD_PROGRESSUI;
  end;
end;

procedure TFormMain.OnMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  i: integer;
  AnyFile: boolean;
begin
  if (ListViewFiles.Selected = nil) or (Busy) then
    exit;

  TVirtualFileStreamDataFormat(DataFormatAdapterSource.DataFormat).FileNames.Clear;

  AnyFile := False;
  for i := 0 to ListViewFiles.Items.Count-1 do
    if (ListViewFiles.Items[i].Selected) and
      (TIdDirItemType(ListViewFiles.Items[i].Data) = ditFile) then
    begin
      AnyFile := True;
      break;
    end;

  if (not AnyFile) then
    exit;

  Status := dsIdle;
  if DragDetectPlus(Handle, Point(X, Y)) then
  begin
    for i := 0 to ListViewFiles.Items.Count-1 do
    begin
      if (ListViewFiles.Items[i].Selected) and
        (TIdDirItemType(ListViewFiles.Items[i].Data) = ditFile) then
        AddSourceFile(ListViewFiles.Items[i].Caption);
    end;

    if (TVirtualFileStreamDataFormat(DataFormatAdapterSource.DataFormat).FileNames.Count = 0) then
      exit;

    Status := dsDrag;

    // Perform an asynchronous drag (in a separate thread).
    if (DropEmptySource1.Execute(True) = drAsync) then
      Status := dsDragAsync
    else
      Status := dsDragAsyncFailed;
  end;
end;

procedure TFormMain.DropEmptySource1Drop(Sender: TObject;
  DragType: TDragType; var ContinueDrop: Boolean);
begin
  // Warning:
  // This event will be called in the context of the transfer thread during an
  // asynchronous transfer. See TFormMain.OnProgress for a comment on this.
  Status := dsDrop;
end;

procedure TFormMain.DropEmptySource1AfterDrop(Sender: TObject;
  DragResult: TDragResult; Optimized: Boolean);
begin
  // Warning:
  // This event will be called in the context of the transfer thread during an
  // asynchronous transfer. See TFormMain.OnProgress for a comment on this.
  Status := dsDropComplete;
end;

procedure TFormMain.DropEmptySource1GetData(Sender: TObject;
  const FormatEtc: tagFORMATETC; out Medium: tagSTGMEDIUM;
  var Handled: Boolean);
begin
  // Warning:
  // This event will be called in the context of the transfer thread during an
  // asynchronous transfer. See TFormMain.OnProgress for a comment on this.
  Status := dsGetData;
end;

type
  TFifoStreamAdapter = class(TFixedStreamAdapter, IStream)
  private
  public
    function Read(pv: Pointer; cb: Longint;
      pcbRead: PLongint): HResult; override; stdcall;
  end;

function TFifoStreamAdapter.Read(pv: Pointer; cb: Integer;
  pcbRead: PLongint): HResult;
begin
  Result := inherited Read(pv, cb, pcbRead);
  if (TFifoStream(Stream).Aborted) then
    Result := E_ABORT;
end;

procedure TFormMain.OnGetStream(Sender: TFileContentsStreamOnDemandClipboardFormat;
  Index: integer; out AStream: IStream);
var
  RingBuffer: TRingBuffer;
  ReadStream: TStream;
  FileDescriptor: PFileDescriptorW;
  FileSize: int64;
begin
  // This event handler is called by TFileContentsStreamOnDemandClipboardFormat
  // when the drop target (e.g. Explorer) requests data from the drop source (that's us).
  //
  // Warning:
  // This method will be called in the context of the transfer thread during an
  // asynchronous transfer. See TFormMain.OnProgress for a comment on this.

  AStream := nil;

  FileDescriptor := TVirtualFileStreamDataFormat(DataFormatAdapterSource.DataFormat).FileDescriptors[Index];
  FileSize := int64(FileDescriptor.nFileSizeLow) or (int64(FileDescriptor.nFileSizeHigh) shl 32);


  Status := dsGetStream;

  RingBuffer := TRingBuffer.Create(16, 1024*64);

  ReadStream := TFifoStream.CreateForRead(RingBuffer, FileSize);

  // Return the stream back to the target as an IStream. Note that the
  // target is responsible for deleting the stream (via reference counting).
  AStream := TFifoStreamAdapter.Create(ReadStream, soOwned);

  PostMessage(Handle, MSG_TRANSFER, Index, integer(RingBuffer));
end;

procedure TFormMain.MsgTransfer(var Message: TMessage);
var
  Index: integer;
  Filename: string;
  FileDescriptor: PFileDescriptorW;
  RingBuffer: TRingBuffer;
begin
  Index := Message.WParam;
  RingBuffer := TRingBuffer(Message.LParam);

  if (TransferInProgress) then
  begin
    PostMessage(Handle, MSG_TRANSFER, Index, integer(RingBuffer));
    exit;
  end;

  BeginTransfer;
  try
    FileDescriptor := TVirtualFileStreamDataFormat(DataFormatAdapterSource.DataFormat).FileDescriptors[Index];
    FCurrentFileSize := int64(FileDescriptor.nFileSizeLow) or (int64(FileDescriptor.nFileSizeHigh) shl 32);
    Filename := TVirtualFileStreamDataFormat(DataFormatAdapterSource.DataFormat).FileNames[Index];

    // Note: For this demo only lower 32 bits of file size is used for progress bar
    ProgressBar1.Max := FCurrentFileSize and $FFFFFFFF;
    ProgressBar1.Position := 0;
    ProgressBar1.Show;
    StatusBar1.Panels[0].Text := 'Reading file from FTP server...';
    Update;

    FWriteStream := TFifoStream.CreateForWrite(RingBuffer);
    try
      FWriteStream.FakeWriteDuringAbort := True;

      IdFTP1.Get(Filename, FWriteStream);
    finally
      FreeAndNil(FWriteStream);
    end;
  finally
    ProgressBar1.Hide;
    StatusBar1.Panels[0].Text := '';
    EndTransfer;
  end;
end;

procedure TFormMain.MsgProgress(var Message: TMessage);
begin
  SetProgress(Message.WParam, Message.LParam);
end;

procedure TFormMain.SetProgress(Count, MaxCount: integer);
begin
  // Make sure GUI updates are performed in the main thread.
  if (GetCurrentThreadID <> MainThreadID) then
  begin
    PostMessage(Handle, MSG_PROGRESS, Count, MaxCount);
    exit;
  end;

  ProgressBar1.Max := MaxCount;
  ProgressBar1.Position := Count;
end;

procedure TFormMain.MsgStatus(var Message: TMessage);
begin
  SetStatus(TDragDropStage(Message.WParam));
end;

procedure TFormMain.SetStatus(const Value: TDragDropStage);
var
  s: string;
begin
  // Make sure GUI updates are performed in the main thread.
  if (GetCurrentThreadID <> MainThreadID) then
  begin
    PostMessage(Handle, MSG_STATUS, ord(Value), 0);
    exit;
  end;

  if (FStatus <> Value) then
  begin
    FStatus := Value;
    case FStatus of
      dsIdle:
        s := 'Ready';
      dsDrag:
        s := 'Drag in progress';
      dsDragAsync:
        s := 'Asynchronous drag started';
      dsDragAsyncFailed:
        s := 'Asynchronous drag failed';
      dsDrop:
        s := 'Data dropped';
      dsGetData:
        s := 'Target reading data';
      dsGetStream:
        s := 'Source writing data';
      dsDropComplete:
        s := 'Drop completed';
    else
      s := '';
    end;

    StatusBar1.Panels[1].Text := s;
    Update;
  end;
end;

procedure TFormMain.ComboAddressKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_RETURN) then
  begin
    Browse(ComboAddress.Text, [boBrowse, boUpdateCombo]);
  end;
end;

procedure TFormMain.ComboAddressCloseUp(Sender: TObject);
begin
  if (FAddress <> ComboAddress.Text) then
    Browse(ComboAddress.Text);
end;

procedure TFormMain.Browse(const Address: string; Options: TBrowseOptions);
var
  i: Integer;
begin
  if (Address = '') then
    exit;

  if (boUpdateCombo in Options) then
  begin
    i := ComboAddress.Items.IndexOf(Address);
    if i = -1 then
      ComboAddress.Items.Insert(0, Address)
    else
      ComboAddress.Items.Move(i, 0);
  end;

  FAddress := Address;
  ComboAddress.Text := FAddress;

  if (boBrowse in Options) then
    PostMessage(Handle, MSG_BROWSE, ord(bkAddress), 0);
end;

procedure TFormMain.ActionBackUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (FHistoryIndex > 0) and (not Busy);
end;

procedure TFormMain.ActionBackExecute(Sender: TObject);
begin
  Dec(FHistoryIndex);
  ComboAddress.Text := FHistoryList[FHistoryIndex];
  Browse(ComboAddress.Text);
end;

procedure TFormMain.ActionForwardUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (FHistoryIndex < FHistoryList.Count-1) and (not Busy);
end;

procedure TFormMain.ActionForwardExecute(Sender: TObject);
begin
  Inc(FHistoryIndex);
  ComboAddress.Text := FHistoryList[FHistoryIndex];
  Browse(ComboAddress.Text);
end;

procedure TFormMain.ActionRefreshUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (ComboAddress.Text <> '') and (not Busy);
end;

procedure TFormMain.ActionRefreshExecute(Sender: TObject);
begin
  PostMessage(Handle, MSG_BROWSE, Ord(bkRefresh), 0);
end;

procedure TFormMain.ActionStopUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (not FAbort) and (Busy);
end;

procedure TFormMain.ActionStopExecute(Sender: TObject);
begin
  FAbort := True;
  if (not FIndyFtpAbortWarningShown) then
  begin
    FIndyFtpAbortWarningShown := True;
    ShowMessage(sIndyFtpAbortWarning);
  end;
end;

procedure TFormMain.ActionUpUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (ComboAddress.Text <> '') and (not Busy);
end;

procedure TFormMain.ActionUpExecute(Sender: TObject);
begin
  PostMessage(Handle, MSG_BROWSE, Ord(bkUp), 0);
end;

procedure TFormMain.ActionHomeUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (not Busy);
end;

procedure TFormMain.ActionHomeExecute(Sender: TObject);
begin
  ComboAddress.Text := sAddressHome;
  Browse(ComboAddress.Text);
end;

procedure TFormMain.MsgBrowse(var Message: TMessage);
var
  URI: TIdURI;
  i: integer;
  SHFileInfo: TSHFileInfo;
  s: string;
  BrowseKind: TBrowseKind;
begin
  BrowseKind := TBrowseKind(Message.WParam);
  FAborted := False;
  BeginBusy;
  Screen.Cursor := crAppStart;
  try
    ListViewFiles.Items.Clear;

    URI := TIdURI.Create(AddTrailingSlash(FAddress));
    try
      if (IdFTP1.Host <> URI.Host) and (IdFTP1.Connected) then
      begin
        StatusBar1.Panels[0].Text := 'Disconnecting from '+IdFTP1.Host+'...';
        Update;
        IdFTP1.Disconnect;
      end;
      if (not IdFTP1.Connected) then
      begin
        StatusBar1.Panels[0].Text := 'Connecting to '+URI.Host+'...';
        Update;
        IdFTP1.Host := URI.Host;
        IdFTP1.Connect;
        // Can't go up or refresh when we have lost the connection
        BrowseKind := bkAddress;
      end;
      try
        if (FAborted) then
          Abort;
        case BrowseKind of
          bkAddress:
            begin
              StatusBar1.Panels[0].Text := 'Navigating to '+URI.Path+'...';
              Update;
              IdFTP1.ChangeDir(URI.Path);
            end;
          bkUp:
            begin
              StatusBar1.Panels[0].Text := 'Navigating to parent folder...';
              Update;
              IdFTP1.ChangeDirUp;
            end;
        end;
        if (FAborted) then
          Abort;

        URI.Path := IdFTP1.RetrieveCurrentDir;
        if (FAborted) then
          Abort;

        s := URI.URI;
        i := FHistoryList.IndexOf(s);
        if (i = -1) then
        begin
          // Remove entries in HistoryList between last address and current address
          if (FHistoryIndex >= 0) and (FHistoryIndex < FHistoryList.Count-1) then
            while FHistoryList.Count-1 > FHistoryIndex do
              FHistoryList.Delete(FHistoryList.Count-1);
          FHistoryIndex := FHistoryList.Add(s);
        end else
          FHistoryIndex := i;

        Browse(s, []);

        StatusBar1.Panels[0].Text := 'Fetching '+URI.Path+'...';
        Update;
        IdFTP1.List(nil);
        if (FAborted) then
          Abort;

        ListViewFiles.Items.BeginUpdate;
        try
          for i := 0 to IdFTP1.DirectoryListing.Count -1 do
            with ListViewFiles.Items.Add do
            begin
              if (FAborted) then
                break;
              Caption := IdFTP1.DirectoryListing[i].FileName;
              SubItems.Add(DateTimeToStr(IdFTP1.DirectoryListing[i].ModifiedDate));
              SubItems.Add(SizeToStr(IdFTP1.DirectoryListing[i].Size));
              Data := pointer(IdFTP1.DirectoryListing[i].ItemType);
              ImageIndex := -1;
              case IdFTP1.DirectoryListing[i].ItemType of
                ditFile:
                  if (SHGetFileInfo(PChar(Caption), FILE_ATTRIBUTE_NORMAL, SHFileInfo, SizeOf(TSHFileInfo), SHGFI_USEFILEATTRIBUTES or SHGFI_SYSICONINDEX) <> 0) then
                    ImageIndex := SHFileInfo.iIcon;
                ditDirectory:
                  if (SHGetFileInfo(PChar(FTempPath), FILE_ATTRIBUTE_DIRECTORY or FILE_ATTRIBUTE_NORMAL, SHFileInfo, SizeOf(TSHFileInfo), SHGFI_USEFILEATTRIBUTES or SHGFI_SYSICONINDEX) <> 0) then
                    ImageIndex := SHFileInfo.iIcon;
              end;
            end;
        finally
          ListViewFiles.Items.EndUpdate;
          // Force recalc of auto size
          ListViewFiles.Width := ListViewFiles.Width+1;
        end;
        if (FAborted) then
          Abort;
      except
        IdFTP1.Disconnect;
        raise;
      end;
    finally
      URI.Free;
    end;
  finally
    StatusBar1.Panels[0].Text := '';
    Screen.Cursor := crDefault;
    EndBusy;
  end;
end;

procedure TFormMain.ListViewFilesDblClick(Sender: TObject);
begin
  if (ListViewFiles.Selected <> nil) then
  begin
    if (TIdDirItemType(ListViewFiles.Selected.Data) = ditDirectory) then
      Browse(AddTrailingSlash(ComboAddress.Text)+ListViewFiles.Selected.Caption);
  end;
end;

procedure TFormMain.Timer1Timer(Sender: TObject);
begin
  UpdateActions;
end;

procedure TFormMain.IdFTP1Work(Sender: TObject; AWorkMode: TWorkMode;
  AWorkCount: TIndyWorkCountInt);
begin
  Application.ProcessMessages;
  if (FAbort) or ((FWriteStream <> nil) and (FWriteStream.Aborted)) then
  begin
    try
      StatusBar1.Panels[0].Text := 'Aborting...';
      Update;
      if (TransferInProgress) then
        IdFTP1.Abort;
      if (FWriteStream <> nil) then
        FWriteStream.Abort;
    finally
      FAbort := False;
      FAborted := True;
    end;
  end else
  if (ProgressBar1.Visible) then
  begin
    ProgressBar1.Position := AWorkCount;
    if (FCurrentFileSize > 1024*1024) then
      StatusBar1.Panels[0].Text := Format('Reading %.0n Kb of %.0n Kb', [Int(AWorkCount div 1024), Int(integer(FCurrentFileSize div 1024))])
    else
      StatusBar1.Panels[0].Text := Format('Reading %.0n of %.0n bytes', [Int(AWorkCount), Int(integer(FCurrentFileSize))]);
    Update;
    UpdateActions;
  end;
end;

procedure TFormMain.IdFTP1WorkBegin(Sender: TObject; AWorkMode: TWorkMode;
  AWorkCountMax: TIndyWorkCountInt);
begin
  BeginTransfer;
  UpdateActions;
  Update;
end;

procedure TFormMain.IdFTP1WorkEnd(Sender: TObject; AWorkMode: TWorkMode);
begin
  EndTransfer;
end;

function TFormMain.GetBusy: boolean;
begin
  Result := (FBusyCount > 0) or TransferInProgress;
end;

procedure TFormMain.BeginBusy;
begin
  inc(FBusyCount);
  if (FBusyCount = 1) then
    AnimateThrobber.Active := True;
end;

procedure TFormMain.EndBusy;
begin
  dec(FBusyCount);
  if (FBusyCount = 0) then
    AnimateThrobber.Active := False;
end;

procedure TFormMain.BeginTransfer;
begin
  inc(FTransferCount);
  if (FTransferCount = 1) then
    BeginBusy;
end;

procedure TFormMain.EndTransfer;
begin
  dec(FTransferCount);
  if (FTransferCount = 0) then
    EndBusy;
end;

function TFormMain.GetTransferInProgress: boolean;
begin
  Result := (FTransferCount > 0);
end;

end.

