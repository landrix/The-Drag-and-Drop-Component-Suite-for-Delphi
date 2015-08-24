unit main;

interface

{$include dragdrop.inc} // Disables .NET warnings

uses
  DragDrop, DropSource, DragDropFile,
  Messages,
  ActiveX, Windows, Classes, Controls, Forms, StdCtrls, ComCtrls, ExtCtrls,
  Buttons, ActnList, ToolWin, ImgList;

const
  MSG_PROGRESS = WM_USER;
  MSG_STATUS = WM_USER+1;

type
  TDragDropStage = (dsNone, dsIdle, dsDrag, dsDragAsync, dsDragAsyncFailed, dsDrop, dsGetData, dsAbort, dsGetStream, dsDoneStream, dsDropComplete);

type
  TFormMain = class(TForm)
    Timer1: TTimer;
    DropEmptySource1: TDropEmptySource;
    DataFormatAdapterSource: TDataFormatAdapter;
    ProgressBar1: TProgressBar;
    Panel5: TPanel;
    StatusBar1: TStatusBar;
    ImageListThrobber: TImageList;
    Panel4: TPanel;
    Label2: TLabel;
    Label3: TLabel;
    RadioButtonNormal: TRadioButton;
    RadioButtonAsync: TRadioButton;
    Panel1: TPanel;
    Label4: TLabel;
    Panel2: TPanel;
    Label1: TLabel;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ActionList1: TActionList;
    ActionAbort: TAction;
    Action2: TAction;
    Action3: TAction;
    ToolButtonThrobber: TToolButton;
    Panel3: TPanel;
    MemoTrace: TMemo;
    procedure Timer1Timer(Sender: TObject);
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
    procedure ActionAbortUpdate(Sender: TObject);
    procedure ActionAbortExecute(Sender: TObject);
  private
    FStatus: TDragDropStage;
    FAborted: boolean;
    Tick: integer;
    FTransferInProgress: boolean;
    procedure SetStatus(const Value: TDragDropStage);
    procedure SetProgress(Count, MaxCount: integer);
    procedure OnGetStream(Sender: TFileContentsStreamOnDemandClipboardFormat;
      Index: integer; out AStream: IStream);
    procedure OnProgress(Sender: TObject; Count, MaxCount: integer);
    procedure MsgProgress(var Message: TMessage); message MSG_PROGRESS;
    procedure MsgStatus(var Message: TMessage); message MSG_STATUS;
  protected
    property Aborted: boolean read FAborted;
    property TransferInProgress: boolean read FTransferInProgress;
    property Status: TDragDropStage read FStatus write SetStatus;
  public
  end;

var
  FormMain: TFormMain;

implementation

{$R *.DFM}

uses
  DragDropFormats,
  ShlObj,
  Graphics;

const
  TestFileSize = 1024*1024*100; // 100Mb

procedure TFormMain.FormCreate(Sender: TObject);
begin
  // Setup event handler to let a drop target request data from our drop source.
  (DataFormatAdapterSource.DataFormat as TVirtualFileStreamDataFormat).OnGetStream := OnGetStream;

  Status := dsIdle;
end;

procedure TFormMain.Timer1Timer(Sender: TObject);
begin
  // Update the throbber to indicate that the application is responding to
  // messages (i.e. isn't blocked).
  Tick := (Tick + 1) mod 12;
  ToolButtonThrobber.ImageIndex := Tick;
  Update;
end;

procedure TFormMain.OnMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin

  Status := dsIdle;
  if DragDetectPlus(Handle, Point(X, Y)) then
  begin
    FAborted := False;
    MemoTrace.Lines.Clear;

    Status := dsDrag;

    // Transfer the file names to the data format.
    // The content will be extracted by the target on-demand.
    TVirtualFileStreamDataFormat(DataFormatAdapterSource.DataFormat).FileNames.Clear;
    TVirtualFileStreamDataFormat(DataFormatAdapterSource.DataFormat).FileNames.Add('big text file.txt');
    // Set the size and timestamp attributes of the filename we just added.
    with PFileDescriptor(TVirtualFileStreamDataFormat(DataFormatAdapterSource.DataFormat).FileDescriptors[0])^ do
    begin
      GetSystemTimeAsFileTime(ftLastWriteTime);
      nFileSizeLow := TestFileSize and $00000000FFFFFFFF;
      nFileSizeHigh := (TestFileSize and $FFFFFFFF00000000) shr 32;
      dwFlags := FD_WRITESTIME or FD_FILESIZE or FD_PROGRESSUI;
    end;

    // Determine if we should perform an async drag or a normal drag.
    if (RadioButtonAsync.Checked) then
    begin

      // Perform an asynchronous drag (in a separate thread).
      if (DropEmptySource1.Execute(True) = drAsync) then
        Status := dsDragAsync
      else
        Status := dsDragAsyncFailed;
    end else
    begin
      // Perform a normal drag (in the main thread).
      DropEmptySource1.Execute;

      Status := dsIdle;
    end;
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

procedure TFormMain.ActionAbortExecute(Sender: TObject);
begin
  FAborted := True;
  Status := dsAbort;
end;

procedure TFormMain.ActionAbortUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := TransferInProgress and (not Aborted);
end;

procedure TFormMain.DropEmptySource1AfterDrop(Sender: TObject;
  DragResult: TDragResult; Optimized: Boolean);
begin
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
  TStreamProgressEvent = procedure(Sender: TObject; Count, MaxCount: integer) of object;

  // TFakeStream is a read-only stream which produces its contents on-the-run.
  // It is used for this demo so we can simulate transfer of very large and
  // arbitrary amounts of data without using any memory.
  TFakeStream = class(TStream)
  private
    FSize, FPosition, FMaxCount: Longint;
    FProgress: TStreamProgressEvent;
    FAborted: boolean;
  protected
    property Aborted: boolean read FAborted;
  public
    constructor Create(ASize, AMaxCount: LongInt);
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    procedure SetSize(NewSize: Longint); override;
    function Write(const Buffer; Count: Longint): Longint; override;
    procedure Abort;
    property OnProgress: TStreamProgressEvent read FProgress write FProgress;
  end;

procedure TFakeStream.Abort;
begin
  FAborted := True;
end;

constructor TFakeStream.Create(ASize, AMaxCount: LongInt);
begin
  inherited Create;
  FSize := ASize;
  FMaxCount := AMaxCount;
end;

destructor TFakeStream.Destroy;
begin
//  Status := dsDoneStream;
  inherited Destroy;
end;

function TFakeStream.Read(var Buffer; Count: Integer): Longint;
begin
  Result := 0;
  if (Aborted) then
    exit;

  if (FPosition >= 0) and (Count >= 0) then
  begin
    Result := FSize - FPosition;
    if Result > 0 then
    begin
      if Result > Count then
        Result := Count;
      if Result > FMaxCount then
        Result := FMaxCount;
      FillChar(Buffer, Result, ord('X'));
      Inc(FPosition, Result);
      if Assigned(FProgress) then
        FProgress(Self, FPosition, FSize);
    end;
  end;
end;

function TFakeStream.Seek(Offset: Integer; Origin: Word): Longint;
begin
  case Origin of
    soFromBeginning: FPosition := Offset;
    soFromCurrent: Inc(FPosition, Offset);
    soFromEnd: FPosition := FSize + Offset;
  end;
  if Assigned(FProgress) then
    FProgress(Self, FPosition, FMaxCount);
  Result := FPosition;
end;

procedure TFakeStream.SetSize(NewSize: Integer);
begin
end;

function TFakeStream.Write(const Buffer; Count: Integer): Longint;
begin
  Result := 0;
end;

procedure TFormMain.OnGetStream(Sender: TFileContentsStreamOnDemandClipboardFormat;
  Index: integer; out AStream: IStream);
var
  Stream: TStream;
begin
  // Warning:
  // This method will be called in the context of the transfer thread during an
  // asynchronous transfer. See TFormMain.OnProgress for a comment on this.

  // This event handler is called by TFileContentsStreamOnDemandClipboardFormat
  // when the drop target requests data from the drop source (that's us).
  Status := dsGetStream;

  // In this demo we just create a dummy stream which contains a lot of 'X'
  // characters. In order to provide smoth feedback through the progress bar
  // (and slow the transfer down a bit) the stream will only transfer up to 32K
  // at a time - Each time TStream.Read is called, the progress bar is updated
  // via the stream's progress event.

  // Create a stream which contains the data to transfer...
  Stream := TFakeStream.Create(TestFileSize, 32*1024);
  try
    TFakeStream(Stream).OnProgress := OnProgress;
    // ...and return the stream back to the target as an IStream. Note that the
    // target is responsible for deleting the stream (via reference counting).
    AStream := TFixedStreamAdapter.Create(Stream, soOwned);
  except
    Stream.Free;
    raise;
  end;

  // Initialize progress bar
  SetProgress(0, 0);
end;

procedure TFormMain.OnProgress(Sender: TObject; Count, MaxCount: integer);
begin
  // Note that during an asynchronous transfer, some event handlers are
  // being called in the context of the transfer thread. This means that these
  // event handlers should adhere to all the normal thread safety rules (i.e.
  // don't call GDI or mess with non-thread safe objects).

  // Update progress bar to show how much data has been transfered so far.
  SetProgress(Count, MaxCount);
  if (Aborted) then
    TFakeStream(Sender).Abort;
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
  StatusMsg: string;
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
        StatusMsg := 'Ready';
      dsDrag:
        StatusMsg := 'Drag in progress';
      dsDragAsync:
        StatusMsg := 'Asynchronous drag started';
      dsDragAsyncFailed:
        StatusMsg := 'Asynchronous drag failed';
      dsDrop:
        StatusMsg := 'Data dropped';
      dsGetData:
        StatusMsg := 'Target reading data';
      dsAbort:
        StatusMsg := 'Abort requested';
      dsGetStream:
        StatusMsg := 'Source writing data';
      dsDoneStream:
        StatusMsg := 'Target done reading data';
      dsDropComplete:
        StatusMsg := 'Drop completed';
    else
      StatusMsg := '';
    end;
    case FStatus of
      dsDrop:
        FTransferInProgress := True;
      dsDropComplete:
        FTransferInProgress := False;
    end;
    StatusBar1.SimpleText := StatusMsg;
    if (StatusMsg <> '') then
      MemoTrace.Lines.Add(StatusMsg);
  end;
end;

end.

