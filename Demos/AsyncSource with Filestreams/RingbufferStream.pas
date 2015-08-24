unit RingbufferStream;

interface

uses
  SyncObjs,
  Windows,
  Classes;

type
  TStreamProgressEvent = procedure(Sender: TObject; Count, MaxCount: integer) of object;

  TBucket = record
    Size: integer;
    InUse: boolean;
    Data: byte;
  end;

  PBucket = ^TBucket;

  TRingBuffer = class
  private
    FBucketCount: integer;
    FBucketSize: integer;
    FBuffer: pointer;
    FBuckets: array of PBucket;
    FAbort: THandle;
    FEmptyBucketSemaphore: THandle;
    FFullBucketSemaphore: THandle;
    FWriteHead: integer;
    FReadTail: integer;
    FAborted: boolean;
    FRefCount: integer;

    FDebugFullBucketCount: integer;
    FDebugEmptyBucketCount: integer;
{$ifopt D+}
    procedure ValidateBucket(Bucket: PBucket);
{$endif}
  public
    constructor Create(ABucketCount: integer; ABucketSize: integer);
    destructor Destroy; override;

    procedure AddRef;
    procedure Release;

    // Write bucket functions
    function PopEmptyBucket: PBucket;
    procedure PushFullBucket(Buffer: PBucket);

    // Read bucket functions
    function PopFullBucket: PBucket;
    procedure PushEmptyBucket(Buffer: PBucket);

    procedure Abort;
    property Aborted: boolean read FAborted;
    property BucketCount: integer read FBucketCount;
    property BucketSize: integer read FBucketSize;
  end;

  TFifoStream = class(TStream)
  private
    FSize: int64;
    FBuffer: TRingBuffer;
    FReadOnly: boolean;
    FWriteOnly: boolean;
    FAborted: boolean;
    FPosition: int64;
    FOverflowBucket: TMemoryStream;
    FFakeWriteDuringAbort: boolean;
    FReset: boolean;
  protected
    function GetAborted: boolean;
    function GetSize: Int64; override;
    procedure SetSize(NewSize: Longint); override;
  public
    constructor Create(ABuffer: TRingBuffer; ASize: int64 = 0);
    constructor CreateForRead(ABuffer: TRingBuffer; ASize: int64);
    constructor CreateForWrite(ABuffer: TRingBuffer);
    destructor Destroy; override;

    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;

    procedure Abort;
    property Aborted: boolean read GetAborted;
    property FakeWriteDuringAbort: boolean read FFakeWriteDuringAbort write FFakeWriteDuringAbort;
  end;

implementation

uses
  SysUtils;

procedure TRingBuffer.Abort;
begin
  FAborted := True;
  SetEvent(FAbort);
end;

procedure TRingBuffer.AddRef;
begin
  InterlockedIncrement(FRefCount);
end;

constructor TRingBuffer.Create(ABucketCount, ABucketSize: integer);
var
  Size: integer;
  i: integer;
  p: PAnsiChar;
begin
  inherited Create;

  FBucketCount := ABucketCount;
  FBucketSize := ABucketSize;

  Size := FBucketCount*(SizeOf(TBucket)-1+FBucketSize);
{$ifopt D+}
  // Add guard bytes
  inc(Size, FBucketCount*SizeOf(DWORD));
{$endif}
  GetMem(FBuffer, Size);
  FillChar(FBuffer^, Size, 0);

  SetLength(FBuckets, FBucketCount);
  p := FBuffer;
  for i := 0 to FBucketCount-1 do
  begin
    FBuckets[i] := PBucket(p);
    inc(p, SizeOf(TBucket)-1+FBucketSize);
{$ifopt D+}
    PDWORD(p)^:= $BAADF00D;
    inc(p, SizeOf(DWORD));
{$endif}
  end;

  FDebugEmptyBucketCount := FBucketCount;

  FFullBucketSemaphore := CreateSemaphore(nil, 0, FBucketCount, nil);
  if (FFullBucketSemaphore = 0) then
    RaiseLastOSError;
  FEmptyBucketSemaphore := CreateSemaphore(nil, FBucketCount, FBucketCount, nil);
  if (FEmptyBucketSemaphore = 0) then
    RaiseLastOSError;
  FAbort := CreateEvent(nil, True, False, nil);
  if (FAbort = 0) then
    RaiseLastOSError;
end;

destructor TRingBuffer.Destroy;
begin
  CloseHandle(FFullBucketSemaphore);
  CloseHandle(FEmptyBucketSemaphore);
  CloseHandle(FAbort);
  FreeMem(FBuffer);

  inherited Destroy;
end;

function TRingBuffer.PopFullBucket: PBucket;
var
  Handles: array[0..1] of THandle;
  Res: DWord;
  n: integer;
begin
  Result := FBuckets[FReadTail];

  // Wait for filled bucket to become available (or abort)
  Handles[0] := FAbort;
  Handles[1] := FFullBucketSemaphore;
  Res := WaitForMultipleObjects(Length(Handles), @Handles[0], False, INFINITE);
  if (Res = WAIT_OBJECT_0) or ((Res >= WAIT_ABANDONED_0) and (Res <= WAIT_ABANDONED_0+Length(Handles)-1)) then
  begin
    Result := nil;
    exit;
  end else
  if (Res = WAIT_FAILED) then
    RaiseLastOSError;

  n := InterlockedDecrement(FDebugFullBucketCount);
  OutputDebugString(PChar(Format('< Full buckets: %d', [n])));

  ASSERT(Res = WAIT_OBJECT_0+1);
  ASSERT(Result.InUse, 'Full bucket marked empty');
  ASSERT(Result.Size <= FBucketSize, 'Invalid bucket size');
{$ifopt D+}
  ValidateBucket(Result);
{$endif}
end;

function TRingBuffer.PopEmptyBucket: PBucket;
var
  Handles: array[0..1] of THandle;
  Res: DWord;
  n: integer;
begin
  Result := FBuckets[FWriteHead];

  // Wait for empty bucket to become available (or abort)
  Handles[0] := FAbort;
  Handles[1] := FEmptyBucketSemaphore;
  Res := WaitForMultipleObjects(Length(Handles), @Handles[0], False, INFINITE);
  if (Res = WAIT_OBJECT_0) or ((Res >= WAIT_ABANDONED_0) and (Res <= WAIT_ABANDONED_0+Length(Handles)-1)) then
  begin
    Result := nil;
    exit;
  end else
  if (Res = WAIT_FAILED) then
    RaiseLastOSError;

  n := InterlockedDecrement(FDebugEmptyBucketCount);
  OutputDebugString(PChar(Format('< Empty buckets: %d', [n])));

  ASSERT(Res = WAIT_OBJECT_0+1);
  ASSERT(not Result.InUse, 'Empty bucket marked full');
  ASSERT(Result.Size = 0, 'Invalid bucket size');
{$ifopt D+}
  ValidateBucket(Result);
{$endif}
end;

procedure TRingBuffer.PushEmptyBucket(Buffer: PBucket);
var
  n: integer;
begin
  ASSERT(FBuckets[FReadTail] = Buffer, 'Push of empty bucket not at tail');
  ASSERT(Buffer.InUse, 'New empty bucket already marked empty');
{$ifopt D+}
  ValidateBucket(Buffer);
{$endif}
  Buffer.InUse := False;
  Buffer.Size := 0;
  FReadTail := (FReadTail+1) mod FBucketCount;
  n := InterlockedIncrement(FDebugEmptyBucketCount);
  OutputDebugString(PChar(Format('> Empty buckets: %d', [n])));
  // Make empty bucket available
  Win32Check(ReleaseSemaphore(FEmptyBucketSemaphore, 1, nil));
end;

procedure TRingBuffer.PushFullBucket(Buffer: PBucket);
var
  n: integer;
begin
  ASSERT(FBuckets[FWriteHead] = Buffer, 'Push of full bucket not at head');
  ASSERT(not Buffer.InUse, 'New full bucket already marked full');
  ASSERT(Buffer.Size <= FBucketSize, 'Invalid bucket size');
{$ifopt D+}
  ValidateBucket(Buffer);
{$endif}
  Buffer.InUse := True;
  FWriteHead := (FWriteHead+1) mod FBucketCount;
  n := InterlockedIncrement(FDebugFullBucketCount);
  OutputDebugString(PChar(Format('> Full buckets: %d', [n])));
  // Make filled bucket available
  Win32Check(ReleaseSemaphore(FFullBucketSemaphore, 1, nil));
end;

procedure TRingBuffer.Release;
begin
  if (InterlockedDecrement(FRefCount) = 0) then
    Free;
end;

{$ifopt D+}
procedure TRingBuffer.ValidateBucket(Bucket: PBucket);
begin
  ASSERT(PDWORD(integer(Bucket)+SizeOf(TBucket)-1+FBucketSize)^ = $BAADF00D, 'Bucket corrupt');
end;
{$endif}

procedure TFifoStream.Abort;
begin
  FAborted := True;
  FBuffer.Abort;
end;

constructor TFifoStream.Create(ABuffer: TRingBuffer; ASize: int64);
begin
  inherited Create;
  FSize := ASize;
  FBuffer := ABuffer;
  FBuffer.AddRef;
  FOverflowBucket := TMemoryStream.Create;
end;

constructor TFifoStream.CreateForRead(ABuffer: TRingBuffer; ASize: int64);
begin
  Create(ABuffer, ASize);
  FReadOnly := True;
end;

constructor TFifoStream.CreateForWrite(ABuffer: TRingBuffer);
begin
  Create(ABuffer);
  FWriteOnly := True;
end;

destructor TFifoStream.Destroy;
begin
  FOverflowBucket.Free;
  FBuffer.Release;
  inherited Destroy;
end;

function TFifoStream.GetAborted: boolean;
begin
  Result := FAborted or FBuffer.Aborted;
end;

function TFifoStream.GetSize: Int64;
begin
  Result := FSize;
end;

function TFifoStream.Read(var Buffer; Count: Integer): Longint;
var
  Bucket: PBucket;
begin
  if (FWriteOnly) or (FReset) then
    raise Exception.Create('Invalid stream operation');

  if (Aborted) or (FPosition >= FSize) then
  begin
    Result := 0;
    exit;
  end;

  if (FOverflowBucket.Position >= FOverflowBucket.Size) then
  begin
    // Get a filled bucket
    Bucket := FBuffer.PopFullBucket;
    if (Bucket = nil) or (Aborted) then
    begin
      Result := 0;
      exit;
    end;

    FOverflowBucket.Position := 0;
    FOverflowBucket.Size := Bucket^.Size;

    // Transfer data from the bucket to the overflow buffer.
    FOverflowBucket.Write(Bucket^.Data, Bucket^.Size);
    FOverflowBucket.Position := 0;

    // Release the bucket
    FBuffer.PushEmptyBucket(Bucket);
  end;

  // Transfer data from the overflow buffer to the drop target
  Result := FOverflowBucket.Read(Buffer, Count);
  inc(FPosition, Result);
end;

function TFifoStream.Seek(Offset: Integer; Origin: Word): Longint;
var
  NewPos: int64;
begin
  NewPos := 0;
  case Origin of
    soFromBeginning:
      NewPos := Offset;
    soFromCurrent:
      NewPos := FPosition+Offset;
    soFromEnd:
      NewPos := FSize-Offset;
  end;
  if (NewPos <> FPosition) then
  begin
    if (NewPos = 0) then
    begin
      FReset := True;
      Abort;
    end else
      raise Exception.Create('Invalid stream operation');
  end;
  Result := NewPos;
end;

procedure TFifoStream.SetSize(NewSize: Integer);
begin
  FSize := NewSize;
end;

function TFifoStream.Write(const Buffer; Count: Integer): Longint;
var
  Bucket: PBucket;
  Size: integer;
  p: pointer;
begin
  if (FReadOnly) or (FReset) then
    raise Exception.Create('Invalid stream operation');

  Result := 0;
  if (Aborted) then
  begin
    if (FakeWriteDuringAbort) then
      Result := Count;
    exit;
  end;

  p := @Buffer;
  while (Count > 0) do
  begin
    Bucket := FBuffer.PopEmptyBucket;
    if (Bucket = nil) or (Aborted) then
    begin
      if (FakeWriteDuringAbort) then
        inc(Result, Count)
      else
        Result := 0;
      exit;
    end;

    Size := Count;
    if (Size > FBuffer.BucketSize) then
      Size := FBuffer.BucketSize;

    Move(p^, Bucket^.Data, Size);
    Bucket^.Size := Size;
    FBuffer.PushFullBucket(Bucket);

    dec(Count, Size);
    inc(Result, Size);
    inc(integer(p), Size);

    if (FWriteOnly) then
      inc(FPosition, Result);
  end;
end;

end.
