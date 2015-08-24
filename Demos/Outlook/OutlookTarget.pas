unit OutlookTarget;

interface

{$include dragdrop.inc} // Disables .NET warnings

uses
  ActiveX,//!!!

  MapiDefs,
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, ComCtrls, StdCtrls, DragDrop, DropTarget, DragDropText, ImgList,
  Menus, ActnList;

type
  TMessage = class(TObject)
  private
    FMessage: IMessage;
    FStorage: IStorage;
    FAttachments: TInterfaceList;
    FAttachmentsLoaded: boolean;
    function GetAttachments: TInterfaceList;
  public
    constructor Create(const AMessage: IMessage; const AStorage: IStorage);
    destructor Destroy; override;
    procedure SaveToStream(Stream: TStream);
    property Msg: IMessage read FMessage;
    property Attachments: TInterfaceList read GetAttachments;
  end;

  TFormOutlookTarget = class(TForm)
    DataFormatAdapterOutlook: TDataFormatAdapter;
    DropEmptyTarget1: TDropEmptyTarget;
    ImageListSmall: TImageList;
    StatusBar1: TStatusBar;
    ImageListBig: TImageList;
    PanelMain: TPanel;
    ListViewBrowser: TListView;
    PanelFrom: TPanel;
    Label1: TLabel;
    EditFrom: TEdit;
    PanelTo: TPanel;
    Label2: TLabel;
    ScrollBox1: TScrollBox;
    ListViewTo: TListView;
    PanelSubject: TPanel;
    Label3: TLabel;
    EditSubject: TEdit;
    MemoBody: TRichEdit;
    SplitterAttachments: TSplitter;
    ListViewAttachments: TListView;
    SplitterBrowser: TSplitter;
    PopupMenuList: TPopupMenu;
    MenuAttachmentViewLargeIcons: TMenuItem;
    MenuAttachmentViewSmallIcons: TMenuItem;
    MenuAttachmentViewList: TMenuItem;
    MenuAttachmentViewDetails: TMenuItem;
    MenuAttachmentOpen: TMenuItem;
    MenuAttachmentView: TMenuItem;
    N1: TMenuItem;
    PopupMenuMain: TPopupMenu;
    ActionList1: TActionList;
    ActionPaste: TAction;
    Paste1: TMenuItem;
    ActionAttachmentOpen: TAction;
    ActionAttachmentViewLargeIcons: TAction;
    ActionAttachmentViewSmallIcons: TAction;
    ActionAttachmentViewList: TAction;
    ActionAttachmentViewDetails: TAction;
    ActionMessageClear: TAction;
    N2: TMenuItem;
    Clear1: TMenuItem;
    ActionMessageSave: TAction;
    Savetofile1: TMenuItem;
    N3: TMenuItem;
    SaveDialog1: TSaveDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure DropTextTarget1Drop(Sender: TObject; ShiftState: TShiftState;
      APoint: TPoint; var Effect: Integer);
    procedure ListViewToInfoTip(Sender: TObject; Item: TListItem;
      var InfoTip: String);
    procedure ListViewAttachmentsDblClick(Sender: TObject);
    procedure ListViewAttachmentsDeletion(Sender: TObject;
      Item: TListItem);
    procedure ListViewBrowserDeletion(Sender: TObject; Item: TListItem);
    procedure ListViewBrowserSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure ListViewAttachmentsResize(Sender: TObject);
    procedure ActionPasteUpdate(Sender: TObject);
    procedure ActionPasteExecute(Sender: TObject);
    procedure ActionAttachmentOpenExecute(Sender: TObject);
    procedure ActionAttachmentOpenUpdate(Sender: TObject);
    procedure ActionAttachmentViewLargeIconsExecute(Sender: TObject);
    procedure ActionAttachmentViewSmallIconsExecute(Sender: TObject);
    procedure ActionAttachmentViewListExecute(Sender: TObject);
    procedure ActionAttachmentViewDetailsExecute(Sender: TObject);
    procedure ActionMessageClearExecute(Sender: TObject);
    procedure ActionMessageSaveUpdate(Sender: TObject);
    procedure ActionMessageSaveExecute(Sender: TObject);
  private
    FCleanUpList: TStringList;
    FOwnedMessage: TMessage;
    FCurrentMessage: TMessage;
    FHasMessageSession: boolean;
    FChildForms: TList;
    FParentForm: TFormOutlookTarget;
  protected
    procedure Reset;
    procedure ResetView;
    procedure CleanUp;
    procedure ViewMessage(AMessage: TMessage);
    procedure ReadBodyText(const AMessage: IMessage);
    procedure AddAttachment(const Attachment: IAttach; Number: integer);
    function GetSender(const AMessage: IMessage): string;
    function GetSubject(const AMessage: IMessage): string;
    procedure FormatAttachmentList;
    procedure SetParentForm(const Value: TFormOutlookTarget);
    property ParentForm: TFormOutlookTarget read FParentForm write SetParentForm;
  public
    property OwnedMessage: TMessage read FOwnedMessage write FOwnedMessage;
  end;

var
  FormOutlookTarget: TFormOutlookTarget;

implementation

{$R *.DFM}

uses
  MapiUtil,
  MapiTags,
  ComObj,
//!!!  ActiveX,
  ShellAPI,
  Contnrs,
  DragDropFormats,
  // Note: In order to get the Outlook data format support linked into the
  // application, we have to include the appropriate units in the uses clause.
  // If you forget to do this, you will get a run time error.
  // The DragDropFile unit contains the TFileContentsStorageClipboardFormat class.
  // The DragDropInternet unit contains the TOutlookDataFormat class.
  DragDropFile,
  DragDropInternet;



constructor TMessage.Create(const AMessage: IMessage; const AStorage: IStorage);
begin
  FMessage := AMessage;
  FStorage := AStorage;
  FAttachments := TInterfaceList.Create;
end;

destructor TMessage.Destroy;
begin
  FAttachments.Free;
  FMessage := nil;
  inherited Destroy;
end;

{$RANGECHECKS OFF}
function TMessage.GetAttachments: TInterfaceList;
const
  AttachmentTags: packed record
    Values: ULONG;
    PropTags: array[0..0] of ULONG;
  end = (Values: 1; PropTags: (PR_ATTACH_NUM));

var
  Table: IMAPITable;
  Rows: PSRowSet;
  i: integer;
  Attachment: IAttach;
begin
  if (not FAttachmentsLoaded) then
  begin
    FAttachmentsLoaded := True;
    (*
    ** Get list of attachment interfaces from message
    **
    ** Note: This will only succeed the first time it is called for an IMessage.
    ** The reason is probably that it is illegal (according to MSDN) to call
    ** IMessage.OpenAttach more than once for a given attachment. However, it
    ** might also be a bug in my code, but, whatever the reason, the solution is
    ** beyond the scope of this demo.
    **
    ** Let me know if you find a solution.
    *)
    if (Succeeded(FMessage.GetAttachmentTable(0, Table))) then
    begin
      if (Succeeded(HrQueryAllRows(Table, PSPropTagArray(@AttachmentTags), nil, nil, 0, Rows))) then
        try
          for i := 0 to integer(Rows.cRows)-1 do
          begin
            // Get one attachment at a time
            if (Rows.aRow[i].lpProps[0].ulPropTag and PROP_TYPE_MASK <> PT_ERROR) and
              (Succeeded(FMessage.OpenAttach(Rows.aRow[i].lpProps[0].Value.l, IAttach, 0, Attachment))) then
              FAttachments.Add(Attachment);
          end;

        finally
          FreePRows(Rows);
        end;
      Table := nil;
    end;
  end;
  Result := FAttachments;
end;

procedure TMessage.SaveToStream(Stream: TStream);
const
  CLSID_MailMessage:TGUID='{00020D0B-0000-0000-C000-000000000046}';
var
  LockBytes: ILockBytes;
  Storage: IStorage;
(*
  Malloc: IMalloc;
  MsgSession: pointer;
  NewMsg: IUnknown;
  ExcludeTags: PSPropTagArray;
*)
//  ProblemArray: PSPropProblemArray;
  Memory: HGLOBAL;
  Buffer: pointer;
  Size: integer;
begin
  (*
  ** This implementation is based, in part, on the Microsoft knowledgebase
  ** article:
  ** Save Message to MSG Compound File
  ** http://support.microsoft.com/kb/171907
  *)
  Memory := GlobalAlloc(GMEM_MOVEABLE, 0);
  try

    OleCheck(CreateILockBytesOnHGlobal(Memory, True, LockBytes));
    try

      // Create compound file
      OleCheck(StgCreateDocfileOnILockBytes(LockBytes,
        STGM_TRANSACTED or STGM_READWRITE or STGM_CREATE, 0, Storage));
      try

        Storage.Commit(STGC_DEFAULT);
        FStorage.CopyTo(0, nil, nil, Storage);
        Storage.Commit(STGC_DEFAULT);
(*
        Malloc := IMalloc(MAPIGetDefaultMalloc);
        try

          // Open an IMessage session.
          OleCheck(OpenIMsgSession(Malloc, 0, MsgSession));
          try

            // Open an IMessage interface on an IStorage object
            OleCheck(OpenIMsgOnIStg(MsgSession,
              @MAPIAllocateBuffer, @MAPIAllocateMore, @MAPIFreeBuffer, Malloc,
              nil, Storage, nil, 0, 0, NewMsg));
            try

              // write the CLSID to the IStorage instance - pStorage. This will
              // only work with clients that support this compound document type
              // as the storage medium. If the client does not support
              // CLSID_MailMessage as the compound document, you will have to use
              // the CLSID that it does support.
              OleCheck(WriteClassStg(Storage, CLSID_MailMessage));

              GetMem(ExcludeTags, SizeOf(TSPropTagArray)+SizeOf(ULONG)*6);
              try

                // Exclude a few properties - just like the MSDN sample
                ExcludeTags.cValues := 7;
                ExcludeTags.aulPropTag[0] := PR_ACCESS;
                ExcludeTags.aulPropTag[ExcludeTags.cValues-6] := PR_BODY;
                ExcludeTags.aulPropTag[ExcludeTags.cValues-5] := PR_RTF_SYNC_BODY_COUNT;
                ExcludeTags.aulPropTag[ExcludeTags.cValues-4] := PR_RTF_SYNC_BODY_CRC;
                ExcludeTags.aulPropTag[ExcludeTags.cValues-3] := PR_RTF_SYNC_BODY_TAG;
                ExcludeTags.aulPropTag[ExcludeTags.cValues-2] := PR_RTF_SYNC_PREFIX_COUNT;
                ExcludeTags.aulPropTag[ExcludeTags.cValues-1] := PR_RTF_SYNC_TRAILING_COUNT;

                // Copy message properties
//                Msg.CopyTo(0, TGUID(nil^), ExcludeTags, 0, nil, IMessage, pointer(NewMsg), 0, ProblemArray);
                OleCheck(Msg.CopyTo(0, TGUID(nil^), ExcludeTags, 0, nil, IMessage, pointer(NewMsg), 0, PSPropProblemArray(nil^)));

              finally
                FreeMem(ExcludeTags);
              end;

              IMessage(NewMsg).SaveChanges(0);
              Storage.Commit(STGC_DEFAULT);

            finally
              pointer(NewMsg) := nil;
            end;

          finally
            CloseIMsgSession(MsgSession);
          end;

        finally
          Malloc := nil;
        end;
  *)
      finally
        Storage := nil;
      end;

      Size := GlobalSize(Memory);
      Buffer := GlobalLock(Memory);
      try
        Stream.Write(Buffer^, Size);
      finally
        GlobalUnlock(Memory);
      end;

    finally
      LockBytes := nil;
    end;

  finally
    GlobalFree(Memory);
  end;
end;

{$RANGECHECKS ON}

type
  TMAPIINIT_0 =
    record
      Version: ULONG;
      Flags: ULONG;
    end;

  PMAPIINIT_0 = ^TMAPIINIT_0;
  TMAPIINIT = TMAPIINIT_0;
  PMAPIINIT = ^TMAPIINIT;

const
  MAPI_INIT_VERSION = 0;
  MAPI_MULTITHREAD_NOTIFICATIONS = $00000001;
  MAPI_NO_COINIT = $00000008;

var
  MapiInit: TMAPIINIT_0 = (Version: MAPI_INIT_VERSION; Flags: 0);

procedure TFormOutlookTarget.FormCreate(Sender: TObject);
var
  SHFileInfo: TSHFileInfo;
begin
  LoadMAPI32;

  try
    // It appears that for for Win XP and later it is OK to let MAPI call
    // coInitialize.
    // V5.1 = WinXP.
//    if ((Win32MajorVersion shl 16) or Win32MinorVersion < $00050001) then
//      MapiInit.Flags := MapiInit.Flags or MAPI_NO_COINIT;

    OleCheck(MAPIInitialize(@MapiInit));
  except
    on E: Exception do
      ShowMessage(Format('Failed to initialize MAPI: %s', [E.Message]));
  end;

  // FCleanUpList contains a list of temporary files that should be deleted
  // before the application exits.
  FCleanUpList := TStringList.Create;

  // FChildForms contains a list of forms that has been created to view a
  // message attachment.
  // It is important that child forms, and the messages they wrap, are destroyed
  // before we destroy this form and its messages.
  // If we performed the clean up in a proper destructor
  // (i.e. "destructor Destroy; override") instead of the OnDestroy event
  // handler then the FChildForms list wouldn't be neccesary.
  FChildForms := TObjectList.Create(True);

  // Get the system image list to use for the attachment listview.
  ImageListSmall.Handle := SHGetFileInfo('', 0, SHFileInfo, sizeOf(SHFileInfo),
    SHGFI_SYSICONINDEX or SHGFI_SMALLICON);
  ImageListBig.Handle := SHGetFileInfo('', 0, SHFileInfo, sizeOf(SHFileInfo),
    SHGFI_SYSICONINDEX or SHGFI_LARGEICON);

  ListViewBrowser.Visible := False;
  SplitterBrowser.Visible := False;
end;

procedure TFormOutlookTarget.FormDestroy(Sender: TObject);
begin
  ParentForm := nil;

  Reset;
  CleanUp;
  FreeAndNil(FCleanUpList);
  FreeAndNil(FOwnedMessage);
  FreeAndNil(FChildForms);

  MAPIUninitialize;
end;

procedure TFormOutlookTarget.CleanUp;
var
  i: integer;
  OutlookDataFormat: TOutlookDataFormat;
begin
  FCurrentMessage := nil;

  for i := 0 to FCleanUpList.Count-1 do
    try
      DeleteFile(FCleanUpList[i]);
    except
      // Ignore errors - nothing we can do about it anyway.
    end;

  FCleanUpList.Clear;

  if (FHasMessageSession) then
  begin
    OutlookDataFormat := DataFormatAdapterOutlook.DataFormat as TOutlookDataFormat;
    OutlookDataFormat.Messages.UnlockSession;
    FHasMessageSession := False;
  end;
end;

procedure TFormOutlookTarget.Reset;
begin
  FChildForms.Clear;
  ListViewBrowser.Items.Clear;
  ListViewBrowser.Visible := False;
  SplitterBrowser.Visible := False;
  ResetView;
end;

procedure TFormOutlookTarget.ResetView;
begin
  FCurrentMessage := nil;
  ListViewTo.Items.Clear;
  ListViewTo.Height := 0;
  EditFrom.Text := '';
  EditFrom.Hint := '';
  EditSubject.Text := '';
  MemoBody.Lines.Clear;
  ListViewAttachments.Items.Clear;
  SplitterAttachments.Hide;
  ListViewAttachments.Hide;
end;

procedure TFormOutlookTarget.SetParentForm(const Value: TFormOutlookTarget);
begin
  if (FParentForm <> Value) then
  begin
    if (FParentForm <> nil) then
      FParentForm.FChildForms.Extract(Self);
    FParentForm := Value;
    if (FParentForm <> nil) then
      FParentForm.FChildForms.Add(Self);
  end;
end;

procedure TFormOutlookTarget.DropTextTarget1Drop(Sender: TObject;
  ShiftState: TShiftState; APoint: TPoint; var Effect: Integer);
var
  OutlookDataFormat: TOutlookDataFormat;
  i: integer;
  Item: TListItem;
  AMessage: IMessage;
begin
  // Check if we have a data format and if so...
  if (DataFormatAdapterOutlook.DataFormat <> nil) then
  begin
    // ...Extract the dropped data from it.
    OutlookDataFormat := DataFormatAdapterOutlook.DataFormat as TOutlookDataFormat;

    (*
    ** Reset everything
    *)
    Reset;

    CleanUp;

    OutlookDataFormat.Messages.LockSession;
    FHasMessageSession := True;

    // Get all the dropped messages
    for i := 0 to OutlookDataFormat.Messages.Count-1 do
    begin
      // Get an IMessage interface
      if (Supports(OutlookDataFormat.Messages[i], IMessage, AMessage)) then
      begin
        try
          Item := ListViewBrowser.Items.Add;
          Item.Caption := GetSender(AMessage);
          Item.SubItems.Add(GetSubject(AMessage));
          Item.Data := TMessage.Create(AMessage, OutlookDataFormat.Storages[i]);
        finally
          AMessage := nil;
        end;
      end;
    end;

    StatusBar1.SimpleText := Format('%d messages dropped', [ListViewBrowser.Items.Count]);

    if (ListViewBrowser.Items.Count > 1) then
    begin
      ListViewBrowser.Visible := True;
      SplitterBrowser.Left := Width;
      SplitterBrowser.Visible := True;
    end;

    // If there's only one message, display it without further ado
    if (ListViewBrowser.Items.Count = 1) then
      ViewMessage(TMessage(ListViewBrowser.Items[0].Data))
    else
      // Otherwise select and display the first message
      if (ListViewBrowser.Items.Count > 0) then
        ListViewBrowser.Items[0].Selected := True;
  end;
end;

procedure TFormOutlookTarget.ListViewBrowserDeletion(Sender: TObject;
  Item: TListItem);
begin
  // Zap TMessage object
  if (Item.Data <> nil) then
    TObject(Item.Data).Free;
end;

procedure TFormOutlookTarget.ListViewBrowserSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  (*
  ** Display the message details when we select a message in the browser listview
  *)
  if (Selected) and (Item.Data <> nil) then
    ViewMessage(TMessage(Item.Data));
end;

function TFormOutlookTarget.GetSender(const AMessage: IMessage): string;
var
  Prop: PSPropValue;
begin
  if (Succeeded(HrGetOneProp(AMessage, PR_SENDER_NAME, Prop))) then
    try
      if (Prop.ulPropTag and PT_UNICODE = PT_UNICODE) then
        { TODO : TSPropValue.Value.lpszW is declared wrong }
        Result := String(PWideChar(Prop.Value.lpszW))
      else
        Result := String(Prop.Value.lpszA);
    finally
      MAPIFreeBuffer(Prop);
    end
  else
    Result := '';
end;

function TFormOutlookTarget.GetSubject(const AMessage: IMessage): string;
var
  Prop: PSPropValue;
begin
  if (Succeeded(HrGetOneProp(AMessage, PR_SUBJECT, Prop))) then
    try
      if (Prop.ulPropTag and PT_UNICODE = PT_UNICODE) then
        { TODO : TSPropValue.Value.lpszW is declared wrong }
        Result := String(PWideChar(Prop.Value.lpszW))
      else
        Result := String(Prop.Value.lpszA);
    finally
      MAPIFreeBuffer(Prop);
    end
  else
    Result := '';
end;

{$RANGECHECKS OFF}
procedure TFormOutlookTarget.ViewMessage(AMessage: TMessage);
const
  AddressTags: packed record
    Values: ULONG;
    PropTags: array[0..1] of ULONG;
  end = (Values: 2; PropTags: (PR_DISPLAY_NAME, PR_EMAIL_ADDRESS_A));

var
  i, j: integer;
  Prop: PSPropValue;
  Table: IMAPITable;
  Rows: PSRowSet;
  Value: string;
  r: TRect;
  ListItem: TListItem;
begin
  ResetView;

  FCurrentMessage := AMessage;

  (*
  ** Get Recipients
  *)
  if (Succeeded(AMessage.Msg.GetRecipientTable(0, Table))) then
  begin
    if (Succeeded(HrQueryAllRows(Table, PSPropTagArray(@AddressTags), nil, nil, 0, Rows))) then
      try
        for i := 0 to integer(Rows.cRows)-1 do
        begin
          ListItem := ListViewTo.Items.Add;

          for j := 0 to Rows.aRow[i].cValues-1 do
          begin
            if (Rows.aRow[i].lpProps[j].ulPropTag and PT_UNICODE = PT_UNICODE) then
              { TODO : TSPropValue.Value.lpszW is declared wrong }
              Value := String(PWideChar(Rows.aRow[i].lpProps[j].Value.lpszW))
            else
              Value := String(Rows.aRow[i].lpProps[j].Value.lpszA);
            if (j = 0) then
              ListItem.Caption := Value
            else
              ListItem.SubItems.Add(Value);
          end;
        end;

        if (ListViewTo.Items.Count > 0) then
        begin
          r := ListViewTo.Items[0].DisplayRect(drBounds);
          ListViewTo.Height := ListViewTo.Items.Count*(r.Bottom-r.Top+2);
        end;

      finally
        FreePRows(Rows);
      end;
    Table := nil;
  end;

  (*
  ** Get sender
  *)
  if (Succeeded(HrGetOneProp(AMessage.Msg, PR_SENDER_EMAIL_ADDRESS_A, Prop))) then
    try
      Value := String(Prop.Value.lpszA);
    finally
      MAPIFreeBuffer(Prop);
    end
  else
    Value := '';

  EditFrom.Text := GetSender(AMessage.Msg);
  EditFrom.Hint := Value;

  (*
  ** Get subject
  *)
  EditSubject.Text := GetSubject(AMessage.Msg);

  (*
  ** Get body
  *)
  ReadBodyText(AMessage.Msg);

  (*
  ** Get attachments
  *)
  for i := 0 to AMessage.Attachments.Count-1 do
    AddAttachment(IAttach(AMessage.Attachments[i]), i);

  (*
  ** Only display attachment listview if msg has any attachments.
  *)
  if (ListViewAttachments.Items.Count > 0) then
  begin
    FormatAttachmentList;
    r := ListViewAttachments.Items[0].DisplayRect(drBounds);
    ListViewAttachments.Height := r.Bottom-r.Top+6;
    ListViewAttachments.Top := 0;
    ListViewAttachments.Show;
    SplitterAttachments.Top := 0;
    SplitterAttachments.Show;
  end;

end;
{$RANGECHECKS ON}

procedure TFormOutlookTarget.ListViewToInfoTip(Sender: TObject;
  Item: TListItem; var InfoTip: String);
begin
  // Display email address as a hint
  InfoTip := Item.SubItems[0];
end;

procedure TFormOutlookTarget.ReadBodyText(const AMessage: IMessage);
var
  Buffer: array of byte;
  Data: TMemoryStream;
  SourceStream: IStream;
  Size: integer;
  Dummy: int64;
const
  BufferSize = 64*1024; // 64Kb
  MaxMessageSize = 256*1024; // 256 Kb
begin
  MemoBody.Lines.Clear;

  (*
  ** We could use HrGetOneProp to get the message body, but since we would like
  ** to limit the amount of data we read, and the body can potentially be quite
  ** big, we read the body text from a stream instead.
  **
  if (Succeeded(HrGetOneProp(AMessage, PR_BODY, Prop))) then
    try
      MemoBody.Lines.Text := Prop.Value.lpszA;
    finally
      MAPIFreeBuffer(Prop);
    end;
  *)

  if (Succeeded(AMessage.OpenProperty(PR_BODY, IStream, STGM_READ, 0, IUnknown(SourceStream)))) then
  begin
    SetLength(Buffer, BufferSize);
    Data := TMemoryStream.Create;
    try
      // Read up to 256Kb from stream
      SourceStream.Seek(0, STREAM_SEEK_SET, Dummy);
      while (Data.Size < MaxMessageSize) and
        (Succeeded(SourceStream.Read(@Buffer[0], Length(Buffer), @Size))) and
        (Size > 0) do
      begin
        Data.Write(Buffer[0], Size);
      end;
      // Write terminating zero
      Buffer[0] := 0;
      Data.Write(Buffer[0], 1);
      Data.Write(Buffer[0], 1);
      // Transfer the data we just read from the buffer to the memo
      MemoBody.Lines.Text := PChar(Data.Memory);
    finally
      Data.Free;
    end;
  end;
end;

const
  // Attachment listview column indices
  ColType = 0;
  ColSize = 1;
  ColDisplay = 2;
  ColFile = 3;

procedure TFormOutlookTarget.ActionAttachmentOpenExecute(Sender: TObject);

  procedure Execute(const FileName: string);
  begin
    if (FileName = '') then
      exit;

    // ... launch the file's default application to open it.
    Screen.Cursor := crAppStart;
    try
      Application.ProcessMessages; {otherwise cursor change will be missed}
      ShellExecute(0, nil, PChar(FileName), nil, nil, SW_NORMAL);
    finally
      Screen.Cursor := crDefault;
    end;

    // Add temp file to list of files to be deleted before we exit
    if (FCleanUpList.IndexOf(FileName) = -1) then
      FCleanUpList.Add(FileName);
  end;

var
  Attachment: IAttach;
  ChildForm: TFormOutlookTarget;

  Method: integer;
  Prop: PSPropValue;

  FileName: AnsiString;
  SourceStream, DestStream: IStream;
  Dummy: int64;

  Msg: IMessage;
begin
  if (ListViewAttachments.Selected <> nil) and (ListViewAttachments.Selected.Data <> nil) then
  begin
    Attachment := IAttach(ListViewAttachments.Selected.Data);

    if (not Succeeded(HrGetOneProp(Attachment, PR_ATTACH_METHOD, Prop))) then
      exit;
    try
      Method := Prop.Value.l;
    finally
      MAPIFreeBuffer(Prop);
    end;

    case Method of
      ATTACH_BY_VALUE:
        // Attachment is a file stored in an IStream object
        begin
          OleCheck(Attachment.OpenProperty(PR_ATTACH_DATA_BIN, IStream, STGM_READ, 0, IUnknown(SourceStream)));

          FileName := AnsiString(ExtractFilePath(Application.ExeName) +
            ListViewAttachments.Selected.SubItems[ColFile]);

          // Extract the attachment to an external file and...
          SourceStream.Seek(0, STREAM_SEEK_SET, Dummy);

          // Note:
          // You can either use OpenStreamOnFile to create an IStream interface
          // to a newly created file, or you can use TFileStream/TStreamAdapter.
          // Either way should work.
          OleCheck(OpenStreamOnFile(PAllocateBuffer(@MAPIAllocateBuffer), PFreeBuffer(@MAPIFreeBuffer),
            // Filename must be AnsiChar even when using Unicode.
            STGM_CREATE or STGM_READWRITE, PAnsiChar(FileName), nil, DestStream));
          // Another way to do it:
          // DestStream := TFixedStreamAdapter.Create(TFileStream.Create(FileName, fmCreate), soOwned);

          SourceStream.CopyTo(DestStream, -1, Dummy, Dummy);
          DestStream := nil;

          Execute(String(FileName));
        end;

      ATTACH_BY_REFERENCE,
      ATTACH_BY_REF_RESOLVE,
      ATTACH_BY_REF_ONLY:
        // Attachment is a link to a file
        begin
          // Get attachment path
          if (not Succeeded(HrGetOneProp(Attachment, PR_ATTACH_PATHNAME_A, Prop))) then
            exit;
          try
            FileName := Prop.Value.lpszA;
          finally
            MAPIFreeBuffer(Prop);
          end;

          Execute(String(FileName));
        end;

      ATTACH_EMBEDDED_MSG:
        // Attachment is a message stored in an IMessage object
        begin
          // Get size of message
          if (Succeeded(Attachment.OpenProperty(PR_ATTACH_DATA_OBJ, IMessage, 0, 0, IUnknown(Msg)))) then
          begin
            ChildForm := TFormOutlookTarget.Create(Self);
            ChildForm.ParentForm := Self;
            ChildForm.OwnedMessage := TMessage.Create(Msg, nil);
            ChildForm.ViewMessage(ChildForm.OwnedMessage);
            ChildForm.Show;
            Msg := nil;
          end;
        end;

      // Attachment is a OLE object stored in a IStream or IStorage object
      ATTACH_OLE:
        // Note: The actual handling of the OLE object is beyond the scope of
        // this demo. You'll have to figure it out for yourself.
        exit;
    else
      // Unsupported attachment
      exit;
    end;
  end;
end;

procedure TFormOutlookTarget.ActionAttachmentOpenUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (ListViewAttachments.Selected <> nil) and
    (ListViewAttachments.Selected.Data <> nil);
end;

procedure TFormOutlookTarget.ActionAttachmentViewDetailsExecute(
  Sender: TObject);
begin
  FormatAttachmentList;
  ListViewAttachments.ViewStyle := vsReport;
end;

procedure TFormOutlookTarget.ActionAttachmentViewLargeIconsExecute(
  Sender: TObject);
begin
  FormatAttachmentList;
  ListViewAttachments.ViewStyle := vsIcon;
end;

procedure TFormOutlookTarget.ActionAttachmentViewListExecute(Sender: TObject);
begin
  FormatAttachmentList;
  ListViewAttachments.ViewStyle := vsList;
end;

procedure TFormOutlookTarget.ActionAttachmentViewSmallIconsExecute(
  Sender: TObject);
begin
  FormatAttachmentList;
  ListViewAttachments.ViewStyle := vsSmallIcon;
end;

procedure TFormOutlookTarget.ActionMessageClearExecute(Sender: TObject);
begin
  Reset;
end;

procedure TFormOutlookTarget.ActionMessageSaveExecute(Sender: TObject);
var
  Dest: TFileStream;
  Prop: PSPropValue;
  Filename: AnsiString;
  p: PAnsiChar;
begin
  // Use message subject as default file name
  if (Succeeded(HrGetOneProp(FCurrentMessage.Msg, PR_SUBJECT_A, Prop))) then
  begin
    Filename := Prop.Value.lpszA;

    p := PAnsiChar(Filename);
    while (p^ <> #0) do
    begin
      if (p^ in ['*', '?', ':', '/', '\']) then
        p^:= ' ';
      inc(p);
    end;

    SaveDialog1.Filename := String(Filename);
  end else
    SaveDialog1.Filename := 'Message';

  if (SaveDialog1.Execute) then
  begin
    Dest := TFileStream.Create(SaveDialog1.FileName, fmCreate);
    try
      FCurrentMessage.SaveToStream(Dest);
    finally
      Dest.Free;
    end;
  end;
end;

procedure TFormOutlookTarget.ActionMessageSaveUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (FCurrentMessage <> nil);
end;

procedure TFormOutlookTarget.ActionPasteExecute(Sender: TObject);
begin
  DropEmptyTarget1.PasteFromClipboard;
end;

procedure TFormOutlookTarget.ActionPasteUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := DropEmptyTarget1.CanPasteFromClipboard;
end;

procedure TFormOutlookTarget.AddAttachment(const Attachment: IAttach; Number: integer);
var
  Method: integer;
  Item: TListItem;
  Prop: PSPropValue;
  s: string;
  Size: integer;
  Stream: IStream;
  Pos: Largeint;
  Msg: IMessage;
  SHFileInfo: TSHFileInfo;
begin
  Item := ListViewAttachments.Items.Add;
  Item.Caption := ''; // Formatted name
  Item.SubItems.Add(''); // File type
  Item.SubItems.Add(''); // Formatted size
  Item.SubItems.Add(''); // Display name
  Item.SubItems.Add(''); // File name
  Item.ImageIndex := 0;

  // Try to get size of attachment (incl. properties and other overhead).
  // Note: This seems to be the correct way of getting the attachment size, but
  // I have never seen the call succeed.
  if (Succeeded(HrGetOneProp(Attachment, PR_ATTACH_SIZE, Prop))) then
    try
      Size := Prop.Value.l;
    finally
      MAPIFreeBuffer(Prop);
    end
  else
    Size := 0;

  if (Succeeded(HrGetOneProp(Attachment, PR_ATTACH_METHOD, Prop))) then
    try
      Method := Prop.Value.l;
    finally
      MAPIFreeBuffer(Prop);
    end
  else
    Method := -1;

  case Method of
    ATTACH_BY_VALUE:
      // Attachment is a file stored in an IStream object
      begin
        // Get size of attachment stream
        if (Size = 0) and
          (Succeeded(Attachment.OpenProperty(PR_ATTACH_DATA_BIN, IStream, STGM_READ, 0, IUnknown(Stream)))) then
        begin
          Stream.Seek(0, STREAM_SEEK_END, Pos);
          Size := Pos;
        end;

        // Get attachment filename
        if (Succeeded(HrGetOneProp(Attachment, PR_ATTACH_FILENAME, Prop))) then
          try
            if (Prop.ulPropTag and PT_UNICODE = PT_UNICODE) then
              { TODO : TSPropValue.Value.lpszW is declared wrong }
              s := String(PWideChar(Prop.Value.lpszW))
            else
              s := String(Prop.Value.lpszA);
          finally
            MAPIFreeBuffer(Prop);
          end
        else
          s := '';

        if (s = '') then
          s := format('Attachment %d', [Number]);

        Item.SubItems[ColDisplay] := s;
      end;

    ATTACH_BY_REFERENCE,
    ATTACH_BY_REF_RESOLVE,
    ATTACH_BY_REF_ONLY:
      // Attachment is a link to a file
      begin
        // Get attachment path
        if (Succeeded(HrGetOneProp(Attachment, PR_ATTACH_PATHNAME, Prop))) then
          try
            if (Prop.ulPropTag and PT_UNICODE = PT_UNICODE) then
              { TODO : TSPropValue.Value.lpszW is declared wrong }
              s := String(PWideChar(Prop.Value.lpszW))
            else
              s := String(Prop.Value.lpszA);
          finally
            MAPIFreeBuffer(Prop);
          end
        else
          s := '';

        if (s = '') then
          s := format('File reference %d', [Number]);
        Item.SubItems[ColDisplay] := s;
      end;

    ATTACH_EMBEDDED_MSG:
      // Attachment is a message stored in an IMessage object
      begin
        // Get size of message
        if (Succeeded(Attachment.OpenProperty(PR_ATTACH_DATA_OBJ, IMessage, 0, 0, IUnknown(Msg)))) then
        begin
          if (Size = 0) then
          begin
            if (Succeeded(HrGetOneProp(Msg, PR_MESSAGE_SIZE, Prop))) then
              try
                Size := Prop.Value.l;
              finally
                MAPIFreeBuffer(Prop);
              end
            else
              Size := 0;
          end;
        end else
          // See comment in TMessage.GetAttachments for a possible reason why might fail
          Msg := nil;


        if (Msg <> nil) then
          s := GetSubject(Msg)
        else
          s := 'Failed to open attachment';
        if (s = '') then
          s := format('Embedded message %d', [Number]);
        Item.SubItems[ColDisplay] := s;
        Item.SubItems[ColFile] := s+'.msg';
      end;

    // Attachment is a OLE object stored in a IStream or IStorage object
    ATTACH_OLE:
      begin
        Item.SubItems[ColDisplay] := 'OLE object';
      end;
  else
    // Unsupported attachment
    Item.SubItems[ColDisplay] := 'Unknown attachment';
  end;

  if (Item.SubItems[ColFile] = '') then
    Item.SubItems[ColFile] := Item.SubItems[ColDisplay];

  // Get the icon associated with the attachment file type
  // Beware: SHGetFileInfo can change the "current directory"
  if (Succeeded(SHGetFileInfo(PChar(Item.SubItems[ColFile]), 0, SHFileInfo, sizeOf(SHFileInfo),
      SHGFI_USEFILEATTRIBUTES or SHGFI_SYSICONINDEX or SHGFI_LARGEICON or SHGFI_TYPENAME))) then
  begin
    Item.ImageIndex := SHFileInfo.iIcon;
    Item.SubItems[ColType] := SHFileInfo.szTypeName;
  end;

  // Format the attachment size
  if (Size > 1024*1014) then
    Item.SubItems[ColSize] := IntToStr(Size div (1024*1024))+'Mb'
  else
    if (Size > 1024) then
      Item.SubItems[ColSize] := IntToStr(Size div 1024)+'Kb'
    else
      if (Size > 0) then
        Item.SubItems[ColSize] := IntToStr(Size)+' bytes'
      else
        Item.SubItems[ColSize] := '';

  // Save interface for later.
  // According to MSDN an Attachment can be opened only once (IMessage.OpenAttach).
  Item.Data := pointer(Attachment);

  // Now that the interface is stored and managed in list items's data property,
  // we avoid decrementing the reference count here.
//  pointer(Attachment) := nil;
end;

procedure TFormOutlookTarget.ListViewAttachmentsDeletion(Sender: TObject;
  Item: TListItem);
begin
  // Zap reference to IAttachment object
//  if (Item.Data <> nil) then
//    IUnknown(Item.Data)._Release;
end;

function MAPIAllocateBuffer(cbSize: ULONG; var lppBuffer: pointer): SCODE; stdcall; external 'mapi32.dll';
function MAPIFreeBuffer(lpBuffer: pointer): ULONG; stdcall; external 'mapi32.dll';

procedure TFormOutlookTarget.ListViewAttachmentsDblClick(Sender: TObject);
begin
  ActionAttachmentOpen.Execute;
end;

procedure TFormOutlookTarget.FormatAttachmentList;
var
  i: integer;
begin
  for i := 0 to ListViewAttachments.Items.Count-1 do
    if (not MenuAttachmentViewDetails.Checked) then
    begin
      if (ListViewAttachments.Items[i].SubItems[ColSize] <> '') then
        // Display attachment as "filename (size)"
        ListViewAttachments.Items[i].Caption :=
          format('%s (%s)', [ListViewAttachments.Items[i].SubItems[ColDisplay],
            ListViewAttachments.Items[i].SubItems[ColSize]])
      else
        // Display attachment as "filename"
        ListViewAttachments.Items[i].Caption :=
          ListViewAttachments.Items[i].SubItems[ColDisplay];
    end else
      ListViewAttachments.Items[i].Caption := ListViewAttachments.Items[i].SubItems[ColDisplay];
end;

procedure TFormOutlookTarget.ListViewAttachmentsResize(Sender: TObject);
begin
  // Work around list view resize/repaint bug.
  ListViewAttachments.Repaint;
end;

end.
