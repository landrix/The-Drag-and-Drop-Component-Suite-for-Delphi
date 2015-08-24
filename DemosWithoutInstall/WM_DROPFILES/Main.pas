unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Buttons, ExtCtrls, ComCtrls;

type
  TFormMain = class(TForm)
    Panel1: TPanel;
    SpeedButton1: TSpeedButton;
    ListView1: TListView;
    procedure SpeedButton1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
  protected
    procedure WMDropFiles(var Msg: TWMDropFiles); message WM_DROPFILES;
  public
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

uses
  ShellAPI;

(*
** TDropFiles
**
** Encapsulates the DragQueryFile, DragQueryPoint and DragFinish functions.
*)
type
  TDropFiles = class
  private
    FDropHandle: HDROP;
    function GetCount: Integer;
    function GetDropPoint: TPoint;
    function GetFile(Index: integer): string;
  protected
  public
    constructor Create(ADropHandle: HDROP);
    destructor Destroy; override;
    property Count: Integer read GetCount;
    property Files[Index: integer]: string read GetFile; default;
    property DropPoint: TPoint read GetDropPoint;
    property DropHandle: HDROP read FDropHandle;
  end;

constructor TDropFiles.Create(ADropHandle: HDROP);
begin
  inherited Create;
  FDropHandle := ADropHandle;
end;

destructor TDropFiles.Destroy;
begin
  DragFinish(FDropHandle);
  inherited Destroy;
end;

function TDropFiles.GetCount: Integer;
begin
  Result := DragQueryFile(FDropHandle, $FFFFFFFF, nil, 0);
end;

function TDropFiles.GetDropPoint: TPoint;
begin
  DragQueryPoint(FDropHandle, Result);
end;

function TDropFiles.GetFile(Index: integer): string;
var
  Size: integer;
begin
  Size := DragQueryFile(FDropHandle, Index, nil, 0);
  SetLength(Result, Size);
  DragQueryFile(FDropHandle, Index, PChar(Result), Size+1);
end;


resourcestring
  sAbout = 'This application demonstrates the old method of handling files dragged from Windows Explorer.'+#13+#13+
    'The application does not use the Drag and Drop Component Suite.';

procedure TFormMain.FormCreate(Sender: TObject);
begin
  // Register the form as a file drop handler
  DragAcceptFiles(Self.Handle, True);
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  // Unregister the form as a file drop handler
  DragAcceptFiles(Self.Handle, False);
end;

procedure TFormMain.SpeedButton1Click(Sender: TObject);
begin
  ShowMessage(sAbout);
end;

procedure TFormMain.WMDropFiles(var Msg: TWMDropFiles);
var
  DropFiles: TDropFiles;
  i: integer;
  ListItem: TListItem;
begin
  // Drop handler
  DropFiles := TDropFiles.Create(Msg.Drop);
  try
    ListView1.Items.BeginUpdate;
    try
      ListView1.Items.Clear;
      for i := 0 to DropFiles.Count-1 do
      begin
        ListItem := ListView1.Items.Add;
        ListItem.Caption := DropFiles[i];
      end;
    finally
      ListView1.Items.EndUpdate;
    end;
  finally
    DropFiles.Free;
  end;
end;

end.
