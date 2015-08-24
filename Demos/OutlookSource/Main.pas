unit Main;

interface

uses
  ActiveX,
  DragDropFile,
  DragDropInternet,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DragDrop, DropSource, ExtCtrls, StdCtrls, DropTarget;

type
  TFormOutlookSource = class(TForm)
    DropEmptySource1: TDropEmptySource;
    DropFileTarget1: TDropFileTarget;
    Memo1: TMemo;
    PanelReady: TPanel;
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure DropFileTarget1Drop(Sender: TObject; ShiftState: TShiftState;
      APoint: TPoint; var Effect: Integer);
  private
    FOutlookDataFormat: TOutlookDataFormat;
  public
  end;

var
  FormOutlookSource: TFormOutlookSource;

implementation

{$R *.dfm}

uses
  ComObj;

procedure TFormOutlookSource.DropFileTarget1Drop(Sender: TObject;
  ShiftState: TShiftState; APoint: TPoint; var Effect: Integer);
var
  Msg: IStorage;
  i: integer;
  Filename: WideString;
begin
  FOutlookDataFormat.Clear;

  for i := 0 to DropFileTarget1.Files.Count-1 do
  begin
    Filename := DropFileTarget1.Files[i];

    OleCheck(StgOpenStorage(PWideChar(Filename), nil, STGM_DIRECT OR STGM_READ OR STGM_SHARE_EXCLUSIVE, nil, 0, Msg));
    try
      FOutlookDataFormat.Storages.AddNamed(Msg, Filename);
    finally
      Msg := nil;
    end;
  end;

  PanelReady.Visible := (FOutlookDataFormat.Storages.Count > 0);
end;

procedure TFormOutlookSource.FormCreate(Sender: TObject);
begin
  FOutlookDataFormat := TOutlookDataFormat.Create(DropEmptySource1);
end;

procedure TFormOutlookSource.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (FOutlookDataFormat.Storages.Count = 0) then
    exit;

  if DragDetectPlus(Sender as TWinControl) then
  begin
    DropFileTarget1.Enabled := False;
    try
      DropEmptySource1.Execute;
    finally
      DropFileTarget1.Enabled := True;
    end;
  end;
end;

end.
