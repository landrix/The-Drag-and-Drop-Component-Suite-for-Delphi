unit Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, DragDrop, DropTarget, DragDropText;

type
  TFormMain = class(TForm)
    DropTextTarget1: TDropTextTarget;
    DataFormatAdapterFile: TDataFormatAdapter;
    DataFormatAdapterURL: TDataFormatAdapter;
    Panel1: TPanel;
    Label1: TLabel;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    MemoText: TMemo;
    MemoFile: TMemo;
    MemoURL: TMemo;
    Panel2: TPanel;
    procedure DropTextTarget1Drop(Sender: TObject; ShiftState: TShiftState;
      Point: TPoint; var Effect: Integer);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.DFM}

uses
  // Note: In order to get the File and URL data format support linked into the
  // application, we have to include the appropiate units in the uses clause.
  // If you forget to do this, you will get a run time error.
  // The DragDropFile unit contains the TFileDataFormat class and the
  // DragDropInternet unit contains the TURLDataFormat class.
  DragDropFormats,
  DragDropInternet,
  DragDropFile;

procedure TFormMain.DropTextTarget1Drop(Sender: TObject;
  ShiftState: TShiftState; Point: TPoint; var Effect: Integer);
begin
  MemoText.Lines.Text := DropTextTarget1.Text;
  // Check if we have a data format and if so...
  if (DataFormatAdapterFile.DataFormat <> nil) then
    // ...Extract the dropped data from it.
    MemoFile.Lines.Assign((DataFormatAdapterFile.DataFormat as TFileDataFormat).Files);

  if (DataFormatAdapterURL.DataFormat <> nil) then
    MemoURL.Lines.Text := (DataFormatAdapterURL.DataFormat as TURLDataFormat).URL;
end;

end.
