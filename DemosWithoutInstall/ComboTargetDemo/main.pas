unit main;

interface

{$include dragdrop.inc} // Disables .NET warnings

uses
  DragDrop,
  DropTarget,
  DragDropGraphics,
  DropComboTarget,
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, StdCtrls, ExtCtrls;

type
  TFormMain = class(TForm)
    DropComboTarget1: TDropComboTarget;
    PageControl1: TPageControl;
    TabSheetText: TTabSheet;
    TabSheetFiles: TTabSheet;
    TabSheetBitmap: TTabSheet;
    TabSheetURL: TTabSheet;
    ListBoxFiles: TListBox;
    ListBoxMaps: TListBox;
    Splitter1: TSplitter;
    Label1: TLabel;
    Label2: TLabel;
    EditURLURL: TEdit;
    EditURLTitle: TEdit;
    MemoText: TMemo;
    TabSheetData: TTabSheet;
    TabSheetMetaFile: TTabSheet;
    ScrollBox1: TScrollBox;
    ImageMetaFile: TImage;
    ScrollBox2: TScrollBox;
    ImageBitmap: TImage;
    Panel2: TPanel;
    PanelDropZone: TPanel;
    Panel1: TPanel;
    GroupBox1: TGroupBox;
    CheckBoxText: TCheckBox;
    CheckBoxFiles: TCheckBox;
    CheckBoxURLs: TCheckBox;
    CheckBoxBitmaps: TCheckBox;
    CheckBoxMetaFiles: TCheckBox;
    CheckBoxData: TCheckBox;
    ListViewData: TListView;
    Label3: TLabel;
    procedure DropComboTarget1Drop(Sender: TObject;
      ShiftState: TShiftState; Point: TPoint; var Effect: Integer);
    procedure CheckBoxTextClick(Sender: TObject);
    procedure CheckBoxFilesClick(Sender: TObject);
    procedure CheckBoxURLsClick(Sender: TObject);
    procedure CheckBoxBitmapsClick(Sender: TObject);
    procedure CheckBoxMetaFilesClick(Sender: TObject);
    procedure CheckBoxDataClick(Sender: TObject);
    procedure ListViewDataDblClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

uses
  ShellApi;

{$R *.DFM}

procedure TFormMain.DropComboTarget1Drop(Sender: TObject;
  ShiftState: TShiftState; Point: TPoint; var Effect: Integer);
var
  Stream: TStream;
  i: integer;
  Name: string;
begin
  // Clear all formats.
  EditURLURL.Text := '';
  EditURLTitle.Text := '';
  MemoText.Lines.Clear;
  ImageBitmap.Picture.Assign(nil);
  ImageMetaFile.Picture.Assign(nil);
  ListBoxFiles.Items.Clear;
  ListBoxMaps.Items.Clear;
  ListViewData.Items.Clear;

  // Extract and display dropped data.
  for i := 0 to DropComboTarget1.Data.Count-1 do
  begin
    Name := DropComboTarget1.Data.Names[i];
    if (Name = '') then
      Name := intToStr(i)+'.dat';
    Stream := TFileStream.Create(ExtractFilePath(Application.ExeName)+Name, fmCreate);
    try
      with ListViewData.Items.Add do
      begin
        Caption := Name;
        SubItems.Add(IntToStr(DropComboTarget1.Data[i].Size));
      end;
      // Copy dropped data to stream (in this case a file stream).
      Stream.CopyFrom(DropComboTarget1.Data[i], DropComboTarget1.Data[i].Size);
    finally
      Stream.Free;
    end;
  end;

  // Copy the rest of the dropped formats.
  ListBoxFiles.Items.Assign(DropComboTarget1.Files);
  ListBoxMaps.Items.Assign(DropComboTarget1.FileMaps);
  EditURLURL.Text := DropComboTarget1.URL;
  EditURLTitle.Text := DropComboTarget1.Title;
  ImageBitmap.Picture.Assign(DropComboTarget1.Bitmap);
  ImageMetaFile.Picture.Assign(DropComboTarget1.MetaFile);
  MemoText.Lines.Text := DropComboTarget1.Text;

  // Determine which formats were dropped.
  TabSheetFiles.TabVisible := (ListBoxFiles.Items.Count > 0);
  TabSheetURL.TabVisible := (EditURLURL.Text <> '') or (EditURLTitle.Text <> '');
  TabSheetBitmap.TabVisible := (ImageBitmap.Picture.Graphic <> nil) and
    (not ImageBitmap.Picture.Graphic.Empty);
  TabSheetMetaFile.TabVisible := (ImageMetaFile.Picture.Graphic <> nil) and
    (TMetaFile(ImageMetaFile.Picture.Graphic).Handle <> 0);
  TabSheetText.TabVisible := (MemoText.Lines.Count > 0);
  TabSheetData.TabVisible := (ListViewData.Items.Count > 0);
end;

procedure TFormMain.CheckBoxTextClick(Sender: TObject);
begin
  // Enable or disable format according to users selection.
  if (TCheckBox(Sender).Checked) then
    DropComboTarget1.Formats := DropComboTarget1.Formats + [mfText]
  else
    DropComboTarget1.Formats := DropComboTarget1.Formats - [mfText];
end;

procedure TFormMain.CheckBoxFilesClick(Sender: TObject);
begin
  // Enable or disable format according to users selection.
  if (TCheckBox(Sender).Checked) then
    DropComboTarget1.Formats := DropComboTarget1.Formats + [mfFile]
  else
    DropComboTarget1.Formats := DropComboTarget1.Formats - [mfFile];
end;

procedure TFormMain.CheckBoxURLsClick(Sender: TObject);
begin
  // Enable or disable format according to users selection.
  if (TCheckBox(Sender).Checked) then
    DropComboTarget1.Formats := DropComboTarget1.Formats + [mfURL]
  else
    DropComboTarget1.Formats := DropComboTarget1.Formats - [mfURL];
end;

procedure TFormMain.CheckBoxBitmapsClick(Sender: TObject);
begin
  // Enable or disable format according to users selection.
  if (TCheckBox(Sender).Checked) then
    DropComboTarget1.Formats := DropComboTarget1.Formats + [mfBitmap]
  else
    DropComboTarget1.Formats := DropComboTarget1.Formats - [mfBitmap];
end;

procedure TFormMain.CheckBoxMetaFilesClick(Sender: TObject);
begin
  // Enable or disable format according to users selection.
  if (TCheckBox(Sender).Checked) then
    DropComboTarget1.Formats := DropComboTarget1.Formats + [mfMetaFile]
  else
    DropComboTarget1.Formats := DropComboTarget1.Formats - [mfMetaFile];
end;

procedure TFormMain.CheckBoxDataClick(Sender: TObject);
begin
  // Enable or disable format according to users selection.
  if (TCheckBox(Sender).Checked) then
    DropComboTarget1.Formats := DropComboTarget1.Formats + [mfData]
  else
    DropComboTarget1.Formats := DropComboTarget1.Formats - [mfData];
end;

procedure TFormMain.ListViewDataDblClick(Sender: TObject);
begin
  // Launch an extracted data file if user double clicks on it.
  Screen.Cursor := crAppStart;
  try
    Application.ProcessMessages; {otherwise cursor change will be missed}
    ShellExecute(0, nil,
      PChar(ExtractFilePath(Application.ExeName)+TListView(Sender).Selected.Caption),
      nil, nil, SW_NORMAL);
  finally
    Screen.Cursor := crDefault;
  end;
end;

end.


