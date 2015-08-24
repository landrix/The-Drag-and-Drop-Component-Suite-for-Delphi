unit DropURL;

interface

{$include DragDrop.inc}

uses
  ImgList,
  DragDrop,
  DropSource,
  DropTarget,
  DragDropGraphics,
  DragDropInternet,
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls, ActiveX, CommCtrl, Menus;

type
  TFormURL = class(TForm)
    Panel1: TPanel;
    ButtonClose: TButton;
    StatusBar1: TStatusBar;
    Memo2: TMemo;
    DropURLTarget1: TDropURLTarget;
    DropURLSource1: TDropURLSource;
    DropBMPSource1: TDropBMPSource;
    DropBMPTarget1: TDropBMPTarget;
    PanelImageTarget: TPanel;
    ImageTarget: TImage;
    Panel3: TPanel;
    Memo1: TMemo;
    PanelImageSource2: TPanel;
    ImageSource2: TImage;
    LabelURL: TLabel;
    ImageList1: TImageList;
    PanelImageSource1: TPanel;
    ImageSource1: TImage;
    DropDummy1: TDropDummy;
    PopupMenu1: TPopupMenu;
    MenuCopy: TMenuItem;
    MenuCut: TMenuItem;
    N1: TMenuItem;
    MenuPaste: TMenuItem;
    PanelURL: TPanel;
    procedure ButtonCloseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure URLMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ImageMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DropURLTarget1Drop(Sender: TObject; ShiftState: TShiftState;
      Point: TPoint; var Effect: Integer);
    procedure DropBMPTarget1Drop(Sender: TObject; ShiftState: TShiftState;
      Point: TPoint; var Effect: Integer);
    procedure MenuCutOrCopyClick(Sender: TObject);
    procedure MenuPasteClick(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure DropBMPSource1Paste(Sender: TObject; Action: TDragResult;
      DeleteOnPaste: Boolean);
  private
    PasteImage: TImage; // Remembers which TImage is the source of a copy/paste
  public
  end;

var
  FormURL: TFormURL;

implementation

{$R *.DFM}

uses
  ComObj;

function GetTransparentColor(const bmp: TBitmap): TColor;
begin
  if (bmp = nil) or (bmp.empty) then
    Result := clWhite
  else
    Result := bmp.TransparentColor;
end;

procedure TFormURL.FormCreate(Sender: TObject);
begin
  // Note: This is an example of "manual" target registration. We could just
  // as well have assigned the TDropTarget.Target property at design-time to
  // register the drop targets.

  // Register the URL and BMP DropTarget controls.
  DropURLTarget1.Register(PanelURL);
  DropBMPTarget1.Register(PanelImageTarget);

  // This enables the dragged image to be visible
  // over the whole form, not just the above targets.
  DropDummy1.Register(Self);
end;

procedure TFormURL.FormDestroy(Sender: TObject);
begin
  // Note: This is an example of "manual" target unregistration. However,
  // Since the targets are automatically unregistered when they are destroyed,
  // it is not nescessary to do it manually.

  // UnRegister the DropTarget windows.
  DropURLTarget1.UnRegister;
  DropBMPTarget1.UnRegister;
  DropDummy1.UnRegister;
end;

procedure TFormURL.ButtonCloseClick(Sender: TObject);
begin
  Close;
end;

//------------------------------------------------------------------------------
// URL stuff ...
//------------------------------------------------------------------------------
type
  // TWinControlCracker is used to gain access to the protected memebers of
  // TWinControl.
  TWinControlCracker = class(TWinControl);

procedure TFormURL.URLMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  DragImage: TBitmap;
begin
  // Wait for user to move cursor before we start the drag/drop.
  if (DragDetectPlus(TWincontrol(Sender))) then
  begin
    // This demonstrates how to create a drag image based on the source control.
    // Note: DropURLSource1.Images = ImageList1
    DragImage := TBitmap.Create;
    try
      DragImage.Width := TWinControl(Sender).Width;
      DragImage.Height := TWinControl(Sender).Height;
      TWinControl(Sender).PaintTo(DragImage.Canvas.Handle, 0, 0);
      ImageList1.Width := DragImage.Width;
      ImageList1.Height := DragImage.Height;
      ImageList1.Add(DragImage, nil);
    finally
      DragImage.Free;
    end;
    DropURLSource1.ImageHotSpotX := X;
    DropURLSource1.ImageHotSpotY := Y;

    DropURLSource1.ImageIndex := 0;
    try
      // Copy the data into the drop source.
      DropURLSource1.Title := PanelURL.Caption;
      DropURLSource1.URL := LabelURL.Caption;

      // Temporarily disable Edit1 as a drop target.
      DropURLTarget1.DragTypes := [];
      try
        DropURLSource1.Execute;
      finally
        // Enable Edit1 as a drop target again.
        DropURLTarget1.DragTypes := [dtLink];
      end;

    finally
      // Now that the drag has completed we don't need the image list anymore.
      ImageList1.Clear;
    end;
  end;
end;

procedure TFormURL.DropURLTarget1Drop(Sender: TObject;
  ShiftState: TShiftState; Point: TPoint; var Effect: Integer);
begin
  // An URL has been dropped - Copy the URL and title from the drop target.
  PanelURL.Caption := DropURLTarget1.Title;
  LabelURL.Caption := DropURLTarget1.URL;
end;

//------------------------------------------------------------------------------
// Bitmap stuff ...
//------------------------------------------------------------------------------
procedure TFormURL.ImageMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  p: TPoint;
begin
  if (Button = mbRight) or (TImage(Sender).Picture.Graphic = nil) then
    exit;

  // First convert the mouse coordinates from Image relative coordinates
  // to screen coordinates.
  p := TControl(Sender).ClientToScreen(Point(X,Y));

  // Since the TImage component hasn't got a window handle, we must
  // use the TPanel behind it instead...
  if (DragDetectPlus(TImage(Sender).Parent.Handle, p)) then
  begin
    // Freeze clipboard contents if we have live data on it.
    // This is only nescessary because the TGraphic based data formats (such as
    // TBitmapDataFormat) doesn't support auto flushing.
    if (DropBMPSource1.LiveDataOnClipboard) then
      DropBMPSource1.FlushClipboard;

    // Copy the data into the drop source.
    DropBMPSource1.Bitmap.Assign(TImage(Sender).Picture.Graphic);

    // This just demonstrates dynamically allocating a drag image.
    // Note: DropBMPSource1.Images = ImageList1
    ImageList1.Width := DropBMPSource1.Bitmap.Width;
    ImageList1.Height := DropBMPSource1.Bitmap.Height;
    ImageList1.AddMasked(DropBMPSource1.Bitmap,
      GetTransparentColor(DropBMPSource1.Bitmap));
    DropBMPSource1.ImageIndex := 0;
    try

      // Perform the drag.
      if (DropBMPSource1.Execute = drDropMove) then
        // Clear the source image if image were drag-moved.
        TImage(Sender).Picture.Graphic := nil;

    finally
      // Now that the drag has completed we don't need the image list anymore.
      ImageList1.Clear;
    end;
  end;
end;

procedure TFormURL.PopupMenu1Popup(Sender: TObject);
var
  PopupSource: TComponent;
begin
  PopupSource := TPopupMenu(Sender).PopupComponent;

  // Enable cut and copy for source image unless it is empty.
  if (PopupSource = ImageSource1) or (PopupSource = ImageSource2) then
  begin
    MenuCopy.Enabled := (TImage(PopupSource).Picture.Graphic <> nil);
    MenuCut.Enabled := MenuCopy.Enabled;
    MenuPaste.Enabled := False;
  end else
  // Enable paste for target image if the clipboard contains a bitmap.
  if (PopupSource = ImageTarget) then
  begin
    MenuCopy.Enabled := False;
    MenuCut.Enabled := False;
    // Enable paste menu if the clipboard contains data in any of
    // the supported formats.
    MenuPaste.Enabled := DropBMPTarget1.CanPasteFromClipboard;
  end;
end;

procedure TFormURL.MenuCutOrCopyClick(Sender: TObject);
begin
  // Clear the current content of the clipboard.
  // This isn't strictly nescessary, but can improve performance; If the drop
  // source has live data on the clipboard and the drop source data is modified,
  // the drop source will copy all its current data to the clipboard and then
  // disconnect itself from it. It does this in order to preserve the clipboard
  // data in the state it was in when the data were copied to the clipboard.
  // Since we are about to copy new data to the clipboard, we might as well save
  // the drop source all this unnescessary work.
  DropBMPSource1.EmptyClipboard;

  // Remember which TImage component the data originated from. This is used so
  // we can clear the image if a "delete on paste" is performed.
  PasteImage := TImage(TPopupMenu(TMenuItem(Sender).GetParentMenu).PopupComponent);

  DropBMPSource1.Bitmap.Assign(PasteImage.Picture.Graphic);

  if (Sender = MenuCut) then
    DropBMPSource1.CutToClipboard
  else
    DropBMPSource1.CopyToClipboard;
  StatusBar1.SimpleText := 'Bitmap copied to clipboard';
end;

procedure TFormURL.DropBMPSource1Paste(Sender: TObject;
  Action: TDragResult; DeleteOnPaste: Boolean);
begin
  // When the target signals that it has pasted the image (after a
  // CutToClipboard operation), we can safely delete the source image.
  // This is an example of a "Delete on paste" operation.
  if (DeleteOnPaste) then
    PasteImage.Picture.Assign(nil);
  StatusBar1.SimpleText := 'Bitmap pasted from clipboard';
end;

procedure TFormURL.MenuPasteClick(Sender: TObject);
begin
  // PasteFromClipboard fires an OnDrop event, so we don't need to do
  // anything special here.
  DropBMPTarget1.PasteFromClipboard;
  StatusBar1.SimpleText := 'Bitmap pasted from clipboard';
end;

procedure TFormURL.DropBMPTarget1Drop(Sender: TObject;
  ShiftState: TShiftState; Point: TPoint; var Effect: Integer);
begin
  // An image has just been dropped on the target - Copy it to
  // our TImage component
  ImageTarget.Picture.Assign(DropBMPTarget1.Bitmap);
end;

end.
