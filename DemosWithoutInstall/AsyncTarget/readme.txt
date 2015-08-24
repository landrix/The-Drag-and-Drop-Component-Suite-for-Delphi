AsyncTransferTarget

This application demonstrates how to receive dropped data asynchronously via a
stream.

The application uses a TDropEmptyTarget component and extends it with a
TDataFormatAdapter component.

While the drag/drop operation takes place in the main thread as always, the
actual data transfer is performed in a separate thread. The advantage of using a
thread is that the source and target applications aren't blocked while the
target application processes the drop.
Note that this approach is normally only used when transferring large amounts of
data (e.g. file contents) or when the drop target is very slow.

Asynchronous drop targets requires the cooperation of the drop source.
The drop target and drop source communicates the progress of the asynchronous
transfer through a special interface; IAsyncOperation. The IAsyncOperation
interface requires version 5.00 of shell32.dll and are only supported on
Windows 2000, Windows ME and later.
If the drop source doesn't support the IAsyncOperation interface or if it hasn't
enabled asynchronous data transfer, the data transfer will progress
synchronously as normal.

