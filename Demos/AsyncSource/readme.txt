AsyncTransferSource

This application demonstrates how to transfer data asynchronously via a stream.

The application uses a TDropEmptySource component and extends it with a
TDataFormatAdapter component. The data formats being transferred is the
FileContent and FileGroupDescriptor formats. The Explorer and Outlook are
examples of applications that can accept data in this format.

While the drag/drop operation takes place in the main thread as always, the
actual data transfer is performed in a separate thread. The advantage of using a
thread is that the source application isn't blocked while the target application
processes the drop.
This approach is most appropriate when transferring large amounts of data (e.g.
file contents) or when the drop source or target is very slow.

While asynchronous drop targets (see the AsyncTransferTarget application)
requires the cooperation of the drop source, an asynchronous drop source can
perform its magic independently of the drop target.

Note that for the purpose of demonstrating asynchronous drop sources, support
for asynchronous drop targets has been disabled in this demo. Otherwise you
wouldn't notice any difference between the two modes of operation demonstrated
by this application, if you dropped onto an application that supported
asynchronous transfer (e.g. the Windows Explorer or Desktop).

