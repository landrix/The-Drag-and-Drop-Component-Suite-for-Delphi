DragDropDemo

This application demonstrates how to drag and drop text, files, URLs and
bitmaps. Both drop target and drop source operations are demonstrated.

In addition to basic drag/drop operations, advanced features such as drag
images, custom cursors, clipboard operations and optimized move are
demonstrated.

In addition to the main unit, the demo consists of the following units:

1) DropFile unit.
This unit demonstrates how to drag and drop files using the following
components:
- TDropFileSource
- TDropFileTarget
- TDropDummy

In addition to basic drag/drop operations, the unit demonstrates:
- Custom drag cursors.
- Drag images.
- Clipboard copy/paste.
- "Delete on paste".
- "Optimized move".
- Providing feedback over controls which aren't drop targets.

2) DropText unit.
This unit demonstrates how to drag and drop text using the following components:
- TDropTextSource
- TDropTextTarget
- TDropDummy

In addition to basic drag/drop operations, the unit demonstrates:
- Custom drag cursors.
- Copying data onto the clipboard.
- Providing feedback over controls which aren't drop targets.

3) DropURL unit.
This unit demonstrates how to drag and drop URLs and bitmaps using the following
components:
- TDropURLSource
- TDropURLTarget
- TDropBMPSource
- TDropBMPTarget
- TDropDummy

In addition to basic drag/drop operations, the unit demonstrates:
- Custom drag cursors.
- Dynamic drag images.
- Clipboard copy/paste.
- "Delete on paste".
- Providing feedback over controls which aren't drop targets.

Note: This demo uses the ShellCtrls unit which doesn't support Delphi 5.
