ComboTargetDemo

This application demonstrates how to use the TDropComboTarget component to
receive multiple data formats with a single target component.

Note that only one target component can be registered against any given control.
I.e. you can't register a control with both a TDropFileTarget and a
TDropURLtarget component at the same time. This is a Windows limitation.

If you need a control to receive drops in more formats than any single target
component supports, you will have to either use a TDataFormatAdapter component
(see the Adapter demo), extend one of the existing components or write a new
component.

