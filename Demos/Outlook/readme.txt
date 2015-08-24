OutlookDemo

This application demonstrates how to receive items (e.g. mail messages) dropped
from Microsoft Outlook.

A TDropEmptyTarget component is extended with the TOutlookDataFormat using a
TDataFormatAdapter component. Extended MAPI is then used to extract the message
from Outlook and finally the message is parsed and displayed.

Note: This demo only works with Outlook, not Outlook Express.
