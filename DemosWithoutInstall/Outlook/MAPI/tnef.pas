unit tnef;

interface

uses Windows, ActiveX, MAPIX, MAPIGUID, MAPIDefs;

const

  //*  OpenTNEFStream */

     TNEF_DECODE                 = $0;
     TNEF_ENCODE                 = $2;

     TNEF_PURE                   = $00010000;
     TNEF_COMPATIBILITY          = $00020000;
     TNEF_BEST_DATA              = $00040000;
     TNEF_COMPONENT_ENCODING     = $80000000;

//*  AddProps, ExtractProps */

     TNEF_PROP_INCLUDE           = $00000001;
     TNEF_PROP_EXCLUDE           = $00000002;
     TNEF_PROP_CONTAINED         = $00000004;
     TNEF_PROP_MESSAGE_ONLY      = $00000008;
     TNEF_PROP_ATTACHMENTS_ONLY  = $00000010;
     TNEF_PROP_CONTAINED_TNEF    = $00000040;

//*  FinishComponent */

     TNEF_COMPONENT_MESSAGE      = $00001000;
     TNEF_COMPONENT_ATTACHMENT   = $00002000;


type

  PSTnefProblem = ^TSTnefProblem;
    TSTnefProblem = record
      ulComponent : ULONG;
      ulAttribute : ULONG;
      ulPropTag   : ULONG;
      dwScode     : SCODE;
    end;

    PSTnefProblemArray = ^TSTnefProblemArray;
    TSTnefProblemArray = record
      cProblem : ULONG;
      aProblem : array[0..MAPI_DIM] of TSTnefProblem;
    end;

    ITNEF = interface(IUnknown)
      [ strIID_ITNEF ]
      function AddProps(ulFlags : ULONG; ulElemID : ULONG; lpvData : pointer; lpPropList : PSPropTagArray):HResult;stdcall;
      function ExtractProps(ulFlags : ULONG; lpPropList:PSPropTagArray; var lpProblems: PSTnefProblemArray):HResult;stdcall;
      function Finish(ulFlags : ULONG; var lpKey : ULONG; var lpProblems: PSTnefProblemArray):HResult;stdcall;
      function OpenTaggedBody(lpMessage : IMessage; ulFlags : ULONG; var lppStream : IStream):HResult;stdcall;
      function SetProps(ulFlags : ULONG; ulElemID : ULONG; cValues : ULONG; lpProps : PSPropValue):HResult;stdcall;
      function EncodeRecips(ulFlags : ULONG; lpRecipientTable : IMAPITABLE):HResult;stdcall;
      function FinishComponent(ulFlags : ULONG; ulComponentID :ULONG; lpCustomPropList: PSPropTagArray;
                               lpCustomProps : PSPropValue; lpPropList : PSPropTagArray; var lppProblems: PSTnefProblemArray):HResult;stdcall;
    end;

const TNEF_SIGNATURE : ULONG = $223E9F78;

       LVL_MESSAGE     = 1;
       LVL_ATTACHMENT  = 2;

       atpTriples      =  $0000;
       atpString       =  $0001;
       atpText         =  $0002;
       atpDate         =  $0003;
       atpShort        =  $0004;
       atpLong         =  $0005;
       atpByte         =  $0006;
       atpWord         =  $0007;
       atpDword        =  $0008;
       atpMax          =  $0009;


 attNull                     = ((0              shl 16) or 0);
 attFrom                     = ((atpTriples     shl 16) or $8000); //* PR_ORIGINATOR_RETURN_ADDRESS */
 attSubject                  = ((atpString      shl 16) or $8004); //* PR_SUBJECT */
 attDateSent                 = ((atpDate        shl 16) or $8005); //* PR_CLIENT_SUBMIT_TIME */
 attDateRecd                 = ((atpDate        shl 16) or $8006); // PR_MESSAGE_DELIVERY_TIME */
 attMessageStatus            = ((atpByte        shl 16) or $8007); // PR_MESSAGE_FLAGS */
 attMessageClass             = ((atpWord        shl 16) or $8008); // PR_MESSAGE_CLASS */
 attMessageID                = ((atpString      shl 16) or $8009); // PR_MESSAGE_ID */
 attParentID                 = ((atpString      shl 16) or $800A); // PR_PARENT_ID */
 attConversationID           = ((atpString      shl 16) or $800B); // PR_CONVERSATION_ID */
 attBody                     = ((atpText        shl 16) or $800C); // PR_BODY */
 attPriority                 = ((atpShort       shl 16) or $800D); // PR_IMPORTANCE */
 attAttachData               = ((atpByte        shl 16) or $800F); // PR_ATTACH_DATA_xxx */
 attAttachTitle              = ((atpString      shl 16) or $8010); // PR_ATTACH_FILENAME */
 attAttachMetaFile           = ((atpByte        shl 16) or $8011); // PR_ATTACH_RENDERING */
 attAttachCreateDate         = ((atpDate        shl 16) or $8012); // PR_CREATION_TIME */
 attAttachModifyDate         = ((atpDate        shl 16) or $8013); // PR_LAST_MODIFICATION_TIME */
 attDateModified             = ((atpDate        shl 16) or $8020); // PR_LAST_MODIFICATION_TIME */
 attAttachTransportFilename  = ((atpByte        shl 16) or $9001); // PR_ATTACH_TRANSPORT_NAME */
 attAttachRenddata           = ((atpByte        shl 16) or $9002);
 attMAPIProps                = ((atpByte        shl 16) or $9003);
 attRecipTable               = ((atpByte        shl 16) or $9004); // PR_MESSAGE_RECIPIENTS */
 attAttachment               = ((atpByte        shl 16) or $9005);
 attTnefVersion              = ((atpDword       shl 16) or $9006);
 attOemCodepage              = ((atpByte        shl 16) or $9007);
 attOriginalMessageClass     = ((atpWord        shl 16) or $0006); // PR_ORIG_MESSAGE_CLASS */

 attOwner                    = ((atpByte        shl 16) or $0000); // PR_RCVD_REPRESENTING_xxx  or
                                                                   // PR_SENT_REPRESENTING_xxx */
 attSentFor                  = ((atpByte        shl 16) or $0001); // PR_SENT_REPRESENTING_xxx */
 attDelegate                 = ((atpByte        shl 16) or $0002); // PR_RCVD_REPRESENTING_xxx */
 attDateStart                = ((atpDate        shl 16) or $0006); // PR_DATE_START */
 attDateEnd                  = ((atpDate        shl 16) or $0007); // PR_DATE_END */
 attAidOwner                 = ((atpLong        shl 16) or $0008); // PR_OWNER_APPT_ID */
 attRequestRes               = ((atpShort       shl 16) or $0009); // PR_RESPONSE_REQUESTED */

type

  TRendData = packed record
        atyp:WORD;//ATYP;
        ulPosition:ULONG;
        dxWidth:WORD;
        dyHeight:WORD;
        dwFlags:DWORD;
  end;

  TDTR = packed record
        wYear : WORD;
        wMonth : WORD;
        wDay : WORD;
        wHour : WORD;
        wMinute : WORD;
        wSecond : WORD;
        wDayOfWeek : WORD;
   end;

implementation

end.
