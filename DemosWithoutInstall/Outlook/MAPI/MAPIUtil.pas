{

THIS FILE MUST BE THE LAST AMONG THE MAPI HEADERS IN THE "USES" CLAUSE
IF YOU WANT TO DYNAMICALLY LINK TO THE MAPI32.DLL !!!!

}

unit MAPIUtil;

interface

uses MAPIDefs,Windows,ActiveX,MAPIX, MAPIGUID, IMessage, TNEF;

const
  Mapi32Dll = 'mapi32.dll';

  // RTF Sync Utilities
  RTF_SYNC_RTF_CHANGED  = $00000001;
  RTF_SYNC_BODY_CHANGED = $00000002;

type

  ITableData = interface;

  TCallerRelease = procedure(ulCallerData : ULONG; lpTblData : ITableData; lpVue : IMAPITable);stdcall;

  ITableData = interface(IUnknown)
    [strIID_IMAPITableData]
    function HrGetView(lpSSortOrderSet : PSSortOrderSet; lpfCallerRelease : TCallerRelease;
                       uCallerData : ULONG; var lppMAPITable : IMAPITable):HResult;stdcall;
    function HrModifyRow(lpSRow : PSRow):HResult;stdcall;
    function HrDeleteRow(lpSPropValue : PSPropValue):HResult;stdcall;
    function HrQueryRow(lpSPropValue : PSPropValue; var lppSRow : PSRow; var lpuliRow : ULONG):HResult;stdcall;
    function HrEnumRow(ulRowNumber : ULONG; var lppSRow : PSRow):HResult;stdcall;
    function HrNotify(ulFlags : ULONG; cValues : ULONG; lpSPropValue : PSPropValue):HResult;stdcall;
    function HrInsertRow(uliRow : ULONG; lpSRow : PSRow):HResult;stdcall;
    function HrModifyRows(ulFlags : ULONG; lpSRowSet : PSRowSet):HResult;stdcall;
    function HrDeleteRows(ulFlags : ULONG; lprowsetToDelete : PSRowSet; var cRowsDeleted : ULONG):HResult;stdcall;
  end;

  PDTCTL = ^TDTCTL;
  TDTCTL = record
    ulCtlType : ULONG;
    ulCtlFlags : ULONG;
    lpbNotif : PBYTE;
    cbNotif : ULONG;
    lpszFilter : LPTSTR;
    ulItemID : ULONG;
    case integer of
      0 : ( lpv : pointer );
      1 : ( lplabel : PDTBLLABEL);
      2 : ( lpedit : PDTBLEDIT);
      3 : ( lplbx : PDTBLLBX);
      4 : ( lpcombobox : PDTBLCOMBOBOX);
      5 : ( lpddlbx : PDTBLDDLBX);
      6 : ( lpcheckbox : PDTBLCHECKBOX);
      7 : ( lpgroupbox : PDTBLGROUPBOX);
      8 : ( lpbutton : PDTBLBUTTON);
      9 : ( lpradiobutton : PDTBLRADIOBUTTON);
      10: ( lpmvlbx : PDTBLMVLISTBOX);
      11: ( lpmvddlbx : PDTBLMVDDLBX);
      12: ( lppage : PDTBLPAGE);
  end;

  PDTPage = ^TDTPage;
  TDTPage = record
    cctl : ULONG;
    lpszResourceName : LPTSTR;
    case integer of
      0 : (
        lpszComponent : LPTSTR;
          );
      1 : (
        ulItemID : ULONG;
        lpctl : PDTCTL;
          );
  end;

//*****************************************************
//        MAPIUtil function
//*****************************************************

TBuildDisplayTable = function(lpAllocateBuffer : PALLOCATEBUFFER;
                              lpAllocateMore : PALLOCATEMORE;
                              lpFreeBuffer : PFREEBUFFER;
                              lpMalloc : IMalloc;
                              HInstance : Longint;
                              cPages : UINT;
                              lpPage : PDTPage;
                              ulFlags : ULONG;
                              var lppTable : IMAPITable;
                              var lppTblData : ITableData):HResult;stdcall;

TCreateTable = function (const lpInterface: TGUID; lpAllocateBuffer : PALLOCATEBUFFER;
                         lpAllocateMore : PALLOCATEMORE; lpFreeBuffer : PFREEBUFFER;
                         lpvReserved : pointer; ulTableType : ULONG;
                         ulPropTagIndexColumn : ULONG;
                         lpSPropTagArrayColumns : PSPropTagArray;
                         var lppTableData : ITableData):HResult;stdcall;

THrQueryAllRows = function (ptable : IMAPITABLE;
                        ptaga:PSPropTagArray;
                        pres:PSRestriction;
                        psos:PSSortOrderSet;
                        crowsMax:longint;
                        var pprows:PSRowSet):HResult;stdcall;

TOpenStreamOnFile = function (lpAllocateBuffer:PALLOCATEBUFFER;
                          lpFreeBuffer:PFREEBUFFER;
                          ulFlags:ULONG;
                          lpszFileName:PAnsiChar;
                          lpszPrefix:PChar;
                          var Stream:IStream):HRESULT;stdcall;

TFreeProws = function (prows : PSRowSet):HResult; stdcall;

TFreePAdrList = function (prows : PAdrList):HResult; stdcall;

THrSzFromEntryID = function (cb : ULONG; pentry : PEntryID; var psz: PChar):HResult; stdcall;

THrGetOneProp = function (pmp : IMAPIPROP; ulPropTag:ULONG; var ppprop : PSPropValue):HResult; stdcall;

THrSetOneProp = function (pmp : IMAPIPROP; pprop : PSPropValue):HResult; stdcall;

TWrapCompressedRTFStream = function (lpCompressedRTFStream : IStream; ulflags : ULONG; var lpUncompressedRTFStream : IStream): HResult; stdcall;
TRTFSync = function(lpMessage : MAPIDefs.IMessage; ulFlags : ULONG; var lpfMessageUpdated : BOOL):HResult; stdcall;

THrEntryIDFromSz = function (sz : PChar; var pcb: ULONG; var ppentry : PENTRYID):HResult; stdcall;

TOpenTnefStreamEx = function (lpvSupport : pointer; lpStreamName : IStream;
                              lpszStreamName : PChar; ulFlags : ULONG; lpMessage : MAPIDefs.IMessage;
                              wKeyVal : ULONG {?}; lpAdressBook : IADDRBOOK; var lppTNEF : ITNEF) :HResult; stdcall;

TMAPIInitIdle = function(lpvReserved : pointer):HResult; stdcall;
TMAPIDeinitIdle = procedure;stdcall;
TFtgRegisterIdleRoutine = function (lpfnIdle : pointer; lpvIdleParam : pointer;
            priIdle : integer; csecIdle : ULONG; iroIdle : ULONG):HResult; stdcall;
TDeregisterIdleRoutine  = procedure(ftg : ULONG);stdcall;


//*****************************************************
//        IMessage function
//*****************************************************

TOpenIMsgSession = function  (
  lpMalloc : IMalloc;                          { -> Co malloc object          }
  ulFlags : ULONG;                             { reserved. Must be zero.      }
  var lppMsgSess : PMSGSESS) : SCODE; stdcall; { <- message session object    }

TCloseIMsgSession = procedure  (
  lpMsgSess : PMSGSESS); stdcall;              { -> message session object    }

TOpenIMsgOnIStg = function  (
  lpMsgSess : PMSGSESS;                { -> message session obj (optional) }
  lpAllocateBuffer : PALLOCATEBUFFER;  { -> AllocateBuffer memory routine  }
  lpAllocateMore : PALLOCATEMORE;      { -> AllocateMore memory routine    }
  lpFreeBuffer : PFREEBUFFER;          { -> FreeBuffer memory routine      }
  lpMalloc : IMalloc;                  { -> Co malloc object               }
  lpMapiSup : Pointer;                 { -> MAPI Support Obj (optional)    }
  lpStg : IStorage;                    { -> open IStorage containing msg   }
  var lpfMsgCallRelease : TMSGCALLRELEASE;{ -> release callback rtn (opt) }
  ulCallerData : ULONG;                { caller data returned in callback  }
  ulFlags : ULONG;                     { -> flags (controls istg commit)   }
  out lppMsg : MapiDefs.IMessage) : SCODE; stdcall;

TGetAttribIMsgOnIStg = function  (lpObject : Pointer;
  lpPropTagArray : PSPropTagArray;
  var lppPropAttrArray : PSPropAttrArray) : HResult; stdcall;

TSetAttribIMsgOnIStg = function  (lpObject : Pointer;
  lpPropTags : PSPropTagArray; lpPropAttrs : PSPropAttrArray;
  var lppPropProblems : PSPropProblemArray) : HResult; stdcall;

TMapStorageSCode = function  (StgSCode : SCODE) : SCODE; stdcall;

//*****************************************************
//        MAPIX function
//*****************************************************

TMAPIInitialize = function  (lpMapiInit : Pointer) : HResult; stdcall;

TMAPIUninitialize = procedure ; stdcall;

TMAPILogonEx = function  (ulUIParam : ULONG; lpszProfileName : PChar;
  lpszPassword : PChar; ulFlags : ULONG; {  ulFlags takes all that SimpleMAPI does + MAPI_UNICODE }
  out lppSession : IMAPISession) : HResult; stdcall;

TMAPIAllocateBuffer = function  (cbSize : ULONG;
  var lppBuffer : Pointer) : SCODE; stdcall;

TMAPIAllocateMore = function  (cbSize : ULONG; lpObject : Pointer;
  var lppBuffer : Pointer) : SCODE; stdcall;

TMAPIFreeBuffer = function  (lpBuffer : Pointer) : ULONG; stdcall;

TMAPIAdminProfiles = function  (ulFlags : ULONG;
  out lppProfAdmin : IProfAdmin) : HResult; stdcall;

TMAPIGetDefaultMalloc = function:pointer;stdcall;



var HrQueryAllRows:THrQueryAllRows = nil;
    OpenStreamOnFile:TOpenStreamOnFile = nil;
    FreeProws:TFreeProws = nil;
    FreePAdrList:TFreePAdrList = nil;
    HrSzFromEntryID:THrSzFromEntryID = nil;
    HrEntryIDFromSz:THrEntryIDFromSz = nil;
    HrGetOneProp:THrGetOneProp = nil;
    HrSetOneProp:THrSetOneProp = nil;
    WrapCompressedRTFStream:TWrapCompressedRTFStream = nil;
    RTFSync:TRTFSync = nil;
    CreateTable : TCreateTable = nil;
    BuildDisplayTable : TBuildDisplayTable = nil;

    OpenIMsgSession:TOpenIMsgSession = nil;
    CloseIMsgSession:TCloseIMsgSession= nil;
    OpenIMsgOnIStg:TOpenIMsgOnIStg= nil;
    GetAttribIMsgOnIStg:TGetAttribIMsgOnIStg= nil;
    SetAttribIMsgOnIStg:TSetAttribIMsgOnIStg= nil;
    MapStorageSCode:TMapStorageSCode= nil;

    MAPIInitialize:TMAPIInitialize = nil;
    MAPIUninitialize:TMAPIUninitialize = nil;
    MAPILogonEx:TMAPILogonEx = nil;
    MAPIAllocateBuffer:TMAPIAllocateBuffer = nil;
    MAPIAllocateMore:TMAPIAllocateMore = nil;
    MAPIFreeBuffer:TMAPIFreeBuffer = nil;
    MAPIAdminProfiles:TMAPIAdminProfiles = nil;
    MAPIGetDefaultMalloc:TMAPIGetDefaultMalloc = nil;
    OpenTnefStreamEx:TOpenTnefStreamEx = nil;

    MAPIInitIdle : TMAPIInitIdle= nil;
    MAPIDeinitIdle : TMAPIDeinitIdle = nil;
    FtgRegisterIdleRoutine :TFtgRegisterIdleRoutine = nil;
    DeregisterIdleRoutine : TDeregisterIdleRoutine = nil;

const
     IRONULL          = $0000;
     FIROWAIT         = $0001;
     FIROINTERVAL     = $0002;
     FIROPERBLOCK     = $0004;
     FIRODISABLED     = $0020;
     FIROONCEONLY     = $0040;


var MAPIDLLHandle:THandle;

implementation

initialization
  MAPIDLLHandle:=LoadLibrary('MAPI32.DLL');
  if MAPIDLLHandle <> 0 then begin

    HrQueryAllRows:=GetProcAddress(MAPIDLLHandle,'HrQueryAllRows@24');
    OpenStreamOnFile:=GetProcAddress(MAPIDLLHandle,'OpenStreamOnFile@24');
    FreeProws:=GetProcAddress(MAPIDLLHandle,'FreeProws@4');
    FreePAdrList:=GetProcAddress(MAPIDLLHandle,'FreePadrlist@4');
    HrSzFromEntryID:=GetProcAddress(MAPIDLLHandle,'HrSzFromEntryID@12');
    HrEntryIDFromSz:=GetProcAddress(MAPIDLLHandle,'HrEntryIDFromSz@12');
    HrSzFromEntryID:=GetProcAddress(MAPIDLLHandle,'HrSzFromEntryID@12');
    HrGetOneProp:=GetProcAddress(MAPIDLLHandle,'HrGetOneProp@12');
    HrSetOneProp:=GetProcAddress(MAPIDLLHandle,'HrSetOneProp@8');
    WrapCompressedRTFStream:=GetProcAddress(MAPIDLLHandle,'WrapCompressedRTFStream');
    RTFSync:=GetProcAddress(MAPIDLLHandle,'RTFSync');
    CreateTable:=GetProcAddress(MAPIDLLHandle,'CreateTable@36');
    BuildDisplayTable:=GetProcAddress(MAPIDLLHandle,'BuildDisplayTable@40');

    OpenIMsgSession:=GetProcAddress(MAPIDLLHandle,'OpenIMsgSession@12');
    CloseIMsgSession:=GetProcAddress(MAPIDLLHandle,'CloseIMsgSession@4');
    OpenIMsgOnIStg:=GetProcAddress(MAPIDLLHandle,'OpenIMsgOnIStg@44');
    GetAttribIMsgOnIStg:=GetProcAddress(MAPIDLLHandle,'GetAttribIMsgOnIStg@12');
    SetAttribIMsgOnIStg:=GetProcAddress(MAPIDLLHandle,'SetAttribIMsgOnIStg@16');
    MapStorageSCode:=GetProcAddress(MAPIDLLHandle,'MapStorageSCode@4');

    MAPIInitialize:=GetProcAddress(MAPIDLLHandle,'MAPIInitialize');
    MAPIUninitialize:=GetProcAddress(MAPIDLLHandle,'MAPIUninitialize');
    MAPILogonEx:=GetProcAddress(MAPIDLLHandle,'MAPILogonEx');
    MAPIAllocateBuffer:=GetProcAddress(MAPIDLLHandle,'MAPIAllocateBuffer');
    MAPIAllocateMore:=GetProcAddress(MAPIDLLHandle,'MAPIAllocateMore');
    MAPIFreeBuffer:=GetProcAddress(MAPIDLLHandle,'MAPIFreeBuffer');
    MAPIAdminProfiles:=GetProcAddress(MAPIDLLHandle,'MAPIAdminProfiles');
    MAPIGetDefaultMalloc:=GetProcAddress(MAPIDLLHandle,'MAPIGetDefaultMalloc@0');
    OpenTnefStreamEx:=GetProcAddress(MAPIDLLHandle,'OpenTnefStreamEx');
    MAPIInitIdle:=GetProcAddress(MAPIDLLHandle,'MAPIInitIdle@4');
    MAPIDeinitIdle:=GetProcAddress(MAPIDLLHandle,'MAPIDeinitIdle@0');
    FtgRegisterIdleRoutine:=GetProcAddress(MAPIDLLHandle,'FtgRegisterIdleRoutine@20');
    DeregisterIdleRoutine:=GetProcAddress(MAPIDLLHandle,'DeregisterIdleRoutine@4');

  end;
finalization
  if MAPIDLLHandle <> INVALID_HANDLE_VALUE then FreeLibrary(MAPIDLLHandle)
end.
