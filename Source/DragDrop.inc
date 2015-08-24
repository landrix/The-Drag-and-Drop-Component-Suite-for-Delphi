// -----------------------------------------------------------------------------
// Project:         New Drag and Drop Component Suite
// Module:          DragDrop
// Description:     Implements base classes and utility functions.
// Version:         5.7
// Date:            28-FEB-2015
// Target:          Win32, Win64, Delphi 6-XE7
// Authors:         Anders Melander, anders@melander.dk, http://melander.dk
// Latest Version   https://github.com/landrix/The-new-Drag-and-Drop-Component-Suite-for-Delphi
// Copyright        © 1997-1999 Angus Johnson & Anders Melander
//                  © 2000-2010 Anders Melander
//                  © 2011-2015 Sven Harazim
// -----------------------------------------------------------------------------

// -----------------------------------------------------------------------------
//              Required compiler directives
// -----------------------------------------------------------------------------

{$BOOLEVAL OFF}         // Short circuit boolean evaluation.
{$EXTENDEDSYNTAX ON}    // Enable Delphi Pascal extensions.
{$LONGSTRINGS ON}       // String = AnsiString.
{$ALIGN ON}             // Aligned data. Required by COM.
{$ifdef BCB}
  {$ObjExportAll ON}    // Required for C++ Builder
{$endif}

// The following are not realy nescessary, but they are good practice.
{$TYPEDADDRESS ON}      // @ operator returns typed pointer.
{$WRITEABLECONST OFF}   // Typed consts are R/O.

// Disable platform warnings. This library is only supported on Windows.
{$if CompilerVersion >= 14.0}
{$WARN SYMBOL_PLATFORM OFF}
{$WARN UNIT_PLATFORM OFF}
{$ifend}
// Disable "unsafe" warnings. Let's assume I know what I'm doing.
{$IF CompilerVersion >= 15.0}
{$WARN UNSAFE_TYPE OFF}
{$WARN UNSAFE_CODE OFF}
{$WARN UNSAFE_CAST OFF}
{$ifend}

