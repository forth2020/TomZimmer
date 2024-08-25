\    File: WinBase.f
\  Author: Jeff Kelm
\ Created: 27-Aug-1998
\ Updated: 13-Oct-1998
\ Extensions and defines for Win32For to aid in the example programs



\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ \\\ Redefined ?WinError to give more debugging information
\
: DefinedAbort,   \ compiles an ABORT" with the file name and line where defined
   STATE @ IF
     SAVE-INPUT  2DROP 2DROP ROT
       IF  S" Defined in file: "  TEMP$ PLACE
           ROT COUNT              TEMP$ +PLACE
           S"  -- Line: "         TEMP$ +PLACE
           DROP (.)               TEMP$ +PLACE
           POSTPONE (ABORT")
           TEMP$ COUNT
           HERE >R DUP C,
           DUP ALLOT
           R@ 1+ SWAP MOVE
           0 C,
           ALIGN
           R> COUNT \N->CRLF
     ELSE  2DROP DROP
     THEN
   ELSE  POSTPONE DROP
   THEN ;

: (?WinError) ( f)   \ show an error dialog box if f=FALSE/0
   0= IF  MB_OK MB_ICONWARNING or  Z" Error" rel>abs
       NULL MAXSTRING TEMP$ rel>abs NULL Call GetLastError NULL
       FORMAT_MESSAGE_FROM_SYSTEM  Call FormatMessage
       IF  TEMP$  ELSE  Z" Unknown Error"  THEN
       rel>abs NULL  Call MessageBox drop
   THEN ;

: ?WinError ( f)
   POSTPONE DUP  POSTPONE (?WinError)  POSTPONE 0= DefinedAbort, ; IMMEDIATE



\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ \\\ Extensions to Win32For
\
: DEFAULTOF   \ defines a default condition for CASE structure (must be last!)
   POSTPONE DUP POSTPONE OF ; IMMEDIATE

200 VALUE (NextID)
: CreateNewID ( -- id)   \ create a new id number (hopefully unique)
   (NextID) DUP 1+ TO (NextID) ;

WinLibrary COMCTL32.DLL   \ Add the advanced control library.
                          \ Needed for CreateToolbar, CreateToolbarEx, CreateStatusWindow

\ are we running on Win 3.x or WFW?
Win32s? [IF]   \ for Win32s

   \ defines not in Win32for WINCON.DLL
   NEEDS defines.f

[ELSE]   \ for Win32

   \ defines not in Win32for WINCON.DLL
       TTN_GETDISPINFOA CONSTANT TTN_NEEDTEXT       \ ANSI version
     TBN_GETBUTTONINFOA CONSTANT TBN_GETBUTTONINFO  \ ANSI version
           TTM_ADDTOOLA CONSTANT TTM_ADDTOOL        \ ANSI version
             WM_USER 1+ CONSTANT SB_SETTEXT

   \ Strings not defined in Win32for
   : STATUSCLASSNAME   Z" msctls_statusbar32" rel>abs ;
   : TOOLBARCLASSNAME  Z" ToolbarWindow32"    rel>abs ;
   : WC_TREEVIEW       Z" SysTreeView32"      rel>abs ;
   : TOOLTIPS_CLASS    Z" tooltips_class32"   rel>abs ;

[THEN]


\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ \\\ Load Resources
\
\ LR_LOADFROMFILE is not claimed to work on NT, but the documentation
\ appears to be out of date since it works on my NT machine.
\    For icons, the alternative would be:
\       0 Z" Toolbar.ico" rel>abs  appInst
\       Call ExtractIcon  DUP VALUE hIcon  ?WinError
\    Couldn't find a simple alternative for bitmaps and didn't look
\    for one for cursors.

\ a is a relative address of a zString for file name,
\ f is resource type (IMAGE_ICON, IMAGE_BITMAP, or IMAGE_CURSOR)
\ returns handle to resource
: GetResource ( a f -- handle)
   2>R  LR_LOADFROMFILE 0 0 R> R> rel>abs
   NULL  Call LoadImage  DUP ?WinError ;

\ create an icon resource from file
: GetIconResource ( a -- handle)
   IMAGE_ICON  GetResource ;

\ create a bitmap resource from file
: GetBmpResource ( a -- handle)
   IMAGE_BITMAP  GetResource ;

\ create a cursor resource from file
: GetCurResource ( a -- handle)
   IMAGE_CURSOR  GetResource ;



\ MAKELONG macro creates an unsigned 32-bit by concatenating two 16-bit values
: MAKELONG ( hi lo -- n)
   0xFFFF AND  SWAP 16 LSHIFT  OR ;



\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ \\\ A basic window class with some primative methods
\
:Class BaseWindow  <Super Object

    INT hWnd   \ the window handle

:M GetHandle: ( -- hWnd)   \ return the window handle
        hWnd
        ;M

:M PutHandle: ( hWnd)   \ sets the window handle
        TO hWnd
        ;M

:M SendMessage: ( lparam wparam msg -- result)
        GetHandle: self  Call SendMessage
        ;M

:M Show: ( -- )   \ show the window
        SW_SHOW  GetHandle: self  Call ShowWindow DROP
        ;M

:M Hide: ( -- )   \ hide the window
        SW_HIDE  GetHandle: self  Call ShowWindow DROP
        ;M

:M SetWindowLong: ( n offset)   \ set window memory contents at offset
        GetHandle: self  Call SetWindowLong ?WinError
        ;M

:M GetWindowLong: ( offset -- n)   \ get window memory contents at offset
        GetHandle: self  Call GetWindowLong DUP ?WinError
        ;M

:M GetWindowRect: ( -- b r t l)   \ get window bounding rectangle
        HERE rel>abs GetHandle: self  Call GetWindowRect  ?WinError
        HERE 3 CELLS + @  HERE 2 CELLS + @  HERE CELL+ @  HERE @
        ;M

:M GetClientRect: ( -- b r t l)   \ get windows client rectangle
        HERE rel>abs GetHandle: self  Call GetClientRect ?WinError
        HERE 3 CELLS + @  HERE 2 CELLS + @  HERE CELL+ @  HERE @
        ;M

:M GetWindowSize: ( -- h w)   \ returns height & width of window
        HERE rel>abs GetHandle: self  Call GetWindowRect ?WinError
        HERE 3 CELLS + @  HERE CELL+ @ -
        HERE 2 CELLS + @  HERE @ -
        ;M

:M GetClientSize: ( -- h w)   \ returns height & width of window's client area
        HERE rel>abs GetHandle: self  Call GetClientRect ?WinError
        HERE 3 CELLS + @  HERE CELL+ @ -
        HERE 2 CELLS + @  HERE @ -
        ;M

:M GetWindowPos: ( -- top left )   \ returns position of upper-left corner
        HERE rel>abs GetHandle: self  Call GetWindowRect ?WinError
        HERE 2@
        ;M

:M MoveTo: ( top left)  \ set position of UL corner of window
        2>R  SWP_NOOWNERZORDER SWP_NOSIZE OR
        0 0 2R> NULL
        GetHandle: self Call SetWindowPos ?WinError
        ;M

:M Resize: ( h w)   \ set window dimensions
        2>R  SWP_NOOWNERZORDER SWP_NOMOVE OR
        2R> 0 0 NULL
        GetHandle: self Call SetWindowPos ?WinError
        ;M

:M Destroy: ( -- )   \ destroy the window
        GetHandle: self  Call DestroyWindow  ?WinError
        ;M

;Class



:Class BaseChildWindow  <Super BaseWindow

    INT hParent   \ handle to window's parent

:M GetParent: ( -- hParent)
        hParent
        ;M

:M PutParent: ( hParent)
        TO hParent
        ;M

;Class

