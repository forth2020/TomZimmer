\ $Id: childwnd.fth 1.2 1994/03/10 10:55:40 andrew Exp $

cr .( Loading Child Window...)

\ Child Window is the base class for all drawing windows.  The windows
\ have a parent, which is the object address, not the window handle.
\ This allows the child to send messages to its parent.

only forth also definitions

:CLASS Child-Window     <Super Window

int Parent      \ window object that is the parent
int id          \ id for this child window

:M Classinit:   ( -- )
                ClassInit: super
                s" ChildWindow" WindowClassName place
                                WindowClassName +NULL
                ;M

:M GetParent:   ( -- parent )
                Parent ;M

:M SetID:  ( n -- )  to id  ;M
:M GetID:  ( -- n )     id  ;M

\ To change the minimum window size, override the MinSize: method.

\ :M MinSize:   ( -- width height )   0  0 ;M      \ override to change
\ :M StartSize: ( -- width height )   0  0 ;M      \ override to change
\ :M StartPos:  ( -- left  top )      0  0 ;M      \ override to change


\ -------------------- Create Child Window --------------------

\ The child window class has the following properties:
\       Private device context (OWNDC)
\       Black background
\       No icon

: register-child-window  ( -- f )
   12 cells                                            to cbSize
   CS_DBLCLKS CS_HREDRAW + CS_VREDRAW + ( CS_OWNDC + ) to Style
   TheWndProc rel>abs                                  to WndProc
   0                                                   to ClsExtra
   0                                                   to WndExtra
   appInst                                             to hInstance
   NULL                                                to hIcon
   IDC_ARROW NULL Call LoadCursor                      to hCursor
   NULL (  WHITE_BRUSH    Call GetStockObject  )               to hbrBackground
   NULL                                                to MenuName
   WindowClassName 1+ rel>abs                          to ClassName
   0                                                   to hIconSm
   WndClassEx rel>abs Call RegisterClassEx 
;

: create-child-window  ( -- hWnd )
        ^base                           \ creation parameters
        appInst                         \ program instance
        id                              \ child id
        Parent conhndl =
        if      conhndl
        else    GetHandle: Parent       \ parent window handle
        then
        StartSize:     [ self ] swap    \ height, width
        StartPos:      [ self ] swap    \ y, x starting position
        WindowStyle:   [ self ]         \ the window style
        WindowTitle:   [ self ] rel>abs \ the window title
        WindowClassName 1+ rel>abs      \ class name
        ExWindowStyle: [ self ]         \ extended window style
        Call CreateWindowEx ;


:M WindowStyle: ( -- style )            \ return the window style
                WS_CHILD
                WS_VISIBLE or
                ;M

:M ExWindowStyle: ( -- extended_style )
                0
                ;M

:M WindowTitle: ( -- Zstring )          \ window caption
                z"  "                   \ we don't want a name, pass NULL
                ;M

:M Start:       ( Parent -- )
                to Parent
                register-child-window drop
                create-child-window to hWnd
                SW_SHOWNORMAL Show: self
                ;M


;Class

