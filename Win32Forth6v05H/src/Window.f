
\ $Id: window.fth 1.7 1994/03/24 10:52:33 andrew Exp $

\ window.f beta 3.1A 2002/09/28 arm SendMessage -> SendMessageTimeout
\ window.f beta 3.3D 2002/10/08 arm Consolidation
\ window.f beta 4.9C 2003/11/05 ron On_Done: nulled, no need to delete menu, Windows does

cr .( Loading Window Class...)
cr .( -- BETA WINDOW.F 4.9C  --)

\ Window class

only forth also definitions

\ The following windows message definition for WM_WIN32FORTH, provides a
\ way for multiple Win32Forth applications to interact between themselves
\ wile running.

\ -1 value WM_WIN32FORTH

\ Each instance of forth running under windows is able through the method
\ WM32Forth: and a unique set of application specific constants
\ (WM_BEEPME in this example) to detect a message being set to itself,
\ and subsequently perform some specific operation as ordered.
\
\ :M Win32Forth:  ( h m w l -- )  \ The REAL version of this is at the
\                                 \ end of this file
\                 over WM_BEEPME =
\                 if      beep
\                 then
\                 ;M

\ WM_WIN32FOR-INIT following obtains a unique windows message value from
\ Windows then Win32Forth starts up, so this instance of forth will be
\ able to talk to other instances of forth.

create MessageName z," WM_WIN32FORTH"

: wm_win32for-init ( -- )
                MessageName
                rel>abs Call RegisterWindowMessage ?dup
                if      to WM_WIN32FORTH
                then    ;

initialization-chain chain-add wm_win32for-init \ initialize the message

\ WIN32FORTH-MESSAGE allows a unique message to be broadcast to all
\ currently running instances of Win32Forth.  The 'w' parameter is the
\ application specific sub-message that each instance can use to
\ determine if it should handle the message.  The 'l' parameter is
\ available to pass specific information between instances of Win32Forth.

: _win32forth-message ( lParam wParam -- )
                2>R 0 SP@ REL>ABS 2 ( ms ) SMTO_ABORTIFHUNG SMTO_BLOCK OR 2R>
                WM_WIN32FORTH HWND_BROADCAST Call SendMessageTimeout 2drop
                ;

' _win32forth-message is win32forth-message      \ link to deferred word

\       typedef struct tagMSG {     // msg
\           HWND   hwnd;
\           UINT   message;
\           WPARAM wParam;
\           LPARAM lParam;
\           DWORD  time;
\           POINT  pt;
\           } MSG;


\ A Win32Forth-message example:

31415 constant WM_BEEPME    \ a command code to beep

\ Send the message "WM_BEEPME" to all running instances of Win32Forth

: beepme        ( -- )
                0 WM_BEEPME win32forth-message ;

\ The chain "FORTH-MSG-CHAIN" receives all messages that are broadcast by any
\ program with the WM_WIN32FORTH message.  "FORTH-MSG-BEEP" tests the sub-message
\ WM_BEEPME, and if it matches, then it beeps the console. Any sub-message that
\ is not recognized by any instance of a Win32Forth program must be ignored.

: forth-msg-beep ( wParam lParam -- wParam lParam )
                 over WM_BEEPME =
                 if     beep  
                 then   ;

forth-msg-chain chain-add forth-msg-beep \ default first entry to forth messages

0 value DefaultMenuBar  \ global default menubar

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\       Global windows procedure error, used here and in CONTROL.F
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

0 value errCFA
 create errString MAXSTRING ALLOT

: WndProcError  ( error_value cfa -- 0 )
                to errCFA
                ?win-error-enabled
                IF      CASE    -2 OF   msg @ count     errString  place
                                        s"  \n  at: "   errString +place
                                        BASE @ >R  HEX
                                        errCFA (.)      errString +place
                                        r> BASE !
                                        errString count ErrorBox
                                   ENDOF
                        ENDCASE
                ELSE    drop
                THEN    0 ;

\ ------------------------------------------------------------

Rectangle WndRect

:CLASS Window         <SUPER Generic-Window

\ -------------------- Window Sizing --------------------

      int CurrentPopup          \ current right mouse popup menu
      int CurrentMenu           \ current menubar
      int Width                 \ current width of client area
      int Height                \ current height of client area
      int OriginX
      int OriginY
      int cursor-on?
      int have-focus?
      int click-func
      int unclick-func
      int dbl-click-func
      int track-func
      int tracking?
      int clicking?
MAXSTRING bytes WindowClassName
    WinDC dc                    \ The window's device context
 16 cells bytes &ps             \ pointer to paint structure

:M Classinit:   ( -- )
                ClassInit: super
                  0 to OriginX
                  0 to OriginY
                  0 to cursor-on?
                  0 to have-focus?
                  0 to tracking?
                  0 to clicking?
                  0 to CurrentPopup
                  0 to CurrentMenu
                640 to Width
                480 to Height
                ['] noop to click-func
                ['] noop to unclick-func
                ['] noop to dbl-click-func
                ['] noop to track-func
                s" ForthAppWindow" WindowClassName place
                                   WindowClassName +NULL
                ;M

: set-size  ( lParam -- )
        word-split  to Height  to Width ;

\ User windows should override the On_Size: method. When this method is
\ called, the variables Width and Height will have already been set.

:M On_Size:     ( -- )                  \ default does nothing
                ;M

:M GetSize:     ( -- w h )
                Width Height
                ;M

:M Width:       ( -- width )
                Width
                ;M

:M Height:      ( -- height )
                Height
                ;M

:M SetSize:     ( w h -- )
                to Height
                to Width
                ;M

:M WM_SIZE      ( hndl msg wparam lparam -- res )
                set-size
                On_Size: [ self ]
                0 ;M

:M WM_MOVE      ( hwnd msg wparam lparam -- res )
                EraseRect: WndRect                      \ make a new rectangle
                WndRect.AddrOf rel>abs
                hWnd Call GetWindowRect                 \ adjust the window
                if      WndRect.Left to OriginX
                        WndRect.Top  to OriginY
                then    EraseRect: WndRect
                0 ;M

\ To change the minimum window size, override the MinSize: method.

:M MinSize:     ( -- width height )    10     10 ;M     \ override to change
:M MaxSize:     ( -- width height )  8192   8192 ;M     \ override to change
:M StartSize:   ( -- width height ) Width Height ;M     \ override to change

:M StartPos:    ( -- left  top )
                OriginX
                OriginY
                ;M

:M SetOrigin:   ( x y -- )
                screen-size 100 - rot min 0max to OriginY
                            100 -     min 0max to OriginX
                ;M

:M WM_GETMINMAXINFO ( hwnd msg wparam lparam -- res )
        >r
        WindowHasMenu: [ self ]                 \ have menu flag?
          WindowStyle: [ self ]                 \ the window style
        0 0                                     \ adjust x,y relative to zero zero
              MinSize: [ self ]
        SetRect: WndRect
        WndRect.AddrOf rel>abs                  \ make a new rectangle
        Call AdjustWindowRect ?win-error        \ adjust the window
        WndRect.Bottom WndRect.Top -            \ adjusted height
        WndRect.Right  WndRect.Left -           \ adjusted  width
        r@ ( lparam ) abs>rel 6 cells+ 2!
        WindowHasMenu: [ self ]                 \ have menu flag?
          WindowStyle: [ self ]                 \ the window style
        0 0                                     \ adjust x,y relative to zero zero
              MaxSize: [ self ]
        SetRect: WndRect
        WndRect.AddrOf rel>abs                  \ make a new rectangle
        Call AdjustWindowRect ?win-error        \ adjust the window
        WndRect.Bottom WndRect.Top -            \ adjusted height
        WndRect.Right  WndRect.Left -           \ adjusted  width
        EraseRect: WndRect
        r> ( lparam ) abs>rel 8 cells+ 2!
        ;M


\ -------------------- Window Class Procedure --------------------

\ The window will store its object address at offset zero in the Win32
\ window structure.  We will need to allocate 4 bytes of extra storage
\ when we register the window.  When the window is created Win32 will
\ initialize this memory to zero.  Until this data is filled in, we pass
\ all messages to DefWindowProc.  When we call CreateWindow, we pass the
\ base address of the object in the creation parameters field.  This is
\ the first DWORD of the CREATEPARMS structure passed in the lParam of the
\ WM_CREATE message.  When we see this message, we retrieve the object
\ address and store it in the extra window memory.  After this, we can
\ just retrieve it and know the address of the object.
\
\ We then search the object's class for the given message selector.  If
\ it is found we execute the method, otherwise we just call
\ DefWindowProc.

: (WndProc)   ( hwnd msg wparam lparam -- res )
        [ also classes ]
        GWL_USERDATA 4 pick Call GetWindowLong  ( object address )

        ?dup 0=
        if
                2 pick WM_CREATE <>
                if
                        DefaultWindowProc exit
                then

                dup abs>rel @   \ window object pointer from
                                \ first cell of CREATEPARMS

                4 pick ( obj hwnd )
                2dup GWL_USERDATA swap Call SetWindowLong drop \ save pointer
                over !          \ set hWnd parameter of window struc

        then

        3 pick ( msg ) over obj>class MFA ((findm))
        if      sp0 @ >r sp@ 4 cells+ sp0 !
                dup>r catch
                ?dup
                if      r@ WndProcError
                then    r>drop
                r> sp0 !
        else    \ -- a1                                 \ the object address
                DefWindowProc: [ ( a1 -- ) ]            \ gets used here
        then

        [ previous ] ;


4 callback TheWndProc (wndproc)
\ ' (wndproc) WndProc TheWndProc


\ -------------------- Register Class --------------------

\ The default window class is appropriate for frame windows.  Child
\ windows will define their own window class.  The default window class
\ has the following properties:
\       No private device context (??)
\       Black background
\       Application icon
((
Record: WndClass
        int Style
        int WndProc
        int ClsExtra
        int WndExtra
        int hInstance
        int hIcon
        int hCursor
        int hbrBackground
        int MenuName
        int ClassName
;Record

: default-window-class ( -- )   \ fill in the defaults for the window class
        CS_DBLCLKS CS_HREDRAW + CS_VREDRAW + ( CS_OWNDC + )     to Style
        TheWndProc rel>abs                                      to wndProc
        0                                                       to clsExtra
        0                                                       to wndExtra
        appInst                                                 to hInstance
\       IDI_APPLICATION NULL         Call LoadIcon              to hIcon
        101 appinst                  Call LoadIcon              to hIcon
\       IDC_ARROW NULL               Call LoadCursor            to hCursor
        DefaultCursor: [ self ] NULL Call LoadCursor            to hCursor
        WHITE_BRUSH                  Call GetStockObject        to hbrBackground
        NULL                                                    to MenuName
        WindowClassName 1+ rel>abs                              to ClassName ;

: register-the-class    ( -- f )        \ register the class structure
        WndClass rel>abs  Call RegisterClass ;
))
Record: WndClassEx
   int cbSize
   int Style
   int WndProc
   int ClsExtra
   int WndExtra
   int hInstance
   int hIcon
   int hCursor
   int hbrBackground
   int MenuName
   int ClassName
   int hIconSm
;Record

: default-window-class ( -- )   \ fill in the defaults for the window class
   12 cells                                              to cbSize
   CS_DBLCLKS CS_HREDRAW + CS_VREDRAW + ( CS_OWNDC + )   to Style
   TheWndProc rel>abs                                    to wndProc
   0                                                     to clsExtra
   0                                                     to wndExtra
   appInst                                               to hInstance
   \ IDI_APPLICATION NULL         Call LoadIcon            to hIcon
   101 appinst                  Call LoadIcon            to hIcon
   \ IDC_ARROW NULL               Call LoadCursor          to hCursor
   DefaultCursor: [ self ] NULL Call LoadCursor          to hCursor
   WHITE_BRUSH                  Call GetStockObject      to hbrBackground
   NULL                                                  to MenuName
   WindowClassName 1+ rel>abs                            to ClassName 
   0                                                     to hIconSm
;

: register-the-class    ( -- f )        \ register the class structure
        WndClassEx rel>abs  Call RegisterClassEx ;

: register-frame-window  ( -- f )       \ init the class and register it
        default-window-class
        register-the-class ;

: create-frame-window  ( -- hwnd )
        0 0                                     \ adjust x,y relative to 0,0
        StartSize:     [ self ]                 \ width, height
        SetRect: WndRect
        WindowHasMenu: [ self ]                 \ have menu flag?
        WindowStyle:   [ self ]                 \ the window style
        AddrOf: WndRect rel>abs                 \ make a new rectangle
        call AdjustWindowRect ?win-error        \ adjust the window
        ^base                                   \ creation parameters
        appInst                                 \ program instance
        NULL LoadMenu: [ self ]                 \ menu
        ParentWindow:  [ self ]                 \ parent window handle
        Bottom: WndRect  Top: WndRect -         \ adjusted height
         Right: WndRect Left: WndRect -         \ adjusted  width
        StartPos:      [ self ] swap            \ y, x starting position
        WindowStyle:   [ self ]                 \ the window style
        WindowTitle:   [ self ] rel>abs         \ the window title
        WindowClassName 1+ rel>abs              \ class name
        ExWindowStyle: [ self ]                 \ extended window style
        Call CreateWindowEx
        EraseRect: WndRect ;


\ January 31st, 1997 - 14:36 tjz changed these two methods to use address
\                                and count, rather than 'z' strings.

:M SetClassName: ( adr len -- )
                WindowClassName place
                ;M

:M GetClassName: ( -- adr len )
                WindowClassName count
                ;M

:M ParentWindow: ( -- parent )          \ return the parent, or 0 = no parent
                0
                ;M

:M DefaultCursor: ( -- cursor-id )      \ return the default cursor for window
                IDC_ARROW
                ;M

:M WindowHasMenu: ( -- flag )
                 FALSE
                 ;M

:M WindowStyle: ( -- style )            \ return the window style
                WS_OVERLAPPEDWINDOW
                ;M

:M ExWindowStyle: ( -- extended_style )
                0
                ;M

:M WindowTitle: ( -- Zstring )          \ window caption
                z" Window"
                ;M

:M SetTitle:    { adr len \ temp$ -- }
                MAXSTRING LocalAlloc: temp$
                adr len "CLIP" temp$ place
                               temp$ +NULL
                               temp$ 1+ rel>abs
                hWnd call SetWindowText ?win-error \ change the window title
                ;M

\ -------------------- Get/Release DC --------------------

: get-dc      ( -- )    GetDC: self  PutHandle: dc ;

: release-dc  ( -- )    GetHandle: dc  ReleaseDC: self ;


\ -------------------- Font Selection --------------------

: system-fixed-font  ( -- )
        SYSTEM_FIXED_FONT SelectStockObject: dc drop ;

: small-font  ( -- )
        ANSI_FIXED_FONT SelectStockObject: dc drop ;

\ -------------------- Begin/End Paint --------------------

\ begin-paint and end-paint must be used in pairs in the same definition.
\ This can be the WM_PAINT method or a word called from it.

: begin-paint  ( -- )
        &ps BeginPaint: self  PutHandle: dc
        ;

: end-paint  ( -- 0 )
        &ps EndPaint: self
        0 ;


\ User windows should override the On_Paint: method. When this method is
\ called, BeginPaint is already setup.

:M On_Paint:    ( -- )               \ default does nothing
\               0 0 S" Hello, Windows!" TextOut: dc
                ;M

:M WM_PAINT     ( hwnd msg wparam lparam -- res )
                begin-paint
                On_Paint: [ self ]
                end-paint ;M


\ -------------------- Window Initialization  --------------------

:M LoadMenu:    ( parent -- menuhandle )
                drop                            \ window has no parent
                WindowHasMenu: [ self ]         \ have menu flag?
                if      CurrentMenu 0=
                        if      DefaultMenuBar to CurrentMenu
                        then
                        MenuHandle: CurrentMenu 0=
                        if   self LoadMenu: CurrentMenu
                        then MenuHandle: CurrentMenu
                else    NULL
                then
                ;M


:M SetMenuBar:  { menubar -- }
                menubar  to CurrentMenu
                self Start: CurrentMenu
                ;M

:M SetPopupBar: { menubar -- }
                menubar  to CurrentPopup
                self Start: CurrentPopup
                ;M

:M Start:       ( -- )         \ create a new window object
                hWnd 0=
                if      register-frame-window drop
                        create-frame-window to hWnd
                        SW_SHOWNORMAL Show: self
                        Update: self
                else    SetFocus: self
                then
                ;M

:M Enable:      ( f1 -- )
                hWnd Call EnableWindow drop
                ;M

:M On_Init:     ( -- )                \ dc has already been setup
                ;M

:M On_Done:     ( -- )
                ;M

\         Your On_Init: and On_Done: methods should look like this:
\
\         :M On_Init:  ( -- )
\                 On_Init: super
\                 ... other initialization ...
\                 :m
\
\         :M On_Done:  ( -- )
\                 ... other cleanup ...
\                 On_Done: super
\                 :M

\ An application window will need the following methods, which cause the
\ program to terminate when the user closes the main application window.
\ Don't un-comment these out here, copy them into your application window
\ Object or Class, and them un-comment them out.

\ :M WM_CLOSE     ( h m w l -- res )
\                 bye
\                 0 ;M
\
\ :M On_Done:     ( h m w l -- res )
\                 0 call PostQuitMessage drop     \ terminate application
\                 On_Done: super                  \ cleanup the super class
\                 0 ;M

:M WM_CLOSE     ( hwnd msg wparam lparam -- res )
                Close: [ self ]
                ;M

:M WM_CREATE
                On_Init: [ self ]
                0 ;M

:M WM_DESTROY
                On_Done: [ self ]
                DefWindowProc: [ self ]
                0 to hWnd
                ;M

\ An application window will need the following definition to cause the
\ program to terminate when the user closes the main application window.

\ :M WM_CLOSE     ( h m w l -- res )
\                 bye
\                 0 ;M

int c-width
int c-height
int c-x
int c-y

:M MoveCursor:  ( gx gy -- )
                1+              \ correct for single pixel start offset
                have-focus?
                 cursor-on? and
                hWnd 0<>    and
                if      hWnd call HideCaret   ?win-error
                        2dup to c-y to c-x
                        swap call SetCaretPos ?win-error
                        hWnd call ShowCaret   ?win-error
                else    to c-y to c-x
                then    ;M

:M MakeCursor:  ( gx gy width height -- )
                have-focus? cursor-on? 0= and
                if      2dup to c-height to c-width
                        swap 0
                        hWnd call CreateCaret ?win-error
                        2dup to c-y to c-x
                        swap call SetCaretPos ?win-error
                        hWnd call ShowCaret   ?win-error
                        TRUE to cursor-on?
                else    2drop 2drop
                then
                ;M

:M DestroyCursor: ( -- )
                have-focus? cursor-on? and
                hWnd 0<>               and
                if      hWnd call HideCaret    drop
                             call DestroyCaret drop
                        FALSE to cursor-on?
                then
                ;M

:M ShowCursor:  ( -- )
                cursor-on? 0=
                if      c-x c-y c-width c-height MakeCursor: self
                then
                ;M

:M HideCursor:  ( -- )
                cursor-on?
                if      DestroyCursor: self
                then    FALSE to cursor-on?
                ;M

:M On_SetFocus: ( h m w l -- )
                true to have-focus?
\ example: When cursor is used, you will need something like the following
\          to control the position of the cursor in the window
\               cursor-col char-width  *
\               cursor-row char-height *
\               char-width char-height MakeCursor: self
                ;M

:M WM_SETFOCUS  ( h m w l -- )
                On_SetFocus: [ self ]
                0 ;M

:M On_KillFocus: ( h m w l -- )
\ example: Use only when you are displaying a cursor in the window
\               DestroyCursor: self
                false to have-focus?
                ;M

:M WM_KILLFOCUS ( h m w l -- res )
                On_KillFocus: [ self ]
                0 ;M

:M PushKey:     ( c1 -- )       \ override to process keys yourself
                pushkey
                ;M

: vga-skey      ( c1 -- )       \ push a special key with control or
                                \ shift bits
                VK_CONTROL Call GetKeyState 0x8000 and
                if control_mask or then
                VK_SHIFT   Call GetKeyState 0x8000 and
                if shift_mask or then
                PushKey: [ self ] ;

: shift-pushkey ( c1 -- )       \ push a special key with shift bits
                dup 32 <                        \ if less than 32
                VK_SHIFT Call GetKeyState 0x8000 and and
                                                \ and shift was pressed
                if shift_mask or then  PushKey: [ self ] ;

:M WM_CHAR      ( h m w l -- res )              \ normal & control chars
                over shift-pushkey
                0 ;M

\ :M WM_SYSCHAR   ( h m w l -- res )              \ alt chars come here
\                 over alt_mask or shift-pushkey  \ flag as an Alt-key
\                 DefWindowProc: [ self ]
\                 0 ;M

:M WM_SYSKEYDOWN   ( h m w l -- res )
                over VK_F10 =                   \ if its an F10,
                if      K_F10 vga-skey          \ then push key value
                        0                       \ and signal we handled it
                else    DefWindowProc: [ self ]
                then
                ;M

:M WM_KEYDOWN   ( h m w l -- res )
                over
        case    VK_F1      of  K_F1     vga-skey  endof
                VK_F2      of  K_F2     vga-skey  endof
                VK_F3      of  K_F3     vga-skey  endof
                VK_F4      of  K_F4     vga-skey  endof
                VK_F5      of  K_F5     vga-skey  endof
                VK_F6      of  K_F6     vga-skey  endof
                VK_F7      of  K_F7     vga-skey  endof
                VK_F8      of  K_F8     vga-skey  endof
                VK_F9      of  K_F9     vga-skey  endof
                VK_F11     of  K_F11    vga-skey  endof
                VK_F12     of  K_F12    vga-skey  endof
                VK_HOME    of  K_HOME   vga-skey  endof
                VK_END     of  K_END    vga-skey  endof
                VK_INSERT  of  K_INSERT vga-skey  endof
                VK_DELETE  of  K_DELETE vga-skey  endof
                VK_LEFT    of  K_LEFT   vga-skey  endof
                VK_RIGHT   of  K_RIGHT  vga-skey  endof
                VK_UP      of  K_UP     vga-skey  endof
                VK_DOWN    of  K_DOWN   vga-skey  endof
                VK_SCROLL  of  K_SCROLL vga-skey  endof
                VK_PAUSE   of  K_PAUSE  vga-skey  endof
                VK_PRIOR   of  K_PGUP   vga-skey  endof
                VK_NEXT    of  K_PGDN   vga-skey  endof
        endcase
                0 ;M

: set-mousexy   ( l -- )
                word-split
                dup 0x8000 and if 0xFFFF0000 or then 0max to mousey
                dup 0x8000 and if 0xFFFF0000 or then 0max to mousex
                ;

: null-check    ( a1 -- a1 )
                ?win-error-enabled 0=
                if      dup 0=
                        if      drop ['] noop   \ convert null to NOOP
                                exit            \ and exit
                        then
                then
                dup 0= s" Attempt to execute a NULL function" ?TerminateBox
                ;

:M OnWmCommand: ( hwnd msg wparam lparam -- hwnd msg wparam lparam )
                dup 0=
                if      over LOWORD
                        CurrentMenu
                        if      dup DoMenu: CurrentMenu
                        then
                        CurrentPopup
                        if      dup DoMenu: CurrentPopup
                        then    drop
                then    ;M

:M WM_COMMAND   ( hwnd msg wparam lparam -- res )
                OnWmCommand: [ self ]
                0 ;M

:M WM_RBUTTONDOWN ( h m w l -- res )
                set-mousexy
                CurrentPopup
                if      rot >r
                        mousex mousey r> Track: CurrentPopup
                then
                0 ;M

:M WM_LBUTTONDOWN ( h m w l -- res )
                set-mousexy
                clicking? 0=
                if      TRUE to clicking?
                        click-func null-check execute
                then
                0 ;M

:M WM_LBUTTONDBLCLK ( h m w l -- res )
                set-mousexy
                dbl-click-func null-check execute
                0 ;M

:M WM_LBUTTONUP ( h m w l -- res )
                set-mousexy
                clicking?
                if      unclick-func null-check CATCH
                        FALSE to clicking?
                        THROW
                then        
                0 ;M

:M WM_MOUSEMOVE ( h m w l -- res )
                set-mousexy
                MK_LBUTTON   and        \ left mouse button is down
                tracking? 0= and        \ and we aren't already tracking
                if      TRUE to tracking?
                        track-func null-check CATCH
                        FALSE to tracking?      \ must reset tracking flag
                        THROW
                then
                0 ;M

:M SetClickFunc: ( cfa -- )
                to click-func
                ;M

:M SetDblClickFunc: ( cfa -- )
                to dbl-click-func
                ;M

:M SetTrackFunc: ( cfa -- )
                to track-func
                ;M

:M SetUnClickFunc: ( cfa -- )
                to unclick-func
                ;M

\ If you define an application specific window class or window object
\ that redefines the method Win32Forth: to perform its own function
\ rather than just doing a beep, then your window will be able to handle
\ interprocess messages.

:M Win32Forth:  ( h m w l -- )
                forth-msg-chain do-chain
                ;M

:M DefWindowProc: ( h m w l -- res )
                2 pick WM_WIN32FORTH =
                if
                        Win32Forth: [ self ]

                else    DefaultWindowProc
                then
                ;M

;CLASS

: find-window   ( z"a1 -- hWnd )
                0 swap rel>abs Call FindWindow ;

: send-window   ( lParam wParam Message_ID hWnd -- )
                Call SendMessage drop ;


