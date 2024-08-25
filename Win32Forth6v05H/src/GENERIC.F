

\ $Id: generic.fth 1.3 1994/03/24 10:52:17 andrew Exp $
\ SetForegroundWindow: -- Fix by Jos Ven (added by rbs)

cr .( Loading Generic Window...)

((
Generic-Window is the base class for all window objects.  This class
contains a single ivar, hWnd, that is the (MS Windows) handle for the
window.  This class encapsulates all the Win32 API calls that specify a
window handle.  There will be 3 subclasses of Generic-Window:

        Window          Adds a device context and the ablility
                        to display text and graphics output.

        Control         Adds support for the standard Win32 controls
                        with subclassing.

        Dialog          Support for dialog boxes

))
                        

only forth also definitions  decimal

VARIABLE windows-link
         windows-link OFF

Rectangle wRect

:CLASS  Generic-Window    <Super Object


\ -------------------- Instance Variables --------------------

\ *** WARNING: DO NOT ADD ANY INSTANCE VARIABLES TO THIS CLASS! ***

\ HWND MUST BE THE FIRST AND ONLY IVAR.  THIS IS ESSENTIAL TO THE
\ WINDOW PROCEDURE OF CLASS WINDOW AND THE SUBCLASSING TECHNIQUE
\ USED BY CLASS CONTROL.

int hWnd        \ handle to Win32 window object

: static-window? ( -- f1 )      \ is this a static window
                self 0 sys-here between ;

\ -------------------- Window Methods --------------------

: trim-windows  ( nfa -- nfa )
\in-system-ok   dup windows-link full-trim ;

\in-system-ok forget-chain chain-add trim-windows

:M Classinit:   ( -- )
                ClassInit: super
                0 to hWnd
                static-window?  \ only link in static windows
                if      windows-link link,     \ link into list
                        self ,
                then
                ;M

:M GetHandle:   ( -- hWnd )             \ return window handle
                hWnd
                ;M

:M PutHandle:   ( hWnd -- )             \ store window handle
                to hWnd
                ;M

:M ZeroWindow:  ( -- )
                0 to hWnd
                ;M

:M DestroyWindow: ( -- )
                hWnd
                if      hWnd Call DestroyWindow ?win-error
                        0 to hWnd
                then
                ;M

:M Close:       ( -- )
                DestroyWindow: self
                ;M

:M Paint:       ( -- )   \ force window repaint
                hWnd
                if      1 0 hWnd Call InvalidateRect ?win-error
                then
                ;M

:M Show:        ( state -- )    \ use words like SW_SHOWNORMAL
                hWnd
                if      hWnd Call ShowWindow drop
                else    drop
                then
                ;M

:M GDIFlush:    ( -- )
                Call GdiFlush ?win-error
                ;M

:M Update:      ( -- )
                hWnd
                if      hWnd Call UpdateWindow ?win-error
                then
                ;M

:M Scroll:      ( x y -- )
                hWnd
                if      swap 0 0 2swap hWnd Call ScrollWindow drop
                else    2drop
                then
                ;M

:M Move:        ( x y w h -- )
                hWnd
                if      >r >r >r >r  1 ( repaint flag ) r> r> r> r>
                        swap 2swap swap hWnd Call MoveWindow ?win-error
                else    4drop
                then
                ;M

:M SetWindowPos: { x y -- }
                hWnd
                if      SWP_NOSIZE
                        SWP_SHOWWINDOW or
                        SWP_NOZORDER or
                        0 0             \ no size specified
                        y x
                        0               \ insert parameter not used
                        hWnd Call SetWindowPos ?win-error
\ April 27th, 1998 - 9:14 tjz removed, reported by Bruno Gauthier
\                else    2drop
                then
                ;M

:M GetWindowRect: ( -- left top right bottom )
                hWnd
                if      EraseRect: wRect
                        wRect.AddrOf rel>abs hWnd Call GetWindowRect ?win-error
                        wRect.Left   wRect.Top
                        wRect.Right  wRect.Bottom
                else    0 0 0 0
                then
                ;M

:M SetMenu:     ( menuhandle -- )
                hWnd
                if      hWnd Call SetMenu ?win-error
                else    drop
                then
                ;M

:M SetText:     { addr len \ text$ -- }
                hWnd
                if      MAXSTRING localAlloc: text$
                        addr len text$ place
                                 text$ +NULL
                        text$ 1+ rel>abs hWnd Call SetWindowText ?win-error
                then        
                ;M

:M SetTextAlign: ( flag -- )
                case    0 of    TA_LEFT         endof
                        1 of    TA_RIGHT        endof
                        2 of    TA_CENTER       endof
                                TA_LEFT swap
                endcase
                TA_UPDATECP or
                hWnd Call GetDC dup>r
                Call SetTextAlign drop
                r> hWnd Call ReleaseDC drop
                ;M

named-new$ gettext$

:M GetText:     ( -- addr len )
                hWnd
                if      MAXCOUNTED gettext$ rel>abs hWnd Call GetWindowText
                        gettext$ swap
                else    gettext$ 0
                then
                ;M


:M GetDC:       ( -- hdc )      hWnd Call GetDC ;M
:M GetWindowDC: ( -- hdc )      hWnd Call GetWindowDC ;M
:M ReleaseDC:   ( hdc -- )      hWnd Call ReleaseDC ?win-error ;M

:M BeginPaint:  ( ps -- hdc )   rel>abs hWnd Call BeginPaint ;M
:M EndPaint:    ( ps -- )       rel>abs hWnd Call EndPaint drop  ;M

:M GetClientRect:  ( rect -- )  rel>abs hWnd Call GetClientRect ?win-error ;M

:M GetWindowLong:  ( index -- value )         hWnd Call GetWindowLong ;M
:M SetWindowLong:  ( value index -- oldval )  hWnd Call SetWindowLong ;M

:M GetStyle: ( -- style )       GWL_STYLE GetWindowLong: self ;M
:M SetStyle: ( style -- )       GWL_STYLE SetWindowLong: self drop ;M
:M +Style:   ( style -- )       GetStyle: self OR SetStyle: self ;M
:M -Style:   ( style -- )       INVERT GetStyle: self AND SetStyle: self ;M

:M SetFocus:            ( -- )  hWnd Call SetFocus drop  ;M


\ :M SetForegroundWindow: ( -- )  hWnd Call SetForegroundWindow ?win-error  ;M
    \ - Fixed by Jos Ven (rbs)

:M SetForegroundWindow: ( -- )
    call GetActiveWindow  0 swap call GetWindowThreadProcessId
    hWnd 2dup  <>
        if    2dup 1 -rot Call AttachThreadInput drop
              rot Call SetForegroundWindow drop
              false -rot Call AttachThreadInput drop
        else  2drop
        then
                ;M


:M SetActiveWindow:     ( -- )  hWnd Call SetActiveWindow drop  ;M

:M MessageBox:  ( szText szTitle style -- result )
                MB_TASKMODAL or -rot rel>abs swap rel>abs
                ( hWnd ) NULL Call MessageBox
                ;M

:M InvalidateRect: ( bgflag rectangle -- )
                rel>abs hWnd call InvalidateRect ?win-error
                ;M


:M GetDlgItem:  ( id -- handle )
                hWnd Call GetDlgItem
                ;M

:M GetDlgItemText:  ( addr len id -- len )
                >r swap rel>abs r> hWnd Call GetDlgItemText
                ;M

:M SetDlgItemText:  ( addr len id -- )
                >r asciiz rel>abs r> hWnd Call SetDlgItemText drop
                ;M

:M SetDlgItemFocus: ( id -- )
                GetDlgItem: self
                Call SetFocus drop
                ;M

:M SelectDlgItemAll: ( id -- )
                >r -1 0 EM_SETSEL r> hWnd Call SendDlgItemMessage drop
                ;M


:M IsDlgButtonChecked: ( id -- f1 )
                hWnd Call IsDlgButtonChecked
                ;M

:M CheckDlgButton: ( u1 id -- )   \ 0=clear, 1=checked, 2=grayed
                hWnd Call CheckDlgButton drop
                ;M

:M SetDlgItemAlign: ( flag id -- )
                GetDlgItem: self dup>r
                Call GetDC >r
       ( flag ) case    0 of    TA_LEFT         endof
                        1 of    TA_RIGHT        endof
                        2 of    TA_CENTER       endof
                                TA_LEFT swap
                endcase TA_UPDATECP or
                r@ Call SetTextAlign drop
                r> r> Call ReleaseDC drop
                ;M

:M SetAlign:    ( flag id -- )          \ hold over from the past
                SetDlgItemAlign: self
                ;M

:M EnableDlgItem: ( flag id -- )  \ 0=disable, 1=enable
                GetDlgItem: self
                Call EnableWindow drop
                ;M

:M ShowDlgItem: ( flag id -- )    \ 0=hide item, 1=show item
                swap
                if      SW_SHOWNORMAL
                else    SW_HIDE
                then
                swap GetDlgItem: self
                Call ShowWindow drop
                ;M

\ control a range of radio buttons, clearing all, and checking one

:M CheckRadioButton: ( check_id first_id last_id -- )
                swap hWnd Call CheckRadioButton drop
                ;M

\ See SDK help for "Messages to List Boxes"

:M SendDlgItemMessage: ( lParam wParam message id -- long )
                hWnd Call SendDlgItemMessage
                ;M

:M SetDlgItemFont: ( FondObject id -- )
                1 -rot WM_SETFONT swap SendDlgItemMessage: self
                ;M

;CLASS


: zero-windows  { \ wlink -- }          \ startup the console's menubar
                windows-link @          \ clear all menu handles
                begin   dup
                while   dup cell+ @ to wlink
                        ZeroWindow: wlink
                        @
                repeat  drop ;

initialization-chain chain-add zero-windows


