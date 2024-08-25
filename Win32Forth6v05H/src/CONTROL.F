\ $Id: control.fth 1.4 1994/03/21 09:56:05 andrew Exp $

cr .( Loading Control Window...)

\ child controls with subclassing

DEFER SUBCLASS-WNDPROC  ' NOOP IS SUBCLASS-WNDPROC

CFA-CODE SUBCLASS-RETURN
                HERE CELL+ APP-,
                HERE CELL+ APP-,
                mov     eax, ebx                \ C return value
                mov     esp, ebp                \ restore stack
                pop     ebp                     \ restore registers
                pop     ebx
                pop     edi
                pop     esi
                ret     # 4 CELLS               \ return & discard params
END-CODE

CFA-CODE SUBCLASS-ENTRY  ( lparam wparam message hwnd -- result )
                pop     eax                     \ thunk address+8
                push    esi                     \ save registers
                push    edi
                push    ebx
                push    ebp
                mov     ebx, eax                \ thunk address+8 to ebx
                mov     ebp, esp                \ make forth stacks
                sub     esp, # 4000             \ room for return stack
                push    5 CELLS [ebp]           \ hwnd
                push    6 CELLS [ebp]           \ message
                push    7 CELLS [ebp]           \ wparam
                push    8 CELLS [ebp]           \ lparam
                mov     edi, ebx
                sub     edi, 0 [edi]            \ edi has forth base address
                mov     edx, fs: 0x14           \ edx is now ptr from TIB pvArbitrary
                sub     ebx, # 12               \ base of object
                sub     ebx, edi                \ make relative address
                lea     esi, SUBCLASS-RETURN [edi]
                mov     eax, # ' SUBCLASS-WNDPROC
                EXEC
END-CODE

: CallWindowProc  ( hwnd msg wparam lparam wndproc -- result )
        >r swap 2swap swap r> Call CallWindowProc ;


\ -------------------- Control Class --------------------

INTERNAL        \ definitions accessible while defining a buttonbar

0 value &binfo                  \ pointer to latest defined button
     create &ButtonRect  4 cells allot  \ temp rectangle for current info msg
            &ButtonRect  4 cells erase
     create &CursorPoint 2 cells allot
            &CursorPoint 2 cells erase
255 constant max-binfo          \ longest info message allowed

: ButtonInfo"   ( -- )          \ set the info for latest button or control
                &binfo 0= abort" Must follow a button definition"
                '"' word count max-binfo min &binfo place
                &binfo count \n->crlf ;

EXTERNAL        \ definitions always accessible

TRUE  value info-flag                   \ are we displaying tool tips
FALSE value mouse-is-down?

: get-mouse-xy  { hWnd -- x y }
                &CursorPoint rel>abs      Call GetCursorPos   drop
                &CursorPoint rel>abs hWnd Call ScreenToClient drop
                &CursorPoint       @
                &CursorPoint cell+ @  ;

: in-button?    { x y hWnd -- f1 }
                &ButtonRect  rel>abs hWnd Call GetClientRect drop
                y &ButtonRect 1 cells+ @ \ 2 +   \ top
                  &ButtonRect 3 cells+ @ \ 2 -   \ bottom
                between
                x &ButtonRect          @ \ 2 +   \ left
                  &ButtonRect 2 cells+ @ \ 2 -   \ right
                between and ;

:Class Control          <Super Generic-Window

(( thunk:

        0       nop nop nop
        3       call control-wndproc
        8       dd      here
))

\ !!! THE FIRST THREE IVARS MUST BE IN THIS ORDER !!!

\ int hWnd              \ window handle (inherited from generic-window)
12 bytes thunk          \ thunk address for new window procedure
int old-wndproc         \ address of old window procedure

int parent              \ address of parent object
int id                  \ the control's ID
int title               \ the counted title string
int handleofparent      \ the frame window handle
int Horizontal
int Vertical
int timering?           \ are we opening a popup info window
int timerclosed?        \ has popup been closed
int auto-close?          \ does info window automatically close after a time?
max-binfo 1+ bytes binfo

32768 value unique-id#

: unique-id     ( -- id )       \ get a unique initial ID for this control
                unique-id#
                1 +to unique-id# ;

\ -------------------- Subclassed Window Procedure --------------------

: _subclass-WndProc   ( hwnd msg wparam lparam window -- res )
        [ also classes ]
        3 pick ( msg ) over obj>class MFA ((findm))

        if      sp0 @ >r sp@ 4 cells+ sp0 !
                dup>r catch
                ?dup
                if      r@ WndProcError
                then    r>drop
                r> sp0 !
        else
                4 cells+ @ ( old-wndproc ) CallWindowProc
        then
        [ previous ] ;

' _subclass-WndProc is subclass-WndProc

\ -------------------- SubClassing --------------------

: subclass  ( -- )
        0xE8909090 thunk !  ( call )
        subclass-entry thunk 2 cells+ -  thunk cell+ !  ( offset )
        thunk 2 cells+ dup !  ( relocate offset )
        thunk rel>abs GWL_WNDPROC hWnd Call SetWindowLong
        to old-wndproc ;


:M ClassInit:   ( -- )
                ClassInit: super
                0 to parent
                0 to handleofparent
                0 to old-wndproc
                unique-id to id
                z" " to title
                binfo off
                binfo to &binfo                 \ so we can set it later
                FALSE to timering?
                FALSE to timerclosed?
                TRUE  to auto-close?
                ;M

:M GetParent:   ( -- parent )
                parent
                ;M

:M GetHandleOfParent: ( -- handleofparent )
                handleofparent
                ;M

:M SetID:       ( id -- )
                to id
                ;M

:M GetID:       ( -- id )
                id
                ;M

:M ExWindowStyle: ( -- exstyle )
                0 ;M

:M WindowStyle: ( -- style )
                WS_CHILD
                WS_VISIBLE or ;M

:M StartSize:   ( -- width height )   0  0 ;M      \ override to change
:M StartPos:    ( -- left  top )      0  0 ;M      \ override to change

\ this pointer to a structure, depends on what kind of window you are
\ creating, so we just default it to NULL

:M CreateStruct: ( -- CreateStrucPointer )
                NULL
                ;M

: create-control  ( z"classname" -- )
                >r
                CreateStruct: [ self ]          \ override if needed
                appInst
                ID
                GetHandle: Parent dup to handleofparent
                StartSize:     [ self ] swap    \ height, width
                StartPos:      [ self ] swap    \ y, x starting position
                WindowStyle:   [ self ]         \ you can override these
                title 1+ rel>abs                \ the control's text
                r> rel>abs                      \ the class name zstring
                ExWindowStyle: [ self ]         \ the extended window style
                Call CreateWindowEx to hWnd
                hWnd
                if      subclass
                then
                ;

1 constant INFO_START_TIMER              \ timer constant
2 constant INFO_END_TIMER                \ timer constant
3 constant INFO_CLOSE_TIMER              \ timer constant

: clear-info    ( -- )
                INFO_START_TIMER hWnd Call KillTimer drop
                INFO_END_TIMER   hWnd Call KillTimer drop
                INFO_CLOSE_TIMER hWnd Call KillTimer drop
                FALSE to timering?
                FALSE to timerclosed?
                Close: InfoWindow ;

:M BInfo:       ( -- a1 )       \ return the counted string of button info
                binfo
                ;M

:M SetAutoClose: ( flag -- )    \ set the state of the automatic infor close feature
                to auto-close?
                ;M

:M GetAutoClose: ( -- flag )
                auto-close?
                ;M

:M WM_TIMER     ( h m w l -- res )
                hWnd get-mouse-xy to Vertical to Horizontal
                over INFO_START_TIMER =
                mouse-is-down? 0= and
                IF      INFO_START_TIMER hWnd Call KillTimer drop
                        Horizontal Vertical hWnd in-button?   \ if timer and still on button
                        timering? and
                        IF      1 to timering?
                                BInfo: [ self ]
                                &ButtonRect rel>abs hWnd Call GetWindowRect drop
                                &ButtonRect       @ Horizontal +
                                &ButtonRect cell+ @ Vertical   + 25 +
                                Start: InfoWindow
                                NULL  100 INFO_END_TIMER   hWnd Call SetTimer drop
                                GetAutoClose: [ self ]
                                IF      NULL 4000 INFO_CLOSE_TIMER hWnd Call SetTimer drop
                                THEN
                        ELSE    FALSE to timering?
                        THEN
                THEN
                over INFO_END_TIMER =
                IF      Horizontal Vertical hWnd in-button? 0=
                        IF      INFO_END_TIMER   hWnd Call KillTimer drop
                                INFO_CLOSE_TIMER hWnd Call KillTimer drop
                                FALSE to timering?
                                Close: InfoWindow
                                FALSE to timerclosed?
                        THEN
                THEN
                over INFO_CLOSE_TIMER =
                IF      INFO_CLOSE_TIMER hWnd Call KillTimer drop
                        FALSE to timering?
                        TRUE  to timerclosed?
                        Close: InfoWindow
                THEN
                0 ;M

:M amForground?: ( -- f1 )
                TRUE
                ;M

:M On_MouseMove: ( h m w -- )
                info-flag                       \ are we displaying tool tips?
                BInfo: [ self ] c@ and          \ and there is text to display
                IF      hWnd get-mouse-xy hWnd in-button?       \ in the button
                        amForground?: [ self ] and              \ and the forground window
                        mouse-is-down? 0= and                   \ and mouse isn't down
                        IF      timering? 1 <                   \ if timering? is 0 or -1
                                timerclosed? 0= and
                                IF
                                        TRUE to timering?
                                        NULL
                                        700 INFO_START_TIMER
                                        hWnd Call SetTimer drop
                                THEN
                        ELSE    clear-info
                        THEN
                THEN
                0 ;M

:M WM_MOUSEMOVE { h m w l -- res }
                l word-split to Vertical  to Horizontal
                h m w On_MouseMove: [ self ]
                old-WndProc
                IF      h m w l old-WndProc CallWindowProc
                THEN
                ;M

;Class

