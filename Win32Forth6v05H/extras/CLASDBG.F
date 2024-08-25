\ clastest.f

create answer 0 c, 50 allot


\ -------------------- Test Dialog --------------------

:CLASS test-dialog              <SUPER dialog

fload testdlg.h
load-dialog template testdlg.res

:M Start:  ( parent -- f )
        template run-dialog ;M

:M On_Command:  ( hCtrl code ID -- f )
        case
          IDOK of

                answer 1+ 50 IDD_EDIT GetDlgItemText: self
                answer c!

                1 end-dialog
          endof

          IDCANCEL of
                0 end-dialog
          endof

          false swap ( default result )

        endcase ;M

;CLASS



\ -------------------- Child Windows --------------------

\ :Class hello-window  <super child-window
\
\
\ \ : border        ( -- )          \ draw the child windows border
\ \                                    2 2 moveto
\ \                           width  2 - 2 lineto
\ \                 width 2 - height 2 -   lineto
\ \                         2 height 2 -   lineto
\ \                                    2 2 lineto ;
\
\ :M On_Paint:    ( -- )
\ \         id 1 = if red else blue then
\ \         2 PS_SOLID create-pen
\ \         border
\ \         delete-pen
\
\         system-fixed-font
\         get-text-metrics
\         window-io
\
\         id 1 =
\         if      blue foreground
\                 1 1 at-xy ." Child Window # " id .
\                 red foreground
\                 1 2 at-xy ." Character height = " char-height .
\                 yellow foreground
\                 1 3 at-xy ." Character width  = " char-width .
\                 black foreground
\         else    1 1 at-xy ." Dialog answer = '" answer count type ." '"
\                 1 2 at-xy ." Window height = " height .
\                 1 3 at-xy ." Window width  = " width .
\         then
\
\         forth-io
\         ;M
\
\ ;Class

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\       Define the BIT-WINDOW window class
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

Windc vga-dc

: moveto        ( x y -- )
                MoveTo: vga-dc ;

: lineto        ( x y -- )
                LineTo: vga-dc ;

: line          ( x1 y1 x2 y2 -- )
                2swap
                MoveTo: vga-dc
                LineTo: vga-dc ;

: line-color    ( color -- )
                LineColor: vga-dc ;

:Class bit-window  <super child-window

int vga-bitmap
int old-pen

: create-vga-bitmap  ( -- )
                640 480 CreateCompatibleBitmap: dc
                to vga-bitmap
                vga-bitmap SelectObject: vga-dc drop ;

: delete-vga-bitmap  ( -- )
                vga-bitmap call DeleteObject drop
                0 to vga-bitmap ;

:M On_Paint:    ( -- )
\                On_Paint: super
                SRCCOPY 0 0 GetHandle: vga-dc GetSize: self 0 0 BitBlt: dc
                ;M

:M WM_CREATE
                get-dc
                0 call CreateCompatibleDC PutHandle: vga-dc
                create-vga-bitmap
                OEM_FIXED_FONT   SelectStockObject: vga-dc drop
                WHITE_PEN        SelectStockObject: vga-dc to old-pen
\               56 malloc dup>r  GetTextMetrics:    vga-dc r> release
                BLACK            SetBkColor:        vga-dc
                WHITE            SetTextColor:      vga-dc
                0 0 640 480 cyan FillArea:          vga-dc
                release-dc
                0 ;M

:M On_Done:     ( -- )
                delete-vga-bitmap
\                On_Done: super
                ;M

;Class


\ -------------------- Test Menu --------------------

\ 101 Constant IDM_FILE_OPEN
102 Constant IDM_FILE_EXIT
103 Constant IDM_HELP
\ 104 Constant IDM_FILE_NEW

: append  ( hmenu flags id string -- )
        rel>abs swap 2swap swap
        Call AppendMenu drop ;


\ -------------------- Frame Window --------------------

:Class debug-window  <super window

bit-window my-bit-window

button b1       101 constant ID_BUTTON1
button b2       102 constant ID_BUTTON2
button b3       103 constant ID_BUTTON3

test-dialog mydialog

int result      ( from run-dialog )

:M LoadMenu:    { \ hmenu popup1 -- hmenu }

                Call CreatePopupMenu to popup1
\               popup1 MF_STRING    IDM_FILE_NEW  Z" &New"  append
\               popup1 MF_STRING    IDM_FILE_OPEN Z" &Open" append
\               popup1 MF_SEPARATOR 0             0         append
                popup1 MF_STRING    IDM_FILE_EXIT Z" E&xit" append

                Call CreateMenu to hmenu
                hmenu MF_POPUP      popup1        Z" &File" append
                hmenu MF_STRING     IDM_HELP      Z" &Help" append
                hmenu           \ return the menu handle after we build it
                ;M

:M WindowTitle: ( -- Zstring )
                Z" Win32f Debugger"
                ;M

:M On_Init:     ( -- )          \ initialize the class
                On_Init: super                  \ first init supper class
                1    SetId: my-bit-window       \ then the child window
                self Start: my-bit-window       \ then startup child window
                self ID_BUTTON1 Z" Button" Start: b1    \ add buttons
                self ID_BUTTON2 Z" Dialog" Start: b2
                self ID_BUTTON3 Z" Exit"   Start: b3
                ;M

:M Refresh:     ( -- )

                Paint: my-bit-window
                ;M

70 constant bwidth
20 constant bheight

:M MinSize:     ( -- width height )     \ minimum window size
                300 275 ;M

:M StartSize:   ( -- width height )     \ starting window size
                640 bwidth + 10 + 480 ;M

:M StartPos:    ( -- left  top    )     \ starting window position
                200 30  ;M

:M On_Size:     ( -- )                  \ handle resize message
        0 0 Width bwidth - 10 - Height Move: my-bit-window
        5 >r
        Width bwidth - 5 - r> dup bheight + 10 + >r bwidth bheight Move: b1
        Width bwidth - 5 - r> dup bheight + 10 + >r bwidth bheight Move: b2
        Width bwidth - 5 - r> dup bheight + 10 + >r bwidth bheight Move: b3
        r>drop
        ;M

: message       ( -- )                  \ local message function
                Z" You Pressed the button, didn't you?"
                Z" A Message to you..."
                MB_OK
                MessageBox: self drop ;


: do-control    ( hCtl code ID -- )     \ handle window controls
                case    ID_BUTTON1 of  message                      endof
                        ID_BUTTON2 of
                                self Start: mydialog to result
                                result if ( Repaint: hello2 ) then  endof
                        ID_BUTTON3 of  close: self                  endof
                endcase 2drop ;

: not-implemented  ( zstr -- )
                Z" Feature Not Implemented"
                MB_OK MB_ICONSTOP or
                MessageBox: self drop ;

: unknown-menu-item  ( id -- )
                S" Unknown menu item: " temp$ place
                0 <# 0 hold #S #> temp$ +place
                temp$ 1+ not-implemented ;

: do-menu  ( ID -- )
                case
\               IDM_FILE_NEW  of  Z" Menu: File New"  not-implemented  endof
\               IDM_FILE_OPEN of  Z" Menu: File Open" not-implemented  endof
                IDM_FILE_EXIT of  Close: self                          endof
                IDM_HELP      of  Z" Menu: Help"      not-implemented  endof
                                  dup unknown-menu-item
                endcase ;

:M WM_COMMAND  ( hwnd msg wparam lparam -- res )
                ?dup
                if      over HIWORD ( notification code ) rot LOWORD ( ID )
                        do-control
                else    LOWORD do-menu
                then    0 ;M

;Class

debug-window debugw

: refresh       ( -- )
                key? ?exit      \ don't refresh screen if a key is waiting
                Refresh: debugw ;


