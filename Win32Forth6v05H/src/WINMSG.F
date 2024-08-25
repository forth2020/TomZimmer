\ WINMSG.F      Windows Message Window Class            by Tom Zimmer

cr .( Loading Message Window...)

:Class MSGWINDOW  <super window

int cols
int rows
int ontop?
int msgactive
int msg-string
int msg-length

:M Classinit:   ( -- )
                ClassInit: super
                s" One Moment Please!" to msg-length to msg-string
                50 to OriginX
                50 to OriginY
                0 to cols
                0 to rows
                0 to msgactive
                0 to ontop?
                ;M

:M OnTop:       ( f1 -- )       \ should window float on top?
                to ontop?
                ;M

:M GetActive:   ( -- f1 )       \ is the message window active
                msgactive
                ;M

:M Close:       ( -- )          \ close if its open
                msgactive
                if      Close: super
                        false to msgactive
                then
                ;M

:M Start:       ( -- )           \ create a new window object
                Close: self
                register-frame-window drop
                create-frame-window to hWnd
                SW_SHOWNOACTIVATE Show: self
                Update: self
                SetFocus: self
                true to msgactive
                ;M

:M WindowStyle: ( -- style )            \ return the window style
                WS_OVERLAPPED WS_CAPTION + WS_THICKFRAME + 
                ;M

:M ExWindowStyle: ( -- extended_style )
                ExWindowStyle: super
                WS_EX_DLGMODALFRAME or
                ontop?                          \ is this a modal message?
                if      WS_EX_TOPMOST or        \ if so, lock on top
                then
                ;M

:M WindowTitle: ( -- Zstring )
                Z" One Moment Please!"
                ;M

:M MessageText: ( a1 n1 -- )
                to msg-length
                to msg-string
                1 to rows
                msg-string msg-length
                begin   2dup 0x0D scan 2dup 2>r nip - nip cols max to cols
                        2r> dup
                while   rows 1+ to rows         \ bump row count
                        2 /string               \ and skip CRLF
                repeat  2drop
                ;M

:M On_Paint:    { \ vpos -- }
                25 to vpos
                msg-string msg-length
                begin   dup
                while   2dup 0x0D scan 2dup 2>r nip -
                        20 vpos 2swap TextOut: dc
                        2r> 2 /string
                        vpos 18 + to vpos
                repeat  2drop
                ;M

:M StartSize:   ( -- width height )             \ starting window size
                cols  9 * 10 + 200 max
                rows 16 * 50 +
                ;M


:M MinSize:     ( -- width height )
                StartSize: [ self ]
                ;M

:M MaxSize:     ( -- width height )
                StartSize: [ self ]
                ;M

:M Refresh:     ( -- )
                hWnd                            \ only if not
                if      StartPos: self  StartSize: self  Move: self
                        Paint: self
                then
                ;M

;Class

msgwindow msg-window

INTERNAL

: _message-off   ( -- )
                Close: msg-window ;

' _message-off is message-off

EXTERNAL

: message-on    ( -- )
                GetActive: msg-window
                if      Refresh: msg-window
                else      Start: msg-window
                then    ;

: message-origin ( x y -- )
                SetOrigin: msg-window ;

INTERNAL

: _"message     ( a1 n1 -- )            \ a floating non-modal message box
                message-off
                dup
                if      FALSE OnTop: msg-window
                        MessageText: msg-window
                              Start: msg-window
                else    2drop
                then    ;

' _"message is "message

: _"top-message ( a1 n1 -- )            \ a floating ON-TOP message box
                message-off
                dup
                if      TRUE  OnTop: msg-window
                        MessageText: msg-window
                              Start: msg-window
                else    2drop
                then    ;

' _"top-message is "top-message

EXTERNAL

: zmessage      ( z& -- )
                MAXCOUNTED 2dup 0 scan nip - "message ;

INTERNAL

((  InfoWindow implements a class of window that is used to display
    tooltip messages when the mouse if held over a button. InfoWindow is used
    inside CONTROL.F in class Control, to implement tooltips.  Info window is
    really just a simple unframed window that allow you to put up some text on
    the screen at a specified location.  The Close: method will take the window
    down.  The window size is automatically adjusted to the text that you put
    in the window, and handles strings that contain "\n" new line desigators.
))

:Object InfoWindow      <Super MSGWINDOW

Font msgFont
 int extentx
 int extenty

4 cells bytes &InfoRect

ColorObject TIPCOLOR

7 constant fwidth
9 constant fheight

:M ClassInit:   ( -- )
                ClassInit: super
                0 to extentx
                0 to extenty
                                fwidth  Width: msgFont
                               fheight Height: msgFont
                s" MS Sans Serif" SetFaceName: msgFont
                Color: LTYELLOW NewColor: TIPCOLOR
                ;M

:M On_Init:     ( -- )
                On_Init: super
                Create: msgFont
                ;M

:M On_Done:     ( -- )
                Delete: msgFont
                On_Done: super
                ;M

:M StartSize:   ( -- width height )             \ starting window size
                extentx extenty

                ;M

:M On_Paint:    {  \ vpos msgmax -- }
                SaveDC: dc
                Handle: msgFont SetFont: dc

                &InfoRect GetClientRect: self
                Brush: TIPCOLOR
                &InfoRect rel>abs GetHandle: dc call FillRect ?win-error

                TRANSPARENT SetBkMode: dc
                0 to vpos
                0 to msgmax
                0 to extentx
                msg-string msg-length
                begin   dup
                while   2dup 0x0D scan 2dup 2>r nip - 2r> 2swap
                        2dup GetTextExtent: dc >r 3 +
                        extentx max to extentx          \ new max width
                        0 vpos 2swap TextOut: dc
                        2 /string
                        r> vpos + to vpos
                        vpos 3 + to extenty
                repeat  2drop
                RestoreDC: dc
                ;M

:M WindowStyle: ( -- style )            \ return the window style
                WS_POPUPWINDOW
                ;M

:M ExWindowStyle: ( -- extended_style )
                0
                ;M

:M Start:       ( c"string" x y -- )
                rot count to msg-length to msg-string
                screen-size fheight 4 + - rot min -rot  \ clip vertical
                msg-string msg-length                   \ actual string
                2dup 0x0D scan nip - nip                \ len of first line
                fwidth * - min swap                     \ clip horizontal
                SetOrigin: self                         \ set window origin
                Close: self
                register-frame-window drop
                create-frame-window to hWnd
                SW_SHOWNOACTIVATE Show: self
                Update: self
                true to msgactive
                OriginX OriginY StartSize: self Move: self
                ;M

;Object

MODULE

