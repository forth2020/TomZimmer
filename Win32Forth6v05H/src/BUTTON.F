\ button.f

cr .( Loading Button and ToolBar...)

125 value defbwidth     \ should be an ODD number for things to work well
 25 value defbheight

INTERNAL        \ definitions accessible while defining a buttonbar

:Class Button           <super Control

int bprev
int bfunc
int bheight
int bwidth

:M ClassInit:   ( -- )
                ClassInit: super
                s" Button" binfo place
                defbwidth  to bwidth
                defbheight to bheight
                ;M

:M SetPrev:     ( bprev -- ) to bprev ;M
:M GetPrev:     ( -- bprev ) bprev ;M

:M GetBwidth:   ( -- button_width )
                bwidth
                ;M

:M SetBwidth:   ( button_width -- )
                to bwidth
                ;M

:M GetBheight:  ( -- button_height )
                bheight
                ;M

:M SetBheight:  ( button_height -- )
                to bheight
                ;M

:M WindowStyle: ( -- style )
                WindowStyle: SUPER
                BS_PUSHBUTTON   or ;M

:M Start:       ( parent -- )
                to parent
                z" BUTTON" create-control
                ;M

:M amForground?: ( -- f1 )
                Call GetForegroundWindow
                GetHandle: [ GetParent: parent ] =      \ foreground window?
                ;M

:M RemoveFocus: ( -- f1 )
                TRUE
                ;M

:M DoButton:    ( -- )
                clear-info
                hWnd get-mouse-xy hWnd in-button?
                if      bfunc execute
                then
                ;M

:M On_LButtonUp: ( h m w l -- )
                old-wndproc CallWindowProc drop
                 ;M

:M WM_LBUTTONUP ( h m w l -- res )
                FALSE to mouse-is-down?
                On_LButtonUp: [ self ]
                0 ;M

:M WM_LBUTTONDOWN ( h m w l -- res )
                TRUE to mouse-is-down?
                old-wndproc CallWindowProc
                 ;M

;Class

EXTERNAL        \ always user accessible definitions

  0 value BuildBar

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\       Define a Generic Push Button Bar class
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

:CLASS Generic-ButtonBar  <Super Child-Window

\ -------------------- Instance Variables --------------------

int hbb         \ Handle to the button bar
int hb          \ handle to current button
int bid         \ button id counter
int bcnt        \ count of buttons in bar

int floatBar

\ -------------------- Methods --------------------

:M ClassInit:   ( -- )
                ClassInit: super
                self to BuildBar
                  0 to hbb
                  0 to hb
                100 to bid
                  0 to bcnt
                  0 to floatBar
\in-system-ok   also hidden
                ;M

:M GetBar:      ( -- hbb )  hbb  ;M
:M PutBar:      ( hbb -- )  to hbb  ;M

:M GetBCnt:     ( -- bcnt ) bcnt ;M
:M PutBCnt:     ( bcnt -- ) to bcnt ;M

:M NextBid:      ( -- bid )
                bid dup 1+ to bid ;M

:M SetButtonWidth:  ( width --  )       to defbwidth ;M
:M SetButtonHeight: ( height -- )       to defbheight ;M

:M On_Button:   { bidl \ hbl -- }
                hbb to hbl                   \ init the chain
                begin   hbl
                while   bidl GetID: hbl =
                        if      RemoveFocus: hbl
                                IF      SetFocus: parent
                                THEN
                                DoButton: hbl
                                0 to hbl
                        else    GetPrev: hbl to hbl
                        then
                repeat
                ;M

:M OnWmCommand: ( hwnd msg wparam lparam -- hwnd msg wparam lparam )
                OnWmCommand: Super
                over LOWORD ( ID ) On_Button: self
                ;M

:M On_Init:     { \ hbl -- }         \ initialize the class
                On_Init: Super
                hbb to hbl                      \ init the chain
                begin   hbl
                while   self                    \ the window handle
                        Start: hbl              \ start the button
                        GetPrev: hbl to hbl     \ select the next button
                repeat  drop
                ;M

:M On_Done:     { \ hbl -- }         \ initialize the class
                hbb to hbl                      \ init the chain
                begin   hbl
                while   self                    \ the window handle
                        Close: hbl              \ start the button
                        GetPrev: hbl to hbl     \ select the next button
                repeat  drop
                On_Done: Super
                ;M


:M WM_LBUTTONUP ( h m w l -- res )
                SetFocus: parent
                WM_LBUTTONUP WM: Super
                ;M

:M WindowStyle: ( -- style )            \ return the window style
                floatBar
                IF
                        WS_OVERLAPPEDWINDOW
                ELSE    WindowStyle: super
                THEN
                ;M

:M Start:       ( parent -- )
                floatBar
                IF      drop
                        register-frame-window drop
                        create-frame-window to hWnd
                        SW_SHOWNORMAL Show: self
                        Update: self
                ELSE    Start: Super
                THEN
                ;M

;CLASS


\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\       Define a Vertical Push Button Bar
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

:CLASS VButtonBar       <Super  Generic-ButtonBar       \ RIGHT VERTICAL


int bvoffset    \ Vertical offset


:M StartSize:   ( -- width height )     \ starting window size
                hbb to hb                       \ init the chain
                0
                begin   hb
                while   GetBwidth: hb max
                        GetPrev: hb to hb
                repeat  4 +                     \ total height of buttons
                hbb to hb                       \ init the chain
                0
                begin   hb
                while   GetBheight: hb 2 + +
                        GetPrev: hb to hb
                repeat  2 +                     \ total height of buttons
                ;M

:M StartPos:    ( -- x y )              \ Starting Position
                GetSize: parent >r
                StartSize: self drop - r>
                ;M

: button+v      ( -- n1 )
                bvoffset
                dup
                GetBheight: hb + 2 + to bvoffset ;

:M On_Size:     ( -- )                  \ handle resize message
                hbb to hb                       \ init the chain
                2 to bvoffset
                begin   hb
                while   2 button+v GetBwidth: hb GetBheight: hb Move: hb
                        GetPrev: hb to hb
                repeat
                ;M

;CLASS

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\       Define a Horizontal Push Button Bar
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

:CLASS BHButtonBar      <Super  Generic-ButtonBar       \ BOTTOM HORIZONTAL


int bhoffset    \ horizontal offset

:M StartSize:   ( -- width height )     \ starting window size
                hbb to hb                       \ init the chain
                0
                begin   hb
                while   GetBwidth: hb 2 + +
                        GetPrev: hb to hb
                repeat  2 +                     \ total width of buttons
                hbb to hb                       \ init the chain
                0
                begin   hb
                while   GetBheight: hb max
                        GetPrev: hb to hb
                repeat  4 +                     \ tallest button controls height
                ;M

:M StartPos:    ( -- x y )              \ Starting Position
                GetSize: parent
                StartSize: self nip -
                ;M

: button+h      ( -- n1 )
                bhoffset
                dup
                GetBwidth: hb + 2 + to bhoffset ;

:M On_Size:     ( -- )                  \ handle resize message
                hbb to hb                       \ init the chain
                2 to bhoffset
                begin   hb
                while   button+h 2
                        GetBwidth: hb GetBheight: hb Move: hb
                        GetPrev: hb to hb
                repeat
                ;M

;CLASS


:CLASS HButtonBar       <Super  Generic-ButtonBar       \ TOP HORIZONTAL


int bhoffset    \ horizontal offset

:M StartSize:   ( -- width height )     \ starting window size
                hbb to hb                       \ init the chain
                0
                begin   hb
                while   GetBwidth: hb 2 + +
                        GetPrev: hb to hb
                repeat  2 +                     \ total width of buttons
                hbb to hb                       \ init the chain
                0
                begin   hb
                while   GetBheight: hb max
                        GetPrev: hb to hb
                repeat  4 +                     \ highest button
                ;M

:M StartPos:    ( -- x y )              \ Starting Position
                0 0
                ;M

: button+h      ( -- n1 )
                bhoffset
                dup
                GetBwidth: hb + 2 + to bhoffset ;

:M On_Size:     ( -- )                  \ handle resize message
                hbb to hb                       \ init the chain
                2 to bhoffset
                begin   hb
                while   button+h 2
                        GetBwidth: hb GetBheight: hb Move: hb
                        GetPrev: hb to hb
                repeat
                ;M

;CLASS


\ HButtonBar Chro-buttons
\
\       50 SetButtonWidth: Chro-buttons
\
\       'X' +k_control AddKeyButton  EXIT "Analyze"
\       ' function AddFuncButton FUNC "myFunc"

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\       Define a Lower Right Push Button Bar
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

:CLASS LRButtonBar      <Super  Generic-ButtonBar       \ LOWER RIGHT

:M ClassInit:   ( -- )
                ClassInit: super
                ;M

:M StartSize:   ( -- width height )     \ starting window size
                hbb to hb               \ init the chain
                0
                begin   hb
                while        GetOrigin: hb drop
                             GetBWidth: hb + max
                               GetPrev: hb to hb
                repeat  4 +             \ total height of buttons
                hbb to hb               \ init the chain
                0
                begin   hb
                while        GetOrigin: hb nip
                            GetBHeight: hb + max
                               GetPrev: hb to hb
                repeat  2 +             \ total height of buttons
                ;M

:M StartPos:    ( -- x y )              \ Starting Position
                StartSize: self         \ my size
                GetSize: parent         \ parents size
                rot - >r - r>           \ put me in lower right hand corner
                ;M

:M On_Size:     ( -- )                  \ handle resize message
                hbb to hb               \ init the chain
                begin   hb
                while        GetOrigin: hb
                             GetBWidth: hb
                            GetBHeight: hb
                                  Move: hb
                               GetPrev: hb to hb
                repeat
                ;M

;CLASS

INTERNAL        \ definitions accessible while defining a buttonbar

\ HButtonBar Chro-buttons
\
\       AddButton "myFunc"   ... forth code ... ;

|CLASS AddButton        <Super Button

:M ClassInit:   ( -- )
                ClassInit: super
                GetBar: BuildBar to bprev       \ end of link is NULL
                bprev 0=                        \ if i'm the first one
                if      self PutBar: BuildBar   \ put me in the bar
                else    begin   bprev
                        while   bprev to id            \ save here temp
                                GetPrev: bprev to bprev
                        repeat  self SetPrev: id       \ temp use
                        0 SetPrev: self
                then
                NextBid: BuildBar to id
                here to title ,"text"
                GetBCnt: BuildBar 1+ PutBCnt: BuildBar      \ bump count
                here to bfunc
                hide !csp docol , ]
                ;M


;Class


\       LRButtonBar BARNAME
\       x y width height AddXYButton "myFunc"  ... forth code ... ;

-1 constant 1/1
-3 constant 1/4
-4 constant 2/4
-5 constant 3/4
-6 constant 4/4
-7 constant 1/2
-8 constant 2/2

|CLASS AddXYButton    <Super Button

int bx
int by

:M GetOrigin:   ( -- x y )
                bx by
                ;M

:M ClassInit:   ( x y width height -- )
                ClassInit: super
                to bheight
                dup -1 = if drop defbwidth                      then
                dup -2 = if drop defbwidth 2/                   then
                dup -3 = if drop defbwidth 2/ 2/ 1-             then
                dup -4 = if drop defbwidth 2/                   then
                dup -5 = if drop defbwidth 2/ dup 2/ +          then
                dup -6 = if drop defbwidth                      then
                dup -7 = if drop defbwidth 2/                   then
                dup -8 = if drop defbwidth 2/                   then
                0max to bwidth
                to by
                dup -1 = if drop 2                              then
                dup -2 = if drop defbwidth 2/ 3 +               then
                dup -3 = if drop 2                              then
                dup -4 = if drop defbwidth 2/ 2/ 3 +            then
                dup -5 = if drop defbwidth 2/    3 +            then
                dup -6 = if drop defbwidth 2/ dup 2/ + 3 +      then
                dup -7 = if drop 2                              then
                dup -8 = if drop defbwidth 2/ 3 +               then
                0max to bx
                GetBar: BuildBar to bprev       \ end of link is NULL
                bprev 0=                        \ if i'm the first one
                if      self PutBar: BuildBar   \ put me in the bar
                else    begin   bprev
                        while   bprev to id            \ save here temp
                                GetPrev: bprev to bprev
                        repeat  self SetPrev: id       \ temp use
                        0 SetPrev: self
                then
                NextBid: BuildBar to id
                here to title ,"text"
                GetBCnt: BuildBar 1+ PutBCnt: BuildBar      \ bump count
                here to bfunc
                hide !csp docol , ]
                ;M

;Class

\ Usage:  <pixels> HSpace
\ Spaces over <pixels> on the toolbar.
\ using a value of -1 for <pixels> will split the
\ toolbar and start another line

|CLASS HSpace   <Super Button

:M ClassInit:   ( n1 -- )
                ClassInit: super
                to bwidth
                GetBar: BuildBar to bprev       \ end of link is NULL
                bprev 0=                        \ if i'm the first one
                if      self PutBar: BuildBar   \ put me in the bar
                else    begin   bprev
                        while   bprev to id            \ save here temp
                                GetPrev: bprev to bprev
                        repeat  self SetPrev: id       \ temp use
                        0 SetPrev: self
                then
                NextBid: BuildBar to id
                0 to title
                GetBCnt: BuildBar 1+ PutBCnt: BuildBar      \ bump count
                ['] noop to bfunc
                ;M


:M Start:       ( parent -- )
                to parent
                ;M

:M Move:        ( x y x y -- )
                4drop
                ;M

;Class

\ BitMap data structures for use with bit mapped tool bars

 create BMPheader
         here                            nostack1
         0 w,    \ bftype        +0
         0 ,     \ bfsize        +2
         0 w,    \ reserved      +6
         0 w,    \ reserved      +8
         0 ,     \ bfOffBits     +10
         here swap - constant sizeof(BMPheader)

 create BMPinfoheader
         here                            nostack1
         0 ,     \ biSize        +0
         0 ,     \ biWidth       +4
         0 ,     \ biHeight      +8
         0 w,    \ biPlanes      +12
         0 w,    \ biBitCount    +14
         0 ,     \ biCompression
         0 ,     \ biSizeImage
         0 ,     \ biXPelsPerMeter
         0 ,     \ biYPelsPerMeter
         0 ,     \ biClrUsed
         0 ,     \ biClrImportant
         here over - swap !

 BMPinfoheader @ constant sizeof(BMPinfoheader)

 create BMPrect
         0 ,     \ left
         0 ,     \ top
         0 ,     \ right
         0 ,     \ bottom

 4 constant sizeof(RGBQUAD)

 24 constant sizeof(BMPbitmap)
             create BMPbitmap sizeof(BMPbitmap) allot

EXTERNAL

23 value DefToolHeight
24 value DefToolWidth
-1 value DefToolSpacing         \ one pixel overlap between buttons

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\       Define the Button Bar for Exec
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

:Class ToolBar  <Super  Generic-ButtonBar

int bhoffset                            \ horizontal offset
int bvoffset                            \ vertical   offset
int toolwidth
int toolheight
int tool-spacing                        \ space between buttons
int bar-name
int started?
int bmp-adr
int bmp-len
int bmp-off
int picture-bitmap
int prev-bitmap

Windc picture-dc
Rectangle tempRect

 3 constant border-width
 3 constant vertical-offset
 0 constant horizontal-offset

:M ClassInit:   ( -<toolbarname>- )     \ follow by .BMP toolbar filename
                ClassInit: super
                DefToolWidth   to toolwidth
                DefToolHeight  to toolheight
                DefToolSpacing to tool-spacing
                here to bar-name ,"text"
                FALSE to started?
                0 to picture-bitmap
                0 to prev-bitmap
                0 to bmp-adr
                0 to bmp-len
                0 to bmp-off
                bar-name c@
                if      bar-name count r/o open-file abort" Couldn't open the ToolBar bitmaps"
                        >r
                        here               to bmp-adr           \ set the bmp address
                        r@ file-size 2drop to bmp-len           \ set the bmp length
                        bmp-len allot                           \ allocate the space
                        bmp-adr bmp-len r@ read-file 2drop      \ read the bmp file
                        r> close-file drop                      \ close file
                then
                ;M

: read-bmp      ( a1 n1 -- )
                >r
                bmp-adr bmp-len bmp-off /string >r swap
                r> r> min dup>r move
                r> +to bmp-off ;

:M GetWinDC:    ( -- dc )
                GetHandle: picture-dc
                ;M

:M StartSize:   { \ Brows -- width height }     \ starting window size
                started?
        if      1 to Brows
                hbb to hb                       \ init the chain
                0
                begin   hb
                while   GetBwidth: hb 0< 0=     \ skip any next row markers
                        if      GetBwidth: hb + tool-spacing +
                        else    1 +to Brows
                                horizontal-offset 4 + + >r 0
                                                \ save toolbar width, start next row
                        then
                        GetPrev: hb to hb
                repeat  horizontal-offset 4 + + \ last row width remains on stack
                Brows 1- 0max                   \ if multiple rows, recover each row
                begin   ?dup                    \ width, and find the longest one
                while   swap r> max swap
                        1-
                repeat                             \ total width of buttons
                toolheight vertical-offset + Brows *
                           vertical-offset +       \ total height
                border-width 2* border-width 2* D+ \ compensate for dialog border width
                floatBar
                IF      20 +
                THEN
        else    0 0
        then
                ;M

:M WindowStyle: ( -- style )            \ return the window style
                WindowStyle: Super
                floatBar 0=
                IF      WS_DLGFRAME or  \ give this child a dialog frame border
                THEN
                ;M

:M ExWindowStyle: ( -- extended_style )
                ExWindowStyle: Super
                WS_EX_TOOLWINDOW or
                ;M

: button+h      ( -- n1 )
                bhoffset
                dup
                GetBwidth: hb 0>
                IF      GetBwidth: hb + tool-spacing +
                THEN    to bhoffset ;

:M On_Size:     ( -- )                          \ handle resize message
                started?
        if      hbb to hb                       \ init the chain
                horizontal-offset 2 + to bhoffset
                0 to bvoffset
                begin   hb
                while   GetBwidth: hb 0<
                        if      horizontal-offset 2 + to bhoffset          \ to left end
                                toolheight  vertical-offset + +to bvoffset \ next row
                        else    button+h    vertical-offset bvoffset +
                                GetBwidth: hb GetBheight: hb Move: hb
                        then
                        GetPrev: hb to hb
                repeat  
        then
                ;M

:M On_Paint:    ( -- )
                LTGRAY_BRUSH Call GetStockObject
                0 0
                StartSize: self 1+ SetRect: tempRect
                tempRect.AddrOf rel>abs GetHandle: dc call FillRect ?win-error
                EraseRect: tempRect
                StartPos: self  StartSize: self   Move: self
                ;M

:M On_Done:       ( -- )
                On_Done: super
                FALSE to started? 
                ;M

:M Start:       { theParent \ hmem1 hmem2 lpbmi lpvBits hdcMem hbm -- }
                GetDC: self  PutHandle: dc
                0 call CreateCompatibleDC PutHandle: picture-dc
                640 32 CreateCompatibleBitmap: dc to picture-bitmap
                picture-bitmap  SelectObject: picture-dc to prev-bitmap
                GetHandle: dc  ReleaseDC: self
                theParent Start: super          \ first start the bar
                                                \ then load the bitmap
                bar-name c@
        if      0 to bmp-off                    \ reset to start of bmp file
                BMPheader                                        \ BMP header dest
                sizeof(BMPheader)                                \ size of BMP header
                read-bmp                                         \ read header

                BMPinfoheader                                    \ dest for header info
                sizeof(BMPinfoheader)                            \ size of info header
                read-bmp                                         \ read info

                1 BMPinfoheader 14 + w@ lshift sizeof(RGBQUAD) * \ size of image
                sizeof(BMPinfoheader) + GHND
                call GlobalAlloc to hmem1                        \ allocate it
                hmem1 call GlobalLock abs>rel to lpbmi           \ lock it
                BMPinfoheader  lpbmi sizeof(BMPinfoheader) move  \ copy info up
                lpbmi sizeof(BMPinfoheader) +                    \ color dest
                1 BMPinfoheader 14 + w@ lshift                   \ size of image
                sizeof(RGBQUAD) *                                \ times quads
                read-bmp                                         \ read colors

                BMPheader dup 2 + @ swap 10 + @ -                \ bit map size
                GHND call GlobalAlloc to hmem2                   \ alloc mem
                hmem2 call GlobalLock abs>rel to lpvBits         \ lock mem
                lpvBits                                          \ dest of bits
                BMPheader dup 2 + @ swap 10 + @ -                \ bit map size
                read-bmp

                DIB_RGB_COLORS
                lpbmi   rel>abs
                lpvBits rel>abs
                CBM_INIT
                BMPinfoheader rel>abs                            \ info pointer
                GetHandle: picture-dc                            \ device context
                call CreateDIBitmap to hbm
                hmem1 call GlobalUnlock 0= ?win-error
                hmem2 call GlobalUnlock 0= ?win-error
                hmem1 call GlobalFree   0= ?win-error
                0 to hmem1
                hmem2 call GlobalFree   0= ?win-error
                0 to hmem2

                GetHandle: picture-dc call CreateCompatibleDC to hdcMem
                hbm hdcMem call SelectObject drop
                BMPbitmap rel>abs sizeof(BMPbitmap) hbm call GetObject drop

                SRCCOPY 0 0
                hdcMem
                BMPbitmap 8 + @                             \ image height
                BMPbitmap 4 + @                             \ image width
                0 0
                GetHandle: picture-dc
                call BitBlt ?win-error
                hdcMem call DeleteDC ?win-error
                hbm call DeleteObject ?win-error
                TRUE to started?
                0 0 SetOrigin: self     \ toolbar at top left corner of window
        then    ;M

:M Close:       ( -- )
                GetHandle: picture-dc
                if                                      \ restore original bitmap
                        prev-bitmap SelectObject: picture-dc drop
                                                        \ release DC if allocated
                        GetHandle: picture-dc Call DeleteDC ?win-error
                        0 PutHandle: picture-dc         \ clear the WinDC variable
                then
                picture-bitmap
                if      picture-bitmap call DeleteObject drop
                        0 to picture-bitmap
                then
                Close: Super
                ;M

;Class


|Class PictureButton    <Super Button

int picture-number

:M ClassInit:   ( n1 -- )
                to picture-number
                ClassInit: super
                DefToolWidth  to bwidth
                DefToolHeight to bheight
                GetBar: BuildBar to bprev               \ end of link is NULL
                bprev 0=                                \ if i'm the first one
                if      self PutBar: BuildBar           \ put me in the bar
                else    begin   bprev
                        while   bprev to id             \ save here temp
                                GetPrev: bprev to bprev
                        repeat  self SetPrev: id        \ temp use
                        0 SetPrev: self
                then
                NextBid: BuildBar to id
                here to title 0 ,
                GetBCnt: BuildBar 1+ PutBCnt: BuildBar  \ bump count
                here to bfunc
                hide !csp docol , ]
                ;M

WinDC dc        \ The window's device context

16 cells bytes &ps

:M WM_PAINT     ( hwnd msg wparam lparam -- res )
                picture-number 0 >=
                IF      &ps BeginPaint: self  PutHandle: dc
                        SRCCOPY                          \ copy mode
                        picture-number DefToolWidth 1- * \ offset to desired icon
                        0                                \ source y
                        GetWinDC: parent                 \ source handle
                        bwidth bheight                   \ width, height
                        0 0                              \ destination
                        BitBlt: dc
                        &ps EndPaint: self
                THEN
                0 ;M

:M WM_LBUTTONDOWN ( h m w l -- res )
                WM_LBUTTONDOWN WM: Super
                picture-number 0 >=
                IF      GetDC: self  PutHandle: dc
                        SRCCOPY                          \ copy mode
                        picture-number DefToolWidth 1- * \ offset to desired icon
                        0                                \ source y
                        GetWinDC: parent                 \ source handle
                        bwidth  3 - bheight 3 -          \ width, height
                        1 1                              \ destination
                        BitBlt: dc
                        GetHandle: dc ReleaseDC: self
                THEN
                 ;M

;Class

\ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
\ EditField is used on a toolbar as shown here:
\ <fieldwidth> EditField <fieldname>
\
\ <fieldname> is used to set the edit fields text contents in the form:
\ s" new text"        SetText: <fieldname>
\ ' nyWmChar        SetWmChar: <fieldname>      \ set WM_CHAR filtering
\ ' nyWmKeyDown  SetWmKeyDown: <fieldname>      \ set WM_KEYDOWN filtering
\ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

:Class EditField <Super EditControl

int bprev
int editwidth
int editheight

:M SetPrev:     ( bprev -- ) to bprev ;M
:M GetPrev:     ( -- bprev ) bprev ;M

:M GetBwidth:   ( -- edit_width )
                editwidth
                ;M

:M GetBheight:  ( -- edit_height )
                editheight
                ;M 

:M StartSize:   ( width height )
                editwidth
                editheight
                ;M

:M ClassInit:   ( editWidth -- )
                ClassInit: super
                to editwidth
                DefToolHeight to editheight
                s" Edit" binfo place
                GetBar: BuildBar to bprev       \ end of link is NULL
                bprev 0=                        \ if i'm the first one
                if      self PutBar: BuildBar   \ put me in the bar
                else    begin   bprev
                        while   bprev to id            \ save here temp
                                GetPrev: bprev to bprev
                        repeat  self SetPrev: id       \ temp use
                        0 SetPrev: self
                then
                NextBid: BuildBar to id
                here to title 0 ,
                GetBCnt: BuildBar 1+ PutBCnt: BuildBar      \ bump count
                ;M

\ the toolbar must not remove the focus from the edit control
:M RemoveFocus: ( -- f1 )
                FALSE
                ;M

:M DoButton:    ( -- )          \ a NOOP needed by TOOLBAR
                ;M

;Class

\ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
\ ComboField is used on a toolbar as shown here:
\ <fieldwidth> ComboField <fieldname>
\
\ <fieldname> is used to set the combo fields text contents in the form:
\ s" new item"   InsertString: <fieldname>      \ insert items into list
\ ' nyWmChar        SetWmChar: <fieldname>      \ set WM_CHAR filtering
\ ' nyWmKeyDown  SetWmKeyDown: <fieldname>      \ set WM_KEYDOWN filtering
\ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

:Class ComboField <Super ComboControl

int bprev
int bheight
int combowidth
int comboheight

:M SetPrev:     ( bprev -- ) to bprev ;M
:M GetPrev:     ( -- bprev ) bprev ;M

:M GetBwidth:   ( -- edit_width )
                combowidth
                ;M

:M GetBheight:  ( -- edit_height )
                bheight
                ;M 

:M StartSize:   ( width height )
                combowidth
                comboheight       \ height of edit control when open
                ;M

:M SetHeight:   ( height -- )
                to comboheight
                ;M

:M ClassInit:   ( editWidth -- )
                ClassInit: super
                to combowidth
                200 to comboheight
                DefToolHeight to bheight
                s" Edit" binfo place
                GetBar: BuildBar to bprev       \ end of link is NULL
                bprev 0=                        \ if i'm the first one
                if      self PutBar: BuildBar   \ put me in the bar
                else    begin   bprev
                        while   bprev to id            \ save here temp
                                GetPrev: bprev to bprev
                        repeat  self SetPrev: id       \ temp use
                        0 SetPrev: self
                then
                NextBid: BuildBar to id
                here to title 0 ,
                GetBCnt: BuildBar 1+ PutBCnt: BuildBar      \ bump count
                ;M

\ the toolbar must not remove the focus from the edit control
:M RemoveFocus: ( -- f1 )
                FALSE
                ;M

:M DoButton:    ( -- )                  \ a NOOP needed by TOOLBAR
                ;M

;Class


