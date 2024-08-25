\ FILEDUMP.F    An example to use the scroll bar by Jih-tung Pai, 6/23/96

comment:
        It's a simple example to use the vertical scroll bar. The program is
based on WINBROWS.F. It can dump the file in HEX format just like DUMP does
to memory.

To use it--
1. type "fload filedump" to load the file
2. type "dump-file"

comment;

only forth also definitions hidden also forth

FileOpenDialog filedump "Dump File" "All Files|*.*|"

:object dump-window <super window

int screen-cols
int screen-rows

Font fdFont

int cur-first-line \ current first line position
0 value first-line#    \ first line number
200 value last-line#   \ last line number
last-line# 20 - value last-top-line#
create cur-filename max-path allot
16 value bytes/line
0 value file-len \ length of the whole file
0 value file-ptr \ address of the memory for file

:m home: ( -- )
        first-line# to cur-first-line paint: self ;m

: "open-file    ( a1 n1 -- )
                2dup r/o open-file 0=
                if      Home: self
                        StartPos: self
                        200 + swap 200 + swap message-origin
                        s" Reading Text File..." _"message
                        >r 127 min cur-filename place
                        \ release/allocate the text buffer
                        file-ptr ?dup if free drop then
                        r@ file-size 2drop to file-len
                        file-len bytes/line / 1+ bytes/line * dup
                        malloc to file-ptr
                        bytes/line - file-ptr + bytes/line erase  \ erase the etra memory
                        \ read the file into memory
                        file-ptr file-len r@ read-file drop
                        to file-len
                        r> close-file drop
                        message-off
                        0 to cur-first-line
                        file-len bytes/line / 1+ to last-line#
                        cur-filename count settitle: self
                else    drop 2drop
                then    ;

: load-file     ( -- )
                gethandle: self Start: filedump dup c@
                if      count "open-file
                else    drop abort" No file selected"
                then    ;

\ ' load-file is load-file-defer

:M On_Init:     ( -- )
                On_Init: super
                 8  Width: fdFont
                14 Height: fdFont
  s" Courier" SetFaceName: fdFont
                   Create: fdFont
                load-file
                ;M

:m on_size: ( w -- )
        width char-width
        / to screen-cols
        height char-height
        / to screen-rows
        last-line# screen-rows - 0max to last-top-line#
        \ set the vertical scroll limits
        false last-top-line# first-line# SB_VERT
        GetHandle: self Call SetScrollRange drop

        ;m

:m startpos: 0 0 ;m

:m startsize: 75 char-width *  20 char-height * ;m

create line-buf 80 allot

: H.N.str       ( n1 n2 -- adr len )    \ display n1 a s a hex number of n2 digits
                BASE @ >R HEX >R
                0 <# R> 0 ?DO # LOOP #>
                R> BASE ! ;

: dump-line ( i -- adr len )
        bytes/line *
        dup 6 h.n.str line-buf place
        spcs 2 line-buf +place
        file-ptr + dup dup bytes/line + swap
        ?do i c@ 2 h.n.str line-buf +place
                spcs 1 line-buf +place
        loop bytes/line line-buf +place
        line-buf count ;

:m on_paint: ( -- )
                SaveDC: dc                      \ save device context
                Handle: fdFont SetFont: dc      \ set the font to be used
                screen-rows 0
                do 0 char-height i *
                        i cur-first-line + dup last-line# >
                        if  drop spcs 80
                        else dump-line
                        then textout: dc
                loop
                RestoreDC: dc
                ;m

:M WindowStyle: ( -- style )            \ return the window style
                WindowStyle: super
                WS_VSCROLL or           \ add vertical scroll bar
                ;M
:m vposition: ( n -- ) \ move to position n
        0max last-top-line# min
        to cur-first-line paint: self ;m

:m vscroll: ( n -- ) \ move n lines up or down
        cur-first-line + vposition: self ;m

:m end: ( -- ) \ move to end, in this case it's 100 bytes down to pad
        last-top-line# to cur-first-line paint: self ;m

:m vpage: ( n -- )  \ down or up n pages
        screen-rows 1- * vscroll: self ;m

:M WM_VSCROLL   ( h m w l -- res )
                swap word-split >r
        CASE
                SB_BOTTOM        of          End: self   endof
                SB_TOP           of         Home: self   endof
                SB_LINEDOWN      of    1 VScroll: self   endof
                SB_LINEUP        of   -1 VScroll: self   endof
                SB_PAGEDOWN      of    1   VPage: self   endof
                SB_PAGEUP        of   -1   VPage: self   endof
                SB_THUMBPOSITION of r@ VPosition: self   endof
                SB_THUMBTRACK    of r@ VPosition: self   endof
        ENDCASE r>drop
        \ position the vertical button in the scroll bar
        TRUE cur-first-line SB_VERT
        GetHandle: self Call SetScrollPos drop

                0 ;M

:m on_done: ( -- )
        file-ptr ?dup
        if free drop
        then
        Delete: fdFont
        on_done: super ;m

;object

: dump-file ( -- )
        start: dump-window ;

\ ***** program end *****
