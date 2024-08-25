\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ \\\\\\\\\\            Hex Dump Window Class               \\\\\\\\\\
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

:object   hexhelp <super helpmsg
:M WindowTitle: ( -- Zstring )
                Z" Hex Editor/Browser Key Usage" ;M
;object

create    hexhelptext
         z," F1 = help\n"
        +z," F2 = toggle Browse/Edit  (white = editable)\n"
        +z," F3 = next changed, shift = previous\n"
        +z," F4 = next memory page, shift = previous\n"
        +z," F5 = upload entire address range\n"
        +z," F6 = start of user memory\n"
        +z," F7 = start of specialty memory\n\n"
        +z," ^Z = clear persistence  (red --> black)\n"
        +z," ^C = copy to clipboard\n"
        +z," ESC = done\n"
        0 c,

:class hexwindow <super textwindow

int newdata     \ current data to display
int olddata     \ data displayed on last refresh
int colordata   \ color array for data-persist feature
int dumpsize    \ # of bytes displayable
int refcount    \ tally of screen updates from the target
int lowaddress  \ lower end of address range
int highaddress \ upper end of address range

:M addresswidth: ( -- n )    addrnibbles ;M



((
character spacing vs charnibbles  x=hex  a=ascii
                                         alloc  chars/line  asciiorg  asc/line
xx xx xx xx xx xx xx xx  aaaaaaaa          32       8           25       8
xxx xxx xxx xxx xxx xxx  aaaaaaaaaaaa      30       6           25      12
xxxx xxxx xxxx xxxx  aaaaaaaa              28       4           21       8
xxxxx xxxxx xxxxx xxxxx  aaaaaaaaaaaa      32       4           25      12
xxxxxx xxxxxx xxxxxx  aaaaaaaaa            30       3           22       9
xxxxxxx xxxxxxx xxxxxxx  aaaaaaaaaaaa      33       3           25      12
xxxxxxxx xxxxxxxx  aaaaaaaa                26       2           19       8
))

\ charnibbles:   0     1     2     3     4     5     6     7     8
create %alloc   32 c, 32 c, 32 c, 30 c, 28 c, 32 c, 30 c, 33 c, 26 c,
create %apl      8 c,  8 c,  8 c, 12 c,  8 c, 12 c,  9 c, 12 c,  8 c,
create %cpl      8 c,  8 c,  8 c,  6 c,  4 c,  4 c,  3 c,  3 c,  2 c,

int charnibbles         \ nibbles/character
int charbytes           \ bytes/character
int columnsalloc        \ columns to allocate
int chars/line
int bytes/line
int nibbles/line
int asciiorigin         \ 1st column in ascii mode

: initwidths    ( #bits -- )
\ initialize char sizes, 8 to 32 bits
                8 max 32 min
                dup 3 + 2 rshift to charnibbles
                    7 + 3 rshift to charbytes
                %cpl    charnibbles + c@ to chars/line
                %apl    charnibbles + c@ to bytes/line
                chars/line charnibbles * to nibbles/line
                nibbles/line chars/line + to asciiorigin
                asciiorigin bytes/line + to columnsalloc
                ;

int cursorposition

: bytepos       ( -- nibble# a )
                cursorposition nibbles/line /mod bytes/line *   ( col addr0 )
                >R charnibbles /mod charbytes * R> +            ( nibl# 'char )
                swap charnibbles 1 and + 2 /mod rot + ;         ( 0/1 a )

int addroffset  \ value to add to displayed address
int editing?    \ editing or just browsing?

:M write-to:    ( asrc adest n -- f )
\ write to external device, default = write to self
                newdata under+ move
                false ;M

:M read-from:   ( asrc adest n -- f )
\ read from external device, default = read from self
                rot newdata + -rot move
                false ;M

:M startaddr:   ( -- a )  lowaddress ;M       \ default start offset
:M specaddr:    ( -- a )  0 ;M


:M MaxStartSize: ( -- w h )  68 17 ;M

((
A hex dump window continuously updates the viewable region of text.
For each byte within the viewable region, a byte is read from the target
and dumped to the display buffer.  Unchanged bytes are different from
changed bytes in that changed bytes are drawn in a different color
and cause the corresponding text row to be marked for update.

At the end of a sweep, the display window is refreshed.
))

:M OnTop:       ( -- f )        true ;M

:M WindowStyle: ( -- style )
                WS_CAPTION
                WS_THICKFRAME   or
                WS_SYSMENU      or
                WS_MAXIMIZEBOX  or
                WS_MINIMIZEBOX  or
                ;M

:M TopMargin:   ( -- height )
                char-height 3 + ;M

: helptext      ( -- a n )      \ the top help line of the window
s" F1=?? F2=Edit F3=Seek F4=PgDn F5=Refresh" ;

:M ReBar:       ( -- )
\ used inside textwindow on_paint:
                ReBar: super            \ draw margin bars
                SaveDC: dc              \ Draw the help line
                Handle: txtFont SetFont: dc  \ set the font to be used
                TopMarginColor: [ self ]  SetBkColor: dc
                Gray                    SetTextColor: dc
                2 1 helptext TextOut: dc
                RestoreDC: dc
                ;M

: rowbounds     ( n -- aend abeg )
\ address bounds for row n
                bytes/line *
                bytes/line bounds ;

: viewablemem   (  -- a len )
\ address bounds for the viewable area
                rowoffset bytes/line *                \ startaddress
                maxrow 1+ bytes/line * dumpsize min ;

: dumprows      ( -- n )
\ number of rows in the entire array
                dumpsize bytes/line / ;

: datasame?     ( a -- f )
\ does the old and new data match?
                dup  newdata + c@
                swap olddata + c@  = ;

: backcolor     ( -- )
\ set the data background color
                editing?
                if      (white)
                else    (ltgray)
                then    >bg: self ;

: forecolor     ( a -- )
\ set the data foreground color
                colordata + c@
                >fg: self ;

variable baseaddr

: DumpLine      ( n -- )
\ dump line n to the text buffer, still need to send to screen later
\ use red foreground for changed bytes, black for non-changed,
\ blue for header data.  Background: ltgray = non-editable
                >r
                r@ bytes/line *  dup baseaddr !
                dup  newdata +
                swap olddata +
                bytes/line tuck compare         \ did this line change?
                if      r@ set-pending          \ mark for redraw if so
                then
                r@ rowbounds
                do      i datasame? 0=
                        if      (ltred)
                                i colordata + c! \ changed data persists
                        then
                        i newdata + c@          \ copy new to old
                        i olddata + c!
                loop
                0 r@  qat-xy: self
                (red)    >fg: self
                (ltgray) >bg: self
                r@ bytes/line * charbytes /
                addroffset +                    \ ADDRESS
                addresswidth: [ self ] (h.) vtype: self
                bl vemit: self
                r@ rowbounds
                do      backcolor               \ DATA HEX
                        i forecolor
                        i newdata + c@
                        charnibbles 1 and
                        if      i baseaddr @ - charbytes mod \ odd sized characters
                                if      2 (h.) vtype: self
                                else    1 (h.) vtype: self
                                then
                        else    2 (h.) vtype: self         \ even sized characters
                        then
                        i baseaddr @ - charbytes mod
                        charbytes 1- =
                        if      (ltgray) >bg: self
                                bl vemit: self
                        then
                loop
                r> rowbounds
                do      backcolor               \ DATA ASCII
                        i forecolor
                        i newdata + c@ 127 and  dup bl <
                        if      drop [char] .
                        then    vemit: self
                loop
                ;

: DumpLines     ( aend abeg -- )
                ?do     i DumpLine
                loop    ;

: DumpAllLines  ( -- )
                dumprows 0 DumpLines ;

: cliptodump    ( n -- n' )
                0max dumpsize 2* 1- min ;

int asciimode   \ 0 = hex dump space, 1 = ascii dump space

:M DumpTitle:   ( -- a n )      s" Hex Window" ;M

: cursorvalid?  ( -- f )        cursorposition u2/  0 dumpsize within ;

: refreshtitle  ( -- )
\ Place cursor in text window and refresh the title bar,
\ also place the blinking cursor
                base{ 
                dumptitle: [ self ] temp$  place   \ name of data space
                s" :  "                   temp$ +place
                cursorposition 2/ dup 0<
                if      drop s" ---"
                else    addroffset + (.)
                then                      temp$ +place  \ "n of [range]"
                s"  of "                  temp$ +place
                addroffset (.)            temp$ +place
                s" .."                    temp$ +place
                dumpsize 1-
                addroffset + (.)          temp$ +place
                s"   "                    temp$ +place
                s" -\/" drop
                refcount 3 mod + 1        temp$ +place
                temp$ count SetTitle: self
                }base 
                ;

:M RefreshCursor: ( -- )
\ Moves cursor to nibble addressed by cursorposition, updates status line
                hwnd
        if      cursorvalid? editing? and
                if      cursorposition nibbles/line /mod swap  ( row col )
                        asciimode
                        if      2/ asciiorigin +
                        else    charnibbles /mod charnibbles 1+ * +
                        then    addresswidth: [ self ] + 1+ swap
                        Point: self
                        ResumeCursor: self
                else    SuspendCursor: self
                then    refreshtitle
        then    ;M

: paintbounds   ( -- last first )
                rowoffset maxrow 1+
                over + dumprows min swap ;

:M On_Paint:    ( -- )
\ Hex dump the entire array to the text buffer.
                refreshcursor: self
                paintbounds DumpLines
                On_Paint: super                 \ display everything
                ;M

:M Refresh:     ( -- )
                hWnd
        if      refreshcursor: self
                paintbounds DumpLines
                Refresh: super                  \ refresh changed lines
        then    ;M

:M ClearPersist: ( -- )
\ Fill the color array with (black)
                colordata dumpsize (black) fill ;M

: clearhexbuffers ( -- )
                newdata dumpsize erase          \ start with all zeros
                olddata dumpsize erase
                ClearPersist: self ;

:M Charwidth:    ( -- #bits )   8 ;M     \ default uses byte characters

:M SetPageRange: ( low high -- )
\ Set the upper and lower limits for accessible range.
\ This must be used before CreateHex
                Charwidth: [ self ] initwidths  \ set up width parameters
                to highaddress
                to lowaddress
                ;M

:M CreateHex:   ( n -- )
\ Allocate memory for buffers,
                dup to dumpsize
                dup malloc  to newdata
                dup malloc  to olddata
                    malloc  to colordata
                dumprows                        \ rows
                columnsalloc
                addresswidth: [ self ] + 1 +    \ cols
                swap CreateBuffer: self
                0 to cursorposition             \ home cursor
                0 to asciimode
                false to editing?
                startaddr: [ self ] to addroffset
                clearhexbuffers
                ;M

:M DestroyHex:  ( -- )
\ Free allocated memory
                destroybuffer: self
                newdata release
                olddata release
                colordata release
                ;M

:M On_Init:     ( -- )
                On_Init: super
                DumpAllLines
                false to editing?
                ;M

:M C!:          ( c a -- )
                newdata + c!
                Refresh: self
                ;M

: seglength     ( n -- len ) TarDataSize min ;

int displayable?  \ T if we want to see a progress window
int blockspan

: UploadBlock   ( a len -- )
\ upload block of data from target board
                dup to blockspan
                displayable?
                if  false sending: progress  start: progress  then
                >r dup newdata +  over olddata +
                r@ move                         \ copy to old
                r>
                begin   dup 0>                  ( a len . )
                while   >r dup                   \ src
                        addroffset charbytes * + \ + offset
                        over newdata +           \ dest
                        r@ seglength
                        read-from: [ self ]  ( ior )    \ read in new
                        key? or                 \ com error or keystroke
                        hWnd 0= or              \ window closed on us
                        if      r>drop 0 >r     \ terminate the upload
                        then
                        TarDataSize +
                        r> TarDataSize -        ( a' len' )
                        displayable?
                        if      blockspan over -
                                blockspan  Update: progress
                                tarerror? if drop 0 then
                        then
                repeat  2drop
                1 +to refcount
                displayable? if close: progress then
                Refresh: self
                ;

:M UploadViewable: ( -- )
\ load viewable region of target data into newdata array and redraw screen
                hWnd
        if      false to displayable?
                viewablemem UploadBlock
        then    ;M



0 value clickcol

: col>asc       ( -- n' )
                clickcol asciiorigin - ;
: col>hex       ( -- n' )
                clickcol dup charnibbles 1+ /mod
                >r charnibbles = or r> - ;      \ invalidate in-between column

: setnewcurs    ( row col -- row )
                over nibbles/line * + to cursorposition ;

:M click:       ( -- )
\ Place cursor at the corresponding byte position
                have-focus?  MouseViewable?: self  and
        if      MouseCR: self           \ current mouse C,R coords
                swap addresswidth: [ self ] - 1-  \ skip address label
                to clickcol
                col>asc dup 0 bytes/line within
                if      2* setnewcurs
                        1 to asciimode
                else    drop
                then
                col>hex dup 0 nibbles/line within
                if      setnewcurs
                        0 to asciimode
                else    drop
                then    drop
                RefreshCursor: self
        then    ;M


: setnewposition ( n -- )
\ set new cursor position and refresh the cursor
\ maintain rowoffset such that the cursor is on-screen
                cliptodump
                dup to cursorposition
                nibbles/line /  to currentrow
                makeviewable: self
                Refresh: self
                ;

: bumphexcurs   ( n -- )
                cursorposition  asciimode
                if      swap 2* +
                else    +
                then    setnewposition ;

: curspagesize  ( -- n ) maxrow nibbles/line *  ;

: cursleft      ( -- )  -1 bumphexcurs ;
: cursright     ( -- )   1 bumphexcurs ;
: cursup        ( -- )  cursorposition nibbles/line - setnewposition ;
: cursdown      ( -- )  cursorposition nibbles/line + setnewposition ;
: cursend       ( -- )  dumpsize 1- setnewposition ;
: curshome      ( -- )  0 setnewposition ;
: curspgup      ( -- )  cursorposition curspagesize - setnewposition ;
: curspgdn      ( -- )  cursorposition curspagesize + setnewposition ;
: curstab       ( -- )  asciimode 1+ 1 and to asciimode
                        RefreshCursor: self ;
: clear-persist ( -- )  ClearPersist: self  DumpAllLines  Paint: self ;

: ishot?        ( a -- f )
                colordata + c@ (ltred) = ;

: sethot        ( a -- )
                2*  setnewposition ;

: nexthot       ( -- )
\ scan to next changed byte after cursor
                bytepos nip
                begin   1+
                        dup dumpsize >=
                        over ishot? or
                until   sethot
                Refresh: self ;

: prevhot       ( -- )
                bytepos nip
                begin   1-
                        dup 0 <=
                        over ishot? or
                until   sethot
                Refresh: self ;

: flipediting   ( -- )
                editing? 0= to editing?
                Paint: self ;

: prevpage      ( -- )
\ adjust address offset to look at the previous page, zero contents
                addroffset
                if      addroffset  dumpsize - lowaddress umax
                        to addroffset
                        clearhexbuffers
                        Paint: self
                then    ;

: nextpage      ( -- )
\ adjust address offset to look at the next page, zero contents
                addroffset dumpsize +
                dup highaddress dumpsize - u>
                if      drop
                else    to addroffset
                        clearhexbuffers
                        Paint: self
                then    ;

: defaultaddr   ( -- )
\ set address offset to the default offset
                startaddr: [ self ] to addroffset
                clearhexbuffers  Paint: self ;

: specialaddr   ( -- )
\ set address offset to the application-specific offset
                specaddr: [ self ] to addroffset
                clearhexbuffers  Paint: self ;

variable tempchar

: byteaddr      ( -- a )
                bytepos nip addroffset + ;

: placebyte     ( c -- )
\ store one byte to the target, read it back out and refresh the screen
                tempchar c!
                tempchar byteaddr 1 write-to:  [ self ] drop   \ store it
                byteaddr tempchar 1 read-from: [ self ] drop   \ retrieve it
                tempchar c@
                bytepos nip newdata + c!      \ store it in the buffer
                cursright
                Refresh: self
                ;

: placenibble   ( n -- )
\ update nibble at cursor, use newdata for the unchanged nibble
                bytepos newdata + c@
                16 /mod  rot                  ( n lo hi . )
                if      nip
                else    drop swap
                then    16 * +
                placebyte ;

: uploadall     ( -- )
                true to displayable?
                0 dumpsize UploadBlock          \ upload the whole block
                DumpAllLines ;                  \ re-dump it

: showhelp      ( -- )
                hexhelptext help: hexhelp ;


:M dokey:       ( c f -- c f' )
\ Perform key action and set f if this window has focus,
                have-focus? hWnd and
        if      drop true               \ set the "have-active" flag
                over
                case    8               of   cursleft           endof
                        k_end           of   cursend            endof
                        k_home          of   curshome           endof
                        k_down          of   cursdown           endof
                        k_up            of   cursup             endof
                        k_left          of   cursleft           endof
                        k_right         of   cursright          endof
                        k_pgdn          of   curspgdn           endof
                        k_pgup          of   curspgup           endof
                        9               of   curstab            endof
                        'C' +k_control  of   copy-to-clipboard  endof
                        'Z' +k_control  of   clear-persist      endof
                        'P' +k_control  of   Paint: self        endof
                        k_F1            of   showhelp           endof
                        k_F2            of   flipediting        endof
                        k_F3            of   nexthot            endof
                        k_F3 +k_shift   of   prevhot            endof
                        k_F4            of   nextpage           endof
                        k_F4 +k_shift   of   prevpage           endof
                        k_F5            of   uploadall          endof
                        k_F6            of   defaultaddr        endof
                        k_F7            of   specialaddr        endof
                        27              of   close: self        endof
                        cursorvalid? editing? and
                        if    \  Refresh: self
                                asciimode
                                if      placebyte
                                else    upc 16 digit
                                        if      placenibble
                                        else    drop
                                        then
                                then    0
                        then
                endcase
        then    ;M

;class


