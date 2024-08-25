\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ \\\\\\\\\\            Text Window Class                   \\\\\\\\\\
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

 0 constant (black)      1 constant (red)
 2 constant (green)      3 constant (blue)
 4 constant (yellow)     5 constant (magenta)
 6 constant (cyan)       7 constant (ltgray)
 8 constant (dkgray)     9 constant (ltred)
10 constant (ltgreen)   11 constant (ltblue)
12 constant (ltyellow)  13 constant (ltmagenta)
14 constant (ltcyan)    15 constant (white)

create fontname ," Fixedsys" 40 allot   \ default global font name

 8 value fontwidth                      \ global font size, subject to change
14 value fontheight

create $squote 3 c, 'S' c, '"' c, bl c,
create $quote  1 c, '"' c,

0 value started

: "fontname"    ( -- )                  \ configuration saver: font name
                fontname count s" Fixedsys" compare
        if      fontwidth (.)           pad place
                s"  "                   pad +place
                fontheight (.)          pad +place
                s"  "                   pad +place  \ W H
                $squote count           pad +place
                fontname count          pad +place  \     s" name"
                $quote count            pad +place
                s"  NEWCONFONT"         pad +place  \               NewConFont
                pad count orgsave
        then    ;
orgsaver chain-add "fontname"


:class TextWindow <super window

Font txtFont

  int bufferptr                 \ pointer to text buffer
  int updateptr                 \ pointer to updated-row buffer
  int coloffset                 \ # of columns to truncate from left
  int rowoffset                 \ # of rows to truncate from top
  int textwidth                 \ dimensions of allocated text
  int textheight
  int currentcolor              \ selected fg/bg color for text
  int currentcol                \ text cursor position
  int currentrow
  int pointercol                \ blinking pointer position
  int pointerrow
  int cursortick                \ tick count for blinking cursor
  int tickenabled?              \ T if cursor blinking is enabled

\ --------------------- default values -------------------------

16 value barwidth               \ width to allocate for scroll bars

:M TopMargin:   ( -- height )     0 ;M
:M LeftMargin:  ( -- width )     10 ;M
:M RightMargin: ( -- width )      1 ;M
:M BottomMargin: ( -- height )    1 ;M
:M LeftMarginColor: ( -- color )    ltcyan ;M
:M TopMarginColor: ( -- color)      ltgray  ;M
:M MarginDark:  ( -- color)      black  ;M
:M MarginLight: ( -- color)      white  ;M
:M BlankColor:  ( -- color )     ltgray ;M

-1 value ascending?     \ -1 if up, 0 if down

 8 value char-width                     \ actual character size in pixels
 8 value char-height                    \ for all text windows

:M MaxStartSize: ( -- w h )  66 26 ;M

: paintable     ( -- f )        height 0<> started 0= or
                                hwnd and ;


\ ------------------- COLORING AND DIMENSIONING ---------------------

create textcolors
        black ,  red ,     green ,   blue ,
        yellow , magenta , cyan ,    ltgray ,
        dkgray , ltred ,   ltgreen , ltblue ,
        ltyellow , ltmagenta , ltcyan , white ,

:M DefaultColor: ( -- c )       (black) 16 * (white) + ;M

:M >fg:         ( n -- )
\ select 1 of 16 foreground colors
                0x000F and 4 lshift
                currentcolor 0x000F and or
                to currentcolor ;M

:M >bg:         ( n -- )
\ select 1 of 16 background colors
                0x000F and
                currentcolor 0x00F0 and or
                to currentcolor ;M

:M >fgbg:       ( n -- )
\ select 1 of 256 color combinations
                to currentcolor ;M

: textcolor     ( n -- color )
                15 and cells textcolors + @ ;

: cr>xy         ( c r -- x y )
\ convert col,row to x,y coordinates
                char-height *  TopMargin: [ self ] +  swap
                char-width *  LeftMargin: [ self ] +  swap  ;

: xy>cr         ( x y -- c r )
\ convert x,y to col,row coordinates
                swap  LeftMargin:   [ self ] -  char-width /
                swap  TopMargin:    [ self ] -  char-height / ;

: viewable      ( -- c r )
\ Calculate the number of cols and rows that will fit in the viewing area
                width
                LeftMargin:   [ self ] -
                RightMargin:  [ self ] -
                char-width /
                height
                TopMargin:    [ self ] -
                BottomMargin: [ self ] -
                char-height / ;

:M MouseCR:     ( -- c r )
\ get mouse position in character coordinates
                mousex mousey xy>cr
                swap coloffset +
                swap rowoffset +
                ;M

:M TextArea:    ( -- c r )      viewable ;M
:M GetColRow:   ( -- c r )      textwidth textheight ;M

: maxrow        ( -- r )        viewable nip ;
: maxcol        ( -- c )        viewable drop ;

: textabs>rel   ( c r -- c' r' f )
\ return text coordinates where text is to be displayed, T = displayable
                rowoffset - swap
                coloffset - swap
                dup  0 maxrow within >r
                over 0 maxcol within r> and ;

:M MouseViewable?: ( -- f )
\ T if the mouse is within the text window
                mousex mousey xy>cr
                0 maxrow between swap
                0 maxcol between and
                ;M


: updateflag!   ( n f -- )
\ update the pending-redraw flag for row n
                swap 0max textheight min
                updateptr + c! ;

: set-pending   ( n -- )          true updateflag! ;
: clr-pending   ( n -- )          false updateflag! ;
: ispending?    ( n -- f )        updateptr + c@ ;

: all-set-pending ( -- )
                textheight 1+ 0
                do  i set-pending  loop ;

: makeviewable  ( -- f )
\ make sure current row is viewable, return T if rowoffset moves
                rowoffset
                currentrow rowoffset - dup
                >r  0 <
                if      currentrow to rowoffset
                then
                r>  maxrow 1- >=
                if      currentrow maxrow 1- -
                        0max to rowoffset
                then
                rowoffset <> ;

: maxrowoffset  ( -- n )
\ row offset needed to make last line viewable
                textheight maxrow  -  0max ;

: maxcoloffset  ( -- n )
\ column offset needed to make last column viewable
                textwidth  maxcol  -  0max ;

:M CharDims:    ( -- x y )      \ return dimensions of painted characters
                char-width char-height ;M

: clipwidth     ( col -- col' )   0max textwidth 1- min ;
: clipheight    ( row -- row' )   0max textheight 1- min ;

: cursoraddr    ( -- a )
\ address of the char pointed to by the text-tracking cursor
                currentrow textwidth *
                currentcol + chars
                bufferptr + ;

: pointeraddr   ( -- a )
\ address of the char pointed to by the visible blinking cursor
                pointerrow textwidth *
                pointercol + chars
                bufferptr + ;

: bufferlen     ( -- n )        textheight textwidth * chars ;
: buffertext    ( -- a n )      bufferptr bufferlen ;
: buffercolor   ( -- a n )      buffertext tuck + swap ;

: blankrow?    ( row -- f )
\ T if row is all blanks
                textwidth * chars bufferptr +   \ -> row
                textwidth chars -trailing nip   \ # of non-blanks
                0= ;

:M SaveStartPos: ( x y -- ) 2drop ;M    \ save start position for next time
:M SaveTxtSize:  ( c r  -- ) 2drop ;M

: saveparams    ( -- )
                originx originy  SaveStartPos: [ self ]
                viewable         SaveTxtSize:  [ self ]  ;


\ -------------------------- PAINTING ---------------------------

int paintcol            \ local for paintrow

: SetTextColor  ( n -- )
                16 /mod
                textcolor SetTextColor: dc      \ set new colors
                textcolor SetBkColor: dc
                ;
: paintchar     ( col row mask -- )
\ paint a character at a given C,R position in the text buffer
                paintable
        if      >r  2dup textwidth * + chars >r         ( c r | mask addr )
                rowoffset - >r coloffset - r>
                over 0 maxcol within
                over 0 maxrow within and                \ in viewable area?
                if      get-dc
                        SaveDC: dc                      \ save device context
                        Handle: txtFont SetFont: dc
                        cr>xy
                        r> dup buffertext + + c@        ( x y a color | mask )
                        r> xor SetTextColor             ( x y a )
                        bufferptr + 1  TextOut: dc      \ display one character
                        RestoreDC: dc  release-dc
                else    r>drop r>drop 2drop
                then
        else    3drop                                   \ no window to paint to
        then    ;


int prevcursorx         \ needed when turning off the cursor
int prevcursory

: clearcursor   ( -- )  0 to cursortick ;

:M SuspendCursor: ( -- )        false to tickenabled?  clearcursor ;M
:M ResumeCursor:  ( -- )        true to tickenabled?   ;M

: cursor!       ( c r n -- )                    \ display 1 character in window
                >r over to prevcursorx          \ n = 0 to refresh, -1 to invert
                    dup to prevcursory
                r> paintchar ;

:M curscolor:   ( -- n )        -1 ;M

: cursor-on     ( -- )
                prevcursorx prevcursory 0 cursor!   \ turn off previous cursor
                pointercol pointerrow
                curscolor: [ self ] cursor!   \ turn on new cursor
                ;

: cursor-off    ( -- )
                prevcursorx prevcursory 0 cursor!  \ turn off previous cursor
                ;

: cursor-update ( -- )
                cursortick 3 mod
                case    0 of 0 to cursortick            endof
                        1 of cursor-off                 endof
                        2 of cursor-on                  endof
                endcase ;

:M PaintRow:    ( n -- )
\ Redraw visible row n, fill with blanks if n is beyond the displayable
\ area. The cursor position is a special case, colors are inverted
                dup rowoffset + clr-pending
                dup rowoffset + textheight >=
        if      >r 0 r@ cr>xy
                maxcol 1+ r> 1+ cr>xy
                BlankColor: [ self ] FillArea: dc
        else    0 to paintcol
                >r r@ rowoffset + textwidth *
                buffercolor drop swap + textwidth   ( a n )
                coloffset /string
                begin   dup
                while   over >r r@ c@                       \ 1st color
                        skip                                \ a' n'
                        over r@ -                           \ a' n' len
                        r@ c@ 16 /mod
                        textcolor SetTextColor: dc          \ set new colors
                        textcolor SetBkColor: dc
                        r> bufferlen - swap                 \ a' n' a n
                        paintcol r@ cr>xy
                        2swap                               \ .. x y a n
                        dup paintcol + to paintcol
                        TextOut: dc
                repeat  2drop
                paintcol r@ cr>xy
                maxcol 1+ r> 2 + cr>xy                 \ blank unused cols
                BlankColor: [ self ] FillArea: dc
        then    ;M

:M ReBar:       ( -- )
\ draw color bars along left and top margins
                SaveDC: dc
                0 0                 width TopMargin: [ self ]
                TopMarginColor: [ self ]     FillArea: dc      \ top
                0 TopMargin: [ self ]  LeftMargin: [ self ] height
                LeftMarginColor: [ self ]    FillArea: dc      \ left
                MarginDark: [ self ]        LineColor: dc
                LeftMargin: [ self ] 1-  dup
                TopMargin:  [ self ]           MoveTo: dc
                height                           LineTo: dc
                0  TopMargin: [ self ] 1- tuck MoveTo: dc
                width swap                       LineTo: dc
                width 1- dup 0                   MoveTo: dc
                TopMargin: [ self ] 1-         LineTo: dc
                MarginLight: [ self ]       LineColor: dc
                0 0                              MoveTo: dc
                width 0                          LineTo: dc
                0 0                              MoveTo: dc
                0 height                         LineTo: dc
                RestoreDC: dc 
                ;M

:M ReTop:       ( -- )  ;M
\ update top margin, default has nothing to do

: PaintChanges  ( -- )
\ Redraw the lines that have changed
                paintable
        if      SaveDC: dc                      \ save device context
                Handle: txtFont SetFont: dc     \ set the font to be used
                maxrow 1+  0
          ?do   i rowoffset + ispending?        \ absolute row pending?
                if      i paintrow: self        \ draw visible row
                then
          loop  RestoreDC: dc
        then    ;

:M RePaint:     ( -- )
\ screen redraw procedure used by On_Paint
                PaintChanges
\ set the vertical scroll bar limits
                FALSE  maxrowoffset 0 SB_VERT
                hWnd  Call SetScrollRange drop
\ position the vertical button in the scroll bar
                TRUE rowoffset 65535 min SB_VERT
                hWnd  Call SetScrollPos drop
\ set the horizontal scroll bar limits
                FALSE  maxcoloffset 0 SB_HORZ
                hWnd Call SetScrollRange drop
\ position the horizontal button in the scroll bar
                TRUE coloffset 65535 min SB_HORZ
                hWnd Call SetScrollPos drop
                ;M

:M On_Paint:    ( -- )
                saveparams
                ReBar: [ self ]
                ReTop: [ self ]
                all-set-pending
                RePaint: self ;M

:M Refresh:      ( -- )
\ refresh changed lines
                paintable
                if      get-dc
                        PaintChanges
                        release-dc
                then    ;M

:M Redraw:      ( -- )
\ refresh all lines, scroll bars, etc.
                all-set-pending
                paintable
                if      get-dc
                        ReTop: [ self ]
                        RePaint: self
                        release-dc
                then    ;M

:M MakeViewable: ( -- )
                makeviewable
                if      ReDraw: self            \ moved, update text+bars
                else    Refresh: self           \ update text only
                then     ;M




\ ----------------------- CURSOR CONTROL ----------------------------

:M Point:       ( c r -- )
\ Place blinking pointer at C R
                to pointerrow
                to pointercol
                ;M

:M Curse:       ( -- )
\ Place blinking pointer at the cursor position
                currentcol currentrow
                Point: self
                ;M

:M QAT-XY:      ( c r -- )
                clipheight to currentrow
                clipwidth  to currentcol
                ;M

:M AT-XY:       ( c r -- )
                qat-xy: self
                Refresh: self
                ;M

:M AT-XY?:      ( -- c r )      currentcol currentrow ;M
:M Offsets!:    ( c r -- )      to rowoffset  to coloffset ;M

: displayableXY ( c r -- x y f )
\ return X Y where cursor is to be displayed, T = displayable
                rowoffset - swap
                coloffset - swap
                dup  0 maxrow within >r
                over 0 maxcol within >r
                cr>xy  r> r> and ;


\ --------------------- MEMORY ALLOCATION -----------------------

: clrscreen     ( -- )
                buffertext blank
                defaultcolor: [ self ] to currentcolor
                buffercolor currentcolor fill
                all-set-pending
                0 to cursortick
                0 0 qat-xy: self ;

: home          ( -- )
                0 0 offsets!: self ;

:M CreateBuffer: ( c r -- )
\ Allocate memory for a text buffer and clear the screen
                2dup to textheight to textwidth
                * 2* malloc to bufferptr
                textheight 1+ malloc to updateptr
                clrscreen home
                ;M

:M DestroyBuffer: ( -- )
                bufferptr   free drop
                updateptr   free drop ;M


: copy-to-clipboard
\ copies the whole text buffer to the clipboard, inserts CRLFs at the
\ end of each line
                { \ gblhndl -- }
                GetHandle: self  call OpenClipboard
        if      buffertext                      ( a n' )
                textheight 2 chars * +  cell+   \ total length needed
                GMEM_MOVEABLE GMEM_DDESHARE or  \ flags
                call GlobalAlloc to gblhndl     \ allocate a buffer
                                                \ lock memory
                gblhndl call GlobalLock abs>rel ( asrc adest )
                textheight
                begin   dup 1- blankrow?
                        over and
                while   1-                      \ remove trailing blank rows
                repeat  0   ( . . src dest )
                ?do     2dup textwidth move     \ lay in each line
                          over textwidth -trailing nip \ strip trailing LFs
                          + swap textwidth + swap      \ fixed 3/01 BNE
                        13 lay 10 lay           \ plus CRLF
                loop    0 lay                   \ null terminate
                2drop
                gblhndl call GlobalUnlock drop  \ unlock it, done
                call EmptyClipboard ?win-error  \ clear out the clipboard
                                                \ pass to windows
                gblhndl CF_TEXT call SetClipboardData ?win-error
                call CloseClipboard ?win-error
        then    ;

:M CopyClip: copy-to-clipboard ;M

0 value keycol
0 value sortptrs
0 value tempsort

: sortwidth     ( a -- a' n )
                keycol +
                textwidth keycol - ;

:M Sort:        ( c -- )
\ Sort textbuffer lines, key on column c
                0max textwidth 1- min to keycol
                textheight cells malloc to sortptrs     \ pointer list
                bufferlen        malloc to tempsort     \ temporary area
\   preprocess: make array of pointers to text
                textheight 0
                ?do     bufferptr i textwidth * +       \ -> line n
                        sortptrs i th !
                loop
\   sort: sort pointer list using text keys
                textheight 0
                ?do     i  textheight i                 \ find next smallest
                        ?do     sortptrs over th @ sortwidth
                                sortptrs    i th @ sortwidth
                                compare 0< ascending? xor
                                if      drop i
                                then
                        loop
                        dup i =
                        if      drop                    \ already placed
                        else    dup i - cells           ( small #toMove )
                                sortptrs rot th @  swap ( 'small n )
                                sortptrs i th dup cell+ rot move
                                sortptrs i th !
                        then
                loop
\   postprocess: shuffle text according to pointer list
                textheight 0
                ?do     sortptrs i th @                 \ src
                        tempsort i textwidth * +        \ dest
                        textwidth move
                loop
                tempsort buffertext move   \ move shuffled data to text buffer
                textheight 0
                ?do     sortptrs i th @  bufferlen +    \ src
                        tempsort i textwidth * +        \ dest
                        textwidth move
                loop
                tempsort buffercolor move   \ shuffle color data too
                tempsort release
                sortptrs release
                paint: self
                ;M


\ --------------------- SCROLL BAR CONTROL ----------------------

: row-scroll    ( n -- )                        \ add to row offset
                rowoffset + 0max  maxrowoffset min
                to rowoffset  ;

: col-scroll    ( n -- )                        \ add to col offset
                coloffset + 0max  maxcoloffset min
                to coloffset  ;

:M Top:         ( -- )  0 to rowoffset  ;M
:M Bottom:      ( -- )  maxrowoffset to rowoffset  ;M
:M LeftEnd:     ( -- )  0 to coloffset  ;M
:M RightEnd:    ( -- )  maxcoloffset to coloffset  ;M
:M LineUp:      ( -- )  -1 row-scroll ;M
:M LineDn:      ( -- )   1 row-scroll ;M
:M Right:       ( -- )   1 col-scroll ;M
:M Left:        ( -- )  -1 col-scroll ;M
:M PageUp:      ( -- )  maxrow negate row-scroll ;M
:M PageDn:      ( -- )  maxrow row-scroll ;M
:M PageRt:      ( -- )  maxcol col-scroll ;M
:M PageLt:      ( -- )  maxcol negate col-scroll ;M
:M Vposition:   ( n -- )  0max maxrowoffset min to rowoffset  ;M
:M Hposition:   ( n -- )  0max maxcoloffset min to coloffset  ;M

: vpan          ( n -- )  row-scroll  redraw: self ;

:M WM_VSCROLL   ( h m w l -- res )
                swap word-split >r
        CASE
                SB_BOTTOM        of  Bottom: self        endof
                SB_TOP           of  Top:    self        endof
                SB_LINEDOWN      of  LineDn: self        endof
                SB_LINEUP        of  LineUp: self        endof
                SB_PAGEDOWN      of  PageDn: self        endof
                SB_PAGEUP        of  PageUp: self        endof
                SB_THUMBPOSITION of r@  Vposition: self  endof
                SB_THUMBTRACK    of r@  Vposition: self  endof

        ENDCASE Redraw: [ self ]
                r>drop
                0 ;M

:M WM_HSCROLL   ( h m w l -- res )
                swap word-split >r
        CASE
                SB_BOTTOM        of  Rightend:  self      endof
                SB_TOP           of  Leftend:   self      endof
                SB_LINEDOWN      of  Right:     self      endof
                SB_LINEUP        of  Left:      self      endof
                SB_PAGEDOWN      of  PageRt:    self      endof
                SB_PAGEUP        of  PageLt:    self      endof
                SB_THUMBPOSITION of r@  Hposition: self  endof
                SB_THUMBTRACK    of r@  Hposition: self  endof
        ENDCASE Redraw: [ self ]
                r>drop
                0 ;M


\ ------------------ TRADITIONAL TERMINAL OUTPUT -------------------

: movebuffer    ( a n c -- )
\ move buffer back one row and write a row of c at the very end
                >r swap >r              ( n             | c a )
                r@ textwidth + over     ( n a+w n       | c a )
                r@ swap textwidth -     ( n a+w a n-w   | c a )
                move    r> +            ( a+n           | c   )
                textwidth tuck - swap   ( a+n-w w       | c   )
                r> fill ;

:M CR:          ( -- )
\ move cursor down, move data if already at max, move scroll button to
\ make visible
                0 to currentcol
                currentrow textheight 1- <
                if      1 +to currentrow
                else    textheight 1- to currentrow
                        buffertext  bl                       movebuffer
                        buffercolor DefaultColor: [ self ] movebuffer
                        all-set-pending
                then
                Makeviewable: self  \ make the current cursor row visible
                ;M

: bumpcursor    ( -- )
\ move cursor right 1 position, wrap at right, limit at bottom
                currentcol textwidth 1- <
        if      1 +to currentcol
        else    cr: self
        then    ;

:M CLS:         ( -- )
\ clear screen, home cursor
                clearcursor
                clrscreen home Refresh: self ;M

: (vemit)       ( c -- )
                cursoraddr  dup >r c!           \ store character
                currentcolor r> bufferlen + c!  \ store color
                ;

: (qemit)       ( c -- )
                (vemit) currentcol currentrow 0 paintchar
                ;

: (vtype)       ( a n -- )
                >r cursoraddr tuck r@ move      ( ad | n )
                bufferlen + r> currentcolor fill
                ;

: (qtype)       ( a n -- )
                over >r >r cursoraddr tuck r@ move ( ad | a n )
                bufferlen + r@ currentcolor fill  \ store color
                currentcol currentrow textabs>rel ( c' r' . | a n )
                if      get-dc SaveDC: dc
                        Handle: txtFont SetFont: dc
                        currentcolor SetTextColor
                        cr>xy 2r> TextOut: dc
                        RestoreDC: dc release-dc
                else    2drop r>drop r>drop       \ outside display area
                then
                ;


:M VEMIT:       ( c -- )
\ fast emit without cursor wrap
                (vemit) 1 +to currentcol
                ;M

:M QEMIT:       ( c -- )
\ place character in the buffer but don't redraw
                (vemit) bumpcursor
                ;M

:M EMIT:        ( c -- )
\ store character to the next position, bump cursors, redraw the line
                 (qemit) bumpcursor
                Makeviewable: self
                ;M

:M QSPACE:      ( -- )                  bl qemit: self ;M
:M SPACE:       ( -- )                  bl  emit: self ;M
:M VSPACE:      ( -- )                  bl vemit: self ;M
:M VSPACES:     ( -- )  begin ?dup while 1- bl vemit: self repeat ;M

:M VTYPE:       ( a n -- )
\ assumes text wont be written past end of buffer, won't be painted either
                tuck (vtype) +to currentcol ;M

:M QTYPE:       ( a n -- )
                cursoraddr bufferptr - over +  bufferlen >= \ too long?
        if      bounds                          \ yes, use slow version
                ?do     i c@ qemit: self
                loop
        else    vtype: self
                currentcol textwidth /mod
                +to currentrow  to currentcol
        then    ;M

:M TYPE:        ( a n -- )
                cursoraddr bufferptr - over +  bufferlen >= \ too long?
        if      bounds                          \ yes, use slow version
                ?do     i c@ emit: self
                loop
        else    tuck (qtype) currentcol + textwidth /mod
                +to currentrow  to currentcol
        then    ;M



\ -------------------- WINDOW OPEN/CLOSE/SIZE ----------------------

: InitFont      ( -- )
                fontwidth   Width: TxtFont
                fontheight Height: TxtFont
                fontname count SetFacename: TxtFont
                Create: txtFont
                get-dc
                SaveDC: dc
                Handle: txtFont SetFont: dc  \ set the font to be used
                s" X" GetTextExtent: dc      \ get size of characters
                to char-height to char-width
                RestoreDC: dc
                release-dc
                ;

:M NewFont:     ( x y a n -- )
\ Switch the text screen to a new font and redraw
                hWnd
        if      fontname place
                to fontheight to fontwidth
                Delete: txtFont
                InitFont
                Paint: self
        else    4drop
        then    ;M

:M ClassInit:   ( -- )
                ClassInit: Super
                self to CurrentWindow
                InitFont
                Delete: txtFont \ will be created later by On_Init
                ;M

:M On_Init:     ( -- )
                On_Init: super
                0 200 1 hWnd Call SetTimer drop \ tick timer
                ResumeCursor: self
                InitFont
                ;M

:M On_Done:     ( -- )
                saveparams  Delete: txtFont
                1 hWnd Call KillTimer drop
                On_Done: super
                ;M

:M On_SetFocus:  ( h m w l -- )
                On_SetFocus: super
                0 to cursortick
\                ResumeCursor: self
                ;M

:M On_KillFocus: ( h m w l -- )
\                SuspendCursor: self
                On_KillFocus: super
                ;M

:M WM_MOVE      ( hwnd msg wparam lparam -- res )
                WM_MOVE WM: Super               \ move myself first
                0 to cursortick                 \ don't allow cursor ticking right away
                0 ;M

:M WM_TIMER     ( h m w l -- res )
\ handle the cursor using .2s ticks
                tickenabled? have-focus? and hwnd and
        if      cursor-update
                1 +to cursortick
        then    0
                ;M

\ :M StartSize!:  ( w h -- )
\                to char-height
\                to char-width ;M

: cr>size       ( c r -- w h )
\ get screen dimensions needed to hold c * r characters plus scroll bars
                LeftMargin: [ self ]
                RightMargin: [ self ] +
                rot char-width * +  1+
                barwidth + 8192 min
                TopMargin: [ self ]
                BottomMargin: [ self ] +
                rot char-height * + 1+
                barwidth + 8192 min ;

:M MaxSize:     ( -- w h )
                textwidth textheight cr>size
                ;M

:M MinSize:     ( -- w h )
                10 3 cr>size
                ;M

:M StartSize:   ( -- width height )     \ starting window size
                textwidth  maxstartsize: [ self ] drop min  10 max
                textheight maxstartsize: [ self ] nip min    3 max
                cr>size
                ;M

:M WindowTitle: ( -- Zstring )          \ window caption
                z" Text Window"
                ;M

:M OnTop:       ( -- f )        false ;M

:M WindowStyle: ( -- style )
                WS_CAPTION
                WS_THICKFRAME   or
                WS_SYSMENU      or
                WS_MAXIMIZEBOX  or
                WS_MINIMIZEBOX  or
                ;M

:M ExWindowStyle: ( -- style )
                ExWindowStyle: SUPER
                OnTop: [ self ]
                if      WS_EX_TOPMOST or
                then    ;M

;class



\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ \\\\\\\\\\            Console Window Class                \\\\\\\\\\
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

:class ConsoleWindow <super TextWindow

  int margintext          \ buffer for text
  int margincolor         \ buffer for foreground color information
  int currentmcolor       \ current margin foreground color
  int currentmrow
  int margchanged?        \ T if margin text has been modified

:M MarginWidth: ( -- n )   9 ;M
:M LeftMarginColor: ( -- color )    ltgray ;M

: marginwidth   ( -- n )   MarginWidth: [ self ] ;
: marginheight  ( -- n )   textheight ;
: 'margrow      ( n -- a )
                0max marginheight 1- min
                marginwidth * margintext + ;

:M SetMargColor: ( color -- )   to currentmcolor ;M
:M SetMargRow:  ( n -- )        to currentmrow ;M
:M MargRow:     ( -- n )        currentmrow ;M
:M MaxMargRow:  ( -- n )        marginheight ;M

:M MargType:    ( a n -- )
\ load text into a row of the margin
                currentmrow
                currentmcolor over cells margincolor + !  \ set color
                'margrow  dup marginwidth blank         \ unused = blank
                swap marginwidth min move               \ place string
                1 +to currentmrow
                true to margchanged?
                ;M

:M BlankMargin: ( -- )
\ fill margin with blanks, set foreground color to black
                marginheight 0
                ?do     i 'margrow marginwidth blank
                        BLACK
                        i  cells margincolor + !
                loop
                0 to currentmrow ;M

:M CreateBuffer: ( c r -- )
                CreateBuffer: super
                marginheight marginwidth *
                malloc to margintext
                marginheight cells
                malloc to margincolor
                BlankMargin: self
                ;M

:M DestroyBuffer: ( -- )
                margincolor free drop
                margintext free drop
                DestroyBuffer: super
                ;M

:M LeftMargin:  ( -- width )
                marginwidth char-width * 3 + ;M

:M ReMargin:    ( -- )
\ Redraw text in the left margin
                SaveDC: dc                      \ save device context
                Handle: txtFont SetFont: dc     \ set the font to be used
                LeftMarginColor: [ self ]  SetBkColor: dc
                margintext
        if      maxrow marginheight min 0
           ?do  i cells margincolor + @ SetTextColor: dc
                2 i char-height * TopMargin: [ self ] +
                i 'margrow marginwidth TextOut: dc
           loop
        then    RestoreDC: dc
                false to margchanged?
                ;M

:M On_Paint:    ( -- )
                On_Paint: super
                ReMargin: self ;M

:M Refresh:     ( -- )
                Refresh: super
                hWnd    margchanged? and
                if      get-dc
                        ReMargin: self
                        release-dc
                then    ;M


;class


