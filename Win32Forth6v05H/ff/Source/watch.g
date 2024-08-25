(( watch window support

Getting watch data from the target:

xt>watch ( xt datatype -- a n ) does the following:
1. pushes the xt onto the target stack and executes it
2. evaluates the watch string selected by datatype such that the desired
   result is left on the target's stack.
3. evaluates the string of host xts selected by datatype, which leaves
   a string result on the stack.

The watch-table array contains info for the watcher.  Each record contains:
Cell#0 = the token# of the token to watch
Cell#1 = the address of the data structure for the reader action

Cell#0 is split into upper and lower words.  The lower word is the XT, the
upper word determines the watch strategy:

0 = execute the word on the target and do pre- and post-processing
1 = use cell#1 as an address in main memory to watch, length = 1 byte
2 = same as 1 except data is a cell instead of a byte

))

:object   wwhelp <super helpmsg
:M WindowTitle: ( -- Zstring )
                Z" Token Catalog Key Usage" ;M
;object

create    wwhelptext
         z," ^C  = copy to clipboard\n"
        +z," ESC = done\n"
        0 c,

create wpad 32 allot

: execwatch     ( n -- a len )
\ execute the code for watching entry N in the watch table,
\ return result string
                watch-table swap 2* th  2@ word-split   ( action xt type )
           ?dup if      ( asrc format space+1 )         \ watching memory
                        1- >r byte-split r>             ( asrc n fmt space )
                        swap >r over >r                 ( as n sp | fmt n )
                        wpad 32 blank
                        wpad -rot read-tar drop         \ wpad now has data
                        r> r>                           ( n . )
                        case    0 of drop wpad c@ (.)             endof   \ char
                                1 of drop wpad w@ (.)             endof   \ 16-bit LE
                                2 of drop wpad w@ byte-swap (.)   endof   \ 16-bit BE
                                3 of drop wpad @  (.)             endof   \ 32-bit LE
                                4 of drop wpad @  byte-rev (.)    endof   \ 32-bit BE
                                5 of wpad swap                    endof   \ ASCII
                                2drop s" ???" 0
                        endcase
                else    over 0<>                        \ watching Forth word
                        if      tpush                   \ push xt onto target
                                b.'EXECUTE @ bexecute   \ execute it
                                count 2dup tar-evaluate \ optional processing
                                + aligned
                                begin   dup @
                                while   >r r@ perform   \ postprocessing
                                        r> cell+
                                repeat  drop
                        else    2drop s" ---"
                        then
                then    ;

 80 value sposx         \ start position of watch window
120 value sposy
120 value svposx        \ start position of virtual console window
160 value svposy
 60 value vconcols
 24 value vconrows


: "watchxy"     ( -- )                  \ save X,Y starting position (pels)
                sposx (.)  pad  place   s"  TO SPOSX "   pad +place
                sposy (.)  pad +place   s"  TO SPOSY "   pad +place
                pad count orgsave
                svposx (.)  pad  place  s"  TO SVPOSX "   pad +place
                svposy (.)  pad +place  s"  TO SVPOSY "   pad +place
                pad count orgsave
                vconcols (.)  pad  place  s"  TO VCONCOLS "   pad +place
                vconrows (.)  pad +place  s"  TO VCONROWS "   pad +place
                pad count orgsave
                ;

orgsaver chain-add "watchxy"


\ -------------------- watch window --------------------------------------

:OBJECT watchwin <super TEXTWINDOW
16 constant w.name      \ name width
 8 constant w.raw       \ raw parameter width (display in hex)
20 constant w.data      \ data field
32 constant w.comment   \ comment field width

0         w.name +   1+ constant c.raw
c.raw     w.raw  +   1+ constant c.data
c.data    w.data +   1+ constant c.comment
c.comment w.comment +   constant c.total

: helptext       ( -- a n )      \ the top help line of the window
s"  Name             Literal  Data                 Description" ;

:M TopMargin:   ( -- height )   char-height 3 + ;M

:M StartPos:    ( -- x y )       sposx sposy ;M
:M SaveStartPos: ( x y -- )      to sposy to sposx ;M

:M OnTop:       true ;M

:M LeftMarginColor: ( -- color )   ltred ;M
:M DefaultColor: ( -- c )          (black) 16 * (ltgray) + ;M

:M fieldwidth:  ( -- cols )        c.total  ;M

int refreshcount

:M Retitle:     ( -- )
\ repaint the title bar
                #watch (.)            temp$ place
                s"  watched: "        temp$ +place
                s" /-\" drop
                refreshcount 3 mod chars +  1 temp$ +place
                temp$ count SetTitle: self
                ;M

:M ReTop:       ( -- )
\ refresh the top margin text and the title bar
\ used inside textwindow on_paint:
                SaveDC: dc              \ Draw the help line
                Handle: txtFont SetFont: dc  \ set the font to be used
                TopMarginColor: [ self ]  SetBkColor: dc
                Red                     SetTextColor: dc
                2 1 helptext coloffset /string
                c.total leftjust             TextOut: dc
                RestoreDC: dc
                Retitle: self
                ;M

\ watch entry for memory watching: cell0 = type*2^16 + format
\                                  cell1 = xt of fetcher on host

\ watch label format: link, counted_string

: memlabel      ( color n -- )
\ look up string n of #watch and show it.
                swap >fg: self
                #watch 1- swap -  wslink
        begin   2dup 0<> and
        while   swap 1- swap @   ( n-1 link )
        repeat  nip cell+
                count vtype: self
                ;

:M Labels:      ( -- )
                cls: self
                #watch 0
        ?do     (blue) >fg: self  (ltgray) >bg: self
                0 i at-xy: self
                watch-table i 2* th @ word-split    ( token# type )
                case    0 of
                        xt>npfa ?dup
                        if      PFA-local !     \ parameters for token wt[i]
                                nfa-count       \ name of token
                                w.name leftjust       vtype: self
                                isliteral?
                                if      c.raw     i at-xy: self
                                        t.litvalue cellnibbles (h.)
                                        (black) >fg: self
                                        vtype: self
                                then
                                (blue) >fg: self
                                c.comment i at-xy: self
                                t.catstring w.comment vtype: self
                        else    drop
                        then    endof
                        1 of    (cyan)    i memlabel   endof \ code
                        2 of    (magenta) i memlabel   endof \ data
                        3 of    (green)   i memlabel   endof \ reg
                        4 of    (red)     i memlabel   endof \ ee
                endcase
        loop    ;M

:M Watchem:     ( -- )
\ refresh screen display once based on watch parameters
                hwnd
        if      #watch 0
          ?do   hwnd                    \ make sure window is still open
                if      (red) >fg: self
                        (ltgray) >bg: self
                        c.data i at-xy: self
                        watch-table i 2* th @ word-split ( token#/ format+type )
                        if      drop i execwatch  hwnd  \ watching memory
                                if      w.data leftjust vtype: self
                                else    2drop   \ window closed during execwatch
                                then
                                on-line? 0= ?leave
                                i set-pending
                        else    \ watching Forth token
                                xt>npfa nip
                                b.'EXECUTE @ 0<> and  ?dup  \ safe to execute?
                                if      PFA-local !     \ parameters for token wt[i]
                                        i execwatch  hwnd
                                        if      w.data leftjust vtype: self
                                        else    2drop   \ window closed during execwatch
                                        then
                                        on-line? 0= ?leave
                                        i set-pending
                                else    drop
                                then
                        then
                        key? ?leave
                then
          loop  hwnd
          if    1 +to refreshcount      \ may have closed during loop
                retitle: self
          then
        then    ;M

: showhelp      ( -- )
                wwhelptext  help: wwhelp ;

:M dokey:       ( c f -- c f' )
\ Perform key action and set f if this window has focus,
                have-focus? hWnd and
        if      drop true               \ set the "have-active" flag
                over upc
                case    'C' +k_control  of   copy-to-clipboard  endof
                        k_F1            of   showhelp           endof
                        27              of   close: self        endof
                endcase
        then    ;M

:M On_Init:     ( -- )
                On_Init: super
                SuspendCursor: self ;M

;object


: OpenWatchWindow ( -- )
\ create a text window and display the watch list in it
                fieldwidth: watchwin
                #watch
                CreateBuffer: watchwin              \ create the buffer
                      Labels: watchwin              \ display labels
                       Start: watchwin              \ open window
                ;

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

\ virtual console window -------------------------------------------------

:OBJECT conwatchwin <super TEXTWINDOW

:M OnTop:       true ;M

:M LeftMarginColor: ( -- color )   ltred ;M
:M DefaultColor: ( -- c )          (black) 16 * (ltgray) + ;M

   0 value refreshcount
   0 value constring    \ raw string data
   0 value concount
  80 constant conx
  50 constant cony
conx cony * constant consize
   0 value datalength

:M MaxStartSize: ( -- w h )  vconcols vconrows ;M
:M SaveTxtSize: ( c r -- )   to vconrows to vconcols ;M

:M Retitle:     ( -- )
\ repaint the title bar
                s" Virtual Output Console ["    temp$  place
                datalength (.)                  temp$ +place
                s"  bytes] "                    temp$ +place
                s" /-\" drop
                refreshcount 3 mod chars +  1   temp$ +place
                temp$ count SetTitle: self
                ;M

:M StartPos:    ( -- x y )       svposx svposy ;M
:M SaveStartPos: ( x y -- )      to svposy to svposx ;M

: bumprow       ( -- f )                \ T if rows are maxxed out
                1 +to currentrow
                currentrow textheight 1- >= ;

: bumpcol       ( -- f )                \ T if maxxed out
                currentcol textwidth 1- >=
                if      0 to currentcol bumprow \ wrap cols
                else    false
                then    ;

: drawconsole    ( a n -- )
                bounds     ( a n )
                clrscreen
                ?do     i c@
                        case  10 of  bumprow ?leave             endof \ LF
                              13 of  0 to currentcol            endof \ CR
                              dup bl 127 between
                              if    (white) >bg: self
                                    (black) >fg: self
                                    vemit: self
                              else  (ltgray) >bg: self
                                    (red)  >fg: self  0x10 /mod
                                    >digit vemit: self  bumpcol ?leave
                                    >digit vemit: self
                              then  bumpcol ?leave
                              0
                        endcase
                        currentrow set-pending
                loop
                hwnd
                if       1 +to refreshcount 
                         retitle: self
                         refresh: self
                then
                ;

variable dead

:M Watch:       ( -- )
\ refresh screen display once based on watch parameters
\ abort this operation if there's a keystroke or a comm error
                hwnd \ on-line? and
        if      b.'VCON$ @ ?dup
           if   bexecute tpop tpop   ( n a )
                swap
                dup to datalength               \ save length of data block
                0max consize min >r
                constring r@                    ( as ad len )
                dead off
                begin   dup 0>  dead off? and
                while   3dup TarDataSize min
                        read-data key? or dead @ or dead !
                        rot TarDataSize +
                        rot TarDataSize +
                        rot TarDataSize -
                repeat  3drop
                constring r>  dead off?
                if      drawconsole
                else    2drop
                then
           then
        then    ;M

:M dokey:       ( c f -- c f' )
\ Perform key action and set f if this window has focus,
                have-focus? hWnd and
        if      drop true               \ set the "have-active" flag
                over upc
                case    'C' +k_control  of   copy-to-clipboard  endof
                        27              of   close: self        endof
                endcase
        then    ;M

:M On_Init:     ( -- )
                On_Init: super
                SuspendCursor: self ;M

:M CreateBuf:   ( -- )
\ Allocate memory for buffers,
                consize malloc to constring
                conx cony CreateBuffer: self
                ;M

:M DestroyBuf:  ( -- )
\ Free allocated memory
                destroybuffer: self
                constring release
                ;M

:M On_Done:     ( -- )
                DestroyBuf: self
                On_Done: super
                ;M

;object

: OpenConsole   ( -- )
\ create a text window and display the virtual console in it
                CreateBuf: conwatchwin           \ create the buffer
                    Start: conwatchwin           \ open window
                ;


