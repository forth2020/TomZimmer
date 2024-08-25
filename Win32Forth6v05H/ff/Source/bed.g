((
        Brad's EDitor:  Replacement for accept

        Uses the following console words:

        GOTOXY          ( col row -- )          place cursor
        GETXY           ( -- col row )          read cursor position
        GETCOLROW       ( -- col row )          viewable dimensions of console
        TYPE            ( a len -- )            send a string to the console
))

  10 value bedx                         \ default start position
 300 value bedy
  76 value BedWinWidth
   6 value BedWinHeight

vocabulary lineedit
      also lineedit definitions

  30 constant bufheight                 \ # of lines in circular buffer
 128 constant bufwidth
bufheight bufwidth * constant bufsize
create history bufsize allot            \ history buffer
       history bufsize blank
create buffer  bufwidth allot           \ active line
   0 value bufptr                       \ position of cursor in line
   0 value lineptr                      \ current line in history buffer
   0 value columns                      \ size of string to edit
   0 value row0  0 value col0           \ origin for refresh
   0 value finished?                    \ T if finished editing
true value insert?
   0 value windowed?                    \ T if a history window was opened

: buflength     ( -- n )  buffer columns -trailing nip  columns min ;
: bumpline      ( n -- )  lineptr + bufheight mod to lineptr ;
: th-history    ( n -- a ) bufwidth * history + ;
: 'historyline  ( -- a )  lineptr th-history ;
: selectcursor  ( -- )    insert? if big-cursor else norm-cursor then ;

: ED-HOME       ( -- )    0 to bufptr ;
: ED-END        ( -- )    buflength to bufptr ;
: ED-INS        ( -- )    insert? not to insert? selectcursor ;
: ED-CURSMOVE   ( n -- )  bufptr + 0max columns 1- min to bufptr ;
: ED-RIGHT      ( -- )     1 ed-cursmove ;
: ED-LEFT       ( -- )    -1 ed-cursmove ;

: ED-BOUNCE     ( -- )
\ find next occurence of double character
                begin   bufptr buffer + count swap c@ <>    \ not a double char
                        bufptr buflength <  and             \ not at the end
                while   ed-right
                repeat  ed-right ;

: curspair      ( -- a a' n )
\ address of cursor, one past cursor, and # of bytes to end of line
                buffer bufptr +  dup 1+
                columns bufptr - 1- 0max ;

: inbounds?     ( n -- n f )
\ return T if the next move will be in bounds
                dup bufptr + 0 columns within ;

: isblank?      ( -- f )
\ is character at the cursor a blank?
                buffer bufptr + c@ bl = ;

: skipblanks    ( n -- )
\ move cursor until it's not on a blank
                begin   inbounds? isblank? and
                while   dup ed-cursmove
                repeat  drop
                ;

: skipnonblanks ( n -- )
\ move cursor until it's on a blank
                begin   inbounds? isblank? not and
                while   dup ed-cursmove
                repeat  drop
                ;

\ ----------------------------------------------------------------------
:object HistWin <super textwindow

:M LeftMarginColor: ( -- color )    ltgray ;M

:M WindowTitle: ( -- Zstring )          \ window caption
                z" History Buffer"
                ;M

: grayoffset    ( -- row )              \ position of gray line in window
                viewable nip 1- 3 min ;

:M StartPos:    ( x y -- )      bedx bedy ;M
:M SaveStartPos: ( x y -- )     to bedy to bedx ;M

:M MaxStartSize: ( -- w h )  BedWinWidth BedWinHeight ;M
:M SaveTxtSize: ( c r -- )   to BedWinHeight to BedWinWidth ;M

:M On_Paint:    ( -- )
\ write to the text buffer, then repaint it
                bufheight 0
                ?do     0 i qat-xy: self
                        i grayoffset =
                        if      (ltgray) >bg: self
                                buffer bufwidth vtype: self
                                bufptr grayoffset point: self
                        else    (white)  >bg: self
                                lineptr grayoffset - i + bufheight mod
                                th-history  bufwidth  ( a len )
                                vtype: self
                        then
                loop
                (ltgray) >bg: self
                on_paint: super
                ;M

:M On_Done:     ( -- )
                destroybuffer: self
                false to windowed?
                On_Done: super
                ;M

:M graypos:     grayoffset ;M

:M curscolor:   ( -- n )        insert? if -1 else 0xEE then ;M

;object
\ ----------------------------------------------------------------------


: ED-FDEL       ( -- )
                curspair >r swap r> move
                bl buffer columns + 1- c! ;

: ED-LWORD      ( -- )
                -1 skipnonblanks
                -1 skipblanks
                -1 skipnonblanks
                isblank? if ed-right then
                ;

: ED-RWORD      ( -- )
                1 skipnonblanks
                1 skipblanks
                ;

: ED-CHAR       ( c -- )
                buffer insert?
                if      curspair move
                then    bufptr + c!
                ed-right ;

: ED-BACK       ( -- )
                ed-left ed-fdel ;

: ED-DONE       ( -- )
                ed-end
                buffer 'historyline bufwidth move
                1 bumpline
                true to finished?
                windowed?
                if      destroybuffer: HistWin
                        close: HistWin
                        false to windowed?
                then    ;

: ed-redo       ( -- )
                -1 bumpline 'historyline buffer bufwidth move
                 1 bumpline
                true to finished?
                windowed?
                if      destroybuffer: HistWin
                        close: HistWin
                        false to windowed?
                then    ;

: ED-UP/DN      ( n -- )
                bumpline
                'historyline buffer bufwidth move ed-end
                0 to bufptr
                windowed?
                if      paint: HistWin
                else    BufWidth BufHeight createbuffer: HistWin
                        start: HistWin
                        true to windowed?
                then    ;

: ED-UP         ( -- )  -1 ed-up/dn ;
: ED-DOWN       ( -- )   1 ed-up/dn ;

: RedLine       ( -- )
\ redraw line on the console screen
                col0 row0 gotoxy                \ home cursor
                buffer columns <ed+> type <ed-> \ type entire string
                col0 bufptr + row0 gotoxy       \ replace cursor
                ;

defer SpecialEd   ' noop is SpecialEd   ( c -- c' )

: EDLINE        ( c -- )
\ process character and redraw the line
                false to finished?
                SpecialEd
        case    k_home              of  ed-home  endof
                k_up                of  ed-up    endof
\                k_pgup              of  ed-PgUp  endof
                k_left              of  ed-left  endof
                k_right             of  ed-right endof
                k_end               of  ed-end   endof
                k_down              of  ed-down  endof
\                k_pgdn              of  ed-PgDn  endof
                k_insert            of  ed-ins   endof
                k_delete            of  ed-fdel  endof
                k_left  +k_control  of  ed-lword endof
                k_right +k_control  of  ed-rword endof
                k_cr                of  ed-done  endof
                k_tab               of  ed-done  endof
                k_backspace         of  ed-back  endof
                'B'     +k_control  of  ed-bounce endof
                k_F6                of  ed-redo  endof
                dup bl 127 between
                if      ed-char
                else    drop
                then    0
        endcase
                RedLine
                windowed?
                if      0 graypos: Histwin qat-xy: HistWin
                        buffer bufwidth type: HistWin
                        bufptr graypos: Histwin point: HistWin
                        Refresh: HistWin
                then
                ;

: _BACCEPT      ( addr len -- n )
\ accept a string of up to len characters at addr, return # of characters
\ actually entered.
                0 to bufptr
                buffer bufwidth blank
                getxy to row0 to col0
                getcolrow drop col0 - 1-  \ max displayable length
                min                     \ clip to len
                bufwidth min            \ clip to buffer size
                to columns              \ # of editable columns
                selectcursor
                columns
                if      RedLine
                        begin   key edline
                                finished?
                        until
                        buffer swap buflength move
                        buflength
                else
                        drop 0
                then    ;


' _baccept is accept

: "bedxy"       ( -- )                  \ save X,Y starting position (pels)
                bedx (.)  pad  place    s"  TO BEDX "   pad +place
                bedy (.)  pad +place    s"  TO BEDY "   pad +place
                pad count orgsave
                BedWinWidth (.)  pad  place  s"  TO BEDWINWIDTH "  pad +place
                BedWinHeight (.) pad +place  s"  TO BEDWINHEIGHT " pad +place
                pad count orgsave
                ;

orgsaver chain-add "bedxy"

previous definitions

