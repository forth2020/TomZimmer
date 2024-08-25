\ LEDIT.SEQ     Line Editor Utility                     by Tom Zimmer

cr .( Loading F-PC Line Editor...)

\ 07-18-95 SMuB replaced getxy and gotoxy calls with _legetxy, _legotoxy.
\          These routines use buffer coordinates instead of screen coordinates
\          since screen coordinates are volatile.

((

  Here is a relatively simple editor for editing one line strings.

  Support is provided for strings up to 255 characters in length, with
full word and character operations using keypad or WordStar keys as follows:

        Ctrl-A                  Left word
        Ctrl-S                  Left character
        Ctrl-D                  Right character
        Ctrl-F                  Right word
        Ctrl-G                  Forward delete
        Ctrl-T                  Word delete
        Ctrl-Y                  Line delete or clear
        Left arrow              Left character
        Ctrl-Left arrow         Left word
        Right arrow             Right character
        Ctrl-Right arrow        Right word
        Home                    Beginning of line
        End                     End of line
        ESC                     Discard changes and leave
        Return/Enter            Save changes and leave

  The parameters needed by LINEEDIT are as follows:

        lineeditor    ( x y a1 n1 --- )

                x = char pos on row,    zero = left edge
                y = row number,         zero = top line
                a1 = counted string
                n1 = edit limit length, maximum value = 80

  Here is an example of a command that would edit a line of text in
SAMPLEBUFFER, with a maximum length of 12 characters, at location
row 10 column 5 on the screen.

                5 10 samplebuffer 12 lineedit

  Two auto resetting flags can be used to control the behavior of the
line editor in special ways.

        The STRIPING_BL'S boolean "VALUE" determines whether the line
        editor will strip trailing blanks from an edited string at
        the completion of the edit. this VALUE defaults to TRUE, do
        strip trailing blanks.

           false to STRIPPING_BL'S      will prevent line edit from
                                        stripping spaces.

        The AUTOCLEAR boolean "VALUE" determines whether the line
        edit buffer will be automatically cleared if the first
        character you enter on starting an edit is a normal text
        char. This is used to ease the users life in the situation
        where you want to give them the option of re-using a string
        or easily entering a new one without having to delete the old
        string first. This VALUE defaults to FALSE, no autoclear.

            true to AUTOCLEAR           will cause line edit to
                                        automatically clear the edit
                                        string if a letter if the
                                        first thing entered.

))

only forth also definitions

true  value stripping_bl's              \ are we stripping trailing blanks?
false value autoclear                   \ automatically clear line if first
true  value insertmode                  \ insert/overwrite mode flag

                                        \ type entered is a letter;

INTERNAL        \ internal words start here

      variable saveflg                  \ are we saving the results

    0    value ?ldone                   \ is line edit done?
    0    value lchar                    \ recent line edit character
    0    value ledit-x                       \ where we are editing X
    0    value ledit-y                       \ where we are editing Y
    0    value ecursor                  \ edit cursor position
    0    value lenlimit                 \ line edit length limit

defer ledbutton         ' noop is ledbutton

MAXCOUNTED constant maxedit
             create editbuf MAXSTRING allot     \ our edit buffer,
                    editbuf off                 \ 255 characters max

: _legetxy      ( --- x ~y ) \ Negative y indicates buffer coordinates
   getxy getrowoff + invert
;

: _legotoxy     ( x y --- )  \ Goto screen or buffer coordinates
  dup 0<
  if    \ if y is negative, go to buffer coordinates (x,~y)
        invert dup getrowoff dup rows + 1- between 0=
        if \ If the desired buffer y is not in the window, scroll it in
             dup rows - 1+ setrowoff
        then
        getrowoff -
  then
  gotoxy
;


: .lecursor     ( --- )                         \ show the cursor
                ecursor to accept-cnt
                ledit-x ecursor + COLS 1- min ledit-y _legotoxy ;

: .leline       ( --- )                         \ redisplay edit line
                ledit-x ledit-y _legotoxy
                editbuf count "clip" _type
                lenlimit ledit-x + COLS 1- min SP>COL ;

: __le-ldel     ( -- )                         \ Line delete
                0 editbuf c!
                0 to ecursor ;

: _lichar      ( c1 -- )
                autoclear               \ should we clear the line on the
                if      __le-ldel       \ first character typed?
                        false to autoclear
                then
                insertmode
                if      editbuf 1+ ecursor + dup 1+
                        maxedit ecursor - move
                        editbuf c@ 1+ lenlimit min editbuf c!
                then
                editbuf 1+ ecursor + c!
                ecursor 1+ lenlimit min COLS 1- min to ecursor
                ecursor editbuf c@ max editbuf c! ;

: ?lechar        ( --- )                 \ handle normal keys, insert them
                lchar   bl 0xFF between
                if      lchar _lichar
                then    ;

: _le-home      ( --- )                         \ beginning of line
                0 to ecursor ;

: _le-end       ( --- )                         \ End of line
                editbuf c@ to ecursor ;

: _le-right     ( --- )                         \ right a character
                ecursor 1+ editbuf c@ min COLS 1- min to ecursor ;

: _le-left      ( --- )                         \ left a character
                ecursor 1- 0MAX to ecursor ;

: _ledone       ( --- )      \ flag edit is finished, save changes
                true to ?ldone
                saveflg on ;

: _lequit       ( false --- true )      \ flag edit is finished, discard chngs
                true to ?ldone
\+ mark-none    mark-none
                saveflg off ;

defer _le-ret   ' _ledone   is _le-ret
defer _le-tab   ' _ledone   is _le-tab
defer _le-quit  ' _lequit   is _le-quit
defer _le-LF    ' noop      is _le-LF
defer _le-pgup  ' noop      is _le-pgup
defer _le-pgdn  ' noop      is _le-pgdn
defer _le-up    ' noop      is _le-up
defer _le-down  ' noop      is _le-down
defer _le-ldel  ' __le-ldel is _le-ldel

: _le-fdel       ( --- )                         \ Forward delete
                ecursor 1+ editbuf c@ max editbuf c!
                editbuf 1+ ecursor + dup 1+ swap maxedit ecursor - move
                -1 editbuf c+! ;

: >to=bl       ( --- )                         \ forward to a blank
                editbuf 1+ dup maxedit + swap ecursor +
                ?do     i c@ bl = ?leave
                        1 +to ecursor
                loop    editbuf c@ ecursor min to ecursor ;

: >to<>bl      ( --- )                         \ forward to a non blank
                editbuf 1+ dup maxedit + swap ecursor +
                ?do     i c@ bl <> ?leave
                        1 +to ecursor
                loop    editbuf c@ ecursor min to ecursor ;

: _le-rword      ( --- )                         \ Forward to next word
                >to=bl
                >to<>bl ;

: <to=bl+1     ( --- )                         \ back to char following BL
                ecursor 1- 0MAX to ecursor
                editbuf 1+ dup ecursor + 1- editbuf 1+ max
                ?do     i c@ bl = ?leave
                        -1 +to ecursor
            -1 +loop    ;

: <to<>bl      ( --- )                         \ Back to non blank
                ecursor 1- 0MAX to ecursor
                editbuf 1+ dup ecursor + 1- editbuf 1+ max
                ?do     i c@ bl <> ?leave
                        -1 +to ecursor
                loop    ;

: _le-lword      ( --- )                         \ back a word
                <to<>bl
                <to=bl+1 ;

: _le-bdel       ( --- )                         \ back delete
                ecursor editbuf c@ max editbuf c!
                ecursor         ( --- f1 )
                _le-left
    ( --- f1 )  if      insertmode              \ if we are in insertmode
                        if      _le-fdel          \ then delete the character
                        else    bl editbuf 1+ ecursor + c!
                                                \ else change char to blank
                        then
                else    beep
                then    ;

: _le-wdel       ( --- )                         \ word delete
                begin   ecursor editbuf c@ <
                        editbuf 1+ ecursor + c@ bl <> and
                while   _le-fdel
                repeat
                begin   ecursor editbuf c@ <
                        editbuf 1+ ecursor + c@ bl = and
                while   _le-fdel
                repeat  ;

: strip_bl's    ( --- )                         \ strip blanks from editbuf
                ecursor >r
                _le-end
                begin   _le-left
                        editbuf 1+ ecursor + c@ bl =
                        ecursor 0<> and
                while   _le-fdel
                repeat  editbuf c@ r> min 0MAX to ecursor
                editbuf    c@  1 =              \ count=1 & char=blank
                editbuf 1+ c@ bl = and
                if      0 editbuf c!            \ then reset buffer to empty
                then    ;

: _le-ins         ( --- )                         \ toggle insert mode
                insertmode 0= dup to insertmode
                if      big-cursor
                else    norm-cursor
                then    ;

: _le-any         ( --- )                         \ handle any character entry
                ;

: ?control      ( --- )                         \ handle control characters
        lchar   bl <
        if      false to autoclear      \ no auto clear now
                lchar exec:
\               0 null    1 a        2 b       3 c        4 d        5 e       6 f
                noop      _le-lword  noop      _le-pgdn   _le-right  _le-up    _le-rword
\               7 g       8 h        9 i       LF         11 k       12 l      Enter
                _le-fdel  _le-bdel   _le-tab   _le-LF     noop       noop      _le-ret
\               14 n      15 o       16 p      17 q       18 r       19 s      20 t
                noop      noop       noop      noop       _le-pgup   _le-left  _le-wdel
\               21 u      22 v       23 w      24 x       25 y       26 z      Esc
                noop      _le-ins    noop      _le-down   _le-ldel   noop      _le-quit
\               28 \      29 ]       30 ^      31 _
                noop      noop       noop      noop
        then    ;



: ?func        ( --- )                          \ handle function keys
                                                \ if function key bit is set
                lchar   function_mask  special_mask or and      \ func or special
                lchar   shift_mask and shift_mask = or          \ or Shift mask
                lchar   bl                        < or          \ or control key
                if                                              \ or other keypad key
                        false to autoclear                      \ no auto clear now
\ "ledit-chain" allows addingto or over-riding a function ckey at Forth commandline
\ use CHAIN-ADD to add a function test, and CHAIN-ADD-BEFORE to over-ride an
\ existing functionkey during commandline editing.
                        lchar FALSE ledit-chain do-chain 0=
                   if
                        case
        k_home                  of      _le-home  endof   \ Home
        k_up                    of      _le-up    endof   \ Up arrow
        k_pgup                  of      _le-PgUp  endof   \ PgDn
        k_left                  of      _le-left  endof   \ Left arrow
        k_right                 of      _le-right endof   \ Right arrow
        k_end                   of      _le-end   endof   \ End
        k_down                  of      _le-down  endof   \ Down arrow
        k_pgdn                  of      _le-PgDn  endof   \ PgDn
        k_insert                of      _le-ins   endof   \ Ins
        k_delete                of      _le-fdel  endof   \ Del
        k_left  +k_control      of      _le-lword endof   \ Ctrl Left arrow
        k_right +k_control      of      _le-rword endof   \ Ctrl Right arrow
                        endcase
                   else         drop    \ already handled, discard key value
                                0 to lchar
                   then
                then    ;

                                                \ c1 = keyboard character
                                                \ f1 = true for done editing
: _le-key        ( c1 --- )                     \ process a key
                to lchar
                ?lechar                         \ handle normal ascii
                ?func                           \ function characters
                ?control ;                      \ control chars

                                                \ x = char pos on row
                                                \ y = line number
                                                \ a1 = counted string
                                                \ n1 = edit limit length
: <ledit>       ( x y a1 n1 --- )       \ Edit line currently in EDITBUF.
                lenlimit >r
                get-cursor >r
                over c@ ecursor min to ecursor
                maxedit min to lenlimit         \ save max edit length
                dup >r                          \ save source address
                editbuf over c@ lenlimit min 1+ move
                editbuf c@ lenlimit min editbuf c!
                dup 0< 0=                       \ SMuB
                if  getrowoff + invert          \ SMuB
                then                            \ SMuB
                to ledit-y to ledit-x           \ save origin
                _le-ins   _le-ins
                false to ?ldone
                begin   .leline
                        .lecursor
                        key _le-key
                        ?ldone
                until   saveflg @ dup           \ proper save exit
                if      stripping_bl's          \ do we want to strip blanks?
                        if      strip_bl's
                        then    true to stripping_bl's      \ force it next time
                        editbuf r@ over c@ lenlimit min 1+ move
                then    r>drop
                r> set-cursor           ( --- f1 )
                r> to lenlimit
                false to autoclear ;            \ no automatic line clear

EXTERNAL        \ externally available words start here

                                        \ x = char pos on row
                                        \ y = line number
                                        \ a1 = counted string
                                        \ n1 = edit limit length
                                        \ f1 = true for saved changes
                                        \ f1 = false for canceled with ESC
: lineeditor    ( x y a1 n1 --- f1 )    \ Edit line in a1
                defer@ _le-quit >r ['] _lequit is _le-quit
                defer@ _le-LF   >r ['] noop    is _le-LF
                0 to ecursor
                <ledit>
                r> is _le-LF
                r> is _le-quit ;

INTERNAL

MAXSTRING constant b/accept     \ each commandline is MAXSTRING bytes
       31 constant n/accept     \ save 31 previous command lines
                                \ use 31 to make it fit in 8k bytes
  0    value accept#
  0    value accepted?

b/accept n/accept * Pointer prev-accept-buf

: accept-init   ( -- )
                0 to accept#
                prev-accept-buf b/accept n/accept * erase ;

initialization-chain chain-add accept-init      \ add to init chain

create laccept-buf     b/accept allot laccept-buf     off

: +accept#      ( n1 -- )
                accept# + n/accept mod to accept# ;

: prev-accept-buf" ( -- a1 n1 )
                prev-accept-buf accept# b/accept * + count ;

\ SMuB defer saved-accept-key          \ a place to save the entry value of key

\ SMuB : accept-key    ( -- c1 )
\ SMuB                 getrowoff >r
\ SMuB                 saved-accept-key        \ -- c1
\ SMuB                 r> setrowoff
\ SMuB                 show-cursor ;

: accept-lup    ( -- )
                false to accepted?
                -1 +accept#
                prev-accept-buf" editbuf place
                editbuf c@ to ecursor ;

: accept-ldown  ( -- )
                accepted? 0=
                if      1 +accept#
                then    false to accepted?
                prev-accept-buf" editbuf place
                editbuf c@ to ecursor ;

: __laccept     ( a1 n1 -- )
                0 ED_READY win32forth-message           \ notify editor we are ready
\ SMuB          ['] accept-key   is key
                ['] accept-lup   is _le-up
                ['] accept-ldown is _le-down
                laccept-buf c@                          \ backup current line
                if      laccept-buf count
                        prev-accept-buf accept# b/accept * + place
                        1 +accept#
                then    true to accepted?
                swap >r >r
                _legetxy laccept-buf dup off r> lineeditor
                if      laccept-buf count r@ swap move
                        laccept-buf c@ _legetxy nip _legotoxy
                else    editbuf off
                        _legetxy nip 0 swap 2dup _legotoxy cols 1- sp>col _legotoxy
                        1 +accept#
                then    r>drop
                laccept-buf c@ ;

: _laccept      ( a1 n1 -- n2 ) \ line editor version of accept
\ SMuB          defer@ key       is saved-accept-key
                defer@ _le-up   >r
                defer@ _le-down >r
                ['] __laccept catch             \ -- f1
                r> is _le-down                  \ restore these functions
                r> is _le-up
\ SMuB          defer@ saved-accept-key is key
( -- f1 )       throw ;

' _laccept is accept

' _laccept to defaultAccept     \ make this the default handler

MODULE

\s

variable samplebuffer 128 allot

: sample        ( --- )
                s" Zimmer, Harold" samplebuffer place
                true to autoclear
                10 04 samplebuffer 24 lineeditor drop
                cr samplebuffer count type ;

