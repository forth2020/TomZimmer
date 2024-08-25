(( May 27th, 2004   by J.v.d.Ven.  ( http://home.planet.nl/~josv )
 Needed compiler Win32forth version 4.2 build 0671.
 Objective: extend Win32forth with my old fashion tools from
            former and other Forth systems. ( 1802 ELFII + Atari ST )
            Many definitions are created as a work around for the
            missing definitions.
            Perhaps you will hate it. That's ok

Modifications: in this version:

\ Solved a bug in set-priority

\ Additions are made at the end. ))

Forth

s" Win32Forth" environment? not [if]  cr
.( Needs Win32Forth version 4.2 build 0671 or better.) abort  [then]  drop

INTERNAL WinLibrary WINMM.DLL EXTERNAL

only forth definitions decimal

anew toolset.f

\ help-file E:\win32api_hlp\WIN32.HLP

8  constant bs                  13 constant carret
10 constant linefeed            10 constant lf
27 constant escape              34 constant quote

synonym read         r/o        synonym write   r/w
synonym erase-screen cls        synonym ?ms     ms@
synonym d            dir        synonym >>>     noop
synonym PRIVATES     noop       synonym PRIVATE noop
\ synonym Private:   noop       synonym Public: noop
synonym DEPRIVE      noop       synonym ;P      ;
synonym ESC          escape     synonym choose  random
synonym s            r@         synonym >s      >r
synonym s>           r>         synonym =:      constant
synonym -s           r>drop     synonym lo      bye

' \ alias **

: reversed      ( - )            16777215 1 fgbg! ;
: normal        ( - )            1 -1 fgbg! ;
: .forth        ( - )            ." Forth" ;
: .ok           ( - )            ."  ok" ;
: bin           ( - )            2 base !  ;
: missing       ( - )            abort" missing"  ;
: tp            ( - )            .s key escape =  if abort  then cr ;
: ftp           ( - )            f.s key escape =  if abort  then cr ;
: always        ( flag - true )  drop true  ;
: never         ( flag - false ) drop false ;
: .cr           ( n - )           . cr ;
: 0<=           ( - flag )       0 <= ;
: 0>=           ( - flag )       0 >= ;

\ D@ is 'natural 64-bit fetch' and is comparable to 2@ SWAP
: d@            ( - d1_natural_64-bit ) 2@ swap ;

: @+            ( adr -- adr n ) dup cell+ swap @ ;
: 2+            ( n - n+2 )      2 + ;
: 1+!           ( adr - )        1 swap +! ;
: 1-!           ( adr - )       -1 swap +! ;
: 4*            ( n - 4*n )      4 * ;
: (cls          ( - )            _MCLS space 0 0  AT-XY ;
' (cls is cls

: -dup  ( n1 n2    - n1 n1 n2 )    >r dup r>  ;
: -over ( n1 n2 n3 - n1 n2 n1 n3 ) >r over r> ;
: -swap ( n1 n2 n3 - n2 n1 n3 )    >r swap r> ;

defer printfile
: $ftype count "ftype ;
\ ' $ftype is printfile
 ' $fprint is printfile

defer abort/bye
 ' abort is abort/bye
\ ' bye is abort/bye

: Error-StopBox   { adr len \ message$ -- }  ( nfa adr len - )
     MAXSTRING localAlloc: message$
      s" at " message$ place
      0 base @ -rot hex (d.) message$ +place base !
      s" \n" message$ +place
      adr len message$ +place
      message$ +NULL
      MB_OK MB_ICONSTOP  or
      MB_TASKMODAL or
      z" Error:" rel>abs
      message$ 1+ rel>abs
      NULL call MessageBox drop
 ;

: _bye"   ( flag nfa- )
    swap
       if   cr ((")) count Error-StopBox abort/bye
       else ((")) 2drop
       then
;

: bye"   ( -<string">- )
   last @ name> postpone lit compile,
   compile _bye"  ,"  ; immediate


\ : test  true bye" Some error " ;  test

 -2 constant THROW_ABORTQ

: _XMESSAGE      ( n -- )
                r@ here ! BASE @ >R DECIMAL
                CR ." Error in: "
                POCKET COUNT TYPE SPACE
                DUP THROW_ABORTQ =
                IF      DROP MSG @ COUNT TYPE
                ELSE    ." Error # " .
                THEN
                ?LOADING @
                IF      BASE @ >R DECIMAL
                        CR ." File: "
                        LOADFILE @ CELL+ COUNT TYPE
                        ."  at line: "
                        LOADLINE ?
                        R> BASE !
                        EDIT-ERROR
                THEN
                R> BASE ! ;

((  ' _XMESSAGE IS MESSAGE
variable at-word
: (ABORT")      ( f -- )
                \ _.rstack
                2r@  abs>rel at-word ! drop ((")) SWAP
                IF      MSG !
                        THROW_ABORTQ THROW
                THEN    DROP ;

: ABORT"        ( -- )
                COMPILE (ABORT")  ,"  ; IMMEDIATE

: wrong            ( n - 4*n )     1 abort" xxx" ;
: test  wrong noop ;

: test2 test ;
 test2 \ ))

: here!         ( n - )         here ! ;
: here@         ( - n )         here @ ;
: pad!          ( n - )         pad ! ;
: pad@          ( - n )         pad @ ;

: (u.           ( - )           0 (d.) type ;
: u.td          ( n - )         base @ swap decimal (u. base ! ;
: u.th          ( n - )         base @ swap hex (u. base ! ;
: u.tb          ( n - )         base @ swap bin (u. base ! ;
: .dot          ( - )           ." ." ;
: u,.           ( n - )         14 u,.r ;
: ?u,.          ( adr - )       @ u,.   ;
: ?u,.cr        ( adr - )       ?u,. cr ;
synonym dec.    u.td

code abs@ ( absolute-adres - n )
       mov  eax , ebx            \ dst src
       mov  ebx , [eax]
    next  c;

: absdump ( from to  - )
   cr  1+ 0
      do   i cells+ dup u.th ." .. "  dup abs@ u.th tab
           i 8 /mod drop 0=
             if   cr   then
      loop
   drop
 ;
\ 1234 here ! here rel>abs 10 cadump abort


\ radians = degress x PI / 180 (deg to rad conversion)
\ degress = radians x 180 / PI (rad to deg conversion)

: frad   ( fs: deg - rad )    fpi f* 180 s>f f/  ;

synonym local  to   \ NOTE define the local as value before using local
synonym fsqr   fsqrt
synonym pi     fpi

0 value is-static?

variable HIDDEN_SET        \ HIDDEN_SET Off
\ variable Private_Used      Private_Used Off
\ variable OLD_CURRENT       0 OLD_CURRENT !


: wnum?         ( - d flag )      bl word ?uppercase count number? not ;

: tb            ( n - )
   base @  >r  bin  wnum?
   abort" <- Not a binary number" drop state @
        if      [compile] literal
        then    r>  base !
 ; immediate

: th            ( n - )
   base @  >r  hex wnum?
   abort" <- Not a hex number" drop state @
        if      [compile] literal
        then    r>  base !
 ; immediate

: td            ( n - )
   base @  >r  decimal wnum?
   abort" <- Not a decimal number" drop state @
        if      [compile] literal
        then    r>  base !
 ; immediate


th A0005 constant K_SHIFT_RIGHT
th A0004 constant K_SHIFT_LEFT
th A0007 constant K_SHIFT_DOWN
th A0006 constant K_SHIFT_UP

: activate-bit ( bit# - n+bit )        1 swap lshift ;

: bit@         ( n bit# - bit )         activate-bit and ;
' bit@ alias bit-active?
: test-bit     ( n bit# - true/false )  bit@ 0<>  ;

: bit!  ( n 1/0 bit# - n-bit! )      \ puts a bit ( 1/0 ) in n
   dup activate-bit rot
       if   rot or nip               \ 1 ( 1 1-bit# - 1-bit )
       else drop over swap bit@ dup
            if   -                   \ 3 ( 0 1-bit# - 0-bit )
            else drop                \ 2 ( 0 0-bit# - 0-bit )
            then
       then
 ;

: #active-bits  ( n - #active-bits )
   0 32 0
     do  over i bit@ 0> swap +
     loop
   nip abs
 ;

\ debug bit!
\ -1 1 2 bit!  abort \ 1 x
\  tb 11111  0 3  bit!  abort \ 0 1
\  tb 10011  0 2  bit!  abort \ 0 0

: 'of   ( -<name>- )     ' [compile] >body postpone literal ; immediate

: SIMPEL_ARRAY
    CREATE  ( n -- )   ALLOT
    DOES>   ( u -- c-addr ) ;

: cell-array
   create 1+ cell * allot   \ compiletime: ( max-cells - )
   does> swap cells+ @ ;    \ runtime:     ( which - value )

: adres-cell-array ( 'cell-array - cell-0 )
   cell+  ;

: array!    ( n  which 'cell-array  - )
    swap 1+ cells+ ! ;

: array+!    ( n which 'cell-array  - )
    swap 1+ cells+ +! ;

: to-cell
   state @                            \ compiletime: ( -<name_cell-array>- )
    if   postpone [']  compile array! \ compiles:    'name_cell-array array!
    else ' array!                \ runtime:  ( n which <name_cell-array> - )
    then  ; immediate

: +to-cell
   state @                             \ compiletime: ( -<name_cell-array>- )
    if   postpone [']  compile array+! \ compiles:    'name_cell-array array!
    else ' array+!                \ runtime:  ( n which <name_cell-array> - )
    then  ; immediate

: least-used ( from n-cells 'cell-array - least )
   here! over dup here@ execute  swap pad ! -rot swap 2dup - 1 >
        if  do
                i here@ execute 2dup <=
                  if   drop
                  else swap drop i pad !
                  then
             loop
        else   2drop
        then   drop pad @
  ;

: cadump ( from to 'name-cell-array - )
   cr -rot 1+ swap
      do   i dup s>d (d.) type ." .." over execute . tab
           i 8 /mod drop 0=
             if   cr   then
      loop
   drop
 ;

: farray
  create 1+  floats allot     \ compiletime: ( max-cells - )
  does> swap floats + f@ ;    \ runtime:     ( which - value )

: farray!     ( f: n - ) ( which 'farray  - )
    swap floats + cell+ f! ;

: farray+!    ( f: n - ) (  which 'farray  - )
    swap floats + cell+  f+! ;

: fto-cell
   state @                             \ compiletime: ( -<name_cell-array>- )
    if   postpone [']  compile farray! \ compiles:    'name_cell-array array!
    else ' farray!       \ runtime: ( f: n - ) (  which <name_cell-array> - )
    then  ; immediate

: +fto-cell
   state @                              \ compiletime: ( -<name_cell-array>- )
    if   postpone [']  compile farray+! \ compiles:    'name_cell-array array!
    else ' farray+!     \ runtime:  ( f: n - ) ( n which <name_cell-array> - )
    then  ; immediate

: fadump ( from to 'name-cell-array - )
   cr -rot 1+ swap
      do   i dup s>d (d.) type ." .." over execute f. tab
           i 8 /mod drop 0=
             if   cr   then
      loop
   drop
 ;

\ 10 farray ftest
\ 1e 1 fto-cell ftest 2e 2 fto-cell ftest
\ 3e 3 fto-cell ftest 10e 10 fto-cell ftest
\ 0 10 ' ftest fadump  abort

defer y/n?

: _y/n?          ( - true | false )  \ y | n
   ."  [y/n] " key upc ascii Y = dup
        if      ." Yes " else ." No "
        then
 ;

' _y/n? is y/n?

defer .any-key

: _.any-key       ( - )
  ." Press any key to continue......." key drop ;

' _.any-key is .any-key

: low-byte      ( n - max-ff ) th ff and ;
: flip          ( n - nflip )
   dup low-byte  8 lshift swap th ff00 and 8 rshift + ;

synonym ><      flip

: C@+           ( c-addr1 -- c-addr2 char )    dup 1+ swap c@  ;

\ : B@            \ <addr> --- <16b>              signed 16-bit fetch
\                C@+ SWAP C@ >< OR
\                DUP $7FFF > IF $FFFF0000 OR
\                         ENDIF ;

\ : B@+           \ <addr> --- <addr'> <16b>      signed 16-bit fetch+
\                C@+ SWAP C@+ ><  ROT OR
\                DUP $7FFF > IF $FFFF0000 OR
\                         ENDIF ;

\ CREATE 16btmp   0 C, 0 C, PRIVATE

\ : B!            \ <16b> <addr> --- <>           16-bit store
\                SWAP 16btmp !
\                16btmp SWAP 2 MOVE ;

synonym B@  W@              synonym B!  w!

: []cell        ( no addr - addr+offset )
   swap cell * + ;

: u.2           ( n - )    \ print 9 as 09
   dup 10 <
        if      ascii 0 emit then u.td ;
\ 9 9 u.2 u.2 .s abort

: .clock         ( second minute hour - )
   u.2 ." :" u.2 ." ." u.2  ;

: ms_win32s          ( ms - )
   ms@ +
        begin   ms@ over - 0 >=
        until   drop
 ;

\  ' ms_win32s is ms   \ win32s version

: ms-key? ( ms - true/false )   \  waits ms or for a key
   ms@ +
        begin   ms@ over - 0 >= ekey? dup pad ! or
        until   drop pad @
 ;

\ tp 5000 ms-key? tp abort

: dfact         ( day month year  -  dfact )
   >r r@ 365 * rot  + over 1- 31 * + over 2 <=
     if   nip r@ 1- 4 / +
     else  swap 40 * 230 + 100 / -  r@ 4 /  +
     then
   r@ 100 / 3 * 4 / 1+ -
   r> 2000 >= abs + \ solves the millenium bug
 ;

: systemtime-dmy ( systemtime-date - d m y ) dup >r 6 + w@   r@ 2 + w@ r> w@  ;

: day           (  dfact - day )        \ 0   1   2   3   4   5   6
   dup 7 / 7 * - ;                      \ sat sun mon tue wed thu fri

defer .day

: .(day  ( day - )
        case                            \ Windows
        0 of ." saturday"       endof   \ 6
        1 of ." sunday"         endof   \ 0
        2 of ." monday"         endof   \ 1
        3 of ." tuesday"        endof   \ 2
        4 of ." wednesday"      endof   \ 3
        5 of ." thursday"       endof   \ 4
        6 of ." friday"         endof   \ 5
                abort" a bad day"
        endcase space
 ;

: .dag  ( day - )
        case
        0 of ." zaterdag"       endof
        1 of ." zondag"         endof
        2 of ." maandag"         endof
        3 of ." dinsdag"        endof
        4 of ." woensdag"      endof
        5 of ." donderdag"       endof
        6 of ." vrijdag"         endof
                abort" a bad day"
        endcase space
 ;

' .(day is .day

: .month ( month - )
        case
        1 of ." January"    endof
        2 of ." February"   endof
        3 of ." March"      endof
        4 of ." April"      endof
        5 of ." May"        endof
        6 of ." June"       endof
        7 of ." July"       endof
        8 of ." August"     endof
        9 of ." September"  endof
       10 of ." October"    endof
       11 of ." November"   endof
       12 of ." December"   endof
                abort" a bad month"
        endcase space
 ;

: seconds@  ( - seconds )    ms@ 1000 /   ;

86400 constant seconds/day

: -time  ( dfact-start seconds-start dfact-end seconds-end -  days seconds )
   >r rot - dup 0< abort" Invalid start-time."
   tuck 0=
      if    0
      else  seconds/day
      then
   r> + swap -
 ;

: today         ( - day month year )
  get-local-time time-buf dup 6 + w@ over 2 + w@ rot w@ ;

\ Another .date"

defer .date
80 constant hld-max
: hld-count  ( count - hld count-1 )  hld swap 1- ;

\ : .dd-mm-jjjj   ( day month year - )    rot  u.2 ." -" swap  u.2 ." -" (u. ;
\ : .mm/dd/jjjj   ( day month year - )    swap u.2 ." /" swap  u.2 ." /" (u. ;

: ldate   ( day month year dwFlags - adr count )
    >r 3dup dfact 1- dup 0=
      if drop 6
      then
    time-buf 4 + w! \ day of week
    time-buf     w! \ year
    time-buf 2 + w! \ month
    time-buf 6 + w! \ day
    hld-max hld rel>abs 0  time-buf rel>abs
    r> LOCALE_SYSTEM_DEFAULT
    call GetDateFormat dup 0= abort" ldate failed" hld-count
 ;

( DATE_SHORTDATE ) DATE_LONGDATE  value date_format

: .ldate  ( day month year - )   date_format ldate type  ;

: ltime         ( time-buf - adr count )    \ time-buf must be filled
    hld-max hld rel>abs 0  time-buf rel>abs
    TIME_FORCE24HOURFORMAT  LOCALE_SYSTEM_DEFAULT
    call GetTimeFormat dup 0= abort" ltime failed" hld-count
 ;

: .ltime        ( - )   get-local-time ltime type ;
: .today        ( - )   today .date ;
: .time-stamp   ( - )
   .today space today dfact day .day .time time-buf 14 + w@ 10 / ." ." u.2 space
 ;

\ ' .dd-mm-jjjj   is .date
\ ' .mm/dd/jjjj is .date

: control-y?    ( - flag )
   key?
        if      key ctrl y =
        else    false
        then
 ;

: kill-line     ( - )           getxy >r cols over - spaces r> gotoxy ;
: up            ( - )           getxy 1- 0 max gotoxy ;
: input         ( - n )         query bl word count number? 2drop ;

\ : testkill 0 1 gotoxy up ." xxxxxxxxxxxxxxxx " 3 0 gotoxy kill-line .s ;
\ testkill  abort

: delete-chars        ( len - )
   0
        ?do  bs emit space bs emit
        loop
 ;

\ August 24th, 2002 - 16:15
: 0term ( $ count - ) + 0 swap c!  ;

: 0terminated ( adr-counted-string - )
   dup c@ 1+ 0term ;

variable passi

: pass+            ( - old-passi ) \ increment passi
   passi @ passi 1+!  ;

: pass-         ( - old-passi ) \ decrement passi
   passi @ -1 passi +!  ;

\ Format string: [count-allocated count-string string 0]  The "0" is not counted

: string:        ( compile-time: len - )  ( run-time: - adr-counted-string )
   create th fc over <                       \ max 252 char
   abort" String out off range ! "           \ map:
   here   swap 3 + dup allot swap c!         \ max-char. counted string
   does> 1+ ;

: add-to-string ( c string.adr - true/false )
   dup >r 1- c@ r@ c@ 3 + <=
        if      r> 2drop false
        else    r@ c@ 1+ dup r@ c! r@ + c! r> 0terminated true
        then
 ;

: emit$  ( char adr$ - )    add-to-string drop ;

: instring   ( adr-string - )   \ keyboard input for a string.
   dup 1+ over 1 - c@ 1- 1 max accept over c! 0terminated  ;

\ Note: there is a different .string in the hidden dictionary
: .string               ( adr - )       count type ;

: $copy                 ( stringadr-source stringadr-dest - ) \ and makes it 0 terminated
   dup 1- c@ 2 pick c@ 3 + >=
        if      >r  r@ over c@ 1+ cmove r> 0terminated
        else    beep abort" String to copy in too small ! "
        then
 ;

: string"    ( stringadr - ) \ " stream to string "
   quote [compile] word swap 2dup 1- c@ swap c@ dup rot 3 - >
   abort" Input-stream too large"
   1+ over >r cmove r> 0terminated
 ;

: $extract ( adr-counted-string from #chars - adr-string-from #chars ) >r + r> ;

\ April 3rd, 2004
\ string concatenation:  $1 + $2 -> $1+$2 in pad
: $concat ( $1 n $2 n - pad n1+2 )
    temp$ place             \ Save old $2. It might be in pad.
    pad   place             \ Put $1 in place.
    temp$ count pad +place  \ Add old $2.
    pad count
 ;

250 constant /tmp /tmp string: tmp$

\ debug string"
tmp$ string" 321"
\ s" abc "  s" next " $concat s" test " $concat s" Before " 2swap $concat type abort

\ tmp$ 0 3 $extract type  abort

\ .time-stamp .s abort

: ##d ( #zeros d - )    rot 0 ?do  #  loop  ;


10 string: $jjjjmmdd

: jjjjmmdd   ( day month year  - adr ) \ July 6th, 2002 - 17:48 stack corrected
    4 swap s>d <# ##d #> $jjjjmmdd  place
    2 swap s>d <# ##d #> $jjjjmmdd +place
    2 swap s>d <# ##d #> $jjjjmmdd +place
    $jjjjmmdd
 ;

synonym flocal fto  \ NOTE define the flocal as fvalue before using flocal

: (+fto    ( n  'cell-array  - )   >body dup f@ f+ f! ;

: +fto
   state @                        \ compiletime: ( FS: n - ) ( -<name_fvalue>- )
    if   postpone [']  compile (+fto
    else ' (+fto                  \ runtime:     ( FS: n - ) ( 'fvalue - )
    then  ; immediate

\ 2e fvalue t1
\ 3e fto t1 t1 f. abort
\ : ftest  t1 t1 f/ fto t1 ;
\ see ftest  abort

: fwithin  \ <> --- <bool> \ f: <n> <rlow> <rhigh> --- <>
   2 fpick f> f< 0=  and ;

: 2^x  ( x - 2^x )                          dup *  ;

: empty_key_buf ( - )                       key? if key drop then  ;

: fchoose     100000 * random s>f 100000e f/ ;        \ <n> --- <> F: <> --- <r>

: fvalue-to-string \ ( adr - ) fs: ( n - ) \ Borrowed from f.
         >r 0 r@ c!                        \ Now it puts a float in a string
                fdepth 0 <=
                IF      ." Empty " r> drop EXIT
                THEN
                precision 1 max set-precision
                fexam 0x0200 and
                IF      fabs  s" -" r@ +place
                THEN
                fdup f0.5 f<
                IF      s" ." r@  +place f1.0 f+ $ftemp
                        precision 1+ maxsig umin
                        represent
                        drop drop drop
                        $ftemp 1+ precision maxsig 1- umin
                        r@ +place s" " r@ +place
                ELSE    $ftemp precision represent 0=
                        IF      drop drop $ftemp precision
                                r@ +place s" " r@ +place
                        ELSE    drop dup precision <
                                IF      dup 0=
                                        IF      drop s" ." r@ +place
                                                $ftemp precision
                                                r@ +place s" " r> +place EXIT
                                        THEN
                                        $ftemp over r@ +place s" ." r@ +place
                                        $ftemp over + swap precision
                                        swap - r@ +place s" " r@ +place
                                ELSE    dup precision =
                                        IF      $ftemp swap r@ +place
                                                s" . " r> +place
                                                EXIT
                                        THEN
                                        $ftemp precision r@ +place r@ pad !
                                        precision - 0
                                        DO       s" 0" pad @ +place
                                        LOOP
                                         s" . " r@ +place
                                THEN
                        THEN
                THEN r> drop ;

: string>float     \ ( adr - f )  FS: ( - n ) \ Note: 0 on FS when f is false
   count >float dup not
         if 0 s>f
         then
 ;

: float-string>number-string \ ( adr - )
   dup c@ 1- swap c!
 ;

: float-number-string \ ( adr - f ) FS: ( - n )
   >r s" e" r@ +place  r@ c@ 1- r@ c! r> string>float
 ;

\ 1.5e2 fvalue aa
\ 10 string: Aa$  Aa$ string" foo"
\ debug fvalue-to-string
\ aa Aa$ fvalue-to-string  Aa$ cr dup 10 dump string>float  cr ftp  cr .s cr
\ cr Aa$ 10 dump cr
\ abort

: init-random  ( - )
   today dfact ms@ * random
 ;

: (cr           ( - )
   carret emit lf emit  ;

: (type         ( adr len - )
  dup 0>
     if  bounds
         do     i c@ emit
         loop
     else 2drop
  then
 ;

\ You can't nest fopen or fcreate

0 value fid

: fcreate       ( adr-counted-string - )
   count write create-file abort" Can't create file" to fid ;

: fopen         ( adr-counted-string fmode - )
   swap count rot open-file abort" open file error" to fid ;

: fclose        ( - )
   fid close-file abort" close error" ;

: fread         ( adr len - len )
   fid read-file abort" read error" ;

: drop-count    ( adr len - adr-counted-string )
   drop 1- ;

\ : testrd
\       cr s" test.txt " drop-count read fopen
\       tmp$ 1+ 10  fread . ." red"
\       fclose cr tmp$ 20 dump ;

variable (femit

: femit         ( char - )
   (femit C! 0 (femit 1+ C! (femit 1 fid write-file abort" write error in femit"
 ;

: fwrite        ( adr len - )
   fid write-file abort" write error" ;

\ : testwr
\       cr s" test2.txt" drop-count fcreate
\       tmp$ count fwrite
\       tmp$ count fwrite
\       th 34 femit th 35 femit
\       fclose ;
\ testwr cr .s testwr abort

: "!.                   ( pfa - )
  dup >body space ? >name dup c@ 2dup - swap type drop  ."  ! " ;

: ?!                    ( - )   \ compile-time: 1 variable
        postpone ['] postpone "!. ; immediate

\ variable test

\ : test2   ?! test ;
\ see test2
\ abort

: screen-only           ( - )
   ['] _mtype is type   ['] _emit is emit       ['] crtab is cr ;

: emit-to-file          ( - )
   ['] fwrite is type   ['] femit is emit       ['] (cr is cr ;

: file                  ( adr-counted-string - )
   fcreate  emit-to-file ;

: end-of-file           ( - )
   screen-only fclose ;

synonym eof end-of-file

\ : temit   \ Not the fastest way
\   s" test.txt " drop-count file
\   8000 0 do  ascii A emit loop eof ;
\   temit .s dir *.txt abort


create file-time-buf-created 2 cells allot

: get-file-created ( fileid -- system-time )
        >r
        file-time-buf-created 2 cells erase     \ pre-clear buffer
        0                               \ last written time and date not needed
        0                               \ last access time not needed
        file-time-buf-created rel>abs   \ creation time needed
        r> call GetFileTime drop

        _systemtime     rel>abs         \ where to put results
        file-time-buf   rel>abs         \ file time/date to convert
        call FileTimeToSystemTime drop
        _systemtime ;


: extension> ( str.x..xx -  )  \ deletes .x..xx
   count dup 0= abort" Bad filename" "minus-ext" swap 1- c! ;

0 value bufcnt   0 value buffer

: init-buffer   ( - )    2024 DynAlloc to buffer ;

init-buffer  initialization-chain chain-add init-buffer

: bufload               ( file-name - )
   fid >r read fopen buffer ( -1) 2000 fread to bufcnt fclose r> to fid ;

: bufwrite              ( file-name - )
   fid r> buffer bufcnt rot fcreate fwrite fclose r> to fid ;

\ buftest
\ tmp$ string" test.txt"
\ : buftest tmp$ bufload buffer 100 dump ;
\ buftest
\ abort


\ Implements needs.  This works as follows:
\ Not activated.
\ needs foo tools.f
\
\ If foo is not defined, the file tools.f will be loaded, which should
\ define foo.  If foo is already defined, nothing will happen.

\ : needs ( -- ) ( Input Stream: desired-word file-name )
\   >in @ defined nip nip
\   bl [compile] word swap
\        if      drop
\        else    tmp$ $copy tmp$ $fload
\        then
\ ;

\ You don't have to define binary files to save a few variables.
\ Save them in a ASCII file.
\
\ : save-on-disk                ( - )
\    cr s" test.f" drop-count file
\       ." forth needs femit tools.f" cr
\       ." only forth also definitions decimal"  cr
\       ?! passi eof
\ ;
\
\ save-on-disk
\ cr  ftype test.f abort

: count!     ( adr len - )    swap c!  ;

: $max-count ( string$-adr - str$ max-allocated )   dup>r 1 - c@ r> 1+ swap ;

: fvalue-to-string-and-count ( n addr$ - addr len )
   >r r@ fvalue-to-string r> dup count ;

1 value checked
: check!     ( 1/0 bit# - )   checked -rot bit! to checked ;

: OpenProcessToken ( - token )
   here rel>abs TOKEN_ADJUST_PRIVILEGES TOKEN_QUERY  or call GetCurrentProcess
   call OpenProcessToken drop here @
 ;

: GetEnvironmentVariable  ( zstr-EnvironmentVariable-name buffer n - adr n )
   swap dup >r rel>abs rot rel>abs call GetEnvironmentVariable r> swap ;

: SetEnvironmentVariable  ( zstr-EnvironmentVariable-name  buffer n - )
   pad +place pad dup +null 1+ rel>abs swap rel>abs
   call SetEnvironmentVariable drop
 ;

: DelEnvironmentVariable  ( zstr-EnvironmentVariable-name  - )
   rel>abs 0 pad ! pad rel>abs swap
   call SetEnvironmentVariable drop
 ;

(( Usage:
 z" TEST" s" Hello" setEnvironmentVariable
 z" TEST"   DelEnvironmentVariable
 z" TEST" s" 2Hello" setEnvironmentVariable
 z" TEST" buffer 256 GetEnvironmentVariable cr dump abort ))

: computername$! ( adr - ) \ March 30th, 2002 was GetComputerName
   100 pad ! pad rel>abs  \ lpszName
   over 1+ rel>abs             \ lpdwbuffer
   call GetComputerName drop  pad @ swap c! ;

: username$!     ( adr - )  \ March 30th, 2002  was GetUserName
   100 pad! pad rel>abs over 1+ rel>abs  call GetUserName  drop
   pad@ 1- swap c!
  ;

: .id-user
  cr ." Username: " here dup UserName$!     .string
  cr ." At      : " here dup computername$! .string ;

cr ' .ldate   is .date
cr today  .ldate space .ltime
.id-user cr 

\ April 22nd, 2002 - 22:04 renamed box to msgbox
: msgbox    { \ title$ message$ -- } ( adr2 len2 adr1 len1 type  - button )
    MAXSTRING localAlloc: message$
    MAXSTRING localAlloc: title$
    >r
    message$ place message$ +NULL
    title$   place title$   +NULL
    r>
    title$ 1+ rel>abs message$ 1+ rel>abs
    NULL call MessageBox
 ;

\ changed May 17th, 2003
: infobox   { \ title$ message$ -- }  ( adr2 len2 adr1 len1  - )
    [ MB_OK MB_ICONINFORMATION or MB_TASKMODAL or ] literal msgbox drop ;

\ changed April 1st, 2002 - 18:49
: qbox    ( adr2 len2 adr1 len1  - button )
    [ MB_YESNOCANCEL MB_ICONQUESTION or MB_TASKMODAL or ] literal msgbox  ;

\ added April 1st, 2002 - 18:49
: y/n-box  ( adr2 len2 adr1 len1  - button )
    [ MB_YESNO MB_ICONQUESTION or MB_TASKMODAL or ] literal msgbox  ;

\ s" Test" s" Continue " infobox .

20 cell-array tmp-array
: to-tmp-array  ( ... k - )   \ will also roll the elements on stack when fetched
  0 do i to-cell tmp-array loop  ;

: nrel>abs ( start end -- ...abs )   swap do i tmp-array rel>abs loop  ;

((  DWORD  lpAppName,    // points to section name
    LPCTSTR lpKeyName,  // points to key name
    LPCTSTR lpDefault,  // points to default string
    LPTSTR lpReturnedString,    // points to destination buffer
    DWORD nSize,        // size of destination buffer
    LPCTSTR lpFileName  // points to initialization filename  ))

\ lpReturnedString will contain a counted string with a 0 at the end
:  GetPrivateProfileString  ( lpAppName lpKeyName lpDefault lpReturnedString nSize lpFileName - ncopied )
   1+ 6 to-tmp-array 0 tmp-array rel>abs 1 tmp-array 2 tmp-array dup >r 1+ rel>abs
   3 6 nrel>abs  call GetPrivateProfileString r> c! ;

: WritePrivateProfileString  ( lpAppName lpKeyName lpString lpFileName - flag )
   1+ 4 to-tmp-array  0 4 nrel>abs  call WritePrivateProfileString 0=
   abort" Failed to write profile string."
 ;


create profile$ 256 allot

(( \ example:

: inifile ( - adres )
   pad 256 erase current-dir$ count pad place s" \awin.ini" pad +place pad ;

: test_GetPrivateProfileString
  z" MSAPPS2" z" WORDART2" z" Error"  profile$  255 inifile
  GetPrivateProfileString
 ;

: test_WritePrivateProfileString
  z" MSAPPS2"  z" WORDART2"  z" C:\WINNT\MSAPPS\WORDART" inifile
  WritePrivateProfileString
 ;

test_WritePrivateProfileString
test_GetPrivateProfileString profile$ .string \ ))

: s>tmp$      ( n - adr )     s>d (d.) tmp$ place tmp$  dup 0terminated 1+ ;

: f>tmp$      ( f: f - adr )  tmp$ dup fvalue-to-string dup 0terminated 1+ ;

50 string: bad-ini-file$   bad-ini-file$ dup 1- c@ erase
bad-ini-file$ string" --- Bad ini file. ---"

: get$        ( inifile section key - Inifile profile$ )
   bad-ini-file$  1+
   profile$  255 5 pick GetPrivateProfileString profile$ ;

: profile>$  ( Inifile section key - Inifile section adr )
   over >r get$ r> swap ;

\ March 18th, 2002 - 15:15
\ profile>f changed for better reporting when there is an error

: profile>f   ( Inifile section key - Inifile section ) ( f: - f )
   2dup 2>r get$ float-number-string not
      if     S"  Section: " profile$ place
             2r@ swap 1- count profile$ +place
             S"  Key: " profile$ +place
             1- count profile$ +place
             profile$ count Error-StopBox abort
      then
   2r> drop
 ;

: profile>s   ( Inifile section key - Inifile section n ) profile>f f>s  ;

: +inifile    ( inifile section key - inifile adres )
   3 pick WritePrivateProfileString ;

: f>profile   ( inifile section key - inifile section ) ( f: f - )
   over >r f>tmp$ +inifile r> ;

: s>profile   ( inifile section key n - inifile section )
   2 pick >r s>tmp$ +inifile r> ;

\ May 29th, 2001 - 10:19

also bug

: f.s-debug           ( -- )  \ display the floating point stack
                fdepth
                IF      fdepth  ." {" 1 .r ." } "
                show-fp-depth fdepth umin dup 1- swap 0
                        DO      10 ?cr
                                dup i - fpick g.
                        LOOP
                        drop
                ELSE    ." Empty fp stack "
                THEN ;

also forth

: fdebug  ( - ) \ shows the floating point stack while debugging
     ['] f.s-debug is debug-.s ;

: ndebug  ( - ) \ shows the normal stack while debugging
     ['] .s-base is debug-.s ;

previous previous

\ August 21st, 2001 - 11:50

: val>$    ( str$ val -  )  s>d (d.)   rot  place  ;
: val>+$   ( str$ val -  )  s>d (d.)   rot +place  ;
: val>$,.  ( str$ val -  )  s>d (ud,.) rot  place  ;
: val>+$,. ( str$ val -  )  s>d (ud,.) rot +place  ;

\ August 27th, 2001 - 14:08

\ For wide strings with a long count
: +unicode    ( adr-dest c - )    swap dup>r @ dup 2+ r@ ! r> cell+ + w!  ;

: c>unicode! ( dest char - dest+2 )   over w! 2 chars + ;

: ansi>unicode  ( caddrSrc u addrDestUnicode -- )
    -rot over + swap
    ?DO i c@ c>unicode! LOOP
    0 swap w! ;

defer pause

\ September 9th, 2001 - 14:38
\ :INLINE was posted in comp.lang.forth by Marcel Hendrix

: NEXT_CHAR ( -- char ) \ NEXT-CHAR was used in float.f
        SOURCE >IN @ <= IF  DROP -1 EXIT  ENDIF
        >IN @ CHARS + C@  ;

: SKIP-LINE ( -- )
        BEGIN  NEXT_CHAR -1 <>
        WHILE  1 >IN +!
        REPEAT ;

-- Embedded linebreaks are allowed. Maximum length is 4096 characters.
-- Not allowed: EXIT LOCALS| DLOCALS| FLOCALS| , DOES> R> DROP etc.
-- It needs a space as the first character on each line

: MULTI-LINE ( quote "ccc<quote>" tmp-buffer -- str len )
        0 LOCALS| ch buff quote |
        buff off
        BEGIN
          NEXT_CHAR TO ch
          ch '\' = IF  SKIP-LINE  -1 TO ch          ENDIF
          ch -1  = IF  BL TO ch REFILL  ELSE  TRUE  ENDIF
        WHILE
          ch quote <>
        WHILE
          ch  buff @+ +  C!  1 buff +!  1 >IN +!
          buff @ 4096 >=
        UNTIL THEN THEN
        buff @+  REFILL DROP ;


: :INLINE ( ccc; -- )
        CREATE  IMMEDIATE
                ';'
                4096 CELL+ CHARS MALLOC DUP>R
                MULTI-LINE  ( addr u tmp-buffer )
                DUP , HERE CELL+ ,
                HERE OVER ALLOT  SWAP MOVE
                R> RELEASE
        DOES>   2@ EVALUATE ;

(( -- Usage:

:INLINE foo
 \ '\' Is allowed when there is a space at the start of the line
 ." string test2 "
 5 6 +
 ;

: test foo foo + . ;

cr see test test \ ))


\ September 20th, 2001 - 12:55


CODE 4>R        ( n1 n2 n3 n4 -- )    \ push a 4 items onto the returnstack
                pop     eax
                sub     ebp, # 16            \ Return Pointer, Forth's subroutine stac
                mov     0 CELLS [ebp], ebx
                mov     1 CELLS [ebp], eax
                pop     eax
                mov     2 CELLS [ebp], eax
                pop     eax
                mov     3 CELLS [ebp], eax
                pop     ebx                 \ tos
                next    c;

CODE 4R>        ( -- n1 n2 n3 n4 )    \ pop a 4 items off the return stack
                push    ebx
                mov     ebx, 0 CELLS [ebp]
                mov     eax, 3 CELLS [ebp]
                push    eax
                mov     eax, 2 CELLS [ebp]
                push    eax
                mov     eax, 1 CELLS [ebp]
                add     ebp, # 16
                push    eax
                next    c;

CODE 4R@        ( -- n1 n2 n3 n4 )    \ get a copy of the top 4 items on the return stack
                push    ebx
                mov     ebx, 0 CELLS [ebp]
                mov     eax, 3 CELLS [ebp]
                push    eax
                mov     eax, 2 CELLS [ebp]
                push    eax
                mov     eax, 1 CELLS [ebp]
                push    eax
                next    c;

\ : t4r    1 2 3 4 4>R 0 4R@ 2r@ 4R> 4drop 4drop 3drop  ;  debug t4r t4r

\ September 25th, 2001 - 11:27

true value sounds_on/off

: sounds  ( z"sound-file" - )
   sounds_on/off
     if   SND_FILENAME NULL rot rel>abs call PlaySound  then drop  ;

\ October 22nd, 2001 - 23:46

also hidden

\ The following 2 definitions allows access to the entire registry.
\ They are copied from registry.f with a few small changes.

\ sadr,slen = the registry section to get the key of
\ return -1 if we could not get the key

: GetRegKey     { sadr slen samDesired \ root-key key$ n -- regkey | -1 }  \ opens the key of a section
                MAXSTRING localAlloc: key$
                sadr slen             key$ place
                                      key$ +NULL
                >r
                disposition rel>abs         \ we get it, but don't use it
                regkey      rel>abs         \ the return value
                NULL
                samDesired
                REG_OPTION_NON_VOLATILE
                NULL
                0
                key$ 1+  rel>abs
                r> \ root-key
                Call RegCreateKeyEx
                if      -1
                else    regkey @
                then    ;


\ sadr,slen  = the registry key section string
\ vadr,vlen  = the registry key value string to read
\ dadr,dlen  = the registry key data string returned
\ samDesired = security access mask  ( added December 12th, 2001 )

: getregistryentry   ( vadr vlen root-key sadr slen samDesired -- dadr dlen )
                ReturnedKey$ off                \ initially clear return buffer
                GetRegKey dup -1 =
                if      drop
                        ReturnedKey$ count
                        EXIT                    \ return on error, empty data
                then    >r  drop >r
                MAXCOUNTED reglen !             \ init max length of string
                reglen          rel>abs
                ReturnedKey$ 1+ rel>abs
                regtype         rel>abs
                0
                r>              rel>abs
                r@
                Call RegQueryValueEx
                if      ReturnedKey$ off \ true abort" Can not access the registry-key"
                else    reglen @ 1- 0max ReturnedKey$ c!
                then
                ReturnedKey$ count
                r> Call RegCloseKey drop
                ;
previous

(( \ usage
: test_reg$
   s" Directory"  HKEY_CURRENT_USER s" Software\Win32For\Settings" KEY_EXECUTE
   GetRegistryEntry  type cr
\ ( w2k) s" ~MHz"  HKEY_LOCAL_MACHINE s" HARDWARE\DESCRIPTION\System\CentralProcessor\0" KEY_EXECUTE
\ ( w2k) GetRegistryEntry drop ? ." Mhz" cr
   ;
test_reg$ \ ))

\ November 3rd, 2001 - 21:19 added:  u,.  ?u,. ?u,.cr
\ November 3rd, 2001 - 21:35 moved cpuid related words to cpu.f

\ November 21st, 2001 - 12:08 added:
: get-priority  ( - priority_class )
    call GetCurrentProcess  call GetPriorityClass ;

: set-priority  ( priority_class - )    \ May 27th, 2004
    call GetCurrentProcess call SetPriorityClass drop  ;

\ IDLE_PRIORITY_CLASS . 64
\ NORMAL_PRIORITY_CLASS . 32
\ HIGH_PRIORITY_CLASS . 128
\ REALTIME_PRIORITY_CLASS . 256

\ December 17th, 2001 - 13:09
\ Added .ldate
\ Added the security access mask for getregistryentry.

\ January 7th, 2002 - 23:19 from Will Baden's Toolbelt

: third  ( x y z -- x y z x )  2 pick ;
: fourth ( w x y z -- w x y z w )  3 pick ;
: andif  s" dup if drop " evaluate ; immediate
: orif  s" ?dup 0= if " evaluate   ; immediate  \ March 10th, 2002 - 16:34

\ January 10th, 2002 - 22:05
\ added seconds@ seconds/day -time

\ January 14th, 2002 - 12:48
\ Added for access to ini files:
\ s>tmp$  f>tmp$  get$  profile>f  profile>s  +inifile  f>profile  s>profile

: file-exist? (  ( adr len -- true-if-file-exist )
   find-first-file not dup>r
      if    find-close 2drop
      else  drop
      then
   r> ;

\ March 12th, 2002 - 12:19 needed-file aborts when the file is not found.

: needed-file  ( count adr - )
   2dup file-exist? not
       if    temp$ place  s"  is needed." temp$ +place
             s" Missing file" temp$ count infobox true abort" aborted."
       else  2drop
       then
 ;

\ May 22nd, 2002 - 18:50
\ s-exchange to prevent roll when posible

code s-exchange ( ... n[k]..0 k -- ... 0..n[k] )
                mov  eax, ebx               \ save k
                mov  ecx, [esp]             \ save nos
                mov  ebx,  0 [esp] [eax*4]  \ duplicate n[k] in tos
                mov  0 [esp] [eax*4] , ecx  \ put nos where n[k] was
                pop eax           \ discard second item on data stack
                next    c;

\ Usage:
\ 10 20 30 40   3  cr .s  s-exchange    cr .s
\ result:
\ [5] 10 20 30 40 3
\ [4] 40 20 30 10   ok....

:inline odd?  ( n - flag )   1 and  ;
: even?          ( n - flag )   odd? not ;

(( \ Reverse n items on stack
\ Usage: 1 2 3 4 5 5 S_REVERSE ==> 5 4 3 2 1
\ Better optimised
                 ..  1   2   3   4   5   ..
                 ^                       ^
    REG PTR      EBX -->             <-- ECX
    ESP OFFSET       16  12  8   4   0         ))


CODE S-REVERSE ( n[k]..2 1 0 k -- 0 1 2..n[k] )
         lea     ecx, -4 [esp]     \ ecx points 4 under top of stack
         lea     ebx, 4 [ecx] [ebx*4] \ ebx points 4 over stack
\ bump pointers, if they overlap, stop
@@1:     sub     ebx, # 4          \ adjust top
         add     ecx, # 4          \ adjust bottom
         cmp     ecx, ebx          \ compare
         jae     short @@2         \ ecx passing ebx, so exit
\ rotate a pair
\ xor a,b xor b,a xor a,b swaps a and b
         mov     eax, 0 [ebx]      \ bottom to eax
         xor     0 [ecx], eax      \ exchange top and eax
         xor     eax,  0 [ecx]
         xor     0 [ecx], eax
         mov     0 [ebx], eax      \ eax to bottom
         jmp     short @@1         \ next pair
@@2:     pop     ebx               \ tos
         next    c;


\ October 15th, 2002, "Lcc Wizard" Gave me a 2nip in assembler

CODE 2nip       ( n1 n2 n3 n4 -- n3 n4 ) \ 2swap 2drop
                pop     eax
                mov     4 [esp], eax
                pop     eax
                next    c;

\ October 7th, 2002 - 10:12

: mkdir  ( pSecurityAttributes  z"path" - ior )  rel>abs call CreateDirectory ;

\ Empty the directory before using rd
: rd     ( z"path" - ior )                       rel>abs call RemoveDirectory ;

: -string  ( adr1 cnt1 adr2 cnt2 - adr1+cnt2 cnt1-cnt2 )
  dup>r + swap r> - rot drop
 ;

: +pad$ ( adr cnt - ) pad +place ;

: merge$  ( adr2 count2 adr1 count1 - pad count1+2 ) pad place +pad$ pad count ;

: unfold ( adr n ascii1 ascii2 - >adr len flag )
   2over rot scan 2>r scan
     if   1+ 2r>
            if    over - true
            else  drop 0 false
            then
     else  2r> 2drop 1- 0 false
     then
 ;

: fold ( adr n ascii1 ascii2 - pad len )
   >r pad c! tuck pad 1+ swap move pad over 1+ + r> swap c! 2+ pad swap
 ;

: $>profile   ( inifile$ section key counted$+null - inifile section )
   1+ 2 pick >r +inifile r>
 ;

\ Initial Made by Ron Aharon.
\ COMPAREI is the same as COMPARE , but case-insensitive
\ Modifications to get a better shell-sort:
\ 1. The version leaves the chars from 0 to 2F unchanged.
\    The effect is that in a sort the non-alphanumeric characters will not be
\    between the alphanumeric characters.
\ 2. It uses jnb instead of jns which makes sure that the character FF is at
\    the end of a sorted list.
\ 3. Changed the name to COMPAREIA to avoid future conflicts.


code compareia    ( adr1 len1 adr2 len2 -- n )
    sub     ebp, # 8
    mov     0 [ebp], edi
    mov     4 [ebp], esi
    pop     eax                     \ eax = adr2
    pop     ecx                     \ ecx = len1
    pop     esi                     \ esi = adr1
    add     esi, edi                \ absolute address
    add     edi, eax                \ edi = adr2 (abs)
    sub     eax, eax                \ default is 0 (strings match)
    cmp     ecx, ebx                \ compare lengths
    je      short @@2
    ja      short @@1
    dec     eax                     \ if len1 < len2, default is -1
    jmp     short @@2
@@1:
    inc     eax                     \ if len1 > len2, default is 1
    mov     ecx, ebx                \ and use shorter length
@@2:
    mov     bl, BYTE [esi]
    mov     bh, BYTE [edi]
    inc     esi
    inc     edi
    cmp     bx, # th 2F2F            \ skip chars beteen 0 and 2F ( now lower case )
    jle     short @@7
    or      bx, # th 2020            \ May 21st, 2003 or is better then xor
@@7:
    cmp     bh, bl
    loopz   @@2

    je      short @@4               \ if equal, return default
    jnb     short @@3               \ ** jnb for an unsigned test ( was jns )
    mov     eax, # 1                \ if str1 > str2, return 1
    jmp     short @@4
@@3:
    mov     eax, # -1               \ if str1 < str2, return -1
@@4:
    mov     ebx, eax
    mov     edi, 0 [ebp]
    mov     esi, 4 [ebp]
    add     ebp, # 8
    next    c;

\ Searchai str1 for substring str2 in a case-insenitive manner.
\ If found, return the address of the start of the
\ string, the characters remaining in str1 and a true flag.
\ Otherwise return the original str1 and a false flag.

\ ESI = pointer to source string (str2)
\ EBX = length  of source string
\ EDI = pointer to destination string (str1)
\ ECX = length  of destination string
\ EDX = pointer for compare

CODE searchia     ( adr1 len1 adr2 len2 -- adr3 len3 flag )
                test    ebx, ebx
                jne     short @@1
                pop     eax
                dec     ebx             \ zero length matches
                jmp     short @@6
@@1:            sub     ebp, # 12
                mov     0 [ebp], edx    \ save UP
                mov     4 [ebp], edi
                mov     8 [ebp], esi
                pop     esi
                add     esi, edi
                mov     ecx, 0 [esp]
                add     edi, 4 [esp]
                jmp     short @@2

@@4:            inc     edi             \ go to next    c; char in destination
                dec     ecx
@@2:            cmp     ecx, ebx        \ enough room for match?
                jb      short @@5
                sub     edx, edx        \ starting index

@@3:            mov     al, 0 [edx] [esi]
                mov     ah, 0 [edx] [edi]
                or      ax, # th 2020        \ make it lowercase
                cmp     al, ah
                jne     short @@4
                inc     edx

                cmp     edx, ebx
                jne     short @@3

                mov     eax, edi        \ found
                mov     edx, 0 [ebp]
                mov     edi, 4 [ebp]
                mov     esi, 8 [ebp]
                add     ebp, # 12
                sub     eax, edi        \ relative address
                mov     4 [esp], eax
                mov     0 [esp], ecx
                mov     ebx, # -1       \ true flag
                jmp     short @@6
@@5:            sub     ebx, ebx        \ not found
                mov     edx, 0 [ebp]    \ restore UP
                mov     edi, 4 [ebp]
                mov     esi, 8 [ebp]
                add     ebp, # 12
@@6:            next    c;

\ EG search for es uppercase or lowercase in substring tESt:
\ s" tESt" s" es"  searchia .s  drop dump

(( Result:
[3] -1087358 3 -1
4293879938  45 53 74                                          ESt ok
 ))

\ June 8th, 2003

create &InfoRect  4 cells allot    ( - &InfoRect )
&InfoRect 4 cells erase
&InfoRect constant window_x
&InfoRect 1 cells+ constant window_y
&InfoRect 2 cells+ constant bottom_x
&InfoRect 3 cells+ constant bottom_y

: windowposition ( hWnd - ) &InfoRect rel>abs swap  Call GetWindowRect ?win-error ;

250 string: inifile$

\ March 22nd, 2004
defined  cd nip not [IF]
        ' chdir alias  cd
[then]

\ April 5th, 2004

defined  ascii-z nip not [IF]
: ascii-z     ( addr len buff -- buff-z )        \ make an ascii string
       dup>r place r> count over + 0 swap c! ;

[then]

defined  zcount nip not [IF]

: zcount    ( adr -- adr len )    -1 2dup 0 scan nip -  ;

[then]

\s

