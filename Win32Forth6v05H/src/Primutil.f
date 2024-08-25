\ load extensions
\ Changes February 14th, 2002 - 1:38 - rls

\ primutil.f beta 2.0A 2002/08/31 arm windows memory managagement
\ primutil.f beta 2.9G 2002/09/24 arm release for testing
\ primutil.f beta 2.9G 2002/10/08 arm Consolidated
\ primutil.f beta 4.9C 2003/02/13 arm Added defered CRTAB, conDC now uses Call GetDC from JvdVen
\ primutil.f beta 4.9C 2002/10/24 rbs Added FORGET chain support for procs, libs
\ primutil.f beta 4.9D 2003/02/19 arm Better OS version support
\ primutil.f 6.05H     2011/10/08 HPO OS version support for Windows 7, added { comment
\ primutil.f 6.05H     2019/02/28 HPO OS version support for Windows 8 and 10

cr .( Loading Primitive Utilities...)
cr .( -- BETA PRIMUTIL.F V6.05H -- )

DECIMAL                                 \ start everything in decimal

\ Some comment words gleaned from various Forths, and C of course.

: _comment      \ char --
                ?loading @
                if      begin   source >in @ /string
                                2 pick scan nip 0=
                        while   refill 0= abort" EOF encountered in comment"
                        repeat
                then    parse 2drop ;

: comment       \ -<char>-
                char _comment ; immediate

-1 value multi-line?    \ we can have multiple line '(' comments

: (             multi-line?
                IF      [char] ) _comment
                ELSE    [char] ) parse 2drop
                THEN    ; immediate

: <A            SOURCE >IN ! DROP ; IMMEDIATE   \ HTML link start marker

: ((            ( -- )
                BEGIN   BL WORD DUP @
                        [ HEX FFFFFF DECIMAL ] LITERAL AND
                        [ HEX 292902 DECIMAL ] LITERAL <>
                WHILE   C@ 0=
                        IF      REFILL
                                0= ABORT" missing ))"
                        THEN
                REPEAT  DROP ; IMMEDIATE

: \S            \ comment to end of file
                FALSE TO INCLUDING?  [COMPILE] \ ; IMMEDIATE

' \S ALIAS \\

: "comment      ( a1 n1 -- )    \ everything is a comment up to the string a1,n1
                ?loading @ 0= abort" Use this comment only while loading"
                2dup upper 2>r
                begin   bl word ?uppercase dup count 2r@ compare
                while   c@ 0=
                        if      refill 0=
                                abort" EOF encountered in comment"
                        then
                repeat  drop 2r> 2drop ;
                
: /*            s" */"     "comment ; immediate         \ comment till */
: (*            s" *)"     "comment ; immediate         \ comment till *)
: DOC           s" ENDDOC" "comment ; immediate         \ comment till ENDDOC
: //            source >in ! drop   ; immediate         \ comment after //
: --            source >in ! drop   ; immediate         \ comment after --
: //{{NO_DEPENDENCIES}} ;                               \ so we can load .H file

: appInst       &hinst @     ;  \ window instance
: cmdline       &cmdlen 2@   ;  \ addr and length of the program commandline
: &cmdline      &cmdlen      ;  \ addr of program commandline addr and length
: _conHndl      &hcon  @     ;  \ window handle for the original console handle
: exception@    &except @    ;  \ exception occured since starting execution?

defer conHndl   ' _conHndl is conHndl   \ so WinEd can change it later

0 constant NULL

: ascii    char          state @ if postpone literal then ; immediate
: alt      char 4096  or state @ if postpone literal then ; immediate
: ctrl     char   31 and state @ if postpone literal then ; immediate

' _exit    constant 'exit

    0 value doClass         \ cfa for classes, initialized in CLASS.F
    0 value do|Class        \ cfa for invisible classes, initialized in CLASS.F

 TRUE value have-kernel.bin?    \ initially pretend we have KERNEL.BIN

: breaker       ( -- )  \ just a place to put a breakpoint if needed
                noop ;

: app-free      ( -- n1 )
                &app-size @ app-here &origin - over min - ;

: app-free!     ( n1 -- )
        2500 max here &origin - +
        dup 500 - &sys-separation @ u>
        if      cr ." New application size would overlap System dictionary!"
                cr ." Clipping to maximum application dictionary allowed!"
                &sys-separation @ umin
                cr ." Application dictionary will have: "
                dup here &origin - - u. ." bytes available."
                cr ." You must use the 'Adjust Forth Dictionaries' menu item"
                cr ." under the Files menu to increase this amount."
        then    &sys-separation @ umin &app-size ! ;

: sys-free      ( -- n1 )
        &sys-size @ dup         \ if size is zero, just return zero
        if      sys-here sys-origin - over min -
        then ;

: sys-free!     ( n1 -- )
        0max dup
        if      sys-here sys-origin - + &sys-size !     \ set system space
        else    &sys-size !                             \ no system/heads
        then ;

: $fload        ( a1 -- f1 )    \ a1 = counted file string
        count included ;  \ f1=false=ok, true=failed

: "fload        ( a1 n1 -- f1 ) \ a1,n1 = file string
        included ;      \ f1=false=ok, true=failed

TRUE constant emit? ( -- f1 )       \ return true if its ok to emit a character

: makedefer     ( cfa -<name>- )   \ turn almost any def into a deferred word
                                    \ and make it execute 'cfa'
                ' >r dodefer r@ ! r> cell+ ! ;

: synonym       ( -<newname> <oldname>- )
                header
                bl word find dup 0= ?missing
                1 =                             \ is it immediate?
                if      immediate               \ make synonym immediate
                then    last @ n>cfaptr ! ;     \ set the cfa pointer of header
                \ Creates an alias of a word that will be
                \ immediate if the original word was immediate.

synonym .. reset-stacks
synonym stop/start start/stop

\ February 16th, 1999 - 17:38 corrected tjz

: ekey>char     ( echar -- char true )  \ return TRUE if displayable character
                dup 0 255 between ;

VARIABLE TONE_FREQ 2640 TONE_FREQ !
VARIABLE TONE_DURA 100 TONE_DURA !
2 PROC Beep

: TONE          ( frequency duration-ms -- )
                swap call Beep drop ;

: BEEP!         ( frequency duration-ms -- )
                TONE_DURA ! TONE_FREQ ! ;
                
: _BEEP         ( -- )
                tone_freq @ tone_dura @ tone ;

defer beep               ' _beep        is beep         \ default sound stuff
defer >bold              ' noop         is >bold
defer >norm              ' noop         is >norm
\ defer do-help          ' noop         is do-help
defer voc-also           ' noop         is voc-also
defer "message           ' 2drop        is "message
defer "top-message       ' 2drop        is "top-message
defer message-off        ' noop         is message-off
defer win32forth-message ' 2drop        is win32forth-message
defer DefaultWindowProc  ' 3drop        is DefaultWindowProc
defer .rstack            ' noop         is .rstack
defer ResetSrcInfo       ' drop         is ResetSrcInfo
defer save-source        ' noop         is save-source

: defer@        ( -<name>- )    \ function currently in deferred word name
                ' >IS
                state @
                if      postpone literal postpone @
                else    @
                then    ; immediate

: _\n->crlf     ( a1 n1 -- )    \ parse "\n" occurances, change to CRLF's
                begin   ascii \ scan dup                \ found a '\' char
                while   over 1+ c@ ascii n =            \ followed by 'n'
                        if      over 13 swap c!         \ replace with CR
                                over 10 swap 1+ c!      \ replace with LF
                        then    1 /string               \ else skip '\' char
                repeat  2drop   ;

' _\n->crlf is \n->crlf                 \ link into kernel deferred word

: -null,        ( -- )
                5 0                     \ remove previous nulls
                do      here 1- c@ ?leave
                        -1 dp +!
                loop    ;

create z-buf  MAXSTRING allot

: asciiz      ( addr len -- buff-z )
  z-buf ascii-z ;


: (z")          ( -- )
                ((")) 1+ ;

: z"            ( -<text">- )
                state @
                if      postpone (z") ,"
                else    [char] " word
                        new$ dup>r over c@ 1+ move
                        r@ count + off
                        r> 1+
                then ; immediate

: z",           ( a1 n1 -- )
                here over 2dup 2>r vec-allot swap move
                2r> \n->crlf
                0 c, align ;            \ terminate with a NULL

: z,"           ( -<text">- ) \ compile text optionally containing "newline"
                ascii " parse z", ;

: +z,"          ( -<text">- )
                -null, z," ;

: +z",          ( a1 n1 -- )
                -null, z", ;

: not           0= ;

: d0=           or 0= ;

: get-commandline ( -- )        \ initialize TIB from the commandline
                0 to source-id
                cmdline (source) 2!
                >in off ;

in-system

: cfa-func      ( -<name>- )
                create hide !csp dodoes call, ] ;

defer enter-assembler   ' noop is enter-assembler
defer exit-assembler    ' noop is exit-assembler

: cfa-code      ( -<name>- )
                create enter-assembler ;

: cfa-comp,     ( cfa -- )      \ compile or execute a CFA
                state @ if , else execute then ;

in-application

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\       words to set the default function for a deferred word
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

: _is-default   ( cfa -- )
                @(ip) >body 2 cells + ! ;

: is-default    ( cfa -<name>- ) \ set the default field of a deferred word
                state @
                if      POSTPONE _is-default
                else    ' >body 2 cells + !
                then    ; immediate

\ define some deferred words with their functions, and defaults

defer page              ' cls        is page

\ Some synonyms that improve compatibility with existing F-PC code.

synonym SP>COL COL
synonym AT-XY gotoxy

: cols          ( -- n1 )               \ current screen columns
                getcolrow drop ;

: rows          ( -- n1 )               \ current screen rows
                getcolrow nip ;

0 value accept-cnt                      \ current count of chars accepted

: _faccept      ( a1 n1 -- n2 )
                0 swap 0
                ?do     drop
                        i to accept-cnt \ save in case we need it
                        key
                        case  8 of      i 1 <           \ if input is empty
                                        if      0       \ do nothing but
                                                beep    \ beep at user
                                        else    1-      \ decrement address 1
                                                -1      08 emit
                                                        bl emit
                                                        08 emit
                                        then
                                                                        endof
                             27 of      dup c@ emit 1+ 1                endof
                             13 of      i leave                         endof
                                        dup emit
                                        2dup swap c!    \ place the character
                                        swap 1+ swap    \ bump the address
                                        1 swap          \ loop increment
                        endcase         i 1+ swap       \ incase loop completes
               +loop    nip ;

' _faccept is accept

: HIWORD        ( n1 -- n2 )
                word-split nip ;

: LOWORD        ( n1 -- n2 )
                word-split drop ;

: "HOLD         ( adr len -- )
                dup negate hld +! hld @ swap move ;

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\       Words that position on the screen
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

 8 value tab-size
 8 value left-margin
 4 value right-margin
 0 value tab-margin
 5 value tabs-max
 0 value tabing?        \ are we tabing, default to no
 0 value first-line?    \ is this the first line of a paragraph
-8 value indent         \ indent/outdent spaces

: wrap?         ( n1 -- f1 )    \ return true if column n1 crosses into the
                                \ right margin area
                getcolrow drop right-margin - > ;


: tab-wrap?     ( n1 -- f1 )    \ return true if column exceeds the maximum
                                \ desired tabs, or crosses into the right
                                \ margin area
                dup tabs-max tab-size * >
                swap wrap? or ;

: TAB           ( -- )
                getxy drop tab-size / 1+ tab-size * col ;

: 0TAB          ( -- )          \ left margin goes to left edge of screen
                0 to tab-margin ;

: +TAB          ( --- )
                tab-size +to tab-margin
                tab-margin tab-wrap?
                IF      0tab
                THEN    ;

: -TAB          ( --- )
                tab-margin tab-size - 0 MAX DUP to tab-margin
                tab-size <
                IF      tabs-max tab-size * to tab-margin
                THEN    ;

: FIRST-LINE    ( -- )          \ set first line flag
                true to first-line?
                0tab ;

: TABING-ON     ( -- )
                true to tabing? ;

: TABING-OFF    ( -- )
                false to tabing? ;

synonym tabbing-off tabing-off
synonym tabbing-on  tabing-on

: _CRTAB        ( -- )
                _cr
                tabing? 0= ?exit
                first-line?
                if      left-margin indent + spaces
                        false to first-line?
                else    left-margin spaces
                        tab-margin spaces
                then    ;
                
DEFER CRTAB ' _CRTAB IS CRTAB

: ?LINE         ( n1 -- )
                0 max getxy drop + wrap?
                if      cr
                then    ;

: allot-to      ( n1 -- )       \ extend the dictionary space of most recent
                                \ word compile to length n1
                last @ name> >body here swap - - dup 0<
                abort" buffer is already too long!" allot ;

create &prognam max-path allot  \ define the buffer that holds the program name
       &prognam off

: "path-only"   ( a1 n1 -- a2 n2 )      \ return path, minus final '\'
                2dup "to-pathend" nip - 2dup + 1- c@ [char] \ =
                if      1- 0max
                then    ;

: "minus-ext"   ( a1 n1 -- a2 n2 )      \ remove the file extension
                2dup [char] . scan nip - ;

: ".ext-only"   ( a1 n1 -- a1 n1 )      \ returns dotted file extension
                2dup "minus-ext" nip /string ;

: ?-\           ( a1 -- )       \ delete trailing '\' if present
                dup count + 1- c@ [char] \ =    \ end in '\'?
                if      -1 swap c+!             \ if so, delete it
                else    drop                    \ else discard a1
                then    ;

: ?+\           ( a1 -- )       \ append a '\' if not already present
                dup count + 1- c@ [char] \ <>   \ end in '\'?
                if      s" \" rot +place        \ if not, append \
                else    drop                    \ else discard a1
                then    ;

: ?+;           ( a1 -- )       \ append a ';' if not already present
                dup count + 1- c@ [char] ; <>   \ end in ';'?
                if      s" ;" rot +place        \ if not, append ;
                else    drop                    \ else discard a1
                then    ;

: ?+:           ( a1 -- )       \ append a [char] : if not already present
                dup count + 1- c@ [char] : <>   \ end in ':'?
                if      s" :" rot +place        \ if not, append ;
                else    drop                    \ else discard a1
                then    ;

\ A word to look through all vocabularies for a matching word to string a1
\ return cfa of nearest definition at or below a1

: ?name         { ?name-val \ ?name-max -- cfa }
                ?name-val aligned cell+ to ?name-val
                &origin to ?name-max    \ don't want any below the origin
                voc-link
                begin   @ ?dup
                while   dup vlink>voc
                        dup voc#threads 0
                        do      dup i cells +
                                begin   @ ?dup
                                while   dup l>name name> ?name-val u<
                                        if      dup l>name name>
                                                ?name-max umax to ?name-max
                                        then
                                repeat
                        loop    drop
                repeat  ?name-max ;

: EXEC:         ( n1 -- )       \ execute the n1 item following
                CELLS R> ABS>REL + @ EXECUTE ;

: ROLL          ( n1 n2 .. nk k -- n2 n3 .. nk n1 )
\  Rotate k values on the stack, bringing the deepest to the top.
                >R R@ PICK SP@ DUP cell + R> 1+ cell * MOVE DROP  ;

: D>S           ( d1 -- n1 )
                drop ;

: CS-PICK       ( dest .. u -- dest )   \ pick both addr and ?pairs value
                2 * 1+ dup>r pick r> pick ;

: CS-ROLL       ( dest -- u -- .. dest ) \ roll both addr and ?pairs value
                2 * 1+ dup>r roll r> roll ;

0 value olddepth

: nostack1      ( -- )
                depth to olddepth ;

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ MSTARSL.F     ANSI extended precision math by Robert Smith
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

: TNEGATE   ( t1lo t1mid t1hi -- t2lo t2mid t2hi )
        invert >r
        invert >r
        invert 0 -1. d+ s>d r> 0 d+
        r> + ;

: UT*   ( ulo uhi u -- utlo utmid uthi )
        swap >r dup>r
        um* 0 r> r> um* d+ ;

: MT*   ( lo hi n -- tlo tmid thi )
        dup 0<
        IF      abs over 0<
                IF      >r dabs r> ut*
                ELSE    ut* tnegate
                THEN
        ELSE    over 0<
                IF      >r dabs r> ut* tnegate
                ELSE    ut*
                THEN
        THEN ;

: UT/   ( utlo utmid uthi n -- d1 )
        dup>r um/mod -rot r> um/mod
        nip swap ;

: M*/  ( d1 n1 +n2 -- d2 )
        >r mt* dup 0<
        IF      tnegate r> ut/ dnegate
        ELSE    r> ut/
        THEN ;

: M+    ( d1 n -- d2 )
        s>d d+ ;

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
  
: FIELD+        ( n1 n2 -<name>- n1+n2 )        \ definer for fields
                create  over , + nostack1
                does>   @ +  ;

: \-            ( -<word>- )            \ load line if word IS NOT defined
                defined nip
                if      POSTPONE \
                then    ; immediate

: \+            ( -<word>- )            \ load line if word IS defined
                defined nip 0=
                if      POSTPONE \
                then    ; immediate

: \IN-SYSTEM-OK ( -<line_to_interpret>- )
                sys-warning? >r
                sys-warning-off
                interpret
                r> to sys-warning? ; immediate

: APP-RESERVE   ( n1 -- )               \ allot some bytes initialized to NULL
                0max app-here over erase app-allot ;

DEFER RESERVE   ' APP-RESERVE IS RESERVE

: SYS-RESERVE   ( n1 -- )               \ allot some bytes initialized to NULL
                0max sys-here over erase sys-allot ;

: C+PLACE       ( c1 a1 -- )    \ append char c1 to the counted string at a1
                dup cincr count + 1- c! ;

\ ,"TEXT" also detect \T embeded in the text and replaces it with a TAB char

: ,"TEXT"       ( -<"text">- )  \ parse out quote delimited text and compile
                                \ it at here  NO EXTRA SPACES ARE NEEDED !!!
                source >in @ /string
                [char] " scan 1 /string                 \ skip past first quote
                2dup [char] " scan                      \ upto next quote
                2dup 2>r nip -                          \ parse out the string
                "CLIP" dup>r
                2dup \n->crlf                           \ fix newlines
                2dup [char] \ scan 2dup 2>r nip -       \ leading part of string
                here place                              \ save in BNAME
                2r> dup
                IF      over 1+ c@ upc [char] T =
                        IF      9         here c+place
                                2 /string here  +place
                                r> 1- >r
                        ELSE    here +place
                        THEN
                ELSE    2drop
                THEN
                r> 1+ allot
                0 c,                            \ null terminate name
                source nip 2r> 1 /string nip - >in !    \ adjust >IN
                ;

: CONVERT       ( ud1 a1 -- ud2 a2 )
                char+ 64 >number drop ;

VARIABLE SPAN

: EXPECT        ( a1 n1 -- )            \ accept the text
                accept span ! ;

: UNUSED        ( -- n1 )               \ return unused HERE in BYTES
                app-free nostack1 ;

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\      2Value words
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

: 2+!           ( d1 a1 -- )            \ double accumulate
                dup>r 2@ d+ r> 2! ;

\ cfa-func do2value              @ 2@  ;    \ in the kernel
  cfa-func do2value!   2 cells - @ 2!  ;
  cfa-func do2value+!  3 cells - @ 2+! ;

: 2value        ( d1 -<name>- )
        header  do2value , here 3 cells+ , do2value! , do2value+! , , , ;

synonym 2to   to
synonym 2+to +to

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\       Array words
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

: byte-array    ( n1 -<name>- )         \ compile time
                ( -- a1 )               \ runtime
                create 1+ here over allot swap erase ;

: word-array    ( n1 -<name>- )         \ compile time
                ( -- a1 )               \ runtime
                create 1+ 2* here over allot swap erase ;

: long-array    ( n1 -<name>- )         \ compile time
                ( -- a1 )               \ runtime
                create 1+ cells here over allot swap erase ;

: double-array  ( n1 -<name>- )         \ compile time
                ( -- a1 )               \ runtime
                create 1+ 2* cells here over allot swap erase ;

: #byte-array   ( n1 -<name>- )         \ compile time          8-bits
                ( n1 -- byte )          \ runtime
                create 1+ here over allot swap erase
                does> + c@ ;

: ^#byte-array  ( a1 -<name>- )         \ compile time          8-bits
                ( n1 -- byte )          \ runtime
                create ,
                does> @ + c@ ;

: #word-array   ( n1 -<name>- )         \ compile time          16-bits
                ( n1 -- word )          \ runtime
                create 1+ 2* here over allot swap erase
                does> swap 2* + w@ ;

: ^#word-array  ( n1 -<name>- )         \ compile time          16-bits
                ( n1 -- word )          \ runtime
                create ,
                does> @ swap 2* + w@ ;

: #long-array   ( n1 -<name>- )         \ compile time          32-bits
                ( n1 -- long )          \ runtime
                create 1+ cells here over allot swap erase
                does> swap cells+ @ ;

: ^#long-array  ( a1 -<name>- )         \ compile time          32-bits
                ( n1 -- long )          \ runtime
                create ,
                does> @ swap cells+ @ ;

: #double-array ( n1 -<name>- )         \ compile time          2 x 32-bits
                ( n1 -- long )          \ runtime
                create 1+ 2* cells here over allot swap erase
                does> swap 2* cells+ 2@ ;

: ^#double-array ( a1 -<name>- )        \ compile time          2 x 32-bits
                ( n1 -- long )          \ runtime
                create ,
                does> @ swap 2* cells+ 2@ ;

: b#->          ( n1 n2 -<name>- )      \ store byte n1 into element n2
                                        \ of byte array
                ' >body state @
                if      POSTPONE literal
                        POSTPONE +
                        POSTPONE c!
                else    + c!
                then    ; immediate

: b#+>          ( n1 n2 -<name>- )      \ store byte n1 into element n2
                                        \ of byte array
                ' >body state @
                if      POSTPONE literal
                        POSTPONE +
                        POSTPONE c+!
                else    + c+!
                then    ; immediate

: w#->          ( n1 n2 -<name>- )      \ store word n1 into element n2
                                        \ of word array
                ' >body state @
                if      POSTPONE 2*
                        POSTPONE literal
                        POSTPONE +
                        POSTPONE w!
                else    swap 2* + w!
                then    ; immediate

: w#+>          ( n1 n2 -<name>- )      \ store word n1 into element n2
                                        \ of word array
                ' >body state @
                if      POSTPONE 2*
                        POSTPONE literal
                        POSTPONE +
                        POSTPONE w+!
                else    swap 2* + w+!
                then    ; immediate

: l#->          ( n1 n2 -<name>- )      \ store long n1 into element n2
                                        \ of long array
                ' >body state @
                if      POSTPONE cells
                        POSTPONE literal
                        POSTPONE +
                        POSTPONE !
                else    swap cells+ !
                then    ; immediate

: l#+>          ( n1 n2 -<name>- )      \ store long n1 into element n2
                                        \ of long array
                ' >body state @
                if      POSTPONE cells
                        POSTPONE literal
                        POSTPONE +
                        POSTPONE +!
                else    swap cells+ +!
                then    ; immediate

: d#->          ( n1 n2 -<name>- )      \ store long n1 into element n2
                                        \ of double array
                ' >body state @
                if      POSTPONE 2*
                        POSTPONE cells
                        POSTPONE literal
                        POSTPONE +
                        POSTPONE 2!
                else    swap 2* cells+ 2!
                then    ; immediate

: d#+>          ( n1 n2 -<name>- )      \ store long n1 into element n2
                                        \ of double array
                ' >body state @
                if      POSTPONE 2*
                        POSTPONE cells
                        POSTPONE literal
                        POSTPONE +
                        POSTPONE 2+!
                else    swap 2* cells+ 2+!
                then    ; immediate

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\      Command line argument words
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

0 0 2value arg"
0 0 2value arg-pos"


: "arg-next"    ( a1 n1 -- a2 n2 )
                bl skip 2dup bl scan nip -
                2dup bl scan 2dup 2>r nip - 2dup 2to arg"
                2r> 2to arg-pos" ;

: arg-1"        ( -- a1 n1 )
                cmdline upper
                cmdline "arg-next" ;

: arg-next"     ( -- a1 n1 )
                arg-pos" "arg-next" ;

: arg-ext"      ( -- a1 n1 )
                arg" "to-pathend" ".ext-only" ;

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\       various number display words
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

: (.)           ( n1 -- a1 n1 ) \ convert number n1 to an ascii string
                s>d (d.) ;

: ?.name        ( cfa -- )      \ try to display the name at CFA
                dup ?name ?dup
                if      .name
                else    dup ." 0x" 1 h.r ."  "
                then    drop ;

: ID.           ( nfa -- )
                NAME> >NAME .ID ;

\ double number display with commas

: (xUD,.)       ( ud commas -- a1 n1 )
                >r
                <#                      \ every 'commas' digits from right
                r@ 0 DO # 2DUP D0= ?LEAVE LOOP
                BEGIN   2DUP D0= 0=     \ while not a double zero
                WHILE   [char] , HOLD
                        r@ 0 DO # 2DUP D0= ?LEAVE LOOP
                REPEAT  #> r> drop ;

: (UD,.)        ( ud -- a1 n1 )
                base @             \ get the base
                dup  10 =          \ if decimal use comma every 3 digits
                swap  8 = or       \ or octal   use comma every 3 digits
                4 + (xUD,.) ;      \ display commas every 3 or 4 digits

: UD,.R         ( ud l -- )        \ right justified, with ','
                >R (UD,.) R> OVER - SPACES TYPE ;

: U,.R          ( n1 n2 -- )       \ display double unsigned, justified in field
                0 SWAP UD,.R ;

: UD.           ( ud -- )          \ display double unsigned
                0 UD,.R ;

: UD.R          ( ud l -- )        \ right justified, WITHOUT ','
                >R 16 (xUD,.) R> OVER - SPACES TYPE ;

: (D.#)         ( d1 n1 -- a1 n1 ) \ display d1 with n1 places behind DP
                >R <#              \ n1=negative will display'.' but no digits
                R> ?DUP            \ if not zero, then display places
                IF      0 MAX 0 ?DO # LOOP [char] . HOLD
                THEN    #S #> ;

: D.R.#         ( d1 n1 n2 -- )    \ print d1 in a field of n1 characters,
                                   \ display with n2 places behind DP
                SWAP >R (D.#) R> OVER - SPACES TYPE ;

: .R.1          ( n1 n2 -- )       \ print n1 right justified in field of n2
                0 SWAP 1 D.R.# ;   \ display with one place behind DP

\ BINARY double number display with commas

: RADIX:        ( n1 -<name>- )
                CREATE  , DOCOL ,  !CSP ]
                DOES>   BASE @ >R
                        LCOUNT BASE ! EXECUTE   \ run headerless definition
                        R> BASE ! ;

2 RADIX: BUD,.R ( ud width -- )   UD,.R ;
2 RADIX: BU,.R  ( n1 width -- )   U,.R ;
2 RADIX: B.     ( n1 -- )       1 U,.R ;

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\       TRIM (forget) primitives
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

: (trim)        ( addr1 addr2 -- addr1 addr3 )
                begin @ 2dup u> until ;

: trim          ( addr voc -- )
                tuck (trim) nip swap ! ;

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\       Execution chain words
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

variable chain-link             \ linked list of chains
         chain-link off

: new-chain     ( -- )
                here app-here u>                \ in system space
                sys-warning? and                \ and we want warnings
                abort" Use NEW-SYS-CHAIN in system dictionary!"
                create 0 , ['] noop , chain-link link, ;

: new-sys-chain ( -- )
                here app-here u> 0=             \ NOT in system space
                abort" Use NEW-CHAIN in application dictionary!"
                create 0 , ['] noop , ;

in-system

: .chain        ( chain -- )
                dup @ 0=
                if      drop ." Empty"
                else    begin   @ ?dup
                        while   dup cell+ @
                                dup >name name> ['] [unknown] <>
                                if      >name
                                        dup nfa-count nip 3 + ?cr
                                        getxy drop 24 <
                                        cols       48 > and
                                        if      24 col
                                        then    .id
                                else    drop
                                then    start/stop
                        repeat
                then    ;

: .chains       ( -- )          \ display the contents of all chains
                chain-link
                begin   @ ?dup
                while   dup 2 cells -
                        cr dup body> .NAME 23 col space .chain
                repeat  cr ;

in-application

: do-chain      ( chain_address -- )
                begin   @ ?dup
                while   dup>r           \ make sure stack is clean during
                        cell+ @
                        execute         \ execution of the chained functions
                        r>              \ so parameters can be passed through
                repeat  ;               \ the chain if items being performed

0 value transient-ptr                   \ only warn "in sys" if not transienting

: ?sys-chain    ( chain_address cfa -- chain_address cfa )
                over app-here u> 0=             \ chain NOT in system space?
                over app-here u> and            \ and cfa in system space?
                sys-warning? and                \ and we want warnings
                transient-ptr 0= and            \ if not in transient area
                if      cr ." ***System word: "
                        2dup .name
                        ." added to chain: " body> .name
                then    ;

: noop-chain-add ( chain_address -- addr )      \ add chain item,
                                                \ return addr of cfa added
                begin   dup @
                while   @
                repeat  here swap ! 0 , here ['] noop , ;

: chain-add     ( chain_address -<word_to_add>- )   \ for normal forward chains
                ' ?sys-chain >r
                begin   dup @
                while   @
                repeat  here swap ! 0 , r> , ;

: chain-add-before ( chain_address -<word_to_add>- )
        \ for reverse chains like BYE
                ' ?sys-chain >r
                here over @ , r> , swap ! ;

\ define some of the chains we need

new-chain initialization-chain  \ chain of things to initialize
new-chain         unload-chain  \ chain of things to de-initialize
new-chain         forget-chain  \ chain of types of things to forget
new-chain    post-forget-chain  \ chain of types of things to forget
new-chain          mouse-chain  \ chain of things to do on mouse down
new-chain      semicolon-chain  \ chain of things to do at end of definition
new-chain       forth-io-chain  \ chain of things to to to restore forth-io
new-chain        number?-chain  \ chain of number conversion options
new-chain          ledit-chain  \ line editor function key chain
new-chain            msg-chain  \ chain of forth key messages
new-chain      forth-msg-chain  \ chain of forth window message

: _do-;chain    ( -- )
                semicolon-chain do-chain ;

' _do-;chain is do-;chain

\ clear all file handle during startup

variable handles-list

: init-handles  ( -- )
                handles-list
                begin   @ ?dup
                while   dup 3 cells - off       \ reset VALUE's handle
                repeat  ;

initialization-chain chain-add init-handles

\ handles can be treated like values, but they are auto zeroed at startup

: handle        ( -<name>- )   \ handles are automatically zeroed during
                ( -- hndl )     \ Win32Forths startup initialization
                0 value
                handles-list link, ;
                
:noname         ( -- )                  \ chain for cleanup
                unload-chain do-chain ; is unload-forth \ install in kernel word


\ ------------------------------- Memory Management functions ------------------------
\
\ Malloc data structure in dynamically allocated memory
\ beta 2.0A 06/09/2002 21:54:51 arm major modifications

\ [malloc_next][heapaddress][mem_type][malloced_memory][extra_cell]
\                                     |
\                                     * returns this address on allocate *
\                                      this is the "address"
\ Changes:
\   All memory calls are now to Windows heap functions
\   Length field has been discarded
\   Heap address is included
\   Currently, only the process heap is used. Only
\     ALLOCATE and REALLOC need to be modified to work against
\     another heap.
\   mem_type is currently unused, will be for pe header
\ see functions in kernel


: (tot-malloc)  ( n link-addr -- n' )           \ add in a single entry's byte count
                mHeapSize malloc-adjlen - + ;   \ get, adjust, increment

: tot-malloc    ( -- n )                        \ count allocated bytes
                (memlock)
                0                               \ run thru malloc chain
                ['] (tot-malloc) malloc-link do-link 
                (memunlock) ;

: (.mallocs)    ( link-addr -- )                \ display one line
                dup rel>abs h.8 ."   " dup link>mem h.8
                dup mHeapSize malloc-adjlen - 10 u,.r
                ."   " link>haddr @ h.8 cr ;

: .mallocs      ( -- )                          \ display all dynamically allocated buffers
                cr
                ." Abs-Addr  Rel-Addr     Bytes  HeapAddr  Type" cr
                s" --------  --------  --------  --------  ----" 2dup type cr
                (memlock)
                ['] (.mallocs) malloc-link do-link
                (memunlock)
                type cr ." Total allocated   " tot-malloc 10 u,.r cr ;

\ --------------------------- End Memory Management functions ------------------------

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ A super version of number that detect the 0x00 'C' style of hex numbers
\ as well as ascii characters in the 'A' format.
\ A HEX number ending in 'L' automatically has the 'L' removed.  This is
\ done so Forth can accept 0x1234L format numbers as they are encountered
\ in 'C' header files.
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

: new-number?   ( a1 n1 f1 -- d1 TRUE | a1 n1 FALSE )
                dup ?exit drop
                2dup number?
                if      2swap 2drop TRUE
                else    2drop FALSE
                then    ;

number?-chain chain-add new-number?             \ first item in NUMBER? chain

\ October 19th, 2000 - 9:39 tjz
\ per a suggestion from Anton Ertl <anton@a0.complang.tuwien.ac.at>, I have
\ modified 0xNUMBER? to allow the following syntax;
\
\ old way still works                   new way also works
\       0x-23                                 -0x23
\
\ old ways didn't work anyway           new way works
\       '-a' -'a'                             -'a'
\

: 0xNUMBER?     { adr len flg \ adr2 len2 -- d1 TRUE | a1 n1 FALSE }
        flg                               \ leave if already converted
        IF      adr len flg
                EXIT
        THEN
        adr c@ ascii - = dup >r                 \ if preceeded by '-'
        IF      adr len 1 /string               \ then strip it off
        ELSE    adr len
        THEN    to len2 to adr2                 \ new string to use
        adr2 c@ ascii ' =                       \ test for leading tick (')
        IF      len2 3 =                        \ and length is three
                adr2 2 + c@ ascii ' = and       \ and trailing tick (')
                IF      adr2 1+ c@ 0 TRUE
                ELSE    adr2 len2 flg
                THEN
        ELSE    base @ >r                       \ preserve base
                adr2 len2
                adr2 2 S" 0X" compare 0=         \ if start with 0x
                IF      hex                     \ use hex number base
                        2 /string               \ remove 0x
                        2dup + 1- c@ ascii L =  \ if have trailing 'L'
                        IF      1- 0 max        \ then remove that also
                        THEN
                ELSE    adr2 1 S" $" compare 0=  \ if start with $
                        IF      hex             \ use hex number base
                                1 /string       \ remove $
                        THEN
                THEN
                FALSE new-number?
                r> base !               \ restore base
        THEN
        IF      r>                      \ converted, so get '-' flag
                IF      DNEGATE         \ if true, then negate result
                THEN    TRUE            \ and return a true flag
        ELSE    r>drop                  \ didn't convert, discard '-' flag
                2drop                   \ discard the string
                adr len                 \ replace with original string
                FALSE                   \ and return a false flag
        THEN    ;

number?-chain chain-add 0xNUMBER?

\ Each number conversion check on the "number?-chain" checks to see if the
\ string a1,n1 has already been converted (f1=TRUE), if it has, then they
\ simply exit.  If string a1,n1 hasn't been converted, then they attempt
\ conversion.  If conversion fails, they return the original string a1,n1
\ and a f1=FLASE flag.  If conversion succeeds, then they return their
\ result, either on the data stack, or the floating point stack, and return
\ f1=TRUE to say conversion was completed properly.  In the case of a
\ floating point conversion, a global flag is set to be used later by
\ NUMBER, when it goes to compile the number.  This is needed since
\ floating point numbers are returned on the floating point stack instead
\ of on the data stack.  The chain number conversion technique allow number
\ conversion to be easily extended to support additional forms of number
\ conversion.

: _discard-number ( d1 -- )     \ discard a converted number
                2drop  ;

defer discard-number
   ' _discard-number is discard-number

: super-number? ( a1 n1 -- d f1 )
                FALSE to double?
                FALSE number?-chain do-chain ;

: new-number    ( ^str -- d )           \ an extensible version of NUMBER
                count find-buffer place
                find-buffer ?uppercase 
                count super-number? 0= ?missing ;

' new-number is number                  \ replace normal number conversion
                                        \ with the new chain scheme

\ Limited support for the '#define' statment from 'C'

: #define ( -<name expression>- )
        >in @ >r
        bl word drop bl word 1+ c@ '"' =
        r> >in !
        if      create
                bl word count 1 /string 2dup '"' scan nip -
                here over 1+ allot place 0 c, align
        else    0 constant  -4 allot
                interpret ,
        then    ;

: get-viewfile  ( cfa -- cfa loadfile TRUE | FALSE )  \ find source for a word
                dup >name >r
                loadfile
                begin   @ ?dup
                while   dup r@ u<
                        if      cell+ TRUE
                                r>drop
                                EXIT            \ leave loop here
                        then
                repeat  drop r>drop FALSE ;

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\       LONG counted string support
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
((
\ March 2nd, 2000 - 11:14 tjz
You must not store more than 4k beyond where you currently are into the
stack, and since LMAXCOUNTED is passed to LOCALALLOC: a bunch of times in
WinEd, we must limit long counted strings to significantly less than
the 4k limit. So, I am limiting long counted strings to 3k bytes.
))

1024                 CONSTANT LMAXCOUNTED    \ lines can be 1k characters long
LMAXCOUNTED 2 CELLS+ CONSTANT LMAXSTRING
                            \ room for leading cell count and trailing cell null

: "LCLIP"       ( a1 n1 -- a1 n1 )   \ clip a string to between 0 and LINE-MAX
        LMAXCOUNTED MIN 0 MAX ;

: LPLACE        ( addr len dest -- )
        SWAP "LCLIP" SWAP
        2DUP 2>R
        CELL+ SWAP MOVE
        2R> ! ;

: +LPLACE       ( addr len dest -- ) \ append string addr,len to LONG counted
                                     \ string dest
        >R "LCLIP" LMAXCOUNTED R@ @ - MIN R>   \ clip total to LINE-MAX string
        2DUP 2>R
        LCOUNT CHARS + SWAP MOVE
        2R> +! ;

: C+LPLACE    ( c1 a1 -- )    \ append char c1 to the LONG counted string at a1
        1 OVER +! LCOUNT + 1- C! ;

: +LNULL        ( a1 -- )       \ append a NULL just beyond the counted chars
        LCOUNT + 0 SWAP C! ;

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\       Some case insensitive version of search and compare
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

\ enhanced caps-search for source string > 255 bytes
\ search for t-adr,t-len within string s-adr,s-len. f1=true if string was found
: CAPS-SEARCH { s-adr s-len t-adr t-len \ t-buf t-str -- adr len flag }
        MAXSTRING localalloc: t-str
        s-len cell+ ALLOCATE 0=
        IF      to t-buf                    \ make a buffer big enough for s-adr
                t-adr t-len t-str place
                t-str count upper
                s-adr t-buf s-len move
                t-buf s-len upper
                t-buf s-len t-str count search
                IF      nip                     \ discard found address
                        s-len swap -            \ offset where string was found
                        s-adr s-len rot /string
                                      \ location of found string in original buf
                        TRUE
                ELSE    2drop
                        s-adr s-len FALSE
                THEN
                t-buf FREE drop 
        ELSE    s-adr s-len FALSE             \ failed, couldn't allocate buffer
        THEN    ;
((
\ search for sa2,sn2 within string sa1,sn1. f1=true if string was found
: CAPS-SEARCH   { sa1 sn1 sa2 sn2 \ st1 st2 -- a3 n3 f1 }
                MAXSTRING LocalAlloc: st1
                MAXSTRING LocalAlloc: st2
                sa1 sn1 st1 place  st1 count upper
                sa2 sn2 st2 place  st2 count upper
                st1 count st2 count search
                IF      >r st1 1+ - sa1 + r> true
                ELSE    2drop sa1 sn1 false
                THEN    ;
))
\ COMPARE compares two strings, ignoring case. The return value is:
\
\        0 = string1 = string2
\       -1 = string1 < string2
\        1 = string1 > string2

: CAPS-COMPARE  { sa1 sn1 sa2 sn2 \ st1 st2 -- f1 }
                MAXSTRING LocalAlloc: st1
                MAXSTRING LocalAlloc: st2
                sa1 sn1 st1 place  st1 count upper
                sa2 sn2 st2 place  st2 count upper
                st1 count st2 count compare ;

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\       Compiler assistance to detect words already define or undefined
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

: [DEFINED]     ( <aword> -- f1 )
                DEFINED nip 0<> ; IMMEDIATE

: [UNDEFINED]   ( <aword> -- f1 )
                DEFINED nip 0= ; IMMEDIATE
                
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\       Keyboard Mask Constant, MUST MATCH THOSE IN TERM.H !!
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

  0x10000   constant function_mask      \ function key maks
  0x20000   constant special_mask       \ special keyboard key mask
  0x40000   constant control_mask       \ control key mask
  0x80000   constant shift_mask         \ shift key mask
  0x100000  constant alt_mask           \ alt key mask
  0x200000  constant mouse_mask         \ mouse operations
  0x400000  constant menu_mask          \ menu operations
  0x002000  constant proc_mask          \ procedure base mask
  0x1000000 constant double_mask        \ double click mask
  0x2000000 constant down_mask          \ mouse down mask
  0x4000000 constant up_mask            \ mouse up mask

 proc_mask value    proc_next           \ next procedure value

: newproc       ( -<proc_name> )        \ define a unique procedure key code
                ( -- n1 )               \ return the procedures key code
                proc_next constant
                1 +to proc_next ;

\ As used in Win32Forth, procedures are just constants, automatically
\ generated with incremented values that are used as keys generated by menu
\ operations, that are later detected in a keyboard interpreters CASE
\ statment to invoke a function. They start with 80000 HEX.

newproc IDK_BASEPROC    \ dummy base procedure showing syntax
                        \ procedure key constants should start with IDK_
                        \ and should be defined and used in UPPERCASE


\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\       Additional XCALL words
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

: cursorinview  ( -- )    \ make sure cursor is visible in the window
                ;

: fgbg!         ( forground background -- )
                27 xcall drop ;

: fg@           ( -- foreground )
                73 xcall ;

: bg@           ( -- background )
                74 xcall ; 

: pushkey       ( c1 -- )       \ push c1 into the keyboard input stream
                28 xcall drop ;

: "pushkeys     ( a1 n1 -- )    \ push the characters of string a1,n1
                0max 127 min bounds
                ?do     i c@ pushkey
                loop    ;

: &the-screen   ( -- a1 )       \ get the forth relative address of the users
                                \ console screen memory buffer
                29 xcall abs>rel ;

: charWH        ( -- width height )     \ get the width and height of the
                                        \ current screen font
                30 xcall word-split swap ;

: SetcharWH     ( width height -- )     \ set the size of the console characters
                71 xcall drop ;

: shiftmask     ( -- mask )
                31 xcall ;

: ?shift        ( -- f1 )       \ return true if shift is down
                shiftmask shift_mask and 0<> ;

: ?control      ( -- f1 )       \ return true if control is down
                shiftmask control_mask and 0<> ;

: setcolrow     ( cols rows -- )                \ set the console size
                13 * swap 8 * swap 32 xcall drop ;

synonym set-consize setcolrow

4 PROC SetWindowPos
: set-conpos    ( x y -- )                      \ set the console position
                2>r ( SWP_NOSIZE ) 1 0 0 2r> ( HWND_TOP ) 0 conHndl
                call SetWindowPos drop ;
\                33 xcall drop ;

1 PROC SetCursor
2 PROC LoadCursor
: set-pointer   ( pointer-identifier -- )       \ set the pointer shape
\                34 xcall drop ;
                null call LoadCursor call SetCursor drop ;

: set-cursor    ( cursor-height -- )            \ set the cursor height
                35 xcall drop ;

: get-cursor    ( -- cursor-height )            \ get the cursor height
                36 xcall ;

: big-cursor    ( -- )                          \ set a block cursor
                charWH nip set-cursor ;

2 value norm-height                             \ hold the norm cursor height

: norm-cursor   ( -- )                          \ set a normal cursor
                norm-height set-cursor ;

0 value havemenu?

: havemenu!     ( flag -- )
                dup to havemenu?
                37 xcall drop ;

1 PROC GetDC
: conDC         ( -- dc )               \ get the console device context
                conHndl call GetDC ;
\                38 xcall ;             \ removed

                                        \ bring up the printer page setup
: print-setup   ( window_handle -- PrintDC )
                39 xcall ;

: print-start   ( -- )                  \ start printing a new page & doc
                40 xcall drop ;

: print-page    ( -- )                  \ finish current page start new page
                41 xcall drop ;

: print-end     ( -- )                  \ finish printing page and doc
                42 xcall drop ;

: print-init    ( -- printDC )          \ initialize the printer, return DC
                0 43 xcall ;

                                        \ rls February 4th, 2002 - 20:24
: print-init2   ( bitmapped flags topage -- printDC )
                0 77 xcall ;            \ initialize the printer, return DC

: auto-print-init ( -- printDC )        \ initialize the printer, return DC
                1 43 xcall ;

                                        \ rls February 4th, 2002 - 5:47
: print-flags   ( -- flag )             \ true if selection radio button chosen
                78 xcall ;

: print-close   ( -- )                  \ close the printer
                44 xcall drop ;

\ if n1 >0 then n1 = printer resolution in DPI
\ if n1 <0 then n1 = device indepentant code, -1 = draft to -4 = high

: quality-print ( -- n1 )       \ return the print quality code
                57 xcall ;

: start-page    ( -- )
                58 xcall drop ;

: end-page      ( -- )
                59 xcall drop ;

: get-copies    ( -- n1 )
                60 xcall ;

: get-frompage  ( -- n1 )
                61 xcall ;

: get-topage    ( -- n1 )
                62 xcall ;



0 value saveconx
0 value savecony

2 PROC ShowWindow

: show-window   ( n -- )
                conHndl call ShowWindow drop ;

: hide-console  ( -- )
                saveconx ?exit
                getcolrow to savecony to saveconx
\                48 xcall drop ;
                ( SW_HIDE ) 0 show-window ;

: unhide-console ( -- )
                saveconx 0= ?exit
\                49 xcall drop                   \ turn console back on
                ( SW_SHOW ) 5 show-window
                saveconx savecony set-consize   \ resize to original size
                0 to saveconx
                0 to savecony ;

synonym show-console unhide-console

: normal-console ( -- )         \ un-minimizes a minimized console window
                ( SW_NORMAL ) 1 show-window ;
\                56 xcall drop ;

1 PROC SetFocus
: focus-console ( -- )
                conHndl call SetFocus drop ;

: setrowoff     ( n1 -- )               \ set the console row offset
                51 xcall drop ;

: getrowoff     ( -- n1 )               \ get the current console row offset
                52 xcall ;

: getmaxcolrow  ( -- maxcols maxrows )  \ get maximum window columns
                53 xcall word-split ;

: setmaxcolrow  ( maxcols maxrows -- )  \ set the saved screen area and clear
                16384 min 20 max swap           \ clip rows
                  256 min 26 max swap           \ clip columns
                54 xcall drop ;

\ if n1 >0 then n1 = printer resolution in DPI
\ if n1 <0 then n1 = device indepentant code, -1 = draft to -4 = high

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\       new definition of key to support minimal mouse down events
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

0 value mousex
0 value mousey
0 value mouseflags
  defer do-mabort

0 value mkstlin         \ hold the status of the current marked console text
0 value mkstcol
0 value mkedlin
0 value mkedcol
0 value mkorlin
0 value mkorcol

: mark-start    ( -- )          \ set a new start of marked console text
                mousex charWH >r /             to mkstcol
                mousey        r> / getrowoff + to mkstlin
                mousex charWH >r /             to mkedcol
                mousey        r> / getrowoff + to mkedlin
                mkstlin to mkorlin
                mkstcol to mkorcol
                mkstlin mkstcol mkedlin mkedcol markconsole ;

: mark-end      { \ lin col -- } \ set a new end of marked console text
                mousex charWH >r /             to col
                mousey        r> / getrowoff + to lin
                lin mkorlin =                   \ same line but earlier in line
                col mkorcol <= and
                lin mkorlin <  or               \ or on an earlier line
                if      lin to mkstlin
                        col to mkstcol
                        mkorlin to mkedlin
                        mkorcol to mkedcol
                else    lin to mkedlin
                        col to mkedcol
                        mkorlin to mkstlin
                        mkorcol to mkstcol
                then
                mkstlin mkstcol mkedlin mkedcol markconsole ;

: mark-all      ( -- )          \ makr all console text
                0 to mkstlin
                0 to mkstcol
                0 to mkedcol
                getxy nip getrowoff + 1+ to mkedlin
                mkstlin mkstcol mkedlin mkedcol markconsole ;

: mark-none     ( -- )          \ clear the marking of any console text
                0 to mkstlin
                0 to mkstcol
                0 to mkedcol
                0 to mkedlin
                mkstlin mkstcol mkedlin mkedcol markconsole ;

: marked?       ( -- f1 )       \ return TRUE if any text is marked
                mkstlin mkedlin <>
                mkstcol mkedcol <> or ;

: _do-mabort    ( -- )
                cr ." Aborted by Mouse!"
                abort ;

' _do-mabort is         do-mabort

: ?mouse_abort  ( -- )          \ abort if both mouse buttons are down
                mouseflags 3 and 3 =
                if      do-mabort
                then    ;

mouse-chain chain-add ?mouse_abort

defer auto_key  ' noop is auto_key      \ default to nothing
defer auto_key? ' noop is auto_key?     \ default to nothing

: _mouse-click  ( -- )
                mouse-chain do-chain ;

defer mouse-click       ' _mouse-click  is mouse-click

: process-mouse ( ekey -- )
        dup down_mask and       \ if mouse is DOWN
        IF      dup>r
                mouse_mask -1 xor and
                down_mask  -1 xor and to mouseflags
                ekey?
                IF      ekey word-split
                        to mousey               \ set y
                        to mousex               \ set x
                                                \ is mouse UP and DOWN
                        mouseflags 3 and 1 =    \ left mouse button
                        IF      r@ up_mask and  \ both masks is a mousemove
                                ?shift or
                                IF      mark-end
                                ELSE    mark-start
                                THEN
                        THEN
                        mouseflags 3 and 2 =    \ right mouse button
                        IF      mouse-click
                        THEN
                THEN    r>drop
        ELSE    dup up_mask and  \ is mouse UP
                IF      mouse_mask -1 xor and
                        up_mask    -1 xor and to mouseflags
                        ekey?
                        IF      ekey word-split
                                to mousey       \ set y
                                to mousex       \ set x
                                mkstlin mkstcol
                                mkedlin mkedcol d= \ pos NOT changed?
                                IF      mouse-click
                                ELSE    mark-end
                                THEN
                        THEN
                ELSE    mouse_mask -1 xor and to mouseflags
                        ekey?
                        IF      ekey word-split
                                to mousey       \ set y
                                to mousex       \ set x
                                mouse-click
                        THEN
                THEN
        THEN    ;

: _mkey         ( -- c1 ) \ get a key from the keyboard, and handle mouse clicks
        auto_key
        BEGIN   ekey
                dup  mouse_mask and             \ mouse operation
                IF      process-mouse
                        false
                THEN    ?dup
        UNTIL ;

' _mkey is key

: _mkey?        ( -- c1 ) \ check for key from keyboard, and handle mouse clicks
                ekey?
                dup mouse_mask and
                if      ekey    drop            \ discard waiting key
                        process-mouse
                        false
                then    auto_key? ;

' _mkey? is key?

: ?mabort       ( -- )      \ give mouse a chance to recognize button press
                WINPAUSE ;

: _mcls         ( -- )
                _cls
                mark-none ;

' _mcls is cls

: _memit        ( c1 -- )               \ allow mouse to abort EMIT
                ?mabort _emit ;

' _memit is emit

: _mtype        ( a1 n1 -- )            \ allow mouse to abort TYPE
                ?mabort "CLIP" _type ;

' _mtype is type

: _mcol         ( n1 -- )
                _col ;

' _mcol is col

: _m?cr         ( n1 -- )
                _?cr ;

' _m?cr is ?cr

: _mcrtab       ( -- )
                crtab ;

' _mcrtab is cr

0 value remote-io?                    \ is I/O being directed to remote console?

defer@ accept value defaultAccept

: _basic-forth-io  ( -- )               \ reset to Forth IO words
                unhide-console
                sizestate 1 =           \ if window is SIZE_MINIMIZED
                if      normal-console
                then
                ['] _memit      is emit
                ['] _mtype      is type
                ['] _mcrtab     is cr
                ['] _m?cr       is ?cr
                ['] _mkey       is key
                ['] _mkey?      is key?
                ['] _mcls       is cls
                [']  cls        is page
                ['] _gotoxy     is gotoxy
                ['] _getxy      is getxy
                ['] _getcolrow  is getcolrow
                ['] _mcol       is col
                defaultAccept   is accept
                focus-console
                tabing-off
                FALSE to remote-io? ;

defer basic-forth-io

' _basic-forth-io is basic-forth-io

forth-io-chain chain-add basic-forth-io

: forth-io      ( -- )
                forth-io-chain do-chain ;

forth-io        \ set the default I/O words

\ March 11th, 2002 - 22:13 Added some documentation for alias and synonym jp
