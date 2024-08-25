\ $Id: debug.f 1.1 1994/04/01 07:52:43 andrew Exp $

\ only forth definitions


: .()  ( n -- )  ." (" 0 .r ." ) " ;


\ vocabulary bug          also bug also definitions


\ -------------------- Variables --------------------

variable ip  0 ,        \ ip & contents of current breakpoint
variable ip0            \ ip at start of word
variable rtop           \ top of return stack
variable nesting        \ nesting level
variable watching       \ watch word

: patch  ( cfa -- )
        ip @ @ ip cell+ !       \ save old word
        ip @ ! ;                \ patch in trace word


\ -------------------- Advance IP --------------------

: colon?        ( cfa -- f ) @ ['] patch @ = ;

: code?         ( cfa -- f ) dup @ swap cell+ = ;

: variable?     ( cfa -- f ) @ ['] ip @ = ;

hex

: does>?        ( cfa -- f ) @ @ 0E8909090 = ;

decimal

: ?JUMP  ( ip f -- ip' )  IF  CELL+ dup @ +  ELSE  2 CELLS +  THEN ;

: <STRING>  ( ip -- ip' )   CELL+ COUNT + 1+ ALIGNED ;

: <EXIT>  ( ip -- ip' )
        drop nesting @ 0>
        if      rtop @ abs>rel  ( unnest )
                -1 nesting +!
        else    ip0 @   ( done, reset ip for next time )
                nesting off
        then ;

: <EXITP>  ( ip -- ip' )
        drop nesting @ 0>
        if      lp @ cell+ @ abs>rel  ( unnest )
                -1 nesting +!
        else    ip0 @   ( done, reset ip for next time )
                nesting off
        then ;

: next  ( -- )
   IP @   DUP @ CASE
     ['] LIT      OF  2 CELLS +                         ENDOF
     ['] COMPILE  OF  2 CELLS +                         ENDOF
     ['] BRANCH   OF  TRUE ?JUMP                        ENDOF
     ['] _again   OF  TRUE ?JUMP                        ENDOF
     ['] _repeat  OF  TRUE ?JUMP                        ENDOF
     ['] ?BRANCH  OF  OVER 0= ?JUMP                     ENDOF
     ['] _until   OF  OVER 0= ?JUMP                     ENDOF
     ['] _while   OF  OVER 0= ?JUMP                     ENDOF
     ['] (DO)     OF  2 CELLS +                         ENDOF
     ['] (?DO)    OF  OVER 3 PICK = ?JUMP               ENDOF
     ['] (LOOP)   OF  1 RTOP @ +OV? NOT ?JUMP           ENDOF
     ['] (+LOOP)  OF  OVER RTOP @ +OV? NOT ?JUMP        ENDOF
     ['] _OF      OF  OVER 3 PICK <> ?JUMP              ENDOF
     ['] (S")     OF  <STRING>                          ENDOF
     ['] (C")     OF  <STRING>                          ENDOF
     ['] (.")     OF  <STRING>                          ENDOF
     ['] (ABORT") OF  <STRING>                          ENDOF
     ['] _EXIT    OF  <EXIT>                            ENDOF
     ['] UNNEST   OF  <EXIT>                            ENDOF
     SWAP CELL+ SWAP
   ENDCASE   IP ! ;


\ -------------------- Trace Commands --------------------

: run-forth
        begin   cr ." forth>  " query  source nip
        while   interpret
        repeat ;

: dbgnest ( -- )
        ip @ @ dup colon?
        if      >body ip !
                1 nesting +!
        else    dup does>?
                if      ." DOES> nesting "
                        @ 2 cells + ip !
                        1 nesting +!
                else    drop ." Can't nest "
                then
        then ;

: dbgunnest ( -- )        \ not valid inside a loop or if >R has been used!
        rtop @ abs>rel ip !
        -1 nesting +! ;

: help  cr
." C-continue,  D-done,  N-nest,  U-unnest,  L-locate,  F-forth,  Q-quit " ;


: locate  ( where am I? )
        ip @ dup
        begin   dup body> colon?
                if  dup body> >name .id ( .name ) - CELL / .()  exit then
                cell -
                over 50 cells - over =
        until
        2drop ." Don't know! " ;

: .wordtype     ( -- )
        ip @ @  dup colon?
        if      drop ."    : " exit
        then    dup code?
        if      drop ." code " exit
        then    dup variable?
        if      drop ." var  " exit
        then    dup does>?
        if      drop ." does " exit
        then    drop 5 spaces ;

\ -------------------- Trace Breakpoint --------------------

: trace  ( -- )
        r>
        r@ rtop !
        cell - dup >r
        abs>rel ip @ <> abort" trace error"
        ip 2@ !  ( restore )
        .s
        watching @ ?dup if  5 spaces execute    then cr
        .wordtype
        ip @ @ .name
        ."   -->  " key upc
        case
          ascii C of  ip0 @ ip ! nesting off    endof
          ascii D of  ip off  exit              endof
          ascii N of  dbgnest                   endof
          ascii U of  dbgunnest                 endof
          ascii F of  run-forth                 endof
          ascii L of  locate                    endof
          ascii Q of  ip off  ." unbug" abort   endof
          ascii ? of  help                      endof
          >r next ( default ) r>
        endcase

        [ last @ name> ] literal patch ;        \ patch in trace


\ -------------------- Initialize Debugger --------------------

\ forth definitions

: watch   ' watching ! ;
: nowatch   watching off ;

: unbug  ( -- )
        ip @ if  ip 2@ !  ip off  then ;

synonym unbp unbug

: debug ( -<name>- )    \ set a breakpoint
        unbug
        '
        dup  colon?
        over does>? or 0= abort" Must be a : or DOES> definition"
        dup  colon?
        if      >body
        else    ." DOES> nesting "
                @ 2 cells +
        then    dup ip0 ! ip !
        ['] trace patch
        nesting off ;

synonym bp debug

: dbg   ( -<name>- )    \ debug a word now
        >in @ debug >in ! ;

\ only forth also definitions


