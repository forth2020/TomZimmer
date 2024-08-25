\ $Id: debug.f 1.1 1994/04/01 07:52:43 andrew Exp $
\ Changes February 14th, 2002 - 1:36 rls
\ debug.f beta 3.9K 2002/10/19 arm CONSTANT? to TRUE

cr .( Loading Debugger...)

only forth also definitions also hidden

32 value max.rstack
 0 value remote-debug?
 0 value in-breakpoint?

: colon?        ( cfa -- f )            \ is cfa a colon definition?
                @ ['] hex @ = ;

: code?         ( cfa -- f )            \ is cfa a code word?
                dup @ swap cell+ = ;

defer debug-.s
defer word-watch ( address -- )
                ' drop  is word-watch

defer stack-watch ( -- )
                ' noop  is stack-watch

defer $watch     ( line filename -- )
                ' 2drop is         $watch
                ' 2drop is-default $watch

defer foreground-console ( -- )
                ' noop is         foreground-console
                ' noop is-default foreground-console

0 value start-watch?
0 value watched-cfa

: cfa-watch     ( cfa -- )                      \ try to display the source for CFA
                with-source?
                if      dup to watched-cfa
                        dup >name name> ['] [unknown] <>
                        if      dup
                                get-viewfile >r swap >view @
                                dup 0> r> and           \ found and not console
                                if      2dup swap $watch
                                then    2drop
                        then
                then    drop ;

: key-breaker   ( -- )
                noop ;

: L.ID  ( nfa len -- )  swap dup .id  c@ 31 and 1+ - spaces  ;

new-chain dbg-next-cell         ( ip cfa -- ip' cfa )
new-chain dbg-nest-chain        ( cfa flag -- cfa false | true )
new-chain .word-type-chain

2 #vocabulary bug also bug also definitions

\ Numeric printing words that do NOT use PAD !!!

: CHR>ASC   ( n -- char )
        dup 9 > 7 and + 48 + ;

: &   ( n1 -- char n2 )
        0 base @ um/mod swap chr>asc swap ;

: &S   ( n1 -- c1 c2 ... 0 )
    BEGIN
        & dup 0=
        UNTIL ;
        
: <&   ( n -- 0 n )
        0 swap ;
        
: &>  drop ;

: &TYPE   ( 0 c1 c2 ... -- )
        BEGIN ?dup
    WHILE emit
    REPEAT ;

\ externally usable number display words that don't use PAD

: U%.   ( u -- )
        <& &s &> &type space ;
        
: %.    ( n -- )
        dup 0<
        IF      abs ascii - emit
        THEN u%. ;

: 0%.R  ( n -- )        \ display signed right justified except in HEX,
                        \ then display unsigned
        base @  0x10 <>
        if      dup 0<
                IF      abs ascii - emit
                THEN
        then    <& &s &> &type ;

: H%.   ( n -- )
        base @ swap hex u%. base ! ;

: %.S   ( ... -- ... )
        ?stack depth .smax @ min dup
        IF      ." [" depth 1- 0%.r ." ] "
        BEGIN
                dup pick 0%.r
            base @ 0x10 =
            IF  ." h" THEN
            space
            1- dup 0=
                UNTIL
        ELSE    ."  empty "
        THEN
        drop ;
        
\ -------------------- Variables --------------------

variable ip  0 ,        \ ip & contents of current breakpoint
variable ip0            \ ip at start of word
variable rtop           \ top of return stack
variable nesting        \ nesting level

0 value ?dbg-cont       \ are we stepping contiuously
0 value 'trace          \ address of the trace function

: patch  ( cfa -- )
        ip @ @ 'trace <>                \ if there is NOT a trace in the target
        if      ip @ @  ip cell+ !      \ then save old word
        then
        ip @ ! ;                        \ patch in trace word

\ -------------------- Advance IP --------------------

: variable?     ( cfa -- f ) @ ['] ip    @ = ;

: defer?        ( cfa -- f ) @ ['] key   @ = ;

: execute?      ( cfa -- f )   ['] execute = ;

: constant?     ( cfa -- f ) @ ['] true  @ = ;

: does>?        ( cfa -- f ) @ @ 0x0E8909090 = ;

: m1cfa?        ( cfa -- f ) @ m1cfa = ;

: m0cfa?        ( cfa -- f ) @ m0cfa = ;

: ?JUMP  ( ip f -- ip' )  IF  CELL+ dup @ +  ELSE  2 CELLS +  THEN ;

: <STRING>  ( ip -- ip' )   CELL+ COUNT + 1+ ALIGNED ;

: <EXIT>  ( ip -- ip' )
        drop nesting @ 0>
        if      rtop @ abs>rel                          \ unnest level
                dup ?name ?dup
                if      ." Unnesting to: " dup .name cfa-watch
                then    -1 nesting +!
        else    ip0 @   ( done, reset ip for next time )
                nesting off
        then ;

: <EXITP>  ( ip -- ip' )
        drop nesting @ 0>
        if      lp @ abs>rel cell+ @ abs>rel            \ unnest level
                dup ?name ?dup
                if      ." Unnesting to: " dup .name cfa-watch
                then    -1 nesting +!
        else    ip0 @   ( done, reset ip for next time )
                nesting off
        then ;

: <EXITM>  ( ip -- ip' )
        drop nesting @ 0>
        if      lp @ abs>rel 2 cells+ @ abs>rel         \ unnest level
                -1 nesting +!
        else    ip0 @   ( done, reset ip for next time )
                nesting off
        then ;

: dbg-next  ( -- )
   IP @   DUP @ CASE
     ['] LIT      OF  2 CELLS+                                  ENDOF
     ['] &LOCAL   OF  2 CELLS+                                  ENDOF
        &FLIT     OF  CELLS/FLOAT CELLS+ CELL+                  ENDOF
     ['] (IS)     OF  2 CELLS+                                  ENDOF
     ['] COMPILE  OF  2 CELLS+                                  ENDOF
     ['] BRANCH   OF  TRUE ?JUMP                                ENDOF
     ['] _endof   OF  TRUE ?JUMP                                ENDOF
     ['] _again   OF  TRUE ?JUMP                                ENDOF
     ['] _repeat  OF  TRUE ?JUMP                                ENDOF
     ['] leave    of  drop rp@ 5 cells+ @ abs>rel               ENDOF
     ['] ?leave   of  over
                      if    drop rp@ 5 cells+ @ abs>rel
                      else  cell+ then                          ENDOF
     ['] ?BRANCH  OF  OVER 0= ?JUMP                             ENDOF
     ['] _until   OF  OVER 0= ?JUMP                             ENDOF
     ['] _while   OF  OVER 0= ?JUMP                             ENDOF
     ['] (DO)     OF  2 CELLS +                                 ENDOF
     ['] (?DO)    OF  OVER 3 PICK = ?JUMP                       ENDOF
     ['] (LOOP)   OF  1 RTOP @ +OV? NOT ?JUMP                   ENDOF
     ['] (+LOOP)  OF  OVER RTOP @ +OV? NOT ?JUMP                ENDOF
     ['] _OF      OF  OVER 3 PICK <> ?JUMP                      ENDOF
     ['] (S")     OF  <STRING>                                  ENDOF
     ['] (C")     OF  <STRING>                                  ENDOF
     ['] (Z")     OF  <STRING>                                  ENDOF
     ['] (.")     OF  <STRING>                                  ENDOF
     ['] (ABORT") OF  <STRING>                                  ENDOF
     'EXIT        OF  <EXIT>                                    ENDOF
     ['] (;CODE)  OF  <EXIT>                                    ENDOF
     ['] UNNEST   OF  <EXIT>                                    ENDOF
     ['] EXITP    OF  <EXITP>                                   ENDOF
     ['] UNNESTP  OF  <EXITP>                                   ENDOF
     ['] EXITM    OF  <EXITM>                                   ENDOF
     ['] UNNESTM  OF  <EXITM>                                   ENDOF
  ['] init-locals OF  2 cells+                                  ENDOF
   DUP @ M1CFA =  IF  SWAP CELL+ SWAP ( skip an extra cell )    THEN
                      dbg-next-cell do-chain
                      SWAP CELL+ SWAP
   ENDCASE   IP ! ;


\ -------------------- Trace Commands --------------------

-1 value nextbreak
 0 value stack-top
 0 value return-top

defer debug-entry       ' noop is debug-entry   \ application init stuff
defer debug-exit        ' noop is debug-exit    \ application un-init stuff

create tib-save         MAXSTRING allot
create pocket-save      MAXSTRING allot
create here-save        MAXSTRING allot
create watch-buf        MAXSTRING allot
       watch-buf OFF                            \ empty to start

: perform-watch ( -- )
                state @ >r state off
                watch-buf count evaluate
                r> state ! ;

: do-watch      ( -- )
                watch-buf c@ 0= ?exit
                cr ." Watch-[" watch-buf count type ." ]: "
                ['] perform-watch catch drop ;

: run-forth
        here   here-save     MAXSTRING move
        pocket pocket-save   MAXSTRING move
        source tib-save swap MAXSTRING min move \ save SOURCE buffer
        (source) 2@ 2>r >in @ >r                \ save SOURCE and >IN
        begin   cr ." forth>  "
                query  source nip
        while   ['] interpret catch
                if      ." <- interpret error!" beep
                then
        repeat
        r> >in ! 2r> (source) 2!                \ restore SOURCE and >IN
        tib-save source move                    \ restore SOURCE buffer
        pocket-save pocket MAXSTRING move
        here-save   here   MAXSTRING move ;

: dbg-watch     ( -- )
        cr ." Enter a line to interpret after each instruction step is performed:"
        cr watch-buf 1+ MAXCOUNTED accept watch-buf c! ;

0 value emit-save
0 value type-save
0 value cr-save
0 value ?cr-save
0 value key-save
0 value key?-save
0 value cls-save
0 value page-save
0 value gotoxy-save
0 value getxy-save
0 value col-save
0 value tabing?-save
0 value left-margin-save
0 value indent-save
0 value x-save
0 value y-save


: _dbg-nest  ( cfa -- )
        dup cfa-watch                           \ try to watch debugging
        false dbg-nest-chain do-chain ?exit
        dup colon?                              \ colon definitions
        if      >body ip !
                1 nesting +! else
        dup does>?                              \ does> definitions
        if      ." DOES> nesting "
                @ dup
                begin   cell- dup >name
                        [ ' [unknown] >name ] literal <>
                        over ['] forth u< or
                until   cfa-watch
                2 cells + ip !
                1 nesting +! else
        dup defer?                              \ deferred words
        if      ." DEFER nesting " dup
                case    ['] type of     drop   type-save  endof
                        ['] emit of     drop   emit-save  endof
                        ['] cr   of     drop     cr-save  endof
                        ['] ?cr  of     drop    ?cr-save  endof
                        ['] key  of     drop    key-save  endof
                        ['] key? of     drop   key?-save  endof
                        ['] cls  of     drop    cls-save  endof
                        ['] page of     drop   page-save  endof
                      ['] gotoxy of     drop gotoxy-save  endof
                       ['] getxy of     drop  getxy-save  endof
                        ['] col  of     drop    col-save  endof
                                        swap >body @ swap
                endcase dup .name
                [ reveal ] _dbg-nest else
        dup execute?                            \ handle execute
        if      ." EXECUTE nesting " over .name
                drop dup _dbg-nest else
        dup m0cfa?                              \ methods type zero
        if      3 cells+ ip !
                1 nesting +! else
        dup m1cfa?                              \ methods type 1
        if      2 cells+ ip !
                1 nesting +! else
        drop ." Can't nest " beep
        then then then then then then ;

: dbg-nest      ( a1 -- )
                ip @ @ _dbg-nest ;

: dbg-unnest    ( -- )  \ not valid inside a loop or if >R has been used!
                rtop @ abs>rel here u<
                rtop @ abs>rel &origin &sys-separation + sys-here between or
                if      rtop @ abs>rel ip !
                        -1 nesting +!
                        rtop @ abs>rel ?name ?dup
                        if      ." Unnesting to: " dup .name cfa-watch
                        then
                else    ." Can't unnest " beep
                then    ;

: dbg-jump      ( -- )          \ set breakpoint beyond following branch word
                ip @ @
                case    ['] branch   of TRUE    endof
                        ['] _repeat  of TRUE    endof
                        ['] ?branch  of TRUE    endof
                        ['] _until   of TRUE    endof
                        ['] _while   of TRUE    endof
                        ['] (loop)   of TRUE    endof
                        ['] (+loop)  of TRUE    endof
                        ['] _of      of TRUE    endof
                        ['] _EXIT    of TRUE    endof
                                        FALSE swap
                endcase
                if      ip @ 2 cells+ to nextbreak
                        nesting off
                        dbg-next
                        0x0D pushkey
                else    beep
                then    ;

: #dbg-rstack   ( a1 a2 -- )
                cr ." RETURN STACK[" 2dup swap - cell / 1 .r ." ]: "
                over max.rstack cells+ umin     \ limit return stack entries
                swap over min
                ?do     i @ abs>rel ?name ?dup
                        if      i @ abs>rel here u<
                                if      dup >name nfa-count type
                                        i @ abs>rel
                                        swap >body - cell / 1- ." +" %.
                                else    h%.
                                then
                        else    i @ h%.
                        then    12 ?cr
          cell +loop    cr ;

: dbg-rstack    ( -- )
                rp@ 1 cells + rp0 @ #dbg-rstack ;

defer dbg-fstack        ' noop is         dbg-fstack
                        ' noop is-default dbg-fstack

: dbg-help
        cr ." ENTER/SPACE-single step"
        cr ." ESC/Q-quit, unbug, abort"
        cr ."  C-continuous step till key"
        cr ."  D-done, run the program"
        cr ."  F-forth commandline"
        cr ."  H-Hex display toggle"
        cr ."  J-Jump over next Word"
        cr ."  N-nest into this definition"
        cr ."  P-proceed to def again"
        cr ." ^P-proceed to this point again"
        cr ."  R-show Return stack"
        cr ."  U-unnest to definition above"
        cr ."  V-Vocabulary search order"
        cr ."  W-watch commands" ;

: .wordtype     ( -- )
        ip @ @ false .word-type-chain do-chain ?exit
                dup colon?
        if      drop ."    :  " exit
        then    dup execute?
        if      drop            exit
        then    dup code?
        if      drop ." code  " exit
        then    dup variable?
        if      drop ." var   " exit
        then    dup does>?
        if      drop ." does  " exit
        then    dup constant?
        if      drop ." const " exit
        then    dup defer?
        if      drop ." defer " exit
        then    dup  m0cfa?
                over m1cfa? or
        if      drop ." Meth: " exit
        then    drop 6 spaces ;

0 value debug-base

: .s-base       ( -- )
                base @ >r debug-base base ! %.s r> base ! ;

' .s-base is debug-.s

: base-toggle   ( -- )
                debug-base 0x10 =
                if      0x0A to debug-base
                else    0x10 to debug-base
                then    ;

: restore-io    ( -- )
                key-save 0= ?exit
                tabing?-save     to tabing?
                left-margin-save to left-margin
                indent-save      to indent
                 key-save defer@ key = ?EXIT
                emit-save        is emit
                type-save        is type
                  cr-save        is cr
                 ?cr-save        is ?cr
                 key-save        is key
                key?-save        is key?
                 cls-save        is cls
                page-save        is page
                gotoxy-save      is gotoxy
                getxy-save       is getxy
                 col-save        is col
                remote-debug?
                if      x-save y-save gotoxy    \ restore cursor position
                then
                0                to key-save    \ clear key saved cfa/flag
                tabbing-off
                FALSE to in-breakpoint? ;       \ flag for remote debugger

: debug-io      ( -- )
                defer@ key key-save = ?EXIT     \ leave already saved
                getxy to y-save to x-save
                defer@ emit    to   emit-save   \ save current contents
                defer@ type    to   type-save
                defer@ cr      to     cr-save
                defer@ ?cr     to    ?cr-save
                defer@ key     to    key-save
                defer@ key?    to   key?-save
                defer@ cls     to    cls-save
                defer@ page    to   page-save
                defer@ gotoxy  to gotoxy-save
                defer@ getxy   to  getxy-save
                defer@ col     to    col-save
                tabing?        to tabing?-save
                left-margin    to left-margin-save
                indent         to indent-save
                remote-debug? 0=
                IF      remote-io? 0=
                        IF      unhide-console
                                normal-console
                                forth-io
                        THEN        
                        ['] ekey       is key
                        ['] ekey?      is key?
                        16             to left-margin
                        -16            to indent
                        tabing-on
                else    ['] drop       is emit
                        ['] 2drop      is type
                        ['] noop       is cr
                        ['] drop       is ?cr
                        ['] ekey       is key
                        ['] ekey?      is key?
                        ['] noop       is cls
                        ['] noop       is page
                        ['] drop       is col
                then    ;

\ -------------------- Trace Breakpoint --------------------

: trace  ( ? -- )
        dup to stack-top
        TRUE to in-breakpoint?          \ flag for remote debugger
        debug-entry
        debug-io
        rp@ to return-top
\ *** Moved WinEd start from here, because we needed to clear the breakpoint
\ *** before executing some kernel words, to prevent recursive breakpoints
        r>
        r@ rtop !
        cell - dup >r

        abs>rel ip @ <>
        IF      true abort" trace error"
        THEN
        ip 2@ !  ( restore )
\ ***  Start WinEd after restoring breakpoint ---
        start-watch? ?dup               \ bring up WinEd if needed
        IF      cfa-watch
                false to start-watch?
        THEN
\ ***  Start WinEd after restoring breakpoint ---
        getxy drop 25 >         \ if column greater than 25
        IF      first-line
                cr 25 col
        THEN
        debug-.s
        stack-watch
        first-line
        do-watch
        0tab
        remote-debug? 0=
        IF      remote-io?
                IF      cr
                ELSE    _cr
                THEN
        THEN    .wordtype
        nesting @ 0max ?dup
        IF      0 do ." |" loop space
        THEN
        obj-save >r
        ip @ word-watch
        debugging? >r true to debugging?
        ip @ dup @ .execution-class drop
        r> to debugging?
        ip @ @ execute?
        IF      ." [ "
                stack-top .name
                ." ]"
        THEN
        
        r> to obj-save
        20 nesting @ ?dup if 1+ - then getxy drop max col
        getxy drop 20 >
        if      cr 20 col
        then    ."  --> "
        ?dbg-cont                               \ are we doing continuous steps
        IF      ekey?                           \ did user press a key
                IF      _mkey drop              \ then discard it
                        false to ?dbg-cont      \ stop continuous
                        _mkey upc               \ and wait for next command
                ELSE    ip @ @
                        dup     'EXIT   =       \ if at EXIT
                        over ['] UNNEST = or    \ or at UNNEST
                        over ['] EXITP  = or    \ or at EXITP
                        over ['] EXITM  = or    \ or at EXITM
                        nip
                        IF      false to ?dbg-cont \ stop continuous
                                _mkey upc          \ and wait for next command
                        ELSE    0x0D               \ else just do an 'enter'
                        THEN
                THEN
        ELSE    _mkey upc                       \ not continuous, get a key
        THEN
        CASE
          ascii P OF  ip0 @ ip ! nesting off            ENDOF
           ctrl P OF  ip @ to nextbreak nesting off
                      dbg-next 0x0D pushkey             ENDOF
          ascii J OF  dbg-jump                          ENDOF
          ascii C OF  true to ?dbg-cont                 ENDOF   \ continuous
          ascii D OF  ip off  restore-io debug-exit
                                               EXIT     ENDOF
          ascii H OF  base-toggle                       ENDOF
          ascii N OF  dbg-nest                          ENDOF
          ascii U OF  dbg-unnest                        ENDOF
          ascii F OF  run-forth                         ENDOF
          ascii . OF  dbg-fstack                        ENDOF
          ascii R OF  dbg-rstack                        ENDOF
\in-system-ok 'V' OF  ORDER                             ENDOF
          ascii W OF  dbg-watch                         ENDOF
\in-system-ok 'O' OF  cr order                          ENDOF
          ascii Q OF  ip off  ." unbug" restore-io forth-io
                                                abort   ENDOF
               27 OF  ip off  ." unbug" restore-io forth-io
                                                abort   ENDOF
          ascii ? OF  dbg-help                          ENDOF

\       **** other keys just cause debugger step execution ****
                      >r dbg-next ( default )
                      nextbreak -1 <>
                      IF        nextbreak ip !
                                -1 to nextbreak
                      THEN
                      r>
        endcase
        restore-io
        debug-exit
        [ last @ name> ] literal patch ;        \ patch in trace

' trace to 'trace

\ -------------------- Initialize Debugger --------------------

forth definitions

: _.rstack      ( -- )
                rp@ rp0 @ #dbg-rstack ;

' _.rstack is .rstack

: unbug  ( -- )
        ip @
        if      ip 2@ !  ip off
        then    ;

synonym unbp unbug

: remote-debug        ( cfa -- flag )   \ set a breakpoint at cfa, return flag
                                        \ return - TRUE=success, FALSE=fail
                unbug                   \ clear any existing breakpoint
                dup cfa-watch
                dup to start-watch?
        begin   false to obj-save
                false to ?dbg-cont      \ turn off contuous step
                base @ to debug-base
                dup  colon?
                over does>?  or
                over defer?  or
                0=
                if      drop FALSE EXIT         \ ERROR EXIT !
                then    dup  colon?
                if      >body   TRUE
                else    dup does>?
                        if      TRUE
                        else    >body @ FALSE
                        then
                then
        until   dup ip0 ! ip !
                ['] trace patch
                TRUE to remote-debug?   \ disable local display of debug info
                nesting off TRUE ;

: adebug        ( cfa -- )              \ set a breakpoint at cfa
                unbug                   \ clear any existing breakpoint
                FALSE to remote-debug?  \ enable local display of debug info
                dup to start-watch?
        begin   false to obj-save
                false to ?dbg-cont      \ turn off contuous step
                base @ to debug-base
                dup  colon?
                over does>?  or
                over defer?  or
                0=
           if   cr ." Must be a :, DEFER or DOES> definition"
                drop EXIT
           then dup  colon?
           if   >body
                TRUE
           else dup does>?
                if      ." DOES> nesting " @ 2 cells +
                        TRUE
                else
                    ." DEFER nesting "
                        >body @ dup .name FALSE
                then
            then
        until   dup ip0 ! ip !
                ['] trace patch
                nesting off ;

((
: auto-debug-key ( -- )
                debug-io
                ['] key-breaker adebug
                restore-io ;

: auto-debug-breaker ( -- )
                debug-io
                ['] breaker adebug
                restore-io ;
))

: debug         ( -<name>- )
                ' adebug ;

synonym bp debug

: debug-io debug-io ;
: restore-io restore-io ;

: dbg           ( -<name>- )                    \ debug a word now
                >in @ debug >in ! ;

: watch         ( -<watch_command_line>- )      \ install a watch commandline
                0 word c@
                if      pocket count watch-buf place
                else    dbg-watch
                then    ;

in-system

: #patchinto    ( a1 n1 -<name1 name2>- ) \ patch a1 into name1 at name2
                >r                        \ at occurance n1
                bl word anyfind 0= abort" Couldn't find the patchinto function"
                bl word anyfind 0= abort" Replacable word isn't defined"
                swap dup 0x200 ['] unnest lscan
                0= abort" Couldn't find end of the function"
                over - rot
                r> 0
                do      dup>r lscan dup
                        0= abort" Couldn't find the replacable word in function"
                        1- swap cell+ swap
                        r>
                loop    2drop cell - ! ;

: patchinto     ( a1 -<name1 name2>- )
                1 #patchinto ;

in-application

only forth also definitions

: with-source   ( -- )
                TRUE to with-source? ;

with-source     \ default to using source

: without-source ( -- )
                FALSE to with-source? ;

\S

: test1  dup dup + + ;
: test2  test1 test1 ;
: test3  1 test2 test2 drop ;

variable foo
: test4  foo @ if  foo @ test2 .  then ;

: test5  10 0 do i foo +! loop ;

: wf  ." foo = " foo ? ;


