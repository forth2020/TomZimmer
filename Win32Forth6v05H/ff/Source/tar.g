((
        Target interpreter

        Virtual stacks are used to effect zero net depth change in the
        backdoor monitor, if the monitor isn't multitasking.  Pushes and
        pops work on the virtual stack.  To execute a word, operation
        proceeds as follows:

        1.  Send SUSPEND command.
        2.  Transfer the virtual stacks to the target.
        3.  Send the execution address and the CALL command.
        4.  Transfer the virtual stacks from the target.
        5.  Send the RESUME command.

        If the monitor is multitasking, it has its own stack space and
        the extra stack handling isn't needed.
))

defer tar-eval \ ( a n -- ) evaluate target word

: tdepth        ( -- n )
\ depth in cells of data stack
                bdepth byte-split drop c>n ;  \ data stack depth

' bpushd is tpush
' bpopd  is tpop

: tpick         ( u -- xu )     bpickd ;
\ pick uth item from data stack

: tdepthr       ( -- )          bdepth 8 rshift c>n ;
\ depth in cells of return stack

: tpickr        ( u -- xu )     bpickr ;
\ pick uth item from return stack


: texecute      ( cfa -- )
\ call subroutine at cfa in the target board
                dup 0=  over -1 = or if
true abort" Attempt to execute CFA of 0 or -1."
     cr  ." If you're in static mode, try DYNAMIC."
                then
                bexecute ;

((
: tarwordok?    ( a n -- f )
\ look up a target word, result is nonzero if its for real
                temp$ place
                forceCORE        \ add CORE to the search order if needed
                PFA-local off
                temp$ FIND
                IF      EXECUTE ?STACK
                        PFA-local @
                ELSE    0
                THEN
                PFA-local off ;
))

:noname         ( a -- ? )
\ Interpret a single word
                forceCORE        \ add CORE to the search order if needed
                PFA-local off
                FIND                    \ use whole search order
                IF      EXECUTE ?STACK
                        PFA-local @
                        dup magic_nosim <> and
                        IF      dynamic?
                                IF      dynamicCFA      \ dynamic
                                ELSE    t.cfa           \ static
                                THEN    texecute
                        THEN
                ELSE    ( not found, must be a number )
                        NUMBER DOUBLE?
                        IF      tsplit SWAP tpush
                        ELSE    D>S
                        THEN    tpush
                THEN
                PFA-local off ; is _tarinterp

: _tarinterpret ( -- )
\ target testing interpreter:
\ static mode: use calls to hard addresses (for early testing of ROM stuff)
\ dynamic mode: use calls into the binding table, CFAs needn't be known.
                BEGIN  BL WORD DUP C@          \ get next word until end
                WHILE  SAVE-SRC
                       _tarinterp
                       ?UNSAVE-SRC
                REPEAT DROP
                ;

: match-if      ( possibly match an [if] )
                ( if count \ c-addr -- if count' \ c-addr )
                dup count s" IF" compare 0= if swap 1+ swap then ;

: match-then    ( possibly match a [then] )
                ( if count \ c-addr -- if count' \ c-addr )
                dup count s" THEN" compare 0= if swap 1- swap then ;

: match-else    ( possibly match an [else] )
                ( if count \ c-addr -- if count' \ c-addr )
                dup count s" ELSE" compare 0= if swap 1- dup if 1+ then swap then ;
                
: definite-word ( definitely get a word from the input stream )
                ( delim -<chars>- c-addr )
                begin   dup word dup c@ 0=
                while   drop refill 0=
                        abort" Input stream exhausted!"
                repeat  nip ;

: (else)        ( -- )  \ copied from INTERPIF.F
                1
                begin   ?dup
                while   bl definite-word ?uppercase
                        match-if
                        match-then
                        match-else drop
                repeat  ;

tester definitions             \ \\\\\\\\  T E S T E R   V O C  \\\\\\\\

: host          ( -- )  previous host ;
: building      ( -- )  builder _building ;
: tokenizing    ( -- )  tokenizer _tokenizing ;
: testing       ( -- )
                communicable?
                if   callable?  if _testing then
                then    ;
: forthing      ( -- )
                communicable?
                if   callable?  if tokenizer _forthing then
                then    ;

: only          ( -- )  only home also tester ;
: previous      ( -- )  underprevious ;              
: also          ( -- )  underalso ;                  
: definitions   ( -- )  underdefs ;
: words         ( -- )  underwords ;

: '             ( <name> -- )                   \ push xt to target stack
                i' tpush ;

: to            ( <name> -- )
                bl word  _tarinterp             \ execute postfix equivalent
                s" (%TO!)" tar-eval ;

: if            ( f -- )        tpop 0= if (else) then ;
: else          ( -- )          (else) ;
: then          ( -- )          ;

synonym bi building
synonym ti tokenizing
synonym fi forthing                                  
synonym te testing                                   

(only) forth also definitions


:noname         ( a n -- )
\ convert string to a token and execute the token on the target
                temp$ place
                temp$ _tarinterp
                ; is tar-eval

: tar-evaluate  ( a n -- )
\ use tar-eval on blank-delimited substrings of a string
                begin   bl skip  dup
                while   2dup bl scan   ( a n a' n' )
                        2swap pluck -   ( a' n' a len )
                        tar-eval
                repeat  2drop ;

: tarhere       ( -- a )
\ get HERE from target board
                b.'here @ dup if bexecute tpop then ;

: maxtartoken   ( -- n )
\ get max token value from target board
                b.'maxtok# @ dup if bexecute tpop then ;


: tar-free      ( -- begin end )
\ estimate free memory on target board
                communicable?
        if      tarhere                 \ last used byte
                jsrsize 0<                                      \ binding table
                if      b.bindorigin @ usedtokens jsrsize * +   \ grows down
                        \ code grows toward binding table
                else    b.codeorigin @ b.codesize @ + 1-        \ grows up
                        \ code grows toward end of code space
                then
        else    0 0
        then    ;

: BUG           ( <name> -- )
\ debug a word in the image using the low level (machine code) debugger
                findtoken
                communicable?
        if      t.xt tpush              \ push xt onto target stack
                s" BIND@" tar-eval      \ convert to CFA
                tpop  >disassy          \ get CFA from target stack
                startlobug
        then    ;

:noname         ( -- )
\ load CFAs from the target board for use in low level debugging
                communicable?
        if      true to dynamic?
                usedtokens dup 0
                false to tarerror?
                false sending: progress  Start: progress
                ?do     key? drop
                        i tpush                 \ push xt onto target stack
                        s" BIND@" tar-eval      \ convert to CFA
                        tpop                    \ get CFA from target stack
                        i xt>npfa nip dup PFA-token !
                        if      t'cfa !         \ modify CFA of this token
                                i over update: progress  \ progress window
                                tarerror? key? or
                                if      cr ." Label Upload Incomplete! "
                                        leave
                                then
                        else    drop            \ token doesn't have CFA
                        then
                loop    drop
                Close: progress
                PFA-token off   predisassemble
        then
        ; is LOADLABELS


