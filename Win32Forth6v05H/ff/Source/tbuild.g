\ ========================================================================
\ --------  BUILDER vocabulary

warning off

((
    Used to compile ROMmable machine code to the image.

    Creates processor specific native/subroutine threaded code.  Incoming
    blank-delimited words are evaluated as shown below.  Subroutine call
    destinations are to the binding table if in DYNAMIC mode, or directly
    to the CFA if in STATIC mode.

    The processor-specific words in BUILDER are defined in an add-on file
    such as BLD8051.G, etc.  One of these add-on files must be FLOADed
    before you can build machine code for a specific processor.  These are
    words such as IF, THEN, DO, :, ;, etc.  Like regular Forth, compiling
    words are immediate.  The word LITERAL compiles code that places a
    number on the data stack, which is needed by binterpret.

    The BUILDER wordlist is at the top of the search order, and one of
    the token wordlists (CORE, for example) is next on the wordlist.
    During compilation, the top (BUILDER) wordlist is excluded to
    minimize name clashes.  For example, a target word that uses HERE in
    its definition isn't affected by the presence of HERE in the BUILDER
    wordlist.

    Search the search order for the word
    Found?
    IF    Compiling?
          IF    Immediate?
                IF    Execute it
                ELSE  Search again, but exclude the BUILDER wordlist
                      Found?
                      IF    Immediate?
                            IF    Error: Not a target word
                            ELSE  Execute it
                                  Was this a target word?
                                  IF    Compile a subroutine call
                                  ELSE  Error: Not a target word
                                  ENDIF
                            ENDIF
                      ELSE  Error: Can't use in compile mode
                      ENDIF
                ENDIF
          ELSE
                Execute it
                Was this a target word?
                IF    Error: Can't interpret target code
                ENDIF
          ENDIF
    ELSE
          Try to convert the word to a number
          Converted ok?
          IF    Compiling?
                IF    generate inline code for literal
                ELSE  leave the number on the stack
                ENDIF
          ELSE  Error: word is unrecognized
    ENDIF

))

defer colonmark    ( -- )        ' noop is colonmark
\ mark the beginning of a colon definition, does nothing if subroutine threading

defer compilelit   ( x -- )
\ compiles code to push a number onto the stack
        :noname  ." lit:" h. ;          is compilelit

variable btempname                      \ -> name to interpret

defer compilecall  ( a -- )
\ compiles code to call a subroutine
        :noname  ." call:"  h. ;        is compilecall

defer compilemacro ( a -- )
\ copies inline code, maybe does some peephole optimization
        :noname  ." macro:"  h. ;       is compilemacro

defer compileexit  ( -- )
\ compiles code to exit a word
        :noname  ." <exit>" ;           is compileexit

defer ins-align    ( -- )       \ align on instruction boundary
         ' noop is ins-align

defer >mark        ( -- a f )
        :noname  ." >mark" 0 0 ;        is >mark
defer >-mark       ( -- a f )
        :noname  ." >-mark" 0 0 ;       is >-mark
defer >0mark       ( -- a f )
        :noname  ." >0mark" 0 0 ;       is >0mark
defer >mmark       ( -- a f )
        :noname  ." >mmark" 0 0 ;       is >mmark
defer >resolve     ( a f -- )
        :noname  2drop ." >resolve" ;   is >resolve
defer <mark        ( -- a f )
        :noname  ." <mark" 0 0 ;        is <mark
defer <resolve     ( a f -- )
        :noname  2drop ." <resolve" ;   is <resolve
defer <0resolve    ( a f -- )
        :noname  2drop ." <0resolve" ;  is <0resolve
defer <-resolve    ( a f -- )
        :noname  2drop ." <-resolve" ;  is <-resolve

defer compileoffset ( -- ) ' noop is compileoffset
\ compiles code to add offset to address on the stack

defer local-fetch   ( n -- )
        :noname ." Local@:" . ;         is local-fetch

defer local-store   ( n -- )
        :noname ." Local!:" . ;         is local-store

defer local-begin                       ( #locals #args -- )
        :noname ." Begin-local " . . ;  is local-begin

defer local-end                         ( #locals -- )
        :noname ." End-local " . ;      is local-end

defer local-find  ' false is local-find  ( addr -- addr 0 | index -1 )

defer isee      ( <name> -- )   \ decompile code

variable localdepth

: B:COMPILE     ( -- )
\ compile a call to the last found token
                rom
                macro?             \ macros aren't rebindable
                if      t.cfa  compilemacro
                else    t.smartcfa compilecall
                then    t'usage incr
                ;

: B:COMPILE?    ( xt f -- )
                1 =                     ( immediate? )
                IF    EXECUTE ?STACK nopfa
                ELSE  DROP              ( must be a token )
\ At this point, the name must be a token and therefore isn't in the
\ BUILDER wordlist. FIND again w/o builder to avoid possible name clash.
                      btempname @ underfind ?dup
                      IF    1 =
                            IF    $type true abort"  not a compile word"
                            ELSE  nopfa
                                  EXECUTE ?STACK
                                  PFA-local @
                                  IF     ( compile a subroutine call )
                                         B:COMPILE
                                  ELSE   btempname @ $type
                                         true abort"  not a target word"
                                  THEN
                            THEN
                      ELSE  $type true
                            abort"  can't be used while compiling"
                      THEN
                THEN  ;

: simulate      ( -- ? )
\ attempt to simulate target code.  Only handles known literals
\ like CONSTANTs, etc.
                PFA-local @ magic_nosim = ?exit
                isliteral?
                if      t.litvalue
                else    btempname @ $type
                        ." ( xt=" t.xt h. ." ) "
                        true abort" Can't interpret target code"
                then    ;

\ ---------------------------------------------------------------------------
\ ROM builder locals support adapted from Win32forth        2/01 BNE

\ Run time: Locals are copied from the data stack to the return stack.
\           (the usual code)
\           Locals are discarded.

\ example for 68000 with 3 locals { a b \ c -- }
\       moveq d0,3  moveq d1,2  jsr (%localbegin)
\       (yourcode)
\       addi.l sp,0C
\       rts

\ example for 8051 with 3 locals
\       mov a,#32  call (%localbegin)
\       (yourcode)
\       mov a,sp  sub a,#0C  mov sp,a
\       ret

\ example for AVR with 3 locals
\       ldi R16,#32 rcall (%localbegin)
\       (yourcode)
\       ldi R16,#3  rcall (%localend)
\       ret

: PARMS,        ( -- )          \ compile runtime to push parameters
                INPARMS PARMS dup localdepth ! local-begin ;

: PFIND         ( addr -- addr FALSE | local# -1 )
\ Find local name in list, return the local# instead of a CFA
                FALSE                   \ flag initially not found
                BSTATE @ 0=             \ if not compiling
                PARMS 1 < OR            \ or no parameters
                IF      _EXIT           \ then don't try to find local
                THEN
                OVER COUNT "FIRSTPARM   \ put in first parm slot
                PARMS 1+ 1
                DO      PARMLIST I NAME-MAX-CHARS 1+ * + COUNT
                        PARMLIST COUNT
                        COMPARE 0=                      \ TRUE if matched
                        IF      2DROP  PARMS  I  -  TRUE    \ ********
                                LEAVE
                        THEN
                LOOP    ;

' pfind is local-find

:noname         ( addr -- )
                dup btempname !
                bstate @
        if      DUP COUNT FIND-BUFFER PLACE
                FIND-BUFFER ?UPPERCASE    ( addr ADDR )
                local-find ( addr ADDR 0 | addr cfa -1 )  \ look for locals
                if      local-fetch drop exit
                then    drop
        then    FIND ?DUP                       \ not a local
                IF    bstate @
                      IF    ( compiling.  0=imm, -1=comp )
                            B:COMPILE?
                      ELSE  drop         \ interpret
                            nopfa
                            EXECUTE ?STACK
                            PFA-local @
                            IF     simulate
                            THEN
                      THEN
                ELSE  ( not found, must be a number )
                      NUMBER
                      DOUBLE? IF tsplit ELSE D>S THEN
                      bstate @
                      IF     DOUBLE? IF swap compilelit THEN
                             compilelit
                      THEN
                THEN
                ; is buildinterp

variable blast  \ -> code of last created header

: makehead      ( datatype cputype <name> -- )
                rom headers @
                if      newheader
                then    ialign ins-align
                create-token
                ihere@ blast ! ;


: makeheader    ( datatype <name> -- )
\ create the header for a new word
                cputype makehead ;

variable chaining

: :build        ( <name> -- f )
\ Begin a colon definition
                rom
                0 makeheader    \ allocate a new token#
                colonmark  hide
                0 to parms
                -1 bstate !  -1 ;

: extend-def    ( <name> -- )
                findtoken t'usage incr PFA-local @ PFA-token ! ;

: :existing     ( -- f )
\ start compilation using an existing token
                rom headers @
                if      newheader
                then    ialign ins-align
                ihere@ blast !
                ihere@ t'cfa !
                -1 bstate !  -1 ;

: :append       ( <name> -- f )
\ Append an existing definition
\ usage:  :APPEND FOO  ( new code ... ) ;
                extend-def t.cfa >r :existing
                r> compilecall          \ invoke the old code
                nopfa
                -1 chaining ! ;

: :prepend      ( <name> -- f )
\ Prepend an existing definition
\ usage:  :PREPEND FOO  ( new code ... ) ;
                extend-def
                t.cfa chaining !        \ ; will wrap this up
                :existing nopfa ;

0 value doesaddr        \ used by >does to indicate building mode

: ;build        ( f -- )
\ End a colon definition
                ?condition
                rom 0 bstate !
                PARMS if PARMS local-end 0 to PARMS then  \ finish locals if any
                chaining @
        if      chaining @ -1 <>
                if      chaining @ compilecall
                then
        else    doesaddr 0= if reveal then
        then    chaining off
                compileexit
                doesaddr
        if
\             cr ." ending IDOES> clause "
\             order
                previous host (also) forth     \ finish IDOES> phrase, switch back to host mode
\             order
        then    0 to doesaddr ;

: tarfind       ( a n -- )
\ find the target word, must be findable (omit top wordlist),
\ abort with error if not a token
                temp$ place
                temp$ underfind                 \ search the order
                if      nopfa
                        execute  PFA-local @    \ found, execute it
                        if      exit
                        then
                else
                        drop
                then
                temp$ $type true abort"  needed by builder"
                ;

: tarcompile    ( a n -- )
\ compile a call to an already defined word (omit top wordlist),
\ abort with error if not a token
                tarfind B:COMPILE nopfa ;

: i'            ( <name> -- xt )
\ find xt of name, abort with error if not a token
                findtoken t'usage incr haveTOF? if t.xt else t.cfa then nopfa ;

: cfa'          ( <name> -- cfa )
\ find xt of name, abort with error if not a token
                findtoken t'usage incr t.smartCFA nopfa ;

: ?EXEC  BSTATE @    ABORT" execution only"    ;
: ?COMP  BSTATE @ 0= ABORT" compilation only"  ;

: ?PAIRS        ( n1 n2 -- )  XOR ABORT" conditionals not paired"  ;

: compilestr    ( <string"> -- )
\ compile a string, compile code that returns the address of the data
                commonmem?
        if      >MARK  ihere@ >r                \ store in ROM
                dmark [char] " parse dup ic,  bounds
                ?do     dmark i c@ ic,
                loop    ialign
                >RESOLVE r>
        else    ram    vhere@                   \ store in value space
                [char] " parse dup ic,  bounds
                ?do     i c@ ic,
                loop    valign
        then    rom compilelit compileoffset ;

: fillzeros     ( bytes -- )
                ram 0 ?do 0 ic, loop rom ;      \ data: zeros

: thisdata      ( -- a )   valign vhere@ ;      \ point to data space, aligned

0 value cfa-uninit

defer lay_binding  ' i, is lay_binding          \ append to binding table

: bindings,     ( -- )
\ execute this at the end of a build file to build an initialization
\ table containing cell-wide initialization data for the binding table
\   # of token code addresses starting with token0
\   ...data...  1 cell per token
                s" UNINITIALIZED" tarfind t.cfa to cfa-uninit
                rom usedtokens dup i, 0         \ # of cells
                ?do     xt-table i th @
                        if      i xt>cfa
                        else    cfa-uninit      \ 0 if no token
                        then    lay_binding     \ XT list starting with 0
                loop    nopfa ;

variable b4exit

2 value does>padding
\ 68K: allows 4-byte branch: DOES> code can be 32Kbytes away.
\ 8051: allows 3-byte long jump
\ AVR: allows 2-word JMP

: _laylit       ( taddr1 -- taddr2 )
                compilelit compileoffset        \ literal points to ROM data
                ihere@ b4exit !                 \ marker for DOES> patch
                compileexit
                ihere@ does>padding + ihere!    \ some extra space for a jump
                ialign ihere@ ;

: IPCREATE      ( <name> -- )  { \ offset start pfa -- }
\ create pointer for data structure in program space
\ leaves space after the EXIT to patch in a DOES> jump
                dt.romarray makeheader
                ihere@ to start
                0 to offset
        begin   start ihere!
                ihere@ _laylit offset + to pfa  \ locate data structure here
                start ihere!
                pfa    _laylit pfa -            \ 0 if exact fit, <0 if extra space
            dup if   0< if      pfa ihere!      \ second LIT was smaller
                                0               \ done
                        else    offset calignment + to offset \ retry
                                1
                        then
                then    0=
        until   ;

: (PCREATE)     ( <name> -- )  { \ offset start pfa -- }
\ create pointer for data structure in program space
                dt.romarray makeheader
                ihere@ to start
                0 to offset
        begin   start ihere!
                ihere@ compilelit compileoffset compileexit ialign \ dry run
                ihere@ offset + to pfa          \ locate data structure here
                start ihere!
                pfa compilelit compileoffset compileexit ialign \ real thing
                ihere@ pfa -     \ 0 if exact fit, <0 if extra space
            dup if   0< if      pfa ihere!      \ second LIT was smaller
                                0               \ done
                        else    offset calignment + to offset \ retry
                                1
                        then
                then    0=
        until   ;

: ,createcode   ( n -- n )
                dup compilelit compileoffset
                ihere@ b4exit ! compileexit ;

: ICREATE       ( <name> -- )
\ create pointer for data structure in data space
                dt.ramarray makeheader
                thisdata ,createcode drop          \ code: literal
                ram ;


\ IDOES> is a host word whose semantics are:
\ compile: append literal(IHERE) to definition, build run-time code.
\ execute: compile a jump to the literal.

\ 0 value doesaddr        \ used by >does to indicate building mode

: jumpdoes      ( addr -- )
\             cr ." compiling jump to " dup . ." at location " b4exit @ .
                rom  ihere@ >r
                b4exit @ ihere! compilecall compileexit
                ihere@ r> umax ihere!  ;

: IDOES>        ( -- f )
                postpone rom
                ihere@ to doesaddr
                ihere@ postpone literal   \ compile the address of >does code
                postpone jumpdoes
                postpone ;
                rom 0 to parms            \ start a headerless colon definition
\             order
                (previous) also builder
                ['] buildinterp  is interp  \ start building code, ; will terminate
                -1 bstate !  -1
\             order
\             cr ." building code for IDOES> "
                ; IMMEDIATE


\ ========================================================================
\ \\\\\\\\\\  B U I L D E R   V O C  \\\\\\\\\\

also builder definitions

: host          ( -- )  previous host ;       immediate
: building      ( -- )  builder _building ;     immediate
: tokenizing    ( -- )  tokenizer _tokenizing ; immediate
: testing       ( -- )
                communicable?
                if   callable?  if tester _testing then
                then    ;                       immediate
: forthing      ( -- )
                communicable?
                if   callable?  if tokenizer _forthing then
                then    ;                       immediate

synonym bi building     immediate
synonym ti tokenizing   immediate
synonym fi forthing     immediate
synonym te testing      immediate

previous        \ after here, definition wordlist isn't in the search order

: only          ( -- )  only home also builder ;
: previous      ( -- )  underprevious ;
: also          ( -- )  underalso ;
: definitions   ( -- )  underdefs ;
: words         ( -- )  underwords ;
: RAM           ( -- )  true  to RAM? ;
: ROM           ( -- )  false to RAM? ;
: static        ( -- )  false to dynamic? ;
: dynamic       ( -- )  true  to dynamic? ;
: [static]      ( -- )  false to dynamic? ;     immediate
: [dynamic]     ( -- )  true  to dynamic? ;     immediate

: c(            ( -- )          cat( ;          immediate
: bindings,     ( -- )          bindings, ;
: values,       ( -- )          values, ;
: decimal       ( -- )          decimal ;
: hex           ( -- )          hex ;
: headers-on    ( -- )          headers on ;
: headers-off   ( -- )          headers off ;
: rom-bounds?   ( -- lo hi )    rom-bounds? ;
: code-bounds?  ( -- lo hi )    code-bounds? ;
: data-bounds?  ( -- lo hi )    data-bounds? ;
: end*code      ( -- ) ;        \ needed for tokenized code words only

: test{         ( -- )          \ mark beginning of a block of test code
                ihere@ dup>r to syncstart
                false to dynamic?       \ safer to not use binding table here
                ROM-BOUNDS?     ( lo hi )
                swap r@ umin swap
                r> 1024 + umax
                ROM-BOUNDS              \ make 1K image here acceptable
                ;

: }test         ( -- )          \ send test code to target
                communicable? 0= abort" No target found"
                dup SyncMsgThreshold >= to tarprogress?
                syncstart ihere@ over -         ( a n )
                >r dup r> download                      \ send code to target
                callable? if tester _testing then       \ switch to tester
                false to dynamic? ;                     

: BEFORE:       ( -- f )        :prepend ;      \ extend existing definition
: AFTER:        ( -- f )        :append ;

: ORG           ( a -- )
                ram?
                if      dataorigin - vhere !
                else    codeorigin - ihere !    \ new absolute origin
                then    ;

: ALLOT         ( n -- )
                ram?
                if      vallot
                else    iallot
                then    ;

: HERE          ( -- a )
                ram?
                if      vhere@
                else    ihere@
                then    ;

: ALIGN         ( -- )          xalign ;
: ALIGNED       ( a1 -- a2 )    xaligned ;

: LITERAL       ( x -- )        rom compilelit ;      IMMEDIATE
\ Compile code to push a number onto the stack

: EXIT          ( -- )          PARMS if PARMS local-end then
                                compileexit ;           IMMEDIATE
: VOCABULARY    ( <name> -- )   tokenvocabulary ;

: DEFER         ( <name> -- )
\ Define a deferred word. Use IS <name> to assign a cfa to it.
                rom token# >r
                headers @ headers off  0 makeheader  headers !
                s" UNINITIALIZED" tarfind  \ default action
                r> t.cfa (cis) nopfa ;     \ points to UNINITIALIZED

: IS            ( xt <name> -- )
\ Assign xt to <name>.
                bstate @
                if      i' compilelit s" REBIND" tarcompile
                else    i' swap xt>cfa (cis)  \ interpret mode
                then    ;                  IMMEDIATE

: ISA           ( cfa <name> -- )
\ Assign cfa to <name>.
\ Use ISA with :NONAME, which returns a CFA not an XT
                bstate @
                abort" ISA is not supported in compile mode"
                i' swap (cis) ;

: VARIABLE      ( <name> -- )
\ runtime: ( -- a )
                dt.variable makeheader
                thisdata ,createcode               \ code: literal
                t'litvalue !  ram 0 i, rom ;       \ data: 0

: CVARIABLE     ( <name> -- )
\ runtime: ( -- a )
                dt.variable makeheader
                thisdata ,createcode               \ code: literal
                t'litvalue !  ram 0 ic, rom ;      \ data: 0

: SEMAPHORE      ( <name> -- )
\ runtime: ( -- a )      Same as VARIABLE
                dt.variable makeheader
                thisdata ,createcode               \ code: literal
                t'litvalue !  ram 0 i, rom ;       \ data: 0

: 2VARIABLE     ( <name> -- )
\ runtime: ( -- a )
                dt.2variable makeheader             \ code: literal
                thisdata ,createcode
                t'litvalue !  ram 0 i, 0 i, rom ;  \ data: 0 0

: CONSTANT      ( x <name> -- )
\ runtime: ( -- x )
                dt.constant makeheader
                dup t'litvalue !                   \ code: literal
                compilelit compileexit ;

: 2CONSTANT     ( d <name> -- )
\ runtime: ( -- d )
                dt.constant makeheader swap        \ code: 2literal
                compilelit compilelit compileexit ;

: ARRAY         ( len <name> -- )
\ runtime: ( -- a )
                dt.ramarray makeheader             \ code: literal
                thisdata ,createcode t'litvalue !
                fillzeros ;                        \ data: zeros

: STRING        ( len <name> -- )
\ runtime: ( -- a len )
                dt.string makeheader               \ code: 2literal
                thisdata compilelit compileoffset dup compilelit compileexit
                fillzeros ;                        \ data: zeros

: CREATE        ( <name> -- )   ICREATE ;
: PCREATE       ( <name> -- )   (PCREATE) ;

: TO            ( <name> -- )
\ runtime: TO XYZ compiles to XYZ (%TO!)
         ?comp  bl word ?uppercase local-find
         if     local-store
         else   count tarcompile
                s" (%TO!)"    tarcompile
         then   ; IMMEDIATE

: VALUE         ( x <name> -- )
\ runtime: ( -- x )
                dt.cell makeheader
                dup t'litvalue !
                thisdata compilelit compileoffset  \ code: literal
                s" (%TO@)" tarcompile              \       (%to@)
                compileexit
                ram i, rom ;                       \ data: x

: CFA           ( <name> - cfa )       cfa' ;
: [CFA]         ( <name> - cfa )       cfa' compilelit compileoffset ; IMMEDIATE
: '             ( <name> -- xt )        i' ;
: [']           ( <name> -- )           i' compilelit ;   IMMEDIATE
: [CHAR]        ( <c> -- )              char compilelit ; IMMEDIATE

: [COMPILE]     ( <name> -- )
                i' compilelit
                s" (COMPILE)" tarcompile ;      IMMEDIATE


\ This LOCAL support adapted from Win32forth:

: {             ( -- )  \ begin local variable usage in the form;
                        \ { initedloc1 initedloc2 \ uninitedloc3 -- comments }
                ?COMP
                PARMS ABORT" Locals may be defined only once per definition."
                0 TO INPARMS
                1 TO LOCFLG
                BEGIN   BL WORD ?UPPERCASE COUNT
                        FIRSTCHR [CHAR] - <>            \ as in { name -- }
                WHILE   FIRSTCHR [CHAR] \ =             \ start of non-initialized
                                                        \ local variables as in
                                                        \    { name \ another -- }
                        FIRSTCHR [CHAR] | = OR          \ or { name | another -- }
                        IF      0 TO LOCFLG 2DROP
                        ELSE    FIRSTCHR [CHAR] } = ABORT" Args missing --"
                                (LOCAL)
                        THEN
                REPEAT  2DROP
                PARMS 0>
                IF      PARMS,  ( compile runtime code )     \ *********
                THEN
                BEGIN   BL WORD COUNT  dup 0= abort" Missing }"
                        FIRSTCHR [CHAR] } = NIP NIP
                UNTIL   ; IMMEDIATE

: LOCALS|       ( -- )
                ?COMP
                PARMS ABORT" Locals may be defined only once per definition."
                0 TO PARMS
                0 TO INPARMS
                1 TO LOCFLG
                BEGIN
                        BL WORD COUNT 2DUP UPPER
                        FIRSTCHR [CHAR] | <>
                WHILE
                        (LOCAL)
                REPEAT  2DROP
                PARMS 0>
                IF      PARMS,  ( compile runtime code )    \ *********
                THEN
                REVERSEARGS ; IMMEDIATE


: ,             ( x -- )        xalign i, ;
: w,            ( w -- )        byte-split bigendian? 0= if swap then ich, ich, ;
                                \ fixed endian 16-bit
: lw,           ( w -- )        byte-split swap ich, ich, ; \ little endian
: bw,           ( w -- )        byte-split ich, ich, ;      \ big endian

: ll,           ( n -- )        word-split swap byte-split swap ich, ich,
                                byte-split swap ich, ich, ;
: bl,           ( n -- )        word-split byte-split ich, ich,
                                byte-split ich, ich, ;

: c,            ( c -- )        ich, ;
: [             ( -- )          0 bstate ! ;    IMMEDIATE
: ]             ( -- )          -1 bstate ! ;

: ,"            ( string" -- )
                xalign '"' word count  dup dmark ic, bounds
                ?do  i c@ dmark ic,  loop ;

: DB"           ( string" -- )
                '"' word count bounds
                ?do  i c@ dmark ic,  loop ;

: USB"          ( type_byte string" -- )
\ lay down unicode string with 1-byte embedded descriptor
\ type_byte is usually 3
                >r [char] " word count dup 1+ 2* dmark ic, r> dmark ic,  ( a n )
         bounds ?do     i c@ dmark ic, 0 dmark ic,     \ chars are little endian
                loop    ;

: code          ( <name> -- f )
\ Begin a code definition
                0 makeheader
                (also) assem true ;

: assemble      ( -- f )
\ enter assembler mode
                ihere@ assemlast !
                (also) assem true ;

: c[            ( -- f )
\ enter assembler mode for inline assembly
                bstate off              \ interpret
                (also) assem true ;             IMMEDIATE

: loco          ( <name> -- f )
\ Begin a code definition with no header
                ihere@ asmlabel
                (also) assem true ;

: call          ( <name> -- )
\ compile code to call an external procedure
\ you still have to define target-specific parameter shuffling words
                ?comp xlabel compilecall ;      IMMEDIATE

: C"            ( <string> -- )
\ compile string, runtime returns addr
                bstate on?
                if      compilestr
                else    [char] " word
                then    ;                       IMMEDIATE

: S"            ( <string> -- )
\ compile string, runtime returns addr,len
                bstate on?
                if      compilestr
                        s" COUNT" tarcompile
                else    [char] " parse
                then    ;                       IMMEDIATE

: ."            ( <string> -- )
\ compile string, runtime types string
                bstate on?
                if      compilestr
                        s" $TYPE" tarcompile
                else    [char] " parse type     \ smart ." like Forth-79
                then    ;                       IMMEDIATE

: H#            ( <hex_number> -- # )
\ OFW word to represent a hex number
\ split for target's cell width if it's a double
                base @ >R hex
                bl word NUMBER
                DOUBLE? IF tsplit ELSE D>S THEN
                bstate @
         IF     DOUBLE? IF swap compilelit THEN
                compilelit
         THEN   R> base ! ;  IMMEDIATE

: RECURSE       ( -- )          \ static call to myself
                ?COMP  blast @ compilecall ;  IMMEDIATE

: RETRY         ( -- )          \ jump to myself
                ?COMP  >MARK ihere@ >r blast @ ihere!
                >RESOLVE     r> ihere! ;        IMMEDIATE

\ -------------------- Structured Conditionals --------------------

: AHEAD         ( -- addr f )
                ?COMP  >MARK 2 ;                IMMEDIATE

: IF            ( -- addr f )
                ?COMP  >0MARK 2 ;               IMMEDIATE

: -IF           ( -- addr f )
                ?COMP  >-MARK 2 ;               IMMEDIATE

: THEN          ( addr f -- )
                ?COMP  2 ?PAIRS     >RESOLVE ;  IMMEDIATE

: ENDIF         ( addr f -- )
                ?COMP  2 ?PAIRS     >RESOLVE ;  IMMEDIATE

: ELSE          ( addr f -- f addr' f )
                ?COMP  2 ?PAIRS  >MARK
                SWAP   >RESOLVE  2 ;            IMMEDIATE

: BEGIN         ( -- addr f )
                ?COMP <MARK 1 ;                 IMMEDIATE

: UNTIL         ( addr f -- )
                ?COMP  1 ?PAIRS  <0RESOLVE ;    IMMEDIATE

: -UNTIL        ( addr f -- )
                ?COMP  1 ?PAIRS  <-RESOLVE ;    IMMEDIATE

: AGAIN         ( addr f -- )
                ?COMP  1 ?PAIRS  <RESOLVE ;     IMMEDIATE

: WHILE         ( addr f -- addr' f )
                ?COMP  >0MARK 2  2SWAP ;        IMMEDIATE

: -WHILE        ( addr f -- addr' f )
                ?COMP  >-MARK 2  2SWAP ;        IMMEDIATE

: REPEAT        ( addr f -- )
                ?COMP  1 ?PAIRS
                <RESOLVE  2 ?PAIRS  >RESOLVE ;  IMMEDIATE

: MULTI         ( -- addr1 addr2 )
\ MULTI ... REPEAT is equivalent to
\ BEGIN R> 1- DUP >R 0>= WHILE ... REPEAT
                <MARK 1
                >MMARK 2 2SWAP ;                IMMEDIATE

: DO            ( -- addr1 addr2 f)
                2 localdepth +!
                ?COMP  s" (%DO)" TARCOMPILE
                >MARK <MARK 3 ;                 IMMEDIATE

: ?DO           ( -- addr1 addr2 f )
                2 localdepth +!
                ?COMP  s" (%?DO)" TARCOMPILE
                >MARK <MARK 3 ;                 IMMEDIATE

: LOOP          ( addr f -- )
                -2 localdepth +!
                ?COMP  3 ?PAIRS
                s" (%LOOP)" TARCOMPILE
                <RESOLVE >RESOLVE ;             IMMEDIATE

: +LOOP         ( addr f -- )
                -2 localdepth +!
                ?COMP  3 ?PAIRS  s" (%+LOOP)" TARCOMPILE
                <RESOLVE >RESOLVE ;             IMMEDIATE


\ -------------------- Eaker CASE statement --------------------

: CASE          ( -- 0 )
                ?COMP   0 ;                     IMMEDIATE

: OF            ( ... -- ... a f  )
                ?COMP  s" (%OF)" TARCOMPILE
                >MARK 4 ;                       IMMEDIATE

: ENDOF         ( a f -- a' f' )
                ?COMP
                4 ?PAIRS
                >MARK SWAP >RESOLVE  5 ;        IMMEDIATE

: ENDCASE       ( ... -- )
                ?COMP  s" DROP" tarcompile
                BEGIN  ?DUP WHILE  5 ?PAIRS
                >RESOLVE  REPEAT ; IMMEDIATE

: call-only     call-only ;     \ these operate on built code, the last header
: macro         macro ;         \ defined.
: nobind        nobind ;
: optim         optim ;

synonym : :build
synonym ; ;build                                IMMEDIATE
synonym immediate (immediate)

(only) forth also definitions


