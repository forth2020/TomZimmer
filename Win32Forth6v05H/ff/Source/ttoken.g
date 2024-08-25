((

    TOKENIZER wordlist

    Used to convert Forth source files to tokenized bytecode in the image.

    Looks up a blank delimited word and either executes it or
    compiles a token for it.  Note that none of the tokens may have names
    that appear in the tokenizer wordlist.

    Search the search order for the word
    Found?
    IF    Execute it
          Was this a target word?
          IF    compile a token for it
          ENDIF
    ELSE
          Try to convert the word to a number
          Converted ok?
          IF    Compile the token for literal
          ELSE  Error: word is unrecognized
          ENDIF
    ENDIF

))

only forth also definitions

: name>xt       ( a n -- )
\ look up named token (omit top wordlist), abort with error if not a token
                temp$ place
                temp$ underfind               \ search the order
                if      PFA-local off
                        execute  PFA-local @  \ found, execute it
                        if      t.xt
                                t'usage incr
                                PFA-local off
                                exit
                        then
                else
                        drop
                then
                temp$ $type true
                abort" not tokenized during evaluation of above word."
                ;

: tokenize      ( a n -- )
\ tokenize named token (omit top wordlist), abort with error if not a token
                name>xt token, ;

: warn-redef    ( a n -- )
                warning on?
                if      noslack on? abort" redefined"
                        ."  redef " type space
                else    2drop
                then    ;

: tokenheader   ( datatype <name> -- xt )
\ if not defined, create the host's header for a new word, bump token#
\ return the xt of <name>.
                isdefined?
        if      drop  bl word count
                2dup warn-redef
                name>xt                 \ get xt of existing word
        else    token# >r
                cputype bit#tokenized? or create-token
                r>                      \ a new word
        then    ;

: laytoken      ( xt a n -- )
\ tokenize, then lay the token#
                tokenize token,  bstate off ;


: tokenlit      ( n -- )                \ output: (n) data...
\ compile a number using smallest available representation
                sext           \ sign extend n (will be sign extended on target)
                dup   -128   127 between
                if s" (lit8)"   tokenize ic, exit then
                dup -32768 32767 between
                if s" (lit16)"  tokenize iw, exit then
                   s" (lit32)"  tokenize il, ;

: loc-teardown  parms
        if      s" (]LOCAL)" tokenize parms ic, \ tear down locals structure
        then    ;

variable struct \ control structure tracking

: (t;)          ( -- )
                bstate off loc-teardown 0 to parms  \ clear locals list
                struct @ abort" Imbalanced control structure"
                s" ;" tokenize ;


variable lasttokendef   \ nfa of this definition
variable definer        \ T if a defining word is compiled

: smarttokenize { \ name -- }
\ tokenize the last parsed word, used by some defining words
                256 LocalAlloc: name
                btempname @ count name place
                bstate off?
        if      0 tokenheader
                name count laytoken
        else    name count tokenize definer on
        then    ;

: makedefining  ( <name> -- )
\ place a new defining word in the tokenizer vocabulary
                current @ >r
                (also) tokenizer definitions
                :
                postpone smarttokenize
                postpone ;
                (previous)
                r> current ! ;

: defining      ( -- )
\ make this definition a defining word for tokenizing
                s" {{ MAKEDEFINING "     temp$ place
                lasttokendef @ nfa-count temp$ +place
                s"  }}"                  temp$ +place
                temp$ count evaluate
                definer on ;

: laylocal      ( local# -- )
                PARMS swap - localdepth @ + 1- ic, ;

:noname         ( a -- )
\ interpret in tokenizer mode
                dup btempname !
                PFA-local off  definer off
                DUP COUNT FIND-BUFFER PLACE
                FIND-BUFFER ?UPPERCASE    ( addr ADDR )
                local-find  ( addr ADDR 0 | addr local# -1 )  \ look for locals
        if      nip s" (LOCAL@)" tokenize       \ fetch from local
                laylocal
        else    drop FIND
                IF    EXECUTE ?STACK
                      PFA-local @
                      dup magic_nosim <> and
                      bstate off? definer off? or and
                      IF     t.xt token,
                      THEN
                ELSE  ( not found, must be a number )
                      NUMBER DOUBLE?
                      IF     tsplit swap tokenlit
                      ELSE   D>S
                      THEN   tokenlit
                THEN  PFA-local off
        then    ; is tokeninterp

: tinterpret    ( -- )
\ tokenizing interpreter
                ROM  bstate off
                BEGIN  BL WORD DUP C@          \ get next word until end
                WHILE  SAVE-SRC tokeninterp
                       ?UNSAVE-SRC
                       parms .
                REPEAT DROP  PFA-local off
                ;

: (:) : ;

variable tcodelast 
variable tokenlast

: laystring"    ( c <string"> -- )
\ create a token header and compile a string for it
                pad 2 lay swap lay '"' lay drop \ form the token name
                pad count tokenize              \ tokenize .", S", etc.
                '"' word count       ( a len )  \ copy counted string
                dup ic, bounds                  \ to image
                ?do     i c@ ic,
                loop    ;

: (S")          ( <string"> -- )   'S' laystring" ;

create tcsstack 8 cells allot
variable tcsptr

: tcs*          ( -- a )   tcsptr @ 7 and cells tcsstack + ;
: tcspush       ( n -- )   tcs* !  tcsptr incr ;
: tcspop        ( -- n )   tcsptr decr  tcs* @ ;
: tmark         ( -- )     ihere@  tcspush ;
: tresolve      ( -- )     tcspop  ihere@ over - 2 -  ( a disp )
                           swap iw! ;       \ resolve it
: tcsswap       ( -- )     tcspop tcspop swap tcspush tcspush ;


\ ========================================================================

tokenizer definitions          \ \\\\\\\\  T O K E N I Z E R   V O C  \\\\

: host          ( -- )  previous host ;
: building      ( -- )  builder _building ;
: tokenizing    ( -- )  _tokenizing ;
: testing       ( -- )
                communicable?
                if   callable?  if tester _testing then
                then    ;
: forthing      ( -- )
                communicable?
                if   callable?  if _forthing then
                then    ;

: only          ( -- )  only home also tokenizer ;
: previous      ( -- )  underprevious ;
: also          ( -- )  underalso ;
: definitions   ( -- )  underdefs ;
: words         ( -- )  underwords ;

: vocabulary    ( <name> -- )   tokenvocabulary ;

synonym bi building
synonym ti tokenizing
synonym fi forthing
synonym te testing

\ : c(            ( -- )          cat( ;
\ : see           ( <name> -- )   isee ;
\ : >token#       ( -- )          to-token# ;

: H#            ( <hex_number> -- # )
\ OFW word to represent hex number
                base @ >R hex
                      bl word NUMBER DOUBLE?
                      IF     tsplit swap tokenlit
                      ELSE   D>S
                      THEN   tokenlit
                R> base ! ;

: PROGRAM       ( <name> -- )
\ Mark the beginning of a tokenized program with a 7-byte header
                dt.program tokenheader drop
                0x07 ic, 0xC9 ic,       \ 0x07C9 marks tokenized block
                ihere@ tokenlast !
                5 iallot                \ checksum: 2 byte, length: 3 byte
                ;

: END           ( -- )
\ mark the end of this block of data and resolve the checksum
                0xFF ic,                \ end token
                ihere@                  \ {
                0 ihere@ tokenlast @    ( 0 'new 'old )
                5 +                     \ only checksum data
                2dup - >r
                ?do     i ic@ +         \ get checksum of data
                loop    0xFFFF and      \ 16-bit checksum
                tokenlast @ ihere!
                iw,    r>               \ resolve checksum
                word-split ic, iw,      \ resolve length
                ihere! ;                \ }

: VARIABLE      ( <name> -- )
\ runtime: ( -- a )
                bstate off?
        if      dt.variable tokenheader
                s" VARIABLE" laytoken
        else    s" VARIABLE" tokenize defining
        then    ;

: 2VARIABLE     ( <name> -- )
\ runtime: ( -- a )
                bstate off?
        if      dt.2variable tokenheader
                s" 2VARIABLE" laytoken
        else    s" 2VARIABLE" tokenize defining
        then    ;

: CONSTANT      ( x <name> -- )
\ runtime: ( -- x )
                bstate off?
        if      dt.constant tokenheader
                s" CONSTANT" laytoken
        else    s" CONSTANT" tokenize defining
        then    ;

: 2CONSTANT     ( d <name> -- )
\ runtime: ( -- x )
                bstate off?
        if      dt.2constant tokenheader
                s" 2CONSTANT" laytoken
        else    s" 2CONSTANT" tokenize defining
        then    ;

: VALUE         ( x <name> -- )
\ runtime: ( -- x )
                bstate off?
        if      dt.cell tokenheader
                s" VALUE" laytoken
        else    s" VALUE" tokenize defining
        then    ;

: STRING        ( len <name> -- )
\ runtime: ( -- a len )
                bstate off?
        if      dt.string tokenheader
                s" STRING" laytoken
        else    s" STRING" tokenize defining
        then    ;

: ARRAY         ( len <name> -- )
\ runtime: ( -- a )
                bstate off?
        if      dt.ramarray tokenheader
                s" ARRAY" laytoken
        else    s" ARRAY" tokenize defining
        then    ;

: %%            ( <name> -- )
\ declare a token, lay the token#
                0 tokenheader drop
                token# 1- token, ;

: CREATE        ( <name> -- )
\ runtime: ( -- a )
                bstate off?
        if      dt.ramarray tokenheader
                s" CREATE" laytoken
        else    s" CREATE" tokenize defining
        then    ;

: MARKER        ( <name> -- )
                bstate off?
        if      0 tokenheader
                s" MARKER" laytoken
        else    s" MARKER" tokenize defining
        then    ;

: CODE{         ( -- magic )
\ Begin a code definition, assembler is not invoked.
\ Sample usage: CODE{ FOO 1 hex[ 01 F3 DE 22 ] }CODE \ 8051
                0 tokenheader 
                s" CODE" tokenize               \ CODE
                bl word number drop ic,         \ <cputype>
                token# 1- token,                \ <xt><length16><data>
                ihere@ tcodelast !  0 ic, 0 ic,
                83776323 ;

: }CODE         ( magic -- )
\ end a code definition, resolve the length of the code string
\ this was started by the tokenizer, see file TTOKEN.G
                83776323 <>
                abort" Expecting CODE{ FOO n hex[ XX XX ... XX ] }CODE"
                tcodelast @ >r
                ihere@ r@ - 2 - byte-split      ( lo hi )
                r@ ic! r> 1+ ic! ;

: CODE          ( -- magic f )
\ Begin a code definition using the current assembler
                0 tokenheader
                s" CODE" tokenize  cputype ic,  \ CODE
                token,                          \ <cputype><xt><length16><data>
                ihere@ tcodelast !  0 ic, 0 ic,
                (also) assem 83776324 true ;    \ normal end-code terminates

: END*CODE      ( magic -- )
\ end a code definition, resolve the length of the code string
                83776324 <> abort" Missing C;  Use CODE FOO ... C; END*CODE"
                tcodelast @ >r
                ihere@ r@ - 2 - byte-split      ( lo hi )
                r@ ic! r> 1+ ic! ;

((
        Example of a tokenized code definition:

     {{ cputype 1 = }}
        [IF]                                    \ is the compiler for an 8051?
        code evenparity ( c -- c' )
        \ set MSB for even parity of a 7-bit character
                mov a, dpl
                anl a, #127
                if_b    p
                        setb acc.7
                then    mov dpl, a
                ret end-code
        end*code                                \ finish "tokenized" code
        [THEN]

))

: {             ( -- )  \ begin local variable usage in the form;
                        \ { initedloc1 initedloc2 \ uninitedloc3 -- comments }
                bstate @
        if      PARMS ABORT" Locals may be defined only once per definition."
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
                IF      s" (local[)" tokenize
                        INPARMS PARMS over -   ( #locs #extras )
                        4 lshift or ic, \ packed #uninit|#init
                THEN
                BEGIN   BL WORD COUNT  dup 0= abort" Missing }"
                        FIRSTCHR [CHAR] } = NIP NIP
                UNTIL
        else    s" {" tokenize          \ not compiling a definition now
        then    ;

: TO            ( -- )
                >in @ >r bl word
                DUP COUNT FIND-BUFFER PLACE
                FIND-BUFFER ?UPPERCASE    ( addr ADDR )
                local-find
        if      nip s" (LOCAL!)" tokenize      \ store to local
                laylocal
                r>drop
        else    2drop r> >in !                  \ normal TO
                s" TO" tokenize
        then    ;

: EXIT          ( -- )  loc-teardown s" EXIT" tokenize ;

: DO            ( -- )  s" DO"    tokenize  2 localdepth +!  35 struct +! ;
: ?DO           ( -- )  s" ?DO"   tokenize  2 localdepth +!  35 struct +! ;
: LOOP          ( -- )  s" LOOP"  tokenize -2 localdepth +! -35 struct +! ;
: +LOOP         ( -- )  s" +LOOP" tokenize -2 localdepth +! -35 struct +! ;

: .             ( -- )  s" (.)"   tokenize  s" TYPE." tokenize ;
: U.            ( -- )  s" (U.)"  tokenize  s" TYPE." tokenize ;
: D.            ( -- )  s" (D.)"  tokenize  s" TYPE." tokenize ;
: UD.           ( -- )  s" (UD.)" tokenize  s" TYPE." tokenize ;
: .R            ( -- )  s" (.R)"  tokenize  s" TYPE." tokenize ;
: U.R           ( -- )  s" (U.R)" tokenize  s" TYPE." tokenize ;

: [COMPILE]     ( <name> -- )
                s" [COMPILE]" tokenize
                i' token, ;

: POSTPONE      ( <name> -- )
                s" POSTPONE" tokenize
                i' token, ;

: CHAR          ( <char> -- )
                s" CHAR" tokenize
                bl word 1+ c@ ic, ;

: [CHAR]        ( <char> -- )
                bl word 1+ c@ tokenlit ;

: #IF           ( -- )
                s" #IF#" tokenize
                tmark 0 ic, 0 ic,  ;

: #ELSE         ( -- )
                s" 0" tokenize
                s" #IF#" tokenize
                tmark 0 ic, 0 ic,
                tcsswap tresolve ;

: #ENDIF        ( -- )
                tresolve ;

: #THEN         ( -- )
                tresolve ;

: ."            ( <name> -- )   '.' laystring" ;
: C"            ( <name> -- )   'C' laystring" ;
: ,"            ( <name> -- )   ',' laystring" ;

: BEGIN         ( -- )          s" BEGIN"  tokenize   7 struct +! ;
: UNTIL         ( -- )          s" UNTIL"  tokenize  -7 struct +! ;
: AGAIN         ( -- )          s" AGAIN"  tokenize  -7 struct +! ;
: WHILE         ( -- )          s" WHILE"  tokenize   4 struct +! ;
: REPEAT        ( -- )          s" REPEAT" tokenize -11 struct +! ;
: MULTI         ( -- )          s" MULTI"  tokenize  11 struct +! ;

: ::            ( <name> -- )
\ redefine an existing token
                struct off
                bstate off?
        if      isdefined? 0= abort" not defined yet"
                s" :" tokenize
                bstate on
                last @ lasttokendef !
        else    s" :" tokenize defining
        then    ;

: :             ( <name> -- )
\ start a new definition
                struct off
                bstate off?
        if      isdefined? abort" Already defined, use :: or different name"
                0 tokenheader
                s" :" laytoken
                bstate on
                last @ lasttokendef !
        else    s" :" tokenize defining
        then    ;

(:) IF          ( -- )          s" IF"     tokenize   4 struct +! ;
(:) THEN        ( -- )          s" THEN"   tokenize  -4 struct +! ;

synonym ;  (t;)
synonym S" (s")

(only) forth also definitions


