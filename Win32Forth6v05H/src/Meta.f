\ Win32 Forth Metacompiler
\ Andrew McKewan, November 1995
\ Many thanks to Henry Laxen & Michael Perry for F83
\ meta.f beta 1.9F 2002/08/13 arm minor modifications
\ meta.f beta 2.0A 2002/08/31 arm added winlibrary meta
\ meta.f beta 2.9G 2002/09/29 arm Release for testing
\ meta.f beta 3.1A 2002/10/04 arm Performance enhancements
\ meta.f beta 3.3D 2002/10/08 Consolidated

cr .( Loading Meta Compiler...)
cr .( -- BETA META.F V3.4E --)

((
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*NOTICE**NOTICE**NOTICE**NOTICE**NOTICE**NOTICE**NOTICE**NOTICE**NOTICE*
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

August 27th, 1996 - 13:44 tjz

NOTICE: Rather than modifying this file, to change the Forth dictionary
        available memory, use the NEW commandline method as follows;

        WIN32FOR.EXE FLOAD META.F SETSIZE BYE

        The above commandline, including the RESIZE parameter, will
        cause you to be prompted for the size of the application and
        system dictionaries you want in the new kernel.

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
))

SYS-WARNING-OFF         \ don't warn about use of system words

ONLY FORTH ALSO DEFINITIONS
ANEW METACOMPILER
NOSTACK
WARNING OFF

cr .( Loading Meta.f from folder : ) cd

FLOAD VERSION.F  \ GET KERNEL VERSION

FLOAD METADLG    \ load new save dialog

: SETSIZE ;      \ must be findable, but doesn't do anything

: REEXTEND      S" DOS FKERNEL.EXE FLOAD src\EXTEND.F BYE " EVALUATE BYE ;

: HAVE ( -- f ) DEFINED NIP 0<> ;

: >WORDLIST  ( voc-cfa -- wordlist )   VCFA>VOC ;  ( Win32Forth )

\ ======================================================================
\ Define the wordlists that are used in the metacompiler

VOCABULARY META         \ metacompiler implementation
VOCABULARY TARGET       \ target words
VOCABULARY TRANSITION   \ special compiling words
VOCABULARY FORWARD      \ forward references

' META        >WORDLIST CONSTANT META-WORDLIST          ( *SYSDEP* )
' TARGET      >WORDLIST CONSTANT TARGET-WORDLIST        ( *SYSDEP* )
' FORWARD     >WORDLIST CONSTANT FORWARD-WORDLIST       ( *SYSDEP* )
' TRANSITION  >WORDLIST CONSTANT TRANSITION-WORDLIST    ( *SYSDEP* )
' ASSEMBLER   >WORDLIST CONSTANT ASSEMBLER-WORDLIST     ( *SYSDEP* )


\ We will use the following search orders:
: IN-FORTH        ONLY FORTH ALSO DEFINITIONS  ;
: IN-META         ONLY FORTH ALSO META ALSO DEFINITIONS ;
: IN-TRANSITION   ONLY FORWARD ALSO TARGET ALSO TRANSITION ;

IN-META

\ ======================================================================
\ Memory Access Words

157 CONSTANT #THREADS                   \ # of threads in FORTH-WORDLIST

0x00000000      VALUE ORIGIN            \ where target image will run
1024 768 *      VALUE SSEPARATION       \ offset from target to heads
1024 700 *      VALUE APPSIZE           \ size of kernel application dictionary
1024 400 *      VALUE SYSSIZE           \ size of kernel system dictionary

0 VALUE LEN-APP
0 VALUE LEN-SYS
\ 0 VALUE LEN-CODE

CREATE KERN-NAME MAXSTRING ALLOT S" FKERNEL.IMG" KERN-NAME PLACE

ALSO HIDDEN

: PROMPT-SIZE   ( -<"SETSIZE">- )  \ if SIZE follows on commandline, prompt
                CMDLINE S" SETSIZE" CAPS-SEARCH NIP NIP
                IF      START: META-DIALOG
                        IF      GETAPPMEM: META-DIALOG 262000 MAX
                                0x10000 naligned TO SSEPARATION
                                SSEPARATION        TO APPSIZE
                                GETSYSMEM: META-DIALOG 212000 MAX
                                0x10000 naligned   TO SYSSIZE
                                GETORIGIN: META-DIALOG dup 0>
                                IF        0x100000 MAX  \ must start at least 1 megabyte
                                        0x10000000 MIN  \ and below 256 megabytes
                                ELSE    drop 0          \ can't specify a negative start address
                                THEN    TO ORIGIN
                        THEN
                THEN    ;

PROMPT-SIZE

PREVIOUS

' CAPS-FIND IS FIND     \ disable local variable and method finding

0x10000 POINTER IMAGE                   \ where target image is built
                IMAGE  0x10000 ERASE    ( clean start )
0x10000 POINTER HIMAGE                  \ where target heads are built
                HIMAGE 0x10000 ERASE
0x10000 POINTER CIMAGE                  \ where target variables are built
                CIMAGE 0x10000 ERASE

VARIABLE DP-T   ORIGIN               DP-T !   \ target dictionary pointer
VARIABLE DP-H   ORIGIN SSEPARATION + DP-H !   \ target header pointer
VARIABLE DP-C   ORIGIN SSEPARATION + DP-C !   \ target header pointer

\ Where building words go: (for example)
\ COMPILE,       into CIMAGE
\        ,       into  IMAGE
\    SYS-,       into HIMAGE

: THERE   ( taddr -- addr )     ORIGIN -  IMAGE +  ;
: C@-T    ( taddr -- char )     THERE C@ ;
: W@-T    ( taddr -- word )     THERE W@ ;
: @-T     ( taddr -- n )        THERE @  ;
: C!-T    ( char taddr -- )     THERE C! ;
: W!-T    ( word taddr -- )     THERE W! ;
: !-T     ( n taddr -- )        THERE !  ;
: HERE-T  ( -- taddr )          DP-T @   ;
: ALLOT-T ( n -- )              ( HERE-T THERE OVER ERASE )  DP-T +!  ;
: C,-T    ( char -- )           HERE-T C!-T   1 DP-T +!  ;
: W,-T    ( w -- )              DUP C,-T  8 RSHIFT C,-T  ;
: ,-T     ( n -- )              HERE-T  !-T   4 DP-T +!  ;
: S,-T    ( addr len -- )       0 ?DO  COUNT C,-T   LOOP DROP  ;
: ALIGN   HERE-T ALIGNED HERE-T - ALLOT-T ;

: ADDR-H  ( taddr -- addr )     ORIGIN SSEPARATION + -  HIMAGE +  ;
: C@-H    ( taddr -- char )     ADDR-H C@ ;
: W@-H    ( taddr -- word )     ADDR-H W@ ;
: @-H     ( taddr -- n )        ADDR-H @  ;
: C!-H    ( char taddr -- )     ADDR-H C! ;
: W!-H    ( word taddr -- )     ADDR-H W! ;
: !-H     ( n taddr -- )        ADDR-H !  ;
: HERE-H  ( -- taddr )          DP-H @   ;
: ALLOT-H ( n -- )              ( HERE-T THERE OVER ERASE )  DP-H +!  ;
: C,-H    ( char -- )           HERE-H C!-H   1 DP-H +!  ;
: W,-H    ( w -- )              DUP C,-H  8 RSHIFT C,-H  ;
: ,-H     ( n -- )              HERE-H  !-H   4 DP-H +!  ;
: S,-H    ( addr len -- )       0 ?DO  COUNT C,-H   LOOP DROP  ;
: HALIGN   HERE-H ALIGNED HERE-H - ALLOT-H ;

: ADDR-C  ( taddr -- addr )     ORIGIN SSEPARATION + -  CIMAGE +  ;
: C@-C    ( taddr -- char )     ADDR-C C@ ;
: W@-C    ( taddr -- word )     ADDR-C W@ ;
: @-C     ( taddr -- n )        ADDR-C @  ;
: C!-C    ( char taddr -- )     ADDR-C C! ;
: W!-C    ( word taddr -- )     ADDR-C W! ;
: !-C     ( n taddr -- )        ADDR-C !  ;
: HERE-C  ( -- taddr )          DP-C @   ;
: ALLOT-C ( n -- )              ( HERE-T THERE OVER ERASE )  DP-C +!  ;
: C,-C    ( char -- )           HERE-C C!-C   1 DP-C +!  ;
: W,-C    ( w -- )              DUP C,-C  8 RSHIFT C,-C  ;
: ,-C     ( n -- )              HERE-C  !-C   4 DP-C +!  ;
: S,-C    ( addr len -- )       0 ?DO  COUNT C,-C   LOOP DROP  ;
: CALIGN   HERE-C ALIGNED HERE-C - ALLOT-C ;

: DUAL-SAVE  ( Haddr Hlen Daddr Dlen Cstr )
        COUNT w/o CREATE-FILE ABORT" Cannot create file"   >R
        R@ WRITE-FILE ABORT" Error writing Dictionary"
        R@ WRITE-FILE ABORT" Error writing headers"
        R> CLOSE-FILE ABORT" Cannot close file" ;

\ ======================================================================
\ Target Memory Dump

: .2   ( n -- )   0 <# # # #> TYPE SPACE ;

: EMIT.  ( n -- )
   ( 127 AND)  DUP BL 127 WITHIN 0= IF  DROP [CHAR] .  THEN  EMIT ;

: DUMP  ( taddr len -- )  ( hex byte format with ascii )
    BASE @ >R HEX  OVER + DUP ROT
    ?DO  CR I 4 .R SPACE SPACE   I 16 + OVER MIN I
         2DUP DO  I C@-T .2  I J 7 + = IF SPACE THEN  LOOP
         2DUP -  16 OVER - 3 *  SWAP 8 < -  1+ SPACES
         DO  I C@-T EMIT.  LOOP
         START/STOP
    16 +LOOP  DROP  R> BASE ! ;

\ Patch the disassembler
HAVE DIS386 [IF]

   ' C@-T  DISASSEMBLER IS TC@ META
   ' W@-T  DISASSEMBLER IS TW@ META
   '  @-T  DISASSEMBLER IS T@  META

[THEN]

\ ======================================================================
\ Modify assembler to place code into target

     ' HERE-T     ASSEMBLER ASM-HIDDEN IS CODE-HERE    META
     ' C,-T       ASSEMBLER ASM-HIDDEN IS CODE-C,      META
     ' W,-T       ASSEMBLER ASM-HIDDEN IS CODE-W,      META
     ' ,-T        ASSEMBLER ASM-HIDDEN IS CODE-D,      META
     ' C@-T       ASSEMBLER ASM-HIDDEN IS CODE-C@      META
     ' C!-T       ASSEMBLER ASM-HIDDEN IS CODE-C!      META
     ' W@-T       ASSEMBLER ASM-HIDDEN IS CODE-W@      META
     ' W!-T       ASSEMBLER ASM-HIDDEN IS CODE-W!      META
     '  @-T       ASSEMBLER ASM-HIDDEN IS CODE-D@      META
     '  !-T       ASSEMBLER ASM-HIDDEN IS CODE-D!      META

\ ASSEMBLER MACROS
: MACRO  ASSEMBLER DEFINITIONS : ;
: END-MACRO   POSTPONE ; META DEFINITIONS ; IMMEDIATE

\ ======================================================================
\ Define Meta Branching Constructs

: ?CONDITION  TRUE - ABORT" Conditionals not paired" ;

: ?>MARK      ( -- f addr )   TRUE   HERE-T   0 ,-T   ;
: ?>RESOLVE   ( f addr -- )   HERE-T OVER - SWAP !-T   ?CONDITION ;
: ?<MARK      ( -- f addr )   TRUE   HERE-T   ;
: ?<RESOLVE   ( f addr -- )   HERE-T - ,-T   ?CONDITION   ;

\ ======================================================================
\ Meta Compiler Forward Reference Linking

\ Structure of a forward reference (cell offsets from BODY)
\       0       - target address if resolved
\       1       - resolved flag
\       2       - link to previous forward reference


VARIABLE FORWARD-LINK   \ linked list of FORWARD words (for .UNRESOLVED)
0 FORWARD-LINK !

: MAKE-CODE     ( pfa -- )
                @ ,-T
                ;

: LINK-BACKWARDS ( pfa -- )
                HERE-T OVER @ ,-T   SWAP !   ;

: RESOLVED?     ( pfa -- f )
                CELL+ @   ;

: DO-FORWARD    ( -- )
                DOES>   DUP RESOLVED?
                        IF  MAKE-CODE  ELSE  LINK-BACKWARDS  THEN ;

: (FORWARD)    ( taddr -- )
        GET-CURRENT >R
        FORWARD-WORDLIST SET-CURRENT
        CREATE
          , ( taddr )
          FALSE , ( resolved flag )
          HERE FORWARD-LINK @ , FORWARD-LINK !
        DO-FORWARD
        R> SET-CURRENT ;

: FORWARD:  ( -- )      \ Explicit forward reference
        0 (FORWARD) ;

: UNDEFINED   ( -- )    \ Undefined words create automatic forward reference
        HERE-T  (FORWARD)  0 ,-T  ;

\ ======================================================================
\ Create Headers in Target Image. We support only one FORTH-WORDLIST in
\ the kernel.

CREATE FORTH-THREADS  #THREADS CELLS ALLOT
       FORTH-THREADS  #THREADS CELLS ERASE

: THREAD        ( addr len -- 'thread )      \ get vocab thread address
                #THREADS "#HASH FORTH-THREADS +  ;

VARIABLE HEADS  HEADS ON
: |   HEADS OFF ;   ( make next word headerless )

: VIEW,  ( -- )
        LOADLINE @ ,-H ;

: ALIGN-HEADER  ( len -- )      \ make sure link field will be cell aligned
        1+ HERE-H +  DUP ALIGNED  SWAP - ALLOT-H ;

VARIABLE LAST-H         \ target address of count byte
VARIABLE OFA-H          \ target address of OFA
VARIABLE OFAING

TRUE OFAING !

\ : .WORD   >IN @  BL WORD COUNT DUP 1+ ?CR TYPE SPACE  >IN ! ;

: HEADER   ( -- )
\        CR .S .WORD
        HALIGN
        ALIGN                                           \ align code space
        BL WORD ?UPPERCASE COUNT
        HEADS @
        IF     ( 0 ,-H  )                                 \ FFA new arm
                HERE-H OFA-H !                          \ save address of OFA
                -1 ,-H                                  \ optimizer field
                DUP ALIGN-HEADER
                2DUP S,-H                               \ name string
                HERE-H LAST-H !                         \ remember nfa
                DUP C,-H                                \ count byte
                VIEW,                                   \ view field
                THREAD  DUP @  HERE-H ROT !  ,-H        \ link field
                HERE-T ,-H                              \ cfa pointer field
        ELSE
                2DROP HEADS ON
        THEN  ;

\ ======================================================================
\ Meta Compiler Create Target Image

VARIABLE TARGET-LINK    \ linked list of TARGET words (for .SYMBOLS)
0 TARGET-LINK !

: DO-TARGET  DOES> MAKE-CODE ;  \ what target words do

: TARGET-CREATE   ( -- )

        >IN @ HEADER >IN !              \ create header in target
        TARGET DEFINITIONS              \ add word to TARGET wordlist
        CREATE
          HERE-T , ( xt )
          HERE TARGET-LINK @ , TARGET-LINK !    \ linked list of target words
        DO-TARGET
        META DEFINITIONS ;

: RECREATE   ( -- )
        >IN @   TARGET-CREATE   >IN !   ;

\ ======================================================================
\ Create target code words

HAVE CLEAR-LABELS 0= [IF]
: CLEAR-LABELS ;
: CHECK-LABELS ;
[THEN]

: INIT-ASSEMBLER  ( -- )        \ prepare for assembly code
        [ ASSEMBLER ] CLEAR-LABELS [ ASM-HIDDEN ] RESET-ASM [ META ]
        ASSEMBLER DEFINITIONS  !CSP ;

: CODE  ( -- )
        TARGET-CREATE
        HERE-T CELL+ ,-T
        HERE-T OFA-H @ !-H              \ init OFA with start of definition
        INIT-ASSEMBLER ;

: NCODE ( -- )                          \ NON inlineable code word
        TARGET-CREATE
        HERE-T CELL+ ,-T
        0 OFA-H !                       \ reset OFA usage
        INIT-ASSEMBLER ;

: CFA-CODE ( -- )
        ALIGN
        0 OFA-H !                       \ reset OFA usage
        INIT-ASSEMBLER  HERE-T CONSTANT  ;


: RESOLVE-OFA   ( -- )
        OFAING @
        OFA-H @  AND
        IF      OFA-H @ @-H -1 <>
                IF      \ save size of code definition
                        [ ASSEMBLER ] A;
                        [ META ] HERE-T OFA-H @ @-H - OFA-H @ !-H
                THEN
        THEN    0 OFA-H ! ;             \ reset OFA pointer

HEX : CALL,  ( t-addr -- )  E8909090 ,-T  HERE-T CELL+ - ,-T  ;  DECIMAL

ASSEMBLER DEFINITIONS
: LABEL   A;  HERE-T CONSTANT ;
: END-CODE   A;  IN-META  CHECK-LABELS
      [ ASM-HIDDEN ]  ?FINISHED ?UNRES  [ ASSEMBLER ]  ?CSP  ;
: C;   END-CODE  ;
META DEFINITIONS

\ ======================================================================
\ Force compilation of target & forward words. We need to reference the
\ special runtime target words like LIT and BRANCH before they are defined,
\ so we store the name of the word and look it up when we need it. Hopefully
\ they will have been defined by then. [TARGET] is for target primatives and
\ [LABEL] is for target assembly labels (runtime of builtin defining words).

: FIND&EXECUTE  ( addr len wordlist -- ? )
        SEARCH-WORDLIST 0= ABORT" Target word not found"  EXECUTE ;

: DEFERRED  ( wordlist -- )
        BL WORD COUNT POSTPONE SLITERAL
        POSTPONE LITERAL
        POSTPONE FIND&EXECUTE ;

: [TARGET]  ( -- )  TARGET-WORDLIST    DEFERRED ; IMMEDIATE
: [LABEL]   ( -- )  ASSEMBLER-WORDLIST DEFERRED ; IMMEDIATE

\ Find the next word in a single wordlist only
: DEFINED-IN  ( wordlist -- xt )
        BL WORD COUNT ROT SEARCH-WORDLIST   0= ?MISSING  ;

: 'T   ( -- xt )   TARGET-WORDLIST  DEFINED-IN ;
: 'F   ( -- xt )   FORWARD-WORDLIST DEFINED-IN ;

: [FORWARD]   ( -- )    'F COMPILE, ;   IMMEDIATE

\ ======================================================================
\ Define transition words, which behave like forth immediate words.

: T:   TRANSITION DEFINITIONS  META  :  ;
: T;   POSTPONE ;  META DEFINITIONS ; IMMEDIATE

: [TRANSITION]  TRANSITION-WORDLIST DEFINED-IN COMPILE, ; IMMEDIATE

T: (   POSTPONE (   T;
T: \   POSTPONE \   T;

: STRING,-T     [CHAR] " PARSE  DUP C,-T  S,-T  0 C,-T  ALIGN  ;

FORWARD: <(.")>
T: ."   [FORWARD]  <(.")>   STRING,-T   T;

FORWARD: <(S")>
T: S"    [FORWARD] <(S")>   STRING,-T   T;

FORWARD: <(C")>
T: C"    [FORWARD] <(C")>   STRING,-T   T;

FORWARD: <(ABORT")>
T: ABORT"   [FORWARD] <(ABORT")>    STRING,-T   T;

\ ======================================================================
\ Define target vocabularies (uh, wordlists)

VARIABLE VOC-LINK-T
FORWARD: <VOCABULARY>

: VOCABULARY   ( -- )
        TARGET-CREATE
        [FORWARD] <VOCABULARY>
        #THREADS ,-T
        HERE-T  VOC-LINK-T @ ,-T   VOC-LINK-T !
        #THREADS 0 DO  0 ,-T  LOOP ;

: IMMEDIATE   ( -- )
        LAST-H @  DUP C@-H 128 OR  ( Precedence Bit )  SWAP C!-H ;
        
VARIABLE STATE-T

T: [COMPILE]   'T EXECUTE    T;

: >BODY-T  4 + ;

FORWARD: <(IS)>
T: IS      [FORWARD] <(IS)>    T;
:  IS    'T >BODY @ >BODY-T  2DUP !-T  8 + !-T ;
 ( patches both current and default value of deferred word )

\ TO used inside a definition
T: TO   'T >BODY @ ( tcfa )  2 CELLS+ ,-T  T;
T: +TO  'T >BODY @ ( tcfa )  3 CELLS+ ,-T  T;

T: CALL 'T >BODY @ CELL+ ,-T T;                               \ added to support call

\ ======================================================================
\ Display the Target Symbol Table

: _@COL  _GETXY DROP ;
FORTH DEFER @COL META ' _@COL FORTH IS @COL META

: TAB   @COL 60 >
        IF  CR  ELSE  20 @COL OVER MOD - SPACES  THEN ;

: .SYMBOLS    ( -- )
        TARGET-LINK
        BEGIN   @ DUP
        WHILE   DUP CELL - ( pfa )
                DUP @ TAB 5 .R SPACE
                BODY> .NAME  ( *SYSDEP* )
                START/STOP
        REPEAT  DROP ;

\ ======================================================================
\ Meta Compiler Resolve Forward References

0 VALUE #UNRESOLVED

: .UNRESOLVED   ( -- )
        0 TO #UNRESOLVED
        FORWARD-LINK
        BEGIN   @ DUP
        WHILE   DUP 2 CELLS - RESOLVED? 0=
                IF      DUP 2 CELLS - BODY> .NAME  ( *SYSDEP* )
                        1 +TO #UNRESOLVED
                THEN
                START/STOP
        REPEAT  DROP
        #UNRESOLVED
        IF      CR ." !!!!!! There were: " #UNRESOLVED . ." words Unresolved !!!!!!"
                3 0 DO BEEP 300 MS LOOP
        ELSE    CR ." *** No words Unresolved ***"
        THEN    ;

: FIND-UNRESOLVED   ( -- cfa f )
        'F    DUP  >BODY RESOLVED?     ;

: RESOLVE   ( taddr cfa -- )
        >BODY   2DUP TRUE OVER CELL+ !   @
        BEGIN   DUP
        WHILE   2DUP @-T   -ROT SWAP !-T
        REPEAT  2DROP  !   ;

: RESOLVES   ( taddr -- )
        FIND-UNRESOLVED
        IF      .NAME ." Already Resolved"   DROP  ( *SYSDEP* )
        ELSE    RESOLVE
        THEN   ;

\ ======================================================================
\ Meta compiler Branching & Looping

T: IF      [TARGET] ?BRANCH  ?>MARK   T;

T: THEN    [TARGET] _THEN    ?>RESOLVE    T;
T: ELSE    [TARGET]  BRANCH  ?>MARK   2SWAP ?>RESOLVE   T;

T: BEGIN   [TARGET] _BEGIN   ?<MARK   T;
T: AGAIN   [TARGET] _AGAIN   ?<RESOLVE   T;
T: UNTIL   [TARGET] _UNTIL   ?<RESOLVE   T;
T: WHILE   [TARGET] _WHILE   ?>MARK  2SWAP  T;
T: REPEAT  [TARGET] _REPEAT  ?<RESOLVE  ?>RESOLVE   T;

T: ?DO     [TARGET] (?DO)    ?>MARK   T;
T: DO      [TARGET] (DO)     ?>MARK   T;
T: LOOP    [TARGET] (LOOP)   2DUP CELL+   ?<RESOLVE   ?>RESOLVE   T;
T: +LOOP   [TARGET] (+LOOP)  2DUP CELL+   ?<RESOLVE   ?>RESOLVE   T;

\ ======================================================================
\ Meta compiler literals

T: LITERAL   ( n -- )   [TARGET] LIT  ,-T   T;
T: [CHAR]    ( -- )     CHAR        [TRANSITION] LITERAL   T;
T: [']       ( -- )     'T >BODY @  [TRANSITION] LITERAL   T;

\ ======================================================================
\ Target EQU is like a constant except that if it is used in a definition
\ it will just compile a literal.

: (EQU)  ( n -- )
        CREATE ,  DOES> @ [TRANSITION] LITERAL ;

: EQU   ( n -<name>- )
        TRANSITION DEFINITIONS
        >IN @  OVER (EQU)  >IN !
        META DEFINITIONS
        CONSTANT ;

: HERE:  ( -<name>- )
        HERE-T
        TRANSITION DEFINITIONS
        >IN @  OVER (EQU)  >IN !
        META DEFINITIONS
        CONSTANT ;

\ ======================================================================
\ Meta compiler defining words

ASSEMBLER DEFINITIONS

: [UP]  ( offset -- ) [edx] ;   \ EDX is user pointer

META DEFINITIONS

FORTH VARIABLE PROC-LIST-T META
FORTH VARIABLE LIBS-LIST-T META

: PROC  ( n -- )                                  \ added to support proc
        | RECREATE
        HERE-T  PROC-LIST-T @ ,-T  PROC-LIST-T !
        >IN @ HERE: >IN !
        [LABEL] DOCALL ,-T
        0 ,-T
        0 ,-T
        C,-T
        BL PARSE DUP C,-T  S,-T  0 C,-T  ALIGN
        ;

: WINLIBRARY ( -- )
        | RECREATE                                \ added to support winlibrary
        HERE-T  LIBS-LIST-T @ ,-T  LIBS-LIST-T !
        0 ,-T
        0 C,-T
        BL PARSE 2dup upper DUP C,-T  S,-T  0 C,-T  ALIGN
        ;

: USER  ( n -- )        \ 960827 bee
        RECREATE   [LABEL] DOUSER ,-T
        DUP ,-T   CONSTANT   ;

: CREATE  ( -- )
        RECREATE  [LABEL] DOVAR ,-T
        HERE-T CONSTANT  ;

: VARIABLE  ( -- )
        CREATE   0 ,-T   ;

: CONSTANT   ( n -- )
        RECREATE   [LABEL] DOCON ,-T
        DUP ,-T   CONSTANT   ;

: VALUE  ( n -- )
        TARGET-CREATE
        [LABEL] DOVALUE   ,-T
        ,-T
        [LABEL] DOVALUE!  ,-T
        [LABEL] DOVALUE+! ,-T  ;

: LOCAL  ( n -- )
        TARGET-CREATE
        [LABEL] LOCAL@  ,-T
        1+ -4 * ,-T
        [LABEL] LOCAL!  ,-T
        [LABEL] LOCAL+! ,-T  ;       
        
FORTH VARIABLE DEFER-LIST-T META

: DEFER   ( -- )
        TARGET-CREATE   [LABEL] DODEFER ,-T
        0 ,-T
        HERE-T  DEFER-LIST-T @ ,-T  DEFER-LIST-T !
        0 ,-T  ;

FORWARD: <(;CODE)>

T: ;CODE   ( -- addr )
        [FORWARD] <(;CODE)>   HERE-T
        STATE-T OFF   IN-META
        INIT-ASSEMBLER T;

T: DOES>     ( -- addr )
        [FORWARD] <(;CODE)>   HERE-T
        [LABEL] DODOES CALL,   T;


\ ======================================================================
\ Identify numbers (single numbers only)

: NUMBER?  ( addr -- n f )
        count OVER C@ [CHAR] - =              \ leading minus sign?
        DUP >R IF  1 /STRING  THEN
        0 0 2SWAP >NUMBER 0= NIP NIP  ( -- u f )
        R> IF SWAP NEGATE SWAP THEN ;

: new-number    ( ^str -- d n )           \ an extensible version of NUMBER
                count temp$ place
                temp$ ?uppercase
                count super-number? nip ;

\ ======================================================================
\ Meta Compiler Compiling Loop.

\ We need a special version of WORD that will span multiple lines.
\ This will also save >IN so we can rescan the input stream.
FORTH VARIABLE T-IN META
: TOKEN  ( -- addr )
        BEGIN   >IN @ T-IN !
                BL WORD  DUP C@ 0=
        WHILE   DROP REFILL  0= ABORT" end of file in definition"
        REPEAT  ?UPPERCASE ;

: ]   ( -- )
        STATE-T ON   IN-TRANSITION
        BEGIN   TOKEN FIND
                IF      EXECUTE
                ELSE    new-NUMBER
                        IF      [TRANSITION] LITERAL
                        ELSE    DROP T-IN @ >IN !
                                UNDEFINED ( create forward reference )
                        THEN
                THEN
                STATE-T @ 0=
        UNTIL   ;

T: [   IN-META   STATE-T OFF   T;
T: ;   [TARGET] UNNEST   [TRANSITION] [   T;

\ ======================================================================
\ Interpretive words for Meta

: '            'T >BODY @   ;
: ,           ,-T ;
: W,         W,-T ;
: C,         C,-T ;
: HERE     HERE-T ;
: ALLOT   ALLOT-T  ;
: ,"    STRING,-T ;
: >BODY   >BODY-T ;

\ : DIS  ( t-addr -- )  THERE [ DISASSEMBLER ] REST [ META ] ;
\ : DS   ' @-T DIS ;

: |: | TARGET-CREATE  [LABEL] DOCOL ,-T   ]  ;   \ headerless : def

: :    TARGET-CREATE  [LABEL] DOCOL ,-T   ]  ;

\ And we're off.....

CR .( Metacompiler Loaded ) cr

\ ======================================================================

FLOAD FKERNEXT.F  \ load kernel extension for NEXT, EXEC code

CR .( Compiling FKERNEL.F, Version: ) kver sp@ 4 type drop cr

FLOAD FKERNEL.F

HERE-T ORIGIN -               AACTUAL !-T        \ set the actual dictionary length
HERE-H ORIGIN SSEPARATION + - SACTUAL !-T        \ set the actual header length

\ Resolve metacompiler forward references
' (.")          RESOLVES <(.")>
' (S")          RESOLVES <(S")>
' (C")          RESOLVES <(C")>
' (ABORT")      RESOLVES <(ABORT")>
' (IS)          RESOLVES <(IS)>
' (;CODE)       RESOLVES <(;CODE)>

\ Initialize variables
DEFER-LIST-T @ DEFER-LIST !-T
PROC-LIST-T @ WINPROC-LINK !-T
LIBS-LIST-T @ WINLIB-LINK !-T
HERE-T  DP !-T
HERE-H SDP !-T
' FORTH >BODY 2 CELLS +
FORTH-THREADS OVER THERE #THREADS CELLS MOVE
DUP CURRENT !-T
DUP CONTEXT !-T
DUP CONTEXT CELL+ !-T
    VOC>VLINK VOC-LINK !-T              \ setup minimal voc-link for >name
    

HERE-T ORIGIN               -  TO LEN-APP
HERE-H ORIGIN SSEPARATION + -  TO LEN-SYS

\ CR .( Host Bytes Free:             )    UNUSED U.
\ CR .( First Target Code Address:   )    ORIGIN THERE U.
\ CR .( Last Target Code Address:    )    HERE-T THERE U.

  cr .( Segment) tab .( Origin) tab .( Size)
  cr .( -------) tab .( ------) tab .( -----)

  CR .( Target) tab ORIGIN               5 H.R .( h)
                 tab LEN-APP 5 .R
  CR .( Header) tab ORIGIN SSEPARATION + 5 H.R .( h)
                 tab LEN-SYS 5 .R
  cr .( -------) tab .( ------) tab .( -----)
  cr .( Total) tab tab LEN-APP LEN-SYS + 5 .r
cr

.UNRESOLVED
#UNRESOLVED [IF]   CR .( Aborting, NO IMAGE SAVED!) ABORT [THEN]
  NOSTACK
  HIMAGE HERE-H ORIGIN SSEPARATION + - IMAGE HERE-T ORIGIN - 4DUP
  KERN-NAME DUAL-SAVE
  CR nip rot drop + .  .( bytes written to file ) KERN-NAME COUNT TYPE CR
  
2 pause-seconds

\ reextend


