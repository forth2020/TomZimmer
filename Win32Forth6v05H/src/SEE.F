\ $Id: see.f 1.1 1994/04/01 07:53:29 andrew Exp $

\ see.f beta 1.9G 2002/08/29 arm minor modifications, CALL support
\ see.f beta 2.9G 2002/09/24 arm release for testing
\ see.f beta 3.3D 2002/10/08 arm Consolidation

cr .( Loading Decompiler...)
cr .( -- BETA SEE.F V2.9G --)

( Decompiler from F83 )

(( A Forth decompiler is a utility program that translates
  executable forth code back into source code.  Normally this is
  impossible, since traditional compilers produce more object
  code than source, but in Forth it is quite easy.  The decompiler
  is almost one to one, failing only to correctly decompile the
  various Forth control stuctures and special compiling words.
  It was written with modifiability in mind, so if you add your
  own special compiling words, it will be easy to change the
  decompiler to include them.  This code is highly implementation
  dependant, and will NOT work on other Forth system.  To invoke
  the decompiler, use the word SEE <name> where <name> is the
  name of a Forth word. ))


only forth also definitions decimal

new-chain .execution-class-chain
new-chain .other-class-chain
new-chain .word-chain

0 value &flit
2 value cells/float
0 value debugging?

: dummy.float   ( a1 -- a2 )
                ." ???" cells/float cells+ ;

defer .float    ' dummy.float is .float

DEFER (SEE)  ( cfa -- )

INTERNAL        \ internal definitions start here

: .proc-name    ( cfa -- )
                cfa>proc proc>name dup c@
                1 name-max-chars between        \ if it looks like a count byte
                if      count type space        \ display the name
                else     h.                     \ else display a number
                then    ;

: .WORD         ( IP -- IP' )
                DUP @ DUP  &origin        HERE within
                      OVER sys-origin sys-here within or
        IF      DUP >R @
                CASE   DOVALUE   OF R@                         .NAME   ENDOF
                       DOVALUE!  OF R@ ." TO "       2 CELLS - .NAME   ENDOF
                       DOVALUE+! OF R@ ." +TO "      3 CELLS - .NAME   ENDOF
                       DOLOCAL   OF R@                         .NAME   ENDOF
                       DOLOCAL!  OF R@ ." TO "       2 CELLS - .NAME   ENDOF
                       DOLOCAL+! OF R@ ." +TO "      3 CELLS - .NAME   ENDOF
                      DO2VALUE!  OF R@ ." 2TO "      2 CELLS - .NAME   ENDOF
                      DO2VALUE+! OF R@ ." 2+TO "     3 CELLS - .NAME   ENDOF
                       (IV@)     OF R@ ." I:"                  .NAME   ENDOF
                       (IV!)     OF R@ ." TO-I: "    2 CELLS - .NAME   ENDOF
                       (IV+!)    OF R@ ." +TO-I: "   3 CELLS - .NAME   ENDOF
\                      (IV[]@)   OF R@ ." I[]:"                .NAME   ENDOF
\                      (IV[]!)   OF R@ ." TO-I[]: "  2 CELLS - .NAME   ENDOF
\                      (IV[]+!)  OF R@ ." +TO-I[]: " 3 CELLS - .NAME   ENDOF
                       DOCALL    OF R@ ." CALL "          .proc-name   ENDOF   \ arm
\+ .M0NAME             M0CFA     OF R@ ." M0:"               .M0NAME   ENDOF
\+ .M1NAME             M1CFA     OF R@ ." M1:"               .M1NAME   ENDOF
                       DOOBJ     OF R@ ." O:"                  .NAME   ENDOF
\ bug fix suggested by "beberst@infonie.fr"
                                R@ 2 cells &origin @ + U>
                                IF      R@ 2 CELLS - @ DOOBJ =
                                ELSE    FALSE
                                THEN
                                IF      ." O:" R@ 2 CELLS - .NAME
                                ELSE    over 2 cells+ R@ =      \ optimized inline code
                                        IF      debugging?
                                                if      ." <Optimized Code>"
                                                else    R@ (see)
                                                then
                                                swap cell+ @ cell- swap
                                        ELSE    r@ false .word-chain do-chain 0=
                                                if      .name
                                                else    drop
                                                then
                                        THEN
                                THEN
                ENDCASE R> DROP
        ELSE    ." 0x" 1 h.r ."  "
        THEN    CELL+ ;

: .BRANCH       ( IP -- IP' )
                .WORD  DUP @ CELL /  dup 0> if ." +" then h.  CELL+   ;

: .STRING       ( IP -- IP' )
                34 emit space
                CELL+
                dup c@ ?line
                COUNT 2DUP TYPE  34 emit space + 1+ aligned ;

: .CALL         ( ip -- ip' )
                .word .word ;

: .LOCALS       ( IP -- IP' )
                ." INIT-LOCALS "
                cols ?line ." LOCALS|"
                DUP 1+ c@ dup 0
                ?do     ."  LOCAL" i 2 pick c@ + 1 .r
                loop    drop
                dup c@ ?dup
                if      ."  \"
                        dup 0
                        ?do     ."  LOCAL" i 1 .r
                        loop    drop
                then    ."  | " cols ?line
                CELL+ ;

\ Decompile each type of word                         28Feb84map

hex E8909090 constant does-op decimal

: does?         ( ip -- ip+ flag )
                dup 8 +  swap @ does-op = ;

: .(;code)      ( ip -- ip' )
                cell+  does?
                IF      ." DOES> "
                ELSE    ." ;CODE "  drop false
                THEN    ;

: d_cr          ( -- )
                debugging? 0=
                if      cr
                then    ;

: .execution-class  ( ip cfa -- ip' )
    case
        ['] lit         of cell+ ." lit "   .word                   endof
            &flit       of cell+ ." flit "  .float                  endof
        ['] (is)        of cell+ ." (is) "  .word                   endof
        ['] (.")        of ." ."        .string                     endof
        ['] (S")        of ." S"        .string                     endof
        ['] (Z")        of ." Z"        .string                     endof
        ['] (C")        of ." C"        .string                     endof
        ['] (abort")    of ." ABORT"    .string                     endof
        ['] ?branch     of      d_cr ." IF      " +tab  cell+ cell+ endof
        ['] branch      of -tab d_cr ." ELSE    " +tab  cell+ cell+ endof
        ['] (do)        of      d_cr ." DO      " +tab  cell+ cell+ endof
        ['] (?do)       of      d_cr ." ?DO     " +tab  cell+ cell+ endof
        ['] (loop)      of -tab d_cr ." LOOP    "       cell+ cell+ endof
        ['] (+loop)     of -tab d_cr ." +LOOP   "       cell+ cell+ endof
        ['] _case       of      d_cr ." CASE    " +tab  cell+       endof
        ['] _of         of      d_cr ." OF      " +tab  cell+ cell+ endof
        ['] _endof      of  tab      ." ENDOF   " -tab  d_cr
                                                        cell+ cell+ endof
        ['] _endcase    of -tab d_cr ." ENDCASE "       cell+       endof
        ['] _then       of -tab d_cr ." THEN    "       cell+       endof
        ['] _begin      of      d_cr ." BEGIN   " +tab  cell+       endof
        ['] _while      of -tab d_cr ." WHILE   " +tab  cell+ cell+ endof
        ['] _until      of -tab d_cr ." UNTIL   "       cell+ cell+ endof
        ['] _repeat     of -tab d_cr ." REPEAT  "       cell+ cell+ endof
        ['] _again      of -tab d_cr ." AGAIN   "       cell+ cell+ endof
        ['] compile     of .word .word                              endof
        ['] unnest      of ." ; "  drop  0                          endof
        ['] unnestm     of ." ;M " drop  0                          endof
        ['] unnestp     of ." ;P " drop  0                          endof
        ['] (;code)     of -tab d_cr .(;CODE) tab +tab              endof
        ['] create      of d_cr .word tab +tab                      endof
        ['] init-locals of cell+ .locals                            endof
                           false .execution-class-chain do-chain 0=
                           if    swap .word swap
                           then
    endcase     ;


\ Decompile a : definition                            15Mar83map

: .PFA          ( cfa -- )
                tabing-on
                0TAB +TAB tab
                BEGIN   14 ?line   DUP @ .EXECUTION-CLASS
                        DUP 0=
                        nuf? or
                UNTIL   DROP
                tabing-off ;

: .IMMEDIATE    ( cfa -- )
                >name c@ 128 and
                if      ." IMMEDIATE "
                then    ;

\ Display category of word                            24APR84HHL

: .CONSTANT     ( cfa -- )
                dup >body ?   ." CONSTANT "   .name   ;

: .USER         ( cfa -- )
                dup >body dup . @ . ." USER "   dup .name
                ." Value = " execute ?   ;

: .VARIABLE     ( cfa -- )
                dup >body .   ." VARIABLE "   dup .name
                ." Value = " >body ?   ;

: .VALUE        ( cfa -- )
                dup cell+ ?  ." VALUE "   .name  ;

: .CLASS        ( cfa -- )
                ." :CLASS "  .name  ;

: .VOCABULARY   ( cfa -- )
                ." VOCABULARY "  .name  ;

: .:            ( cfa -- )
                ." : "  dup .name 2 spaces  >body .pfa   ;

: .DOES>        ( pfa -- )
                ." DOES> "  .pfa   ;


\ Display category of word                            24APR84HHL
: .DEFER        ( cfa -- )
                ." DEFER " DUP .NAME   ." IS "  >BODY @ (SEE)  ;

DEFER DISCODE   ' DROP IS DISCODE

\               ' DROP IS-DEFAULT DISCODE

: .CODE         ( cfa -- )
                ." IS CODE " @ DISCODE ;

: .;CODE        ( cfa -- )
                ." IS ;CODE " @ DISCODE ;

: .SYNONYM      ( cfa -- )
                ." SYNONYM " DUP .NAME ." IS " >BODY CELL+ @ (SEE) ;

: .OTHER        ( cfa -- )
                DUP .NAME
                .other-class-chain do-chain ?dup
                if      DUP @ OVER CELL+ = IF .CODE EXIT  THEN
                        DUP @ DOES? IF  .DOES>   DROP   EXIT   THEN
                        DROP .;CODE
                then    ;

: .OBJECT       ( cfa -- )
                dup >body @ body> .name .name ;

0 value a_value

\ Classify a word based on its CFA                    09SEP83HHL

: .definition-class  ( cfa cfa -- )
    @ case
        ['] .: @        of .:           endof
        ['] TRUE @      of .constant    endof
            dovar       of .variable    endof
            douser      of .user        endof
        ['] (see) @     of .defer       endof
        ['] a_value @   of .value       endof
            doClass     of .class       endof
            do|Class    of .class       endof
        ['] forth @     of .vocabulary  endof
            doObj       of .object      endof
        swap .other
    endcase ;


\ Top level of the Decompiler SEE                     29Sep83map

: ((SEE))       ( Cfa -- )
                CR DUP DUP .DEFINITION-CLASS  .IMMEDIATE   ;

' ((SEE)) IS (SEE)

EXTERNAL

synonym 'see (see)

: SEE           ( -- )
                '   (SEE)    ;

MODULE          \ end of the module

