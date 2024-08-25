( TESTANSI.F )

\ Testing Environment Querys

CREATE #CS  ," /COUNTED-STRING"
\       n       yes     max size of a counted string, in chars
CREATE #H   ," /HOLD"   
\       n       yes     size of the output string buffer, in characters
CREATE #P   ," /PAD"    
\       n       yes     size of the scratch area pointed to by PAD,
CREATE #AU  ," ADDRESS-UNIT-BITS"
\        n      yes     size of one address unit, in bits
CREATE #C   ," CORE"
\       flag    no      true if complete core word set present
CREATE #CX  ," CORE-EXT"
\       flag    no      true if core extensions word set present
CREATE #/F  ," FLOORED"
\       flag    yes     true if floored division is the default
CREATE #MC  ," MAX-CHAR"
\       u       yes     max value of any char in the
\                               implementation-defined character set
CREATE #MD  ," MAX-D"
\       d       yes     largest usable signed double number
CREATE #MN  ," MAX-N"
\       n       yes     largest usable signed integer
CREATE #MU  ," MAX-U"
\       u       yes     largest usable unsigned integer
CREATE #MUD ," MAX-UD"
\       ud      yes     largest usable unsigned double number
CREATE #RS  ," RETURN-STACK-CELLS"
\       n       yes     maximum size of the return stack, in cells
CREATE #SC  ," STACK-CELLS"
\       n       yes     maximum size of the data stack, in cells
CREATE #B   ," BLOCK"
\       flag    no      block word set present
CREATE #BX  ," BLOCK-EXT"
\       flag    no      block extensions word set present
CREATE #D   ," DOUBLE"
\       flag    no      double-number word set present
CREATE #DX  ," DOUBLE-EXT"
\       flag    no      double-number extensions word set present
CREATE #EX  ," EXCEPTION"
\       flag    no      Exception word set present
CREATE #EXX ," EXCEPTION-EXT"
\       flag    no      Exception extensions word set present
CREATE #FC  ," FACILITY"
\       flag    no      facility word set present
CREATE #FCX ," FACILITY-EXT"
\       flag    no      facility extensions word set present
CREATE #FI  ," FILE"
\       flag    no      file word set present
CREATE #FIX ," FILE-EXT"
\       flag    no      file extensions word set present
CREATE #F   ," FLOATING"
\       flag    no      floating-point word set present
CREATE #FX  ," FLOATING-EXT"
\       flag    no      floating-point extensions word set present
CREATE #FS  ," FLOATING-STACK"
\       n       yes     If n = zero, floating-point numbers are kept
\                       on the data stack; otherwise n is the maximum
\                       depth of the separate floating-point stack.
CREATE #MF  ," MAX-FLOAT"
\       r       yes     largest usable floating-point number
CREATE ##L  ," #LOCALS"
\       n       yes     maximum number of local variables in a definition
CREATE #L   ," LOCALS"
\       flag    no      locals word set present
CREATE #LX  ," LOCALS-EXT"      
\       flag    no      locals extensions word set present
CREATE #M   ," MEMORY-ALLOC"
\       flag    no      memory-allocation word set present
CREATE #MX  ," MEMORY-ALLOC-EXT"
\       flag    no      memory-allocation extensions word set present
CREATE #T   ," TOOLS"
\       flag    no      programming-tools word set present
CREATE #TX  ," TOOLS-EXT"
\       flag    no      programming-tools extensions word set present
CREATE #SO  ," SEARCH-ORDER"
\       flag    no      search-order word set present
CREATE #SOX ," SEARCH-ORDER-EXT"
\       flag    no      search-order extensions word set present
CREATE #WL  ," WORDLISTS"
\       n       yes     maximum number of word lists usable in the
\                       search order
CREATE #ST  ," STRING"
\       flag    no      string word set present
CREATE #STX ," STRING-EXT"
\       flag    no      string extensions word set present

: UD.  0 10 UM/MOD -ROT 10 UM/MOD SWAP >R SWAP 0 D.R R> 48 + EMIT ;

: TE    
        SWAP >R COUNT 2DUP CR TYPE 2 SPACES ENVIRONMENT? DUP 
        20 COL 2 .R 0=
        IF    R>DROP EXIT
        THEN  R> 25 COL
        CASE  0 OF      EXIT    ENDOF
              1 OF      .       ENDOF
              2 OF      U.      ENDOF
              3 OF      D.      ENDOF
              4 OF      UD.     ENDOF
              5 OF      FS.     ENDOF
        ENDCASE ;

: TESTENV
        CR CR CR ."  Display ENVIRONMENT? results: " CR
        1 #CS  TE   1 #H   TE   1 #P   TE   1 #AU  TE   0 #C   TE
        0 #CX  TE   0 #/F  TE   2 #MC  TE   3 #MD  TE   1 #MN  TE
        2 #MU  TE   4 #MUD TE   1 #RS  TE   1 #SC  TE   0 #B   TE
        0 #BX  TE   0 #D   TE   0 #DX  TE   0 #EX  TE   0 #EXX TE
        0 #FC  TE   0 #FCX TE   0 #FI  TE   0 #FIX TE   0 #F   TE
        0 #FX  TE   1 #FS  TE   5 #MF  TE   1 ##L  TE   0 #L   TE
        0 #LX  TE   0 #M   TE   0 #MX  TE   0 #T   TE   0 #TX  TE
        0 #SO  TE   0 #SOX TE   1 #WL  TE   0 #ST  TE   0 #STX TE  ;

TESTENV

#B COUNT ENVIRONMENT? 0= #IF   CR .( Loading BLOCK.F ) 
   FLOAD BLOCK.F  
   CR  0 #B TE  0 #BX TE CR
#THEN

CR CR .( End of ENVIRONMENT? display ) CR

: %D  DEFINED 0=
        IF      CR count type ."  was not found. " 0
        ELSE    DROP true 
        THEN 
        AND ;

(  Look for presence of core words: )

        TRUE            NOSTACK1
        %D !
        %D #
        %D #>
        %D #S
        %D '

        %D (
        %D *
        %D */
        %D */MOD
        %D +

        %D +!
        %D +LOOP
        %D ,
        %D -
        %D .

        %D ."
        %D /
        %D /MOD
        %D 0<
        %D 0=
        %D 1+

        %D 1-
        %D 2!
        %D 2*
        %D 2/
        %D 2@
        %D 2DROP
        %D 2DUP
        %D 2OVER

        %D 2SWAP
        %D :
        %D ;
        %D <

        %D <#
        %D =
        %D >
        %D >BODY
        %D >IN
        %D >NUMBER

        %D >R
        %D ?DUP
        %D @
        %D ABORT
        %D ABORT"
        %D ABS

        %D ACCEPT
        %D ALIGN
        %D ALIGNED
        %D ALLOT
        %D AND

        %D BASE
        %D BEGIN
        %D BL
        %D C!
        %D C,
        %D C@

        %D CELL+
        %D CELLS
        %D CHAR
        %D CHAR+
        %D CHARS
        %D CONSTANT

        %D COUNT
        %D CR
        %D CREATE
        %D DECIMAL
        %D DEPTH

        %D DO
        %D DOES>
        %D DROP

        %D DUP
        %D ELSE
        %D EMIT
        %D ENVIRONMENT?

        %D EVALUATE
        %D EXECUTE
        %D EXIT
        %D FILL
        %D FIND

        %D FM/MOD
        %D HERE
        %D HOLD
        %D I
        %D IF

        %D IMMEDIATE
        %D INVERT
        %D J
        %D KEY
        %D LEAVE

        %D LITERAL
        %D LOOP
        %D LSHIFT
        %D M*
        %D MAX
        %D MIN

        %D MOD
        %D MOVE
        %D NEGATE
        %D OR
        %D OVER
        %D POSTPONE

        %D QUIT
        %D R>
        %D R@
        %D RECURSE

        %D REPEAT
        %D ROT
        %D RSHIFT
        %D S"
        %D S>D

        %D SIGN
        %D SM/REM
        %D SOURCE
        %D SPACE
        %D SPACES
        %D STATE
        %D SWAP

        %D THEN
        %D TYPE
        %D U.
        %D U<
        %D UM*

        %D UM/MOD
        %D UNLOOP
        %D UNTIL
        %D VARIABLE

        %D WHILE
        %D WORD
        %D XOR
        %D [
        
        %D [']
        %D [CHAR]
        %D ]

0= #IF   CR .(  ***  Words missing from CORE wordset *** )  TRUE
#ELSE   CR .( All words present in CORE wordset )  TRUE
#THEN


( END of CORE wordset )

( Check for CORE EXT wordset )

        %D #TIB
        %D .(
        %D .R
        %D 0<>
        %D 0>
        %D 2>R

        %D 2R>
        %D 2R@
        %D :NONAME
        %D <>

        %D ?DO
        %D AGAIN
        %D C"

        %D CASE
        %D COMPILE,
        %D CONVERT
        %D ENDCASE

        %D ENDOF
        %D ERASE
        %D EXPECT
        %D FALSE
        %D HEX

        %D MARKER
        %D NIP
        %D OF
        %D PAD

        %D PARSE
        %D PICK
        %D QUERY
        %D REFILL
        %D RESTORE-INPUT

        %D ROLL
        %D SAVE-INPUT
        %D SOURCE-ID
        %D SPAN
        %D TIB

        %D TO
        %D TRUE
        %D TUCK
        %D U.R
        %D U>
        %D UNUSED

        %D VALUE
        %D WITHIN
        %D [COMPILE]
        %D \

0= #IF   CR .(  ***  Words missing from CORE-EXT wordset *** )  TRUE
#ELSE    CR .( All words present in CORE-EXT wordset )  TRUE
#THEN

( End of simple existance test for CORE EXT wordset )

#B COUNT ENVIRONMENT? #IF

( Check for BLOCK wordset )

        %D BLK
        %D BLOCK

        %D BUFFER
        %D EVALUATE
        %D FLUSH
        %D LOAD
        %D SAVE-BUFFERS

        %D UPDATE


( End of BLOCK wordset )

#ELSE   DROP FALSE
#THEN

0= #IF   CR .(  ***  Words missing from BLOCK wordset *** )  TRUE
#ELSE    CR .( All words present in BLOCK wordset )  TRUE
#THEN

#BX COUNT ENVIRONMENT? #IF

( Check for BLOCK-EXT wordset )

        %D EMPTY-BUFFERS
        %D LIST
        %D REFILL
        %D SCR
        %D THRU

        %D \            

( End of BLOCK-EXT wordset )

#ELSE   DROP FALSE
#THEN

0= #IF   CR .(  ***  Words missing from BLOCK-EXT wordset *** )  TRUE
#ELSE   CR .( All words present in BLOCK-EXT wordset )  TRUE
#THEN

( Check for DOUBLE wordset )

        %D 2CONSTANT
        %D 2LITERAL

        %D 2VARIABLE
        %D D+
        %D D-
        %D D.
        %D D.R
        %D D0<
        %D D0=

        %D D2*
        %D D2/
        %D D<
        %D D=
        %D D>S
        %D DABS
        %D DMAX
        %D DMIN

        %D DNEGATE
        %D M*/
        %D M+

0= #IF   CR .(  ***  Words missing from DOUBLE wordset *** )  TRUE
#ELSE   CR .( All words present in DOUBLE wordset )  TRUE
#THEN

( End of DOUBLE wordset )

( Check for DOUBLE EXT wordset )

        %D 2ROT
        %D DU<

0= #IF   CR .(  ***  Words missing from DOUBLE-EXT wordset *** )  TRUE
#ELSE   CR .( All words present in DOUBLE-EXT wordset )  TRUE
#THEN

( End of DOUBLE-EXT wordset )

( Start of EXCEPTION wordset )

        %D CATCH

        %D THROW

( End of EXCEPTION wordset )

0= #IF   CR .(  ***  Words missing from EXCEPTION wordset *** )  TRUE
#ELSE   CR .( All words present in EXCEPTION wordset )  TRUE
#THEN

( Start of EXCEPTION-EXT wordset )

        %D ABORT
        %D ABORT"

0= #IF   CR .(  ***  Words missing from EXCEPTION-EXT wordset *** )  TRUE
#ELSE   CR .( All words present in EXCEPTION-EXT wordset )  TRUE
#THEN

( End of EXCEPTION-EXT wordset )

( Start of FACILITY wordset )

        %D AT-XY
        %D KEY?
        %D PAGE

0= #IF   CR .(  ***  Words missing from FACILITY wordset *** )  TRUE
#ELSE   CR .( All words present in FACILITY wordset )  TRUE
#THEN

( End of FACILITY wordset )

( Start of FACILITY-EXT wordset )

        %D EKEY
        %D EKEY>CHAR
        %D EKEY?
        %D EMIT?
        %D MS
        %D TIME&DATE            \ CURRENTLY MISSING ***

0= #IF   CR .(  ***  Words missing from FACILITY-EXT wordset *** )  TRUE
#ELSE   CR .( All words present in FACILITY-EXT wordset )  TRUE
#THEN

( End of FACILITY-EXT wordset )

( Start of FILE wordset )

        %D (
        %D BIN
        %D CLOSE-FILE

        %D CREATE-FILE
        %D DELETE-FILE
        %D FILE-POSITION
        %D FILE-SIZE
        %D INCLUDE-FILE

        %D INCLUDED
        %D OPEN-FILE
        %D R/O

        %D R/W
        %D READ-FILE

        %D READ-LINE
        %D REPOSITION-FILE
        %D RESIZE-FILE

        %D S"
        %D SOURCE-ID
        %D W/O
        %D WRITE-FILE

        %D WRITE-LINE

0= #IF   CR .(  ***  Words missing from FILE wordset *** )  TRUE
#ELSE   CR .( All words present in FILE wordset )  TRUE
#THEN

( End of FILE wordset )

( Start of FILE-EXT wordset )

        %D FILE-STATUS
        %D FLUSH-FILE
        %D REFILL
        %D RENAME-FILE

0= #IF   CR .(  ***  Words missing from FILE-EXT wordset *** )  TRUE
#ELSE   CR .( All words present in FILE-EXT wordset )  TRUE
#THEN

( End of FILE-EXT wordset )

( Start of FLOATING wordset )

        %D >FLOAT
        %D D>F
        %D F!
        %D F*
        %D F+
        %D F-

        %D F/
        %D F0<
        %D F0=
        %D F<
        %D F>D
        %D F@
        %D FALIGN
        %D FALIGNED

        %D FCONSTANT
        %D FDEPTH
        %D FDROP
        %D FDUP
        %D FLITERAL
        %D FLOAT+

        %D FLOATS
        %D FLOOR
        %D FMAX
        %D FMIN
        %D FNEGATE
        %D FOVER
        %D FROT
        %D FROUND
        %D FSWAP

        %D FVARIABLE
        %D REPRESENT

0= #IF   CR .(  ***  Words missing from FLOATING wordset *** )  TRUE
#ELSE   CR .( All words present in FLOATING wordset )  TRUE
#THEN

( End of FLOATING wordset )


( Start of FLOATING-EXT wordset )

        %D DF!

        %D DF@
        %D DFALIGN
        %D DFALIGNED
        %D DFLOAT+
        %D DFLOATS
        %D F**

        %D F.
        %D FABS
        %D FACOS
        %D FACOSH
        %D FALOG
        %D FASIN
        %D FASINH

        %D FATAN
        %D FATAN2
        %D FATANH
        %D FCOS
        %D FCOSH
        %D FE.
        %D FEXP

        %D FEXPM1
        %D FLN
        %D FLNP1
        %D FLOG
        %D FS.
        %D FSIN

        %D FSINCOS
        %D FSINH
        %D FSQRT
        %D FTAN
        %D FTANH
        %D F~
        %D PRECISION
        %D SET-PRECISION

        %D SF!
        %D SF@
        %D SFALIGN
        %D SFALIGNED
        %D SFLOAT+

        %D SFLOATS

0= #IF   CR .(  ***  Words missing from FLOATING-EXT wordset *** )  TRUE
#ELSE   CR .( All words present in FLOATING-EXT wordset )  TRUE
#THEN

( End of FLOATING-EXT wordset )

( Start of LOCAL wordset )

        %D (LOCAL)

        %D TO

0= #IF   CR .(  ***  Words missing from LOCAL wordset *** )  TRUE
#ELSE   CR .( All words present in LOCAL wordset )  TRUE
#THEN

( End of LOCAL wordset )

( Start of LOCAL-EXT wordset )

        %D LOCALS|

0= #IF   CR .(  ***  Words missing from LOCAL-EXT wordset *** )  TRUE
#ELSE   CR .( All words present in LOCAL-EXT wordset )  TRUE
#THEN

( End of LOCAL-EXT wordset )

( Start of MEMORY wordset )

        %D ALLOCATE

        %D FREE
        %D RESIZE

0= #IF   CR .(  ***  Words missing from MEMORY wordset *** )  TRUE
#ELSE   CR .( All words present in MEMORY wordset )  TRUE
#THEN

( End of MEMORY wordset )

( Start of TOOLS wordset )

        %D .S
        %D ?
        %D DUMP
        %D SEE

        %D WORDS

0= #IF   CR .(  ***  Words missing from TOOLS wordset *** )  TRUE
#ELSE   CR .( All words present in TOOLS wordset )  TRUE
#THEN

( End of TOOLS wordset )

( Start of TOOLS-EXT wordset )

        %D ;CODE
        %D AHEAD

        %D ASSEMBLER
        %D BYE
        %D CODE
        %D CS-PICK

        %D CS-ROLL
        %D EDITOR
        %D FORGET
        %D STATE

        %D [ELSE]
        %D [IF]
        %D [THEN]

0= #IF   CR .(  ***  Words missing from TOOLS-EXT wordset *** )  TRUE
#ELSE   CR .( All words present in TOOLS-EXT wordset )  TRUE
#THEN

( End of TOOLS-EXT wordset )

( Start of SEARCH wordset )

        %D DEFINITIONS
        %D FIND
        %D FORTH-WORDLIST
        %D GET-CURRENT
        %D GET-ORDER

        %D SEARCH-WORDLIST
        %D SET-CURRENT
        %D SET-ORDER
        %D WORDLIST

0= #IF   CR .(  ***  Words missing from SEARCH wordset *** )  TRUE
#ELSE   CR .( All words present in SEARCH wordset )  TRUE
#THEN

( End of SEARCH wordset )

( Start of SEARCH-EXT wordset )

        %D ALSO
        %D FORTH

        %D ONLY
        %D ORDER
        %D PREVIOUS

0= #IF   CR .(  ***  Words missing from SEARCH-EXT wordset *** )  TRUE
#ELSE   CR .( All words present in SEARCH-EXT wordset )  TRUE
#THEN

( End of SEARCH-EXT wordset )

( Start of STRING wordset )

        %D -TRAILING
        %D /STRING
        %D BLANK
        %D CMOVE
        %D CMOVE>

        %D COMPARE
        %D SEARCH
        %D SLITERAL

0= #IF   CR .(  ***  Words missing from STRING wordset *** )  TRUE
#ELSE   CR .( All words present in STRING wordset )  TRUE
#THEN

( End of STRING wordset )

( Start of STRING-EXT wordset )

        ( Empty wordset )

0= #IF   CR .(  ***  Words missing from STRING-EXT wordset *** )  TRUE
#ELSE   CR .( All words present in STRING-EXT wordset )  TRUE
#THEN
DROP    NOSTACK1

( End of STRING-EXT wordset )



( END OF TEST )












