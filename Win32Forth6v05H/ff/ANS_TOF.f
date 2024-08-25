
\ MULTI..WHILE..REPEAT provides a means to do something zero or more times.
\ For example, 4 >R MULTI R@ REPEAT R>DROP puts 3 2 1 0 on the stack.
\ It translates cleanly to machine code: bump the cell on top of the return
\ stack and branch conditionally.
\ Literal >R and R>DROP also reduce to small code sequences.

: MULTI ( -- )        ( -- R: x -- x' )
        postpone BEGIN
        postpone R>  postpone 1-  postpone DUP  postpone >R
        postpone 0<  postpone 0=
        postpone WHILE ; immediate

\ The A register is an idea adapted from machine Forth. Having an A register
\ is very handy for indexing and temporary storage.

variable A
: A!    ( addr -- )     A ! ;                      \ set A register
: A@    ( -- addr )     A @ ;                      \ get A register
: @A    ( -- x ) A @ @ ;                           \ fetch cell from A stream
: @A+   ( -- x ) A @ @  [ 1 CELLS ] LITERAL A +! ; \ lift cell from A stream
: C@A   ( -- c ) A @ C@ ;                          \ fetch char from A stream
: C@A+  ( -- c ) A @ C@ [ 1 CHARS ] LITERAL A +! ; \ lift char from A stream
: !A    ( x -- ) A @ ! ;                           \ store cell to A stream
: !A+   ( x -- ) A @ !  [ 1 CELLS ] LITERAL A +! ; \ append cell to A stream
: C!A   ( x -- ) A @ C! ;                          \ store char to A stream
: C!A+  ( x -- ) A @ !  [ 1 CHARS ] LITERAL A +! ; \ append char to A stream
: @R    ( -- x ) ( R: a -- a )    R@ @ ;           \ fetch cell from R stream
: @R+   ( -- x ) ( R: a -- a+4 )  R@ @             \ lift cell from R stream
                     R> [ 1 CELLS ] LITERAL + >R ;
: !R    ( x -- ) ( R: a -- a )    R@ ! ;           \ store cell to R stream
: !R+   ( x -- ) ( R: a -- a+4 )  R@ !             \ append cell to R stream
                     R> [ 1 CELLS ] LITERAL + >R ;


\ IFSET and IFCLR perform non-destructive bit tests on the top of the stack.
\ They are good for parsing a packed field of bit flags. On processors with bit
\ tests, this produces efficient in-line code.
\ IFSET and IFCLR take a parameter at compile time:
\ [ 3 ] IFSET MyStuff THEN is the same as DUP 8 AND IF MyStuff THEN

: IFSET ( bit# -- )   ( n -- n )
        1 swap lshift
        postpone LITERAL
        postpone OVER postpone AND
        postpone IF ; immediate

: IFCLR ( bit# -- )   ( n -- n )
        1 swap lshift invert
        postpone LITERAL
        postpone OVER postpone AND
        postpone IF ; immediate

\ The [ n ] ?[ ... ]? structure conditionally executes a snippet of code.
\ It compares A to a literal and executes if it's a match. Then it exits.

: QCASE: ( -- )  ( c -- )
        postpone A! ; immediate

: ?[    ( c -- )
        postpone LITERAL
        postpone A@ postpone =
        postpone IF ; immediate

: ]?    ( -- )
        postpone EXIT
        postpone THEN ; immediate

\ Other TOF kernel words not in ANS or Win32forth:

: UD2/          ( d -- d/2 )    d2/ [ -1 1 rshift ] literal and ;  \ strip MSB
: C@P           ( addr -- c )   C@ ;    \ char fetch from program memory
: @P            ( addr -- x )   @ ;     \ fetch from program memory
: W@P           ( addr -- x )   W@ ;    \ 16bit fetch from program memory
: C!P           ( c addr -- )   C! ;    \ char store to program memory
: !P            ( x addr -- )   ! ;     \ store to program memory
: COUNTP        ( a -- a' c )   count ; \ get length of string in program memory
: W!P           ( x addr -- )   W! ;    \ 16bit store to program memory
: PCREATE       ( <name> -- )   create ;
: ROM ;
: RAM ;

: >>A           ( x count -- )          \ arithmetic right shift
                -1 OVER RSHIFT INVERT >R  OVER 0< >R
                RSHIFT R> R> AND OR ;

: D0<>          ( d -- f )  D0= 0= ;    \ true if double-cell <> 0

\ Variables used as boolean flags are more readable with boolean-like tests:

: ON?           ( addr -- f )   @ 0<> ; \ flag at addr <> 0?
: OFF?          ( addr -- f )   @ 0= ;  \ flag at addr = 0?

\ Embedded micros can efficiently test bits in memory. So...

: (setupbit)    ( bit# addr -- addr n mask )  DUP >R  1 SWAP LSHIFT  SWAP R> C@ ;

: BIT-ON        ( bit# addr -- )        \ set bit# of char at addr
                (setupbit) OR SWAP C! ;
: BIT-OFF       ( bit# addr -- )        \ clear bit# of char at addr
                (setupbit) INVERT AND SWAP C! ;
: BIT?          ( bit# addr -- f )      \ test bit# of char at addr
                (setupbit) AND NIP 0<> ;


\ Openboot has COMP, which is the F83 compare, not the ANS compare

: COMP          ( a1 a2 len -- f )   TUCK COMPARE ;


\ Some math I like for 16-bit Forths

: M/MOD         ( d n -- r q )          ( floored )     \ signed   d/n --> r q
                DUP 0<  DUP>R
                IF      NEGATE >R DNEGATE R>
                THEN    >R DUP 0<
                        IF      R@ +
                        THEN    R> UM/MOD R>
                IF      SWAP NEGATE SWAP
                THEN    ;

: MU/MOD        ( ud# un1 -- rem d#quot )               \ unsigned ud/u --> ur udq
                >R  0  R@  UM/MOD  R>  SWAP
                >R  UM/MOD  R> ;

: U/MOD         ( u1 u2 -- r q )  DROP UM/MOD ;         \ unsigned u/u --> r q

: UNDER1+       ( x1 x2 -- x1' x2 )  >R 1+ R> ;         \ add 1 to NOS
: UNDER1-       ( x1 x2 -- x1' x2 )  >R 1- R> ;         \ subtract 1 from NOS
: >DIGIT        ( n -- c ) DUP 9 > 7 AND + [CHAR] 0 + ; \ convert digit to ASCII

hex
: C>N           ( c -- n )      dup   80 and 0<>   -80 and or ;
: W>N           ( c -- n )      dup 8000 and 0<> -8000 and or ;
: BYTE-SPLIT    ( n -- cl ch )                          \ split into lo and hi bytes
                DUP 0FF AND SWAP 8 RSHIFT 0FF AND ;
: BYTE-JOIN     ( cl ch -- n )  8 LSHIFT OR ;           \ join lo and hi bytes
: BYTE-SWAP     ( n -- n' ) BYTE-SPLIT SWAP BYTE-JOIN ; \ swap lower 2 bytes of n

\ : WORD-JOIN     ( nl nh -- n ) 10 LSHIFT OR ;           \ join  lo-16 hi-16 --> 32
\ : WORD-SPLIT    ( n -- nl nh )                          \ split 32 --> lo-16 hi-16
\                 DUP 0FFFF AND SWAP 10 RSHIFT 0FFFF AND ;

decimal

: LW,           ( n -- ) byte-split swap c, c, ;        \ comma 16-bit little endian
: C(            postpone ( ; immediate
: CVARIABLE     variable ;

: {{ ; : }} ;   \ ignore {{ }}

variable debugging
: \D debugging @ 0= if postpone \ then ; immediate
: \ND debugging @   if postpone \ then ; immediate

: timestamp     ( -- n )
\ Get 32-bit time stamp starting at Jan 1st, 2000.  1-second resolution.
                time&date 2000 -        ( s m h d m y )
                12 * swap +             \ Rolls over every 133 years.
                31 * swap +
                24 * swap +
                60 * swap +
                60 * + ;

: uf1.0         ( -- d )	0 1 ;	\ unsigned 1.0

: sf            ( <number> -- n )
\ get signed fractional number ( -1.0 to +1.0 ) from input stream,
\ convert to signed integer ( minint .. maxint )
                bl parse >float 0= abort" Expecting fractional number .XXXXX"
                fdup -1e0 f<
                fdup  1e0 f> or abort" Range must be -1.0 .. +1.0"
                uf1.0 d>f 2e0 f/ f* f>d 0=
                if      dup uf1.0 d2/ drop = if 1- then \ clip +1.0 to maxint
                then    ;

: uf            ( <number> -- n )
\ get unsigned fractional number ( 0 to +1.0 ) from input stream,
\ convert to umsigned integer ( 0 .. umaxint )
                bl parse >float 0= abort" Expecting fractional number .XXXXX"
                fdup  0e0 f<
                fdup  1e0 f> or abort" Range must be -1.0 .. +1.0"
                uf1.0 d>f f* f>d
                if      1-              \ clip +1.0 to umaxint
                then    ;

: USB"          ( type_byte string" -- )
\ lay down unicode string with 1-byte embedded descriptor
                >r [char] " word count dup 1+ 2* c, r> c,   ( a n )
         bounds ?do     i c@ c, 0 c,     \ chars are little endian
                loop    ;


