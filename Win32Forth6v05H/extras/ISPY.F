\ ISPY.F        For debugging in difficult I/O routines.
\ rls - April 9th, 2002 - 0:30
cr .( Loading I Spy )

ANEW ~ISPY

1024 constant ISPYMAX
0 value ISPYNEXT
create ISPYBUF
ISPYMAX cells allot

false value FLAG1
false value FLAG2
false value FLAG3

variable COUNTER1
variable COUNTER2
variable COUNTER3

: BREAKER1  NOOP ;
: BREAKER2  NOOP ;
: BREAKER3  NOOP ;


: ISPY!         ( n -- )
        ISPYNEXT ISPYMAX <
        IF      ISPYBUF ISPYNEXT cells+ !
                1 +to ISPYNEXT
        ELSE    drop BREAKER
        THEN ;

: INITISPY       ( -- )
        0 to ISPYNEXT
        ISPYBUF ISPYMAX cells 0 FILL
        false to FLAG1
        false to FLAG2
        false to FLAG3
        0 COUNTER1 !
        0 COUNTER2 !
        0 COUNTER3 !
        ;

: ISPY          ( n1 n2 ... nn n -- )
        depth 1- 0max min 0
        ?DO     ISPY!
        LOOP ;

: NDROP         ( n1 n2 nn n -- )
        depth 1- 0max min 0
        ?DO     drop
        LOOP ;

: ?ISPY         ( n1 n2 ... nn n flag -- )
        IF      ISPY
        ELSE    NDROP
        THEN ;

: .ISPYN         ( n -- )
        cr 0
        ?DO
                ISPYBUF I cells + @ dup 0xFFFFFF00 and 0x8000 =
                IF      ."  Hex: " 8 h.r
                ELSE    14 .r
                THEN
                I 3 and 3 = IF  cr      THEN
        LOOP ;

: .ISPY         ( -- )
        ISPYNEXT .ISPYN ;

\ Insert ISPY at appropriate places in the source code.  It takes any number of
\ parameters, as indicated.  Or you may wish to use conditional SPYing by using
\ ?SPY .  When the activity is terminated, examine the trace by dumping SPYBUF ,
\ or by using .ISPY .

\S

\ ***********************************************************************

\ Printing out the Virtual Page

: .LINETO       ( a1 n1 -- a1 n1 )
        cr ." LINETO     " over dup c@ 4 .r 1+ dup sw@ 4 .r 2 + sw@ 4 .r ;

: .MOVETO       ( a1 n1 -- a1 n1 )
        cr ." MOVETO     " over dup c@ 4 .r 1+ dup sw@ 4 .r 2 + sw@ 4 .r ;

: .TEXTOT       ( a1 n1 -- a1 n1 )
        cr ." TEXTOT     " over dup c@ 4 .r 1+ dup sw@ 4 .r 2 + dup sw@ 4 .r
        2 + count dup 4 .r 50 min 2 spaces type ;

: .TEXTOTR      ( a1 n1 -- a1 n1 )
        cr ." TEXTOTR    " over dup c@ 4 .r 1+ dup sw@ 4 .r 2 + dup sw@ 4 .r
        2 + count dup 4 .r 50 min 2 spaces type ;

: .TEXTOTL      ( a1 n1 -- a1 n1 )
        cr ." TEXTOTL    " over dup c@ 4 .r 1+ dup sw@ 4 .r 2 + dup sw@ 4 .r
        2 + count dup 4 .r 50 min 2 spaces type ;

: .TEXTOTF      ( a1 n1 -- a1 n1 )
        cr ." TEXTOTF    " over dup c@ 4 .r 1+ dup @ 9 .r 4 + dup sw@ 4 .r 2
        + dup sw@ 4 .r 2 + count dup 4 .r 50 min 2 spaces type ;

: .LINECOLOR    ( a1 n1 -- a1 n1 )
        cr ." LINECOLOR  " over dup c@ 4 .r 1+ @ 9 .r ;

: .FILLAREA     ( a1 n1 -- a1 n1 )
        cr ." FILLAREA   " over dup c@ 4 .r 1+ dup sw@ 4 .r 2 + dup sw@ 4 .r
        2 + dup sw@ 4 .r 2 + dup sw@ 4 .r 2 + @ 9 .r ;

: .PAGE         ( a1 n1 -- a1 n1 )
        cr ." PAGE       " over c@ 4 .r ;

: .MPAGE        ( a1 n1 -- a1 n1 )
        cr ." MPAGE      " over c@ 4 .r ;

: .SETPIXEL     ( a1 n1 -- a1 n1 )
        cr ." SETPIXEL   " over dup c@ 4 .r 1+ dup sw@ 4 .r 2 + dup sw@ 4 .r
        2 + @ 9 .r ;

: .BEZIERTO     ( a1 n1 -- A1 n1 )
        cr ." BEZIERTO   " over dup c@ 4 .r 1+ count 4 .r 9 .r ;

: .BEGINPATH    ( a1 n1 -- A1 n1 )
        cr ." BEGINPATH  " over c@ 4 .r ;

: .ENDPATH    ( a1 n1 -- A1 n1 )
        cr ." ENDPATH    " over c@ 4 .r ;

: .FILLPATH    ( a1 n1 -- A1 n1 )
        cr ." FILLPATH   " over c@ 4 .r ;

: .STROKEPATH  ( a1 n1 -- A1 n1 )
        cr ." STROKEPATH " over c@ 4 .r ;

: .STROKEANDFILL ( a1 n1 -- A1 n1 )
        cr ." STROKEANDFILL " over c@ 4 .r ;

: .DRAWLIST     ( -- )
        cr ."   DRAWLIST Dump"
        cr ." DRAWOFF = " mydrawoff . cr
        mydrawlist
        BEGIN   count over c@
        WHILE   over c@
                CASE
                         5 OF      .TEXTOTL        ENDOF
                         1 OF      .LINETO         ENDOF
                         2 OF      .MOVETO         ENDOF
                         3 OF      .TEXTOT         ENDOF
                         4 OF      .TEXTOTR        ENDOF
                         6 OF      .LINECOLOR      ENDOF
                         7 OF      .FILLAREA       ENDOF
                         8 OF      .PAGE           ENDOF
                         9 OF      .MPAGE          ENDOF
                        10 OF      .SETPIXEL       ENDOF
                        11 OF      .TEXTOTF        ENDOF
                        12 OF      .BEZIERTO       ENDOF
                        13 OF      .BEGINPATH      ENDOF
                        14 OF      .ENDPATH        ENDOF
                        15 OF      .FILLPATH       ENDOF
                        16 OF      .STROKEPATH     ENDOF
                        17 OF      .STROKEANDFILL  ENDOF
                        cr abort" Error in DrawList"
                ENDCASE
                +
        REPEAT 2drop ;

