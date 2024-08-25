\ Brad's extensions

: blip          7 _emit ; \ use OS's beep
: pluck         ( abc -- abca ) 2 pick ;
: lay           ( a c -- a' )   over c! char+ ;
: !+            ( a c -- a' )   over  ! cell+ ;
: @+            ( a -- a' n )   dup cell+ swap @ ;
: w@+           ( a -- a' n )   dup 2 + swap w@ ;
: ccompare      ( a1 a2 -- f )  >r count r> count compare ;
: ">$           ( a n -- str )  pad over lay swap move pad ;
: under++       ( n m -- n' m ) swap 1+ swap ;
: under--       ( n m -- n' m ) swap 1- swap ;
: th            ( a n -- a' )   cells + ;
: off?          ( a -- f )      @ 0= ;
: on?           ( a -- f )      @ 0<> ;
: u>=           ( u1 u2  -- f ) u< 0= ;
: 0>=           ( n -- f )      0< 0= ;
: 3swap         ( ... -- ... )                  \ abcdef
                >r rot >r rot >r rot r> r> r>   \ deabcf
                swap >r -rot r> ;               \ defabc
: 3over         ( ... -- ... )                  \ abcdef
                2 pick  2 pick  2 pick ;

hex
: c>n           ( c -- n )      dup   80 and 0<>   -80 and or ;
: w>n           ( c -- n )      dup 8000 and 0<> -8000 and or ;
decimal
: >digit        ( n -- c )      dup 9 > 7 and + '0' + ;
: byte-split    ( n -- n8 n8 )  256 /mod ;
: byte-join     ( n8 n8 -- n )  8 lshift + ;
: byte-swap     ( n16 -- n16' ) 256 /mod swap 8 lshift + ;
: byte-rev      ( n -- n' )     word-split byte-swap swap byte-swap word-join ;
: $type         ( a -- )        count type ;
: stringlength  ( a -- len )    80 tuck bl scan nip - ;
: (u.)          ( u -- a len )  0 (d.) ;
: (.d)          ( n -- a len )  base @ >r decimal (.) r> base ! ;


\ : (.x)          ( n -- a len )
  \       base @ case  2 of s" 0b" pad +place endof
  \                   16 of s" 0x" pad +place endof
  \              endcase (.) pad +place pad count ;

: rightjust     ( a n width -- a' width )
\ right justify text in a field of <width> chars, truncate if needed
                pad over blank  >r
                tuck r@ swap - 0max     ( n a offset )
                pad + rot move          \ place in right part of string
                pad  r> ;

: leftjust      ( a n width -- a width )
\ left justify text in a field of <width> chars, truncate if needed
                >r tuck pad place       ( n | w )
                r@ over - 0max >r       \ # of bytes to blank
                pad 1+ + r> blank
                pad 1+ r> ;

: (h.)          ( n #digits -- a len )
\ format n in hex dump format
                1 max 8 min
                base @ >r hex
                0 tuck          ( n 0 #dig 0 )
                <# ?do # loop #>
                r> base ! ;

: .Y/N          ( f -- )    if ." Y"    else ." N"    then ;

: (string)      ( n atable -- str len )
\ extract string n of a string table
                swap 0
        ?do     count dup 0=
                if      unloop 2drop exit       \ end of list
                then
                + 1+ aligned
        loop    count ;

: .command      ( a len -- )
                "pushkeys 13 pushkey ;

variable tempchar
create outpad 128 allot

: newout        ( -- )          0 outpad c! ;
: >$            ( a n -- )      outpad +place ;
: >c            ( c -- )        tempchar c! tempchar 1 >$ ;
: .byte         ( c -- )        2 (h.) >$ ;
: .word         ( w -- )        4 (h.) >$ ;
: backspace     ( -- )          outpad c@ 1- outpad c! ;
: tab>$         ( n -- )        outpad @ - 1 max spcs swap >$ ;

: cheaplog      ( n -- log2[n] )
                0 swap
                begin   ?dup
                while   1 under+  1 rshift
                repeat  ;

: ,.            ( -- )
        cr ." You probably wanted to use ., here instead of ,."
        true abort" You may remove this safety check from BRADSTUF.G" ; immediate

: def-ext       ( a$ aext lenext -- a$' )
\ append default extension to filename if there is none
                rot count 2dup pad place
                '.' scan nip            ( ae al . )
                if      2drop           \ has extension, do nothing
                else    s" ." pad +place
                        pad +place
                then    pad ;


\ 16-bit CRC-CCITT calculation
\ adopted from W. David Schwaderer's C implementation

hex
create crctable 0000 , 1081 , 2102 , 3183 , 4204 , 5285 , 6306 , 7387 ,
                8408 , 9489 , A50A , B58B , C60C , D68D , E70E , F78F ,

: _crc-ccitt    ( crc c -- crc' )
\ half of CRC calculation
                over xor 0F and         ( crc index )
                cells crctable + @
                swap  4 rshift xor ;

: CRC-CCITT     ( crc c -- crc' )
\ add byte to 16-bit CRC-CCITT, don't feed this a CRC bigger than 16-bit
                dup>r       _crc-ccitt
                r> 4 rshift _crc-ccitt ;
decimal

\ compiler goodies

: base{         postpone BASE postpone @ postpone >R postpone HEX ; immediate
: }base         postpone R> postpone BASE postpone ! ; immediate

: multi         postpone BEGIN postpone R@ postpone 0>=
                postpone WHILE postpone R> postpone 1- postpone >R ; immediate


