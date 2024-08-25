\ Compiling options
0 constant mute                         \ disable printing for benchmark
0 constant has-colour                   \ all colour display
1 constant 6X10                         \ do 6x10 board, if false do 4x15 board

11 constant Wtot                        \ total width of board
create Bd  500 allot                    \ allow 11 X 30 board
create Pa  12 allot                     \ pieces available
variable Level                          \ recursion level, = # pieces on board
0 value Width                           \ board width actually used
0 value Height                          \ board height actually used
0 value Bstart                          \ pointer to starting square on board
0 value Bend                            \ pointer to ending square on board
variable Soln                           \ # of this solution
variable Tries                          \ count of pieces tried

: pentom-init ( wd ht - )               \ initialise the board & pieces
    to Height  to Width
    Width 1+ Wtot > abort"  Total width too small"
    Wtot Bd + to Bstart                 \ set pointer to start of board
    Wtot Height * Width + Bd + to Bend  \ set pointer to end of board
    0 Bd + Wtot Height 2 + * 1+ -1 fill \ set unused squares to -1
    12 0 do                             \ mark all pieces available
        -1 i Pa + c!
    loop
    0 9 Pa + c!                         \ except the X
    Height 1+ 1 do
        Width 1+ 1 do
            0 j Wtot * i + Bd + c!      \ set unoccupied squares to 0
        loop
    loop ;

create pats
    bl c, bl c,                         \ -1 is boundary, 0 is empty
    char f c, char i c, char l c, char n c,
    char p c, char t c, char u c, char v c,
    char w c, char x c, char y c, char z c,
    char x c,                           \ 13 is manually placed 'x'

: printbd
    mute if exit then
    0 2 at-xy
    Height 1+ 1 do                      \ for each row
        Width 1+ 1 do                   \ and each col
            j Wtot * i +  Bd + c@       \ # in square
[ has-colour ] [if]
            4 lshift set-colour         \ use coloured spaces to 'prettyify'
            2 spaces
            7 set-colour
[else]
            1+ pats +                   \ piece name
            c@ emit                     \ plain jane emit piece names
[then]
        loop cr
    loop
    ." Solution " Soln @ .
    ."  Pieces tried = " Tries @ . cr cr
    key? if
        key 27 = abort"  User aborted "
        key drop
    then ;

\ ************** Start of Guts ****************************

\ I hope you can handle long strings; the following should be
\  one long string of 181 characters ending with 'ai.....'
create orients ," ABCDEiIlJyKyLl.IHnJpKuQv.JKpRt.KLnSv..IHGnJpPwQf.JKpQpRp.QPzRuYl..JKLnRfSw.RQuSzZl...IHGFlJyOzPfQt.JKyPfRf.POwQpXn.QRfYy..JKCuLlQtRfSz.QPfRpYy.RSwZn..QPOvRtXnYy.RSvYyZn.YXlZlai....."
\ 'x' omitted by replacing 'JKyPfQxRf.' with 'JKyPfRf.'

create pos-stack 5 allot                \ stack to hold rel positions
variable posptr  pos-stack posptr !

: push-relpos ( relpos -- )
    posptr @ c!  1 posptr +! ;
: pop-relpos
    -1 posptr +! ;

\ factors for leaf-test
: place-piece ( p# -- )                 \ code to put piece on board
    pos-stack 5 over + swap do
        dup postpone literal
        postpone over i c@ postpone literal postpone +
        postpone c!
    loop drop ;

: lift-piece                            \ code to remove piece from board
    pos-stack 5 over + swap do
        0 postpone literal
        postpone over i c@ postpone literal postpone +
        postpone c!
    loop ;

\ defer is non-ans but everybody has it
defer next-piece                        \ forward reference

\ macro to generate code to recursively test availability of a piece
\  and mark the board and the piece availability accordingly
: leaf-test ( pc# -- )
    Pa + >r        ( R: pc-addr )
    r@ postpone literal postpone c@ postpone if \ is piece available?
        0 postpone literal r@ postpone literal postpone c! \ mark unavailable
        1 postpone literal postpone Tries postpone +! \ inc Tries
        r@ Pa - 1+ place-piece
        postpone dup postpone next-piece
        lift-piece
        -1 postpone literal r> postpone literal postpone c! \ mark available
    postpone then ;

\ factor for testsq
: sq@0= ( relpos -- )                   \ current square empty?
    postpone dup postpone literal postpone +
    postpone c@ postpone 0= ;

create piece#                           \ convert piece names to numbers
\      f..i..l.n.p...tuvwxyz
    ," 0xx1xx2x3x4xxx56789:;"

variable optr                           \ pointer into orients
    orients count drop 1- optr !

\ macro to generate code to recursively find a piece that fits at lead square
\  traverses the string orients to generate code
: testsq
    begin                               \ repeat
        1 optr +!                       \  for each char in orients
        optr @ c@ [char] . = if         \  until recursion is done
            exit
        then
        optr @ c@ [char] a > if         \ at a piece name?
            optr @ c@                   \ yes
            [char] e - piece# + c@ [char] 0 - \ so convert to a number 0-11
            leaf-test exit              \ at leaf so test piece availability
        then
        optr @ c@ [char] A - 3 +
        8 /mod Wtot * swap 3 - + >r  ( R: relpos ) \ posn rel to lead
        r@ sq@0= postpone if            \ square empty?
            r@ push-relpos              \ push to rel posn stack
            recurse
            pop-relpos                  \ pop rel posn stack
        postpone then
        r> drop
    again ; immediate

: soln-print
    1 Soln +!  printbd ;

\ find next piece that fits lead square
:noname ( lead-sq -- )
    1 Level +!                          \ next level, i.e. place a piece
    begin                               \ loop back here
        dup 1+ Bend > if                \ at end of board yet?
            soln-print                  \ yes, so print solution
            -1 Level +!                 \ previous level, i.e. lift up piece
            drop exit                   \ exit when at end of board
        then
        1+                              \ next square
        testsq                          \ place all pieces at lead square
    dup c@ 0= until                     \ loop until lead square is empty
    drop  -1 Level +!
; is next-piece

: solve
    0 Level !                           \ no pieces on board
    Bstart next-piece ;

\ ************** End of Guts ****************************

: placex ( x y val -- )                 \ place or lift the X pattern
    >r              ( x y R: val )
    2dup Wtot * + Bd + r@ swap c!
    2dup 1+ Wtot * + 1- Bd + r@ swap c!
    2dup 1+ Wtot * + Bd + r@ swap c!
    2dup 1+ Wtot * + 1+ Bd + r@ swap c!
    2 + Wtot * + Bd + r> swap c! ;

: x-at ( x y - )                        \ place the X; solve; then lift the X
    2dup 13 placex
    solve
    0 placex ;

: p6*10                                 \ 6 X 10 puzzle 2339 solutions
    6 10 pentom-init                    \ other boards may be constructed
    3 1 x-at                            \ 'x' in upper left quadrant
    2 2 x-at
    3 2 x-at
    2 3 x-at
    3 3 x-at
    2 4 x-at
    3 4 x-at ;

: p4*15                                 \ 4 X 15 puzzle 402 solutions
    4 15 pentom-init
    2 2 x-at
    2 3 x-at
    2 4 x-at
    2 5 x-at
    2 6 x-at
    2 7 x-at ;

variable starttime
: read-secs
    time&date drop 2drop 3600 * swap 60 * + + ;
: start-timing
    read-secs starttime ! ;
: elapsed-time ( -- secs )
    read-secs starttime @ - ;

: pentom
    page
    ." Solutions to the Pentomino Puzzle by Exhaustive Search" cr
    ." Press any key to pause, Esc to abort " cr
    0 Soln !   0 Tries !
    start-timing
[ 6X10 ] [if]
    p6*10
[else]
    p4*15
[then]
    elapsed-time
    cr cr
    ." Total solutions = " Soln @ .
    ."  Total pieces tried = " Tries @ . cr
    ." Elapsed time in secs = " .
;

elapse pentom






