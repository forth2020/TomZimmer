\ monte carlo integer arithmetic testing

2variable test-tally   0 0 test-tally 2!

\ good values for 16-bit systems: 61BF 62DC 6594 6363 5E9B 65E8
\             for 32-BIT  7549D83B 07DCB7DC 6A364C22 62AFDC7A
\             RNG implementation by C. G. Montgomery

0x61BF constant rmult

2variable rloc  111 rloc !    \ seed with nonzero

: rndm          ( -- u )
        rloc 2@ rmult um* rot 0 d+ over rloc 2! ;

: random        ( n -- 0..n-1 )    rndm um* nip ;

20 cells array errortable

errortable 20 cells erase

: zerologic     ( x1 x2 -- 0 )
\ fed any two values, the result should be zero
                2dup xor >r             \ test xor
                2dup and invert >r      \ test and
                or r> and               \ test or
                r> xor ;                \ results should cancel

: zeroadd       ( x1 x2 -- 0 )
\ fed any two values, the result should be zero
                2dup invert + >r        \   (a - ~b)
                swap invert + r> +      \ + (b - ~a)
                0 invert 2* - ;         \ - (2*~0)

: zeromult      ( x1 x2 -- 0 )
\ fed any two values, the result should be zero
                over
        if      2dup >r >r um*  ( xy | y x )
                2r@ + rot rot   ( z xy | y x )
                pluck 0 d+      ( z xy+z | y x )
                r@ um/mod       ( z rz y+qz | y x )
                2r> >r -        ( z rz qz | x )
                r> um* + + -
        else    and             \ don't test if x1=0
        then    ;

: ?fail         ( x1 x2 depth ior error# -- x1 x2 )
\ log error if ior<>0 or depth mismatch
                >r >r depth 1- <> r> or
                if      2dup r> 2* cells errortable + 2!
                else    r> drop
                then    ;

: suite         ( x1 x2 -- )
\ test x1,x2 using a battery of tests
                depth 2dup zerologic 3 ?fail
                depth 2dup zeroadd   4 ?fail
                depth 2dup zeromult  5 ?fail
                2drop ;

: monty         ( -- )
\ perform a random test on the test suite
                pause rndm rndm suite
                1. test-tally 2@ d+
                test-tally 2! ;

