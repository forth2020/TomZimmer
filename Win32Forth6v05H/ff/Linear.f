0 [IF]
Table Lookup Using Linear Interpolation

Author: Brad Eckert     bradbits@hotmail.com    9/30/99

This algorithm finds the point on a line joining points in a lookup table.

This implemention is in ANS Forth using the CORE, CORE EXT and DOUBLE wordsets.

To find the worst case error, let g(x) be the second derivative of f(x).  Find
the value of x that gives the highest absolute value of g(x).  Feed LINEAR a
value of x that lands midway between two table entries. Then, compare the
result to f(x).

---------------------------------------------------------------------------
[THEN] anew lineardemo

\ Table Lookup Using Linear Interpolation

8 cells         constant cellbits  \ bits/cell assuming byte addressing
                                   \ change if your address units aren't bytes

1 cellbits 1- lshift 0  2constant wround  \ i.e. 0x00008000 for 16-bit cells

: LINEAR        ( n1 addr -- n2 )
\ perform interpolation on table at addr
\ n1 = 0..2^cellsize-1
                dup cell+ >r @ um*              ( n1*tablesize | addr )
                >r dup invert u2/ swap u2/ r>   ( frac1 frac0 offset | addr )
                cells r> + 2@                   ( fr1 fr0 y0 y1 )
                >r m* rot r> m* d+
                d2* wround d+ nip ;

: SLINEAR       ( n1 span addr -- n2 )
\ perform linear interpolation on table at addr, n1 = 0..span-1
                >r >r 0 swap r> um/mod nip
                r> linear ;

create ExampleTable                     \ Sine table (1st quadrant)
   16 ,                                 ( 16 points plus 1 endpoint )
    0 ,  3212 ,  6393 ,  9512 , 12540 , 15447 , 18205 , 20788 ,
23170 , 25330 , 27246 , 28899 , 30274 , 31357 , 32138 , 32610 ,
32767 ,                                 \ clipped to maxint for 16-bit 4ths

.( 32768*sin[10degrees] is )  10 90 ExampleTable SLINEAR .



