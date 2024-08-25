((
  Given points y0, y1, y2 and y3, CUBIC4 approximates the missing curve
  between y1 and y2 with a cubic polynomial.  Addr points to y0 in a
  sequential list of cells.  N is the relative distance from y1 on the
  x-axis between y1 and y2.  N = 0..2^(cellbits-1)-1.

  The polynomial is derived as follows, with 0 <= x < 1
  CUBIC4 assumes that N = 2^(cellbits-1) * x.

  f(x) = w0 + w1 * x + w2 * x^2 + w3 * x^3
  For four equally spaced points (n = -1,0,1,2), f(n) gives four
  equations.  Simultaneously solving these equations yields the
  following coefficients upon which the algorithm is based:

  w0 = y1
  w1 = (-2y0 - 3y1 + 6y2 -  y3) / 6
  w2 = ( 3y0 - 6y1 + 3y2      ) / 6
  w3 = ( -y0 + 3y1 - 3y2 +  y3) / 6
))
anew democubic decimal          \ tested using Win32forth

\ Table Lookup Using Cubic Interpolation

8 cells         constant cellbits  \ bits/cell assuming byte addressing
                                   \ change if your address units aren't bytes

1 cellbits 1- lshift 0  2constant wround  \ i.e. 0x00008000 for 16-bit cells

\ : d2*         2dup d+ ;
: d3*           2dup d2* d+ ;           \ quick multiply by constant
: d6*           d2* d3* ;

variable wptr   \ points to the input data

: @seq          ( -- d )                \ get next point for coefficients
                wptr @ @ s>d            ( write in assembly for speed )
                [ 1 cells ] literal wptr +! ;

: seqnew        ( a -- 0.0 )            \ set up for coefficient calculation
                wptr ! 0.0 ;

: w1            ( a -- n )              \ 6 * w1
        seqnew  @seq d2* d-     @seq d3* d-
                @seq d6* d+     @seq d-         drop ;

: w2            ( a -- n )              \ 6 * w2
        seqnew  @seq d3* d+     @seq d6* d-
                @seq d3* d+                     drop ;

: w3            ( a -- n )              \ 6 * w3
        seqnew  @seq d-         @seq d3* d+
                @seq d3* d-     @seq d+         drop ;

: cterm         ( frac n1 n2 -- n3 )    \ n3 = n1 * frac + n2
                >r m* d2* wround d+ nip \ trunc --> round
                r> + ;

: cubic4        ( frac a -- n )         \ frac = 0..maxint
\ perform cubic interpolation on 4-cell table at a
                >r dup dup r@ w3        \ w3
                r@ w2      cterm        \ w3*f + w2
                r@ w1      cterm 6 /    \ (w3*n*n + w2*n + w1) / 6
                r> cell+ @ cterm ;      \ *n + y1

: tcubic        ( n1 addr -- n2 )
\ perform cubic interpolation on table at addr
\ n1 = 0..2^cellsize-1
                dup cell+ >r  @         ( n1 tablesize | addr )
                um*   >r 1 rshift r>    ( frac offset | addr )
                cells r> + cubic4 ;

: CUBIC         ( n1 span addr -- n2 )
\ perform cubic interpolation on table at addr, n1 = 0..span-1
                >r >r 0 swap r> um/mod nip
                r> tcubic ;

((
create exampletable                     \ Sine table (1st quadrant)
   16 ,                                 ( 16 points plus 3 endpoints )
-3212 ,     0 ,  3212 ,  6393 ,  9512 , 12540 , 15447 , 18205 ,
20788 , 23170 , 25330 , 27246 , 28899 , 30274 , 31357 , 32138 ,
32610 , 32767 , 32610 ,                 \ clipped to maxint for 16-bit 4ths

The following would return 32768*sin(10degrees):  10 90 ExampleTable CUBIC

))

((
  The following program tests the above algorithm using an FPU function.
  In this case, the function is 1e9 * sin(0..pi/2).  The more curvature
  there is in a function, the more points are required to represent it
  with much precision.
))

 500 constant maxpoints
  60 value testpoints
  10 value innerpoints  \ # of points to test between each table entry

1e0 fatan 2e0 f* fconstant pi/2

create testtable  maxpoints 4 + cells allot     \ space for the table

: sin(i)        ( n -- sin<n/testpoints*in>*1e9 )
\ change this to try different functions
                s>f testpoints innerpoints * s>d d>f f/         \ 0..1e0
                pi/2 f* 2e0 f*                                  \ 0..pi
                fsin 1e9 f*     f>d drop ;

: makesine      ( -- )
\ create a lookup table using the FPU's sine function
                testpoints testtable !
                testpoints 2 +  -1
                do      i innerpoints * sin(i)
                        testtable i 2 + cells + !
                loop    ;

variable maxerror variable errorpos

: testsine      ( -- )
\ compare the interpolation results to the real function.
                0 maxerror !
                testpoints innerpoints * 0
                do      i testpoints innerpoints *
                          testtable cubic              \ get approximation
                        i sin(i) -                     \ subtract real thing
                        abs dup maxerror @ >
                        if      maxerror !              \ update max error
                                i innerpoints / errorpos !
                        else    drop
                        then
                loop    ;

: err           ( points -- )
\ compute the error for a given table size
                maxpoints min  to testpoints
                cr ." error for " testpoints . ." points is "
                makesine testsine
                maxerror @ .
                ." at table location " errorpos @ . ;

: showme        ( -- )
             cr ." table size vs. error in interpolating 1e9*sin(0..pi)"
                26 1
                do      i 10 * err
                loop    ;

cls showme

