((
Binary Search by Charles Melice

When the key is not found, returns the position of the nearest greater key.
Can be used to insert a new key in the sorted array.

    BSEARCH  ( key array count -- index flag )
        count   count of elements in array.
        array   SORTED array of anything.
        key     the key we are searching for.
        flag    TRUE if key was found.
        index   effective else virtual key position in array.

and, to define please:

    DEFER GET-KEY  ( index array -- key )
        array   sorted array of anything.
        index   index
        key     the value at array[index]

    DEFER B-COMPARE  ( key1 key2 -- result )
        if key1 < key2, return -1
        if key1 > key2, return +1
        else return 0.

))

anew foo

DEFER GET-KEY    ( index array -- key )
DEFER B-COMPARE  ( key1 key2 -- result )

: BSEARCH       ( key array count -- index flag )
                1- 0 0 LOCALS| mid lo hi array key |
        BEGIN   lo hi <= WHILE
                lo hi + 2/ TO mid
                key mid array GET-KEY B-COMPARE
                CASE   -1 OF  mid 1- TO hi   ENDOF
                        1 OF  mid 1+ TO lo   ENDOF
                        0 OF  mid TRUE EXIT  ENDOF
                ENDCASE
        REPEAT  lo hi MAX \ this computes the insertion point
                FALSE ;

(( non-local version [Wil Baden]
    : UNDER  ( x y z -- z y )  ROT DROP  SWAP ; 
    : BSEARCH                        ( key array count -- index flag )

        SWAP >R                        ( key count)( R: array)
        0 SWAP                         ( key lo hi)

        BEGIN  2dup < WHILE
            3dup +  2/  TUCK           ( . . . mid key mid)
            R@ GET-KEY B-COMPARE       ( . . . mid flag)
            0> IF  1+ UNDER               \  mid 1+ to lo
            ELSE   NIP                    \  mid to hi
            THEN
        REPEAT                         ( key lo hi)

        NIP TUCK                       ( index key index)
        R> GET-KEY B-COMPARE 0= ;
))



\ Test.

create array
    here
    0 , 3 , 12 , 23 , 45 , 66 , 88 ,
    here swap - cell / constant NELEM

:noname  ( a b -- res )
    2dup > If 2drop 1 Exit Then < ; IS B-COMPARE

:noname ( index array -- key )
    swap cells + @ ; IS GET-KEY

: TEST  ( key -- )
    array NELEM bsearch cr
    IF ." FOUND = " ELSE ." NOT FOUND, insert = " THEN . cr ;



