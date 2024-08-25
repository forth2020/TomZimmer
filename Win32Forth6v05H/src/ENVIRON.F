\ ENVIRON.F             ENVIRONMENT? support    by Tom Zimmer

cr .( Loading ANS ENVIRONMENT...)

\ Implementation suggested by Andrew McKewan

only forth also definitions
1 #vocabulary environment

: environment?  ( a1 n1 -- false | ?? true )
                ['] environment vcfa>voc search-wordlist
                if      execute true
                else    false
                then    ;

environment definitions

         MAXCOUNTED  CONSTANT /COUNTED-STRING
               0x50  CONSTANT /HOLD
          MAXSTRING  CONSTANT /PAD
                  8  CONSTANT ADDRESS-UNIT-BITS
               TRUE  CONSTANT CORE
               TRUE  CONSTANT CORE-EXT
        -10 7 / -2 = CONSTANT FLOORED

\ -rbs
\         0xFFFFFFFF  CONSTANT MAX-CHAR
0xFF CONSTANT MAX-CHAR

0x7FFFFFFF.FFFFFFFF 2CONSTANT MAX-D
         0x7FFFFFFF  CONSTANT MAX-N
         0xFFFFFFFF  CONSTANT MAX-U
0xFFFFFFFF.FFFFFFFF 2CONSTANT MAX-UD
              0x100  CONSTANT RETURN-STACK-CELLS
              0x100  CONSTANT STACK-CELLS
               TRUE  CONSTANT DOUBLE
               TRUE  CONSTANT DOUBLE-EXT
               TRUE  CONSTANT EXCEPTION
               TRUE  CONSTANT EXCEPTION-EXT
               TRUE  CONSTANT FACILITY
               TRUE  CONSTANT FACILITY-EXT
               TRUE  CONSTANT MEMORY-ALLOC
               TRUE  CONSTANT MEMORY-ALLOC-EXT
               TRUE  CONSTANT FILE
               TRUE  CONSTANT FILE-EXT
               TRUE  CONSTANT TOOLS
               TRUE  CONSTANT TOOLS-EXT
               TRUE  CONSTANT SEARCH-ORDER
               TRUE  CONSTANT SEARCH-ORDER-EXT
              #VOCS  CONSTANT WORDLISTS
               TRUE  CONSTANT STRING
               TRUE  CONSTANT STRING-EXT
           #-LOCALS  CONSTANT #LOCALS
                            : LOCALS TRUE ;
               TRUE  CONSTANT LOCALS-EXT             
               TRUE  CONSTANT WIN32FORTH


forth definitions


