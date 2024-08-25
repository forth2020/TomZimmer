\ POINTER.F     External memory allocation and management   by Tom Zimmer

cr .( Loading Memory Pointer...)

\ BODY          +0             +4      +8            +12
\               [ phy_pointer ][ link ][ size_bytes ][.....

VARIABLE PHEAD  PHEAD OFF

: POINTERS-INIT ( -- )
                PHEAD
                BEGIN   @ ?DUP
                WHILE   DUP CELL- OFF           \ clear each pointer in turn
                REPEAT  ;

initialization-chain chain-add pointers-init

in-system

: TRIM-POINTERS ( a1 -- a1 )
                PHEAD
                BEGIN   @ ?DUP
                WHILE   2DUP TRIM?
                        IF      DUP CELL- @ ?DUP
                                IF      RELEASE
                                THEN
                        THEN
                REPEAT  DUP PHEAD FULL-TRIM ;

\in-system-ok forget-chain chain-add trim-pointers

: .POINTERS     ( -- )
                PHEAD
                BEGIN   @ ?DUP
                WHILE   DUP CELL- BODY> >R
                        CR DUP CELL+ @ 10 U,.R ."  bytes at: "
                        DUP CELL- @ ?DUP
                        IF      13 H.R
                        ELSE    ." <unallocated>"
                        THEN    ."  for: " R> .NAME
                REPEAT  ;

in-application

: ?VALID_POINTER ( plink -- plink )     \ check for valid pointer sturcture
                 DUP 2 cells+ @ OVER <>
                 IF      cell - BODY> DUP CR h. .name
                         ." is NOT a valid POINTER structure!"
                         ABORT
                 THEN    ;

: %UnPointer    ( cfa -- )      \ deallocate pointer given the cfa
                >BODY DUP>R @ 0<>                       \ only if non-zero
                IF      R@ cell+ ?VALID_POINTER DROP
                        R@ @ release            \ release the memory
                        r@ off                  \ clear the memory pointer
                THEN    R>DROP ;

: %SizeOf!      ( bytes cfa -- )    \ set the size of pointer "cfa"
                >BODY cell+ ?VALID_POINTER cell+ ! ;

: %SizeOf@      ( cfa -- bytes )    \ get the size of pointer "cfa"
                >BODY cell+ ?VALID_POINTER cell+ @ ;

: Pointer       ( bytes -<name>- )              \ make a pointer "name"
                here app-here u>
                sys-warning? and
                abort" Can't define a pointer in system dictionary!"
                ( -- address )                  \ it's location
                128 max                         \ at least 160 bytes
                CREATE 0 ,                      \ initialize to unallocated
                HERE  PHEAD @ ,  PHEAD !        \ link into chain
                ,                               \ lay in size in bytes
                PHEAD @ ,                       \ POINTER CHAIN verify word
                DOES>   dup @ 0=
                if      dup 2 cells+ @ malloc ?dup
                        if      dup off         \ set first cell to zeros
                                over !
                        else    true
                                abort" Failed to allocate POINTER"
                        then
                then    @ ;

: ResizePointer ( size -<Pointer>- flag )       \ return FALSE if all went ok
                dup                             \ extra copy of size
                r@ abs>rel @ >body              \ get address of pointer following in memory
                r> cell+ >r                     \ skip to following cell
                dup>r 2 cells+ !                \ first, adjust the size of the desired pointer
                r@ @ 0=
                IF      r>drop                  \ if it isn't allocated yet, just leave
                        drop FALSE              \ things went just fine.
                        EXIT                    \ April 15th, 1999 - 10:51 tjz corrected 
                THEN        
                dup r@ @ realloc 0=
                if      r> ! FALSE
                else    drop r>drop TRUE
                then    ;

in-system

: UnPointer>    ( name -- )     \ deallocate a pointer name following
                ' STATE @
                IF      POSTPONE LITERAL
                        POSTPONE %UNPOINTER
                ELSE    %UNPOINTER
                THEN    ; IMMEDIATE

: SizeOf!>      ( bytes | name -- ) \ set size of the following pointer
                ' STATE @
                IF      POSTPONE LITERAL
                        POSTPONE %SIZEOF!
                ELSE    %SIZEOF!
                THEN    ; IMMEDIATE

: SizeOf@>      ( -<name>- bytes )  \ get size of the following pointer
                ' STATE @
                IF      POSTPONE LITERAL
                        POSTPONE %SIZEOF@
                ELSE    %SIZEOF@
                THEN    ; IMMEDIATE

: named-new$    ( -<name>- )            \ a semi-static buffer of MAXSTRING length
                MAXSTRING Pointer ;

in-application

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\               Dynamic String Support Words
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

\ The dynamic string buffer is allocated when it is first used, and automatically
\ released when the program terminates.

8192 constant max-dyn-string           \ dynamic string space is this big

max-dyn-string Pointer dyn-ptr          \ the dynamic string buffer

0 value dyn-offset                      \ top of the dynamic string buffer

: DynAlloc      ( n1 -- a1 )            \ allocate space in dynamic buffer
                                        \ return a1 a buffer of n1 bytes
                dup
                max-dyn-string 4 / >    \ limit one string to 1/4 of total
                abort" Can't allocate a dynamic string this large"
                dyn-offset over + max-dyn-string >=
                if      0 to dyn-offset
                then    dyn-ptr dyn-offset + swap +to dyn-offset ;

: _new$         ( -- a1 )               \ allocate the next MAXSTRING buffer
                MAXSTRING DynAlloc ;

' _new$ is new$

