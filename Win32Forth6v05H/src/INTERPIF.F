( interpif.f )
( interpretive if control structure )
( Placed in the public domain on 8aug96, by Jim Schneider )
\ August 9th, 1996 - 10:38 tjz slight modifications for Win32Forth

INTERNAL

: definite-word ( definitely get a word from the input stream )
                ( delim -<chars>- c-addr )
                begin   dup word dup c@ 0=
                while   drop refill 0=
                        abort" Input stream exhausted!"
                repeat  nip ;

: match-[if]    ( possibly match an [if] )
                ( if count \ c-addr -- if count' \ c-addr )
                dup count s" [IF]" compare 0= if swap 1+ swap then ;

: match-[then]  ( possibly match a [then] )
                ( if count \ c-addr -- if count' \ c-addr )
                dup count s" [THEN]" compare 0= if swap 1- swap then ;

: match-[else]  ( possibly match an [else] )
                ( if count \ c-addr -- if count' \ c-addr )
                dup count s" [ELSE]" compare 0= if swap 1- dup if 1+ then swap then ;

: match-#if     ( possibly match an #if )
                ( if count \ c-addr -- if count' \ c-addr )
                dup count s" #IF" compare 0= if swap 1+ swap then ;

: match-#then   ( possibly match a #then )
                ( if count \ c-addr -- if count' \ c-addr )
                dup count s" #THEN" compare 0= if swap 1- swap then ;

: match-#endif  ( possibly match a #endif )
                ( if count \ c-addr -- if count' \ c-addr )
                dup count s" #ENDIF" compare 0= if swap 1- swap then ;

: match-#else   ( possibly match an #else )
                ( if count \ c-addr -- if count' \ c-addr )
                dup count s" #ELSE" compare 0= if swap 1- dup if 1+ then swap then ;

EXTERNAL

: [else]        ( interpretive control structure -- alternate selection )
                ( -- )
                1
                begin   ?dup
                while   bl definite-word ?uppercase
                        match-[if]
                        match-[then]
                        match-[else] drop
                repeat  ; immediate

: [if]          ( interpretive control structure -- select on true )
                ( flag -- )
                0= if postpone [else] then ; immediate

: [then]        ( interpretive control structure -- end the structure )
                ( -- )
                ; immediate

: [endif]       ( interpretive control structure -- end the structure )
                ( -- )
                ; immediate

: #else         ( the alternative selection )
                ( -- )
                1
                begin   ?dup
                while   bl definite-word ?uppercase
                        match-#if
                        match-#then
                        match-#endif
                        match-#else drop
                repeat  ; immediate

: #if           ( begin the interpretive control structure )
                ( flag -- )
                0= if postpone #else then ; immediate

: #then         ( mark the end of the interpretive control structure )
                ( -- )
                ; immediate

: #endif        ( mark the end of the interpretive control structure )
                ( -- )
                ; immediate

\ header file support for conditional compilation

: #ifdef        ( 'definedword' -- )
                defined nip 0= if postpone #else then ; immediate

: #ifndef       ( 'notdefinedword' -- )
                defined nip if postpone #else then ; immediate

MODULE

