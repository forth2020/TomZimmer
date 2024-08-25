\ profiler for Win32f                                   by Tom Zimmer

0 value pro-ptr         \ the profilers parrallel array pointer

: profile-init  ( -- )  \ setup to profile the Forth dictionary
                pro-ptr ?exit
                &app-size @ malloc dup 0=                   \ 2-meg dictionary
                abort" Couldn't initialize the profiler!"
                to pro-ptr
                pro-ptr &app-size @ erase ;                 \ prefill with 0

\ initialization-chain chain-add profile-init

create twelve-blanks    0x20 c, 0x20 c, 0x20 c, 0x20 c,
                        0x20 c, 0x20 c, 0x20 c, 0x20 c,
                        0x20 c, 0x20 c, 0x20 c, 0x20 c,

create one-crlf         0x0D c, 0x0A c,

: profile-bye   ( -- )  \ finish up profiling the Forth dictionary
                pro-ptr 65536 < ?exit
                s" PROFILE.INF" r/w create-file
                abort" Couldn't create PROFILE.INF"
                >r                              \ save the file handle
                pro-ptr &app-size @
                ['] pro-ptr /string             \ ignore all lower words
                cell /                          \ length is in CELLS
                begin   0 lskip dup
                while   over
                        dup 0 <# #s #> 12 over -
                        twelve-blanks swap   r@ write-file abort" Write error to PROFILE.INF"
                                             r@ write-file abort" Write error to PROFILE.INF"
                        twelve-blanks 3      r@ write-file abort" Write error to PROFILE.INF"
                        over >name nfa-count r@ write-file abort" Write error to PROFILE.INF"
                        one-crlf 2           r@ write-file abort" Write error to PROFILE.INF"
                        1- swap 4 - swap        \ skip to next word
                repeat  2drop
                r> close-file drop ;

\ unload-chain chain-add-before profile-bye

: profile-enable ( -- )         \ activate profiling
                0 to pro-ptr ;

: profile-disable ( -- )        \ don't activate profiling
                -1 to pro-ptr ;

profile-disable



: do-pro        ( -- )
                r>
                1 r@ abs>rel cell - body> pro-ptr + +!
                >r ;

: do-profile+   ( -- )
                pro-ptr 65536 < ?exit           \ leave if not initialized
                do-pro ;

: _profile:     ( -<name>- )
\+ Parms-init   Parms-init
                header
                hide !csp
                docol ,
                POSTPONE do-profile+
                ]
                ;

: profiling-off ( -- )  \ don't even include the profiling word in each def
                ['] _: is : ;

: profiling-on  ( -- )  \ include the profiling word in each definition
                ['] _profile: is : ;


profiling-off                           \ default to no profiling at all


