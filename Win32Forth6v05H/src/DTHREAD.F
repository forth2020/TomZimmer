\ DTHREAD.F             Display the Threads             by Tom Zimmer

cr .( Loading Hash Thread Display...)

0 value words-cnt
0 value app-cnt         \ count of application dictionary words
0 value header-cnt

: count-voc     ( voc -- )
                dup voc#threads >r
                dup voc>vcfa @
                dup  doClass  =           \ don't look through classes
                swap do|Class = or 0=     \ or invisible classes
        if      r@ 0
                do      dup i cells +
                        begin   @ ?dup
                        while   1 +to words-cnt
                                dup link> here u<
                                if      1 +to app-cnt
                                then
                                dup l>name c@ 1+ 2 cells+ +to header-cnt
                        repeat  start/stop
                loop
        then    drop r>drop ;

: count-words   ( -- n1 )
                0 to words-cnt
                0 to app-cnt
                0 to header-cnt
                voc-link
                begin   @ ?dup
                while   dup vlink>voc count-voc
                repeat  words-cnt ;

: .1thread      { voc-thread \ thread-depth -- }
                0 to thread-depth
                voc-thread
                begin   ?dup
                while   dup l>name dup nfa-count nip 5 + ?cr .id
                        @
                        1 +to thread-depth
                        start/stop
                repeat  cr ." Thread depth: " thread-depth . cr ;

: 1tcount       { voc-thread \ thread-depth -- n1 } \ get thread depth of voc thread
                0 to thread-depth
                voc-thread
                begin   @ ?dup
                while   1 +to thread-depth
                        start/stop
                repeat  thread-depth ;

: .thread       ( n1 -- )       \ display a thread of context vocabulary
                >r
                context @ dup voc>vcfa >name cr ." Vocabulary: " .id cr
                ." Thread " r@ .
                dup voc#threads ."  of " dup . ."  threads" cr
                r> min 0max cells+ @ .1thread ;

: .threads      ( -- )
                context @ dup voc>vcfa >name cr ." Vocabulary: " .id
                dup voc#threads 0
                do      cr ." Thread: " i . cr
                        dup i cells + @ .1thread
                        start/stop
                loop    drop ;

: .counts       { \ #thread-over -- }
                0 to words-cnt
                0 to app-cnt
                0 to #thread-over
                context @ count-voc
                context @ dup voc>vcfa >name cr ." Vocabulary: " .id cr
                dup voc#threads 0
                do      ." Thread:Depth " 13 ?line

                        getxy     drop 12 <
                        over voc#threads I 1+ = over 0= and
                        if      cr
                        then
                        getcolrow drop 12 < or
                        ?leave
                loop
                dup voc#threads 0
                do      13 ?line
                        ."   "   i 3 .r
                        ."  :"
                        dup i cells + 1tcount
                        dup words-cnt context @ voc#threads / >
                        if      >bold
                                1 +to #thread-over
                        then    3 .r 3 spaces
                        >norm
                        start/stop
                loop    drop
                cr ."   Vocabulary words: " words-cnt .
                        ."  Words over average: "
                #thread-over 100 * context @ voc#threads / 1 .r ." %"
                cr ." Total system words: " count-words . ;

