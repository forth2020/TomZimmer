\ Vocabulary search order specification

cr .( Loading Vocabulary...)

7 VALUE #THREADS        \ default number of threads for a vocabulary

: #WORDLIST     ( #threads -- wid )
                1 MAX 511 MIN DUP , VOC-LINK LINK,
                HERE DUP>R OVER CELLS ALLOT
                           SWAP CELLS ERASE R> ;

: WORDLIST      ( -- wid )
                #THREADS #WORDLIST ;

: #VOCABULARY   ( #threads -<name>- )
                >SYSTEM
                CREATE  #WORDLIST DROP
                SYSTEM>
                DOES>   BODY> VCFA>VOC CONTEXT ! VOC-ALSO ;

: VOCABULARY    ( -- )
                #THREADS #VOCABULARY ;

1 #VOCABULARY ROOT

' ROOT @ ' FORTH ! \ Patch the FORTH vocabulary to be like other vocabularies

: ALSO          ( -- )
                CONTEXT DUP CELL+  #VOCS 1- CELLS MOVE  ;

: ONLY          ( -- )
                CONTEXT #VOCS CELLS ERASE  ROOT ALSO VOC-ALSO ;

: PREVIOUS      ( -- )
                CONTEXT DUP CELL+ SWAP  #VOCS CELLS MOVE
                CONTEXT @ 0=
                IF      ROOT
                THEN    VOC-ALSO ;

: FORTH-WORDLIST ( -- wid )
                ['] FORTH VCFA>VOC ;

: GET-CURRENT   ( -- wid )
                CURRENT @ ;

: SET-CURRENT   ( wid -- )
                CURRENT ! ;

: GET-ORDER     ( -- widn .. wid1 n )
                DEPTH >R
                0 #VOCS 1-
                DO      CONTEXT I CELLS+ @
                        DUP 0=
                        IF      DROP
                        THEN
            -1 +LOOP    DEPTH R> - ;

: SET-ORDER     ( widn .. wid1 n -- )
                DUP 0<
                IF      DROP ONLY
                ELSE    CONTEXT #VOCS CELLS ERASE
                        0
                        ?DO     CONTEXT I CELLS+ !
                        LOOP    VOC-ALSO
                THEN    ;

: +ORDER        ( wid - )       \ add wid to search order
                >R GET-ORDER 1+ R> SWAP SET-ORDER ;

: SWAP-CURRENT  ( wid1 - wid2 ) \ change current to wid1, return old wid2
                GET-CURRENT SWAP SET-CURRENT ;

: VOC:          ( wid 'word' - ) \ define 'word' in vocabulary wid
                SWAP-CURRENT >R : R> SET-CURRENT ;

: ORDER         ( -- )
                CR ." Context: " CONTEXT
                #VOCS 0
                DO      DUP @ ?DUP
                        IF      voc>vcfa .NAME 14 ?CR
                        THEN    CELL+
                LOOP    DROP
                CR ." Current: " CURRENT @  voc>vcfa .NAME    ;

: VOCS          ( -- )
                cr ." Vocabularies    #Threads  #Words  #Average"
                cr VOC-LINK @
                BEGIN   DUP VLINK>VOC
                        dup voc>vcfa @
                        dup  doClass  =
                        swap do|Class = or 0=
                        IF      dup voc>vcfa .NAME  18 #tab
                                dup voc#threads         dup>r 4 .r
                                        0 to words-cnt
                                        0 to header-cnt
                                    count-voc words-cnt dup   9 .r
                                                   10 * r> / 10 .r.1
                                cr
                        ELSE    DROP
                        THEN    @ DUP 0=
                UNTIL   DROP
                   ." -----------------------------------------"
                cr ." Total System Words: " count-words 11 .r
                cr ;

ROOT DEFINITIONS

: FORTH             FORTH ;
: FORTH-WORDLIST    FORTH-WORDLIST ;
: SET-ORDER         SET-ORDER ;

ONLY FORTH ALSO DEFINITIONS

variable anyvoc

: anyfind       ( a1 -- a2 f1 )         \ find a word in any vocabulary
                anyvoc off
                dup c@ 0=
                if      0 exit
                then
                find ?dup 0=
                if      context @ >r
                        voc-link
                        begin   @ ?dup
                        while   dup vlink>voc ( #threads cells - )
                                dup voc>vcfa @
                                dup   doClass =
                                swap do|Class = or 0=
                                if      context !  \ set voc
                                        over find ?dup
                                        if      2swap 2drop
                                                context @ anyvoc !
                                                r> context !
                                                EXIT      \ *** EXITS HERE ****
                                        then
                                then    drop
                        repeat  0
                        r> context !
                        anyvoc off
                then    ;

: "anyfind      { adr len \ find$ -- a2 f1 }
                MAXSTRING LocalAlloc: find$
                adr len find$ place
                find$ anyfind ;


13 #VOCABULARY HIDDEN
13 #VOCABULARY EDITOR
43 #VOCABULARY ASSEMBLER

