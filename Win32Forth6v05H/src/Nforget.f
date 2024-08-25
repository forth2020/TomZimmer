\ NFORGET.F
\ nforget.f beta 2003/02/13 rbs Various changes as documented below
\ -rbs 10/24/02 added trim-WinLibs and trim-WinProcs to a new post-forget-chain

\ -rbs new DO-MARK does not generate redef warnings when marked word is
\ executed, also resets vocabulary to ONLY FORTH ALSO DEFINITIONS.
\ -rbs do-marker changed to reset voc also December 10th, 2002

\ -rbs new DO-MARK does not generate redef warnings when marked word is
\ executed, also resets vocabulary to ONLY FORTH ALSO DEFINITIONS.

\ This is a modified version of FORGET.F for dual dictionaries,
\ by Stephen M. Brault.

\ Received: 05 Feb 96 05:26:38 EST
\     From: "Stephen M. Brault" <72163.3434@compuserve.com>

\ February 5th, 1996 - 22:43 tjz
\ Renamed _TRIM? to SYS-TRIM? which describes its function.
\ Renamed TRIM to FULL-TRIM, to avoid a name conflict with the original
\ simple version of TRIM which is used in other areas of the system, and
\ takes slightly different parameters.

\ Notice that TRIM? is deferred, so its function can be changed to make
\ FULL-TRIM trim things other than the dictionary if desired.

cr .( Loading Forget Wordset...)
cr .( -- BETA NFORGET.F 4.9C -- )

\ FORGET limitations:
\ 1) You cannot directly forget an IN-SYSTEM word since
\ it would require extra work to determine the new DP.
\ You can indirectly forget an IN-SYSTEM word by forgetting
\ an earlier IN-APPLICATION word.

variable fence                          \ cannot forget below this address

: sys-trim?     ( nfa addr -- f )       \ TRUE if forgetting nfa removes addr from dict
        tuck app-here u>
        if                              \ addr is IN-SYSTEM or TRANSIENT
                sys-here                \ so use nfa as trim address
        else                            \ addr is IN-APPLICATION
                name> app-here          \ so use cfa as trim address
        then    between ;               \ ?Won't work across the 2Gb line

defer trim? ' sys-trim? is trim?

: full-trim      ( nfa link -- )         \ Order-independent trim
        begin   ( nfa link^ )
                dup @
        while   ( nfa link^ )
                2dup @ trim?
                if      dup @ @ over !  \ Delete chain member
                else    @               \ Retain chain member
                then
        repeat  2drop ;

: trim-chains   ( nfa -- nfa )          \ trim down the chain linked list
        chain-link
        begin   @ ?dup
        while   2dup -2 cells+ full-trim
        repeat  dup chain-link full-trim ;

: trim-loadfile ( nfa -- nfa )
        dup loadfile full-trim ;

\in-system-ok forget-chain chain-add trim-loadfile

: trim-defer    ( nfa -- nfa )          \ trim deferred word list to nfa
        dup defer-list full-trim
        defer-list
        begin   @ ?dup
        while   2dup cell- @
                trim?                   \ if forgetting IS word
                if      dup cell+ @     \ revert to default
                        over cell- !
                then
        repeat ;

\in-system-ok forget-chain chain-add trim-defer

: trim-WinLibs
    winlib-link
    begin   @ dup
    while   dup here >
            if dup @ dup winlib-link ! winlib-last !
            else drop exit
            then
    repeat drop ;

\in-system-ok post-forget-chain chain-add trim-WinLibs

: trim-WinProcs
    winproc-link
    begin   @ dup
    while   dup here >
            if dup @ dup winproc-link ! winproc-last !
            else drop exit
            then
    repeat drop ;

\in-system-ok post-forget-chain chain-add trim-WinProcs

: vtrim ( nfa voc-thread -- ) \ trim VOC-THREAD back to nfa
        dup voc#threads 0
        do      2dup i cells+ full-trim
        loop    2drop ;

: (forget) ( cfa -- )   \ assumes count follows name
        dup app-here u>                 \ if in system area
        over @ ['] FORTH @ <> and         \ but not a vocabulary
        abort" in system or transient dictionary"
        dup cell+ ResetSrcInfo
        >name
        dup fence @ 1- trim? abort" in protected dictionary"
        voc-link 2dup full-trim
        begin   @ ?dup
        while   2dup vlink>voc vtrim
        repeat
        forget-chain do-chain   \ do forget-chain before trimming it
        trim-chains
        context #vocs cells+ context
        do      dup i @ trim?
                if      [ ' forth vcfa>voc ] literal i !
                then
                cell +loop
        dup N>FFA sdp !          \ update SYS-HERE
        name> dup dp @ u<        \ if new address is in application space
        IF      dp !             \ then update HERE
          post-forget-chain do-chain \ post-forget chain
        ELSE    drop
        THEN
        voc-also ;               \ reset look-aside table if present


: forget ( -<name>- )
        bl word count
        current @ search-wordlist 0= ?missing ( cfa ) (forget) ;

: do-mark ( -- )        \ mark must redefine the name that was forgotten
\ -rbs modified to reset vocabs and not generate redef warnings when marked
\ word is executed.
        does>   { does-adr \ mark-name$ -- }
                MAXSTRING LocalAlloc: mark-name$
                s" mark " mark-name$ place
                does-adr body> dup >name nfa-count mark-name$ +place
                (forget)
\ -rbs
\                forth definitions
s" ONLY FORTH ALSO DEFINITIONS" evaluate
WARNING @ WARNING OFF
                mark-name$ count evaluate
WARNING !
                ;

: mark  ( -<name>- )
        >application create save-source do-mark application> ;

\ January 27th, 1999 - 11:27 tjz
\ modified MARKER to preserve and restore the search order

: do-marker ( -- )
        does> dup>r @ (forget)
        r> cell+ dup @ current !                        \ restore current
        cell+ context #vocs cells move ;                \ restore context search list

: marker ( -<name>- ) ( ANS)
        >application
        create save-source here body> ,
        current @ ,                                     \ save current
        context here #vocs cells allot #vocs cells move \ save context search list
        do-marker
        application> ;
\        s" only forth also definitions" evaluate ;

here fence !

\  POSSIBLY                        ( "name" -- )
\     Execute _name_ if it exists; otherwise, do nothing. Useful
\     implementation factor of `ANEW`.

: POSSIBLY  ( "name" -- )  BL WORD FIND  ?dup AND IF  EXECUTE  THEN ;

\ : anew          ( -<name>- )    \ define a new marker
\                >in @ defined
\                if      execute  \ July 25th, 1997 tjz removed NIP
\                else    drop
\                then    >in ! marker ;

\  ANEW                            ( "name" -- )( Run: -- )
\     Compiler directive used in the form: `ANEW _name_`. If the word
\     _name_ already exists, it and all subsequent words are
\     forgotten from the current dictionary, then a `MARKER` word
\     _name_ is created. This is usually placed at the start of a
\     file. When the code is reloaded, any prior version is
\     automatically pruned from the dictionary.
\
\     Executing _name_ will also cause it to be forgotten, since
\     it is a `MARKER` word.
\
\     Useful implementation factor of `EMPTY`.

: ANEW  ( "name" -- )( Run: -- )  >IN @ POSSIBLY  >IN ! MARKER ;



