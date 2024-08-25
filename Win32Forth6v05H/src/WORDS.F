\ $Id: words.f $

cr .( Loading WORDS...)

only forth also definitions

\ display words from one or two patterns

: horizontal-line ( -- )
                getcolrow drop getxy drop - 8 - 0max 8 /mod 0
                ?do     ." --------"
                loop    s" --------" drop swap type
                cr ;

INTERNAL        \ internal definitions start here

variable vocsave

create words-pocket MAXSTRING allot

: .voc-once     ( -- )
                vocsave @ ?dup
                if      cr ." ----------- " .NAME
                        horizontal-line
                        vocsave off
                then    ;

: match?        ( addr len -- f )
                2dup >r >r words-pocket count search nip nip
                pocket  c@
                if      r> r>    pocket count search nip nip and
                else    r> r> 2drop
                then       words-pocket c@ 0= or ;

0 value with-address?

: (words)       { voc \ words-pad$ w#threads -- }
                MAXSTRING LocalAlloc: words-pad$
                voc dup voc#threads to w#threads
                dup voc>vcfa @
                dup   doClass =         \ don't look through classes
                swap do|Class = or 0=   \ don't look through classes
        if      dup here 500 + w#threads cells move     \ copy vocabulary up
                voc>vcfa vocsave !
                begin   here 500 + w#threads largest dup
                while   dup l>name nfa-count 2dup "CLIP" words-pad$ place
                        words-pad$ ?uppercase count match?
                        if      .voc-once with-address?
                                if      2 pick link> dup here u>
                                        if      >bold ." +"
                                        else    space
                                        then    ." 0x" h.8 ."  " >norm
                                        type 30 #tab  space 30 ?cr
                                else    type 20 #tab  space 20 ?cr
                                then
                                1 +to words-cnt
                                start/stop
                                screendelay ms  \ slow down screen output
                        else    2drop
                        then
                        @ swap !
                repeat  2drop
        else    drop
        then    vocsave off ;

\ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
\ ++++++++++++ display windows constants containing adr,len ++++++++++++++
\ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

\ enum constants
\ int EnumWin32Constants(char* addr, int len, CALLBACKPROC* proc)
\ int callback(char* addr, int len, int value)

0 value constant-cnt
0 value constant-tot
0 value count-only?

3 CALLBACK: .WinConstant   ( abs_adr len value -- f )
                count-only? 0=
                if      drop                    \ discard the constant's value
                        swap abs>rel swap       \ convert to relative adr,len
                        2dup match?
                        if      type 20 #tab  space 20 ?cr
                                1 +to constant-cnt
                        else    2drop
                        then    1 +to constant-tot
                        nuf? 0=
                else    3drop
                        1 +to constant-tot
                        1
                then    ;

\ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
\ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

EXTERNAL        \ external definitions start here

: count-constants ( -- n1 )     \ count the constants available to system
                WinConPtr proc>cfa @ dodefer =            \ Leave if 'FindWin32Constant'
                if      0 EXIT                  \ hasn't already been resolved
                then
                TRUE to count-only?
                0 to constant-tot
                &.WinConstant rel>abs
                0 here rel>abs
                CALL EnumWin32Constants DROP
                constant-tot ;

: .words        ( -- )
                count-words dup
                cr app-cnt  dup    6 u,.r - ."  Words in Application dictionary"
                cr                 6 u,.r   ."  Words in System dictionary"
                cr                 6 u,.r   ."  Words total in dictionaries"
                cr count-constants 6 u,.r   ."  Windows Constants available" ;

: CONSTANTS     ( -<optional_name>- )
                cr
                WinConPtr proc>cfa @ dodefer =            \ Leave if 'FindWin32Constant'
                if      ." WINCON.DLL missing"
                        EXIT                    \ hasn't already been resolved
                then
                cr ." ----------- Windows Constants "
                horizontal-line
                0 to constant-cnt
                0 to constant-tot
                FALSE to count-only?
                &.WinConstant rel>abs
                bl word ?uppercase count words-pocket place
                bl word ?uppercase drop
                0 here rel>abs
                CALL EnumWin32Constants DROP
                cr horizontal-line
                cr ." Displayed " constant-cnt . ." of the "
                constant-tot . ." Windows Constants in the system." ;

: with-address  ( -- )
                true to with-address? ;

: WORDS         ( -<optional_name>- )
                0 to words-cnt
                words-pocket off
                bl word ?uppercase c@
                if      pocket count words-pocket place
                        bl word ?uppercase drop
                        voc-link @
                        begin   dup vlink>voc ( #threads cells - )
                                ['] (words) catch
                                if      cr ." Interrupted!"
                                        drop TRUE       \ stop now
                                else    @ dup 0=
                                then
                        until   drop
                else    context @ ['] (words) catch
                        if      drop
                                cr ." Interrupted!"
                        then
                then    0 to with-address?
                base @ >r decimal
                cr horizontal-line
                ." Displayed "       words-cnt . ." of the "
                count-words .  ." words in the system."
                cr ." ** Use: CONSTANTS <substring> to display Windows Constants **"
                r> base ! ;

: on-allwords   { theCFA \ vocBuf -- }
                512 cells LocalAlloc: vocBuf
                voc-link @
                begin   dup vlink>voc
                        dup voc#threads >r
                        dup voc>vcfa @
                        dup   doClass =         \ don't look through classes
                        swap do|Class = or 0=   \ don't look through classes
                        if      vocBuf r@ cells move    \ copy vocabulary up
                                begin   vocBuf r@ largest dup
                                while   dup l>name theCFA execute
                                        @ swap !
                                repeat  2drop
                        else    drop
                        then    r>drop
                        @ dup 0=
                until   drop ;


MODULE          \ end of the module

