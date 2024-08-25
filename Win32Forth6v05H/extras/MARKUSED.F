\ MARKUSED.F  Track the use of Windows Constants and Procedures  by Tom Zimmer

cr .( Loading MARK USED, Procedure Tracking...)

TRUE constant "MARK-USED"

TRUE value load-usage?
   0 value first-usage#                 \ stable usage field counter
   0 value was-used?                    \ was this word used?
   0 value new-usage?                   \ are we defining a new usage database, or
                                        \ are we using an existing database?

6000 constant max-usage                 \ up to this number of functions can be tracked
1009 constant usage-version-constant    \ version check constant

max-usage cells pointer usage-ptr       \ define an array of counters

create usage-buf MAXSTRING allot         \ a static filename buffer
       usage-buf off

: use-field     ( n1 -<name>- n2 )              \ define usage database variables
                create  first-usage# cells ,
                        1 +to first-usage#      \ bump to next usage field
                does> @ usage-ptr + ;

use-field usage-version
use-field partial-usage
use-field usage-count
use-field use-assembler
use-field use-float

: usage-count+    ( -- n1 )               \ get the next function number counter
                usage-count @
                1 usage-count +! ;

: .usage-database ( -- )
                cr ." Functions Used/Total: "
                0
                usage-ptr usage-count @ cells bounds
                ?do     i @ if 1+ then
          cell +loop    1 .r ." /" usage-count @ . ;

: new-usage     ( -- )
                usage-ptr max-usage cells erase         \ clear the array
                usage-version-constant usage-version !  \ init with version #
                TRUE                   partial-usage !  \ mark as a partial usage table
                TRUE                   use-assembler !  \ mark to load the assembler
                TRUE                   use-float     !  \ mark to load floating point
                first-usage#           usage-count   !  \ usage count starts at 1
                TRUE to new-usage?
\               cr ." New Usage Database being built"
                ;

: init-usage    ( -- )
                load-usage? 0= ?exit
                usage-buf c@ 0=
                if      s" USAGE.DB"   usage-buf place
                then
\                cr ." Opening Usage: " usage-buf count type
                usage-buf count 0 fopen-file
                if      drop
                        usage-buf off   \ clear so it will get initialized later
                        new-usage       \ create a new usage database
                else    >r
                        usage-ptr max-usage cells r@ fread-file drop
                        r> fclose-file drop
                        usage-version @ usage-version-constant <>
                        if      new-usage
                        else    partial-usage @ to new-usage?
\                               cr ." Using existing Usage Database"
\                               .usage-database
                                new-usage? 0=
                                if      first-usage# usage-count !
                                then
                        then
                then    ;

initialization-chain chain-add init-usage

init-usage                      \ init and start accumulating statistics
first-usage# usage-count !      \ always reset use count when loading Win32Forth
usage-buf off                   \ clear so it will get initialized later

: _save-usage   ( -- )
                usage-buf c@ 0=
                if      &prognam count "path-only" usage-buf   place
                        usage-buf c@
                        if                         usage-buf ?+\
                        then
                        s" USAGE.DB"               usage-buf  +place
                                                   usage-buf  +NULL
                then
                cr ." To: " usage-buf count type
                usage-ptr max-usage cells usage-buf fsave-file ;

: partial-save-usage ( -- )
                .usage-database
                cr ." Saving PARTIAL Usage Database"
                _save-usage ;

: final-save-usage   ( -- )
                .usage-database
                cr ." Saving FINAL Usage Database"
                new-usage? to load-usage?       \ don't load the usage file as part of the
                                                \ application initialization
                FALSE partial-usage !
                _save-usage ;

: new-used      ( n1 -<name>- )         \ n1=count of arguments
                load-usage? 0=
                if      drop
                        EXIT
                then
                new-usage?              \ are we defining a new usage database
                if      drop            \ discard the number of parameters
                        align           \ align dictionary
                        usage-count+ ,  \ compile the usage index
                else                    \ or are we checking an existing database
                        usage-ptr usage-count+ cells+ @ 0=      \ if this word wasn't used
                        dup to was-used?                        \ flag for use in WINCONSTANT
                        if      0                               \ then
                                ?do     drop                    \ discard its parameters
                                loop    bl word drop            \ and the word name following
                                r>drop                          \ discard one level return stack
                        else    drop
                        then
                then    ;

: mark-used     ( body-address -- )
                load-usage? 0=
                if      drop
                        EXIT
                then
                new-usage?
                if      body> >name nfa-count drop      \ get addr of first char
                        0xFFFFFFFC and                  \ backup to cell boundry
                        cell- @                         \ get the usage index
                        1 usage-ptr rot cells+ +!       \ bump the counter
                else    drop
                then    ;

: load-assembler? ( -- f1 )
                use-assembler @ ;

: load-float?   ( -- f1 )
                use-float @ ;

: no-assembler  ( -- )
                FALSE use-assembler ! ;

: no-float      ( -- )
                FALSE use-float ! ;



