\ LATER.F       A Method of Defering Operations Till Later      by Tom Zimmer

\ LATER a method of defering an operation to a later time
\ numbers, strings and CFA's can be put on the later stack, the FIRST thing
\ you put on the later stack must be a CFA.  The CFA must be that of a word
\ that will remove all its own parameters from the later stack, leaving
\ the next later stack entry pointing at the next later CFA.

cr .( Loading Deferred Execution Stack...)

  10000  pointer later-ptr      \ 10k later buffer
      0    value later-off
3141592 constant later-magic

: later-init    ( -- )          \ clear out the later buffer to NULLs
                later-ptr SizeOf@> later-ptr erase
                0 to later-off ;

initialization-chain chain-add later-init

: ?later-full   ( n1 -- )       \ check to see if there is room for n1 bytes on later stack
                later-off + SizeOf@> later-ptr cell- >
                abort" Later Stack Overflow!" ;

: -later        ( n1 -- )       \ backup later-off by n1
                later-off swap - dup 0max to later-off
                0< abort" Later Stack Underflow" ;

: &later        ( -- a1 )       \ address of the next later stack character
                later-ptr later-off + ;

\ putting things on the later stack

: #later        ( n1 -- )       \ push a number onto the later stack
                cell ?later-full
                &later !
                cell +to later-off ;

: "later        ( a1 n1 -- )
                dup 1+ ?later-full
                >r &later r@ move               \ lay in the string
                r@ 1+ +to later-off             \ skip the text and count bytes
                r> &later 1- c! ;               \ lay in count

: cfa-later     ( cfa -- )      \ push a cfa onto the later stack
                #later                          \ Push CFA onto the later stack
                later-magic #later ;            \ follow CFA with a magic number

\ Getting things off the LATER stack

: later"        ( -- a1 n1 )    \ get a string off the later stack
                1 -later
                &later c@ dup -later
                &later swap ;

: later#       ( -- n1 )        \ get last number from the later stack
                cell -later
                &later @ ;

: do-later      ( -- )
                begin   later-off
                while   later# later-magic <> abort" Top of Later wasn't a CFA"
                        later# execute
                repeat  ;

semicolon-chain chain-add do-later

\ end of LATER stack words


