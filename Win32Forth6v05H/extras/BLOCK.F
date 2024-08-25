\ BLOCK.F       Tom's Forth virtual block system.       by Tom Zimmer

cr .( Loading BLOCK...)

comment:

  Here is an impementation of a virtual block system.  The constants below,
B/BUF, and #BUFFERS control the record or block size, and the number of
buffers the system uses.  These are defaulted to 1024 byte blocks, and
4 buffers.  A true LRU (least recently used) buffer allocation mechanism
is used, implemented as a bubble up buffer stack.  The least recently used
buffer is always on the bottom of the stack.  As buffers are used or
re-used, they are bubbled immediately up to the top of the stack, destined
to settle to the bottom of the stack if the same record is not accessed
again.

A sample block file BANNER.BLK has been included for your examination.

type the following commands after loading BLOCK.F

        OPEN-BLOCKFILE BANNER.BLK
        1 7 THRU

This will load and run a simple demo.

Type DEMO again to run it again after it has been loaded.

comment;

only forth also definitions

\ needs 486asm.f                        \ load assembler if needed

        1024 constant b/buf             \ length of each block
          64 constant c/l               \ character per line
           8 constant #buffers          \ number of virtual buffers
          -1    value blockhandle       \ current block file handle

variable blk                            \ current block number
variable scr                            \ current screen number

only forth also definitions

INTERNAL                                \ internal definitions

variable cur_buffer#                    \ current buffer # of current block
         cur_buffer# off

#buffers cells constant buflen

variable rec_array b/buf #buffers  * allot      \ an array of blocks
variable rec#s            buflen     allot      \ block # array
variable rec#updt         buflen     allot      \ Update flags
variable rec#use          buflen     allot      \ block bubbleup stack
variable rec#fil          buflen     allot      \ hcb for each block

                                        \ n1 = buffer number
                                        \ a1 = address of buffer
: buf#>bufaddr  ( n1 --- a1 )         \ Calculate address a1 of buffer n1.
        b/buf * rec_array + ;

                                        \ n1 = buffer number
                                        \ a1 = buffer address
: >rec#s        ( n1 --- a1 )           \ return the buffer n1's record addr
        rec#s +cells ;

                                        \ n1 = buffer number
                                        \ a1 = buffer address
: >rec#updt     ( n1 --- a1 )           \ return the buffer n1's update addr
        rec#updt +cells ;

                                        \ n1 = buffer number
                                        \ a1 = buffer address
: >rec#fil      ( n1 --- a1 )           \ return the buffer n1's file addr
        rec#fil +cells ;

: chkfil        ( n1 --- n1 f1 )        \ verify file in bufer n1 is current
        dup dup 8 =
        if      drop false exit
        else    >rec#fil @ blockhandle =
        then    ;

: bubbleup      ( n1 --- )              \ move buffer # n1 to end of list
        >r rec#use #buffers r@ lscan dup 0=
        abort" Buffer# number not in buffer list"
        1- cells >r dup cell+ swap r> move  \ move list down except first
        r> rec#use buflen + cell - ! ;      \ stuff first at end of list.

                                        \ n1 = block we are looking for
                                        \ n2 = buffer #
                                        \ f1 = do we have it?, true if we do
: ?gotrec       ( n1 --- <n2> f1 )      \ Do we have block n1 in memory?
        rec#s #buffers rot lscan nip
        #buffers swap - ( tos is buffer # with matching block #)
        chkfil
        if      true
        else    drop false
        then    ;
                                        \ n1 = block to positon to
: pos_block    ( n1 --- )               \ Set file pointer to block pos n1
        0max b/buf * 0 blockhandle reposition-file drop ;

                                        \ a1 = destination address of read
                                        \ n1 = block number to read
: read_block    ( a1 n1 --- )           \ read block n1 to address a1
        pos_block
        b/buf blockhandle read-file swap b/buf <> or
        abort" Error reading block" ;

                                        \ n1 = buffer number
                                        \ n2 = block number to write
: write_block  ( n1 n2 --- )            \ write block n1 to disk
        pos_block
        dup buf#>bufaddr
        b/buf rot >rec#fil @ write-file
        abort" Error writing block, probably out of disk space." ;

EXTERNAL        \ externally available definitions

                                \ n1 = block #
                                \ a1 = bufadr
: save-buffers  ( -- )          \ save all updated buffers to disk
        #buffers 0                              \ through all the buffers
        do      rec#use @ >r                    \ find a buffer
                r@ bubbleup                     \ bump to highest priority
                r@ cur_buffer# !                \ set current buffer var
                r@ >rec#updt dup @              \ check update flag
                if      off                     \ clear update flag
                        r@ dup >rec#s @         \ get block #
                        write_block             \ write it
                else    drop                    \ discard, already cleared
                then    r>drop
        loop    ;

: buffer        ( n1 -- a1 )            \ Assign least used buffer to rec n1
        dup ?gotrec                     \ check if already present
        if      >r drop                 \ buffer already assigned, save it
        else
                rec#use @ >r                 \ assign LRU buffer
                r@ >rec#updt dup @           \ check update flag
                if      off                  \ clear update flag
                        r@ dup >rec#s @      \ get block #
                        write_block          \ write it
                else    drop                 \ discard, already cleared
                then
                r@ >rec#s   !        \ set block #
                blockhandle r@ >rec#fil !    \ set the file hcb
        then
        r@ bubbleup                     \ bump to highest priority
        r@ cur_buffer# !                \ set current buffer var
        r> buf#>bufaddr ;               \ calc buffer addr

: empty-buffers ( -- )                 \ clean out the virtual buffers
        rec_array b/buf #buffers * erase
        rec#s    buflen -1 fill
        rec#updt buflen erase
        rec#fil  buflen erase
        rec#use  #buffers 0
        do      i over ! cell+     \ initialize the bubbleup stack
        loop
        drop ;

: flush         ( -- )                 \ Write any updated buffers to disk
        save-buffers
        empty-buffers ;

: update        ( -- )                 \ mark the current block as updated
        cur_buffer# @ >rec#updt on ;

                                       \ n1 = block # to get
                                       \ a1 is address of block # n1
: block         ( n1 -- a1 )           \ Get block n1 into memory
        dup ?gotrec
        if      nip dup >r buf#>bufaddr
                r@ cur_buffer# ! r> bubbleup
        else    blockhandle 0< abort" No file open"
                dup buffer dup rot read_block
        then    ;

: list          ( n1 -- )       \ display block n1 on the console
        dup scr !
        block b/buf bounds
        do      cr i c/l type
        c/l +loop    ;

: wipe          ( n1 -- )       \ erase the specified block to blanks
        buffer b/buf blank update ;

: set-blockfile ( fileid -- )
        to blockhandle ;

warning off

: evaluate      ( a1 n1 -- )
        blk off evaluate ;

: save-input    ( -- xxx 8 )
        save-input
        blk @ swap 1+ ;

: restore-input ( xxx 8 -- f1 )
        swap blk ! 1-
        restore-input >r
        blk @ 0>
        if      blk @ block b/buf (source) 2! \ force back to block
        then    r> ;

: refill        ( -- f1 )
        blk @ 0=
        if      refill
        else    >in off
                ?loading on
                blk @ 1+ b/buf block (source) 2!
                true
        then    ;

: \     ( -- )
        blk @ 0=
        if      postpone \
        else    >in @ c/l / 1+ c/l * >in !
        then    ; immediate

warning on

: blkmessage    ( n1 -- )
        blk @ 0>
        if      base @ >r
                cr ." Error: " pocket count type space
                dup -2 =
                if      drop msg @ count type
                else    ." Error # " .
                then
                cr ." Block: " blk @ .
                ." at Line: " >in @ c/l / .
                cr blk @ block >in @ c/l / c/l * + c/l type
                blk off   \ reset BLK cause noone else does!!!
                r> base !
        else    _message
        then    ;

' blkmessage is message

: load          { loadblk \ incntr outcntr -- }
        save-input dup 1+ dup to incntr
                              to outcntr
        begin  >r -1 +to incntr  incntr  0= until
        loadblk blk !
        >in off
        ?loading on
        blk @ block b/buf (source) 2!
        interpret
        begin  r> -1 +to outcntr outcntr 0= until
        restore-input drop ;

: thru          ( n1 n2 -- )
        1+ swap
        ?do     i load
        loop    ;

: close-blockfile ( -- )
        blockhandle -1 <>
        if      flush
                blockhandle      \ Roderick Mcban - February 11th, 2002
                close-file drop
        then    -1 to blockhandle ;

: open-blockfile ( -<filename>- )
        close-blockfile
        /parse-word count r/w open-file abort" Failed to open Block File"
        set-blockfile
        empty-buffers ;

: create-blockfile ( u1 -<filename>- )  \ create a blank file of u1 block long
        close-blockfile
        /parse-word count r/w create-file
        abort" Failed to create Block File"
        set-blockfile
        dup b/buf m* blockhandle resize-file
        abort" Unable to create a file of that size"
        empty-buffers
        0
        do      i wipe
        loop
        flush ;

: #blocks       ( -- n1 )       \ return the number of block in the current file
        blockhandle file-size drop b/buf um/mod nip ;

\ +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
\ initialization of the block system
\ +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

empty-buffers           \ Initialize the virtual memory arrays interpretively

INTERNAL        \ another internal definitions

: virtual-init  ( --- ) \ and during the system startup initialization
        -1 to blockhandle
        empty-buffers ;

initialization-chain chain-add virtual-init

MODULE          \ end of the module

environment definitions

: BLOCK         TRUE ;

: BLOCK-EXT     TRUE ;

only forth also definitions


