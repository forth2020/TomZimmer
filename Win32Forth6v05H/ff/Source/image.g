((
Image management and header manipulation

TOKENIZING sets the tokenizer wordlist at the top of the search order but
leaves the rest of the order alone.  BUILDING does a similar thing with
the builder wordlist.  HOST restores the host search order.
))

new-chain init-mem                    
new-chain free-mem
new-chain init-tokens

\ ----------  Vocabulary manipulation words ------------------------------

only forth also definitions
false to sys-warning?

vocabulary builder
vocabulary tokenizer
vocabulary tester
\ defer binterpret  ' forthinterpret is binterpret \ defaults, will be changed
\ defer tinterpret  ' forthinterpret is tinterpret

: host          ( -- )  false to hilitecontext? interpret=forth ;
: _building     ( -- )  true to hilitecontext?  interpret=build ;
: _tokenizing   ( -- )  true to hilitecontext?  interpret=token ;
: _testing      ( -- )  true to hilitecontext?  interpret=target ;
: _forthing     ( -- )  true to hilitecontext?  interpret=totest ;

: building      ( -- )  also builder _building ;
: tokenizing    ( -- )  also tokenizer _tokenizing ;
: testing       ( -- )
                communicable?
                if  callable?  if also tester _testing then
                then    ;

defer forceCORE

: forthing      ( -- )
                communicable?
        if      callable?
                if      also tokenizer _forthing
                        forceCORE
                then
        then    ;

: bi building ;
: ti tokenizing ;
: fi forthing ;
: te testing ;

: underprevious ( -- )  context @  previous previous    also  context ! ;
: underalso     ( -- )  context @  previous also        also  context ! ;
: underdefs     ( -- )  context @  previous definitions also  context ! ;
: underwords    ( -- )  context @  previous words       also  context ! ;

: underfind     ( a -- cfa f )
\ like FIND, but doesn't include the top of the search order
                context @ >r
                previous find
                also  r> context ! ;

: (previous)    ( -- )  previous ;
: (words)       ( -- )  words ;
: (definitions) ( -- )  definitions ;
: (only)        ( -- )  only ;
: (also)        ( -- )  also ;
: (vocabulary)  ( <name> -- )  vocabulary ;


: fixend        ( n -- n' )     \ change byte order if necessary
                bigendian?
        if      0 >r cellsize swap      ( cnt n | n' )
                begin   over
                while   0x100 /mod swap
                        r> 8 lshift or >r
                        >r 1- r>
                repeat
                2drop r>
        then    ;

\ --------  Image manipulation words

  variable ihere          \ offset into image buffer
  variable vhere          \ offset into data space space
   0 value RAM?           \ T if allocating RAM
imagesize value heremax   \ maximum allowable value of ihere
 datasize value datamax   \ maximum allowable value of vhere
32 constant catstringsize \ length of catalog help string
  variable headers        \ T if headers are included
  variable noslack        \ T if warnings abort

: static        ( -- )  false to dynamic? ;
: dynamic       ( -- )  true  to dynamic? ;
: rom           ( -- )  false to RAM? ;
: ram           ( -- )  true  to RAM? ;

: init-image    ( -- )
                imagesize malloc to codebuf    \ ROM image
                datasize  malloc to databuf    \ data list
                imagesize 8 / malloc to progdatabuf ;
: free-image    ( -- )
                progdatabuf release
                databuf release
                codebuf release ;
: clear-image   ( -- )
                codebuf imagesize -1 fill  \ blank EPROM
                databuf datasize erase
                progdatabuf imagesize 8 / erase
                0       ihere !
                0       vhere !
                headers on
                false to RAM?
                false to dynamic?
                0 to codeorigin  ;     \ default origin = 0000

init-mem        chain-add init-image   \ allocate memory for ROM image
free-mem        chain-add free-image
init-tokens     chain-add clear-image  \ clear the ROM image

: new-image     ( -- )
\ clear all target parameters
                init-tokens do-chain ;

\ ---------------------- ROM Pinout Scrambling -----------------------

\ Scrambling is for "miswired" ROMs. The address and data lines may be mixed
\ around to simplify PCB layout and make reverse engineering tougher.
\ Use SCRAMBLE, SCRAMBLE16 and SCRAMBLE32 to scramble data for byte-wide,
\ 16-bit and 32-bit devices. The image will be scrambled, then you can
\ use one of the file-save words.

24 value scram-asize            \ number of address bits to scramble
32 value scram-dsize            \ number of data bits to scramble

create scrambleaddr  scram-asize chars allot
create scrambledata  scram-dsize chars allot
variable scramtemp

defer _scramble@ ( a -- n )     \ fetch ROM image (or whatever)
defer _scramble! ( n a -- )     \ store to temporary scrambled image

: scram-init    ( -- )
\ initialize scramble tables for no scrambling
                scram-asize 0
                do      i i chars scrambleaddr + c!
                loop
                scram-dsize 0
                do      i i chars scrambledata + c!
                loop
                ;
: scramblebits  ( n width table -- m )
\ scramble n using scrambling table
                scramtemp off
                swap 0                  ( n table . . )
                do      over 1 and
                        over c@ lshift  ( n table bit bit# )
                        scramtemp @ or scramtemp !
                        char+ >r u2/ r>
                loop    2drop
                scramtemp @
                ;
: unscramblebits ( n width table -- m )
\ unscramble n using scrambling table
                scramtemp off
                swap 0                  ( n table . . )
                do      2dup c@         ( n t n bit# )
                        1 swap lshift and 0<>
                        1 i lshift and
                        scramtemp @ or scramtemp !
                        char+
                loop    2drop
                scramtemp @
                ;
: (scramble)    ( addr -- )
\ fetch value using a scrambled address, and scramble the value
                dup>r
                scram-asize scrambleaddr unscramblebits      \ cook address
                _scramble@
                scram-dsize scrambledata scramblebits        \ cook data
                r> _scramble!
                ;
: scram-a       ( CPU ROM -- )  \ re-map an address bit
                swap 0max scram-asize 1- min chars scrambleaddr + c!
                ;
: scram-d       ( CPU ROM -- )  \ re-map a data bit
                swap 0max scram-dsize 1- min chars scrambledata + c!
                ;
: ._scram3      ( char n -- )
                0 <# #s rot hold bl hold #> + 3 - 3 type
                ;
: ._scram       ( char width table -- )
\ display a scramble table
                swap 0                  ( char table . . )
                do      scramtemp off
                        i 3 and 0= if cr then
                        over i ._scram3
                        dup i chars + c@
                        dup i =         ( table idx . | ch )
                        if      ."  --> "
                        else    ."  **> "
                        then
                        >r over r> ._scram3
                        2 spaces
                loop    2drop
                ;
: .scram        ( -- )
\ display scramble tables
                cr ." CPU pins --> ROM pins"
                'A' scram-asize scrambleaddr ._scram
                'D' scram-dsize scrambledata ._scram
                ;

1 value scramidx
0 value scrampad
: scram8@       codebuf + c@ ;
: scram16@      codebuf + w@  byte-swap ;
: scram32@      codebuf + @   byte-rev ;
: scram8!       scrampad + c! ;
: scram16!      >r byte-swap r> scrampad + w! ;
: scram32!      >r byte-rev  r> scrampad + ! ;

: iabits        ( -- n )        \ # of address bits needed to hold image
                0 ihere @
                begin ?dup while u2/ >r 1+ r> repeat
                ;

: rabits        ( iabits -- n )
\ calculate # of ROM bits needed to hold the scattered data
                scram-asize                             ( ibits cnt )
                begin   ?dup
                while   1- dup chars scrambleaddr + c@
                        pluck < if nip exit then
                repeat  ;

: maxrombytes   ( -- romsize )
                2 iabits rabits lshift
                ;

: scramblex     ( '@ '! index -- )
\ generic scrambler: image --> scrampad --> image
                to scramidx  is _scramble!  is _scramble@
                imagesize malloc to scrampad    \ use a temporary buffer
                scrampad imagesize -1 fill      \ unused ROM bytes are blank
                0 maxrombytes dup>r scramidx / 0
                cr ." Scrambling " over . scramidx (.) type ." -byte words..."
                ?do     dup (scramble) scramidx +
                loop    drop
                scrampad codebuf r@ move        \ copy back to code image
                r> ihere !
                scrampad release ;

: scramble      ( -- )  ['] scram8@  ['] scram8!  1  scramblex ;
: scramble16    ( -- )  ['] scram16@ ['] scram16! 2  scramblex ;
: scramble32    ( -- )  ['] scram32@ ['] scram32! 4  scramblex ;

scram-init


: ihere@        ( -- a )        ihere @ codeorigin + ;
\ returns the target address that ihere points to

: here@         ( -- a )        ihere@ charsize / ;

: ihere!        ( a -- )        codeorigin - ihere ! ;

: vhere@        ( -- a )        vhere @ dataorigin + ;

: vhere!        ( a -- )        dataorigin - vhere ! ;

: 'image        ( a -- a' )     codeorigin - codebuf + ;
\ convert target address to Forth address

: '-image       ( a -- a' )     codeorigin + codebuf - ;
\ convert Forth address to target address


\ ---------------------- File Output ---------------------------------

: ?foerror      abort" Error opening file" ;
: ?frerror      abort" Error reading file" ;
: ?fwerror      abort" Error writing file" ;
: ?fcerror      abort" Error closing file" ;
: ?fmerror      abort" Error creating file" ;
: ?badformat    abort" Invalid file format" ;
: ?rerror       abort" EEPROM read error" ;
: ?werror       abort" EEPROM write error" ;

: hex.          ( n -- )
                dup cheaplog 1+ 2/ 2/   \ force fit
                addrnibbles max (h.) type ;

0 value outfilename
0 value outfile

: openoutput    ( a -- )
                to outfilename
                outfilename count r/w create-file
                ?fmerror to outfile ;

: openoutput"   ( a -- )
                openoutput codeorigin dup hex.
                ." .."  ihere @ + 1- hex. ;

: closeoutput   ( -- )
                outfile close-file ?fcerror ;

variable checksum

: fbyte         ( c -- )
                dup 2 (h.) pad +place
                checksum +! ;

create $fcr     2 c, 13 c, 10 c,

: fendline      ( -- )
\ append eol to PAD and write to output file
                $fcr count pad +place
                pad count outfile write-file
                ?fwerror ;

:noname         ( a n -- )
\ append a line to the output file
                pad place fendline ;  is orgsave

: hsaveline     ( a n -- )
\ Output Intel hex record to file
                checksum off
                >r word-split swap r>           ( ahi alo n )
                s" :" pad place                 \ :
                pluck 0<> 2 and over + fbyte    \ length, add 2 if ext addr
                over byte-split fbyte fbyte     \ address       ( ahi alo n )
                rot dup
                if       2 fbyte
                        12 lshift byte-split    \ extended address record
                        fbyte fbyte
                else    drop 0 fbyte            \ record type 00
                then
                codebuf codeorigin - under+
                begin   ?dup                    ( a n )
                while   1- >r count fbyte r>
                repeat  drop
                checksum @ negate fbyte         \ checksum
                fendline ;

variable lastStype

: ssaddress     ( a n -- a n )
\ lay down beginning and address of an S record
                >r
                checksum off
                dup word-split byte-split       ( a a01 a2 a3 )
                over 0<> 2 and
                over 0<> 3 and or  1 max        ( a a01 a2 a3 recordtype )
                s" S" pad place                 \ S
                dup lastStype !
                dup (.) pad +place              \ recordtype
                r@  over + 2 + fbyte            \ length = addr + data + chksum
                case    3 of fbyte fbyte endof  \ address
                        2 of  drop fbyte endof
                        1 of  2drop      endof
                endcase
                byte-split fbyte fbyte
                r> ;

: ssaveline     ( a n -- )
\ Output Mot s-record to file
                ssaddress
                codebuf codeorigin - under+
                begin   ?dup                    ( a n )
                while   1- >r count fbyte r>
                repeat  drop
                checksum @ invert fbyte         \ checksum
                fendline ;

: "saving"      ( a -- a )
                cr ." Saving image "  dup $type ;

: $bsave        ( a -- )
\ Save image as a binary file
                "saving" ."  as Binary " openoutput"
                codebuf ihere @ outfile write-file ?fwerror
                closeoutput ;

: $hsave        ( a -- )
\ Save image as intel HEX file
                "saving" ."  as Intel Hex " openoutput"
                codeorigin  ihere @        ( a n )
                begin   dup 0>
                while   2dup 16 min hsaveline           \ save 16-byte line
                        >r 16 + r> 16 -
                repeat  2drop
                s" :00000001FF" pad place fendline      \ end-of-file marker
                closeoutput ;

: $ssave        ( a -- )
\ Save image as Motorola S-record file
                "saving" ."  as Motorola S-record " openoutput"
                codeorigin  ihere @        ( a n )
                begin   dup 0>
                while   2dup 16 min ssaveline           \ save 16-byte line
                        >r 16 + r> 16 -
                repeat  2drop
                codeorigin 0 ssaddress 2drop            \ set start address
                10 lastStype @ - '0' + pad 2 chars + c! \ make this a termination record
                checksum @ invert fbyte                 \ checksum
                fendline                                \ end-of-file marker
                closeoutput ;


: bsave         ( <filename> -- )   bl word $bsave ;
: hsave         ( <filename> -- )   bl word $hsave ;
: ssave         ( <filename> -- )   bl word $ssave ;

msgwindow logpopup


: $comlog        ( $filename -- ) { \ tally time changed -- }
\ Log incoming bytes to a file until a key is pressed
                0 to changed
                communicable?
        if      openoutput
                0 to tally
                s" Press any key to end" MessageText: logpopup
                true ontop: logpopup
                start: logpopup
                ms@ to time
        begin   skey?
                if      Comkey-val 1 outfile write-file ?fwerror
                        0 to Comkey-flg?  1 +to tally
                        true to changed
                then
                ms@ time - 125 >        \ limit refresh to 8 Hz
                changed and             \ we have something to display
                if      s" Bytes: " pad place
                        tally   (.) pad +place
                        pad count MessageText: logpopup
                        Refresh: logpopup
                        ms@ to time
                        false to changed
                then
                key?
        until   key drop closeoutput
                close: logpopup
        else    drop
        then    ;

: comlog        ( <filename> -- )  bl word $comlog ;

0 value infilename
0 value infile

: openinput     ( a -- )
                to infilename
                infilename count r/w open-file ?foerror
                to infile ;

: closeinput    ( -- )
                infile close-file ?fcerror ;

: $,bload       ( a -- )            \ append binary file to image
                openinput
                ihere @ codeorigin + dup codebuf +      ( low a )
                imagesize rot - tuck                    ( maxlen a maxlen )
                infile read-file ?frerror               ( maxlen len )
                closeinput
                tuck - negate dup 0>                    ( len overage )
                if      ." Binary file was too big by " . ." bytes."
                        drop true abort"  "
                else    drop ihere +!
                then    ;

: ,bload        ( <filename> -- )   bl word $,bload ;


: nextnibl      ( a -- a+1 n )  count 16 digit 0= ?badformat ;
: nextbyte      ( a -- a+2 n )  nextnibl 4 lshift >r nextnibl r> + ;
: nextword      ( a -- a+4 n )  nextbyte 8 lshift >r nextbyte r> + ;

: $hload        ( a -- )
\ Load intel .HEX file
                openinput
        begin   pad maxstring infile read-line ?frerror
                nip                             \ don't need length
        while   pad count ':' <> ?badformat
                nextbyte swap                   ( #bytes a )
                nextword swap                   ( #bytes org a )
                >r 2dup + ihere@ max ihere! r>  \ expand image if necessary
                nextbyte 0=                     \ accept only data type 0
                if      -rot swap bounds        ( a . . )
                        ?do     nextbyte i 'image c!  \ load data
                                1 under+
                        loop    drop            \ ignore checksum
                else    3drop
                then
        repeat  closeinput ;

: hload         ( filename ? -- )       bl word $hload  ;

(( ============================================================================
        XCALL support
        Used to call external modules

        Builder words:
                XCALL   ( -- )
                XCALL1  ( n1 -- n2 )
                XCALL2  ( n1 n2 -- n3 n4 )

        Target-specific defered words lay down code to save the Forth registers
        and shuffle parameters.
============================================================================ ))

: $scan         ( a n a-addr len -- a' n' f )
\ scan a string for a substring, T if found
\ the first letter of the substring can't match any letter before the substring.
\ result string contains the string all after the substring
                >r dup r>  2>r c@ scan          ( a1' n1' | a2 n2 )
                2dup r@ min 2r> dup>r compare   ( a1' n1' f | n2 )
                r> swap >r /string
                r> 0= ;

: $numh         ( a n -- number a' n' f )
\ parses the next substring as a number ending in 'H'
                base{ hex
                bl skip 0 0 2swap >number ( ## a' n' )
                over c@ 'H' = >r
                1 /string
                rot drop r>
                }base ;

: $no?          ( a n -- a n' )
\ remove the part of the string beginning with '?'
                2dup '?' scan nip -     \ abc?def --> abc
                over c@ '_' =
        if      1 /string               \ _abc --> abc
        then    ;

vocabulary xlabels

: .procs        ( -- )
                also xlabels words previous ;

: xlabel        ( name -- a )
                get-order only xlabels
                bl word find 2>r
                set-order    2r> 0= abort" Unknown external label"
                execute ;

: xcall_label   ( value a-addr len -- )
\ define a constant in the xlabels vocabulary
                current @ >r
                also xlabels definitions
                (source) 2@ 2>r  >in @ >r       \ save source
                (source) 2!      >in off        \ substring is the new source
                constant                        \ define a constant in the list
                r> >in !  2r> (source) 2!       \ restore source
                previous
                r> current !
                ;

: bl51-line     ( a n -- )
                s" CODE " $scan         \ expect CODE
                if      bl skip $numh   \ expect number
                        if      s" ?PR?" $scan
                                if      $no? xcall_label
                                else    3drop
                                then
                        else    3drop
                        then
                else    2drop           \ not a code line
                then
                ;

: $xload-bl51   ( a -- )
\ Load linker listing file created by Keil 8051 linker,
\ build wordlist of labels
                openinput
        begin   pad maxstring infile read-line ?frerror
        while   pad swap bl51-line
        repeat  drop
                closeinput ;

: xload-bl51    ( filename -- )       bl word $xload-bl51  ;
\ load labels created by BL51 linker


: maxcbound     ( -- a )        heremax imagesize min ;
: maxdbound     ( -- a )        datamax datasize min ;

: ?codebounds   ( a -- )
\ make sure physical address a is within the bounds of code image memory
                dup  codeorigin -
                0 maxcbound between
                if      drop
                else    cr ." Code Address " h. ." is out of code image bounds"
                        cr ." Valid range is " codeorigin h.
                           ." .. "  codeorigin maxcbound + 1- h.
                        true  abort" "
                then    ;

: ?databounds   ( a -- )
\ make sure physical address a is within the bounds of data image memory
                dup  dataorigin -
                0 maxdbound between
                if      drop
                else    cr ." Data Address " h. ." is out of data image bounds"
                        cr ." Valid range is " dataorigin h.
                           ." .. "  dataorigin maxdbound + 1- h.
                        true  abort" "
                then    ;

: ?underilimit  ( a -- )
                dup heremax >
                if      h. true abort" ROM memory limit exceeded"
                else    drop
                then    ;

: ?undervlimit  ( a -- )
                dup datamax >
                if      h. true abort" Data memory limit exceeded"
                else    drop
                then    ;

: vallot        ( n -- )        vhere @ + dup vhere ! ?undervlimit ;
: iallot        ( n -- )        ihere @ + dup ihere ! ?underilimit ;

create dm_mask 1 c, 2 c, 4 c, 8 c, 16 c, 32 c, 64 c, 128 c,

: dmark         ( -- )
\ mark the current byte as a data byte (in code space) for the disassembler
                RAM? 0=
        if      ihere @ 8 /mod progdatabuf + >r
                dm_mask + c@ r@ c@ or r> c!
        then    ;

: dmark?        ( a -- f)
\ is this byte a data byte in code space?
                8 /mod progdatabuf + >r
                dm_mask + c@  r> c@ and ;

: ic,           ( c -- )
\ compile a byte to the code or data image
                RAM?
        if      vhere @ dup dataorigin +
                ?databounds             \ make sure it fits
                databuf + c!            \ place it at the end
                1 vallot
        else    ihere @ dup codeorigin +
                dup ?codebounds         \ make sure it fits
                syncstart umin to syncstart \ sync start address
                codebuf + c!            \ place it at the end
                1 iallot
        then    ;

: iw,           ( n -- )
\ compile 16-bit value to the image, hi byte first
                byte-split ic, ic, ;

: il,           ( n -- )
\ compile 32-bit value to the image, hi byte first
                word-split iw, iw, ;

: i,            ( n -- )
\ compile a number to the image using the appropriate byte order
                cellsize 0= abort" CELLBITS must be nonzero"
                cellsize                ( n #bytes )
                begin   ?dup            \ place bytes on return stack,
                while   1-  swap        \ low byte first, hi byte on top
                        256 /mod        ( #bytes' byte n' )
                        -rot >r         ( n' #bytes' )
                repeat  drop
                cellsize                ( #bytes )
                bigendian?
        if      begin   ?dup            \ hi byte first
                while   1- r> dmark ic, \ send bytes to the image
                repeat
        else    begin   ?dup            \ lo byte first
                while   1- r> swap      \ send bytes to the data stack
                repeat
                cellsize                ( nhi ... nlo #bytes )
                begin   ?dup
                while   1- swap dmark ic, \ send bytes to the image
                repeat
        then    ;

: ich,          ( char -- )             \ {a}
\ charbits/8 bytes, big endian with respect to the image buffer
                charbits 3 rshift
                begin   ?dup
                while   dmark over ic,
                        1- swap 8 rshift swap
                repeat  drop ;


: ic@           ( a -- c )      'image c@ ;
\ fetch byte from a target address in the code image buffer

: ic!           ( c a -- )      'image c! ;
\ store byte to a target address in the code image buffer

: iw@           ( a -- w )
                'image count swap c@    ( 1st 2nd )
                bigendian? if swap then
                byte-join ;

: iw!           ( w a -- )
                'image >r byte-split    ( lo hi | a )
                bigendian? 0= if swap then
                r> swap lay c! ;

: (i@)          ( a -- n )
\ fetch multibyte value from image
                0 swap  cellsize bounds
                ?do     8 lshift i c@ or
                loop    fixend ;

: i@            ( a -- c )
\ fetch word from a target address in the code image buffer
                'image (i@) ;

: (i!)          ( n a -- )
\ store multibyte value to image
                fixend
                cellsize bounds
                ?do     dup i c!  8 rshift
                loop    drop ;

: i!            ( c a -- )      'image (i!) ;
\ store word to a target address in the code image buffer

: HEX[          ( <code...> -- )
\ compile a string of hex code between HEX[ and ].
                base{
        begin   bl word count number? nip ( n . )
        while   ic,
        repeat  drop
                }base ;

 1 value calignment      \ code space alignment, modified by different builders {bld}
 1 value dalignment      \ data space alignment

: idump         ( a len -- )    >r 'image r> dump ;

: ialign        ( -- )
\ append zeros until ihere is aligned on an alignment boundary
                begin   ihere@ calignment mod
                while   ihere incr
                repeat  ;

: valign        ( -- )
\ append zeros until vhere is aligned on an alignment boundary
                begin   vhere @ dalignment mod
                while   vhere incr
                repeat  ;

: XALIGN        ( -- )
                ram?
                if      valign
                else    ialign
                then    ;

: XALIGNED      ( a1 -- a2 )
                dup negate ram?
                if      dalignment
                else    calignment
                then    mod + ;

: ialigned      ( a1 -- a2 )
                dup negate calignment mod + ;



: token,        ( xt -- )
\ append token to image using the shortest representation:
\ tokenbreak .. 255        = 1-byte token
\ 256 .. tokenbreak*256-1  = 2-byte token
\ otherwise                = 3-byte token
                dup  tokenbreak 0x0100  within
                if      ic, exit                then    \ 1
                dup  256 tokenbreak 8 lshift within
                if      byte-split ic, ic, exit then    \ 2
                dup  maxtokens <
                if      0 ic, byte-split ic, ic,        \ 3
                else    . true abort" invalid token#"
                then    ;

: (lift-token)  ( a -- a' xt )
\ lift token number from a data stream
                count ?dup
                if      dup tokenbreak <                ( a c1 . )
                        if    >r count r> byte-join     ( a c1c0 )
                        then
                else    count >r count r> byte-join     \ 3
                then    ;

: lift-token    ( a -- a' xt )
\ lift token from the image
                'image (lift-token)
                >r '-image r> ;

: ROM-BOUNDS    ( lo hi -- )
\ set ROM boundaries
                over b.codeorigin !
                swap to codeorigin
                codeorigin - 0max to heremax ;

: CODE-BOUNDS   ( lo hi -- )
\ set limits of user code RAM.
\ On the target, binding table grows downward, executable code grows upward.
                b.bindorigin ! b.codeorigin ! ;

: DATA-BOUNDS   ( lo hi -- )
\ set limits of user data RAM.
                swap to dataorigin
                dataorigin - 0max to datamax ;

: ROM-BOUNDS?   ( -- lo hi )    codeorigin heremax bounds swap ;
: CODE-BOUNDS?  ( -- lo hi )    b.codeorigin @ b.bindorigin @ ;
: DATA-BOUNDS?  ( -- lo hi )    dataorigin datamax bounds swap ;
: TableLowEnd   ( -- abegin )   code-bounds? + 2/
                                b.bindorigin @ jsrsize 0x1000 * + umax ;

: min..max      ( lo hi -- )    swap hex. ." .." hex. ;

: .membounds    ( -- )
\ display current memory boundaries
                cr ." ROM:       "  ROM-BOUNDS? min..max
                cr ." CODE RAM:  " CODE-BOUNDS? min..max
                cr ." DATA RAM:  " DATA-BOUNDS? min..max
                ;

: (.stat)       ( a n -- )      bounds 6 .r ."  .. " . ;

: .status       ( -- )    \ Display image info
                cr ." ROM code generated: " codeorigin ihere @ (.stat)
                cr ." Data RAM allocated: " dataorigin vhere @ (.stat)
                ;

\ -----------  HEADER MANAGEMENT  ---------------------------------------

\ Maintain a list of active files and tokens and a list of token-
\ containing vocabularies.

\ The filename list provides a means to hyperlink to source code.

\ The XT list is used to find a given token's header information.

\ The vocabulary list is needed to save and restore the tokens (minus
\ source code).

  80 value filenamesize
  64 value maxfilenames
   0 value filenames            \ list of source filenames
   0 value filenamecount
   0 value xt-table             \ xt => NFA lookup table
   0 value cfa-table            \ cfa => NFA lookup table
   0 value watch-table          \ watch => reader lookup table
   0 value #cfas                \ size of the CFA table
   0 value #watch               \ size of the watch table
   0 value cputype
  64 value vocabularies
   0 value vocabularylist
   0 value vocabularycount
   0 value ForthLink            \ -> Tokenized Forth Wordlist
   0 value currentfile#

create 'token#s  8 cells allot

'token#s value 'token#

: token#        ( -- n )        'token# @ ;
: to-token#     ( n -- )        'token# ! ;

: tokenset      ( n -- )        'token#s swap th to 'token# ;
: main-tokens   ( -- )          0 tokenset ;
: low-tokens    ( -- )          1 tokenset ;
: temp-tokens   ( -- )          2 tokenset ;


: filenumber    ( a -- n )
\ looks for a filename in the active list, appends it to the list if
\ not found and returns the file number.  Returns -1 if array is full.
\ Filenames are counted strings.
                dup count upper
                filenames 0                     ( akey asrch n )
        begin   dup filenamecount <
        while   >r 2dup ccompare 0= r> swap
                if      nip nip exit            \ found at n
                then    1+
                swap filenamesize + swap
        repeat                                  \ not found: create
                dup maxfilenames >=
                if      3drop -1 exit           \ won't fit
                then
                >r filenamesize cmove r>        \ store it
                filenamecount 1+                \ bump file count
                maxfilenames min to filenamecount ;

create filestack 40 cells allot
0 value filesp          \ fileid stack

: .files        filenamecount 0
                ?do     i filenamesize *  filenames +
                        cr i . count type
                loop    ;

: get-currentfile# ( -- )
\ this updates the current file# when a file is FLOADed
\ link this into start-include only after init-mem has been performed
\ pocket is the name if the newest include file
                currentfile#  filestack filesp 15 and th !
                1 +to filesp
                pocket filenumber to currentfile# ;

: prev-currentfile# ( -- )
\ get the previous file# from the file stack
                -1 +to filesp
                filestack filesp 15 and th @
                to currentfile# ;

maxtokens constant maxcfas
0 value wslink

: clearwatch    ( -- )                  \ clear the watch list
                watch-table maxwatch 2* cells erase
                0 to wslink
                0 to #watch ;

: new-headlist  ( -- )
                main-tokens 0 to-token# \ start at token # 0
                low-tokens  0 to-token#
                temp-tokens 0 to-token#
                0 to vocabularycount    \ new vocabulary list
                0 to ForthLink
                xt-table maxtokens cells erase
                0 to #cfas              \ clear the XT and CFA lists
                clearwatch
                cur-file filenumber to currentfile#
                                        \ add current file to the file list
                ;

: init-headlist ( -- )
                filenamesize maxfilenames *
                malloc to filenames
                vocabularies cells malloc to vocabularylist
                maxtokens   cells malloc to xt-table
                maxcfas 2*  cells malloc to cfa-table
                maxwatch 2* cells malloc to watch-table
                new-headlist
                0 to filenamecount      \ clear the file list
                ['] get-currentfile#  is start-include
                ['] prev-currentfile# is end-include ;

: free-headlist ( -- )
                vocabularylist release
                watch-table release
                cfa-table  release
                xt-table   release
                filenames  release
                ['] noop is start-include
                ['] noop is end-include ;

init-mem        chain-add init-headlist \ allocate memory for filenames
free-mem        chain-add free-headlist
init-tokens     chain-add new-headlist  \ clear the filename list

: addfile       ( n <filename> -- )
                drop bl word filenumber drop ;

: nofiles       ( -- )  0 to filenamecount ;

: addvocabulary ( nfa -- )
\ append vocabulary info to the list
                vocabularylist vocabularycount cells + !
                vocabularycount 1+ vocabularies min
                to vocabularycount ;

: vocname       ( index -- a len )
                vocabularylist swap th @ nfa-count ;

: .vocs         ( -- )
                vocabularycount 0
        ?do     i vocname type space
        loop    cr ;


((
   Data types:
   datatype is for use by the watch utility, which invokes the token and
   then performs an action to deal with the returned parameter(s).
   Datatype determines the action such as C@ EMIT or @ H. or U.

   There are 64 possible data types.  An array of PFAs is used to get to
   datatypes parameters

   The defining words :WR[ and :WW[ create a datastructure for the watcher.
   :WR[ creates a read action, :WW[ creates a write action.
   The syntax for their use is :WR[ <target words> ] <host words> ;

   Example: :WR[ VCREAD C@ ] TPOP 1 PAD C! PAD 1 ;

   When the watcher needs to update a word whose datatype uses VCREAD,
   it executes the word on the target so that the word leaves an address
   on the stack.  The words before " are evaluated on the target, which
   puts the contents of a byte variable on the stack.  The words after "
   are executed by the host.  They pop a cell off the target stack and
   return a 1-byte string containing the ASCII character.
))

64 constant datatypes

create datatypenames    datatypes cells allot
                        datatypenames datatypes cells erase
create datatypereads    datatypes cells allot
                        datatypereads datatypes cells erase
create datatypewrites   datatypes cells allot
                        datatypewrites datatypes cells erase

: create-datatype ( xt_rd xt_wr n <name> <text> -- | -- n )
\ create a data type n that behaves like a constant but compiles extra data
\ read action: display returned value
\ write action: store a new value
        create  >r  datatypewrites r@ th !      \ write action
                     datatypereads r@ th !      \ read action
                r@ ,
                here datatypenames r> th !      \ -> name
                bl word count  dup c,
                bounds  ?do  i c@ c, loop       \ store counted string
        does>   @ ;

: dt-struct     ( datatype -- a )
\ return the data structure of a given data type, 0 = no structure
                63 and datatypereads swap th @
                ?dup
                if      execute
                else    0
                then    ;

: dt>s          ( datatype -- a n )
\ return a string associated with a given data type
                63 and datatypenames swap th @
                ?dup
                if      count
                else    s"  "
                then    ;

: s>dt          ( a n -- n )
\ look up a datatype number for a string, 0 if not found
                temp$ place
                temp$ count upper       \ key is upper case
                datatypes
                begin   1- ?dup
                while   datatypenames over th @ ( cnt 'str )
                        pad over c@ 1+ move     ( cnt )
                        pad count upper
                        pad temp$ ccompare 0=
                        if      exit
                        then
                repeat  0 ;             \ not found

: :W"           ( <name> <string>; -- )
\ create a target-watch data structure
        create  [char] " parse -trailing \ get target evaluation string
                dup c,  bounds
                ?do     i c@ c,         \ string,
                loop    align
                begin   bl word
                        dup count s" ;" compare
                while   find 0=
                        abort" Not in search order"
                        ,               \ host xt string
                repeat  drop
                0 , ;

create $(.) 68 allot

: (.)(.)        ( n1 n2 -- a len )
                swap
                (.)   $(.)  place
                s"  " $(.) +place
                (.)   $(.) +place
                $(.) count ;

: treadstring   ( a n -- a-addr len )
                0max 68 min >r
                $(.) r@ read-data
                if      r> drop 0 >r    \ bad read, force zero length
                then
                $(.) r> -trailing ;      ( a n )


: tpops         ( -- x )  tpop sext ;

:W" tr-cell     " tpops (.) ;                   \ direct display cell
:W" tr-double   " tpop tpop swap tjoin (d.) ; \ direct display double
:W" tr-cellcell " tpops tpops (.)(.) ;          \ direct display 2 cells
:W" tr-hicell   " tpops tpop drop (.) ; \ direct display upper cell of double
:W" tr-locell   " tpop drop tpops (.) ; \ direct display lower cell of double
:W" tr-var    @ " tpops (.) ;                   \ indirect display cell
:W" tr-2var  2@ " tpop tpop swap tjoin (d.) ;   \ indirect display double
:W" tr-string   " tpop tpop swap treadstring ;  \ string

' tr-cell       0       1 create-datatype  dt.cell      cell
' tr-double     0       2 create-datatype  dt.double    double
' tr-var        0       3 create-datatype  dt.variable  [cell]
' tr-2var       0       4 create-datatype  dt.2variable [double]
' tr-cell       0       5 create-datatype  dt.constant  constant
' tr-double     0       6 create-datatype  dt.2constant 2constant
' tr-2var       0       7 create-datatype  dt.ramarray  [RAMdata]
' tr-2var       0       8 create-datatype  dt.romarray  [ROMdata]
' tr-string     0       9 create-datatype  dt.string    [string]
' tr-cellcell   0      10 create-datatype  dt.cellcell  cellcell
' tr-hicell     0      11 create-datatype  dt.hicell    hicell
' tr-locell     0      12 create-datatype  dt.locell    locell
0               0      13 create-datatype  dt.program   program


\ ----------  point to, fetch/store various token parameters

variable PFA-local
\ pointer to the parameter field of the last found token

variable PFA-token
\ -> parameter field for last token added

: nopfa         ( -- )   0 PFA-local ! ;
\ clear last PFA search result

: localdata@    ( n -- | -- n )
\ child: get info from the last found token header
        create  ,
        does>   @ cells PFA-local @ + @ ;

: 'localdata    ( n -- | -- a )
\ child: get info from the last found token header
        create  ,
        does>   @ cells PFA-local @ + ;

: 'tokendata    ( n -- | -- a )
\ child: address last created token header data
        create  ,
        does>   @ cells PFA-token @ + ;

1 constant #t.cfa

0 localdata@  t.cputype  ( -- n )    \ CPU type 1..31, 0 = tokenized
1 localdata@  t.cfa      ( -- n )    \ -> source
2 localdata@  t.datatype ( -- n )    \ data type
3 localdata@  t.fileid   ( -- n )    \ source file position
4 localdata@  t.filepos  ( -- n )    \ source file position
5 localdata@  t.xt       ( -- xt )   \ token number
6 localdata@  t.flags    ( -- n )    \ option flags + misc.
7 localdata@  t.litvalue ( -- n )    \ value if this word is a literal
8 localdata@  t.usage    ( -- n )    \ usage counter


1 'tokendata  t'cfa      ( -- a )    \ -> CFA
2 'tokendata  t'datatype ( -- a )    \ -> data type
6 'tokendata  t'flags    ( -- a )    \ -> flags for definition
6 'localdata  t'locflags ( -- a )    \ -> flags for found word
7 'tokendata  t'litvalue ( -- a )    \ -> value of literal
8 'localdata  t'usage    ( -- a )    \ -> value of usage counter
9 'tokendata  t'catstring ( -- a )   \ -> catalog (help) string

: t.catstring   ( -- a )        PFA-local @ 9 th ;

: t.filename    ( -- a )
                t.fileid filenamesize *  filenames + ;   ( a )

defer newheader   ( -- )
\ lays down a new header, assumed to be a byte
        :noname  ." <newheader>" ;      is newheader

defer 'newheader  ( -- a )
\ address of the last created target header byte
        :noname ." <'newheader=0>" 0 ;  is 'newheader

: tflags!       ( n -- | -- )
\ child: modify option flag(s) in the header and the image
       create   ,
       does>    @ >r
       r@  t'flags @ or t'flags !       \ modify host header
       r> 'newheader codebuf + codeorigin -   ( c a )
       tuck c@ or swap c! ;             \ modify image header byte

: tflags@       ( n -- | -- f )
\ child: get option flag(s)
       create   ,
       does>    @  t'locflags @ and 0<> ;

0x01 tflags!  call-only   \ cannot use tail recursion
0x01 tflags@  call-only?
0x02 tflags!  (immediate) \ target word is immediate
0x02 tflags@  immediate?
0x04 tflags!  macro       \ use in-line code
0x04 tflags@  macro?
0x08 tflags!  nobind      \ force use of static binding
0x08 tflags@  nobind?

: optim@        ( -- n )        t'locflags @ 8 rshift 0xFF and ;
: optim         ( n -- )
        dup>r 8 lshift t'flags @ or t'flags !   \ modify host header
        r> 'newheader codebuf + codeorigin - char+ c! ; \ modify image byte after status

: .flags        ( flags -- )
        dup 8 and if ." NOBIND " then
        dup 4 and if ." MACRO " then
        dup 2 and if ." IMMEDIATE " then
        dup 1 and if ." NOJUMP " then
        drop ;

create flaglist
        ,"    "           \ 0
        ," NoJump"
        ," Immediate"
        ," Immediate NJ"
        ," Macro"         \ 4
        ," Macro NoJump"
        ," Immediate Macro"
        ," Immed Macro NJ"

0x100 constant bit#tokenized?
\ b8 of cputype indicates that a word is tokenized code (not ROM machine)


: .localtoken   ( -- )
\ dump token data to console
                decimal  t.xt   u.
                hex      t.cfa  u.
                t.datatype dt>s type space
                t.flags 0xFFFF and u.
                decimal ;

: (usedtokens)  ( upper -- lastdefined )
                begin   1-
                        xt-table over th @   ( tk# f )
                        over 0= or
                until   ;

: usedtokens    ( -- n )
\ find the highest used token number, return the next free token#
                maxtokens (usedtokens)
                dup 0x1000 0x1FFF between       \ ignore relocatable tokens
                if      drop 0xFFF (usedtokens)
                then    1+ ;

: xt>npfa       ( xt -- nfa pfa )
\ return parameter field (header data) of an xt, 0 0 = not found
                dup maxtokens u> abort" XT>PFA failure: XT too big!"
                xt-table swap th @ dup dup
                if      n>link link> >body
                then    ;

: xt>cfa        ( xt -- cfa )
                xt>npfa nip  #t.cfa th @ ;

: xt>name       ( xt -- a len )
                xt-table swap th @ nfa-count ;

: nfa>pfa       ( nfa -- pfa )
\ Look up the pfa of a given nfa, store it.  pfa = 0 if not found
\ Skip non-existent tokens   *** not tested
                >r
                nopfa
                xt-table usedtokens         ( a n -- )
                begin   1- ?dup
                while   over @ ?dup         ( a n nfa . )
                        if      r@ =
                                if      n>link link> >body  ( a len pfa )
                                        dup PFA-local !
                                        3drop
                                        r>drop exit
                                then
                        then    cell under+
                repeat  drop
                r>drop ;

: (CIS)         ( xt cfa -- )
\ change the CFA of a given xt, used by DEFER and IS
                swap xt>npfa nip  #t.cfa th ! ;

: dynamicCFA    ( -- cfa )
\ compute CFA from token value
                b.bindorigin @ 1+
                t.xt jsrsize * + ;      \ -> 0th is at the end

: t.smartcfa    ( -- cfa )
\ CFA from headers if static or computed if dynamic
                dynamic?
                nobind? 0= and
                if     dynamicCFA       \ via the binding table
                else   t.cfa            \ or directly
                then   ;

0xF031847B constant magic_nosim

: tokenvocabulary ( -<name>- )
\ like vocabulary but operates on nos, leaves tos alone
                >system
        create  #threads #wordlist drop
                system>
                last @ addvocabulary
        does>   context @ >r
                PREVIOUS body> vcfa>voc context ! voc-also
                ALSO
                r> context !
                magic_nosim PFA-local ! ;

: cat(          ( -- )
\ Parse catalog help text string from the input stream in the form
\ c( This word does such-and-such)
                ')' word  dup c@
                if      count catstringsize leftjust   ( a len )
                        t'catstring swap move
                else    drop
                then    ;                      

create qasmlabs 10 cells allot  \ local labels @@0..@@9
: qasmlabs!  ( n th -- ) cells qasmlabs + ! ;
: qasmlabs@  ( th -- n ) cells qasmlabs + @ ;

vocabulary ASSEM
vocabulary ASMLABELS

: asmlabel      ( n -- )
\ define a new assembler label
                current @ >r
                also asmlabels definitions constant
                previous
                r> current !
                ;

2512846531 constant bogusvalue

: label>n       ( $label -- n )
\ get the value of a previously defined label, bogusvalue if not found
                >r get-order only asmlabels
                r> find 2>r
                set-order    2r>
                if      execute
                else    drop bogusvalue
                then    ;

: asmlabel?     ( <name> -- n )
\ get the value of a previously defined label
                bl word label>n
                dup bogusvalue = abort" not an asm label" ;

: (ASMBYTE)     ( n <name> -- )
                ram?
        if      >r vhere@  dup r> +  vhere!
        else    >r ihere@  dup r> +  ihere!
        then    current @ >r
                also asmlabels definitions
                constant
                previous
                r> current ! ;

: asmbyte       ( <name> -- )   1 (asmbyte) ;
: asmword       ( <name> -- )   2 (asmbyte) ;
: asmlong       ( <name> -- )   4 (asmbyte) ;

: getlabel      ( $name -- cfa | -1 )
                >r get-order r>
                previous previous
                find      2>r
                set-order 2r>
                nopfa
                if      execute
                else    drop -1 exit    \ couldn't find it
                then
                PFA-local @
                if      t.smartcfa
                else    true abort" Unknown label or bad number"
                then    t'usage incr    \ a reference bumps the usage tally
                nopfa ;

variable assemlast      \ -> begin of code marked by ASSEMBLE

: ?condition    ( f -- )        not abort" Conditionals Wrong" ;

: assemmacro    ( <name> -- )
\ create a word that lays down the bytes compiled between ASSEMBLE and here
                current @ >r
                also assem definitions
        create  ihere@ assemlast @  2dup -
                dup 1 100 between 0= abort" Invalid macro structure"
                c,                      \ save the count  ( hi lo )
                ?do     i 'image c@ c,  \ lay down the data
                loop
                previous
                r> current !
                ?condition previous
                assemlast @ ihere!      \ remove macro code from dictionary
        does>   count bounds
                ?do     i c@ ic,
                loop    ;

\ usage: assemble your_code_here macro: foo

: alab-find     ( a -- cfa 1/-1 | a 0 )
\ Look up in assembler labels list
                >r get-order
                only asmlabels
                r> caps-find   2>r
                set-order 2r> ;

: assemcfa?     ( <name> -- cfa f )
\ Finds the token <name> and returns its CFA.  CFA points to the code or to
\ the binding table, depending on Dynamic?
\ The top two wordlists in the search order, BUILDER and ASSEM, are excluded
\ from the search.  Flag is <>0 if not found.
                get-order
                bl word previous previous
                alab-find
        if      execute   >r            \ use assembler label value
                set-order r>
        else    find      2>r           \ look thru the search order
                set-order 2r>
                nopfa
                if      execute
                else    drop  0 1 exit  \ not a valid ASM address
                then
                PFA-local @
                if      t.smartcfa      \ use static or dynamic CFA
                else    nopfa 0 2 exit  \ not a token
                then    t'usage incr
        then    nopfa 0 ;

: assemcfa      ( <name> -- cfa )
\ Finds the token <name> and returns its CFA.  CFA points to the code or to
\ the binding table, depending on Dynamic?
\ The top two wordlists in the search order, BUILDER and ASSEM, are excluded
\ from the search.
                assemcfa?
                dup 1 = abort" not a valid ASM address"
                dup 2 = abort" not a token"
                drop ;


variable cfakey

: cfa>nfa       ( cfa -- nfa f )
\ binary search for a CFA, f=T if found
                cfakey !
                #cfas 1+ cheaplog
                1- 1 swap lshift
                dup                             ( index step )
                begin   ?dup
                while   2/
                        over #cfas 1+ u<
                        if      cfa-table pluck 1- 2* th @
                        else    -1              ( i s tcfa )
                        then
                        cfakey @  2dup =        ( i s n1 n2 )
                        if      3drop           ( i )
                                cfa-table swap 1- 2* th
                                cell+ @         \ get nfa
                                true exit
                        then    u<
                        if      tuck +
                        else    tuck -
                        then    swap
                repeat
                false ;

: .token        ( xt -- )
                xt>npfa  dup
                if      PFA-local !
                        cr NFA-COUNT tuck TYPE 16 swap - 0max SPACES
                        .localtoken
                else    2drop
                then    ;

: .tokens       ( -- )
                maxtokens 0
                do      i .token
                loop    ;

(( OLD VERSION -----------------------------------------------------
: values,       ( -- )
\ execute this at the end of a build file to build an initialization
\ table containing initialization data for data space
\ format: a n data, where a,n are cells and data is cells
\   a = destination in data space
\   n = # of bytes to copy from code space to data space
                rom  dataorigin i,
                vhere @ cellsize / 1+ dup i, 0
                ?do     databuf i cellsize * +          ( a )
                        (i@) i,
                loop    ;
))
\ run length encoding for values ------------------------------------------

variable reptally
variable repsource

: pack-lift     ( --  n )
                repsource @  (i@) fixend \ keep original byte order
                cellsize repsource +! ;

: pack-lay      ( n -- )        i, ;    \ lay down to ROM image

: pack-value    ( old -- )
                reptally @ 0= if drop exit then         \ no value
                reptally @ pack-lay     \ repeat_count
                pack-lay                \ data
                ;
: values,       ( -- )
\ execute this at the end of a build file to build an initialization
\ table containing initialization data for data space.  This data is run-length
\ encoded.
                dataorigin pack-lay
                databuf
                vhere @  cellsize 1- +          \ round last to full cell
                cellsize / 1+                   \ # of cells to pack  ( src n)
                >r repsource !
                -1 reptally ! 0 r>       ( old cnt )
                begin   ?dup
                while   1- >r  pack-lift        ( old new -- )
                        reptally incr
                        2dup <>
                        if      swap pack-value ( new )
                                reptally off
                        else    drop
                                r@ 0= if reptally incr then
                        then    r>
                repeat  pack-value
                0 pack-lay                      \ terminator
                ;
(( Use code similar to this to initialize RAM based on the data structure
   laid down by the above word:
   ======================================================

: plift         ( a -- a+cell n )  dup cell+ swap @ ; \ gets data from ROM
: valstore      ( n -- )        \ lay down cell to data space
                pad @ !  pad @ cell+ pad ! ;
: load-repeat   ( a cnt -- a' ) \ unpack multiple cells
                >r plift r>     ( a' data cnt )
                begin   dup
                while   1- over valstore
                repeat  2drop ;
: NEWVALUES     ( addr -- )     \ unpack the data structure at addr
                plift pad ! plift                       ( a n )
                begin   dup
                while   >r plift dup                    ( a' cnt . )
                        if      dup 15 invert and
                                if      valstore        \ cnt > 15
                                else    load-repeat     \ cnt = 1..15
                                then
                        else    drop plift load-repeat  \ cnt = 0
                        then    r> 1-
                repeat  2drop ;
))

3141520203 constant magiclit            \ this number if not a literal

: insertCFA     ( nfa cfa -- )
\ insert CFA, NFA into the cfa-table list so the list stays ordered by CFA
                #cfas
                if      #cfas 0                 ( n c . . )
                        do      cfa-table i 2* th @
                                over >
                                if      cfa-table i 2* th  \ space for new CFA
                                        dup dup 2 cells +  ( n c src src dest )
                                        #cfas i - 2* cells
                                        move 2!  0.0 leave
                                then
                        loop
                        2dup d0=
                        if      2drop           \ was inserted
                        else    cfa-table #cfas 2* th 2!
                        then
                else    cfa-table !             \ initialize an empty CFA list
                        cfa-table cell+ !
                then    #cfas 1+
                maxcfas 1- min  to #cfas ;

: predisassemble ( -- )
\ rebuild the CFA>NFA table for quick disassembler label lookup
                0 to #cfas
                usedtokens 0
                ?do     xt-table i th @
                        if      i xt>npfa drop
                                i xt>cfa  insertCFA
                        then
                loop    ;


: add-token     ( t# spos sid dt cfa cputype <name> -- )
\ add a token to the current vocabulary
\ t# = token#
\ sid = source file ID
\ spos = file position in source file
\ dt = data type
        create  here PFA-token !
                ,                       \ cputype
                dup     >r ,            \ cfa
                ,                       \ datatype
                , ,                     \ sid spos
                dup ,                   \ xt#
                0 ,                     \ flags off  ( 'xt )
                magiclit ,              \ not a literal
                0 ,                     \ usage
                catstringsize 0         \ blank catalog string
                do bl c, loop
                last @  r>  insertCFA
                last @ xt-table rot th ! \ store pointer to this token's NFA
        does>   PFA-local !
        ;

: isliteral?    ( -- f )        t.litvalue magiclit <> ;

: >literal      ( n -- )        t'litvalue ! ;
: >flags        ( n -- )        t'flags ! ;

: tokendata     ( t# spos sid dt cfa cputype )
        t.xt t.filepos t.fileid t.datatype t.cfa t.cputype ;

\ 0 localdata@  t.cputype  ( -- n )    \ CPU type 1..31, 0 = tokenized
\ 1 localdata@  t.cfa      ( -- n )    \ -> source
\ 2 localdata@  t.datatype ( -- n )    \ data type
\ 3 localdata@  t.fileid   ( -- n )    \ source file position
\ 4 localdata@  t.filepos  ( -- n )    \ source file position
\ 5 localdata@  t.xt       ( -- xt )   \ token number
\ 6 localdata@  t.flags    ( -- n )    \ option flags + misc.
\ 7 localdata@  t.litvalue ( -- n )    \ value if this word is a literal

: create-token  ( datatype cputype <name> -- | -- )
\ create a token from the current input stream
                >r >r token# loadline @ currentfile#
                r> ihere@ r> add-token  \ create the token
                token# 1+ to-token#
                token# maxtokens >=
                if      token# . true abort" Token number too high: "
                then    ;

: sourcefile    ( fileid line# -- )
\ modify last token's source file pointers
                swap
                PFA-token @ cell+ swap !+ ! ;

: datatype      ( x <name> -- )
\ modify last token's datatype
                bl parse s>dt
                ?dup
                if      t'datatype !
                else    true abort" Undefined Data Type"
                then    ;

variable badname

: $findtoken    ( a -- )
\ find a named token, abort with error if not found
                dup badname !
                underfind                     \ search the order
                if      nopfa
                        true \ ||| how to execute only tokens?
                        if      execute  PFA-local @  \ found, execute it
                                ?exit
                        else    drop
                                true abort" Not a token#"
                        then
                else
                        drop
                then
                badname @ count type
                true abort" not in the search order"
                ;

: findtoken     ( <name> -- )  bl word $findtoken ;


\ --------------------- watch support -------------------------------------

\ The watch-table array contains info for the watcher.  Each record contains:
\ Cell#0 = the token# of the token to watch
\ Cell#1 = the address of the data structure for the reader action

\ watch string format: link, counted_string

: stlabel       ( a len -- )
\ store a string to the wslink linked list, use this when adding a watch label
                align wslink here to wslink , dup c,
                bounds ?do i c@ c, loop ;

: appendwatch   ( readaction xt -- )
\ append a new watch action to the watch table
                watch-table #watch 2* th  2!
                #watch 1+  maxwatch 1- min to #watch
                nopfa ;

: watchgen      ( address format type <string"> -- )
                '"' word count stlabel 16 lshift + appendwatch ;


:noname         ( -- )
                t.datatype dt-struct
                t.xt appendwatch ; is _addwatch

: xtaddwatch    ( xt -- )
\ look up token, add its default reader to the watch table
                xt>npfa  swap
                if      PFA-local !  _addwatch
                else    drop
                then    nopfa ;

: addwatch      ( <name> -- )
\ look up token, add its default reader to the watch table
                findtoken t'usage incr _addwatch ;

: watched?      ( xt -- f )
\ search the watch list for xt, T if found
                #watch
                begin   ?dup
                while   1-
                        watch-table over 2* th @        ( xt cnt xt' )
                        pluck =
                        if      2drop true
                                exit
                        then
                repeat  drop false ;


variable isdefined
variable lastvoc

: (isdefined?)  ( voc -- )
\ scan tokens in a wordlist that is known to contain all token headers or
\ non-stack-altering words. isdefined -> parameters if found, 0 otherwise
                DUP VOC#THREADS >R
                HERE 500 + R@ CELLS MOVE        \ copy vocabulary up
                BEGIN   HERE 500 + R@ LARGEST DUP
                WHILE   nopfa
                        DUP LINK> execute       \ -> xt parameters
                        PFA-local @
                        if      dup L>NAME
                                NFA-COUNT       ( a n )
                                temp$ count compare 0=
                                if      PFA-local @ isdefined !
                                then
                        else    L>NAME NFA-COUNT type
                                ."  -- Error searching token list"
                                cr lastvoc @ vocname type
                                true abort" Wordlist must contain only tokens:"
                        then
                        @ SWAP !
                REPEAT  2DROP  R>DROP ;

: _isdefined?   ( a n -- f )
\ Scan all tokens in the search order, return T if found.
                temp$ place
                temp$ count upper        \ dictionary names are upper case
                isdefined off
                #vocs 1-  0
        ?do     context i th @  ?dup          \ scan through the context list
                if      voc>vcfa >name        \ nfa of a voc in the order
                        vocabularycount 0
                        ?do     vocabularylist i th  @
                                over =        \ see if it's in the VOC list
                                if      ( valid_vocabulary )
                                        i lastvoc !
                                        context j th @ (isdefined?)
                                then
                        loop    drop
                then
        loop    isdefined @
        dup PFA-local !
        ;

: isdefined?    ( <name> -- f )
\ look ahead in the input stream for the token name but don't consume it
                >in @ >r
                bl word count _isdefined?
                r> >in ! ;


\ ---------------------------------------------------------------------
\ Disassembler

: .label        ( targetaddr -- )
\ Display label [CFA] with associated name, used by all disassemblers
                base{  s" 0x" >$
                dup (u.)   >$                   \ address
                dup code-bounds? within         \ destination in binding table?
                if      ( dynamic label )
                        b.bindorigin @ 1+ swap - jsrsize abs / ( xt )
                        dup s" (xt:"  >$
                        decimal (.)   >$        \ (xt:XT)
                            s" ) "    >$
                        xt>npfa
                        if      NFA-COUNT >$
                        else    drop            \ no such xt defined yet
                                s" unknown" >$
                        then
                else    ( static label )
                        cfa>nfa
                        if      s" :" >$
                                NFA-COUNT >$    \ :NAME
                        else    drop
                        then
                then    
                }base nopfa ;

: .asmlabel     { n \ w#threads goodlink -- }
\ search the asmlabels wordlist for a constant that matches n
                also asmlabels
                0 to goodlink
                context @  dup voc#threads to w#threads
                dup here 500 + w#threads cells move     \ copy vocabulary up
                begin   here 500 + w#threads largest dup
                        goodlink 0= and
                while   dup link> >body @ n =
                        if      dup to goodlink         \ found
                        then
                        @ swap !
                repeat  3drop
                previous
                goodlink ?dup
                if      l>name nfa-count                \ found, use name
                else    base{ n (.) }base               \ not found, use hex
                then    >$
                ;

: .asmlabels    { \ w#threads -- }
\ search the asmlabels wordlist for a constant
\ display labels with values
                also asmlabels
                context @  dup voc#threads to w#threads
                dup here 500 + w#threads cells move     \ copy vocabulary up
                begin   here 500 + w#threads largest dup
                        key? 0= and
                while   dup l>name nfa-count 16 leftjust type
                        dup link> >body @ cellnibbles (h.) type cr
                        @ swap !
                repeat  3drop cr
                previous ;

: disname       ( targetaddr -- )
\ Append label string to PAD
                cfa>nfa
                if      NFA-COUNT dup>r  pad +place     \ NAME
                        s" : "           pad +place
                        spcs 7 r> - 0max pad +place     \ tab it over
                else    drop
                        s"          "    pad +place
                then
                nopfa ;

' disname is _disname

: sdisassemble  ( at ah -- at' a n )
\ code: disassemble code
\ data: display ASCII equivalent, allow disassyDatawidth (or 4) bytes of data
                dup codebuf - dmark?
        if      s" db" outpad place
                '"' >c bl >c
                disassyDatawidth 4 min bounds
                do      i codebuf - dmark?
                        if      i c@ 127 and dup bl < if drop '.' then >c
                        then    1+
                        i 1+ codebuf - dmark? 0= if leave then
                loop    ialigned
                '"' >c
                outpad count
        else    disassemble
        then    ;

: (disassem)    ( a -- a' a len )
\ Dump address and data to PAD, return string result of disassembly
                dup dup codebuf + codeorigin -      ( at at ah )
                dup >r                            \ -> buffer data
                sdisassemble 2swap                ( a n at at' | ah )
                dup rot -                         ( a n at' len | ah )
                r>  over                          ( a n at' len a len )
                0 pad c!  disoption0
        if
                disassyDatawidth max  bounds      ( at' len . . )
                ?do     2 - dup 0<                \ swap even and odd bytes
                        if      s"     "          \ assumes even byte count
                        else    i w@ 4 (h.)
                        then    pad +place        \ data
                2 +loop drop
        else
                disassyDatawidth max  bounds      ( at' len . . )
                ?do     1- dup 0<
                        if      s"   "
                        else    i c@ 2 (h.)
                        then    pad +place        \ data
                loop    drop
        then
                s"  "           pad +place        \ space
                -rot ;

: $(disassemble) ( a -- a' )
\ disassemble one line of machine code at a given address to PAD
                dup ?codebounds
                dup >r (disassem)
                r> disname
                pad +place                        \ disassembly string
                ;

: $disassemble1 ( a -- a' )
\ disassemble one line to the screen
                winpause              \ keep Windows message handler going
                dup
                $(disassemble) swap  cr
                charsize /                        \ char address units
                addrnibbles (h.) type space       \ address
                pad count type ;                  \ the rest of it

: nextcfa       ( cfa -- cfa' )
\ find the next CFA with a token assigned to it
                >r   imagesize codeorigin +
                maxtokens                       ( lowest cnt | thisCFA )
                begin   ?dup
                while   1-  xt-table over th @
                        if      dup xt>cfa      ( lo cnt cfa | this )
                                dup r@ >
                                if      rot umin swap
                                else    drop
                                then
                        then
                repeat
                r>drop ;


: $disassemble  ( a -- )
\ disassemble a word, ESC quits if things get out of hand
                dup -1 =
                if      drop cr ."  CFA is unknown"
                else    dup nextcfa swap        ( end begin )
                        begin   $disassemble1   ( end a )
                                2dup <=
                                key? if key 27 = or then
                        until   2drop cr
                then    ;

: dis           ( <name> -- )
\ disassemble a word in the search order
                cr ( dup 1- ic@ ) t.flags .flags
                findtoken t.cfa $disassemble  nopfa ;

: nowordlist?   ( a len -- f )
\ return T if a vocabulary isn't in the search order
                #vocs 1-  0
        ?do     context i th @  ?dup    \ test name of each voc in the order
                if      voc>vcfa >name nfa-count
                        2over compare 0=
                        if      2drop unloop false exit
                        then
                then
        loop    2drop true ;


:noname         ( -- )
\ append CORE to the search order if it's not there
                s" CORE" nowordlist?
                if      underalso c" CORE" find
                        if execute else abort" CORE wordlist not found" then
                then    ; is forceCORE

: forceSYSTEM   ( -- )
\ append SYSTEM to the search order if it's not there
                s" SYSTEM" nowordlist?
                if      underalso c" SYSTEM" find
                        if execute else abort" SYSTEM wordlist not found" then
                then    ;

: icsum         ( address length -- sum )
\ compute checksum of a block of image data
                >r 'image r> bounds
                0 -rot
        ?do     i c@ +
        loop    ;

((
  EEPROM usage:

  b.ee0         Offset to primary boot
  b.ee0 + 2     Offset to secondary boot (if any)
  b.ee0 + 4     User Data
  ...           Secondary boot program
  ...           Primary boot program
  b.eesize-1    Highest allowable address

  When placing boot programs, location is computed from EEMAX and the length.
  Secondary needs the length of the primary boot program.  It will not be
  sent if there is no primary program.
))

variable secmax
variable secsize

: .bootrange    ( a n -- a n )
                over hex. ." .." 2dup + 1- hex. ;

: _boottst      ( -- f )
                b.eesize @  0=
                abort" Target doesn't have a boot EEPROM" ;

: _bootwr       ( src dest -- )
                ihere @  true to tarprogress?
                cr ." Writing " .bootrange write-ee ?werror ;

: _bootload     ( -- )
\ loads image into target eeprom primary boot area
                _boottst codebuf
                b.eesize @ cellsize * ihere @ -         \ offset
                dup 5 < abort" Primary boot program won't fit."
                dup byte-swap pad w!
                pad b.ee0 @ 2 write-ee ?werror  \ store offset of primary boot
                b.ee0 @ +  _bootwr ;

: _bootload2    ( -- )
\ loads image into target eeprom secondary boot area
                _boottst
                b.ee0 @ pad 2 read-ee ?rerror   \ find primary boot program
                pad w@ byte-swap b.ee0 @ +      \ -> header
                dup>r
                pad 8 read-ee ?rerror           \ pad = primary header
                pad w@ 0xC907 <> abort" Primary boot program not found."
                r> ihere @ -                    ( 'dest )
                dup 5 < abort" Secondary boot program won't fit."
                dup>r byte-swap pad w!
                pad b.ee0 @ 2 + 2 write-ee ?werror \ store offset of secondary boot
                codebuf r> _bootwr ;

\ derived from win32forth nforget.f
\ these also reset token#

: marker ( -<name>- ) ( ANS)
                CURRENT @
                also home definitions
        create  here body> , TOKEN# , ihere @ , vhere @ ,
                previous
                CURRENT !
        does>   @+ (forget)
                @+ to-token#
                @+ ihere !
                @+ vhere !
                drop ;

: anew          ( -<name>- )    \ define a new marker
                >in @ defined
                if      execute
                else    drop
                then    >in ! marker ;


