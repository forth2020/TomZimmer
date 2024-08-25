((

        HOME Forth wordlist

        This wordlist contains CORE and CORE EXT Forth words, as well
        as useful Firmware Studio specific words.

))

variable homing

only forth also home definitions
false to sys-warning?

\ Firmware Studio --------------------------------------------------

: {{            ( string}} -- )
\ interpret text until }} using this wordlist
                homing on
        begin   get-order  only home
                bl word find 2>r
                set-order    2r>
                if      execute
                else    number
                        double? 0= if drop then
                then
                homing off?
        until   ;               IMMEDIATE

: }}            ( -- )
                homing off ;    IMMEDIATE

variable debugging
: \D debugging @ 0= if postpone \ then ; immediate
: \ND debugging @   if postpone \ then ; immediate

: <builder>  (previous) host also forth also builder (definitions) (previous) ;
: </builder> previous building underdefs ;

: timestamp     ( -- d )
\ Get 32-bit time stamp starting at Jan 1st, 2000.  1-second resolution.
                time&date 2000 -        ( s m h d m y )
                12 * swap +             \ Rolls over every 133 years.
                31 * swap +
                24 * swap +
                60 * swap +
                60 * + ;

: disassemble   ( lower upper -- )      \ disassemble a range of addresses
                2dup u< if swap then    \ convert to ( upper lower )
        begin   $disassemble1           \ one line   ( upper lower' )
                2dup u<
                key? if key 27 = or then
        until   2drop ;

: dsave         ( <filename> -- )       \ save ROM image as disassembled output
                bl word openoutput      \ create output file
                ihere@ codeorigin       ( upper lower )
        begin   dup $(disassemble) swap
                charsize / addrnibbles (h.)  \ address
                       outfile write-file ?fwerror
                s"   " outfile write-file ?fwerror
                fendline
                2dup u<
        until   2drop
                closeoutput ;           \ close file

: uf1.0         ( -- d )
\ target's equivalent of 1.0
                cellbits 32 =
        if      0 1
        else    -1 cellbits lshift invert 1+ 0
        then    ;

: sf            ( <number> -- n )
\ get signed fractional number ( -1.0 to +1.0 ) from input stream,
\ convert to signed integer ( minint .. maxint )
                bl parse >float 0= abort" Expecting fractional number .XXXXX"
                fdup -1e0 f<
                fdup  1e0 f> or abort" Range must be -1.0 .. +1.0"
                uf1.0 d>f 2e0 f/ f* f>d 0=
                if      dup uf1.0 d2/ drop = if 1- then \ clip +1.0 to maxint
                then    ;

: uf            ( <number> -- n )
\ get unsigned fractional number ( 0 to +1.0 ) from input stream,
\ convert to umsigned integer ( 0 .. umaxint )
                bl parse >float 0= abort" Expecting fractional number .XXXXX"
                fdup  0e0 f<
                fdup  1e0 f> or abort" Range must be -1.0 .. +1.0"
                uf1.0 d>f f* f>d
                if      1-              \ clip +1.0 to umaxint
                then    ;

: path+ fpath+ ;  : .path .fpath ;
: .depth depth . ; immediate
: .here         ( -- )  communicable? if tarhere . then ;

: tar?          tar? ;  : .hot .hot ;
: org           ( a -- )  codeorigin - ihere ! ;     \ new origin for image
: bounds>na bounds>na ; : >showme to showme ;
: homeorder homeorder ; : >token# to-token# ;   : token# token# ;
: asmbyte asmbyte ;     : asmword asmword ;     : asmlong asmlong ;
: asmlabel asmlabel ;   : asmlabel? asmlabel? ; : asmarray (asmbyte) ;
: .asmlabels .asmlabels ;

: >as_code to specaddr0 ; : >as_data to specaddr1 ;
: >as_reg  to specaddr2 ; : >as_ee   to specaddr3 ; : >as_img to specaddr4 ;

: >@@0 0 qasmlabs! ;    : >@@1 1 qasmlabs! ;    : >@@2 2 qasmlabs! ;
: >@@3 3 qasmlabs! ;    : >@@4 4 qasmlabs! ;    : >@@5 5 qasmlabs! ;
: >@@6 6 qasmlabs! ;    : >@@7 7 qasmlabs! ;    : >@@8 8 qasmlabs! ;
: >@@9 9 qasmlabs! ;
: $dis $disassemble ;   : view view ;
: low-tokens  low-tokens ;                      : .procs .procs ;
: main-tokens main-tokens ;                     : crc crc-ccitt ;
: temp-tokens temp-tokens ;
: bsave bsave ;         : hsave hsave ;         : ssave ssave ;
: tsave tsave ;         : ndxsave idxsave ;
: addwatch addwatch ;   : clearwatch 0 to #watch ;
: cls <m-> cls ;        : ip= ip= ; : ip? ip? ; : ip: ip: ;
: include-binary ,bload ;   : ~boot0 image>boot0 ;
: hload hload ;         : $hload $hload ;       : comlog comlog ;
: xload-bl51 xload-bl51 ; : $xload-bl51 $xload-bl51 ;

: fsend bl word $sendfile ; : ~boot1 image>boot1 ; : defmacro defmacro ;
: sipprog ( node <filename> -- ) bl word $sipprog ;

: fload fload ;         : include include ;     : needs needs ;
: shell shell ;         : ok ok ;               : loadlabels loadlabels ;
: dos dos ; : sys sys ;
: cell  cellsize charsize / ;   \ assume minimum addressable unit is char
: cells cellsize charsize / * ;
: maxuint -1 cellbits lshift invert ;
: cputype cputype ;     : .vocs .vocs ;
: VMtemplate VMtemplate ;  : CodeTemplate CodeTemplate ;
: >cellbits to cellbits ;       : cellbits cellbits ;
: >addrnibbles to addrnibbles ; : addrnibbles addrnibbles ;
: >charbits to charbits ;       : charbits charbits ;
: branchbytes branchbytes ;     : alignment calignment dalignment max ;
: calignment calignment ;       : dalignment dalignment ;
: edit edit ;           : e e ; : b b ;         : browse browse ;
: marker marker ;       : anew anew ;
: _debug also forth debug previous ;
: _dbg   also forth dbg   previous ;
: host host ;
: building building ;   : bi bi ;
: tokenizing tokenizing ;  : ti ti ;
: testing testing ;     : te te ;
: forthing forthing ;   : fi fi ;
: bug   hbug ;          : makedefining makedefining ;
: trace lbug ;          : (see) see ;           : see isee ;
: (words) (words) ;     : .files .files ;
: NEW-IMAGE NEW-IMAGE ; : NOFILES NOFILES ;     : DATATYPE DATATYPE ;
: WARNING WARNING ;     : VOCABULARY TOKENVOCABULARY ; : >FLAGS >FLAGS ;
: ADDFILE ADDFILE ;     : ADD-TOKEN ADD-TOKEN ; : >LITERAL >LITERAL ;
: UNDERALSO UNDERALSO ; : UNDERDEFS UNDERDEFS ; : UNDERPREVIOUS UNDERPREVIOUS ;
: noslack noslack ;
: loadROM loadROM ;             : >port# to parallelport ;
: byte-split byte-split ;       : word-split word-split ;
: byte-join byte-join ;         : word-join word-join ;
: data-bounds? data-bounds? ;   : code-bounds? code-bounds? ;
: rom-bounds? rom-bounds? ;     : checksum icsum ;
: data-bounds data-bounds ;     : code-bounds code-bounds ;
: rom-bounds rom-bounds ;       : .status .status ;
: scramble-init scram-init ;
: scramble scramble ; : scramble16 scramble16 ; : scramble32 scramble32 ;
: .scram .scram ;  : scram-d scram-d ;  : scram-a scram-a ;
: static  false to dynamic? ;   : hex[ hex[ ; immediate
: dynamic true  to dynamic? ;
: reset tarreset begin winpause test-comm on-line? until ;
: commo=avr    commo=avr ;
: commo=ip     commo=ip ;
: commo=serial commo=serial ;
: commo=multidrop commo=multidrop ;
: modem" modem" ;  : RS485 RS485 ;  : MODEM MODEM ;
: >SmartDongle to SmartDongle? ;
: open-comm open-comm ; : close-comm close-comm ;

: 485port       ( port# -- )    to mdport# ;
\ Sets the desired COM port, usually 1..4.
: 485addr       ( address -- )  0x80 or to packetaddr ;
\ Sets the UUT address.  0..119 are usable.
: 485baud       ( baud -- )     to mdbaud ;
\ Sets the baud rate for multidrop communication.
: begin-comm    ( -- )          _com[ ;
: end-comm      ( -- )          _]com ;

: watchcode"    ( address format string" -- )  1 watchgen ;
: watchdata"    ( address format string" -- )  2 watchgen ;
: watchreg"     ( address format string" -- )  3 watchgen ;
: watchee"      ( address format string" -- )  4 watchgen ;

: g_chan        Channel: graf ;
: g_addr        Addr: graf ;
: g_data        Data: graf ;
: g_length      Length: graf ;
: g_center      Center: graf ;
: g_gain        Gain: graf ;
: g_pitch       Pitch: graf ;
: g_color       Color: graf ;
: g_type        Type: graf ;
: g_clear       Clr: graf ;

dlport0? [if]
: loadflash ( -- ) codebuf codeorigin ihere @ progflash ;
[else]
: loadflash ( -- ) ." No port access" ;
[then]


\ "standard" words

: create create ;       : constant constant ;   : value value ;
: variable variable ;   : on on ;               : off off ;

: chars chars ;         : bye bye ;             : order order ;
: negate negate ;       : invert invert ;       : um/mod um/mod ;
: lshift lshift ;       : rshift rshift ;       : previous previous ;
: only only ;           : also also ;           : definitions definitions ;
: forth forth ;         : home home ;           : words words ;
\ : place place ;

: swap swap ;   : drop drop ;   : nip nip ;     : over over ;
: dup dup ;     : rot rot ;     : pick pick ;   : tuck tuck ;
: */ */ ;       : UM* UM* ;     : 2/ 2/ ;       : u2/ u2/ ;
: /mod /mod ;   : 1+ 1+ ;       : 1- 1- ;       : 2* 2* ;
: hex hex ; : decimal decimal ; : binary binary ; : base base ;
: <> <> ;       : 0= 0= ;       : 0< 0< ;       : >= >= ;
: <= <= ;       : < < ;         : = = ;         : > > ;
: + + ; : - - ; : * * ; : / / ; : @ @ ; : ! ! ; : . . ; : cr cr ; : .s .s ;
: or or ;       : and and ;     : xor xor ;     : emit emit ;
: between between ; : within within ;

: c(  cat( ;       immediate
: to postpone to ; immediate
: \  postpone \  ; immediate
: // postpone \  ; immediate
: \S postpone \S ; immediate
: (  postpone (  ; immediate
: (( postpone (( ; immediate
: .( postpone .( ; immediate
: #if    postpone #if    ; immediate
: #else  postpone #else  ; immediate
: #then  postpone #then  ; immediate
: [if]   postpone [if]   ; immediate
: [else] postpone [else] ; immediate
: [then] postpone [then] ; immediate
: [char] postpone [char] ; immediate

only forth also definitions
true to sys-warning?


