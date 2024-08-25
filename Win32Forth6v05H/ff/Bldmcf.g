((
\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
BUILDER lexicon for ColdFire target

Subroutine threaded, big endian

Cell size = 32 bits
TOS is cached in a data register (see register assignments below)

\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
))

homeorder also forth also definitions

 8 to addrnibbles       \ default to 32-bit addresses
-6 to jsrsize           \ size of long jump required by binding table
32 to cellbits          \ 32 bits per cell
 8 to charbits          \ 8 bits per character
 2 to calignment        \ instructions must be at even addresses
 4 to dalignment        \ data should be on longword boundaries
                        \ 68K kernel assumes all alignments = 4
 4 to branchbytes       \ control structure branches are 4 bytes long
-1 to commonmem?        \ code space and data space overlap
 0 value rel            \ relative addressing off

vocabulary BLD-PRIVATECF   BLD-PRIVATECF definitions
\ Private part of ColdFire builder lexicon
false to sys-warning?

                        \ Register Assignments:
7 constant myTOS        \ data register to use for TOS
6 constant myBASE       \ address register to use for base pointer
5 constant mySP         \ address register to use for data stack pointer
4 constant myADDR       \ address register to use for address register

\ register usage:
\ D0-D3/D7 = forth     D4..D6 = user interrupts
\ A0-A2/A4-A7 = forth  A3     = user interrupts

hex

defer aw,       ( n -- )                \ lay down a 16-bit cell
                ' drop is aw,
defer tahere    ( -- a )                \ current target address
                ' false is tahere
defer tahere!   ( a -- )                \ set new target address
                ' drop is tahere!
defer ta@       ( a -- n )              \ fetch 16-bit word from target code
                ' noop is ta@
defer ta!       ( n a -- )              \ store 16-bit word to target code
                ' 2drop is ta!
defer labeladdr ( $name -- a )          \ addr of code with label, -1 if none
                ' noop is labeladdr

: al,           ( n -- )                word-split aw, aw, ;
: 9<<           ( n -- n' )              9 lshift ;
: 12<<          ( n -- n' )             0C lshift ;

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ \\\\\\\\\\                    COMPILER                          \\\\\\\\\\
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

variable 'olast  \ -> start of last instruction
variable  omode  \ type of instruction left by optimizer
variable  ovalue \ value associated with omode, if any

4E75 constant #RTS

: >movel        ( rs ms rd md -- opcd )         \ build a MOVE.L instruction
                swap 8 * + 8 * + 8 * + 0x2000 + ;

myTOS 0 2constant TOS
mySP  1 2constant S         7 1 2constant RP    myBASE 1 2constant TP
mySP  2 2constant (S)       7 2 2constant (R)   myBASE 2 2constant (TP)
mySP  3 2constant (S)+      7 3 2constant (R)+  myBASE 3 2constant (TP)+
mySP  4 2constant -(S)      7 4 2constant -(R)  myBASE 4 2constant -(TP)

\ omode usage: last operation was a:
\ 0=unknown 1=push 2=pop
\ 3=CCR describes TOS status
\ 4=last was MOVE A0,TOS
\ 5=last was MOVE D0,TOS

\ 10=short-lit 11=long-lit 12=BSR.S 13=BSR 14=JSR16 15=JSR32

: ,ret          ( -- )      #RTS aw, ;
: ,push         ( -- )      [ tos -(s) >movel ] literal aw,    1 omode ! ;
: ,pop          ( -- )      [ (s)+ tos >movel ] literal aw,    2 omode ! ;
: ofl           ( -- )      omode off ;                 \ flush optimizer
: onip          ( n -- )    tahere swap - tahere! ofl ; \ drop last n bytes
: omark         ( -- )      tahere 'olast ! ;
: unmark        ( -- )      'olast @ tahere! ofl ;      \ drop last sequence
: waslit?       ( -- f )    omode @ 10 11 between ;     \ last was literal?
: ,ovalue       ( -- )      ovalue @ al, ofl ;
: ,opush        ( -- )      omode @ 2 = if 2 onip else ,push then ;
: ,opop         ( -- )      omode @ 1 = if 2 onip else ,pop then ;

: ,lit          ( n -- )
                dup ovalue !  omode @ 2 <> >r
                omark ,opush  dup abs 80 u<             \ MOVE.L TOS,-(S)
                if      0FF and
                        [ myTOS 9 lshift 0x7000 + ] literal
                        + aw,                       10  \ MOVEQ #n,TOS
                else    [ 4 7 TOS >movel ]
                        literal aw, al,             11  \ MOVE.L #n,TOS
                then    r> and omode ! ;      \ 10 or 11 if push not optimized

: >amark       ( opcode -- f addr )     aw, true tahere  0 aw, ;
: >aresolve    ( f addr -- )            >r tahere r@ -  dup abs 07E >
                abort" branch too long to resolve" r@ ta@ + r> ta! ?condition ;
: <amark       ( -- f addr )            true   tahere  ;
: <aresolve    ( f addr opcode -- )     >r tahere 2 + -  dup abs 80 <
                if  0FF and r> + aw, else r> aw, aw, then ?condition   ;

\ literal optimization helpers ----------------------------------------------

: quick+:       ( opcode1 opcode1 <name> -- ) ( -- )
                create , , does> ovalue @       ( 'opcds val )
                unmark
                dup 0= if 2drop exit then       \ 0     nothing to do
                dup 9 u<
                if      7 and 9 lshift swap cell+ @ or aw,    \ 1..8
                else    swap @ aw, al,          \ > 8
                then    ofl ;

5080 myTOS +    0680 myTOS +    quick+: ,+lit
5180 myTOS +    0480 myTOS +    quick+: ,-lit

: quicklit:     create , does> @ unmark aw, ,ovalue ;

0080 myTOS +    quicklit: ,orlit
0280 myTOS +    quicklit: ,andlit
0A80 myTOS +    quicklit: ,xorlit

: quicksh:      ( opcode <name> -- ) ( -- )
                create , does> @ ovalue @       ( opcd val )
                unmark
                dup 0= if drop exit then
                dup 1F u> abort"  Shift count is over 31"
                7 and 9 lshift or aw, ;

0xE080 myTOS +  quicksh: ,a>>lit
0xE180 myTOS +  quicksh: ,a<<lit
0xE088 myTOS +  quicksh: ,>>lit
0xE188 myTOS +  quicksh: ,<<lit

: ,picklit      ( -- )
                unmark ,push
                [ mySP 5 tos >movel ] literal aw,       \ MOVE.L s16(S),TOS
                ovalue @ 2 lshift aw, ;


\ program flow -------------------------------------------------------------

: ,testflag     ( -- )
                [ tos 0 0 >movel ] literal aw,          \ MOVE.L TOS,D0
                ofl ,pop ofl                            \ MOVE.L (S)+,TOS
                4A80 aw, ;                              \ TST.L D0

: ,testtos      ( -- )
                omode @ 3 <>                            \ status already known?
        if      [ 4A80 myTOS + ] literal aw, ofl        \ TST.L TOS     ( no )
        then    ;

: ,call         ( ta -- )
                omark  dup tahere - 2 -             ( ta disp )
                dup abs 8000 u<
                if      nip                         ( disp )
                        dup abs 80 u<
                        if      0FF and 6100 + aw,  12  \ BSR.S
                        else    6100 aw, aw,        13  \ BSR
                        then
                else    rel @                       \ relative long jump?
                   if   nip 61FF aw, al,            16  \ BSR.L
                        family @ 0= abort" Can't use BSR.L"
                   else drop dup abs 8000 u<        ( ta . )
                        if      4EB8 aw, aw,        14  \ JSR abs short
                        else    4EB9 aw, al,        15  \ JSR abs long
                        then
                   then
                then    omode !
                ;

: bsr>bra       ( offset -- )  tahere swap -  dup ta@ FEFF and swap ta! ;
: jsr>jmp       ( offset -- )  tahere swap -  dup ta@ 0040 or  swap ta! ;

: ,exit         ( -- )
                omode @
                call-only? 0= and
                case    12 of    2 bsr>bra      endof
                        13 of    4 bsr>bra      endof
                        14 of    4 jsr>jmp      endof
                        15 of    6 jsr>jmp      endof
                        16 of    6 bsr>bra      endof
                        ,ret
                endcase ofl ;

: ,macro        ( a -- )
                100 swap            ( maxcount a )
                dup ta@ 6000 = if 4 + then      \ macro can't start with 6000xxxx
                begin   dup ta@  dup #RTS =     ( maxcnt a instr . )
                        if      2drop 100 = if ofl then \ nothing compiled
                                exit
                        then    showme
                        if      dup h.
                        then    aw, 2 +
                        >r 1- r> over 0=        \ runaway protection
                until   true abort"  Inline expansion exploded"
                ;

: ,swallow      ( a #new #old -- )
\ delete useless simple code sequences formed when concatenating inline code
                showme
        if      cr ." Removing " dup . ." words from previous, " over . ." from new. "
        then
                2* tahere swap - tahere!        \ strip some code from previous word
                over ta@ 6000 =                 \ if new word doesn't start with 6000
                if swap 4 + swap then 2* +      \ skip beginning code of new word
                ,macro ;

: ,eat01        ( a -- )        1 0 ,swallow ;  \ remove 1 instruction from new word
: ,eat10        ( a -- )        0 1 ,swallow ;  \ remove 1 instruction from old word
: ,eat11        ( a -- )        1 1 ,swallow ;  \ remove 1 instruction from each side

: ,drodu        ( a -- )                        \ drop dup --> copy (s) to tos
                showme
        if      cr ." Removing 1 word from previous, 1 from new, "
                cr ." Adding MOVE.L (S),TOS to replace DROP DUP "
        then
                tahere 2 - tahere!              \ strip some code from previous word
                dup ta@ 6000 =                  \ if new word doesn't start with 6000
                if 4 + then 2 +                 \ skip beginning code of new word
                [ (s) tos >movel ] literal aw, ofl  \ lay equivalent of DROPDUP
                ,macro ;                        \ then the rest of the code

\ optim actions:        in                      out
\               1       MOVE TOS,-(S) (dup)     MOVE TOS,-(S) (dup)
\               2       MOVE (S)+,TOS (drop)    MOVE (S)+,TOS (drop)
\               3       TST TOS                 CCR describes TOS status
\               4       MOVE TOS,A0             MOVE A0,TOS
\               5       MOVE TOS,D0             MOVE D0,TOS

: modlast       ( newdata mask -- )     \ modify last instruction
                tahere 2 - dup tahere!          \ back up
                ta@ and or aw, ;                \ modify

: ,desa0        ( -- )  0040 F03F modlast ,eat01 ;  \ change last destination to A0
: ,desd0        ( -- )  0000 F03F modlast ,eat01 ;  \ change last destination to D0


\ inline coding 3:3  prev-out:new-in

create inlines  ' ,macro ,  ' ,macro ,  ' ,macro ,  ' ,macro ,  \ 00 01 02 03
                ' ,macro ,  ' ,macro ,  ' ,macro ,  ' ,macro ,  \ 04 05 06 07
                ' ,macro ,  ' ,macro ,  ' ,eat11 ,  ' ,eat01 ,  \ 10 11 12 13
                ' ,macro ,  ' ,macro ,  ' ,macro ,  ' ,macro ,  \ 14 15 16 17
                ' ,macro ,  ' ,drodu ,  ' ,macro ,  ' ,eat01 ,  \ 20 21 22 23
                ' ,desa0 ,  ' ,desd0 ,  ' ,macro ,  ' ,macro ,  \ 24 25 26 27
                ' ,macro ,  ' ,macro ,  ' ,macro ,  ' ,eat01 ,  \ 30 31 32 33
                ' ,macro ,  ' ,macro ,  ' ,macro ,  ' ,macro ,  \ 34 35 36 37
                ' ,macro ,  ' ,macro ,  ' ,macro ,  ' ,macro ,  \ 40 41 42 43
                ' ,eat11 ,  ' ,desd0 ,  ' ,macro ,  ' ,macro ,  \ 44 45 46 47
                ' ,macro ,  ' ,macro ,  ' ,macro ,  ' ,eat01 ,  \ 50 51 52 53
                ' ,desa0 ,  ' ,eat11 ,  ' ,macro ,  ' ,macro ,  \ 54 55 56 57
                ' ,macro ,  ' ,macro ,  ' ,macro ,  ' ,macro ,  \ 60 61 62 63
                ' ,macro ,  ' ,macro ,  ' ,macro ,  ' ,macro ,  \ 64 65 66 67
                ' ,macro ,  ' ,macro ,  ' ,macro ,  ' ,macro ,  \ 70 71 72 73
                ' ,macro ,  ' ,macro ,  ' ,macro ,  ' ,macro ,  \ 74 75 76 77

: ,inline       ( asrc -- )
                optim@ 10 /mod          ( asrc out-action in-action )
                omode @ rot omode !     ( asrc in-action prev-out )
                showme
        if      cr ." Inlining actions: prev-out new-in = " 2dup . .
        then    3 lshift +              \ oldout|newin
                dup 0x40 <
        if      cells inlines + perform
        else    showme
                if      ." (invalid action) "
                then
                drop ,macro             \ invalid old|new combination
        then    ;

0 value lastheader

:noname         ( -- )
\ header is 16 bits: 1st byte is status bits, 2nd byte is optimization data
                tahere to lastheader    \ point to this header
                0 ic, 0 ic, ;           \ lay down a header
                is newheader

' lastheader    is 'newheader

' ,lit    is compilelit          \ defered words are in the FORTH wordlist
' ,call   is compilecall
' ,exit   is compileexit
' ,inline is compilemacro

:noname   ( -- addr )                   \ compile BRA 0, save pointer to offset
          0x6000 aw, tahere 0 aw, ofl ;         is >MARK

:noname   ( -- addr )                   \ compile BGE 0, save pointer to offset
          ,testtos
          0x6C00 aw, tahere 0 aw, ofl ;         is >-MARK

:noname   ( -- addr )                   \ compile conditional forward branch
          ,testflag  0x6700 aw, tahere 0 aw, ;  is >0MARK

:noname   ( -- addr )
          5397 aw,                      \ subq.l #1,(a7)
          6B00 aw, tahere 0 aw,         \ bmi
          ofl ;                                 is >MMARK

:noname   ( addr -- )                   \ resolve forward branch
          >r tahere r@ - r> ta! ofl ;           is >RESOLVE

:noname   ( -- addr )                   \ save destination of backward branch
          tahere ofl ;                          is <MARK

:noname   ( addr -- )
          tahere - 2 -                  \ 16-bit backward branch
          0x6000 aw, aw, ofl ;                  is <RESOLVE

:noname   ( addr -- )
          ,testflag
          tahere - 2 - dup abs 0x7C <
          if    0xFF and 0x6700 or aw,  \  8-bit backward branch
          else  0x6700 aw, aw,          \ 16-bit backward branch
          then  ofl ;                           is <0RESOLVE

:noname   ( addr -- )
          tahere - 2 - dup abs 0x7C <
          if    0xFF and 0x6C00 or aw,  \  8-bit backward branch
          else  0x6C00 aw, aw,          \ 16-bit backward branch
          then  ofl ;                           is <-RESOLVE


:noname          ( n -- )
                localdepth @ swap - 1- 4 *
   ?dup if      ,opush [ 7 5 tos >movel ] literal aw, aw, \ move.l d16(sp),tos
        else    ,opush [ (r) tos >movel ] literal aw,     \ move.l (sp),tos
        then    ; is local-fetch

:noname          ( n -- )
                localdepth @ swap - 1- 4 *
   ?dup if      [ tos 7 5 >movel ] literal aw, aw, ,pop   \ move.l tos,d16(sp)
        else    [ tos (r) >movel ] literal aw, ,pop       \ move.l tos,(sp)
        then    ; is local-store

:noname         ( #locals #total -- )
                over -   ( #locs #extras )
   ?dup if      begin   ?dup            \ allot non-initialized storage
                while   dup 2 - 0max tuck - ( n' len )
                        4 * 7 and 9 lshift 518F or aw,  \ subq #n,sp
                repeat
        then
   ?dup if      [ tos -(r) >movel ] literal aw, 1-
                0 ?do [ (s)+ -(r) >movel ] literal aw, loop
                ,pop
                \ else    7000 or aw,      \ moveq #locals,d0
                \        [ (s)+ -(r) >movel ] literal aw, \ move.l (s)+,-(r)
                \        5380 aw,         \ subq #1,d0
                \        66FA aw,         \ bne -6
                \ then
        then    ; is local-begin

:noname         ( #total -- )
                dup 7 <                         \ de-allot local storage
        if      begin   ?dup
                while   dup 2 - 0max tuck - ( n' len )
                        4 * 7 and 9 lshift 508F or aw,  \ addq #n,sp
                repeat
        else    4 * [ 4 7 0 0 >movel ] literal aw, al,  \ move.l #data,d0
                0DFC0 aw,                               \ adda.l d0,sp
        then    ; is local-end


\ ----------------------------------------------------------------------------
\ Optimized words:
\ We can make simple words immediate words that compile machine code.  In this
\ application, this is okay since the builder won't try to run run-time code.
\ For example, ' DUP returns the runtime xt of the target's DUP and not the xt
\ of the smart DUP.

also builder (definitions) (previous)
\ words in this vocabulary must be IMMEDIATE

\ optimizing inlining words for the ROM builder
\ omodes (previous instructions) to optimize:
\ 1 = MOVE.L D7,-(A5)  = DUP or PUSH
\ 2 = MOVE.L (A5)+,D7  = DROP or POP
\ 3 = MOVEQ #n,D7      = LIT (short)
\ 4 = MOVE.L #n,D7     = LIT (long)

: +             ( -- )  ( T: a b -- a+b )
                waslit? if ,+lit else
                [ (s)+ myTOS 2 >movel B000 + ] literal aw, then ofl ; immediate

: -             ( -- )  ( T: a b -- a-b )
                waslit? if ,-lit else
                [ (s)+ myTOS 2 >movel 7000 + ] literal aw,
                [ myTOS 4480 + ] literal aw,                \ NEG.L tos
                then ofl ; immediate

: AND           ( -- )  ( T: a b -- c )
                waslit? if ,andlit else
                [ (s)+ myTOS 2 >movel A000 + ] literal aw, then ofl ; immediate

: OR            ( -- )  ( T: a b -- c )
                waslit? if ,orlit else
                [ (s)+ myTOS 2 >movel 6000 + ] literal aw, then ofl ; immediate

: PICK          ( -- )  ( T: ... n -- S[n] )            \ 0 pick = dup
                waslit? if ,picklit else
                [ E588 myTOS + ] literal aw,            \ lsl.l #2,tos
                [ mySP 6 tos >movel ] literal aw,       \ move.l 0(s,tos),tos
                [ myTOS 0C lshift 800 + ] literal aw,
                then ofl ;                                      immediate

: NEWTASK       ( user dstack rstack <name> -- ) ( -- tid )
\ Create a new task.
\ TCB structure: sp0 / rp0 / pc / sp / rp / status / link / userdata...
\ offsets:       -20  -16   -12   -8   -4     0        4      8
\ MEM USAGE: TCB USER DSTACK| RSTACK|
                dt.ramarray makeheader
                thisdata 5 cellsize * +
                dup t'litvalue ! compilelit compileexit
                >r + dup r> + dup>r                ( sp0 rp0 )
                thisdata + swap thisdata +         ( rp0 sp0 )
                ram dup i, over i, 0 i, cellsize - i, cellsize - i, 0 i,
                r> fillzeros ;

: USER          ( n <name> -- )
\ runtime: ( -- a )
                0 makeheader
                0C +                             \ offset to user data
                ,push
                [ 6 1 tos >movel ] literal aw,   \ move.l a6,tos
                [ 0680 mytos + ] literal aw, al, \ add.l #,tos
                compileexit ;

: }vector       ( a -- )
                >r assemcfa word-split
                r@ ta! r> 2 + ta! ;


\ Comparison test for TOS parsing:
\ [ 3 ] ?[ rot -rot ]? compiles to

: ?[    ( c -- )        \ compile comparison test
                0C80 aw, al,       \ cmpi.l D0,#c
                6600 >amark ; immediate

: ]?    ( -- )          \ end comparison test
                ,exit >aresolve ; immediate

: qcase:        ( c - )
                [ TOS 0 0 >movel ] literal aw,  \ move.l D0,TOS
                ,pop ; immediate



: IFSET         ( bit# -- | n -- n )
\ Compile a branch to test a bit in TOS. TOS won't be swallowed.
                ?COMP
\                bl word number drop
                dup 0x1F > abort" Bit# must be between 0 and 31."
                0x0800 MyTOS + aw, aw,      \ btst #n,tos
                0x6700 aw, tahere 0 aw, 2 ;  IMMEDIATE

: IFCLR         ( bit# -- | n -- n )
\ Compile a branch to test a bit in TOS. TOS won't be swallowed.
                ?COMP
\                bl word number drop
                dup 0x1F > abort" Bit# must be between 0 and 31."
                0x0800 MyTOS + aw, aw,      \ btst #n,tos
                0x6600 aw, tahere 0 aw, 2 ;  IMMEDIATE

\ Similar works like DUP 0x100 AND IF.  It's equivalent in typical Forth is:
\ : IFSET   postpone DUP  bl word number
\           1 begin over while 2* swap 1- swap repeat nip
\           postpone LITERAL postpone AND postpone IF ; IMMEDIATE

\ ----------------------------------------------------------------------------

definitions
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ \\\\\\\\\\                    ASSEMBLER                         \\\\\\\\\\
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

vocabulary addrmodes
vocabulary cregs
also cregs definitions  \ control registers for MOVEC
0002 constant CACR      0004 constant ACR0      0005 constant ACR1
0801 constant VBR       080F constant PC
0C00 constant ROMBAR    0C04 constant RAMBAR    0C0F constant MBAR
previous definitions

vocabulary specregs
also specregs definitions \ special registers for MOVE
42C0 constant CCR       40C0 constant SR        A9C0 constant MACSR,CCR
0D00 constant MASK      0900 constant MACSR     0100 constant ACC
previous definitions

variable opdata
2variable *opmode

: opmode=imm    ( -- n )
\ label data = word or long  return mode|reg
                opdata @
                0 7FFF between
                if      0700
                else    0701
                then    ;

: oplabel>pad   ( -- )
\ strip off everything after (, place at pad
                *opmode 2@ 2dup '(' scan nip - pad place ;

: pad>labelval  ( -- )
\ convert string at pad to a value, store it in opdata
\ precedence: assembly-label, number, code-address
                pad label>n  dup bogusvalue =           \ not assembly address?
        if      drop pad count number? nip 0=           \ try number
                if      drop pad labeladdr  dup -1 =    \ try address of word
                        if      pad count 1- s" @@" compare
                                if      ."  ??"
                                        true abort"  Unknown label/immediate data"
                                else    pad 3 chars + c@ '0' -
                                        dup 0 9 between
                                        0=   abort"  Unknown label"
                                        qasmlabs@ nip
                                then
                        then
                then
        then    opdata ! ;

: opmode        ( a-addr len -- mode|reg )
\ Convert to packed mode/reg.  Try various modes, exit when found.
                2dup *opmode 2!  opdata off
                dup 0= abort"  Operand field is missing"
                pad place
                get-order only addrmodes
                pad find  2>r
                set-order 2r>                           \ basic modes?
                if      execute exit                    \ yes, got it.
                then    char+ c@ '#' =                  \ immediate data?
                if      *opmode 2@ 1 /string pad place  \ strip #
                        pad>labelval 0704 exit
                then    *opmode 2@
                '(' scan pad place                      \ strip displacement
                get-order only addrmodes
                pad find  2>r
                set-order 2r>                           \ basic modes?
                if      execute dup 8 rshift
                case    02 of  07 and 500 +       7FFF  endof \ d16(An)
                        0A of  drop 0702          7FFF  endof \ d16(PC)
                        0B of  0F and 12<< 800 + opdata !
                               0703                 7F  endof \ d8(PC,XI)
                        06 of  0FF and 10 /mod swap
                               12<< 800 + opdata !
                               600 +                7F  endof \ d8(An,XI)
                        true abort"  Invalid addressing mode"
                endcase oplabel>pad
                        pad labeladdr  dup -1 =
                        if      drop pad count number? nip 0= \ try number
                                abort"  Invalid displacement"
                        else    tahere - 2 -             \ absolute -> PC rel
                        then    ( modreg disp )
                        over 2* 1+ over and opdata +!    ( modreg n )
                        abs <= abort"  Displacement too large"
                        exit
                then    drop
                oplabel>pad pad>labelval                 \ label is an address
                opdata @ abs 8000 u< if 0700 exit then   \ xxx.w
                0701 ;                                   \ xxx.l

: otest         ( -- )
                bl parse opmode byte-split ." MOD=" h. ." REG=" h.
                opdata @ ." DATA=" h. ;

create operand 40 chars allot
variable size                           \ size: 0=long, 1=word, 2=byte default=0

: _getop        ( -- )                  bl word count operand place size off ;
: getop         ( a -- opcode )         _getop @ ;

: ,scan         ( a n -- a' n' )
\ scan for a comma but ignore commas inside ( )
                2dup '(' scan nip  ?dup
                if      >r 2dup ')' scan nip >r   ( a n | n{ }n )
                        begin   ',' scan
                                dup 2r@ swap between
                                if      1 /string  dup 0=
                                else    true
                                then
                        until   2r> 2drop
                else    ',' scan           \ no ( to deal with at all
                then    ;


: op$           ( operand-number -- a-addr len )
\ extract a substring, comma is a delimiter, ignore commas inside ( )
                >r operand count 2dup r>        \ old new .
                begin   >r 2swap 2drop 2dup ,scan r@ if ',' skip then
                        r> 1-  dup 0<           \ old' new' .
                until   2drop nip over - ;

: expectimm     ( operand# -- n )
                op$ opmode 0704 <>  abort"  Expecting immediate data" opdata @ ;
: expectDn      ( operand# -- n )
                op$ opmode byte-split      abort"  Expecting D0..D7" ;
: expectAn      ( operand# -- n )
                op$ opmode byte-split 1 <> abort"  Expecting A0..A7" ;
: expectAD      ( operand# -- n )
                op$ opmode byte-split 8 * +
                dup 0F > abort"  Expecting D0..D7 or A0..A7" ;

: Dn-dest?      ( -- f )
                1 op$ opmode 8 < ;         \ is the format <ea>,Dn?

: appendEA      ( mod|reg -- )  \ lay down EA extension words
                case    0700 of opdata @ aw,            endof   \ xxx.w
                        0701 of opdata @ al,            endof   \ xxx.l
                        0704 of opdata @ size @
                                if aw, else al, then    endof   \ #data
                        0702 of opdata @ aw,            endof   \ d16(pc)
                        0703 of opdata @ aw,            endof   \ d8(pc,xi)
                        8 rshift
                        5 of    opdata @ aw,            endof   \ d16(An)
                        6 of    opdata @ aw,            endof   \ d8(An,xi)
                endcase ;

: eamask        ( mod|reg -- mask )
\ get mask bit# associated with this mode
                byte-split  dup 7 =
                if + 1+ else nip then  1 swap lshift ;

: finishEA      ( opcode operand# mask xdata -- n )
\ finalize opcode, get EA, lay down extra data if needed
                >r >r op$ opmode
                dup eamask r> and 0= abort"  Invalid Effective Address"
                dup byte-split                  ( opcd RM reg mode )
                3 lshift + rot + aw,            \ opcd format: xxxxxxxxxxMMMRRR
                r> dup -1 = if drop else aw, then \ lay down xdata
                appendEA ;

: loner:        ( opcode <name> -- )            \ single opcode, no operands
                create , does> @ aw, ;

: Ay:           ( opcode <name> -- )            \ A register operand
                create , does> getop 0 expectAn + aw, ;

: Dy:           ( opcode <name> -- )            \ D register operand
                create , does> getop 0 expectDn + aw, ;

: DxDy:         ( opcode <name> -- )            \ ....XXX......YYY  dy,dx
                create , does> getop 0 expectDn + 1 expectDn 9<< + aw, ;

: #imm16:       ( opcode <name> -- )            \ trapf, etc.
                create , does> getop aw, 0 expectimm aw, ;

: #imm32:       ( opcode <name> -- )            \ trapf, etc.
                create , does> getop aw, 0 expectimm al, ;

: #imm,d:       ( opcode <name> -- )            \ addi, etc.
                create , does> getop
                1 expectDn + aw, 0 expectimm al, ;

: #q,ea:        ( opcode <name> -- )            \ addq, etc.
                create , does> getop
                0 expectimm dup 1 8 between 0= abort"  Range must be 1..8"
                7 and 9<< + 1 03FF -1 finishEA ;

: ea,a:         ( opcode <name> -- )            \ adda, etc.
                create , , does> @+ >r getop
                1 expectAn 9<< + 0 r> -1 finishEA ;

: ea,aw:        ( opcode <name> -- )            \ adda, etc.
                create , , does> @+ >r getop
                1 size !                        \ size = word
                1 expectAn 9<< + 0 r> -1 finishEA ;

: ea,d:         ( opcode mask <name> -- )       \ cmp, etc.
                create , , does> @+ >r getop
                1 expectDn 9<< + 0 r> -1 finishEA ;

: ea<>d:        ( opcode <name> -- )            \ add, etc.
                create , does> getop  1 op$ opmode 8 <  \ ea,d?
                if      1 expectDn 9<< + 080 + 0 1FFF -1 finishEA
                else    0 expectDn 9<< + 180 + 1 03FC -1 finishEA
                then    ;

: ea:           ( opcode mask <name> -- )       \ tst.l, etc.
                create , , does> @+ >r getop 0 r> -1 finishEA ;

: eaw:          ( opcode mask <name> -- )       \ tst.w, etc.
                create , , does> @+ >r getop 1 size ! 0 r> -1 finishEA ;

: bitop:        ( opcode <name> -- )    \ ea not be Dn register   ( bitop.B )
                create , does> getop  operand char+ c@ '#' =
                if      800 + 1 03FC  0 expectimm finishEA         \ #,ea
                else    100 + 0 expectDn 9<< + 1 03FC -1 finishEA  \ d,ea
                then    ;

: bitopd:       ( opcode <name> -- )    \ ea must be Dn register  ( bitop.L )
                create , does> getop  operand char+ c@ '#' =
                if      800 + 1 0001  0 expectimm finishEA         \ #,ea
                else    100 + 0 expectDn 9<< + 1 0001 -1 finishEA  \ d,ea
                then    ;

: shift:        ( opcode <name> -- )
                create , does> getop  operand char+ c@ '#' =
                if      0 expectimm dup 1 8 between 0=
                        abort"  Shift count must be 1..8"
                        7 and 9<< + 1 expectDn +
                else    0 expectDn 9<< + 1 expectDn +  20 +
                then    aw, ;

: mull:         ( opcode1 opcode2 <name> -- )    \ mulu.l, etc.
                create , , does> @+ >r getop
                1 op$ opmode byte-split
                >r 10 /mod swap r>               ( opcd DH DL or 0 Dn )
                case    0E of   12<< r> + 400 + +  endof        \ 64-bit
                        00 of   nip 12<< r> +      endof        \ 32-bit
                        abort"  Expecting D0..D7 or Dn:Dm"
                endcase                          ( opcd xdata )
                >r 0 03D r> finishEA ;

: packlist      ( a-addr len -- mask )
\ register list format: An or Dn separated by nothing or /
                0 >r
                begin   dup 0>          ( a n . )
                while   over c@ upc
                        dup  'A' =
                        swap 'D' = or   ( a n f )
                        if      over c@ upc 'A' = 8 and >r      \ regtype
                                1 /string over c@ '0' -
                                dup 7 > abort"  Reg# must be 0..7" r> +
                                1 swap lshift r> or >r
                        else    over c@ '/' <>
                                abort"  Need format: XnXnXn or Xn/Xn/Xn"
                        then    1 /string
                repeat
                2drop r> ;

: findspecreg   ( operand# -- xt found? )
                op$ pad place
                get-order only specregs
                pad find  2>r
                set-order 2r> ;

: .bogus        ( -- )  true abort"  Invalid operand field" ;

: moveop        ( opcode -- )   \ move ea,ea
                0 op$ opmode    \ decide which destinations to allow
                case    0700 of 001F endof      \ xxx.w
                        0701 of 001F endof      \ xxx.l
                        0704 of 001F endof      \ #data
                        0702 of 003F endof      \ d16(pc)
                        0703 of 001F endof      \ d8(pc,xi)
                        8 rshift
                        5 of    003F endof      \ d16(An)
                        6 of    001F endof      \ d8(An,xi)
                        03FF swap               \ otherwise, all allowed
                endcase
                >r 1 op$ opmode
                dup eamask r> and 0= abort"  Invalid EA combination"
                dup byte-split                  ( opcd RM reg mode )
                rot >r  swap 8 * +              ( opcd dest | rm )
                6 lshift swap +                 \ format: xxxxRRRMMM000000
                opdata @ >r
                0 1FFF -1 finishEA              \ get source, lay instr.
                r> opdata !
                r> appendEA ;                   \ and destination extension

: macscale      ( -- n )
              0 2 op$ s" <<" if drop 1 then
                2 op$ s" <<" if drop 3 then ;

: alignYreg     ( opcode n -- opcode' )
                10 /mod >r 2 /mod 8 * + 6 lshift + ;    \ ....YYY..Y......

: getYreg       ( -- n )
                0 op$ opmode byte-split
                0F <> abort"  1st operand must be Xn.L or Xn.U" ;

: getXreg       ( -- n )
                1 op$ opmode byte-split
                0F <> abort"  1st operand must be Xn.L or Xn.U" ;

: mac:          ( opcode1 size <name> -- )      \ mac, etc.
\ operand: Ry,Rx,scale
                create , , does> @+ >r getop
                getYreg 10 /mod >r  alignYreg
                getXreg 10 /mod >r  + aw,       \ ....YYY..Y..XXXX
                macscale 4 * r> 2* r> + + 6 lshift
                r> + aw, ;

: macl:         ( opcode1 size <name> -- )      \ mac, etc.
\ operand: Ry,Rx,<scale>,ea<&>,Rw
                create , , does> @+ >r getop
                getYreg 10 /mod >r  alignYreg
                macscale if 3 else 2 then  03C          ( opcd pos mask )
                getXreg 10 /mod >r
                macscale if 4 else 3 then
                op$ opmode dup 0F > abort"  Rw must be Dn or An"
                12<< + macscale 9<< +           \ w+sf+x
                r> r> 2* + 6 lshift +           \ w+sf+uy+ux+x
                pluck op$ 1- chars + dup c@ '&' =
                if      ',' swap c! 40 +        \ +mam
                else    drop
                then    finishEA ;


: if:           create , does> @ >amark ;
: un:           create , does> @ <aresolve ;
: wh:           create , does> @ >amark 2swap ;

: asmdisp       ( -- disp )             \ displacement to assem address
                _getop 0 op$ opmode
                dup 0700 0702 between 0= abort" Unknown branch address"
                0702 =
                if      opdata @                   \ already a displacement
                else    opdata @ tahere - 2 -
                then    ;

\ Local label support for assembler ============================================

\ label names are LOCAL0 to LOCAL9 and LOCALA to LOCALF
\ label followed by semicolon sets the destination address
128 constant #localstack
create localstack #localstack 2* cells allot    \ list of branches to resolve
variable localsp                        \ 1st = address of offset  2nd = label#

create locallabels 36 cells allot       \ list of destinations

: .locallabel   ( n -- )
                ." LOCAL" h.
                ;
: addlocal      ( -- )
\ get local label# from input stream, add this resolvable branch to the list
                bl word count
                2dup upper
                2dup 1 chars - S" LOCAL" compare >r
                drop 5 chars + c@ 36 digit 0= r> or
                abort" Local labels must be LOCAL0..LOCALZ"
                tahere                 ( 'offset # )
                localsp @  dup #localstack = abort" too many local branches"
                2* cells localstack + 2!
                localsp incr
                ;
: makelocal     ( n <name> -- )
\ create a local marker, usage example "5 makelocal LOCAL5:"
                create  cells ,
                does>   @ locallabels + tahere swap ! ;

: branch:       ( n <name> -- )
\ create a branch
                create  ,
                does>   @ aw, addlocal 0 aw, ;

: resolvelocals ( -- )
\ resolve locals
        begin   localsp @
        while   localsp decr
                localstack localsp @ 2* th 2@   ( # addr )
                locallabels rot th @            ( 'addr dest )
                dup 0= abort" Unresolved local branch"
                over -                          ( addr offset )
                over ta@ + swap ta!             \ resolve branch
        repeat  ;


magiclit value myvector

: (c;)          ( f -- )
\ end a code definition
                ?condition resolvelocals previous
                magiclit myvector <>
                if myvector then
                magiclit to myvector ;

\ opcodes ---------------------------------------------------------------------
also assem definitions

: MACRO:        ( <name> -- )   assemmacro ;

: vector{       ( f -- a f )   \ leave address of state to resolve on stack
                ihere@ 2 + swap ;
(( state machine example:
code state2     vector{
                movea.l #0,a3           rts c;
code state1     movea.l #state2,a3      rts c;
                }vector state1
))


: CALL          ( <name> -- )   assemcfa ,call ofl ;    \ uses smallest call
: GOTO          ( <name> -- )
                assemcfa
                dup tahere - 2 -                    ( ta disp )
                dup abs 8000 u<
                if      nip                         ( disp )
                        dup abs 80 <
                        if      0FF and 6000 + aw,  \ BRA.S
                        else    6000 aw, aw,        \ BRA
                        then
                else    drop dup abs 8000 u<        ( ta . )
                        if      4EF8 aw, aw,        \ JSR xxx.W
                        else    4EF9 aw, al,        \ JSR xxx.L
                        then
                then
                ;

6000 branch: BRA
6100 branch: BSR

6400 branch: BCC        6300 branch: BLS
6500 branch: BCS        6D00 branch: BLT
6700 branch: BEQ        6B00 branch: BMI
6C00 branch: BGE        6600 branch: BNE
6E00 branch: BGT        6A00 branch: BPL
6200 branch: BHI        6800 branch: BVC
6F00 branch: BLE        6900 branch: BVS

: XBRA.S        ( <name> -- )   asmdisp
                dup abs 7F > abort"  Displacement too large"
                0FF and 6000 + aw, ;
: XBRA          ( <name> -- )   asmdisp
                dup abs 7FFF > abort"  Displacement too large"
                6000 aw, aw, ;
: XBSR.S        ( <name> -- )   asmdisp
                dup abs 7F > abort"  Displacement too large"
                0FF and 6100 + aw, ;
: XBSR          ( <name> -- )   asmdisp
                dup abs 7FFF > abort"  Displacement too large"
                6100 aw, aw, ;
: EOR.L         ( -- )  _getop 0 expectDn 9<< B180 +  1 3FD -1 finishEA ;
: LINK          ( -- )  _getop 0 expectAn 4E50 + aw, 1 expectimm aw, ;
: MOVE.B        ( -- )  _getop 2 size ! 1000 moveop ;

: MOVE.W        ( -- )  _getop
\ different kinds of moves: ea,ea ccr,d ea,ccr sr,d ea,sr
                1 size !
                0 findspecreg           \ specreg,???
                if      execute dup
                        case    42C0 of 1 expectDn + aw,        endof
                                40C0 of 1 expectDn + aw,        endof
                                .bogus
                        endcase exit
                then    drop
                1 findspecreg           \ ???,specreg
                if      execute dup
                        case    42C0 of 200 + 0 1001 -1 finishEA endof
                                40C0 of 600 + 0 1001 -1 finishEA endof
                                .bogus
                        endcase exit
                then    drop 3000 moveop ;

: MOVE.L        ( -- )  _getop
\ different kinds of moves:
\ ea,ea  acc,r macsr,r mask,n ea,acc macsr,ccr ea,macsr ea,mask
                0 findspecreg           \ specreg,???
                if      execute dup
                        case    A9C0 of aw,   ( macsr,ccr )      endof
                                42C0 of .bogus                   endof
                                40C0 of .bogus                   endof
                                1 expectDn + 0A08 + aw,
                        endcase exit
                then    drop
                1 findspecreg           \ ???,specreg
                if      execute dup
                        case    A9C0 of .bogus                   endof
                                42C0 of .bogus                   endof
                                40C0 of .bogus                   endof
                                0A00 + 0 1003 -1 finishEA
                        endcase exit
                then    drop 2000 moveop ;

: MOVEC         ( -- )  _getop
                4E7B aw, 0 expectAD 12<<        \ get RY
                1 op$ pad place
                get-order only cregs
                pad find  2>r
                set-order 2r>                   ( cfa f )
                0= abort"  Unknown control register"
                execute + aw, ;

: MOVEM.L       ( -- )  _getop
                operand count 1- chars + c@ ')' =       \ last char = )?
                if      48C0 1 24 -1 finishEA  0 op$ packlist aw,
                else    4CC0 0 24 -1 finishEA  1 op$ packlist aw,
                then    ;

: MOVEQ         ( -- )  _getop
                1 expectDn 9 lshift 7000 +
                0 expectimm dup -80 7F between 0=
                abort"  Value must be -128..127"  0FF and + aw, ;

: TRAP          ( -- )  _getop
                0 expectimm dup 0F > abort"  Trap# must be 0..15"
                4E40 + aw, ;

: WDEBUG.L      ( -- )  _getop FBC0 0 44 3 finishEA ;

  D000      ea<>d:  ADD.L
  D1C0 1FFF ea,a:   ADDA.L
  0680      #imm,d: ADDI.L
  5080      #q,ea:  ADDQ.L
  D180      DxDy:   ADDX.L
  C000      ea<>d:  AND.L
  0280      #imm,d: ANDI.L
  E180      shift:  ASL.L
  E080      shift:  ASR.L
  0040      bitop:  BCHG.B
  0080      bitop:  BCLR.B
  00C0      bitop:  BSET.B
  0000      bitop:  BTST.B
  0040      bitopd: BCHG.L
  0080      bitopd: BCLR.L
  00C0      bitopd: BSET.L
  0000      bitopd: BTST.L
  4200 03FF eaw:    CLR.B
  4280 03FF ea:     CLR.L
  4240 03FF eaw:    CLR.W
  B080 1FFF ea,d:   CMP.L
  B1C0 1FFF ea,a:   CMPA.L
  0C80      #imm,d: CMPI.L
  F4E8      Ay:     CPUSHL
  4C40 0800 mull:   DIVS.L
  81C0 1FFD ea,d:   DIVS.W
  4C40 0000 mull:   DIVU.L
  80C0 1FFD ea,d:   DIVU.W
  0A80      #imm,d: EORI.L
  48C0      Dy:     EXT.L
  4880      Dy:     EXT.W
  49C0      Dy:     EXTB.L
  4AC8      loner:  HALT
  4AFC      loner:  ILLEGAL
  4EC0 0FE4 ea:     JMP
  4E80 0FE4 ea:     JSR
  41C0 1FFF ea,a:   LEA.L
  E188      shift:  LSL.L
  E088      shift:  LSR.L
  A000 0800 mac:    MAC.L
  A000 0000 mac:    MAC.W
  A800 0800 mac:    MACL.L
  A800 0000 mac:    MACL.W
  2040 1FFF ea,a:   MOVEA.L
  3040 1FFF ea,aw:  MOVEA.W
  A000 0900 mac:    MSAC.L
  A000 0100 mac:    MSAC.W
  A800 0900 mac:    MSACL.L
  A800 0100 mac:    MSACL.W
  4C00 0800 mull:   MULS.L
  C1C0 1FFD ea,d:   MULS.W
  4C00 0000 mull:   MULU.L
  C0C0 1FFD ea,d:   MULU.W
  4480      Dy:     NEG.L
  4080      Dy:     NEGX.L
  4E71      loner:  NOP
  4680      Dy:     NOT.L
  8000      ea<>d:  OR.L
  0080      #imm,d: ORI.L
  4840 0FE4 ea:     PEA.L
  4BCC      loner:  PULSE
  4E73      loner:  RTE
  4E75      loner:  RTS
  54C0      Dy:     SCC
  55C0      Dy:     SCS
  57C0      Dy:     SEQ
  51C0      Dy:     SF
  5CC0      Dy:     SGE
  5EC0      Dy:     SGT
  52C0      Dy:     SHI
  5FC0      Dy:     SLE
  53C0      Dy:     SLS
  5DC0      Dy:     SLT
  5BC0      Dy:     SMI
  56C0      Dy:     SNE
  5AC0      Dy:     SPL
  50C0      Dy:     ST
  4E72      #imm16: STOP
  9000      ea<>d:  SUB.L
  91C0 1FFF ea,a:   SUBA.L
  0480      #imm,d: SUBI.L
  5180      #q,ea:  SUBQ.L
  9180      DxDy:   SUBX.L
  58C0      Dy:     SVC
  59C0      Dy:     SVS
  4840      Dy:     SWAP.W
  51FC      loner:  TRAPF
  51FB      #imm32: TRAPF.L
  51FA      #imm16: TRAPF.W
  4A00 1FFF eaw:    TST.B
  4A80 1FFF ea:     TST.L
  4A40 1FFF eaw:    TST.W
  4E58      Ay:     UNLK
  FBC0 03FF eaw:    WDDATA.B
  FBE0 03FF eaw:    WDDATA.L
  FBD0 03FF eaw:    WDDATA.W

  6200 if: IF_LS  6300 if: IF_HI  6400 if: IF_CS  6500 if: IF_CC
  6600 if: IF_EQ  6700 if: IF_NE  6800 if: IF_VS  6900 if: IF_VC
  6A00 if: IF_MI  6B00 if: IF_PL  6C00 if: IF_LT  6D00 if: IF_GE
  6E00 if: IF_LE  6F00 if: IF_GT        \ 6700 if: IF
  6000 if: NEVER                        \ branch past code

  6200 wh: WHILE_LS  6300 wh: WHILE_HI  6400 wh: WHILE_CS  6500 wh: WHILE_CC
  6600 wh: WHILE_EQ  6700 wh: WHILE_NE  6800 wh: WHILE_VS  6900 wh: WHILE_VC
  6A00 wh: WHILE_MI  6B00 wh: WHILE_PL  6C00 wh: WHILE_LT  6D00 wh: WHILE_GE
  6E00 wh: WHILE_LE  6F00 wh: WHILE_GT  \ 6700 wh: WHILE

  6200 un: UNTIL_LS  6300 un: UNTIL_HI  6400 un: UNTIL_CS  6500 un: UNTIL_CC
  6600 un: UNTIL_EQ  6700 un: UNTIL_NE  6800 un: UNTIL_VS  6900 un: UNTIL_VC
  6A00 un: UNTIL_MI  6B00 un: UNTIL_PL  6C00 un: UNTIL_LT  6D00 un: UNTIL_GE
  6E00 un: UNTIL_LE  6F00 un: UNTIL_GT  \ 6700 un: UNTIL

: begin         ( - a f )       <amark ;
: again         ( a f - )       6000 <aresolve ;
: then          ( a f - )       >aresolve ;
: else          ( a f - a f )   6000 >amark 2swap then ;
: repeat        ( a f a f - )   again then ;
: continue      (  a f a f - a f ) 2over repeat ;

\ No for/next: coldfire doesn't have a DBcc instruction

: end-code      ( f -- )        (c;) ;
: c;            ( f -- )        (c;) ;
: ]c            ( f -- )        -1 bstate ! (c;) ;
: code          ( -- f )
                previous
                true abort"  Previous definition missing end-code" ;

00 makelocal LOCAL0:     01 makelocal LOCAL1:
02 makelocal LOCAL2:     03 makelocal LOCAL3:
04 makelocal LOCAL4:     05 makelocal LOCAL5:
06 makelocal LOCAL6:     07 makelocal LOCAL7:
08 makelocal LOCAL8:     09 makelocal LOCAL9:
0A makelocal LOCALA:     0B makelocal LOCALB:
0C makelocal LOCALC:     0D makelocal LOCALD:
0E makelocal LOCALE:     0F makelocal LOCALF:
10 makelocal LOCALG:     11 makelocal LOCALH:
12 makelocal LOCALI:     13 makelocal LOCALJ:
14 makelocal LOCALK:     15 makelocal LOCALL:
16 makelocal LOCALM:     17 makelocal LOCALN:
18 makelocal LOCALO:     19 makelocal LOCALP:
1A makelocal LOCALQ:     1B makelocal LOCALR:
1C makelocal LOCALS:     1D makelocal LOCALT:
1E makelocal LOCALU:     1F makelocal LOCALV:
20 makelocal LOCALW:     21 makelocal LOCALX:
22 makelocal LOCALY:     23 makelocal LOCALZ:

previous definitions

also addrmodes definitions      \ b31=0: mode|reg  b31=1: label
0000 constant D0        0100 constant A0
0001 constant D1        0101 constant A1
0002 constant D2        0102 constant A2
0003 constant D3        0103 constant A3
0004 constant D4        0104 constant A4
0005 constant D5        0105 constant A5
0006 constant D6        0106 constant A6
0007 constant D7        0107 constant A7
myTOS           constant TOS
mySP   100 +    constant S      myBASE 100 +    constant TP
mySP   200 +    constant (S)    myBASE 200 +    constant (TP)
mySP   300 +    constant (S)+   myBASE 300 +    constant (TP)+
mySP   400 +    constant -(S)   myBASE 400 +    constant -(TP)
myADDR 100 +    constant A
myADDR 200 +    constant (A)
myADDR 300 +    constant (A)+
myADDR 400 +    constant -(A)

0A00 constant (PC)      0107 constant SP

0200 constant (A0)      0300 constant (A0)+     0400 constant -(A0)
0201 constant (A1)      0301 constant (A1)+     0401 constant -(A1)
0202 constant (A2)      0302 constant (A2)+     0402 constant -(A2)
0203 constant (A3)      0303 constant (A3)+     0403 constant -(A3)
0204 constant (A4)      0304 constant (A4)+     0404 constant -(A4)
0205 constant (A5)      0305 constant (A5)+     0405 constant -(A5)
0206 constant (A6)      0306 constant (A6)+     0406 constant -(A6)
0207 constant (A7)      0307 constant (A7)+     0407 constant -(A7)
0207 constant (SP)      0307 constant (SP)+     0407 constant -(SP)
0B00 constant (PC,D0)   0B08 constant (PC,A0)
0B01 constant (PC,D1)   0B09 constant (PC,A1)
0B02 constant (PC,D2)   0B0A constant (PC,A2)
0B03 constant (PC,D3)   0B0B constant (PC,A3)
0B04 constant (PC,D4)   0B0C constant (PC,A4)
0B05 constant (PC,D5)   0B0D constant (PC,A5)
0B06 constant (PC,D6)   0B0E constant (PC,A6)
0B07 constant (PC,D7)   0B0F constant (PC,A7)   0B0F constant (PC,SP)
0600 constant (A0,D0)   0601 constant (A0,D1)
0602 constant (A0,D2)   0603 constant (A0,D3)
0604 constant (A0,D4)   0605 constant (A0,D5)
0606 constant (A0,D6)   0607 constant (A0,D7)
0608 constant (A0,A0)   0609 constant (A0,A1)
060A constant (A0,A2)   060B constant (A0,A3)
060C constant (A0,A4)   060D constant (A0,A5)
060E constant (A0,A6)   060F constant (A0,A7)   060F constant (A0,SP)
0610 constant (A1,D0)   0611 constant (A1,D1)
0612 constant (A1,D2)   0613 constant (A1,D3)
0614 constant (A1,D4)   0615 constant (A1,D5)
0616 constant (A1,D6)   0617 constant (A1,D7)
0618 constant (A1,A0)   0619 constant (A1,A1)
061A constant (A1,A2)   061B constant (A1,A3)
061C constant (A1,A4)   061D constant (A1,A5)
061E constant (A1,A6)   061F constant (A1,A7)   061F constant (A1,SP)
0620 constant (A2,D0)   0621 constant (A2,D1)
0622 constant (A2,D2)   0623 constant (A2,D3)
0624 constant (A2,D4)   0625 constant (A2,D5)
0626 constant (A2,D6)   0627 constant (A2,D7)
0628 constant (A2,A0)   0629 constant (A2,A1)
062A constant (A2,A2)   062B constant (A2,A3)
062C constant (A2,A4)   062D constant (A2,A5)
062E constant (A2,A6)   062F constant (A2,A7)   062F constant (A2,SP)
0630 constant (A3,D0)   0631 constant (A3,D1)
0632 constant (A3,D2)   0633 constant (A3,D3)
0634 constant (A3,D4)   0635 constant (A3,D5)
0636 constant (A3,D6)   0637 constant (A3,D7)
0638 constant (A3,A0)   0639 constant (A3,A1)
063A constant (A3,A2)   063B constant (A3,A3)
063C constant (A3,A4)   063D constant (A3,A5)
063E constant (A3,A6)   063F constant (A3,A7)   063F constant (A3,SP)
0640 constant (A4,D0)   0641 constant (A4,D1)
0642 constant (A4,D2)   0643 constant (A4,D3)
0644 constant (A4,D4)   0645 constant (A4,D5)
0646 constant (A4,D6)   0647 constant (A4,D7)
0648 constant (A4,A0)   0649 constant (A4,A1)
064A constant (A4,A2)   064B constant (A4,A3)
064C constant (A4,A4)   064D constant (A4,A5)
064E constant (A4,A6)   064F constant (A4,A7)   064F constant (A4,SP)
0650 constant (A5,D0)   0651 constant (A5,D1)
0652 constant (A5,D2)   0653 constant (A5,D3)
0654 constant (A5,D4)   0655 constant (A5,D5)
0656 constant (A5,D6)   0657 constant (A5,D7)
0658 constant (A5,A0)   0659 constant (A5,A1)
065A constant (A5,A2)   065B constant (A5,A3)
065C constant (A5,A4)   065D constant (A5,A5)
065E constant (A5,A6)   065F constant (A5,A7)   065F constant (A5,SP)
0660 constant (A6,D0)   0661 constant (A6,D1)
0662 constant (A6,D2)   0663 constant (A6,D3)
0664 constant (A6,D4)   0665 constant (A6,D5)
0666 constant (A6,D6)   0667 constant (A6,D7)
0668 constant (A6,A0)   0669 constant (A6,A1)
066A constant (A6,A2)   066B constant (A6,A3)
066C constant (A6,A4)   066D constant (A6,A5)
066E constant (A6,A6)   066F constant (A6,A7)   066F constant (A6,SP)
0670 constant (A7,D0)   0671 constant (A7,D1)
0672 constant (A7,D2)   0673 constant (A7,D3)
0674 constant (A7,D4)   0675 constant (A7,D5)
0676 constant (A7,D6)   0677 constant (A7,D7)
0678 constant (A7,A0)   0679 constant (A7,A1)
067A constant (A7,A2)   067B constant (A7,A3)
067C constant (A7,A4)   067D constant (A7,A5)
067E constant (A7,A6)   067F constant (A7,A7)   067F constant (A7,SP)
mySP 10 * 600 +
dup constant (S,D0)  1+ dup constant (S,D1) 1+
dup constant (S,D2)  1+ dup constant (S,D3) 1+
dup constant (S,D4)  1+ dup constant (S,D5) 1+
dup constant (S,D6)  1+ dup constant (S,D7) 1+
dup constant (S,A0)  1+ dup constant (S,A1) 1+
dup constant (S,A2)  1+ dup constant (S,A3) 1+
dup constant (S,A4)  1+ dup constant (S,A5) 1+
dup constant (S,A6)  1+ dup constant (S,A7)     constant (S,SP)
mySP 10 * 600 + myTOS +   constant (S,TOS)
myTOS 600 +
dup constant (A0,TOS) 10 + dup constant (A1,TOS) 10 +
dup constant (A2,TOS) 10 + dup constant (A3,TOS) 10 +
dup constant (A4,TOS) 10 + dup constant (A5,TOS) 10 +
dup constant (A6,TOS) 10 + dup constant (A7,TOS) constant (SP,TOS)

0F00 constant D0.L      0F08 constant A0.L
0F01 constant D1.L      0F09 constant A1.L
0F02 constant D2.L      0F0A constant A2.L
0F03 constant D3.L      0F0B constant A3.L
0F04 constant D4.L      0F0C constant A4.L
0F05 constant D5.L      0F0D constant A5.L
0F06 constant D6.L      0F0E constant A6.L
0F07 constant D7.L      0F0F constant A7.L
0F10 constant D0.U      0F18 constant A0.U
0F11 constant D1.U      0F19 constant A1.U
0F12 constant D2.U      0F1A constant A2.U
0F13 constant D3.U      0F1B constant A3.U
0F14 constant D4.U      0F1C constant A4.U
0F15 constant D5.U      0F1D constant A5.U
0F16 constant D6.U      0F1E constant A6.U
0F17 constant D7.U      0F1F constant A7.U
0E01 constant D0:D1  0E10 constant D1:D0  0E02 constant D0:D2  0E20 constant D2:D0
0E03 constant D0:D3  0E30 constant D3:D0  0E04 constant D0:D4  0E40 constant D4:D0
0E05 constant D0:D5  0E50 constant D5:D0  0E06 constant D0:D6  0E60 constant D6:D0
0E07 constant D0:D7  0E70 constant D7:D0
0E12 constant D1:D2  0E21 constant D2:D1  0E13 constant D1:D3  0E31 constant D3:D1
0E14 constant D1:D4  0E41 constant D4:D1  0E15 constant D1:D5  0E51 constant D5:D1
0E16 constant D1:D6  0E61 constant D6:D1  0E17 constant D1:D7  0E71 constant D7:D1
0E23 constant D2:D3  0E32 constant D3:D2  0E24 constant D2:D4  0E42 constant D4:D2
0E25 constant D2:D5  0E52 constant D5:D2  0E26 constant D2:D6  0E62 constant D6:D2
0E27 constant D2:D7  0E72 constant D7:D2
0E34 constant D3:D4  0E43 constant D4:D3  0E35 constant D3:D5  0E53 constant D5:D3
0E36 constant D3:D6  0E63 constant D6:D3  0E37 constant D3:D7  0E73 constant D7:D3
0E45 constant D4:D5  0E54 constant D5:D4  0E46 constant D4:D6  0E64 constant D6:D4
0E47 constant D4:D7  0E74 constant D7:D4  0E56 constant D5:D6  0E65 constant D6:D5
0E57 constant D5:D7  0E75 constant D7:D5  0E67 constant D6:D7  0E76 constant D7:D6
0E00 myTOS + constant D0:TOS            0E10 myTOS + constant D1:TOS
0E20 myTOS + constant D2:TOS            0E30 myTOS + constant D3:TOS
0E40 myTOS + constant D4:TOS            0E50 myTOS + constant D5:TOS
0E60 myTOS + constant D6:TOS            0E70 myTOS + constant D7:TOS
0E00 myTOS 10 * + constant TOS:D0       0E01 myTOS 10 * + constant TOS:D1
0E02 myTOS 10 * + constant TOS:D2       0E03 myTOS 10 * + constant TOS:D3
0E04 myTOS 10 * + constant TOS:D4       0E05 myTOS 10 * + constant TOS:D5
0E06 myTOS 10 * + constant TOS:D6       0E07 myTOS 10 * + constant TOS:D7


previous definitions

\ connect to Firmware Factory

true to bigendian?
' iw,           is aw,          \ lay down a 16-bit cell
' ihere@        is tahere       \ current target address
' ihere!        is tahere!      \ set new target address
' iw@           is ta@          \ fetch 16-bit word from target code
' iw!           is ta!          \ store 16-bit word to target code
' getlabel      is labeladdr ( $name -- a ) \ addr of code w/label

only forth also definitions

   3 to CPUtype                 \ assemble for Coldfire processor
   3 to CPUfamily               \ disassemble for Coldfire processor
   0 to CPUsubfamily
true to bigendian?              \ MSB is in low memory

hex
        000000 01FFFF rom-bounds   \ default memory allocation for ColdFire
        400000 40FFFF data-bounds
        410000 41FFFF code-bounds
decimal

homeorder


