((      work in progress
\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
BUILDER lexicon for M-Core target

Subroutine threaded, big endian

Cell size = 32 bits

r0 = RP         return stack pointer
r8 = SP         data stack pointer
r9 = TOS        top of data stack
r10 = TID       user pointer
r11..r14        scratchpad
r15             top of return stack
\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
))

only forth also definitions

 8 to addrnibbles       \ default to 32-bit addresses
-6 to jsrsize           \ size of long jump required by binding table
32 to cellbits          \ 32 bits per cell
 8 to charbits          \ 8 bits per character
 2 to calignment        \ instructions must be at even addresses
 4 to dalignment        \ data should be on longword boundaries
-1 to commonmem?        \ code space and data space overlap

vocabulary BLD-PRIVATEMC   BLD-PRIVATEMC definitions
\ Private part of ColdFire builder lexicon
false to sys-warning?
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

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ \\\\\\\\\\                    COMPILER                          \\\\\\\\\\
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

variable 'olast  \ -> start of last instruction
variable  omode  \ type of instruction left by optimizer
variable  ovalue \ value associated with omode, if any
variable  framed \ # of calls in a word
variable lastcall \ destination of last call

\ omode usage: last operation was a:
\ 0=unknown 1=push 2=pop

\ 10=short-lit 11=long-lit 12=BSR.S 13=BSR 14=JSR16 15=JSR32

: ,push         ( -- )      2438 aw, 9908 aw,  1 omode ! ;
: ,pop          ( -- )      8908 aw, 2038 aw,  2 omode ! ;
: ofl           ( -- )      omode off ;                 \ flush optimizer
: onip          ( n -- )    tahere swap - tahere! ofl ; \ drop last n bytes
: omark         ( -- )      tahere 'olast ! ;
: unmark        ( -- )      'olast @ tahere! ofl ;      \ drop last sequence
: waslit?       ( -- f )    omode @ 10 11 between ;     \ last was literal?
: ,ovalue       ( -- )      ovalue @ al, ofl ;
: ,opush        ( -- )      omode @ 2 = if 4 onip else ,push then ;
: ,opop         ( -- )      omode @ 1 = if 4 onip else ,pop then ;

: ,lit          ( n -- )
                dup ovalue !
                omark ,opush  dup -80 and
        if      7900 aw, al,            11      \ lrw r9,,0 (data)
        else    4 lshift 6009 or aw,    10      \ movi r9,#n
        then    omode ! ;                       \ 10 or 11 if push not optimized

\ program flow -------------------------------------------------------------

: ,testflag     ( -- )
                2A09 aw,                        \ cmpnei r9,#0
                ,pop ofl ;                      \ swallow flag

: ,testtos      ( -- )
                omode @ 3 <>                    \ status already known?
        if      2A09 aw, ofl                    \ TST.L TOS     ( no )
        then    ;

: ,jump         ( ta -- )
                dup tahere - 2 -                ( ta disp )
                dup abs 400 u<
        if      nip                     ( disp )
                F000 or aw,             \ bra
        else    drop                    ( ta )
                7000 aw, al,            \ jmpi
        then    ofl ;

: ,ejmp         ( offset -- )
                onip                            \ delete call instruction
                framed @ 1 =
        if      4 onip framed off               \ delete RP push if possible
        then    lastcall @
                ,jump ;

: ,call         ( ta -- )
                dup lastcall !
                framed @ 0=
        if      9F00 aw, 2030 aw,               \ push R15 to hardware stack
        then    dup tahere - 2 -                ( ta disp )
                dup abs 400 u<
        if      nip                     ( disp )
                F800 or aw,     12      \ bsr
        else    drop                    ( ta )
                7F00 aw, al,    13      \ jsri
        then    omode !
                framed incr ;

: ,exit         ( -- )
                omode @
                call-only? 0= and
        case    12 of    2 ,ejmp   endof
                13 of    8 ,ejmp   endof
        endcase
                framed @
        if      2430 aw, 8F00 aw,               \ un-stack R15
        then    00CF aw,                        \ return = jmp r15
                framed off ofl ;

: ,macro        ( a -- )
                100 swap            ( maxcount a )
                dup ta@ F800 and F000 = if 2 + then  \ macro can't start with BR
                begin   dup ta@  dup 00CF =     ( maxcnt a instr . )
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
                4 * tahere swap - tahere!       \ strip some code from previous word
                over ta@ F800 and F000 =        \ if new word doesn't start with 6000
                if swap 2 + swap then 4 * +     \ skip beginning code of new word
                ,macro ;

: ,eat01        ( a -- )        1 0 ,swallow ;  \ remove 1 instruction from new word
: ,eat10        ( a -- )        0 1 ,swallow ;  \ remove 1 instruction from old word
: ,eat11        ( a -- )        1 1 ,swallow ;  \ remove 1 instruction from each side

: ,drodu        ( a -- )                        \ drop dup --> copy (s) to tos
                showme
        if      cr ." Replacing DROP DUP pair"
        then
                tahere 4 - tahere!              \ strip some code from previous word
                dup ta@ F800 and F000 =         \ if new word doesn't start with BR
                if 2 + then 4 +                 \ skip beginning code of new word
                8201 aw, ofl                    \ lay equivalent of DROPDUP
                ,macro ;                        \ then the rest of the code

\ optim actions:        in                      out
\               1       MOVE TOS,-(S) (dup)     MOVE TOS,-(S) (dup)
\               2       MOVE (S)+,TOS (drop)    MOVE (S)+,TOS (drop)

\ inline coding 2:2  prev-out:new-in

create inlines  ' ,macro ,  ' ,macro ,  ' ,macro ,  ' ,macro ,  \ 00 01 02 03
                ' ,macro ,  ' ,macro ,  ' ,eat11 ,  ' ,macro ,  \ 10 11 12 13
                ' ,macro ,  ' ,drodu ,  ' ,macro ,  ' ,macro ,  \ 20 21 22 23
                ' ,macro ,  ' ,macro ,  ' ,macro ,  ' ,macro ,  \ 30 31 32 33

: ,inline       ( asrc -- )
                optim@ 10 /mod          ( asrc out-action in-action )
                omode @ rot omode !     ( asrc in-action prev-out )
                showme
        if      cr ." Inlining actions: prev-out new-in = " 2dup . .
        then    2 lshift +              \ oldout|newin
                dup 0x10 <
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
          tahere F000 aw, ofl ;                 is >MARK

:noname   ( -- addr )                   \ compile BGE 0, save pointer to offset
          ,testtos
          tahere E800 aw, ofl ;                 is >-MARK

:noname   ( -- addr )                   \ compile conditional forward branch
          ,testflag tahere E800 aw, ;           is >0MARK

:noname   ( -- addr )
          018F aw,                      \ declt r15
          E000 aw,                      \ bt
          ofl ;                                 is >MMARK

:noname   ( addr -- )                   \ resolve forward branch
          >r tahere r@ - 2/ 7FF and r@ ta@ or r> ta! ofl ;   is >RESOLVE

:noname   ( -- addr )                   \ save destination of backward branch
          tahere ofl ;                          is <MARK

:noname   ( addr -- )
          tahere - 2 -                  \ 16-bit backward branch
          2/ 7FFF and F000 or aw, ofl ;         is <RESOLVE

:noname   ( addr -- )
          ,testflag
          tahere - 2 -
          2/ 7FFF and F000 or aw, ofl ;         is <0RESOLVE

:noname   ( addr -- )
          ,testflag
          tahere - 2 -
          2/ 7FFF and F000 or aw, ofl ;         is <0RESOLVE


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
                cellsize 2* 0C + +               \ offset to user data
                ,push 1232 aw,                   \ TP --> TOS
                1- 4 lshift 2002 + aw,           \ TOS = TOS - #
                compileexit ;


\ ----------------------------------------------------------------------------

definitions
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ \\\\\\\\\\                    ASSEMBLER                         \\\\\\\\\\
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

vocabulary addrmodes

2variable opstring                              \ addr, len of substring
create lpad 32 allot

: findop        ( a n -- reg# )
                lpad place
                get-order only addrmodes
                lpad find  2>r
                set-order       2r>
             0= abort" Invalid operand(s)"
                execute ;

: findop0       ( -- reg# )                     \ RX
                bl word count findop ;

: findop01      ( -- rx ry )                    \ RX,RY
                ',' word count findop
                bl  word count bl skip findop ;

: findop0i      ( min max -- rx #imm )          \ RX,#IMM
                2>r
                ',' word count findop
                bl  word count bl skip
                over c@ '#' <> >r               \ must start with '#'
                '#' skip number? 0= r> or
                abort" expecting immediate data #n"
                drop dup
                2r> between 0=
                abort" immediate data out of range" 
                ;

: ?rn           ( n -- n )      dup 0F > abort" expecting R0..R15" ;

: zz:           ( opcode <name> -- )            \ single opcode, no operands
                create , does> @ aw, ;

: rx:           ( opcode <name> -- )            \ single operand
                create , does> @ findop0 ?rn + aw, ;

: xy:           ( opcode <name> -- )            \ 2-register operand
                create , does> @ findop01 ?rn 4 lshift swap ?rn + + aw, ;

: xc:           ( opcode <name> -- )            \ 2-register operand
                create , does> @ findop01  dup 100 11F between 0=
                abort" expecting CR0 .. CR31" 01F and
                4 lshift swap ?rn + + aw, ;

: xi:           ( opcode <name> -- )            \ register,immediate operand
                create , does> @
                0 1F findop0i  4 lshift swap ?rn + + aw, ;

: xj:           ( opcode <name> -- )            \ register,immediate operand
                create , does> @ 
                1 20 findop0i 1- 4 lshift swap ?rn + + aw, ;

: ji:           ( opcode <name> -- )            \ long jump
                create , does> @ aw, al, ;

: getimm        ( a len maxvalue -- n )
\ convert string to immediate data
             >r over c@ '#' <> abort" missing '#'"
\             2dup type space \ |||
                1 /string number? nip 0=
                over r> u> or abort" invalid immediate data" ;

: ld:           ( opcode <name> -- )
                create , does> @
\        cr .s
                ',' word count findop ?rn       \ get rz
                8 lshift + >r
                bl word count '(' scan '(' skip
                2dup 1- chars + c@ ')' <> abort" expecting (rx,#disp)"
                1 chars -                       \ RX,#disp
\        cr 2dup type space \ |||
                over >r dup>r ',' scan          ( a' n' | opcd a n )
                r> over - r> swap
                findop ?rn
                r> + >r  '#' scan               ( a' n' | opcd' )
                0F getimm 4 lshift r> + aw,
\        cr .s cr
                ;



\ Local label support for assembler ============================================

\ label names are LOCAL0 to LOCAL9 and LOCALA to LOCALF
\ label followed by semicolon sets the destination address
80 constant #localstack
create localstack #localstack 2* cells allot    \ list of branches to resolve
variable localsp                        \ 1st = address of offset  2nd = label#

create locallabels 24 cells allot       \ list of destinations

: .locallabel   ( n -- )
                ." LOCAL" h.
                ;
: addlocal      ( branchtype -- )
\ get local label# from input stream, add this resolvable branch to the list
                >r
                bl word count
                2dup upper
                2dup 1 chars - S" LOCAL" compare >r
                drop 5 chars + c@ 24 digit 0= r> or
                abort" Local labels must be LOCAL0..LOCALZ"
                r> + tahere             ( #+type 'offset )
                localsp @  dup #localstack = abort" too many local branches"
                2* cells localstack + 2!
                localsp incr
                ;
: makelocal     ( n <name> -- )
\ create a local marker, usage example "5 makelocal LOCAL5:"
                create  cells ,
                does>   @ locallabels + tahere swap ! ;

: br:           ( n <name> -- )
\ create a branch
                create  ,
                does>   @ 0 addlocal aw, ;

: resolvelocals ( -- )
\ resolve local branches
        begin   localsp @
        while   localsp decr
                localstack localsp @ 2* th 2@   ( #+type addr )
                swap byte-split >r swap         \ r= type
                locallabels rot th @            ( 'addr dest )
                dup 0= abort" Unresolved local branch"
             r> case    0 of over -             ( addr offset )
                          2/ 07FF and  over ta@ + swap ta!  endof
                        1 of over -
                          2/ dup abs 0F > abort" LOOPT branch too big"
                          0F and       over ta@ + swap ta!  endof
                        true abort" bad resolve type"
                endcase
        repeat  ;


\ opcodes ---------------------------------------------------------------------
also assem definitions

: MACRO:        ( <name> -- )   assemmacro ;

: CALL          ( <name> -- )   assemcfa ,call ofl ;    \ uses smallest call
: GOTO          ( <name> -- )   assemcfa ,jump ofl ;    \ uses smallest jump

: BGENI         ( <rx,#imm> -- )
                0 1F findop0i  dup 7 <
        if      1 swap lshift 4 lshift 6000 +           \ 0..6 use movi
        else    1+ 1F and 4 lshift 3200 +               \ 7..31 use bgeni
        then    swap ?rn + aw, ;

: BMASKI        ( <rx,#imm> -- )
                0 1F findop0i  dup 7 <
        if      -1 swap lshift invert 4 lshift 6000 +   \ 0..6 use movi
        else    1+ 1F and 4 lshift 2C00 +               \ 7..31 use bmaski
        then    swap ?rn + aw, ;

: LOOPT         ( <local> -- )
                ',' word count findop ?rn  100 addlocal
                0400 + aw, ;

: MOVI          ( <RX,#IMM> -- )
                0 7F findop0i 4 lshift + 6000 + aw, ;

: LRW           ( <rz,#imm> -- )
                0 0FF findop0i       ( rx imm )
                swap 8 lshift + 7000 + aw, ;

: TRAP          ( <#0..3> -- )  findop dup 1000 1003 between 0=
                abort" Expecting #0 .. #3" 3 and 8 + aw, ;

01E0 rx: abs    0600 xy: addc   2000 xj: addi   1C00 xy: addu
1600 xy: and    2E00 xi: andi   1F00 xy: andn   1A00 xy: asr
3A00 rx: asrc   3A00 xi: asri   3000 xi: bclri  E800 br: bf
1300 xy: bgenr  0000 zz: bkpt   00F0 rx: brev   3400 xi: bseti
F800 br: bsr    F000 br: bt     3600 xi: btsti  01D0 rx: clrf
01C0 rx: clrt   0C00 xy: cmphs  0D00 xy: cmplt  2200 xj: cmplti
0F00 xy: cmpne  2A00 xi: cmpnei 0090 rx: decf   01A0 rx: decgt
0180 rx: declt  01B0 rx: decne  0080 rx: dect   3210 rx: divs
2C10 rx: divu   0006 zz: doze   00E0 rx: ff1    00B0 rx: incf
00A0 rx: inct   1D00 xy: ixh    1500 xy: ixw    00C0 rx: jmp
7800 ji: jmpi   00D0 rx: jsr    7F00 ji: jsri   8000 ld: ld
A000 ld: ld.b   C000 ld: ld.h   8000 ld: ld.w   0060 rx: ldm
0040 rx: ldq    1B00 xy: lsl    3C00 rx: lslc   3C00 xi: lsli
0B00 xy: lsr    3E00 rx: lsrc   3E00 xi: lsri   1000 xc: mfcr
1200 xy: mov    0A00 xy: movf   0200 xy: movt   1800 xc: mtcr
0300 xy: mult   0010 rx: mvc    0030 rx: mvcv   01F0 rx: not
1E00 xy: or     0003 zz: rfi    3800 xi: rotli  1400 xy: rsub
2800 xi: rsubi  0002 zz: rte    0150 rx: sextb  0170 rx: sexth
9000 ld: st     B000 ld: st.b   D000 ld: st.h   9000 ld: st.w
0070 rx: stm    0004 zz: stop   0050 rx: stq    0700 xy: subc
2400 xj: subi   0500 xy: subu   0001 zz: sync   0E00 xy: tst
0190 rx: tstnbz 0005 zz: wait   1700 xy: xor    3800 rx: xsr
0130 rx: xtrb0  0120 rx: xtrb1  0110 rx: xtrb2  0100 rx: xtrb3
0140 rx: zextb  0160 rx: zexth

: c;            ( f -- )        ?condition resolvelocals previous ;
: end-code      ( f -- )        c;
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

also addrmodes definitions
0000 constant R0    0001 constant R1    0002 constant R2    0003 constant R3
0004 constant R4    0005 constant R5    0006 constant R6    0007 constant R7
0008 constant R8    0009 constant R9    000A constant R10   000B constant R11
000C constant R12   000D constant R13   000E constant R14   000F constant R15
0000 constant R     0009 constant TOS   0008 constant S     000A constant TID
000B constant A     000E constant TMP

0100 constant CR0   0101 constant CR1   0102 constant CR2   0103 constant CR3
0104 constant CR4   0105 constant CR5   0106 constant CR6   0106 constant CR7
0108 constant CR8   0109 constant CR9   010A constant CR10  010B constant CR11
010C constant CR12  010D constant CR13  010E constant CR14  010F constant CR15
0110 constant CR16  0111 constant CR17  0112 constant CR18  0113 constant CR19
0114 constant CR20  0115 constant CR21  0116 constant CR22  0117 constant CR23
0118 constant CR24  0119 constant CR25  011A constant CR26  011B constant CR27
011C constant CR28  011D constant CR29  011E constant CR30  011F constant CR31

1000 constant #0    1001 constant #1    1002 constant #2    1003 constant #3

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

   5 to CPUtype                 \ assemble for m-core processor
   5 to CPUfamily               \ disassemble for m-core processor
   0 to CPUsubfamily
true to bigendian?              \ MSB is in low memory

hex
        000000 01FFFF rom-bounds   \ default memory allocation for m-core
        400000 40FFFF data-bounds
        410000 41FFFF code-bounds
decimal

homeorder


