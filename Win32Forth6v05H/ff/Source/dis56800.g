\ 56800 disassembler

anew 56L811_disassembler

include bradstuf.g

hex : >$ type ;
: >>    ( -- n )   5555 ;     \ next word from data stream

\ 4 classes of opcodes:

\ 00 = PDALU
\ 01 =
\ 10 =
\ 11 =

0 value opcode

: >emit         ( c -- )  pad c! pad 1 >$ ;
: .,            ( -- )  [char] , >emit ;
: .]            ( -- )  [char] ) >emit ;
: .[            ( -- )  [char] ( >emit ;
: .Y,           ( -- )  s" Y,"  >$ ;
: .]+           ( -- )  s" )+"  >$ ;
: .]+N          ( -- )  s" )+N" >$ ;
: .]-           ( -- )  s" )-"  >$ ;
: .RR           ( n -- )  3 and 0 <# # [char] R hold #> >$ ;
: .[RR]  .[ .RR .] ;    : .[RR]-  .[ .RR .]- ;
: .[RR]+ .[ .RR .]+ ;   : .[RR]+N .[ .RR .]+N ;
: .mRR          ( n -- )  dup 4 and if .[RR]+N else .[RR]+ then ;
: .2let         ( n a l -- )  drop swap 7 and 2* + 2 -trailing >$ ;
: .3let         ( n a l -- )  drop swap 7 and 3 * + 3 -trailing >$ ;
: .HHH          ( n -- )  s" X0Y0--Y1A B A1B1" .2let ;
: .JJJsrc       ( n -- )  s" ~FF F F X0Y0--Y1" .2let ;
: .FF1          ( n -- )  s" X0--Y0Y1" .2let ;
: .F            ( n -- )  1 and [char] A + >emit ;
: .QQQ          ( n -- )  7 and 5 * chars
s" (res)B1,Y1Y0,Y0A1,Y0Y0,X0Y1,X0(RES)Y1,Y0" drop + 5 >$ ;
: opcode89      ( -- n )  opcode 08 rshift 3 and ;
: opcode89A     ( -- n )  opcode 08 rshift 7 and ;
: opcodeBC      ( -- n )  opcode 0B rshift 3 and ;
: opcodeCD      ( -- n )  opcode 0C rshift 3 and ;
: opcode23      ( -- n )  opcode 02 rshift 3 and ;
: opcode45      ( -- n )  opcode 04 rshift 3 and ;
: opcode456     ( -- n )  opcode 04 rshift 7 and ;
: opcode56      ( -- n )  opcode 05 rshift 3 and ;

create opcodesA
                ," ADD TFR SUB CMP ADD     SUB     "
                ," DECWNEG RND TST     NOT         "
                ,"     ABS                         "
                ," INCWCLR ASL ASR         ROL ROR "
                ," ADD TFR SUB CMP OR      AND EOR "
                ," ADD TFR SUB CMP OR      AND EOR "
                ,"                                 "
                ," ADD TFR SUB CMP OR      AND EOR "
create opcodesB
                ," ADD     SUB CMP OR      AND EOR "
                ," DECWNEG?            NOT         "
                ," ADD     SUB CMP OR      AND EOR "
                ," INCW        ASR         ROL ROR "
                ," ADD     SUB CMP OR      AND EOR "
                ," ADD     SUB CMP OR      AND EOR "
                ,"                                 "
                ," ADD     SUB CMP OR      AND EOR "

create opcodesC ," MPY MAC MPYRMACR"
create opcodesD ," ADD MOVESUB --- "

: .opcd         ( n 'table -- )
\ display opcode based on n=0..63 and a table
                >r 8 /mod r> (string) dup
                if      char+  swap 4 * chars +
                        4 >$  s"  " >$
                else    2drop
                then    ;

: g.F           ( -- )  opcode 80 and 0<> .F ;
: g.FFF         ( -- )  opcode dup 3 and swap 6 rshift 4 and +
                        s" A X0--Y0B ()--Y1" .2let ;
: g.mRR         ( -- )  opcode .mRR ;
: g.HHH         ( -- )  opcode89A .HHH ;
: g.JJ          ( -- )  opcode89 .HHH ;


\ ========================================================================

\ PDALU (Parallel DALU) instructions
\ These instructions perform a memory read/write in parallel with
\ a data ALU operation.

\ 00WkkHHHFJJJ0mRR      non-multiply
\ 00WLLHHHFQQQ1mRR      multiply
\ --baabbbaaaa-bbb

\ DALU portion: kkFJJJ or LLFQQQ
\ F = A or B  JJJkk = opcode, JJJ also determines src  dest = F
\ F = A or B  L2:1=rnd L1:1=acc   QQQ = opcode, dest = F

\ MOVE portion: WHHHmRR
\ W: 0=R>M 1=M>R        HHH: x0 y0 -- y1 a b a1 b1
\ m: 0=(Rn)+ 1=(Rn)+N   RR: r0 r1 r2 r3

: pdalu         ( -- )
                opcodeBC                        \ 0kk
                opcode456 dup >r                \ JJJ
                3 lshift +  opcodesA .opcd      \ "OPCD"
                r> .JJJsrc ., g.f ;             \ "src,dest"

: pdalu-mul     ( -- )
                opcodeBC opcodesC .opcd         \ "OPCD"
                opcode 4 rshift .QQQ ., g.f ;

: opcode00      ( -- )
                opcode 8 and
                if      pdalu-mul       \ multiply PDALU
                else    pdalu           \ non-multiply PDALU
                then    s"   " >$
                opcode 2000 and
                if      g.mRR ., g.HHH  \ move mem to reg
                else    g.HHH ., g.mRR  \ move reg to mem
                then    ;

\ ========================================================================

\ 010... instructions

\ RDALU (reduced ALU) instructions used with dual read
\ 011UU0VVFmJJ0mrr      non-mult
\ 011LL0VVFmQQ1mrr      multiply

\ UUVVFJJ:  UU = ADD MOVE SUB ---   VV = dest  F=A/B
\ LLVVFQQ:  UU = MPY MAC MPYR MACR  QQ = src
\ mmrr: dual read operands from mmrr to VV

: .mmrr         ( -- )
\ display operands for dual parallel read
                opcode .mrr .,                                  \ src,
                opcode89 s" --Y0--Y1" .2let                     \ dest
                s"   " >$
                3 opcode 40 and if .[RR]- else .[RR]+ then .,   \ src,
                opcode89 s" --X0--X0" .2let ;                   \ dest

: rdalu         ( -- )
                opcodeBC opcodesD .opcd                  \ "OPCD"
                opcode45 s" X0Y0--Y1" .2let ., g.f       \ "src,dest"
                s"   " >$ .mmrr ;

: rdalu-mul     ( -- )
                opcodeBC opcodesC .opcd                  \ "OPCD"
                opcode45 4 + .QQQ ., g.f                 \ "src,src,dest"
                s"   " >$ .mmrr ;

: op01-a        ( -- )          \ 011KK1K0 FJJJ00FF
                opcode 7 rshift 4 and opcodeBC +   \ KKK
                opcode456 dup >r 3 lshift + dup >r \ JJJKKK
                opcodesA .OPCD                  \ "OPCD"
                r> r> over 1 3 between
                if      dup 1 = swap 5 = or
                        if      drop .Y, >$
                        else    .JJJsrc .,
                        then
                then    g.F ;                   \ operand(s)

: op01-b0       ( -- )
\ 011001wx Fxxx01xx
                opcode 200 and
                if      s" ADC   Y," >$ .f
                else    s" DIV   " >$ g.JJ ., g.F
                then    ;

: op01-b1       ( -- )
\ 011011CC FJJJ01Cw
                [char] Y >emit
                opcode89 opcode 2* 4 and +
                s" CCCSNEEQGELTGTLE" .2let s"   " >$    \ Tcc
                g.JJJ ,. g.f ;

: op01-b2       ( -- )
\ 011101xx Fxwx01xx
                opcode 20 and
                if      s" LSL   " >$ g.FFF
                else    s" SBC   Y," >$ g.f
                then    ;

: op01-b3       ( -- )
\ 011111wx Fxxx01xx
                opcode 200 and
                if      s" LSR   " >$ g.FFF
                else    s" NORM  R0," >$ g.F
                then    ;

create op01-bta
                ' op01-b0 , ' op01-b1 , ' op01-b2 , ' op01-b3 ,

: op01-b        ( -- )  op01-bta opcodeBC th @ execute ;
\ ___ba_       __
\ 01100100 F1JJ0100     DIV JJ,F
\ 01100110 F0000100     ADC  Y,F=A/B
\ 011011CC FJJJ01Cv
\ 01110110 F0000100     SBC  Y,F=A/B
\ 01110110 F01101FF     v=LSL  FFF=src/dest

\ 01111100 F0110100     NORM R0,F
\ 01111110 F01101FF     v=LSR  FFF=src/dest


: op01-c        ( -- )  s" opcode 011..1.. ....10.." >$ ;

: op01-dres     ( -- )  s" OP01 reserved" >$ ;

: .QQQ4         ( -- )  opcode 4 rshift .QQQ ., ;

: mpysu         ( -- )  s" MPYSU " >$ .QQQ4 g.FFF ;
: macsu         ( -- )  s" MACSU " >$ .QQQ4 g.FFF ;
: impy16        ( -- )  s" IMPY16 " >$ .QQQ4 g.FFF ;
: asrr          ( -- )  s" MACSU " >$ .QQQ4 g.FFF ;
: lsll          ( -- )  s" LSLL  " >$ .QQQ4 g.FFF ;
: lsrr          ( -- )  s" LSRR  " >$ .QQQ4 g.FFF ;
: lsrac         ( -- )  s" LSRAC " >$ .QQQ4 g.F ;
: asrac         ( -- )  s" ASRAC " >$ .QQQ4 g.F ;

create op01-dta
                ' op01-dres ,   ' asrr ,        \ 0 1
                ' impy16 ,      ' lsll ,        \ 2 3
                ' op01-dres ,   ' asrac ,       \ 4 5
                ' op01-dres ,   ' op-asrr ,     \ 6 7
                ' mpysu ,       ' lsrr ,        \ 8 9
                ' op01-dres ,   ' op-asrr ,     \ A B
                ' macsu ,       ' lsrac ,       \ C D
                ' op01-dres ,   ' op-asrr ,     \ E F

: op01-d        ( -- )
\ 011ii1ii FQQQ11FF   iiii is instruction
                opcodeBC 2 lshift  opcode89 + th @ execute ;



create op01-tbl
                ' op01-a , ' op01-b , ' op01-c , ' op01-d ,

: opcode01      ( -- )
                opcode 2000 and
                if      opcode 400 and
                        if      op01-tbl opcode23 th  \ 011- -1-- ---- aa--
                                @ execute
                        else    opcode 8 and
                                if      rdalu-mul     \ multiply RDALU
                                else    rdalu         \ non-multiply RDALU
                                then
                        then
                else    \ opcode010
                then    ;

\ ========================================================================

\ 10... instructions

: .XAA          ( n -- )        s" A0 B0 A2 B2 M01() () SP " .3let ;
: .N/R          ( n -- )        s" R0R1R2R3NDN --()" .2let ;
: .ggg          ( n -- )        s" OMR() () HWS() SR LC LA " .3let ;

create ddddd-tbl ' .HHH , ' .XAA , ' .N/R , ' .GGG ,
: .ddddd        ( n -- )        7 /mod ddddd-tbl swap th @ execute ;
: g.ddddd       ( -- )          opcode 1F and .ddddd ;

: .eeeee        ( n -- )
                dup 10 and >r  0F and           \ rearrange bits
                2 /mod 3 lshift or or .ddddd ;

: op1000.00     ( -- )
                opcode 80 /mod  dup 1C =
                if      s" TSTW  " >$ drop .ddddd
                else    s" MOVE  " >$ swap .ddddd ., .eeeee
                then    ;

: op1000.01     ( -- )
                opcode 7 rshift 1F and 1C =
                if      s" TSTW  (R" >$ opcode01 (.) >$ s" )-" >$
                else    s" Undefined 1000.01" >$
                then    ;

: op1000.10     ( -- )
                opcode9AB 3 =
                if      s" MOVE  #" >> 4 (h.) >$ ., g.ddddd
                else    s"
                then    ;

create op1000tbl ' op1000.00 , ' op1000.01 , ' op1000.10 , ' op1000.11 ,

: op1000        ( -- )  op1000tbl opcode56 th @ execute ;


create op10-tbl ' op1000 , ' op1001 , ' op1010 , ' op1011 ,

: opcode10      ( -- )  op10-tbl opcodeCD th @ execute ;





: z to opcode opcode00 ;


