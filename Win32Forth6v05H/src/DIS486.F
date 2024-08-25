\ 80386 Disassembler

\ rbs - changed vars and functions to hint at parameters used and what they do.
\ Also removed "ptr" from "byte ptr" and "word ptr" as it is unnecessary and
\ inconsistent with "dword" which did not append "ptr" January 5th, 2003

\ Andrew McKewan, April 1994
\ Tom Zimmer,  05/18/94 port to Win32f
\ Modified to word in decimal 08/03/94 10:04 tjz
\ 06-??-95 SMuB NEXT sequence defined in FKERNEL
\ 06-21-95 SMuB removed redundant COUNT calls from txb, lxs.
\ 04-??-97 Extended by C.L. to include P6 and MMX instructions

cr .( Loading 80486 Disassembler...)
cr .( -- BETA DIS486.F 4.9C -- ) 

ONLY FORTH ALSO DEFINITIONS

VOCABULARY DISASSEMBLER
DISASSEMBLER ALSO DEFINITIONS
DECIMAL

32 constant comment-col

\ Var/Values are all dis.<name> so it's easier to see that they aren't
\ functions.  Most display functions are preceded by DOT (for print).
\ Demultiplixing functions have a parameterlist as part of their name,
\ often signifying which bits are being used.

\ pseudo Object DIS for all the vars used by the disassembler
0 value dis.default-16bit?
0 value dis.base-addr
0 value dis.size
0 value dis.16-bit-data
0 value dis.16-bit-addr
0 value dis.prefix-op
0 value dis.mmx-reg?
0 value dis.mem?          \ for suppressing comma after memory address


\ %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\                     Disassembler utilities
\ %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

: default-16bit ( -- )
                true to dis.default-16bit? ;

: default-32bit ( -- )
                false to dis.default-16bit? ;

defer show-name   ( cfa -- )      \ display nearest symbol

: sext  ( byte -- n )  dup 0x80 and if 0xFFFFFF00 or then ;


: parse/sib ( ss-iii-bbb -- bbb iii ss ) \ r including general, special, segment, MMX
    255 and 8 /mod 8 /mod ;


: parse/ModR/M  ( mod-r-r/m -- 3bits=r/m 3bits=r/op 2bits=mod )
    \ r/op includes general, special, segment, MMX registers or exteded opcode
    255 and 8 /mod 8 /mod ;










\ %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\                           Display routines
\ %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


create s-buf MAXSTRING allot

: >s            ( a1 n1 -- )
                s-buf +place ;

: 0>s           ( -- )  \ reset s-buf
                s-buf off ;

: sspaces       ( n1 -- )
                spcs swap s-buf +place ;

: sspace        ( -- )
                1 sspaces ;

: emit>s        ( c1 -- )
                s-buf c+place ;

: s>            ( -- a1 n1 )
                s-buf count ;

: .s"
    postpone s"
    compile >s ; immediate


: d.r>s         ( d w -- )
                >r (d.) r> over - sspaces >s ;

: .r>s          ( n w -- )
                >r  s>d  r>  d.r>s ;

: u.r>s         ( u w -- )
                0 swap d.r>s ;

: h.>s          ( u -- )
                base @ swap  hex 0 (d.) >s sspace   base ! ;

: h.r>s           ( n1 n2 -- )
                base @ >r hex >r
                0 <# #s #> r> over - sspaces >s
                r> base ! ;

: .id>s         ( nfa -- )
                nfa-count >s sspace ;

: .name>s       ( xt -- )
                dup >name dup name> ['] [unknown] =     \ if not found
                if      drop [char] " emit>s 1 h.r>s [char] " emit>s sspace
                else    .id>s drop
                then    ;

: ?.name>s      { op-addr \ cfa -- }
\ rbs - changed display format to addr and comment "( <NAME> @ <offset> )"
                op-addr ?name to cfa
                cfa     \ got a name?
                if      op-addr h.>s .s" ( " cfa .name>s .s" : 0x" op-addr cfa - body> h.>s .s" ) "
                else    op-addr 1 h.r>s sspace
                then    ;

' ?.name>s is show-name


: ???   ( n1 -- )
        .s" ??? " drop ;

: ss. ( n adr len w )  >r drop  swap r@ * +  r> >s sspace ;


: .cnd-code(op) ( code -- )     \ was tttn
    15 and S" o nob aee nebea s nsp npl geleg " 2 ss. ;

: .sreg(XX-NNN-XXX)  ( sreg -- )  3 rshift 7 and S" escsssdsfsgsXXXX" 2 ss. ;
: .creg(XX-NNN-XXX)  ( eee --  )  3 rshift 7 and S" cr0???cr2cr3cr4?????????" 3 ss. ;
: .dreg(XX-NNN-XXX)  ( eee --  )  3 rshift 7 and S" dr0dr1dr2dr3??????dr6dr7" 3 ss. ;
: .treg(XX-NNN-XXX)  ( eee --  )  3 rshift 7 and S" ?????????tr3tr4tr5tr6tr7" 3 ss. ; \ obsolete
: .mreg(XX-XXX-NNN)  ( n -- )  7 and S" mm0mm1mm2mm3mm4mm5mm6mm7" 3 ss. ;

: .rel8  ( addr -- addr' )
        count sext over + show-name ;

: .rel16/32 ( addr -- addr' )
        dis.16-bit-addr
        IF      wCount
        ELSE    LCount
        THEN    over + dis.base-addr - show-name ;

: .reg8(XX-XXX-NNN)  ( n -- )  7 and S" alcldlblahchdhbh" 2 ss. ;
: .reg16(XX-XXX-NNN) ( n -- )  7 and S" axcxdxbxspbpsidi" 2 ss. ;
: .reg32(XX-XXX-NNN) ( n -- )  7 and S" eaxecxedxebxespebpesiedi" 3 ss. ;

: .reg16/32(XX-XXX-NNN)      ( n -- )
                dis.16-bit-data
                if   .reg16(XX-XXX-NNN)
                else .reg32(XX-XXX-NNN)
                then ;



: .reg(XX-XXX-NNN)   ( a n -- a )
        dis.mmx-reg?
        if   .mreg(XX-XXX-NNN)
        else dis.size
             if   .reg16/32(XX-XXX-NNN)
             else .reg8(XX-XXX-NNN)
             then
        then ;


: .[base16](XX-XXX-1NN) ( r/m -- )
    7 and 4 - S" [si][di][bp][bx]" 4 ss. ;
    \ r/m = 4 , 5 , 6 , 7


: .[ind16](XX-XXX-0NN)  ( r/m -- )
    3 and S" [bx+si][bx+di][bp+si][bp+di]" 7 ss. ;
    \ r/m = 0  ,   1  ,   2  ,   3


: .[reg16](XX-XXX-NNN)  ( r/m -- )
    7 and dup 4 <
    if    .[ind16](XX-XXX-0NN)
    else  .[base16](XX-XXX-1NN)
    then ;


: .[reg32](XX-XXX-NNN)  ( n -- )
    7 and S" [eax][ecx][edx][ebx][esp][ebp][esi][edi]" 5 ss. ;


: .[reg*2](XX-XXX-NNN)  ( i -- )
    7 and S" [eax*2][ecx*2][edx*2][ebx*2][XXX*2][ebp*2][esi*2][edi*2]" 7 ss. ;


: .[reg*4](XX-XXX-NNN)  ( i -- )
    7 and S" [eax*4][ecx*4][edx*4][ebx*4][XXX*4][ebp*4][esi*4][edi*4]" 7 ss. ;


: .[reg*8](XX-XXX-NNN)  ( i -- )
    7 and S" [eax*8][ecx*8][edx*8][ebx*8][XXX*8][ebp*8][esi*8][edi*8]" 7 ss. ;


: .disp8[pc++]  ( adr -- adr' )  true to dis.mem? count show-name ;
: .disp16[pc++] ( adr -- adr' )  true to dis.mem? wCount show-name ;
: .disp32[pc++] ( adr -- adr' ) true to dis.mem? LCount ( body> ) show-name ;


: .disp16/32[pc++] ( adr -- adr' )
            dis.16-bit-addr
            if   .disp16[pc++]
            else .disp32[pc++]
            then ;

: .,     ( -- )
    \ delete the space before the comma
    -1 s-buf c+!
    .s" , " ;


: .#
    dis.mem? not
    if
        .,
    then
    .s" # " ;


: .imm8[pc++]   ( adr -- adr' )  .# count h.>s ;


: .imm16/32[pc++]  ( adr -- adr' )
        .# dis.16-bit-data
        IF   wCount
        ELSE LCount
        THEN h.>s ;


: .[index]=SS-III-BBB  ( sib -- )   parse/sib ( b i s ) over 4 =    \ i=4?
                        if    2drop                     \ no esp scaled index
                        else  case ( s )
                                0 of .[reg32](XX-XXX-NNN) endof
                                1 of .[reg*2](XX-XXX-NNN) endof
                                2 of .[reg*4](XX-XXX-NNN) endof
                                3 of .[reg*8](XX-XXX-NNN) endof
                              endcase
                        then ( b ) drop ;


: .btx(XXXN-NXXX)  ( n -- )
        3 rshift
        3 and S" bt btsbtrbtc" 3 ss. 4 sspaces ;














\ %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\                       DeMultiplexing Mechanism
\ %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




: .sib=NN   ( adr mod -- adr )
        >r count tuck 7 and 5 = r@ 0= and
        if    .disp32[pc++] swap .[index]=SS-III-BBB r> drop       \ ebp base and mod = 00
        else  r> case ( mod )
                   1 of .disp8[pc++]  endof
                   2 of .disp32[pc++] endof
                 endcase
              swap dup .[reg32](XX-XXX-NNN) .[index]=SS-III-BBB
        then ;




: mod-r/m32(r/m,mod)     ( adr r/m mod -- adr' )
                dup 3 =
                if    drop .reg(XX-XXX-NNN)             \ mod = 3, register case
                else  over 4 =
                      if nip .sib=NN                    \ r/m = 4, sib case
                      else  2dup 0= swap 5 = and        \ mod = 0, r/m = 5,
                            if 2drop .disp32[pc++]      \ disp32 case
                            else rot swap
                                 case ( mod )
                                   1 of .disp8[pc++]  endof
                                   2 of .disp32[pc++] endof
                                 endcase
                                 swap .[reg32](XX-XXX-NNN)
                            then
                      then
                then ;


: mod-r/m16(r/m,mod)     ( adr r/m mod -- adr' )
    2dup 0= swap 6 = and
    if   2drop .disp16[pc++]            \ disp16 case
    else case ( mod )
           0 of .[reg16](XX-XXX-NNN)                            endof
           1 of swap .disp8[pc++]  swap .[reg16](XX-XXX-NNN)    endof
           2 of swap .disp16[pc++] swap .[reg16](XX-XXX-NNN)    endof
           3 of .reg(XX-XXX-NNN)                                endof
         endcase
    then ;


: mod-r/m(ModR/M) ( adr modr/m -- adr' )
    parse/ModR/M nip dis.16-bit-addr
    if    mod-r/m16(r/m,mod)
    else  mod-r/m32(r/m,mod)
    then ;


: r/m8(ModR/M)      0 to dis.size mod-r/m(ModR/M) ;
: r/m16/32(ModR/M)  1 to dis.size mod-r/m(ModR/M) ;
: r/m16(ModR/M)     true to dis.16-bit-data r/m16/32(ModR/M) ;

: r,r/m()  ( adr -- adr' )
    count dup 3 rshift ( op/reg->reg/m )
    .reg(XX-XXX-NNN) ., mod-r/m(ModR/M) ;

: r/m,r()  ( adr -- adr' )
    count dup >r mod-r/m(ModR/M)
    dis.mem?
    if false to dis.mem?
    else .,
    then r> 3 rshift .reg(XX-XXX-NNN) ;

: r/m()  ( adr op -- adr' )
    2 and if r,r/m() else r/m,r() then ;




\ -------------------- Simple Opcodes --------------------

: inh   ( -<name>- )
        create
        /parse-word count here place
        here c@ 1+ allot
        does> count >s sspace drop ;

inh clc  "clc"
inh stc  "stc"
inh cld  "cld"
inh std  "std"
\ inh rpnz "repnz"
\ inh repz "repz"
inh cwde  "cwde"
inh cdq  "cdq"
inh daa  "daa"
inh das  "das"
inh aaa  "aaa"
inh aas  "aas"
\ inh lock "lock"
inh inb  "insb"
inh osb  "outsb"
inh sah  "sahf"
inh lah  "lahf"
\ inh aam  "aam"
\ inh aad  "aad"
inh hlt  "hlt"
inh cmc  "cmc"
inh xlt  "xlat"
inh cli  "cli"
inh sti  "sti"

inh clt "clts"
inh inv "invd"
inh wiv "wbinvd"
inh ud2 "ud2"
inh wmr "wrmsr"
inh rtc "rdtsc"
inh rmr "rdmsr"
inh rpc "rdpmc"
inh ems "emms"
inh rsm "rsm"
inh cpu "cpuid"
inh ud1 "ud1"
\ inh lss "lss"
\ inh lfs "lfs"
\ inh lgs "lgs"

\ inh d16: "d16:"
\ inh a16: "a16:"
\ inh es:  "es:"
\ inh cs:  "cs:"
\ inh ds:  "ds:"
\ inh fs:  "fs:"
\ inh gs:  "gs:"

: aam   ( adr code -- adr' )
        .s" aam" drop count drop ;

: aad   ( adr code -- adr' )
        .s" aad" drop count drop ;

: d16   ( adr code -- adr' )
        drop .s" d16:"
        true to dis.16-bit-data
        true to dis.prefix-op
        ;

: a16   ( adr code -- adr' )
        drop .s" a16:"
        true to dis.16-bit-addr
        true to dis.prefix-op
        ;

: rpz   ( adr code -- adr' )
        drop .s" repnz"
        true to dis.prefix-op
        ;

: rep   ( adr code -- adr' )
        drop .s" repz"
        true to dis.prefix-op
        ;

: lok   ( adr code -- adr' )  \ This should have error checking added
        drop .s" lock"
        true to dis.prefix-op
        ;

: cs:   ( adr code -- adr' )
        drop .s" cs:"
        true to dis.prefix-op
        ;

: ds:   ( adr code -- adr' )
        drop .s" ds:"
        true to dis.prefix-op
        ;

: ss:   ( adr code -- adr' )
        drop .s" ss:"
        true to dis.prefix-op
        ;

: es:   ( adr code -- adr' )
        drop .s" es:"
        true to dis.prefix-op
        ;

: gs:   ( adr code -- adr' )
        drop .s" gs:"
        true to dis.prefix-op
        ;

: fs:   ( adr code -- adr' )
        drop .s" fs:"
        true to dis.prefix-op
        ;

: isd   ( adr code -- adr' )
        drop dis.16-bit-data
        IF      .s" insw    "
        ELSE    .s" insd    "
        THEN ;

: osd   ( adr code -- adr' )
        drop dis.16-bit-data
        IF      .s" outsw    "
        ELSE    .s" outsd    "
        THEN ;

: inp   ( addr code -- addr' )
        .s" in      " 1 and
        IF      dis.16-bit-data
                IF      .s" ax , "
                ELSE    .s" eax , "
                THEN
        ELSE    .s" al , "
        THEN
        count h.>s ;

: otp   ( addr code -- addr' )
        .s" out     " 1 and
        IF      count h.>s dis.16-bit-data
                IF      .s" , ax"
                ELSE    .s" , eax"
                THEN
        ELSE    count h.>s .s" , al"
        THEN
        ;

: ind   ( addr code -- addr' )
        .s" in      " 1 and
        IF      dis.16-bit-data
                IF      .s" ax , dx"
                ELSE    .s" eax , dx"
                THEN
        ELSE    .s" al , dx"
        THEN
        ;

: otd   ( addr code -- addr' )
        .s" out     " 1 and
        IF      dis.16-bit-data
                IF      .s" dx , ax"
                ELSE    .s" dx , eax"
                THEN
        ELSE    .s" dx , al"
        THEN
        ;

\ -------------------- ALU Opcodes --------------------

: .alu  ( n -- )
        7 and S" addor adcsbbandsubxorcmp" 3 ss. 4 sspaces ;

: alu  ( adr op -- adr' )
        dup 3 rshift .alu r/m() ;

: ali ( adr op -- adr' )
        >r count
        dup 3 rshift .alu
        mod-r/m(ModR/M)
        r> 3 and ?dup
        if      1 =
                if      .imm16/32[pc++]
                else    .# count sext 0 .r>s sspace
                then
        else    .imm8[pc++]
        then ;

: ala  ( adr op -- adr' )
        dup 3 rshift .alu
        1 and if 0 .reg(XX-XXX-NNN) .imm16/32[pc++] else 0 .reg8(XX-XXX-NNN) .imm8[pc++] then ;


\ -------------------- Test/Xchg --------------------

: txb   ( addr op -- addr' )
        dup 3 and S" testtestxchgxchg" 4 ss. 3 sspaces
        1 and
        IF      1 to dis.size r,r/m()     \ SMuB removed COUNT
        ELSE    0 to dis.size r,r/m()     \ SMuB removed COUNT
        THEN
        ;

: tst   ( addr op -- addr' )
        .s" test    " 1 and
        IF      dis.16-bit-data
                IF   .s" ax , "
                ELSE .s" eax , "
                THEN
                .imm16/32[pc++]
        ELSE    .s" al , " .imm8[pc++]
        THEN
        ;

\ -------------------- Inc/Dec ----------------------

: inc  ( addr op -- addr' )
        .s" inc     " .reg16/32(XX-XXX-NNN) ;

: dec  ( addr op -- addr' )
        .s" dec     " .reg16/32(XX-XXX-NNN) ;


\ -------------------- Push/Pop --------------------

: psh   ( addr op -- addr' )
        .s" push    " .reg16/32(XX-XXX-NNN)
        ;

: pop   ( addr op -- addr' )
        .s" pop     " .reg16/32(XX-XXX-NNN) ;

: pss   ( addr op -- addr' )
        .s" push    " .sreg(XX-NNN-XXX) ;

: pps   ( addr op -- addr' )
        .s" pop     " .sreg(XX-NNN-XXX) ;

: psa   ( addr op -- addr' )
        drop dis.16-bit-data
        IF      .s" pusha   "
        ELSE    .s" pushad  "
        THEN ;

: ppa   ( addr op -- addr' )
        drop dis.16-bit-data
        IF      .s" popa    "
        ELSE    .s" popad   "
        THEN ;

: psi   ( addr op -- addr' )
        .s" push    " 2 and
true to dis.mem?
        IF      .imm8[pc++]
        ELSE    .imm16/32[pc++]
        THEN
false to dis.mem?
        ;

: psf   ( addr op -- addr' )
        drop dis.16-bit-data
        IF      .s" pushf   "
        ELSE    .s" pushfd  "
        THEN ;

: ppf   ( addr op -- addr' )
        drop dis.16-bit-data
        IF      .s" popf    "
        ELSE    .s" popfd   "
        THEN ;

: 8F.   ( addr op -- addr' )
        drop count .s" pop     " r/m16/32(ModR/M) ;

\ -------------------- Move --------------------

: mov  ( addr op -- addr' )
        .s" mov     " r/m() ;

: mri  ( addr op -- addr' ) ( mov register, imm )
        .s" mov     " dup 8 and
        IF      .reg16/32(XX-XXX-NNN) .imm16/32[pc++]
        ELSE    .reg8(XX-XXX-NNN) .imm8[pc++]
        THEN ;

: mvi  ( adr op -- adr' )   ( mov mem, imm )
        .s" mov     " drop count mod-r/m(ModR/M)
        dis.size
        IF      .imm16/32[pc++]
        ELSE    .imm8[pc++]
        THEN
        ;

: mrs   ( addr op -- addr' )
\ ? remove redundant >r , r>
        dis.16-bit-data
        IF      .s" mov     " drop
                1 to dis.size
                count dup mod-r/m(ModR/M) .,
                .sreg(XX-NNN-XXX)
        ELSE    ???
        THEN ;

: msr   ( addr op -- addr' )
        dis.16-bit-data
        IF      .s" mov     " drop
                1 to dis.size
                count dup .sreg(XX-NNN-XXX) .,
                mod-r/m(ModR/M)
        ELSE    ???
        THEN ;

: mrc   ( addr op -- addr' )
        .s" mov     "
        drop count dup .reg32(XX-XXX-NNN) .s" , "
        .creg(XX-NNN-XXX) ;

: mcr   ( addr op -- addr' )
        .s" mov     "
        drop count dup .creg(XX-NNN-XXX) .s" , "
        .reg32(XX-XXX-NNN) ;

: mrd   ( addr op -- addr' )
        .s" mov     "
        drop count dup .reg32(XX-XXX-NNN) .s" , "
        .dreg(XX-NNN-XXX) ;

: mdr   ( addr op -- addr' )
        .s" mov     "
        drop count dup .dreg(XX-NNN-XXX) .s" , "
        .reg32(XX-XXX-NNN) ;

: mrt   ( addr op -- addr' )
\ obsolete
        .s" mov     "
        drop count dup .reg32(XX-XXX-NNN) .s" , "
        .treg(XX-NNN-XXX) ;

: mtr   ( addr op -- addr' )
\ obsolete
        .s" mov     "
        drop count dup .treg(XX-NNN-XXX) .s" , "
        .reg32(XX-XXX-NNN) ;

: mv1   ( addr op -- addr' )
        .s" mov     " 1 and
        IF      dis.16-bit-data
                IF      .s" ax , "
                ELSE    .s" eax , "
                THEN
        ELSE    .s" al , "
        THEN
        .disp16/32[pc++] ;

: mv2   ( addr op -- addr' )
        .s" mov     " .disp16/32[pc++] .,
        1 and
        IF      dis.16-bit-data
                IF      .s"  ax"
                ELSE    .s"  eax"
                THEN
        ELSE    .s"  al"
        THEN
        ;

: lea  ( addr op -- addr' )
        .s" lea     " drop  1 to dis.size r,r/m() ;

: lxs   ( addr op -- addr' )
        1 and
        IF      .s" lds     "
        ELSE    .s" les     "
        THEN
        r,r/m()   \ SMuB removed COUNT
        ;

: bnd  ( addr op -- addr' )
        .s" bound   " drop  1 to dis.size r,r/m() ;

: arp   ( addr op -- addr' )
        .s" arpl    " drop
        1 to dis.size
        true to dis.16-bit-data
        r,r/m()
        ;

: mli   ( addr op -- addr' )
        1 to dis.size
        .s" imul    " 0x69 =
        IF      r,r/m() .imm16/32[pc++]
        ELSE    r,r/m() .imm8[pc++]
        THEN ;

\ -------------------- Jumps and Calls --------------------

: jsr  ( addr op -- addr' )
        .s" call    " drop .rel16/32 ;

: jmp  ( addr op -- addr' )
        .s" jmp     " 2 and if .rel8 else .rel16/32 then ;

: .jxx  ( addr op -- addr' )
        .s" j" .cnd-code(op) 4 sspaces ;

: bra  ( addr op -- addr' )
        .jxx .rel8 ;

: lup  ( addr op -- addr' )
        3 and S" loopnzloopz loop  jecxz " 6 ss. 1 sspaces .rel8 ;

: lbr  ( addr op -- addr' )
        .jxx .rel16/32 ;

: rtn  ( addr op -- addr' )
        .s" ret     near " 1 and 0=
        IF      wCount h.>s
        THEN ;

: rtf  ( addr op -- addr' )
        .s" ret     far " 1 and 0=
        IF      wCount h.>s
        THEN ;

: ent  ( addr op -- addr' )
        .s" enter   " drop wCount h.>s count h.>s ;

: cis   ( addr op -- addr' )
        0x9a =
        IF      .s" call    "
        ELSE    .s" jmp     "
        THEN
        dis.16-bit-data
        IF      .s" ptr16:16 "
        ELSE    .s" ptr16:32 "
        THEN
        count mod-r/m(ModR/M) ;

: nt3   ( addr op -- addr' )
        drop .s" int     3 "
        ;

: int   ( addr op -- addr' )
        drop .s" int     "
        count h.>s ;

inh lev "leave"
inh irt  "iret"
inh nto  "into"

\ -------------------- string ops --------------------

: str   inh does> count >s  1 and if .s" d" else .s" b" then ;

str mvs "movs"
str cps "cmps"
str sts "stos"
str lds "lods"
str scs "scas"

\ -------------------- Exchange --------------------

: xga  ( addr op -- addr' )
\        .s" xchg     eax, " .reg16/32(XX-XXX-NNN)
\ -rbs July 7th, 2002
    dup 0x90 =
    if
        drop .s" nop"
    else
        .s" xchg     eax, " .reg16/32(XX-XXX-NNN)
    then ;



\ -------------------- Shifts & Rotates --------------------

: .shift ( n -- )
        7 and S" rolrorrclrcrshlshrxxxsar" 3 ss.  4 sspaces ;

: shf  ( addr op -- addr' )
        >r count
        dup 3 rshift .shift
        mod-r/m(ModR/M) .,
        r> 0xD2 and
        case
           0xC0 of count h.>s      endof
           0xD0 of 1 h.>s          endof
           0xD2 of 1 .reg8(XX-XXX-NNN)          endof
        endcase ;

\ -------------------- Extended Opcodes --------------------

: wf1  ( addr -- addr' )
        1+ count dup
        0x0c0 <
        IF      dup
                3 rshift 7 and
                case 6 of     .s" fstenv  "      mod-r/m(ModR/M)   endof
                     7 of     .s" fstcw   word " mod-r/m(ModR/M)   endof
                     2drop 2 - dup .s" fwait   "
                endcase
        ELSE    drop 2 - .s" fwait   "
        THEN ;

: wf2  ( addr -- addr' )
        1+ count
        case 0xe2 of   .s" fclex   "  endof
             0xe3 of   .s" finit   "  endof
             swap 2 - swap .s" fwait   "
        endcase ;

: wf3  ( addr -- addr' )
        1+ count dup 3 rshift 7 and
        case 6 of     .s" fsave   "      mod-r/m(ModR/M)   endof
             7 of     .s" fstsw   word " mod-r/m(ModR/M)   endof
             2drop 2 - dup .s" fwait   "
        endcase ;

: wf4  ( addr -- addr' )
        1+ count 0xe0 =
        IF      .s" fstsw   ax "
        ELSE    2 - .s" fwait   "
        THEN ;

: fwaitops   ( addr op -- addr' )
        case 0xd9 of    wf1     endof
             0xdb of    wf2     endof
             0xdd of    wf3     endof
             0xdf of    wf4     endof
             .s" fwait   "
        endcase ;

: w8f   ( addr op -- addr' )
        drop dup c@ dup 0xf8 and 0xd8 =
        IF      fwaitops
        ELSE    drop .s" wait    "
        THEN ;

: falu1   ( xopcode -- )
        3 rshift 7 and
        S" fadd fmul fcom fcompfsub fsubrfdiv fdivr"
        5 ss. 2 sspaces ;

: falu5   ( xopcode -- )
        3 rshift 7 and
        s" fadd fmul ???? ???? fsubrfsub fdivrfdiv "
        5 ss. 2 sspaces ;

: sti.   ( op -- )
        7 and .s" ST(" 1 .r>s .s" )";


: fd8   ( addr opcode -- addr' )
        drop count dup falu1
        dup 0xc0 <
        IF      .s" float " mod-r/m(ModR/M)
        ELSE    dup 0xf0 and 0xd0 =
                IF      sti.
                ELSE    .s" ST , " sti.
                THEN
        THEN ;


: fdc   ( addr opcode -- addr' )
        drop count
        dup dup 0xc0 <
        IF      falu1 .s" double " mod-r/m(ModR/M)
        ELSE    falu5 sti. .s"  , ST"
        THEN ;


: fnullary-f   ( op -- )
        0x0f and dup 8 <
        IF
           S" f2xm1  fyl2x  fptan  fpatan fxtractfprem1 fdecstpfincstp"
        ELSE  8 -
           S" fprem  fyl2xp1fsqrt  fsincosfrndintfscale fsin   fcos   "
        THEN
        7 ss. ;


: fnullary-e   ( op -- )
        0x0f and dup 8 <
        IF
           S" fchs   fabs   ???    ???    ftst   fxam   ???    ???    "
        ELSE  8 -
           S" fld1   fldl2t fldl2e fldpi  fldlg2 fldln2 fldz   ???    "
        THEN
        7 ss. ;


: fnullary   ( op -- )
        dup 0xef >
        IF      fnullary-f EXIT
        THEN
        dup 0xe0 <
        IF      0xd0 =
                IF      .s" fnop"
                ELSE    dup ???
                THEN
                EXIT
        THEN
        fnullary-e ;


: fd9   ( addr op -- addr' )
        drop count dup 0xc0 <
        IF      dup 0x38 and
                CASE
                        0x00 OF .s" fld     float "  endof
                        0x10 OF .s" fst     float "  endof
                        0x18 OF .s" fstp    float "  endof
                        0x20 OF .s" fldenv  "        endof
                        0x28 OF .s" fldcw   word "   endof
                        0x30 OF .s" fnstenv "        endof
                        0x38 OF .s" fnstcw  word "   endof
                            dup ???
                ENDCASE
                mod-r/m(ModR/M)
        ELSE
                dup 0xd0 <
                IF      dup 0xc8 <
                        IF      .s" fld     "
                        ELSE    .s" fxch    "
                        THEN
                        sti.
                ELSE    fnullary
                THEN
        THEN ;


: falu3   ( op -- )
        3 rshift 7 and
        S" fiadd fimul ficom ficompfisub fisubrfidiv fidivr"
        6 ss. 1 sspaces ;


: fcmova  ( op -- )
        3 rshift 7 and
        S" fcmovb fcmove fcmovbefcmovu ???    ???    ???    ???    "
        7 ss. ;


: fda   ( addr op -- )
        drop count dup 0xc0 <
        IF      dup falu3 .s" dword " mod-r/m(ModR/M)
        ELSE    0xe9 =
                IF      .s" fucompp" drop
                ELSE    dup fcmova sti.
                THEN
        THEN ;


: falu7  ( op -- )
        3 rshift 7 and
        S" faddp fmulp ???   ???   fsubrpfsubp fdivrpfdivp "
        6 ss. sspace ;


: fde   ( addr op -- addr' )
        drop count dup 0xc0 <
        IF      dup falu3 .s" word " mod-r/m(ModR/M)
        ELSE    dup 0xd9 =
                if    .s" fcompp" drop
                else  dup falu7 sti.
                then
        THEN ;


: fcmovb  ( op -- )
        3 rshift 7 and
        S" fcmovnb fcmovne fcmovnbefcmovnu ???     fucomi  fcomi   ???     "
        8 ss. ;


: fdb   ( addr op -- addr' )
        drop count dup 0xc0 <
        IF      dup 0x38 and
                CASE    0x00 OF .s" fild    dword "    endof
                        0x10 OF .s" fist    dword "    endof
                        0x18 OF .s" fistp   dword "    endof
                        0x28 OF .s" fld     extended " endof
                        0x38 OF .s" fstp    extended " endof
                            dup ???
                ENDCASE
                mod-r/m(ModR/M)
        ELSE
                CASE    0xe2 OF .s" fnclex" endof
                        0xe3 OF .s" fninit" endof
                            dup dup fcmovb sti.
                ENDCASE
        THEN ;


: falu6  ( op -- )
        3 rshift 7 and
        S" ffree ???   fst   fstp  fucom fucomp???   ???   "
        6 ss. sspace ;


: fdd   ( addr op -- addr' )
        drop count dup 0xc0 <
        IF      dup 0x38 and
                CASE    0x00 OF .s" fld     double "  endof
                        0x10 OF .s" fst     double "  endof
                        0x18 OF .s" fstp    double "  endof
                        0x20 OF .s" frstor  "         endof
                        0x30 OF .s" fnsave  "         endof
                        0x38 OF .s" fnstsw  word   "  endof
                            dup ???
                ENDCASE
                mod-r/m(ModR/M)
        ELSE    dup falu6 sti.
        THEN ;


: fdf   ( addr op -- addr' )
        drop count dup 0xc0 <
        IF      dup 0x38 and
                CASE    0x00 OF .s" fild    word "   endof
                        0x10 OF .s" fist    word "   endof
                        0x18 OF .s" fistp   word "   endof
                        0x20 OF .s" fbld    tbyte "  endof
                        0x28 OF .s" fild    qword "  endof
                        0x30 OF .s" fbstp   tbyte "  endof
                        0x38 OF .s" fistp   qword "  endof
                            dup ???
                ENDCASE
                mod-r/m(ModR/M)
        ELSE    dup 0xe0 =
                IF      .s" fnstsw  ax " drop
                ELSE    dup 0x38 and
                        CASE    0x28 OF .s" fucomip " sti. endof
                                0x30 OF .s" fcomip  " sti. endof
                                        ???
                        ENDCASE
                THEN
        THEN ;


: gp6 ( addr op -- addr' )
        drop count dup 3 rshift
        7 and S" sldtstr lldtltr verrverw??? ???" 4 ss. 3 sspaces
        r/m16(ModR/M) ;


: gp7 ( addr op -- addr' )
        drop count dup 3 rshift
        7 and dup S" sgdt  sidt  lgdt  lidt  smsw  ???   lmsw  invlpg" 6 ss. 1 sspaces
        4 and 4 =
        if   r/m16(ModR/M)
        else r/m16/32(ModR/M)
        then ;




: gp8 ( addr op -- addr' )
        drop count dup .btx(XXXN-NXXX)
        r/m16/32(ModR/M) .imm8[pc++] ;

: lar ( addr op -- addr' )
        .s" lar     " drop r,r/m() ;

: lsl ( addr op -- addr' )
        .s" lsl     " drop r,r/m() ;

: lss ( addr op -- addr' )
        .s" lss     " drop r,r/m() ;

: lfs ( addr op -- addr' )
        .s" lfs     " drop r,r/m() ;

: lgs ( addr op -- addr' )
        .s" lgs     " drop r,r/m() ;

: btx ( addr op -- addr' )
        .btx(XXXN-NXXX) r/m,r() ;

: sli ( addr op -- addr' )
        .s" shld    " drop r/m,r() .imm8[pc++] ;

: sri ( addr op -- addr' )
        .s" shrd    " drop r/m,r() .imm8[pc++] ;

: slc ( addr op -- addr' )
        .s" shld    " drop r/m,r() .s" , cl" ;

: src ( addr op -- addr' )
        .s" shrd    " drop r/m,r() .s" , cl" ;

: iml ( addr op -- addr' )
        .s" imul    " drop r,r/m() ;

: cxc ( addr op -- addr' )
        .s" cmpxchg " 1 and to dis.size r/m,r() ;

: mvx { addr op \ op2 -- addr' }
        addr op
        dup 8 and
        if      .s" movsx   "
        else    .s" movzx   "
        then
        1 and >r
        count dup to op2 parse/sib r>                    \ size bit
        if    swap .reg32(XX-XXX-NNN) .,                     \ word to dword case
              3 =
              if   .reg16(XX-XXX-NNN)
              else .s" word " mod-r/m(ModR/M)
              then
        else  swap .reg16/32(XX-XXX-NNN) .,                  \ byte case
              3 =
              if    .reg8(XX-XXX-NNN)
              else
                    drop
                    .s" byte " op2 mod-r/m(ModR/M)
              then
        then
        ;

: xad ( addr op -- addr' )
        .s" xadd    " 1 and to dis.size r/m,r() ;

: bsf ( addr op -- addr' )
        .s" bsf     " drop r,r/m() ;

: bsr ( addr op -- addr' )
        .s" bsr     " drop r,r/m() ;

: cx8 ( addr op -- addr' )
        .s" cmpxchg8b " drop count r/m16/32(ModR/M) ;

: bsp ( addr op -- addr' )
        .s" bswap   " .reg32(XX-XXX-NNN) ;


: F6.  ( addr op -- addr' )
\ ??
        >r count
        dup 3 rshift 7 and dup >r S" testXXXXnot neg mul imuldiv idiv" 4 ss. 3 sspaces
        mod-r/m(ModR/M)
        r> 0= if
                r@ 1 and if .imm16/32[pc++]
                         else .imm8[pc++]
                         then
              then
        r> drop ;

: FE.  ( addr op -- addr' )
        drop count
        dup 3 rshift 7 and
        case
                0 of .s" inc     "  endof
                1 of .s" dec     "  endof
                     ???
        endcase r/m8(ModR/M) ;

: FF.  ( addr op -- addr' )
        drop count
        dup 3 rshift 7 and
        case
                0 of .s" inc     "      endof
                1 of .s" dec     "      endof
                2 of .s" call    "      endof
                3 of .s" call    far "  endof
                4 of .s" jmp     "      endof
                5 of .s" jmp     far "  endof
                6 of .s" push    "      endof
                     ???
        endcase r/m16/32(ModR/M) ;

\ --------------------- conditional move ---------------

: set   ( adr op -- )
        .s" set"
        .cnd-code(op) 2 sspaces
        count r/m8(ModR/M) ;

: cmv   ( adr op -- )
        .s" cmov"
        .cnd-code(op) 1 sspaces
        r,r/m() ;

\ --------------------- MMX Operations -----------------

: mmx-size ( op -- )
        3 and S" bwdq" 1 ss. ;

: upl   ( adr op -- adr' )
        3 and S" punpcklbwpunpcklwdpunpckldq" 9 ss. r,r/m() ;

: uph   ( adr op -- adr' )
        3 and S" punpckhbwpunpckhwdpunpckhdq" 9 ss. r,r/m() ;

: cgt   ( adr op -- adr' )
        .s" pcmpgt" mmx-size r,r/m() ;

: ceq   ( adr op -- adr' )
        .s" pcmpeq" mmx-size r,r/m() ;

: .psx(XXNN-XXXX)  ( op -- )
        0x30 and
        case
             0x10 of .s" psrl" endof
             0x20 of .s" psra" endof
             0x30 of .s" psll" endof
             .s" ???     "  \ -rbs
        endcase ;

: gpa   ( adr op -- adr' )
        \ xx00-xxxx -> ???
        >r count dup .psx(XXNN-XXXX) r> mmx-size 2 sspaces .mreg(XX-XXX-NNN) .imm8[pc++] ;

: puw   ( adr op -- adr' )
        .s" packusdw " drop r,r/m() ;

: psb   ( adr op -- adr' )
        .s" packsswb " drop r,r/m() ;

: psw   ( adr op -- adr' )
        .s" packssdw " drop r,r/m() ;

: mpd   ( adr op -- adr' )
        .s" movd    " drop count parse/ModR/M
        swap .mreg(XX-XXX-NNN) ., 3 =
        if   .reg32(XX-XXX-NNN)
        else mod-r/m(ModR/M)
        then ;

: mdp   ( adr op -- adr' )
        .s" movd    " drop count parse/ModR/M
        3 =
        if   swap .reg32(XX-XXX-NNN)
        else swap mod-r/m(ModR/M)
        then ., .mreg(XX-XXX-NNN) ;

: mpq   ( adr op -- adr' )
        .s" movq    " drop r,r/m() ;

: mqp   ( adr op -- adr' )
        .s" movq    " drop r/m,r() ;

: shx   ( adr op -- adr' )
        \ xx00-xxxx -> ???
        dup .psx(XXNN-XXXX) mmx-size 2 sspaces r,r/m() ;

: mll   ( adr op -- adr' )
        .s" pmullw  " drop r,r/m() ;

: mlh   ( adr op -- adr' )
        .s" pmulhw  " drop r,r/m() ;

: mad   ( adr op -- adr' )
        .s" pmaddwd " drop r,r/m() ;

: sus   ( adr op -- adr' )
        .s" psubus" mmx-size r,r/m() ;

: sbs   ( adr op -- adr' )
        .s" psubs" mmx-size sspace r,r/m() ;

: sub   ( adr op -- adr' )
        .s" psub" mmx-size 2 sspaces r,r/m() ;

: aus   ( adr op -- adr' )
        .s" paddus" mmx-size r,r/m() ;

: ads   ( adr op -- adr' )
        .s" padds" mmx-size sspace r,r/m() ;

: add   ( adr op -- adr' )
        .s" padd" mmx-size 2 sspaces r,r/m() ;

: pad   ( adr op -- adr' )
        .s" pand    " drop r,r/m() ;

: por   ( adr op -- adr' )
        .s" por     " drop r,r/m() ;

: pan   ( adr op -- adr' )
        .s" pandn   " drop r,r/m() ;

: pxr   ( adr op -- adr' )
        .s" pxor    " drop r,r/m() ;

\ -------------------- Opcode Table --------------------

: ops 0x10 0 do ' , loop ;

create op2-table2

\     0   1   2   3    4   5   6   7    8   9   A   B    C   D   E   F

ops  gp6 gp7 lar lsl  ??? ??? clt ???  inv wiv ??? ud2  ??? ??? ??? ???  \ 0
ops  ??? ??? ??? ???  ??? ??? ??? ???  ??? ??? ??? ???  ??? ??? ??? ???  \ 1
ops  mrc mrd mcr mdr  mrt ??? mtr ???  ??? ??? ??? ???  ??? ??? ??? ???  \ 2
ops  wmr rtc rmr rpc  ??? ??? ??? ???  ??? ??? ??? ???  ??? ??? ??? ???  \ 3

ops  cmv cmv cmv cmv  cmv cmv cmv cmv  cmv cmv cmv cmv  cmv cmv cmv cmv  \ 4
ops  ??? ??? ??? ???  ??? ??? ??? ???  ??? ??? ??? ???  ??? ??? ??? ???  \ 5
ops  upl upl upl puw  cgt cgt cgt psb  uph uph uph psw  ??? ??? mpd mpq  \ 6
ops  ??? gpa gpa gpa  ceq ceq ceq ems  ??? ??? ??? ???  ??? ??? mdp mqp  \ 7

ops  lbr lbr lbr lbr  lbr lbr lbr lbr  lbr lbr lbr lbr  lbr lbr lbr lbr  \ 8
ops  set set set set  set set set set  set set set set  set set set set  \ 9
ops  pss pps cpu btx  sli slc ??? ???  pss pps rsm btx  sri src ??? iml  \ A
ops  cxc cxc lss btx  lfs lgs mvx mvx  ??? ud1 gp8 btx  bsf bsr mvx mvx  \ B

ops  xad xad ??? ???  ??? ??? ??? cx8  bsp bsp bsp bsp  bsp bsp bsp bsp  \ C
ops  ??? shx shx shx  ??? mll ??? ???  sus sus ??? pad  aus aus ??? pan  \ D
ops  ??? shx shx ???  ??? mlh ??? ???  sbs sbs ??? por  ads ads ??? pxr  \ E
ops  ??? ??? shx shx  ??? mad ??? ???  sub sub sub ???  add add add ???  \ F

\     0   1   2   3    4   5   6   7    8   9   A   B    C   D   E   F

: 0F.  ( adr code -- )
        drop count dup
        dup 0x70 and 0x50 0x80 within to dis.mmx-reg?
        cells op2-table2 + @ execute
        0 to dis.mmx-reg? ;

create op1-table

\     0   1   2   3    4   5   6   7    8   9   A   B    C   D   E   F

ops  alu alu alu alu  ala ala pss pps  alu alu alu alu  ala ala pss 0F.  \ 0
ops  alu alu alu alu  ala ala pss pps  alu alu alu alu  ala ala pss pps  \ 1
ops  alu alu alu alu  ala ala es: daa  alu alu alu alu  ala ala cs: das  \ 2
ops  alu alu alu alu  ala ala ss: aaa  alu alu alu alu  ala ala ds: aas  \ 3

ops  inc inc inc inc  inc inc inc inc  dec dec dec dec  dec dec dec dec  \ 4
ops  psh psh psh psh  psh psh psh psh  pop pop pop pop  pop pop pop pop  \ 5
ops  psa ppa bnd arp  fs: gs: d16 a16  psi mli psi mli  inb isd osb osd  \ 6
ops  bra bra bra bra  bra bra bra bra  bra bra bra bra  bra bra bra bra  \ 7

ops  ali ali ??? ali  txb txb txb txb  mov mov mov mov  mrs lea msr 8F.  \ 8
ops  xga xga xga xga  xga xga xga xga  cwde cdq cis w8f  psf ppf sah lah  \ 9
ops  mv1 mv1 mv2 mv2  mvs mvs cps cps  tst tst sts sts  lds lds scs scs  \ A
ops  mri mri mri mri  mri mri mri mri  mri mri mri mri  mri mri mri mri  \ B

ops  shf shf rtn rtn  lxs lxs mvi mvi  ent lev rtf rtf  nt3 int nto irt  \ C
ops  shf shf shf shf  aam aad ??? xlt  fd8 fd9 fda fdb  fdc fdd fde fdf  \ D
ops  lup lup lup lup  inp inp otp otp  jsr jmp cis jmp  ind ind otd otd  \ E
ops  lok ??? rpz rep  hlt cmc F6. F6.  clc stc cli sti  cld std FE. FF.  \ F

\     0   1   2   3    4   5   6   7    8   9   A   B    C   D   E   F







\ %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\ User interface
\ %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

: dis-op  ( adr -- adr' )
        0>s
        false to dis.prefix-op          \ SMuB
        false to dis.mem?               \ rbs -- suppress commas after mem
        count
        dup 1 and to dis.size
        dup cells op1-table +  @ execute
        dis.prefix-op 0=
        if dis.default-16bit? 0=
           if   false to dis.16-bit-data
                false to dis.16-bit-addr
           else true  to dis.16-bit-data
                true  to dis.16-bit-addr
           then
        then ;



0 value next-inst

: inst  ( adr -- adr' )
        dup to next-inst
        cols 0x29 <
        if      dis-op
                s-buf count type
        else    dup>r dis-op
                r@ dis.base-addr - 6 h.r space
                dup r>
                2dup - 0x10 u> abort" decompiler error"
                do i c@ 2 h.n loop
                comment-col col s-buf count type
        then    dup to next-inst ;


: dis-db   cr .s" db " count h.>s ;
: dis-dw   cr .s" dw " wCount h.>s ;
: dis-dd   cr .s" dd " LCount h.>s ;
: dis-ds   cr .s" string " 0x22 emit>s count 2dup >s + 0x22 emit>s ;

: dis  ( adr -- )
        begin
                dup
                cr inst
                key upc dup 0x1B = over ascii Q = or not
        while
                case
                  'B' of drop dis-db endof
                  'W' of drop dis-dw endof
                  'D' of drop dis-dd endof
                  'S' of drop dis-ds endof
                         rot drop
                endcase

        repeat 2drop drop ;

\ create stopcode 8 C,                           \ count of length
\                 0xAD C, 0x8B C, 0x0C C, 0x38 C,
\                 0x03 C, 0xCF C, 0xFF C, 0xE1 C,

: next?         ( a1 -- f1 )
                next-seq count tuck compare 0= ;

0 value show-next?      \ default to not showing next instructions

: rest          { adr \ sv1 sv2 -- }
                adr
                begin
                        tabing-on 0tab
                        cr
                        dup next-seq c@ - next? 0=      \ NEXT, behind us?
                while   dup next?
                        if      comment-col col
                                0xD right-margin - ?line
                                .s"  \ NEXT, MACRO"
                                show-next?
                                if      cr
                                        inst
                                else    next-seq c@ +   \ skip over it
                                then
                        else
>r to sv1 to sv2 r>
                                inst
>r sv2 sv2 r>
                        then
                        tabing-off
                        start/stop
                repeat  drop .s" END-CODE  "
                tabing-off ;

hidden

\in-system-ok ' rest is discode       \ link into high level decompiler

decimal

forth definitions

: show-next-on  ( -- )
                TRUE to show-next? ;

: show-next-off ( -- )
                FALSE to show-next? ;

: rest rest ;

: seemore       ( -- )
                tabing-on 0tab
                next-inst
                cr inst
                rest ;

ONLY FORTH ALSO DEFINITIONS

