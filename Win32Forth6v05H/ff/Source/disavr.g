
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ \\\\\\\\\\\\\\\\          AVR disassembler          \\\\\\\\\\\\\\\\
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

0 value standalone
standalone [if]
        anew mydisassembler
        : .label        ( a -- )        h. ;
        : >$            ( a n -- )      type ;
[then]

\ Uses global lookup words for displaying labels:
\ .LABEL  ( a -- )
\         Displays the address in hex, unless it can be identified.
\         Used text labels if possible.  Includes a trailing blank.

vocabulary disasmAVR
also disasmAVR definitions

    0 value opcode
  456 value dpc
    0 value offset              \ add this to dpc to get physical address

\ all instructions are 16-bit:

hex
standalone [if]
: nextword      ( -- n )        123 ;
[else]
: nextword      ( -- n )        dpc offset + w@  2 +to dpc ;
[then]

: >, s" ," >$ ; : >+ s" +" >$ ;  : >- s" -" >$ ;
: >X s" X" >$ ;  : >Y s" Y" >$ ;  : >Z s" Z" >$ ;  
: .num          ( n -- )  (.) >$ ;
: .dnum         ( n -- )  base @ swap decimal .num base ! ;
: .port         ( A -- )  .asmlabel ;
: .Rn           ( n -- )  s" R" >$ .dnum ;
: Rd=           ( -- n )  opcode 4 rshift 1F and  ;     \ rdddddrrrr
: Rd            ( -- )    Rd= .Rn ;   : _Rd >$ Rd ;
: Rr=           ( -- n )  opcode 20 /mod 10 and swap 0F and or ;
: Rr            ( -- )    Rr= .Rn ;   : _Rr >$ Rr ;
: >A            ( -- )    opcode 20 /mod 30 and swap 0F and or .port ;
: d=d?          ( -- f )  Rd= Rr= = ;
: Rd,Rr         ( str - ) >$ Rd >, Rr ;
: Rd,Rr3        ( str - ) >$ Rd= 7 and 10 or .Rn >, Rr= 7 and 10 or .Rn ;
: nibbles       ( -- n0 n1 ) opcode 0FF and 10 /mod ;
: Rd,Rr16       ( str - ) >$ nibbles 10 + .Rn >, 10 + .Rn ;
: Rd,Rrw        ( str - ) >$ nibbles 2* .Rn >, 2* .Rn ;
: Rd,K6         ( str - ) >$ nibbles 4 /mod swap 2* 18 + .Rn >, 4 lshift + .num ;
: Rd,K8         ( str - ) >$ opcode 4 rshift 0F0 and nibbles 10 + .Rn >, + .num ;
: A,b           ( str - ) >$ opcode 0FF and 8 /mod .port >, .num ;
: >flag         ( n -- )  >r s" CZNVSHTI" r> 7 and /string drop 1 >$ ;
: Rd,b          ( str - ) >$ Rd >, opcode >flag ;
: Rd,b#         ( str - ) >$ Rd >, opcode 7 and .num ;
: k16           ( -- )    nextword (.) >$ ;
: srbit         ( str - ) >$ opcode 4 rshift >flag ;

: +q            ( -- )
                >+ opcode 8 /mod 7 rshift       \ low3 q0qq
                4 /mod 2 and 2* + 3 lshift + .num ;

: brs           ( a n -- )      \ 10-bit branch
                opcode 3FF and 8 /mod >r 2* /string drop 2
                s" BR" >$ >$ s"   " >$       \ sign extend 0..127 to -64..+63
                r@ 40 and 0<> -80 and r> or 2* dpc + .label ;

: reljump       ( a n -- )      \ 12-bit branch
                >$ opcode 0FFF and
                dup 800 and 0<> -1000 and or \ sign extend 0..4095 to -2K..+2K
                2* dpc +                \ convert to abs location (byte address)
                heremax and             \ wrap address: heremax is ((2^n)-1)
                .label ;

: longjump      ( a n -- )
                >$ opcode 8 /mod 3E and swap 1 and or   \ bits above b15
                nextword swap word-join 2* .label ;

: if[           ( exec: n1 n2 -- n1 )
                postpone over postpone = postpone if ; immediate
: ]then         ( exec: n1 -- )
                postpone drop postpone exit postpone then ; immediate
: ]]then        ( exec: n1 -- )
                postpone 2drop postpone exit postpone then ; immediate

: dis           ( opcode -- )
                dup to opcode                                   \ save opcode
                0000 if[ s" NOP"         >$             ]then
                9508 if[ s" RET"         >$             ]then
                9518 if[ s" RETI"        >$             ]then
                9419 if[ s" EIJMP"       >$             ]then
                9519 if[ s" EICALL"      >$             ]then
                9588 if[ s" SLEEP"       >$             ]then
                95A8 if[ s" WDR"         >$             ]then
                95C8 if[ s" LPM"         >$             ]then
                95D8 if[ s" ELPM"        >$             ]then
                95E8 if[ s" SPM"         >$             ]then
                95F8 if[ s" ESPM"        >$             ]then
                9509 if[ s" ICALL"       >$             ]then
                9409 if[ s" IJMP"        >$             ]then
                7 rshift                                        \ xxxxxxxxx
                006  if[ opcode 08 and
                         00 if[ s" MULSU " Rd,Rr3       ]]then
                         08 if[ s" FMUL  " Rd,Rr3       ]]then
                         drop then
                007  if[ opcode 08 and
                         00 if[ s" FMULS " Rd,Rr3       ]]then
                         08 if[ s" FMULSU " Rd,Rr3      ]]then
                         drop then
                128  if[ opcode 0F and
                         08 if[ s" SE" srbit            ]]then
                         drop then
                129  if[ opcode 0F and
                         08 if[ s" CL" srbit            ]]then
                         drop then
                1 rshift                                        \ xxxxxxxx
                01 if[ s" MOVW  " Rd,Rrw                ]then
                02 if[ s" MULS  " Rd,Rr16               ]then
                96 if[ s" ADIW  " Rd,K6                 ]then
                97 if[ s" SBIW  " Rd,K6                 ]then
                98 if[ s" CBI   " A,b                   ]then
                99 if[ s" SBIC  " A,b                   ]then
                9A if[ s" SBI   " A,b                   ]then
                9B if[ s" SBIS  " A,b                   ]then
               0EF if[ opcode 0F and
                        0F if[ s" SER   "  _Rd          ]]then
                        drop then
                1 rshift                                        \ xxxxxxx
                40 if[ opcode 0F and
                        00 if[ s" LD    " _Rd >, >Z     ]]then
                        06 if[ s" ELPM  " _Rd >, >Z     ]]then
                        07 if[ s" ELPM  " _Rd >, >Z >+  ]]then
                        08 if[ s" LD    " _Rd >, >Y     ]]then
                        drop then
                41 if[ opcode 0F and
                        00 if[ s" ST    " >$ >Z    >, Rd ]]then
                        08 if[ s" ST    " >$ >Y >, Rd   ]]then
                        drop then
                48 if[ opcode 0F and
                        00 if[ s" LDS   " _Rd >, k16    ]]then
                        01 if[ s" LD    " _Rd >, >Z >+  ]]then
                        02 if[ s" LD    " _Rd >, >- >Z  ]]then
                        04 if[ s" LPM   " _Rd >, >Z     ]]then
                        05 if[ s" LPM   " _Rd >, >Z >+  ]]then
                        06 if[ s" ELPM  " _Rd >, >Z     ]]then
                        07 if[ s" ELPM  " _Rd >, >Z >+  ]]then
                        09 if[ s" LD    " _Rd >, >Y >+  ]]then
                        0A if[ s" LD    " _Rd >, >- >Y  ]]then
                        0C if[ s" LD    " _Rd >, >X     ]]then
                        0D if[ s" LD    " _Rd >, >X >+  ]]then
                        0E if[ s" LD    " _Rd >, >- >X  ]]then
                        0F if[ s" POP   " _Rd           ]]then
                        drop then
                49 if[ opcode 0F and
                        00 if[ s" STS   " >$ k16   >, Rd ]]then
                        01 if[ s" ST    " >$ >Z >+ >, Rd ]]then
                        02 if[ s" ST    " >$ >- >Z >, Rd ]]then
                        09 if[ s" ST    " >$ >Y >+ >, Rd ]]then
                        0A if[ s" ST    " >$ >- >Y >, Rd ]]then
                        0C if[ s" ST    " >$ >X    >, Rd ]]then
                        0D if[ s" ST    " >$ >X >+ >, Rd ]]then
                        0E if[ s" ST    " >$ >- >X >, Rd ]]then
                        0F if[ s" PUSH  "  _rd          ]]then
                        drop then
                4A if[ opcode 0F and
                        00 if[ s" COM   "  _rd          ]]then
                        01 if[ s" NEG   "  _rd          ]]then
                        02 if[ s" SWAP  "  _rd          ]]then
                        03 if[ s" INC   "  _rd          ]]then
                        05 if[ s" ASR   "  _rd          ]]then
                        06 if[ s" LSR   "  _rd          ]]then
                        07 if[ s" ROR   "  _rd          ]]then
                        0A if[ s" DEC   "  _rd          ]]then
                        0C if[ s" JMP   "  longjump     ]]then
                        0D if[ s" JMP   "  longjump     ]]then
                        0E if[ s" CALL  "  longjump     ]]then
                        0F if[ s" CALL  "  longjump     ]]then
                        drop then
                7C if[ opcode 08 and
                        00 if[ s" BLD   "  Rd,b#        ]]then
                        drop then
                7D if[ opcode 08 and
                        00 if[ s" BST   "  Rd,b#        ]]then
                        drop then
                7E if[ opcode 08 and
                        00 if[ s" SBRC  "  Rd,b#        ]]then
                        drop then
                7F if[ opcode 08 and
                        00 if[ s" SBRS  "  Rd,b#        ]]then
                        drop then
                1 rshift                                        \ xxxxxx
                01 if[ s" CPC   "  Rd,Rr                ]then
                02 if[ s" SBC   "  Rd,Rr                ]then
                03 if[ d=d?
                   if s" LSL   " _Rd else s" ADD   " Rd,Rr then ]then
                04 if[ s" CPSE  "  Rd,Rr                ]then
                05 if[ s" CP    "  Rd,Rr                ]then
                06 if[ s" SUB   "  Rd,Rr                ]then
                07 if[  d=d?
                   if s" ROL   " _Rd else s" ADC   " Rd,Rr then ]then
                08 if[ d=d?
                   if s" TST   " _Rd else s" AND   " Rd,Rr then ]then
                09 if[ d=d?
                   if s" CLR   " _Rd else s" EOR   " Rd,Rr then ]then
                0A if[ s" OR    "  Rd,Rr                ]then
                0B if[ s" MOV   "  Rd,Rr                ]then
                27 if[ s" MUL   "  Rd,Rr                ]then
                3C if[ s" CSEQMIVSLTHSTSIE" brs         ]then
                3D if[ s" CCNEPLVCGEHCTCID" brs         ]then
                1 rshift                                        \ xxxxx
                16 if[ s" IN    " >$ Rd >, >A           ]then
                17 if[ s" OUT   " >$ >A >, Rd           ]then
                1 rshift                                        \ xxxx
                03 if[ s" CPI   "  Rd,K8                ]then
                04 if[ s" SBCI  "  Rd,K8                ]then
                05 if[ s" SUBI  "  Rd,K8                ]then
                06 if[ s" ORI   "  Rd,K8                ]then
                07 if[ s" ANDI  "  Rd,K8                ]then
                0C if[ s" RJMP  "  reljump              ]then
                0D if[ s" RCALL "  reljump              ]then
                0E if[ s" LDI   "  Rd,K8                ]then
                opcode 0D208 and
                   8000 if[ s" LDD   " _Rd >, >Z +q     ]]then
                   8008 if[ s" LDD   " _Rd >, >Y +q     ]]then
                   8200 if[ s" STD   " >$ >Z +q >, Rd   ]]then
                   8208 if[ s" STD   " >$ >Y +q >, Rd   ]]then
                   drop
                drop
                s" Unknown:" >$ base @ hex opcode (.) >$ base !
                ;

standalone 0= [if]

: disAVR        ( at ah  -- at' a-addr len )
\ given target address and address of actual data, returns tha next
\ disassemblable address and a string
                4 to disassyDatawidth
                newout
                over to dpc       \ current disassembly pointer (target addr)
                swap - to offset  \ dpc = t   offset = h-t
                nextword dis      \ disassemble
                dpc
                outpad count ;

7 cpuid" AVR|AT90S2313|||||AT90S8515||Mega103|"
\        0  1         23456         78
7 new-disasm disAVR
[then]
decimal
previous definitions 


