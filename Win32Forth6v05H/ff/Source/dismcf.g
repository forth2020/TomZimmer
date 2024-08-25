\ Coldfire Disassembler

\ anew disassemble_this
hex
vocabulary coldfiredis
also coldfiredis definitions

0 constant standalone?          \ want to compile for standalone testing?

standalone? [if]
      0 value   #col                                    \ current console column
      : >$      ( a-addr len -- )  tuck type +to #col ; \ console output
      : .label  ( t-address -- )   s" 0x" >$ (u.) type ; \ address with label
      : tab>$   ( n -- )        #col - 0max spaces 0 to #col ;
      : c>n     ( c -- n )      dup   80 and 0<>   -80 and or ;
      : w>n     ( c -- n )      dup 8000 and 0<> -8000 and or ;
[then]

defer >>        ( -- 16-bits )          ' false is >>   \ data input
0 value         DPC                                     \ returns base address
0 value         operand                                 \ operand storage
0 value         size                                    \ default operand size
8 constant      opcolumn                                \ operands print here

: c>$           ( c -- )        pad c! pad 1 >$ ;       \ char to console
: optab         ( -- )          opcolumn tab>$ ;        \ tab to operand column
: >$>           ( a n -- )      >$ optab ;              \ type and tab
: immediate16   ( -- n )        >> ;
: immediate32   ( -- n )        >> >> swap word-join ;


\ fetch operand fields --------------------------------------------------------
: register      ( -- reg# )             operand 9 rshift 7 and ;
: regy          ( -- reg# )             operand 7 and ;
: mode          ( -- mode )             operand 3 rshift 7 and ;
: opmode        ( -- opmode )           operand 6 rshift 7 and ;
: ea            ( -- reg mode )         operand 3F and 8 /mod ;
: reg/mem       ( -- f )                operand 8 and 0= ;  \ T=reg F=mem
: condition     ( -- cond )             operand 8 rshift 0F and ;
: disp8         ( -- displacement )     operand 0FF and ;
: mulqrs        ( -- regDr regDq size ) >> 8 /mod dup 400 and >r 9 rshift r> ;

\ basic display stuff ---------------------------------------------------------
: .hex          ( n -- )        hex (.) >$ ;
: .chex         ( c -- )        0FF and c>n .hex ;
: .[ '(' c>$ ;  : ., ',' c>$ ;  : .. '.' c>$ ;  : .# '#' c>$ ;  : .] ')' c>$ ;
: .???          ( -- )          s" ???" >$ ;
: .immediate8   ( -- )          immediate16 c>n .hex ;
: .immediate16  ( -- )          immediate16 .hex ;
: .immediate32  ( -- )          immediate32 .hex ;
: .bwl          ( n -- )        .. chars s" BWLL" drop + 1 >$ ;
: .size         ( -- )          size .bwl  ;
: .n            ( n -- )        '0' + c>$ ;
: .xn           ( op1 -- )      0C rshift 8 /mod if 'A' else 'D' then c>$ .n ;
: .trap#        ( -- )          optab operand 0F and .# .hex ;
: .movem        ( -- )          s" MOVEM.L" >$> ;
: .movel        ( -- )          s" MOVE.L" >$> ;
: .unknown      ( -- )          s" unknown" >$ ;
: .condition    ( -- )          operand 7 rshift 01E and chars
                                s" T F HILSCCCSNEEQVCVSPLMIGELTGTLE"
                                drop + 2 >$ ;
: .xnl          ( op1 -- )      dup 400 and \ mulx.l ddivx.l  short or long?
                                if 'D' c>$ dup 7 and .n ':' c>$ then .xn ;

\ display effective addresses -------------------------------------------------
: .dn           ( reg -- )      'D' c>$ .n ;                             \ 0
: .an           ( reg -- )      'A' c>$ .n ;                             \ 1
: .pc           ( -- )          s" PC" >$ ;
: .(an)         ( reg -- )      .[ .an .] ;                              \ 2
: .(an)+        ( reg -- )      .[ .an .] '+' c>$ ;                      \ 3
: .-(an)        ( reg -- )      '-' c>$ .[ .an .] ;                      \ 4
: .bex          ( brex -- )
                dup .xn  dup 800 and 0= if '?' else 'L' then .. c>$   \ xn.L
                9 rshift 3 and  ?dup
                if      s" ,+" >$  chars s" 124?" drop + c@ c>$ \ optional scale
                then    ;
: .(d16,an)     ( reg -- )      .immediate16 .[ .an .] ;                 \ 5
: .(d8,an,xn)   ( reg -- )      >> dup .chex .[  swap .an ., .bex .] ;   \ 6
: .(xxx).w      ( x -- )        drop immediate16 w>n .label ;            \ 8
: .(xxx).l      ( x -- )        drop immediate32 .label ;                \ 9
: .(d16,pc)     ( x -- )        drop DPC >> w>n + .label .[ .pc .] ;     \ 10
: .(d8,pc,xn)   ( x -- )        drop DPC >> tuck 0FF and c>n + .label
                                .[ .pc ., .bex .] ;                      \ 11
: .#(data)      ( x -- )
                .# drop size
                case    0 of .immediate8    endof
                        1 of .immediate16   endof
                        2 of .immediate32   endof
                        3 of .immediate32   endof
                endcase ;
create eamodes  ' .dn ,         ' .an ,         ' .(an) ,       ' .(an)+ ,
                ' .-(an) ,      ' .(d16,an) ,   ' .(d8,an,xn) , ' .???  ,
                ' .(xxx).w ,    ' .(xxx).l ,    ' .(d16,pc) ,   ' .(d8,pc,xn) ,
                ' .#(data) ,    ' .??? ,        ' .??? ,        ' .??? ,

: .eamode       ( reg mode -- ) cells eamodes + @ execute ;

: .ea           ( mask -- )
\ display effective address from MOD/REG field, mask bits indicate which
\ addressing modes are valid: bits 0..7 are for modes, bits 8..15 are for M7
                >r ea  dup 7 =
                if      1+ + dup        \ special modes map to 8..15
                then    1 over lshift   ( r m . )
                r> and
                if      .eamode
                else    2drop .???      \ invalid mode
                then    ;

: .regy         ( -- )        regy .dn ;
: .rega         ( -- )        regy .an ;
: .ea>dx        ( -- )        optab 1FFF .ea ., register .dn ;
: .ea>ax        ( -- )        optab 1FFF .ea ., register .an ;
: .dx>ea        ( mask -- )   optab register .dn ., .ea ;
: .imm>rx       ( -- )        optab .# .immediate32 ., regy .dn ;

: .immbit       ( -- )
                mode if 'B' else 'L' then c>$
                optab .# >> 0FF and .hex ., 03FD .ea ;

: .immea        ( -- )
                mode if 'B' else 'L' then c>$
                03FD .dx>ea ;

: .reglist      ( mask -- )     \ 16 register mask
                0 10 0                          ( mask count . . )
                do      over 1 and
                        if      dup if '/' c>$ then
                                s" D0D1D2D3D4D5D6D7A0A1A2A3A4A5A6SP" drop
                                i 2* chars + 2 >$
                                1+
                        then    >r 2/ r>
                loop    2drop ;

\ opcode handlers -------------------------------------------------------------

: op0           ( -- )
                operand
                case    3 rshift
                        010 of  s" ORI.L" >$  .imm>rx                   endof
                        050 of  s" ANDI.L" >$ .imm>rx                   endof
                        090 of  s" SUBI.L" >$ .imm>rx                   endof
                        0D0 of  s" ADDI.L" >$ .imm>rx                   endof
                        150 of  s" EORI.L" >$ .imm>rx                   endof
                        190 of  s" CMPI.L" >$ .imm>rx                   endof
                        3 rshift
                        020 of  s" BTST." >$ .immbit                    endof
                        021 of  s" BCHG." >$ .immbit                    endof
                        022 of  s" BCLR." >$ .immbit                    endof
                        023 of  s" BSET." >$ .immbit                    endof
                        7 and
                        4 of    s" BTST." >$  .immea                    endof
                        5 of    s" BCHG." >$  .immea                    endof
                        6 of    s" BCLR." >$  .immea                    endof
                        7 of    s" BSET." >$  .immea                    endof
                        .unknown
                endcase ;

create op1sizes 0 c, 0 c, 2 c, 1 c,

: op1           ( -- )  \ use for op1, op2 and op3
                s" MOVE" >$  opmode 1 = if 'A' c>$ then
                operand 0C rshift 3 and chars op1sizes + c@ to size .size
                optab
                1FFF .ea .,                                     \ source
                operand 6 rshift 3F and  8 /mod swap 3 lshift + to operand
                03FF .ea ;                                      \ destination

: .ccr          ( n -- )
                0FFF and
                case    002  of s" CACR" >$      endof
                        004  of s" ACR0" >$      endof
                        005  of s" ACR1" >$      endof
                        801  of s" VBR" >$       endof
                        80F  of s" PC" >$        endof
                        C00  of s" ROMBAR" >$    endof
                        C04  of s" RAMBAR" >$    endof
                        C0F  of s" MBAR" >$      endof
                        .hex
                endcase ;

: op4           ( -- )
                operand
                case    4AC8 of s" HALT"   >$                           endof
                        4BCC of s" PULSE"  >$                           endof
                        4AFC of s" ILLEGAL" >$                          endof
                        4E71 of s" NOP"    >$                           endof
                        4E72 of s" STOP"   >$> .# .immediate16          endof
                        4E73 of s" RTE"    >$                           endof
                        4E75 of s" RTS"    >$                           endof
                        4E7B of s" MOVEC"  >$> >> dup .xn ., .ccr       endof
                        3 rshift                \ format: 0100xxxxxxxxxREG
                        0810 of s" NEGX.L" >$> .regy                    endof
                        0818 of s" MOVE.W" >$> s" SR," >$  .regy        endof
                        0858 of s" MOVE.W" >$> s" CCR," >$ .regy        endof
                        0890 of s" NEG.L"  >$> .regy                    endof
                        0898 of s" MOVE.W" >$> .regy ., s" CCR" >$      endof
                        08D0 of s" NOT.L"  >$> .regy                    endof
                        0908 of s" SWAP"   >$> .regy                    endof
                        0910 of s" EXT.W"  >$> .regy                    endof
                        0918 of s" EXT.L"  >$> .regy                    endof
                        0938 of s" EXTB.L" >$> .regy                    endof
                        09C8 of s" TRAP"   >$  .trap#                   endof
                        09C9 of s" TRAP"   >$  .trap#                   endof
                        09CA of s" LINK"   >$> .rega ., .# .immediate16 endof
                        09CB of s" UNLK"   >$> .rega                    endof
                        0928 of s" EXT.L"  >$> .regy                    endof
                        3 rshift                \ format: 0100xxxxxxMODREG
                        0108 of s" CLR.B"  >$> 03FD .ea                 endof
                        0109 of s" CLR.W"  >$> 03FD .ea                 endof
                        010A of s" CLR.L"  >$> 03FD .ea                 endof
                        0113 of s" MOVE.W" >$> 1 to size
                                               1001 .ea ., s" CCR" >$   endof
                        011B of s" MOVE.W" >$> 1 to size
                                               1001 .ea ., s" SR" >$    endof
                        0121 of s" PEA.L"  >$> 0FE4 .ea                 endof
                        0128 of s" TST.B"  >$> 1FFF .ea                 endof
                        0129 of s" TST.W"  >$> 1FFF .ea                 endof
                        012A of s" TST.L"  >$> 1FFF .ea                 endof
                        0130 of >> dup 800 and if s" MULS.L" else s" MULU.L" then
                                >$> 03D .ea ., .xnl                     endof
                        0131 of >> dup 800 and if s" DIVS.L" else s" DIVU.L" then
                                >$> 03D .ea ., .xnl                     endof
                        013A of s" JSR"    >$> 0FE4 .ea                 endof
                        013B of s" JMP"    >$> 0FE4 .ea                 endof
                        0123 of .movem >> .reglist ., 024 .ea           endof
                        0133 of .movem >> 024 .ea ., .reglist           endof
                        dup 7 and 7 =
                        if      s" LEA.L" >$> 0FE4 .ea ., register .an
                        else    .unknown
                        then
                endcase ;

: .scc          ( -- )
                mode 0=
                if      'S' c>$ .condition optab .regy
                else    .unknown
                then    ;

: .shortdata    ( -- n ) .# register dup 0= if 8 + then .n ;

: op5           ( -- )
                operand
                case    51FA of s" TRAPF.W" >$ optab .# .immediate16    endof
                        51FB of s" TRAPF.L" >$ optab .# .immediate32    endof
                        51FC of s" TRAPF" >$                            endof
                        6 rshift 7 and
                        2 of    s" ADDQ.L" >$> .shortdata ., 03FF .ea   endof
                        3 of    .scc                                    endof
                        6 of    s" SUBQ.L" >$> .shortdata ., 03FF .ea   endof
                        7 of    .scc                                    endof
                        .unknown
                endcase ;

: .bran         ( -- )
                operand 0FF and
                case    0   of >> w>n 2 -               endof \ 16-bit
                        0FF of .. 'L' c>$
                               >> >> swap word-join 4 - endof \ 32-bit
                        .. 'S' c>$ c>n 0                \ 8-bit offset
                endcase dpc + optab .label ;

: op6           ( -- )
                operand 8 rshift 0F and
                case    0 of    s" BRA" >$ .bran                        endof
                        1 of    s" BSR" >$ .bran                        endof
                        s" B" >$ .condition .bran
                endcase ;

: op7           ( -- )  s" MOVEQ" >$> .# operand .chex ., register .dn ;

: op8           ( -- )
                opmode
                case    2 of    s" OR" >$ .size .ea>dx                  endof
                        6 of    s" OR" >$ .size  03FC .dx>ea            endof
                        3 of    s" DIVU.W" >$ 1 to size  .ea>dx         endof
                        7 of    s" DIVS.W" >$ 1 to size  .ea>dx         endof
                        .unknown
                endcase ;

: .addsub       ( -- )
                opmode
                case    2 of    s" .L" >$  .ea>dx                       endof
                        7 of    s" A.L" >$ .ea>ax                       endof
                        6 of    mode 0=
                                if      s" X.L" >$> .regy ., register .dn 
                                else    s" .L" >$  03FC .dx>ea
                                then                                    endof
                        .unknown
                endcase ;

: op9           ( -- )  s" SUB" >$ .addsub ;

: .rn           ( -- )         0F and 'R' c>$ .hex ;
: .mrn          ( -- )         operand .rn ;
: .mea          ( -- )         .movel 1003 .ea ., ;
: .msize        ( op -- op )   dup 0B rshift 1 and 1+ .bwl ;   \ .W or .L
: .muy          ( op -- op )   dup 80 and if 'U' else 'L' then c>$ ;
: .mux          ( op -- op )   dup 40 and if 'U' else 'L' then c>$ ;
: mscf          ( op -- op scale_factor )  dup 9 rshift 3 and ;
: .mry          ( -- )         operand 6 rshift 2 /mod 1 rshift 0E and or .rn ;
: .mrx          ( -- )         operand 0F and .rn ;
: .mscf         ( n -- )       2* chars  s" <<??>>" drop + 2 >$ ;

: .mac          ( -- )
                >> dup 100 and if s" MSAC" else s" MAC" then >$
                .msize optab .mry .muy ., .mrx .mux  mscf ?dup
                if      ., .mscf
                then    drop
                s" ,ACC" >$ ;

: .macl         ( -- )
                >> dup 100 and if s" MSACL" else s" MACL" then >$
                .msize optab .mry .muy ., dup .rn .mux  mscf ?dup
                if      ., .mscf
                then    ., 03C .ea
                dup 20 and if '&' c>$ then .,
                0C rshift .rn ;

: opA           ( -- )  \ ColdFire MAC unit opcodes
                operand
                case    A9C0 of .movel s" MACSR,MACCCR" >$              endof
                        4 rshift
                        0A18 of .movel s" ACC,"   >$ .mrn               endof
                        0A98 of .movel s" MACSR," >$ .mrn               endof
                        0AD8 of .movel s" MASK,"  >$ .mrn               endof
                        2 rshift
                        0284 of .mea   s" ACC"    >$                    endof
                        02A4 of .mea   s" MACSR"  >$                    endof
                        02B4 of .mea   s" MASK"   >$                    endof
                        1 rshift 3 and
                        0 of .mac                                       endof
                        1 of .macl                                      endof
                        .unknown
                endcase ;

: opB           ( -- )
                opmode
                case    2 of    s" CMP" >$ .size .ea>dx                 endof
                        6 of    s" EOR" >$ .size 03FD .dx>ea            endof
                        7 of    s" CMPA" >$ .size .ea>ax                endof
                        .unknown
                endcase ;

: opC           ( -- )
                opmode
                case    2 of    s" AND" >$ .size  .ea>dx                endof
                        6 of    s" AND" >$ .size   03FC .dx>ea          endof
                        3 of    s" MULU.W" >$ 1 to size  .ea>dx         endof
                        7 of    s" MULS.W" >$ 1 to size  .ea>dx         endof
                        .unknown
                endcase ;

: opD           ( -- )  s" ADD" >$ .addsub ;

: op3op         ( n -- )
                3 and 3 * chars s" AS LS ROXRO " drop + 3 -trailing >$
                operand 100 and if 'L' else 'R' then c>$ ;

: opE           ( -- )  \ shift/rotate
                size 3 =
                if      .unknown
                else    mode op3op .size optab operand 20 and
                        if      register .dn            \ dx,dy
                        else    .shortdata              \ #n,dy
                        then    ., regy .dn
                then    ;

: expect3       ( -- )  >> 3 <> if s"  <invalid>" >$ then ;

: opF           ( -- )
                operand
                case    3 rshift
                        1E9D of s" CPUSHL" >$> .rega expect3            endof
                        3 rshift
                        03EF of s" WDEBUG.L" >$> 024 .ea expect3        endof
                        .unknown
                endcase ;

create opcodes  ' op0 , ' op1 , ' op1 , ' op1 , ' op4 , ' op5 , ' op6 , ' op7 ,
                ' op8 , ' op9 , ' opA , ' opB , ' opC , ' opD , ' opE , ' opF ,

: instruction   ( -- )
\ decode opcode and operand
                >> to operand
                operand 6 rshift 3 and to size  \ default size
                operand 0C rshift 0F and cells
                opcodes + @ execute ;


\ connect the disassembler to Firmware Studio ---------------------------

standalone? [if]

16384 constant tdsize
create testdata tdsize allot   variable testptr

:noname         ( -- n )
                testptr @ dup>r w@
                r> 2 + testptr !
                2 +to DPC ;              is >>

: d             ( params... -- )
\ test disassembler from stack parameters:  42A9 1234 --> CLR.L 1234(A0)
                depth 2* testdata + testptr !   \ -> end of data
                depth 0
                do      testptr @ 2 - dup
                        testptr !  w!           \ load test data
                loop
                testdata testptr !              \ zero pointer
                0 to #col
                instruction ;                   \ disassemble test code

0 value myfile

: load          ( <filename> -- )
                bl parse 2dup type r/o open-file abort"  Bogus filename"
                to myfile
                testdata tdsize myfile read-file abort"  Unreadable file"
                . ." bytes read." cr
                ." press any key to scroll, ESC to quit." cr
                testdata tdsize 2/ 0            \ change byte order of 16-bit
                do      dup w@ 0x100 /mod swap 0x100 * +
                        over w!  2 +
                loop    drop
                testdata testptr !              \ zero pointer
                0x1000 to DPC
                begin   cr testptr @ >r DPC h.
                        0 to #col instruction
                        8 spaces r@ testptr @ r> -     ( a n )
                        bounds ?do i c@ hex 0 <# # # #> type loop
                        key 0x1B =
                until   ;

\ use TO DPC to set base address,
\ push immediate data onto stack before D

[else]

0 value offset

:noname         ( -- n )
                DPC offset + w@
                byte-split swap byte-join       \ little --> big endian
                DPC 2 + to DPC ;  is >>

: discf         ( at ah  -- at' a-addr len )
\ given target address and address of actual data, returns the next
\ disassemblable address and a string
                6 to disassyDatawidth
                newout
                over to DPC      \ DPC -> current target address
                swap - to offset \ offset = # to add to DPC to get host addr
                instruction
                DPC
                outpad count ;
decimal
: loadwatchcf   ( -- )
                3 ['] @regw  ['] !regw  66 2 s" SR"     +lowatch
                4 ['] @regl  ['] !regl   0 4 s" D0"     +lowatch
                5 ['] @regl  ['] !regl   4 4 s" D1"     +lowatch
                6 ['] @regl  ['] !regl   8 4 s" D2"     +lowatch
                7 ['] @regl  ['] !regl  12 4 s" D3"     +lowatch
                8 ['] @regl  ['] !regl  16 4 s" D4"     +lowatch
                9 ['] @regl  ['] !regl  20 4 s" D5"     +lowatch
               10 ['] @regl  ['] !regl  24 4 s" D6"     +lowatch
               11 ['] @regl  ['] !regl  28 4 s" D7"     +lowatch
               12 ['] @regl  ['] !regl  32 4 s" A0"     +lowatch
               13 ['] @regl  ['] !regl  36 4 s" A1"     +lowatch
               14 ['] @regl  ['] !regl  40 4 s" A2"     +lowatch
               15 ['] @regl  ['] !regl  44 4 s" A3"     +lowatch
               16 ['] @regl  ['] !regl  48 4 s" A4"     +lowatch
               ;

3 cpuid" 68000|CPU32|ColdFire"  \ CPU family string for type 3
3 new-disasm discf              \ disassembler for type 3
3 new-watchloader loadwatchcf   \ initial watch parameters for 68K/CF

previous definitions
[then] decimal



