\ 8051 low level support

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ \\\\\\\\\\\\\\\\         8031 disassembler          \\\\\\\\\\\\\\\\
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

\ Uses global lookup words for displaying labels:
\ .LABEL  ( a -- )
\         Displays the address in hex, unless it can be identified.
\         Used text labels if possible.  Includes a trailing blank.


vocabulary disasm51
also disasm51 definitions

    0 value >dis                \ -> next byte in the input stream
    0 value offset              \ buffer - physical address
    0 value op                  \ 0..15 hi nibble of instruction

: physical      ( host -- target )
\ translate address
                offset - ;

: nextbyte      ( -- c )        >dis c@  1 +to >dis ;
: nextword      ( -- n )        nextbyte 8 lshift nextbyte + ;
: .displacement ( -- )          nextbyte c>n >dis + physical .label ;

: .#            ( -- )          s" #" >$ ;
: (.immed)      ( -- )          .# 2 (h.) >$ ;
: (.immed16)    ( -- )          .# 4 (h.) >$ ;
: .immediate    ( -- )          nextbyte (.immed) ;
: .immediate16  ( -- )          nextword (.immed16) ;

create SFR_table                \ 8052 SFRs
\ 128-string table
 z," P0  SP  DPL DPH             PCON"    \ 80
+z," TCONTMODTL0 TL1 TH0 TH1         "    \ 88
+z," P1                              "    \ 90
+z," SCONSBUF                        "    \ 98
+z," P2                              "    \ A0
+z," IE                              "    \ A8
+z," P3                              "    \ B0
+z," IP                              "    \ B8
+z,"                                 "    \ C0
+z," T2ON    RC2LRC2HCL2#CH2#        "    \ C8
+z," PSW                             "    \ D0
+z,"                                 "    \ D8
+z," ACC                             "    \ E0
+z,"                                 "    \ E8
+z," B                               "    \ F0
+z,"                                 "    \ F8

create BIT_table                \ 8052 bit addresses
\ 128-string table
 z," P0.0P0.1P0.2P0.3P0.4P0.5P0.6P0.7"    \ P0
+z," IT0 IE0 IT1 IE1 TR0 TF0 TR1 TF1 "    \ TCON
+z," P1.0P1.1P1.2P1.3P1.4P1.5P1.6P1.7"    \ P1
+z," RI  TI  RB8 TB8 REN SM2 SM1 SM0 "    \ SCON
+z," P2.0P2.1P2.2P2.3P2.4P2.5P2.6P2.7"    \ P2
+z," EX0 ET0 EX1 ET1 ES  ET2     EA  "    \ IE
+z," P3.0P3.1P3.2P3.3P3.4P3.5P3.6P3.7"    \ P3
+z," PX0 PT0 PX1 PT1 PS  PT2         "    \ IP
+z,"                                 "    \ $C0
+z," CPR2C/T2TR2 EXE2TCLKRCLKEXF2TF2 "    \ T2ON
+z," P       OV  RS0 RS1 F0  AC  CY  "    \ PSW
+z,"                                 "    \ $D8
+z," AC.0AC.1AC.2AC.3AC.4AC.5AC.6AC.7"    \ ACC
+z,"                                 "    \ $E8
+z," B.0 B.1 B.2 B.3 B.4 B.5 B.6 B.7 "    \ B
+z,"                                 "    \ $F8

   0 value label8

: cannedlabel?  ( n a -- a len f )
\ converts n to a string, returns F if it's not defined in the table
                over 0x80 and
                if      swap 0x7F and 4 * +  4 \ 4-char substring ( a len )
                        -trailing
                        dup 0<>
                else    drop 0 0
                then    ;

: (.label8)     ( n a -- )
                over to label8
                cannedlabel?             \ look up label
                if      >$
                else    2drop            \ not found, display in raw form
                        label8 .asmlabel \ try to display user label
                then    ;

: .bit          ( -- )   nextbyte 0x100 + BIT_table (.label8) ;
: (.direct)     ( n -- )                  SFR_table (.label8) s"  " >$ ;
: .direct       ( -- )   nextbyte  (.direct) ;


\ Opcode decoding

: createname    ( <name> -- )
                pad 32 blank
                >in @
                create  >in !           \ set up to re-parse the name
                bl word count 1 /string \ skip leading dot ( a n )
                >r pad r@ move          \ copy it to pad
                r@ c,                   \ store length
                pad r> 16 max bounds
                ?do     i c@ c,         \ store string
                loop    align ;

: LA            ( <name> -- | -- )
\ Create a word whose action is to append a cleaned-up version of its name
\ to the output stream
                createname
                does>   count 6 max >$ ;

: OPN           ( <name> -- | -- )
\ Like LA but not a fixed field width.  Includes a trailing blank.
                createname
                does>   count 1+ >$ ;

        LA .ACALL       LA .ADD         LA .ADDC        LA .AJMP
        LA .ANL         LA .CJNE        LA .CLR         LA .CPL
        LA .DA          LA .DEC         LA .DIV         LA .DJNZ
        LA .INC         LA .JB          LA .JBC         LA .JC
        LA .JMP         LA .JNB         LA .JNC         LA .JNZ
        LA .JZ          LA .LCALL       LA .LJMP        LA .MOV
        LA .MOVC        LA .MOVX        LA .MUL         LA .NOP
        LA .ORL         LA .POP         LA .PUSH        LA .RET
        LA .RETI        LA .RL          LA .RLC         LA .RR
        LA .RRC         LA .SETB        LA .SJMP        LA .SUBB
        LA .SWAP        LA .XCH         LA .XCHD        LA .XRL
        LA .RESERVED

        OPN .A          OPN .A,         OPN .AB         OPN .,
        OPN .C          OPN .C,         OPN .@A+DPTR    OPN .@A+PC
        OPN .DPTR       OPN .DPTR,      OPN .@DPTR      OPN .@DPTR,
        OPN .@R0        OPN .@R0,       OPN .@R1        OPN .@R1,
        OPN .R0         OPN .R1         OPN .R2         OPN .R3
        OPN .R4         OPN .R5         OPN .R6         OPN .R7

: .JBC:         ( -- )  .JBC  .bit ., .displacement ;
: .JB:          ( -- )  .JB   .bit ., .displacement ;
: .JNB:         ( -- )  .JNB  .bit ., .displacement ;
: .JC:          ( -- )  .JC   .displacement ;
: .JNC:         ( -- )  .JNC  .displacement ;
: .JZ:          ( -- )  .JZ   .displacement ;
: .JNZ:         ( -- )  .JNZ  .displacement ;
: .SJMP:        ( -- )  .SJMP .displacement ;

: .MOV_DP#      ( -- )  .MOV  .dptr, .immediate16 ;
: .OA_C/B       ( -- )  .C,  s" /" >$ .bit ;
: .ORL_C/B      ( -- )  .ORL  .OA_C/B ;
: .ANL_C/B      ( -- )  .ANL  .OA_C/B ;
: .MOVXA@D      ( -- )  .MOVX .A, .@DPTR ;
: .MOVX@DA      ( -- )  .MOVX .@DPTR, .A ;
: .PUSH:        ( -- )  .PUSH .direct ;
: .POP:         ( -- )  .POP  .direct ;

: do-op         ( n a -- )
                swap 0x000F and th @ execute ;

create 'opcode0 ( -- a )
        ' .NOP ,     ' .JBC: ,    ' .JB: ,     ' .JNB: ,
        ' .JC: ,     ' .JNC: ,    ' .JZ: ,     ' .JNZ: ,
        ' .SJMP: ,   ' .MOV_DP# , ' .ORL_C/B , ' .ANL_C/B ,
        ' .PUSH: ,   ' .POP: ,    ' .MOVXA@D , ' .MOVX@DA ,

: opcode0       ( n -- )  'opcode0 do-op ;

: opcode1       ( n -- )
        dup 1 and  if .ACALL else .AJMP then    \ acall or ajmp
        u2/ nextbyte swap 8 lshift or           \  addr11
        >dis physical 0xF800 and or .LABEL ;    \  dest.16

: .LCALL:       ( -- )  .LCALL nextword .label ;
: .LJMP:        ( -- )  .LJMP  nextword .label ;
: oax_da        ( -- )  .direct ., .A ;
: .ORL_DA       ( -- )  .ORL    oax_da ;
: .ANL_DA       ( -- )  .ANL    oax_da ;
: .XRL_DA       ( -- )  .XRL    oax_da ;
: oax_cb        ( -- )  .C,     .bit ;
: .ORL_CB       ( -- )  .ORL    oax_cb ;
: .ANL_CB       ( -- )  .ANL    oax_cb ;
: .MOV_CB       ( -- )  .MOV    oax_cb ;
: .MOV_BC       ( -- )  .MOV    .bit ., .C ;
: .CPL_B        ( -- )  .CPL    .bit ;
: .CLR_B        ( -- )  .CLR    .bit ;
: .SETB_B       ( -- )  .SETB   .bit ;
: .MOVXA@0      ( -- )  .MOVX   .A, .@R0 ;
: .MOVXA@1      ( -- )  .MOVX   .A, .@R1 ;
: .MOVX0@A      ( -- )  .MOVX   .@R0, .A ;
: .MOVX1@A      ( -- )  .MOVX   .@R1, .A ;

create 'opcode2 ( -- a )
        ' .LJMP: ,   ' .LCALL: ,  ' .RET ,     ' .RETI ,
        ' .ORL_DA ,  ' .ANL_DA ,  ' .XRL_DA ,  ' .ORL_CB ,
        ' .ANL_CB ,  ' .MOV_BC ,  ' .MOV_CB ,  ' .CPL_B ,
        ' .CLR_B ,   ' .SETB_B ,  ' .MOVXA@0 , ' .MOVX0@A ,

: .RR_A         ( -- )  .RR  .A ;
: .RRC_A        ( -- )  .RRC .A ;
: .RL_A         ( -- )  .RL  .A ;
: .RLC_A        ( -- )  .RLC .A ;
: oax_d#        ( -- )  .direct ., .immediate ;
: .ORL_D#       ( -- )  .ORL oax_d# ;
: .ANL_D#       ( -- )  .ANL oax_d# ;
: .XRL_D#       ( -- )  .XRL oax_d# ;
: .JMP_A+D      ( -- )  .JMP .@a+dptr ;
: .MOVC_AP      ( -- )  .MOVC .a, .@a+pc ;
: .MOVC_AD      ( -- )  .MOVC .a, .@a+dptr ;
: .INC_DP       ( -- )  .INC .dptr ;
: .CPL_C        ( -- )  .CPL  .C ;
: .CLR_C        ( -- )  .CLR  .C ;
: .SETB_C       ( -- )  .SETB .C ;

create 'opcode3 ( -- a )
        ' .RR_A ,    ' .RRC_A ,   ' .RL_A ,    ' .RLC_A ,
        ' .ORL_D# ,  ' .ANL_D# ,  ' .XRL_D# ,  ' .JMP_A+D ,
        ' .MOVC_AP , ' .MOVC_AD , ' .INC_DP ,  ' .CPL_C ,
        ' .CLR_C ,   ' .SETB_C ,  ' .MOVXA@1 , ' .MOVX1@A ,

: .INC_A        ( -- )  .INC .A ;
: .DEC_A        ( -- )  .DEC .A ;
: a,immed       ( -- )  .A,  .immediate ;
: .ADD_A#       ( -- )  .ADD a,immed ;
: .ADDC_A#      ( -- )  .ADDC a,immed ;
: .ORL_A#       ( -- )  .ORL a,immed ;
: .ANL_A#       ( -- )  .ANL a,immed ;
: .XRL_A#       ( -- )  .XRL a,immed ;
: .MOV_A#       ( -- )  .MOV a,immed ;
: .SUBB_A#      ( -- )  .SUBB a,immed ;
: .DIV_AB       ( -- )  .DIV .AB ;
: .MUL_AB       ( -- )  .MUL .AB ;
: .SWAP_A       ( -- )  .SWAP .A ;
: .DA_A         ( -- )  .DA   .A ;
: .CLR_A        ( -- )  .CLR .A ;
: .CPL_A        ( -- )  .CPL .A ;
: .CJNE_A#      ( -- )  .CJNE a,immed ., .displacement ;

create 'opcode4 ( -- a )
        ' .INC_A ,   ' .DEC_A ,   ' .ADD_A# ,  ' .ADDC_A# ,
        ' .ORL_A# ,  ' .ANL_A# ,  ' .XRL_A# ,  ' .MOV_A# ,
        ' .DIV_AB ,  ' .SUBB_A# , ' .MUL_AB ,  ' .CJNE_A# ,
        ' .SWAP_A ,  ' .DA_A ,    ' .CLR_A ,   ' .CPL_A ,

: opcode2       ( n -- )  'opcode2 do-op ;
: opcode3       ( n -- )  'opcode3 do-op ;
: opcode4       ( n -- )  'opcode4 do-op ;

: .ADD_A        ( -- )  .ADD  .A, ;
: .ADDC_A       ( -- )  .ADDC .A, ;
: .SUBB_A       ( -- )  .SUBB .A, ;
: .ORL_A        ( -- )  .ORL  .A, ;
: .ANL_A        ( -- )  .ANL  .A, ;
: .XRL_A        ( -- )  .XRL  .A, ;
: .XCH_A        ( -- )  .XCH  .A, ;
: .MOV_A        ( -- )  .MOV  .A, ;
: .MOVD_        ( -- )  .MOV .direct ., ;
: .,A           ( -- )  backspace ., .A ;
: .,direct      ( -- )  backspace ., .direct ;
: .,immed       ( -- )  backspace ., .immediate ;
: .,cjne        ( -- )  backspace ., .immediate ., .displacement ;
: .,djnz        ( -- )  backspace ., .displacement ;

create 'names0
\ name instructions for hi-nibbles 0..15
        ' .INC   ,       ' .DEC    ,      ' .ADD_A   ,     ' .ADDC_A  ,
        ' .ORL_A ,       ' .ANL_A  ,      ' .XRL_A   ,     ' .MOV     ,
        ' .MOVD_ ,       ' .SUBB_A ,      ' .MOV     ,     ' .CJNE    ,
        ' .XCH_A ,       ' .DJNZ   ,      ' .MOV_A   ,     ' .MOV     ,

create 'names1
        ' noop   ,       ' noop    ,      ' noop     ,     ' noop     ,
        ' noop   ,       ' noop    ,      ' noop     ,     ' .,immed  ,
        ' noop   ,       ' noop    ,      ' .,direct ,     ' .,cjne   ,
        ' noop   ,       ' .,djnz  ,      ' noop     ,     ' .,A      ,

: .opname       ( n -- )  'names0 do-op ;
: .opname1      ( -- )    op 'names1 do-op ;

: opcode5       ( n -- )  .opname .direct .opname1 ;
: opcode6       ( n -- )  .opname .@r0    .opname1 ;
: opcode7       ( n -- )  .opname .@r1    .opname1 ;
: opcode8       ( n -- )  .opname .r0     .opname1 ;
: opcode9       ( n -- )  .opname .r1     .opname1 ;
: opcode10      ( n -- )  .opname .r2     .opname1 ;
: opcode11      ( n -- )  .opname .r3     .opname1 ;
: opcode12      ( n -- )  .opname .r4     .opname1 ;
: opcode13      ( n -- )  .opname .r5     .opname1 ;
: opcode14      ( n -- )  .opname .r6     .opname1 ;
: opcode15      ( n -- )  .opname .r7     .opname1 ;

create 'opcodes ( -- a )
\ list of actions based on the low nibble
        ' opcode0 ,  ' opcode1 ,  ' opcode2 ,  ' opcode3 ,
        ' opcode4 ,  ' opcode5 ,  ' opcode6 ,  ' opcode7 ,
        ' opcode8 ,  ' opcode9 ,  ' opcode10 , ' opcode11 ,
        ' opcode12 , ' opcode13 , ' opcode14 , ' opcode15 ,

: .MOV_DD       ( -- )  .MOV nextbyte nextbyte (.direct) ., (.direct) ;

: .XCHD@R0      ( -- )  .XCHD .A, .@R0 ;
: .XCHD@R1      ( -- )  .XCHD .A, .@R1 ;

: instruction   ( -- )
\ disassemble instruction c, feed 4-bit operand to 1 of 16 opcodes
                nextbyte
                case    0x85 of .MOV_DD         endof   \ handle 'bumps'
                        0xA5 of .RESERVED       endof
                        0xD6 of .XCHD@R0        endof
                        0xD7 of .XCHD@R1        endof
                        16 /mod swap                    \ decode opcode
                        over to op
                        'opcodes swap th @ execute
              0 endcase ;

: dis8051       ( at ah  -- at' a-addr len )
\ given target address and address of actual data, returns tha next
\ disassemblable address and a string
                3 to disassyDatawidth
                newout
                dup to >dis     \ set up parameters
                swap - to offset
                instruction     \ do the deed
                >dis offset -
                outpad count ;

\ machine watch window support -------------------------------------------

: 'spec         ( a -- atar )   b.special @ + ;

: @specialc     ( a -- c )      'spec @regc ;
: @special      ( a -- n )      'spec @regw ;
: !specialc     ( c a -- )      'spec !regc ;
: !special      ( n a -- )      'spec !regw ;

: loadwatch51   ( -- )
                3 ['] @specialc ['] !specialc    12 1 s" SP'"    +lowatch
                4 ['] @specialc ['] !specialc     0 1 s" A'"     +lowatch
                5 ['] @specialc ['] !specialc     1 1 s" PSW'"   +lowatch
                6 ['] @special  ['] !special      2 2 s" DPTR'"  +lowatch
                7 ['] @specialc ['] !specialc     4 1 s" R0'>"   +lowatch
                8 ['] @specialc ['] !specialc     5 1 s" R1'"    +lowatch
                9 ['] @specialc ['] !specialc     6 1 s" R2'"    +lowatch
               10 ['] @specialc ['] !specialc     7 1 s" R3'"    +lowatch
               11 ['] @specialc ['] !specialc     8 1 s" R4'"    +lowatch
               12 ['] @specialc ['] !specialc     9 1 s" R5'"    +lowatch
               13 ['] @specialc ['] !specialc    10 1 s" R6'"    +lowatch
               14 ['] @specialc ['] !specialc    11 1 s" R7'"    +lowatch
               ;

1 cpuid" 8051|8052|80552|"      \ CPU family string for type 1
1 new-disasm dis8051            \ disassembler for type 1
1 new-watchloader loadwatch51   \ initial watch parameters for 8051

previous definitions



