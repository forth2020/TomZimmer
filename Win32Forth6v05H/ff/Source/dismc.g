\ Motorola M-Core Disassembler      BNE (work in progress)

\ anew disassemble_this
hex
vocabulary mcoredis
also mcoredis definitions

0 constant standalone?          \ want to compile for standalone testing?

standalone? [if]
      0 value   #col                                    \ current console column
      : >$      ( a-addr len -- )  tuck type +to #col ; \ console output
      : .label  ( t-address -- )   hex s" 0x" >$ (.) type ; \ address with label
      : tab>$   ( n -- )        #col - 0max spaces 0 to #col ;
[then]

defer >>        ( -- 16-bits )          ' false is >>   \ data input
0 value         DPC                                     \ returns base address
0 value         operand                                 \ operand storage
0 value         size                                    \ default operand size
8 constant      opcolumn                                \ operands print here

: c>$           ( c -- )        pad c! pad 1 >$ ;       \ char to console
: optab         ( -- )          opcolumn tab>$ ;        \ tab to operand column
: >$>           ( a n -- )      >$ optab ;              \ type and tab

\ basic display stuff ---------------------------------------------------------
: .hex          ( n -- )        hex (.) >$ ;
: .[ '(' c>$ ;  : ., ',' c>$ ;  : .# '#' c>$ ;  : .] ')' c>$ ;
: .reg          ( n -- )        'r' c>$ 0x0F and decimal (.) >$ ;
: .regz         ( -- )          operand 8 rshift .reg ;

: .disp8        ( a n -- )              \ " ADDR/LABEL "
                operand 0xFF and 2* 2 +
                dpc + .label ;          \ zero-extended 8-bit displacement

: .d8           ( a n -- )  >$> .disp8 ;

: .d11          ( a n -- )              \ " ADDR/LABEL "
                >$>
                operand 0x07FF and  dup 0x400 and 0<> 0xFFFFF800 and or
                dpc + optab .label ;    \ sign-extended 11-bit displacement

: .zir          ( a n -- )              \ " RZ,(RX,DISP) "
                >$ optab .regz .,
                operand 0xFF and 0x10 /mod  ( x i )
                .[ swap .reg dup
        if      ., decimal (.) >$
        else    drop
        then    .] ;

: .rx           ( a n -- )      >$> operand .reg ;
: _.sr          ( a n -- )      >$> operand 0x10 /mod swap .reg ., 0x1F and ;
: .sr           ( a n -- )      _.sr .reg ;
: .sbran        ( a n -- )      _.sr 2* dpc + .hex ;
: .cr           ( a n -- )      _.sr s" cr" >$ decimal (.) >$ ;
: .ir           ( a n -- )      _.sr 1+ .# .hex ;

: instruction   ( -- )
\ decode opcode
                >> to operand  base @
                operand
        case    0000 of s" bkpt" >$     endof   \ NNNNNNNNNNNNNNNN
                0001 of s" sync" >$     endof
                0002 of s" rte " >$     endof
                0003 of s" rfi " >$     endof
                0004 of s" stop" >$     endof
                0005 of s" wait" >$     endof
                0006 of s" doze" >$     endof
                2 rshift                        \ NNNNNNNNNNNNNNxx
                0002 of s" trap" >$> operand 3 and .hex endof
                2 rshift                        \ NNNNNNNNNNNNxxxx
                0001 of s" mvc  " .rx    endof
                0003 of s" mvcv " .rx    endof
                0004 of s" ldq  " .rx    endof
                0005 of s" stq  " .rx    endof
                0006 of s" ldm  " .rx    endof
                0007 of s" stm  " .rx    endof
                0008 of s" dect " .rx    endof
                0009 of s" decf " .rx    endof
                000A of s" inct " .rx    endof
                000B of s" incf " .rx    endof
                000C of s" jmp  " .rx    endof
                000D of s" jsr  " .rx    endof
                000E of s" ff1  " .rx    endof
                000F of s" brev " .rx    endof
                0010 of s" xtrb3" .rx    endof
                0011 of s" xtrb2" .rx    endof
                0012 of s" xtrb1" .rx    endof
                0013 of s" xtrb0" .rx    endof
                0014 of s" zextb" .rx    endof
                0015 of s" sextb" .rx    endof
                0016 of s" zexth" .rx    endof
                0017 of s" sexth" .rx    endof
                0018 of s" declt" .rx    endof
                0019 of s" tstnbz" .rx   endof
                001A of s" decgt" .rx    endof
                001B of s" decne" .rx    endof
                001C of s" clrt " .rx    endof
                001D of s" clrf " .rx    endof
                001E of s" abs  " .rx    endof
                001F of s" not  " .rx    endof
                02C0 of s" bmaski" >$> s" #32" >$ ., .rx endof
                02C1 of s" divu " .rx    endof
                0321 of s" divs " .rx    endof
                0380 of s" xsr  " .rx    endof
                03A0 of s" asrc " .rx    endof
                03C0 of s" lslc " .rx    endof
                03E0 of s" lsrc " .rx    endof
                4 rshift                        \ NNNNNNNNxxxxxxxx
                0002 of s" movt " .sr    endof
                0003 of s" mult " .sr    endof
                0004 of s" loopt" .sbran endof
                0005 of s" subu " .sr    endof
                0006 of s" addc " .sr    endof
                0007 of s" subc " .sr    endof
                000A of s" movf " .sr    endof
                000B of s" lsr  " .sr    endof
                000C of s" cmphs" .sr    endof
                000D of s" cmplt" .sr    endof
                000E of s" tst  " .sr    endof
                000F of s" cmpne" .sr    endof
                0012 of s" mov  " .sr    endof
                0013 of s" bengr" .sr    endof
                0014 of s" rsub " .sr    endof
                0015 of s" isw  " .sr    endof
                0016 of s" and  " .sr    endof
                0017 of s" xor  " .sr    endof
                001A of s" asr  " .sr    endof
                001B of s" lsl  " .sr    endof
                001C of s" addu " .sr    endof
                001D of s" ixh  " .sr    endof
                001E of s" or   " .sr    endof
                001F of s" andn " .sr    endof
                0070 of s" jmpi " .d8    endof
                007F of s" jsri " .d8    endof
                1 rshift                        \ NNNNNNNxxxxxxxxx
                0008 of s" mfcr " .cr    endof
                000C of s" mtcr " .cr    endof
                0010 of s" addi " .ir    endof
                0011 of s" cmplti" .ir   endof
                0012 of s" subi " .ir    endof
                0014 of s" rsubi" .ir    endof
                0015 of s" cmpnei" .ir   endof
                0016 of s" bmaski" .ir   endof
                0017 of s" andi " .ir    endof
                0018 of s" clri " .ir    endof
                0019 of s" bgeni" .ir    endof
                001A of s" bseti" .ir    endof
                001B of s" btsti" .ir    endof
                001C of s" rotli" .ir    endof
                001D of s" asri " .ir    endof
                001E of s" lsli " .ir    endof
                001F of s" lsri " .ir    endof
                2 rshift                        \ NNNNNxxxxxxxxxxx
                000C of s" movi " .rx .,
                        operand 4 rshift 7F and .# .hex endof
                001C of s" bt   " .d11   endof
                001D of s" bf   " .d11   endof
                001E of s" br   " .d11   endof
                001F of s" bsr  " .d11   endof
                1 rshift                        \ NNNNxxxxxxxxxxxx
                0007 of s" lrw  " >$> .regz ., operand 0FF and
                        if .disp8 else >> >> swap word-join .label
                        then             endof
                0008 of s" ld   " .zir   endof
                0009 of s" st   " .zir   endof
                000A of s" ld.b " .zir   endof
                000B of s" st.b " .zir   endof
                000C of s" ld.h " .zir   endof
                000D of s" st.h " .zir   endof
                s" unknown" >$
        endcase base ! ;

\ M-Core subroutine threading:
\ BSR instruction has +/- 2K range
\ JSR uses an index register

\ To call long, use LRW R14,#0 instruction followed by 32-bit address
\ then use JSR R14.  Most low level kernel words will use BSRs.

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

: dismc         ( at ah  -- at' a-addr len )
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

5 cpuid" M-core"                \ CPU family string for type 5
5 new-disasm dismc              \ disassembler for type 5

previous definitions
[then] decimal



