\ Okay, I started a builder for the PIC.  The disassembler was trivial.  Maybe I'll 
\ finish this someday but probably not.  AVRs beat PICs usually.  -- BNE

\ PIC assembler

create operands 40 allot

: a,            ( n -- ) dup h. ." --> " dispic1614 ;

: @@            ( -- )          \ get operands from input stream
                bl word count operands place ;

: op0           ( -- a )        \ extract first operand to pad, return addr of pad
                operands count ',' scan drop    \ scan to ',' or ' '  ( a' )
                operands char+ tuck - pad place  pad ;

: op1           ( -- a )        \ extract operand after ',' to pad
                operands count ',' scan 1 /string bl skip
                dup 0= abort" Missing comma or 2nd operand"
                pad place  pad ;

((
: alit          ( a -- n )      \ translate string to number for literal
\ precedence: assembly-label, number, @@?, code-address
                count pad place
                pad label>n  dup bogusvalue =           \ not assembly address?
        if      drop pad count number? nip 0=           \ try number
                if      drop pad getlabel   dup -1 =    \ try address of word
                        if      pad count 1- s" @@" compare
                                if      ."  ??"
                                        true abort"  Unknown label/immediate data"
                                else    pad 3 chars + c@ '0' -
                                        dup 0 9 between \ local label
                                        0=   abort"  Unknown label"
                                        qasmlabs@ nip
                                then
                        then
                then
        then    ;
))
false to sys-warning?
vocabulary bits
: drf           ( a -- n )      \ translate string to direction flag
                >r get-order only bits
                r> find 2>r set-order 2r>
        if      execute exit
        then    true abort" Invalid direction name, expecting 0,1,W,F" ;
true to sys-warning?

also bits definitions
00 constant 0   00 constant W
80 constant 1   80 constant F
bits definitions

: alit  ( a -- n )  number drop ;

: getf          ( -- n )   @@ op0 alit 07F and ;
: bit           ( a -- n )
                count number? 0= nip over -8 and or abort" expecting bit 0..7" ;

: ^op   create , does> @ a, ;                              \ implicit
: ^k    create , does> @ @@ operands alit 0FF and or a, ;  \ literal with W
: ^f    create , does> @ @@ operands alit 07F and or a, ;  \ f only
: ^fd   create , does> @ getf op1 drf or or a, ;           \ f,d
: ^fb   create , does> @ getf op1 bit 7 lshift or or a, ;  \ f,b
: ^jp   create , does> @ @@ operands alit 07FF and or a, ; \ jump

: MOVFF         ( -- )  getf 0800 or a,                    \ MOVF  f0,w
                        op1 alit 07F and 0080 or a, ;      \ MOVWF f1
: MOVLF         ( -- )  @@ op0 alit 0FF and 3000 or a,     \ MOVLF K
                        op1 alit 07F and 0080 or a, ;      \ MOVWF f1

3E00 ^k  ADDLW  0700 ^fd ADDWF  3900 ^k  ANDLW  0500 ^fd ANDWF
1000 ^fb BCF    1400 ^fb BSF    1800 ^fb BTFSC  1C00 ^fb BTFSS
2000 ^jp CALL   0180 ^f  CLRF   0103 ^op CLRW   0064 ^op CLRWDT
0900 ^fd COMF   0300 ^fd DECF   0B00 ^fd DECFSZ 2800 ^jp GOTO
0A00 ^fd INCF   0F00 ^fd INCFSZ 3800 ^k  IORLW  0400 ^fd IORWF
0800 ^fd MOVF   3000 ^k  MOVLW  0080 ^f  MOVWF  0000 ^op NOP
0009 ^op RETFIE 3400 ^k  RETLW  0008 ^op RETURN 0D00 ^fd RLF
0C00 ^fd RRF    0063 ^op SLEEP  3C00 ^k  SUBLW  0200 ^fd SUBWF
0E00 ^fd SWAPF  3A00 ^k  XORLW  0600 ^fd XORWF


((
        Virtual return stack: RP is a register.
        >R      movff indf,d0
                incf  fsr,f
                movff fsr,d1
                movff rp,fsr
                incf  fsr,f
                incf  rp,f
                movff d0,indf
                movff d1,fsr

code DUP        movf  indf,w
                decf  fsr,f
                movwf indf
                return

code 1+         incf  indf,f
                return          macro

code OVER       incf  fsr,f
                movf  indf,w
                decf  fsr,f
                decf  fsr,f
                movwf indf
                return

literal         decf  fsr,f
                movlw N
                movwf indf
))

