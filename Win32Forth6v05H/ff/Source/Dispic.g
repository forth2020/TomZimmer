\ PIC16 disassembler

hex  vocabulary disasmPIC
also disasmPIC definitions

    0 value dpc
    0 value opcd
    0 value offset              \ add this to dpc to get physical address

: nextword      ( -- n )        dpc offset + w@  2 +to dpc ;

: xxwf          ( a len -- )
                >$ opcd 0FF and 80 /mod swap
                .asmlabel s" ," >$
                if s" F" else s" W" then >$ ;

: xxf           ( a len -- ) >$ opcd 07F and .asmlabel ;
: xxlf          ( a len -- ) >$ opcd 0FF and (.) >$ ;
: xxfb          ( a len -- ) >$ opcd 3FF and 80 /mod swap .asmlabel s" ," >$ (.) >$ ;
: xxad          ( a len -- ) >$ opcd 7FF and .label ;

: dispic1614    ( instruction -- )
\ disassemble PIC16 with 14-bit program words
                3FFF and dup to opcd
        case    0064 of s" CLRWDT" >$   endof
                0008 of s" RETURN" >$   endof
                0009 of s" RETFIE" >$   endof
                0063 of s" SLEEP"  >$   endof
                7 rshift
                00 of s" NOP   " >$     endof
                01 of s" MOVWF " xxf    endof
                02 of s" CLRW  " >$     endof
                03 of s" CLRF  " xxf    endof
                1 rshift
                02 of s" SUBWF " xxwf   endof
                03 of s" DECF  " xxwf   endof
                04 of s" IORWF " xxwf   endof
                05 of s" ANDWF " xxwf   endof
                06 of s" XORWF " xxwf   endof
                07 of s" ADDWF " xxwf   endof
                08 of s" MOVF  " xxwf   endof
                09 of s" COMF  " xxwf   endof
                0A of s" INCF  " xxwf   endof
                0B of s" DECFSZ " xxwf  endof
                0C of s" RRF   " xxwf   endof
                0D of s" RLF   " xxwf   endof
                0E of s" SWAPF " xxwf   endof
                0F of s" INCFSZ " xxwf  endof
                38 of s" IORLW " xxlf   endof
                39 of s" ANDLW " xxlf   endof
                3A of s" XORLW " xxlf   endof
                1 rshift
                1E of s" SUBLW " xxlf   endof
                1F of s" ADDLW " xxlf   endof
                1 rshift
                04 of s" BCF   " xxfb   endof
                05 of s" BSF   " xxfb   endof
                06 of s" BTFSC " xxfb   endof
                07 of s" BTFSS " xxfb   endof
                0C of s" MOVLW " xxlf   endof
                0D of s" RETLW " xxlf   endof
                1 rshift
                04 of s" CALL  " xxad   endof
                05 of s" GOTO  " xxad   endof
                s" <undefined>" >$
        endcase
                ;

: dis           ( inst -- )     dispic1614 ;    \ assume PIC16

: disPIC        ( at ah  -- at' a-addr len )
\ given target address and address of actual data, returns tha next
\ disassemblable address and a string
                4 to disassyDatawidth
                newout
                over to dpc       \ current disassembly pointer (target addr)
                swap - to offset  \ dpc = t   offset = h-t
                nextword dis      \ disassemble
                dpc
                outpad count ;

8 cpuid" PIC16"                  \ CPU family string for type 8 = AVR
8 new-disasm disPIC
decimal
previous definitions


