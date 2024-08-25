\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ \\\\\\\\\\\\\\\\         MSL16 disassembler         \\\\\\\\\\\\\\\\
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

\ NOP AND XOR + O= LIT 2/ -            always usable
\ DUP DROP GOTO R> >R @ ! SWAP          can't be 1st opcode

vocabulary dismsl
only forth also dismsl definitions

: .lit          ( n -- )
                base{ s" #" >$ (.) >$ }base
                s"  " >$ ;

variable IR     \ instruction register

: ext8          ( c -- n )
                dup 0x80 and 0<> 0xFF invert and or ;

: ext4          ( c -- n )
                dup 0x08 and 0<> 0x0F invert and or ;

create instrs
   ," NOP AND XOR +   O=  LIT 2/  -   DUP DROPGOTOR>  >R  @   !   SWAP"

: instr         ( n -- n' )
\ disassemble instruction n, n = 3,2,1,0
                1- IR @ over 2* 2* rshift       \ extract instruction nibble
                0x0F and  dup 2* 2* instrs + 1+
                4 -trailing >$ s"  " >$         ( n instr )
                5 =
                if      case 3 of IR @ 4 rshift 0xFF and .lit 1 endof
                             2 of IR @ 0xFF and ext8     .lit 0 endof
                             1 of IR @ 0x0F and ext4     .lit 0 endof
                             0 of 0 endof
                        endcase
                then    ;
                
: disMSL        ( at ah  -- at' a len )
\ given target address and address of actual data, returns the next
\ disassemblable address and a string
                2 to disassyDatawidth
                newout  count >r c@ r> byte-join        \ get 16-bit instr.
                dup 0x8000 and
                if      s" CALL " >$
                        0x7FFF and .label               \ msb=1: call
                else    IR !  4                         \ msb=0: instructions
                        begin   ?dup
                        while   instr
                        repeat
                then
                2 + outpad count ;

2 cpuid" MSL16|"                \ CPU family string for type 2
2 new-disasm disMSL             \ disassembler for type 2

previous definitions


