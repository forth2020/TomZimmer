
\ Disassembler for ECPU Forth processor
\ *** Note: Opcode names are different now. This hasn't been really tested, but it's 
\ *** pretty simple so it shouldn't take much to make it work.
\ *** CPU_ID 6 is reserved for this processor.

\ calignment = # of bytes per cell
\ charsize = bytes per character  ( char = cell )

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ \\\\\\\\\\\\\\\\         ECPU disassembler          \\\\\\\\\\\\\\\\
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

\ Uses global lookup words for displaying labels:
\ .LABEL  ( a -- )
\         Displays the address in hex, unless it can be identified.
\         Used text labels if possible.  Includes a trailing blank.


vocabulary disasmfp
also disasmfp definitions

    0 value >dis                \ -> next byte in the input stream
    0 value offset              \ buffer - physical address

: physical      ( host -- target )      offset - ;
: nextbyte      ( -- c )                >dis c@  1 +to >dis ;

\ names -------------------------------------------------------------

: s,            ( <name> -- )           \ lay down 8-byte name
                pad 20 blank
                bl word count pad swap move
                pad 8 bounds do i c@ c, loop ;

create names    \ 128 possible instructions
s, NOP     s, LAND   s, LXOR   s, MLT    s, L+     s, LC+    s, L+C    s, LC+C
s, NIP     s, AND    s, XOR    s, DROP   s, +      s, C+     s, +C     s, C+C
s, R>DROP  s, R>AND  s, R>XOR  s, DROPR> s, R>+    s, R>C+   s, R>+C   s, R>C+C
s, SWAPAS  s, MLA    s, 2*     s, 2/     s, LIT    s, P@     s, A@     s, ---
s, ---     s, BRA    s, BCC    s, BMS    s, BMI    s, BPL    s, BEQ    s, BNE
s, ---     s, JMP    s, JCC    s, JMS    s, JMI    s, JPL    s, JEQ    s, JNE
s, ---     s, SKIP   s, SCC    s, SMS    s, SMI    s, SPL    s, SEQ    s, SNE
s, FF      s, FT     s, FCC    s, ---    s, FMI    s, FPL    s, FEQ    s, FNE
s, (LIT)   s, DUP    s, UNLIT  s, MTL    s, MRPT   s, SWAPAR s, CALL   s, BSR
s, SWAPAS+ s, SWAPAR+ s, ALA   s, }FILL  s, IDX    s, OVER+  s, R@+    s, MUL
s, MSPT    s, UA!    s, POPL   s, SWAP   s, SWAPR  s, DUP>R  s, RETI   s, RET
s, }@A+    s, }@     s, }@A    s, JMPD   s, CALLD  s, @{     s, !{     s, LIT>R
s, RST0    s, RST1   s, RST2   s, RST3   s, RST4   s, RST5   s, RST6   s, RST7
s, ---     s, ---    s, ---    s, ---    s, ---    s, ---    s, ---    s, ---
s, ---     s, ---    s, ---    s, ---    s, ---    s, ---    s, ---    s, ---
s, ---     s, ---    s, ---    s, ---    s, ---    s, ---    s, ---    s, ---

: .lit          ( c -- )
                base @ hex swap (.) >$ base ! ;

variable litdata
variable litstate
variable litlast

0xA02BAD4C constant magic

: .LITERAL      ( c -- )
                dup 0x40 and
        if      dup 0x20 and 0<> 0xFFFFFFC0 and or litdata !  \ sign extend
                s" S#=" >$   dup 0x20 and if ." -" >$ negate then
                0x3F and .lit
        else    dup 0x3F and litdata @ 6 lshift + litdata !
                s" U#=" >$       0x3F and .lit
        then    litstate on ;

: .NAME         ( c -- )
                litstate @
        if      litdata @ litlast !     \ save last built literal
        then    litstate off
                8 chars * names + 8 -trailing >$ ;

: disfp         ( at ah  -- at' a-addr len )
\ given target address and address of actual data, returns tha next
\ disassemblable address and a string
                calignment to disassyDatawidth
                newout
                litdata off  litstate off  magic litlast !
                dup to >dis     \ set up parameters
                swap - to offset
        begin   nextbyte dup 0x80 and
                if      .literal
                else    .name
                then
                >dis physical calignment mod 0=
        until   litlast @ magic <>
        if      s"   \ L = " >$ litlast @ (.) >$
        then    >dis offset -
                outpad count ;

6 cpuid" E16|E24|E32|"          \ CPU family string for type 6
6 new-disasm disfp              \ disassembler for type 6

previous definitions


