((
\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
BUILDER lexicon for MSL16 target

Used by Firmware Factory to compile code for the MSL16 VHDL microprocessor

\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
))

only forth also definitions decimal

16 to cellbits

 4 to addrnibbles       \ default to 16-bit addresses
 2 to jsrsize           \ size of long jump required by binding table
16 to charbits          \ 16 bits per character

vocabulary BLD-PRIVATEmsl   BLD-PRIVATEmsl definitions
\ Private part of builder lexicon, not for user use.
hex

variable IR     \ instruction being built
variable nibl   \ next available nibble of the IR     0,1,2,3

: constants     ( beginval #const -- )  bounds ?do i constant loop ;

0 0x10
constants #nop #and #xor #+ #0= #lit #2/ #- #dup #drop #goto #r> #>r #@ #! #swap

: nibbles       ( c -- nl nh )  \ split byte into nibbles
                dup 0x0F and  swap 4 rshift 0x0F and ;

: ,instr        ( n -- )
\ append a native instruction to IR, write to dictionary if IR is full
                0x0F and
                dup 8 and  nibl @ 0= and
                if      nibl incr               \ disallow leading 8..F ops
                then
                3 nibl @ - 2* 2* lshift         \ align nibble
                IR +!  nibl incr  nibl @ 3 >    \ store
                if      IR @ i,
                        nibl off IR off
                then    ;

: instruction   ( n -- )                 create , does> @ ,instr ;
: instructions  ( beginval #instrs -- )  bounds ?do i instruction loop ;

: MustNibl      ( n -- )
\ fill with NOPs until nibl pointer = n
                begin   dup nibl @ <>
                while   #nop ,instr
                repeat  drop ;

: IRflush       ( -- )  0 MustNibl ;
\ fill the rest of the IR with NOPs

: ,lit8         ( n -- )
\ compile 8-bit LIT
                1 mustnibl  #lit ,instr         \ NOP LIT #8bit
                nibbles ,instr ,instr ;

: ,lit16        ( n -- )
\ compile 16-bit LIT
                byte-split swap ,lit8           \ NOP LIT #lowpart
                #lit ,instr nibbles ,instr ,instr ;  \ LIT #hipart

: ,lit          ( n -- )
\ compile code for a literal
\ nibl: 0,1 = allow 8-bit, 2 = allow 4-bit, 3 = no good
                dup>r abs 7F >
                if      r> ,lit16
                else    r@ abs 7 >              \ needs 8 bits
                        nibl @ 2 < or           \ or we're set up to use 8 bits
                        if      r> ,lit8        \ NOP LIT #8bit
                        else    2 mustnibl  #lit ,instr
                                r> ,instr
                        then
                then    ;

: ,call         ( a -- )
\ compile a call
                IRflush 0x8000 or i, ;          \ CALL

: ,exit         ( -- )
\ compile an exit
                #R> ,instr #goto ,instr         \ R> GOTO
                IRflush ;                       \ might terminate CONSTANT, etc.

: ,goto         ( a -- )
\ compile a 16-bit jump
                ,lit16 #goto ,instr ;

variable macstate

: ,macro        ( a -- )
\ copy code at a into dictionary, ret (BA) = terminator
                macstate off  
                begin   ( a )
                        dup i@ 4                 ( a n cnt )
                        begin   dup macstate @ 2 <> and
                        while   1- 2dup 2* 2* rshift
                                0x0F and         ( a n cnt nibl )
                                dup #nop =
                                if      drop
                                else    macstate @       \ last was R>
                                        if      #goto =  \ R>GOTO = done
                                                if      macstate incr
                                                else    #R> ,instr
                                                        macstate off
                                                then
                                        else    dup #R> =
                                                if      drop macstate incr
                                                else    ,instr
                                                then
                                        then
                                then
                        repeat  2drop
                        cellsize +
                        macstate @ 2 =
                until   drop ;


0 value lastheader

:noname         ( -- )
                IRflush
                ihere @ to lastheader   \ point to this header
                0 i, ;                  \ lay down a header
                is newheader

' lastheader    is 'newheader
' IRflush       is ins-align

' ,lit   is compilelit          \ defered words are in the FORTH wordlist
' ,call  is compilecall
' ,macro is compilemacro
' ,exit  is compileexit

:noname         ( -- addr )             \ addr is image location
                1 mustnibl IHERE@  0 ,goto ;        is >MARK

:noname         ( -- addr )
                #0= ,instr
                1 mustnibl IHERE@  0 ,lit16 #and ,instr
                #goto ,instr ;                       is >0MARK

:noname         ( addr -- )
                IRflush
                >r  HERE@ byte-split 4 lshift swap   ( ahi alo | 'dest )
                r@ i@ FF00 and or r@ i! \ modify low part
                r> cellsize + tuck      ( a+2 hi a+2 )
                i@ F00F and or swap i! ;             is >RESOLVE

:noname         ( -- addr )
                IRflush HERE@ ;                      is <MARK

:noname         ( addr -- )
                ,goto ;                              is <RESOLVE

:noname         ( addr -- )
                #0= ,instr 1 mustnibl
                ,lit16 #and ,instr
                #goto ,instr ;                       is <0RESOLVE

false to sys-warning?

also builder (definitions) (previous)

: ||   IRflush ; immediate
: NOP  #nop ,instr ; immediate
: AND  #and ,instr ; immediate   : XOR #xor ,instr ; immediate
: +    #+ ,instr ; immediate     : 0= #0= ,instr ; immediate
: -    #- ,instr ; immediate     : 2/ #2/ ,instr ; immediate
: DUP  #dup ,instr ; immediate   : DROP #drop ,instr ; immediate
: GOTO #goto ,instr ; immediate  : R> #R> ,instr ; immediate
: >R   #>R ,instr ; immediate    : SWAP #swap ,instr ; immediate
: @    #@ ,instr ; immediate     : @P  #@ ,instr ; immediate
: C@   #@ ,instr ; immediate
: !    #! ,instr ; immediate     : !P  #! ,instr ; immediate
: C!   #! ,instr ; immediate

: R@     #R> ,instr  #DUP ,instr  #>R ,instr ; immediate
: DUP>R  #DUP ,instr  #>R ,instr ; immediate
: R>DROP #R> ,instr  #DROP ,instr ; immediate
: CALL   #>R ,instr #GOTO ,instr ; immediate

also assem definitions

\ No code words are used in the MSL16 kernel: this might not have been tested.

: /             ( -- )          IRflush ;
: #             ( <#> -- )      bl word number? abort" Invalid number" ,instr ;
: end-code      ( f -- )        ?condition previous ;
: c;            ( f -- )        ?condition previous ;
: code          ( -- f )
                previous
                true abort" Previous definition missing end-code" ;

0 0x10
instructions NOP AND XOR + 0= LIT 2/ - DUP DROP GOTO R> >R @ ! SWAP

only forth also definitions

true to sys-warning?

\ ========================================================================

   2 to CPUtype                 \ assemble for 8031 processor
   2 to CPUfamily               \ disassemble for 8031 processor
   0 to CPUsubfamily
true to bigendian?              \ MSB in low memory

hex
        0000 1FFF rom-bounds    \ default memory allocation
        8000 FFFF data-bounds
        2000 7FFF code-bounds
decimal

homeorder

