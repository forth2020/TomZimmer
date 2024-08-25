((
\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
BUILDER lexicon for 486 target

Segments                Register Usage
--------------------    -------------------
Code space uses CS      EAX = top of data stack
Data space uses DS      EBX = data stack pointer
Return stack uses SS    ESP = return stack pointer
Data stack uses ES

\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
))

only forth also definitions decimal

  32 to cellbits

   6 to addrnibbles     \ default to 16-bit addresses
   5 to jsrsize         \ size of long jump required by binding table
   8 to charbits        \ 8 bits per character
false to bigendian?

vocabulary BLD-PRIVATE86   BLD-PRIVATE86 definitions
\ Private part of builder lexicon
octal                   \ Octal instructions -- a relic from the 8080 days

  variable opto         \ peephole optimization state
 303 value #ret         \ value of return instruction

: discard       ( n-- ) ihere@ swap - ihere! ;
: displacement  ( a -- disp ) ihere@ - 5 - ;
: DUP0          ( -- )  211 ic, 007 ic, ;               \ MOV [BX],AX
: DUP1          ( -- )  113 ic, 113 ic,  1 opto ! ;     \ DEC BX  DEC BX
: ,DUP          ( -- )  DUP0 DUP1 ;

: _DROP0        ( -- )  103 ic, 103 ic, ;               \ INC BX  INC BX
: DROP1         ( -- )  213 ic, 007 ic, ;               \ MOV AX,[BX]
: DROP0         ( -- )
                opto @ 1 =
                if      2 discard                       \ optimize away DEC|INC
                else    _DROP0
                then    opto off ;

: ,DROP         ( -- )  DROP0 DROP1 ;

: ,lit          ( n -- )
\ compile code for a 32-bit literal
                DUP0  273 ic, i,
                DUP1 ;

variable lastcall

: ,call         ( a -- )
\ compile a call, always use far.
                dup lastcall !
                displacement
                350 ic, i,  2 opto ! ;

: ,ret          ( -- )
\ compile a return
                #ret ic,  opto off ;

: ,goto         ( a -- )
\ compile the most compact jump
                displacement  dup abs 0x7D <
        if      353 ic, 3 - ic,         \ short
        else    351 ic, i,              \ normal
        then    opto off ;

: ,exit         ( -- )
\ convert last call to jmp if allowed, otherwise compile ret
                opto @ 2 =
                if      5 discard  lastcall @ ,goto
                else    ,ret
                then    opto off
                ;

: ,macro        ( a -- )
\ copy code at a into dictionary, ret = terminator
                begin   count dup #ret <>
                while   ic,
                repeat  2drop
                opto off ;

0 value lastheader

:noname         ( -- )
                ihere @ to lastheader   \ point to this header
                0 ic,  opto off ;       \ lay down a header
                is newheader

' lastheader    is 'newheader

' ,lit   is compilelit          \ defered words are in the FORTH wordlist
' ,call  is compilecall
' ,macro is compilemacro
' ,exit  is compileexit

: ,(if)         ( -- )
                drop0
                002 ic, 300 ic,         \ AND AX,AX
                drop1
                265 ic, 003 ic,         \ JNZ +3
                ;

:noname         ( -- addr )             \ addr is image location
                351 ic, IHERE@  0 i,  opto off ;     is >MARK

:noname         ( -- addr )
                ,(if) 351 ic, IHERE@ 0 i, opto off ; is >0MARK

:noname         ( addr -- )
                >r IHERE@ r@ - byte-split swap
                r@ ic! r> 1+ ic! opto off ;          is >RESOLVE

:noname         ( -- addr )
                IHERE@ ;                             is <MARK

:noname         ( addr -- )
                ,goto ;                              is <RESOLVE

:noname         ( addr -- )
                ,(if) ,goto ;                        is <0RESOLVE

false to sys-warning?  warning off
also assembler also asm-hidden

also forth definitions

: end-code86 ( end a code definition )
        end-asm ?finished ?unres ;

: mymacro       ( <name> -- )
\ create a word that lays down the bytes compiled between ASSEMBLE and here
                current @ >r
                also assembler definitions
        create  end-code86
                ihere@ assemlast @  2dup -
                dup 1 100 between 0= abort" Invalid macro structure"
                c,                      \ save the count  ( hi lo )
                ?do     i 'image c@ c,  \ lay down the data
                loop
                previous
                r> current !
                assemlast @ ihere!      \ remove macro code from dictionary
        does>   >r a; r>                \ finish assembly of last instruction
                count bounds            \ lay down the macro string
                ?do     i c@ ic,
                loop    ;

: i+! dup>r i@ + r> i! ;

'    ic, is     code-c, ( x -- )
'    iw, is     code-w, ( x -- )
'     i, is     code-d, ( x -- )
'      , is      data-, ( x -- )
'    ic! is     code-c! ( x \ a -- )
'    iw! is     code-w! ( x \ a -- )
'     i! is     code-d! ( x \ a -- )
'      ! is      data-! ( x \ a -- )
'    i+! is     data-+! ( x \ a -- )
'    ic@ is     code-c@ ( a -- x )
'    iw@ is     code-w@ ( a -- x )
'     i@ is     code-d@ ( a -- x )
'      @ is      data-@ ( a -- x )
'   here is   data-here ( -- a )
' ihere@ is   code-here ( -- a )
'   noop is  code-align ( -- )
'   noop is code-header ( -- )

assembler definitions previous

: end-code      end-code86 ;
: c;            end-code86 ;
: macro:        mymacro ;

also builder (definitions) (previous)

: code          ( <name> -- f )
\ Begin a code definition
                0 makeheader init-asm ;

: assemble      ( -- f )
\ enter assembler mode
                ihere@ assemlast ! init-asm ;

: loco          ( <name> -- f )
\ Begin a code definition with no header
                ihere@ asmlabel init-asm ;


only forth also definitions

true to sys-warning?  warning on

\ ========================================================================

   4 to CPUtype                 \ assemble for 8086 processor
   4 to CPUfamily               \ disassemble for 8086 processor
   1 to CPUsubfamily

hex
        0000 3FFF rom-bounds    \ default memory allocation
        8000 FFFF data-bounds
        4000 7FFF code-bounds   \ ROM+code limited to lower 32K
decimal

homeorder

