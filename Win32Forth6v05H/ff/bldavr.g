((
        BUILDER lexicon for AVR target

        Subroutine threaded, little endian
        Supports cell size of 16 bits

        Stacks: Y = data stack, grows downward
                sp = return stack, grows upward

        TOS is cached in X
))

hex only forth also definitions

  10 to cellbits                \ 16-bit cells
   5 to addrnibbles             \ cover 20-bit address range
   2 to calignment              \ code must be on 16-bit boundaries
   8 to charbits                \ 8 bits per character
   0 to haveTOF?                \ No open firmware is available,
   headers off                  \ so there is no need for headers.
   7 to CPUtype                 \ assemble for AVR processor
   7 to CPUfamily               \ disassemble for AVR processor
   0 to CPUsubfamily
   0 to bigendian?              \ LSB is in low memory
  -4 to jsrsize                 \ size of long jump
true to disoption0              \ disassembler displays odd/even as flipped
true to commonmem?              \ strings compile to code space
1 value optimizing?             \ T if optimizing for speed

0 value enhanced                \ T if target chip has enhanced core
1 value longjump                \ T if JMP instruction is available
\ ' programAVRflash is progflash  \ use built-in programmer if possible

\ assume that rom-bounds, data-bounds and code-bounds are already set up

vocabulary BLD-PRIVATEAVR   BLD-PRIVATEAVR definitions
\ Private part of builder lexicon

variable optim
2variable opmark        \ marker for code removal
2variable lastlit
variable lits           \ # of consecutive literals

defer a,        \ lay down a word to the dictionary
:noname byte-split swap ic, ic, ; is a,         \ little-endian 16-bit


: ,ret          ( -- )  9508 a, ;

: new           ( -- )  optim off lits off ;
: swallow       ( n - ) ihere@ swap - ihere! new ;
: pusht         ( -- )  93AA a, 93BA a, 1 optim ! ; \ ST -Y,xl/xh
: opusht        ( -- )  optim @ 2 = if 4 swallow else pusht then ;
: popt          ( -- )  91B9 a, 91A9 a, 2 optim ! ; \ LD xl/xh,Y+
: opopt         ( -- )  optim @ 1 = if 4 swallow else popt  then ;
: omark         ( -- )  opmark @ ihere@ opmark 2! ;

: ,ldi          ( n opcode -- ) >r 0FF and 10 /mod 8 lshift or r> or a, ;

: pushlit       ( n -- )
\ lastlit contains most recent lit in the first cell
                lastlit 2@ nip swap lastlit 2!
                lits @ 1+ 2 min lits ! ;

: ,lit          ( n -- )
\ compile code for a literal
                dup pushlit  omark
                optimizing?
        if      opusht byte-split E0B0 ,ldi E0A0 ,ldi   \ native code literal
        else    lits @ >r
                s" false" tarcompile  byte-split        \ FALSE will be a code word
                ?dup if E0B0 ,ldi then
                ?dup if E0A0 ,ldi then
                optim off
                r> lits !
        then    ;

: qlit?         ( -- F | lit T )
\ if last code was for a literal, remove it and return the literal.
\ this may be called after qlit1 to see if there's a second literal.
                lits @
        if      opmark @ ihere!         \ delete 'literal' code
                lastlit @ true          \ return literal value
        else    false
        then    new ;

: qlit1?        ( -- F | lit T )
\ if last code was for a literal, remove it and return the literal.
                lits @
                optimizing? 0<> and     \ only do this if optimizing for speed
        if      opmark 2@ ihere! opmark ! \ delete 'literal' code
                lastlit 2@ swap lastlit ! \ return literal value
                lits decr  true
        else    false new
        then    ;

: rreach        ( -- maxdistance )
\ For parts 8K and smaller, let RJMP assume the address space wraps around.
                heremax 1FFF > if 07FF else 0FFF then ;


\ RJMP/RCALL can reach anywhere in a 8K AVR device.  For bigger devices, 16-bit
\ JMP/CALL is used.  The address space wraps around in the AT90S8515, so RJMP
\ spans it. Any part bigger than 8K will have the long jump/call instructions
\ so we let longjump always be true.

: ,jmp          ( addr --  )    \ compile jump
                new dup ihere@ 2 + - 2/ ( addr offset )
                dup abs rreach <
        if      nip 0FFF and C000 or a, 4 optim !
        else    drop 2/ longjump
                if      940C a, a,      5 optim !       \ JMP aaaa
                else    byte-split
                        E0F0 ,ldi       \ LDI ZH,ahi
                        E0E0 ,ldi       \ LDI ZL,alo
                        9409 a,         6 optim !       \ IJMP
                then
        then    ;

: ,call         ( addr --  )    \ compile call
                isliteral?
        if      t.litvalue pushlit                 \ mark this as literal code
        else    new
        then    omark
                dup ihere@ 2 + - 2/                ( addr offset )
                dup abs rreach <
        if      nip 0FFF and D000 or a, 7 optim !
        else    drop 2/ longjump
                if      940E a, a,      8 optim !       \ CALL
                else    byte-split
                        E0F0 ,ldi       \ LDI ZH,ahi
                        E0E0 ,ldi       \ LDI ZL,alo
                        9509 a,         9 optim !       \ ICALL
                then
        then    ;

: ,macro        ( a -- )
\ copy code at a into dictionary, ret (0x22) = terminator
                optimizing?
      if        'image                          \ -> actual code
                dup w@ FF00 and C000 =
        if      2 +                             \ skip a "never" branch
        then
        begin   dup 2 + swap w@ dup 9508 <>
        while   a,
        repeat  2drop new
      else      ,call                           \ macro expansion turned off
      then      ;

: ,exit         ( -- )
\ compile an exit or convert last call to a jump
                optim @  call-only? 0= and
        case    7 of -10 ihere @ codebuf + 1 - c+!  endof   \ rcall --> rjmp
                8 of  -2 ihere @ codebuf + 4 - c+!  endof   \ call --> jmp
                9 of  -1 ihere @ codebuf + 1 - c+!  endof   \ icall --> ijmp
                ,ret                                        \ neither
        endcase new ;

0 value lastheader

\ configuration for subroutine threading ---------------------------------

: N_header      ( -- )
                ihere @ to lastheader   \ point to this header
                0 a, ;                 \ lay down a header

: ,(if)   ( -- )        s" (%IF)" TARCOMPILE ;

: ,(if-)  ( -- )        FFB7 a, ;      \ SBRS  tosh,7

: N>MARK     ( -- addr )   IHERE@ 0 a, new ;
: N>0MARK    ( -- addr )   ,(if)  >MARK ;
: N>-MARK    ( -- addr )   ,(if-) >MARK ;
: N>MMARK    ( -- addr )   s" (%MULTI)" TARCOMPILE >MARK ;
: N>RESOLVE  ( addr -- )   >r IHERE@ r@ - 2/ 0BFFF + r> iw! new ;
: N<MARK     ( -- addr )   IHERE@ ;
: N<RESOLVE  ( addr -- )   IHERE@ - 2/ 1- 0FFF and 0C000 or a, new ;
: N<0RESOLVE ( addr -- )   ,(if)  <RESOLVE ;
: N<-RESOLVE ( addr -- )   ,(if-) <RESOLVE ;

: normal        ['] N>MARK     is >MARK
                ['] N>0MARK    is >0MARK
                ['] N>-MARK    is >-MARK
                ['] N>MMARK    is >MMARK
                ['] N>RESOLVE  is >RESOLVE
                ['] N<MARK     is <MARK
                ['] N<RESOLVE  is <RESOLVE
                ['] N<0RESOLVE is <0RESOLVE
                ['] N<-RESOLVE is <-RESOLVE
                ['] ,lit       is compilelit
                ['] ,call      is compilecall
                ['] ,macro     is compilemacro
                ['] ,exit      is compileexit
                ['] N_header   is newheader
                ['] lastheader is 'newheader
                ;

normal

\ configuration for token threading --------------------------------------

: C_header      ( -- )
                ihere @ to lastheader   \ point to this header
                0 ic, ;                 \ lay down a byte header

: C>XM          ( a len -- addr )  TARCOMPILE IHERE@ 0 ic, new ;
: C>MARK        ( -- addr )   s" (%FWD)"   C>XM ;
: C>0MARK       ( -- addr )   s" (%0FWD)"  C>XM ;
: C>-MARK       ( -- addr )   s" (%-FWD)"  C>XM ;
: C>MMARK       ( -- addr )   s" (%MULTI)" C>XM ;
: C>RESOLVE     ( addr -- )   >r IHERE@ r@ -  dup 0 255 between 0=
                abort" Forward branch is too far" r> ic! new ;
: C<MARK        ( -- addr )   IHERE@ ;
: C<RES         ( addr a len -- )  TARCOMPILE
                IHERE@ swap -  dup 0 255 between 0=
                abort" Backward branch is too far" ic, new ;
: C<RESOLVE     ( addr -- )   s" (%REV)"  C<RES ;
: C<0RESOLVE    ( addr -- )   s" (%0REV)" C<RES ;
: C<-RESOLVE    ( addr -- )   s" (%-REV)" C<RES ;

: C,lit         ( N -- )
\ Usable tokens for literals: LIT16 = signed 16-bit, LIT8 = signed 8-bit
                dup -128 127 between
        if      s" (LIT8)"  TARCOMPILE ic,
        else    s" (LIT16)" TARCOMPILE byte-split ic, ic,
        then    ;

: C,call        ( CFA -- )  drop t.xt   \ use XT instead:
                token, ;

: C,macro       ( CFA -- )  drop t.xt token, ; \ no macros, only calls

: C,exit        ( -- )      s" (%;)" TARCOMPILE ;

\ This is something I started but didn't finish. The idea was to run TOF in an
\ AVR by tokenizing source code and running an interpreter to execute it. Normal
\ mode would compile subroutine threaded code, condensed mode would compile
\ tokenized code.

: condensed     ['] C>MARK     is >MARK
                ['] C>0MARK    is >0MARK
                ['] C>-MARK    is >-MARK
                ['] C>MMARK    is >MMARK
                ['] C>RESOLVE  is >RESOLVE
                ['] C<MARK     is <MARK
                ['] C<RESOLVE  is <RESOLVE
                ['] C<0RESOLVE is <0RESOLVE
                ['] C<-RESOLVE is <-RESOLVE
                ['] C,lit      is compilelit
                ['] C,call     is compilecall
                ['] C,macro    is compilemacro
                ['] C,exit     is compileexit
                ['] C_header   is newheader
                ['] lastheader is 'newheader
                ;

:noname         ( n -- )
                localdepth @ swap - 1- 2 * 1+
                E000 + a,
                s" (%local@)" tarcompile ; is local-fetch

:noname         ( n -- )
                localdepth @ swap - 1- 2 * 1+
                E000 + a,
                s" (%local!)" tarcompile ; is local-store

:noname         ( #locals #total -- )
                over -   ( #locs #extras )
                8 lshift or 0F0F and E000 + a,
                s" (%local[)" tarcompile ; is local-begin

:noname         ( #total -- )
                2 * E000 + a, s" (%]local)" tarcompile
                new ;  is local-end    \ new forces us to keep CALL


\ ------------------------------------------------------------------------
\ Prefix Assembler

false to sys-warning?
hex

vocabulary registers    \ valid register names
vocabulary bits         \ valid SREG bit names
vocabulary pointers     \ valid pointer names

create operands 40 allot

: @@            ( -- )          \ get operands from input stream
                bl word count operands place ;

: reg           ( a -- n )      \ translate string to register number
                >r get-order only registers
                r> find 2>r set-order 2r>
        if      execute exit
        then    true abort" Invalid register name" ;

: bit           ( a -- n )      \ translate string to register number
                >r get-order only bits
                r> find 2>r set-order 2r>
        if      execute exit
        then    true abort" Invalid SREG bit name" ;

: pointer       ( a -- n )      \ translate string to pointer number
                >r get-order only pointers
                r> find 2>r set-order 2r>
        if      execute exit
        then    true abort" Invalid pointer name" ;

: alit          ( a -- n )      \ translate string to number for literal
\ precedence: assembly-label, number, @@?, code-address
                count pad place
                pad label>n  dup bogusvalue =           \ not assembly address?
        if      drop pad count number? nip 0=           \ try number
                if      drop pad getlabel   dup -1 =    \ try address of word
                        if      pad count 1- s" @@" compare
                                if      pad count >r count >r count >r c@ ( '|3'x )
                                        [char] ' = r> r>  \ f x ' | 3
                                        [char] ' = r> 3 = \ f x f f
                                        and rot and 0=    \ x f
                                        if      ."  ??"
                                        true abort"  Unknown label/immediate data"
                                        else    nip
                                        then            \ ASCII character
                                else    pad 3 chars + c@ [char] 0 -
                                        dup 0 9 between \ local label
                                        0=   abort"  Unknown local label"
                                        qasmlabs@ nip
                                then
                        then
                then
        then    ;

: >rel          ( addr -- offset ) ihere@ - 2 - ;
: alit7         ( a -- n )  alit dup  -8 and abort" Literal out of 0..7 range" ;
: alit31        ( a -- n )  alit dup -20 and abort" Literal out of 0..31 range" ;
: alit63        ( a -- n )  alit dup -40 and abort" Literal out of 0..63 range" ;
: alitbr        ( a -- n )  alit >rel 2/ dup
                -40 3F between 0= abort" Branch offset out of -64..+63 range"
                7F and ;

: op0           ( -- a )        \ extract first operand to pad, return addr of pad
                operands count ',' scan drop    \ scan to ',' or ' '  ( a' )
                operands char+ tuck - pad place  pad ;

: op1           ( -- a )        \ extract operand after ',' to pad
                operands count ',' scan 1 /string bl skip
                dup 0= abort" Missing comma or 2nd operand"
                pad place  pad ;

: @Rd           ( -- Rd )       operands reg ;          \ Expect a single reg
: @Rd,          ( -- Rd )       op0 reg ;               \ Register before comma
: @Rr           ( -- Rr )       op1 reg ;               \ Register after the comma

: ?1623 dup 10 17 between 0= abort" Rd must be 16..23" 07 and ;
: ?1631 dup 10 1F between 0= abort" Rd must be 16..31" 0F and ;
: ?even dup 1 and abort" Must be EVEN register numbers" 2/ ;
: ^op   create , does> @ a, ;                           \ implicit
: ^rd   create , does> @ @@ @Rd  4 lshift or a, ;       \ single register
: >rn   ( n R -- n' )  10 /mod 9 lshift or or ;
: ^rdrd create , does> @ @@ @Rd dup 4 lshift swap >rn or a, ;
: ^rdrr create , does> @ @@ @Rd, 4 lshift @Rr >rn or a, ;
: ^sbit create , does> @ @@ operands bit 4 lshift or a, ;
: ^iw   create , does> @ @@ @Rd, 18 - dup 1 and over 0< or   \ immediate word
        abort" Expecting register 24,26,28 or 30"  3 lshift     ( n dd )
        op1 alit63  10 /mod 6 lshift or or or a, ;
: _im8  ( n -- n' ) 0FF and 10 /mod 8 lshift or ;
: ^im8  create , does> @ @@ @Rd, ?1631 4 lshift op1 alit 0FF and
        _im8 or or a, ;
: ^im8~ create , does> @ @@ @Rd, ?1631 4 lshift op1 alit 0FF and
        INVERT 0FF AND  _im8 or or a, ;
: ^im16 create , does> @ @@ @Rd, 4 lshift
        op1 alit  0FFFF and  ( n d a ) >r or a, r> a, ;
: ^im61 create , does> @ @@ @Rr 4 lshift
        op0 alit  0FFFF and  ( n d a ) >r or a, r> a, ;
: ^jmp  create , does> @ @@ operands alit >rel 2/ dup abs rreach >=
        abort" Offset is out of branch range" 0FFF and or a, ;
: ^d,b  create , does> @ @@ @Rd, 4 lshift
        op1 alit7 or or a, ;
: ^bb,k create , does> @ @@ op0 bit op1 alitbr 3 lshift or or a, ;
: ^br   create , does> @ @@ operands alitbr 3 lshift or a, ;
: ^ljmp create , does> @ @@ operands alit 2/ word-split swap >r
        2 /mod 2 rshift 3E and or or a, r> a, ;
: ^a,b  create , does> @ @@ op0 alit31 3 lshift  op1 alit7 or or a, ;
: ^dr33 create , does> @ @@ @Rd, ?1623 4 lshift  @Rr ?1623 or or a, ;
: ^dr44 create , does> @ @@ @Rd, ?1631 4 lshift  @Rr ?1631 or or a, ;
: ^io   create , does> @ @@ @Rd, 4 lshift op1 alit63 10 /mod 0A lshift or or or a, ;
: ^lsd  create , does> @ @@ @Rd, 4 lshift op1 2 over c! pointer  \ expecting 9009 or 9001
        dup 9001 xor FFF7 and abort" Expecting Y+.. or Z+.."  8 and
        op1 count '+' scan 1 /string  pad place pad alit63  ( d p# q )
        8 /mod 4 /mod 3 lshift or 0A lshift or or or or a, ;
: _ldp  ( opcd n -- )  swap >r byte-split swap _im8 r@ or a, _im8 10 + r> or a, ;
: ldp   ( opcd -- )  operands alit _ldp ;
: ldp2  ( opcd -- )  operands alit 2/ _ldp ;
: _ldp2 ( opcd addr -- )  alit 2/ _ldp ;
: _vec  ( <reg,name> -- ) @@ @Rd, ?1631 4 lshift E000 or op1 _ldp2 ;

: ^imp  create , does> @ @@ ldp ;
: ^imp2 create , does> @ @@ ldp2 ;
: ^bit  create , does> @ @@ op1 alit 100 /mod abort" Bit address must be 0..255"
        or a, ;
: ?bransize    ( n -- n ) dup -3F 40 between 0= abort" Branch is too long" ;
: >amark       ( op -- f addr op ) >r true ihere@  0 a, r> ;
: >aresolve    ( f addr op -- )
                >r ihere@ over 2 + - 2/                 \ f a offset | op
                r@
        if      ?bransize 7F and 3 lshift r> or
        else    0FFF and C000 or r>drop
        then    swap iw! ?condition ;

: <amark       ( -- f addr )   true ihere@ ;
: <aresolve    ( f addr op -- )     \ op=0 for unconditional branch
                >r ihere@ 2 + - 2/  r@
        if      ?bransize 7F and 3 lshift r> or
        else    0FFF and C000 or r>drop
        then    a, ?condition   ;

: ^if    create , does> @ >amark ;
: ^unt   create , does> @ nip <aresolve ;
: ^whil  create , does> @ >amark 3swap  ;

: ?enhanced     enhanced 0= abort" Enhanced core is needed for this mode" ;

: ,adiwx        ( lit opcd -- )    swap 0FF and 10 /mod 6 lshift or or a, ;
: ,imm8         ( n opcd -- )      swap 0FF and 10 /mod 8 lshift or or a, ;
: ,addimmed     ( n -- )
        new dup    0 03F between if 9610 ,adiwx exit then  \ 0 to 63 = ADIW
        negate dup 0 03F between if 9710 ,adiwx exit then  \ -1 to -63 = SBIW
        byte-split swap 50A0 ,imm8 40B0 ,imm8 ;     \ otherwise
: ,qc!          ( addr lit -- )    E000 ,imm8 9300 a, a, ;
: ,pushi        ( n -- )           E000 ,imm8 930F a, ;


: nreg          ( n <name> -- )
\ define a new register name
        current @ >r
        also registers definitions constant
        previous
        r> current ! ;

: ^ldibit       ( n name -- ) ( strings... ] -- )
\ pack I/O bit assignments into R16 for initialization purposes
        create , does> @ >r
      0 begin   bl word  label>n
                dup bogusvalue <>
        while   1 swap lshift or
        repeat  drop
                _im8 r> or a, ;

variable tempreg variable casedepth

variable csp
create cstack 0x40 cells allot
: csp@          ( -- a ) csp @ 0x3F and cells cstack + ;
: cspsh         ( n -- ) csp incr  csp@ ! ;
: cspop         ( -- n ) csp@ @  csp decr ;

magiclit value myvector

: (c;)          ( f -- )
\ end a code definition
                ?condition previous
                magiclit myvector <>
                if myvector then
                magiclit to myvector ;


\ -------------------------------------------------------------------------
variable temphere
variable temphere1
hex
also BLD-PRIVATEAVR also BUILDER (definitions) (previous)
\ BUILDER ADD-ONS =========================================================

: condensed condensed ;
: normal    normal ;

warning off
: create        ram?
        if      dt.ramarray makeheader
                thisdata compilelit compileexit      \ code: literal
        else    (pcreate)       \ CREATE uses ROM space only
        then    ;
warning on

: macro:        ( <name> -- )   assemmacro ;

: }vector       ( addr <reg,label> -- )     \ resolve address
                ihere@ >r ihere!
                _vec   r> ihere! ;

: /BIT/         ( -- )
\ bracket bit assignments.  Usage: /bit/ asmbyte mybit asmbyte yourbit /bit/
                ihere@ temphere @ ihere! temphere ! ;

: REGISTER:     ( value <name> -- )  nreg ;
\ define a new register name

(( state machine implementation example:
code state3     vector{            reti c;
code state2     vector  z,state3     reti c;
code state1     vector  z,state2     reti c;
                }vector z,state1  \ resolves code laid down by vector{
code demoISR    ( timer ISR, for example )
                ijmp c;           \ assumes Z never gets clobbered
))

: USER          ( n <name> -- )
\ runtime: ( -- a )
                0 makeheader  0A +              \ offset to user data
                pusht  2FA6 a, 2FB7 a,          \ mov tos,u
                ,addimmed ,exit ;               \ add offset to user data


\ literal-optimized words

: +     ( -- )  qlit? if ,addimmed        else s" +" tarcompile then ; immediate
: -     ( -- )  qlit? if negate ,addimmed else s" -" tarcompile then ; immediate
: AND   ( -- )  qlit? if byte-split dup 0xFF <> if 70B0 ,imm8 else drop then
                                    dup 0xFF <> if 70A0 ,imm8 else drop then
                else s" AND" tarcompile then ; immediate
: OR    ( -- )  qlit? if byte-split ?dup if 60B0 ,imm8 then  \ omit ORI X,0
                                    ?dup if 60A0 ,imm8 then
                else s" OR"  tarcompile then ; immediate
: C!    ( -- )  qlit1? if qlit? if ( addr lit ) ,qc!     \ LDI R16,c  STS a,R16
                else 93A0 a, a, popt then                \ STS #n,XL  POPT
                else s" C!"  tarcompile then ; immediate
: C@    ( -- )  qlit1? if opusht 91A0 a, a, 27BB a,      \ LDS XL,#n  CLR XH
                else s" C@"  tarcompile then ; immediate
: !     ( -- )  qlit1? if qlit? if byte-split pluck 1+ swap ,qc! ,qc!
                else 93A0 a, dup a, 93B0 a, 1+ a, popt then \ STS #n,XL STS #n+1,XH
                else s" !"   tarcompile then ; immediate
: @     ( -- )  qlit1? if opusht 91A0 a, dup a, 91B0 a, 1+ a,
                else s" @"   tarcompile then ; immediate \ LDS XL,#n  LDS XH,#n+1
: A!    ( -- )  qlit? if byte-split E090 ,imm8 E080 ,imm8 \ LDIW A,#n
                else s" A!"  tarcompile then ; immediate
: >R    ( -- )  qlit? if byte-split swap ,pushi ,pushi   \ push lit to rstack
                else s" >R"  tarcompile then ; immediate

: ?[    ( -- )          \ compile comparison test
                3000 ,ldi                       \ cpi R16,#c
                F401 >amark                     \ brne fwd
                ; immediate

: ]?    ( -- )          \ end comparison test
                ,exit >aresolve ; immediate

: qcase:        ( c - )
                2F0A a,                         \ mov R16,tosl
                s" DROP" tarcompile ; immediate


: IFSET         ( bit# -- | n -- n )
\ Compile a branch to test a bit in TOS. TOS won't be swallowed.
                ?COMP
                dup 0x0F > abort" Bit# must be between 0 and 15."
                8 /mod 4 lshift FFA0 or or a,   \ SBRS Xx,bit
                ?COMP >MARK 2 ;  IMMEDIATE      \ NEVER

: IFCLR         ( bit# -- | n -- n )
\ Compile a branch to test a bit in TOS. TOS won't be swallowed.
                ?COMP
                dup 0x0F > abort" Bit# must be between 0 and 15."
                8 /mod 4 lshift FDA0 or or a,   \ SBRC Xx,bit
                ?COMP >MARK 2 ;  IMMEDIATE      \ NEVER

\ Similar works like DUP 0x100 AND IF.  It's equivalent in typical Forth is:
\ : IFSET   postpone DUP  bl word number
\           1 begin over while 2* swap 1- swap repeat nip
\           postpone LITERAL postpone AND postpone IF ; IMMEDIATE



\ -------------------------------------------------------------------------

only FORTH also BLD-PRIVATEAVR also ASSEM definitions

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ \\\   ASSEM WORDLIST
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

: vector        ( <reg,label> -- )   _vec ;
\ compile code to load address into Z

: vector{       ( -- )
\ compile code to load address into Z, will be resolved later
                ihere@ to myvector 0 a, 0 a, ;

: FOR           ( - a f )       <amark ;
: NEXT          ( a f - )       @@ @Rd 4 lshift 940A or a, F401 <aresolve ;

: CYCLE_DELAY   ( n -- )
\ generate code to produce an exact cycle delay
                dup 0 770 between 0= abort" Delay out of range"
                3 /mod          \ generate 0 to 770 cycle delay
   ?dup if      _im8 E000 or a, \ ldi r16,q
                950A a,         \ dec r16
                F7F1 a,         \ brne -1
        then
        begin   ?dup
        while   1- 0 a,         \ pad with NOPs
        repeat  ;



: JUMP[         ( Reg# label label ... ]- -- )
\ Generate a jump table, sample usage is JUMP[ R16 MyLabel YourLabel ]JUMP
\ Table can have up to 255 entries.
                @@ @Rd tempreg ! 0          ( tally | )
        begin   assemcfa? 0=
        while   >r 1+
        repeat  drop                    ( tally | lab0 lab1... labN )
                ihere@ 2/ dup>r over + 4 +      \ point to 1st rjmp
                dup r> xor 0FF00 and dup>r
                0= if 1- then                   \ don't need 16-bit subtraction
                E0E0 swap _ldp                  \ lay down code to load Z
                tempreg @ 10 /mod 9 lshift or 19E0 or a, \ SUB ZL,reg
                r> if 40F0 a, then              \ optional SBCI ZH,0
                9409 a,                         \ IJMP
        begin   ?dup
        while   1- r> >rel 2/ dup abs rreach >=
                if . true abort" Offset is out of branch range" then
                0FFF and C000 or a,
        repeat  ;

\ Usage: CASE R16
\          3 of rcall three endof
\          5 or rcall five  endof
\        ENDCASE

: CASE          ( Reg# -- )
                @@ @Rd ?1631 4 lshift tempreg ! \ register must be 16..31
                casedepth @ 8 lshift
                casedepth ! ;                   \ allow 4 nested cases
: OF            ( n -- fao )
                10 /mod 8 lshift or tempreg @ or 3000 or a, \ CPI Rd,K
                F401 >amark  ;                  \ BRNE forward

: ENDOF         ( fao -- fao' )
                0 >amark 3swap >aresolve        \ resolve BRNE, lay "break"
                1 casedepth c+! ;

: ENDCASE       ( ... fao1 fao0 -- )
                casedepth @ 0xFF and  0
        ?do     >aresolve
        loop    casedepth @ 8 rshift
                casedepth ! ;

: |ENDOF        ( fao -- )
                >aresolve ;                     \ resolve BRNE, we don't need break



\ Opcode definitions

: IN    @@ @Rd, 4 lshift op1 alit63 10 /mod 9 lshift or or B000 or a, ;
: OUT   @@ @Rr  4 lshift op0 alit63 10 /mod 9 lshift or or B800 or a, ;

: LDD   8000 @@ @Rd, 4 lshift op1 2 over c! pointer  \ expecting 9009 or 9001
        dup 9001 xor FFF7 and abort" Expecting Y+.. or Z+.."  8 and
        op1 count '+' scan 1 /string  pad place pad alit63  ( d p# q )
        8 /mod 4 /mod 3 lshift or 0A lshift or or or or a, ;

: STD   8200 @@ @Rr 4 lshift op0 2 over c! pointer  \ expecting 9009 or 9001
        dup 9001 xor FFF7 and abort" Expecting Y+.. or Z+.."  8 and
        op0 count '+' scan 1 /string  pad place pad alit63  ( d p# q )
        8 /mod 4 /mod 3 lshift or 0A lshift or or or or a, ;

: LD    ( -- )  @@ op1 pointer  @Rd, 4 lshift or a, ;
: ST    ( -- )  @@ op0 pointer  @Rr  4 lshift or 200 or a, ;
: SER   ( -- )  @@ @rd ?1631 EF0F or a, ;

: LDIW  ( -- )  @@ @Rd, ?1631 4 lshift E000 or op1 alit _ldp ;
: LDIP  ( -- )  @@ @Rd, ?1631 4 lshift E000 or op1  _ldp2 ;

: ELPM  ( -- )
                @@ operands c@ 3 <
        if      95D8 a,         \ implied
        else    ?enhanced
                @Rd, 4 lshift op1 pointer 0F and
                dup 0E and abort" Only Z and Z+ allowed"
                9006 or or a,
        then    ;
: LPM   ( -- )
                @@ operands c@ 3 <
        if      95C8 a,         \ implied
        else    ?enhanced
                @Rd, 4 lshift op1 pointer 0F and
                dup 0E and abort" Only Z and Z+ allowed"
                9004 or or a,
        then    ;

: MOVW  ( -- )
\ Enhanced core has a MOVW instruction, fake it on others.
        @@ enhanced
        if      0100 @Rd, ?even 4 lshift  @Rr ?even or or a,
        else    2C00 @Rd, ?even 5 lshift  @Rr ?even
                2* 10 /mod 9 lshift or or or
                dup a, 0011 or a,       \ substitute two MOV instructions
        then    ;

920A ^rd PUSHD  \ PUSHD Rd is same as ST -Y,Rd
9009 ^rd POPD   \ POPD Rd is same as LD Rd,Y+


: jsr           ( | <name> -- ) assemcfa ,call new ;
: goto          ( | <name> -- ) assemcfa ,jmp new ;
: end-code      ( f -- )        (c;) ;
: c;            ( f -- )        (c;) ;
: ]c            ( f -- )        new -1 bstate ! (c;) ;
: code          ( -- f )
                previous
                true abort" Previous definition missing end-code" ;

: begin         ( - a f o )       <amark 0 ;
: again         ( a f o - )       <aresolve ;                   \ jmp <--
: then          ( a f o - )       >aresolve ;
: never         ( - a f o )       0 >amark ;
: else          ( a f o - a f o )   0 >amark 3swap then ;       \ jmp -->
: repeat        ( a f o a f o - )   again then ;                \ jmp <--
: continue      ( a f o a f o - a f o ) 3over repeat ;
: noway         ( a f o -- a f o a f o ) 0 >amark 3swap ;
: >cs           ( a f o -- )    cspsh cspsh cspsh ;
: cs>           ( -- a f o )    cspop cspop cspop ;
: csdup         ( afo -- afo afo )  3dup ;

: multi         ( -- fao fao )          \ MULTI RD ... REPEAT
                <amark 0
                940A @@ @Rd 4 lshift or a, \ dec Rd
                F002 >amark 3swap ;        \ brmi outahere


1C00 ^rdrr ADC      0C00 ^rdrr ADD      9600 ^iw   ADIW     2000 ^rdrr AND
7000 ^im8  ANDI     9405 ^rd   ASR      9488 ^sbit BCLR     F800 ^d,b  BLD
F400 ^bb,k BRBC     F000 ^bb,k BRBS
F400 ^br   BRCC     F000 ^br   BRCS     F001 ^br   BREQ     F404 ^br   BRGE
F405 ^br   BRHC     F005 ^br   BRHS     F407 ^br   BRID     F007 ^br   BRIE
F000 ^br   BRLO     F004 ^br   BRLT     F002 ^br   BRMI     F401 ^br   BRNE
F402 ^br   BRPL     F400 ^br   BRSH     F406 ^br   BRTC     F006 ^br   BRTS
F403 ^br   BRVC     F003 ^br   BRVS
9408 ^sbit BSET     FA00 ^d,b  BST      940E ^ljmp CALL     9800 ^a,b  CBI
7000 ^im8~ CBR      2400 ^rdrd CLR
9488 ^op   CLC      94D8 ^op   CLH      94F8 ^op   CLI      94A8 ^op   CLN
94C8 ^op   CLS      94E8 ^op   CLT      94B8 ^op   CLV      9498 ^op   CLZ
9400 ^rd   COM      1400 ^rdrr CP       0400 ^rdrr CPC      3000 ^im8  CPI
1000 ^rdrr CPSE     940A ^rd   DEC      9519 ^op   EICALL   9419 ^op   EIJMP
2400 ^rdrr EOR      95F8 ^op   ESPM     0308 ^dr33 FMUL     0380 ^dr33 FMULS
0388 ^dr33 FMULSU   9509 ^op   ICALL    9409 ^op   IJMP     9403 ^rd   INC
940C ^ljmp JMP      E000 ^im8  LDI      9000 ^im16 LDS
0C00 ^rdrd LSL      9406 ^rd   LSR      2C00 ^rdrr MOV
9C00 ^rdrr MUL      0200 ^dr44 MULS     0300 ^dr33 MULSU    9401 ^rd   NEG
0000 ^op   NOP      2800 ^rdrr OR       6000 ^im8  ORI      900F ^rd   POP
920F ^rd   PUSH     D000 ^jmp  RCALL    9508 ^op   RET      9518 ^op   RETI
C000 ^jmp  RJMP     1C00 ^rdrd ROL      9407 ^rd   ROR      6000 ^im8  SBR
0800 ^rdrr SBC      4000 ^im8  SBCI     9A00 ^a,b  SBI      9900 ^a,b  SBIC
9B00 ^a,b  SBIS     9700 ^iw   SBIW     FC00 ^d,b  SBRC     FE00 ^d,b  SBRS
9408 ^op   SEC      9458 ^op   SEH      9478 ^op   SEI      9428 ^op   SEN
9448 ^op   SES      9468 ^op   SET      9438 ^op   SEV      9418 ^op   SEZ
9588 ^op   SLEEP    95E8 ^op   SPM      9200 ^im61 STS      1800 ^rdrr SUB
5000 ^im8  SUBI     9402 ^rd   SWAP     2000 ^rdrd TST      95A8 ^op   WDR

\ custom macros -----------------------------------------------------------
\ load pointer register with 16-bit immediate data divided by 2 (program address)
E0A0 ^imp2 LDXP     E0C0 ^imp2 LDYP     E0E0 ^imp2 LDZP
E000 ^ldibit LDI_R16[
E010 ^ldibit LDI_R17[
E020 ^ldibit LDI_R18[
E030 ^ldibit LDI_R19[
E040 ^ldibit LDI_R20[
E050 ^ldibit LDI_R21[
E060 ^ldibit LDI_R22[
E070 ^ldibit LDI_R23[

\ use 8-bit bit address to allow bit labels
9800 ^bit  CLRB     9A00 ^bit  SETB     9B00 ^bit  SKIPS    9900 ^bit  SKIPC
\ conditionals
F400 ^if   IF_C     F000 ^if   IF_NC    F401 ^if   IF_Z     F001 ^if   IF_NZ
F402 ^if   IF_MI    F002 ^if   IF_PL    F403 ^if   IF_V     F003 ^if   IF_NV
F404 ^if   IF_LT    F004 ^if   IF_GE    F405 ^if   IF_H     F005 ^if   IF_NH
F406 ^if   IF_T     F006 ^if   IF_NT    F407 ^if   IF_IE    F007 ^if   IF_ID
F401 ^if   IF_EQ    F001 ^if   IF_NE
F400 ^whil WHILE_C  F000 ^whil WHILE_NC F401 ^whil WHILE_Z  F001 ^whil WHILE_NZ
F402 ^whil WHILE_MI F002 ^whil WHILE_PL F403 ^whil WHILE_V  F003 ^whil WHILE_NV
F404 ^whil WHILE_LT F004 ^whil WHILE_GE F405 ^whil WHILE_H  F005 ^whil WHILE_NH
F406 ^whil WHILE_T  F006 ^whil WHILE_NT F407 ^whil WHILE_IE F007 ^whil WHILE_ID
F401 ^whil WHILE_EQ F001 ^whil WHILE_NE
F400 ^unt  UNTIL_C  F000 ^unt  UNTIL_NC F401 ^unt  UNTIL_Z  F001 ^unt  UNTIL_NZ
F402 ^unt  UNTIL_MI F002 ^unt  UNTIL_PL F403 ^unt  UNTIL_V  F003 ^unt  UNTIL_NV
F404 ^unt  UNTIL_LT F004 ^unt  UNTIL_GE F405 ^unt  UNTIL_H  F005 ^unt  UNTIL_NH
F406 ^unt  UNTIL_T  F006 ^unt  UNTIL_NT F407 ^unt  UNTIL_IE F007 ^unt  UNTIL_ID
F401 ^unt  UNTIL_EQ F001 ^unt  UNTIL_NE
: ; postpone \ ; immediate      \ allow semicolon comments

previous definitions

decimal
also registers definitions
00 constant R0   01 constant R1   02 constant R2   03 constant R3
04 constant R4   05 constant R5   06 constant R6   07 constant R7
08 constant R8   09 constant R9   10 constant R10  11 constant R11
12 constant R12  13 constant R13  14 constant R14  15 constant R15
16 constant R16  17 constant R17  18 constant R18  19 constant R19
20 constant R20  21 constant R21  22 constant R22  23 constant R23
24 constant R24  25 constant R25  26 constant R26  27 constant R27
28 constant R28  29 constant R29  30 constant R30  31 constant R31
00 constant WL   01 constant WH   00 constant W
02 constant UL   03 constant UH   02 constant U
04 constant IPL  05 constant IPH  04 constant IP
24 constant AL   25 constant AH
26 constant TOSL 27 constant TOSH 26 constant TOS
26 constant XL   27 constant XH   26 constant X
28 constant YL   29 constant YH   28 constant Y
30 constant ZL   31 constant ZH   30 constant Z

hex bits definitions
0 constant 0     1 constant 1     2 constant 2     3 constant 3
4 constant 4     5 constant 5     6 constant 6     7 constant 7
0 constant C     1 constant Z     2 constant N     3 constant V
4 constant S     5 constant H     6 constant T     7 constant I
pointers definitions
900C constant X    900D constant X+   900E constant -X
8008 constant Y    9009 constant Y+   900A constant -Y
8000 constant Z    9001 constant Z+   9002 constant -Z
previous definitions

true to sys-warning?
only forth also definitions
homeorder


