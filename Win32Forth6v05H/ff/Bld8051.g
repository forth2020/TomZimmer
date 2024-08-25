((
\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
BUILDER lexicon for 8051 target

Subroutine threaded, big endian

Supports cell sizes of 16 or 32 bits

Stacks: r0 = data stack, grows downward
        sp = return stack, grows upward

TOS is cached in DPTR or R2R3DPTR for 16/32 bit cells


Used by Firmware Studio to compile code for the 8051 microcontroller

Inline code is used whenever there isn't much penalty for doing so.  For
example, the inline code for a 16-bit literal is 6 bytes whereas a
subroutine call followed by 2 bytes of data would usually be 5 bytes.

Branching structures use a subroutine call to the branch primitive
followed by a long jump, for a total of (usually) 6 bytes.  The primitive
bumps the return address to skip the long jump.  For the 8051, this scheme
is a compromise between code size and raw speed, and is easy to disassemble.

In cases where no flag is consumed, a simple long jump is compiled.

A version of the assembler has been in use in a DOS-based system for over five
years.  It was one of my early Forth efforts so the code is a little ugly.
\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
))

only forth also definitions

cellbits 16 =
cellbits 32 = or not
[IF]    .( Cell size is ) cellbits .
        .(  must be 16 or 32 ) abort
[THEN]

home
internalstack value internalstack
forth

addrnibbles 3 <
[IF]    4 to addrnibbles        \ default to 16-bit addresses
[THEN]

-3 to jsrsize           \ size of long jump required by binding table
 8 to charbits          \ 8 bits per character

vocabulary BLD-PRIVATE51   BLD-PRIVATE51 definitions
\ Private part of builder lexicon, not for user use.
hex

1 value optimizing

variable lastcall       \ destination of last call compiled
variable litmark
variable litlast

: noo           ( -- )          \ clear optimization stuff
                litmark off ;

: nocall        ( -- )          lastcall off noo ;
: ,ret          ( -- )          22 ic, ;
: ,reti         ( -- )          32 ic, ;
: ,dptr!        ( n -- )        90 ic, byte-split ic, ic, ;

: ?,r2          ( c f -- )
        if      7A ic, ic,      \ mov r2, #c
        else    drop
        then    ;

: ?,r3          ( c f -- )
        if      7B ic, ic,      \ mov r3, #c
        else    drop
        then    ;

: ,lit          ( n -- )
\ compile code for a literal
                ihere@ >r dup litlast !
                cellbits 10 =
        if      s" DUP" tarcompile ,dptr!
        else    dup 0<  \ 32-bit cells?
                if      s" TRUE" tarcompile
                        word-split swap dup 0FFFF <>
                        if      ,dptr!
                        else    drop
                        then    byte-split          ( hl hh )
                        dup 0FF <> ?,r2
                        dup 0FF <> ?,r3
                else    s" FALSE" tarcompile
                        word-split swap dup
                        if      ,dptr!
                        else    drop
                        then    byte-split          ( hl hh )
                        dup ?,r2
                        dup ?,r3
                then
        then    r> litmark !
                cellbits 10 <> if noo then      \ only mark if 16-bit cells
                ;

: jmpcall       ( addr opcode --  )
\ Compile 2-byte or 3-byte jump or call.
                over ihere@ 2 + xor 0F800 and   \ out of current page?
                if      1+                      \ short --> long
                        ic, byte-split ic, ic,  \ lcall or ljmp
                else    >r 07FF and byte-split  \ acall or ajmp
                        5 lshift  r> + ic, ic,
                then    noo ;

: ,xcall        ( a -- )
\ Compile lcall/acall to a.
                ihere@ lastcall !  11 jmpcall ; \ store location of call

: ,xjmp         ( a -- )
\ Compile ljmp/ajmp to a.
                01 jmpcall  ;

: ,macro        ( a -- )
\ copy code at a into dictionary, ret (0x22) = terminator
                'image                          \ -> actual code
                begin   count dup 0x22 <>
                while   ic,
                repeat  2drop
                nocall ;

: ,exit         ( -- )
\ compile an exit or convert last call to a jump
                ihere@ lastcall @ - 2 3 between
                call-only? 0=   and
                lastcall on?    and
                if      lastcall @  dup ic@     \ get call opcode
                        0EF and    swap ic!     \ convert to jump
                else    ,ret                    \ else compile RET
                then    nocall ;

: ,x16          ( n -- )
\ compile code to load 16-bit n into r1|A
                byte-split 079 ic, ic,
                074 ic, ic, ;

0 value lastheader

:noname         ( -- )
                ihere @ to lastheader   \ point to this header
                0 ic, ;                 \ lay down a header
                is newheader

' lastheader    is 'newheader


' ,lit   is compilelit          \ defered words are in the FORTH wordlist
' ,xcall is compilecall
' ,macro is compilemacro
' ,exit  is compileexit

: ,(if)   ( -- )        noo s" (%IF)" TARCOMPILE ;

: ,(if-)  ( -- )
\ compile code to test (not swallow) sign bit
                noo cellbits 20 =
                if      0EA ic,                 \ mov a, r2
                else    0E5 ic, 083 ic,         \ mov a, dph
                then    20 ic, 0E7 ic, 3 ic, ;  \ jb acc.7, +3

: icic,   ( n -- )      byte-split ic, ic, ;

:noname   ( -- addr )   02 ic, IHERE@ 0 icic, noo ;          is >MARK
:noname   ( -- addr )   ,(if)  02 ic, IHERE@ 0 icic, noo ;   is >0MARK
:noname   ( -- addr )   ,(if-) 02 ic, IHERE@ 0 icic, noo ;   is >-MARK
:noname   ( -- addr )   s" (%MULTI)" TARCOMPILE >MARK ;      is >MMARK
:noname   ( addr -- )   dup 1+ IHERE@ byte-split swap
                        rot ic! swap ic! nocall ;            is >RESOLVE
:noname   ( -- addr )   noo IHERE@ ;                         is <MARK
:noname   ( addr -- )   02 ic, icic, nocall ;                is <RESOLVE
:noname   ( addr -- )   ,(if) 02 ic, icic, nocall ;          is <0RESOLVE
:noname   ( addr -- )   ,(if-) 02 ic, icic, nocall ;         is <-RESOLVE


:noname         ( n -- )
                localdepth @ - 2* 74 ic, ic,
                s" (%local@)" tarcompile ; is local-fetch

:noname         ( n -- )
                localdepth @ - 2* 74 ic, ic,
                s" (%local!)" tarcompile ; is local-store

:noname         ( #locals #total -- )
                over -   ( #locs #extras )
                4 lshift or 74 ic, ic,
                s" (%local[)" tarcompile ; is local-begin

:noname         ( #total -- )
                2 *  0E5 ic, 81 ic,     \ mov a, sp
                24 ic, negate ic,       \ add a, #-offset
                0F5 ic, 81 ic,          \ mov sp, a
                nocall ;  is local-end


\ ------------------------------------------------------------------------
\ Prefix Assembler
\ One of my early assemblers, not very pretty.

false to sys-warning?

: -comma        ( a1 -- a2 )
        dup dup c@ + dup c@             ( a1 aN c )
        ascii , =                       \ if last ch = ','
        if      bl swap c!              \  then blank comma
                -1 over c+!             \  reduce count
        else    drop
        then    ;

: -pound        ( a1 -- f a2 )
        dup 1+ c@  ascii # =            \ if first ch = '#'
        if      count 1- over c!        \  then omit it
                -1 swap
        else    0 swap
        then    ;

: -slash        ( a1 -- f a2 )
        dup 1+ c@  ascii / =            \ if first ch = '/'
        if      count 1- over c!        \  then omit it
                -1 swap
        else    0 swap
        then    ;


: ?bogus_op     ( f -- )
        abort" %% Invalid operand" ;

: no_#data      ( f -- f )
        dup 0< abort" %% Immediate data not allowed" ;

: no_direct     ( f -- f )
        dup 0> abort" %% Direct address not allowed" ;

: must_bit      ( n -- n )
        dup 100 and 0=
        abort" %% Expecting bit address.  Define $100..$1FF" ;

: must_dir      ( n -- n )
        dup 100 and abort" %% Expecting direct address" ;


: getop         ( -- n f )
\ n = value, f: -1 = #data, 0 = main label, 1 = direct
        bl word
        -comma  -pound  alab-find       \ find next operand     ( f a . )
        if      execute  swap           \ found...              ( n f )
                if      0FF and -1      \ #data
                else    dup 8000 and    \ not #data:
                        if      0       \ b15=1, main label
                        else    1       \ regular label
                        then
                then
        else    count number? nip       \ not predefined...
                not ?bogus_op           \ error if not a number ( f n )
                swap if -1 else 1 then
        then    ;

: get_direct    ( -- n )        getop 1 <> ?bogus_op ;

: get_bit       ( -- n f )
        bl word  dup count upper
        -slash  alab-find
        if      execute                 \ get bit address
        else    count number?  nip
                not ?bogus_op
        then    must_bit swap ;         \ f = T if '/' prefix

: testop        ( n bit# -- n )
\ Check bit (bit#) of n to make sure this operand is allowed.
        over swap 0 do u2/ loop         \ check for valid-bit
        1 and 1-                        \ T if bit is 0
        over 8000 < or  ?bogus_op ;     \ b15 must be 1

: maskhi        ( n1 -- n2 )    1F and ;

: getreg        ( n1 bit# -- n2 )
\ Get next opcode, must be one of the main registers.  n1 = group number.
        getop  ?bogus_op                \ catch anything not in main group
        swap testop maskhi ;            \ catch registers not in group

: get_onereg    ( n -- )
\ Get operand, make sure it's the right one.
        0F getreg <> ?bogus_op ;

: get_A         ( -- )          4 get_onereg ;          \ expect 'A'
: direct1       ( c n -- )      swap 5 + ic, ic, ;      \ store direct
: packop        ( c n -- )      maskhi + ic, ;          \ store register


: (1op)         ( c group -- )
        getop  no_#data                 \ next op, no #data
        if      nip direct1             \ direct
        else    swap testop packop      \ register
        then    ;

: (4op)         ( c group -- )
        getop dup
        if      rot drop 0<             ( c n -1/0 )
                rot + swap direct1      \ #data or direct
        else    drop swap testop packop \ register
        then    ;

\ Defining words for multiple opcodes

: 0op   create c, does> c@ ic, ;                \ no operand
: opa   create c, does> c@ get_A ic, ;          \ 'A' only
: opab  create c, does> c@ 5 get_onereg ic, ;   \ 'AB' only
: 4opa  create c, does> c@ get_A 9 (4op) ;      \ op0=A, op1=#/dir/r/@

: opcl  create c, c, does> @ byte-split
        getop no_#data                          ( c1 c2 n f )
        if      must_bit nip swap ic, ic,       \ <bit>
        else    0B testop maskhi 4 =
                if      ic, drop                \ 'A'
                else    drop 1+ ic,             \ 'C'
                then
        then    ;

: (opan)        ( c n1 -- )
\ 1st operand is direct, expecting either #data or A.  ref ANL, ORL
        getop no_direct
        if      rot 3 + ic, swap                \ op1=#data
        else    maskhi 4 <> ?bogus_op
                swap 2 +                        \ op1=A
        then    ic, ic, ;

: opan  create c, does> c@  getop no_#data
        if      (opan)                          \ op0 = direct
        else    maskhi  4 =
                if      9 (4op)                 \ op0=A
                else    get_bit                 \ op0=C
                        if 5B else 2D then
                        rot + swap direct1
                then
        then    ;

: opxr  create c, does> c@  getop no_#data
        if      (opan)                          \ op0 = direct
        else    maskhi  4 <> ?bogus_op 9 (4op)
        then    ;

: op/a  create c, does> c@  2 get_onereg        \ op0=C
        ic, beep getop 1 = ?bogus_op must_bit ic, ;

: _drct  ( opcode -- )
        getop no_#data  1 <> ?bogus_op
        must_dir direct1 ;

: drct  create c, does> c@ _drct ;              \ direct

: ad11  create c, does> c@ ic, ;

: ad16  create c, does> c@ ic, ;

: (movdptr)     90 ic,  getop 0> ?bogus_op  iw, ;

: (movc)        0A2 ic,  get_direct ic, ;

: (mova)        ( -- )
        getop  dup                              \ op1 = @,r,#,dir
        if      0<
                if      74  else  0E5           \ # or dir
                then    ic, ic,
        else    drop 9 testop maskhi 0E0 + ic,  \ @ri or rn
        then    ;

: (movr)        ( r -- )                        \ r = 6..15
        getop  dup                              ( r n f f )
        if      0<
                if  70 else 0A0 then            \ #data or direct
                rot + ic, ic,
        else    drop maskhi 4 <> ?bogus_op      \ A
                0F0 + ic,
        then    ;

create  movdop  00 c, 00 c, 92 c, 00 c, 0F5 c, 00 c, 86 c, 87 c,
                88 c, 89 c, 8A c, 8B c,  8C c, 8D c, 8E c, 8F c,

: (movdirect)   ( n -- )
        getop  dup                              ( n1 n2 f f )
        if      0<
                if      75 ic, swap ic, ic,     \ #data
                else    85 ic, ic, ic,          \ direct
                then
        else    drop  0D testop  maskhi         \ A/C/@/rn
                movdop + c@ ic, ic,
        then    ;

: (movx)        dup 1 <> swap 3 and and ;

: (cjne)        ( -- )
        0C getreg  dup 4 =
        if      drop getop  dup 0= ?bogus_op
                1 <> 0B5 +              ( c n )
        else    getop 0> abort" Immediate data expected"
                swap 0B0 +
        then    ic, ic, ;

: subca create  does> drop
                ." Use CALL <name>.  No forward reference." abort ;
: subgt create  does> drop
                ." Use GOTO <name>.  No forward reference." abort ;
: subiu create  bl word count dup c, 0
                do      dup c@ c, 1+
                loop    drop
        does>   ." Use if" dup $type ." ...then, begin...until" dup $type
                ." , or begin...while" $type ." ...repeat." abort ;

: >amark       ( -- f addr )   true   ihere@  0 ic,   ;
: >aresolve    ( f addr -- )   ihere@ over 1+ - swap ic! ?condition ;
: <amark       ( -- f addr )   true   ihere@  ;
: <aresolve    ( f addr -- )   ihere@ 1+ -  ic,   ?condition   ;

: ifj   create c, does> c@ ic, >amark ;
: ifbj  create c, does> c@ ic, get_direct ic, >amark ;
: unj   create c, does> c@ ic, <aresolve ;
: unbj  create c, does> c@ ic, get_direct ic, <aresolve ;
: whj   create c, does> c@ ic, >amark 2swap ;
: whbj  create c, does> c@ ic, get_direct ic, >amark 2swap ;

variable temphere
variable temphere1

: ~if_          ( n -- ) ( -- addr f )  \ make special versions of IF
                create , IMMEDIATE does> @ ic,
                ?COMP  asmlabel? must_bit ic, 3 ic,
                02 ic, IHERE@ 0 icic, nocall
                2 ;

: ~until_       ( n -- ) ( -- addr f )  \ make special versions of UNTIL
                create , IMMEDIATE does> @ ic,
                ?COMP  1 ?PAIRS  asmlabel? must_bit ic, 3 ic,
                02 ic, icic, nocall ;

: ~while_       ( n -- ) ( -- addr f )  \ make special versions of WHILE
                create , IMMEDIATE does> @ ic,
                ?COMP  asmlabel? must_bit ic, 3 ic,
                02 ic, IHERE@ 0 icic, nocall
                2  2SWAP ;

magiclit value myvector

: (c;)          ( f -- )
\ end a code definition
                ?condition previous
                magiclit myvector <>
                if myvector then
                magiclit to myvector ;

also assem definitions

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ \\\   ASSEM WORDLIST
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

: macro:        ( <name> -- )   assemmacro ;

: vector        ( | <name> -- )
\ compile code to load address into two IRAM locations (big endian)
                get_direct >r assemcfa byte-split  ( #lo #hi | reg )
                75 ic, r@    ic, ic,    \ mov direct,#hi(address)
                75 ic, r> 1+ ic, ic, ;  \ mov direct+1,#lo(address)

: vector{       ( f -- addr f )
\ compile code to load address into two IRAM locations (big endian)
                get_direct >r
                75 ic, r@    ic, ihere@ swap 0 ic,
                75 ic, r> 1+ ic, 0 ic, ;

(( state machine implementation example:
code state3     vector{ _myvec          reti c;
code state2     vector _myvec, state3   reti c;
code state1     vector _myvec, state2   reti c;
                }vector state1    \ resolves code laid down by vector{
code demoISR    ( timer ISR, for example )
                push _myvec1  push _myvec  ret c;
))

\ Opcode definitions

: DEC   10 0C (1op) ;

: DJNZ  ." Use BEGIN..NEXT XX structure" abort ;

: INC   getop  no_#data
        if      0 swap must_dir direct1 \ direct
        else    maskhi dup 0=
                if      drop 0A3         \ dptr
                then    ic,
        then    ;

: JMP   10 get_onereg  73 ic, ;         \ '@a+dptr'

: MOV   getop no_#data
        if      (movdirect)
        else    0F and  dup 5 >
                if      (movr)
                else    7 and 2/ exec:
                        (movdptr) (movc) (mova)
                then
        then    ;

: MOVC  get_A  0A getreg 10 =
        10 and 83 + ic, ;

: MOVX  8 getreg  dup 4 =
        if      drop 5 getreg (movx)  0E0        \ op0=A
        else    (movx) get_A  F0                \ op1=A
        then    + ic, ;

: MOVY  internalstack
        if      mov     \ int: use regular MOV instruction
        else    movx    \ ext: use regular MOVX instruction
        then    ;

: SETB  getop no_#data
        if      0D2 ic,
        else    maskhi 2 <> ?bogus_op 0D3
        then    ic, ;

: SJMP  ." Use control structures." abort ;

: XCH   get_A   0C0 9 (1op) ;
: XCHD  get_A   6 getreg 0D0 + ic, ;

        subgt   AJMP            subca   ACALL
020     4opa    ADD     030     4opa    ADDC
050     opan    ANL
0B0     op/a    ANL/                    \ substitute for ANL C, /bit
0E4 0C2 opcl    CLR
0F4 0B2 opcl    CPL
        subiu   CJNE    =
0D4     opa     DA      084     opab    DIV
        subiu   JB      _0
        subiu   JBC     _00
        subiu   JC      _nc
        subiu   JNB     _1
        subiu   JNC     _c
        subiu   JNZ     _z
        subiu   JZ      _nz
0A4     opab    MUL     00      0op     NOP
040     opan    ORL
0A0     op/a    ORL/                    \ substitute for ORL C, /bit
0CB     drct    POP     0BB      drct    PUSH
022     0op     RET     032      0op     RETI
023     opa     RL      033      opa     RLC
003     opa     RR      013      opa     RRC
090     4opa    SUBB    0C4      opa     SWAP
060     opxr    XRL



\ control structures

20    ifbj  IF_NB     20    unbj  UNTIL_NB  20    whbj    WHILE_NB
30    ifbj  IF_B      30    unbj  UNTIL_B   30    whbj    WHILE_B
50    ifj   IF_C      50    unj   UNTIL_C   50    whj     WHILE_C
70    ifj   IF_Z      70    unj   UNTIL_Z   70    whj     WHILE_Z
10    ifbj  IF_NBC    10    unbj  UNTIL_NBC 10    whbj    WHILE_NBC
40    ifj   IF_NC     40    unj   UNTIL_NC  40    whj     WHILE_NC
60    ifj   IF_NZ     60    unj   UNTIL_NZ  60    whj     WHILE_NZ

: call          ( | <name> -- ) assemcfa ,xcall ;
: lcall         ( | <name> -- ) assemcfa 12 ic, iw, ;
: goto          ( | <name> -- ) assemcfa ,xjmp ;
: ljmp          ( | <name> -- ) assemcfa 02 ic, iw, ;
: dptr=         ( | <name> -- ) assemcfa 90 ic, iw, ;
: a=hi          ( | <name> -- ) assemcfa 74 ic, byte-split nip ic, ;
: a=lo          ( | <name> -- ) assemcfa 74 ic,       ic, ;
: >A            ( c -- )        74 ic, ic, ;    \ mov a, #data
: >DPTR         ( n -- )        90 ic, iw, ;    \ mov dptr, #data16
: w,            ( n -- )        iw, ;

: begin         ( - a f )       <amark ;
: for           ( - a f )       <amark ;
: until         ( a f - )       70 ic, <aresolve ;       \ jnz <--
: again         ( a f - )       80 ic, <aresolve ;       \ sjmp <--
: then          ( a f - )       >aresolve ;
: else          ( a f - a f )   80 ic, >amark 2swap then ;  \ sjmp -->
: repeat        ( a f a f - )   again then ;            \ sjmp <--
: continue      (  a f a f - a f ) 2over repeat ;
: next          ( a f - )       0D0 7 (1op) <aresolve ;  \ djnz ?? <--
: if=           ( -- a f )      (cjne)  >amark ;
: until=        ( a f -- )      (cjne)  <aresolve ;
: while=        ( -- a f )      >amark 2swap ;

: end-code      ( f -- )        (c;) ;
: c;            ( f -- )        (c;) ;
: ]c            ( f -- )        -1 bstate ! (c;) ;
: code          ( -- f )
                previous
                true abort" Previous definition missing end-code" ;

\ Labels:  b15 = 1 if label is one of the main defining labels,
\          b15 = 0 for others, such as direct and bit addresses.
\          Of these others, b8 = 1 for bit addresses, 0 for direct.

asmlabels definitions
\ Label format:         b15: T = operand, F = direct or bit
\                       b8:  T = bit, F = direct

                          \ Define labels and valid operands
                          \ '*' = operand allowed in group
                          \   A A A A X R A R @ Z
                          \   R R R C Y @ Z     @
                          \   @ @ @       @
                          \   D C
C000   constant  DPTR     \ * * - - - - - - - - - -
8121   constant  @DPTR    \ * - - - - - - * - - * -
A822   constant  C        \ * - * - * - - - - - - -
F904   constant  A        \ * * * * * - - * - - - -
8005   constant  AB       \ * - - - - - - - - - - -
F366   constant  @R0      \ * * * * - - * * - * * -
F367   constant  @R1      \ * * * * - - * * - * * -
F288   constant  R0       \ * * * * - - * - * - - -
F289   constant  R1       \ * * * * - - * - * - - -
F28A   constant  R2       \ * * * * - - * - * - - -
F28B   constant  R3       \ * * * * - - * - * - - -
F28C   constant  R4       \ * * * * - - * - * - - -
F28D   constant  R5       \ * * * * - - * - * - - -
F28E   constant  R6       \ * * * * - - * - * - - -
F28F   constant  R7       \ * * * * - - * - * - - -
8410   constant  @A+DPTR  \ * - - - - * - - - - - -
8411   constant  @A+PC    \ * - - - - * - - - - - -

true to sys-warning?

include alab51.g          \ assembler labels for 8051

previous definitions


( order: ROOT FORTH BLD-PRIVATE51 | BLD-PRIVATE51 )


\ ========================================================================

hex
BLD-PRIVATE51 also BUILDER (definitions) (previous)
( order: ROOT FORTH BLD-PRIVATE51 | BUILDER )

\ *** NOTE: THE WORDLIST BEING BUILT ISN'T IN THE SEARCH ORDER ***

false to sys-warning?
warning off

: NEWTASK       { user dstack rstack -- } ( <name> -- ) ( -- tid )
\ Create a new task.
\ TID structure: sp0 / rp0 / pc / sp / rp / status / link / user...
\ offsets (16):   -4    -2    0    2    4    6       8      0A...
\ offsets (32):   -8    -4    0    4    8    0C      10     14...
\ TID of a task points to its status --------^
\ MEM USAGE: TCB USER RSTACK| DSTACK|
                dt.ramarray makeheader
                thisdata 7 cellsize * + >R \ -> user data
                thisdata 5 cellsize * +    \ -> status
                R@ user + rstack + dup dstack + ( alo ahi )
                over xor -100 and
        if      0FF and negate 100 +    \ need to expand rstack this much
                rstack + to rstack      \ so that dstack starts at page boundary
        else    drop                    \ dstack doesn't cross a page boundary
        then    dup t'litvalue ! compilelit compileexit
                ram                     \ fill in the data structure
                R@ user + rstack + dstack + dup i,      \ SP0
                R> user + rstack + dup i,               \ RP0
                0 i, swap i, i,                 \ pc, sp, rp,
                here 0 i, i,                    \ status, link
                user dstack + rstack + fillzeros ;

: USER          ( n <name> -- )
\ runtime: ( -- addr )
                0 makeheader
                74 ic, ic,                     \ mov a, #n
                s" (%USER)" tarcompile
                compileexit ;

: REG@:         ( <asmlabel> -- )
\ compile code to fetch byte from an SFR
                s" FALSE" TARCOMPILE    \ pushes 0000 onto the stack
                85 ic, get_direct ic, 0x82 ic, \ mov label, DPL
                ; IMMEDIATE

: REG!:         ( <asmlabel> -- )
\ compile code to store byte to an SFR
                85 ic, 82 ic, get_direct ic,   \ mov DPL, label
                s" DROP" TARCOMPILE ; IMMEDIATE

: NOCALL        ( -- )          nocall ;
\ Disable call>goto conversion for this word

: \32           ( -- )  cellbits 20 <> if postpone \ then ;     IMMEDIATE
: \16           ( -- )  cellbits 10 <> if postpone \ then ;     IMMEDIATE
: \IS           ( -- )  internalstack 0= if postpone \ then ;   IMMEDIATE
: \XS           ( -- )  internalstack  if postpone \ then ;     IMMEDIATE

: }vector       ( addr -- )
\ resolve destination of state compiled by >vector 75 dd 00 75 dd 00
\                                              addr --^
                >r assemcfa byte-split  ( lo hi | addr )
                r@ ic! r> 3 + ic! ;

: /BIT/         ( -- )
\ bracket bit assignments.  Usage: /bit/ asmbyte mybit asmbyte yourbit /bit/
                ihere@ temphere @ ihere! temphere ! ;

: /IRAM/        ( -- )
\ bracket iram assignments. Usage: /iram/ asmbyte mybit asmbyte yourbit /iram/
                ihere@ temphere1 @ ihere! temphere1 ! ;

((
: XCALL         ( -- )  xlabel ,x16
                s" (%XCALL)"  TARCOMPILE ; IMMEDIATE
: XCALL1        ( -- )  xlabel ,x16
                s" (%XCALL1)" TARCOMPILE ; IMMEDIATE
: XCALL2        ( -- )  xlabel ,x16
                s" (%XCALL2)" TARCOMPILE ; IMMEDIATE
))

\ Comparison test for TOS parsing:
\ [ 3 ] ?[ rot -rot ]? compiles to
\ mov a,dpl  cjne a,#3,+4  acall rot  ajmp -rot

: ?[            ( -- )          \ compile comparison test
                B4 ic, ic,      \ cjne a, #c, fwd
                >amark   ; immediate

: ]?            ( -- )          \ end comparison test
                ,exit >aresolve ; immediate


: qcase:        ( c -- )
                C0 ic, 82 ic,   \ push dpl
                s" DROP" tarcompile
                D0 ic, E0 ic,   \ push acc
                ; immediate

((
: test          ( c -- )
                qcase:
                [ 1 ] ?[ sqrt ]?
                [ 3 ] ?[ dist ]?
                [ 5 ] ?[ dup over rot ]?
                drop ;
))

: IFSET         ( <bit#> -- | n -- n )
\ Compile a branch to test a bit in TOS. TOS won't be swallowed.
                ?COMP
\                bl word number drop
                dup 0x0F > abort" Bit# must be between 0 and 15."
                8 /mod 0x82 + 0xE5 ic, ic,      \ mov a, direct
                1 swap lshift 0x54 ic, ic,      \ anl a, #mask
                0x7003 iw, 2 ic, ihere@ 0 iw,   \ jnz +3  ljmp 0
                2 ;  IMMEDIATE

: IFCLR         ( bit# -- | n -- n )
\ Compile a branch to test a bit in TOS. TOS won't be swallowed.
                ?COMP
\                bl word number drop
                dup 0x0F > abort" Bit# must be between 0 and 15."
                8 /mod 0x82 + 0xE5 ic, ic,      \ mov a, direct
                1 swap lshift 0x54 ic, ic,      \ anl a, #mask
                0x6003 iw, 2 ic, ihere@ 0 iw,   \ jz +3  ljmp 0
                2 ;  IMMEDIATE

\ Similar works like DUP 0x100 AND IF.  It's equivalent in typical Forth is:
\ : IFSET   postpone DUP  bl word number
\           1 begin over while 2* swap 1- swap repeat nip
\           postpone LITERAL postpone AND postpone IF ; IMMEDIATE




optimizing cellbits 0x10 = and [if]

\ Operations on immediate literals: N AND, N OR, N XOR

: AND           ( -- )
                litmark @ ?dup
        if      ihere!  litlast @ byte-split
                0x53 ic, 0x83 ic, ic,
                0x53 ic, 0x82 ic, ic, noo
        else    s" AND" TARCOMPILE
        then    ; IMMEDIATE

: OR            ( -- )
                litmark @ ?dup
        if      ihere!  litlast @ byte-split
                0x43 ic, 0x83 ic, ic,
                0x43 ic, 0x82 ic, ic, noo
        else    s" OR" TARCOMPILE
        then    ; IMMEDIATE

: XOR           ( -- )
                litmark @ ?dup
        if      ihere!  litlast @ byte-split
                0x63 ic, 0x83 ic, ic,
                0x63 ic, 0x82 ic, ic, noo
        else    s" XOR" TARCOMPILE
        then    ; IMMEDIATE

[then]

0x10 ~if_ IF_BC         0x10 ~while_ WHILE_BC         0x10 ~until_ UNTIL_BC
0x20 ~if_ IF_B          0x20 ~while_ WHILE_B          0x20 ~until_ UNTIL_B
0x30 ~if_ IF_NB         0x30 ~while_ WHILE_NB         0x30 ~until_ UNTIL_NB

\ ========================================================================
\ ========================================================================

previous also definitions

   1 to CPUtype                 \ assemble for 8031 processor
   1 to CPUfamily               \ disassemble for 8031 processor
   0 to CPUsubfamily
true to bigendian?              \ MSB in low memory

hex
        0000 1FFF rom-bounds    \ default memory allocation for 8051
        8000 9FFF data-bounds
        A000 EFFF code-bounds
decimal

homeorder


