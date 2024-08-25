\ optimize.f beta 3.1A 2002/09/25 arm Bug fix

cr .( Loading OPTIMIZE.F V3.1A Beta OPT-LAST Bug fixed...)

((

      BUG: OPT-LAST
      opt-last is a value with initial 0. Used as a pointer to the last
      word optimised, it was used as 
           opt-last @ optimizable?
      which only worked with origin=0 code (i.e., relocatable) as 0
      pointed at the MAGIC word, which (fortunately) wasn't optimisable ;-).
      
      For origin<>0, this causes a fetch from ABSOLUTE 0, gives exception C0000005h.
      
      Fixed arm 4/10/2002 by pointing at _BEGIN in kernel (non-opt NCODE word)
      
))

(( OPTIMIZE.F            An Optimizer for Win32Forth         by Tom Zimmer

  WARNING:  THIS OPTIMIZER IS NOT GUARANTEED TO PRODUCE CORRECT CODE.
            You MUST verify your optimized program for correctness
            and functionality.

  The relatively simple code in this file allows Win32Forth to compile
  code words inline in the middle of colon definitions.  When two or more
  optimizable words occur together in a colon definition, then they are
  concatenated into one embedded headerless code word.  Notice that
  various NCODE words in FKERNEL.F specify code words that should not be
  inlined.  Examples of these are BRANCH and ?BRANCH that expect to pick
  up their destination inline following themselves in the dictionary.

  The primary benefit of inlining is the elimination of the execution of
  NEXT between each code word.

          NEXT
                mov     eax, [esi]
                add     esi, # 4
                mov     ecx, 0 [eax] [edi]
                add     ecx, edi
                jmp     ecx

  Since NEXT will take about 6 or so cycles to execute on a Pentium, it
  often exceeds the amount of cycles it takes for the code word it
  follows to execute.

  Note however that optimization of this sort can expand the code size
  significantly with little benefit.  To get the most benefit from
  optimization, you need to use it only in the parts of your code that
  are executing too slowly, like sections inside a DO LOOP, or BEGIN
  WHILE REPEAT.

April 15th, 1996 tjz
        Optimizes code words, CONSTANTs, VARIABLEs, CREATEs, VALUEs
        and TO.

June 6th, 1996 tjz
        Fixed a number of bugs in OPT-PREVIOUS, that were causing the
        generation of bad code.

June 7th, 1996 tjz
        Added additional optimizers for +, -, AND, OR, XOR, !, +!, C!,
        C+!, LSHIFT, RSHIFT and +TO.

June 24th, 1996 tjz
        Added additional optimizations as suggested by;

                "Maksimov M.O." mak@xperts.rtc.neva.ru

        He found that a bunch of kernel code words begin with "PUSH EBX"
        and end with "POP EBX".  These pairs can be optimized out of
        existence.

June 26th, 1996 tjz
        Added a >NAME (also used by >OFA in the optimizer) cacheing
        mechanism. The NFA's of the last 4096 words looked up are saved
        along with their CFA's in a pair of dynamically allocated
        buffers.  When >NAME is executed, the CFA buffer is scanned and
        the contents of the NFA buffer is returned. This change makes the
        compiler about four times faster. Unfortunately it is still about
        16 times slower than without optimization.

June 27th, 1996 tjz
        Added another form of optimizing to OPTIMIZE.F.  It code compiles
        inline any simple colon definitions is finds while optimizing.
        That is it looks inside any colon definition it finds, and if the
        colon definition contains only optimizable words, then the
        definition is inlined with optimization rather than switching
        back to high level code and compiling a threaded call.

June 28th, 1996 tjz
        Found and corrected a bug in PUSH-EBX-OPT the optimizer that
        eliminates occurances of 'POP EBX' followed by 'PUSH EBX'.  It
        worked too well, in that it didn't assemble the instruction
        'MOV EBX, 0 [ESP]' in their place.  This caused EBX to remain
        unchanged, having the wrong value.

October 22nd, 1996 tjz
        Added additional code to INLINEABLE? to allow inlining of colon
        definitions that contain veriables and literal numbers.

))

INTERNAL

in-application

4096 constant >name-max                 \ maximum >NAME's to cache
   0 value    >name-off                 \ where to put next entry

>name-max cells POINTER >name-cfa       \ buffer to hold >NAME cache of cfa's
>name-max cells POINTER >name-nfa       \ buffer to hold >NAME cache of nfa's

: >NAME-cache-init ( -- )
                >name-cfa >name-max cells erase
                >name-nfa >name-max cells erase ;

initialization-chain chain-add >NAME-cache-init

>NAME-cache-init                \ initialize the buffer now

: >name-add     ( nfa cfa -- )  \ add cfa and nfa to the cache
                >name-cfa >name-off cells+ !                    \ store cfa
                >name-nfa >name-off cells+ !                    \ store nfa
                >name-off 1+ >name-max mod to >name-off ;       \ bump offset

: >NNAME        { cfa -- nfa }  \ optimized >NAME
                cfa                                     \ if cfa is not zero
                if      >name-cfa >name-max 2dup cfa Lscan nip -
                        nip dup >name-max u<            \ did we find it?
                        if      >name-nfa +cells @      \ return nfa from cache
                        else    drop
                                cfa _>NAME dup cfa >name-add
                        then
                else    cfa _>NAME
                then    ;

' >NNAME IS >NAME

: OPTFIND       ( str -- str FALSE | cfa flag )
[ also classes ] msgfind dup                            \ if word was found
[ previous ]    if                                      \ then add it to cache
                        latest-nfa @ dup name> >name-add
                then    ;

' OPTFIND IS FIND

EXTERNAL

: .NAMECACHE    ( -- )
                cr ." Names in Name Cache: "
                >name-nfa >name-max 2dup 0 Lscan nip - nip dup . cr
                0
                ?do     >name-nfa i cells+ @ nfa-count
                        dup 2 + ?cr
                        type 2 spaces
                        start/stop
                loop    ;

INTERNAL

\ allow debugger to skip over optimized code

: dbg-next-cell-opt ( ip cfa -- ip' cfa )
                over 2 cells+ over =
                if      swap cell+ @ cell- swap
                then    ;

dbg-next-cell chain-add dbg-next-cell-opt \ link into the debugger

in-system

      0 value OPT-STATE                 \ are we currently optimizing?
      0 value OPTIMIZING?               \ is optimizing enabled?
    128 value OPT-MAXBYTES              \ optimize functions < 128 bytes
' _BEGIN value OPT-LAST                  \ previous functions address \ arm
\ arm set to point to non-optimisable word _BEGIN in kernel...

new-sys-chain OPT?-CHAIN                \ test for optimizability of cfa
new-sys-chain OPT-CHAIN                 \ optimize cfa

: SET-OPT-LAST  ( -- )
                state @
                if      here aligned to opt-last
                else    ['] _BEGIN to opt-last   \ on pointer if not compiling \ arm
                then    ;

-3 constant INLINEHINT

: INLINEABLE?   ( cfa -- f1 )   \ test definition for inline-ability
                                \ f1=INFLAG if inlineable
                dup colon?                              \ a colon definition
                if      >body
                        begin   dup @ ['] UNNEST <>         \ while not UNNEST
                        while                               \ not optimizable
                                dup @ >ofa @ 0 OPT-MAXBYTES within 0=
                                if      dup  @ @ docon =        \ a constant
                                        over @ @ dovar = or     \ or a var
                                        over @ ['] LIT = or     \ or LIT
                                        0=                      \ **NOT**
                                        if      drop FALSE
                                                EXIT
                                        then
                                then    dup @ ['] LIT =
                                if      cell+
                                then
                                cell+
                        repeat  drop INLINEHINT
                else    drop FALSE
                then    ;

: OPTIMIZABLE?  ( cfa -- f1 )   \ return flag=-1 if cfa is optimizable
                                \ return flag=INFLAG if cfa is inlineable
                dup 1 app-here between
                if      FALSE opt?-chain do-chain ?dup 0=
                        if      dup >ofa @ 0 OPT-MAXBYTES within ?dup 0=
                                if      inlineable?
                                else    nip
                                then
                        else    nip
                        then
                else    drop FALSE
                then    ;

\ build a list of code words that start with 'opcode' byte

: code_list,    { opcode \ w#threads -- }   \ compile a list of optimizable code
                here 0 ,
                voc-link @              \ words starting with 'opcode'
                begin   dup vlink>voc ( #threads cells - )
                        dup voc#threads to w#threads
                        dup voc>vcfa @
                        dup   doClass =         \ don't look through classes
                        swap do|Class = or 0=   \ don't look through classes
                        if      dup here 500 + w#threads cells move
                                voc>vcfa vocsave !
                                begin   here 500 + w#threads largest dup
                                while   dup link> dup code?
                                                        \ code word?
                                        if      over l>name n>ofa @ 0>
                                                        \ optimizable?
                                                if      dup >body c@ opcode =
                                                        \ starts with opcode?
                                                        if      dup ,
                                                        \ compile cfa of code word
                                                        then
                                                then
                                        then    drop
                                        @ swap !
                                repeat  2drop
                        else    drop
                        then    @ dup 0=
                until   drop
                here over - 1 cells / 1- 0max swap ! ;

\ constants representing opcodes for specific instructions

0x50 constant PUSH_EAX
0x53 constant PUSH_EBX
0x58 constant POP_EAX
0x5B constant POP_EBX

: pop-eax-opt   ( -- )
                here 1- c@ PUSH_EAX =           \ preceeded by PUSH EAX ?
                if      -1 allot                \ discard it
                else    macro[ pop eax ]macro   \ else compile POP EAX
                then    ;

: push-ebx-opt  ( -- )
                here 1- c@ POP_EBX =            \ preceeded by POP EBX ?
                if      -1 allot                \ then discard it
                                                \ and assemble register move
                        macro[ mov ebx, 0 [esp] ]macro
                else                            \ else compile PUSH EBX
                        macro[ push ebx         ]macro
                then    ;

create PUSH_EBX_LIST    \ list of optimizable "PUSH EBX" words
                PUSH_EBX code_list,

create POP_EAX_LIST     \ list of optimizable "POP EAX" words
                POP_EAX code_list,

: .optlists     ( -- )  \ display the lists of optimizable words
                cr ." PUSH EBX list: " cr
                push_ebx_list Lcount 0
                do      dup i cells + @ .name 12 ?cr
                loop    drop  cr
                cr ." POP EAX list: " cr
                pop_eax_list  Lcount 0
                do      dup i cells + @ .name 12 ?cr
                loop    drop cr ;

: OPT-LITERAL   ( n1 -- )                       \ compile a literal
                >r
                push-ebx-opt
                macro[ mov ebx, # r> ]macro ;

: DO-OPTIMIZE,  ( cfa -- )
                FALSE opt-chain do-chain 0=
                if      dup >body swap >ofa @   \ -- addr len ;of code word
                        over push_ebx_list Lcount rot body> Lscan nip
                        if      push-ebx-opt
                                1 /string       \ remove first byte of code word
                        then
                        over pop_eax_list  Lcount rot body> Lscan nip
                        if      pop-eax-opt
                                1 /string       \ remove first byte of code word
                        then
                        here                    \ target destination address
                        over allot              \ adjust dictionary pointer
                        swap move               \ move the bytes into target
                else    drop                    \ optimized, discard cfa
                then    ;

: INLINE,       ( cfa -- )      \ compile contents of following colon def inline
                >body
                begin   dup @ ['] unnest <>
                while   dup @ ['] LIT =
                        if      cell+
                                dup @ opt-literal       \ compile a literal
                                cell+
                        else    dup @ do-optimize,      \ compile word inline
                                cell+
                        then
                repeat  drop ;

: OPTIMIZE,     ( cfa f1 -- )   \ compile the instructions of func cfa
                                \ f1=-1 if optimizable
                                \ f1=INLINEHINT if inlineable
                INLINEHINT =
                if      inline,
                else    do-optimize,
                then    ;

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\               Special optimizers for particular data types
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

: OPT?-CONSTANT ( cfa flag -- cfa flag )        \ check for constant
                over 0= ?EXIT
                dup ?EXIT drop
                dup @ docon = ;

OPT?-CHAIN CHAIN-ADD OPT?-CONSTANT

: OPT-CONSTANT  ( cfa flag -- cfa flag )        \ optimize constants
                dup ?EXIT
                over @ docon <> ?EXIT drop
                dup >body @ opt-literal
                TRUE ;

OPT-CHAIN CHAIN-ADD OPT-CONSTANT

: OPT?-VARIABLE ( cfa flag -- cfa flag )        \ check for variable
                over 0= ?EXIT
                dup ?EXIT drop
                dup @ dovar = ;

OPT?-CHAIN CHAIN-ADD OPT?-VARIABLE

: OPT-VARIABLE ( cfa flag -- cfa flag )         \ optimize constants
                dup ?EXIT
                over @ dovar <> ?EXIT drop
                dup >body opt-literal
                TRUE ;

OPT-CHAIN CHAIN-ADD OPT-VARIABLE

: OPT?-VALUE    ( cfa flag -- cfa flag )        \ check for value
                over 0= ?EXIT
                dup ?EXIT drop
                dup @ dovalue = ;

OPT?-CHAIN CHAIN-ADD OPT?-VALUE

: OPT-VALUE     ( cfa flag -- cfa flag )        \ optimize values
                dup ?EXIT
                over @ dovalue <> ?EXIT drop
                dup >body >r
                push-ebx-opt
                macro[  mov ebx, r> [edi] ]macro
                TRUE    ;

OPT-CHAIN CHAIN-ADD OPT-VALUE

\ VALUE! optimization for previous TO <VALUE> recognition

: OPT?-VALUE!   ( cfa flag -- cfa flag )        \ check for TO value
                over 0= ?EXIT                   \ don't check NULL cfa's
                dup ?EXIT drop                  \ don't check if already optimizable
                dup @ dovalue! = ;

OPT?-CHAIN CHAIN-ADD OPT?-VALUE!

: OPT-VALUE!    ( cfa flag -- cfa flag )        \ optimize TO values
                dup ?EXIT
                over @ dovalue! <> ?EXIT drop
                dup cell- >r                    \ back to data field
                macro[  mov     r> [edi], ebx
                        pop     ebx ]macro
                TRUE    ;

OPT-CHAIN CHAIN-ADD OPT-VALUE!

\ TO <VALUE> optimization

: OPT?-TOVALUE  ( cfa flag -- cfa flag )        \ optimize TO value
                over 0= ?exit
                dup ?EXIT
                over ['] TO <> ?EXIT drop
                >in @ ' >r >in !                \ get copy of next word's cfa
                r> >body cell+ @ dovalue! = ;     \ is it TO a value

OPT?-CHAIN CHAIN-ADD OPT?-TOVALUE

: OPT-TOVALUE   ( cfa flag -- cfa flag )        \ TRUE = optimized
                dup ?EXIT
                over ['] TO <> ?EXIT
                drop                            \ discard flag
                >in @ 2>r                       \ save cfa and >IN contents
                ' >body cell+ FALSE OPT-VALUE!  \ can we optimize it?
                if      drop                    \ discard cfa
                        2r> drop  TRUE
                else    drop                    \ discard second cfa
                        2r> >in ! FALSE         \ restore >IN and recover cfa
                                                \ leave FALSE, not optimized
                then    ;

OPT-CHAIN CHAIN-ADD OPT-TOVALUE

\ VALUE! optimization for previous TO <VALUE> recognition

: OPT?-VALUE+!  ( cfa flag -- cfa flag )        \ check for TO value
                over 0= ?EXIT                   \ don't check NULL cfa's
                dup ?EXIT drop                  \ don't check if already optimizable
                dup @ dovalue+! = ;

OPT?-CHAIN CHAIN-ADD OPT?-VALUE!

: OPT-VALUE+!   ( cfa flag -- cfa flag )        \ optimize TO values
                dup ?EXIT
                over @ dovalue+! <> ?EXIT drop
                dup 2 cells - >r                \ back to data field
                macro[  add     r> [edi], ebx
                        pop     ebx ]macro
                TRUE    ;

OPT-CHAIN CHAIN-ADD OPT-VALUE!

\ TO <VALUE> optimization

: OPT?-+TOVALUE ( cfa flag -- cfa flag )        \ optimize TO value
                over 0= ?exit
                dup ?EXIT
                over ['] +TO <> ?EXIT drop
                >in @ ' >r >in !                \ get copy of next word's cfa
                r> >body 2 cells+ @ dovalue+! = ;  \ is it TO a value

OPT?-CHAIN CHAIN-ADD OPT?-+TOVALUE

: OPT-+TOVALUE  ( cfa flag -- cfa flag )        \ TRUE = optimized
                dup ?EXIT
                over ['] +TO <> ?EXIT
                drop                            \ discard flag
                >in @ 2>r                       \ save cfa and >IN contents
                ' >body 2 cells+ FALSE OPT-VALUE+! \ can we optimize it?
                if      drop                    \ discard cfa
                        2r> drop  TRUE
                else    drop                    \ discard second cfa
                        2r> >in ! FALSE         \ restore >IN and recover cfa
                                                \ leave FALSE, not optimized
                then    ;

OPT-CHAIN CHAIN-ADD OPT-+TOVALUE

\ Since binary operators are optimizable by inlining it anyway, we don't need
\ a test version of each binary operator

: BINOPT:       ( -<operator macro>- )          \ define a new nameless optimizer
                opt-chain
                begin   dup @
                while   @
                repeat  align
                here swap ! 0 , here 0 ,
                :NONAME dup>r swap ! !CSP               \ define op test word
                ' ,
                :NONAME drop         !CSP               \ define macro word
                s" { arg -- }" evaluate                 \ has a local
                r> #DOES>
                        LOCALS| dataptr |               \ save data ptr in local
                        ( cfa flag -- cfa flag )        \ TRUE = optimized
                        dup ?EXIT
                        over dataptr @     <> ?EXIT     \ equal optimizable cfa
                        here 7 - c@ 0x53   <> ?EXIT     \ push  ebx
                        here 6 - w@ 0xC3C7 <> ?EXIT     \ mov   ebx, # xxxx
                        drop                            \ discard flag
                        here 4 - @ >r                   \ get the literal xxxx
                        -7 allot                        \ discard previous literal
                        r> dataptr cell+ execute        \ pass stack top to def
                        TRUE ;

binopt: !               macro[ mov arg [edi], ebx               \ store ebx to memory
                               pop ebx              ]macro ;    \ pop stack to ebx
binopt: +!              macro[ add arg [edi], ebx               \ add ebx to memory
                               pop ebx              ]macro ;    \ pop stack to ebx
binopt: C!              macro[ mov arg [edi], bl                \ store ebx to memory
                               pop ebx              ]macro ;    \ pop stack to ebx
binopt: C+!             macro[ add arg [edi], bl                \ add ebx to memory
                               pop ebx              ]macro ;    \ pop stack to ebx
binopt: +       arg if  macro[ add ebx, # arg       ]macro then ;
binopt: -       arg if  macro[ sub ebx, # arg       ]macro then ;
binopt: OR      arg if  macro[ or  ebx, # arg       ]macro then ;
binopt: AND             macro[ and ebx, # arg       ]macro ;
binopt: XOR             macro[ xor ebx, # arg       ]macro ;
binopt: LSHIFT          macro[ shl ebx, # arg       ]macro ;
binopt: RSHIFT          macro[ shr ebx, # arg       ]macro ;
binopt: CELLS+  arg if  macro[ add ebx, # arg cells ]macro then ;
binopt: +CELLS          macro[ lea ebx, [ebx*4]
                               add ebx, # arg       ]macro ;
binopt: U<              macro[ cmp     ebx, # arg
                               sbb     ebx, ebx     ]macro ;


\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\               End of optimizers to handle special cases
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

0 value opt-end-ptr                     \ points to end of optimization

: START-OPTIMIZATION ( -- )             \ lay in double indirect pointer
                true to opt-state       \ we are starting optimization
                here 2 cells+ ,         \ link to end of assembly code
                here 0 , to opt-end-ptr
                here cell+ , ;          \ to instruction following this cell

: END-OPTIMIZATION ( -- )
                false to opt-state      \ we are stopping optimization
                macro[  lea   esi, 0x2345 [edi] \ address of word following
                        next  ]macro
                here next-seq c@ - 4 -          \ addr of literal above     \ arm 4/10/2002
                align                           \ align next address
                here swap !                     \ correct the instruction
                here opt-end-ptr ! ;

: ?END-OPTIMIZATION ( -- )              \ finish up optimization if we are
                opt-state               \ in the middle of optimizing
                if      end-optimization
                then    ;

: OPT-PREVIOUS, ( f1 -- f2 )            \ optimize the function before me
                                        \ f1=-1 if optimizable
                                        \ f1=INFLAG if inlinable
                                        \ f2=TRUE if we optimized
                >r                                      \ save inlineable?
                opt-last @ ['] lit <>                   \ NOT preceeded by lit?
                if      here opt-last - 4 =             \ and only one cell?
                        if      opt-last @              \ prev cfa
                                cell negate allot       \ discard a cell
                                start-optimization      \ start optimization
                                r@ optimize,            \ compile the code
                                TRUE
                        else    FALSE
                        then    r>drop                  \ cleanup return stack
                                EXIT                    \ **EXIT HERE
                then    r>drop
                here opt-last -  8 =                    \ or single lit
                if      here  4 - @                     \ pickup first lit
                        2 cells negate allot            \ discard 2 cells
                        start-optimization              \ start optimization
                        opt-literal                     \ compile the literal
                        TRUE
                        EXIT                            \ **EXIT HERE
                then
                here opt-last - 16 =                    \ and either double
                if      here 2 cells - @ ['] lit <>     \ if not 'lit'
                        if      FALSE
                                EXIT                    \ **EXIT HERE
                        then
                        here  4 - @                     \ pickup second lit
                        here 12 - @                     \ pickup first  lit
                        4 cells negate allot            \ discard 4 cells
                        start-optimization              \ start optimization
                        opt-literal                     \ compile first lit
                        opt-literal                     \ compile second lit
                        TRUE
                        EXIT                            \ **EXIT HERE
                then
                FALSE ;                                 \ wasn't optimizable

: OPT-COMPILE,  ( cfa -- )              \ compile cfa into the dictionary
                opt-state                               \ are we optimizing?
                if      dup optimizable? dup
                        if      optimize,               \ compile this function
                        else    drop                    \ discard flag
                                end-optimization        \ finish optimizing
                                set-opt-last
                                compile,                \ compile a cfa address
                        then
                else    dup        optimizable?         \ cfa is optimizable
                        opt-last @ optimizable?         \ and previous optimizable
                        2dup and                        \ both TRUE sort of
                        if      opt-previous,           \ -- f1
                                if      optimize,       \ compile this function
                                else    drop            \ discard flag
                                        set-opt-last
                                        compile,        \ compile a cfa address
                                then
                        else    2drop                   \ discard flags
                                set-opt-last
                                compile,                \ compile a cfa address
                        then    
                then    ;

: OPT-IMMEDIATE ( cfa -- )      \ optimize immediate words
                opt-state
                if      dup optimizable? dup
                        if      optimize,               \ compile this function
                        else    drop                    \ discard flag
                                end-optimization        \ finish optimizing
                                ['] _BEGIN to opt-last  \ arm
                                execute                 \ compile a cfa address
                        then
                else    dup        optimizable?         \ I'm optimizable
                        opt-last @ optimizable?         \ and previous is optimizable
                        2dup and
                        if      opt-previous,           \ -- f1
                                if      optimize,       \ compile this function
                                else    drop            \ discard flag
                                        set-opt-last
                                        execute         \ compile a cfa address
                                then        
                        else    2drop                   \ discard flags
                                set-opt-last
                                execute                 \ compile a cfa address
                        then
                then    ;

: OPT-DLITERAL  ( d1 -- )
                double? 0=
                if      drop
                then
                state @
                if      double?
                        if      swap  opt-literal
                        then          opt-literal
                then    ;

: OPT-NUMBER,   ( d1 -- )               \ compile an inline number
                float?
                if      ?end-optimization
                        set-opt-last
                        number,
                else    opt-state
                        if      opt-dliteral
                        else    opt-last @ optimizable? dup
                                if      opt-previous,           \ -- f1
                                        if      opt-dliteral
                                        else    set-opt-last
                                                number,
                                        then
                                else    drop                    \ discard flag        
                                        set-opt-last
                                        number,
                                then
                        then
                then    ;

0 value interpret-cnt   \ compiles slowly, so give user something to look at
0 value opt-started? 

: OPT-INTERPRET ( -- )
        begin   bl word dup c@
        while   save-src 
\ cr pocket count type
                find ?dup
                if      state @ =                       \ compiling
                        if      in-system?              \ in system
                                optimizing? 0= or       \ or not optimizing
                                if      ?end-optimization
                                        set-opt-last
                                        compile,
                                else    opt-compile,
                                then
                        else                            \ interpreting
                                dup  ['] \s =
                                over ['] \  = or
                                over ['] (  = or
                                if      execute
                                else    in-system?
                                        optimizing? 0= or
                                        if      ?end-optimization
                                                set-opt-last
                                                execute
                                        else    opt-immediate
                                        then    ?stack
                                then
                        then
                else    number
                        in-system?                      \ in system
                        optimizing? 0= or               \ or not optimizing
                        if      ?end-optimization
                                set-opt-last
                                number,
                        else    opt-number,
                        then
                then    ?unsave-src
        repeat drop ;

0 value prev-interpret

EXTERNAL

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\               Optimizer Control definitions
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

: OPT-ON        ( -- )          \ turn on optimization
                optimizing? ?EXIT
                in-system?  ?EXIT
                TRUE  to optimizing?
                FALSE to opt-state
                ['] _BEGIN to opt-last \ arm
                defer@ interpret to prev-interpret
                ['] opt-interpret is interpret ;

: OPT-OFF       ( -- )          \ turn off optimization
                optimizing? 0= ?EXIT
                false to optimizing?
                prev-interpret ?dup 0=
                if      ['] _interpret
                then    is interpret
                0 to prev-interpret ;

OPT-OFF         \ default to no optimization for now

: OPT[          ( -- )          \ enable optimization for part of a definition
                opt-on interpret ; IMMEDIATE

: ]OPT          ( -- )          \ disable optimization for remainder of definition
                opt-off ; IMMEDIATE

MODULE

in-application

