\ float.f                                                A. McKewan
\                  Modified substantially by R. Smith

\ ------------------- Bug Fix Log -----------------------------------------
\ rls 09-21-97  Bug fixes for FTANH and E by itself.
\ rls 09-30-97  correction to restore change by SMuB, which we don't have a
\               comment to explain
\ 4-1-98 J.v.d.Ven. I put the stack into the user-area.
\               Note: The user-area should be extended.
\ tjz 01-11-99
\ Extended User area to 4096 bytes, added several new user words to kernel.
\ Added macros for DATA stack memory, Floating Point Stack Pointer, and
\ FLoating Point Stack Data area, so I could track changes when the stacks
\ were moved to the USER area.
\ rls 03-13-99  Correction for F>D, includes suggestions from Bill McCarthy
\ tjz 03-16-98  added FCALLRET, FCALL and FS>DS from Bill McCarthy.
\ tjz 04-16-99  Added F2DROP, F2DUP and F2SWAP.
\ -------------------------------------------------------------------------
\ Floating point values are maintained on a separate stack.
\ Major changes by rls to convert to a software stack
\ float.f beta 4.9C 2003/02/13 arm Remove all FWAIT/WAIT as only 386 co-processor requires
\ float.f beta 4.9D 2003/02/18 arm Added RBS code (needs stack comments)

\ anew floating-point-wordset

  cr .( Loading Floating Point...)
  cr .( -- BETA FLOAT.F 4.9D -- )


: (s.)   ( n addr len w -- addr w )
        >r drop  swap r@ * +  r> ;


: s. ( n addr len w -- )
        (s.) type ;

also assembler

: DATASTACK_MEMORY      ( -- )
\in-system-ok           0 [ebx] [edi] ;

previous

0 value float?

create allregs 110 allot


code >fregs   ( addr -- )
                ( fwait )
                frstor  DATASTACK_MEMORY
                ( fwait )
                pop     ebx
                next,
                end-code


code >fregs>   ( addr -- )
                ( fwait )
                fsave   DATASTACK_MEMORY 
                ( fwait )
                frstor  DATASTACK_MEMORY 
                ( fwait )
                pop     ebx
                next,
                end-code


code fpcw>      ( -- n )
                push    ebx
                xor     ebx, ebx
                push    ebx
                mov     ebx, esp
                fstcw   word 0 [ebx]
                ( fwait )
                pop     ebx
                next,
                end-code

code >fpcw      ( n -- )
                push    ebx
                mov     ebx, esp
                fldcw   word 0 [ebx]
                add     esp, # 4
                pop     ebx
                next,
                end-code


code fpsw>      ( -- n )
                push    ebx
                xor     eax, eax
                fstsw   ax
                ( fwait )
                mov     ebx, eax
                next,
                end-code

\- B/FLOAT 10 constant B/FLOAT          \ fp stack is 10 bytes per entry

cr B/FLOAT . .( byte floating point numbers)

B/FLOAT aligned cell / to CELLS/FLOAT   \ CELLS/FLOAT is in SEE.F

also assembler

B/FLOAT 10 =
[IF]    synonym FSIZE extended
[ELSE]  synonym FSIZE double
[THEN]

previous

cell NEWUSER FLOATSP                    \ floating point stack pointer in the user area  (new)
256 constant fstack-elements            \ 256 floating point elements in stack
next-user @ 0x10 naligned next-user !   \ align next user to quadword
fstack-elements B/FLOAT * NEWUSER FLOATSTACK

' FLOATSTACK >body @ constant FSTACK    \ pointer to the stack relative to UP
' FLOATSP >body @ constant FSP          \ pointer to fsp relative to UP

also assembler

: FSP_MEMORY    ( -- )
\in-system-ok   FSP [edx] ;

: FSTACK_MEMORY    ( -- )
\in-system-ok   FSTACK [ecx] [edx] ;

previous

: FSTKUFLO    ( -- )
        true abort"  Floating Point Stack Underflow !!! " ;


: FDEPTH        ( -- n )
                FLOATSP @ B/FLOAT / ;

code fexam      ( fs: r -- r ; -- n )
                push    ebx
                mov     ecx, FSP_MEMORY 
                sub     ecx, # B/FLOAT
                js      L$1
                fld     FSIZE FSTACK_MEMORY 
                fxam
                fstsw   ax
                mov     ebx, eax
                and     ebx, # 0x4700
                fstp    st(0)
                jmp     L$2


L$1:            mov     ebx, # 0x4100
L$2:            next,
                end-code

code (f@)       ( F: -- r ; addr -- )
                fld     FSIZE DATASTACK_MEMORY 
                ( fwait )
                pop     ebx
                next,
                end-code


code finit      ( -- )  \ clears fp stack & sets appropriate byte mode
                finit
                xor     eax, eax
                mov     FSP_MEMORY , eax
                mov     ecx, ebx
                xor     ebx, ebx
                push    ebx
                mov     ebx, esp
                fstcw   word 0 [ebx]
                pop     eax
        B/FLOAT 10 =
        [IF]    or      eax, # 0x0300           \ 10 byte mode
        [ELSE]  and     eax, # 0x0eff           \ 8  byte mode
                or      eax, # 0x0200
        [THEN]
                push    eax
                mov     ebx, esp
                fldcw   word 0 [ebx]
                pop     ebx
                mov     ebx, ecx
                ( fwait )
                next,
                end-code

code fpush      ( f: r -- )  ( fs: -- r )   \ push on simulated stack
                mov     ecx, FSP_MEMORY 
                fstp    FSIZE FSTACK_MEMORY 
                ( fwait )
                add     ecx, # B/FLOAT
                mov     FSP_MEMORY , ecx
                next,
                end-code

code fpop       ( f: -- r )   ( fs: r -- )      \ push on real stack
                mov     ecx, FSP_MEMORY
                sub     ecx, # B/FLOAT
                js      L$1
                fld     FSIZE FSTACK_MEMORY
                mov     FSP_MEMORY , ecx
                jmp     L$2
L$1:            mov     esi, # ' FSTKUFLO >body
                add     esi, edi
L$2:            next,
                end-code


: FVARIABLE     ( -<name>- )                    \ compile time
                ( -- a1 )                       \ runtime
                create B/FLOAT allot ;


code F@         ( addr -- )  ( FS: -- r )
                fld     FSIZE DATASTACK_MEMORY 
                mov     ecx, FSP_MEMORY 
                fstp    FSIZE FSTACK_MEMORY 
                add     ecx, # B/FLOAT
                mov     FSP_MEMORY , ecx
                pop     ebx
                next,
                end-code


code SF@        ( addr -- )  ( FS: -- r )
                fld     float DATASTACK_MEMORY 
                mov     ecx, FSP_MEMORY 
                fstp    FSIZE FSTACK_MEMORY 
                add     ecx, # B/FLOAT
                mov     FSP_MEMORY , ecx
                pop     ebx
                next,
                end-code


code DF@        ( addr -- )  ( FS: -- r )
                fld     double DATASTACK_MEMORY 
                mov     ecx, FSP_MEMORY 
                fstp    FSIZE FSTACK_MEMORY 
                add     ecx, # B/FLOAT
                mov     FSP_MEMORY , ecx
                pop     ebx
                next,
                end-code

code F!         ( addr -- )  ( fs: -- )
                mov     ecx, FSP_MEMORY
                sub     ecx, # B/FLOAT
                js      L$1
                fld     FSIZE FSTACK_MEMORY
                mov     FSP_MEMORY , ecx
                fstp    FSIZE DATASTACK_MEMORY
                pop     ebx
                ( fwait )
                jmp     L$2
L$1:            mov     esi, # ' FSTKUFLO >body
                add     esi, edi
                pop     ebx
L$2:            next,
                end-code

: FVALUE                         \ compiletime: ( FS: n - ) ( -<name>- )
                create here  B/FLOAT allot  f!
                ;code            \ runtime:     ( FS: - n )
                        push    ebx
                        lea     ebx, 4 [eax]
                        fld     FSIZE DATASTACK_MEMORY 
                        mov     ecx, FSP_MEMORY 
                        fstp    FSIZE FSTACK_MEMORY 
                        add     ecx, # B/FLOAT
                        mov     FSP_MEMORY , ecx
                        pop     ebx
                next,
                end-code

code _fto       ( FS: n -  )  ( 'fvalue  - )
                add     ebx, # 4              \ from >body
                mov     ecx, FSP_MEMORY         \ from f!
                sub     ecx, # B/FLOAT
                js      L$1
                fld     FSIZE FSTACK_MEMORY 
                mov     FSP_MEMORY , ecx
                fstp    FSIZE DATASTACK_MEMORY 
                pop     ebx
                ( fwait )
                jmp     L$2
L$1:            mov     esi, # ' FSTKUFLO >body
                add     esi, edi
                pop     ebx
L$2:            next,
                end-code

: FTO
                state @                 \ compiletime: ( FS: n - ) ( -<name_fvalue>- )
                if      postpone [']  compile _fto
                else    '  _fto         \ runtime:     ( FS: n - ) ( 'fvalue - )
                then    ; IMMEDIATE

: FCONSTANT     ( -<name>- ) ( F: r -- )        \ compile time
                             ( F: -- r )        \ runtime
                create here f! B/FLOAT allot does> f@ ;


code SF!        ( addr -- )  ( fs: -- )
                mov     ecx, FSP_MEMORY 
                sub     ecx, # B/FLOAT
                js      L$1
                fld     FSIZE FSTACK_MEMORY 
                mov     FSP_MEMORY , ecx
                fstp    float DATASTACK_MEMORY 
                pop     ebx
                ( fwait )
                jmp     L$2


L$1:            mov     esi, # ' FSTKUFLO >body
                add     esi, edi
                pop     ebx
L$2:            next,
                end-code


code DF!        ( addr -- )  ( fs: -- )
                mov     ecx, FSP_MEMORY 
                sub     ecx, # B/FLOAT
                js      L$1
                fld     FSIZE FSTACK_MEMORY 
                mov     FSP_MEMORY , ecx
                fstp    double DATASTACK_MEMORY 
                pop     ebx
                ( fwait )
                jmp     L$2


L$1:            mov     esi, # ' FSTKUFLO >body
                add     esi, edi
                pop     ebx
L$2:            next,
                end-code


: F,            ( -- ) ( F: r -- )
                here f! B/FLOAT allot ;


\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ Floating point stack operators
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

code FDUP       ( fs: r -- r r )
                mov     ecx, FSP_MEMORY 
                sub     ecx, # B/FLOAT
                fld     FSIZE FSTACK_MEMORY 
                add     ecx, # B/FLOAT
                fstp    FSIZE FSTACK_MEMORY 
                add     ecx, # B/FLOAT
                mov     FSP_MEMORY , ecx
                ( fwait )
                next,
                end-code

code FDROP      ( fs: r -- )
                mov     ecx, FSP_MEMORY 
                sub     ecx, # B/FLOAT
                js      L$1
                mov     FSP_MEMORY , ecx
                jmp     L$2


L$1:            mov     esi, # ' FSTKUFLO >body
                add     esi, edi
L$2:            next,
                end-code

code FSWAP      ( fs: r1 r2 -- r2 r1 )
                mov     ecx, FSP_MEMORY 
                sub     ecx, # B/FLOAT
                fld     FSIZE FSTACK_MEMORY 
                sub     ecx, # B/FLOAT
                fld     FSIZE FSTACK_MEMORY 
                fxch
                fstp    FSIZE FSTACK_MEMORY 
                add     ecx, # B/FLOAT
                fstp    FSIZE FSTACK_MEMORY 
                ( fwait )
                next,
                end-code

code FOVER      ( fs: r1 r2 -- r1 r2 r3 )
                mov     ecx, FSP_MEMORY 
                sub     ecx, # B/FLOAT 2*
                fld     FSIZE FSTACK_MEMORY 
                add     ecx, # B/FLOAT 2*
                fstp    FSIZE FSTACK_MEMORY 
                add     ecx, # B/FLOAT
                mov     FSP_MEMORY , ecx
                ( fwait )
                next,
                end-code

code FROT       ( fs: r1 r2 r3 -- r2 r3 r1 )
                mov     ecx, FSP_MEMORY
                sub     ecx, # B/FLOAT
                fld     FSIZE FSTACK_MEMORY 
                sub     ecx, # B/FLOAT
                fld     FSIZE FSTACK_MEMORY 
                sub     ecx, # B/FLOAT
                fld     FSIZE FSTACK_MEMORY 
                add     ecx, # B/FLOAT 2*
                fstp    FSIZE FSTACK_MEMORY 
                sub     ecx, # B/FLOAT 2*
                fstp    FSIZE FSTACK_MEMORY 
                add     ecx, # B/FLOAT
                fstp    FSIZE FSTACK_MEMORY 
                ( fwait )
                next,
                end-code

 code FPICK     ( n -- ) ( fs: -- r )
                push    edx
                mov     eax, # B/FLOAT
                mul     eax, ebx
                pop     edx
                mov     ecx, FSP_MEMORY
                mov     ebx, ecx
                sub     ecx, # B/FLOAT
                sub     ecx, eax
                fld     FSIZE FSTACK_MEMORY
                mov     ecx, ebx
                fstp    FSIZE FSTACK_MEMORY
                add     ecx, # B/FLOAT
                mov     FSP_MEMORY , ecx
                pop     ebx
                ( fwait )
                next,
                end-code

\ : FPICK         ( n -- ) ( fs: -- r )
\                 B/FLOAT * FLOATSTACK FLOATSP @ + B/FLOAT - swap - f@ ;

: F2DROP        ( fs: r1 r2 -- )
                fdrop fdrop ;

: F2DUP         ( fs: r1 r2 -- r1 r2 r3 r4 )
                fover fover ;

: F2SWAP        ( fs: r1 r2 -- r2 r1 )
                { \ ftemp -- }
                B/FLOAT LocalAlloc: ftemp       \ hold a floating point number
                FROT ftemp f!                   \ move out of the way into ftemp
                FROT ftemp f@ ;                 \ recover saved float

code fpi        ( fs: -- r )
                mov     ecx, FSP_MEMORY 
                fldpi
                fstp    FSIZE FSTACK_MEMORY 
                add     ecx, # B/FLOAT
                mov     FSP_MEMORY , ecx
                ( fwait )
                next,
                end-code


code f1.0       ( fs: -- r )
                mov     ecx, FSP_MEMORY 
                fld1
                fstp    FSIZE FSTACK_MEMORY 
                ( fwait )
                add     ecx, # B/FLOAT
                mov     FSP_MEMORY , ecx
                next,
                end-code


code fL2t       ( fs: -- r )            \ log base 2 of 10
                mov     ecx, FSP_MEMORY 
                fldl2t
                fstp    FSIZE FSTACK_MEMORY 
                add     ecx, # B/FLOAT
                mov     FSP_MEMORY , ecx
                ( fwait )
                next,
                end-code


code fL2e       ( fs: -- r )             \ log base 2 of e
                mov     ecx, FSP_MEMORY 
                fldL2e
                fstp    FSIZE FSTACK_MEMORY 
                add     ecx, # B/FLOAT
                mov     FSP_MEMORY , ecx
                ( fwait )
                next,
                end-code


code fLog2      ( fs: -- r )            \ log base 10 of 2
                mov     ecx, FSP_MEMORY 
                fldLg2
                fstp    FSIZE FSTACK_MEMORY 
                add     ecx, # B/FLOAT
                mov     FSP_MEMORY , ecx
                ( fwait )
                next,
                end-code


code fLn2       ( fs: -- r )            \ ln 2   natural logarithm
                mov     ecx, FSP_MEMORY 
                fldLn2
                fstp    FSIZE FSTACK_MEMORY 
                add     ecx, # B/FLOAT
                mov     FSP_MEMORY , ecx
                ( fwait )
                next,
                end-code


\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ Various floating point constants
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\


B/FLOAT 10 =                    nostack
[IF]                            stack-check
        f1.0 fvariable af0
                0 af0 !  0 af0 cell+ !  0 af0 2 cells+ w!


        : f0.0  af0 f@ ;


        f0.0 fconstant finf                     \ infinity
                ' finf >body  0x80000000 over cell+ !   nostack
                0x7fff swap 2 cells+ w!                 stack-check


        f1.0 fconstant f2.0
                1 ' f2.0 >body 2 cells+ w+!


        f1.0 fvariable a2**63
                0 a2**63 !  0x80000000 a2**63 cell+ !
                0x403e a2**63 2 cells+ w!       \ January 13th, 1998 - 9:32 tjz for RLS
                                                \ per suggestion from Pierre Abbat


        f1.0 fvariable sq2m1                    \ sqrt(2) - 1
                0xe7799210 sq2m1 !
                0xd413cccf sq2m1 cell+ !
                0x3ffd sq2m1 2 cells+ w!


        f1.0 fvariable sq2/2m1                  \ sqrt(2)/2 - 1
                0x0c4336f8 sq2/2m1 !
                0x95f61998 sq2/2m1 cell+ !
                0xbffd sq2/2m1 2 cells+ w!


        f0.0 fconstant f10.0                    \ floating 10.0
                ' f10.0 >body  0xa0000000 over cell+ !  nostack
                0x4002 swap 2 cells+ w!                 stack-check


        f1.0 fconstant f0.5                     \ floating 0.5
                0x3ffe ' f0.5 >body 2 cells+ w!


        f0.0 fconstant fbig
                ' fbig >body  -1 over !         nostack
                -1 over cell+ !
                0x7ffe swap 2 cells+ w!         stack-check


        f0.0 fconstant feps             \ smallest non-zero-number
                1 ' feps >body !


        f0.0 fvariable afsmall
                0 afsmall !  0x80000000 afsmall cell+ !
                1 afsmall 2 cells+ w!


        : fsmall  afsmall f@ ;  \ smallest "normal" number


[ELSE]  ( 8 byte mode )                 stack-check
        f1.0 fvariable af0
                0 af0 !  0 af0 cell+ !


        : f0.0  af0 f@ ;


        f0.0 fconstant finf                     \ infinity
                ' finf >body  0 over !          nostack
                0x7ff00000 swap cell+ !         stack-check


        f1.0 fconstant f2.0
                0x40000000 ' f2.0 >body cell+ !


        f1.0 fvariable a2**63
                0 a2**63 !  0x43e00000 a2**63 cell+ !


        f1.0 fvariable sq2m1                    \ sqrt(2) - 1
                0x99fcef34 sq2m1 !
                0x3fda8279 sq2m1 cell+ !


        f1.0 fvariable sq2/2m1                  \ sqrt(2)/2 - 1
                0x33018866 sq2/2m1 !
                0xbfd2bec3 sq2/2m1 cell+ !


        f0.0 fconstant f10.0                    \ floating 10.0
                ' f10.0 >body  0 over !  0x40240000 swap cell+ !


        f1.0 fconstant f0.5                     \ floating 0.5
                ' f0.5 >body 0 over !  0x3fe00000 swap cell+ !


        f0.0 fconstant fbig             \ largest non-infinite number
                ' fbig >body  -1 over !         nostack
                0x7fefffff swap cell+ !         stack-check


        f0.0 fconstant feps             \ smallest non-zero number
                1 ' feps >body !


        f1.0 fvariable afsmall
                0 afsmall !  0x00100000 afsmall cell+ !


        : fsmall  afsmall f@ ;  \ smallest "normal" number


[THEN]


stack-check


\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ Floating point literals. We store the 80-bit floating point literal
\ inline and push to the FP stack at runtime.
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\


code flit       ( fs: -- r )
                fld     FSIZE 0 [esi]
                mov     ecx, FSP_MEMORY 
                fstp    FSIZE FSTACK_MEMORY 
                add     ecx, # B/FLOAT
                mov     FSP_MEMORY , ecx
        B/FLOAT 10 =
        [IF]    add     esi, # 12        \ to keep dword alignment !
        [ELSE]  add     esi, # 8
        [THEN]
                ( fwait )
                next,
                end-code


' flit to &flit


: FLITERAL      ( fs: r -- )
                postpone flit
                here
                CELLS/FLOAT cells allot
                f!
                ; immediate


stack-check

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ Rounding Modes
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

variable cwtemp

code FLOOR      ( f1 -- f2 )
                push    ebx
                mov     ecx, FSP_MEMORY 
                sub     ecx, # B/FLOAT
                js      L$1
                fld     FSIZE FSTACK_MEMORY 
                fstcw   word cwtemp [edi]
                mov     ax, cwtemp [edi]
                mov     bx, ax
                and     ax, # 0x0f3ff
                or      ax, # 0x00400           \ Set rounding to -infinity
                mov     cwtemp [edi], ax
                ( fwait )
                fldcw   word cwtemp [edi]
                ( fwait )
                frndint
                fstp    FSIZE FSTACK_MEMORY 
                mov     cwtemp [edi], bx
                ( fwait )
                fldcw   word cwtemp [edi]
                ( fwait )
                jmp     L$2

L$1:            mov     esi, # ' FSTKUFLO >body
                add     esi, edi
L$2:            pop     ebx
                next,
                end-code

code FCEIL      ( f1 -- f2 )                    \ Added 3/13/99 rls
                push    ebx
                mov     ecx, FSP_MEMORY 
                sub     ecx, # B/FLOAT
                js      L$1
                fld     FSIZE FSTACK_MEMORY 
                fstcw   word cwtemp [edi]
                mov     ax, cwtemp [edi]
                mov     bx, ax
                and     ax, # 0x0f3ff
                or      ax, # 0x00800           \ Set rounding to +infinity
                mov     cwtemp [edi], ax
                ( fwait )
                fldcw   word cwtemp [edi]
                ( fwait )
                frndint
                fstp    FSIZE FSTACK_MEMORY 
                mov     cwtemp [edi], bx
                ( fwait )
                fldcw   word cwtemp [edi]
                ( fwait )
                jmp     L$2

L$1:            mov     esi, # ' FSTKUFLO >body
                add     esi, edi
L$2:            pop     ebx
                next,
                end-code

code FTRUNC     ( f1 -- f2 )                    \ Added 3/13/99 rls
                push    ebx
                mov     ecx, FSP_MEMORY 
                sub     ecx, # B/FLOAT
                js      L$1
                fld     FSIZE FSTACK_MEMORY 
                fstcw   word cwtemp [edi]
                mov     ax, cwtemp [edi]
                mov     bx, ax
                and     ax, # 0x0f3ff
                or      ax, # 0x00c00           \ Set rounding to truncate
                mov     cwtemp [edi], ax
                ( fwait )
                fldcw   word cwtemp [edi]
                ( fwait )
                frndint
                fstp    FSIZE FSTACK_MEMORY 
                mov     cwtemp [edi], bx
                ( fwait )
                fldcw   word cwtemp [edi]
                ( fwait )
                jmp     L$2

L$1:            mov     esi, # ' FSTKUFLO >body
                add     esi, edi
L$2:            pop     ebx
                next,
                end-code


code FROUND     ( f1 -- f2 )
                push    ebx
                mov     ecx, FSP_MEMORY 
                sub     ecx, # B/FLOAT
                js      L$1
                fld     FSIZE FSTACK_MEMORY 
                fstcw   word cwtemp [edi]       \ Save current Control Word
                mov     ax, cwtemp [edi]
                mov     bx, ax
                and     ax, # 0x0f3ff           \ set rounding to nearest
                mov     cwtemp [edi], ax
                ( fwait )
                fldcw   word cwtemp [edi]
                ( fwait )
                frndint
                fstp    FSIZE FSTACK_MEMORY 
                mov     cwtemp [edi], bx
                ( fwait )
                fldcw   word cwtemp [edi]
                ( fwait )
                jmp     L$2


L$1:            mov     esi, # ' FSTKUFLO >body
                add     esi, edi
L$2:            pop     ebx
                next,
                end-code


\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ Integer to float convsersion
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

code D>F        ( d -- )  ( F: -- r )
                xchg    ebx, 0 [esp]
                push    ebx
                fild    qword 0 [esp]
                add     esp, # 8
                pop     ebx
                mov     ecx, FSP_MEMORY 
                fstp    FSIZE FSTACK_MEMORY
                add     ecx, # B/FLOAT
                mov     FSP_MEMORY , ecx
                ( fwait )
                next,
                end-code

\ Changed 3/13/99 rls
code F>D        ( -- d )  ( fs: r -- )  \ ? out of range errors ?
                push    ebx
                sub     esp, # 8
                mov     ecx, FSP_MEMORY
                sub     ecx, # B/FLOAT
                js      L$1
                fld     FSIZE FSTACK_MEMORY

                fstcw   word cwtemp [edi]       \ rls - 03/09/99
                mov     ax, cwtemp [edi]
                mov     bx, ax
                and     ax, # 0x0f3ff
                or      ax, # 0x00c00           \ Set rounding to chop
                mov     cwtemp [edi], ax
                ( fwait )
                fldcw   word cwtemp [edi]
                ( fwait )

                fistp   qword 0 [esp]

                mov     cwtemp [edi], bx
                ( fwait )
                fldcw   word cwtemp [edi]
                ( fwait )

                pop     ebx
                xchg    ebx, 0 [esp]
                mov     FSP_MEMORY , ecx
                jmp     L$2

L$1:            mov     esi, # ' FSTKUFLO >body
                add     esi, edi

L$2:            next,
                end-code

code ZF>D       ( -- d )  ( fs: r -- )  \ ? out of range errors ?
                push    ebx
                sub     esp, # 8
                mov     ecx, FSP_MEMORY
                sub     ecx, # B/FLOAT
                js      L$1
                fld     FSIZE FSTACK_MEMORY
                fistp   qword 0 [esp]
                pop     ebx
                xchg    ebx, 0 [esp]
                mov     FSP_MEMORY , ecx
                ( fwait )
                jmp     L$2

L$1:            mov     esi, # ' FSTKUFLO >body
                add     esi, edi

L$2:            next,
                end-code

: s>f           s>d d>f ;

: f>s           f>d drop ;



\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ Floating point comparison operators
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\


code fcomppx     ( -- flags )   ( fs: r1 r2 -- )
                mov     ecx, FSP_MEMORY 
                sub     ecx, # B/FLOAT 2*
                js      L$1
                push    ebx
                add     ecx, # B/FLOAT
                fld     FSIZE FSTACK_MEMORY 
                sub     ecx, # B/FLOAT
                fld     FSIZE FSTACK_MEMORY 
                fcompp
                fstsw   ax
                mov     ebx, eax
                and     ebx, # 0x4100
                mov     FSP_MEMORY , ecx
                jmp     L$2


L$1:            mov     esi, # ' FSTKUFLO >body
                add     esi, edi
L$2:            next,
                end-code


code ftstp      ( -- flags )  ( fs: r1 -- )
                mov     ecx, FSP_MEMORY 
                sub     ecx, # B/FLOAT
                js      L$1
                push    ebx
                fld     FSIZE FSTACK_MEMORY 
                ftst
                fstsw   ax
                mov     ebx, eax
                and     ebx, # 0x4100
                fstp    st              \ pop
                mov     FSP_MEMORY , ecx
                jmp     L$2


L$1:            mov     esi, # ' FSTKUFLO >body
                add     esi, edi
L$2:            next,
                end-code


: F0=           ( -- f )  ( F: r -- )   ftstp 0x4000 = ;
: F0<           ( -- f )  ( F: r -- )   ftstp 0x0100 = ;
: f0>           ( -- f )  ( F: r -- )   ftstp 0= ;


: f=            ( -- f )  ( F: r1 f2 -- )   fcomppx 0x4000 = ;
: F<            ( -- f )  ( F: r1 f2 -- )   fcomppx 0x0100 = ;
: f>            ( -- f )  ( F: r1 f2 -- )   fcomppx 0= ;


: f<=           f> not ;
: f>=           f< not ;


: FMAX          ( f1 f2 -- f3 )
                fover fover f<
                IF      fswap fdrop
                ELSE    fdrop
                THEN ;


: FMIN          ( f1 f2 -- f3 )
                fover fover f>
                IF      fswap fdrop
                ELSE    fdrop
                THEN ;


\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ Simple math operators
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\


code F+         ( fs: r1 r2 -- r3 )             \ ? overflow errors ?
                mov     ecx, FSP_MEMORY 
                sub     ecx, # B/FLOAT 2*
                js      L$1
                fld     FSIZE FSTACK_MEMORY 
                add     ecx, # B/FLOAT
                fld     FSIZE FSTACK_MEMORY 
                faddp   st(1), st
                sub     ecx, # B/FLOAT
                fstp    FSIZE FSTACK_MEMORY 
                add     ecx, # B/FLOAT
                mov     FSP_MEMORY , ecx
                ( fwait )
                jmp     L$2


L$1:            mov     esi, # ' FSTKUFLO >body
                add     esi, edi
L$2:            next,
                end-code


code F-         ( fs: r1 r2 -- r3 )             \ ? overflow errors
                mov     ecx, FSP_MEMORY 
                sub     ecx, # B/FLOAT 2*
                js      L$1
                fld     FSIZE FSTACK_MEMORY 
                add     ecx, # B/FLOAT
                fld     FSIZE FSTACK_MEMORY 
                fsubp   st(1), st   \ should be fsubrp but this works
                sub     ecx, # B/FLOAT
                fstp    FSIZE FSTACK_MEMORY 
                add     ecx, # B/FLOAT
                mov     FSP_MEMORY , ecx
                ( fwait )
                jmp     L$2


L$1:            mov     esi, # ' FSTKUFLO >body
                add     esi, edi
L$2:            next,
                end-code


code F*         ( fs: r1 r2 -- r3 )             \ ? overflow errors
                mov     ecx, FSP_MEMORY 
                sub     ecx, # B/FLOAT 2*
                js      L$1
                fld     FSIZE FSTACK_MEMORY 
                add     ecx, # B/FLOAT
                fld     FSIZE FSTACK_MEMORY 
                fmulp   st(1), st
                sub     ecx, # B/FLOAT
                fstp    FSIZE FSTACK_MEMORY 
                add     ecx, # B/FLOAT
                mov     FSP_MEMORY , ecx
                ( fwait )
                jmp     L$2


L$1:            mov     esi, # ' FSTKUFLO >body
                add     esi, edi
L$2:            next,
                end-code


code FATAN2    ( fs: r1 r2 -- r3 )              \ OK for valid args
                mov     ecx, FSP_MEMORY 
                sub     ecx, # B/FLOAT 2*
                js      L$1
                fld     FSIZE FSTACK_MEMORY 
                add     ecx, # B/FLOAT
                fld     FSIZE FSTACK_MEMORY 
                fpatan
                sub     ecx, # B/FLOAT
                fstp    FSIZE FSTACK_MEMORY 
                add     ecx, # B/FLOAT
                mov     FSP_MEMORY , ecx
                ( fwait )
                jmp     L$2


L$1:            mov     esi, # ' FSTKUFLO >body
                add     esi, edi
L$2:            next,
                end-code


code F/         ( fs: r1 r2 -- r3 )     \ ? error for ofl or /0 ?
                mov     ecx, FSP_MEMORY 
                sub     ecx, # B/FLOAT 2*
                js      L$1
                fld     FSIZE FSTACK_MEMORY 
                add     ecx, # B/FLOAT
                fld     FSIZE FSTACK_MEMORY 
                fdivp   st(1), st
                sub     ecx, # B/FLOAT
                fstp    FSIZE FSTACK_MEMORY 
                add     ecx, # B/FLOAT
                mov     FSP_MEMORY , ecx
                ( fwait )
                jmp     L$2


L$1:            mov     esi, # ' FSTKUFLO >body
                add     esi, edi
L$2:            next,
                end-code


code FNEGATE    ( fs: r1 -- r3 )
                mov     ecx, FSP_MEMORY 
                sub     ecx, # B/FLOAT
                js      L$1
                fld     FSIZE FSTACK_MEMORY 
                fchs
                fstp    FSIZE FSTACK_MEMORY 
                ( fwait )
                jmp     L$2


L$1:            mov     esi, # ' FSTKUFLO >body
                add     esi, edi
L$2:            next,
                end-code


code f2/        ( fs: r1 -- r3 )
                mov     ecx, FSP_MEMORY 
                sub     ecx, # B/FLOAT
                js      L$1
                fld1
                fchs
                fld     FSIZE FSTACK_MEMORY 
                fscale
                fstp    FSIZE FSTACK_MEMORY 
                fstp    st(0)
                ( fwait )
                next,


L$1:            mov     esi, # ' FSTKUFLO >body
                add     esi, edi
                next,
                end-code


code f2*        ( fs: r1 -- r3 )                \ ? overflow error
                mov     ecx, FSP_MEMORY 
                sub     ecx, # B/FLOAT
                js      L$1
                fld1
                fld     FSIZE FSTACK_MEMORY 
                fscale
                fstp    FSIZE FSTACK_MEMORY 
                fstp    st(0)
                ( fwait )
                jmp     L$2


L$1:            mov     esi, # ' FSTKUFLO >body
                add     esi, edi
L$2:            next,
                end-code


code FABS       ( fs: r1 -- r3 )
                mov     ecx, FSP_MEMORY 
                sub     ecx, # B/FLOAT
                js      L$1
                fld     FSIZE FSTACK_MEMORY 
                fabs
                fstp    FSIZE FSTACK_MEMORY 
                ( fwait )
                jmp     L$2


L$1:            mov     esi, # ' FSTKUFLO >body
                add     esi, edi
L$2:            next,
                end-code


code FSQRT      ( fs: r1 -- r2 )        
                mov     ecx, FSP_MEMORY 
                sub     ecx, # B/FLOAT
                js      L$1
                fld     FSIZE FSTACK_MEMORY 
                fabs                            \ ? error for x < 0 ?
                fsqrt
                fstp    FSIZE FSTACK_MEMORY 
                ( fwait )
                jmp     L$2


L$1:            mov     esi, # ' FSTKUFLO >body
                add     esi, edi
L$2:            next,
                end-code


code FLN        ( fs: r1 -- r2 )
                mov     ecx, FSP_MEMORY 
                sub     ecx, # B/FLOAT
                js      L$1
                fldln2
                fld     FSIZE FSTACK_MEMORY 
                fabs                            \ ? error if arg < 0
                fyl2x
                fstp    FSIZE FSTACK_MEMORY 
                ( fwait )
                jmp     L$2


L$1:            mov     esi, # ' FSTKUFLO >body
                add     esi, edi
L$2:            next,
                end-code


code FACOSH     ( fs: r1 -- r2 )        \ ? error for x < 1
                mov     ecx, FSP_MEMORY 
                sub     ecx, # B/FLOAT
                js      L$1
                fldln2
                fld     FSIZE FSTACK_MEMORY 
                fabs
                fld     st(0)
                fmul    st(0), st(0)
                fld1
                fsubp   st(1), st(0)
                fabs
                fsqrt
                faddp   st(1), st(0)
                fyl2x
                fstp    FSIZE FSTACK_MEMORY 
                ( fwait )
                jmp     L$2


L$1:            mov     esi, # ' FSTKUFLO >body
                add     esi, edi
L$2:            next,
                end-code


code FASINH     ( fs: r1 -- r2 )        \ Note: well defined for r1 < 0
                mov     ecx, FSP_MEMORY 
                sub     ecx, # B/FLOAT
                js      L$1
                fldln2
                fld     FSIZE FSTACK_MEMORY 
                fld     st(0)
                fmul    st(0), st(0)
                fld1
                faddp   st(1), st(0)
                fsqrt
                faddp   st(1), st(0)
                fyl2x
                fstp    FSIZE FSTACK_MEMORY 
                ( fwait )
                jmp     L$2


L$1:            mov     esi, # ' FSTKUFLO >body
                add     esi, edi
L$2:            next,
                end-code


code (fsin)     ( f: r1 -- r2 )
                mov     ecx, FSP_MEMORY 
                sub     ecx, # B/FLOAT
                js      L$1
                fld     FSIZE FSTACK_MEMORY 
                fsin
                fstp    FSIZE FSTACK_MEMORY 
                ( fwait )
                jmp     L$2


L$1:            mov     esi, # ' FSTKUFLO >body
                add     esi, edi
L$2:            next,
                end-code


code (fcos)     ( f: r1 -- r2 )
                mov     ecx, FSP_MEMORY 
                sub     ecx, # B/FLOAT
                js      L$1
                fld     FSIZE FSTACK_MEMORY 
                fcos
                fstp    FSIZE FSTACK_MEMORY 
                ( fwait )
                jmp     L$2


L$1:            mov     esi, # ' FSTKUFLO >body
                add     esi, edi
L$2:            next,
                end-code


code (fsincos)  ( f: r1 -- r2 )
                mov     ecx, FSP_MEMORY 
                sub     ecx, # B/FLOAT
                js      L$1
                fld     FSIZE FSTACK_MEMORY 
                fsincos
                fxch
                fstp    FSIZE FSTACK_MEMORY 
                add     ecx, # B/FLOAT
                fstp    FSIZE FSTACK_MEMORY 
                add     ecx, # B/FLOAT
                mov     FSP_MEMORY , ecx
                ( fwait )
                jmp     L$2


L$1:            mov     esi, # ' FSTKUFLO >body
                add     esi, edi
L$2:            next,
                end-code

\ January 13th, 1998 - 9:33 tjz for RLS
\ corrected branch destination for L$3

code frem2pi    ( f: r1 -- r2 )
                mov     ecx, FSP_MEMORY 
                sub     ecx, # B/FLOAT
                js      L$1
                fld1
                fldpi
                fscale
                fstp    st(1)
                fld     FSIZE FSTACK_MEMORY 
L$3:            fprem1
                fstsw   ax
                sahf
                jp      L$3
                fstp    FSIZE FSTACK_MEMORY 
                fstp    st(0)
                ( fwait )
                jmp     L$2


L$1:            mov     esi, # ' FSTKUFLO >body
                add     esi, edi
L$2:            next,
                end-code



: FSIN          ( f: r1 -- r2 )         \ ? error if x > 2**63 ?
                fdup fabs a2**63 f@ f<
                IF      (fsin)
                ELSE    frem2pi (fsin)
                THEN ;


: FCOS          ( f: r1 -- r2 )         \ ? error if x > 2**63 ?
                fdup fabs a2**63 f@ f<
                IF      (fcos)
                ELSE    frem2pi (fcos)
                THEN ;


: FSINCOS       ( f: r1 -- r2 )         \ ? error if x > 2**63 ?
                fdup fabs a2**63 f@ f<
                IF      (fsincos)
                ELSE    frem2pi (fsincos)
                THEN ;


: FTAN          ( f: r1 -- r2 )         \ ? error if x > 2**63 ?
                fsincos f/ ;


code flnp1      ( fs: r1 -- r2 )                \ ? error for x <= -1
                mov     ecx, FSP_MEMORY 
                sub     ecx, # B/FLOAT
                js      L$1
                fldln2
                fld     FSIZE FSTACK_MEMORY 
                fld     FSIZE sq2m1 [edi]
                fcomp   st(1)
                fstsw   ax
                sahf
                jp      L$3
                ja      L$4
                fld     FSIZE sq2/2m1 [edi]
                fcomp   st(1)
                fstsw   ax
                sahf
                jb      L$4
                fyl2xp1
                fstp    FSIZE FSTACK_MEMORY 
                ( fwait )
                jmp     L$2


L$4:            fld1                    \ add the "1" explicitly
                faddp   st(1), st(0)
                fabs
                fyl2x
                fstp    FSIZE FSTACK_MEMORY 
                jmp     L$2


L$3:            fcompp                  \ return arg if incomparable
                jmp     L$2


L$1:            mov     esi, # ' fstkuflo >body
                add     esi, edi


L$2:            next,
                end-code


code FLOG       ( fs: r1 -- r2 )
                mov     ecx, FSP_MEMORY 
                sub     ecx, # B/FLOAT
                js      L$1
                fldlg2
                fabs                            \ ? error for x <= 0
                fld     FSIZE FSTACK_MEMORY 
                fyl2x
                fstp    FSIZE FSTACK_MEMORY 
                ( fwait )
                jmp     L$2


L$1:            mov     esi, # ' FSTKUFLO >body
                add     esi, edi
L$2:            next,
                end-code


code FEXPM1     ( fs: r1 -- r2 )        \ ? overflow error
                mov     ecx, FSP_MEMORY 
                sub     ecx, # B/FLOAT
                js      L$1
                fldl2e
                fld     FSIZE FSTACK_MEMORY 
                fld1
                fcom    st(1)
                fstsw   ax
                sahf
                jbe     L$4
                fchs
                fcomp   st(1)
                fstsw   ax
                sahf
                jnc     L$5
                je      L$5
                fmulp   st(1), st(0)
                f2xm1
                fstp    FSIZE FSTACK_MEMORY 
                ( fwait )
                jmp     L$2


L$4:            fstp    st(0)                                   \ 2
L$5:            fmulp   st(1), st(0)                            \ 1
                fld     st(0)           \ duplicate exponent    \ 2
                jp      L$3
                frndint                 \ take integer part     \ 2
\ June 5th, 1996 - 16:03 tjz changed to FSUB which should be correct, since
\ the FSUBR instruction was used to correct for a bug in the MPE assembler
                fsub    st(1), st       \ get fractional part   \ 2
\               fsubr   st(1), st       \ get fractional part   \ 2
                fld1                                            \ 3
                fscale                  \ 2**int                \ 3
                fstp    st(1)           \ remove unneeded part  \ 2
                fxch    st(1)           \ frac                  \ 2
                f2xm1                   \ (2**frac) - 1         \ 2
                fld1                    \ 1.0                   \ 3
                faddp   st(1), st       \ 2**frac               \ 2
                fmulp   st(1), st       \ 2**(int + frac)       \ 1
                fld1
                fsubp   st(1), st       \ Should be fsubrp ???????
                fstp    FSIZE FSTACK_MEMORY                 \ 0
                ( fwait )
                jmp     L$2


L$3:            fstp    st(1)
                jmp     L$2


L$1:            mov     esi, # ' FSTKUFLO >body
                add     esi, edi
L$2:            next,
                end-code


code FEXP       ( fs: r1 -- r2 )        \ ? error for x too large
                mov     ecx, FSP_MEMORY 
                sub     ecx, # B/FLOAT
                js      L$1
                fldl2e                  \ log base 2 of e       \ 1
                fld     FSIZE FSTACK_MEMORY                 \ 2
                fmulp   st(1), st       \ modified exponent     \ 1
                fld     st(0)           \ duplicate exponent    \ 2
                frndint                 \ take integer part     \ 2
\ June 5th, 1996 - 16:03 tjz changed to FSUB which should be correct, since
\ the FSUBR instruction was used to correct for a bug in the MPE assembler
                fsub    st(1), st       \ get fractional part   \ 2
\               fsubr   st(1), st       \ get fractional part   \ 2
                fld1                                            \ 3
                fscale                  \ 2**int                \ 3
                fstp    st(1)           \ remove unneeded part  \ 2
                fxch    st(1)           \ frac                  \ 2
                f2xm1                   \ (2**frac) - 1         \ 2
                fld1                    \ 1.0                   \ 3
                faddp   st(1), st       \ 2**frac               \ 2
                fmulp   st(1), st       \ 2**(int + frac)       \ 1
                fstp    FSIZE FSTACK_MEMORY                 \ 0
                ( fwait )
                jmp     L$2


L$1:            mov     esi, # ' FSTKUFLO >body
                add     esi, edi
L$2:            next,
                end-code


: FCOSH         ( f: r1 -- r2 )         \ ? error for x too large
        fabs fexp f1.0 fover f/ f+ f2/ ;


: FTANH         ( f: r1 -- r2 )         \ should be OK for legal args
        fdup fabs f1.0 f<
        IF      f2* fexpm1 fdup f2.0 f+ f/
        ELSE    f2* fdup f0<
                IF      fexpm1 fdup f2.0 f+ f/
                ELSE    fnegate fexp fdup f1.0 fswap f-  \ *** rls Sep. 21, 1997
                        fswap f1.0 f+ f/
                THEN
        THEN ;


code FATAN      ( fs: r1 -- r2 )        \ should be OK for legal args
                mov     ecx, FSP_MEMORY 
                sub     ecx, # B/FLOAT
                js      L$1
                fld     FSIZE FSTACK_MEMORY 
                fld1
                fpatan
                fstp    FSIZE FSTACK_MEMORY 
                ( fwait )
                jmp     L$2


L$1:            mov     esi, # ' FSTKUFLO >body
                add     esi, edi
L$2:            next,
                end-code


code FACOS      ( fs: r1 -- r2 )        \ ? error for |x| > 1
                mov     ecx, FSP_MEMORY 
                sub     ecx, # B/FLOAT
                js      L$1
                fld     FSIZE FSTACK_MEMORY 
                fld     st(0)
                fld     st(0)
                fmulp   st(1), st(0)
                fld1
                fsubpr  st(1), st
                fsqrt
                fabs
                fxch    st(1)
                fpatan
                fstp    FSIZE FSTACK_MEMORY 
                ( fwait )
                jmp     L$2


L$1:            mov     esi, # ' FSTKUFLO >body
                add     esi, edi
L$2:            next,
                end-code


code FASIN      ( fs: r1 -- r2 )        \ ? error for |x| > 1
                mov     ecx, FSP_MEMORY 
                sub     ecx, # B/FLOAT
                js      L$1
                fld     FSIZE FSTACK_MEMORY 
                fld     st(0)
                fld     st(0)
                fmulp   st(1), st(0)
                fld1
                fsubpr  st(1), st
                fabs
                fsqrt
                fpatan
                fstp    FSIZE FSTACK_MEMORY 
                ( fwait )
                jmp     L$2


L$1:            mov     esi, # ' FSTKUFLO >body
                add     esi, edi
L$2:            next,
                end-code



code F+!        ( addr -- ) ( fs: r -- )        \ ? overflow error
                mov     ecx, FSP_MEMORY 
                sub     ecx, # B/FLOAT
                js      L$1
                fld     FSIZE FSTACK_MEMORY 
                fld     FSIZE DATASTACK_MEMORY 
                faddp   st(1), st
                fstp    FSIZE DATASTACK_MEMORY 
                mov     FSP_MEMORY , ecx
                pop     ebx
                ( fwait )
                jmp     L$2


L$1:            pop     ebx
                mov     esi, # ' FSTKUFLO >body
                add     esi, edi
L$2:            next,
                end-code


\ : f+!           ( addr -- ) ( F: r -- ) \ high level but fast enough
\                 dup>r f@ f+ r> f! ;


: (fsinh)       ( f: r1 -- r2 )
        fexp f1.0 fover f/ f- f2/ ;


: FSINH         ( f: r1 -- r2 )         \ ? overflow error ?
        fdup f0<
        IF      fabs (fsinh) fnegate
        ELSE    (fsinh)
        THEN ;



: FATANH        ( f: r1 -- r2 )         \ OK for valid args
                fdup f1.0 f+ fdup f0=
                IF      fdrop fdrop finf fnegate EXIT
                THEN
                fswap f1.0 f- fdup f0=
                IF      fdrop fdrop finf EXIT
                THEN
                f/ fln f2/ ;


\ July 29th, 1998 - 8:53 tjz
\ added '@' after FLOATSP to fix a bug reported by Pierre Abbat

: F~            ( f: r1 r2 r3 -- )  ( -- flag )
        fdup f0<
        IF      fabs fover fabs 3 fpick fabs f+ f*      \ r1 r2 r3*(r1+r2)
                frot frot f- fabs fswap f<
        ELSE    fdup f0=
                IF      fdrop FLOATSTACK FLOATSP @ + B/FLOAT - dup B/FLOAT -
                        swap B/FLOAT swap B/FLOAT compare 0=
                        fdrop fdrop
                ELSE    \ January 16th, 1998 rls add "fswap"
                        frot frot f- fabs fswap f<
                THEN
        THEN ;


\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ floating point defining words and array operators
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\


: FALIGN                ( -- ) ;
: FALIGNED              ( addr -- addr ) ;
: DFALIGN               ( -- ) ;
: DFALIGNED             ( addr -- addr ) ;
: SFALIGN               ( -- ) ;
: SFALIGNED             ( addr -- addr ) ;


: FLOAT+        ( addr1 -- addr2 )
        B/FLOAT + ;


: FLOATS        ( n1 -- n2 )
        B/FLOAT * ;


: DFLOAT+       ( addr1 -- addr2 )
        8 + ;


: DFLOATS       ( n1 -- n2 )
        8 * ;


: SFLOAT+       ( addr1 -- addr2 )
        4 + ;


: SFLOATS       ( n1 -- n2 )
        4 * ;


: float-array   ( n1 -<name>- )         \ compile time
                ( -- a1 )               \ runtime
                create floats allot ;


\ pointer to a float primitives


cfa-func ^float@  2 cells+ @ f@  ;
cfa-func ^float!    cell+  @ f!  ;
cfa-func ^float+!          @ f+! ;


\ float primitives


cfa-func float@  2 cells+ f@  ;
cfa-func float!    cell+  f!  ;
cfa-func float+!          f+! ;


\ pointer to a float array primitives


cfa-func ^#float@  2 cells+ @ swap floats + f@  ;
cfa-func ^#float!    cell+  @ swap floats + f!  ;
cfa-func ^#float+!          @ swap floats + f+! ;


\ float array primitives


cfa-func #float@  2 cells+ swap floats + f@  ;
cfa-func #float!    cell+  swap floats + f!  ;
cfa-func #float+!          swap floats + f+! ;


: floater?      ( a1 -- a1 f1 )     \ is a1 the CFA of a float? or ^float?
                dup @
                dup   float@ =
                swap ^float@ = or ;


: #float?       ( a1 -- a1 f1 )     \ is a1 the CFA of a #float? or #^float?
                dup @
                dup   #float@ =
                swap ^#float@ = or ;


: ?float        ( cfa -- cfa )          \ abort if not a FLOAT
                floater? 0= abort" is not a FLOAT or ^FLOAT" ;


: ?#float       ( cfa -- cfa )          \ abort if not a #FLOAT-ARRAY
                #float? 0= abort" is not a #FLOAT-ARRAY or #^FLOAT-ARRAY" ;


: ^float        ( a1 -<name>- )            \ compile time          64-bits
                ( -- ) ( F: -- r )   \ runtime
                header ^float@ , ^float! , ^float+! , ( a1 ) , ;


: #^float-array ( n1 -<name>- )         \ compile time          64-bits
                ( n1 -- ) ( F: -- r )   \ runtime
                header ^#float@ , ^#float! , ^#float+! , ( n1 ) , ;


: FLOAT         ( -<name>- )            \ compile time          64-bits
                ( -- ) ( F: -- r )   \ runtime
                header float@ , float! , float+! ,
                1 floats here over erase allot ;


: #float-array  ( n1 -<name>- )         \ compile time          64-bits
                ( n1 -- ) ( F: -- r )   \ runtime
                header #float@ , #float! , #float+! ,
                ( n1 ) 1+ floats here over erase allot ;


: f->           ( n1 -<name>- )         \ store a value into a float
\in-system-ok   ' ?float  1 cells+ cfa-comp, ; immediate


: f+>           ( n1 -<name>- )         \ increment the value of a float
\in-system-ok   ' ?float  2 cells+ cfa-comp, ; immediate


: f#->          ( n1 -<name>- )         \ store a value into a float
\in-system-ok   ' ?#float  1 cells+ cfa-comp, ; immediate


: f#+>          ( n1 -<name>- )         \ increment the value of a float
\in-system-ok   ' ?#float  2 cells+ cfa-comp, ; immediate



\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ Floating point number conversion. We do not have the floating point
\ integrated with the interpreter, so we must prefix floating point
\ literals with f#. ( f# 1.23E6 )
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\


code f**+n     ( f: r1 -- r2 ; n -- )
                mov     ecx, FSP_MEMORY 
                sub     ecx, # B/FLOAT
                js      L$4
                fld1
                or      ebx, ebx
                je      L$5
                fld     FSIZE FSTACK_MEMORY 
                fxch    st(1)
L$1:            shr     ebx, # 1
                jnc     L$2
                fmul    st(0), st(1)
L$2:            jz      L$3
                fxch    st(1)
                fmul    st(0), st(0)
                fxch    st(1)
                jmp     L$1


L$3:            fxch    st(1)
                fstp    st(0)
L$5:            fstp    FSIZE FSTACK_MEMORY 
                jmp     L$6


L$4:            mov     esi, # ' FSTKUFLO >body
                add     esi, edi
L$6:            pop     ebx
                ( fwait )
                next,
                end-code


: f**n     ( F: r1 -- r2 ; n -- )  \ Floating number raised to integer power.
        DUP 0<
        IF      ABS F**+N F1.0 FSWAP F/
        ELSE    F**+N
        THEN ;

\ : F**           ( F: r1 r2 -- r3 )
\                fdup fround fdup f>s f-         \ r1 r4
\                fdup f0= 0=
\                IF      \ non-zero fractional part of exponent
\                        fover fabs fln f* fexp fswap f**n f*
\                ELSE    fdrop f**n
\                THEN ;


\ rbs January 26th, 2003 -->
fpi f2/ fconstant fpi/2
fpi f2* fconstant f2pi

: f0<>  f0= 0= ;

: f^2   fdup f* ;


: f^x   fswap fln f* fexp ;

synonym f** f^x 

: fsqr  f0.5 f^x ;


: 1/f   -1 f**n ;


: f>r   r> rp@ b/float - rp! rp@ f! >r ;


: fdup>r  r> fdup rp@ b/float - rp! rp@ f! >r ;

: r>f   r> rp@ f@ b/float rp@ + rp! >r ;

: fr@   r> r@ f@ >r ;
\ <-- rbs

fvariable ftemp
create $ftemp 128 allot
create fbcd-buf 10 allot


: FALOG         ( f: r1 -- r2 )         \ ? overflow error ?
                f10.0 fswap f** ;


: 10**N-0.5     ( f: -- r )  ( n -- )
                f10.0 f**n f0.5 f- ;


code f>bcd      ( f: r -- )  ( addr -- )
                mov     ecx, FSP_MEMORY 
                sub     ecx, # B/FLOAT
                js      L$4
                mov     FSP_MEMORY , ecx
                fld     FSIZE FSTACK_MEMORY 
                fbstp   DATASTACK_MEMORY 
                jmp     L$5


L$4:            mov     esi, # ' FSTKUFLO >body
                add     esi, edi
L$5:            pop     ebx
                ( fwait )
                next,
                end-code


: bcd-char@     ( n -- char )
                dup 1 and swap 2/ fbcd-buf + c@ swap
                IF      4 rshift
                ELSE    0x0f and
                THEN
                ascii 0 + ;


: bcd-char!     ( char n -- )
                swap 0x0f and swap
                dup 1 and swap 2/ fbcd-buf + swap
                IF      dup c@ 0x0f and rot 4 lshift
                ELSE    dup c@ 0xf0 and rot
                THEN
                or swap c! ;


B/FLOAT 10 =                            nostack
[IF]    4932 constant big-power
[ELSE]  308  constant big-power
[THEN]                                  stack-check


: f*10**n       ( n -- )  ( f: f1 -- f2 )
        dup big-power >
        IF      big-power f10.0 f**n f*
                big-power - f10.0 f**n f*
        ELSE    f10.0 f**n f*
        THEN ;


: rep-normal    ( addr u -- n true )
        1 max 2dup ascii 0 fill 18 umin
        fabs fdup dup 10**n-0.5 f<
        IF      fdup dup dup 1- 10**n-0.5 f<
                IF      fdup flog floor f>s 1+ dup>r
                        - f*10**n
                ELSE    >r
                THEN
        ELSE    fdup flog floor f>s 1+ dup>r
                over - f10.0 f**n f/
        THEN
        fround fdup f10.0 dup f**n f<
        IF      fdup f10.0 dup 1- f**n f<
                IF      f10.0 f* r> 1- >r
                THEN
        ELSE    f10.0 f/ r> 1+ >r
        THEN
        fbcd-buf f>bcd dup>r + 1- r> 0
        DO      I bcd-char@ over c! 1-
        LOOP
        drop r> true
        ;


: rep-denormal   ( addr u -- n true )
        rep-normal ;


\ February 6th, 1996 - 18:05 tjz added 'FDROP' to the following def to
\ correct for a floating point zero left on the stack during E. and F.S
: rep-zero   ( addr u -- 0 true )  ( f: r -- )
        ascii 0 fill 0 true fdrop ;
        
: rep-spec   ( addr u cstr -- n false )  ( f: r -- )
        >r 2dup blank r> count                  \ addr1 u addr2 v
        rot umin >r swap r> move
        0x7fffffff 0 fdrop ;


: REPRESENT   ( addr u -- n flag1 flag2 ) ( f: r -- )
        fexam 8 rshift                          \ get type of operand
        dup 2 and 0<> >r                        \ save sign = flag1
        fabs
        0x45 and                                \ clear C1
        CASE    0x00    OF      c" Unsupported" rep-spec        ENDOF
                0x01    OF      c" NAN"         rep-spec        ENDOF
                0x04    OF      rep-normal                      ENDOF
                0x05    OF      c" Infinity"    rep-spec        ENDOF
                0x40    OF      rep-zero                        ENDOF
                0x41    OF      c" Empty"       rep-spec        ENDOF
                0x44    OF      rep-denormal                    ENDOF
                ( else )        c" Unknown"     rep-spec        
        ENDCASE
        r> swap ;


: xsign         ( char -- char false | nflag true )
        CASE    ascii + OF      false true              ENDOF
                ascii - OF      true  true              ENDOF
                ( else )        false over
        ENDCASE ;


: e-char        ( char -- true | char false )
        CASE    ascii D         OF      true            ENDOF
                ascii d         OF      true            ENDOF
                ascii E         OF      true            ENDOF
                ascii e         OF      true            ENDOF
                ( else ) false over
        ENDCASE ;


: 10digit       10 digit ;


: digit0        dup ascii 0 = ;


variable expsign
variable intcnt
variable fracnt
variable expcnt
variable charcnt
variable zerochar
variable havedigits
create $fsignif 128 allot


: next-char     ( addr1 -- addr2 char flag ) \ flag = 0 means valid char
                count -1 charcnt +!  charcnt @ 0< ;


code sig>f
                fbld    fbcd-buf [edi]
                mov     ecx, FSP_MEMORY 
                fstp    FSIZE FSTACK_MEMORY 
                ( fwait )
                add     ecx, # B/FLOAT
                mov     FSP_MEMORY , ecx
                next,
                end-code


: >float-int    ( f: -- r )
                intcnt @ 18 min dup 1- swap 0
                ?DO     $fsignif I + c@ over bcd-char! 1-
                LOOP
                drop sig>f
                intcnt @ 18 >                           \ allow looong input
                IF      $fsignif 18 + c@ 5 >=
                        IF      $fsignif 18 + c@ 5 >
                                IF      f1.0 f+
                                ELSE    $fsignif 17 + c@ 1 and
                                        IF      f1.0 f+
                                        ELSE    intcnt @ 19
                                                ?DO     $fsignif I + c@
                                                        0<>
                                                        IF      f1.0 f+
                                                                LEAVE
                                                        THEN
                                                LOOP
                                        THEN
                                THEN
                        THEN
                        f10.0 intcnt @ 18 - f**n f*
                THEN ;  


: >float-int.frac   ( f: -- r )
                >float-int f10.0 fracnt @ negate f**n f* ;


: init->float   ( -- )
                0 expsign !  0 intcnt !                 \ initialize various
                0 fracnt !  0 expcnt !                  \ counts and such
                0 zerochar !
                fbcd-buf 10 erase                       \ clear bcd buffer
                $fsignif 128 erase                      \ clear intermed. buf
                ;


: >FLOAT        ( addr len -- f )  ( f: -- r | <nothing> )
        false havedigits !
        dup 0=
        IF      drop drop false EXIT
        THEN
        128 umin charcnt !                      \ save character count
        init->float
        next-char drop                          \ get first character
        dup bl =
        IF      drop charcnt @ bl skip charcnt !
                next-char
                IF      drop drop f0.0
                        true EXIT               \ special case of 0.0
                THEN
        THEN
        xsign
        IF      IF      8       ELSE    0  THEN
                19 bcd-char!
                next-char
                IF      drop drop false EXIT
                THEN
        THEN
        BEGIN   digit0                          \ check for leading 0's
        WHILE   drop next-char
                IF      drop drop f0.0 true EXIT
                THEN
                true zerochar !
                true havedigits !
        REPEAT
        10digit
        IF      true havedigits !
                BEGIN   $fsignif intcnt @ + c!
                        1 intcnt +!
                        next-char
                        IF      drop drop >float-int true EXIT
                        THEN
                        10digit 0=
                UNTIL
                false zerochar !
\        ELSE    drop drop false EXIT     \ *** rls September 25th, 1997 - 9:57
        THEN
        dup ascii . =                           \ decimal point?
        IF      drop next-char
                IF      drop drop >float-int true EXIT
                THEN
                intcnt @ 0=
                IF
                        BEGIN   digit0
                        WHILE   drop next-char
                                IF      drop drop f0.0 true EXIT
                                ELSE    1 fracnt +!
                                THEN
                                true zerochar !
                                true havedigits !
                        REPEAT
                THEN
                10digit
                IF      true havedigits !
                        false zerochar !
                        BEGIN   $fsignif intcnt @ + c!
                                1 intcnt +!
                                1 fracnt +!
                                next-char
                                IF      drop drop >float-int.frac
                                        true EXIT
                                THEN
                                10digit 0=
                        UNTIL
                THEN
        THEN
        e-char
        IF                                      \ exponent indicator
                havedigits @ 0=
                IF      drop false EXIT         \ September 25th, 1997 - 9:48 tjz
                THEN
\                zerochar @                      \ optimization, mantissa=0
\                IF      drop f0.0 true EXIT     \ then whole number is zero
\                THEN                            \ unfortunately skips validation
                intcnt @ 0=
                IF      ( SMuB 07-20-95 drop ) f1.0 19 bcd-char@
                        ascii 0 <>  ( <-- SMuB 07-20-95 )
                        IF      fnegate
                        THEN
                ELSE    >float-int
                THEN
                next-char
                IF      drop drop fracnt @
                        IF      f10.0 fracnt @ negate f**n f*
                        THEN
                        zerochar @                      \ mantissa=0?
                        IF      fdrop f0.0              \ then make result 0.0
                        THEN
                        true EXIT
                THEN
                xsign
                IF      >r next-char
                        IF      r>drop drop drop
                                fracnt @
                                IF      f10.0 fracnt @
                                        negate f**n f*
                                THEN
                                zerochar @              \ mantissa=0?
                                IF      fdrop f0.0      \ then make result 0.0
                                THEN
                                true EXIT
                        THEN
                ELSE    0 >r
                THEN
        ELSE    >float-int
                xsign
                IF      >r next-char        \ rls 09-30-97
                        IF    \ rls r>drop drop drop fdrop false EXIT
                              r>drop drop ( SMuB 07-20-95 --> ) drop    \ rls
                              fracnt @                                  \ rls
                              IF      f10.0 fracnt @                    \ rls
                                      negate f**n f*                    \ rls
                              THEN                                      \ rls
                              true EXIT                                 \ rls
                        THEN
                ELSE    drop drop fdrop false EXIT
                THEN
        THEN
        drop 1- charcnt @ 1+ number?
        double? 0= and                  \ October 1st, 1996 - 10:51 tjz & am
                                        \ double exponent not allowed
        IF      d>s r>
                IF      negate
                THEN
                fracnt @ negate + f10.0 f**n f*
                zerochar @                       \ mantissa=0?
                IF      fdrop f0.0               \ then make result 0.0
                THEN
                true
        ELSE
                2drop fdrop false r>drop EXIT
        THEN ;

: f#            ( F: -- r )
                bl word count >float
                0= abort" invalid floating point number"
                state @
                IF  postpone fliteral  THEN ;   immediate



\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ Output conversion.
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\


variable sigdigits      6 sigdigits !   \ default precision

B/FLOAT 10 = [IF]  18  [ELSE]  16  [THEN]  constant maxsig


: PRECISION     ( -- n )
                sigdigits @ ;


: SET-PRECISION ( n -- )
                sigdigits ! ;


: F.            ( F: r -- )
                fdepth 0 <=
                IF      ." Empty " EXIT
                THEN
                precision 1 max set-precision
                fexam 0x0200 and
                IF      fabs ." -"
                THEN
                fdup f0.5 f<
                IF      ." ." f1.0 f+ $ftemp
                        precision 1+ maxsig umin
                        represent
                        drop drop drop
                        $ftemp 1+ precision maxsig 1- umin
                        type space
                ELSE    $ftemp precision represent 0=
                        IF      drop drop $ftemp precision
                                type space
                        ELSE    drop dup precision <
                                IF      dup 0=
                                        IF      drop ." ."
                                                $ftemp precision
                                                type space EXIT
                                        THEN
                                        $ftemp over type ." ."
                                        $ftemp over + swap precision
                                        swap - type space
                                ELSE    dup precision =
                                        IF      $ftemp swap type ." . "
                                                EXIT
                                        THEN
                                        $ftemp precision type
                                        precision - 0
                                        DO      ascii 0 emit
                                        LOOP
                                        ." . "
                                THEN
                        THEN
                THEN ;


: FE.           ( F: r -- )     \ display fp # in engineering notation
                precision 3 max set-precision
                fexam 0x0200 and
                IF      fabs ." -"
                THEN
                $ftemp precision represent 0=
                IF      drop drop
                        $ftemp precision type space EXIT
                THEN
                drop 1- 3 /mod swap 1+ $ftemp over type ." ."
                $ftemp over + swap precision swap - type
                ." E" 3 * base @ >r decimal . r> base ! ;


: E.            ( f: r -- )
                precision 1 max set-precision
                fexam 0x0200 and
                IF      fabs ." -"
                THEN
                $ftemp precision represent 0=
                IF      drop drop
                        $ftemp precision type space EXIT
                THEN
                drop 1- $ftemp 1 type ." ."
                $ftemp 1+ precision 1- type
                ." E" base @ >r decimal . r> base ! ;


: G.            ( f: r -- )
                fdup fabs f10.0 precision 2/ negate f**n f<
                IF      e. EXIT
                THEN
                fdup fabs f10.0 precision f**n f<
                IF      f.
                ELSE    e.
                THEN ;


: FS.           ( f: r -- r )   \ display fp # in scientific notation
                e. ;


6 value show-fp-depth


: f.s           ( -- )  \ display floating point stack
                fdepth
                IF      fdepth cr ." {" 1 .r ." } "
                show-fp-depth fdepth umin dup 1- swap 0
                        DO      10 ?cr
                                dup i - fpick g.
                        LOOP
                        drop
                ELSE    ." Empty fp stack "
                THEN ;


: .fdepth       ( -- )
                ." Items; " fdepth .
                ." Bytes: " FLOATSP @ . ;


also bug


' .fdepth is dbg-fstack


environment definitions


: FLOATING TRUE ;


: FLOATING-EXT TRUE ;


fstack-elements constant FLOATING-STACK


fbig FCONSTANT MAX-FLOAT


previous hidden definitions


\ ******************************************************************
\ Debugging tools
\ ******************************************************************


: (tag)   ( n1 addr -- n2 )          \ Returns the 2 bit tag for ST(n1)
        dup>r cell+ @ 0x3800 and 0x0800 / +
        7 and
        R> 2 cells+ @ swap 0
        ?DO     2/ 2/
        LOOP
        3 and ;


: .ftag2   ( i -- )
        allregs (tag)
        s" valid  zero   specialempty  " 7 s. ;


: npu-depth   ( -- n )
\        FR1 >FREGS>
        0 0 7
        DO      I allregs (tag) 3 <>
                IF      DROP I 1+ LEAVE
                THEN
        -1 +LOOP ;


77 CONSTANT FPDISPSIZE
allregs 7 cells+ CONSTANT npstk


: .tenbyte   ( addr -- )               \ Needs lots of work yet.
        10 + 10 0
        DO      1- dup c@ s>d <# # # #> type space
        LOOP
        drop ;


: h.npustack   ( -- )
        8 0
        DO      cr ." ST(" I 1 .r ." ) "
                npstk I B/FLOAT * + .tenbyte
                I .ftag2
        LOOP
        ;


: .fmask   ( n -- )
        0 5
        DO      space dup i rshift 1 and
                i s" PrecUfloOfloZeroDnrmInvl" 4 s.
                ." =" 1 .r
        -1 +LOOP
        drop ;


: .fstatus   ( n -- )
        ."  Flags: "
        0 7
        DO      dup i rshift 1 and
                i s" ESSFPEUEOEZEDEIE" 2 s. ." =" .
        -1 +LOOP
        drop ;


: .ftag1   ( n -- )
        4 base !
        0 7
        DO      dup i 2* rshift 3 and .
        -1 +LOOP
        drop ;


: fdump ( -- )      \ Dump of the real Floating Point Unit
        cr cr ."      Dump of the Floating Point NPU "
        allregs >fregs>  cr
        base @ >r
        allregs @ dup
        ." Cntl:  " hex 0x0ffff and 4 .r
        dup 10 rshift 3 and
        ."  Rnd-" s" neardownup  chop" 4 s.
        dup 8 rshift 3 and
        ."  Size = " s" sngl????dbl ext " 4 s.
        cr ."  Msk: " .fmask
        cr allregs cell+ @
        ." Status: " 0x0ffff and dup 4 .r space
        ." CC = " dup 8 rshift dup 0x40 and
        IF      7 and 8 or
        ELSE    7 and
        THEN
        binary s>d <# # # # # #> type hex
        ."  Busy = " dup 15 rshift .
        ."  Top = " dup 11 rshift 7 and .
        cr .fstatus
        cr ." Tags: "
        allregs 2 cells+ @ 0xffff and .ftag1
        cr ." IP = "
        allregs 3 cells+ @ dup 8 .r
        ."   IP(rel) = " abs>rel 8 .r
        cr ." Last Opcode = "  hex
        allregs 4 cells+ @ dup 24 rshift
        0xd8 or 0x0ff and 3 .r
        dup 16 rshift 0x0ff and 3 .r
        cr ."  CS Selector: " 0x0ffff and 4 .r
        cr ." Data Pointer: "
        allregs 5 cells+ @ dup 8 .r
        ."   DP(rel) = " abs>rel 8 .r
        cr ." Operand Pointer: "
        allregs 6 cells+ @ 0x0ffff and 4 .r
        decimal
        cr ." NPU stack depth = " npu-depth .
        ."   Simulated stack depth = " fdepth .
        hex h.npustack cr
\        allregs 108 dump
        r> base ! ;


: .ftempx        ( -- )
        0 B/FLOAT 1-
        DO      ftemp I + c@ h.2 space
        -1 +LOOP ;


: f.x   ( f: r1 r2 ... rn -- r1 r2 ... rn )     \ Hex dump of fstack
        fdepth
        IF      fdepth cr ." {" 1 .r ." } "
                show-fp-depth fdepth umin dup 1- swap 0
                DO      cr dup i - fpick fdup ftemp f!
                        .ftempx 2 spaces g.
                LOOP
                drop
        ELSE    ." Empty fp stack "
        THEN ;


initialization-chain chain-add finit
finit           \ Initialize the fpu


\ If you don't understand what the following definition is doing, don't
\ concern yourself. It is moving a floating point number from a1 into
\ the body of its own definition so it can display it easily.


: .onefloat     ( -- r1 )
                f# 0 g. ;


: see.float     ( a1 -- a2 )
                dup ['] .onefloat >body cell+ CELLS/FLOAT cells move
                .onefloat
                CELLS/FLOAT cells+ ;


' see.float is .float


: FLOAT-NUMBER? ( a1 n1 f1 -- )                 \ input parameters
                ( -- a1 n1 TRUE )               \ on already converted
                ( -- a1 n1 FALSE )              \ on failed to convert
                ( -- TRUE ) ( f: -- r1 )        \ on a float
                FALSE to float?                 \ need to initialize to non-floating#
\ floating point number should only be accepted if we are in DECIMAL
\ they can still be entered manually using F# <number>
                base @ 0x0A <> ?EXIT            \ only in decimal
                dup ?EXIT drop                  \ leave if already converted
\ September 24th, 1997 - 11:59 tjz added
\ interpretive number conversion is explicitly more restrictive than >FLOAT,
\ so we add an extra test for the presence of the required letter 'E' in a
\ floating point string.
                2dup 'E' scan nip dup 0= ?EXIT drop
                2dup >FLOAT 0=
                if      FALSE                   \ failed all
                else    TRUE to float?          \ turn on converted flag
                        2drop                   \ discard original string
                        TRUE                    \ return true flag
                then    ;


number?-chain chain-add float-number?           \ link into number conversion chain


: _FLOAT-NUMBER, ( d1 -- d1 )           \ interpreting a double
                 ( d1 -- )              \ compiling a double
                 ( f: r1 -- r1 )        \ interpreting a float
                 ( f: r1 -- )           \ compiling a float
                float? 0=
                if      _number,
                else    state @
                        if      postpone fliteral
                        then
                then    ;


' _float-number, is number,             \ link into interpreter



: FLOAT-RESET-STACKS ( ? -- )
                finit
                _reset-stacks ;


' float-reset-stacks is reset-stacks    \ link into interpreter


: fdiscard-number ( ?? -- ?? )  \ discard a floating point number if present
                float?
                if      fdrop
                else    _discard-number
                then    ;


' fdiscard-number is discard-number

only forth also definitions

\ FCALL from Bill McCarthy <WJMc@pobox.com> March 15th, 1999

code fcallret   ( n1 -- ) ( fpu: r -- ) ( f: -- r )
                mov     ecx, FSP_MEMORY
                fstp    FSIZE FSTACK_MEMORY
                add     ecx, # B/FLOAT
                mov     FSP_MEMORY , ecx
                pop     ebx
                ( fwait )
                next,
                end-code

code FS>DS      ( -- d )  ( f: r -- )   \ move floating point number bits to
                                        \ data stack as a 64-bit float
                mov     ecx, FSP_MEMORY
                sub     ecx, # B/FLOAT
                js      L$1
                fld     FSIZE FSTACK_MEMORY
                mov     FSP_MEMORY , ecx
                push    ebx
                push    ebx
                push    ebx
                fstp    double 0 [esp]
                ( fwait )
                pop     ebx
                jmp     L$2
L$1:            mov     esi, # ' FSTKUFLO >body
                add     esi, edi
                pop     ebx
L$2:            next,
                end-code

\ Thanks to Andrew McKewan for the following idea:

: fcall postpone call s" fcallret" evaluate ; immediate

\ from Jos v.d. Ven

\ Push the top of the float stack onto the data stack as a 32-bit float
code SFS>DS     ( -- float )  ( fs: r -- )    \  10 b/float
                mov     ecx, FSP_MEMORY
                sub     ecx, # B/FLOAT
                js      L$1
                fld     FSIZE FSTACK_MEMORY
                mov     FSP_MEMORY , ecx
                push    ebx
                push    ebx
                fstp    float 0 [esp]
                ( fwait )
                pop     ebx
                jmp     L$2
L$1:            mov     esi, # ' FSTKUFLO >body
                add     esi, edi
                pop     ebx
L$2:            next,
                end-code

