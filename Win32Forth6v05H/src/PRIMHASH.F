\ PRIMHASH.F    primitive hash functions
\ primhash.f 11/02/2003 22:33:59 arm Moved primitives from the kernel

cr .( Loading Primitive Hash...)
cr .( -- BETA PRIMHASH.F 4.1B)

\ ---------------- 32-bit Hash Function for Objects ----------------

CODE METHOD-HASH ( addr len -- hash-val )
                mov     ecx, ebx                \ ecx = count
                pop     ebx                     \ ebx = address
                mov     eax, ecx                \ eax = result (includes count)
                jecxz   short @@3
@@1:            rol     eax, # 7
                xor     al, 0 [ebx] [edi]       \ add in next    c; byte
                inc     ebx
                dec     ecx
                jnz     short @@1
@@3:            test    eax, eax                \ make sure it's negative
                js      short @@2
                not     eax
@@2:            mov     ebx, eax
                next    c;

\ -------------------- Method/Ivar Search --------------------

CODE ((FINDM))  ( SelID addr -- 0cfa t | f )
                pop     eax
@@1:            mov     ebx, 0 [ebx] [edi]      \ follow link
                test    ebx, ebx                \ end of list?
                je      short @@2
                cmp     eax, 4 [ebx] [edi]      \ selectors match?
                jne     short @@1
                add     ebx, # 8                \ method cfa
                push    ebx
                mov     ebx, # -1               \ and true flag
@@2:            next    c;

\ -------------------- Runtime for Methods --------------------
\ m0cfa is executed when the object address is on the stack
\ m1cfa is executed when the ivar offset is compile in-line

\ CREATE M0CFA
CFA-CODE M0CFA
\        0 OFA-H !               \ disable OFA resolution
\                INIT-ASSEMBLER
                0 OFA-LAST !
                xchg    esp, ebp
                push    esi
                push    OP [UP]
                push    LP [UP]
                xchg    esp, ebp
                mov     LP [UP]  , ebp          \ load new local pointer
                mov     OP [UP] , ebx           \ get object address
                pop     ebx
                lea     esi, 16 [eax] [edi]      \ get new ip (skip m1cfa)
                movzx   eax, byte ptr -8 [esi]   \ get # of uninitialized locals
                lea     eax, [eax*4]            \ cell size
                sub     ebp, eax                \ allocate non-initialized
                movzx   ecx, byte ptr -7 [esi]   \ get # of initialized locals
                mov     eax, -4 [esi]            \ optimised next
                or      ecx, ecx
                jne     short @@2               \ do initialised
                exec                            \ exit to next word
@@2:            push    ebx
@@1:            pop     -4 [ebp]                \ push on return stack
                sub     ebp, # 4
                dec     ecx
                jnz     short @@1
                pop     ebx
                exec    c;

\ CREATE M1CFA
CFA-CODE M1CFA
\        0 OFA-H !               \ disable OFA resolution
\                INIT-ASSEMBLER
                0 OFA-LAST !
                mov     ecx, 0 [esi]            \ get inline offset
                add     esi, # 4
                xchg    esp, ebp
                push    esi
                push    OP [UP]
                push    LP [UP]
                xchg    esp, ebp
                mov     LP [UP]  , ebp            \ load new local pointer
                lea     esi, 12 [eax] [edi]      \ get new ip
                add     OP [UP]  , ecx            \ add offset to object address
                movzx   eax, byte ptr -8 [esi]   \ get # of uninitialized locals
                lea     eax, [eax*4]            \ cell size
                sub     ebp, eax                \ allocate non-initialized
                mov     eax, -4 [esi]            \ optimised next
                movzx   ecx, byte ptr -7 [esi]   \ get # of initialized locals
                or      ecx, ecx
                jne     short @@2               \ do initialised
                exec                            \ exit to next word
@@2:            push    ebx
@@1:            pop     -4 [ebp]                \ push on return stack
                sub     ebp, # 4
                dec     ecx
                jnz     short @@1
                pop     ebx
                exec    c;

\ end of method definition

CODE EXITM      ( -- )
                mov     ebp, LP [UP]
                xchg    esp, ebp
                pop     LP [UP]                   \ restore local pointer
                pop     OP [UP]                   \ restore object pointer
                pop     esi                       \ restore ip
                xchg    esp, ebp
                next    c;
    -1 ' EXITM >OFA !                            \ no optimisation

CODE UNNESTM    ( -- )
                mov     ebp, LP [UP]
                xchg    esp, ebp
                pop     LP [UP]                   \ restore local pointer
                pop     OP [UP]                   \ restore object pointer
                pop     esi                       \ restore ip
                xchg    esp, ebp
                next    c;
    -1 ' UNNESTM >OFA !                          \ no optimisation

\ -------------------- Runtime for Objects --------------------

\ CREATE DOOBJ    ( -- addr )
\                INIT-ASSEMBLER
CFA-CODE DOOBJ
                push    ebx
                lea     ebx, 8 [eax]
                next    c;

\ return the base of the current object

CODE ^BASE      ( -- addr )
                push    ebx
                mov     ebx, OP [UP]
                next    c;

\ -------------------- Runtime for Instance Variables --------------------

\ These are the "non-object" instance variables.  They have the same
\ names and syntax as the regular Forth versions.  They are defined in
\ the META vocabulary and will be found when inside a class definition.
\ The Offset from OP stored at cfa+15
\ bytes runtime

CFA-CODE (&IV)
                push    ebx
                mov     ebx, 4 [eax] [edi]      \ get ivar offset
                add     ebx, OP [UP]            \ add to base of current object
                next    c;

\ single byte (8bit) instance variables

CFA-CODE (IVC@)
                push    ebx
                mov     eax, 4 [eax] [edi]      \ get ivar offset
                add     eax, OP [UP]            \ add to base of current object
                movzx   ebx, byte ptr 0 [eax] [edi] \ fetch value
                next    c;

CFA-CODE (IVC!)
                mov     eax, -4 [eax] [edi]     \ get ivar offset
                add     eax, OP [UP]            \ add to base of current object
                mov     0 [eax] [edi], bl       \ store value
                pop     ebx
                next    c;

CFA-CODE (IVC+!)
                mov     eax, -8 [eax] [edi]     \ get ivar offset
                add     eax, OP [UP]            \ add to base of current object
                add     0 [eax] [edi], bl       \ add value
                pop     ebx
                next    c;

\ word number (16bit) instance variables

CFA-CODE (IVW@)
                push    ebx
                mov     eax, 4 [eax] [edi]      \ get ivar offset
                add     eax, OP [UP]            \ add to base of current object
                movzx   ebx, word ptr 0 [eax] [edi] \ fetch WORD value
                next    c;

CFA-CODE (IVW!)
                mov     eax, -4 [eax] [edi]     \ get ivar offset
                add     eax, OP [UP]            \ add to base of current object
                mov     0 [eax] [edi], bx       \ store WORD value
                pop     ebx
                next    c;

CFA-CODE (IVW+!)
                mov     eax, -8 [eax] [edi]     \ get ivar offset
                add     eax, OP [UP]            \ add to base of current object
                add     0 [eax] [edi], bx       \ add WORD value
                pop     ebx
                next    c;

\ single number (32bit) instance variables

CFA-CODE (IV@)
                push    ebx
                mov     eax, 4 [eax] [edi]      \ get ivar offset
                add     eax, OP [UP]            \ add to base of current object
                mov     ebx, 0 [eax] [edi]      \ fetch value
                next    c;

CFA-CODE (IV!)
                mov     eax, -4 [eax] [edi]     \ get ivar offset
                add     eax, OP [UP]            \ add to base of current object
                mov     0 [eax] [edi], ebx      \ store value
                pop     ebx
                next    c;

CFA-CODE (IV+!)
                mov     eax, -8 [eax] [edi]     \ get ivar offset
                add     eax, OP [UP]            \ add to base of current object
                add     0 [eax] [edi], ebx      \ add value
                pop     ebx
                next    c;

\ double number (64bit) instance variable

CFA-CODE (IVD@)
                push    ebx
                mov     eax, 4 [eax] [edi]      \ get ivar offset
                add     eax, OP [UP]              \ add to base of current object
                mov     ecx, 4 [eax] [edi]      \ fetch value at cell +
                mov     ebx, 0 [eax] [edi]      \ fetch value
                push    ecx
                next    c;

CFA-CODE (IVD!)
                mov     eax, -4 [eax] [edi]     \ get ivar offset
                add     eax, OP [UP]              \ add to base of current object
                mov     0 [eax] [edi], ebx      \ store value
                pop     ebx
                mov     4 [eax] [edi], ebx      \ store value
                pop     ebx
                next    c;

CFA-CODE (IVD+!)
                mov     eax, -8 [eax] [edi]     \ get ivar offset
                add     eax, OP [UP]              \ add to base of current object
                pop     ecx
                add     4 [eax] [edi], ecx      \ add low part first
                adc     0 [eax] [edi], ebx      \ add high part with carry
                pop     ebx
                next    c;

\ -------------------- Object pointer --------------------

: OP@  OP @ ;   \ to allow UP to be kept in the EDX register
: OP!  OP ! ;

\ -------------------- Find Name for Hashed Value --------------------

17 #vocabulary hashed

    vocabulary classes
' classes here u< [if]    app-here ' classes >body -          \ voc-pfa-size
                          5 cells app-reserve                 \ extra for a class
                          constant voc-pfa-size
                  [else]  sys-here ' classes >body -          \ voc-pfa-size
                          5 cells sys-reserve                 \ extra for a class
                          constant voc-pfa-size
                  [then]


' hashed vcfa>voc constant hash-wid

: unhash  ( hash-val -- addr len )
        hash-wid voc#threads ( #threads ) 0
        do      hash-wid i cells+
                begin   @ ?dup
                while   ( hash-val link-field )
                        2dup link> >body @ =
                        if      nip ( discard hash value )
                                l>name nfa-count ( addr len )
                                unloop exit
                        then
                repeat
        loop
        drop S" Unknown" ;

: ?unhash  ( hash-val -- f1 )
        hash-wid voc#threads ( #threads ) 0
        do      hash-wid i cells+
                begin   @ ?dup
                while   ( hash-val link-field )
                        2dup link> >body @ =
                        if      2drop true
                                unloop exit
                        then
                repeat
        loop    drop false ;

0 value obj-save

: .M0NAME       ( a1 -- )
                1 cells - @ unhash type space ;

: .M1NAME       ( a1 a2 -- a3 )
                2 cells - @ unhash type
                cell+   ( a1 becomes a3 )       \ skip next cell also
                dup @ ?dup
        if      obj-save
                if      obj-save cell - @           \ should use obj>CLASS, not yet there
                        voc-pfa-size cell+ +        \ should use IFA,       not yet there
                        begin   @ 2dup 3 cells+ @ u< 0=
                                start/stop
                        until   nip dup
                        if      cell+ @ unhash space type
                        else    drop ." ???"
                        then    space
                else    ." NULL "
                then
        else    ."  self "
        then    ;

: add-hash  ( addr len hash-val -- )
        >r 2dup hash-wid search-wordlist
        if
                r> 2drop 2drop  ( already found )
        else
                current @ >r
                hash-wid current !
                "header
                r> current !
                docon ,
                r> ,
        then ;


