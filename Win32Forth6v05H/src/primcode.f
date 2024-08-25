\ primcode.f beta 501B 18/04/2003 arm Primitive code utilities

cr .( Loading Primtive Code Utilities...)
cr .( -- BETA PRIMCODE.F V501B --)

\ Scan for char BACKWARDS starting at addr, back through len bytes
\ before addr, returning addr' and len' of char.

CODE -SCAN      ( addr len char -- addr' len' )
                mov     eax, ebx
                pop     ecx
                jecxz   short @@1
                mov     ebx, edi
                pop     edi
                add     edi, ebx                \ edi = absolute address
                std
                repnz   scasb
                cld
                jne     short @@2
                inc     ecx
                inc     edi
@@2:            sub     edi, ebx
                push    edi
                mov     edi, ebx
@@1:            mov     ebx, ecx
                next    c;

\ Skip occurances of char BACKWARDS starting at addr, back through
\ addr-len, returning addr' and len' of char.

CODE -SKIP      ( addr len char -- addr' len' )
                mov     eax, ebx
                pop     ecx
                jecxz   short @@1
                mov     ebx, edi
                pop     edi
                add     edi, ebx                \ edi = absolute address
                std
                repz    scasb
                cld
                je      short @@2
                inc     ecx
                inc     edi
@@2:            sub     edi, ebx
                push    edi
                mov     edi, ebx
@@1:            mov     ebx, ecx
                next    c;

CODE WSKIP      ( adr len word -- adr' len' )
                pop     ecx
                jecxz   short @@2
                mov     eax, ebx                \ eax = character
                mov     ebx, edi                \ ebx = forth base address
                pop     edi
                add     edi, ebx                \ edi = absolute address
                repz    scasw
                je      short @@1
                inc     ecx
                dec     edi
                dec     edi
@@1:            sub     edi, ebx                \ relative
                push    edi
                mov     edi, ebx                \ restore base ptr
@@2:            mov     ebx, ecx
                next    c;

CODE WSCAN      ( adr len word -- adr' len' )
                pop     ecx
                jecxz   short @@2
                mov     eax, ebx                \ eax = character
                mov     ebx, edi                \ ebx = forth base address
                pop     edi
                add     edi, ebx                \ edi = absolute address
                repnz   scasw
                jne     short @@1
                inc     ecx
                dec     edi
                dec     edi
@@1:            sub     edi, ebx                \ relative
                push    edi
                mov     edi, ebx                \ restore base ptr
@@2:            mov     ebx, ecx
                next    c;

CODE LSKIP      ( adr len long -- adr' len' )
                pop     ecx
                jecxz   short @@2
                mov     eax, ebx                \ eax = character
                mov     ebx, edi                \ ebx = forth base address
                pop     edi
                add     edi, ebx                \ edi = absolute address
                repz    scasd
                je      short @@1
                inc     ecx
                sub     edi, # 4
@@1:            sub     edi, ebx                \ relative
                push    edi
                mov     edi, ebx                \ restore base ptr
@@2:            mov     ebx, ecx
                next    c;

CODE LSCAN      ( adr len long -- adr' len' )
                pop     ecx
                jecxz   short @@2
                mov     eax, ebx                \ eax = character
                mov     ebx, edi                \ ebx = forth base address
                pop     edi
                add     edi, ebx                \ edi = absolute address
                repnz   scasd
                jne     short @@1
                inc     ecx
                sub     edi, # 4
@@1:            sub     edi, ebx                \ relative
                push    edi
                mov     edi, ebx                \ restore base ptr
@@2:            mov     ebx, ecx
                next    c;

CODE WCOUNT     ( str -- addr len )  \ word counted strings
                movzx   eax, word ptr 0 [ebx] [edi]
                inc     ebx
                inc     ebx
                push    ebx
                mov     ebx, eax
                next    c;

CODE CELL-SORT  ( a1 n1 -- )      \ perform in place sort buffer a1 of n1 cells
                push    ebx
                cmp     ebx, # 2        \ don't sort if less than 2 elements
                jnl     short @@2
                jmp     short @@3
@@1:            mov     eax,   0 [ebx] [edi]
                xchg    eax, 4 [ebx] [edi]
                xchg    eax,   0 [ebx] [edi]
                cmp     eax,   0 [ebx] [edi]
                jl      short @@1
                add     ebx, # 4
                loop    @@1
@@2:            pop     ecx
                pop     ebx
                push    ebx
                dec     ecx
                push    ecx
                jg      short @@1
@@3:            add     esp, # 8
                pop     ebx
                next    c;

CODE BYTE-SORT  ( a1 n1 -- )      \ perform in place sort buffer a1 of n1 bytes
                push    ebx
                cmp     ebx, # 2        \ don't sort if less than 2 elements
                jnl     short @@2
                jmp     short @@3
@@1:            mov     al,   0 [ebx] [edi]
                xchg    al, 1 [ebx] [edi]
                xchg    al,   0 [ebx] [edi]
                cmp     al,   0 [ebx] [edi]
                jl      short @@1
                inc     ebx
                loop    @@1
@@2:            pop     ecx
                pop     ebx
                push    ebx
                dec     ecx
                push    ecx
                jg      short @@1
@@3:            add     esp, # 8
                pop     ebx
                next    c;

\ add two numbers and return true if they would produce arithemtic
\ overflow (used by the debugger).

CODE +OV?       ( n1 n2 -- f )
                sub     ecx, ecx
                pop     eax
                add     ebx, eax
                jno     short @@1
                dec     ecx
@@1:            mov     ebx, ecx
                next    c;


