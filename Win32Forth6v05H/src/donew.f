CODE (DO)       ( n1 n2 -- )    \ "runtime" setup loop using n1,n2
                pop     ecx
                mov     eax, 0 [esi]
                add     eax, esi
                add     ecx, # 0x80000000
                sub     ebx, ecx
                mov     -1 cells [ebp], eax
                mov     -2 cells [ebp], ecx
                mov     -3 cells [ebp], ebx
                pop     ebx
                mov     eax, 4 [esi]
                add     esi, # 8
                sub     ebp, # 12
                exec    
                next    c;     \ next to stop decompiler

CODE (LOOP)     ( -- )          \ "runtime" bump count and branch to after
                                \ DO if loop count not complete
                inc     dword ptr 0 [ebp]
                jno     short @@1
                mov     eax, 4 [esi]
                add     esi, # 8
                add     ebp, # 12
                exec

@@1:            add     esi, 0 [esi]
                mov     eax, -4 [esi]
                exec 
                next    c;

CODE (?DO)      ( n1 n2 -- )    \ "runtime" setup loop using n1,n2, if n1=n2
                                \ then discard n1,n2 and branch to after DO
                pop     ecx
                cmp     ecx, ebx
                je      short @@1
                mov     eax, 0 [esi]
                add     eax, esi
                add     esi, # 4
                add     ecx, # 0x80000000
                sub     ebx, ecx
                mov     -1 cells [ebp], eax
                mov     -2 cells [ebp], ecx
                mov     -3 cells [ebp], ebx
                sub     ebp, # 12
                pop     ebx
                next

@@1:            add     esi, 0 [esi]
                pop     ebx
                next    c;

CODE (+LOOP)    ( n1 -- )       \ "runtime" bump count by n1 and branch to
                                \ after DO if loop count not complete
                add     0 [ebp], ebx
                pop     ebx
                jno     short @@1
                mov     eax, 4 [esi]
                add     esi, # 8
                add     ebp, # 12
                exec

@@1:            add     esi, 0 [esi]
                mov     eax, -4 [esi]
                exec 
                next    c;


: DO     ?COMP  COMPILE (DO) >MARK 8 ; IMMEDIATE
: ?DO    ?COMP  COMPILE (?DO)  >MARK 8 ; IMMEDIATE
: LOOP   ?COMP  8 ?PAIRS  COMPILE (LOOP)   DUP 2 CELLS+ <RESOLVE  >RESOLVE ; IMMEDIATE
: +LOOP  ?COMP  8 ?PAIRS  COMPILE (+LOOP)  DUP 2 CELLS+ <RESOLVE  >RESOLVE ; IMMEDIATE

: x 5 3 do i . 8 6 do j . loop loop ;
: Y 5 3 ?do i . loop ;
: Z 5 5 ?do i . loop ;
: A 100 0 do I . 10 +loop ;

\s


