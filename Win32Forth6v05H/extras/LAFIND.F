\ LAFIND.F              Look Aside Find                 by Tom Zimmer

cr .( Loading Look Aside Find...)

la-size cells Pointer &aside-nfa        \ the look aside array

 0 value &last-header

\ CODE "LAHASH    ( a1 n1 -- n2 )
\         mov     eax, ebx                \ copy count into EAX
\         mov     ecx, ebx                \ copy count into ECX
\         pop     ebx                     \ get string address into EBX
\         push    edx                     \ save EDX
\         xor     edx, edx                \ clear EDX
\ L$1:    rol     eax, # 7                \ rotate result some
\         mov     dl, 0 [ebx] [edi]       \ get first character of name
\         add     eax, edx                \ accumulate another character
\         inc     ebx                     \ bump to address of next character
\         loop    L$1
\         add     eax, la-offset [edi]
\         xor     edx, edx                \ clear high part of dividend
\         mov     ebx, # aside-size       \ get the divisor
\         div     ebx                     \ perform modulus
\         mov     ebx, edx                \ move result into EBX
\         pop     edx                     \ recover EDX
\         next,
\         END-CODE

: add-aside     ( hash string -- )
                (find)
                if      >name
                        &aside-nfa rot cells+ !  \ if found, put in table
                else    2drop                   \ else discard
                then    ;

: lafind        ( string -- string 0 | cfa flag true )
                dup c@ 0= abort" Null String"
                dup>r count 2dup "lahash
                &aside-nfa over cells+ @ ?dup
                if      nip                     \ if defined, dscard hash
                else    dup r@ add-aside
                        &aside-nfa swap cells+ @ ?dup 0=
                        if      2drop
                                r> 0 exit               \ if not found, EXIT
                        then
                then
                dup>r nfa-count compare 0=
                if      r> r>drop dup name> swap c@ 0x80 and
                        if      1
                        else   -1
                        then
                else    r>drop
                        r@ (find) dup
                        if      over >name
                                r@ count "lahash
                                &aside-nfa swap cells+ !    \ put in table
                        then    r>drop
                then    ;

: la"header     ( a1 n1 -- )    \ add names created to look aside table
                                \ *** EXCEPT METHODS !!!! ***
                dup name-max-chars >
                if      cr ." Truncating name longer than 63 characters!"
                        cr ." ->"
                        2dup type
                then    name-max-chars min 2dup _"header
                here >name to &last-header      \ save away NFA for use by LA;
                "lahash &aside-nfa +cells off ; \ make sure not findable yet

: la;           ( -- )
                &aside-nfa 0= ?exit
                &last-header ?dup
                if      dup nfa-count + 1- c@ ':' <> \ ** ONLY if NOT a METHOD **
                        if      dup nfa-count "lahash &aside-nfa +cells !
                        else    drop
                        then    0 to &last-header
                then    ;

semicolon-chain chain-add la;   \ link into chain of stuff to be done at the
                                \ end of a colon definition

classes also

: lamsgFind     ( addr -- addr false | cfa true )
                pfind   ?dup if  exit  then
                lafind  ?dup if  exit  then
                _msgFind ;

: 0look-aside   ( -- )
                1 +to la-offset ;

: la-on         ( -- )
                0look-aside
                &aside-nfa la-size cells erase
                ['] lamsgfind   is find
                ['] la"header   is "header
                ['] 0look-aside is voc-also ;

initialization-chain chain-add la-on

: la-off        ( -- )
                ['] msgfind  is find
                ['] _"header is "header
                ['] noop     is voc-also ; 

only forth also definitions


