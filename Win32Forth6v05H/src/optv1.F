\ optv1.f 01/12/2002 21:58:49 experimental forth optimiser

((

Optimisations available:

LITERAL consolidation
---------------------

Words of the form 
 : x ... 1 2 3 ... ;
would normally compile
 | ... | LIT | 1 | LIT | 2 | LIT | 3 | ... |
These are modified to form
 | ... | 3LIT | 1 | 2 | 3 | ... |
where nLIT pushes n values in the stream onto the data stack.

CONSTANT Reduction
------------------

Words of the form 
 10 CONSTANT Y
 : X Y ;
would normally compile
 | ... | ' Y | ... |
These are modified to
 | ... | LIT | 10 | ... |
and may be further consolidated by the literal consoildation process

VARIABLE @ Reduction
--------------------

Words of the form
n VARIABLE Y
: X Y @ ;
would normally compile to
 | ... | Y | @ | ... |
These are modified to
 | ... | @VAR | ' Y | ... |
where @VAR directly fetches the value.

))


0 value prev-interpret
0 value OPT-STATE                 \ are we currently optimising?
0 value optimising?               \ is optimizing enabled?

new-chain opt-chain-funcs         \ a chain of optimising functions
new-chain opt-chain-term          \ a chain of termination fucntions


\ ------------------------ Optimised code ---------------------------------------

CODE 2LIT       ( -- n n )                        \ push the 2 literals following
                push    ebx
                push    0 [esi]
                mov     ebx, 4 [esi]
                mov     eax, 8 [esi]
                add     esi, # 12
                exec    c;

CODE 3LIT       ( -- n n n )                      \ push the 3 literals following
                push    ebx
                push    0 [esi]
                push    4 [esi]
                mov     ebx, 8 [esi]
                mov     eax, 12 [esi]
                add     esi, # 16
                exec    c;

CODE 4LIT       ( -- n n n n )                    \ push the 4 literals following
                push    ebx
                push    0 [esi]
                push    4 [esi]
                push    8 [esi]
                mov     ebx, 12 [esi]
                mov     eax, 16 [esi]
                add     esi, # 20
                exec    c;

CODE NLIT       ( -- nx .. ny )                   \ push the n>0 literals following
                mov     ecx, 0 [esi]              \ get count
                lea     eax, 4 [esi]              \ point at first entry
                lea     esi, 0 [ecx*4] [esi]      \ point at entry n 
                push    ebx                       \ save ebx
@@1:            push    0 [eax]                   \ next constant
                add     eax, # 4                  \ next entry
                cmp     eax, esi                  \ is eax < esi
                jb      short @@1                 \ yes, next word
                mov     ebx, 0 [eax]              \ last entry
                mov     eax, 4 [esi]
                add     esi, # 8
                exec    c;

CODE NMLIT      ( -- nx .. ny )                   \ push the n>0 literals following
                push    ebx                       \ save ebx
                mov     ecx, 0 [esi]              \ get count
                lea     ebx, [ecx*4]              \ save it in ebx as cells
                sub     esp, ebx                  \ adjust stack 
                mov     ebx, 0 [esi]              \ last value
                add     esi, # 4                  \ first entry
                mov     -4 [ebp], edi             \ save edi
                mov     edi, esp                  \ stack to dest
                dec     ecx
                rep     movsd                     \ move the words
                mov     edi, -4 [ebp]             \ restore edi
                mov     eax, 4 [esi]              \
                add     esi, # 8                  \ next word
                exec    c;

CODE @VAR       ( -- n )                          \ directly fetch value from a variable
                push    ebx                       \ save ebx
                mov     ecx, 0 [esi]              \ get pointer to body
                mov     eax, 4 [esi]              \ next word
                add     esi, # 8
                mov     ebx, 0 [ecx] [edi]        \ fetch value
                exec    c;

CODE !VAR       ( n -- )                          \ directly set value from a variable
                mov     ecx, 0 [esi]              \ get pointer to body
                mov     eax, 4 [esi]              \ next word
                add     esi, # 8
                mov     0 [ecx] [edi], ebx        \ set value
                pop     ebx
                exec    c;


variable opt-lastxt                               \ last xt compiled
variable opt-lasthere                             \ last here where it was compiled

: opt-lastreset ( -- )                           \ reset
                ['] noop opt-lastxt !
                0 opt-lasthere !
                ;
                
opt-chain-term chain-add opt-lastreset           \ add to term chain

0 value opt-litcount
create opt-litstore 20 cells allot                \ literal store

: opt-genlit,   ( -- )                            \ generate lit words
                opt-litcount ?dup
                if
                  case 1 of POSTPONE  LIT   endof
                       2 of POSTPONE 2LIT   endof
                       3 of POSTPONE 3LIT   endof
                       4 of POSTPONE 4LIT   endof
                    POSTPONE NMLIT opt-litcount ,
                  endcase
                  opt-litcount 0 ?DO opt-litstore i cells+ @ , LOOP
                  0 to opt-litcount
                then
                ;
                
opt-chain-term chain-add opt-genlit,              \ add to cleanup

: opt-lit,      ( n -- )                          \ compile literal
                opt-litcount 20 >=                \ check store isn't full
                if                                \ full, so empty
                  opt-genlit,                     \ generate list
                then
                opt-litstore opt-litcount cells+ ! \ save the literal
                1 +to opt-litcount
                ;


: opt-number,   ( d -- )                         \ compile number
                float? if
                  opt-genlit,                    \ stop optimising numbers
                  number,                        \ generate number
                else
                  DOUBLE? 0= IF DROP THEN
                  STATE @
                  IF DOUBLE? IF SWAP OPT-LIT, THEN
                    OPT-LIT,
                  THEN
                then
                ;

: opt-gencompile, ( xt -- )                      \ compile
                opt-genlit,                      \ generate constants
                dup opt-lastxt !                 \ save last xt
                here opt-lasthere !              \ save last here
                compile,                         \ then compile
                ;


: opt-compile,  ( xt -- )                        \ compile xt
                dup
                case
                
                  dup @ docon of                 \ n CONSTANT y to LIT N
                    opt-lastreset                \ reset last
                    >body @ opt-lit, drop
                    exit                         \ change constant to literal
                  endof drop

                  ['] @ of                     \ is it a @
                    opt-lastxt @ @ dovar = if    \ was last xt a var?
                      ['] @var opt-lasthere @ !  \ yes, use direct fetch
                      drop                       \ drop the xt
                      opt-lastxt @ >body         \ and replace with body of the var
                    then
                  endof

                  ['] ! of                     \ is it a !
                    opt-lastxt @ @ dovar = if      \ was last xt a var?
                      ['] !var opt-lasthere @ !    \ yes, use direct set
                      drop                       \ drop the xt
                      opt-lastxt @ >body         \ and replace with body of the var
                    then
                  endof

                endcase

                opt-gencompile,                  \ generate
                ;

: opt-scan      ( -- str len )
                bl word count ;

: opt-interpret ( -- )                             \ optimising interpreter
            BEGIN   opt-scan                     \ read a word
            WHILE   SAVE-SRC FIND ?DUP
              IF 
                STATE @ =
                IF opt-COMPILE,                  \ COMPILE TIME
                ELSE
                  optimising? if
                    opt-chain-term do-chain      \ clean up if immediate
                  then
                  EXECUTE ?STACK              \ INTERPRET
                THEN
              ELSE
                NUMBER opt-NUMBER,
              THEN
              ?UNSAVE-SRC
            REPEAT DROP
            ;
                
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\               Optimizer Control definitions
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

: OPT-ON        ( -- )          \ turn on optimization
                optimising? ?EXIT
                in-system?  ?EXIT
                TRUE  to optimising?
                FALSE to opt-state
                defer@ interpret to prev-interpret
                ['] opt-interpret is interpret ;

: OPT-OFF       ( -- )          \ turn off optimization
                opt-chain-term do-chain     \ finish anything we're optimising
                optimising? 0= ?EXIT
                false to optimising?
                FALSE to opt-state
                prev-interpret is interpret
                0 to prev-interpret ;


: OPT[          ( -- )          \ enable optimization for part of a definition
                opt-on interpret ; IMMEDIATE

: ]OPT          ( -- )          \ disable optimization for remainder of definition
                opt-off interpret ; IMMEDIATE

OPT-OFF         \ default to no optimization for now

