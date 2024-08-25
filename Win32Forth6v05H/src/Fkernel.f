\ FKERNEL.FTH  - WIN32FORTH KERNEL
\ Andrew McKewan
\ March 1994
\ Given to Tom Zimmer 05/13/94, assemblable with TASM32
\ Metacompiler version 11/95 Andrew McKewan
\ Separated heads version started December 19th, 1995 tjz
\ Added User variables for multi-tasking August 29th, 1996 bee/tjz

\ fkernel.f beta 1.9G 2002/08/29 arm major modifications, CALL/PROC in kernel
\ fkernel.f beta 2.0A 2002/08/31 arm windows ANS file words
\ fkernel.f beta 2.0C 2002/09/12 arm windows memory management
\ fkernel.f beta 2.0E 2002/09/17 arm reorder source, minor cosmetic changes
\ fkernel.f beta 2.9G 2002/09/24 arm RELEASE FOR TESTING
\ fkernel.f beta 3.0A 2002/09/25 arm Performance enhancements
\ fkernel.f beta 3.3D 2002/10/08 arm Consolidated
\ fkernel.f beta 3.9K 2002/11/12 arm Finalised build
\ fkernel.f beta 3.9K 2002/11/15 ENHANCED: Modified read-line to support CRLF, CR and LF as delimiters
\ fkernel.f beta 4.1B 2002/11/27 arm Performance enhancements, CALLBACK modified
\ fkernel.f beta 4.1B 2002/12/12 arm Pasted sections from wrong kernel! Now corrected...
\ fkernel.f beta 4.9C 2002/02/10 arm Finalised build
\ fkernel.f beta 4.9D 2002/02/18 arm Bill McCarthy posted correction to READ-LINE

DECIMAL

\        sp     equ     <esp>   \ Stack Pointer for Forth, the hardware stack
\        rp     equ     <ebp>   \ Return Pointer, Forth's subroutine stack
\        ip     equ     <esi>   \ "absolute" Instruction Pointer for Forth
\        bp     equ     <edi>   \ "absolute" base of Forth image
\        up     equ     <edx>   \ "absolute" user pointer
\        tos    equ     <ebx>   \ Top of stack is in EBX

26350037 EQU MAGIC#         \ Magic # to indentify a dictionary image

 -1 EQU THROW_ABORT
 -2 EQU THROW_ABORTQ
 -3 EQU THROW_STACK_OVERFLOW
 -4 EQU THROW_STACK_UNDERFLOW
-13 EQU THROW_UNDEFINED

\ -------------------- Boot-Up Literals --------------------

HERE: ORIG        ORIGIN ,        \ My physical starting address
HERE: MAGIC       MAGIC# ,        \ magic number (make sure I am ME)
HERE: ASIZE       APPSIZE ,       \ desired dictionary size in bytes
HERE: AACTUAL     0 ,             \ actual dictionary length in bytes
HERE: SSIZE       SYSSIZE ,       \ desired head space size in bytes
HERE: SACTUAL     0 ,             \ actual head length in bytes
HERE: SSEPARATE   SSEPARATION ,   \ code to head separation (multiple of 64k)
HERE: ENTRY       0 ,             \ cold start entry point
HERE: TABLE       0 ,             \ pointer to xcall table entries
( HERE: HINST )   0 ,             \ applications instance handle
HERE: CMDLEN      0 ,             \ length  of command line
HERE: CMDADR      0 ,             \ address of command line
HERE: HCON        0 ,             \ applications console handle
( HERE: EXCEPT )  0 ,             \ exception variable, defaults to none, no longer used
( HERE: EXREC  )  0 ,             \ a pointer to the exception record, no longer used
( HERE: EXRSTK )  0 , 0 , 0 , 0 , \ room for top return stack entries, no longer used
( HERE: UNFORTH ) 0 ,             \ un-initialize forth, no longer used
HERE: K32LIBX     0 ,             \ handle of kernel32.dll
HERE: K32GPAX     0 ,             \ ep of GetProcAddress
HERE: MSGNTRY     0 ,             \ message callbacks for key and key?
HERE: WINMSGNTRY  0 ,             \ windows message callbacks


\ -------------------- Inner Interpreter --------------------

((      SEE META.F AND FKERNEXT.F
MACRO EXEC      ( -- )                  \ execute RELATIVE cfa in eax
MACRO NEXT      ( -- )                  \ Inner interpreter
))

\ -------------------- Runtime Routines --------------------

CFA-CODE DOCOL  ( -- )                  \ runtime for colon definitions
                mov     -4 [ebp], esi
                sub     ebp, # 4
                lea     esi, 8 [eax] [edi]
                mov     eax, -4 [esi]
                exec    c;

CFA-CODE DODOES ( -- )                  \ runtime for DOES>
                mov     -4 [ebp], esi
                sub     ebp, # 4
                pop     esi
                push    ebx
                lea     ebx, 4 [eax]
                next    c;

CFA-CODE DOVAR  ( -- a1 )               \ runtime for CREATE and VARIABLE
                push    ebx
                lea     ebx, 4 [eax]
                next    c;

CFA-CODE DOUSER ( -- a1 )               \ runtime for USER variables
                push    ebx
                mov     ebx, 4 [eax] [edi]      \ get offset
                add     ebx, edx                \ add absolute user base
                sub     ebx, edi                \ remove absolute forth base
                next    c;

CFA-CODE DOCON  ( -- n1 )               \ runtime for constants
                push    ebx
                mov     ebx, 4 [eax] [edi]
                next    c;

CFA-CODE DODEFER ( -- )                 \ runtime for DEFER
                mov     eax, 4 [eax] [edi]
                exec    c;

CFA-CODE DOVALUE ( -- n1 )              \ runtime for VALUE fetch
                push    ebx
                mov     ebx, 4 [eax] [edi]
                next    c;

CFA-CODE DOVALUE! ( n1 -- )               \ runtime for VALUE store
                mov     -4 [eax] [edi], ebx
                pop     ebx
                next    c;

CFA-CODE DOVALUE+! ( n1 -- )               \ runtime for VALUE increment
                add     -8 [eax] [edi], ebx
                pop     ebx
                next    c;

CFA-CODE DO2VALUE ( d1 -- )               \ runtime for 2VALUE fetch
                push    ebx
                mov     ebx, 4 [eax] [edi]
                mov     eax, 4 [ebx] [edi]
                mov     ebx, 0 [ebx] [edi]
                push    eax
                next    c;


\ Define pointer constants for the basic Forth "runtime" routines.  A pointer
\ to one of these small assembly language routines is installed at the start
\ of every Forth definitions created.  They can actually be thought of as the
\ only form of Forth data "type"ing.  That is it is possible to look at the
\ contents of the CFA of any Forth definition, evaluate what is there, and
\ determine what type of definition it is.

ASSEMBLER DOCOL         META CONSTANT DOCOL
ASSEMBLER DOCON         META CONSTANT DOCON
ASSEMBLER DODOES        META CONSTANT DODOES
ASSEMBLER DOVAR         META CONSTANT DOVAR
ASSEMBLER DODEFER       META CONSTANT DODEFER
ASSEMBLER DO2VALUE      META CONSTANT DO2VALUE
ASSEMBLER DOVALUE       META CONSTANT DOVALUE
ASSEMBLER DOVALUE!      META CONSTANT DOVALUE!
ASSEMBLER DOVALUE+!     META CONSTANT DOVALUE+!
ASSEMBLER DOUSER        META CONSTANT DOUSER

\ -------------------- Vector Variables --------------------

\ Define the Forth base image pointer constants. These constants point to
\ fields at the beginning of Forth that hold various information that must
\ be sent to or received from the wrapper.
          ORIGIN             CONSTANT ^IMAGE          \ The pointer to the start of the Image
          ORIG               CONSTANT &ORIGIN
          MAGIC              CONSTANT &MAGIC
          ASIZE              CONSTANT &APP-SIZE
          AACTUAL            CONSTANT &APP-ACTUAL
          SSIZE              CONSTANT &SYS-SIZE
          SACTUAL            CONSTANT &SYS-ACTUAL
          SSEPARATE          CONSTANT &SYS-SEPARATION
                             VARIABLE &HINST
          CMDLEN             CONSTANT &CMDLEN
          CMDADR             CONSTANT &CMDADR
          HCON               CONSTANT &HCON
          MSGNTRY            CONSTANT &MESSAGE-CALLBACK
          WINMSGNTRY         CONSTANT &WINMESSAGE-CALLBACK
          K32LIBX          | CONSTANT &K32LIB
          K32GPAX          | CONSTANT &K32GPA
          KVER               CONSTANT &KVER
                             VARIABLE &EXCEPT
                             VARIABLE &EXREC

4 4096 *   CONSTANT      RSTACKSIZE    \ rstack size
1 4096 *   CONSTANT      USERSIZE      \ user area size for task variables
RSTACKSIZE USERSIZE + 4096 / CONSTANT PROBESTACK \ amount to probe the stack (pages)

\ -------------------- Primitives --------------------

NCODE _EXIT     ( -- )          \ exit the current Forth definition
                mov     esi, 0 [ebp]
                add     ebp, # 4
                next    c;

NCODE UNNEST    ( -- )          \ exit the current Forth definition
                mov     esi, 0 [ebp]
                add     ebp, # 4
                next    c;

CODE LIT        ( -- n )        \ push the literal value following LIT in the
                                \ dictionary onto the data stack
                push    ebx
                mov     eax, 4 [esi]
                mov     ebx, 0 [esi]
                add     esi, # 8
                exec    c;

NCODE EXECUTE   ( xt -- )       \ go perform the Forth function whose address
                                \ is "xt"
                mov     eax, ebx
                pop     ebx
                exec    c;

NCODE PERFORM  ( a1 -- )        \ go perform the Forth function whose address
                                \ is contained in address "a1"
                mov     eax, 0 [ebx] [edi]
                pop     ebx
                exec    c;

CODE NOOP       ( -- )          \ the Forth do-nothing function
                next    c;
\ --------------------------- Constants --------------------------------

 4 CONSTANT CELL
-4 CONSTANT -CELL
32 CONSTANT BL
-1 CONSTANT TRUE
 0 CONSTANT FALSE

260      EQU MAXBUFFER      \ Size of any string buffer, MUST match the
                            \ size of a windows maximum path string,
                            \ which is 260 bytes. ** DON'T CHANGE THIS **
255      EQU MAXCSTRING     \ max counted string
16       EQU NUMVOCS        \ Maximum number of vocabularies in search order

MAXBUFFER CONSTANT MAXSTRING    \ maximum length of a counted string
MAXBUFFER CONSTANT MAX-PATH     \ maximum length of a filename buffer
MAXBUFFER CONSTANT MAX-HANDLE   \ maximum length of filename
MAXCSTRING CONSTANT MAXCOUNTED  \ maximum length of contents of a counted string

VOCABULARY FORTH                \ main vocabulary, only one in kernel

\ -------------------- System WIde Constants ----------------

CREATE CRLF$ 2 C, 13 C, 10 C,   \ counted CRLF string

\ NEXT-SEQ returns the address of a counted string that is the code
\ compiled by NEXT. Used by the disassembler.

CREATE NEXT-SEQ
        0 OFA-H !               \ disable OFA resolution
      HERE 0 C,
        INIT-ASSEMBLER
        NEXT
        END-CODE
      HERE OVER - 1- SWAP C!-T

\ EXEC-SEQ returns the address of a counted string that is the code
\ compiled by EXEC. Used by the disassembler.

CREATE EXEC-SEQ
        0 OFA-H !               \ disable OFA resolution
      HERE 0 C,
        INIT-ASSEMBLER
        EXEC
        END-CODE
      HERE OVER - 1- SWAP C!-T

\ -------------------- Branching & Looping --------------------

NCODE _BEGIN    ( -- )          \ "runtime" marker for the decompiler, a noop
                next    c;

NCODE _THEN     ( -- )          \ "runtime" marker for the decompiler, a noop
                next    c;

NCODE _CASE     ( -- )          \ "runtime" marker for the decompiler, a noop
                next    c;

NCODE _OF       ( n1 n2 -- [n1] ) \ "runtime"
                                  \ if n1<>n2 branch to after ENDOF, return n1
                                  \ else continue and don't return n1
                pop     eax
                cmp     ebx, eax
                je      short @@1
                mov     ebx, eax
                add     esi, 0 [esi]
                next
@@1:            pop     ebx
                mov     eax, 4 [esi]
                add     esi, # 8
                exec    c;

NCODE _ENDCASE  ( n1 -- )       \ "runtime" discard n1 and continue
                pop     ebx
                next    c;

NCODE _ENDOF    ( -- )          \ "runtime" branch to after ENDCASE
                add     esi, 0 [esi]
                next    c;

NCODE BRANCH    ( -- )                    \ "runtime" for branch always
                add     esi, 0 [esi]      \  come here for branch
                next    c;

NCODE ?BRANCH   ( f1 -- )                 \ "runtime" for branch on f1=FALSE
                test    ebx, ebx
                pop     ebx
                je      short @@1        \ yes, do branch
                mov     eax, 4 [esi]      \ optimised next
                add     esi, # 8
                exec
@@1:            add     esi, 0 [esi]
                next    c;

NCODE _AGAIN    ( -- )          \ "runtime" branch back to after BEGIN
                add     esi, 0 [esi]
                next    c;

NCODE _REPEAT   ( -- )          \ "runtime" branch back to after BEGIN
                add     esi, 0 [esi]
                next    c;

NCODE _UNTIL    ( f1 -- )       \ "runtime" if f1=FALSE branch to after BEGIN
                test    ebx, ebx
                pop     ebx
                je      short @@1
                mov     eax, 4 [esi]
                add     esi, # 8
                exec
@@1:            add     esi, 0 [esi]
                next    c;

NCODE _WHILE    ( f1 -- )       \ "runtime" if f1=FALSE branch to after REPEAT
                test    ebx, ebx
                pop     ebx
                je      short @@1
                mov     eax, 4 [esi]
                add     esi, # 8
                exec
@@1:            add     esi, 0 [esi]
                next    c;

NCODE (LOOP)    ( -- )          \ "runtime" bump count and branch to after
                                \ DO if loop count not complete
                inc     dword ptr 0 [ebp]
                jno     short @@1
                mov     eax, 4 [esi]
                add     esi, # 8
                add     ebp, # 12
                exec
@@1:            add     esi, 0 [esi]
                next    c;

NCODE (+LOOP)   ( n1 -- )       \ "runtime" bump count by n1 and branch to
                                \ after DO if loop count not complete
                add     0 [ebp], ebx
                pop     ebx
                jno     short @@1
                mov     eax, 4 [esi]
                add     esi, # 8
                add     ebp, # 12
                exec
@@1:            add     esi, 0 [esi]
                next    c;

NCODE UNLOOP    ( -- )          \ discard LOOP parameters from return stack
                add     ebp, # 12
                next    c;

NCODE (DO)      ( n1 n2 -- )    \ "runtime" setup loop using n1,n2
                pop     ecx
LABEL pdo1      mov     eax, 0 [esi]
                add     eax, esi
                add     ecx, # 0x80000000
                sub     ebx, ecx
                mov     -4 [ebp], eax
                mov     -8 [ebp], ecx
                mov     -12 [ebp], ebx
                sub     ebp, # 12
                pop     ebx
                mov     eax, 4 [esi]
                add     esi, # 8
                exec    c;

NCODE (?DO)     ( n1 n2 -- )    \ "runtime" setup loop using n1,n2, if n1=n2
                                \ then discard n1,n2 and branch to after DO
                pop     ecx
                cmp     ecx, ebx
                jne     pdo1
                pop     ebx
                add     esi, 0 [esi]
                next    c;

CODE BOUNDS     ( adr len -- lim first ) \ calculate loop bounds from adr,len
                mov     eax, 0 [esp]
                add     ebx, eax
                xchg    ebx, eax
                mov     0 [esp], eax
                next    c;

CODE I          ( -- n1 )       \ push n1, the value of the current loop index
                push    ebx
                mov     ebx, 0 CELLS [ebp]
                add     ebx, 1 CELLS [ebp]
                next    c;

CODE J          ( -- n1 )       \ push n1, the value of the outer loop index
                push    ebx
                mov     ebx, 3 CELLS [ebp]
                add     ebx, 4 CELLS [ebp]
                next    c;

CODE K          ( -- n1 )       \ push n1, value of the second outer loop index
                push    ebx
                mov     ebx, 6 CELLS [ebp]
                add     ebx, 7 CELLS [ebp]
                next    c;

NCODE LEAVE     ( -- )          \ exit the current DO LOOP immediately
                mov     esi, 2 CELLS [ebp]
                add     ebp, # 3 CELLS
                next    c;

NCODE ?LEAVE    ( f1 -- )       \ exit the current DO LOOP if f1=TRUE
                test    ebx, ebx
                pop     ebx
                je      short @@1
                mov     esi, 2 CELLS [ebp]
                add     ebp, # 3 CELLS
@@1:            next    c;


\ -------------------- Stack Operators --------------------

CODE DROP       ( n -- )        \ discard top entry on data stack
                pop     ebx
                next    c;

CODE DUP        ( n -- n n )    \ duplicate top entry on data stack
                push    ebx
                next    c;

CODE SWAP       ( n1 n2 -- n2 n1 ) \ exchange first and second items on data stack
                mov     eax, 0 [esp]
                mov     0 [esp], ebx
                mov     ebx, eax
                next    c;

CODE OVER       ( n1 n2 -- n1 n2 n1 ) \ copy second item to top of data stack
                push    ebx
                mov     ebx, 4 [esp]
                next    c;

CODE ROT        ( n1 n2 n3 -- n2 n3 n1 ) \ rotate third item to top of data stack
                mov     ecx, 0 [esp]
                mov     eax, 4 [esp]
                mov     0 [esp], ebx
                mov     4 [esp], ecx
                mov     ebx, eax
                next    c;

CODE -ROT       ( n1 n2 n3 -- n3 n1 n2 ) \ rotate top of data stack to third item
                mov     ecx, 4 [esp]
                mov     eax, 0 [esp]
                mov     4 [esp], ebx
                mov     0 [esp], ecx
                mov     ebx, eax
                next    c;

CODE ?DUP       ( n -- n [n] )  \ duplicate top of data stack if non-zero
                test    ebx, ebx
                je      short @@1
                push    ebx
@@1:            next    c;

CODE NIP        ( n1 n2 -- n2 ) \ discard second item on data stack
                add     esp, # 4
                next    c;

CODE TUCK       ( n1 n2 -- n2 n1 n2 ) \ copy top data stack to under second item
                mov     eax, 0 [esp]  \ eax=n1
                mov     0 [esp], ebx  \ n2 n1 ebx=n2
                push    eax
                next    c;

CODE PICK       ( ...  k -- ... n[k] )
                mov     ebx, 0 [esp] [ebx*4]      \ just like that!
                next    c;

\ --------------------  Stack Operations --------------------

CODE SP@        ( -- addr ) \ get addr, the pointer to the top item on data stack
                push    ebx
                mov     ebx, esp
                sub     ebx, edi
                next    c;

CODE SP!        ( addr -- )     \ set the data stack to point to addr
                lea     esp, [ebx] [edi]
                pop     ebx
                next    c;

CODE RP@        ( -- a1 )       \ get a1 the address of the return stack
                push    ebx
                mov     ebx, ebp
                sub     ebx, edi
                next    c;

CODE RP!        ( a1 -- )       \ set the address of the return stack
                lea     ebp, [ebx] [edi]
                pop     ebx
                next    c;

CODE >R         ( n1 -- )       \ push n1 onto the return stack
                mov     -4 [ebp], ebx
                sub     ebp, # 4
                pop     ebx
                next    c;

CODE R>         ( -- n1 )       \ pop n1 off the return stack
                push    ebx
                mov     ebx, 0 [ebp]
                add     ebp, # 4
                next    c;

CODE R@         ( -- n1 )       \ get a copy of the top of the return stack
                push    ebx
                mov     ebx, 0 [ebp]
                next    c;

CODE DUP>R      ( n1 -- n1 )    \ push a copy of n1 onto the return stack
                mov     -4 [ebp], ebx
                sub     ebp, # 4
                next    c;

CODE R>DROP     ( -- )          \ discard one item off of the return stack
                add     ebp, # 4
                next    c;

CODE 2>R        ( n1 n2 -- )    \ push two items onto the returnstack
                mov     -2 CELLS [ebp], ebx
                pop     -1 CELLS [ebp]
                sub     ebp, # 8
                pop     ebx
                next    c;

CODE 2R>        ( -- n1 n2 )    \ pop two items off the return stack
                push    ebx
                mov     ebx, 0 CELLS [ebp]
                push    1 CELLS [ebp]
                add     ebp, # 8
                next    c;

CODE 2R@        ( -- n1 n2 )    \ get a copy of the top two items on the return stack
                push    ebx
                mov     ebx, 0 CELLS [ebp]
                push    1 CELLS [ebp]
                next    c;

\ -------------------- Memory Operators --------------------

CODE @          ( a1 -- n1 )    \ get the cell n1 from address a1
                mov     ebx, 0 [ebx] [edi]
                next    c;

CODE !          ( n1 a1 -- )    \ store cell n1 into address a1
                pop     0 [ebx] [edi]
                pop     ebx
                next    c;

CODE +!         ( n1 a1 -- )    \ add cell n1 to the contents of address a1
                pop     eax
                add     0 [ebx] [edi], eax
                pop     ebx
                next    c;

CODE C@         ( a1 -- c1 )    \ fetch the character c1 from address a1
                movzx   ebx, byte ptr 0 [ebx] [edi]
                next    c;

CODE C!         ( c1 a1 -- )    \ store character c1 into address a1
                pop     eax
                mov     0 [ebx] [edi], al
                pop     ebx
                next    c;

CODE C+!        ( c1 a1 -- )    \ add character c1 to the contents of address a1
                pop     eax
                add     0 [ebx] [edi], al
                pop     ebx
                next    c;

CODE W@         ( a1 -- w1 )    \ fetch the word (16bit) w1 from address a1
                movzx   ebx, word ptr 0 [ebx] [edi]
                next    c;

CODE SW@        ( a1 -- w1 )    \ fetch the sign extended word (16bit) w1 from address a1
                movsx   ebx, word ptr 0 [ebx] [edi]
                next    c;

CODE W!         ( w1 a1 -- )    \ store word (16bit) w1 into address a1
                pop     eax
                mov     0 [ebx] [edi], ax
                pop     ebx
                next    c;

CODE W+!        ( w1 a1 -- )    \ add word (16bit) w1 to the contents of address a1
                pop     eax
                add     0 [ebx] [edi], ax
                pop     ebx
                next    c;

\ -------------------- Cell Operators --------------------

CODE CELLS      ( n1 -- n1*cell )       \ multiply n1 by the cell size
                lea     ebx, 0 [ebx*4]
                next    c;

CODE CELLS+     ( a1 n1 -- a1+n1*cell ) \ multiply n1 by the cell size and add
                                        \ the result to address a1
                pop     eax
                lea     ebx, 0 [ebx*4] [eax]
                next    c;

CODE CELLS-     ( a1 n1 -- a1-n1*cell ) \ multiply n1 by the cell size and subtract
                                        \ the result from address a1
                lea     eax, 0 [ebx*4]
                pop     ebx
                sub     ebx, eax
                next    c;

CODE CELL+      ( a1 -- a1+cell )       \ add a cell to a1
                add     ebx, # 4
                next    c;

CODE CELL-      ( a1 -- a1-cell )       \ subtract a cell from a1
                sub     ebx, # 4
                next    c;

CODE +CELLS     ( n1 a1 -- n1*cell+a1 ) \ multiply n1 by the cell size and add
                                        \ the result to address a1
                pop     eax
                lea     ebx, 0 [eax*4] [ebx]
                next    c;

CODE -CELLS     ( n1 a1 -- a1-n1*cell ) \ multiply n1 by the cell size and
                                        \ subtract the result from address a1
                pop     eax
                lea     eax, 0 [eax*4]
                sub     ebx, eax
                next    c;

\ -------------------- Char Operators --------------------

CODE CHARS      ( n1 -- n1*char )       \ multiply n1 by the character size (1)
                next    c;

CODE CHAR+      ( a1 -- a1+char )       \ add the characters size in bytes to a1
                inc     ebx
                next    c;

\ -------------------- Address Conversion --------------------

ORIGIN [IF]                     \ if origin is non-zero:

: REL>ABS ; IMMEDIATE
: ABS>REL ; IMMEDIATE

[ELSE]                          \ make rel>abs, abs>rel comments

CODE REL>ABS    ( rel -- abs )  \ convert from a relative address as used by
                                \ Forth, to an absolute address as used by Windows
                add     ebx, edi
                next    c;

CODE ABS>REL    ( abs -- rel )  \ convert from an absolute address as used by
                                \ Windows to a relative address as used by Forth
                sub     ebx, edi
                next    c;

[THEN]

\ -------------------- Block Memory Operators --------------------

NCODE CMOVE     ( from to count -- )    \ move bytes "from" -> "to" of "count"
                mov     ecx, ebx        \ start at first byte of "from"
                mov     eax, esi
                mov     ebx, edi
                pop     edi
                pop     esi
                push    edx             \ save UP
                add     edi, ebx
                add     esi, ebx
LABEL PMOVE     mov     edx, ecx        \ CMOVE case (optimized)
                shr     ecx, # 2
                rep     movsd
                mov     ecx, edx
                and     ecx, # 3
                rep     movsb
                mov     esi, eax
                mov     edi, ebx
                pop     edx             \ restore UP
                pop     ebx
                next    c;

NCODE CMOVE>    ( from to count -- )    \ move bytes "from" -> "to" of "count"
                mov     ecx, ebx        \ start at last byte of "from"
                mov     eax, esi
                mov     ebx, edi
                dec     ecx
                pop     edi
                pop     esi
                push    edx             \ save UP
                add     edi, ecx
                add     edi, ebx
                add     esi, ecx
                add     esi, ebx
                inc     ecx
LABEL PCMOVE>   std
                repnz   movsb
                cld
                mov     esi, eax
                mov     edi, ebx
                pop     edx             \ restore UP
                pop     ebx
                next    c;

NCODE MOVE      ( source dest len -- )  \ move len bytes from source address to
                                        \ dest address
                mov     ecx, ebx
                mov     eax, esi
                mov     ebx, edi        \ ebx = base
                pop     edi
                pop     esi
                push    edx             \ save UP
                add     esi, ebx        \ absolute address
                add     edi, ebx        \ ditto
                mov     edx, edi        \ check for overlap
                sub     edx, esi
                cmp     edx, ecx
                jnb     short PMOVE     \ jump to cmove (no overlap)
                add     edi, ecx        \ CMOVE> case (dull)
                dec     edi
                add     esi, ecx
                dec     esi
                jmp     short PCMOVE>
                c;

NCODE FILL       ( addr len char -- )    \ fill addr with char for len bytes
                mov     bh, bl          \ bh & bl = char
                shl     ebx, # 16
                mov     eax, ebx
                shr     eax, # 16
                or      eax, ebx
LABEL FILLJ     mov     ebx, edi        \ ebx = base
                pop     ecx             \ ecx = len
                pop     edi             \ edi = addr
                add     edi, ebx        \ absolute address
                push    ecx             \ optimize
                shr     ecx, # 2
                rep     stosd
                pop     ecx
                and     ecx, # 3
                rep     stosb
                mov     edi, ebx        \ restore
                pop     ebx
                next    c;

NCODE ERASE     ( addr len -- )         \ fill addr for len bytes with zero
                push    ebx
                xor     eax, eax
                jmp     short fillj
                c;

NCODE BLANK     ( addr len -- )         \ fill addr for len bytes with blanks
                push    ebx
                mov     eax, # 0x20202020 \ all blanks
                jmp     short fillj
                c;            

\ -------------------- Logical Operators --------------------

CODE AND        ( n1 n2 -- n3 ) \ perform bitwise AND of n1,n2, return result n3
                pop     eax
                and     ebx, eax
                next    c;

CODE OR         ( n1 n2 -- n3 ) \ perform bitwise OR of n1,n2, return result n3
                pop     eax
                or      ebx, eax
                next    c;

CODE XOR        ( n1 n2 -- n3 ) \ perform bitwise XOR of n1,n2, return result n3
                pop     eax
                xor     ebx, eax
                next    c;

CODE INVERT     ( n1 -- n2 )    \ perform a bitwise -1 XOR on n1, return result n2
                not     ebx
                next    c;

CODE LSHIFT     ( u1 n -- u2 )  \ shift u1 left by n bits (multiply)
                mov     ecx, ebx
                pop     ebx
                shl     ebx, cl
                next    c;

CODE RSHIFT     ( u1 n -- u2 )  \ shift u1 right by n bits (divide)
                mov     ecx, ebx
                pop     ebx
                shr     ebx, cl
                next    c;

CODE INCR       ( addr -- )     \ increment the contents of addr
                inc     dword ptr 0 [ebx] [edi]
                pop     ebx
                next    c;

CODE DECR       ( addr -- )     \ decrement the contents of addr
                dec     dword ptr 0 [ebx] [edi]
                pop     ebx
                next    c;

CODE CINCR      ( addr -- )     \ increment the BYTE contents of addr
                inc     byte 0 [ebx] [edi]
                pop     ebx
                next    c;

CODE CDECR      ( addr -- )     \ decrement the BYTE contents of addr
                dec     byte 0 [ebx] [edi]
                pop     ebx
                next    c;

CODE ON         ( addr -- )     \ set the contents of addr to ON (-1)
                mov     dword ptr 0 [ebx] [edi], # -1
                pop     ebx
                next    c;

CODE OFF        ( addr -- )     \ set the contests of addr of OFF (0)
                mov     dword ptr 0 [ebx] [edi], # 0
                pop     ebx
                next    c;

CODE TOGGLE     ( addr byte -- ) \ XOR the byte contents of "addr" with "byte"
                pop     eax
                xor     0 [eax] [edi], bl
                pop     ebx
                next    c;

\ -------------------- Arithmetic Operators --------------------

CODE +          ( n1 n2 -- n3 ) \ add n1 to n2, return sum n3
                pop     eax
                add     ebx, eax
                next    c;

CODE -          ( n1 n2 -- n3 ) \ subtract n2 from n1, return difference n3
                pop     eax
                sub     eax, ebx
                mov     ebx, eax
                next    c;

CODE UNDER+     ( a x b -- a+b x ) \ add top of stack to third stack item
                add     4 [esp], ebx
                pop     ebx
                next    c;

CODE NEGATE     ( n1 -- n2 ) \ negate n1, returning 2's complement n2
                neg     ebx
                next    c;

CODE ABS        ( n -- |n| ) \ return the absolute value of n1 as n2
                test    ebx, ebx
                jns     short @@1
                neg     ebx
@@1:            next    c;

CODE 2*         ( n1 -- n2 ) \ multiply n1 by two
                lea     ebx, 0 [ebx] [ebx]
                next    c;

CODE 2/         ( n1 -- n2 ) \ signed divide n1 by two
                sar     ebx, # 1
                next    c;

CODE U2/        ( n1 -- n2 ) \ unsigned divide n1 by two
                shr     ebx, # 1
                next    c;

CODE 1+         ( n1 -- n2 ) \ add one to n1
                inc     ebx
                next    c;

CODE 1-         ( n1 -- n2 ) \ subtract one from n1
                dec     ebx
                next    c;

CODE D2*        ( d1 -- d2 ) \ multiply the double number d1 by two
                pop     eax
                shl     eax, # 1
                rcl     ebx, # 1
                push    eax
                next    c;

CODE D2/        ( d1 -- d2 ) \ divide the double number d1 by two
                pop     eax
                sar     ebx, # 1
                rcr     eax, # 1
                push    eax
                next    c;

\ -------------------- Unsigned Multiply & Divide --------------------

CODE UM*        ( u1 u2 -- ud1 ) \ multiply unsigned u1 by unsigned u2
                mov     ecx, edx        \ save UP
                pop     eax
                mul     ebx
                push    eax
                mov     ebx, edx
                mov     edx, ecx        \ restore UP
                next    c;

CODE UM/MOD     ( ud1 u1 -- rem quot ) \ divide unsigned double ud1 by the
                                       \ unsigned number u1
                mov     ecx, edx        \ save UP
                pop     edx
                pop     eax
                div     ebx
                push    edx
                mov     ebx, eax
                mov     edx, ecx        \ restore UP
                next    c;

CODE WORD-SPLIT ( u1 -- low high ) \ split the unsigned 32bit u1 into its high
                                   \ and low 16bit quantities.
                xor     eax, eax
                mov     ax, bx
                push    eax
                shr     ebx, # 16
                next    c;

CODE WORD-JOIN  ( low high -- n1 ) \ join the high and low 16bit quantities
                                   \ into a single 32bit n1
                shl     ebx, # 16
                pop     eax
                mov     bx, ax
                next    c;

\ -------------------- Comparison Operators --------------------

CODE 0=         ( n1 -- f1 )    \ return true if n1 equals zero
                sub     ebx, # 1
                sbb     ebx, ebx
                next    c;

CODE 0<>        ( n1 -- f1 )    \ return true if n1 is not equal to zero
                sub     ebx, # 1
                sbb     ebx, ebx
                not     ebx
                next    c;

CODE 0<         ( n1 -- f1 )    \ return true if n1 is less than zero
                sar     ebx, # 31
                next    c;

CODE 0>         ( n1 -- f1 )    \ return true if n1 is greater than zero
                dec     ebx
                sar     ebx, # 31
                not     ebx
                next    c;

CODE =          ( n1 n2 -- f1 ) \ return true if n1 is equal to n2
                sub     ebx, 0 [esp]
                sub     ebx, # 1
                sbb     ebx, ebx
                add     esp, # 4
                next    c;

CODE <>         ( n1 n2 -- f1 ) \ return true if n1 is not equal to n2
                pop     eax
                sub     eax, ebx
                neg     eax
                sbb     ebx, ebx
                next    c;

CODE >          ( n1 n2 -- f1 ) \ return true if n1 is greater than n2
                pop     eax
                sub     ebx, eax
                sar     ebx, # 31
                next    c;

CODE <          ( n1 n2 -- f1 ) \ return true if n1 is less than n2
                mov     eax, ebx
                pop     ebx
                sub     ebx, eax
                sar     ebx, # 31
                next    c;

CODE <=         ( n1 n2 -- f1 ) \ return true if n1 is less than or equal to n2
                pop     eax
                sub     ebx, eax
                sar     ebx, # 31
                not     ebx
                next    c;

CODE >=         ( n1 n2 -- f1 ) \ return true if n1 is greater or equal to n2
                pop     eax
                dec     ebx
                sub     ebx, eax
                sar     ebx, 31
                next    c;
                
CODE U<         ( u1 u2 -- f1 ) \ return true if unsigned u1 is less than
                                \ unsigned u2
                pop     eax
                cmp     eax, ebx
                sbb     ebx, ebx
                next    c;

CODE U>         ( u1 u2 -- f1 ) \ return true if unsigned u1 is greater than
                                \ unsigned n2
                pop     eax
                cmp     ebx, eax
                sbb     ebx, ebx
                next    c;

CODE DU<        ( ud1 ud2 -- f1 ) \ return true if unsigned double ud1 is
                                  \ less than undigned double ud2
                pop     eax
                pop     ecx
                xchg    edx, 0 [esp]    \ save UP
                sub     edx, eax
                sbb     ecx, ebx
                sbb     ebx, ebx
                pop     edx             \ restore UP
                next c;

CODE UMIN       ( u1 u2 -- n3 ) \ return the lesser of unsigned u1 and
                                \ unsigned u2
                pop     eax
                cmp     ebx, eax
                jb      short @@1
                mov     ebx, eax
@@1:            next    c;

CODE MIN        ( n1 n2 -- n3 ) \ return the lesser of n1 and n2
                pop     eax
                cmp     ebx, eax
                jl      short @@1
                mov     ebx, eax
@@1:            next    c;

CODE UMAX       ( u1 u2 -- n3 ) \ return the greater of unsigned u1 and
                                \ unsigned u2
                pop     eax
                cmp     ebx, eax
                ja      short @@1
                mov     ebx, eax
@@1:            next    c;

CODE MAX        ( n1 n2 -- n3 ) \ return the greater of n1 and n2
                pop     eax
                cmp     ebx, eax
                jg      short @@1
                mov     ebx, eax
@@1:            next    c;

CODE 0MAX       ( n1 -- n2 ) \ return n2 the greater of n1 and zero
                cmp     ebx, # 0
                jg      short @@1
                xor     ebx, ebx
@@1:            next    c;

CODE WITHIN     ( n1 low high -- f1 ) \ f1=true if ((n1 >= low) & (n1 < high))
                pop     eax
                pop     ecx
                sub     ebx, eax
                sub     ecx, eax
                sub     ecx, ebx
                sbb     ebx, ebx
                next    c;

CODE BETWEEN    ( n1 low high -- f1 ) \ f1=true if ((n1 >= low) & (n1 <= high))
                inc     ebx           \ bump high
                jmp     ' within >body \ and go to
                c;

\ -------------------- Double memory Operators --------------------

CODE 2@         ( a1 -- d1 ) \ fetch the double number n1 from address a1
                push    4 [ebx] [edi]
                mov     ebx, 0 [ebx] [edi]
                next    c;

CODE 2!         ( d1 a1 -- ) \ store the double number d1 into address a1
                pop     0 [ebx] [edi]
                pop     4 [ebx] [edi]
                pop     ebx
                next    c;

\ -------------------- Double Stack Operators --------------------

CODE 2DROP      ( n1 n2 -- ) \ discard two single items from the data stack
                add     esp, # 4
                pop     ebx
                next    c;

CODE 2DUP       ( n1 n2 -- n1 n2 n1 n2 ) \ duplicate the top two single items
                                         \ on the data stack
                push    ebx
                push    4 [esp]
                next    c;

CODE 2SWAP      ( n1 n2 n3 n4 -- n3 n4 n1 n2 )
                mov     eax, 4 [esp]      \ eax=n2
                mov     ecx, 8 [esp]      \ ecx=n1
                mov     4 [esp], ebx      \ n1 n4 n3 eax=n2 ecx=n1 ebx=n4
                mov     ebx, 0 [esp]      \ ebx=3
                mov     0 [esp], ecx      \ n3 n4 n1
                mov     8 [esp], ebx      \ n3 n4 n3
                mov     ebx, eax          \ n3 n4 n1 n2
                next    c;

CODE 2OVER      ( n1 n2 n3 n4 -- n1 n2 n3 n4 n1 n2 )
                mov     eax, 8 [esp]
                push    ebx
                push    eax
                mov     ebx, 12 [esp]
                next    c;

CODE 2ROT       ( n1 n2 n3 n4 n5 n6 -- n3 n4 n5 n6 n1 n2)   \ slow!
                pop     eax
                xchg    ebx, 0 [esp]
                xchg    eax, 4 [esp]
                xchg    ebx, 8 [esp]
                xchg    eax, 12 [esp]
                push    eax
                next    c;

CODE 3DROP      ( n1 n2 n3 -- ) \ discard three items from the data stack
                add     esp, # 8
                pop     ebx
                next    c;

CODE 4DROP      ( n1 n2 n3 n4 -- ) \ discard four items from the data stack
                add     esp, # 12
                pop     ebx
                next    c;

CODE 3DUP       ( n1 n2 n3 -- n1 n2 n3 n1 n2 n3 )
                mov     eax, 0 [esp]      \ n2
                mov     ecx, 4 [esp]      \ n1
                push    ebx               \ n3
                push    ecx               \ n1
                push    eax               \ n2
                next    c;

CODE 4DUP       ( a b c d -- a b c d a b c d )
                mov     eax, 8 [esp]
                push    ebx
                push    eax
                mov     ebx, 12 [esp]
                mov     eax, 8 [esp]
                push    ebx
                push    eax
                mov     ebx, 12 [esp]
                next    c;

\ -------------------- Signed Multiply & Divide --------------------

CODE M*         ( n1 n2 -- d1 ) \ multiply n1 by n2, return double result d1
                mov     ecx, edx        \ save UP
                pop     eax
                imul    ebx
                push    eax
                mov     ebx, edx
                mov     edx, ecx        \ restore UP
                next    c;

CODE *          ( n1 n2 -- n3 ) \ multiply n1 by n2, return single result n3
                mov     ecx, edx        \ save UP
                pop     eax
                mul     ebx
                mov     ebx, eax
                mov     edx, ecx        \ restore UP
                next    c;

CODE SM/REM     ( d n -- rem quot )
                mov     ecx, edx        \ save UP
                pop     edx
                pop     eax
                idiv    ebx
                push    edx
                mov     ebx, eax
                mov     edx, ecx        \ restore UP
                next    c;

CODE FM/MOD     ( d n -- rem quot )
                pop     ecx             \ high numerator
                mov     eax, ecx        \ copy for testing
                xor     eax, ebx        \ test against denominator
                jns     short @@1       \ if signs differ jump

                xchg    ecx, edx        \ save UP
                pop     eax
                idiv    ebx
                test    edx, edx        \ set zero flag
                je      short @@2
                add     edx, ebx        \ add divisor to remainder
                dec     eax             \ decrement quotient
                jmp     short @@2

@@1:            xchg    ecx, edx        \ preserve DX in CX, DX=high num
                pop     eax             \ EAX=low numerator
                idiv    ebx             \ perform the division

@@2:            push    edx             \ push remainder
                mov     ebx, eax        \ quotient to EBX
                mov     edx, ecx        \ restore UP
                next    c;

CODE /MOD       ( n1 n2 -- rem quot )
                pop     ecx
                mov     eax, ecx
                xor     eax, ebx
                jns     short @@1

                mov     eax, ecx        \ low order part to eax
                mov     ecx, edx        \ save UP
                cdq
                idiv    ebx
                test    edx, edx        \ set zero flag
                je      short @@2

                add     edx, ebx        \ add divisor to remainder
                dec     eax             \ decrement quotient
                jmp     short @@2

@@1:            mov     eax, ecx        \ low order part to eax
                mov     ecx, edx
                cdq
                idiv    ebx
@@2:            push    edx
                mov     ebx, eax
                mov     edx, ecx        \ restore UP
                next    c;

: /             ( n1 n2 -- n3 )
                /MOD NIP ;

: MOD           ( n1 n2 -- n3 )
                /MOD DROP ;

CODE */MOD      ( n1*n2/n3 -- remainder quotient)
                pop     ecx
                pop     eax
                push    edx             \ save UP
                imul    ecx
                mov     ecx, edx
                xor     ecx, ebx
                jns     short @@1

                idiv    ebx
                test    edx, edx        \ set zero flag
                je      short @@2
                add     edx, ebx        \ add divisor to remainder
                dec     eax             \ decrement quotient
                jmp     short @@2

@@1:            idiv    ebx
@@2:            mov     ebx, eax
                pop     ecx
                push    edx
                mov     edx, ecx        \ restore UP
                next    c;

: */            ( n1*n2/n3 -- quotient )
                */MOD NIP ;

\ -------------------- Class Primatives --------------------

CODE SKIP       ( adr len char -- adr' len' )
                pop     ecx
                jecxz   short @@2
                mov     eax, ebx                \ eax = character
                mov     ebx, edi                \ ebx = forth base address
                pop     edi
                add     edi, ebx                \ edi = absolute address
                repz    scasb
                je      short @@1
                inc     ecx
                dec     edi
@@1:            sub     edi, ebx                \ relative
                push    edi
                mov     edi, ebx                \ restore base ptr
@@2:            mov     ebx, ecx
                next    c;

CODE SCAN       ( adr len char -- adr' len' )
                pop     ecx
                jecxz   short @@2
                mov     eax, ebx                \ eax = character
                mov     ebx, edi                \ ebx = forth base address
                pop     edi
                add     edi, ebx                \ edi = absolute address
                repnz   scasb
                jne     short @@1
                inc     ecx
                dec     edi
@@1:            sub     edi, ebx                \ relative
                push    edi
                mov     edi, ebx                \ restore base ptr
@@2:            mov     ebx, ecx
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

\ COMPARE compares two strings. The return value is:
\
\        0 = string1 = string2
\       -1 = string1 < string2
\        1 = string1 > string2

CODE COMPARE    ( adr1 len1 adr2 len2 -- n )
                sub     ebp, # 8
                mov     0 [ebp], edi
                mov     4 [ebp], esi
                pop     eax                     \ eax = adr2
                pop     ecx                     \ ecx = len1
                pop     esi                     \ esi = adr1
                add     esi, edi                \ absolute address
                add     edi, eax                \ edi = adr2 (abs)
                xor     eax, eax                \ default is 0 (strings match)
                cmp     ecx, ebx                \ compare lengths
                je      short @@2
                ja      short @@1
                dec     eax                     \ if len1 < len2, default is -1
                jmp     short @@2
@@1:            inc     eax                     \ if len1 > len2, default is 1
                mov     ecx, ebx                \ and use shorter length
@@2:            repz    cmpsb                   \ compare the strings
                je      short @@4               \ if equal, return default
                jns     short @@3
                mov     eax, # -1               \ if str1 < str2, return -1
                jmp     short @@4
@@3:            mov     eax, # 1                \ if str1 > str2, return 1
@@4:            mov     ebx, eax
                mov     edi, 0 [ebp]
                mov     esi, 4 [ebp]
                add     ebp, # 8
                next    c;

\ Search str1 for substring str2.
\ If found, return the address of the start of the
\ string, the characters remaining in str1 and a true flag.
\ Otherwise return the original str1 and a false flag.

\ ESI = pointer to source string (str2)
\ EBX = length  of source string
\ EDI = pointer to destination string (str1)
\ ECX = length  of destination string
\ EDX = pointer for compare

CODE SEARCH     ( adr1 len1 adr2 len2 -- adr3 len3 flag )
                test    ebx, ebx
                jne     short @@1
                pop     eax
                dec     ebx             \ zero length matches
                jmp     short @@6
@@1:            sub     ebp, # 12
                mov     0 [ebp], edx    \ save UP
                mov     4 [ebp], edi
                mov     8 [ebp], esi
                pop     esi
                add     esi, edi
                mov     ecx, 0 [esp]
                add     edi, 4 [esp]
                jmp     short @@2
@@4:            inc     edi             \ go to next    c; char in destination
                dec     ecx
@@2:            cmp     ecx, ebx        \ enough room for match?
                jb      short @@5
                sub     edx, edx        \ starting index
@@3:            mov     al, 0 [edx] [esi]
                cmp     al, 0 [edx] [edi]
                jne     short @@4
                inc     edx
                cmp     edx, ebx
                jne     short @@3
                mov     eax, edi        \ found
                mov     edx, 0 [ebp]
                mov     edi, 4 [ebp]
                mov     esi, 8 [ebp]
                add     ebp, # 12
                sub     eax, edi        \ relative address
                mov     4 [esp], eax
                mov     0 [esp], ecx
                mov     ebx, # -1       \ true flag
                jmp     short @@6
@@5:            sub     ebx, ebx        \ not found
                mov     edx, 0 [ebp]    \ restore UP
                mov     edi, 4 [ebp]
                mov     esi, 8 [ebp]
                add     ebp, # 12
@@6:            next    c;

CODE LARGEST    ( a1 n1 --- a2 n2 )
                mov     ecx, ebx          \ count of array to search
                pop     ebx               \ starting address of array
                push    edx               \ save UP
                xor     eax, eax          \ starting highest value = 0
                mov     edx, ebx          \ highest value addr = start address
@@1:            cmp     0 [ebx] [edi], eax  \ compare 32bit words
                jle     short @@2         \ branch if not greater
                mov     edx, ebx          \ if greater, save offset in EDX
                mov     eax, 0 [ebx] [edi]  \ and contents in EAX
@@2:            add     ebx, # 4          \ bump to next element
                dec     ecx
                jnz     short @@1
                mov     ecx, edx
                pop     edx               \ restore UP
                push    ecx
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

\ -------------------- Double Arithmetic Operators --------------------

CODE D+         ( d1 d2 -- d3 ) \ perform a double add (64bit)
                pop     eax
                pop     ecx
                add     0 [esp], eax
                adc     ebx, ecx
                next    c;

CODE D-         ( d1 d2 -- d3 ) \ perform a double subtract (64bit)
                pop     eax
                pop     ecx
                sub     0 [esp], eax
                sbb     ecx, ebx
                mov     ebx, ecx
                next    c;

CODE DNEGATE    ( d1 -- d2 )    \ negate d1, returning 2's complement d2
                pop     eax
                neg     ebx
                neg     eax
                sbb     ebx, # 0
                push    eax
                next    c;

CODE DABS       ( d1 -- d2 )    \ return the absolute value of d1 as d2
                test    ebx, ebx
                jns     short @@1
                pop     eax
                neg     ebx
                neg     eax
                sbb     ebx, # 0
                push    eax
@@1:            next    c;

CODE S>D        ( n1 -- d1 ) \ convert single signed single n1 to a signed
                             \ double d1
                push    ebx
                shl     ebx, 1          \ put sign bit into carry
                sbb     ebx, ebx
                next    c;

: D=            ( d1 d2 -- f1 ) \ f1=true if double d1 is equal to double d2
                D- OR 0= ;

CODE D0<        ( d1 -- f1 )
\ Signed compare d1 double number with zero.  If d1 < 0, return TRUE.
                sar     ebx, # 31
                pop     ecx
                next    c;

CODE D<         ( d1 d2 -- f )
\ Signed compare two double numbers.  If d1 < d2, return TRUE.
                pop     eax
                pop     ecx
                cmp     0 [esp], eax
                pop     eax
                sbb     ecx, ebx
                mov     ebx, # 0
                jge     short @@1
                dec     ebx
@@1:            next    c;


: D>            ( d1 d2 -- f )
\ Signed compare two double numbers.  If d1 > d2 , return TRUE.
                2SWAP D<   ;

: D<>           ( d1 d2 -- d )
\ Signed compare two double numbers.  If d1 not equal d2 , return TRUE.
                D= 0= ;

: DMIN          ( d1 d2 -- d3 )
\  Replace the top two double numbers with the smaller of the two (signed).
                4DUP D> IF  2SWAP  THEN 2DROP ;

: DMAX          ( d1 d2 -- d3 )
\  Replace the top two double numbers with the larger of the two (signed).
                4DUP D< IF  2SWAP  THEN  2DROP ;        \ 05/25/90 tjz

\ -------------------- Strings --------------------

CODE COUNT      ( str -- addr len ) \ byte counted strings
                movzx   eax, byte ptr 0 [ebx] [edi]
                inc     ebx
                push    ebx
                mov     ebx, eax
                next    c;

CODE WCOUNT     ( str -- addr len )  \ word counted strings
                movzx   eax, word ptr 0 [ebx] [edi]
                inc     ebx
                inc     ebx
                push    ebx
                mov     ebx, eax
                next    c;

CODE LCOUNT     ( str -- addr len )  \ long counted strings
                mov     eax, 0 [ebx] [edi]
                add     ebx, # 4
                push    ebx
                mov     ebx, eax
                next    c;

: "CLIP"        ( a1 n1 -- a1 n1 )   \ clip a string to between 0 and MAXCOUNTED
                MAXCSTRING MIN 0 MAX ;

: PLACE         ( addr len dest -- )
                SWAP "CLIP" SWAP
                2DUP 2>R
                CHAR+ SWAP MOVE
                2R> C! ;

: +PLACE        ( addr len dest -- ) \ append string addr,len to counted
                                     \ string dest
                >R "CLIP" MAXCSTRING R@ C@ - MIN R>
                                        \ clip total to MAXCOUNTED string
                2DUP 2>R
                COUNT CHARS + SWAP MOVE
                2R> C+! ;

CODE -TRAILCHARS ( addr n1 c1 -- addr n2 ) \ remove trailing c1's from addr,n1
                mov     eax, ebx
                pop     ecx
                jecxz   short @@2
                mov     ebx, 0 [esp]
                add     ebx, ecx
@@1:            dec     ebx
                cmp     0 [ebx] [edi], al
                jne     short @@2
                dec     ecx
                jnz     short @@1
@@2:            mov     ebx, ecx
                next    c;

: -TRAILING     ( addr n1 -- addr n2 ) \ remove trailing blanks from addr,n1
                BL -TRAILCHARS ;

: -NULLS        ( addr n1 -- addr n2 ) \ remove trailing nulls from addr,n1
                0 -TRAILCHARS ;

\ remove n1 characters from the beginning of string addr1,len1
\ if n1 greater than len1, then returned len2 will be zero.

\ ** November 27th, 2000 - 12:00 tjz
\ ** the following statment is no longer true **
\ if n1 lessthan zero, then returned len2 will be zero.

CODE /STRING    ( addr1 len1 n1 -- addr2 len2 )
                pop     eax
// November 27th, 2000 - 11:58 tjz
// Added the following two lines to allow a negative argument to be passed
// to /STRING, such that the string will be expanded.  Dangerous, but legal.
                cmp     ebx, # 0
                jle     short @@1
                cmp     ebx, eax
                jbe     short @@1
                mov     ebx, eax
@@1:            add     0 [esp], ebx
                sub     eax, ebx
                mov     ebx, eax
                next    c;

\ ---------------------- Proc link offset routines ----------------------

code lib>handle ( addr -- addr )            \ offset from link to pfa
                add    ebx, # 4
                next   c;

code lib>type   ( addr -- addr )            \ offset to lib type
                add    ebx, # 8
                next   c;

code lib>name   ( addr -- addr )            \ offset to lib name
                add    ebx, # 9
                next   c;

code cfa>proc   ( addr -- addr )            \ offset to proc from cfa
                sub    ebx, # 4
                next   c;

code proc>cfa   ( addr -- addr )            \ offset to proc cfa
                add    ebx, # 4
                next   c;

code proc>ep    ( addr -- addr )            \ offset to proc ep
                add    ebx, # 8
                next   c;

code proc>lib   ( addr -- addr)             \ offset to lib pointer
                add    ebx, # 12
                next   c;

code proc>pcnt  ( addr -- addr )            \ offset to proc count
                add    ebx, # 16
                next   c;

code proc>name  ( addr -- addr )            \ offset to proc name
                add    ebx, # 17
                next   c;

\ ------------------- Early procedure support ------------------------------

|: res-wincall  ( proc-cfa -- )                 \ only called by docall-later
                dup res-loadcall                \ resolve call
                execute                         \ execute it
                ;

CFA-CODE DOCALL ( [ n ] -- r )                    \ runtime for a DLL call
                push    ebx
                mov     ebx, edx                  \ save UP
                call    4 [eax] [edi]             \ call address is absolute!!!
                mov     edx, ebx                  \ restore UP
                mov     ebx, eax
                next    c;

CFA-CODE DOCALL-LATER ( -- )                    \ called to resolve from DOCALL
                pop      ecx                    \ drop the return address
                mov      ebx, eax               \ top of stack is proc-cfa
                mov      eax, 8 [eax] [edi]     \ point at res-wincall
                exec     c;                     \ go do it

ASSEMBLER DOCALL        META CONSTANT DOCALL
ASSEMBLER DOCALL-LATER  META CONSTANT DOCALL-LATER

|: 0winproc     ( proc-addr -- )                 \ init proc at proc-addr
                docall over proc>cfa !           \ cfa is docall
                docall-later rel>abs over proc>ep ! \ docall-later must be absolute address
                ['] res-wincall swap proc>lib !
                ;

\ -------------------- User Variables --------------------
\ Task based variables are here. Each task has its own local copy of an uninitialised
\ variable. All other VARIABLEs and VALUEs are global the the entire process, and must
\ not be changed in a task unless locked or you know what you're doing.

VARIABLE NEXT-USER                              \ offset of next user variable

CODE UP@        ( -- addr )                     \ get the pointer to the user area
                push    ebx
                mov     ebx, fs: 0x14           \ TIB pvArbitrary is user base
                sub     ebx, edi
                next    c;

CODE UP!        ( addr -- )                     \ set the pointer to the user area
                lea     edx, 0 [ebx] [edi]      \ make absolute user base
                mov     fs: 0x14 , edx          \ save in TIB pvArbitrary
                pop     ebx
                next    c;

-2 CELLS ( init offset so TCB is 0 ) ( order IS important )
  DUP USER RP0        CELL+ ( initial return stack pointer )
  DUP USER SP0        CELL+ ( initial data stack pointer )
  DUP USER TCB        CELL+ ( task control block )
  DUP ( *1 )                ( absolute min user area )
  DUP USER HANDLER    CELL+ ( throw frame )
  DUP USER MSG        CELL+ ( abort message pointer )
  DUP USER LP         CELL+ ( local variable pointer )
  DUP USER OP         CELL+ ( object pointer )
  DUP USER BASE       CELL+ ( numeric radix )
  DUP | USER RLLEN    CELL+ ( read line length, used in file i/o see read-line )
  DUP | USER RLNEOF   CELL+ ( read line not eof, used in file i/o see read-line )
  DUP USER HLD        CELL+ ( numeric output pointer )
         80 CHARS ALIGNED + ( numeric output formatting buffer )
  DUP USER PAD        CELL+ ( extra )
  MAXSTRING CHARS ALIGNED + ( user string buffer )
  DUP USER C#         CELL+ ( text column position )
  DUP USER L#         CELL+ ( text column position )
  DUP NEXT-USER !-T                 ( save top user variable offset in NEXT-USER )
( *2 )    OVER - CONSTANT USEREXTRA ( add to USERMIN if you are going to do I/O )
( *1 ) 3 CELLS + CONSTANT USERMIN   ( absolute min user area )
       TCB RP0 - CONSTANT USEROFFS  ( user offset, 2 cells )

\ -------------------- System Variables --------------------

\ CREATE POCKET           MAXSTRING ALLOT   \ now allocated dynamically, see : COLD
\ CREATE CUR-FILE         MAXSTRING ALLOT   
\ CREATE TEMP$            MAXSTRING ALLOT
\ CREATE FIND-BUFFER      MAXSTRING ALLOT
0 VALUE POCKET
0 VALUE CUR-FILE
0 VALUE TEMP$
0 VALUE FIND-BUFFER

NUMVOCS CONSTANT #VOCS

VARIABLE CONTEXT  #VOCS CELLS ALLOT     \ make context array of #VOCS+1 cells
VARIABLE CURRENT
VARIABLE LAST                   \ NFA of last header created
VARIABLE LAST-LINK              \ address of last link for last header created
VARIABLE VOC-LINK               \ linked list of vocabularies

0 VALUE SOURCE-ID
0 VALUE SOURCE-POSITION

\ CREATE TIB                    \ terminal input buffer, now allocated dynamically, see : COLD
\         MAXSTRING 2 + ALLOT

0 VALUE TIB
MAXSTRING 2 + EQU TIBLEN        \

CREATE (SOURCE)                 \ input stream pointer
        0 ,                     \ length of input stream
        TIB ,                   \ address of input stream, adjusted in : COLD

(SOURCE) CONSTANT #TIB          \ address of terminal input buffer length

(SOURCE)     EQU S_LEN          \ source address & length for the assembler
(SOURCE) 4 + EQU S_ADR

VARIABLE >IN
VARIABLE STATE
VARIABLE WARNING TRUE WARNING !-T
VARIABLE CAPS    TRUE CAPS    !-T

CREATE .SMAX   8 ,              \ max number of stack entries to show

VARIABLE LOADLINE
VARIABLE ?LOADING

\ -------------------- External C Calls --------------------

39      equ printdlg_x          \ 08/15/94 17:13 tjz added
40      equ startprint_x        \ 08/15/94 17:13 tjz added
41      equ pageprint_x         \ 08/16/94 13:29 tjz added
42      equ endprint_x          \ 08/16/94 13:29 tjz added
43      equ initprint_x         \ 08/16/94 14:55 tjz added
44      equ closeprint_x        \ 08/16/94 15:01 tjz added
57      equ qualityprint_x      \ 10/03/94 14:55 tjz added
58      equ startpage_x         \ 12/16/94 09:12 tjz added
59      equ endpage_x           \ 12/16/94 09:13 tjz added
60      equ printcopies_x       \ 12/30/94 15:22 tjz added
61      equ printfrompages_x    \ 12/30/94 15:23 tjz added
62      equ printtopages_x      \ 12/30/94 15:23 tjz added
67      equ orientation_x       \ April 17th, 1996 - 17:44 yjz added
77      equ initprint2_x        \ January 31st, 2002 - 16:17 rls added
78      equ printflags_x        \ February 4th, 2002 - 5:35 rls added

0       equ exit_x
1       equ initconsole_x
2       equ key_x
3       equ accept_x
4       equ emit_x
5       equ type_x
6       equ cr_x
7       equ cls_x
8       equ keyq_x
9       equ qcr_x
19      equ gotoxy_x
20      equ getxy_x
23      equ getcolrow_x
27      equ setfgbg_x
28      equ pushkey_x
29      equ thescreen_x         \ 06/22/94 10:01 tjz added
30      equ charwh_x            \ 06/22/94 10:43 tjz added
31      equ shiftmask_x         \ 06/22/94 11:49 tjz added
32      equ resize_x            \ 07/06/94 15:56 tjz added
35      equ cursorshape_x       \ 07/12/94 10:47 tjz added
36      equ getcursor_x         \ 07/12/94 12:42 tjz added
37      equ havemenu_x          \ 07/26/94 17:51 tjz added
51      equ wscroll_x           \ 09/15/94 15:32 tjz added
52      equ rowoffset_x         \ 09/15/94 16:21 tjz added
53      equ maxcolrow_x         \ 09/15/94 16:21 tjz added
54      equ setmaxcolrow_x      \ 09/20/94 10:28 tjz added
56      equ normconsole_x       \ 09/27/94 09:51 tjz added
66      equ sizestate_x         \ March 11th, 1996 - 10:39 tjz added
71      equ setcharwh_x         \ December 9th, 1996 - 8:31 tjz added
72      equ mark_x              \ May 20th, 1997 - 18:40 tjz added
73      equ getfg_x             \ March 1st, 2000 - 15:49 tjz added
74      equ getbg_x             \ March 1st, 2000 - 15:49 tjz added

CODE XCALL      ( [...] func# -- result )       \ from XCALL
                xchg    ebx, edx                \ save UP
                lea     edx, 0 [edx*4]
                add     edx, table [edi] 
                call    0 [edx]
                mov     edx, ebx                \ restore UP
                mov     ebx, eax                \ function return value
                next    c;
                
DEFER INIT-CONSOLE
DEFER INIT-SCREEN
DEFER SIZESTATE
DEFER MARKCONSOLE

-1 VALUE STDOUT
-1 VALUE STDERR
-1 VALUE STDIN

1 PROC GetStdHandle
1 PROC AllocConsole
1 PROC FreeConsole

: _DOSCONSOLE   ( fl -- )                        \ true = open, false = close
                if call AllocConsole drop
                  STD_OUTPUT_HANDLE Call GetStdHandle to STDOUT
                  STD_INPUT_HANDLE  Call GetStdHandle to STDIN
                  STD_ERROR_HANDLE  Call GetStdHandle to STDERR
                else call FreeConsole drop
                then ;

defer unload-forth ' noop is unload-forth         \ things to do at end

1 PROC ExitProcess

: _BYE          ( -- )          \ Exit Forth
                unload-forth
                exit_x xcall
\                0 call ExitProcess 
                 ;

: _INIT-CONSOLE ( -- f1 )       \ initialize the Forth console window
                                \ f1=false if already inited
                HCON @ >R
                initconsole_x XCALL
                DUP HCON !
                0= THROW
                R> HCON @ -
                ;

2 PROC ShowWindow

: _INIT-SCREEN   ( -- )          \ init the screen
                INIT-CONSOLE DROP
                SW_NORMAL HCON @ call ShowWindow DROP ;

: EKEY          ( -- key )
                key_x XCALL ;

: _ACCEPT       ( addr len -- n )
                SWAP REL>ABS
                accept_x XCALL ;

CODE _TYPE      ( addr len -- )                 \ SWAP REL>ABS TYPE_X XCALL DROP
                push    edx
                lea     edx, C# [UP]
                add     [edx], ebx
                pop     edx
                
                pop     eax
                add     eax, edi
                push    ebx
                push    eax
                mov     ebx, # type_x
                xchg    ebx, edx                \ save UP
                lea     edx, 0 [edx*4]
                add     edx, table [edi]
                call    0 [edx]
                mov     edx, ebx                \ restore UP
                pop     ebx                     \ drop
                next    c;

CODE _EMIT      ( char -- )                     \ SP@ REL>ABS 1 _TYPE DROP
                inc     C# [UP]
                push    ebx
                mov     eax, esp
                push    # 1
                push    eax
                mov     ebx, # type_x
                xchg    ebx, edx                \ save UP
                lea     edx, 0 [edx*4]
                add     edx, table [edi]
                call    0 [edx]
                mov     edx, ebx                \ restore UP
                add     esp, # 4                \ drop drop 
                pop     ebx
                next    c;

: _CR           ( -- )
                0 C# ! 
                cr_x XCALL DROP ;

: _CLS          ( -- )
                cls_x XCALL DROP ;

: EKEY?         ( -- f )
                keyq_x XCALL ;

: _?CR          ( n -- )
                qcr_x XCALL DROP ;

: _SIZESTATE     ( -- state )    \ state of the display
                sizestate_x XCALL ;

: _GOTOXY       ( x y -- )
                SWAP gotoxy_x XCALL DROP ;

: _GETXY        ( -- x y )
                getxy_x XCALL WORD-SPLIT ;

: _GETCOLROW    ( -- cols rows )
                getcolrow_x XCALL WORD-SPLIT ;
                
: _MARKCONSOLE   ( startline startcol endline endcol -- )
                mark_x XCALL DROP ;
                
: PRINT-ORIENTATION ( f1 -- hDC ) \ true = landscape
                orientation_x XCALL ;

\ ------------------- Deferred I/O -------------------


DEFER BYE       ' _BYE       IS BYE
' _INIT-CONSOLE IS INIT-CONSOLE
' _INIT-SCREEN  IS INIT-SCREEN
' _SIZESTATE    IS SIZESTATE
' _MARKCONSOLE  IS MARKCONSOLE

DEFER KEY       ' EKEY       IS KEY
DEFER ACCEPT    ' _ACCEPT    IS ACCEPT
DEFER EMIT      ' _EMIT      IS EMIT
DEFER TYPE      ' _TYPE      IS TYPE
DEFER CR        ' _CR        IS CR
DEFER CLS       ' _CLS       IS CLS
DEFER KEY?      ' EKEY?      IS KEY?
DEFER ?CR       ' _?CR       IS ?CR
DEFER CONSOLE   ' NOOP       IS CONSOLE
DEFER GOTOXY    ' _GOTOXY    IS GOTOXY
DEFER GETXY     ' _GETXY     IS GETXY
DEFER GETCOLROW ' _GETCOLROW IS GETCOLROW
DEFER MS        ' DROP       IS MS

\ ------------------------- Windows Error functions ------------------------
\

align HERE: K32GLE                          \ loaded in init-k32
-1 PROC GetLastError
6 PROC FormatMessage

variable WinErrMsg 0 WinErrMsg !-T                \ win error flag

: GetLastWinErr ( -- n )                    \ build string for error message debugging
               call GetLastError >R         \ save error number
               0 MAXCOUNTED 1-              \ buff len
               temp$ CHAR+ rel>abs          \ buffer
               0 R@ 0                       \ langid error null
               FORMAT_MESSAGE_FROM_SYSTEM FORMAT_MESSAGE_MAX_WIDTH_MASK OR
               call FormatMessage temp$ C!  \ save length in buffer
               WinErrMsg @
                 if temp$ count type r> throw then
               R> ;                         \ return error code

\ ------------------------------- UPPER/lower functions ------------------------
\
\ Windows function calls

WINLIBRARY USER32.DLL
1 PROC CharUpper
2 PROC CharUpperBuff
2 PROC CharLowerBuff

: UPC           ( char -- char )        \ convert char to uppercase
                call CharUpper ;

: UPPER         ( addr len -- )         \ convert string addr,len to uppercase
                SWAP REL>ABS call CharUpperBuff DROP ;

: LOWER         ( addr len -- )         \ convert string addr,len to lowercase
                SWAP REL>ABS call CharLowerBuff DROP ;

: UPPERCASE     ( str -- str )
                dup COUNT upper ;

NCODE ?UPPERCASE ( str -- str )
                mov     cl, 0 [ebx] [edi]
                cmp     cl, # 3                 \ length 3
                jnz     short @@4
                mov     cl, 1 [ebx] [edi]
                cmp     cl, # CHAR '            \ first char is a tick
                jnz     short @@4
                mov     cl, 3 [ebx] [edi]
                cmp     cl, # CHAR '            \ third char is a tick
                jz      short @@3
@@4:            test    dword ptr CAPS [edi], # -1
                jz      short @@3
                mov     cl, 0 [ebx] [edi]
                and     ecx, # 0xFF
                jz      short @@3
                lea     eax, ' UPPERCASE        \ get address of UPPERCASE
                exec                            \ go execute it
@@3:            next    c;

\ -------------------- Number Input --------------------

CODE DIGIT      ( char base -- n flag )
                mov     eax, 0 [esp]
                sub     al, # 48
                jb      short @@1
                cmp     al, # 9
                jbe     short @@2
                sub     al, # 7
                cmp     al, # 10
                jb      short @@1
@@2:            cmp     al, bl
                jae     short @@1
                mov     0 [esp], eax
                mov     ebx, # -1
                jmp     short @@3
@@1:            sub     ebx, ebx
@@3:            next    c;


CODE >NUMBER    ( ud addr len -- ud addr len )
                xchg    esp, ebp                \ 0 CELLS [ebp] = addr
                push    esi                     \ 1 CELLS [ebp] = high
                push    edi                     \ 2 CELLS [esp] = low
                push    edx                     \ save UP
                mov     ecx, ebx                \ ecx = length
                jecxz   short @@4
                mov     esi, 0 [ebp]            \ esi = address
                add     esi, edi                \ absolute address
                mov     ebx, BASE [edx]         \ get the number base
@@1:            xor     eax, eax
                mov     al, 0 [esi]             \ get next digit
                cmp     al, # '0'
                jb      short @@3               \ if below '0' branch to done
                cmp     al, # '9'
                jbe     short @@2               \ go convert it
                and     al, # 0xDF              \ convert to uppercase
                cmp     al, # 'A'               \ if below 'A'
                jb      short @@3               \ then branch to done
                sub     al, # 7
@@2:            sub     al, # '0'
                cmp     al, bl
                jae     short @@3
                xchg    eax, 1 CELLS [ebp]      \ high word * base
                mul     ebx
                xchg    eax, 2 CELLS [ebp]      \ low word * base
                mul     ebx
                add     eax, 1 CELLS [ebp]      \ add
                adc     edx, 2 CELLS [ebp]
                mov     2 CELLS [ebp], eax      \ store result
                mov     1 CELLS [ebp], edx
                inc     esi
                dec     ecx
                jnz     short @@1
@@3:            sub     esi, edi                \ relative address
                mov     0 CELLS [ebp], esi      \ address of unconvertable digit
                mov     ebx, ecx                \ length remaining
@@4:            pop     edx                     \ restore UP
                pop     edi
                pop     esi
                xchg    esp, ebp
                next    c;

0 VALUE DOUBLE?

-1 VALUE DP-LOCATION

\ a version of NUMBER? that detects a decimal point.

: NUMBER?       ( addr len -- d1 f1 )
                FALSE TO DOUBLE?                \ initially not a double #
                -1 TO DP-LOCATION
                OVER C@ [CHAR] - =
                OVER 0> AND DUP>R
                IF      1 /STRING
                THEN
                DUP 0=
                IF      R>DROP
                        2DROP 0 0 FALSE _EXIT   \ always return zero on failure
                THEN
                0 0 2SWAP >NUMBER
                OVER C@ [CHAR] . =              \ next char is a '.'
                OVER 0> AND                     \ more chars to look at
                IF      DUP 1- TO DP-LOCATION
                        1 /STRING >NUMBER
                        DUP 0=
                        IF      TRUE TO DOUBLE? \ mark as a double number
                        THEN
                THEN    NIP 0=
                R>
                IF      >R DNEGATE R>
                THEN    ;

: ?MISSING      ( f -- )
                ABORT" is undefined" ;

|: (NUMBER)      ( str -- d )
                COUNT FIND-BUFFER PLACE
                FIND-BUFFER ?UPPERCASE COUNT NUMBER? 0= ?MISSING ;

DEFER NUMBER   ' (NUMBER) IS NUMBER

\ -------------------- Number Output --------------------

: SPACE         ( -- )
                BL EMIT ;

256 CONSTANT SPCS-MAX  ( optimization for SPACES )

\ CREATE SPCS         SPCS-MAX ALLOT
\        SPCS THERE   SPCS-MAX BLANK

0 VALUE SPCS

: SPACES        ( n -- )
                BEGIN   DUP 0>
                WHILE   DUP SPCS-MAX MIN
                        SPCS OVER TYPE
                        -
                REPEAT  DROP ;

: _COL          ( n -- )
                _GETCOLROW DROP 1- MIN _GETXY DROP - SPACES ;

DEFER COL       ' _COL       IS COL

: #TAB          ( n1 -- )
                GETXY DROP OVER / 1+ * COL ;

: .ID           ( nfa -- )
                NFA-COUNT TYPE SPACE ;

: DECIMAL       10 BASE ! ;
: HEX           16 BASE ! ;
: BINARY         2 BASE ! ;
: OCTAL          8 BASE ! ;

CODE HOLD       ( char -- )
                mov     eax, HLD [UP]
                dec     eax
                mov     0 [eax] [edi], bl
                mov     HLD [UP]  , eax
                pop     ebx
                next    c;

: <#            ( -- )
                PAD HLD ! ;

: #>            ( d1 -- addr len )
                2DROP  HLD @ PAD OVER - ;

: SIGN          ( f1 -- )
                0< IF  [CHAR] - HOLD  THEN ;

CODE #          ( d1 -- d2 )
                push    edx                     \ save UP
                mov     ecx, BASE [UP]
                sub     edx, edx
                mov     eax, ebx
                div     ecx
                mov     ebx, eax
                mov     eax, 4 [esp]
                div     ecx
                mov     4 [esp], eax
                mov     eax, edx
                pop     edx                     \ restore UP
                cmp     al, # 9
                jbe     short @@1
                add     al, # 7
@@1:            add     al, # CHAR 0
                mov     ecx, HLD [UP]
                dec     ecx
                mov     0 [ecx] [edi], al
                mov     HLD [UP]  , ecx
                next    c;

: #S            ( d1 -- d2 )            BEGIN  #  2DUP OR 0= UNTIL ;
: (D.)          ( d -- addr len )       TUCK DABS  <# #S ROT SIGN #> ;
: D.            ( d -- )                (D.) TYPE SPACE ;
: D.R           ( d w -- )              >R (D.) R> OVER - SPACES TYPE ;
: .             ( n -- )                S>D  D. ;
: .R            ( n w -- )              >R  S>D  R>  D.R ;
: U.            ( u -- )                0 D. ;
: U.R           ( u w -- )              0 SWAP D.R ;
: H.            ( u -- )                BASE @ SWAP  HEX U.  BASE ! ;
: ?             ( addr -- )             @ . ;

\ -------------------- Parse Input Stream --------------------

: SOURCE        ( -- addr len )   (SOURCE) 2@ ;

CODE WORD       ( char -- addr )
                push    esi
                push    edi
                mov     al, bl                  \ al = delimiter
                mov     ebx, edi                \ ebx is now our base pointer
                mov     edi, S_ADR [ebx]        \ edi = input pointer
                add     edi, >IN [ebx]          \ add >in
                add     edi, ebx                \ make absolute address
                mov     ecx, S_LEN [ebx]        \ ecx = input length
                sub     ecx, >IN [ebx]          \ subtract >in
                ja      short @@9
                sub     ecx, ecx                \ at end of input
                jmp     short @@8
@@9:            cmp     al, # 32
                jne     short @@5
        \ Delimiter is a blank, treat all chars <= 32 as the delimiter
@@1:            cmp     0 [edi], al             \ leading delimiter?
                ja      short @@2
                inc     edi                     \ go to next character
                dec     ecx
                jnz     short @@1
                mov     esi, edi                \ esi = start of word
                mov     ecx, edi                \ ecx = end of word
                jmp     short @@7
@@2:            mov     esi, edi                \ esi = start of word
@@3:            cmp     0 [edi], al             \ end of word?
                jbe     short @@4
                inc     edi
                dec     ecx
                jnz     short @@3
                mov     ecx, edi                \ ecx = end of word
                jmp     short @@7
@@4:            mov     ecx, edi                \ ecx = end of word
                inc     edi                     \ skip over ending delimiter
                jmp     short @@7
        \ delimiter is not a blank
@@5:            repz    scasb
                jne     short @@6
                mov     esi, edi                \ end of input
                mov     ecx, edi
                jmp     short @@7
@@6:            dec     edi                     \ backup
                inc     ecx
                mov     esi, edi                \ esi = start of word
                repnz   scasb
                mov     ecx, edi                \ ecx = end of word
                jne     short @@7
                dec     ecx                     \ account for ending delimiter
        \ Update >IN pointer and get word length
@@7:            sub     edi, ebx                \ relative address
                sub     edi, S_ADR [ebx]        \ offset from start
                mov     >IN [ebx], edi          \ update >IN
                sub     ecx, esi                \ length of word
                cmp     ecx, # MAXCOUNTED       \ max at MAXCOUNTED
                jbe     short @@8
                mov     ecx, # MAXCOUNTED       \ clip to MAXCOUNTED
        \ Move string to pocket
@@8:            mov     edi, 0 [esp]            \ old value of edi
                add     edi, ' POCKET >BODY [ebx] \ edi = pocket
                mov     0 [edi], cl             \ store count byte
                inc     edi
                rep     movsb                   \ move rest of word
                xor     eax, eax                \ clear EAX
                stosb                           \ append a NULL to pocket
                pop     edi
                pop     esi
                mov     ebx, ' pocket >BODY [edi] \ return pocket
                next    c;

: PARSE         ( char -- addr len )
                >R SOURCE >IN @ /STRING
                2DUP R> SCAN NIP -
                DUP 1+ >IN +! ;

: .(            ( -- )
                [CHAR] ) PARSE TYPE ; IMMEDIATE
                
\ -------------------------------------------------------
\ Header structure as of June 1996
\
\ \      [ flag field       ] -8           FFA          \ arm temp removed
\       [ optimize field   ] -4           OFA
\       [ alignment bytes  ] 0 to 3 bytes for header alignment
\       [ the name letters ]
\       [ count byte       ] -9  -5       NFA
\       [ view field       ] -8  -4       VFA
\       [ link field       ] -4  +0       LFA
\       [ cfa ptr field    ] +0           CFA-PTR
\               |
\               v
\       [ cfa  field       ] +0           CFA
\       [ body field       ] +4           PFA
\ -------------------------------------------------------


\ -------------------------------------------------------
\ Vocabulary dictionary structure
\
\       [ cfa field        ] +0           VCFA = vocabulary cfa
\       [ voc link         ] +4           VLINK
\       [ num voc threads  ] +8           #THREADS
\       [ voc thread 0     ] +12          VOC thread 0 = voc-address
\       [ voc thread 1     ] +16          VOC thread 1
\       [ voc thread 2     ] +20          VOC thread 2
\       [ voc thread ...   ] +n*4+12      VOC thread n
\ -------------------------------------------------------

CODE >BODY      ( cfa -- pfa )
                add     ebx, # 4        \ cell+
                next    c;

CODE BODY>      ( pfa -- cfa )
                sub     ebx, # 4        \ cell-
                next    c;

CODE L>NAME     ( lfa -- nfa )
                sub     ebx, # 5        \ 1- cell-
                next    c;

CODE LINK>      ( link -- cfa )
                add     ebx, # 4        \ cell+ @
                mov     ebx, 0 [ebx] [edi]
                next    c;

CODE N>LINK     ( nfa -- lfa )
                add     ebx, # 5        \ cell+ 1+
                next    c;

CODE VLINK>VOC  ( voc-link-field -- voc-address )
                add     ebx, # 4        \ cell+
                next    c;

CODE VOC>VLINK  ( voc-address -- voc-link-field )
                sub     ebx, # 4        \ cell -
                next    c;

CODE VOC#THREADS ( voc-address -- #threads )
                sub     ebx, # 8        \ 2 cells - @
                mov     ebx, 0 [ebx] [edi]
                next    c;

CODE VCFA>VOC   ( vocabulary-cfa -- voc-address )
                add     ebx, # 12       \ >BODY 2 CELLS+
                next    c;

CODE VOC>VCFA   ( voc-address -- vocabulary-cfa )
                sub     ebx, # 12       \ 2 CELLS - BODY>
                next    c;

VARIABLE [UNKNOWN]

\ search thread for cfa contents of [UNKNOWN], return its nfa if found

CODE ?THREAD    ( FALSE thread -- FALSE FALSE | nfa TRUE TRUE )
@@2:            mov     ebx, 0 [ebx] [edi]        \ get contents of thread
                test    ebx, ebx                \ if top of stack = zero
                je      short @@1               \ branch to exit
                mov     eax, 4 [ebx] [edi]      \ get the CFA
                sub     eax, [unknown] [edi]    \ does CFA match?
                test    eax, eax
                jne     short @@2               \ if they match
                sub     ebx, # 5
                pop     eax
                push    ebx
                mov     ebx, # TRUE
                push    ebx
                jmp     short @@3
@@1:            mov     ebx, # FALSE            \ extra FALSE on stack
@@3:            next    c;

: _>NAME        ( cfa -- nfa )  \ find the nfa of the provided cfa.
                                \ returns the nfa of [UNKNOWN] if the nfa of
                                \ cfa can't be found
                BEGIN   [UNKNOWN] !
                        VOC-LINK
                        BEGIN   @ ?DUP
                        WHILE   FALSE
                                OVER VLINK>VOC DUP VOC#THREADS CELLS BOUNDS
                                ?DO     I ?THREAD ?LEAVE CELL +LOOP
                                IF      NIP
                                        _EXIT           \ LEAVE WE FOUND IT
                                THEN
                        REPEAT  ['] [UNKNOWN]
                AGAIN   ;

DEFER >NAME     ' _>NAME IS >NAME

CODE NAME>      ( nfa -- cfa )
                mov     ebx, 9 [ebx] [edi]      \ 9 + @
                next    c;

CODE N>CFAPTR   ( nfa -- cfa-pointer )
                add     ebx, # 9                \ 9 +
                next    c;

: >VIEW         ( cfa -- vfa )
                >NAME 1+ ;

CODE VIEW>      ( vfa -- cfa )
                mov     ebx, 8 [ebx] [edi]      \ 8 + @
                next    c;

CODE NFA-COUNT  ( nfa -- adr len )              \ name count
                movzx   eax, byte ptr 0 [ebx] [edi] \ get count from nfa
                and     eax, # 63               \ and out to len
                sub     ebx, eax                \ adjust the address
                push    ebx                     \ on stack
                mov     ebx, eax                \ addr len
                next    c;

: N>OFA         ( nfa -- ofa )
                DUP
                IF      NFA-COUNT DROP -ALIGNED CELL-
                THEN    ;

: >OFA          ( cfa -- ofa )                  \ get the Optimization Field Address
                >NAME N>OFA ;
                
: N>FFA         ( nfa -- ffa )
                DUP
                IF      NFA-COUNT DROP -ALIGNED CELL-        \ !!
                THEN    ;

: >FFA          ( cfa -- ffa )                  \ get the Flag Field Address
                >NAME N>FFA ;
                
\ -------------------- Dictionary Search --------------------

VARIABLE LATEST-NFA

CODE SEARCH-1WORDLIST    ( addr len wid -- 0 | cfa flag )
                pop     eax                     \ eax = length
                xchg    ebp, esp                \ [ebp] = addr
                push    esi
                push    edi
                push    edx                     \ save UP
                test    eax, eax                \ zero length does not match
                je      short @@5
                mov     edx, edi                \ edx = new base pointer
                add     0 [ebp], edx            \ make string address absolute
                jmp     short @@2
@@1:            mov     cl, -5 [ebx] [edx]      \ count byte
                and     ecx, # 63               \ 05/13/94 tjz max length now 63
                cmp     ecx, eax                \ counts equal?
                je      short @@3
@@2:            mov     ebx, 0 [ebx] [edx]      \ follow link
                test    ebx, ebx
                jne     short @@1               \ end of chain?
@@5:            add     ebp, # 4                \ discard string address
                sub     ebx, ebx                \ return not found
                jmp     short @@4
@@3:            mov     esi, 0 [ebp]              \ esi = string address
                lea     edi, -5 [ebx] [edx]     \ edi = name field
                mov     LATEST-NFA [edx], edi   \ save nfa for other use
                sub     LATEST-NFA [edx], edx   \ convert from abs to rel
                sub     edi, ecx                \ edi = first char of name
                repz    cmpsb
                jne     short @@2
                mov     eax, 4 [ebx] [edx]      \ cfa
                mov     0 [ebp], eax            \ this is the cfa
                mov     al, -5 [ebx] [edx]      \ get count byte
                mov     ebx, # 1                \ assume word is immediate
                and     al, # 128               \ test immediate bit
                jne     short @@4
                neg     ebx                     \ not immediate
@@4:            pop     edx                     \ restore UP & exit
                pop     edi
                pop     esi
                xchg    ebp, esp
                next    c;

CODE "#HASH     ( a1 n1 #threads -- n2 )
                pop     eax                     \ pop count into EAX
                mov     ecx, ebx                \ save #threads in ecx
                pop     ebx                     \ get string address into EBX
                push    edx                     \ save UP
                xor     edx, edx                \ clear EDX
                cmp     eax, # 0                \ if count is zero
                je      short @@2                     \ then exit
                push    ecx                     \ save #threads for later
                mov     ecx, eax                \ copy count into ECX
@@1:            rol     eax, # 7                \ rotate result some
                mov     dl, 0 [ebx] [edi]         \ get first character of name
                add     eax, edx                \ accumulate another character
                inc     ebx                     \ bump to address of next    c; character
                dec     ecx
                jnz     short @@1
                xor     edx, edx                \ clear high part of dividend
                pop     ebx                     \ get number of threads
                div     ebx                     \ perform modulus
@@2:            mov     ebx, edx                \ move result into EBX
                lea     ebx, 0 [ebx*4]            \ multiply by cell size
                pop     edx                     \ restore UP
                next    c;

: _SEARCH-WORDLIST ( addr len voc -- 0 | cfa flag )
                >R 2DUP R@ VOC#THREADS "#HASH R> + SEARCH-1WORDLIST ;

63 VALUE NAME-MAX-CHARS                 \ function names can be this long

: SEARCH-WORDLIST ( addr len voc -- 0 | cfa flag )
                >R FIND-BUFFER PLACE
                FIND-BUFFER ?UPPERCASE COUNT
                2DUP R@ VOC#THREADS "#HASH R> + SEARCH-1WORDLIST ;

\ WARNING: (FIND) is a case sensitive find.  If you need to be able to find
\ words in the dictionary that have not already been passed through ?UPPERCASE,
\ then you should use CAPS-FIND which will uppercase the string before trying
\ to find it in the dictionary.

: (FIND)        ( str -- str FALSE | cfa flag )
                DUP C@ 0= IF 0 _EXIT THEN
                CONTEXT
                BEGIN   DUP @                   \ while not at end of list
                WHILE   DUP 2@ <>               \ and not the same vocabulary
                                                \ as NEXT time
                        IF      OVER COUNT NAME-MAX-CHARS MIN
                                2 PICK @ _SEARCH-WORDLIST ?DUP
                                IF      2SWAP 2DROP     \ found it, so
                                        _EXIT           \ we're done searching
                                THEN
                        THEN    CELL+                   \ step to next vocabulary
                REPEAT  DROP
                FALSE  ;

: CAPS-FIND     ( str -- str FALSE | cfa flag )
                DUP COUNT FIND-BUFFER PLACE
                FIND-BUFFER ?UPPERCASE (FIND)
                DUP>R
                IF      NIP
                ELSE    DROP
                THEN     R> ;

DEFER FIND      ( str -- str 0 | cfa flag )

: DEFINED       ( -- str 0 | cfa flag )
                BL WORD PARMFIND ;

\ -------------------- Compiler --------------------


VARIABLE DP                             \ application dictionary pointer
VARIABLE SDP                            \ system dictionary pointer
VARIABLE CDP                            \ code pointer (unused)
\ VARIABLE DDP                            \ data pointer (unused)

CODE CODE-HERE  ( -- a1 )       \ next free byte of code space
                push    ebx
                mov     ebx, CDP [edi]           \  CDP @
                next    c;

CODE CODE-C,    ( c1 -- )       \ store byte in code space
                mov     eax, CDP [edi]           \ get cdp
                mov     0 [eax] [edi], bl        \ store the byte
                inc     eax                      \ bump pointer
                mov     CDP [edi], eax           \ bump cdp
                pop     ebx
                next    c;

CODE CODE-W,    ( w1 -- )       \ store word in code space
                mov     eax, CDP [edi]           \ get cdp
                mov     0 [eax] [edi], bx        \ store word
                inc     eax
                inc     eax
                mov     CDP [edi], eax           \ bump cdp
                pop     ebx
                next    c;

CODE CODE-,     ( n1 -- )       \ store dword in code space
                mov     eax, CDP [edi]           \ get cdp
                mov     0 [eax] [edi], ebx       \ store dword
                add     eax, # 4                 \ bump cdp
                mov     CDP [edi], eax           \ bump cdp
                pop     ebx
                next    c;
                
CODE CODE-ALLOT ( n -- )        \ allocate code space
                add     CDP [edi], ebx
                pop     ebx
                next    c;

CODE CODE-ALIGN ( --  )         \ align code space, does nothing as code not reqd aligned
                next    c; IMMEDIATE

CODE APP-HERE   ( -- a1 ) \ return a1 the address of the next free byte of
                          \ application dictionary space
                push    ebx
                mov     ebx, DP [edi]
                next    c;

DEFER HERE      ' APP-HERE IS HERE

CODE SYS-HERE   ( -- a1 ) \ return a1 the address of the next free byte of
                          \ system dictionary space
                push    ebx
                mov     ebx, SDP [edi]
                next    c;

CODE APP-ALLOT  ( n1 -- ) \ allot n1 bytes in the application dictionary
                add     DP [edi], ebx
                pop     ebx
                next    c;

DEFER VEC-ALLOT ' APP-ALLOT IS VEC-ALLOT

CODE SYS-ALLOT  ( n1 -- ) \ allot n1 bytes in the system dictionary
                add     SDP [edi], ebx
                pop     ebx
                next    c;

: SYS-ORIGIN    ( -- a1 ) \ return the absolute origin of system dictionary
                &ORIGIN @ &SYS-SEPARATION @ + ;

: ?MEMCHK       ( n1 -- ) \ test to see if we have enough memory
                0MAX DUP &ORIGIN &APP-SIZE @ + U>
                SWAP 512 + APP-HERE + &ORIGIN &APP-SIZE @ + U> OR
                ABORT" OUT OF APPLICATION MEMORY!"
                &SYS-SIZE @ SYS-HERE SYS-ORIGIN - - 512 <
                ABORT" Out of System/Header memory!" ;

: ALLOT         ( n1 -- ) \ define ALLOT with a memory full check
                DUP 1000 + ?MEMCHK VEC-ALLOT ;

CODE _APP-COMPILE, ( xt -- )
                mov     eax, DP [edi]
                mov     0 [eax] [edi], ebx
                add     eax, # 4
                mov     DP [edi], eax
                pop     ebx
                next    c;

TRUE VALUE SYS-WARNING?

: ?SYS-ADDRESS  ( a1 -- ) \ check that a1 is not in system dictionary,
                          \ warn user if it is.
                DUP APP-HERE U>                 \ in system space
                SYS-WARNING? AND                \ and we want warnings
                IF      BASE @ >R DECIMAL
                        CR ." ****System word: "
                        DUP SYS-HERE U<
                        IF      DUP .NAME
                        ELSE    DUP H.
                        THEN    ." used in: "   LAST @ NFA-COUNT TYPE
                        cr ."  at file: " LOADFILE @ CELL+ COUNT TYPE
                        ."  line: " LOADLINE @ .
                        R> BASE !
                THEN    DROP ;

: APP-COMPILE,  ( xt -- )
                DUP ?SYS-ADDRESS _APP-COMPILE, ;

DEFER COMPILE,  ' APP-COMPILE, IS COMPILE,

: SYS-WARNING-OFF ( -- ) \ disable warning for use of system words in application
                FALSE TO SYS-WARNING? ;

: SYS-WARNING-ON  ( -- ) \ enable warning for use of system words in application
                TRUE  TO SYS-WARNING? ;

CODE SYS-COMPILE, ( xt -- )
                mov     eax, SDP [edi]
                mov     0 [eax] [edi], ebx
                add     eax, # 4
                mov     SDP [edi], eax
                pop     ebx
                next    c;

CODE APP-,      ( n1 -- )
                mov     eax, DP [edi]
                mov     0 [eax] [edi], ebx
                add     eax, # 4
                mov     DP [edi], eax
                pop     ebx
                next    c;

DEFER ,         ' APP-, IS ,

CODE SYS-,      ( n1 -- )
                mov     eax, SDP [edi]
                mov     0 [eax] [edi], ebx
                add     eax, # 4
                mov     SDP [edi], eax
                pop     ebx
                next    c;

CODE APP-C,     ( c1 -- )
                mov     eax, DP [edi]
                mov     0 [eax] [edi], bl
                inc     eax
                mov     DP [edi], eax
                pop     ebx
                next    c;

DEFER C,        ' APP-C, IS C,

CODE SYS-C,     ( c1 -- )
                mov     eax, SDP [edi]
                mov     0 [eax] [edi], bl
                inc     eax
                mov     SDP [edi], eax
                pop     ebx
                next    c;

CODE APP-W,     ( w1 -- )
                mov     eax, DP [edi]
                mov     0 [eax] [edi], bx
                add     eax, # 2
                mov     DP [edi], eax
                pop     ebx
                next    c;

DEFER W,        ' APP-W, IS W,

CODE SYS-W,     ( w1 -- )
                mov     eax, SDP [edi]
                mov     0 [eax] [edi], bx
                add     eax, # 2
                mov     SDP [edi], eax
                pop     ebx
                next    c;

CODE APP-COMPILE  ( -- )
                lodsd
                mov     ecx, DP [edi]
                mov     0 [ecx] [edi], eax
                add     ecx, # 4
                mov     DP [edi], ecx
                next    c;

DEFER COMPILE   ' APP-COMPILE IS COMPILE

CODE SYS-COMPILE ( -- )
                lodsd
                mov     ecx, SDP [edi]
                mov     0 [ecx] [edi], eax
                add     ecx, # 4
                mov     SDP [edi], ecx
                next    c;

CODE APP-ALIGN  ( -- )
                mov     eax, DP [edi]
                jmp     short @@2
@@1:            mov     cl, # 0
                mov     0 [eax] [edi], cl
                inc     eax
@@2:            mov     ecx, eax
                and     ecx, # 3
                jne     short @@1
                mov     DP [edi], eax
                next    c;

DEFER ALIGN     ' APP-ALIGN IS ALIGN

CODE SYS-ALIGN  ( -- )
                mov     eax, SDP [edi]
                jmp     short @@2
@@1:            mov     cl, # 0
                mov     0 [eax] [edi], cl
                inc     eax
@@2:            mov     ecx, eax
                and     ecx, # 3
                jne     short @@1
                mov     SDP [edi], eax
                next    c;

CODE ALIGNED    ( addr1 -- addr2 )
                add     ebx, # 3
                and     ebx, # -4
                next    c;

CODE -ALIGNED   ( addr1 -- addr2 )
                and     ebx, # -4
                next    c;
                
CODE NALIGNED   ( addr n -- addr2 )
                mov    eax, ebx         \ n
                dec    eax              \ n-1
                neg    ebx              \ -n
                pop    ecx              \ addr
                add    eax, ecx         \ addr+n-1
                and    ebx, eax         \ addr+n-1 and -n
                next   c;

FALSE VALUE IN-SYSTEM?

: IN-APPLICATION ( -- )
                FALSE TO IN-SYSTEM?
                ['] APP-HERE     IS HERE
                ['] APP-ALLOT    IS VEC-ALLOT
                ['] APP-COMPILE, IS COMPILE,
                ['] APP-,        IS ,
                ['] APP-C,       IS C,
                ['] APP-W,       IS W,
                ['] APP-COMPILE  IS COMPILE
                ['] APP-ALIGN    IS ALIGN ;

: IN-SYSTEM     ( -- )
                TRUE TO IN-SYSTEM?
                ['] SYS-HERE     IS HERE
                ['] SYS-ALLOT    IS VEC-ALLOT
                ['] SYS-COMPILE, IS COMPILE,
                ['] SYS-,        IS ,
                ['] SYS-C,       IS C,
                ['] SYS-W,       IS W,
                ['] SYS-COMPILE  IS COMPILE
                ['] SYS-ALIGN    IS ALIGN ;

: >SYSTEM       ( -- )          \ select system dictionary, save prev dict
                R> IN-SYSTEM? >R >R IN-SYSTEM ;

: SYSTEM>       ( -- )  \ return to previous dictionary
                R> R> 0=
                IF      IN-APPLICATION
                THEN    >R ;

: >APPLICATION  ( -- )  \ select system dictionary, save prev dict
                R> IN-SYSTEM? >R >R IN-APPLICATION ;

: APPLICATION>  ( -- )  \ return to previous dictionary
                R> R>
                IF      IN-SYSTEM
                THEN    >R ;

: IMMEDIATE     ( -- )  \ mark the last header create as an immediate word
                LAST @ 128 TOGGLE ;

: HIDE          ( -- )
                LAST @ N>LINK @ LAST-LINK @ ! ;

: REVEAL        ( -- )
                LAST @ N>LINK LAST-LINK @ ! ;

: LITERAL       ( n -- )
                COMPILE LIT , ; IMMEDIATE

: CHAR          ( -- char )
                BL WORD 1+ C@ ;

: [CHAR]        ( -- char )
                CHAR  [COMPILE] LITERAL ; IMMEDIATE

: '             ( -- cfa )
                DEFINED 0= ?MISSING ;

: [']           ( -<name>- )
                ' [COMPILE] LITERAL ; IMMEDIATE

: [COMPILE]     ( -<name>- )
                ' COMPILE, ; IMMEDIATE

: POSTPONE      ( -<name>- )
                DEFINED DUP 0= ?MISSING
                0< IF  COMPILE COMPILE  THEN
                COMPILE, ; IMMEDIATE

\ -------------------- Link Operations    --------------------

\ usage: [parms] ' x link do-link
\ follows link, for each link executes x
\ x must have stack ( [parms ...] link -- [parms ...] )
\ safe to use even if x destroys next link
\ and can be used recursively

: DO-LINK       ( [parms ...] cfa-to-do link-address -- [parms] )
                swap >r @ >r                      \ save cfa, next link address
                begin r>  ?dup                    \ check the address
                while dup @ r@ swap >r            \ get link, get cfa, & save next pointer
                      execute                     \ execute the cfa
                repeat r>drop ;                   \ drop saved cfa

CODE ADD-LINK   ( addr link -- )                  \ add a link into a chain
                pop    eax                        \ fetch addr
                mov    ecx , 0 [ebx] [edi]        \ fetch address pointed to by link
                mov    0 [eax] [edi] , ecx        \ point addr at pointed to
                mov    0 [ebx] [edi] , eax        \ point link at addr
                pop    ebx                        \ clear stack
                next   c;

\ ---------------------- Pointers for procs & libs ----------------------

align
HERE: K32DLL
WinLibrary KERNEL32.DLL
2 K32DLL 2 CELLS+ C!-T                      \ make it a KERN library

variable winlib-link                        \ linkage for libraries
         K32DLL winlib-link !-t
variable winlib-last                        \ last library found/defined
         K32DLL winlib-last !-t

variable winproc-link                \ linkage for procedures
variable winproc-last                \ last proc found/defined

0 value winarea-size            \ size of area allocated
variable winarea-ptr            \ pointer to area for undeclared proc builds
         0 winarea-ptr !-T      \ none allocated
0 value winarea-len             \ undeclared calls will fail, set in winlib.f

0 value ignore-missing-procs?   \ used to ignore entry point missing, default is load now

\ -------------------- Required PROCs -------------------------
align HERE: K32LLI
1 proc LoadLibrary
align HERE: K32GPA
1 proc GetProcAddress
1 proc FreeLibrary

\ -------------------- Resolving Procedures --------------------

: _proc-error ( addr -- )
         abort" procedure not found" ;

defer proc-error ' _proc-error is proc-error

\ in assembler, as used at run time by wined (for some obtruse reason...)

CODE "find-proc  ( addr len -- proccfa -1 | 0 ) \ find windows proc by name **WINED**
                pop     eax                     \ eax is addr, ebx is len
                add     eax, edi                \ rel>abs
                push    esi                     \ save esi
                push    edx                     \ save edx
                lea     edx, ' winproc-link >body \ get ptr to first proc
@@1:            mov     edx, 0 [edx] [edi]      \ get the proc ptr
                or      edx, edx                \ check if zero
                jz      short @@9               \ yes, so not found exit
                movzx   ecx, byte ptr 0 proc>name [edx] [edi] \ get the count of the name in ecx
                cmp     ebx, ecx                \ compare lengths
                jne     short @@1               \ no, so next proc
                push    edi                     \ save edi
                lea     esi, 0 proc>name char+ [edx] [edi] \ point esi at name string
                mov     edi, eax                \ edi is address
                rep     cmpsb                   \ check if equal
                pop     edi                     \ restore regs
                jne     short @@1               \ no, so next
                add     edx, # 0 proc>cfa       \ point at cfa
                mov     ebx, # -1               \ exit code
                pop     ecx                     \ pop old edx
                xchg    edx, ecx                \ restore edx
                pop     esi
                push    ecx                     \ proccfa
                next                            \ out
@@9:            xor     ebx, ebx                \ zero
                pop     edx                     \ old edx
                pop     esi
                next    c;
        
|: res-loadproc ( procname lib -- proc-ep | 0 )   \ helper to get proc address
               MAXCOUNTED _LOCALALLOC >R
               swap                              ( lib procname )
               dup>r count drop rel>abs over     \ name, lib handle
               call GetProcAddress
               dup 0=                            \ if not ok
               if drop r>
                 count r@ place                  \ copy to temp buffer
                 [char] A
                 r@ count + w!                   \ add an "A<null>" after last char
                 r> char+ rel>abs over
                 call GetProcAddress
               else
                 r>drop
               then
               nip _LOCALFREE ;


: res-loadcall  ( proc-cfa -- )                 \ resolve proc address
                cfa>proc                        \ point at link, not cfa
                ['] proc-error over proc>lib !  \ in case we recurse through here
                winlib-link
                begin @ ?dup                    \ loop through libraries
                while
                     over proc>name             \ get proc, point at name
                     over load-winlibrary       \ attempt load of library
                     if                         \ did it load?
                       over lib>handle @        \ get lib handle
                       res-loadproc             \ now load the proc
                       dup 0=                   \ found?
                       if drop                  \ no, drop and next library round
                       else >r                  \ save entry point ( proc lib )
                            over proc>lib !     \ save lib ptr in proc
                            r> swap proc>ep !   \ save the ep in the proc address
                            _EXIT               \ and exit
                       then
                     else
                       drop                     \ drop unused procname
                     then
                repeat
                proc-error                      \ it's an error, couldn't load the proc
                ;

\ -------------------- Defining Procedures --------------------

|: proc-inarea ( a1 n1 n2 -- )                 \ define a procedure from string a1,n1 at area
        >r                                    \ save n2 on rstack
        winarea-len 48 - 0< abort" - Insufficient space for PROC"  
        winarea-ptr @ aligned                 \ align pointer
        dup winproc-last !                    \ point as last created
        dup winproc-link add-link             \ linked list of all procs
        dup 0winproc                          \ build default entry
        dup proc>pcnt r> swap c!              \ save n2
        proc>name 3dup place                  \ move to name
        + 1+ dup 0 swap c!                    \ end of name add x'00'
        1+ dup>r                              \ point at next, save
        winproc-last @ swap - +to winarea-len \ update length
        r> winarea-ptr !                      \ update the pointer
        drop
        ;

|: proc-inline ( a1 n1 n2 -- )                 \ define a procedure from string a1,n1 at here
        align here                            \ get aligned pointer, allot to parm count
        winproc-link link,                    \ link address
        0winproc 12 allot                     \ build proc at addr
        c,                                    \ # of parms ( n2 )
        here over 1+ allot place              \ move in name
        0 c,                                  \ terminate with null
        align                                 \ and align
        winproc-link @ winproc-last !         \ last created
        ;

: "#proc ( a1 n1 n2 -- )                        \ define a procedure from string a1,n1
        >r 2dup "find-proc                      \ find procedure
        if
           nip nip r>drop                       \ ok, then exit
           cfa>proc winproc-last !              \ save in last (find-proc returns cfa addr)
        else
          r> proc-inline                        \ build the proc here
        then
        ;

: proc          ( #params -<name>- )            \ #arguments proc MessageBeep
                >r bl word count r> "#proc
                ;


\ -------------------- Calling Procedures --------------------

: call          ( [args..] -<proc>- result )    \ compile or execute a windows procedure
                bl word count                   \ Calling...
                state @
                if                              \ compiling
                  2dup "find-proc               \ is it defined?
                  if                            \ yes, just compile
                    dup>r compile,
                    2drop                       \ drop proc name string
                  else
                    -2 proc-inarea              \ build the proc (-2 = unknown # parms)
                    winproc-last @ proc>cfa     \ point at cfa
                    dup>r compile,              \ compile it
                  then
                  r> ignore-missing-procs? if   \ should we ignore missing?
                    drop                        \ yes, just drop
                  else
                    res-loadcall                \ else resolve call now!
                  then
                else                            \ interpreting
                  -2 "#proc                     \ define/find the proc
                  winproc-last @ proc>cfa       \ point at cfa
                  execute                       \ execute it
                then
                ; IMMEDIATE

\ ****************** Library Routines ****************************

|: find-winlib   ( addr len -- lib -1 | 0 )      \ find windows library by name
        2>r                                     \ save string
        winlib-link                             \ loop through windows procs
        begin @ dup
        while   dup lib>name count
                2r@ compare 0=                  \ compare called to entry
                if  TRUE                        \ ok, then leave
                    r>drop r>drop
                    _exit
                then
        repeat
        r>drop r>drop                           \ otherwise leave with 0 at end
        ;

|: make-winLibrary ( libtype addr len -- )       \ make/find a library entry
        2dup
        find-winlib 0=                          \ find the library
        if
          rot                                   \ ( addr len libtype )
          align
          here winlib-last !                    \ point last at here
          winlib-link link,                     \ the link of all libraries
          0 ,                                   \ the library handlehandle
          c,                                    \ ( libtype ) mark as type of library
          here over 1+ allot place 0 c,
        else
          winlib-last !                         \ save last lib
          3drop                                 \ drop unused
        then
        ;

: load-winlibrary ( lib-entry -- f1 )           \ f1=TRUE if all is ok
        dup lib>handle @ 0=                     \ library address NULL?
        if      dup lib>name                    \ to counted library name
                count drop rel>abs
                call LoadLibrary
                dup 0=
                if  2drop FALSE                 \ discard leave null
                else swap lib>handle ! TRUE     \ store address
                then
        else    drop TRUE                       \ if not, then return true
        then
        ;

: free-winlibrary ( lib-entry -- )              \ free a lib entry
          dup lib>handle @ ?dup                 \ it's got a handle...
          if
            call FreeLibrary drop               \ free it
            lib>handle off _exit                \ zero it & leave
          then drop
        ;

\ -------------------- Library Procedures --------------------

: "WinLibrary   ( adr len -- )
        0 -rot make-winlibrary
        ;

: WinLibrary   ( 'name.DLL' -- )        \ usage: WinLibrary user32.dll
        bl word uppercase count "WinLibrary
        ;
        
|: init-proc ( -- )                          \ initialize all procedure libraries

        ['] 0winproc winproc-link do-link       \ zero the procs
                                                \ NOTE! run-time rather than compile-time
        init-k32

        ['] free-winlibrary winlib-link do-link \ zero out app libraries
        ;

\ ------------------------------- Memory Management functions ------------------------
\
\ Malloc data structure in dynamically allocated memory
\ beta 2.0A 06/09/2002 21:54:51 arm major modifications

\ [malloc_next][heapaddress][mem_type][malloced_memory][extra_cell]
\                                     |
\                                     * returns this address on allocate *
\                                      this is the "address"
\ Changes:
\   All memory calls are now to Windows heap functions
\   Length field has been discarded
\   Heap address is included
\   Currently, only the process heap is used. Only
\     ALLOCATE and REALLOC need to be modified to work against
\     another heap.
\   mem_type is currently unused, will be for pe header
\ Windows function calls

0 PROC GetProcessHeap            \ Heap functions
3 PROC HeapAlloc
3 PROC HeapFree
4 PROC HeapReAlloc
3 PROC HeapSize

0 constant malloc-hflag                          \ heap flags
variable malloc-haddr                            \ heap address
variable malloc-link                             \ head of single linked list
         0 malloc-link !-T

: link>haddr   ( addr -- addr' )                 \ from link to heap address
               [ cell ] literal + ;

|: link>memtype ( addr -- addr' )                 \ from link to memtype
               [ 2 cells ] literal + ;

: link>mem     ( addr -- addr' )                 \ from link to mem pointer
               [ 3 cells ] literal + ;

|: mem>link     ( addr' -- addr )                 \ from mem to link
               [ 3 cells ] literal - ;

: malloc-adjlen ( -- n )                         \ adjustment for headers + extra cell
               [ 4 cells ] literal ;

|: mHeapCheck   ( abs-addr | 0 -- rel-addr flag ) \ check flag and change to rel-addr
               dup abs>rel swap 0= ;

|: mHeapAlloc   ( n -- rel-addr fl )              \ allocate n bytes, return rel address
               malloc-hflag malloc-haddr @       \ flags, heapaddress
               call HeapAlloc mHeapCheck ;

|: mHeapParm    ( rel-addr -- abs-addr 0 abs-heap-addr ) \ set up parms
               dup rel>abs swap                  \ abs rel
               link>haddr @ malloc-hflag swap    \ abs 0 heap-addr
               ;

|: mHeapFree    ( rel-addr -- f )                 \ free rel-addr bytes
               mHeapParm
               call HeapFree 0= ;                \ flags, heapaddress

|: mHeapReAlloc ( n rel-addr -- rel-addr' fl )    \ realloc n rel-addr bytes
               mHeapParm
               call HeapReAlloc                  \ flags, heapaddress
               mHeapCheck ;

: mHeapSize    ( rel-addr -- n )                 \ size of rel-addr bytes
               mHeapParm
               call HeapSize ;                   \ flags, heapaddress

defer (memlock)   ' noop is (memlock)             \ memory lock and unlock, see task.f
defer (memunlock) ' noop is (memunlock)           \ used to serialise requests for tasks

|: malloc-addlink ( addr -- )                     \ link memory into list
                malloc-haddr @ over link>haddr !  \ save heap address
                (memlock)                         \ lock
                malloc-link add-link              \ link into malloc_next
                (memunlock)                       \ unlock
                ;

|: malloc-unlink ( addr -- f1 )                   \ unlink from list
                 >r malloc-link                   \ f1=FALSE=ok
                (memlock)                         \ lock
                begin dup @ ?dup                  \ prev and next
                while                             \ if it points somewhere...
                  dup r@ =                        \ is it me?
                  if
                    @ swap !                      \ yes, so unlink me
                    (memunlock) r>drop FALSE _exit
                  then
                nip                               \ drop prev
                repeat
                (memunlock)                       \ unlock
                drop r>drop TRUE ;                \ didn't find it...

: allocate      ( n -- addr fl )                \ ansi version of malloc
                malloc-adjlen + mHeapAlloc      \ modified to use Windows call
                if      drop 0 TRUE             \ error, fl=true
                else    dup malloc-addlink      \ link in
                        link>mem FALSE          \ point at real mem
                then    ;                       \ -- f1 = true on error

: malloc        ( n -- addr )                   \ allocate dynamic memory
                allocate abort" Failed to allocate memory" ;

|: (free)       ( link-addr -- f1 )             \ free memory (addr points link)
                dup malloc-unlink               \ first, delete from malloc list
                if      drop TRUE               \ if it failed, return failure
                else    mHeapFree               \ then actually release the mem
                then    ;

: free          ( addr -- f1 )                  \ release the memory pointer
                                                \ f1=TRUE=failed, f1=FALSE=ok
                mem>link (free) ;               \ point at true address

|: (release)     ( link-addr -- )                \ release block
                dup (free)
                if     ." Failed to release memory at address 0x" link>mem h. cr
                       ." Press any key to continue"
                       key drop cr
                else   drop
                then ;

: release       ( addr -- )
                mem>link (release) ;

: realloc       ( n addr -- addr' fl )
                mem>link dup malloc-unlink      \ remove from list
                if      nip TRUE                \ if not in list
                        _EXIT                   \ then fail the function
                then
                malloc-adjlen under+ mHeapReAlloc \ make longer
                if   TRUE                       \ failed is true
                else dup malloc-addlink link>mem FALSE \ add in to list
                then    ;                       \ -- f1 = true if failed

: resize        ( a1 n1 -- a2 f1 )              \ ansi version of realloc
                swap realloc ;                  \ -- f1 = true on error

: init-malloc  ( -- )
               0 malloc-link !
               call GetProcessHeap malloc-haddr ! ; \ heap address save in var

: term-malloc   ( -- )                         \ release all allocated memory
                ['] (release) malloc-link do-link
                ;

\ -------------------- ANS File Functions --------------------

7 PROC CreateFile
1 PROC CloseHandle
4 PROC ReadFile
4 PROC WriteFile
1 PROC DeleteFile
2 PROC MoveFile
4 PROC SetFilePointer
1 PROC FlushFileBuffers
2 PROC SetEndOfFile

                     : bin ;                   \ BIN
GENERIC_READ  constant r/o                     \ GENERIC_READ
GENERIC_WRITE constant w/o                     \ GENERIC_WRITE
r/o w/o +     constant r/w                     \ READ/WRITE

: ascii-z     ( addr len buff -- buff-z )        \ make an ascii string
   dup>r place r> count over + 0 swap c! ;

: open-file ( adr slen fmode -- fileid ior )
   -rot MAXSTRING _LOCALALLOC ascii-z        \ fmode adrstr - & convert to zstring
   2>r                                       \ ( r: adrstr fmode  )
   0                                         \ hTemplateFile
   FILE_FLAG_SEQUENTIAL_SCAN
   OPEN_EXISTING                             \ fdwCreate
   0                                         \ lpsa
   FILE_SHARE_READ
   r@ r/o =                                  \ read, share writing
   if FILE_SHARE_WRITE or                    \ fdwShareMode
   then
   2r> rel>abs                               \ fdwAcess(fmode) lpszName(adr)
   call CreateFile
   dup INVALID_HANDLE_VALUE =                \ fileid ior = 0 = success
   _LOCALFREE ;                              \ release buffer

: create-file ( adr slen fmode -- fileid ior )
   -rot MAXSTRING _LOCALALLOC ascii-z       \ fmode adrstr - & convert to zstring
   2>r                                       \ ( r: adrstr fmode  )
   0                                         \ hTemplateFile
   0                                         \ fdwAttrsAndFlag
   CREATE_ALWAYS                             \ fdwCreate
   0                                         \ lpsa
   FILE_SHARE_READ                           \ fdwShareMode
   2r> rel>abs                               \ fdwAcess(fmode) lpszName(adr)
   call CreateFile
   dup INVALID_HANDLE_VALUE =                \ fileid ior - 0 = success
   _LOCALFREE ;                              \ release buffer

: close-file ( fileid -- ior )
   call CloseHandle 0= ;                     \ hObject 0 = success

| CODE FPARMS-RW  ( addr len fileid -- 0 0 ptr len addr fileid ) \ parms for read/write
\ ptr points here:                     ^
         xor     ecx, ecx                \ zero ecx
         lea     eax, 4 [esp]            \ get esp, ebx is fileid
         push    eax                     \ addr len ptr
         push    4 [esp]                 \ addr len ptr len
         push    12 [esp]                \ addr len ptr len addr
         mov     16 [esp], ecx           \ 0 len ptr len addr
         mov     12 [esp], ecx           \ 0 0 ptr len addr
         add     0 [esp], edi            \ rel>abs
         next    c;

: read-file     ( b-adr b-len fileid -- len ior )
   fparms-rw
   call ReadFile 0= ;                        \ len ior = 0 = success

: write-file    ( adr slen fileid -- ior )
   fparms-rw
   call WriteFile nip 0= ;                   \ ior = 0 = success

: delete-file ( adr len -- ior )
   MAXSTRING _LOCALALLOC ascii-z rel>abs    \ lpszFileName
   call DeleteFile 0=                        \ ior - 0 = success
   _LOCALFREE ;                              \ free buffer

: rename-file ( adr1 len adr2 len -- ior )
   MAXSTRING MAXSTRING + _LOCALALLOC DUP>R     \ get 2 buffers
   ascii-z rel>abs -rot                        \ addr2
   r> MAXSTRING + ascii-z rel>abs              \ addr1
   call MoveFile 0=                            \ adr1a adr2a
   _LOCALFREE ;
   
CODE FPARMS-FP  ( len-ud fileid move --  \ parms for file-position words using SetFilePointer
\                -- MoveHigh move ptrMoveHigh MoveLow fileid ) \ results
\ ptr points here:   ^
                mov     -4 [ebp], edx     \ save edx
                pop     eax               \ fileid
                pop     ecx               \ movehigh
                pop     edx               \ movelow
                push    ecx               \ ( movehigh )
                mov     ecx, esp
                push    ebx               \ ( movehigh move )
                push    ecx               \ ( movehigh move ptrmovehigh )
                push    edx               \ ( movehigh move ptrmovehigh movelow )
                mov     ebx, eax          \ ( movehigh move ptrmovehigh movelow fileid )
                mov     edx, -4 [ebp]     \ restore edx
                next    c;

|: SetFP        ( parms -- len-ud 0 | len-ud err )
   FPARMS-FP Call SetFilePointer dup -1 ( INVALID_SET_FILE_POINTER ) =
   IF Call GetLastError DUP NO_ERROR =
     IF   DROP SWAP 0                         \ return len-ud 0=success
     else NIP 0 SWAP then                     \ return 0 0 ior=err
   else
     SWAP 0                                   \ return len-ud 0=success
   then ;

: file-position ( fileid -- len-ud ior )
   >R 0 0 R> FILE_CURRENT SETFP ;

: advance-file ( len-ud fileid -- ior ) \ RELATIVE position file, not ANS
   FILE_CURRENT SetFP >R 2DROP R> ;     \ ior - 0 = success

: reposition-file ( len-ud fileid -- ior )
   FILE_BEGIN SetFP >R 2DROP R> ;       \ ior - 0 = success

: file-append   ( fileid -- ior )
   >R 0 0 R> FILE_END SetFP >R 2DROP R> ; \ ior - 0 = success

: file-size     ( fileid -- len-ud ior )
   dup>r file-position
   ?DUP IF r>drop >R 2drop 0 0 R> _EXIT THEN  \ error, exit
   0 0 R@                                \ position, fileid
   FILE_END SetFP                       \ move to end
   ?dup if r>drop 4drop _EXIT then      \ error, exit
   2SWAP R> reposition-file ;           \ otherwise reposition

: flush-file ( fileid -- ior )
   call FlushFileBuffers 0= ;                   \ ior - 0 = success

| CODE ADJ-LENS ( buff len buff' -- len len' )  \ adjust lengths: rot - tuck swap - ;
                pop     eax                     \ length of buff
                pop     ecx                     \ address of buff
                sub     ebx, ecx                \ subtract where found (buff - buff')
                push    ebx                     \ save as length of string read
                sub     ebx, eax                \ subtract from original buffer, length to move
                next    c;
  
|: read-line-CRLF ( buff len -- len len true | false ) \ return len of string, len to move file, flag
                2dup 0x0D scan                  \ look for cr
                if
                  dup char+ c@ 0x0A =           \ check next for LF
                  if                            \ is next an LF?
                    adj-lens 2 +                \ len of string and length back
                  else
                    adj-lens 1+                 \ just a CR
                  then
                  true _exit                    \ leave with found
                then                            \ cr not found, perhaps it's an LF only
                drop
                2dup 0x0A scan                  \ look for LF
                if
                  adj-lens 1+                   \ length back
                  true _exit
                then
                3drop false  ;                  \ no just a plain string

: read-line    ( adr len fileid -- len eof ior )
                >r                              \ save the fileid
                0max dup RLLEN !                \ save length requested
                1+ 2dup r@ read-file ?dup       \ read requested chars+1
                if                              \ if read not ok
                  r>drop                        \ drop fileid
                  >r 3drop 0 -1 r> _EXIT        \ ior <> 0 = error
                then
                2dup = RLNEOF !                 \ if req=read is equal, not end of file
                min dup                         \ if read ANY characters
                if
                  RLLEN @ min dup RLLEN !       \ reset length read
                  read-line-crlf                \ scan for line break characters
                  if                            \ if line break
                    RLNEOF @ if 1- 0 min then   \ if not end, need to adjust for extra char read
                    ?dup if                     \ if it's ok to position
                      s>d r> advance-file       \ position file for next time
                      -1 swap _exit             \ len -1 ior
                    else
                      r>drop -1 0 _exit         \ len -1 ior
                    then
                  then
                  RLNEOF @ if                   \ correct if not eof (Bill McCarthy fix)
                    -1. r@ advance-file ?dup    \ we over-read, so step back 1 char
                    if r>drop 0 0 rot _exit     \ reposition-file error                 
                    then
                  then
                  r>drop RLLEN @ -1 0           \ no line break, so len -1 0
                else
                  2drop r>drop 0 0 0            \ nothing read return 0=len, eof=false ior=false
                then    ;

: write-line    ( adr len fileid -- ior )
                dup>r write-file
                crlf$ count r>  write-file or ; \ ior - 0 = success

: resize-file   ( len-ud fileid -- ior )
                dup>r reposition-file drop
                r> call SetEndOfFile 0= ;    \ ior - 0 = success

: file-status   ( adr len -- x ior )
                r/o open-file dup 0=
                if      swap close-file drop
                else    nip
                then    0 swap ;         \ ior - 0 = success

\ -------------------- File I/O --------------------

: FSAVE-FILE    ( addr len filename -- )
                count r/w create-file abort" Failed to create file"
                dup>r write-file abort" Failed to write file"
                r> close-file drop ;

\ -------------------- String Literals --------------------

\ Convert occurances of \N within string a1,n1 to the CRLF pairs.
\ Useful mostly for strings that will get passed to the operating system.

DEFER \N->CRLF  ( a1 n1 -- )    ' 2DROP IS \N->CRLF

: ,"            ( -<string">- )
                [CHAR] " PARSE
                HERE >R DUP C,
                DUP ALLOT
                R@ 1+ SWAP MOVE
                0 C,
                ALIGN
                R> COUNT \N->CRLF ;

NCODE (("))     ( -- counted-string )
                push    ebx
                mov     eax, 0 [ebp]
                movzx   ecx, byte ptr 0 [eax]
                mov     ebx, eax
                lea     eax, 5 [eax] [ecx]  \ account for null at end
                and     eax, # -4       \ align
                mov     0 [ebp], eax
                sub     ebx, edi        \ relative address
                next    c;

DEFER NEW$    ' TEMP$ IS NEW$           ( a1 -- a2 )

: (C")          ( -- counted-string )
                ((")) ;

: C"            ( -<string">- )
                STATE @
                IF      COMPILE (C")  ,"
                ELSE    [CHAR] " WORD
                        NEW$ DUP>R OVER C@ 1+ MOVE
                        r>
                THEN ; IMMEDIATE

: (S")          ( -- addr len )
                ((")) COUNT ;

: S"            ( -<string">- )
                STATE @
                IF      COMPILE (S")  ,"
                ELSE    [CHAR] " WORD
                        NEW$ DUP>R OVER C@ 1+ MOVE
                        r> COUNT
                THEN ; IMMEDIATE

: (.")          ( -- )
                ((")) COUNT TYPE ;

: ."            ( -<string">- )
                COMPILE (.")  ,"  ; IMMEDIATE
                
                : +NULL         ( a1 -- )       \ append a NULL just beyond the counted chars
                count + 0 swap c! ;
                
\ tjz, as posted from Bernd Paysan Thu, 05 Jul 2001  Thanks Bernd

: /parse        ( -- addr u )
                >in @ char swap >in ! dup '"' = over ''' =
                or IF  dup parse 2drop  ELSE  drop bl  THEN  parse ;

: /parse-word   ( -- a1 )
                /parse pocket place     \ word may start with ' or " into pocket
                pocket +null            \ make sure it is null terminated
                pocket ;
                
: /parse-s$     ( -- a1 )               \ parse possibly quoted string
                source >in @ /string    \ addr len of where we are
                bl skip nip             \ skip blanks
                source rot - >in ! drop \ adjust >in
                /parse-word
                ;


\ -------------------- Error Handler --------------------

: CATCH         ( cfa -- flag )
                SP@ >R
                LP @ >R
                OP @ >R
                HANDLER @ >R
                RP@ HANDLER !
                EXECUTE
                R> HANDLER !
                R>DROP
                R>DROP
                R>DROP
                0 ;

: THROW         ( n -- )
                ?DUP
                IF      HANDLER @ RP!
                        R> HANDLER !
                        R> OP !
                        R> LP !
                        R> SWAP >R
                        SP! DROP
                        R>
                THEN ;

: ABORT         ( -- )
                THROW_ABORT THROW ;

: (ABORT")      ( f -- )
                ((")) SWAP
                IF      MSG !
                        THROW_ABORTQ THROW
                THEN    DROP ;

: ABORT"        ( -- )
                COMPILE (ABORT")  ,"  ; IMMEDIATE

\ -------------------- Structured Conditionals --------------------

: ?EXEC  STATE @    ABORT" execution only"    ;
: ?COMP  STATE @ 0= ABORT" compilation only"  ;

: ?PAIRS        ( n1 n2 -- )  XOR ABORT" conditionals not paired"  ;

: >MARK         ( -- addr )   HERE 0 , ;
: >RESOLVE      ( addr -- )   HERE OVER -  SWAP ! ;
: <MARK         ( -- addr )   HERE ;
: <RESOLVE      ( addr -- )   HERE - , ;

: AHEAD  ?COMP  COMPILE  BRANCH  >MARK 2 ; IMMEDIATE
: IF     ?COMP  COMPILE ?BRANCH  >MARK 2 ; IMMEDIATE
: THEN   ?COMP  2 ?PAIRS  COMPILE _THEN  >RESOLVE ; IMMEDIATE
: ENDIF  [COMPILE] THEN ; IMMEDIATE
: ELSE   ?COMP  2 ?PAIRS  COMPILE BRANCH >MARK  SWAP >RESOLVE  2 ; IMMEDIATE

: BEGIN  ?COMP  COMPILE _BEGIN  <MARK 1 ; IMMEDIATE
: UNTIL  ?COMP  1 ?PAIRS  COMPILE _UNTIL  <RESOLVE ; IMMEDIATE
: AGAIN  ?COMP  1 ?PAIRS  COMPILE _AGAIN  <RESOLVE ; IMMEDIATE
: WHILE  ?COMP  COMPILE _WHILE  >MARK 2  2SWAP ; IMMEDIATE
: REPEAT ?COMP  1 ?PAIRS  COMPILE _REPEAT <RESOLVE  2 ?PAIRS >RESOLVE ; IMMEDIATE

: DO     ?COMP  COMPILE (DO)   >MARK 3 ; IMMEDIATE
: ?DO    ?COMP  COMPILE (?DO)  >MARK 3 ; IMMEDIATE
: LOOP   ?COMP  3 ?PAIRS  COMPILE (LOOP)   DUP CELL+ <RESOLVE  >RESOLVE ; IMMEDIATE
: +LOOP  ?COMP  3 ?PAIRS  COMPILE (+LOOP)  DUP CELL+ <RESOLVE  >RESOLVE ; IMMEDIATE

\ -------------------- Eaker CASE statement --------------------

: CASE   ?COMP  COMPILE _CASE  0 ; IMMEDIATE
: OF     ?COMP  COMPILE _OF  >MARK 4 ; IMMEDIATE
: ENDOF  ?COMP  4 ?PAIRS  COMPILE _ENDOF  >MARK  SWAP >RESOLVE  5 ; IMMEDIATE

: ENDCASE  ?COMP  COMPILE _ENDCASE
           BEGIN  ?DUP WHILE  5 ?PAIRS  >RESOLVE  REPEAT ; IMMEDIATE

\ -------------------- Build Header --------------------

: LINK,         ( addr -- )    \ build link from list head at addr
                HERE  OVER @ ,  SWAP !  ;

: SYS-LINK,     ( addr -- )     \ build link from list head at addr
                SYS-HERE  OVER @ SYS-,  SWAP !  ;

: VIEW,         ( -- )          \ compile the view field
                ?LOADING @
                IF      LOADLINE @ SYS-,
                ELSE    -1 SYS-,
                THEN    ;

0 VALUE OFA-LAST

|: "NAME,       ( a1 n1 -- )    \ align and compile name a1,n1 at here
                NAME-MAX-CHARS MIN
                SYS-ALIGN               \ align system space
                APP-ALIGN               \ align application space
                DUP 0= ABORT" Need a NAME to create!"
\                0  SYS-,                \ FFA cell for optimizing compiler
                SYS-HERE TO OFA-LAST
                -1 SYS-,                \ OFA cell for optimizing compiler
                2>R
                CAPS @
                IF      2R@ UPPER
                THEN
                3 2R@ NIP 3 AND - SYS-ALLOT \ pre-align for name length
                2R@ CURRENT @ _SEARCH-WORDLIST
                IF      WARNING @
                        IF      CR ?LOADING @
                                IF      ." From file: " CUR-FILE COUNT TYPE
                                        ."  word: "
                                THEN    2R@ TYPE ."  isn't unique "
                        THEN    DROP
                THEN
                2R> >R SYS-HERE R@ MOVE
                R@ SYS-ALLOT
                SYS-HERE LAST !
                R> SYS-C, ;

|: _"HEADER     ( a1 n1 -- )    \ build a hashed header from a1,n1
                NAME-MAX-CHARS MIN 2DUP 2>R "NAME, VIEW,
                CURRENT @ DUP 2R> ROT VOC#THREADS "#HASH +
                DUP LAST-LINK ! SYS-LINK,
\ the following line looks obscure, but what it does is make sure the CFA
\ follows the CFA-pointer when compiling definitions into system space
                SYS-HERE 0 SYS-, HERE SWAP ! ;

DEFER "HEADER   ' _"HEADER IS "HEADER

: _HEADER       ( -<name>- )    \ build a header, but check available memory
                2000 ?MEMCHK
                BL WORD COUNT "HEADER ;

DEFER HEADER    ( -<name>- )
                ' _HEADER IS HEADER

: CREATE        ( -<name>- )
                HEADER  DOVAR ,  ;

: ALIAS         ( xt -<name>- ) \ make another 'name' for 'xt'
                HEADER LAST @ N>CFAPTR ! ;
                                \ Useage is  ' OLD ALIAS NEW
                                \ See also SYNONYM for an immediate version

\ -------------------- Colon Compiler --------------------

VARIABLE CSP    \ Current Stack Pointer variable

: !CSP          ( -- )  \ save current stack pointer for later stack depth check
                SP@ CSP ! ;

: ?CSP          ( -- )  \ check current stack pointer against saved stack pointer
                SP@ CSP @ XOR ABORT" stack changed" ;

: CALL,         ( addr -- )  \ compile call
                [ HEX E8909090 DECIMAL ] LITERAL ,
                HERE CELL+ - , ;

: (;CODE)       ( -- )
                R> ABS>REL  LAST @ NAME> !  ;

: #(;CODE)      ( a1 -- )
                R> ABS>REL SWAP ! ;

: _]            ( -- )
                STATE ON ;

: _[            ( -- )          \ turn off compiling
                STATE OFF ;

DEFER ]           ' _] IS ]
DEFER [ IMMEDIATE ' _[ IS [     \ turn off compiling

: :NONAME       ( -- xt )       \ start a headerless colon definition
                ALIGN HERE   DOCOL ,  !CSP ] ;

0 VALUE ?:M
0 VALUE PARMS   \ number of parameters

: PARMS-INIT    ( -- )
                FALSE TO ?:M
                0 TO PARMS ;

: _:            ( -<name>- )    \ Forth's primary function defining word
                PARMS-INIT
                HEADER HIDE  DOCOL ,  !CSP ] ;

DEFER :   ' _: IS :

: RECURSE       ( -- )          \ cause current definition to execute itself
                ?COMP  LAST @ NAME> COMPILE, ; IMMEDIATE

\ -------------------- Defining Words --------------------

: CONSTANT      ( n -<name>- )  \ create a constant (unchangable) value
                HEADER  DOCON ,  ,  ;

: VARIABLE      ( -<name>- )    \ create a variable (changable) value
                CREATE 0 , ;

VARIABLE DEFER-LIST             \ The head of a linked-list of deferred words

: DEFER         ( -<name>- )    \ create a deferred execution function
                HEADER DODEFER ,
                COMPILE NOOP
                DEFER-LIST LINK,
                COMPILE NOOP ;

: USER          ( n -<name>- )  \ create a user variable (changable) value
                HEADER  DOUSER ,  , ;

: NEWUSER       ( size - )      \ Creates a user. A user can be
                                \ a byte, cell, float, string or stack
                NEXT-USER @ SWAP OVER + NEXT-USER !
                USER ;

: .USERSIZE     ( - )           \ Shows what is left in the user-area
                USERSIZE 3 CELLS - NEXT-USER @
                cr ." Next-user at " DUP .
                cr ." Free user space " - . ." bytes" ;

\ DO VOCABULARY

CFA-CODE DOVOC  ( -- )                  \ "runtime" for VOCABULARY
                add     eax, # 3 CELLS  \ set CONTEXT to VOC address
                mov     context [edi], eax
                next    c;

ASSEMBLER DOVOC META RESOLVES <VOCABULARY>

: DEFINITIONS   ( -- )
                CONTEXT @ CURRENT ! ;

: 2CONSTANT     ( n1 n2 -- )
                CREATE , ,
                ;CODE   0 OFA-H !       \ disable OFA resolution
                        push    ebx
                        push         2 CELLS [eax] [edi]
                        mov     ebx, 1 CELLS [eax] [edi]
                        next    c;
                DROP                    \ cleanup the stack for meta compiler

: 2VARIABLE     ( -<name>- )
                VARIABLE 0 , ;

\ -------------------- Redefine DEFER-red Words --------------------

NCODE @(IP)     ( -- n )
                push    ebx
                mov     eax, 0 [ebp]
                mov     ebx, 0 [eax]
                add     eax, # 1 CELLS
                mov     0 [ebp], eax
                next    c;

CODE >IS        ( xt -- addr )
                add     ebx, # 4                  \ return >BODY
                next    c;

: (IS)          ( xt -- )   @(IP) >IS ! ;

: ?IS           ( xt -- xt )                    \ error if not a deferred word
                DUP @ DODEFER <> ABORT" Use IS on DEFER-red words!" ;

: IS            ( xt -<name>- )
                STATE @
                IF      COMPILE (IS)   ' ?IS ,
                ELSE      DUP APP-HERE U> >R      \ if xt is system
                        ' DUP APP-HERE U< R> AND  \ and deferred word isn't
                        IF      OVER ?SYS-ADDRESS \ warn user about problem
                        THEN    ?IS >IS !         \ store new deferred func
                THEN ; IMMEDIATE

\ -------------------- Value --------------------

: VALUE         ( n -<name>- )  \ creat a self fetching value, like a constant
                                \ but changable
                HEADER
                DOVALUE   ,
                ( n )     ,
                DOVALUE!  ,
                DOVALUE+! ,   ;

: &OF           ( -<value_name>- addr )
                ' >BODY
                STATE @ IF  [COMPILE] LITERAL  THEN ; IMMEDIATE

|: ?TO_CHECK    ( xt -- xt_body )
                DUP @ >R
                >BODY DUP CELL+ @ -1 =  \ no special words
                R@ DOCON   = OR         \ no constants
                R@ DOCOL   = OR         \ no colon definitions
                R@ DODOES  = OR         \ no DOES> words
                R@ DOVAR   = OR         \ no variables
                R> DODEFER = OR         \ no deferred words
                ABORT" Use TO and +TO on VALUE type words!" ;

: TO            ( n -<value_name>- )
                ' ?TO_CHECK CELL+
                STATE @ IF COMPILE, ELSE EXECUTE THEN ; IMMEDIATE

: +TO           ( n -<value_name>- )
                ' ?TO_CHECK CELL+ CELL+
                STATE @ IF COMPILE, ELSE EXECUTE THEN ; IMMEDIATE

\ -------------------- Interpreter --------------------

CODE DEPTH      ( -- n )        \ return the current data stack depth
                push    ebx
                mov     ebx, SP0 [UP]
                add     ebx, edi
                sub     ebx, esp
                sar     ebx, # 2  \ shift right two is divide by 4
                next    c;

: ?STACK        ( -- )          \ check the data stack for stack underflow
                DEPTH 0< ABORT" stack underflow"  ;

: QUERY         ( -- )          \ accept a line of input from the user to TIB
                TIB DUP MAXSTRING ACCEPT (SOURCE) 2!
                >IN OFF
                0 TO SOURCE-ID 0 TO SOURCE-POSITION ;

: _NUMBER,      ( d -- )
                DOUBLE? 0= IF DROP THEN
                STATE @
                IF      DOUBLE? IF  SWAP  [COMPILE] LITERAL  THEN
                        [COMPILE] LITERAL
                THEN ;

DEFER NUMBER,           ' _NUMBER, IS NUMBER,
DEFER SAVE-SRC          ' NOOP     IS SAVE-SRC
DEFER ?UNSAVE-SRC       ' NOOP     IS ?UNSAVE-SRC

: _INTERPRET    ( -- )
                BEGIN   BL WORD DUP C@
                WHILE   SAVE-SRC FIND ?DUP
                        IF      STATE @ =
                                IF      COMPILE,   \ COMPILE TIME
                                ELSE               \ INTERPRET
                                  EXECUTE ?STACK
                                THEN
                        ELSE    NUMBER NUMBER,
                        THEN    ?UNSAVE-SRC
                REPEAT DROP ;

DEFER INTERPRET    ' _INTERPRET IS INTERPRET

\ -------------------- Evaluate --------------------

: EVALUATE      ( addr len -- ) \ interpret string addr,len
                SOURCE 2>R
                >IN @ >R
                SOURCE-ID >R
                SOURCE-POSITION >R
                (SOURCE) 2!
                >IN OFF
                -1 TO SOURCE-ID
                0 TO SOURCE-POSITION
                ['] INTERPRET CATCH
                R> TO SOURCE-POSITION
                R> TO SOURCE-ID
                R> >IN !
                2R> (SOURCE) 2!
                THROW ;
                
1 PROC Sleep

: WINPAUSE      ( -- )  \ release control to OS for a moment
                0 call Sleep drop ;

\ ----------------- LOADFILE linkage -----------------

VARIABLE LOADFILE                 \ head of linked list of loaded files
         HERE 0 , ," SRC\FKERNEL.F" LOADFILE !-T

\ -------------------- File Loading --------------------

VARIABLE ECHO

: ?.REFILL      ( -- )
                ECHO @
                IF      CR SOURCE TYPE
                        START/STOP
                THEN    ;

DEFER .REFILL   ' ?.REFILL IS .REFILL

\ August 11th, 1997 - 9:02 tjz added to correct for performance bug
0 value len-prev

: REFILL        ( -- f )        \ refill TIB from current input stream
                SOURCE-ID ?DUP
                IF      1+  ( not from evaluate )
                        IF      1 LOADLINE +!
                                LOADLINE @ 255 AND 0=   \ once each 256 lines
                                IF      WINPAUSE        \ release control to OS
                                THEN                    \ for a moment
                                TIB DUP MAXSTRING
\ August 11th, 1997 - 9:02 tjz correction for performance bug
\                                SOURCE-ID FFILE-POSITION DROP ( flag )
\                                DROP ( high part ) TO SOURCE-POSITION
        len-prev +to source-position
\                                SOURCE-ID FREAD-LINE ABORT" read error"
                                SOURCE-ID READ-LINE ABORT" read error"
                                IF      dup 2 + to len-prev
                                        (SOURCE) 2!
                                        >IN OFF
                                        .REFILL
                                        TRUE _EXIT
                                else    0 to len-prev
                                THEN
                                2DROP
                        THEN
                        FALSE _EXIT
                THEN
                CR QUERY TRUE ;

: LINKFILE      ( a1 -- ) \ link name a1 as current file IF LOADING ONLY !!
                ?LOADING @
                IF      LOADFILE SYS-LINK,
                        COUNT SYS-HERE PLACE
                        SYS-HERE C@ 1+ SYS-ALLOT
                ELSE    DROP
                THEN ;

DEFER STACK-CHECK       ' NOOP   IS STACK-CHECK

VARIABLE START-LINE

FALSE VALUE INCLUDING?

: >LINE         ( n1 -- )     \ move to line n1, 1 based
                1- 0 MAX
                ?DUP
                IF      0 DO  REFILL DROP  LOOP
                THEN ;

: DO-INCLUDE    ( -- )
                INCLUDING? >R
                TRUE TO INCLUDING?
                START-LINE @ >LINE
                START-LINE OFF
\ August 11th, 1997 - 9:02 tjz two lines added to correct for performance bug
                source-position >r 0 to source-position
                len-prev        >r 0 to len-prev
                BEGIN   REFILL
                        INCLUDING? AND
                WHILE   INTERPRET
                        STACK-CHECK
                REPEAT
\ August  11th, 1997 -  9:02 tjz two lines added to correct for performance bug
\ October 24th, 1997 - 11:47 tjz reversed following two lines to correct bug
                r> to len-prev
                r> to source-position
                R> TO INCLUDING? ;

16 CONSTANT DEFEXTMAX
CREATE DEFEXT$  ( -- a1 ) \ the default extension buffer
                ," F" 0 , 0 , 0 , 0 ,

VARIABLE EXT_ADDED?             \ January 17th, 2000 - 10:31 added to better
                                \ handle files without extensions
   FALSE EXT_ADDED? !-T

VARIABLE DEFEXT_ON?             \ January 17th, 2000 - 10:31 added to better
                                \ handle files without extensions
    TRUE DEFEXT_ON? !-T

: DEFEXT        ( -<F>- )       \ make F the default extension
                BL WORD COUNT DEFEXTMAX MIN DEFEXT$ PLACE ;

: "TO-PATHEND"  ( a1 n1 --- a2 n2 )     \ return a2 and count=n1 of filename
                OVER 1+ C@ [CHAR] : =   \ second char is ':'
                OVER 2 > AND            \ and name is longer than two characters
                IF      2 /STRING       \ then remove first two characters
                THEN                    \ now scan to end of last '\' in filename
                BEGIN   2DUP [CHAR] \ SCAN ?DUP
                WHILE   2SWAP 2DROP 1 /STRING
                REPEAT  DROP ;

: ?DEFEXT       ( addr -- )       \ conditionally add a default extension
                EXT_ADDED? OFF
                DEFEXT_ON? @
                IF      DUP COUNT "TO-PATHEND"  \ strip off path from filename
                        [CHAR] . SCAN NIP 0=    \ skip to file extension after '.',
                                                \ if not file extension was specified
                        DEFEXT$ C@ 0<> AND      \ and DEFEXT$ is not NULL
                        IF      >R
                                S" ."         R@ +PLACE \ then lay down a decimal point
                                DEFEXT$ COUNT R@ +PLACE \ and lay in default file extension
                                R>
                                EXT_ADDED? ON
                        THEN
                THEN    DROP ;

: _"OPEN        ( a1 n1 -- fileid f1 )  \ open filename a1,n1
                                        \ return fileid and f1=false=ok
                MAXSTRING _LOCALALLOC >R
                R@ PLACE                           \ drop name to OPENBUF
                R@ ?DEFEXT                         \ add extension if needed
                R@ COUNT r/o OPEN-FILE              \ try to open it
                DUP 0=                                  \ if we succeeded
                IF      R@ COUNT CUR-FILE PLACE    \ then set current file
                THEN    R> COUNT POCKET PLACE      \ and set POCKET
                _LOCALFREE ;

DEFER "OPEN     ( a1 n1 -- fileid f1 )  \ open filename a1,n1
   ' _"OPEN IS "OPEN                    \ return fileid and f1=false=ok

: $OPEN         ( addr -- fileid f1 )   \ open counted filename specified by addr
                                        \ return fileid and f1=false=ok
                COUNT "OPEN ;

DEFER START-INCLUDE     ' NOOP IS START-INCLUDE
DEFER   END-INCLUDE     ' NOOP IS   END-INCLUDE

: INCLUDE-FILE  ( fileid -- ) \ load file open on "fileid" to current dictionary
                LOADFILE @ CELL+ >R             \ save pointer to filename
                ?LOADING @ >R
                LOADLINE @ >R
                >IN @ >R
                SOURCE-ID >R
                ( fileid ) TO SOURCE-ID
                SOURCE DUP _LOCALALLOC DUP>R    \ a place to save TIB contents
                SWAP CMOVE                      \ save input line
                SOURCE 2>R                      \ save current source
                SOURCE-POSITION >R

                ?LOADING ON                     \ mark as loading a file
                POCKET LINKFILE                 \ create a filename link
                LOADLINE OFF                    \ clear the loadline counter
                0 TO SOURCE-POSITION            \ reset the loadfile position
                START-INCLUDE
                ['] DO-INCLUDE CATCH            \ load file and catch errors
                SOURCE-ID CLOSE-FILE DROP

                R> TO SOURCE-POSITION
                2R> (SOURCE) 2!                 \ restore input source buffer
                R> SOURCE CMOVE                 \ recover last input line
                _LOCALFREE
                R> TO SOURCE-ID
                END-INCLUDE
                THROW                           \ throw load error if any

                R> >IN !
                R> LOADLINE !
                R> ?LOADING !
                ALIGN
                R> LINKFILE
                ?LOADING @
                IF      LOADFILE @ CELL+        \ step to most recent file
                        COUNT "CLIP"            \ get addr and length
                        CUR-FILE PLACE          \ make current again
                THEN      ;

: INCLUDED      ( addr len -- )   \ load file addr,len into current dictionary
                "OPEN ABORT" file not found"  INCLUDE-FILE ;

: FLOAD         ( -<filename>- )  \ load "filename" into application dictionary
                /PARSE-S$ COUNT INCLUDED ;

: SYS-FLOAD     ( -<filename>- )  \ load "filename" into system dictionary
                IN-SYSTEM? >R IN-SYSTEM
                FLOAD
                R> 0= IF IN-APPLICATION THEN ;

: INCLUDE       ( -<filename>- )  \ load "filename" into application dictionary
                FLOAD ;

: OK            ( -- )
                CUR-FILE COUNT INCLUDED ;

\ -------------------- Comment words --------------------

: \             ( -- )
                SOURCE >IN ! DROP ; IMMEDIATE

\ ----------------------- Locals Allocation on rstack --------------

NCODE _LOCALFREE ( -- )                         \ release local allocation
                mov     ebp, LP [UP]
                xchg    esp, ebp        \ ?
                pop     LP [UP]
                xchg    esp, ebp
                next    c;

NCODE _LOCALALLOCP ( n1 -- a1 )
                sub     ebp, ebx        \ subtract n1 from return stack
                and     ebp, # -4       \ cell align return stack
                mov     ebx, ebp        \ move to top of stack
                sub     ebx, edi        \ convert from absolute to relative
                next    c;

NCODE _LOCALALLOC ( n1 -- a1 )
                xchg    esp, ebp        \ ?
                push    LP [UP]
                xchg    esp, ebp
                mov     LP [UP]  , ebp
                sub     ebp, ebx        \ subtract n1 from return stack
                and     ebp, # -4       \ cell align return stack
                mov     ebx, ebp        \ move to top of stack
                sub     ebx, edi        \ convert from absolute to relative
                next    c;

: LOCALALLOC    ( n1 -- a1 )            \ allocate n1 bytes of return stack
                                        \ return a1 the address of the array
                PARMS                   \ if no locals, setup stack frame
                IF      COMPILE _LOCALALLOCP
                ELSE    -1 TO PARMS
                        COMPILE _LOCALALLOC
                THEN    ; IMMEDIATE

\ -------------------- Local Variable Runtime --------------------

CFA-CODE LOCAL@
                push    ebx
                mov     ecx, LP [UP]
                mov     eax, 4 [eax] [edi]
                mov     ebx, 0 [eax] [ecx]
                next    c;

CFA-CODE LOCAL!
                mov     ecx, LP [UP]
                mov     eax, -4 [eax] [edi]
                mov     0 [eax] [ecx], ebx
                pop     ebx
                next    c;

CFA-CODE LOCAL+!
                mov     ecx, LP [UP]
                mov     eax, -8 [eax] [edi]
                add     0 [eax] [ecx], ebx
                pop     ebx
                next    c;

ASSEMBLER LOCAL@  META CONSTANT DOLOCAL
ASSEMBLER LOCAL!  META CONSTANT DOLOCAL!
ASSEMBLER LOCAL+! META CONSTANT DOLOCAL+!

 0 LOCAL LOCAL0
 1 LOCAL LOCAL1
 2 LOCAL LOCAL2
 3 LOCAL LOCAL3
 4 LOCAL LOCAL4
 5 LOCAL LOCAL5
 6 LOCAL LOCAL6
 7 LOCAL LOCAL7
 8 LOCAL LOCAL8
 9 LOCAL LOCAL9
10 LOCAL LOCAL10
11 LOCAL LOCAL11

\ allocate locals/args

NCODE EXITP
                mov     ebp, LP [UP]
                xchg    esp, ebp
                pop     LP [UP]
                pop     esi
                xchg    esp, ebp
                next    c;

NCODE UNNESTP
                mov     ebp, LP [UP]
                xchg    esp, ebp
                pop     LP [UP]
                pop     esi
                xchg    esp, ebp
                next    c;

NCODE INIT-LOCALS ( loc1 loc2 ... -- )
                xchg    ebp, esp
                push    LP [UP]                 \ save local pointer
                xchg    ebp, esp
                mov     LP [UP] , ebp           \ save new local pointer
                movzx   eax, byte ptr 0 [esi]   \ get # of uninitialized locals
                lea     eax, [eax*4]            \ cell size
                sub     ebp, eax                \ allocate non-initialized
                movzx   ecx, byte ptr 1 [esi]   \ get # of initialized locals
                mov     eax, 4 [esi]            \ optimised next
                add     esi, # 8
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
                
12 CONSTANT #-LOCALS

CREATE LOCAL-PTRS ' LOCAL0 , ' LOCAL1 , ' LOCAL2 , ' LOCAL3 , ' LOCAL4 ,
                  ' LOCAL5 , ' LOCAL6 , ' LOCAL7 , ' LOCAL8 , ' LOCAL9 ,
                  ' LOCAL10 , ' LOCAL11 ,

: >LOC  ( n -- cfa )  CELLS LOCAL-PTRS + @ ;

\ -------------------- Parameter Name List --------------------

\ Names in Win32For are limited in length to name-max-chars characters.
\ Parameter names are stored as name-max-chars byte counted string.
\ PARMLIST holds #-LOCALS plus one names of locals.  The first slot is
\ used as a temp for searching and re-ordering local variable names.

\ CREATE PARMLIST                \ now allocated dynamically in : cold
\         #-LOCALS 1+ NAME-MAX-CHARS 1+ * ALLOT   \ list of parameter names
                                \ first slot is temp for current name
#-LOCALS 1+ NAME-MAX-CHARS 1+ * | CONSTANT PARMLIST-LEN \ headerless
0 VALUE PARMLIST

: "FIRSTPARM    ( addr cnt -- ) \ store string in first slot of parmlist
                NAME-MAX-CHARS MIN PARMLIST PLACE ;

: ADDPARM       ( addr cnt -- ) \ add string to end of parmlist
                PARMS #-LOCALS 1- > ABORT" Too many parameters"
                1 +TO PARMS
                2DUP "FIRSTPARM
                NAME-MAX-CHARS MIN PARMLIST PARMS
                NAME-MAX-CHARS 1+ * + PLACE ;

\ -------------------- Parameter Compiler --------------------

0 VALUE INPARMS         \ number of input paramters
0 VALUE LOCFLG          \ 1 = compiling args, 0 = compiling locals

: PARMS,        ( -- )   \ compile runtime to push parameters
        ?:M                             \ in method?
        IF      CELL NEGATE ALLOT       \ then deallocate the cell layed down
        ELSE    COMPILE INIT-LOCALS     \ else this is a normal local def
        THEN
        INPARMS PARMS OVER - C, ( #locals ) C, ( #args )
        0 C, 0 C,  ( unused filler bytes for cell alignment )   ;

: FIRSTCHR      ( addr cnt -- addr cnt chr )
                OVER C@ ;

: (LOCAL)       ( addr cnt -- )
                DUP 0=
                IF      2DROP
                ELSE    ADDPARM
                        INPARMS LOCFLG + TO INPARMS
                THEN    ;

\ August 2nd, 1999 - 11:13 tjz
\ modfied versin of a word suggested by Robert Smith, to get a word from the
\ input stream, delimited by 'char', even if a line crossing is needed.

: NEXTWORD      ( char -- adr flag ) \ flag=TRUE if we got a word, else FALSE
                BEGIN   DUP WORD DUP C@ 0=
                WHILE   refill 0=
                        IF      NIP     \ discard 'char'
                                FALSE   \ can't find a word
                                _EXIT   \ time to leave
                        ELSE    DROP    \ discard empty word
                        THEN
                REPEAT  NIP TRUE ;

\ August 2nd, 1999 - 10:31 tjz
\ The following source lines were originally supposed to prevent the
\ creation of an extra local variable, when a 'name' type comment was
\ inserted into a local variable declaration. I now believe this was a
\ bad idea, and could prevent real local variables from being defined,
\ when they start with a ''' character.
\                FIRSTCHR DUP [CHAR] - <>       \ as in { name -- }
\                        SWAP [CHAR] ' <> AND   \ or as { name 'name' -- }

\ Also reworked, to allow local variables to be broken into multiple
\ lines, as suggested by Robert Smith.
\ February 11th, 2002 - 17:45  Changes made as per Tom Zimmer request.
\ This now handles the case of   : foo { a b c } ;

: {     ( -- )  \ begin local variable usage in the form;
                \ { initedloc1 initedloc2 \ uninitedloc3 -- comments }
        ?COMP
        PARMS ABORT" Locals may be defined only once per definition."
        0 TO INPARMS
        1 TO LOCFLG
        BEGIN   BL NEXTWORD 0=
                abort" EOF encountered while searching for '}' "
                ?UPPERCASE COUNT
                FIRSTCHR [CHAR] - = >R          \ as in { name -- }

                FIRSTCHR [CHAR] } = R> OR 0=    \ or in { name }
                \ IF neither, then not done
        WHILE   FIRSTCHR [CHAR] \ =             \ start of non-initialized
                                                \ local variables as in
                                                \ { name \ another -- }
                IF      0 TO LOCFLG 2DROP
                ELSE    FIRSTCHR [CHAR] } = ABORT" Args missing --"

                        (LOCAL)
                THEN
        REPEAT  DROP C@ [CHAR] } <> >R
        PARMS 0>
        IF      PARMS,  ( compile runtime code )
        THEN    R>
        IF      BEGIN   BL NEXTWORD 0= ABORT" Args missing }"
                        COUNT FIRSTCHR [CHAR] } = NIP NIP
                UNTIL
        THEN    ; IMMEDIATE

: REVERSEARGS   ( -- )
                INPARMS ?DUP
                IF      2/ 0
                        ?DO     PARMLIST I 1+ NAME-MAX-CHARS 1+ * +
                                PARMLIST NAME-MAX-CHARS 1+ MOVE
                                PARMLIST INPARMS I - NAME-MAX-CHARS 1+ * +
                                PARMLIST I 1+ NAME-MAX-CHARS 1+ * +
                                NAME-MAX-CHARS 1+ MOVE
                                PARMLIST
                                PARMLIST INPARMS I - NAME-MAX-CHARS 1+ * +
                                NAME-MAX-CHARS 1+ MOVE
                        LOOP
                THEN    ;

: LOCALS|       ( -- )
                ?COMP
                PARMS ABORT" Locals may be defined only once per definition."
                0 TO PARMS
                0 TO INPARMS
                1 TO LOCFLG
                BEGIN
                        BL WORD COUNT 2DUP UPPER
                        FIRSTCHR [CHAR] | <>
                WHILE
                        (LOCAL)
                REPEAT  2DROP
                PARMS 0>
                IF      PARMS,  ( compile runtime code )
                THEN
                REVERSEARGS ; IMMEDIATE

: &LOCAL        ( -<name>- a1 ) \ return the address of local "name"
                R> DUP CELL+ >R
                ABS>REL @ DUP @ DOLOCAL <>
                ABORT" Must be followed by a local variable"
                CELL+ @ LP @ ABS>REL + ;

: PFIND         ( addr -- addr FALSE | cfa -1 | cfa 1 )
                FALSE                   \ flag initially not found
                STATE @ 0=              \ if not compiling
                PARMS 1 < OR            \ or no parameters
                IF      _EXIT           \ then don't try to find local
                THEN
                OVER COUNT "FIRSTPARM   \ put in first parm slot
                PARMS 1+ 1
                DO      PARMLIST I NAME-MAX-CHARS 1+ * + COUNT
                        PARMLIST COUNT
                        COMPARE 0=                      \ TRUE if matched
                        IF      2DROP  PARMS  I  -  >LOC TRUE
                                LEAVE
                        THEN
                LOOP    ;

: PARMFIND      ( addr -- addr FALSE | cfa -1 | cfa 1 )
                DUP COUNT FIND-BUFFER PLACE
                FIND-BUFFER ?UPPERCASE
                PFIND ?DUP 0= IF (FIND)  THEN
                DUP>R
                IF      NIP
                ELSE    DROP
                THEN     R> ;

' PARMFIND IS FIND

\ -------------------- Text Interpreter Loop --------------------

\ ( -- )  start editor at error
DEFER EDIT-ERROR   ' NOOP IS EDIT-ERROR

: _RESET-STACKS ( ?? -- )
                SP0 @ SP! ;

DEFER RESET-STACKS   ' _RESET-STACKS IS RESET-STACKS

CREATE NOMSG ," <undefined>"

: _MESSAGE      ( n -- )
                BASE @ >R DECIMAL
                CR ." Error: "
                POCKET COUNT TYPE SPACE
                DUP THROW_ABORTQ =
                IF      DROP MSG @ COUNT TYPE
                ELSE    ." Error # " .
                THEN
                ?LOADING @
                IF      BASE @ >R DECIMAL
                        CR ." File: "
                        LOADFILE @ CELL+ COUNT TYPE
                        ."  at line: "
                        LOADLINE ?
                        R> BASE !
                        EDIT-ERROR
                THEN
                R> BASE ! ;

DEFER MESSAGE   ' _MESSAGE IS MESSAGE

: QUERY-INTERPRET   ( -- )
                QUERY SPACE INTERPRET ;

: _QUIT         ( -- )
                RP0 @ RP!
                BEGIN   [COMPILE] [
                        ?LOADING OFF

                        BEGIN   CR ['] QUERY-INTERPRET CATCH  ?DUP 0=
                        WHILE   STATE @ 0=
                                IF      ."  ok"
                                        DEPTH .SMAX @ MIN 0
                                        ?DO  [CHAR] . EMIT  LOOP
                                THEN
                        REPEAT
                        CONSOLE         \ select the forth console
                        DUP 1+          \ no message on abort
                        IF      MESSAGE
                        THEN
                        RESET-STACKS    \ reset the stacks
                AGAIN ;

: DOCMDTAIL     ( -- )
                CMDLEN 2@ EVALUATE ;

: /IMAGE        ( 'imagename' -- )      \ dummy, really loaded by wrapper
                BL WORD DROP ;          \ just discard the name following

DEFER QUIT              ' _QUIT         IS QUIT
DEFER COMMANDLINE       ' DOCMDTAIL     IS COMMANDLINE
DEFER BOOT              ' INIT-SCREEN   IS BOOT

\ ----------------------- Task support & initialisation ----------------------

maxstring 4 *                                   \ dynamic string lengths
  spcs-max +                                    \ allocates   TIB CUR-FILE TEMP$ FIND-BUFFER
  parmlist-len +                                \ POCKET SPCS PARMLIST
  tiblen + 1024 NALIGNED | CONSTANT DYNALLOCLEN \ declare as headerless constant
  
\ -------------------- Main Entry Point ---------------------------------

1 PROC GetModuleHandle

|: MAIN         ( bufptr -- )                    \ cold start forth main entry point
                ( bufptr )  dup     to TIB       \ adjusted return stack, allocate buffers
                tiblen +    dup     to CUR-FILE
                maxstring + dup     to TEMP$
                maxstring + dup     to FIND-BUFFER
                maxstring + dup     to POCKET
                maxstring + dup     to SPCS
                spcs-max  +         to PARMLIST
                tib (source) cell+ !                  \ adjust tib ptr in (SOURCE)
                spcs spcs-max blank                   \ fill spaces buffer

                init-proc                             \ *** MUST BE FIRST IN MAIN ***

                NULL call GetModuleHandle &HINST ! \ get module handle

                init-malloc

\                PAGE_READWRITE MEM_COMMIT MEM_RESERVE OR
\                0x100000 0 call VirtualAlloc abs>rel
\                0x0FFFF0 + dup SP0 ! SP!              \ new heap

                ['] BOOT CATCH
                IF      BYE         ( fatal error, exit)
                THEN
                &EXCEPT @ 0=
                IF      ['] COMMANDLINE CATCH DUP
                        IF      DUP 1+ IF MESSAGE THEN
                        THEN
                THEN
                RESET-STACKS
                SOURCE-ID
                IF      SOURCE-ID -1 <>
                        IF      SOURCE-ID CLOSE-FILE DROP
                        THEN    0 TO SOURCE-ID
                                0 TO SOURCE-POSITION
                THEN

                QUIT ;

\ -------------------- Cold Start Entry Point --------------------

CFA-CODE _COLD  ( -- )
                HERE ENTRY !-T                  ( entry point )
                push    esi                     \ save regs
                push    edi
                push    ebx
                push    ebp
                mov     ebp, esp                \ top of return stack

                call    @@1
@@1:            pop     edi
                sub     edi, # HERE 1- a;       \ edi = forth base, here 1- is @@1

                mov     ecx, # PROBESTACK 1+    \ number of pages to probe stack
                mov     edx, esp                \ stack address
@@2:            mov     -4092 [edx], # 0        \ probe page at [edx]
                sub     edx, # 4096             \ next page down
                loop    @@2                     \ loop
                
                sub     esp, # DYNALLOCLEN 256 +   \ over-allocate length
                lea     ecx, 128 [esp]          \ pad a little (left & right)
                sub     ecx, edi                \ abs>rel, ecx is area
                
                and     esp, # -16              \ align to 16 byte boundary
                mov     eax, esp                \ rstack top in eax
                sub     eax, edi                \ abs>rel
                sub     esp, # RSTACKSIZE       \ room for return stack
                mov     edx, esp                \ user area is on stack
                mov     fs: 0x14 , edx          \ save in TIB at pvArbitrary
                mov     RP0 [UP] , eax          \ save RP0
                sub     esp, # USERSIZE         \ subtract usersize
                mov     eax, esp                \ top of data stack
                sub     eax, edi                \ abs>rel
                mov     SP0 [UP] , eax          \ save SP0
                mov     BASE [UP] , # 10        \ default base to decimal
                mov     ebx, # NOMSG
                mov     MSG [UP] , ebx          \ set default throw msg

                mov     eax, # ' main           \ no return to this point
                push    ecx
                pop     ebx                     \ stack top value is DYNALLOCLEN ptr
                exec    c;                      \ go do it

\ --------------------- Early DLL and EP initialisation, temporary -------------------

|: init-K32
\ Code to be replaced when wrapper completed...
                &k32lib @ K32DLL lib>handle !     \ loadpoint of kernel32.dll
                &k32gpa @ K32GPA proc>ep !        \ get GetProcAddress
                K32DLL K32GPA proc>lib !          \ set library
                K32LLI proc>name K32DLL lib>handle @ res-loadproc
                  K32LLI proc>ep !                \ LoadLibrary (can't use res-loadcall, recurses)
                K32DLL K32LLI proc>lib !          \ set library
                K32GLE proc>cfa res-loadcall      \ GetLastError early load
                ;
\ -------------------- Tools --------------------

: .S            ( -- )
                ?STACK
                DEPTH .SMAX @ MIN DUP
                IF      ." ["
                        DEPTH 1- 1 .R
                        ." ] "
                        BEGIN   DUP PICK 1 .R
                                BASE @ 16 =
                                IF       ." h"
                                THEN
                                SPACE
                                1- DUP 0=
                        UNTIL
                ELSE    ."  empty "
                THEN    DROP ;

: WAIT          ( -- )
                KEY ( k_ESC ) 27 = IF ABORT THEN ;

18 VALUE SCREENDELAY    \ delay value for some screen output

CREATE DELAYS     0 W,    1 W,   18 W,   25 W,   40 W,
                 60 W,   90 W,  120 W,  200 W,  500 W,

: _START/STOP   ( -- )
                KEY?
                IF KEY  10 DIGIT ( number keys select delay )
                        IF 2 * DELAYS + W@ TO SCREENDELAY
                        ELSE  ( k_ESC ) 27 = IF ABORT THEN  WAIT
                        THEN
                THEN ;

DEFER START/STOP ' _START/STOP IS START/STOP

: NUF?          ( -- f1 )
                ['] START/STOP CATCH ;

: H.R           ( n1 n2 -- )    \ display n1 as a hex number right
                                \ justified in a field of n2 characters
                BASE @ >R HEX >R
                0 <# #S #> R> OVER - SPACES TYPE
                R> BASE ! ;

: H.N           ( n1 n2 -- )    \ display n1 as a HEX number of n2 digits
                BASE @ >R HEX >R
                0 <# R> 0 ?DO # LOOP #> TYPE
                R> BASE ! ;

: H.2           ( n1 -- ) 2 H.N ;               \ two digit HEX number
: H.4           ( n1 -- ) 4 H.N ;               \ four digit HEX number
: H.8           ( n1 -- ) 8 H.N ;               \ eight digit HEX number

: .NAME         ( xt -- )       \ show name, if can't find name, show address
                DUP >NAME DUP NAME> ['] [UNKNOWN] =     \ if not found
                IF      DROP [CHAR] " EMIT ." 0x" 1 H.R [CHAR] " EMIT SPACE
                ELSE    .ID DROP
                THEN    ;

: KWORDS        ( -- )
                CR CONTEXT @
                DUP VOC#THREADS >R
                HERE 500 + R@ CELLS MOVE        \ copy vocabulary up
                BEGIN   HERE 500 + R@ LARGEST DUP
                WHILE   DUP L>NAME NFA-COUNT NIP 7 + ?CR
                        DUP LINK> ." 0x" H.8 SPACE
                        DUP L>NAME .ID 23 #TAB
                        @ SWAP !
                        START/STOP
                REPEAT  2DROP  R>DROP ;

: EMIT.         ( n -- )
                DUP BL 255 BETWEEN 0= IF  DROP [CHAR] .  THEN  EMIT ;

: +NO-WRAP      ( a1 n1 -- a2 ) \ add n1 to a1, limit to address 0xFFFFFFFF
                                \ don't allow wrap around to address zero
                0 TUCK D+ -1 0 DMIN DROP ;

DEFER DUMPC@    ' C@ IS DUMPC@

: DUMP          ( adr len -- )  ( hex byte format with ascii )
                OVER +NO-WRAP DUP ROT
                ?DO     CR I 4 h.R SPACE ." |" SPACE
                        I 16 +NO-WRAP OVER UMIN I
                        2dup
                        DO      I DUMPC@ H.2 space I J 7 + = IF SPACE THEN
                        LOOP    2DUP -  16 OVER - 3 *  SWAP 8 < -  SPACES ." |"
                        DO      I DUMPC@ EMIT.
                        LOOP    ." |" NUF? ?LEAVE
                        SCREENDELAY MS          \ slow down output
            16 +LOOP    DROP    ;

: DM            ( a1 -- )
                -1 DUMP ;

\ =================================================================
\ =================================================================
\
\       These definitions must be last so they are not used
\       during meta compilation
\
\ =================================================================
\ =================================================================

|: EXIT_A       ( -- fl )
                ?COMP
                ?:M     ( -- F1 )
                FALSE TO ?:M
                ; IMMEDIATE

: EXIT          ( -- )
                EXIT_A 
                        ( -- f1 ) ABORT" Can't use EXIT in a Method!"
                PARMS
                IF      COMPILE EXITP
                ELSE    COMPILE _EXIT
                THEN    ; IMMEDIATE

: ?EXIT         ( F1 -- )
                EXIT_A 
                        ( -- f1 ) ABORT" Can't use ?EXIT in a Method!"
                COMPILE ?BRANCH >MARK
                [COMPILE]  EXIT
                COMPILE _THEN
                >RESOLVE ; IMMEDIATE

|: DOES>_A      ( -- )
                ?COMP
                ?:M     ( -- F1 )
                FALSE TO ?:M
                        ( -- f1 ) ABORT" Can't use DOES> in a Method!"
                PARMS
                IF      COMPILE _LOCALFREE
                        COMPILE PARMS-INIT
                THEN
                ; IMMEDIATE
                
|: DOES>_B      ( -- )
                DODOES CALL,
                PARMS-INIT ; IMMEDIATE

: DOES>         ( -- )
                DOES>_A
                COMPILE (;CODE)
                DOES>_B ; IMMEDIATE

: #DOES>        ( -- )          \ "compile time"
                ( a1 -- )       \ "runtime" a1=cfa of word being defined
                DOES>_A
                COMPILE #(;CODE)
                DOES>_B ; IMMEDIATE

: LOCALALLOC:   ( n1 -<name>- )   \ allocate a local n1 byte buffer to local "name"
                ?COMP
                [COMPILE] LOCALALLOC [COMPILE] TO ; IMMEDIATE

\ --------------------------------------------------------------------------
\ --------------------------------------------------------------------------
\ Semicolon ';' must be compiled after its last use in the meta compile
\ --------------------------------------------------------------------------
\ --------------------------------------------------------------------------

DEFER DO-;CHAIN ' NOOP IS DO-;CHAIN

: ;             ( -- )
                ?COMP
                ?:M     ( -- F1 )
                FALSE TO ?:M
                        ( -- f1 ) ABORT" Methods must END in ;M !"
                ?CSP REVEAL
                PARMS
                IF      COMPILE UNNESTP
                ELSE    COMPILE UNNEST
                THEN    [COMPILE] [ 0 TO PARMS DO-;CHAIN ; IMMEDIATE

\ -------------------- The End --------------------

' THROW         RESOLVES THROW
' NFA-COUNT     RESOLVES NFA-COUNT
\ ' HERE          RESOLVES HERE
\ ' UNLOADFORTH   RESOLVES UNLOADFORTH  \ no longer used
' START/STOP    RESOLVES START/STOP
' -ALIGNED      RESOLVES -ALIGNED
' _LOCALALLOC   RESOLVES _LOCALALLOC
' _LOCALFREE    RESOLVES _LOCALFREE
' PARMFIND      RESOLVES PARMFIND
' .NAME         RESOLVES .NAME

\ arm resolves added as kernel modified
' LINK,         RESOLVES LINK,
' LOAD-WINLIBRARY RESOLVES LOAD-WINLIBRARY
' RES-LOADCALL  RESOLVES RES-LOADCALL
' LOADFILE      RESOLVES LOADFILE
' INIT-K32      RESOLVES INIT-K32 
\ Usage for ALIAS added March 11th, 2002 - 21:57 jp
