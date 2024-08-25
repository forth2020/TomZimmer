{ ------------------------------------------------------------------------
String comparison

Compare the string specified by c-addr1 u1 to the string specified by
c-addr2 u2. The strings are compared beginning at the given addresses,
character by character, up to the length of the shorter string or until
a difference is found. If the two strings are identical, n is 0. If the
two strings are identical up to the length of the shorter string, n is
-1 if u1 is less than u2 and 1 otherwise. If the two strings are not
identical up to the length of the shorter string, n is -1 if the first
non-matching character in the string specified by c-addr1 u1 has a
lesser numeric value than the corresponding character in the string
specified by c-addr2 u2 and 1 otherwise.

REPE-CMPSB(UC) is a subroutine which replaces REPE CMPSB with the
   distinction of being case insensitive. No free registers.

COMPARE tests the two strings for equality, returning a multi-flag.
   -1 if string 1 is less than string 2, +1 if greater, 0 if equal.
COMPARE(NC) is the same, but non-case-sensitive.
COMP is a string compare which respects the system variable CAPS.
------------------------------------------------------------------------ }

\ WHILE CountReg <> 0
\ DO
\    [source-index] - [destination-index];
\    source-index := source-index + 1;
\    destination-index := destination-index + 1;
\
\    CountReg := CountReg - 1;
\    IF (instruction is REP/REPE/REPZ) AND (ZF=1)
\       THEN exit WHILE loop
\ LOOP

LABEL REPE-CMPSB(UC)
        EAX PUSH
        2 L# JECXZ                  \ while countreg <> 0

1 L:                                \ read a byte at dest and uppercase it
        0 [EDI] AL MOV              \ read one byte at dest
        EDI INC                     \ point to next
        CHAR a # AL CMP             \ check against "a"
        5 L# JB                     \ below, do nothing
        CHAR z # AL CMP             \ check against "z"
        5 L# JA                     \ below, do nothing
        32 # AL XOR                 \ make upper case

5 L:

        0 [ESI] AH MOV              \ read one byte at source
        ESI INC                     \ point to next
        CHAR a # AH CMP             \ check against "a"
        6 L# JB                     \ below, do nothing
        CHAR z # AH CMP             \ check against "z"
        6 L# JA                     \ below, do nothing
        32 # AH XOR                 \ make upper case

6 L:
        AL AH CMP                   \ compare against each other
        2 L# JNZ                    \ bail now on mismatch
        1 L# LOOPE                 \ decrement ecx and continue on match

2 L:
        EAX POP
        RET END-CODE

CODE COMPARE ( c-addr1 len1 c-addr2 len2 -- n )
   EBX ECX MOV   EBX EBX SUB    \ ecx = len2, ebx = 0
   4 [EBP] ECX CMP              \ len2 - len1
   0<> IF                       \ if not equal
      0< IF                     \ len2<len1
         EBX INC                \ return 1 if len2 > len1
      ELSE                      \ or
         EBX DEC                \ return -1 if len1 < len2
         4 [EBP] ECX MOV        \ but set to use len1 anyway
   THEN THEN
   ECXNZ IF                     \ skip if nothing to compare
      ESI PUSH   EDI PUSH       \ save registers
      8 [EBP] ESI MOV           \ get addresses
      0 [EBP] EDI MOV
      REPE CMPSB                \ do the compare
      0<> IF                    \ so if a non-zero count compared
         CC IF                  \ check for carry clear
            1 # EBX MOV         \ and return 1
         ELSE
            -1 # EBX MOV        \ or -1
      THEN THEN                 \ or return ebx from first test if count=0
   EDI POP   ESI POP   THEN     \ restore registers
   12 [EBP] EBP LEA             \ clean up stack
   RET   END-CODE

CODE COMPARE(NC) ( c-addr1 len1 c-addr2 len2 -- n )
   EBX ECX MOV   EBX EBX SUB    \ ecx = len2, ebx = 0
   4 [EBP] ECX CMP              \ len2 - len1
   0<> IF                       \ if not equal
      CS IF                     \ len2<len1
         EBX INC                \ return 1 if len2 is zero
      ELSE                      \ or
         EBX DEC                \ return -1 if len1 is zero
         4 [EBP] ECX MOV        \ but set to use len1 anyway
   THEN THEN
   ECXNZ IF
      ESI PUSH   EDI PUSH       \ save registers
      8 [EBP] ESI MOV           \ get addresses
      0 [EBP] EDI MOV
      REPE-CMPSB(UC) CALL       \ do the compare
      0<> IF                    \ so if a non-zero count compared
         CC IF                  \ check for carry clear
         1 # EBX MOV            \ and return 1
         ELSE
            -1 # EBX MOV        \ or -1
      THEN THEN                 \ or return ebx from first test if count=0
   EDI POP   ESI POP   THEN     \ restore registers
   12 [EBP] EBP LEA             \ clean up stack
   RET   END-CODE

: COMPARE(CS) ( addr1 u1 addr2 u2 -- n )        \ Use current case-sensitivity
   CAPS @ IF  COMPARE(NC)  ELSE  COMPARE  THEN ;

{ --------------------------------------------------------------------
-MATCH is a polyForth primitive which looks for (s #) in (a n), and
   returns the address after the match if found and false or (if not
   found) garbage and true.
-------------------------------------------------------------------- }

CODE -MATCH ( a n s # -- a t)
   ESI PUSH                         \ save registers
   EDI PUSH                         \
   EBX EDX MOV                      \ # to edx
   0 [EBP] EAX MOV                  \ s to eax
   4 [EBP] ECX MOV                  \ n to ecx
   8 [EBP] EDI MOV                  \ a to edi
   EBX EAX XCHG                     \ swap # with s
   8 [EBP] EBP LEA                  \ clean up stack
   0 [EBX] AL MOV                   \ read first char
   EDX DEC                          \ decrement count
   EDX ECX SUB                      \ how many chars in source to consider
   0> IF                            \
      EBX INC                       \
      BEGIN                         \
         REPNZ SCASB                \ scan for the first char of pattern
      0= WHILE   ( ECXst match)     \ if we found a first char match
         ECX PUSH                   \ save where we are
         EDI PUSH                   \
         EBX ESI MOV                \
         EDX ECX MOV                \
         REPE CMPSB                 \ compare the strings
      0= NOT WHILE                  \ while not a match
         EDI POP                    \ restore the pointers
         ECX POP                    \
      REPEAT                        \ if repeat, look some more
         8 # ESP ADD                \ match, clean up return stack
         EAX EAX SUB                \ and return a zero
      THEN                          \
   THEN                             \
   EDI 0 [EBP] MOV                  \ write final address to stack
   EAX EBX MOV                      \ return flag
   EDI POP                          \ restore registers
   ESI POP                          \
   RET END-CODE

{ ------------------------------------------------------------------------
Case conversion

UPPER  converts a single character to uppercase.
UPCASE  converts a string to uppercase. Note that it performs this
   conversion in-situ, modifying the string given to it.
------------------------------------------------------------------------ }

ICODE UPPER ( char -- CHAR )
   CHAR a # BL CMP                  \ check char against "a"
   U>= IF                           \ if below, do nothing
      CHAR z # BL CMP               \ check against "z"
      U<= IF                        \ if above, do nothing
         32 # BL XOR                \ was between, mask with $20 to uppercase
      THEN                          \
   THEN                             \
   RET END-CODE

CODE UPCASE ( a # -- )
   EBX ECX MOV                      \ count to ecx
   0 [EBP] EDX MOV                  \ address in edx
   ECX ECX OR  0<> IF
      BEGIN                         \
         0 [EDX] AL MOV             \ read a char
         CHAR a # AL CMP            \ check char against "a"
         U>= IF                     \ if below, do nothing
            CHAR z # AL CMP         \ check against "z"
            U<= IF                  \ if above, do nothing
               32 # AL XOR          \ was between, mask with $20 to uppercase
            THEN                    \
         THEN                       \
         AL 0 [EDX] MOV             \ and write it back
         EDX INC                    \ point to next address
      LOOP                          \
   THEN                             \
   4 [EBP] EBX MOV                  \ refresh tos
   8 # EBP ADD                      \ clean up stack
   RET END-CODE

{ --------------------------------------------------------------------
SPLIT divides a string at a given character. The first part of the
   string is on top, the remaining part underneath. The remaining part
   begins with the scanned-for character.
-------------------------------------------------------------------- }

: SPLIT ( addr len char -- addr len addr len )
   >R 2DUP R> SCAN ROT OVER - >R ROT R> ;
