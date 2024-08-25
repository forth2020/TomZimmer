anew w_search.f  \ October 7th, 2002  By J.v.d.Ven.

\ July 27th, 2002
\ Corrected a number of bugs. Now w-search returns the found string.

\ May 23rd, 2003
\ - Made w-search case insensitive

\ July 7th, 2003
\ - Made this source independed from toolset.f
\ - added an extra flag to w-search. The flag determines if the search
\   is case insensitive or not.
\ - *search puts a * before the specification string.
\ - Then it performs a w-search

\ July 21st, 2003
\ - Changed w_search. Now it will return the stack right when a search
\   buffer or a specification string has a lenght of 0.

\  From toolset.f \ load it here when you would like to use it.

defined toolset.f nip not [IF]

: never         ( flag - false ) drop false ;
: -dup  ( n1 n2    - n1 n1 n2 )    >r dup r>  ;

\ October 15th, 2002, "Lcc Wizard" Gave me a 2nip in assembler

CODE 2nip       ( n1 n2 n3 n4 -- n3 n4 ) \ 2swap 2drop
                pop     eax
                mov     4 [esp], eax
                pop     eax
                next    c;

\ May 22nd, 2002 - 18:50
\ s-exchange to prevent roll when posible

code s-exchange ( ... n[k]..0 k -- ... 0..n[k] )
                mov  eax, ebx               \ save k
                mov  ecx, [esp]             \ save nos
                mov  ebx,  0 [esp] [eax*4]  \ duplicate n[k] in tos
                mov  0 [esp] [eax*4] , ecx  \ put nos where n[k] was
                pop eax           \ discard second item on data stack
                next    c;

\ Usage:
\ 10 20 30 40   3  cr .s  s-exchange    cr .s
\ result:
\ [5] 10 20 30 40 3
\ [4] 40 20 30 10   ok....


\ Initial Made by Ron Aharon.
\ COMPAREI is the same as COMPARE , but case-insensitive
\ Modifications to get a better shell-sort:
\ 1. The version leaves the chars from 0 to 2F unchanged.
\    The effect is that in a sort the non-alphanumeric characters will not be
\    between the alphanumeric characters.
\ 2. It uses jnb instead of jns which makes sure that the character FF is at
\    the end of a sorted list.
\ 3. Changed the name to COMPAREIA to avoid future conflicts.


code compareia    ( adr1 len1 adr2 len2 -- n )
    sub     ebp, # 8
    mov     0 [ebp], edi
    mov     4 [ebp], esi
    pop     eax                     \ eax = adr2
    pop     ecx                     \ ecx = len1
    pop     esi                     \ esi = adr1
    add     esi, edi                \ absolute address
    add     edi, eax                \ edi = adr2 (abs)
    sub     eax, eax                \ default is 0 (strings match)
    cmp     ecx, ebx                \ compare lengths
    je      short @@2
    ja      short @@1
    dec     eax                     \ if len1 < len2, default is -1
    jmp     short @@2
@@1:
    inc     eax                     \ if len1 > len2, default is 1
    mov     ecx, ebx                \ and use shorter length
@@2:
    mov     bl, BYTE [esi]
    mov     bh, BYTE [edi]
    inc     esi
    inc     edi
    cmp     bx, # $2F2F            \ skip chars beteen 0 and 2F ( now lower case )
    jle     short @@7
    or      bx, # $2020            \ May 21st, 2003 or is better then xor
@@7:
    cmp     bh, bl
    loopz   @@2

    je      short @@4               \ if equal, return default
    jnb     short @@3               \ ** jnb for an unsigned test ( was jns )
    mov     eax, # 1                \ if str1 > str2, return 1
    jmp     short @@4
@@3:
    mov     eax, # -1               \ if str1 < str2, return -1
@@4:
    mov     ebx, eax
    mov     edi, 0 [ebp]
    mov     esi, 4 [ebp]
    add     ebp, # 8
    next    c;

\ Searchai str1 for substring str2 in a case-insenitive manner.
\ If found, return the address of the start of the
\ string, the characters remaining in str1 and a true flag.
\ Otherwise return the original str1 and a false flag.

\ ESI = pointer to source string (str2)
\ EBX = length  of source string
\ EDI = pointer to destination string (str1)
\ ECX = length  of destination string
\ EDX = pointer for compare

CODE searchia     ( adr1 len1 adr2 len2 -- adr3 len3 flag )
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
                mov     ah, 0 [edx] [edi]
                or      ax, # $2020        \ make it lowercase
                cmp     al, ah
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

[THEN]

0 value last-cnt
0 value CaseSensitive?

: starting-with? ( adres-spec /spec  adr-search /search - next-adr /next flag )
   locals| /spec adr-search |
   dup -rot adr-search over CaseSensitive?
      if    compare
      else  compareia
      then
   not dup
      if    >r dup  dup +to last-cnt adr-search + /spec rot - r>  \ First
      else  nip adr-search /spec rot
      then
 ;

: containing? ( adres-spec /spec adr-search /search -- next-adr /next flag )
    drop dup>r  2swap  CaseSensitive?
        if   search
        else searchia
        then
    dup
       if     r>  2 pick - +to last-cnt
       else  r>drop
       then
  ;

ascii * value wildcard-char

: continue-w-search? ( /search /spec result - /search flag )
   swap 0> and over 0> and ;

: scan-for-wildcard   ( adr-spec /spec - adr len flag )
   wildcard-char scan dup 0= ;

: target-search ( >adr-spec max-spec adr-spec /spec adr-search /search - >adr-spec1 max-spec1 adr-spec /spec adr-search /search )
   2over scan-for-wildcard
       if   2drop
       else  dup 7 s-exchange swap - 4 s-exchange drop 6 s-exchange drop
       then
 ;

: next-search ( >adr-spec max-spec adr-spec /spec adr-search /search - >adr-spec max-spec adr-spec1 /spec2 adr-search /search  )
   2>r 2drop 2dup 2r>  ;

: wildcard ( >adr-spec max-spec adr-spec /spec adr-search /search - >adr-spec max-spec adr-spec1 /spec2 adr-search /search  )
   2>r 2drop 1- swap 1+ swap    \ in spec$
   2dup scan-for-wildcard
      if    2drop  >r 0 over r>
      else  2swap  2 pick -
      then
   2r>
 ;


\ adr and len are invalid when the flag returns 0 in w-search
\ w-search is might be case sensitive
\ CaseSensitive? 0= stands for case insensitive.
\ A * in the specification string skips 0 or more characters until
\ the next substring is found. A duplicate substring might be confusing.

: w-search ( adr-spec /spec adr-search /search flag - adr len flag )
   to CaseSensitive?
   0  0 locals| /last adr  |
   dup 0> 3 pick 0> and
   if  2>r 2dup 2r>   true >r
       2 pick r@ continue-w-search? 0 to last-cnt
           if
                begin  2 pick r@ continue-w-search?
                while  3 pick c@ wildcard-char  =
                        if    wildcard 4dup
                               adr containing?   adr 0=  \ *
                                  if  0 to last-cnt
                                  then
                        else  target-search  4dup adr 0=
                                  if    over to adr
                                  then
                               starting-with?
                        then
                     r> and dup>r adr 0= and
                        if    over to adr
                        then
                     2nip 2 pick to /last next-search
                repeat
                4drop 2drop last-cnt /last +
             else drop
             then
        adr swap r@ 0<> and r>
     else   2nip false   \ July 21st, 2003 Solves the zero buffer problem
     then
 ;

: +trailing ( adr count char - adr2 count2 )
    -rot dup>r over + swap dup>r
        ?do   i c@ over <>
                if   i leave
                then
        loop
     nip r> -dup - r> swap -
 ;

: #number-line> ( adr count which seperator - n flag )
    locals| seperator |   dup 0>
       if    >r seperator  +trailing r> 0
                do    seperator scan seperator +trailing
                loop
       else  drop seperator  +trailing
       then
    -dup seperator scan drop over -
     number? -rot d>s swap
 ;

: search-window ( adr cnt - hndl )
   2>r call GetActiveWindow dup
    begin
       GW_HWNDNEXT swap call GetWindow dup 0<>
          if  dup  MAXSTRING pad rel>abs rot call GetWindowText pad swap
              2r@ 2swap w-search nip never
          else never 0 true
          then
      or
     until nip 2r> 2drop
 ;

create *buffer maxstring allot
\ *search puts a * before the specification string.
\ Then it performs a w-search

: *search ( adr-spec /spec adr-search /search flag - adr len flag )
  >r 1 *buffer c!  wildcard-char *buffer 1+ c!
  2swap  *buffer +place
  *buffer count 2swap  r> w-search
 ;

\s

create search-buffer maxstring allot
 s" xxxx <ccc>"  search-buffer place
 s" *<*>"
 search-buffer count false w-search  cr .s [if]   .( Found: ) type
                                           [else] .( String not found:) 2drop
                                           [Then]
\s
\ Result Found: <ccc>

 s" <*>"
 search-buffer count false *search  cr .s [if]   .( Found: ) type
                                          [else] .( String not found:) 2drop
                                          [Then]

\ Result Found: <ccc>
\s

