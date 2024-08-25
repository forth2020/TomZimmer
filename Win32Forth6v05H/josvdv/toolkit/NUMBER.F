anew number.f    \ March 12th, 2002 - 11:48

\ Shows numbers according to the international settings of the control panel.
\ March 11th, 2002 - 14:57 changed init_digits.
\ Now it seems to be right for various windows-systems

needs toolset.f  \ Update: December 17th, 2001 or better.

: s(d.#)   ( d1 n1 sign -- a1 count ) \ display d1 with n1 places behind DP
    >r  >r <#
    r>  ?dup            \ if not zero, then display places
        if   0 max 0
             ?do #
             loop [char] . hold
        then
    #s r>
        if   [char] - hold   \ including the sign
        then
    #>
    ;

: lprep  ( l h n1 sign - base hld-max hld_abs 0 adr$ 0 LOCALE_SYSTEM_DEFAULT )
   2>r base @ -rot decimal
   2>r  hld-max hld rel>abs 0 2r>
   pad 30 erase 2r> s(d.#) drop  rel>abs 0 LOCALE_SYSTEM_DEFAULT
 ;

: lfprep ( f: f - ) ( - base hld-max hld_abs 0 adr$ 0 LOCALE_SYSTEM_DEFAULT )
   base @ decimal hld-max hld rel>abs 0
   pad 30 erase pad fvalue-to-string pad 1+ rel>abs 0 LOCALE_SYSTEM_DEFAULT
 ;

0 value #idigits
0 value #iCurrDigits

: digit@>s   ( adr - digit   )  c@ 0xf and ;
: reg-digit@ ( adr n - digit )  drop digit@>s ;

: no_idigits ( count - count_without_the_idigits )
   #idigits 0>
      if    #idigits - 1-
      then
 ;

: type-r ( adr count right-justify - )
  2dup >=
     if    drop
     else  over - spaces
     then  type
 ;

: dsigned?   ( l h - ul hl sign ) 2dup dabs 2swap d0< ;

: (l.$fin)   ( l h - adr count )
   dsigned? #iCurrDigits swap lprep
   call GetCurrencyFormat hld-count rot base !
 ;
: (l.ud)     ( l h n1 sign  - adr count ) \ max: +/- -18.446.744.073.709.551.615
    lprep call GetNumberFormat hld-count rot base ! ;


: (l.d)      ( l h n1  - adr count ) >r dsigned? r> swap (l.ud) ;
: (l.dint)   ( l h - adr count )     0 (l.d) no_idigits ; \ January 11th, 2002
: (l.int)    ( n -  adr count )      s>d (l.dint) ;       \ January 11th, 2002
: l.ud       ( l h - )               #idigits false (l.ud) type ;
: l.d        ( l h - )               #idigits (l.d) type ;
: l.         ( n - )                 s>d l.d ;
: l.int      ( n - )                 (l.int)  type ;
: l.d$fin    ( l h - )               (l.$fin) type ;
: l.$fin     ( n - )                 s>d l.d$fin ;
: l.dr       ( r l h - )             #idigits (l.d) rot type-r ;
: l.r        ( n r - )               swap s>d l.dr ;
: l.intr     ( n r - )               swap s>d 0 (l.d) no_idigits rot type-r ;
: l.d$finr   ( l h r - )             -rot (l.$fin) rot type-r ;
: l.$finr    ( n r - )               swap s>d rot l.d$finr ;

: (l.f)        ( f: f - adr count )  \ not very accurate when B/FLOAT is 8
   precision 26 set-precision lfprep
   call GetNumberFormat hld-count rot base ! rot set-precision
 ;

: l.f        ( f: f - ) (l.f) type ;
: l.fr       ( r - )  ( f: f - ) (l.f) rot type-r ;

: #zeros-in$? ( adr count - #zeros )
    0 swap 0    do    over i + c@ ascii 0 = abs +    loop  nip ;

: init_digits  ( - )
   1 0 0 (l.d)    #zeros-in$? to #idigits
   1 0   (l.$fin) #zeros-in$? to #iCurrDigits
 ;

initialization-chain chain-add init_digits
init_digits

((  ( Remove or disable this line to see the demo )

\ Usage:

cr -1234e  l.f
cr  123.4e l.f
cr
cr -12e      12 l.fr
cr  123.4e   12 l.fr
cr -12345e   12 l.fr
cr  1239.47e 12 l.fr

cr
cr 9841  l.
cr -9841 l.int

cr
cr -12345 10 l.intr
cr 123    10 l.intr
cr 1      10 l.intr
cr 12345  10 l.r

cr \ The Euro-sign will be shown when it is in the current font.
cr -1234567 l.$fin
cr
cr -1234567 15 l.$finr
cr    12345 15 l.$finr
cr      -12 15 l.$finr

cr
cr  1  1 l.ud
cr -1 -1 l.ud

: .GetProcessWorkingSetSize
   hld rel>abs
   hld 4 + rel>abs
   call GetCurrentProcess
   call GetProcessWorkingSetSize ?win-error
   hld 4 + @ hld @
   cr 10 ." Maximum working set size" l.intr
   cr 10 ." Minimum working set size" l.intr
 ;
cr .GetProcessWorkingSetSize cr \ ))

 \s
