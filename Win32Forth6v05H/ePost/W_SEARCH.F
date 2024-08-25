anew w_search.f  \ October 7th, 2002 - 10:26 By J.v.d.Ven.

\ July 27th, 2002 - 17:24
\ Corrected a number of bugs. Now w-search returns the found string.

needs toolset.f

0 value last-cnt

: starting-with? ( adres-spec /spec  adr-search /search - next-adr /next flag )
   locals| /spec adr-search |
   dup -rot adr-search over compare not dup
      if    >r dup  dup +to last-cnt adr-search + /spec rot - r>  \ First
      else  nip adr-search /spec rot
      then
 ;

: containing? ( adres-spec /spec adr-search /search -- next-adr /next flag )
    drop dup>r  2swap   search  dup
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
\ w-search is case sensitive
\ A * in the specification string skips 0 or more characters until
\ the next substring is found. A duplicate substring might be confusing.

: w-search ( adr-spec /spec adr-search /search - adr len flag )
   0  0 locals| /last adr  |
   2>r 2dup 2r>   true >r
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


\s

create search-buffer maxstring allot
 s" xxxx <ccc>"  search-buffer place
 s" *<*>"
 search-buffer count w-search  cr .s [if]   .( Found: ) type
                                     [else] .( String not found:) 2drop
                                     [Then]
\ Result Found: <ccc>
\s

