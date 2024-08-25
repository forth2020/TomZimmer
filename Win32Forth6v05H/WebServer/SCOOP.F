\ SCOOP = Scoped Component Polymorphic library
\ Tom Dixon

\ This is a few simple words to allow development of 
\ Components and objects.  Should be pretty easy to 
\ port to whatever.


\ *** List Library ***

: cons ( node list -- list ) over ! ;
: lrest ( list -- list ) @ ;
: node, ( -- node ) here 0 , ;

: lmap ( xt list -- )
  begin dup while
    2dup 2>r CELL+ swap execute 2r>
  lrest repeat 2drop ;


\ *** Parsing Helpers ***

: atoi ( str len -- n ) 0 0 2swap >number 2drop d>s ;
: peek-word ( -- str len ) >in @ parse-word rot >in ! ;
: skip-word ( -- ) parse-word 2drop ;
: peek ( -- str len )
  begin peek-word dup 0= while
    refill 0= if exit else 2drop then
  repeat ;


\ *** Simple Scoped Polymorphism Library ***

sys-warning-off
: scope ( <name> -- ) wordlist value ;

0 value withlength

: with ( <name> -- ) 
  withlength 1+ to withlength ' execute 
  >r get-order r> swap 1+ set-order ; IMMEDIATE
  
: endwith ( -- ) 
  withlength 0= if exit then
  get-order nip 1- set-order 
  withlength 1- to withlength ; IMMEDIATE
  
: endwiths ( -- ) 
  withlength 0 ?do ['] endwith execute loop ;

: disp-bind ( xt f -- ) 
  0> if execute exit then 
  state @ if compile, else execute then ;

: (dispatch) ( str len scope -- ) 
  search-wordlist dup if disp-bind else drop then ; 
  
: dispatch ( scope -- ) 
  peek rot search-wordlist dup 
  if skip-word disp-bind else drop then ; 


\ *** Simple Object Extentions to Scopes ***

: osize ( scope -- n ) s" size" rot (dispatch) ;

: object: ( scope -- ) 
  create dup , osize allot IMMEDIATE
  does> dup cell+ state @ 
    if POSTPONE literal else swap then @ dispatch ;

: array: ( n scope -- ) 
  create dup , osize dup , * allot IMMEDIATE
  does> dup >r 2 cells + r@ cell+ @ state @ 
    if POSTPONE literal POSTPONE * POSTPONE literal POSTPONE + 
    else rot * + then r> @ dispatch ;  

sys-warning-on
