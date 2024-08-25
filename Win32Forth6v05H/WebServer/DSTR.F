\ Dynamic Strings
\ Tom Dixon

2 cells CONSTANT $size
  
  : null? @ 0= ; 
  
  : $count ( o -- str len ) dup @ swap cell+ @ ;
  
  : $. ( o -- ) [char] " emit count type [char] " emit ;
  
  : $free ( o -- ) dup >r @ dup if free then drop 0 0 r> 2! ;
  
  : (buf+) ( len o -- addr ) 
    dup >r cell+ @ + r> dup null? if drop allocate throw 
    else @ swap resize throw then ;
    
  : $append ( str len o -- )
    >r dup r@ (buf+) dup r@ ! r@ CELL+ @ + 
    swap dup r> CELL+ +! cmove ;
    
  : $prepend ( str len 0 -- )
    >r dup r@ (buf+)
    dup r@ ! 2dup + r@ CELL+ @ cmove>
    r@ @ swap dup r> CELL+ +! cmove ;
  
: $place ( str len o -- ) dup free append ;
  
  : $Left ( n o -- str len ) count rot min ;
  : $Right ( n o -- str len ) count rot /string ;
  : $Mid ( start len o -- str len ) rot swap $Right rot min ;
  

