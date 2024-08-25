\ BigStrings.f  V1.0  2021 Nov 17

\ *****************************************************************************
\ Big 32 bit count versions of Place , Append and Count
\ The first 32 bit cell contains a 32 bit byte count
\ *****************************************************************************

\ : 2**   ( c -- u )   1 swap lshift ;  \ takes 2 to the power c 

09 2** cell+ Buffer: BIG_STRBUF

((
\ Note that this form does not handle the corner case  s" ABCD" 2dup over BigPlace 1 cells + dump
: BigPlace_notWB ( a n a -- )
   2dup !  cell+  swap  cells  move
;
))
\ from Wil Baden
\ Note that this form handles the corner case  s" ABCD" 2dup over BigPlace 1 cells + dump
: BigPlace ( a n a -- )
    2dup 2>r  cell+ swap cells move  2r> ! 
;

: BigCount ( a -- a n )
   dup @ swap cell+ swap
;

: BigAppend ( a n a -- )
   2dup 2>r  BigCount + swap move  2r> +! 
;

((
\ Note : BigSUBST does not work...
: BigSUBST ( source len old len new len -- result len )
   LOCALS| nl n pl p |   0 BIG_STRBUF !
   BEGIN   DUP 0> WHILE
      2DUP p pl SEARCH WHILE ( s l m l)
         ROT OVER - >R ROT R> BIG_STRBUF BigAppend
         n nl BIG_STRBUF BigAppend
         pl /STRING
   REPEAT 2SWAP BIG_STRBUF BigAppend THEN
   2DROP BIG_STRBUF BigCount ;

: BigREPLACE ( new l source l old l -- result l )
   DUP>R  2OVER 2>R
   SEARCH IF    ( new l a n)
      DUP 2R> ROT - BIG_STRBUF BigPlace
      R> /STRING  2SWAP BIG_STRBUF BigAppend  BIG_STRBUF BigAppend
   ELSE         ( new l a n)
      2R> BIG_STRBUF BigPlace  R>DROP 2DROP 2DROP
   THEN  BIG_STRBUF BigCount ;

: ttBR ( -- )
   s" totally wonderful"  s" hello replaceme world!!!" cr 2dup type
   s" replaceme" BigREPLACE  cr type
;

: ttSUBST ( -- )
   s" totally wonderful"  s" Forth is replaceme! Hello replaceme world!!!" cr 2dup type
   s" replaceme" BigSUBST  cr type
;
))

\ *****************************************************************************

\ Based on : c.l.f https://groups.google.com/g/comp.lang.forth/c/SRLOo2Tzjyg
\ the.bee...@gmail.com
\ unread,
\ Nov 12, 2021, 6:21:07 PM (3 days ago) 
\ to
\ An old library, haven't used it in ages. Your mileage may vary.

\ Delete string $2 from string $1
\ Return the resulting string in $1' and the remainder of the
\ line after the BigDelete in string $2'
\ If the deletion was made, f is 1 otherwise 0

: BigDelete ( $1 $2 -- $1' $2' f )  \ delete $2 from $1, remainder in $2', f is false if $2 was not found
    2>r 2dup 2r> dup >r search
    dup r> swap >r >r ( $1 $2 f )
    if ( $1 $ 2)
    2swap r@ - 2swap r@ - ( a1 n3 a2 n4 )
    2dup over r@ chars + ( a1 n3 a2 n4 a3 )
    -rot cmove ( a1 n3 a2 n4 )
    then
    r> drop r>
;

\ Delete all occurences of string $2 in string $1
\ Return the resulting string in a1/n3
: BigDeleteAll ( $1 $2 -- $1' )   \ delete all $2 from $1
    2>r swap dup rot ( a1 $1)
    begin ( a1 $1)
    2r> 2dup 2>r BigDelete ( a1 a1 n3 a2 n4 f)
    while ( a1 a1 n3 a2 n4)
    2swap 2drop ( a1 a2 n4)
    repeat ( a1 a2 n4)
    2drop chars + over - ( a1 n5)
    2r> 2drop ( a1 n5)
;

\ Spread string $1 at position n2 by n3 characters
\ Return the resulting string in $1' and the opened up
\ space in string $2'
: BigSpread ( $1 n2 n3 -- $1' $2' )    \ insert n3 characters into $1 at position n2
    rot >r >r ( a1 n2 )
    over over chars + swap ( a1 $2 )
    over dup r@ + rot ( a1 a2 a2 a3 n2 )
    r> swap r@ swap - swap >r ( a1 a2 a2 a3 n1-n2 )
    move r@ rot r> r> + ( a2 n3 a1 n4 )
    2swap ( a1 n4 a2 n3 )
;

\ Insert string $2 into string $1 at position n
\ Return the resulting string in $1' and the remainder of
\ the line after the insertion in $2'
: BigInsert ( $1 $2 n -- $1' $2' )
    -rot 2dup 2>r ( $1 n3 $2 )
    nip BigSpread ( a1 n3 a3 n2 )
    over 2r> ( a1 n3 a3 n2 a3 $2 )
    rot swap cmove chars + ( a1 n3 a4 )
    >r 2dup chars + r@ - ( a1 n3 n4 )
    r> swap
;

\ Replace string $2 in string $1 by string $3
\ Return the resulting string in $1' and the remainder of the
\ line after the replacement in string $4
\ If the replacement was made, f is 1 otherwise 0
: BigReplace() ( $1 $2 $3 -- $1' $4 f )   \ in $1 replace $2 by $3, remainder in $4, f is false if $2 was not found
   2>r 
   BigDelete 
   dup 2r> 
   rot >r 
   2>r
   if
      nip over swap - 2r> rot BigInsert
   else
      2r> 2drop
   then
   r>
;

: BigReplace ( $1 $2 $3 -- $1' )
   BigReplace()  drop 2drop
;


\ Replace all occurences of string $2 in string $1 by string $3
\ Return the resulting string in a1/n4
: BigReplaceAll ( $1 $2 $3 -- $1' )    \ in $1 replace all $2 by $3
  2>r 2>r   \ move $3 and $2 onto the return stack 
  swap dup rot    \ leave the length of $1, then the 
  begin 
    2r> 2dup 2r> 2dup 2>r 2swap 2>r BigReplace() 
  while
    2swap 2drop
  repeat
  2drop chars + over -
  2r> 2r> 2drop 2drop   \ clean up the return stack
;
\ Hans Bezemer

\ *****************************************************************************
\ Tests
\ *****************************************************************************
$100 Buffer: MyBuffer1 \ for testing - must be big enough to hold the string after replacing
$100 Buffer: MyBuffer2 \ for testing - must be big enough to hold the string after replacing
$100 Buffer: MyBuffer3 \ for testing - must be big enough to hold the string after replacing

\ a version of type that displays control characters as '.'s, and shows the length of the string
: type. ( $ -- ) 
   2dup swap ." args = " . . 0 max $100 min
   dup 2 u.r ."  | " 
   over + swap ?do  i c@ dup $20 < if  drop  [char] . then  emit  loop 
;

\ : BigDelete ( $1 $2 -- $1' $2' f )  \ delete $2 from $1, remainder in $2', f is false if $2 was not found
: tt_BigDelete ( -- )
   cr ." BigDelete 'NOT ' "
   s" I will NOT do it!" 
   MyBuffer1 place  MyBuffer1 count    \ copy the compiled string to a bigger buffer     
   cr ."    was     : " 2dup type.
   s" NOT " 
   BigDelete 
   if  cr ."    *** deleted *** "  then
   2>r 
   cr ."    is      : " type.
   2r> 
   cr ."    remain  : " type.
   cr
;

\ : BigDeleteAll ( $1 $2 -- $1' )   \ delete all $2 from $1
: tt_BigDeleteAll ( -- )
   cr ." BigDeleteAll all 'NOT 's "
   s" I will NOT NOT NOT do it! And I mean it NOT !!!" 
   MyBuffer1 place  MyBuffer1 count    \ copy the compiled string to a bigger buffer     
   cr ."    was     : " 2dup type.
   s" NOT " 
   BigDeleteAll 
   cr ."    is      : " type.
   cr
;

\ : BigSpread ( $1 n2 n3 -- $1' $2' )    \ insert n3 characters into $1 at position n2
: tt_BigSpread ( -- )
   cr ." BigSpread 4 chars at position 7 "
   MyBuffer1 $100 erase
   s" I will do it!" 
   MyBuffer1 place  MyBuffer1 count    \ copy the compiled string to a bigger buffer     
   cr ."    marker       | 012345678901234567890123456789"
   cr ."    was     : " 2dup type.
   7 4 BigSpread   
   2swap
   cr ."    is      : " type.
   cr ."    gap     : " type. 
   cr
;

\ : BigInsert ( $1 $2 n -- $1' $2' )
: tt_BigInsert ( -- )
   cr ." BigInsert 'NOT ' at position 7 "
   s" I will do it!" 
   MyBuffer1 place  MyBuffer1 count    \ copy the compiled string to a bigger buffer 
   cr ."    marker       | 012345678901234567890123456789"
   cr ."    was     : " 2dup type.   
   s" NOT " 7 BigInsert 
   2swap
   cr ."    is      : " type.
   cr ."    remain  : " type. 
   cr
;

\ : BigReplace() ( source$ old$ new$ -- result$ remainder$ f )   \ in source$ replace old$ by new$, result in result$, remainder in remainder$ f is true if $old was found
: tt_BigReplace() ( -- )
   cr ." BigReplace just one 'replaceme' by 'totally wonderful'"
   s" Forth is replaceme , hello replaceme world." 
   MyBuffer1 place  MyBuffer1 count    \ copy the compiled string to a bigger buffer
   cr ."    was     : "  2dup type.
   s" replaceme"  
   MyBuffer2 place  MyBuffer2 count    \ copy the compiled string to a bigger buffer
   s" totally wonderful"  
   MyBuffer3 place  MyBuffer3 count    \ copy the compiled string to a bigger buffer
   BigReplace()  
   if  cr ."    *** replaced *** "  then
   2swap
   cr ."    is      : " type.
   cr ."    remain  : " type.  
   cr
;

\ : BigReplace ( $1 $2 $3 -- $1' )   \ in $1 replace $2 by $3, remainder in $1'
: tt_BigReplace ( -- )
   cr ." BigReplace just one 'replaceme' by 'totally wonderful'"
   s" Forth is replaceme , hello replaceme world." 
   MyBuffer1 place  MyBuffer1 count    \ copy the compiled string to a bigger buffer
   cr ."    was     : "  2dup type.
   s" replaceme"  
   MyBuffer2 place  MyBuffer2 count    \ copy the compiled string to a bigger buffer
   s" totally wonderful"  
   MyBuffer3 place  MyBuffer3 count    \ copy the compiled string to a bigger buffer
   BigReplace  
   cr ."    is      : " type.
   cr
;

\ : BigReplaceAll ( $1 $2 $3 -- $1' )    \ in $1 replace all $2 by $3
: tt_BigReplaceAll ( -- )
   cr ." BigReplaceAll all 'replaceme's by 'totally wonderful'"
   s" Forth is replaceme , hello replaceme world." 
   MyBuffer1 place  MyBuffer1 count    \ copy the compiled string to a bigger buffer  
   cr ."    was     : "  2dup type.
   s" replaceme"  s" totally wonderful"  BigReplaceAll  
   cr ."    is      : " type.
   cr
;

: tt_All ( -- )
    page
    cr ." BigStrings.f  tests"
    cr
\   cr ." BigPlace corner case : "  s" ABCD" 2dup over BigPlace_notWB 1 cells + dump
    cr ." BigPlace corner case : "  s" ABCD" 2dup over BigPlace 1 cells + dump 
    cr
    tt_BigDelete
    tt_BigDeleteAll
    tt_BigSpread
    tt_BigInsert
    tt_BigReplace
    tt_BigReplaceAll
    cr
;

((
: BigReplaceAllInBuffer ( $find $replace -- )
    2>r 2>r  MyFileBuffer[] |MyFileBuffer|  2r> 2r>  BigReplaceAll  2drop
;

: MakeAllChanges ( -- )
    \ Note : the order is important, because if "replaceme" has already been replaced "replaceme2" will not be found
    s" replaceme2"  s" totally wonderful2"  BigReplaceAllInBuffer
    s" replaceme"   s" totally wonderful"   BigReplaceAllInBuffer
;

: RevertAllChanges ( -- )
    \ Note : the order is important, because if "replaceme" has already been replaced "replaceme2" will not be found
    s" totally wonderful2" s" replaceme2"  BigReplaceAllInBuffer
    s" totally wonderful"  s" replaceme"   BigReplaceAllInBuffer
;

: FileReplace ( $filename -- )
    ReadMyFile

    MakeAllChanges
    \ RevertAllChanges
    cr  MyFileBuffer[] |MyFileBuffer|  drop $200 type+

    WriteBackMyFile
;

: tt_FileReplace ( -- )
    s" FileReplaceTestFile.c" ['] FileReplace catch if  2drop then
    s" FileReplaceTestFile.h" ['] FileReplace catch if  2drop then
;

))

