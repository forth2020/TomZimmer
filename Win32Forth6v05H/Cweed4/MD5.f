\ *****************************************************************************
\ *S md5.f V1.0  Howerd Oakford 2010 Mar 28
\ ** The MD5 secure hash algorithm
\ ** 32 bit little endian ANS Forth version for the PC.
\ ** The known answers are from hashcalc.exe 
\ ** available from http://www.slavasoft.com/hashcalc/
\ ** and were checked using the Java applet at http://pajhome.org.uk/crypt/md5/
\ ** and http://www.fileformat.info/tool/hash.htm
\ ** - thanks!
\ ** Tested on SwiftForth, Win32Forth and VFXForth.
\ ** Added WEP PassPhrase to hex string conversion program
\ *****************************************************************************

\ *************************************************************
\ Bug notice 2006 Jul 01 :
\ All my previous versions of md5.f and md5.fth have a bug :
\ The "55 >" was incorrectly " 1+ $38 < 0=" and gave an 
\ incorrect hash for strings of length 55 ( modulo 64 ). 
\ *************************************************************

\ : BE! ( x addr -- )     \ BigEndian !  for LittleEndian processors
\    over $0000000FF and             over 3 + c!
\    over $00000FF00 and $08 Rshift  over 2 + c!
\    over $000FF0000 and $10 Rshift  over 1 + c!
\    swap $0FF000000 and $18 Rshift  swap     c! ;

: BE@ ( addr -- x )     \ BigEndian @ for LittleEndian processors
   dup  3 + c@
   over 2 + c@ $08 Lshift or
   over 1 + c@ $10 Lshift or
   swap     c@ $18 Lshift or
;

Create SineTable  hex
\ *G The table of y = ( 2** 32 ) * sin[ x ]
\ ** where x goes from 1 to 64 radians.
(  1 ) D76AA478 , E8C7B756 , 242070DB , C1BDCEEE ,
(  5 ) F57C0FAF , 4787C62A , A8304613 , FD469501 ,
(  9 ) 698098D8 , 8B44F7AF , FFFF5BB1 , 895CD7BE ,
( 13 ) 6B901122 , FD987193 , A679438E , 49B40821 ,
( 17 ) F61E2562 , C040B340 , 265E5A51 , E9B6C7AA ,
( 21 ) D62F105D , 02441453 , D8A1E681 , E7D3FBC8 ,
( 25 ) 21E1CDE6 , C33707D6 , F4D50D87 , 455A14ED ,
( 29 ) A9E3E905 , FCEFA3F8 , 676F02D9 , 8D2A4C8A ,
( 33 ) FFFA3942 , 8771F681 , 6D9D6122 , FDE5380C ,
( 37 ) A4BEEA44 , 4BDECFA9 , F6BB4B60 , BEBFBC70 ,
( 41 ) 289B7EC6 , EAA127FA , D4EF3085 , 04881D05 ,
( 45 ) D9D4D039 , E6DB99E5 , 1FA27CF8 , C4AC5665 ,
( 49 ) F4292244 , 432AFF97 , AB9423A7 , FC93A039 ,
( 53 ) 655B59C3 , 8F0CCC92 , FFEFF47D , 85845DD1 ,
( 57 ) 6FA87E4F , FE2CE6E0 , A3014314 , 4E0811A1 ,
( 61 ) F7537E82 , BD3AF235 , 2AD7D2BB , EB86D391 ,
decimal

variable NumBytes
\ *G the number of bytes processed

variable MD5pad $40 allot
\ *G the buffer used to accumulate up to 64 bytes

variable MD5padPtr 
\ *G pointer into the MD5pad

: MD5padC! ( c)  
\ *G store byte c into the MD5 pad at the next free location
   MD5pad MD5padPtr @ $3F and + C!  1 MD5padptr +! ;

variable SineTablePtr
\ *G a counter to select table entries 

variable RotatePtr
\ *G a counter to select one of four rotation values

variable MD5hash $10 allot
\ *G the MD5 result array
\ ** An initial value is put in here which is mangled by the message to
\ ** give a one-way function MD5 secure hash key.
MD5hash $00 + constant MD5[a]      \ accessed by name
MD5hash $04 + constant MD5[b]
MD5hash $08 + constant MD5[c]
MD5hash $0C + constant MD5[d]

: Lrotate                \ u c -- u
\ *G cyclicly rotate the 32 Bit word u left c bits
\ ** i.e put the MSB into the LSB when it drops off the left hand end.
   2dup Lshift >r 32 swap - Rshift  r> or
;

: RotateValue ( u a - u' )   
\ *G rotate the top of stack
   RotatePtr @ 3 and + c@ Lrotate  1 RotatePtr +! 
;

: AddSineTableValue ( u - u')   
\ *G add in the table value
   SineTable SineTablePtr @ + @  +  cell SineTablePtr +! 
;

: RotateHash ( u c - u')    
\ *G move the four cells of the has around and add in u
   MD5[b] @ + >r
   MD5[d] @ 
   MD5[c] @ MD5[d] !
   MD5[b] @ MD5[c] !
\  MD5[a] @ MD5[b] !  \ overwritten 2 lines below :
   MD5[a] !
   r> MD5[b] !
;

Create FFrotate $07 C, $0C C, $11 C, $16 C,
\ *G lists the four possible rotate values for this function

: MD5-FF ( c - c')
\ *G takes 4 byte value k of the message and mangles it
\ ** into the hash value using function FF.
   dup $04 + $3F and swap MD5pad + @

   MD5[c] @   MD5[b] @ and
   MD5[d] @   MD5[b] @ invert   and
   or  MD5[a] @ + 
   +   
   AddSineTableValue
   FFrotate RotateValue
   RotateHash
;

Create GGrotate $05 C, $09 C, $0E C, $14 C,
\ *G lists the four possible rotate values for this function

: MD5-GG ( c - c')
\ *G MD5-GG  takes 4 byte value k of the message and mangles it
\ ** into the hash value using function GG.
   dup $14 + $3F and swap MD5pad + @  
   MD5[b] @  MD5[d] @ and
   MD5[c] @  MD5[d] @ invert and
   or  MD5[a] @  +
   +
   AddSineTableValue
   GGrotate RotateValue
   RotateHash
;

Create HHrotate $04 C, $0B C, $10 C, $17 C,
\ *G lists the four possible rotate values for this function

: MD5-HH ( c - c')
\ *G MD5-HH  takes 4 byte value k of the message and mangles it
\ ** into the hash value using function HH.
   dup $0C + $3F and swap MD5pad + @
   MD5[b] @  MD5[c] @  MD5[d] @  xor  xor
   MD5[a] @  +
   +
   AddSineTableValue
   HHrotate RotateValue
   RotateHash
;

Create IIrotate 6 C, 10 C, 15 C, 21 C,
\ *G lists the four possible rotate values for this function

: MD5-II ( c - c')
\ *G takes 4 byte value k of the message and mangles it
\ ** into the hash value using function II.
   dup $1C + $3F and  swap MD5pad + @
   MD5[b] @  MD5[d] @ invert  or
   MD5[c] @  xor
   MD5[a] @  +
   +
   AddSineTableValue
   IIrotate RotateValue
   RotateHash
;

: MD5block   
\ *G processes a 64 byte block of the message in MD5pad
   0 SineTablePtr !
   0 RotatePtr !
   16 0 do  MD5hash 12 i - + @  cell +loop \ push the current hash onto the stack
   0 cells  16 0 do  MD5-FF  loop  drop   \ round 1 start at 0, add 1 each time
   1 cells  16 0 do  MD5-GG  loop  drop   \ round 2 start at 1, add 5 each time
   5 cells  16 0 do  MD5-HH  loop  drop   \ round 3 start at 5, add 3 each time
   0 cells  16 0 do  MD5-II  loop  drop   \ round 4 start at 0, add 7 each time
   16 0 do  MD5hash i + +!  cell +loop    \ add in the saved hash
   MD5pad $40 erase
   0 MD5padPtr !
;

: MD5init 
\ *G puts the initial values into the MD5hash array as specified by the RFC
   $67452301 MD5[a] !
   $EFCDAB89 MD5[b] !
   $98BADCFE MD5[c] !
   $10325476 MD5[d] !
   0 MD5padPtr !
   0 NumBytes !
   MD5pad $40 erase
;

\ *******************************
\ *S one byte at a time interface 
\ *******************************

: MD5byte ( c)
\ *G add one byte to the MD5 hash
   1 NumBytes +!
   MD5pad 
   MD5padPtr @ $3F and + C!  
   1 MD5padptr +! 
   MD5padPtr @ $40 = if
      MD5block
      0 MD5padPtr !
   then
;

: MD5final
\ *G processes the final part of the message
   MD5padPtr @ $3F and >r 
   $80 r@ ( u ) MD5pad + c!
   r> ( u ) 55 > if    \ padding will exceed block
      MD5block
   then
   NumBytes @ 8 UM* swap MD5pad 56 + 2! 
   MD5block
;

: MD5 ( a n -- )   
\ *G calculate the MD5 hash of n bytes at address a 
\ ** the result is in MD5hash 
\ ** Use this as a template - put the three MD5 functions into your code...
   MD5init  
   over + swap ?do  i c@ MD5byte  loop  
   MD5final 
;

\ *******************
\ *S Array interface
\ *******************

\ : /STRING ( a n n2 - a n )
\ \ *G removes n2 bytes from the start of string a n
\    >r r@ - 0 max  swap r> + swap
\ ;

: MD5finalArray ( a n -- )  \ Note that n < 64
\ *G processes the final part of the message
   dup NumBytes +!
   ( c-addr u ) >r  
   MD5pad r@ cmove
   $80 r@ ( u ) MD5pad + c!
   r> ( u ) 55 > if    \ padding will exceed block
      MD5block
   then
   NumBytes @ 8 UM* swap MD5pad 56 + 2! 
   MD5block
;

: MD5array ( a n -- ) 
\ *G calculate the MD5 hash of n bytes at address a 
\ ** the result is in MD5hash 
\ Use this if your data is already in an array
   MD5init
   begin                   \ process 64 bytes at a time
      dup 64 < 0=
   while                   
      over                 
      MD5pad $40 move  MD5block  \ process 64 bytes of the input string
      $40 NumBytes +!
      $40 /STRING          \ remove the first 64 bytes from the string
   repeat                  \ c-addr u ; process the remainder of the input
   MD5finalArray           \ process the remainder of the input string
;

\ ******************
\ *S File interface 
\ ******************

0 value FileID
variable MyByte

: FileC@ ( - c)   
\ *G fetch onebyte from the file with handle FileID
   FileID 0= abort" No file opened! " 
   MyByte 1 FileID READ-FILE abort" Could not read file! "
   1 = not abort" End of file! "
   MyByte c@
;

: .MD5hash                
\ *G displays the MD5 hash result array, with spaces
   cr  base @ >r  hex
   MD5hash $10 over + swap do  I c@ 3 U.R  loop  
   r> base ! 
;

: (MD5file) ( c-addr n -- )   
\ *G display the MD5 hash of the file with name NumBytes n at address a
   R/O OPEN-FILE abort" Could not open file! " to FileID
   MD5init  
   FileID FILE-SIZE abort" Could not get file size! " drop
   0 ?do  FileC@ MD5byte  loop
   FileID CLOSE-FILE abort" Could not close the file! "
   MD5final
   .MD5hash
;  

: MD5file"   
\ *G display the MD5 hash of the file whose name follows, terminated by "
   [char] " 
   word count (MD5file)
;

: MD5file   
\ *G display the MD5 hash of the file whose name follows
   32 word count (MD5file)
;

\ *******************************************************************************
\ *S Test functions
\ ** The known answers are from hashcalc.exe 
\ ** available from http://www.slavasoft.com/hashcalc/
\ ** and were checked using the Java applet at http://pajhome.org.uk/crypt/md5/
\ *******************************************************************************

: MD5hash>stack   \ -- a b c d
\ *G get the MD5 data in the local endian format in BigEndian
   MD5[a] BE@
   MD5[b] BE@
   MD5[c] BE@
   MD5[d] BE@
;

: MD5hash>$ ( -- c-addr n)
\ *G fetches the MD5 hash result from the array and formats it as a string.
\ ** Note that the string is NOT in LittleEndian format.
\ ** It is in the same format as the test strings...
   base @ >r hex
   MD5hash>stack 0 0
   <#   4 0 do  2drop 0  # # # # # # # #  loop  #>
    r> base !
;

: .MD5          
\ *G displays the MD5 hash result array, without spaces
   MD5hash>$ type ;

0 [if]
\ *******************************************************************************
\ Known Answer Tests
\ *******************************************************************************

\ *******************************************************************************
\ report result - 0 is OK
\ *******************************************************************************

variable NumErrors

: .Pass/Fail ( f -- )   
   if  
      1 NumErrors +!  ."  FAILED <<<<<<<<<<<<<<<< "  
   else  
      ."  passed "  
   then 
;

\ *******************************************************************************
\ WEP PassPhrase to hex string converter
\ Many Wireless LAN ( WLAN ) routers offer a short ASCII string
\ to hex string conversion facility.
\ This works for LinkSys and some other WLAN routers using 104 bit WEP - YMMV
\ Checked using http://www.wepkey.com/ - thanks!
\ *******************************************************************************

\ $40 Buffer: PassPhraseString
variable PassPhraseString $40 cell - allot 

: PassPhraseToHex (  c-addr n -- c-addr n )
   0 max  $40 min  >r   \ save the length of the ASCII PassPhrase
   \ copy the PassPhrase to the start of the buffer
   PassPhraseString r@ cmove   
   cr ." The given ASCII PassPhrase = " PassPhraseString r@ type
   \ copy the given string repeatedly to fill the buffer
   PassPhraseString   PassPhraseString r@ +   $40 r> - cmove
   cr ." repeated to fill 64 bytes  = " PassPhraseString $40 type 
   \ take its MD5 hash
   PassPhraseString $40 MD5  
   cr ." Full MD5 HEX string result = "  MD5hash>$ 2dup type
   cr ." LinkSys is shortened to    = "  6 - 2dup type
;  

: TestP2H   s" password"  PassPhraseToHex s" 2B204A3F1042643E480FDD655E" compare 0= ;

: ShowResult
\ *G show the number of errors
   cr  NumErrors @ if  
      ." !!!!! Tests failed with " NumErrors @ .  ." error"   
      NumErrors @ 1 = not if  ." s"  then  ."  !!!!! "  
   else
      ." All tests passed "  then
   cr
;

: MD5test ( a n a' n')
\ *G takes a string and its pre-calculated MD5 hash,
\ ** and compares this to its own calculation.
  cr  2>r  2dup type  ."  -> "
  MD5 MD5hash>$  2dup type
\  MD5array MD5hash>$  2dup type
  2r>
  compare .Pass/Fail

;

variable NULL$  0 NULL$ !
\ *G a null string

: MD5tests                 \ --
\ *G runs a standard set of tests to verify the MD5 program
   cr ." MD5 tests : " 
   0 NumErrors !
   NULL$ 0  S" D41D8CD98F00B204E9800998ECF8427E" MD5test
   S" a"  S" 0CC175B9C0F1B6A831C399E269772661" MD5test
   S" abc"  S" 900150983CD24FB0D6963F7D28E17F72" MD5test
   S" message digest"
   S" F96B697D7CB7938D525A2F31AAF161D0" MD5test
   S" abcdefghijklmnopqrstuvwxyz"
   S" C3FCD3D76192E4007DFB496CCA67E13B" MD5test
   S" ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
   S" D174AB98D277D9F5A5611C2C9F419D9F" MD5test
   S" 12345678901234567890123456789012345678901234567890123456789012345678901234567890"
   S" 57EDF4A22BE3C955AC49DA2E2107B67A" MD5test
   S" A"  S" 7FC56270E7A70FA81A5935B72EACBE29" MD5test
   S" AA"  S" 3B98E2DFFC6CB06A89DCB0D5C60A0206" MD5test
   S" AAA"  S" E1FAFFB3E614E6C2FBA74296962386B7" MD5test
   S" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"  
      S" DE05237F7D3965E0B33351893D23E05E" MD5TEST \ 54

   S" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"  
      S" E38A93FFE074A99B3FED47DFBE37DB21" MD5TEST \ 55 - see the bug report above...

   S" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"   
      S" A2F3E2024931BD470555002AA5CCC010" MD5TEST \ 56 
   S" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"   
      S" A2F3E2024931BD470555002AA5CCC010" MD5test \ 57 
   S" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"   
      S" 9A7C38569E5A96E3CFBAD45FB9CE5209" MD5test \ 58 
   S" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"   
      S" EF843F60078DD0D52413DD05309F8503" MD5test \ 59 
   S" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"   
      S" B0B5E976F4E7E61B01F13817AAF7DA7E" MD5test \ 60 
   S" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"   
      S" E009747E74DD24F3274FC71C240921B7" MD5test \ 61 
   S" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"   
      S" 14259830F67657A39CB0BDF5D6BB4E4B" MD5test \ 62 
   S" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"   
      S" A5446E80ABD7C822BF6A154887CAEA36" MD5test \ 63 
   S" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"   
      S" 5F1C4BB2970471A5C75B7BA1DC9EE3ED" MD5test \ 64 
   S" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"   
      S" D289A97565BC2D27AC8B8545A5DDBA45" MD5test \ 65 
   S" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"   
      S" 162B6D6EB17CD9DA55F95F8C73A32DDA" MD5test \ 66 
   S" http://www.slavasoft.com/hashcalc/"   
      S" 1747B749FF64A6717A0C92930894ED52" MD5test \ 66      
   ShowResult
;

MD5tests

[then]