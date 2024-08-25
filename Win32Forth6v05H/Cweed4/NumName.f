\ NumName.f Howerd Oakford V1.0 2021 Nov 11  Win32Forth version  www.inventio.co.uk
\ Display a 15 bit number (from 0 to 32767) as an English word, and two 15 bit numbers as a pair of words.
\ The two-word phrase calculated from the checksum or hash of two different files has a roughly 1 in a billion chance of being the same
\ This allows two files in different locations and/or times to be compared by human beings...
\ This code requires the file NumName.txt in the same folder
\ 
\ The words are ordered by size, and roughly alphabetically. 
\ Using up to eight characters gives over 32768 words, but going up to 65536 is not practical without using long or obscure words.
\ I have tried to use only well-known words, proper names and country & capital city names.
\ Homophones ( e.g. caught and court, there and their etc ) have been removed, as have rude or offensive words (hopefully).
\ If you find any that I have missed, please let me know : howerd@inventio.co.uk :-)

decimal
32768 constant NUM_OF_NAMES

: MyFileName ( -- $ )   s" NumName.txt" ;

variable ReadFileID
0 ReadFileID !

: OpenReadFile 
   MyFileName r/o open-file 
   if ." Couldn't open read file "  MyFileName type space  2001 throw  then
   ReadFileID !
;

0x100 constant |ReadLineBuffer| 
variable ReadLineBuffer    |ReadLineBuffer| ALLOT 

: ReadOneLine ( -- )    \ into the buffer
   ReadFileID @ 0= if  OpenReadFile  then
   ReadLineBuffer 1+ |ReadLineBuffer| ReadFileID @ Read-line abort" could not ReadOneLine from file"
   drop
   ReadLineBuffer c! \ count
;

: CloseReadFile ( -- )  
   ReadFileID @ CLOSE-FILE drop 
;

: STRING,noalign ( a n -- )  
\   2drop exit
   HERE >R DUP C,
   DUP ALLOT
   R> 1+ SWAP MOVE
\   0 C,
\   ALIGN
;

: CreateNameTable
   ( -- )
   OpenReadFile
   
   NUM_OF_NAMES 10 + 0 do
      ReadOneLine
      ReadLineBuffer count  
\       cr  2dup type
\      STRING,noalign
      dup c, over + swap ?do  i c@ c,  loop 
   loop  
   CloseReadFile
;

\ here
create NumNameStartAddress   CreateNameTable
\ here swap cr .( CreateNameTable size = )  - .

: NumName ( n -- c-addr n )
   1+  \ skip over the first line - it is the version info for  NumName.txt  
   NumNameStartAddress swap 0 ?do  count +  loop
   count 
;
 
variable NumNameBuffer  32 ALLOT 

: NumNames ( u -- )
   >r
\   s" 1-" NumNameBuffer place    \ the NumName.txt file version - I am assuming changes will be made, but then again, maybe not...
   r@ 16 rshift 0x00007FFF and NumName NumNameBuffer place \ append
   s" -" NumNameBuffer append
   r> 0x00007FFF and NumName NumNameBuffer append  
;

: .NumNames ( u -- )   NumNames NumNameBuffer count dup >r type  19 r> - 0 max  spaces ;


\ *****************************************************************************
\ tests
\ *****************************************************************************

\ check that there are no 0 length strings
: ttNumName ( -- n )
   NumNameStartAddress 
   40000 0 do  
      count dup 0= if  cr i .  i 32768 1+  < if  ."   Ooops - missing line!!! "  then  leave  then
      +   
   loop
   2drop
;

: ttnn ( u -- )   
   base @ >r 
   cr dup hex 8 u.r  2 spaces  .NumNames 
   r> base ! 
;

: ttnns 
   cr ." ttnns"
   base @ >r   hex
   cr -1 NumName type      \ the version line form the file
   20 0        do  cr i 8 u.r 2 spaces  i NumName type  loop
   32780 32760 do  cr i 8 u.r  2 spaces i NumName type  loop
   cr
   0x00000000 ttnn
   0x00000001 ttnn
   0x00000002 ttnn
   0x00000003 ttnn
   0x00008001 ttnn
   0x00010001 ttnn
   0x7FFF7FFE ttnn
   0x7FFF7FFF ttnn
   r> base ! 
;
