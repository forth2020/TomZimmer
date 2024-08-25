\ *************************************************************************** 
\ crc32.f 2006 May 25 Howerd Oakford, 
\ based on code by Petrus Prawirodidjojo and Wil Baden - thanks!
\ 
\ The International Standard 32-bit cyclical redundancy check defined by :
\ [ITU-T-V42] International Telecommunications Union, "Error-correcting
\ Procedures for DCEs Using Asynchronous-to-Synchronous Conversion",
\ ITU-T Recommendation V.42, 1994, Rev. 1.
\ and
\ [ISO-3309]
\ International Organization for Standardization,
\ "Information Processing Systems--Data Communication High-Level Data Link
\ Control Procedure--Frame Structure", IS 3309, October 1984, 3rd Edition.
\ 
\ This is a 32 bit Little Endian ANS Forth version ( PC ).
\ *************************************************************************** 

\ CRC-32 with polynomial $04c11db7, as specified in IEEE 802.3 ( Ethernet )

Create CRCtable \ created by ShowCRCtable below
 $000000000 , $077073096 , $0EE0E612C , $0990951BA , $0076DC419 , $0706AF48F , $0E963A535 , $09E6495A3 ,
 $00EDB8832 , $079DCB8A4 , $0E0D5E91E , $097D2D988 , $009B64C2B , $07EB17CBD , $0E7B82D07 , $090BF1D91 ,
 $01DB71064 , $06AB020F2 , $0F3B97148 , $084BE41DE , $01ADAD47D , $06DDDE4EB , $0F4D4B551 , $083D385C7 ,
 $0136C9856 , $0646BA8C0 , $0FD62F97A , $08A65C9EC , $014015C4F , $063066CD9 , $0FA0F3D63 , $08D080DF5 ,
 $03B6E20C8 , $04C69105E , $0D56041E4 , $0A2677172 , $03C03E4D1 , $04B04D447 , $0D20D85FD , $0A50AB56B ,
 $035B5A8FA , $042B2986C , $0DBBBC9D6 , $0ACBCF940 , $032D86CE3 , $045DF5C75 , $0DCD60DCF , $0ABD13D59 ,
 $026D930AC , $051DE003A , $0C8D75180 , $0BFD06116 , $021B4F4B5 , $056B3C423 , $0CFBA9599 , $0B8BDA50F ,
 $02802B89E , $05F058808 , $0C60CD9B2 , $0B10BE924 , $02F6F7C87 , $058684C11 , $0C1611DAB , $0B6662D3D ,
 $076DC4190 , $001DB7106 , $098D220BC , $0EFD5102A , $071B18589 , $006B6B51F , $09FBFE4A5 , $0E8B8D433 ,
 $07807C9A2 , $00F00F934 , $09609A88E , $0E10E9818 , $07F6A0DBB , $0086D3D2D , $091646C97 , $0E6635C01 ,
 $06B6B51F4 , $01C6C6162 , $0856530D8 , $0F262004E , $06C0695ED , $01B01A57B , $08208F4C1 , $0F50FC457 ,
 $065B0D9C6 , $012B7E950 , $08BBEB8EA , $0FCB9887C , $062DD1DDF , $015DA2D49 , $08CD37CF3 , $0FBD44C65 ,
 $04DB26158 , $03AB551CE , $0A3BC0074 , $0D4BB30E2 , $04ADFA541 , $03DD895D7 , $0A4D1C46D , $0D3D6F4FB ,
 $04369E96A , $0346ED9FC , $0AD678846 , $0DA60B8D0 , $044042D73 , $033031DE5 , $0AA0A4C5F , $0DD0D7CC9 ,
 $05005713C , $0270241AA , $0BE0B1010 , $0C90C2086 , $05768B525 , $0206F85B3 , $0B966D409 , $0CE61E49F ,
 $05EDEF90E , $029D9C998 , $0B0D09822 , $0C7D7A8B4 , $059B33D17 , $02EB40D81 , $0B7BD5C3B , $0C0BA6CAD ,
 $0EDB88320 , $09ABFB3B6 , $003B6E20C , $074B1D29A , $0EAD54739 , $09DD277AF , $004DB2615 , $073DC1683 ,
 $0E3630B12 , $094643B84 , $00D6D6A3E , $07A6A5AA8 , $0E40ECF0B , $09309FF9D , $00A00AE27 , $07D079EB1 ,
 $0F00F9344 , $08708A3D2 , $01E01F268 , $06906C2FE , $0F762575D , $0806567CB , $0196C3671 , $06E6B06E7 ,
 $0FED41B76 , $089D32BE0 , $010DA7A5A , $067DD4ACC , $0F9B9DF6F , $08EBEEFF9 , $017B7BE43 , $060B08ED5 ,
 $0D6D6A3E8 , $0A1D1937E , $038D8C2C4 , $04FDFF252 , $0D1BB67F1 , $0A6BC5767 , $03FB506DD , $048B2364B ,
 $0D80D2BDA , $0AF0A1B4C , $036034AF6 , $041047A60 , $0DF60EFC3 , $0A867DF55 , $0316E8EEF , $04669BE79 ,
 $0CB61B38C , $0BC66831A , $0256FD2A0 , $05268E236 , $0CC0C7795 , $0BB0B4703 , $0220216B9 , $05505262F ,
 $0C5BA3BBE , $0B2BD0B28 , $02BB45A92 , $05CB36A04 , $0C2D7FFA7 , $0B5D0CF31 , $02CD99E8B , $05BDEAE1D ,
 $09B64C2B0 , $0EC63F226 , $0756AA39C , $0026D930A , $09C0906A9 , $0EB0E363F , $072076785 , $005005713 ,
 $095BF4A82 , $0E2B87A14 , $07BB12BAE , $00CB61B38 , $092D28E9B , $0E5D5BE0D , $07CDCEFB7 , $00BDBDF21 ,
 $086D3D2D4 , $0F1D4E242 , $068DDB3F8 , $01FDA836E , $081BE16CD , $0F6B9265B , $06FB077E1 , $018B74777 ,
 $088085AE6 , $0FF0F6A70 , $066063BCA , $011010B5C , $08F659EFF , $0F862AE69 , $0616BFFD3 , $0166CCF45 ,
 $0A00AE278 , $0D70DD2EE , $04E048354 , $03903B3C2 , $0A7672661 , $0D06016F7 , $04969474D , $03E6E77DB ,
 $0AED16A4A , $0D9D65ADC , $040DF0B66 , $037D83BF0 , $0A9BCAE53 , $0DEBB9EC5 , $047B2CF7F , $030B5FFE9 ,
 $0BDBDF21C , $0CABAC28A , $053B39330 , $024B4A3A6 , $0BAD03605 , $0CDD70693 , $054DE5729 , $023D967BF ,
 $0B3667A2E , $0C4614AB8 , $05D681B02 , $02A6F2B94 , $0B40BBE37 , $0C30C8EA1 , $05A05DF1B , $02D02EF8D ,

\ variable based version :

variable CurrentCRC

: CRC32Init   -1 CurrentCRC ! ;

: CRC32Get ( - u)   CurrentCRC @ invert ;

\ Add byte c to the accumulated CRC u 
: CRC32+ ( c)
    CurrentCRC @  xor  $FF and  cells CRCtable +  @  
    CurrentCRC @  8 rshift  xor  CurrentCRC !
;

\ calculate the CRC32 u for n bytes at a
: CRC32  ( a n - u)
   CRC32Init
   over + swap ?do   
      i c@ CRC32+
   loop 
   CRC32Get 
;

(( 
\ Stack based version :

\ Add byte c to the accumulated CRC u 
: (CRC32)             ( u c - u')
    over xor  $FF and  cells CRCtable +  @  swap  8 rshift  xor 
;

\ calculate the CRC32 u for n bytes at a
: CRC32  ( a n - u)
   -1 rot rot
   over + swap ?do   
      i c@ (CRC32)
   loop 
   invert 
;
))

\ The CRC of a string and its CRC gives a constant, magic number :
\ : @MAGIC ( - u)   pad $1C $66 fill  pad $1C CRC32 pad $1C + !  pad $20 CRC32 ;
\ @MAGIC constant MAGIC_CRC

$2144DF1C constant MAGIC_CRC

0 [if]
\\ ... ignore the rest of the file

\ *************************************************************************** 
\ Tests :
\ The reference for the known answers is from Wil Baden's original source and
\ from hashcalc.exe , available from http://www.slavasoft.com/hashcalc/
\ *************************************************************************** 

variable #failed

\ CRCtest  tests the CRC32 of a string given its correct result u.
: CRCtest ( a n u )     base @ >r  hex 
   >r
   cr  2 spaces  2dup type  $30 over - 0 max spaces
   CRC32  dup base @ >r  hex  0 <#  # # # # # # # #  #> type  r> base !
   r>    2dup = if  
      2drop ."  ok"  
   else  
      ."  <-- should be " 8 u.r ."  failed!"  drop  1 #failed +!  
   then 
   r> base ! 
;


\ ttcrc  tests a selection of strings against known answers.
: TTCRC
   0 #failed !
   cr ." CRC-32 tests : " 
   s" 0" $f4dbdf21 CRCtest
   s" 01" $cf412436 CRCtest
   s" 012" $d5a06ab0 CRCtest
   s" 0123" $a6669d7d CRCtest
   s" 01234" $dda47024 CRCtest
   s" www.inventio.co.uk/crc32.f" $b6dff7c8 CRCtest
   s" An Arbitrary String 012345" $e94ed77f CRCtest
   s" ABCDEFGHIJKLMNOPQRSTUVWXYZ" $abf77822 CRCtest
   s" ZYXWVUTSRQPONMLKJIHGFEDBCA" $99CDFDB2 CRCtest
   s" abcdefghijklmnopqrstuvwxyz" $4c2750bd CRCtest
   s" zyxwvutsrqponmlkjihgfedbca" $7e1dd52d CRCtest  
   s" A"                                                $d3d99e8b CRCtest
   s" 1"                                                $83dcefb7 CRCtest
   s" 12"                                               $4f5344cd CRCtest
   s" 123"                                              $884863d2 CRCtest
   s" 1234"                                             $9be3e0a3 CRCtest
   s" 12345"                                            $cbf53a1c CRCtest
   s" An Arbitrary String"                              $6FBEAAE7 CRCtest
   s" ZYXWVUTSRQPONMLKJIHGFEDBCA"                       $99CDFDB2 CRCtest
   s" 123456789"                                        $CBF43926 CRCtest
   s" THE QUICK BROWN FOX JUMPS OVER THE LAZY DOG"      $4C32B6B9 CRCtest
   s" 0123456789012345678901234567890123456789"         $A6463C49 CRCtest
   s" http://www.slavasoft.com/hashcalc/"               $8ef6a97e CRCtest
   s" http://www.inventio.co.uk/"                       $5bdfb3fe CRCtest
   pad #26 $00 fill pad #26 $afecbe32 CRCtest  ."  ( 26 bytes of 0 )"
   pad #26 $11 fill pad #26 $85e4d237 CRCtest  ."  ( 26 bytes of hex 11 )"
   cr  #failed @ if  
      ." Failed with "  #failed @ . ." error" #failed @ 1 = not if  ." s"  then ." !"  
   else  
      ." All tests passed"  
   then
;

\\ ... ignore the rest of the file

\ *************************************************************************** 
\ Create the table used above
\ *************************************************************************** 

$EDB88320 constant CRCpolynomial \ this is $04c11db7 bit-reversed

{ 
\ SwiftForth assembler version :
\ return the table entry for byte c
code (CRC32calc)   ( c - u')
    0 # edx mov \ intial zero
    8 # ecx mov \ loop count
  1 l:
    edx shr	\ shift crc
    bh rcr
    bl ror	\ shift character
    bx ax mov   \ save character
    bh bl xor   \ xor
    2 l# jns	\ skip if equal
    CRCpolynomial # edx xor  
  2 l:
    ax bx mov   \ restore character
    1 l# loop   \ next bit
    edx ebx mov \ crc to tos
    ret 
    end-code
}

\ return the table entry for byte c
: (CRC32calc)   ( c - u)
   8 0 do
      dup 1 and if
         1 rshift  
	 CRCpolynomial xor
      else
         1 rshift
      then
   loop 
;

: ShowCRCtable 
   hex cr
   $100 0 do  
      i (CRC32calc) 

      \ SwiftForth format
\      0 <# [char] , hold bl hold # # # # # # # # #  [char] $ hold  bl hold #> type
      \ C format - remember to remove the final ,
      0 <# [char] , hold # # # # # # # # #  [char] x hold  [char] 0 hold  bl hold #> type

      i 7 and 7 = if  cr  then 
   loop
;

ShowCRCtable

[then]
