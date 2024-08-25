anew struct.f   \ for Win32Forth  October 7th, 2002 - 10:35
\ For C like structures.

\ Added ulong ushort
\ renamed @+ to n+adr@

\ August 22nd, 2001 - 19:23
\    added OFFSET    saves calculations
\          >STRUCT   saves runtime
\  changed }STRUCT for better cloning

\ January 15th, 2002 - 16:46 removed a bug from _add-struct
\ July 13th, 2002 - 16:26 Added Field:
\ May 29th, 2004  - 17:30 Changed INT and UINT

: .struct      ( struct sizeof - )
   0
     do  key?
           if   key drop leave  then
         cr i . dup i + ? 4
     +loop drop ;

\ The following memory allocation words allow nesting and cloning
\ of a memory structure. Definitions made in C can be used with
\ small modifications.

0 value  _struct

vocabulary allocation \ avoids conflicts. e.g. word

also allocation definitions

code n+adr@ ( n adr - adr@+n )  \ was incorrect @+
     mov     ebx, 0 [ebx] [edi] \ @
     pop     eax                \ +
     add     ebx, eax
     next    c;

: _add-struct ( sizeof.struct - )
     _struct here ! cell allot +to _struct ;

: add-struct ( sizeof.struct - )
   create _add-struct
   does> n+adr@ ;

: byte
   create 1 _add-struct  \ compile-time: ( - )
   does>  n+adr@ ;           \ run-time: ( offset - offset+dword ) immediate

: word
   create 2 _add-struct  \ compile-time: ( - )
   does>  n+adr@  ;          \ run-time: ( offset - offset+dword ) immediate

: dword
   create 4 _add-struct  \ compile-time: ( - )
   does>  n+adr@  ;          \ run-time: ( offset - offset+dword ) immediate

: double
   create 8 _add-struct   \ compile-time: ( - )
   does>  n+adr@ ;            \ run-time: ( offset - offset+dword ) immediate

: long_double
   create 10 _add-struct  \ compile-time: ( - )
   does>  n+adr@ ;            \ run-time: ( offset - offset+dword ) immediate

: guid
   create 16 _add-struct  \ compile-time: ( - )
   does>  n+adr@ ;            \ run-time: ( offset - offset+dword ) immediate


: qword
   create 32 _add-struct  \ compile-time: ( - )
   does>  n+adr@ ;            \ run-time: ( offset - offset+dword ) immediate

' noop alias unsigned  \ 0 can be ignored when allocating

' byte  alias char  \ 1

' word  alias short \ 2
' word  alias ushort
' word  alias wchar

' dword alias long  \ 4
' dword alias ulong
' dword alias langid
' dword alias lpvoid
' dword alias float
' dword alias int  \ May 29th, 2004 must be 32 in W95 or better
' dword alias uint

' add-struct alias field:


\ Not standard in C

: b/float
   create b/float _add-struct  \ compile-time: ( - )   \ 8 or 10
   does>  n+adr@ ;                 \ run-time: ( offset - offset+dword )

: cell
   create cell _add-struct  \ compile-time: ( - )  \ Forth depended
   does>  n+adr@ ;              \ run-time: ( offset - offset+dword )

: offset
   create _struct ,         \ compile-time: ( - ) It is a kind of label
   does>  n+adr@ ;              \ run-time: ( offset - offset+dword )

: }struct
   \in-system-ok previous
   create _struct , ( store the offset/size )   \ unnested
   does> @ create _struct , _add-struct  ( get the offset and create a field with it ) \ nested
   does> n+adr@                          ( that is itself that offsetword )
;


only forth definitions forth

: getsize-struct    ( -<name-struct>- - n ) ' >body @ ;

: sizeof  ( - size )
   getsize-struct state @
       if     postpone literal
       then ; immediate

\ >struct compiles the adress and offset as 1 adress inside a definition
: >struct (  -<name-struct>- -<member>- - adress+offset_in_structure )
  ' >body cell+ getsize-struct + postpone literal  ; immediate


: struct{      ( - )
   \in-system-ok also allocation
   0 to _struct ;

: mkstruct:  ( size-struct - )       create dup , allot does> cell+ ;

\ July 13th, 2002 - 13:17
\ Note: next-offset is Forth-depended
: next-offset ( 'adr - next-offset )    12 +  ;

\s

\ Examples:

struct{ \ language
   LANGID  language.LanguageID
   CHAR    language.szDialect[LANG_LEN]
   }struct language

\ There is nothing allocated yet only the positions in memory are defined
\ Now it is going to be allocated in two different locations.

sizeof language mkstruct: languageTemp1
sizeof language mkstruct: languageTemp2

\ Change language.szDialect[LANG_LEN] of languageTemp1 as follows:
1 languageTemp1 language.szDialect[LANG_LEN] c!

cr cr .( The value of languageTemp1 is: )
 languageTemp1 language.szDialect[LANG_LEN] c@ .

\ The names are a bit longer, this is needed to avoid duplicate names.

struct{  \ BitmapFileHeader
        WORD   bfType
        LONG   bfSize
        WORD   bfReserved1
        WORD   bfReserved2
        DWORD  bfOffsetBits
        OFFSET >BitmapInfoHeader
}struct BitmapFileHeader

struct{ \  BITMAPINFOHEADER
        DWORD      biSize
        LONG       biWidth
        LONG       biHeight
        WORD       biPlanes
        WORD       biBitCount
        DWORD      biCompression
        DWORD      biSizeImage
        LONG       biXPelsPerMeter
        LONG       biYPelsPerMeter
        DWORD      biClrUsed
        DWORD      biClrImportant
 }struct BITMAPINFOHEADER

struct{ \ RGBQUAD
        BYTE    rgbBlue
        BYTE    rgbGreen
        BYTE    rgbRed
        BYTE    rgbReserved
}struct RGBQUAD

struct{ \ BITMAPINFO
    BitmapInfoHeader  pbmiBitmapInfoHeader
    offset pbmiColors
    sizeof RGBQUAD 256 * _add-struct
}struct bitmapinfo

sizeof bitmapinfo mkstruct: pbmi
cr .( The size of bitmapinfo is: ) sizeof bitmapinfo .

: test
    cr ." A member can be compiled as an adres."
    2 >struct pbmi biSize !
    cr ." The color array starts at: " >struct pbmi pbmiColors .
 ;
cr see test test

\s

