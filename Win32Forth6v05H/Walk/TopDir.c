\ walk.f 2011 Aug 24
\ Walk through every file in a folder, recursively

: .HostVersion   ." V1.0 2011 Aug 24" ;

: GREEN.    7 ATTRIBUTE ;
: YELLOW.   6 ATTRIBUTE ;
: MAGENTA.  5 ATTRIBUTE ;
: CYAN.     4 ATTRIBUTE ;
: BLUE.     3 ATTRIBUTE ;
: RED.      2 ATTRIBUTE ;
: INVERSE.  1 ATTRIBUTE ;
: NORMAL.   0 ATTRIBUTE ;

: DIM.     CYAN. ;
: DATA.    BLUE. ;
: BRIGHT.  INVERSE. ;
: ERROR.   RED. ;
: PASS.    GREEN. ;

: -."  \ compiling: ( "ccc<">" -- )  executing: ( -- )
   POSTPONE S" POSTPONE DIM. POSTPONE TYPE  POSTPONE DATA. ;  IMMEDIATE

: +."  \ compiling: ( "ccc<">" -- )  executing: ( -- )
   POSTPONE S" POSTPONE DATA. POSTPONE TYPE  POSTPONE DATA. ;  IMMEDIATE

: -u.r (  u c -- )   DIM.  u.r  DATA. ;
: +u.r (  u c -- )   BRIGHT.  u.r  DATA. ;

: -. (  n -- )   DIM.  .  DATA. ;
: +. (  n -- )   BRIGHT.  .  DATA. ;

: +type ( a n -- )   BRIGHT.  type  DATA. ;

: .hex ( u n)   base @ >r  hex  u.r  r> base ! ;
: d.hex ( d n)   base @ >r  hex  d.r  r> base ! ;

: .dml ( u n)   base @ >r  decimal  u.r  r> base ! ;
: d.dml ( d n)   base @ >r  decimal  d.r  r> base ! ;

: WaitKey   begin  pause  EKEY? until ;


\ *****************************************************************************
\ Directory walking
\ *****************************************************************************
{ 
: PWD ( -- )
   PAD 256 OVER GetCurrentDirectory TYPE ;

: CD ( -- ) \ Usage: CD <dirpath> _or_ CD
   0 WORD DUP C@ IF ( a dir spec followed, we hope)
      0 OVER COUNT + C!  1+  SetCurrentDirectory
      0= ABORT" Invalid Directory" EXIT
   THEN DROP PWD ;
}
{ 
 The initial Attrib parameter is used to specify the type of files to be
searched for, based on the following table:

        Attribute   Constant        Meaning

        0           A_NORMAL        Normal Files
        1           A_READONLY      Files marked read only
        2           A_HIDDEN        Files marked as hidden
        4           A_SYSTEM        Files marked as system files
        16          A_DIRECTORY     Directories only
        32          A_ARCHIVE       Files whose archive bit is on
        64          A_COMPRESSED    Compressed files (NT only)
        128         A_COMBINE       COMBINE normal (0) attribute
        256         A_NODOTS        Exclude . and .. from return
        256+16      A_SUBDIRECTORY  SUBDIRECTORIES only (exclude dots)
        512         A_FULLNAME      Returns fully qualified name
        1024        A_SHORTNAME     Returned 8.3 filename
        8192        A_IGNOREARCHIVE Ignores archive bit
        32768       A_EVERYTHING    Returns all entries
        131072      A_EXACTMATCH    All attr. must match
}

\ display the file entry attribute c
: .fAttribute ( u -- )
   ."   ATTR = " dup 3 .hex
   dup $0F and $0F = if  ."  LONG_FILE"  drop  exit  then
   dup $01 and if  ."  R/O "  else  ."  R/W "  then   \ Read Only or Read/Write
   dup $02 and if  ." H"  else  ."  "  then           \ Hidden
   dup $04 and if  ." S"  else  ."  "  then           \ System
   dup $08 and if  ." V"  else  ."  "  then           \ Volume
   dup $10 and if  ." D"  else  ."  "  then           \ Directory
   dup $20 and if  ." A"  else  ."  "  then           \ Archive
\   dup $40 and if  key drop ( ." X"  else  ."  "  ) then           \ End!!!
   drop
;

$1E constant NOT_A_NORMAL_FILE

: .FileName ( a -- f ) 
   dup 
   NOT_A_NORMAL_FILE and 0= drop -1 if
      cr dup .fAttribute  4 spaces 
       zcount type 
   else
      drop 
   then
;

{ 
: Walk ( z$-- )
   PAD FindFirstFile >R
   R@ INVALID_HANDLE_VALUE <>
   BEGIN 
      ( flag) 
   WHILE
      PAD .FileName
      R@ PAD FindNextFile 
      ( flag)
   REPEAT 
   R> FindClose DROP 
;

: ttw  z" Dir1\*" Walk ;

} 

decimal

60 MAX_PATH + Buffer: FileAttributeStruct

: struct ( a n -- a )   over  CONSTANT + ;

FileAttributeStruct
         4 struct FileAttributes
         8 struct CreationTime
         8 struct LastAccessTime
         8 struct LastWriteTime
         8 struct FileSize
         8 struct Reserved
  MAX_PATH struct FileName
        14 struct AlternateFileName
         2 struct Dummy
FileAttributeStruct - constant |FileAttributeStruct|

variable HANDLE

MAX_PATH BUFFER: SPEC
MAX_PATH BUFFER: ADDR

: IS-DIR ( -- flag )
   FileAttributes @ FILE_ATTRIBUTE_DIRECTORY and 0<> ;

: IS-SUBDIR ( -- flag )
   FileAttributes @ FILE_ATTRIBUTE_DIRECTORY and 0<>
   FileName zcount s" ."  compare 0<> and   
   FileName zcount s" .." compare 0<> and ; 

: FIRST ( zstr -- flag )
   ADDR FindFirstFile DUP HANDLE !  INVALID_HANDLE_VALUE <> ;

: NEXT ( -- flag )
   HANDLE @ ADDR FindNextFile ;

: CLOSE ( -- )
   HANDLE @ FindClose drop ;


defer FILE-ACTION ( zstr -- flag ) 

: NoAction ( zstr -- )   drop 0 ;
: MyAction ( zstr -- )   .Filename 0 ;

\ ' NoAction is FILE-ACTION
' MyAction is FILE-ACTION

\ give search a filespec to search for, and it will look
\ in the current directory and all the subdirs until
\ the file-action defer returns a non-zero


: SEARCH ( zstr -- flag )
   [OBJECTS DIRTOOL MAKES FD OBJECTS]
\   LOCALS| FD |
   \ R-BUF
   ZCOUNT FD SPEC ZPLACE
   FD SPEC  FD FIRST BEGIN
      0<> WHILE
      FD IS-DIR NOT IF
         FD FileName FILE-ACTION
         ?DUP IF  FD CLOSE  EXIT  THEN
      THEN
      FD NEXT
   REPEAT FD CLOSE
   Z" *.*" FD FIRST BEGIN
      0<> WHILE
      FD IS-SUBDIR IF
         FD FileName SetCurrentDirectory DROP
         FD SPEC RECURSE ( flag)
         Z" .." SetCurrentDirectory drop
         ?dup if  FD CLOSE  exit  then
      THEN
      FD NEXT
   REPEAT 
   FD CLOSE 0 ;

: ttt   z" *.*" SEARCH ; 

