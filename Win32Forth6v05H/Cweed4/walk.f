\ Walk.f for Win32Forth V6.05H
\ Walk through every file in a folder, recursively

: $Version   s" Walk V1.0 2021 Nov 11" ;

[defined] .hex not [if]
: .hex ( u c -- )   base @ >r  hex  u.r  r> base ! ;
[then]

[defined] VFXFORTH [if]    \ VFX Forth

[then]

[defined] LIBRARY [if]     \ SwiftForth

: R-FREE ( -- )   ;    \ not required in SwiftForth because R-ALLOC self deallocates on exiting the word

[then]

[defined] winlibrary [if]  \ WIN32FORTH

: ZCOUNT ( zaddr -- addr n )
   DUP DUP IF  65535 0 SCAN DROP OVER - THEN ;

: ZPLACE ( from n to -- )
   TUCK OVER + >R  CMOVE  0 R> C! ;

: ZAPPEND ( from n to -- )
   ZCOUNT + ZPLACE ;

[undefined] BUFFER: [if]
: BUFFER: ( n "name" -- )
   CREATE ALLOT ;
[then]

' MAX-PATH ALIAS MAX_PATH ( -- n )

\ Note : these must be aliasses, not colon words, because they change the R-stack
' _LOCALALLOC ALIAS R-ALLOC ( n -- a )  
' _LOCALFREE  ALIAS R-FREE ( a -- )   

\ Windows interface
: GetCurrentDirectory ( n buffer -- f )   rel>abs swap call GetCurrentDirectory ;
: SetCurrentDirectory ( z$ -- f )    rel>abs  call SetCurrentDirectory ;
: FindFirstFile ( z$ attr -- handle )   rel>abs  swap  rel>abs  call FindFirstFile ;
: FindNextFile ( handle z$ -- f )   rel>abs swap call FindNextFile ;
: FindClose ( handle -- f )   call FindClose ;

[then]

\ *****************************************************************************
\ Directory walking
\ *****************************************************************************
\ 
\ The initial Attrib parameter is used to specify the type of files to be
\ searched for, based on the following table:
\ 
\        Attribute   Constant        Meaning
\ 
\        0           A_NORMAL        Normal Files
\        1           A_READONLY      Files marked read only
\        2           A_HIDDEN        Files marked as hidden
\        4           A_SYSTEM        Files marked as system files
\        16          A_DIRECTORY     Directories only
\        32          A_ARCHIVE       Files whose archive bit is on
\        64          A_COMPRESSED    Compressed files [NT only]
\        128         A_COMBINE       COMBINE normal [0] attribute
\        256         A_NODOTS        Exclude . and .. from return
\        256+16      A_SUBDIRECTORY  SUBDIRECTORIES only [exclude dots]
\        512         A_FULLNAME      Returns fully qualified name
\        1024        A_SHORTNAME     Returned 8.3 filename
\        8192        A_IGNOREARCHIVE Ignores archive bit
\        32768       A_EVERYTHING    Returns all entries
\        131072      A_EXACTMATCH    All attr. must match
\ 

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

decimal

: struct ( n c -- n' )   create  over , +  DOES> ( a -- a' ) @ + ;

\ This is the Windows WIN32_FIND_DATA File Attribute structure
?loading @   0 ?loading !            \ to prevent a warning being given at compile time
0        4 struct oFileAttributes
         8 struct oCreationTime
         8 struct oLastAccessTime
         8 struct oLastWriteTime
         8 struct oFileSize
         8 struct oReserved
  MAX_PATH struct oFileName
        14 struct oAlternateFileName
         2 struct oDummy
         4 struct oHandle              \ we add this on the end of the Windows structure
constant |WIN32_FIND_DATA|
?loading !                             \ restore the value of ?loading

$10 constant FILE_ATTRIBUTE_DIRECTORY

: IsDirectory? ( a -- flag )
    oFileAttributes @ FILE_ATTRIBUTE_DIRECTORY and 0<> 
;

: IsSubDirectory? ( a -- flag )
\    oFileAttributes @ ( FILE_ATTRIBUTE_SUBDIRECTORY FILE_ATTRIBUTE_NODOTS or ) $110 and 0<>    \ Note : this does not work (WinXP)
   >r
   r@ IsDirectory? 
   r@ oFileName zcount s" ."  compare 0<> and
   r@ oFileName zcount s" .." compare 0<> and 
   r> drop
; 

MAX_PATH 2* Buffer: MyLongPathBuffer

: .FileName ( a -- ) 
   >r
   cr
\   r@ oFileAttributes @ .fAttribute
\   ."   size = " r@ oFileSize 2@ 8 D.R 
   MAX_PATH MyLongPathBuffer GetCurrentDirectory drop 
   s" \" MyLongPathBuffer zappend
   r@ oFileName ZCOUNT MyLongPathBuffer zappend  
   MyLongPathBuffer zcount  dup >r type
   60 r> - 0 max spaces  
\   ." | " 
\   r@ IsDirectory? if ."   Dir"  then
\   r@ IsSubDirectory? if ."   SubDir"  then
   r> drop
;

MAX_PATH BUFFER: FileSpecification  \ stores the file filter : *.* , *.c , *.f , fred*.com etc.
                                    \ Note : only one filter per scan...

defer FileAction ( a -- flag )   \ return true to exit the Walk loop

: NullAction ( zstr -- f )   .Filename  0 ;
' NullAction is FileAction

: Walk ( -- flag )
   |WIN32_FIND_DATA| R-ALLOC >r    \ allocate storage on the R-stack
                                                                     \ In the first loop, display any files that are not directories
   FileSpecification  r@ oFileAttributes  FindFirstFile ( -- newHandle) \ Start by finding the first file that matches the FileSpecification
   
   r@ oHandle !   \ save the old handle

   r@ 
   oHandle @ INVALID_HANDLE_VALUE <>
   BEGIN
      0<> WHILE
      r@ IsDirectory? NOT IF
         r@ FileAction                                               \ perform the required action on each file here
         ?DUP IF  r@ oHandle @ FindClose drop  r> drop  exit  then   \ if the action returns a true flag, exit completely from Walk
      THEN
      r@ oHandle @ r@ oFileAttributes  FindNextFile 
   REPEAT 
   r@ oHandle @ FindClose drop
                                                                     \ In the second loop, look for sub-directories, and if found change into that directory
   Z" *.*"  r@ oFileAttributes FindFirstFile r@ oHandle ! 
   r@ 
   oHandle @ INVALID_HANDLE_VALUE <> 
   BEGIN
      0<> WHILE
      r@ IsSubDirectory? IF
         r@ oFileName SetCurrentDirectory drop
         RECURSE ( flag)                                             \ Having found a sub-directory, recurse, allowing the first loop to display the files...
         Z" .."  SetCurrentDirectory drop                            \ Then change back to the next directory level up 
         ?dup if  r@ oHandle @ FindClose drop  r> drop  exit  then
      THEN
      r@ oHandle @ r@ oFileAttributes FindNextFile 
   REPEAT 
   r@ oHandle @ FindClose drop 
   0           \ return a flag here if we made it this far...
   r> drop 
   R-FREE
;

variable lastStackDepth

\ 4.21
\ perform the action of xt for each file in the current folder, recursively
: WalkFilesC&F ( xt -- )   is FileAction  
   0 ShowZeroFileLengthError !
   depth lastStackDepth !
   cr  s" *.c" 2dup type FileSpecification zplace  Walk drop
   depth lastStackDepth @ - StackDepthValue !
   cr  s" *.h" 2dup type FileSpecification zplace  Walk drop
\   cr  s" *.ini" 2dup type FileSpecification zplace  Walk drop
   -1 ShowZeroFileLengthError !   
; 

\ original 4.2
\ perform the action of xt for each file in the current folder, recursively
: WalkFilesOrig ( xt -- )   is FileAction
   0 ShowZeroFileLengthError !
   depth lastStackDepth !
   cr  s" *.c??" 2dup type FileSpecification zplace  Walk drop
   depth lastStackDepth @ - StackDepthValue !
   cr  s" *.h??" 2dup type FileSpecification zplace  Walk drop
   cr  s" *.ini" 2dup type FileSpecification zplace  Walk drop
   -1 ShowZeroFileLengthError !
;

variable FolderFilesC&Honly
-1 FolderFilesC&Honly !

: ShowFolderFilesTypes ( -- )
   -1 FolderFilesC&Honly !
   FolderFilesC&Honly @ if  ." *.c and *.h files only"  else  ." *.c?? , *.h?? and *.ini files"  then 
;

: WalkFiles ( xt -- )   
   FolderFilesC&Honly @ if
      WalkFilesC&F
   else
      WalkFilesOrig
   then
;

: ttwfsn  ['] NullAction WalkFiles  cr ."  ttwfs done "  .s  .StackDepths ;

\ ***********************************************************************************
\ Simple walk, not recursively
\ ***********************************************************************************

: WalkSimple ( -- flag )
   |WIN32_FIND_DATA| R-ALLOC >r    \ allocate storage on the R-stack
   FileSpecification r@ oFileAttributes  FindFirstFile r@ oHandle !
   r@ oHandle @ INVALID_HANDLE_VALUE <>
   BEGIN 
      ( flag) 
   WHILE
      r@ .FileName
      r@ oHandle @  r@ oFileAttributes FindNextFile
      ( flag)
   REPEAT 
   r@ oHandle @ FindClose drop 
   r> drop
   R-FREE
;

\\
\ demo program
: ttt   
   cr  ." Walk "  
   s" *.c??" 2dup type FileSpecification zplace  
   Walk cr . 
   cr  ." Walk "  
   s" *.h??" 2dup type FileSpecification zplace  
   Walk cr . 
; 

: ttw   
   cr  ." WalkSimple "  
   s" *.c??" 2dup type FileSpecification zplace  
   WalkSimple  
   cr  ." WalkSimple "  
   s" *.h??" 2dup type FileSpecification zplace  
   WalkSimple  
;

cr .( ttt  to show demo )
