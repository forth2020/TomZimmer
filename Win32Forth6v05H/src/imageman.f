\ imageman.f 0.9A 09/01/2003 13:15:45 Image Manager, Alex McDonald
\ imageman.f 0.9B 30/01/2003 12:53:05 Some changes to keywords
\ imageman.f 0.9C 02/02/2003 14:06:37 Correction to IMGH-#SECTS using @ instead of W@, and corrected
\                                     bug with incorrect header length calculation (too short).
\ imageman.f 0.9C 02/02/2003 14:06:37 Testing shows doesn't work under 95/98 with IMPORT built
\                                     into header; moved to separate section
\ imageman.f 0.9C 02/02/2003 14:06:37 Added correct DOS header for 95/98 as per RBS suggestion
\ imageman.f 0.9C 02/02/2003 14:06:37 Some pretty dreadful documentation
\ imageman.f 0.9D 05/02/2003 12:57:05 Incorrect DOS code built, corrected
\ imageman.f 0.9D 05/02/2003 12:57:05 Temporary code to support multiple kernel versions
\ imageman.f 0.9E 05/02/2003 15:28:40 Added support for a .rsrc section

cr .( Loading Image Manager 0.9C)

((

IMAGEMAN builds Windows EXE images.

For documentation on the PECOFF format, see http://www.microsoft.com/hwdev/hardware/PECOFF.asp.
Note: not included because of copyright restrictions, but freely downloadable. Also see 
"Peering Inside the PE: A Tour of the Win32 Portable Executable File Format" by Matt Pietrek, and
"An In-Depth Look into the Win32 Portable Executable File Format", same author, both findable on
http://msdn.microsoft.com.

There is very little documentation on the loader. An exception is "What Goes On Inside Windows
2000: Solving the Mysteries of the Loader" by Russ Osterlund, also from MSDN.

This quite complex code has 6 major parts.

1. Field definitions using FLDBASE and FLD words.
2. File handling and IMAGE words
3. Section building and writing words
4. Import library building and writing words
5. Header building and writing words
6. Resource copying from module to module

Example:
--------

                         VIMAGE
                         COMPACT
                         NEWIMAGE imgtest.EXE
             GUI         SUBSYSTEM
             n           LOADPOINT
             m           ENTRYPOINT

     0x10000 0x1000      STACKSIZE
     0x10000 0x1000      HEAPSIZE

                         SECTION   .idata
                         SECTIONIS READ INIT
                         IMPLIB  KERNEL32.DLL
                             0   IMPORT GetProcAddress
                             0   IMPORT LoadLibraryA
                             0   IMPORT AllocConsole
                             0   IMPORT FreeConsole
                         IMPLIB  USER32.DLL
                             0   IMPORT DestroyWindow
                             0   IMPORT GetWindowRect
                             0   IMPORT MessageBoxA
                         ENDIMPORTS
                         ENDSECTION

                         SECTION   .code
                         SECTIONIS CODE EXECUTE INIT
               addr len  SECTIONDATA
                         ENDSECTION

                         ENDIMAGE

                         FORTH

Commentary:
-----------

VIMAGE                   IMAGEMAN has its own dictionary (baecause of possible name collisions)
                         so a separate dictionary is used. 
COMPACT                  Standard file is built with 4096 (0x1000) file sections. COMPACT
                         specifies 512 (0x200) file sections, which builds a smaller EXE file
                         at possibly the expense of slightly longer load times.
NEWIMAGE <-name->        Identifies the new image. Note that the filename shouldn't contain
                         spaces as it's parsed with WORD currently
X SUBSYSTEM              where X is CUI (DOS application) or GUI (graphical)
n LOADPOINT              The loadpoint of the program (default is 0x400000). Must be aligned to
                         a 64K boundary.
n ENTRYPOINT             The (relative) entrypoint address.
N M STACKSIZE            The reserved and committed stack size. Default is 1Mb, 4K
N M HEAPSIZE             The reserved and committed heap size. Default is 1Mb, 4K
SECTION <-name->         The name of the section, case sensitive and 8 chars max.
SECTIONIS A ... Z        Section characterisics: CODE EXECUTE READ WRITE INIT UNINIT DISCARD SHARE
                         CODE EXECUTE: section contains executable code
                         READ: section can be read
                         WRITE: section can be written
                         INIT: section contains initialised data
                         Default is none 
N M SECTIONDATA          Address and length of the section
N SECTIONRVA             Not normally used, but can set the (relative) address of the section
N SECTIONSIZE            Size of section. Must be equal to or longer than the sectiondata. When
                         loaded, the section will zeroed and padded out to this length with zeros.
                         Optional, set to size of the section data
ENDSECTION               Required to end a section
IMPLIB <-name->          Import library name
N IMPORT <-name->        Import the procedure <-name-> with a hint of N. N can be zero, but the
                         correct hint will speed (marginally) load time.
ENDIMPORTS               Required to end the imports sections
ENDIMAGE                 Creates the image from the information given above.
FORTH                    Back to the forth directory, optional

Requirements/restrictions:
--------------------------

Keyword order is important. The following MUST appear in the following order:

  NEWIMAGE SECTION ENDIMAGE

  SECTION / ENDSECTION must enclose SECTIONDATA and optionally
  SECTIONIS and SECTIONRVA keywords.

  SECTION / ENDSECTION must enclose IMPLIB and IMPORT

  ENDIMPORTS must terminate IMPLIB and IMPORT

Other keywords can appear in any order anywhere between NEWIMAGE and ENDIMAGE.

SECTIONIS cannot have any comments on the same line.

Sections are always aligned in memory on 4K boundaries. Each section follows the next in memory.
So, the following sections:

                 SECTION .code
        a 0x1234 SECTIONDATA
                 ENDSECTION
                 SECTION .extra
        a 0x1120 SECTIONDATA
                 ENDSECTION

The first section always starts on 0x1000. Section #1 will start at 0x1000, and be 0x1234 bytes
padded out to the next 4K boundary (out to 0x2FFF). Section #2 will start at 0x3000, be 0x1120
bytes padded out to 0x4FFF.

Note that if your sections need to be at a specific (relative) address, or if you need to address
one section from another, then you must understand where the sections will be loaded as there is
currently no method of determining programatically where a section will end up. This is a serious
flaw and will need rectified. It's safe (just now) to assume that the first section will start at
0x1000, and if it is an import section (.idata), then the second section will start at 0x2000.

No resource section is built (section .rsrc). To be addressed. (Tough one, as the format isn't
documented properly.)


Defaults:
---------

             GUI SUBSYSTEM
             0x400000 LOADPOINT
             0x100000 0x1000 STACKSIZE
             0x100000 0x1000 HEAPSIZE

             n SECTIONRVA
             n SECTIONSIZE

))

VOCABULARY VIMAGE
ALSO VIMAGE DEFINITIONS

INTERNAL

\ support multiple kernels for now

[UNDEFINED] NALIGNED [IF]
cr .( Warning: Kernel does not support NALIGNED -- added)
CODE NALIGNED   ( addr n -- addr2 )
                mov    eax, ebx         \ n
                dec    eax              \ n-1
                neg    ebx              \ -n
                pop    ecx              \ addr
                add    eax, ecx         \ addr+n-1
                and    ebx, eax         \ addr+n-1 and -n
                next   c;
[THEN]

[UNDEFINED] GETLASTWINERR [IF]          
cr .( Warning: Kernel does not support GETLASTWINERR -- added)
CODE GETLASTWINERR ( -- n )             \ get last error
                push ebx
                mov eax, fs: 0x18
                mov ebx, 0x34 [eax]
                next c;

VARIABLE WINERRMSG
[THEN]

[UNDEFINED] ADD-LINK [IF]
cr .( Warning: Kernel does not support ADD-LINK -- added)
CODE ADD-LINK   ( addr link -- )                  \ add a link into a chain
                pop    eax                        \ fetch addr
                mov    ecx , 0 [ebx] [edi]        \ fetch address pointed to by link
                mov    0 [eax] [edi] , ecx        \ point addr at pointed to
                mov    0 [ebx] [edi] , eax        \ point link at addr
                pop    ebx                        \ clear stack
                next   c;
[THEN]

\ ----------------------- PECOFF structure ------------------------

BASE @ DECIMAL NOSTACK1

: FLD        ( basevar offset len -<name>- -- basevar offset+len )  \ definer for fields
             create
             3dup + 2>R               \ n1 n2 n3 r: n1 n2+n3
             drop                     \ n1 n2 r: n1 n2+n3
             , , 2R> nostack1         \ build n2 n1
             DOES>  ( -- baseaddr )
               dup @                  \ fetch n2
               swap cell+ @ @ + ;     \ fetch n1 value, add

: FLDBASE    ( -<name>- )         \ set field base name (starts field)
             CREATE here 0 , 0 NOSTACK1 ;  \ base of record


FLDBASE BASE-DOSH
         2 FLD         DOSH-MZ          \ "MZ"
         2 FLD         DOSH-CBLP        \ Bytes on last page of file
         2 FLD         DOSH-CP          \ Pages in file
         2 FLD         DOSH-CRLC        \ Relocations
         2 FLD         DOSH-CPARHDR     \ Size of header in paragraphs
         2 FLD         DOSH-MINALLOC    \ Minimum extra paragraphs needed
         2 FLD         DOSH-MAXALLOC    \ Maximum extra paragraphs needed
         2 FLD         DOSH-SS          \ Initial (relative) SS value
         2 FLD         DOSH-SP          \ Initial SP value
         2 FLD         DOSH-CSUM        \ Checksum
         2 FLD         DOSH-IP          \ Initial IP value
         2 FLD         DOSH-CS          \ Initial (relative) CS value
         2 FLD         DOSH-LFARLC      \ File address of relocation table
         2 FLD         DOSH-OVNO        \ Overlay number
         8 FLD         DOSH-RES         \ Reserved words
         2 FLD         DOSH-OEMID       \ OEM identifier (for e_oeminfo)
         2 FLD         DOSH-OEMINFO     \ OEM information; e_oemid specific
        20 FLD         DOSH-RES2        \ Reserved words
         4 FLD         DOSH-FP-PEHEAD   \ pointer to PE section at offset 0x3c
         0 FLD         DOSH-CODE        \ area for code
     0x80 NALIGNED VALUE LEN-DOSH DROP  \ length may be modified

FLDBASE BASE-IMGH
         4 FLD         IMGH-PE00        \ "PE\0\0"
         2 FLD         IMGH-CPUTYPE     \ 0x14c, x86
         2 FLD         IMGH-#SECTS      \ count of sections
         4 FLD         IMGH-TIMEDATE    \ seconds since December 31st, 1969, at 4:00 P.M.
         4 FLD         IMGH-FP-SYMB     \ ptr to symbol table
         4 FLD         IMGH-#SYMBS      \ number of symbols
         2 FLD         IMGH-OPTHEADSIZE \ size of "optional" header
         2 FLD         IMGH-CHARACTER   \ characteristics, 0x10F
         2 FLD         IMGH-MAGIC       \ 0x10B (PE32)
         1 FLD         IMGH-MAJLINKVER  \ we set to kernel ver (4)
         1 FLD         IMGH-MINLINKVER  \ we set to kernel .rev (2)
         4 FLD         IMGH-CODESIZE    \ size of all code sections
         4 FLD         IMGH-INITSIZE    \ size of all initialised
         4 FLD         IMGH-UNINITSIZE  \ size of all uninitialised
         4 FLD         IMGH-RVA-EP      \ entry point
         4 FLD         IMGH-RVA-CODEBASE    \ base of code
         4 FLD         IMGH-RVA-DATABASE    \ base of data
         4 FLD         IMGH-VA-IMAGE    \ base of image
         4 FLD         IMGH-SECTALIGN   \ section alignment
         4 FLD         IMGH-FILEALIGN   \ file alignment
         2 FLD         IMGH-MAJOS       \ major os (4)
         2 FLD         IMGH-MINOS       \ minor os (0)
         2 FLD         IMGH-MAJIMAGEVER \ major image ver (6.03)
         2 FLD         IMGH-MINIMAGEVER \ minor image ver (build)
         2 FLD         IMGH-MAJSUBSYSVER \ major subsys ver (4)
         2 FLD         IMGH-MINSUBSYSVER \ minor subsys ver (0)
    4 +  4 FLD         IMGH-IMAGESIZE   \ image size
         4 FLD         IMGH-HEADSIZE    \ headersize
         4 FLD         IMGH-CHECKSUM    \ checksum
         2 FLD         IMGH-SUBSYS      \ subsystem, GUI=2
         2 FLD         IMGH-DLLSYS      \ dll sys, 0
         4 FLD         IMGH-RESSTACK    \ reserve stack
         4 FLD         IMGH-COMSTACK    \ commit stack
         4 FLD         IMGH-RESHEAP     \ reserve heap
         4 FLD         IMGH-COMHEAP     \ commit heap
    4 +  4 FLD         IMGH-#DDICT      \ number of dict entries (16)
         8 FLD         IMGD-EXPORT      \ addr & len pairs
         8 FLD         IMGD-IMPORT      \ set by endimports word
         8 FLD         IMGD-RESOURCE
         8 FLD         IMGD-EXCEPTION
         8 FLD         IMGD-SECURITY
         8 FLD         IMGD-FIXUP
         8 FLD         IMGD-DEBUG
         8 FLD         IMGD-ARCHITECTURE
         8 FLD         IMGD-GLOBALPTR
         8 FLD         IMGD-TLS
         8 FLD         IMGD-LOADCONFIG
         8 FLD         IMGD-BOUNDIMPORT
         8 FLD         IMGD-IAT         \ set by endimport word
         8 FLD         IMGD-DELAYIMPORT
         8 FLD         IMGD-COM+
         8 FLD         IMGD-RESERVED
   CONSTANT LEN-IMGH DROP

FLDBASE BASE-SECT
         8 FLD         SECT-NAME        \ name, null terminated (if not= 8)
         4 FLD         SECT-VIRTSIZE    \ size in memory
         4 FLD         SECT-RVA         \ address in memory
         4 FLD         SECT-DATASIZE    \ size of data
         4 FLD         SECT-FP-DATA     \ file pointer to data
         4 FLD         SECT-FP-RELOCS   \ file pointer to relocs (0)
         4 FLD         SECT-FP-LINE#    \ fp to linenums
         2 FLD         SECT-#RELOCS     \ set to zero for exe
         2 FLD         SECT-#LINE#      \ set to zero
         4 FLD         SECT-CHARACTER   \ characteristics, see SECTIONIS
   CONSTANT LEN-SECT DROP

FLDBASE BASE-IID
         4 FLD         IID-RVA-ILT      \ import lookup table
         4 FLD         IID-TIMEDATE     \ time date of binding
         4 FLD         IID-FORWARDER    \ forwarder index
         4 FLD         IID-RVA-NAME     \ RVA to name
         4 FLD         IID-RVA-IAT      \ import address table
   CONSTANT LEN-IID DROP

\ FLDBASE BASE-HINT                       \ not currently used
\          2 FLD         HINT-INDEX       \ hint index
\          0 FLD         HINT-NAME        \ import name
\    VALUE LEN-HINT DROP                  \ modifiable

\ ---------------------------- File handling --------------------------

CREATE IMAGE-NAME MAXSTRING ALLOT
-1 VALUE IMAGE-HNDL

: IMAGE-FERROR   ( -- )
                 cr ." File '"
                 IMAGE-NAME count type ." ' : "
                 WinErrMsg ON GetLastWinErr ;

: IMAGE-FCREATE  ( -- )
                 IMAGE-NAME COUNT r/w create-file if IMAGE-FERROR then
                 to IMAGE-HNDL ;

: IMAGE-FCLOSE   ( -- )
                 IMAGE-HNDL CLOSE-FILE if IMAGE-FERROR then
                 -1 to IMAGE-HNDL ;

: IMAGE-FWRITE   ( addr len -- )
                 IMAGE-HNDL WRITE-FILE if IMAGE-FERROR then ;

\ : IMAGE-2!WRITE  ( n -- )                            \ write a 2-byte entry off the stack
\                  SP@ REL>ABS 2 IMAGE-FWRITE DROP ;
\
\ : IMAGE-4!WRITE  ( n -- )                            \ write a 4-byte entry off the stack
\                  SP@ REL>ABS 4 IMAGE-FWRITE DROP ;

: IMAGE-FREPOS   ( n -- )                            \ reposition file
                 s>d IMAGE-HNDL REPOSITION-FILE if IMAGE-FERROR then ;

: IMAGE-FPOS     ( -- n )                            \ file position
                 IMAGE-HNDL FILE-POSITION if IMAGE-FERROR then
                 d>s ;

0x1000 VALUE LEN-HEAD                               \ header length, see compact
0x1000 VALUE FILE-ALIGN                             \ file alignment, see compact
0x1000 POINTER HEAD-BUFF                            \ header buffer
0x1000 POINTER ZERO-BUFF                            \ binary zeros for padding
0x1000 POINTER IMPS-BUFF                            \ import section buffer

: IMAGE-ALIGNED  ( n -- n' )                          \ align to image file alignment
                 IMGH-FILEALIGN @ NALIGNED            \
                 ;

: IMAGE-PADWRITE ( addr len -- )                      \ write zero padded block
                 DUP>R IMAGE-FWRITE
                 ZERO-BUFF
                 R> dup IMAGE-ALIGNED SWAP - IMAGE-FWRITE \ what's left to write
                 ;

\ ------------------------- Section building --------------------------

INTERNAL

0x00000020 CONSTANT SECTIS-CODE                       \ section constants
0X00000040 CONSTANT SECTIS-INIT
0X00000080 CONSTANT SECTIS-UNINIT
0X02000000 CONSTANT SECTIS-DISCARD
0X10000000 CONSTANT SECTIS-SHARE
0X20000000 CONSTANT SECTIS-EXECUTE
0X40000000 CONSTANT SECTIS-READ
0X80000000 CONSTANT SECTIS-WRITE

CREATE SECTIS-TABLE
      SECTIS-CODE     , ," CODE"
      SECTIS-INIT     , ," INIT"
      SECTIS-UNINIT   , ," UNINIT"
      SECTIS-DISCARD  , ," DISCARD"
      SECTIS-SHARE    , ," SHARE"
      SECTIS-EXECUTE  , ," EXECUTE"
      SECTIS-READ     , ," READ"
      SECTIS-WRITE    , ," WRITE"

: SECTCOMPARE ( str len entry -- f )                  \ compare entry
              cell+ count compare 0=  ;               \ check if equal

: SECTLOOP   ( str len TABLE -- flg )
             8 0 do
               3dup sectcompare
               if @ nip nip unloop unnest then        \ found
               cell+ count + 1+ aligned               \ skip to next entry
             loop ;

EXTERNAL

: SECTIONIS  ( -- <-name-> [<-name->] )               \ section charcteristics
             0
             begin bl word uppercase count pad place
                   pad count dup
             while
               sectis-table sectloop or               \ or in what we find
             repeat 2drop SECT-CHARACTER ! ;
             
INTERNAL

0 VALUE HEAD-BASESECT                                 \ base of section ptr
0 VALUE PREV-SECTRVA                                  \ previous section's RVA

: SECTINIT   ( -- )                                   \ initialise
             0 IMGH-#SECTS !                          \ no sections
             0x1000 to PREV-SECTRVA                   \ previous RVA
             ;

EXTERNAL

: SECTION    ( -- <-name -> )                         \ section name code
             IMGH-#SECTS w@ len-sect *                \ offset for this section
             HEAD-BASESECT + BASE-SECT !              \ set as section base
             1 IMGH-#SECTS +!                         \ current section number
             sect-name len-sect erase                 \ set to bin zero
             BL WORD COUNT 8 MIN 2dup lower           \ parse out the word
             sect-name swap cmove                     \ and move to dest
             prev-sectrva sect-rva !                  \ in case no rva specified
             ;

: SECTIONRVA ( n -- ) SECT-RVA ! ;                    \ RVA of section

: SECTIONSIZE ( n -- )                                \ section virtual size
             0x1000 NALIGNED DUP SECT-VIRTSIZE !      \ align to 4k boundary
             +TO PREV-SECTRVA
             ;

: SECTIONDATA ( addr len -- )                         \ section data, some fields get filled later
             DUP SECT-DATASIZE !                      \ length of real data
             SECTIONSIZE                              \ and it's the default section size too
             SECT-FP-DATA !                           \ will become file ptr when written
             ;

: ENDSECTION ( -- )                                   \ tidy up section
             SECT-DATASIZE @ IMAGE-ALIGNED >R         \ put aligned datasize on rstack
             SECT-CHARACTER @ DUP
             SECTIS-CODE SECTIS-EXECUTE OR AND
             IF                                       \ is section code?
               R@ IMGH-CODESIZE +!                    \ add to code size
               SECT-RVA @ IMGH-RVA-CODEBASE !         \ set codebase rva
             THEN DUP
             SECTIS-INIT AND
             IF                                       \ is section init?
               R@ IMGH-INITSIZE +!                    \ add to init size
               SECT-RVA @ IMGH-RVA-DATABASE !         \ set database rva
             THEN DUP
             SECTIS-UNINIT AND
             IF                                       \ is this uniited?
               R@ IMGH-UNINITSIZE +!                  \ add to uninit size
             THEN
             drop R>DROP

             BASE-SECT @ LEN-SECT + BASE-DOSH @ -     \ max address in header
             FILE-ALIGN NALIGNED TO LEN-HEAD          \ adjust the length of the header

             ;

INTERNAL

: SECTWRITE  ( -- )                                   \ write out sections
             IMGH-#SECTS w@ 0 ?DO                     \ now each section
               HEAD-BASESECT I LEN-SECT * + BASE-SECT ! \ set as section base
               IMAGE-FPOS                             \ get where we are
               SECT-FP-DATA @                         \ address of data
               SECT-DATASIZE @ DUP>R                  \ len to write, save on rstack
               IMAGE-PADWRITE                         \ write out the section padded
               SECT-FP-DATA !                         \ update header where we wrote the data
               R> IMAGE-ALIGNED SECT-DATASIZE !       \ align length to file size, adjust header
             LOOP
             SECT-VIRTSIZE @ SECT-RVA @ + IMGH-IMAGESIZE ! \ imagesize is max address when loaded
             ;

\ -----------------------Import Function building --------------------

((

   IMPLIB-LINK --> | next | func |      | .... | library name

 "next" pointer points to a list of all libraries (IMPLIBs)
 "func" pointer points to a list of functions in this library (IMPORTs in IMPLIB)

   IMPFUNC-LINK -> | next | func |      | .... | function name

 "next" pointer points to a list of all functions (IMPORT)
 "func" pointer points to a list of functions in this library (IMPORTs in IMPLIB)
 
 Uses structure based on BASE-IMPSTR

))

VARIABLE IMPLIB-LINK                                  \ head of IMPLIBs
VARIABLE IMPFUNC-LINK                                 \ head of IMPORTS
0 VALUE  IMPLIB-COUNT                                 \ count of IMPLIBS
0 VALUE  IMPFUNC-COUNT                                \ count of IMPORTS

FLDBASE BASE-IMPSTR
      CELL FLD IMP-NEXT                               \ next ptr
      CELL FLD IMP-FUNC                               \ func ptr *must be second*
      CELL FLD IMP-RVA                                \ RVA of field
        4  FLD IMP-HINT                               \ hint, only for functions
       33  FLD IMP-NAME                               \ name counted string, max id length is 33
   CONSTANT IMP-LEN DROP                              \ length


: IMPINIT    ( -- )                                   \ initialise import
             IMPLIB-LINK OFF
             IMPFUNC-LINK OFF
             0 TO IMPLIB-COUNT
             0 TO IMPFUNC-COUNT
             ;

: IMPSTR     ( link -<name>- -- )                     \ scan, allocate string
             >R ALIGN HERE IMP-LEN ALIGNED ALLOT BASE-IMPSTR !  \ save link, allocate and set base
             IMP-NEXT IMP-LEN ERASE                   \ clear
             IMP-NEXT R> ADD-LINK                     \ add as link
             BL WORD COUNT IMP-NAME PLACE             \ point at name, move it
             ;

EXTERNAL

: IMPLIB     ( -<name>- )                             \ define library name
             IMPLIB-LINK IMPSTR                       \ allocate the string
             1 +TO IMPLIB-COUNT
             ;

: IMPORT     ( n -- -<name>- )                        \ define proc in the above library
             IMPFUNC-LINK IMPSTR                      \ allocate the procname
             IMP-HINT !                               \ save the hint
             IMP-FUNC IMPLIB-LINK @ CELL+ ADD-LINK    \ point at last library func link, add link
             1 +TO IMPFUNC-COUNT
             ;

((

 Add imports a stand-alone section normally called .idata. Section must be declared.
 
 Steps:

 Build a dummy IAT (import address table)
 Build an ILT (import lookup table). Identical to IAT.
 Build the IIDs, one per library
 Build the lib names, remember where we put them (write into the linked list at IMP-RVA)
 Build the hint/func names, remember where we put them
  
 If section isn't big enough, make it larger (we haven't written it yet)

 End result is this:

 IID1:                      ILT          FUNCS              IAT
   IID-RVA-ILT ----------> |   | ---> | hint | func | <--- |   |  <--1
   IID-TIMEDATE            | 0 |                           | 0 |
   IID-FORWARDER      +--> |   | ---> | hint | func | <--- |   |  <--+
   IID-RVA-NAME --... |    |   | ---> | hint | func | <--- |   |     |
   IID-RVA-IAT -->1   |    | 0 |                           | 0 |     |
 IID2:                |     ...                             ...      |
   IID-RVA-ILT -------+                                              |
   IID-TIMEDATE                                                      |
   IID-FORWARDER                                                     |
   IID-RVA-NAME ---> libname                                         |
   IID-RVA-IAT ------------------------------------------------------+
 ...
 
 # of IIDs = IMPLIB-COUNT + 1
 # of ILT entries = IMPLIB-COUNT + IMPFUNC-COUNT (same for IAT)
 
 IAT is built first, and is the table modified by the loader to contain load addresses. 
 Note that the entries are built back-to-front from the declaration order -- the last function
 in the last library appears first, and the first appears last.

))

INTERNAL

\ 0 VALUE  HEAD-IID                                     \ head of IID section
\ 0 VALUE  HEAD-ILT                                     \ head of ILT section
\ 0 VALUE  HEAD-FUNCS                                   \ head of funcs section
\ 0 VALUE  HEAD-IAT                                     \ head of IAT section
0 VALUE  CURR-IID                                     \ current IID
0 VALUE  CURR-ILT                                     \ current ILT section
0 VALUE  CURR-FUNCS                                   \ current funcs section
0 VALUE  CURR-IAT                                     \ current IAT section
0 VALUE  LEN-IAT                                      \ length of IAT
0 VALUE  LEN-ALLIIDS                                  \ length of all IIDs

: ->RVA      ( n -- n' )                              \ convert to RVA
             IMPS-BUFF SECT-RVA @ - - ;               \ from start of header

EXTERNAL

: ENDIMPORTS ( -- )                                   \ build import words in header
    IMPLIB-COUNT 1+ LEN-IID * TO LEN-ALLIIDS          \ length of all IIDs
    IMPLIB-COUNT IMPFUNC-COUNT + CELLS TO LEN-IAT     \ length of IAT

\     IMPS-BUFF TO HEAD-IAT                             \ base of import stuff in header id IID
\     HEAD-IAT LEN-IAT + TO HEAD-ILT                    \ pointer to IAT
\     HEAD-ILT LEN-IAT + TO HEAD-IID                    \ pointer to funcs
\     HEAD-IID LEN-ALLIIDS + TO HEAD-FUNCS              \ pointer to ILT
    IMPS-BUFF TO CURR-IAT                             \ base of import stuff in header id IID
    CURR-IAT LEN-IAT + TO CURR-ILT                    \ pointer to IAT
    CURR-ILT LEN-IAT + TO CURR-IID                    \ pointer to funcs
    CURR-IID LEN-ALLIIDS + TO CURR-FUNCS              \ pointer to ILT

\     HEAD-IID TO CURR-IID      HEAD-ILT TO CURR-ILT
\     HEAD-FUNCS TO CURR-FUNCS  HEAD-IAT TO CURR-IAT

\     HEAD-IID ->RVA IMGD-IMPORT !                      \ point at imports
    CURR-IID ->RVA IMGD-IMPORT !                      \ point at imports
    LEN-ALLIIDS IMGD-IMPORT CELL+ !                   \ length of IIDs

\     HEAD-IAT ->RVA IMGD-IAT !                         \ point at IAT
    CURR-IAT ->RVA IMGD-IAT !                         \ point at IAT
    LEN-IAT IMGD-IAT CELL+ !                          \ length of IAT

    IMPLIB-LINK                                       \ loop through the libraries
    BEGIN @ ?DUP WHILE
      DUP BASE-IMPSTR !                               \ point at IMPSTR with lib name in it
      CURR-IID BASE-IID !                             \ set base of IID to current
      CURR-ILT ->RVA IID-RVA-ILT !                    \ point at ILT
      CURR-IAT ->RVA IID-RVA-IAT !                    \ point at IAT

      CURR-FUNCS ->RVA IID-RVA-NAME !                 \ point as RVA from header to lib
      IMP-NAME COUNT 1+ ALIGNED DUP>R CURR-FUNCS SWAP CMOVE \ copy the library name (padded)
      R> +TO CURR-FUNCS                               \ update function pointer

      BEGIN IMP-FUNC @ ?DUP WHILE
         CELL- BASE-IMPSTR !                          \ point at func impstr
         CURR-FUNCS ->RVA CURR-ILT !                  \ point ILT at func
         CURR-FUNCS ->RVA CURR-IAT !                  \ point IAT at func
         IMP-HINT @ CURR-FUNCS W! 2 +TO CURR-FUNCS    \ save hint, move on two bytes
         IMP-NAME COUNT 1+ ALIGNED DUP>R CURR-FUNCS SWAP CMOVE \ move the function name
         R> +TO CURR-FUNCS                            \ update the func pointer
         CELL +TO CURR-ILT CELL +TO CURR-IAT          \ update ILT/IAT pointers
      REPEAT
      
      CELL +TO CURR-ILT CELL +TO CURR-IAT             \ update ILT/IAT pointers (zero entry)
      LEN-IID +TO CURR-IID                            \ next IID
    REPEAT

    IMPS-BUFF CURR-FUNCS ->RVA SECT-RVA @ - SECTIONDATA \ calculate section & length

    ;

\ ------------------------- Header building --------------------------

CREATE DOSCODE HERE 0 C, NOSTACK1                   \ dos code stub as counted string
             0x0E C, 0x1F C, 0xBA C, 0x0E C,
             0x00 C, 0xB4 C, 0x09 C, 0xCD C,
             0x21 C, 0xB8 C, 0x01 C, 0x4C C,
             0xCD C, 0x21 C,
             HERE OVER - 1- SWAP C!                 \ save length

CREATE DOSMESG HERE NOSTACK1
             ," This program cannot be run in DOS mode"
             -null,
             0x0D C, 0x0D C, 0x0A C, CHAR $ C,
             HERE OVER - 1- SWAP C!                 \ save length

EXTERNAL

2 CONSTANT GUI
3 CONSTANT CUI

: SUBSYSTEM  ( n -- ) IMGH-SUBSYS W! ;                \ declare subsystem
: LOADPOINT  ( n -- ) IMGH-VA-IMAGE ! ;               \ declare loadpoint
: ENTRYPOINT ( n -- ) IMGH-RVA-EP ! ;                 \ declare entrypoint
: STACKSIZE  ( r c -- ) IMGH-COMSTACK ! IMGH-RESSTACK ! ; \ declare stack
: HEAPSIZE   ( r c -- ) IMGH-COMHEAP ! IMGH-RESHEAP ! ; \ declare heap
: COMPACT    ( -- )                                   \ build a compact header
             0x200  TO FILE-ALIGN
             0x200  TO LEN-HEAD
             ;

: NEWIMAGE   ( -- <-name-> )                          \ declare new image
             bl word count IMAGE-NAME PLACE           \ image name
             
             ZERO-BUFF 0x1000 ERASE                   \ clear out zero buff
             HEAD-BUFF 0x1000 ERASE                   \ and header buff
             IMPS-BUFF 0x1000 ERASE                   \ and imports buffer
             HEAD-BUFF DUP  BASE-DOSH !               \ set base DOS header address
             LEN-DOSH + dup BASE-IMGH !               \ base image
             LEN-IMGH + dup BASE-SECT !               \ base section headers
                            TO HEAD-BASESECT          \ and the save ptr

     ( MZ )  0x5A4D      DOSH-MZ W!                   \ set MZ
             0x90        DOSH-CBLP     W!             \ fill out dos header
             0x3         DOSH-CP       W!
             0x4         DOSH-CPARHDR  W!
             0xFFFF      DOSH-MAXALLOC W!
             0xB8        DOSH-SP       W!
             0x40        DOSH-LFARLC   W!
             LEN-DOSH    DOSH-FP-PEHEAD !             \ set PE ptr
             DOSCODE COUNT DUP>R DOSH-CODE SWAP CMOVE \ set code
             DOSMESG COUNT DOSH-CODE R> + SWAP CMOVE  \ add on message

    ( PE00 ) 0x4550      IMGH-PE00 !                  \ PE\0\0
             0x014C      IMGH-CPUTYPE W!              \ CPU type = 80386
             0x00E0      IMGH-OPTHEADSIZE W!          \ opt header size
             0x010F      IMGH-CHARACTER   W!          \ No relocs|linenums|symbols, 32 bit app

             0x010B      IMGH-MAGIC      W!           \ PE32
             0x0         IMGH-MAJLINKVER C!           \ link ver 0.90
             0x90        IMGH-MINLINKVER C!

             0x1000      IMGH-SECTALIGN    !          \ 4k section align
             FILE-ALIGN  IMGH-FILEALIGN    !          \ n BYTE file align (mult of 512 bytes)
             0x4         IMGH-MAJOS        W!
             0x0         IMGH-MINOS        w!
             version# 0 10000 sm/rem                  \ derive major/minor from version#
                         IMGH-MAJIMAGEVER  W!
                         IMGH-MINIMAGEVER  W!
             0x4         IMGH-MAJSUBSYSVER W!
             0x0         IMGH-MINSUBSYSVER W!
             LEN-HEAD    IMGH-HEADSIZE     !          \ n byte header size (mult of filealign)
             0x10        IMGH-#DDICT       !          \ 16 dict entries

             GUI SUBSYSTEM
             0x400000 LOADPOINT
             0x100000 0x1000 STACKSIZE
             0x100000 0x1000 HEAPSIZE

             IMPINIT                                  \ initialise
             SECTINIT
             ;
             
: ENDIMAGE   ( -- )                                   \ fixup all the missing info
             cr ." Building image " IMAGE-NAME COUNT TYPE
             IMAGE-FCREATE                            \ create the file
                                     
             FILE-ALIGN  IMGH-FILEALIGN    !          \ n BYTE file align (mult of 512 bytes)
             LEN-HEAD    IMGH-HEADSIZE     !          \ n byte header size (mult of filealign)
             ZERO-BUFF LEN-HEAD IMAGE-PADWRITE        \ write out a dummy header

             SECTWRITE                                \ write out sections

             IMAGE-FPOS >R                            \ get file position
             0 IMAGE-FREPOS                           \ position to 0

             HEAD-BUFF LEN-HEAD IMAGE-PADWRITE        \ rewrite corrected header

             R> ." ; built, 0x" h. ." bytes long"
             cr

             IMAGE-FCLOSE
             ;
             
BASE !

MODULE

ONLY FORTH ALSO DEFINITIONS

