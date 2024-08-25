\ Cweed4.f  Howerd Oakford  Main load file
\ Most of the interesting stuff is in the file "Files.f"

only forth also definitions

ANEW Cweed4_program    \ mark the start of the program

: HostVersion ( -- )   s" Cweed4 file weeder and scanner - V4.07 2022 Aug 03 " ;  \ VERSION

0 constant NO_SPACE_AFTER_//        \ remove the space after each "//" , if true, if required by "Software Coding Guideline (VW AIO)"  

32769 CONSTANT SPEC_MAX_NUM_LINES   \ What the spec says it should be (for error reporting only)
250 constant MAX_LINE_LENGTH        \ max allowed length of a line (for error reporting only)
  1 constant COMMENT_BLOCK_INDENT   \ extra spaces to indent lines starting with '*' in a comment block
  4 constant INDENT_SPACES          \ spaces to indent for each indent level 
  0 constant INDENT_SPACES_XXX      \ spaces to indent for each  #ifdef ... #endif  indent level 
  0 constant DISABLE_MD5_IN_FOLDER_REPORT?    \ true disables MD5 calculation in the Folder Report
  1 constant INDENT_CASE_RIGHT      \ code within a case statement is shifted right by one indent if true

\ Compatibility
: COUNTER ( - n)   ms@ 182 10000 */ ;

VARIABLE CurrentFilename  max-path allot \ the current file name

0 value GetHandleOfMainWindow    \ the main window's handle

148 constant ScreenWidth        \ width in characters
28 constant ScreenHeight        \ height in characters

 8 constant char-width-main     \ character width in pixels
16 constant char-height-main    \ character height in pixels

 7 constant char-width-help     \ character width in pixels
14 constant char-height-help    \ character height in pixels

variable SavedDispMode

variable StackDepthValue   \ used to saved stack depth changes deep in code...

: .StackDepths
   depth if   cr ." *** depth = "  depth .   ."     TOS = "  dup . depth 1 > if  over .  then   then
   StackDepthValue @ if  ." *** StackDepthValue change = "  StackDepthValue @ . then
;

variable ShowZeroFileLengthError 

: .HEX ( u n)   base @ >r  hex  u.r  r> base ! ;
: D.HEX ( d n)   base @ >r  hex  d.r  r> base ! ;

: 2**   ( c -- u )   \ takes 2 to the power c
   1 swap lshift
;

: GotoColumn ( n -- ) 
   c# @ - 0 max  0 ?do  space  loop
;

decimal
0 constant FIRST_TAB
40 constant SECOND_TAB_SHORT
80 constant SECOND_TAB

: emit+wrap ( c -- )   
   c# @ SECOND_TAB 2 - > if  cr  FIRST_TAB GotoColumn 3 spaces  then  
   emit  
;

: emit+wrap_short ( c -- )   
   c# @ SECOND_TAB_SHORT 2 - > if  cr  FIRST_TAB GotoColumn 3 spaces  then  
   emit  
;

: type+wrap ( c-addr n -- )   over + swap ?do  i c@  emit+wrap  loop ;

: type+wrap_short ( c-addr n -- )   over + swap ?do  i c@  emit+wrap_short  loop ;

: tttw   cr  s" ABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZ" type+wrap ;

: Buffer: ( n -- )    create  allot ;

variable InputFileLen           \ length of the whole input file
variable OutputFileLen          \ length of the whole output file
variable InputFileLenSave       \ length of the whole file

variable NumInputLines          \ number of lines in the input file

variable InputFilePtr           \ address of the memory for input file
variable OutputFilePtr          \ address of the memory for output file
variable InputFileSavePtr       \ address of the memory for file
variable LineArrayPtr           \ address of the lines memory for file


\ variable LineArrayPtr2          \ address of the saved lines memory for file

\ The LineArray fields :
1024 CONSTANT BYTES_PER_LINE
BYTES_PER_LINE 10 CELLS - CONSTANT MAX_LINE_SIZE
\ data[MAX_LINE_SIZE] | length | comment status | Indent{} | Indent() | spare |
40000 CONSTANT MAX_NUM_LINES     \ was 2048 then 8192 then 16384 HPO  5 Sep 2002 : what we can handle

2048 1024 * constant FILE_BUFFER_SIZE  \ malloc'ed so can be big

MAX_NUM_LINES 1+ BYTES_PER_LINE *  constant LINE_ARRAY_SIZE


: de_init_buffers             \ remove the four buffers
   InputFilePtr @ ?dup if  free  drop   0 InputFilePtr !  then
   OutputFilePtr @ ?dup if  free  drop  0 OutputFilePtr !  then
   InputFileSavePtr @ ?dup if  free  drop  0 InputFileSavePtr !  then
   LineArrayPtr @ ?dup if  free  drop  0 LineArrayPtr !  then
\   LineArrayPtr2 @ ?dup if  free  drop  0 LineArrayPtr2 !  then
;

: init_buffers
   \ de_init_buffers 
\   0 InputFilePtr ! 
\   0 OutputFilePtr !
\   0 InputFileSavePtr !
\   0 LineArrayPtr !
\   0 LineArrayPtr2 !

   FILE_BUFFER_SIZE allocate 0= if  InputFilePtr !  then
   FILE_BUFFER_SIZE allocate 0= if  OutputFilePtr !  then
   FILE_BUFFER_SIZE allocate 0= if  InputFileSavePtr !  then
   LINE_ARRAY_SIZE allocate 0= if  LineArrayPtr !  then
\   LINE_ARRAY_SIZE allocate 0= if  LineArrayPtr2 !  then
;


include CRC16.f
include MD5.f
include NumName.f
include walk.f 
include BigStrings.f

include Graphics.f  \ line drawing etc
include Menu.f

\ allow Windows to Paint the Window so we can see the text we have written...
: LetMeSee
   Refresh: mainWindow  
   ekey? drop
;

include Help.f
include Files.f


\ ---------------------------------------------------------------
\ Top Level program starts here
\ ---------------------------------------------------------------

: ekey+ ( - c )   ekey dup memit ;

VARIABLE MYSPAN
VARIABLE MAXSPAN
VARIABLE MYPTR
variable theKey

: BACKSPACE
   GetXY l# ! c# !
   MYSPAN @ DUP >R  1 - 0 MAX  MYSPAN !
   32 MYPTR @ MYSPAN @ + C!
   L# @ C# @ 1 - 0 MAX  swap GotoXY
     R> IF  32 EMIT  THEN
   L# @ C# @ 1 - 0 MAX  swap GotoXY
;

: bs backspace ;

: ACCEPT+ ( a n - n)
   DUP 0= IF  2DROP  EXIT  THEN

   MAXSPAN !
   MYPTR !
   0 MYSPAN !

   BEGIN
      key DUP theKey !
      CASE
           8 OF  32 theKey !  BACKSPACE  ENDOF
          13 OF  32 theKey !  SPAN @  MAXSPAN ! ENDOF
         127 OF  32 theKey !  BACKSPACE  ENDOF

         theKey @ DUP  MYPTR @ MYSPAN @ + C!  EMIT

         1 MYSPAN +!

      ENDCASE
\      theKey @ cr ." key = " . ."   span = " MYSPAN @ . ."   addr = " MYPTR @ .
   MYSPAN @ MAXSPAN @ > UNTIL
   MYSPAN @
;

: TA1  PAD 10 ERASE  PAD 10 ACCEPT+  CR ." >>> " DUP . ."  | "  PAD SWAP  TYPE ;


: quit+
   mconsole
   ['] ekey is key
   ['] accept+ is accept
   ['] memit is emit
   cls  cr ."  ok"

                RP0 @ RP!
                BEGIN   [COMPILE] [
                        ?LOADING OFF

                        BEGIN   CR  QUERY-INTERPRET ?DUP 0=
                        WHILE   STATE @ 0=
                                IF      ."  ok"
                                        DEPTH .SMAX @ MIN 0
                                        ?DO  [CHAR] . EMIT  LOOP
                                THEN
                        REPEAT
                        DUP 1+          \ no message on abort
                        IF      MESSAGE
                        THEN
\                        RESET-STACKS    \ reset the stacks
                AGAIN
;

: expired ( ms -- t)
   counter -  -86400000 1 WITHIN ;


: ?keyRefresh ( -- c )
   begin 
      counter 1 and if     \ ( 18.2 / 2 ) Hz 
         Refresh: mainWindow
      then
   Key? until  
   drop 
;


: ShowFolderReportOptions
   FolderReportJustLintErrorFiles @ if
      page  ." FolderReport only lists files with Lint errors"  
   else
      page  ." FolderReport lists all files in the selected folder"  
   then
   LetMeSee
   2000 ms 
;

\ *****************************************************************************

256 Buffer: ShellString0[]
256 Buffer: ShellString1[]

: >SHELL ( addr n -- )   
   BL SKIP  2DUP BL SCAN  2SWAP  THIRD -
   ( a n a n)  ShellString0[] ZPLACE  ShellString1[] ZPLACE
   0
   NULL
   NULL
   ShellString0[] rel>abs
   ShellString1[] rel>abs
   conhndl 
   Call ShellExecute drop             
;

: ForthConsole ( -- )
   basic-forth-io  
   \ page 
   cr ." Forth Console e.g type : 'loc Cweed4 g'" 
   cr  ."  ok " 
   cr  
   quit  
;

: ForthConsolePlus ( -- )
   s" Forth Console"  s" Click on 'OK' to run the Forth Console" 
   Dialogue
   case
      1 of ( OK ) ForthConsole  endof
      2 of ( Cancel ) ( 2000 200  tone  23 throw )  endof
   endcase
;

\ *****************************************************************************

: Cweed4() ( -- )

   decimal

   0 FullLineInfo !     \ do not display full Line information

   GetFile      \ if its there, load it in and parse it, else InputFileLen = 0
   SaveOriginal \ save the original file, if it was loaded

   begin

      RefreshDisplay
      Refresh: mainWindow
   
      key             \ handle keyboard interpretation

      dup 'a' 'z' 1+ within if   223 and  then    \ ignore case

      dup >r
      r@ 'M' =
      r@ 'O' +k_control  = or
      r@ k_F1 +k_control = or
      r> k_F1 = or
      not if   \ not if Undo or Help or Select File
         ?SelectFile       \ open a file if not already done so
      then

      case

      k_F1            of  DispMode @ SavedDispMode !  5 DispMode ! ( ShowHelp )  endof
      k_F1 +k_control of  about-demo         endof
      k_F2            of  ApplyScriptRulesPlus  7 DispMode !  endof
      k_F4            of  s" Cweed4_help.pdf" >SHELL  endof
      k_F4 +k_control of  ForthConsolePlus  endof
      
      'O' +k_control  of   SelectFile   endof
      'Q' +k_control  of   ApplyCapitaliseRulesPlus  endof
      'S'             of   filtering off  GetFile  1 DispMode !  endof
      'I'             of   filtering off  GetFile  2 DispMode !  0 StartLine!   endof
      'J'             of   filtering on   GetFile  2 DispMode !  0 StartLine!   endof
      'L'             of   FolderReportJustLintErrorFiles -1 toggle  ShowFolderReportOptions  endof
      'V'             of   filtering off  GetFile  3 DispMode !  0 ?FixIndents !  endof
      'X'             of   filtering off  GetFile ( 3 DispMode ! ) -1 ?FixIndents !  DoIndents  endof


      '2'             of   2 SetTabSize ReplaceTabsPlus SaveToFile  4 SetTabSize  endof
      '3'             of   3 SetTabSize ReplaceTabsPlus SaveToFile  4 SetTabSize  endof
      '4'             of   4 SetTabSize ReplaceTabsPlus SaveToFile  endof
      '8'             of   8 SetTabSize ReplaceTabsPlus SaveToFile  4 SetTabSize  endof

      'U'             of   PC->Unix                  SaveToFile  endof
      'P'             of   Unix->PC                  SaveToFile  endof
      'Q'             of   Convert//Comments         SaveToFile  endof  \ <-- New!!! 2007Dec10
      'R' +k_control  of   BlocksToNewFile                       endof  \ <-- New!!! 2008Mar07

      'T'             of   RemoveTrailingSpaces      SaveToFile  endof
      'B'             of   RemoveTwoBlankLines       SaveToFile  endof
      'W'             of   Weed                      SaveToFile  endof


      'M'             of   RestoreOriginal           SaveToFile  endof
      'Z' +k_control  of   RestoreOriginal           SaveToFile  endof

      'F'             of   -1 FolderFilesC&Honly !  DispMode @ SavedDispMode !  6 DispMode !  page  ." Show Folder "  ShowFolderFilesTypes  ."   ...  " LetMeSee  endof
      'R'             of   0 FolderFilesC&Honly !  DispMode @ SavedDispMode !  6 DispMode !  page  ." Show Folder "  ShowFolderFilesTypes  ."   ...  " LetMeSee  endof

      'N'             of   filtering off  GetFile  4 DispMode !  endof
      'N' +k_control  of   filtering on   GetFile  4 DispMode !  endof

\      'K'             of   page  ttemits  key drop  endof

      'Q' +k_control  of   cr ." ok "  cr  quit   endof
      'F' +k_control  of   FullLineInfo @  0=  FullLineInfo !  endof


      'B' +k_control  of   RemoveBit7sAndDels        SaveToFile  endof
      'C' +k_control  of   RemoveComments            SaveToFile  endof
      'S' +k_control  of                             SaveToFile  endof \ save

     k_esc            of          exit                               endof
     k_esc +k_control of                                         endof


       131088 ( Page up )   of  StartLine @ DisplayHeight -  StartLine!  endof
       131089 ( Page down ) of  StartLine @ DisplayHeight +  StartLine!  endof
       131072 ( Home )      of  0 StartLine!  endof
       131073 ( End )       of  100000 StartLine!  endof
       131078 ( Up )        of  StartLine @ 1 -  StartLine!  endof
       131079 ( Down )      of  StartLine @ 1 +  StartLine!  endof
     endcase

   again
;

: Cweed4 ( -- )
   Start: mainWindow
   GetHandle: mainWindow to  GetHandleOfMainWindow
   StartPos: mainWindow 50 + swap 50 + swap message-origin

   0 fid !
   s" MySrc.c" CurrentFilename place

   4 SetTabSize \ default tab spacing
   0 StartLine ! \ where to start displaying the file
   0 DispMode ! \  0 = noop, 1 = ScanFile,  2 = ShowFile, 3 = ShowIndents

   0 NoMD5 !   \ enable MD5 calculations for FolderReport ( these may be disabled for speed )

   0 FolderReportJustLintErrorFiles !

   \ Below sets the default file types - use 'R' or 'F' to select from the keyboard before running a folder report
   \ 0 FolderFilesC&Honly ! \ original 4.2 *.c* and *.h* and *.ini        'R'
   -1 FolderFilesC&Honly ! \ new 4.21 *.c and *.h                       'F'

   0 StackDepthValue !  \ used to check for stack over- or under-flow in folder report CheckFile etc...
   -1 ShowZeroFileLengthError !  \ used to enable the warning window when a zero length file is found

   Begin ['] Cweed4() Catch  again 
;

: ttt ( -- )   Cweed4 ;

(( 
: TTT2
   s" mysrc.c" CurrentFilename place
   InitVariables
   GetFile
   0 LineNumber !
   .Line
;

: TT
   1 LineNumber +!
   .Line
;
))

$4000 $4000 ' Cweed4 APPLICATION Cweed4.exe
\ ' Cweed4 turnkey Cweed4.exe       \ build an application on disk
\ Note : if you get an error here, run Win32for.exe and type RE-REGISTER , then try again.

\ MODULE          \ end of the module
