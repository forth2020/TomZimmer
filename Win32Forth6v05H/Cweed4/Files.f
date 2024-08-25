\ Files.f  Howerd Oakford  2022 Jul 07  File processing using the V2.0 Line array
\ added *.c*  to file selection list

variable fid \ the currently open file's ID

variable numbers 256 cells allot

: numbers@ ( c - n)   cells numbers + @ ;

variable #//s   \ number of 'single line comment' "//" pairs
variable #/*s   \ number of 'comment' "/*" pairs
variable maxline
variable >f-key
variable accumulate
variable accumulateIndentErrors

variable LineNumber           \ the current line number
variable TotalLinesShown      \ filtering mode displayable lines in file

variable Lastf-key            \ the last character received
variable LastLastf-key        \ the last last character received

variable eof                  \ the end of the file has been reached
variable filtering            \ 0x01 remove comments 0x02 remove whitespace
variable Checksum             \ sum of bytes
variable CRCchecksum          \ CRC32

variable LastSavedChecksum    \ sum of bytes of my last saved file

variable StartLine            \ Display starting on this line
variable LastLineWasBlank
variable Result
variable FileFormat           \ 1 = PC format file, 2 = Unix format, 0 = neither
variable 'DoLine

variable IndentLevel{}        \ Indent level for curly brackets
variable IndentLevel()        \ Indent level for normal brackets
variable IndentLevelNS        \ Indent level for the namespace keyword
variable IndentLevel#XXX      \ Indent level for #if ... #endif

variable NextIndentLevel{}    \ delayed value of Indent level for curly brackets
variable NextIndentLevel()    \ delayed value of Indent level for normal brackets
variable NextIndentLevelNS    \ delayed value of Indent level for the namespace keyword
variable NextIndentLevel#XXX  \ delayed value of Indent level for #if ... #endif

variable LastWasWhiteSpace
variable v_Comments           \ 1 if we have started a /* ... comment, 2 for a \\, else 0

variable v_Last_LineStatus    \ the last value of the LineStatus that we saw

variable start_of_//_comment

variable v_start_of_comments_line
variable v_end_of_comments_line
variable v_number_of_//_comments
variable v_last_number_of_//_comments

variable v_NumberOfPrecedingBlankLines 
variable v_last_NumberOfPrecedingBlankLines


variable DispMode             \ format of results display
variable DispModeSave         \ saved format of results display
variable ?FixIndents          \
variable DoNotSave            \ RAM restore copy of the file is invalid

variable 'DoChar
variable CharResult
variable EndOfLine
variable PositionInLine

variable FullLineInfo         \ display full Line information if true ( see .Line )
variable NoMD5                \ suppress the calculation and display of the MD5 hash, for speed

variable #LintSaves           \ number of "lint -save"
variable #LintRestores        \ number of "lint -restore"
variable #LintRestoreErrors   \ number of "lint -restore -e" errors
variable #LintSaveRestoreErrors   \ number of save/-restore unbalanced errors
variable #LintSpaceErrors     \ number of "//   lint" instead of "//lint" errors
variable #LintEsymErrors      \ -save -esym on the sme line - esyms are not saved!
variable #LintStackDepthMin   \ checks for restore/save in the wrong order
variable #LintStackDepthMinError \ saves a save/restore stack underflow
variable #LintLastLine        \ the line number when the last lint error occurred

: InitVariables ( -- )   \ initialise the whole-file variables
   0 >f-key !
   0 maxline !  0 LineNumber !
   eof off
   0 IndentLevel{} !
   0 IndentLevel() !
   0 IndentLevelNS !
   0 IndentLevel#XXX !
   0 NextIndentLevel{} !
   0 NextIndentLevel() !
   0 NextIndentLevelNS !
   0 NextIndentLevel#XXX !
   -1 LastWasWhitespace !

   0 v_Comments !
   0 v_Last_LineStatus !
   0 start_of_//_comment !
   0 v_start_of_comments_line !
   0 v_end_of_comments_line !
   0 v_number_of_//_comments !
   0 v_last_number_of_//_comments !
   0 v_NumberOfPrecedingBlankLines !     
   0 v_last_NumberOfPrecedingBlankLines !
;

: InitVariablesLint ( -- )             \ initialise whole file Lint variables
   0 #LintSaves !
   0 #LintRestores !
   0 #LintRestoreErrors !
   0 #LintSaveRestoreErrors !
   0 #LintSpaceErrors !
   0 #LintEsymErrors !
   0 #LintStackDepthMin !
   0 #LintStackDepthMinError !
   0 #LintLastLine !
;

\ ************************  line data access  ***************************
: >LineStart# ( line# -- a )   \ the start address of all data for the given line
   BYTES_PER_LINE * LineArrayPtr @ +
;

: LineStartPtr ( -- a )   \ the start address of all data for the current line
   LineNumber @ >LineStart#
;

: LineLength ( -- a )
   LineStartPtr MAX_LINE_SIZE +
;

\ Properties for the current line
\ 0x01 = cr, 0x02 = lf, 0x10 = discard on output, 0x100 /* ... */, 0x200 = // , 0x400 = start of >3 line "//" comment block
: >LineStatus ( line# -- a )
   >LineStart# MAX_LINE_SIZE + cell +
;

: LineStatus ( -- a )
   LineNumber @ >LineStatus
;

\ returns true on the first line of a 3 line "//" or "/*...*/" comment block
: StartOf//CommentBlock ( -- f )
   LineStatus @ 0x00000400 and 0= 0=
;

\ returns the current line's v_Comments state, true if the line is a "//" comment
: //_comment ( -- f )
   LineStatus @ 0x00000200 and 0= 0=
;

\ returns the current line's v_Comments state, true if the line is a comment
: WithinComments ( -- f )
   LineStatus @ 0x0000ff00 and 0= 0=
;

\ The indent level created by { and }
: LineIndent{} ( -- a )
   LineStartPtr MAX_LINE_SIZE + 2 cells +
;

\ The function parameter indent level created by ( and )
: LineIndent()  ( -- a )
   LineStartPtr MAX_LINE_SIZE + 3 cells +
;

\ The function parameter indent level created by the namespace keyword
: LineIndentNS  ( -- a )
   LineStartPtr MAX_LINE_SIZE + 4 cells +
;

\ The function parameter indent level created by ( and )
: LineIndent#XXX  ( -- a )
   LineStartPtr MAX_LINE_SIZE + 5 cells +
;

\ The number of preceding blank lines
: >LinePrecedingBlanks  ( line# -- a )
   >LineStart#  MAX_LINE_SIZE + 6 cells +
;

\ look at this line's indent state
: GetCurrentIndent ( -- n )
   LineIndent{} @  LineIndent() @ +
   LineIndent#XXX @ +  \ HPO 2014 May 21
   LineIndentNS @ -  \ subtract the namespace offset
;

\ look at the previous line's indent state
: GetLastIndent ( -- n )
   LineNumber @ 0= if  0  exit  then
   -1 LineNumber +!
   GetCurrentIndent
   1 LineNumber +!
;

: GetNextIndent ( -- n )
   LineNumber @ NumInputLines @ >= if  0  exit  then
   1 LineNumber +!
   GetCurrentIndent
   -1 LineNumber +!
;

\ ********************  by line and by file actions ***********************

: DoLine ( xt -- )   'DoChar !
   0 CharResult !
   LineLength @ 0 max  BYTES_PER_LINE min 0 ?do
      LineStartPtr i + c@
     'DoChar @ ?dup if  execute  then
   loop
;

: DoLines ( xt -- )   'DoLine !

   InitVariables

   0 OutputFileLen !    \ start to construct the output file at the beginning
   0 Result !  0 LastLineWasBlank !  0 eof !

   NumInputLines @ 0 max MAX_NUM_LINES min 0 ?do
      i LineNumber !
      'DoLine @ ?dup if  execute  then
   loop
;

\ like +! , but OR's the bits in u into the variable
: +or ( u a -- )  >r  r@ @ or r> ! ;

\ like +! , but AND's out the bits in u from the variable
: -or ( u a -- )
   >r -1 xor  r@ @ and r> ! ;

\ *************************  File <--> Buffer  ****************************

: SaveOriginal ( -- )

   InputFileLen @ if
      \ save the file for Restore
      InputFilePtr @ InputFileSavePtr @ InputFileLen @ move
      InputFileLen @ InputFileLenSave !

      \ save the checksum of this data
      Checksum @ LastSavedChecksum !
  then
;

: ScanFile ( -- )   \ scan the input file buffer for information about the file

    InitVariables

    numbers 256 cells erase

    0 Checksum !
    InputFileLen @ 0 ?do
       1 numbers InputFilePtr @ i + c@  dup Checksum +! cells + +!
    loop

    InputFilePtr @ InputFileLen @ CRC32 CRCchecksum !  \ calculate the CRC32 for the input file

\    NoMD5 @ 0= if
       InputFilePtr @ InputFileLen @ MD5                  \ calculate the MD5 hash for the input file
\    then

\    10 numbers@ 1+ NumInputLines !                 \ save for use as a loop limit

    0 FileFormat !

    13 numbers@ 10 numbers@ =
    10 numbers@ 0= not and  if                  \ #crs = #lfs != 0
       1 FileFormat !       \ its a PC format file
    then

    13 numbers@ 0= 10 numbers@ 0= not and if    \ #crs = 0, #lfs != 0
       2 FileFormat !       \ its a Unix format file
    then

    InputFileLen @ 0= if
       3 FileFormat !       \ 0 length file
    then
;

: File->InputBuf ( -- )   \ get the file into the file buffer
   CurrentFilename count r/o open-file abort" open-file error!" fid !
\   0 fid @ reposition-file abort" reposition-file error!"
   fid @ file-size abort" file-size error!" drop ( file-size returns a double )
   ( File length ) dup >r  FILE_BUFFER_SIZE min  0 max  InputFileLen !
   InputFilePtr @ InputFileLen @ fid @ read-file  abort" read-file error!"
   fid @ close-file
   2drop
   DoNotSave off
   r> ( File length ) FILE_BUFFER_SIZE > if
      2 DoNotSave +or     \ file is too big to fit in the buffer ( or 0 length )
   then

   InputFileLen @ 0=
   ShowZeroFileLengthError @ and
   if
       1000 300 tone    \ make a beep
       s" Oops!"
       s" File " pad place
       CurrentFilename count pad +place
       s"  has zero length!"  pad +place
       pad count Monologue
       2 DoNotSave +or          \ file is 0 length
       6 throw                  \ abort now
   then

   ScanFile      \ scan the input file buffer for information about the file
;

\ ***********************  Single characters  *********************************

(( \ Not used, but could be useful : gets chars from the LineArray
variable >L-key
variable 'EndOfLineAction

: \L-key
   0 >L-key !  0 LineNumber !
   0 'EndOfLineAction !
;

: L-key() ( - c ]          \ raw get character from the LineArray

   begin
      >L-key @ LineLength @ >=
   while
      'EndOfLineAction @ ?dup if  execute  then
      0 >L-key !
      1 LineNumber +!
      LineNumber @ NumInputLines @ > if  11 throw  then
   repeat

   >L-key @ LineStartPtr + c@
   1 >L-key +!
;
))

: f-key() ( -- c )               \ raw get character
   >f-key @ InputFilePtr @ + c@
   1 >f-key +!
   >f-key @ InputFileLen @ > if  -1 eof !  10 throw  then
;

: f-key-() ( -- c ) \ optionally remove all duplicate whitespace except linefeeds

   filtering @ 0x02 and if
      0                         \ dummy key
      begin
         drop
         f-key() >r

         r@ 33 255 1+  within     \ it is a printable character
         r@ 10 = or               \ or a linefeed
         r@ 13 = or               \ or a cr
         dup 0= if
            drop
            r> drop 32  >r  \ replace by a space
            LastWasWhiteSpace @
            0=   \ the last one was not, so output this one as a space
\           drop -1 \ output all whitespace
         then

         r@ 0 32 1+ within LastWasWhiteSpace !
         r>  swap
      until
   else
      f-key()
   then
;

: Nextf-key() ( - c) \ the next get character
   >f-key @ >r  \ save position in file
   f-key-()
   r> >f-key !  \ restore position in file
;

$400 constant |LocalLineBuffer|
|LocalLineBuffer| Buffer: LocalLineBuffer[]

: GetNextF-keysToEndOfLine ( -- a c )   \
   >f-key @ >r  \ save position in file
   LocalLineBuffer[] |LocalLineBuffer| erase
   |LocalLineBuffer| 1 do  f-key-() dup LocalLineBuffer[] i + c!  $0A = if  i LocalLineBuffer[] c!  leave  then loop
   r> >f-key !  \ restore position in file
   LocalLineBuffer[] count
;

: NumberOf_characters ( -- c )
   0  \ initial count
   GetNextF-keysToEndOfLine over + swap ?do  i c@ [char] _ = if  1+  then  loop
;

: FindDoubleUnderscore? ( -- f )
   GetNextF-keysToEndOfLine s" __" SEARCH >r 2drop r>
;

: f-key-comments() ( -- c )      \ fetch a character and update the comment state
   f-key-() >r

   r@ [char] / =  if        \ first "/"
      Nextf-key() case
         [char] * of
            v_Comments @ 2 and 0= if      \ do not detect /* after //
               1 v_Comments +or
            \   0x100 LineStatus +or \ mark comment state of the line
            then
          endof      \ "/*"
         [char] / of
            v_Comments @ 1 and 0= if      \ do not detect // inside /* ... */
               2 v_Comments +or
               0x00000200 LineStatus +or \ mark comment state of the line
            then
         endof      \ "//"
      endcase
   then

   v_Comments @ 1 and 1 =              \ */ comment
   LastLastf-key @ [char] * = and
   Lastf-key @ [char] / = and
   if
      0x100 LineStatus +or       \ mark comment state of the line
      0 v_Comments !             \ we have found a */ so terminate state
   then

   v_Comments @ 2 and 2 =        \ // comment
   r@ 10 = and                   \ linefeed
   if
      v_Comments @ 0x100 * LineStatus +or \ mark comment state of the line
      0 v_Comments !             \ we have hit a new line, so terminate // state
   then

   v_Comments @ 0= if             \ not commented out
       r@ case
          [char] ( ( )   of  INDENT_SPACES NextIndentLevel() +!  endof
          [char] )       of  INDENT_SPACES NEGATE NextIndentLevel() +!  endof
          [char] { ( } ) of  INDENT_SPACES NextIndentLevel{} +!  endof
          [char] }       of
            NextIndentLevel{} @  NextIndentLevelNS @
\            NextIndentLevel{} @ 2 < if
\               cr ." } LineIndent = "  over .  ."   NS = " dup .
\            then
            = if    \ the last closing } for namespace
\            ."  <><><> "
\               LineStartPtr LineLength @  s" namespace" SEARCH 0= if
\                   cr ." Closing namespace } found with no 'namespace' tag on line "  LineNumber @ 1+ 3 u.r  ."  !"
\                   cr ." >>>"  LineStartPtr LineLength @ type
\               then
\               2drop
               NextIndentLevelNS @ INDENT_SPACES - 0 max  NextIndentLevelNS !  \ end of namespace {...} bracket pair
            then
            INDENT_SPACES NEGATE NextIndentLevel{} +!
          endof
       endcase
   then

   LastLastf-key @ [char] # =
   Lastf-key @ [char] i = and
   r@ [char] f = and
   if
      FindDoubleUnderscore? not if
         INDENT_SPACES_XXX NextIndentLevel#XXX +!
      then
   then

   LastLastf-key @ [char] # =
   Lastf-key @ [char] e = and
   r@ [char] n = and
   if
      FindDoubleUnderscore? not if
         INDENT_SPACES_XXX negate NextIndentLevel#XXX +!
      then
   then

   Lastf-key @ LastLastf-key !  \ save this too
   r@ Lastf-key !               \ save it
   r>
;

: f-key-no-comments() ( -- c )   \ fetch a character, ignore comments
   0
   Begin
      drop
      f-key-comments()
      dup 10 =                  \ pass linefeeds through
      v_Comments @ 0= or
   until
;

: f-key ( -- c )
   filtering @ if
      f-key-no-comments()
   else
      f-key-comments()
   then
;

: www ( f -- )  filtering !  InitVariables  cr  200 0 do  f-key
      dup 10 = if  cr  then
    emit  loop ;

: femit ( c -- )
   OutputFilePtr @ OutputFileLen @ + c!
   1 OutputFileLen +!
   OutputFileLen @ FILE_BUFFER_SIZE > if
      -1 OutputFileLen +!       \ limit to the buffer
      8 DoNotSave +or
   then
;

\ ********************** Line analysis ***********************

: #NonSpaces() ( c -- )   32 > if  1 CharResult +!  then ;
: #NonSpaces ( -- n )   ['] #NonSpaces() DoLine  CharResult @ ;

: #Spaces() ( c -- )   32 = if  1 CharResult +!  then ;
: #Spaces ( -- n )  ['] #Spaces() DoLine  CharResult @ ;

: #PageBreaks() ( c -- )   12 = if  1 CharResult +!  then ;
: #PageBreaks ( -- n )  ['] #PageBreaks() DoLine  CharResult @ ;

: #CRs() ( c -- )   13 = if  1 CharResult +!  then ;
: #CRs ( -- n )  ['] #CRs() DoLine  CharResult @ ;

: #nonprintable() ( c -- )   33 < if  1 CharResult +!  then ;
: #nonprintable ( -- n )  ['] #nonprintable() DoLine  CharResult @ ;

\ return true if the curretn line is blank
: BlankLine ( -- f )   \ true if line has no printable chars ( and no pagebreak )
   #NonSpaces
   #Pagebreaks = \ only pagebreaks or spaces
;

: -Leading ( a n - a' n' )
   dup 0= if  exit  then 
   2dup 
   over + swap ?do
      i c@ $20 > if  leave  then
      1 /string
   loop  
   0 max
;

\ returns true if string $ starts with string $tag, ignoring leading whitespace
: StartsWith ( $ $tag -- f )
   2>r
   dup 2r@ swap drop < if  2r> 2drop 2drop 0 exit  then  \ ignore an empty $
   -Leading drop     \ the address of the $ string
   2r>               \ the $tag string
   rot over          \ $ string with the length of the $tag string
\   cr GREEN.  2dup type 2 spaces 2over type RESTORE. 2 spaces
   compare 0=
;

: ShowOK ( -- )
\   2 spaces  GREEN. ." ok" RESTORE.
   \ 60 ToColumn 
   \ GREEN. 
   ." ok" 
   \ RESTORE.
;

\ : ShowDone ( -- )   GREEN. cr ." done" RESTORE. ;

variable v_fail_errors

: show_Pass/Fail ( f -- )  \ passes true
   if  
      ."  Passed "  
   else  
      1 v_fail_errors +!
      ."  Failed !!! "   
   then 
;

: tt_StartsWith ( -- )
   0 v_fail_errors !
   cr  s" 0 length string           "  2dup type  drop 0 2 spaces s" --int" 2dup type         2 spaces StartsWith dup 2 .r  0 = show_Pass/Fail
   cr  s" does not start with --int "  2dup type         2 spaces s" --int" 2dup type         2 spaces StartsWith dup 2 .r  0 = show_Pass/Fail
   cr  s" --int starts with         "  2dup type         2 spaces s" --int" 2dup type         2 spaces StartsWith dup 2 .r -1 = show_Pass/Fail
   cr  s"    --int starts with      "  2dup type         2 spaces s" --int" 2dup type         2 spaces StartsWith dup 2 .r -1 = show_Pass/Fail
   cr  s"    --int starts with      "  2dup type         2 spaces s" 0 len" 2dup type drop 0  2 spaces StartsWith dup 2 .r -1 = show_Pass/Fail
   v_fail_errors @ 0= if  cr ." All tests passed :-) "  else  cr v_fail_errors @ . ." tests FAILED!!!"  then
;

\ *************************  Buffer <-> LineArray  ****************************

\ The LineArray holds each line in a fixed length field.
\ Linefeeds and cr's are removed from the data, but flagged in the LineStatus.
\ cr's other than just before the linefeed are ignored
\ Indent and Comment information is put in the LineStatus ( see fkey ).

variable LastCharacter

: EndOfLineActions ( -- )

   LastCharacter @ 13 = if        \ the line ends with a cr
      0x01 LineStatus +or         \ bit 0 = cr, bit 1 = lf
   then

   v_Comments @ 0= LineLength @ 0<> and if
      LineStartPtr 9  s" namespace" SEARCH  \ must be at the start of the line
      if
         INDENT_SPACES NextIndentLevelNS +!
      then
      2drop
   then

   v_Comments @ 0x01 and 0x100 * LineStatus +or      \ save the /*...*/ Comment state

   IndentLevel() @ LineIndent() !         \ store last time's values
   IndentLevel{} @ LineIndent{} !
   IndentLevelNS @ LineIndentNS !
   IndentLevel#XXX @ LineIndent#XXX !
   NextIndentLevel{} @ IndentLevel{} !    \ save this time's values
   NextIndentLevel() @ IndentLevel() !
   NextIndentLevelNS @ IndentLevelNS !
   NextIndentLevel#XXX @ IndentLevel#XXX !

   \ Count up the number of preceding blank lines
   BlankLine if 
      1 v_NumberOfPrecedingBlankLines +!
   then

   LineStatus @ 0x00000100 and if            \ do nothing inside /* ... */
      \ do not count up blank lines in a /*...*/ comment block
      0 v_NumberOfPrecedingBlankLines !
      exit           \ do not do anything else...
   then

\   LineStartPtr LineLength @ s" //" StartsWith if   \ the line starts with "//"
   LineStartPtr 2 s" //" compare 0= if   \ the line starts with "//", on the left margin
      0x00000200 LineStatus +or  \ mark C++ "//" comment state of the line
      1 v_number_of_//_comments +!
   else
      0 v_number_of_//_comments !
   then

   \ Detect and save the start and end of a "//" comment line or block
   LineStatus @ 0x00000200 and  v_Last_LineStatus @ 0x00000200 and = not if   \ change from "//" comment to not "//" comment
      LineStatus @ 0x00000200 and 0= not if  \ start of "//" comment block
         LineNumber @ v_start_of_comments_line !                                 \ save the line number where the //comment block started      
         v_NumberOfPrecedingBlankLines @ v_last_NumberOfPrecedingBlankLines !    \ save the number of preceding blank lines
      else  \ end of "//" comment block
         LineNumber @ v_end_of_comments_line !

         v_end_of_comments_line @  v_start_of_comments_line @ - 3 >= if   
            0x00000400 v_start_of_comments_line @ >LineStatus +or    \ mark the start of "//" comment block, on the start-of-block line

            v_last_NumberOfPrecedingBlankLines @ 2 < if                 \ mark if there are two or more preceding blank lines
               0x00000800 v_start_of_comments_line @ >LineStatus +or    \ mark blank lines preceding the comment block
            then
         
         then

         v_last_NumberOfPrecedingBlankLines @ v_start_of_comments_line @ >LinePrecedingBlanks !    \ just for debug...
      
      then
      LineStatus @ v_Last_LineStatus !
   then

   \ Reset the number of preceding blank lines
   BlankLine not if  
      0 v_NumberOfPrecedingBlankLines !
\      0 v_number_of_//_comments !
   then

   v_number_of_//_comments @ v_last_number_of_//_comments !
;

: Check_MaxLines_incr_and_reset ( -- )

   1 LineNumber +!                \ move to the next line in the array

   0 LineLength !                 \ start the next line with no length
   0 LineStatus !                 \ start with null line status

   LineNumber @ MAX_NUM_LINES 1- > if
      cr ." Hit the top!!! Ln#, Max" LineNumber @ . MAX_NUM_LINES .
      16 DoNotSave +or  16 throw
   then
;

: InputBuf->LineArray() ( -- )   \ convert to line by line format

      f-key >r                            \ fetch the character
      r@ 10 =  r@ 13 = or not if           \ do not store lf or cr
         r@ LineStartPtr LineLength @ + c! \ put others into the line buffer
\         r@ emit
         1 LineLength +!     \ count up the line length
         LineLength @ MAX_LINE_SIZE 1- > if  4 DoNotSave +or 17 throw  then
      then

      r@ 10 = if                        \ the end of a line
         0x02 LineStatus +or            \ bit 1 = linefeed
         EndOfLineActions
         Check_MaxLines_incr_and_reset
      then

      r> LastCharacter !                \ save it for the linefeed calc.
;

: InputBuf->LineArray ( -- )           \ convert to line by line format
   0 LineStatus !                       \ start with null line status
   0 LastCharacter !                    \ last character was not a cr

   0 LineNumber !                       \ start at the first line
   0 LineLength !                       \ start with no length
   0 >f-key !

   begin
      ['] InputBuf->LineArray() catch
   until

   LineLength @ 0= not if       \ line not terminated by lf
      EndOfLineActions          \ count the un-terminated line
      Check_MaxLines_incr_and_reset
   then

   LineNumber @ NumInputLines !         \ save the file info
;

: OutputBuf->InputBuf ( -- )
   OutputFilePtr @ InputFilePtr @ OutputFileLen @ move
   OutputFileLen @ InputFileLen !
   ScanFile                             \ update the file information
;

variable FileWriteError

: SaveToFile ( -- )   \ copy the output buffer to the current file

   DoNotSave @ if
      s" Sorry! No can do."
      s" Data is invalid, cannot save the file!" Monologue
      exit
   then   \ do not save garbage into the file...

   0 FileWriteError !   \ no error yet

   CurrentFilename count r/w open-file \ abort" Could not open the file!"
   if
       1000 300 tone    \ make a beep
       s" Sorry! No can do."
       s" File " pad place
       CurrentFilename count pad +place
       s"  is read only!"  pad +place
       pad count Monologue
       5 throw                  \ abort now
   then
   fid !
   0. fid @ resize-file  if
      1 FileWriteError !
   then

   OutputFilePtr @ OutputFileLen @ fid @ write-file  if
      2 FileWriteError !
   then

   fid @ close-file  drop

   FileWriteError @ if  6 throw  then

   \ save the checksum of this saved data
   0  OutputFileLen @ 0 ?do  OutputFilePtr @ i + c@ +  loop  LastSavedChecksum !

   \ update all our buffers to saved file's data
   OutputBuf->InputBuf
   InputBuf->LineArray                   \ convert to line by line format
;

: SaveToNewFile ( -- )   \ copy the output buffer to a new file

   CurrentFilename count r/w create-file \ abort" Could not open the file!"
   if
       1000 300 tone    \ make a beep
       s" Sorry! No can do."
       s" Could not create file " pad place
       CurrentFilename count pad +place
       pad count Monologue
       5 throw                  \ abort now
   then
   fid !

   0 FileWriteError !   \ no error yet

\   cr ." File length to create = "  OutputFileLen @ .

   OutputFileLen @ 0 ( takes a double value ) fid @ resize-file  if
      1 FileWriteError !
   then

   0. fid @ reposition-file if
      3 FileWriteError !
   then

   OutputFilePtr @ OutputFileLen @ fid @ write-file  if
      2 FileWriteError !
   then

   fid @ ?dup if  close-file  drop  then

   FileWriteError @ if  6 throw  then

   \ save the checksum of this saved data
   0  OutputFileLen @ 0 ?do  OutputFilePtr @ i + c@ +  loop  LastSavedChecksum !

   \ update all our buffers to saved file's data
   OutputBuf->InputBuf
   InputBuf->LineArray                   \ convert to line by line format
;

: RestoreOriginal ( -- )

   DoNotSave @ if
      s" Sorry! No can do."
      s" File data is invalid, cannot restore the file!" Monologue
      20 throw
   then   \ do not save garbage into the file...

   InputFileLenSave @ 0= if
      s" Sorry! No can do."
      s" No Restore data available!" Monologue
      21 throw
   then

   InputFileSavePtr @ 0= if
      s" Sorry! No can do."
      s" No Restore buffer available!" Monologue
      22 throw
   then

   File->InputBuf Checksum @ LastSavedChecksum @ = not if
      s" Just checking..."
      s" File has been changed by someone else! Overwrite the file anyway?"
      Dialogue
      case
         1 of ( OK )    endof
         2 of ( Cancel )  2000 200  tone  23 throw  endof
      endcase
   then

   InputFileSavePtr @ OutputFilePtr @  InputFileLenSave @ move
   InputFileLenSave @ OutputFileLen !
;

\ ****************************************************************************

: GetFile ( -- )
   ['] File->InputBuf catch if  0 InputFileLen !  1 DoNotSave +or  then
   InputBuf->LineArray
   -1 TotalLinesShown !         \ mark as not set
;

\ ************************* Line by line to RAM ********************************

: OneLine->Output ( -- )   \ convert line by line format to RAM
   LineStatus @ 0x10 and if  exit  then     \ ignore a discarded line
   ['] femit DoLine                         \ output the main text
   LineStatus @ 1 and if 13 femit  then     \ and a cr if required
   LineStatus @ 2 and if 10 femit  then     \ and a lf if required
;

: LineArray->Output                 \ copy "line by line" data to the output buffer
   0 OutputFileLen !            \ initialise the output pointer
   ['] OneLine->Output DoLines  \ copy for each line
;

 \ ********************** Character analysis ***********************

: Cspace= ( c -- f )
   >r
   r@ 32 =           \ space
   r@ [char] ( = or  \ ( will count as a space
   r@ [char] ) = or  \ ) will count as a space
   r@ [char] [ = or  \ [ will count as a space
   r@ [char] ] = or  \ ] will count as a space
   r> drop
;

: Cvalid= ( c -- f )  \ only these characters are valid in a C name
   >r  0
   r@ [char] _ = or \ an underscore
   r@ [char] 0 [char] 9 1+ within or
   r@ [char] a [char] z 1+ within or
   r@ [char] A [char] Z 1+ within or
   r> drop
;

variable LineEvent

: #LeadSpaces() ( c -- )   32 = not if  -1 LineEvent !  then
     LineEvent @ 0= if  1 CharResult +!  then ;
: #LeadSpaces ( -- n )   0 LineEvent !
   ['] #LeadSpaces() DoLine  CharResult @ ;

\ returns true if the first non-space characters on the line are /*
: FirstComment ( -- f )
   #LeadSpaces  LineStartPtr + >r
   r@    c@ '/' =
   r@ 1+ c@ '*' = and
   r> drop
;

\ find a string preceeded by spaces and followed by a character
\ which is not valid in a C name, at c characters into the line
: FindCstring ( a n c - f )
   LineStartPtr +  2dup 2>r  over compare 0=
   2r> + c@
   Cvalid= not
   and
;

: FirstCase ( -- f )         \ "case" followed by non-valid C name character
   s" case" #LeadSpaces FindCstring
;

: FirstDefault ( -- f )      \ "default" followed by non-valid C name character
   s" default" #LeadSpaces FindCstring
;

: FirstClassSpecifier ( -- f )      \ "xxxxxx" followed by non-valid C name character
   s" public"    #LeadSpaces FindCstring
   s" private"   #LeadSpaces FindCstring or
   s" protected" #LeadSpaces FindCstring or
;

variable LinePosition
variable CharToFind
variable NumberToFind   \ find the 1st, 2nd, 3rd occurance etc

: SearchLine ( c-addr n -- f )   \ returns true if the given string is found anywhere in the current line
   LineLength @ 0= if  2drop  0  exit  then   \ you cannot find the string in an empty line!
   LineStartPtr LineLength @ 2swap search >r 2drop r>
;

: ttsl ( -- )
   s" 12345lint" LineStartPtr swap dup LineLength ! move
   LineStartPtr LineLength @ cr dump
   LineStartPtr LineLength @ s" lint" search ."    flag = " .
   drop
   LineStartPtr - cr .
;

\ Check lint errors on the current line
: LintCheck ( -- )
   s" lint" SearchLine if  \ this looks like a lint line
      s" //lint" SearchLine      \ C++ comment
      s" /*lint" SearchLine or   \ C comment
      not if   \ there must be no spaces before the 'lint' command  (e.g. "// lint"
         LineStartPtr LineLength @ s" lint" search drop  drop  LineStartPtr -    \ position of 'lint' from the start of the line
         #LeadSpaces 5 + < if  \ the 'lint' text is less than 5 characters from the first '//' string
            \ if the 'lint' text is more than 4 characters from the start of the '//', we count it as purely a text comment, i.e. we count up to two spaces as an error
            s" -" SearchLine if  \ if there is no '-' , it cannot be a meaningful lint command line
               1 #LintSpaceErrors +!  1 accumulate +!  LineNumber @ #LintLastLine !
            then
         then
      else
         s" -save" SearchLine if
            1 #LintSaves +!
            1 #LintStackDepthMin +!

            s" -esym" SearchLine if  \ there is a "-esym" on the same line as a "-save" - esyms are not saved/restored!
               1 #LintEsymErrors +!  1 accumulate +!  LineNumber @ #LintLastLine !
            then
         then

         s" -restore" SearchLine
         if
            1 #LintRestores +!
            -1 #LintStackDepthMin +!

            \ detect a minus sign after the -restore
            LineStartPtr LineLength @ s" -restore" search drop
            8 /string  \ step past the '-restore'
            dup if
\            cr ." >>>"  2dup type  key drop
               s" -" search >r 2drop r>
               if
                  1 #LintRestoreErrors +!  1 accumulate +!  LineNumber @ #LintLastLine !
               then
            else
              2drop
            then
         then

         #LintStackDepthMin @ 0< if    \ wrongly ordered!!
            1 #LintStackDepthMinError +!
            1 accumulate +!
            LineNumber @ #LintLastLine !
         then

      then
   then
;

\ return the position in the line of the nth character c
: FindChar() ( c -- )   CharToFind @ = if
      NumberToFind @ 0= if
         LinePosition @ 1+ CharResult !   \ add one : 0 means not found
      then
      -1 NumberToFind +!
   then
   1 LinePosition +!
;

: FindChar ( c n -- n )   \ the result, -1 = not found, else position in line
   NumberToFind !
   CharToFind !
   0 LinePosition !     \ a counter
   ['] FindChar() DoLine  CharResult @ 1- \ subtract 1 : see above
;

(( \ some test words :

: .found ( n)  dup -1 = if  drop  ."   --"  else  4 u.r  then ;

: TTF1 ( -- )
   LineStartPtr 0= if  init_buffers  then

   cr   5 0 do  10 0 do  i [char] 0 + emit  loop  loop  \ counting aid

   0 LineNumber !
   s"    ( *t_struct fred, *t_st jim(*), ***(*));;;;; "
   2dup  cr type

   dup LineLength !
   LineStartPtr swap move
   cr
   cr  ."   ( ="  5 0 do  [char] ( i FindChar  .found  loop
   cr  ."   * ="  5 0 do  [char] * i FindChar  .found  loop
   cr  ."   ) ="  5 0 do  [char] ) i FindChar  .found  loop
   cr  ."   ; ="  5 0 do  [char] ; i FindChar  .found  loop
   cr  ."   t ="  5 0 do  [char] t i FindChar  .found  loop
;

: TTF2 ( -- )
   0 LineNumber !
   s"    case "  LineStartPtr swap move
   Firstcase cr  ." case " if ."                   ok "  else  ." failed!"  then
   0 LineNumber !
   s"  default:"  LineStartPtr swap move
   FirstDefault cr  ." default " if ."                ok "  else  ." failed!"  then
      0 LineNumber !
   s"  defaults"  LineStartPtr swap move
   FirstDefault cr  0= ." defaults not matched : " if ." ok "  else  ." failed!"  then
;
))

\ ***************************  Indent test  ************************************

variable AllowGreaterThan       \ true if indent can be greater than nominal
variable my_indent_level        \ ( where the text should start ) / 4

: ?IndentOK ( -- f )      \ returns true if the indent is correct

   AllowGreaterThan off

   #LeadSpaces  LineStartPtr + c@
   case
      '#' of
            LineIndent#XXX @       \ HPO 2014 May 21
            LineStartPtr LineLength @ s" __"  SEARCH >r 2drop r> not if    \ lines with double underscores are not indented, so do not un-indent these lines :
               LineStartPtr LineLength @ s" #else"  SEARCH >r 2drop r> if  INDENT_SPACES_XXX - 0 max  then
               LineStartPtr LineLength @ s" #endif" SEARCH >r 2drop r> if  INDENT_SPACES_XXX - 0 max  then
            then
            \ s" #define" LineStartPtr over compare 0= if  drop 0  then
      endof  \ #defines must start according to #if...#endif indentation
      '{' of  GetCurrentIndent                           endof  \ must start at current indent
      '}' of  GetCurrentIndent INDENT_SPACES - 0 max     endof  \ must be indented one tab left
      '*' of
         GetCurrentIndent
         WithinComments if
            COMMENT_BLOCK_INDENT +
         then

      endof  \ indent /* ... * ... */  comment blocks by 1 space

      FirstComment if
         GetCurrentIndent
      else
         FirstCase FirstDefault or 
         drop 0   \ do not indent "case" and "default" one level left
         if   \ if "case" and "default" must be indented one indent left
            GetCurrentIndent
            INDENT_CASE_RIGHT if
               INDENT_SPACES -
            then
         else
            \ normal text may be on or after the indent position
            GetCurrentIndent
            \ AllowGreaterThan on   \ must be exactly aligned if this is commented out
         then
      then

      FirstClassSpecifier if  drop  2  then   \ public: , private: etc start in column 2

      swap      \ allow endcase to drop the char, not the indent

   endcase

   LineIndent() @ if
      AllowGreaterThan off  \ must be an exact position for parameter indents
   then

   LineLength @ 0= if  drop 0  then    \ empty lines must start at 0

   my_indent_level !    \ save this value for .Indent  and  OutputIndent

   AllowGreaterThan @ if
      #LeadSpaces  my_indent_level @  >=
   else
      #LeadSpaces  my_indent_level @  =
   then

\  cr ." DEPTH = "  DEPTH . \ ToDo: fix the stack overflow problem!!!
;

\ ***************************  Display  ****************************************

: 3.x ( c -- )   base @ >r  hex  3 u.r  r> base ! ;

: ttemit  32 emit+ 13 emit+ 09 emit+ 65 emit+ 66 emit+ ;

: ttemits    256 0 do
      i 10 mod 0= if  cr  i 5 u.r ."  | "  then  i emit  loop ;

: .CommentMark          \ type " | "  or similar, according to comment state
   FullLineInfo @ if
      space
      LineStatus @ 0x00000300 and  0x00000100 / case
         1 of  [char] 1 emit  endof
         2 of  [char] 2 emit  endof
         3 of  [char] X emit  endof  \ illegal!
         [char] | emit
      endcase
      space
      LineStatus @ 0x0000FFFF and 4 .hex space
   else
      space
      LineStatus @ $0C00 and case
         0x00000000 of  [char] | emit  endof  \ not start or end of comment block
         0x00000400 of  [char] / emit  endof  \ start of comment block
         0x00000800 of  [char] - emit  endof  \ less than 2 blank lines before this start of the comment block
         0x00000C00 of  [char] + emit  endof  \ start of comment block, with less than 2 blank lines before it
      endcase
      space
   then
   LineNumber @ >LinePrecedingBlanks @ 1 u.r space
;

variable LinesShown

: .flag ( f -- )   if  ." +"  else  ." -"  then ;

: .1digit ( f -- )  dup 0< if  ." -"  else  1 u.r  then ;

: .Line ( -- )
   cr  LineNumber @ 1+ 3 u.r
   FullLineInfo @ if
      ."  {"  LineIndent{} @ .1digit ." }"
      ."  ("  LineIndent() @ .1digit ." )"
      ."  #"  LineIndent#XXX @ .1digit ." #"
      ."  /*" FirstComment .flag
      ."  c" FirstCase     .flag
      ."  d" FirstDefault  .flag
      ."  s" FirstClassSpecifier .flag
      ."  I" ?IndentOK     .flag
     ."  GC"  GetCurrentIndent 2 .r
\     ."  GN"  GetNextIndent 2 .r
     ."  NS"  LineIndentNS @ 2 .r
\     ."   St" LineStatus   @ 3.x
   then
   .CommentMark
   ['] emit+ DoLine
   LineStatus @ 1 and if  13 emit+  then        \ show the cr
\   LineStatus @ 2 and if  10 emit+  then       \ show the linefeed ?
;

: DisplayHeight ( -- n )   ScreenHeight 2 - ;

: .Lines+ ( -- )   \ show a page of lines with comments
   0 LinesShown !

   StartLine @ LineNumber !

   begin
      LineNumber @ NumInputLines @ <
      LinesShown @ DisplayHeight < and
   while
      .Line
      1 LinesShown +!
      1 LineNumber +!
   repeat
;

variable FindLine  \ find the nth displayed line
variable NthLine

: FindTotalLinesShown ( -- )   \ set the TotalLinesShown

   0 TotalLinesShown !
   0 LinesShown !
   0 LineNumber !

   begin
      LineNumber @ NumInputLines @ <
   while
      LineLength @  if
         1 LinesShown +!
         LinesShown @ TotalLinesShown !
      then
      1 LineNumber +!
   repeat
;

: FindNthLine ( n -- )   \ set NthLine to the LineNumber of the nth displayed line

   ( n) FindLine !
   0 LinesShown !
   0 LineNumber !
   0 NthLine !

   begin
      LineNumber @ NumInputLines @ <
      LinesShown @ FindLine @ < and
   while
      LineLength @  if
         1 LinesShown +!
      then

      1 LineNumber +!
      LineNumber @ NthLine !

   repeat
;

((
: .lins
   cr ." Line# = "  LineNumber @ .  ."   StartLine = "  StartLine @ .
      ."   TotalLinesShown = " TotalLinesShown @ .
   key drop
;
))

: .Lines-       \ show a page of lines without comments

   TotalLinesShown @ -1 = if                 \ not yet found
      ['] FindTotalLinesShown catch drop     \ so find it now
   then

   StartLine @ ['] FindNthLine catch drop  NthLine @ LineNumber !

   0 LinesShown !

   begin
      LineNumber @ NumInputLines @ <
      LinesShown @ DisplayHeight < and
   while
      LineLength @  if
         .Line
         1 LinesShown +!
      then
      1 LineNumber +!
   repeat
;

: StartLine! ( n -- )   0 max

   filtering 0x01 and @ if
      TotalLinesShown @
   else
      NumInputLines @
   then
   DisplayHeight - 0 max  min

   StartLine !     \ select the start line within the relevant limits

   2 DispMode !    \ show the file
;

: ttl2 ( -- )
   18 0 do
      i ['] FindNthLine catch drop
      LineNumber @ .
   loop
;

: .Line3.x ( -- )
   LineNumber @ 1+ StartLine @  dup ScreenHeight 2 - + within if
      cr  LineNumber @ 1+ 3 u.r
      .CommentMark
      ['] 3.x DoLine
   then
;

: ShowFilename ( -- )
    space CurrentFilename  count type
;

: ShowFilename+ ( -- )
    ShowFilename
    ."   " InputFileLen @ 7 u.r
    ."  bytes  "  NumInputLines @ 6 u.r  ."  lines"
;

: ShowFile ( -- )   \ display the file

   page  ." Line| "  ShowFilename

   filtering @ if
      ."  : no comments"
\      SaveLineArray
\      InputBuf->LineArray           \ convert to line by line format
      ['] .Lines- catch if cr ." .Lines- caught..."  then
\      restoreLineArray
\      filtering off
\      InputBuf->LineArray    \ do not leave the comment less file around
\      filtering on

   else
      ."  : with comments"
      .Lines+
   then
;

variable FoundIt

: MarkDel ( c -- )  \ is this a Del character?
   127 = if  -1 FoundIt !  then
;

: ShowDels() ( -- )
   0 FoundIt !
   ['] MarkDel DoLine
   FoundIt @ if
      .CommentMark
      LineNumber @ 1+ 3 u.r space
   then
;

: ShowDels ( -- )  \ display lines containing Del characters
   InitVariables
   ."    at line # "
   ['] ShowDels() DoLines
;

: MarkBit7 ( c -- )  \ does this character have bit7 set?
   128 and if  -1 FoundIt !  then
;

: ShowBit7s() ( -- )
   0 FoundIt !
   ['] MarkBit7 DoLine
   FoundIt @ if
      .CommentMark
      LineNumber @ 1+ 3 u.r space
   then
;

: ShowBit7s ( -- )   \ display lines containing Del characters
   InitVariables
   ."    at line # "
   ['] ShowBit7s() DoLines
;

\ ******************************* The Tests ************************

: T-Line>MAX_LINE_LENGTH ( -- )
\   LineLength @  ( dup ." LOL = "  . )
   LineLength @ MAX_LINE_LENGTH >
   if  1 Result +!  1 accumulate +!  ."  | "  LineNumber @ 1+ 5 u.r  then
   LineLength @ maxline @  max  maxline !
;

: T-LinesWithOnlySpaces ( -- )
   LineLength @ 0= if  exit  then
   #nonprintable  \ number of non-printable characters
\   FileFormat @ if  #CRs - 0 max  then \ less these - ignore CRs in a PC file
   0= not
   BlankLine  and
   #pagebreaks 0= and \ this is ok on a line
   if  1 Result +!  1 accumulate +!  ."  | "  LineNumber @ 1+ 5 u.r  then
;

\ : next_line_is_start_of_comment_block ( -- f )
\    LineNumber @ 1+ >LineStatus @ 0x00000400 and       \ start of a '//' or '/*...*/' comment block
\ ;

: next_line_is_start_of_//_comment_block ( -- f )
   LineNumber @ 1+ >LineStatus @ 0x00000400 and       \ start of a '//'  comment block
   \ LineNumber @ 1+ >LineStatus @ 0x00000200 and and   \ and a C++ '//' style comment
;

: T-TwoBlankLines ( -- )
   BlankLine dup
   LastLineWasBlank @ swap LastLineWasBlank !
   and ( both this line and last were blank )
\   eof @ 0= and         \ not the end of the file
   next_line_is_start_of_//_comment_block not  and    \ and the next line is not the start of a comment block
   if  1 Result +!  1 accumulate +!  ."  | "  LineNumber @ 1+ 5 u.r  then
;

: count_TwoBlankLines_errors() ( -- )
   BlankLine dup
   LastLineWasBlank @ swap LastLineWasBlank !
   and ( both this line and last were blank )
   next_line_is_start_of_//_comment_block not  and    \ and the next line is not the start of a comment block
   if  1 Result +!  then   
;

: count_TwoBlankLines_errors ( -- n )
   0 LastLineWasBlank !
   ['] count_TwoBlankLines_errors() DoLines
   Result @    
;

\ See EndOfLineActions 0x00000800 to set a bit if there is only 0 or 1 blank lines before a comment block
\ A comment block is 3 or more consecutive comment lines 
: T-MissingBlankLine ( -- )
   LineStatus @ 0x00000800 and
   if  1 Result +!  1 accumulate +!  ."  | "  LineNumber @ 1+ 5 u.r  then
;

: count_MissingBlankLine_errors() ( -- )
   LineStatus @ 0x00000800 and
   if  1 Result +!  then   
;

: count_MissingBlankLine_errors ( -- n )
   ['] count_MissingBlankLine_errors() DoLines
   Result @    
;


: printable? ( c -- f ) \ true if printable, non-special character
   >r
   r@ 32 >      \ any printable character, except :
   r@ [char] * = not and
   r@ [char] ( = not and
   r@ [char] ) = not and
   r@ [char] { = not and
   r@ [char] } = not and
   r@ [char] [ = not and
   r@ [char] ] = not and
   r> drop
;

variable myResult2
variable myResult3
variable foundspace
variable foundstar
variable foundprintable
variable myResult

: .rest ( -- )
   cr ." Rest of line : "
   LineLength @ PositionInLine @ ?do
      LineStartPtr i + c@ emit+
   loop  ."  :  flag = "
;

: FindStructStart ( -- f )   \ look for "t_" or "e_" at the start of a struct
   0                            \ flag
   LineLength @ PositionInLine @ ?do
      LineStartPtr i +  c@ dup [char] t =  swap [char] e = or
      LineStartPtr i + 1+ c@ [char] _ = and if
         i 2 + LineLength @ min
            PositionInLine !  \ new start of search position  ignore _
         1-                     \ mark flag as true
         leave
      then
   loop

   dup 0= if    \ not found
      LineLength @ PositionInLine !    \ end the search
   then
;

: FP1 FindStructStart  .rest .  ;

: FindStructEnd ( -- f )   \ look for the end of the structure's name
   0                            \ flag
   LineLength @ PositionInLine @ ?do
      LineStartPtr i +  c@
      Cvalid= not
      if
         i PositionInLine !  \ new start of search position ( include this character )
         1-                  \ mark flag as true
         leave
      then
   loop
;

: FP2 FindStructEnd .rest . ;

: FindStruct ( -- f )   \ look for a complete structure's name
   FindStructStart if
      FindStructEnd
   else
      0
   then
;

: FP3 FindStruct  .rest . ;

variable my_next_char

: FindNonStarOrSpace ( -- f )   \ look for anything other than * or space

   0 foundstar !  0 foundspace !         \ clear the flags
   0 myresult ! \ clear the result flag

   LineLength @ PositionInLine @ ?do

      LineStartPtr i + 1+  c@ my_next_char !

      LineStartPtr i +  c@ >r
      r@ 32 = if
         -1 foundspace !
         foundstar @ if \ with a previous star
            2 myResult !   \ flag an error : * must not be followed by a space
         then
      then
      r@ [char] * = if  \ found a star
         -1 foundstar !
         foundspace @ 0= if \ without a previous space...
            my_next_char @ [char] / = not if  \ not a */ end of comment
               1 myResult !   \ flag an error : * must be preceeded by a space
            then
         then
      then
      r@ [char] * =
      r> 32 = or
      not               \ leave if not a * or space
      if
         i 1+ PositionInLine !  \ new start of search position
         leave
      then
   loop
   myresult @
;

: FP4 ( -- )  FindNonStarOrSpace  .rest . ;

: T-FloatingPointers ( -- )
\   cr  ." T-FP : Line " LineNumber @ 5 u.r   ."  | " LineStartPtr LineLength @ type

   LineLength @ 0= if  exit  then

   0 PositionInLine !   \ start parsing at the start of the line

   begin

      FindStruct if                     \ t_fred  or  e_fred
         FindNonStarOrSpace if       \ check order or space and *
            1 Result +!
            \ 1 accumulate +!
            ."  | "  LineNumber @ 1+ 5 u.r
         then
      then

   PositionInLine @ LineLength @ >= until
;

: FP5  0 Result !  T-FloatingPointers  Result @ ."  Result = " . ;

: T-DoubleSlashes ( -- )
   LineLength @ 1- 0 max 0 ?do
      LineStartPtr i + c@      [char] / =
      LineStartPtr i + 1+  c@  [char] / = and
      if
         1 Result +!
\         ."  | "  LineNumber @ 1+ 5 u.r
      then
   loop
;

variable endCR  \ the line ends with a CR

((
\ High-level version
variable -trail#

: -trailing ( a n - a n' )
   -trail# !
   dup

   dup -trail# @ + 1-     \ address of last character
   -trail# @ 0 ?do
      dup i - c@ 0x20 = not if  leave  else  -1 -trail# +!  then
   loop
   drop
   -trail# @
;

: tt-t  s" hello        "  -trailing 2dup  swap  . .  -trailing 2dup swap . . ;
))

: T-TrailingSpaces ( -- )

   LineStartPtr LineLength @
   2dup 1- 0 max + c@ 13 = if  -1 endCR !
       1- 0 max
   else
      0 endCR !
   then

   2dup
   -trailing

   swap  drop  rot  drop  swap  \ was is
\   cr ." T-SP = "  2dup . .  key drop
   = not
   if   1 Result +!  ."  | "  LineNumber @ 1+ 5 u.r  then
;

: LastCharC@ ( -- c )   InputFilePtr @ InputFileLen @ 1- 0 max + c@ ;

\ ***********************  File Dialogue box  *********************************

FileOpenDialog FileSelectDownloadWindow "Select File" "All Files (*.*)|*.*|Text (*.c*,*.h,*.pl,*.f,*.fth,*.bld,*.txt)|*.c*;*.h;*.pl;*.f;*.fth;*.txt;*.bld|C source files (*.c*)|*.c|Header files (*.h)|*.h|perl files (*.pl)|*.pl|"

: SelectFile ( -- )

   0 Start: FileSelectDownloadWindow dup c@
   if
      count CurrentFilename place   \ save the name

      InputFilePtr @ if
         InputFilePtr @ FILE_BUFFER_SIZE erase    \ clear the file buffer
      then

      GetFile                       \ scan file for numbers of characters etc
      SaveOriginal                  \ save the original file, if it was loaded
      DispMode @ 0= if
         1 DispMode !   \ select the default mode, if none other has been chosen
      then
   else
      drop
   then

   DispMode @ >r
   StartLine @ StartLine!         \ start the display within the file's limits
   r> DispMode !
;

: ?SelectFile ( -- )
     InputFileLen @ 0= if  SelectFile  then
;

variable File#tabs
variable File#Cweed4Errors
variable FileLintErrors          \ number of lint errors in the current file

variable Folder#tabs
variable Folder#Cweed4Errors
variable Folder_CapitalisationErrors
variable Folder_TwoBlankLines_errors                             
variable Folder_MissingBlankLine_errors

variable FolderLintErrors        \ number of files with lint errors
variable FolderLintErrorsTotal   \ total of all lint errors

: ShowLintErrors ( -- )
   #LintSaves @ 3 u.r ." /" #LintRestores @ (.) dup >r  type  3 r> - 0 max  spaces

   #LintSaves @  #LintRestores @ - 0<> if  ." ! Unbalanced!!!"  1 accumulate +!   1 FileLintErrors +!  then

   #LintStackDepthMinError @
   #LintRestoreErrors @ or
   #LintSpaceErrors @ or
   #LintEsymErrors @ or
   if
      1 FileLintErrors +!
      ." ! Line " #LintLastLine @ 1+ .
      #LintStackDepthMinError @ if  ." ! Order! "  then
      #LintRestoreErrors @ if  ." !  '-' after -restore "  then
      #LintSpaceErrors @ if  ." ! Space before 'lint' "  then
      #LintEsymErrors @ if  ." ! '-esym' with '-save'"  then
   then
;

\ *********************** File Fixing *****************************

variable col#   \ number of characters outputted since the last linefeed

variable spaces/tab

: SetTabSize ( c -- )   0 max  8 min  spaces/tab ! ;

: ReplaceTabs ( -- )
   InitVariables
   0 col# !
   0 OutputFileLen !    \ start to construct the output file at the beginning
   InputFileLen @ 0 ?do
      f-key() dup
      case
         10 of   0 col# !  endof
         09 of   drop  32               \ convert tab to at least one space
             spaces/tab @ col# @ 1- 3 and -      \ calculate how many more
             1- 0 max 0 ?do  32 femit  1 col# +!  loop
            endof
         endcase
      femit  1 col# +!
   loop
   OutputBuf->InputBuf
   InputBuf->LineArray
;

$100 Buffer: TempPad[]

: ReplaceTabsPlus ( -- )
   s" ReplaceTabs"

   s" Replace all tabs by " TempPad[] place
   spaces/tab @ (.) TempPad[] append
   s"  spaces? " TempPad[] append

   TempPad[] count Dialogue
      case
         1 of ( OK )    endof
         2 of ( Cancel )
            2000 200  tone
            s" User pressed Escape"   s" Cancelled." Monologue
            123400010 throw  endof
      endcase
   ReplaceTabs
;

: PC->Unix ( -- )      \ remove the cr character flag from all lines
   NumInputLines @ 0 ?do
      i LineNumber !
      0x01 LineStatus -or
   loop

   LineArray->Output
   OutputBuf->InputBuf
;

: Unix->PC ( -- )      \ add the cr character flag to all lines
   NumInputLines @ 0 ?do
      i LineNumber !
      0x01 LineStatus +or
   loop
   LineArray->Output
   OutputBuf->InputBuf
;

: RemoveTrailingSpaces() ( -- )
   LineStartPtr LineLength @
   -trailing
   swap drop
   LineLength !
;

: RemoveTrailingSpaces ( -- )  \ change the line by line array
   InitVariables
   ['] RemoveTrailingSpaces() DoLines
   LineArray->Output
   OutputBuf->InputBuf
;

\ *****************************************************************************
\ String support
\ *****************************************************************************

CODE THIRD ( n1 n2 -- n1 n2 n1 ) \ copy second item to top of data stack
   push    ebx
   mov     ebx, 8 [esp]
   next    c;

CODE FOURTH ( n1 n2 -- n1 n2 n1 ) \ copy second item to top of data stack
   push    ebx
   mov     ebx, 1 [esp]
   next    c;

CREATE STRBUF   512 ALLOT

: SUBST ( source len old len new len -- result len )
   LOCALS| nl n pl p |   0 STRBUF C!
   BEGIN   DUP 0> WHILE
      2DUP p pl SEARCH WHILE ( s l m l)
         ROT OVER - >R ROT R> STRBUF APPEND
         n nl STRBUF APPEND
         pl /STRING
   REPEAT 2SWAP STRBUF APPEND THEN
   2DROP STRBUF COUNT ;

: REPLACE ( new l source l old l -- result l )
   DUP>R  2OVER 2>R
   SEARCH IF    ( new l a n)
      DUP 2R> ROT - STRBUF PLACE
      R> /STRING  2SWAP STRBUF APPEND  STRBUF APPEND
   ELSE         ( new l a n)
      2R> STRBUF PLACE  R>DROP 2DROP 2DROP
   THEN  STRBUF COUNT ;

\ Win32Forth  : Replace ( source$ old$ new$ -- result$ )   \ in source$ replace old$ by new$, remainder in result$
\ SwiftForth  : REPLACE ( new$ source$ old$ -- result$ )
: tt_Replace ( -- )
   cr ." Replace just one 'replaceme' by 'totally wonderful'"
   s" Forth is replaceme , hello replaceme world." 
   MyBuffer1 place  MyBuffer1 count    \ copy the compiled string to a bigger buffer
   cr ."    was     : "  2dup type.
   s" replaceme"  
   MyBuffer2 place  MyBuffer2 count    \ copy the compiled string to a bigger buffer
   s" totally wonderful"  
   MyBuffer3 place  MyBuffer3 count    \ copy the compiled string to a bigger buffer
   2swap 2>r 2swap 2r> Replace  
   cr ."    is      : " type.
   cr
;

: REPLACE-CHAR ( source n oldch newch -- )
   2SWAP BEGIN ( old new a n)
      DUP WHILE
      FOURTH SCAN
      DUP IF 3DUP DROP C! THEN
   REPEAT 2DROP 2DROP ;

: SPLITSTR ( source n pattern n -- remainder n beginning n )
   2>R OVER SWAP BEGIN
      DUP 0> WHILE
      OVER 2R@ TUCK  COMPARE WHILE  \ case insensitive compare
      1 /STRING
   REPEAT THEN
   ROT THIRD OVER - 2R> 2DROP ;

\ returns true if string $ contains string $tag
: contains ( $ $tag -- f )   search >r 2drop r> ;

\ ****************************************************************************
\ MarkStartOf//commentBlock
\ A //comment block is 3 or more lines starting with at least 2 '/' characters :
\ 
\ ////////////////////////
\ // A comment block
\ ////////////////////////
\ 
\ ****************************************************************************

: MarkStartOf//commentBlock() ( -- )
   LineStartPtr LineLength @ s" //" StartsWith if
      \ cr ." >>>>>>>>>>>>"
      0x00000400  LineNumber @ >LineStatus  +or

      v_number_of_//_comments @ 0= if
         LineNumber @ v_start_of_comments_line !    \ save the line number where the //comment block started         
      then
      1 v_number_of_//_comments +!                      \ start counting
      v_number_of_//_comments @ 3 = if
         \ we have counted 3 comment lines in a row, so mark the start of the comment block
          0x400  v_start_of_comments_line @ >LineStatus  +or  
      then
   else
      0 v_number_of_//_comments !                       \ reset the count 
   then
;

: MarkStartOf//commentBlock ( -- )   \ change the line by line array
   0 start_of_//_comment !
   0 v_start_of_comments_line !
   0 v_end_of_comments_line !

   0 v_number_of_//_comments !
   0 v_last_number_of_//_comments !

   InitVariables
   ['] MarkStartOf//commentBlock() DoLines
   LineArray->Output
   OutputBuf->InputBuf
;

\ *********************** File Information *****************************

: ShowFileInfo ( -- )
   \ ."  : information"
   ." Sum = "  Checksum @ 8 .hex
   ."   CRC32 = " CRCchecksum @ 8 .hex
   NoMD5 @ 0= if
      ."   MD5 = "  .MD5
   then
   2 spaces MD5hash @ .NumNames
   FileFormat @ 0 = if  ."   Mixed format"  then
   FileFormat @ 1 = if  ."   PC format  "   then
   FileFormat @ 2 = if  ."   Unix format"   then
   FileFormat @ 3 = if  ."   Empty file"    then
;

: ShowFileInfoShort ( -- )
   \ ."  : information"
   2 spaces  Checksum @ 8 .hex
   2 spaces CRCchecksum @ 8 .hex
   NoMD5 @ 0= if
      2 spaces .MD5
   then
   2 spaces MD5hash @ .NumNames
   FileFormat @ 0 = if  ."   Mixed"  then
   FileFormat @ 1 = if  ."   PC  "   then
   FileFormat @ 2 = if  ."   Unix"   then
   FileFormat @ 3 = if  ."   Empty"  then
;

: ShowInfo ( -- )

   \ ***** File based tests *****

   page

   0 accumulate !      \ reset the running totals of significant errors
   0 accumulateIndentErrors !

   cr ShowFilename+ cr
   cr ShowFileInfo

   InputFileLen @ 0= if  exit  then

   InputFileLen @ FILE_BUFFER_SIZE = if  cr ."  Max size limit! "  then

   LastCharC@ 10 = not if  ."     File does not end with LF!"  then

   cr  ." # Tabs        ( 09 ) : "  09 numbers@ dup File#tabs ! 5 u.r
   cr  ." # Linefeeds   ( 10 ) : "  10 numbers@ 5 u.r
   cr  ." # PageBreaks  ( 12 ) : "  12 numbers@ 5 u.r
   cr  ." # <cr>s       ( 13 ) : "  13 numbers@ 5 u.r
   cr  ." # Others < 32        : "
   0  32 0 do  i numbers@ +  loop  dup ( * )

   FileFormat @ 1 = if  13 numbers@  -  then \ allow crs in a PC format file
   10 numbers@ -    \ allow LFs
   12 numbers@ -    \ allow page breaks
   FileFormat @ 3 = if  \ Empty file
      drop              \ so do not accumulate errors...
   else
      accumulate +!
   then

   ( * ) 09 numbers@ -  10 numbers@ - 12 numbers@ - 13 numbers@ -  drop \ File#tabs @ 5 u.r

   cr  ." # Dels       ( 127 ) : "
   127 numbers@ dup dup 5 u.r  accumulate +!
   if  \ dels present
      ShowDels
   then

   cr  ." # Bit 7 set          : "
   0  256 128 do  i numbers@ +  loop  dup dup 5 u.r  accumulate +!
   if  \ bit 7 set characters present
      ShowBit7s
   then

   \ ***** Line based tests *****

   cr  ." Double slashes  //   : "
   ['] T-DoubleSlashes DoLines
   Result @ if  ( ."  | Total = " )  Result @ 5 u.r  else  ."  none"  then

   cr  ." Lines > " MAX_LINE_LENGTH 3 u.r ."  chars    : "
   ['] T-Line>MAX_LINE_LENGTH DoLines
   Result @ if  ."  | Total = " Result @ 4 u.r  else  ."  none"  then
   Result @ if  ."   max = "  maxline @ 4 u.r  then

   \    Result @ if
   \    1 accumulate +!
   \    cr  ."                Total :  " Result @ 4 u.r
   \    ."   max = "  maxline @ 4 u.r
   \    else  ."  none"  then

   cr  ." Lines w/ only spaces : "
   ['] T-LinesWithOnlySpaces DoLines
   Result @ if  ."  | Total = " Result @ 4 u.r  else  ."  none"  then

   cr  ." Trailing spaces      : "
   ['] T-TrailingSpaces DoLines
   Result @ if  ."  | Total = " Result @ 4 u.r  else  ."  none"  then

   cr  ." Two Blank Lines      : "
   ['] T-TwoBlankLines DoLines
   Result @ if  ."  | Total = " Result @ 4 u.r  else  ."  none"  then

   cr  ." Missing Blank Line   : "
   ['] T-MissingBlankLine DoLines
   Result @ if  ."  | Total = " Result @ 4 u.r  else  ."  none"  then

   cr  ." Floating Pointers    : "
   ['] T-FloatingPointers DoLines
   Result @ if  ."  | Total = " Result @ 4 u.r  else  ."  none"  then

   NumInputLines @ SPEC_MAX_NUM_LINES > if
   cr  ." Number of lines = "  NumInputLines @ .  ."  : " SPEC_MAX_NUM_LINES .
    ." or more ! "
   1 accumulate +!
   then

   \ MarkStartOf//commentBlock   

   accumulate @ File#Cweed4Errors !

   InitVariablesLint
   ['] LintCheck DoLines
   cr ." Lint save/restores   : " ShowLintErrors
   .StackDepths
   cr

   accumulate @ 0= if
      ." ----------------   Pass   -----------------"
   else
      ." ****************  "  accumulate @ .  ." Error"
      accumulate @ 1 = not if  ." s"  then
      ."   ****************"
   then

   DoNotSave @ if
      cr  ." RAM copy of the file is not valid - cannot save it. "
      DoNotSave @ 0x01 and if  ." File not found. "  then
      DoNotSave @ 0x02 and if  ." Input file too big. "  then
      DoNotSave @ 0x04 and if  ." Line too long. Try pressing ctrlR "  then
      DoNotSave @ 0x08 and if  ." Output file too big. "  then
      DoNotSave @ 0x10 and if  ." Too many lines in file. "  then
   then
;

\ ****************************************************************************
\ Path support
\ ****************************************************************************

: pathENDING ( addr len char -- addr len )
   >R BEGIN  2DUP R@ SCAN
      ?DUP WHILE  2SWAP 2DROP  1 /STRING
   REPEAT  R> 2DROP ;

\ -NAME strips the name and trailing \ from the end of a full pathname.
: -NAME ( a n -- a n )
   2DUP  [CHAR] \ pathENDING  NIP -  1-  0 MAX ;

\ -EXT strips the trailing extension from the filename.
: -EXT ( a n -- a n )
   2DUP  [CHAR] . pathENDING  NIP -  1-  0 MAX ;

\ -PATH strips the path from the beginning of a full pathname.
: -PATH ( a n -- a n )
   [CHAR] \ pathENDING  0 MAX ;

\ ****************************************************************************
\ Support words
\ ****************************************************************************

: @EXECUTE ( addr -- )
   ?dup if
      execute
   then
;

: FILE-EXISTS ( caddr u -- flag )
   FILE-STATUS NIP 0= ;

variable v_numberOfLeadingSpaces
variable v_numberOfTrailingSpaces

\ return the number of leading spaces in the given string
: NumberOfLeadingSpaces ( $ -- c )    
   0 -rot
   over + swap ?do
      i c@ $20 > if  leave  then
      1+
   loop  
;

\ return the number of trailing spaces in the given string
: NumberOfTrailingSpaces ( $ -- c )    
   2dup -Trailing  swap drop  rot drop -
;

$400 constant |TempLineBuffer|  \ must be a power of 2
|TempLineBuffer| Buffer: TempLineBuffer0[]
|TempLineBuffer| Buffer: TempLineBuffer1[]
|TempLineBuffer| Buffer: TempLineBuffer2[]
|TempLineBuffer| Buffer: TempLineBuffer3[]

variable v_TempLineBufferPtr0
variable v_TempLineBufferPtr1
variable v_TempLineBufferPtr2
variable v_TempLineBufferPtr3

: ToTempLineBuffer2 ( c -- a )
    |TempLineBuffer| 1- and  TempLineBuffer2[] +
;

: TempLineBuffer2_put8 ( c -- )
    v_TempLineBufferPtr2 @  ToTempLineBuffer2 c!  
    1 v_TempLineBufferPtr2 +!
;

: init_TempLineBuffer2 ( -- )
    0 v_TempLineBufferPtr2 !
;

: get_TempBuffer0 ( -- $ )
   TempLineBuffer0[] v_TempLineBufferPtr0 @ |TempLineBuffer| 1- and
;

: get_TempBuffer1 ( -- $ )
   TempLineBuffer1[] v_TempLineBufferPtr1 @ |TempLineBuffer| 1- and
;

: get_TempBuffer2 ( -- $ )
   TempLineBuffer2[] v_TempLineBufferPtr2 @ |TempLineBuffer| 1- and
;

: get_TempBuffer3 ( -- $ )
   TempLineBuffer3[] v_TempLineBufferPtr3 @ |TempLineBuffer| 1- and
;


: copy_toTempBuffer0 ( $ -- )
   v_TempLineBufferPtr0 ! TempLineBuffer0[] v_TempLineBufferPtr0 @ |TempLineBuffer| 1- and move
;

: copy_toTempBuffer1 ( $ -- )
   v_TempLineBufferPtr1 ! TempLineBuffer1[] v_TempLineBufferPtr1 @ |TempLineBuffer| 1- and move
;

: copy_toTempBuffer2 ( $ -- )
   v_TempLineBufferPtr2 ! TempLineBuffer2[] v_TempLineBufferPtr2 @ |TempLineBuffer| 1- and move
;

\ : xxxto_TempLineBuffer3 ( -- a )   get_TempBuffer3 + ;

: copy_toTempBuffer3 ( $ -- )
   v_TempLineBufferPtr3 ! TempLineBuffer3[] v_TempLineBufferPtr3 @ |TempLineBuffer| 1- and move
;

: putChar_TempBuffer3 ( c -- )
   TempLineBuffer3[] v_TempLineBufferPtr3 @ |TempLineBuffer| 1- and  +  c!
   1 v_TempLineBufferPtr3 +!
;

: append_TempBuffer0 ( $ -- )
   ( n -- ) >r  ( a -- ) get_TempBuffer0 +  r@ move  r@ v_TempLineBufferPtr0 +!  r> drop
;

: append_TempBuffer1 ( $ -- )
   ( n -- ) >r  ( a -- ) get_TempBuffer1 +  r@ move  r@ v_TempLineBufferPtr1 +!  r> drop
;

: append_TempBuffer2 ( $ -- )
   ( n -- ) >r  ( a -- ) get_TempBuffer2 +  r@ move  r@ v_TempLineBufferPtr2 +!  r> drop
;

: append_TempBuffer3 ( $ -- )
   ( n -- ) >r  ( a -- ) get_TempBuffer3 +  r@ move  r@ v_TempLineBufferPtr3 +!  r> drop
;

\ : REPLACE ( new$ source$ old$ -- result$ ) 
\ : BigReplace() ( source$ old$ new$ -- result$ remainder$ f )   \ in source$ replace old$ by new$, result in result$, remainder in remainder$ f is true if $old was found
: replaceBuf ( $old $new -- )
   2>r get_TempBuffer0  2swap  2r> 
\   ( new$ old$ source$ -- ) 2swap REPLACE \ must be BigReplace !!!
   ( source$ old$ new$ -- result$ )  BigReplace 
   copy_toTempBuffer0
;

: replaceBufAtStartOfLine ( $old $new -- )
   2over get_TempBuffer0 2swap startsWith not if  2drop 2drop exit  then
   replaceBuf
;

: LineEndsWith; ( -- f )
   LineStartPtr LineLength @ -Trailing + 1- c@ [char] ; =
;

: TempBufferEndsWith_; ( -- f )
   get_TempBuffer0 -Trailing + 2 - 2 s"  ;" compare 0= 
;


\ ****************************************************************************
\ Barr Coding Standards user interface
\ ****************************************************************************

variable v_Cweed4_script_line_action      \ stores the xt of the script action
0 v_Cweed4_script_line_action !

variable v_Cweed4_script_check_only       \ only list the errors, do not change anything
variable v_Cweed4_script_check_show_line_numbers       \ display the line numbers while checking

variable v_Cweed4_script_NumberOfErrors   \ number of lines with Barr errors

include Cweed4_script.f                   \ set the default (pre-compiled) action from the script 

\ There are three ways that we call ApplyScriptRules() to check the file :
\ 1. Count how many lines have errors        = 1
\ 2. List the line numbers that heave errors = 2
\ 3. Fix the line numbers that heave errors  = 3

: ApplyScriptRules() ( f -- )   \ change or view the line by line array rules
   InitVariables
   v_Cweed4_script_line_action @ DoLines
   LineArray->Output
   OutputBuf->InputBuf
;

\ Count the number of lines that have errors
: count_ApplyScriptRules_Lines ( -- n )   
   -1 v_Cweed4_script_check_only !
   0 v_Cweed4_script_check_show_line_numbers !
   0 v_Cweed4_script_NumberOfErrors !
   ApplyScriptRules()
   v_Cweed4_script_NumberOfErrors @
;

\ List the line numbers of lines that have errors
: list_ApplyScriptRules_Lines ( -- )   
   -1 v_Cweed4_script_check_only !
   -1 v_Cweed4_script_check_show_line_numbers !
   ApplyScriptRules()
;

\ Fix the lines that heave errors
: fix_ApplyScriptRules_Lines ( f -- )   
   0 v_Cweed4_script_check_only !
   ApplyScriptRules()
;


: Show_BarrReport ( f -- )   \ change the line by line array using the script file rules
   -1 v_Cweed4_script_check_only !
   -1 v_Cweed4_script_check_show_line_numbers !
   0 v_Cweed4_script_NumberOfErrors !
   page 
   cr ShowFilename+ cr
   cr ShowFileInfo
   cr 
   cr ." Checking the file using rules from Cweed4_script.f "
   count_ApplyScriptRules_Lines dup if
      dup 1 = if
         cr ." Errors found in " . ."  line, line number that fails the tests : " 
      else
         cr ." Errors found in " . ."  lines, line numbers that fail the tests : " 
      then
      cr
      list_ApplyScriptRules_Lines
   else
      drop
      cr ." Barr Coding Standards OK :-) " 
   then
;

: ApplyScriptRulesPlus ( -- )

   \ reload the script file here, if it exists
   s" Cweed4_script.f" FILE-EXISTS if
      s" Cweed4_script.f" included  \ r/o OPEN-FILE abort" OPEN-FILE error!" Include-File 
   then
  
   Show_BarrReport

   v_Cweed4_script_NumberOfErrors @ 0= if  exit  then
   
   s" Barr Coding Standards"
   s" Apply and fix Barr Coding Standards (2018) whitespace rules?"
   Dialogue
   case
      1 of ( OK )  -1  endof
      2 of ( Cancel )
         2000 200  tone
         \ s" User pressed Escape"   s" Cancelled." Monologue
         0 
      endof
      0
   endcase

   0 = if  exit then    \ do nothing if the user clicked on Cancel

   v_Cweed4_script_NumberOfErrors @ if    \ only fix it if it is broken...
      fix_ApplyScriptRules_Lines
      SaveToFile
      Show_BarrReport   \ list the errors in the newly fixed file, should be 0 ;-)
   then
;

\ ****************************************************************************
\ Capitalise first character after '// ' comment
\ ****************************************************************************

variable v_Capitalise_check_only       \ only list the errors, do not change anything
variable v_Capitalise_line_action
variable v_Capitalise_errors   
variable v_Capitalise_check_show_line_numbers

: Capitalise_line_action() ( -- )
   LineStartPtr LineLength @  s" //"         contains 0= if  exit  then    \ ignore lines that do not contain a "//"
   LineStartPtr LineLength @  s" //<"        contains    if  exit  then    \ ignore lines that contain this comment sub-string

   NO_SPACE_AFTER_// if
      \ remove the space after each "//" , if required by Software Coding Guideline (VW AIO), but not for triple "///" comments 
      LineStartPtr LineLength @  s" /// "    contains    if  exit  then    \ ignore lines that contain this comment sub-string
   then

   LineStartPtr LineLength @  s" ////"       startsWith  if  exit  then    \ ignore lines with many "////" 
   
   \ count leading and trailing spaces, save for later
   LineStartPtr LineLength @ NumberOfLeadingSpaces   v_numberOfLeadingSpaces !
   LineStartPtr LineLength @ NumberOfTrailingSpaces  v_numberOfTrailingSpaces !

   \ load our temporary buffer
   LineStartPtr LineLength @ copy_toTempBuffer0

   get_TempBuffer0 s" //" SPLITSTR 2drop 2 /string drop >r

   r@ c@ '/' = if  r> 1+ >r  then   \ skip past the 3rd '/' as in "/// Hello" 
   
   r@ c@ $20 = not if 
      s" //"  s" // " replaceBuf    \ just replace the first "//" by adding a space
   then

   \ do not UPPER case the first character if any of the conditions in the list below apply :
   0 
   r@ 1+ LineLength @ s"  " SPLITSTR 2swap 2drop s" _" contains  or  \ look for a '_' in the comment up to the next space - it must be a variable or function name... hopefully
   r@ 1+ LineLength @ s"  " SPLITSTR 2swap 2drop s" /" contains  or  \ look for a '/' in the comment up to the next space - it must be a variable or function name... hopefully
   r@ 1+ LineLength @ s"  " SPLITSTR 2swap 2drop s" &" contains  or  \ look for a '&' in the comment up to the next space - it must be a variable or function name... hopefully
   r@ 1+ 4 s" http"   compare 0= or
   r@ 1+ 3 s" www"    compare 0= or 
   r@ 1+ 6 s" printf" compare 0= or
   r@ 1+ 6 s" memset" compare 0= or
   r@ 1+ 6 s" memcpy" compare 0= or
   r@ 1+ 6 s" exit(0" compare 0= or
   r@ 1+ 6 s" valueP" compare 0= or
   r@ 1+ 6 s" printf" compare 0= or 
   r@ 1+ 5 s" nodes"  compare 0= or 
   r@ 1+ 6 s" case '" compare 0= or 
   
   not if
      \ make the first character UPPER case
      r@ 1 + c@ UPC r@ 1 + c!   
   then

   NO_SPACE_AFTER_// if
      \ remove the space after each "//" , if required by Software Coding Guideline (VW AIO) 
      s" // "  s" //" replaceBuf    \ just replace the first "// " by removing the space
   then

   r> drop

   v_Capitalise_check_only @ 0= if 
      get_TempBuffer0 
      \ -Trailing 
      set_LineBuffer    \ set the processed line back into our line buffer
   else
      get_TempBuffer0 get_LineBuffer compare 0= not if
         1 v_Capitalise_errors +!
         v_Capitalise_check_show_line_numbers @ if
            LineNumber @ 1+ .    \ Note: we count from 0, line numbers count from 1
         then
         \ show_TempBuffers   \ for debug
\         else
         \ ." ." 
      then
   then
;   

: ApplyCapitaliseRules() ( f -- )   \ change or view the line by line array Capitalisation rule
   InitVariables
   0 v_Capitalise_errors !
   ['] Capitalise_line_action() DoLines
   LineArray->Output
   OutputBuf->InputBuf
;

: ApplyCapitaliseRules ( f -- )   \ change the line by line array using the script file rules
   0 v_Capitalise_check_only !
   0 v_Capitalise_check_show_line_numbers !
   ApplyCapitaliseRules()
;

: count_ApplyCapitaliseRules_errors ( -- n )
   -1 v_Capitalise_check_only !
   0 v_Capitalise_check_show_line_numbers !
   ApplyCapitaliseRules()
   v_Capitalise_errors @
;

: Show_CapitaliseRule ( f -- )   \ change the line by line array using the Capitalise rule
   GetFile
   page 
   cr ShowFilename+ cr
   cr ShowFileInfo
   cr 
   cr ." Checking the file for lowercase letters and correct spacing after a '//' comment "
   count_ApplyCapitaliseRules_errors drop
   -1 v_Capitalise_check_only !
   -1 v_Capitalise_check_show_line_numbers !
   v_Capitalise_errors @ if
      cr ." Line numbers that fail the tests : " 
      ApplyCapitaliseRules()
      cr 
      cr ." Capitalisation errors found on "  v_Capitalise_errors @ .  ." lines. "
   else
      cr 
      cr       
      cr ." No Capitalisation errors found. "  
   then
;

: ApplyCapitaliseRulesPlus ( -- )
 
   Show_CapitaliseRule

   v_Capitalise_errors @ 0= if  ekey drop  exit  then 
   
   s" Capitalise first character after '// '"
   s" Apply Capitalise rule to the file?"
   Dialogue
   case
      1 of ( OK )  -1  endof
      2 of ( Cancel )
         2000 200  tone
         \ s" User pressed Escape"   s" Cancelled." Monologue
         0 
      endof
      0
   endcase

   -1 = if  
      ApplyCapitaliseRules
      SaveToFile
   then

   Show_CapitaliseRule
   ekey drop
;

\ ****************************************************************************
\ ****************************************************************************
\ ****************************************************************************
\ ****************************************************************************

: LineHas// ( -- n ) 
   0 Result !
   LineLength @ 1- 0 max 0 ?do
      LineStartPtr i + c@      [char] / =
      LineStartPtr i + 1+  c@  [char] / = and
      if   $2A LineStartPtr i + 1+ c!  1 Result +!  then
   loop
   Result @
;

\ convert  "//xxxx" to /*xxxx */
: Convert//Comments() ( -- )
   LineStatus @ $100 and 0= if   \ not within /* ... */ already
      LineHas// if
         LineStartPtr LineLength @ + >r
         cr r@ 10 - 12 type
         $20 r@ c!
         $2A r@ 1+ c!
         $2F r@ 2 + c!
         r> drop
         3 LineLength +!
      then
   then
;

\ convert all "//xxxx" to /*xxxx */ in file
: Convert//Comments ( -- )  \ change the line by line array
   InitVariables
   ['] Convert//Comments() DoLines
   LineArray->Output
   OutputBuf->InputBuf
;

((
\ ToDo:
: Convert/*Comments*/() ( -- )
      LineHas// if
         LineStartPtr LineLength @ + >r
         cr r@ 10 - 12 type
         $20 r@ c!
         $2A r@ 1+ c!
         $2F r@ 2 + c!
         r> drop
         3 LineLength +!
      then
;

: Convert/*Comments*/ ( -- )  \ change the line by line array
   InitVariables
   ['] Convert/*Comments*/() DoLines
   LineArray->Output
   OutputBuf->InputBuf
;
))

\ ****************************************************************************
\ RemoveTwoBlankLines
\ ****************************************************************************

: RemoveTwoBlankLines() ( -- )
   LineStartPtr LineLength @  BlankLine        \ this is blank line
   dup LastLineWasBlank @ swap LastLineWasBlank !
   and ( both this line and last were blank )
   if 
      next_line_is_start_of_//_comment_block not if    \ if the next line is not the start of a comment block
         LineStatus @ 0x10 or  LineStatus !         \ mark this line as discarded
      then
   then
;

: RemoveTwoBlankLines ( -- )   \ change the line by line array
   InitVariables
   ['] RemoveTwoBlankLines() DoLines
   LineArray->Output
   OutputBuf->InputBuf
;

\ ****************************************************************************
\ ****************************************************************************

\ C compilers often require the file to end with a LineFeed
: EndWithLF ( -- )    \ YYYY

   LineNumber @ >r

   NumInputLines @ 1- 0 max  LineNumber !       \ the last line

   FileFormat @ 1 =  if
      0x01 LineStatus +or
   then

   0x02 LineStatus +or

   r> LineNumber !

   LineArray->Output
   OutputBuf->InputBuf
;

: Weed ( -- )
   ReplaceTabs
   RemoveTrailingSpaces
   RemoveTwoBlankLines
   EndWithLF
\   ConvertBit7sAndDels         \ you probably don't want to do this!
;

\ *********************** Hidden extra functions  *****************************

: Really?  ( z$ -- )
   rel>abs  >r

   MB_TASKMODAL
   MB_OKCANCEL or  \ Ok or cancel?
   MB_ICONQUESTION or  \ addd a big "?"

   z" Hidden and Dangerous!" rel>abs      \ title bar

   r>   \ text

   GetHandleOfMainWindow

   Call MessageBox         \ returns 1 for OK, 2 for Cancel
   1 = not if  abort  then
;

\ ****************************************************************************
variable NumberOfCharsSoFar

: NotBlockFile? ( -- n )  \ actually the number of LineFeeds in the first block
   0
   1024 0 ?do
      InputFilePtr @ i + c@  10 = if  1+  then
   loop
;

: BlocksToNewFile ( -- )  \ convert a block file by adding CRLFs
   z" Do you really want to convert this Block file to a text file?" Really?

   ['] File->InputBuf catch if  0 InputFileLen !  1 DoNotSave +or  exit  then

   NotBlockFile? if  \ the first 1K bytes contain one or more linefeeds
       z" The first 1K bytes contain one or more linefeeds : Press OK if you REALLY want to convert this file" Really?
   then

   s" .f" CurrentFilename  +place  \ add a .f file extension

   s" This may take some time..."
   s" Click OK to convert to file " pad place
   CurrentFilename count pad +place
   s"  " pad +place
   pad count Monologue

   InitVariables
   0 NumberOfCharsSoFar !

   InputFileLen @ 0 ?do
      f-key() femit
      1 NumberOfCharsSoFar +!
      NumberOfCharsSoFar @ $3F and 0= if
         13 femit  10 femit
      then \ add a CRLF every 64 characters
   loop

   SaveToNewFile

   -1 TotalLinesShown !         \ mark as not set

   cr ." done.

   exit
\ copy the output file back into the input buffer, now with added CRLFs
   OutputFilePtr @ InputFilePtr @ OutputFileLen @  move
   OutputFileLen @ InputFileLen !

   InputBuf->LineArray
   -1 TotalLinesShown !         \ mark as not set
;

\ ****************************************************************************

\ A whole file character conversion example
: RemoveBit7sAndDels ( -- )   \ convert them to spaces
   z" Do you really want to remove all Bit7s and Dels?" Really?
   0 OutputFileLen !    \ start to construct the output file at the beginning
   InputFileLen @ 0 ?do
      InputFilePtr @ i + c@
      dup 127 255 1+ within if  drop  32  then
      femit
   loop
   OutputBuf->InputBuf
;

: RemoveComments ( -- )
   z" Do you really want to remove Comments?" Really?
   filtering @ >r
   InitVariables
   filtering on
   InputBuf->LineArray      \ load the line by line array with filtered data
   LineArray->Output
   r> filtering !
;

\ ********************************* Indents  **********************************

: OneIndented->Output ( -- )      \ output a line indented to my_indent_level

   LineStatus @ 0x10 and if  exit  then      \ ignore a discarded line

   ?IndentOK if         \ output the existing line
      #LeadSpaces LineLength @ = not if   \ not a blank line
         LineLength @ 0 ?do  LineStartPtr i +  c@ femit  loop
      then
   else                 \ correct the indent
      #LeadSpaces LineLength @ = not if   \ not a blank line
         my_indent_level @ 60 min  0 max 0 ?do  32 femit  loop
         LineLength @ #LeadSpaces ?do  LineStartPtr i +  c@ femit  loop
      then
   then

   LineStatus @ 1 and if 13 femit  then      \ and a cr if required
   LineStatus @ 2 and if 10 femit  then      \ and a lf if required
;

: .Indent ( -- )       \ display the badly indented line
   ?IndentOK if  exit  then               \ only display the bad ones
   LineLength @ 0= if  exit  then       \ ignore empty lines
   #LeadSpaces LineLength @ = if  exit  then       \ ignore blank lines
   1 accumulateIndentErrors +!
   .Line                        \ show the normal line info
   56 l# @ GotoXY               \ goto a convenient point
   ."   indent ="  #LeadSpaces 3 u.r
   AllowGreaterThan @ if
      ."   should be >= " my_indent_level @ .  8 spaces
   else
      ."   should be =  " my_indent_level @ .  8 spaces
   then
;

: DoIndent() ( -- )
   ?FixIndents @ if             \ we are fixing the indents
      OneIndented->Output
   else                         \ we are displaying the indents
     .Indent
   then
;

: ShowIndentFilename ( -- )
   ?FixIndents @ 0= if   \ do display something
      page
      ShowFilename  ."  : indents"
\      ."   Sum = "  Checksum @ base >r  hex  .  r> base !  decimal
      cr ."      v   "  12 0 do  ." v"  INDENT_SPACES 1- 0 max spaces  loop
   then
;

: DoIndents ( -- )

   InputFileLen @ 0= if  exit  then

   0 accumulate !       \ reset the running total of significant errors
   0 accumulateIndentErrors !
   \ ReplaceTabs           \ tabs would confuse us!

   InitVariables        \ do not clear the Lint variables

   ['] DoIndent() DoLines

   ?FixIndents @ if
      ['] SaveToFile catch if  8 throw  then
      0 ?FixIndents !
      exit     \ don't display anything
   then

   cr

   NextIndentLevel{} @ NextIndentLevelNS @
\   cr ." IL = "  over .  ."   ILNS = "  dup .
   -
   if  \ the namespace keyword adds a {
      cr ." Error  { and } not matched : Indent level at end of file = "
      NextIndentLevel{} @ dup .  ."   ( extra "
      0 < if  ." } )"  else  ." { )"  then
   then

   NextIndentLevel() @ if
      cr ." Error  ( and ) not matched : Parameter indent level at end of file = "
      NextIndentLevel() @ dup .  ."   [ extra "
      0 < if  ." ) ]"  else  ." ( ]"  then
   then

   NextIndentLevel#XXX @ if
      cr ." Error  #if  and  #endif  not matched : Parameter indent level at end of file = "
      NextIndentLevel#XXX @ dup .  ."   [ extra "
      0 < if  ."  #endif ]"  else  ."  #if ]"  then
   then

   cr

\   LineIndent{} @ 0= not if  1 accumulateIndentErrors +!  then

   accumulateIndentErrors @ 0=
   LineIndent() @ 0= and
\   GetCurrentIndent 0= and
   if
      ." ----------------   Indents Pass   -----------------"
   else
      ." *********** " accumulateIndentErrors @ .  ." Indent Error"
      accumulateIndentErrors @ 1 = not if  ." s"  then
      ." ! ***********"
   then

   accumulateIndentErrors @ if
      cr cr
      accumulateIndentErrors @ . ." Indent Error"
      accumulateIndentErrors @ 1 = not if  ." s"  then
   then

\   cr ." Depth = "  Depth .
;

: ShowIndents ( -- )
   ?FixIndents @ >r
   0 ?FixIndents !
   DoIndents
   r> ?FixIndents !
;

: .Number ( n i -- )
    dup 33 < if  2 u.r  else  emit  space  then  5 u.r  ."  | " ;

: .Numbers ( -- )

   page
   ShowFilename
\   ."    CS = "  Checksum @ base >r  hex  .  r> base !  decimal
   cr ." Numbers of each character in the file : "

   128 0 do
      i 7 and 0= if  cr  i 3 u.r  ."  | "  then
      i numbers@ i .Number
   loop

   0  256 128 do  i numbers@ +  loop 0= if  exit  then

   cr ." Press any key for characters from 128 to 255... "

   key drop

   page
   ShowFilename
\   ."    CS = "  Checksum @ base >r  hex  .  r> base !  decimal
   cr ." Number of each character in the file : "
   256 128 do
      i 7 and 0= if  cr  i 3 u.r  ."  | "  then
      i numbers@ i .Number
   loop
;

: ShowSelect ( -- )
   cls  cr ."                            Please select a file  ------> "
;

\ *****************************************************************************
\ Folder Report
\ *****************************************************************************

9 2** constant |FileReportBuffer|  \ must be a power of 2 !!!
|FileReportBuffer| Buffer: FileReportBuffer
variable FileReportPtr
variable NumberOfFilesScanned
variable ReportFileHandle

MAX-PATH Buffer: FolderReportFilename

\ *****************************************************************************

\ time&date
\ 2012Feb29 format
: (date)+ ( yr mth day -- c-addr u2)   BASE @ >R  DECIMAL
   0 <#
\   [CHAR] _ HOLD
   # #
   2drop
   3 * C" JanFebMarAprMayJunJulAugSepOctNovDec" +  3 0 DO  DUP C@ HOLD 1-  LOOP DROP
   0 # # # #
   #>  R> BASE ! ;

: :00+ ( ud1 -- ud2)   DECIMAL  #  6 BASE !  # ( [CHAR] - HOLD ) ;

: (TIME)+ ( hour min sec -- c-addr u)
   BASE @ >R
   0 <#  :00+ 2drop  0 :00+  2drop  0 DECIMAL # #  #>
   R> BASE !
;

: @time ( -- sec min hour )
   get-local-time
   time-buf  8 + w@        \ hours
   time-buf 10 + w@        \ minutes
   time-buf 12 + w@        \ seconds
;

: @date ( -- year month day )
   get-local-time
   time-buf      w@        \ year
   time-buf  2 + w@        \ month of year
   time-buf  6 + w@        \ day of month
;

: tttime  @date (date)+ type  ." _" @time (time)+ type ;

MAX-PATH Buffer: SavedFolderName

\ returns the name of the current folder, excluding path information
: GetFolderName$ ( -- c-addr n )
   MAX-PATH PAD GetCurrentDirectory drop  pad zcount
   begin
      2dup [char] \ scan
   ?dup while
      2swap 2drop  1 /string
   repeat
   drop  0 max
;

: GetSavedFolderName ( -- )
   GetFolderName$ SavedFolderName place
;

: MakeFolderReportFilename ( -- )
   GetSavedFolderName
   s" .\Cweed4FolderReport" FolderReportFilename place
   s" _"  FolderReportFilename append
   SavedFolderName count FolderReportFilename append
\   MAX-PATH PAD GetCurrentDirectory drop  pad zcount FolderReportFilename append
   s" _"  FolderReportFilename append
   @date (date)+ FolderReportFilename append
   s" _"FolderReportFilename append
   @time (time)+ FolderReportFilename append
   s" .txt" FolderReportFilename append
;

: OpenReportFile ( -- )
   MakeFolderReportFilename
   FolderReportFilename count r/w create-file
   if  cr ." Could not create file " FolderReportFilename count type  ."  !"  abort  then
   ReportFileHandle !
;

: InitReportFile ( -- )
   0 FileReportPtr !
   OpenReportFile
;

: pemit ( c -- )
   FileReportBuffer FileReportPtr @ |FileReportBuffer| 1- and + c!
   1 FileReportPtr +!
   1 C# +!
;

: ptype ( c-addr n -- )
   over + swap ?do  i c@ pemit  loop
;

: pcr ( -- )
\   13 pemit  10 pemit
   FileReportBuffer FileReportPtr @ |FileReportBuffer| 1- and ReportFileHandle @ WRITE-LINE drop
   0 FileReportPtr !
   0 C# !
   1 l# +!
;

: pcls ( -- )
   0 C# !  0 L# !
\   pcr
;

: FileConsole ( -- )
    ['] pemit is EMIT
    ['] ptype is TYPE
    ['] pcr   is CR
    ['] pcls  is CLS
    ['] mGotoXY IS GOTOXY
;

: CloseReportFile ( -- )

   ReportFileHandle @ close-file drop
;

MAX-PATH Buffer: SavedCurrentFilename

: CheckFile ( c-addr n -- )

   GetFile  ShowInfo  cr cr  ShowIndents
;

variable FolderReportJustLintErrorFiles

\ : ttwot cr CurrentFilename count dup >r type ."  " 100 r> - 0 max spaces  ." | "  ;
\ 80 constant FILE_NAME_WIDTH

\ : .Len ( u -- )   dup  .  1023 + 1024 /  ."   = "  .  ." K bytes" ;

\ : 2**   ( c -- u )   \ takes 2 to the power c
\    1 swap lshift
\ ;


variable v_FolderNameLength   \ the length of the folder path e.g. "C:\MyFolder", used to remove this when displaying filenames

: ShowFolderFileAction ( a -- f )
   NullConsole          \ disable all text output for now

   0 FileLintErrors !
   .Filename

   MyLongPathBuffer zcount ( 2dup dump key 27 = if  9876 throw  then )  CurrentFilename place
   GetFile

   ShowInfo  cr cr  ShowIndents  \ gather file info for use later - nothing is displayed because of the NullConsole above

\   cr CurrentFilename count dup >r type 100 r> - 0 max spaces  ."  | "  5 u.r  accumulateIndentErrors @ 5 u.r  ShowLintErrors
   FileConsole

   FileLintErrors @ FolderReportJustLintErrorFiles @ 0= or if
      cr FileFormat @ case
         0 of  ." !"  endof   \ mismatched CR and LF
         1 of ( ." P" ) ."  "  endof   \ PC format    \ do not display a "P" if the file has a normal PC format
         2 of  ." U"  endof   \ Unix Format
         3 of  ." 0"  endof   \ zero length file
            ." ?"             \ Unknown format
         endcase
         space
      CurrentFilename count v_FolderNameLength @ /string  type+wrap_short  SECOND_TAB_SHORT GotoColumn   ." | "
      File#Cweed4Errors @ 5 u.r  accumulateIndentErrors @ 5 u.r space  
      File#tabs @ 5 u.r  2 spaces

      \ Check for "// lower case first letter of comment" etc.
      count_ApplyCapitaliseRules_errors dup 6 u.r space Folder_CapitalisationErrors +!

      count_TwoBlankLines_errors        dup 6 u.r space Folder_TwoBlankLines_errors +!

      count_MissingBlankLine_errors     dup 6 u.r space Folder_MissingBlankLine_errors +!


      ShowLintErrors
      ShowFileInfoShort

   then

   1 NumberOfFilesScanned +!

   File#Cweed4Errors @ Folder#Cweed4Errors +!
   File#tabs @ Folder#tabs +!

   FileLintErrors @ if
      1 FolderLintErrors +!
      FileLintErrors @ FolderLintErrorsTotal +!
   then

   0  \ return a true flag to exit the Walk loop...
\   mConsole  ." ."
;

\ \ strips the name and trailing \ from the end of a full pathname.
\ : -NAME ( a n -- a n )
\    2dup begin
\       2dup [char] \ scan
\       ?dup while  2swap 2drop  1 /string
\    repeat drop nip -  1-  0 max
\ ;

: tt-n ( -- )
   s" c:\myfoldername\subfolder\filename.txt" CurrentFilename place
   cr  ." FolderReport for  "  CurrentFilename count -NAME type
;

: ttwfs ( -- )  init_buffers ['] ShowFolderFileAction WalkFiles  .s  de_init_buffers ;
: ttwfs2 ( -- )  init_buffers ['] NullAction WalkFiles  .s  de_init_buffers ;

: ShowFolderReportPreface ( -- )
   page  ." Cweed4 Folder Report "  .month,day,year  space .time  2 spaces  HostVersion type  space  ." www.inventio.co.uk"
   cr    ." FolderReport for  "  MAX-PATH MyLongPathBuffer GetCurrentDirectory drop  MyLongPathBuffer zcount  
   dup   
   1+    \ add the '\' to the folder path name length to remove it from each filename
   v_FolderNameLength !  type 
   ." \"    \ add the slash to the folder path
   2 spaces  ShowFolderFilesTypes
   cr
;

: ShowFolderReportResult ( -- )
   cr  ." Number of files scanned                 = " NumberOfFilesScanned @ .   ."     "  .elapsed
   cr  ." Total number of Cweed4 errors           = " Folder#Cweed4Errors @ .
   cr  ." Total number of Capitalisation errors   = " Folder_CapitalisationErrors @ .
   cr  ." Total number of TwoBlankLines errors    = " Folder_TwoBlankLines_errors @ .
   cr  ." Total number of MissingBlankLine errors = " Folder_MissingBlankLine_errors @ .

   cr  ." Total number of tabs (09) in the folder = " Folder#tabs @ .
   FolderLintErrors @ if
      cr  FolderLintErrorsTotal @ . ." Lint errors found in "  FolderLintErrors @ .  ." file"  FolderLintErrors @ 1 = not if  ." s"  then   ." !!!"
   then
   cr    \ needed to write to the file
;

: FolderReport ( -- )

   mconsole

   page
   cr
   ShowFolderReportPreface
\   ." FolderReport for  "  MAX-PATH MyLongPathBuffer GetCurrentDirectory drop  MyLongPathBuffer zcount type
\   CurrentFilename count -name  swap drop 0=  drop -1
   cr
   time-reset
   InitReportFile
   NoMD5 @ >r

   DISABLE_MD5_IN_FOLDER_REPORT? NoMD5 !

   0 NumberOfFilesScanned !
   0 Folder#tabs !
   0 Folder#Cweed4Errors !
   0 FolderLintErrors !
   0 FolderLintErrorsTotal !
   0 Folder_CapitalisationErrors !
   0 Folder_TwoBlankLines_errors !                              
   0 Folder_MissingBlankLine_errors !

   CurrentFilename count SavedCurrentFilename place

   FileConsole
   ShowFolderReportPreface
   cr ."   Filename"   SECOND_TAB_SHORT GotoColumn ."   Cweed4 Indents Tabs //caps 2Blank -Blank Lint     Sum     CRC       MD5"

   ['] ShowFolderFileAction WalkFiles

   FileConsole
   cr
   ShowFolderReportResult

\   drop \ no idea where this comes from :-(

   SavedCurrentFilename count CurrentFilename place

   ['] CloseReportFile catch drop

   r> NoMD5 !

   mConsole
\   page  \ we are sharing the C# and L# variables between mConsole and FileConsole, so we must reposition here
   0 2 AT-XY   \ we are sharing the C# and L# variables between mConsole and FileConsole, so we must reposition here
   ShowFolderReportResult
   cr  ." Report saved to " FolderReportFilename count type ."  in folder :"
   cr  MAX-PATH PAD GetCurrentDirectory drop  pad zcount type
   cr
   cr  ." Use 'Select File' to select a file in the folder to scan"
   cr  ." Press 'L' to toggle only files with Lint errors in the report"
   cr
   .StackDepths
   cr
   cr ." Done."

;

\ *****************************************************************************
\ Display modes
\ *****************************************************************************

: RefreshDisplay ( -- )   \ redisplay the file in the current DispMode

   RESET-STACKS

   InputFileLen @ 0= if       \ no file selected
      DispMode @ 5 = not if   \ and we are not displaying the help screen
         0 DispMode !         \ so don't try to display it!
      then
   then

   DispMode @ case
\     -1 of  DispModeSave @ DispMode !  endof
      0 of  ShowSelect  endof
      1 of  ShowInfo  cr cr  ShowIndents  endof
      2 of  ShowFile  endof
      3 of  ShowIndentFilename  DoIndents endof
      4 of  .Numbers  endof
      5 of  ShowHelp  SavedDispMode @ DispMode !  endof
      6 of  FolderReport  SavedDispMode @ DispMode !  endof     \ display the message at the start of the Folder Report scan
      7 of  Show_BarrReport  endof
\      8 of  SavedDispMode @ DispMode !  endof
\      99 of  1 DispMode !  endof     \ do nothing, leave what is on the display
   endcase

;

\ ***** program end *****
