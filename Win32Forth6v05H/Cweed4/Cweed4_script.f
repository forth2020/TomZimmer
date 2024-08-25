\ Cweed4_script.f  2022 May 17
\ 
\ This is always going to be "work in progress" - the combination of the keywords of the
\ C language with the Barr coding rules means that it can probably never be perfect.
\ 
\ ToDo: any lines containing two or more '"' characters are not processed. 
\       this should be only the text between quotation marks that is ignored
\ ToDo: any lines containing comments are not processed
\       it should be only commented out text that is ignored
\ 
\ *****************************************************************************
\ Barr Coding Standard (2018) White Space Rules 3.1
\ https://barrgroup.com/sites/default/files/barr_c_coding_standard_2018.pdf
\ https://barrgroup.com/embedded-systems/books/embedded-c-coding-standard/white-space-rules/spaces
\ *****************************************************************************
\ 3.1 Spaces
\ Rules:
\ a. Each of the keywords if, while, for, switch, and return shall be followed
\ by one space when there is additional program text on the same line.
\ b. Each of the assignment operators =, +=, -=, *=, /=, %=, &=, |=, ^=, ~=, and !=
\ shall always be preceded and followed by one space.
\ c. Each of the binary operators +, -, *, /, %, <, <=, >, >=, ==,!=, <<, >>, &, |, ^, &&, and || 
\ shall always be preceded and followed by one space.
\ d. Each of the unary operators +, -, ++, --, ! , and ~, shall be written without a
\ space on the operand side.
\ e. The pointer operators * and & shall be written with white space on each side
\ within declarations but otherwise without a space on the operand side.
\ f. The ? and : characters that comprise the ternary operator shall each always
\ be preceded and followed by one space.
\ g. The structure pointer and structure member operators (-> and ., respectively) (  
\ shall always be without surrounding spaces.
\ h. The left and right brackets of the array subscript operator ([ and ]) shall be 
\ without surrounding spaces, except as required by another white space rule.
\ i. Expressions within parentheses shall always have no spaces adjacent to the
\ left and right parenthesis characters.
\ j. The left and right parentheses of the function call operator shall always be
\ without surrounding spaces, except that the function declaration shall feature
\ one space between the function name and the left parenthesis to allow that
\ one particular mention of the function name to be easily located.
\ k. Except when at the end of a line, each comma separating function parameters
\ shall always be followed by one space.

ANEW Cweed4_script   \ create a marker so that we can FORGET from here

\  "XXXX ;  " --> "XXXX;  "   preserves trailing spaces
: remove_lastButOne_space ( -- )
   get_TempBuffer0 swap drop 3 < if  exit  then    \ do not do this if the line is too short
   [char] ; get_TempBuffer0 -Trailing + 2 - c!  \ set the last but one character to a ';'
   BL get_TempBuffer0 -Trailing + 1 - c!        \ set the last but one character to a space
   v_TempLineBufferPtr0 @ 1- 0 max v_TempLineBufferPtr0 !    \ chop the last character off
;

: line_starts_at_column_0 ( -- f )
    LineStartPtr c@ BL = not   
;

\ work out if the current line is either a function definition or function declaration
: line_is_a_definition_or_declaration ( -- f )  \
   \ extern void MyFunction ( uint8_t myValue );
   0
   LineStartPtr LineLength @ s" static"   startsWith or
   LineStartPtr LineLength @ s" extern"   startsWith or
   LineEndsWith; and
   if  -1  exit  then  \ if the line starts with one of these, and ends with a ';' it is a declaration

   \ void MyFunction ( uint8_t myValue )
   LineEndsWith; if  0 exit  then  \ other than the above, if it ends with a ';' it cannot be a definition or declaration

   0
   LineStartPtr LineLength @ s" if"     startsWith or
   LineStartPtr LineLength @ s" while"  startsWith or
   LineStartPtr LineLength @ s" for"    startsWith or
   LineStartPtr LineLength @ s" switch" startsWith or
   LineStartPtr LineLength @ s" return" startsWith or
   if  0  exit  then    \ if it starts with a keyword it cannot be a definition      
   
   line_starts_at_column_0 not if  0 exit  then     \ all definitions and declarations must start at column 0

   -1
;

\ work out if the current line is a function definition
: line_is_a_definition ( -- f )  \ 

\   line_starts_at_column_0 not if  0 exit  then     \ all definitions and declarations must start at column 0. No - functions can be indented within #ifdefs
   LineEndsWith; if  0 exit  then  \ if it ends with a ';' it cannot be a definition

   \ extern void MyFunction ( uint8_t myValue );
   0
\   LineStartPtr LineLength @ s" static"   startsWith or
   LineStartPtr LineLength @ s" extern"   startsWith or
   if  0  exit  then  \ if the line starts with this it is a declaration, so is not a definition

   \ void MyFunction ( uint8_t myValue )

   0
   LineStartPtr LineLength @ s" if"     startsWith or
   LineStartPtr LineLength @ s" while"  startsWith or
   LineStartPtr LineLength @ s" for"    startsWith or
   LineStartPtr LineLength @ s" switch" startsWith or
   LineStartPtr LineLength @ s" return" startsWith or
   if  0  exit  then    \ if it starts with a keyword it cannot be a definition      
   
   -1
;

\ return the number of quotation marks in the given string
: number_of_quote_characters ( -- c )
   0 -rot  \ initial count
   over + swap ?do  i c@ [char] " ( " ) = if  1+  then  loop
;

\ get the current LineBuffer contents 
: get_LineBuffer ( -- $ )   LineStartPtr LineLength @ ;

\ copy the our temporary buffer back into the LineBuffer
: set_LineBuffer ( $ -- )   ( n -- )  LineLength !  ( a -- )  LineStartPtr LineLength @ move ;

variable v_withinParens

: 2.hex ( c -- )   base @ >r  hex  0 <#  # #  #> type  r> base ! ;

\ dump on the same line
: dumpL ( a n -- )   0 max  $200 min  space  dup 2.hex ."  |  " over + swap ?do  i c@  2.hex space  loop ; 

\ The only places that there can be more than one space together is at the start and end of the line
\ so the number of Leading and Trailing spaces is saved and restored at the end
\ 
\ Operators can be one or two characters long, for example '=" or '=='
\ to avoid interpreting '==' as two '=' , the marked versions of 2-character names are processed first
\ s" §=§§=§"  s"  == "    replaceBuf 
\ '==' --> '§=§§=§' --> ' == '
\ then the single character marked names are processed 
\ s" §=§"     s"  = "     replaceBuf
\ the 5 0 do ... loop  processes up to 5 occurances of the name on one line

\ Uses the TempLineBuffer0[] to adjust whitespace on one line
: AddSpacesAroundKeywords ( -- )    
   \ Rule c.
   init_TempLineBuffer2
   0 v_withinParens !

   get_TempBuffer0 
   -Leading        \ remove leading spaces

   \ wrap every character with two special '§' characters
   over + swap ?do
      '§' TempLineBuffer2_put8     \ '§' == $A7  added before every character
      i c@ BL = if   \ mark a hard space, to be put back at the end
         '¨' TempLineBuffer2_put8  \ '¨' == $A8  makes a "hard space", one that always appears in the output
      else   
         i c@ TempLineBuffer2_put8
      then
      '§' TempLineBuffer2_put8     \ '§' == $A7  added after every character
   loop
   '§' TempLineBuffer2_put8     \ '§' == $A7  added after the string, to allow a final space to be converted

   get_TempBuffer2  copy_toTempBuffer0

   \ Note: in Forth, s" ABC" represents the string "ABC" . The space after the s" is part of the s" word.
   \ Having added '§' around every character, remove them again in order, 
   \ adding spaces on either side, or not, as required.
   \ Process the longer operator names first, to avoid treating "==" as two "=" characters.

   \ Rule d. Each of the unary operators +, -, ++, --, ! , and ~,  have no space on the left
   \ 2-character operators with no leading space
   5 0 do  s" §+§§+§"  s" ++ "     replaceBuf  loop \ originally "++" --> "++ " ( with a space on the right)
   5 0 do  s" §-§§-§"  s" -- "     replaceBuf  loop

   \ Rule b. Each of the assignment operators =, +=, -=, *=, /=, %=, &=, |=, ^=, ~=, and !=  have a space on either side
   \ 2-character assignment operators with leading and trailing spaces
   5 0 do  s" §+§§=§"  s"  += "    replaceBuf  loop \ originally "+=" --> " += " ( with spaces on both sides)
   5 0 do  s" §-§§=§"  s"  -= "    replaceBuf  loop
   5 0 do  s" §*§§=§"  s"  *= "    replaceBuf  loop
   5 0 do  s" §/§§=§"  s"  /= "    replaceBuf  loop
   5 0 do  s" §%§§=§"  s"  %= "    replaceBuf  loop
   5 0 do  s" §&§§=§"  s"  &= "    replaceBuf  loop
   5 0 do  s" §|§§=§"  s"  |= "    replaceBuf  loop
   5 0 do  s" §^§§=§"  s"  ^= "    replaceBuf  loop
   5 0 do  s" §~§§=§"  s"  ~= "    replaceBuf  loop
   5 0 do  s" §!§§=§"  s"  != "    replaceBuf  loop
   \ also the shift operators with an = sign (not mentioned in the Barr spec.)
   5 0 do  s" §<§§<§§=§" s"  <<= " replaceBuf  loop
   5 0 do  s" §>§§>§§=§" s"  >>= " replaceBuf  loop

   \ Rule c. Each of the binary operators +, -, *, /, %, <, <=, >, >=, ==,!=, <<, >>, &, |, ^, &&, and ||  have a space on either side
   \ 2-character operators with leading and trailing spaces
   5 0 do  s" §=§§=§"  s"  == "    replaceBuf  loop
   5 0 do  s" §!§§=§"  s"  != "    replaceBuf  loop
   5 0 do  s" §>§§=§"  s"  >= "    replaceBuf  loop
   5 0 do  s" §<§§=§"  s"  <= "    replaceBuf  loop
   5 0 do  s" §<§§<§"  s"  << "    replaceBuf  loop
   5 0 do  s" §>§§>§"  s"  >> "    replaceBuf  loop
   5 0 do  s" §&§§&§"  s"  && "    replaceBuf  loop
   5 0 do  s" §|§§|§"  s"  || "    replaceBuf  loop


   \ 1-character binary operators
   5 0 do  s" §*§"  s"  * "        replaceBuf  loop
   5 0 do  s" §/§"  s"  / "        replaceBuf  loop
   5 0 do  s" §%§"  s"  % "        replaceBuf  loop
   5 0 do  s" §<§"  s"  < "        replaceBuf  loop
   5 0 do  s" §>§"  s"  > "        replaceBuf  loop
   5 0 do  s" §&§"  s"  & "        replaceBuf  loop
   5 0 do  s" §|§"  s"  | "        replaceBuf  loop
   5 0 do  s" §^§"  s"  ^ "        replaceBuf  loop
   5 0 do  s" §+§"  s"  + "        replaceBuf  loop    \ ??? how to tell if this is a unary or binary operator???
   5 0 do  s" §-§"  s"  - "        replaceBuf  loop    \ ??? how to tell if this is a unary or binary operator???

   \ add a space on either side of ';' The ';' at the end of the line is tidied up later, without a preceding space
   5 0 do  s" §;§"  s"  ; "        replaceBuf  loop

   \ "case 123:" in a switch statment does not have a space before the ':', only ternary operators, which have a '?" .
   get_TempBuffer0 s" ?" Contains if 
      \ f. The ? and : characters that comprise the ternary operator have a space on either side
      5 0 do  s" §?§"  s"  ? "        replaceBuf  loop
      5 0 do  s" §:§"  s"  : "        replaceBuf  loop
   then

   \ Rule b. Each of the assignment operators =, +=, -=, *=, /=, %=, &=, |=, ^=, ~=, and !=  have a space on either side
   \ 1-character assignment operators with leading and trailing spaces
   5 0 do  s" §=§"     s"  = "     replaceBuf  loop

   \ Rule d. Each of the unary operators +, -, ++, --, ! , and ~,  have no space on the operand side
   \ 1-character unary operators with no space on the operand side
   \    5 0 do  s" §+§"     s" + "       replaceBuf  loop     \ ??? how to tell if this is a unary or binary operator???
   \    5 0 do  s" §-§"     s" - "       replaceBuf  loop     \ ??? how to tell if this is a unary or binary operator???
   5 0 do  s" §!§"     s"  !"       replaceBuf  loop
   5 0 do  s" §~§"     s"  ~"       replaceBuf  loop

   \ similarly ',' in a list of function arguments only has a space to the right
   5 0 do  s" §,§"     s" , "       replaceBuf  loop

   \ 1-character keywords with spaces on both sides
   5 0 do  s" §(§"  s" ("          replaceBuf  loop
   5 0 do  s" §)§"  s" )"          replaceBuf  loop

   \    5 0 do  s" §(§"  s" ("          replaceBuf  loop
   \    5 0 do  s" §)§"  s" )"          replaceBuf  loop

   \ remove spaces around [ and ]
\   5 0 do  s"  §[§"  s" ["         replaceBuf  loop
\   5 0 do  s" §]§ "  s" ]"         replaceBuf  loop

   \ convert any marked hard spaces '§¨§' back to a space
   \ Note : the 2/ is to avoid "replacing" over the end of the buffer - this could be done better...
   |TempLineBuffer| 2/ 0 do  s" §¨§"  s"  "  replaceBuf  loop   
   \ remove any remaining marker characters
   |TempLineBuffer| 2/ 0 do  s" §"  s" "     replaceBuf  loop
   |TempLineBuffer| 2/ 0 do  s" ¨"  s"  "    replaceBuf  loop

   \ remove any spaces after the opening '(' and before the closing ')'
   5 0 do  s" ( "  s" ("           replaceBuf  loop
   5 0 do  s"  )"  s" )"           replaceBuf  loop

   \ clean up the final ';'
   1 0 do  s" ) ;"  s" );"         replaceBuf  loop
   1 0 do  s"  );"  s" );"         replaceBuf  loop

   \ tidy up some double spaces, convert to single spaces
   5 0 do  s"   ;"  s"  ;"         replaceBuf  loop
   5 0 do  s" ;  "  s" ; "         replaceBuf  loop
   5 0 do  s"   ="  s"  ="         replaceBuf  loop
   5 0 do  s" =  "  s" = "         replaceBuf  loop

   \ Rule c. Each of the binary operators +, -, *, /, %, <, <=, >, >=, ==,!=, <<, >>, &, |, ^, &&, and ||  have a space on either side
   \ Tidy up 2-character operators with leading and trailing spaces, remove double spaces
   5 0 do  s"   != "  s"  != "    replaceBuf  loop
   5 0 do  s"   == "  s"  == "    replaceBuf  loop
   5 0 do  s"   >= "  s"  >= "    replaceBuf  loop
   5 0 do  s"   <= "  s"  <= "    replaceBuf  loop
   5 0 do  s"   << "  s"  << "    replaceBuf  loop
   5 0 do  s"   >> "  s"  >> "    replaceBuf  loop
   5 0 do  s"   && "  s"  && "    replaceBuf  loop
   5 0 do  s"   || "  s"  || "    replaceBuf  loop

   5 0 do  s"  !=  "  s"  != "    replaceBuf  loop
   5 0 do  s"  ==  "  s"  == "    replaceBuf  loop
   5 0 do  s"  >=  "  s"  >= "    replaceBuf  loop
   5 0 do  s"  <=  "  s"  <= "    replaceBuf  loop
   5 0 do  s"  <<  "  s"  << "    replaceBuf  loop
   5 0 do  s"  >>  "  s"  >> "    replaceBuf  loop
   5 0 do  s"  &&  "  s"  && "    replaceBuf  loop
   5 0 do  s"  ||  "  s"  || "    replaceBuf  loop

   \ Rule c. Each of the binary operators +, -, *, /, %, <, <=, >, >=, ==,!=, <<, >>, &, |, ^, &&, and ||  have a space on either side
   \ Tidy up 2-character operators with leading and trailing spaces, remove double spaces
   \ 1-character binary operators
   5 0 do  s"   * "  s"  * "        replaceBuf  loop
   5 0 do  s"   / "  s"  / "        replaceBuf  loop
   5 0 do  s"   % "  s"  % "        replaceBuf  loop
   5 0 do  s"   < "  s"  < "        replaceBuf  loop
   5 0 do  s"   > "  s"  > "        replaceBuf  loop
   5 0 do  s"   & "  s"  & "        replaceBuf  loop
   5 0 do  s"   | "  s"  | "        replaceBuf  loop
   5 0 do  s"   ^ "  s"  ^ "        replaceBuf  loop
   5 0 do  s"   + "  s"  + "        replaceBuf  loop    \ ??? how to tell if this is a unary or binary operator???
   5 0 do  s"   - "  s"  - "        replaceBuf  loop    \ ??? how to tell if this is a unary or binary operator???

   5 0 do  s"  *  "  s"  * "        replaceBuf  loop
   5 0 do  s"  /  "  s"  / "        replaceBuf  loop
   5 0 do  s"  %  "  s"  % "        replaceBuf  loop
   5 0 do  s"  <  "  s"  < "        replaceBuf  loop
   5 0 do  s"  >  "  s"  > "        replaceBuf  loop
   5 0 do  s"  &  "  s"  & "        replaceBuf  loop
   5 0 do  s"  |  "  s"  | "        replaceBuf  loop
   5 0 do  s"  ^  "  s"  ^ "        replaceBuf  loop
   5 0 do  s"  +  "  s"  + "        replaceBuf  loop    \ ??? how to tell if this is a unary or binary operator???
   5 0 do  s"  -  "  s"  - "        replaceBuf  loop    \ ??? how to tell if this is a unary or binary operator???

   \ 1-character assignment operators with leading and trailing spaces
   5 0 do  s"   = "     s"  = "     replaceBuf  loop
   5 0 do  s"  =  "     s"  = "     replaceBuf  loop

   \ remove a leading spaces in a ',' separated argument list
   5 0 do  s"  ,"      s" , "       replaceBuf  loop

   \ remove a space after an '&' if it occurs after a '(', because it must be an address operator, not a binary AND
   5 0 do  s" (& "      s" (&"       replaceBuf  loop

   \ "xxxxx ;" --> "xxxxx;"
   TempBufferEndsWith_; if 
      remove_lastButOne_space
   then

   \ tidy up 
   5 0 do  s" ] ;"     s" ];"       replaceBuf  loop
    
   \ Replace multiple spaces by one space
\   5 0 do  s"     "    s"  "        replaceBuf  loop
   5 0 do  s"    "     s"  "        replaceBuf  loop
   5 0 do  s"   "      s"  "        replaceBuf  loop

   \ add the processed text in TempLineBuffer0[] to the indentation spaces
   get_TempBuffer0 -Leading -Trailing append_TempBuffer3
   \ put back the trailing spaces
   v_NumberOfTrailingSpaces @ 0 max 0 ?do  BL putChar_TempBuffer3  loop
   get_TempBuffer3 copy_toTempBuffer0
;

: show_TempBuffers ( -- )
   cr ."  was  : |||" get_LineBuffer   2dup type 2drop ." |||"   \ spaces dumpL  
   ."   leading = " v_NumberOfLeadingSpaces @ .  ."   trailing = " v_NumberOfTrailingSpaces @ .
   cr ."  buf2 : |||" get_TempBuffer2  2dup type 2drop ." |||"   \ spaces dumpL
   cr ."  buf3 : |||" get_TempBuffer3  2dup type 2drop ." |||"   \ spaces dumpL
   cr ."  is   : |||" get_TempBuffer0  2dup type 2drop ." |||"   \ spaces dumpL 
   cr ."  KAT  : |||" get_TempBuffer1  2dup type 2drop ." |||"   \ spaces dumpL 
   cr                                                                 
;

: ApplyBarrWhitespaceRules_script ( -- )
   LineLength @ 254 > if  s" Line too long" s" ..." Monologue  exit  then   \ ignore lines that are too long

   LineStartPtr LineLength @ -Trailing swap drop 0= if   exit  then         \ ignore empty lines 

   LineStartPtr LineLength @ s" #define"  startsWith if  exit  then         \ do not touch #defines     
   LineStartPtr LineLength @ s" #include"  startsWith if  exit  then        \ do not touch #includes     

   LineStartPtr LineLength @ number_of_quote_characters 1 > if  exit  then  \ do not touch lines containing "strings" 

   \ 0x01 = cr, 0x02 = lf, 0x10 = discard on output, 0x100 /* ... */, 0x200 = //
   LineStatus @ $300 and if  exit  then  \ within /* ... */ or \ so do nothing, do not touch comments     

   \ count leading and trailing spaces, save for later
   LineStartPtr LineLength @ NumberOfLeadingSpaces   v_numberOfLeadingSpaces !
   LineStartPtr LineLength @ NumberOfTrailingSpaces  v_numberOfTrailingSpaces !
   LineStartPtr  v_numberOfLeadingSpaces @  copy_toTempBuffer3  \ save the indentation spaces
   
   \ load our temporary buffer
   LineStartPtr LineLength @ -Leading  copy_toTempBuffer0

   \ Rule h.  The left and right brackets of the array subscript operator ([ and ]) shall be without surrounding spaces
   5 0 do  s"  ["  s" [" replaceBuf  loop  \ h. 
   5 0 do  s" [ "  s" [" replaceBuf  loop  \ h.
   5 0 do  s"  ]"  s" ]" replaceBuf  loop  \ h.

   line_is_a_definition if
      \ Rule j.
      \ do not remove the space when a function is defined, add one instead
      s" ("  s" _(" replaceBuf                  \ mark the first '(' by adding a '_'
      5 0 do  s"  _("  s" _(" replaceBuf  loop  \ remove spaces before 
      s" _("  s"  (" replaceBuf                 \ put just one space back

      \ remove any spaces after the opening '(' and before the closing ')'
      5 0 do  s" ( "  s" ("           replaceBuf  loop
      5 0 do  s"  )"  s" )"           replaceBuf  loop
   else
      \ Rule a.  if, while, for, switch, and return
      s" if ("     s" if_("     replaceBufAtStartOfLine  \ to prevent getting the space removed
      s" while ("  s" while_("  replaceBufAtStartOfLine  \ to prevent getting the space removed
      s" for ("    s" for_("    replaceBufAtStartOfLine  \ to prevent getting the space removed
      s" switch (" s" switch_(" replaceBufAtStartOfLine  \ to prevent getting the space removed
      s" return (" s" return_(" replaceBufAtStartOfLine  \ to prevent getting the space removed

      s" if("      s" if_("     replaceBufAtStartOfLine  \ to prevent getting the space removed
      s" while("   s" while_("  replaceBufAtStartOfLine  \ to prevent getting the space removed
      s" for("     s" for_("    replaceBufAtStartOfLine  \ to prevent getting the space removed
      s" switch("  s" switch_(" replaceBufAtStartOfLine  \ to prevent getting the space removed
      s" return("  s" return_(" replaceBufAtStartOfLine  \ to prevent getting the space removed

      \ Rule i.
      \ remove additional spaces
      5 0 do  s"  ("  s" (" replaceBuf  loop  \ i.
      5 0 do  s" ( "  s" (" replaceBuf  loop  \ i.
      5 0 do  s"  )"  s" )" replaceBuf  loop  \ i.

      \ Rule a.  if, while, for, switch, and return
      \ put back just one spaces after each keyword
      s" if_("     s" if ("     replaceBufAtStartOfLine     \ put the space back now
      s" while_("  s" while ("  replaceBufAtStartOfLine     \ put the space back now
      s" for_("    s" for ("    replaceBufAtStartOfLine     \ put the space back now
      s" switch_(" s" switch (" replaceBufAtStartOfLine     \ put the space back now
      s" return_(" s" return (" replaceBufAtStartOfLine     \ put the space back now

   then

   AddSpacesAroundKeywords       

   \ Rule g.
   5 0 do  s" -> "      s" ->"     replaceBuf  loop     \ remove surrounding spaces 
   5 0 do  s"  ->"      s" ->"     replaceBuf  loop     \ remove surrounding spaces 

   v_Cweed4_script_check_only @ 0= if 
      get_TempBuffer0 
      \ -Trailing 
      set_LineBuffer    \ set the processed line back into our line buffer
   else
      get_TempBuffer0 get_LineBuffer compare 0= not if
         1 v_Cweed4_script_NumberOfErrors +!
         v_Cweed4_script_check_show_line_numbers @ if
            ." #" LineNumber @ 1+ .    \ Note: we count from 0, line numbers count from 1
         then
         \ show_TempBuffers   \ for debug
         else
         \ ." ." 
      then
   then
;

\ set the xt for the line action into the variable
' ApplyBarrWhitespaceRules_script v_Cweed4_script_line_action !


\ *****************************************************************************
\ Tests
\ *****************************************************************************

: tt_line_is_a_definition_one ( $ -- f )
   cr 2dup type  2 spaces  
   set_LineBuffer  
   line_is_a_definition if  ." definition"   else  ." no "  then
;

: tt_line_is_a_definition ( -- )
   init_buffers
   s" myOtherFunction ( myArray );"                tt_line_is_a_definition_one
   s" void MyFunction ; "                          tt_line_is_a_definition_one
   s" for MyFunction  "                            tt_line_is_a_definition_one
   s" extern void MyFunction ( uint8_t myValue ) " tt_line_is_a_definition_one
   s" void MyFunction ( uint8_t myValue ) "        tt_line_is_a_definition_one
   s" void Sw_Procedure_Datenverarbeitung(void);"  tt_line_is_a_definition_one
   de_init_buffers
;
   
variable v_numberOfTests
variable v_numberOfFailures

\ Test one string
: tt_barr_one ( $ $kat -- )
   2dup copy_toTempBuffer1    \ to display later if there is an error
   2>r  \ the Known Answer Test
   LineLength !  
   LineStartPtr LineLength @ move  
   LineStartPtr LineLength @ cr ." test " v_numberOfTests @ 2 u.r  ."  |||" type ." |||" 
   v_Cweed4_script_line_action @ execute
   LineStartPtr LineLength @ cr ."         |||" 2dup type ." |||" 
   2r> compare 0= not if
      ."  ***failed*** "
      show_TempBuffers
      1 v_numberOfFailures +!
   else 
      ."  passed "
   then
   1 v_numberOfTests +!
   cr
;

\ Test a variety of strings 
: tt_ApplyBarrWhitespaceRules ( -- )
   0 v_Cweed4_script_check_only !   \ actually change the line

   0 v_numberOfTests !
   0 v_numberOfFailures !

   page
   init_buffers
   \   input string                                   Expected KAT result
   s"     (myArray[17] == 0) "                     s"     (myArray[17] == 0) "                     tt_barr_one
   s"     (myArray[ 17  ]==0) "                    s"     (myArray[17] == 0) "                     tt_barr_one
   s" uint8_t i; "                                                                            2dup tt_barr_one
   s"  if( !(myA [ 17 ]==0) )"                     s"  if (!(myA[17] == 0))"                       tt_barr_one
   s" c=(a<b)?a:b;"                                s" c = (a < b) ? a : b;"                        tt_barr_one
   s"    for( i=123;i<130;i++) "                   s"    for (i = 123 ; i < 130 ; i++) "           tt_barr_one
   s" myOtherFunction ( myArray );"                s" myOtherFunction(myArray);"                   tt_barr_one
   s" myArray [ 8 ]     "                          s" myArray[8]     "                             tt_barr_one
   s" not_an_int myFunction ( 8 )     "            s" not_an_int myFunction (8)     "              tt_barr_one
   s" int myFunction ( 8 )     "                   s" int myFunction (8)     "                     tt_barr_one
   s" myFunction ( uint8_t myValue );     "        s" myFunction(uint8_t myValue);     "           tt_barr_one
   s" void myFunction ( uint8_t myValue );     "   s" void myFunction(uint8_t myValue);     "      tt_barr_one
   s"  void myFunction ( uint8_t myValue );     "  s"  void myFunction(uint8_t myValue);     "     tt_barr_one
   s" extern void myFunction ( uint8_t myVal);  "  s" extern void myFunction (uint8_t myVal);  "   tt_barr_one
   s" static void myFunction ( uint8_t myVal);  "  s" static void myFunction (uint8_t myVal);  "   tt_barr_one
   s"     if( myArray [ 0 ] != 0 )"                s"     if (myArray[0] != 0)"                    tt_barr_one
   s"     if( indented with 4 spaces )"            s"     if (indented with 4 spaces)"             tt_barr_one
   s" void MyFun( int  arg1  , uint arg2  )"       s" void MyFun (int arg1, uint arg2)"            tt_barr_one
   s" void MyFun( int  arg1  , uint arg2  );"      s" void MyFun(int arg1, uint arg2);"            tt_barr_one
   s" int a   =10   , b=  20, c ;"                 s" int a = 10, b = 20, c;"                      tt_barr_one
   s"     tmr_Runtime = tmr_Runtime + (((uint32_t)p_tMinZustandsdauer * (uint32_t)1000) / (uint32_t)p_tTickPeriod);" tt_barr_one
   de_init_buffers

   v_numberOfFailures @ 0= if
      cr ." All tests passed :-) "
   else
      cr v_numberOfFailures @ . ." test" v_numberOfFailures @ 1 = not if ." s"  then ."  failed!!!" 
   then
;

: ttbarr ( -- )
   tt_ApplyBarrWhitespaceRules
;

: tt_compare_one ( $ $ f -- )
   >r 
   cr 2over 2 spaces 2dup type  dumpL  2 spaces  2dup 2 spaces 2dup type  dumpL  2 spaces compare dup  ."    result = " .
   r> 2dup = not if  ."   <<<<<<<<<<--------------------- failed = " . .  else  2drop  then
;
: tt_compare ( -- )
   \  string0              string1           should be
   s" TheSame"          s" TheSame"          0  tt_compare_one
   s" TheSame"          s" NotTheSame"       1  tt_compare_one
   s" TheSame" drop 0   s" TheSame"         -1  tt_compare_one
   s" TheSame"          s" TheSame" drop 0   1  tt_compare_one
   s" TheSame" drop 0   s" TheSame" drop 0   0  tt_compare_one
;
