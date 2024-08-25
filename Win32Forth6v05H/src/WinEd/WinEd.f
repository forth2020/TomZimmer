(( WinEd.F     An Editor for Win32Forth by Tom Zimmer Globalization by -rbs
\ 1
  WinEd.F         An Editor for Win32Forth     Origated by Tom Zimmer
  Revision 2.09a  Revised April 23rd, 2003     Works with Win32Forth 6.nn

  **************************************************************************
   *                                                                      *
    *   NOTE: At Tom Zimmer's request, WinView has been re-named WinEd   *
   *                                                                      *
  **************************************************************************

  WinEd is a multiple file text editor for Win32Forth.
  Features, changes and enhancements are noted at the end of the file.
  Use Ctrl+End to get there instantly.  Then Ctrl+PgUp to the paragraph.
))

Anew wined-2
cr .( Loading WinEd...)

False value colons-only         \ rda   could be in the editor variables????

WinLibrary WININET.DLL

only forth also definitions

\ moved so we can use in more than one place    \ rls January 3rd, 2001 - 3:06
: /split        { str len part -- remainder len1 prefix len2 }
                str len part /string            \ adr len of remainder of string
                str len part min 0max ;         \ adr len of prefix of string

editor also definitions
Defer MY-APPLICATION
((  My-Application is bound to Ctrl+E in Wined.f in view-key-loop. It is or
will be resolved to _EXTEND-PRICES in an application named CG.F  Feel free to
vector it to what ever you want in your own application. Please do not remove
this word from this file.  It uses very little memory and will do no harm.  jap
))

: "GetDefault   ( a1 n1 -- a2 n2 )
                s" Settings" RegGetString ;

\ Write the a2,n2 sub-section section of 'Settings' to a data string of a1,n1
\ which could be the ascii comma delimited x,y coordinate of where the
\ window should be placed next time the application starts up.

: "SetDefault   ( a1 n1 a2 n2 -- )
                s" Settings" RegSetString ;

\ rls January 2nd, 2001 - 16:06
FALSE value RectHigh
0x0100 constant RectTerm                \ Flag for Rectangular Hilight Clipboard
create MSPCS  LMAXSTRING allot          \ create a large array of spaces
mspcs lmaxstring blank


\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ 2     Brad Eckert's colorization support                              {BE}
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

create  keycolors
                black ,    red ,       green ,    yellow ,
                blue ,     magenta ,   cyan ,     ltgray ,
                dkgray ,   ltred ,     ltgreen ,  brown ,
                ltblue ,   ltmagenta , ltcyan ,   gray ,

\ -rbs temp fix below
0 value keyfileName$
CREATE keyfileName1$ 260 allot

        \ init now..
        keyfileName1$ to keyfileName$
        s" COLORIZE.F" Prepend<home>\ keyfileName$ place

: keyfileName   ( -- a n )
                keyfileName$ count ;
\ <----rbs temp fix above

create tempkey MAXSTRING chars allot    \ used by iskey?
1024    value maxkeys
  16    value maxkeylen      \ keys are really variable length, might be typical
   0    value keyhere
   0    value keyfile        \ file handle for color file
   0    value ASMoptions

variable OVERSTRIKE  ( insert )     \ toggle with insert key

maxkeys 1+ cells    Pointer keywords          \ sorted list of links to keywords
maxkeys maxkeylen * Pointer keyword-strings   \ space for the strings

: 'keyword      ( n -- addr )   \ address a keyword link
        1+ cells keywords + ;

: keyc,         ( c -- )        \ lay a character into the keyword-strings area
        keyhere c! [ 1 chars ] literal +to keyhere ;

: kparse        ( a n char -- a' n' a len ) \ parse out next XXXX___
        -rot 2dup 2>r rot dup>r scan r> skip
        2r> 2 pick - ;

: ParseNum      ( a n -- a' n' number )
        \ parse number from a string, return 0 if not a number
        bl kparse -trailing number? nip 0<> and ;

CODE 16bit+     ( a1 -- a1+2 )          \ same as 2 chars +
        add     ebx, # 2
        next    c;

: open-keywords ( a n -- )             \ load keywords file
\ Opens the keyword list and loads it from a text file.  Each line contains:
\ COLOR ACTION NAME where color=0..255, action=0..255, name is the string name
    keywords off
    0 to ASMoptions
    keyword-strings to keyhere
    r/o open-file 0=
    IF  to keyfile
        BEGIN
            pad 80 keyfile read-line
            IF      2drop EXIT
            THEN
            keywords @ maxkeys u< AND           \ maxxed out link table
            keyhere keywords -                  \ length of keyword strings
            80 +                                \ could add 80 more characters
            maxkeys maxkeylen * u< AND          \ less than allowed length?
        WHILE
            pad swap                             ( addr len )
            ParseNum dup
            IF  CASE
                    1000 OF  2drop 1 to ASMoptions   \ enable ASM options
                    ENDOF
                    1001 OF  2drop 2 to ASMoptions
                    ENDOF
                    keyhere >r  keyc,  ParseNum keyc,
                    bl kparse 2swap 2drop -trailing
                    dup keyc,                       \ save length of string
                    bounds
                    ?DO     i c@ keyc,
                    LOOP         \ save number and string
                    r> keywords @ dup>r 'keyword !  \ place link at end of table
                    r@
                    BEGIN
                        ?dup                      \ bubble up so table is sorted
                    WHILE
                        dup 1- 'keyword           \ cnt idx
                        dup 2@ >r 16bit+ count r>
                        16bit+ count  compare 0<
                        IF  dup>r 2@ swap r> 2!   \ swap indicies
                        ELSE
                            drop                  \ finishes with sort
                            drop 1                \ bubbling is finished
                        THEN
                        1-
                    REPEAT  r> 1+ keywords !
                0 ENDCASE
            ELSE 3drop                            \ 1st wasn't a number
            THEN
        REPEAT  drop
        keyfile close-file drop
    ELSE    drop
    THEN    ;

: close-keywords ( -- )
\ Releases memory used by keyword colorization
\                keyword-strings release     \ Is this in use ????
\                keywords release            \ Will this be in use later?
;

: default-keywords ( -- ) keyfileName open-keywords ;

initialization-chain chain-add default-keywords \ open keyword file upon startup
unload-chain chain-add-before close-keywords    \ make sure memory is released


\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ 3     Multiple File Support Data Structure
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

128           constant entry-max       \ maximum number of files in file stack
entry-max 2 + constant entry-console   \ the console is always in the same place
                                       \ always two above the
entry-max 3 + constant entry-max-items \ last, defines the stack size in items

0             value    entry#          \ the internal entry in the file stack
0             value    entry-#bytes    \ adjusted to the size of a stack entry
0             Pointer  entry-buffer    \ the pointer to the start of file stack

: entry-size    ( -- n1 )              \ total size of all file stack entries
                entry-max-items entry-#bytes * ;

cfa-code $entry@
                push    ebx
                mov     ecx, 4 [eax] [edi]              \ @
                mov     eax, &of entry# [edi]           \ entry#
                mov     ebx, &of entry-#bytes [edi]     \ entry-#bytes
                push    edx                             \ save UP
                mul     ebx                             \ *
                mov     ebx, eax                        \
                add     ebx, ecx                        \ +
                add     ebx, &of entry-buffer [edi]     \ entry-buffer +
                pop     edx                             \ restore UP
                next
                END-CODE

\ cfa-func  entry@             entry-translate  @ ;

cfa-code entry@
                push    ebx
                mov     ecx, 4 [eax] [edi]              \ @
                mov     eax, &of entry# [edi]           \ entry#
                mov     ebx, &of entry-#bytes [edi]     \ entry-#bytes
                push    edx                             \ save UP
                mul     ebx                             \ *
                mov     ebx, eax                        \
                add     ebx, ecx                        \ +
                add     ebx, &of entry-buffer [edi]     \ entry-buffer +
                pop     edx                             \ restore UP
                mov     ebx, 0 [ebx] [edi]              \ @
                next
                END-CODE

\ cfa-func  entry!   2 cells - entry-translate  ! ;

cfa-code entry!
                push    ebx
                mov     ecx, -4 [eax] [edi]             \ 2 cells - @
                mov     eax, &of entry# [edi]           \ entry#
                mov     ebx, &of entry-#bytes [edi]     \ entry-#bytes
                push    edx                             \ save UP
                mul     ebx                             \ *
                mov     ebx, eax                        \
                add     ebx, ecx                        \ +
                add     ebx, &of entry-buffer [edi]     \ entry-buffer +
                pop     edx                             \ restore UP
                pop     eax
                mov     0 [ebx] [edi], eax              \ !
                pop     ebx
                next
                END-CODE

\ cfa-func  entry+!   3 cells - entry-translate  +! ;

cfa-code entry+!
                push    ebx
                mov     ecx, -8 [eax] [edi]             \ 3 cells - @
                mov     eax, &of entry# [edi]           \ entry#
                mov     ebx, &of entry-#bytes [edi]     \ entry-#bytes
                push    edx                             \ save UP
                mul     ebx                             \ *
                mov     ebx, eax                        \
                add     ebx, ecx                        \ +
                add     ebx, &of entry-buffer [edi]     \ entry-buffer +
                pop     edx                             \ restore UP
                pop     eax
                add     0 [ebx] [edi], eax              \ +!
                pop     ebx
                next
                END-CODE

variable entry-init-link
         entry-init-link off

: entry-init+   ( n1 -- )
                entry-init-link link,
                entry-#bytes , , ;

: entry+        ( size -- offset )
                entry-#bytes
                swap +to entry-#bytes ;

: entry         ( -<name>- )           \ un-initialized CELL entry
                header  entry@  , CELL entry+ , entry! , entry+! ,
                entry-size Sizeof!> entry-buffer ;

: #entry        ( n1 -<name>- )        \ initialized CELL entry
                entry-init+
                header  entry@  , CELL entry+ , entry! , entry+! ,
                entry-size Sizeof!> entry-buffer ;

: $entry        ( -<name>- )           \ string entry, MAXSTRING bytes long
                header  $entry@ , MAXSTRING entry+ , -1 , -1 ,
                entry-size Sizeof!> entry-buffer  ;

: n$entry       ( n1 -<name>- )        \ string entry, 'n1' bytes long
        header  $entry@ , entry+ , -1 , -1 ,
        entry-size Sizeof!> entry-buffer  ;

              entry edit-changed?
              entry file-lines     \ the number of lines in the file
              entry num-pages      \ the number of pages        rls - page
              entry browse?        \ are we in browse mode, don't allow changes?
              entry from-web?      \ was this page read from web?

        1000 #entry max-lines      \ initial maximum number of lines
         200 #entry max-pages      \ initial maximum number of pages  rls - page
         512 #entry max-cols       \ maximum width of text currently editing
          84 #entry screen-cols    \ default rows and columns at startup
          66 #entry screen-rows
          14 #entry window-lmargin \ graphics pixels of the window left margin
           0 #entry right-edge     \ default right margin
           0 #entry lend-char      \ value of first line end character
           2 #entry lend-len       \ length of line end sequence 0=binary/block
           0 #entry imageDC        \ image DC for bitmap files
           0 #entry vga-bitmap     \ the bitmap of the image file
           0 #entry bitImage?      \ is this a bitmapped file
              entry line-tbl       \ address of the line pointer table
              entry page-tbl       \ address of page table      rls - page
              entry text-ptr       \ address of text buffer
              entry text-blen      \ total text buffer length
   LMAXSTRING n$entry URL$         \ a place to save the URL

entry-#bytes CONSTANT mirror#      \ number of bytes to mirror

\ These following fields are not mirrored between multiple copies of a file.

                    entry line-cur       \ the current top screen line
                    entry page-cur       \ the current page   rls - page
                    entry  col-cur       \ the current left column

                    entry cursor-col     \ absolute column on current line
                    entry cursor-line    \ absolute line in current file

                    entry hlst           \ highlight line start
                    entry hled           \ highlight line end
                    entry hcst           \ highlight column start
                    entry hced           \ highlight column end
                    entry mlst
                    entry mcst

                   $entry cur-filename
       LMAXSTRING n$entry cur-buf       \ 1k line buffer

: entries-init  ( -- )                  \ first NULL out the entire buffer
                entry-buffer entry-size erase
                                        \ then initialize any non-0 entries
                entry-init-link
                BEGIN   @ ?dup
                WHILE   dup cell+ @ over 2 cells+ @ swap
                        entry-max-items 0
                        DO      2dup i entry-#bytes * entry-buffer + + !
                        LOOP    2drop
                REPEAT  ;

entries-init    \ initialize the buffer now so we can use it

initialization-chain chain-add entries-init

FALSE value editAsBinary?       \ should we edit this file as a binary file?

  16 CONSTANT BLOCKLINE-SIZE    \ the length of each line of a binary/block file
  16 CONSTANT BLOCK-LINES       \ lines in a block
1024 CONSTANT BLOCK-SIZE        \ a Forth block size

   0 value WinEdWindow          \ for forward references
   0 value WinEdToolbar         \ allow forward references to Toolbar
   0 value ConsoleWindow        \ the console window object

  80 value printed-columns      \ printed columns on paper
  12 value find-top-margin      \ lines from top where to display found text

0 value def-right-edge          \ default right edge for word wrap

  24 value low-right-edge       \ don't allow the right margin to be too low

10000 value start-text-size     \ initial text buffer size in bytes

 640 value start-width
 480 value start-height
 400 value drag-barV
  60 value drag-barH
   4 value drag-thick
   5 value edit-min             \ minimum with (pixels) of edit window
 240 value console-height
  26 value listHeight           \ height of the SubjectList control
  26 value listHeightDefault    \ default list height

   0 value using-Win32s?        \ are we running under Win32s?
   0 value using98/NT?          \ are we running Windows98 or WindowsNT?
 500 value console-savelines    \ number of console lines to save
   0 value first-line-saved     \ first line of file to be saved

FALSE value show-console?       \ should the Forth console window be displayed?
TRUE  value recent-files?       \ should a list of recent files be kept in the
                                \ Files menu if it doesn't already exist
    0 value search-hndl         \ handle of file we are searching
    0 value findComboEdit
      defer addFileMenu

 TRUE value colorize?           \ {BE}
 TRUE value page-lines?         \ should page markers be displayed?
FALSE value auto-new?           \ automatically make a new file
 TRUE value auto-margin?
FALSE value mousedown?
FALSE value focusing?           \ we are just setting the focus to a window
FALSE value minimized?
 TRUE value tool-bar?           \ is the toolbar turned on?
FALSE value floating-bar?       \ is the toolbar floating?
FALSE value min-tool-bar?       \ are we using the minimum toolbar?
FALSE value save-find?          \ preserve the find buffer across edit sessions?
FALSE value WinEd-web?          \ use WinEd for Web page browsing when TRUE
FALSE value save-margin?
FALSE value sub-dirs?
FALSE value all-occur?          \ find all occurances of a string in a file,
                                \ not just first
FALSE value save-minutes        \ automatically save changes after n minutes
FALSE value open-previous?      \ should we open the file we had open previously
FALSE value term-canceled?      \ did we cancel program termination
FALSE value second-copy?        \ am I the second editor copy to load
FALSE value as-pc?              \ save file as a PC file?
FALSE value start-browse?       \ are we starting in browse mode?
FALSE value interpreting?       \ is Forth console intepreting a line?
    0 value max-toolbar         \ address of the Max toolbar
    0 value displayingLine

9001 CONSTANT SAVE_TIMER        \ a marker for the auto save timer

named-new$ StatusString         \ a place to hold the status

named-new$ command-args         \ commandline arguments

0 value FilesList
0 value FoundList
0 value DocWindow
0 value EditWindow

create unnamed-file ," <UNNAMED FILE>"

: unnamed-file? { nameadr \ file$ -- f1 }
        max-path localAlloc: file$              \ a temporary buffer
        nameadr unnamed-file c@ file$ place     \ lay in filename
        file$ count upper                       \ make it uppercase
        file$ count                             \ compare against UNNAMED-FILE
        unnamed-file count compare 0= ;

: last-entry#   ( -- n1 )
        entry# >r
        0 entry-max 0
        DO      i max
                i to entry#             \ select the hyper file index
                cur-filename c@ 0= ?leave
        LOOP
        r> to entry#  ;

: edit-top      ( -- n1 )
        tool-bar?
        IF      StartSize: WinEdToolbar nip
        ELSE    0
        THEN    ;

: StartUpForth  { \ file$ -- }
        MAXSTRING LocalAlloc: file$
        &prognam count "path-only" file$ place
                                   file$ ?+\
        s" Win32For.exe"           file$ +place
                                   file$ +NULL
                                   file$ 1+ zEXEC ;

: highlight-cursor ( -- )
        cursor-line dup to hlst dup to hled to mlst
        cursor-col  dup to hcst dup to hced to mcst ;

Font vFont                      \ define a fonts for WinEd to use
Font uFont
Font iFont
Font bFont
Font xFont

create font-list vFont , uFont , iFont , bFont , xFont ,
5 constant #fonts

Font SFont

defer ?wrap-word
defer back-delete-character
defer save-text
defer save-bitmap-as
defer close-text
defer ?save-text
defer "+open-text
defer before-bye
defer open-previous

: ?line-tbl-ok  ( -- )
                line-tbl ?EXIT
                TRUE s" Line-TBL not initialized" ?terminatebox ;

: end.addr      ( -- a1 )
                ?line-tbl-ok
                line-tbl file-lines cells+ @ ;

: #line.addr    ( n1 -- a1 )
                ?line-tbl-ok
                line-tbl swap 0max file-lines min cells+ @ ;

: #line.bytes   ( n1 -- n2 )            \ the real line length, including CRLF
                line-tbl
                IF      line-tbl swap 0max file-lines 1- min cells+ 2@ -
                ELSE    drop 0
                THEN    ;

: #line"        ( n1 -- a1 n2 )       \ get line # n1, return address and length
        dup file-lines >=
        IF      drop                  \ discard requested line number
                end.addr 0
        ELSE    line-tbl swap 0max cells+ 2@ tuck - 0max
                lend-len
                IF      2dup + lend-len - c@ lend-char =
                        IF      lend-len - 0max
                        THEN
                THEN
        THEN    ;

\ rls January 11th, 2001 - 23:19      \ Used in Rectangular Paste
: #line+"  ( n1 offset -- a1 n2 )     \ get line # n1, return address and length
        over file-lines >=
        IF      2drop                 \ discard requested line number
                end.addr 0
        ELSE    >r line-tbl swap 0max cells+ 2@ r@ + tuck
                swap r> + swap - 0max
                lend-len
                IF      2dup + lend-len - c@ lend-char =
                        IF      lend-len - 0max
                        THEN
                THEN
        THEN    ;

: #line.len     ( n1 -- n2 )            \ the line length without CRLF
        line-tbl
        IF      #line" nip
        ELSE    drop 0
        THEN    ;

: text-length   ( -- n1 )       \ total text length in buffer
        line-tbl
        IF      file-lines #line.addr text-ptr -
        ELSE    0
        THEN    ;

: LastPage   ( -- n )           \ rls February 3rd, 2002 - 10:00
        file-lines 1- PRINTER-ROWS / 1+ ;

0 value warned?

defer warn-to-save
defer must-save
defer primitive-save-text

: #line!        { caddr clen cline \ cbuf$ cdiff -- }
      LMAXSTRING localAlloc: cbuf$
      cbuf$ LMAXSTRING blank                  \ prefill with blanks
      caddr clen  cbuf$  LPLACE               \ save string in a temp
      lend-len 2 =                            \ two char line terminator?
      IF    crlf$ count cbuf$ +LPLACE         \ then append CRLF
      ELSE  lend-len 1 =                      \ are we using a line terminator?
            IF    lend-char cbuf$ C+LPLACE    \ append single line terminator
            ELSE                              \ otherwise no line terminator,
                  BLOCKLINE-SIZE cbuf$ !      \ set length to 64
            THEN
      THEN
      cline #line.bytes cbuf$ @ - to cdiff    \ difference in lengths
      cline 1+ #line.addr                     \ source
      dup cdiff -                             \ destination
      end.addr cline 1+ #line.addr - move     \ move rest of doc
      cdiff negate to cdiff
      file-lines 2 + cline 1+
      ?DO     cdiff line-tbl i cells+ +!
      LOOP
      cbuf$ LCOUNT cline #line.addr swap move     \ move the line into buffer
      text-length 4000 + text-blen >              \ check for buffer full
      warned? 0= and                              \ and haven't warned used?
      IF    GetStack: DocWindow entry-console =
            IF      TRUE to edit-changed?         \ SET changed flag
                    primitive-save-text
            ELSE    beep
                    IF      beep
                            warn-to-save
                            true to warned?       \ we warned you!
                    THEN
            THEN
      THEN
      text-length 1000 + text-blen >
      IF      must-save
      THEN  ;

: page>line     ( page -- startline )   \ rls - page
        0max file-lines min
        cells page-tbl + @ 0x0FFFFF and ;

\ rls - page
: this-page     { line \ lopage hipage loline hiline page -- page }
        0 to lopage  num-pages to hipage  lopage to page
        BEGIN   lopage page>line to loline
                hipage page>line to hipage
                hipage lopage 1+ >
        WHILE
                hipage lopage - line loline - hiline loline - */mod
                IF      1+      THEN
                +to page
                page page>line line >=
                IF      page to lopage
                ELSE    page to hipage
                THEN
        REPEAT
        page ;

: get-cursor-line ( -- )               \ get the current line from file
        cursor-line #line" "LCLIP" cur-buf LPLACE
        \ Use MAXSTRING minus one since there is a count byte at the
        \ beginning of the buffer that isn't accounted for any other way.
        cur-buf LCOUNT LMAXSTRING CELL-
        swap /string blank ;            \ add trailing blanks

: put-cursor-line ( -- )
                cur-buf LCOUNT cursor-line #line! ;

: -trailing-blanks ( -- )
                cursor-line #line" 2dup -trailing nip - nip
                IF      get-cursor-line
                        cur-buf LCOUNT -trailing nip cur-buf !
                        put-cursor-line
                THEN    ;

: set-mirrored  { m-entry# -- } \ if n1<>-1 then mark current as mirrored
        m-entry# -1 <>
        IF      text-ptr ?dup IF release 0 to text-ptr THEN
                line-tbl ?dup IF release 0 to line-tbl THEN
                m-entry# entry-#bytes * entry-buffer +
                  entry# entry-#bytes * entry-buffer +
                entry-#bytes move       \ move master to mirror
        THEN    ;

: update-mirrors { \ open$ -- }                 \ make mirrors have same flags
        entry# entry-max > ?EXIT
        MAXSTRING LocalAlloc: open$
        cur-filename count open$ place
        entry#
        entry-max 0
        DO      i to entry#
                dup i <>                        \ not myself
                                                \ and matching filename
                cur-filename count open$ count caps-compare 0= and
                cur-filename c@ 0<> and
                IF      dup entry-#bytes * entry-buffer +
                        i   entry-#bytes * entry-buffer +
                        mirror# move            \ move master to mirror
                        file-lines cursor-line <
                        \ if other copies are beyond end of file
                        IF      file-lines 1- 0max to cursor-line
                                cursor-line Height: DocWindow
                                CharHeight: DocWindow / - 1+ to line-cur
                        THEN
                THEN
        LOOP
        to entry# ;  \ f1 = -1 if not found, else ENTRY# if found

: update-mirror-browse { \ open$ browse?? edit-changed?? -- }
\ make mirror browse flags the same
                entry# entry-max > ?EXIT
                MAXSTRING LocalAlloc: open$
                cur-filename count open$ place
                entry#
                browse? to browse??
                edit-changed? to edit-changed??
                entry-max 0
                DO      i to entry#             \ select the hyper file index
                                                \ if they match
                        dup i <>                \ not myself
                                                \ and matching filename
                        cur-filename count open$ count caps-compare 0= and
                        cur-filename c@ 0<> and
                        IF      edit-changed?? to edit-changed?
                                browse?? to browse?
                        THEN
                LOOP
                to entry# ;  \ f1 = -1 if not found, else ENTRY# if found

: "already-open# { adr len \ open$ -- n1 } \  return number of times it's open
                len 0=
                IF      0 EXIT                  \ leave if null filename
                THEN
                MAXSTRING LocalAlloc: open$
                adr len open$ place
                open$ count "path-file drop open$ place
                open$ ?defext
                0                               \ non open with this name
                entry# >r
                entry-max 0
                DO      i to entry#             \ select the hyper file index
                                                \ if they match
                        cur-filename count open$ count caps-compare 0=
                        cur-filename c@ 0<> and
                        IF      1+
                        THEN
                LOOP
                r> to entry# ;  \ f1 = -1 if not found, else ENTRY# if found

: sync-mirrors  { \ open$ -- } \ copy new pointers to mirrors
                entry# entry-max > ?EXIT
                MAXSTRING LocalAlloc: open$
                cur-filename count open$ place
                entry#
                entry-max 0
                DO      i to entry#             \ select the hyper file index
                                                \ if they match
                        dup i <>                \ not myself
                                                \ and matching filename
                        cur-filename count open$ count caps-compare 0= and
                        cur-filename c@ 0<> and
                        IF      dup entry-#bytes * entry-buffer +
                                i   entry-#bytes * entry-buffer +
                                mirror# move    \ move master to mirror
                        THEN
                LOOP
                to entry# ;

: file-has-changed ( -- )
                save-minutes 0>
                IF      NULL save-minutes 60000 * SAVE_TIMER
                        Gethandle: WinEdWindow Call SetTimer drop
                THEN
                edit-changed? 0=                \ -- f1 ; not already changed?
                true to edit-changed?           \ mark as changed
                IF                              \ f1 -- ; then update mirrors
                        update-mirrors
                THEN
                EditMode: WinEdToolbar ;

: beeper        ( -- )
                700 50 tone ;

' beeper is beep

: ?beep         ( f1 -- )       \ if f1=FALSE, then BEEP
                0=
                IF      beep
                THEN    ;


\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ 4     Multi-Directory File processing
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

FALSE #if       \ use the 'C' version of file and line operations

synonym win-open-file           fopen-file
synonym win-close-file          fclose-file
synonym win-read-line           fread-line
synonym win-reposition-file     freposition-file

#else           \ use the Forth version of file ane line operations

synonym win-open-file           open-file
synonym win-close-file          close-file
synonym win-read-line           read-line
synonym win-reposition-file     reposition-file

#then

create mask-buf here  max-path + ," *.F" 0 , here - allot
       mask-buf value mask-ptr

create name-buf MAXSTRING allot

 2variable mask-source
     defer process-1file                 \ holds function for processing a file
     defer processing-1dir
 ' noop is processing-1dir

0 value search-aborted?

: next-mask"    ( -- a1 n1 )            \ get the next path from dir list
                mask-source 2@ 2dup ';' scan  2dup 1 /string mask-source 2!
                nip - ;

: first-mask"   ( -- a1 n1 )            \ get the first forth directory path
                mask-ptr count mask-source 2!
                next-mask" ;

: process-afile ( adrd adr len -- )     \ search this file for find string
        name-buf  place         \ lay in directory
        11 cells+               \ adrz
        asciiz->asc-len         \ adrz slen -- adr len
        name-buf +place         \ append filename
\ cr name-buf count type
        name-buf count r/o win-open-file 0=
        IF      to search-hndl
                process-1file
                search-hndl win-close-file drop
                0 to search-hndl
        ELSE    drop
        THEN    ;

: "process-mask-directory { adr1 len1 adr2 len2 \ buf -- }
        processing-1dir
        len1
        IF      max-path allocate abort" Memory Allocate error" to buf
                adr2 len2 buf  place
                adr1 len1 buf +place
                buf count find-first-file 0=
                IF      adr2 len2 process-afile
                        BEGIN   find-next-file  0=
                                search-aborted? 0= and
                        WHILE   adr2 len2 process-afile
                        REPEAT  drop
                        find-close drop
                ELSE    drop
                THEN
        buf free drop
        THEN    ;

variable _hdl-dir                       \ hold the find directory handle

: next-sub-dir"  ( -- sub-dir sub-len )   \ "first-sub-dir" must be called first
      BEGIN _win32-find-data rel>abs                \ lpffd - _WIN32_FIND_DIR
            _hdl-dir @                              \ hFindFile
            call FindNextFile                       \ ior -
            IF    _win32-find-data @              \ file_attributes
                  FILE_ATTRIBUTE_DIRECTORY and    \ is the dir bit set
                  IF    _win32-find-data 11 cells+    \ adrz
                        asciiz->asc-len               \ -- adr len
                        2dup s" ."  compare 0= >r
                        2dup s" .." compare 0= r> or  \ ignore '.' or '..'
                  ELSE  0 0 TRUE                      \ try again
                  THEN
            ELSE  0 0 false
            THEN
      WHILE 2drop
      REPEAT  ;

: "first-sub-dir"    { adr len -- sub-adr sub-len }
      adr len asciiz   rel>abs                    \ adrz -
      _win32-find-data rel>abs                    \ lpffd - _WIN32_FIND_DIR
      swap                                        \ lpszSourceFile
      call FindFirstFile                          \ a search handle if O.K.
                                                  \ else INVALID_HANDLE_VALUE
      _hdl-dir !                                  \ store to the search handle
      _hdl-dir @ -1 <>                            \ adrd ior = 0 = success
      IF    _win32-find-data @                    \ file_attributes
            FILE_ATTRIBUTE_DIRECTORY and          \ is the dir bit set
            IF    _win32-find-data 11 cells+
                  asciiz->asc-len         \ -- adr len
                  2dup s" ."  compare 0= >r
                  2dup s" .." compare 0=  r> or   \ ignore either '.' or '..'
                  IF      2drop                   \ discard adr,len
                          next-sub-dir"           \ find a sub directory
                  ELSE                            \ we found a directory
                  THEN
            ELSE  next-sub-dir"                   \ find a sub directory
            THEN
      ELSE  0 0
      THEN    ;

: sub-dir-close ( -- ior )      \ close the _hdl-dir handle
        _hdl-dir @ call FindClose 0= ;          \ ior - 0 = success

: "process-directory { adr len \ buf1 buf2 -- }
    len
    IF
        \ allocate two buffers in case we need are nesting dirs
        max-path 2 * allocate abort" Memory Allocate error"
        dup to buf1 max-path + to buf2          \ init the buffer pointers
        adr len     buf1 place                  \ save the name
                    buf1 ?+\                    \ must end in '\'
        first-mask" buf1 count "process-mask-directory
        BEGIN       next-mask" dup
                    search-aborted? 0= and
        WHILE       buf1 count "process-mask-directory
        REPEAT      2drop
        sub-dirs?                       \ processing sub directories?
        IF  buf1 count buf2  place
            s" *"      buf2 +place
            buf2 count "first-sub-dir" ?dup
            IF
                buf1 count buf2  place      \ init to parent
                           buf2 +place      \ append sub dir
                           buf2 ?+\         \ add '\' if needed
                _hdl-dir @ >r               \ save before recursing
                buf2 count RECURSE          \ recursively repeat
                r> _hdl-dir !               \ restore from recursion
                BEGIN   next-sub-dir" dup
                        search-aborted? 0= and
                WHILE
                        buf1 count buf2 place   \ init to parent
                        buf2 +place             \ append sub dir
                        buf2 ?+\                \ add '\' if needed
                        _hdl-dir @ >r           \ save before recursing
                        buf2 count RECURSE      \ recursively repeat
                        r> _hdl-dir !           \ restore from recursion
                REPEAT  2drop
                sub-dir-close drop              \ close dir find
            ELSE
                drop
            THEN
        THEN
        buf1 free drop
    THEN ;

: do-files-process ( -- )
                first-path" "process-directory
                BEGIN   next-path" dup
                        search-aborted? 0= and
                WHILE   "process-directory
                REPEAT  2drop  ;


\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ 5     Drawing into the Image DC 
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

: moveto        ( x y -- )
                imageDC MoveTo: [ ] ;

: lineto        ( x y -- )
                imageDC LineTo: [ ] ;

: line          ( x1 y1 x2 y2 -- )
                2swap moveto lineto ;

: line-color    ( color_object -- )
                ?ColorCheck
                imageDC LineColor: [ ] ;


\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ 6     Define the EDIT-WINDOW object   
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

                     0 value textBG
                     0 value textFG
    0   0   0 rgb  new-color highBG
  255 255 255 rgb  new-color highFG
  Color: WHITE     new-color normalBG
  Color: BLACK     new-color normalFG
  192 225 255 rgb  new-color marginColor
  Color: LTGRAY    new-color linesColor
  Color: BLACK     new-color binaryColor
( BLUE ) BROWN     value BmarginColor       \ browse margin color

defer trailBG
' normalBG is trailBG
defer selectBG
' normalBG is selectBG

: highlighting? ( -- f1 )
                hcst hlst hced hled d= 0= ;     \ if start and end are same

: increment/    ( n1 -- n2 )            \ adjust for very large file
                file-lines 65536 >                      \ if MANY lines, then
                IF      file-lines 65536 / 1+ /         \ divide scroll lines
                THEN    ;

0 value the-width
0 value the-height
0 value html-link?
0 value on-text?

0 value saved-depth

create tempLine MAXSTRING allot

WinDC screenDC          \ The screen's device context

\ is string a2,n2 at the start of string a1,n1?  If it is, then remove n2
\ leading characters from a1,n1 returning the result as a3,n3 and a TRUE flag.
\ else return a1,n1 unmodified as a3,n3 and return a FALSE flag.

: HTML?         ( a1 n1 a2 n2 -- a3 n3 f1 )
                2>r over 2r@ tuck caps-compare 0=       \ a2,n2 starts a1,n1?
                IF      2r@ nip /string                 \ then remove n2 chars
                        TRUE                            \ return true
                ELSE    FALSE                           \ else return false
                THEN    2r> 2drop ;                     \ cleanup return stack


\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ 7     Simple Statusbar Class
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

:Class Statusbar  <Super Child-Window

:M Start:       ( hParent -- )   \ creates an empty statusbar in parent window
                to Parent
                create-child-window to hWnd
                SW_SHOWNORMAL Show: self
                ;M

:M ClassInit:   ( -- )   \ initialize class
                ClassInit: super
                s" msctls_statusbar32" WindowClassName place
                ;M

;Class

Statusbar StatBar
variable colon-here?  \ July 4th, 2002 - 18:17 RDA JAP

:Class EditWindowClass <super child-window

        int xpixel
        int ypixel
        int refreshing?
        int TopOfWindow
        int Stack#
        int built-cursor?
        int alreadyPainting
        int paintAgain

MAXSTRING bytes update-flags  \ bytes to mark lines that need updating

Rectangle ClientRect

Record: LPWinScrollInfo
        int cbSize
        int fMask
        int nMin
        int nMax
        int nPage
        int nPos
        int nTrackPos
;RecordSize: sizeof(LPWinScrollInfo)

:M ClassInit:   ( -- )
        ClassInit: super
        sizeof(LPWinScrollInfo) to cbSize       \ init structure to right size
        0 to TopOfWindow
        0 to Stack#
        0 to built-cursor?
        TRUE to refreshing?
        FALSE to alreadyPainting
        FALSE to paintAgain
        #fonts 0
        DO      font-list i cells+ @ >r
                 8                Width: [ r@ ]
                14               Height: [ r@ ]
                s" Fixedsys" SetFaceName: [ r> ]  \ default to Courier
        LOOP
        ;M

:M SetStack:    ( n1 -- )
                dup to Stack#
                    to entry#
                ;M

:M GetStack:    ( -- n1 )
                Stack#
                ;M

:M CharHeight:  ( -- n1 )  \ return the font character height
                the-height
                ;M

:M CharWidth:   ( -- n1 )
                the-width
                ;M

0 value ignoring?       \ TRUE if we are ignoring text till '>' encountered
0 value linking?        \ TRUE if we are displaying a link
0 value ruling?         \ TRUE if we are drawing a Horizontal Rule
0 value HRSize          \ Horizontal Rule height in pixels

: "IgnoreTo     ( a1 n1 -- )             \ ignore text till a1,n1 string found
                dup >r caps-search       \ ONLY on same line though
                IF      r@ /string       \ strip off found string
                THEN    r>drop ;

\ ------------------------------------------------------------------------------
\ Colorization stuff {BE}

: ISKEY?        ( a n -- action color T | F )  { \ maxkey -- }
\ Returns the color of the string and a flag: FALSE if not found.
                -trailing tempkey place  tempkey uppercase drop
                keywords @ 1- to maxkey
                0 maxkey
                BEGIN   ?dup
                WHILE   2/ 1 under+
                REPEAT                  \ position of MSB?
                1 swap lshift
                dup 2/                          \ binary search of keyword list
        BEGIN   over                            ( size index . )
        WHILE   >r 2/ r>
                dup 'keyword @ char+ char+ count
                tempkey count compare  dup
                IF   0< IF      over + maxkey min
                        ELSE    over - 0max
                        THEN
                ELSE    drop nip 'keyword @ count swap c@
                        TRUE
                        EXIT                    \ found
                THEN
        REPEAT
        2drop  false ;                          \ not found

: ishex?        ( a n -- f )
                BASE @ >r hex number? r> BASE ! nip nip ;

: ISNUMBER?     ( a n -- f )
        -trailing 2dup number? nip nip
        IF      2drop TRUE EXIT THEN            \ decimal number
        dup 3 =
        IF      over count [char] ' = swap      ( a n f a' )
                char+ c@   [char] ' = and
                IF 2drop TRUE EXIT THEN         \ '?' literal
        THEN
        2dup >float
        IF      fdrop 2drop TRUE EXIT THEN      \ floating point
        over c@ '$' =  ASMoptions and
        IF      1 /string ishex? EXIT THEN      \ $hex
        over 2 s" 0x" compare 0=
        IF      2 /string ishex? EXIT THEN      \ 0xhex
        2drop 0 ;

: typestr       ( a n -- )
        xpixel ypixel 2over TextOut: dc         \ output the text
        GetTextExtent: dc drop +to xpixel ;     \ and adjust X pixel counter

: ColorWord     ( n -- )                \ set color based on 4-bit value
        0x0F and cells keycolors + @  SetTextColor: dc ;

: Forth-Source? ( -- flag )                      \ Is current file *.F ?
        cur-filename count '.' scan 2 =
        IF      1+ c@ toupper 'F' =
        ELSE    drop false
        THEN ;

: colorable?    ( -- f )
        Forth-Source?
        normalFG textFG = and
        colorize? and                           \ ok to customize?
        keywords @ 0<> and                      \ we have a table
        lend-len 0> and ;                       \ and not in binary mode

0 value comment?

: endface  ( rda )                       \ jp May 12th, 2002 - 10:57
        Handle: vFont SetFont: dc ;      \ set the font to be used

\ new one
: (typetext)    ( addr len -- ) \ rda made a new version
                 colorable?
         if      begin   dup                             \ this is text
                 while   bl kparse                       ( a' n' a n )
                         2dup iskey?
                         if comment? ?dup if swap drop then
                            over dup 10 = swap 12 = or
                            if dup to comment? then
                            ColorWord
case    1 of                     endof  \ no action
2 of typestr 0 0 2swap   endof  \ EOL comment
3 of typestr ')' kparse  endof  \ string)
4 of typestr '"' kparse  endof  \ string"
5 of typestr '}' kparse  endof  \ string}
6 of typestr ']' kparse  endof  \ string]
7 of typestr '>' kparse  endof  \ string>
8 of typestr bl  kparse 0 ColorWord endof
9 of typestr bl  kparse  endof  \ pair
   11 of false to comment?   endof
endcase
                         else  2dup isnumber? comment? 0= and
                               if RED SetTextColor: dc
                               then
                         then
                         typestr comment? 0=
                         if BLACK SetTextColor: dc ( RDA )
                         then
                 repeat  2drop
         else    typestr                       \ type whole string
         then    ;


: typecomment   ( a n -- )
        GREEN SetTextColor: dc                \ display text in green
        typestr
        BLACK SetTextColor: dc ;

: typetext      ( a n -- )
        colorable? ASMoptions and             \ ASM colorization options?
        IF      2dup ';' scan dup             ( a n a' n' . )
                IF      tuck 2>r - (typetext) \ type the non-comment part
                        2r> typecomment exit  \ type the comment part
                ELSE    2drop
                THEN
                ASMoptions 1 >
                IF      over c@ '*' =
                        IF  typecomment exit
                        THEN
                THEN
        THEN    (typetext) ;

\ --------------------------------------------------------------------------

: "PutTextOut   ( a1 n1 -- )
        ignoring?                               \ are we still discarding HTML
        IF      '>' scan dup                    \ discard text upto '>' or end
                IF      1 /string               \ and remove '>' too
                        FALSE to ignoring?      \ and we are done skipping
                ELSE    2drop
                        EXIT
                THEN                            \ still skipping till '>' found
        THEN
        \ Added the following to make viewing binaries more readable,
        \ masks out to blanks
\        lend-len 0=                \ if not a TEXT file, then discard non ASCII
\        IF      tempLine place                 \ save text for processing
\                tempLine count bounds
\                ?DO     i c@ dup 0x7f >        \ Del me please ????
\                        IF      drop BL        \ If not in use ????
\                        THEN    BL max i c!
\                LOOP
\                tempLine count
\        THEN
        lend-len 0=                 \ if not a TEXT file, then discard non ASCII
        IF      dup
                IF      base @ >r hex
                        tempLine off                   \ clear the temp buffer
                        displayingLine BLOCKLINE-SIZE *
                        0 <# # # # # BL hold # # # # #>
                        tempLine +place                \ 9 wide
                        s"   | "
                        tempLine +place                \ 4 wide
                        2dup bounds
                        ?DO     i c@ 0 <# # # BL hold #>
                                tempLine +place        \ 3 * BLOCKLINE-SIZE wide
                        LOOP
                        SPCS BLOCKLINE-SIZE 3 * 14 + tempLine c@ - 0MAX
                        tempLine +place
                        tempLine +place
                        tempLine count
                        r> base !
                THEN
        THEN
        browse?
        IF      \ expand TABs if we are browsing
                BEGIN   dup                      \ while there is text remaining
                WHILE   2dup K_TAB scan          \ scan for TAB
                        2dup 2>r nip -
                        typetext \ {BE}
                        2r> over c@ K_TAB =      \ if we found a TAB
                        IF                       \ then calc pixels by TAB width
                                tab-size xpixel the-width
                                / tab-size 1 max mod -
                                the-width * +to xpixel  \ expand by a tab width
                                1 /string        \ remove TAB from string
                        THEN
                REPEAT  2drop
        ELSE    typetext \ {BE}
        THEN    ;

: IgnoreTo'>'   ( -- )
                TRUE to ignoring? ;

: Get#To'>'     ( a1 n1 -- a2 n2 n3 )
        2dup '>' scan                   \ find '>'
        2dup 1 /string 2>r              \ skip '>' and save following string
        nip - number? 2drop             \ extracted number string
        2r> rot ;

VARIABLE #lit

: TextTo';'     ( a1 n1 -- a2 n2 )
        2dup ';' scan                   \ find ';'
        2dup 1 /string 2>r              \ skip ';' and save following string
        nip - number? 2drop             \ extracted number string
        #lit c! #lit 1 "PutTextOut      \ output as a literal character
        2r> ;

: HorizontalRule ( -- )                 \ put out a line on screen
        TRUE to ruling? ;

: Underline     ( -- )
        Handle: uFont 0=
        IF     TRUE Underline: uFont
                       Create: uFont
        THEN
        Handle: uFont SetFont: dc ;      \ set the font to be used

: /Underline    ( -- )
        Handle: vFont SetFont: dc ;      \ set the font to be used

: Italics       ( -- )
        Handle: iFont 0=
        IF       TRUE Italic: iFont
                       Create: iFont
        THEN
        Handle: iFont SetFont: dc ;      \ set the font to be used

: /Italics      ( -- )
        Handle: vFont SetFont: dc ;      \ set the font to be used

: Bold  ( -- )
        Handle: bFont 0=
        IF    FW_BOLD Weight: bFont
                       Create: bFont
        THEN
        Handle: bFont SetFont: dc ;      \ set the font to be used

: /Bold         ( -- )
        Handle: vFont SetFont: dc ;      \ set the font to be used

: StrikeOut     ( -- )
        Handle: xFont 0=
        IF    TRUE StrikeOut: xFont
                       Create: xFont
        THEN               
        Handle: xFont SetFont: dc ;      \ set the font to be used

: /StrikeOut    ( -- )
        Handle: vFont SetFont: dc ;      \ set the font to be used

: StartLink     ( -- )
        linking? 0=
        IF      Underline
                LTBLUE SetTextColor: dc
                TRUE to linking?
        THEN    ;

: EndLink       ( -- )
        linking?
        IF      BLACK SetTextColor: dc
                /Underline
                FALSE to linking?
        THEN    ;

\ Examples of Links:
\   Read the <A HREF="file:disclaim.html">Disclaimer</A>.
\   Send a message to PSE with <A HREF="mailto:PSE@ix.netcom.com">suggestions</A>.


\ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
\ 8 ++++++++++++++++++++ HTML statment processors ++++++++++++++++++++++  
\ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

new-chain html-chain
new-chain &-chain

: ?HTML-EXIT    ( a1 n1 a2 n2 -- a3 n3 )
        dup 3 pick >                \ if looking for longer string than we have
        IF      2drop
                r>drop
                EXIT
        THEN
        2>r over 2r@ tuck compare 0=     \ a2,n2 starts a1,n1?
        IF      2r> nip /string          \ then remove n2 chars
        ELSE    2r> 2drop
                r>drop                   \ don't execute rest of HTML statment
        THEN    ;

: HTML:         ( "htmlstring" -- )      \ compile time
        ( a1 n1 -- a2 n2 )               \ runtime
        html-chain                       \ link into html-chain
        BEGIN   dup @
        WHILE   @
        REPEAT  here swap ! 0 , here cell+ ,
        docol ,                          \ make a headerless def
        compile (s") ,"text" align       \ compile in html command string
        compile ?HTML-EXIT               \ compile in html test
        !csp ] ;                         \ define a headerless definition

: ?&-EXIT       ( a1 n1 a2 n2 -- a3 n3 )
         dup 3 pick >        \ if looking for longer string than we have
         IF      2drop
                 r>drop
                 EXIT
         THEN
         2>r over 2r@ tuck compare 0=    \ a2,n2 starts a1,n1?
         IF      2r> nip /string         \ then remove n2 chars
         ELSE    2r> 2drop
                 r>drop
         THEN    ;

: &:     ( "&string" -- )                \ compile time
         ( a1 n1 -- a2 n2 )              \ runtime
         &-chain                         \ link into html-chain
         BEGIN   dup @
         WHILE   @
         REPEAT  here swap ! 0 , here cell+ ,
         docol ,                         \ make a headerless def
         compile (s") ,"text" align      \ compile in html command string
         compile ?&-EXIT                 \ compile in html test
         !csp ] ;                        \ define a headerless definition

\ ISO88591.F
\ ISO 8859-1 characters set

&: "&quot;"     s$ '"'  "PutTextOut ;    \ Quotes
&: "&QUOT;"     s$ '"'  "PutTextOut ;    \  idem
&: "&amp;"      s" &"   "PutTextOut ;    \ Ampersand
&: "&AMP;"      s" &"   "PutTextOut ;    \  idem
&: "&LT;"       s" <"   "PutTextOut ;    \ Less Than
&: "&lt;"       s" <"   "PutTextOut ;    \  idem
&: "&GT;"       s" >"   "PutTextOut ;    \ Greater than
&: "&gt;"       s" >"   "PutTextOut ;    \  idem
&: "&nbsp;"     s"  "   "PutTextOut ;    \ Non-breaking space
&: "&iexcl;"    s" ¡"   "PutTextOut ;    \ Inverted exclamation
&: "&cent;"     s" ¢"   "PutTextOut ;    \ Cent sign
&: "&pound;"    s" £"   "PutTextOut ;    \ Pound sterling
&: "&curren;"   s" ¤"   "PutTextOut ;    \ General currency sign
&: "&yen;"      s" ¥"   "PutTextOut ;    \ Yen sign
&: "&brvbar;"   s" ¦"   "PutTextOut ;    \ Broken vertical bar
&: "&sect;"     s" §"   "PutTextOut ;    \ Section sign
&: "&uml;"      s" ¨"   "PutTextOut ;    \ Umlaut - dieresis
&: "&copy;"     s" ©"   "PutTextOut ;    \ Copyright
&: "&ordf;"     s" ª"   "PutTextOut ;    \ Feminine ordinal
&: "&laquo;"    s" «"   "PutTextOut ;    \ Left angle quote, guillemet left
&: "&not;"      s" ¬"   "PutTextOut ;    \ Not sign
&: "&shy;"      s" ­"   "PutTextOut ;    \ Soft hyphen
&: "&reg;"      s" ®"   "PutTextOut ;    \ Registered trademark
&: "&macr;"     s" ¯"   "PutTextOut ;    \ Macron accent
&: "&deg;"      s" °"   "PutTextOut ;    \ Degree sign
&: "&plusmn;"   s" ±"   "PutTextOut ;    \ Plus or minus
&: "&sup2;"     s" ²"   "PutTextOut ;    \ Superscript two
&: "&sup3;"     s" ³"   "PutTextOut ;    \ Superscript three
&: "&acute;"    s" ´"   "PutTextOut ;    \ Acute accent
&: "&micro;"    s" µ"   "PutTextOut ;    \ Micro sign
&: "&para;"     s" ¶"   "PutTextOut ;    \ Paragraph sign
&: "&middot;"   s" ·"   "PutTextOut ;    \ Middle dot
&: "&cedil;"    s" ¸"   "PutTextOut ;    \ Cedilla
&: "&sup1;"     s" ¹"   "PutTextOut ;    \ Superscript one
&: "&ordm;"     s" º"   "PutTextOut ;    \ Masculine ordinal
&: "&raquo;"    s" »"   "PutTextOut ;    \ Right angle quote, guillemotright
&: "&frac14;"   s" ¼"   "PutTextOut ;    \ Fraction one-fourth
&: "&frac12;"   s" ½"   "PutTextOut ;    \ Fraction one-half
&: "&frac34;"   s" ¾"   "PutTextOut ;    \ Fraction three-fourths
&: "&iquest;"   s" ¿"   "PutTextOut ;    \ Inverted question mark
&: "&Agrave;"   s" À"   "PutTextOut ;    \ Capital A, grave accent
&: "&Aacute;"   s" Á"   "PutTextOut ;    \ Capital A, acute accent
&: "&Acirc;"    s" Â"   "PutTextOut ;    \ Capital A, circumflex accent
&: "&Atilde;"   s" Ã"   "PutTextOut ;    \ Capital A, tilde
&: "&Auml;"     s" Ä"   "PutTextOut ;    \ Capital A, dieresis or umlaut mark
&: "&Aring;"    s" Å"   "PutTextOut ;    \ Capital A, ring
&: "&AElig;"    s" Æ"   "PutTextOut ;    \ Capital AE dipthong, ligature
&: "&Ccedil;"   s" Ç"   "PutTextOut ;    \ Capital C, cedilla
&: "&Egrave;"   s" È"   "PutTextOut ;    \ Capital E, grave accent
&: "&Eacute;"   s" É"   "PutTextOut ;    \ Capital E, acute accent
&: "&Ecirc;"    s" Ê"   "PutTextOut ;    \ Capital E, circumflex accent
&: "&Euml;"     s" Ë"   "PutTextOut ;    \ Capital E, dieresis or umlaut mark
&: "&Igrave;"   s" Ì"   "PutTextOut ;    \ Capital I, grave accent
&: "&Iacute;"   s" Í"   "PutTextOut ;    \ Capital I, acute accent
&: "&Icirc;"    s" Î"   "PutTextOut ;    \ Capital I, circumflex accent
&: "&Iuml;"     s" Ï"   "PutTextOut ;    \ Capital I, dieresis or umlaut mark
&: "&ETH;"      s" Ð"   "PutTextOut ;    \ Capital Eth, Icelandic
&: "&Ntilde;"   s" Ñ"   "PutTextOut ;    \ Capital N, tilde
&: "&Ograve;"   s" Ò"   "PutTextOut ;    \ Capital O, grave accent
&: "&Oacute;"   s" Ó"   "PutTextOut ;    \ Capital O, acute accent
&: "&Ocirc;"    s" Ô"   "PutTextOut ;    \ Capital O, circumflex accent
&: "&Otilde;"   s" Õ"   "PutTextOut ;    \ Capital O, tilde accent
&: "&Ouml;"     s" Ö"   "PutTextOut ;    \ Capital O, dieresis or umlaut mark
&: "&times;"    s" ×"   "PutTextOut ;    \ Multiply sign
&: "&Oslash;"   s" Ø"   "PutTextOut ;    \ Capital O, slash
&: "&Ugrave;"   s" Ù"   "PutTextOut ;    \ Capital U, grave accent
&: "&Uacute;"   s" Ú"   "PutTextOut ;    \ Capital U, acute accent
&: "&Ucirc;"    s" Û"   "PutTextOut ;    \ Capital U, circumflex accent
&: "&Uuml;"     s" Ü"   "PutTextOut ;    \ Capital U, dieresis or umlaut mark
&: "&Yacute;"   s" Ý"   "PutTextOut ;    \ Capital Y, acute accent
&: "&THORN;"    s" Þ"   "PutTextOut ;    \ Capital THORN, Icelandic
&: "&szlig;"    s" ß"   "PutTextOut ;    \ Small sharp s, German, sz ligature
&: "&agrave;"   s" à"   "PutTextOut ;    \ Small a, grave accent
&: "&aacute;"   s" á"   "PutTextOut ;    \ Small a, acute accent
&: "&acirc;"    s" â"   "PutTextOut ;    \ Small a, circumflex accent
&: "&atilde;"   s" ã"   "PutTextOut ;    \ Small a, tilde accent
&: "&auml;"     s" ä"   "PutTextOut ;    \ Small a, dieresis or umlaut mark
&: "&aring;"    s" å"   "PutTextOut ;    \ Small a, ring
&: "&aelig;"    s" æ"   "PutTextOut ;    \ Small ae diphthong, ligature
&: "&ccedil;"   s" ç"   "PutTextOut ;    \ Small c, cedilla
&: "&egrave;"   s" è"   "PutTextOut ;    \ Small e, grave accent
&: "&eacute;"   s" é"   "PutTextOut ;    \ Small e, acute accent
&: "&ecirc;"    s" ê"   "PutTextOut ;    \ Small e, circumflex accent
&: "&euml;"     s" ë"   "PutTextOut ;    \ Small e, dieresis or umlaut mark
&: "&igrave;"   s" ì"   "PutTextOut ;    \ Small i, grave accent
&: "&iacute;"   s" í"   "PutTextOut ;    \ Small i, acute accent
&: "&icirc;"    s" î"   "PutTextOut ;    \ Small i, circumflex accent
&: "&iuml;"     s" ï"   "PutTextOut ;    \ Small i, dieresis or umlaut mark
&: "&eth;"      s" ð"   "PutTextOut ;    \ Small eth, Icelandic
&: "&ntilde;"   s" ñ"   "PutTextOut ;    \ Small n, tilde
&: "&ograve;"   s" ò"   "PutTextOut ;    \ Small o, grave accent
&: "&oacute;"   s" ó"   "PutTextOut ;    \ Small o, acute accent
&: "&ocirc;"    s" ô"   "PutTextOut ;    \ Small o, circumflex accent
&: "&otilde;"   s" õ"   "PutTextOut ;    \ Small o, tilde
&: "&ouml;"     s" ö"   "PutTextOut ;    \ Small o, dieresis or umlaut mark
&: "&divide;"   s" ÷"   "PutTextOut ;    \ Division sign
&: "&oslash;"   s" ø"   "PutTextOut ;    \ Small o, slash
&: "&ugrave;"   s" ù"   "PutTextOut ;    \ Small u, grave accent
&: "&uacute;"   s" ú"   "PutTextOut ;    \ Small u, acute accent
&: "&ucirc;"    s" û"   "PutTextOut ;    \ Small u, circumflex accent
&: "&uuml;"     s" ü"   "PutTextOut ;    \ Small u, dieresis or umlaut mark
&: "&yacute;"   s" ý"   "PutTextOut ;    \ Small y, acute accent
&: "&thorn;"    s" þ"   "PutTextOut ;    \ Small thorn, Icelandic
&: "&yuml;"     s" ÿ"   "PutTextOut ;    \ Small y, dieresis or umlaut mark

create trademark-ligature 153 c,
&: "&trade;" trademark-ligature 1 "PutTextOut ;  \ Trademark
&: "&TRADE;" trademark-ligature 1 "PutTextOut ;

&: "&#"         TextTo';' ;             \ handle literal characters

: "Process&"    { adr len \ &$ -- a2 n2 }       \ process one HTML statement
        LMAXSTRING LocalAlloc: &$
        adr len &$ LPLACE               \ save original string away
        &$ LCOUNT
        &-chain do-chain                \ go look for an html command
        over &$ CELL+ =                 \ if we didn't process any,
        IF      2drop                   \ August 4th, 1997 tjz
                                        \ correct for a stack depth problem
                adr 1 "PutTextOut       \ then output 1 original character
                1                       \ one character processed
        ELSE    nip len swap -          \ else calc the processed characters
        THEN    adr len rot 0MAX /string ;   \ remove them from original string

: "&PutTextOut  ( adr len -- )
        BEGIN
                2dup s" &" search
        WHILE
                2swap 2over nip -
                "PutTextOut   \ display leading text
                "Process&"
        REPEAT  2drop
        "PutTextOut ;

\ HTML statements

HTML: "<A HREF" ( a1 n1 -- a2 n2 )         \ an embeded reference
                IgnoreTo'>'                \ ignore text till '>' encountered
                StartLink ;                \ turn on hypertext marker

HTML: "</A>"    ( a1 n1 -- a2 n2 )         \ put out text upto end of link
                EndLink ;

\ The following 3 definitions are NOT Standard:

HTML: "<R>"     RED     SetTextColor: dc ; \ RDA March 16th, 2002
HTML: "<BL>"    BLUE    SetTextColor: dc ; \ RDA March 16th, 2002
HTML: "<BK>"    BLACK   SetTextColor: dc ; \ RDA March 16th, 2002

HTML: "<A NAME"  IgnoreTo'>' ;             \ ignore label during display
HTML: "<ADDRESS>"  ;                       \ ignore Address
HTML: "</ADDRESS>" ;                       \ ignore Address
HTML: "<B>"       Bold ;                   \ turn on  Bold
HTML: "</B>"      /Bold ;                  \ turn off Bold
HTML: "<BIG>"     ;                        \ ignore     Big font
HTML: "</BIG>"    ;                        \ ignore not Big font
HTML: "<BLINK>"   ;                        \ ignore turn on  Blink
HTML: "</BLINK>"  ;                        \ ignore turn off Blink
HTML: "</BLOCKQUOTE>" ;                    \ ignore end   BLOCKQUOTE
HTML: "<BODY>"    ;                        \ ignore start of body
HTML: "</BODY>"   ;                        \ ignore end   of body
HTML: "<BR>"      ;                        \ ignore line break
HTML: "</BUTTON>"   ;                      \ ignore end  button
HTML: "</CAPTION>"  ;                      \ ignore turn off Captioning
HTML: "<CENTER>"  ;                        \ ignore turn on  Centering
HTML: "</CENTER>" ;                        \ ignore turn off Centering
HTML: "<CITE>"    Italics ;                \ turn on  Citation
HTML: "</CITE>"   /Italics ;               \ turn off Citation
HTML: "<CODE>"    ;                        \ ignore CODE
HTML: "</CODE>"   ;                        \ ignore /CODE
HTML: "<DFN>"     ;                        \ ignore start definition
HTML: "<DD>"     ;                         \ ignore ???
HTML: "</DD>"     ;                        \ ignore ???
HTML: "<DT>"     ;                         \ ignore ???
HTML: "</DT>"     ;                        \ ignore ???
HTML: "</DFN>"    ;                        \ ignore end   definition
HTML: "<DIR>"     ;                        \ ignore start directory list
HTML: "</DIR>"    ;                        \ ignore end   directory list
HTML: "<DEL>"     ;                        \ ignore start delete
HTML: "</DEL>"     ;                       \ ignore end   delete
HTML: "<DIV>"     ;                        \ ignore start division
HTML: "</DIV>"    ;                        \ ignore end   division
HTML: "<DL>"      ;                        \ ignore Definition list
HTML: "</DL>"     ;                        \ ignore Definition list
HTML: "<EM>"      Italics ;                \ turn on  Emphasize
HTML: "</EM>"     /Italics ;               \ turn off Emphasize
HTML: "<FIELDSET>"   ;                     \ ignore start fieldset
HTML: "</FIELDSET>"   ;                    \ ignore end   fieldset
HTML: "</FONT>"   ;                        \ ignore font selection
HTML: "</FORM>"   ;                        \ ignore end of Form
HTML: "<H1>"      Bold ;                   \ Start current heading 1
HTML: "</H1>"     /Bold ;                  \ end current heading 1
HTML: "<H2>"      Italics ;                \ Start current heading 2
HTML: "</H2>"     /Italics ;               \ end current heading 2
HTML: "<H3>"      Bold ;                   \ Start current heading 3
HTML: "</H3>"     /Bold ;                  \ end current heading 3
HTML: "<H4>"      ;                        \ Start current heading 4
HTML: "</H4>"     ;                        \ end current heading 4
HTML: "<H5>"      ;                        \ Start current heading 5
HTML: "</H5>"     ;                        \ end current heading 5
HTML: "<H6>"      ;                        \ Start current heading 6
HTML: "</H6>"     ;                        \ end current heading 6
HTML: "<HEAD>"    ;                        \ ignore start of header
HTML: "</HEAD>"   ;                        \ ignore end   of header
HTML: "<HR>"      HorizontalRule ;         \ ignore Horizontal Rule
HTML: "<HR SIZE=" Get#To'>' to HRSize ;    \ extract Horiz Rule size parameter
HTML: "<HTML>"    ;                        \ ignore start of html document
HTML: "</HTML>"   ;                        \ ignore end   of html document
HTML: "<I>"       Italics ;                \ turn on  Italics
HTML: "</I>"      /Italics ;               \ turn off Italics
HTML: "<INS>"     ;                        \ ignore start insert
HTML: "</INS>"     ;                       \ ignore end   insert
HTML: "<KBD>"     ;                        \ ignore keyboard input
HTML: "</KBD>"    ;                        \ ignore not keyboard input
HTML: "<LEGEND>"    ;                      \ ignore start legend
HTML: "</LEGEND>"    ;                     \ ignore end   legend
HTML: "<LI>"      s"   * " "PutTextOut ;   \ start a list item
HTML: "</MAP>"    ;                        \ ignore map
HTML: "<META>"    ;                        \ ignore Meta information
HTML: "<MENU>"    ;                        \ ignore start Menu
HTML: "</MENU>"   ;                        \ ignore end   Menu
HTML: "<NOBR>"    ;                        \ ignore start No Break
HTML: "</NOBR>"   ;                        \ ignore stop No Break
HTML: "<NOFRAME>"  ;                       \ ignore No frames
HTML: "</NOFRAME>" ;                       \ ignore Not No frames
HTML: "</OBJECT>"  ;                       \ ignore /Object
HTML: "<OL>"  ;                            \ ignore ???
HTML: "</OL>"  ;                           \ ignore ???
HTML: "<OPTION>"  ;                        \ ignore Options
HTML: "<P>"       ;                        \ ignore start paragraph
HTML: "</P>"      ;                        \ ignore end paragraph
HTML: "<PRE>"     ;              \ ignore (Use text as formatted in file)
HTML: "</PRE>"    ;              \ ignore (Stop using text as formatted in file)
HTML: "</Q>"      ;                        \ ignore end question
HTML: "<S>"       Strikeout ;              \ turn on  Strikeout
HTML: "</S>"      /Strikeout ;             \ turn off Strikeout
HTML: "<SAMP>"    ;                        \ ignore     Sample output
HTML: "</SAMP>"   ;                        \ ignore not Sample output
HTML: "</SCRIPT>" ;                        \ ignore not Script
HTML: "</NOSCRIPT>" ;                      \ ignore not NoScript
HTML: "<SELECT>"  ;                        \ ignore     Selection list
HTML: "</SELECT>" ;                        \ ignore     Selection list
HTML: "<SMALL>"   ;                        \ ignore     Small font
HTML: "</SMALL>"  ;                        \ ignore not Small font
HTML: "<STRIKE>"  Strikeout ;              \ turn on  Strikeout
HTML: "</STRIKE>" /Strikeout ;             \ turn off Strikeout
HTML: "<STRONG>"  Bold ;                   \ turn on  Strong Emphesis
HTML: "</STRONG>" /Bold ;                  \ turn off Strong Emphesis
HTML: "<STYLE>"   ;                        \ ignore     Stype
HTML: "</STYLE>"  ;                        \ ignore not Style
HTML: "<SUB>"     ;                        \ ignore Subscript
HTML: "<SUP>"     ;                        \ ignore Superscript
HTML: "<TD>"      ;                        \ ignore Table Cols
HTML: "</TD>"     ;                        \ ignore Table Cols
HTML: "<TH>"      ;                        \ ignore Table Rows
HTML: "</TH>"     ;                        \ ignore Table Rows
HTML: "<TABLE>"   ;                        \ ignore start table
HTML: "</TABLE>"  ;                        \ ignore end   table
HTML: "<TBODY>"   ;                        \ ignore tbody
HTML: "<TFOOT>"   ;                        \ ignore tfoot
HTML: "<TITLE>"   s" </TITLE>" "IgnoreTo ; \ start of title
HTML: "</TITLE>"  ;                        \ end   of title
HTML: "<TR>"      ;                        \ ignore Table Rows
HTML: "</TR>"     ;                        \ ignore Table Rows
HTML: "<TT>"      ;                        \ ignore Typewriter font
HTML: "<U>"       Underline ;              \ turn on  Underline
HTML: "</U>"      /Underline ;             \ turn off Underline
HTML: "<UL>"      ;                        \ ignore start unordered list
HTML: "</UL>"     ;                        \ ignore end   unordered list
HTML: "<VAR>"     ;                        \ ignore start variable
HTML: "</VAR>"    ;                        \ ignore end   variable
HTML: "<WBR>"     ;                        \ ignore Word Break


\ *************************************************************************
\ 9 These processors need to be last, since they are short and open ended.
\ *************************************************************************

HTML: "<A REL"    IgnoreTo'>' ;          \ ignore Relationships
HTML: "<A REV"    IgnoreTo'>' ;          \ ignore Reverse Relationships
HTML: "<A SHAPE"  IgnoreTo'>' ;          \ ignore shape statements
HTML: "<APPLET"   IgnoreTo'>' ;          \ ignore APPLETs
HTML: "<AREA "    IgnoreTo'>' ;          \ ignore Map Sections
HTML: "<BASE "    IgnoreTo'>' ;          \ ignore Base specifications
HTML: "<BASEFONT" IgnoreTo'>' ;          \ ignore BASEFONTs
HTML: "<BLOCKQUOTE" IgnoreTo'>' ;        \ ignore blockquotes
HTML: "<BODY "    IgnoreTo'>' ;          \ ignore Color references
HTML: "<BR "      IgnoreTo'>' ;          \ ignore Clear textwrap
HTML: "<BUTTON"   IgnoreTo'>' ;          \ ignore buttons
HTML: "<CAPTION"  IgnoreTo'>' ;          \ ignore CAPTIONs
HTML: "<COL"      IgnoreTo'>' ;          \ ignore COLumn specifications
HTML: "<DIR "     IgnoreTo'>' ;          \ ignore Directory compact
HTML: "<DIV "     IgnoreTo'>' ;          \ ignore Align Division
HTML: "<DL "      IgnoreTo'>' ;          \ ignore Definition list
HTML: "<EMBED "   IgnoreTo'>' ;          \ ignore Embeded objects
HTML: "<FONT"     IgnoreTo'>' ;          \ ignore FONTs
HTML: "<FORM"     IgnoreTo'>' ;          \ ignore Forms
HTML: "<FRAME"    IgnoreTo'>' ;          \ ignore FRAMEs
HTML: "<H1 "      IgnoreTo'>' ;          \ start a new heading 1
HTML: "<H2 "      IgnoreTo'>' ;          \ start a new heading 2
HTML: "<H3 "      IgnoreTo'>' ;          \ start a new heading 3
HTML: "<H4 "      IgnoreTo'>' ;          \ start a new heading 4
HTML: "<H5 "      IgnoreTo'>' ;          \ start a new heading 5
HTML: "<H6 "      IgnoreTo'>' ;          \ start a new heading 6
HTML: "<HR "      IgnoreTo'>' ;          \ ignore Horizontal parameters
HTML: "<IMG "     IgnoreTo'>' ;          \ ignore IMAGEs
HTML: "<INPUT "   IgnoreTo'>' ;          \ ignore Input fields
HTML: "<ISINDEX"  IgnoreTo'>' ;          \ ignore searchable index
HTML: "<LI "      IgnoreTo'>' ;          \ ignore List bullet
HTML: "<LINK "    IgnoreTo'>' ;          \ ignore Relationship
HTML: "<NOSCRIPT" IgnoreTo'>' ;          \ ignore NoScript
HTML: "<MAP "     IgnoreTo'>' ;          \ ignore MAPs
HTML: "<MENU "    IgnoreTo'>' ;          \ ignore Menu compact
HTML: "<META "    IgnoreTo'>' ;          \ ignore Client Pull
HTML: "<MULTICOL" IgnoreTo'>' ;          \ ignore MULTICOLs
HTML: "<OBJECT"   IgnoreTo'>' ;          \ ignore Object
HTML: "<OL "      IgnoreTo'>' ;          \ ignore Ordered list
HTML: "<OPTION "  IgnoreTo'>' ;          \ ignore Options
HTML: "<P "       IgnoreTo'>' ;          \ ignore Alignment
HTML: "<Q "       IgnoreTo'>' ;          \ ignore question
HTML: "<SCRIPT"   IgnoreTo'>' ;          \ ignore Script
HTML: "<SELECT "  IgnoreTo'>' ;          \ ignore Selection list
HTML: "<SPACER"   IgnoreTo'>' ;          \ ignore SPACERs
HTML: "<TABLE "   IgnoreTo'>' ;          \ ignore TABLEs
HTML: "<TEXTAREA " IgnoreTo'>' ;         \ ignore Textarea
HTML: "<THEAD"    IgnoreTo'>' ;          \ ignore thead
HTML: "<TD "      IgnoreTo'>' ;          \ ignore TABLEs
HTML: "<TH "      IgnoreTo'>' ;          \ ignore TABLEs
HTML: "<TR "      IgnoreTo'>' ;          \ ignore TABLEs
HTML: "<UL "      IgnoreTo'>' ;          \ ignore unordered list
HTML: "<!"        IgnoreTo'>' ;          \ comments are not displayed

: "ProcessHTML" { adr len \ html$ -- a2 n2 }    \ process one HTML statement
        LMAXSTRING LocalAlloc: html$
        adr len html$ LPLACE            \ save original string away
        html$ LCOUNT
        2dup '>' scan nip - upper       \ convert partial to uppercase
        html$ LCOUNT
        html-chain do-chain             \ go look for an html command
        over html$ CELL+ =              \ if we didn't process any,
                                        \ at the end of the chain
        IF      2drop                   
                adr 1 "PutTextOut       \ then output 1 original character
                1                       \ one character processed
        ELSE    nip len swap -          \ else calc the processed characters
        THEN    adr len rot 0MAX /string     \ remove them from original string
        ;

-1 value ignoreNextLine

: "/TTO  ( a1 n1 n2 -- a2 n3)   \ show part n2 of string a1,n1, return remainder
        /split                  \ split string a1 n1 into two parts
        browse?
        IF      ignoreNextLine displayingLine =
                \ if we are on the next display line
                IF      -1 to ignoreNextLine
                        2drop
                        EXIT
                THEN
                BEGIN   2dup s" <" search
                WHILE   2swap 2over nip -
                        "&PutTextOut            \ display leading text
                        "ProcessHTML"
                REPEAT  2drop
                linking?
                IF      2drop
                        FALSE to ignoring?
                        displayingLine 1+ #Line"
                        BEGIN   2dup s" <" search
                        WHILE   2swap 2over nip -
                                "&PutTextOut            \ display leading text
                                "ProcessHTML"
                                displayingLine 1+ to ignoreNextLine
                        REPEAT   2drop
                THEN
                "&PutTextOut
        ELSE    "PutTextOut
        THEN    ;               \ return the remainder of string as a2 n3

: _+bg          ( -- )
                highBG   to textBG textBG   SetBkColor: dc
                highFG   to textFG textFG SetTextColor: dc ;

defer +bg       ' _+bg is +bg

: -bg           ( -- )
        normalBG to textBG textBG   SetBkColor: dc
        normalFG to textFG textFG SetTextColor: dc ;

\ rls January 2nd, 2001 - 16:09
: "/TTO+    ( a1 n1 n2 -- a2 n3)
        \ show part n2 of string a1,n1, returning remainder
        2dup >=
        IF      "/TTO
        ELSE
                over - >r dup "/TTO
                mspcs r> dup "/TTO
                2drop
        THEN ;

: showHighlightLine { theline \ stcol edcol -- }
      theline #line" col-cur /string          \ portion of text on screen
      dup hcst col-cur - 0max min to stcol    \ clip start to length of line
      dup hced col-cur - 0max min to edcol    \ clip end   to length of line
      theline hled <>                         \ extra if not last line
      hlst    hled <> and                     \ and if multiple lines
      theline hlst hled between and           \ and if highlighting this line
      IF      1 +to edcol
              1+
      THEN
      RectHigh
      IF
              theline hlst hled between
              IF
                      theline #line" col-cur /string
                      hcst col-cur - 0max to stcol
                      hced col-cur - 0max to edcol
                      stcol "/TTO+                    \ show unhighlighted part
                      +bg edcol stcol - "/TTO+        \ show highlighted part
                      -bg dup "/TTO                   \ show remainder
              ELSE
                      -bg                     \ any other line, just display it
                      dup "/TTO               \ other lines are not highlighted
              THEN
              2drop EXIT
      THEN
      theline hlst =
      IF      stcol "/TTO                     \ show unhighlighted part of line
              theline hled =
              IF      +bg
                      edcol stcol - "/TTO     \ show highlighted portion
                      -bg dup "/TTO           \ show remainder
              ELSE    +bg dup "/TTO           \ all rest is highlighted
                      -bg
              THEN
      ELSE    theline hlst >
              theline hled < and              \ middle lines, then highlight all
              IF      +bg
                      dup "/TTO               \ just show it all highlighted
                      -bg
              ELSE    theline hled =          \ last line, highlight start
                      IF      +bg
                              edcol "/TTO     \ unhighlighted up to end column
                              -bg
                              dup "/TTO       \ remainder is shown highlighted
                      ELSE    -bg             \ any other line, just display it
                              dup "/TTO       \ other lines are not highlighted
                      THEN
              THEN
      THEN    2drop ;                         \ discard any remaining text

:M StartPos:    ( -- x y )
        0 StartSize: WinEdToolbar nip 1+
        ;M

:M SetTopOf:    ( n1 -- )
        to TopOfWindow
        ;M

:M WindowStyle: ( -- style )            \ return the window style
        WindowStyle: super
        WS_VSCROLL or           \ add vertical scroll bar
        WS_HSCROLL or           \ add horizontal scroll bar
        WS_OVERLAPPED or
        ;M

:M ExWindowStyle: ( -- extended_style )
        ExWindowStyle: super
        ;M

: window-lines  ( -- n1 )
        Height: self CharHeight: self / ;

: margin-check  ( -- )
        s" 9" GetTextExtent: dc drop            \ width of a '9' char
        5 3 */ 14 max to window-lmargin ;

:M all-lines:   ( -- )
        update-flags MAXSTRING 1 fill
        ;M

:M 1-line-flag: ( n1 -- )       \ n1 = number in file to update if on screen
        line-cur -                  \ convert to screen relative
        1 swap 0max MAXSTRING 1- min update-flags + c!
        ;M


\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ 10  Start RePaint that works with Colons-Only  July 6th, 2002 RDA JAP
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

    0 value colonskip          \ rda
    1 value incloop            \ rda
false value done               \ rda

:M RePaint:     ( -- )
    alreadyPainting 0=
    IF  TRUE to alreadyPainting
        BEGIN
            FALSE to paintAgain
            SaveDC: dc                      \ save device context
            entry# >r                       \ save the current entry#
            Stack# to entry#
            Handle: vFont SetFont: dc
            text-ptr
            IF  margin-check
                        \ draw left border
                0 0 window-lmargin 2 -
                Height: self
                browse?
                IF    BmarginColor
                ELSE  marginColor
                THEN  FillArea: dc
                      \ vertical line beside left border
                Gray LineColor: dc
                3 0
                DO  window-Lmargin 3 - i + 0 MoveTo: dc
                    window-Lmargin 3 - i +
                    Height: self
                    LineTo: dc
                    White LineColor: dc
                LOOP
                -bg  0 to colonskip 1 to incloop  false to done \ rda
                window-lines 1+ 0
                ?DO
                    update-flags i + c@
                    IF  0 update-flags i + c!
                        window-lmargin       to xpixel  \ x
                        CharHeight: self i * to ypixel  \ y
                        xpixel ypixel
                        s" 0" GetTextExtent: dc nip
                        >r Width: WinEdWindow
                        xpixel + r> ypixel + 1+
                        line-cur i + colonskip + file-lines 1- >   \ rda
                            \ set the trail color
                        IF  LTGRAY true to done
                        ELSE
                            line-cur i + hlst hled between
                            highlighting? and
                            IF     selectBG
                            ELSE    trailBG
                            THEN
                        THEN
                        FillArea: dc
                        done not  \ rda
                        if       \ rda
                      line-cur i + colonskip + to displayingLine   \ rda
                        highlighting?
                        IF  displayingLine showHighlightLine
                        ELSE
                            displayingLine #line"
                            over c@ ascii : = colons-only 0= or \ rda
                            if                                 \ rda
                             col-cur /string
                             dup "/TTO 2drop
                            else 2drop 1 +to colonskip        \ rda
                                 1 update-flags i + c!        \ rda
                                 0 to incloop                 \ rda
                            then
                        THEN
                        ruling?
                        IF  BLACK               LineColor: dc
                            HRSize 1 max 12 min 0
                            DO      window-Lmargin ypixel I +
                                    MoveTo: dc
                                    Width ypixel I + LineTo: dc
                            LOOP
                            FALSE to ruling?
                        THEN
                        then           \ rda
                    THEN
                    incloop 1 to incloop    \ rda
                +LOOP                       \ rda
\ --------------------------------------------------------------
\ End of changes to code for Colon-only  July 6th, 2002 - 20:03 JAP

                browse? 0=
                IF  right-edge 0=
                    IF  page-lines?
                        self ConsoleWindow <> and
                             \ show page lines if not console
                        IF lend-len 0=
                             IF    binaryColor  LineColor: dc
                                   PRINTER-COLS
                             ELSE  linesColor  LineColor: dc
                                   PRINTER-COLS
                             THEN
                             col-cur - 0max dup>r
                             CharWidth: self * window-lmargin + 0
                             MoveTo: dc
                             r> CharWidth: self * window-lmargin +
                             Height LineTo: dc
                        THEN
                    ELSE
                        LTRED LineColor: dc
                        right-edge col-cur - 0max
                        CharWidth: self * window-lmargin + 0
                        MoveTo: dc
                        right-edge col-cur - 0max
                        CharWidth: self * window-lmargin + Height
                        LineTo: dc
                    THEN
                THEN
                window-lmargin 0>
                IF  linesColor   LineColor: dc   \ display page breaks
                    marginColor SetBkColor: dc
                    lend-len 0=
                    IF      binaryColor   LineColor: dc
                            BLOCK-LINES to PRINTER-ROWS
                    THEN
                    line-cur PRINTER-ROWS / PRINTER-ROWS *
                    line-cur -
                    BEGIN dup window-lines <
                    WHILE dup
                        page-lines? and
                        self ConsoleWindow <> and
                        \ show page lines if not console
                        IF  window-Lmargin
                            over CharHeight: self * MoveTo: dc
                            Width
                            over CharHeight: self * LineTo: dc
                        THEN  \ -- screenline#
                        \ display page number at page break line
                        dup>r line-cur + PRINTER-ROWS /
                        lend-len
                        IF  1+
                        THEN
                        0 <# # 2dup #> 2dup GetTextExtent: dc drop
                        window-Lmargin 5 - swap - 1 max
                        r@ CharHeight: self *
                        2swap TextOut: dc
                        over 0> r@ 0> and
                        IF  <# # 2dup #> 2dup GetTextExtent: dc drop
                            window-Lmargin 5 - swap - 1 max
                            r@ 1 - CharHeight: self *
                            2swap TextOut: dc
                        THEN
                        over 0> r@ 1 - 0> and
                        IF  <# # 2dup #> 2dup GetTextExtent: dc drop
                            window-Lmargin 5 - swap - 1 max
                            r@ 2 - CharHeight: self *
                            2swap TextOut: dc
                        THEN
                        over 0> r@ 2 - 0> and
                        IF  <# # 2dup #> 2dup GetTextExtent: dc drop
                            window-Lmargin 5 - swap - 1 max
                            r@ 3 - CharHeight: self *
                            2swap TextOut: dc
                        THEN
                        2drop r> PRINTER-ROWS +
                    REPEAT  drop
                THEN
                textBG SetBkColor: dc
            THEN
            using98/NT? 0=
            \ only support variable sized scroll bars in Windows98 and WindowsNT
                    \ *** See note below ***
            file-lines 32767 > OR   \ if we have a big file, revert to
                                    \ non-resizable scroll buttons
            IF
                \ set the vertical scroll bar limits
                FALSE file-lines window-lines - 0max 32767 min 0 SB_VERT
                GetHandle: self Call SetScrollRange drop

                \ position the vertical button in the scroll bar
                TRUE line-cur file-lines 32767 min file-lines 1 -
                1 max */ 32767 min SB_VERT
                GetHandle: self Call SetScrollPos drop
                \ set the horizontal scroll bar limits
                FALSE max-cols screen-cols 3 - - 0max col-cur max
                32767 min 0 SB_HORZ
                GetHandle: self Call SetScrollRange drop

                \ position the horizontal button in the scroll bar
                TRUE col-cur 32767 min SB_HORZ
                GetHandle: self Call SetScrollPos drop
            ELSE
                \ set the vertical scroll bar limits, position and page size
                window-lines to nPage
                line-cur to nPos
                0 to nMin
                file-lines to nMax
                SIF_ALL to fMask
                TRUE LPWinScrollInfo rel>abs SB_VERT
                GetHandle: self Call SetScrollInfo drop

                screen-cols to nPage
                col-cur to nPos
                0 to nMin
                max-cols to nMax
                SIF_ALL to fMask
                TRUE LPWinScrollInfo rel>abs SB_HORZ
                GetHandle: self Call SetScrollInfo drop
            THEN
            \ restore the current entry#
            r> to entry#
            \ restore the original DC
            RestoreDC: dc
            paintAgain 0=
        UNTIL
        FALSE to alreadyPainting
    ELSE
        TRUE to paintAgain
    THEN
    ;M


((      *** Note referenced above ***  
( 11 )
February 24th, 2000 - 11:06 tjz
Can't even really get it to work in WindowsNT, because SB_THUMBTRACK doesn't
work for live scroll tracking

Here is an excerpt from the Win32SDK help entry for GetScrollInfo, which is the
function you use to get the scroll position of the scroll bars;

"The limitation on this technique applies to real-time scrolling of a window’s
contents.  An application implements real-time scrolling by processing the
WM_HSCROLL or WM_VSCROLL messages that carry the SB_THUMBTRACK notification
value, thereby tracking the position of the scroll box (thumb) as the user moves
it. Unfortunately, there is no function to retrieve the 32-bit position
scroll-box position as the user moves the scroll box.  Because GetScrollInfo
provides only the static position, an application can obtain only 32-bit
position data before or after a scroll operation."

So, all we can do, is use variable size scroll bars when the file contains less
than 32767 lines.  Oh, well.
))

:M On_Paint:    ( -- )          \ screen redraw method
        bitImage?
        IF      SRCCOPY 0 0 imageDC GetHandle: [ ] GetSize: self 0 0 BitBlt: dc
        ELSE    all-lines: self
                RePaint: self
        THEN
        ;M

:M GetHandleOfDC: ( -- a1 )
        GetHandle: dc
        ;M

:M On_Init:     ( -- )
        On_Init: super
        #fonts 0
        DO      font-list i cells+ @ >r
                char-width            Width: [ r@ ]
                char-height          Height: [ r> ]
        LOOP                         Create: vFont
        GetWindowDC: self PutHandle: screenDC
        SaveDC: screenDC                        \ save device context
        Handle: vFont SetFont: screenDC         \ set the font to be used
        s" #" ( A CHARACTER ) GetTextExtent: screenDC
        to the-height to the-width
        RestoreDC: screenDC                     \ restore the device context
        GetHandle: screenDC ReleaseDC: self
        ;M

:M On_Done:     ( -- )
                Stack# entry-console <>
                IF      #fonts 0
                        DO      font-list i cells+ @ >r
                                Delete: [ r> ]
                        LOOP
                THEN
                On_Done: super
                ;M

:M WM_CLOSE     ( -- )
                FALSE to cursor-on?
                WM_CLOSE WM: Super
                ;M

:M RefreshOn:   ( -- )
                TRUE to refreshing?
                ;M

:M RefreshOff:  ( -- )
                FALSE to refreshing?
                ;M

0 value prev-depth

:M Refresh:     ( -- )          \ refresh the windows contents
                refreshing?
                IF      bitImage?
                        IF      Paint: self
                        ELSE    entry# >r
                                Stack# to entry#
                                get-dc
                                RePaint: self
                                release-dc
                                r> to entry#
                        THEN
                THEN
                ;M

:M RefreshAll:  ( -- )
                Paint: self
                ;M

:M VPosition:   ( n1 -- )       \ move to line n1 in file
                line-cur >r
                0max file-lines 1 - 0max min to line-cur
                r> line-cur <>
                IF      all-lines: self
                THEN
                ;M

:M HPosition:   ( n1 -- )       \ move to column n1
                col-cur >r
                0max max-cols screen-cols 4 - - 0max min to col-cur
                r> col-cur <>
                IF      all-lines: self
                THEN
                ;M

:M Home:        ( -- )          \ goto the top of the current file
                0 VPosition: self
                0 HPosition: self
                0 to cursor-line
                0 to cursor-col
                highlight-cursor
                RefreshAll: self
                ;M

:M End:         ( -- )          \ goto the end of the current file
                file-lines window-lines - VPosition: self
                file-lines 1- to cursor-line
                0             HPosition: self
                0             to cursor-col
                highlight-cursor
                ;M

:M VScroll:     ( n1 -- )       \ scroll up or down n1 lines in file
                line-cur + VPosition: self
                ;M

:M VPage:       ( n1 -- )       \ scroll up or down n1 pages in file
                window-lines 3 - 0max * VScroll: self
                ;M

:M HScroll:     ( n1 -- )       \ scroll horizontally n1 characters
                col-cur + HPosition: self
                ;M

:M HPage:       ( n1 -- )       \ scroll horizontally by n1 page
                screen-cols 4 - 0max * HScroll: self
                ;M

:M DefaultCursor: ( -- cursor-id )
                IDC_IBEAM
                ;M

: cur-column    ( -- n1 )
                mousex window-lmargin - 0max CharWidth: self /mod
                swap CharWidth: self 2 - > IF 1+ THEN
                col-cur +  ;

create info$ MAXSTRING allot

0 value lremain

:M WM_SETCURSOR { hndl msg wparam lparam \ oladr ladr llen lstrt lend lcol theLine linkCnt -- res }
\ { hndl msg wparam lparam \ oladr ladr llen lstrt lend lcol theLine linkCnt --
\   res }
    EraseRect: ClientRect                \ init to zeros
    ClientRect.AddrOf GetClientRect: self
    hWnd get-mouse-xy ClientRect.Top  ClientRect.Bottom between
    over ClientRect.Left ClientRect.Right  between and
    IF  ClientRect.Left dup window-lmargin 2 - + between
        IF  uparrow-cursor
        ELSE
            FALSE to html-link?
            FALSE to on-text?
            browse?
            IF  cur-column to lcol
                0 to lend
                mousey CharHeight: self / line-cur + file-lines
                1- min to theLine
                theLine #line"  to llen to ladr
                ladr to oladr
                0 to linkCnt              \ #found links
                BEGIN
                    html-link? 0=         \ haven't found a link
                    llen    7 >= and      \ and we have enough text remaining
                    linkCnt 3 <  and      \ ignore more than 3 links on a line
                    IF  ladr llen s" <A HREF"
                        caps-search
                    ELSE
                        0 0 FALSE
                    THEN
                WHILE
                    1 +to linkCnt           \ found one link
                    llen over - to lstrt    \ start of "<A"
                    2dup
                    7 /string               \ remove "<A HREF"
                    bl skip                 \ skip blanks and equal
                    '=' skip
                    bl skip
                    '"' skip                \ and leading '"'
                    '#' skip                \ and leading '#'
                    2dup '>' scan nip -     \ scan to ending '>'
                    2dup + 1- c@ '"' =
                    IF   1- THEN            \ remove trailing '"'
                    s"   Link to: "
                    SetStatus: WinEdWindow
                    s" >"     search drop   \ find ">" and discard flag
                    1 /string 2dup          \ remove the '>' char from text
                    s" </A>" caps-search        \ skip to </A>
                    IF  dup 4 - 0max to lremain \ remove </A> from text
                        nip - nip 1- 0max       \ calc length till </A>
                        lstrt + to lend
                        lcol lstrt lend between dup
                        IF      lstrt 1+ to html-link?
                        THEN
                    ELSE
                        theLine 1+ #line" 2dup
                        s" </A>" caps-search drop   \ skip to </A>
                        dup 4 - 0max to lremain     \ remove </A> from text
                        nip - nip 1- 0max           \ calc length till </A>
                        lstrt + to lend
                        lcol lstrt lend between dup
                        IF      lstrt 1+ to html-link?
                        THEN
                    THEN
                    lcol lend - 0max to lcol
                    ladr llen llen lremain - 0MAX
                    /string to llen to ladr
                REPEAT
                2drop
                html-link? 0=
                IF  lcol llen <
                    IF      oladr lcol + c@ bl <>    \ if NOT a blank
                            dup to on-text?
                    ELSE    FALSE
                    THEN
                    s"  " StatusString count
                    SetStatus: WinEdWindow
                ELSE
                    TRUE
                THEN
                IF  using-Win32s?
                    IF      hand-cursor
                    ELSE    harrow-cursor
                    THEN
                ELSE
                    arrow-cursor
                THEN
            ELSE
                ibeam-cursor
                s"  " StatusString count SetStatus: WinEdWindow
            THEN
        THEN
        1
    ELSE
        drop
        hndl msg wparam lparam DefWindowProc: [ self ]
    THEN
    ;M

:M RefreshCursor: ( -- )
        have-focus?
        IF      cursor-col  col-cur  - CharWidth: self * window-lmargin +
                cursor-line line-cur - dup 0<
                IF      drop -4
                THEN    CharHeight: self * MoveCursor: self
        THEN
        ;M 

:M On_SetFocus: ( h m w l -- )
        On_SetFocus: super
        DocWindow >r
        entry# >r
        self to DocWindow
        Stack# to entry#        \ select our file in file stack
        cursor-col   col-cur -  CharWidth: self * window-lmargin +
        cursor-line line-cur - CharHeight: self *
        2 CharHeight: self 1- MakeCursor: self
        r> to entry#
        r> to DocWindow
        ;M

:M On_KillFocus: ( h m w l -- )
        DestroyCursor: self
        On_KillFocus: super
        ;M

\ Wheelmouse support added
\ March 9th, 2003 - 11:49  dbu
0x020A constant WM_MOUSEWHEEL \ because it's missing in the Windows Constants

CODE SWORD-SPLIT ( s1 -- low high ) \ split the signed 32bit s1 into its high
        movsx eax, bx               \ and low 16bit quantities.
        push eax                    \ by Alex McDonald
        shr ebx, # 16
        movsx ebx, bx
        next c;

: S-HIWORD SWORD-SPLIT NIP ;
: S-LOWORD SWORD-SPLIT DROP ;

:M WM_MOUSEWHEEL   ( h m w l -- res )
        SetFocus: self
        self to DocWindow
        Stack# to entry#

        \ get the WHEEL_DELTA (hiword of wParam)
        \ A positive value indicates that the wheel was rotated forward, away
        \ from the user; a negative value indicates that the wheel was rotated
        \ backward, toward the user.
        OVER S-HIWORD
        0<
        IF
                \ is the shift-button down?
                OVER S-LOWORD MK_SHIFT AND
                IF    1   VPage: self \ scroll one page up
                ELSE  3 VScroll: self \ scroll three lines up
                THEN
        ELSE
                \ is the shift-button down?
                OVER S-LOWORD MK_SHIFT AND
                IF   -1   VPage: self \ scroll one page down
                ELSE -3 VScroll: self \ scroll three lines down
                THEN
        THEN

        RefreshAll: self
        RefreshCursor: self
        0 ;M

0 value prevLine

:M WM_VSCROLL   ( h m w l -- res )
        SetFocus: self
        self to DocWindow
        Stack# to entry#
        swap word-split >r
        CASE
                SB_BOTTOM        OF          End: self          ENDOF
                SB_TOP           OF         Home: self          ENDOF
                SB_LINEDOWN      OF    1 VScroll: self          ENDOF
                SB_LINEUP        OF   -1 VScroll: self          ENDOF
                SB_PAGEDOWN      OF    1   VPage: self          ENDOF
                SB_PAGEUP        OF   -1   VPage: self          ENDOF
                SB_THUMBTRACK
                OF    ?control
                      IF      r@ prevLine < 2* 1+ VScroll: self
                      ELSE    file-lines 32767 >
                              IF     file-lines r@ 32767 */ VPosition: self
                              ELSE              r@          VPosition: self
                              THEN
                      THEN
                ENDOF
        ENDCASE
        r> to prevLine
        RefreshAll: self
        RefreshCursor: self
        0 ;M

:M WM_HSCROLL   ( h m w l -- res )
                SetFocus: self
                self to DocWindow
                Stack# to entry#
                swap word-split >r
                CASE
                        SB_BOTTOM        OF          End: self   ENDOF
                        SB_TOP           OF         Home: self   ENDOF
                        SB_LINELEFT      OF   -1 HScroll: self   ENDOF
                        SB_LINERIGHT     OF    1 HScroll: self   ENDOF
                        SB_PAGELEFT      OF   -1   HPage: self   ENDOF
                        SB_PAGERIGHT     OF    1   HPage: self   ENDOF
                        SB_THUMBPOSITION OF r@ HPosition: self   ENDOF
                        SB_THUMBTRACK    OF r@ HPosition: self   ENDOF
                ENDCASE r>drop
                RefreshAll: self
                RefreshCursor: self
                0 ;M

\ Don't allow window to be moved up into toolbar

:M WM_WINDOWPOSCHANGING ( h m w l -- res )
                dup 3 cells+ abs>rel dup @ TopOfWindow max swap !
                DefWindowProc: [ self ]
                ;M

;Class

EditWindowClass EDIT-WINDOW
EditWindowClass CONSOLE-WINDOW
        entry-console SetStack: CONSOLE-WINDOW  \ console is always fixed

edit-window    to EditWindow
edit-window    to DocWindow
console-window to ConsoleWindow

:Class Splitter         <super child-window

:M WindowStyle: ( -- style )            \ return the window style
                WindowStyle: super
                WS_OVERLAPPED or
                WS_DLGFRAME or
                WS_DISABLED or
                ;M

:M On_Paint:    ( -- )          \ screen redraw method
                0 0 Width Height LTGRAY FillArea: dc
                ;M

;Class

Splitter SplitterV
Splitter SplitterH

: set-console-height ( -- )
                GetSize: WinEdWindow nip drag-barV - to console-height ;

: in-EditWindow? ( -- f1 )
                GetHandle: EditWindow dup>r get-mouse-xy r> in-button? ;

: in-ConsoleWindow? ( -- f1 )
                GetHandle: ConsoleWindow dup
                IF     dup>r get-mouse-xy r> in-button?
                THEN    ;

: minimized-EditWindow? ( -- f1 )              \ is edit window NOT showing
                drag-barV StartSize: WinEdToolbar nip char-height 2* + < ;

: unminimize-EditWindow ( -- )
                minimized-EditWindow?
                IF      GetSize: WinEdWindow nip 2/ to drag-barV
                        set-console-height
                        Refresh: WinEdWindow
                THEN    ;

: minimized-ConsoleWindow? ( -- f1 )           \ is console window NOT showing
                console-height char-height 2* u< ;

: unminimize-ConsoleWindow ( -- )
                minimized-ConsoleWindow?
                IF      GetSize: WinEdWindow nip 2/ to drag-barV
                        set-console-height
                        Refresh: WinEdWindow
                ELSE    Refresh: EditWindow
                THEN    ;

: >F            ( -- )          \ select the console window as active
                show-console? 0=
                IF      StartConsole: WinEdWindow
                THEN    WINPAUSE
                DocWindow ConsoleWindow <>
                IF      ConsoleWindow to DocWindow
                        GetStack: DocWindow to entry#
                        SetFocus: DocWindow
                        unminimize-ConsoleWindow
                THEN    ;

: >E            ( -- )
                EditWindow to DocWindow
                GetStack: DocWindow to entry#
                show-console?
                IF      SetFocus: EditWindow
                THEN    ;

: >E-unminimize ( -- )
                >E unminimize-EditWindow ;

: next-window   ( -- )
                DocWindow EditWindow =
                IF      show-console?
                        IF      >F
                        ELSE    beep
                        THEN
                ELSE    >E-unminimize
                THEN
                ReTitle: WinEdWindow
                Refresh: WinEdWindow ;


\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ 12    Demo about dialog, copied from the Forth About Dialog  
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

:Object about-demo-dialog       <SUPER dialog

IDD_ABOUT_FORTH forthdlg find-dialog-id constant template

create about-edit-msg
        z," WinEd,  Built: "
        get-local-time time-buf >month,day,year" +z",    
        +z," \nPublic Domain since January 1995\n"
        +z," Written by: Tom Zimmer\n"
        -null, here 0 c, align about-edit-msg - constant about-edit-len

create about-edit-msg2
         z," November 1995:\n"
        +z," Released as a replacement for the NEWZ editor in\n"
        +z," Win32Forth. All of NEWZ's functionality has been\n"
        +z," implemented in WinEd. Plus many new features\n"
        +z," have been added, like the toolbar and Font\n"
        +z," selection plus automatic saves of user defaults."
        -null, here 0 c, align about-edit-msg2 - constant about-edit-len2

:M On_Init:     ( hWnd-focus -- f )
        about-edit-msg  about-edit-len  IDD_ABOUT_TEXT  SetDlgItemText: self
        about-edit-msg2 about-edit-len2 IDD_ABOUT_TEXT2 SetDlgItemText: self
        1 ;M

:M GetTemplate: ( -- template )
        template
        ;M

:M On_Command:  ( hCtrl code ID -- f1 )
                CASE 
                IDCANCEL OF     0 end-dialog    ENDOF
                                false swap ( default result )
                ENDCASE ;M

;Object

: about-demo    ( -- )
                WinEdWindow Start: about-demo-dialog drop
                SetFocus: DocWindow ;


\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ 13    Setup the line pointers and scroll bar for a new file  
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

: set-line-pointers { textlen \ bincnt -- }
    wait-cursor             \ release/allocate line pointer table
    line-tbl ?dup IF release THEN
    1000 to max-lines
    max-lines 10 + cells malloc to line-tbl
    0 to file-lines         \ one total lines to start
    text-ptr line-tbl !     \ init first line
    1 to line-cur           \ bump to next line pointer
    0 to lend-len           \ mark as no line terminator initially
    0 to lend-char          \ and therefore the terminator is a null
    editAsBinary? 0=        \ if not editing as binary, then check file type
    IF  text-ptr textlen LMAXCOUNTED 1- umin 0x0D
        scan drop 1+ c@ 0x0A =                  \ CRLF or
        textlen 0= or                           \ empty file
        IF  0x0D to lend-char
            2    to lend-len
            text-ptr textlen + 2 - w@ 0x0A0D <>
            \ really 0x0D, 0x0A, cause bytes are reversed for W@
            IF  0x0A0D text-ptr textlen + w!
                2 +to textlen
            THEN
        THEN
        text-ptr textlen LMAXCOUNTED umin 0x0A
        scan 0> swap 1- c@ 0x0D <> and          \ LF only APPLE
        IF  0x0A to lend-char
            1 to lend-len
            text-ptr textlen + 1 - c@ lend-char <>
            IF   lend-char text-ptr textlen + c!
                 1 +to textlen
            THEN
        THEN
        text-ptr textlen LMAXCOUNTED 1-  umin 0x0D scan 0> swap 1+ c@
        0x0A <> and \ CR only UNIX
        IF  0x0D to lend-char
            1 to lend-len
            text-ptr textlen + 1 - c@ lend-char <>
            IF  lend-char text-ptr textlen + c!
                1 +to textlen
            THEN
        THEN
    THEN
    FALSE to editAsBinary?
    lend-len                        \ normal line terminator
    IF  text-ptr textlen
        BEGIN
            lend-char scan dup
        WHILE
            lend-len /string over line-tbl line-cur cells+ !
            \ if line longer than LMAXCOUNTED chars
            line-tbl line-cur cells+ dup @ swap
            cell- @ 2dup - LMAXCOUNTED 1- u>
            IF  tuck -                   \ convert to adr,len
                BEGIN
                    BLOCKLINE-SIZE /string dup   \ treat each 64 chars as a line
                WHILE
                    BEGIN
                        2dup swap c@ bl <> and  \ look for next blank
                    WHILE
                        1 /string         \ scan till find one
                    REPEAT
                    over c@ bl =          \ found a blank?
                    IF  1 /string         \ skip to next char
                    THEN                  \ fill in line pointer table, but only
                    dup                   \ if anything remains
                    IF      over line-tbl line-cur cells+ !
                            1 +to line-cur  \ bump current line
                            1 +to file-lines
                    THEN
                REPEAT                    \ fill in last part of very long line
                drop    line-tbl line-cur cells+ !
            ELSE
                2drop
            THEN
            1 +to line-cur                \ bump current line
            1 +to file-lines
            line-cur max-lines u>         \ while not full
            IF      4000 +to max-lines
                    max-lines 10 + cells line-tbl realloc
                    s" Failed to adjust the line pointer table"
                    ?TerminateBox
                    to line-tbl
            THEN
        REPEAT  drop
    ELSE                      \ no line terminator, make 64 character lines
        text-ptr textlen
        BEGIN
            dup
        WHILE
            BLOCKLINE-SIZE /string over line-tbl line-cur cells+ !
            1 +to line-cur                  \ bump current line
            1 +to file-lines
            max-lines line-cur u<           \ if it's full
            IF  4000 +to max-lines          \ then extend table
                max-lines 10 + cells line-tbl realloc
                s" Failed to adjust the line pointer table"
                ?TerminateBox
                to line-tbl
            THEN
        REPEAT  drop
    THEN
    line-cur   1 max to line-cur
    file-lines 1 max to file-lines
    dup line-tbl line-cur     cells+ !
    dup line-tbl line-cur 1+  cells+ !
        line-tbl line-cur 2 + cells+ !
    0 to line-cur ;

: set-longest-line ( -- )
                wait-cursor
                0
                file-lines 0
                ?DO     i #line" nip max
                LOOP    1+ to max-cols
                ;

0 value prLine          \ cumulative printer line count on a page

\ Note: each page-tbl entry uses MS 4 bits to indicate the number of extension
\ lines at the top of the NEXT page.  The remainder of the entry is the line
\ number of the last real line on the current page.

: next-ext-lines        ( page -- ext-lines )
        cells page-tbl + @ 28 rshift ;

: last-line-of-page     ( page -- line# )
        cells page-tbl + @ 0x0FFFFFFF and ;

: set-pages     ( -- )                  \ rls - page
        page-tbl ?dup IF release THEN
        file-lines max-cols XLCnt * printer-rows / 2 + to max-pages
        max-pages 10 + cells malloc to page-tbl
        0 to page-cur
        0 to line-cur
        0 to prLine
        BEGIN   line-cur #line.len XlCnt prLine + dup printer-rows <
                IF      to prLine
                ELSE    1 +to page-cur
                        printer-rows swap - dup to prLine
                        28 lshift line-cur 1+ 0x0FFFFF and or
                        page-tbl page-cur cells+ !
                THEN
                line-cur 1+ dup to line-cur
                #line.bytes 0=
        UNTIL
        page-cur to num-pages
        file-lines 1- 0max page-tbl page-cur 1+ cells+ !
        0 to line-cur ;

: refresh-line  ( -- )
                cursor-line 1-line-flag: DocWindow
                                Refresh: DocWindow ;

: refresh-screen ( -- )
                RefreshAll: DocWindow
                ReTitle: WinEdWindow ;

: no-highlight  ( -- )
                normalBG to textBG
                highlight-cursor
                refresh-screen ;

: highlight-all ( -- )
                0 to hlst 0 to mlst
                0 to hcst 0 to mcst
                file-lines dup to hled #line" nip to hced
                refresh-screen ;

\ begin/end hilighting -------------------------

\ Concept provided   by: Brad Eckert
\ Some modifications by: Tom Zimmer
\ Other modifications by rls January 4th, 2001 - 20:57

0 value rowmark
0 value colmark

: highlight-mark ( -- )         \ Ctrl+Q Select Toggle
\ endpoint is cursor, startpoint is last marker
        RectHigh
        IF      cursor-col  colmark <
                IF      cursor-col  to hcst
                        colmark to hced
                ELSE    colmark to hcst
                        cursor-col  to hced
                THEN
                cursor-line rowmark <
                IF      cursor-line to hlst
                        rowmark to hled
                ELSE    rowmark to hlst
                        cursor-line to hled
                THEN
                cursor-col  to colmark
                cursor-line to rowmark
        ELSE
                cursor-line rowmark =
                cursor-col  colmark < and
                cursor-line rowmark < or
                IF      rowmark to hled
                        colmark to hced
                        cursor-col  to hcst cursor-col  to colmark
                        cursor-line to hlst cursor-line to rowmark
                ELSE    rowmark to hlst
                        colmark to hcst
                        cursor-col  to hced cursor-col  to colmark
                        cursor-line to hled cursor-line to rowmark
                THEN
        THEN
        refresh-screen ;

\ ----------------------------------------------

: do-esc        ( -- )
                no-highlight ;

: cursor-on-screen ( -- )
      cursor-line line-cur <
      IF    cursor-line find-top-margin - VPosition: DocWindow
      ELSE  DocWindow EditWindow =
            IF    cursor-line line-cur - screen-rows 3 - =
                  IF      cursor-line screen-rows 8 - - VPosition: DocWindow
                  THEN
                  cursor-line line-cur - screen-rows 4 - >
                  IF      cursor-line screen-rows 2/  - VPosition: DocWindow
                  THEN
            ELSE  cursor-line line-cur - screen-rows 1 - >=
                  IF      cursor-line screen-rows 2 - - VPosition: DocWindow
                  THEN
            THEN
      THEN ;

: browse-toggle ( -- )
        >E-unminimize
        bitImage? ?EXIT
        browse? 0= to browse?
\ [rda] this causes loss of edit changes JP stands corrected December 1st, 2002
\       browse?
\       IF      FALSE to edit-changed?
\       THEN
\ [jap] end of \ slashed lines
        update-mirror-browse
        refresh-screen
        Refresh: WinEdWindow
        ;

: +row-cursor   ( n1 -- )
        highlighting?
        IF      no-highlight
        ELSE    cursor-line 1-line-flag: DocWindow
        THEN
        line-cur >r
        -trailing-blanks
        cursor-line dup>r + 0max file-lines 1- min to cursor-line
                        \ if the cursor was on screen, just move it
        r> line-cur dup screen-rows 1- + between
        IF      cursor-on-screen
        ELSE    cursor-line find-top-margin - VPosition: DocWindow
        THEN    highlight-cursor
        refresh-line
        r> line-cur <>
        IF      refresh-screen
        ELSE    ReTitle: WinEdWindow
        THEN    ;

: >col-cursor   ( n1 -- )
        highlighting?
        IF      no-highlight
        THEN
        col-cur >r
        dup 0<                                  \ before start of line
        cursor-line 0> and                      \ and not on line zero
        IF
                -1 +row-cursor
                1+                              \ one less chars
                cursor-line #line" nip +        \ move to end of line
        THEN    0max to cursor-col
        cursor-col max-cols max "LCLIP" to max-cols \ adj right margin
        cursor-col col-cur - screen-cols 4 - >
        IF      cursor-col screen-cols 4 - -
                HPosition: DocWindow
        ELSE    cursor-col col-cur <
                IF      cursor-col
                        HPosition: DocWindow
                THEN
        THEN
        highlight-cursor
        refresh-line
        r> col-cur <>
        IF      refresh-screen
        THEN    ;


\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ 14    Text Search functions  
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

\ Modified by rls January 6th, 2002 - 6:02

0 value busy?                   \ flag to prevent reentrancy
FALSE value find-label?         \ are we searching for a label?

0 value CaseSensitive?
0 value srch_len
0 value startcol

\ search for sa2,sn2 within string sa1,sn1.
\ n3 is the remaining string length.
\ n4=length of string matched, possibly excluding some leading or trailing spaces
\ flg is truth value of search
\ (Allow ALT 0160 followed by a space as a pseudo leading space )

: xsearch       { sa1 sn1 sa2 sn2 \ pspf ldsp1 ldsp2 trsp2 n4 -- a3 n3 flg }
        \ case sensitive selectable
                \ sa1   target starting address
                \ sn1   target initial count
                \ sa2   search buffer starting address
                \ sn2   search buffer count
                \ pspf  pseudo-space flag
                \ ldsp1 count of leading spaces in target
                \ ldsp2 count of leading spaces in search buffer
                \ trsp2 count of trailing spaces in search buffer
                \ a3    starting address of possible matched string in target
                \ n3    remaining count in target
                \ n4    true count of matched string in target
        0 to srch_len
        false to pspf
        0 to ldsp2
        0 to trsp2
        0 to n4
        sa1 sn1 bl skip drop
        sa1 - to ldsp1
        sa2 w@ 0x20A0 =                 \ a pseudo-space flag?
        IF      sa2 sn2 1 /string
                to sn2  to sa2
                sa2 sn2 bl skip drop
                sa2 - to ldsp2
                startcol 0=
                ldsp2 ldsp1 > and
                IF
                        true to pspf    \ only if we are at the line start
                THEN
        ELSE    sa2 sn2 bl skip drop
                sa2 - to ldsp2
        THEN
        sa2 sn2 -trailing
        sn2 over - to trsp2             \ count trailing spaces
        dup to sn2                      \ modify sn2 to exclude trailing spaces
        bl skip sn2 swap -
        to ldsp2 drop                   \ count leading spaces
        pspf                            \ possible pseudo space case
        IF      sa1 sa2 sn2
                ldsp2 ldsp1 - /string   \ advance search buffer pointer
                dup to n4 swap n4       \ ( sa1 n4 sa2 n4 )
                CaseSensitive? 0=
                IF      caps-compare
                ELSE    compare
                THEN
                0=              \ if result is 0, string is matched to start
                IF              \ of trailing spaces
                        sa1 sn1 2dup
                        n4 /string              \ look for trailing spaces
                        2dup bl skip dup 0=
                        IF      \ We have reached the end of the target buffer
                                2drop nip trsp2 min n4 + dup to srch_len
                                0<> EXIT
                        ELSE    \ We have NOT reached the end of the target
                                nip - nip trsp2 >=
                                IF      \ Enough trailing spaces
                                        trsp2 n4 + dup to srch_len 0<> EXIT
                                THEN
                        THEN
                THEN
                2drop                   \ clean up stack
        THEN
        sn2 to n4
        BEGIN
                CaseSensitive? 0=                       \ search
                IF      sa1 sn1 sa2 sn2 caps-search
                ELSE    sa1 sn1 sa2 sn2      search
                THEN
                IF      \ String is matched to start of trailing spaces
                        2dup n4 /string         \ look for trailing spaces
                        2dup bl skip dup 0=
                        IF      \ We have reached the end of the target buffer
                                2drop nip trsp2 min n4 + dup to srch_len
                                0<> EXIT
                        ELSE    \ We have NOT reached the end of the target
                                nip - nip trsp2 >=
                                IF      \ Enough trailing spaces
                                        trsp2 n4 + dup to srch_len 0<> EXIT
                                THEN
                        THEN
                        1 /string dup n4 <
                        IF      \ Not enough characters left for a match
                                0 to srch_len 0 EXIT
                        THEN
                        to sn1
                        dup sa1 - +to startcol
                        to sa1
                ELSE    \ No match possible
                        0 to srch_len 0 EXIT
                THEN
        AGAIN ;

: findsizemin   ( addr cnt -- cnt2 )            \ rls January 8th, 2002 - 19:32
        over c@ 0xA0 =
        IF      1 /string       THEN
        bl skip -trailing nip ;

: to-find-line  ( col row -- )          \ zero based row
                file-lines 1- min 0max
                to cursor-line
                to cursor-col
                0 HPosition: DocWindow
                cursor-on-screen
                cursor-col find-buf c@ + screen-cols 3 - - HPosition: DocWindow
                cursor-line dup to hlst to hled
                cursor-col         to hcst
                hcst find-buf c@ + to hced ;

\ selectably case sensitive search backwards

\ rls mods January 6th, 2002 - 20:35
: -xsearch      { sadr slen fadr flen \ ffnd srch_lenz -- a3 n3 n4 }
                0 to ffnd
                sadr slen
                BEGIN   fadr flen xsearch
                WHILE   dup +to startcol
                        srch_len to srch_lenz
                        2dup to slen to sadr
                        1 /string
                        true to ffnd
                REPEAT
                srch_lenz to srch_len
                2drop sadr slen ffnd ;

0 value search-till

: .searching    ( n1 -- )
        dup 0=
        IF      0 to search-till
                FALSE to search-aborted?
        THEN
        search-till 4000 >=
        IF      search-till 4000 =
                IF      s" Searching:             \n\nPress ESC to abort"
                        "message
                THEN
                dup 1000 mod 0=
                IF      s" Searching:             \n\nPress ESC to abort" 2>r
                        0 (ud,.)
                        2r@ bl scan drop 1+ swap 11 min move
                        2r> MessageText: msg-window
                        Refresh: msg-window
                        key?
                        IF     key k_esc = to search-aborted?
                        THEN
                ELSE    drop
                THEN
        ELSE    drop
        THEN    1 +to search-till ;

\ rls mods January 6th, 2002 - 21:11
: _back-find-text-again ( -- f1 )        \ f1=TRUE if found
        FALSE busy? ?EXIT               \ leave if already busy
        drop
        TRUE to busy?
        cursor-line 0=
        cursor-col  0= and
        IF      End: DocWindow
                cursor-line #line.len to cursor-col                        
        THEN
        cursor-line 1-line-flag: DocWindow
        0 to startcol
        cursor-line #line" cursor-col min
        find-buf count -xsearch
        IF      cursor-line #line"
                cursor-col min rot - nip nip to cursor-col
                0 HPosition: DocWindow
                cursor-line dup to hlst to hled
                cursor-col              to hcst
                hcst srch_len   +       to hced
                TRUE
        ELSE    2drop true
                0 .searching
                0 cursor-line 1- 0max
                ?DO     i .searching
                        0 to startcol
                        i #line" find-buf count -xsearch
                        IF      i #line" rot - nip nip
                                i to-find-line
                                hcst srch_len + to hced
                                0= LEAVE
                        ELSE    2drop
                        THEN
                        search-aborted? ?LEAVE
            -1 +LOOP
                IF      End: DocWindow
                        cursor-line #line" nip to cursor-col
                        FALSE
                ELSE    TRUE
                THEN
                message-off
        THEN
        refresh-line
        ReTitle: WinEdWindow
        FALSE to busy? ;

: back-find-text-again ( -- )
        bitImage? ?EXIT
        busy? ?EXIT                     \ leave if already busy
        find-buf c@ 0=
        IF      TRUE to busy?
                find-buf WinEdWindow Start: FindTextDlg
                dup 2 = to CaseSensitive?
                FALSE to busy?
                find-buf c@ 0=
                IF      drop FALSE
                        beep
                THEN
        ELSE    true
        THEN    \ -- f1
        IF      _back-find-text-again 0=
                IF      _back-find-text-again ?beep
                THEN
        THEN
        SetFocus: DocWindow ;

: find-label    { line# -- flag }       \ flag=TRUE if we found the label
        line# #line" s" <A NAME" dup>r xsearch
        IF      r> /string                    \ remove leading "<A NAME" string
                bl skip                       \ skip any leading blanks
                '=' skip                      \ skip equal sign
                bl skip                       \ skip any more leading blanks
                2dup '>' scan nip -           \ trim text after '>' in line
                                              \ retain trailing '"' (quote)
                     '"' skip                 \ strip off leading '"' (quote)
                drop find-buf count tuck
                caps-compare 0=
                IF      0 line# to-find-line  \ if found, move to this line
                        TRUE                  \ mark as all done searching
                ELSE    FALSE                 \ else we continue searching
                THEN
        ELSE    2drop
                r>drop
                FALSE
        THEN    ;

\ rls mods January 6th, 2002 - 21:14
: _find-text-again      ( -- f1 )
      FALSE   busy? ?EXIT \ leave if already busy
      drop                \ discard the boolean, we will generate another
      TRUE to busy?
      find-buf c@
      IF      find-buf count InsertString: findComboEdit
              cursor-line file-lines 1- >=
              IF      Home: DocWindow
                      0 >col-cursor
                      FALSE to busy?
                      false beep EXIT
              THEN
              cursor-line 1-line-flag: DocWindow
              cursor-line #line" cursor-col /string
              cursor-col to startcol
              dup find-buf count findsizemin >= >r
              find-buf count xsearch r> and             \ on current line?
              IF      cursor-line #line" rot - nip nip
                      cursor-line to-find-line
                      hcst srch_len + to hced
                      TRUE
              ELSE    cursor-line file-lines 1- =
                      cursor-col  cursor-line #line" nip = and
                      IF      Home: DocWindow
                              0 to cursor-col
                      THEN
                      2drop true
                      0 .searching
                      file-lines
                      cursor-line file-lines 1- min 1+
                      ?DO     i .searching
                              find-label?
                              IF      i find-label
                                      IF      0=      \ invert the flag on stack
                                              LEAVE
                                      THEN
                              ELSE    0 to startcol
                                      i #line"
                                      dup find-buf count findsizemin <
                                      IF        \ Don't search: line too short
                                                2drop
                                      ELSE
                                              find-buf count xsearch
                                              IF      i #line" rot - nip nip
                                                      i to-find-line
                                                      hcst srch_len + to hced
                                                      0= LEAVE
                                              ELSE    2drop
                                              THEN
                                      THEN
                              THEN
                              search-aborted? ?LEAVE
                      LOOP
                      IF      FALSE
                              Home: DocWindow
                              0 to cursor-col
                      ELSE    TRUE
                      THEN
                      message-off
              THEN
              refresh-line
      ELSE    FALSE
      THEN
      ReTitle: WinEdWindow
      FALSE to busy? ;
      
defer find-text ( -- )

: find-text-again ( -- )
                bitImage? ?EXIT
                find-buf c@ 0=
                IF      find-text
                ELSE    1 +to cursor-col       \ skip one character
                        _find-text-again 0=
                        IF      _find-text-again ?beep
                        THEN
                THEN    SetFocus: DocWindow ;

: +row-scroll   ( n1 -- )
                line-cur + 0max file-lines 1- min to line-cur
                refresh-screen ;

: +col-cursor   ( n1 -- )
                cursor-col +
                lend-len 0=
                IF      63 min
                THEN    >col-cursor
                cursor-on-screen
                ReTitle: WinEdWindow ;

: _>row-col     ( row col -- )
                lend-len 0=
                IF      63 min
                THEN
                to cursor-col
                to cursor-line
                cursor-on-screen ;

: >row-col      ( row col -- )  \ move display to row and column
                _>row-col
                no-highlight
                refresh-screen ;

: +page-cursor  ( 1 -- )
                all-lines: DocWindow
                -trailing-blanks
                screen-rows 1- * dup
                line-cur    + 0max file-lines 1- min to line-cur
                cursor-line + 0max file-lines 1- min to cursor-line
                highlight-cursor
                line-cur VPosition: DocWindow 
                refresh-screen ;

: cell-fill     { addr len value -- }
                addr len cells bounds
                ?DO     value i !       \ fill each cell with value
          cell +LOOP    ;

: insert-lines  { #lines -- }           \ insert a number of blank lines
                line-tbl cursor-line          cells+
                line-tbl cursor-line #lines + cells+
                file-lines cursor-line -  3 + cells move
                #lines +to file-lines
                line-tbl cursor-line          cells+    \ addr of new lines
                #lines                                  \ length in cells
                over @                                  \ address to fill with
                cell-fill                               \ fill the cells
                cursor-line #lines bounds
                ?DO     cur-buf 0 i #line!              \ CRLF in each line
                LOOP
                ;

               8192 constant del-max
del-max MAXSTRING + Pointer  del-buf
                  0 value    del-len

: del-buf"      ( -- a1 n1 )
                del-buf del-len 1- 0max
                BEGIN   2dup 0 scan dup
                WHILE   2swap 2drop
                        1 /string
                REPEAT  2drop
                dup 1+ negate del-len + 0max to del-len ;

: del-ov-check  ( -- )
                BEGIN   del-len del-max >       \ if the buffer is full
                WHILE                           \ then discard an old word
                        del-buf del-len 0 scan dup
                        IF      1 /string dup to del-len \ strip leading word
                                del-buf swap move        \ move remainder up
                        ELSE    0 to del-len
                                2drop
                        THEN
                REPEAT  ;

: "del-buf      ( a1 n1 -- )
                "LCLIP" >r del-buf del-len + dup r@ + off r@ move
                r> 1+ +to del-len 
                del-ov-check ;

: c+del-buf     ( c1 -- )
                del-buf del-len + dup off c!
                2 +to del-len
                del-ov-check ;

: stop-delete   ( -- )
                MAXCOUNTED c+del-buf ;

: delete-lines  { #lines \ cdiff -- }    \ delete n1 entire lines
                                                        \ clip to legal value
                #lines file-lines 1- cursor-line - 0max min to #lines
                #lines 1 < ?EXIT                        \ exit if none to del
                cursor-line file-lines 1- >= ?EXIT      \ leave if last line
                                                        \ or greater
                0 to cdiff                              \ init diff to none
                cursor-line #lines bounds
                DO      i #line" "del-buf
                        0x0D c+del-buf
                        i #line.bytes negate +to cdiff   \ accum total length
                LOOP
                cursor-line #lines + #line.addr         \ source
                cursor-line          #line.addr         \ destination
                end.addr
                cursor-line #lines + #line.addr - move  \ move rest of doc
                file-lines 2 + cursor-line #lines +
                ?DO     cdiff line-tbl i cells+ +!      \ adj line pointers
                LOOP
                line-tbl cursor-line #lines + cells+    \ then delete lines
                line-tbl cursor-line          cells+
                file-lines 2 + cursor-line #lines + - cells move
                file-lines #lines - 0max to file-lines  \ shorten by one line
                refresh-screen ;

: delete-characters { #chars -- }       \ delete a number of characters
                #chars 0max to #chars           \ at least zero characters
                #chars 0= ?EXIT
                get-cursor-line
                cur-buf @ cursor-col <=         \ deleting past end of line?
                cursor-line file-lines 1- <> and \ and not last file line?
                IF                              \ if so, then append next line
                        all-lines: DocWindow    \ update all lines in file
                        cursor-col cur-buf !    \ adjust line len to cursor
                                                \ append next line
                        cursor-line 1+ #line"   \ get next line
                        cur-buf +LPLACE         \ append to this line
                        1 delete-lines          \ delete current line
                        0x0D c+del-buf
                ELSE                            \ if not, then just del chars
                        cur-buf LCOUNT cursor-col /string
                        2dup #chars min "del-buf
                        >r
                        dup #chars + swap r> move
                        cursor-col cur-buf @ #chars + <
                        IF      cur-buf @ #chars - 0max cur-buf !
                        THEN
                THEN    put-cursor-line
                refresh-line ;

: _delete-character ( -- )
                get-cursor-line
                cur-buf LCOUNT cursor-col /string >r
                dup 1+ swap r> move
                cursor-col cur-buf @ 1+ <
                IF      cur-buf @ 1- 0max cur-buf !
                THEN    put-cursor-line ;

\ rls January 13th, 2001 - 4:37
: #line2"       ( n -- adr1 len1 len2 )
        dup>r #line" r> #line.bytes ;

\ rls January 13th, 2001 - 4:37
: Delete-Rect   { \ rectx recty dadr0 dadr sadr slen1 slen2 lend jadr -- }
        hced hcst - to rectx
        hled hlst - 1+ to recty
        hled 1+ #line.addr to jadr
        hlst #line.addr to dadr
        recty 0
        DO      dadr to dadr0
                I hlst + #line2" to slen2 to slen1 to sadr
                slen2 slen1 - to lend
                slen1 hcst <=
                IF      sadr dadr slen2 move
                        slen2 +to dadr
                ELSE    sadr slen1 hcst /split
                        dadr swap move
                        hcst +to dadr
                        rectx /string dup
                        IF      >r dadr r@ lend + move
                                r> lend + +to dadr
                        ELSE    dadr0 dadr over - -trailing + to dadr
                                sadr slen1 + dadr lend move
                                lend +to dadr
                        THEN
                THEN
        LOOP
        line-cur >r
        jadr dadr end.addr jadr - move
        text-length jadr dadr - - set-line-pointers
        set-longest-line
        r> to line-cur ;

\ rls January 13th, 2001 - 4:37
: delete-highlight ( -- )
        highlighting? 0= ?EXIT
        hcst to cursor-col
        hlst to cursor-line
        cursor-on-screen
        hled file-lines 1- >
        IF      hled 1- 0max to hled            \ set to end of file
                hled #line" nip to hced         \ and not beyond end
        THEN
        stop-delete
        hlst hled =
        IF      hced hcst - 0max delete-characters
        ELSE    all-lines: DocWindow
                RectHigh
                IF      Delete-Rect
                ELSE    cursor-col
                        IF      cursor-line #line" hcst /string nip ?dup
                                IF      delete-characters
                                        0x0D c+del-buf
                                THEN
                                0 to cursor-col         \ back to start of line
                                cursor-line 1+ file-lines 1- min to cursor-line
                                hled hlst 1+ - delete-lines
                                hced delete-characters  \ beginning of last line

                        ELSE    hled hlst -  delete-lines
                                hced delete-characters  \ beginning of last line
                        THEN
                THEN
        THEN
        cursor-line file-lines 1- >             \ if beyond end of file
        IF      file-lines 1- 0 >row-col        \ backup to file end
        THEN
        no-highlight
        file-has-changed ;

: _insert-character ( char -- )          \ insert a char into text line
                dup  bl   >=                            \ blank or greater
                over 0xFF <= and                        \ and tilda or less
        IF
                get-cursor-line                         \ make a hole
                cur-buf LCOUNT cursor-col /string >r dup 1+ r> move
                cur-buf LCOUNT drop cursor-col + c!      \ lay in character
                                                        \ bump and clip count
                cur-buf @ cursor-col max 1+ "LCLIP" cur-buf !
                put-cursor-line
                file-has-changed
                1 +col-cursor
        ELSE    drop
                beep
        THEN    ;

: insert-character ( char -- )
                browse?
                IF      drop
                        EXIT
                THEN
                delete-highlight
                _insert-character
                ?wrap-word
                refresh-line ;

: insert-string { sadr slen -- }
                browse? ?EXIT
                slen 0= ?EXIT
                get-cursor-line
                                                        \ make a hole
                cur-buf LCOUNT cursor-col /string >r dup slen + r> move
                sadr cur-buf CELL+ cursor-col + slen move  \ lay in string
                                                        \ adjust line length
                cur-buf @ cursor-col max slen + "LCLIP" cur-buf !
                put-cursor-line
                slen +col-cursor
                file-has-changed
                ?wrap-word
                refresh-line ;

: paste-date/time ( -- )
                bitImage? ?EXIT
                browse? ?EXIT
                delete-highlight
                get-local-time time-buf
                >month,day,year"        insert-string
                time&date
                2drop drop              \ discard year, month and day
                s"  - "                   insert-string \ a space
                (.)   insert-string s" :" insert-string \ hour
                2 .#" insert-string                     \ minute
                drop
                refresh-line ;

: insert-spaces ( n1 -- )
                spcs-max min spcs swap insert-string ;

: delete-character ( -- )
        bitImage? ?EXIT
        browse? ?EXIT
        highlighting?
        IF      delete-highlight
        ELSE    stop-delete
                get-cursor-line
                \ this is looking at the last character of the line
                cur-buf CELL+ cursor-col + 1- c@ bl <>  \ no bl before cursor?
                cursor-line #line.len cursor-col = and  \ and at line end?
                cursor-col 0> and                       \ and not in column zero
                IF      bl insert-character
                        \ look at character under cursor
                        cur-buf CELL+ cursor-col + c@ bl =      \ if on blank
                        IF     1 delete-characters             \ then delete it
                        THEN
                ELSE    1 delete-characters
                THEN
        THEN    file-has-changed ;

: _back-delete-character ( -- )
        bitImage? ?EXIT
        browse? ?EXIT
        highlighting?
        IF      delete-highlight
        ELSE    cursor-line 0>          \ if not on the first
                cursor-col  0> or       \ character of the file
                IF      stop-delete
                        cursor-col >r
                        -1 +col-cursor
                        0x08 c+del-buf
                                        \ if cursor is within the line
                        cursor-col cursor-line #line.len <
                        r> 0= or        \ or was on column zero
                        IF      get-cursor-line
                                cur-buf CELL+ cursor-col +
                                1- c@ bl <>             \ no bl before cursor?
                                cursor-line #line.len
                                cursor-col = and        \ and at line end?
                                cursor-col 0> and       \ and not in column zero
                                IF      bl insert-character
                                THEN
                                1 delete-characters
                                file-has-changed
                        THEN
                ELSE    beep
                THEN
        THEN    ;

' _back-delete-character is back-delete-character

: "-blanks"     ( a1 n1 -- a2 n2 )      \ remove leading and trailing blanks
                bl skip -trailing ;

: highlight"    ( -- a1 n1 )    \ return the highlighting for current line
                hlst hled =
                IF      hlst #line" drop hcst + hced hcst - 0max
                ELSE    hlst #line" swap hcst + swap hcst - 0max
                THEN    ;

: ?get-highlight ( -- )
                highlight" dup
                IF      2dup InsertString: findComboEdit
                        "CLIP" find-buf place
                ELSE    2drop
                THEN    ;

: home-line     ( -- )
                -trailing-blanks
                cursor-line #line" 2dup bl skip nip - nip
                dup cursor-col <
                left-margin 0> and
                IF      dup to left-margin
                        >col-cursor
                ELSE    drop
                        0 to left-margin
                        0 >col-cursor
                THEN    ReTitle: WinEdWindow ;

: end-line      ( -- )
                -trailing-blanks
                cursor-line #line.len >col-cursor
                ReTitle: WinEdWindow ;

: home-doc      ( -- )
                -trailing-blanks
                Home: DocWindow
                home-line home-line
                refresh-screen ;

: end-doc       ( -- )
                -trailing-blanks
                End: DocWindow
                end-line
                refresh-screen ;

: word-left     ( -- )
                cursor-col 0=
                IF      -1 +row-cursor
                        end-line
                        refreshAll: DocWindow
                ELSE    cursor-line #line" cursor-col 1- min tuck + swap
                        over c@ bl =
                        IF      bl -skip
                        THEN    bl -scan swap c@ bl = over or
                        IF      1+ dup cursor-col =
                                IF      1-
                                THEN
                        THEN    >col-cursor
                THEN    ReTitle: WinEdWindow ;

: word-right    ( -- )
                cursor-col cursor-line #line.len >=
                IF      1 +row-cursor
                        home-line
                        refreshAll: DocWindow
                ELSE    cursor-line #line" tuck cursor-col /string
                        over c@ bl <>
                        IF      bl scan
                        THEN    bl skip nip - >col-cursor
                THEN    ReTitle: WinEdWindow ;

: character-left ( -- )
                highlighting?
                IF      hlst hcst >row-col
                ELSE    -1 +col-cursor
                THEN    ;

: character-right ( -- )
                highlighting?
                IF      hled hced >row-col
                ELSE    1 +col-cursor
                THEN    ;

: highlight-right ( -- )
                1 +to hced
                refresh-line ;

: highlight-left ( -- )
                hcst 1- 0max to hcst 
                refresh-line ;

: highlight-up  ( -- )
                hlst 1- 0max to hlst
                all-lines: DocWindow        
                refresh-line ;

: highlight-down ( -- )
                hled 1+ file-lines 1- min to hled
                refresh-screen ;

: highlight-home-line ( -- )
                0 to hcst
                refresh-screen ;

: highlight-end-line ( -- )
                cursor-line #line.len to hced
                refresh-line ;

: highlight-whole-line ( -- )
                0 to hced
                cursor-line 1+ to hled
                refresh-line ;

: highlight-word ( -- )                \ highlight the current word under cursor
                cursor-line #line" nip cursor-col < ?EXIT
                cursor-line dup to hlst to hled
                hlst #line" dup cursor-col min to cursor-col
                2dup cursor-col nip
                BEGIN   2dup bl scan dup
                WHILE   2swap 2drop
                        bl skip
                REPEAT  2drop drop 2 pick - dup to hcst
                /string 2dup bl scan nip - hcst + to hced drop
                refresh-line ;

: word-delete   ( -- )
        bitImage? ?EXIT
        browse? ?EXIT
        highlighting?
        IF      delete-highlight
        ELSE    stop-delete
                cursor-line #line.len cursor-col <=  \ at or beyond line end?
                IF      delete-character
                        cur-buf LCOUNT cursor-col /string   \ is text remaining
                        IF      c@ bl =                     \ then on blank?
                        ELSE    drop FALSE                  \ else NO
                        THEN
                ELSE    true
                THEN
                IF      cursor-line #line" cursor-col /string 2dup
                        over c@ bl <>
                        IF      bl scan
                        THEN    bl skip nip - nip delete-characters
                THEN
        THEN
        file-has-changed ;

: line-delete   ( -- )
                bitImage? ?EXIT
                browse? ?EXIT
                highlighting?
                IF      delete-highlight
                ELSE    0 >col-cursor
                        stop-delete
                        1 delete-lines
                THEN    file-has-changed ;

: insert-tab    { \ hlsts hcsts hleds hceds -- }
                bitImage? ?EXIT
                browse? ?EXIT
                highlighting?
                IF      hlst to hlsts
                        hcst to hcsts
                        hled to hleds
                        hced to hceds
                        hced 0>                 \ if cursor isn't in col 0
                        IF      hled 1+         \ include last line
                        ELSE    hled            \ else don't
                        THEN    hlst
                        ?DO     i hcsts _>row-col
                                get-cursor-line
                                cur-buf @
                                IF      tab-size cursor-col tab-size 1 max mod -
                                        0max 0
                                        ?DO     bl _insert-character
                                        LOOP
                                THEN
                                refresh-line
                        LOOP
                        hceds 0>
                        IF      1 +to hceds             \ over one column
                        THEN
                        hleds hceds _>row-col
                        hlsts to hlst
                        hcsts to hcst
                        hleds to hled
                        hceds to hced
                        file-has-changed
                ELSE    tab-size cursor-col tab-size 1 max mod - 0max 0
                        ?DO     bl insert-character
                        LOOP
                THEN    ;

: back-tab      { \ hlsts hcsts hleds hceds -- }
        bitImage? ?EXIT
        browse? ?EXIT
        highlighting?
        IF      hlst to hlsts
                hcst to hcsts
                hled to hleds
                hced to hceds
                hced 0>                 \ if cursor isn't in col 0
                IF      hled 1+         \ include last line
                ELSE    hled            \ else don't
                THEN    hlst
                ?DO     i hcsts _>row-col
                        get-cursor-line
                        tab-size 0
                        ?DO     cur-buf CELL+ cursor-col +
                                c@ bl =                        \ blank at cursor
                                cursor-col cur-buf @ < and     \ & not at end
                                IF      _delete-character
                                THEN
                        LOOP
                        refresh-line
                LOOP

                hceds 1- 0max to hceds          \ back one column
                hleds hceds _>row-col
                hlsts to hlst
                hcsts to hcst
                hleds to hled
                hceds to hced
                file-has-changed
        ELSE    character-left
                cursor-col tab-size 1 max mod 0max 0
                ?DO     character-left
                LOOP
        THEN    ;

: do-backspace  ( -- )
        bitImage? ?EXIT
        browse?
        IF      ?get-highlight
                find-buf c@
                IF      back-find-text-again
                ELSE    -1 +row-scroll
                THEN
        ELSE    highlighting?
                IF      delete-highlight
                ELSE    ConsoleWindow DocWindow =
                        cursor-col 0= and
                        IF      beep
                        ELSE    back-delete-character
                        THEN
                THEN
        THEN    ;

named-new$ replace-buf

: replace-text-again ( -- )
                browse? ?EXIT
                highlighting?
                IF      busy? ?EXIT             \ leave if already busy
                        TRUE to busy?
                        delete-highlight
                        replace-buf count insert-string
                        refresh-line
                        FALSE to busy?          \ clear busy, so find will work
                        _find-text-again ?beep
                ELSE    beep
                THEN    ;

: replace-text-all ( -- )
                browse? ?EXIT
                highlighting?
                IF      busy? ?EXIT          \ leave if already busy
                        BEGIN   highlighting?
                        WHILE   replace-text-again
                        REPEAT
                ELSE    beep
                THEN    ;

:Object ReplaceTextDialog <Super  ModelessDialog

IDD_EDIT2_DIALOG forthdlg find-dialog-id constant template

int originX
int originY

:M ClassInit:   ( -- )
                ClassInit: super
                0 to originX
                0 to originY
                ;M
                
:M On_Init:     ( hWnd-focus -- f )
                originX originY or
                IF      originX originY SetWindowPos: self
                THEN
                s" Find - Replace Text"                 SetText: self
                find-buf    count IDD_EDIT_TEXT         SetDlgItemText: self
                replace-buf count IDD_EDIT2_TEXT        SetDlgItemText: self
                s" Search for:"   IDD_PROMPT_TEXT       SetDlgItemText: self
                s" Replace with:" IDD_PROMPT2_TEXT      SetDlgItemText: self
                s" Find"          IDOK                  SetDlgItemText: self
                s" Done"          IDCANCEL              SetDlgItemText: self
                s" Replace"       IDOK2                 SetDlgItemText: self
                s" Replace ALL"   IDOK3                 SetDlgItemText: self
                s" Case Sensitive Search" IDB_OPTION    SetDlgItemText: self
                CaseSensitive?            IDB_OPTION    CheckDlgButton: self
                1 ;M

:M GetTemplate: ( -- template )
                template
                ;M

:M ExWindowStyle: ( -- )
                ExWindowStyle: super
                Win32s? 0=      \ if not Win32s, then use Tool Window
                IF      WS_EX_TOOLWINDOW or
                THEN
                ;M

: get-dialog    ( -- )
                find-buf    1+ 254 IDD_EDIT_TEXT  GetDlgItemText: self
                find-buf    c!
                find-buf count InsertString: findComboEdit
                replace-buf 1+ 254 IDD_EDIT2_TEXT GetDlgItemText: self
                replace-buf c!
                IDB_OPTION IsDlgButtonChecked: self to CaseSensitive? ;

:M On_Command:  ( hCtrl code ID -- )
    CASE
        IDOK     OF     get-dialog 1 +to cursor-col       \ skip one character
                                   _find-text-again ?beep  ENDOF
        IDOK2    OF     get-dialog replace-text-again      ENDOF
        IDOK3    OF     get-dialog replace-text-all        ENDOF
        IDCANCEL OF     get-dialog DestroyWindow: self
                        SetFocus: DocWindow                ENDOF
                        false swap ( default result )
    ENDCASE ;M

 :M WM_CLOSE    ( -- )
                get-dialog
                GetWindowRect: self 2drop to originY to originX
                WM_CLOSE WM: Super
                ;M

;Object

: replace-text  ( -- )
        bitImage? ?EXIT
        browse?
        IF      find-buf WinEdWindow Start: FindTextDlg
                dup 2 = to CaseSensitive?
                IF      1 +to cursor-col       \ skip one character
                        _find-text-again ?beep
                THEN
        ELSE    WinEdWindow Start: ReplaceTextDialog
        THEN    ;

' replace-text is find-text

: find-text-highlight ( -- )
        bitImage? ?EXIT
        highlighting? 0=                \ nothing is highlighted
        cursor-col cursor-line or  and  \ not at start of file
        IF      highlight-word          \ try to highlight something
        THEN
        ?get-highlight
        find-buf c@                     \ if we have something
        IF      find-text-again         \ then try to find it
        ELSE    replace-text            \ else show find and replace dialog
        THEN    ;


\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ 15    Goto Line Dialog
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

NewEditDialog GotoDlg "Goto Line" "Line Number:" "Goto"  ""  ""

: goto-line     { \ line$ -- }  \ move cursor to line entered in dialog
                bitImage? ?EXIT
                MAXSTRING LocalAlloc: line$
                line$ off
                BEGIN   line$ WinEdWindow Start: GotoDlg
                        IF      line$ count number? nip
                                IF      0 swap 1- 0max to-find-line
                                        refresh-line
                                        TRUE
                                ELSE    drop FALSE
                                THEN
                        ELSE    TRUE
                        THEN
                UNTIL
                ReTitle: WinEdWindow ;


\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ 16    Text Copy Functions  
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

\ rls January 5th, 2001 - 10:44
: copy-text     { \ gblhndl gblptr hwidth scnt -- } \ Copy text to clipboard
        bitImage? ?EXIT
        highlighting? 0=                \ nothing is highlighted
        IF      highlight-word          \ try to highlight something
        THEN
        highlighting? 0=                \ if still not highlighting
        IF      beeper EXIT             \ then just beep and leave
        THEN
        wait-cursor             \ release/allocate line pointer table
        GetHandle: WinEdWindow call OpenClipboard 0=
        IF      beep EXIT
        THEN
        RectHigh
        IF
                hled file-lines 1- min to hled
                hced hcst - to hwidth
                hwidth 2 + hled hlst - 1+ *          \ size of buffer
                cell+                                \ total length needed
                GMEM_MOVEABLE GMEM_DDESHARE or       \ flags
                call GlobalAlloc to gblhndl          \ allocate a buffer
                gblhndl call GlobalLock              \ lock memory
                abs>rel to gblptr
                hled 1+ hlst                         \ highlighted lines
                ?DO
                        I #line" hcst /string        \ source to start col
                        dup hwidth <
                        IF                           \ need to append to source
                                dup to scnt          \ save source count
                                gblptr swap move     \ move source to buffer
                                scnt +to gblptr      \ bump destination ptr
                                MSPCS gblptr
                                hwidth scnt - move   \ copy spaces
                                hwidth scnt -
                                +to gblptr           \ bump destination ptr
                        ELSE                         \ we can just copy source
                                drop gblptr hwidth
                                move
                                hwidth +to gblptr    \ bump dest pointer
                        THEN
                        CRLF$ count gblptr swap move \ append a cr+lf
                        2 +to gblptr
                LOOP
                RectTerm gblptr !
                4 +to gblptr
        ELSE
                hlst #line" drop hcst +             \ start of data
                hled file-lines <
                IF      hled #line" hced min +      \ end of highlighted data
                ELSE    file-lines 1- #line" +      \ or end of file
                THEN    over -                      \ -- start len
                dup cell+                           \ total length needed
                GMEM_MOVEABLE GMEM_DDESHARE or      \ flags
                call GlobalAlloc to gblhndl         \ allocate a buffer
                                                    \ lock memory
                gblhndl call GlobalLock
                abs>rel to gblptr
                2dup gblptr swap move               \ fill in text
                nip gblptr + off                    \ null terminate
        THEN
        gblhndl call GlobalUnlock drop              \ unlock it, done
        call EmptyClipboard ?win-error              \ clear out the clipboard
                                                    \ pass to windows
        gblhndl CF_TEXT call SetClipboardData ?win-error
        call CloseClipboard ?win-error ;

: clear-text    ( -- )
                bitImage? ?EXIT
                browse? ?EXIT
                delete-highlight ;

: cut-text      ( -- )
                bitImage? ?EXIT
                copy-text
                clear-text ;

\ rls  January 8th, 2001 - 0:17
: clean-paste   ( -- )
        call CloseClipboard ?win-error
        update-mirrors
        all-lines: DocWindow
        refresh-screen ;

\ rls  January 8th, 2001 - 0:17
: Split+Move     { adr1 cnt1 adr2 cnt2 --  adr3 cnt3 }
        cnt1 cnt2 >=
        IF      adr1 cnt1 cnt2 /split adr2 swap move
        ELSE    adr1 adr2 cnt1 move
                mspcs adr2 cnt1 + cnt2 cnt1 - move      \ append spaces
                adr1 cnt1 + 0
        THEN ;

\ rls  January 8th, 2001 - 0:17
: RectPaste     { tadr tlen \ iadr idelt rectx recty dcnt -- }
      tadr tlen crlf$ count search 0= \ get end of first insert line
      IF      beep EXIT       THEN
      drop tadr - to rectx          \ save length of first line of insert source
      tlen rectx 2 + /mod to recty    \ save number of lines of insert source
      IF      beep EXIT       THEN    \ Verify insert is rectangular
      rectx cursor-col + 2 +
      recty * to idelt                \ get maximum insert size
      text-length tlen + start-text-size +
      to text-blen
      text-blen text-ptr realloc      \ adj buffer
      s" Failed to adjust the text buffer size"
      ?TerminateBox
      to text-ptr
      text-length set-line-pointers   \ pointer may have moved
      cursor-line #line.addr to iadr  \ place to start insert destination
      iadr dup idelt + end.addr iadr -
      move                            \ open up file for maximum possible insert
      recty 0
      DO
            I cursor-line + idelt #line+"
            dup cursor-col max rectx + to dcnt
            iadr cursor-col Split+Move      \ copy partial line from source
            I rectx 2 + * tadr +
            iadr cursor-col + rectx move    \ Move line from rectangular buffer
            iadr cursor-col + rectx +
            swap move                    \ Move remaining chars from source line
            iadr dcnt -trailing dup>r       \ drop trailing spaces
            + 0x0a0d swap w!                \ add crlf
            r> 2 + +to iadr                 \ update iadr
      LOOP
      cursor-line recty + #line.addr idelt +
      dup iadr over end.addr idelt + swap -
      move                                    \ move source back to fill hole
      iadr - idelt swap - text-length +
      set-line-pointers                       \ Update the line pointers
\      set-longest-line
      ;

\ rls  January 8th, 2001 - 0:17
: paste-text    { \ gblhndl gblptr tlen tadr iadr textlen rectmode -- }
        bitImage? ?EXIT
        browse? ?EXIT
        delete-highlight
        GetHandle: WinEdWindow call OpenClipboard 0=
        IF      beep clean-paste        EXIT    THEN
        0 to rectmode
        wait-cursor                        \ release/allocate line pointer table
        rectmode 0=
        IF                                 \ cursor may be to right of line end
                get-cursor-line
                cur-buf @ cursor-col max "LCLIP" cur-buf !      \ extend line
                put-cursor-line                                 \ if needed
        THEN
        CF_TEXT call GetClipboardData dup 0=
        IF      beep clean-paste        EXIT    THEN
        to gblhndl
        gblhndl call GlobalLock abs>rel to gblptr       \ lock memory
        gblptr 4000000 2dup 0 scan swap
        w@ RectTerm = to rectmode
        - to tlen to tadr
        tlen 0=
        IF      beep gblhndl call GlobalUnlock drop
                clean-paste       EXIT
        THEN
        text-length to textlen
        cursor-line #line.len cursor-col - >r           \ save relative col
        file-lines                         >r
        file-lines line-cur -              >r           \ save top line from end
        file-lines cursor-line -           >r           \ save line from end
        rectmode
        IF      tadr tlen RectPaste
        ELSE
                textlen tlen + start-text-size +
                to text-blen
                text-blen text-ptr realloc              \ adj buffer
                s" Failed to adjust the text buffer size"
                ?TerminateBox
                to text-ptr
                textlen set-line-pointers           \ pointer may have moved
                cursor-line #line.addr cursor-col +
                to iadr                             \ insert point
                iadr                                \ from
                iadr tlen +                         \ to
                end.addr iadr -                     \ remaining text len
                move                                \ make a hole for new text
                tadr iadr tlen move                 \ move in new text
                textlen tlen + set-line-pointers
        THEN
        set-longest-line
        file-lines r> - to cursor-line              \ restore cursor line
        file-lines r> - 0max                        \ lines from bottom of file
        file-lines r> -                             \ lines added to file
         - 0max                                     \ backup that count of lines
        cursor-line screen-rows 3 - - 0max          \ top line at least this
        max to line-cur                             \ use largest of the two
        cursor-line #line.len r> - to cursor-col    \ restore cursor col
        highlight-cursor
        file-has-changed                                \ mark as modified
        gblhndl call GlobalUnlock drop                  \ unlock it, done
        clean-paste ;

: temp-text     ( -- )          \ Copy text and tell Forth to paste and load
        highlighting? 0=                \ nothing is highlighted,
        IF                              \ then highlight entire line
                cursor-line dup to hlst to hled
                0 to hcst
                cursor-line #line.len to hced
        THEN
        copy-text
        0 WM_PASTELOAD win32forth-message ;

: change-to-pc  { \ inform? -- }
    >E-unminimize
    bitImage? ?EXIT
    lend-len 2 <    \ if unix, apple or BLOCK, then change to PC
    IF  file-lines 4000 >
        IF  s" Too long to convert to PC file in memory,\nuse Save MAC/UNIX file as PC\n\nPress a key.."
\ s" Too long to convert to PC file in memory,\nuse Save MAC/UNIX file as"+
\ +S"  PC\n\nPress a key.."
            "message beep key drop
            message-off
            EXIT
        THEN
        file-lines 1000 > dup to inform?
        IF  s" Converting to PC file format....\n\nPress ESC to Stop" "message
        THEN
        FALSE to search-aborted?
        0 to cursor-line
        FALSE to editAsBinary?
        text-length set-line-pointers
        file-lines 0
        ?DO i 1+ 15 and 0=
            inform? and
            IF      key?
                    IF      key K_ESC = to search-aborted?
                    THEN
                    search-aborted? ?LEAVE
                    16 +row-cursor
            THEN
            1 to lend-len i #line" "LCLIP"
            2 to lend-len i #line!
            1 to lend-len
        LOOP
        0x0D to lend-char       \ set line terminator info
        2    to lend-len
        message-off
    THEN ;

: change-to-apple  { \ inform? old-len old-char -- }
        >E-unminimize
        bitImage? ?EXIT
        lend-len  TO old-len
        lend-char TO old-char
        file-lines 1000 > dup to inform?
        IF      s" Converting to Apple file format....\n\nPress ESC to Stop"
                "message
        THEN
        FALSE to search-aborted?
        0 to cursor-line
        FALSE to editAsBinary?
        text-length set-line-pointers
        file-lines 0
        ?DO     i 1+ 15 and 0=
                inform? and
                IF      key?
                        IF      key K_ESC = to search-aborted?
                        THEN
                        search-aborted? ?LEAVE
                        16 +row-cursor
                THEN
                old-char to lend-char                 \ set line terminator info
                old-len  to lend-len i #line" "LCLIP"
                0x0D     to lend-char                 \ set line terminator info
                1        to lend-len i #line!
                old-char to lend-char                 \ set line terminator info
                old-len  to lend-len
        LOOP
        0x0D to lend-char       \ set line terminator info
        1    to lend-len
        message-off ;

: change-to-binary-toggle  { \ inform? old-len old-char -- }
        >E-unminimize
        bitImage? ?EXIT
        lend-len  TO old-len
        lend-char TO old-char
        file-lines 1000 > dup to inform?
        IF      s" Converting to Binary file format....\n\nPress ESC to Stop"
                "message
        THEN
        FALSE to search-aborted?
        0 to cursor-line
        lend-len 0=
        IF      FALSE to editAsBinary?
        ELSE    TRUE  to editAsBinary?
        THEN
        text-length set-line-pointers
        message-off
        update-mirrors
        all-lines: DocWindow
        refresh-screen ;


\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ 17    Select a file to edit and print  
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

: open-1-*-file ( -- )
        name-buf count "+open-text ;

: *-open-file   { sadr slen \ spath$ smask$ -- }
        MAXSTRING LocalAlloc: spath$
        MAXSTRING LocalAlloc: smask$
        mask-ptr >r
        path-ptr >r
        spath$ MAXSTRING erase
        smask$ MAXSTRING erase
        sadr slen "path-only"  spath$ place
        spath$ c@ 0=
        IF      current-dir$ count spath$ place
        THEN
        spath$ to path-ptr
        sadr slen "to-pathend" smask$ place
        smask$ to mask-ptr
        ['] open-1-*-file is process-1file
        ['] noop          is processing-1dir
        s" Opening Files..." "top-message
        1 seconds
        FALSE to search-aborted?
        do-files-process
        message-off
        r> to path-ptr
        r> to mask-ptr ;


\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ 18    Word count usage functions  
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

            20000 constant MAX-WORDS
NAME-MAX-CHARS 1+ constant WORD-BYTES
WORD-BYTES CELL + constant ENTRY-BYTES
0 value words-found

  ENTRY-BYTES MAX-WORDS 10 + * Pointer word-storage

word-storage value append-pointer

: "find-word+   ( a1 n1 -- a1 n1 f1)
                append-pointer word-storage
                ?DO     2dup i CELL+ count caps-compare 0=
                        IF      i INCR
                                FALSE
                                UNLOOP EXIT
                        THEN
   ENTRY-BYTES +LOOP    TRUE ;

: "save-word    ( a1 n1 -- )
                "find-word+        ( a1 n1 -- a1 n1 f1 )
                IF      1 +to words-found
                        words-found MAX-WORDS >=
                        IF      2drop
                                TRUE to search-aborted?
                        THEN
                        append-pointer CELL+ place
                        append-pointer INCR
                        ENTRY-BYTES +TO append-pointer
                ELSE    2drop
                THEN    ;

: count-file    { \ line$ -- }
        search-aborted? ?EXIT
        MAXSTRING LocalAlloc: line$
        1 loadline !                            \ reset line counter to zero
        line$ MAXSTRING erase                   \ clear buffer
        source 2>r              \ save source pointers
        BEGIN                                   \ get a line
                line$ 1+ MAXCOUNTED search-hndl win-read-line
                rot line$ c!
                                0= and
                search-aborted? 0= and
        WHILE
                line$ count (source) 2! >in off
                BEGIN   bl word dup c@
                        dup '\' =               \ ignore '\' and beyond
                        IF      drop FALSE
                        THEN
                WHILE   count NAME-MAX-CHARS min "save-word
                REPEAT  drop
                search-aborted? ?EXIT
                1 loadline +!
                loadline @ 15 and 0=
                IF      WINPAUSE
                THEN
        REPEAT
        2r> (source) 2! ;        \ restore source pointers

: count-1-file  { \ line$ -- }
                search-aborted? ?EXIT
                name-buf count "+open-text
                count-file
                close-text ;

: "save-counts   { adr len \ fileid -- }
                adr len r/w CREATE-FILE 0=
                IF      to fileid
                        append-pointer word-storage
                        ?DO     i @
                                0 (D.) 6 OVER - spcs swap fileid write-file drop
                                                          fileid write-file drop
                                                spcs 2    fileid write-file drop
                                i CELL+ count             fileid write-line drop
           ENTRY-BYTES +LOOP    fileid CLOSE-FILE drop
                ELSE    drop
                THEN    ;

: word-count-file { sadr slen \ spath$ smask$ -- }
                MAXSTRING LocalAlloc: spath$
                MAXSTRING LocalAlloc: smask$
                mask-ptr >r
                path-ptr >r
                spath$ MAXSTRING erase
                smask$ MAXSTRING erase
                sadr slen "path-only"  spath$ place
                spath$ c@ 0=
                IF      current-dir$ count spath$ place
                THEN
                spath$ to path-ptr
                sadr slen "to-pathend" smask$ place
                smask$ to mask-ptr
                ['] count-1-file is process-1file
                ['] noop          is processing-1dir
                1 seconds
                FALSE to search-aborted?
                word-storage TO append-pointer
                0 to words-found
                do-files-process
                r> to path-ptr
                r> to mask-ptr
                s" WORDCOUNTS.TXT" 2dup 2>r "save-counts
                2r> "+open-text ;

: cwords        ( <file> -- )
                s" *.f" word-count-file ;

Rectangle bitRect

: "open-bitmap  { openAdr len \ editHndl bitDC hbm hdcMem --  }
        \ open the bitmap, openAdr is null terminated, len is not used

        GetHandle: EDIT-WINDOW to editHndl
        editHndl call GetDC to bitDC            \ get the edit window's DC

        Heap> WinDC to imageDC                  \ create the Image DC
        0 call CreateCompatibleDC imageDC PutHandle: [ ]

        SM_CYSCREEN Call GetSystemMetrics  4 -  \ maximum size of screen
        SM_CXSCREEN Call GetSystemMetrics  4 -
        bitDC Call CreateCompatibleBitmap       \ create a compatible bitmap
        to vga-bitmap
                                                \ select it into the Image DC
        vga-bitmap imageDC SelectObject: [ ] drop
        bitDC editHndl call ReleaseDC drop      \ release the edit window's DC

        LR_LOADFROMFILE                         \ open the .BMP file
        LR_CREATEDIBSECTION or
        NULL
        NULL
        IMAGE_BITMAP
        openAdr rel>abs
        NULL
        Call LoadImage to hbm
        imageDC GetHandle: [ ]
        Call CreateCompatibleDC to hdcMem

        0 0                                     \ initialize the bit rectangle
        SM_CXSCREEN Call GetSystemMetrics 4 -
        SM_CYSCREEN Call GetSystemMetrics 4 -  SetRect: bitRect

        LTGRAY_BRUSH Call GetStockObject
        AddrOf: bitRect rel>abs imageDC GetHandle: [ ] call FillRect ?win-error

        hbm hdcMem Call SelectObject  drop

        SRCCOPY                                 \
        0 0                                     \ y,x origin
        hdcMem                                  \ from memory dc
        SM_CYSCREEN Call GetSystemMetrics  4 -  \ height of dest rect
        SM_CXSCREEN Call GetSystemMetrics  4 -  \ width of dest rect
        0 0                                     \ y,x dest
        imageDC GetHandle: [ ]                  \ to screen
        Call BitBlt ?win-error
        hdcMem Call DeleteDC ?win-error ;

FileOpenDialog ColorText "Load Color File" "Color Files (*.COL)|*.COL|All Files (*.*)|*.*|"

: load-colors   ( -- )
        GetHandle: WinEdWindow Start: ColorText dup c@        \ -- a1 n1
        IF      count open-keywords
        ELSE    drop beep                                       \ {BE}
        THEN    refresh-screen ;

FileNewDialog ViewText "View Text File" "Forth Files (*.F)|*.F|C Files (C,CPP,etc..)|*.C;*.CPP;*.H;*.MAK;*.RC|HTML Files (*.HTM,*.HTML)|*.HTM;*.HTML|Batch Files (*.BAT)|*.BAT|All Text Files|*.C;*.CPP;*.F;*.TXT;*.SEQ;*.H;*.LST;*.RC;*.MAK|All Files (*.*)|*.*|"
\ "Forth Files (*.F)|*.F|            ( Why must the above line be so long???? )
\ C Files (C,CPP,etc..)|*.C;*.CPP;*.H;*.MAK;*.RC|
\ HTML Files (*.HTM)|*.HTM|Batch Files (*.BAT)|*.BAT|
\ All Text Files|*.C;*.CPP;*.F;*.TXT;*.SEQ;*.H;*.LST;*.RC;*.MAK|
\ All Files (*.*)|*.*|"

FileSaveDialog SaveText "Save Text File" "Text Files|*.c;*.cpp;*.f;*.txt;*.seq;*.h;*.lst|All Files (*.*)|*.*|"
\ "Text Files|*.c;*.cpp;*.f;*.txt;*.seq;*.h;*.lst|All Files (*.*)|*.*|"

: "open-text    { nadr nlen \ textlen open$ create$ -- }
    GetHandle: DocWindow 0=                 \ if no window, make one
    IF  WinEdWindow Start: DocWindow        \ then startup child window
        Refresh: WinEdWindow
    THEN
    def-right-edge to right-edge            \ set default right edge
    nadr nlen '*' scan nip
    IF  nadr nlen *-open-file
        EXIT
    THEN
    URL$ OFF                                \ clear the URL buffer
    MAXSTRING localAlloc: open$
    MAXSTRING localAlloc: create$
    nadr nlen "CLIP" open$ place
    nadr unnamed-file? 0=                   \ if its a real file
    IF  open$ count "path-file drop open$ place
        open$ count r/o open-file           \ try to open original file
        IF  drop                            \ rda jp 01/27/2002  DROP added
            open$ ?defext
            open$ count r/o open-file       \ try to open original file
            IF      drop                    \ can't, then strip off path
                    nadr nlen "to-pathend"
                    "CLIP" open$ place
                                            \ search through the Forth path
                    open$ count "path-file
                    drop "CLIP" open$ place
                    open$ ?defext           \ make sure it has an extension
            ELSE    close-file drop         \ else close if we opened it
            THEN
        ELSE
            close-file drop                \ else close if we opened it
        THEN
    THEN
    wait-cursor                            \ release/allocate line pointer table
    open$ count r/o open-file 0=           \ -- string len handle flag
    IF  open$ count s" .BMP" caps-search nip nip        \ if it's a BMP
        open$ count s" .DIB" caps-search nip nip or     \ or a DIB
        IF  close-file drop                         \ don't open this way
            open$ count "CLIP" cur-filename place
            cur-filename +NULL                      \ need NULL termination
            cur-filename count "open-bitmap         \ for "open-bitmap
            text-ptr ?dup IF release THEN           \ make dummy empty file
            2 to textlen
            textlen start-text-size + to text-blen
            text-blen malloc to text-ptr
            crlf$ count text-ptr swap move  \ move in CRLF
            0x0D to lend-char               \ set line terminator info
            2    to lend-len
            FALSE to browse?
            browse-toggle
            TRUE to bitImage?
            Home: DocWindow
        ELSE                                    \ else open as text file
            FALSE to bitImage?
            FALSE to browse?
            Home: DocWindow
            >r                              \ save the file handle
            open$ count "CLIP" cur-filename place
                               cur-filename +NULL
                                            \ release/allocate the text buffer
            text-ptr ?dup IF release THEN
            r@ file-size 2drop to textlen

            textlen start-text-size +  to text-blen
            text-blen malloc to text-ptr
            textlen 200000 >                \ if greater than 200k bytes
            IF      s" Reading text file..." "message
            THEN                            \ read the file into memory
            text-ptr textlen r@ read-file drop
            to textlen
            \ if its and EXE file, THEN edit as a binary file
            open$ count s" .BLK" caps-search nip nip to editAsBinary?
            r> close-file drop
            addFileMenu
        THEN
    ELSE
        drop                                \ discard garbage handle
                                            \ we make a NEW FILE here
        nadr unnamed-file?
        auto-new? or                    \ automatically make a new file
                                        \ if it doesn't already exist
        IF  TRUE
        ELSE
            unnamed-file count RECURSE      \ recurse, make new
            nadr nlen "CLIP" open$ place
            open$ count "path-file drop open$ place
            open$ ?defext           \ make sure it has an extension
            z" Do you want to Create it?"
            s" Can't Open: " create$  place
            open$ count      create$ +place
                             create$ +NULL
            open$ count upper
            create$ 1+
            MB_OKCANCEL MB_ICONSTOP or
            WinEdMessageBox: WinEdWindow IDOK =
        THEN                            \ -- TRUE if making a new empty file
        IF  open$ count "CLIP" cur-filename place
            text-ptr ?dup IF release THEN
            2 to textlen
            textlen start-text-size + to text-blen
            text-blen malloc to text-ptr
            crlf$ count text-ptr swap move  \ move in CRLF
            0x0D to lend-char               \ set line terminator info
            2    to lend-len
            FALSE to browse?                \ reset browse mode
        ELSE
            unnamed-file count RECURSE      \ recurse, make new
            close-text                      \ canceled. close file
            EXIT                            \ and leave
        THEN
    THEN
    textlen set-line-pointers
    set-longest-line
    message-off                             \ clear message box
    0 #line" s" <HTML>" xsearch nip nip     \ detect HTML document
    IF  FALSE to browse?                    \ reset browse mode
        browse-toggle                       \ so we can set it again
    THEN
    false to edit-changed?
    home-doc
    Refresh: WinEdWindow
    Update: FilesList
    ReTitle: WinEdWindow
    ;

: "browse       ( a1 n1 -- )
        >E-unminimize
        "+open-text
        browse? 0=
        IF      browse-toggle
        THEN  ;

: _save-text    { \ save$ fhndl -- }    \ save the file in memory to a disk file
        bitImage?
        IF      save-bitmap-as
                EXIT            \ save as bitmap and leave
        THEN
       MAXSTRING LocalAlloc: save$
\ [rda] okay to save in browse mode (changes made in edit mode)
\       browse?
\       IF      beep
\               EXIT
\       THEN 
\ [jap] end of \ slashed lines brows mode quit woring
        edit-changed? 0= ?EXIT
        cur-filename c@ 0=
        IF      GetHandle: WinEdWindow Start: SaveText dup c@
                IF     \ dup   May 5th, 1998 tjz remove 'dup' per Bruno Gauthier
                        count "CLIP" cur-filename place
                ELSE    drop EXIT       \ leave if canceled save
                THEN
        THEN
        wait-cursor             \ release/allocate line pointer table
        cur-filename count r/w create-file 0=           \ -- handle flag
        IF      to fhndl                                \ save file handle
                first-line-saved #line.addr             \ from
                file-lines #line.addr over -            \ length
                dup 200000 >
                IF      s" Saving text file..." "message
                THEN
                as-pc?
                IF      2drop                           \ discard total params
                        file-lines first-line-saved     \ loop on all lines
                        ?DO     i #line"        save$  place
                                crlf$ count     save$ +place
                                save$ count fhndl write-file drop
                        LOOP    FALSE to as-pc?
                ELSE    fhndl write-file drop
                THEN
                fhndl close-file drop
                false to edit-changed?                  \ clear changed flag
                line-cur >r
                cursor-col >r
                cursor-line >r
                text-length dup>r start-text-size + to text-blen
                text-blen text-ptr realloc              \ adj buffer
                s" Failed to adjust the text buffer size"
                ?TerminateBox to text-ptr
                r> set-line-pointers
                set-longest-line
                update-mirrors
                r> to cursor-line
                r> to cursor-col
                r> to line-cur
                false to warned?
                message-off
        ELSE    drop beep
                z" Use SaveAs to save your file to a new drive or name"
                z" WARNING Failed to create file!"
                MB_OK WinEdMessageBox: WinEdWindow drop
        THEN
        SetFocus: DocWindow
        ReTitle: WinEdWindow ;

' _save-text is primitive-save-text

: save-text-as  ( -- )          \ save with a new name
        >E-unminimize
        bitImage?
        IF      save-bitmap-as
        ELSE    no-highlight
                GetHandle: WinEdWindow Start: SaveText dup c@
                IF      dup count "CLIP" cur-filename place
                        file-has-changed
                        _save-text
                        Update: FilesList       \ October 28th, 1997 tjz
                                                \ bug fix per Fredrich Prinz
                THEN    drop
        THEN    SetFocus: DocWindow ;

: save-text-as-pc ( -- )        \ SAVE AS a PC file
        >E-unminimize
        bitImage? ?EXIT
        TRUE to as-pc?
        save-text-as 
        false to edit-changed?
        cur-filename count "open-text
        sync-mirrors ;

: save-text-pc  ( -- )          \ save to same filename as a PC file
        >E-unminimize
        bitImage? ?EXIT
        TRUE to as-pc?
        no-highlight
        file-has-changed
        _save-text
        false to edit-changed?
        cur-filename count "open-text
        sync-mirrors ;

: do-save-text  ( -- )
        >E-unminimize
        bitImage?
        IF      save-bitmap-as
        ELSE    cur-filename count 2dup '.' scan nip - nip 0=   \ no file,
                cur-filename 1+ unnamed-file? or                \ or new file
                IF      save-text-as
                ELSE    _save-text
                THEN
        THEN    update-mirrors ;

' do-save-text is save-text

: save-all-text ( -- )          \ save all modified files
        >E-unminimize
        entry# >r
        entry-max 0
        DO      i to entry#             \ select the hyper file index
                edit-changed?
                IF
                        refresh-screen
                        save-text
                THEN
        LOOP
        r> to entry#
        refresh-screen
        ReTitle: WinEdWindow ;

: _warn-to-save ( -- )
        z" Can I save your edit changes now?\n\nThe edit buffer length will also be increased"
\ z" Can I save your edit changes now?\n\nThe edit buffer length will also be"+
\ +Z"  increased"
        z" WARNING You are Almost OUT of Buffer Space!"
        MB_YESNO WinEdMessageBox: WinEdWindow
        CASE
                IDYES     OF    save-text       ENDOF
                IDNO      OF                    ENDOF
        ENDCASE ;

' _warn-to-save is warn-to-save

: _must-save    ( -- )
   z" MUST save your edit changes now?\n\nThe edit buffer length will also be increased"
\ z" MUST save your edit changes now?\n\nThe edit buffer length will also be"+
\ +Z"  increased"
        z" WARNING Buffer Space EXAUSTED!"
        MB_YESNO WinEdMessageBox: WinEdWindow
        CASE
                IDYES     OF    save-text       ENDOF
                IDNO      OF    bye             ENDOF
        ENDCASE ;

' _must-save is must-save

: revert-text   ( -- )
        >E-unminimize
        false to edit-changed?
        cur-filename count "open-text
        sync-mirrors ;

: _?save-text   ( -- f1 )       \ return f1=true if we canceled
      cur-filename count "already-open# 2 <
      edit-changed? and
      IF    save-minutes 0>
            IF    save-text
                  FALSE
            ELSE  z" Save the changes to the current file?"
                  z" The Current File Has Been Modified!"
                  MB_YESNOCANCEL MB_ICONSTOP or
                  WinEdMessageBox: WinEdWindow   
                  CASE
                        IDYES     OF    save-text               FALSE ENDOF
                        IDNO      OF    false to edit-changed?  FALSE ENDOF
                        IDCANCEL  OF                            TRUE  ENDOF
                        FALSE swap
                  ENDCASE
            THEN
      ELSE  false to edit-changed?
            FALSE
      THEN ;

' _?save-text is ?save-text     \ link into deferred word

: ?save-new     ( -- f1 )
                cur-filename 1+ unnamed-file?
                IF      ?save-text
                ELSE    false
                THEN    ;


\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ 19    Save Bitmap File support  
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

FileSaveDialog SaveBitmap "Save Bitmap File" "Bitmap Files (*.BMP)|*.BMP|*.DIB|All Files (*.*)|*.*|"
\ "Save Bitmap File" "Bitmap Files (*.BMP)|*.BMP|*.DIB|All Files (*.*)|*.*|"

        4 constant sizeof(RGBQUAD)
       14 constant sizeof(BitmapFileHeader)
       40 constant sizeof(BitmapInfoHeader)

        0 constant biSize
        4 constant biWidth
        8 constant biHeight
       12 constant biPlanes
       14 constant biBitCount
       16 constant biCompression
       20 constant biSizeImage
       24 constant biXPelsPerMeter
       28 constant biYPelsPerMeter
       32 constant biClrUsed
       36 constant biClrImportant

: _save-bitmap-as { \ nBits pbmi lpBits hbm  hdcMem hfile nrgbquad BitmapFileHeader save$  -- }
\ { \ nBits pbmi lpBits hbm  hdcMem hfile nrgbquad BitmapFileHeader save$  -- }
        16 to nBits                     \ always saves 16bit color files
        14 LocalAlloc: BitmapFileHeader
        max-path    LocalAlloc: save$
        s" Save Bitmap File: "  save$ place
        nBits (.)               save$ +place
        s"  Bit"                save$ +place
        save$ count SetTitle: SaveBitmap
        GetHandle: WinEdWindow Start: SaveBitmap dup c@

        IF count save$ place

                sizeof(BitmapInfoHeader)  sizeof(RGBQUAD) 256 * + malloc to pbmi
                \ ******** DON'T DELETE FOLLOWING LINE: ***********************
                pbmi sizeof(BitmapInfoHeader) sizeof(RGBQUAD) 256 * + erase
        sizeof(BitmapInfoHeader)                pbmi biSize            +   !
        SCREEN-WIDTH                            pbmi biWidth           +   !
        SCREEN-HEIGHT                           pbmi biHeight          +   !
        1                                       pbmi biPlanes          +  w!
        nBits                                   pbmi biBitCount        +  w!
        nBits
         CASE
          1 OF BI_RGB    2 to nrgbquad    ENDOF
          4 OF BI_RLE4  16 to nrgbquad    ENDOF \ Could also be BI_RGB for
          8 OF BI_RLE8 256 to nrgbquad    ENDOF \ uncompressed format
         16 OF BI_RGB    0 to nrgbquad    ENDOF
         24 OF BI_RGB    0 to nrgbquad    ENDOF
         32 OF BI_RGB    0 to nrgbquad    ENDOF
         ENDCASE                                   pbmi biCompression     +   !

      \  0    pbmi biSizeImage       +   !       NOT NEEDED           (1)
      \  0    pbmi biXPelsPerMeter   +   !       SINCE
      \  0    pbmi biYPelsPerMeter   +   !       pbmi IS ERASED
      \  0    pbmi biClrUsed         +   !       ABOVE
      \  0    pbmi biClrImportant    +   !

        SCREEN-HEIGHT
        SCREEN-WIDTH
        imageDC GetHandle: [ ]
        Call CreateCompatibleBitmap to hbm

        imageDC GetHandle: [ ]
        Call CreateCompatibleDC to hdcMem
        hbm hdcMem Call SelectObject drop

        SRCCOPY                                   \
        0 0                                       \ y,x origin
        imageDC GetHandle: [ ]                    \ from screen dc
        SCREEN-HEIGHT                             \ height of dest rect
        SCREEN-WIDTH                              \ and width of dest rect
        0 0                                       \ y,x dest
        hdcMem                                    \ to memory dc
        Call BitBlt ?win-error                    \

        DIB_RGB_COLORS
        pbmi rel>abs
        NULL
        SCREEN-HEIGHT
        0
        hbm
        hdcMem
        Call GetDIBits 0= abort" 1st GetDIBits"
        pbmi biSizeImage + @ malloc rel>abs to lpBits
        lpBits abs>rel pbmi biSizeImage + @ erase

        DIB_RGB_COLORS
        pbmi rel>abs
        lpBits
        SCREEN-HEIGHT
        0
        hbm
        hdcMem
        Call GetDIBits 0= abort" 2nd GetDIBits"
        save$
        count
        GENERIC_READ GENERIC_WRITE or
        create-file abort" CreateFile Error"
        to hfile

        0x4d42 BitmapFileHeader     w!                        \ hdr.bfType

        sizeof(BitmapFileHeader)
        sizeof(BitmapInfoHeader) +
        nrgbquad sizeof(RGBQUAD) * +
        pbmi biSizeImage + @     +
               BitmapFileHeader 2 +  !                        \ hdr.bfSize
        0      BitmapFileHeader 6 + w!                        \ hdr.bfReserved1
        0      BitmapFileHeader 8 + w!                        \ hdr.bfReserved2
        sizeof(BitmapFileHeader)
        sizeof(BitmapInfoHeader) +
        nrgbquad sizeof(RGBQUAD) * +
               BitmapFileHeader 10 + !                        \ hdr.bfOffBits

        BitmapFileHeader
        sizeof(BitmapFileHeader)
        hfile write-file  drop

        pbmi
        sizeof(BitmapInfoHeader)
        nrgbquad sizeof(RGBQUAD) * +
        hfile write-file drop

        lpBits abs>rel
        pbmi biSizeImage + @
        hfile write-file drop

        hfile close-file drop

        hdcMem call DeleteDC ?win-error
        hbm call DeleteObject ?win-error

        lpBits abs>rel release
        pbmi release
     ELSE drop
     THEN       ;

' _save-bitmap-as is save-bitmap-as


\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ 20    Calculate printer font height  
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

: WinEd_calc_font_height ( --- points_high )
        83 printer-lpi / ;              \ rls February 6th, 2002 - 16:00

' WinEd_calc_font_height is calc_font_height


\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ 21    Print Text File support  
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

                                                \ rls February 5th, 2002 - 11:21
: print-text    { \ message$ fromline toline -- }
        bitImage? ?EXIT
        MAXSTRING localAlloc: message$
        printed-columns 9 *    to screen-width
        484                    to screen-height
        char-height     >r  12 to char-height
        char-width      >r   9 to char-width
        true PD_HIDEPRINTTOFILE lastpage
        start-scaled2   ( -- flag )
        print-flags PD_SELECTION and highlighting? and
        IF      hlst to fromline
                hled to toline
                s" Selection from File: "  message$ place
        ELSE    0 to fromline
                file-lines to toline
                s" File: "  message$  place
        THEN
        cur-filename count message$ +place
        message$ count UserMessage: ThePrinter
        #pages-up ?dup
        IF    2 =
              IF      two-page
              ELSE    four-page
              THEN
        THEN
        ( flag )
        IF
              s" Courier New" SetPrinterFont: ThePrinter
              toline fromline
              ?DO   i #line"
                    browse? \ if browsing, ignore HTML when printing
                    IF    BEGIN
                                2dup s" <" search
                          WHILE
                                2swap 2over nip - Type: ThePrinter
                                2dup '>' scan dup
                                IF    2swap 2drop
                                ELSE  2drop dup
                                      IF      over 1 Type: ThePrinter
                                      THEN
                                THEN  1 /string
                          REPEAT
                          2drop
                    ELSE
                          lend-len 0= \ if not TEXT file, then discard non ASCII
                          IF    dup
                                IF    i BLOCKLINE-SIZE * to displayingLine
                                      base @ >r hex
                                      tempLine off       \ clear the temp buffer
                                      displayingLine
                                      0 <# # # # # BL hold # # # # #>
                                      tempLine +place    \ 9 wide
                                      s"   | "
                                      tempLine +place    \ 4 wide
                                      2dup bounds
                                      ?DO     i c@ 0 <# # # BL hold #>
                                      tempLine +place  \ 3 * BLOCKLINE-SIZE wide
                                      LOOP
                                      SPCS BLOCKLINE-SIZE 3 * 14 +
                                      tempLine c@ - 0MAX tempLine +place
                                      tempLine +place
                                      tempLine count
                                      r> base !
                                THEN
                          THEN
                    THEN
                    Type: ThePrinter
                      Cr: ThePrinter
              LOOP
              print-scaled            \ print the results if single-page
              single-page             \ print the results if multi-page
        THEN
        r> to char-width
        r> to char-height
        FALSE to direct-print? ;    \ if we enable auto-print, use it only once.

: text-setup    ( -- )
        bitImage? ?EXIT
        GetHandle: WinEdWindow Setup: ThePrinter ;

: text-up-setup ( -- )
        bitImage? ?EXIT
        WinEdWindow Start: page-up-dialog to #pages-up
        SetFocus: DocWindow ;


\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ 22    Hyper Text linkage words  
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

map-handle hyper-hndl
named-new$ hyper-buf

: hyper-delete  ( -- )
        cur-filename count "already-open# 2 <   \ if open less than twice
        IF      text-ptr ?dup IF release 0 to text-ptr THEN
                line-tbl ?dup IF release 0 to line-tbl THEN
                bitImage?
                IF      imageDC GetHandle: [ ] Call DeleteDC ?win-error
                        FALSE to bitImage?
                        vga-bitmap call DeleteObject drop
                        0 to vga-bitmap
                        imageDC Dispose         \ destroy object
                        0 to imageDC            \ reset pointer
                THEN

        THEN
        entry-buffer                            \ buffer start
        entry-#bytes entry-max *                \ total buffer bytes
        entry-#bytes entry#    *                \ current hyper offset
        /string                                 \ remove leading bytes
        >r dup entry-#bytes + swap r> move      \ remove one file cell
        entry-buffer
        entry-#bytes entry-max * +              \ end of hyper buffer
        entry-#bytes erase ;                    \ clear last entry

: hyper-insert  ( -- )
        entry-buffer                            \ buffer start
        entry-#bytes entry-max *                \ total buffer bytes
        entry-#bytes entry#    *                \ current hyper offset
        /string                                 \ remove leading bytes
        over >r                                 \ save for later erase
                                                \ insert one file cell
        >r dup entry-#bytes + r> entry-#bytes - move
        r> entry-#bytes erase ;                 \ clear the inserted entry

: +hyper        ( n1 -- )       \ next hyper
        DocWindow EditWindow =
        IF      GetStack: EditWindow + 0max entry-max 1- min
                dup to entry# SetStack: EditWindow
        ELSE    drop    \ May 5th, 1998 tjz added from Bruno Gauthier
        THEN    ;

: hyper-open    ( -- )
                cur-filename c@ 0=              \ leave if no name specified
                IF      beep
                        EXIT
                THEN
\                update-mirrors
                refresh-screen
                ReTitle: WinEdWindow
                Refresh: WinEdWindow
                Refresh: FilesList ;

: "hyper-find   ( a1 n1 -- n2 ) \ look for filename a1,n1 in the file list,
                                \ return n2 the index of the desired file,
                                \ or -1 not found
                -1 -rot         \ default n2 if not found is -1
                entry# >r
                entry-max 0
                DO      i to entry#     \ select the hyper file index
                        2dup cur-filename count compare 0=
                        IF      rot drop i -rot
                                LEAVE
                        THEN
                lOop    2drop
                r> to entry# ;          \ restore entry#

: "already-open? { adr len \ open$ -- f1 } \ if already open, return ENTRY#
                                           \ if not     open, return -1
                MAXSTRING LocalAlloc: open$
                adr len open$ place
                open$ count "path-file drop open$ place
                open$ ?defext
                -1                              \ -1 marks not found
                entry# >r
                entry-max 0
                DO      i to entry#             \ select the hyper file index
                                                \ if they match
                        cur-filename count open$ count caps-compare 0=
                        cur-filename c@ 0<> and
                        IF      drop            \ discard the -1
                                i               \ return the ENTRY#
                                LEAVE
                        THEN
                LOOP
                r> to entry# ;  \ f1 = -1 if not found, else ENTRY# if found

: "already-open-switch? { adr len \ open$ -- f1 } \ if already open, switch, TRUE
        MAXSTRING LocalAlloc: open$
        adr len open$ place
        open$ count "path-file drop open$ place
        open$ ?defext
        FALSE entry#
        entry-max 0
        DO      i to entry#             \ select the hyper file index
                                        \ if they match
                cur-filename count open$ count caps-compare 0=
                cur-filename c@ 0<> and
                IF      2drop
                        i SetStack: DocWindow
                        hyper-open
                        TRUE entry#     \ return this entry#
                        LEAVE
                THEN
        LOOP    to entry# ; \ f1=true if found and the file was selected

0 value hyper-start

named-new$ hyper-string  
named-new$ prev-hyper-string

: _"hyper-link  { adr len adr2 len2 -- }
        >E-unminimize
        ?save-new ?EXIT
        cur-filename count hyper-string place
        cur-filename 1+ unnamed-file? 0=        \ if un-named file
        IF      1 +hyper                        \ next hyper
                hyper-insert
        THEN
        adr len -trailing 254 min 0max hyper-buf  place
        s"  "                          hyper-buf +place \ add a space
        hyper-buf count upper             \ for uniqueness
        adr2 len2 hyper-hndl open-map-file 0=
        IF      hyper-hndl >hfileAddress @
                hyper-hndl >hfileLength  @
                hyper-start
                IF      prev-hyper-string count hyper-string place
                THEN
                hyper-start /string     \ skip first part of file
                BEGIN                   \ does word match?
                        2dup >r hyper-buf count tuck compare
                        r> 0<> and      \ & there's file left
                WHILE   over c@ k_TAB = \ file lines marked with TAB
                        IF      2dup
                                2dup 0x0D scan nip - 1 /string
                                hyper-string place \ save flname
                        THEN
                        0x0D scan 2 /string     \ skip a line
                REPEAT  dup
                IF      hyper-string count prev-hyper-string place
                        hyper-string count "already-open? >r
                        hyper-string count "open-text
                        r> set-mirrored         \ mark as mirrored
                                                \ if already open
                        2dup 0x0D scan          \ move to line end
                                                \ len, skip next time
                        over 2 + hyper-hndl >hfileAddress @ -
                        to hyper-start          \ save for reentry
                        nip -                   \ parse line
                        bl scan bl skip number? \ get line#
                        2drop file-lines 1- min
                        1- 0max to cursor-line
                        0 to cursor-col
                        cursor-line find-top-margin - VPosition: DocWindow
                        no-highlight
                        RefreshCursor: DocWindow \ 05/25/98 added from Bruno Gauthier
                ELSE    2drop
                        beep    \ didn't find word in index
                        cur-filename 1+ unnamed-file? 0=
                        IF      hyper-delete
                                -1 +hyper
                        THEN
                THEN
                hyper-hndl close-map-file drop
        ELSE    beep            \ can't open index file
                cur-filename 1+ unnamed-file? 0=
                IF      hyper-delete
                        -1 +hyper
                THEN
        THEN    ;

: "hyper-link   ( a1 n1 -- )
 ( rbs )        s" WINED.NDX" Prepend<home>\ _"hyper-link ;

: "help-link    ( a1 n1 -- )
 ( rbs )        s" HELP.NDX" Prepend<home>\ _"hyper-link ;

: next-hyper-link ( -- )
                hyper-buf count "hyper-link ;

NewEditDialog  HyperTextDlg "Hyper Link to a Word" "Hyper Link to:" "Link" "" ""

: word-link     ( -- )          \ let user type in a word to link to
                hyper-buf count -trailing nip hyper-buf c!  \ no trailing bl's
                hyper-buf WinEdWindow start: HyperTextDlg
                IF      0 to hyper-start
                        next-hyper-link
                THEN    SetFocus: DocWindow ;

: hyper-link    ( -- )          \ link to the currently highlighted word
                bitImage? ?EXIT
                highlighting? 0=                \ something is highlighted
                IF      highlight-word
                THEN
                highlighting?
                hlst hled = and                 \ but only on one line
                IF      0 to hyper-start
                        highlight" "-blanks" "hyper-link
                ELSE    word-link               \ if nothing highlightable
                THEN    ;

: help-link     ( -- )          \ link to the currently highlighted word
                bitImage? ?EXIT  \ it is bound to Ctrl-F1   
                highlighting? 0=                \ something is highlighted
                IF      highlight-word
                THEN
                0 to hyper-start
                highlighting?
                hlst hled = and                 \ but only on one line
                IF      highlight" "-blanks" "help-link
                ELSE    s" THIS" "help-link     \ happens to appear very early
                                                \ in the hypertext index
                THEN
                ;

fload SRC\WINED\hyper.f

: hyper-edit    ( -- )
                >E-unminimize
                s" WINED.CFG" Prepend<home>\ "+open-text ;

: hyper-compile ( -- )
                build-index ;

: prev-link     ( -- )
                >E-unminimize
                ?save-new ?EXIT
                GetStack: EditWindow 0=
                IF      beep
                ELSE    update-mirrors
                        -1 +hyper
                        cur-filename c@
                        IF      hyper-open
                        ELSE    beep
                                1 +hyper
                        THEN
                THEN    ;

: next-link     ( -- )
                >E-unminimize
                ?save-new ?EXIT
                update-mirrors
                1 +hyper                \ next hyper file
                cur-filename c@         \ but only if it has a name in it
                IF      hyper-open
                ELSE    beep
                        -1 +hyper
                THEN    ;

: make-new-text ( -- )
                >E
                ?save-new ?EXIT
                cur-filename count hyper-string place
                unnamed-file count "+open-text
                TRUE to browse?
                browse-toggle ;

: new-text      ( -- )
                >E-unminimize
                make-new-text ;

: _close-text   ( -- )
                >E
                ?save-text ?EXIT
                hyper-delete
                -1 +hyper
                cur-filename c@ 0=              \ if there's no file to open
                IF      hyper-delete            \ then delete it too
                        cur-filename c@         \ and try again
                        IF      hyper-open      \ open a file if it's there
                        ELSE                    \ else just open a new file
                                unnamed-file count "open-text
                                TRUE to browse?
                                browse-toggle
                        THEN
                ELSE    hyper-open              \ else open preceeding
                THEN
                refresh-screen
                Update: FilesList
                ReTitle: WinEdWindow ;

' _close-text is close-text

: _"+open-text  { adr len -- }
                len 0= ?EXIT                    \ EXIT if NULL length file
                                                \ switch if already open & EXIT
                ?save-new ?EXIT                 \ EXIT if changed and canceled
                cur-filename count hyper-string place
                cur-filename 1+ unnamed-file? 0=  \ if not unnamed
                cur-filename c@ 0> and            \ and if already opened a file
                IF      1 +hyper                  \ bump to next file entry
                        hyper-insert              \ make room for a file
                THEN
                adr len "already-open? >r
                adr len "open-text
                r> set-mirrored ;                 \ mark as mirrored
                                                  \ if already open

' _"+open-text is "+open-text 

1 value FilterIndex

: open-text     { \ open$ -- }
      max-path LocalAlloc: open$
      >E-unminimize
      ?save-new ?EXIT
      FilterIndex
      GetHandle: WinEdWindow Start2: ViewText dup c@ \ -- a1 n1
      IF    count open$ place
            open$ count '.' scan nip 0=           \ if file contains no '.'
            IF      open$ count r/o open-file 0=  \ then try to open it
                    IF      close-file drop       \ if it opened, then close it
                            '.' open$ c+place     \ and append a decimal point
                    ELSE    drop               \ else let others append file ext
                    THEN
            THEN
            open$ count "+open-text
            open$ count 2dup upper "path-only" SetDir: ViewText
      ELSE  drop
            cur-filename count 2dup '.' scan nip - nip 0=
            IF      new-text
            THEN
      THEN
      SetFocus: EditWindow
      RefreshAll: EditWindow
      get-filter-index to FilterIndex
      ;

: "skip-URLbase" ( a1 n1 --- a2 n2 )  \ return a2 and count=n2 of URL after base
                                      \ scan past "HTTP://" to last '/' in URL
                s" //" search IF 2 /string THEN
                [char] / scan dup
                IF      1 /string
                        BEGIN   2dup [char] / scan ?dup
                        WHILE   2swap 2drop 1 /string
                        REPEAT  drop
                THEN    ;

: "URL-base"    ( a1 n1 -- a2 n2 )      \ return URL base
                2dup "skip-URLbase" nip - ;

LMAXSTRING Pointer URLcombined

: "build-URL"   { relURL relLen -- adr2 len2 }
        URL$    LCOUNT "URL-base"       URLcombined  LPLACE
        URLcombined LCOUNT + 1- c@ [char] / <>      \ make sure it ends in a '\'
        IF      [char] / URLcombined C+LPLACE
        THEN
        relURL relLen over c@ [char] / = IF 1 /string THEN  URLcombined +LPLACE
        URLcombined +LNULL
        URLcombined LCOUNT ;

#define INTERNET_OPEN_TYPE_PRECONFIG    0   // use registry configuration
#define INTERNET_OPEN_TYPE_DIRECT       1   // direct to net
#define INTERNET_OPEN_TYPE_PROXY        3   // via named proxy

#define INTERNET_FLAG_NO_CACHE_WRITE    0x04000000  // don't write this item to the cache
#define INTERNET_FLAG_DONT_CACHE        INTERNET_FLAG_NO_CACHE_WRITE
#define INTERNET_FLAG_MAKE_PERSISTENT   0x02000000  // make this item persistent in cache
#define INTERNET_FLAG_OFFLINE           0x01000000  // use offline semantics

0 value HAVEInternet?
0 value HINTERNETSESSION
0 value HINTERNETURL

: html-release  ( -- )                  \ release the session handle
        HAVEInternet? 0= ?EXIT
        s" InternetCloseHandle" [ also hidden ] "find-proc 0= ?EXIT drop [ previous ]
        HINTERNETSESSION Call InternetCloseHandle drop
        NULL to HINTERNETSESSION ;

UNLOAD-CHAIN CHAIN-ADD-BEFORE html-release       \ add to termination chain

VARIABLE NumberOfBytesRead
4000     CONSTANT HTMLSIZE
HTMLSIZE Pointer  HtmlBuf
0        value    HTMLcnt

: "HTML         { htmlAdr HtmlLen \ open$ html$ name$ fhndl -- }
      max-path   LocalAlloc: open$
      LMAXSTRING LocalAlloc: html$
      MAXSTRING  LocalAlloc: name$
      htmlAdr HtmlLen html$ LPLACE
                      html$ +LNULL
      >E-unminimize
      ?save-new ?EXIT
      s" InternetAttemptConnect"
      [ also hidden ] "find-proc 0= ?EXIT drop [ previous ]
      HAVEInternet? 0=
      IF    s" Interogating Internet..." "message
            0 Call InternetAttemptConnect dup 0= to HAVEInternet?
            IF    beep message-off EXIT
            THEN
      THEN
      HINTERNETSESSION 0=
      IF    s" Opening Connection..." "message
            NULL
            NULL
            NULL
            INTERNET_OPEN_TYPE_DIRECT
            z" WinEd" rel>abs
            Call InternetOpen dup to HINTERNETSESSION 0=
            IF    beep message-off EXIT
            THEN
      THEN
      s" Opening URL..." "message
      0                                          \ dwContext
      INTERNET_FLAG_DONT_CACHE                   \ dwFlags
      s" Accept: */*\n\n" swap        rel>abs    \ dwHeadersLength, lpszHeader
      html$ LCOUNT DROP rel>abs                  \ lpszUrl
      HINTERNETSESSION                           \ hInternetSession
      Call InternetOpenUrl dup to HINTERNETURL
      IF    s" WinEd."          name$  place
            HTMLcnt 0 <# # # # #> name$ +place
            1 +to HTMLcnt
            name$ count       delete-file drop
            name$ count   r/w create-file 0=
            IF    to fhndl
                  s" Reading URL..." "message
                  BEGIN NumberOfBytesRead       rel>abs
                        HTMLSIZE
                        HtmlBuf                 rel>abs
                        HINTERNETURL
                        Call InternetReadFile >r
                        HtmlBuf NumberOfBytesRead @ fhndl write-file drop
                        r> 0<>                          \ read succeeded
                        NumberOfBytesRead @ 0> and 0=   \ and more bytes to read
                  UNTIL
                  fhndl close-file drop
                  name$ count "+open-text
                  FALSE to browse?
                  browse-toggle
                  TRUE to from-web?                     \ mark as from the Web
                  html$ LCOUNT URL$ LPLACE              \ save the URL
                               URL$ +LNULL
            ELSE  beep
            THEN
            HINTERNETURL Call InternetCloseHandle drop
            NULL to HINTERNETURL
      ELSE  beep
      THEN
      message-off
      SetFocus: EditWindow
      RefreshAll: EditWindow 
      ReTitle: WinEdWindow ;

: "ViewWeb-Link ( a1 n1 -- )
        WinEd-web?                    \ user selected WinEd for web browsing
        ?control OR
        IF      "HTML
        ELSE    GetHandle: WinEdWindow "Web-Link
        THEN    ;

NewEditDialog OpenUrlDlg "Open URL" "URL Address:" "Open"  ""  ""

create url-buf MAXSTRING allot
       url-buf off

: open-html     ( -- )
                url-buf conhndl Start: OpenUrlDlg
                IF      url-buf count "HTML
                THEN    ;

: open-text-highlighted { \ highlight$ -- }
        bitImage? ?EXIT
        MAXSTRING LocalAlloc: highlight$
        highlighting? 0=                \ something is highlighted
        IF      highlight-word
        THEN
        highlighting?                   \ something is highlighted
        hlst hled = and                 \ but only on one line
        IF                              \ get the screen text
                highlight"  highlight$ place
                >E-unminimize
                ?save-new ?EXIT
                cur-filename count hyper-string place
                highlight$ count "-blanks" "+open-text
        ELSE    open-text
        THEN    ;

create commandline-string
        z," WinEd [</option> </option>...] [[<filename> <line> <col>]...]\n\n"
        +z,"     /B                             Startup in Browse mode\n"
        +z,"     /INDEX <filename>  Build hypertext index with 'filename.CFG'\n"
        +z,"     /Rnn                         Right margin = 'nn'\n"
        +z,"     /Xext                        Default file extension = 'ext'"

: show-command-help     ( -- )
        commandline-string
        >r
        MB_OK MB_TASKMODAL or
        z" WinEd Commandline Help" rel>abs
        r> rel>abs NULL call MessageBox drop ;

named-new$ file-to-edit$

: command-options ( -- )
        command-args off
        BEGIN   /parse-word
                dup  1+ c@ '/' =               \ either '/'
                over 1+ c@ '-' = or            \ or     '-'
                over 1+ c@ '?' = or            \ or     '?'
        WHILE   count 1 /string                \ remove leading char
                over dup c@ '?' =              \ help ONLY
                swap 1- c@ '?' = or
                IF      show-command-help
                        bye
                THEN
                2dup upper
                2dup s" INDEX" compare 0=      \ re-build hyper index
                IF     /parse-word count       \ pick up next word
                       2dup '.' scan nip -     \ remove trailing file extension
                       dup                     \ if name specified use it
    ( rbs )            IF      2dup     &WINED.CFG  place
    ( rbs )                    s" .CFG" &WINED.CFG +place
    ( rbs )                             &WINED.NDX  place
    ( rbs )                   s" .NDX"  &WINED.NDX +place
                       THEN
                       uninit-shared-memory             \ I'm not here
                       hyper-compile
                       BYE
                THEN
        \ just a dummy command to ignore the /IMAGE directive to the wrapper
                2dup s" IMAGE" compare 0=               \ in case started with
                                                        \ /IMAGE imagename
                IF     dup /string                      \ discard remainder
                       /parse-word drop
                THEN
                \ These should be last because they only look at the
                \ first character to decide if they match or not.
                2dup 1 min s" B" compare 0=
                IF      TRUE to start-browse?           \ browse mode switch
                        dup /string                     \ discard remainder
                THEN
                2dup 1 min s" F" compare 0=
                IF      TRUE to start-browse?
                        dup /string
                        0 word count command-args place \ remainder of line
                        command-args c@ 0=
                        IF      s"  " command-args place
                        THEN
                THEN
                2dup 1 min s" X" compare 0=
                IF
                \ added new clipping to DEFEXT$, for new max length of 16 chars
                        2dup 1 /string DEFEXTMAX min
                        defext$ place                   \ default file extension
                        dup /string                     \ discard remainder
                THEN
                2dup 1 min s" R" compare 0=             \ right margin
                IF      2dup 1 /string number? 2drop
                        dup 0<>
                        IF      low-right-edge max "LCLIP"
                        THEN
                        to right-edge
                        dup /string                     \ discard remainder
                THEN
                \ if command was not recognized, just display the help dialog
                nip
                IF      show-command-help
                        bye
                THEN
        REPEAT
        count file-to-edit$ place ;

: open-initial-file ( -- )
        EditWindow to DocWindow
        GetStack: EditWindow to entry#
        show-console?                           \ if Forth is displayed
        file-to-edit$ c@ 0= and                 \ and no file specified
        minimized-ConsoleWindow? 0= and         \ and Forth isn't minimized
        IF      open-previous? 0=               \ open the previous file?
                IF      make-new-text           \ no, create a new file
                ELSE    unminimize-EditWindow
                        open-previous           \ yes, open previous file
                THEN
        ELSE    unminimize-EditWindow
                file-to-edit$ c@                \ is there a file to open
                IF      file-to-edit$ count
                        bl skip over c@ '"' = IF 1 /string THEN
                        2dup '"' scan nip - "open-text
                        start-browse?
                        IF      FALSE to browse?
                                browse-toggle
                        THEN
                        BEGIN   /parse-word count ?dup
                        WHILE   number? nip
                                IF      1- 0max to cursor-line
                                        /parse-word count number? nip
                                        IF      1- 0max to cursor-col
                                        ELSE    drop
                                                pocket count "+open-text
                                        THEN
                                ELSE    drop
                                        pocket count "+open-text
                                THEN
                                start-browse?
                                IF      FALSE to browse?
                                        browse-toggle
                                THEN
                        REPEAT  drop
                ELSE                            \ no file to open
                        open-previous? 0=       \ open the previous file?
                        IF      new-text
                        ELSE    open-previous   \ yes, open previous file
                        THEN
                        start-browse?
                        IF      FALSE to browse?
                                browse-toggle
                        THEN
                THEN
        THEN ;

: split-line    ( -- )                          \ split line at cursor
        browse?
        IF      highlighting?                   \ if a word is highlighted
                IF      hyper-link              \ then link, else
                THEN    EXIT                    \ just EXIT
        THEN
        delete-highlight
        get-cursor-line
        auto-margin?
        IF      cur-buf LCOUNT 2dup bl skip nip -
                nip cursor-col min
                right-edge 0>
                IF      right-edge 10 - min
                THEN
                0max to left-margin
        ELSE    0 to left-margin
        THEN
        cur-buf @ >r                            \ save original length
        cur-buf LCOUNT cursor-col min dup>r
        -trailing nip cur-buf !                 \ shorten to cursor minus bl's
        1 insert-lines                          \ insert a new blank line
        put-cursor-line                         \ save the shortened line
        1 +to cursor-line                       \ move down to next line
        r> cur-buf !                            \ restore buf length
                                                \ move remainder to start
        cur-buf LCOUNT r> swap /string cur-buf LPLACE
        put-cursor-line                         \ save remainder in old line
        0 >col-cursor                           \ cursor to start of line
        left-margin insert-spaces
        cursor-line screen-rows  2 - - 0max
        line-cur max to line-cur
        file-has-changed
        refresh-screen ;

\ rls January 16th, 2001 - 17:36
: word-undelete ( -- )
        bitImage? ?EXIT
        browse? ?EXIT
        no-highlight
        del-buf" dup
        IF      BEGIN   2dup 1 =                \ one character long string
                        swap c@ 255 = and 0=    \ the stop undelete char
                        over 0> and             \ and have more than zero chars
                WHILE   over c@ 0x0D =
                        IF      2drop
                                split-line
                                0 >col-cursor
                                character-left
                        ELSE
                                over c@ 0x08 =
                                IF      2drop
                                        character-right
                                ELSE    cursor-col >r
                                        insert-string
                                        r> to cursor-col
                                THEN
                        THEN
                        del-buf"        \ get the next string
                REPEAT  2drop
        ELSE    2drop beep
        THEN
        refresh-line ;

: _?wrap-word   { \ original-col wrap-col -- }
    right-edge 0>                            \ right-edge=0 for no word wrap
    cursor-line #line.len right-edge >= AND
    IF   all-lines: DocWindow
         cursor-col to original-col          \ save original cursor position
         right-edge to cursor-col
         get-cursor-line
         cur-buf CELL+ cursor-col + 1- c@
         bl <>                               \ char before cursor not a blank?
         IF      word-left
         THEN
         cursor-col to wrap-col              \ save wrap position
         cur-buf CELL+ cursor-col + c@ bl =  \ char at cursor is a blank
                                             \ and some text remains on line
         cur-buf LCOUNT cursor-col /string
         bl skip nip 0<> and
         IF      word-right                  \ then skip forward to after blanks
         THEN
         split-line
         cursor-col cursor-line #line.len <  \ if not at or beyond end of line
         IF      end-line                    \ go to end of line
         THEN
         cursor-col right-edge <             \ only if word wasn't too long
         cursor-line 1+ #line.len 0<> and    \ and following line has text in it
         IF      word-delete
                 recurse
         THEN
         original-col wrap-col <
         IF                                 \ return to previous cursor position
                 cursor-line 1- original-col
         ELSE                                \ back to middle of wrap word
                 cursor-line original-col
                 wrap-col - left-margin +
         THEN
         >row-col
    THEN    ;

' _?wrap-word is ?wrap-word


\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ 23    Cause text command string to be interpreted.  
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

: rem-sendkey   ( c1 -- )          \ pass a key to a remote Forth
                WM_KEY win32forth-message ;

: "Forth        ( a1 n1 -- )
                TRUE to interpreting?
                bounds
                ?DO     i c@ rem-sendkey
                        WINPAUSE
                LOOP    0x0D rem-sendkey ;


\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ 24    Process a carraige return from user  
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

0 value cr-count
0 value quit-now?

: do-console-cr ( -- )
        GetStack: DocWindow to entry#
        auto-margin? >r
        FALSE to auto-margin?
        -trailing-blanks
        cursor-line #line.len >col-cursor      \ to end of line
        cursor-line #line"                     \ the line
        cursor-col min                         \ clip to cursor
        split-line                             \ split line, for output
        RefreshCursor: DocWindow
        HideCursor: DocWindow
        2dup 2dup '\' scan nip - s" bye" caps-search nip nip to quit-now?
        "Forth
        SetFocus: DocWindow
        ShowCursor: DocWindow
        r> to auto-margin?
        BEGIN   key?                           \ handle keyboard interpretation
                interpreting? OR
        WHILE   key?
                IF      key rem-sendkey        \ just send keys to Forth console
                THEN
        REPEAT  ;

: do-cr         ( -- )
        bitImage? ?EXIT
        browse?
        IF      beep
        ELSE    ConsoleWindow DocWindow =
                highlighting? 0= and
                IF      do-console-cr
                        1 +to cr-count
                ELSE    split-line
                THEN
        THEN    ;

: "CONSOLE      ( a1 n1 -- )
        >F                            \ select the Forth console window
        insert-string                 \ insert command in Console Window
        do-cr ; 

: console-text  ( -- )
        show-console?
        IF      entry# >r
                DocWindow >r
                ConsoleWindow to DocWindow
                GetStack: ConsoleWindow to entry#
                command-args c@
                IF      unminimize-ConsoleWindow
                THEN
                TRUE to auto-new?
                s" CONSOLE.TXT" "open-text
                sync-mirrors
                0 to right-edge
                FALSE to auto-new?
                0 VPosition: ConsoleWindow
                        End: ConsoleWindow
                refresh-screen
                r> to DocWindow
                r> to entry#
        THEN ;

: save-console  ( -- )
                entry-console to entry#         \ automatically save console
                edit-changed?                   \ if it changed
                IF      end-doc split-line
                        console-savelines 0<    \ are we saving all lines?
                        IF      0
                        ELSE    file-lines console-savelines - 0max
                        THEN    to first-line-saved
                        _save-text
                THEN    0 to first-line-saved ;

: do-goto-line  ( -- )
                ConsoleWindow DocWindow =
                IF     auto-margin? >r
                        FALSE to auto-margin?
                        split-line
                        r> to auto-margin?
                ELSE    goto-line
                THEN    ;

: upper-case    ( a1 n1 -- )
                swap rel>abs Call CharUpperBuff drop ;

: lower-case    ( a1 n1 -- )
                swap rel>abs Call CharLowerBuff drop ;

0 value upper-case?

: "upper/lower  ( a1 n1 -- )
                upper-case?
                IF      upper-case
                ELSE    lower-case
                THEN    ;

2variable prevhst
2variable prevhed

: highlight-case-toggle ( -- )
        bitImage? ?EXIT
        browse? ?EXIT
        highlighting? 0=                \ if nothing is highlighted
        IF      highlight-word          \ then try to highlight something
        THEN
        hlst hcst prevhst 2@ d=
        hled hced prevhed 2@ d= and 0=  \ if NOT the same highlight as last time
        IF                              \ then goto uppercase
                TRUE to upper-case?
                hlst hcst prevhst 2!
                hled hced prevhed 2!
        ELSE                            \ if the same, toggle case
                upper-case? 0= to upper-case?
        THEN
        hled 1+ hlst
        ?DO     i hlst =                \ first line
                IF      i hled =        \ and last line
                        IF      i #line"  hced min hcst /string "upper/lower
                        ELSE            \ to end of line
                                i #line" hcst /string "upper/lower
                        THEN
                ELSE    i hled =        \ last line
                        IF      i #line" hced min "upper/lower
                        ELSE            \ middle lines
                                i #line" "upper/lower
                        THEN
                THEN
        LOOP
        refresh-line
        file-has-changed ;


\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ 25    Search Files for text dialog Object  
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

0 value FindInWindow

\ in the form: [line-number(cell)][count(byte)][characters]

0x20000 value   maxList         \ start with a 128k search buffer
maxList Pointer sifList
      0 value   sifListLen
      0 value   total-files
      0 value   total-found
      0 value   maxName         \ longest filename length without the path
\ October 3rd, 2000 - 9:46 tjz
\ on my 256 megabyte machine, this upper limit of occurances to find works well
\ if you have less memory, then your computer will probably appear to mostly
\ come to a halt with memory allocation problems before 20000 occurances can be
\ found.  I have 750 megabytes of virtual memory allocated, and my machine
\ actually crashed the application when it allocated over 700 megabytes to the
\ search occurance list, while trying to find all occurances of spaces in all
\ files.  WinEd was setup to look for upto 40000 occurances, and had reached
\ about 36000, before it ran out of memory.  It appears that WindowsNT doesn't
\ handle allocation well when your buffers get to be larger than physical
\ memory.  What a suprise.
\ The moral of this story, is; Don't make the below number bigger.
19999 CONSTANT  MAXOCCURANCES

MAXSTRING 2 * cell+ CONSTANT sifSIZE

: >sifEntry     ( n1 -- a1 f1 ) \ locate address a1 of search in sfiles entry n1
                                \ return f1=TRUE if entry is valid, else FALSE
                sifSIZE * dup sifList +
                swap sifSIZE + sifListLen <= ;

: >sifLine#     ( a1 -- n1 )
                @ 1 - 0max ;

: >sifFile"     ( a1 -- a2 n1 )
                cell+ count -trailing ;

: >sifText"     ( a1 -- a2 n1 )
                MAXSTRING cell+ + count -trailing ;

: "AddFile      ( a1 n1 a2 n2 line -- ) \ add file a2,n2 to file list
        \ also add search line text a1,n1 to file list
        sifListLen maxList 990 - <
                               \ should never be less than about 1500 bytes free
        IF      dup>r sifList sifListLen + !           \ lay in line number
                "clip" 2dup
                2dup "to-pathend" nip maxName max
                to maxName           \ save longest filename length without path
                sifList sifListLen + cell+ place       \ lay in filename
                2swap "clip"
                sifList sifListLen + cell+
                MAXSTRING + place                      \ lay in search line text
                sifListLen sifSIZE + TO sifListLen
                r> "AddFile: FindInWindow
                1 +to total-found
                total-found 1000 >
                IF      total-found 15 and 0=
                        IF      total-found total-files  Total: FindInWindow
                                5 MS
                        THEN
                ELSE    total-found total-files  Total: FindInWindow
                THEN
                total-found MAXOCCURANCES >         \ stop if over MAXOCCURANCES
                IF      TRUE to search-aborted?
                THEN
        ELSE    2drop drop
                TRUE to search-aborted?
        THEN
        \ now resize search list if it is relatively near being full.
        \ perform this adjustment after adding, so that the addresses passed
        \ into "AddFile will be valid during the add.  A resize may
        \ invalidate the addresses.
        \ this fixes a bug, where a garbage file was inserted, when the
        \ search buffer was resized before the file was inserted.
        sifListLen maxList 2000 - >=
                                \ keep lots of free space at end of search list
        IF                      \ expand search buffer by 128k if needed
                maxList 0x20000 + ResizePointer sifList 0=
                IF      0x20000 +to maxList     \ readjust max list size
                THEN
        THEN    WINPAUSE ;

: search-1file  { \ line$ -- }  \ search this file for find string
        search-aborted? ?EXIT
        MAXSTRING LocalAlloc: line$
        1 +to total-files
        name-buf count 60 "file-clip" LineText: FindInWindow
        1 loadline !                            \ reset line counter to zero
        line$ MAXSTRING erase                   \ clear buffer
        BEGIN                                   \ get a line
                line$ 1+ MAXCOUNTED search-hndl win-read-line
                rot line$ c!
                                0= and
                search-aborted? 0= and
        WHILE   line$ count find-buf count xsearch nip nip
                IF      line$ count name-buf count loadline @ "addFile
                        search-aborted? ?EXIT
                        all-occur?   0= ?EXIT   \ leave, all done
                THEN    1 loadline +!
        REPEAT
        name-buf 0 LineText: FindInWindow ;

: searching-1dir  ( -- )
        s" Looking for files to search..." LineText: FindInWindow
        total-found total-files               Total: FindInWindow ;

: do-files-search ( -- )
        busy? ?EXIT          \ leave if already busy
        TRUE to busy?
        0 to total-files
        0 to total-found
        0 to sifListLen
        0 to maxName
        Reset: FindInWindow
        WINPAUSE                               \ refresh dialog
        ['] search-1file   is process-1file
        ['] searching-1dir is processing-1dir
        FALSE to search-aborted? 
        do-files-process
        Update: FindInWindow
        total-found MAXOCCURANCES >             \ stop if over MAXOCCURANCES
        IF      s" STOPPED, Search Results Buffer Full"
        ELSE    search-aborted?
                IF      s" Search Interrupted !"
                ELSE    total-found 0=
                        IF      s" No Matching Files Found"
                        ELSE    s" Done"
                        THEN
                THEN
                Update: FoundList
        THEN    LineText: FindInWindow
        0 SetFile: FindInWindow
        FALSE to busy? ;

0 value Selected?

: do-file-search-open ( -- )
        ?save-text ?EXIT
        ['] search-1file is process-1file
        Selected? 0=
        IF
                cur-filename 1+ unnamed-file? 0=
                IF      1 +hyper
                        hyper-insert
                THEN
                TRUE to Selected?
        THEN
        GetSelection: FindInWindow              \ selected item number
        >sifEntry                               \ -- a1 f1
        IF      dup >sifFile" "open-text        \ and open the file
                >sifLine# to cursor-line        \ then set line number
                0 to cursor-col
                sync-mirrors
                cursor-line find-top-margin 2 / - VPosition: DocWindow
                no-highlight
                hlst 1+ to hled
                refresh-screen
                Refresh: FilesList
        ELSE    drop                            \ discard address
                beep
        THEN    ;

load-dialog WINEDIT     \ load the dialogs for WinEd

:Object FindInFilesDlg  <Super  ModelessDialog

IDD_SEARCHINFILES WinEdit find-dialog-id constant template

MAXSTRING bytes siText
int originX
int originY

:M ClassInit:   ( -- )
                ClassInit: super
                self to FindInWindow
                0 to originX
                0 to originY
                ;M

:M GetTemplate: ( -- template )
                template
                ;M

:M ExWindowStyle: ( -- )
                ExWindowStyle: super
                Win32s? 0=      \ if not Win32s, then use Tool Window
                IF      WS_EX_TOOLWINDOW or
                THEN
                ;M

:M Total:       { found# total# \ message$ -- }
                64 localAlloc:  message$
                s" Found: "     message$  place
                found# 0 (d.)   message$ +place
                s"  in "        message$ +place
                total# 0 (d.)   message$ +place
                                message$ count ID_FILECOUNT SetDlgItemText: self
                ;M

:M LineText:    ( a1 n1 -- )
                ID_LINETEXT SetDlgItemText: self
                WINPAUSE                               \ force a screen update
                ;M

:M Reset:       ( -- )
                0 0 LB_RESETCONTENT ID_FILELIST SendDlgItemMessage: self drop
                total-found total-files Total: self
                Paint: self
                ;M

:M SetFile:     ( n1 -- )
                0 swap LB_SETCURSEL ID_FILELIST SendDlgItemMessage: self drop
                ;M

:M GetFile:     ( -- a1 n1 )
                siText dup rel>abs
                0 0 LB_GETCURSEL  ID_FILELIST SendDlgItemMessage: self ( -- n1 )
         ( n1 -- )  LB_GETTEXT    ID_FILELIST SendDlgItemMessage: self
                ;M

:M GetSelection: ( -- n1 )
                0 0 LB_GETCURSEL  ID_FILELIST SendDlgItemMessage: self
                ;M

:M "AddFile:    { adr len line \ msg$ -- }
                MAXSTRING localAlloc: msg$
                s"   "                  msg$    place
                line 0max 0 (d.)        msg$   +place   \ line number
                k_tab                   msg$  c+place   \ a tab
                adr len 50 "file-clip"  msg$   +place   \ filename
                                        msg$   +NULL
                                        msg$ 1+ rel>abs
                0 LB_ADDSTRING ID_FILELIST SendDlgItemMessage: self drop
                ;M

:M Update:      { \ msg$ -- }
        MAXSTRING localAlloc: msg$
        Gethandle: self
        IF      total-found 1000 >
                IF      originX 7 + originY 50 + SetOrigin: msg-window
                        s" Building List Window" MessageText: msg-window
                        FALSE OnTop: msg-window
                        Start: msg-window
                THEN
                0 0 LB_RESETCONTENT ID_FILELIST SendDlgItemMessage: self drop
                0                                               \ start at zero
                BEGIN   dup >sifEntry
                WHILE   s"     "                msg$   place
                        dup @ 0max 0 (d.)       msg$  +place    \ line number
                        K_TAB                   msg$ c+place    \ a tab
                        >sifFile"
                        50 "file-clip"          msg$  +place    \ filename
                                                msg$  +NULL
                                                msg$ 1+ rel>abs
                        0 LB_ADDSTRING ID_FILELIST SendDlgItemMessage: self drop
                        1+
                REPEAT  2DROP
                total-found 1000 >
                IF      message-off
                THEN
                total-found total-files Total: self
                Paint: self
        THEN
        ;M

:M On_Init:     ( hWnd-focus -- f )
                   Update: self
                originX originY or
                IF      originX originY SetWindowPos: self
                THEN
                0 SetFile: self
                path-ptr   count ID_DIRECTORY   SetDlgItemText: self
                mask-buf count ID_MASK          SetDlgItemText: self
                Find-buf count ID_SEARCHTEXT    SetDlgItemText: self
                CaseSensitive? ID_CASE          CheckDlgButton: self
                sub-dirs?      ID_SUBDIR        CheckDlgButton: self
                all-occur?     ID_ALLOCCUR      CheckDlgButton: self
                FALSE to Selected?
                FALSE to search-aborted?
                1 ;M

: save-search-list { \ lsthndl lsterr msg$ file$ -- }
      MAXSTRING localAlloc: msg$
      MAXSTRING localAlloc: file$
      Gethandle: self
      IF    total-found 1000 >
            IF    originX 7 + originY 50 + SetOrigin: msg-window
                  s" Saving Search List" MessageText: msg-window
                  FALSE OnTop: msg-window
                  Start: msg-window
            THEN
( rbbs )    s" Searchlist.txt" Prepend<home>\ r/w create-file 0=             
            IF    to lsthndl
                  0 to lsterr
                  file$ off
                  0                                           \ start at zero
                  BEGIN   dup >sifEntry                       \ -- a1 f1
                        lsterr 0= and                         \ and no error
                  WHILE dup >sifFile" file$ count compare     \ if file changed
                        IF    dup >sifFile"   file$  place    \ filename
                              SPCS 2          msg$   place    \ leading spaces
                              file$ count     msg$  +place
                              msg$ count lsthndl write-line lsterr or to lsterr
                        THEN
                        dup @ 0max 0 (d.)
                        8 over - SPCS swap 0max msg$   place  \ leading spaces
                                                msg$  +place  \ line number
                        SPCS 4                  msg$  +place  \ trailing spaces
                        >sifText"               msg$  +place  \ search line
                        msg$ count lsthndl write-line
                        lsterr or to lsterr
                        1+
                  REPEAT
                        2DROP
                  lsthndl close-file drop
            ELSE  drop beep
            THEN
            total-found 1000 >
            IF      message-off
            THEN
      THEN ;

: get-parameters ( -- )
        path-ptr 1+ MAXCOUNTED ID_DIRECTORY  GetDlgItemText: self path-ptr c!
        mask-buf 1+ MAXCOUNTED ID_MASK       GetDlgItemText: self mask-buf c!
        Find-buf 1+ MAXCOUNTED ID_SEARCHTEXT GetDlgItemText: self Find-buf c!
        Find-buf count InsertString: findComboEdit
        ID_CASE   IsDlgButtonChecked: self to CaseSensitive?
        ID_SUBDIR IsDlgButtonChecked: self to sub-dirs?
        ID_ALLOCCUR IsDlgButtonChecked: self to all-occur?
        GetWindowRect: self 2drop to originY to originX ;

: do-list-box   ( select_message -- )
                CASE
                LBN_DBLCLK OF   busy? to search-aborted?
                                get-parameters
                                0       end-dialog      ENDOF   \ done
             LBN_SELCHANGE OF   busy? to search-aborted?
                                get-parameters
                                do-file-search-open
                                                        ENDOF   \ select changed
                ENDCASE ;

:M On_Command:  ( hCtrl code ID -- )
        CASE
                IDOK        OF  busy?
                                IF      TRUE to search-aborted?
                                ELSE    get-parameters
                                        s" Stop !" IDOK SetDlgItemText: self
                                        do-files-search
                                        s" Search" IDOK SetDlgItemText: self
                                        FALSE to search-aborted?
                                THEN                            ENDOF   \ search
                IDCANCEL    OF  busy? to search-aborted?
                                get-parameters
                                DestroyWindow: self
                                SetFocus: DocWindow             ENDOF   \ done
                ID_FILELIST OF     do-list-box                  ENDOF
                IDB_SAVE    OF     save-search-list             ENDOF
                        false swap ( default result )
        ENDCASE ;M

:M WM_CLOSE     ( -- )
                busy? to search-aborted?
                get-parameters
                WM_CLOSE WM: Super
                ;M

;Object
  
: find-in-files ( -- )
        ?get-highlight
        >E-unminimize
        GetHandle: FindInFilesDlg       \ if window already open,
                                        \ then set search text
        IF      Find-buf count ID_SEARCHTEXT SetDlgItemText: FindInFilesDlg
        ELSE    WinEdWindow Start: FindInFilesDlg
        THEN    ;

: searchlist-edit    ( -- )
        >E-unminimize
( rbs) s" SEARCHLIST.TXT" Prepend<home>\ "+open-text ;

:Object TextSizeDlg  <Super  dialog

MAXSTRING bytes textfont$

IDD_SIZE WinEdit find-dialog-id constant template

:M ClassInit:   ( -- )
        ClassInit: super
        ;M

: "add-font     { hadr hlen lplf \ message$ -- }
      MAXSTRING localAlloc: message$
      hadr hlen message$ place
      lplf abs>rel to lplf
      lplf 27 + c@ 3 and 3 and FIXED_PITCH =
      IF    lplf 28 + LF_FACESIZE 2dup 0 scan nip - message$ +place
                                                    message$ +NULL
                                                    message$ 1+ rel>abs
            0 LB_ADDSTRING ID_TEXTFONT SendDlgItemMessage: self drop
            message$ count 1+ GetFaceName: vFont compare 0=
            IF    0 0    LB_GETCOUNT  ID_TEXTFONT SendDlgItemMessage: self 1-
                  0 swap LB_SETCURSEL ID_TEXTFONT SendDlgItemMessage: self drop
            THEN
      THEN ;

: add-1font     { lplf lptm dwType lpData -- int }
        dwType
        CASE
                DEVICE_FONTTYPE   OF s" Device Font: "   lplf "add-font ENDOF
                RASTER_FONTTYPE   OF s" Raster Font: "   lplf "add-font ENDOF
                TRUETYPE_FONTTYPE OF s" TrueType Font: " lplf "add-font ENDOF
        ENDCASE
        1 ;

4 callback GetFontsFunc add-1font

: highlight-cur-font { \ font$ -- }
                64 LocalAlloc: font$
                0 0    LB_GETCOUNT  ID_TEXTFONT SendDlgItemMessage: self
                0
                ?DO     font$ dup rel>abs
                        i LB_GETTEXT ID_TEXTFONT SendDlgItemMessage: self
                        ':' scan bl scan bl skip GetFaceName: vFont compare 0=
                        IF      0 i LB_SETCURSEL ID_TEXTFONT
                                SendDlgItemMessage: self drop
                                leave
                        THEN
                LOOP    ;

: add-fonts     ( -- )
        0 0 LB_RESETCONTENT ID_TEXTFONT SendDlgItemMessage: self drop
        GetDC: DocWindow >r
        0
        GetFontsFunc rel>abs
        0
        r@                              \ the DocWindow's DC
        Call EnumFonts drop
        r> ReleaseDC: DocWindow ;

:M GetFont:     { \ csel -- a1 n1 }
        textfont$ off
        0 0 LB_GETCURSEL  ID_TEXTFONT SendDlgItemMessage: self to csel
        csel 0<
        IF      s" Courier"
        ELSE    textfont$ rel>abs csel
                LB_GETTEXT ID_TEXTFONT SendDlgItemMessage: self
                \ -- a1 n1 skip two words
                textfont$ swap bl skip bl scan bl skip bl scan bl skip
        THEN
        ;M

create size-table 21 ,
        06008 , 07009 , 07012 , 08008 , 08010 , 08012 , 08014 , 08016 ,
        09012 , 09015 , 09018 , 10014 , 10016 , 10018 , 12015 , 12018 ,
        16024 , 32048 , 64072 , 72096 , 96128 ,

: add-size      { size# \ message$ pad$ -- }
        64 localAlloc: message$
        32 localAlloc: pad$
        pad 32 - pad$ 32 move                   \ save contents of PAD
        size# 1+ size-table +cells @ 1000 /mod  \ -- height width
        0 <# #s #>     message$  place
        s"  x "        message$ +place
        0 <# #s #>     message$ +place
                       message$ +NULL
                       message$ 1+ rel>abs
        0 LB_ADDSTRING ID_TEXTSIZE SendDlgItemMessage: self drop
        pad$ pad 32 - 32 move ;                 \ restore PAD

:M On_Init:     ( hWnd-focus -- f )
        0 0 LB_RESETCONTENT ID_TEXTSIZE SendDlgItemMessage: self drop
        size-table @ 0
        DO      i add-size
        LOOP
        size-table lcount NIP 0 \ May 4th, 1998 added NIP, adr not needed
        ?DO     size-table i 1+ cells+ @ 1000 mod char-height =
                IF      0 i LB_SETCURSEL ID_TEXTSIZE
                        SendDlgItemMessage: self drop
                        LEAVE
                THEN
        LOOP
        add-fonts
        highlight-cur-font
        1 ;M

:M GetTemplate: ( -- template )
                template
                ;M

:M GetFont/Size: ( -- )
        0 0 LB_GETCURSEL  ID_TEXTSIZE SendDlgItemMessage: self
        size-table @ over >
        IF      1+ size-table +cells @ 1000 /mod to char-width to char-height
                #fonts 0
                DO      font-list i cells+ @ >r
                        char-width              Width: [ r@ ]
                        char-height            Height: [ r@ ]
                        GetFont: self     SetFaceName: [ r@ ]
                                               Create: [ r> ]
                LOOP
                GetWindowDC:   self PutHandle: screenDC
                Handle: vFont    SelectObject: screenDC drop
                s" #" ( JUST A CHARACTER ) GetTextExtent: screenDC
                to the-height  to the-width
                GetHandle: screenDC ReleaseDC: self
                ReSize: WinEdWindow
                Paint: DocWindow
        ELSE    drop
        THEN
        ;M

: do-font/size-box   ( select_message -- )
        CASE
                LBN_DBLCLK    OF   GetFont/Size: self
                                   0 end-dialog         ENDOF \ dbl click
                LBN_SELCHANGE OF   GetFont/Size: self   ENDOF \ select font/size
        ENDCASE ;

:M On_Command:  ( hCtrl code ID -- f1 ) \ returns 0=cancel,
                                        \ returns 1=option-off
                                        \ returns 2=option-on
        CASE
                IDOK        OF     GetFont/Size: self
                                   0     end-dialog        ENDOF   \ set size
                IDCANCEL    OF     GetFont/Size: self
                                   0     end-dialog        ENDOF   \ cancel size
                ID_TEXTSIZE OF     do-font/size-box        ENDOF
                ID_TEXTFONT OF     do-font/size-box        ENDOF
                false swap ( default result )
        ENDCASE ;M

;Object

: text-size     ( -- )
                bitImage? ?EXIT
                WinEdWindow Start: TextSizeDlg drop
                SetFocus: DocWindow ;

named-new$ filter-save

: set-filter    { \ filter$ -- }
                MAXSTRING localAlloc: filter$
                defext$ count 2dup upper s" F" compare
                IF      defext$ count     filter$  place
                        s"  Files (*."    filter$ +place
                        defext$ count     filter$ +place
                        s" )|*."          filter$ +place
                        defext$ count     filter$ +place
                        s" |"             filter$ +place
                        filter-save count filter$ +place
                                          filter$  count
                ELSE    filter-save count
                THEN    SetFilter: ViewText ;

:Object OptionsDlg  <Super  dialog

IDD_OPTIONS WinEdit find-dialog-id constant template

:M ClassInit:   ( -- )
                ClassInit: super
                ;M

:M GetTemplate: ( -- template )         \ each dialog MUST have this method
                template
                ;M

:M On_Init:     ( hWnd-focus -- f )
        tool-bar? Floating: WinEdToolbar or
                                 IDB_TOOLBAR     CheckDlgButton: self
        min-tool-bar? 0=         IDB_FULL        CheckDlgButton: self
        save-find?               IDB_SAVEFIND    CheckDlgButton: self
        WinEd-web?               IDB_WEB         CheckDlgButton: self
        save-margin?             IDB_SAVEMARGIN  CheckDlgButton: self
        colorize?                IDB_COLORIZE    CheckDlgButton: self 
\        open-previous?           IDB_OPENPROMPT  CheckDlgButton: self
        page-lines?              IDB_PAGELINES   CheckDlgButton: self
        show-console?            IDB_CONSOLE     CheckDlgButton: self
        border?                  IDB_BORDER      CheckDlgButton: self
        recent-files?            IDB_RECENTFILES CheckDlgButton: self
        defext$ count            IDE_EXT         SetDlgItemText: self
        tab-size          0 (d.) IDE_TAB         SetDlgItemText: self
        save-minutes      0 (d.) IDE_MIN         SetDlgItemText: self
        printed-columns   0 (d.) IDE_COLS        SetDlgItemText: self
        console-savelines 0 (d.) IDE_CONLINES    SetDlgItemText: self
        printer-lpi       0 (d.) IDE_LPI         SetDlgItemText: self
        path-ptr count           IDE_PATH        SetDlgItemText: self
        def-right-edge    0 (d.) IDE_WRAP        SetDlgItemText: self
        1 ;M

: skip-number?  ( a1 n1 -- n2 f1 )      \ single precision number conversion
                                        \ discards leading & trailing blanks
        bl skip -trailing number? nip ;

: get-options   { \ number$ -- }
        64 localAlloc: number$
        \ added new clipping to DEFEXT$, for new max length of 16 chars
        defext$ 1+ DEFEXTMAX IDE_EXT  GetDlgItemText: self defext$ c!
        defext$ count '.' scan dup  \ remove any leading decimal point
        IF      1 /string DEFEXTMAX min defext$ place
                \ May 6th, 1998 - 17:53 tjz added 'ELSE 2DROP' per Bruno Gauthier
        ELSE    2drop
        THEN
        set-filter
        path-ptr 1+ MAXCOUNTED IDE_PATH  GetDlgItemText: self path-ptr c!
        path-ptr count + 1- c@ '\' =
        IF      path-ptr c@ 1- 0max path-ptr c!     \ remove trailing '\' char
        THEN
        number$ dup 6  IDE_WRAP  GetDlgItemText: self skip-number?
        IF      dup 0<>
                IF      low-right-edge max "LCLIP"
                THEN    dup     to right-edge
                                to def-right-edge
        ELSE    drop
        THEN
        number$ dup 6  IDE_TAB   GetDlgItemText: self skip-number?
        IF      0max 32 min to tab-size
                tab-size SetTabSize: ThePrinter
        ELSE    drop
        THEN
        number$ dup 6  IDE_MIN   GetDlgItemText: self skip-number?
        IF      0 max 60 min to save-minutes
        ELSE    drop
        THEN
        number$ dup 6  IDE_LPI   GetDlgItemText: self skip-number?
        IF      3 max 12 min to printer-lpi
        ELSE    drop
        THEN
        number$ dup 6  IDE_COLS   GetDlgItemText: self skip-number?
        IF      30 max 230 min to printed-columns
        ELSE    drop
        THEN
        number$ dup 10 IDE_CONLINES GetDlgItemText: self skip-number?
        IF      20000 umin
        ELSE    drop 500
        THEN    to console-savelines
        show-console? >R
        IDB_SAVEFIND    IsDlgButtonChecked: self 0<> to save-find?
        IDB_WEB         IsDlgButtonChecked: self 0<> to WinEd-web?
        IDB_SAVEMARGIN  IsDlgButtonChecked: self 0<> to save-margin?
        IDB_COLORIZE    IsDlgButtonChecked: self 0<> to colorize?
\        IDB_OPENPROMPT IsDlgButtonChecked: self 0<> to open-previous?   
        IDB_PAGELINES   IsDlgButtonChecked: self 0<> to page-lines?
        page-lines? 0= to print-extended-lines
        IDB_BORDER      IsDlgButtonChecked: self 0<> to border?
        IDB_RECENTFILES IsDlgButtonChecked: self 0<> to recent-files?
        IDB_CONSOLE    IsDlgButtonChecked: self 0<> to show-console?
        IDB_FULL       IsDlgButtonChecked: self 0= to min-tool-bar?
        min-tool-bar? 0=
        WinEdToolbar max-toolbar = XOR
        tool-bar? Floating: WinEdToolbar OR AND
        IF      SwitchToolBar: WinEdWindow
        THEN
        IDB_TOOLBAR    IsDlgButtonChecked: self 0<> ( -- f1 )
   ( f1 -- ) IF      StartToolBar: WinEdWindow
        ELSE    Floating: WinEdToolbar
                IF      TRUE to tool-bar?
                THEN    CloseToolBar: WinEdWindow
        THEN

        second-copy?     0=                     \ not the second copy of WinEd
        R> show-console? <> and                 \ and console state changed
        IF      show-console?                   \  we want the console
                IF      StartConsole: WinEdWindow
                ELSE    CloseConsole: WinEdWindow
                        0 WM_FORTHIO win32forth-message
                THEN
        ELSE    second-copy?                    \ clear console if second copy
                IF      FALSE to show-console?
                THEN
        THEN    ;

:M On_Command:  ( hCtrl code ID -- f1 )         \ returns 0=cancel
                                                \ returns 1=ok
        CASE
                IDOK     OF     get-options
                                1        end-dialog     ENDOF   \ set size
                IDCANCEL OF     0        end-dialog     ENDOF   \ cancel size
                false swap ( default result )
        ENDCASE ;M

;Object

: text-options  ( -- )
                WinEdWindow Start: OptionsDlg drop
                SetFocus: DocWindow
                refresh-screen ;


\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ 26    Various Higher Level Functons    
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

: reformat-paragraph ( -- )
        end-line
        BEGIN   cursor-line 1+ #line.len 0>             \ next line not blank
                cursor-line file-lines 1- < and         \ not on last line
        WHILE   RefreshOff: DocWindow
                word-delete
                end-line ?wrap-word                     \ to last column of line
                end-line ?wrap-word
                end-line ?wrap-word
                end-line ?wrap-word
                end-line
                RefreshOn: DocWindow
                refresh-screen
                WINPAUSE
        REPEAT  end-line ?wrap-word
        home-line ;

: reformat-text ( -- )   \ reformat the current paragraph to the current margins
        >E-unminimize
        bitImage? ?EXIT
        right-edge 0>   \ if wrap edge is not zero, then enable word wrap
        IF      reformat-paragraph
        ELSE
   s" Need to set Right Margin First\n\nOpening WinEd Preferences Dialog...\n"
            "message beep 2 seconds message-off
            text-options
        THEN    ;

: right-to-cursor ( -- )
        cursor-col low-right-edge max "LCLIP" to right-edge ;

: expand-tabs   { \ cntr -- }
        >E-unminimize
        bitImage? ?EXIT
        home-doc
        find-buf @ >r           \ save count and first three chars of FIND-BUF
        1 find-buf c!
        k_tab find-buf 1+ c!    \ init find buffer to a tab
        s" Expanding ALL TABs..." "top-message
        WINPAUSE
        RefreshOff: DocWindow
        TRUE
        BEGIN   _find-text-again and
        WHILE   1 +to cntr
                delete-highlight
                spcs tab-size cursor-col tab-size 1 max mod - 0max Insert-string
                0 >col-cursor
                0 to left-margin
                cntr 15 and 0=
                IF      RefreshOn: EditWindow
                        refresh-screen
                        WINPAUSE
                        key? 0=
                        RefreshOff: EditWindow
                ELSE    TRUE
                THEN
        REPEAT
         RefreshOn: DocWindow
        message-off
        r> find-buf !           \ restore the contents of FIND-BUF
        refresh-screen ;


\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ 27    Toggle Colorization project in the works                        ????
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

\ : toggle-color  ( -- )                  \ toggle colorization {BE}
\                colorize? 0= to colorize?
\                refresh-screen ;


\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ 28    Paragraph Sort Routines 
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

0 value sort-to
0 value sort-from
0 value sort-col

: sortline      { \ sort1$ sort2$ -- }
                LMAXSTRING LocalAlloc: sort1$
                LMAXSTRING LocalAlloc: sort2$
                cursor-line #line" sort1$ LPLACE
                sort1$ LCOUNT upper
                line-delete
                sort-from dup sort-to
                DO      i #line" sort2$ LPLACE
                        sort2$ LCOUNT upper
                        sort2$ LCOUNT sort-col /string
                        sort1$ LCOUNT sort-col /string  compare 0>
                        IF      drop i LEAVE
                        THEN
                LOOP    0 _>row-col
                word-undelete
                1 +to sort-from
                sort-from cursor-col _>row-col
                sort-from 15 and 0=
                IF      refresh-screen
                        WINPAUSE
                THEN    ;

: do-sort-paragraph ( -- )
                cursor-line #line.len 0=
                cursor-line file-lines 1- >= or
                IF      1 +row-scroll
                        EXIT
                THEN
                0 to left-margin        \ clear left margin to zero
                cursor-col  to sort-col
                cursor-line to sort-to
                1 +row-cursor                   \ move cursor down one line
                cursor-line to sort-from
                BEGIN   get-cursor-line
                        cursor-line #line.len 0>
                        cursor-line file-lines 1- < and
                        key? 0= and
                WHILE   sortline
                REPEAT
                sort-col to cursor-col
                sort-to cursor-col >row-col  ;

: sort-paragraph ( -- )         \ sort lines of a paragraph
        bitImage? ?EXIT
        browse? ?EXIT
z" Do you want to sort the lines of the current \n paragraph on the Cursor Column?"
        z" Sort Paragraph Lines"
        MB_OKCANCEL WinEdMessageBox: WinEdWindow IDOK =
        IF      >E-unminimize
                do-sort-paragraph
                key?
                IF      key drop
                THEN
                file-has-changed
        THEN
        refresh-screen ;


\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ 29    Debug a Forth application  
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

: stepbp        ( -- )          \ execute a single step
                0 WM_STEPBP win32forth-message ;

: nestbp        ( -- )          \ nest into definition
                0 WM_NESTBP win32forth-message ;

: unestbp       ( -- )          \ unnest from definition
                0 WM_UNESTBP win32forth-message ;

: contbp        ( -- )          \ run continuous till key press
                0 WM_CONTBP win32forth-message ;

: jumpbp        ( -- )          \ jump over branch
                0 WM_JUMPBP win32forth-message ;

: beginbp       ( -- )          \ stop at beginning again
                0 WM_BEGNBP win32forth-message ;

: herebp        ( -- )          \ stop here next time
                0 WM_HEREBP win32forth-message ;

: rstkbp        ( -- )          \ display return stack
                0 WM_RSTKBP win32forth-message ;

: donebp        ( -- )          \ done debugging
                0 WM_DONEBP win32forth-message ;

NewEditDialog InquireDlg "Inquire for Data Item" "Get Current Value for:" "Inquire" "" ""
\  "Inquire" "" ""
0 value inq-running?

: inquirebp     ( -- )          \ inquire for the value of a data item
        ed-ptr 0= ?EXIT         \ only if we have shared memory
        inq-running? 0=
        IF      TRUE to inq-running?
                TRUE                            \ default to show dialog
                ?control                        \ if contrl key, get screen
                IF      highlighting? 0=        \ if nothing highlighted
                        IF      highlight-word  \ try to highlight something
                        THEN
                        highlighting?           \ if something highlighted
                        hlst hled = and         \ but only on one line
                        IF      drop            \ discard TRUE flag
                                highlight"      \ get highlight
                                ed-inquire place
                                                \ check shift key
                                ?shift          \ -- f1 ;if TRUE show dialog
                        THEN
                THEN    \ -- f1         ;flag, should we display dialog
                IF      ed-inquire WinEdWindow Start: InquireDlg
                ELSE    TRUE
                THEN
                ed-inquire c@ 0> and    \ buffer is not empty, and
                                        \ flag from above is TRUE
                IF                      \ then send message to Win32Forth
                        0 WM_INQUIRE win32forth-message
                THEN
                FALSE to inq-running?
        ELSE    beep
        THEN    ;

0 value debug-buttons?

create prev-lines MAXSTRING 5 * allot
       prev-lines MAXSTRING 5 * blank

:Object DbgButtonsDlg  <Super  ModelessDialog

IDD_DEBUG WINEDIT find-dialog-id constant template

 int HexBase
Font dbFont

:M ClassInit:   ( -- )
                ClassInit: super
                FALSE to HexBase        \ FALSE = Decimal, TRUE = Hex
                 8                Width: dbFont
                14               Height: dbFont
                s" Courier" SetFaceName: dbFont         \ default to Courier
                ;M

:M GetTemplate: ( -- template )
                template
                ;M

:M ExWindowStyle: ( -- )
                ExWindowStyle: super
                Win32s? 0=      \ if not Win32s, then use Tool Window
                IF      WS_EX_TOOLWINDOW or
                THEN
                ;M

: "addstack     { adr len \ ztemp -- }
                MAXSTRING LocalAlloc: ztemp             \ allocate buffer
                ztemp MAXSTRING erase                   \ null fill buffer
                adr ztemp len MAXSTRING 1- min move     \ move text to buffer
                ztemp rel>abs
                0 LB_ADDSTRING IDL_STACK SendDlgItemMessage: self drop ;

: "addreturn    { adr len \ ztemp -- }
                MAXSTRING LocalAlloc: ztemp             \ allocate buffer
                ztemp MAXSTRING erase                   \ null fill buffer
                adr ztemp len MAXSTRING 1- min move     \ move text to buffer
                ztemp rel>abs
                0 LB_ADDSTRING IDL_RETURN SendDlgItemMessage: self drop ;

: n>"           ( n1 -- a1 n2 )
                base @ >r
                HexBase
                IF      HEX
                ELSE    DECIMAL
                THEN
                HexBase
                IF      0 <# #s s" 0x" "hold #>
                ELSE    s>d tuck dabs <# #s rot sign #>
                THEN
                r> base ! ;

:M ShowStack:   { \ temp$ -- }
                MAXSTRING LocalAlloc: temp$
                s" Debugging: " temp$  place
                ed-name count   temp$ +place
                temp$ count IDT_NAME SetDlgItemText: self drop
                0 0 LB_RESETCONTENT IDL_STACK SendDlgItemMessage: self drop
                ed-stack @ ?dup
                IF      dup 0<
                        IF      drop 11 0
                                ?DO     s" UnderFlow!" "addstack
                                LOOP
                        ELSE    10 min 1 swap
                                DO      ed-stack i cells+ @ n>" "addstack
                            -1 +LOOP
                        THEN
                ELSE    s" Empty" "addstack
                THEN
                0 0 LB_RESETCONTENT IDL_RETURN SendDlgItemMessage: self drop
                ed-return count bl skip
                bl scan bl skip                 \ skip "RETURN"
                bl scan bl skip                 \ skip "STACK[xx]"
                BEGIN   2dup bl scan 2dup 2>r nip - dup
                WHILE   "addreturn 2r> bl skip
                REPEAT  2drop 2r> 2drop ;M

: "adddebug     { adr len \ ztemp -- }
                MAXSTRING LocalAlloc: ztemp             \ allocate buffer
                ztemp MAXSTRING erase                   \ null fill buffer
                adr ztemp len MAXSTRING 1- min move     \ move text to buffer
                ztemp rel>abs
                0 LB_ADDSTRING IDL_WORDS SendDlgItemMessage: self drop ;

:M ShowDebug:   { -- }
                0 0 LB_RESETCONTENT IDL_WORDS SendDlgItemMessage: self drop
                prev-lines dup MAXSTRING + swap MAXSTRING 4 * move
                ed-dbgline count 2dup 0x0D scan 2dup 1 /string 2>r
                2drop
                2r> prev-lines MAXSTRING 4 * + place
                5 0
                ?DO     prev-lines i MAXSTRING * + count "adddebug
                LOOP
                ;M

: show-inquire  { \ temp$ -- }
                MAXSTRING LocalAlloc: temp$
                s"  " temp$ place               \ init to empty type
                ed-result @ 4 min 0
                ?DO     ed-result i 1+ cells+ @ n>" temp$ +place
                        s"  "                       temp$ +place
                LOOP    temp$ count IDT_RESULT SetDlgItemText: self ;

:M On_Init:     ( -- )
                On_Init: super
                HexBase
                IF      IDR_HEX
                ELSE    IDR_DECIMAL
                THEN    IDR_DECIMAL IDR_HEX CheckRadioButton: self
                Handle: dbFont 0=
                IF      Create: dbFont
                THEN
                Handle: dbFont
                IF      Handle: dbFont IDL_STACK  SetDlgItemFont: self
                        Handle: dbFont IDL_RETURN SetDlgItemFont: self
                        Handle: dbFont IDL_WORDS  SetDlgItemFont: self
                THEN
                ShowStack: self
                ;M
                                        
:M On_Command:  ( hCtrl code ID -- f1 ) \ returns 0=cancel
                CASE
                     IDB_STEP     OF stepbp                             ENDOF
                     IDB_NEST     OF nestbp                             ENDOF
                     IDB_UNEST    OF unestbp                            ENDOF
                     IDB_CONT     OF contbp                             ENDOF
                     IDB_JUMP     OF jumpbp                             ENDOF
                     IDB_PROC     OF beginbp                            ENDOF
                     IDB_HERE     OF herebp                             ENDOF
                     IDB_DONE     OF donebp                             ENDOF
                     IDB_INQUIRE  OF inquirebp show-inquire             ENDOF
                     IDR_HEX      OF TRUE  to HexBase ShowStack: self   ENDOF
                     IDR_DECIMAL  OF FALSE to HexBase ShowStack: self   ENDOF
                     IDCANCEL     OF FALSE to debug-buttons?
                                     Delete: dbFont
                                     DestroyWindow: self                ENDOF
                                     false swap ( default result )
                ENDCASE ;M

:M WM_CLOSE     ( -- )
                FALSE to debug-buttons?
                Delete: dbFont
                WM_CLOSE WM: Super
                ;M

;Object

: debug-buttons ( -- )
                WinEdWindow Start: DbgButtonsDlg
                TRUE to debug-buttons? ;

: zMessageBox   ( szString -- )
                z" Notice"
                MB_OK MB_ICONSTOP or
                WinEdMessageBox: WinEdWindow ;

MAXSTRING pointer debug-buf

NewEditDialog DebugDlg "Insert BreakPoint at Word" "BreakPoint at: ' [ vocabulary (sp) ]  word '" "Set" "" ""
\ [ vocabulary (sp) ]  word '" "Set" "" ""
: "debug-word   ( a1 n1 -- )
        ed-name place                   \ the name we want debugged
        ed-response off                 \ clear return result
        0 WM_SETBP win32forth-message
        ed-response @ 0=
        IF      z" Failed to set BreakPoint!" zMessageBox
        THEN
        ed-response @                   \ browse mode it BP is set
        IF      debug-buttons
        THEN    ;

: debug-word    ( -- )          \ set a breakpoint on a word
        highlighting?
        hlst hled = and                 \ but only on one line
        ?control and                    \ must have control key
        ?shift 0= and                   \ must not have shift key
        IF      highlight" "-blanks" "debug-word
        ELSE    debug-buf count -trailing nip debug-buf c!  \ no trailing bl's
                debug-buf WinEdWindow start: DebugDlg
                IF       debug-buf count "debug-word
                THEN
        THEN    ;

: debug-forth   ( -- )
        ed-ptr
        IF      ed-forth-count @ 0>  \ must be more than 1, since I count as one
        ELSE    FALSE
        THEN
        IF      debug-word
        ELSE
           z" Sorry, there is no instance of a\nWin32Forth application running!"
           zMessageBox
        THEN ;


\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ 30    Convert a decimal number to hex in the 0x00000000 format  
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

create hex-buf  32 allot

: make-hex      ( -- )
        bitImage? ?EXIT
        highlighting? 0=                \ something is highlighted
        IF      highlight-word
        THEN
        highlighting?
        hlst hled = and                              \ but only on one line
        IF      base @ >r                            \ save the base
                highlight" "-blanks"                 \ extract highlighted text
                2dup bl scan nip - 31 min            \ parse out first word
                hex-buf place
                FALSE   hex-buf count bounds
                ?DO     i c@     '0' '9' between
                        i c@ upc 'A' 'F' between or
                        i c@ upc 'X' = or 0= or      \ TRUE if not DECIMAL or HEX or X
                LOOP            \ -- f1=TRUE if not a good DECIMAL or HEX number
                IF      r> base !
                        beep                         \ bad number
                        EXIT
                THEN
                hex-buf 1+  c@     '0' =             \ if starts with "0x", then
                hex-buf 2 + c@ upc 'X' = and         \ already a HEX number
                IF      delete-highlight             \ delete old number
                        hex-buf count                \ make it DECIMAL
                        2 /string                    \ remove "0x" from start
                        HEX                          \ using HEX number base
                        number? 2drop                \ convert to binary
                        DECIMAL                      \ using DECIMAL number base
                ELSE    FALSE hex-buf count bounds   \ else make it HEX
                        ?DO     i c@ '0' '9' between 0= or
                        LOOP    \ -- f1=TRUE if not a good DECIMAL number
                        IF      r> base !
                                beep                 \ beep and EXIT
                                EXIT
                        THEN
                        delete-highlight             \ delete old number
                        DECIMAL                      \ using DECIMAL number base
                        hex-buf count number? 2drop  \ convert to binary
                        HEX                          \ using HEX number base
                        s" 0x" insert-string         \ preceed with "0x"
                THEN
                0 <# # # # # # # # #                 \ convert 8 digits
                   #>   insert-string                \ insert the new number
                word-left                            \ back to start of word
                r> base !                            \ restore the base
        ELSE    beep                                 \ nothing to convert
        THEN ;


\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ 31    Macro Functions  
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

also hidden

: edit-macro    ( -- )
                Gethandle: WinEdWindow Start: EditLog dup c@
                IF      cur-filename count hyper-string place
                        count "+open-text
                ELSE    drop
                THEN    SetFocus: DocWindow ;

: play-macro    ( -- )
                Gethandle: WinEdWindow Start: PlayLog dup c@
                IF      count "playkeys
                ELSE    drop
                THEN    SetFocus: DocWindow ;

: new-macro    ( -- )
                Gethandle: WinEdWindow Start: NewLog dup c@
                IF      count "new-log
                ELSE    drop
                THEN    SetFocus: DocWindow ;

10 to playrate  \ set the macro execution DELAY to small number for
                \ HIGH SPEED, normally set to 1000

previous

: clip-console-lines    ( -- )
        file-lines console-savelines 200 + >
        IF     cursor-line >r
                cursor-col >r
                0 to cursor-col
                100 to cursor-line
                0 to hlst 0 to mlst
                0 to hcst 0 to mcst
                cursor-line dup to hled #line" nip to hced
                delete-highlight
                r> to cursor-col
                r> 100 - to cursor-line
        THEN    ;


\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ 32    View  
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

: view-emit     ( -- )
                GetHandle: ConsoleWindow
                IF      DocWindow >r
                        entry# >r
                        ConsoleWindow to DocWindow
                        GetStack: DocWindow to entry#
                        ed-result @ insert-character
                        unminimize-ConsoleWindow
                        r> to entry#
                        r> to DocWindow
                THEN    ;

: view-type     ( -- )
        GetHandle: ConsoleWindow
        IF      DocWindow >r
                entry# >r
                ConsoleWindow to DocWindow
                GetStack: DocWindow to entry#
                ed-result count insert-string
                unminimize-ConsoleWindow
                r> to entry#
                r> to DocWindow
        THEN    ;

: view-crtab    ( -- )
        GetHandle: ConsoleWindow 0= ?EXIT
        DocWindow >r
        entry# >r
        ConsoleWindow to DocWindow
        GetStack: DocWindow to entry#
        auto-margin? >r
        FALSE to auto-margin?
        split-line
        ed-result @ insert-spaces
        unminimize-ConsoleWindow
        clip-console-lines
        ReTitle: WinEdWindow
        r> to auto-margin?
        r> to entry#
        r> to DocWindow ;

: view-?cr      ( -- )
        GetHandle: ConsoleWindow
        IF      DocWindow >r
                entry# >r
                ConsoleWindow to DocWindow
                GetStack: DocWindow to entry#
                auto-margin? >r
                FALSE to auto-margin?
                ed-result @ cursor-col + screen-cols 2 - >
                IF      split-line
                        unminimize-ConsoleWindow
                        cursor-line screen-rows line-cur + 2 - <
                        IF      Paint: DocWindow        \ paint if not scrolling
                        THEN
                        clip-console-lines
                        ReTitle: WinEdWindow
                THEN
                r> to auto-margin?
                r> to entry#
                r> to DocWindow
        THEN    ;

: view-col      ( -- )
                GetHandle: ConsoleWindow
                IF     DocWindow >r
                        entry# >r
                        ConsoleWindow to DocWindow
                        GetStack: DocWindow to entry#
                        auto-margin? >r
                        FALSE to auto-margin?
                        ed-result @  cursor-col - 0max ?dup
                        IF      insert-spaces
                        THEN
                        r> to auto-margin?
                        r> to entry#
                        r> to DocWindow
                THEN    ;

: view-gotoxy   ( -- )
        GetHandle: ConsoleWindow
        IF     DocWindow >r
                entry# >r
                ConsoleWindow to DocWindow
                GetStack: DocWindow to entry#
                ed-result @                             \ the new column
                screen-cols 2 - min                     \ clipped to screen
                ed-result CELL+ @                       \ the new row
                file-lines      min                     \ clipped to file lines
                to-find-line                            \ goto new location
                r> to entry#
                r> to DocWindow
        THEN    ;

: view-getxy    ( -- )
                GetHandle: ConsoleWindow
                IF      DocWindow >r
                        entry# >r
                        ConsoleWindow to DocWindow
                        GetStack: DocWindow to entry#
                        cursor-line     ed-result CELL+ !
                        cursor-col      ed-result !
                        r> to entry#
                        r> to DocWindow
                THEN    ;

: view-getcolrow ( -- cols rows )
                GetHandle: ConsoleWindow
                IF      DocWindow >r
                        entry# >r
                        ConsoleWindow to DocWindow
                        GetStack: DocWindow to entry#
                        screen-rows     ed-result CELL+ !
                        screen-cols 2 - ed-result !
                        r> to entry#
                        r> to DocWindow
                ELSE    0 0
                THEN    ;

: view-cls      ( -- )
                GetHandle: ConsoleWindow 0= ?EXIT
                DocWindow >r
                entry# >r
                ConsoleWindow to DocWindow
                GetStack: DocWindow to entry#
                auto-margin? >r
                FALSE to auto-margin?
                highlight-all
                delete-highlight
                unminimize-ConsoleWindow
                RefreshCursor: DocWindow
                r> to auto-margin?
                r> to entry#
                r> to DocWindow ;

also hidden

: view-key      ( -- c1 )
                GetHandle: ConsoleWindow
                IF      Paint: ConsoleWindow
                        ReTitle: WinEdWindow
                THEN    _mkey log-also ;

previous

: clear-screen  ( -- )
                GetHandle: ConsoleWindow 0= ?EXIT
                ConsoleWindow to DocWindow
                GetStack: DocWindow to entry#
                SetFocus: DocWindow
                view-cls ;


\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ 33    Select List Class  
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

:Class SelectListClass    <Super ListControl

int temp-entry#
int hidden?
MAXSTRING bytes lineBuf

:M ClassInit:   ( -- )
                ClassInit: super
                0     to temp-entry#
                FALSE to hidden?
                ;M

:M NextLine:    ( -- a1 n2 f1 )         \ override for other select-windows
                s" Empty" FALSE
                ;M

:M Select:      ( -- )                  \ override for other select windows
                ;M

: "AddLine      { adr len \ list$ -- }
                MAXSTRING LocalAlloc: list$
                adr len list$ place
                        list$ +NULL
                list$ 1+ rel>abs
                0 LB_ADDSTRING
                GetID: self SendDlgItemMessage: WinEdWindow drop
                ;

: selected-item ( -- n1 )       \ the currently selected item of list box
                0 0 LB_GETCURSEL
                GetID: self SendDlgItemMessage: WinEdWindow ; ( -- n1 )

:M GetLine:     { ndx -- a1 n1 }
                lineBuf 1+ rel>abs ndx LB_GETTEXT
                GetID: self SendDlgItemMessage: WinEdWindow drop
                0 ndx LB_GETTEXTLEN
                GetID: self SendDlgItemMessage: WinEdWindow lineBuf c!
                lineBuf count
                ;M

:M SetLine:     ( n1 -- )
                0max temp-entry# min
                0 swap LB_SETCURSEL
\ April 21st, 1998 - 16:14 tjz added 'drop'
                GetID: self SendDlgItemMessage: WinEdWindow drop
                ;M

: setTheFont    ( -- )
                0 Handle: sFont WM_SETFONT
                GetID: self SendDlgItemMessage: WinEdWindow drop ;

:M Update:      ( -- )
                hWnd
                hidden? 0= and
                IF      0 0 LB_RESETCONTENT
                        GetID: self SendDlgItemMessage: WinEdWindow drop
                        entry# >r
                        0 to temp-entry#
                        BEGIN   NextLine: [ self ]      \ -- a1 n1 f1
                        WHILE   "AddLine
                                1 +to temp-entry#
                        REPEAT
                        temp-entry# 0=
                        IF      "AddLine
                        ELSE    2drop
                        THEN
                        r> to entry#
                          SetFocus: EditWindow
                        RefreshAll: EditWindow
                THEN
                ;M

:M Refresh:     ( -- )
                hWnd
                hidden? 0= and
                IF      Paint: self
                THEN
                ;M

:M Hide:        ( f1 -- )
                dup hidden? <>
                IF      dup to hidden?
                        IF      SW_HIDE       Show: self
                        ELSE    SW_SHOWNORMAL Show: self
                        THEN
                        Update: self
                        Refresh: WinEdWindow
\ April 24th, 1998 - 9:12 tjz added following line
                ELSE    drop
                THEN
                ;M

:M Showing:     ( -- f1 )
                hidden? 0=
                ;M

:M Start:       ( -- )
                Start: Super
                setTheFont
                Update: self
                ;M

\ List boxes don't normally send WM_COMMAND messages to the listbox, but
\ in this case, the WM_COMMAND message is being reflected from the "parent".
\ See: WM_COMMAND message processing in the parent window

:M WM_COMMAND   { hwnd msg wparam lparam -- res }
                wparam HIWORD ( NOTIFY ) LBN_SELCHANGE =
                \ May 4th, 1998 tjz removed inappropriate 'and' after '=' above
                IF      Select: [ self ]
                        1
                ELSE    0
                THEN
                ;M

;Class

:Object files-list <Super SelectListClass

int last-entry

13 constant list-height

:M ClassInit:   ( -- )
                ClassInit: super
                -1 to last-entry
                ;M

:M Update:      ( -- )
                Update: Super
                0 entry# LB_SETCURSEL
                GetID: self SendDlgItemMessage: WinEdWindow drop
                ;M

:M NextLine:    ( -- a1 n2 f1 )
                temp-entry# to entry#
                cur-filename c@ 0=
                IF      unnamed-file count
                        FALSE
                ELSE    cur-filename count "to-pathend"
                        TRUE
                THEN    ;M

:M BInfo:       ( -- a1 )       \ return address of filename under cursor
                                \ in the files list box
        Vertical list-height / last-entry# 1- >
        IF      binfo off
        ELSE    entry# >r
                Vertical list-height / to entry#
                s"   File:  "                   binfo  place
                cur-filename count              binfo +place
                s"   \n  Line: "                binfo +place
                cursor-line 1+ 0 <# #s #>       binfo +place
                s"   "                          binfo +place
                s"   Column: "                  binfo +place
                cursor-col 1+ 0 <# #s #>        binfo +place
                s"   "                          binfo +place
                r> to entry#
        THEN    binfo
        ;M

:M On_MouseMove: ( h m w -- )
        Vertical list-height / dup
        last-entry <>                   \ if on a different filename
        IF      clear-info              \ then clear info message
                                        \ so it can redisplay it correctly
        THEN    to last-entry
        On_MouseMove: Super
        ;M

:M Select:      ( -- )
        clear-info
        selected-item SetStack: EditWindow
        hyper-open
        >E
        SetFocus: EditWindow
        ;M

:M Refresh:     ( -- )
        hWnd
        hidden? 0= and
        IF      GetStack: EditWindow SetLine: self
        THEN
        Refresh: Super
        ;M

;Object

Addr: files-list to FilesList
                        \ initialize the object pointer for forward references

:Object found-list <Super files-list

create fileEntry MAXSTRING allot
        fileEntry OFF

:M ClassInit:   ( -- )
        ClassInit: super
        FALSE SetAutoClose: self
        ;M

:M NextLine:    ( -- a1 n2 f1 )
        temp-entry# >sifEntry
        IF      dup >sifFile" "to-pathend"                      fileEntry place
                SPCS maxName 22 min fileEntry c@ - 0max 2 +     fileEntry +place
                    >sifText"                                   fileEntry +place
                fileEntry count                                 TRUE
        ELSE    drop
                s" No Found Text"                               FALSE
        THEN    ;M

:M BInfo:       ( -- a1 )       \ return address of filename under cursor
                                \ in the files list box
        0 0 LB_GETTOPINDEX
        GetID: self SendDlgItemMessage: WinEdWindow  \ -- n1 index of top item
        Vertical list-height / +                       \ add offset to list item
        >sifEntry                                      \ get entry address
        IF      s"   File:  "           binfo  place
                dup >sifFile"           binfo +place   \ and add to buffer
                s"   at Line: "         binfo +place
                dup >sifLine# (.)       binfo +place   \ then set line number
                s"   \n  Text: "        binfo +place
                >sifText"               binfo +place
                s"   "                  binfo +place
        ELSE    drop                                   \ discard address
                binfo off
        THEN    binfo
        ;M

:M Select:      ( -- )
        clear-info
        selected-item >sifEntry
        IF      dup >sifFile" "open-text        \ and open the file
                >sifLine# to cursor-line        \ then set line number
                sync-mirrors
                cursor-on-screen
                refresh-screen
                SetFocus: EditWindow
        ELSE    drop                            \ discard address
                beep
        THEN
        ;M

;Object

Addr: found-list to FoundList

create window-list              \ the list of subject windows
        FilesList    ,
        FoundList    ,
        0 ,

:Object SubjectList <Super ComboListControl

int hidden?
int curSubject

:M ClassInit:   ( -- )
        ClassInit: super
        0 to hidden?
        0 to curSubject
        ;M

:M InitSubject: ( window -- )
        to curSubject
        ;M

: SelectSubject { theWindow -- }
        curSubject
        IF      TRUE  Hide: curSubject
        THEN
        FALSE Hide: theWindow
        theWindow to curSubject ;

: addToList     ( z" -- )
        rel>abs
        0 CB_ADDSTRING
        GetID: self SendDlgItemMessage: WinEdWindow drop
        ;

:M Start:       ( Parent -- )
        Start: Super
        0 0 CB_RESETCONTENT
        GetID: self SendDlgItemMessage: WinEdWindow drop

        z" Open Files"    addToList
        z" Found Files"   addToList

\        z" Files Loaded"  addToList
\        z" Vocabularies"  addToList
\        z" Classes"       addToList
\        z" Deferred words" addToList
\        z" Chains"        addToList
\        z" Pointers"      addToList

        0 0 CB_SETCURSEL
        GetID: self SendDlgItemMessage: WinEdWindow drop
        0 edit-top drag-barH 1- 160 Move: self
        ;M

:M Select:      ( -- )
        0 0 CB_GETCURSEL
        GetID: self SendDlgItemMessage: WinEdWindow ( -- n1 )
        ( n1 -- ) window-list +cells @ SelectSubject
        ;M

:M Hide:        ( f1 -- )
        dup hidden? <>
        IF      dup to hidden?
                IF     0 to listHeight
                        SW_HIDE
                ELSE    listHeightDefault to listHeight
                        SW_SHOWNORMAL
                THEN    Show: self
\ April 24th, 1998 - 9:12 tjz added following line
        ELSE    drop
        THEN    Refresh: curSubject
        ;M

:M Showing:     ( -- f1 )
                hidden? 0=
                ;M

:M WM_COMMAND   ( hwnd msg wparam lparam -- res )
        over HIWORD ( NOTIFY ) CBN_SELCHANGE =
        IF      Select: self
                1
        ELSE    0
        THEN
        ;M

;Object

: close-all-text ( -- )
        EditWindow to DocWindow
        RefreshOff: EditWindow
        last-entry# 1+ 0
        ?DO     ?save-text ?LEAVE
                hyper-delete
                -1 +hyper
                cur-filename c@ 0=              \ if there's no file to open
                IF      hyper-delete            \ then delete it too
                        cur-filename c@         \ and try again
                        IF      hyper-open      \ open a file if it's there
                        ELSE                    \ else just open a new file
                                unnamed-file count "open-text
                                TRUE to browse?
                                browse-toggle
                        THEN
                ELSE    hyper-open              \ else open preceeding
                THEN
        LOOP
        RefreshOn: EditWindow
        RefreshAll: EditWindow ;

: load-forth-file { \ load$ file$ -- }
        MAXSTRING LocalAlloc: load$
        MAXSTRING LocalAlloc: file$
        Gethandle: WinEdWindow start: LoadForth dup c@
        IF      count file$ place
                show-console? 0=
                IF      ed-forth-count @ 0=  \ if no forth running, then start one
                        IF      StartUpForth
                                500 ms        \ wait if we had to start Forth
                        THEN
                ELSE    >f                    \ else select Forth console
                THEN
                s" foreground-console" "Forth \ make Forth the front application
                s" cd " load$ place
                file$ count "path-only" load$ +place
                load$ count "Forth            \ Make file directory active
                s" FLOAD "  load$  place
                file$ count load$ +place
                load$ count "Forth
        ELSE    drop
        THEN    ;

\ >>> Load the current active file from disk. No questions asked.

: load-active-file { \ load$ file$ -- }
        MAXSTRING LocalAlloc: load$
        MAXSTRING LocalAlloc: file$
        cur-filename count file$ place
        show-console? 0=
        IF      ed-forth-count @ 0=     \ if no forth running, then start one
                IF      StartUpForth
                        500 ms          \ wait if we had to start Forth
                THEN
        ELSE    >f                      \ else select Forth console
        THEN
        s" foreground-console" "Forth   \ make Forth the front application
        s" cd " load$ place
        file$ count "path-only" load$ +place
        load$ count "Forth              \ Make file directory active
        s" FLOAD " load$ place
        file$ count load$ +place
        load$ count "Forth ;            \ Compile the source file

\ >>> Save the current active file on disk, start Forth and compiles the

\ saved file and then load it. No questions asked.
: save-and-load ( - )     save-text  load-active-file  ;

named-new$ tempBuf

: SaveFindStrings ( -- )
        GetHandle: findComboEdit               \ only save if combo is open
        tool-bar? and
        IF      GetCount: findComboEdit
                10 min 0                       \ only save first 10 find strings
                ?DO     tempBuf i GetString: findComboEdit
                        tempBuf count  s" FindString "
                        2dup + 1- i '0' + swap c!     \ append the count 0-9
                        "SetDefault
                LOOP
        THEN    ;

: LoadFindStrings ( -- )
        GetHandle: findComboEdit                \ only save if combo is open
        tool-bar? and
        IF      10 0
                ?DO     s" FindString "
                        2dup + 1- 9 i - '0' + swap c!  \ append the count 0-9
                        "GetDefault dup
                        IF      2dup InsertString: findComboEdit
                                "CLIP" find-buf place
                        ELSE    2drop
                        THEN
                LOOP
        THEN    ;


\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ 34    Enhance the ToolBar class for WinEd  
\       We need a special class, because of the extra status information
\       that is displayed in the toolbar, with the nice box around it.
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

0 value EditingButton

:Class WVToolBar <Super ToolBar

int offsetX
int offsetY

:M Floating:    ( -- f1 )
        floatBar
        ;M

: show-edit-mode ( -- )
        tool-bar? 0= floatBar 0= and ?EXIT
        min-tool-bar? ?EXIT
        GetHandle: self 0= ?EXIT
        LTGRAY  SetBkColor: dc
        12 StartSize: self nip border-width 2* -
        floatBar
        IF 20 -
        THEN
        4 / 3 *
        swap 14 + swap 7 -
        \ -- x y
        2dup
        browse?
        IF  s" Browse     " BmarginColor                        \ browsing
            SetTextColor: dc TextOut: dc
        ELSE
            entry# GetStack: ConsoleWindow =
            IF  s" Forth      " BLACK                           \ console
                SetTextColor: dc TextOut: dc
            ELSE
                entry# GetStack: ConsoleWindow >                \ other windows
                IF  2drop
                ELSE
                    edit-changed?
                    IF  s" Modified           "          LTRED  \ modified
                    ELSE
                        lend-len 2 =
                        IF  s" Editing            "      CYAN   \ PC
                        ELSE
                            lend-len 0=
                            IF  s" Binary             "  BLUE   \ binary or block
                            ELSE
                                lend-len 1 =
                                IF  lend-char 0x0D =
                                    IF  s" Apple              " WHITE \ Macintosh
                                    ELSE
                                        s" Unix               " BLACK \ UNIX
                                    THEN
                                ELSE
                                    s" *UNKNOWN*          "  RED      \ error
                                THEN
                            THEN
                        THEN
                    THEN
                    SetTextColor: dc TextOut: dc
                THEN
            THEN
        THEN
        3 - swap 11 - swap
        2dup           MoveTo: dc
        2dup 114  0 d+ LineTo: dc
        2dup 114 22 d+ LineTo: dc
        2dup   0 22 d+ LineTo: dc
        2dup           LineTo: dc
        DkGray      LineColor: dc
        2dup   1 20 d+ MoveTo: dc
        2dup   1  1 d+ LineTo: dc
        2dup 113  1 d+ LineTo: dc

        White       LineColor: dc
        2dup 113  1 d+ MoveTo: dc
        2dup 113 21 d+ LineTo: dc
        2dup   1 21 d+ LineTo: dc
        2drop ;

:M EditMode:    ( -- )
                tool-bar?
                IF      get-dc
                        show-edit-mode
                        release-dc
                THEN
                ;M

: vline         { offset color -- }
                color                           LineColor: dc
                offset 13                          MoveTo: dc \ vert
                offset StartSize: self nip border-width 2* -
                floatBar
                IF 20 -
                THEN
                4 -                                LineTo: dc ; \ line 

:M On_Paint:    ( -- )
                On_Paint: Super
                 4 WHITE vline
                 6 BLACK vline
                 8 WHITE vline
                10 BLACK vline
                show-edit-mode
                ;M

:M Close:       ( -- )
                Close: Super
                floatBar
                IF      FALSE to floatBar
                        0 0 SetOrigin: self
                        StartToolBar: WinEdWindow
                        FALSE to floating-bar?
                THEN    Paint: self
                ;M

:M Float:       (  -- )
                floatBar 0=
                IF      CloseToolBar: WinEdWindow
                        TRUE to floatBar
                        WinEdWindow Start: self
                        min-tool-bar?
                        IF      StartPos: WinEdWindow 54 - SetOrigin: self
                        ELSE    StartPos: WinEdWindow 80 - SetOrigin: self
                        THEN
                        Update: self
                        TRUE to tool-bar?
                        LoadFindStrings
                        FALSE to tool-bar?
                        TRUE to floating-bar?
                        Paint: self
                ELSE    Floating: WinEdToolbar
                        IF      Close: self
                        THEN
                THEN
                ;M

;Class
             
|Class TinyButton <Super PictureButton

:M ClassInit:   ( n1 -- )
                ClassInit: Super
                7 to bwidth
                7 to bheight
                ;M

;Class


\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ 35    Define the ToolBar for the application  
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

WVToolBar max-Edit-Tool-Bar "WINED.BMP"  \ Close all instances of Forth
\ and then it willwork.  Also Fload from the proper directory
\ currently it is  win32for

max-Edit-Tool-Bar to max-toolbar
        3 HSpace
    22 TinyButton    Float: WinEdToolbar ;
        ButtonInfo"  Float ToolBar toggle "
        5 HSpace
     0 PictureButton                    new-text        ; \ new
        ButtonInfo"  New Text File  (Ctrl+N) "
     1 PictureButton                    open-text       ; \ open
        ButtonInfo"  Open Text File  (Ctrl+O) "
     2 PictureButton                    save-text       ; \ save
        ButtonInfo"  Save Text File  (Ctrl+S) "
    21 PictureButton                    save-all-text   ; \ save all files
        ButtonInfo"  Save All Changed Files "
     4 PictureButton                    close-text      ; \ close
        ButtonInfo"  Close Text File  (Ctrl+W) "

        8 HSpace
     3 PictureButton print-text  ;   \ print   ** not doing this any more -> TRUE to direct-print?
        ButtonInfo"  Print File  (Ctrl+P) "

        8 HSpace
     6 PictureButton                    cut-text        ; \ cut
        ButtonInfo"  Cut Text  (Ctrl+X) "
     7 PictureButton                    copy-text       ; \ copy
        ButtonInfo"  Copy Text  (Ctrl+C) "
     8 PictureButton                    paste-text      ; \ paste
        ButtonInfo"  Paste Text  (Ctrl+V) "

DefToolWidth DefToolSpacing + HSpace
        8 HSpace
        8 HSpace
    13 PictureButton     hyper-link      ; \ hyper link
        ButtonInfo"  Hyper Link to: Highlighted Word  (Ctrl+Shft+F9) "
    16 PictureButton next-hyper-link RefreshCursor: DocWindow ShowCursor: DocWindow ;
        ButtonInfo"  Hyper Link to: Next Occurance of Word  (Ctrl+F9) "
        8 HSpace
    14 PictureButton prev-link RefreshCursor: DocWindow ShowCursor: DocWindow ;
        ButtonInfo"  Previous File or  (Ctrl+PgUp) \n Previous Hyper Link "
    15 PictureButton next-link RefreshCursor: DocWindow ShowCursor: DocWindow ;
        ButtonInfo"  Next File or  (Ctrl+PgDn) \n Next Hyper Link "

        8 HSpace
    20 PictureButton     debug-word      ; \ Debug Forth
        ButtonInfo"  Set a Break Point (Ctrl+B) \n in Win32Forth "
    19 PictureButton                    text-options    ; \ Preferences
        ButtonInfo"  WinEd Preferences  (Ctrl+Shft+P) "

        8 HSpace
     5 PictureButton   ( rbbs )            s" DOC\STARTUP.TXT"
     Prepend<home>\ "browse ; ButtonInfo"  Help me get started  (F1) "
        5 HSpace

        -1 HSpace                       \ split the ToolBar into tow lines

       14 HSpace

\        AddButton "Editing" beep ;
\        NewObject to EditingButton
\        DefToolWidth DefToolSpacing + 5 *  SetBwidth: EditingButton
\                                       22 SetBheight: EditingButton

 DefToolWidth DefToolSpacing + 5 * HSpace
        8 HSpace
    17 PictureButton                    find-in-files   ; \ find in files
        ButtonInfo"  Find Text in Files...  (Ctrl+Shft+F) "

        8 HSpace
     9 PictureButton                    replace-text    ; \ find
        ButtonInfo"  Find - Replace Text... \n (Ctrl+F) or (Ctrl+Shft+F3) "
    11 PictureButton                find-text-highlight ; \ find highlight
        ButtonInfo"  Find Highlighted Text Forward  (Ctrl+F3) "

        8 HSpace
    12 PictureButton               back-find-text-again ; \ find again back
        ButtonInfo"  Find Text Again Backward  (Shft+F3) "
    10 PictureButton                    find-text-again ; \ find again
        ButtonInfo"  Find Text Again Forward  (F3) "

        8 HSpace
                                  \ "findCombo" is used to manipulate the list
DefToolWidth DefToolSpacing + 8 * ComboField findCombo
        ButtonInfo"  Find Text "
        250 SetHeight: findCombo
        findCombo to findComboEdit     \ link for forward reference

        5 HSpace

ENDBAR

\ save away find text and perform search when user presses ENTER

: myWmChar      ( h m w l obj -- res )
        2 pick VK_RETURN  =             \ if return
        IF      >r GetText: [ r@ ]      \ get adr,len of edit control text
                "CLIP" find-buf place   \ save string in find buffer
                find-text-again         \ search for it
                SetFocus: [ r> ]        \ retain the focus
                FALSE                   \ we already processed this message
        ELSE    drop                    \ discard object
                TRUE                    \ and use default processing
        THEN    ;

' myWmChar SetWmChar: findCombo

\ save away find text and perform search when user presses F3

: myWmKeyDown   ( h m w l obj -- res )
        2 pick VK_F3  =                 \ if F3
        IF      GetText: [ ]            \ get adr,len of edit control text
                "CLIP" find-buf place   \ save string in find buffer
                ?shift                  \ if we have the shift key
                IF      back-find-text-again
                ELSE    find-text-again \ search for it
                THEN
                FALSE                   \ we already processed this message
        ELSE    drop                    \ discard object
                TRUE                    \ and use default processing
        THEN    ;

' myWmKeyDown SetWmKeyDown: findCombo

\ save the find text away when the combo box gets a WM_KILLFOCUS message

: myWmKillFocus   ( h m w l obj -- res )
        GetText: [ ]                    \ get adr,len of edit control text
        "CLIP" find-buf place           \ save string in find buffer
        TRUE ;                          \ and use default processing

' myWmKillFocus SetWmKillFocus: findCombo
     
WVToolBar min-Edit-Tool-Bar "WINED.BMP"     \ jap ????
        3 HSpace
    22 TinyButton    Float: WinEdToolbar ;
        ButtonInfo"  Float ToolBar toggle "
        5 HSpace
     0 PictureButton                    new-text        ; \ new
        ButtonInfo"  New Text File  (Ctrl+N) "
     1 PictureButton                    open-text       ; \ open
        ButtonInfo"  Open Text File  (Ctrl+O) "
     2 PictureButton                    save-text       ; \ save
        ButtonInfo"  Save Text File  (Ctrl+S) "
    21 PictureButton                    save-all-text   ; \ save all files
        ButtonInfo"  Save All Changed Files "
     4 PictureButton                    close-text      ; \ close
        ButtonInfo"  Close Text File  (Ctrl+W) "

\ DefToolWidth DefToolSpacing + HSpace
        8 HSpace
        8 HSpace
        8 HSpace
    17 PictureButton                    find-in-files   ; \ find in files
        ButtonInfo"  Find Text in Files...  (Ctrl+Shft+F) "

\ DefToolWidth DefToolSpacing + HSpace
        8 HSpace
        8 HSpace
        8 HSpace
     9 PictureButton                    replace-text    ; \ find
        ButtonInfo"  Find - Replace Text... \n (Ctrl+F) or (Ctrl+Shft+F3) "
    11 PictureButton                find-text-highlight ; \ find highlight
        ButtonInfo"  Find Highlighted Text Forward  (Ctrl+F3) "

        8 HSpace
    12 PictureButton               back-find-text-again ; \ find again back
        ButtonInfo"  Find Text Again Backward  (Shft+F3) "
    10 PictureButton                    find-text-again ; \ find again
        ButtonInfo"  Find Text Again Forward  (F3) "

\ DefToolWidth DefToolSpacing + HSpace
        8 HSpace
        8 HSpace
        8 HSpace
    19 PictureButton                    text-options    ; \ Preferences
        ButtonInfo"  WinEd Preferences  (Ctrl+Shft+P) "

        8 HSpace
     5 PictureButton    ( rbbs )           s" DOC\STARTUP.TXT"
     Prepend<home>\ "browse ; ButtonInfo"  Help me get started  (F1) "
        5 HSpace

ENDBAR


\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ 36    Define the Editor popup menu for the application  
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

POPUPBAR Edit-Popup-bar
    POPUP " "
        MENUITEM        "Find - Replace Text..."  replace-text ;
        MENUITEM        "Find Again forward"      find-text-again ;
        MENUITEM        "Find Again backward"     back-find-text-again ;
        MENUSEPARATOR                                        
        MENUITEM        "Cut"                     cut-text    ;
        MENUITEM        "Copy"                    copy-text   ;
        MENUITEM        "Paste"                   paste-text  ;
        MENUITEM        "Delete"                  clear-text  ;
        MENUITEM        "Select All"              highlight-all  ;
        MENUSEPARATOR
        MENUITEM        "Copy && Paste to Win32Forth"   temp-text ;
        MENUSEPARATOR
        MENUITEM        "Set Right Margin to Cursor Column"  right-to-cursor ;
        MENUITEM        "Browse/Edit Toggle"      browse-toggle ;
        MENUSEPARATOR
        MENUITEM        "Open File..."            open-text ;
        MENUITEM        "Close File"              close-text ;
        MENUITEM        "Open Highlighted File"   open-text-highlighted ;
        MENUITEM        "Open SEARCHLIST.TXT"     searchlist-edit ;
        MENUSEPARATOR
        MENUITEM        "Open Forth Process"      >F ;
        MENUITEM        "Open ToolBar"            StartToolBar: WinEdWindow ;
        MENUSEPARATOR
        MENUITEM        "Print File..."           print-text ;
        MENUSEPARATOR
        MENUITEM        "Exit"                    bye ;
ENDBAR


\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ 37    Define the Console popup menu for the application  
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

POPUPBAR Console-Popup-bar
    POPUP " "
        MENUITEM        "Abort Current Operation"    k_ESC pushkey ;
        MENUITEM        "Pause Current Operation"    BL    pushkey ;
        MENUSEPARATOR
        MENUITEM        "Clear Console Buffer"       clear-screen ;
        MENUITEM        "Separate Forth Console"     CloseConsole: WinEdWindow
                                0 WM_FORTHIO win32forth-message ;
        MENUITEM        "Close Forth Process"        CloseConsole: WinEdWindow
                                1 WM_TERMINATE win32forth-message ;
        MENUSEPARATOR
        MENUITEM        "Exit WinEd"               bye ;
ENDBAR


\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ 38    Define the ToolBar popup menus for the application  
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

POPUPBAR max-ToolBar-Popup-bar
    POPUP " "
        MENUITEM        "Minimum ToolBar"       SwitchToolBar: WinEdWindow ;
        MENUITEM        "Float ToolBar toggle"          Float: WinEdToolbar ;
        MENUITEM        "Close ToolBar"          CloseToolBar: WinEdWindow ;
ENDBAR

POPUPBAR min-ToolBar-Popup-bar
    POPUP " "
        MENUITEM        "Full ToolBar"          SwitchToolBar: WinEdWindow ;
        MENUITEM        "Float ToolBar toggle"          Float: WinEdToolbar ;
        MENUITEM        "Close ToolBar"          CloseToolBar: WinEdWindow ;
ENDBAR

hidden also

:Class :POPUP <Super  POPUP

int insertedMenus                       \ count of inserted menus

8 CONSTANT MAXMENU

MAXMENU 1 + CELLS bytes mid[]           \ the ids of the menu items
MAXMENU 1 + CELLS bytes mfunc[]         \ the menu function pointers

:M DoMenu:      { theID \ text$ -- }
        MAXSTRING LocalAlloc: text$
        pm >r                   \ save in case we are reentered while running
        hpm to pm
        BEGIN   pm
        WHILE   theID DoMenu: pm
                   GetPrev: pm to pm
        REPEAT
        insertedMenus 0
        ?DO     theID mid[] i CELLS+ @ =
                IF      MF_BYCOMMAND
                        MAXSTRING
                        text$ 1+ rel>abs
                        theID
                        pid Call GetMenuString text$ c!

                        text$ count mfunc[] i CELLS+ @ execute-menufunc
                THEN
        LOOP
        r> to pm
        ;M

:M GetCount:    ( -- n1 )
                insertedMenus
                ;M

:M GetString:   ( adr index -- )
                over >r >r >r
                MF_BYCOMMAND
                MAXSTRING
                r> 1+ rel>abs
                mid[] r> CELLS+ @
                pid Call GetMenuString r> c!
                ;M

:M ClassInit:   ( -- )
                ClassInit: Super
                mid[]   MAXMENU CELLS erase    \ init id list
                mfunc[] MAXMENU CELLS erase    \ init function pointers to NULL
                0 to insertedMenus
                ;M


((
RECENT FILES menu now shows themost recently used files on top, LRF off bottom.
\ This is thanks to Bruno Gauthier
 \ Future: Put a double down arrow on the bottom for more files like MS programs
  \ Remove "Recent Files" title to make room for two more files in the list
))

:M InsertMenuItem: { zMenuText pFunc \ iRank iFlag text$ -- }
        recent-files?
        IF  MAXSTRING LocalAlloc: text$
            FALSE to iFlag                        \ mark as not already
\ inserted
            FALSE To iRank
            insertedMenus 0
            ?DO
                mid[] i CELLS+ @ NULL <>          \ if the id isn't NULL
                IF  MF_BYCOMMAND
                    MAXSTRING
                    text$ 1+ rel>abs
                    mid[] i CELLS+ @              \ use the ID to get text
                    pid Call GetMenuString text$ c!
                    zMenuText text$ count tuck
                    CAPS-COMPARE dup 0= if i to iRank then
                                     0= iFlag or to iFlag
                THEN
            LOOP
            iFlag 0=                              \ if not already inserted
            IF  insertedMenus 0=
                IF  NextId    mid[] insertedMenus CELLS+ !  \ set the menu
\ ID
                    pFunc   mfunc[] insertedMenus CELLS+ !  \ set the
\ function

                    zMenuText rel>abs
                     mid[] insertedMenus cells+ @
                    MF_STRING
                    pid
                    Call AppendMenu drop
                    1 +to insertedMenus
                ELSE                   \ make room for an additional entry
                    mid[]   dup CELL+ insertedMenus CELLS move
                    mfunc[] dup CELL+ insertedMenus CELLS move
                                       \ the the entry in the table
                    NextId   mid[] !
                    pFunc  mfunc[] !
                                       \ if the table is full, delete last
\ entry
                    insertedMenus MAXMENU >=
                    IF  MF_BYCOMMAND
                        mid[] insertedMenus CELLS+ @
                        pid
                        Call DeleteMenu drop
                    ELSE
                        1 +to insertedMenus
                    THEN
                                       \ insert the new entry in menu
                    zMenuText rel>abs
                    mid[] @
                    MF_STRING
                    MF_BYCOMMAND or
                    mid[] CELL+ @
                    pid
                    Call InsertMenu drop
                THEN

            ELSE        iRank 0<>  ( if already the 1st no need to change)

                IF                              \ delete the entry in menu
                        MF_BYCOMMAND
                        mid[] iRank  CELLS+ @
                        pid
                        Call DeleteMenu drop
                        -1 +to insertedMenus
                                                \ make room for the entry
                        mid[]  dup CELL+ iRank CELLS move
                        mfunc[] dup CELL+ iRank CELLS move
                                                \ the entry in the table
                        NextId   mid[] !
                        pFunc  mfunc[] !
                                                \ insert the new entry in
\ menu
                        zMenuText rel>abs
                        mid[] @
                        MF_STRING
                        MF_BYCOMMAND or
                        mid[] CELL+ @
                        pid
                        Call InsertMenu drop
                        1 +to insertedMenus

                THEN

            THEN
        THEN
        ;M
\ END OF NEW TESTED VERSION  !???






;Class

previous forth


\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ 39    Define the menubar for the application
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

MENUBAR Edit-Menu-bar

:POPUP fileMenu "&File"
   MENUITEM     "&New  File               \tCtrl+N"      new-text ;
   MENUITEM     "&Open File...            \tCtrl+O"      open-text ;
   MENUITEM     "Open &Highlighted File   \tCtrl+Shft+O" open-text-highlighted ;
   MENUSEPARATOR
   MENUITEM     "&Open URL..."                           open-html ;
   MENUITEM     "Open Forth Window"                     >F ;
   MENUSEPARATOR
   MENUITEM     "Close File &Window       \tCtrl+W"      close-text ;
   MENUITEM     "Close All File Windows   \tCtrl+Shft+W" close-all-text ;
   MENUSEPARATOR
   MENUITEM     "&Save File               \tCtrl+S"      save-text ;
   MENUITEM     "Save &All Changed Files"                save-all-text ;
   MENUITEM     "Save File As.."                         save-text-as ;
   MENUSEPARATOR
   MENUITEM     "Save MAC/UNIX File As PC.."             save-text-as-pc ;
   MENUITEM     "&Revert to Last Saved Version"          revert-text ;
   MENUSEPARATOR
   MENUITEM     "&Load the active Forth Source \tCtrl+L" load-active-file ;
( >>> ) MENUITEM   "Load a Forth Source File..."            load-forth-file ;
( >>> ) MENUITEM   "Save and Load the active Forth Source \tF12" save-and-load ;
   MENUITEM     "Load &Color Configuration File"         load-colors ; \ {BE}
   MENUSEPARATOR
   MENUITEM     "Print Setup... "                        text-setup ;
   MENUITEM     "Pages Up Setup..."                      text-up-setup ;
   MENUITEM     "&Print File...           \tCtrl+P"      print-text ;
   MENUSEPARATOR
   MENUITEM     "Save All Changes && Exit \tShft+F10"    save-all-text bye ;
   MENUITEM     "E&xit                    \tAlt+F4"      bye ;
   MENUSEPARATOR
 false MENUMESSAGE     "Recent Files"
   MENUSEPARATOR

 POPUP "&Edit"
   MENUITEM        "&Undo                      \tCtrl+Z"      word-undelete ;
   MENUSEPARATOR
   MENUITEM        "Cu&t                       \tCtrl+X"      cut-text    ;
   MENUITEM        "&Copy                      \tCtrl+C"      copy-text   ;
   MENUITEM        "&Paste                     \tCtrl+V"      paste-text  ;
   MENUITEM        "&Delete                    \tDel"         clear-text  ;
   MENUITEM        "Select &All                \tCtrl+A"      highlight-all ;
   MENUITEM        "Select Toggle              \tCtrl+Q"      highlight-mark ;
   MENUSEPARATOR
   MENUITEM        "Copy && Paste to Win32Forth\tF11"         temp-text ;
   MENUSEPARATOR
   MENUITEM        "&Insert Date Time          \tCtrl+Shft+V" paste-date/time ;
   MENUSEPARATOR
   MENUITEM        "&Browse/Edit Toggle        \tShft+F9"     browse-toggle ;
   MENUITEM        "WinEd &Preferences...    \tCtrl+Shft+P" text-options ;
   MENUSEPARATOR
   SUBMENU     "ToolBar Options"
       MENUITEM        "Open/Close ToolBar"
                       tool-bar?
                       IF      CloseToolBar: WinEdWindow
                       ELSE    StartToolBar: WinEdWindow
                       THEN ;
       MENUITEM        "Float/Dock ToolBar"    Float: WinEdToolbar ;
       MENUITEM        "Full/Minimum ToolBar"  SwitchToolBar: WinEdWindow ;
       ENDSUBMENU
   MENUITEM    "Display Debug &ButtonBar   \tCtrl+D"     debug-buttons ;
   MENUITEM    "&Set a BreakPoint          \tCtrl+B"     debug-word ;

 POPUP "F&ormat"
   MENUITEM      "&Paragraph Reformat        \tCtrl+R"   reformat-text ;
   MENUITEM      "&Select Font and Type Size..."         text-size ;
\   MENUITEM      "Toggle &Colorization"                 toggle-color ; \ {BE}
   MENUSEPARATOR
   MENUITEM      "Change Cas&e               \tCtrl+U"  highlight-case-toggle ;
   MENUITEM      "Change &Hex/Decimal        \tCtrl+Shft+H"  make-hex ;
   MENUSEPARATOR                                                    
   MENUITEM      "&Expand File TABS to Spaces\tCtrl+Shft+E"  expand-tabs ;
   MENUITEM      "Sort Paragraph &Lines..."              sort-paragraph ;
   MENUSEPARATOR
   MENUITEM      "Convert to a PC file"                  change-to-pc ;
   MENUITEM      "Convert to a Apple file"               change-to-apple ;
   MENUITEM      "Convert to/from a Binary file"      change-to-binary-toggle ;

 POPUP "&Search"
   MENUITEM      "&Find / Replace Text...\tCtrl+F"       replace-text ;
   MENUITEM      "Find &Again Forward   \tF3"            find-text-again   ;
   MENUITEM      "Find Again &Backward  \tShft+F3"     back-find-text-again   ;
   MENUITEM      "Find &Highlight       \tCtrl+F3"       find-text-highlight ;
   MENUSEPARATOR
   MENUITEM      "Find &Text in File(s)...\tCtrl+Shft+F"   find-in-files ;
   MENUITEM ( rbs ) "&Edit Searchlist.txt" Prepend<home>\ searchlist-edit ;
   MENUSEPARATOR
   MENUITEM      "&Goto Line Number...  \tCtrl+Enter"    goto-line ;

 POPUP "&Macros"
   MENUITEM      "&New Key Recording File..."            new-macro ;
   MENUITEM      "&Start - Stop Key Recording \tCtrl+Shft+S" start/stop-macro ;
   MENUSEPARATOR                                                  
   MENUITEM      "&Edit Key File"                        edit-macro ;
   MENUITEM      "&Play Key File"                        play-macro ;
   MENUITEM      "&RePlay last Key File       \tCtrl+Shft+M or F5"
                     replay-macro ;
   MENUITEM      "&Repeat Key File n times..  \tCtrl+Shft+R"
                     WinEdWindow repeat-amacro ;

 POPUP "H&yper"
   MENUITEM   "&Link to a Word...               \tCtrl+Shft+F9"  word-link ;
   MENUITEM   "Link to &Highlighted Word        \tF9"            hyper-link ;
   MENUITEM   "Link to Next &Occurance of &Word \tCtrl+F9"    next-hyper-link ;
   MENUITEM   "&UnLink to Previous Link         \tF10"           close-text ;
   MENUSEPARATOR
   MENUITEM   "&Previous Hyper Link             \tCtrl+PgUp"     prev-link ;
   MENUITEM   "&Next Hyper Link                 \tCtrl+PgDn"     next-link ;
   MENUSEPARATOR                                                   
   MENUITEM   "&Edit Hyper Link Configuration File"              hyper-edit ;
   MENUITEM   "&Build Hyper Link Index File"                   hyper-compile ;
            \ CD to the folder with wined cfg if it does not work.
SYS-WARNING-OFF

 POPUP "&Display"
 false MENUMESSAGE     "--- System Functions ---       \tForth Word"
   MENUSEPARATOR
   MENUITEM   "This Programs Name             \t.PROGRAM"  s" .PROGRAM"  "CONSOLE ;
   MENUITEM   "Version of Win32Forth          \t.VERSION"  s" .VERSION " "CONSOLE ;
   MENUITEM   "Operating System Version       \t.PLATFORM" s" .PLATFORM" "CONSOLE ;
   MENUITEM   "Current HELP file              \t.HELP"     s" .HELP"     "CONSOLE ;
   MENUITEM   "Return Stack Contents          \t.RSTACK"   s" .RSTACK"   "CONSOLE ;
   MENUITEM   "Memory Used and Available      \t.FREE"     s" .FREE"     "CONSOLE ;
   MENUITEM   "File Search Path               \t.FPATH"    s" .FPATH"    "CONSOLE ;
   MENUSEPARATOR
 false MENUMESSAGE     "--- Vocabulary Functions ---   \tForth Word"
   MENUSEPARATOR
   MENUITEM   "Words in Current Vocabulary    \tWORDS"     s" WORDS"     "CONSOLE ;
   MENUITEM   "All Vocabulary Statistics      \tVOCS"      s" VOCS"      "CONSOLE ;
   MENUITEM   "Current Vocab Thread Counts    \t.COUNTS"   s" .COUNTS"   "CONSOLE ;
   MENUITEM   "Current Vocab Thread Words     \t.THREADS"  s" .THREADS"  "CONSOLE ;
   MENUSEPARATOR
 false MENUMESSAGE     "--- List Functions ---         \tForth Word"
   MENUSEPARATOR
   MENUITEM   "List of Classes in Win32Forth  \t.CLASSES"  s" .CLASSES"  "CONSOLE ;
   MENUITEM   "List of Loaded Files           \t.LOADED"   s" .LOADED"   "CONSOLE ;
   MENUITEM   "List of Fonts in System        \t.FONTS"    s" .FONTS"    "CONSOLE ;
   MENUITEM   "List of Deferred &Word         \t.DEFERRED" s" .DEFERRED" "CONSOLE ;
   MENUITEM   "List of Execution Chains       \t.CHAINS"   s" .CHAINS"   "CONSOLE ;
   MENUITEM   "List of Pointers               \t.POINTERS" s" .POINTERS" "CONSOLE ;
   MENUITEM   "List of Dynamic Memory Used    \t.MALLOCS"  s" .MALLOCS"  "CONSOLE ;
   MENUITEM   "List of Win32API Calls Used    \t.PROCS"    s" .PROCS"    "CONSOLE ;
   MENUITEM   "List of Win32API Libraries Used\t.LIBS"     s" .LIBS"     "CONSOLE ;
   MENUSEPARATOR
 false MENUMESSAGE     "--- Date Functions ---         \tForth Word"
   MENUSEPARATOR
   MENUITEM   "Todays &Date                   \t.DATE"  s" .DATE" "CONSOLE ;
   MENUITEM   "The Current &Time              \t.TIME"  s" .TIME" "CONSOLE ;

 POPUP "&Documentation Help"                  \ jp
   MENUITEM   "&Help me get Started!  ( rbs ) \tF1" s" HTM\STARTUP.TXT" Prepend<home>\ "BROWSE ;
   MENUITEM   "&Help on highlighted ANS-Forth word \tCtrl+F1" help-link ;
   MENUITEM   "&Win32Forth READ.ME"           s" HTM\READ.ME" Prepend<home>\ "BROWSE ;
   MENUITEM   "&Win32Forth FAQ's"             s" HTM\WIN32FOR.FAQ" Prepend<home>\ "BROWSE ;
   MENUITEM   "&Commandline Help"             show-command-help ;
   MENUSEPARATOR          ( rbs for several lines )
   MENUITEM   "&Whats NEW in Win32Forth"      s" HTM\WIN32FOR.NEW" Prepend<home>\ "BROWSE ;
   MENUITEM   "&Previous Win32Forth Changes"  s" HTM\WIN32FOR.PRV" Prepend<home>\ "BROWSE ;
   MENUITEM   "&Utilities in Win32Forth"      s" HTM\UTILDOC.TXT" Prepend<home>\ "BROWSE ;
   MENUITEM   "&ANS Required Documentation"   s" HTM\ANSI.TXT" Prepend<home>\ "BROWSE ;
   MENUITEM   "&DPANS94 Document"             s" HTM\DPANS.HTM" Prepend<home>\ "BROWSE ;
   MENUITEM   "ANS Forth Word List"           s" HTM\DPANSF.HTM" Prepend<home>\ "BROWSE ;
   MENUITEM   "Assembler Documentation"       s" HTM\486ASM.TXT" Prepend<home>\ "BROWSE ;
   MENUSEPARATOR
 false MENUMESSAGE     "Web Based Resources"
   MENUSEPARATOR
   SUBMENU    "Forth Interest Group"
       MENUITEM    "Forth Home page"
               s" http://www.forth.org"                     "ViewWeb-Link ;
       MENUITEM    "FIG Membership Information"
               s" http://www.forth.org/membership.html"     "ViewWeb-Link ;
       MENUITEM    "Forth Literature page"
               s" http://www.forth.org/forthlit.html"       "ViewWeb-Link ;
       MENUITEM    "Forth Compilers page"
               s" http://www.forth.org/forthcomp.html"      "ViewWeb-Link ;
       MENUITEM    "FTP access"
               s" ftp://ftp.forth.org/pub/forth"            "ViewWeb-Link ;
       MENUITEM    "FIG UK"
               s" http://www.users.zetnet.co.uk/aborigine/forth.htm"
               "ViewWeb-Link ;
   ENDSUBMENU
   SUBMENU     "Forth Web pages"
     false MENUMESSAGE      "Introductory"
       MENUSEPARATOR
       MENUITEM    "Leo Wong           \tForth Tutorial"
               s" http://www.albany.net/~hello/simple.htm"  "ViewWeb-Link ;
       MENUSEPARATOR
     false MENUMESSAGE     "Win32Forth"
       MENUSEPARATOR
       MENUITEM    "Dave Pochin        \tOnline Guide"
               s" http://www.sunterr.demon.co.uk/guide.htm" "ViewWeb-Link ;
       MENUITEM    "Jeff Kelm          \tWin32API Examples"
               s" http://www.concentric.net/~jkelm/"        "ViewWeb-Link ;
       MENUITEM    "Michael Hillerström\tDialog Editor"
               s" http://users.cybercity.dk/~ccc27382/"     "ViewWeb-Link ;
       MENUITEM    "Marc Petremann     \tForth FAQs & Examples"
               s" http://perso.wanadoo.fr/mp7/forth/"       "ViewWeb-Link ;
       MENUSEPARATOR
     false MENUMESSAGE     "Macintosh"
       MENUSEPARATOR
       MENUITEM    "Michael Hore       \tJfar MOPS Page"
               s" http://www.netaxs.com/~jayfar/mops.html"  "ViewWeb-Link ;
   ENDSUBMENU
   SUBMENU     "Forth News Groups"
       MENUITEM    "comp.lang.forth    \tGeneral Forth"
               s" news:comp.lang.forth"                     conhndl "Web-Link ;
       MENUITEM    "comp.lang.forth.mac\tMacintosh Specific"
               s" news:comp.lang.forth.mac"                 conhndl "Web-Link ;
       MENUITEM    "de.comp.lang.forth \tGerman Language"
               s" news:de.comp.lang.forth"                  conhndl "Web-Link ;
   ENDSUBMENU
   SUBMENU     "comp.lang.forth FAQs"
       MENUITEM    "General Information"
               s" http://www.faqs.org/faqs/computer-lang/forth-faq/part1/"
               "ViewWeb-Link ;
       MENUITEM    "Online Resources"
               s" http://www.faqs.org/faqs/computer-lang/forth-faq/part2/"
               "ViewWeb-Link ;
       MENUITEM    "Vendors & Authors"
               s" http://www.faqs.org/faqs/computer-lang/forth-faq/part3/"
               "ViewWeb-Link ;
       MENUITEM    "Forth Systems"
               s" http://www.faqs.org/faqs/computer-lang/forth-faq/part4/"
               "ViewWeb-Link ;
       MENUITEM    "Books & Periodicals"
               s" http://www.faqs.org/faqs/computer-lang/forth-faq/part5/"
               "ViewWeb-Link ;
       MENUITEM    "Groups & Organizations"
               s" http://www.faqs.org/faqs/computer-lang/forth-faq/part6/"
               "ViewWeb-Link ;
   ENDSUBMENU
   MENUSEPARATOR
   MENUSEPARATOR
   MENUITEM    "&About Win32Forth" about-Win32Forth ;
   MENUITEM    "&About WinEd"    about-demo ;
ENDBAR

SYS-WARNING-ON

: EnableDisplayMenu ( flg -- )  \ enable/disable the 'Display' menu
                6 PosEnable: Edit-Menu-bar ;

: "OpenMenuFile ( a1 n1 -- )
                >E-unminimize
                "+open-text ;

: _addFileMenu   ( -- )
                cur-filename 1+ ['] "OpenMenuFile InsertMenuItem: fileMenu ;

' _addFileMenu is addFileMenu


\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ 40    Define the main window for the application  
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

:Object FrameWindow  <super window

Rectangle EditRect
Rectangle ClientRect

MAXSTRING bytes StatusBuf

: abs-create-frame-window  ( -- hwnd )
        0 0                                 \ adjust x,y relative to zero, zero
        StartSize:     [ self ]             \ width, height
        SetRect: EditRect
        ^base                               \ creation parameters
        appInst                             \ program instance
        NULL LoadMenu: [ self ]             \ menu
        ParentWindow:  [ self ]             \ parent window handle
        EditRect.Bottom EditRect.Top -      \ adjusted height
        EditRect.Right  EditRect.Left -     \ adjusted  width
        StartPos:      [ self ] swap        \ y, x starting position
        WindowStyle:   [ self ]             \ the window style
        WindowTitle:   [ self ] rel>abs     \ the window title
        WindowClassName 1+ rel>abs          \ class name
        ExWindowStyle: [ self ]             \ extended window style
        Call CreateWindowEx
        EraseRect: EditRect ;

:M Start:       ( -- )         \ create a new window object
        hWnd 0=
        IF      s" WinEdWindow" SetClassName: self \ set the class name
                default-window-class
                \ over-ride the default background (WHITE) color
                NULL to hbrBackground
                register-the-class drop
                abs-create-frame-window to hWnd
                SW_SHOWNORMAL Show: self
                Update: self
        ELSE    SetFocus: self
        THEN
        ;M

MACRO -status   " 19 -"         ( n1 -- n2 )

0 value vdraging?
0 value hdraging?

: in-vdrag?     ( -- f1 )       \ in vert drag bar
                hWnd get-mouse-xy nip
                drag-barV dup drag-thick + 2 + 1 - between
                show-console? and ;

: in-hdrag?     ( -- f1 )       \ in horizontal drag bar
        hWnd get-mouse-xy       \ -- x y ;mouse position
        show-console?
        IF      edit-top drag-barV      between
        ELSE    edit-top Height -status between
        THEN
        swap
        drag-barH dup drag-thick + 2 +  between and ;

\ mouse click routines for FrameWindow to track the dragbar movement

: wineditW-track ( -- )
        mousedown? 0= ?EXIT
        vdraging?
        IF      mousey drag-thick 2/ - Height drag-thick 2 + -
                -status 1 - 0max min
                StartSize: WinEdToolbar nip max to drag-barV
                set-console-height
                drag-barV edit-top listHeightDefault + <= Hide: Subjectlist
        THEN
        hdraging?
        IF      mousex drag-thick 2/ - 0max Width edit-min - 0max min
                0max to drag-barH
        THEN
        Showing: SubjectList
        IF      0 edit-top drag-barH 1- 160 Move: SubjectList
        THEN
        Refresh: WinEdWindow
        WINPAUSE ;

: wineditW-click ( -- )
        mousedown? 0=
        IF      hWnd Call SetCapture drop
        THEN
        true to mousedown?
        in-vdrag?  to vdraging?
        in-hdrag?  to hdraging?
        wineditW-track ;

: wineditW-unclick ( -- )
        mousedown?
        IF      Call ReleaseCapture drop
        THEN
        false to mousedown?
        false to vdraging?
        false to hdraging? ;

: dbl-click-vdrag ( -- )
        show-console? 0= ?EXIT
        in-vdrag? 0= ?EXIT
        drag-barV drag-thick 2 / +
        StartSize: WinEdToolbar nip drag-thick + 2 + Height
        -status 0max between
        IF      StartSize: WinEdToolbar nip to drag-barV
                ConsoleWindow to DocWindow
        ELSE    Height drag-thick 2 + - -status 1 - 0max to drag-barV
                EditWindow to DocWindow
        THEN    GetStack: DocWindow to entry#
        set-console-height
        show-console?
        IF      drag-barV edit-top listHeightDefault + <= Hide: Subjectlist
        THEN    ;

: dbl-click-hdrag ( -- )
        in-hdrag? 0= ?EXIT
        drag-barH 8 >
        IF      0 to drag-barH
        ELSE    132 Width 2/ min to drag-barH
        THEN
        Showing: SubjectList
        IF      0 edit-top drag-barH 1- 160 Move: SubjectList
        THEN    ;

: wineditW-dblclick ( -- )       \ highlight the current word
        false to mousedown?
        mousey StartSize: WinEdToolbar nip < ?EXIT
        dbl-click-vdrag
        dbl-click-hdrag
        SetFocus: DocWindow
        Refresh: WinEdWindow ;

:M Classinit:   ( -- )
        ClassInit: super                \ init super class
                     3 to OriginX
                     3 to OriginY
                  self to WinEdWindow \ make myself the cur window
         8                Width: sFont
        14               Height: sFont
        s" Courier" SetFaceName: sFont  \ default to Courier
        min-Edit-Tool-Bar  to WinEdToolbar
        ['] wineditW-click       SetClickFunc: self
        ['] wineditW-unclick   SetUnClickFunc: self
        ['] wineditW-track       SetTrackFunc: self
        ['] wineditW-dblclick SetDblClickFunc: self
        ;M

:M On_Init:     ( -- )          \ initialize the class
      On_Init: super                  \ first init super class
      Create: sFont                   \ create the font for file list
      second-copy?    0=
      show-console?   and
      IF    Height 4 /  SetTopOf: ConsoleWindow  \ set the console window's top
            1    SetId: ConsoleWindow       \ then the child window
            self Start: ConsoleWindow       \ then startup child window
            2    SetId: SplitterV
            self Start: SplitterV
            z" RMOT" @ ed-console-remote !          \ set remote IO operations
            ed-forth-count @ 0=
            IF      z" HIDN" @ ed-console-hidden !  \ flag Forth to hide console
                    StartUpForth
            ELSE    0 WM_REMOTEIO win32forth-message
            THEN
      ELSE
            FALSE to show-console?
      THEN
      min-tool-bar?
      IF    min-Edit-Tool-Bar  to WinEdToolbar
      ELSE  max-Edit-Tool-Bar  to WinEdToolbar
      THEN
      tool-bar?
      IF      3    SetId: WinEdToolbar      \ then the next child window
              self Start: WinEdToolbar      \ then startup child window
      THEN
      edit-top SetTopOf: EditWindow           \ set the edit window's top
      4    SetId: EditWindow                  \ then the child window
      self Start: EditWindow                  \ then startup child window
      5    SetId: SplitterH
      self Start: SplitterH
      self Start: SubjectList
      FilesList  InitSubject: SubjectList          \ we start with files subject
      self Start: StatBar              \ start the status bar
      window-list
      BEGIN   dup @ ?dup
      WHILE   >r
              self Start: [ r@ ]
              TRUE  Hide: [ r> ]
              cell+
\              swap 1+ swap \ May 7th, 1998 tjz removed per Bruno Gautier
      REPEAT  DROP         \ May 7th, 1998 tjz added per Bruno Gauthier
      FALSE Hide: FilesList
      FALSE Hide: Subjectlist         \ then hide SubjectList
      Edit-Menu-Bar  SetMenuBar: self
      min-ToolBar-Popup-bar SetPopupBar: min-Edit-Tool-Bar
      max-ToolBar-Popup-bar SetPopupBar: max-Edit-Tool-Bar
      Edit-Popup-Bar SetPopupBar: EditWindow
      Console-Popup-Bar SetPopupBar: ConsoleWindow
      show-console? EnableDisplayMenu
      ;M

: set-col/rows  { theWindow -- }
        entry# >r
        GetHandle: theWindow
        IF      GetStack: theWindow to entry#
                screen-cols >r
                     Width: theWindow
                 CharWidth: theWindow / to screen-cols
                    Height: theWindow
                CharHeight: theWindow / to screen-rows
                r> screen-cols 2dup <
                IF      2dup - col-cur + 0max to col-cur
                THEN    2drop
        THEN    r> to entry#  ;

: adjust-col/rows ( -- )
        EditWindow    set-col/rows
        ConsoleWindow set-col/rows ;

:M SetStatus:   ( a1 n1 a2 n2 -- )
        StatusBuf place
        StatusBuf +place
        StatusBuf count SetText: StatBar        \ set the current text on status
        Paint: StatBar
        ;M

:M Refresh:     ( -- )
      minimized? 0=
      IF      show-console?
              IF      Height console-height - StartSize: WinEdToolbar nip max
              ELSE    Height
              THEN    to drag-barV
              drag-barH Width edit-min - 0max min to drag-barH

              edit-top        SetTopOf: EditWindow
              drag-barV       SetTopOf: ConsoleWindow

(( The peculiar calculations for window positions and sizes, result from the
   fact that Windows places windows "inside" of the window space specified, and
   was thus leaving unrefreshed lines between all my child windows if I didn't
   fill in the space with an extra pixel here and there.
   tjz February 24th, 1997
))
              all-lines: EditWindow

              drag-barH drag-thick + 1+
              Height edit-top -                     -status 1-
              Width drag-barH drag-thick + - 18             Move: StatBar

              drag-barH drag-thick + 1+
              edit-top
              Width drag-barH drag-thick + -
              show-console?
              IF      drag-barV edit-top -
              ELSE    Height edit-top -             -status
              THEN                                          Move: EditWindow
              window-list
              BEGIN   dup @ ?dup
              WHILE   >r
                      Showing: [ r@ ]
                      IF      0 edit-top listHeight + dup>r
                              drag-barH 1-
                              show-console?
                              IF      drag-barV r> -
                              ELSE    Height r> -   -status
                              THEN                          Move: [ r@ ]
                      THEN    r>drop
                      cell+
              REPEAT  drop

              drag-barH 1- edit-top 1-
              drag-thick 2 +
              show-console?
              IF      drag-barV edit-top - 2 +
              ELSE    Height edit-top - 2 +         -status
              THEN                                          Move: SplitterH

              show-console?
              IF     all-lines: ConsoleWindow
                      -1 drag-barV
                      Width 2 + drag-thick 2 +              Move: SplitterV

                      -1 drag-barV drag-thick + 2 + dup>r
                      Width 2 + Height r> -        -status Move: ConsoleWindow
              THEN

              Showing: SubjectList
              IF      0 edit-top drag-barH 1- 160   -status Move: SubjectList
              THEN

              adjust-col/rows
        THEN
        ;M

:M StartToolBar: ( -- )
        tool-bar? 0=
        Floating: WinEdToolbar 0= and
        IF      TRUE to tool-bar?
                GetSize: WinEdWindow nip 2/ to drag-barV
                set-console-height
                self Start: WinEdToolbar          \ then startup child window
                LoadFindStrings
                Refresh: self
        ELSE    tool-bar? 0=                        \ if flagged as not open
                Floating: WinEdToolbar and        \ but flagged as floating
                IF      SetFocus: WinEdToolbar    \ then bring to front
                THEN
        THEN
        ;M

:M CloseToolBar: ( -- )
        tool-bar? Floating: WinEdToolbar or
        IF      SaveFindStrings
                Close: WinEdToolbar
                FALSE to tool-bar?
                Refresh: self
        THEN
        ;M

:M SwitchToolBar: ( -- )
        Floating: WinEdToolbar >r
        CloseToolBar: self
        WinEdToolbar max-Edit-Tool-Bar =
        IF      min-Edit-Tool-Bar  to WinEdToolbar
                TRUE to min-tool-bar?
        ELSE    max-Edit-Tool-Bar  to WinEdToolbar
                FALSE to min-tool-bar?
        THEN
        StartToolBar: self
        r>
        IF      Float: WinEdToolbar
        THEN
        ;M

:M StartConsole: ( -- )
      second-copy? 0=
      IF    Height 4 /                      \ console is a quarter screen
            SetTopOf: ConsoleWindow         \ set the console window's top
            self Start: ConsoleWindow       \ then startup child window
            self Start: SplitterV
            TRUE to show-console?           \ must be before the ">F" below
            console-text
            end-doc
            TRUE EnableDisplayMenu
            >F
            Refresh: self
            z" RMOT" @ ed-console-remote !  \ set remote IO operations
            ed-forth-count @ 0=
            IF      z" HIDN" @ ed-console-hidden !  \ flag Forth to hide console
                    StartUpForth
            ELSE    0 WM_REMOTEIO win32forth-message
            THEN
      ELSE  FALSE to show-console?
      THEN
      ;M

:M CloseConsole: ( -- )
        save-console
        FALSE to show-console?
        >E-unminimize
        Close: ConsoleWindow
        Close: SplitterV
        FALSE EnableDisplayMenu
        Refresh: self
        Retitle: [ self ]
        0 ed-console-remote !
        0 ed-console-hidden !
        ;M

:M On_Done:     ( h m w l -- res )
        Delete: sFont
        0 call PostQuitMessage drop     \ terminate application
        On_Done: super                  \ cleanup the super class
        0 ;M

:M WM_CLOSE     ( h m w l -- res )
        before-bye
        term-canceled? 0=               \ if we didn't cancel the close
        IF      bye                     \ then just terminate the program
                0
        ELSE    1                       \ abort program termination
                FALSE to term-canceled?
        THEN
        ;M

:M StartSize:   ( -- width height )     \ starting window size
        start-width
        SM_CXSCREEN Call GetSystemMetrics  4 -
        StartPos: self drop - min               \ screen width
        start-height
        SM_CYSCREEN Call GetSystemMetrics  4 -
        StartPos: self nip  - min               \ screen height
        ;M

:M StartPos:    ( -- x y )
        OriginX 0max OriginY 0max
        ;M

:M MinSize:     ( -- width height )     \ minimum window size
        0 -20
        ;M

:M WindowTitle: ( -- Zstring )          \ window caption
        z" WinEd"
        ;M

:M On_Paint:    ( -- )
        On_Paint: super
        tool-bar?
        IF
                0 0  StartSize: WinEdToolbar  Move: WinEdToolbar
                LTGRAY_BRUSH Call GetStockObject
                StartSize: WinEdToolbar drop 0        \ x,y origin
                Width
                StartSize: WinEdToolbar nip 1+ SetRect: EditRect
                EditRect.AddrOf rel>abs GetHandle: dc call FillRect ?win-error
                WHITE                                LineColor: dc \ white color
                EditRect.left EditRect.top              MoveTo: dc \ horiz
                StartSize: self drop Width max 0        LineTo: dc \ line
                BLACK                                LineColor: dc
                EditRect.left EditRect.bottom 2 - dup>r MoveTo: dc \ horiz
                StartSize: self drop Width max r>       LineTo: dc \ line
                EraseRect: EditRect
        THEN
        ListHeight      \ Only if SubjectList is turned on
        IF      \ line below the subject selection dropdown list
                BLACK                                LineColor: dc
                0 edit-top listHeight + 2 -             MoveTo: dc
                drag-barH  edit-top listHeight + 2 -    LineTo: dc
                WHITE                                LineColor: dc
                0 edit-top listHeight + 1-              MoveTo: dc
                drag-barH  edit-top listHeight + 1-     LineTo: dc
        THEN
        ;M

:M WM_SETCURSOR ( h m w l -- )
        EraseRect: ClientRect                \ init to zeros
        ClientRect.AddrOf GetClientRect: self
        hWnd get-mouse-xy
             ClientRect.Top  ClientRect.Bottom between
        swap ClientRect.Left ClientRect.Right  between and
        IF      in-vdrag?
                IF      splitv-cursor 1
                ELSE    in-hdrag?
                        IF      splith-cursor 1
                        ELSE    arrow-cursor 1
                        THEN
                ELSE    DefWindowProc: [ self ]
                THEN
        ELSE    DefWindowProc: [ self ]
        THEN
        1 ;M

:M ReTitle:     { \ title$ pad$ -- }
        LMAXSTRING localAlloc: title$
                32 localAlloc: pad$
        pad 32 - pad$ 32 move           \ save PAD
        entry# >r
        entry# entry-console =
        IF      s" Win32Forth Console"     title$  lplace
        ELSE    GetStack: EditWindow to entry#
                s" WinEd 2.09a: "          title$  lplace
                from-web?
                IF      URL$ lcount        title$ +lplace
                ELSE
                        cur-filename count title$ +lplace
                THEN
        THEN
        title$ lcount 255 min SetTitle: self
                              EditMode: WinEdToolbar

        s"  Column: "                                   StatusString  place
        cursor-col 1+  0 <# #s #>                       StatusString +place
        s"  of "                                        StatusString +place
        max-cols       0 <# #s #>                       StatusString +place
        s" ,  Line: "                                   StatusString +place
        cursor-line 1+ 0 (ud,.)                         StatusString +place
        s"  of "                                        StatusString +place
        file-lines     0 (ud,.)                         StatusString +place
        s" ,   Size: "                                  StatusString +place
        text-length    0 (ud,.)                         StatusString +place
        s"  Characters  "                               StatusString +place
        lend-len 0=
        IF      s" ("                                   StatusString +place
                text-length BLOCK-SIZE   /mod 0 (ud,.)  StatusString +place
                s"  Blocks, "                           StatusString +place
                BLOCKLINE-SIZE           /mod 0 (ud,.)  StatusString +place
                s"  Lines, "                            StatusString +place
                                              0 (ud,.)  StatusString +place
                s"  Chars) "                            StatusString +place
        THEN
        ed-forth-count @
        IF      s" (Forth Instances: "                  StatusString +place
                ed-forth-count @ 0 (d.)                 StatusString +place
                s" )  "                                 StatusString +place
        THEN
        overstrike @
        if   s"  <OvrStrk> "
        else s"  <Insert> " then statusstring +place
        
        r> to entry#
        pad$ pad 32 - 32 move           \ restore PAD
        ;M

:M WinEdMessageBox:  ( szText szTitle style -- result )
        MB_TASKMODAL or -rot rel>abs swap rel>abs
        ( hWnd ) NULL Call MessageBox
        ;M

\ the l parameter has already been removed by WINDOW.F, and put
\ into Height and Width

:M On_Size:     ( h m w -- )                  \ handle resize message
        dup SIZE_MINIMIZED = to minimized?      \ w is sub-message
        Refresh: self
        ;M

:M ReSize:      ( -- )
        entry# >r
        GetStack: EditWindow to entry#
         Width: EditWindow  CharWidth: EditWindow / to screen-cols
        Height: EditWindow CharHeight: EditWindow / 1- to screen-rows
        Paint: self
        r> to entry#
        ;M

:M On_SetFocus: ( h m w l -- )
        On_SetFocus: super
        SetFocus: edit-window
        ;M

:M On_KillFocus: ( h m w l -- )
        On_KillFocus: super
        ;M

: reflect-window { wparam \ theWindow -- WID }  \ return ID of reflected window
        window-list
        BEGIN   dup @ dup to theWindow
        WHILE   Showing: theWindow
                IF      wparam LOWORD ( ID ) GetID: theWindow =
                        IF      DROP    \ May 9th, 1998 tjz added DROP
                                theWindow
                                EXIT
                        THEN
                THEN    cell+
        REPEAT  drop
        FALSE   ;

:M WM_COMMAND   { hwnd msg wparam lparam -- res }
        wparam reflect-window ?dup
        IF      \ if message is from a listbox, then reflect it
                \ back to the listbox for processing

                >r hwnd msg wparam lparam WM_COMMAND WM: [ r> ]

        ELSE       hwnd msg wparam lparam WM_COMMAND WM: Super

                \ We must send others to 'Super', so that keyboard
                \ processing will work properly, since 'Super'
                \ handles keyboard WM_COMMAND messages
        THEN
        ;M

\ All SC_xxxx command types always have the high nibble set to 0xF

:M WM_SYSCOMMAND ( hwnd msg wparam lparam -- res )
                over 0xF000 and 0xF000 <>
                IF      over LOWORD
                        DoMenu: CurrentMenu
                        0
                ELSE    DefWindowProc: [ self ]
                THEN    ;M


\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ 41    Support for Drag and Drop files from File Manager to WinEd  
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

:M ExWindowStyle: ( -- )
        ExWindowStyle: super
        WS_EX_ACCEPTFILES or            \ allow files to be dropped on WinEd
        Win32s? 0=      \ if not Win32s, then use Overlapped Window
        IF      WS_EX_OVERLAPPEDWINDOW or
        THEN
        ;M

:M WM_DROPFILES { hndl message wParam lParam \ cFiles drop$ -- res }
        MAXSTRING LocalAlloc: drop$
        >E-unminimize
        SetForegroundWindow: self
        0
        NULL
        -1
        wParam                          \ HDROP structure
        Call DragQueryFile to cFiles    \ -- count of files dropped
        RefreshOff: EditWindow
        FALSE defext_on? dup @ >r !     \ save, but reset the default extension
                                        \ addition
        cFiles 0
        ?DO     MAXCOUNTED
                drop$ 1+ rel>abs
                i
                wParam
                Call DragQueryFile drop$ c!
\                drop$ count '.' scan nip 0=
\                IF      s" ." drop$ +place
\                THEN
                drop$ count "+open-text
        LOOP
        r> defext_on? !                 \ restore the default extension additon
        wParam Call DragFinish drop
        RefreshOn: EditWindow
        ;M


\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ 42    Message support from Win32Forth, allows files to be opened remotely  
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

: remote-window { wParam -- }
        wParam ED_WATCH =
        IF      cursor-line 2 -
        ELSE    cursor-line find-top-margin screen-rows 2 - 2/ min -
        THEN    VPosition: edit-window
        refresh-screen ;

: remote-open   { wParam -- }
        ed-ptr 0= ?EXIT                 \ leave if shared memory not inited
        >E
        wParam ED_WATCH <>
        IF      SetForegroundWindow: self
        THEN    SW_RESTORE Show: self
        ed-filename  ?uppercase count "to-pathend"
        cur-filename ?uppercase count "to-pathend" compare 0=
        \ only move cursor if we are not near the correct location already
        IF      ed-line @ 1- 0max cursor-line dup screen-rows 4 - + between 0=
                IF      ed-line @ 1- 0max to cursor-line
                        wParam remote-window
                THEN
        ELSE    ed-filename count "already-open? >R
                ed-filename count "+open-text
                ed-filename  count "to-pathend"
                cur-filename count "to-pathend" compare 0=
                IF      R@
                        IF      wParam ED_OPEN_BROWSE = to browse?  \ browsing?
                        THEN
                        ed-line @ 1- 0max to cursor-line
                        wParam remote-window
                THEN
                R>DROP
        THEN
        Refresh: WinEdWindow
        refresh-screen
        RefreshCursor: DocWindow
        EditMode: WinEdToolbar ;

: remote-word   ( -- )
        ed-ptr 0= ?EXIT                     \ leave if shared memory not inited
        browse? 0=
        IF      browse-toggle
        THEN
        ed-line @ 1- 0max to cursor-line
        ed-column @       to cursor-col
        highlight-word
        line-cur cursor-line screen-rows 4 - - 0max cursor-line between 0=
        IF      cursor-line screen-rows 2 - 2/ - VPosition: edit-window
        THEN
        refresh-screen
        RefreshCursor: DocWindow
        EditMode: WinEdToolbar ;

: receive-stack ( -- )                                  \ get stack from Forth
        ShowStack: DbgButtonsDlg ;

: receive-debug ( -- )
        ShowDebug: DbgButtonsDlg ;

: no-breakpoint ( -- )          \ not currently in a breakpoint
        beep ;

:M Win32Forth:  { hndl msg wParam lParam -- }   \ respond to Win32Forth messages
        second-copy? 0=
        IF  wParam
            CASE
            \ debug support messages
                  ED_OPEN_EDIT    OF    wParam remote-open     ENDOF
                  ED_OPEN_BROWSE  OF    wParam remote-open     ENDOF
                  ED_WATCH        OF    wParam remote-open     ENDOF
                  ED_WORD         OF    remote-word            ENDOF
                  ED_STACK        OF    receive-stack          ENDOF
                  ED_DEBUG        OF    receive-debug          ENDOF
                  ED_NOTINBP      OF    no-breakpoint          ENDOF
            \ embeded console messages
                  ED_READY        OF    RefreshCursor: DocWindow
                                        FALSE to interpreting?
                                                               ENDOF
                  ED_ALIVE        OF    show-console? 0=
                                        IF      StartConsole: WinEdWindow
                                        THEN
                                                               ENDOF
                  ED_SHUTDOWN     OF    quit-now?    \ we sent a 'bye' to Forth
                                        IF      bye  \ then just terminate
                                        ELSE    CloseConsole: WinEdWindow
                                        THEN
                                                               ENDOF
                  ED_EMIT         OF    view-emit              ENDOF
                  ED_TYPE         OF    view-type              ENDOF
                  ED_CRTAB        OF    view-crtab             ENDOF
                  ED_QCR          OF    view-?cr               ENDOF
                  ED_COL          OF    view-col               ENDOF
                  ED_CLS          OF    view-cls               ENDOF
                  ED_GOTOXY       OF    view-gotoxy            ENDOF
                  ED_GETXY        OF    view-getxy             ENDOF
                  ED_GETCOLROW    OF    view-getcolrow         ENDOF
            ENDCASE
        THEN
        ;M

: init-shared-edit ( -- )
        ed-present @ to second-copy?
        1 ed-present ! ;                \ Set flag of Editors currently running

' init-shared-edit is init-shared-type

: uninit-shared-edit ( -- )
        second-copy? 0=
        IF      0 ed-present !        \ clear flag of Editors currently running
        THEN    ;

' uninit-shared-edit is uninit-shared-type


\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ 43    Automatic save of edit changes after a specifiable number of minutes  
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

:M WM_TIMER     ( h m w l -- res )
        over SAVE_TIMER =
        IF      SAVE_TIMER hWnd Call KillTimer drop
                save-text
        THEN    0 ;M

;Object

: calc-column   ( -- )  \ try to round the click position between characters
        mousex window-lmargin - 0max CharWidth: EditWindow /mod
        swap CharWidth: EditWindow 2 - > IF 1+ THEN
        col-cur + to cursor-col ;

: get-mouse-pos ( -- x y )
        Gethandle: DocWindow get-mouse-xy ;

: update-highlight ( -- )               \ rls January 2nd, 2001 - 16:06
        line-cur mousey CharHeight: DocWindow / +
        file-lines 1- min to cursor-line
        RectHigh
        IF      cursor-col  mcst >
                IF      mcst        to hcst
                        cursor-col  to hced
                ELSE    cursor-col  to hcst
                        mcst        to hced
                THEN
                cursor-line mlst >
                IF      mlst        to hlst
                        cursor-line to hled
                ELSE    cursor-line to hlst
                        mlst        to hled
                THEN
        ELSE
                cursor-line mlst =
                cursor-col  mcst > and
                cursor-line mlst > or
                IF                      \ Highlight forwards from (mlst,mcst)
                        cursor-line to hled
                        cursor-col  to hced
                        mlst        to hlst
                        mcst        to hcst
                        mousex window-lmargin 2 - <
                        IF      cursor-line 1+ to hled
                                cursor-col     to hced
                        THEN
                ELSE    cursor-line mlst =
                        cursor-col  mcst < and
                        cursor-line mlst < or
                        IF              \ Highlight backwards from (mlst,mcst)
                                cursor-line to hlst
                                cursor-col  to hcst
                                mlst        to hled
                                mcst        to hced
                                mousex window-lmargin 2 - <
                                IF      mlst #line.len to hced
                                THEN
                        THEN
                THEN
        THEN
        cursor-col cursor-line #line.len >
        RectHigh 0= and
        IF      hlst #line.len hcst min to hcst
                hled #line.len hced min to hced
        THEN
        refresh-screen ;

0 value tracking?

\ rls January 14th, 2001 - 2:20  added variable scrolling speed
: winedit-track { \ cursor-row excess -- }
        html-link?    ?EXIT
        tracking?     ?EXIT
        mousedown? 0= ?EXIT
        focusing?     ?EXIT
        tracking?     ?EXIT
        TRUE to tracking?
        calc-column
        update-highlight                                \ scroll down
        BEGIN   get-mouse-pos nip Height: DocWindow CharHeight: DocWindow -
                2dup - to excess >
        WHILE   1 +row-scroll
                update-highlight
                WINPAUSE
                CharHeight: DocWindow 4 * 20 excess 2 + */
                500 min 20 max ms
        REPEAT
                                                        \ scroll up
        BEGIN   get-mouse-pos nip dup negate to excess 0 <
                line-cur 0> and
        WHILE   -1 +row-scroll
                update-highlight
                WINPAUSE
                CharHeight: DocWindow 4 * 20 excess 2 + */
                500 min 20 max ms
        REPEAT
                                                        \ scroll right
        BEGIN   get-mouse-pos drop screen-cols 1- CharWidth: DocWindow *
                2dup - to excess >
        WHILE   cursor-col 1+ >col-cursor
                WINPAUSE
                CharWidth: DocWindow 8 * 20 excess 2 + */
                500 min 20 max ms
        REPEAT
        get-mouse-pos drop 0 <=                         \ scroll left
        col-cur 0> and                                  \ if needed
        IF      BEGIN   cursor-col 1- 0max >col-cursor
                        WINPAUSE
                        get-mouse-pos drop dup abs 2 max to excess
                        CharWidth: DocWindow 20 excess */
                        300 min 20 max ms
                        window-lmargin >=
                UNTIL
        THEN
        FALSE to tracking? ;

0 value click-column

: winedit-click { \ cursor-row -- }
        html-link? ?EXIT
        DocWindow >r
        in-ConsoleWindow?
        IF      ConsoleWindow to DocWindow
        ELSE    EditWindow    to DocWindow
        THEN
        GetStack: DocWindow to entry#
        SetFocus: DocWindow
        HideCursor: DocWindow
        mousedown? 0=
        IF      Gethandle: DocWindow Call SetCapture drop
        THEN
        true to mousedown?
        ?CONTROL to RectHigh
        DocWindow r> =
        IF      calc-column
                cursor-col to click-column
                mousey CharHeight: DocWindow /           to cursor-row
                line-cur cursor-row + file-lines 1- min dup cursor-line <>
                IF      -trailing-blanks
                THEN                                     to cursor-line
                ?shift 0=       \ if NOT extending the selection
                IF      cursor-line dup to hlst dup to hled to mlst
                        cursor-col  dup to hcst dup to hced to mcst
                        mousex window-lmargin 2 - <
                        IF      cursor-line 1+ to hled
                                cursor-col     to hced
                        THEN
                        refresh-screen
                ELSE
                        RectHigh
                        IF      cursor-col  hcst <
                                IF      hced to mcst
                                ELSE    hcst to mcst
                                THEN
                                cursor-line hlst <
                                IF      hled to mlst
                                ELSE    hlst to mlst
                                THEN
                                winedit-track
                        ELSE
                                cursor-line hlst =
                                cursor-col  hcst < and
                                cursor-line hlst < or
                                IF      hled to mlst
                                        hced to mcst
                                ELSE    cursor-line hled =
                                        cursor-col  hced > and
                                        cursor-line hled > or
                                        IF      hlst to mlst
                                                hcst to mcst
                                        THEN
                                THEN
                                winedit-track
                        THEN
                THEN
                ReTitle: WinEdWindow
        ELSE    SetFocus: DocWindow
                 ReTitle: WinEdWindow
                   Paint: WinEdWindow
                TRUE to focusing?
                winedit-track
        THEN    ;

: "find-label   ( a1 n1 -- )
        2dup InsertString: findComboEdit
        "CLIP" find-buf place
        Home: DocWindow
        0 to cursor-col
        TRUE to find-label?
        find-text-again         \ then try to find it
        FALSE to find-label?
        line-cur cursor-line 1 - max to line-cur
        no-highlight  ;

: do-html-link  { \ html$ -- }
     MAXCOUNTED LocalAlloc: html$
     mousey CharHeight: DocWindow / line-cur + file-lines 1- min
     #line" html-link? 1- 0MAX /string s" <A HREF" caps-search
     IF  7 /string                         \ remove "<A HREF"
         bl skip '=' skip bl skip
         2dup '>' scan nip -               \ discard after '>'
         -trailing
         2dup  '"' scan nip                \ if link is quoted
         IF    '"' scan 1 /string 2dup
               '"' scan nip -              \ remove the quotes
         THEN
         over c@ '#' =                     \ searching in this document?
         IF  1 /string                     \ then remove leading '#'
             "clip" html$ place        \ save text to find, in case file changes
             cur-filename count "+open-text \ open another copy of same document
             browse? 0=
             IF      browse-toggle
             THEN
             html$ count "find-label
         ELSE                    \ open a file
             s" FILE:" HTML?
             IF  2dup s" .EXE" caps-search nip nip   \ check for executable
                 IF  "CLIP" html$ PLACE
                     html$ +NULL
                     html$ $EXEC
                     IF      beep
                     THEN
                 ELSE
                     2dup '#' scan               \ scan for string search marker
                     2dup 1 /string html$ place  \ save string to search for
                     nip -  "+open-text          \ parse out and open the file
                     browse? 0=
                     IF      browse-toggle
                     THEN
                     html$ c@
                     IF      html$ count "find-label
                     THEN
                 THEN
             ELSE
                 s" HTTP:" HTML?
                 IF  5 + swap 5 - swap                   \ add "HTTP:" back in
                     "ViewWeb-Link
                 ELSE
                     s" NEWS:" HTML?
                     IF  5 + swap 5 - swap               \ add "NEWS:" back in
                         GetHandle: FrameWindow "Web-Link
                     ELSE
                         s" FTP:" HTML?
                         IF  4 + swap 4 - swap           \ add "FTP:" back in
                             GetHandle: FrameWindow "Web-Link
                         ELSE
                             s" MAILTO:" HTML?
                             IF  7 + swap 7 - swap       \ add "MAILTO:" back in
                                 GetHandle: FrameWindow "Web-Link
                             ELSE                \ just a file, or sub web page
                                                 \ of current page
                                 2dup '#' scan   \ scan for string search marker
                                 2dup 1 /string html$ place
                                                 \ save string to search for
                                 nip -           \ -- adr len ;of file to open
                                 from-web?               \ if this is a web file
                                 IF  "build-URL" "HTML   \ then open as web file
                                 ELSE
                                     "+open-text   \ parse out and open the file
                                     browse? 0=
                                     IF      browse-toggle
                                     THEN
                                 THEN
                                 html$ c@
                                 IF  html$ count "find-label
                                 THEN
                             THEN
                         THEN
                     THEN
                 THEN
             THEN
         THEN
     ELSE
         2drop beep
     THEN    ;

: winedit-unclick ( -- )
      mousedown?
      IF      Call ReleaseCapture drop
      THEN
      FALSE to mousedown?
      FALSE to focusing?
          all-lines: DocWindow
      RefreshCursor: DocWindow
      ShowCursor: DocWindow
      html-link?
      IF    k_F11 +k_control +k_shift PushKey
      ELSE  browse?                         \ browsing
            cursor-col click-column = and   \ and save column
            IF
                  on-text?                \ on text
                  IF    highlight-word
                        highlighting?
                        IF      k_F9 PushKey
                                browse? 0= \ destination should be in browse
                                IF      browse-toggle
                                THEN
                        ELSE    beep
                        THEN
                  ELSE  beep
                  THEN
            THEN
      THEN    ;

: winedit-dblclick ( -- )       \ highlight the current word
        FALSE to mousedown?
        FALSE to focusing?
        html-link? ?EXIT
        calc-column
        mousey CharHeight: DocWindow /
        line-cur + file-lines 1- min to cursor-line
        highlight-word ;


\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ 44    Save and restore default settings for WinEd  
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

: save-defaults ( -- )
        DECIMAL                 \ MUST be in decimal when saving defaults
        GetHandle: FrameWindow
        IF    >E
              save-find?                s>d (d.) s" SaveFind"       "SetDefault
              WinEd-web?              s>d (d.) s" WebBrowse"      "SetDefault
              tool-bar? Floating: WinEdToolbar or
                                        s>d (d.) s" ToolBar"        "SetDefault
              Floating: WinEdToolbar  s>d (d.) s" FloatBar"       "SetDefault
              min-tool-bar?             s>d (d.) s" MinToolBar"     "SetDefault
              show-console?             s>d (d.) s" ForthWindow"    "SetDefault
              open-previous?            s>d (d.) s" OpenPrevious"   "SetDefault
              page-lines?               s>d (d.) s" PageMarks"      "SetDefault
              border?                   s>d (d.) s" PrintBorders"   "SetDefault
              recent-files?             s>d (d.) s" RecentFiles"    "SetDefault
              CaseSensitive?            s>d (d.) s" CaseSensitive"  "SetDefault
              sub-dirs?                 s>d (d.) s" SubDirectories" "SetDefault
              all-occur?                s>d (d.) s" AllOccurances"  "SetDefault
              save-margin?              s>d (d.) s" SaveMargin"     "SetDefault
              colorize?                 s>d (d.) s" Colorize"       "SetDefault 
              char-height 5 max         s>d (d.) s" CharHeight"     "SetDefault
              char-width  4 max         s>d (d.) s" CharWidth"      "SetDefault
              printer-lpi               s>d (d.) s" PrinterLPI"     "SetDefault
              def-right-edge            s>d (d.) s" RightEdge"      "SetDefault
              tab-size                  s>d (d.) s" TabSize"        "SetDefault
              console-savelines         s>d (d.) s" SaveLines"      "SetDefault
              save-minutes  0max 60 min s>d (d.) s" AutoSave"       "SetDefault
              printed-columns 30 max 230 min
                                        s>d (d.) s" PrinterCols"    "SetDefault
              cur-filename count        s" PreviousFile"            "SetDefault
              defext$      count        s" DefExt"                  "SetDefault
              mask-buf     count        s" SearchMask"              "SetDefault
              find-buf     count        s" SearchText"              "SetDefault
              path-ptr     count        s" SearchPath"              "SetDefault
              minimized? 0=                 \ ONLY SAVE WINDOW IF NOT MINIMIZED
              IF      GetWindowRect: FrameWindow
                      2over             s>d (d.) s" WindowTop"      "SetDefault
                                        s>d (d.) s" WindowLeft"     "SetDefault
                      rot -             s>d (d.) s" WindowHeight"   "SetDefault
                      swap -            s>d (d.) s" WindowWidth"    "SetDefault
                      console-height drag-thick max
                                        s>d (d.) s" SplitHeight"    "SetDefault
                  drag-barH             s>d (d.) s" SelectWidth"    "SetDefault
              THEN
              GetFaceName: vFont        s" WindowFont"              "SetDefault
              SaveFindStrings
              GetCount: fileMenu 8 min 0        \ only save first 8 file strings
              ?DO     tempBuf i GetString: fileMenu
                      tempBuf count  s" FileString "
                      2dup + 1- i '0' + swap c!           \ append the count 0-7
                      "SetDefault
              LOOP

        THEN ;

: load-defaults ( -- )
    DECIMAL                 \ MUST be in decimal when loading defaults
\ -rbs globalizing registry    ????
\    s" WinEd\"         progreg   place     \ append the WinEd marker
     s" WinEd\"         progreg   place     \ append the WinEd marker
\    current-dir$ count progreg  +place     \ separate defaults in each directory
\                   ':' progreg c+place     \ separate with a ';' char
\    &prognam     count progreg  +place     \ and for each copy of the program
\    progreg count '\' scan 1 /string       \ leave first '\' in the string
\    BEGIN
\            2dup  '\' scan                 \ change the rest to '/' characters
\    WHILE
\            '/' swap c!
\    REPEAT
\    drop 2drop
\    progreg ?+\
\ <---rbs

    s" SaveFind"       "GetDefault dup
                          IF 2dup number? 2drop 0<> to save-find?     THEN 2drop
    s" WebBrowse"      "GetDefault dup
                          IF 2dup number? 2drop 0<> to WinEd-web?   THEN 2drop
    s" ToolBar"        "GetDefault dup
                          IF 2dup number? 2drop 0<> to tool-bar?      THEN 2drop
    s" MinToolBar"     "GetDefault dup
                          IF 2dup number? 2drop 0<> to min-tool-bar?  THEN 2drop
    s" ForthWindow"    "GetDefault dup
                          IF 2dup number? 2drop 0<> to show-console?  THEN 2drop
    s" OpenPrevious"   "GetDefault dup
                          IF 2dup number? 2drop 0<> to open-previous? THEN 2drop
    s" PageMarks"      "GetDefault dup
                          IF 2dup number? 2drop 0<> to page-lines?    THEN 2drop
    s" PrintBorders"   "GetDefault dup
                          IF 2dup number? 2drop 0<> to border?        THEN 2drop
    s" RecentFiles"    "GetDefault dup
                          IF 2dup number? 2drop 0<> to recent-files?  THEN 2drop
    s" CaseSensitive"  "GetDefault dup
                          IF 2dup number? 2drop 0<> to CaseSensitive? THEN 2drop
    s" SubDirectories" "GetDefault dup
                          IF 2dup number? 2drop 0<> to sub-dirs?      THEN 2drop
    s" AllOccurances"  "GetDefault dup
                          IF 2dup number? 2drop 0<> to all-occur?     THEN 2drop
    s" SaveMargin"     "GetDefault dup
                          IF 2dup number? 2drop 0<> to save-margin?   THEN 2drop
    s" Colorize"       "GetDefault dup
                          IF 2dup number? 2drop 0<> to colorize?      THEN 2drop
    s" CharHeight"     "GetDefault dup
                          IF 2dup number? 2drop     to char-height    THEN 2drop
    s" CharWidth"      "GetDefault dup
                          IF 2dup number? 2drop     to char-Width     THEN 2drop
    s" PrinterLPI"     "GetDefault dup
                          IF 2dup number? 2drop     to printer-lpi    THEN 2drop
    save-margin?
    IF
            s" RightEdge"  "GetDefault dup
            IF      2dup number? 2drop to def-right-edge
            THEN
            2drop
    THEN
    page-lines? 0= to print-extended-lines
    s" TabSize"     "GetDefault dup
    IF      2dup number? 2drop to tab-size
    THEN
    2drop
    s" AutoSave"    "GetDefault dup
    IF      2dup number? 2drop to save-minutes
    THEN
    2drop
    s" PrinterCols" "GetDefault dup
    IF      2dup number? 2drop to printed-columns
    THEN
    2drop
    \ added new clipping to DEFEXT$, for new max length of 16 chars
    s" DefExt"      "GetDefault dup
    IF      2dup DEFEXTMAX min defext$ place
    THEN
    2drop
    set-filter
    s" SearchMask"  "GetDefault dup
    IF      2dup "CLIP" mask-buf  place
    THEN
    2drop
    save-find?
    IF
            s" SearchText"     "GetDefault dup
            IF      2dup "CLIP" find-buf  place
            THEN
            2drop
    THEN
    s" SearchPath"   "GetDefault dup
    IF      2dup "CLIP" path-ptr  place
    THEN
    2drop
    s" WindowLeft"   "GetDefault number? 2drop
    s" WindowTop"    "GetDefault number? 2drop  SetOrigin: FrameWindow
    s" WindowHeight" "GetDefault dup
    IF      2dup number? 2drop to start-height
    THEN
    2drop
    s" WindowWidth"  "GetDefault dup
    IF      2dup number? 2drop to start-width
    THEN
    2drop
    s" SplitHeight"  "GetDefault dup
    IF      2dup number? 2drop to console-height
    THEN
    2drop
    s" SaveLines"    "GetDefault dup
    IF      2dup number? 2drop to console-savelines
    THEN
    2drop
    s" SelectWidth"  "GetDefault dup
    IF      2dup number? 2drop to drag-barH
    THEN
    2drop
    s" WindowFont"   "GetDefault dup
    IF
            2dup SetFaceName: vFont
    THEN
    2drop
    s" FloatBar"     "GetDefault dup
    IF      2dup number? 2drop to floating-bar?
    THEN
    2drop
    ;

: load-more-defaults ( -- )
        LoadFindStrings
        8 0
        ?DO     s" FileString "
                2dup + 1- 7 i - '0' + swap c!   \ append the count 0-7
                "GetDefault
                IF      ['] "OpenMenuFile InsertMenuItem: fileMenu
                ELSE    drop
                THEN
        LOOP    ;

: _open-previous ( -- )
                s" PreviousFile"  "GetDefault "CLIP" "open-text ;

' _open-previous is open-previous
                           
: View-before-bye { \ last-entry -- }
      decimal
      -1 to last-entry
      entry# >r
      save-defaults
      FALSE to term-canceled?         \ initially NOT canceled
      save-console
      show-console?                   \ IF Forth present, tellit to terminate
      IF    \ 1 tells Win32Forth not to tell me to close my console window
            1 WM_TERMINATE win32forth-message
      THEN
      entry-max 0
      DO    i to entry#             \ select the hyper file index
            edit-changed?
            IF    save-minutes 0>
                  IF    save-text
                  ELSE  i to last-entry
                        refresh-screen
                        z" Save the changes to the current file before quitting?"
                        z" The Current File Has Been Modified!"
                        MB_YESNOCANCEL MB_ICONSTOP or
                        WinEdMessageBox: WinEdWindow
                        CASE
                              IDYES     OF    save-text               ENDOF
                              IDNO      OF    FALSE to edit-changed?
                                              update-mirrors          ENDOF
                              IDCANCEL  OF    TRUE to term-canceled?  ENDOF
                        ENDCASE
                  THEN
            THEN
            term-canceled? ?LEAVE
      LOOP
      last-entry -1 <>
      IF      last-entry SetStack: EditWindow
      ELSE    r@ to entry#
      THEN    r>drop ;

also hidden

: view-release  ( -- )                          \ release the editors buffers
        DestroyWindow: FrameWindow
        text-ptr ?dup IF release 0 to text-ptr THEN
        line-tbl ?dup IF release 0 to line-tbl thEn  ;

unload-chain chain-add-before view-release         \ make sure memory is released

: viewbye       ( -- )
        view-before-bye
        term-canceled? 0=
        IF      _BYE
        THEN    ;

: "viewmessage  ( a1 n1 )       \ force all message window on top of application
        GetHandle: FrameWindow
        IF      StartPos: FrameWindow 100 100 rot + >r + r> message-origin
        THEN
        _"message ;

: "topviewmessage  ( a1 n1 )
        GetHandle: FrameWindow
        IF      StartPos: FrameWindow 100 100 rot + >r + r> message-origin
        THEN
        _"top-message ;


\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ 45    Include some improvements  by Robert Ackerman for John Peters  
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

\ RDA for JAP on October 10, 2001

: +SCR ( -- )           \ down 1 screen ending with 2 or more empty lines
        0               \ count of empty lines
        BEGIN   dup -1 > cursor-line file-lines 1- < and \ ?end of file
        WHILE
                cursor-line 1+ to cursor-line   \ inc line
                get-cursor-line cur-buf @ 0=    \ inc count if empty line
                IF      1+
                ELSE    dup 1 >
                        IF      drop -1
                        ELSE    drop 0
                        THEN
                THEN
        REPEAT
        -1 =
        IF      cursor-line to line-cur refresh-screen
        ELSE    line-cur to cursor-line
        THEN ;

: -SCR  ( -- )                  \ up 1 screen ending with 2 or more empty lines
        cursor-line 2 >                         \ at least 2 lines above
        IF      cursor-line 1- to cursor-line   \ up one line
                get-cursor-line cur-buf @ 0=    \ line is empty
                IF cursor-line 1- to cursor-line  \ up a line
                        get-cursor-line cur-buf @ 0=   \ true->2 empty lines
                        IF
                                BEGIN   cur-buf @ 0= cursor-line 0> and \ until top or non-empty
                                WHILE   cursor-line 1- to cursor-line
                                        get-cursor-line
                                REPEAT
                        THEN
                THEN
        THEN
        0
        BEGIN   dup 2 < cursor-line 0> and
        WHILE
                cursor-line 1- to cursor-line
                get-cursor-line cur-buf @ 0=
                IF      1+
                ELSE    drop 0
                THEN
        REPEAT
        cursor-line + dup to line-cur to cursor-line
        refresh-screen ;

\ Here is the version with no line length limit:
: OVERSTRIKE-CHARACTER  ( char -- )             \ was _overstrike
        dup bl >= over 0xff <= and
        IF      get-cursor-line
                cur-buf lcount drop             \ start of text in buffer
                cursor-col + c!                 \ put character into buffer
                cur-buf @ cursor-col max
                1+ "LCLIP" cur-buf !            \ increment buf size
                put-cursor-line
                file-has-changed
                1 +col-cursor
        ELSE    drop beep
        THEN ;

: INSERT/OVERSTRIKE-CHARACTER  ( char -- ) \ modified word replaces the original
        browse?
        IF      drop EXIT
        THEN
        delete-highlight

        ( Test for ins/over mode and do it. jap )
        overstrike @
        IF overstrike-character      \ was _overstrike-character or what!!
        ELSE _insert-character
        THEN
        ?wrap-word
        refresh-line ;


\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ 46    These words handle the backspace properly when in the overstrike mode
\       Robert Ackerman with John Peters on March 9th, 2002 - 10:15 to 11:20 
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

: DEL-CHAR      ( -- )          \  sub for k_delete  \ Working ok.
        overstrike @
        IF      bl overstrike-character
        ELSE    delete-character
        THEN ;

: DO-BS         ( -- )          \ sub for k_backspace
        overstrike @
        IF      cursor-col 0>
                IF      cursor-col 1- dup to cursor-col
                        bl overstrike-character
                        to cursor-col
                THEN
        ELSE    do-backspace
        THEN ;
defer xit
: _xit ( rda )  gethandle: winedwindow   focus-console  drop  r> drop  exit  ;

defer _Control+W  \ for use by an application jap
' close-text is _control+w

: toggle-colons-only   colons-only 0= to colons-only  refresh-screen ; \ RDA
: toggle-insert  overstrike @ 0= overstrike !
                 ReTitle: WinEdWindow ;


\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ 47    The main editor loop
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

: VIEW-KEY-LOOP ( -- )
      BEGIN
            depth to saved-depth            \ so we can check stack depth
\ [rda] keep the two flags separate
\           browse?
\           IF      false to edit-changed?  \ not modifiable
\           THEN  
\ [jap] end of back slashed  Did not work well
            ibeam-cursor
            RefreshCursor: DocWindow
            ShowCursor: DocWindow
            EditMode: WinEdToolbar

            command-args c@
            IF    TRUE to interpreting?
                  BEGIN key?                   \ handle keyboard interpretation
                        interpreting? OR
                  WHILE key?
                        IF    key rem-sendkey  \ just send keys to Forth console
                        THEN
                  REPEAT
                  end-doc split-line
                  command-args count "CONSOLE
                  command-args off
            THEN
            BEGIN   s"  " StatusString count SetStatus: WinEdWindow
                    key                        \ handle keyboard interpretation
                    ConsoleWindow DocWindow =
                    interpreting? and
            WHILE   rem-sendkey                \ just send keys to Forth console
            REPEAT
              SetFocus: DocWindow
            HideCursor: DocWindow
            CASE


\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ 48    Control key bindings  
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

      'A'     +k_control            OF      highlight-all               ENDOF
      'A'     +k_control +k_shift   OF      toggle-colons-only          ENDOF
      'B'     +k_control            OF      debug-word                  ENDOF
      'B'     +k_control +k_shift   OF      debug-word                  ENDOF
      'C'     +k_control            OF      copy-text                   ENDOF
      'C'     +k_control +k_shift   OF      >F                          ENDOF
      'D'     +k_control            OF      debug-buttons               ENDOF
      'E'     +k_control +k_shift   OF      expand-tabs                 ENDOF
      'E'     +k_control            OF      my-application              ENDOF
      'F'     +k_control            OF      replace-text                ENDOF
      'F'     +k_control +k_shift   OF      find-in-files               ENDOF
      'G'     +k_control            OF      delete-character            ENDOF
      'H'     +k_control +k_shift   OF      make-hex                    ENDOF
( > ) 'L'     +k_control            OF      load-active-file            ENDOF
      'M'     +k_control +k_shift   OF      replay-macro                ENDOF
      'N'     +k_control            OF      new-text                    ENDOF
      'O'     +k_control            OF      open-text                   ENDOF
      'O'     +k_control +k_shift   OF      open-text-highlighted       ENDOF
      'P'     +k_control            OF      print-text                  ENDOF
      'P'     +k_control +k_shift   OF      text-options                ENDOF
      'Q'     +k_control            OF      highlight-mark              ENDOF
      'R'     +k_control            OF      reformat-text               ENDOF
      'R'     +k_control +k_shift   OF      WinEdWindow repeat-amacro   ENDOF
      'S'     +k_control            OF      save-text                   ENDOF
      'S'     +k_control +k_shift   OF      Start/Stop-macro            ENDOF
      'T'     +k_control            OF      word-delete                 ENDOF
      'T'     +k_control +k_shift   OF      word-undelete               ENDOF
      'U'     +k_control            OF      highlight-case-toggle       ENDOF
      'V'     +k_control            OF      Paste-text                  ENDOF
      'V'     +k_control +k_shift   OF      Paste-date/time             ENDOF
\     'W'     +k_control         OF  s"  " insert-string character-left ENDOF
      'W'     +k_control            OF      _control+w                  ENDOF              
\     'W'     +k_control            OF      close-text                  ENDOF
      'W'     +k_control +k_shift   OF      close-all-text              ENDOF
      'X'     +k_control            OF      cut-text                    ENDOF
      'Y'     +k_control            OF      line-delete                 ENDOF
      'Z'     +k_control            OF      word-undelete               ENDOF


\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ 49    Function key bindings   +SCR is 1 page down
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

      k_F1    ( rbs )               OF      s" HTM\STARTUP.TXT" Prepend<home>\ "browse    ENDOF   
      k_F1    +k_control            OF      help-link                   ENDOF
      k_F2                          OF      toggle-colons-only          ENDOF
      k_F3                          OF      find-text-again             ENDOF
      k_F3    +k_shift              OF      back-find-text-again        ENDOF
      k_F3    +k_control            OF      find-text-highlight         ENDOF
      k_F3    +k_shift +k_control   OF      replace-text                ENDOF
      k_F5                          OF      replay-macro                ENDOF
      k_F5    +k_control +k_shift   OF      cwords                      ENDOF
      k_F7                          OF      browse-toggle               ENDOF
      k_F9                          OF      hyper-link                  ENDOF
      k_F9    +k_control            OF      next-hyper-link             ENDOF
      k_F9    +k_shift              OF      browse-toggle               ENDOF
      k_F9    +k_control +k_shift   OF      word-link                   ENDOF
      k_F10                         OF      close-text                  ENDOF
      k_F10   +k_shift              OF      save-all-text bye           ENDOF
      k_F11                         OF      temp-text                   ENDOF
      k_F11   +k_control +k_shift   OF      do-html-link                ENDOF
      k_F12   +k_control +k_shift   OF      save-text-pc                ENDOF
      k_F12                         OF      save-and-load               ENDOF


\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ 50    PgDn, End, Home
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

      k_down                        OF      1 +row-cursor               ENDOF
      k_down  +k_control            OF      1 +row-scroll               ENDOF
      k_down  +k_shift              OF      highlight-down              ENDOF
      k_end                         OF      end-line                    ENDOF
      k_end   +k_shift              OF      highlight-end-line          ENDOF
      k_end   +k_shift +k_control   OF      highlight-whole-line        ENDOF
      k_end   +k_control            OF      end-doc                     ENDOF
      k_home                        OF      home-line                   ENDOF
      k_home  +k_shift              OF      highlight-home-line         ENDOF
      k_home  +k_control            OF      home-doc                    ENDOF


\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\       Arrow left
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

      k_left                        OF      character-left              ENDOF
      k_left  +k_control            OF      word-left                   ENDOF
      k_left  +k_shift              OF      highlight-left              ENDOF


\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ 51    PgDn  PgUp        
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

      k_pgdn                        OF      1 +page-cursor              ENDOF
      k_pgdn  +k_control  ( jap )   OF      +SCR ( next-link )          ENDOF
      k_pgdn  +k_shift              OF      +SCR  ( jap )               ENDOF
      k_pgup                        OF      -1 +page-cursor             ENDOF
      k_pgup  +k_control            OF      -SCR ( ORG prev-link )      ENDOF
      k_pgup  +k_shift              OF      -SCR  ( jap )               ENDOF


\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\       Right-Arrow   Up-Arrow
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
      k_right                       OF      character-right             ENDOF
      k_right +k_control            OF      word-right                  ENDOF
      k_right +k_shift              OF      highlight-right             ENDOF
      k_up                          OF      -1 +row-cursor              ENDOF
      k_up    +k_control            OF      -1 +row-scroll              ENDOF
      k_up    +k_shift              OF      highlight-up                ENDOF


\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ 52    Mode, Select, Copy, Paste, Tab, Del, Goto-line, Insert\Over-strike
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

      k_tab                         OF      insert-tab                  ENDOF
      k_tab   +k_shift              OF      back-tab                    ENDOF
      k_insert       ( jap )        OF      toggle-insert               ENDOF
      k_insert +k_shift             OF      paste-text                  ENDOF
      k_insert +k_control           OF      copy-text                   ENDOF
      k_delete ( orig may be best ) OF      delete-character            ENDOF
\     k_delete     ( jap )          OF      del-char                    ENDOF
      k_delete  +k_shift            OF      cut-text                    ENDOF
      k_delete  +k_control          OF      word-delete                 ENDOF
      k_BACKSPACE    ( jap )        OF      do-bs                       ENDOF
      k_BACKSPACE   +k_shift        OF      next-window                 ENDOF
      k_ESC   ( RDA 6-30-02 )       OF      xit                         ENDOF
      k_LF    ( Control Enter )     OF      goto-line                   ENDOF
      k_CR    ( ENTER key )         OF      do-cr                       ENDOF
                                        dup insert/overstrike-character
        ENDCASE
      AGAIN   ;


\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ 53    Initalization
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

: view-error    ( -- )   \ Is this a test word or is it used some where????      
        .rstack
        view-key-loop ;

: WinEd-init  ( -- )
      ['] view-before-bye is before-bye       \ called by WM_CLOSE
      ['] viewbye         is bye
      ['] "viewmessage    is "message
      ['] "topviewmessage is "top-message
      ['] view-error      is win-abort

      edit-window    to EditWindow
      edit-window    to DocWindow
      console-window to ConsoleWindow

      ['] winedit-click        SetClickFunc: EditWindow
      ['] winedit-unclick    SetUnClickFunc: EditWindow
      ['] winedit-track        SetTrackFunc: EditWindow
      ['] winedit-dblclick  SetDblClickFunc: EditWindow
      ['] winedit-click        SetClickFunc: ConsoleWindow
      ['] winedit-unclick    SetUnClickFunc: ConsoleWindow
      ['] winedit-track        SetTrackFunc: ConsoleWindow
      ['] winedit-dblclick  SetDblClickFunc: ConsoleWindow

      GetFilter: ViewText filter-save place  \ preserve unmodified filter string
      FALSE to browse?
      0 to left-margin
      Win32s?       to using-Win32s?         \ so we don't have to perform CALLs
      NT? Win98? or to using98/NT?
      0             to right-edge
      false to edit-changed?
      cur-buf off

      COLOR_WINDOWTEXT Call GetSysColor NewColor: normalFG
      COLOR_WINDOW     Call GetSysColor NewColor: normalBG ;

: beep2drop     ( adr len -- )
                beep 2drop ;

: WINED       ( -- )
        WinEd-init
        load-defaults
        command-options
        Start: FrameWindow
        floating-bar?
        IF      Float: WinEdToolbar
        THEN
        load-more-defaults
        new-text
        console-text
        make-new-text
        open-initial-file
        cursor-line find-top-margin - VPosition: EditWindow

        command-args c@
        show-console? 0= AND
        IF      StartConsole: WinEdWindow
        THEN
        show-console?
        IF      0 VPosition: ConsoleWindow
        THEN
        show-console?                           \ if Forth is displayed
        file-to-edit$ c@ 0= and                 \ and no file specified
        minimized-EditWindow? or
        minimized-ConsoleWindow? 0= and
        IF      SetFocus: ConsoleWindow
                ConsoleWindow to DocWindow
        ELSE    SetFocus: EditWindow
                EditWindow to DocWindow
        THEN
        GetStack: DocWindow to entry#
        refresh-screen
        no-highlight
        view-key-loop
        EXIT ;

: re-enter SetFocus: EditWindow
           EditWindow to DocWindow
           view-key-loop ;

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ 54    The defaults 
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

: init-defaults ( -- )
                current-dir$ count path-ptr place
                find-buf off ;

init-defaults

forth-msg-chain off     \ disable WinEd's ability to recognize windows
                        \ messages through the console.
                        \ We don't want the editor trying to insert a breakpoint
                        \ into itself.

.free                   \ how much memory did we really use?

-1 ed-forth-count +!    \ remove myself from Forth count

with-img ' wined turnkey WinEd    \ save WinEd.EXE and WinEd.IMG
\ You may need to use BYE and boot up again for it all to work properly.  jp

1 pause-seconds


\s \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ 55    Wish list for Wined.    The numbers incicate the relative importance
|  \\\\\\\\\\\\\\ see also C:\win32for\src\wish.f \\\\\\\\\\\\\\\\\\\\\\\\\\

xx Make Colorize.f stop coloring words between a (( and a )) or after a \ as if
they were Forth words.

99 Retain the list of files in the narrow window on the left of the editor
window like it does in the pull down menu. It would save having to click on
File, Open  and give the same result. Would this have to wait for the C
unwrapping party?

90 Enable the use of the ALT key in combination with the letter keys. jap

12 Add ability to colorize words with italics, underline, and bold even though
   bold messes up the cursor location as it looses track with wider chars. jap

40 Make colorize quit coloring words after a  \s  at the end of the file. jap

00 A smart combination of ED <file> and EDIT <word>  Some code sent in by rda
   which is yet to be understood and implemented by jap. jap

10 Ctrl+PgDn should go to the bottom of the file, not stop at last paragraph

10 Ring the bell when user tries to edit in the browse mode.  (More friendly)
   One tends to type quite a bit and then realize it is not working

02 F2 colons-only mode is very nice.  Please make it allow editing the file.

02 Every word to briefly state what it does.  Most have stack comments.
   When we can edit in the colons-only mode this will be easier.

10 BOLD needs an icon and necessary code symilar to MS applications

02 HTML interpreter to display HTML files the same as standard browsers
  big job. . . mybe switch to VIM editor.  You can Google it. rls

10 Move the Find - Replace Text window pop up and out of the way by default.
   Some pointers came in by email.  Not yet implemented.

04 COLORIZATION of ((  Comments to be all the same color.  Colorizaton of (((
All text till the next ))) to be in red. \ Some progress but needs more
testing.

03 Rename wined.col to Colorize.f  Allows the colors to show in th file.

?? Fix vertical gray line to correspond to actual maximum number of columns.
Add the ability to VIEW the source of words in Objects.  When the "Find Text
in Files" function is called, and then one of the files is inviked by clicking
on the file name, the file comes up in WinEd with the ENTIRE line containing
the text is highlighted.  That in itself is not too bad.  BUT secondary
problems develop if you do not carefully un-highlight that line  before
searching for additional occurrences of that text in the same file.  It seems
preferable to me that ONLY the desired text should be highlighted. rls

08 Colorization of multi-line comments using (( ))  and ((( and )))
"comment:" and "comment;") does not work very well.  I think that fixing this
involves a fair amount of effort.  There are somewhat similiar problems
involving display of HTML files. rls jap and others.


\s \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ 56    Change comments.  Latest on bottom.  Ctrl+Shift+V inserts date
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

rda Circa April 2002
  The escape key is crudely functioning as a escape from the editor to the
  console window if the console window is open right behind the editor.
  Otherwise it drops back to what ever window is in back of the editor. ????

rls Circa July 2002
  Indentation is reduced to cut down on long line lengths where possible. rls

rls September 2002
  The printing function has now been extended to allow very long lines to be
  displayed.  Continuation lines (with a leading "+" sign to the left of the
  left margin) will be printed under the following circumstances: (1) the
  "show page break markers" dialog box item (under "Edit/WinEd Preferences")
  must be UNchecked.  (2) The area to be printed is selected by highlighting
  the selected area, and then choosing the "Selection" option under the
  "Print" function.  (3) The lines are longer than that nominally allowed for
  the specified paper size and orientation.

  You may print selected regions of text by highlighting any number of
  sequential lines of text, and then choosing the "Selection" option in the
  Print dialog box.  Text is highlighted by clicking the mouse in the blue
  area to the left of the first line you wish to print.  Move to the last line
  but press the "Shift" key before clicking the mouse.  Finally, select the
  File/Print function, and click on the "Selection" box, and then the "OK" box.

  Note to future implementors: Code having comments with the sequence:
        rls - page   have partially implemented the ability to display the
  proper page numbers when using the "Continuation Line" discussed above. Full
  and proper implementation of that feature appears to be somewhat complicated
  and not worth the effort at this time.  It could be done if there is
  sufficient user interest.

jap October 14th, 2002
  F2 is now COLONS-ONLY shows only lines with a bang left colon.  You can use
     it to see if there are any words with missing documentaton.
  F7 is toggle from edit to browse mode and browse mode is now black bordered.

  Ctrl+PgDn jumps to the start of each paragraph that is deleniated by a
  pair of double blank lines at the top of each paragraph.

November 15, 2002 A pole on Yahoo was 4 to 0 to ok changing the command to flip
  thru open files to Shift-PgUp and Shift-PgDn from a Control-Page up or down.
  This makes room to use Control-PgUp and Control-PgDn for next and previous
  virtual screen or paragraph as deleniated by two consecutive blank lines.

jap Circa November 2002 Control-W is deffered.  It is now bound to close-text
as Tom Zimmer designed it.  I prefer it to be bound to push-right as follows:
    : Push-right  s"  " insert-string character-left ;
    ' Push-right is _Control+w

VIEW-KEY-LOOP is split in to pargraphs.  It is the main loop in the editor.

jap October 15th, 2002 - 17:18  Initals of developers are listed below.
     TZ      Tom Zimmer
     {BE}    Bruno Gauthier
     RLS     Robert L. (Bob) Smith
     RDA     Robert Dudley Ackerman
     JAP     John A. Peters
     RBS     Rainbow Sally
     DBU     Dirk Busch

jap and rbs F1 STARTUP.TXT menu of hypertext links is the first of the file
jap October 17th, 2002  F2 and F7 now test good on 6.01 system.
rda October 17th, 2002  Fixed the overstrike/insert toggle.
rda & jap   17th, 2002 Added a CR in boot up notice in the word win32For.cfg
rda Circa November 2002 Added Brown color in place of yellow. Yellow is missing
rda & jap November 2002 Brown color used for border in browse mode.
jap November 20th, 2002 Changed wined.col to colorize.f
rda November 24th, 2002 Editor always saves even if you switch to browse mode.
jap November 28th, 2002 Search menu: Changed SEARCHLIST.TXT to lower case.
rda Circa October, 2002 Added <OvrStrk> and <insert> on bottom status line.
jap December 1st, 2002 Re did the reliable save after corrections to my edit.
jap December 4th, 2002 Updated the version numbers on the top blue line.
dbu April 23rd, 2003 Wheelmouse support added


Bottom







