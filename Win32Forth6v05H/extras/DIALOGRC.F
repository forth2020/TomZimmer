\ DIALOGRC.F     a Dialog Ressource Compiler wordset
\                                by mis@kitchen.forth-ev.de  (Michael Schroeder)

\ April 22nd, 1996 - 15:05 tjz
\ modified to move NEW$ back into POINTER.F, and to remove FIND-DIALOG-TEMPLATE
\ which is not needed any longer since FIND-DIALOG-ID has been changed to be
\ compatible with the use of FIND-DIALOG-TEMPLATE.

\ --------------------------------------------------------------
\       A Dialog Ressource Compiler Add-On for Win32Forth
\
\ The package contains the following files:
\       - DIALOGRC.F
\               this file, the main source of the ressource
\               compiler wordset
\       - DIALOGS.F
\               the dialog ressources from FORTHDLG.DLG
\               ported to the DIALOGRC syntax with the original
\               ressource scripts as comments. Shows the way of
\               porting existing ressource scripts and was used
\               to test the output of dialogrc.
\       - DLGTEST.F
\               the original file FORTHDLG.F with some changes to
\               use the new form of inline compiled dialog templates.
\               I used it, to see if DIALOGRC proper works.
\       - DIALOGRC.TXT
\               glossary of the Dialog Ressource Compiler wordset
\               (still incomplete)
\
\ All of these files are released to public domain.
\
\ --------------------------------------------------------------

\ --------------------------------------------------------------

\ -------------------- Wide String (UNICODE) Literals ----------

DEFER \N->CRLFW   ( a1 n1 -- )    ' 2DROP IS \N->CRLFW

: ,"W    ( -- )         \ compile string with 2-byte wide chars (UNICODE)
        [CHAR] " PARSE
        HERE >R DUP W,
        BOUNDS ?DO
            I C@ W,
        LOOP
        0 W,  ALIGN
        R> WCOUNT \N->CRLFW ;

CODE (("W))    ( -- w-string )
        push    ebx
        mov     eax, [ebp]
        mov     ebx, eax
        sub     ebx, edi     \ relative address
        sub     ecx, ecx
        mov     cx, [eax]
        shl     ecx, # 1
        add     eax, ecx
        add     eax, # 7     \ account for count word and extra null word after string
        and     eax, # -4    \ align
        mov     [ebp], eax
        next    end-code

: (C"W)  ( -- w-string )  (("W)) ;

: C"W    POSTPONE (C"W)  ,"W  ; IMMEDIATE

: (S"W)  ( -- addr len )  (("W)) WCOUNT ;

\ : S"W    ( -- )
\        STATE @
\        IF      POSTPONE (S"W)  ,"W
\        ELSE    [CHAR] " WORD
\                DUP C@ TEMP$ W!
\                COUNT 1+ 0 ?DO          \ run through word plus trailing zero
\                    DUP I + C@  TEMP$ 2 + I 2* + W!
\                LOOP
\                DROP TEMP$ WCOUNT
\        THEN ; IMMEDIATE

: S"W    { \ widestring -- addr len }
        STATE @
        IF      POSTPONE (S"W)  ,"W
        ELSE    NEW$ TO widestring      \ get a dynamic string buffer
                [CHAR] " PARSE
                MAXSTRING 2 - 2/ MIN
                DUP widestring W!
                0 ?DO                   \ run through parsed string
                    DUP I + C@  widestring 2 + I 2* + W!
                LOOP
                DROP
                0 widestring WCOUNT 2* + W! \ append trailing NULL
                widestring WCOUNT
        THEN ; IMMEDIATE

\ "PARSE-TEXT" ( -<"text">- -- addr len ) parses out quote delimited
\ text and return it as unpacked string; The string is allocated from a
\ dynamic buffer with NEW$, and will be overwritten after several
\ executions of NEW$. "PARSE-TEXT" also detect \T embeded in the
\ text and replaces it with a TAB char

: "PARSE-TEXT"  { \ strbuf strlen -- addr len }
        NEW$ TO strbuf
        source >in @ /string
        [char] " scan 1 /string                 \ skip past first quote
        2dup [char] " scan                      \ upto next quote
        2dup 2>r nip -                          \ parse out the string
        255 min dup TO strlen
        2dup [char] \ scan 2dup 2>r nip -       \ leading part of string
        strbuf place                            \ save in strbuf
        2r> dup
        if      over 1+ c@ upc [char] T =
                if      9         strbuf c+place
                        2 /string strbuf  +place
                        -1 +TO strlen
                else    strbuf +place
                then
        else    2drop
        then
        0 strbuf count + c!                     \ null terminate name
        source nip 2r> 1 /string nip - >in !    \ adjust >IN
        strbuf COUNT
;

\ convert ANSIChar-String to WideChar-String (UNICODE)
: ANSI>WIDECHAR { addr len \ strbufw -- strbufw len }
        len 2 + 2* Dynalloc TO strbufw
        len strbufw W!
        len 0 ?DO
                addr I + C@
                strbufw I 1+ 2* + W!
        LOOP
        0 strbufw wcount 2* + W!
        strbufw wcount
;

\ -------------------- Wide String Literals --------------------
\ --------------- test and self documentation ------------------

\ : t1 c"w compile quote wide string!" wcount 2* dump ;
\ t1
\ 385846  63 00 6F 00 6D 00 70 00  69 00 6C 00 65 00 20 00  c.o.m.p.i.l.e. .
\ 385862  71 00 75 00 6F 00 74 00  65 00 20 00 77 00 69 00  q.u.o.t.e. .w.i.
\ 385878  64 00 65 00 20 00 73 00  74 00 72 00 69 00 6E 00  d.e. .s.t.r.i.n.
\ 385894  67 00 21 00                                       g.!.

\ : t2 s"w state smart quote wide string!" 2* dump ;
\ t2
\ 385938  73 00 74 00 61 00 74 00  65 00 20 00 73 00 6D 00  s.t.a.t.e. .s.m.
\ 385954  61 00 72 00 74 00 20 00  71 00 75 00 6F 00 74 00  a.r.t. .q.u.o.t.
\ 385970  65 00 20 00 77 00 69 00  64 00 65 00 20 00 73 00  e. .w.i.d.e. .s.
\ 385986  74 00 72 00 69 00 6E 00  67 00 21 00              t.r.i.n.g.!. ok..

\ --------------------------------------------------------------
\ -------------------- Ressource Scope -------------------------
: BEGIN-RESSOURCE   ( --<RessourceName>-- -- a 6 )
    NOSTACK                         \ suppress stack check at line end
    CREATE HERE                     \ start of named ressource
    0 ,                             \ placeholder for the ressource length
    6                               \ check value for ?pairs
;                                   \ to check scope integrity

: END-RESSOURCE
    6 ?PAIRS                        \ abort if scope IDD isn't paired
    HERE OVER - CELL- SWAP !        \ fill the placeholder
    checkstack                      \ re-enable stack check
;

\ --------------------------------------------------------------
\ -------------------- Dialog Scope ----------------------------

0 VALUE STYLEFLAGS              \ hold the desired window style, until it will compiled
\ --------------------------------------------------------------
0 VALUE pointsize               \ same for the desired font size
named-new$ FontName             \ same for some global memory handles
named-new$ Caption              \ place to hold the caption string

: DIALOG-CAPTION ( 6 --<quoted caption text>-- -- 6 )
        "PARSE-TEXT" ANSI>WIDECHAR      \ get following "quoted text"
        MAXSTRING 2/ 2 - MIN
        1+ 2* DUP Caption W!
        Caption 2 + SWAP MOVE
        DUP 6 ?PAIRS
        ;

: DIALOG-FONT ( 6 points --<quoted font name>-- -- 6 )
        STYLEFLAGS DS_SETFONT OR TO STYLEFLAGS  \ perhabs you forgot to declare it
        "PARSE-TEXT" ANSI>WIDECHAR              \ get following "quoted text"
        MAXSTRING 2/ 2 - MIN
        1+ 2* DUP FontName W!
        FontName 2 + SWAP MOVE
        TO pointsize
        DUP 6 ?PAIRS
        ;

: BEGIN-DIALOG { id x y cx cy -- addr0 addr1 7 }
        ALIGN HERE >R                           \ save start of ressource entry
        \ first create a 32 Byte generic ressource header (not part of the template)
        0 ,                     \ placeholder for length of the following res. data
        0x00000020 ,            \ length of this header (32 Bytes)
        0x0005FFFF ,            \ constant of unknown meaning (surely RC-dependent)
        0xFFFF W,  id  W,       \ the IDD_XXX constant, that identifies the dialog
        0 ,                     \ unknown meaning, or reserved
        0x04091030 ,            \ dito
        0 , 0 ,                 \ dito
        \ now start compiling the template header
        STYLEFLAGS DUP 0= ABORT" No dialog style specified!" ,      \ STYLEFLAGS
        0 ,             \ unknown value, perhabs language identifier??, always Null
        HERE >R         \ address to patch the following Word
        0  W,           \ Word: Number of Items - placeholder
        x  W,  y W,     \ compile x, y
        cx W, cy W,     \ ...and cx, cy
        0 ,     \ bother szMenuName[] and szClassName[] aren't supported
        Caption WCOUNT HERE SWAP        \ source dest len
        DUP ALLOT MOVE
        1 Caption !                     \ reset Caption to Null-string
        STYLEFLAGS DS_SETFONT AND IF
                pointsize W, 0 TO pointsize
                fontname WCOUNT HERE SWAP       \ source dest len
                DUP ALLOT MOVE ALIGN
                1 fontname !                    \ reset fontname to Null-string
        THEN 0 TO STYLEFLAGS                \ reset STYLEFLAGS
        ALIGN                           \ once more for safety
        DUP 6 ?PAIRS                    \ check ressource scope
        2R> 7                           \ open another scope ... until END-DIALOG
        ;


: END-DIALOG ( 6 addr0 addr1 7 -- 6 )   \
        7 ?PAIRS
        DROP                            \ NumberOfItems already patched
        HERE OVER -                     \ total distance
        OVER CELL+ @ -                  \ - header-len
        SWAP !                          \ patch in field data-len
        ;

\ -------------------- Control Compiler Words ------------------

\ some common Class-ID constants for use with CONTROL
0X80 CONSTANT CLASS_BUTTON
0X81 CONSTANT CLASS_EDIT
0X82 CONSTANT CLASS_STATIC
0X83 CONSTANT CLASS_LISTBOX
0X84 CONSTANT CLASS_SCROLLBAR
0X85 CONSTANT CLASS_COMBOBOX

\ GENERIC_CONTROL ( txt-addr txt-len identifier classID xpos ypos xsize ysize -- )
\ low level control compiler word. The other control compiler
\ keywords are build onto GENERIC_CONTROL. No need to use ist
\ directly.

: GENERIC_CONTROL { txt-addr txt-len identifier classID xpos ypos xsize ysize -- }
        ALIGN           \ pre-align memory address
        STYLEFLAGS ,    \ compile STYLEFLAGS
        0 TO STYLEFLAGS \ and reset it
        0 ,             \ compile a zero dword ( don't know why ? :-)
        xpos W, ypos W, \ compile x and y
        xsize W, ysize W, \     .. cx and cy
        identifier W,   \ compile the control ID,
        0X0FFFF W,      \ a prefix of class-ID,
        classID W,      \ and class-ID itself
        txt-addr HERE txt-len 1+ 2* \   --- source dest len
        DUP ALLOT MOVE  \ compile control's text string
        0 W,            \ append a word item, a delimiter or whatsoever?
        2DUP 7 ?PAIRS   \ check stack balance
        1 SWAP W+!      \ increment bNumberOfItems
        ;

\ aCONTROL  ( identifier classID xpos ypos xsize ysize --<quoted text>-- -- )
\ Defines a generic control within a BEGIN-DIALOG...END-DIALOG structure.
\ The following predefined control classes may be used with aCONTROL:
\       CLASS_BUTTON    CLASS_EDIT          CLASS_STATIC
\       CLASS_LISTBOX   CLASS_SCROLLBAR     CLASS_COMBOBOX
\
\ STYLEFLAGS has to be explicitely preset, with: "... TO STYLEFLAGS"
\ as seen in DLGTEST.F aCONTROL adds only the styles WS_CHILD and
\ WS_VISIBLE, as almost all controls will need them.
\
\ See DIALOGS.F for an example.

: aCONTROL { identifier classID xpos ypos xsize ysize --<quoted text>-- -- }
        STYLEFLAGS WS_CHILD WS_VISIBLE OR OR TO STYLEFLAGS
        "PARSE-TEXT" ANSI>WIDECHAR      \ get following "quoted text"
        identifier classID xpos ypos xsize ysize
        GENERIC_CONTROL
;


\ PUSHBUTTON  ( identifier xpos ypos xsize ysize --<quoted text>-- -- )
\ Defines a "push button" control within a BEGIN-DIALOG...END-DIALOG structure.
\ The control styles used by PUSHBUTTON are:
\   BS_PUSHBUTTON WS_TABSTOP WS_CHILD WS_VISIBLE
\ Additional control styles may be preset with: "... TO STYLEFLAGS"

: PUSHBUTTON  { identifier xpos ypos xsize ysize --<text>-- -- }
        STYLEFLAGS BS_PUSHBUTTON WS_TABSTOP WS_CHILD WS_VISIBLE
        OR OR OR OR TO STYLEFLAGS
        "PARSE-TEXT" ANSI>WIDECHAR      \ get following "quoted text"
        identifier CLASS_BUTTON
        xpos ypos xsize ysize
        GENERIC_CONTROL
;


\ DEFPUSHBUTTON  ( identifier xpos ypos xsize ysize --<quoted text>-- -- )
\ Defines a "default push button" control within a BEGIN-DIALOG...END-DIALOG
\ structure. This button will also be "clicked" if the user hit the <ENTER> key.
\ Therefor in each dialog structure can only be one default pushbutton.
\ The control styles used by DEFPUSHBUTTON are:
\   BS_DEFPUSHBUTTON WS_TABSTOP WS_CHILD WS_VISIBLE
\ Additional control styles may be preset with: "... TO STYLEFLAGS"

: DEFPUSHBUTTON  { identifier xpos ypos xsize ysize --<text>-- -- }
        STYLEFLAGS BS_DEFPUSHBUTTON WS_TABSTOP WS_CHILD WS_VISIBLE
        OR OR OR OR TO STYLEFLAGS
        "PARSE-TEXT" ANSI>WIDECHAR      \ get following "quoted text"
        identifier CLASS_BUTTON
        xpos ypos xsize ysize
        GENERIC_CONTROL
;

\ EDITTEXT  ( identifier xpos ypos xsize ysize -- )
\ Defines a "edit text" control within a BEGIN-DIALOG...END-DIALOG structure.
\ The user can type a string into the edit text control and edit the text with
\ the usual selection, cut, copy, and paste operations.
\ The control styles used by EDITTEXT are:
\   ES_LEFT WS_TABSTOP WS_BORDER WS_CHILD WS_VISIBLE
\ Additional control styles may be preset with: "... TO STYLEFLAGS"

: EDITTEXT  { identifier xpos ypos xsize ysize -- }
        STYLEFLAGS ES_LEFT WS_TABSTOP WS_BORDER WS_CHILD WS_VISIBLE
        OR OR OR OR OR TO STYLEFLAGS
        S"W "               \ set empty control text
        identifier CLASS_EDIT
        xpos ypos xsize ysize
        GENERIC_CONTROL
;

\ MLETEXT  ( identifier xpos ypos xsize ysize -- )
\ Defines a "multi-line edit text" control within a BEGIN-DIALOG...END-DIALOG
\ structure.  The user can type a string into the edit text control and edit
\ the text with the usual selection, cut, copy, and paste operations.
\ The control styles used by MLETEXT are:
\  ES_MULTILINE ES_LEFT WS_TABSTOP WS_BORDER WS_CHILD WS_VISIBLE
\ Additional control styles may be preset with: "... TO STYLEFLAGS"

: MLETEXT  { identifier xpos ypos xsize ysize --- }
        STYLEFLAGS ES_MULTILINE ES_LEFT WS_TABSTOP WS_BORDER WS_CHILD WS_VISIBLE
        OR OR OR OR OR OR TO STYLEFLAGS
        S"W "               \ set empty control text
        identifier CLASS_EDIT
        xpos ypos xsize ysize
        GENERIC_CONTROL
;

\ CHECKBOX  ( identifier xpos ypos xsize ysize --<quoted text>-- -- )
\ Defines a "check box" control within a dialog.
\ The control styles used by CHECKBOX are:
\   BS_CHECKBOX WS_CHILD WS_TABSTOP WS_VISIBLE
\ Additional control styles may be preset with: "... TO STYLEFLAGS"

: CHECKBOX  { identifier xpos ypos xsize ysize --<text>-- -- }
        STYLEFLAGS BS_CHECKBOX WS_CHILD WS_TABSTOP WS_VISIBLE
        OR OR OR OR TO STYLEFLAGS
        "PARSE-TEXT" ANSI>WIDECHAR      \ get following "quoted text"
        identifier CLASS_BUTTON
        xpos ypos xsize ysize
        GENERIC_CONTROL
;

\ AUTOCHECKBOX  ( identifier xpos ypos xsize ysize --<quoted text>-- -- )
\ Defines a "auto check box" control within a dialog.
\ The control styles used by CHECKBOX are:
\   BS_AUTOCHECKBOX WS_CHILD WS_TABSTOP WS_VISIBLE
\ Additional control styles may be preset with: "... TO STYLEFLAGS"

: AUTOCHECKBOX  { identifier xpos ypos xsize ysize --<text>-- -- }
        STYLEFLAGS BS_AUTOCHECKBOX WS_CHILD WS_TABSTOP WS_VISIBLE
        OR OR OR OR TO STYLEFLAGS
        "PARSE-TEXT" ANSI>WIDECHAR      \ get following "quoted text"
        identifier CLASS_BUTTON
        xpos ypos xsize ysize
        GENERIC_CONTROL
;

\ RADIOBUTTON          ( identifier xpos ypos xsize ysize --<quoted text>-- -- ) or
\ synonym OPTIONBUTTON ( identifier xpos ypos xsize ysize --<quoted text>-- -- )
\ defines a "radio button" control within a dialog structure.
\ RADIOBUTTON uses the following styles: BS_RADIOBUTTON WS_CHILD WS_VISIBLE
\ Additional control styles may be preset with: "... TO STYLEFLAGS"

: RADIOBUTTON  { identifier xpos ypos xsize ysize --<text>-- -- }
        STYLEFLAGS BS_RADIOBUTTON WS_CHILD WS_VISIBLE
        OR OR OR TO STYLEFLAGS
        "PARSE-TEXT" ANSI>WIDECHAR      \ get following "quoted text"
        identifier CLASS_BUTTON
        xpos ypos xsize ysize
        GENERIC_CONTROL
;
synonym  OPTIONBUTTON RADIOBUTTON   \ the word basic dialog editor uses the synonym


\ GROUPBOX ( identifier xpos ypos xsize ysize -- )
\ Defines a "group box" control within a dialog. GROUPBOX usually frames a group #
\ of RADIOBUTTONs. GROUPBOX uses the following styles:
\   BS_GROUPBOX WS_CHILD WS_VISIBLE
\ Additional control styles may be preset with: "... TO STYLEFLAGS"

: GROUPBOX  { identifier xpos ypos xsize ysize -- }
        STYLEFLAGS  BS_GROUPBOX WS_CHILD WS_VISIBLE
        OR OR OR  TO STYLEFLAGS
        S"W "               \ set empty control text
        identifier CLASS_BUTTON
        xpos ypos xsize ysize
        GENERIC_CONTROL
;

\ LISTBOX  ( identifier xpos ypos xsize ysize -- )
\ defines a "list box" control within a dialog structure.
\ LISTBOX's default control styles are:  LBS_STANDARD WS_CHILD WS_VISIBLE
\ Additional control styles may be preset with: "... TO STYLEFLAGS"

: LISTBOX  { identifier xpos ypos xsize ysize -- }
        STYLEFLAGS LBS_STANDARD WS_CHILD WS_VISIBLE
        OR OR OR TO STYLEFLAGS
        S"W "               \ set empty control text
        identifier CLASS_LISTBOX
        xpos ypos xsize ysize
        GENERIC_CONTROL
;


\ COMBOBOX  ( identifier xpos ypos xsize ysize -- )
\ defines a "combo box" control within a dialog structure. A combo box is
\ basically a combination of a "list box" and a "edit text" control.
\ The control styles used by COMBOBOX are:
\   CBS_SIMPLE WS_CHILD WS_VISIBLE WS_TABSTOP
\ Additional control styles may be preset with: "... TO STYLEFLAGS"

: COMBOBOX  { identifier xpos ypos xsize ysize -- }
        STYLEFLAGS CBS_SIMPLE WS_CHILD WS_VISIBLE WS_TABSTOP
        OR OR OR OR TO STYLEFLAGS
        S"W "               \ set empty control text
        identifier CLASS_COMBOBOX
        xpos ypos xsize ysize
        GENERIC_CONTROL
;


\ CTEXT  ( identifier xpos ypos xsize ysize --<quoted text>-- -- )
\ defines a centered static text control within a dialog.
\ CTEXT's styles are: SS_CENTER WS_GROUP WS_CHILD WS_VISIBLE
\ Additional control styles may be preset with: "... TO STYLEFLAGS"
\ (But is this useful?)

: CTEXT  { identifier xpos ypos xsize ysize --<text>-- -- }
        STYLEFLAGS SS_CENTER WS_GROUP WS_CHILD WS_VISIBLE
        OR OR OR OR TO STYLEFLAGS
        "PARSE-TEXT" ANSI>WIDECHAR      \ get following "quoted text"
        identifier CLASS_STATIC
        xpos ypos xsize ysize
        GENERIC_CONTROL
;

\ LTEXT  ( identifier xpos ypos xsize ysize --<quoted text>-- -- )
\ defines a left-justified static text control within a dialog structure.
\ LTEXT's styles are: SS_LEFT WS_GROUP WS_CHILD WS_VISIBLE
\ Additional control styles may be preset with: "... TO STYLEFLAGS"
\ (But is this useful?)

: LTEXT  { identifier xpos ypos xsize ysize --<text>-- -- }
        STYLEFLAGS SS_LEFT WS_GROUP WS_CHILD WS_VISIBLE
        OR OR OR OR TO STYLEFLAGS
        "PARSE-TEXT" ANSI>WIDECHAR      \ get following "quoted text"
        identifier CLASS_STATIC
        xpos ypos xsize ysize
        GENERIC_CONTROL
;

\ RTEXT  ( identifier xpos ypos xsize ysize --<quoted text>-- -- )
\ defines a right-justified static text control within a dialog structure.
\ RTEXT's control styles are: SS_RIGHT WS_GROUP WS_CHILD WS_VISIBLE
\ Additional control styles may be preset with: "... TO STYLEFLAGS"
\ (But is this useful?)

: RTEXT  { identifier xpos ypos xsize ysize --<text>-- -- }
        STYLEFLAGS SS_RIGHT WS_GROUP WS_CHILD WS_VISIBLE
        OR OR OR OR TO STYLEFLAGS
        "PARSE-TEXT" ANSI>WIDECHAR      \ get following "quoted text"
        identifier CLASS_STATIC
        xpos ypos xsize ysize
        GENERIC_CONTROL
;

\ ICON  ( identifier xpos ypos xsize ysize --<quoted text>-- -- )
\ defines an "icon" bitmap control within a dialog structure. The bitmap is
\ an icon ressource in the executable file. Therefore ICON cannot be used to
\ create new icons without commercial ressource compiler :-( sorry!
\ The control styles used by ICON are: SS_ICON WS_CHILD WS_VISIBLE.

: ICON  { identifier xpos ypos xsize ysize --<text>-- -- }
        STYLEFLAGS SS_ICON WS_CHILD WS_VISIBLE
        OR OR OR TO STYLEFLAGS
        "PARSE-TEXT" ANSI>WIDECHAR      \ get following "quoted text"
        identifier CLASS_STATIC
        xpos ypos xsize ysize
        GENERIC_CONTROL
;


