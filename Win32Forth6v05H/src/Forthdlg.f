\ FORTHDLG.F

cr .( Loading Forth System Dialogs...)
cr .( -- BETA FORTHDLG.F V3.1A --)

\ forthdlg.f BETA 3.1A 24/09/2002 arm (minor) fixed hex problem on origin
\ forthdlg.f BETA 3.3D 2002/10/08 arm Consolidation

only forth also definitions

load-dialog FORTHDLG    \ load the dialogs for Forth

INTERNAL        \ start of non-user definitions

\ -------------------- Forth Dialog --------------------

:Object Font-dialog  <SUPER dialog

128 bytes fontname

IDD_FONTDLG forthdlg find-dialog-id constant template

:M On_Init:     ( hWnd-focus -- f )
                                        \ lParam wParam LB_message id -- long
                z" Hello1" rel>abs
                0 LB_ADDSTRING IDD_FONT SendDlgItemMessage: self drop
                z" Hello2" rel>abs
                0 LB_ADDSTRING IDD_FONT SendDlgItemMessage: self drop
                z" Hello3" rel>abs
                0 LB_ADDSTRING IDD_FONT SendDlgItemMessage: self drop

                S" Hello" IDD_TEXT SetDlgItemText: self
                              1 IDD_TEXT SetDlgItemAlign: self


                1 ;M

:M Start:       ( -- f )
                conhndl template run-dialog
                ;M

:M On_Command:  ( hCtrl code ID -- f )
                case
                IDOK     of     0 0 LB_GETCURSEL IDD_FONT SendDlgItemMessage: self
                                fontname 1+ rel>abs swap
                                      LB_GETTEXT IDD_FONT SendDlgItemMessage: self
                                dup 0< 0=
                                if      127 min fontname c!
                                        cr fontname count type
                                else    drop
                                then
                                1 end-dialog            endof
                IDCANCEL of     0 end-dialog            endof
                                false swap              ( default result )
                endcase
                ;M

;Object

fload SRC\BUILTBY.F

EXTERNAL        \ these definitions need to be in Forth

: .version      ( -- )
\                cr ."  Version: "
\                version# 0 <# # # # # s"   Build: " "hold # '.' hold #s #> type
\                builtby count type ;
\ -rbs October 12th, 2002 - 19:56
                cr ." Version: "
                version# 0 <# # # # # s"  Build: " "hold # # '.' hold #s #> type 
                \ cr
                \ builtby count type
                \ ."  Kernel " kver sp@ 4 type drop
                \ ."  at origin 0x" &origin @ h.8
                \ arm
                ;

: next-version  { \ version$ vhndl -- }
                64 localAlloc: version$
                ?loading @ 0= ?EXIT                     \ only while loading
                s" SRC\version.f" r/w open-file
                                  abort" Failed to Open VERSION.F"
                to vhndl                                \ save file handle
                version$ 100 vhndl read-file
                                  abort" Read error to VERSION.F"
                version$ swap bl skip 2dup bl scan nip - number? nip 0=
                                  abort" Number conversion error in VERSION.F"
                0 0 vhndl reposition-file drop
                1+ dup to version# 0 <# #s #>  vhndl write-file
                                  abort" Write error to VERSION.F"
                s"  value version#\n" 2dup \n->crlf   vhndl write-file
                                  abort" Write error to VERSION.F"
                vhndl close-file drop cr .version ;

next-version

\ *****************************************************************************
\ Show the lines following in the compiled file
\ *****************************************************************************

: STRING, ( $ -- )   \ compile a counted string at HERE
   HERE >R DUP C,
   DUP ALLOT
   R@ 1+ SWAP MOVE
   0 C,
   ALIGN
   R> COUNT \N->CRLF
;

\ text between Show{ and End} is typed or compiled to be typed when the compiled word is executed
: Show{ ( -- ) \ } 
   begin 
      
      REFILL 0= abort" Show{ REFILL failed!"   \ get the next line

      SOURCE 2dup s" End}" SEARCH >r 2drop r> 
      if   \ we have found the end marker
         2drop -1
      else
         STATE @ if  \ compiling
            POSTPONE CR  POSTPONE (S")  ( a n -- ) STRING,  POSTPONE TYPE   
         else        \ interpreting
            cr ( a n -- ) type
         then
         0
      then 
   until
   REFILL 0= abort" Show{ #2 REFILL failed!"             \ discard the line containing End}
; IMMEDIATE          \ do this while compiling a word

((
\ example code : text between Show{ and End} is typed 
: ttSNL
   Show{ 
   hello world!!!
I just follow the indent that I see...
   I just follow the indent that I see...
      I just follow the indent that I see...
   1 2 3 4 5 

   22            33           44        
   
   
   13 14 15 6 
   44
   End}
;
))

: $, ( $ -- )  \ compile a string at HERE
   HERE >R     \ DUP C,
   DUP ALLOT
   R@ SWAP MOVE
   R> drop
;

variable zCompileFirstLine

\ Compile a null terminated string at here, from the text following, until End}
: zCompile{ ( -- ) \ } 
   -1 zCompileFirstLine !
   begin 
      
      REFILL 0= abort" Show{ REFILL failed!"   \ get the next line

      SOURCE 2dup s" End}" SEARCH >r 2drop r> 
      if   \ we have found the end marker
         2drop -1
      else
         zCompileFirstLine @ 0= if
            $0D c, $0A c,           \ add CRLF characters, but not on the first line
         else
            0 zCompileFirstLine !
         then
         ( a n -- ) $,              \ add the string on the current line being parsed
         0
      then 
   until
   REFILL 0= abort" Show{ #2 REFILL failed!"    \ discard the line containing End}
   0 c,  \ null terminate
; 

((
create about-msg
zCompile{
      Win32Forth,  Public Domain,  February 2019
   Version: 
   End}
        here to &about-version
        version# 0 <# # # # # '.' hold #s #> +z",
        +z,"     Compiled: "  compile-version >date" +z",  +z," , "  compile-version >time" +z",
        -null,                      \ remove the null terminator so we can add more...
zCompile{
   Contributors:
      Andrew McKewan, Tom Zimmer, Jim Schneider,
   End}
   0 c,  
   here align about-msg - constant about-len
))

INTERNAL        \ back to non-user definitions

0 value &about-version

create about-msg
         z," Win32Forth,  Public Domain,  February 2019\n\n"
        +z," Version: "
        -null, here to &about-version
        version# 0 <# # # # # '.' hold #s #> +z",
        \ +z," "               \ some extra spaces for safety
        +z,"  Compiled: "
        compile-version >date" +z",
        +z," , "
        compile-version >time" +z",
        +z," \n2019 Win10 updates: Howerd Oakford, Rod Oakford"
        -null, here 0 c, align about-msg - constant about-len

create about-msg2
         z," Contributors:\n"
        +z,"    Andrew McKewan, Tom Zimmer, Jim Schneider,\n"
        +z,"    Robert Smith, Y. T. Lin, Andy Korsak\n\n"
        +z," Portions derived from:\n"
        +z,"    F-PC Forth, Public Domain, November 1987\n\n"
        +z," Assembler 486ASM.F:\n"
        +z,"    Copyright September 1994, Jim Schneider"
        -null, here 0 c, align about-msg2 - constant about-len2

:Object about-forth-dialog <SUPER dialog

IDD_ABOUT_FORTH forthdlg find-dialog-id constant template

: init_version  ( -- )
                version# 0 <# # # # # '.' hold #s #>
                &about-version swap move ;

:M On_Init:     ( hWnd-focus -- f )
                init_version
                about-msg  about-len  IDD_ABOUT_TEXT  SetDlgItemText: self
                about-msg2 about-len2 IDD_ABOUT_TEXT2 SetDlgItemText: self
                1 ;M

:M Start:       ( -- f )
                true \ nt?
                if      conhndl template run-dialog
                else    init_version
                        cr cr about-msg about-len bounds
                        do      i c@ emit
                        loop
                        cr about-msg2 about-len2 bounds
                        do      i c@ emit
                        loop    cr 1
                then    ;M

:M On_Command:  ( hCtrl code ID -- f1 )
                case
                IDCANCEL of     0 end-dialog    endof
                                false swap ( default result )
                endcase ;M

;Object

: about-win32forth ( -- )
                start: about-forth-dialog drop ;

:Object save-memory-dialog <SUPER dialog

IDD_SAVE_MEMORY forthdlg find-dialog-id constant template

int sysmem
int appmem
int origin

:M ClassInit:   ( -- )
                ClassInit: super
                700000 to appmem
                400000 to sysmem
                     0 to origin
                ;M

:M On_Init:     ( hWnd-focus -- f )
                appmem 0 <# #s #>               IDD_AVAIL_APPMEMORY SetDlgItemText: self
                sysmem 0 <# #s #>               IDD_AVAIL_SYSMEMORY SetDlgItemText: self
 ( arm fix) hex origin 0 <# # # # # # # # # #> decimal IDD_START_ADDRESS   SetDlgItemText: self
                1 ;M

:M GetAppMem:   ( -- appmemory )
                appmem
                ;M

:M SetAppMem:   ( appmemory -- )
                TO appmem
                ;M

:M GetSysMem:   ( -- sysmemory )
                sysmem
                ;M

:M SetSysMem:   ( sysmemory -- )
                TO sysmem
                ;M

:M GetOrigin:   ( -- origin )
                origin
                ;M        

:M SetOrigin:   ( origin -- )
                TO origin
                ;M        

:M Start:       ( -- n1 )       \ return size of image
                conhndl template run-dialog ;M

:M On_Command:  { hCtrl code ID \ memory$ flag -- f1 }
                64 localAlloc: memory$
                0 to flag
                ID
                case
                IDOK     of     memory$ 31 IDD_AVAIL_APPMEMORY GetDlgItemText: self
                                memory$ swap number? abs +to flag drop to appmem
                                memory$ 31 IDD_AVAIL_SYSMEMORY GetDlgItemText: self
                                memory$ swap number? abs +to flag drop to sysmem
                                memory$ 31 IDD_START_ADDRESS   GetDlgItemText: self
                            HEX memory$ swap number? abs +to flag drop to origin DECIMAL
                                origin 0xFFFF0000 AND to origin
                                flag 3 =        \ if both are ok, then we are done
                                if      1 end-dialog
                                else    beep
                                then            endof
                IDCANCEL of     0 end-dialog    endof
                                false swap ( default result )
                endcase ;M

;Object

: save-forth    ( -- )
                Start: save-memory-dialog
        if      conhndl start: SaveForth dup c@
                if      app-free >r                     \ save current memory specifications
                        sys-free >r
                        GetAppMem: save-memory-dialog 15000 max app-free! \ set new free space
                        GetSysMem: save-memory-dialog 10000 max sys-free!
                        count ['] "fsave catch ?dup
                        r> sys-free!
                        r> app-free!
                        if  message
                        then
                else    drop
                then
        then    ;

EXTERNAL

:Object page-up-dialog <SUPER dialog

IDD_PAGEUP forthdlg find-dialog-id constant template

:M On_Init:     ( hWnd-focus -- f )
                1 ;M

:M Start:       ( parent-window -- n1 )       \ return size of image
                template run-dialog ;M

:M On_Command:  ( hCtrl code ID -- f1 )
                case
                IDCANCEL of     0 end-dialog    endof
                IDD_2UP  of     2 end-dialog    endof
                IDD_4UP  of     4 end-dialog    endof
                                false swap ( default result )
                endcase ;M

;Object

: page-up-setup ( -- )
                conhndl Start: page-up-dialog to #pages-up  ;

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ Edit text dialog Class
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

:Class NewEditDialog    <Super  dialog

IDD_EDIT_DIALOG forthdlg find-dialog-id constant template

int szText
int szTitle
int szPrompt
int szDoit
int szCancel
int szOption
int OptionState

:M ClassInit:   ( -- )
                ClassInit: super
                here to szText   0 ,            \ null text string
                here to szTitle  ,"text"
                here to szPrompt ,"text"
                here to szDoit   ,"text"
                here to szCancel ,"text"
                here to szOption ,"text"
                ;M

:M On_Init:     ( hWnd-focus -- f )
\ Setting the title must be handled specially, since the dialog itself isn't
\ considered to be a dialog item
                szTitle  count                          SetText: self
                szText   count IDD_EDIT_TEXT            SetDlgItemText: self
                szPrompt count IDD_PROMPT_TEXT          SetDlgItemText: self
                szOption c@
                if      szOption count IDB_OPTION       SetDlgItemText: self
                        OptionState    IDB_OPTION       CheckDlgButton: self
                        TRUE
                else    FALSE
                then                   IDB_OPTION       ShowDlgItem: self
                szDoit   count dup
                if       2dup  IDOK                     SetDlgItemText: self
                then     2drop
                szCancel count dup
                if       2dup  IDCANCEL                 SetDlgItemText: self
                then     2drop
                1 ;M

:M Start:       ( counted_text_buffer parent -- f )
                swap to szText
                template run-dialog
                ;M

:M On_Command:  ( hCtrl code ID -- f1 ) \ returns 0=cancel,
                                        \ returns 1=option-off
                                        \ returns 2=option-on
                case
                IDOK     of     szText 1+ max-handle 2 - IDD_EDIT_TEXT GetDlgItemText: self
                                szText c!
                                IDB_OPTION IsDlgButtonChecked: self
                                dup to OptionState
                                1 and 1+ end-dialog    endof
                IDCANCEL of     0        end-dialog    endof
                                false swap ( default result )
                endcase ;M

;Class

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ definer      name         title       prompt        ok     cancel   Option
\                            text        text        text     text     text
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

NewEditDialog FindTextDlg "Find Text" "Search for:" "Find"     ""   "Case Sensitive Search"

create find-buf MAXSTRING allot
       find-buf off

: find-text-edit ( -- f1 )
                find-buf conhndl start: FindTextDlg ;

MODULE          \ finish up the module

only forth also definitions

