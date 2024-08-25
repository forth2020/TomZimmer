\ DLGTEST.F     a copy of FORTHDLG.F
\       the names have all a "2" appended, to distinguish from original

cr .( Loading Forth System Dialogs Again...)

\ -------------------- Forth Dialog --------------------

:Object Font-dialog2  <SUPER dialog

128 bytes fontname

IDD_FONTDLG forthdlg2 find-dialog-id constant template

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

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ fload VERSION.F \ load the VERSION# definition
\
\ : .version      ( -- )
\                 cr ."  Version: "
\                version# 0 <# # # # # s"   Build: " "hold # '.' hold #s #> type ;
\
\ : next-version  { \ version$ vhndl -- }
\                64 localAlloc: version$
\                ?loading @ 0= ?EXIT                     \ only while loading
\                s" version.f" r/w open-file
\                                  abort" Failed to Open VERSION.F"
\                to vhndl                                \ save file handle
\                version$ 100 vhndl read-file
\                                  abort" Read error to VERSION.F"
\                version$ swap bl skip 2dup bl scan nip - number? nip 0=
\                                  abort" Number conversion error in VERSION.F"
\                0 0 vhndl reposition-file drop
\                1+ dup to version# 0 <# #s #>  vhndl write-file
\                                  abort" Write error to VERSION.F"
\                s"  value version#\n" 2dup \n->crlf   vhndl write-file
\                                  abort" Write error to VERSION.F"
\                vhndl close-file drop .version ;
\
\ next-version
\
\ 0 value &about-version
\
\ create about-msg
\         z," Win32Forth, Public Domain, January 1994\n\n"
\        +z," Version: "
\        -null, here to &about-version
\        version# 0 <# # # # # '.' hold #s #> +z",
\        +z,"    "               \ some extra spaces for safety
\        +z," \nCompiled: "
\        compile-version >date" +z",
\        +z," , "
\        compile-version >time" +z",
\        -null, here 0 c, align about-msg - constant about-len
\
\ create about-msg2
\         z," Contributors:\n"
\        +z,"    Andrew McKewan, Tom Zimmer, Jim Schneider,\n"
\        +z,"    Robert Smith, Y. T. Lin, Andy Korsak\n\n"
\        +z," Portions derived from:\n"
\        +z,"    F-PC Forth, Public Domain, November 1987\n\n"
\        +z," Assembler 486ASM.F:\n"
\        +z,"    Copyright September 1994, Jim Schneider"
\        -null, here 0 c, align about-msg2 - constant about-len2
\
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

:Object about-forth-dialog2 <SUPER dialog

IDD_ABOUT_FORTH forthdlg2 find-dialog-id constant template

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

: about-win32forth2 ( -- )
                start: about-forth-dialog drop ;

:Object save-memory-dialog2 <SUPER dialog

IDD_SAVE_MEMORY forthdlg2 find-dialog-id constant template

:M On_Init:     ( hWnd-focus -- f )
                S" 100000" IDD_AVAIL_MEMORY SetDlgItemText: self
                1 ;M

:M Start:       ( -- n1 )       \ return size of image
                conhndl template run-dialog ;M

:M On_Command:  { \ memory$ -- f1 }
                64 localAlloc: memory$
                case
                IDOK     of     memory$ 31 IDD_AVAIL_MEMORY GetDlgItemText: self
                                memory$ swap number? nip
                                if      end-dialog
                                else    beep
                                then            endof
                IDCANCEL of     0 end-dialog    endof
                                false swap ( default result )
                endcase ;M

;Object

: save-forth2    ( -- )
                Start: save-memory-dialog2 ?dup
        if      &app-size @ >r                  \ save current memory end
                0max ?dup
                if      app-free!            \ set new free space
                then
                conhndl start: SaveForth dup c@
                if      count ['] "fsave catch ?dup
                        if  message
                        then
                else    drop
                then    r> &app-size !
        then    ;

:Object page-up-dialog2 <SUPER dialog

IDD_PAGEUP forthdlg2 find-dialog-id constant template

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

: page-up-setup2 ( -- )
                conhndl Start: page-up-dialog2 to #pages-up
                ;


\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ Edit text dialog Class
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

:Class NewEditDialog2    <Super  dialog

IDD_EDIT_DIALOG forthdlg2 find-dialog-id constant template

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

NewEditDialog2 FindTextDlg2 "Find Text" "Search for:" "Find"     ""   "Case Sensitive Search"

create find-buf2 max-handle allot
       find-buf2 off

: find-text-edit2 ( -- f1 )
                find-buf2 conhndl start: FindTextDlg2 ;


