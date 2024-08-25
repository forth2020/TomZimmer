(( WINBROWS.F                                             by Tom Zimmer

  WinBrowse is a simple (as simple as windows programs ever get) program
  that can browse a text file.  It is the basis upon which the more
  complex example WinEd editor was built.  You can scroll through a
  file with the scroll bar, resize the window, minimize, maximize, open a
  new file, and print the file with one, two or four pages printed on
  each page.

  WinBrowse requires the file WINBROWS.BMP to compile, and after
  compiling it, the files WINBROWS.EXE and WINBROWS.IMG are created.
  These two files are needed to run WinBrowse and should be in the same
  directory.

  Compiling Winbrowse adds 33k bytes to the 194k byte Win32Forth .IMG
  file to create the application. 21.5k bytes of that 33k is the file
  WINBROWS.BMP which gets included in the .IMG file during compilation.

  Since WinBrowse is turnkeyed to create the application program, 217k
  bytes of system dictionary are discarded when creating WINBROWS.IMG.
  This makes WinBrowse considerably smaller than the almost 400k byte
  WIN32FOR.IMG file on which it is built.

))

only forth also definitions

   0 value using98/NT?          \ are we running Windows98 or WindowsNT?
   0 value BrowseWindow
   0 value text-len             \ length of text
   0 value text-ptr             \ address of current text line
   0 value text-blen            \ total text buffer length

   0 value line-tbl             \ address of the line pointer table
   0 value line-cur             \ the current top screen line
   0 value line-last            \ the last file line
   0 value col-cur              \ the current left column

1000 value max-lines            \ initial maximum nuber of lines
 512 value max-cols             \ maximum width of text currently editing

  80 value screen-cols          \ default rows and columns at startup
  34 value screen-rows

create cur-filename max-path allot

: #line"        ( n1 -- a1 n2 ) \ get the address and length a1,n2 of line n1
                line-tbl swap 0max line-last min cells+ 2@ tuck - 2 - 0max ;

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\       Define the EDIT-WINDOW child window class object
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

:Object EDIT-WINDOW <super child-window

        Font bFont
        int alreadyPainting
        int paintAgain

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
                ClassInit: Super
                FALSE to alreadyPainting
                FALSE to paintAgain
                char-width        Width: bFont
                char-height      Height: bFont
                s" Courier" SetFacename: bFont
                ;M

:M On_Paint:    ( -- )          \ screen redraw procedure
                SaveDC: dc                      \ save device context
                Handle: bFont SetFont: dc       \ set the font to be used
                text-ptr ?dup
                IF      screen-rows 0
                       ?do      char-width char-height i *      \ x, y
                                line-cur i + #line"
                                col-cur /string
                                screen-cols min                 \ clip to win
                                TabbedTextOut: dc
                                word-split drop                 \ x
                                char-width +                    \ extra space
                                i char-height *                 \ y
                                over char-width / >r
                                spcs screen-cols r> - 0max      \ at least zero
                                spcs-max min TextOut: dc        \ and less than max
                        loop    2drop
                THEN
                using98/NT? 0=          \ only support variable sized scroll bars in Windows98 and WindowsNT
                line-last 32767 > OR    \ if we have a big file, revert to non-resizable scroll buttons
                IF
                        \ set the vertical scroll bar limits
                        FALSE line-last screen-rows - 0max 32767 min 0 SB_VERT
                        GetHandle: self Call SetScrollRange drop

                        \ position the vertical button in the scroll bar
                        TRUE line-cur line-last 32767 min line-last 1 - 1 max */ 32767 min SB_VERT
                        GetHandle: self Call SetScrollPos drop

                        \ set the horizontal scroll bar limits
                        FALSE max-cols screen-cols 3 - - 0max col-cur max 32767 min 0 SB_HORZ
                        GetHandle: self Call SetScrollRange drop

                        \ position the horizontal button in the scroll bar
                        TRUE col-cur 32767 min SB_HORZ
                        GetHandle: self Call SetScrollPos drop
                ELSE
                        \ set the vertical scroll bar limits and position
                        screen-rows to nPage
                        line-cur to nPos
                        0 to nMin
                        line-last to nMax
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
\ restore the original font
                RestoreDC: dc
        ;M

:M On_Init:     ( -- )
                On_Init: super
                Create: bFont
                ;M

:M On_Done:     ( -- )
                Delete: bFont   \ delete the font when no longer needed
                On_Done: super
                ;M

:M Erase:       ( -- )          \ erase the text window
                get-dc
                0 0
                screen-cols char-width  *
                screen-rows char-height * WHITE FillArea: dc
                release-dc
                ;M

:M Refresh:     ( -- )          \ refresh the windows contents
                Paint: self
                s" Browsing File: "     pad  place
                cur-filename count      pad +place
                                        pad count SetTitle: BrowseWindow
                ;M

:M VPosition:   ( n1 -- )       \ move to line n1 in file
                0max line-last 1+ screen-rows 1- - 0max min to line-cur
                ;M

:M HPosition:   ( n1 -- )       \ move to column n1
                0max max-cols 1+ screen-cols 1- - 0max min to col-cur
                ;M

:M Home:        ( -- )          \ goto the top of the current file
                0 VPosition: self
                0 HPosition: self
                Refresh: self
                ;M

:M End:         ( -- )          \ goto the end of the current file
                line-last 1+ VPosition: self
                0            HPosition: self
                Refresh: self
                ;M

:M VScroll:     ( n1 -- )       \ scroll up or down n1 lines in file
                line-cur + VPosition: self
                Refresh: self
                ;M


:M VPage:       ( n1 -- )       \ scroll up or down n1 pages in file
                screen-rows * line-cur + VPosition: self
                Refresh: self
                ;M

:M HScroll:     ( n1 -- )       \ scroll horizontally n1 characters
                col-cur + HPosition: self
                Refresh: self
                ;M


:M HPage:       ( n1 -- )       \ scroll horizontally by n1 page
                screen-cols * col-cur + HPosition: self
                Refresh: self
                ;M

:M WindowStyle: ( -- style )            \ return the window style
                WindowStyle: super
                WS_VSCROLL or           \ add vertical scroll bar
                WS_HSCROLL or           \ add horizontal scroll bar
                ;M
                
:M WM_VSCROLL   ( h m w l -- res )
                swap word-split >r
        CASE
                SB_BOTTOM        OF          End: self   ENDOF
                SB_TOP           OF         Home: self   ENDOF
                SB_LINEDOWN      OF    1 VScroll: self   ENDOF
                SB_LINEUP        OF   -1 VScroll: self   ENDOF
                SB_PAGEDOWN      OF    1   VPage: self   ENDOF
                SB_PAGEUP        OF   -1   VPage: self   ENDOF
                SB_THUMBTRACK    OF     line-last 32767 >
                                        IF     line-last r@ 32767 */ VPosition: self
                                        ELSE             r@          VPosition: self
                                        THEN             ENDOF
        ENDCASE r>drop
                Paint: self
                0 ;M

:M WM_HSCROLL   ( h m w l -- res )
                swap word-split >r
        CASE
                SB_BOTTOM        OF          End: self   ENDOF
                SB_TOP           OF         Home: self   ENDOF
                SB_LINELEFT      OF   -1 HScroll: self   ENDOF
                SB_LINERIGHT     OF    1 HScroll: self   ENDOF
                SB_PAGELEFT      OF   -1   HPage: self   ENDOF
                SB_PAGERIGHT     OF    1   HPage: self   ENDOF
                SB_THUMBPOSITION OF r@ HPosition: self
                                         Refresh: self   ENDOF
                SB_THUMBTRACK    OF r@ HPosition: self
                                         Refresh: self   ENDOF
        ENDCASE r>drop
                Paint: self
                0 ;M


;Object


\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\       WinBrowse about dialog, copied from the Forth About Dialog
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

:Object about-winbrowse-dialog       <SUPER dialog

IDD_ABOUT_FORTH forthdlg find-dialog-id constant template

create about-text-msg
         z," WinBrowse by Tom Zimmer\n\n"
        +z," Public domain example for Win32Forth\n"
        +z," January 12th, 1996"
        -null, here 0 c, align about-text-msg - constant about-text-len

:M On_Init:     ( hWnd-focus -- f )
                about-text-msg about-text-len IDD_ABOUT_TEXT SetDlgItemText: self
                1 ;M

:M Start:       ( -- f )
                Addr: BrowseWindow template run-dialog
                ;M

:M On_Command:  ( hCtrl code ID -- f1 )
                case
                IDCANCEL of     0 end-dialog    endof
                                false swap ( default result )
                endcase ;M

;Object

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\       Setup the line pointers and scroll bar for a new file
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

: set-line-pointers ( -- )
                wait-cursor
                1 to line-last          \ one total lines to start
                text-ptr line-tbl !     \ init first line
                1 +to line-cur          \ bump to next line pointer
                text-ptr text-len
                begin   0x0D scan dup
                while   2 /string over line-tbl line-cur    cells+ !
                        1 +to line-cur                  \ bump current line
                        max-lines line-cur =            \ while not full
                        if      4000 +to max-lines
                                max-lines 4 + cells line-tbl realloc
                                s" Failed to adjust the line pointer table"
                                ?TerminateBox
                                to line-tbl
                        then
                repeat  drop  dup      line-tbl line-cur    cells+ !
                                       line-tbl line-cur 1+ cells+ !
                line-cur to line-last                   \ set total lines
                0 to line-cur
                arrow-cursor ;

: set-longest-line ( -- )
                wait-cursor
                0
                line-last 1+ 0
                do      i #line" nip max
                loop    2 + to max-cols
                arrow-cursor ;

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\       Select a file to edit and print
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

FileOpenDialog BrowseText "Open Text File" "Text Files|*.txt;*.f;*.asm;*.doc;*.seq;*.bat;*.c??;*.h|All Files|*.*|"

: "open-text    ( a1 n1 -- )
                2dup r/o open-file 0=
                if      Home: edit-window
                        StartPos: BrowseWindow
                        200 + swap 200 + swap message-origin
                        s" Reading Text File..." "message
                        >r 127 min cur-filename place
                        \ release/allocate the text buffer
                        text-ptr ?dup if free drop then
                        r@ file-size 2drop to text-len
                        text-len 10000 + to text-blen
                        text-blen malloc to text-ptr
                        \ release/allocate line pointer table
                        line-tbl ?dup if free drop then
                        1000 to max-lines
                        max-lines 4 + cells malloc to line-tbl
                        \ read the file into memory
                        text-ptr text-len r@ read-file drop
                        to text-len
                        r> close-file drop
                        set-line-pointers
                        set-longest-line
                        300 ms message-off
                        Refresh: edit-window
                else    2drop drop
                then    ;

: open-text     ( -- )
                GetHandle: BrowseWindow Start: BrowseText dup c@
                if      count "open-text
                else    drop
                then    ;

: print-text    { \ message$ -- }
                MAXSTRING            localAlloc: message$
                                \ attempt to center filename message
                spcs 64 cur-filename c@ 6 + - 2/ message$  place
                s" File: "                       message$ +place
                cur-filename count               message$ +place
                message$ count UserMessage: ThePrinter
                #pages-up ?dup
                if      2 =
                        if      two-page
                        else    four-page
                        then
                then
                start-scaled
        if      #pages-up >r
                line-last 0
                ?do     i #line" Type: ThePrinter
                                   Cr: ThePrinter
                loop
                print-scaled            \ print the results if single-page
                single-page             \ print the results if multi-page
                r> to #pages-up         \ restore the number of pages/page
        then    ;

: text-setup    ( -- )
                GetHandle: BrowseWindow Setup: ThePrinter ;

: text-up-setup ( -- )
                BrowseWindow Start: page-up-dialog to #pages-up ;

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ Here is the TOOLBAR for the application
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

ToolBar Edit-Tool-Bar "WINBROWS.BMP"

        08 HSpace
     1 PictureButton                    open-text  ; \ open
        ButtonInfo"  Open Text File "                \ tooltip for open text
     3 PictureButton                    print-text ; \ print
        ButtonInfo"  Print File "                    \ tooltip for print
        16 HSpace                                    \ a space between button
                                                     \ groups
     5 PictureButton                    start: about-winbrowse-dialog drop ;
        ButtonInfo"  About WinBrowse "               \ tooltip for about
        8 HSpace                                     \ a smaller space at the
ENDBAR                                               \ end of the toolbar

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\       Define the right mouse popup menu for the application
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

POPUPBAR Edit-Popup-bar

    POPUP " "
        MENUITEM        "Open File..."  open-text   ;
        MENUSEPARATOR
        MENUITEM        "Print File..." print-text  ;
        MENUSEPARATOR
        MENUITEM        "Exit"          bye         ;
ENDBAR

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\       Define the menubar for the application
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

MENUBAR Edit-Menu-bar

    POPUP "&File"
        MENUITEM        "&Open Text File... \tCtrl-O"   open-text ;
        MENUSEPARATOR
        MENUITEM        "Print Setup... "               text-setup ;
        MENUITEM        "Pages/Page Setup... "          text-up-setup ;
        MENUITEM        "&Print File...     \tCtrl-P"   print-text ;
        MENUSEPARATOR
        MENUITEM        "E&xit              \tAlt-F4"   bye ;

    POPUP "&Help"
        MENUITEM        "&About WinBrowse" start: about-winbrowse-dialog drop ;
ENDBAR

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\       Define the main window for the application
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

:Object EDITW  <super window

Rectangle EditRect

:M Classinit:   ( -- )
                ClassInit: super                \ init super class
                self to BrowseWindow            \ make myself the cur window
                Edit-Menu-Bar  to CurrentMenu   \ set the menubar
                Edit-Popup-Bar to CurrentPopup
                ;M

:M WindowHasMenu: ( -- )                        \ yes window has a menubar
                TRUE
                ;M

:M On_Init:     ( -- )          \ initialize the class
                On_Init: super                  \ first init super class
                1    SetId: edit-window         \ then the child window
                self Start: edit-window         \ then startup the editor child window
                2    SetId: Edit-Tool-Bar       \ then the next child window
                self Start: Edit-Tool-Bar       \ then startup toolbar window
                self Start: CurrentPopup        \ startup the right mouse popup
                ;M                              \ menu

:M On_Done:     ( h m w l -- res )
                CloseMenu: CurrentMenu          \ discard the menubar
                0 call PostQuitMessage drop     \ terminate application
                On_Done: super                  \ cleanup the super class
                0 ;M

:M WM_CLOSE     ( h m w l -- res )
                WM_CLOSE WM: Super
                bye
                0 ;M

:M StartSize:   ( -- width height )     \ starting window size
                screen-cols char-width  *
                screen-rows char-height *
                StartSize: Edit-Tool-Bar nip + 1+
                ;M

:M MinSize:     ( -- width height )     \ minimum window size
                char-width  20 *
                StartSize: Edit-Tool-Bar >r 25 + max
                char-height  4 * r> + 1+
                ;M

:M WindowTitle: ( -- Zstring )          \ window caption
                z" WinBrowse"
                ;M

:M On_Paint:    ( -- )
                LTGRAY_BRUSH Call GetStockObject
                0 0
                StartSize: self drop width max
                StartSize: Edit-Tool-Bar nip SetRect: EditRect
                EditRect.AddrOf rel>abs GetHandle: dc call FillRect ?win-error
                EraseRect: EditRect
                On_Paint: super
                WHITE                                LineColor: dc \ white color
                0 0                                     MoveTo: dc \ horiz
                StartSize: self drop width max 0        LineTo: dc \ line
                0 0                                     MoveTo: dc \ vertical
                StartSize: Edit-Tool-Bar nip 0 swap     LineTo: dc \ line
                StartSize: Edit-Tool-Bar swap 1+ swap   MoveTo: dc \ vertical
                StartSize: Edit-Tool-Bar drop 1+ 0      LineTo: dc \ line
                BLACK                                LineColor: dc
                0 StartSize: Edit-Tool-Bar nip dup>r    MoveTo: dc
                  StartSize: self drop width max r>     LineTo: dc

                  StartSize: Edit-Tool-Bar over 0       MoveTo: dc
                                                        LineTo: dc
                ;M

\ the l parameter has already been removed by WINDOW.F, and put
\ into Height and Width

:M On_Size:     ( h m w -- )                  \ handle resize message
                col-cur >r screen-cols >r
                Width  char-width  / to screen-cols
                r> screen-cols - col-cur + r> min 0max to col-cur

                line-cur >r screen-rows >r
                Height char-height / to screen-rows
                r> screen-rows - line-cur + r> min 0max to line-cur

                0 StartSize: Edit-Tool-Bar nip 1+ dup>r
                Width Height r> - Move: edit-window

                0 0 StartSize: Edit-Tool-Bar Move: Edit-Tool-Bar
                ;M

\ All SC_xxxx command types always have the high nibble set to 0xF

:M WM_SYSCOMMAND ( hwnd msg wparam lparam -- res )
                over 0xF000 and 0xF000 <>
                if      over LOWORD
                        DoMenu: CurrentMenu
                        0
                else    DefWindowProc: [ self ]
                then    ;M

;Object

: release-buffers ( -- )
                DestroyWindow: EDITW
                text-ptr ?dup if free drop then
                line-tbl ?dup if free drop then  ;

unload-chain chain-add-before release-buffers

create win-browse-class ," WinBrowse"     \ define the browsers class name
       win-browse-class count SetClassName: EDITW

\ The main application program

: WINBROWSE     ( -<filename>- )
                NT? Win98? or to using98/NT?            \
                Start: EDITW
                cmdline bl skip 2dup bl scan nip - dup
                if      2dup "path-only" SetDir: BrowseText
                        "open-text
                else    2drop
                        open-text
                then
                Refresh: edit-window
                begin   Refresh: edit-window
                        key             \ handle keyboard interpretation
                    case
                        'O' +k_control  of          open-text           endof
                        'P' +k_control  of          print-text          endof
                        k_end           of          End: edit-window    endof
                        k_home          of         Home: edit-window    endof
                        k_pgup          of   -1   VPage: edit-window    endof
                        k_pgdn          of    1   VPage: edit-window    endof
                        k_up            of   -1 VScroll: edit-window    endof
                        k_down          of    1 VScroll: edit-window    endof
                                             ( dup insert-character )
                    endcase
                again   ;

with-img ' winbrowse turnkey winbrows \ save the new application program .EXE and .IMG

5 pause-seconds         \ pause for user to see what happened

