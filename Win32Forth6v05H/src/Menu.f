\ MENU.F                Menu objects                    by Tom Zimmer
\ menu.f beta 2002/11/05 ron Added support for multiple instances capability

cr .( Loading Window Menus...)
cr .( -- BETA MENU.F 4.9C -- )

\ Menu class

only forth also definitions

NewEditDialog HtmlDlg "Open Web Page Address" "Page (WWW. and .COM optional):" "Open"  ""   ""

: open-web      { \ web$ -- }
                MAXSTRING LocalAlloc: web$
                web$ off
                web$ conhndl Start: HtmlDlg
                IF      web$ count conhndl "Web-Link
                THEN    ;

INTERNAL        \ internal definitions start here

: menu-append  ( hmenu flags id string -- )
        rel>abs swap 2swap swap
        Call AppendMenu ?win-error ;

: _execute-menufunc ( cfa -- )
                execute ;

defer execute-menufunc
   ' _execute-menufunc is execute-menufunc

0 value BuildMenu

VARIABLE menubar-link
         menubar-link OFF

200 value IdCounter

: NextId        ( -- bid )
                IdCounter dup 1+ to IdCounter ;

in-system

: ENDBAR        ( -- )          \ finish defining a menubar
                context @ ['] hidden vcfa>voc =
                if      previous
                then    ;

in-application

EXTERNAL        \ externally available definitions start here

:CLASS MENUBAR  <Super Object   \ start defining a menubar

\ hmb   ->      menu
\ mb            used for menu bar traversal

int hmb         \ head pointer of the menubar's menu list
int mb          \ temporary variable used for menubar traversal
int hmenu       \ handle to the menubar
int prevbar
int parent      \ the parent window

: trim-menus   ( nfa -- nfa )
\in-system-ok   dup menubar-link full-trim ;

\in-system-ok forget-chain chain-add trim-menus

:M ClassInit:   ( -- )
                self to BuildMenu
                0    to hmb
                0    to hmenu
                menubar-link link,      \ link into list
                self ,
\in-system-ok   also hidden             \ also search the menus vocabulary
                ;M

:M GetBar:      ( -- hmb )  hmb  ;M
:M PutBar:      ( hmb -- )  to hmb  ;M

:M MenuHandle:  ( -- hmenu )
                hmenu ;M

:M ReDrawMenu:  ( -- )
                conhndl parent =
                if      conhndl
                else    GetHandle: parent
                then    Call DrawMenuBar drop
                ;M

:M PosEnable:   ( flag pos -- )
                swap
                if      MF_ENABLED
                else    MF_GRAYED
                then    MF_BYPOSITION or swap hmenu Call EnableMenuItem drop
                ReDrawMenu: self
                ;M

:M CurrentId:   ( -- bid )
                IdCounter
                ;M

:M LoadMenu:    ( parent -- )
                to parent
                Call CreateMenu to hmenu
                hmb to mb
                begin   mb
                while   hmenu parent LoadMenu: mb
                               GetPrev: mb to mb
                repeat   ;M

:M Start:       ( parent -- )
                LoadMenu: self
                hmenu -1 <>
                if      hmenu SetMenu: parent
                then
                ;M

:M DoMenu:      ( ID -- )       \ called from a WINDOW
                mb >r           \ save in case we are reentered while running
                hmb to mb
                begin   mb
                while   dup DoMenu: mb
                           GetPrev: mb to mb
                repeat  drop
                r> to mb
                ;M

:M CloseMenu:   ( -- )
                ;M

:M ZeroMenu:    ( -- )
                0 to hmenu
                ;M

;CLASS

:CLASS POPUPBAR <Super MENUBAR

:M Start:       ( parent -- )
                LoadMenu: self
                ;M

2 cells bytes xy

:M Track:       { x y win-handle -- }
                y xy cell+ !    \ save away relative window coordinates
                x xy       !
                hmenu -1 <>
                if      0
                        hmenu
                        Call GetSubMenu >r
                        xy rel>abs      \ ClientToScreen begin here
                        win-handle      \ convert relative to absolute coords
                        Call ClientToScreen drop
                        0               \ TrackPopupMenu begin here
                        win-handle
                        0
                        xy cell+ @      \ recover absolute screen coordinates
                        xy       @
                        TPM_LEFTALIGN
                        TPM_RIGHTBUTTON or
                        r>
                        Call TrackPopupMenu drop
                then
                ;M


;CLASS

INTERNAL

0 value BuildPopup
0 value BREAK_FLAG

: BREAKPOPUP    ( -- )  \ make the next POPUP break an an new line
                MF_MENUBARBREAK to BREAK_FLAG ;

|Class POPUP    <Super  Object

int pprev
int popid
int pid
int mid
int hpm     \ handle for the popup menu head pointer
int pm      \ temp for popup menu list processing
int ptext
int BROKEN_FLAG
int parent

:M GetPopup:    ( -- hpm )  hpm  ;M
:M PutPopup:    ( hpm -- )  to hpm  ;M

:M SetPrev:     ( pprev -- ) to pprev ;M
:M GetPrev:     ( -- pprev ) pprev ;M

:M ClassInit:   ( -- )
                   self to BuildPopup
                   GetBar: BuildMenu to pprev           \ end of link is NULL
                pprev 0=                                \ if i'm the first one
                if      self PutBar: BuildMenu          \ put me in the bar
                else    begin   pprev
                        while   pprev to pid            \ save here temp
                                GetPrev: pprev to pprev
                        repeat  self SetPrev: pid       \ temp use
                        0 SetPrev: self
                then
                here to ptext ,"text"
                0 to popid
                BREAK_FLAG to BROKEN_FLAG
                0 to BREAK_FLAG
                ;M

:M LoadMenu:    ( mb parent -- )
                to parent
                >r
                Call CreatePopupMenu to pid
                hpm to pm
                begin   pm
                while   pid parent LoadMenu: pm
                                    GetPrev: pm to pm
                repeat
                r@ to popid
                r> MF_POPUP BROKEN_FLAG or pid ptext 1+ menu-append
                ;M

:M DoMenu:      ( ID -- )
                pm >r           \ save in case we are reentered while running
                hpm to pm
                begin   pm
                while   dup DoMenu: pm
                           GetPrev: pm to pm
                repeat  drop
                r> to pm
                ;M

;Class


|Class POPUPITEM <Super  POPUP

int mfunc       \ the menu function

:M ClassInit:   ( -- )
                ClassInit: super
                NextId to mid
                here to mfunc
                hide !csp docol , ]
                BREAK_FLAG to BROKEN_FLAG
                0 to BREAK_FLAG
                ;M

:M LoadMenu:    ( mb parent -- )
                to parent
                >r
                Call CreatePopupMenu to pid
                hpm to pm
                begin   pm
                while   pid LoadMenu: pm
                             GetPrev: pm to pm
                repeat
                r@ to popid
\ this "menu-append" MUST use "mid" NOT "pid" to be selectable
                r> MF_STRING BROKEN_FLAG or mid ptext 1+ menu-append
                ;M

:M DoMenu:      ( IDM -- )
                mid =
                if      mfunc execute-menufunc
                then    ;M

;Class

|Class SYSPOPUP  <Super  Object

int pprev
int popid
int pid
int mid
int hpm     \ handle for the popup menu head pointer
int pm      \ temp for popup menu list processing
int parent
cell bytes &anull

:M GetPopup:    ( -- hpm )  hpm  ;M
:M PutPopup:    ( hpm -- )  to hpm  ;M

:M SetPrev:     ( pprev -- ) to pprev ;M
:M GetPrev:     ( -- pprev ) pprev ;M

:M ClassInit:   ( -- )
                0 &anull !
                self to BuildPopup
                GetBar: BuildMenu to pprev      \ end of link is NULL
                pprev 0=                        \ if i'm the first one
                if      self PutBar: BuildMenu  \ put me in the bar
                else    begin   pprev
                        while   pprev to pid            \ save here temp
                                GetPrev: pprev to pprev
                        repeat  self SetPrev: pid       \ temp use
                        0 SetPrev: self
                then
                0 to popid
                0 to BREAK_FLAG
                ;M

:M LoadMenu:    ( mb parent -- )
                to parent
                >r
                conhndl parent =
        if      true  conhndl           Call GetSystemMenu drop   \ reset sysmenu
                false conhndl           Call GetSystemMenu to pid \ get handle
        else    true  GetHandle: parent Call GetSystemMenu drop   \ reset sysmenu
                false GetHandle: parent Call GetSystemMenu to pid \ get handle
        then    hpm to pm
                begin   pm
                while   pid parent LoadMenu: pm
                                    GetPrev: pm to pm
                repeat
                r> to popid
                ;M

:M DoMenu:      ( ID -- )
                pm >r           \ save in case we are reentered while running
                hpm to pm
                begin   pm
                while   dup DoMenu: pm
                           GetPrev: pm to pm
                repeat  drop
                r> to pm
                ;M

;Class


0 value prevpop

|Class SUBMENU  <Super  POPUP

int subpopup

:M ClassInit:   ( -- )
                prevpop    to subpopup
                BuildPopup to prevpop
                GetPopup: BuildPopup to pprev           \ end of link is NULL
                pprev 0=                                \ if i'm the first one
                if      self PutPopup: BuildPopup       \ put me in the bar
                else    begin   pprev
                        while   pprev to pid            \ save here temp
                                GetPrev: pprev to pprev
                        repeat  self SetPrev: pid       \ temp use
                        0 SetPrev: self
                then
                NextId to mid
                here to ptext ,"text"
                self to BuildPopup
                ;M

:M UnSUBMENU:   ( -- )
                prevpop to BuildPopup
                subpopup to prevpop
                0 to subpopup
                ;M

;Class

: ENDSUBMENU    ( -- )
                UnSUBMENU: BuildPopup ;

\ MENUBAR SampleMenuBar
\       POPUP "&File"
\               MENUITEM "&Open"  'O' +k_control pushkey ;
\               MENUITEM "E&xit"  'X' +k_control pushkey ;

:Class MENUITEMS <Super  Object

int mprev
int mid         \ menu ID and also used as a temp for PREV menu item
int mtext
int popid       \ handle of this items popup menu
int parent

:M SetPrev:     ( mprev -- ) to mprev ;M
:M GetPrev:     ( -- mprev ) mprev ;M

:M ClassInit:   ( -- )
                GetPopup: BuildPopup to mprev           \ end of link is NULL
                mprev 0=                                \ if i'm the first one
                if      self PutPopup: BuildPopup       \ put me in the bar
                else    begin   mprev
                        while   mprev to mid            \ save here temp
                                GetPrev: mprev to mprev
                        repeat  self SetPrev: mid       \ temp use
                        0 SetPrev: self
                then
                NextId to mid
                0 to mtext
                0 to popid
                ;M

: m"text"       ( -<"text">- )
                here to mtext ,"text" ;

:M Check:       ( f1 -- )
                if      MF_CHECKED
                else    MF_UNCHECKED
                then    mid popid Call CheckMenuItem drop
                ;M

:M Enable:      ( f1 -- )
                if      MF_ENABLED
                else    MF_GRAYED
                then    mid popid Call EnableMenuItem drop
                ;M

:M SetMenuText: ( z$ -- )
                rel>abs
                mid
                MF_BYCOMMAND MF_STRING or
                mid popid Call ModifyMenu ?win-error
                ;M

:M LoadMenu:    ( pid parent -- )
                to parent
                mtext 0= abort" Empty Menu Text String"
                dup to popid
                MF_STRING mid mtext 1+ menu-append
                ;M

:M DoMenu:      ( IDM -- )
                drop ;M

;Class


|Class MENUSEPARATOR <Super  MENUITEMS

:M LoadMenu:    ( pid parent -- )
                to parent
                dup to popid
                MF_SEPARATOR 0 0 menu-append
                ;M

;Class

\ MENUBAR SampleMenuBar
\       POPUP "&File"
\               MENULINE "&Open" "BYE"

|Class MENULINE <Super  MENUITEMS

:M ClassInit:   ( -- )
                ClassInit: super
                m"text" ,"text"
                ;M

:M DoMenu:      ( IDM -- )
                mid =
                if      mtext count + 1+ count bounds
                        do      i c@ pushkey
                        loop    13 pushkey
                then    ;M

;Class

|Class MENUMESSAGE  <Super  MENUITEMS

int check_flag

:M ClassInit:   ( check_flag -- )
                ClassInit: super
                m"text"
                to check_flag
                ;M

:M LoadMenu:    ( pid parent -- )
                to parent
                MF_STRING MF_GRAYED or
                check_flag
                if      MF_CHECKED or
                then    mid mtext 1+ menu-append
                ;M

;Class

\ MENUBAR SampleMenuBar
\       POPUP "&File"
\               MENUITEM "&Open"  'O' +k_control pushkey ;
\               MENUITEM "E&xit"  'X' +k_control pushkey ;

|Class MENUITEM <Super  MENUITEMS

int mfunc               \ the menu function

:M ClassInit:   ( -- )
                ClassInit: super
                m"text"
                here to mfunc
                hide !csp docol , ]
                ;M

:M DoMenu:      ( IDM -- )
                mid =
                if      mfunc execute-menufunc
                then    ;M

;Class

\ define a menu item that has a handle, so we can manipulate it
\               :MENUITEM chkm1 "E&xit"  'X' +k_control pushkey ;
\ then check it with:   true Check: chkm1

:Class :MENUITEM <Super MENUITEM
;Class


:Class :MENUFUNC <Super  MENUITEM

MAXSTRING bytes mstring          \ the menu text

:M "SetMenuText: ( a1 n1 -- )
                mstring place
                mstring +NULL
                mstring 1+ SetMenuText: super
                mstring to mtext
                ;M

:M SetMenuText: ( z$ -- )
                dup SetMenuText: super
                1- to mtext
                ;M

:M GetMenuText: ( -- a1 n1 )
                mstring count 
                ;M

:M SetMenuFunc: ( cfa -- )
                to mfunc
                ;M

:M DoMenu:      ( IDM -- )              \ function receives the address of the menu object
                mid =                   \ which it should discard before returning
                if      Addr: self mfunc execute-menufunc
                then    ;M

:M LoadMenu:    ( pid parent -- )
                to parent
                dup to popid
                MF_STRING mid mtext 1+ menu-append
                ;M

;Class


|Class MENUCONSOLE <Super  MENUITEM   \ Only for use with the console window

:M DoMenu:      ( IDM -- )
                mid =
                if      mfunc execute-menufunc
                        cr getxy to ledit-y to ledit-x  \ move lineeditor down
                then    ;M

;Class     

: adjust-forth  ( -- )
                1 c" ADJFORTH.TXT" $browse ;

EXTERNAL

MENUBAR Min-Menu-bar
    POPUP "&File"
        MENUCONSOLE     "E&xit"                 bye ;
    POPUP "&Help"
        MENUITEM        "Help Contents"         help-index ;
        MENUITEM        "Help on Help"          help-on-help ;
ENDBAR

Min-Menu-Bar to DefaultMenuBar

MENUBAR Win32Forth-Menu-bar

\    SYSPOPUP
\        MENUSEPARATOR
\        MENUCONSOLE     "Stop Current Operation"  true abort" Aborting!" ;

    POPUP "&File"
        MENUITEM        "&Edit Forth File...\tCtrl+O"      edit-forth ;
        MENUITEM        "&Load Forth File...\tCtrl+L"      load-forth ;
        MENUSEPARATOR
        MENUITEM        "Open a Web link...\tCtrl+W"       open-web ;
        MENUSEPARATOR
        MENUCONSOLE     "&Save Forth System..."            save-forth ;
        MENUCONSOLE     "&Adjust Forth Dictionaries..."    adjust-forth ;
        MENUSEPARATOR

\ *****************************************************************************
\ *****************************************************************************
\ ******** The following lines are for example ONLY, they show how to use sub
\ ******** menus, the code will not run, so don't un-comment it out.
\ *****************************************************************************
\ *****************************************************************************
\         SUBMENU         "Keyboard &Macros"
\             MENUITEM    "&New  Key Macro Recording..."   a-new-log ;
\             MENUITEM    "&Stop Key Macro Recording"      logging-off ;
\             MENUSEPARATOR
\             MENUITEM    "&Edit Key Macro Log File..."       edit-log ;
\             MENUITEM    "&Play Key Macro Log File..."       play-log ;
\ \ ******** SUBMENUs can even be nested as many levels as you wish! *********
\ \            SUBMENU         "Keyboard Macros"
\ \                MENUITEM    "Edit Key Log File"        edit-log ;
\ \                MENUITEM    "Play Key Log File"        play-log ;
\ \                ENDSUBMENU
\             ENDSUBMENU
\         MENUSEPARATOR

        MENUITEM        "Print Setup... "                       page-setup ;
        MENUITEM        "Pages Up Setup..."                     page-up-setup ;
        MENUITEM        "&Print Forth File..."                  print-forth ;
        MENUITEM        "Print Forth Console Window...\tCtrl+P" print-screen ;
        MENUITEM        "Print Forth Console Buffer..."         print-console ;
        MENUSEPARATOR
        MENUCONSOLE     "E&xit Win32Forth     \tBYE"    bye ;

    POPUP "&Edit"
        MENUITEM        "&Cut and Clear Console \tCtrl+X"  cut-console ;
        MENUITEM        "&Copy Highlighted Text \tCtrl+C"  copy-console ;
        MENUITEM        "&Paste to Keyboard     \tCtrl+V"  paste-load ;
        MENUSEPARATOR
        MENUITEM        "&Mark all Text         \tCtrl+A"  mark-all ;

    POPUP "&Display"
  false MENUMESSAGE     "--- System Functions ---       \tForth Word"
        MENUSEPARATOR
        MENULINE        "This Programs Name             \t.PROGRAM"  ".PROGRAM"
        MENULINE        "Version of Win32Forth          \t.VERSION"  ".VERSION"
        MENULINE        "Operating System Version       \t.PLATFORM" ".PLATFORM"
        MENULINE        "Current HELP file              \t.HELP"     ".HELP"
        MENULINE        "Console Current Size           \tGetColRow . ." "GetColRow . ."
        MENULINE        "Console Maximum Size           \tGetMaxColRow . ." "GetMaxColRow . ."
        MENULINE        "Return Stack Contents          \t.RSTACK"   ".RSTACK"
        MENULINE        "Memory Used and Available      \t.FREE"     ".FREE"
        MENULINE        "File Search Path               \t.FPATH"    ".FPATH"
        MENUSEPARATOR
  false MENUMESSAGE     "--- Vocabulary Functions ---   \tForth Word"
        MENUSEPARATOR
        MENULINE        "Words in Current Vocabulary    \tWORDS"     "WORDS"
        MENULINE        "All Vocabulary Statistics      \tVOCS"      "VOCS"
        MENULINE        "Current Vocab Thread Counts    \t.COUNTS"   ".COUNTS"
        MENULINE        "Current Vocab Thread Words     \t.THREADS"  ".THREADS"
        MENUSEPARATOR
  false MENUMESSAGE     "--- List Functions ---         \tForth Word"
        MENUSEPARATOR
        MENULINE        "List of Classes in Win32Forth  \t.CLASSES"  ".CLASSES"
        MENULINE        "List of Loaded Files           \t.LOADED"   ".LOADED"
        MENULINE        "List of Fonts in System        \t.FONTS"    ".FONTS"
        MENULINE        "List of Deferred &Word         \t.DEFERRED" ".DEFERRED"
        MENULINE        "List of Execution Chains       \t.CHAINS"   ".CHAINS"
        MENULINE        "List of Pointers               \t.POINTERS" ".POINTERS"
        MENULINE        "List of Dynamic Memory Used    \t.MALLOCS"  ".MALLOCS"
        MENULINE        "List of Win32API Calls Used    \t.PROCS"    ".PROCS"
        MENUSEPARATOR
  false MENUMESSAGE     "--- Date Functions ---         \tForth Word"
        MENUSEPARATOR
        MENULINE        "Todays &Date                   \t.DATE"    ".DATE"
        MENULINE        "The Current &Time              \t.TIME"    ".TIME"

    POPUP "&Macros"
        MENUITEM    "&New Key Recording File..."                con-new-macro ;
        MENUITEM    "&Start - Stop Key Recording \tCtrl+Shft+S" start/stop-macro ;
        MENUITEM    "&Edit Key Macro Log File..."               edit-log ;
        MENUSEPARATOR
        MENUITEM    "&Play Key File"                            con-play-macro ;
        MENUITEM    "RePlay &Last Key File       \tCtrl+Shft+M" replay-macro ;
        MENUITEM    "&Repeat Key File 'n' times..\tCtrl+Shft+R" conhndl repeat-amacro ;

    POPUP "&Help"
        MENULINE    "&Help me get Started!       \tF1    'BROWSE STARTUP.TXT'"          "BROWSE HTM\STARTUP.TXT"
        MENULINE    "&Win32Forth FAQ's           \t      'BROWSE WIN32FOR.FAQ'"         "BROWSE HTM\WIN32FOR.FAQ"
\       :MENUITEM hlpmc  "Help &Contents"                                               help-index ;
\        MENUITEM        "&Help on Help"                                                help-on-help ;
        MENUSEPARATOR
        MENULINE    "&Whats NEW in Win32Forth    \t      'BROWSE WIN32FOR.NEW'"         "BROWSE HTM\WIN32FOR.NEW"
        MENULINE    "&Previous Win32Forth Changes\t      'BROWSE WIN32FOR.PRV'"         "BROWSE HTM\WIN32FOR.PRV"
        MENULINE    "&Utilities in Win32Forth    \t      'BROWSE UTILDOC.TXT'"          "BROWSE HTM\UTILDOC.TXT"
        MENULINE    "&ANS Required Documentation \t      'BROWSE ANSI.TXT'"             "BROWSE HTM\ANSI.TXT"
        MENULINE    "&DPANS94 Ansi Standard      \t      'BROWSE DPANS.HTM'"            "BROWSE HTM\DPANS.HTM"
        MENULINE    "ANS Forth Word List         \t      'BROWSE DPANSF.HTM'"           "BROWSE HTM\DPANSF.HTM"
        MENULINE    "Assembler Documentation     \t      'BROWSE 486ASM.TXT'"           "BROWSE HTM\486ASM.TXT"
        MENUSEPARATOR
  false MENUMESSAGE     "Web Based Resources"
        MENUSEPARATOR
        SUBMENU     "Forth Interest Group"
                MENUITEM    "Forth Home page"                   s" http://www.forth.org"                                conhndl "Web-Link ;
                MENUITEM    "FIG Membership Information"        s" http://www.forth.org/membership.html"                conhndl "Web-Link ;
                MENUITEM    "Forth Literature page"             s" http://www.forth.org/forthlit.html"                  conhndl "Web-Link ;
                MENUITEM    "Forth Compilers page"              s" http://www.forth.org/forthcomp.html"                 conhndl "Web-Link ;
                MENUITEM    "FTP access"                        s" ftp://ftp.forth.org/pub/forth"                       conhndl "Web-Link ;
                MENUITEM    "FIG UK"                            s" http://www.users.zetnet.co.uk/aborigine/forth.htm"   conhndl "Web-Link ;
                ENDSUBMENU
        SUBMENU     "Forth Web pages"
          false MENUMESSAGE      "Introductory"
                MENUSEPARATOR
                MENUITEM    "Leo Wong           \tForth Tutorial"   s" http://www.albany.net/~hello/simple.htm"  conhndl "Web-Link ;
                MENUSEPARATOR
          false MENUMESSAGE     "Win32Forth"
                MENUSEPARATOR
                MENUITEM    "Dave Pochin        \tOnline Guide"          s" http://www.sunterr.demon.co.uk/guide.htm" conhndl "Web-Link ;
                MENUITEM    "Jeff Kelm          \tWin32API Examples"     s" http://www.concentric.net/~jkelm/"        conhndl "Web-Link ;
                MENUITEM    "Michael Hillerström\tDialog Editor"         s" http://users.cybercity.dk/~ccc27382/"     conhndl "Web-Link ;
                MENUITEM    "Marc Petremann     \tForth FAQs & Examples" s" http://perso.wanadoo.fr/mp7/forth/"       conhndl "Web-Link ;
                MENUITEM    "Jos v.d.Ven        \tGraphics & Examples"   s" http://home.planet.nl/~josv/"             conhndl "Web-Link ;
                MENUSEPARATOR
          false MENUMESSAGE     "Macintosh"
                MENUSEPARATOR
                MENUITEM    "Michael Hore       \tJfar MOPS Page"       s" http://www.netaxs.com/~jayfar/mops.html"  conhndl "Web-Link ;
                ENDSUBMENU
        SUBMENU     "Forth News Groups"
                MENUITEM    "comp.lang.forth    \tGeneral Forth"        s" news:comp.lang.forth"                     conhndl "Web-Link ;
                MENUITEM    "comp.lang.forth.mac\tMacintosh Specific"   s" news:comp.lang.forth.mac"                 conhndl "Web-Link ;
                MENUITEM    "de.comp.lang.forth \tGerman Language"      s" news:de.comp.lang.forth"                  conhndl "Web-Link ;
                ENDSUBMENU
        SUBMENU     "comp.lang.forth FAQs"
                MENUITEM    "General Information"       s" http://www.faqs.org/faqs/computer-lang/forth-faq/part1/"  conhndl "Web-Link ;
                MENUITEM    "Online Resources"          s" http://www.faqs.org/faqs/computer-lang/forth-faq/part2/"  conhndl "Web-Link ;
                MENUITEM    "Vendors & Authors"         s" http://www.faqs.org/faqs/computer-lang/forth-faq/part3/"  conhndl "Web-Link ;
                MENUITEM    "Forth Systems"             s" http://www.faqs.org/faqs/computer-lang/forth-faq/part4/"  conhndl "Web-Link ;
                MENUITEM    "Books & Periodicals"       s" http://www.faqs.org/faqs/computer-lang/forth-faq/part5/"  conhndl "Web-Link ;
                MENUITEM    "Groups & Organizations"    s" http://www.faqs.org/faqs/computer-lang/forth-faq/part6/"  conhndl "Web-Link ;
                ENDSUBMENU
        MENUSEPARATOR
        MENUITEM        "&About Win32Forth"     about-Win32Forth ;

ENDBAR

\ these two lines illustrate how to make a popup that runs code

\   POPUPITEM "WORDS"           words ;

\   BREAKPOPUP                          \ force start of a new menu line
\   POPUPITEM "DUMPHERE"   here 32 dump ;


\ Note: The :MENUITEM hplmc above creates a named menu item that can be
\       later checked or unchecked with the following commands:
\
\       true  Check: hlpmc              \ turn on the items check mark
\       false Check: hlpmc              \ clear the items check mark
\
\ The Enable: method can also be sued in the same way as follows:
\
\       true  Enable: hlpmc             \ Enable the item
\       false Enable: hlpmc             \ Disable the item

Win32Forth-menu-bar value console-menu  \ the default Forth console menu

POPUPBAR Win32Forth-Popup-bar
    POPUP " "
        MENUITEM        "&Copy Highlighted Text \tCtrl+C"  copy-console ;
        MENUITEM        "&Paste to Keyboard     \tCtrl+V"  paste-load ;
        MENUSEPARATOR
        MENUITEM        "&Mark all Text         \tCtrl+A"  mark-all ;
        MENUSEPARATOR
        MENUCONSOLE     "Exit"                  bye ;
ENDBAR

Win32Forth-Popup-bar value console-popup

INTERNAL        \ more internal definitions

: menukey       ( -- c1 )               \ keyboard/event handler for console menus
                cursorinview
                BEGIN   _mkey
                        dup menu_mask and
                WHILE   havemenu?
                        IF      0xFFFF and
                                dup DoMenu: console-menu
                                dup DoMenu: console-popup
                        THEN    drop
                REPEAT  menukey-more ;

: menu-forth-io ( -- )
                ['] menukey is key ;

menu-forth-io

FORTH-IO-CHAIN CHAIN-ADD MENU-FORTH-IO

: RightMouseClick ( -- )                        \ Handle a right mouse click
                mouseflags 3 and 2 <> ?EXIT     \ exit if not right mouse clicked
                mousex mousey conhndl Track: console-popup ;

MOUSE-CHAIN CHAIN-ADD RightMouseClick

: Start-console-menu { \ mlink -- }     \ startup the console's menubar
                menubar-link @          \ clear all menu handles
                begin   dup
                while   dup cell+ @ to mlink
                        ZeroMenu: mlink
                        @
                repeat  drop
                true havemenu!
             ZeroMenu: console-menu
     conhndl loadmenu: console-menu
           menuhandle: console-menu
          conhndl call SetMenu havemenu!
             ZeroMenu: console-popup
     conhndl loadmenu: console-popup ;

INITIALIZATION-CHAIN CHAIN-ADD START-CONSOLE-MENU

: Menu-off      ( -- )                  \ turn off the console's menubar
                false havemenu!
                0 conhndl call SetMenu 0= havemenu!
                ;

: Set-console-menu ( menubar -- )       \ switch to a new console menubar
                menu-off
                to console-menu
                start-console-menu ;

: Set-console-popup ( menubar -- )       \ switch to a new console popup
                to console-popup ;

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ Function key additions to the keyboard interpreter during commandline entry
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

: F1-help       ( -- )                  \ F1 starts up the initial help file
                0 c" F1-help.txt" $BROWSE ;

: ?f1-help      ( char flag -- char flag )
                dup ?exit               \ exit if flag is TRUE
                over k_F1 = if 0= F1-help then  ;

ledit-chain chain-add ?f1-help          \ help key recognition

: ?macro-keys   ( chad flag -- char flag )
                dup ?exit
                over 'S' +k_control +k_shift = if 0= start/stop-macro      EXIT then
                over 'M' +k_control +k_shift = if 0= replay-macro          EXIT then
                over 'R' +k_control +k_shift = if 0= CONHNDL repeat-amacro EXIT then
                over 'O' +k_control          = if 0= edit-forth            EXIT then
                over 'W' +k_control          = if 0= open-web              EXIT then
                over 'L' +k_control          = if 0= load-forth            EXIT then 
                over 'P' +k_control          = if 0= print-screen          EXIT then
                over 'X' +k_control          = if 0= cut-console
                                                  getxy nip to ledit-y     EXIT then
                over 'A' +k_control          = if 0= mark-all              EXIT then
                over 'C' +k_control          = if 0= copy-console          EXIT then
                over 'V' +k_control          = if 0= paste-load            EXIT then
                ;

ledit-chain chain-add ?macro-keys       \ add macro key recognition

MODULE          \ end of the module

