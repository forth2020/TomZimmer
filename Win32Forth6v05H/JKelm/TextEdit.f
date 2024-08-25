\    File: TextEdit.f
\  Author: Jeff Kelm
\ Created: 04-Sep-1998
\ Updated: 15-Oct-1998



NEEDS WinBase.f
NEEDS Menubar.f
NEEDS Toolbar.f
NEEDS Statbar.f
NEEDS Editbox.f



\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ \\\ Define control ID values
\
CreateNewID CONSTANT IDM_NEW
CreateNewID CONSTANT IDM_OPEN
CreateNewID CONSTANT IDM_SAVE
CreateNewID CONSTANT IDM_SAVEAS
CreateNewID CONSTANT IDM_PGSETUP
CreateNewID CONSTANT IDM_PRINT
CreateNewID CONSTANT IDM_EXIT
CreateNewID CONSTANT IDM_UNDO
CreateNewID CONSTANT IDM_CUT
CreateNewID CONSTANT IDM_COPY
CreateNewID CONSTANT IDM_PASTE
CreateNewID CONSTANT IDM_DEL
CreateNewID CONSTANT IDM_ALL
CreateNewID CONSTANT IDM_FIND
CreateNewID CONSTANT IDM_NEXT
CreateNewID CONSTANT IDM_REPL
CreateNewID CONSTANT IDM_TOPICS
CreateNewID CONSTANT IDM_ABOUT
CreateNewID CONSTANT IDM_WRAP
CreateNewID CONSTANT ID_EDITCHILD
CreateNewID CONSTANT IDM_READONLY
CreateNewID CONSTANT ID_TOOLBAR



\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ \\\ Create a semi-standard window menubar
\
:Object appMenu <Super Menubar

   PopupMenu FileMenu   \ standard File menu
   PopupMenu EditMenu   \ standard Edit menu
   PopupMenu SrchMenu   \ standard Search menu
   PopupMenu HelpMenu   \ standard Help menu

   :M Create:  \ create the menubar and add items
               \ the File menu
               IDM_NEW      Z" &New"            AddItem: FileMenu
               IDM_OPEN     Z" &Open..."        AddItem: FileMenu
               IDM_SAVE     Z" &Save"           AddItem: FileMenu
               IDM_SAVEAS   Z" Save &As..."     AddItem: FileMenu
                                              Seperator: FileMenu
               IDM_PGSETUP  Z" Page Se&tup..."  AddItem: FileMenu
               IDM_PRINT    Z" &Print"          AddItem: FileMenu
                                              Seperator: FileMenu
               IDM_READONLY Z" &Read Only"      AddItem: FileMenu
                                              Seperator: FileMenu
               IDM_EXIT     Z" E&xit"           AddItem: FileMenu

               \ the Edit menu
               IDM_UNDO    Z" &Undo\tCtrl+Z"   AddItem: EditMenu
                                             Seperator: EditMenu
               IDM_CUT     Z" Cu&t\tCtrl+X"    AddItem: EditMenu
               IDM_COPY    Z" &Copy\tCtrl+C"   AddItem: EditMenu
               IDM_PASTE   Z" &Paste\tCtrl+V"  AddItem: EditMenu
               IDM_DEL     Z" De&lete\tDel"    AddItem: EditMenu
                                             Seperator: EditMenu
               IDM_ALL     Z" Select &All"     AddItem: EditMenu
                                             Seperator: EditMenu
               IDM_WRAP    Z" &Word Wrap"      AddItem: EditMenu

               \ the Search menu
               IDM_FIND    Z" &Find..."        AddItem: SrchMenu
               IDM_NEXT    Z" Find &Next\tF3"  AddItem: SrchMenu
               IDM_REPL    Z" &Replace"        AddItem: SrchMenu

               \ the Help menu
               IDM_TOPICS  Z" &Help Topics"      AddItem: HelpMenu
                                               Seperator: HelpMenu
               IDM_ABOUT   Z" &About Editor..."  AddItem: HelpMenu

               \ the menubar
               GetHandle: FileMenu  Z" &File"   AddMenu: self
               GetHandle: EditMenu  Z" &Edit"   AddMenu: self
               GetHandle: SrchMenu  Z" &Search" AddMenu: self
               GetHandle: HelpMenu  Z" &Help"   AddMenu: self

               \ disable the unimplemented search menu items
               IDM_FIND   Gray: SrchMenu
               IDM_NEXT   Gray: SrchMenu
               IDM_REPL   Gray: SrchMenu
               ;M

   :M CheckWrap:  IDM_WRAP Checked: EditMenu ;M

   :M UncheckWrap:  IDM_WRAP Unchecked: EditMenu ;M

   :M CheckReadOnly:  IDM_READONLY Checked: FileMenu ;M

   :M UncheckReadOnly:  IDM_READONLY Unchecked: FileMenu ;M

;Object



\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ \\\ Toolbar Definition
\
:Object tBar  <Super GenericToolbar

:M Create: ( hParent)
      Create: super

      \ GetBmpResource won't work under win32s?
      Win32s? IF  LoadStdBmp: self   ( use built-in bmps)
            ELSE  15  Z" StdTBar.bmp" GetBmpResource
                  LoadBitmap: self
            THEN

      Seperator: self

      IDM_NEW  STD_FILENEW  Button: self
      IDM_OPEN STD_FILEOPEN Button: self
      IDM_SAVE STD_FILESAVE Button: self IDM_SAVE DisableButton: self

      Seperator: self

      IDM_CUT   STD_CUT    Button: self
      IDM_COPY  STD_COPY   Button: self
      IDM_PASTE STD_PASTE  Button: self
      IDM_DEL   STD_DELETE Button: self

      Seperator: self

      IDM_UNDO  STD_UNDO  Button: self

      Seperator: self

      IDM_PRINT  STD_PRINT  Button: self

      Seperator: self

      IDM_ABOUT  STD_HELP  Button: self

      Show: self
      ;M

:M SetTooltipText: ( lparam)   \ Tooltip text for buttons
      DUP CELL+ abs>rel @   \ get control id
      CASE
         IDM_NEW   OF  Z" New"    ENDOF
         IDM_OPEN  OF  Z" Open"   ENDOF
         IDM_SAVE  OF  Z" Save"   ENDOF
         IDM_CUT   OF  Z" Cut"    ENDOF
         IDM_COPY  OF  Z" Copy"   ENDOF
         IDM_PASTE OF  Z" Paste"  ENDOF
         IDM_UNDO  OF  Z" Undo"   ENDOF
         IDM_DEL   OF  Z" Delete" ENDOF
         IDM_PRINT OF  Z" Print"  ENDOF
         IDM_ABOUT OF  Z" About"  ENDOF
            DEFAULTOF  Z" Undef"  ENDOF
         ENDCASE
      rel>abs SWAP 3 CELLS + abs>rel !
      ;M

;Object



\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ \\\ Statusbar
\
Statusbar sBar



\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ \\\ Openfile Dialog Box
\
FileOpenDialog BrowseText "Open Text File" "Text Files|*.txt;*.f;*.asm;*.doc;*.seq;*.bat;*.c??;*.h|All Files|*.*|"

CREATE cur-filename MAX-PATH ALLOT
   0 VALUE text-len             \ length of text
   0 VALUE text-ptr             \ address of current text line
   0 VALUE text-blen            \ total text buffer length

: "open-text    ( a1 n1 -- )
                2DUP R/O OPEN-FILE 0=
                IF      >R MAX-PATH MIN cur-filename place
                        \ release/allocate the text buffer
                        text-ptr ?DUP IF FREE DROP THEN
                        R@ FILE-SIZE DROP ( ior) D>S ( 32-bits) TO text-len
                        text-len 10000 + TO text-blen
                        text-blen ALLOCATE DROP TO text-ptr
                        \ read the file into memory
                        text-ptr text-len R@ READ-FILE DROP
                        TO text-len
                        R> CLOSE-FILE DROP
                        \ null terminate text
                        0 text-ptr text-len + C!
                else    3drop \ October 9th, 1998 - 22:09 Bruno Gauthier
                then    ;



\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ \\\ Multiline Edit Control Window
\
GenericMultilineEdit wEdit



\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ \\\ Generic Sample Window for testing
\

    0 VALUE hWnd          \ to hold handle for application window

: appName   Z" Editor" rel>abs ;
: wTitle   Z" Untitled - WinPad" rel>abs ;



\ Object to build and hold window title
:Object szTitle  <Super String

:M Update:   \ use filename to create window title
   cur-filename count  Put: self
   S"  - WinPad" Append: self
   ;M

:M Display: ( -- lpString)
   Get: self  DROP  rel>abs
   ;M

;Object



\ Default "Not Implemented" function for menu items
: MenuFunc
   MB_OK  Z" Command" rel>abs  Z" Not implemented yet." rel>abs
   hWnd  Call MessageBox drop ;



\ Define the window procedure
: (MainWndProc)  { hWnd msg wParam lParam -- result }
   msg CASE
      WM_CREATE OF
         hWnd Create: tBar      \ create toolbar control
         hWnd Create: sBar      \ Create a statusbar
         hWnd Create: wEdit     \ Create a TextEdit control
         \ initialize menu check marks
         UncheckWrap: appMenu
         UncheckReadOnly: appMenu
         0 ENDOF

      WM_SETFOCUS OF  GetHandle: wEdit Call SetFocus DROP  0 ENDOF

      WM_SIZE OF  TRUE
                  \ size Edit control
                  lParam HIWORD
                     GetWindowSize: tBar DROP -
                     GetWindowSize: sBar DROP - ( height)
                  lParam LOWORD                 ( width)
                  GetWindowSize: tBar DROP      ( top)
                  0                             ( left)
                  GetHandle: wEdit  Call MoveWindow
                  \ size Toolbar
                  0 0 TB_AUTOSIZE SendMessage: tBar DROP
                  \ size Statusbar
                  Redraw: sBar
                  0 ENDOF

      WM_COMMAND OF   ( htBar idButton WM_COMMAND hWnd)
         wParam LOWORD CASE
            IDM_UNDO OF  Undo: wEdit  ENDOF

            IDM_CUT OF  Cut: wEdit  ENDOF

            IDM_COPY OF  Copy: wEdit  ENDOF

            IDM_PASTE OF  Paste: wEdit  ENDOF

            IDM_DEL OF  Clear: wEdit  ENDOF

            IDM_ALL OF  SelectAll: wEdit  ENDOF

            IDM_WRAP OF  MenuFunc  ENDOF

            IDM_ABOUT  OF  MenuFunc  ENDOF

            IDM_READONLY OF  GWL_STYLE GetWindowLong: wEdit
                             ES_READONLY AND 0=
                               IF  TRUE ReadOnly: wEdit
                                   CheckReadOnly: appMenu
                             ELSE  FALSE ReadOnly: wEdit
                                   UncheckReadOnly: appMenu
                             THEN
                             hWnd Call DrawMenuBar ?WinError
                             ENDOF

            IDM_NEW OF  SelectAll: wEdit
                        Clear: wEdit
                        0 0 EM_EMPTYUNDOBUFFER GetHandle: wEdit  Call SendMessage DROP
                        S" Untitled" cur-filename place ( reset titlebar)
                        update: szTitle
                        display: szTitle 0 WM_SETTEXT hWnd  Call SendMessage DROP
                        ENDOF

            IDM_OPEN  OF  \ open a file
                          hWnd Start: BrowseText DUP C@
                          IF    COUNT "open-text
                          ELSE  DROP  THEN
                          \ Add text to window
                          text-ptr rel>abs 0 WM_SETTEXT
                             SendMessage: wEdit  DROP
                          hWnd Call UpdateWindow
                          text-ptr FREE DROP ( free alloc buffer)
                          \ update titlebar
                          update: szTitle
                          display: szTitle 0 WM_SETTEXT hWnd  Call SendMessage DROP
                          ENDOF

            IDM_SAVE    OF  MenuFunc  ENDOF
            IDM_SAVEAS  OF  MenuFunc  ENDOF
            IDM_PGSETUP OF  MenuFunc  ENDOF
            IDM_PRINT   OF  MenuFunc  ENDOF
            IDM_FIND    OF  MenuFunc  ENDOF
            IDM_NEXT    OF  MenuFunc  ENDOF
            IDM_REPL    OF  MenuFunc  ENDOF
            IDM_TOPICS  OF  MenuFunc  ENDOF

            IDM_EXIT OF  hWnd  Call DestroyWindow ?WinError  ENDOF

            ENDCASE
         0 ENDOF

      WM_NOTIFY OF   \ ( ^LPNMHDR idButton WM_NOTIFY hWnd)
         lparam 2 CELLS + abs>rel @   \ fetch code from NMHDR structure
         CASE
            TTN_NEEDTEXT OF  lparam SetTooltipText: tBar  ENDOF
                  DEFAULTOF  TRUE  ENDOF
            ENDCASE
         ENDOF

      WM_CLOSE OF  text-ptr FREE DROP
                   ENDOF

      WM_DESTROY OF  Call PostQuitMessage  ENDOF

      DEFAULTOF  lParam wParam msg hWnd Call DefWindowProc  ENDOF

   ENDCASE ;

4 callback MainWndProc (MainWndProc)


\ Application Window Class EXtented structure
CREATE wcSampleEx   \ WNDCLASSEX structure
   12 cells ,                                   \ cbSize
   NULL ,                                       \ style
   MainWndProc rel>abs ,                        \ lpfnWndProc
   0 ,                                          \ cbClsExtra
   0 ,                                          \ cbWndExtra
   appInst ,                                    \ hInstance
   IDI_APPLICATION NULL  call LoadIcon ,        \ hIcon
   IDC_ARROW NULL        call LoadCursor ,      \ hCursor
   WHITE_BRUSH           call GetStockObject ,  \ hbrBackground
   appName ,                                    \ lpszMenuName
   appName ,                                    \ lpszClassName
   0 ,                                          \ hIconSm

: CreateMainWindow ( -- hWnd)
   NULL                         \ creation parameters
   appInst                      \ instance handle
   GetHandle: appMenu           \ menu handle
   NULL                         \ parent window
   CW_USEDEFAULT CW_USEDEFAULT  \ window size ( h w)
   CW_USEDEFAULT CW_USEDEFAULT  \ window position ( y x )
   WS_OVERLAPPEDWINDOW          \ window style
   wTitle                       \ window title
   appName                      \ class name
   NULL                         \ extended window style
   Call CreateWindowEx ;



\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ \\\ Main Routines

: CLEANUP
   appInst appName Call UnregisterClass ?WinError
   text-ptr FREE DROP ;

: DEMO  ( -- )
   Create: appMenu
   wcSampleEx rel>abs Call RegisterClassEx ?WinError
   CreateMainWindow DUP TO hWnd         ?WinError
   SW_SHOWNORMAL hWnd Call ShowWindow   DROP
                 hWnd Call UpdateWindow ?WinError
   ;


CR .( Type 'DEMO' to run program and 'CLEANUP' afterwards )

