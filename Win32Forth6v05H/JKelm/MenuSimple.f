\    File: MenuSimple.f
\  Author: Jeff Kelm
\ Created: 03-Sep-1998
\ Updated: 07-Oct-1998



NEEDS WinBase.f
NEEDS Menubar.f



\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ Define menu item ID values

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



\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ Create a semi-standard window menubar
PopupMenu FileMenu   \ standard File menu
   IDM_NEW     Z" &New"            AddItem: FileMenu
   IDM_OPEN    Z" &Open..."        AddItem: FileMenu
   IDM_SAVE    Z" &Save"           AddItem: FileMenu
   IDM_SAVEAS  Z" Save &As..."     AddItem: FileMenu
                                 Seperator: FileMenu
   IDM_PGSETUP Z" Page Se&tup..."  AddItem: FileMenu
   IDM_PRINT   Z" &Print"          AddItem: FileMenu
                                 Seperator: FileMenu
   IDM_EXIT    Z" E&xit"           AddItem: FileMenu

PopupMenu EditMenu   \ standard Edit menu
   IDM_UNDO    Z" &Undo\tCtrl+Z"   AddItem: EditMenu
                                 Seperator: EditMenu
   IDM_CUT     Z" Cu&t\tCtrl+X"    AddItem: EditMenu
   IDM_COPY    Z" &Copy\tCtrl+C"   AddItem: EditMenu
   IDM_PASTE   Z" &Paste\tCtrl+V"  AddItem: EditMenu
   IDM_DEL     Z" De&lete\tDel"    AddItem: EditMenu
                                 Seperator: EditMenu
   IDM_ALL     Z" Select &All"     AddItem: EditMenu

PopupMenu SrchMenu   \ standard Search menu
   IDM_FIND    Z" &Find..."        AddItem: SrchMenu
   IDM_NEXT    Z" Find &Next\tF3"  AddItem: SrchMenu
   IDM_REPL    Z" &Replace"        AddItem: SrchMenu

PopupMenu HelpMenu   \ standard Help menu
   IDM_TOPICS  Z" &Help Topics"   AddItem: HelpMenu
                                Seperator: HelpMenu
   IDM_ABOUT   Z" &About App..."  AddItem: HelpMenu


Menubar appMenu  \ create the application menubar
   GetHandle: FileMenu  Z" &File"    AddMenu: appMenu
   GetHandle: EditMenu  Z" &Edit"    AddMenu: appMenu
   GetHandle: SrchMenu  Z" &Search"  AddMenu: appMenu
   GetHandle: HelpMenu  Z" &Help"    AddMenu: appMenu



\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ \\\ Generic Sample Window for testing

0 VALUE hWnd    \ to hold handle for application window
: appName   Z" Sample" rel>abs ;
: wTitle   Z" Sample Window" rel>abs ;


\ Default "Not Implemented" function for menu items
: MenuFunc
   MB_OK  Z" Command" rel>abs  Z" Not implemented in this sample." rel>abs
   hWnd ( NULL)  Call MessageBox drop ;

\ Define the window procedure
: (MainWndProc)  { hWnd msg wParam lParam -- result }
   msg CASE

      WM_COMMAND OF   \ ( hWndToolbar idButton WM_COMMAND hWnd)
         wParam LOWORD CASE
                 IDM_NEW  OF  MenuFunc  ENDOF
                 IDM_OPEN OF  MenuFunc  ENDOF
                 IDM_EXIT OF  MenuFunc  ENDOF
                   DEFAULTOF  MenuFunc  ENDOF \ 'cuz I'm lazy
            ENDCASE
         0 ENDOF

\      WM_DESTROY OF  Call PostQuitMessage  ENDOF

      DEFAULTOF  lParam wParam msg hWnd Call DefWindowProc  ENDOF
   ENDCASE ;

4 callback MainWndProc (MainWndProc)


\ Application Window Class structure
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
   hWnd  Call DestroyWindow ?WinError
   appInst appName Call UnregisterClass ?WinError ;

: DEMO  ( -- )
   wcSampleEx rel>abs Call RegisterClassEx ?WinError
   CreateMainWindow DUP TO hWnd         ?WinError
   SW_SHOWNORMAL hWnd Call ShowWindow   DROP
                 hWnd Call UpdateWindow ?WinError ;


CR .( Type 'DEMO' to run program and 'CLEANUP' afterwards )

