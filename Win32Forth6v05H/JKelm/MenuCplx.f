\    File: MenuCplx.f
\  Author: Jeff Kelm
\ Created: 04-Sep-1998
\ Updated: 07-Oct-1998
\ Example of menu creation (including submenus) using the class libraries



NEEDS WinBase.f
NEEDS MenuBar.f



\ Command identifiers
CreateNewID CONSTANT IDM_NEW
CreateNewID CONSTANT IDM_OPEN
CreateNewID CONSTANT IDM_SAVE
CreateNewID CONSTANT IDM_EXIT
CreateNewID CONSTANT IDM_CUT
CreateNewID CONSTANT IDM_COPY
CreateNewID CONSTANT IDM_PASTE
CreateNewID CONSTANT IDM_ABOUT
CreateNewID CONSTANT IDM_TOFILE
CreateNewID CONSTANT IDM_ITEM1
CreateNewID CONSTANT IDM_ITEM2
CreateNewID CONSTANT IDM_ITEM3
CreateNewID CONSTANT IDM_ITEM4
CreateNewID CONSTANT IDM_ITEM5
CreateNewID CONSTANT IDM_ITEM6
CreateNewID CONSTANT IDM_ITEM7
CreateNewID CONSTANT IDM_ITEM8
CreateNewID CONSTANT IDM_PAL1
CreateNewID CONSTANT IDM_PAL2
CreateNewID CONSTANT IDM_PAL3

\ Create the window Menu

PopupMenu TooMuch   \ example sub-submenu
   IDM_ITEM1  Z" Item #1" AddItem: TooMuch
   IDM_ITEM2  Z" Item #2" AddItem: TooMuch
   IDM_ITEM3  Z" Item #3" AddItem: TooMuch
   IDM_ITEM4  Z" Item #4" AddItem: TooMuch
   \ break menu to a second column
   IDM_ITEM5  Z" Item #5\tCtrl+5" MF_STRING MF_MENUBARBREAK OR
                               AppendMenu: TooMuch
   IDM_ITEM6  Z" Item #6" AddItem: TooMuch
   IDM_ITEM7  Z" Item #7" AddItem: TooMuch
   IDM_ITEM8  Z" Item #8" AddItem: TooMuch

PopupMenu PrintSubmenu   \ example submenu
   IDM_TOFILE  Z" to &File..." AddItem: PrintSubmenu
   GetHandle: TooMuch  Z" to &Printer..." AddSubmenu: PrintSubmenu

PopupMenu FileMenu   \ standard File menu
   IDM_NEW   Z" &New"         AddItem: FileMenu
   IDM_OPEN  Z" &Open..."     AddItem: FileMenu
   IDM_SAVE  Z" &Save"        AddItem: FileMenu
   IDM_SAVE  Z" Save &As..."  AddItem: FileMenu
                            Seperator: FileMenu
   GetHandle: PrintSubmenu  Z" &Print"  AddSubmenu: FileMenu
                            Seperator: FileMenu
   IDM_EXIT  Z" E&xit"        AddItem: FileMenu

PopupMenu EditMenu   \ standard Edit menu
   IDM_CUT     Z" Cu&t\tAlt+X"   AddItem: EditMenu
   IDM_COPY    Z" &Copy\tAlt+C"  AddItem: EditMenu
   IDM_PASTE   Z" &Paste\tAlt+V" AddItem: EditMenu

PopupMenu HelpMenu   \ standard Help menu
   IDM_ABOUT   Z" &About App..."  AddItem: HelpMenu

PopupMenu PalMenu
   IDM_PAL1    Z" Tool 1\tCtrl+1" AddItem: PalMenu
   IDM_PAL2    Z" Tool 2\tCtrl+2" AddItem: PalMenu
   IDM_PAL3    Z" Tool 3\tCtrl+3" AddItem: PalMenu


Menubar appMenu  \ create the application menubar
   GetHandle: FileMenu  Z" &File"     AddMenu: appMenu
   GetHandle: EditMenu  Z" &Edit"     AddMenu: appMenu
   GetHandle: HelpMenu  Z" &Help"     AddMenu: appMenu
   GetHandle: PalMenu   Z" &Palette"  AddMenu: appMenu



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
                 IDM_SAVE OF  MenuFunc  ENDOF
                 IDM_EXIT OF  MenuFunc  ENDOF
                   DEFAULTOF  MenuFunc  ENDOF \ 'cuz I'm lazy
            ENDCASE
         0 ENDOF

\      WM_DESTROY OF  Call PostQuitMessage  ENDOF

      DEFAULTOF  lParam wParam msg hWnd Call DefWindowProc  ENDOF
   ENDCASE ;

4 callback MainWndProc (MainWndProc)



\ Application Window Class structure
CREATE wcSample   \ WNDCLASSEX structure
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
   wcSample rel>abs Call RegisterClassEx ?WinError
   CreateMainWindow DUP TO hWnd         ?WinError
   SW_SHOWNORMAL hWnd Call ShowWindow   DROP
                 hWnd Call UpdateWindow ?WinError ;


CR .( Type 'DEMO' to run program and 'CLEANUP' afterwards )

