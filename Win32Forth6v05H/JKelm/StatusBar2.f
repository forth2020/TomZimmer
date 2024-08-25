\    File: StatusBar2.f
\  Author: Jeff Kelm
\ Created: 12-Aug-1998
\ Updated: 07-Oct-1998
\ Similar to StatusBar.f, but implements a multi-part status bar



NEEDS WinBase.f
NEEDS Menubar.f
NEEDS Statbar.f



\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ \\\ Define Command ID's
\
CreateNewID constant IDM_NEW
CreateNewID constant IDM_OPEN
CreateNewID constant IDM_SAVE
CreateNewID constant IDM_COPY
CreateNewID constant IDM_PASTE
CreateNewID constant IDM_PRINT
CreateNewID constant IDM_ABOUT
CreateNewID CONSTANT IDM_EXIT
CreateNewID constant IDM_CUT
CreateNewID constant ID_Status



\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ \\\ Define Menu
\
PopupMenu FileMenu   \ standard File menu
   IDM_NEW     Z" &New"            AddItem: FileMenu
   IDM_OPEN    Z" &Open..."        AddItem: FileMenu
   IDM_SAVE    Z" &Save"           AddItem: FileMenu
                                 Seperator: FileMenu
   IDM_PRINT   Z" &Print"          AddItem: FileMenu
                                 Seperator: FileMenu
   IDM_EXIT    Z" E&xit"           AddItem: FileMenu

PopupMenu EditMenu   \ standard Edit menu
   IDM_CUT     Z" Cu&t\tCtrl+X"    AddItem: EditMenu
   IDM_COPY    Z" &Copy\tCtrl+C"   AddItem: EditMenu
   IDM_PASTE   Z" &Paste\tCtrl+V"  AddItem: EditMenu

PopupMenu HelpMenu   \ standard Help menu
   IDM_ABOUT   Z" &About..."  AddItem: HelpMenu


Menubar appMenu  \ create the application menubar
   GetHandle: FileMenu  Z" &File"    AddMenu: appMenu
   GetHandle: EditMenu  Z" &Edit"    AddMenu: appMenu
   GetHandle: HelpMenu  Z" &Help"    AddMenu: appMenu



\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ \\\ Statusbar
\
MultiStatusbar sBar



\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ \\\ Main Routine
\
: MenuName    Z" StatusbarMenu"    rel>abs ;
: ClassName   Z" StatusbarWClass"  rel>abs ;
: wTitle      Z" Statusbar Sample" rel>abs ;

0 VALUE hWnd            \ application window handle



: MenuFunc   \ Default "Not Implemented" function for menu items
   MB_OK  Z" Command" rel>abs  Z" Not implemented in this sample." rel>abs
   hWnd ( NULL)  Call MessageBox drop ;



\ Object to build and hold mouse position string
:Object szBuf  <Super String

:M Update: ( n1 n2)
   SWAP
   S" Mouse position: " Put: self
   S>D (D.) Append: self
   S" , "   Append: self
   S>D (D.) Append: self
   ;M

:M Display: ( -- lpString)
   Get: self  DROP
   ;M

;Object



\ Data for a multipart status bar
Create aWidths   \ distance to right side of each section from beginning
   150 , 300 , 450 , -1 ,
4 VALUE nParts



\ Define the window procedure
: (HELLO-WNDPROC)  { hWnd msg wParam lParam -- result }
   msg CASE
      WM_CREATE OF
      \ Create the Status Bar
         hWnd Create: sBar
      \ Initialize muli-part status bar
         aWidths nParts SetParts: sBar
      \ Put text in last three sections
         Z" The Second Section"   1  SetText: sBar
         Z" The Third Section"    2  SetText: sBar
         Z" The Last Section"     3  SetText: sBar
         0 ENDOF

      WM_MOUSEMOVE OF
         lParam LOWORD  lParam HIWORD  Update: szBuf
         Display: szBuf 0 SetText: sBar
         ENDOF

      WM_SIZE OF
         Redraw: sBar       \ resize Status Window
         0  ENDOF

      WM_COMMAND OF   \ ( hWndToolbar idButton WM_COMMAND hWnd)
         wParam LOWORD CASE
                 IDM_NEW    OF  MenuFunc  ENDOF
                 IDM_OPEN   OF  MenuFunc  ENDOF
                 IDM_SAVE   OF  MenuFunc  ENDOF
                 IDM_CUT    OF  MenuFunc  ENDOF
                 IDM_COPY   OF  MenuFunc  ENDOF
                 IDM_PASTE  OF  MenuFunc  ENDOF
                 IDM_PRINT  OF  MenuFunc  ENDOF
                 IDM_EXIT   OF  MenuFunc  ENDOF
                 IDM_ABOUT  OF  MenuFunc  ENDOF
            ENDCASE
         0 ENDOF

      DEFAULTOF  lParam wParam msg hWnd Call DefWindowProc  ENDOF
   ENDCASE ;

4 callback hello-wndproc (hello-wndproc)



\ Application Window Class EXtended structure
CREATE wcStatusbarEx   \ WNDCLASSEX structure
   12 cells ,                                   \ cbSize
   NULL ,                                       \ style
   hello-wndproc rel>abs ,                      \ lpfnWndProc
   0 ,                                          \ cbClsExtra
   0 ,                                          \ cbWndExtra
   appInst ,                                    \ hInstance
   IDI_WINLOGO NULL      Call LoadIcon ,        \ hIcon
   IDC_CROSS NULL        call LoadCursor ,      \ hCursor
   WHITE_BRUSH           call GetStockObject ,  \ hbrBackground
   MenuName ,                                   \ lpszMenuName
   ClassName ,                                  \ lpszClassName
   0 ,                                          \ hIconSm

: REGISTER-CLASS-EX   wcStatusbarEx rel>abs Call RegisterClassEx ?WinError ;

: CREATE-DEMO-WINDOW
   NULL                         \ creation parameters
   AppInst                      \ instance handle
   GetHandle: appMenu           \ menu handle
   NULL                         \ parent window
   CW_USEDEFAULT CW_USEDEFAULT  \ window size ( h w)
   CW_USEDEFAULT CW_USEDEFAULT  \ window position ( y x )
   WS_OVERLAPPEDWINDOW          \ window style
   wTitle                       \ window title
   ClassName                    \ class name
   NULL                         \ extended window style
   Call CreateWindowEx DUP  TO hWnd  ?WinError ;




\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ \\\ Main Call
\
: CLEANUP
   hWnd Call DestroyWindow ?WinError
   appInst ClassName Call UnregisterClass ?WinError ;

: DEMO  ( -- )
        REGISTER-CLASS-EX  CREATE-DEMO-WINDOW
        SW_SHOWNORMAL hWnd Call ShowWindow   DROP
                      hWnd Call UpdateWindow DROP ;


CR .( Type 'DEMO' to run program, 'CLEANUP' to kill demo.)

