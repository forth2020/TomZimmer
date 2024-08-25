\    File: TBarExmp.f
\  Author: Jeff Kelm
\ Created: 24-Jul-1998
\ Updated: 15-Oct-1998



NEEDS WinBase.f
NEEDS Menubar.f
NEEDS Toolbar.f



\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ \\\ Define Command ID's
\
CreateNewID CONSTANT ID_TOOLBAR
CreateNewID CONSTANT IDM_COMBO
CreateNewID CONSTANT IDM_NEW
CreateNewID CONSTANT IDM_OPEN
CreateNewID CONSTANT IDM_SAVE
CreateNewID CONSTANT IDM_EXIT
CreateNewID CONSTANT IDM_CUT
CreateNewID CONSTANT IDM_COPY
CreateNewID CONSTANT IDM_PASTE
CreateNewID CONSTANT IDM_PRINT
CreateNewID CONSTANT IDM_ABOUT



Z" Toolbar.ico" GetIconResource  VALUE hIcon
Z" Toolbar.bmp" GetBmpResource  VALUE hBmp



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



\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ \\\ Toolbar Definition
\
:Object tBar  <Super GenericToolbar

:M Create: ( hParent)
      Create: super
      9 hBmp LoadBitmap: self

      Seperator: self       Seperator: self
      Seperator: self       Seperator: self
      Seperator: self       Seperator: self
      Seperator: self       Seperator: self
      Seperator: self       Seperator: self
      Seperator: self       Seperator: self
      Seperator: self

      IDM_NEW  0  Button: self
      IDM_OPEN 1  Button: self
      IDM_SAVE 2  Button: self

      Seperator: self

      IDM_CUT   3  Button: self
      IDM_COPY  4  Button: self
      IDM_PASTE 5  Button: self

      Seperator: self

      IDM_PRINT  6  Button: self

      Seperator: self

      IDM_ABOUT  7  Button: self

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
         IDM_PRINT OF  Z" Print"  ENDOF
         IDM_ABOUT OF  Z" About"  ENDOF
            DEFAULTOF  Z" Undef"  ENDOF
         ENDCASE
      rel>abs SWAP 3 CELLS + abs>rel !
      ;M

;Object



\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ \\\ Tooltip Window
\
0 VALUE hWndTT          \ tooltip window handle

10 CELLS CONSTANT sizeof(lpToolInfo)

CREATE lpToolInfo   sizeof(lpToolInfo) ALLOT
   lpToolInfo
   DUP CONSTANT lpToolInfo.cbSize      CELL+
   DUP CONSTANT lpToolInfo.uFlags      CELL+
   DUP CONSTANT lpToolInfo.hwnd        CELL+
   DUP CONSTANT lpToolInfo.uId         CELL+
   DUP CONSTANT lpToolInfo.rect      4 CELLS +
   DUP CONSTANT lpToolInfo.hinst       CELL+
       CONSTANT lpToolInfo.lpszText



\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ \\\ Combo Box
\
0 VALUE hWndCombo       \ combo box handle
0 VALUE lpfnDefCombo    \ default combo box procedure

\ Add strings to combo box
: AddComboStrings
   Z" Nancy"    rel>abs  -1  CB_INSERTSTRING  hWndCombo  Call SendMessage  DROP
   Z" Dale"     rel>abs  -1  CB_INSERTSTRING  hWndCombo  Call SendMessage  DROP
   Z" Dennis"   rel>abs  -1  CB_INSERTSTRING  hWndCombo  Call SendMessage  DROP
   Z" Herman"   rel>abs  -1  CB_INSERTSTRING  hWndCombo  Call SendMessage  DROP
   Z" Ken"      rel>abs  -1  CB_INSERTSTRING  hWndCombo  Call SendMessage  DROP
   Z" Kyle"     rel>abs  -1  CB_INSERTSTRING  hWndCombo  Call SendMessage  DROP
   Z" Nigel"    rel>abs  -1  CB_INSERTSTRING  hWndCombo  Call SendMessage  DROP
   Z" Renan"    rel>abs  -1  CB_INSERTSTRING  hWndCombo  Call SendMessage  DROP
   Z" Ruediger" rel>abs  -1  CB_INSERTSTRING  hWndCombo  Call SendMessage  DROP ;



CREATE ComboMsg   6 CELLS ALLOT
   ComboMsg
   DUP CONSTANT msg.hwnd     CELL+
   DUP CONSTANT msg.message  CELL+
   DUP CONSTANT msg.wParam   CELL+
   DUP CONSTANT msg.lParam   CELL+
   DUP CONSTANT msg.time     CELL+
       CONSTANT msg.pt

: (ComboWndProc)  { hWnd msg wParam lParam -- result }
   msg WM_MOUSEMOVE   =
   msg WM_LBUTTONDOWN = OR
   msg WM_LBUTTONUP   = OR
   IF
      hWnd    msg.hwnd    !
      msg     msg.message !
      wParam  msg.wParam  !
      lParam  msg.lParam  !
      GetTooltips: tBar  TO hWndTT
      ComboMsg rel>abs 0 TTM_RELAYEVENT hWndTT  Call SendMessage DROP
   THEN
   lParam wParam msg hWnd lpfnDefCombo Call CallWindowProc ;

4 callback ComboWndProc (ComboWndProc)



\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ \\\ About Box Routine
\
COMMENT:
This is not implemented like the sample which uses the DialogBox macro.
Mostly this means that the close box works here but not in the sample.
Also ESC closes this window and not sample.
Also location is different (S/B: 160, 78, 144, 77)
Text is not centered (nonproportional font).

From Toolbar.rc:
   ABOUTBOX DIALOG DISCARDABLE  160, 78, 144, 77
   STYLE DS_MODALFRAME | WS_POPUP | WS_VISIBLE | WS_CAPTION | WS_SYSMENU
   CAPTION "About Toolbar Sample"
   FONT 8, "MS Sans Serif"
   BEGIN
       PUSHBUTTON      "OK",IDOK,54,51,40,14
       LTEXT           "Toolbar Sample version 1.5",-1,28,17,92,8
       LTEXT           "written by Nancy Cluts",-1,36,27,76,8,NOT WS_GROUP
       LTEXT           "Microsoft Developer Network",-1,25,37,99,8,NOT WS_GROUP
   END

COMMENT;



CREATE AboutText
    Z,"             Based on\n"
   +Z,"    Toolbar Sample version 1.5\n"
   +Z," originally written by Nancy Cluts\n"
   +Z,"    Microsoft Developer Network\n"
   +Z," Converted to Win32For by Jeff Kelm"

: DoAboutBox ( hWnd)   \ Create a message box with version info
   >R  MB_OK  Z" About Toolbar Sample" rel>abs
   AboutText rel>abs
   R>  Call MessageBox drop ;



\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ \\\ Main Routine
\
: MenuName    Z" ToolbarMenu"    rel>abs ;
: ClassName   Z" ToolbarWClass"  rel>abs ;
: wTitle      Z" Toolbar Sample" rel>abs ;

0 VALUE hWnd            \ application window handle
0 VALUE hdc             \ window device context handle

CREATE ps 64 ALLOT ( paintstruct )

\ Default "Not Implemented" function for menu items
: MenuFunc
   MB_OK  Z" Command" rel>abs  Z" Not implemented in this sample." rel>abs
   hWnd  Call MessageBox DROP ;



\ Define the window procedure
: (MainWndProc)  { hWnd msg wParam lParam -- result }
   msg CASE
      WM_CREATE OF
      \ create toolbar control
         hWnd Create: tBar
      \ create combo box and add it to the toolbar
         NULL
         appInst
         IDM_COMBO
         GetHandle: tBar
         250 100 0 0
         WS_CHILD WS_BORDER or WS_VISIBLE or
         CBS_HASSTRINGS or CBS_DROPDOWN or
         NULL
         Z" COMBOBOX" rel>abs
         NULL
         Call CreateWindowEx  DUP TO hWndCombo  ?WinError
      \ Add strings to combo box
         AddComboStrings
      \ Set the window procedure for the combo box
         GWL_WNDPROC hWndCombo Call GetWindowLong TO lpfnDefCombo
         ComboWndProc rel>abs GWL_WNDPROC hWndCombo Call SetWindowLong  DROP
      \ Get the handle to the tooltip window
         GetTooltips: tBar  TO hWndTT
      \ Fill in the TOOLINFO structure
         sizeof(lpToolInfo)                     lpToolInfo.cbSize !
         TTF_IDISHWND TTF_CENTERTIP OR          lpToolInfo.uFlags !
         Z" Drop-down list combo box" rel>abs   lpToolInfo.lpszText !
         hWnd                                   lpToolInfo.hwnd !
         hWndCombo                              lpToolInfo.uId !
      \ Set up tooltips for the combo box
         lpToolInfo rel>abs 0 TTM_ADDTOOL hWndTT Call SendMessage ?WinError
      ENDOF

      WM_PAINT OF  ps rel>abs hWnd Call BeginPaint TO hdc
                   S" Hi there!" SWAP rel>abs 100 200 hdc Call TextOut DROP
                   ps rel>abs hWnd Call EndPaint DROP  0  ENDOF

      WM_SIZE OF  Autosize: tBar  0  ENDOF

      WM_COMMAND OF   \ ( hWndToolbar idButton WM_COMMAND hWnd)
         wParam LOWORD CASE
                 IDM_NEW    OF  MenuFunc  ENDOF
                 IDM_OPEN   OF  MenuFunc  ENDOF
                 IDM_SAVE   OF  MenuFunc  ENDOF
                 IDM_CUT    OF  MenuFunc  ENDOF
                 IDM_COPY   OF  MenuFunc  ENDOF
                 IDM_PASTE  OF  MenuFunc  ENDOF
                 IDM_PRINT  OF  MenuFunc  ENDOF
\                 IDM_EXIT   OF  Call PostQuitMessage  ENDOF
                 IDM_EXIT   OF  MenuFunc  ENDOF
                 IDM_ABOUT  OF  hWnd DoAboutBox  ENDOF
            ENDCASE
         0 ENDOF

      WM_LBUTTONDBLCLK OF
                0 0 TB_CUSTOMIZE SendMessage: tBar  DROP  0  ENDOF

      WM_NOTIFY OF   \ ( ^LPNMHDR idButton WM_NOTIFY hWnd)
         lparam 2 CELLS + abs>rel @   \ fetch code from NMHDR structure
         CASE
            TTN_NEEDTEXT      OF  lparam SetTooltipText: tBar  ENDOF
            TBN_QUERYDELETE   OF  TRUE  ENDOF
            TBN_GETBUTTONINFO OF  FALSE  ENDOF
            TBN_QUERYINSERT   OF  TRUE  ENDOF
            TBN_CUSTHELP      OF  MB_OK NULL Z" This help is custom." rel>abs
                                  hWnd Call MessageBox DROP  ENDOF
            TBN_TOOLBARCHANGE OF  Autosize: tBar  ENDOF
                       DEFAULTOF  TRUE  ENDOF
            ENDCASE
         ENDOF

\      WM_DESTROY OF  Call PostQuitMessage  ENDOF

      DEFAULTOF  lParam wParam msg hWnd Call DefWindowProc  ENDOF
   ENDCASE ;

4 callback MainWndProc (MainWndProc)



\ Application Window Class structure
CREATE wcToolbarEx   \ WNDCLASSEX structure
   12 cells ,                             \ cbSize
   NULL ,                                 \ style
   MainWndProc rel>abs ,                  \ lpfnWndProc
   0 ,                                    \ cbClsExtra
   0 ,                                    \ cbWndExtra
   appInst ,                              \ hInstance
   hIcon ,                                \ hIcon
   IDC_IBEAM NULL  call LoadCursor ,      \ hCursor
   WHITE_BRUSH     call GetStockObject ,  \ hbrBackground
   MenuName ,                             \ lpszMenuName
   ClassName ,                            \ lpszClassName
   0 ,                                    \ hIconSm

: CreateMainWindow ( -- hWnd)
   NULL                         \ creation parameters
   appInst                      \ instance handle
   GetHandle: appMenu           \ menu handle
   NULL                         \ parent window
   CW_USEDEFAULT CW_USEDEFAULT  \ window size ( h w)
   CW_USEDEFAULT CW_USEDEFAULT  \ window position ( y x )
   WS_OVERLAPPEDWINDOW          \ window style
   wTitle                       \ window title
   ClassName                    \ class name
   NULL                         \ extended window style
   Call CreateWindowEx ;



\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ \\\ Main Call
\
: CLEANUP
   hWnd Call DestroyWindow ?WinError
   appInst ClassName Call UnregisterClass ?WinError ;

: DEMO  ( -- )
   wcToolbarEx rel>abs Call RegisterClassEx ?WinError
   CreateMainWindow DUP TO hWnd         ?WinError
   SW_SHOWNORMAL hWnd Call ShowWindow   DROP
                 hWnd Call UpdateWindow ?WinError ;


CR .( Type 'DEMO' to run program, 'CLEANUP' to kill demo.)

