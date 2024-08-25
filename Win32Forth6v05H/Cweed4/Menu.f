\ Menu.f  Howerd Oakford  2021 Nov 11  Menu objects

variable StBuf 128 allot

\ create the filename status string
: StatusString ( - $ )
   s"     File : "  StBuf place
   CurrentFilename dup c@ if  count
      else   drop  s" -------"  then   StBuf +place

   StBuf  count
;

\ ---------------------------------------------------------------
\ Menu and push button support
\ ---------------------------------------------------------------

LRButtonBar Demo-LR-Buttons
ENDBAR

VButtonBar Demo-V-buttons

     AddButton  " Select file"         'O' +k_control pushkey ;
        ButtonInfo" Select the file  "

     AddButton  " Refresh " 'S' pushkey ;
        ButtonInfo"  Display information about the file"

     AddButton  " Show file +"             'I' pushkey ;
        ButtonInfo" Show file with comments "

     AddButton  " Show file -"             'J' pushkey ;
        ButtonInfo" Show file without comments "

     AddButton  " Show indents"         'V' pushkey ;
        ButtonInfo" Show and check indentation "

     AddButton  " Show #     "         'N' pushkey ;
        ButtonInfo" Display numbers of each character "

     AddButton  " Show Folder" 'R' pushkey ;
        ButtonInfo"  Scan the current folder and create a report file"

     AddButton  " Replace tabs 4" '4' pushkey ;
        ButtonInfo" Replace 4 character Tabs by spaces"

     AddButton  " Barr Rules 3.1 " k_F2 pushkey ;
        ButtonInfo" Apply Barr Coding Standard Rules 3.1 Spaces a, g, h, i and j"

     AddButton  " -Trailing "  'T' pushkey ;
        ButtonInfo" Remove trailing spaces"

     AddButton  " -2 Blank Lines "  'B' pushkey ;
        ButtonInfo" Remove double blank lines"

     AddButton  " // to /* */"  'Q' pushkey ;
        ButtonInfo" Convert C++ comments to C"

     AddButton  " Weed "         'W' pushkey ;
        ButtonInfo" Remove tabs, trailing spaces and double blank lines"

     AddButton  " Fix indents "         'X' pushkey ;
        ButtonInfo" Fix indentation " 

     AddButton  " PC->Unix" 'U' pushkey ;
        ButtonInfo"  Remove all Carriage Return characters"

     AddButton  " Unix->PC" 'P' pushkey ;
        ButtonInfo"  Replace LF by CRLF"

     AddButton  " Restore "         'M' pushkey ;
        ButtonInfo" Restore the original file "

\     AddButton  " Help "      k_F1 pushkey ;
\        ButtonInfo"  Display the Help Screen "

ENDBAR

100 SetButtonWidth: Demo-V-buttons      \ adjust the button width

ToolBar Demo-Tool-Bar "Cweed4.BMP"
        03 HSpace
      1 PictureButton                    'O' +k_control pushkey  ; \ open
        ButtonInfo"  Select file "

\     3 PictureButton                    'P' +k_control pushkey  ;
\        ButtonInfo"  Print Plot "
\     3 PictureButton                    'Q' +k_control pushkey  ;
\        ButtonInfo"  Print BitMap "
\        8 HSpace
\     6 PictureButton                    'X' +k_control pushkey  ; \ cut
\        ButtonInfo"  Cut Window "
\     7 PictureButton                    'C' +k_control pushkey  ; \ copy
\        ButtonInfo"  Copy Window Normal "
\     8 PictureButton                    'V' +k_control pushkey  ; \ paste
\        ButtonInfo"  Paste "
\        8 HSpace
    11 PictureButton                    'W' pushkey            ; \ weed
        ButtonInfo" Weed "
    12 PictureButton                    'M' pushkey            ; \ undo
        ButtonInfo"  Restore the original file  "
        8 HSpace
     5 PictureButton                    k_F1 pushkey            ; \ help
        ButtonInfo"  Help "
        8 HSpace
ENDBAR

POPUPBAR Demo-Popup-bar

    POPUP " "
        MENUITEM        "Open File..."  'O' +k_control pushkey  ;
        MENUSEPARATOR
        MENUITEM        "Print File..." 'P' +k_control pushkey  ;
        MENUSEPARATOR
        MENUITEM        "Exit"          bye                     ;
ENDBAR

MENUBAR Demo-Menu-bar

\  SYSPOPUP
\     MENUSEPARATOR
\     SUBMENU   "Debugging"
\         MENUITEM  "Print Forth Window"        print-screen ;
\         MENUSEPARATOR
\         MENUITEM  "Exit to Forth"             console abort ;
\         ENDSUBMENU
  
((
    POPUP "&File"
        MENUITEM     "&Open File...  \tCtrl+O"                 'O' +k_control pushkey ;
         SUBMENU      "&Save Bitmap As"
           MENUITEM     "    1 Bits / 2 Colors\t1"              '1' pushkey ;
           MENUITEM     "    4 Bits / 16 Colors\t2"             '2' pushkey ;
           MENUITEM     "    8 Bits / 256 Colors\t3"            '3' pushkey ;
           MENUITEM     "   16 Bits / 65536 Colors\tCtrl+S"     'S' +k_control pushkey ;
           MENUITEM     "   24 Bits / 16 Million Colors\t5"     '5' pushkey ;
           MENUITEM     "   32 Bits / 4 Billion Colors\t6"      '6' pushkey ;
         ENDSUBMENU
        MENUSEPARATOR
        MENUITEM     "Page Setup..."                    'P' +k_control +k_shift pushkey ;
        MENUITEM     "&Print Plot... \tCtrl+P"          'P' +k_control pushkey ;
        MENUITEM     "&Print BMP..."                    'Q' +k_control pushkey ;
        MENUSEPARATOR
        MENUITEM     "E&xit          \tAlt-F4"  bye ;

    POPUP "&Edit"
false   MENUMESSAGE     "Cut"
        SUBMENU         "&Copy"
             MENUITEM   "Plot Window &Normal"   'C' +k_control pushkey ;
             MENUITEM   "Plot Window &Inverted" 'C' +k_control +k_shift pushkey ;
             ENDSUBMENU
        MENUITEM        "Paste"                 'V' +k_control pushkey ;

    POPUP "&Display"
        MENUITEM        "Run Demo     \tEnter"  k_cr  pushkey ;
        MENUITEM        "Run Line Walk"         k_cr  +k_control pushkey ;
        MENUITEM        "Erase Window \t  Esc"  k_esc pushkey ;
        MENUITEM        "Stop Plotting"         k_esc +k_control pushkey ;
))

    POPUP "&Help"
        MENUITEM        "Cweed PDF"          k_F4 pushkey ;
        MENUSEPARATOR
        MENUITEM        "Cweed Help"         k_F1 pushkey ;
        MENUSEPARATOR
        MENUITEM        "About Cweed"        k_F1 +k_control pushkey ;
ENDBAR

0 value myHandle

16 2** constant |WINTEXT_BUFFER|
variable WinTextBuffer |WINTEXT_BUFFER| cell - allot
variable TextWindowYorg
8 constant TEXT_WINDOW_X_ORG
5 constant TEXT_WINDOW_Y_ORG

variable needanupdate


: mup   1 needanupdate !
   myHandle if   0 0 myHandle Call InvalidateRect ?win-error   then
;

: memit ( c )   l# @ 256 * c# @ + |WINTEXT_BUFFER| 1- and WinTextBuffer + c!  1 c# +! mup ;

: mGotoXY ( x y )   l# !  c# ! ;

: mtype ( a n )  over + swap ?do  i c@ memit  loop ; \ On_paint: self ;

: mcr   1 l# +!  l# @  0 max  ScreenHeight 1- min  l# !
   0 c# !  88 0 do  32 memit  loop  0 c# !  \ clear the line
;

: mcls    0 c# !  0 l# !   WinTextBuffer |WINTEXT_BUFFER| 32 fill  mup ;

: mconsole
    ['] memit      IS EMIT
    ['] mtype      IS TYPE
    ['] mcr        IS CR
    ['] mcls       IS CLS
    ['] mGotoXY    IS GOTOXY
    ['] ekey is key
    ['] _accept is accept
;

: mmm    mconsole  cls s" mmm test this!" type ; \ ~mconsole ;

: RestoreConsole
    ['] _emit      IS EMIT
    ['] _type      IS TYPE
    ['] _cr        IS CR
    ['] _cls       IS CLS
    ['] _GOTOXY    IS GOTOXY
;

: NullConsole             \ do nothing
    ['] drop       IS EMIT
    ['] 2drop      IS TYPE
    ['] noop       IS CR
    ['] noop       IS CLS
    ['] 2drop      IS GOTOXY
    ['] ekey is key
    ['] _accept is accept
;

:Object mainWindow  <super window

   Font mainFont \ font data structure

\   bit-window  vga-bit-window
\   fill-window button-fill-window
\   Rectangle   DemoRect
   Statusbar   StatBar

   2 constant bitorigx
   2 constant bitorigy

   MAXSTRING bytes StatusBuf      \ place to collect the status info

   variable TextWindowYorg



:M Classinit:   ( -- )          \ static initialization goes here, IF needed
                ClassInit: super
                ;M

:M On_Init:     ( -- )          \ initialize the class

        init_buffers                   \ allocate the buffers

        On_Init: super                 \ first init super class

        GetHandle: self to myHandle

        mconsole

\        1    SetId: button-fill-window  \ then the child window
\        self Start: button-fill-window  \ then startup child window
\        2    SetId: vga-bit-window      \ then the child window
\       self Start: vga-bit-window      \ then startup child window

        3    SetId: Demo-V-Buttons      \ then the next child window
        self Start: Demo-V-Buttons      \ then startup child window

        4    SetId: Demo-Tool-Bar       \ then the next child window
        self Start: Demo-Tool-Bar       \ then startup child window

        5    SetId: Demo-LR-Buttons     \ then the next child window
        self Start: Demo-LR-Buttons     \ then startup child window

        Demo-menu-bar  SetMenuBar:  self

\        Demo-popup-Bar SetPopupBar: vga-bit-window

        self Start: StatBar             \ the status bar

        StartSize: Demo-Tool-Bar nip 1+ TextWindowYorg !

\    s"  Arial" SetFaceName: mainFont
    s"       Courier New" SetFaceName: mainFont
    char-width-main  Width: mainFont
    char-height-main Height: mainFont

    Create: mainFont

    HostVersion  SetTitle: self

;M

:M On_Done:     ( h m w l -- res )
        0 call PostQuitMessage drop
        On_Done: super
0
;M

:M WM_CLOSE     ( h m w l -- res )
   de_init_buffers    \ remove the buffers
                WM_CLOSE WM: Super
\                bye
                0 ;M

:M Refresh:     ( -- )
\                Paint: StatBar
\                Paint: vga-bit-window
                ;M


:M StartSize:   ( -- width height )     \ starting window size
   bitorigx 2* ScreenWidth char-width-main * +
   StartSize: Demo-V-Buttons drop 2 - + 10 +
   TEXT_WINDOW_X_ORG +

   bitorigy 2*
   ScreenHeight char-height-main *       +
   StartSize: Demo-Tool-Bar nip 1+       +
   StartSize: StatBar nip 1+             +
   StartSize: Demo-LR-Buttons nip 1+     +
   ( Demo-menu-bar )  48 +
   TEXT_WINDOW_Y_ORG +

;M


((
:M MinSize:     ( -- width height )     \ minimum window size
\    startsize: self
;M

:M MaxSize:     ( -- width height )     \ maximum window size
\    startsize: self
;M
))

:M WindowTitle: ( -- Zstring )          \ window caption
                z" Cweed : C source and header scanner"
                ;M

:M WindowStyle: ( -- style )            \ return the window style
                WS_OVERLAPPED WS_CAPTION + WS_SYSMENU + WS_MINIMIZEBOX +
                ;M


:M SetStatus:   ( a1 n1 a2 n2 -- )
                StatusBuf place
                StatusBuf +place
                StatusBuf count SetText: StatBar        \ set the current text on status
                Paint: StatBar
                ;M

:M On_Paint:    ( -- )

    s" " StatusString SetStatus: self

    Handle: mainFont SetFont: dc      \ set the font to be used

                On_Paint: super

\                WHITE                                LineColor: dc \ white color
\                0 0                                     MoveTo: dc \ horiz
\                StartSize: self drop width max 0        LineTo: dc \ line
\                0 0                                     MoveTo: dc \ vertical
\                StartSize: Demo-Tool-Bar nip 0 swap     LineTo: dc \ line
\                StartSize: Demo-Tool-Bar swap 1+ swap   MoveTo: dc \ vertical
\                StartSize: Demo-Tool-Bar drop 1+ 0      LineTo: dc \ line

                BLACK                                LineColor: dc

                0 StartSize: Demo-Tool-Bar nip dup>r    MoveTo: dc
                  StartSize: self drop width max r>     LineTo: dc

                  StartSize: Demo-Tool-Bar over 0       MoveTo: dc
                                                        LineTo: dc

\                s"   "  s" Main status???"  SetStatus: self
\                0 0 0 0 Move: StatBar 	\ move to its default position

   On_Paint: StatBar

\   needanupdate @ if  needanupdate @ 1 - 0 max  needanupdate !
      \ Draw the Text Buffer
      ScreenHeight 0 do
         TEXT_WINDOW_X_ORG i char-height-main * TextWindowYorg @ + TEXT_WINDOW_Y_ORG +
         WinTextBuffer i 256 * + ScreenWidth
         TextOut: dc
      loop
\   then


;M

\ the l parameter has already been removed by WINDOW.F, and put
\ into Height and Width

:M On_Size:     ( h m w -- )                  \ handle resize message

   Width  StartSize: Demo-V-Buttons >r  -  2 +  r>
   Height StartSize: Demo-LR-Buttons bitorigy  2 -  0 max  +

   swap 4 - >r dup >r - over -

   r> 4 >          \ if there are buttons in the bar
   IF
      2 -     \ then leave two more pixels of room#
   ELSE       \ else we'll already have two pixels of room
      r>drop StartSize: Demo-V-Buttons drop 4 - 0max >r
   THEN
   r> swap

   StartSize: Demo-Tool-Bar nip 1+ ( height ) >r
   2swap r@ + 2swap r> -

\  Move: button-fill-window

   bitorigx

   bitorigy StartSize: Demo-Tool-Bar nip ( height) + 1+
   Width  StartSize: Demo-V-Buttons drop - 2 -
   dup to screen-width

   Height 4 - StartSize: Demo-Tool-Bar nip - 1-

   StartSize: StatBar nip 1+ - \ allow room for the status bar
   dup to screen-height

\                Move: vga-bit-window

                  Width  StartSize: Demo-V-Buttons  drop - bitorigy 2 - 0max
                         StartSize: Demo-Tool-Bar   nip + 1+
                         StartSize: Demo-V-Buttons  Move: Demo-V-Buttons
                  Width  StartSize: Demo-LR-Buttons drop -
                  Height StartSize: Demo-LR-Buttons nip  - bitorigy 2 - 0max -
                         StartSize: Demo-LR-Buttons Move: Demo-LR-Buttons
                0 0      StartSize: Demo-Tool-Bar   Move: Demo-Tool-Bar

                0 0 0 0 Move: StatBar 	\ move to its default position
;M


((
:M ExWindowStyle: ( -- extended_style )
                ExWindowStyle: super
                WS_EX_TOPMOST or        \ if so, lock on top
;M
))

:m on_move: ( w -- )
\   hWnd  if   0 0 hWnd Call InvalidateRect ?win-error   then
\   On_Paint: self
;m


((
:M On_Size:     ( h m w -- )
                Width  StartSize: Demo-V-Buttons >r - 2 + r>
                Height StartSize: Demo-LR-Buttons bitorigy 2 - 0max +
                swap 4 - >r dup>r - over -
                r> 4 >          \ if there are buttons in the bar
                IF      2 -     \ then leave two more pixels of room
                                \ else we'll already have two pixels of room
                ELSE    r>drop StartSize: Demo-V-Buttons drop 4 - 0max >r
                THEN    r> swap
                        StartSize: Demo-Tool-Bar nip 1+ >r
                        2swap r@ + 2swap r> -
                        Move: button-fill-window

   0 0 0 0 Move: StatBar 	\ move to its default position
   2drop drop
   StartSize: self
   On_Paint: self
;M
))

((
:M SetVButtonBar: { buttonbar -- }
                buttonbar Demo-V-Buttons <>
                IF      Close: Demo-V-Buttons
                        buttonbar to Demo-V-Buttons
                        self  Start: Demo-V-Buttons
                        On_Size: self
                then
                ;M

:M SetLRButtonBar: { buttonbar -- }
                buttonbar Demo-LR-Buttons <>
                IF      Close: Demo-LR-Buttons
                        buttonbar to Demo-LR-Buttons
                        self  Start: Demo-LR-Buttons
                        On_Size: self
                then
                ;M

))

((
\ Mouse support connections from the applications window to the bitmapped
\ window that will actually receive the mouse clicks

:M SetClickFunc: ( cfa -- )
                SetClickFunc: vga-bit-window
                ;M

:M SetUnClickFunc: ( cfa -- )
                SetUnClickFunc: vga-bit-window
                ;M

:M SetDblClickFunc: ( cfa -- )
                SetDblClickFunc: vga-bit-window
                ;M

:M SetTrackFunc: ( cfa -- )
                SetTrackFunc: vga-bit-window
                ;M

))
\ All SC_xxxx command types always have the high nibble set to 0xF

:M WM_SYSCOMMAND ( hwnd msg wparam lparam -- res )
                over 0xF000 and 0xF000 <>
                IF      over LOWORD
                        DoMenu: CurrentMenu
                        0
                ELSE    DefWindowProc: [ self ]
                THEN    ;M

;Object


: uninit-demo   ( -- )
                DestroyWindow: mainWindow ;

unload-chain chain-add-before uninit-demo

\ ---------------------------------------------------------------
\ Demo about dialog, copied from the Forth About Dialog
\ ---------------------------------------------------------------

create about-demo-msg
         z," Cweed C source and header scanner " HostVersion  +z",
        +z," \nInventio Software  www.inventio.co.uk \n"
        get-local-time time-buf >month,day,year" +z",
        +z," \n"
        -null, here 0 c, align about-demo-msg - constant about-demo-len

:Object AboutCweed  <SUPER dialog

IDD_ABOUT_FORTH forthdlg find-dialog-id constant template

:M On_Init:     ( hWnd-focus -- f )
                about-demo-msg about-demo-len IDD_ABOUT_TEXT  SetDlgItemText: self
                1 ;M

:M Start:       ( -- f )
        Addr: mainWindow template run-dialog
;M

:M On_Command:  ( hCtrl code ID -- )
                case
                IDCANCEL of     end-dialog    endof
                                false swap ( default result )
                endcase ;M

;Object

: about-demo ( -- )
                start: AboutCweed ;

: "demo-message ( a1 n1 -- )
                message-off
                dup
                IF      StartPos: mainWindow StartPos: msg-window
                        rot + >r + r> message-origin
                        MessageText: msg-window
                              Start: msg-window
                ELSE    2drop
                THEN    ;

: demo-message-off ( -- )
                message-off
                StartPos: mainWindow StartPos: msg-window
                rot - 0max >r swap - 0max r> message-origin ;

\ copy VGA-DC bitmap, f1=true=inverted

: copy-demo-bitmap   { flag \ hbm hdcMem -- }
                GetHandle: mainWindow call OpenClipboard 0=
        IF      s" Can't Open Clipboard\n\n...press a key to continue"
                "demo-message
                key drop demo-message-off
                EXIT
        THEN    flag
                SCREEN-HEIGHT
                SCREEN-WIDTH
                GetHandle: demo-dc
                call CreateCompatibleBitmap to hbm

                GetHandle: demo-dc
                call CreateCompatibleDC to hdcMem

                hbm hdcMem call SelectObject drop
                r>
                IF      NOTSRCCOPY
                ELSE    SRCCOPY
                THEN
                0 0                                     \ y,x origin
                GetHandle: demo-dc                      \ from the screen
                SCREEN-HEIGHT                           \ source height
                SCREEN-WIDTH                            \ source width
                0 0 hdcMem                              \ to new bitmap
                call BitBlt ?win-error                  \ invert the bitmap

                call EmptyClipboard ?win-error  \ clear out the clipboard
                hbm CF_BITMAP call SetClipboardData ?win-error
                call CloseClipboard ?win-error

                hdcMem call DeleteDC ?win-error
\ We don't delete the bitmap because it is now owned by the clipboard !!
\               hbm call DeleteObject ?win-error
                ;

: paste-demo-bitmap   { flag \ hbm hdcMem -- }
                GetHandle: mainWindow call OpenClipboard 0=
        IF      s" Can't Open Clipboard\n\n...press a key to continue"
                "demo-message
                2000 ms demo-message-off
                EXIT
        then

                SCREEN-WIDTH
                SCREEN-HEIGHT
                CreateCompatibleBitMap: demo-dc to hbm

                GetHandle: demo-dc

                call CreateCompatibleDC to hdcMem

                CF_BITMAP call GetClipboardData dup to hbm ?win-error

                hbm hdcMem call SelectObject drop

                flag

                IF     NOTSRCCOPY
                ELSE   SRCCOPY
                THEN
                0 0                       \ y,x origin
                hdcMem                    \ from memory dc
                SCREEN-HEIGHT             \ source height
                SCREEN-WIDTH              \ source width
                0 0                       \ y,x dest
                GetHandle: demo-dc        \ to screen

                call BitBlt ?win-error          \ invert the bitmap

                call CloseClipboard ?win-error

                hdcMem call DeleteDC ?win-error
\               hbm call DeleteObject ?win-error
                ;


FileOpenDialog ViewBitmap "Open Bitmap File" "Bitmap Files (*.BMP)|*.BMP|*.DIB|All Files (*.*)|*.*|"
FileSaveDialog SaveBitmap "Save Bitmap File" "Bitmap Files (*.BMP)|*.BMP|*.DIB|All Files (*.*)|*.*|"

\ ---------------------------------------------------------------
\ Open image file support
\ ---------------------------------------------------------------

: open-demo-bitmap     { \ open$ hbm hdcMem --  }
                max-path LocalAlloc: open$
                GetHandle: mainWindow Start: ViewBitmap dup c@ \ -- a1 n1
                IF      count open$ place
                        LR_LOADFROMFILE
                        LR_CREATEDIBSECTION or
                        NULL
                        NULL
                        IMAGE_BITMAP
                        open$ dup +NULL 1+ rel>abs
                        NULL
                        Call LoadImage to hbm
                        GetHandle: demo-dc
                        Call CreateCompatibleDC to hdcMem
                        hbm hdcMem Call SelectObject  drop

                        SRCCOPY                                   \
                        0 0                                       \ y,x origin
                        hdcMem                                    \ from memory dc
                        SCREEN-HEIGHT                             \ height of dest rect
                        SCREEN-WIDTH                              \ width of dest rect
                        0 0                                       \ y,x dest
                        GetHandle: demo-dc                        \ to screen

                        Call BitBlt ?win-error                    \

                        hdcMem Call DeleteDC ?win-error

                ELSE    DROP
                THEN
                 ;

\ ---------------------------------------------------------------
\ Save File support
\ ---------------------------------------------------------------

        4 constant sizeof(RGBQUAD)
       14 constant sizeof(BitmapFileHeader)
       40 constant sizeof(BitmapInfoHeader)

        0 constant biSize
        4 constant biWidth
        8 constant biHeight
       12 constant biPlanes
       14 constant biBitCount
       16 constant biCompression
       20 constant biSizeImage
       24 constant biXPelsPerMeter
       28 constant biYPelsPerMeter
       32 constant biClrUsed
       36 constant biClrImportant

: show-BITMAPINFOHEADER { pbmih \ bmih$ -- }
        max-path localalloc: bmih$
        s" BITMAPINFOHEADER"                   bmih$  place
        s" \nbiSize : "                        bmih$ +place
       pbmih biSize + @           0 <# #s #>   bmih$ +place
        s" \nbiWidth : "                       bmih$ +place
       pbmih biWidth + @          0 <# #s #>   bmih$ +place
        s" \nbiHeight : "                      bmih$ +place
       pbmih biHeight + @         0 <# #s #>   bmih$ +place
        s" \nbiPlanes : "                      bmih$ +place
       pbmih biPlanes + w@        0 <# #s #>   bmih$ +place
        s" \nbiBitCount : "                    bmih$ +place
       pbmih biBitcount + w@      0 <# #s #>   bmih$ +place
        s" \nbiCompression : "                 bmih$ +place
       pbmih biCompression + @    0 <# #s #>   bmih$ +place
        s" \nbiSizeImage : "                   bmih$ +place
       pbmih biSizeImage + @      0 <# #s #>   bmih$ +place
        s" \nbiXPelsPerMeter : "               bmih$ +place
       pbmih biXPelsPerMeter + @  0 <# #s #>   bmih$ +place
        s" \nbiYPelsPerMeter : "               bmih$ +place
       pbmih biYPelsPerMeter + @  0 <# #s #>   bmih$ +place
        s" \nbiClrUsed : "                     bmih$ +place
       pbmih biClrUsed + @        0 <# #s #>   bmih$ +place
        s" \nbiClrImportant :"                 bmih$ +place
       pbmih biClrImportant + @   0 <# #s #>   bmih$ +place
       bmih$ count "message key drop message-off
        ;


: save-demo-bitmap { nBits \  pbmi lpBits hbm  hdcMem hfile nrgbquad BitmapFileHeader save$  -- }
        14 LocalAlloc: BitmapFileHeader
        max-path    LocalAlloc: save$
        s" Save Bitmap File: "  save$ place
        nBits (.)               save$ +place
        s"  Bit"                save$ +place
        save$ count SetTitle: SaveBitmap
        GetHandle: mainWindow Start: SaveBitmap dup c@

     IF count save$ place

        sizeof(BitmapInfoHeader)  sizeof(RGBQUAD) 256 * + malloc to pbmi
        pbmi sizeof(BitmapInfoHeader) sizeof(RGBQUAD) 256 * + erase   \ (1) DON'T DELETE THIS LINE
                                                                      \

        sizeof(BitmapInfoHeader)                   pbmi biSize            +   !
        SCREEN-WIDTH                               pbmi biWidth           +   !
        SCREEN-HEIGHT                              pbmi biHeight          +   !
        1                                          pbmi biPlanes          +  w!
        nBits                                      pbmi biBitCount        +  w!

        nBits
         CASE
          1 OF BI_RGB    2 to nrgbquad    ENDOF
          4 OF BI_RLE4  16 to nrgbquad    ENDOF \ Could also be BI_RGB for
          8 OF BI_RLE8 256 to nrgbquad    ENDOF \ uncompressed format
         16 OF BI_RGB    0 to nrgbquad    ENDOF
         24 OF BI_RGB    0 to nrgbquad    ENDOF
         32 OF BI_RGB    0 to nrgbquad    ENDOF
         ENDCASE                                   pbmi biCompression     +   !

      \  0    pbmi biSizeImage       +   !       NOT NEEDED           (1)
      \  0    pbmi biXPelsPerMeter   +   !       SINCE
      \  0    pbmi biYPelsPerMeter   +   !       pbmi IS ERASED
      \  0    pbmi biClrUsed         +   !       ABOVE
      \  0    pbmi biClrImportant    +   !


        SCREEN-HEIGHT
        SCREEN-WIDTH
        GetHandle: demo-dc
        Call CreateCompatibleBitmap to hbm


        GetHandle: demo-dc
        Call CreateCompatibleDC to hdcMem
        hbm hdcMem Call SelectObject drop

        SRCCOPY                                   \
        0 0                                       \ y,x origin
        GetHandle: demo-dc                        \ from screen dc
        SCREEN-HEIGHT                             \ height of dest rect
        SCREEN-WIDTH                              \ width of dest rect
        0 0                                       \ y,x dest
        hdcMem                                    \ to memory dc
        Call BitBlt ?win-error                    \


        DIB_RGB_COLORS
        pbmi rel>abs
        NULL
        SCREEN-HEIGHT
        0
        hbm
        hdcMem
        Call GetDIBits 0= abort" 1st GetDIBits"


\        pbmi show-bitmapinfoheader

        pbmi biSizeImage + @ malloc rel>abs to lpBits
        lpBits abs>rel pbmi biSizeImage + @ erase


        DIB_RGB_COLORS
        pbmi rel>abs
        lpBits
        SCREEN-HEIGHT
        0
        hbm
        hdcMem
        Call GetDIBits 0= abort" 2nd GetDIBits"

\        pbmi show-bitmapinfoheader

        save$
        count
        GENERIC_READ GENERIC_WRITE or
        create-file abort" CreateFile"
        to hfile

        0x4d42 BitmapFileHeader     w!                        \ hdr.bfType

        sizeof(BitmapFileHeader)
        sizeof(BitmapInfoHeader) +
        nrgbquad sizeof(RGBQUAD) * +
        pbmi biSizeImage + @     +
               BitmapFileHeader 2 +  !                        \ hdr.bfSize

        0      BitmapFileHeader 6 + w!                        \ hdr.bfReserved1
        0      BitmapFileHeader 8 + w!                        \ hdr.bfReserved2

        sizeof(BitmapFileHeader)
        sizeof(BitmapInfoHeader) +
        nrgbquad sizeof(RGBQUAD) * +
               BitmapFileHeader 10 + !                        \ hdr.bfOffBits

        BitmapFileHeader
        sizeof(BitmapFileHeader)
        hfile write-file  drop


        pbmi
        sizeof(BitmapInfoHeader)
        nrgbquad sizeof(RGBQUAD) * +
        hfile write-file drop

        lpBits abs>rel
        pbmi biSizeImage + @
        hfile write-file drop

        hfile close-file drop

        hdcMem call DeleteDC ?win-error
        hbm call DeleteObject ?win-error


        lpBits abs>rel release
        pbmi release
     ELSE drop
     THEN
        ;
defer save-bitmap

' save-demo-bitmap is save-bitmap

only forth also definitions

\ ---------------------------------------------------------------
\               Printing support
\ ---------------------------------------------------------------

: print-demo    ( -- )
                TRUE to do-printing?
                seed1-save to seed1     \ print same as displayed
                seed2-save to seed2
                seed3-save to seed3
                200 TO line_max
                400 TO lines_max
                0 TO line_limit
                0 TO cnt-down
                3 TO cnt-down-max
                single-page
                start-scaled
\                IF      erase-demo
\                        S" Printing DEMO..." "demo-message
\                        walking?
\                        IF      line-walk
\                        ELSE    run-demo
\                        THEN
\                        print-scaled
\                        demo-message-off
\                THEN
                FALSE to do-printing? ;

: print-demo-bmp { nBits \  pbmi lpBits hbm  hdcMem    -- }
             Open: ThePrinter
        GetHandle: ThePrinter 0= ?EXIT
            Start: ThePrinter

             sizeof(BitmapInfoHeader) sizeof(RGBQUAD) 256 * + malloc to pbmi
        pbmi sizeof(BitmapInfoHeader) sizeof(RGBQUAD) 256 * + erase   \ (1) DON'T DELETE THIS LINE
                                                                      \

        sizeof(BitmapInfoHeader)                   pbmi biSize            + !
        SCREEN-WIDTH                               pbmi biWidth           + !
        SCREEN-HEIGHT                              pbmi biHeight          + !
        1                                          pbmi biPlanes          + w!
        nBits                                      pbmi biBitCount        + w!

        BI_RGB                                     pbmi biCompression     + !

      \  0    pbmi biSizeImage       +   !       NOT NEEDED           (1)
      \  0    pbmi biXPelsPerMeter   +   !       SINCE
      \  0    pbmi biYPelsPerMeter   +   !       pbmi IS ERASED
      \  0    pbmi biClrUsed         +   !       ABOVE
      \  0    pbmi biClrImportant    +   !


        SCREEN-HEIGHT
        SCREEN-WIDTH
        GetHandle: demo-dc
        Call CreateCompatibleBitmap to hbm


        GetHandle: demo-dc
        Call CreateCompatibleDC to hdcMem
        hbm hdcMem Call SelectObject drop

        SRCCOPY                                   \
        0 0                                       \ y,x origin
        GetHandle: demo-dc                        \ from screen dc
        SCREEN-HEIGHT                             \ height of dest rect
        SCREEN-WIDTH                              \ width of dest rect
        0 0                                       \ y,x dest
        hdcMem                                    \ to memory dc
        Call BitBlt ?win-error                    \


        DIB_RGB_COLORS
        pbmi rel>abs
        NULL
        SCREEN-HEIGHT
        0
        hbm
        hdcMem
        Call GetDIBits 0= abort" 1st GetDIBits"


       \ pbmi show-bitmapinfoheader

        pbmi biSizeImage + @ malloc rel>abs to lpBits
        lpBits abs>rel pbmi biSizeImage + @ erase


        DIB_RGB_COLORS
        pbmi rel>abs
        lpBits
        SCREEN-HEIGHT
        0
        hbm
        hdcMem
        Call GetDIBits 0= abort" 2nd GetDIBits"

      \  pbmi show-bitmapinfoheader


        SRCCOPY
        DIB_RGB_COLORS
        pbmi rel>abs
        lpBits

        SCREEN-HEIGHT
        SCREEN-WIDTH
        0
        0
        Height: ThePrinter
        Width: ThePrinter
        0
        0
        GetHandle: ThePrinter
        Call StretchDIBits GDI_ERROR = ABORT" StretchDIBits"
          End: ThePrinter
        Close: ThePrinter
        hdcMem call DeleteDC ?win-error
        hbm call DeleteObject ?win-error


        lpBits abs>rel release
        pbmi release
        ;


((
\ *********************************  Window ********************************

:object NotUsedWindow <super window

Font mainFont        \ font data structure

: Underline     ( -- )
                               Delete: mainFont
                       TRUE Underline: mainFont
                               Create: mainFont
                Handle: mainFont SetFont: dc ;      \ set the font to be used

: /Underline    ( -- )
                               Delete: mainFont
                      FALSE Underline: mainFont
                               Create: mainFont
                Handle: mainFont SetFont: dc ;      \ set the font to be used


int screen-cols
int screen-rows
int SlowCounter         \ increments when the window is updated

int cur-first-line \ current first line position
0 value first-line#    \ first line number
200 value last-line#   \ last line number
last-line# 20 - value last-top-line#
16 value bytes/line

8 constant charX \ character width in pixels
16 constant charY \ character height in pixels

: H.N.str  ( n1 n2 -- adr len )    \ display n1 as a hex number of n2 digits
         BASE @ >R HEX >R
         0 <# R> 0 ?DO # LOOP #>
         R> BASE ! ;

:m home: ( -- )
        first-line# to cur-first-line paint: self ;m

: xSetupFont
\   InitDC

   charX 3 * Width: mainFont
   charY Height: mainFont
\   s"  Arial" SetFaceName: mainFont
   s"       Courier" SetFaceName: mainFont

   Create: mainFont

   Handle: mainFont SetFont: dc   \ set the font to be used
;

:M On_Init:     ( -- )          \ things to do at the start of window creation
   On_Init: super             \ do anything superclass needs

   charX 2* Width: mainFont
   charY Height: mainFont
   s" Courier" SetFaceName: mainFont
   Create: mainFont
   Handle: mainFont SetFont: dc   \ set the font to be used

   CurrentFilename count dup if  settitle: self  else  2drop  then

   GetFile    \ scan file for numbers of characters etc

\   0 to SlowCounter               \ initialize counter to zero
\   0 200 1 hWnd Call SetTimer drop \ init timer to a 200 ms rate

;M

:m on_size: ( w -- )
        width char-width-main / to screen-cols
        height char-height  / to screen-rows
        last-line# screen-rows - 0max to last-top-line#
        \ set the vertical scroll limits
\        false last-top-line# first-line# SB_VERT
\        GetHandle: self Call SetScrollRange drop
   On_Paint: self
;m

:m on_move: ( w -- )
   On_Paint: self
;m

:m startpos: ( StartPos: parent 20 20 v+ ) 20 100 ;m

:m startsize: 64 char-width-main *  30 char-height * ;m

\ variable Painting  0 Painting !

:m on_paint: ( -- )
\   SaveDC: dc                   \ save device context

\   20 70 SlowCounter (.) textout: dc

\   RestoreDC: dc
   on_paint: super
\   1 +to SlowCounter        \ increment the counter
\   SlowCounter @ (.)  settitle: self
;m

:M WM_TIMER    ( h m w l - res )
   1 +to SlowCounter        \ increment the counter
   Paint: self
   0
;m

:m on_display: ( -- )
   SaveDC: dc                  \ save device context
   SetupFont
\   DisplayCurves
   RestoreDC: dc
;m

:M WM_LBUTTONDOWN     ( -- res )

      Beep
      Paint: self
   0
;M

:M WM_RBUTTONDOWN     ( -- res )

      Beep
      Paint: self
   0
;M

:M SetClickFunc: ( cfa -- )
  SetClickFunc: self
;M

:M On_Done:    ( h m w l -- res )

   1 hWnd Call KillTimer  drop        \ destroy the timer
   Delete: mainFont            \ remove the font
   On_Done: Super    \ the class's close function
   DestroyWindow: self
\   bye             \ quit everything
   0 ;M

:M WindowStyle: ( -- style )                    \ return the window style
                WindowStyle: super
\                WS_VSCROLL -1 xor and          \ don't add vertical scroll bar
\                WS_THICKFRAME -1 xor and       \ don't have a border
        WS_BORDER  or
                ;M

((
:M ExWindowStyle: ( -- extended_style )
                ExWindowStyle: super
                WS_EX_TOPMOST or        \ if so, lock on top
;M
))

((
:m vposition: ( n -- ) \ move to position n
        0max last-top-line# min
        to cur-first-line paint: self ;m

:m vscroll: ( n -- ) \ move n lines up or down
        cur-first-line + vposition: self ;m

:m end: ( -- ) \ move to end, in this case it's 100 bytes down to pad
        last-top-line# to cur-first-line paint: self ;m

:m vpage: ( n -- )  \ down or up n pages
        screen-rows 1- * vscroll: self ;m

:M WM_VSCROLL   ( h m w l -- res )
                swap word-split >r
        CASE
                SB_BOTTOM        of          End: self   endof
                SB_TOP           of         Home: self   endof
                SB_LINEDOWN      of    1 VScroll: self   endof
                SB_LINEUP        of   -1 VScroll: self   endof
                SB_PAGEDOWN      of    1   VPage: self   endof
                SB_PAGEUP        of   -1   VPage: self   endof
                SB_THUMBPOSITION of r@ VPosition: self   endof
                SB_THUMBTRACK    of r@ VPosition: self   endof
        ENDCASE r>drop
        \ position the vertical button in the scroll bar
        TRUE cur-first-line SB_VERT
        GetHandle: self Call SetScrollPos drop

                0 ;M
))
((
;object
))



