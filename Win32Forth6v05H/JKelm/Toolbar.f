\    File: Toolbar.f
\  Author: Jeff Kelm
\ Created: 09-Sep-1998
\ Updated: 15-Oct-1998



NEEDS WinBase.f



\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ Generic Toolbar class
\
:Class GenericToolbar  <Super BaseChildWindow

    INT tbarID         \ toolbar command identifier
\ use uID    INT hBmp           \ handle to bitmap used in toolbar
    INT images         \ number of images in the bitmap
    INT NextButton     \ index of next button to be defined
    Record: tbAddbitmap   \ toolbar bitmap data structure
        INT hInst       \ handle to module instance (NULL)
        INT uID         \ resource identifier (bitmap handle)
    ;Record
    Record: tbButton   \ toolbar button data structure
        INT   iBitmap     \ Zero-based index of button image to use
        INT   idCommand   \ Command identifier associated with the button
        BYTE  fsState     \ Button state flags
        BYTE  fsStyle     \ Button style
        Align             \ 2 bytes of padding to word align dwData
        INT   dwData      \ Application-defined value
        INT   iString     \ Zero-based index of button string
    ;RecordSize: sizeof(tbButton)

:M ToolbarStyle: ( -- style)   \ override to disable tooltips or something other?
        WS_BORDER  WS_VISIBLE OR   \ WS_CHILD is implied
        TBSTYLE_TOOLTIPS OR
        ;M

:M Create: ( hParent)   \ create an empty toolbar window
        PutParent: self
        CreateNewID TO tbarID
        TBSTATE_ENABLED TO fsState   \ default button state
        TBSTYLE_BUTTON TO fsStyle    \ default button style
        NULL appInst tbarID  GetParent: self 0 0 0 0
        WS_CHILD ToolbarStyle: self OR
        NULL TOOLBARCLASSNAME 0
        Call CreateWindowEx  DUP PutHandle: self  ?WinError
        0 sizeof(tbButton) TB_BUTTONSTRUCTSIZE
           SendMessage: self DROP
        ;M

:M GetID: ( -- tbarID)   \ return toolbar command identifier
        tbarID
        ;M

:M LoadBitmap: ( n hBmp)   \ loads toolbar bitmap with n images
        TO uID  TO images  NULL TO hInst  
        tbAddbitmap rel>abs images TB_ADDBITMAP SendMessage: self  DROP
        ;M

:M LoadStdBmp: ( -- )   \ loads the system default toolbar bitmap
        IDB_STD_SMALL_COLOR TO uId  15 TO images  HINST_COMMCTRL to hInst
        tbAddbitmap rel>abs images TB_ADDBITMAP SendMessage: self  DROP
        ;M

:M GetBitmap: ( -- hBmp)   \ returns handle to toolbar bitmap
        uID
        ;M

\ TBSTYLE_BUTTON, TBSTYLE_GROUP,     \ default button behavior
\ TBSTYLE_CHECK, TBSTYLE_CHECKGROUP  \ toggles between pressed and unpressed
\ or TBSTYLE_SEP                     \ seperation between buttons
:M SetButtonStyle: ( fsState id)   \ set specified button to this style
        ( tbd) TO fsStyle DROP
        ;M

:M GetButtonStyle: ( id -- fsStyle)
        0 SWAP TB_COMMANDTOINDEX SendMessage: self
        tbButton rel>abs SWAP TB_GETBUTTON SendMessage: self ?WinError
        fsStyle
        ;M

\ TBSTATE_CHECKED, TBSTATE_PRESSED,
\ TBSTATE_ENABLED, TBSTATE_HIDDEN, TBSTATE_INDETERMINATE,
\ or TBSTATE_WRAP   \ A line break follows the button. The button must also
                    \ have the TBSTATE_ENABLED state
:M SetButtonState: ( fsState id)   \ set specified button to this state
        0 ROT MAKELONG SWAP
        TB_SETSTATE SendMessage: self ?WinError
        ;M

:M GetButtonState: ( id -- fsState)   \ get current state of specified button
        0 SWAP TB_GETSTATE SendMessage: self
        ;M

:M Button: ( id iBmp -- )   \ create the next button with bmp offset iBmp
        TO iBitmap
        TO idCommand
        TBSTATE_ENABLED TO fsState
        TBSTYLE_BUTTON TO fsStyle
        tbButton rel>abs  NextButton  TB_INSERTBUTTON
           SendMessage: self ?WinError
        NextButton 1+ TO NextButton
        ;M

:M Seperator: ( -- )   \ add a seperator to toolbar
        0 TO iBitmap
        0 TO idCommand
        TBSTATE_ENABLED TO fsState
        TBSTYLE_SEP TO fsStyle      \ set to seperator style
        tbButton rel>abs  NextButton  TB_INSERTBUTTON
           SendMessage: self ?WinError
        NextButton 1+ TO NextButton
        ;M

:M GetIndex: ( id -- posn)
        0 SWAP TB_COMMANDTOINDEX SendMessage: self
        ;M

:M GreyButton: ( id)
        0 TRUE makelong SWAP TB_INDETERMINATE SendMessage: self ?WinError
        ;M

:M UnGreyButton: ( id)
        0 FALSE makelong SWAP TB_INDETERMINATE SendMessage: self ?WinError
        ;M

:M EnableButton: ( id)
        0 TRUE makelong SWAP TB_ENABLEBUTTON SendMessage: self ?WinError
        ;M

:M DisableButton: ( id)
        0 FALSE makelong SWAP TB_ENABLEBUTTON SendMessage: self ?WinError
        ;M

:M ShowButton: ( id)
        0 FALSE makelong SWAP TB_HIDEBUTTON SendMessage: self ?WinError
        ;M

:M HideButton: ( id)
        0 TRUE makelong SWAP TB_HIDEBUTTON SendMessage: self ?WinError
        ;M

:M CheckButton: ( id)
        0 TRUE makelong SWAP TB_CHECKBUTTON SendMessage: self ?WinError
        ;M

:M UncheckButton: ( id)
        0 FALSE makelong SWAP TB_CHECKBUTTON SendMessage: self ?WinError
        ;M

:M GetTooltips: ( -- hndl)   \ returns handle to toolbar tooltip window
        0 0 TB_GETTOOLTIPS  SendMessage: self  DUP ?WinError
        ;M

:M Autosize: ( -- )   \ resizes toolbar to fill width of parent window
        0 0 TB_AUTOSIZE SendMessage: self DROP
        ;M

;Class



\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ Toolbar class to allow other than default (24 by 22) size buttons
\
:Class SizableToolbar <Super GenericToolbar   \ call with size & parent on stack

:M ClassInit: ( x y hWnd)
        >R  SWAP 16 LSHIFT OR 0 TB_SETBUTTONSIZE SendMessage: self ?WinError
        R>  ClassInit: super
        ;M

;Class

