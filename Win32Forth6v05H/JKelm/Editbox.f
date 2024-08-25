\    File: Editbox.f
\  Author: Jeff Kelm
\ Created: 02-Oct-1998
\ Updated: 15-Oct-1998



NEEDS WinBase.f



\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ \\\ Generic Multiline Edit Control Class
\
:Class GenericMultilineEdit  <Super BaseChildWindow

    font wfont   \ font to use for edit control

    CREATE 'tabs  16 ,   \ tabstops in dialog-box units (32 default)

:M Facename: ( -- a n)   \ returns font facename
        S" Terminal"
        ;M

:M CharSet: ( -- f)   \ return font character set to use
        OEM_CHARSET
        ;M

:M PitchAndFamily: ( -- f)   \ return font family and pitch
        FIXED_PITCH FF_MODERN OR
        ;M

:M ClassInit: ( -- )
        \ Create initial font for control
        FaceName: self  SetFaceName: wFont
        CharSet: self   CharSet: wFont
        PitchAndFamily: self  PitchAndFamily: wFont
        Create: wFont
        ;M

:M Style: ( -- style)   \ typical multiline, none-wrapped control style
        WS_CHILD WS_VISIBLE OR ES_LEFT OR ES_MULTILINE OR
        WS_VSCROLL OR ES_AUTOVSCROLL OR
        WS_HSCROLL OR ES_AUTOHSCROLL OR
        ES_NOHIDESEL OR
        ;M

:M LeftMargin: ( -- n)   4 ;M
:M RightMargin: ( -- n)   4 ;M

:M Create: ( hParent -- )
        PutParent: self
        NULL
        GWL_HINSTANCE  GetParent: self  Call GetWindowLong
        CreateNewID
        GetParent: self
        0 0 0 0
        Style: self
        NULL
        Z" EDIT" rel>abs
        NULL
        Call CreateWindowEx  DUP PutHandle: self  ?WinError
        \ set window font
        TRUE Handle: wFont WM_SETFONT SendMessage: self  DROP
        \ set tab stops for window
        'tabs rel>abs 1 EM_SETTABSTOPS SendMessage: self  ?WinError
        \ set margins for window
        LeftMargin: self  RightMargin: self  MAKELONG
           EC_LEFTMARGIN EC_RIGHTMARGIN OR  EM_SETMARGINS
           SendMessage: self DROP
        ;M

:M SetText: ( szText)   \ send text to control
        ;M

:M GetText: ( szBuffer)   \ send control text to buffer
        ;M

:M Undo: ( -- )   \ Send WM_UNDO only if there is something to be undone
        0 0 EM_CANUNDO  SendMessage: self
          IF  0 0 WM_UNDO  SendMessage: self  ?WinError
        ELSE  MB_OK
           Z" Undo notification" rel>abs
           Z" Nothing to undo." rel>abs
           GetHandle: self  Call MessageBox  DROP
        THEN
        ;M

:M Cut: ( -- )   \ cut selected text to clipboard
        0 0 WM_CUT SendMessage: self  DROP
        ;M

:M Copy: ( -- )   \ copy selected text to clipboard
        0 0 WM_COPY SendMessage: self  DROP
        ;M

:M Paste: ( -- )   \ paste clipboard text to control
        0 0 WM_PASTE SendMessage: self  DROP
        ;M

:M Clear: ( -- )   \ delete selected text (not to clipboard!)
        0 0 WM_CLEAR SendMessage: self  DROP
        ;M

:M SetSelection: ( nEnd nStart -- )   \ set selection range
        EM_SETSEL SendMessage: self  DROP
        ;M

:M GetSelection: ( -- nEnd nStart)   \ return selection range
        NULL NULL EM_GETSEL SendMessage: self
        DUP HIWORD SWAP LOWORD
        ;M

:M SelectAll: ( -- )   \ select all the text in control
        -1 0 SetSelection: self  DROP
        ;M

:M RemoveSelection: ( -- )   \ remove any selection
        0 -1 SetSelection: self  DROP
        ;M

:M GetCursor: ( -- n)   \ return location of cursor (chars from start)
        GetSelection: self
        RemoveSelection: self
        GetSelection: self  >R
        SetSelection: self  R> DROP
        ;M

:M ReadOnly: ( f -- )
        0 SWAP EM_SETREADONLY SendMessage: self  ?WinError
        ;M

:M Wrap: ( -- )   \ set control to wrap text
        ;M

:M Unwrap: ( -- )   \ set control to scroll instead of wrap text
        ;M

;Class

