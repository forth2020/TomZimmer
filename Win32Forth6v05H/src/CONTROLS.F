\ CONTROLS.F            Additional controls     by Freidrick Prinz

\ September 7th, 1999 - 22:48 tjz corrected a bug reported by Jos v.c. Ven,
\ seems I forgot to initialize the super class of EditControl when I added
\ some enhancements.

\ July 29th, 1999 - 15:16 tjz enhanced EditControl and ComboControl to
\ make them generalized enough to put an edit field on a ToolBar. 

\ January 9th, 1996 - 13:57 tjz Modified and updated both this file and
\ Win32Forth to make this kind of thing easier.  Added CONTROL.F to the
\ Win32Forth system

cr .( Loading Low Level Controls...)

:Class EditControl <Super CONTROL

\ pointers to filter function to allow key capturing.

int pWmChar      \ function returns '0' if it handled message, non-zero otherwise
int pWmKeyDown   \ function returns '0' if it handled message, non-zero otherwise
int pWmKillFocus \ function returns '0' if it handled message, non-zero otherwise

Rectangle ClientRect

:M ClassInit:   ( -- )
                ClassInit: Super
                0 to pWmChar
                0 to pWmKeyDown
                ;M

:M StartSize:   ( width height )
                100
                25
                ;M

:M WindowStyle: ( -- Style )
                WindowStyle: SUPER
                WS_BORDER OR
                WS_TABSTOP OR
                ES_AUTOHSCROLL OR      \ allow horizontal scrolling
                ;M

:M Start:       ( Parent -- )
                TO Parent
                z" EDIT" Create-Control
                ;M

\ Install these filter functions if you want to capture certain keys, like
\ Return or F3, or whatever.

:M SetWmChar:   ( pWmChar -- )          \ install the WM_CHAR filter function
                to pWmChar
                ;M

:M SetWmKeyDown: ( pWmKeyDown -- )      \ install the WM_KEYDOWN filter function
                to pWmKeyDown
                ;M

:M SetWmKillFocus: ( pWmKillFocus -- )  \ install the WM_KILLFOCUS filter function
                to pWmKillFocus
                ;M

:M SubClass:    ( hWnd Parent -- )              \ subclass this control
                to parent
                to hWnd
                subclass
                ;M

:M WM_CHAR      ( h m w l -- res )              \ normal & control chars
                pWmChar
                IF      self pWmChar execute dup
                        IF      DROP
                                old-wndproc CallWindowProc
                        THEN
                ELSE    old-wndproc CallWindowProc
                THEN
                ;M

((
\ example function to process WM_CHAR messages
: myWmChar      ( h m w l obj -- res )
                2 pick VK_RETURN  =
                IF      GetText: [ ]    \ get adr,len of edit control text
                        ...<process WM_CHAR message>...
                        FALSE           \ we already processed this message
                ELSE    drop            \ discard object
                        TRUE            \ and use default processing
                THEN    ;
))

:M WM_KEYDOWN   ( h m w l -- res )      \ normal & control chars
                pWmKeyDown
                IF      self pWmKeyDown execute dup
                        IF      DROP
                                old-wndproc CallWindowProc
                        THEN
                ELSE    old-wndproc CallWindowProc
                THEN
                ;M

:M WM_KILLFOCUS ( h m w l -- res )      \ Allow intervention on kill focus
                pWmKillFocus
                IF      self pWmKillFocus execute dup
                        IF      DROP
                                old-wndproc CallWindowProc
                        THEN
                ELSE    old-wndproc CallWindowProc
                THEN
                ;M

:M WM_SETCURSOR { hndl msg wparam lparam -- res }
                EraseRect: ClientRect                \ init to zeros
                ClientRect.AddrOf GetClientRect: self
                hWnd get-mouse-xy ClientRect.Top  ClientRect.Bottom between
                             over ClientRect.Left ClientRect.Right  between and
                IF      ibeam-cursor
                        1
                ELSE    DROP
                        hndl msg wparam lparam DefWindowProc: [ self ]
                THEN
                ;M

;Class

:Class ComboControl <Super CONTROL      \ an editable combo box

EditControl ComboEdit

:M StartSize:   ( width height )
                100
                100
                ;M

:M WindowStyle: ( -- Style )
                WindowStyle: SUPER
                CBS_DROPDOWN OR
                WS_VSCROLL   OR
                WS_TABSTOP OR
                WS_VISIBLE OR
                ES_AUTOHSCROLL OR       \ allow horizontal scrolling
                ;M

\ Install these filter functions if you want to capture certain keys, like
\ Return or F3, or whatever.

:M SetWmChar:   ( pWmChar -- )          \ install the WM_CHAR filter function
                SetWmChar: ComboEdit
                ;M

:M SetWmKeyDown: ( pWmKeyDown -- )      \ install the WM_KEYDOWN filter function
                SetWmKeyDown: ComboEdit
                ;M

:M SetWmKillFocus: ( pWmKillFocus -- )  \ install the WM_KILLFOCUS filter function
                SetWmKillFocus: ComboEdit
                ;M

:M InsertString: ( adr len -- )
                hWnd NULL =             \ must have a valid handle
                IF      2drop           \ just discard if not running
                ELSE    2dup SetText: ComboEdit
                        asciiz rel>abs
                        dup 0 CB_FINDSTRINGEXACT
                        GetID: self SendDlgItemMessage: parent dup CB_ERR = \ if it's not in list
                        IF      DROP
                                0 CB_INSERTSTRING
                                GetID: self SendDlgItemMessage: parent drop
                                0 0 CB_SETCURSEL        \ set first as current item
                                GetID: self SendDlgItemMessage: parent drop
                        ELSE    NIP                     \ discard string
                                0 swap CB_SETCURSEL     \ set found item as current item
                                GetID: self SendDlgItemMessage: parent drop
                        THEN
                THEN        
                ;M

\ Use: GetText: to get the current combo box string
\ Use: GetString: to get indexed items out of the combo box string list

:M GetString:   ( adr index -- )
                swap dup>r 1+ rel>abs swap
                CB_GETLBTEXT
                GetID: self SendDlgItemMessage: parent 0 max r> c!
                ;M

\ Use: GetCount: to get the count of items in the combo box string list

:M GetCount:    ( -- n1 )       \ n1 = count of items
                0 0 CB_GETCOUNT
                GetID: self SendDlgItemMessage: parent 0 max
                ;M

:M Start:       ( Parent -- )
                TO Parent
                z" COMBOBOX" Create-Control
\ new subclass the embedded EditControl, so we can handle WM_CHAR & WM_KEYDOWN
\ messages to capture keys like Return and F3.
                5 5
                hWnd
                Call ChildWindowFromPoint self SubClass: ComboEdit
                0 0 CB_RESETCONTENT
                GetID: self SendDlgItemMessage: parent drop
                ;M


;Class

:Class ComboListControl <Super ComboControl     \ select only combo box

:M WindowStyle: ( -- Style )
                WindowStyle: SUPER
                CBS_DROPDOWNLIST OR
                ;M

;Class

:Class ListControl <Super CONTROL               \ a list box

:M WindowStyle: ( -- Style )
                WindowStyle: SUPER
                WS_VSCROLL OR
                LBS_NOTIFY OR                   \ enable parent notify
                LBS_NOINTEGRALHEIGHT OR         \ disable integral height
                WS_TABSTOP OR
                ;M

:M Start:       ( Parent -- )
                TO Parent
                z" LISTBOX" Create-Control
                ;M
;Class

:Class GroupControl <Super CONTROL

:M WindowStyle: ( -- Style )
                WindowStyle: SUPER
                BS_GROUPBOX OR
                ;M

:M Start:       ( Parent -- )
                TO Parent
                z" BUTTON" Create-Control
                ;M
;Class

:Class StaticControl <Super CONTROL

:M Start:       ( Parent -- )
                TO Parent
                z" STATIC" Create-Control
                ;M
;Class

:Class CheckControl <Super CONTROL

:M WindowStyle: ( -- style )
                WindowStyle: SUPER
                BS_AUTOCHECKBOX OR
                WS_TABSTOP OR
                ;M

:M Start:       ( Parent -- )
                TO Parent
                z" BUTTON" Create-Control
                ;M

;Class

:Class RadioControl <Super CONTROL

:M WindowStyle: ( -- style )
                WindowStyle: SUPER
                BS_AUTORADIOBUTTON OR
                WS_TABSTOP OR
                ;M

:M Start:       ( Parent -- )
                TO Parent
                z" BUTTON" Create-Control
                ;M

;Class

:Class ButtonControl <Super CONTROL

int buttonfunc

:M ClassInit:   ( -- )
                ClassInit: super
                ['] noop to buttonfunc
                ;M

:M SetFunc:     ( cfa -- )
                to buttonfunc
                ;M

:M WindowStyle: ( -- style )
                WindowStyle: SUPER
                BS_PUSHBUTTON OR
                WS_TABSTOP OR
                ;M

:M Start:       ( Parent -- )
                to Parent
                z" BUTTON" Create-Control
                ;M

:M WM_LBUTTONUP ( h m w l -- res )
                hWnd get-mouse-xy hWnd in-button?
                if      buttonfunc execute
                then
                old-wndproc CallWindowProc
                ;M

;Class

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\       Dialog Window Class
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

VARIABLE Dialog-link
         Dialog-link OFF

:CLASS DialogWindow   <Super Window

: trim-dialogs   ( nfa -- nfa )
\in-system-ok   dup Dialog-link full-trim ;

\in-system-ok forget-chain chain-add trim-dialogs

:M ClassInit: ( -- )
        ClassInit: super
        Dialog-link link,       \ link into list
        self ,
        ;M

: DoDialogMsg { pMsg flag \ mlink -- pMsg f | pMsg FALSE }
        Dialog-link @                   \ all modeless property sheet handles
        begin   dup 0<>                 \ while not end of chain
                flag and                \ and haven't found a handler
        while   dup cell+ @ to mlink >r
                flag Gethandle: mlink 0<> and
                if      pMsg Gethandle: mlink
                        Call IsDialogMessage 0= to flag
                then
                r> @
        repeat  drop pMsg flag ;

msg-chain chain-add DoDialogMsg

;Class

\s ********* SAMPLE Follows ********* SAMPLE Follows *********
\  ********* SAMPLE Follows ********* SAMPLE Follows *********
\  ********* SAMPLE Follows ********* SAMPLE Follows *********


0 value check1

:OBJECT EditSample <Super DialogWindow

  EditControl Edit_1     \ an edit window
StaticControl Text_1     \ a static text window
ButtonControl Button_1   \ a button
ButtonControl Button_2   \ another button
 CheckControl Check_1    \ a check box
 RadioControl Radio_1    \ a radio button
 RadioControl Radio_2    \ another radio button

: CloseSample   ( -- )
                Close: [ self ] ;

:M ExWindowStyle: ( -- style )
                ExWindowStyle: SUPER
                ;M

:M WindowStyle: ( -- style )
                WindowStyle: SUPER
                WS_BORDER OR
                WS_OVERLAPPED OR
                ;M

:M WindowTitle: ( -- title )
                z" " ;M

:M StartSize:   ( -- width height )
                200 100 ;M

:M StartPos:    ( -- x y )
                3 3 ;M

:M On_Init:     ( -- )
                On_Init: super
                self               Start: Check_1
                4 25 60 20          Move: Check_1
                s" Hello"        SetText: Check_1

                self               Start: Radio_1
                80 25 80 20         Move: Radio_1
                s" Hello2"       SetText: Radio_1
                                GetStyle: Radio_1 \ get the default style
                WS_GROUP   OR
                                SetStyle: Radio_1 \ Start a group

                self               Start: Radio_2
                80 45 120 20        Move: Radio_2
                s" Hello Again"  SetText: Radio_2

                self               Start: Text_1 \ start up static text
                                GetStyle: Text_1 \ get the default style
                WS_GROUP     OR                  \ End a group
                SS_CENTER    OR                  \ and centering
                WS_BORDER    OR
                                SetStyle: Text_1 \ and border to style
                4  4 192 20         Move: Text_1 \ position the window
                s" Sample Text"  SetText: Text_1 \ set the window message

                self               Start: Edit_1
                3 72  60 25         Move: Edit_1
                s" 000,00"       SetText: Edit_1

                IDOK               SetID: Button_1
                self               Start: Button_1
                110 72 36 25        Move: Button_1
                s" OK"           SetText: Button_1
                                GetStyle: Button_1
                BS_DEFPUSHBUTTON OR
                                SetStyle: Button_1

                self               Start: Button_2
                150 72 45 25        Move: Button_2
                s" Beep"         SetText: Button_2
                ['] beep         SetFunc: Button_2
                ;M

:M On_Paint:    ( -- )          \ screen redraw procedure
                0 0 200 100 LTGRAY FillArea: dc
                ;M

:M Close:       ( -- )
                GetText: Edit_1 cr type cr
                Close: SUPER ;M

:M WM_COMMAND  ( hwnd msg wparam lparam -- res )
                over LOWORD ( ID )
                case
                IDOK            of Close: self                          endof
                GetID: Check_1  of GetID: Check_1
                                   IsDlgButtonChecked: self to check1 beep  endof
                endcase
                0 ;M

;OBJECT

: demo          ( -- )
                Start: EditSample ;

