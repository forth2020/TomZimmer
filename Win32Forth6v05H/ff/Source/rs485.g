\ Multidrop communication dialog box

0 value mdport#         \ COM port
128 value mdaddress
1 value RTStransmit?
create baud$ 20 chars allot
3758921 constant MDconnect_key

:OBJECT MDparams <SUPER WINDOW \ -----------------------------------------------

 ComboListControl CbList_1   \ com port list box
 ComboListControl CbList_2   \ baud rate list box
    ButtonControl Button_1   \ OK button
     CheckControl Check_1    \ RTS check box

:M ClassInit:   ( -- )
                ClassInit: super
                ;M

:M ExWindowStyle: ( -- style )
                ExWindowStyle: SUPER
                ;M

:M WindowStyle: ( -- style )
                WindowStyle: SUPER
                WS_BORDER OR
                WS_OVERLAPPED OR
                ;M

:M WindowTitle: ( -- title )
                z" Multidrop RS485 Settings" ;M

:M StartSize:   ( -- width height )
                260 60 ;M

:M StartPos:    ( -- x y )
                100 100 ;M

:M Close:       ( -- )
\                GetText: Edit_1  cr type cr
                Close: SUPER
                ;M

: +com   rel>abs 0 CB_ADDSTRING   GetID: CbList_1 SendDlgItemMessage: self drop ;
: +bd    rel>abs 0 CB_ADDSTRING   GetID: CbList_2 SendDlgItemMessage: self drop ;

:M On_Init:     ( -- )
                self                Start: Check_1
                10 40 200 20         Move: Check_1
                s" RTS T/R control" SetText: Check_1
                self                Start: CbList_1
                10  5 80 90         Move: CbList_1
                0 0 CB_RESETCONTENT GetID: CbList_1 SendDlgItemMessage: self drop
                z" COM 1" +com   z" COM 2" +com
                z" COM 3" +com   z" COM 4" +com
                0 0 CB_SETCURSEL    GetID: CbList_1 SendDlgItemMessage: self drop
                self                Start: CbList_2
                100 5 80 90         Move: CbList_2
                0 0 CB_RESETCONTENT GetID: CbList_2 SendDlgItemMessage: self drop
                z" 2400" +bd   z" 4800" +bd   z" 9600" +bd
                z" 14400" +bd  z" 19200" +bd  z" 38400" +bd
                z" 57600" +bd  z" 115200" +bd
                0 5 CB_SETCURSEL    GetID: CbList_2 SendDlgItemMessage: self drop

                IDOK                SetID: Button_1
                self                Start: Button_1
                190 5 60 50          Move: Button_1
                s" OK"            SetText: Button_1
                                 GetStyle: Button_1
                BS_DEFPUSHBUTTON   +Style: Button_1
                RTStransmit? GetID: Check_1 CheckDlgButton: self  
                ;M

:M On_Paint:    ( -- )          \ screen redraw procedure
                0 0 StartSize: self LTGRAY FillArea: dc
                ;M

:M WM_COMMAND   ( hwnd msg wparam lparam -- res )
                over LOWORD ( ID )
        IDOK =
        if      GetText: CbList_1 drop 4 chars + c@ 7 and to mdport# \ COM #
                GetText: CbList_2 baud$ place                      \ baud string
                GetID: Check_1  IsDlgButtonChecked: self to RTStransmit?
                Close: self
                MDconnect_key pushkey
        then
                0 ;M

;OBJECT \ ----------------------------------------------------------------------


: StartMdrop    ( -- )
                Start: MDparams
                ;

