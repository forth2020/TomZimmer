\ WINDILOG.F    Example of a user created Dialog        by Freidrich Prinz

\ Modified for Win32Forth by Tom Zimmer

:OBJECT EditSample <SUPER WINDOW

      ListControl List_1     \ a list box
 ComboListControl CbList_1   \ a combo list box
     GroupControl Group_1    \ a frame around a group
      EditControl Edit_1     \ an edit window
    StaticControl Text_1     \ a static text window
    ButtonControl Button_1   \ a button
     CheckControl Check_1    \ a check box
     RadioControl Radio_1    \ a radio button
     RadioControl Radio_2    \ another radio button

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
                z" " ;M

:M StartSize:   ( -- width height )
                300 350 ;M

:M StartPos:    ( -- x y )
                30 30 ;M

:M Close:       ( -- )
\                GetText: Edit_1  cr type cr
                Close: SUPER
                ;M

:M On_Init:     ( -- )
                self                Start: Check_1
                4 25 60 20           Move: Check_1
                s" Hello"         SetText: Check_1

                self                Start: Radio_1
                100 75 130 20        Move: Radio_1
                s" Hello Again 1" SetText: Radio_1
                WS_GROUP           +Style: Radio_1 \ Start a group

                self                Start: Radio_2
                100 95 130 20        Move: Radio_2
                BS_CENTER          +Style: Radio_2 \ and centering
                s" Hello Again 2" SetText: Radio_2

                self                Start: Group_1
                90 55 140 70         Move: Group_1
                s" Radios"        SetText: Group_1

                self                Start: List_1
                4 140 100 60         Move: List_1
                0 0 LB_RESETCONTENT GetID: List_1 SendDlgItemMessage: self drop
z" Ola1 " rel>abs 0 LB_ADDSTRING    GetID: List_1 SendDlgItemMessage: self drop
z" Ola2 " rel>abs 0 LB_ADDSTRING    GetID: List_1 SendDlgItemMessage: self drop
z" Ola3 " rel>abs 0 LB_ADDSTRING    GetID: List_1 SendDlgItemMessage: self drop

                self                Start: CbList_1
                4 220 100 90         Move: CbList_1
                0 0 CB_RESETCONTENT GetID: CbList_1 SendDlgItemMessage: self drop
z" Ola4 " rel>abs 0 CB_ADDSTRING    GetID: CbList_1 SendDlgItemMessage: self drop
z" Ola5 " rel>abs 0 CB_ADDSTRING    GetID: CbList_1 SendDlgItemMessage: self drop
z" Ola6 " rel>abs 0 CB_ADDSTRING    GetID: CbList_1 SendDlgItemMessage: self drop
z" Ola7 " rel>abs 0 CB_ADDSTRING    GetID: CbList_1 SendDlgItemMessage: self drop
z" Ola8 " rel>abs 0 CB_ADDSTRING    GetID: CbList_1 SendDlgItemMessage: self drop
z" Ola9 " rel>abs 0 CB_ADDSTRING    GetID: CbList_1 SendDlgItemMessage: self drop
                0 0 CB_SETCURSEL    GetID: CbList_1 SendDlgItemMessage: self drop

                self                Start: Text_1 \ start up static text
                WS_GROUP           +Style: Text_1 \ End a group
                SS_CENTER          +Style: Text_1 \ and centering
                WS_BORDER          +Style: Text_1 \ and border to style
                4  4 192 20          Move: Text_1 \ position the window
                s" Sample Text"   SetText: Text_1 \ set the window message

                self                Start: Edit_1
                3 72  60 25          Move: Edit_1
                s" 000,00"        SetText: Edit_1

                IDOK                SetID: Button_1
                self                Start: Button_1
                126 172 70 25        Move: Button_1
                s" OK"            SetText: Button_1
                                 GetStyle: Button_1
                BS_DEFPUSHBUTTON   +Style: Button_1
                ;M

:M On_Paint:    ( -- )          \ screen redraw procedure
                0 0 StartSize: self LTGRAY FillArea: dc
                ;M

:M WM_COMMAND   ( hwnd msg wparam lparam -- res )
        over LOWORD ( ID )
        case
                IDOK            of Close: self          endof
                GetID: Check_1  of GetID: Check_1 IsDlgButtonChecked: self
                                   if      beep
                                   then                 endof
        endcase
        0 ;M

;OBJECT

: DEMO          ( -- )
                Start: EditSample ;

.( Type DEMO to run the dialog demo. )

