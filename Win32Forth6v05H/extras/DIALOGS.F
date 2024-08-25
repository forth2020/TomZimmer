\ DIALOGS.F     sample dialogs from forthdlg.dlg inline compiled
\                                   mis@kitchen.forth-ev.de  (Michael Schroeder)

BEGIN-RESSOURCE FORTHDLG2

( 1 DLGINCLUDE "FORTHDLG.H" )

( IDD_FONTDLG DIALOG 130, 90, 160, 135
LANGUAGE LANG_ENGLISH, SUBLANG_ENGLISH_US
STYLE DS_MODALFRAME | WS_POPUP | WS_VISIBLE | WS_CAPTION | WS_SYSMENU
CAPTION "Console Font"
FONT 8, "MS Sans Serif"
BEGIN
    PUSHBUTTON      "OK", IDOK, 114, 6, 40, 14
    PUSHBUTTON      "Cancel", IDCANCEL, 114, 23, 40, 14
    LTEXT           "Select the Forth Console Font", 102, 9, 7, 100, 12
    LISTBOX         IDD_FONT, 11, 20, 95, 61, LBS_SORT | WS_VSCROLL |
                    WS_TABSTOP
    EDITTEXT        IDD_TEXT, 12, 94, 97, 12, ES_AUTOHSCROLL
END )

 DS_MODALFRAME WS_POPUP OR WS_VISIBLE OR
 WS_CAPTION OR WS_SYSMENU OR
 TO STYLEFLAGS
 8  DIALOG-FONT    "MS Sans Serif"
 DIALOG-CAPTION "Console Font"
 IDD_FONTDLG 130 90 160 135
 BEGIN-DIALOG
    IDOK        114 6 40 14     PUSHBUTTON      "OK"
    IDCANCEL    114 23 40 14    PUSHBUTTON      "Cancel"
    102         9 7 100 12      LTEXT           "Select the Forth Console Font"
  LBS_SORT WS_VSCROLL OR WS_TABSTOP OR TO STYLEFLAGS
    IDD_FONT    11 20 95 61     LISTBOX
  ES_AUTOHSCROLL TO STYLEFLAGS
    IDD_TEXT     12 94 97 12    EDITTEXT
 END-DIALOG

( IDD_ABOUT_FORTH DIALOG 97, 55, 212, 173
LANGUAGE LANG_ENGLISH, SUBLANG_ENGLISH_US
STYLE DS_MODALFRAME | WS_POPUP | WS_VISIBLE | WS_CAPTION | WS_SYSMENU
CAPTION "About"
FONT 8, "MS Sans Serif"
BEGIN
    PUSHBUTTON      "Done", IDCANCEL, 166, 153, 40, 14
    LTEXT           "", IDD_ABOUT_TEXT, 14, 12, 187, 36
    LTEXT           "", IDD_ABOUT_TEXT2, 14, 52, 187, 93
END )

 DS_MODALFRAME WS_POPUP OR WS_VISIBLE OR
 WS_CAPTION OR WS_SYSMENU OR
 TO STYLEFLAGS
 8  DIALOG-FONT    "MS Sans Serif"
    DIALOG-CAPTION "About"
 IDD_ABOUT_FORTH 97 55 212 273
 BEGIN-DIALOG
    IDCANCEL        166 153 40 14     PUSHBUTTON  "Done"
    IDD_ABOUT_TEXT  14 12 187 36      LTEXT       ""
    IDD_ABOUT_TEXT2 14 52 187 93      LTEXT       ""
 END-DIALOG

( IDD_SAVE_MEMORY DIALOG 110, 76, 147, 82
LANGUAGE LANG_ENGLISH, SUBLANG_ENGLISH_US
STYLE DS_MODALFRAME | WS_POPUP | WS_VISIBLE | WS_CAPTION | WS_SYSMENU
CAPTION "Saved Image Memory"
FONT 8, "MS Sans Serif"
BEGIN
    LTEXT           "How many bytes of dictionary space do you want available when this saved Forth image starts up?",
                    201, 11, 10, 122, 33
    EDITTEXT        IDD_AVAIL_MEMORY, 22, 45, 44, 12, ES_AUTOHSCROLL
    PUSHBUTTON      "Save", IDOK, 101, 44, 40, 14
    PUSHBUTTON      "Cancel", IDCANCEL, 101, 63, 40, 14
END )

 DS_MODALFRAME WS_POPUP OR WS_VISIBLE OR
 WS_CAPTION OR WS_SYSMENU OR
 TO STYLEFLAGS
 8  DIALOG-FONT    "MS Sans Serif"
    DIALOG-CAPTION "Saved Image Memory"
 IDD_SAVE_MEMORY 110 76 147 82
 BEGIN-DIALOG
        201 11 10 122 33    LTEXT "How many bytes of dictionary space do you want available when this saved Forth image starts up?"
        ES_AUTOHSCROLL TO STYLEFLAGS
        IDD_AVAIL_MEMORY 22 45 44 12     EDITTEXT
        IDOK            101 44  40 14    PUSHBUTTON "Save"
        IDCANCEL        101 63  40 14    PUSHBUTTON "Cancel"
 END-DIALOG

( IDD_PAGEUP DIALOG 122, 67, 206, 93
LANGUAGE LANG_ENGLISH, SUBLANG_ENGLISH_US
STYLE DS_MODALFRAME | WS_POPUP | WS_VISIBLE | WS_CAPTION | WS_SYSMENU
CAPTION "Page Up Setup"
FONT 8, "MS Sans Serif"
BEGIN
    PUSHBUTTON      "1 Up", IDCANCEL, 158, 23, 40, 14
    PUSHBUTTON      "2 Up", IDD_2UP, 158, 47, 40, 14
    PUSHBUTTON      "4 Up", IDD_4UP, 158, 70, 40, 14
    LTEXT           "Specify the number of logical page images to print on each physical page.",
                    301, 8, 41, 78, 35
    CONTROL         "", 304, "Static", SS_BLACKFRAME, 133, 20, 11, 17
    CONTROL         "", 305, "Static", SS_BLACKFRAME, 128, 45, 11, 17
    CONTROL         "", 306, "Static", SS_BLACKFRAME, 130, 68, 9, 10
    CONTROL         "", 307, "Static", SS_BLACKFRAME, 138, 68, 9, 10
    CONTROL         "", 308, "Static", SS_BLACKFRAME, 138, 77, 9, 10
    CONTROL         "", 309, "Static", SS_BLACKFRAME, 130, 77, 9, 10
    CONTROL         "", 310, "Static", SS_BLACKFRAME, 138, 45, 11, 17
    CONTROL         "", 311, "Static", SS_BLACKFRAME, 98, 44, 13, 10
    CONTROL         "", 312, "Static", SS_BLACKFRAME, 98, 53, 13, 10
    LTEXT           "or", 313, 115, 50, 10, 8
    RTEXT           "Reselect  "" 1 Up ""   to complete multipage printing",
                    314, 17, 7, 181, 8
END )

 DS_MODALFRAME WS_POPUP OR WS_VISIBLE OR
 WS_CAPTION OR WS_SYSMENU OR
 TO STYLEFLAGS
 8  DIALOG-FONT    "MS Sans Serif"
    DIALOG-CAPTION "Page Up Setup"
 IDD_PAGEUP 122 67 206 93
 BEGIN-DIALOG
        IDCANCEL   158 23 40 14          PUSHBUTTON "1 Up"
        IDD_2UP    158 47 40 14          PUSHBUTTON "2 Up"
        IDD_4UP    158 70 40 14          PUSHBUTTON "4 Up"
        301 8 41 78 35
        LTEXT "Specify the number of logical page images to print on each physical page."
      SS_BLACKFRAME TO STYLEFLAGS
        304 CLASS_STATIC 133 20 11 17  aCONTROL ""
      SS_BLACKFRAME TO STYLEFLAGS
        305 CLASS_STATIC 128 45 11 17  aCONTROL ""
      SS_BLACKFRAME TO STYLEFLAGS
        306 CLASS_STATIC 130 68 9 10   aCONTROL ""
      SS_BLACKFRAME TO STYLEFLAGS
        307 CLASS_STATIC 138 68 9 10   aCONTROL ""
      SS_BLACKFRAME TO STYLEFLAGS
        308 CLASS_STATIC 138 77 9 10   aCONTROL ""
      SS_BLACKFRAME TO STYLEFLAGS
        309 CLASS_STATIC 130 77 9 10   aCONTROL ""
      SS_BLACKFRAME TO STYLEFLAGS
        310 CLASS_STATIC 138 45 11 17  aCONTROL ""
      SS_BLACKFRAME TO STYLEFLAGS
        311 CLASS_STATIC 98 44 13 10   aCONTROL ""
      SS_BLACKFRAME TO STYLEFLAGS
        312 CLASS_STATIC 98 53 13 10   aCONTROL ""
        313 115 50 10 8   LTEXT "or"
        314 17 7 181 8    RTEXT "Reselect  < 1 Up >   to complete multipage printing"
 END-DIALOG


( IDD_EDIT2_DIALOG DIALOG 333, 150, 178, 78
LANGUAGE LANG_ENGLISH, SUBLANG_ENGLISH_US
STYLE DS_MODALFRAME | WS_POPUP | WS_VISIBLE | WS_CAPTION | WS_SYSMENU
CAPTION "Text Entry Dialog Title"
FONT 8, "MS Sans Serif"
BEGIN
    EDITTEXT        IDD_EDIT_TEXT, 5, 31, 107, 12, ES_AUTOHSCROLL
    EDITTEXT        IDD_EDIT2_TEXT, 5, 59, 107, 12, ES_AUTOHSCROLL
    PUSHBUTTON      "OK", IDOK, 130, 6, 40, 14
    PUSHBUTTON      "Cancel", IDCANCEL, 130, 23, 40, 14
    LTEXT           "Text Entry Prompt", IDD_PROMPT_TEXT, 5, 19, 107, 10
    LTEXT           "Text Entry Prompt", IDD_PROMPT2_TEXT, 5, 46, 106, 10
    PUSHBUTTON      "", IDOK2, 119, 41, 51, 14
    PUSHBUTTON      "", IDOK3, 119, 58, 51, 14
    AUTOCHECKBOX    "", IDB_OPTION, 7, 6, 105, 10
END )

 DS_MODALFRAME WS_POPUP OR WS_VISIBLE OR
 WS_CAPTION OR WS_SYSMENU OR
 TO STYLEFLAGS
 8  DIALOG-FONT    "MS Sans Serif"
    DIALOG-CAPTION "Text Entry Dialog Title"
 IDD_EDIT2_DIALOG 333 150 178 78
 BEGIN-DIALOG
        ES_AUTOHSCROLL TO STYLEFLAGS
        IDD_EDIT_TEXT    5 31 107 12     EDITTEXT
        ES_AUTOHSCROLL TO STYLEFLAGS
        IDD_EDIT2_TEXT   5 59 107 12     EDITTEXT
        IDOK             130  6  40 14   PUSHBUTTON "OK"
        IDCANCEL         130 23  40 14   PUSHBUTTON "Cancel"
        IDD_PROMPT_TEXT  5 19 107 10     LTEXT      "Text Entry Prompt"
        IDD_PROMPT2_TEXT 5 46 106 10     LTEXT      "Text Entry Prompt"
        IDOK2            119 41 51 14    PUSHBUTTON ""
        IDOK3            119 58 51 14    PUSHBUTTON ""
        IDB_OPTION       7  6 105 10     AUTOCHECKBOX   ""
  END-DIALOG

( IDD_EDIT_DIALOG DIALOG 105, 69, 200, 65
LANGUAGE LANG_ENGLISH, SUBLANG_ENGLISH_US
STYLE DS_MODALFRAME | WS_POPUP | WS_VISIBLE | WS_CAPTION | WS_SYSMENU
CAPTION "Text Entry Dialog Title"
FONT 8, "MS Sans Serif"
BEGIN
    EDITTEXT        IDD_EDIT_TEXT, 5, 45, 189, 12, ES_AUTOHSCROLL
    PUSHBUTTON      "OK", IDOK, 154, 6, 40, 14
    PUSHBUTTON      "Cancel", IDCANCEL, 154, 23, 40, 14
    LTEXT           "Text Entry Prompt", IDD_PROMPT_TEXT, 6, 25, 138, 10
    AUTOCHECKBOX    "", IDB_OPTION, 7, 9, 134, 10
END )


 DS_MODALFRAME WS_POPUP OR WS_VISIBLE OR
 WS_CAPTION OR WS_SYSMENU OR
 TO STYLEFLAGS
 8  DIALOG-FONT    "MS Sans Serif"
    DIALOG-CAPTION "Text Entry Dialog Title"
 IDD_EDIT_DIALOG 105 69 200 65
 BEGIN-DIALOG
        ES_AUTOHSCROLL TO STYLEFLAGS
        IDD_EDIT_TEXT    5 45 189 12     EDITTEXT
        IDOK             154  6  40 14   PUSHBUTTON "OK"
        IDCANCEL         154 23  40 14   PUSHBUTTON "Cancel"
        IDD_PROMPT_TEXT  6 25 138 10     LTEXT      "Text Entry Prompt"
        IDB_OPTION       7  9 134 10     AUTOCHECKBOX   ""
  END-DIALOG

  END-RESSOURCE

IDD_FONTDLG     forthdlg  find-dialog-id constant orgFontTpl
IDD_FONTDLG     forthdlg2 find-dialog-id constant newFontTpl

IDD_ABOUT_FORTH forthdlg  find-dialog-id constant orgAboutTpl
IDD_ABOUT_FORTH forthdlg2 find-dialog-id constant newAboutTpl


IDD_SAVE_MEMORY forthdlg  find-dialog-id constant orgMemTpl
IDD_SAVE_MEMORY forthdlg2 find-dialog-id constant newMemTpl


IDD_PAGEUP      forthdlg  find-dialog-id constant orgPageTpl
IDD_PAGEUP      forthdlg2 find-dialog-id constant newPageTpl


IDD_EDIT2_DIALOG forthdlg  find-dialog-id constant orgEd2Tpl
IDD_EDIT2_DIALOG forthdlg2 find-dialog-id constant newEd2Tpl


IDD_EDIT_DIALOG forthdlg  find-dialog-id constant orgEdTpl
IDD_EDIT_DIALOG forthdlg2 find-dialog-id constant newEdTpl

\S
\ --------- helper words, to dump two memory areas ---------------------

0 value memory1
0 value memory2

\ prints twin dump, to visually compare memory areas, and set up for
\ twin+ or twin-
: twindump { area1 area2 dumplen -- }
        CR >BOLD ." ERSTER BEREICH: " area1 H. >NORM
        area1 dumplen dump CR
        CR >BOLD ." ZWEITER BEREICH: " area2 H. >NORM
        area2 dumplen dump CR

        area1 dumplen + TO memory1
        area2 dumplen + TO memory2

        area1 dumplen area2 dumplen compare
        CR >BOLD ." Compare result: " . >NORM
        CR
;

: twin+ { dumplen -- }
        memory1 memory2 = ?EXIT         \ only if they are different
        memory1 memory2 dumplen twindump

;

: twin- { dumplen -- }
        memory1 memory2 = ?EXIT         \ only if they are different
        memory1 dumplen - 0 MIN
        memory2 dumplen - 0 MIN
        dumplen twindump

;

\ ----------------------------------------------------------------------


