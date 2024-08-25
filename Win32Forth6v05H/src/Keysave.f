\ KEYSAVE.F             Save All Keystrokes             Tom Zimmer

cr .( Loading Keyboard Macro...)

INTERNAL        \ start of non-user definitions

:Class LogFile          <Super  Object

int logfilename
int logfiledefault
int loghndl
int playhndl
int logout
int logtabsize
int log#

max-path bytes logfilebuf

:M ClassInit:   ( -- )
                ClassInit: super
                here to logfiledefault ,"text"
                logfiledefault to logfilename
                0 to logout
                8 to logtabsize
                0 to log#
                0 to loghndl
                0 to playhndl
                ;M

:M GetLogName:  ( -- a1 n1 )
                logfilename count 
                ;M

:M SetLogTab:   ( n1 -- )
                to logtabsize
                ;M

:M CloseLog:    ( -- )
                loghndl
                if      loghndl close-file drop
                        0 to loghndl
                then    ;M

:M "Log:        ( a1 n1 -- )
                dup +to logout
                loghndl
                if      loghndl write-file
                        dup s" Log File Write Error" ?MessageBox
                        if      CloseLog: self
                        then
                else    2drop
                then    ;M

:M LogSpaces:   ( n1 -- )
                spcs-max min spcs swap "Log: self
                ;M

:M LogTab:      ( -- )
                logout logtabsize / 1+ logtabsize * logout - 0max
                LogSpaces: self
                ;M

create &key 0 c,

:M LogEmit:     ( c1 -- )
                1 +to logout
                loghndl
                if      &key c! &key 1 loghndl write-file
                        dup s" Log File Write Error" ?MessageBox
                        if      CloseLog: self
                        then
                else    drop
                then    ;M

:M LogCr:       ( -- )
                0 to logout
                loghndl
                if      crlf$ count loghndl write-file
                        dup s" Log File Write Error" ?MessageBox
                        if      CloseLog: self
                        then
                then    ;M

:M OpenLog:     ( -- )
                loghndl
                if      CloseLog: self
                then
                logfilename count r/w open-file
                if      drop                            \ discard handle
                        logfilename count
                        r/w create-file
                        dup s" Failed to Create the LOG File" ?MessageBox
                        if      drop
                        else    to loghndl
                                base @ >r decimal
                                LogTab: self
                                s"  \ "              "Log: self
                                log# 0 <# # # # #>   "Log: self
                                s"    "              "Log: self
                                LogCr: self
                                r> base !
                                LogTab: self
                                s"  \ Log Created: " "Log: self
                                get-local-time
                                time-buf dup >date"  "Log: self
                                            s"  - "  "Log: self
                                             >time"  "Log: self
                                LogCr: self
                                loghndl file-append drop
                        then
                else    to loghndl                      \ save handle
                        loghndl file-append drop
                then
                ;M

 :M "NewLog:     ( a1 n1 -- )
                CloseLog: self
                logfilebuf place
                logfilebuf to logfilename
                logfilename count r/w create-file 
                dup s" Error Creating LOG file" ?MessageBox
                if      drop
                else    to loghndl
                        OpenLog:  self
                        CloseLog: self
                then
                ;M

:M NewLog:      { \ key$ -- }
                MAXSTRING localAlloc: key$
                s" KEYS"              key$ place        \ lay in start of Macro name
                log# 0 <# # # # #>    key$ +place       \ append the file number
                s" .LOG"              key$ +place       \ append the file extension
                1 +to log#                              \ bump to next log number
                key$ count "NewLog: self
                ;M

:M PlayClose:   ( -- )
                playhndl
                if      playhndl close-file drop
                        0 to playhndl
                then
                ;M

:M PlayLog:     ( a1 n1 -- )    \ play log file a1,n1
                PlayClose: self
                2dup r/o open-file
                dup s" Key LOG file doesn't exist!" ?MessageBox
                if      drop 2drop 
                else    to playhndl
                        logfilebuf place
                        logfilebuf to logfilename
                        logfilebuf count "minus-ext" dup 3 - 0MAX /string
                        number?                         \ if a numbered keyfile
                        if      drop 1+ to log#         \ then set log number
                        else    2drop                   \ else discard it
                        then
                then
                ;M

128 bytes keybuf

: get1line      ( -- )
                keybuf 128 erase
                begin   keybuf count + 1 playhndl read-file
                        dup s" Read Error on Key Log file!" ?MessageBox
                        if      CloseLog: self
                        then
                        0>
                        keybuf count + c@ 0x0A <> and
                while   1 keybuf c+!
                repeat                  \ repeat till
                keybuf count upper ;

: get1hexline   ( -- )
                begin   get1line
                        keybuf c@ 0=            \ end of file or
                        keybuf 1+ c@ bl <> or   \ line starts with non-blank
                until   ;

: chartokey     ( -- char | -1 = end_of_file)
                keybuf c@
                if      keybuf count 2dup bl scan nip - nip keybuf c!
                        keybuf NUMBER drop
                else    PlayClose: self
                        -1
                then    ;

:M PlayKey:     ( -- char )
                playhndl
                if      get1hexline
                        chartokey
                else    -1
                then    ;M

:M Playing:     ( -- f1 )
                playhndl 0<>
                ;M

;Class

LogFile key-log-file "KEYS.LOG"

  defer log-more
        ' noop is log-more

0 value logging?

0 value ignored-keys            \ keys in this counted string are not logged

create view-ignored-keys        \ list of keys ignored by macro recording
                1 c,
                'S' +k_control +k_shift ,    \ stops macro recording, ignore it

view-ignored-keys to ignored-keys

: F"Log         ( -- )
                s" Function Key: " "Log: key-log-file ;

: log-emit      { logChar \ prepad$ -- }
                16 LocalAlloc: prepad$           \ a place to save here
                ignored-keys count logChar lscan nip ?EXIT   \ ignore this key and EXIT
                logging? 0= ?EXIT
                OpenLog:  key-log-file
                s" 0x" "Log: key-log-file
                pad 16 -  prepad$ 16 move               \ save bytes before pad
                hld  @ >r
                base @ >r HEX
                logChar 0 (d.) "Log:  key-log-file
                r> base !
                r> hld !
                prepad$ pad 16 - 16 move                \ restore bytes before pad
                      LogTab: key-log-file
                s"  \ " "Log: key-log-file

                logChar
                dup 0 +k_Control and
                if      s" Control " "Log: key-log-file
                        0 +k_Control -1 xor and
                then
                dup 0 +k_Shift and
                if      s" Shift " "Log: key-log-file
                        0 +k_Shift -1 xor and
                then
                case
                        k_f1    of F"Log s" F1"         "Log: key-log-file endof
                        k_f2    of F"Log s" F2"         "Log: key-log-file endof
                        k_f3    of F"Log s" F3"         "Log: key-log-file endof
                        k_f4    of F"Log s" F4"         "Log: key-log-file endof
                        k_f5    of F"Log s" F5"         "Log: key-log-file endof
                        k_f6    of F"Log s" F6"         "Log: key-log-file endof
                        k_f7    of F"Log s" F7"         "Log: key-log-file endof
                        k_f8    of F"Log s" F8"         "Log: key-log-file endof
                        k_f9    of F"Log s" F9"         "Log: key-log-file endof
                        k_f11   of F"Log s" F11"        "Log: key-log-file endof
                        k_f12   of F"Log s" F12"        "Log: key-log-file endof
                        k_left  of       s" Left"       "Log: key-log-file endof
                        k_right of       s" Right"      "Log: key-log-file endof
                        k_up    of       s" Up"         "Log: key-log-file endof
                        k_down  of       s" Down"       "Log: key-log-file endof
                       k_insert of       s" Insert"     "Log: key-log-file endof
                       k_Delete of       s" Delete"     "Log: key-log-file endof
                        k_Home  of       s" Home"       "Log: key-log-file endof
                        k_End   of       s" End"        "Log: key-log-file endof
                        k_PgUp  of       s" PgUp"       "Log: key-log-file endof
                        k_PgDn  of       s" PgDn"       "Log: key-log-file endof
                        k_Esc   of       s" ESC"        "Log: key-log-file endof
                        k_Tab   of       s" Tab"        "Log: key-log-file endof
                        0x20    of       s" Space"      "Log: key-log-file endof
                    k_BackSpace of       s" BackSpace"  "Log: key-log-file endof

                        dup
                        dup bl 1+ <     \ if less than printable character
                        if      s" CTRL " "Log: key-log-file
                                0x40 +
                        then
                        dup 0x80 <      \ if it is a printable character
                        if              \ just put it in the file
                                LogEmit: key-log-file
                        else            \ else display as a hex number
                                s" Hex: " "Log: key-log-file
                                pad 16 - prepad$ 16 move
                                hld  @ >r
                                base @ >r hex
                                0 (d.) "Log:  key-log-file
                                r> base !
                                r> hld !
                                prepad$ pad 16 - 16 move
                        then
                endcase log-more        \ hook for more info in logfile
                LogCr:    key-log-file
                CloseLog: key-log-file ;

: "log          ( a1 n1 -- )
                logging?
        if           OpenLog: key-log-file
                      LogTab: key-log-file
                s"  \ " "Log: key-log-file
                        "Log: key-log-file
                s"  "   "Log: key-log-file
                get-local-time
                time-buf dup >date" "Log: key-log-file
                            s"  - " "Log: key-log-file
                             >time" "Log: key-log-file
                LogCr:    key-log-file
                CloseLog: key-log-file
        else    2drop
        then    ;

defer menukey-more
        ' noop is menukey-more

: log-also      ( c1 -- c1 )
                dup log-emit ;

: logging-on    ( -- )
                logging? ?exit
                true to logging?
                s" Logging ON: " "log
                ['] log-also is menukey-more ;

: logging-off   ( -- )
                logging? 0= ?exit
                s" Logging OFF: " "log
                false to logging?
                ['] noop is menukey-more ;

: new-log       ( -- )
                NewLog: key-log-file 
                logging-on ;

: "new-log      ( a1 n1 -- )
                "NewLog: key-log-file
                logging-on ;

: play1key      ( -- )
                ekey? 0=
                if      PlayKey: key-log-file dup -1 =
                        if      drop
                                ['] noop is auto_key    \ disable playkey
                                ['] noop is auto_key?   \ disable playkey?
                        else    pushkey
                        then
                then    ;

 0 value play0cnt
50 value playrate

: play1key?     ( f1 -- f1 )
                dup 0=
                if      play0cnt 1 <
                        if      play1key
                                playrate 0max to play0cnt
                        else    -1      +to play0cnt
                        then
                then    ;

: "playkeys     ( a1 n1 -- )
                logging-off                     \ disble current logging
                0 to play0cnt
                0x0 pushkey                     \ kick start playkeys
                PlayLog: key-log-file
                ['] play1key  is auto_key
                ['] play1key? is auto_key? 
                play1key ;

FileOpenDialog PlayLog "Play Key Log File" "Log Files (*.LOG)|*.LOG|All Files (*.*)|*.*|"
FileSaveDialog NewLog "New Log File" "Log Files (*.LOG)|*.LOG|All Files (*.*)|*.*|"

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\       Macro Functions
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

: replay-macro  ( -- )
                Playing: key-log-file 0=        \ replay only if not already 
                                                \ playing some keys
                if      GetLogName: key-log-file  "playkeys
                then    ;

: con-play-macro ( -- )
                conhndl Start: PlayLog dup c@
                if      count "playkeys
                else    drop
                then    ;


: con-new-macro ( -- )
                conhndl Start: NewLog dup c@
                if      count "new-log
                else    drop
                then    ;

: start/stop-macro ( -- )
                logging?
                if      logging-off
                        s" Finished Recording Macro"
                else    new-log
                        s" Starting New KEYS.LOG"
                then    "message 300 ms message-off ;

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ definer      name         title            prompt                        ok     cancel   Option
\                            text             text                        text     text     text
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

NewEditDialog RepeatMacroDlg "Repeat Macro" "Repeat Macro how many times:" ""       ""       ""

0 value #repeating-macro

\ In this definition, 'parent' can be either the parent window, or CONHNDL
\ in the case of the forth console window

: repeat-amacro { parent \ repeat$ -- }
                32 localAlloc: repeat$
                #repeating-macro 0=
                if      s" 1" repeat$ place
                        repeat$ dup parent Start: RepeatMacroDlg
                        if      count number? 2drop 0max 999 min
                        else    drop 0
                        then    to #repeating-macro
                then    
                #repeating-macro
                if      GetLogName: key-log-file PlayLog: key-log-file
                        begin   PlayKey: key-log-file dup -1 <>
                        while   pushkey
                        repeat  drop
                        #repeating-macro 1- to #repeating-macro
                        #repeating-macro
                        if      'R' +k_control +k_shift pushkey  \ repeat cmd
                        then
                then    ;

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\       paste clipboard into the keyboard
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

0 value paste-ptr
0 value paste-len
0 value paste-off
0 value paste-hdl

: macro/paste-close   ( -- )    \ turn off macros and paste on error
                ['] noop is auto_key                    \ disable playkey
                ['] noop is auto_key?                   \ disable playkey?
                PlayClose: key-log-file
                paste-hdl 0= ?exit
                paste-hdl call GlobalUnlock drop        \ unlock it, done
                0 to paste-hdl                          \ clear handle
                Call CloseClipboard ?win-error ;        \ close clipboard

forth-io-chain chain-add macro/paste-close              \ turn off paste on error

: paste1key     ( -- )
                ekey? 0=
                if      paste-off paste-len u< 0=
                        if      macro/paste-close
                        else    paste-ptr paste-off + c@        \ get next key
                                1 +to paste-off                 \ skip paste it
                                dup 0x0D =                      \ if its CR
                                if      1 +to paste-off         \ skip LF
                                then    pushkey                 \ push the key
                        then
                then    ;

: paste1key?    ( f1 -- f1 )
                dup 0=
                if      play0cnt 1 <
                        if      paste1key
                                playrate 0max to play0cnt
                        else    -1      +to play0cnt
                        then
                then    ;

: paste-load    ( -- )
                conhndl call OpenClipboard 0=
        if      beep
        else    CF_TEXT call GetClipboardData ?dup
           if   to paste-hdl
                paste-hdl call GlobalLock abs>rel to paste-ptr       \ lock memory
                paste-ptr 1000000 2dup 0 scan nip - to paste-len to paste-ptr
                paste-len
                if      0 to paste-off
                        0 to play0cnt
                        0x0 pushkey                     \ kick start playkeys
                        ['] paste1key  is auto_key
                        ['] paste1key? is auto_key?
                        paste1key
                else    beep
                        0 to paste-ptr
                        0 to paste-len
                        0 to paste-off
                        paste-hdl call GlobalUnlock drop \ unlock it, done
                        call CloseClipboard ?win-error
                then                                    \ cleanup for clipboard
          else  beep
                Call CloseClipboard ?win-error
          then
        then    ;

: win-paste-load ( wParam lParam -- wParam lParam )
                ed-ptr   0= ?EXIT               \ exit if no shared memory
                sys-free 0= ?EXIT               \ exit if no heads are present
                over WM_PASTELOAD =             \ tell Forth to PASTE and LOAD
                if      paste-load
                then    ;

forth-msg-chain chain-add win-paste-load

: copy-console  { \ gblhndl gblptr b/l l/s len -- } \ Copy text to Windows clipboard
                marked? 0=
                if      beep EXIT
                then
                conhndl call OpenClipboard 0=
        if      beep
        else    getmaxcolrow to l/s                     \ lines per screen (really total)
                             to b/l                     \ bytes per line
                 b/l 2 + l/s * 2 cells +                \ max buffer size needed
                GMEM_MOVEABLE GMEM_DDESHARE or          \ flags
                call GlobalAlloc to gblhndl             \ allocate a buffer
                gblhndl call GlobalLock abs>rel to gblptr \ lock memory

                getxy nip getrowoff + l/s min to l/s    \ adjust to lines needed
                0 to len

                l/s 0
                ?do     i mkstlin mkedlin between       \ if its a line we want
                        if      i mkstlin =
                                if      i b/l * &the-screen +
                                        i mkedlin =
                                        if      b/l -trailing mkedcol min
                                                mkstcol /string >r
                                                gblptr len + r@ move    \ append text
                                                r> +to len              \ adjust length
                                        else    b/l mkstcol /string -trailing >r
                                                gblptr len + r@ move    \ append text
                                                r> +to len              \ adjust length
                                                crlf$ count             \ srlf string
                                                gblptr len + swap move  \ append crlf
                                                2 +to len               \ adjust length
                                        then
                                else    i mkedlin =
                                        if      i b/l * &the-screen +
                                                mkedcol b/l min -trailing >r
                                                gblptr len + r@ move    \ append text
                                                r> +to len              \ adjust length
                                        else    i b/l * &the-screen +   \ the line
                                                b/l -trailing >r        \ remove extra spaces
                                                gblptr len + r@ move    \ append text
                                                r> +to len              \ adjust length
                                                crlf$ count             \ srlf string
                                                gblptr len + swap move  \ append crlf
                                                2 +to len               \ adjust length
                                        then
                                then
                        then
                loop    gblptr len + off                \ null terminate buffer

                gblhndl call GlobalUnlock drop          \ unlock the buffer
                call EmptyClipboard ?win-error          \ clear out the clipboard
                                                        \ pass to windows
                gblhndl CF_TEXT call SetClipboardData ?win-error
                call CloseClipboard ?win-error
        then    ;

: cut-console   ( -- )
                marked?
                if      beep
                else    mark-all
                        copy-console
                        cls
                then    ;

MODULE          \ finish up the module

