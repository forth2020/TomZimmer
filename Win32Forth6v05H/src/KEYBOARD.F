\ KEYBOARD.F            functon key constants           by Tom Zimmer

  cr .( Loading Function Key Words...)

: litkey        ( n1 -<name>- )
                constant ;

0x1B litkey K_ESC       0x09 litkey K_TAB       0x08 litkey K_BACKSPACE
0x0D litkey K_CR        0x0A litkey K_LF


: fkey          ( n1 -<name>- )
                function_mask or constant ;

0x01 fkey K_F1          0x02 fkey K_F2          0x03 fkey K_F3
0x04 fkey K_F4          0x05 fkey K_F5          0x06 fkey K_F6
0x07 fkey K_F7          0x08 fkey K_F8          0x09 fkey K_F9
0x10 fkey K_F10         0x11 fkey K_F11         0x12 fkey K_F12

: splkey        ( n2 -<name>- )
                special_mask or constant ;

0x00 splkey K_HOME      0x01 splkey K_END       0x02 splkey K_INSERT
0x03 splkey K_DELETE    0x04 splkey K_LEFT      0x05 splkey K_RIGHT
0x06 splkey K_UP        0x07 splkey K_DOWN      0x08 splkey K_SCROLL
0x09 splkey K_PAUSE     0x10 splkey K_PGUP      0x11 splkey K_PGDN


: +K_SHIFT      ( c1 -- c2 )  shift_mask or ;   \ add in shift bit
\ 07/18/95 08:56 tjz ALT keys are for Windows Use ONLY!"
: +K_ALT        ( c1 -- c2 )    alt_mask or ;   \ add in the Alt bit

: +K_CONTROL    ( c1 -- c2 )
                dup proc_mask 0x7FF or and upc 'A' 'Z' between
                if      0xFF1F and              \ handle control letters
                else    control_mask or         \ add in control bit
                then    ;

\S

The above words are used as shown in the following example:



: key_test      ( -- )
        begin   cr ." Press a key, Enter to stop: "
                key
        case    k_f1            of      ."  F1"                 endof
                k_f2            of      ."  F2"                 endof
                k_f3            of      ."  F3"                 endof
                k_f4            of      ."  F4"                 endof
                k_f5            of      ."  F5"                 endof
                k_f6            of      ."  F6"                 endof
                k_f7            of      ."  F7"                 endof
                k_f8            of      ."  F8"                 endof
                k_f9            of      ."  F9"                 endof
                k_f10           of      ."  F10"                endof
                k_f11           of      ."  F11"                endof
                k_f12           of      ."  F12"                endof
                k_f1 +k_control of      ."  Control F1"         endof
                k_f2 +k_control of      ."  Control F1"         endof
                k_f3 +k_control of      ."  Control F1"         endof
                k_f4 +k_control of      ."  Control F1"         endof
                k_f5 +k_control of      ."  Control F1"         endof
                k_f6 +k_control of      ."  Control F1"         endof
                k_f7 +k_control of      ."  Control F1"         endof
                k_f8 +k_control of      ."  Control F1"         endof
                k_f9 +k_control of      ."  Control F1"         endof
               k_f10 +k_control of      ."  Control F10"        endof
               k_f11 +k_control of      ."  Control F11"        endof
               k_f12 +k_control of      ."  Control F12"        endof
                k_f1 +k_shift   of      ."  Shift F1"           endof
                k_f2 +k_shift   of      ."  Shift F1"           endof
                k_f3 +k_shift   of      ."  Shift F1"           endof
                k_f4 +k_shift   of      ."  Shift F1"           endof
                k_f5 +k_shift   of      ."  Shift F1"           endof
                k_f6 +k_shift   of      ."  Shift F1"           endof
                k_f7 +k_shift   of      ."  Shift F1"           endof
                k_f8 +k_shift   of      ."  Shift F1"           endof
                k_f9 +k_shift   of      ."  Shift F1"           endof
               k_f10 +k_shift   of      ."  Shift F10"          endof
               k_f11 +k_shift   of      ."  Shift F11"          endof
               k_f12 +k_shift   of      ."  Shift F12"          endof
                          0x0D  of      ." Stopping" exit       endof
                                        dup h.
                                        ." Unknown key"
        endcase ."  pressed."
        again   ;



