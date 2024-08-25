\ GCLOCK.SEQ  A simple Graphic Clock program  by Tom Zimmer & Robert Smith

only forth also definitions

1280 value screen-mwidth
1024 value screen-mheight
400 to screen-width
300 to screen-height

\ ---------------------------------------------------------------
\       Define the BIT-WINDOW global drawing functions
\ ---------------------------------------------------------------

Windc demo-dc

  2 value bit-originx           \ we have a two pixel border around the bitmap
  2 value bit-originy
  0 value VGA-X                 \ VGA x coordinate in pixels
  0 value VGA-Y                 \ VGA y coordinate in pixels

-1 value prev-x
-1 value prev-y

: moveto        ( x y -- )
                0max screen-height 4 - min swap 0max screen-width 4 - min swap
                bit-originy + swap bit-originx +  swap
                over prev-x = over prev-y = and
                IF      2drop
                ELSE    2dup to prev-y
                             to prev-x
                        MoveTo: demo-dc
                THEN    ;

: lineto        ( x y -- )
                0max screen-height 4 - min swap 0max screen-width 4 - min swap
                bit-originy + swap bit-originx +  swap
                over prev-x = over prev-y = and
                IF      2drop
                ELSE    2dup to prev-y
                             to prev-x
                        LineTo: demo-dc
                then    ;

: line          ( x1 y1 x2 y2 -- )
                2swap moveto lineto ;

: line-color    ( color_object -- )
                LineColor: demo-dc ;

\ ---------------------------------------------------------------
\       Define the BIT-WINDOW window class
\ ---------------------------------------------------------------

:Class bit-window  <super child-window

int vga-bitmap

:M On_Paint:    ( -- )
                SRCCOPY 0 0 GetHandle: demo-dc GetSize: self 0 0 BitBlt: dc
                ;M

:M Clear:       ( -- )
                0 0 screen-mwidth screen-mheight BLACK FillArea: demo-dc
                ;M

:M WM_CREATE    ( hwnd msg wparam lparam -- res )
                get-dc
                0 call CreateCompatibleDC PutHandle: demo-dc
                screen-mwidth screen-mheight CreateCompatibleBitmap: dc
                to vga-bitmap
                vga-bitmap             SelectObject: demo-dc drop
                OEM_FIXED_FONT    SelectStockObject: demo-dc drop
                WHITE_PEN         SelectStockObject: demo-dc drop
                BLACK                    SetBkColor: demo-dc
                WHITE                  SetTextColor: demo-dc
                0 0 screen-mwidth screen-mheight BLACK FillArea: demo-dc
                release-dc
                0 ;M

:M On_Done:     ( -- )
                vga-bitmap call DeleteObject drop
                0 to vga-bitmap
                On_Done: super
                ;M

;Class

\ ---------------------------------------------------------------
\ Menu and push button support
\ ---------------------------------------------------------------

MENUBAR Demo-Menu-bar

    POPUP "&File"
        MENUITEM     "E&xit          \tAlt-F4"  bye ;

ENDBAR

:Object GCLOCK  <super window

 bit-window vga-bit-window

 0 constant marginSize  \ sets the clock white margin size in pixels

 marginSize constant bitorigx
 marginSize constant bitorigy

 bitorigx marginSize + 1+ constant bitrightmargin
 bitorigx marginSize + 1+ constant bitbottommargin

:M On_Init:     ( -- )          \ initialize the class
                On_Init: super                  \ first init super class
                2    SetId: vga-bit-window      \ then the child window
                self Start: vga-bit-window      \ then startup child window
\                Demo-menu-bar  SetMenuBar:  self
                ;M

:M On_Done:     ( h m w l -- res )
                0 call PostQuitMessage drop
                On_Done: super
                0 ;M

:M WM_CLOSE     ( h m w l -- res )
                WM_CLOSE WM: Super
                bye
                0 ;M

:M Refresh:     ( -- )
                Paint: vga-bit-window
                ;M

:M StartSize:   ( -- width height )     \ starting window size
                480 480
                ;M

:M MinSize:     ( -- width height )     \ minimum window size
                100 100
                ;M

:M MaxSize:     ( -- width height )     \ maximum window size
                screen-mwidth screen-mheight
                ;M

:M WindowTitle: ( -- Zstring )          \ window caption
                z" WinClock"
                ;M

\ the l parameter has already been removed by WINDOW.F, and put
\ into Height and Width

:M On_Size:     ( h m w -- )                  \ handle resize message
                Clear: vga-bit-window
                bitorigx bitorigy
                Width  bitrightmargin  - dup to screen-width
                Height bitbottommargin - dup to screen-height
                Move: vga-bit-window
                ;M

;Object

: unload-clock  ( -- )
                DestroyWindow: GCLOCK ;

unload-chain chain-add-before unload-clock


create sintbl
          0 ,    25 ,    49 ,    74 ,    97 ,   120 ,   141 ,   160 ,
        178 ,   194 ,   207 ,   219 ,   228 ,   234 ,   238 ,   240 ,
        238 ,   234 ,   228 ,   219 ,   207 ,   194 ,   178 ,   160 ,
        141 ,   119 ,    97 ,    74 ,    49 ,    25 ,     0 ,   -25 ,
        -49 ,   -74 ,   -97 ,  -120 ,  -141 ,  -160 ,  -178 ,  -194 ,
       -207 ,  -219 ,  -228 ,  -234 ,  -238 ,  -240 ,  -238 ,  -234 ,
       -228 ,  -219 ,  -207 ,  -194 ,  -178 ,  -160 ,  -141 ,  -119 ,
        -97 ,   -74 ,   -49 ,   -25 ,     0 ,    25 ,    49 ,    74 ,
         97 ,   120 ,   141 ,   160 ,   178 ,   194 ,   207 ,   219 ,
        228 ,   234 ,   238 ,   240 ,   238 ,

320 value    center-x
175 value    center-y
240 value    scale-y

: >screenx      ( n1 -- n2 )
                screen-width 480 */ ;

: >screeny      ( n1 -- n2 )
                screen-width 480 */ ;

: makesin       ( -- )
                cr
                462 0
                do      i 0 d>f fsin f# 240.0 f* f>d 8 d.r
                        100 ms
                        10 ?cr
             6 +loop    ;

: xxy-scale     ( 6deg scale -- x1 y1 )
                >r
                dup       6 * 0 d>f fsin f# 240.0 f* f>d drop
                        >screenx dup r@  center-x */
                swap 1 and +        center-x +
                swap 15 + 6 * 0 d>f fsin f# 240.0 f* f>d drop
                        >screeny dup r>   scale-y */
                swap 1 and + negate center-y + ;

: xy-scale      ( 6deg scale -- x1 y1 )
                >r
                dup        sintbl +CELLS @ >screenx dup r@  center-x */
                swap 1 and +        center-x +
                swap 15 +  sintbl +CELLS @ >screeny dup r>   scale-y */
                swap 1 and + negate center-y + ;

-1 value last-hour
-1 value last-minute
-1 value last-hour-minute
-1 value last-second

 0 value this-hour
 0 value this-minute
 0 value this-second

: .sec          ( n1 -- )
                >r
                center-x center-y  r@    center-x 20 - xy-scale line
                r@ 1- 020 xy-scale r@    center-x 20 - xy-scale line
                r@ 1+ 020 xy-scale r@    center-x 20 - xy-scale line
                center-x center-y  r@ 1- 020           xy-scale line
                center-x center-y  r@ 1+ 020           xy-scale line
                r>drop ;

: .min          ( n1 -- )
                >r
                center-x center-y  r@           center-x dup 10 / - xy-scale line
                r@ 1- center-x 3 / xy-scale r@  center-x dup 10 / - xy-scale line
                r@ 1+ center-x 3 / xy-scale r@  center-x dup 10 / - xy-scale line
                center-x center-y  r@ 1-        center-x 3 /        xy-scale line
                center-x center-y  r@ 1+        center-x 3 /        xy-scale line
                r>drop ;

: .hr           ( n1 -- )
                >r
                center-x center-y   r@          center-x 2 / xy-scale line
                r@ 2 - center-x 3 / xy-scale r@ center-x 2 / xy-scale line
                r@ 2 + center-x 3 / xy-scale r@ center-x 2 / xy-scale line
                center-x center-y   r@ 2 -      center-x 3 / xy-scale line
                center-x center-y   r@ 2 +      center-x 3 / xy-scale line
                r>drop ;

: .second       ( -- )          \ draw second display
                black line-color
                last-second 60 mod .sec this-second TO last-second
                white line-color
                this-second 60 mod .sec ;

: .minute       ( -- )          \ draw minute display
                this-minute last-minute <>
                IF      black line-color
                        last-minute 60 mod .min
                        this-minute TO last-minute
                THEN
                ltgreen line-color
                this-minute 60 mod .min ;

: .hour         ( -- )          \ draw hour  display
                this-hour last-hour <>
                IF      black line-color
                        last-hour 5 * last-hour-minute 12 / + 60 mod .hr
                        this-hour   TO last-hour
                        this-minute TO last-hour-minute
                THEN
                ltblue line-color
                this-hour 5 * this-minute      12 /   + 60 mod .hr ;

: ?second       ( -- f )        \ f = true if second has changed.
                get-local-time
                time-buf 12 + w@ last-second <>
                IF      time-buf 12 + w@ TO this-second         \ seconds
                        time-buf 10 + w@ TO this-minute         \ minutes
                        time-buf  8 + w@ TO this-hour           \ hours
                        TRUE
                ELSE    FALSE
                THEN    ;

: init-vars     ( -- )
                ?second drop            \ to initialize LAST-SECOND
                this-second TO last-second
                this-minute TO last-minute
                this-minute TO last-hour-minute
                this-hour   TO last-hour ;

 1 value delay-ms
16 value cdiam
 0 value ccolor

create colors
        DKGRAY ,        RED ,           LTRED ,         GREEN ,
        LTGREEN ,       BLUE ,          LTBLUE ,        YELLOW ,
        LTYELLOW ,      MAGENTA ,       LTMAGENTA ,     CYAN ,
        LTCYAN ,        GRAY ,          WHITE ,         LTGRAY ,

: >color        ( n1 -- color_object )
                15 and colors  +cells @ ;

: show-circle   ( -- )
                1 +TO ccolor
                ccolor >color line-color
                60 0
                DO      I    cdiam xy-scale 2dup 1+ swap 1+ swap line
                LOOP    5 +TO cdiam
                cdiam center-x 30 - >
                IF      16 TO cdiam
                THEN    ;

: .hms          ( -- )
                .second         \ draw second
                .minute         \ draw minute
                .hour           \ draw hour
                show-circle ;   \ draw the circles

: show-border   ( -- )
                60 0
                do      white  line-color
                        i center-x  1 - xy-scale i 1+ center-x  1 - xy-scale line
                        i center-x 12 - xy-scale i 1+ center-x 12 - xy-scale line
                        i 5 mod
                        if      ltcyan line-color           \ 1 second markers
                                i center-x 12 - xy-scale i center-x 1 - xy-scale line
                        else    yellow line-color           \ 5 second markers
                                i center-x 20 - xy-scale i center-x 1 - xy-scale line
                        then
                loop    ;

: new-clock     ( -- )          \ draw a new clock,
                screen-width  2 / 1- TO center-x
                screen-height 2 / 1- TO center-y       \ calibrate screen center
                center-x center-x center-y */ TO scale-y          \ calibrate aspect ratio
                white line-color                        \ default color=white
                show-border
                show-circle ;                    \ display the circle

: show-time     ( -- )          \ show the time if it has changed
                ?second                         \ if second changed
                if      .hms                    \ and then the time
                then    ;

\ ---------------------------------------------------------------
\ Top Level program starts here
\ ---------------------------------------------------------------

: WinClock      { \ c-width c-height -- }
                Start: GCLOCK
                RANDOM-INIT                             \ initialize random numbers
                screen-width  2 / 1 - TO center-x
                screen-height 2 / 1 - TO center-y       \ calibrate screen center
                white line-color                        \ default color=white
                new-clock
                init-vars                       \ then initialize variable
                .hms                            \ show initial time
                screen-width  to c-width
                screen-height to c-height
                begin   c-width      c-height
                        screen-width screen-height d= 0=
                        if       1 to delay-ms
                                16 to cdiam
                                new-clock
                                screen-width to  c-width
                                screen-height to c-height
                        then
                        show-time               \ just keep showing the current time
                        show-circle
                        Refresh: GCLOCK
                        key? drop
                        delay-ms 1+ 200 min to delay-ms
                        delay-ms ms
                again   ;

 ' WinClock turnkey WinClock     \ build an application on disk
5 pause-seconds

