\ Graphics.f  Howerd Oakford  2021 Nov 11  Windows bitmap graphics

only forth also definitions

1280 value screen-mwidth
1024 value screen-mheight
640 to screen-width
640 to screen-height


\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ \\\ Simple Statusbar Class
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

:Class Statusbar  <Super Child-Window

:M Start:       ( hParent -- )   \ creates an empty statusbar in parent window
                to Parent
                create-child-window to hWnd
                SW_SHOWNORMAL Show: self
;M

:M StartSize:  ( - x y )
\ width is the width of the parent window, height is 20
                StartSize: Super drop 32 
;M

:M ClassInit:   ( -- )   \ initialize class
                ClassInit: super
                s" msctls_statusbar32" WindowClassName place
;M

;Class

\ ---------------------------------------------------------------
\       Define the BIT-WINDOW global drawing functions
\ ---------------------------------------------------------------

Windc demo-dc

  2 value bit-originx
  2 value bit-originy
  0 value VGA-X                 \ VGA x coordinate in pixels
  0 value VGA-Y                 \ VGA y coordinate in pixels
  0 value LINE-VALUE
  0 value walking?
  0 value line-count
  0 value save-count
  0 value do-printing?

-1 value prev-x
-1 value prev-y

: moveto        ( x y -- )
                0max screen-height 4 - min swap 0max screen-width 4 - min swap
                bit-originy + swap bit-originx +  swap
                over prev-x = over prev-y = and
                IF      2drop
                ELSE    2dup to prev-y
                             to prev-x
                        2dup PrinterMoveTo: ThePrinter
                                    MoveTo: demo-dc
                THEN    ;

: lineto        ( x y -- )
                0max screen-height 4 - min swap 0max screen-width 4 - min swap
                bit-originy + swap bit-originx +  swap
                over prev-x = over prev-y = and
                IF      2drop
                ELSE    2dup to prev-y
                             to prev-x
                        2dup PrinterLineTo: ThePrinter
                                    LineTo: demo-dc
                then
                1 +to line-count ;

: line          ( x1 y1 x2 y2 -- )
                2swap moveto lineto ;

: line-color    ( color_object -- )
                ?ColorCheck
                dup to line-value
                dup
                case    black   of white        endof
                        white   of black        endof
                        yellow  of blue         endof
                                   dup
                endcase PrinterLineColor: ThePrinter
                               LineColor: demo-dc ;

\ ---------------------------------------------------------------
\       Define the BIT-WINDOW window class
\ ---------------------------------------------------------------

:Class bit-window  <super child-window

int vga-bitmap
int SlowCounter         \ increments when the window is updated

:M On_Paint:    ( -- )
   SRCCOPY 0 0 GetHandle: demo-dc GetSize: self 0 0 BitBlt: dc
 \  DisplayCurves
   On_Paint: super
;M

:M WM_CREATE    ( hwnd msg wparam lparam -- res )
                get-dc
                0 call CreateCompatibleDC PutHandle: demo-dc
                screen-mwidth screen-mheight CreateCompatibleBitmap: dc
                to vga-bitmap
                vga-bitmap             SelectObject: demo-dc drop
                OEM_FIXED_FONT    SelectStockObject: demo-dc drop
                WHITE_PEN         SelectStockObject: demo-dc drop
                WHITE                    SetBkColor: demo-dc
                BLACK                  SetTextColor: demo-dc
                0 0 screen-mwidth screen-mheight WHITE FillArea: demo-dc
   0 to SlowCounter               \ initialize counter to zero
\   0 500 1 hWnd Call SetTimer drop 	\ kick off a 2Hz timer
   0 200 1 hWnd Call SetTimer drop \ init timer to a 200 ms rate
               release-dc
                0 ;M

:M On_Done:     ( -- )
   1 hWnd Call KillTimer  drop		\ destroy the timer
                vga-bitmap call DeleteObject drop
                0 to vga-bitmap
                On_Done: super
                ;M

:M WM_MOUSEMOVE ( h m w l -- res )
                set-mousexy
                MK_LBUTTON and
                IF      track-func null-check execute
                then
                0 
;M

:M WM_LBUTTONDOWN
   Beep
\   TransferFile
   Paint: self
;M

:M WM_RBUTTONDOWN
\  Beep
\  DisplayCurves
\   TransferFile
   Paint: self
;M

:M WM_TIMER	( h m w l - res )
   1 +to SlowCounter		\ increment the counter
   Paint: self
   0
;m

;Class

Windc fill-dc

:Class fill-window  <super child-window

int fill-bitmap

:M On_Paint:    ( -- )
   On_Paint: super
   SRCCOPY 0 0 GetHandle: fill-dc GetSize: self 0 0 BitBlt: dc
;M

:M WM_CREATE
                get-dc
                0 call CreateCompatibleDC       PutHandle: fill-dc
                150 screen-mheight CreateCompatibleBitmap: dc to fill-bitmap
                fill-bitmap                  SelectObject: fill-dc drop
                0 0 150 screen-mheight    DKGRAY FillArea: fill-dc
                release-dc
                0 ;M

:M On_Done:     ( -- )
                fill-bitmap call DeleteObject drop
                0 to fill-bitmap
                On_Done: super
                ;M

;Class

\ ---------------------------------------------------------------
\               Actual application section for DEMO
\ ---------------------------------------------------------------

0 value SEED1-SAVE
0 value SEED2-SAVE
0 value SEED3-SAVE

1 value dinc
0 value -hdots
0 value -vdots

ColorObject TheNextColor

: next-color    ( -- )
                BLACK line-color        \ make sure that TheNextColor object
                                        \ is not selected into the DC
                256 random 1+           \ before trying to create a new color
                256 random 1+
                256 random 1+ rgb
                NewColor: TheNextColor
                          TheNextColor line-color ;

: erase-demo    ( -- )
                0 0 screen-width screen-height WHITE PrinterFillArea: ThePrinter
                1 1 screen-width 2 - screen-height 2 - WHITE        FillArea: demo-dc
                seed1 to seed1-save     \ save this seed set in case we
                seed2 to seed2-save     \ want to print it
                seed3 to seed3-save
                0 to line-count ;


\ ---------------------------------------------------------------
\               Actual application section for LINEWALK
\ ---------------------------------------------------------------

0 value x1 0 value y1 0 value x2 0 value y2

: bounce_xy1    ( x2 y2 x1 y1 -- x2 y2 x1 y1 )
                swap    dup -hdots >= IF -1 TO x1  0 TO x2 next-color THEN
                        dup 1 <       IF  1 TO x1  0 TO x2 next-color THEN
                swap    dup -vdots >= IF -1 TO y1  0 TO y2 next-color THEN
                        dup 1 <       IF  1 TO y1  0 TO y2 next-color THEN ;

: bounce_xy2    ( x1 y1 x2 y2 -- x1 y1 x2 y2 )
                swap    dup -hdots >= IF -1 TO x2  0 TO x1 next-color THEN
                        dup 1 <       IF  1 TO x2  0 TO x1 next-color THEN
                swap    dup -vdots >= IF -1 TO y2  0 TO y1 next-color THEN
                        dup 1 <       IF  1 TO y2  0 TO y1 next-color THEN ;

200 value line_max
400 value lines_max
0 value line_limit

: limit_xy      ( x2 y2 x1 y1 -- x2 y2 x1 y1 )
                2swap over >r 2swap over r> - dup abs line_limit >
                IF      dup 0<
                        IF      1 TO x1 -1 TO x2
                        ELSE    1 TO x2 -1 TO x1
                        THEN    y1 0= y2 0= or
                        IF 2 RANDOM 2 RANDOM - TO y1 then
                        next-color
                THEN    drop
                2swap dup>r   2swap dup  r> - dup abs line_limit >
                IF      dup 0<
                        IF      1 TO y1 -1 TO y2
                        ELSE    1 TO y2 -1 TO y1
                        THEN    x1 0= x2 0= or
                        IF 2 RANDOM 2 RANDOM - TO x1 then
                        next-color
                THEN    drop ;

0 value cnt-down
3 value cnt-down-max

: draw-a-line   ( x y x y -- )
                cnt-down 0=
                IF      line
                        cnt-down-max TO cnt-down
                ELSE    -1 +to cnt-down
                        2drop 2drop
                THEN    ;

: draw_1line    ( x y x y -- x y x y )
                line_max RANDOM 10 max TO line_limit
                2 RANDOM 2 RANDOM - TO x1 2 RANDOM 2 RANDOM - TO y1
                2 RANDOM 2 RANDOM - TO x2 2 RANDOM 2 RANDOM - TO y2
                lines_max RANDOM 1+ 0
                DO      4dup draw-a-line
                        limit_xy
                        swap x1 + swap y1 + bounce_xy1 2swap
                        swap x2 + swap y2 + bounce_xy2 2swap
                        i 1+ RANDOM 15 and 0=
                        IF      next-color
                        THEN
                        key? ?leave
                LOOP    next-color ;


\ : (WinType) ( x y a n )  textout: demo-dc ;
\ ' (Wintype) is Htype


