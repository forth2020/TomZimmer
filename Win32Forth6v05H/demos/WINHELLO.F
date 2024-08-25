\ WINHELLO.F            Simple Windows Hello World              by Tom Zimmer

\ See also HELLO.F for a similar example that doesn't use objects

\ Define an object "HelloWindow" that is a super object of class "Window"

:Object HelloWindow     <Super Window

int counter                     \ a local variable for a counter

:M StartSize:   ( -- w h )      \ the screen origin of our window
                170 90
                ;M

:M StartPos:    ( -- x y )      \ the width and height of our window
                200 100
                ;M

:M WindowTitle: ( -- Zstring )          \ window caption
                z" Hello World"
                ;M

:M On_Paint:    { \ temp$ -- }  \ all window refreshing is done by On_Paint:
                MAXSTRING LocalAlloc: temp$
                s" Repainted "        temp$  place
                counter (.)           temp$ +place
                s"  times"            temp$ +place
                20 ( x ) 50 ( y )     temp$  count TextOut: dc
                20 ( x ) 20 ( y )  s" Hello World" TextOut: dc
                ;M

:M On_Init:     ( -- )          \ things to do at the start of window creation
                On_Init: super             \ do anything superclass needs
                0 to counter               \ then initialize counter is zero
                0 200 1 hWnd Call SetTimer drop \ init timer to a 200 ms rate
                ;M

:M WM_TIMER     ( h m w l -- res ) \ handle the WM_TIMER events
                1 +to counter              \ bump the counter
                Paint: self                \ refresh the window
                0 ;M

:M On_Done:     ( -- )          \ things to do before program termination
                1 hWnd Call KillTimer drop \ destroy the timer, we are done
                On_Done: super             \ then do things superclass needs
                ;M

;Object

: DEMO          ( -- )                  \ start running the demo program
                Start: HelloWindow ;

: UNDEMO        ( -- )                  \ close the demo window
                Close: HelloWindow ;

cr .( Type: DEMO to start, and: UNDEMO to stop) cr

