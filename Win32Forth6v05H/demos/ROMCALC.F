\ ROMCALC.F            calculator with Roman numbers            by Lars Krueger

(( I'm not fully sure how the Romans entered their numbers into their
electronic pocket calculators so I chose this method:
As you enter the numbers they are added to the number in the display.
The numbers starting with 4,5,9 and 1 are handled separately. (see graph below)
The --- Button multiplies the displayed number by 1000.
After pressing the ENTER-key the number is pushed onto the number stack in the
second window. The rest is a regular stack calculator.

The following rules are used for constructing the Roman numbers:
M       1000
CM       900
D        500
CD       400
C        100
XC        90
L         50
XL        40
X         10
IX         9
V          5
IV         4
I          1

The rule that only three chararacters of the same number ( i.e. III ) can appear
in sequence is not handled by the input. I tried it but the graph for the
state machine was much too large for a single A4 sheet of paper. If there is
a shorter solution than the state machine for this I can't see it in the moment.
Anyway, the rule is used in the output.
A further less known rule is used to display larger numbers: Factors of one
thousand are displayed by painting a line above the factor. Factor of one
million are displayed by two and of one billion by three lines.
This rule applies only to numbers above 3999 ( the largest number without line).
For convenience the numbers are shown as arabic ( in other words normal )
numbers.
Examples:

123     CXXIII
2038    MMXXVIII
1996    MCMXCVI
3999    MMMCMXCIX

        __
4567    IVDLXVII


        =
1000002 III

        =__
1004567 IIVDLXVII


    +-------------------------------------+<--+
    |                                     |   |
    |                              else   |   |
    |                            +--------+   |
    v                            |            |
  +---+      C,100    +---+      |  M, 800    |
  | 0 |---+---------->| 1 |------+------------+
  +---+   |           +---+      |            ^
          |                      |  D, 300    |
          |                      +------------+
          |                                   ^
          |                                   |
          |                        else       |
          |                      +------------+
          |                      |            ^
          |  X,10     +---+      | C, 80      |
          +---------->| 2 |------+------------+
          |           +---+      |            ^
          |                      | L, 30      |
          |                      +------------+
          |                                   ^
          |                                   |
          |                        else       |
          |                      +------------+
          |                      |            ^
          |  I,1      +---+      | X, 8       |
          +---------->| 3 |------+------------+
          |           +---+      |            ^
          |                      | V, 3       |
          |                      +------------+
          |                                   ^
          |  M, 1000                          |
          +-----------------------------------+
          |                                   ^
          |  D,  500                          |
          +-----------------------------------+
          |                                   ^
          |  L,   50                          |
          +-----------------------------------+
          |                                   ^
          |  V,    5                          |
          +-----------------------------------+


Graph of the finite state machine controlling the number input of the
calculator.
Initial state is 0.

))


0 value is-standalone
\ cls
.( Do you want this program becoming a standalone executable? )
key dup ascii y = swap ascii Y = or to is-standalone
\ keep this in one line or use nostack1
is-standalone #if
.( Yes. )
#else
.( No. )
#endif

\ This is the part that actually shows the Roman numbers.
\ We need this three times therefore it is not coupled to an object.

create romziff ," M"  align 1000 , \ This assumes that a cells is larger than
               ," CM" align  900 , \ 2 byte soo this is makes it not a
               ," D"  align  500 , \ Standard program according to the
               ," CD" align  400 , \ ANS-document included.
               ," C"  align  100 ,
               ," XC" align   90 , \ Anyway all the Windows stuff is
               ," L"  align   50 , \ non-standard too so nothing to worry about.
               ," XL" align   40 ,
               ," X"  align   10 , \ We keep it simpler this way. First cell
               ," IX" align    9 , \ string, second cell number.
               ," V"  align    5 ,
               ," IV" align    4 ,
               ," I"  align    1 ,


WinDC globalDC

: ((.rom)) { n x y -- }
  13 0 do
    romziff i 2* 1+ cells + @                                                   ( diff )
    0
    begin                                                                       ( diff cnt )
      over                                                                      ( diff cnt diff )
      n                                                                         ( diff cnt diff n )
      <=
      if                                                                        ( diff cnt )
        \ Write the string
        romziff i 2* cells + count                                              ( diff cnt addr len )
        pad +place                                                              ( diff cnt )
        \ calculate n-diff
        n                                                                       ( diff cnt n )
        2 pick                                                                  ( diff cnt n diff )
        - to n                                                                  ( diff cnt )
      then
      1+ dup
      3 >=
    until                                                                       ( diff cnt )
    2drop
  loop
  \ Now write the string to hdc.
  x y pad count TextOut: globalDC
  ;

: 1000^n ( n -- 1000^n )
  case
    0 of          1 endof
    1 of       1000 endof
    2 of    1000000 endof
    3 of 1000000000 endof
  endcase ;

: (rom.streich) { x y l t -- l } \ Paints t lines of length l above x y
  t 1+ 1 do
    y i 3 * -                                                                   ( y-i )
    dup x                                                                       ( yi yi x )
    swap MoveTo: globalDC                                                       ( yi )
    x l +                                                                       ( yi xl )
    swap LineTo: globalDC
  loop
  l
  ;

: (.rom) ( n x y t -- n%1000^tiefe x+? y | ) \ Paints a number ( 1 and 999 )
                                             \ * 1000^t. Corrects x,y and n.
  dup
  0= if
    drop                                                                        ( n x y )
    ((.rom))
  else
    dup 1000^n                                                                  ( n x y t 1000^t )
    dup                                                                         ( n x y t 1000^t 1000^t)
    5 pick                                                                      ( n x y t 1000^t 1000^t n )
    < if                                                                        ( n x y t 1000^t )
      4 roll                                                                    ( x y t 1000^t n )
      swap 2dup                                                                 ( x y t n 1^t n 1^t )
      /                                                                         ( x y t n 1^t n/1^t )
      5 pick                                                                    ( x y t n 1^t n/1 x )
      5 pick                                                                    ( x y t n 1^t n/1 x y )
      ((.rom))                                                                  ( x y t n 1^t )
      mod                                                                       ( x y t n )
      3 pick 3 pick                                                             ( x y t n x y )
      pad count GetTextExtent: globalDC drop                                    ( x y t n x y l )
      4 roll                                                                    ( x y n x y l t )
      (rom.streich)                                                             ( x y n l )
      \ x+=l
      3 roll                                                                    ( y n l x )
      +                                                                         ( y n x )
      2 roll                                                                    ( n x y )
      s" " pad place \ We clean pad here. Otherwise the sign would be lost.
    else  \ no line above, just clean up                                        ( n x y t 1000^t )
      2drop
    then
  then ;

: .rom ( n x y hdc -- ) \ Print the positive number n at x y in device context
                        \ hdc. n MUST be positive and y must be larger than 10 if
                        \ n can get larger than 1000.
  PutHandle: globalDC                                                           ( n x y )
  2 pick dup                                                                    ( n x y n )
  0< if         \ negative number
    drop
    s" -"
  else
    0= if
      s" ----"  \ zero ( You can put here i.e. s" no number available". )
    else
      s" "      \ anything else therefore positive numbers
    then
  then
  pad place                                                                     ( n x y )
  2 roll abs                                                                    ( x y n )
  2 roll 2 roll 2 pick                                                          ( n x y n )
  3999 <=
  if
    ((.rom)) \ In case of numbers below 4000 we can leave out the lines.
  else
    3                                                                           ( n x y 3 )
    (.rom)
    2 (.rom)                                                                    ( n x y )
    1 (.rom)
    0 (.rom)
  then
  ;


\ Keep these in this order !!! These are used as the rows in the state-machine
\ table.
100 constant ID-M
101 constant ID-D
102 constant ID-C
103 constant ID-L
104 constant ID-X
105 constant ID-V
106 constant ID-I

\ If you like mix these codes up, change the numbers or not or whatever.
107 constant ID-_
108 constant ID-+
109 constant ID--
110 constant ID-*
111 constant ID-/
112 constant ID-=
113 constant ID-NEG
114 constant ID-0
115 constant ID-Z
116 constant ID-A

:Class AboutWindow <Super Window
\ This is largely copied from winhello.f

int counter                     \ a local variable for a counter

:M StartSize:   ( -- w h )      \ the screen origin of our window
                300 150
                ;M

:M StartPos:    ( -- x y )      \ the width and height of our window
                200 100
                ;M

:M WindowStyle: ( -- style )
                WS_POPUP WS_BORDER or  \ No WS_SYSTEMMENU because we don't
                                       \ want to disturb our on/off switch.
                WS_CAPTION or
                ;M

:M WindowTitle: z" About" ;M            \ " What my name ?" asked the window.

:M On_Paint:    5  5 s" Roman number calculator" TextOut: dc
                \ We can change these three to one StaticControl but this
                \ is easier.
                5 30 s" by Lars Krueger" TextOut: dc
                5 55 s" email: ai108@rz.tu-ilmenau.de" TextOut: dc
                counter 5 90 GetHandle: dc .rom
                5 110 s" seconds open." TextOut: dc
                ;M

:M On_Init:     ( -- )          \ things to do at the start of window creation
                On_Init: super             \ do anything superclass needs
                0 to counter               \ then initialize counter is zero
                0 1000 1 hWnd Call SetTimer drop
                ;M

:M WM_TIMER     ( h m w l -- res ) \ handle the WM_TIMER events
                1 +to counter              \ bump the counter
                Paint: self                \ refresh the window
                0 ;M

:M On_Done:     ( -- )          \ things to do before program termination
                1 hWnd Call KillTimer drop \ destroy the timer, we are done
                On_Done: super             \ then do things superclass needs
                ;M

:M Start:       ( -- )         \ create a new window object
                s" RomAboutWindow" SetClassName: self
                Start: super
                ;M
;Class

10 constant stackmax

:Class StackWindow <Super Window

int stackptr
stackmax 1+ cells bytes nstack

:M StartSize:   ( -- w h )      \ the screen origin of our window
                250 190
                ;M

:M StartPos:    ( -- x y )      \ the width and height of our window
                48 200
                ;M

:M WindowStyle: ( -- style )
                WS_POPUP WS_BORDER or
                WS_CAPTION or
                ;M

:M WindowTitle: z" ROM-Calc-Stack" ;M

:M Start:       ( -- )         \ create a new window object
                s" RomStackWindow" SetClassName: self
                Start: super
                ;M

:M On_Init:     ( -- )          \ things to do at the start of window creation
                0 to stackptr
                On_Init: super
                ;M

:M On_Paint:    stackptr 10                       ( n 10 )
                \ From n down to zero
                begin                                    ( n y )
                  over
                while
                   \ Paint the number at 5 y in roman numbers
                   \ and at 5 y+20 in arabic numbers
                   over                                  ( n y n )
                   1- cells nstack + @
                   5 2 pick                              ( n y v 5 y )
                   GetHandle: dc .rom
                   20 +                                  ( n y+20 )
                   over 1- cells nstack + @              ( n y v )
                   5 2 pick                              ( n y v 5 y )
                   2 pick abs 0                          ( n y v 5 y v 0 )
                   <# #s                                 ( n y v 5 y v 0 )
                   4 roll sign #>
                   TextOut: dc                           ( n y )
                   25 +
                   swap 1- swap
                repeat
                2drop
                ;M

:M PushVal:     ( n -- ) \ No need to ask for stackptr smaller than stackmax
                         \ because the button will be disabled.
                nstack stackptr cells+ !
                stackptr 1+ stackmax min to stackptr    \ prevent stack overflow
                Paint: self
                ;M

:M PopVal:      ( -- n ) \ See above but larger than zero.
                stackptr 1- 0 max to stackptr           \ prevent stack underflow
                nstack stackptr cells+ @
                Paint: self
                ;M

:M CanCalc:     ( -- flag )
                stackptr 2 >= ;M

:M ClearStack:  0 to stackptr
                Paint: self
                ;M

:M CanEnter:    stackptr stackmax < ;M

;Class

:Object CalcWindow     <Super Window
ButtonControl M-Button
ButtonControl D-Button
ButtonControl C-Button
ButtonControl L-Button
ButtonControl X-Button
ButtonControl V-Button
ButtonControl I-Button
ButtonControl _-Button
ButtonControl +-Button
ButtonControl --Button
ButtonControl *-Button
ButtonControl /-Button
ButtonControl =-Button
ButtonControl 0-Button
ButtonControl N-Button
ButtonControl Z-Button
ButtonControl A-Button
int about
StackWindow sw
int state
int display

:M StartSize:   ( -- w h )      \ the screen origin of our window
                250 190
                ;M

:M StartPos:    ( -- x y )      \ the width and height of our window
                304 200
                ;M

:M WindowStyle: ( -- style )
                WS_POPUP WS_BORDER or
                WS_CAPTION or
                WS_SYSMENU or
                WS_MINIMIZEBOX or
                ;M

:M WindowTitle: z" ROM-Calc" ;M

:M Start:       ( -- )         \ create a new window object
                s" RomCalcWindow" SetClassName: self
                Start: sw               \ Start: causes a WM_MOVE message to be
                                        \ sent to the window. Therefore we need
                                        \ to start sw first or HWND of sw is
                                        \ not a valid handle. That would cause a
                                        \ Windowserror 87.
                Start: super
                FALSE Enable: sw        \ 8/7/96 tjz keep user from ckicking
                                        \ on the stack window
                SetFocus: self
                0 to about
                ;M

:M On_Paint:    5 5 MoveTo: dc \ A rectangle
                245 5 LineTo: dc
                245 60 LineTo: dc
                5 60 LineTo: dc
                5 5 LineTo: dc
                display 20 20 GetHandle: dc .rom
                20 40 display abs 0 <# #s display sign #> TextOut: dc
                ;M

:M On_Init:     ( -- )          \ things to do at the start of window creation
                On_Init: super             \ do anything superclass needs
                \ Our Buttons. We need to set the ID before we create the
                \ control because Windows wants tho know the ID.
                ID-M            SetID:  M-Button
                self            Start:  M-Button
                05  80 30 20     Move:  M-Button
                s" M"         Settext:  M-Button

                ID-D            SetID:  D-Button
                self            Start:  D-Button
                40  80 30 20     Move:  D-Button
                s" D"         Settext:  D-Button

                ID-C            SetID:  C-Button
                self            Start:  C-Button
                75  80 30 20     Move:  C-Button
                s" C"         Settext:  C-Button

                ID-L            SetID:  L-Button
                self            Start:  L-Button
                05 105 30 20     Move:  L-Button
                s" L"         Settext:  L-Button

                ID-X            SetID:  X-Button
                self            Start:  X-Button
                40 105 30 20     Move:  X-Button
                s" X"         Settext:  X-Button

                ID-V            SetID:  V-Button
                self            Start:  V-Button
                75 105 30 20     Move:  V-Button
                s" V"         Settext:  V-Button

                ID-I            SetID:  I-Button
                self            Start:  I-Button
                05 130 30 20     Move:  I-Button
                s" I"         Settext:  I-Button

                ID-_            SetID:  _-Button
                self            Start:  _-Button
                40 130 65 20     Move:  _-Button
                s" ----"      Settext:  _-Button

                ID--            SetID:  --Button
                self            Start:  --Button
                125 105 40 20    Move:  --Button
                s" -"         Settext:  --Button

                ID-*            SetID:  *-Button
                self            Start:  *-Button
                125 130 40 20    Move:  *-Button
                s" *"         Settext:  *-Button

                ID-/            SetID:  /-Button
                self            Start:  /-Button
                125  80 40 20    Move:  /-Button
                s" /"         Settext:  /-Button

                ID-+            SetID:  +-Button
                self            Start:  +-Button
                125 155 40 20    Move:  +-Button
                s" +"         Settext:  +-Button

                ID-=            SetID:  =-Button
                self            Start:  =-Button
                05 155 100 20    Move:  =-Button
                s" ENTER"     Settext:  =-Button

                ID-0            SetID:  0-Button
                self            Start:  0-Button
                170  80 40 20    Move:  0-Button
                s" 0"         Settext:  0-Button

                ID-NEG          SetID:  N-Button
                self            Start:  N-Button
                170 105 40 20    Move:  N-Button
                s" +/-"       Settext:  N-Button

                ID-Z            SetID:  Z-Button
                self            Start:  Z-Button
                170 130 40 20    Move:  Z-Button
                s" CLR"       Settext:  Z-Button

                ID-A            SetID:  A-Button
                self            Start:  A-Button
                170 155 40 20    Move:  A-Button
                s" ~~~"       Settext:  A-Button

                0 to state
                0 to display
                false ID-+ EnableDlgItem: self
                false ID-- EnableDlgItem: self
                false ID-* EnableDlgItem: self
                false ID-/ EnableDlgItem: self
                on_init: sw
                ;M

:M On_Done:     ( -- )          \ things to do before program termination
                about if \ Is the About-box open?
                  Close: about
                  about Dispose
                then
                Close: sw
                On_Done: sw
                On_Done: super             \ then do things superclass needs
                ;M

create state-table
\ July 9th, 1997 - 8:56 tjz correction from Pierre Abbat
\ Within one state: <<new state>> , <<number to add>> ,
\ Key  State 0          State 1         State 2                 State 3
( M )  0 , 1000 ,       0 , 800 ,       0 , 0 , ( n.a. )        0 , 0 , ( n.a. )
( D )  0 ,  500 ,       0 , 300 ,       0 , 0 , ( n.a. )        0 , 0 , ( n.a. )
( C )  1 ,  100 ,       0 , 100 ,       0 , 80 ,                0 , 0 , ( n.a. )
( L )  0 ,   50 ,       0 ,  50 ,       0 , 30 ,                0 , 0 , ( n.a. )
( X )  2 ,   10 ,       2 ,  10 ,       0 , 10 ,                0 , 8 ,
( V )  0 ,    5 ,       0 ,   5 ,       0 ,  5 ,                0 , 3 ,
( I )  3 ,    1 ,       3 ,   1 ,       3 ,  1 ,                0 , 1 ,

(( Original ---
\ Within one state: <<new state>> , <<number to add>> ,
\ Key  State 0          State 1         State 2                 State 3
( M )  0 , 1000 ,       0 , 800 ,       0 , 0 , ( n.a. )        0 , 0 , ( n.a. )
( D )  0 ,  500 ,       0 , 300 ,       0 , 0 , ( n.a. )        0 , 0 , ( n.a. )
( C )  1 ,  100 ,       0 , 100 ,       0 , 80 ,                0 , 0 , ( n.a. )
( L )  0 ,   50 ,       0 ,  50 ,       0 , 30 ,                0 , 0 , ( n.a. )
( X )  2 ,   10 ,       0 ,  10 ,       0 , 10 ,                0 , 8 ,
( V )  0 ,    5 ,       0 ,   5 ,       0 ,  5 ,                0 , 3 ,
( I )  3 ,    1 ,       0 ,   1 ,       0 ,  1 ,                0 , 1 ,
))

( n.a. means: this is not allowed by rule )

: state-machine ( key -- ) \ Finds for (state,key) the newstate and the value to
                           \ add to display.
  \ key -> row in table
  ID-M - 8 cells *                           ( row-addr )
  state 2* cells state-table + + dup cell+   ( addr addr+cell )
  @ display dup 0< if
   swap -
  else
  +
  then
  to display
  @ to state
  ;

:M WM_COMMAND   ( wparam lparam -- res ) \ wparam contains the ID of the clicked
                                         \ control. Can be done too by using
                                         \ the SetFunc: method. This version
                                         \ will be longer ( more to type ).
                swap ( lparam wparam )
                case
                  ID-0 of 0 to display Paint: self endof
                  ID-NEG of display negate to display Paint: self endof
                  ID-= of
                    display PushVal: sw
                    0 to display
                    0 to state
                    Paint: self
                  endof
                  ID-Z of ClearStack: sw endof
                  ID-+ of PopVal: sw PopVal: sw + PushVal: sw endof
                  ID-- of PopVal: sw PopVal: sw swap - PushVal: sw endof
                  ID-_ of display 1000 * to display
                          0 to state
                          Paint: self
                       endof
                  ID-* of PopVal: sw PopVal: sw * PushVal: sw endof
                  ID-/ of PopVal: sw PopVal: sw swap dup 0= if
                       swap PushVal: sw
                     else
                       /
                     then
                     PushVal: sw
                  endof
                  ID-A of
                    about if
                      Close: about
                      about Dispose
                      0 to about
                    else
                      New> AboutWindow to about
                      Start: about
                    then
                  endof
                  ( lparam wparam )
                  dup state-machine
                  Paint: self
                endcase
                display abs
                2147483 <= ID-_ EnableDlgItem: self
                CanCalc: sw dup 2dup
                ID-+ EnableDlgItem: self
                ID-- EnableDlgItem: self
                ID-* EnableDlgItem: self
                ID-/ EnableDlgItem: self
                CanEnter: sw ID-= EnableDlgItem: self
                drop 0
                ;M

\ These two methods are corrections by Tom Zimmer. This WM_... inheritance is
\ cool. I had to correct them a bit ( stackeffects of WM_MOVE WM: super ).

:M WM_MOVE      ( wparam lparam -- res )
                WM_MOVE WM: Super               \ move my"self" first
                drop                            \ throw away result from Super
                GetWindowRect: self 2drop       \ returns calcwindows real global
                                                \ screen coordinates.
                swap StartSize: sw drop - 6 - swap \ then adjust "sw" left
                SetWindowPos: sw                   \ and set its position
                SetFocus: self  \ simplified, just sets the focus
                2drop                           \ get rid of wparam and lparam
                0 ;M                            \ and return a result

:M WM_CLOSE     ( -- )  \ allow close button to close windows, but doesn't quit
                Close: self
                WM_CLOSE WM: Super
[ is-standalone #if
                postpone bye   \ only needed for standalone program
#endif ]
                ;M


:M WM_SYSCOMMAND        ( wparam lparam -- res )
                        over
                        case
                          SC_MINIMIZE of
                              true Enable: sw \ Nessesary for iconizing. If left
                                              \ out, sw keeps a window
                              dup 2 pick  ( wp lp lp wp )
                              WM_SYSCOMMAND GetHandle: sw
                              call SendMessage drop
                              \ We become an icon, the stack becomes an icon.
                              false Enable: sw \ Get rid of the icon.
                          endof
                          SC_RESTORE of
                              true Enable: sw \ See above.
                              dup 2 pick  ( wp lp lp wp )
                              WM_SYSCOMMAND GetHandle: sw
                              call SendMessage drop
                              \ Both come back.
                              false Enable: sw
                          endof
                        endcase
                        hWnd WM_SYSCOMMAND 2swap DefWindowProc: self
                        ;M

;Object

is-standalone #IF
cr cr .( Creating a standalone executable.)

: do-romcalc    ( -- )
                decimal
                Start: CalcWindow ;

with-img ' do-romcalc turnkey romcalc  \ Create a standalone program with the
                                       \ "Main"-program or -word auto-start.

#ELSE
: DEMO          ( -- )
                Start: CalcWindow
                cr ." Type UNDEMO to finish or close window " ;

: UNDEMO        ( -- )
                Close: CalcWindow ;

cr
.( Type DEMO to start the calculator )

#ENDIF


