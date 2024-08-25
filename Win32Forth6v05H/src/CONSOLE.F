<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN">
<HTML><HEAD>
<META http-equiv=Content-Type content="text/html; charset=windows-1252"></HEAD>
<BODY><PRE>((

The orginal version was posted by Alex McDonald
On:      &lt;Win32Forth@topica.com&gt;
Subject: Removing the C Wrapper code...
Date:    woensdag 5 juni 2002 0:49

J.v.d.Ven, December 9th, 2002 - 14:59
See the ** for the most significant changes.
Removed some variables which are not used.

December 16th, 2002 - 11:12
Changed the typedef structs for turnkey applications.

  replacement for Win32Forth console
  for details see winbase.h

  This code uses the console provided by Windows to replace the console
  created by the wrapper. There were two ways to undertake this code:

  1. intercept xcalls and leave existing code in place

  2. replace words like accept, type etc directly. This course chosen.

  Work is required on this code before it could be considered for
  inclusion in the kernel are many. Here are some:

Solved: 1. KEY? and EKEY? need work - there's a bug in the routine that
     can best be seen by running WORDS and halting - restarting it with
     the spacebar. Some of the keystrokes are being lost.

  2. There's an extra space appearing from somewhere on accept.
     Try pad 128 accept - the cursor is in col 2 instead of col 1

  3. No mouse support yet, all the mouse functionality is provided by
     the console so far

  4. Not all properties are supported, such as bold&gt; etc, and the font
     can only be changed from the properties page
     (click on system box|properties to change font)

Solved   5. More seriously, winlibx.f and other modules are required. This will
     make it difficult to port to fkernel.f

Solved  6. Loads of others, including stack warning messages, use of
     makedefer, my weird coding style, won't turnkey or application,

Added: 7. ^c leaves Forth. How can it be disabled?  ))


\ variables, supporting colon defs

false value xinit-console-done?        \  if we've already started console

variable &amp;xCharCount                   \  character count

  BACKGROUND_BLUE    BACKGROUND_GREEN      or
  BACKGROUND_RED or  BACKGROUND_INTENSITY  or
constant xDfltColours                   \ default black text on white background

2 PROC SetConsoleMode
: x_SetMode     ( n -- )                   \ set
                StdIn   call SetConsoleMode
                0= abort" SetConsoleMode failed"
                ;

: x_SetRaw      ( -- )                     \ set mode
                [ ENABLE_WINDOW_INPUT ENABLE_MOUSE_INPUT or literal ]
                x_SetMode
                ;

: x_SetCooked   ( -- )                     \ set mode
                [ ENABLE_LINE_INPUT ENABLE_ECHO_INPUT or
                ENABLE_PROCESSED_INPUT or literal ]
                x_SetMode
                ;

create &amp;xScrBuffInfo 24 allot               \ allocate buffer

&amp;xScrBuffInfo
      dup constant      &amp;xScrCols                   \ cols
  2 + dup constant      &amp;xScrRows                   \ rows
  2 + dup constant      &amp;xCsrCol                    \ cursor column
  2 + dup constant      &amp;xCsrRow                    \ cursor row
  2 + dup constant      &amp;xAttrib                    \ attribute
  2 + dup constant      &amp;xWLeft                     \ Window left
  2 + dup constant      &amp;xWTop                      \ Window top
  2 + dup constant      &amp;xWRight                    \ Window right
  2 + dup constant      &amp;xWBottom                   \ Window bottom
  2 + dup constant      &amp;xMaxCols                   \ max cols
  2 +     constant      &amp;xMaxRows                   \ max rows

2 PROC GetConsoleScreenBufferInfo
: x_GetScrBuffInfo ( -- )                   \ fill buffinfo
                &amp;xScrBuffInfo rel&gt;abs
                StdOut
                call GetConsoleScreenBufferInfo
                0= abort" GetConsoleScreenBufferInfo failed" ;

: x_ScrColsRows ( -- cols rows )            \ buffer cols &amp; rows
                &amp;xScrCols w@
                &amp;xScrRows w@ ;

: x_CsrColRow   ( -- col row )              \ cursor col &amp; row
                x_GetScrBuffInfo
                &amp;xCsrCol w@
                &amp;xCsrRow w@ ;

: x_Attrib      ( -- n )                    \ char attributes
                x_GetScrBuffInfo
                &amp;xAttrib w@ ;

: x_ScrInCons   ( -- l t r b )    \ left top right bottom buffer in window
                x_GetScrBuffInfo
                &amp;xWLeft w@
                &amp;xWTop w@
                &amp;xWRight w@
                &amp;xWBottom w@ ;

1 PROC SetConsoleTitleA
: xSetTitle     ( zaddr -- )                \  sets title of screen
                rel&gt;abs call SetConsoleTitleA
                0= abort" SetConsoleTitle failed" ;

: xSetTitleReady  ( -- )                    \  set title to ready
               \ z" Win32Forth XConsole " xSetTitle
                ; \ ready

: xSetTitleWait  ( -- )                    \  set title to Wait
               \ z" Win32Forth XConsole " xSetTitle
                ; \ Wait...


2 PROC SetConsoleTextAttribute
5 PROC FillConsoleOutputAttribute
: x_SetFgBg     ( n -- )                    \ set colours of screen
                dup&gt;r  StdOut  call SetConsoleTextAttribute
                0= abort" SetConsoleTextAttribute failed"
                &amp;xCharCount rel&gt;abs
                0                          \ note - that's 0 0 in coords
                x_ScrColsRows *            \ total length of buffer
                r&gt;                         \ colours required
                StdOut
                call FillConsoleOutputAttribute
                0= abort" FillConsoleOutputAttribute failed"
                ;

\  override for gotoxy
\  : _GOTOXY       ( x y -- )
\                 SWAP gotoxy_x XCALL DROP ;

2 PROC SetConsoleCursorPosition
: x_gotoxy      ( x y -- )
                word-join               \  create x.y coords (both 16bit values)
                StdOut                  \  std out
                call SetConsoleCursorPosition
                0= abort" SetConsoleCursorPosition failed"
  ;

\ override for getxy
\ : _GETXY        ( -- x y )
\                 getxy_x XCALL WORD-SPLIT ;
: x_getxy       ( -- x y )
                x_GetScrBuffInfo
                &amp;xCsrCol w@
                &amp;xCsrRow w@ ;

\ override for getcolrow
\ : _GETCOLROW    ( -- cols rows )
\                 getcolrow_x XCALL WORD-SPLIT ;
: x_getcolrow   ( -- rows cols )
                &amp;xScrCols w@
                &amp;xScrRows w@ ;

: x_col          ( n -- )
                x_CsrColRow drop 1- min x_getxy drop - spaces ;

\  override for sizestate in fkernel.f
\  : SIZESTATE     ( -- state )    \ state of the display
\                  sizestate_x XCALL ;

: x_sizestate     ( -- state )       0 ;     \  SIZE_RESTORED

create &amp;xEvent 128 allot                  \ event buffer

&amp;xEvent
    dup constant &amp;xEventType                    \ event type
dup 2 +                                         \ as a union
2 + dup constant &amp;xKeyDown                      \ 1 = key down, 0 = key up
4 + dup constant &amp;xKRepeatCount                 \ repeat count
2 + dup constant &amp;xKVirtKey                     \ virtual key
2 + dup constant &amp;xKVirtScan                    \ virtual scan
2 + dup constant &amp;xKChar                        \ the character
4 + dup constant &amp;xKCtrlKeyState                \ control key state
drop
2 + dup constant &amp;xMCol                         \ mouise column
2 + dup constant &amp;xMRow                         \ mouse row
4 + dup constant &amp;xMButtonState                 \ button state
4 + dup constant &amp;xMCtrKeyState                 \ control key state
4 +     constant &amp;xMEventFlags                  \ mouse event flags

\  override for ekey
\  : EKEY          ( -- key )
\                  key_x XCALL ;

2 PROC WaitForSingleObject
: event-wait    ( hEvent - )   \ ** wait while event or object is NOT set
   INFINITE  swap  Call WaitForSingleObject drop  ;

\  override for EKEY?
\  : EKEY?         ( -- f )
\                 keyq_x XCALL ;

\ added: EmptyInputBuffer event-set? scan_event
1 PROC FlushConsoleInputBuffer
: EmptyInputBuffer ( - )    StdIn call FlushConsoleInputBuffer drop ;

: event-set?  ( hEvent - true/false )   0  swap  Call WaitForSingleObject 0=  ;

4 PROC PeekConsoleInputA
: scan_event ( -  )
                x_setraw
                &amp;xCharCount rel&gt;abs 1 &amp;xEvent rel&gt;abs StdIn
                call PeekConsoleInputA  0= abort" PeekConsoleInput failed"
                x_setcooked
 ;

: x_ekey?       ( -- f )
                StdIn event-set?  \ ** To test if any keys is pressed.
                EmptyInputBuffer ;

\ ** Now x_ekey is able to see a number of key combinations
: x_ekey  ( - char )
    StdIn event-wait
       begin     scan_event  150 ms  StdIn event-set?
       until
    &amp;xKChar c@   EmptyInputBuffer
 ;

\  override for _TYPE
\  : _TYPE         ( addr len -- )
\                  SWAP REL&gt;ABS type_x XCALL DROP ;

4 PROC PeekConsoleInputA
5 PROC WriteConsole
: x_type        ( addr len -- )
                0 pad rel&gt;abs               \  null, chars written
                2swap                       
                swap rel&gt;abs                \  now len addr
                StdOut                     \ fetch handle
                call WriteConsole           \  write string
                0= abort" WriteConsole failed"
                ;

\  override for _EMIT
\  : _EMIT         ( char -- )
\                 emit_x XCALL DROP ;

\ ** A x_emit which uses the stack as the buffer
: x_emit        ( char -- )   sp@ 1 x_type drop ;

\  override for _CR
\  : _CR           ( -- )
\                  cr_x XCALL DROP ;

\ ** A x_cr which uses the stack as the buffer
: x_cr          ( -- )     0x00000d0a sp@ 2  x_type drop ;


\  override for _CLS
\  : _CLS          ( -- )
\                  cls_x XCALL DROP ;

5 PROC FillConsoleOutputCharacter
: x_cls         ( -- )                     \ clear the screen
                x_GetScrBuffInfo
                &amp;xCharCount rel&gt;abs
                0                          \ note - that's 0 0 in coords
                x_ScrColsRows *            \ total length of buffer
                bl StdOut
                call FillConsoleOutputCharacter
                0= abort" FillConsoleOutputCharacter failed"
                0 0 x_gotoxy
                xDfltColours x_SetFgBg      \ clear out attributes
                ;


\  override for _?CR
\  : _?CR          ( n -- )
\                  qcr_x XCALL DROP ;
: x_?cr         ( n -- )                  \ enuf room for n more chars in current line?
                x_CsrColRow               \ get col row on stack
                drop                      \ drop row
                x_ScrColsRows             \ get cols, rows
                drop                      \ drop rows
                - +                       \ left to required
                0&gt; if x_cr then           \ cr if more chars to print
                ;

\  override for _accept
\  : _ACCEPT       ( addr len -- n )
\                 SWAP REL&gt;ABS
\                 accept_x XCALL ;

5 PROC ReadConsoleA
: x_accept      ( addr len -- n )
                xSetTitleReady
                0 &amp;xCharCount rel&gt;abs     \  null, chars actually read
                2swap                     \ ** saving one Forth-call
                swap rel&gt;abs              \  now len addr
                StdIn                    \  handle to stdin
                call ReadConsoleA         \  read string
                0= abort" ReadConsole failed"
                &amp;xCharCount @  2 -        \  put length read on stack minus crlf
                xSetTitleWait
                ;

\  override for init-console in fkernel.f
\  : INIT-CONSOLE  ( -- f1 )       \ initialize the Forth console window
\                                  \ f1=false if already inited
\                  HCON @ &gt;R
\                  initconsole_x XCALL
\                  DUP HCON !
\                  0= THROW
\                  R&gt; HCON @ - ;

0 PROC AllocConsole

: x_init-console  ( -- f )            \  initialise console
                xinit-console-done?  drop 0
                0= if
                      call AllocConsole
                      0= abort" AllocConsole failed"
                      \  get handles
                      STD_ERROR_HANDLE call GetStdHandle
                      dup true = abort" GetStdHandle failed"
                      to StdErr
                      STD_OUTPUT_HANDLE call GetStdHandle
                      dup true = abort" GetStdHandle failed"
                      to StdOut
                      STD_INPUT_HANDLE call GetStdHandle
                      dup true = abort" GetStdHandle failed"
                      to StdIn
                      true to xinit-console-done?
                then
                xinit-console-done? ;

\  override for init_screen in fkernel.f
\  : INIT-SCREEN   ( -- )          \ init the screen
\                  INIT-CONSOLE DROP
\                  normconsole_x XCALL DROP ;

: x_init-screen ( -- )              \  init the default screen
                x_init-console drop   x_cls   xSetTitleReady
  ;


\ Override all vectored defers and makedefer those that aren't

' x_EKEY      IS KEY
' x_ACCEPT    IS ACCEPT
' x_EMIT      IS EMIT
' x_TYPE      IS TYPE
' x_CR        IS CR
' x_CLS       IS CLS
' x_EKEY?     IS KEY?
' x_?CR       IS ?CR
' x_GOTOXY    IS GOTOXY
' x_GETXY     is GETXY
' x_GETCOLROW IS GETCOLROW
' X_COL       IS COL


' x_EKEY?  makedefer  EKEY?
' x_accept makedefer _accept
' x_emit   makedefer _emit
' x_type   makedefer _type
' x_cr     makedefer _cr
' x_gotoxy makedefer _gotoxy
' x_cls    makedefer _cls
' x_?cr    makedefer _?cr
' x_GETXY  makedefer _GETXY
' x_GETCOLROW makedefer _GETCOLROW
' x_EKEY   makedefer EKEY
' x_init-screen makedefer init-screen


' x_init-screen IS init-screen
\ ' noop     is basic-forth-io     \ sets some weird output if we let it
' x_sizestate   is sizestate
' x_init-console is init-console




init-screen

: tinfo
 s" Welcome to xConsole" type cr cr
 x_getscrbuffinfo
 s" Screen characteristics: " type cr
 x_scrcolsrows
 s" Rows: " type . cr
 s" Cols: " type . cr
 s" Console characteristics: " type cr
 x_scrincons
 s" bottom:   " type . cr
 s" right:    " type . cr
 s" top:      " type . cr
 s" left:     " type . cr
;

tinfo

\ : test  key? if key emit then ;

</PRE></BODY></HTML>
