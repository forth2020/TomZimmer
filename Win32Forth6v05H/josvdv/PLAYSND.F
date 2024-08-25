\ September 27th, 2001 - 16:02 Playsnd.f for Win32Forth by J.v.d.Ven
\
\ The idea is to see how small a turnkey application for windows
\ could by changing extend.f.

\ When fkernel.exe and w2k are used, playsnd.exe will 'only' be 221,184 bytes.

\ Conditions when fkernel.exe is used:
\ 1. Copy small.f and playsnd.f to your Win32for directory
\ 2. Activate the line with FLOAD SMALL.F by deleting the backslash and
\    save playsnd.f
\ 3. Start fkernel.exe
\ 4. Type: fload playsnd.f

\ There are no conditions when win32for.exe is used.
\ playsnd.exe will be 300,032 bytes when win32for.exe and w2k are used.

\ Use:
\ Tell Windows to start playsnd.exe when you click twice
\ on a wave file.
\ Click twice on a wave file.
\ Playsnd.exe plays direct the wave file without showing a window or msgbox.

\ Code:

\ FLOAD SMALL.F ( *** activate this line when you use fkernel.exe *** )
\ small.f can be downloaded from: http://home.planet.nl/~josv

INTERNAL
WinLibrary WINMM.DLL
EXTERNAL

true value sounds_on/off

: sounds  ( z"sound-file" - )
   sounds_on/off
     if   SND_FILENAME NULL rot rel>abs call PlaySound   then drop  ;

\ In w98 a file-name is NOT between 2 quotes.
\ In w2k a file-name may be between 2 quotes.

\ unquote removes them when there are 2 quotes and
\ replaces the last one by a 0.

: unquote ( a - a1 )
   ascii " over c@ =
     if   1+ dup maxstring  ascii " scan drop 0 swap c!   then ;
     
: play-sound  ( - )
    cmdline drop unquote sounds bye ;

' play-sound turnkey playsnd cr dir playsnd.*

\s
