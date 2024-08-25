((

I recently had the need for a hi-res timer. I submit these for your review,
now I'm just a novice, so there is probably alot that can be improved upon.
there is no roundoff error correction for sure. If you think it's useable,
please do, if not flush it.=20
I'm not in the loop and am homeless on the net ( just borrowing a friends line
and just now getting update 5 if its available).  Thanks for all the forth
tools.

Floyd

High resolution timer tool. muTime dialates ELAPSE time exponentially. These
words use Win32 API kernel calls and require hardware commonly found on PC's.

   timer range: min =3D .muTimeLag
                     max =3D muTimeRes * MAX-D                 (Y247K ready?)

This tool is only a reference. Don't bank on it's precision.

Currently testing in Win95 & Win32s =BB counter not incrementing in OS/2.3.
Not "certifiable" =BB minimal error & formating code, etc. Just a novice Forth
bit
head seeking more time.

Thanks to the Win32Forth development team for the most excellent tools.

))

Anew muTimer.f

\ CR .( High resolution timer loading...)

create &muTimeF   2 cells allot
create &muTimeOn  2 cells allot
create &muTimeOff 2 cells allot

\ reckon timer reference
  &muTimeF rel>abs CALL QueryPerformanceFrequency ?win-error \ whats the freq?
  &muTimeF @ &muTimeF cell + @ f1.0 d>f f/                   \ t =3D 1/f
  fconstant muTimeRes                                        \ t factor

: muTime                               \ fire it up
     &muTimeOn rel>abs Call QueryPerformanceCounter
     drop ;                            \ err code to bit bucket

: (.muTime  ( f: - f)                   \ results
     &muTimeOff rel>abs Call QueryPerformanceCounter
     drop                              \ err code to bit bucket
     &muTimeOff @  &muTimeOff cell + @ \ stop count
     &muTimeOn @ &muTimeOn cell + @    \ start count
     d- d>f muTimeRes F*  ;            \ calc delta t


: .muTime                              \ results
    (.muTime space fe. ." sec " ;      \ enlighten

: .muTimeLag                           \ test spin
     muTime
     .muTime ;

: .muTimeSpec
     CR ." Usage: muTime <testword> .muTime"
     CR ." Timer resolution =3D " muTimeRes fe. ." sec "
     CR ." latency time =3D " .muTimeLag  ;

\ .muTimeSpec

