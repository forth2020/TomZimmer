anew multithr.f  \ April 27th, 2002 - 14:19 for Win32Forth by J.v.d.Ven

((
This system uses a simpel way to do parallel arithmetic.
Use events to synchronize several threads.
If an event is not set, the WaitForSingleObject enters an efficient wait state,
consuming very little processor time while waiting till the event is set.

Limitations:
Do not decompile a running thread.
Do not change a deferred execution vector while a thread is using it. ( eg pause)
Do not forget a running thread, leave Win32Forth.
The use of this pack is at your own risk.
This version run needs Win32forth version 4.1
The word pause is changed.
The result in pardemo is more than 100 better then the old one when
you use 2 counters.

Results: old version:
          Number to count for each counter is: 1917.00
 Running 1 counter using an integer. Moment... Elapsed time: 00:00:09.560
 Moment...Running 2 counters using integers Elapsed time: 00:00:09.550
 Moment...Running 2 counters using floats Elapsed time: 00:00:09.560
 Moment...Running 7 counters using floats Elapsed time: 00:00:09.500

Results: The new version when the number to count is 100 times bigger:
          Number to count for each counter is: 191700.
 Running 1 counter using an integer. Moment... Elapsed time: 00:00:04.500
 Moment...Running 2 counters using integers Elapsed time: 00:00:06.980
 Moment...Running 2 counters using floats Elapsed time: 00:00:08.680
 Moment...Running 7 counters using floats Elapsed time: 00:00:23.670

Note: The version of 7 counters can be improved.

May 26th, 2002 - 13:56 added: event-wait-max
Renamed offset to offset-up to avoid a conflict with struct.f ))

0 value h_ev_wake_all

: event-set     ( hEvent - )  Call SetEvent   0= abort" Event not set" ;
: event-reset   ( hEvent - )  Call ResetEvent 0= abort" Event not reset" ;

: event-wait-max   ( hEvent #ms - )  \ wait a #ms or till an event or object is set
  swap  Call WaitForSingleObject drop  ;

: event-wait    ( hEvent - )            \ wait while event or object is NOT set
   INFINITE  event-wait-max  ;

\ Events-to-wait-for can wait till ALL or ONE event is set.
\ The handles of the events are in an array of pHandles.

\ if bWaitAll is false events-to-wait-for will wait till one event or object is set
\ if bWaitAll is true  events-to-wait-for will wait till all events or objects are set
\ pHandles is a pointer to an array with events or object handles
\ nCount is the number of handles in the array

: events-to-wait-for  ( bWaitAll pHandles nCount - #waitobject )
   dup MAXIMUM_WAIT_OBJECTS > abort" Too many objects"
   >r rel>abs INFINITE -rot r> Call WaitForMultipleObjects ;

\ Note: In W98 it does not matter if bWaitAll is true or false

: event-set?    ( hEvent - true/false )    \ set/not_set
   0  swap  Call WaitForSingleObject 0=  ;

: make-event-set     ( z"name" - ) \ In Win32
    rel>Abs            \ addr
    false              \ init state      ( seems ignored ? )
    true               \ manuel reset ( seems ignored ? )
    NULL               \ lpSecurityAttrib
    Call CreateEvent   \ handle event, the event seems allways NOT set
    dup event-set ;

: make-event-reset     ( z"name" - ) \ In Win32
    rel>Abs            \ addr
    false              \ init state      ( seems ignored ? )
    true               \ manuel reset ( seems ignored ? )
    NULL               \ lpSecurityAttrib
    Call CreateEvent   \ handle event, the event seems allways NOT set
    dup event-reset ;

\ : test     h_ev_wake_all ev_set?     ;

\ If there isn't a pause in your thread then your thread will not run.

: (pause   0 Call Sleep drop ;

\ : (pause 0  false Call SleepEx DROP ;
\ : (pause-wake  h_ev_wake_all event-wait (pause ;


defined pause nip not [IF]    defer pause    [THEN]

\ ' (pause-wake is pause \ activate when you would like to use h_ev_wake_all

' (pause is pause

variable lpThreadID  666 lpThreadID !

cell newuser thread-handle

\ The idea to use a callback came from Eric Colin.

1 Callback: ThreadFunc ( arg -- f )  execute  ( return )  1 ;

: thread-up ( user-area-thread - thread-up )   3 cells+ ;   \ Forth depended.
: offset-up    ( user - offset-relative-to-up )   >body  @ ;

0 value thr

: start ( user-area-thread tid - )
   lpThreadID rel>abs        \ ptr to DWORD
   0                         \ 0 or CREATE_SUSPENDED
   rot                       \ arg for ThreadFunc tid
   &ThreadFunc rel>abs       \ address of ThreadFunc callback
   0                         \ thread's stack size: 0-> default
   0                         \ security attributes: 0 -> default or LP
   call CreateThread         \  ( - thread-handle )
   dup 0= abort" Thread not created."
  swap ['] thread-handle offset-up thread-up +  ! \ save the thread-handle in its
 ;                                                \ user-area-thread

\ Note: In w9x the created thread gets the same handle as the thread which
\ created the thread. So I decided to use events and WaitForSingleObject to let
\ threads wait.

: init-thread-user  ( user-area-thread - )
   csp @ sp0 !                            \ restore sp0 in the main thread
   thread-up up!                          \ now up points into the new user-area
   sp@ sp0 ! rp@ rp0 !                    \ put rp and sp in it \ handler ??
 ;

: create-thread-user:
   create  here usersize allot            \ allocate an user area
           conuser swap usersize move     \ copy the main user area
   does> ;

\s

