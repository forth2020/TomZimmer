\ TASKER.F      Simple multitasking for Win32Forth      by Bill Muench

((
Subject: [up]
Date: Fri, 30 Aug 1996 06:02:30 -0400
From: BiMu@aol.com
To: zforth@ix.netcom.com

Hi Tom

I have attached a working version of the multitasker. It is all in
hilevel Forth. Of course I will optimize it. I just wanted to show you,
now.

Bee

    ---------------------------------------------------------------------
))
( eForth Multitask on Win32Forth )

\ 960903 tjz minor changes and adjustments for Win32Forth
\        renamed HAT to NEWTASK for readability
\ 960828 bee adapt to w32f33a7
\ 960811 bee adapt to w32f32
\        BRANCH is relative
\        note use of ABS>REL and REL>ABS
\        lomem \user\ --<sp --<rp himem
\        changed BUILD to ALSOTASK to avoid name conflict

ONLY FORTH ALSO DEFINITIONS

DECIMAL

\ =============================================================

.( Multitask )

: LOCAL         ( tid a -- a ) ( index another task's local variable )
                RP0 - ( address to offset )
                + ; ( offset to address )

INTERNAL

: _PASS         ( -- ) ( hilevel absolute branch )
                R> abs>rel @ rel>abs >R ;

' _PASS CONSTANT PASS

: _WAKE         ( -- ) ( restore follower )
                R> abs>rel UP!  TOS @ SP! RP! ;

' _WAKE CONSTANT WAKE

EXTERNAL

: PAUSE         ( -- ) ( allow another task to execute )
                RP@ SP@ TOS !  FOLLOWER @ rel>abs >R ;

: STOP          ( -- ) ( sleep current task )
                PASS STATUS ! PAUSE ;

: SLEEP         ( tid -- ) ( sleep another task )
                PASS SWAP STATUS LOCAL ! ;

: AWAKE         ( tid -- ) ( wake another task )
                WAKE SWAP STATUS LOCAL ! ;

: ACTIVATE      ( tid -- )
                DUP 2@                ( tid sp rp )
                R> OVER !             ( save entry at rp )
                OVER !                ( save rp at sp )
                OVER TOS LOCAL !      ( save sp in tos )
                AWAKE ; 

\ =============================================================

\ Define a new task 'name', with u user area bytes, s stack byte
\ space, and r return stack space.

((
: NEWTASKSTACKALLOC ( i*n u tid -- i*n )
                SP0 @                           \ get CONUSER sp0
                SWAP SP0 LOCAL !                \ set task sp0
                CELLS SP0 @ SWAP - TOS !        \ save new CONUSER sp0,
                                                \ TOS free when task active
                DEPTH
                BEGIN   DEPTH 1 -               \ empty CONUSER stack
                WHILE   SWAP >R
                REPEAT  >R
                TOS @ DUP SP0 ! SP!             \ set CONUSER stack pointer
                R>
                BEGIN   ?DUP
                WHILE   R> SWAP 1 -             \ restore CONUSER stack
                REPEAT  ;
))

: NEWTASK       ( u s r -<name>- ) ( -- tid )
        CREATE  HERE >R  ( rp0 ) 0 , ( sp0 ) 0 ,
                ROT USERMIN + ( user area )
                ROT + ALLOT ( data  stack )  HERE  -1 DUP , , ( tracer )
                SWAP ALLOT ( return stack )  HERE  -1 DUP , , ( tracer )
                R> 2! ;

\ initialize task tid, and put it to sleep

: ALSOTASK      ( tid -- )
                DUP CONUSER = IF DROP EXIT THEN       ( not main task )
                DUP SLEEP                             ( sleep new task )
                FOLLOWER @ OVER FOLLOWER LOCAL !      ( link new task )
                STATUS LOCAL FOLLOWER ! ;             ( link old task )

: ONLYTASK      ( -- ) ( initialize main task )
                CONUSER  DUP [ FOLLOWER RP0 - ] LITERAL + UP!
                STATUS FOLLOWER !  AWAKE ;

ONLYTASK ( initialize main task )

\ =============================================================
\ Shared resource locking

: SEMAPHORE     ( -<name>- )   \ define a semaphore for resource arbitation
                CREATE 0 , ;

\ get a semaphore, locking out other tasks from using the resource
\ related to this semaphore

: SEM-GET       ( semaphore -- )
                PAUSE ( remember your manners )
                DUP @ STATUS XOR ( owner ? )
                IF BEGIN DUP @ WHILE PAUSE REPEAT ( no, wait for release )
                  STATUS SWAP ! ( lock ) EXIT
                THEN DROP ;

\ release a semaphore, allowing other tasks to access the resource
\ related to this semaphore

: SEM-RELEASE   ( semaphore -- )
                DUP @ STATUS XOR IF DROP EXIT THEN  0 SWAP ! ( unlock ) ;

INTERNAL

: MULTI-KEY     ( -- c )        \ new version of key for multi-tasking
                BEGIN PAUSE KEY? UNTIL MENUKEY ;

EXTERNAL

: MULTI         ( -- )          \ enable multi-tasking
                ['] MULTI-KEY IS KEY ;

: SINGLE        ( -- )          \ disable multi-tasking
                ['] MENUKEY IS KEY ;

MODULE

\S =============================================================
\  Test and Example Code Follows
\  =============================================================

0 1000 CELLS 20000 CELLS NEWTASK T1
  T1 ALSOTASK

0 1000 CELLS 20000 CELLS NEWTASK T2
  T2 ALSOTASK

VARIABLE V1

: T1GO ( -- )
  0 V1 !
  T1 ACTIVATE
  98765
  BEGIN PAUSE 1 V1 +!
  AGAIN ;

VARIABLE V2

: T2GO ( -- )
  0 V2 !
  T2 ACTIVATE
  65432 78901
  BEGIN PAUSE 1 V2 +!
  AGAIN ;

: O ( -- ) V1 ? V2 ? ;
: P ( -- ) PAUSE O ;

\ =============================================================

