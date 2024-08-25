\ thread.f beta 5.01.0 14/03/2003 20:33:36 arm Callback changes to use assembler

cr .( Loading Windows Threads...)
cr .( -- BETA THREAD.F V501A)

\ -------------------- Task Control Block Offsets --------------------

: task>parm     ( thread -- addr )             \ thread parameter
                CELL+ ;

: task>id       ( thread -- addr )             \ thread id
                2 CELLS+ ;

: task>handle   ( thread -- addr )             \ thread handle
                3 cells+ ;

: task>stop     ( thread -- addr )             \ thread handle
                4 cells+ ;                     \ the stop flag

: task>parm@    ( task-block -- parm )         \ extract the parameter
                task>parm @ ;

\ -------------------- Task Start Initialisation --------------------

: exit-task     ( n -- )                        \ exit the thread
                call ExitThread ;

: (task)        ( task-block -- )               \ helper routine
\                dup TCB !                       \ save as task control block
\                tcb @ task>parm @                 \ task-block parm
                tcb @ perform                    \ task-block -> cfa  ( parm -- exitval )
                exit-task                       \ and exit the thread, never returns
                ;

cfa-code BEGIN-TASK ( -- )                      \ thread management. init a new thread/task
                push    esi                     \ save regs
                push    edi
                push    ebx
                push    ebp
                mov     ebp, esp                \ top of return stack

                call    @@1
@@1:            pop     edi
                sub     edi, # HERE 1- a;       \ edi = forth base, here 1- is @@1

                mov     ecx, # PROBESTACK       \ number of pages to probe stack
                mov     edx, esp                \ stack address
@@2:            mov     -4092 [edx], # 0        \ probe page at [edx]
                sub     edx, # 4096             \ next page down
                loop    @@2                     \ loop

                and     esp, # -16              \ align to 16 byte boundary
                mov     eax, esp                \ rstack top in eax
                sub     eax, edi                \ abs>rel
                sub     esp, # RSTACKSIZE       \ room for return stack
                mov     edx, esp                \ user area is on stack
                mov     fs: 0x14 , edx          \ save in TIB at pvArbitrary
                mov     RP0 [UP] , eax          \ save RP0
                sub     esp, # USERSIZE         \ subtract usersize
                mov     eax, esp                \ top of data stack
                sub     eax, edi                \ abs>rel
                mov     SP0 [UP] , eax          \ save SP0
                mov     BASE [UP] , # 10        \ default base to decimal
                mov     ebx, # NOMSG
                mov     MSG [UP], ebx           \ set default throw msg

                mov     eax, 5 cells [ebp]      \ get task block
                mov     TCB [UP] , eax          \ save in TCB
                mov     ebx, 4 [eax]            \ parameter
\                push    ebx
\                mov     ebx, 0 [eax]            \ cfa
                mov     eax, # ' (task)         \ get helper entry point
                exec    c;                      \ go do it

\ -------------------- Task Management  --------------------

: (create-task) ( addr state -- flag )          \ create a task
                swap                            \ state addr
                dup task>stop off               \ turn off stop flag
                dup>r                           \ put address of task on rstack
                task>id rel>abs                 \ threadid pointer
                swap ( CREATE_SUSPENDED | 0  )  \ run it later? from state on stack
                r@                              \ parameter (ptr to cfa/parm pair)
                begin-task rel>abs              \ task entry code
                0 0                             \ stack, thread attributes
                call CreateThread dup
                r> task>handle !                \ save in threadid
                0<> ;                           \ and set the flag, true=ok
                
: create-task   ( addr -- flag )                \ create task suspended
                CREATE_SUSPENDED (create-task) ;

: run-task      ( addr -- flag )                \ create task running
                0 (create-task) ;

: suspend-task  ( addr -- flag )                \ suspend a task
                task>handle @                   \ point at thread handle
                call SuspendThread -1 <> ;      \ true=0K

: resume-task   ( addr -- flag )                \ suspend a task
                task>handle @                   \ point at thread handle
                call ResumeThread -1 <> ;       \ true=0K

: stop-task     ( addr -- )                     \ stop the task
                task>stop on ;                  \ stop flag
                
: task-sleep    ( n -- )                        \ sleep the task for n ms
                call Sleep drop ;
                
: (task-block)  ( parm cfa-task addr -- len )   \ build a task block at addr
                dup>r !                         \ cfa
                r@ cell+ !                      \ parameter for the task
                r> 2 cells+ 0 over !            \ 0 threadid
                cell+ 0 over !                  \ thread handle
                cell+ 0 swap !                  \ flag
                5 cells
                ;

: task-block    ( parm cfa-task -- addr )       \ a task-block
                here >r                         \ return this block's address
                ,                               \ cfa to execute as task
                ,                               \ parameter for task (extracted later)
                0 ,                             \ threadid
                0 ,                             \ thread handle
                0 ,                             \ stop flag
                r> ;                            \ return structure
                
: task-stop?    ( task-block -- )               \ pause, stop if we're told
                task>stop @ ;                   \ check, exit if stop set

\ -------------------- Task Lock Definitions --------------------

: lock          ( lock -- )                    \ lock on a lock
                rel>abs call EnterCriticalSection drop ;

: unlock        ( lock -- )                    \ unlock a lock
                rel>abs call LeaveCriticalSection drop ;

: trylock       ( lock -- fl )                 \ try a lock
                rel>abs call TryEnterCriticalSection 0<> ;  \ 0 -- lock is blocked

: make-lock     ( -- <name--> )                \ create a lock
                create
                here 6 cells allot
                0 swap rel>abs call InitializeCriticalSectionAndSpinCount drop ;
                
\ -------------------- Task Specific Overrides --------------------
\ Memory locks, see kernel & primutil memory words

make-lock mem-lock                             \ to make mem allocation thread safe

:noname mem-lock lock ;   is (memlock)         \ override defered lock memory word
:noname mem-lock unlock ; is (memunlock)       \ override defered lock memory word


\ -------------------- Demonstrations  --------------------
\ demo1 code, fairly complex example.
\ creates several running tasks and waits for them to complete.
\ each task runs and produces output on a line number passed as a parameter
\ and waits between printing numbers based on the line it's on. 
\ wait-eachtask is notable -- it waits on all the tasks. as each task completes
\ it then rewaits on those that are still running until none are left.

make-lock console-lock                     \ a simple console lock, the console is not thread-safe
: c-lock console-lock lock getxy ;         \ lock console, save where the cursor is
: c-unlock gotoxy console-lock unlock ;    \ unlock, restore cursor

4 newuser location

: my-task { y -- }  \ prints a counter from 1 to 99 with a wait
                               \ that depends on which line it is running
          y location !         \ show that user & local variables work
          c-lock
          1 location @ gotoxy
          ." Task " tcb @ task>id @ .  tab
          ."  running at line " location @ 1+ .
          c-unlock
          100 1 do
          location @ 15 * task-sleep  \ sleep depends on line number, bigger=longer
          c-lock
            40 location @  gotoxy
            i  .
          c-unlock
          loop
          c-lock 50 location @ gotoxy ." Exiting..." c-unlock 1 ;     \  my exit code

15 value taskcount                      \ number of tasks to start
create taskblocks 15 cells allot        \ cells to hold task blocks ptrs
create taskhndls  15 cells allot        \ cells to hold task handles for wait function

: make-tasks   ( n -- )                  \ create the task blocks
    to taskcount
    taskcount 0 do
        i 1 +                            \ line number for my-task
        ['] my-task task-block           \ create the task block
        taskblocks i cells+ !            \ save in the taskblocks area
    loop
    ;

: run-tasks  ( -- )                      \ run all the tasks
    taskcount 0 do                       \ for each task
       taskblocks i cells+ @             \ get the task-block
       dup run-task drop                  \ run the tasks
       task>handle @ taskhndls i cells+ ! \ save all the task handles created
    loop
    ;

winerrmsg on

0 value taskwaits
: wait-eachtask ( -- )                   \ wait for each task
        taskcount to taskwaits
        begin
          taskwaits
        while
          INFINITE false taskhndls rel>abs    \ wait for 1 or more tasks to end
          taskwaits call WaitForMultipleObjects \ wait on handles list
          dup WAIT_FAILED = if getlastwinerr then \ note the error
          WAIT_OBJECT_0 +
          dup>r taskblocks +cells @ task>id @   \ get the task id
          console-lock lock
          ." Task " . ." completed" cr
          console-lock unlock
          -1 +to taskwaits                    \ 1 fewer task, clean up the list
          taskhndls taskwaits cells+ @        \ get last handle in list
          taskhndls r@ cells+ !               \ store in signaled event ptr
          taskblocks taskwaits cells+ @       \ get last block in list
          taskblocks r> cells+ !              \ store in signaled block
        repeat
        ." All tasks completed" cr
    ;

: start-tasks ( n -- )
     make-tasks
     run-tasks
     console-lock lock
     0 25 gotoxy ." Main task is waiting for " taskcount . ." tasks" cr
     console-lock unlock
     wait-eachtask
     ." All tasks ended" cr
     ;

: demo1 
     cls
     ." Demo1: Creating free running tasks "
     taskcount start-tasks ;

cr .( Type Demo1 to start Demo1) cr

\ demo2 creates 2 tasks that read the same file, but at varying speeds, showing
\ that file i/o is thread safe

4 newuser fhndl

: t2-openfile ( addr len -- )
        r/o open-file abort" File open error!" fhndl ! ;
        
: my-task2 { speed -- }
        console-lock lock
        tcb @  task>id @ ." Task" . ." is running with a delay of"
           speed . cr 
        console-lock unlock
        s" src\task.f" t2-openfile
        begin
          pad 256 fhndl @ read-line abort" IO Error!"
          tcb @  task-stop? not and
        while
          console-lock lock
          ." Task" tcb @  task>id @ .
          pad swap type cr
          console-lock unlock
          speed task-sleep
        repeat
        fhndl @ close-file ;
        
0 value task-slow
0 value task-fast
100 constant  task-slow-speed
30 constant  task-fast-speed

: demo2
  cls ." Multithread file I/O, press any key to stop" cr
  task-slow-speed ['] my-task2 task-block to task-slow
  task-fast-speed ['] my-task2 task-block to task-fast
  task-slow run-task drop
  task-fast run-task drop
  key drop
  task-slow stop-task
  task-fast stop-task
  ." Ended"
;

.( Type Demo2 to start Demo2, any key to stop) cr

