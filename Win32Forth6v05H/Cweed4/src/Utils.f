\ UTILS.F               A file to hold some utilities   by Tom Zimmer
\ -rbs globalized path init
\ Changes February 14th, 2002 - 1:37 - rls

\ utils.f beta 2.0A 2002/08/31 arm windows ANS file words
\ utils.f beta 2.9G 2002/09/24 arm release for testing
\ utils.f beta 3.3D 2002/10/08 arm Consolidated

cr .( Loading Utility Words...)
cr .( -- BETA UTILS.F V2.9G --)
only forth also definitions

include .\src\WinVersion6V05.f

\ -rbs added globalization
TRUE constant AbsolutePaths?    \ this flag will remain for the
                                \ current system compile.
create <home$> MAXSTRING allot

: Prepend<home>\
        \ S" CDEF:\PROGRA~1\WIN32FOR\" <HOME$> PLACE
        AbsolutePaths?
        if      &forthdir count <home$> place
        else    0 <home$> !
        then    <home$> +place
        <home$> count ;

\ <--- rbs


: hide-cursor   ( -- )
                conHndl call HideCaret drop ;

synonym cursor-off hide-cursor

: show-cursor   ( -- )
                conHndl call ShowCaret drop ;

synonym cursor-on show-cursor

: minimize-console ( -- )
                SW_SHOWMINIMIZED conhndl call ShowWindow drop ;

: screen-size   ( -- width height )     \ get windows screen size
                SM_CXSCREEN call GetSystemMetrics       \ screen width
                SM_CYSCREEN call GetSystemMetrics ;     \ screen height

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\       User specifiable string delimiter utility
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

: ,$            ( -< #text#>- )
                >in @ bl word swap >in ! 1+ c@
                word count here place here c@ 1+ allot 0 c, align ;

: .$            ( -< #text#>- )
                POSTPONE (.") ,$ ; immediate

: s$            ( -< #text#>- )
                ( -- a1 n1 )
                POSTPONE (s") ,$ ; immediate

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\       fill in some deferred words default functions
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

' _gotoxy    is-default gotoxy
' _getxy     is-default getxy
' _getcolrow is-default getcolrow
' _beep      is-default beep
' _do-mabort is-default do-mabort

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\       sound extension
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

synonym note tone               \ freq duration --

: beep-init     ( -- )          \ initialize beep to new parameters
                700 50 beep! ;

INITIALIZATION-CHAIN CHAIN-ADD BEEP-INIT

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\       define a word to restore a deferred word to its default function
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

: _restore_default ( -- )
                @(ip) >body dup 2 cells + @ swap ! ;

: restore-default ( -<name>- )    \ reset name to its default function
                state @
                if      POSTPONE _restore_default
                else    ' >body dup 2 cells + @ swap !
                then    ; immediate

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ Display the deferred words in the system, and their *current function
\ along with the default function.
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

: .deferred     ( -- )
                defer-list @
                begin   ?dup
                while   cr ." Deferred: "
                        dup cell - dup body> .NAME
                        23 col ."  does: " @ .NAME
                        45 col ."  defaults to: " dup cell+ @ .NAME
                        @
                        start/stop
                repeat  ;

: .cur-file     ( -- )
                ." The current file is: " cur-file count type ;

synonym .file    .cur-file

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ old original version of $EXEC, superceeded by the following series of words
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

\ : $EXEC         ( a1 -- f1 )
\                 dup count + off         \ null terminate string
\                 SW_SHOWNORMAL swap 1+ rel>abs call WinExec ;

INTERNAL

create StartupInfo
                here 0 ,                \ cb
                0 ,                     \ lpReserved
                0 ,                     \ lpDesktop
                0 ,                     \ lpTitle
                373 ,                   \ dwX
                3 ,                     \ dwY
                0 ,                     \ dwXSize
                0 ,                     \ dwYSize
                80 ,                    \ dwXCountChars
                50 ,                    \ dwYCountChars
                0 ,                     \ dwFillAttribute
                STARTF_USEPOSITION
                STARTF_USECOUNTCHARS +
                STARTF_USESHOWWINDOW +
                ,                       \ dwFlags
                SW_SHOWNORMAL W,        \ wShowWindow
                0 W,                    \ cbReserved2
                0 ,                     \ lpReserved2
                0 ,                     \ hStdInput
                0 ,                     \ hStdOutput
                0 ,                     \ hStdError
                here over - swap !

create ProcInfo
                0 ,             \ hProcess
                0 ,             \ hThread
                0 ,             \ dwPriocessId
                0 ,             \ dwThreadId

EXTERNAL

: zEXEC         ( a1 -- f1 )    \ pass to NT without any interpretation
                                \ f1 = TRUE on error
                >r                      \ null terminated parameter string
                ProcInfo    rel>abs     \ lppiProcInfo
                StartupInfo rel>abs     \ lpsiStartInfo
                0                       \ lpszCurDir
                0                       \ lpvEnvironment
                0                       \ fdwCreate
                0                       \ fInheritHandles
                0                       \ lpsaThread
                0                       \ lpsaProcess
                r> rel>abs              \ lpszCommandLine
                0                       \ lpszImageName
                call CreateProcess 0= ;

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\       Multiple directory path search capability for file open
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

named-new$ &execbuf
    create &fpath    MAXSTRING allot     \ a static forth path buffer
           &fpath    off
  variable &linenum  &linenum off

2variable path-source

&fpath value path-ptr                   \ initialize the path buffer pointer

: next-path"    ( -- a1 n1 )            \ get the next path from dir list
                path-source 2@ 2dup ';' scan  2dup 1 /string path-source 2!
                nip - ;

: first-path"   ( -- a1 n1 )            \ get the first forth directory path
                path-ptr count path-source 2!
                next-path" ;

: "fpath+       ( a1 n1 -- )            \ append a directory to forth path
                2dup upper
                2dup + 1- c@ '\' =              \ end in '\'?
                if      1- 0max                 \ if so, delete it
                then    first-path"                     \ get first path
                begin   dup>r 2over compare dup r> and  \ check it
                while   drop
                        next-path"                      \ and remaining paths
                repeat  0=              \ -- f1=true if already in list
                if      2drop
                else    path-ptr ?+; path-ptr +place
                then    ;

INTERNAL

: program-path-init ( -- )
                current-dir$ count 2dup upper path-ptr place
                path-ptr ?-\
                &prognam count "path-only" "fpath+
                path-ptr ?-\
\ -rbs
AbsolutePaths?
if
s" \;" &fpath +place
s" SRC;" Prepend<home>\ &fpath +place
s" HTM" Prepend<home>\ &fpath +place
then
\ <--- rbs
                ;

program-path-init       \ initialize the program name buffer

INITIALIZATION-CHAIN CHAIN-ADD PROGRAM-PATH-INIT

EXTERNAL

: .program      ( -- )
                &prognam count type ;

: .fpath        ( -- )          \ display the forth directory search path list
                path-ptr count
                begin   ?dup
                while   2dup ';' scan 2dup 2>r nip - dup 1+ ?cr type
                        2r> 1 /string dup
                        if      ." ;"
                        then
                repeat  drop ;

: fpath+        ( -<directory>- )       \ append a directory to forth path
                /parse-s$ count "fpath+ ;

create open-save$ MAXSTRING allot       \ buffer to save the file being opened
create open-path$ MAXSTRING allot
                                        \ f1=FALSE=success, TRUE=failed
: n"open        ( a1 n1 -- handle f1 )          \ open file a1,n1 with path search
                open-save$ place                \ save filename for later
                open-save$ count _"open dup     \ if we couldn't open the file
                if      open-save$ count 1 min + c@ ':' <>      \ not if second is ':'
                        open-save$ count 2 min + c@ '\' <> and  \ and if  third is '\'
                        open-save$ count 0 min + c@ '\' <> and  \ not if  first is '\'
                        open-save$               c@ 3   <  or   \ or if string is too short
                        if      2drop                           \ discard _"open results
                                first-path"
                                begin   dup>r
                                        open-path$ place        \ first path
                                        open-path$ c@           \ append if path is there
                                        if      open-path$ ?+\  \ plus '\'
                                        then
                                        open-save$ count
                                        open-path$ +place               \ append name
                                        open-path$ count _"open dup     \ open it
                                        r> and
                                while   2drop
                                        next-path"
                                repeat
                        then
                else    open-save$ count 1 min + c@ ':' <>      \ not if second is ':'
                        open-save$ count 2 min + c@ '\' <> and  \ and if  third is '\'
                        open-save$ count 0 min + c@ '\' <> and  \ not if  first is '\'
                        open-save$               c@ 3   <  or   \ or if string is too short
                        if      current-dir$ count open-path$   place
                                                   open-path$ ?+\
                                open-save$   count open-path$  +place
                        else    open-save$   count open-path$   place
                        then
                then    ;               \ return n2=handle, f1=false if found

' n"open is "open       \ link multi-path open word into system

: "path-file    ( a1 n1 -- a2 n2 f1 )   \ find file a1,n1 return full path
                                        \ a2,n2 and f1=false, succeeded
                                        \ else return a1,n1 and f1=true, failed
                2dup "open 0=
                if      close-file drop
                        2drop
                        open-path$ count false
                else    drop true
                then    ;

: "file-clip"   { adr len limit \ temp$ pre -- 'adr 'len }
                                                        \ clip filename to limit
                MAX-PATH LocalAlloc: temp$
                limit 20 max to limit                   \ must be at east 16
                limit 20 - 2 / 6 + to pre               \ balance start and end
                len limit >
                if      adr pre 3 -     temp$  place    \ lay in first 5 chars
                        s" ..."         temp$ +place    \ append some dots
                        adr len dup limit pre - - 0MAX /string \ clip to last part
                                        temp$ +place    \ of name and lay in
                                        temp$  count
                else    adr len                         \ no need to clip file
                then    ;

in-system

: .platform     ( -- )
                cr ." Platform: Windows "
                winver case
                 WIN95     of ." 95" endof
                 WIN98     of ." 98" endof
                 WINME     of ." ME" endof
                 WINNT351  of ." NT3.51" endof
                 WINNT4    of ." NT4" endof
                 WIN2K     of ." 2000" endof
                 WINXP     of ." XP" endof
                endcase ;

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\       Fsave stuff:    Save a Forth image into a resource
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

: "resource-image-save { adr len \ image$ exehndl -- } \ save a Forth executable resource
                max-path localAlloc: image$
                adr len image$ place
                        image$ +NULL            \ null terminate name
                end-transient
                clear-transient                 \ discard any transient stuff

                here &origin -                  \ current actual application dictionary size
                &app-actual !                   \ set actual dictionary length field

                sys-here sys-origin -           \ current actual system dictionary size
                &sys-size @ min                 \ limit to currently specified system size
                &sys-actual !                   \ set actual dictionary length field

                FALSE                           \ FALSE=don't delete all resources in .EXE
                image$ 1+ rel>abs               \ .EXE filename
                Call BeginUpdateResource dup 0= abort" Failed to open .EXE file"
                to exehndl
                ?loading @ >r ?loading off      \ preserve but clear ?loading
                source-id  >r 0 to source-id    \ preserve but clear source-id
                malloc-link @ >r malloc-link off \ save and clear malloc-link
                &sys-size @                     \ if we are saving system dictionary
                                                \ then allocate a big buffer for both
                                                \ application and system dictionaries
                if      &app-actual @ &sys-actual @ + allocate
                        ABORT" Failed to allocate resource image buffer"
                        >r                      \ allocate and save ptr
                        &origin r@ &app-actual @ move                    \ move in app image
                        sys-origin r@ &app-actual @ + &sys-actual @ move \ then    sys image

                        &app-actual @ &sys-actual @ +           \ total resource size
                        r@ rel>abs                              \ buffer addres absolute
                        0x409  ( LANG_ENGLISH + SUBLANG_ENGLISH_US ) \ language ID
                        1234                                    \ resource 1234
                        z" IMAGE" rel>abs                       \ user defained data type
                        exehndl                                 \ the resource handle
                        Call UpdateResource 0= ABORT" Error updating resource in .EXE file"
                        r> release
                else    &app-actual @                           \ total resource size
                        &origin rel>abs                         \ buffer addres absolute
                        0x409  ( LANG_ENGLISH + SUBLANG_ENGLISH_US ) \ language ID
                        1234                                    \ resource 1234
                        z" IMAGE" rel>abs                       \ user defained data type
                        exehndl                                 \ the resource handle
                        Call UpdateResource 0= ABORT" Error updating resource in .EXE file"
                then
                r> malloc-link !                \ restore malloc-link
                r> to source-id                 \ restore source-id
                r> ?loading !                   \ restore ?loading
                FALSE                           \ false=write changes
                exehndl                         \ the .EXE file resource handle
                Call EndUpdateResource 0= ABORT" Failed to write to .EXE file" ;

: resource-image-save       ( -<name>- )
                FALSE to check-registry-directory?
                /parse-s$ count "resource-image-save ;

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\       Fsave stuff:    Save a Forth image to a .IMG disk file
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

: "image-save   { adr len \ image$ imagehndl -- } \ save a Forth executable image
                max-path localAlloc: image$
                adr len 2dup '.' scan nip - image$ place
                end-transient
                clear-transient                 \ discard any transient stuff
                s" .IMG" image$ +place          \ append image extension
                         image$ +NULL           \ null terminate name

                here &origin -                  \ current actual application dictionary size
                &app-actual !                   \ set actual dictionary length field

                sys-here sys-origin -           \ current actual system dictionary size
                &sys-size @ min                 \ limit to currently specified system size
                &sys-actual !                   \ set actual dictionary length field

                image$ count r/w bin create-file abort" Failed to create file"
                to imagehndl
                ?loading @ >r ?loading off      \ preserve but clear ?loading
                source-id  >r 0 to source-id    \ preserve but clear source-id
                malloc-link @ >r malloc-link off \ save and clear malloc-link
                &origin &app-actual @ imagehndl WRITE-FILE
                ABORT" Error writing Application Dictionary"
                r> malloc-link !                \ restore malloc-link
                r> to source-id                 \ restore source-id
                r> ?loading !                   \ restore ?loading
                &sys-size @                     \ don't save system/heads if size is zero
                if      sys-origin &sys-actual @ imagehndl WRITE-FILE
                        ABORT" Error writing System Dictionary"
                then
                imagehndl CLOSE-FILE ABORT" Failed to close file" ;

: image-save       ( -<name>- )
                /parse-s$ count "image-save ;

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\       Fsave stuff:    Copy KERNEL.BIN to new .EXE file and save the
\                       Forth iamge file
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

FALSE value with-img?

: "fsave        { adr len \ from$ to$ -- }    \ save a Forth executable
                max-path localAlloc: from$
                max-path localAlloc:   to$
                adr len                to$ place
\+ message-off  message-off
                end-transient
                clear-transient                         \ discard any transient stuff
                to$ count upper                               \ upper case filename
                to$ count "minus-ext" nip to$ c!        \ remove any file extension
                &prognam count upper                    \ upper case program name
                &prognam count "to-pathend" "minus-ext" \ parse out filename only
                to$ count compare 0=
                abort" Can't save to the currently running program!"
                s" .EXE"               to$ +place       \ append .EXE
                                       to$ +NULL        \ null terminate destination
                &forthdir count from$ place             \ forth installation dir
                from$ c@ if from$ ?+\ then              \ append '\' if needed
                s" KERNEL.BIN"             from$ +place \ append kernel exe image
                                           from$ +NULL  \ end name with a NULL
                FALSE                                   \ copy even if to-name exists
                to$   1+ rel>abs                        \ copy to   filename
                from$ 1+ rel>abs                        \ copy from filename
                Call CopyFile 0=
                if      cr ." Copying From: " from$ count type
                        cr ."           To: " to$   count type
                        TRUE abort" Failed to copy KERNEL.BIN!"
                then
                nt?                                     \ on NT and
                with-img? 0= and                        \ not WITH-IMG
                if      to$ count "resource-image-save  \ save image into a resource
                        to$      count "to-pathend" "minus-ext" 2dup upper
                                                        \ a1,n1 of name
                        s" FKERNEL" compare             \ don't allow delete of FKERNEL.IMG
                        if      to$ count 2dup '.' scan nip - from$ place
                                s" .IMG" from$ +place           \ append image extension
                                         from$ +NULL            \ null terminate name
                                from$ count delete-file drop    \ delete any .IMG file with this name
                        then
                else    FALSE to with-img?                      \ for Win32s & 95, create a separate
                        to$ count "image-save                   \ executable .IMG file
                then    cr ." Save Complete" ;

IN-APPLICATION  \ these MUST be in the application

\ Default system initialization, when running a turnkeyed application.
\ replace the contents of DEFAULT-HELLO with your own initialization word
\ if you need to do some other type of initialization.  Be sure to include
\ DEFAULT-APPLICATION at the end of it though, so your application will
\ get executed.  TURNKEY stores the application cfa you pass to it into
\ DEFAULT-APPLICATION.

\ changing DEFAULT-APPLICATION to anything other than 'bye' will cause
\ HELLO to execute DEFAULT-APPLICATION when you create an APPLICATION.

defer DEFAULT-APPLICATION ' bye is default-application

: _default-hello ( -- )                          \ startup stuff
                decimal
                0 to source-id                  \ not loading from a file yet
                init-console                    \ -- f1
                if      initialization-chain do-chain
                then
                exception@
                if      bye
                then
                hide-console
                get-commandline                 \ commandline to SOURCE
                default-application ;

defer DEFAULT-HELLO ' _default-hello is default-hello

IN-SYSTEM       \ back to system dictionary

((

FSAVE, TURNKEY and APPLICATION all create new programs on your hard drive. They
differ, in the type of program they create.

FSAVE   Creates a new copy of Win32Forth on your harddrive, that is a duplicate
        of the Forth you are already running, and will still startup running
        Forth when the new program is executed.  FSAVE is followed by the name
        of the program to create.  The name passed to FSAVE must not be the
        same name as the Forth program you are currently running. See notes
        following.   FSAVE is used as follows;

        FSAVE newforth <enter>

        The amount of available dictionary space available will remain the
        same as before FSAVE was performed.  If you need to expand the amount
        of application dictionary or system dictionary space available for a
        subsequent program compile, you can use the words;

        <free-bytes> SYS-FREE!          \ set system dictionary free space
        <free-bytes> APP-FREE!          \ set application dictionary free space

        Setting the available dictionary space to a very large value will cause
        slowdowns and possible crashes.  As Win32Forth is shipped, you can
        expand the application dictionary upto a total of about  two megabytes
        before it will run into the system dictionary.  Use ".FREE" to see the
        current status of the two dictionaries.  If you need to build an
        applicaiton that will have an application dictionary larger than
        two megabytes, then you will need to edit the line in META.F that
        contains SSEPARATION, to a larger value, re-metacompile, re-extend
        Win32Forth, and then fsave a new copy of Win32Forht with an APP-FREE!
        value as large as you require.  

        1024 1024 * 2 * VALUE SSEPARATION     \ offset from target to heads

        If you do this, you will need machine with at least 32 megabytes of
        memory.

TURNKEY Creates an application program that cannot run Forth any
        longer, because all headers are discarded during the program
        save process.  The 'cfa' of a forth definition that runs your
        program must be passed to TURNKEY. TURNKEY reduces the newly
        saved program image size by over 200k bytes. TURNKEYed programs
        cannot be debugged. TURNKEY is followed by the name of the
        program to create. The name passed to TURNKEY must not be the
        same name as the Forth you are currently running. See notes
        following. TURNKEY is used as follows;

        ' do-myprogram TURNKEY newprogram <enter>

        TURNKEYed programs always have 3000 bytes of application
        dictionary space available at program startup, and zero bytes of
        system dictionary space available, since all of the system
        dictionary is discarded.

        TURNKEY performs a default form of initialization defined above.

        Your application word 'do-myprogram' as used above, is installed
        into the deferred word DEFAULT-APPLICATION by TURNKEY, so it will
        be run after initialization is completed.  If you need to perform
        some other initialization that shown above, then you will need to
        copy copy the original definition of _DEFAULT-HELLO, change it
        and then install it into the deferred word DEFAULT-HELLO.

APPLICATION Creates an application program  on your harddisk that is also fully
        Forth. The 'cfa' of a forth definition that performs any needed program
        initialization must be passed to APPLICATION. APPLICATIONed programs
        can still be debugged. APPLICATION is followed by the name of the
        program to create. The name passed to APPLICATION must not be the same
        name as the Forth you are currently running. See notes following.
        APPLICATION is used as follows;

        4000 4000 ' do-newapplication APPLICATION newapplication <enter>

        Where the first number is the bytes of application dictionary space
        you want to be available at program startup.  The second number is the
        bytes of system dictionary space you want to be available at program
        startup.

NOTES:  All of the program save words require that a file called KERNEL.BIN
        must be resident in the Win32Forth installation directory.  KERNEL.BIN
        is a copy of the file FKERNEL.EXE.  KERNEL.BIN is copied to the new
        program you specify as "name".EXE, then the Forth image is written to
        disk either into the resource fork of "name".EXE (if you are running
        WindowsNT), or into file "name".IMG (if you are running Windows 3.1
        with Win32s, or Windows95).  The reason the program save works the way
        it does, is because only WindowsNT supports writing into the resource
        of a program, and Windows 3.1 and Windows95 won't allow a running
        program to copy its own executable.  Even worse, Widows 3.1 cannot
        successfully read a user defined resource from an executable, so even
        if you were to create a single file executable under WindowsNT, it
        won't run under Windows 3.1.  For this reason, if you are developing
        programs under WindowsNT (like I am), and you want those programs to
        run under Windows 3.1 with Win32s, then you must use the WITH-IMG
        directive before using any of these save words, to force the save to
        create a "name".IMG file.

))

: WITH-IMG      ( -- )                  \ force FSAVE to create an .IMG file
                TRUE to with-img? ;     \ even on WindowsNT

: FSAVE         ( -<name>- )
                /parse-s$ count "fsave ;

: TURNKEY       ( cfa -<name>- )        \ create application "name" that runs
                                        \ the forth definition specified by 'cfa'.
                sys-warning? 0=
                abort" Can't TURNKEY with SYS-WARNING-OFF!"
                dup ?sys-address
\ WHEN TURNKEYING, IGNORE MISSING PROCEDURE WARNINGS !
                ignore-missing-procs? >r true to ignore-missing-procs?
                defer@ boot >r
                defer@ default-application >r
                app-free    >r
                sys-free    >r
                defer@ default-hello is boot \ initialization program to BOOT
                                             \ calls default application after init
                is default-application
                3000 app-free!               \ set application free space very low
                   0 sys-free!               \ set system free space to zero, no system
                FALSE to with-source?        \ no source level debugging
                                             \ don't mess with Forth directory
                FALSE to check-registry-directory?
                &except off                  \ no previous exceptions...     
                fsave
                r> sys-free!
                r> app-free!
                r> is default-application
                r> is boot
                r> to ignore-missing-procs?
                ;

: APPLICATION   ( app-mem sys-mem app-cfa -<name>- ) \ create application "name"
                depth 3 < abort" Need 3 arguments, APP-MEM SYS-MEM and APP-CFA"
                is default-application  \ makes HELLO call DEFAULT-APPLICATION
                sys-free!               \ set system dictionary free space
                app-free!               \ set application dictionary free space
                                        \ don't mess with Forth directory
                FALSE to check-registry-directory?
                fsave ;

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\       display the files loaded into the system
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

: .loaded       ( -- )
                cr
                loadfile @
                begin   ?dup
                while   14 ?cr
                        dup cell+ count "to-pathend"
                        2dup 2dup '.' scan nip - nip 10 swap - spaces
                        2dup 2dup upper type
                        '.' scan nip 4 swap - 0 max spaces
                        dup @
                        if      \ if no code compiled, then discard filename
                                dup>r @ dup cell+ count + r> =
                                if      @
                                then
                        else    @
                        then
                        start/stop
                repeat  ;

INTERNAL
                                                \ adr,len is name loaded?
: "loaded?      { adr len \ temp$ -- f1 }       \ f1 = true if file has been loaded
                max-path LocalAlloc: temp$      \ allocate local string buffer
                adr len upper
                adr len "to-pathend" to len to adr      \ strip off path
                loadfile @      \ top of the file loaded chain
                begin   ?dup    \ for as long as we aren't at the end
                while   dup cell+ count "to-pathend" temp$ place
                        temp$ ?defext
                        temp$ count upper
                        temp$ count adr len compare 0=
                        if      drop true
                                EXIT            \ exit with true on stack
                        then
                        dup @
                        if      \ if no code compiled, then discard filename
                                dup>r @ dup cell+ count + r> =
                                if      @
                                then
                        else    @
                        then
                repeat  false ;

EXTERNAL

: NEEDS         ( -<name>- ) \ conditionally load file "name" if not loaded
                >in @ >r
                /parse-s$ ?defext                 \ add file ext if needed
                pocket count "loaded? 0=        \ if file isn't loaded
                if      r@ >in !
                        fload                   \ then loadit
                then    r>drop ;

: \LOADED-      ( -<name>- ) \ if the following file IS NOT LOADED interpret line
                >in @ >r
                /parse-s$ ?defext
                pocket count "loaded? 0=
                if      interpret
                else    postpone \
                then    r>drop ;

: \LOADED       ( -<name>- ) \ if the following file IS LOADED interpret line
                >in @ >r
                /parse-s$ ?defext
                pocket count "loaded?
                if      interpret
                else    postpone \
                then    r>drop ;

in-application

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\       SHELL support with interpreted string replacement for selected words
\       %FILENAME  %DIR  %LINE
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

INTERNAL

: execbuf+      ( a1 n1 a2 -- ) \ append to the exec buffer
                &execbuf 2dup c@ + MAXCOUNTED > abort" Too long for EXEC buffer"
                +place ;

true value new-prompt?

                                \ Invoke a DOS command string with
EXTERNAL

: $EXEC         ( a1 -- f1 )    \ preprocess for file and line parameters
                                \ f1 = TRUE on error
                base @ >r decimal
                &execbuf off                    \ pre-zero the buffer
                count
                begin   2dup ascii % scan dup
                while   2dup 2>r nip - execbuf+ 2r>
                                over s" %FILENAME" tuck compare 0= >r
                                over s" %filename" tuck compare 0= r> or
                        if      new-prompt?
                           if   cur-file count "path-file
                                if      cr ." File doesn't exist, create it? [Y/N] (N):"
                                        key upc 'Y' <> abort" Aborting"
                                then    execbuf+
                           else cur-file count execbuf+
                           then
                                9 /string               \ remove %FILENAME
                        else
                                over s" %DIR"      tuck compare 0= >r
                                over s" %dir"      tuck compare 0= r> or
                        if      &prognam count 2dup "to-pathend" nip -
                                execbuf+
                                4 /string       \ remove %LINE
                        else
                                over s" %LINE"     tuck compare 0= >r
                                over s" %line"     tuck compare 0= r> or
                        if      &linenum @ 0 <# #s #> execbuf+
                                5 /string       \ remove %LINE
                        else
                                over 1 execbuf+
                                1 /string       \ remove one % char
                        then
                        then
                        then
                repeat  nip - execbuf+
\                cr &execbuf count type
                &execbuf +NULL
                &execbuf 1+ zEXEC
                r> base ! ;


create editor$  ," %DIRWinEd %FILENAME %LINE"      MAXSTRING allot-to
create browse$  ," %DIRWinEd /B %FILENAME %LINE"   MAXSTRING allot-to
create shell$   ," CMD /C "                        MAXSTRING allot-to
create dos$     ," CMD"                            MAXSTRING allot-to

: editor"       ( -<string">- ) \ set the editor command string
                ascii " word count editor$ place ;

: browse"       ( -<string">- ) \ set the browser command string
                ascii " word count browse$ place ;

: shell"        ( -<string">- ) \ set the shell command string
                ascii " word count shell$ place ;

: dos"          ( -<string">- ) \ set the dos command string
                ascii " word count dos$ place dos$ +NULL ;

: .editor       ( -- )  \ display the editor, browser, shell & dos strings
                cr .$ 'EDITOR" ' editor$ count type .$ '"'
                cr .$ 'BROWSE" ' browse$ count type .$ '"'
                cr .$ 'SHELL" '   shell$ count type .$ '"'
                cr .$ 'DOS" '       dos$ count type .$ '"' ;

synonym .shell  .editor
synonym .dos    .editor
synonym .browse .editor

: ?MessageBox   { flag adr len \ message$ -- }
                MAXSTRING localAlloc: message$
                flag
                if      adr len message$ place
                        message$ +NULL
                        MB_OK MB_ICONSTOP or MB_TASKMODAL or
                        z" Notice!" rel>abs
                        message$ 1+ rel>abs
                        NULL call MessageBox drop
                then    ;

: ?ErrorBox     { flag adr len \ message$ -- }
                MAXSTRING localAlloc: message$
                flag
                if      adr len message$ place
                        message$ +NULL
                        MB_OKCANCEL MB_ICONSTOP or MB_TASKMODAL or
                        z" Application Error, ABORT?" rel>abs
                        message$ 1+ rel>abs
                        NULL call MessageBox
                        IDCANCEL =
                        if      bye
                        then    abort
                then    ;

: ?TerminateBox { flag adr len \ message$ -- }
                MAXSTRING localAlloc: message$
                flag
                if      adr len message$ place
                        message$ +NULL
                        MB_OK MB_ICONSTOP or MB_TASKMODAL or
                        z" Error Notice!" rel>abs
                        message$ 1+ rel>abs
                        NULL call MessageBox drop
                        bye
                then    ;

variable cur-line
         cur-line off

INTERNAL        \ internal definitions start here

: do-edit       ( -- )
                editor-present?                         \ TRUE if editor is loaded
                if      cur-file count "path-file drop ed-filename place
                        cur-line @     ed-line !
                                       ed-column off
                        0 ED_OPEN_EDIT win32forth-message
                else    editor$ $exec drop
                then    ;

: do-browse     ( -- )
                editor-present?                         \ TRUE if editor is loaded
                if      cur-file count "path-file drop ed-filename place
                        cur-line @     ed-line !
                                       ed-column off
                        0 ED_OPEN_BROWSE win32forth-message
                else    browse$ $exec drop
                then    ;

: do-watch      { \ pocket$ -- }
                MAXSTRING LocalAlloc: pocket$            \ a place to preserve pocket
                pocket pocket$ MAXSTRING move            \ get current pocket contents
                editor-present?                         \ TRUE if editor is loaded
                if      cur-file count "path-file 0=
                        if      ed-filename place
                        else    2drop
                                cur-file count ed-filename place
                        then
                        watched-cfa >name nfa-count ed-name place
                        cur-line @     ed-line !
                                       ed-column off
                        0 ED_WATCH win32forth-message
                else    ed-ptr
                        if      watched-cfa >name nfa-count ed-name place
                        then
                        browse$ $exec drop      \ startup the editor
                then
                pocket$ pocket MAXSTRING move ;  \ restore contents of pocket

: [$edit]       { line_number file_name edit_cfa -- }
                file_name -1 <>
                if      file_name count bl skip 2dup bl scan 2dup 2>r nip -
                        "CLIP" cur-file place
                        2r> bl skip dup
                        if      number? 2drop 1 max to line_number
                        else    2drop
                        then
                        line_number &linenum !          \ set the line# variables
                        line_number cur-line !
                        edit_cfa execute                \ execute the editor
                then    ;

EXTERNAL        \ external definitions start here

: $edit         ( line filename | dummy -1 -- )
                ['] do-edit   [$edit] ;

: $browse       ( line filename | dummy -1 -- )
                ['] do-browse [$edit] ;

INTERNAL

: _$watch       ( line filename -- )
                ['] do-watch  [$edit] ;

' _$watch is $watch     \ link watch into the debugger

in-system

: locate-height ( -- n1 )
                getcolrow nip 8 - 20 min ;

: locate-header ( -- n1 )
                locate-height 4 / ;

-1 value orig-loc

: $locate       ( line# filename | dummy -1 -- )
                { line# file$ \ loc$ locHdl lcnt -- }
                file$ 0< ?EXIT
                max-path LocalAlloc: loc$
                file$ $open abort" Couldn't open source file!"
                to locHdl
                0 to lcnt
                base @ >r decimal
                cls >bold ." From file: " cur-file count type
                ."  At line: " line# . line# cur-line !
                cr horizontal-line
                line# locate-header - 0 max 0
                ?do     loc$ MAXCOUNTED locHdl read-line
                        abort" Read Error"
                        nip 0= ?leave
                        1 +to lcnt
                loop
                locate-height 0
                do      loc$ dup MAXCOUNTED locHdl read-line
                        abort" Read Error"
                        if      cols 1- min
                                1 +to lcnt
                                lcnt orig-loc =
                                if      horizontal-line
                                        >bold type cr
                                        horizontal-line
                                else          type cr
                                then
                                getxy nip getcolrow nip 4 - >
                                ?leave
                        else    2drop leave
                        then
                loop    horizontal-line
                locHdl close-file drop
                r> base ! ;

in-application

EXTERNAL

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\       handle error returned by window functions
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

defer win-abort ' abort is win-abort

: ?win-error    ( f1 -- )       \ f1=0=failed
                0=
                ?win-error-enabled and
                if      false to ?win-error-enabled
                        debug-io
                        cr ." On Function: "
                        r@ abs>rel 2 cells - @ .proc-name 
                        ."  Windows Returned Error:"
                        Call GetLastError .
                        tabbing-off
                        forth-io
                        win-abort
                        restore-io
                then    ;

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\       A utility to allow invoking a DOS shell on a following commandline
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

INTERNAL

create shell-name$ ," SHELL.BAT"  MAXSTRING allot-to
create shell-buf                  MAXSTRING allot

0 value ?shell-pause

EXTERNAL

: $shell        ( a1 -- )
                dup c@
        if      shell-name$ count w/o create-file       \ make the file
                abort" Couldn't create SHELL.BAT"
                >r                                      \ save file handle
                ( a1 ) count r@ write-file drop         \ write commandline
                crlf$  count r@ write-file drop         \ line terminator
                ?shell-pause
          if    s" PAUSE"    r@ write-file drop         \ wait for results
                crlf$  count r@ write-file drop         \ line terminator
          then  r> close-file drop                      \ close the file
                shell$      count shell-buf  place      \ the command
                shell-name$ count shell-buf +place      \ append batch name
                                  shell-buf +NULL
                                  shell-buf 1+ zEXEC drop \ perform command
        else    drop
                ?shell-pause
                if      dos$ +NULL
                        dos$ 1+ zEXEC drop
                then
        then    ;

: shell         ( -<string>- )
                true to ?shell-pause
                0 word $shell ;

synonym `   shell
synonym sys shell

: dos           ( -<string>- )
                false to ?shell-pause
                0 word $shell ;

: copyfile      { \ from$ to$ -<from to>- }     \ copy a file to a directory
                max-path localAlloc: from$
                max-path localAlloc: to$
                /parse-s$ count from$  place
                /parse-s$ count to$    place
                              to$    ?+\
                from$ count "to-pathend" to$ +place
                from$ +NULL
                to$   +NULL
                cr ." Copying: " from$  count type
                cr ."      To: " to$    count type
                false
                to$    1+ rel>abs
                from$  1+ rel>abs
                Call CopyFile 0=
                abort" The COPY Failed!" ;

in-system

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\       more primitive utilities to support view, browse and edit
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

                                                \ a1=cfa, a2=loadfile string
: $.viewinfo    ( a1 -- line filename )
                get-viewfile 0= abort" Undefined word!"
                ."  loaded from: " over >view @ 0<
                if      ." CONSOLE" 2drop 0 -1
                else    base @ >r decimal
                        dup ?uppercase count type 15 ?cr
                        ."  at line: "
                        swap >view @ dup . swap
                        r> base !
                        dup count cur-file place
                then    ;

: .viewinfo     ( -<name>- line filename )
                bl word anyfind
                if       $.viewinfo
                else    c@ abort" Undefined word!"
                        cur-line @ cur-file
                then    over to orig-loc ;

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\       highlevel words used to view, browse and edit words and file
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

: where         ( -<name>- )    \ tell me WHERE the source for a word is
                .viewinfo drop cur-line ! ;


synonym .v where

: locate        ( -<name>- )    \ show some source lines of word
                .viewinfo $locate ;

synonym l  locate
synonym ll locate
synonym loc locate

synonym APPEND +PLACE

\ returns the name of the current folder, excluding path information
: GetFolderName ( -- c-addr n )   
   current-dir$ count 
   begin
      2dup [char] \ scan
   ?dup while  
      2swap 2drop  1 /string
   repeat  
   drop  0 max 
;

\ loads the file "folder.f" where folder is the current folder name
: tt ( -- )   
   GetFolderName
   pad PLACE s" .f" pad +PLACE  pad COUNT
   2dup cr ." include "  type 2 spaces 
   included 
;

: n             ( -- )          \ show the next bunch of lines
                cur-line @ locate-height 4 - + cur-file $locate ;

: b             ( -- )          \ show the previous bunch of lines
                cur-line @ locate-height 4 - - 0 max cur-file $locate ;

: linelist      ( n1 -- )
                cur-file $locate ;

: view          ( -<name>- )    \ VIEW the source for a word
                .viewinfo $browse ;

synonym v view                  \ V is an synonym for VIEW


: e             ( -<name>- )    \ EDIT the source for a word
                .viewinfo $edit ;

synonym ed e                    \ E is a synonym for EDIT
synonym g e                    \ E is a synonym for EDIT

: edit          ( -<filename>- ) \ EDIT a particular file
                0 word c@
                if      cur-line off
                        0 pocket
                else    cur-line @ cur-file
                then    $edit ;

synonym z edit                  \ Z is a synonym for EDIT

: browse        ( -<filename>- ) \ BROWSE a particular file
                0 word c@
                if      cur-line off
                        0 pocket
                else    cur-line @ cur-file
                then    $browse ;

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\       Compiler utilities
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\       Utility to allow loading a file starting at a specified line number
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

: #fload        ( n1 -<name>- )         \ load file "name" from line n1, 1 based
                start-line !                    \ set start line
                /parse-s$ $fload ;                \ do the load

: lineload      ( n1 -- )               \ load the current file from line n1
                start-line !
                cur-file $fload ;

in-application

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ Useful additions
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ SPLIT divides a string at a given character. The first part of the
\   string is on top, the remaining part underneath. The remaining part
\   begins with the scanned-for character.

: SPLIT ( addr len char -- addr len addr len )
   >R 2DUP R> SCAN ROT OVER - >R ROT R> ;

\ Search the string specified by a1 u1 for 
\ the string specified by a2 u2
\ flag = true match found a3 u3 
\ flag = false no match return a1 u1 

: thirdelem ( -- u )   2over nip ;
: (s=) ( a1 u1 a2 u2 -- f )   drop over CAPS-COMPARE 0= ;

: SEARCH(NC) ( a1 u1 a2 u2 -- a3 u3 f ) 
   thirdelem over < if 
      2drop false exit 
   then 
   2over 
   begin 
      thirdelem over <= while 
      2over 2over (s=) if 
         2>r 2drop 2drop 2r> true  exit 
      then 
      1 /string 
   repeat 
   2drop 2drop false 
;


\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\       Linkage to automatically invoke the editor on a compile error
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

: _edit-error   ( -- )
                loadline @ loadfile @ cell+ $edit ;

: autoediton    ( -- )  \ link into deferred auto edit on error word
                ['] _edit-error is edit-error ;

autoediton

: autoeditoff   ( -- )  \ disable automatic edit on error
                ['] noop is edit-error ;

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\       Display the amount of used and available program memory
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

: .free         ( -- )
                base @ decimal
                cr ." Application address:   "   &origin             h.8  ." h"
                cr ."               Total: " &app-size @               10 u,.r
                cr ."                Used: " here  &origin -           10 u,.r
                cr ."                Free: " app-free                  10 u,.r
                cr ." System      address:   "   sys-origin          h.8  ." h"
                cr ."               Total: " &sys-size @               10 u,.r
                cr ."                Used: " sys-here sys-origin -     10 u,.r
                cr ."                Free: " sys-free                  10 u,.r
                cr ." Malloc/Pointer  Mem: " tot-malloc                10 u,.r
                base ! ;

synonym .mem .free

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\       Display the mount of memory used by the following command line
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

: used          ( -- )
                sys-free >r app-free >r
                interpret
                cr
                r> app-free - ." Used App mem: " dup 1 u,.r
                r> sys-free - ."  Sys mem: "     dup 1 u,.r
                + ."  Total: " 1 u,.r ;

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\       A simple error number extension to error handling
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

: ?error        ( f1 n1 -- )    \ abort with error code n1 if f1=true
                swap
                if      throw
                else    drop
                then    ;

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\       ANSI Save and Restore Input Functions
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

: save-input    ( -- xxx 7 )
                loadfile @ cell+
                ?loading @
                loadline @
                >in @
                source-id
                (source) 2@
                7 ;

: restore-input ( xxx 7 -- f1 )
                drop
                (source) 2!
                to source-id
                >in !
                loadline !
                ?loading !
                align linkfile 0 ;

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\       Compile time stack depth checking
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

synonym checkstack nostack1

: nostack       ( -- )
                -1 to olddepth ;

: stack-empty?  ( -- )
                depth abort" The stack should have been empty here!" ;

: _stack-check  ( -- )
                ?loading @ 0=           \ if we are not loading
                state @ or              \ or we are in compile state,
                                        \ then don't check stack depth change
                olddepth 0< or ?exit    \ or if olddepth is below zero
                                        \ or if assembling
                context @ [ ' assembler vcfa>voc ] literal = ?exit
                depth olddepth >        \ if stack depth has increased
                if                      \ then warn of extra item on stack
                        cr ." Stack depth increased in file: "
                        loadfile @ cell+ count type
                        ."  at line: " base @ decimal loadline @ . base !
                        ." Stack: " .s cr
                then    depth to olddepth ;

nostack ' _stack-check is stack-check

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\       A word to allow defining a word into the Forth vocabulary
\       without changing the vocabulary search order
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

\ : F:          ( -<name>- )    \ define a word in the FORTH vocabulary
\               current @ >r                    \ save CURRENT
\               ['] forth vcfa>voc current !    \ set to FORTH
\               header                          \ make a header
\               r> current !                    \ restore current
\               !csp POSTPONE docol ] ;         \ switch to compiling

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\       Time control words
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

create TIME-BUF
here            nostack1
        0 w,    \ +0  year
        0 w,    \ +2  month
        0 w,    \ +4  day of week
        0 w,    \ +6  day of month
        0 w,    \ +8  hour
        0 w,    \ +10 minute
        0 w,    \ +12 second
        0 w,    \ +14 milliseconds
here swap - constant TIME-LEN

create date$ 32 allot
create time$ 32 allot

: get-local-time ( -- )         \ get the local computer date and time
        time-buf rel>abs call GetLocalTime drop ;

create compile-version time-len allot   \ a place to save the compile time

get-local-time                          \ save as part o compiled image

time-buf compile-version time-len move  \ move time into buffer

: time&date     ( -- sec min hour day month year )
                get-local-time
                time-buf 12 + w@        \ seconds
                time-buf 10 + w@        \ minutes
                time-buf  8 + w@        \ hours
                time-buf  6 + w@        \ day of month
                time-buf  2 + w@        \ month of year
                time-buf      w@ ;      \ year

: .#"           ( n1 n2 -- a1 n3 )
                >r 0 <# r> 0 ?do # loop #> ;

: >date"        ( time_structure -- )
                dup  2 + w@ 0 (d.) date$  place s" /" date$ +place \ month
                dup  6 + w@ 0 (d.) date$ +place s" /" date$ +place \ day
                         w@ 0 (d.) date$ +place                    \ year
                date$ count ;

: .date         ( -- )
                get-local-time time-buf >date" type ;

create months   ," January "
                ," February "
                ," March "
                ," April "
                ," May "
                ," June "
                ," July "
                ," August "
                ," September "
                ," October "
                ," November "
                ," December "
                0 ,

create days     ," st, " ," nd, " ," rd, " ," th, " 0 ,

: >month,day,year" ( time_structure -- )
                dup  2 + w@ 1- 0max 11 min months swap 0
                ?do     count + 1+ aligned
                loop    count          date$  place
                dup  6 + w@ dup 0 (d.) date$ +place
                dup 20 > if 10 mod then
                1- 3 umin days swap 0
                ?do     count + 1+ aligned
                loop    count          date$ +place
                         w@     0 (d.) date$ +place
                date$ count ;

create monthsShort
   ," Jan "
   ," Feb "
   ," Mar "
   ," Apr "
   ," May "
   ," Jun "
   ," Jul "
   ," Aug "
   ," Sep "
   ," Oct "
   ," Nov "
   ," Dec "
   0 ,

: >monthShort,day,year" ( time_structure -- )
                dup  2 + w@ 1- 0max 11 min monthsShort swap 0
                ?do     count + 1+ aligned
                loop    count          date$  place
                dup  6 + w@ dup 0 (d.) date$ +place
                dup 20 > if 10 mod then
                1- 3 umin days swap 0
                ?do     count + 1+ aligned
                loop    count          date$ +place
                         w@     0 (d.) date$ +place
                date$ count ;


: .month,day,year ( -- )
                get-local-time time-buf >month,day,year" type ;

: >time"        ( time_structure -- )
                dup  8 + w@ 0 (d.) time$  place s" :" time$ +place \ hours
                dup 10 + w@ 2 .#"  time$ +place s" :" time$ +place \ minutes
                    12 + w@ 2 .#"  time$ +place                    \ seconds
                time$ count ;

: .time         ( -- )
                get-local-time time-buf >time" type ;

: >am/pm"       ( time_structure -- )
                dup  8 + w@ dup>r dup 12 > if 12 - then
                                  0 (d.)        time$  place \ hours
                                   s" :"        time$ +place
                    10 + w@       2 .#"         time$ +place \ minutes
                r> 12 > if         s" pm"       time$ +place
                        else       s" am"       time$ +place
                        then
                time$ count ;

: .am/pm        ( -- )
                get-local-time time-buf >am/pm" type ;

: .cversion     ( -- )
   ." compiled: "
   compile-version dup >month,day,year" type
   drop \ ." , " >am/pm"          type 
;

: ms@           ( -- ms )
                get-local-time
                time-buf
                dup   8 + w@     60 *           \ hours
                over 10 + w@ +   60 *           \ minutes
                over 12 + w@ + 1000 *           \ seconds
                swap 14 + w@ + ;                \ milli-seconds

0 value start-time

: time-reset    ( -- )
                ms@ to start-time ;

' time-reset alias timer-reset

: .elapsed      ( -- )
                ." Elapsed time: "
                ms@ start-time -
                1000 /mod
                  60 /mod
                  60 /mod 2 .#" type ." :"
                          2 .#" type ." :"
                          2 .#" type ." ."
                          3 .#" type ;

: elapse        ( -<commandline>- )
                time-reset interpret cr .elapsed ;

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\      Random number generator for Win32Forth
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

3141592 value SEED1
6535897 value SEED2
9323846 value SEED3

: RANDOM        ( n1 -- n2 )    \ get a pseudo random number between 0 and n1 as n2
                dup 0= if 1+ then
                SEED1 177 /MOD  2*    SWAP  171 *  SWAP  - DUP  to SEED1
                SEED2 176 /MOD  35 *  SWAP  172 *  SWAP  - DUP  to SEED2
                SEED3 178 /MOD  63 *  SWAP  170 *  SWAP  - DUP  to SEED3
                +  + SWAP  MOD  ;

: RANDOM-INIT   ( -- )          \ initialize the random number generator
                get-local-time
                time-buf 3 cells + @ to SEED1
                time-buf 2 cells + @ to SEED2
                time-buf 1 cells + @ to SEED3 ;

INITIALIZATION-CHAIN CHAIN-ADD RANDOM-INIT      \ randomize at boot time

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\      Delay Time Words
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

: _MS           ( n1 -- )       \ delay n1 milli-seconds
                Win32s?         \ if Win32s then don't use "Sleep", it doesn't work
                if      ms@ + 15000 0                   \ max delay ~15 seconds
                        do      dup ms@ u< ?leave       \ check for all done
                                50 0                    \ just a small pause
                                do      WINPAUSE        \ to let OS have time
                                loop
                        loop    drop
                else    Call Sleep drop
                then    ;

' _MS IS MS

: SECONDS       ( n1 -- )
                0max 0
                ?do     10 0
                        do      100 ms
                                key?
                                if      key drop
                                        unloop
                                        unloop
                                        EXIT
                                then
                        loop
                loop    ;

: pause-seconds ( n1 -- )
                cr ." Delaying: " dup . ." seconds, press a key to HOLD "
                30 min 1 max 10 * 0
                ?do     100 ms
                        key?
                        if
        cr ." HOLDING,  Space=continue delaying, Enter=cancel pause, ESC=abort"
                                key     dup k_ESC =
                                if      cr ." Aborted" abort
                                then    K_CR = ?leave
                                key     dup k_ESC =
                                if      cr ." Aborted" abort
                                then    K_CR = ?leave
                                cr      ." Press a key to pause "
                        then
                loop    ;

synonym ?keypause  start/stop           \ from F-PC, pauses if a key is pressed

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\       Utility to type a file to the console
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

: "ftype        { \ locHdl typ$ -<name>- }   \ type file "name" to the console
                max-path LocalAlloc: typ$
                "open abort" Couldn't open file!"
                to locHdl
                cur-line off
                >bold cr ." Typing file: " open-path$ count type >norm cr
                begin   typ$ dup MAXCOUNTED locHdl read-line
                        abort" Read Error"
                        nuf? 0= and
                while   type cr
                        10 ms
                repeat  2drop
                locHdl close-file drop ;

: ftype         ( -<filename>- )
                /parse-s$ count "ftype ;

synonym flist ftype

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\       An addition to CASE OF ENDOF ENDCASE, to allow testing ranges
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

: _of-range     ( n1 n2 n3 -- n1 f1 )
                2 pick -rot between ;

: of-range      ( n1 n2 n3 -- n1 )      \ extension to CASE for a range
                ?comp POSTPONE _of-range POSTPONE ?branch >mark 4 ; immediate

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\       Help system interface words
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

  create help_file$ ," WIN32SDK.HLP" MAXSTRING allot-to
\ create help_file$ ," Api32.hlp" MAXSTRING allot-to

: $help-file    ( a1 -- )               \ set the name of the current help file
                help_file$ 256 erase            \ pre-clear help filename
                count help_file$ place ;        \ lay in new filename

: .help         ( -- )                  \ display the current help file string
                cr >bold ." HELP file: " >norm help_file$ count type
                cr ." Use: 'HELP-FILE <filename>' to select another help file. " ;

: help-file     ( -<filename>- )        \ specify a new help filename
                /parse-s$ $help-file ;

synonym set-help help-file

: $help         ( a1 -- )               \ help on a string
                1+ rel>abs              \ help subject word
                HELP_PARTIALKEY         \ the command to the help system
                                        \ pointer to a help file string
                help_file$ 1+ rel>abs   \ the help file to use
                conHndl                 \ tell help its from our window
                call WinHelp drop ;

: #help         ( n1 -- )               \ help on a help context index number
                HELP_CONTEXT            \ the command to the help system
                help_file$ 1+ rel>abs   \ the help file to use
                conHndl                 \ tell help its from our window
                call WinHelp drop ;

: help          ( -<word>- )
                bl word $help ;         \ help subject word

: help-index    ( -- )
                0
                HELP_INDEX
                help_file$ 1+ rel>abs   \ the help file to use
                conHndl                 \ tell help its from our window
                call WinHelp drop ;

: help-on-help  ( -- )                  \ get help on windows help
                0
                HELP_HELPONHELP
                help_file$ 1+ rel>abs   \ the help file to use
                conHndl                 \ tell help its from our window
                call WinHelp drop ;

INTERNAL

: _help-release ( -- )                  \ release our marker to help system
                0                       \ NULL
                HELP_QUIT               \ the command to the help system
                0                       \ NULL pointer to a help file string
                conHndl
                call WinHelp drop ;

UNLOAD-CHAIN CHAIN-ADD-BEFORE _HELP-RELEASE       \ add to termination chain

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\       mouse typing
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

: mxy>cxy       ( x y -- cx cy ) \ convert from mouse xy to character xy
                charwh rot 2>r / 2r> swap / ;

: char@screen   ( x y -- c1 )
                getmaxcolrow drop * + &the-screen + c@ ;

: word@mouse"   ( -- a1 n1 )
                &the-screen
                mousex mousey mxy>cxy getrowoff + getmaxcolrow drop * +
                2dup + c@ bl <>
        if      0 over
                ?do     over i + c@ bl =
                        if      drop i leave    \ found blank, leave loop
                        then
             -1 +loop                           \ a1=screen, n1=offset to blank
                getmaxcolrow * swap /string     \ -- a1,n1 of remaining screen
                bl skip                         \ remove leading blanks
                2dup bl scan nip -              \ return addr and length
        else    + 0
        then    ;


: word@mouse>keyboard ( -- )            \ send word at mouse to keyboard
                mouseflags double_mask and 0= ?exit \ double clicked mouse
                word@mouse" ?dup
                if      "pushkeys
                        bl pushkey    \ push a space
                else    drop
                then    ;

MOUSE-CHAIN CHAIN-ADD WORD@MOUSE>KEYBOARD

: line@mouse"   ( -- a1 n1 )
                &the-screen
                mousex mousey mxy>cxy getrowoff + swap >r   \ save x for later
                getmaxcolrow drop swap * + r>   \ -- a1,n1 the line upto mouse
                -trailing ;                     \ remove trailing blanks


: line@mouse>keyboard ( -- )            \ send the line at mouse to keyboard
                mouseflags 0xFF and 0x09 <> ?exit \ ctrl-left mouse button down
                                                \ along with the control key
                line@mouse" ?dup
                if      "pushkeys
                        0x0D pushkey    \ automatically press Enter
                else    drop
                then    ;

MOUSE-CHAIN CHAIN-ADD LINE@MOUSE>KEYBOARD

(( MOUSEFLAGS info:

        3               both  buttons, currently assigned to abort

        1               left  button
        9 control       left  button
       13 control shift left  mouse button
        5         shift left  mouse button

        2               right button
       14 control shift right mouse button
       10 control       right mouse button
        6         shift right mouse button

))

((
: exit-do-chain ( chain_address -- )
                begin   @ ?dup
                while   dup>r           \ make sure stack is clean during
                        cell+ @
                        execute         \ execution of the chained functions
                        r>              \ so parameters can be passed through
                repeat  ;               \ the chain of items being performed
))

EXTERNAL

: comment:      ( -<comment;>- )        \ all that follows is a comment
                                        \ till COMMENT; is encountered
                begin   bl word ?uppercase
                        dup count s" COMMENT;" compare
                while   c@ 0=
                        if      refill 0=
                                abort" missing COMMENT;"
                        then
                repeat  drop ; immediate

\ Make console the foreground window. Ignore error which will occur if we are
\ running under Windows95 and we are already the foreground window.
: _foreground-console   ( -- )
                        conhndl Call SetForegroundWindow drop ;

' _foreground-console is foreground-console

: make-cursor   ( cursor_constant appinst -- )
                create , ,
                does>  dup cell+ @ swap @
                if      APPINST
                else    NULL
                then    Call LoadCursor
                        Call SetCursor drop ;

\ Standard Win32 API Cursors

IDC_APPSTARTING FALSE   make-cursor appstarting-cursor
IDC_ARROW       FALSE   make-cursor arrow-cursor
IDC_CROSS       FALSE   make-cursor cross-cursor
IDC_HELP        FALSE   make-cursor help-cursor
IDC_IBEAM       FALSE   make-cursor ibeam-cursor
IDC_NO          FALSE   make-cursor noway-cursor
IDC_SIZEALL     FALSE   make-cursor sizeall-cursor
IDC_SIZENESW    FALSE   make-cursor sizenesw-cursor
IDC_SIZENS      FALSE   make-cursor sizens-cursor
IDC_SIZENWSE    FALSE   make-cursor sizenwse-cursor
IDC_SIZEWE      FALSE   make-cursor sizewe-cursor
IDC_UPARROW     FALSE   make-cursor uparrow-cursor
IDC_WAIT        FALSE   make-cursor wait-cursor

\ Cursors added from MFC

IDC_SPLITV      TRUE    make-cursor splitv-cursor
IDC_SPLITH      TRUE    make-cursor splith-cursor
IDC_MAGNIFY     TRUE    make-cursor magnify-cursor
IDC_HAND        TRUE    make-cursor hand-cursor
IDC_HARROW      TRUE    make-cursor harrow-cursor

: 2literal      ( d1 -- )
                swap POSTPONE LITERAL POSTPONE LITERAL ; immediate

: sliteral      ( a1 n1 -- )
                POSTPONE (s")
                here >r dup c, dup allot r@ 1+ swap move
                0 c, align r> count \n->crlf ; immediate

: MACRO                  ( "name <char> ccc<char>" -- )
                :
                char parse POSTPONE sliteral
                POSTPONE evaluate
                POSTPONE ; immediate ;

MODULE

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\       HTML linkage support
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

: "Web-Link     { adr len hWnd \ web$ -- }    \ open the Web link supplied, using the web browser
                LMAXSTRING LocalAlloc: web$
                                       web$ off                  \ reset buffer initially
                adr len bl skip -trailing to len to adr         \ remove leading & trailing blanks
                adr len 4 min s" WWW."    caps-compare 0=       \ if www. present
                adr len 5 min s" FILE:"   caps-compare 0= or    \ or http: present
                adr len 5 min s" HTTP:"   caps-compare 0= or    \ or http: present
                adr len 5 min s" NEWS:"   caps-compare 0= or    \ or news: present
                adr len 4 min s" FTP:"    caps-compare 0= or    \ or ftp: present
                adr len 4 min s" FTP."    caps-compare 0= or    \ or ftp. present
                adr len 7 min s" MAILTO:" caps-compare 0= or    \ or mailto: present
                len 0= or                                       \ or NULL string
                IF      adr len         web$  LPLACE            \ then pass through un-modified
                ELSE    s" www."        web$  LPLACE            \ else prepend "www."
                        adr len         web$ +LPLACE            \ append specified string
                        s" .com"        web$ +LPLACE            \ append ".com"
                THEN
                web$ +LNULL                                      \ null terminate string
                web$ @                                         \ if there is any thing there
                IF      0
                        NULL
                        NULL
                        web$ CELL+ rel>abs
                        z" open" rel>abs
                        hWnd Call ShellExecute drop             \ tell Windows we want this link
                ELSE    beep
                THEN    ;

: Web           ( -<www.???.com>- )     \ open the Web link specified, using the Web browser
                bl word count conhndl "Web-Link ;

only forth also definitions

