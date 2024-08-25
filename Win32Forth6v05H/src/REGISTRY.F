(( registry.f   The Registry Interface for Win32Forth            by Tom Zimmer

        Andrew implemented the functionality in C, and I translated it
        into Forth

))
\ primutil.f beta 2.0C 2002/08/31 arm (minor) use ANS file words replaceing FXXX-FILE
\ primutil.f beta 2.9G 2002/09/24 arm release for testing
\ primutil.f beta 3.3D 2002/10/08 arm Consolidation

cr .( Loading Windows Registry...)
cr .( -- BETA REGISTRY.F V2.9G --)

TRUE value check-registry-directory?

\ Default registry key.  Change this string to put your programs registry
\ information in a place other than "Win32For\". Look for PROGREG in WinEd
\ for an example of how to change the program base registry key to a value
\ that will be specific not only to your program, but to the particular
\ directory instance of your program that is running.

create PROGREG ," Win32For\"   MAX-PATH allot-to

INTERNAL

  create basereg ," SOFTWARE\"
variable disposition
variable regkey
variable regtype
variable reglen

named-new$ ReturnedKey$

EXTERNAL

\ sadr,slen = the registry section to get the key of
\ return -1 if we could not get the key

: RegGetKey     { sadr slen \ key$ -- regkey | -1 }  \ read the key of a section
                MAXSTRING localAlloc: key$
                basereg count         key$  place
                progreg count         key$ +place
                sadr slen             key$ +place
                                      key$ +NULL
                disposition rel>abs         \ we get it, but don't use it
                regkey      rel>abs         \ the return value
                NULL
                KEY_ALL_ACCESS
                REG_OPTION_NON_VOLATILE
                NULL
                0
                key$ 1+  rel>abs
                HKEY_CURRENT_USER
                Call RegCreateKeyEx 
                if      -1
                else    regkey @
                then    ;


\ read registry key value string 'vadr,vlen' 
\ from            section string 'sadr,slen'
 \ return             data string 'dadr,dlen'

\ sadr,slen = the registry key section string
\ vadr,vlen = the registry key value string to read
\ dadr,dlen = the registry key data string returned

: RegGetString  { vadr vlen sadr slen -- dadr dlen }
                ReturnedKey$ off                \ initially clear return buffer
                sadr slen RegGetKey dup -1 =
                if      drop
                        ReturnedKey$ count
                        EXIT                    \ return on error, empty data
                then    >r
                MAXCOUNTED reglen !             \ init max length of string

                reglen          rel>abs
                ReturnedKey$ 1+ rel>abs
                regtype         rel>abs
                0
                vadr            rel>abs
                r@
                Call RegQueryValueEx
                if      ReturnedKey$ off
                else    reglen @ 1- 0max ReturnedKey$ c!
                then    ReturnedKey$ count
                r> Call RegCloseKey drop ;

\ Write to the registry, a key value string 'vadr,vlen'
\ in                         section string 'sadr,slen'
\ the                           data string 'dadr,dlen'

: RegSetString  { dadr dlen vadr vlen sadr slen \ val$ khdl -- }
                MAXSTRING localAlloc: val$      \ allocate a dynamic string
                dlen "CLIP" to dlen             \ clip key to 255 characters
                sadr slen RegGetKey to khdl
                khdl -1 =
                if      EXIT                    \ just return, ignore error
                then
                dadr dlen val$ place
                          val$ +NULL
                dlen 1+                         \ data length including NULL
                val$ 1+ rel>abs                 \ null terminated data string
                REG_SZ
                0
                vadr    rel>abs
                khdl Call RegSetValueEx drop 
                khdl Call RegCloseKey drop ;

: SetSetting    ( a1 n1 a2 n2 -- )      \ a1,n1=value string, a2,n2=key string
                s" Settings" RegSetString ;

: GetSetting    ( a1,n1 -- a2 n2 )      \ a1,n1-key string, a2,n2=value string
                s" Settings" RegGetString ;

create &forthdir max-path allot         \ static forth installation directory
       &forthdir off

: current-dir$  ( -- a1 )       \ get the full path to the current directory
                new$ dup 1+ rel>abs
                MAXCOUNTED call GetCurrentDirectory over c! ;

: $current-dir! ( a1 -- f1 )    \ a1 is a null terminated directory string
                rel>abs call SetCurrentDirectory ;

: chdir         ( -<optional_new_directory>- )
                /parse-word dup c@
                if      dup 1+ $current-dir! drop
                then    drop
                cr ." Current directory: " current-dir$ count type ;

synonym cd chdir

INTERNAL

: program-name-init ( -- )
                MAXCOUNTED &prognam 1+ rel>abs
                0 call GetModuleHandle ?dup
                if      call GetModuleFileName
                else    2drop 0
                then    &prognam c! ;

program-name-init       \ initialize the program name buffer

initialization-chain chain-add program-name-init

: KERNEL.BIN-check ( -- )
                have-kernel.bin? ?EXIT
                cr ." ******************** Warning! *********************"
                cr ." Kernel image file; KERNEL.BIN was not found!"
                cr ." FSAVE & TURNKEY won't work unless file: KERNEL.BIN"
                cr ." is copied into directory: "
                                        &forthdir count 2dup upper type
                cr ." If you have moved Win32Forth to another directory,"
                cr ." then you can type: 'RE-REGISTER <enter>' to"
                cr ." make Win32Forth re-register itself in the registry."
                cr ." Reinstalling Win32Forth will also fix this problem."
                cr ." NOTE: KERNEL.BIN is a copy of FKERNEL.EXE"
                cr ." ******************** Warning! *********************"
                cr beep ;


: init-Win32fDirectory { \ kernel$ -- }
                max-path LocalAlloc: kernel$
                TRUE to have-kernel.bin?
                check-registry-directory? 0= ?EXIT
                s" Version" GetSetting number?  \ Get the installed version#
                2drop version# =                \ if it matchs my version#
                if      s" Directory" GetSetting nip 0= \ has directory been set?
                        if      &prognam count "path-only" &forthdir place
                                &forthdir count s" Directory" SetSetting
                        else    s" Directory" GetSetting &forthdir place
                        then
                else                                    \ make me the installed version
                        version# (.)               s" Version"   SetSetting
                        &prognam count "path-only" &forthdir place
                        &forthdir count            s" Directory" SetSetting
                then
                &forthdir c@ 0=                 \ if no dir specified
                if                              \ make it the current dir
                        current-dir$ count &forthdir place
                then
                &forthdir c@                    \ append '\' if needed
                if      &forthdir ?+\
                then
                &forthdir count            kernel$   place
                s" KERNEL.BIN"             kernel$  +place
                kernel$ count r/o open-file      \ use ans file open
                if      drop                    \ discard dummy handle
                        FALSE to have-kernel.bin?
                else    close-file drop
                then
            ;

initialization-chain chain-add init-Win32fDirectory

EXTERNAL

: .registry     ( -- )
                cr ."   Win32Forth version:" s" Version"   GetSetting
                over 1 type ." ." 1 /string over 1 type ."  build: " 1 /string type
                cr ."       Home directory:" s" Directory" GetSetting type
                cr ."     Console location:" s" Console"   GetSetting type ;

: re-register   ( -- )  \ force the registry to point to myself
                TRUE to check-registry-directory?
                0 (.) s" Version" SetSetting    \ clear version number
                init-Win32fDirectory            \ re-init version
                .registry ;

re-register     \ force registration of myself

MODULE

\s      Example code for registry use

\ The following word is executed at compile time to setup the
\ current programs base registry key.

: app-key-init  ( -- )  \ intialize the program base registry key
                s" MyApplication\" progreg place

app-key-init

: test          ( -- )
                s" WindowPosition" GetSetting type ;

\ Write the 'WindowPosition" value in section 'Settins' 
\ to a data string of '5,9' which would presumably be the x,y coordinate
\ of where the window should be placed next time the application starts up.

: test!         ( -- )
                s" 5,9" s" WindowPosition" SetSetting ;


