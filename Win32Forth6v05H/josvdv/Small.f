\ This file is a copy of extend.f
\ Many sources are disabled. Enable or disable them as you wish.
\ Copy this file to your win32for directory

\ See playsnd.f to see how it can be used.

\ extend the kernel

      fload version.f     \ load the VERSION# definition
      fload primutil.f    \ primitive utilities
    \ fload xdebug.f      \ a slightly simpler debugger loadable early
  sys-fload nforget.f
      fload pointer.f
      fload dbgsrc1.f     \ source level debugging support part one
  sys-fload dthread.f     \ display threads

 sys-fload order.f        \ vocabulary support

sys-fload module.f        \ scoping support for modules

    \ fload ctype.f       \ 'c' style character typing
sys-fload interpif.f      \ interpretive conditionals
sys-fload resforth.h      \ load the headerfile with a few constants
sys-fload transit.f       \ minimal transient support
    \ fload fcases.f      \ extended CASE structure
    \ fload assert.f
      fload primhash.f    \ primitive hash functions for OOP later
      fload see.f
      fload debug.f
    \ fload build.f       \ experimental version of BUILD DO: defining wordset
      fload later.f

      fload winlib.f      \ windows stuff
      fload callback.f    \ windows callback support
  sys-fload words.f
      fload class.f       \ ***** Object Oriented Programming Support *****
    \ fload scrnctrl.f    \ screen control words
      fload registry.f    \ Win32 Registry support
      fload ansfile.f     \ ansi file words
      fload keyboard.f    \ function and special key constants
      fload mapfile.f     \ Windows32 file into memory mapping words
    \ fload environ.f     \ environment? support
    \ fload lineedit.f    \ a line editor utility
      fload utils.f       \ load other misc utility words
      fload dbgsrc2.f     \ source level debugging support part two
      fload exceptio.f    \ utility words to support windows exception handling
\ sys-fload 486asm.f      \ Jim's 486 assembler
\ sys-fload asmmac.f      \ Jim's 486 macros
\ sys-fload asmwin32.f    \ NEXT for Win32forth
only forth also definitions
\ sys-fload dis486.f      \ load the disassembler
\ 8 constant B/FLOAT      \ default to 8 byte floating point numbers
    \ fload float.f       \ floating point support

\ ***** Object Oriented Support Continues *****

\ sys-fload classdbg.f
      fload colors.f
      fload fonts.f       \ font class
      fload dc.f          \ device context class
      fload generic.f     \ generic window class
    \ fload window.f
    \ fload childwnd.f    \ child windows
    \ fload winmsg.f
    \ fload control.f
    \ fload controls.f
    \ fload button.f
      fload dialog.f
      fload forthdlg.f
    \ fload keysave.f
    \ fload menu.f

create hello$ 0 c, MAXSTRING allot-to

: hello+"       ( -<text">- )                   \ append to hello string
                '"' word count hello$ +place ;

hello+" 32bit Forth for Windows 95, 98, and NT"

0 value &win32for.cfg

: config"       ( -<config_file_name">- )
                here to &win32for.cfg
                '"' word count here place
                here c@ 1+ allot 0 c, ;

config" WIN32FOR.CFG"

\ ........ set the default system access strings ..........

: set-shell
        nt?
        if      s" CMD.EXE /c "
        else    s" \command.com /c "
        then    shell$ place
        nt?
        if      s" CMD.EXE "
        else    s" \command.com "
        then    dos$ place  ;

: .mem-free     ( -- )
                cr app-free 1000 / 1 u,.r ." k bytes free" ;

in-system

: HELLO         { \ doing-app? -- }             \ startup stuff
                ?loading off                    \ we aren't loading initially
                only forth also definitions
                decimal
                source-id                       \ if loading a file, close it
                IF      source-id -1 <>
                        IF      source-id fclose-file drop
                        THEN    0 to source-id
                THEN
                defer@ default-application ['] bye <> to doing-app?
                init-console                    \ -- f1
                dup                             \ init if we created a console
                IF      \ !!!! HAVE TO DO THE INITIALIZATION CHAIN    !!!!
                        \ !!!! BEFORE WE USE ANY WINDOWS SYSTEM CALLS !!!!
                        initialization-chain do-chain
                THEN
                ed-ptr  0<>                     \ if shared memory was inited
                IF      ed-forth-count @ 1 >
                        IF      TRUE to second-forth?
                                TRUE
                        ELSE    ed-console-remote @
                                z" RMOT" @ =
                                IF      do-remote-forth-io \ select remote IO operations
                                THEN
                                ed-console-hidden @     \ allow external app to start Forth hidden
                                z" HIDN" @ <>           \ test string for hidden
                        THEN
                ELSE    FALSE                           \ if no mapped memory, then normal
                THEN
                IF      normal-console
                THEN
                exception@ 0=                   \ -- f1 f2
                doing-app? 0= and               \ if no app, display info
                IF      ed-console-remote @ z" RMOT" @ <>
                        IF      cls
                        THEN
                        hello$ count type
                        \ .cversion \ .version .platform
                        set-shell
                THEN                            \ f1 --
                IF      doing-app? 0=           \ if no app, display more info
                                                \ and load config file
                        IF      .mem-free
                                \ .words
                                [ HIDDEN ] KERNEL.BIN-check [ FORTH ]
                                &win32for.cfg count "path-file nip nip 0=  \ if found
                                IF      &win32for.cfg ['] $fload catch ?dup
                                        IF      message
                                                sp0 @ sp!
                                                QUIT
                                        THEN
                                THEN
                        THEN
                ELSE    exception@
                        IF      0 &cmdline !            \ reset commandline length
                                .exception
                        ELSE    cr ."  ** Warm Start Performed **"
                                .mem-free
                        THEN
                THEN
                exception@ 0=                   \ if no exception
                doing-app? and                  \ and have an application to run
                IF      default-application     \ then run the users application
                        bye                     \ and terminate when it's done
                THEN    ;

\in-system-ok   ' hello is boot

in-application

here fence !

mark empty

cur-file off            \ clear the default file
cur-line off            \ clear the current line

\ The following two lines should ONLY be done just BEFORE a SAVE

\ 200000 app-free!        \ 200k of application space
\ 125000 sys-free!        \ 125k of system space (mostly for headers)

cr cr .( Extensions Loaded, ) count-words . .( words in dictionary)
.registry
cr

SaveInfo WIN32FOR.DBG

(( with-img fsave Win32for         \ save Win32For.EXE and Win32For.IMG

5 pause-seconds ))


