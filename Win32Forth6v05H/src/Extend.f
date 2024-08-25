\ Extend.f
\ extend the kernel
\ fixed set-shell
\ moved globalization to Utils.f -rbs

cr .( -- BETA META EXTEND.F V4.0A 20/11/2002 12:30:50 ) cr

\ modified from original - early load of assembler and winlib

    FLOAD SRC\version.f     \ load the VERSION# definition
    FLOAD SRC\primutil.f    \ primitive utilities
sys-FLOAD SRC\nforget.f
    FLOAD SRC\pointer.f
sys-FLOAD SRC\dthread.f     \ display threads
sys-FLOAD SRC\order.f       \ vocabulary support
sys-FLOAD SRC\module.f      \ scoping support for modules
sys-FLOAD SRC\interpif.f    \ interpretive conditionals
sys-FLOAD SRC\486asm.f      \ Jim's 486 assembler
sys-FLOAD SRC\asmmac.f      \ Jim's 486 macros
sys-FLOAD SRC\asmwin32.f    \ NEXT for Win32forth
    FLOAD SRC\winlib.f      \ windows stuff
sys-FLOAD SRC\vcall.f       \ virtual forth word call mechanism
    FLOAD SRC\callback.f    \ windows callback support
    FLOAD SRC\dbgsrc1.f     \ source level debugging support part one
    FLOAD SRC\ctype.f       \ 'c' style character typing
sys-FLOAD SRC\resforth.h    \ load the headerfile with a few constants
\ sys-fload src\optcomp       \ compiler options
\    FLOAD SRC\fcases.f      \ extended CASE structure
\    FLOAD SRC\assert.f
    FLOAD SRC\primhash.f    \ primitive hash functions for OOP later
    FLOAD SRC\see.f
    FLOAD SRC\debug.f
\    FLOAD SRC\build.f       \ experimental version of BUILD DO: defining wordset
\    FLOAD SRC\later.f
sys-FLOAD SRC\words.f
    FLOAD SRC\class.f       \ ***** Object Oriented Programming Support *****
    FLOAD SRC\scrnctrl.f    \ screen control words
    FLOAD SRC\registry.f    \ Win32 Registry support
    FLOAD SRC\ansfile.f     \ ansi file words
    FLOAD SRC\keyboard.f    \ function and special key constants
    FLOAD SRC\mapfile.f     \ Windows32 file into memory mapping words
    FLOAD SRC\environ.f     \ environment? support
    FLOAD SRC\lineedit.f    \ a line editor utility
sys-FLOAD SRC\transit.f     \ minimal transient support
    FLOAD SRC\utils.f       \ load other misc utility words
    fload src\exceptio.f    \ utility words to support windows exception handling
    FLOAD SRC\dbgsrc2.f     \ source level debugging support part two
only forth also definitions
sys-FLOAD SRC\dis486.f      \ load the disassembler
8 constant B/FLOAT      \ default to 8 byte floating point numbers
    FLOAD SRC\float.f       \ floating point support

\ ***** Object Oriented Support Continues *****

sys-FLOAD SRC\classdbg.f
    FLOAD SRC\colors.f
    FLOAD SRC\fonts.f       \ font class
    fload src\xfiledlg.f    \ xcall replacements for open dialogs
\   fload src\xprtdlg.f     \ xcall replacements for print dialogs
    FLOAD SRC\dc.f          \ device context class
    FLOAD SRC\generic.f     \ generic window class
    FLOAD SRC\window.f
    FLOAD SRC\childwnd.f    \ child windows
    FLOAD SRC\winmsg.f
    FLOAD SRC\control.f
    FLOAD SRC\controls.f
    FLOAD SRC\button.f
    FLOAD SRC\dialog.f
    FLOAD SRC\forthdlg.f
    FLOAD SRC\keysave.f
    FLOAD SRC\menu.f
    FLOAD src\v6_compat.f

: StartEditor  { \ file$ -- }
   MAXSTRING LocalAlloc: file$
   &prognam count "path-only" file$ place
                            file$ ?+\
   s" Win32For.exe"           file$ +place
                            file$ +NULL
                            file$ 1+ zEXEC ;

create hello$ 0 c, MAXSTRING allot-to

: hello+"       ( -<text">- )                   \ append to hello string
                '"' word count hello$ +place ;

hello+" Win32Forth: ANS Forth for Windows 95/98/ME/NT/2K/XP/7/8/10/11"

0 value &win32for.cfg

: config"       ( -<config_file_name">- )
                here to &win32for.cfg
                '"' word count Prepend<home>\ here place
                here c@ 1+ allot 0 c, ;

config" WIN32FOR.CFG"

\ ........ set the default system access strings ..........

\ -rbs make shell work in other drives
: set-shell
        winver winnt351 >=
        if      s" CMD.EXE "
        else    s" c:\command.com " then
        2dup dos$ place shell$ place
        s" /c " shell$ +place ;

: .mem-free     ( -- )
   app-free 512 + 1024 / 1 u,.r ." K bytes free" ;

in-system

: HELLO         { \ doing-app? -- }             \ startup stuff
   ?loading off                     \ we aren't loading initially
   only forth also definitions
   decimal
   source-id IF                     \ if loading a file, close it      
      source-id -1 <> IF  source-id close-file drop  THEN    
      0 to source-id
   THEN
   
   defer@ default-application ['] bye <> to doing-app?
   
   init-console                    \ -- f1
   dup                             \ init if we created a console
   IF    
      \ !!!! HAVE TO DO THE INITIALIZATION CHAIN    !!!!
      \ !!!! BEFORE WE USE ANY WINDOWS SYSTEM CALLS !!!!
      initialization-chain do-chain
   THEN

   ed-ptr 0<>                     \ if shared memory was initialised
   IF      
      ed-forth-count @ 1 >
      IF      
         TRUE to second-forth?
         TRUE
      ELSE    ed-console-remote @
         z" RMOT" @ = IF  do-remote-forth-io  THEN    \ select remote IO operations       
         ed-console-hidden @     \ allow external app to start Forth hidden
         z" HIDN" @ <>           \ test string for hidden
      THEN
   ELSE    
      FALSE                      \ if no mapped memory, then normal
   THEN

   IF      
      normal-console
   THEN

   exception@ 0=                   \ -- f1 f2
   doing-app? 0= and               \ if no app, display info
   IF      
      ed-console-remote @ z" RMOT" @ <> IF  cls  THEN
      hello$ count type
      .version 2 spaces .mem-free \ .cversion  \ .platform
      cr ." OS: " WinVer ShowWindowsVersion  2 spaces  ." Home directory: " s" Directory" GetSetting type
      set-shell
   THEN                            \ f1 --
   IF      
      doing-app? 0= IF              \ if no app, display more info, and load config file        
         \ cr ." Version " version# 0 <# # # # # '.' hold # # '.' hold  #s #> type 
         \ .version 2 spaces  .cversion  2 spaces .mem-free
         \ .words
         [ HIDDEN ] KERNEL.BIN-check [ FORTH ]
         &win32for.cfg count "path-file nip nip 0=  \ if found
         IF      
            &win32for.cfg ['] $fload catch ?dup
            IF      
               message
               sp0 @ sp!
               QUIT
            else
               \ cr ." Cannot find  Win32For.cfg  configuration file!!! " 
            THEN
         THEN
      else
         cr ." doing-app? is 0 "
      THEN
   ELSE    
      exception@
      IF      
         0 &cmdline !            \ reset commandline length
         .exception
      ELSE    
         \ cr ."  ** Warm Start Performed **"
         cr .mem-free
      THEN
   THEN

   exception@ 0=                   \ if no exception
   doing-app? and                  \ and have an application to run
   IF      default-application     \ then run the users application
      bye                     \ and terminate when it's done
   THEN  
;

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

with-img fsave Win32for         \ save Win32For.EXE and Win32For.IMG

1 pause-seconds

