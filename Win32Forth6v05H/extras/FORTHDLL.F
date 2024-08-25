\ FORTHDLL.F      Make a DLL Version of Memory Image            by Tom Zimmer

only forth also definitions

\LOADED- MESSAGES.F   cr .( MESSAGES.F should already be loaded, loading it..)
\LOADED- MESSAGES.F   fload MESSAGES.F

cr .( Loading FORTHDLL.F..)

((

This file is part two of DLL support for Win32Forth.  Part one is MESSAGES.F
which must be loaded first.  If contains definitions for defining messages and
DLL subfunction entry points like DLL_INTERPRET defined below.

Be sure to read the following notices, they can significantly effect your
ability to get a DLL to work.

NOTICE1: DLL construction can only be done under WindowsNT.  Only WindowsNT
         has the required system call support for writing a resource into a
         DLL or EXE file.  If you want to make DLLs, then you MUST buy
         WindowsNT to build on. DLLs built on WindowsNT will subsequently run
         under Windows95, but will not run under Windows 3.1 with Win32s or
         Win-OS/2 with Win32s. Bugs in Win32s fail to properly read a
         resource with a user defined type.


NOTICE2: All :DLLFunc's have the same stack arguments as described below.

         { lParam wParam message function -- }

         These local variables are created automatically by the ":DLLFunc"
         defining word. The comments on the same line or a :DLLFunc
         definition are comments designed for the user of the automatically
         generated C header file in MESSAGES.H, or the Basic equates file
         MESSAGES.BAS.

         The names above are available to be used in the :DLLFunc function to
         access the parameters described in the comments.

))

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\    DLL function entry point support, Example DLL sub function definiton
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

:DLLFunc DLL_INTERPRET  \ lParam = "Forth commandline string to interpret"
                lParam abs>rel @ ?dup           \ null string is NULL here
                if                              \ only if NOT a NULL string
                        abs>rel                 \ strings are double indirect
                        255 2dup 0 scan nip -   \ z to addr,len
                        DO-DLL evaluate
                else    1 to dll-result         \ flag all was OK
                then    ;


\ *** Example DLL entry, that displayes the Forth console, so it can show
\ *** results from a forth word being executed.
\ *** See code at the end of this file, for an example of calling this function.

:DLLFunc DLL_WORDS      \ show words in DLL
                unhide-console
                normal-console
                words
                1 to dll-result ;               \ flag all was OK

((

In the above code for dealing with strings, you will note the sequence;

        lParam abs>rel @ abs>rel

Strings in Visual Basic are passed as pointers to the actual string data.
When passing other types of data, then Visual Basic passes either the actual
value of the parameter passed, in which case you would use;

        lParam

This would give you the value passed, or if lParam can be a pointer to the
data as in a data structure. In this case, you would use the sequence;

        lParam abs>rel

Which would obtain the Forth relative address of the data structure passed.

))

\ !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
\ !!!!!!!!!!!!! Additional :DLLFunc definitions go     !!!!!!!!!!!!!!!!
\ !!!!!!!!!!!!! here or are loaded before this file is !!!!!!!!!!!!!!!!
\ !!!!!!!!!!!!! loaded and after MESSAGES.F is loaded. !!!!!!!!!!!!!!!!
\ !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

: DoForthFunc   { lParam WParam message function -- return }
                FALSE to dll-result                     \ initially set as failed
                lParam wParam message function DLLFunc-chain do-chain
                4drop                                   \ clean arguments off stack
                dll-result ;                            \ return the result

4 callback &DoForthFunc DoForthFunc     \ create the callback for DoForthFunc

: DoForthDLL-init ( -- )        \ must runtime init, because of ABSOLUTE address
                &DoForthFunc rel>abs &DLL-callback ! ; 

DoForthDLL-init

initialization-chain chain-add DoForthDLL-init

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\       DLL cold start initialization
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

: DLLhello      ( -- )                          \ Initialization stuff
                decimal
                ?loading off
                0 to source-id
                init-console                    \ -- f1
                dup                             \ init if we created a console
                if      initialization-chain do-chain
                then
                if
\ hide the console after debugging your DLL.
\                       hide-console
\                        unhide-console
\                        normal-console
                        TRUE to doing-dll?      \ flag to everyone, we are a DLL!
\       +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
\               All application initialization goes here.
\               Be sure to remember to return after initializaton
\               is finished or the DLL won't run properly.
\       +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
                else    unhide-console
                        normal-console
                        exception@
                        if      .exception
                        else    ."  ** Warm Start Performed **"
                                .mem-free
                        then
                then    ;


' DLLhello is boot

\ un-comment out the following line to remove the system dictionary from the
\ saved DLL.

\ 0 sys-free!           \ set system free space to zero, no system space

\ By inserting a debug statment here, just before the image save, you can
\ cause the DLL to drop into the Forth debugger when a call is made to a
\ word being used by a DLL subfunction.

\ debug evaluate

debug breaker

cr .( Saving the Forth Image into FORTHDLL.DLL..)

DOS COPY FORTHDLL.BIN FORTHDLL.DLL

cr .( Copied the master DLL image from FORTHDLL.BIN to FORTHDLL.DLL..)

resource-image-save FORTHDLL.DLL

.( .Done!)

\S ** STOP compiling here ***
\S ** STOP compiling here ***
\S ** STOP compiling here ***

\ ***   This section of code needs to be compiled onto Win32Forth, to access the
\       DLL we are building.


WinLibrary FORTHDLL.DLL                 \ make the DLL available to Forth

4 proc DLLForthEntry                    \ declare the DLL entry point
                                        \ this not really needed, but it can be done
fload MESSAGES.H

: DLLWORDS  ( -- )
        0 0 0 DLL_WORDS                 \ the four arguments to the DLL
        call DLLForthEntry drop ;       \ call the DLL_WORDS function
                                        \ in the Forth DLL

DLLWORDS                                \ execute 'words' out of the DLL
                                        \ wait for it to finish.....

BYE                                     \ leaving Win32Forth, also terminates the DLL





