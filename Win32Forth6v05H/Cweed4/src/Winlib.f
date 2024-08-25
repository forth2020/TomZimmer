\ $Id: library.f 1.1 1994/04/01 07:53:13 andrew Exp $

\ winlib.f beta 1.9F 2002/08/13 arm major modifications
\ winlib.f beta 2.0A 07/09/2002 22:14:48 arm minor cosmetic
\ winlib.f beta 2.9G 2002/09/24 arm release for testing
\ winlib.f beta 3.3D 2002/10/08 arm Consolidation
\ winlib.f beta 4.9D 2002/10/08 arm OS support removed to PRIMUTIL.F

cr .( Loading Windows API Library...)
cr .( -- BETA WINLIB.F V4.9D --)


INTERNAL                        \ non-user definitions start here
in-application                  \ required for application

0x3000 to winarea-size          \ set larger for more undeclared procs if needed
winarea-size to winarea-len        
here winarea-ptr ! winarea-len allot

0 value winproc-count                      \ number of procs
0 value winproc-found                      \ that we found

\ ---------------------- Debugging/Info routines ----------------------

: (.libs)       ( lib-addr -- )
                dup H.8                         \ address
                tab dup lib>handle @ dup 0<>
                if h.8
                else drop ." -noload-"
                then
                tab dup lib>type c@
                case
                    0x01 of ." SYS"  endof   \ library types
                    0x02 of ." KERN" endof
                    0x00 of ." APP"  endof
                    drop
                endcase
                tab lib>name count type
                cr
                ;

: (.procs)      ( addr len proc-addr --  addr len )
                3dup proc>name count temp$ place temp$
                uppercase count 2swap search
                >r 2drop r>
                if
                  dup H.8 tab                       \ address
                  dup proc>name count type 40 col
                  dup proc>pcnt c@ dup 128 <
                  if . else drop then 45 col
                  dup proc>ep @
                  over proc>ep @ docall-later rel>abs =
                  if drop tab
                  else h.8
                    dup dup proc>lib @
                    case 0 of drop endof
                        -1 of ." -error!-" drop endof
                        55 col lib>name count type
                    endcase
                  then                            \ ep
                  cr
                  1 +to winproc-found
                then
                drop
                1 +to winproc-count
                ;

EXTERNAL

: .libs         ( -- )
                tab-size >r
                10 to tab-size
                cr
                ." Location  Handle    Type      Name" cr
                ." --------  --------  ----      -----------" cr
                ['] (.libs) winlib-link do-link
                r> to tab-size ;

: .procs        ( <optional name> -- )
                { \ str$ }
                MAXSTRING LocalAlloc: str$
                0 to winproc-count
                0 to winproc-found
                cr
                tab-size >r
                10 to tab-size
                ." Location  ProcName" 40 col ." Prm  ProcEP    LibName"  cr
                ." --------  --------" 40 col ." ---  --------  --------" cr
                bl word count str$ place str$ uppercase count \ get optional name
                ['] (.procs) winproc-link do-link
                2drop
                ." Allocated Space: " winarea-size 8 u,.r ."  bytes" cr
                ." Free Space Left: " winarea-len 8 u,.r  ."  bytes" cr
                ." Displayed " winproc-found . ." of "
                winproc-count . ." procedures defined" cr
                r> to tab-size
                ;


\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\       Minimal Error Box Functon
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

: ErrorBox      ( a1 n1 -- )
                pad place
                pad +NULL
                8192 16 + ( MB_TASKMODAL MB_ICONERROR )
                z" Application Error" rel>abs
                pad count drop rel>abs
                NULL Call MessageBox drop ;

INTERNAL                                        \ more non-user definitions

\ fload iid.f                                   \ commented
\ -------------------- Load Standard Libraries --------------------

\ Root kernel library
WinLibrary KERNEL32.DLL
WinLibrary GDI32.DLL                            \ libraries
WinLibrary ADVAPI32.DLL
WinLibrary SHELL32.DLL
WinLibrary USER32.DLL

\ -------------------- Windows Constant Server --------------------

WinLibrary WINCON.DLL
winlib-last @ constant WinConLib
3 proc FindWin32Constant
winproc-last @ constant WinConPtr      \ for **WORDS.F**
3 proc EnumWin32Constants
winproc-last @ constant WinEnumPtr      \ for **WORDS.F**

\ Linkage for Windows Constant server through the number chain

: wincon-number? ( a1 n1 f1 -- d1 TRUE | a1 n1 FALSE )
                { \ con$ WinVal -- }
                MAXSTRING LocalAlloc: con$      \ allocate a buffer the con name
                dup ?EXIT drop
                2dup con$ place                 \ lay string in buffer
                2dup swap rel>abs
                &LOCAL WinVal rel>abs -rot      \ under adr & len
                Call FindWin32Constant          \ find it
                IF      2drop
                        WinVal 0 TRUE           \ return constant, zero and TRUE
                ELSE    s" A" con$ +place       \ append an 'A'
                        con$ count swap rel>abs
                        &LOCAL WinVal rel>abs -rot      \ under adr & len
                        Call FindWin32Constant  \ find it
                        IF      2drop
                                WinVal 0 TRUE
                        ELSE    FALSE
                        THEN
                THEN    ;

number?-chain chain-add wincon-number?          \ windows constant server

4 PROC DefWindowProc

: _DefaultWindowProc ( hwnd msg wparam lparam -- res )
                swap 2swap swap call DefWindowProc ;

' _DefaultWindowProc is DefaultWindowProc

EXTERNAL

\ April 23rd, 1999 - 14:05 tjz
\ not updated like WINCON-NUMBER? to succeed on ANSI versions, because
\ this only needs to detect WM_ constants for OOP.

: IsWinConstant ( str -- str FALSE | val TRUE )
                { \ WinVal -- }
                &LOCAL WinVal rel>abs over
                count swap rel>abs Call FindWin32Constant
                if      drop
                        WinVal TRUE
                else    FALSE
                then    ;

\ Only WINED uses the following functions.

: NT?           ( -- fl )       \ Retained for compatability -- deprecated
                WinVer 4 >= ;   \ NT3.51 or above

: Win95?        ( -- f1 )       \ Retained for compatability -- deprecated
                winver 1 = ;

: Win98?        ( -- f1 )       \ Retained for compatability -- deprecated
                winver 2 3 between ;

: Win32s?       ( -- f1 )       \ Retained for compatability -- deprecated
                false ;         \ no longer supported

MODULE          \ finishup the module

