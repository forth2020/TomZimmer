\ WINPAR.F       A Simple Parallel Port Output Program       by Tom Zimmer

\ A Win32Forth example program that illustrates how to open LPT1 for
\ bi-directional communications and then prints some characters on
\ any printer that happens to be connected there.

0 value ParHndl
                                
: LPT1Init      ( -- )          \ Initialize LPT1
                cr ." Initializing Parallel Port"
                NULL                            \ no template
                FILE_ATTRIBUTE_NORMAL           \ open file attributes
                OPEN_EXISTING                   \ creation distribution
                NULL                            \ no security attributes
                0                               \ exclusive access
                GENERIC_READ GENERIC_WRITE or   \ desired access modes
                z" LPT1" rel>abs                \ filename
                Call CreateFile ?dup 0= Abort" Failed to open LPT1"
                to ParHndl                      \ save the handle to the port
                ;

: LptClose      ( -- )                          \ close com port if its open
                ParHndl ?dup
                if      Call CloseHandle drop
                        0 to ParHndl
                then    ;

: pemit         { char -- }     \ write a character to the parallel port
                &LOCAL char 1 ParHndl write-file abort" Failed to write LPT1" ;

: ptype         ( a1 n1 -- )
                ParHndl write-file abort" Failed to write LPT1" ;

: ptest         ( -- )
                LPT1Init
                40 0
                do      s" Hello there, how are you today?\n" ptype
                loop    12 pemit
                LptClose ;


