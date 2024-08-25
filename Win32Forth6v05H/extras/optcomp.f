\ OPTCOMP.F Compiler optimisation flags 28/11/2002 21:40:58 arm

\ --------------------  Compiler optimisation flags ---------------------
\ Used by META.F and optimisers to mark value COMPILE-OPTIONS, and to allow
\ other forth code to determine what optimisations have occurred or have
\ been requested.
\ -----------------------------------------------------------------------

\ 0 VALUE COMPILE-OPTIONS                       \ defined in FKERNEL.F

INTERNAL

IN-SYSTEM

\ BIT                NAME
  1         CONSTANT OPT-FIXEDLOAD
  2         CONSTANT OPT-NEXTEXEC
  4         CONSTANT OPT-VAR
  8         CONSTANT OPT-CON
  
: OPT-SET?  ( opt -- f )                  \ is option set?
            COMPILE-OPTIONS AND 0<> ;         \ set flag

: OPT-ON    ( opt -- )                    \ set option
            COMPILE-OPTIONS OR                \ or in flag
            TO COMPILE-OPTIONS ;              \ set the option

: OPT-OFF   ( opt -- )                    \ unset option
            INVERT COMPILE-OPTIONS AND        \ rest bit to zero
            TO COMPILE-OPTIONS ;

: .OPT-ONOFF ( f -- )                      \ print on/off
            IF >bold ." ON" >norm ELSE ." off" THEN ;

EXTERNAL

: .OPTS     ( opt -- )                    \ print options
            cr ." Compiler Options: "
            cr ." ----------------- "
            .version
            cr ." KERNEL Version: "   30 col kver sp@ 4 type drop
            cr ." Fixed Loadpoint "   30 col OPT-FIXEDLOAD OPT-SET? .OPT-ONOFF
            cr ."   Loadpoint is at"  30 col 
  [defined] META \ is meta vocab defined?
            [IF] ORIGIN 
            [ELSE] ^IMAGE @ 
            [THEN]
              ." 0x" h.
            cr ." NEXT/EXEC"          30 col OPT-NEXTEXEC OPT-SET? .OPT-ONOFF
            cr ." VARIABLEs"          30 col OPT-VAR OPT-SET? .OPT-ONOFF
            cr ." CONSTANTs"          30 col OPT-CON OPT-SET? .OPT-ONOFF
            cr   ;
            
IN-APPLICATION

MODULE

