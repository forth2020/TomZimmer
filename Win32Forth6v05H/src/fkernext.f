\ fkernext.f beta 3.1A 2002/09/25 arm Performance enhancements
\ fkernext.f beta 3.3D 08/10/2002 arm Consolidation
\ fkernext.f beta 4.0A 18/11/2002 arm Tidy up of macros

cr .( Loading FKERNEXT.F V4.0A Beta kernel extensions...)

(( 
   
   Code moved out of ASMWIN32.F and FKERNEL.F so that definitions of NEXT and
   EXEC are next to each other for ease of edit. 
   
   There are two definitions, one for use in ASMWIN32.F, that builds NEXT and EXEC
   as macro: for use in assembled code; and one for META.F that builds NEXT and EXEC
   as macro definition for use in CODE and NCODE sections of FKERNEL.F.
   
   Relocatable definitions
   -----------------------

     Relocatable EXEC is

                        mov ecx , [eax] [edi]
                        add ecx , edi
                        jmp ecx

     Relocatable NEXT is
                        mov     eax, 0 [esi]
                        mov     ecx, 0 [eax] [edi]
                        add     esi, # 4
                        add     ecx, edi
                        jmp     ecx

     Relocation uses register EDI that contains the load address of the code. 
     ORIGIN (in META) and &ORIGIN @ (in other code) will both contain 0 (zero)
     and NEXT and EXEC are built using EDI. (Note that the NEXT has a slightly
     optimised EXEC at its tail to prevent register stall).
     
   Fixed Loadpoint
   ---------------

     Fixed EXEC is
                        jmp     [eax]
                        
     Fixed NEXT is
                        mov     eax, 0 [esi]
                        add     esi, # 4
                        jmp     [eax]
                        
     Fixed does NOT use EDI, as ORIGIN (or @ORIGIN @) of non-zero means that this
     has been built to load at that, AND ONLY THAT, address. It's much faster (25-30%)
     but cannot be moved in memory, and must be loaded at a specific address. EDI will
     contain 0, but NOTE -- other code will use EDI, and it is ESSENTIAL that it is 
     not changed elsewhere in the code.
     
   EXE vs DLL
   ----------

   EXE files are loaded at specific addresses, and do not require relocation sections. Under
   all opsys except NT, this address is 0x00400000 (NT 0x00010000). Currently, because
   the C wrapper occupies that address, to build a FIXED loadpoint image requires that it
   is built elsewhere (for instance 0x00800000).
   
   DLLs require relocatability. Win32Forth doesn't build relocation sections for addresses
   so the relocatable code MUST be used, as the loadpoint can be anywhere.
   
   Advantages of this technique:
   -----------------------------
   
   1. Old code works fine with an origin of 0 (i.e. relocatable at run-time, use EDI). 
      EXE and DLL both supported, no change at all.
   2. New code with FIXED loadpoint (origin <>0) can build EXE files that run much
      faster, especially compute intensive, without requring optimisation. Can't build
      DLLs that do this however. Code is shorter -- so kernel is smaller.

   No attempt has been made to optimise other code. For instance, @ is
      
      CODE @          ( a1 -- n1 )    \ get the cell n1 from address a1
                      mov     ebx, 0 [ebx] [edi]
                      next    c;

   There's not much to be gained by changing to <mov ebx, 0 [ebx]>.

))

macro: exec-fixed        \ macro for fixed loadpoint exec
                jmp     [eax]
endm

macro: exec-reloc        \ macro for relocatable exec
                mov     ecx , [eax] [edi]
                add     ecx , edi
                jmp     ecx
endm

macro: next-fixed        \ macro for fixed next
                mov     eax, 0 [esi]
                add     esi, # 4
                jmp     [eax]
endm

macro: next-reloc        \ macro for relocatable next
                mov     eax, 0 [esi]
                mov     ecx, 0 [eax] [edi]
                add     esi, # 4
                add     ecx, edi
                jmp     ecx
endm


[undefined] META \ is meta vocab undefined?


[IF] \ in ASMWIN32.F
    &ORIGIN @ [IF] \ fetch loadpoint origin
        cr .( --- Compiling FIXED LOADPOINT at 0x) &origin @ H.8
        macro: exec ( assemble code to execute the cfa in eax )
                        ( -- )
                        /set-prefix >r
                        exec-fixed
                        r> reset-syntax
        endm

        macro: next ( assemble the code to do a next )
                        ( -- )
                        /set-prefix >r
                        a; resolve-ofa          \ resolve the optimizer field address
                        next-fixed
                        r> reset-syntax
        endm

    [ELSE]
        cr .( --- Compiling RELOCATABLE origin)
        macro: exec ( assemble code to execute the cfa in eax )
                        ( -- )
                        /set-prefix >r
                        exec-reloc
                        r> reset-syntax
        endm

        macro: next ( assemble the code to do a next )
                        ( -- )
                        /set-prefix >r
                        a; resolve-ofa          \ resolve the optimizer field address
                        next-reloc
                        r> reset-syntax
        endm
    [THEN] \ end of ASMWIN32.F


[ELSE] \ in META.F
    ORIGIN [IF]
        CR .( Compiling FIXED LOADPOINT at 0x) origin H.8
        MACRO NEXT      ( -- )                  \ Inner interpreter
                        RESOLVE-OFA             \ save size of code definition
                        next-fixed
                        END-MACRO

        MACRO EXEC      ( -- )                  \ execute RELATIVE cfa in eax
                        exec-fixed
                        END-MACRO
    [ELSE]
        cr .( Compiling RELOCATABLE loadpoint)
        MACRO NEXT      ( -- )                  \ Inner interpreter
                        RESOLVE-OFA             \ save size of code definition
                        next-reloc
                        END-MACRO
        
        MACRO EXEC      ( -- )                  \ execute RELATIVE cfa in eax
                        exec-reloc
                        END-MACRO
    [THEN] 
[THEN] \ end of META.F


