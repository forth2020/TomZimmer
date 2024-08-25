\ ----------------------------------------------------------------------
\ Decompiler words

0 value seedone? \ end-decompile flag
0 value seetab   \ tabulation counter
6 value tabsize

: s.cr          ( a -- a ) cr dup h. space ;
: s.newtab      ( -- )   tabsize to seetab s.cr ;
: >seetab       ( n -- ) seetab + 64 min 0max to seetab ;
: s.tab         ( -- )   s.cr seetab spaces ;
: s.tab+        ( -- )   s.tab  tabsize >seetab ;
: s.tab-        ( -- )   tabsize negate >seetab s.tab ;
: s.tab-+       ( -- )   s.tab- tabsize >seetab ;

: s.next        ( a -- a' c )   dup ?codebounds 'image count >r '-image r> ;
: s.next16      ( a -- a' n )   s.next   >r s.next   r> byte-join ;
: s.next32      ( a -- a' n )   s.next16 >r s.next16 r> word-join ;

: .unknownxt    ( xt -- )
\ display unknown xt
                ." xt:" h. ;

: .nextxt       ( a -- a' )
\ get and display the next token
                lift-token
                dup xt>npfa ?dup                ( xt nfa pfa . )
                if      PFA-local !  nip
                        nfa-count type space
                else    drop .unknownxt         \ unknown name
                then    ;

: .data"        ( a -- a' )
\ get and display a counted string
                '"' emit space
                s.next 0
                ?do     s.next emit
                loop    '"' emit space ;

: (;)           ." ; "                 s.newtab ;
: (:)           s.newtab  ." : "         .nextxt ;

vocabulary seewords
also seewords definitions \ ======================================

: (LIT8)        s.next   dup   127 > 0xFFFFFF80 and or . ;
: (LIT16)       s.next16 dup 32767 > 0xFFFF8000 and or . ;
: (LIT32)       s.next32 . ;
: CONSTANT      ." CONSTANT "   .nextxt s.newtab ;
: 2CONSTANT     ." 2CONSTANT "  .nextxt s.newtab ;
: VARIABLE      ." VARIABLE "   .nextxt s.newtab ;
: 2VARIABLE     ." 2VARIABLE "  .nextxt s.newtab ;
: MARKER        ." MARKER "     .nextxt s.newtab ;
: CREATE        ." CREATE "     .nextxt s.newtab ;
: STRING        ." STRING "     .nextxt s.newtab ;
: ARRAY         ." ARRAY "      .nextxt s.newtab ;
: IF            s.tab+  ." IF    " ;
: THEN          s.tab-  ." THEN  " ;
: ELSE          s.tab-+ ." ELSE  " ;
: ?DO           s.tab+  ." ?DO   " ;
: DO            s.tab+  ." DO    " ;
: LOOP          s.tab-  ." LOOP  " ;
: +LOOP         s.tab-  ." LOOP  " ;
: CASE          s.tab+  ." CASE  " ;
: OF            s.cr    ." OF " ;
: ENDCASE       s.tab-  ." ENDCASE " ;
: (LOCAL[)      s.tab+  ." LOCAL[ (" s.next 16 /mod
                (.) type ." |" (.) type ." ) " ;

: (]LOCAL)      s.tab-  ." ]LOCAL (" s.next (.) type ." ) " ;
: (LOCAL@)      ." LOCAL["    s.next (.) type ." ] " ;
: (LOCAL!)      ." TO_LOCAL[" s.next (.) type ." ] " ;

: S"            ." S" .data" ;
: ."            ." ." .data" ;
: C"            ." C" .data" ;

\ : (STRING)      s.tab ascii S emit ascii " emit space
\                s.next 0 ?do s.next emit loop ascii " emit ;
((
: CODE          s.newtab  ." CODE" seenext dup xt>npfa drop
                s.next to cputype
                cputype 0 CPUID type    \ cpu type (51, 68, etc)
                ?dup
                if      .id drop        \ name
                else    ." xt:" h.
                then    s.next16        \ length
                drop ;
\           s.disassemble ;
))

synonym : (:)
synonym ; (;)

previous definitions  \ ==========================================
false to sys-warning?

: see-find      ( a n -- cfa f )
                pad place
                also seewords get-order
                over 1 set-order
                pad find 2>r
                set-order  previous 2r> ;

true to sys-warning?

: $see1         ( a -- a' )
\ display the next bytecode
                lift-token
                dup xt>npfa ?dup                ( xt nfa pfa . )
                if      PFA-local !  nip        ( nfa )
                        nfa-count 2dup see-find \ is this token special?
                        if      nip nip execute \ yes
                        else    drop type space \ no, just a token
                        then
                else    drop .unknownxt         \ unknown name
                then    ;

: $see          ( CFA datatype -- )
\ disasemble tokenized word starting at a specified address
\ disassemble a word, any key quits if things get out of hand
                over -1 =
        if      2drop cr ." Image address unknown"
        else
                dt.program =
                if      cr dup h. ."  program: "
                        s.next16 0x7C9 <> if ." ??" then        \ id:   2
                        s.next16 ." checksum=" .                \ csum: 2
                        s.next >r s.next16 r> word-join         \ len:  3
                        dup      ." length=" .                  \ length of actual data
                        cr tabsize spaces 
                        over +          ( a a' )
                else    dup nextcfa     ( a a' )
                then    swap            ( end begin )
                begin   $see1           ( end a )
                        2dup <=
                        key? if key 27 = or then
                until   2drop cr
        then    ;


:noname ( isee )  ( <name> -- )
\ decompile or disassemble
                findtoken t.cfa
                t.cputype bit#tokenized? and    \ bytecode or native?
                if      t.datatype $see
                else    cr ." Disassembled code " t.flags .flags
                        $disassemble
                then
                PFA-local off ;  is isee

