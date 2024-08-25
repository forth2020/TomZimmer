\ classdbg.f

cr .( Loading Object Debugging...)

: _methods      ( class_body -- )
[ classes ]     MFA
                begin   @ ?dup
                while   dup cell+ @ unhash
                        type 14 #tab space 14 ?cr
                repeat  ;

: cmethods      ( 'class' -- )          \ methods of a class
                ' >body _methods ;

: methods       ( object-address -- )
[ classes ]     depth 0=
                if      ' execute
                then
                cr obj>class _methods ;

: ivars         ( object-address -- )
[ classes ]     depth 0=
                if      ' execute
                then
                cr obj>class IFA
                begin   @ ?dup
                while   dup cell+ @ unhash
                        type 10 #tab ."  (" dup 3 cells+ @ 1 .r ." )"
                        10 #tab space 20 ?cr
                repeat ;

INTERNAL

0 value theClass

: .obj          ( nfa -- )
                dup name> dup @ doobj =
                if      >body @ theClass =
                        if      nfa-count type cr
                        else    drop
                        then
                else    2drop
                then    ;

EXTERNAL

: objects       ( 'class' -- )  \ display direct object for 'class'
[ also classes ] ' >body to theClass
                ['] .obj on-allwords ;
previous

MODULE

classes also hidden also bug also

: GetMethod     { \ m0cfa -<method: object>- m0cfa }
                @word _msgFind 1 <> abort" Undefined Method"
                TRUE to get-reference?  \ tell do_message to return method
                depth >r
                execute to m0cfa        \ execute do_message
                depth r> <
                if      0               \ if it was a class, object is NULL
                then    to obj-save m0cfa ;

: msee          ( -<method object>- )
                GetMethod cr ." :M " dup .m0name 2 spaces
                3 cells+                        \ step to the PFA
                .pfa ;                          \ decompile the definition

: mdebug        ( -<method object>- )
                unbug
                false to ?dbg-cont              \ turn off contuous step
                base @ to debug-base
                GetMethod
                obj-save 0= abort" Must specify an Object, not a Class!"
                3 cells+ dup ip0 ! ip !
                ['] trace patch
                nesting off ;

: mdbg          ( -<method object>- )    \ debug a method now
                >in @ mdebug >in ! ;

