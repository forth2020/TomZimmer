\ CLASS.F       Version 1.1 1994/04/01 07:52:15         by Andrew McKewan
\ -rbs Added ClassRoot for <Super ... so that classes that do not include records
\ do not have to include undefined methods.  Was crashing for methods get:,
\ put: etc. when no record was used.  Now use "<Super ClassRoot" for those
\ classes.
\ class.f 4.9C 2003/01/29 rbs changed class string to inherit class root
\ class.f 5.01A 2003/04/14 arm Changed _msgfind to check that object is really an object

cr .( Loading Primitive Object Class...)
cr .( -- BETA CLASS.F V4.9C --)

only forth also definitions

true value ?win-error-enabled   \ initially errors are enabled

   0 value  NewObject           \ Newest object being created

: @word         ( -<word>- addr )
                bl word uppercase ;

: hash>         ( -<word>- hash )       \ rls
                @word count method-hash ;

classes also definitions

\ -------------------- Selectors --------------------

: ?isSel        ( str -- str f1 )       \ f1 = true if it's a selector
                dup dup c@ +  c@ ascii : = ;            \ ends in ':'

: >selector     ( str -- SelID )        \ get a selector from the input stream
                ?isSel 0= abort" not a selector" count method-hash ;

: getSelect     ( -- SelID )            \ get a selector from the input stream
                @word >selector ;


\ -------------------- Class Structure --------------------

0 value ^Class          \ pointer to class being defined

\ references are from class pfa

: MFA    [ voc-pfa-size 0 cells+ ] literal + ;  \ method dictionary
: IFA    [ voc-pfa-size 1 cells+ ] literal + ;  \ instance variable dictionary
: DFA    [ voc-pfa-size 2 cells+ ] literal + ;  \ data area size in bytes
: XFA    [ voc-pfa-size 3 cells+ ] literal + ;  \ width of indexed items
: SFA    [ voc-pfa-size 4 cells+ ] literal + ;  \ pointer to superclass

voc-pfa-size 5 cells+ constant class-size       \ size of class pfa


: >obj          ( objCfa -- ^obj )      >body cell+ ;

: obj>          ( ^obj -- objCfa )      cell- body> ;

: obj>Class     ( ^obj -- ^class )      CELL - @ ;

: Class>Obj     ( ^Class -- ^obj )      cell+ ;

: Class>        ( ^Class -- objCfa )    body> ;

: >Class        ( objCfa -- ^Class )    >body ;

: ClassPointer? ( class -- f )  XFA @ -1 <> ; 

: class-allot   ( n -- )  ^class DFA +! ;

0 value bitcnt
0 value bitmaxval

: >mask         ( n1 -- mask )
                0 swap 0
                ?do     1 lshift 1 or
                loop    ;

: class-bitallot ( n -- )
                bitmaxval 0=
                abort" Bit fields are not allowed on this data type"
                dup 0=
                abort" Zero length bit fields are not allowed"
                bitcnt bitmaxval =
                if      0 to bitcnt
                then
                bitcnt ,                  \ save the bit offset into data item
                dup >mask bitcnt lshift , \ save bit mask of data in item
                dup bitcnt + bitmaxval >
                abort" Bit field exceeded bits allowed in this field"
                +to bitcnt ;

: bitmax        ( n1 -- )       \ set max bits for any following bit fields
                bitcnt bitmaxval <>
                if      cr ." WARNING: Bit fields weren't completely filled"
                then
                dup to bitcnt
                    to bitmaxval ;

\ -------------------- Find Methods --------------------

\ code ((findm))  ( SelID addr -- 0cfa t OR f )  

named-new$ tempmsg$

: (FINDM)   ( SelID ^class -- m0cfa )   \ find method in a class
        2dup
        MFA ((findm)) if  nip nip EXIT  then
        nip
        S"  not understood by class " tempmsg$ +place
        body> >name nfa-count tempmsg$ +place
        tempmsg$ msg !  -2 throw ;

: FIND-METHOD   ( SelID ^obj -- ^obj m0cfa )   \ find method in object
                ?dup if  tuck obj>Class (findm) EXIT  then
                ?win-error-enabled
                if      false to ?win-error-enabled
                        forth-io cr
                        cr unhash  type ."  NULL"
                        .rstack
                        ABORT
                else    MAXSTRING LocalAlloc >r
                        unhash          r@  place
                        s"   NULL"      r@ +place
                                        r@ +NULL
                        MB_ICONSTOP MB_OK or MB_TASKMODAL or
                        z" Find Method Error!" rel>abs
                        r> 1+  rel>abs NULL Call MessageBox drop
                        BYE
                then    ;

: (Defer)       ( ^obj -- )   \ look up SelID at IP and run the method
                @(ip) swap  ( SelID ^obj )
                Find-Method execute ;

: dbg-next-cell-class ( ip cfa -- ip' cfa )
                dup ['] (Defer) =
                if      swap cell+ swap
                then    ;

dbg-next-cell chain-add dbg-next-cell-class \ link into the debugger

: dbg-nest-class ( top-of-user-stack cfa flag -- cfa false | true )
                dup ?exit                       \ leave if already found
                over ['] (Defer) =
                if      2drop cr .s
\ !!! USES A COPY OF THE ADDRESS ON TOP OF THE STACK TO LOCATE THE METHOD !!!
                [ bug ] ip @ cell+ @ over Find-Method nip 3 cells+ ip !
                        2 nesting +!
                        true
                then    ;

classes

dbg-nest-chain chain-add dbg-nest-class

: .word-type-class      ( cfa flag -- cfa false | true )
        dup ?exit
        over ['] (Defer) =
        if      2drop
                ." Late: "
                true
        then    ;

.word-type-chain chain-add .word-type-class

: .execution-class-class ( ip cfa flag -- ip' cfa flag )
                dup ?EXIT                       \ leave if non-zero flag
                over ['] (Defer) =              \ is it a late bound method
                if      drop                    \ discard original flag
                        ." [ " swap cell+
                        dup @ unhash type
                        cell+ swap ."  ] "
                        true                    \ return true if we handled it
                then    ;

.execution-class-chain chain-add .execution-class-class

   0 Value  ^Self
   0 Value  ^Super              \ nfa of SUPER pseudo-Ivar

: ?isClass  ( cfa -- f )  @ dup   doCLass =
                            swap do|Class = or ;
: ?isObj    ( cfa -- f )  @ doObj = ;
: ?isValue  ( cfa -- f )  @ doValue = ;
: ?isVect   ( cfa -- f )  @ dup doValue =
                           over doDefer = or
                           swap (iv@)   = or ;

: ?isParen      ( cfa -- f )
                >name nfa-count drop c@ ascii [ = ;

\ ERROR if not compiling a new class definition
: ?Class        ( -- )
                ^class   0= abort" Not in a class" ;

\ Determine if next word is an instance var.
\ Return pointer to class field in ivar structure.
: VFIND         ( str -- str f OR ^iclass t )
                ^class
                IF      dup count method-hash ^class IFA ((findm))
                        dup if  rot drop  then
                ELSE    0
                THEN ;

: classVFIND    ( str ^class -- str f OR ^iclass t )
                >r dup count method-hash r> IFA ((findm))
                dup if  rot drop  then  ;

: IDX-HDR       ( #elems ^class OR ^class -- indlen )
                XFA @ dup 0>    \ if XFA holds -1 then its a non-classpointer class
                                \ if XFA holds 0> then its the indexed data width
                if      2dup ( width ) w, ( #elems ) w, *
                then    0max ;

(* special handling for XFA=-1 *)

\ -------------------- Initialize Instance Variables --------------------
((
Instance variable consists of five 4-byte fields.  A sixth field is
used for indexed ivars only.

    Offset   Name      Description
    ------   ----      ---------------------------------------
       0 0   link      points to link of next ivar in chain
       4 1   name      32-bit hash value of name
       8 2   class     pointer to class pfa
      12 3
      16 4   offset    offset in object to start of ivar data
      20 5   #elem     number of elemens (indexed ivars only)

In the stack diagrams, "ivar" refers to the starting address of this
structure.  The IFA field of a class points to the first ivar.
))


: iclass     ( ivar -- 'class )   2 cells+ ;

: @IvarOffs  ( ivar -- offset )   3 cells+ @ ;

: @IvarCPtr  ( ivar -- flag )     4 cells+ @ ;

: @IvarElems ( ivar -- #elems )   5 cells+ @ ;


\ send ClassInit: message to ivar on stack
: InitIvar  ( ivar offset -- )
                over @IvarOffs + newObject +            ( ivar addr )
                [ getSelect ClassInit: ] literal
                rot iclass @ (findm) execute ;

0 value contiguous-data?

\ ITRAV traverses the tree of nested ivar definitions in a class,
\ building necessary indexed area headers.
: ITRAV   { ivar offset -- }
        Begin
                ivar ^Self <>
        While
                ivar iclass @ IFA @
                ivar @IVarOffs offset + RECURSE

                ivar iclass @ ?dup      ( Why would an Ivar have no class ?? )
                if      dup ClassPointer?       \ is this a classpointer class
                        ivar @IvarCPtr 0= and   \ and not contiguous data flag
                        if      newObject offset + ivar @IvarOffs +
                                                                ( ^class ivarAddr )
                                2dup cell- !                    \ store class pointer
                                over XFA @
                                if over DFA @ +                 \ addr of indexed area
                                        swap XFA @ over W!      \ Index width
                                        ivar @IvarElems swap 2 + w! \ #elems
                                else 2drop
                                then
                        else    drop
                        then
                        ivar offset initIvar                    \ send ClassInit:
                then
                ivar @ to ivar                                  \ next ivar in chain
        Repeat ;


defer ClassInit  ( -- ) \ send ClassInit: to newObject
' NOOP is ClassInit

\ Compile an instance variable dictionary entry
: <VAR          ( #elems ^class OR ^class -- )
                dup XFA @ >r dup>r              \ save XFA contents and class ptr
                @word Vfind abort" Duplicate Instance Variable"
                contiguous-data?                \ if contiguous flag non zero
                if      -1 r@ XFA !             \ set XFA to -1
                then

                dup count 2dup method-hash add-hash

                ^Class IFA link,                \ link
                count method-hash ,             \ name hash
                dup ,                           \ class
                dup ClassPointer?               \ is this a classpointer class
                if      4 class-allot           \ then indexed, save 4 for class ptr
                then
                ^class DFA @ ,                  \ offset
                contiguous-data? ,              \ contiguous data flag
                dup XFA @ dup 0>                (* special handling for XFA=-1 *)
                if      rot dup , * 4 +
                then    0max                    \ #elems
                swap DFA @ +                    \ Account for named ivar lengths
                class-allot
                r> r> swap XFA ! ;              \ restore XFA contents

: (|Build)      ( #elems ^class OR ^class -- )  \ Build an instance of a class
                ^class
                IF      <Var                    \ build an ivar
                ELSE    doObj ,                 \ cfa
                        dup ,                   \ class
                        here to newObject
                        dup DFA @ reserve       \ allot space for ivars
                        dup>r IDX-HDR reserve   \ allot space for indexed data
                        r> IFA @ 0 ITRAV        \ init instance variables
                        ClassInit               \ send CLASSINIT: message
                THEN ;

: (Build)       ( #elems ^class OR ^class -- )  \ Build an instance of a class
                ^class
                IF      <Var                    \ build an ivar
                ELSE    header
                        doObj ,                 \ cfa
                        dup ,                   \ class
                        here to newObject
                        dup DFA @ reserve       \ allot space for ivars
                        dup>r IDX-HDR reserve   \ allot space for indexed data
                        r> IFA @ 0 ITRAV        \ init instance variables
                        ClassInit               \ send CLASSINIT: message
                THEN ;

create obj-buf MAXSTRING allot

: (Obj-Build)   ( #elems ^class OR ^class -- )  \ Build an instance of a class
                obj-buf count "header
                doObj ,         \ cfa
                dup ,           \ class
                here to newObject
                dup DFA @ reserve       \ allot space for ivars
                dup>r IDX-HDR reserve   \ allot space for indexed data
                r> IFA @ 0 ITRAV        \ init instance variables
                ClassInit               \ send CLASSINIT: message
                ;


\ -------------------- Heap Objects --------------------

\ build a new object on the heap for class. Use: Heap> className
\ gets heap, and returns ptr.

in-application

forth definitions

\           ( <number_of_elements> theClass -- )
: (heapObj) { theClass \ dLen obAddr idWid #els -- }
        0 to #els
        theClass DFA @ to dLen
        theClass XFA @ dup 1+ 0= - to idWid
        idWid
        IF      to #els                 \ save the optional number of elements
                                        \ from stack, if it's an array of objects
        THEN
        dLen cell+
        idWid
        IF      idWid #els * cell+ +    \ get total length of obj
        THEN
        ALLOCATE abort" Out of Memory"
        theClass over !                 \ create the class ptr
        cell+ to obAddr                 \ get nonReloc heap, save ptr to cfa
        idWid
        IF      obAddr dLen + idWid over w! 2 + #els swap w!
        THEN
        obAddr to newObject
        theClass IFA @ 0 Itrav classinit obAddr ;

in-system

: Heap>         ( -- addr )
                '  dup ?isClass not abort" Use: New> classname "
                >body
                STATE @
                IF      POSTPONE literal
                        POSTPONE (heapObj)
                ELSE    (heapObj)
                THEN    ; IMMEDIATE

synonym New> Heap>

\ See " Dispose " later for releasing dynamic objects

in-application

classes definitions

\ --------------- Build SUPER and SELF pseudo ivars ---------------

S" SUPER" hash> SUPER add-hash

Here to ^Super
        0 ,             \ link
        hash> SUPER ,   \ name
        0 ,             \ class
        0 ,             \ offset (was -1)
        0 ,             \ contiguous flag

S" SELF" hash> SELF add-hash

Here to ^Self
        ^Super ,        \ link
        hash> SELF ,    \ name
        0 ,             \ class
        0 ,             \ offset (was -1)
        0 ,             \ contiguous flag

^Self   ' classes >Class IFA !      \ latest ivar


\ -------------------- Create a new Class --------------------

0 value oldcurrent

in-system

\ Build a class header with its superclass pointer
: inherit       ( pfa -- )
                dup here class-size move        \ copy class data
                here body> vcfa>voc voc>vlink
                voc-link @ over !
                voc-link !
                class-size allot                \ reserve rest of class data

                dup ^Class SFA !                \ store pointer to superclass
                ^Super iclass !                 \ store superclass in SUPER
                ^Class ^Self iclass !           \ store my class in SELF
                                                \ add to search order
                ^Class XFA OFF
                also ^class body> vcfa>voc context ! definitions ;

in-application

forth definitions

here 0 , value Obj-CLASS
       0 value Obj-LOADLINE

\ :OBJECT creates an object with a nameless class.
\ useful for creating unique objects that are not similar to other objects,
\ and where there will only ever be one object of this nameless class.
\ Additional objects of classes can be created with an object as their
\ superclass.
: :Object       ( -<object-name>- )
                bl word count "CLIP" obj-buf place
                current @ to oldcurrent         \ save context for later restoral
                false to ?:M
                doClass ,                       \ dummy filler to fool the system
                                                \ into thinking this is a definition
                here to Obj-CLASS
                here to ^Class
                0 op!                           \ for error checking in runIvarRef
                ?loading @
                if      loadline @
                else    -1
                then    to Obj-LOADLINE ;

\ :CLASS defines a class for creating a group of similar objects
: :Class        ( -- )
                current @ to oldcurrent         \ save context for later restoral
                false to ?:M
                create
                        0    to Obj-CLASS
                        here to ^Class
                        0 op!                   \ for error checking in runIvarRef
                does>
                        [ here 8 - to doClass ] \ a dirty trick!
                        (Build) ;

in-system

\ Specify the superclass of the class or object being created. Used as follows;
\
\       :Class  <newclassname> <Super <superclassname>
\       ;Class
\
\        - or -
\
\       :Object <newclassname> <Super <superclassname>
\       :Object

: <Super        ( -- )        \ allow inheriting from a class or an object
                ' dup  ?isClass
                if      >Class inherit
                else    dup ?isObj 0= abort" not a class or object"
                        >Class @ inherit
                then    ;

synonym <Object <Super
synonym <Class  <Super

\ Create an identical copy (clone) of an existing object
\
\ Use the following syntax;
\
\       ' ExistingObject Clone NameOfNewObject

: Clone         ( CfaOfObject  'newobject' -- ) \ clone an static object
                dup @ doobj =                   \ must be cfa of object
                0= abort" Can only clone Objects"
                >body @ (build) ;               \ build the object

\ Obsolete way to create a clone of an existing object;
\
\       Clone: anObject NewObject
\
\ : Clone:        ( 'object' 'newobject' -- )
\                 ' Clone ;

in-application

\ |CLASS defines a class that creates headerless objects
\ |Class objects must be linked in the ClassInit: method
\ Used primarily for defining menus, where the names of objects are not needed,
\ and where the objects are only accessed through a linked list, built using
\ the ClassInit: method.
: |Class        ( -- )
                current @ to oldcurrent         \ save context for later restoral
                false to ?:M
                create
                        0    to Obj-CLASS
                        here to ^Class
                        0 op!                   \ for error checking in runIvarRef
                does>
                        [ here 8 - to do|Class ] \ a dirty trick!
                        (|Build) ;

classes definitions

in-system

: ;Class        ( -- )
                Obj-CLASS abort" Classes must start with :Class or |Class"
                0 ^Super iclass !
                0 ^Self  iclass !
                0 to ^Class
                forth definitions previous
                oldcurrent ?dup
                if      current !
                        0 to oldcurrent
                then    ;

: ;Object       ( -- )
                Obj-CLASS 0= abort" Objects must start with :Object"
                0 ^Super iclass !
                0 ^Self  iclass !
                0 to ^Class
                forth definitions previous
                oldcurrent ?dup
                if      current !
                        0 to oldcurrent
                then    Obj-CLASS (Obj-Build)
                Obj-LOADLINE last @ name> >view ! ;

in-application

\ -------------------- Method Compiler --------------------


: method        ( SelID -- )   \ Build a methods dictionary entry for selector
                ?Class ?Exec
                dup pocket count rot add-hash
                ^Class MFA link,        \ link
                ,                       \ name is selector's hashed value
                m0cfa ,                 \ build methods cfas
                m1cfa ,
                0 ,                     \ #locals & #args
                !csp ] ;                \ start compiler

\ For Windows messages, we would like the selector to be a constant
\ defined as the Window message number.  :M will support both types of
\ selectors.

2024 constant unres-len

create unres-methods unres-len allot
       unres-methods unres-len erase

\ :M creates a method for the current Class or Object being defined.
\ Method names always end in a ':' (colon), or in the case of Windows messages,
\ are Windows constants like WM_PAINT.
: :M            ( -- )
                unres-methods unres-len erase   \ pre-clear unresolved methods array
                @word IsWinConstant 0=          \ a windows constant
                if      dup (find)                      \ if its defined
                        if      dup @ doCon =           \ and a constant
                                if      nip execute     \ then user its value
                                else    drop >selector  \ else get selector
                                then
                        else    drop >selector          \ or a selector
                        then
                then    method
                true to ?:M     ; immediate     \ mark as making a new method

: ;M            ( -- )
                ?:M 0= abort" Methods must START with :M !"
                false to ?:M
                ?csp
                postpone unnestm
                postpone [
                0 to Parms
                semicolon-chain do-chain
                voc-also                        \ don't add to hash table
                ; IMMEDIATE

: resolve-methods ( -- )
                unres-methods
                begin   count dup
                while   2dup
                        2dup method-hash add-hash
                        +
                repeat  2drop
                unres-methods unres-len erase ;

\ -------------------- Object Compiler --------------------

\ Key to instantiation actions
\ 0 = notFnd            -not previously defined (not used)
\ 1 = objTyp            -defined as an object
\ 2 = classTyp          -as a class
\ 3 = vecTyp            -as an object vector (value or defer)
\ 4 = parmTyp           -as a named parm
\ 5 = parenType         -open paren for defer group

: refToken      ( str -- cfa tokenID )  \ Determine type of token referenced
                                        \ by str.
                pFind if  4 exit  then
                (find) 0= ?missing
                dup ?IsObj   if  1 exit  then
                dup ?IsClass if  2 exit  then
                dup ?IsParen if  5 exit  then   \ needs to preceed next line,
                dup ?IsVect  if  3 exit  then   \ because [ is a deferred word
                1 abort" Invalid object type" ;

-1 value method_hval

\ -------------------- Late Binding --------------------

\ Force late binding of method to object, as in SmallTalk
\ compiles a defer: selID.  This will build a deferred reference to the
\ parenthesized group.

: LateBound     ( SelId -- ) \ Compile or execute a deferred message send
                >R
                [CHAR] ] PARSE  EVALUATE
                source >in @ /string drop c@ ']' =      \ skip extra ']'
                IF      1 >in +!
                THEN
                State @
                IF      POSTPONE (Defer)  R> ,
                ELSE    R> swap Find-Method execute
                THEN    ;

create met_hstring name-max-chars 2 + allot
create obj_hstring name-max-chars 2 + allot

0 value &(IVB@)

: VarFind       { Class -- CfaObj } (* argument is a class *)
                obj_hstring count       \ -- a1,n1
                Class body> vcfa>voc search-wordlist
                0= abort" Can't find Variable"
                dup  @  (IV@)  =
                over @ &(IVB@) = or
                over @  (IVC@) = or
                over @  (IVW@) = or
                over @  (IVD@) = or
                over @  (&IV)  = or
                0= abort" Can ONLY use DOT notation on BYTE, SHORT, INT, BYTES or RECORD:
                ;

: NestedObject { Obj \ rem$ -- class 'Obj } (* returns obj AND class, rewritten *)
                MAXSTRING LocalAlloc: rem$
                Obj obj>class ( class )
                begin obj_hstring count 2dup '.' scan ?dup
                while 2dup 1 /string                    \ remove decimal point
                        name-max-chars min              \ clip to legal max
                        rem$ place                      \ lay remainder into temp buf
                        nip - name-max-chars min        \ calc & clip to legal max
                        obj_hstring c!                  \ lay down object name
                                                        ( class obj_hstring+1 )
                        1- swap classVFIND 0= abort" Can't find object"
                        2@ swap +to obj
                        rem$ count obj_hstring place    \ recover remainder of string
                repeat                    ( class obj_hstring+1 count obj_hstring+1 )
                2drop 1- over classVFIND            ( class str f | class ^iclass t )
                if      nip 2@ swap +to Obj             \ accumulate object
                        obj_hstring off                 \ no remaining string
                else drop
                then Obj ;                              \ then return the object

: NestedIVar { Class offset \ rem$ -- 'Class 'offset }
(* argument is class and class is returned, word is rewritten *)
                MAXSTRING LocalAlloc: rem$
                begin   obj_hstring count 2dup '.' scan ?dup
                while   2dup 1 /string                  \ remove decimal point?
                        name-max-chars min              \ clip to legal max
                        rem$ place                      \ lay remainder into temp buf
                        nip - name-max-chars min        \ calc & clip to legal max
                        obj_hstring c!                  \ lay down object name
                        1- Class classVFIND 0= abort" Can't find object"
                        2@ to Class +to offset
                        rem$ count obj_hstring place    \ recover remainder of string
                repeat
                2drop 1- Class classVFIND
                if      2@ to Class +to offset
                        obj_hstring off                 \ no remaining string
                else    drop
                then    class offset ;                  \ then return the object

: Obj.Var,      { selID ObjCfa \ Obj Class -- }
                ObjCfa >obj NestedObject to Obj to Class obj_hstring c@
                if      Class VarFind POSTPONE LITERAL
                then
                Obj POSTPONE LITERAL
                selID Class (findm) compile, ;

0 value varCfa

: RunObj.Var    { selID ObjCfa \ Obj Class -- ^obj m0cfa }
                ObjCfa >obj NestedObject to Obj to Class obj_hstring c@
                if      Class VarFind to varCfa
                else    0 to varCfa
                then Obj selID Class (findm) ;

: ivarRef       { selID ^iclass \ Class offset -- } \ compile ivar reference
                ^iclass 2@ swap NestedIVar to offset to Class obj_hstring c@
                if      Class VarFind POSTPONE LITERAL
                then
                selID Class (findm) cell+ ( m1cfa ) COMPILE, offset , ;

: runIvarRef { selID ^iclass \ Obj Class -- } \ run ivar reference (DEBUG ONLY!!)
                ^base 0= abort" No object exposed"
                ^iclass dup @ to Class Class>Obj to Obj
                obj_hstring count '.' scan nip
                if      Obj NestedObject to Obj to Class
                then
                obj_hstring c@
                if      Class VarFind
                then selID Obj Find-Method
                swap @ ( offset ) ^base + swap execute ;

: getIvarRef    { selID ^iclass -- ^obj m0cfa }
                ^base 0= abort" No object exposed"
                selID ^iclass Class>Obj Find-Method
                swap @ ( offset ) ^base + swap ;

: objRef        ( selID $str -- )  \ Build a reference to an object or vector
                Case refToken
                  0 ( ?      ) of  abort                        endof
                  1 ( object ) of  Obj.Var,                     endof
                  2 ( class  ) of  >Class (findm) ,             endof
                  3 ( vector ) of  ,  POSTPONE (defer) ,        endof
                  4 ( parm   ) of  ,  POSTPONE (defer) ,        endof
                  5 ( paren  ) of  drop LateBound               endof
                Endcase ;

: getRef        ( selPfa $str -- ^obj m0cfa )
                0 to varCfa
                Case refToken
                  0 ( ?      ) of  abort                        endof
                  1 ( object ) of  RunObj.Var                   endof
                  2 ( class  ) of  >Class (findm)               endof
                  3 ( vector ) of  execute Find-Method          endof
                  4 ( parm   ) of  abort                        endof
                  5 ( paren  ) of  2drop ['] noop               endof
                Endcase ;

: runRef        ( selPfa $str -- )  \ Execute using token in stream
                getRef
                varCfa ?dup
                if      -rot
                then    execute ( executes m0cfa ) ;

\ ================= Selector support ==========================

0 value get-reference?

: _do_message   ( val string -- )               \ normal stack format
                ( val string -- ^obj m0cfa )    \ if 'get-reference?' is ON
                \ this second stack picture provided for debugger/decompiler
        STATE @
        IF      false to get-reference?         \ ignore get reference flag
                VFIND           \ instance variable?
                IF    ivarRef   \ ivar reference
                ELSE   objRef   \ compile object/vector reference
                THEN
        ELSE    get-reference?
                IF      false to get-reference? \ just this once
                        VFIND
                        IF      getIvarRef
                        ELSE    getRef 
                        THEN
                ELSE    VFIND
                        IF      runIvarRef      ( Debug only )
                        ELSE    runRef  \ run state - execute object/vector ref
                        THEN
                THEN
        THEN    ;

: WM:           ( WM_MESSAGE -<object>- )       \  WM_CLOSE WM: Super
                here 2 cells - @ ['] LIT <>
                abort" Must be preceeded by a WM_MESSAGE"
                here cell - @                   \ get the literal
                -2 cells allot                  \ release the space
                obj_hstring off
                @word _do_message ; immediate

create x.buf name-max-chars 1+ allot
       x.buf off

: x.do_message  ( -- )
                x.buf count 2dup '.' scan       \ find decimal point?
                2dup 1 /string                  \ strip it off
                name-max-chars min              \ clip to legal max
                obj_hstring place               \ lay remainder into var buf
                nip - name-max-chars min        \ calc & clip to legal max
                met_hstring place               \ lay down object name
                method_hval met_hstring _do_message ; IMMEDIATE

\ message is the message compiler invoked by using a selector
: do_message    ( -- )
                @word count 2dup '.' scan       \ find decimal point?
                2dup 1 /string                  \ strip it off
                name-max-chars min              \ clip to legal max
                obj_hstring place               \ lay remainder into var buf
                nip - name-max-chars min        \ calc & clip to legal max
                met_hstring place               \ lay down object name
                method_hval met_hstring _do_message ; IMMEDIATE


: _msgFind      { addr \ temp$ -- addr false | cfa true }
                name-max-chars 1+ LocalAlloc: temp$
                addr ?isSel
                if      count name-max-chars min
                        2dup tempmsg$ place     \ in case there is an error
                        2dup method-hash dup ?unhash
                        if      nip nip
                        else    >r unres-methods
                                begin   dup c@
                                while   count +
                                repeat  2dup + 1+            \ end of string
                                unres-methods unres-len + >  \ beyond end?
                                abort" Unresolved Methods buffer overflow!"
                                place
                                r>
                        then    to method_hval ['] do_message
                        1 EXIT
                else    dup count
                        super-number? 0=        \ must NOT be a number!
                    if  2drop
                        dup  count s" '.'" compare 0<>     \ ignore '.'
                        over count s" ':'" compare 0<> and \ ignore ':'
                        over                               \ and
                        dup  count '.' scan nip    0<>     \ contains either a DOT
                        swap count ':' scan nip    0<> or  \ or contains a COLON
                                                       and
                        over 1+  c@ '.'             <> and \ no start '.'
                        if      dup>r count 2dup '.' scan nip - x.buf place
                                r@    count 2dup ':' scan nip - dup x.buf c@ <
                                if      x.buf place
                                else    2drop
                                then
                                x.buf   (find) 
 ( check it is obj )     ( arm) dup if drop dup ?isObj then ( arm)
                                nip ?dup 0=
                                if      x.buf vfind nip
                                then
                                if      r@ count ':' scan nip
                                        if      r@ count ':' scan 1 /string
                                                temp$ place
                                                ':' temp$ c+place
                                                temp$ count method-hash to method_hval
                                                r> count 2dup ':' scan nip -
                                                x.buf place
                                        else    s" GET:" method-hash to method_hval
                                                r> count x.buf place
                                        then    ['] x.do_message
                                        1
                                else    r> 0
                                then    EXIT
                        then
                    else discard-number
                    then
                then
                0 ;

\ msgFind is the new action for find.  We look in the following order:
\ 1. Local variables
\ 2. Forth Dictionary (full search order)
\ 3. If word ends in ":" treat it as a selector

: msgFind       ( addr -- addr false | cfa true )
                dup count find-buffer place
                find-buffer ?uppercase
                PFIND ?dup 0=
                if      (FIND) ?dup 0=
                        if      _MSGFIND
                        then
                then    dup>r
                if      nip
                else    drop
                then    r> ;

' msgfind is find

: _classInit  ( -- )    CLASSINIT: newObject  ;
' _classInit is ClassInit

\ double left bracket has no meaning unless preceded by a selector.

: [[  true abort"  [[ must be preceeded by a selector "   ; IMMEDIATE

: <noClassPointer ( -- ) -1 ^class XFA ! ;    \ XFA is -1 when no class pointer

\ Set a class and its subclasses to indexed

: <Indexed  ( width -- )  ?Class  ^Class XFA ! ( <ClassPointer ) ;

\ Compile a self reference, but only if the class is guaranteed to
\ have a class pointer.  We can send ourself late-bound messages
\ with the syntax:   Msg: [ self ]

: Self  ( -- addr )
        POSTPONE ^base ; IMMEDIATE

\ ----------- Instance Variable Contiguous Control -----------

0 value BeginningOfRecordAddress

: Record:       ( -- )  \ define a word that returns the starting address of
                        \ a group of data fields that need be contiguous
                -1 to contiguous-data?
                header
                (&iv) ,         \ return address of array of bytes
                ^Class DFA @ dup , to BeginningOfRecordAddress
                (iv!) ,         \ store integer into first cell of array ??
                (iv+!) , ;      \ add integer to first cell of array     ??

: ;Record       ( -- )  \ end a group of data fields that need to contiguous
                0 to contiguous-data? ;

: ;RecordSize:  ( -<name>- ) \ create a name with the size of the record
                0 to contiguous-data?
                ^Class DFA @ BeginningOfRecordAddress - CONSTANT ;

\ -------------------- Instance Variables --------------------

: bytes         ( n -<name>- )         \ unstructure array of bytes
\                create  ^class DFA @ ,  class-allot
\                does> @  ^base + ;
                header
                (&iv) ,                 \ return address of array of bytes
                ^Class DFA @ ,
                -1 ,                    \ Can't store integer into array
                -1 ,                    \ Can't add integer to array
                0 bitmax                \ verify & set bit field finished & new max
                class-allot ;

: byte          ( -<name>- )           \ byte (8bit) instance variable
                header
                (ivc@) ,
                ^Class DFA @ ,
                (ivc!) ,
                (ivc+!) ,
                8 bitmax                \ verify & set bit field finished & new max
                1 class-allot ;

cfa-func (ivb@) ( pfa -- bitfield_contents )
                dup>r       @ ^base + @         \ get 32bit contents
                r@ 4 cells+ @ and               \ mask out all other bits
                r> 3 cells+ @ rshift ;          \ shift down to low bits

(ivb@) to &(ivb@)       \ so we can verify data type

cfa-func (ivb!) ( bits_to_store pfa+2*cells -- )
                >r 
                r@ 1 cells+  @ lshift           \ scale up to proper bits
                r@ 2 cells+  @ and              \ mask out other bits
                r@ 2 cells - @ ^base + @        \ get existing field contents
                r@ 2 cells+  @ invert and       \ mask out OUR bits
                or                              \ mask in new value of bits
                r> 2 cells - @ ^base + ! ;      \ store field back

cfa-func (ivb+!) ( bits_to_add pfa+2*cells -- )
                0 0
                LOCALS| mask data fld bits |    \ use locals to simplify
                fld 1 cells+  @ to mask         \ get valid bit mask for field
                bits fld @ lshift               \ scale up to proper bits
                mask and to bits                \ mask out other bits and save
                fld 3 cells - @ ^base + @ to data \ get existing field contents
                data mask invert and            \ mask OUT OUR bits
                data mask and                   \ leave ONLY OUR bits
                bits +                          \ add in new value of bits
                mask and                        \ clear out any bit overflow
                or                              \ or into remaining bits
                fld 3 cells - @ ^base + ! ;     \ store field back

: bits          { nbits -<name>- }     \ define an 'nbits' bit field in prev data item
                header
                (ivb@) ,
                ^Class DFA @ bitmaxval 8 / - ,  \ bits are in previous data item
                (ivb!) ,
                (ivb+!) ,
                nbits class-bitallot ;

: short         ( -<name>- )           \ word integer (16bit) instance variable
                header
                (ivw@) ,
                ^Class DFA @ ,
                (ivw!) ,
                (ivw+!) ,
                16 bitmax               \ verify & set bit field finished & new max
                2 class-allot ;

: int           ( -<name>- )           \ long integer (32bit) instance variable
                header
                (iv@) ,
                ^Class DFA @ ,
                (iv!) ,
                (iv+!) ,
                32 bitmax               \ verify & set bit field finished & new max
                cell class-allot ;

: dint          ( -<name>- )           \ double (64bit) instance variable
                header
                (ivd@) ,
                ^Class DFA @ ,
                (ivd!) ,
                (ivd+!) ,
                0 bitmax                \ verify & set bit field finished & new max
                2 cells class-allot ;

\ : int-array    ( size -<name>- )
\                header
\                (iv[]@) ,
\                ^Class DFA @ ,
\                (iv[]!) ,
\                (iv[]+!) ,
\                cells class-allot ;

forth definitions

: Dispose       ( addr -- )     \ dispose of an dynamically allocated object
                ~: [ dup ] cell- Free Abort" Disposing Object failed!" ;


\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\                   The Base Class "Object"    
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

\ :Class object   ' classes >Class classes inherit

\ Revised by -rbs July 9th, 2002


:Class ClassRoot   ' classes >Class classes inherit
\ -rbs Adding a true Base Class that has no default methods for record types.

        \ Use this class if you have no ivars in your class.
        \ It will trap undefined methods that might slip through otherwise.
        \ Note: Class String SHOULD use this as its Super.  Not changed
        \ at this time.  There are only (expected default) methods defined
        \ here.

        :M ClassInit:   ;M
        :M ~:           ;M              \ the default destructor method
;Class


\ Use "<Super OBJECT" for classes that use RECORD:
:Class object   <Super ClassRoot

\ :M ClassInit:   ;M
\ :M ~:           ;M              \ the default destructor method

\ the rest of the code remains the same as it was... -rbs

:M Addr:        ( -- addr )     ^base  ;M

:M Print:       ( -- )          ." Object@" ^base .  ;M

:M Get:         ( -- n1 )                execute ;M

:M Put:         ( n1 -- )       2 cells+ execute ;M

:M Add:         ( n1 -- )       3 cells+ execute ;M

:M And:         ( n1 -- )       Get: self AND put: self ;M

:M Or:          ( n1 -- )       Get: self OR  put: self ;M

:M Xor:         ( n1 -- )       Get: self XOR  put: self ;M



unres-methods unres-len erase

semicolon-chain chain-add resolve-methods  \ link into definition completion

;Class

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\               Define data type class for strings
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

:Class String   <Super ClassRoot

MAXSTRING bytes theBuffer

:M Get:         ( -- a1 n1 ) theBuffer  count ;M
:M Put:         ( a1 n1 -- ) theBuffer  place thebuffer +null ;M
:M Add:         ( a1 n1 -- ) theBuffer +place thebuffer +null ;M
:M Append:      ( a1 n1 -- ) theBuffer +place thebuffer +null ;M

;Class

:Class Rectangle  <Super Object

Record: AddrOf
        int Left
        int Top
        int Right
        int Bottom
;Record

:M EraseRect:   ( -- )
                0 to Left
                0 to Top
                0 to Right
                0 to Bottom
                ;M

:M ClassInit:   ( -- )
                ClassInit: super
                EraseRect: self
                ;M

:M SetRect:     ( left top right bottom -- )
                to Bottom
                to Right
                to Top
                to Left
                ;M

:M AddrOf:      ( -- n1 )       AddrOf ;M
:M Left:        ( -- n1 )       Left   ;M
:M Top:         ( -- n1 )       Top    ;M
:M Right:       ( -- n1 )       Right  ;M
:M Bottom:      ( -- n1 )       Bottom ;M

:M .Rect:       ( -- )
                cr ." Rect: " Left . Top . Right . Bottom .
                ;M

;Class

Rectangle TempRect      \ a sample rectangle object, not used by the system

: .CLASSES      ( -- )          \ display all classes in the system
                cr              \ classes are really vocabularies
                voc-link @
                BEGIN   dup vlink>voc
                        voc>vcfa dup ?isClass
                        IF      dup >name name>
                                ['] [UNKNOWN] =         \ if not found
                                IF      drop            \ then discard the class
                                ELSE    .name
                                        20 #tab 20 ?cr
                                THEN
                        ELSE    drop
                        THEN
                        @ dup 0=
                UNTIL   drop  ;

only forth also definitions

