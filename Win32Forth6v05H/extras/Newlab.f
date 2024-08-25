\ NEWLAB.SEQ   New local label mechanism   by Tom Zimmer & Jerry Boutelle

anew labeler

((
  NEWLAB was designed to handle the very flexible needs of F-PC, TCOM and
any target processor being brought up on TCOM or F-PC.  The local label
mechanism it replaces did not know how to handle all of the various types
of forward references that might need to be resolved. This new method,
defines a resolution type for each forward reference record, and calls a
procedure provided by the current assembler for each type of reference
being resolved.  This places the burden of actually resolving the
reference on the assembler where it belongs.

  NEWLAB uses an F-PC "POINTER" array to hold the datum for each local
label, and each forward reference that occurs.  The array is split into
two parts, the 512 local label records and the 256 reference records that
are allocated as forward references are encountere.  References and
labels are released to free chain as forward references are resolved.


  The local label and forward reference records appear as follows:

                           LOCAL LABEL RECORD

        cell res-ptr   A pointer to the resolution chain,
                       0 if the label has already been resolved.
        cell res-addr  The resolved address of this local label
        ^
        |____ The above record is repeated for each local label up to
              MAX-LLAB-1 the maximum number of local labels allowed.

        The label record area of the local label array is then fixed in
        size as MAX-LLAB times 4 (a cell), or 2048 bytes for MAX-LLAB=512
        local labels.

                        FORWARD REFERENCE RECORD

        cell ref-link  Link of next forward reference of this label
        cell ref-type  Resolution type for this forward reference
        cell targ-adr  The target address for this forward reference

        .... The above record is repeated and linked for each forward
             reference.

        The reference record area is also fixed in size as MAX-LREF times
        12 (3 cells), or 1024 bytes for MAX-LREF=255 references.


FORWARD REFERENCE TYPES

  Forward reference types are currently values in the range 1 to 7 decimal.
The value zero is reserved to indicate a reference that was not properly
setup by the assembler.  This might happen if an illegal mnemonic was used
in the TCOM compiler, where no actual code was compiled for a line which
contains a local label. Here is an example:


                MAV AX, mytable         \ get first table entry
                1PUSH   END-CODE

LABEL mytable   345 , 567 , 789 ,       \ make a table

  In this example, MAV is not a legal instruction, but TCOM doesn't know
that, so it turns "MAV" into a local label like "mytable". When the local
lable for "mytable" comes along, two local labels are now defined on the
same line, both with an unknown resolution type, which will cause an error:

                 " Label or symbol incorrectly assembled"

))

only forth also definitions

vocabulary compiler compiler definitions also

\ **********************************************************************
\ a field defining word, makes words that add a value to top stack item
\ **********************************************************************

: +field        ( n1 n2 -<name>- n3 )   \ define a reference record field
\in-system-ok   create  over , +
                does> @ + ;

\ **********************************************************************
\ Define the fields for local label records and forward reference records
\ **********************************************************************

   0  nostack1          \ LOCAL LABEL fields
cell +field >llptr      \ move to the local label reference pointer "type" field
cell +field >lladr      \ move to the local label target address field
constant b/llab         \ bytes per local label

   0  nostack1          \ REFERENCE fields
cell +field >rlink      \ move to the reference link field  (cell bytes)
cell +field >rtype      \ move to the reference type field  (cell bytes)
cell +field >taddr      \ move the the target address field (cell bytes)
constant b/lref         \ bytes per label forward reference

\ **********************************************************************
\ *********** Runtime specifiable limits of local labels ***************
\ **********************************************************************
\
\  The formula:
\
\          MAX-LLAB B/LLAB * MAX-LREF B/LREF * +
\
\ **********************************************************************

   8 value MAX-LNEST    \ maximum local label nesting in macros
 512 value MAX-LLAB     \ maximum number of local labels during a compile
 256 value LOC-LLAB     \ maximum LOCAL labels, the rest are global
 255 value MAX-LREF     \ maximum forward references outstanding
                        \ *** MUST BE LIMITED TO 256 CURRENTLY ***

\ **********************************************************************
\ Half of the labels are created as local, that is only half of the labels
\ are cleared by CLEAR_LABELS or LLAB-INIT. As defined above, labels with
\ a number between LOC-LLAB and MAX-LLAB-1 are always global labels, and
\ may not be DEFINED more than ONCE during a program compile.
\ **********************************************************************

\ **********************************************************************
\ Create the label segment "pointer". 
\ **********************************************************************

max-llab b/llab * max-lref b/lref * + value b/llset \ bytes per local label set

b/llset max-lnest 1+ * Pointer LabPtr   \ let local labels recurse eight times


\ **********************************************************************
\ some local label pseudo constants, initialized by LLAB-INIT
\ **********************************************************************

0 value llab-start      \ start of label record area = 0
0 value lref-start      \ start of label reference area
0 value llab-size       \ label record area size in bytes
0 value lref-size       \ label reference area size in bytes

0 value labels-inited?  \ has the local label mechanism been initialize?

\ **********************************************************************
\ 8 deep round robin of the sets of local labels
\ **********************************************************************

forth definitions

\ 0 value llset_cnt       \ debugging tool, keeps track of push/pop sequence

\+ llset_cnt    : +llset_cnt    ( n1 -- )
\+ llset_cnt            llset_cnt + to llset_cnt ;

: llset+        ( -- )
\+ llset_cnt    1 +llset_cnt
                labels-inited? 0=
                abort" Attempt to use LLSET+ without initializing labels"
                b/llset max-lnest * >r
                LabPtr LabPtr b/llset +    r@ move
                LabPtr r> + LabPtr    b/llset move ;

: llset-        ( -- )
\+ llset_cnt     -1 +llset_cnt
                b/llset max-lnest * >r
                LabPtr           LabPtr r@ + b/llset move
                LabPtr b/llset + LabPtr           r> move ;

compiler definitions

\ **********************************************************************
\       Deferred words to fill in with target definitions
\ **********************************************************************


defer target-here       '  here is target-here
defer target-8!         ' 2drop is target-8!
defer target-16!        ' 2drop is target-16!
defer target-32!        ' 2drop is target-32!
defer target-8,         '  drop is target-8,
defer target-16,        '  drop is target-16,
defer target-32,        '  drop is target-32,
defer target-a;         '  noop is target-a;

defer instruction-8,    '  drop is instruction-8,
defer instruction-16,   '  drop is instruction-16,
defer instruction-32,   '  drop is instruction-32,


\ **********************************************************************
\ Some VALUE variables
\ **********************************************************************

0 value free-lref       \ next free reference record
0 value cur-label       \ the current local label
0 value cur-ref         \ current forward reference record number

\ **********************************************************************
\ Translate the local label number or forward reference number into the
\ address of a local label or reference record in the "LabPtr" array.
\ **********************************************************************

: llab>line     ( n1 -- addr )       \ locate the local label n1
                [ forth ]
                dup 0 max-llab within 0= abort" Local label out of range"
                LabPtr swap b/llab * + llab-start + ;

: lref>line     ( n1 -- addr )       \ locate reference given ref#=n1
                [ forth ]
                LabPtr swap b/lref * + lref-start + ;

\ **********************************************************************
\ Words to allocate ane release forward reference records
\ **********************************************************************

: release-ref   ( n1 -- )               \ add lref record to free chain
                [ forth ]
                free-lref over lref>line >rlink ! to free-lref ;

: allocate-ref  ( -- n1 )               \ get one lref from the free chain
                [ forth ]
                free-lref dup  lref>line >rlink @ to free-lref ;

\ **********************************************************************
\ Initialize the local label and forward reference arrays
\ **********************************************************************

: llab-init     ( -- )                  \ initialize the local label array
                [ forth ]
                labels-inited? 0=
        if      max-llab b/llab * dup to llab-size      \ label area size
                                  dup to lref-start     \ start of ref area
                max-lref b/lref * dup to lref-size +    \ ref area size
                                      to b/llset        \ size of label set
                true to labels-inited?
                max-lnest 0
            do  LabPtr llab-start + llab-size -1 fill   \ -1 labels
                llab-size 2/ to llab-size               \ only clear half
                                                        \ of the labels later
                LabPtr lref-start + lref-size  0 fill   \ zero references
                max-lref 0                              \ release all records
                do      i release-ref                   \ to the free chain
                loop    llset+                          \ next group
             loop
\+ llset_cnt    off> llset_cnt                          \ clear set counter
        then    -1 to cur-label                         \ clear current label
                -1 to cur-ref                           \ clear current ref
                LabPtr llab-start + llab-size -1 fill ; \ -1 labels

initialization-chain chain-add llab-init

: llab-zero     ( -- )                  \ zero out all local labels PERIOD!
                [ forth ]               \ locals and globals
                llab-init                                  \ first norm init
                LabPtr llab-start + llab-size 2* -1 fill   \ -1 labels
                LabPtr lref-start + lref-size     0 fill   \ zero references
                -1 to cur-label                            \ clear current label
                -1 to cur-ref                              \ clear current ref
                max-lref 0                                 \ release all records
                do      i release-ref                      \ to the free chain
                loop    ;

: clr-1lab      ( n1 -- )                       \ zero out a local label record
                target-a;
                [ forth ]
                llab>line b/llab -1 fill ;

: clr-ref       ( n1 -- )                       \ zero out this reference record
                [ forth ]
                lref>line b/lref 0 fill ;       \ clear link, type & taddr

\ **********************************************************************
\ used by the assembler to install the information needed to resolve
\ a forward reference.
\ **********************************************************************

defer asm!lref

: !lref         ( targ-adr ref-type -- )        \ store targ-adr & ref-type
                [ forth ]                       \ into the current ref rec
                cur-ref 0<                      \ was there a forward ref?
                if      2drop                   \ no, discard parameters
                else    cur-ref lref>line >r    \ yes, fill in ref record
                        r@ >rtype !             \ set the reference type
                        r> >taddr !             \ set the target address
                        -1 to cur-ref           \ reset reference number
                then    ;

' !lref is asm!lref                     \ link into deferred word

\ **********************************************************************
\ Reference a numbered local label
\ **********************************************************************

: $             ( n1 -- a1 )            \ reference local label n1 and
                                        \ return a1 the resolved address
                [ forth ]
                dup to cur-label        \ set current label
                llab>line >r            \ translate to physical label
                r@ >lladr @ -1 <>       \ if not -1 then resolved
        if      r@ >lladr !             \ just return the resolve addr
                -1 to cur-ref           \ clear any current reference
        else    cur-ref -1 <>
            if  cur-ref lref>line >rtype @      \ get previous reference type
                0= abort" Label or symbol incorrectly assembled"
            then                                \ check previous label was setup ok
                allocate-ref to cur-ref         \ get a new reference record,
                                                \ mark it as the current ref record
                cur-ref clr-ref                 \ zero out the reference record
                r@ >llptr @                     \ get and save current ref link
                cur-ref r@ >llptr !             \ link reference record to locl label
                cur-ref lref>line >rlink !
                                                \ install link from prev ref record
                target-here                     \ return the target "HERE"
        then    2r> 2drop ;

\ **********************************************************************
\ Local label resolution words
\ **********************************************************************

defer resolve_ref       ( a1 n1 -- )

: res_1ref      ( n1 -- n1 )            \ resolve one forward reference
                [ forth ]
                dup  lref>line                  \ translate to physical ref
                dup >taddr @ swap               \ get the target address
                    >rtype @                    \ and the reference type
                resolve_ref ;                   \ go resolve the reference

\ **********************************************************************
\ An error handling function, warns of errors without aborting
\ **********************************************************************

defer "err      \ resolution errors are displayed by this function

: _"err         ( a1 n1 -- )    \ default to a simple error handler
                cr type ;

' _"err is "err

\ **********************************************************************
\ Define a numbered local label at the current target "HERE"
\ **********************************************************************

: $:            ( n1 -- )
                >r target-a; r>         \ force compilation of prev instr
                [ forth ]
                -1 to cur-ref                   \ clear any current reference
                dup to cur-label
                llab>line >r                    \ translate to physical label
                r@ >lladr dup @ -1 <>
                if      s" Branch label xxx is already resolved"
                        cur-label 0 <# # # # #> drop 2 pick 13 + 3 move
                        "err                    \ with inserted label #
                then
                target-here swap !              \ install resolved targ addr
                                                \ into the local label field
                begin   r@ >llptr @ dup -1 <>   \ while ref rec<>-1
                while   res_1ref                \ res one reference
                        dup lref>line >rlink @  \ get next reference
                        r@ >llptr !             \ link to prev ref
                        dup  clr-ref            \ zero the ref record
                             release-ref        \ release the ref rec
                repeat  r>drop drop ;

\ **********************************************************************
\  Some linkage into the assembler to perform needed initialization
\ **********************************************************************

0 value ll-global?              \ are we using local labels globally?

: lab-start     ( -- )
                [ forth ]
                ll-global? 0=
                if      llab-init               \ in case labels used
                then    ;

: #ll-errs?     ( n1 -- )       \ DUMMY to check for unresolved labels
                [ forth ]
                base @ >r decimal
                loc-llab min 0                  \ only check the LOCAL labels
                ?do     i llab>line >llptr @ -1 <>
                        if      s" Branch label xxx was not resolved"
                                i 0 <# # # # #> drop 2 pick 13 + 3 move
                                "err            \ with inserted label #
                        then
                loop    r> base ! ;

: ll-errs?      ( -- )
                loc-llab #ll-errs? ;

: lab-end       ( -- )
                [ forth ]
                ll-global? 0=
                if      ll-errs?        \ check for local label errors
                then    ;

: res_ERROR     ( targ_addr -- )
                drop s" Illegal forward resolution type" "err abort ;

: res_RELATIVE_8,  ( targ_addr -- )             \ resolve a byte RELATIVE targ addr
                here over 1+ - dup 0x-80 0x7F between 0=
                if      s"  Byte Relative Address out of range " "err
                then    swap Target-8! ;

: res_RELATIVE_16,  ( targ_addr -- )            \ resolve a word RELATIVE targ addr
                here over 2 + - dup 0x-8000 0x7FFF between 0=
                if      s"  Word Relative Address out of range " "err
                then    swap Target-16! ;

: res_SIGNED_8, ( targ_addr -- )
                here dup 0x-80 0x7F between 0=
                if      s"  Signed byte offset out of range " "err
                then    swap Target-8! ;        \ resolve a byte OFFSET targ addr

: res_OFFSET_8, ( targ_addr -- )
                here dup 0x00 0xFF between 0=
                if      s"  Byte Offset out of range " "err
                then    swap Target-8! ;        \ resolve a byte OFFSET targ addr

: res_OFFSET_16, ( targ_addr -- )
                here dup 0x00 0xFFFF between 0=
                if      s"  Word Offset out of range " "err
                then    swap Target-16! ;       \ resolve a word OFFSET targ addr

: res_OFFSET_32, ( targ_addr -- )
                here swap Target-32! ;          \ resolve a long OFFSET targ addr

: res_RELATIVE_32,  ( targ_addr -- )    \ resolve a long RELATIVE targ addr
                here over cell+ - swap Target-32! ;

: resolve_x86   ( targ_addr res_type -- )       \ resolve a forward reference
                0max 7 min exec:
                res_ERROR               \ any lower = error
                res_RELATIVE_8,         \ type 1
                res_RELATIVE_16,        \ type 2
                res_OFFSET_8,           \ type 3
                res_OFFSET_16,          \ type 4
                res_SIGNED_8,           \ type 5
                res_OFFSET_32,          \ type 6
                res_RELATIVE_32,        \ type 7
                res_ERROR ;             \ any higher = error

' resolve_x86 is resolve_ref

: RELATIVE_8,   ( a1 -- )
                here 1+ - dup 0x-80 0x7F between 0=
                if      s"  RELATIVE_8 Address out of range" "err
                then    here 1 asm!lref                 \ res type 1
                Target-8, ;                             \ 8bit RELATIVE

: RELATIVE_16,  ( a1 -- )
                here 2 + - dup 0x-8000 0x7FFF between 0=
                if      s"  RELATIVE_16 Address out of range" "err
                then    here 2 asm!lref                 \ res type 2
                Target-16, ;                           \ 16bit RELATIVE

: RELATIVE_32,  ( a1 -- )
                here cell+ -
                here 7 asm!lref                         \ res type 7
                Target-32, ;                            \ 32bit RELATIVE

: SIGNED_8,     ( a1 -- )
                dup 0x-80 0x7F between 0=
                if      s"  SIGNED_8 Address out of range" "err
                then    here 5 asm!lref                 \ res type 5
                Target-8, ;                             \ 8bit SIGNED OFFSET

: OFFSET_8,     ( a1 -- )
                dup 0x00 0xFF between 0=
                if      s"  OFFSET_8 Address out of range" "err
                then    here 3 asm!lref                 \ res type 3
                Target-8, ;                             \ 8bit OFFSET

: OFFSET_16,    ( a1 -- )
                dup 0x0000 0xFFFF between 0=
                if      s"  OFFSET_16 Address out of range" "err
                then    here 4 asm!lref                 \ res type 4
                Target-16, ;                           \ 16bit OFFSET

: OFFSET_32,    ( a1 -- )
                here 6 asm!lref                         \ res type 6
                Target-32, ;                            \ 32bit OFFSET

\ **********************************************************************
\ Select whether local labels are to be used globally
\ **********************************************************************

synonym CLEAR_LABELS LLAB-INIT

forth definitions

synonym CLEAR_ALL_LABELS LLAB-ZERO

: LOCAL_REF     ( --- )
                false to  LL-GLOBAL? ;       LOCAL_REF
                                        \ default to LOCAL references only

: GLOBAL_REF    ( --- )
                true to LL-GLOBAL? ;

only forth also definitions

checkstack

\ **********************************************************************
\ a utility to display the label record and forward reference record
\ chain of local label record n1.
\ **********************************************************************

: .lab          ( n1 -- )               \ display a local label record
                [ target also forth ]
                llab>line 2>r
                cr ." Address  = " r@ >lladr @ h.
                r> >llptr @
                begin   cr ." Ref rec  = " dup h.
                        dup -1 <>
                while   lref>line >r
                        ."  at target = " r@ >taddr @  h.
                        ."  of type   = " r@ >rtype @  h.
                        ."  links to  = " r> >rlink @  dup h.
                repeat  drop ;


