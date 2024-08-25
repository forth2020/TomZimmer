\ MODULE.F              Local Definitions for Modules           by Tom Zimmer

cr .( Loading Modules...)

only forth also hidden also definitions

0 value pre-voc         \ previous vocabulary

forth definitions

: INTERNAL      ( -- )  \ start the internal list of words for a module
                context @ ['] hidden vcfa>voc =         \ if hidden is ontop
                if      hidden definitions              \ just do definitions
                else    current @ to pre-voc            \ else save definitions
                        also hidden definitions         \ and do definitions
                then    ;

: EXTERNAL      ( -- )  \ Start the external list of words for a module
                pre-voc 0= abort" Use Only while building a module"
                pre-voc current ! ;

: MODULE        ( -- )  \ complete the module
                EXTERNAL
                context @ ['] hidden vcfa>voc =
                if      previous                \ of the internal vocabulary
                then    0 to pre-voc ;

only forth also definitions

\S

INTERNAL

.. internal definitions ..

EXTERNAL

.. externally available definitions ..

MODULE

.. back to whatever vocabulary we started in ..

