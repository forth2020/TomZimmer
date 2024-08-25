\ header list saver

\ anew mystuff0

1 [IF]
: headtype      ( a len -- )
                outfile write-file
                abort" Error writing file" ;

create $cr   13 c, 10 c,

: headcr        ( -- )  $cr 2 headtype ;

[ELSE]

: headtype  type ;  : headcr cr ;

[THEN]


24 constant tsmargin
16 constant tswidth

: padshim       ( -- )          s"  " pad +place ;
: headtypecr    ( a len -- )    headtype headcr ;

: +.hex         ( n -- )
\ append hex number to pad, must begin with 0..9
                base{ (.)
                over c@ 'A' >=
                if      s" 0" pad +place
                then    pad +place
                padshim
                }base ;

: +.spaces      ( n -- )
                spcs swap pad +place ;

: +.tab         ( col -- )
                pad c@ - 0max +.spaces ;

: $vocname      ( -- a len )
                vocabname vocnamesize  ;

create tsvocname 64 allot

: taddtoken     ( a n -- )
\ create token record and output it, performed by scantokens
                $vocname tsvocname over compare         \ R X X   | X
                if      s" UNDERALSO "      pad  place  \ R X X X | X
                        $vocname -trailing  pad +place  \ R X T X | X
                        s"  UNDERDEFS "     pad +place  \ R X T X | T
                        S"  UNDERPREVIOUS"  pad +place  \ R X X   | T
                        pad count headtypecr
                        $vocname tsvocname swap move
                then
                0 pad c!
                t.xt       +.hex   padshim              \ data fields
                t.filepos  +.hex
                t.fileid   +.hex
                t.datatype +.hex
                -1 ( t.cfa ) +.hex
                t.cputype  +.hex
                tsmargin pad c@ - 0max  +.spaces        \ append blanks
                s" add-token " pad +place
                tuck pad +place                         \ name
                tswidth swap - 1 max    +.spaces
                t.catstring catstringsize -trailing dup
                if      s" c( " pad +place pad +place  \ catalog comment
                        s" ) "    pad +place
                else    2drop
                then
                t.flags ?dup
                if  +.hex s" >FLAGS " pad +place then   \ flags, if necessary
                isliteral?
                if  t.litvalue +.hex s" >LITERAL " pad +place then
                pad count headtype headcr
                ;

: tsave         ( <filename> -- )
\ save token assignment information to a file, suitable for FLOADing
                bl word dup openoutput
                cr ." Writing token list to " $type 
   s" HOST    HOMEORDER       \ base search order"      headtypecr
   s" NEW-IMAGE  NOFILES  HEX \ clear image & headers " headtypecr
   s" WARNING OFF"                                              headtypecr
                vocabularycount 0
                ?do     s" VOCABULARY " headtype \ create vocabularies
                        i vocname headtypecr
                loop
                filenamecount 0                       \ create filenames
                ?do     i (.) headtype s"  ADDFILE " headtype
                        i filenamesize *  filenames +
                        count headtypecr
                loop
   s" \ Add-token parameters (hex values):"        headtypecr
   s" \ XT       token# of this word"              headtypecr
   s" \ FILEPOS  position in file of source"       headtypecr
   s" \ FILEID   file ID # (see above list)"       headtypecr
   s" \ DATATYPE data identifier"                  headtypecr
   s" \ -1       CFA is not saved"                 headtypecr
   s" \ CPUTYPE  b8=1 for tokens, low byte nonzero for native code" headtypecr
   ['] taddtoken is listtoken  scantokens
   s" DEFINITIONS  WARNING ON  "   headtypecr
   s" MAIN-TOKENS " pad place  token# +.hex
   s"  >TOKEN#"  pad +place        pad count        headtypecr   \ last token#
   s" DECIMAL"   headtypecr
                closeoutput ;

variable previfile

: idxtoken      ( a n -- )
                t.fileid previfile @ <>
        if      t.fileid previfile !
                1 pad c! 9 pad 1+ c!
                t.filename count pad +place
                pad count headtypecr
        then    pad place  s"  " pad +place             \ name
                t.filepos (.d) pad +place
                pad count headtypecr ;

: idxsave       ( <filename> -- )
\ save hyperlink index for Winview
                bl word openoutput
                1234 previfile !
                ['] idxtoken is listtoken  scantokens
                closeoutput ;

false to sys-warning?

: DefinedInList? ( a1 n1 a2 n2 -- f )
\ is word1 findable in wordlist2?
                2>r temp$ place
                get-order
                only home also 2r> evaluate previous    \ set up search order
                temp$ find nip >r
                set-order r> ;

: hiddenword?   ( a n -- f )    s" HIDDEN" DefinedInList? ;
: coreword?     ( a n -- f )    s" CORE"   DefinedInList? ;

true to sys-warning?

: VMtemplate    ( <filename> -- )
\ create a template file that can be used with another Forth to create
\ a virtual machine with the same bytecode assignments
                bl word openoutput cr ." Writing VM template file..."
s" \ Virtual Machine Template: Modify for your particular Forth"  headtypecr
                headcr
                s" HEX"                         headtypecr
                s" ONLY FORTH ALSO DEFINITIONS" headtypecr
                s" VOCABULARY VCORE"            headtypecr
                s" VCORE DEFINITIONS"           headtypecr
                headcr
                usedtokens 0
                ?do     i xt>name 2dup coreword?
                        if      s" : " pad place                \ :
                                2dup pad +place                 \ name
                                16 +.tab
                                i xt>npfa nip pfa-local !
                                s" \ " pad +place  i +.hex      \ \ xt
                                t.catstring catstringsize -trailing pad +place
                                pad count headtypecr
                                pad off 16 +.tab
                                pad +place                      \ name
                                s"  ;" pad +place               \ ;
                                40 +.tab
                                immediate? if s"  IMMEDIATE" pad +place then
                                pad count headtypecr
                        else    2drop
                        then
                        key? if leave then
                loop
                s" FORTH DEFINITIONS"           headtypecr
                s" CREATE INITTABLE"            headtypecr
                usedtokens 0 over
                pad off +.hex
                s" , " pad +place pad count  headtypecr pad off
                ?do     xt-table i th @
                        if      i xt>name
                                2dup hiddenword?
                                if      2drop s" hidden"
                                then
                        else    s" none"        \ no token
                        then
                        s" ' "  pad +place
                        tuck    pad +place
                        s"  , " pad +place
                        12 swap - 0max +.spaces
                        i 4 mod 3 =
                        if      s" \ " pad +place i 3 - +.hex
                                pad count headtypecr pad off
                                key? if leave then
                        then
                loop    pad count headtypecr
                headcr
                s" DECIMAL"                             headtypecr
                closeoutput
                ." done " ;

: CodeTemplate  ( <filename> -- )
\ create a template file that can be used with an external assembler to make
\ a list file from which comma'd code words can be extracted
                bl word openoutput cr ." Writing code template file."
                s" ; ******** CODE TEMPLATE FOR CORE WORDS ********"
                headtypecr
                headcr
                usedtokens 0
                ?do     i xt>name 2dup coreword?
                        if      s" ;CODE "      pad place       \ CODE
                                                pad +place      \ name
                                16 +.tab
                                s" ( -- )  \ "  pad +place  i +.hex   \ \ xt
                                pad count headtypecr
                                i xt>npfa nip pfa-local !
                                s" ;c( " pad place               \ c(
                                t.catstring catstringsize -trailing pad +place
                                s"  )" pad +place                \ )
                                immediate? if s"  \ IMMEDIATE" pad +place then
                                pad count headtypecr
                        else    2drop
                        then
                        key? if leave then
                loop
                closeoutput ;


