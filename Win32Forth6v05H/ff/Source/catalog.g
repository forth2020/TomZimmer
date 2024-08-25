\ --------------------  Token browser  -----------------------------------

\ Lists all words that are:
\   in the search order
\   token header structures

\ Since the only way to tell if a word is a token header structure is to
\ execute it (we don't want to execute any non-token headers), the
\ tokenvocabulary list is used to decide which wordlists contain token
\ headers.

   12 constant   vocnamesize
create vocabname vocnamesize allot
: vocname$      ( -- a n )   vocabname vocnamesize -trailing ;

defer listtoken ( a n -- )  :noname type space ; is listtoken

0 value tokencount
0 value catclosed?

80 value catx0
80 value caty0
30 value catz0

: "catxy"       ( -- )                  \ save X,Y starting position (pels)
                catx0 (.)  pad  place   s"  TO CATX0 "   pad +place
                caty0 (.)  pad +place   s"  TO CATY0 "   pad +place
                catz0 (.)  pad +place   s"  TO CATZ0 "   pad +place
                pad count orgsave ;

orgsaver chain-add "catxy"

: listtokendry  ( a n -- )
\ first action of listtoken, count number of tokens to list
                2drop 1 +to tokencount ;


\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ \\\\\\\\   Token Catalog Window Object      \\\\\\\\\\\\\\\\\\\\\\\\\\\
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

msgwindow catpopup

:object   cathelp <super helpmsg
:M WindowTitle: ( -- Zstring )
                Z" Token Catalog Key Usage" ;M
;object

create    cathelptext
         z," ^C  = copy to clipboard\n"
        +z," A   = sort by address\n"
        +z," D   = sort by data type\n"
        +z," N   = sort by name\n"
        +z," T   = sort by token#\n"
        +z," U   = sort by usage\n"
        +z," W   = sort by wordlist\n"
        +z," +   = ascending mode\n"
        +z," -   = descending mode\n"
        +z," ^P  = repaint\n"
        +z," ctrl-shift-G = glossary\n"
        +z," ESC = done\n"
        0 c,


:Class CatClass <super TextWindow

    int indextable          \ -> text indicies used by sort
    int PFAtable            \ -> PFAs associated with each line

12 constant c.namewidth         \ NAME          ASCII, left justified
 6 constant c.xtwidth           \ XT            decimal, right justified
11 constant c.dtwidth           \ Datatype      ASCII, left justified
 4 constant c.uswidth           \ Usage         decimal, right justified
: c.vocwidth    vocnamesize ;   \ Voc Name      ASCII, left justified
: c.cfawidth    addrnibbles ;   \ CFA           hex, fixed width
: c.helpwidth   catstringsize ; \ Help text     ASCII, left justified

: helptext       ( -- a n )      \ the top help line of the window
s"  Name        Token#  Datatype  Use Wordlist    Addr      Description" ;

0                               constant col.name
col.name     c.namewidth +      constant col.xt
col.xt       c.xtwidth +  1+    constant col.datatype
col.datatype c.dtwidth +        constant col.usage
col.usage    c.uswidth +        constant col.wordlist
col.wordlist vocnamesize +      constant col.addr
: col.description ( -- col )    col.addr     c.cfawidth + 2 +  ;
: col.end         ( -- col )    col.description c.helpwidth +  ;

:M MaxStartPos: ( -- c r )      col.end catz0 ;M
:M SaveTxtSize: ( c r -- )      nip to catz0 ;M
:M StartPos:    ( -- x y )      catx0 caty0 ;M
:M SaveStartPos: ( x y -- )     to caty0 to catx0 ;M

:M OnTop:       ( -- f )        true ;M  \ for test only {}

:M ReTop:       ( -- )
\ refresh the top bar, also the title bar.
\ used inside textwindow on_paint:
                SaveDC: dc              \ Draw the help line
                Handle: txtFont SetFont: dc  \ set the font to be used
                TopMarginColor: [ self ]  SetBkColor: dc
                Red                     SetTextColor: dc
                2 1 helptext coloffset /string
                textwidth leftjust           TextOut: dc
                RestoreDC: dc
                textheight (.)       temp$  place
                s"  tokens in search order, "       temp$ +place
                #watch (.)           temp$ +place
                s"  watched.  "      temp$ +place
                Ascending? if s" Ascending"
                         else s" Descending"
                         then        temp$ +place
                temp$ count SetTitle: self
                ;M

:M LeftMarginColor: ( -- color )    ltgreen ;M

:M WindowStyle: ( -- style )
                WS_CAPTION
                WS_THICKFRAME   or
                WS_SYSMENU      or
                WS_MAXIMIZEBOX  or
                ;M

:M TopMargin:   ( -- height )
                char-height 3 + ;M

:M Linewidth:   ( -- cols )     col.end ;M

create flagcolors
                (blue) c,       \ normal
                (green) c,      \ call-only
                (red) c,        \ immediate
                (yellow) c,     \ call-only + immediate (unlikely)

create flagchars
\       none   c      i      c+i    m      m+c    m+i    m+c+i
        bl c, 'c' c, 'i' c, 'b' c, 'm' c, 'C' c, 'I' c, 'B' c,

:M HilightLine: ( n -- )
\ force red foreground for line n, no way to un-hilight without re-list
                textwidth * buffercolor drop +  \ -> color data for this line
                textwidth bounds                ( bgcolor . . )
                ?do     i c@ 15 and             \ keep same background
                        (ltred) 4 lshift or     \ make all text red
                        i c!
                loop    ;M

:M ListToken:   ( a n -- )
\ Display the next token information.  Name is a n, other info is from
\ parameter list pointed to by PFA-local.  Uses PAD.
                0 tokencount  qat-xy: self
                t.cputype bit#tokenized? and   \ tokenized code?
                if      t.cputype 0xFF and     \ yes
                        if    (ltgreen) \ machine code
                        else  (ltgray)  \ tokenized code (no CPU type)
                        then
                else    (ltcyan)        \ ROM code
                then     >bg: self
                t.flags 3 and
                flagcolors + c@ >fg: self
                c.namewidth leftjust            vtype: self
                t.xt 4 (h.) c.xtwidth rightjust vtype: self  vspace: self
                (black)  >fg: self
                t.flags 7 and flagchars + c@    vemit: self             \ flags
                t.datatype dt>s c.dtwidth 1- leftjust vtype: self       \ data type
                (green)  >fg: self
                t.usage 999 min (.) 3 over - 0max vspaces: self         \ usage
                              vtype: self  vspace: self
                (black)  >fg: self
                vocabname  c.vocwidth           vtype: self             \ vocab
                (red)    >fg: self
                t.cfa -1 =
                if      s"                " c.cfawidth min              \ cfa
                else    t.cfa c.cfawidth (h.)
                then    vtype: self  vspace: self
                (blue)   >fg: self              vspace: self
                t.catstring c.helpwidth         vtype: self             \ summary
                PFA-local @ PFAtable tokencount th !  \ build PFA list
                t.xt watched?
                if      tokencount HilightLine: self  \ hilite if watched
                then
                1 +to tokencount ;M

:M CreateBuffer: ( c r -- )
                CreateBuffer: super
                textheight cells  dup
                malloc to PFAtable
                malloc to indextable
                ;M

:M DestroyBuffer: ( -- )
                true to catclosed?
                indextable release
                PFAtable release
                DestroyBuffer: super
                ;M


:M On_Init:     ( -- )
                On_Init: super
                SuspendCursor: self ;M

:M On_Done:     ( -- )
                On_Done: super
                DestroyBuffer: self
                ;M

:M DefaultColor: ( -- c )       (black) 16 * (ltgray) + ;M

: ?exec         ( row col xt begin end -- row )
\ execute xt (which uses row) if col is within bounds
                rot >r within
                if      r> execute
                else    r> drop
                then    ;

: ?sort         ( col begin end -- )
\ sort at col if col is within bounds
                >r tuck r> within
                if      sort: self
                else    drop
                then    ;

:M ExtractXT:   ( row -- n )
\ convert the hex number in the token field to a number
                textwidth * bufferptr +
                col.xt +  c.xtwidth
                bl skip  0.0 2swap
                base{ >number }base
                3drop  ;M

:M ClickName:   ( row -- )      ." Name" ExtractXT: self . ;M
:M ClickXT:     ( row -- )      ." XT"   ExtractXT: self . ;M
:M ClickDatatype: ( row -- )    ." dt"   ExtractXT: self . ;M
:M ClickUsage:  ( row -- )      ." Use"  ExtractXT: self . ;M
:M ClickWordlist: ( row -- )    ." wl"   ExtractXT: self . ;M
:M ClickCFA:    ( row -- )      ." CFA"  ExtractXT: self . ;M

: ck0           ( row -- row )  dup     ClickName: [ self ] ;
: ck1           ( row -- row )  dup       ClickXT: [ self ] ;
: ck2           ( row -- row )  dup ClickDataType: [ self ] ;
: ck3           ( row -- row )  dup    ClickUsage: [ self ] ;
: ck4           ( row -- row )  dup ClickWordlist: [ self ] ;
: ck5           ( row -- row )  dup      ClickCFA: [ self ] ;

:M Click:       ( -- )
\ process single mouse click
                MouseCR: self                   ( col row )
                MouseViewable?: self
                if      over ['] ck0 col.name     col.xt        ?exec
                        over ['] ck1 col.xt       col.datatype  ?exec
                        over ['] ck2 col.datatype col.usage     ?exec
                        over ['] ck3 col.usage    col.wordlist  ?exec
                        over ['] ck4 col.wordlist col.addr      ?exec
                        over ['] ck5 col.addr  dup c.cfawidth + ?exec
                        2drop
                else    rowoffset -             \ remove row offset
                        -1 =                    \ in the help line?
                        if      dup  col.name     col.xt        ?sort
                                dup  col.xt       col.datatype  ?sort
                                dup  col.datatype col.usage     ?sort
                                dup  col.usage    col.wordlist  ?sort
                                dup  col.wordlist col.addr      ?sort
                                dup  col.addr   col.description ?sort
                                col.description   col.end       ?sort
                        else    drop
                        then
                then
                ;M


: showhelp      ( -- )
                cathelptext  help: cathelp ;

: sort          ( col -- )      sort: self ;

: sortorder     ( n -- )        to ascending?  ReTop: self ;

\ --------------------------------------------------------------------------
\ glossary maker

: skipline      13 scan 1 /string 10 skip ;

: $fileline     ( line# filename -- a len )  { \ fileid tempbuf maxbyte -- }
\ open a file, extract a line and close it.
\ Uses PAD, leaves next immediate line in temp$.
                swap 1- 0max >r
                1024 kb malloc to tempbuf
                count r/o open-file abort" Couldn't open file" to fileid
                tempbuf 1024 kb fileid read-file abort" file read error"
                tempbuf swap    ( a n | count )
        begin   dup 0<> r@ and          \ not at end and not at line
        while   skipline
                r> 1- >r
        repeat  r>drop                  ( a' n' )
                over >r skipline over >r
                skipline drop r@ - r@ swap
                2 - 0max                \ omit CRLF
                temp$ place             ( | '1st '2nd )
                2r> over - 2 - 0max
                pad place
                fileid close-file drop
                tempbuf release pad count ;

14 value w_gname        \ name
28 value w_gsp          \ stack picture
 6 value w_gxt
12 value w_gdt
 1 value moreglos

create disp$ 256 allot
: disp$write    ( -- )
                disp$ count pad place fendline ; \ write line to file

create stdspics
                ,"  "    ," -- x" ," -- d" ," -- a"     \ 0..3
                ," -- a"         ," -- x" ," -- d" ," -- a"     \ 4..7
                ," -- a" ," -- a" ," -- x1 x2" ," -- x" \ 8..B
                ," -- x" ," --"   ,"  "  0 c,           \ C..E

: .spic         ( n -- a n )
                0xFF and 14 min stdspics (string) dup 1 >
        if      s" ( " pad place pad +place  \ make our own stack pic
                s"  )" pad +place  pad count
        else    2drop s"  "
        then    ; 

: _displaytoken ( a n -- ) { \ tally -- }
                w_gname leftjust                disp$ place     \ NAME
                t.filepos t.filename $fileline
        begin   '(' scan
                over 1- c@ bl <> >r     \ look for ' ( '
                over 1+ c@ bl <> r> or
                over and
        while   1 /string
        repeat  dup
        if      over >r ')' scan drop 1+  ( 'end | 'begin )
                r> swap over -
        else    2drop
                t.datatype .spic
        then    w_gsp leftjust                  disp$ +place    \ STACK PICTURE
                s" xt = 0x"                     disp$ +place
                base{ t.xt 0 <# # #s #> w_gxt leftjust disp$ +place  \ XT
                }base 
                t.datatype dt>s w_gdt leftjust  disp$ +place    \ optional data type
                vocname$                        disp$ +place    \ VOCABULARY
                disp$write                                      \ --------------
                s"  " w_gname leftjust          disp$ place
                t.catstring catstringsize                       \ DESCRIPTION
                over 1 upper                    disp$ +place    \ 1st char is upper case
                s"                      "        disp$ +place
                t.flags 2 and
                if      s" IMMEDIATE" disp$ +place
                then
                1 to tally
        begin   temp$ count 2 min s" \ " compare 0=
                moreglos and
        while   disp$write                                      \ --------------
                s"  " w_gname leftjust          disp$ place
                temp$ count 2 /string           disp$ +place    \ extra comments
                t.filepos tally + t.filename $fileline 2drop    \ next comment line
                1 +to tally
        repeat
                disp$write                                      \ --------------
                disp$ off disp$write            \ blank line
                ;

: makeglossary  ( -- )
                glosname openoutput
                tokencount 0
        do      i ExtractXT: self dup>r xt>npfa nip PFA-local !
                r> xt>name  _displaytoken
        loop    closeoutput beep
                cr ." Created text glossary: " glosname count type space
                close: self ;

: flpad         ( <name> -- ) ( -- )  \ run-time: write constant text to pad
                create -1 word dup c@ 1+ 0
                ?do count c, loop drop
                does> count disp$ +place ;
: fline         ( <name> -- ) ( -- )  \ run-time: write constant text to file
                create -1 word dup c@ 1+ 0
                ?do count c, loop drop
                does> count pad place fendline ;
\ beginning of HTML file
fline  fl0 <html>
fline  fl1 <head>
fline  fl2 <meta http-equiv="Content-Type" content="text/html; charset=windows-1252">
fline  fl3 <meta name="GENERATOR" content="Firmware Studio">
fline  fl4 <title>GLOSSARY</title>
fline  fl5 </head>
fline  fl6 <body>
fline  fl7 <table border="0" width="100%">
\ end of HTML file
fline fl10 </table>
fline fl11 </body>
fline fl12 </html>

fline   fltr+  <tr>
fline   fltr-  </tr>
flpad flname0   <td valign="top" align="left" nowrap><b>
flpad flname1 &nbsp;</b></td>
flpad   flsp0   <td nowrap><i>
flpad   flsp1 </i></td>
flpad   flsu0   <td nowrap><b>
flpad   flsu1 </b></td>
flpad   flxt0   <td nowrap>
flpad   flxt1 </td>
flpad  flimm0   <td><i>
flpad  flimm1 </i></td>
flpad  flcom0   <td width="100%" colspan="4">
flpad  flcom1 </td>

fline flb0 <td valign="top" bgcolor="#008080"><font color="#FFFFFF"
fline flb1 face="Arial"><strong>Name</strong></font></td>
fline flb2 face="Arial"><strong>Stack Effect</strong></font></td>
fline flb3 face="Arial"><strong>Summary</strong></font></td>
fline flb4 face="Arial"><strong>Token</strong></font></td>
fline flb5 face="Arial"><strong>Wordlist</strong></font></td>
: hheader fltr+ flb0 flb1 flb0 flb2 flb0 flb3 flb0 flb4 flb0 flb5 fltr- ;

: +html         ( a n -- )
\ append to disp$ but handle special characters
 bounds ?do     i c@ case
                        '&' of s" &amp;"  disp$ +place endof
                        '<' of s" &lt;"   disp$ +place endof
                        '>' of s" &gt;"   disp$ +place endof
                        '"' of s" &quot;" disp$ +place endof
                        disp$ count + c!
                        1 disp$ c+!  0
                endcase
        loop    ;

: _Hdisplaytoken ( a n -- ) { \ tally remaining -- }
                256 localalloc to remaining       \ comment remaining on this line
                fltr+ disp$ off
                flname0 +html flname1 disp$write  \ NAME field
                t.filepos t.filename $fileline
                \ 2dup type cr \ |||
                2dup -trailing remaining place
        begin   '(' scan
                over 1- c@ bl <> >r     \ look for ' ( '
                over 1+ c@ bl <> r> or
                over and
        while   1 /string
        repeat  dup
        if      over >r ')' scan
                2dup -trailing remaining place
                drop 1+  ( 'end | 'begin )
                r> swap over -
        else    2drop
                t.datatype .spic
        then    disp$ off flsp0 +html flsp1 disp$write         \ STACK PICTURE
                disp$ off flsu0 t.catstring catstringsize      \ DESCRIPTION
                over 1 upper                                   \ 1st char is upper case
                -trailing +html flsu1 disp$write
                disp$ off flxt0 s" xt = 0x" +html
                base{ t.xt 0 <# # #s #> +html                  \ XT
                }base flxt1 disp$write
                disp$ off flxt0 vocname$ +html flxt1 disp$write \ VOCABULARY
                fltr-
                temp$ count 2 min s" \ " compare 0=  \ commentary
                t.flags 2 and or                     \ or immediate
                t.datatype dt>s -trailing nip or     \ or a datatype?
        if      fltr+
                disp$ off flimm0
                t.flags 2 and
                if      s" IMMEDIATE" +html
                then
                t.datatype dt>s -trailing
                dup if s"  " +html then +html                   \ optional data type
                flimm1 disp$write
                disp$ off flcom0
                remaining count '\' scan dup
        if      begin   1 /string
                        2dup '\' scan dup
                while   2swap 2drop
                repeat  2drop
                bl skip over 1 upper
                +html disp$write
                disp$ off
        else    2drop
        then
                1 to tally
        begin   temp$ count 2 min s" \ " compare 0=
                moreglos and
        while   disp$write
                disp$ off
                temp$ count 2 /string +html                  \ extra comments
                t.filepos tally + t.filename $fileline 2drop \ next comment line
                1 +to tally
        repeat  flcom1 disp$write
                fltr-
        then    ;

: makeHTML      ( -- )          \ 11/7/01 added HTML format
                glosnamh openoutput
                fl0 fl1 fl2 fl3 fl4 fl5 fl6 fl7
                tokencount 0
        do      i 20 mod 0= if hheader then     \ header every 20 tokens
                i ExtractXT: self dup>r xt>npfa nip PFA-local !
                r> xt>name  _Hdisplaytoken
        loop    fl10 fl11 fl12 closeoutput beep
                cr ." Created HTML glossary: " glosnamh count type space
                close: self ;

:M dokey:       ( c f -- c f' )
\ Perform key action and set f if this window has focus,
                have-focus? hWnd and
        if      drop true               \ set the "have-active" flag
                over upc
                case    k_end           of   999999 vpan        endof
                        k_home          of   -999999 vpan       endof
                        k_down          of    1 vpan            endof
                        k_up            of   -1 vpan            endof
                        k_pgdn          of   maxrow vpan        endof
                        k_pgup          of   maxrow negate vpan endof
                        'C' +k_control  of   copy-to-clipboard  endof
                        'P' +k_control  of   Paint: self        endof
                        'N'             of   col.name     sort  endof
                        'T'             of   col.xt       sort  endof
                        'D'             of   col.datatype sort  endof
                        'W'             of   col.wordlist sort  endof
                        'A'             of   col.addr     sort  endof
                        'U'             of   col.usage    sort  endof
                        '+'             of   -1 sortorder       endof
                        '-'             of    0 sortorder       endof
                        k_F1            of   showhelp           endof
                        27              of   close: self        endof
                        'G' +k_control +k_shift of makeglossary endof
                        'H' +k_control +k_shift of makeHTML     endof
                endcase
        then    ;M

;Class

\ -----------------------------------------------------------------------
\ Define click actions for catalog window

:Object CatWindow <super CatClass

:M ClickXT:     ( row -- )
                ExtractXT: self  xt>npfa nip PFA-local !
                t.cputype bit#tokenized? and
                if      t.cfa t.datatype $see
                else    t.cfa $disassemble
                then    ;M

:M ClickCFA:    ( row -- )      ClickXT: self ;M

:M ClickName:   ( row -- )
                ExtractXT: self  xt>npfa nip PFA-local !
                t.filepos t.filename $browse
                ;M

:M ClickDataType: ( row -- )
                dup  ExtractXT: self xtaddwatch \ add to watch list
                HilightLine: self               \ highlight the added line
                paint: self
                ;M

:M ClickWordlist: ( row -- )    drop ;M
:M ClickUsage:  ( row -- )      drop ;M

;Object


\ -----------------------------------------------------------------------
\ Define click actions for catalog parameter selection window

:Object CatParamWindow <super CatClass

:M ClickXT:     ( row -- )
                ExtractXT: self
                to catparam  close: self
                ;M

:M ClickCFA:    ( row -- )
                ExtractXT: self  xt>npfa nip PFA-local !
                t.cfa
                to catparam  close: self
                ;M

:M ClickName:   ( row -- )
                ClickCFA: self
                ;M

:M ClickDatatype: ( row -- )
                drop ;M

:M ClickWordlist: ( row -- )
                drop ;M

:M ClickUsage:    ( row -- )
                drop ;M
;Object


: (scantokens)  ( voc -- )
\ scan tokens in a wordlist that is known to contain all token headers or
\ non-stack-altering words.
                DUP VOC#THREADS >R
                HERE 500 + R@ CELLS MOVE        \ copy vocabulary up
                BEGIN   HERE 500 + R@ LARGEST DUP
                WHILE   0 PFA-local !
                        DUP LINK> execute       \ -> xt parameters
                        PFA-local @
                        dup magic_nosim <>
                        if      if      dup L>NAME
                                        NFA-COUNT       ( a n )
                                        listtoken
                                else    l>name .id ." is in " vocname$ type space
                                        true abort" Wordlist must contain only tokens: "
                                then
                        else    drop            \ skip vocabularies
                        then
                        @ SWAP !
                REPEAT  2DROP  R>DROP
                0 PFA-local ! ;

: scantokens    ( -- )
\ Scan all tokens in the search order.  Set the action for LISTTOKEN first.
                #vocs 1-  0
        ?do     context i th @  ?dup
                if      voc>vcfa >name          \ nfa of a voc in the order
                        vocabularycount 0
                        ?do     vocabularylist i th  @
                                over =
                                if      ( valid vocab )
                                        dup nfa-count
                                        vocabname vocnamesize blank
                                        vocnamesize min
                                        vocabname swap move
                                        context j th @ (scantokens)
                                then
                        loop    drop
                then
        loop    ;

: listtokenwet  ( a n -- )
\ first action of listtoken, count number of tokens to list
                cr type space .localtoken ;

: dumptokens    ( -- )
                0 to tokencount
                ['] listtokendry is listtoken  scantokens
                tokencount cr . ." tokens:"
                ['] listtokenwet is listtoken  scantokens ;


: listtokencat  ( a n -- )
\ list the next token to the catalog text buffer
                ListToken: CatWindow ;

: listtokenpcat ( a n -- )
\ list the next token to the other catalog text buffer
                ListToken: CatParamWindow ;

: catclick      click: catwindow ;
: catparamclick click: catparamwindow ;

: cat#tokens    ( -- c r  )
                0 to tokencount
                   linewidth: CatWindow \ # of columns needed
                ['] listtokendry is listtoken  scantokens
                tokencount ;            \ # of rows needed

: OpenCatWindow ( -- )
\ create a text window and display the tokens in it
                cat#tokens
                CreateBuffer: CatWindow \ create the buffer
                0 to tokencount
                ['] listtokencat is listtoken  scantokens  \ dump the data
                       Start: CatWindow
                ['] catclick  SetClickFunc: CatWindow
                ;

:noname         ( -- n )
\ Get a parameter from the token browser
                0 to catclosed?
                0 to catparam
                cat#tokens
                CreateBuffer: CatParamWindow \ create the buffer
                0 to tokencount
                ['] listtokenpcat is listtoken  scantokens  \ dump the data
                        Start: CatParamWindow
                ['] catparamclick  SetClickFunc: CatParamWindow
                begin   key? drop
                        catclosed?
                until   catparam
                ;  is getcatval


