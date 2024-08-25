((
        High Level Debug

        Given a filename, displays the text file
))

:object   hibughelp <super helpmsg
:M WindowTitle: ( -- Zstring )
                Z" High Level Trace: Key Usage" ;M
;object

create    hibughelptext
         z," Space = Step one instruction\n"
        +z," ESC   = done\n"
        +z," ^C    = copy to clipboard\n"
        +z," (I)nto  = Step into highlighted instruction\n"
        +z," (O)utof = Link out of highlighted instruction\n"
        +z," a(N)imate = Step until any key is pressed\n"
        +z," Click to place cursor, or use left/right arrows\n"
        0 c,

100 constant maxbugfiles                \ up to 100 hyperlinks
128 constant hibug-width                \ record width

 40 value hborgx                        \ default start position
 60 value hborgy

 60 value hbw0                          \ default width & height
 32 value hbh0

: "hibugxy"     ( -- )                  \ save X,Y starting position (pels)
                hborgx (.)        pad  place
                s"  TO HBORGX "   pad +place
                hborgy (.)        pad +place
                s"  TO HBORGY "   pad +place
                hbw0 (.)          pad +place
                s"  TO HBW0 "     pad +place
                hbh0 (.)          pad +place
                s"  TO HBH0 "     pad +place
                pad count orgsave ;

orgsaver chain-add "hibugxy"

0 value hibug-files                     \ space for file stack
0 value hibug-filesp                    \ file stack pointer, 0 = empty

: init-hibug    hibug-width maxbugfiles * malloc to hibug-files ;
: free-hibug    hibug-files release ;

init-mem        chain-add init-hibug    \ add to initialization chain
free-mem        chain-add free-hibug

: PaFIND        ( addr -- addr FALSE | local# -1 )
\ Find local name in list, return the local# instead of a CFA
        false parms
        if      ( addr false )
                OVER COUNT "FIRSTPARM   \ put in first parm slot
                PARMS 1+ 1
                DO      PARMLIST I NAME-MAX-CHARS 1+ * + COUNT
                        PARMLIST COUNT
                        COMPARE 0=                      \ TRUE if matched
                        IF      2drop PARMS  I  -  TRUE    \ ********
                                LEAVE
                        THEN
                LOOP
        then    ;


: hibug-clear   ( -- )  0 to hibug-filesp ;

: hibug-push    ( a len position -- )
                hibug-files
                hibug-filesp hibug-width * +
                swap !+ place
                hibug-filesp 1+ maxbugfiles 1- min
                to hibug-filesp ;

: hibug-drop    ( -- )
                hibug-filesp 1- 0max
                to hibug-filesp ;

: hibug'pick    ( index -- a )
                >r hibug-files
                hibug-filesp r> - 0max hibug-width * + ;

: hibug-pick    ( index -- a len position )
                hibug'pick  @+ >r count r> ;

: hibug-pop     ( -- a len position )
                hibug-drop  0 hibug-pick ;

defer hibug-hyper+      ( a1 len1 row a2 len2 -- )
defer hibug-hyper-      ( -- )
defer $hibug            ( a -- )


0 value hstate          \ colorizer state

: colorcomment  ( a n row col char -- a n row col forecolor )
\ append text to temp$ until char delimiter or end-of-line is found
                -rot 2>r                        ( a n ch | r c )
                over >r pluck >r scan           ( a' n' | r c n a )
                dup
                if      1 /string               \ include delimiter
                        false to hstate         \ turn off skipping
                then
                r> r>                           ( a' n' a n | r c )
                pluck -                         ( a' n' a len | r c )
                temp$ +place
                2r>  2 ;                        \ comment color

: _constant constant ;

false to sys-warning?

wordlist value hibug-color   \ -------------------------------------------
also hibug-color set-current \ colorization wordlist

\ all words in this list have the stack effect
\ ( a' n' row col -- a' n' row col forecolor )
\ control structures include a mark/resolve action:
\ 1 = >mark
\ 2 = >resolve
\ 3 = <mark
\ 4 = <resolve
\ 5 = >mark swap >resolve
\ 6 = >mark <mark
\ 7 = >mark swap
\ 8 = <resolve >resolve
\ 9 = dup <resolve
\ F = <mark >mark

0x101 _constant AHEAD
0x101 _constant IF             \ control structure words
0x501 _constant ELSE
0x201 _constant THEN
0x201 _constant ENDIF
0x301 _constant BEGIN
0x701 _constant WHILE
0x801 _constant REPEAT
0x601 _constant MULTI
0x401 _constant UNTIL
0x401 _constant AGAIN
0x301 _constant DO
0x301 _constant ?DO
0x401 _constant LOOP
0x401 _constant +LOOP
0x901 _constant LEAVE
1 _constant UNLOOP
1 _constant EXIT
0xB01 _constant CASE
0xC01 _constant OF
0xD01 _constant ENDOF
0xE01 _constant ENDCASE
0x101 _constant {
0x201 _constant }
0x101 _constant ?[
0x201 _constant ]?
0x101 _constant IFSET
0x101 _constant IFCLR

3 _constant CONSTANT            \ defining words ...
3 _constant 2CONSTANT
3 _constant VARIABLE
3 _constant 2VARIABLE
3 _constant STRING
3 _constant CREATE
3 _constant PROGRAM
3 _constant INCLUDE             \ looks good, not really a defining word
4 _constant HOST                \ a few host directives ...
4 _constant BUILDING
4 _constant TOKENIZING
4 _constant ONLY
4 _constant ALSO
4 _constant FORTH
4 _constant DEFINITIONS
4 _constant ASMBYTE
4 _constant ASMWORD
4 _constant ASMLONG
4 _constant MAIN-TOKENS
4 _constant LOW-TOKENS
4 _constant TEMP-TOKENS
: ((        1 to hstate  2 ;
: ))        0 to hstate  2 ;
: C(    ')' 2 to hstate  colorcomment ; \ parse comment text until )
: (     ')' 2 to hstate  colorcomment ; \ parse comment text until )
: \     -1  colorcomment  ;
3 _constant :
1 _constant ;
1 _constant I
1 _constant J
previous definitions        \ --------------------------------------------

: hibug-colorize ( a' n' row col -- a' n' row col forecolor )
\ choose the color for the word in temp$, which may fool with the parameters
                hstate 2 =
                if      ')' colorcomment
                else
                        temp$ c@                        \ not in comment mode
                        if      get-order   hibug-color 1 set-order
                                temp$ CAPS-FIND 2>r
                                set-order    2r>
                                if      execute
                                else    drop 0          \ regular text
                                then
                        else    4                       \ nul string
                        then    hstate
                        if      drop 2  \ force comment color if still comment
                        then
                then
                ;

: hb-0bran      ( . . f -- . )
\ remove flag from target data stack, use for conditional branch
                tpop
                if      nip
                else    drop    \ take branch
                then    ;

: hb-multi      ( . . f -- . )
\ remove flag from target data stack, use for conditional branch
                0 bpickr
        if      s" R> 1- >R " tar-evaluate
                nip
        else    drop
        then    ;

: hb-do         ( . . -- . )
\ fake a DO instruction: put params on loop stack
                nip
                s" SWAP >R >R" tar-evaluate ;

: hb-loop       ( . . -- . )
                s" R> 1+ R>" tar-evaluate
                tpop tpop       ( limit index )
                2dup =
        if      2drop nip       \ loop terminated
        else    tpush tpush
                s" >R >R" tar-evaluate
                drop
        then    ;

2variable temploop ( L I' )

: hb-+loop      ( . . -- . )  \ IJL
                s" R> SWAP OVER + R>" tar-evaluate
                tpop tpop 2dup temploop 2! tpop  ( L I' I )
                rot dup>r - swap r> - ( I-L I'-L )
                xor 0x8000 and
        if      nip           \ loop terminated
        else    temploop 2@ tpush tpush
                s" >R >R" tar-evaluate
                drop
        then    ;

defer getnext ( -- addr )

: hb-local      ( -- )  \ get local names up to }
                        \ begin local variable usage in the form;
                        \ { initedloc1 initedloc2 \ uninitedloc3 -- comments }
                0 to PARMS
                0 TO INPARMS
                1 TO LOCFLG
                BEGIN   getnext ?UPPERCASE COUNT
                        FIRSTCHR [CHAR] - <>            \ as in { name -- }
                WHILE   FIRSTCHR [CHAR] \ =             \ start of non-initialized
                                                        \ local variables as in
                                                        \ { name \ another -- }
                        IF      0 TO LOCFLG 2DROP
                        ELSE    FIRSTCHR [CHAR] } = ABORT" Args missing --"
                                (LOCAL)
                        THEN
                REPEAT  2DROP
                PARMS 0>
                IF      INPARMS PARMS dup localdepth ! swap - ( extras )
                        0 ?do 0 tpush loop
                        PARMS 0 ?do s" >R" tar-evaluate loop
                THEN
                BEGIN   getnext COUNT  dup 0= abort" Missing }"
                        FIRSTCHR [CHAR] } = NIP NIP
                UNTIL   ;

: hb-ifset      ( . . n bit# -- n . )
                1 tpop lshift
                tpop dup tpush and
                if      nip
                else    drop    \ take branch
                then    ;

: hb-ifclr      ( . . n bit# -- n . )
                1 tpop lshift
                tpop dup tpush and 0=
                if      nip
                else    drop    \ take branch
                then    ;

variable qcbyte

: hb-qc         tpop qcbyte c! ;

: hb-ifbyte     ( . . n -- n . )
                tpop qcbyte c@ =
                if      nip
                else    drop    \ take branch
                then    ;



wordlist value hibug-struc   \ -------------------------------------------
also hibug-struc set-current \ colorization wordlist

\ all words in this list have the stack effect ( dest cursor -- cursor' )

: QCASE:        nip hb-qc ;
: ?[            hb-ifbyte ;
: IF            hb-0bran ;
: IFSET         hb-ifset ;
: IFCLR         hb-ifclr ;
: ELSE          drop ;
: THEN          nip ;
: ENDIF         nip ;
: AHEAD         drop ;
: BEGIN         nip ;
: MULTI         hb-multi ;
: WHILE         hb-0bran ;
: UNTIL         hb-0bran ;
: REPEAT        drop ;
: AGAIN         drop ;
: DO            hb-do ;
: ?DO           hb-do ;
: LOOP          hb-loop ;
: +LOOP         hb-+loop ;
: LEAVE         drop ;
: UNLOOP        nip s" R>DROP R>DROP" tar-evaluate ;
: EXIT          nip k_F10 pushkey ;
: ]?            nip k_F10 pushkey ;
: ?EXIT         nip tpop if k_F10 pushkey then ;
: {             drop hb-local ;
: }             nip ;

: [']           nip k_F8  pushkey ;
: TO            nip 'T' +k_control  pushkey ;
: C"            nip 'U' +k_control  pushkey ;
: S"            nip 'V' +k_control  pushkey ;
: ."            nip 'W' +k_control  pushkey ;
: [             nip 'X' +k_control  pushkey ;
\ CASE
\ OF
\ ENDOF
\ ENDCASE
: ;             nip parms 0 ?do s" R> DROP" tar-evaluate loop  0 to parms
                k_F10 pushkey ;
: I             nip s" R@" tar-evaluate ;      
: J             nip s" R> R> R@ SWAP >R SWAP >R" tar-evaluate ;
previous definitions        \ --------------------------------------------

: hibug-eval    ( dest cursor -- cursor' )
\ evaluate the word in temp$
                temp$ c@
                if      temp$ ?UPPERCASE PaFIND
                   if   parms swap - 1- bpickr tpush nip
                   else drop
                        get-order   hibug-struc 1 set-order
                        temp$ CAPS-FIND 2>r
                        set-order    2r>
                        if      execute
                        else    drop nip
                                temp$ _tarinterp  \ run word on the target
                        then
                   then
                else    nip
                then    PFA-local off ;

true to sys-warning?
msgwindow hibugmsg


:Object HIBUG <super textwindow

 16 constant words/line         \ average words per line * 2
128 constant stacksize          \ size of control stack space

int word-table                  \ cursor position of each steppable word
int #words                      \ word tally for fileposition table
int cursor                      \ composite cursor position in word table
int stack                       \ control stack
int sp
int currentfile                 \ current file name

:M MaxStartSize: ( -- w h )     hbw0 hbh0 ;M
:M SaveTxtSize:  ( c r -- )     to hbh0 to hbw0 ;M

: helptext      ( -- a n )      \ the top help line of the window
s" Step Into Outof aNimate " ;

create filename 128 allot

:M StartPos:    ( -- x y )      hborgx hborgy ;M
:M SaveStartPos: ( x y -- )     to hborgy to hborgx ;M

:M OnTop:       ( -- f )        true ;M

:M TopMargin:   ( -- height )      char-height 3 + ;M    

:M LeftMarginColor: ( -- color )   ltred ;M
:M DefaultColor: ( -- c )          (black) 16 * (ltgray) + ;M

:M CreateBuf:   ( cols rows -- )
\ Allocate memory for buffers
                tuck CreateBuffer: self
                words/line *
                cells malloc to word-table      \ word fileposition table
                stacksize cells malloc to stack
                256 malloc to currentfile
                0 to #words
                0 to cursor
                0 to sp
                ;M

:M DestroyBuf:  ( -- )
\ Free allocated memory
                currentfile release
                stack release
                word-table release
                destroybuffer: self
                ;M

: cs-push       ( n -- )
                sp 1+ stacksize 1- min  to sp
                stack sp th !
                ;

: cs-pop        ( -- n )
                stack sp th @
                sp 1- 0max              to sp
                ;

: .cs           ( -- )
                '[' emit sp (.) type ']' emit space
                sp 0
                ?do     stack i 1+ th @ .
                loop    ;

: cs-dup        ( -- )  cs-pop dup cs-push cs-push ;
: cs-swap       ( -- )  cs-pop cs-pop swap cs-push cs-push ;

: z>cr          ( position -- col row ) textwidth /mod ;
: cr>z          ( col row -- position ) textwidth * + ;
: cursor@       ( -- position )         word-table cursor 2* th @ ;

: cursor!       ( position -- )
\ binary search for position, set cursor
                pad !
                #words 1+ cheaplog
                1- 1 swap lshift   
                dup                             ( index step )
                begin   ?dup
                while   2/
                        over #words u<
                        if      word-table pluck 1- 2* th @
                        else    -1              ( i s tcfa )
                        then    
                        pad @   2dup =          ( i s n1 n2 )
                        if      3drop           ( i )
                                1- to cursor
                                exit
                        then    u<
                        if      tuck +
                        else    tuck -
                        then    swap 
                repeat
                1- to cursor ;

:M ReTop:       { \ title$ pad$ -- }
\ refresh the title bar, used inside textwindow on_paint:
                MAXSTRING localAlloc: title$
                       32 localAlloc: pad$
                pad 32 - pad$ 32 move           \ save PAD
                s" Debug: "                title$  place
                currentfile count          title$ +place
                cursor@ z>cr swap
                s" ,  Column: "            title$ +place
                1+ (.)                     title$ +place
                s" /"                      title$ +place
                textwidth (.)              title$ +place
                s" ,  Line: "              title$ +place
                1+ (.)                     title$ +place
                s" /"                      title$ +place
                textheight (.)             title$ +place
                s"   Depth="               title$ +place
                hibug-filesp (.)           title$ +place
\                s"   Words="               title$ +place
\                #words (.)                 title$ +place
                title$ count SetTitle: self
                pad$ pad 32 - 32 move           \ restore PAD
                SaveDC: dc                      \ Draw the help line
 \                get-dc
                Handle: txtFont SetFont: dc     \ set the font to be used
                TopMarginColor: self   SetBkColor: dc
                Gray                 SetTextColor: dc
                2 1 helptext
                false to ?win-error-enabled \ for NT
                textwidth leftjust        TextOut: dc
 \                release-dc
                RestoreDC: dc    \ !!!!!!!!!!
                ;M

:M On_Init:     ( -- )
                On_Init: super
                SuspendCursor: self ;M

:M On_Done:     ( -- )
                On_Done: super
                DestroyBuf: self
                ;M

int oldcursor

: hilite        ( position color -- )
\ color the background of the text at position
                word-table rot 2* th @ >r  ( color | offset )
                bufferptr r@ + stringlength
                buffercolor drop r@ + swap  ( color a len )
                r> z>cr nip set-pending
                bounds
                ?do     i c@  0xF0 and over or
                        i c!
                loop    drop  ;

: showcurs      ( -- )
\ keep cursor on the screen
                oldcursor -1 <>
                if      oldcursor (ltgray) hilite
                then    cursor    (ltyellow) hilite
                cursor to oldcursor
                cursor@ z>cr qat-xy: self
                MakeViewable: self
                ReTop: self
                ;

:M getcursor:   ( -- n )        cursor@ ;M

:M savecursor:  ( n -- )
\ copy the current cursor position onto the hyperlink stack so that
\ it will be restored to its current position by hyper-
                1 hibug'pick ! ;M

create colors
                4 c,
                (black) c,      \ 0 = normal text
                (blue) c,       \ 1 = control structure
                (green) c,      \ 2 = comment
                (red) c,        \ 3 = defining word
                (cyan) c,       \ 4 = host directive

variable cursorptr              \ current cursor position

: >mark         ( 'special -- 'special )
\                cr ." >MARK " temp$ $type space
                dup cs-push
                ;

: >resolve      ( 'special -- 'special )
\                cr ." >RESOLVE " temp$ $type space
                cursorptr @ cs-pop
                dup  word-table u<                      \ underrange?
                over word-table #words 1+ 2* th u> or   \ overrange?
                if      2drop
                else    !
                then    ;

: <mark         ( 'special -- 'special )
\                cr ." <MARK " temp$ $type space
                cursorptr @ cs-push ;

: <resolve      ( 'special -- 'special )
\                cr ." <RESOLVE " temp$ $type space
                cs-pop over !   ;

\ 1 = >mark
\ 2 = >resolve
\ 3 = <mark
\ 4 = <resolve
\ 5 = >resolve >mark  (else)
\ 6 = >mark <mark
\ 7 = >mark swap
\ 8 = <resolve >resolve
\ 9 = dup <resolve

: evalstructure ( 'special type -- )
                case    1 of    >mark                   endof
                        2 of    >resolve                endof
                        3 of    <mark                   endof
                        4 of    <resolve                endof
                        5 of    >resolve >mark          endof
                        6 of    >mark <mark cs-pop 1- cs-push endof \ multi
                        7 of    >mark cs-swap           endof
                        8 of    <resolve >resolve       endof
                        9 of    cs-dup <resolve         endof
                endcase drop ;

variable structuretype          \ type of control structure, 0 = none
variable hbcolor

: HiEval        ( a' n' row col -- a' n' )
\ evaluate temp$, alter colors at row,col
                hibug-colorize                          \ evaluate temp$
                byte-split structuretype !
                dup hbcolor !
                colors count rot min 0max               \ clip index
                + c@ 4 lshift                           \ look up fg color
                >r
                swap textwidth * +  dup cursorptr !     \ cursor position
                hbcolor @ 0 1 between
                if      0 over                          \ steppable word
                        word-table #words 2* th
                        dup>r 2! r> cell+               ( 'special-addr )
                        structuretype @  dup
                        hstate 0= and
                        if      evalstructure           \ control structure
                        else    2drop
                        then
                        1 +to #words
                then
                buffercolor drop +                      \ -> text color
                temp$ c@
                r> DefaultColor: self or  fill          \ set color
                ;


: HiEvaluate    ( a n row -- )
\ evaluate this line, change colors in the display window
\ if hstate = 2, parse this line until ) is found
                pluck 2>r                                ( a n | row a )
                begin   bl skip  dup
                while   over >r  bl scan  r@            \ parse next word
                        pluck over -  temp$ place       ( a' n' ) \ into temp$
                        r> 2r@  rot swap -              ( a' n' row col )
                        HiEval                          ( a' n' ) \ eval temp$
                repeat  2r> 2drop 2drop
                ;


int infile      \ handle for input file

: opennewfile   ( a n -- )
\ opens a new file window, file is already open.
                key? drop
                s" Opening Source Text" MessageText: hibugmsg
                key? drop
                true ontop: hibugmsg
                start: hibugmsg
                0 0
                begin   pad 128 infile read-line
                        abort" Error reading file"      ( len eof )
                while           max  1 under+
                repeat  drop
                0.0 infile reposition-file drop         ( rows cols )
                swap CreateBuf: self                    \ buffer for window
                -1 to oldcursor
                0 to hstate
                0 to sp
                currentfile place                       \ save filename
                DefaultColor: self >fgbg: self          \ default color
                textheight 0
                ?do     0 i qat-xy: self
                        pad 128 infile read-line nip    ( len ior )
                        abort" error reading file"
                        pad swap  2dup vtype: self      \ send to screen
                        i HiEvaluate                    \ set highlighting
                loop
                close: hibugmsg
                Start: self                             \ open window
                ;

:M LoadFile:    ( a len -- ior )
\ given a file name, load it into the text buffer:
\ scan file for row count & max column, reset file
\ load file into text buffer, close file
                2dup r/o open-file
        if      3drop true                      \ couldn't open file
        else    to infile                       \ file is open
                hwnd
                if      currentfile count
                        2over compare
                        if      close: self     \ starting a new window
                                opennewfile
                        else    2drop ReTop: self
                        then
                else    opennewfile
                then    false
                infile close-file drop
        then
                ;M

: movecursor    ( n -- f )
\ move cursor n positions, T if a limit was hit
                cursor + 0max  #words 1- min
                dup to cursor
                dup 0= swap #words 1- = or      \ hit a limit?
                ;

: cursor+       ( -- )  1 movecursor drop showcurs ;  \ next word
: cursor-       ( -- ) -1 movecursor drop showcurs ;  \ previous word
:M >cursor+:    ( position -- )    cursor! cursor+ ;M

:M Click:       ( -- )
\ position hilighted cursor on word nearest mouse cursor
                have-focus?  MouseViewable?: self  and
                if      MouseCR: self           \ current mouse C,R coords
                        cr>z bufferptr +        \ -> the word
                        begin   dup c@ bl <>
                        while   1-
                        repeat  1+
                        bufferptr -             \ 1st character in the word
                        cursor!  showcurs
                then    ;M

:M Word+:       ( a len row -- position )
\ Position the cursor on the word immediately after the word "a len",
\ searching the given row.  Return the actual position of the cursor.
                1- 0max >r
                0 r@ cr>z  dup cursor!             \ default if not found
                bufferptr + textwidth
                pad place    pad count upper
                temp$ place  temp$ count upper     \ both strings uppercase
                pad count
                begin   bl skip ?dup               ( a n . )
                while   over >r  bl scan  r@       \ parse next word
                        pluck over -               ( a' n' a len )
                        temp$ count compare 0=
                        if      r> pad char+ -
                                0 r@ cr>z +
                                cursor!            \ position cursor
                                drop 0             \ terminate search
                        else    r>drop
                        then
                repeat  drop
                r>drop
                cursor+                            \ bump cursor right
                cursor@ ;M

: briteword     ( -- a len )
\ return the string that is highlighted
                cursor@ bufferptr +               \ -> text
                dup stringlength ;

: briteeval     ( -- )
\ evaluate the highlighted word
                communicable?
        if      briteword temp$ place
                word-table cursor 2* th cell+ @   \ special parameter
                cursor@ hibug-eval cursor!        \ evaluate the word in temp$
                cursor+   targetsnap              \ display results
        then    ;

:noname         ( -- addr )
                cursor+ briteword temp$ place temp$ ; is getnext

: tick          ( -- )
\ find the highlighted word's xt, push it onto the stack
                communicable?
        if      briteword temp$ place
                temp$ $findtoken t.xt tpush
                cursor+   targetsnap              \ display results
        then    ;

: doto          ( -- )
                communicable?
        if      briteword temp$ place             \ execute postfix equivalent
                temp$ _tarinterp
                s" (%TO!)" tar-eval
                cursor+   targetsnap              \ display results
        then    ;

: loadstring    ( -- )
\ parse string" from the file and save to memory
                communicable?
        if      cursor@ bufferptr +               \ -> text
                dup 80 '"' scan  swap >r          ( a 80-len | dest )
                80 swap -  temp$ place            \ save counted string
                temp$ dup c@ 1+                   ( src len )
                tar-free + 2/ dup>r swap write-data \ load string to free memory
                if      ."  error loading string to memory "
                then
                r> tpush                          \ destination location
                r> bufferptr - cursor! cursor+
        then    ;

: do-c"         ( -- )
                loadstring         ( -- a )
                targetsnap                        \ display results
                ;

: do-s"         ( -- )
                communicable?
        if      loadstring         ( -- a n )
                s" COUNT" tar-eval
                targetsnap                        \ display results
        then    ;

: do-."         ( -- )
                communicable?
        if      loadstring
                s" COUNT" tar-eval
                s" TYPE"  tar-eval
                targetsnap                        \ display results
        then    ;

: do[]          ( -- )  \ evaluate home words until ]
                depth >r
                ']' word count 1- evaluate
                depth r> -  ( ... diff )
                dup 0< if true abort" [ stack underflow ]" then
        case    0 of endof
                1 of tpush              endof
                2 of swap  tpush tpush  endof
                3 of swap rot tpush tpush tpush  endof
                abort" too many params"
        endcase ;

: hyperlink     ( -- )
\ hyperlink to highlighted word
                briteword temp$ place
                temp$ $hibug ;

: animate       ( -- )
\ briteeval until a key is pressed or faked
                begin   key? 0=
                while   briteeval
                repeat  ;

: showhelp      ( -- )
                hibughelptext help: hibughelp ;

:M dokey:       ( c f -- c f' )
\ Perform key action and set f if this window has focus,
                have-focus? hWnd and
        if      drop true               \ set the "have-active" flag
                over upc
                case    8               of   cursor-            endof
                        k_left          of   cursor-            endof
                        k_right         of   cursor+            endof
                        k_F1            of   showhelp           endof
                        27              of   close: self        endof
                        13              of   briteeval          endof
                        bl              of   briteeval          endof
                        'I'             of   hyperlink          endof
                        'O'             of   hibug-hyper-       endof
                        'S'             of   briteeval          endof
                        'N'             of   animate            endof
                        k_F8            of   tick               endof
                        k_F9            of   hyperlink          endof
                        k_F10           of   hibug-hyper-       endof
                        'T' +k_control  of   doto               endof
                        'U' +k_control  of   do-c"              endof
                        'V' +k_control  of   do-s"              endof
                        'W' +k_control  of   do-."              endof
                        'X' +k_control  of   do[]               endof
                        'C' +k_control  of   copy-to-clipboard  endof
                        'P'             of   Paint: self        endof
                endcase
        then    ;M

;object

: _hbclick      ( -- )  click: hibug ;
: hbclick       ( -- )  ['] _hbclick  SetClickFunc: hibug ;

:noname         ( a1 len1 row# a2 len2 -- )
\ link to word a1len1 in file a2len2, row row#
                getcursor: hibug >r
                2dup loadfile: hibug
                if      r>drop beep
                        2drop 3drop             \ couldn't open file
                else    r> savecursor: hibug
                        2>r word+: hibug        \ save filename, set cursor
                        2r> rot hibug-push      \ save parameters on stack
                        hbclick
                        ReTop: hibug
                then
                ; is hibug-hyper+

:noname         ( -- )
\ unlink to the previous file and cursor
                hibug-drop
                hibug-filesp 0>
        if      1 hibug-pick >r
                loadfile: hibug                 \ load old file
                if      r>drop
                else    r> >cursor+: hibug      \ jump to cursor & bump
                then    hbclick
        else    close: hibug                    \ no old file to load
        then    ; is hibug-hyper-

false to sys-warning?

:noname         ( a -- )
\ debug a word in the image using the high level debugger
                $findtoken
                communicable?
        if      callable?
            if  hilitecontext? 0= if also then
                tester _testing  forcecore
                t.xt xt>name   t.filepos
                t.fileid filenamesize * filenames + count
                hibug-hyper+
                PFA-local off
            then
        then    ; is $HIBUG

: HBUG          ( <name> -- )
                hibug-clear
                bl word $hibug ;

: open-hibug    ( -- )
                communicable?
        if      callable?
            if  hilitecontext? 0= if also then forcecore
                tester _testing
                cur-file count LoadFile: hibug drop
                hibug-clear hbclick
            then
        then    ;

true to sys-warning?


