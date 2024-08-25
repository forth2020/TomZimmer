\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ \\\\\\\\\\            Low Level Debugger                  \\\\\\\\\\
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

 200 value lborgx                       \ default start position
 100 value lborgy
 60 value  lbw0                         \ default max width & height
 32 value  lbh0

: "lobugxy"     ( -- )                  \ save X,Y starting position (pels)
                lborgx (.)        pad  place
                s"  TO LBORGX "   pad +place
                lborgy (.)        pad +place
                s"  TO LBORGY "   pad +place
                lbw0 (.)          pad +place
                s"  TO LBW0 "     pad +place
                lbh0 (.)          pad +place
                s"  TO LBH0 "     pad +place
                pad count orgsave ;

orgsaver chain-add "lobugxy"

: nulldisassembly ( n x -- n a len ) drop 1 + s" No disassembler available" ;

create disassemblers  #families cells allot

: nodisassemblers ( -- )
\ erase the disassembler list
                #families 0
                ?do     ['] nulldisassembly
                        disassemblers i th !
                loop    ;

: new-disasm    ( n <name> -- )
\ define a new disassembler
                '                       ( n xt )
                disassemblers rot th !
                ;

nodisassemblers

create watchloaders  #families cells allot

: nowatchloaders ( -- )
\ erase the disassembler list
                #families 0
                ?do     ['] noop
                        watchloaders i th !
                loop    ;

: new-watchloader ( n <name> -- )
\ define a new lo-watch parameter loader
                '                       ( n xt )
                watchloaders rot th !
                ;

nowatchloaders

: disassemble ( at ah  -- at' a len )
\ given target address and address of actual data, returns the next
\ disassemblable address and a string
                disassemblers
                CPUfamily th @ execute ;

variable disassyPC      \ current trace program counter
variable disassyBRK     \ breakpoint
variable disassyORG     \ origin address of disassembly
 8 value disassyDatawidth

: >disassy      dup disassyPC !         \ set hard defaults for BUG
                dup disassyBRK !
                disassyORG ! ;

defer _disname ' drop is _disname       \ resolve in TOKEN.G
defer getcatval                         \ resolve in CATALOG.G

:object   lobughelp <super helpmsg
:M WindowTitle: ( -- Zstring )
                Z" Low Level Trace: Key Usage" ;M
;object

create    lobughelptext
         z," F1 = help\n"
        +z," F2 = toggle Browse/Edit  (white = editable)\n"
        +z," F5 = refresh: upload/disassemble new data\n\n"
        +z," Enter = accept edited field\n"
        +z," ESC = done\n\n"
        +z," ^C  = copy to clipboard\n"
        +z," (S)tep = Run until PC points to the next line\n"
        +z," (I)nto = Execute one instruction\n"
        +z," (G)o   = Run until breakpoint (blue)\n"
        +z," a(N)imate = (I)nto until any key is pressed\n"
        +z," Click to place breakpoint or set cursor\n"
        +z," DoubleClick to place PC or browse tokens\n"
        0 c,

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

((
Low level debug window, based on textwindow class.


Left margin contains editable hex fields with labels,

Each hex field contains a text header, data size, address of data,
xt of fetching word, xt of storing word.  There is one of these data
structures per line.

The left margin configuration data is held in a RAM array and is loaded
from a CREATEd array at AcquireTarget time.  Each processor type has its
own array.

Several host parameters appear in the watch area, such as:

PC      The current PC for the debug thread
ORG     Origin of the disassembly
BRKPT   The next breakpoint

Clicking on a line sets the breakpoint
Double clicking on a line sets the PC

))

:object lobug <super textwindow

 true value editing?
    0 value watchdata           \ pointer to watch parameter array
    0 value addresses           \ pointer to viewable target addresses
    0 value roidata             \ pointer to disassemblable buffer
  512 value roisize             \ max size of temporary data
    6 value labelwidth          \ # of chars allowed in the label
    3 value leftgutter
    0 value ed-row              \ cursor position of field to edit
    0 value ed-digit

 200 value lborgx              \ default start position
 100 value lborgy

:M OnTop:       ( -- f )        true ;M

:M LeftMarginColor: ( -- color )    ltgray ;M
:M MaxStartSize: ( -- w h )     32 22 ;M
:M StartPos:    ( -- x y )      lborgx lborgy ;M
:M SaveStartPos: ( x y -- )     to lborgy to lborgx ;M
:M MaxStartSize: ( -- w h )     lbw0 lbh0 ;M
:M SaveTxtSize:  ( c r -- )     to lbh0 to lbw0 ;M

: helptext      ( -- a n )      \ the top help line of the window
s" Step Into Go Animate Refresh" ;

:M DefaultColor: ( -- c )       (black) 16 * (ltgray) + ;M

create hilitecolors
                (ltgray) c,     \ none
                (ltgreen) c,    \ PC
                (ltblue) c,     \ breakpoint
                (ltcyan) c,     \ both

:M LeftMargin:  ( -- width )
                labelwidth addrnibbles +  1+    \ # of chars in margin
                char-width *  leftgutter 2* +   \ + gutter
                ;M

:M TopMargin:   ( -- height )    char-height 2 + ;M  \ |||

:M WindowTitle: ( -- Zstring )                  \ window caption
                z" Machine Level Trace,  F1 = help"
                ;M

4 cells 1+ labelwidth + align  constant watchwidth
\ width of the watch parameter array
\ data structure: [xt(@)][xt(!)][a][n][datasize][label]

: watchsize     ( -- n )
                textheight watchwidth * ;

: watchrow      ( n -- a )
                watchwidth * watchdata + ;

: 'watch@       ( n -- a )      watchrow ;  \ indicies into watch structure
: 'watch!       ( n -- a )      watchrow 1 th ;
: 'address      ( n -- a )      watchrow 2 th ;
: 'value        ( n -- a )      watchrow 3 th ;
: 'datasize     ( n -- a )      watchrow 4 th ;
: 'label        ( n -- a )      'datasize 1+ ;


:M AddWatch:    ( n  xt@ xt! addr datasize a len -- )
                >r >r >r >r >r >r watchrow     \ -> place to put data
                r> !+ r> !+ r> !+
                0  !+ r> lay                 \ lay XTs and datasize
                dup labelwidth blank            \ label is initially blank
                r> swap r> move                 \ place label
                ;M

: AddDefaults   ( -- )
\ Loads default values based on CPUfamily
  0  ['] @ ['] !  disassyPC   addrnibbles 2/  s" PC"   AddWatch: self
  1  ['] @ ['] !  disassyBRK  addrnibbles 2/  s" BRK"  AddWatch: self
  2  ['] @ ['] !  disassyORG  addrnibbles 2/  s" ORG"  AddWatch: self
                watchloaders
                CPUfamily th @ execute ;        \ load the rest (if any)

:M CreateBuffer: ( c r -- )
                CreateBuffer: super             \ buffers for text & margin
                watchsize malloc to watchdata   \ create buffer for the params
                watchdata watchsize erase       \ clear parameter list
                roisize malloc to roidata
                roidata roisize erase           \ region of interest buffer
                textheight cells malloc to addresses \ address list
                AddDefaults
                ;M

:M DestroyBuffer: ( -- )
                addresses release
                roidata release
                watchdata release
                DestroyBuffer: super
                ;M

-123456789 value ROIorg         \ first time needs upload

: UploadBlock   ( -- )
\ upload block of data from target board to region-of-interest buffer
\ This is invoked whenever disassyORG is changed.
                ROIorg disassyORG @ <>
        if      disassyORG @ to ROIorg          \ mark new origin
                true to tarprogress?
                disassyORG @ roidata roisize
                read-code drop                   \ load memory
        then    ;


0 value dishere

: DisassembleWin ( -- )
\ convert roidata to ASCII text suitable for repainting.
                clrscreen
                disassyORG @
                textheight 0
        ?do     0 i qat-xy: self
                dup  addresses i th !             \ add to address list
                dup  disassyPC  @ = 1 and         \ highlight special lines
                over disassyBRK @ = 2 and  or
                hilitecolors +  c@     >bg: self
                (red)                  >fg: self
                dup addrnibbles (h.) vtype: self \ address
                s"  "                vtype: self
                (black)  >fg: self
                dup >r
                dup dup disassyORG @ - roidata +  ( at at ah )
                dup to dishere                    \ -> buffer data
                disassemble 2swap dup rot -       ( a n at' len  )
                dishere over                      ( a n at' len a len )
                disassyDatawidth max  bounds      ( a n at' len . . )
                ?do     1- dup 0<
                        if      s"   "
                        else    i c@ 2 (h.)
                        then          vtype: self
                loop    drop -rot                 ( at' a n )
                s"  "                 vtype: self
                0 pad c!
                r> _disname pad count vtype: self
                                      vtype: self
        loop    drop
                ;

: LastAddress   ( -- a )
\ last displayable target address in the text window
                addresses textheight 1- th @ ;

: makePCviewable ( -- )
\ Maintain disassyORG such that disassyPC stays within the viewable window
                disassyPC @  dup disassyORG @ u<
                if      disassyORG !    \ moved to before window
                        UploadBlock
                else    LastAddress - dup 0>=
                        if      16 - 0<
                                if      ( moved a short distance )
                                        addresses textheight 4 - th @
                                        disassyORG ! UploadBlock
                                else    ( moved a long way )
                                        disassyPC @ disassyORG !
                                        UploadBlock
                                then
                        else    drop
                        then
                then    ;

: yorg          ( row# -- pel )                 \ y offset for painting
                char-height *  TopMargin: self + ;

: validrow?     ( n -- f )                      \ this row used?
                'watch@ @ ;

:M RedLabels:   ( -- )
\ Redraw labels in the left margin
                SaveDC: dc                      \ save device context
                Handle: txtFont SetFont: dc     \ set the font to be used
                LeftMarginColor: [ self ]  SetBkColor: dc
                Red                      SetTextColor: dc
                textheight 0
        ?do     i validrow?                     \ is it watchable?
                if      leftgutter i yorg
                        i 'label labelwidth
                        false to ?win-error-enabled \ for NT
                        TextOut: dc ( x y a len )  \ !!!!!!!!!!!
                then
        loop
\ Draw the help line in the top margin
                TopMarginColor: [ self ]  SetBkColor: dc
                Gray                    SetTextColor: dc
                2 1 helptext TextOut: dc
                RestoreDC: dc
                ;M

: RefreshTitle  ;

variable outchar

:M RedValues:   ( -- )
\ Redraw values associated with the labels in the left margin
                SaveDC: dc                      \ save device context
                Handle: txtFont SetFont: dc     \ set the font to be used
                Black   SetTextColor: dc
                maxrow 1+ textheight min 0
        ?do     i validrow?                     \ is it watchable?
                if      editing?
                        if      WHITE
                        else    LeftMarginColor: self
                        then    SetBkColor: dc
                        labelwidth char-width * leftgutter +
                        i yorg
                        i 'value  @             ( . . val )
                        i 'datasize  c@
                        cellsize min 2* (h.)    ( . . a len )
                        i ed-row = editing? and \ does this have a cursor?
                        if    0 ?do     i ed-digit =
                                        if LTCYAN else WHITE then
                                        SetBkColor: dc    ( x y a )
                                        count outchar !
                                        >r 2dup outchar 1 TextOut: dc
                                        char-width under+ r>
                                loop    3drop
                        else    TextOut: dc     \ paint this value
                        then
                then
        loop    RestoreDC: dc
                RefreshTitle
                ;M

:M Reload:      ( -- )
\ reloads values from various sources via the configuration list
                textheight 0
                ?do     i validrow? 
                        if      i 'address @
                                i 'watch@ @
                                execute         \ load the value
                                i 'value !      \ save for painting
                        then
                loop    ;M

:M On_Paint:    ( -- )
                DisassembleWin
                SuspendCursor: self
                On_Paint: super
                winpause
                RedLabels: self      \ !!!!!!!!!!!!!
                RedValues: self
                RefreshTitle
                ;M

:M Refresh:     ( -- )
                hWnd
        if      UploadBlock
                DisassembleWin
                SuspendCursor: self
                all-set-pending
                Refresh: super
                hWnd
                if      get-dc
                        RedValues: self
                        release-dc
                then
        then    ;M

: LeftMouseCol  ( row -- digit# f )
                >r mousex leftgutter -
                char-width / labelwidth -       \ digit#
                dup 0 r@ 'datasize c@ 2* within \ T if valid digit#
                r> validrow? and ;

: re-fields     ( -- )
                reload: self
                refresh: self ;

: (re-fields)   ( -- )
\ invoke re-fields by faking a Ctrl-R keystroke
                'R' +k_control pushkey ;

: (refresh)     ( -- )
                'S' +k_control pushkey ;

: updatevalue   ( row -- )
\ store value to row n using storage word in watch table
                dup>r 'value @          \ get the value to store
                r@    'address @
                r> 'watch! @ execute    \ store it
                ;

:M Click:       ( -- )
\ process single mouse click
                MouseCR: self  nip              ( row )
                MouseViewable?: self
                if      addresses swap th @     \ target addr for this row
                        disassyBRK !            \ set new breakpoint
                        (re-fields)
                else    rowoffset -             \ remove row offset
                        dup LeftMouseCol        ( row digit# f )
                        if      to ed-digit
                                to ed-row
                                (re-fields)
                        else    2drop
                             -1 to ed-row
                        then
                then
                ;M

:M DblClick:    ( -- )
\ process double mouse click
                MouseCR: self  nip              ( row )
                MouseViewable?: self
                if      addresses swap th @     \ target addr for this row
                        disassyPC !             \ set new PC
                        (re-fields)
                else    dup LeftMouseCol nip    \ is it a valid data field?
                        if      dup 'value >r
                                getcatval r> !  \ get value from catalog
                                updatevalue
                                makePCviewable
                                (re-fields)
                        else    drop
                        then
                then
                ;M

: cursable?     ( -- f )                \ is the current row a data field?
                ed-row 0< 0=
                ed-row validrow? and ;

: lastdigit     ( -- n )                \ highest digit# in this field
                ed-row 'datasize c@ 2* 1- ;

: +ed-digit     ( n -- )
                ed-digit +
                0 max lastdigit min     \ limit to the editable field
                to ed-digit
                (refresh) ;

: +ed-row       ( n -- )
                ed-row >r               ( n | start )
                begin   dup +to ed-row
                        ed-row 0 textheight within 0=
                        cursable?  or
                until   drop r>         ( old )
                cursable?
                if      drop
                else    to ed-row       \ couldn't move further
                then
                (re-fields) ;

: cursleft      ( -- )          cursable? if -1 +ed-digit then ;
: cursright     ( -- )          cursable? if  1 +ed-digit then ;
: cursdown      ( -- )          cursable? if  1 +ed-row   then ;
: cursup        ( -- )          cursable? if -1 +ed-row   then ;

: flipediting   ( -- )
                editing? 0= to editing? 
                refresh: self ;

: placenibble   ( n -- )
\ place 4-bit n into the value at ed-row, ed-digit
                lastdigit ed-digit -    \ digits from left
                4 *  dup 0<             ( n dig# . )
                if      2drop
                else    dup >r lshift   ( n' )
                        15  r> lshift   ( n' mask )
                        ed-row 'value @ ( n' mask x )
                        swap invert and or
                        ed-row 'value !
                        cursright
                then    ;

: entervalue    ( -- )
\ store the edited value in this row to the target location
                ed-row validrow?
                if      ed-row updatevalue
                        0 to ed-digit            \ home cursor
                        cursdown
                then
                makePCviewable
                (re-fields) ;

: showhelp      ( -- )
                lobughelptext help: lobughelp ;

: do-steps      ( n -- )
\ execute n CPU instructions
                steppable?
        if      disassyPC @ ssaddr!     \ set the start address
                ['] ssstep catch        \ return 0 if not aborted
                if      noop
                else    disassyPC ! 
                        makePCviewable
                        (re-fields)
                then
        else    drop                    \ not steppable
        then    ;

: step-into     ( -- )
\ execute one CPU instruction
                1 do-steps ;

: animate       ( -- )
\ execute until any key is pressed or reached breakpoint
                steppable?
        if      begin   1
                        disassyPC @ ssaddr!     \ set the start address
                        ['] ssstep catch        \ return 0 if not aborted
                        if      noop
                        else    disassyPC !
                                makePCviewable
                                re-fields
                        then
                        disassyPC @ disassyBRK @ =
                        key? or
                until   _mkey drop
        then    ;

: run-to-brkpt  ( -- )
\ execute instructions until the breakpoint
                steppable?
        if      disassyPC  @ ssaddr!     \ set the start address
                disassyBRK @ ssbrkpt!
                ['] ssrun catch          \ return 0 if not aborted
             0= if      disassyBRK @ disassyPC !
                        makePCviewable (re-fields)
                then
        then    ;
                 

: getPCindex    ( -- index )
\ scan the address list for the PC's address, assumed to be within the
\ current disassembly range
                disassyPC @  textheight
        begin   1-                      ( a n )
                2dup addresses swap th @  \ address mismatch
                =   over 0= or            \ and count <> 0
        until   nip ;


: step-next     ( -- )
\ move the breakpoint to the next instruction after the PC's instruction
\ and then run to breakpoint
                getPCindex              \ index of current PC
                1+ dup textheight =     \ are we at the end?
                if      drop            \ move origin, re-disassemble
                        addresses textheight u2/ th @
                        disassyORG ! DisassembleWin
                        getPCindex 1+
                then                    ( idx' )
                addresses swap th @ disassyBRK !
                run-to-brkpt ;


:M ForceUpload: ( -- )
                disassyORG @ 1+ to ROIorg       \ mark as changed
                UploadBlock                     \ upload and redraw
                makePCviewable (re-fields)
                ;M


:M dokey:       ( c f -- c f' )
\ Perform key action and set f if this window has focus,
                have-focus? hWnd and
        if      drop true               \ set the "have-active" flag
                over upc
                case    8               of   cursleft           endof
                        k_down          of   cursdown           endof
                        k_up            of   cursup             endof
                        k_left          of   cursleft           endof
                        k_right         of   cursright          endof
                        'P'             of   Paint: self        endof
                        k_F1            of   showhelp           endof
                        k_F2            of   flipediting        endof
                        27              of   close: self        endof
                        13              of   entervalue         endof
                        'R'             of   ForceUpload: self  endof
                        k_F5            of   ForceUpload: self  endof
                        'I'             of   step-into          endof
                        bl              of   step-next          endof
                        'G'             of   run-to-brkpt       endof
                        'S'             of   step-next          endof
                        'N'             of   animate            endof
                        'C' +k_control  of   copy-to-clipboard  endof
                        'R' +k_control  of   re-fields          endof
                        'S' +k_control  of   refresh: self      endof
                        editing? cursable? and
                        if      16 digit
                                if      placenibble
                                else    drop
                                then    0
                        then 
                endcase
        then    ;M

:M org:         ( addr -- )
                dup disassyBRK !
                dup disassyPC !
                disassyORG !
                ;M

;object

: +LOWATCH      ( n xt@ xt! addr datasize a len -- )
                addwatch: lobug ;

: LOWATCH       ( n xt@ xt! addr datasize <name> -- )
                bl word count +lowatch ;


