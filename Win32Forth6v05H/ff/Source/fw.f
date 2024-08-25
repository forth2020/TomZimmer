((
        Firmware Studio
))
anew firmfactmark   time-reset
only forth also definitions
decimal
     0 constant oldstartup      \ Normally 0.  T if you want to compile FF then
                                \ use Win32forth's debugger to troubleshoot FF.
     1 value dlport0?           \ compile parallel port features that use DriverLinx driver
     1 value testmore           \ test snapshot displays more info
  true value haveTOF?           \ target has TOF unless otherwise specified
   214 value version            : kb 1024 * ;
 64 kb value maxtokens          \ max # of tokens
   256 value maxwatch           \ max # of items to watch
512 kb value imagesize          \ size in bytes of allocated image buffer
imagesize 2/ value datasize     \ size of data space image
     0 value hexoriginX         \ starting placement of hex dump windows
    80 value hexoriginY
    60 value hexstaggerX        \ staggering for default starting placement
    30 value hexstaggerY
hexoriginX hexstaggerX 0 * +  value hex0x       \ default start positions
hexoriginY hexstaggerY 0 * +  value hex0y
hexoriginX hexstaggerX 1 * +  value hex1x
hexoriginY hexstaggerY 1 * +  value hex1y
hexoriginX hexstaggerX 2 * +  value hex2x
hexoriginY hexstaggerY 2 * +  value hex2y
hexoriginX hexstaggerX 3 * +  value hex3x
hexoriginY hexstaggerY 3 * +  value hex3y
hexoriginX hexstaggerX 4 * +  value hex4x
hexoriginY hexstaggerY 4 * +  value hex4y
 4 kb value maxhexsize          \ max size of a hex dump window page
   32 value #families           \ # of CPU families allocated for
false value hilitecontext?      \ T if the first context is highlighted
    8 value maxorder            \ max # of vocabularies to list
    0 value codeorigin          \ base address of ROM/code image
    0 value dataorigin          \ base address of data image
   variable sync                \ T if need to sync image to target
    0 value syncstart           \ lowest image address since last sync
    0 value hardsync            \ hard lower limit for sync
  256 value SyncMsgThreshold    \ block size at which message pops up
    0 value codebuf             \ pointer to the code image buffer
    0 value databuf             \ pointer to the data image buffer
    0 value progdatabuf         \ pointer to the data list (for disassembler)
   32 value cellbits            \ # of bits per cell
    8 value charbits            \ # of bits per character
    8 value addrnibbles         \ # of nibbles per address
    0 value CurrentWindow
    0 value CurrentHBar
   32 value TarDataSize         \ block size of multi-byte target commo
 0x20 value tokenbreak          \ start of 2-byte tokens
 true value dynamic?            \ T = dynamic binding, F = static binding
    0 value catparam            \ parameter picked by token browser
 true value signed?             \ T if the stack display is signed
false value commonmem?          \ T if code space is the same as data space
 true value bigendian?          \ T if MSB is in low memory
    0 value disoption0          \ T if disassembler flips odd/even bytes
    0 value branchbytes         \ # of bytes in a control structure branch
false value turbo?              \ T if want to steal the UART from Windows
    0 value droppedchars        \ # of dropped commo bytes
create glosname ," FFGLOS.TXT"  \ glossary builder output filename
create glosnamh ," FFGLOS.HTM"  \ glossary builder output filename
create ffpath ," FF" 80 allot
    0 value ShowEveryline?      \ T = upload after each tokenized line
    0 value showme              \ T = dump info during compilation for debug
  250 value tcperiod            \ minimum msec between test-comm polls
    0 value clipboard           \ Local clipboard data
64 kb value clipboardsize
    0 value specaddr0           \ app specific special address for
    0 value specaddr1           \ code/data/reg/eeprom
    0 value specaddr2
    0 value specaddr3
    0 value specaddr4

: lobugscreen   ( -- w h )   80 64 ; \ size of low level debugger window
: tspcolor      ( -- color )  ltyellow ;  \ target sp display colors
: ts0color      ( -- color )  black ;
: depthcolor    ( -- color ) green ; \ target depth color
: stackcolor    ( -- color ) black ; \ target stack item color
: fn-dump       ( -- a n )   s" tokimage.bin" ; \ test dump file name
create &ff.cfg ," FF.CFG"

   defer   ConnectToTarget      \ Connect to target board
   defer   buildinterp          \ Builder interpreter
   defer   tokeninterp          \ Tokenizer interpreter
   defer   _tarinterp           \ Target interpreter
   defer   interp               \ inner interpreter action
   defer   ffinterpret          \ generic interpreter with stack redraw
   defer   testinterpret        \ Testing interpreter with stack redraw
   defer   IDEKEY
   defer   hostcon              \ host is console window
   defer   startlobug           \ start low level debug
   defer   loadlabels           \ load labels from target for lobug
   defer   tpush                \ target push and pop for watcher
   defer   tpop
   defer   snapshot             \ display snapshot in left margin
   defer   hostsnap             \   host
   defer   targetsnap           \   target
   defer   gohome               \ reset search order
   defer   gohost               \ back to host mode
   defer   _addwatch            \ add current token to watch list
   defer   <e+>   defer   <e->  \ set message colors for fcon
   defer   <m+>   defer   <m->
   defer   <ed+>  defer   <ed->

  variable bstate               \ builder I/C state

: charsize      ( -- n )        charbits 7 + 3 rshift ;  \ in bytes
: cellsize      ( -- n )        cellbits 7 + 3 rshift ;
: cellnibbles   ( -- n )        cellbits 3 + 2 rshift ;
: bounds>na    ( lo hi -- n a ) over - 1+ cellsize 0 swap um/mod nip swap ;

: snap=host     ( -- )          ['] hostsnap is snapshot ;
: snap=target   ( -- )          ['] targetsnap is snapshot ;

: forthinterp   \ inner part of _interpret copied from Win32forth kernel
                FIND ?DUP
        IF      STATE @ =
                IF      COMPILE,
                ELSE    EXECUTE ?STACK
                THEN
        ELSE    NUMBER NUMBER,
        THEN    ;

: interpret=forth ( -- )
\ use host Forth interpreter
                bstate off  snap=host
                ['] forthinterp  is interp
                ['] ffinterpret  is interpret ;

: interpret=build ( -- )
\ use the builder interpreter
                snap=host
                ['] buildinterp  is interp
                ['] ffinterpret  is interpret ;

: interpret=token ( -- )
\ use the tokenizer interpreter
                snap=host
                ['] tokeninterp  is interp
                ['] ffinterpret  is interpret ;

: interpret=target ( -- )
\ use target-testing, non-tokening interpreter
                snap=target
                testmore 0<> haveTOF? and to dynamic?
                ['] _tarinterp   is interp
                ['] ffinterpret  is interpret ;

: interpret=totest ( -- )
\ use tokenizing interpreter for target testing
                snap=target
                true to dynamic?
                ['] testinterpret is interpret ;

vocabulary home                 \ home Forth vocabulary
new-chain orgsaver
new-chain fwbegin
new-chain fwend

defer orgsave ( a len -- ) :noname cr type ; is orgsave

include bradstuf.g              \ some useful primitives
include commo.g                 \ communications to the target
include net.g                   \ TCP/IP connection to target
\ include RS485.g                 \ serial multidrop connection to target
include clastext.g              \ textwindow & consolewindow class
include clashelp.g              \ help window class
include clashex.g               \ hex dump class
include lobug.g                 \ low level debugger
include image.g                 \ image manager
include tbuild.g                \ builder
include ttoken.g                \ tokenizer
include tar.g                   \ target tester
include see.g                   \ token decompiler
include catalog.g               \ token catalog class & window
include watch.g                 \ watch window
include hibug.g                 \ high level debugger
include tsave.g                 \ token saver
include bed.g                   \ brad's line editor
include parallel.g              \ ROM emulator support
include scope.g                 \ Memory graphing
include dis8051.g               \ 8051 disassembler                 1
include disMSL16.g              \ MSL16 disassembler                2
include disMCF.g                \ Motorola ColdFire disassembler    3
include dis486.g                \ Intel 8086 disassember            4
include disMC.g                 \ Motorola M-core disassember       5
include disfp.g                 \ Brad's Forth CPU disassember      6
include disavr.g                \ Atmel AVR disassember             7
include dispic.g                \ Microchip PIC16 disassember       8

decimal false to sys-warning?

: homeorder     ( -- )
                only home definitions ;

warning on

 0 value fconx0                         \ main console start position
20 value fcony0
66 value fconx                          \ main console starting area
26 value fcony
17 value hex0z                          \ hex window heights
17 value hex1z
17 value hex2z
17 value hex3z
17 value hex4z

: "hexxy"       ( -- )                  \ save X,Y starting position (pels)
                fconx0 (.)  pad  place  s"  TO FCONX0 "  pad +place
                fcony0 (.)  pad +place  s"  TO FCONY0 "  pad +place
                pad count orgsave
                fconx (.)  pad  place   s"  TO FCONX "  pad +place
                fcony (.)  pad +place   s"  TO FCONY "  pad +place
                pad count orgsave
                hex0x (.)  pad  place   s"  TO HEX0X "   pad +place
                hex0y (.)  pad +place   s"  TO HEX0Y "   pad +place
                hex0z (.)  pad +place   s"  TO HEX0Z "   pad +place
                pad count orgsave
                hex1x (.)  pad  place   s"  TO HEX1X "   pad +place
                hex1y (.)  pad +place   s"  TO HEX1Y "   pad +place
                hex1z (.)  pad +place   s"  TO HEX1Z "   pad +place
                pad count orgsave
                hex2x (.)  pad  place   s"  TO HEX2X "   pad +place
                hex2y (.)  pad +place   s"  TO HEX2Y "   pad +place
                hex2z (.)  pad +place   s"  TO HEX2Z "   pad +place
                pad count orgsave
                hex3x (.)  pad  place   s"  TO HEX3X "   pad +place
                hex3y (.)  pad +place   s"  TO HEX3Y "   pad +place
                hex3z (.)  pad +place   s"  TO HEX3Z "   pad +place
                pad count orgsave
                hex4x (.)  pad  place   s"  TO HEX4X "   pad +place
                hex4y (.)  pad +place   s"  TO HEX4Y "   pad +place
                hex4z (.)  pad +place   s"  TO HEX4Z "   pad +place
                pad count orgsave ;

orgsaver chain-add "hexxy"

:object h1 <super hexwindow
:M DumpTitle:   ( -- a n )       s" CODE" ;M
:M StartPos:    ( -- x y )       hex0x hex0y ;M
:M SaveStartPos: ( x y -- )      to hex0y to hex0x ;M
:M read-from:   ( as ad n -- f ) read-code ;M
:M write-to:    ( as ad n -- f ) write-code ;M
:M startaddr:   ( -- a )         b.codeorigin @ ;M
:M Charwidth:   ( -- #bits )     charbits ;M
:M MaxStartSize: ( -- c r )      68 hex0z ;M
:M SaveTxtSize: ( c r -- )       nip to hex0z ;M
:M specaddr:    ( -- a )         specaddr0 ;M
;object

:object h2 <super hexwindow
:M DumpTitle:   ( -- a n )       s" DATA" ;M
:M StartPos:    ( -- x y )       hex1x hex1y ;M
:M SaveStartPos: ( x y -- )      to hex1y to hex1x ;M
:M read-from:   ( as ad n -- f ) read-data ;M
:M write-to:    ( as ad n -- f ) write-data ;M
:M startaddr:   ( -- a )         b.dataorigin @ ;M
:M Charwidth:   ( -- #bits )     charbits ;M
:M MaxStartSize: ( -- c r )      68 hex1z ;M
:M SaveTxtSize: ( c r -- )       nip to hex1z ;M
:M specaddr:    ( -- a )         specaddr1 ;M
;object

:object h3 <super hexwindow
:M DumpTitle:   ( -- a n )       s" REG" ;M
:M StartPos:    ( -- x y )       hex2x hex2y ;M
:M SaveStartPos: ( x y -- )      to hex2y to hex2x ;M
:M read-from:   ( as ad n -- f ) read-reg ;M
:M write-to:    ( as ad n -- f ) write-reg ;M
:M Charwidth:   ( -- #bits )     charbits ;M
:M MaxStartSize: ( -- c r )      68 hex2z ;M
:M SaveTxtSize: ( c r -- )       nip to hex2z ;M
:M specaddr:    ( -- a )         specaddr2 ;M
;object

:object h4 <super hexwindow
:M DumpTitle:   ( -- a n )       s" EEPROM" ;M
:M StartPos:    ( -- x y )       hex3x hex3y ;M
:M SaveStartPos: ( x y -- )      to hex3y to hex3x ;M
:M read-from:   ( as ad n -- f ) read-ee ;M
:M write-to:    ( as ad n -- f ) write-ee ;M
:M startaddr:   ( -- a )         b.ee0 @ ;M
:M MaxStartSize: ( -- c r )      68 hex3z ;M
:M SaveTxtSize: ( c r -- )       nip to hex3z ;M
:M specaddr:    ( -- a )         specaddr3 ;M
;object

:object himg <super hexwindow
:M DumpTitle:   ( -- a n )      s" IMAGE" ;M
:M StartPos:    ( -- x y )       hex4x hex4y ;M
:M SaveStartPos: ( x y -- )      to hex4y to hex4x ;M
:M Startaddr:   ( -- a )         codeorigin ;M
:M read-from:   ( as ad n -- f ) 2>r 'image 2r> move 0 ;M
:M write-to:    ( as ad n -- f ) >r 'image r> move 0 ;M
:M On_Done:     ( -- )          destroyhex: self  on_done: super ;M
:M addresswidth: ( -- n )       8  ;M
:M LeftMarginColor: ( -- color )    ltgreen ;M
:M TopMarginColor: ( -- color)      ltgray  ;M
:M Charwidth:   ( -- #bits )     charbits ;M
:M MaxStartSize: ( -- c r )      68 hex4z ;M
:M SaveTxtSize: ( c r -- )       nip to hex4z ;M
:M specaddr:    ( -- a )         specaddr4 ;M
;object

: h1click       click: h1 ;
: h2click       click: h2 ;
: h3click       click: h3 ;
: h4click       click: h4 ;
: himgclick     click: himg ;

: chipclick     click: lobug ;
: chip2click dblclick: lobug ;


false value hexcreated?         \ this is true once buffers have been created

: hexsize       ( begin end -- size )
                swap - maxhexsize min ;

:noname         ( -- )
\ Get size information from target board, then set up hex windows
                open-comm       \ search ports for the target board
                on-line?  hexcreated? 0=  and
        if      b.main0 @ b.mainsize @ cellsize * over +
                2dup SetPageRange: h1  hexsize createhex: h1
                b.io0 @   b.iosize @ cellsize *   over +
                2dup SetPageRange: h2  hexsize createhex: h2
                b.reg0 @  b.regsize @ cellsize *  over +
                2dup SetPageRange: h3  hexsize createhex: h3
                b.ee0 @   b.eesize @  cellsize *   over +
                2dup SetPageRange: h4  hexsize createhex: h4
                lobugscreen createbuffer: lobug
                true to hexcreated?
        then    ;  is ConnectToTarget

: starth1       communicable?                                ( main )
                if      ['] h1click  setclickfunc: h1
                        start: h1
                then ;

: starth2       communicable?                                ( I/O )
                if      ['] h2click  setclickfunc: h2
                        start: h2
                then ;

: starth3       communicable?                                ( regs )
                if      ['] h3click  setclickfunc: h3
                        start: h3
                then ;

: starth4       communicable?                                ( eeprom )
                if      ['] h4click  setclickfunc: h4
                        start: h4
                then ;

: starthimg     ['] himgclick  setclickfunc: himg
                codeorigin dup imagesize +
                2dup SetPageRange: himg  hexsize createhex: himg
                start: himg ;           \ host image hex window

: startwwin     communicable?
                if      OpenWatchWindow
                then    ;

: startgwin     communicable?
                if      Update: graf    \ load data from target board's memory
                        start: graf     \ display it
                then    ;

: startcwin     communicable?
                if      200 ms ( ||| ) OpenConsole
                then    ;

:noname         communicable?
                if      ['] chipclick     SetClickFunc: lobug
                        ['] chip2click SetDblClickFunc: lobug
                        ForceUpload: lobug
                        start: lobug
                        Reload: lobug
                then ;  is startlobug

: lbug          ( <name> -- )           \ start the low level debugger
                communicable?
        if      s" BIND@" _isdefined? findtoken
                if      t.xt tpush s" bind@" tar-eval tpop  \ get CFA from target
                else    t.cfa           \ get CFA from host
                then    nopfa
                org: lobug  startlobug
        then    ;

: tar?          communicable?   if  _tar?       then  ;
: image>boot0   communicable?   if  _bootload   then  ;
: image>boot1   communicable?   if  _bootload2  then  ;

: hexdestroy    ( -- )
     destroybuffer: lobug
        destroyhex: himg
        destroyhex: h4
        destroyhex: h3
        destroyhex: h2
        destroyhex: h1
        ;

\ fwend   chain-add hexdestroy

defer NewConFont        \ changes the font for all the windows
defer RefreshCon        \ refreshes the Forth console window

: blastable?    ( -- f )
                key? 0= ;

variable rstpending
variable refcontime
: refconMark    ( -- )   ms@ refcontime ! ;
: refconElapsed ( -- n ) ms@ refcontime @ - ;

: RefreshWatch  ( -- )
\ Update the watch windows as long as there isn't a keystroke pending
        begin   20 ms  on-line?
                if      blastable?  if  UploadViewable: h1  then
                        blastable?  if  UploadViewable: h2  then
                        blastable?  if  UploadViewable: h3  then
                        blastable?  if  UploadViewable: h4  then
                        blastable?  if  Watchem: watchwin   then
                        blastable?  if  Watch: conwatchwin  then
                        blastable?  if  Refresh: graf       then
                then    blastable?  if  UploadViewable: himg  then
                key?  dup if refconmark then
                refconElapsed tcperiod u>                    \ time for update?
                if      rstpending @
                        if      rstpending off
                                TarReset                     \ reset target board
                                false to on-line?
                        else    test-comm                    \ test comm link
                        then   refconmark
                then
                RefreshCon
        until   ;


:noname         ( -- c )
\ Evaluate incoming keystrokes until no window other than the console
\ is active
                begin   winpause
                        RefreshWatch
                        _MKEY false
                        dokey: h1
                        dokey: h2
                        dokey: h3
                        dokey: h4
                        dokey: himg
                        dokey: lobug
                        dokey: hibug
                        dokey: catwindow
                        dokey: watchwin
                        dokey: conwatchwin
                        dokey: graf
                while   drop
                repeat  ;       is IDEKEY

: .bldbounds    ( -- lo hi )    swap h. ." .. " h. ;

: .bldconfig    ( -- )
\ display various builder parameters
           <m+> cr ." Cell size:      " cellbits 2 .r ." bits"
                cr ." Character size: " charbits 2 .r ." bits"
                cr ." CPU type: Build=" CPUtype . ." See=" CPUfamily .
                   bigendian? if ." Big" else ." Little" then ."  endian"
                cr ." Builder's ROM code area: "  ROM-BOUNDS?  .bldbounds
                cr ." RAM code area:           "  CODE-BOUNDS? .bldbounds
                cr ." RAM data area:           "  DATA-BOUNDS? .bldbounds
                cr ." Binding table:           "
\ code grows upward, table grows up/down:
\ up:   |code>>>    <<<table|
\ down: |code>>>  table>>>  |
                   CODE-BOUNDS?
                   jsrsize 0<
                   if   nip ." ... 1=" dup jsrsize + h. ." 0=" h.
                   else 2drop TableLowEnd
                        ." 0=" dup h. ." 1=" jsrsize + h. ." ..."
                   then
                cr addrnibbles 4 * . ." bit debugger addresses"
           <m-> ;

: version"      ( -- a len )
                version 100 /mod swap 0         ( hi lo 0 )
                <# # # nip '.' hold #s #> ;

: $sendfile     ( <filename> -- )
\ send a bytecode file to the target and evaluate it.
                0 to tarprogress?
                communicable?
        if      openinput
                imagesize malloc to scrampad            \ temporary buffer
                scrampad imagesize infile read-file     \ read it in
                if      scrampad release
                        true abort" Error reading file"
                then    closeinput
                scrampad swap                           ( src len )
                tar-free + 2/ ialign    \ place in middle of freespace ( src len dest )
                <e+>
                ?dup
                if      dup tpush       \ set start address
                        swap
                        true to tarprogress?
                        write-code  if cr ." Download incomplete!" then
                        b.'EVAL=MEM @ bexecute
                        b.'EVALUATE @ bexecute
                        tpop ?dup
                        if      cr ." Evaluation error on target: " .
                        then
                else    2drop
                        cr ." Insufficient target dictionary space"
                then
                <e->
                scrampad release
        else    drop
        then    ;

: $sipprog      { node filename \ len intermediate verify -- }
\ send a binary file to the EEPROM on a SIP device
                0 to tarprogress?
                communicable?
        if      forceCORE forceSYSTEM   \ we'll need these in the search order
                cr ." Programming node " node .
                   ." boot EEPROM with " filename count type
                filename openinput
                imagesize malloc to scrampad            \ temporary buffer
                scrampad imagesize infile read-file     \ read it in
                if      scrampad release
                        true abort" Error reading file"
                then    closeinput
                to len
                s" DEADBUS ON BOOTIMAGE" tar-evaluate  \ a place to put the data
\ disable polling for faster loading
                tpop 16 + to intermediate
\ 16 bytes into bootimage is safe even if polling is enabled.
                imagesize malloc to verify
                true to tarprogress?
                cr ." image -> RAM(" intermediate (.) type ." ) "
                scrampad intermediate len write-data
                                if cr <e+> ." Download incomplete!" <e-> then
                node tpush s" X_NODE" tar-eval
                0 tpush s" X_WRSR" tar-eval \ remove write protection
                ." -> EEPROM(0) "
                intermediate tpush
                0 tpush
                len tpush       ( target: src dest len )
                100 ms  s" X_WRITE" tar-eval     \ write it
                12 tpush s" X_WRSR" tar-eval  100 ms  \ re-protect
                \ intermediate cellsize + to intermediate
                cr ." EEPROM(0) --> RAM(" intermediate (.) type ." ) "
                0 tpush
                intermediate tpush
                len tpush
                s" X_READ" tar-eval     \ read back
                true to tarprogress?
                ." -> verify  "
                intermediate verify len read-data
                scrampad len verify len compare
                                if cr <e+> ." Verify error" <e->
                cr scrampad 64 dump
                cr verify 64 dump
                                else cr ." Verified OK!"
                                then
                verify release
                scrampad release
        then    ;

: TargetReset   ( -- )
\ resut button pressed
                communicable?
        if      rstpending on
        then    ;



\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\       FW Factory about dialog, copied from the Forth About Dialog
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

:Object about-FW-dialog       <SUPER dialog

IDD_ABOUT_FORTH forthdlg find-dialog-id constant template

create about-edit-msg
         z," Firmware Studio version " version" +z",
        +z," \n\nTiny Open Firmware Development Environment\n"
        +z," For Microcontrollers and Microprocessors\n"
        -null, here 0 c, align about-edit-msg - constant about-edit-len

create about-edit-msg2
         z," Public Domain, No Warrantees Whatsoever.\n"
        +z," Need help? Contact author: Brad Eckert  brad@tinyboot.com\n\n"
        +z," or visit http://www.tinyboot.com\n\n"
        +z," Press F1 for help in any window.\n"
        -null, here 0 c, align about-edit-msg2 - constant about-edit-len2

:M On_Init:     ( hWnd-focus -- f )
                about-edit-msg  about-edit-len
                IDD_ABOUT_TEXT  SetDlgItemText: self
                about-edit-msg2 about-edit-len2
                IDD_ABOUT_TEXT2 SetDlgItemText: self
                1 ;M

:M GetTemplate: ( -- template )
                template
                ;M

:M On_Command:  ( hCtrl code ID -- f1 )
                case
                IDCANCEL of     0 end-dialog    endof
                                false swap ( default result )
                endcase ;M

;Object

: about-factory ( -- )
                CurrentWindow  Start: about-FW-dialog  drop ;

defer fcon>clipboard
defer clipboard>fcon
defer open-text
defer send-bytecode
defer send-SIP
defer image>binfile
defer image>hexfile
defer image>motfile
defer source>image
defer cfgsave
defer comm>file

: ~~testmore    <m+> testmore 0= dup to testmore cr
                if ." Verbose" else ." Quiet" then ."  target snapshot" <m-> ;

: ~~ShowEveryline?  <m+> ShowEveryline? 0= dup to ShowEveryline? cr ." Upload "
                if ." a line at a time" else ." in blocks" then <m-> ;

\ keyboard macros: F9..F12 are macro keys
create macdata 512 chars allot      \ four 127-byte strings
       macdata 512 erase            \ initially empty
: 'macro        ( n -- addr ) 3 and 128 * chars macdata + ;
: execmacro     ( n -- )      'macro count dup if .command else 2drop then ;
: defmacro      ( n -<string>- -- ) 9 - 'macro 0 word count rot place ;

\ Clipboard stuff -------------------------------------------------------------

: init-clip     ( -- ) clipboardsize malloc to clipboard \ local clipboard
                       clipboard off ;
: free-clip     ( -- ) clipboard release ;
init-mem        chain-add init-clip
free-mem        chain-add free-clip

: clipinterpret
\ Interpret local clipboard text: evaluate CRLF delimited lines
                clipboard @
           IF   clipboard lcount
                BEGIN   dup
                WHILE   2dup 13 scan dup>r swap >r -    ( a len | n' a' )
                        evaluate
                        r> r> 13 skip
                        over c@ 10 = if 1 /string then
                REPEAT  2drop
          ELSE  blip
          THEN  ;

: open-html     ( addr len -- ) { \ name -- }  \ open an HTML file
                256 localalloc to name
                s" file:/"   name  place
                ffpath count name +place
                s" /"        name +place
                             name +place
                name count conhndl "Web-Link ;

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ \\\\\\\\                   IDE                         \\\\\\\\\\\\
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

ToolBar Forth-Tool-Bar "FIRMFACT.BMP"

        08 HSpace
     6 PictureButton  >r >r interpret=forth  \ host:
                      gohome
                      r> r> ;
        ButtonInfo"  Host & Home Search Order "
     3 PictureButton  >r >r interpret=forth  \ host:
                      gohost
                      r> r> ;
        ButtonInfo"  Host Session"
\     8 PictureButton  image>target  ;       \ upload image to target
\        ButtonInfo"  Upload to Target "
    11 PictureButton  >r >r interpret=forth       \ target:
                      s" bi" .command
                      r> r> ;
        ButtonInfo"  Builder Mode "
\    23 PictureButton  >r >r interpret=forth
\                      gohome
\                      r> r>
\                      close-comm sync off RefreshCon    \ probably want comm off
\                      s" ok" .command
\                      ;
\        ButtonInfo"  Load Current File "
    15 PictureButton  >r >r interpret=forth       \ target:
                      haveTOF? if s" fi" else s" te" then .command
                      r> r> ;
        ButtonInfo"  Target Session "

        16 HSpace

     9 PictureButton OpenCatWindow       ;  \ see
        ButtonInfo"  Token Browser "
     0 PictureButton starthimg           ;  \ dump image
        ButtonInfo"  Hex Dump of Image "
    20 PictureButton open-hibug          ;  \ bug
        ButtonInfo"  High Level Debug "
    10 PictureButton startlobug          ;  \ view chip
        ButtonInfo"  Low Level Debug "

        16 HSpace

    22 PictureButton startwwin           ;  \ watch window
        ButtonInfo"  Watch Target "
    13 PictureButton startcwin           ;  \ watch virtual console
        ButtonInfo"  Watch Console "
    16 PictureButton starth1             ;
        ButtonInfo"  Edit Code Memory "
    17 PictureButton starth2             ;
        ButtonInfo"  Edit Data Memory "
    18 PictureButton starth3             ;
        ButtonInfo"  Edit Register Space "
    19 PictureButton starth4             ;
        ButtonInfo"  Edit Non-volatile Memory "
    26 PictureButton startgwin           ;  \ virtual scope
        ButtonInfo"  Graphic Data "

        16 HSpace

    14 PictureButton communicable? drop RefreshCon ;
        ButtonInfo"  Connect to Target Board "
    21 PictureButton close-comm sync off RefreshCon ;
        ButtonInfo"  Disconnect from Target "
    24 PictureButton TargetReset ;
        ButtonInfo"  Reset Target Board"
        16 HSpace

     5 PictureButton  about-factory      ;
        ButtonInfo"  About Firmware Studio "
    25 PictureButton  k_F6 pushkey ;
        ButtonInfo"  Redo Last Line "
        8 HSpace
ENDBAR

POPUPBAR Forth-Popup-bar

    POPUP " "
        MENUITEM        "Open Text File    \tCtrl-O"  open-text  ;
        MENUITEM        "Load Source File  \tCtrl-L"
                        >r >r >r >r source>image snapshot r> r> r> r> ;
        MENUSEPARATOR
        MENUITEM        "Browse Tokens..."  OpenCatWindow ;
        MENUSEPARATOR
        MENUITEM        "Test mode"  >r >r interpret=forth
                         haveTOF? if s" fi" else s" te" then .command
                         r> r> ;
        MENUITEM        "Build mode"  >r >r interpret=forth
                         s" bi" .command  r> r> ;
        MENUITEM        "Rebuild (BI OK)"  >r >r interpret=forth
                         s" bi ok" .command  r> r> ;
        MENUSEPARATOR
        MENUITEM        "Exit"             bye   ;
ENDBAR

MENUBAR Forth-Menu-bar

    POPUP "&File"
        MENUITEM        "&Open Text File    \tCtrl-O"  open-text  ;
        MENUITEM        "&Load Source File  \tCtrl-L"  source>image  ;
        MENUITEM        "&Upload Bcode File \tCtrl-U"  send-bytecode  ;
        MENUITEM        "&Program SIP 0 EEPROM"        send-SIP ;
        MENUSEPARATOR
        MENUITEM        "Save as &Binary "    image>binfile  ;
        MENUITEM        "Save as Intel &Hex"  image>hexfile  ;
        MENUITEM        "Save as Motorola &S" image>motfile  ;
        MENUSEPARATOR
        MENUITEM        "&Copy console      \tCtrl-C" fcon>clipboard ;
        MENUITEM        "Make Configuration"  cfgsave ;
        MENUSEPARATOR
        MENUITEM        "E&xit              \tAlt-F4"  bye  ;

    POPUP "&View"
        MENUITEM        "&Browse Tokens" OpenCatWindow  ;
        MENUITEM        "&Image (host)" starthimg ;
        MENUSEPARATOR
        MENUITEM        "&Watch window"  startwwin ;
        MENUITEM        "&Virtual console" startcwin ;
        MENUITEM        "&Code Memory"  starth1 ;
        MENUITEM        "&Data Memory"  starth2 ;
        MENUITEM        "&Register Memory"  starth3 ;
        MENUITEM        "&NonVol Memory" starth4 ;

    POPUP "&Host"
        MENUITEM        "&Signed/unsigned \tCtrl-S" 'S' +k_control pushkey ;
        MENUITEM        "&Decimal radix   \tCtrl-D" 'D' +k_control pushkey ;
        MENUITEM        "He&x radix       \tCtrl-X" 'X' +k_control pushkey ;
        MENUSEPARATOR
        MENUITEM        "&Redo Last Line  \tF6"     k_F6 pushkey ;
        MENUITEM        "&Host mode       \tF7"     k_F7 pushkey ;
        MENUITEM        "&Home order      \tF8"     k_F8 pushkey ;
        MENUITEM        "&Evaluation speed" 'X' ~~ShowEveryline? ;

    POPUP "&Target"
        MENUITEM        "&Serial port connection" commo=serial ;
        MENUITEM        "TCP/&IP connection" commo=ip ;
        MENUITEM        "&Multidrop connection" commo=multidrop ;
dlport0? [if]
        MENUITEM        "&AVR SPI connection" commo=avr ;
       [then]
        MENUITEM        "&Connect to Target Board" communicable? drop RefreshCon ;
        MENUITEM        "&Disconnect from Target " close-comm sync off RefreshCon ;
        MENUITEM        "&Reset Target Board"  'R' +k_control pushkey ;
        MENUSEPARATOR
        MENUITEM        "&Target Characteristics" tar?  ;
        MENUITEM        "Capture incoming serial" comm>file ;
        MENUITEM        "&High Level Debug" open-hibug ;
        MENUITEM        "&Low Level Debug" startlobug ;
        MENUITEM        "&Verbose/quiet snapshot"  ~~testmore ;

    POPUP "F&ont"
        MENUITEM        "&System"       8 16 s" FixedSys"       NewConFont ;
        MENUITEM        "&Courier"      8 12 s" Courier"        NewConFont ;
        MENUITEM       "Terminal &Med" 10 18 s" Terminal"       NewConFont ;
        MENUITEM        "&Lucida 15"    9 15 s" Lucida Console" NewConFont ;
        MENUITEM        "Courier &Big"  8 20 s" Courier"        NewConFont ;

    POPUP "&Upload"
        MENUITEM        "Load to &Pre-Boot area"    image>boot0 ;
        MENUITEM        "Load to Post-&Boot area"   image>boot1 ;
        MENUITEM        "Load &ROM emulator"    loadROM ;
        MENUITEM        "&Toggle LPT port#"     toggleport ;
\        MENUITEM        "&Download from memory"  ;

    POPUP "&Info"
        MENUITEM        "&About Firmware Studio" about-factory ;
        MENUITEM        "&Builder Configuration" .bldconfig  ;
        MENUITEM        "&Vocabularies" .vocs  ;
        MENUSEPARATOR
        MENUITEM    "FS User's Manual"   s" documentation/man/index.html" open-html ;
        MENUITEM    "ANS Forth Standard" s" documentation/ansforth/dpans.htm" open-html ;
        SUBMENU     "Forth Web pages"
                MENUITEM    "Tinyboot" s" http://www.tinyboot.com" conhndl "Web-Link ;
                MENUITEM    "Forth Interest Group" s" http://www.forth.org" conhndl "Web-Link ;
                ENDSUBMENU
ENDBAR

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ \\\\\\\\\\\\\\      Forth Console Window        \\\\\\\\\\\\\\\\\\\
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

:object fcon <super consolewindow

:M MarginWidth: ( -- n )   12 ;M

:M StartPos:    ( -- x y )       fconx0 fcony0 ;M
:M SaveStartPos: ( x y -- )      to fcony0 to fconx0 ;M

:M MaxStartSize: ( -- c r )      fconx fcony ;M

:M GetClip:     ( -- ior ) { \ gblhndl gblptr tlen tadr -- }   \ Get clipboard data
                GetHandle: self  call OpenClipboard
        if      CF_TEXT call GetClipboardData ?dup  \ handle of data
                if      to gblhndl
                        gblhndl call GlobalLock abs>rel to gblptr       \ lock memory
                        \ extract length from 0 delimited string
                        gblptr 4000000 2dup 0 scan nip - to tlen to tadr
                        tlen clipboard !        \ use CLIPBOARD LCOUNT
                        tadr clipboard cell+ tlen move
                        0                       \ success
                        gblhndl call GlobalUnlock drop  \ unlock it, done
                else    2                       \ couldn't open handle
                then
        else    1                               \ couldn't open the clipboard
        then    call CloseClipboard ?win-error ;M

:M On_SetFocus: ( h m w l -- )
                On_SetFocus: super
                hostcon ;M                      \ direct I/O to this window

:M Classinit:   ( -- )
                ClassInit: super                \ init super class
                self to CurrentWindow           \ make myself the cur window
                Forth-Menu-Bar  to CurrentMenu  \ set the menubar
                Forth-Popup-Bar to CurrentPopup
                Forth-Tool-Bar  to CurrentHBar
                ;M

:M LoadMenu:    ( -- hmenu )
                MenuHandle: CurrentMenu 0=      \ load my menubar
                if      LoadMenu:   CurrentMenu
                then    MenuHandle: CurrentMenu
                ;M

:M WindowHasMenu: ( -- )                        \ yes window has a menubar
                TRUE
                ;M

:M On_Init:     ( -- )          \ initialize the class
                On_Init: super                  \ first init super class
                1    SetId: CurrentHBar         \ then the next child window
                self Start: CurrentHBar         \ then startup toolbar window
                self Start: CurrentPopup        \ startup the right mouse popup
                ;M                              \ menu

:M On_Done:     ( h m w l -- res )
                fwend do-chain                  \ wrap things up
                CloseMenu: CurrentMenu          \ discard the menubar
                0 call PostQuitMessage drop     \ terminate application
                On_Done: super                  \ cleanup the super class
                0 ;M

:M On_Size:     ( h m w -- )                  \ handle resize message
                On_size: super
                0 0 StartSize: CurrentHBar  Move: CurrentHBar
                ;M

:M TopMargin:   ( -- height )
                StartSize: CurrentHBar nip 1+ ;M

int titlebuf

:M CreateBuffer: ( -- )
                CreateBuffer: super
                MAXCOUNTED malloc to titlebuf
                ;M

:M DestroyBuffer: ( -- )
                titlebuf release
                DestroyBuffer: super
                ;M

variable prevonline     \ previous on-line status

:M Refresh:     ( -- )
\ refresh the changed text in the window, then update the title if changed
                Refresh: super
                s" Firmware Studio "             temp$  place
\                version s>d <# # # '.' hold #s #> temp$ +place
                s" ["                             temp$ +place
                cur-file count                    temp$ +place
                s" ] "                            temp$ +place
                connected?
        if      on-line?
                if      s" CPU="                  temp$ +place
                        CPUfamily  CPUsubfamily
                        CPUID                     temp$ +place
                        s"   "                    temp$ +place
                        comsourcename   \ append source name to PAD
                else    s" off-line"              temp$ +place
                then
\                on-line? prevonline @ 0= and if snapshot then
\                on-line? prevonline !   \ refresh display if target woke up
        else    cmsg-nc         \ "not connected"
                prevonline off
        then
                titlebuf count temp$ count compare
                if      temp$ titlebuf over c@ char+ move
                        temp$ count SetTitle: CurrentWindow
                then    ;M

also lineedit
:M curscolor:   ( -- n )        insert? if -1 else 0xEE then ;M
previous

;object


:noname         4dup NewFont: fcon      \ select new font
                4dup NewFont: h1
                4dup NewFont: h2
                4dup NewFont: h3
                4dup NewFont: h4
                4dup NewFont: himg
                4dup NewFont: CatWindow
                4dup NewFont: watchwin
                4dup NewFont: conwatchwin
                4dup NewFont: hibug
                     NewFont: lobug
                ;                       is NewConFont

:noname         Refresh: fcon ;         is RefreshCon

:noname         CopyClip: fcon ;        is fcon>clipboard

:noname         GetClip: fcon if blip then ; is clipboard>fcon


\ ------------------------------------------------------------------------
\ Special line editor actions

FileOpenDialog Fialog "Edit File" "Firmware Studio|*.FF;*.F?;*.F??|All Files (*.*)|*.*|"

:noname         ( -- )
                GetHandle: fcon  Start: Fialog dup c@
                if      0 swap $edit
                else    drop
                then    ;               is open-text

FileOpenDialog Sialog "Send Bytecode File" "ByteCode Files (*.BC)|*.BC|All Files (*.*)|*.*|"

:noname         ( -- )
                GetHandle: fcon  Start: Sialog dup c@
                if      $sendfile
                else    drop
                then    ;               is send-bytecode

FileOpenDialog Bialog "Program SIP Boot File" "SIP Boot Files (*.SB)|*.SB|All Files (*.*)|*.*|"

:noname         ( -- )
                GetHandle: fcon  Start: Bialog dup c@
                if      0 swap $sipprog
                else    drop
                then    ;               is send-SIP

: .hot          ( -- )          \ display editor hot keys
        <m+>
 cr ." ^C  Copy console to clipboard       ^S  Toggle signed/unsigned"
 cr ." ^L  fLoad a source file             ^D  Decimal display"
 cr ." ^O  Open a source file              ^X  heX display"
 cr ." ^U  Upload bytecode file to target  ^T  host token# = target token#"
 cr ." ^B  Skip to next double letter      ^V  interpret clipboard"
 cr ." F4  Redo clipboard interpret        F6  Redo last command"
 cr ." F7  Host mode, same order           F8  Host mode root (home)"
 cr ." F9..F12 execute macro string (defined using n DefMacro [text]) n=9..12"
        <m-> ;

also lineedit
:noname         ( c -- c' )
\ special lineeditor commands, note stack depth of +3 wrt main loop
        dup case
        'C'  +k_control  of  fcon>clipboard                     endof
        'L'  +k_control  of  source>image                       endof
        'O'  +k_control  of  open-text                          endof
        'R'  +k_control  of  TargetReset                        endof
        'T'  +k_control  of  communicable? if maxtartoken to-token#
                                     >r >r >r snapshot r> r> r> then endof
        'U'  +k_control  of  send-bytecode                      endof
        'S'  +k_control  of  signed? 0= to signed?
                                     >r >r >r snapshot r> r> r> endof
        'D'  +k_control  of  decimal >r >r >r snapshot r> r> r> endof
        'X'  +k_control  of  hex     >r >r >r snapshot r> r> r> endof
        'V'  +k_control  of  >r >r >r  clipboard>fcon clipinterpret
                             r> r> r> endof
        k_F1             of  .hot  endof
        k_F4             of  >r >r >r  clipinterpret   r> r> r> endof
        k_F7             of  >r >r >r interpret=forth gohost r> r> r> endof
        k_F8             of  >r >r >r interpret=forth gohome r> r> r> endof
        k_F9             of  >r >r >r  0 execmacro  r> r> r> endof
        k_F10            of  >r >r >r  1 execmacro  r> r> r> endof
        k_F11            of  >r >r >r  2 execmacro  r> r> r> endof
        k_F12            of  >r >r >r  3 execmacro  r> r> r> endof
        endcase ;                       is SpecialEd
previous


\ ------------------------------------------------------------------------
\ File loads/saves

FileOpenDialog LoadDialog "Load File" "Firmware Studio (*.FF)|*.FF|All Files (*.*)|*.*|"
FileNewDialog BinDialog "Save Binary" "Binary Files (*.BIN)|*.BIN|All Files (*.*)|*.*|"
FileNewDialog HexDialog "Save Hex" "Hex Files (*.HEX)|*.HEX|All Files (*.*)|*.*|"
FileNewDialog MotDialog "Save S-record" "S-rec Files (*.S)|*.S|All Files (*.*)|*.*|"
FileNewDialog CfgDialog "Save Configuration" "CFG files|*.CFG|"
FileNewDialog ComDialog "Save Comm Data" "LOG Files (*.LOG)|*.LOG|All Files (*.*)|*.*|"

:noname         ( -- )
                GetHandle: fcon  Start: LoadDialog dup c@
                if  s" FF"  def-ext $fload drop else  drop  then
                ; is source>image

:noname         ( -- )
                GetHandle: fcon  Start: BinDialog dup c@
                if  s" BIN" def-ext $bsave  else  drop  then
                ; is image>binfile

:noname         ( -- )
                GetHandle: fcon  Start: HexDialog dup c@
                if  s" HEX" def-ext $hsave  else  drop  then
                ; is image>hexfile

:noname         ( -- )
                GetHandle: fcon  Start: MotDialog dup c@
                if  s" S"   def-ext $ssave  else  drop  then
                ; is image>motfile

:noname         ( -- )
                GetHandle: fcon  Start: ComDialog dup c@
                if  s" LOG" def-ext $comlog  else  drop  then
                ; is comm>file

: $cfgsave      ( a -- )
\ save current configuration
                decimal
                openoutput
                s" {{ DECIMAL }}" orgsave
                s" {{ ALSO FORTH }}" orgsave
                orgsaver do-chain
                s" {{ PREVIOUS }}" orgsave
                closeoutput
                ;

:noname         ( -- )
                GetHandle: fcon  Start: CfgDialog dup c@
                if      $cfgsave
                else    drop
                then    ;       is cfgsave


\ ------------------------------------------------------------------------
\ Left Margin Drawing

: .vocab        ( row voc -- )
                voc>vcfa >NAME NFA-COUNT
                MargType: fcon ;

: showbase      ( -- )
                s"  "             pad +place
                s" ??B.....o. .....H*" drop
                base @ 17 min + 1 pad +place  ;

: showvocs      ( -- )
\ display vocabulary order in the left margin
                green SetMargColor: fcon
                CURRENT @ .vocab
                CONTEXT #VOCS maxorder 1- min 0          \ context
                ?DO     i 0=  hilitecontext? and
                        if      gray
                        else    blue
                        then    SetMargColor: fcon
                        DUP @ ?DUP   \ order
                        IF      .vocab
                        THEN    CELL+
                LOOP    DROP ;

: (hostsnap)    ( -- )
                BlankMargin: fcon
                black          SetMargColor: fcon
                s" --- HOST ---"   MargType: fcon
                showvocs
                red            SetMargColor: fcon
                base @ >r hex
                s" Here="    pad  place
                here@ (u.)   pad +place
                pad count          MargType: fcon
                s" Tok#="    pad  place
                token# (u.)  pad +place
                pad count          MargType: fcon
                r> base !
                green          SetMargColor: fcon
                s" Depth="   pad  place
                depth (.d)   pad +place
                showbase
                pad count          MargType: fcon
                black          SetMargColor: fcon
                depth
                MaxMargRow: fcon    MargRow: fcon
                - min 0max  0
                ?do     i pick signed? if (.) else (u.) then  MargType: fcon
                loop
                Refresh: fcon ;

:noname         ( -- )
\ display important host parameters in the left margin
                source-id 0=
        if      (hostsnap)
        then    ; is hostsnap


: con-emit      ( c -- )        emit: fcon  curse: fcon  makeviewable: fcon ;
: con-type      ( a n -- )      type: fcon  curse: fcon  makeviewable: fcon ;
: con-cr        ( -- )          cr: fcon  curse: fcon ;
: con-cls       ( -- )          cls: fcon  curse: fcon ;
: con-gotoxy    ( c r -- )      at-xy: fcon  curse: fcon ;
: con-getxy     ( -- c r )      at-xy?: fcon ;
: con-getcr     ( -- maxc maxr ) getcolrow: fcon ;
: con-col       ( c -- )        at-xy?: fcon  nip con-gotoxy ;
: con-host      ( -- )          SetActiveWindow: fcon ;

: ConCRTAB      ( -- )
                cr: fcon
                tabing? 0= ?exit
                first-line?
                if      left-margin indent + spaces
                        false to first-line?
                else    left-margin spaces
                        tab-margin spaces
                then    ;


:noname         ( -- )
\ route Forth's defered words to our console window
                ['] con-emit   IS EMIT
                ['] con-type   IS TYPE
                ['] con-cls    IS CLS
                ['] con-gotoxy IS GOTOXY
                ['] con-getxy  IS GETXY
                ['] con-getcr  IS GETCOLROW
                ['] con-col    IS COL
                ['] concrtab   IS CR
                ['] con-host   IS CONSOLE
                ['] IDEKEY     IS KEY
[ also lineedit ]
                ['] _baccept   IS ACCEPT
[ previous ]
                ;              is hostcon

variable tc-param

: (t.h)         addrnibbles (h.) ;
: (t.s)         signed? if sext (.) else (u.) then ;

:noname         ( -- )
\ display important target parameters in the left margin
                source-id 0=
        if      communicable?
          if    BlankMargin: fcon
                ltred      SetMargColor: fcon
                s" ** TARGET **" MargType: fcon
                showvocs
                testmore haveTOF? and
                if      red            SetMargColor: fcon
                        base @ >r hex
                        s" Here="      pad  place
                        tarhere (u.)   pad +place
                        pad count          MargType: fcon
                        s" Tok#="    pad  place
                        token# (u.)  pad +place
                        pad count          MargType: fcon
                        s" Rel#="    pad  place
                        maxtartoken (u.) pad +place
                        pad count          MargType: fcon
                        r> base !
                then
                gray       SetMargColor: fcon
                dynamic?
                if      s" dynamic"
                else    s" static "
                then        MargType: fcon
                depthcolor SetMargColor: fcon
                s" sDepth="     pad  place
                tdepth (.d)     pad +place
                showbase
                pad count   MargType: fcon
                stackcolor    SetMargColor: fcon
                tdepth 0max 10 min  0
                ?do     i tpick (t.s) MargType: fcon
                loop
                depthcolor SetMargColor: fcon
                s" rDepth="     pad  place
                tdepthr (.d)    pad +place
                showbase
                pad count   MargType: fcon
                stackcolor SetMargColor: fcon
                tdepthr 0max 10 min  0
                ?do     i tpickr (t.s) MargType: fcon
                loop
                Refresh: fcon
          then
        then    ; is targetsnap

:noname         ( -- )                  \ generic interpret
                BEGIN   BL WORD DUP C@
                WHILE   SAVE-SRC
                        interp          \ this changes for different modes
                        ?UNSAVE-SRC
                REPEAT DROP
                snapshot ;  is ffinterpret

variable lasthere

: dumpimage     ( asrc n -- )
                >r codebuf + r>
                fn-dump w/o create-file
                if      3drop           \ couldn't make dump file, do nothing
                else    dup>r write-file abort" error writing dump file"
                        r> close-file drop
                then    ;

\ tokenizer / tester progress window -----------------------------------------
256 value tbksize
variable  testpopup?
create toktitle 32 allot

200 value tprogx0
280 value tprogy0

: "commtxy"      ( -- )                  \ save X,Y starting position (pels)
                tprogx0 (.)        pad  place
                s"  TO TPROGX0 "   pad +place
                tprogy0 (.)        pad +place
                s"  TO TPROGY0 "   pad +place
                pad count orgsave ;
orgsaver chain-add "commtxy"

:Object TestProgress <SUPER MSGWINDOW

:M WindowTitle: ( -- Zstring )
                Z" Tokenizing & Sending"
                ;M

create propad 20 allot

:M Update:      ( -- )
                s" Line "        propad  place
                loadline @ (.d)  propad +place
                propad count MessageText: self
                winpause                \ let windows message loop breathe
                originx to tprogx0       \ save last known window position
                originy to tprogy0
                refresh: self
                ;M

:M StartPos:    ( -- x y )
                tprogx0 tprogy0 ;M

;OBJECT


: tokflush      ( -- )
\ flush a block of tokenized code to the evaluator
                0 to tarprogress?
                lasthere @ dup ?codebounds
                ihere@ over -           ( asrc n )
                ?dup
                if      source-id
                        testpopup? @ 0= and
                        if      testpopup? on
                                true ontop: testprogress
                                s" Line 1    " MessageText: testprogress
                                start: testprogress
                        then
                        0xFF ic, 1+     \ place end marker
                        2dup dumpimage  \ test output
                        tar-free + 2/ ialign    \ place in middle of freespace
                        ?dup
                        if      dup tpush       \ set start address
                                swap download   \ send to target
                                b.'EVAL=MEM @ bexecute
                                b.'<EVALDROP> @ bexecute
\                        s" EVAL=MEM <EVAL EVAL EVAL> DROP" tar-evaluate
                        else    2drop <e+>
                                cr ." Insufficient target dictionary space" <e->
                        then
                        update: testprogress
                else    drop            \ nothing to evaluate
                then
                lasthere @ ihere! ;     \ restart tokenizing here

: closetestprog ( -- )
                testpopup? @
                if      testpopup? off  blip
                        close: testprogress
                then    ;

:noname         ( -- )
\ tokenizing target test: tokenize code, upload to image, evaluate
\ this is invoked once for each line of a file or each command line
                source-id 0=
        if      testpopup? off
                ihere@ lasthere !
                communicable? 0= if exit then  \ make sure we're on line
                token# maxtartoken <           \ current token# is in use?
                if      maxtartoken to-token#  \ yes, move to first unused token#
                then
        then    ROM  bstate off
                BEGIN  BL WORD DUP C@          \ get next word until end of line
                WHILE  SAVE-SRC tokeninterp
                       ?UNSAVE-SRC
                REPEAT DROP  PFA-local off
                ihere@ lasthere @ - tbksize >  \ upload if we've accumulated so much code
                ShowEveryline? source-id and or \ or we want to go a line at a time
                bstate @ 0= and if tokflush then \ but only if not in compile mode
                on-line? 0=
        if      closetestprog true abort" Target went off-line"
        then    source-id 0=
        if      tokflush closetestprog
                snapshot
        then    ; is testinterpret

:noname         ( -- )
                false to hilitecontext?
                homeorder
                interpret=forth
                (hostsnap)
                ; is gohome

:noname         ( -- )
                false to hilitecontext?
                previous
                interpret=forth
                (hostsnap)
                ; is gohost

: start-IDE     ( -- )
                fwbegin do-chain
                init-mem do-chain
                0 to on-line?
                80 512  CreateBuffer: fcon
                               start: fcon
                              hide-console
                               paint: fcon
                0 0            at-xy: fcon
                homeorder
                hostcon interpret=forth
                (hostsnap)
                ;

include home.g                  \ aliases for the HOME wordlist
false to sys-warning?

' NOOP IS STACK-CHECK

\ ---------- conversion to number ----------

initialization-chain chain-add start-IDE        \ startup stuff

new-chain      mynumber-c \ our own number evaluation chain
mynumber-c chain-add new-number?
mynumber-c chain-add 0xNUMBER?

: super-num?    ( a1 n1 -- d f1 )
                FALSE to double?
                FALSE mynumber-c do-chain ;

: new-num       ( ^str -- d )           \ an extensible version of NUMBER
\ this one doesn't support Windows constants or floating point
                count find-buffer place
                find-buffer ?uppercase
                count super-num? 0= ?missing ;

' new-num is number                     \ replace normal number conversion
                                        \ with the new chain scheme

: ffhello       { \ doing-app? -- }             \ startup stuff
\ adapted from EXTEND.F
                ?loading off                    \ we aren't loading initially
                only forth also definitions
                decimal
                source-id                       \ if loading a file, close it
                if      source-id -1 <>
                        if      source-id fclose-file drop
                        then    0 to source-id
                then
                defer@ default-application ['] bye <> to doing-app?
                depth dup
                begin   ?dup            \ save any stack data
                while   rot >r 1-       \ to get prettier startup
                repeat  >r
                initialization-chain do-chain \ set up for Windoze operation
                r>
                begin   ?dup                    \ restore stack data
                while   1- r> swap
                repeat
                exception@ 0=                   \ -- f1 f2
                doing-app? 0= and               \ if no app, display info
                if      hostcon
                        cls <m+>
                            ." Firmware Studio Version " version" type
                        cr  ." Compiled with Win32forth ver " version# 0
                        <# # # # # # '.' hold #s #> type <m->
                        set-shell
                then
                                                \ f1 --
                false to ?win-error-enabled     \ needs this for NT fix
                refresh: fcon
                true to started                 \ use zero height to kill painting
                current-dir$ count '\' scan 1 /string ffpath place
                ffpath count bounds             \ default file path
        ?do     i c@ '\' = if '/' i c! then     \ change \ to /
        loop
                true
                if      doing-app? 0=           \ if no app, display more info
                                                \ and load config file
                        if      \ .mem-free
                                \ .words
                                &ff.cfg count "path-file nip nip 0=  \ if found
                                if      &ff.cfg ['] $fload catch ?dup
                                        if      message
                                                sp0 @ sp!
                                                quit
                                        then
                                then
                        then
                then
                exception@ 0=                   \ if no exception
                doing-app? and                  \ and have an app
                if      default-application     \ then execute it
                        bye                     \ and terminate
                then
                exception@
                if      bye                     \ can't do anything about exceptions
                then
                ;

true to sys-warning?

oldstartup 0= [if] ' ffhello is boot [then]

: mymessage  <e+> _message <e->                 \ error messages are red
                bstate off      \ default to interpret state
                0 to parms      \ clear locals list
                ;

:noname  (ltred) >fg: fcon ; is <e+>  \ color for errors, messages and editor
:noname  (blue)  >fg: fcon ; is <e->  \ see CLASTEXT.G for available colors
:noname  (blue)  >fg: fcon ; is <m+>
:noname  (blue)  >fg: fcon ; is <m->
:noname  (black) >fg: fcon ; is <ed+>
:noname  (blue)  >fg: fcon ; is <ed->

' mymessage is message

cr .elapsed with-img fsave ff
cr .(  Moving FF.EXE, FF.IMG to parent directory)
copyfile FF.EXE ..\ s" FF.EXE" delete-file drop
copyfile FF.IMG ..\ s" FF.IMG" delete-file drop

oldstartup [if]
        start-IDE
        .( Type BYE or press alt-F4 to exit)
[else]  3 seconds bye
[then]


