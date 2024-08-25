\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ \\\\\\\\        Serial communication interface         \\\\\\\\\\\\
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

ONLY FORTH ALSO DEFINITIONS

\ Communication with the target is through the following set of words:

false value ON-LINE?    ( -- f )        \ current status of target
defer TEST-COMM         ( -- )          \ test comm integrity, update on-line?
defer COMSOURCENAME     ( -- )          \ description of com source, --> temp$
defer CONNECTED?        ( -- f )        \ has communication been established?
defer OPEN-COMM         ( -- )          \ attempt to establish communications
defer CLOSE-COMM        ( -- )          \ disconnect communications
defer COMMUNICABLE?     ( -- f )        \ get comm status, try to open if not.
defer TRANSCEIVE        ( a len -- )    \ send/receive string, error throws
defer TARRESET          ( -- )          \ reset target hardware
defer CMSG-NC           ( -- )          \ append "not connected" msg to temp$
defer comm-bye          \ called when leaving program
defer comm-pacify       \ called when collecting target data

: scmsg-nc  ( -- ) s" Target Not Connected"   temp$ +place ;
' scmsg-nc is cmsg-nc

hex     \ backdoor commands: 0xC0..0xCF = digit, 0xE0..0xFF = cmd
        025 constant b=ack
        052 constant b=nack
        0E1 constant b.n?        \ shift byte from scratchpad
        0E2 constant b.depth     \ depth of door stacks
        0E3 constant b.clear     \ clear door stacks
        0E4 constant b.step      \ run single step thread n instructions
        0E5 constant b.run       \ run single step thread until breakpoint
        0E6 constant b.ssaddr    \ set next address in single step thread
        0E7 constant b.ssbrkpt   \ set breakpoint for single step thread
        0E8 constant b.pc@       \ get the last grabbed PC for profiler
        0E9 constant b.address   \ set address for memory operations
        0EA constant b.call      \ call routine at n, return n when done
        0EB constant b.@env      \ get byte n of environment string
        0EC constant b.@reg      \ get next byte in register space
        0ED constant b.@code     \ get next byte in code memory
        0EE constant b.@data     \ get next byte in data memory
        0EF constant b.!reg      \ store next byte to register space
        0F0 constant b.!code     \ store next byte to main memory
        0F1 constant b.!data     \ store next byte to i/o space
        0F2 constant b.@eestart  \ start EEPROM block read
        0F3 constant b.@eenext   \ read next byte in eeprom
        0F4 constant b.@eelast   \ read next byte in eeprom, end block read
        0F5 constant b.!eestart  \ start EEPROM block write
        0F6 constant b.!eenext   \ write next byte in eeprom
        0F7 constant b.!eelast   \ write next byte in eeprom, end write
        0F8 constant b.pushd     \ push n to the data stack
        0F9 constant b.popd      \ pop n from the data stack
        0FA constant b.pickd     \ pick n from the data stack
        0FB constant b.pickr     \ pick n from the return stack
decimal

defer getenv
  0 value tarerror?      \ T if a comm error occured during the transfer
  0 value tarprogress?   \ T if a progress window is desired
100 value allowableMS    \ time to wait before aborting operation
250 value timelimit-default \ increase this if your computer is really slow
10000 value timelimit-call  \ allow the target 10 seconds to do its thing
100 value patience       \ reasonable amount of time for Windows to space out

  16 constant envheadersize             \ # of bytes in the env header

create envheader envheadersize allot    \ byte-size target data

   0 constant b'cellbits                \ cell size in bits
  14 constant b'charbits                \ character size in bits
   1 constant b'addrnibbles             \ # of nibbles in addresses
   2 constant b'bindsize                \ binding table cell size
   3 constant b'features                \ available features + endianness
   4 constant b'CPUfamily               \ CPU family
   5 constant b'CPUsubfamily            \ CPU subfamily
   6 constant b'xtal                    \ crystal frequency, 4 bytes
  10 constant b'user                    \ reserved for user
  12 constant b'rev#                    \ backdoor revision#, 2 bytes
  15 constant b'#params                 \ # of cell-wide parameters

: envc@         ( n -- c )      envheader + c@ ;

  32 constant envparamsize

create envparams envparamsize cells allot \ cell-size target parameters
       envparams envparamsize cells erase

: envparam      ( n -- | -- a )
                cells envparams + constant ;

   0 envparam b.main0                   \ main memory
   1 envparam b.mainsize
   2 envparam b.reg0                    \ register space
   3 envparam b.regsize
   4 envparam b.io0                     \ I/O space
   5 envparam b.iosize
   6 envparam b.ee0                     \ EEPROM space
   7 envparam b.eesize
   8 envparam b.sp0                     \ origins of stacks
   9 envparam b.rp0
  10 envparam b.'sp                     \ locations of stack pointers
  11 envparam b.'rp
  12 envparam b.special                 \ address of special data
  13 envparam b.codeorigin              \ start of user code space
  14 envparam b.codesize                \ binding table grows downward
  15 envparam b.dataorigin              \ user data space
  16 envparam b.datasize
  17 envparam b.bindorigin              \ binding table origin
  18 envparam b.'vcon$
  19 envparam b.'execute                \ watch helpers
  20 envparam b.'here
  21 envparam b.'eval=mem
  22 envparam b.'evaluate
  23 envparam b.'<evaldrop>
  24 envparam b.'maxtok#                \ returns max token# seen

\ : addrunits     ( -- n )                \ cell size in address units
\                envheader b'cellsize + c@ ;

200 value progx0
200 value progy0

: "commxy"      ( -- )                  \ save X,Y starting position (pels)
                progx0 (.)        pad  place
                s"  TO PROGX0 "   pad +place
                progy0 (.)        pad +place
                s"  TO PROGY0 "   pad +place
                pad count orgsave ;

orgsaver chain-add "commxy"

\ -------------------------------------------------------------------
\ transfer progress window, press "stop" to abort transfer

:Object Progress <SUPER WINDOW

    StaticControl Text_1     \ a static text window
    ButtonControl Button_1   \ a button

:M ClassInit:   ( -- )
                ClassInit: super
                ;M

:M ExWindowStyle: ( -- style )
                ExWindowStyle: SUPER
                WS_EX_TOPMOST or
                ;M

:M WindowStyle: ( -- style )
                WindowStyle: SUPER
                WS_BORDER OR
                WS_OVERLAPPED OR
                ;M

int sending?

:M Sending:     ( f -- )   to sending? ;M

:M WindowTitle: ( -- title )
                sending?
                if      z" Sending"
                else    z" Receiving"
                then    ;M

: TextOrigin    ( -- x y )      4  4 ;
: TextBorder    ( -- x y )     80 20 ;
: ButtonOrigin  ( -- x y )    100  4 ;
: ButtonBorder  ( -- x y )     80 20 ;

:M StartSize:   ( -- width height )
                ButtonOrigin ButtonBorder d+ TextOrigin d+ ;M

:M MinSize:     StartSize: self ;M
:M MaxSize:     StartSize: self ;M

:M StartPos:    ( -- x y )
                progx0 progy0 ;M

:M Close:       ( -- )
                Close: SUPER
                ;M

:M On_Init:     ( -- )
                self                Start: Text_1 \ start up static text
                WS_GROUP           +Style: Text_1 \ and a group
                SS_CENTER          +Style: Text_1 \ and centering
                WS_BORDER          +Style: Text_1 \ and border to style
                TextOrigin TextBorder
                                     Move: Text_1 \ position the window
                s" 0%"            SetText: Text_1 \ set the window message

                IDOK                SetID: Button_1
                self                Start: Button_1
                ButtonOrigin ButtonBorder
                                     Move: Button_1
                s" Stop"          SetText: Button_1
                                 GetStyle: Button_1
                BS_DEFPUSHBUTTON   +Style: Button_1
                ;M

:M On_Done:      ( -- )
                originx to progx0
                originy to progy0
                On_Done: super ;M

:M On_Paint:    ( -- )          \ screen redraw procedure
                0 0 StartSize: self LTGRAY FillArea: dc
                ;M

create propad 8 allot

:M Update:      ( level span -- )
                100 -rot */
                -1 max 101 min
                (.d)   propad  place
                s" %"  propad +place
                propad count SetText: Text_1
                key? drop       \ let windows message loop breathe
                originx to progx0       \ save last known window position
                originy to progy0
                ;M

:M WM_COMMAND   ( hwnd msg wparam lparam -- res )
        over LOWORD ( ID )
        case
                IDOK  of   true to tarerror?   endof
        endcase
        0 ;M

;OBJECT


\ ---------------------------------------------------------------------

    0 value port#                       \ port# target was found on
    0 value baudrate

: _comsourcename ( -- )
\ append name of com source to temp$
                s" COM"                temp$ +place
                port# (.d)             temp$ +place
                s" : "                 temp$ +place
                baudrate 100 /
                10 /mod (.d)           temp$ +place
                s" ."                  temp$ +place
                (.d)                   temp$ +place
                ;

: _connected?   ( -- f )        port# 0<> ;

' _comsourcename is comsourcename
' _connected?    is connected?

:Object DCB        <Super Object

Record: AddrOf
        int   DCBlength
        int   BaudRate
        int   BinaryBits                \ a 32bit cell of bit fields
      1 bits  fBinary                   \ define the bit fields
      1 bits  fParity
      1 bits  fOutxCtsFlow
      1 bits  fOutxDsrFlow             \ SDI changed for Rts to Dsr
      2 bits  fDtrControl
      1 bits  fDtrSensitivity
      1 bits  fTXContinueOnXoff
      1 bits  fOutX
      1 bits  fInx
      1 bits  fErrorChar
      1 bits  fNull
      2 bits  fRtsControl
      1 bits  fAbortOnError
     17 bits  fDummy
        short wReserved
        short XonLim
        short XoffLim
        byte  ByteSize
        byte  Parity
        byte  StopBits
        byte  XonChar
        byte  XoffChar
        byte  ErrorChar
        byte  EofChar
        byte  EvtChar
        short wReserved1
;RecordSize: SIZEOFDCB

:M Reset:       ( -- )
                AddrOf SIZEOFDCB erase
                ;M

:M ClassInit:   ( -- )
                ClassInit: super
                Reset: self             \ create structure as Reset
                ;M

;Object

\ define the field names for the Communications Timeout structure

0       CELL Field+ .ReadIntervalTimeout
        CELL Field+ .ReadTotalTimeoutMultiplier
        CELL Field+ .ReadTotalTimeoutConstant
        CELL Field+ .WriteTotalTimeoutMultiplier
        CELL Field+ .WriteTotalTimeoutConstant
CONSTANT COMMTIMEOUTSBYTES

: ComTimeouts   { cHndl \ CT -- }     \ Initialize the communications timeouts
                COMMTIMEOUTSBYTES LocalAlloc: CT    \ allocate a CT structure
                CT COMMTIMEOUTSBYTES erase          \ initialize it to zeros
        \ set read timeouts to magic value of don't wait, just poll
                -1 CT .ReadIntervalTimeout          !
                 0 CT .ReadTotalTimeoutMultiplier   !
                 0 CT .ReadTotalTimeoutConstant     !
                 1 CT .WriteTotalTimeoutMultiplier  !
                20 CT .WriteTotalTimeoutConstant    !
                CT rel>abs
                cHndl
                Call SetCommTimeouts drop ;

: ComInit       ( -- cHndl f )
                temp$ 20 erase                    \ form "port#"
                s" COM"   temp$  place
                port# (.) temp$ +place
                temp$ 1+
                >R
                NULL                            \ no template
                NULL                            \ open file attributes
                OPEN_EXISTING                   \ creation distribution
                NULL                            \ no security attributes
                0                               \ exclusive access
                GENERIC_READ GENERIC_WRITE or   \ desired access modes
                R> rel>abs                      \ zstring filename
                Call CreateFile                 \ returns handle or -1
                dup -1 <>                       \ if -1 then error
                ;                               \ return handle to port

\ ************************************************************
\ here is a list of valid parameters for ComSetup.  Except for
\ the ByteSize parameter, these are all windows constants.

\ --- BaudRate    CBR_110       CBR_300       CBR_600
\                 CBR_1200      CBR_2400      CBR_4800
\                 CBR_9600      CBR_14400     CBR_56000
\                 CBR_19200     CBR_38400     CBR_57600
\                 CBR_115200    CBR_128000    CBR_256000
\
\ --- ByteSize    5, 6, 7, 8
\
\ --- Parity      NOPARITY      ODDPARITY     MARKPARITY
\                 EVENPARITY    SPACEPARITY
\
\ --- StopBits    ONESTOPBIT    TWOSTOPBITS   ONE5STOPBITS
\
\ ************************************************************

\ Setup the Communications state to the parameters specified

: ComSetup      { baud size parity stop cHndl -- }
                DCB.AddrOf rel>abs
                cHndl
                Call GetCommState ?win-error

                baud       Put: DCB.BaudRate
                size       Put: DCB.ByteSize
                parity 0<> Put: DCB.fParity             \ parity enabled flag
                parity     Put: DCB.Parity
                stop       Put: DCB.StopBits

                DCB.AddrOf rel>abs
                cHndl
                Call SetCommState ?win-error ;

: ComClose      ( cHndl -- )    \ close com port if its open
                ?dup
                if      Call CloseHandle drop
                then    ;

:Object COMSTAT        <Super Object

Record: AddrOf
        int   lpComStatBits             \ a 32bit cell of bit fields
      1 bits  fCtsHold                  \ define the bit fields Low to High
      1 bits  fDsrHold
      1 bits  fRlsdHold
      1 bits  fXoffHold
      1 bits  fXoffSent
      1 bits  fEof
      1 bits  fTxim
     25 bits  fReserved
        int   cbInQue
        int   cbOutQue
;RecordSize: SIZEOFCOMSTAT

:M Reset:       ( -- )
                AddrOf SIZEOFCOMSTAT erase
                ;M

:M ClassInit:   ( -- )
                ClassInit: super
                Reset: self             \ create structure as Reset
                ;M

;Object



: ComErrorClear { comhndl \ lpErrors -- f }      \ true = success
                COMSTAT.addrof  rel>abs
                lpErrors  rel>abs
                comhndl Call ClearCommError 0<>  ;    \ not used


: badread?      ( f -- )
\ error type 2: failed to read from serial port
                if 2 throw then ;

: badwrite?     ( f -- )
\ error type 3: failed to write to serial port
                if 3 throw then ;

0  value ComHndl
variable Comkey-val            \ a place to save last key received
0  value Comkey-flg?           \ have we already got a key?

: skey?         ( -- c1 )       \ get a key from serial port, don't wait long
                Comkey-flg? 0=
                if      Comkey-val 1 ComHndl read-file        \ -- len flag
                        badread?
                        to Comkey-flg?
                then    Comkey-flg? ;

1 value keyfudge                               \ # of iterations before ms@ test

: skey          ( -- c1 )                      \ must return a key
\ throw 1 if timeout
                keyfudge
        begin   ?dup                           \ wait a few ms without ms@ call
        while   1- skey?
                if      drop
                        Comkey-val c@          \ return the key
                        0 to Comkey-flg? exit  \ clear the save buffer
                then
        repeat  ms@                            \ over 20 ms elapsed
        begin   ms@ over -
                allowableMS > if 1 throw then   \ timed out
                skey?                  \ loop till we get one
        until   drop
                Comkey-val c@                  \ return the key
                0 to Comkey-flg? ;             \ clear the save buffer

: semit         { char -- }     \ write a character to the serial port
                &LOCAL char 1 ComHndl write-file
                badwrite? ;

: safekey       ( -- c )
                ['] skey catch          \ look for a response
                if      -1              \ no response, substitute -1
                        false to on-line?
                then    ;

: sbounce       ( c1 -- c2 )    semit skey ;
: _safebounce   ( c1 -- c2 )    semit safekey ;


: _TRANSCEIVE   ( a len -- )            \ send/receive string
\ transceive using 1:1 no-handshake protocol
                bounds
                ?do     on-line?
                        if      i c@ semit skey
                        else    -1
                        then    i c!
                loop
                ;
' _TRANSCEIVE is TRANSCEIVE

: auto>baud     ( c -- const rate )
\ calculate baud rate from response, 0 = no good
        case    0xE0    of   CBR_19200   19200   endof
                0xFC    of   CBR_38400   38400   endof
                0xFE    of   CBR_57600   57600   endof
                0xFF    of  CBR_115200  115200   endof
                0 swap
        endcase ;

: auto>baudLow  ( c -- const rate )
\ calculate baud rate from response, 0 = no good
        case    0xE0    of    CBR_2400    2400   endof
                0xFC    of    CBR_4800    4800   endof
                0xFE    of    CBR_9600       0   endof  \ 7200 not available
                0xFF    of   CBR_14400   14400   endof
                0 swap
        endcase ;

MsgWindow seekmsg

: testcom       ( -- f )
\ test communication link: 0xFC returns 0xFF, b.address returns nack
\ T if communications is OK
                0xFC sbounce 0xFF =
                b.address sbounce b=ack = and ;

: qtestcom      ( -- f )
                b.address _safebounce b=ack = ;

: .comopened    ( -- )
                cr ." Connection to target established on COM"
                port# . ." at " baudrate (.d) type ."  bps"
                ;

: ComSeek       ( -- )
\ search for the target device, scanning com ports 1 through 4.
\ port# = 0 if not found
                4 1
        do      115200 to baudrate
                i to port#  COMinit             \ try to open port
                if      to comHndl
                            Reset: DCB
                        FALSE Put: DCB.fOutxCtsFlow     \ Ignore CTS
                        CBR_115200 8 NOPARITY ONESTOPBIT comHndl ComSetup
                        comHndl ComTimeouts
                        250 ms                  \ let port power up
                        0xE0 _safebounce auto>baud  \ get autobaud response
                        dup
                        if      to baudrate     \ set correct baud rate
                                8 NOPARITY ONESTOPBIT comHndl ComSetup
                                qtestcom        \ make sure this is it
                                if      .comopened
                                        unloop  \ done with search
                                        exit
                                then
                        else    drop
                        then
                        comHndl COMclose
                else    drop                    \ no such port
                then
        loop    0 to port# ;

: ComSeekLow       ( -- )
\ search for the target device, scanning com ports 1 through 4.
\ port# = 0 if not found
\ This is a copy of ComSeek, but does it seek using 14400 instead of 115200 bps
                4 1
        do      14400 to baudrate
                i to port#  COMinit             \ try to open port
                if      to comHndl
                            Reset: DCB
                        FALSE Put: DCB.fOutxCtsFlow     \ Ignore CTS
                        CBR_14400 8 NOPARITY ONESTOPBIT comHndl ComSetup
                        comHndl ComTimeouts
                        250 ms                  \ let port power up
                        0xE0 _safebounce auto>baudlow  \ get autobaud response
                        dup
                        if      to baudrate     \ set correct baud rate
                                8 NOPARITY ONESTOPBIT comHndl ComSetup
                                qtestcom        \ make sure this is it
                                if      .comopened
                                        unloop      \ done with search
                                        exit
                                then
                        else    drop
                        then
                        comHndl COMclose
                else    drop                    \ no such port
                then
        loop    0 to port# ;

: termemitend   ( -- )          \ flush pad to console if necessary
                pad c@
        if      pad count type cr
        then    pad off ;

: termemit      ( c -- )        \ handle control characters
        case    10 of noop                      endof
                13 of termemitend               endof
                pad count + c!
                1 pad c+!  pad c@ 80 > if termemitend then
                0
        endcase ;

: _test-comm    ( -- )
\ see if the target is communicating, update on-line? status
                port#
        if      pad off
                begin   skey?            \ should be nothing in the buffer
                while   skey termemit
                repeat  termemitend
                100 to allowableMS       \ ms before time-out
                ['] testcom catch        \ look for a response
                if      false to on-line?        \ nothing out there
                else    to on-line?              \ result of the test
                then
                timelimit-default to allowableMS \ default time-out
        then    ;

' _test-comm is test-comm


: DisconnectTarget ( -- )
                port#
                if      comHndl ComClose \ close if already connected
                        0 to port#
                        false to on-line?
                then    ;

' DisconnectTarget is close-comm


: _headerbad?   ( a lo hi -- f )    \ T if env byte out of bounds
                rot envc@ c>n -rot between 0= ;

: headergood?   ( -- f )
\ F if header data doesn't make sense
                b'cellbits      8 64 _headerbad? if 0 exit then
                b'addrnibbles   2 16 _headerbad? if 0 exit then
                b'bindsize     -8  8 _headerbad? if 0 exit then
                b'CPUfamily     0 63 _headerbad? if 0 exit then
                b'#params       0 63 _headerbad? if 0 exit then
                true ;

: AcquireTarget ( -- )
\ start communications, load environment parameters.
\ sets port# = 0 if no target device is available
                DisconnectTarget  1 to keyfudge
                s"  Seeking Target " MessageText: seekmsg
                true ontop: seekmsg
                start: seekmsg
                COMseek                  \ find the device
                port# 0= if COMseekLow then     \ try low baud rates if not found
                <e+> port#
        if      50 ms test-comm
                getenv  5               \ try loading multiple times
                begin   headergood? 0=  \ (working around wierdness)
                        over and        \ load environment parameters
                while   1-  250 ms  test-comm getenv
                        '.' emit
                repeat  0=
                if      cr ." Invalid target environment information"
                else
                        begin   ms@ keyfudge             \ calculate fudge value for 20 msec.
                                begin   ?dup
                                while   1- skey? drop
                                        0 if noop then
                                repeat  ms@ swap -       \ time of fudge
                                20 <
                        while   keyfudge  2* to keyfudge
                        repeat
                then
        else    cr ." No target found."
        then    <e->
                close: seekmsg
                ;

' AcquireTarget is open-comm

:noname         ( -- f )
\ determine whether communications is possible
                connected? 0=            \ port not yet selected, try to find
                if      ConnectToTarget  \ open comm, allocate memory for hex
                then    on-line?      \ return on-line status
                key? drop             \ let Windows breathe
                ; is communicable?



ONLY FORTH ALSO DEFINITIONS

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ \\\\\\\\\     Target Backdoor Communications          \\\\\\\\\\\\\\\\
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

80 constant CPUIDwidth          \ max length of family ID string

CPUIDwidth #families * constant CPUIDsize
create CPUIDs  CPUIDsize allot  \ create space for ID strings
CPUIDs CPUIDsize blank          \ start with blank IDs

: CPUID"        ( n <string>" -- )
\ Get CPU ID string n from the input stream, add it to the system
                [char] " word count
                CPUIDwidth min          ( n a len )
                rot #families 1- min    ( a len n )
                CPUIDwidth * CPUIDs +   ( a len a' )
                swap move ;

: (cpuid)       ( a len member# -- a' n' )
\ look up substring describing the CPU member
                begin   dup             \ scan to mth '|'
                while   >r
                        [char] | scan
                        1 /string
                        r> 1-
                repeat  drop
                dup
                if      2dup [char] | scan   ( a n a' n' )
                        nip -
                then    ;

: CPUID         ( family member -- a len )
\ Look up the name of a CPU in the ID array
                >r CPUIDwidth * CPUIDs +
                CPUIDwidth -trailing    \ family string
                2dup r> (cpuid)         ( a n str )
                dup 0=                  \ use member 0 if unknown member
                if      2drop 0 (cpuid) dup ?exit
                        2drop s" unknown"
                else    2swap 2drop     \ valid member name
                then    ;

msgwindow compopup

   0 value CPUfamily
   0 value CPUsubfamily
   0 value jsrsize           \ size of long jump required by binding table

variable tempcomm


\ --------  Interface to the target device  ---------------------------

\ All I/O should go through these words so that the physical interface
\ can be changed (to error-correcting modem, for example).

: rwbounce      ( c -- c' )
                tempcomm c!  tempcomm 1  transceive
                tempcomm c@ ;

: safebounce    ( c -- c' )
                ['] rwbounce catch
                if      drop -1
                then    ;

: stransceive   ( a len -- )    \ safe transceive
                ['] transceive catch
                if      false to on-line?
                        2drop
                then    ;


create commpad 256 allot
variable commptr

: comlay        ( c -- )        commpad commptr @ + c!
                                commptr @ 1+ 255 min commptr ! ;
: comlaynibble  ( n -- )        0xC0 or comlay ;
: comlaybyte    ( c -- )        0x10 /mod comlaynibble comlaynibble ;
: sendcomm      ( -- )          commpad commptr @ stransceive ;

: (>n)          ( n #nibbles -- )
\ load n to commpad, ready to append or send
                <#  0
                ?do     16 /mod swap 0xC0 + hold
                loop    0
                #> dup commptr !
                commpad swap move ;

: >n            ( n -- )
\ load n to the target's scratchpad
                cellnibbles (>n) sendcomm ;

: (n>)          ( #bytes -- n )
\ get n from the scratchpad, low byte first
                >r
                commpad r@ b.n? fill
                commpad r@
                stransceive
                0 commpad r@ +                  \ -> end of data
                r>                              ( x a . )
                begin   ?dup
                while   1- >r 1-                ( x a' )
                        swap 8 lshift over c@ + ( a' x' )
                        swap
                        r>
                repeat  drop ;

: n>            ( -- n )
                cellsize (n>) ;

: set-address   ( a -- )
\ set the address for I/O to the target
                addrnibbles (>n) b.address comlay sendcomm ;

: >byte         ( c -- )
\ load 2-nibble n to the scratchpad
                cellbits >r
                8  to cellbits  >n
                r> to cellbits ;

:noname         ( -- )
\ load the target's environment string
\ the target's arbitrary-sized params are mapped to 32-bit params here
                16 to cellbits  0 >n                    \ start at beginning
                envheader envheadersize b.@env fill
                on-line? if comm-pacify then
                envheader envheadersize stransceive     \ load byte-size stuff
                on-line? if comm-pacify then
                headergood? on-line? and to on-line?    \ offline if bad header
                on-line?
        if      b'cellbits     envc@ to cellbits
                b'charbits     envc@ ?dup if to charbits then
                b'CPUfamily    envc@ to CPUfamily
                b'CPUsubfamily envc@ to CPUsubfamily
                b'bindsize     envc@ c>n to jsrsize
                b'addrnibbles  envc@ to addrnibbles
                b'#params envc@
                envparamsize min  0             ( #params 0 )
                ?do     comm-pacify winpause
                        commpad cellsize b.@env fill   \ load cell-size stuff
                        commpad cellsize stransceive
                        0 commpad cellsize bounds
                        ?do     8 lshift i c@ +
                        loop
                        envparams i th !
                loop
        then    ; is getenv


: FeatureOn?    ( mask -- f )
\ gets option flag, returns T if it's set
                b'features envc@ and 0<> ;

: steppable?    ( -- f )   0x80 FeatureOn? ;    \ single step capability
: callable?     ( -- f )   0x40 FeatureOn? ;    \ call command is available
: mem-bigend?   ( -- f )   0x08 FeatureOn? ;    \ memory endian-ness
: reg-bigend?   ( -- f )   0x04 FeatureOn? ;    \ register endian-ness

: bpushd        ( n -- )        cellnibbles (>n)  b.pushd comlay sendcomm ;
: bpopd         ( -- n )        b.popd safebounce drop  n> ;
: bpickd        ( n -- n' )     >byte b.pickd safebounce drop  n> ;
: bpickr        ( n -- n' )     >byte b.pickr safebounce drop  n> ;
: bclear        ( -- )          b.clear safebounce drop ;
: bdepth        ( -- n )        b.depth safebounce drop  n> ;

: bexecute      ( cfa -- )      timelimit-call to allowableMS
                                addrnibbles (>n)  b.call  comlay sendcomm ;

: ssaddr!       ( a -- )        \ set single-step address
                addrnibbles (>n) b.ssaddr comlay sendcomm ;

: ssbrkpt!      ( a -- )        \ set breakpoint address
                addrnibbles (>n) b.ssbrkpt comlay sendcomm ;

: endstep       ( c -- a )
\ finish a trace session, give message if it's taking a long time
                -1 =
                if      ( open popup window )
                        s"  Running... " MessageText: compopup
                        true ontop: compopup
                        start: compopup
                        begin   key? if key 27 = else 0 then
                                0 safebounce -1 <>  or
                        until
                        ( close window )
                        close: compopup
                        false to on-line?
                        1 throw                 \ 1 = user abort
                else    addrnibbles 2/ (n>)     \ get ending pc
                then    ;


: ssstep        ( n -- a )
\ trace n instructions, return address a.  a=-1 if aborted by user
                >n b.step safebounce endstep ;

: ssrun         ( -- )
\ run until trace PC matches breakpoint
                timelimit-call to allowableMS
                b.run safebounce endstep drop ;

variable tempcmd

: swrite-x      ( asrc adest n cmd -- )
\ write to a memory space, throws if comm error
                tempcmd !
                swap set-address swap           ( n asrc )
                commptr off
                begin   over
                while   count comlaybyte
                        tempcmd @ comlay        \ write command
                        -1 under+
                repeat  2drop
                commpad commptr @
                transceive
                ;


: swrite-code   ( asrc adest n -- )     b.!code swrite-x ;
: swrite-data   ( asrc adest n -- )     b.!data swrite-x ;
: swrite-reg    ( asrc adest n -- )     b.!reg  swrite-x ;

: swrite-ee     ( asrc adest n -- )
\ write to EEPROM/non-volatile space
\ throws: 1 = time-out, 2/3 = I/O error
                dup
        if      swap >n
                b.!eestart rwbounce drop        \ set start address ( as n )
                bounds
                ?do     i c@ >byte
                        b.!eenext rwbounce drop
                loop
                b.!eelast rwbounce drop
        else    3drop                           \ no data to send
        then    ;

: sread-x       ( asrc adest n cmd -- )
\ read from a memory space, throws if comm error
                >r temp$ over r> fill             ( as ad n )
                rot set-address
                temp$ over transceive             ( ad n )
                temp$ -rot move ;

: sread-code    ( asrc adest n -- )     b.@code sread-x ;
: sread-data    ( asrc adest n -- )     b.@data sread-x ;
: sread-reg     ( asrc adest n -- )     b.@reg  sread-x ;

: sread-ee      ( asrc adest n -- )
\ read from EEPROM/non-volatile space
\ throws: 1 = time-out, 2/3 = I/O error
                rot  >n
                b.@eestart rwbounce drop
                1- dup 0<                       ( a n' . )
                if      2drop                   \ no @next bytes
                else    0
                        ?do     b.@eenext rwbounce
                                lay
                        loop
                then
                b.@eelast rwbounce swap c!
                ;

 0 value tarspan        \ span of progress window
 0 value tarcount       \ # of bytes left to transfer

: tarupdate     ( -- )
\ update progress window
                tarprogress?
                if      tarspan tarcount -
                        tarspan update: progress
                then    key? drop              \ keep Windows chugging along
                ;

: tarbegin      ( n -- )     dup to tarspan to tarcount false to tarerror? ;
: tarinc        ( n -- n' )      TarDataSize + ;

: tarnext       ( a b -- a' b' )
                TarDataSize + TarDataSize under+
                TarDataSize negate +to tarcount
                tarupdate ;

: tarsize       ( -- n )
                TarDataSize tarcount min ;

: tarabort      ( -- )
                0 to tarcount  true to tarerror? ;

create tarpad 64 allot

: tarwrite      ( asrc adest n xt -- ior )
\ write to target in TarDataSize chunks
                tarprogress?
                if  true sending: progress  Start: progress  then
                >r tarbegin
                begin   TarCount 0>             ( as ad . | xt )
                        tarerror? not and
                while   over tarpad tarsize move     \ outgoing --> tarpad
                        tarpad over tarsize     ( as ad n | xt )
                        r@ catch
                        if      tarabort        \ comm error, quit.
                                3drop           \ should have swallowed as ad n
                        then    tarnext
                repeat
                r>drop 2drop
                tarerror?
                tarprogress? if Close: progress then
                false to tarprogress?
                ;

create tarwriters
                ' swrite-code , ' swrite-data , ' swrite-reg , ' swrite-ee ,

: write-tar     ( asrc adest n space -- ior )
\ write to any memory space
                tarwriters swap 3 and th @
                tarwrite ;

: write-code    ( asrc adest n -- ior )     0 write-tar ;
: write-data    ( asrc adest n -- ior )     1 write-tar ;
: write-reg     ( asrc adest n -- ior )     2 write-tar ;
: write-ee      ( asrc adest n -- ior )     3 write-tar ;

: tarread       ( asrc adest n xt -- ior )
\ read from target in TarDataSize chunks
                tarprogress?
                if  false sending: progress  Start: progress  then
                >r tarbegin
                begin   TarCount 0>               ( as ad . | xt )
                        tarerror? not and
                while   over tarpad tarsize r@ catch \ read from target
                        if      tarabort       \ comm error, quit.
                                3drop          \ should have swallowed as ad n
                        then
                        tarpad over tarsize move     \ copy to memory
                        tarnext
                repeat
                r>drop 2drop
                tarerror?
                tarprogress? if Close: progress then
                false to tarprogress?
                ;

create tarreaders
                ' sread-code , ' sread-data , ' sread-reg , ' sread-ee ,

: read-tar      ( asrc adest n space -- ior )
\ read from any memory space
                tarreaders swap 3 and th @
                tarread ;

: read-code     ( asrc adest n -- ior )     0 read-tar ;
: read-data     ( asrc adest n -- ior )     1 read-tar ;
: read-reg      ( asrc adest n -- ior )     2 read-tar ;
: read-ee       ( asrc adest n -- ior )     3 read-tar ;


: download      ( asrc adest n -- )
\ download data from image space to the target adest
                rot codeorigin - codebuf + -rot
                <e+> write-code
                if cr ." Download incomplete!" then <e-> ;

: @nextreg      ( -- c )
\ get the next sequential register space byte
                b.@reg safebounce ;

: !nextreg      ( c -- )
\ store the next sequential register space byte
                >byte b.!reg safebounce drop ;

variable tempreg \ register read and write 8-bit, 16-bit and 32-bit --------

: @regc         ( a -- c )      tempreg 1 read-reg drop  tempreg c@ ;
: !regc         ( c a -- )      swap tempreg c!  tempreg swap 1 write-reg drop ;

: @regw         ( a -- n )      tempreg 2 read-reg drop
                                tempreg w@ bigendian? if byte-swap then ;

: !regw         ( n a -- )      >r bigendian? if byte-swap then tempreg w!
                                tempreg r> 2 write-reg drop ;

: @regl         ( a -- n )      tempreg 4 read-reg drop
                                tempreg @ bigendian? if byte-rev then ;

: !regl         ( n a -- )      >r bigendian? if byte-rev then tempreg !
                                tempreg r> 4 write-reg drop ;

2variable tempvar

: tar@          ( addr len space endian -- n )
\ generic fetch from target
\ len:     length of n in bytes
\ memtype: 0=main, 1=register, 2=i/o, 3=nonvolative
\ endian:  0=bigendian (MSB first)
                >r           >r tuck            ( len a len )
                tempvar swap r> read-tar        \ read raw data
                if true to tarerror? then
                0 tempvar rot bounds
                r>                              ( 0 a' a endianness )
                if      swap                    ( 0 a a' )
                        ?do     8 lshift i c@ +
                     -1 +loop
                else    ?do     8 lshift i c@ + \ hi byte first
                        loop
                then    ;

: tar!          ( n addr len space endian -- )
\ generic store to target
\ len:     length of n in bytes
\ memtype: 0=main, 1=register, 2=i/o, 3=nonvolative
\ endian:  0=bigendian (MSB first)
                >r      3 pick pluck            ( n a l s  n l )
                        tempvar swap            ( .... n adest len )
                r>
                if      bounds
                        ?do     dup i c!        \ low byte first
                                8 rshift
                        loop
                else    under-- bounds swap
                        ?do     dup i c!        \ hi byte first
                                8 rshift
                     -1 +loop
                then
                drop                            ( n a l s )
                2>r  tempvar swap               \ write raw data
                2r>  write-tar
                if true to tarerror? then
                drop ;

: tar-xtal      ( -- n )
\ get target's crystal frequency in Hz
                0  b'xtal 4 bounds
                do      8 lshift
                        i envc@ or
                loop    ;

: tar-revision  ( -- lo hi )
\ get target version#, hi (major) and lo (minor) parts.
                b'rev# 1+ envc@
                b'rev#    envc@ ;

: tar-user      ( -- n )
\ get 16-bit user data
                b'user 1+ envc@
                b'user    envc@  byte-join ;

: sext          ( u -- x )
\ sign extend u based on target cell size in bits
\ i.e. 000FFFFF (-1 on 20-bit target) --> FFFFFFFF
                cellbits 32 <
                if      dup cellbits 1- rshift 1 and    \ test the sign bit
                        if      -1 cellbits lshift or   \ sign extend
                        then
                then    ;

: tjoin         ( ulo uhi -- d )
\ join target-cellsize cells to form a double cell on the host
\ don't sign extend ulo and uhi, but sign extend the result
                cellbits 8 32 within
                if      -1 cellbits lshift invert >r    \ mask
                        r@ and sext s>d
                        cellbits 0 do d2* loop rot      \ d ulo
                        r> and rot or swap
                then    ;

: tsplit        ( d -- nlo nhi )
\ opposite of tjoin
                cellbits 32 <
                if      -1 cellbits lshift invert >r    \ mask
                        over r@ and -rot        ( nlo d )
                        cellbits 0 ?do d2/ loop drop
                        r> and
                then    ;


: .meg          ( u -- )
\ display as millionths
                0 <# # # # # # # '.' hold #s #>
                '0' -trailchars          \ drop trailing zeros
                '.' -trailchars          \ drop trailing decimal
                type space ;

: .LSF          ( t -- )
                if      ." big endian    "
                else    ." little endian "
                then    ;

: ah.           ( n -- )
                addrnibbles (h.) type space ;

: _tar?         ( -- )
\ verbose listing of target characteristics
                <m+>
                cr cellbits . ." bits per cell, "
                    addrnibbles 4 * . ." bit debugger addresses"
                cr ." Code memory: "    mem-bigend? .LSF
                   b.codeorigin @ ah. ." ..."
                cr ." Binding table:             "
                   b.bindorigin @ 1+ dup jsrsize +
                   jsrsize 0<
                   if   ." ... 1=" ah. ." 0=" ah.
                   else ." 0=" swap ah. ." 1=" ah. ." ..."
                   then
                cr ." Data memory: "    mem-bigend? .LSF
                   b.dataorigin @ dup ah.
                   ." .. "  b.datasize @ cellsize * 1- + ah.
                cr ." Single steppable:       " steppable?   .y/n
                cr ." Call command:           " callable?    .y/n
                cr ." Family:                 " CPUfamily (.) type
                   ." ." CPUsubfamily .
                cr ." CPU clock frequency:    " tar-xtal .meg ." MHz"
                cr ." backdoor revision:      " tar-revision
                   2 (h.) type ." ." 2 (h.) type
                cr ." user data:              " tar-user .
                <m-> ;


: _HwReset      { cHndl state -- }
                DCB.AddrOf rel>abs cHndl
                Call GetCommState ?win-error
                  state  Put: DCB.fDtrControl
                DCB.AddrOf rel>abs cHndl                        
                Call SetCommState ?win-error  ;                 

: HwReset       ( -- )
\ Reset target board by dropping DTR for 1/2 second
                on-line?
        if      comHndl  DTR_CONTROL_DISABLE  _HwReset
                500 ms
                comHndl  DTR_CONTROL_ENABLE   _HwReset
        else    beep
        then
                ;

' HwReset is TarReset

' close-comm is comm-bye
' noop is comm-pacify

: commo=serial  ( -- )
\ Direct communications to the local serial ports.
        ['] _test-comm       is test-comm
        ['] _comsourcename   is comsourcename
        ['] _connected?      is connected?
        ['] AcquireTarget    is open-comm
        ['] DisconnectTarget is close-comm
        ['] _transceive      is transceive
        ['] HwReset          is TarReset
        ['] scmsg-nc         is cmsg-nc
        ['] close-comm       is comm-bye
        ['] noop             is comm-pacify
        ;

fwend chain-add comm-bye


\ =============================================================================
\ Multidrop RS485 communication support

128 value packetaddr     \ default multidrop RS485 address
1 value RTStransmit?                    \ T = no embedded switch codes
19200 value mdbaud                      \ 19200 baud
1 value mdport#                         \ COM1

: mdcmsg-nc     ( -- )
                s" [" temp$ +place  mdport# (.) temp$ +place
                s" :" temp$ +place
                packetaddr 0x7F and (.d)        temp$ +place
                s" ] Not Connected" temp$ +place
                ;

0 value maxtime \ timeout value for 19 characters (9600=20)

: isbaud?       ( baud time CBR -- F | CBR T )
                swap to maxtime
                swap mdbaud <>
                if drop false else true then ;

: MyCBR         ( -- baudCBR )
                256000  1  CBR_256000 isbaud? ?exit
                128000  2  CBR_128000 isbaud? ?exit
                115200  3  CBR_115200 isbaud? ?exit
                 57600  7  CBR_57600  isbaud? ?exit
                 56000  7  CBR_56000  isbaud? ?exit
                 38400 10  CBR_38400  isbaud? ?exit
                 19200 20  CBR_19200  isbaud? ?exit
                 14400 36  CBR_14400  isbaud? ?exit
                  9600 48  CBR_9600   isbaud? ?exit
                  4800 96  CBR_4800   isbaud? ?exit
                192 to maxtime CBR_2400 ;   \ default to 2400

1 value  packetID
variable mdTimeout
variable mdtype         \ last received packet ID and type byte
0 value Com1Hndl

\ ERROR LOGGING ----------------------------------------------------------------

0 value checkdepth  0 value tag
: chk[          ( ch -- )  to tag  depth to checkdepth ;
: ]chk          ( n -- )   checkdepth + depth 1- - ?dup if tag emit . then ;


0 value logging  \ Set to 1 to log bus traffic, for testing link robustness
0 value logfile  create logstr 1024 allot
: fname s" 485.log" ;

: errlog        ( addr len -- )
\ Open the log file, append the string, and close the log file.
                logging 0= if 2drop exit then           \ not logging
        begin   fname r/w open-file   swap to logfile   \ look for log file
        while   fname r/w create-file drop to logfile   \ create the file
                logfile close-file drop
        repeat  logfile file-append drop
                logfile write-line drop
                logfile close-file drop ;

: err=nack      s" NACK received -----------------" errlog  ;
: err=crc       s" Mangled ACK received ----------" errlog  ;
: err=ack       s" ACK with mismatched ID received" errlog  ;
: err=timeout   s" TIME-OUT **********************" errlog  ;
: err=dup       s" RX packet was a response to a duplicate" errlog ;
: err=other     s" UNKNOWN received --------------" errlog  ;
: err=write     s" ERROR WRITING TO COM PORT *****" errlog  ;
: err=read      s" ERROR READING COM PORT ********" errlog  ;
: duf           bounds ?do i c@ 2 (h.) logstr +place
                s"  " logstr +place loop  logstr count errlog ;
: txlog         ( a n -- )    logging if   s" TX: " logstr place duf
                              else 2drop then ;
: rxlog         ( a n -- )    logging if   s" RX[" logstr place
                mdtype c@ 2 (h.) logstr +place s" ] " logstr +place
                duf           else 2drop then ;

\ ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

create MDoutbuffer      66 allot        \ pending output data
create MDinbuffer       66 allot        \ received raw input data
create MDrawbuffer     258 allot
0 value tout


: rerror?       ( f -- ) if err=read then ;        \ possible errors: read
: werror?       ( f -- ) if err=write then ;            \           write
: timeout?      ( -- f )   ms@ MDtimeout @ - 0> dup to tout ;
: timeout!      ( -- )   patience + ms@ + MDtimeout !  0 to tout ;
: mdgetch       ( -- c )
                -1 Comkey-val c!
        begin   winpause
                Comkey-val 1 Com1Hndl read-file drop
                1 =  timeout? or
        until   Comkey-val c@
                dup MDrawbuffer dup c@ + 1+ c!
                1 MDrawbuffer c+!
                ;
variable mdcrc  \ ----------------------------- CRC handling
: mdcrc@        ( -- n )        mdgetch mdgetch 0x07 lshift or
                                mdgetch  0x0E lshift or ;
: crc[          ( -- )          PacketAddr mdcrc ! ;
: crc+          ( c -- c )      mdcrc @ over crc-ccitt mdcrc ! ;
: ]crc          ( -- f )        mdcrc@ mdcrc @ <> ;
: laycrc        ( a crc -- a' ) dup>r 0x7F and lay  r@ 07 rshift 0x7F and lay
                                r> 0x0E rshift 0x7F and lay ;
variable packedMSBs \ ------------------------- MSB unpacking
: nextMSB       ( -- c )   packedMSBs c@ dup 2* packedMSBs c! 0x40 and 2* ;
: bumpPacketID  ( -- )     packetID 1+ 7 and to packetID ;
: shorttime     ( -- )     maxtime Timeout! ;

: RecDatapacket ( -- result )
                mdgetch crc+ 0x3F and
                MDinbuffer char+ swap 0         ( dest . . )
        ?do     mdgetch crc+  i 7 and 0=
                if      packedMSBs c!           \ every 8th byte is MSBs
                else    nextMSB + lay
                then
        loop    MDinbuffer char+ - MDinbuffer c!
                ]crc if 5 else 6 then ;         \ 6 = ACK received

create tempdepth 40 allot

: _MDlisten     ( -- result )
\ Wait for response from slave after a transmission
\ 0 = received NACK        3 = timed out
\ 5 = received mangled ACK 6 = received good ACK
                MDrawbuffer off
        begin   begin   tout if err=timeout 3 exit then
                        mdgetch PacketAddr =    \ wait for an address match
                until   mdgetch crc[ crc+ shorttime
                dup mdtype c!
                case PacketAddr of false endof  \ double address is our echo
                     0x0E and 4 of true  endof  \ accept response
                     false swap                 \ anything else is not
                endcase
        until                                   \ ignore non-data packets
                mdtype c@ 0x0F and
        case    0x04 of RecDatapacket           endof
                0x05 of RecDatapacket           endof
                0x06 of ." NA " 5                       endof   \ please resend
                err=other 0 swap
        endcase
;

create mdpad 16 allot 

: _MDpoll       ( -- result )          \ POLL command
                mdpad PacketAddr lay PacketAddr lay 0 lay drop
                mdpad 3 2dup txlog  Com1Hndl write-file werror?
        begin   begin   mdgetch PacketAddr =       \ wait for an address match
                        tout or
                until   mdgetch dup PacketAddr =
                if      drop false                 \ ignore our own echo
                else    true
                then
        until   tout if drop 3 then ;

: MdPoll        ( -- n )
\ Poll the slave device, return result:
\ 1,2,4 = comm error    3 = time-out
\ 6 = READY             7 = EMPTY       8 = BUSY
                maxtime timeout! _MDpoll ;

: _MDsend       ( -- )
\ Transmit the output buffer contents
                MDoutbuffer count txlog
                MDoutbuffer count Com1Hndl write-file werror? ;

2variable *echo \ data buffer we get data from and return data to.

: bogusecho     ( -- )   *echo 2@ erase ;
\ comm error occurred, fake a response.

0xFC value TXcommand
0xFD value RXcommand
FALSE value SmartDongle?

: formpacket    ( a n type -- )
                hld !
                dup 64 u> abort" Multidrop TX data too long"
                2>r
                MDoutbuffer 0 lay
                SmartDongle? if TXcommand lay then      \ ?? TXCMD
                PacketAddr lay  PacketAddr lay          \ address address
                packetID 7 and  4 lshift hld @ or lay   \ packetID&type
                r@ lay                                  \ length
                2r> bounds    ( dest . . )
        ?do     i c@ 0x7F and lay                       \ pack bytes
        loop    MDoutbuffer
                SmartDongle? if 4 else 3 then + 2dup -  ( dest src n )
                PacketAddr -rot  bounds
        ?do     i c@ crc-ccitt
        loop    laycrc                                  \ lay crc
                SmartDongle? if RXcommand lay then      \ lay RXCMD
                MDoutbuffer 1+ - MDoutbuffer c!         \ resolve length
                ;
: MDlisten      ( -- f )
                winpause allowableMS Timeout!           \ timeout time
                _MDlisten ;

\ ------------------------------------------------------------------------------
200 value hangx0
200 value hangy0

: "hangxy"      ( -- )                  \ save X,Y starting position (pels)
                hangx0 (.)        pad  place
                s"  TO HANGX0 "   pad +place
                hangy0 (.)        pad +place
                s"  TO HANGY0 "   pad +place
                pad count orgsave ;

orgsaver chain-add "hangxy"

:Object HangWarning <SUPER WINDOW

    ButtonControl Button_1   \ a button

:M ClassInit:   ( -- )
                ClassInit: super
                ;M

:M ExWindowStyle: ( -- style )
                ExWindowStyle: SUPER
                WS_EX_TOPMOST or
                ;M

:M WindowStyle: ( -- style )            \ return the window style
                WS_OVERLAPPED WS_CAPTION + WS_THICKFRAME + 
                ;M


:M WindowTitle: ( -- title )
                z" HANGING" ;M

:M StartSize:   ( -- width height )   120 28 ;M
:M MinSize:     StartSize: self ;M
:M MaxSize:     StartSize: self ;M

:M StartPos:    ( -- x y )
                progx0 progy0 ;M

:M Close:       ( -- )
                Close: SUPER  ;M

:M On_Init:     ( -- )
                IDOK                SetID: Button_1
                self                Start: Button_1
                10 4 100 20          Move: Button_1
                s" Abort"         SetText: Button_1
                                 GetStyle: Button_1
                BS_DEFPUSHBUTTON   +Style: Button_1
                ;M

:M On_Done:     ( -- )
                originx to hangx0
                originy to hangy0
                On_Done: super ;M

:M On_Paint:    ( -- )          \ screen redraw procedure
                0 0 StartSize: self LTGRAY FillArea: dc
                ;M

:M WM_COMMAND   ( hwnd msg wparam lparam -- res )
        over LOWORD ( ID )
        case
                IDOK  of   close-comm   endof
        endcase
        0 ;M

;OBJECT

\ ------------------------------------------------------------------------------

variable mwopened
: md-warning-open ( -- )
                start: HangWarning  mwopened on ;

: md-warning-close ( -- )
                mwopened @ if close: HangWarning then mwopened off ;

variable mt-tally
create mdtxpad 256 chars allot
: mdtx          ( -- )  mt-tally incr mdtxpad count 2 formpacket _MDsend ;

: mdecho        ( a n -- )
\ send and receive an encapsulated string of debugger data:
\ form an ACK packet containing the input string
                port# 0= if 2drop exit then             \ fake data if commo aborted
         dup 0= if      s" Zero length Transceive ==========" errlog
                        2drop exit
                then    2dup mdtxpad place              \ store raw string
                *echo 2!  mt-tally off                  \ points to tranceive result
                500 to allowableMS  mdtx
\ send data until we get a valid ACK packet
        begin   mt-tally @ 6 =
                if      md-warning-open                 \ open warning window
                then
                MDlisten
                case    0 of err=nack  false  endof     \ NACK --> resend ACK
                        5 of err=crc   false  endof     \ mangled --> resend ACK
                        6 of mdtype c@ 0x0F and 4 <>    \ OK... repeated packet?
                             dup
                             if err=dup bumpPacketID then \ yes, ignore it and retry
                             0= endof
                        3 of false          endof       \ timeout -> resend
                        1 of bogusecho exit endof       \ comm error
                        2 of bogusecho exit endof       \ comm error
                        false swap
                endcase
                dup 0= if mdtx then                     \ have to re-send?
                MDrawbuffer count rxlog
                port# 0= or                             \ forced to quit?
        until   md-warning-close
\ bump the packet ID and return the result
                bumpPacketID
                MDinbuffer count
                *echo 2@
                drop swap move
                ;

: _]com         port#
        if      Com1Hndl ComClose
                0 to Com1Hndl           \ clear handle value
                0 to port#
                0 to on-line?
        then    ;

defer ]com  ' _]com is ]com

: wayfalse      ( -- false )
                s" Error during POLL, disconnecting" errlog ]com false ;

: mdonline?     ( -- f )
                port#
        if      maxtime 10 + to allowableMS
                0xFF Comkey-val c! Comkey-val 1 2 formpacket
                ['] _MDsend catch if wayfalse exit then
                MDlisten bumpPacketID
                case    1 of wayfalse endof             \ comm error
                        2 of wayfalse endof             \ comm error
                        3 of false endof                \ time-out
                        true swap                       \ some kind of response
                endcase            \ MDinbuffer will contain FF
        else    false
        then    ;

variable mdonlines
4 value mdbounces       \ debouncing level

: mdtestcomm    ( -- )
\ Windows sometimes spaces out and sorta forgets to send data for a while, which
\ we have no control over.  We debounce the readings.
                'T' chk[
                mdonline? if mdbounces else -1 then
                mdonlines @ +  0max mdbounces min  dup
                mdonlines !  0<> to on-line?
                0 ]chk
                ;

: mdsourcename  ( -- )
\ append name of com source to temp$
                s" Slave "              temp$ +place
                port# (.d)              temp$ +place
                s" :"                   temp$ +place
                PacketAddr 0x7F and (.d) temp$ +place
                ;

: mdHwReset     ( -- )
                mdpad 0 1 formpacket
                ['] _MDsend catch drop MDlisten drop \ don't care about response
                250 ms ;

3                  value RTSctrl        \ RTS drops after transmission
FALSE              value CTSctrl        \ Don't pay attention to CTS
DTR_CONTROL_ENABLE value DTRctrl        \ DTR is always on

: _com[         ( -- )
                mdport# to port#
                ComInit 0= if ]com true Abort" Failed to open COM port!" then
                to com1Hndl  Reset: DCB
                CTSctrl Put: DCB.fOutxCtsFlow
                RTSctrl Put: DCB.fRtsControl
                DTRctrl Put: DCB.fDtrControl
                MyCBR 8 NOPARITY ONESTOPBIT com1Hndl ComSetup
                com1Hndl ComTimeouts ;

defer com[  ' _com[ is com[

: RS485         ( -- )                          \ set handshake for RS485 dongle
                3  to RTSctrl                   \ RTS drops after transmission
                FALSE to CTSctrl                \ Don't pay attention to CTS
                DTR_CONTROL_ENABLE to DTRctrl   \ DTR is always on
                ['] _com[ is com[
                ['] _]com is ]com ;

: MODEM         ( -- )                          \ set handshake for MODEM
                RTS_CONTROL_ENABLE to RTSctrl   \ RTS is always on
                FALSE to CTSctrl                \ Don't pay attention to CTS
                DTR_CONTROL_ENABLE to DTRctrl   \ DTR is always on
                ['] noop is com[
                ['] noop is ]com ;

: mdstartup     ( -- )
                com[ mdonline? mdonline? or
                dup to on-line? 0=
        if      begin   cr
                        ." Slave not found at address " PacketAddr (.d) type
                        ." . Scan all addresses (Y/n)?" key upc 'Y' =
                while   0xF9 0x80
                        do      i to PacketAddr
                                mdonline? mdonline? or
                                if      ."  MDA=" i 0x7F and (.d) type
                                        unloop exit
                                then
                        loop
                repeat
                ]com
        then    ;

: mdacquire     ( -- )
                mdstartup on-line?
        if      bumpPacketID getenv  5  \ try loading multiple times
                begin   headergood? 0=  \ (working around wierdness)
                        over and        \ load environment parameters
                        bumpPacketID
                while   1-  250 ms  test-comm getenv
                        '.' emit
                repeat  0=
                if      cr ." Invalid target environment information"
                then
        else    cr ." Connection attempt aborted."
        then    ;

: commo=multidrop ( -- )
\ Direct communications to the local serial port using multidrop protocol.
        ['] mdtestcomm       is test-comm
        ['] mdsourcename     is comsourcename
        ['] _connected?      is connected?
        ['] mdacquire        is open-comm
        ['] ]com             is close-comm
        ['] mdecho           is transceive
        ['] mdHwReset        is TarReset
        ['] mdcmsg-nc        is cmsg-nc
        ['] close-comm       is comm-bye
        ['] noop             is comm-pacify
        ;

: modem"        ( string" -- )
        '"' word count Com1Hndl write-file drop ;

\ ==============================================================================
dlport0? 0= [if] \s [then]      \ not including port DLL

\ Bit handshing using AVR programming cable

winlibrary dlportio.dll  \ Port access DLL courtesy of www.sstnet.com

: pc@   ( addr -- n ) Call DlPortReadPortUchar ;
: pc!   ( n addr -- ) Call DlPortWritePortUchar drop ;

defer xfer_byte ( n1 -- n2 )    \ transfer byte between PC and target board

hex
378 value PDA  \ bit 7 = sck
379 value PIN  \ bit 6 = ack = ~miso
37A value PSR  \ bit 2 = ~reset, bit 1 = ~mosi

: AVRreset      ( -- )                          \ blip reset line
                02 PSR pc!  100 ms              \ reset on
                06 PSR pc! ;                    \ reset off

: xbit          ( flag -- bit ) { \ msec -- }   \ transfer a bit
\ AllowableMS milliseconds allowed for the bit transfer before error
                ms@ AllowableMS + to msec
                0<> 80 and PDA pc!              \ set up SCK (outgoing bit)
                04 psr pc!                      \ set MOSI
        begin   PIN pc@ 40 and 0=               \ wait for response (MISO)
        while   ms@ msec > if 4 throw then
        repeat  80 PDA pc!                      \ float SCK line
\ Once we release the SCK line, it should be pulled high by a resistor.
\ A 5K resistor pulling up a 200pF load takes 1usec, which is fast enough to
\ keep up with the quick turnaround here.
                PIN pc@ 80 and 0= 1 and         \ read SCK line
                06 psr pc!                      \ clear MOSI
        begin   PIN pc@ 40 and                  \ wait for response
        while   ms@ msec > if 4 throw then
        repeat  ;

: xbyte0        ( input -- output )
\ Transfer one byte: send a start bit and eight data bits. Once in a while
\ we'll send a run of stop bits to guarantee framing.
    8 0 do      2* dup 200 and xbit or
        loop    dup    100 and xbit drop
                0FF and ;

: xbyte         ( input -- output ) ['] xbyte0 catch if drop 0x100 then ;
\ transfer a byte, return 0x100 if error.


: xclear        ( -- )             \ send a run of '1' bits to establish framing
                6 psr pc! 80 pda pc!
           10 0 do  1 ['] xbit catch nip
                    if leave then
                loop ;

' xbyte is xfer_byte

: xtransceive   { addr len \ xpad -- }
\ transfer data to/from target board using this buffer area.
\ xfer_byte returns data that's delayed by one byte, so we discard the 1st byte
\ and fetch an extra byte at the end.
                100 localalloc to xpad
    len if      \ addr len dump
                addr len bounds
                do      i c@ xfer_byte
                        i addr - xpad + c!

                loop
                0FF xfer_byte                   \ load a do-nothing command
                xpad len + c!                   \ store last byte
                xpad 1+ addr len move           \ update array
                \ addr len dump
    then        ;


: xcomsourcename1 ( -- ) s" SPI=LPT1" temp$ +place ;
: xcomsourcename2 ( -- ) s" SPI=LPT2" temp$ +place ;

: par1          ( -- ) 378 to PDA  379 to PIN  37A to PSR   \ select LPT1
                       ['] xcomsourcename1  is comsourcename ;
: par2          ( -- ) 278 to PDA  279 to PIN  27A to PSR   \ select LPT2
                       ['] xcomsourcename2  is comsourcename ;
: door=avr      ( -- ) ['] xbyte is xfer_byte ;

: xtestcom      ( -- f )
\ test communication link: T if communications is OK
                xclear
                0xFC xfer_byte 0x100 = if 0 exit then \ no comm
                0xFE xfer_byte 0xFF =
                0xFF xfer_byte 0xFF = and ;

: xtest-comm    ( -- )
\ see if the target is communicating, update on-line? status
                port#
        if      100 to allowableMS        \ ms before time-out
                xtestcom to on-line?      \ result of the test
                timelimit-default to allowableMS \ default time-out
        then    ;

: xAcquireTarget ( -- )
\ look on both parallel ports for a device, set port# if found.
\ start communications, load environment parameters.
\ sets port# = 0 if no target device is available
                100 to allowableMS               \ ms before time-out
                0 to port#
                false to on-line?
                s"  Seeking Target " MessageText: seekmsg
                true ontop: seekmsg
                start: seekmsg
                par1 xtestcom
                if      true to on-line?         \ result of the test
                        1 to port#
                else    par1 xtestcom   \ maybe the first one didn't take.
                        if      true to on-line?         \ try again
                                1 to port#
                        else    par2 xtestcom
                        if      true to on-line?
                                2 to port#
                        then
                        then
                then
                <e+> port#
        if      50 ms test-comm
                getenv  5               \ try loading multiple times
                begin   headergood? 0=  \ (working around wierdness)
                        over and        \ load environment parameters
                while   1-  250 ms  test-comm getenv
                        '.' emit
                repeat  0=
                if      cr ." Invalid target environment information"
                else    <e->
                        cr ." SPI connection established on LPT" port# .
                then
        else    cr ." No target found."
        then    <e->
                close: seekmsg
                ;

: Xdisconnect   ( -- )
                0 to port#
                false to on-line? ;


\ =============================================================================
\ AVR programming utility

2variable slowness
hex
\ parallel port interface to AVR ---------------------------------------------
\ hardware binding uses these words: par1 par2 res+ res- xfer bumpclk
\ mosi=feed reset=init sck=d7 miso=ack
\ 378 value PDA
\ 379 value PIN  \ bit 6 = ack
\ 37A value PSR  \ bit 2 = reset, bit 1 = mosi
\ : par1  ( -- ) 378 to PDA  379 to PIN  37A to PSR ; \ select LPT1
\ : par2  ( -- ) 278 to PDA  279 to PIN  27A to PSR ; \ select LPT2
: pclock ( f --) over 80 and if 0 else 2 then   \ set MOSI
                PSR pc! PDA pc!                 \ set SCK
                slowness 2@
        begin   1. d- dup 0<
        until   2drop ;
: res+  ( -- )  0 PDA pc!                       \ pull reset low
                2 PSR pc! 64 ms ;
: res-  ( -- )  0 PDA pc!
                6 PSR pc! ;

\ cable testing:
\  4 PSR pc! raises MOSI
\  6 PSR pc! lowers MOSI
\ 80 PDA pc! raises SCK
\  0 PDA pc! lowers SCK

: pbit  ( n -- n' )
\ transfer a bit: read bit, shift out next bit
        80 pclock
        2* PIN pc@ 40 and 0<> 1 and or
        0 pclock ;
: xfer  ( n -- n' )     \ transfer a byte over the SPI
        0 pclock
        pbit pbit pbit pbit pbit pbit pbit pbit
        0FF and ;
: bumpclk ( -- )       \ add an extra clock
        0 80 pclock 0 pclock drop ;
\ ----------------------------------------------------------------------------

: SLOWER ( -- )                         \ double calibration delay
        slowness 2@ d2* slowness 2! ;
: noreset ( - )  par1 res- par2 res- ;  \ de-assert reset on both ports
: m!    ( n -- ) xfer drop ;            \ send byte out the SPI
: sig   ( byte# -- c ) 030 m! 0 m! m! 0 xfer ;        \ read signature byte
: erase_chip ( -- ) 0AC m! 80 m! 0 m! 0 m! 90 ms ;    \ erase chip
: setlock ( -- ) 0AC m! 0E0 m! 0 m! 0 m! 90 ms ;      \ set both lock bits
: nosig? ( -- f ) 0 sig 01E <> ;                      \ T if no Atmel signature

: pen?  ( -- f )                        \ send programming enable command
        0AC m! 53 m! 0 xfer 0 m! 53 = ;

: solid? ( -- f )                       \ look for solid prog-enable
     64 begin   1- dup                  \ want 100 consecutive good reads
        while   pen? 0= if drop 0 exit then
        repeat  drop true ;

: connect ( -- f )                      \ synchronize command stream
     20 begin   1- ?dup
        while   solid? ?exit bumpclk    \ good "enable programming" command
        repeat  false ;                 \ return 0 if no good

: calibrate { \ msec -- f }             \ establish the fastest connection
\ f=true if connection was made
        8. slowness 2!                  \ that works reliably
        par1 res+ par2 res+             \ reset AVR
        ms@ 400 + to msec
        begin   par1 connect if slower solid? exit then slower
                par2 connect if slower solid? exit then
                key? if key 01B =       \ ESC or timeout terminates
                else ms@ msec >         \ allow 2 seconds
                then
        until   false ;

: avradr ( addr -- low hi bit# ) 2 /mod byte-split rot 3 lshift ;
: prog@  ( addr -- c) avradr 20 or m! m! m! 0 xfer ;    \ fetch from flash
: ee@    ( addr -- c) byte-split 0A0 m! m! m! 0 xfer ;  \ fetch from EEPROM

: FLsize ( -- n )   1 sig               \ flash size based on signature
        case    93 of 2000 endof        \ 8K
                92 of 1000 endof        \ 4K
                91 of 0800 endof        \ 2K
                0                       \ unknown
        endcase ;
: EEsize ( -- n )   1 sig               \ EEPROM size based on signature
        case    93 of 200 endof         \ 512
                92 of 100 endof         \ 256
                91 of 080 endof         \ 128
                0                       \ unknown
        endcase ;

: pgmbyte ( c addr -- )  { \ msec -- }
        ms@ 64 + to msec
        over 0FF = if 2drop exit then           \ no need to program 0FF
        dup avradr 40 or m! m! m! over m!       \ program it
        over 07F = if 32 ms 2drop exit then     \ not pollable, allow 50ms
        begin   2dup prog@ <>                   \ wait
        while   ms@ msec > if 2drop exit then
        repeat  2drop ;

: pgmEE ( c addr -- )
        over 0FF = if 2drop exit then           \ no need to program 0FF
        byte-split 0C0 m! m! m! m! 32 ms ;      \ program it, allow 50ms

: programAVRflash { addr dest len \ msec -- }
\                addr . dest . len . cr
                Xdisconnect   \ make sure we're not trying to talk to the target
                0 to tarerror?
                calibrate <e+>
        if      true sending: progress          \ start progress window
                Start: progress
                erase_chip
                FLsize len dest + <
                if FLsize
                   cr ." WARNING: AVR flash is too small."
                   cr ." Image is " len . ." bytes, AVR is " dup . ." bytes."
                then
                ms@ 64 + to msec
                addr len bounds
                ?do     i c@ i addr - dest + pgmbyte   \ program flash bytes
                        ms@ msec >
                        if      i addr - len update: progress
                                ms@ 64 + to msec
                        then
                        tarerror?
                        if      cr ." AVR programming aborted"
                                0 to len leave
                        then
                loop
                Close: progress
            len if      false sending: progress          \ reading to verify
                        Start: progress
                addr len bounds
                ?do     i c@ i addr - dest + prog@ <>    \ verify flash bytes
                        if      cr ." Programming failure at " i addr - .
                                leave
                        then
                        ms@ msec >
                        if      i addr - len update: progress
                                ms@ 32 + to msec
                        then
                loop
                        Close: progress
                then
        else    cr ." AVR not found"
        then    80 pda pc! noreset <e-> ;

defer progflash  ( src dest len -- )
' programAVRflash is progflash

: commo=avr     ( -- )
\ Handshaked serial bit interface via parallel port, AVR format.
        door=avr
        ['] xtest-comm       is test-comm
        ['] xcomsourcename1  is comsourcename
        ['] _connected?      is connected?
        ['] xAcquireTarget   is open-comm
        ['] Xdisconnect      is close-comm
        ['] xtransceive      is transceive
        ['] AVRreset         is TarReset
        ['] scmsg-nc         is cmsg-nc
        ['] noop             is comm-bye
        ['] noop             is comm-pacify
        ['] programAVRflash  is progflash
        ;
decimal

