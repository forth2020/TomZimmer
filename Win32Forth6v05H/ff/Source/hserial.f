\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ \\\\\\\\        Serial communication interface         \\\\\\\\\\\\
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

\ This is basically the file COMMO.G with some stuff ripped out.

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

defer ConnectToTarget      \ Connect to target board
defer getenv
  0 value tarerror?      \ T if a comm error occured during the transfer
  0 value tarprogress?   \ T if a progress window is desired
10000 value allowableMS       
10000 value timelimit-default \ allow 10 seconds for target to stare into space

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

\ --- BuadRate    CBR_110       CBR_300       CBR_600
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
                s" Target @ COM" pad place
                port# (.) pad +place  s" at " pad +place
                baudrate (.d) pad +place s"  bps" pad +place
                pad count .msg1: HostWin ;

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
                100 to allowableMS 
                DisconnectTarget  1 to keyfudge
                s"  Seeking Target " MessageText: seekmsg
                true ontop: seekmsg
                start: seekmsg
                COMseek                  \ find the device
                port# 0= if COMseekLow then     \ try low baud rates if not found
                port#
        if      50 ms test-comm
                getenv  5               \ try loading multiple times
                begin   headergood? 0=  \ (working around wierdness)
                        over and        \ load environment parameters
                while   1-  250 ms  test-comm getenv
                repeat  0=
                if      s" Invalid target environment information" .msg1: HostWin
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
        else    s" No Target Found" .msg1: HostWin
        then    timelimit-default to allowableMS
                close: seekmsg
                ;

' AcquireTarget is open-comm

:noname         ( -- f )
\ determine whether communications is possible
                connected? 0=         \ port not yet selected, try to find
                if      open-comm     \ open comm
                then    on-line?      \ return on-line status
                winpause              \ let Windows breathe
                ; is communicable?

8 value cellbits     4 value addrnibbles
8 value charbits
: charsize      ( -- n )        charbits 7 + 3 rshift ;  \ in bytes
: cellsize      ( -- n )        cellbits 7 + 3 rshift ;
: cellnibbles   ( -- n )        cellbits 3 + 2 rshift ;
: bounds>na    ( lo hi -- n a ) over - 1+ cellsize 0 swap um/mod nip swap ;


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
                 4 to addrnibbles
                envheader envheadersize b.@env fill
                envheader envheadersize stransceive     \ load byte-size stuff
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
                ?do     commpad cellsize b.@env fill   \ load cell-size stuff
                        commpad cellsize stransceive
                        0 commpad cellsize bounds
                        ?do     8 lshift i c@ +
                        loop
                        envparams i th !
                loop
        then    ; is getenv


ONLY FORTH ALSO DEFINITIONS

: commo=serial  ( -- )
\ Direct communications to the local serial ports.
\        ['] _test-comm       is test-comm
        ['] _comsourcename   is comsourcename
        ['] _connected?      is connected?
        ['] AcquireTarget    is open-comm
        ['] DisconnectTarget is close-comm
        ['] _transceive      is transceive
        ;

: _HwReset      { cHndl state -- }
                DCB.AddrOf rel>abs cHndl
                Call GetCommState ?win-error
                  state  Put: DCB.fDtrControl
                DCB.AddrOf rel>abs cHndl                        
                Call SetCommState ?win-error  ;                 

: HwReset       ( -- )
                on-line?
        if      comHndl  DTR_CONTROL_DISABLE  _HwReset
                1000 ms
                comHndl  DTR_CONTROL_ENABLE   _HwReset
        else    beep
        then
                ;



