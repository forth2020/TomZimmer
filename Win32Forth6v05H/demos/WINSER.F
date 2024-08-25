\ WINSER.F           A Simple Serial Communications Program
\ Written by Andrew McKewan in Visual C++
\ Translated to Forth by Tom Zimmer
\ January 3nd, 1997 tjz
\       Updated to include changes from Skip Inskeep, as well as
\       other changes.
\ February 10th, 1997 by Tom Zimmer
\       Updated with a couple of additional changes from Skip Inskeep.

\ A Win32Forth example program that illustrates how to open COM1 for
\ bi-directional communications and then allows a simple terminal
\ program to communicate with whatever is connected at 9600 baud.

\ define the object for the Device Control Block structure

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

: ComTimeouts   { cHndl \ CT -- }      \ Initialize the communications timeouts
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

: ComOpen       ( z1 -- cHndl ) \ Open Com port for z" COM1", or z" COM2"
                >R
                NULL                            \ no template
                NULL                            \ open file attributes
                OPEN_EXISTING                   \ creation distribution
                NULL                            \ no security attributes
                0                               \ exclusive access
                GENERIC_READ GENERIC_WRITE or   \ desired access modes
                R> rel>abs                      \ zstring filename
                Call CreateFile                 \ returns handle or -1
                dup -1 =                        \ if -1 then error
                Abort" Failed to open COM port!"
                ;                               \ -- chndl ;return handle to port

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
                comhndl Call ClearCommError 0<>  ;

\ Example: Simple terminal program

0  value Com1Hndl
variable Com1key-val            \ a place to save last key received
0  value Com1key-flg?           \ have we already got a key?

: Com1key?      ( -- c1 )       \ get a key from serial port, don't wait long
                Com1key-flg? 0=
                if      Com1key-val 1 Com1Hndl read-file        \ -- len flag
                        abort" Failed to read serial port"      \ -- len
                        to Com1key-flg?
                then    Com1key-flg? ;

: Com1key       ( -- c1 )       \ must return a key
                begin   Com1key?                \ loop till we get one
                until
                Com1key-val c@                  \ return the key
                0 to Com1key-flg? ;             \ clear the save buffer

: Com1emit      { char -- }     \ write a character to the serial port
                &LOCAL char 1 Com1Hndl write-file
                abort" Failed to write serial port" ;

: do-Com1key    ( c1 -- f1 )    \ process keys pressed on user keyboard
                dup k_ESC =                     \ was ESC pressed?
                if      drop                    \ if so, discard c1 and shut down
                        Com1Hndl ComClose       \ close the com port
                        0 to Com1Hndl           \ clear handle value
                        ." Done"          TRUE  \ return TRUE, we are terminating
                else    Com1emit          FALSE \ else emit char and continue
                then    ;

: Com1talk      ( -- )          \ simple terminal talker to the serial port
                cr ." Initializing Serial Port to: "
                cr ." COM1: baud=9600 parity=N data=8 stop=1"
                z" COM1" ComOpen to com1Hndl
                    Reset: DCB
                FALSE Put: DCB.fOutxCtsFlow      \ Don't pay attention to CTS
                CBR_9600 8 NOPARITY ONESTOPBIT com1Hndl ComSetup
                com1Hndl ComTimeouts
                cr ." Press ESC to stop talking." cr
                BEGIN   key?
                        if      key do-Com1key ?EXIT
                        then
                        Com1key?
                        if      Com1key emit
                        then
                AGAIN ;


\S Example2: Simple terminal program for COM2, watches CTS

0  value Com2Hndl
variable Com2key-val            \ a place to save last key received
0  value Com2key-flg?           \ have we already got a key?

: Com2key?      ( -- c1 )       \ get a key from serial port, don't wait long
                Com2key-flg? 0=
                if      Com2key-val 1 Com2Hndl read-file        \ -- len flag
                        abort" Failed to read serial port"      \ -- len
                        to Com2key-flg?
                then    Com2key-flg? ;

: Com2key       ( -- c1 )       \ must return a key
                begin   Com2key?                \ loop till we get one
                until
                Com2key-val c@                  \ return the key
                0 to Com2key-flg? ;             \ clear the save buffer

: Com2emit      { char -- }     \ write a character to the serial port
                begin
                     &LOCAL char 1 Com2Hndl write-file
                     if
                        Com2Hndl ComErrorClear drop
                        Get: COMSTAT.lpComStatBits $7E AND \ fail on all but CTS
                        abort" Failed to write serial port"
                        false
                     else
                        true
                     then
                until  ;

: do-Com2key    ( c1 -- f1 )    \ process keys pressed on user keyboard
                dup k_ESC =                    \ was ESC pressed?
                if      drop                   \ if so, discard c1 and shut down
                        Com2Hndl ComClose      \ close the com port
                        0 to Com2Hndl          \ clear handle value
                        ." Done"
                                        TRUE   \ return TRUE, we are terminating
                else    Com2emit        FALSE  \ else emit char and continue
                then    ;

: Com2init      ( -- )          \ simple terminal talker to the serial port
                cr ." Initializing Serial Port to: "
                cr ." COM2: baud=19200 parity=N data=8 stop=1"
                z" COM2" ComOpen to Com2Hndl
                   Reset: DCB
                TRUE Put: DCB.fOutxCtsFlow      \ pay attention to CTS
                CBR_19200 8 NOPARITY ONESTOPBIT Com2Hndl ComSetup
                Com2Hndl ComTimeouts ;

: Com2talk      ( -- )          \ simple terminal talker to the serial port
                Com2init
                cr ." Press ESC to stop talking." cr
                BEGIN   key?
                        if      key do-Com2key ?EXIT
                        then
                        Com2key?
                        if      Com2key emit
                        then
                AGAIN ;

