
\ Server for target board communications over the Internet.

\ Tools/buttons:
\ Connect-to-serial       Trys to talk to target board
\ Disconnect-serial       Stop trying to talk to target board
\ Reset                   Reset target board by dropping CTS and DTR lines
\ Exit                    Quit program

\ If the active client goes away for too long, the socket is disconnected
\ so a new user can get access.

\ Incoming labels:
\        73 = Evaluate debugger string
\        74 = Reset target board
\        75 = Send text message to window and log file
\        76 = Log off
\        77 = test on-line status

\ Outgoing labels:
\        0 = acknowledge
\        1 = unknown command
\        2 = target environment header
\        3 = debugger data
\        4 = online/offline status

 anew yourhost

1 constant turnkey?
10000 value ptimeout            \ 10 second timeout
423 value netport

: fname s" FFHOST.LOG" ;        \ name of log file

\ x y w h
  6 constant buttons            \ # of buttons across window
 70 constant bw                 \ button dimensions
 30 constant bh
  4 constant b-separator        \ space between buttons
: bx    ( n -- x )      bw b-separator + * b-separator + ; \ button x position
: by    ( n -- y )      drop  66 ;
: butn  ( n -- x y w h ) dup>r bx r> by bw bh ;         \ button param list

: winwidth ( -- pels )  buttons bx ;

include bradstuf.g              \ some useful primitives

needs sockets.f

0 value client-addr             \ IP address of client   0 = no client connected
variable hits                   \ # of packets within the last few seconds
variable hsmark

\ LOG FILE --------------------------------------------------------------------

variable logmiss
0 value outfile
create $crlf 2 c, 13 c, 10 c,

: (.2+)         ( n -- )        0 <# # # #> temp$ +place ;
: +lg           ( a n -- )      temp$ +place ;
: timestamp     ( -- a n )
                time&date temp$ off (.) +lg s" ." +lg   \ year
                (.2+) s" ." +lg  (.2+) s"  " +lg        \ mo day
                (.2+) s" :" +lg  (.2+) s" :" +lg  (.2+) \ time
                s"  " +lg  temp$ count ;

: log           ( addr len -- )
\ Open the log file, append the string, and close the log file.
                logmiss off
        begin   fname r/w open-file   swap to outfile   \ look for log file
        while   fname r/w create-file swap to outfile   \ create the file
                if 200 error then
                logmiss incr  logmiss @ 1 >  if 201 error then
                outfile close-file           if 202 error then
        repeat  outfile file-append  if 203 error then  \ set up to append to file
      timestamp outfile write-file   if 204 error then  \ time stamp
                outfile write-file   if 204 error then  \ string
    $crlf count outfile write-file   if 204 error then  \ EOL
                outfile close-file   if 205 error then
                ;



\ #############################################################################

:OBJECT HostWin <SUPER DialogWindow

StaticControl w_UserIP          \ a window for user ID
StaticControl w_UserName        \ a window for user name

ButtonControl b_reset           \ reset target board
ButtonControl b_quit            \ quit
ButtonControl b_net             \ start a new connection
ButtonControl b_hangup          \ hang up on the current connection
ButtonControl b_connect         \ try to talk to target board
ButtonControl b_disconnect      \ disconnect from target board

:M ExWindowStyle: ( -- style )
                ExWindowStyle: SUPER
                ;M

:M WindowStyle: ( -- style )
                WindowStyle: SUPER
                WS_BORDER OR
                WS_OVERLAPPED OR
                ;M

:M WindowTitle: ( -- title )
                z" FFhost Starting" ;M

:M StartSize:   ( -- width height )
                winwidth 100 ;M

:M StartPos:    ( -- x y )
                100 10 ;M

:M .IP:         ( IP -- )       \ display IP address in window
           ?dup if      s" IP client = "         pad  place
                        (.ip)                    pad +place
                else    s" Waiting for Connection Attempt" pad  place
                then    pad count SetText: w_UserIP ;M

:M .msg:        ( a n -- )  SetText: w_UserIP ;M
:M .msg1:       ( a n -- )  SetText: w_UserName ;M

:M On_Init:     ( -- )
                On_Init: super
                self               Start: w_UserIP \ start up static text
                                GetStyle: w_UserIP \ get the default style
                WS_GROUP     OR                    \ End a group
                SS_CENTER    OR                    \ and centering
                WS_BORDER    OR
                                SetStyle: w_UserIP \ and border to style
                b-separator b-separator winwidth b-separator 2* - 25
                                    Move: w_UserIP \ position the window
                s" Attempting to Use Winsock" SetText: w_UserIP

                self               Start: w_UserName \ start up static text
                                GetStyle: w_UserName \ get the default style
                WS_GROUP     OR                    \ End a group
                SS_CENTER    OR                    \ and centering
                WS_BORDER    OR
                                SetStyle: w_UserName \ and border to style
                b-separator b-separator 30 + winwidth b-separator 2* - 25
                                    Move: w_UserName \
                s"  "            SetText: w_UserName

                'C'                SetID: b_connect
                self               Start: b_connect
                0 butn              Move: b_connect
                s" &Plug"        SetText: b_connect
                'D'                SetID: b_disconnect
                self               Start: b_disconnect
                1 butn              Move: b_disconnect
                s" &Unplug"      SetText: b_disconnect
                'R'                SetID: b_reset
                self               Start: b_reset
                2 butn              Move: b_reset
                s" &Reset"       SetText: b_reset
                'N'                SetID: b_net
                self               Start: b_net
                3 butn              Move: b_net
                s" &Net"         SetText: b_net
                'H'                SetID: b_hangup
                self               Start: b_hangup
                4 butn              Move: b_hangup
                s" &Hangup"      SetText: b_hangup
                'Q'                SetID: b_quit
                self               Start: b_quit
                5 butn              Move: b_quit
                s" &Quit"        SetText: b_quit
                                GetStyle: b_quit
                BS_DEFPUSHBUTTON OR
                                SetStyle: b_quit
                ;M

:M On_Paint:    ( -- )          \ screen redraw procedure
                0 0 winwidth 100 LTGRAY FillArea: dc
                ;M

:M WM_COMMAND  ( hwnd msg wparam lparam -- res )
                over LOWORD ( ID ) pushkey  \ fake a keystroke
                0 ;M

:M WM_CLOSE     ( h m w l -- res )
                WM_CLOSE WM: Super
                bye
                0 ;M

;OBJECT

include hserial.f               \ communications to the target

variable errors

: error         ( n -- )
                cr ." Error " .  errors incr
                ."  Quit? Y/n:" key upc 'N' <> if bye then
                ;

create inpacket    512 allot    \ incoming packet
create outpacket   512 allot    \ outgoing packet
0 value packetlen               \ length of outgoing packet

create IP-from      16 allot
create packets     256 allot
variable msmark                 \ time-out time marker
0 value s-host                  \ socket# of listening socket
0 value s-client                \ socket# of the client

: in_cmd        ( -- c )        inpacket c@ ;   \ incoming command
: clrtimer      ( -- )          ms@ msmark ! ;  \ reset timeout timer

: SendPacket    ( addr len label -- )
\ send a packet to the client the supplied 1-byte label
                outpacket 1 lay c!              \ label is the header
                outpacket +place                \ outpacket is data to send
                outpacket count s-client WriteSocket if 12 throw then ;

: packack       ( -- )                          \ acknowledge a command
                s" " 0 SendPacket ;

: packabort     ( -- )                          \ send "aborting" signal
                s" " 123 SendPacket ;

: targeteval    ( -- )                          \ evaluate debugger string
                inpacket packetlen 1 /string    ( a len )
                >r temp$ r@ move
                temp$ r@ transceive
                temp$ r> 3 SendPacket ;

: logoff        ( -- )
                s" Logoff command received" log
                '*' pushkey 0 0 2 SendPacket ;

\ ------------------------------------------------------------------------------

: winmessage    ( -- )                          \ display local message from client
                inpacket packetlen 2 /string    \ to window and log file
                2dup log .msg1: HostWin ;

: .hits         ( -- )                          \ display packets per second
                s" FFhost PPS=" temp$  place
                hits @ (.)  temp$ +place
                temp$ count SetTitle: HostWin ;

: showcomm      ( -- )
                test-comm on-line?
                if .comopened else s" off-line" .msg1: HostWin then ;

variable finished
variable onnet
defer timedelay

: testkey       ( f -- f' )          \ f = T if break
           key? if      key upc
                        case    'H' of  drop 1 onnet off  packabort     endof
                                'N' of  drop 1 onnet on  10 timedelay   endof
                                '*' of  drop 1 onnet on                 endof
                                'Q' of  drop 1 packabort  finished on   endof
                                'R' of  HwReset 0 to on-line?
                                        s" Hardware reset" log          endof
                                'C' of  open-comm                       endof
                                'D' of  close-comm
                                 s" Target disconnected" .msg1: HostWin endof
                        endcase
                then    ;

:noname         ( n -- )                        \ count down to 1
                s" Please minimize this window." .msg: HostWin 
          dup 0 ?do     dup i - (.) SetTitle: HostWin
                        20 0 do winpause 50 ms loop
                   key? if      key upc 'Q' = if bye then leave
                        then
                loop    drop
                ; is timedelay

: communicate   ( -- <throwval> )
\ in this loop, we process incoming packets until a keypress gets us out.
\ a TCP/IP error THROWS this.
        begin   s-client ToRead  if 10 throw then       \ 10: ToRead error
                if      inpacket 512 s-client ReadSocket
                        if 11 throw then                \ 11: ReadSocket error
                        to packetlen   in_cmd
                        case    73 of targeteval                endof
                                74 of 'R' pushkey  packack      endof
                                75 of winmessage packack        endof
                                76 of logoff                    endof
                                77 of test-comm on-line? pad c!
                                      pad 1 4 SendPacket        endof
                                s" " 1 SendPacket       \ invalid command
                        endcase
                        ClrTimer  hits incr
                        0
                else    ms@ msmark @ - ptimeout >     \ connection died?
                then    ( f:finished? )
                ms@ hsmark @ - 1000 >                 \ show packets per second
                if      .hits  hits off               \ in the title bar
                        ms@ hsmark !                  \ once a second
                        showcomm
                then
                testkey
        until   ;


: host-remote   ( -- )
                0 to client-addr
                0 .ip: HostWin
                IP-from 16 s-host socket-accept swap to s-client
\ note: socket-accept locks up task while waiting. Thanks, Bill.
\ Minimize the window before socket-accept is called if you plan to do other
\ thinks with the host PC while the server is running.
                ?dup if s" Socket-Accept Error" log onnet off exit then
                IP-from cell+ @ to client-addr  \ connected!
                client-addr .ip: HostWin
                s" Connection to "      pad place
                client-addr (.ip)       pad +place
                s"  = "                 pad +place
                client-addr GetHostName
                if s" GetHostName Error" log onnet off 2drop exit then pad +place
                pad count log
        begin
                ClrTimer ms@ hsmark !
                ['] communicate catch
                dup 1 =
        if
                s" Target board timed out, Resetting." 2dup log .msg: HostWin
                HwReset 0 to on-line?   \ comm time-out, reset the target board
                100                     \ wait 10 seconds for the board to come back 
                begin   ?dup on-line? 0= and
                while   1- 100 ms test-comm
                repeat
                on-line? 0=
                if      s" Reset failure, Disconnected." log
                        exit
                then    packack
                client-addr .ip: HostWin
        else    client-addr (.ip) pad place
                s"  Client Disconnected. " pad +place
           ?dup if      s" Error " pad +place (.) pad +place
                then
                pad count log
                exit
        then
        again   ;

: HOST          ( -- )  \ ----------------------------------------------------
                s" Server opened" log
                onnet off
                Start: HostWin
                SocketsStartup
           if   true  s" Error starting sockets" .msg1: HostWin
           else 50 ms my-ip-addr
                GetHostName nip nip       ( f )
           then
   if           s" Can't talk to ISP" 2dup log .Msg: HostWin  beep
                5 seconds
   else
\ we have internet access, set up to listen for a client
                s" Please Plug and Net" .msg: HostWin
                errors off
                s" My IP address is "   pad  place
                my-ip-addr (.ip)        pad +place
                pad count 2dup log .msg1: HostWin
                CreateSocket         if 100 error then to s-host
                netport s-host BindSocket if 101 error then
                s-host ListenSocket  if 102 error then

                finished off
        begin   onnet @
           if   s" FFhost Waiting for Client"   SetTitle: HostWin
                host-remote
           else s" FFhost OFF NET"              SetTitle: HostWin
                begin   port# if showcomm then
                        100 ms
                        0 testkey
                until
           then
                finished @
        until
                s-host CloseSocket if 104 error then
                SocketsCleanup     if 105 error then
   then         close-comm
                s" Server closed" log
                Close: HostWin
                bye ;

: release-buffers ( -- )
                DestroyWindow: HostWin  ;

unload-chain chain-add-before release-buffers

turnkey? [IF]
' host turnkey host       \ build an application on disk
cr .(  Moving HOST.EXE to parent directory)
copyfile HOST.EXE ..\ s" HOST.EXE" delete-file drop
nt? 0= [if]
copyfile HOST.IMG ..\ s" HOST.IMG" delete-file drop
[then]
3 pause-seconds
[THEN]
bye

