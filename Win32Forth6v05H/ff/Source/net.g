\ -----------------------------------------------------------------------
\ Internet connection \ May 16th, 2000 - 10:10  Brad Eckert
needs sockets.f       \ Winsock support by Andrey Cherezov & Mihail 0. Maksimov

10500 value packet-timeout      \ elapsed time before we assume packet got lost
423 value netport               \ server uses this port

: ncmsg-nc  ( -- ) s" Server Not Connected"   temp$ +place ;

: ip:           ( <number> -- IP_addr ) \ format: nn.nn.nn.nn
                base @ decimal
                0 bl parse      \ ip addr len
                4 0
        do      0 0 2swap >number 1 /string 2>r drop ( ip # | xx )
                dup 0xFF > abort" Please use IP format NN.NN.NN.NN where NN=0..255"
                >r 8 rshift r>
                0x18 lshift or              2r>      ( ip' addr len )
        loop    2drop  >r
                base ! r> ;

ip: 209.36.34.214 value ip-address      \ default IP address of server

: ip=           ( <num> -- )    ip: to ip-address ;


260 value maxipad
create inpacket  maxipad allot  \ incoming packet data
create outpacket maxipad allot  \ outgoing packet data
0 value packetlen

0 value ip-sock
variable ipcpending

: in_cmd        ( -- c )        inpacket c@ ;   \ incoming command
: indata        ( -- a )        inpacket char+ ;
: ip?           ( -- )          ip-address (.ip) cr type ;

: ?endservice   ( f a n -- )
    rot if      ip-sock closesocket drop \ close if already connected
                0 to port#
                false to on-line?
                <e+>
                cr type ."  error.  Disconnecting from server"
                <e-> abort
        else    2drop
        then    ;


: service       ( addr len command -- )
\ send a packet, get a response in inpacket
                port#
        if      s" +"  outpacket  place
                >r     outpacket +place
                r>     outpacket char+ c!
                outpacket count ip-sock WriteSocket s" IP WriteSocket error" ?endservice
                ms@
          begin winpause
                ip-sock ToRead                      s" IP ToRead error" ?endservice
                if      inpacket maxipad ip-sock
                        readsocket                  s" IP ReadSocket error" ?endservice
                        to packetlen drop exit
                then
                dup ms@ - packet-timeout >
          until drop                           true s" IP connection went dead" ?endservice
                in_cmd 123 =
          if    <e+>
                cr type ." Disconnected by remote server"
                ipcpending on  0 to port#
                <e-> abort
          then
        else    3drop inpacket off
        then    ;

MsgWindow netmsg

: ip-close      ( -- )
                port#
                if      s"  Closing Connection " MessageText: netmsg
                        true ontop: netmsg
                        start: netmsg
                        s" " 76 service          \ issue log-off request
                        ip-sock closesocket drop \ close if already connected
                        0 to port#
                        false to on-line?
                        cr ." Logged off server"
                        200 ms
                        close: netmsg
                then    ;

: ip-close1     ( -- )        \ if we're on-line, wait for a good time to close
                on-line? if ipcpending on else ip-close then ;

variable paramtally

: ip-pacify     { \ mypad -- }
                40 localalloc: mypad
                paramtally incr
                s"  Verifying target "  mypad place
                paramtally @ (.)        mypad +place
                s"  "                   mypad +place
                mypad count MessageText: netmsg
                Paint: netmsg
                winpause ;

: ip-open       ( -- )
                paramtally off
                ip-close
                createsocket abort" Couldn't create socket" to ip-sock
                s"  Attempting Connection " MessageText: netmsg
                true ontop: netmsg
                start: netmsg
                ip-address netport ip-sock connectsocket ?dup
                if      cr ." Error " .
                        close: netmsg
                        true abort" Couldn't connect to server"
                then    true to port#           \ connected to server
                true to on-line?                \ say we're on-line
                s"  Connecting target " MessageText: netmsg
                Paint: netmsg
                winpause
                getenv  on-line?                \ bogus header clears on-line?
                close: netmsg
        if      cr ." Successful connection to server"
                200 ms 
        else    <e+>
                cr ." Invalid target configuration, try resetting target.
                <e->
        then    ;

: ip-reset      ( -- )
                port#
        if      s" " 74 service                 \ send reset command
                cr ." Target board reset"
        then    ;

: ip-transceive ( a n -- )
                2dup 73 service        \ get remote data
                indata -rot move ;

: ip-srvname    ( -- )
                base @ decimal
                ip-address (.ip) temp$ +place
                base ! ;

variable itctally

: ip-test-comm  ( -- )  \ update on-line? status
\ this is typically called 4 times a second while the socket is open
                itctally incr  itctally @ 1 and         \ test every other time
                drop true                               \ not really
                port# 0<> and              
        if      s" " 77 service         \ indata = status
                indata c@ 0<> to on-line?
        then    ipcpending @
        if      ip-close  ipcpending off        \ a safe time to close
        then    ;


: commo=ip
        SocketsStartup abort" Can't use sockets"
        ['] ip-srvname       is comsourcename
\        ['] _connected?      is connected?
        ['] ip-open          is open-comm
        ['] ip-close1        is close-comm
        ['] ip-transceive    is transceive
        ['] ip-reset         is TarReset
        ['] ip-TEST-COMM     is TEST-COMM
        ['] ncmsg-nc         is cmsg-nc
        ['] ip-close         is comm-bye
        ['] ip-pacify        is comm-pacify
        ;


((
Outgoing labels:
        73 = Evaluate debugger string
        74 = Reset target board
        75 = Send text message to window and log file
        76 = Send target environment header
        77 = test on-line status

Incoming labels:
        0 = acknowledge
        1 = unknown command
        2 = target environment header
        3 = debugger data
        4 = online/offline status
))


