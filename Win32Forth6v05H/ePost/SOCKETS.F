\  Windows Sockets of Andrey Cherezov

anew sockets.f

winlibrary wsock32.dll

1  CONSTANT SOCK_STREAM
-1 CONSTANT INVALID_SOCKET
-1 CONSTANT SOCKET_ERROR
2  CONSTANT PF_INET
2  CONSTANT AF_INET
6  CONSTANT IPPROTO_TCP

0
2 FIELD+ sin_family
2 FIELD+ sin_port
4 FIELD+ sin_addr
8 FIELD+ sin_zero
CONSTANT /sockaddr_in
CREATE sock_addr HERE /sockaddr_in DUP ALLOT ERASE
AF_INET sock_addr sin_family W!

: ASCIIZ>  100 2dup 0 scan nip - ;
: ztype ( z"a -- )
  ASCIIZ>   type ;

: CreateSocket ( -- socket ior )
  IPPROTO_TCP SOCK_STREAM PF_INET
  call socket DUP INVALID_SOCKET =
  IF call WSAGetLastError
  ELSE 0 THEN
;
: ToRead ( socket -- n ior )
  \ сколько байт можно сейчас прочесть из сокета
  \ можно использовать перед ReadSocket для того чтобы
  \ избежать блокирования при n=0
  0 >r
  rp@ REL>ABS [ HEX ] 4004667F [ DECIMAL ] ROT
  call ioctlsocket SOCKET_ERROR =
  IF r>drop 0 call WSAGetLastError ELSE r> 0 THEN
;
: ConnectSocket ( IP port socket -- ior )
  >R
  256 /MOD SWAP 256 * +
  sock_addr sin_port W!
  sock_addr sin_addr !
  /sockaddr_in sock_addr REL>ABS R> call connect SOCKET_ERROR =
  IF call WSAGetLastError ELSE 0 THEN
;
: CloseSocket ( s -- ior )
  call closesocket SOCKET_ERROR =
  IF call WSAGetLastError ELSE 0 THEN
;

: WriteSocket ( addr u s -- ior )
  >r 0 swap rot REL>ABS r>   \  0 u addr s
  call send SOCKET_ERROR =
  IF call WSAGetLastError ELSE 0 THEN
;
: SWrite ( addr u s -- wlen )
  >r 0 swap rot REL>ABS r>   \  0 u addr s
  call send
;
: WriteSocketLine ( addr u s -- ior )
  DUP >R WriteSocket ?DUP IF R> DROP EXIT THEN
  crlf$ COUNT R> WriteSocket
;
: WriteSocketCRLF ( s -- ior )
  HERE 0 ROT WriteSocketLine
;
: ReadSocket ( addr u s -- rlen ior )
   >r 0 swap rot REL>ABS r>   \  0 u addr s
  call recv DUP SOCKET_ERROR =
  IF call WSAGetLastError ELSE 0 THEN
  OVER 0= IF DROP -1002 THEN
  ( если принято 0, то обрыв соединения )
;
: SRead ( addr u s -- r )
  >r 0 swap rot REL>ABS r>   \  0 u addr s
  call recv
;
CODE a>r@      ( a1 -- n1 )
                mov     ebx, 0 [ebx]
                next    c;

: GetHostName ( IP -- addr u ior )
  >r PF_INET 4 rp@ REL>ABS call gethostbyaddr
  ?DUP IF A>R@ ABS>REL ASCIIZ> 0
       ELSE HERE 0 call WSAGetLastError
       THEN  r>drop
;
: Get.Host.Name ( addr u -- addr u ior )
  DROP REL>ABS call inet_addr GetHostName
;
: zGetHostIP ( z" -- IP ior )
  dup c@ [char] 0 [char] 9 between over and
  if rel>abs call inet_addr 0
  else dup if REL>ABS then call gethostbyname DUP
       IF  3 CELLS + A>R@ A>R@ A>R@ 0
       ELSE call WSAGetLastError
       THEN
  then
;

: my-ip-addr 0 zGetHostIP drop ;

: GetHostIP ( addr len -- IP ior )
  RP@ 265 - RP!
  RP@ 265 ERASE
  RP@ SWAP 265 UMIN CMOVE
  RP@ zGetHostIP
  RP@ 265 + RP!
;
CREATE sock_addr2 HERE /sockaddr_in DUP ALLOT ERASE
AF_INET sock_addr2 sin_family W!

: GetPeerName ( s -- addr u ior )
  /sockaddr_in >r
  rp@ REL>ABS sock_addr2 REL>ABS ROT call getpeername SOCKET_ERROR =
  IF HERE 0 call WSAGetLastError
  ELSE sock_addr2 sin_addr @ GetHostName THEN  r>drop
;
: SocketsStartup ( -- ior )
  HERE rel>abs 257 call WSAStartup
;
: SocketsCleanup ( -- ior )
  call WSACleanup
;
: BindSocket ( port s -- ior )
  >R /sockaddr_in ALLOCATE ?DUP IF NIP R> DROP EXIT THEN
  >R
  256 /MOD SWAP 256 * +
  R@ sin_port W!
  AF_INET R@ sin_family W!
  R@
  0 R@ sin_addr !
  /sockaddr_in R> REL>ABS R> call bind SWAP FREE DROP SOCKET_ERROR =
  IF call WSAGetLastError ELSE 0 THEN
;
: ListenSocket ( s -- ior )
  2 SWAP call listen SOCKET_ERROR =
  IF call WSAGetLastError ELSE 0 THEN
;
CREATE SINLEN /sockaddr_in ,

: SOCKET-ACCEPT  { ADDR ALEN FH -- s2 ior }
   &LOCAL ALEN REL>ABS ADDR REL>ABS  FH call accept
   DUP INVALID_SOCKET =
   IF call WSAGetLastError ELSE 0 THEN
;

 :  #IP   ( du -- 0 )
           #S  [CHAR] . HOLD  2DROP 0
 ;
 : (.IP)  ( IP -- addr u )
  0 256 UM/MOD 0 256 UM/MOD 0 256 UM/MOD
        0 <#   \  0      HOLD
           #IP #IP #IP #S #> ;
: NtoA (.IP) ;

: CLIENT-OPEN ( addr u port -- s  )
       >r GetHostIP abort" Server not available "
       r> CreateSocket DROP DUP >r
       ConnectSocket abort" Can't connect "  r>
;

\s
  SocketsStartup [if] cr .(  SocketsStartup error) abort [then]
create my-ip-name
      cr  my-ip-addr cr  dup NtoA type GetHostName drop space type
      \ dup 1+ allot my-ip-name place

\s
