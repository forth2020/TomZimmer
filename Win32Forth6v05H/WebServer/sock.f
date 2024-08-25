\ Socket Library
\ Tom Dixon

\ *! Sock
\ *T Simple Socket Library
\ *Q Tom Dixon

\ *P This is intended to be a simple wordset for sockets in forth.
\ ** The words do not match the standard socket api.  It has been adapted to 
\ ** be easier to use in Forth.
\ ** It's simplicity should make it easy to port network apps to other
\ ** forth systems.


\ these constants come from a complicated formula in winsock2.h
$4004667f constant FIONREAD 
$8004667e constant FIONIO
$8004667d constant FIONASYNC

\ Import WinSock2 Dll

winlibrary ws2_32.dll


\ Import functions from the dll

\ *S Network Formatting Words
\ ** These words are for converting 16-bit and 32-bit values to the right 
\ ** format so any machine should be able to convert them back into their
\ ** values.  

1 PROC htonl ( hostlong -- u_long )
\ *G Convert a 32-bit number on the stack to a network acceptable 
\ ** byte-ordered value.

1 PROC htons ( hostshort -- u_short )
\ *G Convert a 16-bit number on the stack to a network acceptable 
\ ** byte-ordered value.

1 PROC ntohl ( netlong -- u_long )
\ *G Convert a network compatible 32-bit number on the stack to the
\ ** correct 32-bit integer

1 PROC ntohs ( netshort -- u_short )
\ *G Convert a network compatible 16-bit number on the stack to the
\ ** correct 16-bit integer


1 PROC inet_addr ( *cp -- in_addr )
1 PROC inet_ntoa ( in_addr -- *char )

2 PROC listen ( backlog sock -- int )
4 PROC recv   ( flags len *buf sock -- int )
5 PROC select ( *timeout *exceptfds *writefds *readfds nfds -- int )
4 PROC send   ( flags len *buf sock -- int )
2 PROC shutdown ( how sock -- int )
3 PROC socket ( protocol type af-- sock )
3 PROC bind ( namelen 'sock sock -- int )
1 PROC closesocket ( sock -- int )
3 PROC connect ( len 'sock sock -- int )
3 PROC ioctlsocket ( *argp cmd sock -- int )
1 PROC gethostbyname ( *name -- hostent )

2 PROC WSAStartup ( lpWSAData wVersionRequired -- int )
0 PROC WSACleanup ( -- int )


\ *S Socket Library and Initialization Words
\ ** These words are for initializing and unloading the windows socket 
\ ** dll.  They are automatically called when the console is initialized
\ ** and right before it closes, so normally a developer would never need
\ ** to use these.

\ API Setup and Closure (linked to initialization and closure chains)

: wsocket2-init ( -- ) 
\ *G Initializes the windows socket dll \n
\ ** called in initialization-chain
  pad $202 WSAStartup drop ;
  
: wsocket2-cleanup ( -- )
\ *G Initializes the windows socket dll \n
\ ** called in initialization-chain
  WSACleanup drop ;

initialization-chain    chain-add wsocket2-init
unload-chain            chain-add wsocket2-cleanup

wsocket2-init   \ initialize sockets


\ User Area Definition
\ This is to make all socket functions thread-safe

16 newuser saddr \ socket address structure

\ *S Main Socket Words
\ ** These words represent the core of the socket library.
\ ** They have been written to be thread-safe.

: host>iaddr ( str len -- iaddr )
\ *G This function converts a host string to an ip address \n
\ ** The host string could be anything from a domain name to ip address. \n
\ ** Returns 0 if the host is unable to be looked up.
  pad place 0 pad c+place pad 1+ gethostbyname 
  dup if 12 + @ @ @ then ;
  
: iaddr>str ( iaddr -- str len ) 
\ *G This converts an ip address to a readable string.  
\ ** It does not look up the host name, the string is in the "255.255.255.255" format
  inet_ntoa zcount ;

  
\ *W <br><br><U>Example:</U> simple host lookup.<br>
\ *E s" www.win32forth.org" host>iaddr
\ ** dup . \ should be anything other than 0
\ ** iaddr>str type \ should return ip address of win32forth.org



: sock-open ( addr len port -- sock )
\ *G This opens up a new socket to a host name on a given port number \n
\ ** the host name will be looked up and the port number is converted implicitly \n
\ ** If the socket cannot be opened, a exception will be thrown. 
  htons saddr 2 + w!
  AF_INET saddr w!
  host>iaddr saddr 4 + ! 
  0 SOCK_STREAM AF_INET socket dup
  16 saddr rot connect abort" Unable to connect!" ;


: sock-read ( addr len sock -- len ) 
\ *G Reads data from the socket to a buffer. \n
\ ** It works very similarly to 'read-file', but has different return parameters \n
\ ** a returned 'len' of -1 means there was a socket error (SOCKET_ERROR) \n
\ ** If the provided 'len' is larger than the amount of data ready to be read from the socket, 
\ ** the socket will block until it has revceived the full amount of data.\n
\ ** If the socket is a non-blocking socket, it will read what it can and return 
\ ** right away.
  >r swap 0 -rot r> recv ;

: sock-write ( addr len sock -- len ) 
\ *G Write data from a buffer to the socket. \n
\ ** It works very similarly to 'write-file' \n
\ ** a returned 'len' of -1 means there was a socket error (SOCKET_ERROR) \n
\ ** If the socket is currently unable to take any data,
\ ** the socket will block until it has room in it's internal buffer to send the data.\n
\ ** If the socket is a non-blocking socket, it will write what it can and return 
\ ** right away. (amount actually written is returned as 'len')
  >r swap 0 -rot r> send ;

: sock-close ( sock -- ior )
\ *G Closes socket - very similar to close-file\n
\ ** ior is 0 if the close was successful
  closesocket ;


\ *W <br><br><U>Example:</U> Get data from a socket.<br>
\ *W This will dump the html data from google's homepage through the use of sockets.<br>
\ *E create tbuf 256 allot
\ ** 0 value sock
\ ** : sdump ( sock -- ) 
\ **   begin 
\ **     dup sock-read? if dup tbuf 256 rot sock-read tbuf swap type then 
\ **     dup sock-closed? key? or until 
\ **   sock-close drop ;
\ **
\ ** s" www.google.com" 80 sock-open value sock 
\ ** s" GET / HTTP/1.0" sock sock-write .
\ ** crlf$ count sock sock-write .
\ ** crlf$ count sock sock-write . 
\ ** sock sdump 



\ *S Socket Listening Words
\ ** These words are for writting the serving-end of network applications.\n
\ ** They have also been written to be thread-safe.

: sock-create ( p -- sock )
\ *G Make a new socket for listening on port 'p'
\ ** Used only for server-side sockets
  htons saddr 2 + w!
  AF_INET saddr w!
  INADDR_ANY saddr 4 + ! 
  0 SOCK_STREAM AF_INET socket dup
  16 saddr rot bind abort" Unable to bind socket!" ;

: sock-listen ( n sock -- )  
\ *G This tells a socket to start queuing sockets that want to connect.\n
\ ** 'n' is the size of the queue that should be created to listen.
\ ** after 'n' sockets have tried to connect and have yet to be accepted, 
\ ** further sockets will be refused until waiting sockets are accepted.
\ ** (standard queue size is 5)
  listen drop ;

: sock-accept ( sock -- sock iaddr ) 
\ *G This will accept a socket that is in the listening queue. \n
\ ** 'iaddr' is the ip address of the connecting socket and can be converted
\ ** into an easy-to-read number through the 'iaddr>str' word. \n
\ ** If no sockets are in queue to be accepted, this function will block
\ ** until one tries to connect. \n
\ ** If the socket is a non-blocking socket, then the function will fail
\ ** and return immediately if the queue has no sockets to accept. \n
\ ** If the function fails, it will return '0' as the iaddr and '-1' 
\ ** (or INVALID_SOCKET) as the socket.
  16 >r rp@ saddr rot call accept r> drop 
  dup INVALID_SOCKET = if 0 
  else saddr 4 + @ then ;
  


\ *S Asyncronous Socket Words
\ ** These words are for the ability to use the sockets without having them block.\n
\ ** Very useful for apps that need to do many things at once.


: sock-read? ( sock -- n )
\ *G This function returns the amount of data that the socket can read 
\ ** without blocking.  It is useful for working with socket asyncronously.\n
\ ** It will return -1 if the socket has no data to read (will block, or socket closed).
  0 >r rp@ FIONREAD rot ioctlsocket if r> drop -1 exit then r> ;

: sock-write? ( sock -- flag )
\ *G This function returns true if the socket can write data without blocking.\n
\ ** You can send 0-1024 bytes to the socket asyncronously without blocking if 
\ ** the flag is true.
  1 saddr ! 
    saddr 4 + ! 
  0 saddr 8 + ! 
  0 saddr 12 + ! 
  saddr 8 + 0 saddr 0 0 select ;

: sock-accept? ( sock -- flag )
\ *G This function returns true if the socket has other sockets in queue that 
\ ** want to be connected.  It is to be used in conjunction with 'sock-accept'
\ ** so you can call sock-accept without blocking.
  1 saddr ! 
    saddr 4 + ! 
  0 saddr 8 + ! 
  0 saddr 12 + ! 
  saddr 8 + 0 0 saddr 0 select ;

: sock-closed? ( sock -- flag ) 
\ *G This function tests to see if the socket has been closed at the other end
\ ** or broken at any point.
  dup sock-accept? 1 = swap sock-read? 0 = and ;

: sock-err? ( sock -- n )
\ *G This function tests to see if there are any errors on the socket.
  1 saddr ! 
    saddr 4 + ! 
  0 saddr 8 + ! 
  0 saddr 12 + ! 
  saddr 8 + saddr 0 0 0 select ;
  
  
: sock-blocked ( flag sock -- )
\ *G This function sets a socket to blocked or unblocked mode.\n
\ ** If the flag is false, the socket will be set to 'unblocked'.\n
\ ** If the flag is true, the socket will be set to 'blocked'.\n
  swap not >r rp@ FIONIO rot ioctlsocket r> 2drop ;  
  
  
