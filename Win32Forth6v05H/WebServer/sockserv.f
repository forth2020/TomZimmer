\ Socket Server
\ Tom Dixon


\ *! SockServer
\ *T Socket Library Extension for Servers
\ *Q Tom Dixon

\ *P This library is built off of the socket library and provides some 
\ ** generic support for socket servers.  The current implementation is
\ ** asycronous, single-threaded and is select-based and does not use 
\ ** the poll() function.

needs Sock.F \ Socket Library

\ Re-Define the list library if it is not already in the system
[DEFINED] cons NOT [IF]

: cons ( node list -- list ) over ! ;
: lrest ( list -- list ) @ ;

[THEN]

\ This value ought to be made into a user variable to give it better
\ multithreaded support.
0 value sservdata

6 cells constant servuser
  \ structure: |link|sock|onconnect|onread|onwrite|onclose|


\ *S Socket Event Vectors
\ ** These words are used to define the behavior of the sockets on the 
\ ** server.\n
\ ** Each event is defined as a word with no stack effects ( -- ).\n
\ ** Defining these vectors applies to the currently active client 
\ ** connection.  If you want to set the default behavior for incoming
\ ** client connections, please see 'serv-vecselect'.

: OnClose! ( xt -- ) 
\ *G This word stores a new closure behavior for the socket connection.
  sservdata 5 cells + ! ;
  
: doOnClose ( -- ) sservdata 5 cells + @ execute ;
 
: OnRead! ( xt -- ) 
\ *G This word stores a new read behavior for the socket connection.
  sservdata 3 cells + ! ;
  
: doOnRead ( -- ) sservdata 3 cells + @ execute ;
  
: OnWrite! ( xt -- ) 
\ *G This word stores a new write behavior for the socket connection.
  sservdata 4 cells + ! ;
  
: doOnWrite ( -- ) sservdata 4 cells + @ execute ;
  
: OnConnect! ( xt -- ) 
\ *G This word stores a new connection behavior for the socket.
  sservdata 2 cells + ! ;
  
: doOnConnect ( -- ) sservdata 2 cells + @ execute ;


\ *S Global Socket Data
\ ** When a socket event is being processed, these words contain are to
\ ** be used in obtaining specific information about the request.

: servdata ( -- addr ) 
\ *G Returns a pointer to the user-defined data area associated with 
\ ** every request.  The size of this user area is specified by the 
\ ** server.
  sservdata servuser + ;
  
: servsock ( -- sock ) 
\ *G Returns the socket that the event has been triggered on.
  sservdata cell+ @ ;
  
: close-client ( -- ) 
\ *G Closes the current socket at frees up the memory from the server.
  servsock sock-close drop 
  0 sservdata cell+ ! ;
  
  
  
\ *S Socket Server Words
\ ** A socket server is the listening server that takes requests, 
\ ** processes them, and closes them.

: serv-vecselect ( server -- ) 
\ *G Selects the server for vector behavior.  Directly after this word
\ ** is called, default behaviors for the entire server can be specified.
  4 cells + to sservdata ;  

: sockserver ( datasize p <name> -- )
\ *G This word defines a socket server on port "p" and the size of the 
\ ** user-defined data area per client.
  create here serv-vecselect
  0 , 0 , , servuser + , servuser allot 
  ['] noop onconnect!
  ['] noop onwrite!
  ['] noop onread!
  ['] noop onclose! ;
  
: serv-init ( server -- ) 
\ *G Initializes the server and starts listening for requests.
  dup 2 cells + @ sock-create dup rot ! 5 swap sock-listen ;
  
: serv-close ( server -- ) 
\ *G Closes the server - open requests are still able to execute, though.
  dup @ sock-close drop 0 swap ! ;

\ *N FromIp
\ **  A user variable to save the IP-adres of the sender

0 user FromIp

: serv-accept ( server -- )
  begin dup @ sock-accept? while
      dup @ sock-accept FromIp ! ?dup if
      over 3 cells + @ allocate throw
      to sservdata 
      sservdata cell+ ! 
      dup 4 cells + 2 cells +
      sservdata 2 cells + 4 cells cmove
      dup cell+ lrest sservdata swap cons drop 
      dup cell+ sservdata cons drop 
      >r doOnConnect r> 
    then
  repeat drop ;
  

: (serv-poll) ( server -- )
  cell+ @ to sservdata 
  begin sservdata while
    servsock sock-err? if close-client then
    servsock sock-closed? if doOnClose close-client then
    servsock sock-read? if doOnRead then
    servsock sock-write? if doOnWrite then
    sservdata lrest to sservdata
  repeat ;
  
: (serv-cleanup) ( server -- ) 
  cell+ to sservdata 
  begin sservdata lrest while
    sservdata lrest cell+ @ 0= if 
      sservdata lrest dup lrest sservdata swap cons drop 
      free throw 
    then 
    sservdata lrest ?dup if to sservdata then 
  repeat ;
  
: serv-poll ( server -- ) 
\ *G The meat-and-potatoes function of the socket server.  This 
\ ** word will deal with all incoming socket requests, poll through
\ ** and process existing socket requests, and cleanup after closed
\ ** requests.
  dup serv-accept dup >r (serv-poll) r> (serv-cleanup) ;
  

\ *S Example Code
\ ** This is a simple test of the socket server code.  Typing in the 
\ ** word 'demo' will start the test.  Any incoming request will simply 
\ ** be printed to the console.  (Yes, it's not very useful, but it is
\ ** a minmal example of use.  Please see other examples that should 
\ ** be with this file).

\ *E 256 8000 sockserver test
\ ** test serv-vecselect 
\ ** :noname servdata 256 servsock sock-read servdata swap type ; 
\ ** onread!
\ ** 
\ ** test serv-init
\ ** : demo begin test serv-poll 10 ms key? until ;
