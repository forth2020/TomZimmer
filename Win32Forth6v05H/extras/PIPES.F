\ PIPES.F               Named Pipe Operations           by Tom Zimmer

\ NOTICE: For Win32Forth on WindowsNT ONLY,
\         CreateNamedPipe is not implemented on Windows95 or Win32s.

create pipe-name z," \\.\pipe\win32forth-named-pipe"

0 value pipe-handle

: init-pipe     ( -- )
                NULL                    \ lpSecurityAttributes address of security attributes structure
                1000                    \ nDefaultTimeOut,     time-out time, in milliseconds
                512                     \ nInBufferSize,       input buffer size, in bytes
                512                     \ nOutBufferSize,      output buffer size, in bytes
                1                       \ nMaxInstances,       maximum number of instances
                PIPE_TYPE_MESSAGE       \ dwPipeMode,          pipe-specific modes
                PIPE_ACCESS_DUPLEX
                FILE_FLAG_OVERLAPPED or \ dwOpenMode,          pipe open access mode
                pipe-name rel>abs       \ lpName,              address of pipe name
                Call CreateNamedPipe to pipe-handle
                pipe-handle 0= abort" Failed to Create Named Pipe"

                NULL
                pipe-handle
                Call ConnectNamedPipe 0= abort" Failed to Connect Named Pipe" ;

  create pipe-input  512 allot
  create pipe-output 512 allot
variable pipe-read-bytes
variable pipe-write-bytes

: read-pipe     ( -- )
                NULL
                pipe-read-bytes rel>abs
                512
                pipe-input rel>abs
                pipe-handle
                Call ReadFile 0= abort" Failed to read pipe" ;

: write-pipe    ( -- )
                NULL
                pipe-write-bytes rel>abs
                512
                pipe-output rel>abs
                pipe-handle
                Call WriteFile 0= abort" Failed to write pipe" ;

: open-pipe     ( -- )
                NULL
                NULL
                OPEN_EXISTING
                NULL
                NULL
                GENERIC_READ GENERIC_WRITE or
                pipe-name rel>abs
                Call CreateFile
                dup INVALID_HANDLE_VALUE = abort" Failed to create pipe"  to pipe-handle ;

: close-pipe    ( -- )
                pipe-handle Call CloseHandle 0= abort" Failed to close named pipe handle" ;

: uninit-pipe   ( -- )
                pipe-handle Call DisconnectNamedPipe 0= abort" Failed to disconnect named pipe"
                close-pipe ;

\ once this word is executed, the server window is locked up till a command comes in
\ from the client to tell the server what to do.

: server        ( -- )          \ load file and execute server on the server
                init-pipe
                begin   read-pipe
                        pipe-input c@
                while   pipe-input count ['] evaluate catch
                        if      s" Error"
                        else    s" Ok"
                        then    pipe-output place
                        write-pipe
                repeat  uninit-pipe ;


\ you can type commands into the client which the server will execute
\ press Enter on an empty line to terminate the connection

: client        ( -- )          \ load file and execute client on the client
                open-pipe
                begin   query
                        source pipe-output place
                        write-pipe
                        pipe-output c@
                while   read-pipe
                        space pipe-input count type cr
                repeat  close-pipe ;

