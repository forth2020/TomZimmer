\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ \\\\\\\\      Parallel Port ROM Emulator Interface     \\\\\\\\\\\\
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

ONLY FORTH ALSO DEFINITIONS

0 value ParHndl
                                
: LPTInit       ( n -- )                        \ Initialize LPTn
                temp$ 20 erase                    \ form "port#"
                s" LPT" temp$ place (.) temp$ +place
                temp$ char+  >R
                NULL                            \ no template
                FILE_ATTRIBUTE_NORMAL           \ open file attributes
                OPEN_EXISTING                   \ creation distribution
                NULL                            \ no security attributes
                0                               \ exclusive access
                GENERIC_READ GENERIC_WRITE or   \ desired access modes
                R> rel>abs                      \ filename
                Call CreateFile ?dup 0= Abort" Failed to open parallel port"
                to ParHndl                      \ save the handle to the port
                ;

: LptClose      ( -- )                          \ close com port if its open
                ParHndl ?dup
                if      Call CloseHandle drop
                        0 to ParHndl
                then    ;


\ The ROM emulator looks like a printer. Blowing n bytes of data out the
\ parallel port results in that data being placed in locations 0..n-1 of the
\ ROM emulator. Byte splitting for wide data busses is automatically handled
\ by the emulator. Obviously, you have to be careful not to send the data
\ to a real printer.

\ See www.nakatsu.com for ROM emulators.

1 value parallelport

: _loadROM      ( a n -- )
                parallelport LPTinit
                cr ." Sending " dup . ." bytes to ROM emulator on LPT"
                parallelport .
                begin   dup
                while   2dup 4096 min tuck  ( a n sublen a sublen )
                        ParHndl write-file
                        abort" Failed to write parallel port" 
                        /string
                        key? drop           \ keep windows rolling
                repeat  2drop
                LPTclose ;

: loadROM       ( -- )
                codebuf ihere @ _loadROM ;  \ typical usage

: toggleport    ( -- )
                parallelport 1 = if 2 else 1 then to parallelport
                cr ." ROM emulator destination is now LPT" parallelport . ;


