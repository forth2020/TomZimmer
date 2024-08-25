\ MAPFILE.F             File Memory Mapping             by Tom Zimmer

cr .( Loading File Memory Mapping...)

: map-handle    ( -<name>- )    \ define a data structure to hold a mapped file
                CREATE
                -1 ,                    \ hfile
                -1 ,                    \ hfileMapping
                 0 ,                    \ hfileAddress
                 0 ,                    \ hfileLength
                 0 ,                    \ hfileMaxLength
                 0 c, max-path 1- allot \ hfileName
                DOES> ;

map-handle dummy-map-handle

: map-field+    ( n1 n2 -<name>- n3 )
                ( a1 -- a2 )
                CREATE over + swap , nostack1
                DOES>   over body> @ ['] dummy-map-handle @ <>
                        abort" Must be a MAP-HANDLE!"
                        @ + ;

0   cell map-field+ >hfile
    cell map-field+ >hfileMapping
    cell map-field+ >hfileAddress
    cell map-field+ >hfileLength
    cell map-field+ >hfileMaxLength
max-path map-field+ >hfileName
drop

: create-file-map ( map-handle -- )
                dup >hfile @ -1 = abort" File must first be OPENED!"
                >r                      \               the file handle
                0                       \ *MapName      no name is specified
                r@ >hfileMaxLength @    \ MaxSizeLow    default or specified low size
                0                       \ MaxSizeHi     zero high part
                PAGE_READWRITE          \ fdwProtect    a read and writable file
                0                       \ psa           no security
                r@ >hfile @             \               the file handle
                Call CreateFileMapping r> >hfileMapping ! ;

: map-name      ( a1 n1 map-handle -- )
                >r 127 min r> >hfileName dup 128 erase place ;

: map-view-file ( map-handle -- )
                dup >hfileMapping @ -1 =
                abort" File must first be OPENED and MAPPED!"
                >r
                0                       \ amount of file to map=all of it
                0 0                     \ starting address of file
                FILE_MAP_WRITE
                r@ >hfileMapping @
                call MapViewOfFile ?dup 0=
                IF      Call GetLastError
                        cr ." Map-View Error: " . abort
                ELSE    abs>rel r@ >hfileAddress !
                THEN    r>drop ;

: flush-view-file ( map-handle -- f1 )  \ flush the file to disk
                dup  >hfileLength  @
                swap >hfileAddress @ rel>abs
                Call FlushViewOfFile 0= ;

: unmap-view-file ( map-handle -- f1 )
                >hfileAddress @ rel>abs
                Call UnmapViewOfFile 0= ;

: close-map-file ( map-handle -- f1 )
                dup >hfile @ -1 <>
                IF         dup  unmap-view-file >r
                           dup  >hfileMapping @ call CloseHandle 0= >r
                        -1 over >hfileMapping !
                           dup  >hfile        @ call CloseHandle 0= >r
                        -1 swap >hfile        !
                        r> r> or r> or
                ELSE    drop 0
                THEN    ;


: open-map-file ( a1 n1 map-handle -- f1 )
                >r
                r@ close-map-file drop
                r@ map-name
                r@ >hfileName count r/w open-file dup 0=
                IF      swap    r@ >hfile !
                                r@ >hfile @ file-size 2drop
                                r@ >hfileLength !
                                r@ create-file-map
                                r@ map-view-file
                ELSE    nip
                THEN    r>drop ;

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\       Shared memory functions
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

: close-share   ( memory_pointer handle -- )
                call CloseHandle drop
                rel>abs Call UnmapViewOfFile drop ;

0 value i-was-first

: open-share    ( z"name" length -- memory_pointer handle )
                FALSE to i-was-first
                over rel>abs
                FALSE
                FILE_MAP_WRITE
                call OpenFileMapping ?dup 0=
                IF      >r
                        rel>abs            \ *MapName   no name is specified
                        r>                 \ MaxSizeLow shared memory size
                        dup to i-was-first \ save size as a flag
                        0                  \ MaxSizeHi  zero high part
                        PAGE_READWRITE     \ fdwProtect a read and writable file
                        0                  \ psa        no security
                        -1                 \            the file handle
                        Call CreateFileMapping
                ELSE    nip nip
                THEN    dup
                IF      >r
                        0                  \ amount of file to map=all of it
                        0 0                \ starting offset into file
                        FILE_MAP_WRITE
                        r@ call MapViewOfFile ?dup 0=
                        IF      r> Call CloseHandle drop
                                0 FALSE
                        eLse    abs>rel    \ -- mapped_address
                                i-was-first \ if first, erase buffer
                                IF      dup i-was-first erase
                                THEN
                                r>         \ -- mapped_address file_handle
                        THEN
                ELSE    drop 0 FALSE       \ failed, return failure flag
                THEN    ;


\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\   primitive utilities to support view, browse and edit of words and files
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

0 value ed-terminated?
0 value ed-hndl
0 value ed-ptr
FALSE value second-forth?               \ am I the second or more copy of forth to load

: ed-field+     ( n1 n2 -<name>- n1+n2 )
                CREATE  over , + nostack1
                DOES>   @ ed-ptr +  ;

0
cell      ed-field+ ed-present          \ is the editor already in memory
cell      ed-field+ ed-console-hidden   \ "HIDN" if console is to be hidden
cell      ed-field+ ed-console-remote   \ "RMOT" if console is to be remote
cell      ed-field+ ed-forth-count      \ count of forths running
cell      ed-field+ ed-response         \ response from the editor
cell      ed-field+ ed-line             \ line number to edit
cell      ed-field+ ed-column           \ column number to edit
MAXSTRING ed-field+ ed-result           \ result of request
MAXSTRING ed-field+ ed-filename         \ filename passed to editor
MAXSTRING ed-field+ ed-name             \ name of word to debug
64 cells  ed-field+ ed-stack            \ buffer to hold stack passed to editor
MAXSTRING ed-field+ ed-return           \ buffer to hold the return stack
MAXSTRING ed-field+ ed-dbgline          \ debug output line
MAXSTRING ed-field+ ed-inquire          \ editor requests value of data item

4096 + 4096 / 4096 * constant ed-size   \ multiples of 4k bytes

\ Messages To Forth FROM the Editor
newproc WM_SETBP
newproc WM_STEPBP
newproc WM_NESTBP
newproc WM_UNESTBP
newproc WM_CONTBP
newproc WM_JUMPBP
newproc WM_BEGNBP
newproc WM_HEREBP
newproc WM_RSTKBP
newproc WM_DONEBP
newproc WM_INQUIRE

\ tell Forth to load TEMP.F
newproc WM_PASTELOAD

\ Messages TO the Editor FROM the Editor
newproc ED_OPEN_EDIT
newproc ED_OPEN_BROWSE
newproc ED_WATCH
newproc ED_WORD
newproc ED_STACK
newproc ED_DEBUG
newproc ED_NOTINBP

\ Messages FROM Editor TO Forth for Console

newproc WM_EVALUATE                             \ send Forth a string to evaulate
newproc WM_KEY                                  \ give Forth a key
newproc WM_REMOTEIO                             \ tell Forth to direct all I/O to the Editor's console window
newproc WM_FORTHIO                              \ tell Forth to go back to its won console window
newproc WM_TERMINATE                            \ tell Forth to shutdown

\ Messages TO Editor FROM Forth for Console

newproc ED_READY                                \ notify editor Forth is ready to accept characters
newproc ED_ALIVE                                \ notify editor Forth is alive (not used)
newproc ED_EMIT
newproc ED_TYPE
newproc ED_QCR
newproc ED_CRTAB
newproc ED_COL
newproc ED_GETXY
newproc ED_GOTOXY
newproc ED_GETCOLROW
newproc ED_CLS
newproc ED_SHUTDOWN                             \ tell editor to shut down the Forth console window

defer   init-shared-type                        \ set the sharing type for current program
defer uninit-shared-type                        \ set the sharing type for current program

: init-shared-forth ( -- )
                1 ed-forth-count +! ;           \ bump count of Forths currently running

' init-shared-forth is init-shared-type

: init-shared-memory ( -- )
                0 to ed-ptr                     \ initialize to not present
                0 to ed-hndl
                z" SharedEditMemory" ed-size open-share dup
                IF      to ed-hndl
                        to ed-ptr
                        init-shared-type        \ bump count of Forths currently running
                ELSE    2drop
                THEN    ;

init-shared-memory                              \ init it now per smb March 6th, 1996

initialization-chain chain-add init-shared-memory

: uninit-shared-forth ( -- )
                ed-terminated? 0=               \ the editor isn't telling me to quit
                second-forth?  0= and           \ and I'm the editors copy of Forth
                IF      0 ed-result !           \ then tell console to close the console
                        0 ED_SHUTDOWN win32forth-message
                THEN
                -1 ed-forth-count +! ;          \ bump count of Forths currently running

' uninit-shared-forth is uninit-shared-type

: uninit-shared-memory ( -- )
                ed-ptr  0<>                     \ if shared memory was inited
                ed-hndl 0<> and
                IF      uninit-shared-type
                        ed-ptr ed-hndl close-share
                        0 to ed-ptr
                        0 to ed-hndl
                THEN    ;

unload-chain chain-add-before uninit-shared-memory

: editor-present? ( -- f1 )
                ed-ptr
                IF      ed-present @
                ELSE    FALSE
                THEN    ;

: _rcls         ( -- )
                editor-present?                         \ TRUE if editor is loaded
                IF      0 ed-result !
                        0 ED_CLS win32forth-message
                THEN    ;

: _remit        ( c1 -- )               \ allow mouse to abort EMIT
                editor-present?                         \ TRUE if editor is loaded
                IF      dup ed-result !
                        0 ED_EMIT win32forth-message
                THEN    drop ;

: _rtype        ( a1 n1 -- )            \ allow mouse to abort TYPE
                editor-present?                         \ TRUE if editor is loaded
                IF      2dup ed-result place
                        0 ED_TYPE win32forth-message
                THEN    2drop ;

: _rcol         ( n1 -- )
                editor-present?                         \ TRUE if editor is loaded
                IF      dup ed-result !
                        0 ED_COL win32forth-message
                THEN    drop ;

: _r?cr         ( n1 -- )
                editor-present?                         \ TRUE if editor is loaded
                IF      dup ed-result !
                        0 ED_QCR win32forth-message
                THEN    drop ;

: _rcrtab       ( -- )
                editor-present?                         \ TRUE if editor is loaded
                IF      tabing?
                        IF      first-line?
                                IF      left-margin indent +
                                        false to first-line?
                                ELSE    left-margin tab-margin +
                                THEN    ed-result !     \ set the left margin
                        ELSE    0 ed-result !           \ clear left margin
                        THEN
                        0 ED_CRTAB win32forth-message
                THEN  ;

: _raccept      ( a1 n1 -- )
                editor-present?                         \ TRUE if editor is loaded
                IF      0 ed-result !
                        0 ED_READY win32forth-message
                THEN    _accept ;

: _rgotoxy      ( x y -- )
                editor-present?                         \ TRUE if editor is loaded
                IF      over ed-result !                \ save column
                        dup  ed-result CELL+ !          \ save row
                        0 ED_GOTOXY win32forth-message
                THEN    2drop ;

: _rgetxy       ( -- col row )
                editor-present?                         \ TRUE if editor is loaded
                IF      0 ed-result !
                        0 ED_GETXY win32forth-message
                        200 0
                        DO      WINPAUSE
                                ed-result @ ?leave
                        LOOP
                        ed-result @                     \ col
                        ed-result CELL+ @               \ row
                THEN  ;

: _rgetcolrow   ( -- cols rows )
                editor-present?                         \ TRUE if editor is loaded
                IF      0 ed-result !
                        0 ED_GETCOLROW win32forth-message
                        200 0
                        DO      WINPAUSE
                                ed-result @ ?leave
                        LOOP
                        ed-result @                     \ cols
                        ed-result CELL+ @               \ rows
                THEN  ;

: remote-forth-io  ( -- )               \ reset to Forth IO words
                hide-console
                ['] _remit      is emit
                ['] _rtype      is type
                ['] _rcrtab     is cr
                ['] _r?cr       is ?cr
                ['] _rcls       is cls
                [']  cls        is page
                ['] _raccept    is accept
                ['] _rgotoxy    is gotoxy
                ['] _rgetxy     is getxy
                ['] _rgetcolrow is getcolrow
                ['] _rcol       is col
                tabing-off
                TRUE to remote-io? ;

: do-remote-forth-io ( -- )
                ['] remote-forth-io is console
                remote-forth-io ;

: do-forth-io   ( -- )
                ['] forth-io is console
                forth-io ;

: do-forth-evaluate ( -- )
                ed-result count "pushkeys ;

also bug

: do-inquire    ( -- )  \ respond to an inquiry from the editor on a data item
                ed-result off
                here   here-save     MAXSTRING move
                pocket pocket-save   MAXSTRING move
                source tib-save swap MAXSTRING min move \ save SOURCE buffer
                (source) 2@ 2>r >in @ >r                \ save SOURCE and >IN
                sp@ >r depth >r
                ed-inquire count ['] evaluate catch
                if      beep
                else    depth r@ - 0max dup ed-result !
                        32 min 0                        \ no more than 32 data items
                        ?do     ed-result i 1+ cells+ ! \ move results in
                        loop
                then
                r>drop r> sp!                           \ restore stack depth
                r> >in ! 2r> (source) 2!                \ restore SOURCE and >IN
                tib-save source move                    \ restore SOURCE buffer
                pocket-save pocket MAXSTRING move
                here-save   here   MAXSTRING move ;

previous

: db-pushkey    ( c1 -- )
                in-breakpoint?
                if      pushkey
                else    drop
                        0 ED_NOTINBP win32forth-message
                then    ;

: win-set-breakpoint { wParam lParam \ bp$ -- wParam lParam }
                wParam lParam
                second-forth? ?EXIT             \ ignore messages if second copy
                MAXSTRING LocalAlloc: bp$
                ed-ptr   0= ?EXIT               \ exit if no shared memory
                sys-free 0= ?EXIT               \ exit if no heads are present
                over WM_SETBP =                 \ if we are being told to set a breakpoint
                if      context @ >r            \ save context vocabulary
                        ed-name count bl skip 2dup bl scan ?dup
                        if      2dup 2>r nip - bp$ place
\in-system-ok                   bp$ anyfind
                                if      execute
                                        2r> bl skip bp$ place
\in-system-ok                           bp$ anyfind
                                else    2r> 2drop
                                        FALSE
                                then
\in-system-ok           else    ed-name anyfind
                        then
                        if      unbug                   \ remove any previous BP
                                remote-debug            \ set the breakpoint
                                dup ed-response !       \ non zero=success
                                if      with-source     \ enable source viewing
                                then
                        else    drop                    \ couldn't find it
                                0 ed-response !         \ 0=failure
                        then
                        r> context !            \ restore the context vocabulary
                        EXIT
                then    over
                CASE
\ --- Debugger support

                        WM_STEPBP       OF      0x0D           db-pushkey       ENDOF
                        WM_NESTBP       OF      'N'            db-pushkey       ENDOF
                        WM_UNESTBP      OF      'U'            db-pushkey       ENDOF
                        WM_CONTBP       OF      'C'            db-pushkey       ENDOF
                        WM_JUMPBP       OF      'J'            db-pushkey       ENDOF
                        WM_BEGNBP       OF      'P'            db-pushkey       ENDOF
                        WM_HEREBP       OF      'P' +k_control db-pushkey       ENDOF
                        WM_RSTKBP       OF      'R'            db-pushkey       ENDOF
                        WM_DONEBP       OF      'D'            db-pushkey       ENDOF
                        WM_INQUIRE      OF      do-inquire                      ENDOF

\ --- Support for console interface

                        WM_EVALUATE     OF      do-forth-evaluate               ENDOF
                        WM_KEY          OF      dup pushkey                     ENDOF
                        WM_REMOTEIO     OF      do-remote-forth-io              ENDOF
                        WM_FORTHIO      OF      do-forth-io                     ENDOF
                        WM_TERMINATE    OF      TO ed-terminated?
                                                bye                             ENDOF
                ENDCASE ;

forth-msg-chain chain-add win-set-breakpoint

\S

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\       simple memory file mapping example
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

map-handle ahndl

: mapfile       ( -<name>- a1 n1 )      \ map file name into memory
                /parse-word count ahndl open-map-file
                abort" Failed to open and map the file!"
                ahndl >hfileAddress @
                ahndl >hfileLength  @ ;

: unmapfile     ( -- )                  \ unmap and close the file
                ahndl close-map-file drop ;


\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\       shared memory example
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

0 value share-hndl
0 value share-ptr

: share         ( -- )
                z" SharedFile" 4096 open-share ?dup 0=
                abort" Failed to open shared memory"
                to share-hndl
                to share-ptr ;

: unshare       ( -- )
                share-ptr share-hndl close-share
                0 to share-ptr
                0 to share-hndl ;

: .share        ( -- )
                share-ptr 0= abort" Nothing shared!"
                share-ptr 40 dump ;


