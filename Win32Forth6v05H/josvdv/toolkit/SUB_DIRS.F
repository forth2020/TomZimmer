anew sub_dirs.f

\ For scanning and processing files of 1 directory or a whole tree.
\ Extracted from WinView and changed by J.v.d.Ven.

\ May 31st, 2003 Now it uses one big heap so that it has not
\ to re-allocate memory again.
\ Now sdir will also needs a flag on the stack for handling of subdirectorys


create name-buf MAXSTRING allot

defer process-1file     \ holds function for processing a file

internal                \ Hide a number of words

TRUE value sub-dirs?

create mask-buf here  max-path + ," *.j*;*.p*" 0 , here - allot
       mask-buf value mask-ptr


 2variable mask-source

  0 value aborted?

: next-mask"    ( -- a1 n1 )            \ get the next path from dir list
                mask-source 2@ 2dup ';' scan  2dup 1 /string mask-source 2!
                nip - ;

: first-mask"   ( -- a1 n1 )            \ get the first forth directory path
                mask-ptr count mask-source 2!
                next-mask" ;

: process-afile ( adrd adr len -- )     \ search this file for find string
                name-buf  place         \ lay in directory
                11 cells+               \ adrz
                asciiz->asc-len         \ adrz slen -- adr len
                name-buf +place         \ append filename
\  cr name-buf count type
                process-1file  
                ;

create buf max-path allot

: "process-mask-directory { adr1 len1 adr2 len2  -- }
                len1
                IF      adr2 len2 buf  place
                        adr1 len1 buf +place
                        buf count find-first-file 0=
                        IF      adr2 len2 process-afile
                                BEGIN   find-next-file 0=      \ file level
                                        aborted?       0= and
                                WHILE   adr2 len2 process-afile
                                REPEAT  drop
                                find-close drop \ 0= abort" err"
                        ELSE    drop
                        THEN \ find-close drop \ 0= abort" err"
                THEN

                ;

variable _hdl-dir                       \ hold the find directory handle

: next-sub-dir"      ( -- sub-dir sub-len )     \ "first-sub-dir" must be called first
        BEGIN   _win32-find-data rel>abs                        \ lpffd - _WIN32_FIND_DIR
                _hdl-dir @                                      \ hFindFile
                call FindNextFile                               \ ior -
                IF      _win32-find-data @                      \ file_attributes
                        FILE_ATTRIBUTE_DIRECTORY and            \ is the dir bit set
                        IF      _win32-find-data 11 cells+      \ adrz
                                asciiz->asc-len                 \ -- adr len
                                2dup s" ."  compare 0= >r
                                2dup s" .." compare 0=  r> or   \ ignore either '.' or '..'
                        ELSE    0 0 TRUE                        \ try again
                        THEN
                ELSE    0 0 false
                THEN
        WHILE   2drop
        REPEAT  ;

: "first-sub-dir"    { adr len -- sub-adr sub-len }
        adr len asciiz   rel>abs                \ adrz -
        _win32-find-data rel>abs                \ lpffd - _WIN32_FIND_DIR
        swap                                    \ lpszSourceFile
        call FindFirstFile                      \ a search handle if O.K.
                                                \ else INVALID_HANDLE_VALUE
        _hdl-dir !                              \ store to the search handle
        _hdl-dir @ -1 <>                        \ adrd ior = 0 = success
        IF      _win32-find-data @              \ file_attributes
                FILE_ATTRIBUTE_DIRECTORY and    \ is the dir bit set
                IF      _win32-find-data 11 cells+
                        asciiz->asc-len         \ -- adr len
                        2dup s" ."  compare 0= >r
                        2dup s" .." compare 0=  r> or   \ ignore either '.' or '..'
                        IF      2drop                   \ discard adr,len
                                next-sub-dir"           \ find a sub directory
                        ELSE                            \ we found a directory
                        THEN
                ELSE    next-sub-dir"                   \ find a sub directory
                THEN
        ELSE    0 0
        THEN    ;

: sub-dir-close ( -- ior )      \ close the _hdl-dir handle
        _hdl-dir @ call FindClose 0= ;          \ ior - 0 = success


\ create buf1 max-path allot
\ create buf2 max-path allot

0 value mHndl
0 value #mHndl
0 value HeapBuffer

: new-buffer ( - adr )    HeapBuffer #mHndl max-path * +  ;

: "process-directory { adr len  \ buf1 buf2 -- }
                len
                if
\ cr adr len type ." <-process dir"
                        \ allocate two buffers in case we need are nesting dirs
                    new-buffer
                    dup to buf1 max-path + to buf2  \ init the buffer pointers
                    1 +to #mHndl
                        adr len     buf1 place          \ save the name
                                    buf1 ?+\            \ must end in '\'
                        first-mask" buf1 count "process-mask-directory
                        BEGIN       next-mask" dup
                                    aborted? 0= and
                        WHILE       buf1 count "process-mask-directory
                        REPEAT      2drop
                        sub-dirs?                       \ processing sub directories?
                        IF      buf1 count buf2  place
                                s" *"      buf2 +place
                                buf2 count "first-sub-dir" ?dup
                                IF
\ cr 2dup type ."   <======= first subdir"
                                        buf1 count buf2  place          \ init to parent
                                                   buf2 +place          \ append sub dir
                                                   buf2 ?+\             \ add '\' if needed
                                        _hdl-dir @ >r                   \ save before recursing
                                        buf2 count  RECURSE          \ recursively repeat
                                        r> _hdl-dir !                   \ restore from recursion
                                        BEGIN   next-sub-dir" dup
                                                aborted? 0= and
                                        WHILE
\ cr 2dup type ."  <---- next subdir"
                                                buf1 count buf2 place   \ init to parent
                                                buf2 +place             \ append sub dir
                                                buf2 ?+\                \ add '\' if needed
                                                _hdl-dir @ >r           \ save before recursing
                                                buf2 count  RECURSE      \ recursively repeat
                                                r> _hdl-dir !           \ restore from recursion
                                        REPEAT  2drop
                                        sub-dir-close drop              \ close dir find
                                ELSE    drop
                                THEN
                        THEN
                      -1 +to #mHndl
\ cr #mHndl .
                THEN    ;

: do-files-process ( -- )
      first-path" "process-directory
                BEGIN   next-path" dup
                        aborted? 0= and
                WHILE   "process-directory
                REPEAT  2drop
                ;

: init-heap-buffer
   0 to #mHndl
   0 [ 260 260 * 2 * ] literal dup>r HEAP_GENERATE_EXCEPTIONS  call HeapCreate to mHndl
   r>  HEAP_GENERATE_EXCEPTIONS mHndl call HeapAlloc  abs>rel to HeapBuffer
 ;

init-heap-buffer
initialization-chain chain-add init-heap-buffer

: delete-heap ( - )
\   mHndl call LocalFree  ?win-error
    mHndl call HeapDestroy ?win-error
 ;

UNLOAD-CHAIN chain-add delete-heap

: list-file  ( -- )   cr name-buf count type  ;

\ Tell sdir what to do for each file

' list-file is process-1file

external

: sdir ( path count file-spec count flag-subdir - )
        to sub-dirs?
        mask-buf place mask-buf +NULL
        path-ptr place path-ptr +NULL
        do-files-process
 ;

previous

 \s \ Disable or remove this line too see how it can be used.

\ The word list-file is executed for each file

cr cr .( No subdirectories )
s" c:\" s" *.com" false sdir

cr .( Hit any key ) key drop cr

cr .( Including subdirectories)

s" c:\" s" *.com" true sdir
cr cr .( Was including the subdirectories)

\s
