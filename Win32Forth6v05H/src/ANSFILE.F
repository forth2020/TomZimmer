\ ANSFILE.F

\ ansfile.f beta 2.0A 2002/08/31 arm windows ANS file words
\ ansfile.f beta 2.9G 2002/09/24 arm release for testing
\ ansfile.f beta 3.3D 2002/10/08 arm Consolidation

cr .( Loading ANSI File Wordset...)
cr .( -- BETA ANSFILE.F V2.9G --)

\ This is an ANSI file words set
\ ansfile.f
\
\ The words defined in this file follow the draft proposed AMERICAN
\ NATIONAL  Standard for Information System - Programing Language - Forth
\ - June 30, 1993
\
\ If ior = 0, operation is O.K.; Otherwse, it is a failure.


only forth also definitions


variable _hdl-search
         _hdl-search off

0 value #files                  \ number of files found
0 value total-file-bytes        \ total bytes in files found


create _win32-find-data                 \ struct FindFileFirst or
                                        \ FindNextFile Functions
   0 ,                                  \ dwFile Attributees
   0 , 0 ,                              \ ftCreationTime - FILETIME -
                                        \ (struct) dwLowDateTime
                                        \          dwHighDateTime
   0 , 0 ,                              \ ftLastAccessTime - FILETIME
   0 , 0 ,                              \ ftLastWriteTime - FILETIME
   0 ,                                  \ nFileSizeHigh
   0 ,                                  \ nFileSizeLowh
   0 ,                                  \ dwReserved0
   0 ,                                  \ dwReserved1
   MAX-PATH allot                       \ cFileName[MAX_PATH]
   14 allot                             \ cAlternateFileNane[14]

create ptr-DOS-date 0 w,                \ ptr to 16 bit DOS date
create ptr-DOS-time 0 w,                \ ptr to 16 bit DOS time

create _systemtime                      \ struct FileTimeToSystemTime
                                        \ function; this struc is same
                                        \ as time-buf in kernel.tom
        0 w,                            \ +0  wYear
        0 w,                            \ +2  wMonth
        0 w,                            \ +4  wDayOfWeek
        0 w,                            \ +6  wDayOfMonth
        0 w,                            \ +8  wHour
        0 w,                            \ +10 wMinute
        0 w,                            \ +12 wSecond
        0 w,                            \ +14 wMilliseconds



\ Get a drives free space, cluster and sector information
\ "zroot" is the root directory spec zString for the desired drive in the
\ format z" x:\", where x can be a, b, c, d or e etc...
: get-fspace { zroot \ clus freclus b/sec s/clus -- as bs cs ds }
        &LOCAL clus     rel>abs                 \ lpClusters - bs
        &LOCAL freclus  rel>abs                 \ lpFreeClusters - as
        &LOCAL b/sec    rel>abs                 \ loBytesPerSector - ds
        &LOCAL s/clus   rel>abs                 \ lpSectorsPerCluster - cs
        zroot           rel>abs                 \ lpszRootPathName
        call GetDiskFreeSpace
        if      freclus  clus                   \ as bs -
                s/clus  b/sec                   \ as bs cs ds -
        else    0 0 0 0                         \ 0 0 0 0 - for failure
        then    ;

: find-first-file ( adr slen -- adrd ior )
  \ name(adr) - null-terminated valid directory or path and filename
  \ len - length of ascii name in bytes
  \ adrd - address of _win32-find-data struc
   260 malloc dup>r
   ascii-z rel>abs                            \ adrz -
   _win32-find-data rel>abs                  \ lpffd - _WIN32_FIND_DATA
   swap                                      \ lpszSourceFile
   call FindFirstFile                        \ a search handle(hdl if O.K.
                                             \ else INVALID_HANDLE_VALUE)
   _hdl-search !                             \ store to the search handle
   _win32-find-data                          \ adrd -
   _hdl-search @ -1 =                        \ adrd ior - 0 = success
   r> release ;                              \ free buff


: find-next-file ( -- adrd ior ) \ find-first-file word must be called
  \ before this word can be called due to that _hdl-search is needed
   _win32-find-data rel>abs                  \ lpffd - _WIN32_FIND_DATA
   _hdl-search @                             \ hFindFile
   call FindNextFile                         \ ior -
   _win32-find-data                          \ ior adrd -
   swap 0= ;                                 \ adrd ior - 0 = success

: find-close ( -- ior ) \ close the _hdl-search handle
   _hdl-search @
   call FindClose 0= ;                       \ ior - 0 = success

\ : file-info     ( fileid -- adrd ior )
\   \ get windows information on the currently open file
\    _win32-find-data rel>abs swap
\    call GetFileInformationByHandle
\    _win32-find-data swap 0= ;

\ : .file-info    ( fileid -- )   \ display information about the current file
\         file-info abort" Failed to get information about the file"
\         cr ." Attributes: "    dup @ h.
\         drop ;


: get-DOS-create-datetime ( -- ;convert 64 bit file time to MS_DOS )
\ date and time values
   \ need to call directory-fine-first or directory-fine-next word
   \ before usning this word
   ptr-DOS-time rel>abs ptr-DOS-date rel>abs \ ptr's to 16 bit DOS
                                        \ time & date
   _win32-find-data rel>abs 1 cells+
   call FileTimeToDosDateTime drop ;

: get-DOS-access-datetime ( -- ;convert 64 bit file time to MS_DOS )
\ date and time values
   \ need to call directory-fine-first or directory-fine-next word
   \ before usning this word
   ptr-DOS-time rel>abs ptr-DOS-date rel>abs \ ptr's to 16 bit DOS
                                        \ time & date
   _win32-find-data rel>abs 3 cells+
   call FileTimeToDosDateTime drop ;

: get-DOS-write-datetime ( -- ;convert 64 bit file time to MS_DOS )
\ date and time values
   \ need to call directory-fine-first or directory-fine-next word
   \ before usning this word
   ptr-DOS-time rel>abs ptr-DOS-date rel>abs \ ptr's to 16 bit DOS
                                          \ time & date
   _win32-find-data rel>abs 5 cells+
   call FileTimeToDosDateTime drop ;

: get-file-size ( -- size )
   \ need to call directory-fine-first or directory-fine-next word
   \ before usning this word
   _win32-find-data 8 cells+ @ ;

: get-file-name ( -- adr; address for file name )
   \ need to call directory-fine-first or directory-fine-next word
   \ before usning this word
   _win32-find-data 11 cells+ ;

create file-time-buf 2 cells allot

: get-file-modified ( fileid -- system-time )
        >r
        file-time-buf 2 cells erase     \ pre-clear buffer
        file-time-buf rel>abs           \ address of where to put the file's
                                        \ last written time and date
        0                               \ last access time not needed
        0                               \ creation time not needed
        r> call GetFileTime drop

        _systemtime     rel>abs         \ where to put results
        file-time-buf   rel>abs         \ file time/date to convert
        call FileTimeToSystemTime drop
        _systemtime ;

: filetime-to-systemtime ( cell# -- ; convert file time to system time )
   \ need to call directory-fine-first or directory-fine-next word
   \ before usning this word
   \ cell# is offset in number of cells to _win32-find-data
   \ 1 for creation time; 3 for access time; and 5 for write time
   \ normall 5 should be used
   >r                                           \ cell# -> rstack
   _systemtime rel>abs                          \ struct FileTimeToSystemTime
                                                \ function
   _win32-find-data rel>abs r> cells+           \ struct FILETIME for
                                                \ ftWriteTime
   call FileTimeToSystemTime drop ;

: asciiz->asc-len ( adrz -- sadr slen )
        MAX-PATH 2dup 0                         \ adrz scan-len scan-let -
        scan                                    \ adr0 len-rem -
        nip - ;                                 \ slen -

: .dir->file-name ( -- ;print file name in the dir )
        _win32-find-data 11 cells+              \ adrz
        asciiz->asc-len                         \ adrz scan-len slen
        dup>r type                              \ adrz len  ;print file name
        12 r> - 0max spaces ;


: .dir->file-size ( -- ;print file size )
                _win32-find-data @ FILE_ATTRIBUTE_DIRECTORY and
                if   >bold   ."          dir " >norm
                else _win32-find-data 8 cells+ @ dup 12 u,.r space \ print nFileSizeLow
                     +to total-file-bytes
                then ;

: _print-dir-files ( adr slen -- )
                0 to total-file-bytes
                0 to #files                          \ reset # of files in dir
                find-first-file nip                  \ adrd ior - find first file
                0 <> if exit then                    \ if file is not found, exit
                cr .dir->file-size                   \ print the size of file
                   .dir->file-name                   \ print the filename found
                1 +to #files                         \ update file number
                BEGIN find-next-file nip 0=          \ ior - find next file
                WHILE   1 +to #files                   \ update file number
                        25 ?cr
                        .dir->file-size                \ print the size of file
                        .dir->file-name                \ and the name of file
                        start/stop
                REPEAT ;

: print-dir-files ( adr slen -- )
                cr >bold ." Directory of: " 2dup type >norm
                ['] _print-dir-files catch
                IF      2drop                   \ discard abort results
                THEN
                find-close drop
                cr >bold #files . ." Files displayed, using "
                total-file-bytes 1 u,.r ."  bytes of disk." >norm ;

: dir           ( -<name>- )    \ name = f.e where f or e can be ascii's, * , or ?
                /parse-word dup c@ 0=                    \ if not spec given, use *.*
                IF   s" *.*" pocket place
                THEN dup count + 1- c@ ':' =         \ if just a drive, add \
                IF   s" \"   pocket +place
                THEN dup count + 1- c@ '\' =         \ if it ends in a \, add *.*
                IF   s" *.*" pocket +place
                THEN count print-dir-files ;

: do-rename-afile { RenamePart1$ RenamePart2$ \ RenameTemp$ -- } \ rename one file
                MAXSTRING LocalAlloc: RenameTemp$
                _win32-find-data 11 CELLS+                      \ adrz
                asciiz->asc-len                                 \ adrz -- adr len
                2dup 2dup RenamePart1$ count caps-search
                IF      2dup 2>r nip -              RenameTemp$  place \ leading part
                        RenamePart2$ count          RenameTemp$ +place
                        2r> RenamePart1$ c@ /string RenameTemp$ +place
                        RenameTemp$ count rename-file ( adr1 len adr2 len -- ior=0=OK )
                        0=
                        IF      1 +to #files                    \ update file number
                        THEN
                ELSE    2drop 2drop 2drop
                THEN    ;

: rename        ( -<name1 part1 part2>- )     \ rename file or files
                { \ RenameFrom$ RenamePart1$ RenamePart2$ -- }
                MAXSTRING LocalAlloc: RenameFrom$
                MAXSTRING LocalAlloc: RenamePart1$
                MAXSTRING LocalAlloc: RenamePart2$
                /parse-word count RenameFrom$  place
                /parse-word count RenamePart1$ place
                /parse-word count RenamePart2$ place
                RenameFrom$  c@ 0>
                RenamePart1$ c@ 0> and
                RenamePart2$ c@ 0> and 0=
                IF      beep
                        cr ." Usage:"
                        cr ."    RENAME <filespec> <oldsubstring> <newsubstring>"
                        EXIT                    \ leave if no names specified
                THEN        
                cr >bold ." Rename Files: "
                RenameFrom$  count type ."  from: "
                RenamePart1$ count type ."  To: "
                RenamePart2$ count type >norm
                0 to #files                                     \ reset # of files in dir
                RenameFrom$ count find-first-file nip 0=        \ adrd ior - find first file
                IF      RenamePart1$
                        RenamePart2$
                        do-rename-afile
                        BEGIN   find-next-file nip 0=           \ ior - find next file
                        WHILE   RenamePart1$
                                RenamePart2$
                                do-rename-afile
                        REPEAT find-close drop
                        cr >bold #files . ." Files renamed" >norm
                ELSE    cr ." No matching files Found"          \ if file is not found, exit
                THEN    ;


