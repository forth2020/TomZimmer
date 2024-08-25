
\ SysLoad.f - Loads file from Src\LIB\LibName if not already loaded -- also
\ checks DefaultLib if not found on first pass.

((

    Usage: SysLoad Filename1.f Libname\Filename2.f ...
        If no Libname it looks for the file in the default lib

))

#ifndef Prepend<home>\

    \ If not using v6.xx this needs to be defined here.
    create <home$> maxstring allot

    : Prepend<home>\    ( addr len -- addr' len' )
        &Forthdir count <home$> place <home$> +place
        <home$> count ;

#then


\ The DefaultLib$ can be pointed to a new default at run time.  Utils
\ is the norm.  Don't forget the backslash.

0 Value LibNestLevel
0 Value LibListLoaded?
Defer DefaultLib$
Defer ?Obsolete    ( Path Len -- )  \ Full Path Name and extension

    ' 2drop is ?Obsolete            \ init to do nothing

: DefaultLib1$ s" UTILS\" ;
    ' DefaultLib1$ dup is DefaultLib$ is-default DefaultLib$


create LibPath$ max-path allot

: >in[]     ( n -- char )
    \ gets n-th char from current position of input stream
    tib >in @ + + c@ ;


: (SysLoad)   ( |<subdir\>FileName1 <subdir\>FileName2... )
    { \ TryDefaultLib -- }
    \ save Src\Lib\LibName\Filename

    \ LibPrinting MUST be set to TRUE (-1) to printout dependencies
    \ otherwise we assume an error occurred on the last pass.
    LibListLoaded? TRUE =
    if      1 +to LibNestLevel
    else    0 dup to LibListLoaded?
            to LibNestLevel
    then

    begin
        LibListLoaded? 0<> to LibListLoaded?    \ restore flag to -1

        \ Check for end of line.
        >in @ >r bl word dup c@ 0<> r> >in !

        ( StringAddr NotEndFlag )

        \ Check for libname = NONE.  This is for symmetry in lib files.  Each
        \ Lib file can start with a SysLoad statement and those with no
        \ dependencies can also be documented this way.

        swap count s" NONE" caps-compare 0<>

        ( NotEndFlag StringsNotEqualFlag )

        and

    while
        begin
            \ Skip extra blanks in input stream until end of line
            0 >in[] bl <>
            ( NotBlankFlag )
            >in incr
            >in @ #tib @ >
            ( NotBlankFlag EndOfLineFlag )
            or
        until
        \ Back up one character
        >in decr

        \ Look for single char command in list.
        1 >in[] bl =
        if
            \ is the command an open paren or backslash?  Open paren
            \ aborts, backslash quits.
            0 >in[] '(' = abort" Can't Handle Parenthetic Comments In SysLoad"
            0 >in[] '\' =
            if  exit
            then
        then
        \ If not one of the single char commands, continues here.
        \ Init the TryDefaultLib flag and enter the next loop.
        0 to TryDefaultLib
        begin
            \ Init the Library path
            s" Src\Lib\" Prepend<home>\ LibPath$ place

            \ If not found on first try, add the default lib to the path
            TryDefaultLib
            if      DefaultLib$ LibPath$ +place
            then
            \ Add the filename from the input stream to the path.
            >in @
                bl word count LibPath$ +place
            >in !
            \ append ".f" if extension is missing
            LibPath$ ?DEFEXT
            \ check if already loaded
            LibPath$ count "TO-PATHEND"
            [ sys-warning-off also hidden ] "Loaded?
            [ previous sys-warning-on ]
            not
            ( NotLoadedFlag )
            if
                \ check if in list of obsolete files
                \ and issue warning if found.
                LibPath$ count ( addr len ) \ full path+ext
                ?Obsolete
                LibPath$ count "OPEN
                if
                    \ Enters here if there was an error trying to open file.
                    drop ( the bum handle )
                    \ Check if already looking at the DefaultLib (2nd pass)
                    \ and abort if really not found.
                    TryDefaultLib 0<>

                    if
                        \ Reset Printing flag for next pass
                        FALSE to LibListLoaded?
                        \ Issue error message with filename and abort.
                        LibPath$ count "TO-PATHEND" LibPath$ place
                        s" <---" LibPath$ +place
                        LibPath$ count "open drop true
                            Abort" Can't Open Lib File"
                    then

                    \ Otherwise, signal to check the Default Lib for a 2nd
                    \ pass at trying to open the file.
                    true to TryDefaultLib
                else
                    \ Enters here if there was no error opening the file.
                    LibListLoaded?
                    if      LibNestLevel 0max 0 ?do '*' emit loop ." > "
                            LibPath$ count type cr
                    then

                    INCLUDE-FILE

                    \ Signal that we are no longer trying to load the default
                    \ library so we can exit the loop.
                    0 to TryDefaultLib
                    \ And point to the next filename in the input stream
                    bl word drop

                    \ mark as having returned
                    LibListLoaded? 1 and to LibListLoaded?
                then
            else
                \ it's already loaded
                bl word drop
            then
            \ Loop back only if now trying the Default Lib (2nd pass)
            TryDefaultLib 0=
        until
    repeat
    0 word drop                 \ in case 'NONE' was used.
    \ reduce the nesting level indicator (***)
    -1 +to LibNestLevel
    ;


: sysload
    rp@ usermin userextra + - rp!
    handler rp@ usermin userextra + move
    \ op
    ['] (sysload) catch dup
    if      ( we are crashing ) rp@ handler usermin userextra + move
    then
    \ clean up and pass the error along
    usermin userextra + rp@ + rp!
    ( ErrID ) throw ;


SYNONYM Dependencies: SysLoad


: LibEdit
    s" edit src\lib\" new$ dup>r place
    bl word count r@ +place
    r> count evaluate ;

: LibList
    new$ >r
    s" dos del " r@ place

    s" liblist.txt" prepend<home>\ r@ +place

    r@ count evaluate
    100 ms
    s" dos copy " r@ place

    [char] " r@ c+place
    s" src=lib\" prepend<home>\ r@ +place
    bl word count -trailing r@ +place
    s" \*.F" r@ +place
    [char] " r@ c+place

    s"  " r@ +place

    [char] " r@ c+place
    s" liblist.txt" prepend<home>\ r@ +place
    [char] " r@ c+place

    r@ count evaluate
    1 seconds   \ give dos box a chance to finish up
    s" edit " r@ place
    s" liblist.txt" prepend<home>\ r@ +place
    r@ count evaluate
    r>drop
    ;


Defer .Dependencies: ( |LibFile -- )


: .SysLoad
    cr
    TRUE to LibListLoaded? SysLoad FALSE to LibListLoaded?
    ;

' .SysLoad is .Dependencies:



: DOS-SysLoad
    \ patch for Batch file parameter losing first char of library
    >in decr SysLoad ;


: Obsolete:
    \ For obsolete definitions in an otherwise good library.  Mostly for
    \ naming convention errors.
    create ' , immediate does>
    cr cr dup body> >name nfa-count type ."  is obsolete.  Use "
        @ >name nfa-count type
    key abort" Can't continue..." ;

: LibLink:  ( -<LibName>- )
    \ noop to allow filename as hyperlink if the file name is not
    \ the same as the main definition.  Add LibLink: to the words
    \ to scan for in WinEd.CFG
    0 word drop ;

\ s ----------------------------------- SysLoad.F

