\ DBGSRC2.F    Add Source Level Debugging to Win32Forth    by Tom Zimmer

only forth also definitions

INTERNAL

create SrcInfoName MAXSTRING allot
       SrcInfoName off

: _DoLoadInfo  { \ temp$ -- }    \ load the debugger information from disk
        MAXSTRING LocalAlloc: temp$
        temp$ off
        UnPointer> SrcInfoPtr           \ release any memory already allocated
        &app-size @ SizeOf!> SrcInfoPtr \ set the new size
        1 SrcInfoPtr !                  \ mark as not loadable initiallly
\        cr ." Loading Source Information File: "
\        SrcInfoName count type
        SrcInfoName count                      r/w open-file dup
        if      2drop
                SrcInfoName count "to-pathend" r/w open-file
        then
        dup
        if      2drop
                &prognam count "path-only"     temp$ place
                                               temp$ ?+\
                SrcInfoName count "to-pathend" temp$ +place
                temp$ count                    r/w open-file
        then
        0=
        if      >r
                SrcInfoPtr SrcInfoCnt cells r@ read-file 2drop
                r> close-file drop
        else    drop
        then    ;

' _DoLoadInfo is DoLoadInfo

EXTERNAL

: LoadInfo      ( -<name>- )
        /parse-word count SrcInfoName place
        _DoLoadInfo ;

: SaveInfo      ( -<name>- )    \ save the debugger information to disk
\        cr ." Saving Source Pointer Information for: " SrcInfoCnt .
\        ."  locations."
\        cr ." To: "
        /parse-word count SrcInfoName place
\        SrcInfoName count type
        SrcInfoName count r/w create-file abort" Failed to create Info File"
        >r
        SrcInfoPtr SrcInfoCnt cells r@ write-file abort" Failed to write Info File"
        r> close-file drop ;

INTERNAL

: buf-type      ( a1 n1 -- )     ed-dbgline  +place ;
: buf-emit      ( c1 -- )        ed-dbgline c+place ;

: _word-watch   { ip@ -- }      \ ip@ is address we want shown in source
                with-source?                            \ if we want to show source
                editor-present? and                     \ and if editor is loaded
                if      SrcInfoPtr @ 0=
                        if      _DoLoadInfo
                        then
                        SrcInfoPtr @ -1 =               \ if its in memory
                        if      SrcInfoPtr SrcInfoCnt ip@ lscan
                                if      dup cell + @
                                        ?dup            \ only if source is available
                                        if      word-split ed-column !
                                                           ed-line !
                                                0 ED_WORD win32forth-message
                                        then
                                then    drop
                        then
                then
                editor-present? 0= ?EXIT
                defer@ type >r ['] buf-type is type
                defer@ emit >r ['] buf-emit is emit
                ed-dbgline off
[ also bug also hidden ]
                .smax @ >r 3 .smax !    \ limit stack display to 3 items
                debug-.s                \ display stack
                r> .smax !              \ restore stack display limit
                0x0D emit               \ use CR as a separator
                .wordtype
                nesting @ 0max ?dup
                if      0 do ." |" loop space
                then
                                        \ display word to be executed
                debugging? >r true to debugging?
                ip@ dup @ .execution-class drop
                r> to debugging?
                                        \ if its EXECUTE, display target word
                ip@ @ execute?
                if      ." [ " stack-top .name ." ]"
[ previous ]    then
                r> is emit
                r> is type
                0 ED_DEBUG win32forth-message ;

' _word-watch is word-watch

: rst-type      ( a1 n1 -- )    ed-return  +place ;
: rst-emit      ( c1 -- )       ed-return c+place ;

-1 value prev-return

: _stack-watch  { \ stack-cnt -- }
                editor-present? 0= ?EXIT
                1 to stack-cnt
                ed-stack 64 cells erase                 \ null init
                depth ed-stack !                        \ set the counter
                depth 0>
                if      depth 63 min
                        BEGIN   dup pick ed-stack stack-cnt cells+ !
                                1 +to stack-cnt
                                1- dup 0=
                        UNTIL   drop
                then prev-return return-top -   \ if return stack has changed
                if      defer@ type >r ['] rst-type is type
                        defer@ emit >r ['] rst-emit is emit
                        ed-return off
[ also bug also hidden ]
                        return-top      \ if returnstack is set
                        if      return-top 1 cells +
                                rp0 @ return-top 12 cells + min
                                #dbg-rstack
                                return-top to prev-return
[ previous ]            then
                        r> is emit
                        r> is type
                then
                0 ED_STACK win32forth-message ;

' _stack-watch is stack-watch

MODULE

