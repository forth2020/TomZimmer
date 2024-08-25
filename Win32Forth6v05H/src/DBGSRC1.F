\ DBGSRC1.F    Add Source Level Debugging to Win32Forth    by Tom Zimmer

TRUE value with-source?

app-free Pointer SrcInfoPtr     \ an array of information values
0        value   SrcInfoCnt     \ count of cells in array

defer DoLoadInfo        ' noop is DoLoadInfo

: SrcInfo,      ( n1 -- )       \ compile a number into the info array
                SrcInfoPtr SrcInfoCnt cells+ !
                1 +to SrcInfoCnt ;

-1 SrcInfo, 0 SrcInfo,          \ initialize to presently in memory

: _ResetSrcInfo  { ip@ -- }
                SrcInfoPtr @ -1 =               \ if its in memory
                if      SrcInfoPtr SrcInfoCnt ip@ lscan nip
                        negate +to SrcInfoCnt   \ remove remaining length
\                else    drop   \ May 20th, 1998 tjz removed line
                then    ;

' _ResetSrcInfo is ResetSrcInfo

0 value SrcInfoHere

: _save-source  ( -- )
                with-source? 0= ?EXIT   \ exit if not saving source markers
                SrcInfoPtr @ 0=
                if      DoLoadInfo
                then
                SrcInfoPtr @ -1 <> ?EXIT        \ leave if it isn't in memory
                here to SrcInfoHere             \ save for comparison later
                here SrcInfo,                   \ save the address
                loadline @                      \ and source line
                >in @ pocket c@ - 0max          \ and the source column
                word-join SrcInfo, ;            \ merged into a 32bit number

: _save-src     ( -- )
                state @      0= ?EXIT   \ exit if not compiling
                _save-source ;

: _?unsave-src  ( -- )
                state @          0= ?EXIT \ exit if not compiling
                with-source?     0= ?EXIT \ exit if not saving source markers
                here SrcInfoHere <> ?EXIT \ discard if we didn't compile anything
                SrcInfoCnt 2 - 0max to SrcInfoCnt ;

' _save-source  is save-source
' _save-src     is save-src
' _?unsave-src  is ?unsave-src



