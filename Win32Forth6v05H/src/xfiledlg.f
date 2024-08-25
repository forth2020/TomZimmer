\ load print/open replacements for xcalls

\ xfiledlg.f beta 3.1A 2002/09/28 arm replace xcalls for file open/save/new dialogs
\ xfiledlg.f beta 3.3D 2002/10/08 arm Consolidation

cr .( Loading Open Dialog Functions...)
cr .( -- BETA xfiledlg.F V3.1A --)

\ ------------------- Common Open/Save/New Dialog funcs ----------------------

WINLIBRARY COMDLG32.DLL

1 PROC GetOpenFileName
1 PROC GetSaveFileName
1 PROC CommDlgExtendedError

create ofn-struct 19 cells , 22 CELLS allot       \ OPENFILENAME struct
\ This struct can be 22 cells for Windows 2000/XP, but NT or less demands 19?
\ Lowest common denominator - have gone for 19.

: fdlg-filter ( abs-addr -- abs-addr )       \ change all | to \0 in filter
        abs>rel dup                          \ back to rel address
        begin dup c@ ?dup                    \ fetch char
          while                              \ if not end of string
            [char] | = if 0 over c! then     \ make a \0
            char+                            \ next char
          repeat drop                        \ loose addr
        rel>abs                              \ back to abs
        ;

: fdlg-build    ( filename diraddr titleaddr specaddr owner -- )
        ofn-struct lcount erase                       \ clear structure
        ofn-struct cell+   !                          \ save owner in hwnd
        fdlg-filter                                   \ modify filter
        ofn-struct 3  cells+ !                        \ save filter in filter string
        1 ofn-struct 6  cells+ !                      \ filterindex=1
        ofn-struct 12 cells+ !                        \ save title
        ofn-struct 11 cells+ !                        \ save initial dir
        1+ ofn-struct 7 cells+ !                      \ save initial filename
        maxcounted  ofn-struct  8 cells+ !            \ file length
        ;
        
: fdlg-getfile  ( -- filename )                  \ fetch filename
        ofn-struct 7 cells+ @ abs>rel            \ fetch returned filename
        ;

: fdlg-adjfile  ( -- filename )                  \ adjust filename returned
        fdlg-getfile                             \ fetch returned filename
        dup maxcounted 0 scan drop               \ find end of string
        over - over 1- c! 1- rel>abs             \ adjust filename to cstr
        ;
        
: fdlg-nofile   ( -- filename )                  \ return null filename
        fdlg-getfile                             \ fetch returned filename
        1- 0 over c! rel>abs                     \ null string
        ;
        
: fdlg-openf    ( -- )                       \ set open flags in struct
        OFN_PATHMUSTEXIST   OFN_FILEMUSTEXIST OR
        OFN_HIDEREADONLY OR OFN_SHAREAWARE    OR
        ofn-struct 13 cells+ W!                       \ flags
        ;
        
: fdlg-newf    ( -- )                       \ set open flags in struct
        OFN_PATHMUSTEXIST
        OFN_HIDEREADONLY OR OFN_SHAREAWARE    OR
        ofn-struct 13 cells+ W!                       \ flags
        ;
        
: fdlg-open     ( addr -- rc )
        call GetOpenFileName ;
        
: fdlg-savef    ( -- )                       \ set save flags in struct
        OFN_OVERWRITEPROMPT OFN_HIDEREADONLY OR
        ofn-struct 13 cells+ W!                       \ flags
        ;

: fdlg-save     ( addr -- rc )
        call GetSaveFileName ;

: fdlg-call     ( xt -- filename )               \ call GetxxxxFileName
        ofn-struct rel>abs swap execute 0=
        if call CommDlgExtendedError
          ?dup if ." Error: GetxxxxFileName failed RC=0x" h.
            abort
          else
            fdlg-nofile                          \ no file to return
          then
        else
          fdlg-adjfile                           \ return filename
        then
        ;

: open-dialog   ( filename diraddr titleaddr specaddr owner -- filename )
\ was               45 xcall ;
        fdlg-build                               \ build ofn-struct
        fdlg-openf                               \ set open flags
        ['] fdlg-open fdlg-call                  \ call dialog
        ;

                                        \ rls February 4th, 2002 - 5:47
: open-dialog2  ( filterindx filenam diradr titleadr specadr owner -- filename )
\ was               75 xcall ;
        fdlg-build
        ofn-struct 6 cells+ !                    \ set filter index
        fdlg-openf                               \ set open flags
        ['] fdlg-open fdlg-call                  \ call dialog
        ;

: get-filter-Index   ( -- n )
\ was               79 xcall ;
        ofn-struct 6 cells+ @                    \ get filter index
        ;

: save-dialog   ( filename diraddr titleaddr specaddr owner -- filename )
\ was               46 xcall ;
        fdlg-build
        fdlg-savef                               \ set save flags
        ['] fdlg-save fdlg-call                  \ call dialog
        ;

: new-dialog    ( filename diraddr titleaddr specaddr owner -- filename )
\ was               55 xcall ;
        fdlg-build                               \ build ofn-struct
        fdlg-newf                                \ set new flags
        ['] fdlg-open fdlg-call                  \ call dialog
        ;

                                \ rls February 4th, 2002 - 20:18
: new-dialog2   ( filterindex filename diraddr titleaddr specaddr owner --
                                                                    filename )
\ was               76 xcall ;
        fdlg-build
        ofn-struct 6 cells+ !                    \ set filter index
        fdlg-newf                                \ set new flags
        ['] fdlg-open fdlg-call                  \ call dialog
        ;


