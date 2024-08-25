\ load print replacements for xcalls

\ xprtdlg.f beta 3.1A 2002/09/28 arm replace xcalls for print dialogs

cr .( Loading Print Dialog Functions...)
cr .( -- BETA XPRTDLG.F V3.1A --)

\ \ \ incomplete

\ ------------------- Common Print Dialog funcs ----------------------

WINLIBRARY COMDLG32.DLL

1 PROC PrintDlg
1 PROC StartPage
1 PROC EndPage
1 PROC EndDoc

1 PROC DeleteDC \ in gdi32.dll


create pd-struct 66 dup , allot
false value pd-hDC

: pdlg-build    ( winh -- )
        pd-hDC not if
          pd-struct lcount erase          \ clear out the struct
          pd-struct cell+ !               \ hwndOwner
          1 999 word-join
          dup  pd-struct 6 cells+ !       \ nMinPage/nMaxPage
               pd-struct 7 cells+ !       \ nFromPage/Topage
          1    pd-struct 8 cells+ W!      \ nCopies
          PD_RETURNDC
            PD_HIDEPRINTTOFILE or
            PD_PAGENUMS or
            PD_NOSELECTION or
            PD_USEDEVMODECOPIES or
            PD_PRINTSETUP or
          pd-struct 5 cells+ !            \ flags
        then
        ;

: print-setup   ( window_handle -- PrintDC )
\ was                39 xcall ;
          pdlg-build
          pd-struct rel>abs call PrintDlg 0=
          if call CommDlgExtendedError
            ?dup if ." Error: PrintDlg failed RC=0x" h.
              abort
            else
              0                           \ no hdc to return
            then
          else
            pd-struct 4 cells+ @          \ return hDC
            dup to pd-hDC
          then
          ;

: print-start   ( -- )                  \ start printing a new page & doc
\ was                40 xcall drop ;
          ;

: start-page    ( -- )
\ was                 58 xcall drop ;
          pd-hDC call StartPage drop ;

: end-page      ( -- )
\ was               59 xcall drop ;
          pd-hDC call EndPage drop ;

: print-page    ( -- )                  \ finish current page start new page
\ was                41 xcall drop ;
          end-page start-page ;

: print-end     ( -- )                  \ finish printing page and doc
\ was                42 xcall drop ;
          pd-hDC call EndDoc drop ;

: print-init    ( -- printDC )          \ initialize the printer, return DC
\ was                0 43 xcall ;
          0 ;

                                        \ rls February 4th, 2002 - 20:24
: print-init2   ( bitmapped flags topage -- printDC )
\ was                 0 77 xcall ;            \ initialize the printer, return DC
          3drop 0 ;

: auto-print-init ( -- printDC )        \ initialize the printer, return DC
\ was                 1 43 xcall ;
          0 ;

                                        \ rls February 4th, 2002 - 5:47
: print-flags   ( -- flag )             \ true if selection radio button chosen
\ was                78 xcall ;
          pd-struct 5 cells+ @ ;

: print-close   ( -- )                  \ close the printer
\ was                44 xcall drop ;
          pd-hDC ?dup if
            call DeleteDC drop
            false to pd-hDC
          then
          ;

: quality-print ( -- n1 )       \ return the print quality code
\ was                57 xcall ;
          0 ;

: get-copies    ( -- n1 )
\ was                60 xcall ;
          pd-struct 8 cells+ w@ ;

: get-frompage  ( -- n1 )
\ was                61 xcall ;
          pd-struct 7 cells+ @ word-split drop ;

: get-topage    ( -- n1 )
\ was                62 xcall ;
          pd-struct 7 cells+ @ word-split nip ;


: PRINT-ORIENTATION ( f1 -- hDC ) \ true = landscape
\ was                orientation_x XCALL ;   \ 67 xcall ;
          drop 0 ;

