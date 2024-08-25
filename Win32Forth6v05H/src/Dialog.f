\ $Id: dialog.fth 1.4 1994/03/31 15:04:47 andrew Exp $
\ dialog.f BETA 3.3E 2002/10/11 arm Change load to res\<filename>.res, use malloc/release
\ dialog.f BETA 4.9C 2002/11/09 rbs Crashed on errors in find-dialog-id, fixed

cr .( Loading Dialog Box...)
cr .( -- BETA DIALOG.F V4.9C --)

\ -------------------- Load Dialog Resource File --------------------

((
The .RES file structure is a series of records.  Each record contains
a header and a data field.  The structure of a header is as follows:

        offset  length
          0       4     length of data field
          4       4     length of header
         10       2     record type
         14       2     dialog ID number (for dialogs)
))


\ Given the address of a header in a resource file, return true if this
\ is the header for a dialog resource.  I'm only guessing here.

: dialogID?     ( hdr ID -- f )
                over 14 + w@   =                \ does ID match
                swap 10 + w@ 5 = and ;          \ is this also a dialog

\ Find dialog template given address and length of resource file in memory.

: ?dlgerr  ( ior -- )   abort" Error loading dialog resource" ;

\ April 18th, 1996 tjz switched to LONG count from WORD count
: find-dialog-ID ( id addr -- address-of-template-header )
                swap >r
                lcount
                begin   over r@ dialogID?
                        if      r>drop                  \ discard the ID
                                drop                    \ discard the length
                                \ return the template header address
                                EXIT                    \ ALL DONE, LEAVE
                        then
                        over 2@ + aligned
                        /string dup 0=
                until   2drop r>
                cr ." Looking for dialog: " . true ?dlgerr ;

\ Read resource file and return address and length of dialog template.

\ April 18th, 1996 tjz switched to LONG count from WORD count
: read-dialog   ( name namelen -- )
                "path-file ?dlgerr R/O open-file ?dlgerr >r
                r@ file-size 2drop here !
                here lcount dup cell+ allot       \ room for file and word cnt
                r@ read-file ?dlgerr 0= ?dlgerr
                r> close-file ?dlgerr ;

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ Load template from dialog resource to here and allot memory
\ Usage:  load-dialog dialog.res
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

create ld-buf maxstring allot   \ needs to be long enough to hold a path too

: load-dialog   ( -<filename-without-an-extension>- )
                >in @ >r                        \ save the input pointer
                bl word c@ name-max-chars >     \ check filename length
                abort" Dialog files are limited to 63 chars"
                r> >in !                        \ restore the input pointer
                s" res\"   ld-buf  place        \ add directory res\
                create last @ nfa-count         \                     name length
                2dup       ld-buf +place        \ lay in filename
                s" .res"   ld-buf +place        \ add extension       name.res
                ld-buf count read-dialog        \ load resource file
                s" fload res\" ld-buf  place        \ load header file
                ( a1 n1 )  ld-buf +place        \ Append filename
                s" .h"     ld-buf +place        \ add extension       name.h
                ld-buf count evaluate           
                postpone \ ;                    \ ignore rest of line


\ -------------------- Dialog Class --------------------

:CLASS Dialog   <SUPER generic-window

: (DialogProc)   ( hwnd msg wparam lparam -- res )
        [ also classes ]

        GWL_USERDATA 4 pick Call GetWindowLong  ( object address )

        ?dup 0=
        if
                2 pick WM_INITDIALOG <>
                if
                        0 exit
                then

                dup             \ window object pointer from
                                \ lparam of DialogBoxIndirectParam

                4 pick ( obj hwnd )
                2dup GWL_USERDATA swap Call SetWindowLong drop  \ save obj pointer
                over !          \ set hWnd parameter of window struc
                
        then

        3 pick ( msg ) over obj>class MFA ((findm))
        if
                execute
        else
                0
        then

        [ previous ] ;

4 callback DialogProc (DialogProc)
\ ' (DialogProc) WndProc DialogProc


\ TEMPLATE has been changed to be the template header address, instead of
\ the address of the template it self, so we can move the template into
\ globally allocated memory for Win32S

: run-dialog  { parent template \ tmplhndl -- f }
        self
        DialogProc rel>abs
        parent 0 <>                     \ if parent is not zero
        parent conhndl <> and           \ and parent is not the console handle
        if      GetHandle: parent       \ then use the specified parent
        else    conhndl                 \ else use the console for the parent
        then
        template 2@ + malloc to tmplhndl
        template dup cell+ @ +                  \ from
        tmplhndl template @ move                \ move the length
        tmplhndl rel>abs                        \ new way, template handle
        appInst
        Call DialogBoxIndirectParam
        tmplhndl release ;

\ -------------------- Helpers --------------------


:M Start:       ( parent -- flag )
                GetTemplate: [ self ] run-dialog
                ;M

:M EndDialog:   ( return-value -- )
                hwnd Call EndDialog drop
                ;M

: end-dialog    ( value -- flag )
                EndDialog: [ self ] 1 ;


\ -------------------- Initialization --------------------

:M WM_INITDIALOG  swap On_Init: [ self ] ;M

:M On_Init:  ( hwndfocus -- f )  drop 1 ;M


\ -------------------- Process Commands from Controls --------------------

:M WM_COMMAND  ( hwnd msg wparam lparam -- res )
        over HIWORD ( notification code ) rot LOWORD ( ID )
        On_Command: [ self ] ;M

:M On_Command:  ( hCtrl code ID -- f )
        case

          IDOK of
                1 end-dialog
          endof

          IDCANCEL of
                0 end-dialog
          endof

          false swap ( default result )

        endcase ;M

;Class

VARIABLE Modeless-link
         Modeless-link OFF

:Class ModelessDialog   <SUPER Dialog

      int hTemplate

: trim-modeless  ( nfa -- nfa )
\in-system-ok   dup Modeless-link full-trim ;

\in-system-ok forget-chain chain-add trim-modeless

:M ClassInit:   ( -- )
                ClassInit: super
                0 to hTemplate
                Modeless-link link,   \ link into list
                self ,
                ;M

:M WindowStyle: ( -- n1 )
                GetTemplate: [ self ] dup
                if      dup cell+ @ + @
                then
                ;M

:M ExWindowStyle: ( -- n1 )
                GetTemplate: [ self ] dup
                if      dup cell+ @ + cell+ @
                then
                ;M

:M Origin:      ( -- x y )
                GetTemplate: [ self ] ?dup
                if      dup cell+ @ + 2 cells+ 2 + @ word-split
                else    0 0
                then
                ;M


: run-modeless-dialog  { parent template \ tmplhndl -- hWnd }
                self
                DialogProc rel>abs
                parent 0 <>                      \ if parent is not zero
                parent conhndl <> and            \ and parent is not the console handle
                if      GetHandle: parent        \ then use the specified parent
                else    conhndl                  \ else use the console for the parent
                then
                template 2@ + malloc to tmplhndl
                template dup cell+ @ +           \ from
                tmplhndl template @ move         \ move the length
                  WindowStyle: [ self ]    tmplhndl       !
                ExWindowStyle: [ self ]    tmplhndl cell+ !
                Origin: [ self ] word-join tmplhndl 2 cells+ 2 + !
                tmplhndl rel>abs                 \ new way, template handle
                appInst
                Call CreateDialogIndirectParam
                SW_SHOW over Call ShowWindow drop
                dup Call UpdateWindow drop
                dup Call SetFocus drop
                tmplhndl ;

:M Start:       (  parent -- )
                hTemplate 0=
                if      GetTemplate: [ self ]
                        run-modeless-dialog to hTemplate to hWnd
                else    drop
                        SetFocus: self
                then
                ;M

:M EndDialog:   ( n1 -- )
                drop        
                DestroyWindow: self 
                ;M

:M WM_DESTROY   ( -- result )
                hTemplate release
                0 to hTemplate
                0 ;M

:M WM_CLOSE     ( -- )
                DestroyWindow: self
                ;M

: DoModelessMsg { pMsg flag \ mlink -- pMsg f | pMsg FALSE }
                Modeless-link @                 \ all modeless dialog handles
                begin   dup 0<>                 \ while not end of chain
                        flag and                \ and haven't found a handler
                while   dup cell+ @ to mlink >r
                        flag Gethandle: mlink 0<> and
                        if      pMsg Gethandle: mlink
                                Call IsDialogMessage 0= to flag
                        then
                        r> @
                repeat  drop pMsg flag ;

msg-chain chain-add DoModelessMsg

;Class





