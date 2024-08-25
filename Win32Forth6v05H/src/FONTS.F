\ FONTS.F       Font Class and Methods

\ Font creation Class and control methods

VARIABLE font-link      \ Global linked list of font objects
         font-link OFF

:Class Font     <Super Object

int handle              \ the handle to the created font

Record: LOGFONT         \ structure that holds
                int   lfHeight          \ width  in pixels, device specific
                int   lfWidth           \ height in pixels, device specific
                int   lfEscapement
                int   lfOrientation     \ in 10ths of a degree
                int   lfWeight
                byte  lfItalic          \ TRUE/FALSE
                byte  lfUnderline       \ TRUE/FALSE
                byte  lfStrikeOut       \ TRUE/FALSE
                byte  lfCharSet
                byte  lfOutPrecision
                byte  lfClipPrecision
                byte  lfQuality
                byte  lfPitchAndFamily
    LF_FACESIZE bytes lfFaceName        \ the font name
;Record

: trim-fonts  ( nfa -- nfa )
\in-system-ok   dup font-link full-trim ;

\in-system-ok forget-chain chain-add trim-fonts

:M ClassInit:   ( -- )
                 0                  to handle           \ clear handle
                14                  to lfHeight
                 9                  to lfWidth
                 0                  to lfEscapement
                 0                  to lfOrientation    \ in 10th degrees
                FW_DONTCARE         to lfWeight
                FALSE               to lfItalic
                FALSE               to lfUnderline
                FALSE               to lfStrikeOut
                ANSI_CHARSET        to lfCharSet
                OUT_TT_PRECIS       to lfOutPrecision
                CLIP_DEFAULT_PRECIS to lfClipPrecision
                PROOF_QUALITY       to lfQuality
                FIXED_PITCH                             \ font pitch
                0x04 or                                 \ use TrueType fonts
                FF_SWISS or         to lfPitchAndFamily \ font family
                lfFaceName LF_FACESIZE erase            \ clear font name
                s" Courier New" lfFaceName swap move    \ move in default name
                font-link link,                         \ link into list
                self ,                                  \ so we can send
                ;M                                      \ ourself messages

:M Height:         ( n1 -- )  to lfHeight          ;M
:M Width:          ( n1 -- )  to lfWidth           ;M
:M Escapement:     ( n1 -- )  to lfEscapement      ;M
:M Orientation:    ( n1 -- )  to lfOrientation     ;M   \ 10th/degree increments
:M Weight:         ( n1 -- )  to lfWeight          ;M
:M Italic:         ( f1 -- )  to lfItalic          ;M   \ TRUE/FALSE
:M Underline:      ( f1 -- )  to lfUnderline       ;M   \ TRUE/FALSE
:M StrikeOut:      ( f1 -- )  to lfStrikeOut       ;M   \ TRUE/FALSE
:M CharSet:        ( n1 -- )  to lfCharSet         ;M
:M OutPrecision:   ( n1 -- )  to lfOutPrecision    ;M
:M ClipPrecision:  ( n1 -- )  to lfClipPrecision   ;M
:M Quality:        ( n1 -- )  to lfQuality         ;M
:M PitchAndFamily: ( n1 -- )  to lfPitchAndFamily  ;M

:M SetFaceName: ( a1 n1 -- )
                lfFaceName LF_FACESIZE erase
                LF_FACESIZE 1- min lfFaceName swap move
                ;M

:M GetFaceName: ( -- a1 n1 )
                lfFaceName LF_FACESIZE 2dup 0 scan nip -
                ;M

:M Delete:      ( -- )
                handle ?dup
                if      Call DeleteObject ?win-error
                        0 to handle
                then
                ;M

:M Create:      ( -- )
                Delete: self
                LOGFONT rel>abs Call CreateFontIndirect to handle
                ;M

:M Handle:      ( -- HFONT )
                handle
                ;M

\ **************** INTERNAL SYSTEM FUNCTIONS FOLLOW ****************
\ The following functions and methods make sure that any font objects
\ create in your application get reset at system startup, and deleted
\ when Win32Forth closes.

:M zHandle:     ( -- )          \ zero the font handle
                0 to handle
                ;M

: zero-fonts    { \ flink -- }
                font-link @             \ clear all font handles
                begin   dup
                while   dup cell+ @ to flink
                        zHandle: flink
                        @
                repeat  drop ;

initialization-chain chain-add zero-fonts

: delete-fonts  { \ flink -- }
                font-link @             \ clear all font handles
                begin   dup
                while   dup cell+ @ to flink
                        Delete: flink
                        @
                repeat  drop ;

unload-chain chain-add delete-fonts

;Class

