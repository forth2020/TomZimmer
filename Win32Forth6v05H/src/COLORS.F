\ COLORS.F              The Colors Class                 by Tom Zimmer

cr .( Loading Object Color...)

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ Brushes and pens for defined colors are created at program initialization
\ time and are automatically deleted when the program terminates.
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

: RGB           ( red green blue -- colorref )
                PC_NOCOLLAPSE   \ flag             ( for NON-palette rgb value )
                256 * +         \ flag*256 + blue
                256 * +         \ flag*256 + blue*256 + green
                256 * + ;       \ flag*256 + blue*256 + green*256 + red

: PALETTERGB    ( red green blue -- colorref )
                PC_EXPLICIT     \ flag             ( for palette rgb value )
                256 * +         \ flag*256 + blue
                256 * +         \ flag*256 + blue*256 + green
                256 * + ;       \ flag*256 + blue*256 + green*256 + red

: PALETTEINDEX  ( index -- colorref )              \ return ref of system color
                PC_RESERVED 0x1000000 * or ;

variable colors-link            \ Global private link of all colors in system
         colors-link off

: .colors       { \ clink -- }  \ Displays the current set of defined
                cr              \ colors
                colors-link @
                begin   dup
                while   dup cell+ @ cell - body> .NAME
                        12 #tab space 12 ?cr
                        @
                repeat  drop ;

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ A utility word to check that an operation about to be performed is really
\ being done on a color object, helps prevent horrible crashes
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

: ?ColorCheck   ( a1 -- a1 )    \ verify that a1 is a color object address
                ?win-error-enabled 0= ?EXIT     \ leave if error checking is not enabled
                >r colors-link @
                begin   dup
                while   dup cell+ @ r@ =        \ match this color object?
                        if      drop r> EXIT    \ leave test, passed
                        then    @
                repeat  drop
                r>drop
                forth-io
                .rstack
                true Abort" This is not a ColorObject!" ;

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ Define the class of color objects. A color object returns its object
\ address if executed at the console, use "Color: <objectname>" to obtain
\ the color reference value of a color object.
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

:Class ColorObject  <Super Object 

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ internal initialization and un-initialization routines for color objects.
\ It seems Windows doesn't like a program that creates and destroys color
\ objects repeatedly as the program exewcutes, so we need to do this at
\ program start only.
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

: trim-colors   ( nfa -- nfa )
\in-system-ok   dup colors-link full-trim ;

\in-system-ok forget-chain chain-add trim-colors

: init-colors   { \ clink -- }          \ initialize brushes and pens
                colors-link @
                begin   dup
                while   dup cell+ @ to clink
                        ZeroColor: clink
                        InitColor: clink
                        @
                repeat  drop ;

initialization-chain chain-add init-colors

: uninit-colors { \ clink -- }          \ delete brushes and pens
                colors-link @
                begin   dup
                while   dup cell+ @ to clink
                        UnInitColor: clink
                        @
                repeat  drop ;

unload-chain chain-add-before uninit-colors

\ local data allocated for each color object that is defined.

int colorref            \ the actual color reference value
int hbrush              \ the brush handle
int hpen                \ the pen handle
int penwidth            \ width of the pen in pixels
int PenStyle            \ the style of the pen being created

:M ZeroColor:   ( -- )
                0 to hpen
                0 to hbrush
                ;M

:M UnInitColor: ( -- )
                hpen                    \ delete any already existing pen
                if      hpen   Call DeleteObject 1 ?win-error_plus
                        0 to hpen
                then
                hbrush                  \ delete any already existing brush
                if      hbrush Call DeleteObject 2 ?win-error_plus
                        0 to hbrush
                then    
                ;M

:M InitColor:   ( -- )  \ default initialization method, creates a pen
                        \ and a brush for each color object.
                UnInitColor: self       \ delete previous pens/brushes
                colorref penwidth PenStyle Call CreatePen        to hpen
                colorref                   Call CreateSolidBrush to hbrush
                ;M

:M ClassInit:   ( -- )  \ compile time initialization for each color object
                        \ as it is defined
                0 0 0 rgb to colorref
                0         to hpen
                0         to hbrush
                1         to penwidth
                PS_SOLID  to PenStyle
                colors-link link,
                InitColor: [ self ]
                self ,  ;M

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\               ******** Start of user methods ********
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

:M Color:       ( -- colorref )                 \ get the colorref value
                colorref ;M

:M Pen:         ( -- hpen )                     \ get the color pen handle
                hpen     ;M

:M Brush:       ( -- hbrush )                   \ get the color brush handle
                hbrush   ;M

:M NewColor:    ( colorref -- ) \ set a color object to a new color ref value
                dup colorref <>
                if      to colorref
                          InitColor: [ self ]   \ create the new  pens/brushes
                else    drop
                then    ;M

:M PenWidth:    ( pen_width -- )                \ set the pen width
                1 max dup penwidth <>
                if      to penwidth
                          InitColor: [ self ]
                else    drop
                then    ;M

;Class


\ ExtCreatePen is ONLY supported on Windows95 and WindowsNT, NOT Win32s.

:Class ExtColorObject  <Super ColorObject

int StyleCount
int lpStyle

 Record: &LOGBRUSH
        int lbStyle
        int lbColor
        int lbHatch
;Record

\ Use InitColor: to initialize an extended color object, after first
\ setting all the appropriate parameters of the extended color object,
\ using the dotted '.' notation.

:M InitColor:   ( -- )  \ default initialization method, creates a pen
                        \ and a brush for each color object.
                UnInitColor: self       \ delete previous pens/brushes
                colorref to lbColor     \ same as colorref
                lpStyle dup             \ if not NULL
                if      rel>abs         \ then convert to absolute address
                then
                StyleCount
                &LOGBRUSH rel>abs       \ LOGBRUSH address must be absolute
                penwidth PenStyle Call ExtCreatePen     to hpen
                colorref          Call CreateSolidBrush to hbrush
                ;M


:M ClassInit:   ( -- )  \ compile time initialization for each color object
                        \ as it is defined
                ClassInit: self
                \ init the LOGBRUSH structure to some defaults
                BS_SOLID  to lbStyle
                colorref  to lbColor
                NULL      to lbHatch
                \ init remaining parameters to defaults
                PS_GEOMETRIC
                PS_SOLID or
                PS_ENDCAP_ROUND or
                PS_JOIN_ROUND or to PenStyle
                0 to StyleCount
                NULL to lpStyle
                ;M


;Class


\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ Pseudo defining word for creating the color objects we always want to
\ have around.
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

: new-color     ( colorref -<name>- )   \ make a solid color
                >APPLICATION
                ColorObject                     \ define a new object
                NewColor: NewObject             \ and initialize it
                APPLICATION> ;

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ Now define the default color objects.
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

  0   0   0  palettergb new-color BLACK
 64  64  64  palettergb new-color DKGRAY
128   0   0  palettergb new-color RED
255   0   0  palettergb new-color LTRED
  0 128   0  palettergb new-color GREEN
  0 255   0  palettergb new-color LTGREEN
  0   0 128  palettergb new-color BLUE
  0   0 255  palettergb new-color LTBLUE
128 128   0  palettergb new-color YELLOW
255 255   0  palettergb new-color LTYELLOW
128   0 128  palettergb new-color MAGENTA
255   0 255  palettergb new-color LTMAGENTA
  0 128 128  palettergb new-color CYAN
  0 255 255  palettergb new-color LTCYAN
128 128 128  palettergb new-color GRAY
255 255 255  palettergb new-color WHITE
192 192 192  palettergb new-color LTGRAY
128  64   0  palettergb new-color BROWN  \ rda jap

: foreground    { color_object \ theDC -- }
                color_object ?ColorCheck drop
                conHndl call GetDC to theDC  \ get and save the Device Control #
                Color: color_object theDC Call SetTextColor drop
                theDC conHndl call ReleaseDC drop ;

: background    { color_object \ theDC -- }
                color_object ?ColorCheck drop
                conHndl call GetDC to theDC  \ get and save the Device Control #
                Color: color_object theDC Call SetBkColor drop
                theDC conHndl call ReleaseDC drop ;

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ A class for HATCHED colors, HatchColorObject is a superset of ColorObject
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

:Class HatchColorObject  <Super ColorObject

\ We just redefine the initialization method to work a little differently

:M InitColor:   ( -- )                  \ define color as a hatched color
                UnInitColor: self       \ delete previous pens/brushes
                colorref penwidth PS_SOLID Call CreatePen        to hpen
                colorref HS_DIAGCROSS      Call CreateHatchBrush to hbrush
                ;M

;Class

\ : new-hatch-color ( colorref -<name>- ) \ make a hatched color
\                 HatchColorObject                \ define a new object
\                 NewColor: NewObject ;           \ and initialize it


