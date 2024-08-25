\ $Id: dc.fth 1.2 1994/03/22 14:57:01 andrew Exp $

cr .( Loading Device Context and Printing...)
cr .( -- BETA DC.F V2.9G --)

\ dc.f BETA 24/09/2002 arm (minor) changed old Fxxx-FILE functions to use ANS file set 
\ dc.f BETA 08/10/2002 arm Consolidation 


((  Changes and enhancements are noted at the end of the file.  Use Ctrl+End to
  get there instantly.
))

: NewStruct     ( n1 --- hstruct )
        dup>r malloc dup r> erase ;

: DeleteStruct  ( hstruct -- )
        release ;

  8 value CHAR-WIDTH            \ Width of each character in pixels
 14 value CHAR-HEIGHT           \ Height of each character in pixels


:CLASS WinDC   <Super Object

int hdc         \ Handle to the device context
int tabbuf
int tabcnt
int tabwidth
int currentfont

Rectangle FillRect

40 constant deftabs

deftabs cells bytes tabarray

:M DefaultTabs: ( -- )
        deftabs 0
        ?DO     i 1+ tabwidth * char-width *
                tabarray i cells+ !             \ fill default tabs
        LOOP 
        tabarray to tabbuf
        deftabs  to tabcnt
        ;M

:M ClassInit:   ( -- )
        ClassInit: super
        8 to tabwidth
        DefaultTabs: self
        0        to hdc
        ;M

:M SetTabs:     ( a1 n1 -- )    \ a1 is array of cells with offsets
        to tabcnt
        to tabbuf
        ;M

:M SetTabSize:  ( n1 -- )
        to tabwidth
        DefaultTabs: self
        ;M

:M GetTabSize:  ( -- n1 )
        tabwidth
        ;M

:M GetHandle:   ( -- hdc )  hdc  ;M

:M PutHandle:   ( hdc -- )  to hdc  ;M

:M SelectObject:  ( object -- oldobj )
        hDC Call SelectObject
        ;M

:M DeleteObject:  ( object -- )
        Call DeleteObject 3 ?win-error_plus
        ;M

:M GetStockObject: ( id -- object )
        Call GetStockObject
        ;M

:M SelectStockObject:  ( id -- oldobj )
        GetStockObject: self  SelectObject: self
        ;M

:M GetTextMetrics:  ( tm -- )
        rel>abs hDC Call GetTextMetrics ?win-error
        ;M

:M GetTextExtent:  { adr len \ exttemp -- width height }
        2 cells LocalAlloc: exttemp     \ allocate some space
        exttemp 2 cells erase           \ init it to zeros
        exttemp rel>abs
        len
        adr rel>abs
        Win32s?
        IF      hDC Call GetTextExtentPoint   ?win-error
        ELSE    hDC Call GetTextExtentPoint32 ?win-error
        THEN
        exttemp @                       \ the width
        exttemp cell+ @                 \ the height
        ;M

:M SetTextColor:  { color_object -- }
        color_object ?ColorCheck drop
        Color: color_object hdc Call SetTextColor drop
        ;M

:M SetBkColor:  { color_object -- }
        color_object ?ColorCheck drop
        Color: color_object hDC Call SetBkColor   drop
        ;M

:M SetBkMode:   ( mode  -- )
        hDC Call SetBkMode    drop
        ;M

:M SaveDC:      ( -- )  \ Save current DC context and objects including the
                                                        \ current font.
        hDC Call SaveDC drop
        ;M

:M RestoreDC:   ( -- )  \ restore current DC context including font
        -1 hDC Call RestoreDC ?win-error
        ;M

:M SetFont:     ( font_handle -- )
        dup to currentfont
        SelectObject: self drop
        ;M

:M GetFont:     ( -- font_handle )
        currentfont
        ;M

:M TextOut:     ( x y addr len -- )
        swap rel>abs 2swap swap hDC Call TextOut ?win-error
        ;M

:M DrawText:    ( addr len rect format -- )
        swap rel>abs 2swap swap rel>abs hDC Call DrawText drop
        ;M

:M TabbedTextOut: ( x y addr len -- text_dimensions )
        2>r 2>r 0 tabbuf rel>abs tabcnt 2r> 2r>
        swap rel>abs 2swap swap hDC Call TabbedTextOut
        ;M

:M LineColor:   { color_object -- }
        color_object ?ColorCheck drop
        Pen: color_object hDC Call SelectObject drop
        ;M

:M PenColor:    { color_object -- }
        color_object LineColor: self
        ;M

:M BrushColor:  { color_object -- }
        color_object ?ColorCheck drop
        Brush: color_object hDC Call SelectObject drop
        ;M

:M MoveTo:      ( x y -- )
        0 -rot swap
        hDC Call MoveToEx ?win-error
        ;M

:M LineTo:      ( x y -- )
        swap
        hDC Call LineTo ?win-error
        ;M

\ July 29th, 1998 - 9:03 tjz
\ Removed an extra swap after 'rel>abs' in the following two definitions,
\ per a bug reported by Pierre Abbat

:M PolyBezierTo: ( ptr cnt -- )         \ rls - new *** Note: NOT in Win32S!
        swap rel>abs hDC Call PolyBezierTo ?win-error ;M

:M PolyBezier:  ( ptr cnt -- )          \ rls - new *** Note: NOT in Win32S!
        swap rel>abs hDC Call PolyBezier   ?win-error ;M

        \ July 29th, 1998 - 9:03 tjz
        \ Added Polygon: as suggested by Pierre Abbat

:M Polygon:     ( ptr cnt - )           \ tjz - new *** Note: NOT in Win32S!
        swap rel>abs hDC Call Polygon ?win-error ;m

:M PolyDraw:    ( tptr pptr cnt -- )    \ rls - new *** Note: NOT in Win32S!
        rot rel>abs rot rel>abs rot
        hDC Call PolyDraw   ?win-error ;M

:M BeginPath:   ( -- )
        hDC Call BeginPath      ?win-error ;M

:M FillPath:    ( -- )
        hDC Call FillPath       ?win-error ;M

:M StrokePath:  ( -- )                 
        hDC Call StrokePath     ?win-error ;M

:M FillPath:    ( -- )                  \ rls - new  ? Needs Brushes ?
        hDC Call FillPath       ?win-error ;M

:M StrokeAndFillPath:    ( -- )         \ rls - new  ? Needs Brushes ?
        hDC Call StrokeAndFillPath      ?win-error ;M

:M EndPath:     ( -- )                
        hDC Call EndPath       ?win-error ;M

:M SetROP2:     ( mode -- oldmode )
        hDC Call SetROP2
        ;M

:M SetPixel:    { x y color_object -- }
                color_object ?ColorCheck drop
                Color: color_object y x hDC Call SetPixel drop
                ;M

:M GetPixel:    ( x y -- colorref )   \ returns a "COLORREF", not a color object
                swap
                hDC Call GetPixel
                ;M

:M BitBlt:      ( blitmode sourcex,y sourcedc sizex,y destinationx,y -- )
                2>r 2>r >r swap r> 2r> swap 2r> swap
                hdc ( 9 win-parameters ) call BitBlt ?win-error ;M

:M StretchBlt:  ( blitmode srcsizex,y srcx,y srcdc dstsizex,y dstx,y -- )
        2>r                             \ save dstx,y
        2>r                             \ save dstsizex,y
        >r                              \ save srcdc
        2>r                             \ save srcx,y
        swap                            \ swap srcsizex,y
        2r> swap                        \ recover, swap srcx,y
        r>                              \ recover srcdc
        2r> swap                        \ recover, swap dstsizex,y
        2r> swap                        \ recover, swap dstx,y
        hdc ( 11 win-parameters ) call StretchBlt ?win-error ;M

:M FillRect:    { color_object rectangle -- }
        color_object ?ColorCheck drop
        Brush: color_object
        rectangle rel>abs hdc
        ( 3 win-parameters ) Call FillRect ?win-error
        ;M

:M FillArea:    { left top right bottom color_object -- }
        color_object ?ColorCheck drop
        Brush: color_object
        left top right bottom SetRect: FillRect
        FillRect.AddrOf rel>abs hdc
        ( 3 win-parameters ) Call FillRect ?win-error
        ;M

:M Ellipse:     { left top right bottom -- }
        bottom right top left
        hdc Call Ellipse ?win-error ;M

:M Arc:         { left top right bottom x1 y1 x2 y2 -- }
        y2 x2 y1 x1 bottom right top left
        hdc Call Arc ?win-error ;M

:M FillCircle:  { x y radius -- }
        x radius - y radius - x radius + y radius +
        Ellipse: self ;M

:M Circle:      { x y radius -- }
        x radius - y radius - x radius + y radius +
        0 0 0 0 Arc: self ;M

:M CreateCompatibleBitmap: ( width height -- hbitmap )
        swap hdc
        Call CreateCompatibleBitmap ;M

;Class


  0 value #PAGES-UP
644 value SCREEN-WIDTH          \ Width of screen in bits
484 value SCREEN-HEIGHT         \ Height of screen in bits
  0 value PRINTING?             \ are we currently printing?
  6 value PRINTER-LPI           \ line per inch of the printer
 10 value PRINTER-CPI           \ characters per inch of the printer
 62 value PRINTER-ROWS          \ line per page of the printer
 80 value PRINTER-COLS          \ columns on printer page
  0 value PRINTER-#OUT          \ characters output on this line of printer
  0 value PRINTER-#LINE         \ lines output to the printer on this page
  0 value MULTI-PAGE?           \ multiple pages per page flag
  0 value PRINTER?              \ are we outputing to a line printer
 16 value #XINDENT              \ nominal indentation of extended lines - rls

\ rls added constants and values:

0 constant PR_SCALED
1 constant PR_RAW

0         value ?tab
1         value XLineCount
pr_scaled value PRINTER-MODE
4800      value PRINTER-HSIZE
6350      value PRINTER-VSIZE
600       value PRINTER-HRES
600       value PRINTER-VRES
false     value PRINT-EXTENDED-LINES

INTERNAL

: _calc_font_height ( --- points_high )
                83 printer-lpi / ;

EXTERNAL

defer calc_font_height

' _calc_font_height is calc_font_height

FALSE value auto-on?            \ automatic printer initialization, no dialog
FALSE value direct-print?
 TRUE value border?             \ should a border be printed on each page

: XLCnt   ( charcnt -- n )  \ Extended Line Count used for printing - rls - page
        printer-#out +
        dup printer-cols <=
        IF      drop 1
        ELSE    #xindent 1+ - printer-cols #xindent - / 1+
        THEN ;

\ a1 points to a backwards string, a2 points to a forward string.  This routine
\ scans backwards to find a space, returns a forward string from there.

: -BLSCAN1        ( a1 n1 -- a2 n2 )                            \ rls - page
        dup>r bl -scan dup
        IF      r> swap - 1+
        ELSE    drop r> + 1+ 0
        THEN ;

: SplitLine     { a1 n1 \ a2 n2 n3 n4 -- a2 n2 indent a1 n1' }   \ rls - page
        n1 printer-#out + printer-cols >
        IF      printer-cols printer-#out - to n2
                n1 n2 - to n3
                n2 #xindent min to n4
                a1 n2 + 1- n4 -blscan1 dup to n4 n3 +
                bl skip                                 ( a2 n2 )
                #xindent n4 -                           \ indent on next line
                a1 n2 n4 - -trailing
        ELSE    a1 n1 + 0 a1 n1 0
        THEN ;

:Class WinPrinter <SUPER WinDC

int page#
int #pages
int scalex
int scaley
int borderx
int bordery
int midx
int midy
int drawlist
int drawoff
int drawing?
int drawmax
int penwidth
int page-ended?
int first-printed-page
int last-printed-page
int sequential-pages?
12 constant INT-BORDER-D
17 constant EXT-BORDER-D
int int-font                    
MAXSTRING bytes user-message-buf
MAXSTRING bytes user-title-buf
MAXSTRING bytes border-buf

ColorObject PRINTCOLOR
ColorObject PRINTFILLCOLOR

Font hFont
Font vFont
Font tFont
Font lFont

:M SetPrinterFont: ( a1 n1 -- )
        2dup SetFaceName: hFont
        2dup SetFaceName: vFont
        2dup SetFaceName: tFont
        SetFaceName: lFont
        ;M

:M ClassInit:   ( -- )
        ClassInit: super
        0 to #pages
        1 to page#
        0 to drawlist
        0 to drawoff
        0 to drawing?
        0 to int-font
        FALSE to sequential-pages?
        65536 to drawmax
        -1 to penwidth
        user-message-buf off
        user-title-buf off
        border-buf off
                                \ set the default font type for printing
        s" Courier New" SetPrinterFont: self
        FW_NORMAL Weight: tFont
        CLIP_TT_ALWAYS ClipPrecision: tFont
        FIXED_PITCH 0x04 or FF_MODERN or PitchAndFamily: tFont
        FW_NORMAL Weight: lFont
        CLIP_TT_ALWAYS ClipPrecision: lFont
        FIXED_PITCH 0x04 or FF_MODERN or PitchAndFamily: lFont
        FW_NORMAL Weight: hFont
        CLIP_TT_ALWAYS ClipPrecision: hFont
        FIXED_PITCH 0x04 or FF_MODERN or PitchAndFamily: hFont
        FW_NORMAL Weight: vFont
        CLIP_TT_ALWAYS CLIP_LH_ANGLES or ClipPrecision: vFont
        ( orientation in tenth degree increments ) 900 Orientation: vFont
        FIXED_PITCH 0x04 or FF_MODERN or PitchAndFamily: vFont
        ;M

:M UserMessage: ( a1 n1 -- )
        user-message-buf place
        ;M

:M UserTitle:   ( a1 n1 -- )
        user-title-buf place
        ;M

:M SetPage:     ( n1 -- )       \ set the page number of next page printed
        to page#
        ;M

:M SequentialState: ( flag -- )
        to sequential-pages?
        ;M

:M RefLineColor: ( colorref -- )    \ version for the printer, pen width two
        NewColor: PRINTCOLOR
        penwidth PenWidth: PRINTCOLOR
        Pen: PRINTCOLOR hDC Call SelectObject drop
        ;M

:M RefFillArea: { left top right bottom colorref -- }
        colorref NewColor: PRINTFILLCOLOR
                    Brush: PRINTFILLCOLOR
        left top right bottom SetRect: FillRect
        FillRect.AddrOf rel>abs hdc
        ( 3 win-parameters ) Call FillRect ?win-error
        ;M

\ printer resolution function

:M Width:       ( -- horizontal-dots-on-page )
        HORZRES hdc Call GetDeviceCaps
        ;M

:M Height:      ( -- vertical-dots-on-page )
        VERTRES hdc Call GetDeviceCaps
        ;M

:M DPI:         ( -- horizontal-dots-per-inch  vertical-dots-per-inch )
        LOGPIXELSX hdc Call GetDeviceCaps
        LOGPIXELSY hdc Call GetDeviceCaps
        ;M

\ Modes for SetStretchBltMode:
\
\       BLACKONWHITE  COLORONCOLOR  WHITEONBLACK  HALFTONE

:M SetStretchBltMode:  ( mode_value -- )
        hdc Call SetStretchBltMode drop
        ;M

:M Nullify:     ( -- )                  \ mark the printer hdc as not in use
        0 to hdc
        0 to drawlist
        0 to drawoff
        0 to drawing?
        ;M

: set-print-quality
        DPI: self drop
        quality-print dup 0<
        IF 
                CASE    DMRES_HIGH   OF DPI: self drop          ENDOF 
                        DMRES_MEDIUM OF DPI: self drop 2 /      ENDOF 
                        DMRES_LOW    OF DPI: self drop 4 /      ENDOF 
                        DMRES_DRAFT  OF DPI: self drop 8 /      ENDOF 
                                        DPI: self drop swap
                ENDCASE 
\ 09/08/95 10:13 tjz added the ELSE section to deal with the fact that
\ Windows NT 3.51 returns zero (0) for print quality, which was interpreted
\ as a very low resolution printer. Now we limit low resolution printing
\ to 32 DPI or higher. When a resolution lower than that is encountered,
\ then we just use the HIGHEST resolution available.
        ELSE    dup 32 <                \ if really low resolution
                IF      drop            \ then use the highest
                        DPI: self drop  \ resolution available
                THEN 
        THEN    1 max / 1 max to penwidth ;

int font-height
int tall?

: PORTRAIT?     ( -- flag )             \ True if portrait mode
        tall? #pages-up 2 =             \ For 2-up, reverse mode.
        IF      0=      THEN ;

: set-rows-cols   ( -- )                                \ rls - pages
        Height: self   to PRINTER-VSIZE
        DPI: self nip  to PRINTER-VRES
        Width: self    to PRINTER-HSIZE
        DPI: self drop to PRINTER-HRES
        multi-page? 2 =
        IF
                PRINTER-VSIZE printer-cpi PRINTER-VRES
                dup>r char-width / min r> */ to PRINTER-COLS
                PRINTER-HSIZE printer-lpi PRINTER-HRES
                dup>r char-height / min r> */ 1- to PRINTER-ROWS
        ELSE
                PRINTER-VSIZE printer-lpi PRINTER-VRES
                dup>r char-height / min r>
                */ 1- to PRINTER-ROWS                   \ set lines per page
                PRINTER-HSIZE printer-cpi PRINTER-HRES
                dup>r char-width / min r>
                */ to PRINTER-COLS                      \ set columns per line
        THEN ;

: set-print-params ( -- )                       \ rls - many changes
        Height: self Width: self > to tall?     \ tall page flag
        set-rows-cols
        Width:  self int-border-d / to borderx  \ calc scaling
        Height: self int-border-d / to bordery
        Width:  self borderx 2* - dup 2/ to midx  printer-cols / to scalex
        Height: self bordery 2* - dup 2/ to midy  printer-rows 1+ / to scaley
        calc_font_height to font-height

        printer-mode
        CASE    pr_scaled
                OF      screen-width to printer-hsize
                        screen-height to printer-vsize
                ENDOF
                pr_raw
                OF      DPI: self to printer-vres to printer-hres
                        Height: self to printer-hsize
                        Width: self to printer-vsize
                ENDOF
        ENDCASE

        0 to printer-#out
        0 to printer-#line
        set-print-quality ;

:M Open:        ( -- f1 )               \ open the printer for use
        printing? 0=
        IF      auto-on?
                IF      auto-print-init
                ELSE         print-init
                THEN    dup to hdc
        ELSE    hdc
        THEN    dup
        IF      set-print-params
                0 to #pages
        THEN 
        ;M

                                        \ rls February 5th, 2002 - 3:26
:M Open2:       ( bitmapped flags topage -- f1 )    \ open the printer for use
        printing? 0=
        IF      auto-on?
                IF      2drop drop auto-print-init
                ELSE         print-init2
                THEN    dup to hdc
        ELSE    2drop drop hdc
        THEN    dup
        IF      set-print-params
                0 to #pages
        THEN
        ;M

:M AutoOpen:    ( -- f1 )               \ open the printer for use
        printing? 0=
        IF      auto-print-init dup to hdc
        ELSE    hdc
        THEN    dup
        IF      set-print-params
                0 to #pages
        THEN 
        ;M

:M Close:       ( -- )                  \ close the printer
        ;M

:M Landscape:   ( -- )
        TRUE print-orientation dup to hdc
        IF      set-print-params
                0 to #pages
        THEN 
        ;M

:M Portrait:    ( -- )
        FALSE print-orientation dup to hdc
        IF      set-print-params
                0 to #pages
        THEN 
        ;M

:M Start:       ( -- )                  \ start a new page and document
        hdc 0=                  \ if not initialized
        penwidth -1 = or        \ or penwidth hasn't been set
        IF      Open: self      \ -- f1 = true if ready to print
        ELSE    hdc
                set-print-params
        THEN 
        IF      print-start
                0 to printer-#out
                0 to printer-#line
                true to printing?
        THEN 
        ;M

:M End:         ( -- )                  \ end current page and document
        printing?
        IF      end-page
                true to page-ended?
                print-end
                false to printing?
        THEN 
        ;M

:M Setup:       ( window_handle -- )
        print-setup ?dup
        IF      to hdc
                set-rows-cols
        THEN    ;M

:M DrawlistOpen: ( -- )
        drawlist 0=
        IF      drawmax malloc to drawlist
                drawlist 0=
                s" Out of memory error while Printing!" ?ErrorBox
                drawlist drawmax erase          \ prezero buffer
                0 to drawoff
        THEN 
        ;M

:M DrawlistClose: ( -- )
        drawlist
        IF      drawlist release
                    0 to drawlist
                    0 to drawoff
                65536 to drawmax
        THEN 
        ;M

:M PrinterStart: ( -- )
        DrawlistOpen: self
        multi-page? 0=
        IF      0 to drawoff
        THEN 
        true to drawing?
        ;M

:M DrawingOff:  ( -- )
        false to drawing?
        ;M

 1 constant P_LINETO
 2 constant P_MOVETO
 3 constant P_TEXTOT
 4 constant P_TEXTOTR
 5 constant P_TEXTOTL
 6 constant P_LINECOLOR
 7 constant P_FILLAREA
 8 constant P_PAGE
 9 constant P_MPAGE
10 constant P_SETPIXEL
11 constant P_TEXTOTF
12 constant P_BEZIERTO
13 constant P_BEGINPATH
14 constant P_ENDPATH
15 constant P_FILLPATH
16 constant P_STROKEPATH
17 constant P_STROKEANDFILL

int offsety
int c-page

\ Put a border around the screen text

: draw-border
        { \ top left bottom right midwidth midheight foot1 foot2 head1 -- }
        border?                                 \ print a border on page?
        IF                                      \ setup the location pointers
                Width:  self ext-border-d /                   to left
                Height: self ext-border-d /                   to top
                Width:  self ext-border-d / ext-border-d 1- * to right
                Height: self ext-border-d / ext-border-d 1- * to bottom
                Height: self  2 /                             to midheight
                Width:  self  2 /                             to midwidth
                bottom top 20 / 2* +                         to foot1
                bottom top 20 / 7 * +                         to foot2
                top 5 / 3 *                                   to head1

                left  top    MoveTo: self       \ top left corner
                right top    LineTo: self       \ top line
                right bottom LineTo: self       \ right side line
                left  bottom LineTo: self       \ bottom line
                left  top    LineTo: self       \ left line

                multi-page? ?dup                \ split the pages per page
                IF  2 =
                    IF   Tall?
                         IF   left     midheight MoveTo: self   \ left middle
                              right    midheight LineTo: self   \ right middle
                         ELSE midwidth top       MoveTo: self   \ top middle
                              midwidth bottom    LineTo: self   \ bottom middle
                         THEN 
                    ELSE left     midheight MoveTo: self        \ left middle
                         right    midheight LineTo: self        \ right middle
                         midwidth top       MoveTo: self        \ top middle
                         midwidth bottom    LineTo: self        \ bottom middle
                    THEN 
                THEN 

                Handle: tFont SetFont: self             \ select the tiny font

                s" Date: "                                border-buf  place
                get-local-time time-buf >month,day,year"  border-buf +place
                s"   "                                    border-buf +place
                get-local-time time-buf >am/pm"           border-buf +place
                s"    "                                   border-buf +place

                left foot1
                border-buf count TextOut: self          \ display the text
                left foot2
                user-message-buf count TextOut: self    \ display user message

                #pages
                IF      multi-page?
                        IF      s" Pages: "               border-buf  place
                                page# 1- multi-page? * 1+
                                0 <# #s #>                border-buf +place
                                s"  to "                  border-buf +place
                                page# 1- multi-page? * 1+ multi-page? 1- +
                        ELSE    s" Page: "                border-buf  place
                                page#
                        THEN    0 <# #s #>                border-buf +place
                        s"  of "                          border-buf +place
                        #pages 1+ 0 <# #s #>              border-buf +place
                        right                           \ from the right edge
                                                        \ back off by text width 
                        border-buf count GetTextExtent: self drop -
                        foot1
                        border-buf count TextOut: self  \ display page of pages
                THEN 
                user-title-buf c@                    \ if header text?
                IF      Handle: lFont SetFont: self  \ select line printer font
                        Width: self 2/               \ middle of page
                                                     \ center the title
                        user-title-buf count GetTextExtent: self
                        drop 2/ - 0max head1
                        user-title-buf count TextOut: self \ display the header
                THEN 
                Handle: hFont SetFont: self             \ select horiz font
        THEN    ;

: ?page-started ( -- )
        page-ended?
        IF      start-page
                false to page-ended?
        THEN    ;

: print-page?   ( -- f1 )
        page# first-printed-page >= ;

: p-page        ( a1 n1 -- a1 n1 )
        page-ended? 0=          \ don't do more than one in sequence
        IF      print-page?
                IF      draw-border
                        end-page
                        true to page-ended?
                THEN
        THEN    1 +to page# ;

: p-mpage       ( -- )
        c-page multi-page? 1- =
        IF      p-page
                0 to c-page
        ELSE    1 +to c-page
        THEN    ;

: x-position    ( logicalx -- physicalx )
        PRINTER-MODE pr_raw = ?EXIT
        multi-page? ?dup
        IF      2 =
                IF      tall? 0=
                        IF
                                c-page 1 and
                                IF      1+ scalex * 2/ midx +
                                ELSE
                                        1- scalex * 2/
                                THEN
                        ELSE    scalex *
                        THEN
                ELSE    c-page 1 and
                        IF      1+ scalex * 2/ midx +
                        ELSE    1- scalex * 2/
                        THEN
                THEN 
        ELSE    scalex *
        THEN
        borderx + ;

: y-position    ( logicaly -- physicaly )
        PRINTER-MODE pr_raw = ?EXIT
        multi-page? ?dup
        IF      2 =
                IF      tall?
                        IF      c-page 1 and
                                IF      1+ scaley * 2/ midy +
                                ELSE    1- scaley * 2/
                                THEN
                        ELSE    scaley *
                        THEN 
                ELSE    c-page 2 and
                        IF      1+ scaley * 2/ midy +
                        ELSE    1- scaley * 2/
                        THEN
                THEN
        ELSE    scaley *
        THEN
        bordery + ;

: p-lineto      ( a1 n1 -- a1 n1 )
        print-page? 0= ?exit
        ?page-started
        over 1+ dup sw@ x-position
        swap    2 + sw@ y-position LineTo: self ;

: p-moveto      ( a1 n1 -- a1 n1 )
        print-page? 0= ?exit
        ?page-started
        over 1+ dup sw@ x-position
        swap    2 + sw@ y-position MoveTo: self ;

: p-textot      ( a1 n1 -- a1 n1 )      \ horizontal text (normal)
        print-page? 0= ?exit
        ?page-started
        over 1+  dup sw@           x-position
        swap 2 + dup sw@ offsety + y-position
        swap 2 +     count        TextOut: self ;

: p-textotr     ( a1 n1 -- a1 n1 )      \ vertical text
        print-page? 0= ?exit
        ?page-started
        over 1+  dup sw@           x-position
        swap 2 + dup sw@ offsety + y-position
        swap 2 +     count Handle: vFont SetFont: self
        TextOut: self
        Handle: hFont SetFont: self ;

: p-textotl     ( a1 n1 -- a1 n1 )      \ line printer text
        print-page? 0= ?exit
        ?page-started
        over 1+  dup sw@ x-position
        swap 2 + dup sw@ y-position
        swap 2 + count Handle: lFont SetFont: self
        TextOut: self
        Handle: hFont SetFont: self ;

: p-textotf     ( a1 n1 -- a1 n1 )      \ font-specifed text out
        print-page? 0= ?exit
        ?page-started
        over 1+ @ to int-font   \ get font object
        over 5 + dup sw@
        swap 2 + dup sw@
        swap 2 + count
        Handle: int-font SetFont: self
        TextOut: self
        Handle: hfont SetFont: self ;

: p-linecolor   ( a1 n1 -- a1 n1 )
        print-page? 0= ?exit
        ?page-started
        over 1+ @ ( colorref ) RefLineColor: self ;

: p-fillarea    ( a1 n1 -- a1 n1 )
        print-page? 0= ?exit
        ?page-started
        over 1+  >r r@ sw@ x-position                   
        r> 2 +   >r r@ sw@ y-position                   
        r> 2 +   >r r@ sw@ x-position                   
        r> 2 +   >r r@ sw@ y-position                   
        r> 2 +       @ ( colorref ) RefFillArea: self ;

: p-setpixel    ( a1 n1 -- a1 n1 )
        print-page? 0= ?exit
        ?page-started
        over 1+ >r
        r@     sw@ 1- x-position                        
        r@ 2 + sw@ 1- y-position                        
        r@     sw@ 1+ x-position                        
        r@ 2 + sw@ 1+ y-position                        
        r> 4 +   @ ( colorref ) RefFillArea: self ;

: p-bezierto    ( a1 n1 -- a1 n1 )                      \ only works RAW
        over 1+ count PolyBezierTo: self ;

: p-beginpath   ( a1 n1 -- a1 n1 )
        BeginPath: self ;

: p-endpath     ( a1 n1 -- a1 n1 )
        EndPath: self ;

: p-fillpath    ( a1 n1 -- a1 n1 )
        FillPath: self ;

: p-strokepath  ( a1 n1 -- a1 n1 )
        StrokePath: self ;

: p-strokeandfill ( a1 n1 -- a1 n1 )
        StrokeAndFillPath: self ;

:M SetPageLimits:   ( -- )
        get-frompage to first-printed-page
        get-topage   to  last-printed-page
        multi-page?
        IF      first-printed-page 1 umax
                1- multi-page? / 1+ to first-printed-page
                last-printed-page 1 umax
                1- multi-page? / 1+ to last-printed-page
        THEN ;M

:M PrinterEnd:   ( -- )
        drawlist
        IF      SetPageLimits: self
                0 drawlist drawoff + !                  \ NULL terminate list
                0 to c-page
                0 to page-ended?
                sequential-pages? 0=
                IF      1 to page#
                THEN 
                set-print-quality                       \ init penwidth

\ tiny font for page labeling
                0 to offsety
                scalex 100 115 */ Width: tFont
                scaley 100 130 */ Height: tFont
                Create: tFont

\ line printer font
                scalex multi-page? ?dup
                IF      2 = tall? and 0=
                        IF      2/      THEN
                THEN
                Width: lFont                            \ set font width
                scaley multi-page? ?dup
                IF      2 = tall? 0= and 0=
                        IF      2/      THEN
                THEN
                Height: lFont
                Create: lFont

\ horizontal font (normal)
                scalex multi-page? ?dup
                IF      2 = tall? and 0=
                        IF      2/      THEN
                THEN
                Width: hFont
                scaley multi-page? ?dup
                IF      2 = tall? 0= and 0=
                        IF      2/      THEN
                THEN
                Height: hFont
                Create: hFont

\ vertical font
                scalex multi-page? ?dup
                IF      2 = tall? and 0=
                        IF      2/      THEN
                THEN
                Width: vFont
                scaley multi-page? ?dup
                IF      2 = tall? 0= and 0=
                        IF      2/      THEN
                THEN
                Height: vFont Create: vFont
                
                Start:  self
                drawlist            \ starting at the top of the list
                BEGIN count dup     \ proceed through each record
                                    \ stop if not within desired page range
                    page# last-printed-page <= and
                WHILE over c@
                    CASE   P_LINETO OF p-lineto        ENDOF
                           P_MOVETO OF p-moveto        ENDOF
                           P_TEXTOT OF p-textot        ENDOF   \ scaled
                          P_TEXTOTR OF p-textotr       ENDOF   \ rotated
                          P_TEXTOTL OF p-textotl       ENDOF   \ LPT
                          P_TEXTOTF OF p-textotf       ENDOF   \ specific font
                        P_LINECOLOR OF p-linecolor     ENDOF 
                         P_FILLAREA OF p-fillarea      ENDOF 
                             P_PAGE OF p-page          ENDOF 
                            P_MPAGE OF p-mpage         ENDOF 
                         P_SETPIXEL OF p-setpixel      ENDOF 
                         P_BEZIERTO OF p-bezierto      ENDOF 
                        P_BEGINPATH OF p-beginpath     ENDOF 
                          P_ENDPATH OF p-endpath       ENDOF 
                         P_FILLPATH OF p-fillpath      ENDOF 
                       P_STROKEPATH OF p-strokepath    ENDOF 
                    P_STROKEANDFILL OF p-strokeandfill ENDOF
                    ENDCASE 
                    +
                REPEAT  2drop

                page-ended? 0=
                IF      p-page
                        sequential-pages? 0=
                        IF      first-printed-page to page#
                        THEN 
                THEN 
                print-end
                false to printing?
                Delete: hFont
                Delete: vFont
                Delete: tFont
                Delete: lFont
        THEN
        false to drawing?
        hdc
        IF      print-close
                0 to hdc
        THEN 
        ;M

: drawlist_overflow? ( -- )
        drawoff drawmax MAXSTRING - u>
        IF      drawmax 65536 + dup drawlist realloc
                s" Failed to Expand the DRAWLIST!" ?ErrorBox
                to drawlist                     \ set new buffer addr
                drawlist drawmax + 65536 erase  \ clear extra buffer
                to drawmax                      \ set new buffer len
        THEN    ;

: d-c,          ( c1 -- )               \ compile a BYTE
        drawlist drawoff + c!
        1 +to drawoff drawlist_overflow? ;

: d-w,          ( w1 -- )               \ compile a WORD
        drawlist drawoff + w!
        2 +to drawoff drawlist_overflow? ;

: d-,           ( c1 -- )               \ compile a CELL
        drawlist drawoff + !
        cell +to drawoff drawlist_overflow? ;

MAXCOUNTED 10 - CONSTANT string-max     \ later 6 is added to this, and the sum
                                        \ must be less than MAXCOUNTED

: d-",          ( a1 n1 -- )            \ compile a string
        string-max min                  \ clip to max allowed string
        dup>r
        drawlist  drawoff + place
        r> 1+ +to drawoff drawlist_overflow? ;

int lastcall

:M PrinterLineto: ( x y -- )
        drawing?                        \ we are drawing
        IF      lastcall P_MOVETO =
                IF      drawlist drawoff + >r
                        2dup r@ 2 - sw@ =
                        swap r> 4 - sw@ = and 0=
                ELSE    true
                THEN 
                IF
                        5 d-c,                  \ record data length is 5 bytes
                        P_LINETO to lastcall
                        P_LINETO  d-c,          \ opcode
                        swap d-w, d-w,          \ x and y
                ELSE    2drop
                        -6 +to drawoff
                        0 drawlist drawoff + !  \ null terminate list
                THEN
        ELSE    2drop
        THEN    ;M

:M PrinterMoveto: ( x y -- )
        drawing?                        \ we are drawing
        IF      5 d-c,                  \ record data length is 5 bytes
                P_MOVETO to lastcall
                P_MOVETO  d-c,          \ opcode
                swap d-w, d-w,          \ x and y
        ELSE    2drop
        THEN    ;M

:M PrinterBezierTo: ( addr cnt -- )     \ cnt is number of points
                                        \ in the array at addr
        drawing?
        IF      dup 3 mod
                IF      2drop           \ error if not multiple of 3
                ELSE    P_BEZIERTO to lastcall
                        BEGIN   dup 30 >        \ we can't exceed 253/(8*cnt)
                        WHILE   30 -            \ adjust cnt
                                242 d-c,
                                P_BEZIERTO d-c,
                                30 d-c,         \ count of "points"
                                over 240 d-",
                                30 +
                        REPEAT
                        dup
                        IF      dup 8 * 2 + c,
                                P_BEZIERTO d-c,
                                dup d-c,
                                8 * d-",
                        ELSE    2drop
                        THEN
                THEN
        ELSE    2drop
        THEN    ;M

:M PrinterBeginPath: ( -- )
        drawing?
        IF      1 d-c,                  \ record length is 1 byte + count
                P_BEGINPATH to lastcall
                P_BEGINPATH d-c,        \ opcode
\ April 20th, 1998 - 16:34 tjz removed
\       ELSE    drop
        THEN    ;M

:M PrinterEndPath: ( -- )
        drawing?
        IF      1 d-c,                  \ record length is 1 byte + count
                P_ENDPATH to lastcall
                P_ENDPATH d-c,          \ opcode
\ April 20th, 1998 - 16:34 tjz removed
\       ELSE    drop
        THEN    ;M

:M PrinterStrokePath: ( -- )
        drawing?
        IF      1 d-c,                  \ record length is 1 byte + count
                P_STROKEPATH to lastcall
                P_STROKEPATH d-c,        \ opcode
\ April 20th, 1998 - 16:34 tjz removed
\       ELSE    drop
        THEN    ;M

:M PrinterFillPath: ( -- )
        drawing?
        IF      1 d-c,                  \ record length is 1 byte + count
                P_FILLPATH to lastcall
                P_FILLPATH d-c,         \ opcode
        THEN    ;M

:M PrinterStrokeAndFill: ( -- )
        drawing?
        IF      1 d-c,                      \ record length is 1 byte + count
                P_STROKEANDFILL to lastcall
                P_STROKEANDFILL d-c,        \ opcode
        THEN    ;M

:M PrinterTextOut: ( x y addr len -- )
        drawing?                        \ we are drawing
        IF      string-max min          \ clip to max allowed string
                dup 6 +   d-c,    \ record length 2 words + opcode + string cnt
                P_TEXTOT  to lastcall
                P_TEXTOT  d-c,          \ opcode
                2swap
                swap d-w, d-w,          \ x and y
                d-",                    \ text string
        ELSE    4drop
        THEN    ;M

:M PrinterTextOutFont: ( x y addr len font_object -- )
        drawing?
        IF      >r string-max min
                dup 10 + d-c,      \ str_len+draw_len opcode font4 x2 y2 str len
                P_TEXTOTF to lastcall
                P_TEXTOTF d-c,                  \ opcode
                r> d-,                          \ font object
                2swap swap d-w,                 \ x-position (raw)
                d-w,                            \ y-position (raw)
                d-",                            \ string
        ELSE    4drop drop
        THEN    ;M

:M PrinterRotatedTextOut: ( x y addr len -- )
        drawing?                                \ we are drawing
        IF      string-max min                  \ clip to max allowed string
                dup 6 +   d-c,          \ record length 2 words + opcode + count
                P_TEXTOTR to lastcall
                P_TEXTOTR d-c,                  \ opcode
                2swap
                swap d-w, d-w,                  \ x and y
                d-",                            \ text string
        ELSE    4drop
        THEN    ;M

:M LPTPrinterTextOut: ( addr len -- )
        drawing?                                \ we are drawing
        IF      string-max min                  \ clip to max allowed string
                dup 6 +   d-c,  \ record length 2 words + opcode + string cnt
                P_TEXTOTL to lastcall
                P_TEXTOTL d-c,                  \ opcode
                printer-#out  d-w,              \ col
                printer-#line d-w,              \ row
                d-",                            \ text string
        ELSE    2drop                  
        THEN ;M

\ January 20th, 1997 tjz
\ changed this definition to save the color reference, rather than saving the
\ color object, since some applications, particularly WINDEMO.F, use the same
\ color object over and over again, just changing the color in the object.
\ We now save the color reference, so we can correctly regenerate the needed
\ color when printing occurs.

:M PrinterLineColor: { color_object -- }
        color_object ?ColorCheck drop
        drawing?
        IF      5 d-c,                  \ record length is 5 bytes + count
                P_LINECOLOR to lastcall
                P_LINECOLOR d-c,        \ opcode
                Color: color_object d-, \ compile the colorref of cur color obj
        THEN    ;M

:M PrinterFillArea: { left top right bottom color_object -- }
        color_object ?ColorCheck drop
        drawing?
        IF      13 d-c,                 \ record is 13 bytes + count
                P_FILLAREA to lastcall
                P_FILLAREA d-c,         \ opcode
                left  d-w, top    d-w,
                right d-w, bottom d-w,
                Color: color_object d-, \ compile the colorref of cur color obj
        THEN    ;M

:M PrinterPage:  ( -- )
        drawing?
        IF      1 d-c,                  \ record length is 1 byte + count
                P_PAGE to lastcall
                P_PAGE d-c,             \ opcode
                1 +to #pages
        THEN    ;M

:M PrinterMultiPage: ( -- )
        drawing?
        IF      1 d-c,                  \ record length is 1 byte + count
                P_MPAGE to lastcall
                P_MPAGE d-c,            \ opcode
                1 +to #pages
        THEN    ;M

:M PrinterSetPixel: { xpos ypos color_object -- }
        color_object ?ColorCheck drop
        drawing?
        IF      9 d-c,                  \ record is 9 bytes + count
                P_SETPIXEL to lastcall
                P_SETPIXEL d-c,         \ opcode
                xpos d-w,
                ypos d-w,
                Color: color_object d-, \ compile the colorref of cur color obj
        THEN    ;M

1 bytes emit_buffer

:M Page:        ( -- )                  \ start a new page
        multi-page?
        IF      PrinterMultiPage: self
        ELSE         PrinterPage: self
        THEN 
        0 to printer-#line
        0 to printer-#out
        ;M

:M Cr:          ( -- )
        1 +to printer-#line
        0  to printer-#out
        printer-#line printer-rows >=
        IF      Page: self
        THEN 
        ;M

: >tab          ( -- )
        \ TAB-SIZE is not right, but ok for now
        printer-#out tabwidth / 1+ tabwidth *
        printer-cols 1- min printer-#out -
        BEGIN   dup 0>
        WHILE   dup spcs-max min
                spcs over dup>r LPTPrinterTextOut: self
                r> +to printer-#out -
        REPEAT drop ;

:M ?Cr:         ( n1 -- )
        printer-#out + printer-cols >
        IF      Cr: self
        THEN 
        ;M

:M Emit:        ( c1 -- )
        emit_buffer c!
        emit_buffer 1 LPTPrinterTextOut: self
        1 +to printer-#out
        ;M

: Type1Line   { a1 n1 \ a2 n2 n3 -- a2 n2 n3 }          \ rls - page
        BEGIN
                a1 n1 k_tab scan to n2  to a2           \ try to print upto tab
                n1 n2 - to n3
                n1 n2 - printer-#out + printer-cols >   \ is line too long?
                IF
                        a1 n1 SplitLine dup to n1      \ Print to max - margin
                        LPTPrinterTextOut: self
                        n1 +to printer-#out             \ update current pos.
                        to n2 to n1 to a1 0
                ELSE
                        a1 n3 LPTPrinterTextOut: self   \ print to possible tab
                        n3 +to printer-#out
                        >tab a2 n2 1 /string            \ then do a tab, if any
                        to n1  to a1                    \ and
                        0 to n2
                        n1
                THEN
                0=
        UNTIL
        a1 n1 n2 ;

:M Type:  { a1 n1 \ n2 -- }                             \ rls - page
        PRINT-EXTENDED-LINES
        IF
                n1 xlcnt to XLineCount
                a1 n1 k_tab scan to n2 drop
                n2 0<> to ?tab
                a1 n1 Type1Line to n2  to n1  to a1
                XLineCount 1
                ?DO
                        Cr: self  -2 to printer-#out
                        ascii + Emit: self  ?tab
                        IF      0
                        ELSE    n2
                        THEN
                        to printer-#out
                        a1 n1 Type1Line to n2  to n1  to a1
                LOOP
        ELSE    a1 n1
                BEGIN   2dup k_tab scan 2dup 2>r nip -
                        dup>r
                        LPTPrinterTextOut: self
                        r> +to printer-#out
                        2r> dup
                WHILE   >tab
                        1 /string
                REPEAT  2drop
        THEN
        ;M

;CLASS

WinPrinter ThePrinter

INTERNAL

: start-printer  ( -- f1 )
        direct-print?
        IF true to auto-on?
        THEN 
        Open: ThePrinter dup
        IF      PrinterStart: ThePrinter
        THEN    ;

EXTERNAL

                                \ rls February 5th, 2002 - 3:36
: start-printer2  ( bitmapped flags topage -- f1 )
        direct-print?
        IF true to auto-on?
        THEN 
        Open2: ThePrinter dup
        IF      PrinterStart: ThePrinter
        THEN    ;

: page-setup    ( -- )
        conhndl Setup: ThePrinter  ;

: start-scaled  ( -- f1 )
        pr_scaled to printer-mode
        start-printer ;

                        \ rls February 5th, 2002 - 8:48
: start-scaled2  ( bitmapped flags topage -- f1 )
        pr_scaled to printer-mode
        start-printer2 ;

: start-raw  ( -- f1 )
        pr_raw to printer-mode
        start-printer ;

: start-raw2  ( bitmapped flags topage -- f1 )  \ rls February 5th, 2002 - 8:48
        pr_raw to printer-mode
        start-printer2 ;

: print-multi-page ( -- )
        multi-page?
        IF         PrinterEnd: ThePrinter
                DrawlistClose: ThePrinter
                false to multi-page?
                SetPageLimits: ThePrinter
        THEN    ;

: page-scaled   ( -- )
        Page: ThePrinter ;

: print-scaled  ( -- )
        multi-page? 0=
        IF             PrinterEnd: ThePrinter
                    DrawlistClose: ThePrinter
        ELSE     PrinterMultiPage: ThePrinter
                       DrawingOff: ThePrinter
                false to printing?
        THEN    ;

: single-page   ( -- )
        multi-page?
        IF      s" Spooling Printing to Windows" "message
                print-multi-page
                300 ms message-off
        THEN    ;

\ synonym 1page    single-page
\ synonym one-page single-page

: two-page      ( -- )
        2 to multi-page?
        SetPageLimits: ThePrinter ;

\ synonym 2page two-page

: four-page     ( -- )
        4 to multi-page?
        SetPageLimits: ThePrinter ;

\ synonym 4page four-page

INTERNAL

: _pemit        ( c1 -- )
        Emit: ThePrinter ;

: _ptype        ( a1 n1 -- )
        0max bounds
        ?DO     i c@ Emit: ThePrinter
        LOOP ;

: _pgetcolrow   ( -- cols rows )
        printer-cols printer-rows ;

: _pgetxy       ( -- x y )
        printer-#out printer-#line ;

: _pgotoxy      ( x y -- )
        to printer-#line to printer-#out ;

: _pcr          ( -- )
        Cr: ThePrinter ;

: _p?cr         ( n1 -- )
        ?Cr: ThePrinter ;

: _ppage        ( -- )
        Page: ThePrinter ;

: _pcol         ( n1 -- )
        printer-cols 1- min printer-#out - spaces ;

EXTERNAL

: PRINTER       ( -- )
        true to printer?
        start-scaled
        IF      ['] _pemit      is emit
                ['] _ptype      is type
                ['] _pgetcolrow is getcolrow
                ['] _pgetxy     is getxy
                ['] _pgotoxy    is gotoxy
                ['] _pcr        is cr
                ['] _p?cr       is ?cr
                ['] _ppage      is page
                ['] _pcol       is col
        ELSE    false to printer?
        THEN    ;

INTERNAL

: console-forth-io ( -- )
        print-scaled
        direct-print?
        IF      false to auto-on?
        THEN 
        false to direct-print?
        false to printer? ;

' FORTH-IO is console

FORTH-IO-CHAIN CHAIN-ADD console-forth-io

EXTERNAL

: PRINT         ( -- )
        printer
        printer?
        IF      source UserTitle: ThePrinter
                interpret
        THEN    console ;

\ type filename a1 to the printer
: $fprint       { the-name \ message$ fpr$ locHdl -- }
        MAXSTRING LocalAlloc: fpr$
        MAXSTRING localAlloc: message$
        the-name $open abort" Couldn't open file!" to locHdl
        cur-line off
        printer
        printer?
        IF      s" Printing file: " message$  place
                open-path$ count    message$ +place
                                    message$ count "message
                s" File: "          message$  place
                open-path$ count    message$ +place
                                    message$ count UserMessage: ThePrinter
                cr
                BEGIN   fpr$ dup MAXCOUNTED locHdl read-line
                        abort" Read Error"
                WHILE   Type: ThePrinter cr
                REPEAT  2drop
        THEN
        console
        locHdl close-file drop
        message-off ;

: FPRINT        ( -<name>- )
        /parse-word $fprint ;

: 2print        ( -- )
        two-page fprint single-page ;

: 4print        ( -- )
        four-page fprint single-page ;

80 value MAXCONCOLS
80 value conscols

: #print-screen ( start_line lines -- ) \ print a range of lines from saved
                                        \ Forth screen buffer
        cols to conscols
        cursor-off
        #pages-up ?dup
        IF      2 =
                IF      two-page
                ELSE    four-page
                THEN 
        THEN 
        printer
        printer?
        IF      ( -- start lines )
                &the-screen -rot
                ( -- start lines ) bounds
                DO
                        dup getmaxcolrow drop i * +       \ line starting addr
                        conscols MAXCONCOLS max getmaxcolrow drop min
                        -trailing                         \ addr len
                        Type: ThePrinter
                        Cr: ThePrinter                    \ next line
                LOOP
                drop
        ELSE    2drop
        THEN    console single-page cursor-on ;

: print-screen  ( -- )          \ print the physical screen
        getrowoff rows
        #print-screen ;

: print-console ( -- )       \ print all lines used in screen save buffer
        0 getrowoff rows +
        #print-screen ;

INTERNAL

: _printer-release ( -- )               \ release the printer DC if allocated
        DrawlistClose: ThePrinter
        Close: ThePrinter ;

unload-chain chain-add-before _printer-release    \ add to termination chain

: _nullify-printer ( -- )               \ mark printer as not used yet
        Nullify: thePrinter ;

initialization-chain chain-add _nullify-printer

EXTERNAL

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ File dialog Class
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

:Class FileDialogs      <Super  Object

max-handle bytes szFile
max-handle bytes szDir
int szFilter
int szTitle

:M ClassInit:   ( -- )
        ClassInit: super
        0 szDir !
        s" UNTITLED" szFile place
                     szFile +NULL
        here 1+ to szTitle  ,"text"
        max-handle here szTitle - - allot       \ extend to max string
        here 1+ to szFilter ,"text"             \ lay in filter, then
        max-handle here szFilter - - allot      \ extend to max string
        ;M

:M SetDir:      ( a1 n1 -- )            \ set the dialog directory string
        max-handle 2 - min szDir place          \ lay in the directory
        szDir +NULL                             \ null terminate
        szDir count upper                       \ make path uppercase
        szDir ?-\                               \ remove trailing \
        ;M

:M GetDir:      ( -- a1 n1 )    \ get the current dialog directory string
        szDir count
        ;M

:M SetTitle:    ( a1 n1 -- )
        szTitle 1- place        \ lay in new string
        szTitle 1- +NULL        \ null terminate it
        ;M

\ a new file filter string would be in the following format, with vertical
\ bars separating filter name from filter spec, and between filter spec
\ and succeeding filter names.
\ A maximum of 255 characters is allowed for the total filter specs string
\ s" Forth Files (*.f)|*.f|Text Files (*.txt)|*.txt|All Files (*.*)|*.*|"

:M SetFilter:   ( a1 n1 -- )            \ set new file filter spec
        szFilter 1- place       \ lay in new string
        szFilter 1- +NULL       \ null terminate it
        ;M

:M GetFilter:   ( -- a1 n1 )            \ return current file filter string
        szFilter 1- count
        ;M

: run-dialog    ( owner_handle dialog-func-cfa -- a1 )
        2>r
        szFile count "to-pathend" szFile place
        szFile +NULL
        szFile      rel>abs     \ takes a counted string for filename
        szDir    1+ rel>abs
        szTitle     rel>abs
        szFilter    rel>abs
        2r> execute abs>rel
        dup count 2dup upper "path-only" szDir place ;

;Class

:Class FileNewDialog    <Super  FileDialogs

:M Start:       ( owner_handle -- a1 )
        ['] new-dialog run-dialog
        ;M

:M Start2:      ( filterindex owner_handle -- a1 )
        ['] new-dialog2 run-dialog
        ;M

;Class

:Class FileOpenDialog   <Super  FileDialogs

:M Start:       ( owner_handle -- a1 )
        ['] open-dialog run-dialog
        ;M

;Class


\ Dialog boxes

FileOpenDialog EditForth "Edit Forth File" "Forth Files (*.f)|*.f|Text Files (*.txt)|*.txt|All Files (*.*)|*.*|"

: edit-forth    ( -- )
        conhndl start: EditForth dup c@
        IF      count pocket place
                0 pocket $edit
        ELSE    drop
        THEN    ;

FileOpenDialog LoadForth "Load Forth File" "Forth Files (*.f)|*.f|All Files (*.*)|*.*|"

: load-forth    ( -- )
        conhndl start: LoadForth dup c@
        IF      count pocket place
                s" FLOAD "   "pushkeys
                pocket count "pushkeys
                0x0D pushkey
        ELSE    drop
        THEN    ;

FileOpenDialog PrintForth "Print Forth File" "Forth Files (*.f)|*.f|Text Files (*.txt)|*.txt|All Files (*.*)|*.*|"

: print-forth    ( -- )
        conhndl start: PrintForth dup c@
        IF      count pocket place
                #pages-up ?dup
                IF      2 =
                        IF      two-page
                        ELSE    four-page
                        THEN    pocket $fprint
                        single-page
                ELSE    pocket $fprint
                THEN 
        ELSE    drop
        THEN    ;

FileOpenDialog EditLog "Edit Key Log File" "Log Files (*.LOG)|*.LOG|Text Files (*.txt)|*.txt|All Files (*.*)|*.*|"

: edit-log      ( -- )
        conhndl start: EditLog dup c@
        IF      count pocket place
                0 pocket $edit
        ELSE    drop
        THEN    ;

:Class FileSaveDialog   <Super  FileDialogs

:M Start:       ( owner_handle -- a1 )
        ['] save-dialog run-dialog
        ;M

;Class

FileSaveDialog SaveForth "Save Forth Image" "Image Files (*.exe)|*.exe|All Files (*.*)|*.*|"

MODULE

\S

This file has had extensive changes by rls (Robert L. Smith, nickname: Bob).
The most important addition is the ability to print excessively long source
lines.  A new value named PRINT-EXTENDED-LINES has been added, and the Type:
function has been changed.  A number of other changes have been made to simplify
some of the complicated code in here.

2-up printing has an improved format.


