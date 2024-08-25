\ Graph windows:

\ Top text bar: YMAX    PITCH   ADDRESS
\ Bottom text bar: YMIN \ AMIN .. AMAX

:object   scopehelp <super helpmsg
:M WindowTitle: ( -- Zstring )
                Z" Graphic Memory Display: Key Usage" ;M
;object

create    scopehelptext
         z," F2  = Toggle auto/manual reload\n"
        +z," F5  = Reload\n"
        +z," '0' = Show parameters for chan 0\n"
        +z," '1' = Show parameters for chan 1\n"
        +z," '2' = Show parameters for chan 2\n"
        +z," '3' = Show parameters for chan 3\n"
        +z," ESC = done\n"
        0 c,


:Object Graf <super Window

1030 cells pointer channel0     \ channels start with 16 bytes of configuration:
1030 cells pointer channel1     \ start address.32
1030 cells pointer channel2     \ length.16
1030 cells pointer channel3     \ centerline.16
                                \ gain.16 = 0..1  0x0000=autoscale
                                \ pitch.8 : pixels per sample
                                \ type.8 : 0=byte,1=word,2=long, +4=alternating
                                \          +8=little-endian,+16=unsigned,
                                \          +32=autoscale
                                \ color.8
                                \ title.64 = 8-byte text label
int chanptr                     \ current channel selection
int auto                        \ T if automatic redraw

create linecolors
                black ,    red ,       green ,    yellow ,
                blue ,     magenta ,   cyan ,     gray ,
                dkgray ,   ltred ,     ltgreen ,  ltyellow ,
                ltblue ,   ltmagenta , ltcyan ,   white ,

: >addr         ( chan -- addr )
                chanptr 3 and exec: channel0 channel1 channel2 channel3 ;

: 'addr         ( -- a )        >addr ;         \ L
: 'len          ( -- a )        >addr  4 + ;    \ W
: 'center       ( -- a )        >addr  6 + ;    \ W
: 'gain         ( -- a )        >addr  8 + ;    \ L
: 'pitch        ( -- a )        >addr 12 + ;    \ B
: 'type         ( -- a )        >addr 13 + ;    \ B
: 'color        ( -- a )        >addr 14 + ;    \ B
: 'label        ( -- a )        >addr 15 + ;    \ string[8]
: 'data         ( -- a )        >addr 24 + ;
: >color        ( c -- color )  15 and cells linecolors + @ ;
: center        ( -- pos )      'center w@ ;
: maxX          ( -- pos )      'pitch c@ 'len w@ * ;

:M Channel:     ( chan# -- )    to chanptr ;M
:M Addr:        ( addr -- )     'addr   ! ;M
:M Data:        ( -- addr )     'data ;M
:M Length:      ( length -- )   'len   w! ;M
:M Center:      ( pels -- )    'center w! ;M
:M Gain:        ( scale -- )    'gain  ! ;M
:M Pitch:       ( pitch -- )    'pitch c! ;M
:M Color:       ( color -- )    'color c! ;M
:M Type:        ( color -- )    'type c! ;M
:M Clr:         ( -- )
                'addr 1030 cells erase
                1 'pitch c!             \ default pitch = 1
                100 'center w!          \ dafault centerline = 100
                10 'color c!            \ default color = light green
                0 'gain !               \ gain = 0
                32 'type c!             \ unsigned byte, autoscale
                'label 8 blank ;M

:M Label:       ( string -- )
                bl word count 8 min 'label swap move ;M

:M Retitle:     ( -- )
\ repaint the title bar
                s" Graph ["    temp$  place
                s" ] "                      temp$ +place
                temp$ count SetTitle: self
                ;M

: pointlength   ( -- #bytes )  1 'type c@ 3 and lshift ;

: nextpoint     ( addr -- addr' Ydata )
                'type c@ 3 and
                case    0 of  count endof       \ get next point
                        1 of wcount endof
                        2 of lcount endof
                        >r count r>
                endcase
                'type c@ 8 and 0=
                if      'type c@ 3 and          \ big endian
                        case    1 of byte-swap endof
                                2 of byte-rev  endof
                        endcase
                then
                'type c@ 16 and 0=
                if      'type c@ 3 and
                        case    0 of dup 127 > -128 and or endof
                                1 of dup 32767 > -32768 and or endof
                                2 of 0x80000000 xor endof
                        endcase
                then    ;

: SetAuto       { \ minY maxY -- }
\ sweep the points, find the max and min points, move the centerline, set scale.
                0x7FFFFFFF to minY
                0x80000000 to maxY
                'data 'len w@ 0
        do      nextpoint dup
                minY min to minY
                maxY max to maxY
        loop    drop
                height 2/
                maxY minY + 2/  ( centerline_of_data )
                + 'center w!    \ set centerline
                maxY minY - >r  \ span
                height r@ 1 max / 7 >
        if      r>drop 0x7FFFFFFF \ max gain
        else    height 0x10000000 r> */
        then    'gain ! ;


:M On_Paint:    ( -- )
\ Paint graphical data on a (color0) background
                hwnd if
                0 0 width height BLACK FillArea: dc   \ paint background
                chanptr >r 4 0
        do      i to chanptr
                'len w@
                if      'type c@ 32 and if SetAuto then
                        7 >color linecolor: dc  \ draw centerline
                        0 center moveto: dc
                        maxX center lineto: dc
                        'color c@ >color linecolor: dc
                        'data 'len w@ 0
                        do      nextpoint
                                'gain @ 0x10000000 */
                                center swap - ( y )
                                i 'pitch c@ * swap
                                i if    lineto: dc
                                else    moveto: dc
                                then
                                'type c@ 4 and                  \ alternating
                                if      1 'type c@ 3 and lshift +
                                then
                        loop    drop
                then
        loop    r> to chanptr
                then ;M

:M StartSize:   ( -- width height )  320 200 ;M  \ QVGA
:M MinSize:     ( -- width height )  64 64 ;M

1 [if]
variable live

:M Update:
\ Reload all waveform data
                chanptr >r 4 0
                live on
        do      i to chanptr
                'addr @ 'data 'len w@ pointlength *
                live on
                begin   dup 0>  live @ and
                while   3dup TarDataSize min
                        read-data key? or 0= live @ and live !
                        rot TarDataSize +
                        rot TarDataSize +
                        rot TarDataSize -
                repeat  3drop
        loop    r> to chanptr
                ;M

:M Refresh:     auto
        if      Update: self
                Paint: self
        then    ;M

: display       ( n -- ) 3 and
                chanptr >r to chanptr
                cr ." Channel " chanptr .
                ." [" 'label 8 -trailing type ." ] configuration:"
             cr 'len w@ . ." points of " 'type c@ >r
                r@ 16 and if ." un" then ." signed "
                r@ 3 and
                case 0 of ." 8-bit" endof
                     1 of ." 16-bit" endof
                     2 of ." 32-bit" endof
                     ." ???-bit"
                endcase
                r@ 4 and if ."  alternating" then
             cr ." Start address = " 'addr @ .
             cr r@ 32 and if ." Auto-scale " then
                ." Center=" 'center w@ . ." Gain=" 'gain @ .
                r>drop
             cr ." Pitch=" 'pitch c@ . ." Color=" 'color c@ .
                r> to chanptr ;

: showhelp      ( -- )
                scopehelptext help: scopehelp ;

:M dokey:       ( c f -- c f' )
\ Perform key action and set f if this window has focus,
                have-focus? hWnd and
        if      drop true               \ set the "have-active" flag
                over upc
                case    k_F5    of   Update: self  paint: self  endof
                        'P'     of   paint: self                endof
                        27      of   close: self                endof
                        k_F2    of   auto 0= to auto            endof
                        k_F1    of   showhelp                   endof
                        '0'     of   0 display                  endof
                        '1'     of   1 display                  endof
                        '2'     of   2 display                  endof
                        '3'     of   3 display                  endof
                endcase
        then    ;M


[then]

:M WindowTitle: ( -- Zstring )       z" Graph" ;M

;Object

3 channel: graf   clr: graf
2 channel: graf   clr: graf
1 channel: graf   clr: graf
0 channel: graf   clr: graf


