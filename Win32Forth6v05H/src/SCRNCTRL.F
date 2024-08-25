\ SCRNCTRL.F            Screen Control words            by Tom Zimmer

cr .( Loading Screen Control...)

\ allow the user to set the current display FONT

: set-font      ( font_value -- )
                conHndl call GetDC >r     \ get and save the Device Control #
                call GetStockObject       \ return the object information
                r@ call SelectObject drop \ selects the object
                r> conHndl call ReleaseDC drop ;

: _>bold        ( -- )
                OEM_FIXED_FONT set-font ;

' _>bold is >bold

: _>norm        ( -- )
                ANSI_FIXED_FONT set-font ;

' _>norm is >norm

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\       Some cursor shape control words
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

: new-pointer   ( a1 -- )
                create ,
                does>   @ set-pointer ;

IDC_ARROW    new-pointer arrow-pointer
IDC_CROSS    new-pointer cross-pointer
IDC_IBEAM    new-pointer ibeam-pointer
IDC_ICON     new-pointer icon-pointer
IDC_NO       new-pointer noway-pointer
IDC_WAIT     new-pointer wait-pointer
IDC_UPARROW  new-pointer uparrow-pointer
IDC_SIZE     new-pointer size-pointer

\S

: .font         ( -- )
                256 16
                do      cr 8 i + i
                        do      i 4 .r space
                                i emit
                        loop
             8 +loop    ;


