\ Win32forth has a built-in 486 disassembler...

\ Hooked into Firmware Studio, October 1999 by Brad Eckert

only forth also disassembler definitions
0 to sys-warning?

\ dis-op uses host address for its data
\ base-addr = target address - host address    ba = ta - ha   ha = ta - ba

variable baseoffset

: dis8086       ( at ah -- at' a len )
\ given target address and address of actual data, returns the next
\ disassembleable address and a string
                5 to disassyDatawidth   \ leave display space for 5 code bytes
                0 to base-addr
                2dup - baseoffset !
                dup dis-op - - s-buf count ;

: .label86      ( ah -- )       \ use our labels
                baseoffset @ +          \ xlate host to target address
                newout .label outpad count >s ;

' .label86 is show-name

4 cpuid" 8086|80486|"           \ CPU family string for type 4
4 new-disasm dis8086            \ disassembler for type 4

only forth also definitions
1 to sys-warning?

