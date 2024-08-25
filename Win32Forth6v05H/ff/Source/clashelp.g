\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ \\\\\\\\           Help window          \\\\\\\\\\\\\\\\\\\\\\\\\\\
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

\ This class is for simple context-sensitive help


:Class helpmsg  <SUPER msgwindow

:M WindowTitle: ( -- Zstring )
                z" Information"
                ;M

:M Help:        ( a -- )
                dup 65535 0 scan drop
                over -
                MessageText: self
                Start: self ;M

:M On_KillFocus: ( h m w l -- )
                On_Killfocus: super
                Close: self
                ;M

:M Classinit:   ( -- )
                ClassInit: super
                On_Init: super
                true to OnTop?
                ;M
;class


