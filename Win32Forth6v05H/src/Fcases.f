\ FCASES.F              Case extensions                 by Bozhil Makaveev
((

Subject: Few case extensions
Date: Sun, 16 Mar 1997 19:52:05 -0800
From: Bozhil Makaveev <bozhil@pacbell.net>
To: zforth@ix.netcom.com

Hi Tom or guys there,

These are some lines that I always add to my programs, so feel free to
use them, and if you like them - add to win32for files.

Regretabily, I'm not working in Forth company here, but in Bulgaria,
from where I came, I'm still partner in United Software Writers Ltd, one
of the software companies, which promoted Forth there. I have more than
10 years Forth programming, and currently I'm working on Winview  [now called
WinEd] improvements, which I'll post later.

You can contact me at:

Home phone and fax: (818) 348-2653
Work phone: (818) 883-5211 ext. 280.
))

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\
\       Different data types in case structures
\       By Bozhil Makaveev, bozhil@iname.com
\
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

INTERNAL

: ("of-compare) ( counted-source a1 c1 -- counted-source f )
                2>r dup count 2r> compare 0= dup if nip then ;

: ("of-include) ( counted-source a1 c1 -- counted-source f )
                2>r dup count 2r> compare 1 = dup if nip then ;

: ("of-contain) ( counted-source a1 c1 -- counted-source f )
                2>r dup count 2r> search nip nip dup if nip then ;

EXTERNAL

IN-SYSTEM

\ compares two strings for exact match

: "of-compare   ( n1 n2 n3 -- n1 )      \ extension to CASE for a string
                ?comp POSTPONE ("of-compare) POSTPONE ?branch >mark 4 ; immediate

\ compares two strings that start with a1,c1 string. Useful for building
\ interpeters where the first n characters of a keyword are taken into account.

: "of-include   ( n1 n2 n3 -- n1 )      \ extension to CASE for a string
                ?comp POSTPONE ("of-include) POSTPONE ?branch >mark 4 ; immediate

\ checks for tokens presence in the source string

: "of-contain   ( n1 n2 n3 -- n1 )      \ extension to CASE for a string
                ?comp POSTPONE ("of-contain) POSTPONE ?branch >mark 4 ; immediate

\ this is the most useful word, so I reserved the shortest name for it.

' "of-compare alias "of immediate

IN-APPLICATION

MODULE

\S      **********  Some test code ***********

: test_"of-compare ( -- )

                bl word

                case
                        s" string1" "of-compare ." string 1 matched" endof
                        s" string2" "of-compare ." string 2 matched" endof
                        s" string3" "of-compare ." string 3 matched" endof
                endcase ;

cr test_"of-compare string2

: test_"of-include ( -- )

                bl word

                case
                        s" string1" "of-include ." string 1 matched" endof
                        s" string2" "of-include ." string 2 matched" endof
                        s" string3" "of-include ." string 3 matched" endof
                endcase ;

cr test_"of-include string2_ThisIsNotImportantString

: test_"of-contain ( -- )

                bl word

                case
                        s" string1" "of-contain ." string 1 matched" endof
                        s" string2" "of-contain ." string 2 matched" endof
                        s" string3" "of-contain ." string 3 matched" endof
                endcase ;

cr test_"of-contain ThisIsNotImportantString_string2

