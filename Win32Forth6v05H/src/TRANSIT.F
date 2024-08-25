\ Transient program support, for the assembler

cr .( Loading Transient Assembler...) 

\ 03-01-96 SMuB Adapted for dual dictionary 

\ Transient modules must reside IN-SYSTEM.
\ Anything compiled IN-APPLICATION will not be transient.

0 value transient-state

: start-transient ( -- )
                transient-ptr   abort" A transient module is already present"
                sys-free 100000 < abort" Increase System Dictionary space by 100k first!"
                &origin &sys-separation @ + &sys-size @ + 65536 - to transient-ptr
                in-system?
                if      1                       \ Remember which dictionary area we were
                else    2                       \ using before START-TRANSIENT
                then to transient-state
                in-system                       \ Transient module will reside in system area
                sys-here transient-ptr !        \ Save non-transient SDP
                transient-ptr cell+ sdp ! ;     \ Set SYS-HERE to where transient stuff goes

: end-transient ( -- )
                transient-state
                if
                        sys-here                \ Get pointer to end of transient module
                        transient-ptr @ sdp !   \ Reset SYS-HERE to non-transient area
                        to transient-ptr        \ Save pointer to end of module
                        transient-state 1 =
                        if      in-system       \ Return to the dictionary area we were using
                        else    in-application  \ before START-TRANSIENT
                        then
                        0 to transient-state    \ Say we're no longer compiling transient stuff
                then ;

: clear-transient ( -- )
                end-transient   \ Make sure we aren't transienting
                only forth also definitions       \ Make sure we aren't using transient vocabs application
                transient-ptr
                if      s" odd*name" "header here \ Make a forgettable word and remember its CFA dovar ,
                                                  \ Fill in the CFA.
                        transient-ptr sdp !       \ Set SYS-HERE to end of the transient module
                        (forget)                  \ Forget forgettable word and transient module
                        0 to transient-ptr        \ Say no transient module is present
                then    ;

\s Received from Steve Brault March 7th, 1996

        Date: Thu, 7 Mar 1996 14:22:25 -0700
        X-Sender: braultsm@rtd.com
        Mime-Version: 1.0
        To: zforth@ix.netcom.com (Tom Zimmer)
        From: 72163.3434@compuserve.com (Stephen M. Brault)
        Subject: TRANSIT.F for Win32Forth dual dictionary
        X-Attachments: I:\4THS\NOEDI31B\TRANSIT.F;

Tom:
        Here is a verion of the transient module code for split dictionaries.
        Note that it expects more free IN-SYSTEM space than the default.

        Steve Brault
        72163.3434@compuserve.com






