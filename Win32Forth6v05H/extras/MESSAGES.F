\ MESSAGES.F       Message and DLL interface support for Win32Forth    by Tom Zimmer

((  This file provides a mechanism for creating windows messages and DLL interface
functions that can be used by other programs for accessing Win32Forth.  At the
time each message function or DLL function is created, constants are written
to a pair of global equate files.  The two files are called MESSAGES.H and
MESSAGES.BAS.  These files can be included within a Visual C++ or Visual Basic
project to allow them to interface to Win32Forth published functons.  Below
is listed examples of the type of files created;

************************************************************************************************
************************************************************************************************

// MESSAGES.H *** WARNING DO NOT MODIFY ***
// *** Machine generated common equate file ***

#define WP_MESSAGEBASE 100 // the first message in the message header file
#define WP_NORMALIZE 101   // normalize window and force to forground
#define DLL_GETSCAN 102    // lParam = SPECTRUM structure, get a spectrum from data system
#define DLL_DATAFILE 103   // lParam = "drive:\directory\filename.MS", open the datafile
#define DLL_CALIFILE 104   // lParam = "drive:\directory\filename.QCI/QCE", open INT or EXT CALI file
#define DLL_SHOWDATA 105   // lParam = SW_?????????,  show/hide/minimize/etc. the data system window

************************************************************************************************
************************************************************************************************

'MESSAGES.BAS *** WARNING DO NOT MODIFY ***
'*** Machine generated common equate file ***

Public Const WP_MESSAGEBASE = 100 ' the first message in the message header file
Public Const WP_NORMALIZE = 101   ' normalize window and force to forground
Public Const DLL_GETSCAN = 102    ' lParam = SPECTRUM structure, get a spectrum from data system
Public Const DLL_DATAFILE = 103   ' lParam = "drive:\directory\filename.MS", open the datafile
Public Const DLL_CALIFILE = 104   ' lParam = "drive:\directory\filename.QCI/QCE", open INT or EXT CALI file
Public Const DLL_SHOWDATA = 105   ' lParam = SW_?????????,  show/hide/minimize/etc. the data system window 

************************************************************************************************
************************************************************************************************

Notice that the comments following the equates are extracted from the source file and
automatically included.

The functions available from this file for your use are;

#newmessage   ( n1 -<name>- )           defines a newmessage "name" with value n1

        usage:  1234 #NEWMESSAGE ONETWOTHREEFOUR

znewmessage   ( -<name> <string>- )     defines a null terminated string

        usage:  ZNEWMESSAGE MYFILE C:\TEST\MYFILE.XYZ

"NEWMESSAGE   ( a1 n1 -- )              define auto-increment message from a1,n1

        usage:  s" HELLO" "NEWMESSAGE   ( used in a colon definition )

NEWMESSAGE    ( -<name>- )              define auto-increment message from "name"

        usage:  NEWMESSAGE HELLOTOO

:MESSAGE      ( -<name>- )              define a message name to recognize in Win32Forth's
                                        global message handler.  "name" becomes a wParam
                                        message automatically detected and passed to
                                        the remainder of the definition.

        usage: :MESSAGE FORTH_MSG1  ( -- )
           ...                      cr ." Message received" ;

:DLLFunc      ( -<name>- )              define a DLL entry point, actually a colon definition
                                        that you can call from an external program using
                                        the DLL interface mechanism of Win32.  See the file;
                                        MAKEDLL.F for the second part of DLL operation.

        usage: :DLLFunc DLL_MYFUNC  ( -- )
                                    cr ." Doing MYFUNC" ;

See the file MAKEDLL.F for additional information on making a DLL.  This file is normally
loaded before your application, and then MAKEDLL.F is loaded at the end, to save the
image file to FORTHDLL.IMG


MESSAGEFILE   ( n1 -<name>- )           create a message file containing #define constants
                                        with a base number of n1.

        usage:  100 MESSAGEFILE MESSAGES



))

new-chain message-chain
new-chain DLLFunc-chain

0 value message#                        \ number of next message created
0 value msghndlh
0 value msghndlb
create msgnameh max-path allot   \ must be static, to preserve the filename
       msgnameh off              \ across multiple compiles and fsaves
create msgnameb max-path allot   \ must be static, to preserve the filename
       msgnameb off              \ across multiple compiles and fsaves

: ?open-messagefile ( -- )      \ make sure the message file is open
                msghndlh ?exit                          \ exit if it's already open
                msgnameh count r/w open-file            \ try to open it
                abort" Failed to open message file"     \ abort if we can't
                to msghndlh                             \ save the file handle
                msghndlh file-append drop               \ move to end of file

                msgnameb count r/w open-file            \ try to open it
                abort" Failed to open message file"     \ abort if we can't
                to msghndlb                             \ save the file handle
                msghndlb file-append drop ;             \ move to end of file

: make#define   { val adr len \ temp$ -- }
                MAXSTRING LocalAlloc: temp$
                ?open-messagefile
                msghndlh 0= abort" No message file created yet!"
                s" #define "   temp$   place            \ start with #define
                adr len        temp$  +place            \ append the name
                s"  "          temp$  +place            \ and a space
                val (.)        temp$  +place            \ append the value
                0x0D           temp$ c+place            \ plus carraige return
                0x0A           temp$ c+place            \ plus line feed
\ #define <name> <value> <crlf>
                temp$ count msghndlh write-file          \ write the #define to file
                abort" Failed to write to message file" ;

: makePubConst  { val adr len \ temp$ -- }
                MAXSTRING LocalAlloc: temp$
                ?open-messagefile
                msghndlb 0= abort" No message file created yet!"
                s" Public Const " temp$   place         \ start with #define
                adr len           temp$  +place         \ append the name
                s"  = "           temp$  +place         \ and a space
                val (.)           temp$  +place         \ append the value
                0x0D              temp$ c+place         \ plus carraige return
                0x0A              temp$ c+place         \ plus line feed
\ Public Constant <name> = <value> <crlf>
                temp$ count msghndlb write-file          \ write the #define to file
                abort" Failed to write to message file" ;

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ make a #define and a constant from name adr,len with the message number val
: #"newmessage  { adr len val typ \ temp$ temp2$ comment$ -- }
                MAXSTRING LocalAlloc: temp$
                MAXSTRING LocalAlloc: temp2$
                MAXSTRING LocalAlloc: comment$
                adr len temp2$ place
                ?open-messagefile
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
                msghndlh 0= abort" No message file created yet!"
                s" #define "           temp$   place     \ start with #define
                temp2$ count           temp$  +place     \ append the name
                s"  "                  temp$  +place     \ and a space
                typ                    
                if     '"'             temp$ c+place
                       bl word         count
                                       temp$  +place
                       '"'             temp$ c+place
                else   val (.)         temp$  +place     \ append the value
                then                   
                s"  // "               temp$  +place
                bl word drop                             \ discard following word
                0 word count bl skip comment$ place      \ save remainder of comment away
                comment$ count         temp$  +place
                0x0D                   temp$ c+place     \ plus carraige return
                0x0A                   temp$ c+place     \ plus line feed
\ #define <name> <value> <crlf>
                temp$ count msghndlh write-file          \ write the #define to file
                abort" Failed to write to message file"
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
                msghndlb 0= abort" No message file created yet!"
                s" Public Const "      temp$   place            \ start with #define
                temp2$ count           temp$  +place     \ append the name
                s"  = "                temp$  +place     \ and a space
                typ                   
                if     '"'             temp$ c+place
                       pocket count
                                       temp$  +place
                       '"'             temp$ c+place
                else   val (.)         temp$  +place     \ append the value
                then                   
                s"  ' "                temp$  +place
                comment$ count         temp$  +place
                0x0D                   temp$ c+place     \ plus carraige return
                0x0A                   temp$ c+place     \ plus line feed
\ Public Constant <name> = <value> <crlf>
                temp$ count msghndlb write-file          \ write the #define to file
                abort" Failed to write to message file"
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
                typ
                if      s" CREATE "    temp$   place
                        temp2$ count   temp$  +place
                        s$ #  z," #    temp$  +place
                        pocket count   temp$  +place
                        s$ #"#         temp$  +place
                else    val (.)        temp$   place    \ build a constant source string
                        s"  CONSTANT " temp$  +place
                        temp2$ count   temp$  +place
\ <value> CONSTANT <name>
                then
                temp$ count evaluate ;          \ make a constant
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

: #NEWMESSAGE   ( n1 -<name>- ) \ define a newmessage "name" with value n1
                bl word count rot FALSE #"newmessage ;

: ZNEWMESSAGE   ( -<name> <string>- )
                bl word count   0 TRUE  #"newmessage ;

\ make a #define and a constant from name adr,len with the next message number
: "NEWMESSAGE   ( a1 n1 -- )
                message# FALSE #"newmessage
                1 +to message# ;                        \ bump to next message number

: NEWMESSAGE    ( -<name>- )    \ define a new message from "name"
                bl word count "newmessage ;

: ">messagefileh ( a1 n1 -- )
                msghndlh write-file abort" Error writing to message file" ;

: ">messagefileb ( a1 n1 -- )
                msghndlb write-file abort" Error writing to message file" ;

: MESSAGEFILE   ( n1 -<name>- )         \ create a message file containing #define constants
                to message#             \ starting message number
                0 to msghndlh           \ initialize handle to null
                0 to msghndlb           \ initialize handle to null

                bl word count msgnameh  place
                s" .H"        msgnameh +place

                pocket  count msgnameb  place
                s" .BAS"      msgnameb +place

                msgnameh count r/w create-file abort" Failed to create message file"
                to msghndlh
                s" // "                                 ">messagefileh
                msgnameh count                          ">messagefileh
                s"  *** WARNING DO NOT MODIFY ***\n"    ">messagefileh
                s" // *** Machine generated common equate file ***\n\n"
                ">messagefileh

                msgnameb count r/w create-file abort" Failed to create message file"
                to msghndlb
                s" '"                                   ">messagefileb
                msgnameb count                          ">messagefileb
                s"  *** WARNING DO NOT MODIFY ***\n"    ">messagefileb
                s" '*** Machine generated common equate file ***\n\n"
                ">messagefileb ;

\ define constants and words that are automatically placed on the Forth message-chain
: :MESSAGE      { \ temp$ -<name>- } \ define a colon definition in the message function chain
                MAXSTRING LocalAlloc: temp$
                bl word count temp$ place       \ get the message name
                temp$ count upper
                temp$ count "newmessage         \ define a new message constant
                message-chain noop-chain-add >r     \ add a noop entry to the message chain
                :noname >r                      \ define a colon definition & save cfa
                postpone dup
                temp$ find 0= abort" Failed to create the message constant"
                ,
                postpone <>
                postpone ?exit
                r> r> ! !csp ;                  \ link headerless word into the message chain

: :DLLFunc      { \ temp$ -<name>- } \ define a colon definition in the DLL function chain
                MAXSTRING LocalAlloc: temp$
                bl word count temp$ place       \ get the message name
                temp$ count upper
                temp$ count "newmessage         \ define a new message constant
                DLLFunc-chain noop-chain-add >r     \ add a noop entry to the message chain
                :noname >r                      \ define a colon definition & save cfa
                s" { lParam wParam message function -- return }" evaluate
                postpone LOCAL3
                postpone LOCAL2
                postpone LOCAL1
                postpone LOCAL0
                postpone dup
                temp$ find 0= abort" Failed to create the message constant"
                ,
                postpone <>
                postpone ?exit
                r> r> ! !csp ;                  \ link headerless word into the message chain

: init-messagefile ( -- )       \ make sure message file is open if needed
                0 to msghndlh
                0 to msghndlb ;

initialization-chain chain-add init-messagefile

: uninit-messagefile ( -- )
                msghndlh
                if      s" \n" ">messagefileh
                        msghndlh close-file drop
                        0 to msghndlh
                then
                msghndlb
                if      s" \n" ">messagefileb
                        msghndlb close-file drop
                        0 to msghndlb
                then    ;

unload-chain chain-add-before uninit-messagefile

100 messagefile MESSAGES        \ define the message file now

newmessage WP_MESSAGEBASE       \ the first message in the message header file

FALSE value doing-dll?          \ set to true by DLLHello, to be used as a flag
                                \ by your program to tell if it is running as a DLL
FALSE value dll-result          \ A flag that should be set to TRUE by each :DLLFunc
                                \ the succeeds with it's function. This value is
                                \ passed back to the caller as the result of each
                                \ DLL function call.

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\       DLL function support words, used to assure Forth won't ever abort
\       and cause the caller to hang waiting for Forth to return.
\
\       See DLL_INTERPRET in MAKEDLL.F for an example of usage for DO-DLL
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

: do-dll-func   ( a1 -- )               \ perform func a1 don't allow any error abort
                catch 0=
                if      1 to dll-result                         \ all went ok
                then    ;

: DO-DLL        ( -<name>- )
                compile LIT ' , compile do-dll-func ; immediate



