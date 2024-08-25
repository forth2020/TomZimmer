\ HTTP Web Server
\ Tom Dixon

\ *D doc\
\ *! WebServer
\ *T WebServer -- Webserver
\ *S Glossary

anew -WebServer.f

false value create-turnkey? \ set to false when you don't want a turnkey app
\ ------------------------ Character translation tables -----------------------

CREATE UCASETAB                         \ uppercase a thru z to A thru Z
               $00 C, $01 C, $02 C, $03 C, $04 C, $05 C, $06 C, $07 C,
               $08 C, $09 C, $0A C, $0B C, $0C C, $0D C, $0E C, $0F C,
               $10 C, $11 C, $12 C, $13 C, $14 C, $15 C, $16 C, $17 C,
               $18 C, $19 C, $1A C, $1B C, $1C C, $1D C, $1E C, $1F C,
               $20 C, $21 C, $22 C, $23 C, $24 C, $25 C, $26 C, $27 C, \ | !"#$%&'|
               $28 C, $29 C, $2A C, $2B C, $2C C, $2D C, $2E C, $2F C, \ |()*+,-./|
               $30 C, $31 C, $32 C, $33 C, $34 C, $35 C, $36 C, $37 C, \ |01234567|
               $38 C, $39 C, $3A C, $3B C, $3C C, $3D C, $3E C, $3F C, \ |89:;<=>?|
               $40 C, $41 C, $42 C, $43 C, $44 C, $45 C, $46 C, $47 C, \ |@ABCDEFG|
               $48 C, $49 C, $4A C, $4B C, $4C C, $4D C, $4E C, $4F C, \ |HIJKLMNO|
               $50 C, $51 C, $52 C, $53 C, $54 C, $55 C, $56 C, $57 C, \ |PQRSTUVW|
               $58 C, $59 C, $5A C, $5B C, $5C C, $5D C, $5E C, $5F C, \ |XYZ[\]^_|
               $60 C, $41 C, $42 C, $43 C, $44 C, $45 C, $46 C, $47 C, \ |`ABCDEFG|
               $48 C, $49 C, $4A C, $4B C, $4C C, $4D C, $4E C, $4F C, \ |HIJKLMNO|
               $50 C, $51 C, $52 C, $53 C, $54 C, $55 C, $56 C, $57 C, \ |PQRSTUVW|
               $58 C, $59 C, $5A C, $7B C, $7C C, $7D C, $7E C, $7F C, \ |XYZ{|}~ |
               $80 C, $81 C, $82 C, $83 C, $84 C, $85 C, $86 C, $87 C,
               $88 C, $89 C, $8A C, $8B C, $8C C, $8D C, $8E C, $8F C,
               $90 C, $91 C, $92 C, $93 C, $94 C, $95 C, $96 C, $97 C,
               $98 C, $99 C, $9A C, $9B C, $9C C, $9D C, $9E C, $9F C,
               $A0 C, $A1 C, $A2 C, $A3 C, $A4 C, $A5 C, $A6 C, $A7 C,
               $A8 C, $A9 C, $AA C, $AB C, $AC C, $AD C, $AE C, $AF C,
               $B0 C, $B1 C, $B2 C, $B3 C, $B4 C, $B5 C, $B6 C, $B7 C,
               $B8 C, $B9 C, $BA C, $BB C, $BC C, $BD C, $BE C, $BF C,
               $C0 C, $C1 C, $C2 C, $C3 C, $C4 C, $C5 C, $C6 C, $C7 C,
               $C8 C, $C9 C, $CA C, $CB C, $CC C, $CD C, $CE C, $CF C,
               $D0 C, $D1 C, $D2 C, $D3 C, $D4 C, $D5 C, $D6 C, $D7 C,
               $D8 C, $D9 C, $DA C, $DB C, $DC C, $DD C, $DE C, $DF C,
               $E0 C, $E1 C, $E2 C, $E3 C, $E4 C, $E5 C, $E6 C, $E7 C,
               $E8 C, $E9 C, $EA C, $EB C, $EC C, $ED C, $EE C, $EF C,
               $F0 C, $F1 C, $F2 C, $F3 C, $F4 C, $F5 C, $F6 C, $F7 C,
               $F8 C, $F9 C, $FA C, $FB C, $FC C, $FD C, $FE C, $FF C,

CREATE LCASETAB                         \ LOWERcase a thru z to A thru Z
               $00 C, $01 C, $02 C, $03 C, $04 C, $05 C, $06 C, $07 C,
               $08 C, $09 C, $0A C, $0B C, $0C C, $0D C, $0E C, $0F C,
               $10 C, $11 C, $12 C, $13 C, $14 C, $15 C, $16 C, $17 C,
               $18 C, $19 C, $1A C, $1B C, $1C C, $1D C, $1E C, $1F C,
               $20 C, $21 C, $22 C, $23 C, $24 C, $25 C, $26 C, $27 C, \ | !"#$%&'|
               $28 C, $29 C, $2A C, $2B C, $2C C, $2D C, $2E C, $2F C, \ |()*+,-./|
               $30 C, $31 C, $32 C, $33 C, $34 C, $35 C, $36 C, $37 C, \ |01234567|
               $38 C, $39 C, $3A C, $3B C, $3C C, $3D C, $3E C, $3F C, \ |89:;<=>?|
               $40 C, $61 C, $62 C, $63 C, $64 C, $65 C, $66 C, $67 C, \ |@abcdefg|
               $68 C, $69 C, $6A C, $6B C, $6C C, $6D C, $6E C, $6F C, \ |hijklmno|
               $70 C, $71 C, $72 C, $73 C, $74 C, $75 C, $76 C, $77 C, \ |pqrstuvw|
               $78 C, $79 C, $7A C, $5B C, $5C C, $5D C, $5E C, $5F C, \ |xyz[\]^_|
               $60 C, $61 C, $62 C, $63 C, $64 C, $65 C, $66 C, $67 C, \ |`abcdefg|
               $68 C, $69 C, $6A C, $6B C, $6C C, $6D C, $6E C, $6F C, \ |hijklmno|
               $70 C, $71 C, $72 C, $73 C, $74 C, $75 C, $76 C, $77 C, \ |pqrstuvw|
               $78 C, $79 C, $7A C, $7B C, $7C C, $7D C, $7E C, $7F C, \ |xyz{|}~ |
               $80 C, $81 C, $82 C, $83 C, $84 C, $85 C, $86 C, $87 C,
               $88 C, $89 C, $8A C, $8B C, $8C C, $8D C, $8E C, $8F C,
               $90 C, $91 C, $92 C, $93 C, $94 C, $95 C, $96 C, $97 C,
               $98 C, $99 C, $9A C, $9B C, $9C C, $9D C, $9E C, $9F C,
               $A0 C, $A1 C, $A2 C, $A3 C, $A4 C, $A5 C, $A6 C, $A7 C,
               $A8 C, $A9 C, $AA C, $AB C, $AC C, $AD C, $AE C, $AF C,
               $B0 C, $B1 C, $B2 C, $B3 C, $B4 C, $B5 C, $B6 C, $B7 C,
               $B8 C, $B9 C, $BA C, $BB C, $BC C, $BD C, $BE C, $BF C,
               $C0 C, $C1 C, $C2 C, $C3 C, $C4 C, $C5 C, $C6 C, $C7 C,
               $C8 C, $C9 C, $CA C, $CB C, $CC C, $CD C, $CE C, $CF C,
               $D0 C, $D1 C, $D2 C, $D3 C, $D4 C, $D5 C, $D6 C, $D7 C,
               $D8 C, $D9 C, $DA C, $DB C, $DC C, $DD C, $DE C, $DF C,
               $E0 C, $E1 C, $E2 C, $E3 C, $E4 C, $E5 C, $E6 C, $E7 C,
               $E8 C, $E9 C, $EA C, $EB C, $EC C, $ED C, $EE C, $EF C,
               $F0 C, $F1 C, $F2 C, $F3 C, $F4 C, $F5 C, $F6 C, $F7 C,
               $F8 C, $F9 C, $FA C, $FB C, $FC C, $FD C, $FE C, $FF C,

CODE ISTR=      ( adr1 len1 adr2 len2 -- flag ) \ compares two strings, case insensitive, true if equal
                mov     -4 [ebp], esi           \ save esi
                pop     edi                     \ edi=adr2
                pop     ecx                     \ ecx=len1
                pop     esi                     \ esi=adr1
                cmp     ebx, ecx                \ lengths equal?
                jne     short @@7               \ no, leave now=false

                add     ecx, esi                \ point to end of source
                xor     eax, eax                \ zero eax
                xor     ebx, ebx                \ and ebx
                jmp     short @@2               \ start checking

@@1:            mov     bl, 0 [edi]             \ get first byte
                mov     al, 0 [esi]             \ of each string
                mov     bl, ucasetab [ebx]      \ uppercase
                inc     edi
                inc     esi
                cmp     bl, ucasetab [eax]      \ equal?
                jne     short @@7               \ no, exit
@@2:            cmp     esi, ecx                \ past end of buffer?
                jb      short @@1               \ no, back round

                mov     ebx, # -1               \ ebx=true
                jmp     short @@9               \ leave

@@7:            xor     ebx, ebx                \ zero ebx
@@9:            mov     esi, -4 [ebp]           \ restore esi
                xor     edi, edi                \ zero edi
                next    c;

needs http
needs httpecho
needs httpmime
needs httpfile
\ needs browsefld

80 httpserver http
\ *G our http-server instance

: server-set-title ( -- )
\ *G set title of the console window
    Z" Win32Forth - HTTP Web Server" conHndl call SetWindowText drop ;

s" Help\html\" Prepend<home>\ webpath place
\ *G the path to the web content

: server-choose-path   { \ $path -- }
\ *G chosse the path for the folder with the web content
    z" Choose the web content folder."
    MAXSTRING LocalAlloc: $path webpath count $path place
\    $path conHndl BrowseForFolder
\    if   $path count webpath place
\         webpath ?+\
\    then
    s" .\" webpath place
;

: server-init   ( -- )
\ *G init the server
    http setup-http http serv-init ;

: server-run    ( -- )
\ *G run the server
    begin http serv-poll 10 ms key? until ;

: server-exit   ( -- )
\ *G close the server
    http serv-close

    create-turnkey?
    if   
      bye
    else 
      cr \ ." Type  WebServer  to restart the server."
    then 
;

: WebServer    ( -- )
\ *G Starts the server.
\ ** Tell your firewall that Win32Forth is allowed to use port 80. \n
\ ** Then you should able to see the server in your browser by using: \n
\ ** http://localhost/DPANSE.HTM \n
\ ** Assuming that the file p-index.htm is in your webpath. \n
\ ** When you would like to use the websever over the internet. \n
\ ** you must replace "localhost" by your IP-adres. \n
\ ** The IP-adres can be seen at http://checkip.dyndns.org \n

    server-set-title

    cls ." The Win32Forth V6.05 HTTP Web Server is running." cr

    server-choose-path  ( EG: ~\win32forth\Help\html\dpans )

    cr ." webpath = " webpath count type cr
    cr ." Press any key to stop the server."

    server-init
    server-run
    server-exit
    ;

CREATE &appdir MAXCOUNTED 1+ ALLOT \ static application directory
       &appdir OFF

: .appdir       ( -- ) \ type application directory
                &appdir COUNT ?dup if TYPE else drop ." not set" then ;


\ create an application that runs in the console
create-turnkey? [IF]

\ and create the application
&forthdir count &appdir place   \ in the Forth-directory
65536 65536 ' WebServer APPLICATION WebServer.exe
1 pause-seconds bye

[else]

\ WebServer

[then]

