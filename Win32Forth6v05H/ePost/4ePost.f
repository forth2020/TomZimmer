anew 4ePost.f        \  October 10th, 2002 - 21:01

needs toolset.f
needs muTimer.f     \ A High resolution timer by Floyd
needs struct.f
needs shell_r.f
needs w_search.f
needs sockets.f     \ Windows Sockets         By Andrey Cherezov

WinLibrary WININET.DLL

struct{ \ servent
     DWORD s_name
     DWORD s_aliases
     SHORT s_port
     DWORD s_proto
}struct servent


: getservbyname  ( z"string" n - &servent ior )
   swap rel>abs call getservbyname dup abs>rel swap 0=
 ;

: get-mail-server-port ( - port )
   z" mail" 0 getservbyname
     if    drop IPPORT_SMTP \ try the default port 25
     else  s_port w@ call htons
     then
  ;

0 value TreeView
0 value pop3-socket
0 value smtp-socket
0 value nntp-socket

30 string:  pop3-server$ pop3-server$ string" pop.wxs.com"
30 string:  smtp-server$ smtp-server$ string" mail.wxs.com"
30 string:  nntp-server$ nntp-server$ string" news.planet.com"

: get-pop3-server  ( - IP ior ) pop3-server$  1+ zGetHostIP ;
: get-smtp-server  ( - IP ior ) smtp-server$  1+ zGetHostIP ;
: get-nntp-server  ( - IP ior ) nntp-server$  1+ zGetHostIP ;

110 value pop3-port
 25 value smtp-port
119 value nntp-port

: init-pop3  ( - s )   pop3-server$ count pop3-port client-open   ;
: init-smtp  ( - s )   smtp-server$ count  ( get-mail-server-port) smtp-port client-open ;
: init-nntp  ( - s )   nntp-server$ count nntp-port client-open  ;


: .send    ( adr u - adr u )  cr ." > " 2dup type ;

: "wP ( adr u - )
    .send pop3-socket WriteSocketLine abort" Can't write to the pop3-server."
  ;

: "wS ( adr u - )
    .send smtp-socket WriteSocketLine abort" Can't write to the smtp-server."
 ;

: "wN ( adr u - )
    .send nntp-socket WriteSocketLine abort" Can't write to the nntp-server."
 ;

max-dyn-string value /receive-buffer    0 value receive-buffer
: init-receive-buffer   ( - )    /receive-buffer malloc to receive-buffer ;

max-dyn-string value /new-msg-buffer    0 value new-msg-buffer
: init-new-msg-buffer   ( - )    /new-msg-buffer malloc to new-msg-buffer ;

init-receive-buffer  initialization-chain chain-add init-receive-buffer
init-new-msg-buffer  initialization-chain chain-add init-new-msg-buffer


: read-pop3-socket ( buffer n - n f )    pop3-socket ReadSocket  ;
: read-smtp-socket ( buffer n - n f )    smtp-socket ReadSocket  ;
: read-nntp-socket ( buffer n - n f )    nntp-socket ReadSocket  ;

: _rP ( adr size - u )  read-pop3-socket abort" pop3-server did not respond." ;
: rP  ( adr - adr u )   dup /receive-buffer _rP ;

: rS   ( adr - adr u )
   dup maxstring read-smtp-socket abort" smtp-server did not respond."
 ;

: rN    ( adr - adr u )
   dup /receive-buffer read-nntp-socket abort" nntp-server did not respond."
 ;

: +tmp$ ( adr count - ) tmp$ +place ;

: open-line ( adr size - adr2 /line )
   2dup  s" *" tmp$ place crlf$ count +tmp$ tmp$ count
   2swap w-search
      if   drop nip over - 2 +
      else 2drop over + nip 0
      then
 ;

: -eol ( n - n1 )  2 - 0 max  ;

\ receive and type
: rtype ( adr count - )    open-line -eol type  ;

: rPt  ( - )   cr /receive-buffer rP rtype ; \ receive from Pop3-server and type
: rSt  ( - )   cr here rS rtype ;
: rNt  ( - adr count )    cr receive-buffer rN 2dup rtype ;

: "wNr  ( adr-send u - adr-rec count-rec ) "wN rNt ;
: "wNr_ ( adr-send u - ) "wN rNt 2drop ;
: "wSr  ( adr u - )      "wS rSt ;
: "wPr  ( adr u - )      "wP rPt ;

(( : wP ( <stream> - )
    cr ." > " carret word count tmp$ place tmp$ count  2dup type
    pop3-socket WriteSocketLine abort" Can't write to the pop3-server."
    rPt
 ; ))


maxstring create msg-id$ allot

: +msg-id$ ( n pos - )   swap s>d <# ##d #> msg-id$ +place ;

: gen-msg-id ( n - )
   today jjjjmmdd count msg-id$ place
   time-buf dup>r 8 + w@ 2  +msg-id$
     r@ 10 + w@ 2  +msg-id$
     r@ 12 + w@ 2  +msg-id$
     r> 14 + w@ 4  +msg-id$
     th FFFF and   4  +msg-id$
 ;

: +space>$ ( adr - )  s"  " rot +place  ;
: n>tmp$   ( n - )    0 (d.) tmp$ place ;
: n>+tmp$  ( n - )    0 (d.) +tmp$ tmp$ +space>$ ;

5 constant len-last-octet
s" RE: " tmp$ place tmp$ 1+ @ constant repl$_   \ Only valid for 32bits Forth
s" Fw: " tmp$ place tmp$ 1+ @ constant fw$_     \ Only valid for 32bits Forth

variable  mAutodisconnect- true mAutodisconnect- !
variable  mSpam-           true mSpam- !

0 value &account
0 value &outbox
0 value &last-cluster
0 value &msg-viewing

22 constant /msg-ID
75 constant /line
35 constant /date

 30 constant message-
200 constant deleted-cluster-
201 dup constant adresbook- constant cluster-
202 constant msg-list-
204 constant msg-grp-
206 constant account-     \ accounts are also in the adresbook
210 constant spam-


\ Record discriptions of headers.seq in which headers and mailclusters exist
\ Note: The next version might be different
\ A cluster can be an account group list or an email adres

struct{ \ msg-header
               /line Field:  msg-To
                   1 Field:  cnt-To

                    4 Field:  msg-subject-flag \ Re: Fw:
offset acc-cluster-name
                /line Field:  msg-Subject
offset acc-cnt-cluster-name
                    1 Field:  cnt-Subject \ including msg-subject-flag when > 0
offset  msg-flag
              /msg-ID Field:  record-ID \ The first byte = msg-flag. ASCII means it is a msg
offset acc-name
                /line Field:  msg-From
offset acc-cnt-name
                    1 Field:  cnt-From
offset next-group
                /date Field:  msg-Date
                    1 Field:  cnt-Date
                    1 Field:  msg-reserved1
offset acc-password
                /line Field:  msg-Reply-To
offset acc-cnt-password
                    1 Field:  cnt-Reply-To
                    1 Field:  msg-Status        \ 0=read 1=to-send
offset &h-cluster
              1 cells Field:  &R-cluster  \ rel pointer to cluster
offset grp-last-read
              1 cells Field:  next-Outbox
offset msg-protocol   \ When msg is in the outbox 0=msg
              1 cells Field:  msg-reserved2
}struct msg-header

: acc-spam$      ( rec-adr - spam$ )   [ 0 acc-password 34 + ] literal + ;
: acc-cluster-*  ( rec-adr - spam$ )   [ 0 acc-cluster-name 1 - ] literal + ;

sizeof msg-header to record-size \ should be < 2024 ( buffer-size)

: by-subject ( - )   0 cnt-Subject to key-len  0 msg-Subject to key-start  ;
: by-date ( - )      /msg-ID to key-len  0 record-ID to key-start  ;

by-subject

struct{
  maxstring field: tmp-to
  maxstring field: tmp-subj
  maxstring field: tmp-msg- 
  maxstring field: tmp-msg-id
}struct  tmp-buffer

s" test1" msg-id$ place

22 string: database-headers$  database-headers$ string" headers.seq"

: ?create-database ( - ) \ If it does not exist
   database-headers$  count file-exist? not
    if  database-headers$ count r/w create-file
        abort" Can't create database for headers."
        1 swap extend-file
    then
 ;

map-handle out-hndl
map-handle msg-hndl
map-handle hdr-hndl
map-handle hdr-idx-hndl
map-handle database-hdr-hndl

: file-size>s (  ( fileid -- len )    file-size drop d>s  ;

: map-hndl>vadr ( m_hndl - vadr ) >hfileAddress @ ;

: >last-record ( database_hdr_hndl record-size - vadr )
    >r dup map-hndl>vadr swap >hfileLength @ r> - +
 ;

: >carret$ ( from count - adr count )
   2dup carret scan 0<>
     if    nip over -
     else  drop
     then
 ;

: cont_@_?   ( adr len - >adr len )    ascii @ scan  ;

hex

create eot-octet   0d c, 0a c, 2e c, 0d c, 0a c,
create end-of-body ascii * c, 0d c, 0a c, 0d c, 0a c,

decimal

5 constant /end-of-body

: header-missing? ( adr count - adr count )
   dup 0=
       if    2drop s" -- Missing --"
       then
 ;

: *>buffer ( - )   s" *" buffer place  ;
: *new-line>buffer ( - )   s" *" buffer place   eot-octet 2 buffer +place ;

: get-msg-field  ( hdr len adr-spec len - hdr /hdr adr-after len )
   buffer +place buffer count
   2over w-search
      if 2over 2swap -string >carret$
      else 2drop over false
      then
   /line min
 ;

: get-header-from-new-line ( hdr len adr-spec len - hdr /hdr adr-after len )
   *new-line>buffer get-msg-field
 ;

: get-from-field ( hdr len - hdr len adr-after len )
   s" From: " *>buffer get-msg-field header-missing?
 ;

: get-Subject-field ( hdr len - hdr len adr-after len )
   s" Subject: " get-header-from-new-line header-missing?
 ;

: get-to-field ( hdr len - hdr len adr-after len )
   s" To: " get-header-from-new-line
 ;

: get-cc-field ( hdr len - hdr len adr-after len )
   s" Cc: " get-header-from-new-line
 ;

: get-bcc-field ( hdr len - hdr len adr-after len )
   s" Bcc: " get-header-from-new-line
;

: get-date-field ( hdr len - hdr len adr-after len )
   s" Date: " get-header-from-new-line header-missing?
 ;

: get-replyto-field ( hdr len - hdr len adr-after len )
   s" Reply-To: " get-header-from-new-line
 ;

: extract-email-adres ( adr len - email-adr len flag )
   2dup ascii < ascii > unfold
     if    2swap
     then
   2drop 2dup cont_@_? nip 0> swap /line min swap
 ;

: write-msg-field ( adr len field-adr cnt-adr  - )
   >r swap dup>r cmove 2r> swap c!
 ;

: ?write-msg-field ( adr len vadr cnt-adr flag - )
    if    write-msg-field
    else  4drop
    then
 ;

30 string: header-file$
30 string: file$
0 value msgtree-base


: set-data-pointers
   database-hdr-hndl  map-hndl>vadr to records-pointer
   hdr-idx-hndl       map-hndl>vadr to aptrs
 ;

: last-record ( - adr )   database-hdr-hndl record-size >last-record ;

: map>last-record ( - adr>lastrecord )
   database-headers$ count database-hdr-hndl open-map-file drop
   last-record
   set-data-pointers
 ;

: unmap-msg-base  ( - )
    database-hdr-hndl    dup flush-view-file drop close-map-file
   hdr-idx-hndl dup flush-view-file drop close-map-file 2drop
 ;

: size-database (  hndl - size ) >hfileLength @ ;

: extend-database \ with 1 record
   unmap-msg-base
   record-size database-headers$ count
   r/w open-file abort" Can't find file to extend."
   dup file-size>s 1 =
     if  -1 s>d 2 pick reposition-file throw swap 1- swap
     then
   extend-file
   map>last-record record-size erase
 ;

0 value &r>group
/line string: newsgroup$
12173 value id-last-read-news

: cluster-mail  ( to cnt  rec-msg - spam-in-msg- ) \ 0 = cluster found
   0 locals| spam-in-msg- |
          swap 1 max swap -rot &last-cluster r>record
                begin
                    dup>r  msg-flag c@ account- =
                         if   r@ acc-spam$ count
                         else r@ acc-cluster-* r@ acc-cnt-cluster-name c@ 1+
                         then
                         2over
                    w-search nip nip 
                       if    r@ record>r 3 pick &R-cluster ! r>drop false
                             dup to spam-in-msg-
                       else  r> next-group @ dup r>record
                             swap 0<> and
                       then
                dup 0=
                until
            4drop  spam-in-msg-
 ;

: cluster-news  ( cluster-news to cnt rec-msg - )
      3 pick  over &R-cluster !
      &r>group r>record grp-last-read id-last-read-news swap ! 3drop
 ;

: strip-Re ( adr1 cnt1 rec-adr - )
   >r  cr ."    Subject: " 2dup type
   /line min over @  sp@ 4 upper repl$_ =
      if    r@ msg-subject-flag  s" Re: " 4 pick swap cmove
      else  r@ msg-Subject   0 r@ msg-subject-flag c!
      then
   r> cnt-Subject write-msg-field
 ;

: hdr>database ( - header count hdr-record )
   header-file$ count hdr-hndl open-map-file abort" Unable to map the header."
   hdr-hndl dup map-hndl>vadr swap >hfileLength @
   extend-database last-record >r
   0 r@ &R-cluster !
   get-from-field  2dup r@ msg-From r@ cnt-From  write-msg-field r@ cluster-mail drop
   get-subject-field  r@ strip-Re
   get-date-field     /date min   r@ msg-Date  r@ cnt-Date  write-msg-field
   msg-id$ count      r@ record-ID swap cmove
   get-replyto-field  dup
     if    r@ msg-Reply-To r@ cnt-Reply-To  write-msg-field
     else  2drop
     then
   get-to-field 2dup r@ cluster-mail
      if  2drop get-cc-field r@ cluster-mail
           if  2drop s" In Cc-list"
           else get-bcc-field 2dup r@ cluster-mail
                  if 2drop s" In Bcc-list"
                  then
           then
      then
      r@ msg-To r@ cnt-To  write-msg-field
   r>
 ;

10 string: hd-index-file$ hd-index-file$ string" hdr.idx"

: create-index-file ( #bytes - f )
   hd-index-file$ create-file-ptrs
   hd-index-file$ open-file-ptrs
   extend-file
 ;

: rebuild-index-hdrs  ( - ) \ database must mapped
   database-hdr-hndl #records-in-database build-file-ptrs
 ;

: map-msg-base
   ?create-database database-headers$ count database-hdr-hndl open-map-file throw
   hd-index-file$ count file-exist? not
      if    1 create-index-file
      then
   hd-index-file$    count hdr-idx-hndl open-map-file throw
   set-data-pointers
    database-hdr-hndl #records-in-database
   hdr-idx-hndl >hfileLength @ dup>r cell / 2dup >
      if    hdr-idx-hndl close-map-file throw
            2dup - cells r@ 1 =
              if   0 create-index-file
              then
            hd-index-file$ open-file-ptrs  extend-file
            hd-index-file$ count hdr-idx-hndl open-map-file throw
            hdr-idx-hndl map-hndl>vadr to aptrs
            swap add-file-ptrs
      else  2drop
      then  r>drop
 ;

: sync-pointers ( - ) unmap-msg-base  map-msg-base ;
: type-space    ( adr cnt -  )   type space  ;
: type-cr       ( adr cnt -  )   type cr  ;

:inline message?      ( rec-adr - flag )   msg-flag c@ deleted-cluster- <  ;
:inline include-subject-flag    ( adr - adr-incl-Re )
   msg-subject-flag dup c@ 0=
      if  4 +
      then
 ;

: list-headers ( - ) \ The database must be mapped, use: map-msg-base list-headers
  hdr-idx-hndl >hfileLength @ cell / 0
     do    cr i dup . n>record .
           i n>record >r
           r@ message?
               if   r@ record-ID     /msg-ID                       type-space
                    r@ msg-To        r@ cnt-To c@       ." To: "   type-space
                    r@ include-subject-flag  r@ cnt-Subject c@  ." Subj: " type-cr tab
                    r@ msg-From      r@ cnt-From c@     ." From: " type-space
                    r@ msg-Date      r@ cnt-Date c@                type-space tab
                    r@ msg-Reply-To  r@ cnt-Reply-To c@ ." Reply to: "   type
                    r> ."  ->" &R-cluster ?

               else r>drop
               then
     loop
 ;

: list-clusters
   cr cr ." Clusters: " cr &last-cluster
       begin
         dup . tab r>record >r
           r@ msg-flag c@ cluster- =
               if    r@ msg-From  r@ cnt-From c@  ." Account of: "   type-space
               then
           r@ msg-Subject   r@ cnt-Subject c@   type-space tab
           cr
           r> next-group @ dup
       0= until
    drop
 ;

10 string: r-end$   s" *" r-end$ place   eot-octet len-last-octet r-end$ +place

: r-end?   ( buffer n -  buffer n flag )
     2dup 1- tuck + swap lf -scan  5 <
             if    always
             else  dup 4 - len-last-octet
                   eot-octet len-last-octet compare
                     if    drop false
                     else  nip over - 1+ true
                     then
             then
 ;

: retr-msg-fragments   ( buffer size - adr cnt )
   -dup -dup 0 locals| cnt max-size |
       begin   max-size _rP  r-end?
               not over +to cnt over negate +to max-size
       while   + dup
       repeat
    2drop cnt
 ;

: scan-for-body ( buffer count - body )
   end-of-body /end-of-body 2swap w-search not abort" No header received." +
 ;

\ Notes:
\ 1. My may ISP generate 2 different headers for the same when asked 2 times !
\ 2. Some fields belong to the header, not to the body.
\ 3. Sometimes he loves to add a number of zero's after sending an email.

: retr ( n total-size hdr-size - ) \ n and size are retrieved from maillist.tmp
   drop locals|  total-size   |
   total-size maxstring + to total-size
   s" retr " tmp$ place  n>+tmp$ tmp$ count "wP
   total-size malloc dup>r  total-size retr-msg-fragments to total-size
   msg-id$ count pad place s" .bdy"  +pad$ pad +null
   pad count r/w create-file abort" Can't create file for message."  >r
   dup total-size scan-for-body tuck swap - total-size swap -
   r@ write-file abort" Can't write to messsage."
   r> close-file drop
   r> release
;

0 value #new-mail

: dele    ( n - )        s" DELE " tmp$ place n>+tmp$ tmp$ count "wPr ;
: +ok?    ( adr len - )  s" +OK*" 2swap w-search nip nip  ;
: rP-ok?  ( - )          receive-buffer rP +ok? ;
: rS-ok?  ( - )          pad rS +ok? ;

: transaction-state? ( - flag )     s" noop" "wP  rP-ok? ;

: msg-header-name ( - name$ )
   msg-id$ count header-file$ place s" .hdr" header-file$ +place header-file$
 ;

: file-name$ ( extension count - file$ )
   msg-id$ count file$ place file$ +place file$ +null file$
 ;

: cut-line ( adr - next-line /buffer )   dup>r open-line  tuck + r> rot -  ;

: wP-ok?  ( - )    "wP  receive-buffer 5 _rP drop  ;

\ The header goes 2 times over the line.
\ 1. To get the size and to look for spam.
\ 2. To get the mail.
\ The idea is to be able to delete spam before downloading the hole message.

: get-header ( n - size-header )
    true 0 locals| size first-line- |
    msg-header-name count r/w create-file abort" Can't create message file." >r
    s" top " tmp$ place n>+tmp$ 0 n>+tmp$ tmp$ count wP-ok?

      begin   receive-buffer off receive-buffer rP r-end?  not  \ first-line- and
      while   first-line-   \ skip first line in mail.tmp
                 if    dup>r open-line  tuck + r> rot - false to first-line-
                 else
                 then
              dup +to size r@ write-file abort" Can't save header."
      repeat
   len-last-octet -
   dup +to size r@ write-file drop
   r> close-file drop size
 ;

: get-mail ( - #new-mail )
    0  s" maillist.tmp" r/w open-file drop >r  \ min 1 file-length
        begin  buffer maxstring r@ read-line drop
        while  buffer over bl scan >r 1+ r@ 1- number? 2drop
               buffer rot r> - number? 2drop
               swap  over 0> over 0> and
                        if    over gen-msg-id over get-header
                              hdr>database  3drop
                              mSpam- @  last-record &R-cluster @ or
                                  if    retr
                                  else  3drop cr ." Spam. Not downloaded."
                                  then
                              1+ dup dele
                              database-hdr-hndl close-map-file
                              hdr-hndl close-map-file 2drop
                        else  2drop
                        then
        repeat
   r> close-file 2drop
 ;

: mail-list ( - )
   s" LIST"  "wP \ check +OK
   s" maillist.tmp" r/w create-file abort" Can't create message file." >r
      begin   receive-buffer off receive-buffer rP r-end? not
      while   r@ write-file abort" Can't save message listing."
      repeat
   r@ write-file
   r> close-file 2drop
 ;

: encryption-key ( - adr count  )    s" 4ePost"  ;

: encrypt/decrypt$ ( orginal$|encrypted$ count - encrypted$|orginal$ count )
   encryption-key third 0 locals| key-char cnt max-key |
   -rot 0
       do   i  max-key /mod drop third + c@ to key-char  \ key-char
            dup i + c@                                   \ char to encript/decript
            dup i 1+ key-char + 8 /mod drop tuck test-bit not swap bit! \ encript/decript
            i tmp$ + c!                                  \ store it
       loop
    2drop tmp$ cnt
   ;


: pass  ( - )
   s" PASS " .send pop3-socket  WriteSocket drop
   &account r>record dup acc-password  swap acc-cnt-password c@
   encrypt/decrypt$  pop3-socket WriteSocketLine drop
 ;



: pop3-user  ( - )
  &account r>record acc-cluster-name /line 2dup cont_@_? nip -
  s" USER "  merge$ "wP rPt rPt  ;

: mail-authorize ( - )
   s" HELLO"      "wP rPt
   pop3-user
   pass
   rP-ok? not abort" Invalid password or account."
 ;

: mail-transactions ( - )
   transaction-state?
     if     mail-list  get-mail +to #new-mail
     else   true abort" Transaction state error"
     then
   s" QUIT"  "wP rPt
   cr ." End mail"
  ;

: +date$+sp ( - ) date$ +place date$ +space>$ ;

\ Wed, 24 Jun 2002 19:15:09 +0200
: gmt-days  ( day - adr cnt )
        case
           0 of s" Sun" endof
           1 of s" Mon" endof
           2 of s" Tue" endof
           3 of s" Wed" endof
           4 of s" Thu" endof
           5 of s" Fri" endof
           6 of s" Sat" endof
                abort" A bad day."
        endcase
 ;

: gmt-months  ( month - adr cnt )
        case
           1 of s" Jan" endof
           2 of s" Feb" endof
           3 of s" Mar" endof
           4 of s" Apr" endof
           5 of s" May" endof
           6 of s" Jun" endof
           7 of s" Jul" endof
           8 of s" Aug" endof
           9 of s" Sep" endof
          10 of s" Oct" endof
          11 of s" Nov" endof
          12 of s" Dec" endof
                abort" A bad month."
        endcase
 ;

10 string: gmt-zone$ gmt-zone$ string" +0200"

: >gmt" ( time_structure -- )
                dup  4 + w@ gmt-days  date$  place s" , " date$ +place
                dup  6 + w@ 0 (d.)      +date$+sp
                dup  2 + w@ gmt-months  +date$+sp
                dup w@     0 (d.)    +date$+sp
                dup  8 + w@ 0 (d.)    date$ +place s" :" date$ +place \ hours
                dup 10 + w@ 2 .#"     date$ +place s" :" date$ +place \ minutes
                    12 + w@ 2 .#"    +date$+sp                        \ seconds
                gmt-zone$ count       date$ +place
                date$ count ;

: .gmt ( -- ) get-local-time time-buf >gmt" type ;

: start-smtp    ( - )           s" HELO"   "wSr ;   \ 220

: merge-rcpt"   ( adr count - pad count ) s" RCPT TO: " merge$ ;
: nntp-rcpt     ( adr count - ) merge-rcpt" "wN ;
: smtp-rcpt     ( adr count - ) merge-rcpt" "wS ;  \ 250

: gmt-date"     ( - pad count ) get-local-time time-buf >gmt" s" Date: " merge$ ;
: smtp-date     ( - )           gmt-date"  "wS ;
: nntp-date     ( - )           gmt-date"  "wN ;

: data"         ( - adr count )    s" DATA " ;
: smtp-data     ( adr count??? - ) data" "wS ;      \ 250
: nntp-data     ( adr count??? - ) data" "wN ;

: 4ePost-footer" ( adr count - tmp$ count ) ( f: time - )
   r/w open-file abort" Can't find file for statistics."
   dup file-size>s swap close-file drop
   0 tmp$ !
   s" =="  +tmp$ crlf$ count +tmp$
   s" 4ePost: " +tmp$ s>d (UD,.) +tmp$
   s"  bytes in mail. Elapsed time to buffer: " +tmp$
   pad fvalue-to-string pad count +tmp$
   s"  sec." +tmp$
   tmp$ count
 ;

: eot-octed"    ( - adr count )  eot-octet 2+ 3 ;
: smtp-end-msg  ( - )            eot-octed" "wSr ; \ 354
: nntp-end-msg  ( - adr count )  eot-octed" "wNr_ ;

: x-mailer"     ( - adr count ) s" X-Mailer: 4ePost compiled with Win32Forth" ;
: smtp-x-mailer ( - )           x-mailer" "wS ;
: nntp-x-mailer ( - )           x-mailer" "wN ;

: get-subject$  ( rec-adr - pad count )
    dup msg-Subject swap cnt-Subject c@ s" Subject: " merge$ \ cr 2dup dump
 ;

: smtp-subject    ( rec-adr - )    get-subject$ "wS  ;
: nntp-subject    ( rec-adr - )    get-subject$ "wN  ;

: get-from$ ( - adr count )
   tmp$ off  &account r>record  dup>r
   acc-name r@ acc-cnt-name c@ \ ascii "  ascii " fold
   tmp$ place  s"  " +tmp$
   r@
   acc-cluster-name  r>  acc-cnt-cluster-name c@ ascii < ascii > fold +tmp$
   tmp$ count
 ;

: nntp-newsgroups ( adr count - ) s" Newsgroups: " merge$ "wN ;
: smtp-from       ( - )   get-from$  s" MAIL FROM:" merge$ "wSr ; \ 250

: nntp-from       ( - )
   tmp$ off
   &account r>record >r
   r@ acc-name r@ acc-cnt-name c@ \ ascii (  ascii ) fold
   +tmp$ s"  " +tmp$
   r@ acc-cluster-name r> acc-cnt-cluster-name c@ ascii < ascii > fold
   +tmp$
   tmp$ count s" From: " merge$ "wN
 ;

: nntp-help  ( - )           s" HELP " "wNr_  ;
: nntp-post  ( rec-adr - )   s" POST " "wNr_  ;
: to"        ( adr count - ) s" To: " merge$  ;
: smtp-to    ( - )           to" "wS          ;

: sender" ( - adr count ) \ Is used to fill the from field when a msg is received
   s" From: " tmp$ place
   &account r>record dup>r acc-name r@ acc-cnt-name c@ ascii "  ascii " fold +tmp$
   s"  " +tmp$
   r@ acc-cluster-name r> acc-cnt-cluster-name c@
   ascii < ascii > fold +tmp$  tmp$ count
 ;

: smtp-sender ( - ) sender" "wS ;
: nntp-sender ( - )
   s" Sender: " tmp$ place
   &account r>record dup>r acc-name r@ acc-cnt-name c@ ascii "  ascii " fold +tmp$
   s"  " +tmp$
   r@ acc-cluster-name r> acc-cnt-cluster-name c@
   ascii < ascii > fold +tmp$  tmp$ count "wN
 ;

: .sending ( - )  cr ." Sending the body." ;

: map-file-to-send ( count adr - vadr cnt )
   out-hndl open-map-file abort" Body of message not found."
   out-hndl map-hndl>vadr out-hndl >hfileLength @
 ;

: smtp-send-bdy ( count adr - f: time )
   map-file-to-send   .sending
   muTime
   smtp-socket WriteSocketLine abort" Can't write to the smtp-server."
   (.muTime
 ;

: nntp-send-bdy ( count adr - f: time )
   map-file-to-send   .sending
   muTime
   nntp-socket WriteSocketLine abort" Can't write to the nntp-server."
   (.muTime
 ;

0 value last-selected-rec
0 value hItem-last-selected

: last-selected>record ( last-selected - r>record ) last-selected-rec r>record ;

: email-adres?   ( record - email-adres|news-group cnt msg-flag )
    dup dup msg-flag c@ cluster- <
    over &R-cluster c@
    last-selected>record record>r 0= and
    abort" You can not send a message from the spambox.\nUse the adresbook or select a cluster."
       if   &R-cluster @ r>record
       then
    dup msg-flag c@ msg-grp- <=
       if   nip dup acc-cluster-name over acc-cnt-cluster-name c@ rot
            msg-flag c@ msg-grp- <>
       else drop dup msg-From over cnt-From c@ true
       then
 ;

: fold-email-adres ( adr cnt - adr2 cnt2 )
    s" *<*>" 2over w-search
      if     2drop
      else   2drop ascii < ascii > fold
      then
 ;

: pad$_ok? ( - pad count flag )     pad +null pad count dup 2 /line between  ;
: init-dlg ( adr count - pad base ) pad place pad msgtree-base ;

: show-entry ( - )
   last-selected>record dup message? not abort" Select a message."
   cr dup>r  ." Record adres: " .
        ." Id msg: " r@ record-ID   /msg-ID                    type-cr
        r@ msg-From      r@ cnt-From c@     ." From: "         type-cr
        r@ msg-To        r@ cnt-To c@       ." To: "           type-cr
        r@ include-subject-flag  r@ cnt-Subject c@  ." Subj: " type-cr
        r@ msg-Reply-To  r@ cnt-Reply-To c@ ." Reply to: "     type-cr
        ." Date: " r@ msg-Date      r@ cnt-Date c@             type-cr
        r> ." Cluster: " &R-cluster @ r>record
           dup acc-cluster-name swap acc-cnt-cluster-name c@   type-cr
 ;

NewEditDialog subjectDlg "Enter the subject" "The subject is:" "Ok" "Cancel" ""

: ask-subject ( - adr cnt )
   last-selected>record dup msg-flag c@ cluster- <
        if    s" Re: " tmp$ place dup msg-subject-flag
               swap cnt-Subject c@ over c@ 0 >
                  if     4 -
                  then
               swap 4 + swap
               +tmp$ tmp$ count
        else  drop s" Your subject."
        then
    init-dlg  Start: subjectDlg >r
    pad$_ok? /line min 0= r>  and
       if    true abort" Missing subject, message aborted."
       then
 ;

: check-msg-to-send ( - flag )
   decimal
   new-msg-buffer c@  0= new-msg-buffer maxstring + c@ 0= or
       abort" Prepare a reply or new message first and save it."
   new-msg-buffer tmp-msg-id count r/w open-file  
     abort" Can't find the message to send. Save it first."
   dup file-size>s  swap close-file drop
   s" Message to: " receive-buffer place
   new-msg-buffer count receive-buffer +place
   receive-buffer +null
   receive-buffer count
   s" Close your message before you send it to the Internet.\nThe size is: "
   tmp$ place rot n>+tmp$
   s" bytes. Would you like to put it in your outbox ?" +tmp$ tmp$ count y/n-box
   IDYES =
 ;

defer messages>treeview  ( #start #end -- ) ' 2drop is messages>treeview
defer open-cluster-in-treeview  ( &cluster -- ) ' drop is open-cluster-in-treeview


: msg>outbox ( - )  \  prepare-message before msg>outbox
   check-msg-to-send
     if   extend-database last-record >r
          new-msg-buffer count dup r@ cnt-To c!  r@ msg-To swap cmove
          new-msg-buffer maxstring + count dup r@ cnt-Subject c! r@ msg-Subject swap cmove
          new-msg-buffer tmp-msg- c@ r@ msg-protocol c!
          new-msg-buffer tmp-msg-id count 4 - r@ record-ID swap cmove
          get-local-time time-buf >gmt" dup r@ cnt-Date c! r@ msg-Date swap cmove
          1 activate-bit r@ msg-Status !
          &outbox r>record next-Outbox @    r@ next-Outbox !
          r@ record>r &outbox r>record next-Outbox !
          &outbox  r> &R-cluster !
          new-msg-buffer off
          sync-pointers
          database-hdr-hndl #records-in-database dup 1- messages>treeview
          &outbox open-cluster-in-treeview drop
     then
  ;

: send-smtp-message ( rec-adr - )
   start-smtp smtp-from
   dup msg-To over cnt-To c@ 2dup
    extract-email-adres drop ascii < ascii > fold
    tmp$ place tmp$ count smtp-rcpt  \  Email adres receiver
   smtp-data   smtp-date   smtp-sender  smtp-to
   dup smtp-subject
   smtp-x-mailer
   record-ID /msg-ID tmp$ place s" .bdy" +tmp$ tmp$ dup +null count 2dup
   smtp-send-bdy
   4ePost-footer" "wS
   smtp-end-msg
 ;

: quit-news  ( - )   s" QUIT " "wNr 2drop  ;

: nntp-error?  ( msg count - )
   over c@ ascii 4 >   \  =
    if   buffer place     quit-news nntp-socket CloseSocket drop
         buffer count 2 - s" Fatal error from news-server:" 2swap
         [ MB_OK MB_ICONSTOP or MB_TASKMODAL or ] literal msgbox
         abort
    else 2drop
    then
 ;

: group ( group count - msg count )
   s" GROUP " pad place pad +place pad count "wNr 2dup nntp-error?
 ;

: fold-to$ ( rec-adr - tmp$-folded count )
    dup msg-To over cnt-To c@ 2dup
    extract-email-adres drop ascii < ascii > fold
    tmp$ place tmp$ count
 ;

: open-nntp-server ( - )
   cr cr .gmt
   cr s" News-server: " type nntp-server$ dup +null 1+ zGetHostIP drop NtoA type
   init-nntp to nntp-socket
   rNt nntp-error?
 ;

defer refresh-treeview ( - ) ' noop is refresh-treeview

: del-all  ( rel-rec - )
    database-hdr-hndl #records-in-database  0
       ?do   i  n>record dup message?
             if   dup  &R-cluster @ 2 pick =
                     if   dup &R-cluster off  0 swap  msg-Status c!
                     else drop
                     then
             else drop
             then
       loop
    drop
 ;


: delete-all-msg-from-cluster ( rec-adr - )
   dup>r msg-flag c@ msg-list- account- between 
     if    r>  record>r  del-all
     else  r>drop  true abort" Deletion aborted.\nSelect a cluster to delete."
     then
 ;

: delete-warning ( - flag )
   s" Warning !" s" Deletion in progress. Continue?"
   y/n-box IDYES <>
 ;

: mdel-all ( - )
   delete-warning
     if exit
     then
   last-selected>record
   dup   delete-all-msg-from-cluster   0 swap grp-last-read !
   refresh-treeview
 ;

: delete-item  ( rec-adr - flag )
   >r r@ msg-flag c@ dup adresbook- >
        if    delete-warning
                   if    r>drop false exit
                   then
               r@ delete-all-msg-from-cluster
               deleted-cluster- r@ msg-flag c!
               refresh-treeview
        else   0 r@ msg-Status c!
               r@ msg-flag c@ ascii 9 min r@ msg-flag c!
        then
   0 r> &R-cluster ! true
 ;

: send-nntp-message ( rec-adr - )
   open-nntp-server
   s" MODE READER"  "wNr ( rNt) type
   dup>r msg-To r@ cnt-To c@  2dup group drop c@ ascii 4 = abort" Can't select group."
   nntp-post    2dup nntp-newsgroups
   nntp-sender    nntp-from    nntp-date    r@ nntp-subject
   s" Content-Type: text/plain; charset=ISO-8859-1"  "wN
   s" Content-Transfer-Encoding: 8bit"  "wN
   s" X-Newsreader: 4ePost v1.0 compiled with Win32Forth"  "wN
   crlf$ count "wN
   r@ record-ID /msg-ID tmp$ place s" .bdy" +tmp$ tmp$ dup +null count 2dup
   nntp-send-bdy
   4ePost-footer" "wN
   nntp-end-msg
   r> delete-item drop
   quit-news
 ;

: send-messages  ( - )
   unmap-msg-base map-msg-base
   &outbox r>record next-Outbox @ dup 0= if drop exit then
   r>record
      begin   dup msg-Status @
                 if   dup msg-protocol c@
                          if    dup send-nntp-message
                          else  dup send-smtp-message
                          then  dup delete-item drop
                 then
              next-Outbox @
              dup r>record swap
              0=
      until
   drop
 ;

 20 string: $Forth-editor $Forth-editor string" WinView"
\ 20 string: $Forth-editor $Forth-editor string" WinEd"

: start-editor  ( - )
   cur-line off   0 pad !
   s" Directory" GetSetting +pad$
   s" \" +pad$  $Forth-editor count +pad$ s" .exe" +pad$
   pad +null pad $exec drop
 ;

: check-editor  ( - )
   editor-present? not
     if  start-editor
     then
 ;

: generate-msg
   ask-subject   new-msg-buffer tmp-subj place
   decimal gen-msg-id s" .bdy" msg-id$  +place
   msg-id$ count new-msg-buffer tmp-msg-id place
   0  current-dir$ count pad place
   s" \" +pad$
   msg-id$ count +pad$ pad
   +null pad $edit
 ;

: prepare-smtp-message ( adr cnt - )
    fold-email-adres new-msg-buffer place generate-msg
    0 new-msg-buffer tmp-msg- c!
  ;

: prepare-nntp-message ( adr cnt - )
   new-msg-buffer place generate-msg
   msg-grp- new-msg-buffer tmp-msg- c!
 ;


: prepare-message ( - )
   0 new-msg-buffer maxstring + ! 0 new-msg-buffer !
   last-selected>record email-adres?
     if    prepare-smtp-message
     else  prepare-nntp-message
     then
 ;


: bdy>pad ( &rec flag - ) \  flag 0 displays header
   pad off >r record-ID  /msg-ID pad place r>
      if     s" .bdy"
      else   s" .hdr"
      then
   pad +place pad +null
 ;

: reply-to-mail ( - )
   &msg-viewing dup 0= abort" Select a message to reply to."
 ;

: stat  ( n - )
   s" STAT " tmp$ place n>+tmp$ tmp$ count  "wNr nntp-error?
 ;

: create-news-header ( - Hndl )
    msg-header-name count r/w create-file abort" Can't create file."
 ;

: create-header/bdy ( - Hndl )
    count r/w create-file abort" Can't create file."
 ;

: data>file ( Hndl-  )
    true 0 locals| size first-line? Hndl |
      begin   receive-buffer off receive-buffer rN r-end? not
      while   Hndl write-file abort" Can't save data."
      repeat
   len-last-octet -
   dup +to size Hndl write-file drop
   Hndl close-file drop
 ;


: save-2nd-line+data ( buffer size hndl - ) \ first-line = error or response
   >r cut-line 2dup r@ write-file abort" Can't save data."
    r-end?
       if    r> close-file drop
       else  r> data>file
       then
    2drop
 ;

: body ( - )
  s" BODY" "wNr 2dup nntp-error?
  s" .bdy" file-name$ create-header/bdy
  save-2nd-line+data
;

: head  ( - )
   s" HEAD" "wNr 2dup nntp-error?
   create-news-header save-2nd-line+data
 ;


: get-news-msg ( group which - group )
   dup to id-last-read-news dup stat  gen-msg-id  head  body
   hdr>database 1 +to #new-mail
   cluster-news
   database-hdr-hndl close-map-file drop
   hdr-hndl close-map-file drop
 ;

: get-news ( group #start #end - group )
    swap
       do   i get-news-msg
       loop
 ;

\ 211 426 11193 11623 comp.lang.forth

: close-nntp-server ( - )  nntp-socket CloseSocket drop  ;

NewEditDialog maxDwnDlg "Maximum news messages to download" "Maximum number for all groups:" "Ok" "Cancel" ""

99 value #max-download-news

: ask-maxDwn  ( - )
   #max-download-news n>tmp$ tmp$ count init-dlg  Start: maxDwnDlg drop
   pad count number? not abort" Maximum news to download not changed."
   d>s to #max-download-news
;

: new-news ( group count -  )
   0 0 0 locals| #last #first #max |
   group 2>r
        2r@ 3 bl #number-line> not abort" Last #messages not available." to #last
        2r@ 2 bl #number-line> not abort" First #messages not available." to #first
        2r> 1 bl #number-line> not abort" #messages not available." to #max
        #last id-last-read-news >
            if    #last #max #max-download-news min -
                  id-last-read-news max 1+
                  #last 1+ get-news
            else  cr ." No news in this group" cr
            then
 ;


: start-news ( - )
   &last-cluster map-msg-base
       begin   dup to &r>group dup r>record dup msg-flag c@ msg-grp- =
                   if    dup grp-last-read @ to id-last-read-news
                         dup acc-cluster-name swap acc-cnt-cluster-name c@ newsgroup$ place
                         unmap-msg-base newsgroup$ count new-news
                         map-msg-base
                   else  drop
                   then
               r>record next-group @ dup 0=
       until
   drop
 ;

250 string: inifile$

: put-current-path>inifile  ( - )
   inifile$  250 erase current-dir$ inifile$ $copy
 ;

: inifile ( - adres )
   put-current-path>inifile s" \4ePost.ini" inifile$ +place
   inifile$ z" 4ePost" z" ini-file" profile>$ nip nip
   count s" 4ePost.ini" compare 0<>            \ inifile$ <> 4ePost.ini
        If   profile$ 2+ c@ ascii : =
                if    profile$ inifile$ $copy \ string includes full path eg: c:\alert_nl.ini
                else  profile$ put-current-path>inifile   \ Inifile in program directory
                      s" \"  inifile$  +place count inifile$ +place
                then
        then
    inifile$
   ;

create &InfoRect  4 cells allot    ( - &InfoRect )
&InfoRect 4 cells erase
&InfoRect constant window_x
&InfoRect 1 cells+ constant window_y
&InfoRect 2 cells+ constant _width
&InfoRect 3 cells+ constant _height

300 _width  !
400 _height !

: save-defaults-to-ini
   inifile$
      z" 4ePost"  z" sizeh"   _height @       s>profile
                  z" sizew"   _width  @       s>profile
                  z" posx"   window_x @ 0 max s>profile
                  z" posy"   window_y @ 0 max s>profile
                  z" spam"   mSpam-   @       s>profile
                  z" #max-dwn-news"   #max-download-news s>profile
                  z" autodisconnect"   mAutodisconnect-  @  s>profile
   2drop
 ;

: save-cluster-to-ini
   inifile$
      z" 4ePost"  z" &last-cluster"   &last-cluster  s>profile
                  z" &account"        &account       s>profile
                  z" &outbox"         &outbox        s>profile
      2drop
 ;

NewEditDialog accountDlg "Generating main account" "Your name is:" "Ok" "Cancel" ""
NewEditDialog emailDlg   "Email adress for account" "Your email adress is:" "Ok" "Cancel" ""
NewEditDialog passwordDlg "The password will be encrypted in the database" "Enter your password:" "Ok" "Cancel"  ""
NewEditDialog spamDlg "Enter your spam string" "* are allowed when substrings are unique:" "Ok" "Cancel"  ""

NewEditDialog pop3Dlg   "Pop3 server for incomming mail" "The pop3-server is: " "Ok" "Cancel" ""
NewEditDialog smtpDlg   "Smtp server for out going mail" "The smtp-server is: " "Ok" "Cancel" ""
NewEditDialog nntpDlg   "Nntp server for news"           "The nntp-server is: " "Ok" "Cancel" ""
NewEditDialog zoneDlg   "Your timezone"   "The format is +HHMM or -HHMM: " "Ok" "Cancel" ""

1 constant outbox
0 constant spambox

: init-spambox ( - )
    spambox >record dup>r record-size spam- fill
    spambox r@ next-group !
    s" Spam" tuck r@ acc-cluster-name swap cmove
    r> acc-cnt-cluster-name c!
;

: init-outbox ( - )
    outbox >record dup>r record-size 0 fill \ outbox
    msg-list- r@ msg-flag c!
    s" outbox" tuck r@ acc-cluster-name dup>r swap cmove
    r> over 0term
    r@ acc-cnt-cluster-name c!
    r@ record>r to &outbox
    spambox r> next-group !
 ;

: ask_password  ( - encrypted-password$ count )
    s"   2 till 30 numbers or letters  " init-dlg  Start: passwordDlg >r
    pad$_ok? 0= r>  and
       if    2drop true abort" Invalid password."  then
    encrypt/decrypt$
 ;
       
: 4ePost-section   ( - inifile 4ePost-section )  inifile$   z" 4ePost" ;

: $>4ePost-profile ( profile$ $ - )
   4ePost-section  2swap dup +null $>profile 2drop ;

: ask_timezone  ( - pop3-server count )
    gmt-zone$ count init-dlg  Start: zoneDlg >r
    pad$_ok? 0= r>  and
       if    2drop true abort"  Invalid timezone.\n Try something like +0700"  then
       5 min gmt-zone$ place
    z" gmt-zone"  gmt-zone$  $>4ePost-profile
 ;

: ask_pop3server  ( - pop3-server count )
    pop3-server$ count init-dlg  Start: pop3Dlg >r
    pad$_ok? 0= r>  and
       if    2drop true abort" Invalid popserver."  then
       pop3-server$ place
       z" pop3server"  pop3-server$  $>4ePost-profile
 ;

: ask_smtp3server  ( - smtp-server count )
   smtp-server$ count init-dlg  Start: smtpDlg >r
    pad$_ok? 0= r>  and
       if    2drop true abort" Invalid smtp-server."  then
    smtp-server$ place
    z" smtp-server"  smtp-server$ $>4ePost-profile
 ;

: ask_nntpserver  ( - nntp-server count )
   nntp-server$ count init-dlg  Start: nntpDlg >r
    pad$_ok? 0= r>  and
       if    2drop true abort" Invalid nntp-server."  then
    nntp-server$ place
    z" nntp-server"  nntp-server$ $>4ePost-profile
 ;



: ask_spam$  ( - encrypted-password$ count )
    &account r>record acc-spam$ count init-dlg  Start: spamDlg >r
    pad$_ok? 0= r>  and
       if    2drop true abort" Invalid spam string."  then
 ;

: change_password  ( - )
    ask_password >r &account r>record tuck
    acc-password r@ cmove
    r> swap acc-cnt-password c!
 ;

: change_spam$  ( - )  ask_spam$  &account r>record acc-spam$ place  ;

: account>database  ( - )
    database-hdr-hndl size-database
    unmap-msg-base
    buffer record-size erase
    s"  " init-dlg Start: accountDlg >r \ 1 = ok
    pad$_ok? 0= r> and
       if    2drop true abort" Invalid account name."   then
    >r buffer acc-name r@ 1+ cmove
    r> buffer acc-cnt-name c!
    ask_password
    >r buffer acc-password r@ 1+ cmove
    r> buffer acc-cnt-password c!
    s" EG: john@wxs.com"  init-dlg  Start: emailDlg >r
    pad$_ok? 0= r> and
       if    2drop true abort" Invallid email adres."  then
    s" *" buffer acc-spam$ place   2dup buffer acc-spam$ +place
    >r buffer acc-cluster-name r@ cmove
    r> buffer acc-cnt-cluster-name c!
    account- buffer msg-flag c!
      record-size > abort" Not allowed. Empty your mail folder."
    extend-database  extend-database extend-database
    last-record
    init-spambox     init-outbox
    dup record>r dup to &account to &last-cluster  \ main account
    outbox records buffer next-group !   \  main account points to the outbox
    buffer swap record-size cmove        \  move the data to de database
    save-cluster-to-ini                  \  save info in an inifile
    sync-pointers                        \  add pointers
    ask_timezone
    ask_pop3server
    ask_smtp3server
    ask_nntpserver
 ;

NewEditDialog AddNewsDlg "Subscribe to newsgroup" "The newsgroup is:" "Ok" "Cancel"  ""

: cluster>database  ( grp- adr cnt - )
   unmap-msg-base
   extend-database 2dup last-record
   dup>r acc-cluster-name swap 1+ cmove       \ email-adr
   dup r@ acc-cnt-cluster-name c!

   2dup r@ acc-name swap 1+ cmove    \     sender
   r@ acc-cnt-name c! drop

   r@ msg-flag c!
   &last-cluster r@ next-group !
   0 r@ grp-last-read !
   r> record>r to &last-cluster
   save-cluster-to-ini
   sync-pointers
 ;

: group>database
   msg-grp- s" comp.lang.forth"  init-dlg  Start: AddNewsDlg >r
   pad$_ok? r> 0=   and
       if    2drop exit   then
   cluster>database
 ;

NewEditDialog AddMsgGrpDlg "Add Message group" "The message group is:" "Ok" "Cancel"  ""

: MsgGrp>database
    msg-list- s" win32forth@yahoogroups.com"  init-dlg  Start: AddMsgGrpDlg >r
    pad$_ok? r> 0=   and
       if    2drop exit   then
   cluster>database
    ascii *  last-record
    acc-cluster-* c!
 ;

NewEditDialog AddNameDlg  "Add adress"   "The name of the person or organisation is:" "Ok" "Cancel"  ""
NewEditDialog AddEmailDlg "Email adress" "The email adress is:" "Ok" "Cancel"  ""

: Person>database
    tmp$ /tmp erase
    adresbook- s"  EG: John Larson"  init-dlg  Start: AddNameDlg drop
    pad$_ok?
       if     ascii "  ascii " fold tmp$ place s"  " +tmp$
       else   0 tmp$ ! 2drop
       then
     s" EG: john@wxs.com" init-dlg  Start: AddEmailDlg >r
    pad$_ok? r> 0=   and
       if    2drop exit   then
    ascii <  ascii > fold +tmp$
    tmp$ count cluster>database
   last-record  dup record>r swap &R-cluster !
  ;

: load-startup-from-ini
    inifile dup
    z" 4ePost" z" sizew" profile>s screen-size >r  min  _width !
               z" sizeh" profile>s r> min  _height !
               z" posx"  profile>s 0 max screen-size >r  min window_x !
               z" posy"  profile>s 0 max r>  min window_y !
               z" &last-cluster"  profile>s to &last-cluster
               z" &account"       profile>s to &account
               z" &outbox"        profile>s to &outbox
               z" #max-dwn-news"  profile>s to #max-download-news
               z" pop3server"     profile>$ pop3-server$ $copy
               z" smtp-server"    profile>$ smtp-server$ $copy
               z" nntp-server"    profile>$ nntp-server$ $copy
               z" gmt-zone"       profile>$ gmt-zone$    $copy
               z" spam"           profile>s mSpam- !
               z" autodisconnect" profile>s mAutodisconnect- !

     2drop
     count "path-only" pad place pad +null pad 1+ $current-dir! drop
  ;

FileSaveDialog maildir-dlg "Select or create a directory for the database and mail : " "Files (*.*)"

: setup-mail-dir
   current-dir$ tmp$ $copy
   s" Open your mail directory" maildir-dlg place
   msgtree-base start: maildir-dlg  count "path-only" 2>r
   s" \4ePost.ini" tmp$  +place tmp$ dup +null z" 4ePost"   z" ini-file"
   2r> pad place s" \4ePost.ini" +pad$  pad $>profile
   pad inifile$ $copy save-defaults-to-ini
   0 to &last-cluster 0 to &account save-cluster-to-ini
   2drop
 ;

\s

