\ HTTP Server Component - Files
\ Tom Dixon

\ URL points to a file

create webpath 256 allot
s" c:\cprparse\" webpath place

: chreplace ( newch oldch str len -- )
  over + swap ?do dup i c@ = if over i c! then loop 2drop ;

( with httpreq )
  : doURLFile ( -- flag )  
    webpath count hbuf place
    url [char] / scan [char] / skip 
    2dup [char] ? scan nip - hbuf +place
    [char] \ [char] / hbuf count chreplace
    hbuf count + 1- c@ [char] \ = if s" \index.html" hbuf +place then
    hbuf count r/o open-file 
    if drop false exit then 
    http-file ! hbuf count get-mediatype
    s" Content-Type: " http-type http-type http-cr http-cr
    true ;
    
  doURL doURLFile http-done
( endwith )
