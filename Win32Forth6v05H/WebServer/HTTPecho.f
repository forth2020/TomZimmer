\ HTTP Server Component - Echo
\ Tom Dixon

\ This just repeats back the request when the url starts with /echo/
\ Useful for debugging

\ with httpreq
  : doEcho ( -- flag )  
    url [char] / scan [char] / skip 
    2dup [char] / scan nip - s" ECHO" istr=
    if ( with dstr ) 
         http-reply free cr
         s" <HTML><BODY><PRE>" http-reply append
         s" <BOLD>Forth WebServer Echo</BOLD>" http-reply append
         http-request count http-reply append
         s" </PRE></BODY></HTML>" http-reply append
       ( endwith ) 200 http-code ! true
    else false then ;
    
  doURL doEcho http-done
\ endwith
