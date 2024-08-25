\ HTTP Error Codes
\ Thomas Dixon

: err-code ( n -- err ) \ translates some of the many many error codes
  case
    200 of s" OK" exit endof
    201 of s" Created" exit endof
    202 of s" Accepted" exit endof
    203 of s" Non-Authoritative Information" exit endof
    204 of s" No Content" exit endof
    205 of s" Reset Content" exit endof
    206 of s" Partial Content" exit endof
    300 of s" Multiple Choices" exit endof
    301 of s" Moved Permanently" exit endof
    302 of s" Found" exit endof
    303 of s" See Other" exit endof
    304 of s" Not Modified" exit endof
    305 of s" Use Proxy" exit endof
    306 of s" (Unused)" exit endof
    307 of s" Temporary Redirect" exit endof
    400 of s" Bad Request" exit endof
    401 of s" Unauthorized" exit endof
    402 of s" Payment Required" exit endof
    403 of s" Forbidden" exit endof
    404 of s" Not Found" exit endof
    405 of s" Method Not Allowed" exit endof
    406 of s" Not Acceptable" exit endof
    407 of s" Proxy Authentication Required" exit endof
    408 of s" Request Timeout" exit endof
    409 of s" Conflict" exit endof
    410 of s" Gone" exit endof
    411 of s" Length Required" exit endof
    412 of s" Precondition Failed" exit endof
    413 of s" Request Entity Too Large" exit endof
    414 of s" Request-URI Too Long" exit endof
    415 of s" Unsupported Media Type" exit endof
    416 of s" Requested Range Not Satisfiable" exit endof
    417 of s" Expectation Failed" exit endof
    500 of s" Internal Sever Error" exit endof
    501 of s" Not Implemented" exit endof
    502 of s" Bad Gateway" exit endof
    503 of s" Service Unavailable" exit endof
    504 of s" Gateway Timeout" exit endof
    505 of s" HTTP Version Not Supported" exit endof
  endcase s" Error" ;