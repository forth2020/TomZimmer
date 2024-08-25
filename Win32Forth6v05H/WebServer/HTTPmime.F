\ Mime Types
\ Thomas Dixon

: get-mediatype ( addr len -- addr len ) \ media type MIME translation
  [char] . scan [char] . skip drop 3
  2dup s" htm" ISTR= if 2drop s" text/html" exit then
  2dup s" txt" ISTR= if 2drop s" text/plain" exit then
  2dup s" jpg" ISTR= if 2drop s" image/jpeg" exit then
  2dup s" gif" ISTR= if 2drop s" image/gif" exit then
  2dup s" bmp" ISTR= if 2drop s" image/x-xbitmap" exit then
  2dup s" doc" ISTR= if 2drop s" application/msword" exit then
  2dup s" rtf" ISTR= if 2drop s" application/rtf" exit then
  2dup s" zip" ISTR= if 2drop s" application/zip" exit then
  2dup s" avi" ISTR= if 2drop s" video/avi" exit then
  2dup s" mpg" ISTR= if 2drop s" video/mpeg" exit then
  2dup s" mov" ISTR= if 2drop s" video/quicktime" exit then
  2dup s" wav" ISTR= if 2drop s" audio/wav" exit then
  2dup s" mal" ISTR= if 2drop s" message/RFC822" exit then
  2dup s" mp3" ISTR= if 2drop s" audio/mp3" exit then
  2drop s" text/plain" ;