anew shell_rel.f       \   October 6th, 2002 - 19:30

\ This version saves its pointers as relative offsets in an index file.
\ The database and the pointers must be mapped.

\ Notes:
\ Minimum file size of the database must be 1 byte
\ When the database is resized, the database has to be re-mapped.
\ File-status not right when a file is mapped

\ A test of 804 headers:
\ Sort time:   15.3740E-3 sec
\ Resort time:  5.41661E-3 sec using the same key


    23 VALUE record-size
     7 VALUE key-len
     5 VALUE key-start

   112 VALUE #records
     0 VALUE aptrs  \ an array of cells containing pointers to records
     0 VALUE records-pointer

: n>aptr   ( n -- a )   S" aptrs +cells                 " EVALUATE ; IMMEDIATE
: r>record ( n -- a )   S" records-pointer ( CHARS) +   " EVALUATE ; IMMEDIATE
: record>r ( a -- n )   S" records-pointer ( CHARS) -   " EVALUATE ; IMMEDIATE
: n>record ( n -- a )   S" n>aptr @ r>record            " EVALUATE ; IMMEDIATE
: >key     ( a -- a' )  S" key-start +                  " EVALUATE ; IMMEDIATE
: n>key    ( n -- a )   S" n>record >key                " EVALUATE ; IMMEDIATE
: records  ( n -- ra )  S" record-size *                " EVALUATE ; IMMEDIATE
: >record  ( n -- a )   S" records r>record             " EVALUATE ; IMMEDIATE
: xchange  ( a1 a2 -- ) S" dup>r @ over @ r> ! swap !   " EVALUATE ; IMMEDIATE

: shell-rel  ( aptrs #records -- )
   1 BEGIN 3 * 1+ 2DUP 1+ U< UNTIL  \ gap*3
   BEGIN 3 / DUP WHILE
     2DUP - >R DUP CELLS R>
     0 DO DUP 4 PICK DUP I CELLS +
\        DO DUP I + DUP @ >KEY  I TUCK @ >KEY
         DO DUP I + DUP @ r>record >KEY  I TUCK @ r>record >KEY
           KEY-LEN TUCK COMPARE 0< 0= IF 2DROP LEAVE THEN
           xchange
           DUP NEGATE
        +LOOP DROP
     LOOP DROP
   REPEAT 2DROP DROP ;

: build-ptrs ( #records -- )
   to #records #records 1+ CELLS ALLOCATE THROW  TO aptrs
   #records 1+ 0 DO
\      records-pointer I records ( CHARS ) + aptrs I CELLS + !
   LOOP ;

: free-ptrs     ( -- )  aptrs FREE THROW ;
: free-records  ( -- )  records-pointer FREE THROW ;

: check-keys  ( -- )
    space #records 1-
    0 DO  I n>key I 1+ n>key  key-len TUCK COMPARE 0>
      IF ." UN" LEAVE THEN LOOP ." sorted " ;

: create-file-ptrs ( name -- )
   COUNT R/W CREATE-FILE ABORT" Can't create index file."
   CLOSE-FILE THROW
 ;

: open-file-ptrs ( name -- hndl )
   COUNT R/W OPEN-FILE ABORT" Can't open index file."
 ;

: extend-file ( size hndl - )
    \ >r cells r@
    dup>r
    file-size drop d>s +
    s>d r@ resize-file abort" Can't extend file."
    r> close-file  drop
 ;

: #records-in-database ( m_hndl - #records )  >hfileLength @ record-size /   ;

: add-file-ptrs ( #start #end - )
   dup to #records swap 
      DO  I records aptrs I CELLS + !
      LOOP
 ;

: build-file-ptrs ( #records -- ) 0 swap  add-file-ptrs ;

\s

