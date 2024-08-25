\  DFC Differential File Comparison

ANEW --DFC--   DECIMAL

\  2002-01-31  Replaced <spaces>( comment )<eol> 
\              with <spaces>\  comment<eol>
\  2002-01-30  Empty file now rejected.
\  2002-01-29  Single line in file has been allowed.
\  2001-10-29  Dependency on ToolBelt has been removed.

0 [IF] =======================================================
<A HREF="dfc.txt"> <SMALL>TEXT</SMALL></A>
                                        Wil Baden  2002-01-30

A necessary tool for every programmer is a utility to compare
files - particularly source files - to find where and how
they are different.  A prudent programmer will make a copy of
a file before modifying it. In the course of making a series
of small modifications to fix a misbehaving application the
programmer can easily lose track of just what has been done.
Then the file comparison utility can be used to show the
changes.

This utility can be used to show the differences between
released versions as well.

To do the job right is not a trivial task. The obvious
algorithm will sooner than later fail miserably.

The obvious algorithm is to compare lines until a difference
is found, then search forward in both files to find where
they are the same again.

The trick is not to look for differences but to look for the
longest common subsequence - the longest set of lines which
are the same in both files and in the same order with what's
different interspersed. What's left are the differences.

How to do this is the subject of

 Hunt, J. W. and M. D. McIlroy [1976],

 "An algorithm for differential file comparison,"

 Computing Science Technical Report 41, AT&T Bell
 Laboratories, Murray Hill, N.J.

It is based on

 Hunt, J.W and T.G. Szymanski, [1977]

 "A fast algorithm for computing longest common subsequences","

 Comm. ACM , vol. 20 no. 5, pp. 350-353.

In 1976 I implemented this using my own code in Fortran II
for a 8K 16-bit word IBM 1130. It has followed me ever
since, becoming re-incarnated on each new platform in
whatever the language of the moment was.

I even did this in C for Unix because my output format was
more useful to me than that of the Unix tool `diff`.

Some years ago I did it for MacForth.  In the present
incarnation it is Standard Forth.

Differential file comparison is the foundation of version
control systems - SCCS, RCS, SCVS.

The algorithm is essentially brute force.  Read and save one
file, then read records from the other file trying to find
with each record a longer common subsequence than you already
have.

Potentially this could require M * N line comparisons,  where
M and N are the number of lines in each file. In real life
that never happens. The time and memory constraints are still
too extravagant. So a really slick trick is used. Instead of
comparing whole lines, an integer hash value is computed for
each line, and the associated hash values are compared.
Making believe that every unique line has a unique hash
value, we compute a longest common subsequence. Not until we
print do we check whether equal hash values represent
identical lines.

In 25 years of use this has hardly ever happened.  In the
very few times it has, the effect has been negligible. (You
can tell that it has happened when an insertion appears just
before a deletion.)  I haven't seen it since 1988.

Of course you can force it to happen by using a poor hashing
function. However the hashing function doesn't have to be
sophisticated. The one used here has worked fine with 32-bit
or 16-bit arithmetic.

Where I used to work, the Pascal incarnation was used 30 to
200 times a day for ten years, using 16-bit arithmetic.  It
was used even after the company went to Unix.

       How to Use

   old-file-id TO OLDFILE
   new-file-id TO NEWFILE
   DFC

NEWFILE and OLDFILE may be assigned in either order.

You should adapt the file-opening to your environment.

Here is an example from John Peters that works on two versions of WinEdit.

   S" C:\WIN32FOR\WINVIEW.F" R/O OPEN-FILE DROP TO OLDFILE
   S" C:\WIN32FORCG\WINVIEW.F" R/O OPEN-FILE DROP TO NEWFILE
   DFC

The following compares an old source for `DFC` with a revision.

   S" DFC.4TH" R/O OPEN-FILE DROP TO OLDFILE
   S" DFCNEW.4TH" R/O OPEN-FILE DROP TO NEWFILE
   DFC

The output was:

     1 ---- ( DFC - Differential File Comparison.  Wil Baden 1976-1996 )
  ++++    1 ( DFC - Differential File Comparison Using HERE  Wil Baden )
     2    2

    50   37
    51 ---- 6000 CONSTANT   lcs-space       ( The larger the better. )
    52 ---- CREATE          LCS   lcs-space CELLS ALLOT
    53 ----
  ++++   38 0 VALUE lcs-space       0 VALUE LCS
    54   39 0 VALUE oldlines        0 VALUE newlines

[Some lines omitted]

   394  379     ( Differential file comparison. )
  ++++  380     ALIGN HERE TO LCS
  ++++  381     UNUSED 1 CELLS - 1+ ALIGNED 1 CELLS / TO LCS-Space
   395  382     Read-Newerfile   Sort-Hash-Values   Mark-Hash-Classes

   397  384     Build-Candidate-Table   Show-Differences
  ++++  385     oldlines newlines - 2 - LCS @ - . ." deletions, "
  ++++  386         newlines 1- LCS @ - . ." insertions, "
  ++++  387             LCS @ . ." unchanged " CR
   398  388     OLDFILE REWIND   NEWFILE REWIND

This shows that in the old file, DFC.4TH,

*   Line 1 has been replaced.

*   Lines 51 through 53 have been replaced by a single line.

*   A few new lines have been inserted after lines 394 and 397.

The numbers in the first column are the line numbers in the
first file.

The numbers in the second column are the line numbers in the
second file.

The code has been checked for 16-bit and 32-bit cell size.

------------------------------------------------------- [THEN]
0 [IF] =======================================================
                                          Wil Baden 1976-1996
       DFC - Differential File Comparison.

Make a line by line comparison of two files, showing
where and how they are different.

Usage:

   old-file-id TO OLDFILE
   new-file-id TO NEWFILE
   DFC

For a slightly more complete example:

    S" DFC2M.F" R/O OPEN-FILE DROP TO OLDFILE
    S" DFC3M.F" R/O OPEN-FILE DROP TO NEWFILE

This incarnation dynamically assigns workspace in unused
dataspace, giving maximum memory for the data structures.
The maximum size of a record, `DFC-MAXLINE`, can be set
before running.  This lets files with long records be
compared.

------------------------------------------------------- [THEN]
0 [IF] =======================================================

       Common Functions

------------------------------------------------------- [THEN]

\  Comment out definitions that you already have.
\  Exchange  0<>  with  0=  to comment them all out.

TRUE 0<> [IF]

\   : BOUNDS  ( addr len -- addr+len addr )  over + SWAP ;
   : IS-WHITE  ( char -- flag )  33 - 0< ;
\   : NOT  ( x -- flag )  0= ;
\   : OFF  ( addr -- )  FALSE SWAP ! ;
\   : ON   ( addr -- )  TRUE SWAP ! ;
   : \\   ( "...<eof>" -- )  BEGIN  -1 PARSE  2DROP  REFILL 0= UNTIL ;

[THEN]

0 [IF] =======================================================

       Application Values and Variables

OLDFILE          ( -- file-id )
   Value for file-ID of "old" file.  To be set by user.

NEWFILE          ( -- file-id )
   Value for file-ID of "newer" file.  To be set by user.

DFC-MAXLINE      ( -- n )
   Value of maximum size of line for DFC comparisons.
   Initially 255, but may be extended before running `DFC`.

DFC-RIGHT-MARGIN  ( -- n )
   Value of the right-hand margin for automatically wrapping
   output lines.  Set this to a convenient size for you.

DFC-COLLATE      ( -- addr )
   Variable should be set ON to collate instead of showing
   differences.

REWIND           ( file-id -- )
   Rewind file.

------------------------------------------------------- [THEN]

0 VALUE OLDFILE
0 VALUE NEWFILE

255 VALUE DFC-MAXLINE
80 VALUE DFC-RIGHT-MARGIN

VARIABLE DFC-COLLATE   DFC-COLLATE OFF

: REWIND   ( fid -- )
   0 0 ROT REPOSITION-FILE ABORT" Can't REWIND " ;

0 [IF] =======================================================
       Implementation
------------------------------------------------------- [THEN]

VOCABULARY Differential-File-Compare

: INTERFACE  ( -- )
  GET-ORDER
  >R  over SET-CURRENT  R>
  SET-ORDER ;

ALSO Differential-File-Compare

DEFINITIONS

0 VALUE DFC-Space   \  Will be calculated in unused dataspace.

0 VALUE &OLDTEXT
0 VALUE &NEWTEXT
0 VALUE &MATCHINGTEXT
0 VALUE &Cleaned-Oldtext
0 VALUE &Cleaned-Newtext

0 [IF] =======================================================

PADDING                        ( -- n )
   Environmentally dependent value to find free space in unused data
   space. 1000 should be more than enough. If `PAD` is defined as
   usual in data space, `PADDING` can be `PAD HERE -`.  [For Power
   MacForth, 136 is enough.]

LCS
   Cell for each record + 3 * matching-candidates.
   Thus 6000 cells takes care of files up to at least 1200 lines.

Cell-Place                     ( c_addr len addr -- )
   Cell version of `PLACE`.

Cell-Count                     ( addr -- c_addr len )
   Cell version of `COUNT`.

------------------------------------------------------- [THEN]

\ 1000 CONSTANT PADDING

136 CONSTANT PADDING

0 VALUE LCS

: Cell-Place                   ( c_addr len addr -- )
   2dup 2>R  CELL+  SWAP chars MOVE 2R> !
   ;

: Cell-Count                   ( addr -- c_addr len )
   dup CELL+ SWAP @ -TRAILING ;

   VARIABLE Skipping

   : Clean-Line               ( c_addr len -- c_addr len' )
       \  Remove fairy characters.
       Skipping OFF
       >R 0 over R>       ( c_addr len' c_addr len )
       chars BOUNDS ?DO   ( c_addr len')
           I C@ IS-WHITE IF
               Skipping ON
           ELSE
               Skipping @ IF
                   2dup chars + BL SWAP C!
                   1+
                   Skipping OFF
               THEN
               2dup chars +  I C@ SWAP C!
               1+
           THEN
       LOOP ;

   131 CONSTANT HASH-FACTOR

: HASH                         ( c_addr len -- hash-value )
   \  Compute hash value for a string.
   Skipping OFF
   0 ROT ROT chars BOUNDS ?DO    ( hash-value)
       I C@ IS-WHITE IF
           Skipping ON
       ELSE
           Skipping @ IF
               HASH-FACTOR *  BL +
               Skipping OFF
           THEN
           HASH-FACTOR *  I C@ +
       THEN
   LOOP ;

: Read-Text                    ( buffer fileid -- flag )
   >R  dup CELL+  DFC-MAXLINE  R> READ-LINE
       ABORT" Can't read. "
   SWAP ROT ! ;

: Write-Text                   ( buffer --  )
   Cell-Count                    ( addr len)
   BEGIN dup DFC-RIGHT-MARGIN > WHILE
       over DFC-RIGHT-MARGIN              ( . . addr2 len2)

       BEGIN dup WHILE
           2dup chars + C@ IS-WHITE NOT
       WHILE   1-   REPEAT
       ELSE DROP DFC-RIGHT-MARGIN THEN

       TUCK TYPE ( str len len2) 1+ /STRING ( str len)
       CR 10 SPACES
   REPEAT
   TYPE CR ;

0 [IF] =======================================================

NEWLINES    ( -- n )
   1 + lines in newer file.

OLDLINES    ( -- n )
   1 + lines in old file + 1 + lines in newer file.

CAND        ( -- addr )
   Next candidate.

LCS
   In `Find-Longest-Common-Subsequence`, pointer to
   candidate. In `Show-Differences`, number of matched
   lines.

X
   Generally, working variable. In `Show-Differences`,
   old line number.

Y
   Generally, working variable. In `Show-Differences`,
   new line number.

SLOT        ( n -- addr )
   Address of _n_'th item in working memory from the bottom.
   Has the record number of a line.

SLOT-H      ( n -- addr )
   Address of _n_'th item in working memory from the top.
   Used for the hash value of a line. The memory for this is
   separate from the line numbers so it can be recovered
   after being sorted. The area will then be used for
   candidate identification.

------------------------------------------------------- [THEN]

0 VALUE NEWLINES        0 VALUE OLDLINES
VARIABLE X              VARIABLE Y
VARIABLE CAND

: SLOT    S" CELLS LCS + " EVALUATE ; IMMEDIATE
: SLOT-H  S" DFC-Space SWAP - CELLS LCS + " EVALUATE ; IMMEDIATE

0 [IF] =======================================================

Read-Newerfile                    ( -- )
   Read in the newer file, which is generally longer. Work
   from both ends toward the middle.  From the beginning of
   `LCS` put in the line numbers: 1, 2, 3, ....  From the end
   of `LCS put` in corresponding hash values: ... h3, h2, h1.
   Cell `LCS[0]` is not used now.

------------------------------------------------------- [THEN]

: Read-Newerfile                    ( -- )
   \  Read newer file saving line numbers and hash values.
   \  Output: newlines ; Use: newtext
   0                                   ( n)
   BEGIN
       1+
       &NEWTEXT NEWFILE Read-Text
   WHILE
       dup 2* DFC-Space > ABORT" Sorry, not enough space. "
       dup  dup SLOT !
       &NEWTEXT Cell-Count HASH over SLOT-H !
   REPEAT
   TO NEWLINES                         ( )
   ;

0 [IF] =======================================================

Sort-Hash-Values                  ( -- )
   Order the hash values, carrying the line numbers as the
   minor key. The result has the first n-1 line numbers in
   the cells 1..n-1 sorted by the hash value of the text
   of the corresponding lines.

------------------------------------------------------- [THEN]

   : Insert-Hash-Value             ( Gap j -- Gap )
       \  Inner insertion loop for custom Shell sort.
       \  Use: X Y
       dup SLOT-H @  X !    dup SLOT @  Y !
       over -                              ( Gap j)
       BEGIN   dup SLOT-H @  X @ < NOT
       WHILE
           dup SLOT-H @  X @ >
           ?dup 0= IF   dup SLOT @  Y @ >  THEN
       WHILE
           2dup + >R
               dup SLOT-H @    R@ SLOT-H !
               dup SLOT @
           R> SLOT !
           over -
           dup 1 <
       UNTIL   THEN THEN   over +      ( Gap j+Gap)
       X @  over SLOT-H !    Y @  over SLOT !
       DROP                            ( Gap)
       ;

: Sort-Hash-Values                  ( -- )
   \  Shell sort for unusual data structure.
   \  Input: newlines
   NEWLINES   1                        ( lines gap)
   BEGIN   2dup 1+ > WHILE  2*  1+  REPEAT
   BEGIN   2/   dup
   WHILE
       2dup 1+ ?DO   I Insert-Hash-Value   LOOP
   REPEAT                              2DROP
   ;

0 [IF] =======================================================

Mark-Hash-Classes                 ( -- )
   Mark the hash value equivalence classes by negating
   the last line number associated with a hash value.

------------------------------------------------------- [THEN]

: Mark-Hash-Classes                 ( -- )
   \  Negate lines with different hash from next line.
   \  Input: newlines
   NEWLINES 1-  1  ?DO
       I SLOT-H @  I 1+ SLOT-H @ = NOT
           IF   I SLOT dup @ NEGATE SWAP !   THEN
   LOOP
   NEWLINES 1- SLOT dup @ NEGATE SWAP !
   ;

0 [IF] =======================================================

Read-Oldfile                  ( -- )
   Reserve two cells following the line numbers of the newer
   file. Now read in each line of the old file. Take the hash
   value of the line, and find the first line in the newer
   file having the same hash value. Store the number of the
   cell containing line number in the next successive cell.
   If the line in the old file does not appear anywhere in
   newer file, store 0.

------------------------------------------------------- [THEN]

   : UNDER ( x y z -- z y ) ROT DROP SWAP ;

   : Search-for-Hash   ( match high low hash -- match )
       >R                      ( match high low)( R: hash)
           BEGIN   over 1+ over <
           WHILE
               2dup + 2/           ( match low high mid)
               dup SLOT-H @  R@ < IF
                   UNDER           ( match low high)
               ELSE                ( match low high mid)
                   NIP             ( match low high)
                   dup SLOT-H @  R@ =
                       IF   UNDER over   THEN
               THEN
           REPEAT   2DROP          ( match)
       R> DROP                     ( R: )
       ;

: Read-Oldfile                  ( -- )
   \  Read oldfile and match newfile hashed lines.
   \  Input: newlines ; Output: oldlines
   NEWLINES 1+                       ( biased-line-number)
   BEGIN
       1+
       &OLDTEXT OLDFILE Read-Text
   WHILE
       dup NEWLINES + DFC-Space >
           ABORT" Sorry, out of space for newer file. "
       0 0 NEWLINES               ( . match low high)
       &OLDTEXT Cell-Count HASH Search-for-Hash
                                  ( biased-line-number match)
       over SLOT !                ( biased-line-number)
   REPEAT
   TO OLDLINES                    ( )
   ;

0 [IF] =======================================================

We are done with the sub-array of hash values, and the
memory can be used for something else.

Find-Longest-Common-Subsequence   ( -- )
   Find the longest common subsequence.  Following the
   sub-array used for the old file, build a doubly-linked
   list representing the potential longest common subsequences
   in reverse order. In doing this, replace the value in
   the cells associated with the old file with the cell number
   of the appropriate doubly-linked list. The two cells that
   were reserved are used as the bounds of the subsequences.

------------------------------------------------------- [THEN]

: CANDIDATE                 ( x y z -- candidate-pointer)
   \  Make a new candidate for LCS.
   \  In/Out: cand
   CAND @ DFC-Space 2 - >
       ABORT" Sorry, candidate space exhausted. "
   CAND @ >R                       ( R: candidate-pointer)
       >R >R                       ( x)
           CAND @ SLOT !           (  )
           1 CAND +!
           R> ( y) CAND @ SLOT !   (  )
           1 CAND +!
       R> ( z) CAND @ SLOT !       (  )
       1 CAND +!
   R>                           ( candidate-pointer)( R: )
   ;

: Search-for-Match ( Value low high -- 0 | Value wherefound )
   \  Binary search for LCS candidates.
   ROT >R                          ( low high)( R: Value)
       BEGIN   2dup > NOT
       WHILE
           2dup + 2/               ( low high mid)
           dup SLOT @ 1+ SLOT @  R@ < NOT IF
               1-    NIP           ( low high)
           ELSE                    ( low high mid)
               dup 1+ SLOT @ 1+ SLOT @  R@ < NOT
                   IF    NIP NIP R> SWAP EXIT    THEN
               1+    UNDER         ( low high)
           THEN
       REPEAT                      2DROP
   R> DROP                             ( R: )
   0                               ( 0)
   ;

: New-Candidate         ( value wherefound i -- flag)
   \  Make and link a new LCS candidate.
   \  In/Out: X Y LCS
   ROT ROT                     ( i value wherefound)
   dup >R
       2dup 1+ SLOT @ 1+ SLOT @  < IF
           Y @  X @ SLOT !
           dup 1+  X !
           SLOT @  CANDIDATE Y !       ( )
       ELSE   2DROP DROP   THEN
   R>  LCS @ =                         ( flag)
   dup IF ( Move fence. )
       LCS @ 1+ SLOT @  LCS @ 2 + SLOT !
       1 LCS +!
   THEN                                ( flag)
   ;

: Find-Longest-Common-Subsequence   ( -- )
   \  Nuf ced.
   \  Input: oldlines newlines ; Use: cand LCS X Y
   OLDLINES  CAND !
   NEWLINES  LCS !
   NEWLINES 1+  0 0 CANDIDATE  LCS @ SLOT !
   OLDLINES  NEWLINES 0 CANDIDATE  LCS @ 1+ SLOT !
   OLDLINES  NEWLINES 2 +
   ?DO                ( )
       I SLOT @                ( newer-line-number)
       dup IF
           NEWLINES  dup X !  SLOT @  Y !
           BEGIN
               dup SLOT @ ABS  ( . value)
               X @  LCS @  Search-for-Match
                               ( . 0 | . value wherefound)
               dup IF   I  New-Candidate   THEN
                               ( newer-line-number flag)
               0=
           WHILE               ( newer-line-number)
               dup SLOT @ 0>
           WHILE
               1+
           REPEAT THEN
           Y @  X @ SLOT !
       THEN                    DROP
   LOOP
   ;

0 [IF] =======================================================

Build-Candidate-Table             ( -- )
   Untangle the linked reverse list of the longest common
   subsequence to become a simple linear list in forward
   order in the sub-array used for the old file.

------------------------------------------------------- [THEN]

: Build-Candidate-Table             ( -- )
   \  Unravel LCS.
   \  Input: LCS oldlines newlines
   LCS @ SLOT @                        ( c)
   OLDLINES  NEWLINES 2 +
       ?DO    0  I SLOT !    LOOP
   NEWLINES  OLDLINES  SLOT !
   BEGIN   dup
   WHILE
       dup 1+ SLOT @                   ( c j)
       over SLOT @ SLOT !              ( c)
       2 + SLOT @
   REPEAT                              DROP
   ;

0 [IF] =======================================================

Show-Differences              ( -- )
   The values are 0 if the line does not appear in the newer
   file, or the line number of a candidate match in the
   newer file.  Skipped numbers are lines that are new in
   the newer file.

   Display the lines that were deleted from the old file,
   inserted in the newer file, or unchanged.

   State: 0= delete, 0< add, 0> copy.

------------------------------------------------------- [THEN]

: DELETED                   ( previous-state -- state )
   \  What to do when the line is in the old file only.
   \  Input: X Y oldtext
   \  In/Out: matchingtext
   &MATCHINGTEXT @ 0< NOT IF
       X @ 1- 4 U.R SPACE
           Y @ 4 U.R    SPACE
               &MATCHINGTEXT Write-Text
       -1 &MATCHINGTEXT !
   THEN
   X @ 4 U.R    SPACE    ." ---- "
       &OLDTEXT Write-Text
   DROP 0 ( delete )
   ;

: ADDED                     ( previous-state -- state )
   \  What to do when the line is in the newer file only.
   \  Input: X Y newtext
   \  In/Out: matchingtext
   &MATCHINGTEXT @ 0< NOT IF
       X @ 1- 4 U.R    SPACE
           Y @ 1- 4 U.R   SPACE
               &MATCHINGTEXT Write-Text
       -1 &MATCHINGTEXT !
   THEN
   ." ++++ "    Y @ 4 U.R    SPACE
       &NEWTEXT Write-Text
   DROP -1 ( add )
   ;

: MATCHED                   ( previous-state -- state )
   \  What to do when the line is in both files.
   \  Input: X Y oldtext newtext
   \  In/Out: LCS : number of matched lines.
   \  Output: matchingtext
   1 LCS +!
   dup 1- 0< DFC-COLLATE @ OR ( adding or deleting ) IF
       X @ 4 U.R    SPACE
           Y @ 4 U.R    SPACE
               &NEWTEXT Write-Text
       DROP 1 ( copy )
   ELSE ( copying, = number of lines just copied. )
       1+
       3 over = IF    CR    THEN
       &NEWTEXT Cell-Count &MATCHINGTEXT Cell-Place
   THEN
   ;

   : Handle-Deleted            ( state -- same )
       BEGIN
           1 X +!    X @ NEWLINES + 1+  OLDLINES  < IF
               &OLDTEXT OLDFILE Read-Text 0=
                   ABORT" Oops, error with old file. "
           THEN
           X @ NEWLINES + 1+  SLOT @
               ( i.e. newer-line-number) 0=
       WHILE   DELETED   REPEAT
       ;

   : Handle-Added              ( state -- same )
       BEGIN
           1 Y +!    Y @  NEWLINES < IF
               &NEWTEXT NEWFILE Read-Text 0=
                   ABORT" Oops, error with newer file. "
           THEN
           X @ NEWLINES + 1+  SLOT @ Y @ >
       WHILE   ADDED   REPEAT
       ;

   : Clean-Compare                ( s1 . s2 . -- 0|-1|1 )
       &Cleaned-Newtext Cell-Place        ( s1 .)
       &Cleaned-Oldtext Cell-Place        ( )
       &Cleaned-Oldtext Cell-Count Clean-Line    ( s1' .)
       &Cleaned-Newtext Cell-Count Clean-Line    ( s2' .)
       COMPARE                    ( 0|-1|1)
       ;

   : Handle-Matched            ( state -- same )
       \  Check that matched records are really the same.
       &OLDTEXT Cell-Count  &NEWTEXT Cell-Count
           Clean-Compare 0= IF
                   MATCHED
           ELSE    ADDED   DELETED   THEN
       ;

: Show-Differences              ( -- )
   \  Let's see them.
   \  Input: oldlines newlines ; Use: X Y LCS matchingtext
   OLDFILE REWIND   NEWFILE REWIND
   0 X !    0 Y !    0 LCS !
   -1 &MATCHINGTEXT !
   1 ( copying )
   BEGIN                           ( state)
       Handle-Deleted    Handle-Added
       Y @ NEWLINES <
   WHILE   Handle-Matched
   REPEAT                          DROP
   ;

0 [IF] =======================================================
<BIG>

DFC      ( -- )
   Differential File Comparison.

</BIG>
------------------------------------------------------- [THEN]

   : Textbuffer-Size ( -- n )
       DFC-MAXLINE 2 + chars ALIGNED CELL+ ;

   : Allocate-Input-Buffers
       HERE PADDING + ALIGNED
           dup TO &OLDTEXT   Textbuffer-Size +
           dup TO &NEWTEXT   Textbuffer-Size +
       TO LCS
       UNUSED  1 CELLS - 1+ ALIGNED  LCS -  HERE +  1 CELLS /
           TO DFC-Space
       ;

INTERFACE

: >OLDFILE      ( addr cnt -- )
        R/O OPEN-FILE DROP TO OLDFILE ;

: >NEWFILE      ( addr cnt -- )
        R/O OPEN-FILE DROP TO NEWFILE ;

: DFC                           ( -- )
   \   Differential file comparison.

   NEWFILE FILE-SIZE DROP  OR 0= ABORT" Size of NEWFILE is 0. "
   OLDFILE FILE-SIZE DROP  OR 0= ABORT" Size of OLDFILE is 0. "

   Allocate-Input-Buffers

   Read-Newerfile   Sort-Hash-Values   Mark-Hash-Classes
   Read-Oldfile     Find-Longest-Common-Subsequence
   Build-Candidate-Table

   OLDLINES 1+ SLOT
       dup TO &MATCHINGTEXT   Textbuffer-Size +
       dup TO &Cleaned-Oldtext  Textbuffer-Size +
       dup TO &Cleaned-Newtext  Textbuffer-Size +
   HERE UNUSED + U< NOT
       ABORT" Sorry, no room for comparison. "

   CR Show-Differences

   OLDLINES NEWLINES - 2 - LCS @ - . ." deletions, "
   NEWLINES 1- LCS @ - . ." insertions, "
   LCS @ . ." unchanged. " CR

   OLDFILE REWIND   NEWFILE REWIND
   ;

PREVIOUS DEFINITIONS

cr cr
.( Example of use: ) cr cr
.( s" old-file-name" >oldfile ) cr
.( s" new-file-name" >newfile ) cr
.( DFC ) cr cr

0 [IF] =================================================================

       NEWER and OLDER

When `CLIPBOARD` is defined, text can be copied to the clipboard and
then written to files `Newer` and `Older` with file-IDs `NEWFILE` and
`OLDFILE`. They can then be compared with `DFC`.

----------------------------------------------------------------- [THEN]

[DEFINED] CLIPBOARD [IF]

   : INOUT  ( str len -- fid )
           2dup DELETE-FILE DROP
           R/W CREATE-FILE ABORT" Can't CREATE-FILE "
           ;

   : NEWER S" Newer" INOUT TO NEWFILE
           CLIPBOARD NEWFILE WRITE-FILE
               ABORT" Can't WRITE-FILE "
           NEWFILE REWIND ;

   : OLDER S" Older" INOUT TO OLDFILE
           CLIPBOARD OLDFILE WRITE-FILE
               ABORT" Can't WRITE-FILE "
           OLDFILE REWIND ;

   : SWAPFILES  ( -- )
       OLDFILE NEWFILE TO OLDFILE TO NEWFILE ;

[THEN]

\\  End of Differential File Comparison

