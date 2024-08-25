; Smallest code AVR Sqrt
; Target: All AVRs including AT90S1200
; June 1999, by Jack Tidwell
; Calculates the sqrt by subtracting an ODD number that self increments (by 2)
;	for each iteration. If the new ODD 'subber' is LESS than the previous
;	results, increment our root by 1, and loop again.
; Worst case is about 200us at 8mhz and less than 15us for 8 bit.
; Example sqrt(100)
;num     'odd_suber'     sqrt
;100     1               1
;99      3               2
;96      5               3
;91      7               4
;84      9               5
;75      11              6
;64      13              7
;51      15              8
;36      17              9
;19      19              10

so the sqrt(100) = 10

.def numlo = r16
.def numhi = r17
.def sqrt  = r18
.def suber = r24
.def suberh= r25

; enter with the 16 bit Number in r16,r17

Sqrt:
        clr     sqrt
        ldi     suber,1	; initialize the seed to be subtracted
        clr     suberh	;	for each iteration
loop:   sub     numlo,suber
        sbc     numhi,suberh
	brlo	exit
        inc     sqrt
        adiw    suber,2	; keep the number to subtract ODD.
        rjmp    loop
exit:   ret             ; the sqrt(num) on exit is stored in r18
;
