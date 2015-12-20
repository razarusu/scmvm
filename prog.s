;;; This program calculates a square root of a square number.
;;;  if x = 1 + 3 + 5 + ... + n
;;;        (1   2   3   ...   i)
;;;  then sqrt(x) = i

	in	r1

	load	r2 -1
	load	r3 0
	load	r4 -2

loop:	inc	r3
	add	r1 r2 r1
	add	r2 r4 r2
	neg	r2
	jump-if-bigger	r2 r1 (finish)
	neg	r2
	jump	(loop)

finish:	out	r3
	out	r1
	halt
