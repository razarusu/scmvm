	in	r1
	in	r2
loop:	div	r1 r2 r1 r3
	jump-if-zero	r3 (finish)
	copy	r3 r2
	jump	(loop)
finish:	halt
