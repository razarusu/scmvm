;;; Push/pop example

	load	r1 16
	push	r1
	travel	(shift-right)
	pop	r1
	out	r1
	halt

shift-right:
	pop	r8
	load	r7 2
	div	r8 r7 r8 r7
	push	r8
	return
