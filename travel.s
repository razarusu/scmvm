;; TRAVEL instruction example.

	load	r1 1
	travel	(double)
	inc	r1
	travel	(double)
	inc	r1
	travel	(double)
	halt

double:
	copy	r1 r2
	add	r2 r2 r2
	out	r2
	return
