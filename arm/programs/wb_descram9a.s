	add	r1, r1, r2
	mov	r2, r1, lsr #31
	add	r1, r1, r2
	and	r1, r1, #1
	rsb	r2, r2, r1