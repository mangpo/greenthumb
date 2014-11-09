	mov	r3, r3, asl #1
	mov	r4, r0, lsr #30
	add	r2, r4, r2, asl #2
	mov	r0, r0, asl #2
	mov	r4, r3, asl #1
	add	r4, r4, #1
	cmp	r2, r4
	rsbcs	r2, r4, r2
	addcs	r3, r3, #1
