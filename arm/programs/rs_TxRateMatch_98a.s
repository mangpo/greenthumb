	add	r6, r0, r0, asl #1
	mov	r6, r6, asl #11
	add	r6, r6, #12
	mla	r2, r6, r2, r3
	add	r3, r3, #1
	mla	r2, r4, r0, r2
	add	r2, r2, #12
