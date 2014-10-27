	mov	r0, r2, asl #2
	rsb	r5, r0, r2
	mul	r2, r6, r2
	mov	r5, r5, asl #11
	sub	r5, r5, #12
	rsb	r3, r2, r3
	mla	r3, r5, r10, r3
	mla	r1, r11, r1, r3