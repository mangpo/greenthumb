	cmp	r1, #-2147483648
	add	r3, r1, #7
	mov	r5, r1, asr #31
	movcc	r3, r1
	cmp	r2, #0
	mov	r2, r3, asr #3
	mov	r3, r5, lsr #29
	mov	r4, #1
	add	r1, r1, r3
	and	r1, r1, #7
	rsb	r3, r3, r1
	orrne	r1, r0, r4, asl r3
	biceq	r1, r0, r4, asl r3
	uxtbne	r1, r1

