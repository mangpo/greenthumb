	cmp	r1, #-2147483648
	mov	r3, r1, asr #31
	add	r2, r1, #7
	mov	r3, r3, lsr #29
	movcc	r2, r1
	add	r1, r1, r3
	and	r1, r1, #7
	rsb	r3, r3, r1
	mov	r0, r0, asr r3
	and	r0, r0, #1
