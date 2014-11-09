	cmp	r1, #-2147483648
	add	r3, r1, #7
	mov	r2, r2, lsr #29
	movcc	r3, r1
	add	r1, r1, r2
	and	r1, r1, #7
	rsb	r2, r2, r1
	mov	r4, #1
	eor	r2, r5, r4, asl r2
