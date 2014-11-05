	movw	r3, #43690
	movt	r3, 43690
	and	r3, r0, r3
	movw	r2, #21845
	movt	r2, 21845
	movw	r1, #13107
	and	r2, r0, r2
	movw	r0, #52428
	add	r3, r2, r3, lsr #1
	movt	r0, 52428
	and	r0, r3, r0
	movt	r1, 13107
	and	r1, r3, r1
	movw	r2, #61680
	add	r1, r1, r0, lsr #2
	movt	r2, 61680
	and	r2, r1, r2
	movw	r3, #3855
	movt	r3, 3855
	and	r3, r1, r3
	add	r3, r3, r2, lsr #4
	bic	r1, r3, #-16777216
	bic	r3, r3, #16711680
	bic	r1, r1, #65280
	add	r3, r1, r3, lsr #8
	mov	r0, r3, lsr #16
	uxtah	r0, r0, r3
