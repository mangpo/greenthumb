	movw	r1, #21845
	bfi	r1, r1, #16, #16
	and	r1, r1, r0, asr #1
	movw	r2, #13107
	rsb	r1, r1, r0
	movt	r2, 13107
	movw	r3, #13107
	bfi	r3, r3, #16, #16
	and	r2, r1, r2
	and	r1, r3, r1, asr #2
	add	r2, r2, r1
	movw	r3, #3855
	movt	r3, 3855
	add	r2, r2, r2, asr #4
	and	r3, r2, r3
	add	r3, r3, r3, lsr #8
	add	r0, r3, r3, lsr #16
	and	r0, r0, #63
	
