	uxth	r5, r0
	mov	r3, r0, asr #16
	uxth	r4, r1
	mov	r1, r1, asr #16
	mul	r2, r5, r4
	mul	r4, r3, r4
	mul	r5, r5, r1
	add	r2, r4, r2, lsr #16
	mov	r0, r2, asr #16
	uxtah	r2, r5, r2
	mla	r0, r1, r3, r0
	add	r0, r0, r2, asr #16
