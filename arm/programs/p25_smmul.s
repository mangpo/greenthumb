	mov	r3, r0, asr #16
	uxth	r0, r1
	mov	r1, r1, asr #16
	mul	r2, r0, ip
	mul	r0, r0, r3
	add	r2, r0, r2, asr #16
	mul	r0, r1, ip
	uxtah	r0, r0, r2
	mov	r2, r2, asr #16
	add	r0, r2, r0, asr #16
	mla	r0, r1, r3, r0
