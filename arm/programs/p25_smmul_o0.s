	str	r0, [fp, #-32]
	str	r1, [fp, #-36]
	ldr	r3, [fp, #-32]
	uxth	r3, r3
	str	r3, [fp, #-28]
	ldr	r3, [fp, #-32]
	mov	r3, r3, asr #16
	str	r3, [fp, #-24]
	ldr	r3, [fp, #-36]
	uxth	r3, r3
	str	r3, [fp, #-20]
	ldr	r3, [fp, #-36]
	mov	r3, r3, asr #16
	str	r3, [fp, #-16]
	ldr	r3, [fp, #-28]
	ldr	r2, [fp, #-20]
	mul	r3, r2, r3
	str	r3, [fp, #-12]
	ldr	r3, [fp, #-24]
	ldr	r2, [fp, #-20]
	mul	r3, r2, r3
	str	r3, [fp, #-8]
	ldr	r3, [fp, #-28]
	ldr	r2, [fp, #-16]
	mul	r3, r2, r3
	str	r3, [fp, #-28]
	ldr	r3, [fp, #-24]
	ldr	r2, [fp, #-16]
	mul	r3, r2, r3
	str	r3, [fp, #-24]
	ldr	r3, [fp, #-12]
	mov	r3, r3, asr #16
	str	r3, [fp, #-12]
	ldr	r2, [fp, #-12]
	ldr	r3, [fp, #-8]
	add	r3, r2, r3
	str	r3, [fp, #-12]
	ldr	r3, [fp, #-12]
	uxth	r3, r3
	str	r3, [fp, #-8]
	ldr	r3, [fp, #-12]
	mov	r3, r3, asr #16
	str	r3, [fp, #-12]
	ldr	r2, [fp, #-8]
	ldr	r3, [fp, #-28]
	add	r3, r2, r3
	str	r3, [fp, #-8]
	ldr	r3, [fp, #-8]
	mov	r3, r3, asr #16
	str	r3, [fp, #-8]
	ldr	r2, [fp, #-8]
	ldr	r3, [fp, #-12]
	add	r3, r2, r3
	str	r3, [fp, #-8]
	ldr	r2, [fp, #-8]
	ldr	r3, [fp, #-24]
	add	r3, r2, r3
	mov	r0, r3
