	str	r0, [fp, #-40]
	str	r1, [fp, #-44]
	ldr	r3, [fp, #-40]
	uxth	r3, r3
	str	r3, [fp, #-36]
	ldr	r3, [fp, #-40]
	mov	r3, r3, asr #16
	str	r3, [fp, #-32]
	ldr	r3, [fp, #-44]
	uxth	r3, r3
	str	r3, [fp, #-28]
	ldr	r3, [fp, #-44]
	mov	r3, r3, asr #16
	str	r3, [fp, #-24]
	ldr	r3, [fp, #-36]
	ldr	r2, [fp, #-28]
	mul	r3, r2, r3
	str	r3, [fp, #-20]
	ldr	r3, [fp, #-32]
	ldr	r2, [fp, #-28]
	mul	r2, r2, r3
	ldr	r3, [fp, #-20]
	mov	r3, r3, lsr #16
	add	r3, r2, r3
	str	r3, [fp, #-16]
	ldr	r3, [fp, #-16]
	uxth	r3, r3
	str	r3, [fp, #-12]
	ldr	r3, [fp, #-16]
	mov	r3, r3, asr #16
	str	r3, [fp, #-8]
	ldr	r3, [fp, #-24]
	ldr	r2, [fp, #-36]
	mul	r2, r2, r3
	ldr	r3, [fp, #-12]
	add	r3, r2, r3
	str	r3, [fp, #-12]
	ldr	r3, [fp, #-32]
	ldr	r2, [fp, #-24]
	mul	r2, r2, r3
	ldr	r3, [fp, #-8]
	add	r2, r2, r3
	ldr	r3, [fp, #-12]
	mov	r3, r3, asr #16
	add	r3, r2, r3
	mov	r0, r3
