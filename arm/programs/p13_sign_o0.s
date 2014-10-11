	str	r0, [fp, #-16]
	ldr	r3, [fp, #-16]
	mov	r3, r3, asr #31
	str	r3, [fp, #-12]
	ldr	r3, [fp, #-16]
	rsb	r3, r3, #0
	str	r3, [fp, #-8]
	ldr	r3, [fp, #-8]
	mov	r3, r3, asr #31
	str	r3, [fp, #-8]
	ldr	r2, [fp, #-12]
	ldr	r3, [fp, #-8]
	orr	r3, r2, r3
	mov	r0, r3
	
