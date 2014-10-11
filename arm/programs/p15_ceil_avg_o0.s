	str	r0, [fp, #-16]
	str	r1, [fp, #-20]
	ldr	r2, [fp, #-16]
	ldr	r3, [fp, #-20]
	orr	r3, r2, r3
	str	r3, [fp, #-12]
	ldr	r2, [fp, #-16]
	ldr	r3, [fp, #-20]
	eor	r3, r2, r3
	str	r3, [fp, #-8]
	ldr	r3, [fp, #-8]
	mov	r3, r3, asr #1
	str	r3, [fp, #-8]
	ldr	r2, [fp, #-12]
	ldr	r3, [fp, #-8]
	rsb	r3, r3, r2
	mov	r0, r3
	
