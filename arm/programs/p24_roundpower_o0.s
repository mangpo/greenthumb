	str	r0, [fp, #-8]
	ldr	r3, [fp, #-8]
	sub	r3, r3, #1
	str	r3, [fp, #-8]
	ldr	r3, [fp, #-8]
	mov	r3, r3, asr #1
	ldr	r2, [fp, #-8]
	orr	r3, r2, r3
	str	r3, [fp, #-8]
	ldr	r3, [fp, #-8]
	mov	r3, r3, asr #2
	ldr	r2, [fp, #-8]
	orr	r3, r2, r3
	str	r3, [fp, #-8]
	ldr	r3, [fp, #-8]
	mov	r3, r3, asr #4
	ldr	r2, [fp, #-8]
	orr	r3, r2, r3
	str	r3, [fp, #-8]
	ldr	r3, [fp, #-8]
	mov	r3, r3, asr #8
	ldr	r2, [fp, #-8]
	orr	r3, r2, r3
	str	r3, [fp, #-8]
	ldr	r3, [fp, #-8]
	mov	r3, r3, asr #16
	ldr	r2, [fp, #-8]
	orr	r3, r2, r3
	str	r3, [fp, #-8]
	ldr	r3, [fp, #-8]
	add	r3, r3, #1
	str	r3, [fp, #-8]
	ldr	r3, [fp, #-8]
	mov	r0, r3
	
